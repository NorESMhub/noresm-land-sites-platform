'''
Created on Aug 30, 2013

@author: alfg
'''

from cdo import *
cdo = Cdo()
from lxml import etree as ET
import os
import progressbar
import uuid

class levelFractionFile(object):
	def __init__(self, absPath):
		self.absPath = absPath
	
	def getFullPath(self):
		return self.absPath
		
	def findClosestTimeStep(self,aDate):
		"""
		Returns time step in file
		"""
		dateStrings = cdo.showdate(input=self.absPath)[0].split()
		searchMonth = aDate.month
		retVal = 1
		minVal = 99999999999999999
		i = 1
		for aDate in dateStrings:
			ymd = aDate.split("-")
			aMonth = int(ymd[1])
			if(abs(aMonth-searchMonth) < minVal):
				retVal = i
				minVal = abs(aMonth-searchMonth)
			i=i+1
		return retVal

class camOutputFile(object):

	def __init__(self, inputPath,outputPath,xmlElement):
		self.inputPath=inputPath
		self.outputPath=outputPath
		self.inputFiles = []              #list of inputfiles
		self.xmlElement= xmlElement
		self.fullPath=""
		self.levels=0
		self.scenario=""
		self.component=""
		self.source=""
		self.bottomLevel=0.0
		self.levelValues=[]
		self.minYear=0
		self.maxYear=9999999
		self.interfaceLevels=[]
		
		#Error checking
		if(not os.path.isdir(self.inputPath)):
			raise Exception("input directory " + self.inputPath + " does not exist")
		if(not os.path.isdir(self.outputPath)):
			raise Exception("output directory " + self.outputPath + " does not exist")
	
	def getLevelValues(self):
		return self.levelValues

	def getBottomLevel(self):
		return self.bottomLevel
			
	def getOutputPath(self):
		return self.outputPath

	def getNumberOfLayers(self):
		return self.levels
	
	def __setLevels(self,levels):
		self.levels = levels

	def getInputFileList(self):
		return self.inputFiles
	
	def getFullPath(self):
		return self.fullPath
	
	def getLevelHeight(self,i):
		return self.interfaceLevels[i+1] - self.interfaceLevels[i]
	
	def __setFullPath(self,outputFileName):
		fullPath = os.path.normpath(os.path.join(self.outputPath,outputFileName))
		self.fullPath=fullPath

	def getMinYear(self):
		return self.minYear
	
	def getMaxYear(self):
		return self.maxYear

	def getInterfaceLevels(self):
		return self.interfaceLevels

	def configure(self):
		"""Purpose: Based on the 
		"file field in the config-file.. Create a set of input-
		files with the same properties.. Child is an e-tree xml-node"""
		relativePathName = self.xmlElement.attrib["name"]
		
		self.__setFullPath(relativePathName)
		
		if self.xmlElement.attrib.has_key("bottomLevel"):
			self.bottomLevel = float(self.xmlElement.attrib["bottomLevel"])
		
		if self.xmlElement.attrib.has_key("levelValues"):
			levelValues = (self.xmlElement.attrib["levelValues"])
			levelValuesList=levelValues.split(",")
			levelValuesList2=[]
			#Make sure the level values are numbers..
			for i in levelValuesList:
				numberValue = float(i)
				levelValuesList2.append(numberValue)
			self.levelValues = levelValuesList2
		
		#set number of levels	
		self.__setLevels(len(self.levelValues))
		
		#find the levels of the interfaces given the midpoints		
		self.interfaceLevels=[]
		self.interfaceLevels.append(0.0)
		idx=0
		if(len(self.levelValues) > 0 ):
			for aLevel in self.levelValues:
				#add another entry to the interface levels
				self.interfaceLevels.append(self.interfaceLevels[idx] + 2.0*(self.levelValues[idx] -self.interfaceLevels[idx]))
				idx += 1
				#Check for error in midpoint layer config
				if(idx <= len(self.levelValues)-1):
					if(self.interfaceLevels[idx] > self.levelValues[idx]):
						raise Exception("Error of output levels in level " + str(idx) + " upper interface "+str(self.interfaceLevels[idx])
						  + " midpoint above:  " + str(self.levelValues[idx]) + self.fullPath)
		
		tmpMinYear = 999999999999999999999
		tmpMaxYear = 0
		if self.xmlElement.attrib.has_key("year"):
			yearTag = str(self.xmlElement.attrib["year"])
			years=yearTag.split(",")
			for aYear in years:
				fYear = float(aYear)
				if(fYear < tmpMinYear):
					tmpMinYear = fYear
					self.minYear = fYear
				if(fYear > tmpMaxYear):
					self.maxYear = fYear
					tmpMaxYear = fYear
		
		#Search through list of input files
		for inputFileElement in self.xmlElement.getiterator("inputfile"):
				#Create an input-file instance
				aCamInputFile = camInputFile()
		
				aCamInputFile.configure(inputFileElement,  self.inputPath  ,  aYear, self)
				
				#Successfully configured ==> add our list of needed input files
				self.inputFiles.append(aCamInputFile)
			
class camInputFile(object):
	'''
	Class describing an input file, resolution etc
	'''
	def __init__(self):
		self.year=""
		self.fullPath=""    #name of the file
		self.fields=[]      #list of cam-fields
		self.hasVerticalProfile=False
		self.sector=dict()
		self.fieldLevel=dict()
		self.inputLevels=[]
		self.inputInterfaceLevels=[]
		self.layerHeight=[]
		self.conversion = None
		self.levelFractionFile=None
		self.levelFractionFileReGrid=None
	
	def hasLevelFractionFile(self):
		return (self.levelFractionFile != None)
	
	def getLevelFractionFileName(self):
		if self.levelFractionFile != None:
			return self.levelFractionFile.getFullPath()
		
	def getConversionFactor(self):
		if(self.conversion != None):
			return self.conversion.getConversionFactor()
		else:
			return 1.0
	
	def getInterfaceLevels(self):
		return self.inputInterfaceLevels
	
	def hasConversion(self,aString):
		if(self.conversion != None):
			return self.conversion.hasConversion(aString)
		else:
			return False
	
	def setField(self,fieldName,fieldSector,outputLevel):
		#fxm: Guard against same sector several times in file
		#levels is list of levels.., normally a list of size 1 with levels[0]=0
		self.sector[fieldSector]=fieldName
		self.fieldLevel[fieldName]=outputLevel
		
	def configure(self,inputFileElement,inputPath,aYear,parentOutputFile):
		#Get file name
		fileWithCodes=inputFileElement.get("name")
		
		#Replace the codes to create the real name
		aFileName=fileWithCodes.replace("$year$",aYear)

		#Check if the file exists
		inputFullPath = os.path.join(os.path.normpath(inputPath),aFileName)
		if(os.path.isfile(inputFullPath)):
			#Create the output file with the configuration options
			self.fullPath = inputFullPath
		else:
			raise Exception ("Could not find file " + inputFullPath + "===> error in xml-config file") 
	
		#Check names of fields in file
		fieldNamesInFile = cdo.showname(input=self.getFullPath())[0].split(" ")
		fieldLevelsInFile = cdo.showlevel(input=self.getFullPath())
		for expr in fieldLevelsInFile:
			list2 = expr.split(" ")
			for expr2 in list2:
				if (float(expr2) > 0.0):
					self.hasVerticalProfile = True
	
		#Find the level values of the input file (m)
		self.inputLevels=[]
		if inputFileElement.attrib.has_key("levelValues"):
			levelValues = (inputFileElement.attrib["levelValues"])
			levelValuesList=levelValues.split(",")
			levelValuesList2=[]
			#Make sure the level values are numbers..
			for i in levelValuesList:
				numberValue = float(i)
				levelValuesList2.append(numberValue)
			self.inputLevels = levelValuesList2
	
		#find the levels of the interfaces given the midpoints		
		self.inputInterfaceLevels=[]
		self.inputInterfaceLevels.append(0.0)
		idx=0
		if(len(self.inputLevels) > 0 ):
			for aLevel in self.inputLevels:
				#add another entry to the interface levels
				self.inputInterfaceLevels.append(self.inputInterfaceLevels[idx] + 2.0*(self.inputLevels[idx] -self.inputInterfaceLevels[idx]))
				idx += 1
				#Check for error in midpoint layer config
				if(idx <= len(self.inputLevels)-1):
					if(self.inputInterfaceLevels[idx] > self.inputLevels[idx]):
						raise Exception("Error of input levels in level " + str(idx) + " upper interface "+str(self.inputInterfaceLevels[idx])
						  + " midpoint above:  " + str(self.inputLevels[idx]) + self.fullPath)
				#Calculate the layer height(m)
				self.layerHeight.append(self.inputInterfaceLevels[idx]-self.inputInterfaceLevels[idx-1])
		
		#Go through the fields the user asked for..
		#Either, we have 1 layer in input file which can go to any level.. Either we have a 3d input file
		for field in inputFileElement.getiterator("field"):
			fieldName=field.get("name")
			fieldSector=field.get("sector")
			aLevel = 1                          #default is output layer 1
			if field.attrib.has_key("level"):
				aLevel=int(field.get("level"))

			#Verify the configured fields against the names actually in the file
			found = False
			#Check all field names in file and see if we find this one..
			for aName in fieldNamesInFile:
				if (aName == fieldName):
					found = True
			if(not found):
				raise(Exception("Requested field "+ fieldName + " not found in file " + self.fullPath))
			else: #==> ALL OK
				self.setField(fieldName,fieldSector,aLevel)
		
		for lf in inputFileElement.getiterator("levelFractionFile"):
			aFileName=lf.get("name")
			fullname = os.path.join(os.path.normpath(inputPath),aFileName)
			self.levelFractionFile = fullname
			
		if (self.levelFractionFile != None):
			###########################################################################
			#NEED TO REGRID THE LEVEL FRACTION FILE TO SAME GRID AS INPUT FILE
			#DO THIS ON INIT (WHEN READING THE INPUT FILE
			############################################################################
			griddes = cdo.griddes(input=self.getFullPath())
			basepath = parentOutputFile.getOutputPath()
			gridfilename = os.path.normpath(os.path.join(basepath,"griddes01"))
				
			#create a grid file from this list output (grid description of fraction file)
			startIndex = 0
			endIndex = 0
			icnt = 0
			gridOK=False
			for item in griddes:
				if(startIndex == 0 or endIndex == 0):
					if(item.find("#") != -1 ):
						if(not gridOK):
							startIndex = icnt
						else:
							endIndex = icnt
					if item.find("gridtype") != -1 and item.find("generic") == -1 : 
						gridOK = True
				icnt = icnt + 1
			if(endIndex ==0):
				endIndex = len(griddes)-1
				
			aString=""
			icnt=startIndex
			while icnt < endIndex : 
				aString = aString + griddes[icnt]
				aString = aString + os.linesep
				icnt = icnt + 1
			f = open(gridfilename,'w')
			f.write(aString)
			f.close()
			#Now we have the grid description!!
			#Interpolate level fractions to same grid as input file
			tmpFileName1= os.path.normpath(os.path.join(basepath,str(uuid.uuid4())+".nc"))
			tmpFileName2= os.path.normpath(os.path.join(basepath,str(uuid.uuid4())+".nc"))
			tmpFileName3= os.path.normpath(os.path.join(basepath,str(uuid.uuid4())+".nc"))
			
			cdo.remapbil(gridfilename,input=self.levelFractionFile,output=tmpFileName1) # fraction file is now for one time step to input file format
			os.remove(gridfilename)
			
			#Make sure vertical sum is one
			cdo.vertsum(input=tmpFileName1, output=tmpFileName2)
			subprocess.check_call(["ncrename","-v","fraction,sumFraction",tmpFileName2])
			#avoid division by zero
			expr = "sumFraction=sumFraction+1.e-30"
			cdo.expr(expr,input=tmpFileName2,output=tmpFileName3)
			os.remove(tmpFileName2)
			
			subprocess.check_call(["ncks","-A","-v","sumFraction",tmpFileName3,tmpFileName1])
			os.remove(tmpFileName3)
			
			expr="fraction=fraction/sumFraction"
			cdo.expr(expr,input=tmpFileName1,output=tmpFileName2)
			
			self.levelFractionFileReGrid = tmpFileName2
			os.remove(tmpFileName1)
		
	def getLevelFractionFileReGrid(self):
		return self.levelFractionFileReGrid
		
	def cleanFiles(self):
		if (self.levelFractionFileReGrid != None):
			os.remove(self.levelFractionFileReGrid)
		
	def getLevelHeight(self,i):
		return self.layerHeight[i]
	
	def getNumberOfLevels(self):
		return len(self.inputLevels)
	
	def getLevelValues(self):
		return self.inputLevels
	
	def getHasVerticalProfile(self):
		return self.hasVerticalProfile
	
	def validateFieldNames(self, fieldNames):
		"""Check if the field names obtained from xml config file
		are the same as the fields in the netCDF file"""
		foundAll = True
		
		for aSector in self.sector.keys():              #These are the fields the xml-files says we should have..
			aField = self.sector[aSector]
			foundOne = False
			for ncField in fieldNames:                   #Look through the fields we do have..
				if(ncField == aField):
					foundOne = True
			if(not foundOne):
				print "could not find field specified in xml config file" + aField + " in " + self.fullPath
				foundAll = False
		return foundAll

	#fxm: This should take an optional argument.. outputfile		
	def getConfiguredFields(self):
		ret=[]
		for aSector in self.sector.keys():
			ret.append(self.sector[aSector])
		return ret
	
	def getConfiguredFieldsInLayer(self,layer):
		ret=[]
		fields = self.getConfiguredFields()
		for aField in fields:
			if(self.fieldLevel[aField] == layer):
				ret.append(aField)
		return ret
	
	def getFullPath(self):
		return self.fullPath
	
	def setPaths(self, fullpath, outputfile):
		self.fullPath   = fullpath
		self.outputfilename = outputfile
		

class FileProvider(object):
	'''
	Class which provides information about files in the system
	'''

	def __init__(self,xmlpath):
		'''
		Constructor
		'''
		self.inputDataPath=""
		self.outputDataPath=""
		self.xmlconfigfile=xmlpath
		self.outputFile=None
		
		if(not os.path.isfile(xmlpath)):
			raise Exception(xmlpath + "Can not find " + xmlpath)
		
	def createFileStructure(self):
			self.__cleanAll()
		
			self.__createFileList()
			
	def __cleanAll(self):
		self.outputFileList = []
		
	def getOutputFile(self):
		return self.outputFile
		
	def __createFileList(self):
		"""Purpose: Get a list of camFiles based on an xml-file"""
		
		#Parse xml file
		tree = ET.parse(self.xmlconfigfile)
		
		#Get xml root element..
		root = tree.getroot()

		#Get input data path
		for anElement in root.getiterator("inputDataPath"):
			self.inputDataPath = str(anElement.get("name"))	
	
		#Get output data path
		for anElement in root.getiterator("outputDataPath"):
			self.outputDataPath=str(anElement.get("name"))
		
		print "configuring output file"
		for child in root.getiterator("outputfile"):
			self.outputFile = camOutputFile(self.inputDataPath,self.outputDataPath,child)
			
			self.outputFile.configure()

			#Check if already exists
			if(os.path.isfile(self.outputFile.getFullPath())):
				raise Exception ("File to create : " + self.outputFile.getFullPath() + " already exists ! ==> exiting now")
			if(not os.path.isdir(os.path.dirname(self.outputFile.getFullPath()))):
				raise Exception("Creation directory does not exist")