'''
Created on Sep 2, 2013

@author: alfg
'''

import sys
import os
from cdo import *
cdo = Cdo()
from camOsloFileProcessor import camInputFile, camOutputFile, FileProvider, levelFractionFile
import uuid
import glob
import datetime
import shutil
import progressbar
import time

areaEarth = 5.1e14

##################################################################

class outputCreator(object):
	'''
	classdocs
	'''

	def __init__(self, fileProvider):
		'''
		Constructor
		'''		
		self.FileProvider = fileProvider
		self.dateMap = dict()
		
	def __getOutputFile(self):
		return self.FileProvider.getOutputFile()
	
	
	def __createTmpFileNames(self,anOutputFile,n):
		
		retVal = []
		
		aUuid = str(uuid.uuid4())
		
		nFiles = 0
		while nFiles < n:
			retVal.append(os.path.normpath(os.path.join(anOutputFile.getOutputPath(),aUuid+"_"+str(nFiles)+".nc")))
			nFiles = nFiles +1
		return retVal
		
	def __create3DFromLevelFractions(self,timestepFile, input2DFile, anOutputFile, aDate):
		"""
		Creates 3D output from 1d input and level fractions
		"""
		##################################################
		#multiplying by layer fraction messes up the dates
		timestepFileDate=cdo.showdate(input=timestepFile)[0]
		
		#This is a file with a variable "fraction" which is vertical fraction to be 
		#applied to the 1D input file
		lfname=self.__createTmpFileNames(anOutputFile,1)
		levelFractionFileName = input2DFile.getLevelFractionFileReGrid()
		aLevelFractionFile=levelFractionFile(levelFractionFileName)
		closestTimeStep = aLevelFractionFile.findClosestTimeStep(aDate)
		cdo.seltimestep(closestTimeStep,input=levelFractionFileName,output=lfname[0])
		
		interfaceLevels = input2DFile.getInterfaceLevels()
		
		#Get all the levels from the 2D input file
		theLevels = input2DFile.getLevelValues()
		
		uuidLevel = str(uuid.uuid4())
		aPattern=os.path.normpath(anOutputFile.getOutputPath()+"/"+uuidLevel)
		aPattern2 = aPattern+"*.nc"
		
		cdo.splitlevel(input=lfname[0],output=aPattern)
	
		#These are the fields we want to multiply with this fraction
		fields = input2DFile.getConfiguredFields()
	
		#Get back all the level fraction files
		levelFiles = glob.glob(aPattern2)
		sortedLevelFiles = [""]*len(levelFiles)
		foundLevel=[False]*len(levelFiles)
		
		for lf in levelFiles:
			aLevel = cdo.showlevel(input=lf)
			cnt = 0
			for lev in theLevels:
				if(str(aLevel[0]) == str(int(lev)) ):
					sortedLevelFiles[cnt] = lf
					foundLevel[cnt]=True
				cnt += 1
		
		if(False in set(foundLevel)):
			raise Exception("Inconsistent levels in fractionFile and xml file")
		
		levelFileList=[]
		cnt=0
		for levelFile in sortedLevelFiles:
			workFiles = self.__createTmpFileNames(anOutputFile,2)
			
			#Remove level-dimension in the fraction-file
			subprocess.check_call(["ncwa","-O","-a","height",levelFile,workFiles[0]])
			
			#set correct date in the workfile
			cdo.setdate(timestepFileDate,input=workFiles[0],output=workFiles[1])
			
			#put "fraction" in the inputfile (which is the time step we want to treat)
			subprocess.check_call(["ncks","-A","--fix_rec_dmn","-v","fraction",workFiles[1],timestepFile])
		
			height = str(float(interfaceLevels[cnt+1]) - float(interfaceLevels[cnt]))
			cnt += 1
		
			#Go through fields and multiply by fraction and level height
			expr="'"
			for aField in fields:
				expr += aField+"="+aField+"*fraction"+"/"+height+"*1.e-2;"
				
			#remove last ";"
			expr=expr[:-1]
			expr+="'"
			
			cdo.expr(expr,input=timestepFile,output=workFiles[1])
			
			levelFileList.append(workFiles[1])
			
			self.__cleanButLeave(workFiles,1)
			
		self.__cleanButLeave(sortedLevelFiles)
		self.__cleanButLeave(lfname)

		#forward to the subroutine which creates a 3D-file from a list of 1d-files
		a3DFile = self.__create3DFromFixedFiles(levelFileList,anOutputFile)
		try:
			os.remove(timestepFile)
		except:
			pass
		return a3DFile
		
		
	##############################################################################
	def __create3DFrom1D(self,timestepFile, input2DFile,anOutputFile):
		"""
		Convert a 1D file to a 3D file with correct number of levels
		"""
		#When specific levels are specified, it is the output levels we are talking about
		inputLevels = len(anOutputFile.getLevelValues())
		tmpLayerFiles = []
		level=1
		while (level <= inputLevels):
			
			#In this case levels are about output file levels
			layerHeight = anOutputFile.getLevelHeight(level-1)
			
			workFiles = self.__createTmpFileNames(anOutputFile,3)
			
			#get the fields to be placed in this layer
			fieldsInLayer = input2DFile.getConfiguredFieldsInLayer(level)
			allFields = input2DFile.getConfiguredFields()
			fieldsNotInLayer= set(allFields) - set(fieldsInLayer)
			
			anExpression="'"
			for field in fieldsInLayer:
				anExpression+=field+"="+field+"*1.e-2/"+str(layerHeight) #==> molec/cm2/s ==> molec/cm2/sec
			for field in fieldsNotInLayer:
				anExpression+=field+"="+field+"*0.0;"
			anExpression=anExpression[:-1]
			anExpression+="'"
			#execute expression
			cdo.expr(anExpression,input=timestepFile, output=workFiles[1])
			
			#Remove record dimension using ncks ==> create file without record dimension..
			subprocess.check_call(["ncks","--fix_rec_dmn", workFiles[1], workFiles[2]]) 
			
			#Remember the file for this layer
			tmpLayerFiles.append(workFiles[2])
			
			#Leave all but this file
			self.__cleanButLeave(workFiles,2)
			
			#increase levels created
			level+=1
		
		#Done loop of levels to create
		return self.__create3DFromFixedFiles(tmpLayerFiles,anOutputFile)
			
	################################################################################		
	def __create3DFromFixedFiles(self,tmpLayerFiles,anOutputFile):
		
		workFiles = self.__createTmpFileNames(anOutputFile,2)
		#Create new dimension "record" which is actually layers
		myCall = []
		myCall.append("ncecat")
		for fixedTmpFile in tmpLayerFiles:
			myCall.append(fixedTmpFile)
		myCall.append(workFiles[0])
		subprocess.check_call(myCall)
		
		#Rename record ==level
		subprocess.check_call(["ncrename","-d","record,level",workFiles[0]])
		
		#change dimensions
		subprocess.check_output(["ncpdq","-a","time,level",workFiles[0],workFiles[1]])
			
		self.__cleanButLeave(workFiles,1)
		self.__cleanButLeave(tmpLayerFiles)
		return workFiles[1]
	
	#############################################################################
	def __cleanButLeave(self,fileList,leave=-99):
		index = 0
		for aFile in fileList:
			if(index != leave and os.path.isfile(aFile)):
				os.remove(aFile)
			index = index + 1
			
	##############################################################################
	def __mergeOutputFiles(self,anOutputFile):
		
		#Now we have gone through all input files and have a bunch of temporary input files valid for different dates
		#Need to merge all files with same date!
		#We add up all the files which have the same date.
		#The code below is needed because "cdo add" only takes two arguments..
		
		#These are the files with will go to final cdo.mergetime, the files in outfilelist have different dates
		outfiles = self.__createTmpFileNames(anOutputFile,1)
		
		dateFiles=[]
		#Go through the files with same dates
		for aKey in self.dateMap:
			fileString=""
			workFiles = self.__createTmpFileNames(anOutputFile,2)
			aFileList = []	
			aFileList.extend(self.dateMap.get(aKey))  #All the files we need to add up (the ones with same date)
			fileString += " ".join(aFileList)
			fileString += " "
			#Create only one file per date (with all the entries in it)
			#print "merging for date" + aKey  + " " + fileString
			#xyz = cdo.showdate(input=fileString)
			#print "date in input  :" + xyz[0]
			cdo.merge(input=fileString,output=workFiles[0])
			dateFiles.append(workFiles[0])
			self.__cleanButLeave(aFileList)
			
		fileString=" ".join(dateFiles)
		cdo.mergetime(input=fileString, output=outfiles[0])
		#print "merged timesteps " + fileString 
			
		self.__cleanButLeave(dateFiles)
			
		print "Created "+ outfiles[0]+ " with dates " + cdo.showdate(input=outfiles[0])[0]		
		
		return outfiles[0]

	#########################################################################################################
	def __checkTotals(self,anOutputFile):
		#Done one output file, check totals, print to screen
		nOutTimeStep = 1
		workFiles = self.__createTmpFileNames(anOutputFile,2)
		for aKey in self.dateMap:
			cdo.seltimestep(nOutTimeStep, input=anOutputFile.getFullPath(), output=workFiles[0] )
			cdo.vertsum(input=workFiles[0], output=workFiles[1])
			os.remove(workFiles[0])
			cdo.fldmean(input=workFiles[1], output=workFiles[0])
			os.remove(workFiles[1])
			output = cdo.infon(input=workFiles[0])
			fieldInfo = output[len(output)-1]
			fieldInfoList = fieldInfo.split(" ")
			fieldMean = fieldInfoList[len(fieldInfoList)-1]
			fieldMeanNbr = float(fieldMean)
			timesAearth = fieldMeanNbr*areaEarth*3600.0*24.0*365.0/1.e9
			print("field " + "  "+ str(aKey) + "  MEAN: "+ fieldMean + "  TIMES AEARTH*AYEAR(Tg): " + str(timesAearth))
			#clean up
			for aFile in workFiles:
				if(os.path.isfile(aFile)):
					os.remove(aFile)

	##########################################################################################
	
	def doWork(self,resolutionFile):

		if(not os.path.isfile(resolutionFile)):
			raise Exception("Can not find resolution file : "+ str(resolutionFile))


		time_startAll = time.time()
		nOutputFiles = 0
		anOutputFile = self.__getOutputFile()

		#Datemap is only valid for one output file
		self.dateMap = dict()

		time_start = time.time()

		nOutputFiles = nOutputFiles + 1
		
		#IF WE NEED A 3D-FILE, PUT IT TO OUTPUT-RESOLUTION NOW
		outputZaxisDef=""
		if(anOutputFile.getNumberOfLayers() > 0):
			stringList = [str(i) for i in anOutputFile.getLevelValues()]
			outputZaxisDef = ",".join(stringList)
					
		print"*********************************************************"
		print"*********************************************************"
		print "==> creating output file " + anOutputFile.getFullPath() + "  "
		print"*******************************************************"
		aUuid = str(uuid.uuid4())                              #temporary id for this output file
		maxInputFiles = len(anOutputFile.getInputFileList())
		inputFileCounter = 0
		for anInputFile in anOutputFile.getInputFileList():    #Go through all input files contributing to this file
			inputFileCounter = inputFileCounter  + 1
			inputFullPath = anInputFile.getFullPath()          #Get the input file
			
			#Write output-values in m in z-axis definition
			f = open('inputZAxisDef', 'w')
			levels = anInputFile.getLevelValues()
			#if the input file does not have levels, then we use the output z-axis
			if(len(levels) == 0):
				levels = anOutputFile.getLevelValues()
			f.write("zaxistype = height \n")
			f.write("size = "+ str(len(levels)) + " \n" )
			stringList = [str(i) for i in levels]
			expr = " ".join(stringList)
			f.write("levels =" + expr + "\n")
			f.close()
			
			#Create one file per date!!
			print "  ==>preparing output based on  " + os.path.basename(inputFullPath) + " " + str(int(float(inputFileCounter)/float(maxInputFiles)*100.0))+"%"
			dateStrings = cdo.showdate(input=inputFullPath)[0].split()
			ntimestep = 1
			maxtimesteps = len(dateStrings)
			datebar = progressbar.ProgressBar(maxval=maxtimesteps,widgets=[progressbar.Bar('x', '[', ']'), ' ', progressbar.Percentage()])
			for aDateString in dateStrings:
				
				#Create some tmp-filenames which will be created for this input file and date
				datebar.update(ntimestep)

				ymd = aDateString.split("-")
				aDate = datetime.date(int(ymd[0]),int(ymd[1]),int(ymd[2]))
				
				#print("year is " + str(aDate.year)+  " max/min:  " + str(anOutputFile.getMaxYear()) +"/"+ str(anOutputFile.getMinYear()))
				if(aDate.year > anOutputFile.getMaxYear() or aDate.year < anOutputFile.getMinYear()):
					ntimestep += 1
					#print("skipping input for year " +str(aDate.year) +"in " + anInputFile.getFullPath())
					continue
				
				workFiles = self.__createTmpFileNames(anOutputFile,4)
				
				#print "    ==>found date in " +anInputFile.getFullPath() + aDate.isoformat() +  "   " + str(int(float(ntimestep)/float(maxtimesteps)*100.0)) + "%"
			
				#Select the time corresponding to this time step in the file workFiles[0]
				cdo.seltimestep(ntimestep, input=anInputFile.getFullPath(), output=workFiles[0])
				
				#print "created file for timestep " + str(ntimestep) + workFiles[0]
				#print "created file has date " + cdo.showdate(input=workFiles[0])[0]
				
				#print("processing timestep "+str(ntimestep) + " in " + anInputFile.getFullPath() )
				
				timeStepFile=""
				
				#FOUR OPTIONS
				#1) OUTPUT FILES HAS LEVELS AND INPUT FILE DOES NOT... 
				#==> CREATE FROM LEVEL FRACTIONS
				if(anOutputFile.getNumberOfLayers() > 0 and (not anInputFile.getHasVerticalProfile()) and 
					anInputFile.hasLevelFractionFile()):
					
					timeStepFile = self.__create3DFromLevelFractions(workFiles[0], anInputFile , anOutputFile, aDate)
					
				#2) OUTPUT FILE HAS LEVELS AND INPUT FILE DOES NOT HAVE LEVELS ==> 3D file in workfiles[0]
				#==>PLACE SOME STUFF IN CONFIGURED LEVELS
				elif(anOutputFile.getNumberOfLayers() > 0 and (not anInputFile.getHasVerticalProfile())):
					timeStepFile = self.__create3DFrom1D(workFiles[0], anInputFile, anOutputFile )
				
				#3) BOTH FILES HAVE LAYERS ==> JUST INTERPOLATE
				elif(anOutputFile.getNumberOfLayers() > 0 and (anInputFile.getHasVerticalProfile() )):
					timeStepFile = workFiles[0]
				
				elif(anOutputFile.getNumberOfLayers() == 0 and anInputFile.getNumberOfLevels() == 0 ):
				#4) OUTPUT FILE DOES NOT HAVE LEVELS, AND INPUT FILE DOES NOT HAVE LEVELS
					timeStepFile = workFiles[0]
				else:
					raise("undefined case")
				
				if(anOutputFile.getNumberOfLayers() > 0 ):
					cdo.setzaxis("inputZAxisDef",input=timeStepFile,output=workFiles[1])
					cdo.intlevelx(outputZaxisDef,input=workFiles[1],output=workFiles[2]) #interpolate to output levels
				else:
					shutil.move(workFiles[0],workFiles[2])
					
				try:
					os.remove(timeStepFile)
				except:
					pass
				
				#set correct unit attribute (required by mozart)
				if (anOutputFile.getNumberOfLayers() > 0):
					fileUnits="molecules/cm3/s"
				else:
					fileUnits="molecules/cm2/s"
				for aField in anInputFile.getConfiguredFields():
					att_dsc=str("units,"+str(aField)+",o,c,"+fileUnits)
					subprocess.check_call(["ncatted","-a",att_dsc,workFiles[2]]) 
				
				###############################################################################
					
				#Do the regridding (surface grid)
				cdo.remapbil(resolutionFile,input=workFiles[2],output=workFiles[3])
				
				#Remove these tmp-files ==> no longer needed
				self.__cleanButLeave(workFiles,3)
				
				#Modify the list of files per date
				aList = []
				if (self.dateMap.has_key(aDate.isoformat())):
					#==> we already have input files for this date
					aList = self.dateMap.get(aDate.isoformat())
				#Put the new list back in map
				aList.append(workFiles[3])  
				self.dateMap[aDate.isoformat()] = aList      
					
				#prepare for next time step
				ntimestep = ntimestep + 1 
				
			#Done loop on dates in one input file
			anInputFile.cleanFiles()
		#Done loop on all inputfiles
		
		#Add up all input file from "self.datemap" searched into one output-file
		wrongCoordinateFile = self.__mergeOutputFiles(anOutputFile) 
		
		################################################################
		#create output z-axis definition (in km needed for MAM)
		#Write output-values in km in z-axis definition
		f = open('outputZAxisDef', 'w')
		levels = anOutputFile.getLevelValues()  #level values in km
		l2=[]
		for aLevel in levels:
			l2.append(float(aLevel)*1.e-3) #==> km
		f.write("zaxistype = height \n")
		f.write("size = "+ str(anOutputFile.getNumberOfLayers()) + " \n" )
		stringList = [str(i) for i in l2]
		expr = " ".join(stringList)
		f.write("levels =" + expr + "\n")
		f.close()
		
		workFiles2 = self.__createTmpFileNames(anOutputFile,2)
		cdo.setzaxis("outputZAxisDef",input=wrongCoordinateFile,output=workFiles2[0])
		
		#create the interface levels
		f = open("foo.cdl", 'w')
		f.write("netcdf foo{ \n")
		f.write("dimensions:")
		f.write("altitude_int = " + str(anOutputFile.getNumberOfLayers()+1) + ";\n")
		f.write("variables: \n")
		f.write("float altitude_int(altitude_int) ;\n")
		f.write("data: \n")
		interfaceLevels = anOutputFile.getInterfaceLevels()
		il2=[]
		for aLevel in interfaceLevels:
			il2.append(str(float(aLevel)*1.e-3))
		expr = ",".join(il2)
		f.write("altitude_int="+expr+"; \n")
		f.write("} \n")
		f.close()
		
		#Add the axis definition used by mozart
		subprocess.check_call(["ncgen","foo.cdl"])                          #create foo.nc
		subprocess.check_call(["ncks","-A","foo.nc",workFiles2[0]]) #append to wrong coordinate file
		subprocess.check_call(["ncrename","-d","height,altitude",workFiles2[0]]) #rename dimension height to altitude
		subprocess.check_call(["ncrename","-v","height,altitude",workFiles2[0]]) #rename variable height to altitude
		subprocess.check_call(["ncatted","-O","-a","units,altitude,o,c,km",workFiles2[0]])
		subprocess.check_call(["ncatted","-O","-a","units,altitude_int,o,c,km",workFiles2[0]])
		#shutil.move(workFiles2[0],anOutputFile.getFullPath())
		os.remove(wrongCoordinateFile)
		os.remove("foo.cdl")
		os.remove("foo.nc")
		
		#create the "date" variables needed by mozart levels
		f = open("dates.cdl", 'w')
		f.write("netcdf dates{ \n")
		f.write("dimensions:")
		f.write("time = " + str(len(self.dateMap)) + ";\n")
		f.write("variables: \n")
		f.write("int date(time) ;\n")
		f.write("data: \n")
		il2=[]
		for aKey in self.dateMap:
			dateVal = aKey.replace("-","")
			il2.append(int(dateVal))
		il2.sort()
		il4=[]
		for aKey2 in il2:
			il4.append(str(aKey2))
		expr = ",".join(il4)
		f.write("date="+expr+"; \n")
		f.write("} \n")
		f.close()
		
		#Add the date used by mozart
		subprocess.check_call(["ncgen","dates.cdl"])                          #create dates.nc
		subprocess.check_call(["ncks","-A","dates.nc",workFiles2[0]]) #append to wrong coordinate file
		
		
		shutil.move(workFiles2[0],anOutputFile.getFullPath())
		os.remove("dates.cdl")
		os.remove("dates.nc")
		
		#Check how much time was spent for this file..
		time_now = time.time()
		print "Time spent  for " + anOutputFile.getFullPath() + " " + str(int((time_now -time_start)/60.0)) + " minutes"
		print "Total time spent  : " + str(float((time_now -time_startAll)/60.0)) + " minutes"
		time_start = time_now
		
		#Remove all tmp-files needed to create this output file
		pattern=os.path.normpath(anOutputFile.getOutputPath()+"/*"+aUuid+"*")
		filelist = glob.glob(pattern)
		for tmpFile in filelist:
			os.remove(tmpFile)
		print "                         "

		#Remove all input files which were ever created for this output file
		for aKey in self.dateMap:
			aFileList = self.dateMap[aKey]
			self.__cleanButLeave(aFileList)
			
		#Check totals
		#self.__checkTotals(anOutputFile)
			
		#end of function
		return
					
				
				
				