'''
Created on Aug 30, 2013

@author: alfg
'''

from cdo import *
cdo = Cdo()
import sys
from camOsloFileProcessor import FileProvider
from interpolator import outputCreator
import argparse

#MAIN PROGRAM
if __name__ == '__main__':
	
	parser = argparse.ArgumentParser()
	#parser.add_argument("-s","--scenario",type=str,help="Scenario for which to create emissions (default historical)",default="historical")
	parser.add_argument("-x","--xmlConfigFile",type=str,help="config xml-file (default exampleConfig.xml)",default="exampleConfig.xml")
	#parser.add_argument("-c","--component",type=str,help="component to include (default BC)", default="BC")
	#parser.add_argument("-y","--years",type=str,help="comma separated min and max years (default: 0,3000)",default="0,3000")
	#parser.add_argument("-p","--provider",type=str,help="provider of emission data, for example \"IPCC\" or \"ECLIPSE\" (default IPCC)", default="IPCC")
	parser.add_argument("-r","--resolution",type=str,help="file describing model resolution (default camRegularGrid144x72)",default="camRegularGrid144x72")
	#parser.add_argument("-t","--sourcetype" ,type=str,choices=["ff","bb","air","all"],help="emission source to create (Default all)",default="all")
	args = parser.parse_args()
	
	############################################################
	#SET UP THE FILES WE WANT TO READ
	############################################################
	try:
		provider = FileProvider(args.xmlConfigFile)
	except Exception as e:
		print e
		sys.exit(1)
	
	try:
		provider.createFileStructure()
		aFile = provider.getOutputFile()
		if(aFile == None):
			print "No files to create for options : "
			print "scenario " + args.scenario
			print "xmlConfigFile " + args.xmlConfigFile
			print "component " + args.component
			print "provider "+args.provider
			print "resolution "+args.resolution
			print "sourcetype " + args.sourcetype  
			print "years" + args.years
			
	except Exception as e:
		print e
		print "a problem occurred when scanning the config file"
		sys.exit(1)
	
	#Make an output-creator and ask it to create the output based on the file list
	try:
		worker = outputCreator(provider)
		worker.doWork(args.resolution)
	except Exception as e:
		print e
		sys.exit(1)
	
	#Successful exit
	sys.exit(0)