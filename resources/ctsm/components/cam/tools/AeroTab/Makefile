##################################################################
# Makefile for multiple platforms
##################################################################
#
#Get machine info
UNAMES   = $(shell uname -s)
MAKEFILE = Makefile
MAIN = AeroTab
MAKDEP= ./makdep
MY_OBJ_DIR=./obj
#List of directories in which to search for source files
#MDL_PTH:=. ../../src/chemistry/oslo_aero 
####################################################
#Normally don't touch anything below this line 
#(except to add or remove a source file)

#Define some signs needed for the Makefile below
null=
comma=${null},${null}
space=${null} ${null}
kolon=${null}:${null}

#Directory where you store dependency files (same as MY_OBJ_DIR)
MY_DPN_DIR:=${MY_OBJ_DIR}

LDFLAGS :=

#Initialize includes (Directories to include when looking for files to compile)
INCLUDES:= 


# Set the right compiler and compiler options for the right architecture
# This part (and the directories above) should in theory be the only machine
# dependent things... 
#-----------------------------------------------------------------------------

#Set the value of DPN_GNR to makdep
DPN_GNR:=${MAKDEP}

#OPTIONS SET FOR GFORTRAN!!
FC          = gfortran
CC          = gcc
FIXEDFLAGS  = -ffixed-form
FREEFLAGS   = -ffree-form
STDOPT      = -fdefault-real-8 
FCOPT       = -O4
DEBUG       = -g -ggdb

OPTFLAGS=${FCOPT}

#############################################################################

#Set the Makefile VPATH variable (change space for colon in modelpath)
VPATH_TMP:=$(subst ${space},${kolon},${MDL_PTH})
# Source file names with directories removed
SRC_LST := shr_kind_mod.F90\
   commondefinitions.F90\
   AeroTab.f\
   specbands.f\
   constsize.f\
   hygro.f\
   tableinfo.f\
   openfiles.f\
   modepar.f\
   drydist.f\
   conteq.f\
   conteqlight.f\
   sizemie.f\
   chandrav.f\
   tabrefind.f\
   refind.f\
   miev0.f\
   coagsub.f\
   condsub.f\
   rhsub.f\
   rhsublight.f\
   koehler.f\
   mixsub.f\
   smolar.f\
   modetilp.f\
   modetilplight.f

# Prepend -I to use for compiler argument 
#include directories to search for #included files
MDL_INC := $(foreach dir,${MDL_PTH},-I${dir})

#Set model source to be SRC_LST
MDL_SRC:= ${SRC_LST}

#Expand includes
INCLUDES += ${MDL_INC}

#Set LDFLAGS (only needed if libraries. eg for including netcdf)
#LDFLAGS += -L${NETCDF_LIB} -lnetcdf -lnetcdff
LDFLAGS +=

#USR_TKN has to be changed for AIX where the preprocessing is of the form
#-WF,-DTKN1,-DTKN2.... All the other machines use the form -DTKN1 -DTKN2...
ifeq (${UNAMES},AIX)
USR_TKN := $(subst $(space)${space}$(space),$(null),${USR_TKN})  #Get rid of triple spaces
USR_TKN := $(subst $(space)${space},$(null),${USR_TKN})          #Get rid of double spaces
USR_TKN := $(subst $(space),$(null),${USR_TKN})                  #Get rid of single spaces
USR_TKN := $(subst -D,$(comma)-D,$(USR_TKN))                     #Change "-D" to ",-D"
USR_TKN2 := $(subst -WF${comma},${null},$(USR_TKN))              #Argument to c processor..
USR_TKN2 := $(subst ${comma},${space},$(USR_TKN2))               #..is on the form -D1 -D2
else #not AIX
USR_TKN2 := ${USR_TKN}  #Needed for compiling .F90 files
endif

#Includes for makdep (in should not look for netcdf.o)
#MAKDEPINC := $(subst -I${NETCDF_INC},$(null),${INCLUDES})
MAKDEPINC := ${INCLUDES}

#After VPATH_TMP has been expanded by all different modules,
#we now construct the variable VPATH by changing space with kolon
VPATH := $(subst $(space),$(kolon),${VPATH_TMP}) 

# List of object files is constructed directly from source files
# It is made made AFTER the source list is constructed
# In this statement we say that all object files are in MY_OBJ_DIR
# and that they are based on the files in MDL_SRC 
MDL_OBJ := ${addprefix ${MY_OBJ_DIR}/,${addsuffix .o, ${basename ${MDL_SRC}}}} 

OBJDIR = ${MY_OBJ_DIR}

#List of dependency files. Make will read the dependency files and find out what files need
#To be recompiled in case of a change in any file. This is useful if you use modules and #include
MDL_DPN := $(addprefix ${MY_OBJ_DIR}/,$(addsuffix .d, $(basename ${MDL_SRC})))

# limit what files to check for updates
.SUFFIXES:
.SUFFIXES: .f .f90 .F .F90 .c .o

#Pattern rules: Tell make how to construct a pattern from another pattern 
#Note the pattern to construct *.f90 from *.F90: The HP-UX compiler does not recognize .F90 as fortran code
#Other compilers recognize *.F90 as fortran code, but it is better to to it general for all compilers. 
#Normally, *.F90 is fortran code in free format with CPP statements (#ifdefs)
#http://devrsrc1.external.hp.com/STK/man/11.20/f90_1.html
${MY_OBJ_DIR}/%.o : %.f
	${FC} ${FIXEDFLAGS} ${STDOPT} ${INCLUDES} ${OPTFLAGS} -o ${MY_OBJ_DIR}/${notdir $@} -c $<
${MY_OBJ_DIR}/%.o : %.F
	${FC} ${FIXEDFLAGS} ${STDOPT} ${INCLUDES} ${OPTFLAGS} ${USR_TKN} -o ${MY_OBJ_DIR}/${notdir $@} -c $<
${MY_OBJ_DIR}/%.o : %.f90
	${FC} ${FREEFLAGS} ${STDOPT} ${INCLUDES} ${OPTFLAGS} -o ${MY_OBJ_DIR}/${notdir $@} -c $<
%.f90 : %.F90
	${CC} -C -E ${INCLUDES} ${USR_TKN2} $< > $@

#Dependency rules Generate dependency files which tells Make what files to 
#recompile and not after a given change in the code. You need the makdep program
#to do this. The last line in the makefile -include ${MDL_DPN} tells make to use this.
#Makdep is publicly available, f.ex. http://dust.ps.uci.edu/dead/makdep.c
#Compile it with ${CC} -o makdep makdep.c and put the binary somewhere in your path.
${MY_DPN_DIR}/%.d : %.F
	@echo "Building dependency file $@"
	${DPN_GNR} -f ${MAKDEPINC} -D ${MY_DPN_DIR} -O ${MY_OBJ_DIR} -s f -s f90 $< > $@
${MY_DPN_DIR}/%.d : %.f
	@echo "Building dependency file $@"
	${DPN_GNR} -f ${MAKDEPINC} -D ${MY_DPN_DIR} -O ${MY_OBJ_DIR} -s f -s f90 $< > $@
${MY_DPN_DIR}/%.d : %.F90
	@echo "Building dependency file $@"
	${DPN_GNR} -f ${MAKDEPINC} -D ${MY_DPN_DIR} -O ${MY_OBJ_DIR} -s f -s f90 $< > $@
${MY_DPN_DIR}/%.d : %.f90
	@echo "Building dependency file $@"
	${DPN_GNR} -f ${MAKDEPINC} -D ${MY_DPN_DIR} -O ${MY_OBJ_DIR} -s f -s f90 $< > $@

#Make will try to execute the first target of the Makefile
.PHONY: all 
all: ${MAIN} ${MDL_OBJ} ${MDL_DPN}
# I think .PHONY tells make that "all" is a non file target
# I am not sure if it helps, but the manual recommends it.

# Here I tell make only to update MAIN if 
# MDL_OBJ is newer
${MAIN}: ${MDL_OBJ} 
	@echo COMPILING AND LINKING MAIN
	${FC} ${STDOPT} ${OPTFLAGS} -o $@ ${MDL_OBJ} ${LDFLAGS}

${MDL_OBJ}: | ${OBJDIR}

${MDL_DPN}: | ${OBJDIR}

${MDL_DPN}: | ${MAKDEP}


${MAKDEP}: makdep.c
	cc -o ./makdep makdep.c

$(OBJDIR):
	mkdir -p $(OBJDIR)

#We need new object files if the Makefile is newer
${MDL_OBJ}: ${MAKEFILE}

#We need new dependency files if the Makefile is newer
${MDL_DPN}: ${MAKEFILE}

#Check if what you think is really set in this makefile 
#The command "make check" will print all the stuff below
check:
	@echo SOURCE FILES ${MDL_SRC}
	@echo MDL_OBJ ${MDL_OBJ}
	@echo INCLUDES ${INCLUDES}
	@echo makdepinc ${MAKDEPINC}
	@echo CORE ${CORE_SRC}
	@echo USR_TKN ${USR_TKN}
	@echo FIXEDFLAGS ${FIXEDFLAGS}
	@echo MY_OBJ_DIR ${MY_OBJ_DIR}
	@echo OBJDIR ${OBJDIR}
	@echo MAIN ${MAIN}	
	@echo MAKEFILE ${MAKEFILE}
	@echo Dependency files ${MDL_DPN}
	@echo VPATH_TMP ${VPATH_TMP}
	@echo VPATH ${VPATH}
	@echo comma a${comma}a
	@echo space a${space}a
	@echo kolon a${kolon}a
	@echo USR_TKN2 ${USR_TKN2}
	@echo MAKDEP ${MAKDEP}
# Clean up 
clean:
	rm -f ${MY_OBJ_DIR}/*.o
	rm -f ${MY_DPN_DIR}/*.d
	rm -f ${MDL_DPN}
	rm -f ${MDL_OBJ}
	rm -f loader.info
	rm -f ${MAIN}
	rm -f core
	rm -f *.mod
	rm -f *~	

#This last part includes the dependencies
#But we only want to do this for options which actually compile something
#That's why we want to check on "GOALS_WHICH...BLA BLA.."
INCLUDE_DPN := TRUE
GOALS_WHICH_IGNORE_DEPENDENCY_FILES := clean clean_all check
ifeq (${null},$(findstring $(MAKECMDGOALS),${GOALS_WHICH_IGNORE_DEPENDENCY_FILES}))
 INCLUDE_DPN := TRUE
else
 INCLUDE_DPN := FALSE
endif
ifeq (${INCLUDE_DPN},TRUE)
# Following incorporates dependency files into Makefile rules
-include ${MDL_DPN}
endif
