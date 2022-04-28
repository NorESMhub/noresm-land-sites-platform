  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

MWH2SO4 = 98.0
MWNH4HSO4 = 114.0
MWScale = MWNH4HSO4/MWH2SO4

if (.not. isvar("variableName")) then
   variableName="SS_A3_OCW"
end if

model = "Oslo"

if (model .eq. "Oslo") then
   POST_process=(/"DDF","SFWET","SFSIC","SFSBC","SFSIS","SFSBS", "_mixnuc1"/)
   POST_reverse=(/False,True,True,True,True,True,True /)          ;Dry dep is a loss, but plotted as positive!
else
   POST_process=(/"DDF","SFWET","SFSIC","SFSBC","SFSIS","SFSBS", "_mixnuc1"/)
   POST_reverse=(/False,True,True,True,True,True,True /)          ;Dry dep is a loss, but plotted as positive!
end if

alternativeBurdenName=""
if(model .eq. "Oslo")
   ;Some tracers are lumped tracers
   if(variableName .eq. "sulfate")then
      subTracerList=(/ "SO4_A1", "SO4_A2", "SO4_AC", "SO4_N","SO4_NA", "SO4_PR"/)
      scaleFactor=(/  1.0, MWScale , 1.0, 1.0, 1.0, 1.0, 1.0/)
   else if (variableName .eq. "bc")then
      subTracerList=(/ "BC_N", "BC_AX", "BC_NI", "BC_AC","BC_A", "BC_AI"/)
      scaleFactor=(/  1.0, 1.0 , 1.0, 1.0, 1.0, 1.0, 1.0/)
   else if (variableName .eq. "om")then
      subTracerList=(/ "OM_NI", "OM_AI","OM_AC", "SOA_N", "SOA_NA", "SOA_A1" /)
      scaleFactor=(/  1.0, 1.0 , 1.0, 1.0, 1.0, 1.0 /)
   else if (variableName .eq. "dust") then
      subTracerList=(/ "DST_A2", "DST_A3" /)
      scaleFactor=(/  1.0, 1.0 , 1.0 /)
   else if (variableName .eq. "salt")then
      subTracerList=(/ "SS_A1", "SS_A2", "SS_A3"/)
      scaleFactor=(/  1.0, 1.0 , 1.0 /) 
   else
      subTracerList=(/variableName/)
      scaleFactor=(/1.0/)
   end if
   end if
   end if
   end if
   end if
else
   ;Some tracers are lumped tracers
   if(variableName .eq. "sulfate")then
      alternativeBurdenName="BURDENSO4"
      subTracerList=(/ "so4_a1", "so4_a2", "so4_a3"/)
      scaleFactor=(/  1.0, 1.0 , 1.0/)
   else if (variableName .eq. "bc")then 
      alternativeBurdenName="BURDENBC"
      subTracerList=(/ "bc_a1"/) 
      scaleFactor=(/  1.0/)
   else if (variableName .eq. "om")then
      alternativeBurdenName="BURDENPOM"
      subTracerList = (/"pom_a1"/)
      scaleFactor = (/ 1.0 /)
   else if (variableName .eq. "dust")then
      alternativeBurdenName="BURDENDUST"
      subTracerList = (/"dst_a1", "dst_a3"/)
      scaleFactor = (/ 1.0, 1.0 /)
   else if (variableName .eq. "salt")then
      alternativeBurdenName="BURDENSEASALT"
      subTracerList = (/"ncl_a1", "ncl_a2", "ncl_a3"/)
      scaleFactor = (/ 1.0, 1.0, 1.0 /)
   else
      subTracerList=(/variableName/)
      scaleFactor=(/1.0/)
   end if
   end if
   end if
   end if
   end if
end if

;THE FILE NAME TO USE
myFileName="/lustre/mnt/alfg/condTimeStep/R566SoaNucl/atm/hist/1982_1983_AVG.nc"
;myFileName="/disk1/alfg/noresmrun/PTAER/PTAERO1_1YR_HR/BF1NudgePD2000.cam.h0.1979_AVG.nc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; No changes by the user should be necessary below...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

myFile=addfile(myFileName,"r")

; Get the gaussian weights
wgty = myFile->gw

dim_PRE = 0
dim_POST = dimsizes(POST_process)
numberOfVariables = dim_PRE + dim_POST
dim_SUBTRACER = dimsizes(subTracerList)

;Name of all the processes to print
process=new((/numberOfVariables, dim_SUBTRACER/),string)
reverse=new(numberOfVariables,logical)
budget=new(numberOfVariables, float)

print("NV " + numberOfVariables)

;Do the plotting
format="pdf"
wks = gsn_open_wks(format,variableName)

;Go through the variables and check if they are in the file
var_idx = 0
numberOfVariablesUsed = 0
fieldOK = new((/numberOfVariables,dim_SUBTRACER/), logical)


   tracer_idx = 0
   do while (tracer_idx .lt. dim_SUBTRACER)

      ;Do initial processing and construct variable names to take out
      varName2 = subTracerList(tracer_idx)
      var_idx=0
      do while(var_idx .lt. dim_PRE)
         str_array = (/PRE_process(var_idx), varName2/)
         lookupVariable=str_concat(str_array)
         process(var_idx,tracer_idx) = lookupVariable
         reverse(var_idx) = PRE_reverse(var_idx)
         print("lookupVariable " + lookupVariable + "  " + var_idx)
         var_idx = var_idx + 1
      end do

      var_idx = 0
      do while(var_idx .lt. dim_POST)
         str_array = (/ varName2, POST_process(var_idx)/)
         lookupVariable=str_concat(str_array)
         process(dim_PRE+var_idx,tracer_idx) = lookupVariable
         reverse(dim_PRE+var_idx)=POST_reverse(var_idx)
         print("lookupVariable " + lookupVariable + "  " + (dim_PRE + var_idx))
         var_idx = var_idx + 1
      end do

      ;This is the column
      if(alternativeBurdenName .ne. "") then
         cbFieldSum = myFile->$alternativeBurdenName$
      else
         cbVarName = str_concat( (/"cb_",varName2/))
         print("column variable name is " + cbVarName)

         cbField= myFile->$cbVarName$

         if(tracer_idx .eq. 0)then
            cbFieldSum = cbField
         else
            cbFieldSum = cbFieldSum + cbField
         end if

         ;Need to save the un-touched cbField since
         ;coordinates are lost when multiplying by scale factor
         tmpField = cbField
         cbField = tmpField* scaleFactor(tracer_idx)

         copy_VarCoords(tmpField, cbField)
         copy_VarCoords(cbField, cbFieldSum)
      end if
      

      var_idx = 0
      do while (var_idx .lt. dim_POST + dim_PRE)
         ;Check if field exists in file 
         print("var_idx, tracer_idx" + var_idx +"  "+ tracer_idx)
         lookupVariable = process(var_idx,tracer_idx)
         ;print("searching for variable " + lookupVariable)
         fieldOK(var_idx,tracer_idx) = isfilevarcoord(myFile, lookupVariable, "lon")
         if(fieldOK(var_idx,tracer_idx))then
            if(tracer_idx .eq. 0)then
               numberOfVariablesUsed = numberOfVariablesUsed + 1
            end if
         else
         ;   print("Could not find field " + lookupVariable + " in file " + myFileName)
         end if
         var_idx = var_idx + 1
      end do

      tracer_idx = tracer_idx + 1
   end do ; tracers

print("Number of variables_USED " + numberOfVariablesUsed)

plot=new(numberOfVariablesUsed,graphic)

budget(:) = 0.0
var_idx_used = 0
var_idx=0
nv_found = 0
do while(var_idx .lt. numberOfVariables)

   if(var_idx .lt. dim_PRE)
      titleName = str_concat((/ model, " : ", variableName, " : ", PRE_process(var_idx)/) )
   else
      titleName = str_concat( (/ model, " : ", variableName, " : ", POST_process(var_idx-dim_PRE) /))
   end if
   print("TITLENAME " + titleName)

   if(fieldOK(var_idx,0))then

      tracer_idx = 0

      do while(tracer_idx .lt. dim_SUBTRACER)

         print("   ")
         lookupVariable = process(var_idx,tracer_idx)
         print("WILL TRY TO ACCESS " + lookupVariable)

         if(fieldOK(var_idx, tracer_idx))then
            print ("field " + lookupVariable + " is ok")
            budgetField= myFile->$lookupVariable$*scaleFactor(tracer_idx)
         else
            print(" field " + lookupVariable + "not ok ==> set to zero")
            budgetField = budgetField * 0.0;
         end if
      
         ;Reverse field if field is plotted the wrong way..
         if(reverse(var_idx))then
            print("reversing field  " + lookupVariable)
            budgetField = budgetField * (-1.0)
         end if

         copy_VarCoords(cbFieldSum, budgetField)

         ;Sum budget over several processes...
         if(tracer_idx .eq.0)then
            print("Initializing  budgetFieldSum")
            budgetFieldSum = budgetField
         else
            budgetFieldSum = budgetFieldSum + budgetField
         end if

         tracer_idx = tracer_idx + 1
      end do ; sub-tracers

      ;This is the lifetime
      myField2 =cbFieldSum / (budgetFieldSum + 1.e-30)/3600.0/24.0 ; days

      copy_VarCoords(budgetField, myField2)
      copy_VarCoords(budgetField, budgetFieldSum)

      print("WILL PLOT " + lookupVariable + " in " + var_idx_used)

      glAve1 = wgt_areaave_Wrap(budgetFieldSum, wgty, 1.0, 1)
      glAve2 = wgt_areaave_Wrap(cbFieldSum, wgty, 1.0, 1)
     
      print("avg burden " + glAve2 + " / avg loss " + glAve1)

      if(abs(glAve1) .gt. 1.e-100)then
         budget(var_idx)=(glAve2/glAve1)/3600.0/24.0 ;days
      else
         budget(var_idx) = 9999999; days
      end if

      ;Set intervals for plotting
      maxPT = budget(var_idx)*4.0 ;max(myField2)
      minPT = budget(var_idx)*0.2 ; 

      print("min/max "  + minPT + " " + maxPT)

      maxint = 10
      intervals = fspan(minPT, maxPT, maxint)

      res                   = True                  ; plot mods desired

      res@cnFillOn          = True                  ; color fill 
      res@cnLevelSelectionMode      = "ExplicitLevels"       ; manual levels
      res@cnLevels          = sprintf("%3.1e",intervals)
      res@gsnFrame          = False                 ; Do not draw plot
      res@gsnDraw           = False                 ; Do not advance frame

      res@gsnLeftString   = titleName  
      avgString = " LIFETIME = " + sprintf("%5.2f",budget(var_idx)) + " [ "+ "days" + " ]" 
      res@gsnRightString  = avgString

      print("Creating plot for var_idx used " + var_idx_used)
      plot(var_idx_used) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(myField2,0),res)    ; create the plot
   
      var_idx_used = var_idx_used + 1
    end if ; check if field OK

   var_idx=var_idx+1

;end do number of variables
end do

;create panel plot
pres                  = True
gsn_panel(wks,plot,(/4,3/),pres)              ; create panel plot

end
