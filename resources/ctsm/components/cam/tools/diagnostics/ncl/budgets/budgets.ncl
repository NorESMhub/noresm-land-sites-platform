  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

if (.not. isvar("variableName")) then
   variableName="BC_N"
end if

model = "Oslo"

if (model .eq. "Oslo") then
   ;SOME PROCESSSES HAVE THE PROCESS-ID IN FRONT OF THE VAR-NAME
   PRE_process=(/"SF","GS_","AQ_","DF_", "WD_A_"/)
   PRE_reverse=(/False, False, False,False,False/)
   ;SOME PROCESSES HAVE THE PROCESS-ID AFTER THE VAR-NAME
   POST_process=(/"_mixnuc1","DDF","SFWET","condTend","coagTend","clcoagTend"/)
   POST_reverse=(/False,True,False,False,False,False /)          ;Dry dep is a loss, but plotted as positive!
else
   ;MAM-MODEL
   ;SOME PROCESSSES HAVE THE PROCESS-ID IN FRONT OF THE VAR-NAME
   PRE_process=(/"SF","GS_","AQ_"/)
   PRE_reverse=(/False, False, False/)
   ;SOME PROCESSES HAVE THE PROCESS-ID AFTER THE VAR-NAME
   POST_process=(/"_mixnuc1","DDF","SFWET","_sfcoag1","_sfcsiz3","_sfcsiz4","_sfgaex1","_sfgaex2","_sfnnuc1","AQH2SO4","AQSO4"/)
   POST_reverse=(/False,True,False,False,False,False,False,False,False,False,False /)          ;Dry dep is a loss, but plotted as positive!
end if

;THE FILE NAME TO USE
;myFileName="/disk1/alfg/noresmrun/PTAER/MAM3_1YR_HR/MAM3_HR_NUDGE2_1979_AVG.nc"
myFileName="//disk1/oyvindse/NorESM2/aerbudgets/budgetfiles/NFAMIPNUDGEPTAERO_f09_f09_v519_so4n.cam.h0.1980.nc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; No changes by the user should be necessary below...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

myFile=addfile(myFileName,"r")

; Get the gaussian weights
wgty = myFile->gw

dim_PRE = dimsizes(PRE_process)
dim_POST = dimsizes(POST_process)
numberOfVariables = dim_PRE + dim_POST

;Name of all the processes to print
process=new(numberOfVariables,string)
reverse=new(numberOfVariables,logical)
budget=new(numberOfVariables, float)

;Do initial processing and construct variable names to take out
var_idx=0
do while(var_idx .lt. dim_PRE)
   str_array = (/PRE_process(var_idx), variableName/)
   lookupVariable=str_concat(str_array)
   process(var_idx) = lookupVariable
   reverse(var_idx) = PRE_reverse(var_idx)
   print("lookupVariable " + lookupVariable + "  " + var_idx)
   var_idx = var_idx + 1
end do

var_idx = 0
do while(var_idx .lt. dim_POST)
   str_array = (/ variableName, POST_process(var_idx)/)
   lookupVariable=str_concat(str_array)
   process(dim_PRE+var_idx) = lookupVariable
   reverse(dim_PRE+var_idx)=POST_reverse(var_idx)
   print("lookupVariable " + lookupVariable + "  " + (dim_PRE + var_idx))
   var_idx = var_idx + 1
end do

print("NV " + numberOfVariables)

;Do the plotting
format="pdf"
wks = gsn_open_wks(format,variableName)

;Go through the variables and check if they are in the file
var_idx = 0
numberOfVariablesUsed = 0
fieldOK = new(numberOfVariables, logical)
do while(var_idx .lt. numberOfVariables)
   budget(var_idx) = 0.0
   ;Check if field exists in file 
   lookupVariable = process(var_idx)
   ;print("searching for variable " + lookupVariable)
   fieldOK(var_idx) = isfilevarcoord(myFile, lookupVariable, "lon")
   if(fieldOK(var_idx))then
      numberOfVariablesUsed = numberOfVariablesUsed + 1
   else
   ;   print("Could not find field " + lookupVariable + " in file " + myFileName)
   end if
   var_idx = var_idx + 1
end do

print("Number of variables_USED " + numberOfVariablesUsed)

plot=new(numberOfVariablesUsed,graphic)

var_idx_used = 0
var_idx=0
nv_found = 0
do while(var_idx .lt. numberOfVariables)

   lookupVariable = process(var_idx)
   print("WILL TRY TO PLOT " + lookupVariable)

   if(fieldOK(var_idx))then
      print ("field " + lookupVariable + " is ok")
      myField= myFile->$lookupVariable$

      ;Reverse field if field is plotted the wrong way..
      if(reverse(var_idx))then
         myField = myField * (-1.0)
      end if

      print("WILL PLOT " + lookupVariable + " in " + var_idx_used)

      ;Set intervals for plotting
      maxPT = max(myField)
      minPT = min(myField)

      maxint = 10
      intervals = fspan(minPT, maxPT, maxint)

      res                   = True                  ; plot mods desired

      res@cnFillOn          = True                  ; color fill 
      ;res@cnLevelSelectionMode      = "ExplicitLevels"       ; manual levels
      res@cnLevels          = sprintf("%3.1e",intervals)
      res@gsnFrame          = False                 ; Do not draw plot
      res@gsnDraw           = False                 ; Do not advance frame

      glAve = wgt_areaave_Wrap(myField, wgty, 1.0, 1)
      budget(var_idx)=glAve

      res@gsnLeftString   = lookupVariable  
      avgString = " AVG = " + sprintf("%5.2e",glAve) + " [ "+ myField@units + " ]" 
      res@gsnRightString  = avgString

      plot(var_idx_used) = gsn_csm_contour_map_ce(wks,dim_avg_n_Wrap(myField,0),res)    ; create the plot
   
      var_idx_used = var_idx_used + 1
    end if

   var_idx=var_idx+1

;end do number of variables
end do

var_idx=0
totprod = 0.0
totloss = 0.0
do while(var_idx .lt. numberOfVariables)
   print("BUDGET " + var_idx + "  " + process(var_idx) + budget(var_idx))
   if(budget(var_idx) .gt. 0.0) then
      totprod = totprod + budget(var_idx)
   else
      totloss = totloss + budget(var_idx)*(-1.0)
   end if
   var_idx = var_idx + 1
end do

totalBudget = totprod - totloss
totalBudgetString = variableName + " : PROD : " + sprintf("%5.2e",totprod) + "; LOSS : " + sprintf("%5.2e",totloss) + " ; NET : " + sprintf("%5.2e",totalBudget) 

print("FINAL STRING   " + totalBudgetString)

;create panel plot
pres                  = True
pres@txString=totalBudgetString
gsn_panel(wks,plot,(/4,3/),pres)              ; create panel plot

end
