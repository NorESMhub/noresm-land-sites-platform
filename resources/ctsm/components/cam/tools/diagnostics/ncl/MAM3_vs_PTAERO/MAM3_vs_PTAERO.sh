#!/bin/sh

#Create the plots
varlist="BC SO4 SS DUST OM BC_SRF SO4_SRF SS_SRF DUST_SRF OM_SRF SO2_SRF BC_CLOUD SO4_CLOUD SS_CLOUD DUST_CLOUD OM_CLOUD SF_DUST SF_SALT BC_CLFR OM_CLFR DUST_CLFR SS_CLFR SO4_CLFR BC_ZM OM_ZM DUST_ZM SS_ZM SO4_ZM"
for var in $varlist
do
   echo $var
   expression=\'var=\"$var\"\'
   cmd="ncl $expression ./MAM3_vs_PTAERO.ncl"
   echo $cmd
   eval $cmd
   pdf2ps $var.pdf
   ps2eps -f $var.ps
   epstopdf $var.eps
   #echo $alf
done

pdflatex burdens.tex
pdflatex burdens.tex

