#!/bin/sh

#Write heading for latex output document
echo "\\documentclass{beamer}" > output.tex
echo "\\usetheme{Frankfurt}" >> output.tex
echo "\\usepackage{color}" >> output.tex
echo "\\\\begin{document}" >> output.tex
echo "\\\\title{Component budgets}" >> output.tex
echo "\\\\frame{\\\\titlepage}" >> output.tex

#Create the plots
#varlist="BC SO4 SS DUST OM BC_SRF SO4_SRF SS_SRF DUST_SRF OM_SRF SO2_SRF BC_CLOUD SO4_CLOUD SS_CLOUD DUST_CLOUD OM_CLOUD"
varlist="SO4_N SO4_NA SO4_A1 SO4_A2 SO4_AC SO4_PR BC_N BC_AC BC_AX BC_NI BC_A BC_AI OM_NI OM_AI OM_AC DST_A2 DST_A3 SS_A1 SS_A2 SS_A3 SOA_A1 SOA_N SOA_NA"
varlist2=""

for var in $varlist
do
   varlist2="$varlist2 $var ${var}_OCW"
done

#for MAM, the above does not work, so uncomment the below for MAM 
#varlist2="bc_a1 bc_c1 so4_a1 so4_a2 so4_a3 so4_c1 so4_c2 so4_c3 dst_a1 dst_a3 ncl_a1 ncl_a2 ncl_a3 ncl_c1 ncl_c2 ncl_c3 soa_a1 soa_a2 soa_c1 soa_c2 pom_a1 pom_c1"

for var in $varlist2
do
   echo $var
   varName=`echo $var|sed 's/_/\\\\_/g'`
   echo $varName
   expression=\'variableName=\"$var\"\'
   cmd="ncl $expression ./budgets.ncl"
   echo $cmd
   eval $cmd
   pdf2ps $var.pdf
   ps2eps -f $var.ps
   epstopdf $var.eps

   #add frame with result for this component
   echo "\\\\begin{frame}{$varName - BUDGET}" > tmp.txt
   echo '\\begin{center}' >> tmp.txt
   echo "\\\\includegraphics[width=\\\\textwidth,height=0.8\\\\textheight]{$var.pdf}" >> tmp.txt 
   echo "\\\\end{center}" >> tmp.txt
   echo "\\\\end{frame}"  >> tmp.txt
   
   cat tmp.txt >> output.tex

done

echo "\\\\end{document}" >> output.tex

pdflatex output.tex
