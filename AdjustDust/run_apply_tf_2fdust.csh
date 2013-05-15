#!/bin/csh -f
#
#*****************************************************************************
#set echo
setenv GRID 12km
setenv SCEN SEMAP_base07b

setenv INDIR1    /nas01/depts/ie/cempd/SEMAP/Emissions/SMOKE/data/run_${SCEN}/outputs/cmaq.cb05p25.$GRID
setenv INDIR2    /nas01/depts/ie/cempd/SEMAP/Emissions/EI/BELD_TFs
setenv OUTDIR    $INDIR1
setenv TRANS_FAC $INDIR2/xportfract.beld3.SEMAP_${GRID}.ncf


foreach SECTOR (fdust)

   setenv YEAR    2006
   setenv STDATE  2006338
   setenv ENDDATE 2006360
   setenv DATE $STDATE

 while ($DATE <= $ENDDATE ) 
         
   setenv FDUST_IN   ${INDIR1}/${SECTOR}gts_s.${DATE}.$GRID.$SCEN.cmaq.cb05p25.ncf

   if (-e $FDUST_IN ) then

     setenv FDUST_OUT  ${OUTDIR}/${SECTOR}gts_s_xportfrac.${DATE}.$GRID.$SCEN.cmaq.cb05p25.ncf

     echo '===>>>> proccing day '$DATE

./apply_tf_2fdust<<eof
TRANS_FAC
FDUST_IN
FDUST_OUT
eof

    gzip -av $FDUST_IN
    mv ${FDUST_IN}.gz $OUTDIR/fdust_no_tf
   endif

    @ DATE = $DATE + 1

 end
end

