load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
    amoc_2d_file    =   "amoc_2d_file_dir"
    file_amoc       =   addfile(amoc_2d_file,"r")
    
    type    =       "pdf"
    wks     =       gsn_open_wks(type,"amoc_2d_average")
    gsn_define_colormap(wks,"BlWhRe")

    res     =       True
    res@cnFillOn        =    True
    res@cnLineLabelsOn   =   True
    res@cnLabelMasking             = True
    res@cnLineLabelBackgroundColor = "transparent"
    res@cnLineLabelPlacementMode = "constant"
    res@gsnSpreadColors         = True
    res@lbLabelStride           = 2
    res@cnLineLabelInterval     = 1

    res@gsnLeftString           = ""
    res@gsnRightString          = ""
    res@gsnCenterString         = ""
    res@tiMainString            = "AMOC"
    res@tiMainPosition          = "Center"
    res@tiMainFontHeightF       = 0.025
    res@tiYAxisString           = "Depth (m)"
    res@trYReverse              =  False
    res@gsnYAxisIrregular2Linear = True
    res@gsnXAxisIrregular2Linear = True
    res@cnMissingValPerimOn     = True
    res@cnMissingValFillPattern = 3
    res@cnMissingValFillColor   = "black"

    res@vpHeightF       =   0.4
    res@vpWidthF        =   0.6

    res@cnLevelSelectionMode    =   "ManualLevels"
    res@cnMinLevelValF          =   -35.0
    res@cnMaxLevelValF          =    35.0
    res@cnLevelSpacingF         =     5.0
    res@gsnStringFontHeightF = 0.020
    res@gsnLeftString = "units: Sv"
    res@gsnRightString  =   "min: "+sprintf("%4.2f", min(file_amoc->amoc))+\
                             " " + \
                            "max: "+sprintf("%4.2f", max(file_amoc->amoc))
    plot = gsn_csm_contour(wks,file_amoc->amoc(:,3:150),res)
    


end
