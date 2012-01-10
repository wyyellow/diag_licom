load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
    amoc_time_series    =  "/home/hwy/diag_licom/outputs/piControl/nc_data/amoc_time_series.nc" 
    exp_id  =   "exp_id_name"

    wks=gsn_open_wks("pdf","amoc_time_series")
    file1 = addfile(amoc_time_series,"r")
    time  = file1->time
    amoc  = file1->amoc

    res = True
    
    res@vpHeightF       =   0.4
    res@vpWidthF        =   0.65
    res@trXMinF = 540
    res@trXMaxF = 542
    res@tiMainString = "AMOC changes"
    res@tiMainPosition = "Center"
    res@tiMainFontHeightF = 0.02
    res@tiYAxisString = "Sv"
    res@tiXAxisString = "Year"
    res@xyMonoDashPattern = False
    res@xyDashPatterns     = (/0/)
    res@xyLineThicknesses = (/2.0/)
    res@xyLineColors      = (/"black"/)

    res@gsnRightString          =   exp_id
    plot = gsn_csm_xy(wks,time,amoc,res)
end
