load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
    acc_time_series    =  "acc_time_series_dir" 
    exp_id  =   "exp_id_name"

    wks=gsn_open_wks("pdf","acc_time_series")
    file1 = addfile(acc_time_series,"r")
    time  = file1->time
    acc  = file1->acc

    res = True
    
    res@vpHeightF       =   0.4
    res@vpWidthF        =   0.65
    res@trXMinF = fig_start_year
    res@trXMaxF = fig_end_year
    res@tiMainString = "ACC changes"
    res@tiMainPosition = "Center"
    res@tiMainFontHeightF = 0.02
    res@tiYAxisString = "Sv"
    res@tiXAxisString = "Year"
    res@xyMonoDashPattern = False
    res@xyDashPatterns     = (/0/)
    res@xyLineThicknesses = (/2.0/)
    res@xyLineColors      = (/"black"/)

    res@gsnRightString          =   exp_id
    plot = gsn_csm_xy(wks,time,acc,res)
end
