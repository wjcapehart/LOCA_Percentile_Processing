
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"


begin



  time_hist = ispan(18262 , 38715, 1)
  time_hist@standard_name = "time" ;
  time_hist@long_name = "time" ;
  time_hist@description = "time"
  time_hist@units = "days since 1900-01-01 00:00:00" ;
  time_hist@calendar = "standard" ;
  time_hist!0 = "time_hist"
  time_hist&time_hist = time_hist


  time_futr = ispan(38716 , 73048, 1)
  time_futr@standard_name = "time" ;
  time_futr@long_name = "time" ;
  time_futr@units = "days since 1900-01-01 00:00:00" ;
  time_futr@calendar = "standard"
  time_futr!0 = "time_futr"
  time_futr&time_futr = time_futr

  calendar_date_hist = stringtochar( cd_string(time_hist, "%Y/%N/%D 12:%M:%S"))
  calendar_date_hist@long_name = "Calendar Date for Historical Period" ;
  calendar_date_hist@description = "Calendar Date for Historical Period"
  calendar_date_hist@calendar = "standard" ;
  calendar_date_hist!0 = "time_hist"
  calendar_date_hist&time_hist = time_hist
  calendar_date_hist!1 = "calendar_string_len"



  calendar_date_futr= stringtochar(cd_string(time_futr, "%Y/%N/%D 12:%M:%S"))
  calendar_date_futr@long_name = "Calendar Date for Future Period" ;
  calendar_date_futr@description = "Calendar Date for Future Period"
  calendar_date_futr@calendar = "standard" ;
  calendar_date_futr!0 = "time_futr"
  calendar_date_futr&time_futr = time_futr
  calendar_date_futr!1 = "calendar_string_len"

  setfileoption("nc", "Format",        "NetCDF4")
  setfileoption("nc", "FileStructure", "Advanced")

  system("rm -frv ./LOCA_Calendar_Lookup_Table.nc")
  f = addfile("./LOCA_Calendar_Lookup_Table.nc", "c")

  f->time_hist = time_hist
  f->time_futr = time_futr

  f->calendar_date_hist = calendar_date_hist
  f->calendar_date_futr = calendar_date_futr


  delete(f)





end
