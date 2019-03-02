#' Convert oxygen concentration data in umolO2/L into various other units
#' 
#' This function will convert between oxygen concentration in water expressed in umolO2/L to other units of concentration and pressure.
#' @param value Measured oxygen concentration (or vector of concentrations) of water in umolO2/L
#' @param salinity Salinity of water in ppt or psu. Default is 32.  Can be a vector
#' @param temp Temperature of water in celcius. Default is 11.  Can be a vector
#' @param atmP Atmospheric pressure in mbar or hPa. Default is 1013.25. Can be a vector.
#' @return Returns a dataframe with one row per measured oxygen concentration value entered. Columns report salinity, temperature and atmospheric pressure used, and then retuns oxygen concentration/pressure in Saturation (%), kilopascals (kPa), hectopascals or millibar (hPa.mbar), torr, umolO2/liter, and mgO2/L.
#' @export

umolO2L=function(value,salinity=32,temp=11,atmP=1013.25) {
  A1=-173.4292
  A2=249.6339
  A3=143.3483
  A4=-21.8492
  B1=-0.033096
  B2=0.014259
  B3=-0.0017
  t=temp+273.15
  MWO2=32
  MolVol=22.414
  dens=1.429/(MWO2/1000)
  
  mL.per.L=exp(A1+A2*(100/t)+A3*log(t/100)+A4*(t/100)+salinity*(B1+B2*(t/100)+B3*(t/100)*(t/100)))
  fullsat=mL.per.L*dens
  O2sat=(value/fullsat)*100
  hPa.mbar=(O2sat/100)*0.2095*atmP
  kPa=hPa.mbar/10
  torr=hPa.mbar*0.75
  mgO2.per.L=value*(MWO2/1000)
  umolO2.per.L=value
  output<-data.frame(salinity, temp, atmP, O2sat, kPa, hPa.mbar,torr,umolO2.per.L, mgO2.per.L)
  return(output)
}


