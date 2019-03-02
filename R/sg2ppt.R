#' Convert salinity readings from a hydrometer in specific gravity to parts per thousand
#' 
#' Will convert the salinity readings of a hydrometer in specific gravity to parts per thousand. Calibration values for the hydrometer being used can be entered using the cal.stand, cal.read, & cal.temp arguments.
#' @param read Reading of the hydrometer. Can be a vector or single value.
#' @param temp Temperature of the water being measured in celcius. Can be a vector or single value.
#' @param cal.stand Vector of salinities, in ppt, of calibration solutions used.
#' @param cal.read Vector of hydrometer readings of calibration solutions.
#' @param cal.temp Vector or single value of the temperature of calibration solutions in celcius.
#' @return A vector or single value of salinities in ppt calculated from the hydrometer readings in specific gravity.
#' @export
#' @importFrom marelac sw_dens

sg2ppt=function(read,temp,cal.stand=c(25,30,35),cal.read=c(1.0185,1.0220,1.0256),cal.temp=25){
  density=sw_dens(S=cal.stand,t=cal.temp,method="Gibbs")
  cal.lm=lm(density~cal.read)
  dens.lm=lm(c(0:40)~sw_dens(S=c(0:40), t=temp, method="Gibbs"))
  dens=read*cal.lm$coefficients[2]+cal.lm$coefficients[1]
  result=round(dens*dens.lm$coefficients[2]+dens.lm$coefficients[1],1)
  return(as.numeric(result))
}