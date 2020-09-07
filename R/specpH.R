#' Calculates pH of seawater from spectrophotometric data
#' 
#' This function will calculate the pH of seawater from the absorbance values of seawater with m-cresol purple dye. Method is based on Dickson, 2007.
#' @param salinity Salinity of water in ppt or psu. Default is 32.  Can be a vector
#' @param temp Temperature of water in celcius. Default is 11.  Can be a vector
#' @param A434 Absorbance value of seawater + dye at 434 nm. Can be a vector.
#' @param A578 Absorbance value of seawater + dye at 578 nm. Can be a vector.
#' @param A730 Absorbance value of seawater + dye at 730 nm. Can be a vector.
#' @param B434 Absorbance value of blank seawater at 434 nm. Can be a vector.
#' @param B578 Absorbance value of blank seawater at 578 nm. Can be a vector.
#' @param B730 Absorbance value of blank seawater at 730 nm. Can be a vector.
#' @param adj A vector of length two containing the y-intercept and coefficient of linear regression fitting the observed values to known values.
#' @return Returns a value or vector containing the calculated pH values.
#' @export

specpH=function(salinity=32,temp=11,A434,A578,A730,B434,B578,B730,adj=c(0,1)) {
  K=temp+273.15
  pK2=(1245.69/K)+3.8275+0.00211*(35-salinity)
  A1A2=(A578-B578-(A730-B730))/(A434-B434-(A730-B730))
  corr=adj[2]*A1A2+adj[1]
  pH=pK2+log10((corr-0.00691)/(2.2220-corr*0.1331))
  return(pH)
}
