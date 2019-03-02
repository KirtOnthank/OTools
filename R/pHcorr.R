#' Determines dye addition correction for use in spectrophotometric pH determination.
#' 
#' This function will calculate dye addition correction values to spectrophotometrically determine pH using the m-cresol purple method found in SOP 6b (Dickson, 2007).  This function take the raw absorbance values from at least two solutions of known pH (seawater pH buffer or seawater of known pCO2 and Alkalinity).
#' @param known_pH Known pH values of solutions. Can be a vector
#' @param salinity Salinity of water in ppt or psu. Default is 32.  Can be a vector
#' @param temp Temperature of water in celcius. Default is 11.  Can be a vector
#' @param A434 Absorbance value of seawater + dye at 434 nm. Can be a vector.
#' @param A578 Absorbance value of seawater + dye at 578 nm. Can be a vector.
#' @param A730 Absorbance value of seawater + dye at 730 nm. Can be a vector.
#' @param B434 Absorbance value of blank seawater at 434 nm. Can be a vector.
#' @param B578 Absorbance value of blank seawater at 578 nm. Can be a vector.
#' @param B730 Absorbance value of blank seawater at 730 nm. Can be a vector.
#' @return Returns a vector of length two containing the y-intercept and coefficient of linear regression fitting the observed values to known values.
#' @export

pHcorr=function(known_pH,salinity=32,temp=11,A434,A578,A730,B434,B578,B730) {
  K=temp+273.15
  pK2=(1245.69/K)+3.8275+0.00211*(35-salinity)
  calculated=(A578-B578-(A730-B730))/(A434-B434-(A730-B730))
  actual=(2.2220*10^(known_pH-pK2)+0.00691)/(1+0.1331*10^(known_pH-pK2))
  output=as.numeric(lm(actual~calculated)$coefficients)
  return(output)
}

