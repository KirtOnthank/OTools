#' Calculate respiration rates from open respirometry
#' 
#' Will calculate respiration rates for flow-through respirometry from the output file from the PyroScience FireStingO2 or Presens oxygen logger software read into R by the command read.pyro or read.presens.
#' @param x Dataframe from read.pyro from Pyro Oxygen Logger files
#' @param inflow The channel to which the inflow to the respirometer was assigned (1-4)
#' @param outflow The channel to which the inflow to the respirometer was assigned (1-4)
#' @param flow.rate Flow rates through the respirometer measured in milliliters per minute
#' @param weight Weight of the organism place into the respirometer measured in grams.
#' @param back Background respiration of the respirometer as read out by the function back.pyro
#' @return Returns a data frame of four columns: times, oxy, po2 and resp. The column "times" is time in seconds. The column "oxy" is the oxygen concentration in the respirometer outflow in umolO2/L. The column "po2" is oxygen partial pressure in the respirometer outflow in kPa. The column "resp" is respiration rate in umolO2/g/hr.
#' @export
resp.open=function(x,inflow=1,outflow=2,flow.rate,weight,back=0) {
  resp=(((x[,3+inflow]-x[,3+outflow])*(flow.rate/1000)*60)-back)/weight
  if(ncol(x)>11){
    kPa=umolO2L(x[,3+outflow],temp=x[,7+outflow],atmP=x$pressure)$kPa
  } else {
    kPa=umolO2L(x[,3+outflow],temp=x[,7+outflow])$kPa
  }
  result=data.frame(cbind(x$times,x[,3+outflow],kPa,resp))
  colnames(result)=c("times","oxy","po2","resp")
  return(result)
}

