#' Calculate respiration rates from Pyro Oxygen Logger files of closed respirometery
#' 
#' Will calculate respiration rates for closed respirometry from the output file from the Pyro oxygen logger software read into R by the command read.pyro. This function assumes oxygen concentration was measured as umol/L.
#' @param x Dataframe from read.pyro from Pyro Oxygen Logger files
#' @param volume Volume in liters of the respirometer used.
#' @param weight Weight of the organism place into the respirometer measured in grams.
#' @param channel The channel from which oxygen concentration data should be used.
#' @param back The background respiration rate of the respirometer as determined from back.closed()
#' @param smooth The number of points over which to smooth the data.  Data points for smooth will be equally distributed before and after the point to be smoothed, and points will be weighted by a guassian distrbution with distance from point to be smoothed.
#' @return Returns a data frame of four columns: times, oxy, po2 and resp. The column "times" is time in seconds. The column "oxy" is the oxygen concentration in the respirometer outflow in umolO2/L. The column "po2" is oxygen partial pressure in the respirometer outflow in kPa. The column "resp" is respiration rate in umolO2/g/hr.
#' @export
resp.closed=function(x, volume, weight, channel=1, back=0, smooth=0){
  coeff=dnorm(seq(from=-3,to=3,length.out=smooth+1))/sum(dnorm(seq(from=-3,to=3,length.out=smooth+1)))
  work=x[complete.cases(x[,3+channel]),]
  work[,3+channel]=filter(work[,3+channel],coeff,sides=2)
  elap=as.numeric(difftime(work$time[4],work$time[3],units="secs"))
  clock=work$times[2:nrow(work)]-work$times[1:(nrow(work)-1)]
  back1=back*(elap/60)
  oxy=work[1:(nrow(work)-1),channel+3]-work[2:nrow(work),channel+3]-back1
  resp=((oxy*(3600/elap))*volume)/weight
  time=work$times[2:nrow(work)]
  conc=work[2:nrow(work),channel+3]
  if(ncol(x)>11){
    kPa=umolO2L(conc,temp=work[2:nrow(work),channel+7],atmP=work$pressure[2:nrow(work)])$kPa
  } else {
    kPa=umolO2L(conc,temp=work[2:nrow(work),channel+7])$kPa
  }
  final=data.frame(cbind(time,conc,kPa,resp))
  colnames(final)=c("times","oxy","po2","resp")
  return(final)
}




