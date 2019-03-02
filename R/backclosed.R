#' Calculate background respiration rates from Pyro Oxygen Logger files of closed respirometery
#' 
#' Will calculate background respiration rates for closed respirometry from the output file from the Pyro oxygen logger software read into R by the command read.pyro. This function assumes oxygen concentration was measured as umol/L.
#' @param x Dataframe from read.pyro from Pyro Oxygen Logger files
#' @param volume Volume in liters of the respirometer used.
#' @param channel The channel from which oxygen concentration data should be used.
#' @return Returns background respiration rates in umolO2/min.
#' @export
back.closed=function(x, volume, channel=1){
  print('Click on graph to choose begining and end of data to use, then click "finish"')
  plot(x[,3+channel])
  coords1=identify(x[,3+channel])
  work=x[coords1[1]:coords1[2],]
  clock=work$times[2:length(work[,1])]-work$times[1:(length(work[,1])-1)]
  oxy=work[1:(length(work[,1])-1),channel+3]-work[2:length(work[,1]),channel+3]
  oxy.mean=mean(oxy)
  elap=as.numeric(difftime(work$time[3],work$time[2],units="secs"))
  oxy.mean.norm=oxy.mean*(60/elap)
  return(oxy.mean.norm)
}
