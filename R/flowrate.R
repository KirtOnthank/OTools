#' Function to aid in the timing and calculation of gas flow rate from a volumetric pipettor flow rate apparatus
#' 
#' This function takes timed inputs (when user presses Enter) and corresponding volume increments and calculates instantaneous and cumulative flow rates based on the use of a volumetric pipette gas flow apparatus. Function is stopped when "q" is entered into the prompt.
#' @param increment increments (in mL) at which flow rate is to be timed
#' @return Returns a dataframe containing instantaneous flow rates and cumulative flow rates
#' @export


flowrate=function(increment=1) {
  output=data.frame(0)
  colnames(output)="volume"
  readline("Press Enter:")
  output$time=Sys.time()
  output$elapsed=0
  output$inst_flow=0
  output$tot_flow=0
  
  what="not q"
  while(what!="q"){
    what=readline("Press Enter:")
    line=data.frame(0)
    colnames(line)="volume"
    iter=length(output[,1])+1
    line$volume=(iter-1)*increment
    line$time=Sys.time()
    line$elapsed=as.numeric(difftime(line$time,output$time[iter-1],units="min"))
    line$inst_flow=increment/line$elapsed
    line$tot_flow=line$volume/as.numeric(difftime(line$time,output$time[1],units="min"))
    output[iter,]=line
  }
  return(output[-length(output[,1]),])
}
