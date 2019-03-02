#' Read Pyro Oxygen Logger Files
#' 
#' Will read the output file from the Pyro oxygen logger software into R as a dataframe.
#' @param file File to be read
#' @return All data columns of the file will be read in, but header information will be discarded
#' @export
read.pyro=function(file) {
  pyro=read.table(file=file,header=F,fill=T,stringsAsFactors=F)
  pyro=pyro[27:length(pyro[,1]),1:28]
  colnames(pyro)=c("date","time","times","O21","O22","O23","O24","temp1","temp2","temp3","temp4","pressure","humidity","temp_probe","internal_temp","analog_in","dphi1","dphi2","dphi3","dphi4","signal1","signal2","signal3","signal4","ambient1","ambient2","ambient3","ambient4")
  for(i in 3:28) {
    pyro[,i]=as.numeric(pyro[,i])
  }
  pyro$time=strptime(paste(pyro$date,pyro$time),format="%m/%d/%Y %H:%M:%S")
  return(pyro)
}
