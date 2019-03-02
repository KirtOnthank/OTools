#' Read Calibration Data From Pyro Oxygen Logger Files
#' 
#' Will read calibration data from the output file from the Pyro oxygen logger software into R as a linear model.
#' @param file File to be read
#' @param channel Channel for which calibration is to be read
#' @return Returns an object of class "lm" based on the calibration data in file for the specified channel.
#' @export
read.cal=function(file,channel) {
  temp=read.table(file=file,header=F,fill=T,stringsAsFactors=F)
  dphi0=as.numeric(temp[channel+11,max(c(which(temp[channel+11,]=="Air"),which(temp[channel+11,]=="Water")))+6])
  dphi100=as.numeric(temp[channel+11,max(c(which(temp[channel+11,]=="Air"),which(temp[channel+11,]=="Water")))+7])
  dphi=c(dphi0,dphi100)
  sat=c(0,100)
  temp.lm=lm(sat~dphi)
  return(temp.lm)
}


