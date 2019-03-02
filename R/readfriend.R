#' Read Respirometer's Friend Files
#' 
#' Will read the output file from the Respirometer's Friend software into R as a dataframe.
#' @param file File to be read
#' @return All data columns of the file will be read in, but header information will be discarded
#' @export

read.friend=function(file) {
  flow=read.csv(file=file,header=F,fill=T,stringsAsFactors=F)
  flow.date=as.character(flow[1,1])
  flow.time=as.character(flow[2,1])
  flow=flow[5:length(flow[,1]),] 
  colnames(flow)=c("time","count1","count2","ph1","ph2","CO2flow","CO2set","O2set")[1:length(flow[1,])]
  
  for(i in 1:length(flow[1,])) {
    flow[,i]=as.numeric(flow[,i])
  }

  flow$ctime=strptime(paste(flow.date,flow.time),format="%m/%d/%Y %I:%M %p")+flow$time

  elapse=c(0,flow$time[2:length(flow[,1])]-flow$time[1:(length(flow[,1])-1)])
  rate=flow$count1
  rate[2:length(flow[,1])]=flow$count1[2:length(flow[,1])]-flow$count1[1:(length(flow[,1])-1)]
  flow$rate1=rate*(0.25/1000)*(60/elapse)

  rate=flow$count2
  rate[2:length(flow[,1])]=flow$count2[2:length(flow[,1])]-flow$count2[1:(length(flow[,1])-1)]
  flow$rate2=rate*(0.25/1000)*(60/elapse)

  flow=data.frame(flow[2:length(flow[,1]),])
  
  return(flow)
}
