#' Add flow rates to a respirometry data file
#' 
#' Will add flow rates to dataset from the PyroScience FirestingO2 using output from Respirometers Friend
#' @param resp Dataset from PyroScience FirestingO2 read in using read.pyro
#' @param flow Dataset from Respirometer's Friend read in using read.friend
#' @return Returns dataset from respirometer with two columns added of flow rates
#' @export

addflow=function(resp,flow) {
  common.time=difftime(flow[,length(flow[1,])-2],resp[1,2],units="secs")

  flow1=rep(0,length(resp[,1]))
  flow2=rep(0,length(resp[,1]))
  
  for (i in 2:length(resp[,1])) {
    flow1[i]=mean(flow$rate1[common.time>=resp$times[i-1]&common.time<=resp$times[i]])
  }
  
  
  for (i in 2:length(resp[,1])) {
    flow2[i]=mean(flow$rate2[common.time>=resp$times[i-1]&common.time<=resp$times[i]])
  }
  
  resp=cbind(resp,flow1,flow2)  

  return(resp)
}