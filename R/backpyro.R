#' Background respiration from Pyro Oxygen Logger Files
#' 
#' Will read the background respiration from a specific background run in a Pyro Oxygen Logger data file of a flow-through respirometery system. This function assumes that channels 1 and 2 are the input and output of the first respirometer, respectively, and that channels 3 & 4 are the input and output of the second respirometer, respectively.
#' @param x Dataframe from read.pyro from Pyro Oxygen Logger files
#' @param in1 Identifies the channel which monitors inflow of respirometer 1
#' @param in2 Identifies the channel which monitors inflow of respirometer 2
#' @param out1 Identifies the channel which monitors outflow of respirometer 1
#' @param out2 Identifies the channel which monitors outflow of respirometer 2
#' @param flow1 Flow rates through the first respirometer measured in liters per minute
#' @param flow2 Flow rates through the second respirometer measured in liters per minute
#' @return A vector of two values corresponding to the background respiration rate in first and second respirometers.
#' @export
back.pyro=function(x,in1=1,in2=3,out1=2,out2=4,flow1,flow2){
  print('Click on graph for first respirometer to choose begining and end of data to use, then click "finish"')
  plot(x[,3+in1]-x[,3+out1])
  coords1=identify(x[,3+in1]-x[,3+out1])
  print('Now, click on graph for second respirometer to choose begining and end of data to use, then click "finish"')
  plot(x[,3+in2]-x[,3+out2])
  coords2=identify(x[,3+in2]-x[,3+out2])
  flow1=if(length(flow1)==1) flow1 else flow1[coords1[1]:coords1[2]]
  flow2=if(length(flow2)==1) flow2 else flow2[coords2[1]:coords2[2]]
  resp1.in=x[coords1[1]:coords1[2],3+in1]
  resp1.out=x[coords1[1]:coords1[2],3+out1]
  resp2.in=x[coords2[1]:coords2[2],3+in2]
  resp2.out=x[coords2[1]:coords2[2],3+out2]
  back1=mean((resp1.in-resp1.out)*flow1*60,na.rm=T)
  back2=mean((resp2.in-resp2.out)*flow2*60,na.rm=T)
  return(c(back1,back2))
}


