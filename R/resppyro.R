#' Calculate respiration rates from Pyro Oxygen Logger files
#' 
#' Will calculate respiration rates for flow-through respirometry from the output file from the Pyro oxygen logger software read into R by the command read.pyro.  This function assumes that channels 1 and 2 are the input and output of the first respirometer, respectively, and that channels 3 & 4 are the input and output of the second respirometer, respectively.
#' @param x Dataframe from read.pyro from Pyro Oxygen Logger files
#' @param flow Flow rates through the first (flow1) and second (flow2) respirometers measured in liters per minute
#' @param weight Weight of the organism place into the first (weight1) and second (weight2) respirometers measured in grams.
#' @param back Background respiration of the first (back1) and second (back2) respirometers as read out by the function back.pyro
#' @param start The position of the file, in hours, to begin calculating respiration.
#' @param end The position of the file, in hours, to end calculating respiration.
#' @return Returns a data frame of four columns: oxy1 & oxy 2, the oxygen concentrations in the outflows of respirometers 1 and 2 respectively, and resp1 & resp2, respiration rates in the first and second respirometers, respectively.
#' @export
resp.pyro=function(x,flow1,flow2,weight1,weight2,back1=0,back2=0,start=0,end=max(x$times/3600)) {
  print('Click on graph to choose begining and end of data to use')
  plot(x[,4]-x[,5],type="l")
  coords=identify(x[,4]-x[,5],n=2)
  work=x[coords[1]:coords[2],]
  flow1=if(length(flow1)==1) flow1 else flow1[coords[1]:coords[2]]
  flow2=if(length(flow2)==1) flow2 else flow2[coords[1]:coords[2]]
  resp1=(((work$O21-work$O22)*flow1*60)-back1)/weight1
  resp2=(((work$O23-work$O24)*flow2*60)-back2)/weight2
  result=data.frame(cbind(work$O22,work$O24,resp1,resp2))
  colnames(result)=c("oxy1","oxy2","resp1","resp2")
  return(result)
}

