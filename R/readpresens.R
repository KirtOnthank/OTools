#' Read Presens Oxygen Logger Files
#' 
#' Will read the output file from the Presens oxygen logger software into R as a dataframe.
#' @param file File to be read
#' @param convert.to.umol Logical. Use to disable the default behavior of converting percent oxygen saturation (what Presens records by default) to micromoles of oxygen per liter.
#' @param atmP Atmospheric pressure at which measurements were taken. 
#' @return All data columns of the file will be read in, but header information will be discarded
#' @export

read.presens=function(file,convert.to.umol=T,atmP=1013.25){
  files=list.files(dirname(file),full.names = T)
  file.base=gsub("(.*)ch\\d.txt","\\1",file)
  to.read=sort(grep(file.base,files,value=T,ignore.case=T))
  
  to.skip=grep("\\d+\\/\\d+\\/\\d\\d\\d\\d\\;",readLines(to.read[1]))[1]-1
  
  presens=read.table(file=to.read[1],sep=";",header=F,fill=T,stringsAsFactors=F,skip=to.skip)
  colnames(presens)=c("date","time","times","O21","phase","amp","temp1","error")
  presens$times=presens$times*60
  presens$time=strptime(paste(presens$date,presens$time),format="%m/%d/%Y %H:%M:%S")
  if (convert.to.umol){
    presens$O21=O2sat(presens$O21,temp=presens$temp1,atmP=atmP)$umolO2.per.L
  }

  
  presens=cbind(presens[,1:4],NA,NA,NA,presens[,7],NA,NA,NA)
  colnames(presens)=c("date","time","times","O21","O22","O23","O24","temp1","temp2","temp3","temp4")
  
  
  
  if (length(to.read)>1){
    for (j in 2:length(to.read)){
      to.skip2=grep("\\d+\\/\\d+\\/\\d\\d\\d\\d\\;",readLines(to.read[j]))[1]-1
      presens2=read.table(file=to.read[j],sep=";",header=F,fill=T,stringsAsFactors=F,skip=to.skip2)
      colnames(presens2)=c("date","time","times","O2","phase","amp","temp","error")
      presens2$times=presens2$times*60
      presens2$time=strptime(paste(presens2$date,presens2$time),format="%m/%d/%Y %H:%M:%S")
      if (convert.to.umol){
        presens2$O2=O2sat(presens2$O2,temp=presens2$temp,atmP=atmP)$umolO2.per.L
      }
      
      reconcile=numeric()
      
      if (nrow(presens)!=nrow(presens2)){
        for (i in 1:nrow(presens)){
          reconcile[i]=which.min(abs(presens2$time-presens$time[i]))
        }
        presens[,j+3]=presens2$O2[reconcile]
        presens[j+7]=presens2$temp[reconcile]
      } else {
        presens[j+3]=presens2$O2
        presens[j+7]=presens2$temp
      }
    }
  }
  
  # This is alternate reconcilitation code, but in testing found to be about 2x longer
  # to run than the loop. (1min vs 30s for ~4500 line dataset)
  #apply(abs(outer(presens$time,presens2$time,"difftime")),1,FUN="which.min") 
  
  return(presens)
  
}

