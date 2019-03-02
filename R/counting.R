#' Counter function
#' 
#' This function will count each time the RETURN button is pressed, and start a new count each time "] followed by ENTER is pressed. Function is ended when "q" followed by ENTER is pressed. This function was programmed to work as a cell counting aid.
#' @return Returns a value or vector counts of how many times the ENTER button was pressed.
#' @export


counting=function() {
  counts=0
  index=1
  key=0
  while(key!="q"){
    key=""
    while(key!="]"&key!="q"){
      key=readline("Press ENTER to count cell, Press ']' to start new count")
      counts[index]=counts[index]+1
    }
  index=index+1
  counts=c(counts,0)
  }
return(counts[1:(length(counts)-1)])
}
