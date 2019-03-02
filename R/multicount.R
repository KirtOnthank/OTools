#' Counter function
#' 
#' This function will count each time the RETURN button is pressed, and start a new count each time "] followed by ENTER is pressed. Function is ended when "q" followed by ENTER is pressed. This function was programmed to work as a cell counting aid.
#' @return Returns a value or vector counts of how many times the ENTER button was pressed.
#' @export


multicount=function() {
  key=""
  tally=""
  while(key!="q"){
    key=readline("Press ay key exept 'q' and ENTER to count cell: ")
    tally=c(tally,key)
  }
  return(table(tally[1:(length(tally)-1)]))
}
