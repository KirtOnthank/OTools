#' Trim a section out of a file using a graphic interface
#' 
#' Trim an internal segment from a an object as specified by selecting the segment in a graph.
#' @param x Dataframe from read.pyro from Pyro Oxygen Logger files
#' @param column The column numebr by which you wish to select segment to be removed
#' @return Returns the same object, x, with selected rows removed
#' @export
trim=function(x,column){
  print('Select begining and end of segment to remove, then press finish')
  plot(x[,column])
  coords=identify(x[,column],n=2)
  return(x[-(coords[1]:coords[2]),])
}





