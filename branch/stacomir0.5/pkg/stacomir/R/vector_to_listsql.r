#' Transforms a vector into a string called within an sql command  e.g. c(A,B,C) => in ('A','B','C')
#' @param vect 
#' @returnType character
#' @return listsql a list of value
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
vector_to_listsql<-function(vect)
{

  # vect = un vecteur caractere
  if (length(vect)==1) 
  {
    listsql=paste("(","'",vect,"'",")",sep="")
  }
  
  if (length(vect)>2)
  {
    listsql=paste("(","'",vect[1],"'",",", sep="")
    for(j in 2:(length(vect)-1)){
      listsql=paste(listsql,"'",vect[j],"'",",",sep="")
    }
    listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="")
  } 
  else if  (length(vect)==2)
  {
   listsql=paste("(","'",vect[1],"'",",", sep="")
   listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="") 
  }

  return(listsql)
}     