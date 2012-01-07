# Nom fichier :        funtable.R

#' function to print and save statistics in .csv and .html formats
#' @param tableau 
#' @param duree 
#' @param taxon 
#' @param stade 
#' @param DC 
#' @param resum 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
funtable=function(tableau,duree,taxon,stade,DC,resum){
	annee=unique(strftime(as.POSIXlt(duree),"%Y"))
 path1=file.path(path.expand(datawd),paste(DC,"_",taxon,"_",stade,"_",annee,".csv",sep=""),fsep ="\\")
	write.table(tableau,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	funout(paste(get("msg",envir=envir_stacomi)$funtable.1,path1,"\n"))
	path1html=file.path(path.expand(datawd),paste(DC,"_",taxon,"_",stade,"_",annee,".html",sep=""),fsep ="\\")
   funhtml(tableau,
            caption=paste(DC,"_",taxon,"_",stade,"_",annee,".csv",sep=""),
            top=TRUE,
            outfile=path1html,
            clipboard=FALSE,
            append=FALSE,
            digits=2
            )
	funout(paste("ecriture de",path1html,"\n"))
	if( !is.null(resum) )
	{
    path2=file.path(path.expand(datawd),paste("res",annee,".csv",sep=""),fsep ="\\")
  	write.table(resum,path2,row.names=TRUE,col.names=TRUE,sep=";",append=TRUE)
  	path2html=file.path(path.expand(datawd),paste("res",annee,".html",sep=""),fsep ="\\")
  	funout(paste("ecriture de",path2,"\n"))
  	 funhtml(resum,
            caption=paste("Sommes mensuelles",annee),
            top=TRUE,
            outfile=path2html,
            clipboard=FALSE,
            append=TRUE,
            digits=2
            )
    funout(paste(get("msg",envir=envir_stacomi)$funtable.1,path2html,"\n"))       
  	rm(path1,path1html,path2,path2html)
	}
#	setwd(wd)
}


