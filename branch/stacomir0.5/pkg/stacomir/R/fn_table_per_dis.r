#' functions called in DF and DC
#' @param typeperiode 
#' @param tempsdebut 
#' @param tempsfin 
#' @param libelle 
#' @param date 
#' @returnType list
#' @return listeg
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
# typeperiode=fonctionnementDF$per_tar_code
#tempsdebut= fonctionnementDF$per_date_debut
#tempsfin=fonctionnementDF$per_date_fin
#libelle=fonctionnementDF$libelle
fn_table_per_dis <-function(typeperiode, tempsdebut, tempsfin,libelle,date=TRUE){
listeg=list()
for (j in 1:5){
        if (!date){
        # pour utilisation ulterieure de la classe Posixct
            if (sum(unique(typeperiode)==j)>0){
                choix_periode <-typeperiode==j
                liste<-list(    "debut"=tempsdebut[choix_periode],
                                "fin"=tempsfin[choix_periode],
                                "nom"=libelle[choix_periode][1])
                listeg[[as.character(j)]]<-liste
                }
         # pour utilisation ulterieure de la classe date       
        }  else {
                if (sum(unique(typeperiode)==j)>0){
                choix_periode <-typeperiode==j
                liste<-list(    "debut"=as.Date(tempsdebut[choix_periode]),
                                "fin"=as.Date(tempsfin[choix_periode]),
                                "nom"=as.character(libelle[choix_periode][1]))
                listeg[[as.character(j)]]<-liste
                }
        }
}
return(listeg)}