#' functions called in DF and DC
#' 
#' 
#' 
#' @param typeperiode ref.tr_typearretdisp_tar(per_tar_code) the code of the
#' period (see table ref.tr_typearretdisp_tar)
#' @param tempsdebut ref.tr_typearretdisp_tar(per_date_debut) starting
#' timestamp of the period
#' @param tempsfin The posgres column ref.tr_typearretdisp_tar(per_date_fin) ending timestamp of
#' the period
#' @param Libelle The posgres column ref.tr_typearretdisp_tar(libelle )description of the period
#' type
#' @param date Boolean, should the function return a POSIXt or date value
#' @return listeg A list
#' @note returns either POSIXt or date if date=TRUE
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fn_table_per_dis <-function(typeperiode, tempsdebut, tempsfin,libelle,date=TRUE){
listeg=list()
for (j in 1:5){
        if (!date){
        # pour utilisation ulterieure de la classe Posixct
            if (sum(unique(typeperiode)==j)>0){
                choice_periode <-typeperiode==j
                liste<-list(    "debut"=tempsdebut[choice_periode],
                                "fin"=tempsfin[choice_periode],
                                "nom"=libelle[choice_periode][1])
                listeg[[as.character(j)]]<-liste
                }
         # pour utilisation ulterieure de la classe date       
        }  else {
                if (sum(unique(typeperiode)==j)>0){
                choice_periode <-typeperiode==j
                liste<-list(    "debut"=as.Date(tempsdebut[choice_periode]),
                                "fin"=as.Date(tempsfin[choice_periode]),
                                "nom"=as.character(libelle[choice_periode][1]))
                listeg[[as.character(j)]]<-liste
                }
        }
}
return(listeg)}
