#' This function is programmed to be called in fungraph and fungraphcivelle
#' 
#' This function is programmed to be called in fungraph and fungraphcivelle
#' 
#' 
#' @param per_dis_identifiant period of function of either fishway or counting
#' device
#' @param dateDebut starting date
#' @param dateFin ending date
#' @return sql query
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fn_sql_dis<-function    (per_dis_identifiant,
                        dateDebut=as.Date(duree[min(index)]),
                        dateFin=as.Date(duree[max(index)]))
        {
                        
        sql=paste("SELECT",
        " per_dis_identifiant,",
        " per_date_debut,",
        " per_date_fin,",
        " per_commentaires,",
        " per_etat_fonctionnement,",
        " per_tar_code,",
        " tar_libelle AS libelle",
        " FROM  ",get("sch",envir=envir_stacomi),"t_periodefonctdispositif_per per",
        " INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",
        " WHERE   per_date_fin >='",
        dateDebut,
        "' AND per_date_debut <='" ,
        dateFin,
        "' AND per_dis_identifiant=",
        per_dis_identifiant,
        " ORDER BY per_date_debut;",sep = "")
        return(sql)
        
        }
