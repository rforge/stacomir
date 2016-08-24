# Nom fichier :        Bilanpoidsmoyen.R    (classe)
# Projet :             controle migrateur 
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   07/02/2009 21:30:54
#' Class "Bilan_poids_moyen"
#' 
#' Bilan_poids_moyen class The objective is to calculate mean weight of glass
#' eel which are counted from weight measurements and to reintegrate weight to
#' number coefficients
#' 
#' 
#' @include RefCoe.r
#' @note We have also tools available to import glass eel measurement from
#' experimental fishing in the estuary For the charge method dates for the
#' request are from august to august (a glass eel season)
#' @section Slots: \describe{ \item{list("data")}{Object of class
#' \code{"data.frame"} data for bilan lot }\item{:}{Object of class
#' \code{"data.frame"} data for bilan lot } \item{list("dc")}{Object of class
#' \code{"RefDC"} refDC an instantiation of the counting device
#' class}\item{:}{Object of class \code{"RefDC"} refDC an instantiation of the
#' counting device class} \item{list("anneedebut")}{Object of class
#' \code{"RefAnnee"} refAnnee allows to choose year of beginning
#' }\item{:}{Object of class \code{"RefAnnee"} refAnnee allows to choose year
#' of beginning } \item{list("anneefin")}{Object of class \code{"RefAnnee"}
#' refAnnee allows to choose year of ending }\item{:}{Object of class
#' \code{"RefAnnee"} refAnnee allows to choose year of ending }
#' \item{list("coe")}{Object of class \code{"RefCoe"} class loading coefficient
#' of conversion between quantity (weights or volumes of glass eel) and numbers
#' }\item{:}{Object of class \code{"RefCoe"} class loading coefficient of
#' conversion between quantity (weights or volumes of glass eel) and numbers }
#' \item{list("liste")}{Object of class \code{"RefListe"} RefListe referential
#' class choose within a list, here do you want subsamples or not
#' }\item{:}{Object of class \code{"RefListe"} RefListe referential class
#' choose within a list, here do you want subsamples or not } }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_carlot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} 
#' \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} 
#' \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @concept Bilan Object 
#' @examples
#'  \dontrun{
#' showClass("Bilan_poids_moyen")
#' object=new("Bilan_poids_moyen")
#' }
#' @export 
setClass(Class="Bilan_poids_moyen",        
		representation= representation(data="data.frame",
				dc="RefDC",
				anneedebut="RefAnnee",
				anneefin="RefAnnee",
				coe="RefCoe",
				liste="RefListe"),
		prototype=prototype(data=data.frame(),
				dc=new("RefDC"),
				anneedebut=new("RefAnnee"),
				anneefin=new("RefAnnee"),
				coe=new("RefCoe"),
				liste=new("RefListe")))#

#' connect method for Bilan_Poids_moyen
#' @return Bilan_Poids_Moyen request corresponding to user choices
#' @note dates for the request are from august to august (a glass eel season)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
# object<-bilan_poids_moyen
setMethod("connect",signature=signature("Bilan_poids_moyen"),definition=function(object,h) {
			#CHARGEMENT DU TABLEAU DES POIDS MOYENS
			requete=new("RequeteODBCwheredate")
			requete@baseODBC=get("baseODBC",envir=envir_stacomi)
			requete@datedebut=strptime(paste(object@anneedebut@annee_selectionnee-1,"-08-01",sep=""),format="%Y-%m-%d")
			requete@datefin=strptime(paste(object@anneefin@annee_selectionnee,"-08-01",sep=""),format="%Y-%m-%d")
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@select= paste("SELECT lot_identifiant,ope_date_debut,ope_date_fin,lot_effectif,car_valeur_quantitatif as poids,",
					" (car_valeur_quantitatif/lot_effectif) AS poids_moyen,",
					" (ope_date_fin-ope_date_debut)/2 AS time.sequence,",
					" ope_date_debut+(ope_date_fin-ope_date_debut)/2 as datemoy,",
					" date_part('year', ope_date_debut) as annee,",
					" date_part('month',ope_date_debut) as mois",
					" FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car_qan",sep="")
			requete@and=paste(" AND ope_dic_identifiant=",object@dc@dc_selectionne,
					" AND std_libelle='civelle'",
					ifelse(object@liste@listechoice=="tous", "",paste(" AND  lot_effectif", object@liste@listechoice)),
					" AND upper(car_methode_obtention::text) = 'MESURE'::text",
					" AND car_par_code='A111'",sep="")
			requete<-stacomirtools::connect(requete)			
			object@data<-requete@query			
			#CHARGEMENT DES COEFFICIENTS DE CONVERSION
			object@coe@datedebut=requete@datedebut
			object@coe@datefin=requete@datefin
			object@coe<-charge(object@coe)
			funout(get("msg",envir_stacomi)$Bilan_poids_moyen.1)
			funout(paste(nrow(object@coe@data),get("msg",envir_stacomi)$Bilan_poids_moyen.2))			
			return(object)
		})

# Cette methode permet de verifier que les boites ont ete cliquees et va chercher les
# objects qui sont colles dans l'environnement envir_stacomi de l'interface 
#' charge method for Bilan_poids_moyen class
#' @param h a handler 
#' @return Bilan_poids_moyen with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("Bilan_poids_moyen"),definition=function(object,h) {
			if (exists("refliste",envir_stacomi)) {      
				object@liste<-get("refliste",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.9, arret=TRUE)             
			} 
			if (exists("refDC",envir_stacomi)) {      
				object@dc<-get("refDC",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)          
			}            
			if (exists("refAnneeDebut",envir_stacomi)) {      
				object@anneedebut<-get("refAnneeDebut",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.10,arret=TRUE)             
			}
			if (exists("refAnneeFin",envir_stacomi)) {      
				object@anneefin<-get("refAnneeFin",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.11,arret=TRUE)       
			}                    
			object<-connect(object)
			assign("bilan_poids_moyen",object,envir=envir_stacomi)
			return(object) 
		})









#' fungraphBilan_poids_moyen
#' 
#' Function for bilanPoidsMoyen also allowing to pass some objects from the
#' peche iav database
#' 
#' 
#' @param h handler
#' @param \dots additional arguments passed to the function
fungraphBilan_poids_moyen = function(h,...) {
	if (exists("toolbarlistgraph",.GlobalEnv)) {
		delete(group,toolbarlistgraph) 
		rm("toolbarlistgraph",envir= .GlobalEnv)
	}
	if (exists("toolbarlistgraph1",.GlobalEnv)) 
	{delete(group,toolbarlistgraph1) 
		rm("toolbarlistgraph1",envir= .GlobalEnv)
	}
	if ((!exists("peche", envir=envir_stacomi))){
		# si il existe j'ai traffique l'object bilan poids moyen a la main
		# et je ne souhaite pas qu'il soit ecrase
		bilan_poids_moyen=charge(bilan_poids_moyen)
	} 
	
	donnees=bilan_poids_moyen@data # on recupere le data.frame
	coeff=bilan_poids_moyen@coe@data
	coeff$poids_moyen=1/coeff$coe_valeur_coefficient
	coeff$date=as.POSIXct(coeff$coe_date_debut)
	funout(get("msg",envir_stacomi)$Bilan_poids_moyen.3)
	# changement des noms
	donnees=stacomirtools::chnames(donnees,c("lot_identifiant","ope_date_debut","ope_date_fin","lot_effectif","poids","poids_moyen","time.sequence","datemoy"),
			c("lot","date","date_fin","effectif","poids","poids_moyen","time.sequence","date"))
	# correction de manques d'effectifs dans la base
	if (sum(is.na(donnees$effectif))>0) warnings(paste(get("msg",envir_stacomi)$Bilan_poids_moyen.4, paste(unique(donnees$lot[is.na(donnees$effectif)]),collapse=" ")))
	don=donnees[,c(8,6,4,1)]
	coe=coeff[,c(10,9)]
	
	# graphique des poids moyens en fonction du milieu de l'operation de contrele)
	# la date est la date moyenne du lot
	
# fonction handler appellees
	hgra=function(h,...){
		p<-qplot(x=date,y=poids_moyen,data=don) 
		print(p)
		assign("p",p,envir=envir_stacomi)
		funout("object p assigned to envir_stacomi")
	}
	hcoe=function(h,...){
		type_poids= switch (bilan_poids_moyen@liste@listechoice,
				">1"=get("msg",envir_stacomi)$Bilan_poids_moyen.5,
				"=1"=get("msg",envir_stacomi)$Bilan_poids_moyen.6,
				"tous"=get("msg",envir_stacomi)$Bilan_poids_moyen.7)  
		plot(x=don$date,y=don$poids_moyen,xlab=get("msg",envir_stacomi)$Bilan_poids_moyen.8,
				ylab=get("msg",envir_stacomi)$Bilan_poids_moyen.9,
				main=paste(get("msg",envir_stacomi)$Bilan_poids_moyen.10,
						type_poids,get("msg",envir_stacomi)$lBilan_poids_moyen.11,
						bilan_poids_moyen@anneedebut@annee_selectionnee,get("msg",envir_stacomi)$Bilan_poids_moyen.12,
						bilan_poids_moyen@anneefin@annee_selectionnee),
				ylim=c(0.2,0.5))
		points(coe$date,coe$poids_moyen, col="cyan")
		if (h$action=="coe") {
			legend("topright",c("Obs.", "Coeff base"), col=c("black","cyan"),pch="o",cex = 0.8)
		} 
	}
	hsize=function(h,...){
		print(p+aes(size=effectif))
	}
	hreg=function(h,...){
		seq=seq(as.Date(bilan_poids_moyen@coe@datedebut),as.Date(bilan_poids_moyen@coe@datefin),by="day")
		don$jour=as.numeric(strftime(as.POSIXlt(don$date),format="%j"))
		jour=as.numeric(strftime(as.POSIXlt(seq),format="%j"))
		coe=data.frame("date"=seq,"poids_moyen"=NA,"effectif"=NA,"lot"=NA,"jour"=jour)
		
		fm <- stats::nls(formula=poids_moyen ~ a*cos(2*pi*(jour-T)/365)+b ,data=don,start=list(a=10,T=30,b=10))
		pred<-stats::predict(fm, newdata=coe)
		com=paste(get("msg",envir_stacomi)$Bilan_poids_moyen.13,paste("a=",round(coef(fm),2)[1],"T=",round(coef(fm),2)[2],"b=",round(coef(fm),2)[3],collapse=""))
		if (h$action=="reg" ){     # bouton reg clique
			hcoe(h)
			points(as.POSIXct(seq),pred, col="magenta")
			legend("topright",c("Obs.", "Coeff base","Mod"), col=c("black","cyan","magenta"),pch="o",cex = 0.8)
			mtext(com,side=3,line=0.5)     
		}  else if (h$action=="export" ){ # bouton export clique
			import_coe=data.frame(
					"coe_tax_code"='2038',
					"coe_std_code"='CIV',
					"coe_qte_code"=1,
					"coe_date_debut"=seq[-length(seq)],
					"coe_date_fin"=seq[-1],
					"coe_valeur_coefficient"=1/pred[-length(seq)],
					"coe_commentaires"=com)
			fileout= paste("C:/base/","import_coe",bilan_poids_moyen@anneedebut@annee_selectionnee,bilan_poids_moyen@anneefin@annee_selectionnee,".csv",sep="")
			#attention ecrit dans C:/base....
			utils::write.table(import_coe,file=fileout, row.names = FALSE,sep=";")
			assign("import_coe",import_coe,envir=envir_stacomi)
			funout(get("msg",envir_stacomi)$Bilan_poids_moyen.14)
			funout(paste(get("msg",envir_stacomi)$Bilan_poids_moyen.15,fileout,"\n"))
			
			requete=new("RequeteODBC")
			requete@baseODBC=get("baseODBC",envir=envir_stacomi)
			requete@sql=paste("COPY ",get("sch",envir=envir_stacomi),"tj_coefficientconversion_coe FROM '",fileout, "' USING DELIMITERS ';' WITH CSV HEADER NULL AS '';",sep="")
			requete=stacomirtools::connect(requete)  # appel de la methode stacomirtools::connect de l'object requeteODBC
		}
	}
	
	hexp=function(h,...){
		# export d'un tableau que l'on peut ecrire dans la base
		gWidgets::gconfirm(get("msg",envir_stacomi)$Bilan_poids_moyen.16,
				title=get("msg",envir_stacomi)$Bilan_poids_moyen.17,
				handler=hreg,action="export")
	}
	hquit=function(h,...){
		delete(toolbarlistgraph,group)
		delete(toolbarlistgraph1,group)  
	}
	### deuxieme toobar    
	aGra=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.18,icon="lines",handler=hgra)
	aCoe=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.19,icon="Coe",handler=hcoe,action="coe") 
	aSize=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.20,icon="gWidgetsRGtk2-bubbles",handler=hsize)         
	aReg=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.21,icon="gWidgetsRGtk2-function1",handler=hreg,action="reg")
	aExp=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.22,icon="gWidgetsRGtk2-dataframe",handler=hexp)         
	aQuit1=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.23,icon="close", handler=hquit)
	toolbarlistgraph <- gmenu(list(gra=aGra,coe=aCoe,size=aSize))
	assign("toolbarlistgraph",toolbarlistgraph,.GlobalEnv)
	toolbarlistgraph1<-gmenu(list(reg=aReg,exp=aExp,Quit1=aQuit1))
	assign("toolbarlistgraph1",toolbarlistgraph1,.GlobalEnv)
	add(group,toolbarlistgraph)
	add(group,toolbarlistgraph1)  
}   

#
funtableBilan_poids_moyen = function(h,...) {
	bilan_poids_moyen=charge(bilan_poids_moyen)  
	donnees=bilan_poids_moyen@data # on recupere le data.frame  
	assign("bilan_poids_moyen",bilan_poids_moyen,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_poids_moyen.3)  
	donnees[is.na(donnees)]<-""  
	donnees$ope_date_debut=as.character(donnees$ope_date_debut)  
	donnees$ope_date_fin=as.character(donnees$ope_date_fin)   
	donnees$datemoy=as.character(donnees$datemoy)    
	gdf(donnees, container=TRUE)    
}   
