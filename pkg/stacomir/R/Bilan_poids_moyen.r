# Nom fichier :        Bilanpoidsmoyen.R    (classe)
# Projet :             controle migrateur 
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand00@gmail.com
# Date de creation :   07/02/2009 21:30:54
# TODO Calcul des valeurs en interannuel pour le calage de la regression et d'une valeur par defaut des coefficients de conversion
#' Bilan_poids_moyen class
#' Objectif faire un calcul des poids moyens et permettre de reintegrere les coefficents de conversion poids effectif
#' @note Un programme permet pour l'iav d'étendre le chargement des donnees de la base mortciv pour etendre la gamme des valeurs modelisees OK done internal IAV
#' @slot data="data.frame"
#' @slot dc="RefDC"
#' @slot anneedebut="RefAnnee"
#' @slot anneefin="RefAnnee"
#' @slot coe="RefCoe"
#' @slot liste="RefListe"
#' @method connect
#' @method charge
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
#' @expamples objet=new("Bilan_poids_moyen")
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
setMethod("connect",signature=signature("Bilan_poids_moyen"),definition=function(objet,h) {
			#CHARGEMENT DU TABLEAU DES POIDS MOYENS
			requete=new("RequeteODBCwheredate")
			requete@baseODBC=baseODBC
			requete@datedebut=strptime(paste(objet@anneedebut@annee_selectionnee-1,"-08-01",sep=""),format="%Y-%m-%d")
			requete@datefin=strptime(paste(objet@anneefin@annee_selectionnee,"-08-01",sep=""),format="%Y-%m-%d")
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@select= paste("SELECT lot_identifiant,ope_date_debut,ope_date_fin,lot_effectif,car_valeur_quantitatif as poids,",
					" (car_valeur_quantitatif/lot_effectif) AS poids_moyen,",
					" (ope_date_fin-ope_date_debut)/2 AS duree,",
					" ope_date_debut+(ope_date_fin-ope_date_debut)/2 as datemoy,",
					" date_part('year', ope_date_debut) as annee,",
					" date_part('month',ope_date_debut) as mois",
					" FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car_qan",sep="")
			requete@and=paste(" AND ope_dic_identifiant=",objet@dc@dc_selectionne,
					" AND std_libelle='civelle'",
					ifelse(objet@liste@listechoix=="tous", "",paste(" AND  lot_effectif", objet@liste@listechoix)),
					" AND upper(car_methode_obtention::text) = 'MESURE'::text",
					" AND car_par_code='A111'",sep="")
			requete<-connect(requete)			
			objet@data<-requete@query			
			#CHARGEMENT DES COEFFICIENTS DE CONVERSION
			objet@coe@datedebut=requete@datedebut
			objet@coe@datefin=requete@datefin
			objet@coe<-charge(objet@coe)
			funout(get("msg",envir_stacomi)$Bilan_poids_moyen.1)
			funout(paste(nrow(objet@coe@data),get("msg",envir_stacomi)$Bilan_poids_moyen.2))
			
			return(objet)
		})

# Cette methode permet de verifier que les boites ont ete cliquees et va chercher les
# objets qui sont colles dans l'environnement envir_stacomi de l'interface 
#' charge method for Bilan_poids_moyen class
#' @param h a handler 
#' @returnType Bilan_poids_moyen class
#' @return Bilan_poids_moyen with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
setMethod("charge",signature=signature("Bilan_poids_moyen"),definition=function(objet,h) {
			if (exists("refliste",envir_stacomi)) {      
				objet@liste<-get("refliste",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.9, arret=TRUE)             
			} 
			if (exists("refDC",envir_stacomi)) {      
				objet@dc<-get("refDC",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)          
			}            
			if (exists("refAnneeDebut",envir_stacomi)) {      
				objet@anneedebut<-get("refAnneeDebut",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.10,arret=TRUE)             
			}
			if (exists("refAnneeFin",envir_stacomi)) {      
				objet@anneefin<-get("refAnneeFin",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.11,arret=TRUE)       
			}                    
			objet<-connect(objet)    
			return(objet) 
		})



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
		# si il existe j'ai traffique l'objet bilan poids moyen à la main
		# et je ne souhaite pas qu'il soit ecrase
		bilan_poids_moyen=charge(bilan_poids_moyen)
	}
	donnees=bilan_poids_moyen@data # on recupere le data.frame
	coeff=bilan_poids_moyen@coe@data
	coeff$poids_moyen=1/coeff$coe_valeur_coefficient
	coeff$date=as.POSIXct(coeff$coe_date_debut)
	assign("bilan_poids_moyen",bilan_poids_moyen,envir_stacomi)#assign("bilan_lot",vue_ope_lot,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_poids_moyen.3)
	# changement des noms
	donnees=chnames(donnees,c("lot_identifiant","ope_date_debut","ope_date_fin","lot_effectif","poids","poids_moyen","duree","datemoy"),
			c("lot","date","date_fin","effectif","poids","poids_moyen","duree","date"))
	# correction de manques d'effectifs dans la base
	if (sum(is.na(donnees$effectif))>0) warnings(paste(get("msg",envir_stacomi)$Bilan_poids_moyen.4, paste(unique(donnees$lot[is.na(donnees$effectif)]),collapse=" ")))
	don=donnees[,c(8,6,4,1)]
	coe=coeff[,c(9,8)]
	
	# graphique des poids moyens en fonction du milieu de l'operation de contrôle)
	# la date est la date moyenne du lot
	
# fonction handler appellees
	hgra=function(h,...){
		p<<-qplot(x=date,y=poids_moyen,data=don) 
		print(p)
	}
	hcoe=function(h,...){
		type_poids= switch (bilan_poids_moyen@liste@listechoix,
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
		
		fm <- nls(formula=poids_moyen ~ a*cos(2*pi*(jour-T)/365)+b ,data=don,start=list(a=10,T=30,b=10))
		pred<-predict(fm, newdata=coe)
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
			write.table(import_coe,file=fileout, row.names = FALSE,sep=";")
			assign("import_coe",import_coe,envir=envir_stacomi)
			funout(get("msg",envir_stacomi)$Bilan_poids_moyen.14)
			funout(paste(get("msg",envir_stacomi)$Bilan_poids_moyen.15,fileout,"\n"))
			
			requete=new("RequeteODBC")
			requete@baseODBC
			requete@baseODBC=baseODBC
			requete@sql=paste("COPY ",get("sch",envir=envir_stacomi),"tj_coefficientconversion_coe FROM '",fileout, "' USING DELIMITERS ';' WITH CSV HEADER NULL AS '';",sep="")
			requete=connect(requete)  # appel de la methode connect de l'objet requeteODBC
		}
	}
	
	hexp=function(h,...){
		# export d'un tableau que l'on peut ecrire dans la base
		gconfirm(get("msg",envir_stacomi)$Bilan_poids_moyen.16,
				title=get("msg",envir_stacomi)$Bilan_poids_moyen.17,
				handler=hreg,action="export")
	}
	hquit=function(h,...){
		delete(toolbarlistgraph,group)
		delete(toolbarlistgraph1,group)  
	}
	### deuxieme toobar    
	aGra=gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.18,icon="lines",handler=hgra)
	aCoe=gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.19,icon="Coe",handler=hcoe,action="coe") 
	aSize=gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.20,icon="gWidgetsRGtk2-bubbles",handler=hsize)         
	aReg=gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.21,icon="gWidgetsRGtk2-function1",handler=hreg,action="reg")
	aExp=gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.22,icon="gWidgetsRGtk2-dataframe",handler=hexp)         
	aQuit1=gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.23,icon="close", handler=hquit)
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