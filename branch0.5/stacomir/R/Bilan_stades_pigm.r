#' Bilan class for pigment stage structure analysis (glass eel)
#' 
#' The pigment stages analysis has been developed to allow to analyze the
#' change in pigment stage structure for glass eel (Anguilla anguilla).  The
#' class uses the parameters calibrated by Briand et al. , 2005 to
#' backcalculate the probable date when the glass eels arrived in the estuary
#' (i.e. at a fully transparent stage VB. The evolution of pigment stages is
#' modeled with gamma functions which use a pigment time calculated from daily
#' temperatures and salinities.  The temperatures has a major influence on the
#' glass eel pigment stage evolution.
#' @include RefCheckBox.r
#' @include ReftextBox.r
#' @note This class is displayed by interface_bilan_stades_pigm, The class uses
#' temperature (from an abiotic measure station) and mean salinity to calculate
#' the change towards one stage
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_stades_pigm", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
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
#' \code{\link{fnstade}}
#' @references BRIAND C., FATIN D., CICCOTTI E. and LAMBERT P., 2005. A
#' stage-structured model to predict the effect of temperature and salinity on
#' glass eel Anguilla anguilla pigmentation development. J Fish Biol, 67,
#' 995-1009.
#' \url{http://www3.interscience.wiley.com/journal/118686679/abstract}
#' \url{http://www.eptb-vilaine.fr/site/index.php/publications-scientifiques/46-publications-migrateurs/60-dynamique-de-population-et-de-migration-des-civelles-en-estuaire-de-vilaine.}
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @examples
#' 
#' showClass("Bilan_stades_pigm")
#' @export 
setClass(Class="Bilan_stades_pigm",
		representation= representation(data="data.frame",
				datatempsal="data.frame",
				tablestades="data.frame",
				phi="list",
				dates="POSIXt",
				Vparm="list",
				dc="RefDC",
				stationMesure="RefStationMesure",
				horodate="RefHorodate",
				datedebut="POSIXlt",
				datefin="POSIXlt",
				lmax="RefChoix",
				options="RefCheckBox",
				salinite="RefTextBox",
				labelretro="character",
				labelgraphstades="character",
				effectifs="numeric"),
		prototype=prototype(data=data.frame(),
				datatempsal=data.frame(),
				phi=data.frame(),
				Vparm=list(pigment_stage=list("p1"=0.267,"p2"=0.835,"p3"=1.560,"p4"=3.682),
						pigmentation=list("teta"=30,"sigma"=40,"sigma2"=-5,"p5"=4.566,
								"p6"=8.141,"p7"=0.071,"p8"=0.426)),
				dc=new("RefDC"),
				stationMesure=new("RefStationMesure"),
				horodate=new("RefHorodate"),
				lmax=new("RefChoix"),
				options=new("RefCheckBox"),
				salinite=new("RefTextBox")
		))
#
#' connect method for Bilan_stades_pigm
#' 
#' 
#' @note will try to get data for the temperature (refstation) only if retrocalcul is checked
#'  by default it is not when lanching
#' @return An object of class Bilan_stades_pigm
#' @param h a handler
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("connect",signature=signature("Bilan_stades_pigm"),definition=function(object,h) {
			# pour debug object<-new("Bilan_stades_pigm")
			#  chargement du tableau des stades pigmentaires
			requete=new("RequeteODBCwheredate")
			requete@baseODBC=get("baseODBC",envir=envir_stacomi)
			requete@select= paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car",sep="")
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@order_by="ORDER BY ope_date_debut"
			requete@datedebut=strptime(object@datedebut,format="%Y-%m-%d")
			requete@datefin=strptime(object@datefin,format="%Y-%m-%d")
			requete@and=paste(" AND ope_dic_identifiant=",object@dc@dc_selectionne,
					" AND lot_tax_code= '2038'",
					" AND lot_std_code= 'CIV'",
					" AND car_par_code='1791'",sep="")
			requete<-connect(requete) # appel de la methode connect de l'object ODBCWHEREDATE
			funout(get("msg",envir_stacomi)$Bilan_stades_pigm.1)
			object@data<-stacomirtools::killfactor(requete@query)
			if (nrow (requete@query)>0)	{
				
				stades<-stacomirtools::killfactor(requete@query)
				choixpere=c("lotpere","date")
				funout(paste("Attention il peut y avoir plusieurs lots a la meme date, et certains stades sont fait sans lotpere (ex taille-poids-stade)\n"))
				choixpere=select.list(choixpere,preselect="date",multiple=FALSE,
						title=paste("Regroupement des ech par lot pere ou par date ?"))
				lst<-fntablestade(stades,choixpere)
				dates<-lst[["dates"]]
				tablestades<-lst[["tablestades"]]
				# transformation en pourcentages
				effectifs=rowSums(tablestades)
				object@effectifs<-effectifs
				tablestades=tablestades/effectifs
				object@tablestades<-tablestades
				object@dates<-dates
			} else funout("pas de donnees de stades pigmentaires",arret=TRUE)
			if (object@options@checked[2]){
				# chargement du tableau des temperatures
				requete@datedebut=as.POSIXlt(strptime(object@datedebut,format="%Y-%m-%d")-5184000) # 60 jours avant
				requete@colonnedebut="env_date_debut"
				requete@colonnefin="env_date_fin"
				requete@select=paste("SELECT", 
						" env_date_debut,",
						" env_date_fin,",
						" env_methode_obtention,",
						" val_libelle as env_val_identifiant,",
						" env_valeur_quantitatif,",
						" env_stm_identifiant",
						" FROM ",get("sch",envir=envir_stacomi),"tj_conditionenvironnementale_env",
						" LEFT JOIN ref.tr_valeurparametrequalitatif_val on env_val_identifiant=val_identifiant",sep="")
				requete@order_by<-"ORDER BY env_stm_identifiant, env_date_debut"			
				tmp<-vector_to_listsql(object@stationMesure@data$stm_identifiant)
				requete@and=paste(" AND env_stm_identifiant IN ",tmp )			
				requete<-connect(requete)
				funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.1)
				if (nrow (requete@query)>0)	{
					if (unique(requete@query$env_stm_identifiant)>1) funout("vous avez choisi plusieurs stations", arret=TRUE)
					object@datatempsal<-stacomirtools::killfactor(requete@query)[,c("env_date_debut","env_valeur_quantitatif")]
					object@datatempsal$salinite=as.numeric(object@salinite@label)
					colnames(object@datatempsal)<-c("date","temperature","salinite")
				} else {
					funout("pas de donnees de temperature, vous ne pourrez pas faire de retrocalcul des dates d'arrivees")
				}
			}
			return(object)
		})

#' function calculating a table with pigment stages VB to VIA3 from lines retreived from the database
#' containing individual characteristic of glass eel#' 
#' this function is called from within the charge method it was separated from the charge method
#' as it it convenient to use elsewhere
#' @usage fntablestade(stades,choixpere="lotpere")
#' @param stades a data frame containing stage values
#' @param choixlotpere either "date" or "lot_pere" the first will group pigment stage by date, 
#' the second will allow to keep separate lines when several samples have been collected a given day   
#' @return a list with tablestades atable with numbers per stage for a given date or lotpere (sample), and date                                                                                                                
#' @author Cedric Briand \\email{cedric.briand"at"eptb-vilaine.fr}                                                                                                                           
#' @seealso \code{\linkS4class{Bilan_stades_pigm}}                                                                                                                                    
fntablestade<-function(stades,choixpere="lotpere"){
	if (choixpere=="lotpere"){
		tablestades=stats::ftable(stats::xtabs(stades$lot_effectif ~ stades$lot_pere +
								+ stades$val_libelle))
		tablestades<-tab2df(tablestades)# fonction developpee dans utilitaires
		# recuperation des dates correspondant aux numeros d'operation
		# le format de ftable n'est pas celui d'un data frame
		indx<-match(sort(unique(stades$lot_pere)),stades$lot_pere)
		dates<-stades[indx,"ope_date_debut"]
		# creation d'une matrice qui somme les stades VA+VB et les stades VIA3 et VIA4
		if ("VA"%in%dimnames(tablestades)){
			tablestades$VB=tablestades$VB+tablestades$VA
			tablestades=tablestades[,-c("VA")]
		}
		if ("VIA4"%in%dimnames(tablestades)){
			tablestades$VIA3=tablestades$VIA3+tablestades$VIA4
			tablestades=tablestades[,-"VIA4"]
		}
		tablestades=tablestades[order(dates),]   # on reclasse par dates
		print(cbind(tablestades, "lot_pere"=sort(unique(stades$lot_pere))[order(dates)]))
		dates=sort(dates)
		# je colle les numeros de lots peres en les reordonnant en fonction du classt des dates
	} else if (choixpere=="date"){
		tablestades=stats::ftable(stats::xtabs(stades$lot_effectif ~ stades$ope_date_debut +
								+ stades$val_libelle))
		print(stats::xtabs(stades$lot_effectif  ~ stades$ope_date_debut +
								+ stades$val_libelle))
		dates<-sort(unique(stades$ope_date_debut))
		tablestades<-tab2df(tablestades) # fonction developpee dans utilitaires
		if ("VA"%in%dimnames(tablestades)[[2]]){
			tablestades$VB=tablestades$VB+tablestades$VA
			tablestades=tablestades[,-c("VA")]
		}
		if ("VIA4"%in%dimnames(tablestades)[[2]]){
			tablestades$VIA3=tablestades$VIA3+tablestades$VIA4
			tablestades=tablestades[,-match("VIA4",dimnames(tablestades)[[2]])]
		}
		#dimnames(tablestades) <- list(as.character(dates),
		#		c("VB","VIA0","VIA1","VIA2","VIA3"))
	}
	
	return(list("tablestades"=tablestades,"dates"=dates))
}		
#' Charge method for BilanStadePigm
#' 
#' method used by the graphical interface to build object from values
#' stored in envir_stacomi
#' 
#' @return Bilan_stades_pigm with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("Bilan_stades_pigm"),definition=function(object,h) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			} 	
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_stades_pigm_date_debut",envir_stacomi)) {
				object@datedebut<-get("bilan_stades_pigm_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.5,arret=TRUE)
			}
			if (exists("bilan_stades_pigm_date_fin",envir_stacomi)) {
				object@datefin<-get("bilan_stades_pigm_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.6,arret=TRUE)
			}         
			if (exists("refCheckBox",envir_stacomi)) {
				object@options<-get("refCheckBox",envir_stacomi)
			} else {
				# rien de toutes facons les choix par defaut sont copies dans envir_stacomi
			}  
			if (exists("refchoix",envir_stacomi)) {
				object@lmax<-get("refchoix",envir_stacomi)
			} else {
				# l'assignation d'un object liste choix remplace la liste des valeurs possibles
				# par la valeur choisie (pour l'instant "0.8")
				object@lmax@listechoix<-"0.8"
			}
			if (exists("refTextBox",envir_stacomi)) {
				object@salinite<-get("refTextBox",envir_stacomi)
			} else {
				# rien de toutes fa�ons les choix par defaut sont copies dans envir_stacomi
			} 
			if (object@options@checked[2]){
				if (exists("refStationMesure",envir_stacomi)) {
					object@stationMesure<-get("refStationMesure",envir_stacomi)
				} else {
					funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.2,arret=TRUE)
				}
			}
			object<-connect(object)			
			return(object)
		})


#' plots polygons
#' 
#' 
#' @param xmat a matrix of x values of the polygons
#' @param ymat a matrix where the number of rows is of the same length as xmat
#' @param ordre the order in which the polygons will be drawn
#' @param couleur a color vector
#' @param \dots additional arguments passed to the function
#' @return Bilan_stades_pigm with slots filled with user choice
#' @author Laurent Beaulaton \email{laurent.beaulaton"at"onema.fr}
surface=function(xmat,ymat,ordre=1:dim(ymat)[2],couleur=1:dim(ymat)[2],...) {
	x=c(xmat,rev(xmat))
	nbcol=dim(ymat)[2]
	nblig=dim(ymat)[1]
	total=numeric(nblig)
	for (i in 1:nbcol) total=total+ymat[,i]	
	nouvmat=matrix(nrow=nblig*2,ncol=nbcol)
	nouvmat[1:nblig,1]=ymat[,ordre[1]]
	nouvmat[(nblig+1):(nblig*2),1]=0
	for (i in 2:nbcol) {
		nouvmat[1:nblig,i]=ymat[,ordre[i]]+nouvmat[1:nblig,i-1]
		nouvmat[(nblig+1):(nblig*2),i]=rev(nouvmat[1:nblig,i-1])
	}
	plot(x[1:nblig],total,type="l",ylim=c(0,max(total)*1.1),...)
	for (i in 1:nbcol) polygon(x,nouvmat[,i],col=couleur[i])	
}



#' Function to calculate pigmentation times.
#' 
#'  
#' @param parm parameters of the model
#' @param datatempsal data.frame containing temperatures and salinities
#' @return list("dates"=duree,"phi_jour"=phi_jour)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funphi<-function(parm,datatempsal){
	temperature=datatempsal$temperature
	salinity=datatempsal$salinite
	duree=as.character(datatempsal$date)
	phi_T=pbeta(temperature/parm$teta,parm$p5,parm$p6)  #(duree,nb_area)
	phi_S=1-pbeta((salinity-parm$sigma2)/(parm$sigma-parm$sigma2),parm$p7,parm$p8)
	phi_jour=phi_T*phi_S
	return(list("dates"=duree,"phi_jour"=phi_jour))
}







#' function drawing polygon from gamma law describing pigmentation change in
#' glass eel
#' 
#' function calculating from the gamma law the coordinates x and y allowing to
#' draw a polygon, the function fnstade may be used to draw a polygon(neg=TRUE)
#' or simply return the values from gamma function of each stage
#' 
#' 
#' @param par1 Parameter describing the gamma law for the first stage
#' @param par2 Parameter of the gamma law for the second stage
#' @param phicum cumulated pigmentation times for test : phicum=seq(0,20,
#' length.out=100)
#' @param phidates Dates
#' @param VB If TRUE, then calculation for first stage VB which differs from
#' the others
#' @param neg If FALSE then calculation of stages according to the pigmentation
#' time
#' @param lmax Scale parameter for the graphical function, lmax=0 allows to
#' draw the real values of abundances per stage along time, lmax=1 or 0.8 will
#' draw all stages at the same scale
#' @return A list with x and y
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso \code{\linkS4class{Bilan_stades_pigm}}
#' @references BRIAND C., FATIN D., CICCOTTI E. and LAMBERT P., 2005. A
#' stage-structured model to predict the effect of temperature and salinity on
#' glass eel Anguilla anguilla pigmentation development. J Fish Biol, 67,
#' 995-1009.
#' \url{http://www3.interscience.wiley.com/journal/118686679/abstract}
#' \url{http://www.eptb-vilaine.fr/site/index.php/publications-scientifiques/46-publications-migrateurs/60-dynamique-de-population-et-de-migration-des-civelles-en-estuaire-de-vilaine.}
fnstade<-function(par1, par2=NULL,phicum,phidates,VB=FALSE,neg=TRUE,lmax=1){
	if (neg){
		phidates=as.numeric(as.POSIXct(strptime(phidates,format="%Y-%m-%d")))
	}
	sequ=phicum
	if (VB){ #VB
		dist1<-stats::pgamma(sequ,par1)
		dist=1-dist1
	} else if (is.null(par2)) { # VIA3
		dist1<-stats::pgamma(sequ,par1)
		dist=dist1
	}  else if (!is.null(par2)){      # VIA0...VIA3
		dist1<-stats::pgamma(sequ,par1)
		dist2<- stats::pgamma(sequ,par2)
		dist=dist1-dist2          
	}
	if (lmax>0){
		dist=lmax*dist/max(dist)
	}
	if (neg){
		x=c(rev(phidates),phidates)
		y=c(rev(dist),-dist)
	} else {
		x=sequ
		y=dist
	}
	return(list("x"=x,"y"=y))
}







#' used by \code{fundist} function, returns the value of obj where more than 50
#' percent of the distribution objc is reached
#' 
#' Allows to point the middle of the gamma distribution for each stage
#' 
#' 
#' @param obj The pigment time scale
#' @param objc Cumulated gamma curve
#' @return d50 The center of the distribution for the different stages
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso \code{\linkS4class{Bilan_stades_pigm}}
fun50<-function(obj,objc){
	d50<-obj[objc>0.5][1]
	return(d50)
}






#' fundist =function to calculate the median of the distribution of pigment
#' stages
#' 
#' see code
#' 
#' 
#' @param Vparm Parameters for the gamma functions describing pigment stages
#' @param phicum Cumulated pigmentation time
#' @param graph Logical, to see the curves type graph = TRUE
#' @param lmax Scale parameter of the graphical function see \link{fnstade}
#' @note pigment stage functions are not standard statistical distribution,
#' calculating where 50\% of the distribution lies is done with fun50 this
#' function uses \link{fnstade} to calculate the values of pigment times on a
#' regular scale (phicum)
#' @seealso \code{\linkS4class{Bilan_stades_pigm}}
#' @examples
#' 
#' Vparm<-list()
#' # below param for briand et al.,2005 pigmentation function in glass eel
#' Vparm$pigment_stage$p1<-0.267 # parameters for gamma functions describing changes from stage to stage
#' Vparm$pigment_stage$p2<-0.835
#' Vparm$pigment_stage$p3<-1.56
#' Vparm$pigment_stage$p4<-3.682
#' Vparm$pigmentation$teta<- 30 # bounding parameters for beta function
#' Vparm$pigmentation$sigma<-40 # bounding parameters for beta function
#' Vparm$pigmentation$sigma2<--5 # bounding parameters for beta function
#' Vparm$pigmentation$p5<- 4.566 # param for beta function
#' Vparm$pigmentation$p6<-8.141
#' Vparm$pigmentation$p7<-0.071 # param for beta function
#' Vparm$pigmentation$p8<-0.426
#' fundist(Vparm,seq(0,10, length.out=10000),graph=FALSE,lmax=1)
#' fundist(Vparm,seq(0,10, length.out=10000),graph=TRUE,lmax=1)
#' fundist(Vparm,seq(0,10, length.out=10000),graph=TRUE,lmax=0)
#' plot(seq(0,10, length.out=10000),pgamma(seq(0,10, length.out=10000),Vparm$pigment_stage[[1]]),col="pink")
#' points(seq(0,10, length.out=10000),pgamma(seq(0,10, length.out=10000),Vparm$pigment_stage[[2]]),col="firebrick") 
#' 
fundist=function(Vparm, phicum,graph=TRUE,lmax=1){
	VB=fnstade(par1=Vparm$pigment_stage[[1]],VB=TRUE,phicum=phicum,neg=FALSE,lmax=lmax)
	VBc=cumsum(VB$y)/sum(VB$y)  # surface
	VIA0= fnstade(par1=Vparm$pigment_stage[[1]],par2=Vparm$pigment_stage[[2]],VB=FALSE,phicum=phicum,neg=FALSE,lmax=lmax)
	VIA0c=cumsum(VIA0$y)/sum(VIA0$y)  # surface
	VIA1= fnstade(par1=Vparm$pigment_stage[[2]],par2=Vparm$pigment_stage[[3]],VB=FALSE,phicum=phicum,neg=FALSE,lmax=lmax)
	VIA1c=cumsum(VIA1$y)/sum(VIA1$y)  # surface
	VIA2= fnstade(par1=Vparm$pigment_stage[[3]],par2=Vparm$pigment_stage[[4]],VB=FALSE,phicum=phicum,neg=FALSE,lmax=lmax)
	VIA2c=cumsum(VIA2$y)/sum(VIA2$y)  # surface
	VIA3= fnstade(par1=Vparm$pigment_stage[[4]],VB=FALSE,phicum=phicum,neg=FALSE,lmax=lmax)
	VIA3c=cumsum(VIA3$y)/sum(VIA3$y)  # surface
	if(graph){
		x11()
		matplot(VB$x,cbind(VB$y,VIA0$y,VIA1$y,VIA2$y,VIA3$y))
		
		x11()
		matplot(VB$x,cbind(VBc,VIA0c,VIA1c,VIA2c,VIA3c))
	}
	#traitement a part de VB
	# raison = ca marche pas sinon
	# dans le modele VB = 1-p(VIA0) proba de ne pas etre au stade suivant ? 
	out=c( VB$x[VB$x>Vparm$pigment_stage[[1]]][1],
			fun50(VIA0$x,VIA0c),
			fun50(VIA1$x,VIA1c),
			fun50(VIA2$x,VIA2c),
			fun50(VIA3$x,VIA3c))
	#list("VB"=VB,"VBc"=VBc,
#            "VIA0"=VIA0,"VIA0c"=VIA0c,
#            "VIA1"=VIA1, "VIA1c"=VIA1c,
#            "VIA2"=VIA2, "VIA2c"=VIA2c,
#            "VIA3"= VIA3, "VIA3c"=VIA3c)
	return(out)   
}







#' main launching function for class Bilan_stades_pigm
#' 
#' Function with handler which calls charge (and thus connect) and calculates
#' the title
#' 
#' 
#' @param h A handler
#' @param additional arguments
#' @param list() additional arguments
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funcalcbilan_stades_pigm<-function(h,...){
	bilan_stades_pigm<-charge(bilan_stades_pigm)
	if (nrow(bilan_stades_pigm@datatempsal)>0){
		bilan_stades_pigm@phi<-funphi(parm=bilan_stades_pigm@Vparm$pigmentation,bilan_stades_pigm@datatempsal)
	}
	funout("Chargement des donnees dans la base ")
	dates<-bilan_stades_pigm@dates
	bilan_stades_pigm@labelgraphstades<-paste(get("msg",envir=envir_stacomi)$Bilan_stades_pigm.4,
			if(strftime(as.POSIXlt(dates[1]),"%Y")==
							strftime(as.POSIXlt(dates[length(dates)]),"%Y")) {
						strftime(as.POSIXlt(dates[1]),"%Y")} else { paste(
								strftime(as.POSIXlt(dates[1]),"%Y"),"-",
								strftime(as.POSIXlt(dates[length(dates)]),"%Y"))},
			get("msg",envir=envir_stacomi)$Bilan_stades_pigm.5)
	bilan_stades_pigm@labelretro="dates d'arrivees en estuaires"
	enabled(toolbarlist[["SetTitle"]])<-TRUE
	enabled(toolbarlist[["Graph"]])<-TRUE
	enabled(toolbarlist[["Graphgg"]])<-TRUE
	assign("bilan_stades_pigm",bilan_stades_pigm,.GlobalEnv)
}

#' handler function for fungraphstades
hfungraphstades=function(h,...){
	fungraphstades(
			tablestades=bilan_stades_pigm@tablestades,
			retrocalcul=bilan_stades_pigm@options@checked[2],
			datatempsal=bilan_stades_pigm@datatempsal,    
			points=bilan_stades_pigm@options@checked[3],
			nb=bilan_stades_pigm@options@checked[4],
			graphstades=bilan_stades_pigm@options@checked[1],  
			lmax=as.numeric(bilan_stades_pigm@lmax@listechoix), 
			labelretro=bilan_stades_pigm@labelretro,
			labelgraphstades=bilan_stades_pigm@labelgraphstades,
			phi=bilan_stades_pigm@phi, # tableau des temps pigmentaires et des dates format "%d/%m/%Y"
			maxVIA3=10, # valeur maximale autorisee pour VIA3 
			dates=bilan_stades_pigm@dates,
			Vparm=bilan_stades_pigm@Vparm,
			effectifs=bilan_stades_pigm@effectifs  # pour le label si nb =TRUE
	)
}







#' Main function for class Bilan_stades_pigm allowing to calculate and then
#' draw the graphs
#' 
#' see R code for details
#' 
#' 
#' @param tablestades A data frame with stages VB VIA0 VIA1 VIA2 VIA3
#' @param retrocalcul Logical TRUE or FALSE, do you want to retrocalculate when
#' the glass eel have arrived in the estuary, in this case provide datatempsal,
#' data for temperature and salinity
#' @param datatempsal to draw the graph of recalculated dates of arrival,
#' provide this data.frame, format graphique des durees en estuaire, format
#' [,c("date","temperature","salinite")]
#' @param points logical, do you want to draw the points on the cumulative
#' pigmentation graph
#' @param nb Do you want to write number in sample in the pigmentation stage
#' graph
#' @param graphstades Do you want to draw the graph of cumulated stage
#' @param lmax parameter for retrocalcul graph, see \link{fnstade} scale
#' parameter for the graphical function, use 0.8 to avoid overlapping of the
#' polygons from several samples or dates
#' @param labelretro label for retrocalcul graph, can be changed in the
#' graphical interface
#' @param labelgraphstades label for stage graph, can be changed in the
#' graphical interface
#' @param phi table of pigmentation time and dates format "\%d/\%m/\%Y"
#' @param maxVIA3 10, maximum value of pigment time for VIA3, limits the
#' duration of this longer stage
#' @param dates dates
#' @param Vparm parameters for pigment stage function
#' @param effectifs logical : do you want to display numbers on the graph
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fungraphstades<-function(
		tablestades,
		retrocalcul=TRUE,  # deuxieme partie du graphe dans ce cas fournir datatempsal
		datatempsal,    # graphique des durees en estuaire, format [,c("date","temperature","salinite")]
		points=TRUE,    # affichage des points
		nb=TRUE, # affichage des effectifs
		graphstades=TRUE,  # affichage du graphe pour evol stades
		lmax=1, # largeur ex:0.8 pour eviter un chevauchement des graphes 
		labelretro, # titre du graphe retro si celui-ci est trace tout seul
		labelgraphstades,
		phi, # tableau des temps pigmentaires et des dates format "%d/%m/%Y"
		maxVIA3=10, # valeur maximale autorisee pour VIA3 
		dates,
		Vparm,
		effectifs  # pour le label si nb =TRUE
){
	VB  =as.vector(tablestades[,1])  #vector of stades VB+VIA0 observed data
	VIA0=as.vector(tablestades[,2]+tablestades[,1])
	VIA1=as.vector(tablestades[,3]+tablestades[,2]+tablestades[,1])
	VIA2=as.vector(tablestades[,4]+tablestades[,3]+tablestades[,2]+tablestades[,1])
	VIA3=as.vector(tablestades[,5]+tablestades[,4]+tablestades[,3]+tablestades[,2]+tablestades[,1])
	stadescum=cbind(VIA0,VIA1,VIA2,VIA3)
	
	if (retrocalcul & graphstades) {
		vec<-c(rep(2,3),rep(1,2))
		mat <- matrix(vec,length(vec),1)
	}  else { # un seul graphe
		mat <- matrix(1,1,1)
	}   # on ne trace pas le graphe suivant
	layout(mat)
	##############################################################################################	
	if (retrocalcul){
		# on verifie les donnees environnementales
		na.fail(phi) #Pas de donnees manquantes
		# si les vecteur phi ne va pas jusqu'au dernier stade, on tronque le tableau 
		# et on envoie un warning
		strdates=strftime(as.POSIXlt(dates),"%Y-%m-%d")
		curv=list()
		phist=fundist(Vparm,phicum=seq(0,10, length.out=1000),graph=FALSE)
		
# creation d'une matrice (tps) ayant en ligne chaque donnee de stade et
# en colonne phist (temps pigmentaires moyens standard des distributions pour chaque stade)
# et en derniere colonne la moyenne ponderee par les stades i.e; le temps pigmentaire moyen de
# l'echantillon 
		tps =c(phist,"moy"=sum(tablestades[1,]*phist))
		for (j in 2:nrow(tablestades)){
			tps=rbind(tps, c(unlist(phist),
							sum(tablestades[j,]*phist)))
		}
		dimnames(tps)=list(strdates,c("VB","VIA0",
						"VIA1","VIA2","VIA3","moy"))
		cherchenuls=tablestades   
		cherchenuls[cherchenuls>0]=1
		cherchenuls=cbind(cherchenuls,rep(1,length(dates)))
		# cherchenul cherche les stades nuls (sans valeur) pour ne pas renvoyer de warning
		# correpondant � un depassement du temps pigmentaire :
		# ex : les temps physiologiques remontent un mois avant, les stades VIA3
		# auraient necessite 1 mois et demi mais ils sont absents du jeu de donnees   
		
		#vecteur des temps pigmentaires moyens de chaque stade
		for (j in 1:nrow(tablestades)){
			# On part de la date observee 1:match(rownames(tps)[j],phi$dates)])
			# et on calcule le cumul du vecteur inverse
			phicum=cumsum(rev(phi$phi_jour[1:match(strdates[j],phi$dates)]))
			phicum=phicum[phicum<maxVIA3]
			# pour des raisons graphiques, je m'arrete � un temps pigmentaire de maxVIA3
			# au del� on sait que c'est 100% de VIA3
			# il faudrait avoir modelise jusqu'au stade VIA4
			# on va chercher la date correspondante
			phidates=rev(phi$dates[1:match(strdates[j],phi$dates)])[1:length(phicum)]
			# structures des stades en x et y calcules � partir de la fonction gamma
			# x = les phicum (croissant en remontant dans le temps
			# y = la distribution dist/max(dist) entre zero et 1
			#
			curv[[strdates[j]]]$VB =fnstade(par1=Vparm$pigment_stage[[1]],VB=TRUE,phicum=phicum,phidates=phidates,neg=TRUE,lmax=lmax)
			curv[[strdates[j]]]$VB$y=tablestades[j,1]*curv[[strdates[j]]]$VB$y
			curv[[strdates[j]]]$VIA0= fnstade(par1=Vparm$pigment_stage[[1]],
					par2=Vparm$pigment_stage[[2]],VB=FALSE,phicum=phicum,phidates=phidates,neg=TRUE,lmax=lmax)
			curv[[strdates[j]]]$VIA0$y=tablestades[j,2]*curv[[strdates[j]]]$VIA0$y
			curv[[strdates[j]]]$VIA1= fnstade(par1=Vparm$pigment_stage[[2]],
					par2=Vparm$pigment_stage[[3]],VB=FALSE,phicum=phicum,phidates=phidates,neg=TRUE,lmax=lmax)
			curv[[strdates[j]]]$VIA1$y=tablestades[j,3]*curv[[strdates[j]]]$VIA1$y
			curv[[strdates[j]]]$VIA2= fnstade(par1=Vparm$pigment_stage[[3]],
					par2=Vparm$pigment_stage[[4]],VB=FALSE,phicum=phicum,phidates=phidates,neg=TRUE,lmax=lmax)
			curv[[strdates[j]]]$VIA2$y=tablestades[j,4]*curv[[strdates[j]]]$VIA2$y
			curv[[strdates[j]]]$VIA3= fnstade(par1=Vparm$pigment_stage[[4]],VB=FALSE,phicum=phicum,
					phidates=phidates,neg=TRUE,lmax=lmax)
			curv[[strdates[j]]]$VIA3$y=tablestades[j,5]*curv[[strdates[j]]]$VIA3$y
			# tps qui etait un fichier de temps pigmentaires est ici remplace par un fichier de dates
			for (k in 1:6){
				# dans le cas normal premier element superieur � tps, dates correcpondante remplace tps
				if (sum(phicum>tps[j,k])>0) {
					tps[j,k]=phidates[phicum>tps[j,k]][1]
				} else {
					# sinon on remplace par le premier
					tps[j,k]=phidates[length(phidates)] 
					if (cherchenuls[j,k]!=0 ){
						# teste si il a des stades dans la case selectionnee
						warning(paste(strdates[j],colnames(tps)[k],"le tableau des temperatures et salinites",
										"ne remonte pas assez loin dans le temps","\n"))
					}
				}
			}
		}
		# limite inf = arrivee la plus precoce -30 jours, max = dernier echantillon
		# pour l'instant dates renseignees dans phi
		xlim=c(min( strptime(rownames(tps), format="%Y-%m-%d"))-5184000,max(dates)) 
		newdates=seq(from=xlim[1],to=xlim[2],by="day")
		
		if (graphstades) { 
			par("mar"=c(2, 4, 0, 2)+ 0.1)
# si l'autre graphique n'est pas trace on etend les marges
			main1=""
		} else {
			main1=labelretro
		}    
		
		plot(x=newdates,
				y= seq(from=0,to=nrow(tablestades)+1,length.out=length(newdates)),
				type= "n",
				xlab="",
				xaxt="n",
				yaxt="n",
				ylab="echantillons",
				# bty="n",
				cex=1,
				main=main1)
		axis( 1,labels=strftime(as.POSIXlt(dates),"%d-%m-%y"),
				at=as.numeric(as.POSIXct(dates)),tick=FALSE )
		
		for (j in 1:nrow(tablestades)){
			curvsum=curv[[strdates[j]]][[1]]$y+
					curv[[strdates[j]]][[2]]$y+
					curv[[strdates[j]]][[3]]$y+
					curv[[strdates[j]]][[4]]$y+
					curv[[strdates[j]]][[5]]$y
			for (k in 5:1){
				polygon(x=curv[[strdates[j]]][[k]]$x,
						y=nrow(tablestades)-j+1+curvsum,
						col=gray(5:1/6)[k],
						lty=1,
						lwd=1,
						border=NA)
				curvsum=curvsum-curv[[strdates[j]]][[k]]$y
			}
			segments(    x0=as.numeric(as.POSIXct(trunc.POSIXt(dates[j],digits="day"))),
					y0=nrow(tablestades)+1,
					x1=as.numeric(as.POSIXct(trunc.POSIXt(dates[j],digits="day"))),
					y1=nrow(tablestades)-j+1,
					lty=3,
					col="gray")
			# temps moyens
			points      (y=nrow(tablestades)-j+1,
					x=as.numeric(as.POSIXct(strptime(tps[j,"moy"],format="%Y-%m-%d"))),
					col="red",
					pch=18,
					cex=0.8)
		}
	} else {
		xlim=range(dates)
	}
	if (graphstades)  {
		# stades cumules calcul necessaire pour points et graphique durees
		# le graphique ne supporte pas plusieurs echantillons a la même date d'ou le choix
		
		par("mar"=c(2, 4, 3, 2)+ 0.1)
		surface(dates,tablestades,couleur=gray(5:1/6),ordre=c(1,2,3,4,5),
				axe=TRUE,
				xaxt="n",
				ylab="% par stade",
				xlab="date",
				xlim=as.numeric(as.POSIXct(xlim)),
				main= labelgraphstades)
		legend( x=as.numeric(as.POSIXct(xlim[1]))+(as.numeric(as.POSIXct(xlim[2]))-
							as.numeric(as.POSIXct(xlim[1])))/80,
				y=0.7,legend=c("VIA3","VIA2","VIA1","VIA0","VB"),
				fill=gray(1:5/6),
				bg="white",
				bty="0"
		)
		axis(2)
		at.axis=seq(from=xlim[1],to=xlim[2],by="2 weeks")
		axis( 1,labels=strftime(as.POSIXlt(at.axis),"%d-%b"),
				at=as.numeric(at.axis),tick=FALSE )
		
		if (points) {
			Stagecum=cbind(VB,VIA0,VIA1,VIA2,VIA3)
			colorstage=c("steelblue2","blue","limegreen","orange","red")
			for (u in 5:1){
				points(dates,Stagecum[,u],
						type=("p"),
						pch=17,
						col=colorstage[u])
			}
			legend( x=as.numeric(as.POSIXct(xlim[1])),y=0.7,legend=c("","","","",""),
					pch=17,col=colorstage[5:1], bty="n"  )
		}
		if (nb) {
			text(x=dates,y=rep(-0.02,length(dates)),labels=effectifs)
		}
	}
	if (retrocalcul){
		return(as.POSIXct(strptime(tps[,6],format="%Y-%m-%d")))
	}
}


#' gglot function to draw graps of pigment stages
fungraphgg=function(h,...){
	g<-ggplot(bilan_stades_pigm@data) # recupere le data.frame vue_ope_lot qui a ete ecrit apres avoir
	g<-g+geom_bar(aes(x="",y=lot_effectif,fill=val_libelle,width=1),stat='identity')+  # cette ecriture de geom_bar demande de bien mettre stat='identity', on peut alors passer � geom_bar un x et un y...
			coord_polar(theta="y", start=pi)+ # coordonnees polaires = cercle
			scale_fill_grey(name="stades pigmentaires",start=0.8, end=0.2)+ # scale_fill_grey permet d'avoir des graduations de gris
			theme_bw() +  # on efface le fond gris
			facet_wrap(~ope_date_debut,scales="free_y")+ # facet_wrap permet d'afficher un ruban unidimentionnel en deux dimensions (un graphique par date)
			ggtitle("Stades pigmentaires",labels=c(x="",y="effectifs")) # options
	print(g)
	assign("g",g,"envir_stacomi")
	funout("l'object graphique est disponible dans l'environnement principal, tapper g<-get('g',envir=envir_stacomi)")
}








#' Handler function which retrun the graphics titles
#' date
#' 
#' 
#' 
#' @param h A handler
#' @param ... Other arguments passed to the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funtitle_bilan_stades_pigm=function(h,...){
	wintitle=gwindow()
	hgettext=function(h,...){
		if(bilan_stades_pigm@labelgraphstades!=svalue(titre2)|bilan_stades_pigm@labelretro!=svalue(titre4)){
			bilan_stades_pigm@labelgraphstades<-gsub("\n","",svalue(titre2))
			bilan_stades_pigm@labelretro<-gsub("\n","",svalue(titre4))
			assign("bilan_stades_pigm",bilan_stades_pigm,envir=.GlobalEnv)		
			funout("modification du titre \n")
		}
		dispose(wintitle)
	}
	group1<-ggroup(horizontal=FALSE,container=wintitle)
	titre1 <- glabel( text= "Titre du graphique de stades pigmentaires (graphstades = TRUE)", editable=FALSE,container=group1)
	titre2 <- gtext( text= bilan_stades_pigm@labelgraphstades,font.attr= c(foreground.colors="blueblue"),height=40,container=group1)  
	titre3 <- glabel( text= "Titre du graphique de retrocalcul quand il est seul (graphstades = FALSE)", editable=FALSE,container=group1) 
	titre4 <- gtext(  text= bilan_stades_pigm@labelretro, editable=TRUE,height=40,container=group1) 
	
	aOK=gaction(label="OK",icon="gtk-ok",handler=hgettext)         
	aQuit=gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=function(h,...) dispose(wintitle))
	toolbarlist <- list(
			OK=aOK, 
			Quit = aQuit)
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	add(group1,ggroupboutonsbas)
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
}

#' An interface that calls the object to build the user interface
#' 
#' 
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_Bilan_stades_pigm = function()
{  
	bilan_stades_pigm=new("Bilan_stades_pigm")
	assign("bilan_stades_pigm",bilan_stades_pigm,envir = .GlobalEnv)
	funout(get("msg",envir=envir_stacomi)$Bilan_stades_pigm.2)
	bilan_stades_pigm@dc=charge(bilan_stades_pigm@dc)
	bilan_stades_pigm@stationMesure=charge(bilan_stades_pigm@stationMesure)
	bilan_stades_pigm@lmax<-charge(bilan_stades_pigm@lmax,vecteur=c("0.6","0.8","1","1.2"),label="choix de la largeur des distributions",selected=as.integer(2))
	bilan_stades_pigm@options<-charge(bilan_stades_pigm@options,title="options du graphique",labels=c("graphstades","retrocalcul","points","nb"),checked=c(TRUE,FALSE,TRUE,TRUE))
	bilan_stades_pigm@salinite<-charge(bilan_stades_pigm@salinite,title= "Valeur de la salinite moyenne, cliquer pour editer",label="15")
	
	quitte() # vidange de l'interface
	group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)
	add(ggroupboutons,group)
	gl=glabel(text=get("msg",envir=envir_stacomi)$Bilan_stades_pigm.3,container=group)
	choix(bilan_stades_pigm@lmax)
	choix(bilan_stades_pigm@options)
	# on assigne directement (sans forcement changer les options...)
	assign("refCheckBox",bilan_stades_pigm@options,envir_stacomi) 
	choix(bilan_stades_pigm@salinite)
	# on assigne directement (sans forcement changer les options...)
	assign("refTextBox",bilan_stades_pigm@salinite,envir_stacomi)
	choix(bilan_stades_pigm@stationMesure,title="choix de la temperature")
	choix(bilan_stades_pigm@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilan_stades_pigm_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
	choix(bilan_stades_pigm@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilan_stades_pigm_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
	choix(bilan_stades_pigm@dc,objectBilan=bilan_stades_pigm,is.enabled=TRUE)
	#getStockIcons(toolkit=guiToolkit())
	aCalcul=gaction(label="calcul",icon="gtk-execute",handler=funcalcbilan_stades_pigm,tooltip="Chargement des donnees")         
	aSetTitle=gaction(label="title",icon="rename",handler=funtitle_bilan_stades_pigm,tooltip=get("msg",envir=envir_stacomi)$Bilan_stades_pigm.6)
	aGraph=gaction(label="graph",icon="gWidgetsRGtk2-contour",handler=hfungraphstades,tooltip="Graphique Principal")
	aGraphgg=gaction(label="graphgg",icon="gWidgetsRGtk2-bubbles",handler=fungraphgg,tooltip="Graphique supplementaire avec ggplot")
	aQuit=gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9)
	
	toolbarlist <- list(
			Calcul=aCalcul, 
			SetTitle= aSetTitle,
			Graph=aGraph,
			Graphgg=aGraphgg,
			Quit = aQuit)
	assign("toolbarlist",toolbarlist,.GlobalEnv)
	enabled(toolbarlist[["SetTitle"]])<-FALSE
	enabled(toolbarlist[["Graph"]])<-FALSE
	enabled(toolbarlist[["Graphgg"]])<-FALSE
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	add(ggroupboutons,ggroupboutonsbas)
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)	
	addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal       
	#assign("graphes",graphes,envir=.GlobalEnv) 	
	x11()
	# A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
}


