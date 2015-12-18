# Nom fichier :        BilanMigrationCar    (classe)
# Date de creation :   31/03/2008 17:21:18

#' @title Class BilanMigrationPar, Migration report along with quantitative and qualitative characteristics
#' Migration along with qualitative or quantitative characteristics 
#'or both  (e.g.) weight of eels according to the size class per period of time,
#'weight of fish according to gender...
#' @slot parquan = "Refparquan"
#' @slot parqual = "Refparqual"
#' @slot echantillon = "RefChoix"
#' @slot valeurs_possibles : data.frame valeurs possibles d'un param�tre qualitatif (discret)
#' @slot dc="RefDC" inherited from BilanMigration
#' @slot taxons="RefTaxon" inherited from BilanMigration
#' @slot stades="RefStades" inherited from BilanMigration
#' @slot pasDeTemps="PasDeTempsJournalier" inherited from BilanMigration
#' @slot data="data.frame" inherited from BilanMigration
#' @slot duree="POSIXct" inherited from BilanMigration
#' @method calcule
#' @method graphe
#' @note program : default two parameter choice, checking box "aucun" will allow the program to ignore the parameter
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
setClass(Class="BilanMigrationPar",
		representation=representation(parquan="Refparquan",
				parqual="Refparqual",
				echantillon="RefChoix",
				valeurs_possibles="data.frame"),
		prototype=prototype(parquan=new("Refparquan"),
				parqual=new("Refparqual"),
				echantillon=new("RefChoix"),
				valeurs_possibles=data.frame()),
		contains="BilanMigration")
#object=bilanMigrationPar

setValidity("BilanMigrationPar",function(object)
		{
			rep1=length(object@dc)==1
			if (!rep1) retValue="length(object@dc) different de 1, plusieurs dc alors que la classe n'en comporte qu'un"  
			rep2=length(object@taxons)==1
			if (!rep2) retValue="length(object@taxons) different de 1, plusieurs taxons alors que la classe n'en comporte qu'un" 
			rep3=length(object@stades)==1
			if (!rep3) retValue="length(object@stades) different de 1, plusieurs stades alors que la classe n'en comporte qu'un" 
			rep4=length(object@pasDeTemps)==1
			if (!rep4) retValue="length(object@pasDeTemps) different de 1, plusieurs stades alors que la classe n'en comporte qu'un" 
			rep5=length(object@parqual)==1|length(object@parquan)==1 #au moins un qualitatif ou un quantitatif
			if (!rep5) retValue="length(object@parqual)==1|length(object@parquan)==1 non respecte"  
			return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5,TRUE,retValue))
		}   )

#' handler for bilanmigrationpar
hbilanMigrationParcalc=function(h,...){
	calcule(h$action)
}

#' methode calcule
#'@param objet "BilanMigrationPar" passe par le handler
setMethod("calcule",signature=signature("BilanMigrationPar"),definition=function(objet,...){ 
			bilanMigrationPar<-objet  
			if (exists("refDC",envir_stacomi)) {
				bilanMigrationPar@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			}
			if (exists("refTaxons",envir_stacomi)) {
				bilanMigrationPar@taxons<-get("refTaxons",envir_stacomi)
			} else {      
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilanMigrationPar@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("pasDeTemps",envir_stacomi)){
				bilanMigrationPar@pasDeTemps<-get("pasDeTemps",envir_stacomi)
				# pour permettre le fonctionnement de Fonctionnement DC
				assign("fonctionnementDC_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
				assign("fonctionnementDC_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$BilanMigration.1,arret=FALSE)
				warning(get("msg",envir=envir_stacomi)$BilanMigration.1)
			}
			if (exists("refchoix",envir_stacomi)){
				bilanMigrationPar@echantillon<-get("refchoix",envir_stacomi)
			} else 
			{
				bilanMigrationPar@echantillon@listechoix<-"avec"
				bilanMigrationPar@echantillon@selected<-as.integer(1)
			}
			if (exists("refparquan",envir_stacomi)){
				bilanMigrationPar@parquan<-get("refparquan",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.7,arret=TRUE)
			}
			if (exists("refparqual",envir_stacomi)){
				bilanMigrationPar@parqual<-get("refparqual",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.8,arret=TRUE)
			}
			
			stopifnot(validObject(bilanMigrationPar, test=TRUE))
			funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.1)
			# save(bilanMigrationPar,file="EXAMPLES/devt.Rdata")
			#undebug(funSousListeBilanMigrationPar)
			# debug(funSousListeBilanMigrationPar)
			if (bilanMigrationPar@parquan@data$par_nom=="aucune" & bilanMigrationPar@parqual@data$par_nom=="aucune") {
				funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.2,arret=TRUE)}
			res<-funSousListeBilanMigrationPar(bilanMigrationPar=bilanMigrationPar)
			if (exists("progres")) close(progres)
			data<-res[[1]]
			data[,"Debut_pas"]<-as.POSIXct(strptime(x=data[,"Debut_pas"],format="%Y-%m-%d"))   # je repasse de caractere 
			data[,"Fin_pas"]<-as.POSIXct(strptime(data[,"Fin_pas"],format="%Y-%m-%d"))
			bilanMigrationPar@valeurs_possibles<-res[[2]]   # definitions des niveaux de parametres qualitatifs rencontres.
			# funout("\n")
			#	assign("data",data,envir_stacomi)
			#funout("la table bilan migration est stockee dans l'environnement envir_stacomi\n")
			#data<-get("data",envir_stacomi)
			# chargement des donnees suivant le format chargement_donnees1  
			bilanMigrationPar@duree=seq.POSIXt(from=min(data$Debut_pas),to=max(data$Debut_pas),by=as.numeric(bilanMigrationPar@pasDeTemps@dureePas)) # il peut y avoir des lignes repetees poids effectif
			
			if (bilanMigrationPar@taxons@data$tax_nom_commun=="Anguilla anguilla"& bilanMigrationPar@stades@data$std_libelle=="civelle") 
			{
				funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.3)
			}
			funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.4)
			bilanMigrationPar@data<-data 
			assign("bilanMigrationPar",bilanMigrationPar,envir_stacomi)
			assign("data",data,envir_stacomi)
			# graphiques (a affiner pb si autre chose que journalier)
			# pour sauvegarder sous excel
		})
#' le handler appelle la methode generique graphe sur l'objet choix=1
#' @param h, pass� par le handler
hbilanMigrationPargraph = function(h,...) {
	if (exists("bilanMigrationPar",envir_stacomi)) {
		bilanMigrationPar<-get("bilanMigrationPar",envir_stacomi)
		graphe(bilanMigrationPar,choix=1)
	} else {      
		funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.5,arret=TRUE)
	}
}
#' le handler appelle la methode generique graphe sur l'objet choix=2
#' @param h, pass� par le handler
hbilanMigrationPargraph2=function(h,...){
	if (exists("bilanMigrationPar",envir_stacomi)) {
		bilanMigrationPar<-get("bilanMigrationPar",envir_stacomi)
		graphe(bilanMigrationPar,choix=2)
	} else {      
		funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.5,arret=TRUE)
	}
}
#' le handler appelle la methode generique graphe sur l'objet choix 3
#' @param h, pass� par le handler
hbilanMigrationParstat=function(h,...){
	if (exists("bilanMigrationPar",envir_stacomi)) {
		bilanMigrationPar<-get("bilanMigrationPar",envir_stacomi)
		graphe(bilanMigrationPar,choix=3)
	} else {      
		funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.5,arret=TRUE)		
	}
}

#' graphe method for BilanMigrationPar
#' @param choix
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
setMethod("graphe",signature=signature("BilanMigrationPar"),definition=function(objet,choix=1,...){ 
			###########################
			bilanMigrationPar<-objet # ne pas passer dessus en debug manuel
			##########################
			colnames(bilanMigrationPar@data)<-gsub("Debut_pas","Date",colnames(bilanMigrationPar@data))
			if (bilanMigrationPar@parqual@data$par_nom!="aucune"& bilanMigrationPar@parquan@data$par_nom!="aucune") {# il y a des qualites et des quantites de lots
				nmvarqan=gsub(" ","_",bilanMigrationPar@parquan@data$par_nom) # nom variable quantitative
				colnames(bilanMigrationPar@data)<-gsub("quantite",nmvarqan,colnames(bilanMigrationPar@data))
				mb=melt(bilanMigrationPar@data,id.vars=c(1:4),measure.vars=grep(nmvarqan,colnames(bilanMigrationPar@data)))
				# ici je ne sors que les variables quantitatives pour les graphes ulterieurs (j'ignore les effectifs) 
			} else if (bilanMigrationPar@parqual@data$par_nom!="aucune"){ # c'est que des caracteristiques qualitatives
				mb=melt(bilanMigrationPar@data,id.vars=c(1:4),measure.vars=grep("effectif",colnames(bilanMigrationPar@data)))  # effectifs en fonction des variables qualitatives, il n'y a qu'une seule colonne     
			} else if (bilanMigrationPar@parquan@data$par_nom!="aucune"){ # c'est que des caracteristiques quantitatives
				nmvarqan=gsub(" ","_",bilanMigrationPar@parquan@data$par_nom) # nom variable quantitative
				colnames(bilanMigrationPar@data)<-gsub("quantite",nmvarqan,colnames(bilanMigrationPar@data)) # je renomme la variable quant
				mb=melt(bilanMigrationPar@data,id.vars=c(1:4),measure.vars=grep(nmvarqan,colnames(bilanMigrationPar@data))) # valeurs quantitatives (il n'y a qu'une) 
			} else if (bilanMigrationPar@parquan@data$par_nom=="aucune"&bilanMigrationPar@parqual@data$par_nom=="aucune"){
				stop("normalement ce cas est impossible")
				# ce cas est impossible
			}
			mb=chnames(mb,"value","sommes")
			mb=funtraitementdate(data=mb,nom_coldt="Date") 
			# transformation du tableau de donnees
			
			if (choix==1) {
				
				g<-ggplot(mb)
				g<-g+geom_bar(aes(x=mois,y=sommes,fill=variable),stat='identity',stack=TRUE)
				assign("g",g,envir_stacomi)
				funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.6)
				print(g)
			} #end choix1
			if (choix==2) { 
				
				g<-ggplot(mb)
				g<-g+geom_point(aes(x=Date,y=sommes,col=variable),stat='identity',stack=TRUE)
				assign("g",g,envir_stacomi)
				funout(get("msg",envir=envir_stacomi)$BilanMigrationPar.6)
				print(g)
			} #end choix2
			if (choix==3) {
				table=round(tapply(mb$somme,list(mb$mois,mb$variable),sum),1)
				table=as.data.frame(table)
				table[,"total"]<-rowSums(table)
				gdf(table, container=TRUE)
				nomdc=bilanMigrationPar@dc@data$df_code[match(bilanMigrationPar@dc@dc_selectionne,bilanMigrationPar@dc@data$dc)]
				annee=unique(strftime(as.POSIXlt(bilanMigrationPar@duree),"%Y"))
				path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste(nmvarqan,"_mensuel_",nomdc,"_",bilanMigrationPar@taxons@data$tax_nom_commun,"_",bilanMigrationPar@stades@data$std_libelle,"_",annee,".csv",sep=""),fsep ="\\")
				write.table(table,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
				funout(paste(get("msg",envir=envir_stacomi)$BilanMigrationPar.7,path1,"\n")) 
				path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste(nmvarqan,"_journalier_",nomdc,"_",bilanMigrationPar@taxons@data$tax_nom_commun,"_",bilanMigrationPar@stades@data$std_libelle,"_",annee,".csv",sep=""),fsep ="\\")
				write.table(bilanMigrationPar@data,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
				funout(paste(get("msg",envir=envir_stacomi)$BilanMigrationPar.7,path1,"\n"))
			} # end choix3 
		})





