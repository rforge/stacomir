#' Migration report along with quantitative and
#' qualitative characteristics
#' 
#' Migration along with qualitative or quantitative characteristics or both
#' (e.g.) weight of eels according to the size class per period of time, weight
#' of fish according to gender. This class does not split migration evenly over 
#' time period. So, unlike calculations made in class BilanMigration and BilanMigrationMult
#' the whole time span of the migration operation is not considered, only  the date of beginning of 
#' the operation is used to perform calculation. 
#' 
#' @include Refparquan.r
#' @include Refparqual.r
#' @include RefChoix.r
#' @note The program by default uses two parameter choice, checking box "aucun" will
#' allow the program to ignore the parameter
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationPar", ...)}.  they are loaded by the interface
#' using interface_BilanMigrationPar function.
#' @slot parquan An object of class \link{Refparquan-class}, quantitative parameter 
#' @slot parqual An object of class \link{Refparqual-class}, quanlitative parameter
#' @slot echantillon An object of class \link{RefChoix-class}, vector of choice
#' @slot valeurs_possibles A \code{data.frame} choice among possible choice of a qualitative parameter (discrete)
#' @slot dc an object of class \link{RefDC-class} inherited from \link{BilanMigration-class}
#' @slot taxons An object of class \link{RefTaxon-class} inherited from \link{BilanMigration-class}
#' @slot stades An object of class \link{RefStades-class} inherited from \link{BilanMigration-class}
#' @slot pasDeTemps An object of class \link{PasDeTempsJournalier-class} inherited from \link{BilanMigration-class}
#' @slot data A \code{data.frame} inherited from \link{BilanMigration-class}, stores the results
#' @slot time.sequence An object of class "POSIXct" inherited from \link{BilanMigration-class}
#' @note program : default two parameter choice, checking box "aucun" will allow the program to ignore the parameter
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class 
#' \code{\linkS4class{Bilan_carlot}}, 
#' \code{\linkS4class{Bilan_poids_moyen}}, 
#' \code{\linkS4class{Bilan_stades_pigm}}, 
#' \code{\linkS4class{Bilan_taille}}, 
#' \code{\linkS4class{BilanConditionEnv}}, 
#' \code{\linkS4class{BilanEspeces}}, 
#' \code{\linkS4class{BilanFonctionnementDC}}, 
#' \code{\linkS4class{BilanFonctionnementDF}}, 
#' \code{\linkS4class{BilanMigration}}, 
#' \code{\linkS4class{BilanMigrationConditionEnv}}, 
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}, 
#' \code{\linkS4class{BilanMigrationPar}}
#' @concept Bilan Object 
#' @keywords classes
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
#' @param h handler
#' @param ... Additional parameters
hbilanMigrationParcalc=function(h,...){
	calcule(h$action)
}

#' calcule methode
#' 
#' 
#'@param object An object of class \code{\link{BilanMigrationPar-class}} 
setMethod("calcule",signature=signature("BilanMigrationPar"),definition=function(object){ 
			bilanMigrationPar<-object  
			if (exists("refDC",envir_stacomi)) {
				bilanMigrationPar@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n"),arret=TRUE)
			}
			if (exists("refTaxon",envir_stacomi)) {
				bilanMigrationPar@taxons<-get("refTaxon",envir_stacomi)
			} else {      
				funout(gettext("You need to choose a taxa, clic on validate\n"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilanMigrationPar@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a stage, clic on validate\n"),arret=TRUE)
			}
			if (exists("pasDeTemps",envir_stacomi)){
				bilanMigrationPar@pasDeTemps<-get("pasDeTemps",envir_stacomi)
				# pour permettre le fonctionnement de Fonctionnement DC
				assign("bilanFonctionnementDC_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
				assign("bilanFonctionnementDC_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			} else {
				funout(gettext("Attention, no time step selected, compunting with default value\n"),arret=FALSE)
				warning("Attention, no time step selected, compunting with default value\n")
			}
			if (exists("refchoice",envir_stacomi)){
				bilanMigrationPar@echantillon<-get("refchoice",envir_stacomi)
			} else 
			{
				bilanMigrationPar@echantillon@listechoice<-"avec"
				bilanMigrationPar@echantillon@selected<-as.integer(1)
			}
			if (exists("refparquan",envir_stacomi)){
				bilanMigrationPar@parquan<-get("refparquan",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a quantitative parameter\n"),arret=TRUE)
			}
			if (exists("refparqual",envir_stacomi)){
				bilanMigrationPar@parqual<-get("refparqual",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a qualitative parameter\n"),arret=TRUE)
			}
			
			stopifnot(validObject(bilanMigrationPar, test=TRUE))
			funout(gettext("Attention, no time step selected, compunting with default value\n"))
			if (bilanMigrationPar@parquan@data$par_nom=="aucune" & bilanMigrationPar@parqual@data$par_nom=="aucune") {
				funout(gettext("You need to choose at least one quantitative or qualitative attribute\n"),arret=TRUE)}
			res<-funSousListeBilanMigrationPar(bilanMigrationPar=bilanMigrationPar)
			if (exists("progres")) close(progres)
			data<-res[[1]]
			data[,"debut_pas"]<-as.POSIXct(strptime(x=data[,"debut_pas"],format="%Y-%m-%d"))   # je repasse de caractere 
			data[,"fin_pas"]<-as.POSIXct(strptime(data[,"fin_pas"],format="%Y-%m-%d"))
			bilanMigrationPar@valeurs_possibles<-res[[2]]   # definitions des niveaux de parametres qualitatifs rencontres.
			# funout("\n")
			#	assign("data",data,envir_stacomi)
			#funout(gettext("the migration summary table is stored in envir_stacomi\n"))
			#data<-get("data",envir_stacomi)
			# chargement des donnees suivant le format chargement_donnees1  
			bilanMigrationPar@time.sequence=seq.POSIXt(from=min(data$debut_pas),to=max(data$debut_pas),by=as.numeric(bilanMigrationPar@pasDeTemps@stepDuration)) # il peut y avoir des lignes repetees poids effectif
			
			if (bilanMigrationPar@taxons@data$tax_nom_commun=="Anguilla anguilla"& bilanMigrationPar@stades@data$std_libelle=="civelle") 
			{
				funout(gettext("\"Be careful, the processing doesnt take lot\"s quantities into account\"\n"))
			}
			funout(gettext("\"Writing data into envir_stacomi environment : write data=get(data\",envir_stacomi)\"\n"))
			bilanMigrationPar@data<-data 
			assign("bilanMigrationPar",bilanMigrationPar,envir_stacomi)
			assign("data",data,envir_stacomi)
			# graphiques (a affiner pb si autre chose que journalier)
			# pour sauvegarder sous excel
		})
#' le handler appelle la methode generique graphe sur l'object plot.type=1
#' 
#' @param h handler
#' @param ... Additional parameters
hbilanMigrationPargraph = function(h,...) {
	if (exists("bilanMigrationPar",envir_stacomi)) {
		bilanMigrationPar<-get("bilanMigrationPar",envir_stacomi)
		plot(bilanMigrationPar,plot.type="barplot")
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n"),arret=TRUE)
	}
}
#' le handler appelle la methode generique graphe sur l'object plot.type=2
#' 
#' @param h handler
#' @param ... Additional parameters
hbilanMigrationPargraph2=function(h,...){
	if (exists("bilanMigrationPar",envir_stacomi)) {
		bilanMigrationPar<-get("bilanMigrationPar",envir_stacomi)
		plot(bilanMigrationPar,plot.type="xyplot")
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n"),arret=TRUE)
	}
}
#' This handler calls the generic method graphe on object plot.type 3
#' 
#' 
#' @param h handler
#' @param ... Additional parameters
hbilanMigrationParstat=function(h){
	if (exists("bilanMigrationPar",envir_stacomi)) {
		bilanMigrationPar<-get("bilanMigrationPar",envir_stacomi)
		plot(bilanMigrationPar,plot.type="summary")
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",arret=TRUE)		)
	}
}

#' plot method for BilanMigrationPar
#' 
#' 
#' @param x An object of class BilanMigrationPar
#' @param y not used there
#' @param plot.type One of "barplot", "xyplot", "summary table
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("plot",signature=signature(x="BilanMigrationPar",y="ANY"),definition=function(x,y,plot.type="barplot",...){ 
			###########################
			bilanMigrationPar<-x # ne pas passer dessus en debug manuel
			##########################
			colnames(bilanMigrationPar@data)<-gsub("debut_pas","Date",colnames(bilanMigrationPar@data))
			if (bilanMigrationPar@parqual@data$par_nom!="aucune"& bilanMigrationPar@parquan@data$par_nom!="aucune") {# il y a des qualites et des quantites de lots
				nmvarqan=gsub(" ","_",bilanMigrationPar@parquan@data$par_nom) # nom variable quantitative
				colnames(bilanMigrationPar@data)<-gsub("quantite",nmvarqan,colnames(bilanMigrationPar@data))
				mb=reshape2::melt(bilanMigrationPar@data,id.vars=c(1:4),measure.vars=grep(nmvarqan,colnames(bilanMigrationPar@data)))
				# ici je ne sors que les variables quantitatives pour les graphes ulterieurs (j'ignore les effectifs) 
			} else if (bilanMigrationPar@parqual@data$par_nom!="aucune"){ # c'est que des caracteristiques qualitatives
				mb=reshape2::melt(bilanMigrationPar@data,id.vars=c(1:4),measure.vars=grep("effectif",colnames(bilanMigrationPar@data)))  # effectifs en fonction des variables qualitatives, il n'y a qu'une seule colonne     
			} else if (bilanMigrationPar@parquan@data$par_nom!="aucune"){ # c'est que des caracteristiques quantitatives
				nmvarqan=gsub(" ","_",bilanMigrationPar@parquan@data$par_nom) # nom variable quantitative
				colnames(bilanMigrationPar@data)<-gsub("quantite",nmvarqan,colnames(bilanMigrationPar@data)) # je renomme la variable quant
				mb=reshape2::melt(bilanMigrationPar@data,id.vars=c(1:4),measure.vars=grep(nmvarqan,colnames(bilanMigrationPar@data))) # valeurs quantitatives (il n'y a qu'une) 
			} else if (bilanMigrationPar@parquan@data$par_nom=="aucune"&bilanMigrationPar@parqual@data$par_nom=="aucune"){
				stop("normalement ce cas est impossible")
				# ce cas est impossible
			}
			mb=stacomirtools::chnames(mb,"value","sum")
			mb=funtraitementdate(data=mb,nom_coldt="Date") 
			# transformation du tableau de donnees
			
			if (plot.type=="barplot") {
				
				g<-ggplot(mb)
				g<-g+geom_bar(aes(x=mois,y=sum,fill=variable),stat='identity',
						stack=TRUE)
				assign("g",g,envir_stacomi)
				funout(gettext("\"Writing the graphical object into envir_stacomi environment : write g=get(g\",envir_stacomi)\"\n"))
				print(g)
			} #end plot.type = "barplot"
			if (plot.type=="xyplot") { 
				
				g<-ggplot(mb)
				g<-g+geom_point(aes(x=Date,y=sum,col=variable),stat='identity',stack=TRUE)
				assign("g",g,envir_stacomi)
				funout(gettext("\"Writing the graphical object into envir_stacomi environment : write g=get(g\",envir_stacomi)\"\n"))
				print(g)
			} #end plot.type="xyplot"
			#TODO create summary method
			if (plot.type=="summary") {
				table=round(tapply(mb$sum,list(mb$mois,mb$variable),sum),1)
				table=as.data.frame(table)
				table[,"total"]<-rowSums(table)
				gdf(table, container=TRUE)
				nomdc=bilanMigrationPar@dc@data$df_code[match(bilanMigrationPar@dc@dc_selectionne,bilanMigrationPar@dc@data$dc)]
				annee=unique(strftime(as.POSIXlt(bilanMigrationPar@time.sequence),"%Y"))
				path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste(nmvarqan,"_mensuel_",nomdc,"_",bilanMigrationPar@taxons@data$tax_nom_commun,"_",bilanMigrationPar@stades@data$std_libelle,"_",annee,".csv",sep=""),fsep ="\\")
				write.table(table,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
				funout(gettextf("Writing of %s",path1))
				path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste(nmvarqan,"_journalier_",nomdc,"_",bilanMigrationPar@taxons@data$tax_nom_commun,"_",bilanMigrationPar@stades@data$std_libelle,"_",annee,".csv",sep=""),fsep ="\\")
				write.table(bilanMigrationPar@data,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
				funout(gettextf("Writing of %s",path1))
			} # end plot.type summary 
		})





