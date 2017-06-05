#' interface for BilanAnnuels class 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanAnnuels = function()
{
	quitte() # vidange de l'interface
	bilanAnnuels=new("BilanAnnuels")
	assign("bilanAnnuels",bilanAnnuels,envir=envir_stacomi)
	objectBilan="bilanAnnuels"
	# the following name is created by the interface
	# as I can't get the name from within the function (deparse(substitute(objectBilan)) does not return
	# "bilanMigrationMult" see refDC choice_c method)
	# so this will allow to assign "bilanMigrationMult" in envir_stacomi while using other class
	# like refDC
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	bilanAnnuels@dc=charge(bilanAnnuels@dc)
	bilanAnnuels@taxons=charge(bilanAnnuels@taxons)
	bilanAnnuels@stades=charge(bilanAnnuels@stades)
	bilanAnnuels@anneedebut=charge(bilanAnnuels@anneedebut,objectBilan="BilanAnnuels")
	bilanAnnuels@anneefin=charge(bilanAnnuels@anneefin,objectBilan="BilanAnnuels")


	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = envir_stacomi)  
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	add(ggroupboutons,group)
	notebook <- gnotebook(container=group)	
	assign("notebook",notebook,envir=envir_stacomi)
	size(notebook)<-c(400,300)
	# pour preselectionner une date on lui fournit l'indice de la date dans le RefAnnee. indice = 11 pour 2005
	
	choice(bilanAnnuels@anneedebut,
			nomassign="anneedebut",
			funoutlabel=gettext("The year of beginning has been chosen\n",domain="R-stacomiR"),
			titleFrame=gettext("First year",domain="R-stacomiR"),
			preselect=which(bilanAnnuels@anneedebut@data==min(bilanAnnuels@anneedebut@data)))
	choice(bilanAnnuels@anneefin,
			nomassign="anneefin",
			funoutlabel=gettext("The last year has been chosen\n",domain="R-stacomiR"),
			titleFrame=gettext("Last year",domain="R-stacomiR"),
			preselect=which(bilanAnnuels@anneefin@data==max(bilanAnnuels@anneefin@data)))
	choicemult(bilanAnnuels@dc,objectBilan=bilanAnnuels,is.enabled=TRUE)
	svalue(notebook)<-1
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	
	toolbarlist = list(
			aGraph=gWidgets::gaction(label="barplot",icon="barplot",handler=hbarplotBilanAnnuels,tooltip=gettext("Migration of all the years in the same graphic",domain="R-stacomiR")),
			aGraph2=gWidgets::gaction(label="plot",icon="plot",handler=hplotBilanAnnuels,tooltip=gettext("Migration of all the years in the same graphic",domain="R-stacomiR")),
		    aTable=gWidgets::gaction(handler=hxtableBilanAnnuels, icon="dataframe", label="xtable", tooltip="xtable"),  
			aQuit=gWidgets::gaction(label="fermer",icon="close", handler=quitte,tooltip=gettext("Exit",domain="R-stacomiR"))
	)
	
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)


}