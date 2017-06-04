#' interface for BilanMigrationPar class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationPar = function()
{
	quitte()
	
	bilanMigrationCar=new("BilanMigrationCar")
	assign("bilanMigrationCar",bilanMigrationCar,envir=envir_stacomi)
	
	bilanFonctionnementDC=new("BilanFonctionnementDC") # appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
	assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
	
	objectBilan="BilanMigrationCar"
	# the following name is created by the interface
	# as I can't get the name from within the function (deparse(substitute(objectBilan)) does not return
	# "bilanMigrationMult" see refDC choice_c method)
	# so this will allow to assign "bilanMigrationMult" in envir_stacomi while using other class
	# like refDC
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	funout(gettext("Loading of the lists for taxons, stages, counting devices, qualitative and quantitative parameters\n",domain="R-stacomiR"))
	bilanMigrationCar@taxons=charge(bilanMigrationCar@taxons)
	bilanMigrationCar@stades=charge(bilanMigrationCar@stades)
	bilanMigrationCar@dc=charge(bilanMigrationCar@dc)
	bilanMigrationCar@parquan=charge(bilanMigrationCar@parquan)
	bilanMigrationCar@parqual=charge(bilanMigrationCar@parqual)

	bilanMigrationCar@echantillon=charge(bilanMigrationCar@echantillon,vecteur=gettext("with","without",domain="R-stacomiR"),
			label=gettext("Choice of batch type, inclusion of samples ?",domain="R-stacomiR"), 
					selected=as.integer(1))
	#######################
	# Interface Graphique 
	##########################
	group <- gWidgets::ggroup(horizontal=TRUE)   # doit toujours s'appeller group	
	assign("group",group,envir = .GlobalEnv)
	notebook <- gnotebook(container=group)	
	assign("notebook",notebook,envir=.GlobalEnv)
	size(notebook)<-c(400,400)
		

	choicemult(bilanMigrationCar@horodatedebut,label=gettext("from",domain="R-stacomiR"),decal=-1)
	choicemult(bilanMigrationCar@horodatefin,label=gettext("to",domain="R-stacomiR"),decal=0)
	choicemult(bilanMigrationCar@echantillon)
	choicemult(bilanMigrationCar@dc,objectBilan=bilanMigrationCar,is.enabled=TRUE)
# FIXME Error in .local(object, ...) : 
#  unused arguments (label = "Qualitative feature", frameassign = "frame_parqual") verify
	svalue(notebook)<-1	
	gWidgets::add(ggroupboutons,group)
	# ggroupboutons is attached to the original frame
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler = hbmCcalc,
					icon = "new",
					label=gettext("calculation"),
					action=bilanMigrationCar,
					tooltip=gettext("calculation",domain="R-stacomiR")),
			Graph=gWidgets::gaction(handler = hbmCplotquan,
					icon = "graph",
					label="gr qual",
					tooltip=gettext("Plot for qualitative parm",domain="R-stacomiR")),
			Graph2=gWidgets::gaction(handler = hbmCplotqual,
					icon = "graph2",
					label="gr quan",
					tooltip=gettext("plot for quantitative parm",domain="R-stacomiR")),
			Graph3=gWidgets::gaction(handler = hbmCplotcrossed,
					icon = "graph2",
					label="gr crossed",
					tooltip=gettext("Crossed graph for qualitative and quantitative parameter",domain="R-stacomiR")),
			Stat =gWidgets::gaction(handler= hbmCstat,
					icon = "matrix",
					label="stat",
					tooltip=gettext("Summary",domain="R-stacomiR")),
			annuler=gWidgets::gaction(handler= quitte,
					icon = "close",
					label=gettext("Exit",domain="R-stacomiR")))
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
}