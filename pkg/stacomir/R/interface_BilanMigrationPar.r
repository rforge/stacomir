#' interface for BilanMigrationPar class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationPar = function()
{
	quitte()
	
	bilanMigrationPar=new("BilanMigrationPar")
	assign("bilanMigrationPar",bilanMigrationPar,envir=envir_stacomi)
	
	bilanFonctionnementDC=new("BilanFonctionnementDC") # appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
	assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
	
	funout(gettext("Loading of the lists for taxons, stages, counting devices, qualitative and quantitative parameters\n",domain="R-stacomiR"))
	bilanMigrationPar@taxons=charge(bilanMigrationPar@taxons)
	bilanMigrationPar@stades=charge(bilanMigrationPar@stades)
	bilanMigrationPar@dc=charge(bilanMigrationPar@dc)
	bilanMigrationPar@parquan=charge(bilanMigrationPar@parquan)
	bilanMigrationPar@parqual=charge(bilanMigrationPar@parqual)
	#TODO transformer la valeur logique de échantillon en un refchoix correct (radiobutton)
	bilanMigrationPar@echantillon=charge(bilanMigrationPar@echantillon,vecteur=gettext("with","without",domain="R-stacomiR"),
			label=gettext("Choice of batch type, inclusion of samples ?",domain="R-stacomiR"), 
					selected=as.integer(1))
	#######################
	# Interface Graphique 
	##########################
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = .GlobalEnv)
	
	gWidgets::add(ggroupboutons,group)
	choice(bilanMigrationPar@pasDeTemps)
	choice(bilanMigrationPar@echantillon)
	choice(bilanMigrationPar@dc,objectBilan=bilanMigrationPar,is.enabled=TRUE)
	
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler = hbilanMigrationParcalc,
					icon = "new",
					label=gettext("calculation"),
					action=bilanMigrationPar,
					tooltip=gettext("Calculation of numbers by time step",domain="R-stacomiR")),
			Graph=gWidgets::gaction(handler = hbilanMigrationPargraph,
					icon = "graph",
					label="graph",
					tooltip=gettext("Monthly graphic",domain="R-stacomiR")),
			Graph2=gWidgets::gaction(handler = hbilanMigrationPargraph2,
					icon = "graph2",
					label="grjour",
					tooltip=gettext("Daily graphic",domain="R-stacomiR")),
			Stat =gWidgets::gaction(handler= hbilanMigrationParstat,
					icon = "matrix",
					label="stat",
					tooltip=gettext("Summary in .csv",domain="R-stacomiR")),
			annuler=gWidgets::gaction(handler= quitte,
					icon = "close",
					label=gettext("Exit",domain="R-stacomiR")))
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=envir_stacomi)
	dev.new()
	
}