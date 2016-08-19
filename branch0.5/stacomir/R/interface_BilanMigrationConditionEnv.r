# Nom fichier :        interface_BilanMigrationConditionEnv    

#' interface for BilanMigrationConditionEnv class
#' @aliases interface_BilanMigration
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationConditionEnv = function()
{
	bilanMigrationConditionEnv=new("BilanMigrationConditionEnv")
	assign("bilanMigrationConditionEnv",bilanMigrationConditionEnv,envir=.GlobalEnv)	
	funout(get("msg",envir=envir_stacomi)$interface_BilanMigrationConditionEnv.2)
	bilanMigrationConditionEnv@bilanConditionEnv@stationMesure=charge(bilanMigrationConditionEnv@bilanConditionEnv@stationMesure)
	bilanMigrationConditionEnv@bilanMigration@taxons=charge(bilanMigrationConditionEnv@bilanMigration@taxons)
	bilanMigrationConditionEnv@bilanMigration@stades=charge(bilanMigrationConditionEnv@bilanMigration@stades)
	bilanMigrationConditionEnv@bilanMigration@dc=charge(bilanMigrationConditionEnv@bilanMigration@dc)
	quitte()
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)
	add(ggroupboutons,group)
	
	choice(bilanMigrationConditionEnv@bilanMigration@pasDeTemps)
	choice(bilanMigrationConditionEnv@bilanConditionEnv@stationMesure)
	choice(bilanMigrationConditionEnv@bilanMigration@dc,objectBilan=bilanMigrationConditionEnv@bilanMigration,is.enabled=TRUE)
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler = hbilanMigrationConditionEnvcalc,action=bilanMigrationConditionEnv,icon = "new",label="calcul",tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationConditionEnv.1),
			Graph=gWidgets::gaction(handler = hbilanMigrationConditionEnvgraph,icon = "graph",label="graph",tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			#Graph2=gWidgets::gaction(handler = hbilanMigrationConditionEnvgraph2,icon = "graph2",label="grcum",tooltip="graphe cumul"),
			#Stat =gWidgets::gaction(handler= hbilanMigrationConditionEnvstat,icon = "matrix",label="stat",tooltip="tables bilan en .csv"),
			annuler=gWidgets::gaction(handler= quitte,icon = "close",label="quitter"))
	assign("toolbarlist",toolbarlist,envir=.GlobalEnv)
	enabled(toolbarlist[["Graph"]])<-FALSE
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)	
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=.GlobalEnv)
	grDevices::X11()
}