#' Interface for BilanMigration class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigration=function(){
	bilanMigration=new("BilanMigration")
	assign("bilanMigration",bilanMigration,envir = .GlobalEnv)
	fonctionnementDC=new("BilanFonctionnementDC")
	# appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
	assign("fonctionnementDC",fonctionnementDC,envir = .GlobalEnv)
	funout(get("msg",envir=envir_stacomi)$interface_BilanMigration.1)
	bilanMigration@taxons=charge(bilanMigration@taxons)
	bilanMigration@stades=charge(bilanMigration@stades)
	bilanMigration@dc=charge(bilanMigration@dc)   
	quitte()
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	add(ggroupboutons,group)
	choice(bilanMigration@pasDeTemps)
	choice(bilanMigration@dc,objectBilan=bilanMigration,is.enabled=TRUE)
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler=hbilanMigrationcalc, action=bilanMigration, icon="new", label="calcul", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.2),
			Graph=gWidgets::gaction(handler=hbilanMigrationgraph, icon="graph", label="graph", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			Graph2=gWidgets::gaction(handler=hbilanMigrationgraph2, icon="graph2", label="grcum", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.4),
			Stat=gWidgets::gaction(handler=hTableBilanMigration, icon="dataframe", label="stat", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.5),    
			annuler=gWidgets::gaction(handler= quitte,icon = "close",label="quitter")
	)    
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=600,height=600)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=.GlobalEnv) 
	grDevices::X11()
}