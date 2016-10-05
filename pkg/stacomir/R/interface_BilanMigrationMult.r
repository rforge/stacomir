#' graphical interface for BilanMigrationMult class
#' 
#' launches the graphical interface after the main gwidget dialog has been launched
#' This function is called from a handler in the main graphical interface
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationMult=function(){
	quitte() # quitte must be the first in interface methods 
	#(destroys everything in envir_stacomi except stuff required at to level)
	objectBilan="bilanMigrationMult"
	# the following name is created by the interface
	# as I can't get the name from within the function (deparse(substitute(objectBilan)) does not return
	# "bilanMigrationMult" see refDC choice_c method)
	# so this will allow to assign "bilanMigrationMult" in envir_stacomi while using other class
	# like refDC
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	bilanMigrationMult=new("BilanMigrationMult")
	assign("bilanMigrationMult",bilanMigrationMult,envir = envir_stacomi)
	bilanFonctionnementDC=new("BilanFonctionnementDC")
	assign("bilanFonctionnementDC",bilanFonctionnementDC,envir = envir_stacomi)
	bilanFonctionnementDF=new("BilanFonctionnementDF")
	assign("bilanFonctionnementDF",bilanFonctionnementDF,envir = envir_stacomi)
	bilanOperation=new("BilanOperation")
	assign("bilanOperation",bilanOperation, envir=envir_stacomi)
	bilanMigration=new("BilanMigration")
	assign("bilanMigration",bilanMigration,envir = envir_stacomi)
	bilanMigrationMult@taxons=charge(bilanMigrationMult@taxons)
	bilanMigrationMult@stades=charge(bilanMigrationMult@stades)
	bilanMigrationMult@dc=charge(bilanMigrationMult@dc)   	
	group = ggroup(horizontal=TRUE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	# the notebook will contain all elements from 
	notebook <- gnotebook(container=group)	
	assign("notebook",notebook,envir=.GlobalEnv)
	size(notebook)<-c(400,300)
	add(ggroupboutons,group)
	choicemult(bilanMigrationMult@pasDeTemps)
	choicemult(bilanMigrationMult@dc,objectBilan=bilanMigrationMult,is.enabled=TRUE)
	svalue(notebook)<-1
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir=.GlobalEnv)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	# todo addmsg
	# getStockIcons()
	toolbarlist = list(
			Calc=gWidgets::gaction(handler=hbilanMigrationMultcalc,  icon="new", label="calcul", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.2),
			Graph=gWidgets::gaction(handler=hbilanMigrationMult_graph, icon="graph", label="graph", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			Graph2=gWidgets::gaction(handler=hbilanMigrationMultgraph2, icon="graph2", label="grcum", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.4),
			Graph3=gWidgets::gaction(handler=hbilanMigrationMultgraph3, icon="gWidgetsRGtk2-barplot", label="gr(tous)", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			Stat=gWidgets::gaction(handler=hTableBilanMigrationMult, icon="dataframe", label="stat", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.5),    
			Out=gWidgets::gaction(handler=houtBilanMigrationMult, icon="gtk-info", label="code", tooltip=get("msg",envir=envir_stacomi)$BilanMigrationMult.1),    
			annuler=gWidgets::gaction(handler= quitte,icon = "close",label="quitter")
	)    
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=650,height=650)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=envir_stacomi) 
	dev.new()
}

