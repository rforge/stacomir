#' Interface for BilanMigration class, internal use, this function is called
#' by a handler in the main graphical interface
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigration=function(){ 
	# the quitte() method removes everything assigned in envir_stacomi by Bilan classes 
	quitte()
	# classes used along with BilanMigration by functions fungraph and fungraphcivelle
	# they are loaded there to be used later by methods "load" and "connect" of BilanMigration
	# so that connections to the database are not used later in examples (they are surrounded
	# by dontrun{}
	bilanFonctionnementDC=new("BilanFonctionnementDC")
	assign("bilanFonctionnementDC",bilanFonctionnementDC,envir = envir_stacomi)
	bilanFonctionnementDF=new("BilanFonctionnementDF")
	assign("bilanFonctionnementDF",bilanFonctionnementDF,envir = envir_stacomi)
	bilanOperation=new("BilanOperation")
	assign("bilanOperation", envir=envir_stacomi)
	bilanMigration=new("BilanMigration")
	assign("bilanMigration",bilanMigration,envir = envir_stacomi)
	# see bilanMigrationMult for explaination
	# this is used internally by refDC
	objectBilan="bilanMigration"
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	funout(get("msg",envir=envir_stacomi)$interface_BilanMigration.1)
	bilanMigration@taxons=charge(bilanMigration@taxons)
	bilanMigration@stades=charge(bilanMigration@stades)
	bilanMigration@dc=charge(bilanMigration@dc)  
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	add(ggroupboutons,group)
	choice(bilanMigration@pasDeTemps)
	choice(bilanMigration@dc,objectBilan=bilanMigration,is.enabled=TRUE)	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir=.GlobalEnv)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler=hbilanMigrationcalc, action=bilanMigration, icon="new", label="calcul", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.2),
			Graph=gWidgets::gaction(handler=hbilanMigrationgraph, icon="graph", label="graph", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			Graph2=gWidgets::gaction(handler=hbilanMigrationgraph2, icon="graph2", label="grcum", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.4),
			Stat=gWidgets::gaction(handler=hTableBilanMigration, icon="dataframe", label="stat", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.5),  
			write=gWidgets::gaction(handler=hbilanMigrationwrite, icon="gtk-harddisk", label="write", tooltip=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5),    
			Out=gWidgets::gaction(handler=houtBilanMigration, icon="gtk-info", label="code", tooltip=get("msg",envir=envir_stacomi)$BilanMigrationMult.1),    
			annuler=gWidgets::gaction(handler= quitte,icon = "close",label="quitter")
	)    
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	dev.new()
}