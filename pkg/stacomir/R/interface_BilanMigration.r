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
	assign("bilanOperation",bilanOperation, envir=envir_stacomi)
	bilanMigration=new("BilanMigration")
	assign("bilanMigration",bilanMigration,envir = envir_stacomi)
	# see bilanMigrationMult for explaination
	# this is used internally by refDC
	objectBilan="bilanMigration"
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	funout(gettext("Loading of the lists for taxons, stages and counting devices\n",domain="R-stacomiR"))
	bilanMigration@taxons=charge(bilanMigration@taxons)
	bilanMigration@stades=charge(bilanMigration@stades)
	bilanMigration@dc=charge(bilanMigration@dc)  
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	add(ggroupboutons,group)
	choice(bilanMigration@pasDeTemps)
	choice(object=bilanMigration@dc,objectBilan=bilanMigration,is.enabled=TRUE)	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir=.GlobalEnv)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler=hbilanMigrationcalc, action=bilanMigration, icon="new", label=gettext("calculate",domain="R-stacomiR"), tooltip=gettext("Calculation of numbers by time step",domain="R-stacomiR")),
			Graph=gWidgets::gaction(handler=hbilanMigrationgraph, icon="graph", label=gettext("graph",domain="R-stacomiR"), tooltip=gettext("Balance graphic",domain="R-stacomiR")),
			Graph2=gWidgets::gaction(handler=hbilanMigrationgraph2, icon="graph2", label=gettext("grcum",domain="R-stacomiR"), tooltip=gettext("Cumulative graphic",domain="R-stacomiR")),
			Stat=gWidgets::gaction(handler=hTableBilanMigration, icon="dataframe", label=gettext("stat",domain="R-stacomiR"), tooltip=gettext("Balance sheet in .csv",domain="R-stacomiR")),
			write=gWidgets::gaction(handler=hbilanMigrationwrite, icon="gtk-harddisk", label=gettext("write",domain="R-stacomiR"), tooltip=gettext("Writing daily summary in the database",domain="R-stacomiR")),
			Out=gWidgets::gaction(handler=houtBilanMigration, icon="gtk-info", label=gettext("code",domain="R-stacomiR"), tooltip=gettext("Code",domain="R-stacomiR")),
			annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext("quit",domain="R-stacomiR"))
	)    
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	dev.new()
}