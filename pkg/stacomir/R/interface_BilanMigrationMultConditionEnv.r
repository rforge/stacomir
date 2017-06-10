#' interface for BilanMigrationConditionEnv class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationMultConditionEnv = function()
{
	quitte()
	bmmCE=new("BilanMigrationMultConditionEnv")
	assign("bmmCE",bmmCE,envir=envir_stacomi)	
	funout(gettext("Loading of the lists for taxons, stages, counting devices and monitoring stations\n",domain="R-stacomiR"))
	bmmCE@bilanConditionEnv@stationMesure=charge(bmmCE@bilanConditionEnv@stationMesure)
	#(destroys everything in envir_stacomi except stuff required at to level)
	objectBilan="bilanMigrationMult"
	# the following name is created by the interface
	# as I can't get the name from within the function (deparse(substitute(objectBilan)) does not return
	# "bilanMigrationMult" see refDC choice_c method)
	# so this will allow to assign "bilanMigrationMult" in envir_stacomi while using other class
	# like refDC
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	bmmCE@bilanMigrationMult=new("BilanMigrationMult")
	assign("bilanMigrationMult",bmmCE@bilanMigrationMult,envir = envir_stacomi)
	bilanFonctionnementDC=new("BilanFonctionnementDC")
	assign("bilanFonctionnementDC",bilanFonctionnementDC,envir = envir_stacomi)
	bilanFonctionnementDF=new("BilanFonctionnementDF")
	assign("bilanFonctionnementDF",bilanFonctionnementDF,envir = envir_stacomi)
	bilanOperation=new("BilanOperation")
	assign("bilanOperation",bilanOperation, envir=envir_stacomi)
	bilanMigration=new("BilanMigration")
	assign("bilanMigration",bilanMigration,envir = envir_stacomi)
	
	
	bmmCE@bilanMigrationMult@taxons=charge(bmmCE@bilanMigrationMult@taxons)
	bmmCE@bilanMigrationMult@stades=charge(bmmCE@bilanMigrationMult@stades)
	bmmCE@bilanMigrationMult@dc=charge(bmmCE@bilanMigrationMult@dc)
	group = ggroup(horizontal=TRUE)   # doit toujours s'appeller group
	assign("group",group,envir = envir_stacomi)  
	choice(bmmCE@bilanConditionEnv@stationMesure)
	notebook <- gnotebook(container=group)	
	assign("notebook",notebook,envir=envir_stacomi)
	size(notebook)<-c(400,300)
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	add(ggroupboutons,group)
	
	choicemult(bmmCE@bilanMigrationMult@pasDeTemps)
	choicemult(bmmCE@bilanMigrationMult@dc,objectBilan=bmmCE@bilanMigrationMult,is.enabled=TRUE)
	svalue(notebook)<-1
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	
	toolbarlist = list(
			Calc=gWidgets::gaction(handler = hbmmCEcalc,	
					icon = "new",
					label="calcul",
					tooltip=gettext("Calculation of environnemental conditions by time step",domain="R-stacomiR")),
			Graph=gWidgets::gaction(handler = hbmmCEgraph,
					icon = "graph",
					label="graph",
					tooltip=gettext("Balance graphic",domain="R-stacomiR")),
			annuler=gWidgets::gaction(handler= quitte,
					icon = "close",
					label="quitter"))
	assign("toolbarlist",toolbarlist,envir=envir_stacomi)
	enabled(toolbarlist[["Graph"]])<-FALSE
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)	
	gWidgets::addSpring(group)
	return(invisible(NULL))
}