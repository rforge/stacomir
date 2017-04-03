#' interface for BilanMigrationConditionEnv class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationMultConditionEnv = function()
{
	quitte()
	bmmCE=new("BilanMigrationMultConditionEnv")
	assign("bmmCE",bmmCE,envir=envir_stacomi)	
	funout(gettext("Loading of the lists for taxons, stages, counting devices and monitoring stations\n",domain="R-stacomiR"))
	bmmCE@bilanConditionEnv@stationMesure=charge(bmmCE@bilanConditionEnv@stationMesure)
	bmmCE@bilanMigrationMult@taxons=charge(bmmCE@bilanMigrationMult@taxons)
	bmmCE@bilanMigrationMult@stades=charge(bmmCE@bilanMigrationMult@stades)
	bmmCE@bilanMigrationMult@dc=charge(bmmCE@bilanMigrationMult@dc)

	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir=.GlobalEnv)
	add(ggroupboutons,group)
	
	choice(bmmCE@bilanMigrationMult@pasDeTemps)
	choice(bmmCE@bilanConditionEnv@stationMesure)
	choice(bmmCE@bilanMigrationMult@dc,objectBilan=bmmCE@bilanMigrationMult,is.enabled=TRUE)
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler = hbmmCEcalc,action=bmmCE,
					icon = "new",
					label="calcul",
					tooltip=gettext("Calculation of environnemental conditions by time step",domain="R-stacomiR")),
			Graph=gWidgets::gaction(handler = hbmmCEgraph,
					icon = "graph",
					label="graph",
					tooltip=gettext("Balance graphic",domain="R-stacomiR")),
			#Graph2=gWidgets::gaction(handler = hbmmCEgraph2,icon = "graph2",label="grcum",tooltip="graphe cumul"),
			#Stat =gWidgets::gaction(handler= hbmmCEstat,icon = "matrix",label="stat",tooltip="tables bilan en .csv"),
			annuler=gWidgets::gaction(handler= quitte,
					icon = "close",
					label="quitter"))
	assign("toolbarlist",toolbarlist,envir=.GlobalEnv)
	enabled(toolbarlist[["Graph"]])<-FALSE
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
assign("ggroupboutonsbas",ggroupboutonsbas,envir=.GlobalEnv)	
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=envir_stacomi)
	dev.new()
}