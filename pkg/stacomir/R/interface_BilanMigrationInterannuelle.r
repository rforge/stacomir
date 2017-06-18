#' interface for BilanMigrationInterannuelle class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_BilanMigrationInterAnnuelle = function()
{
	quitte() # vidange de l'interface
	bilanMigrationInterAnnuelle=new("BilanMigrationInterAnnuelle")
	assign("bilanMigrationInterAnnuelle",bilanMigrationInterAnnuelle,envir=envir_stacomi)
	funout(gettext("Loading of the existing daily summaries\n",domain="R-stacomiR"))
	bilanMigrationInterAnnuelle@anneeDebut=charge(bilanMigrationInterAnnuelle@anneeDebut)
	bilanMigrationInterAnnuelle@anneeFin=charge(bilanMigrationInterAnnuelle@anneeFin)
	bilanMigrationInterAnnuelle@dc=charge(bilanMigrationInterAnnuelle@dc)
	bilanMigrationInterAnnuelle@taxons=charge(bilanMigrationInterAnnuelle@taxons)
	bilanMigrationInterAnnuelle@stades=charge(bilanMigrationInterAnnuelle@stades)
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = envir_stacomi)  
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	add(ggroupboutons,group)
	
	# pour preselectionner une date on lui fournit l'indice de la date dans le Ref_year. indice = 11 pour 2005
	
	choice(bilanMigrationInterAnnuelle@anneeDebut,
			nomassign="anneeDebut",
			funoutlabel=gettext("The year of beginning has been chosen\n",domain="R-stacomiR"),
			titleFrame=gettext("Beginning year",domain="R-stacomiR"),
			preselect=which(bilanMigrationInterAnnuelle@anneeDebut@data==min(bilanMigrationInterAnnuelle@anneeDebut@data)))
	choice(bilanMigrationInterAnnuelle@anneeFin,
			nomassign="anneeFin",
			funoutlabel=gettext("The last year has been chosen\n",domain="R-stacomiR"),
			titleFrame=gettext("Ending year",domain="R-stacomiR"),
			preselect=which(bilanMigrationInterAnnuelle@anneeDebut@data==max(bilanMigrationInterAnnuelle@anneeFin@data)))
	choice(bilanMigrationInterAnnuelle@dc,objectBilan=bilanMigrationInterAnnuelle,is.enabled=TRUE)
	
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	### premiere toobar
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	
	toolbarlist1 = list(
			aGraph=gWidgets::gaction(label="all",
					icon="lines",
					handler=hgraphBilanMigrationInterAnnuelle,
					tooltip=gettext("Migration of all the years in the same graphic",domain="R-stacomiR")),
			aGraph7=gWidgets::gaction(label="cum15",
					icon="curve",
					handler=hgraphBilanMigrationInterAnnuelle7,
					tooltip=gettext("Migration of all the years in the same graphic, cumulated",domain="R-stacomiR")),
			aGraph3=gWidgets::gaction(label="step",
					icon="graph2",
					handler=hgraphBilanMigrationInterAnnuelle3,
					tooltip=gettext("cumulated migrations %",domain="R-stacomiR")),
			aSummary=gWidgets::gaction(handler=hsummaryBilanMigrationInterannuelle,
					icon="dataframe", 
					label="stat",
					tooltip="summary"),  
			aQuit=gWidgets::gaction(label="fermer",
					icon="close", 
					handler=quitte,
					tooltip=gettext("Exit",domain="R-stacomiR"))
	)
	toolbarlist2=list(
			aGraph2=gWidgets::gaction(label=gettext("day",domain="R-stacomiR"),
					icon="hist",handler=hgraphBilanMigrationInterAnnuelle2,
					tooltip=gettext("Daily migration",domain="R-stacomiR")),
			aGraph4=gWidgets::gaction(label=gettext("week",domain="R-stacomiR"),
					icon="hist",handler=hgraphBilanMigrationInterAnnuelle4,
					action="semaine",
					tooltip=gettext("weekly migration",domain="R-stacomiR")),
			aGraph5=gWidgets::gaction(label=gettext("fortnight",domain="R-stacomiR"),
					icon="hist",
					handler=hgraphBilanMigrationInterAnnuelle4,
					action="quinzaine",
					tooltip=gettext("Fortnight Migration",domain="R-stacomiR")),
			aGraph6=gWidgets::gaction(label=gettext("month",domain="R-stacomiR"),
					icon="hist",handler=hgraphBilanMigrationInterAnnuelle4,
					action="mois",
					tooltip=gettext("Monthly migration",domain="R-stacomiR"))
	)
	toolbarlist3=list(
			aGraph1=gWidgets::gaction(label=gettext("week",domain="R-stacomiR"),
					icon="gWidgetsRGtk2-points",handler=hgraphBilanMigrationInterAnnuelle5,
					action="semaine",
					tooltip=gettext("weekly migration",domain="R-stacomiR")),
			aGraph2=gWidgets::gaction(label=gettext("fortnight",domain="R-stacomiR"),
					icon="gWidgetsRGtk2-points",
					handler=hgraphBilanMigrationInterAnnuelle5,
					action="quinzaine",
					tooltip=gettext("Fortnight migration",domain="R-stacomiR")),
			aGraph3=gWidgets::gaction(label=gettext("month",domain="R-stacomiR"),
					icon="gWidgetsRGtk2-points",handler=hgraphBilanMigrationInterAnnuelle5,
					action="mois",
					tooltip=gettext("Monthly migration",domain="R-stacomiR"))
	)
	add(ggroupboutonsbas, gtoolbar(toolbarlist1))
	add(ggroupboutonsbas, gtoolbar(toolbarlist2)) 
	add(ggroupboutonsbas, gtoolbar(toolbarlist3))
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=envir_stacomi) 
	dev.new()
	
# A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
	
}