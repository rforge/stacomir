# Nom fichier :        interface_BilanMigrationInterAnnuelle.R    (interface)

#' interface for BilanMigrationInterannuelle class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationInterAnnuelle = function()
{
	quitte() # vidange de l'interface
	bilanMigrationInterAnnuelle=new("BilanMigrationInterAnnuelle")
	assign("bilanMigrationInterAnnuelle",bilanMigrationInterAnnuelle,envir=.GlobalEnv)
	funout(get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.1)
	bilanMigrationInterAnnuelle@anneeDebut=charge(bilanMigrationInterAnnuelle@anneeDebut)
	bilanMigrationInterAnnuelle@anneeFin=charge(bilanMigrationInterAnnuelle@anneeFin)
	bilanMigrationInterAnnuelle@dc=charge(bilanMigrationInterAnnuelle@dc)
	bilanMigrationInterAnnuelle@taxons=charge(bilanMigrationInterAnnuelle@taxons)
	bilanMigrationInterAnnuelle@stades=charge(bilanMigrationInterAnnuelle@stades)
	
	group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	add(ggroupboutons,group)
	
	# pour preselectionner une date on lui fournit l'indice de la date dans le RefAnnee. indice = 11 pour 2005
	
	choix(bilanMigrationInterAnnuelle@anneeDebut,
			nomassign="anneeDebut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.2,
			titleFrame=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.3,
			preselect=which(bilanMigrationInterAnnuelle@anneeDebut@data==min(bilanMigrationInterAnnuelle@anneeDebut@data)))
	choix(bilanMigrationInterAnnuelle@anneeFin,
			nomassign="anneeFin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.4,
			titleFrame=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.5,
			preselect=which(bilanMigrationInterAnnuelle@anneeDebut@data==max(bilanMigrationInterAnnuelle@anneeFin@data)))
	choix(bilanMigrationInterAnnuelle@dc,objectBilan=bilanMigrationInterAnnuelle,is.enabled=TRUE)
	
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choix pour l'affichage du fils si il existe
	### premiere toobar
	
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	add(ggroupboutons,ggroupboutonsbas)
	
	toolbarlist1 = list(
			aGraph=gaction(label="all",icon="lines",handler=hgraphBilanMigrationInterAnnuelle,tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.6),
			aGraph7=gaction(label="cum15",icon="curve",handler=hgraphBilanMigrationInterAnnuelle7,tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.6),
			aGraph3=gaction(label="cum",icon="graph2",handler=hgraphBilanMigrationInterAnnuelle3,tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.7),
			aTable=gaction(label="table",icon="dataframe",handler=htableBilanMigrationInterAnnuelle,tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.8),
			aQuit=gaction(label="fermer",icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.9)
	)
	toolbarlist2=list(
			aGraph2=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.10,
					icon="hist",handler=hgraphBilanMigrationInterAnnuelle2,
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.11),
			aGraph4=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.12,
					icon="hist",handler=hgraphBilanMigrationInterAnnuelle4,
					action="semaine",
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.13),
			aGraph5=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.14,
					icon="hist",
					handler=hgraphBilanMigrationInterAnnuelle4,
					action="quinzaine",
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.15),
			aGraph6=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.16,
					icon="hist",handler=hgraphBilanMigrationInterAnnuelle4,
					action="mois",
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.17)      
	)
	toolbarlist3=list(
			aGraph1=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.12,
					icon="gWidgetsRGtk2-points",handler=hgraphBilanMigrationInterAnnuelle5,
					action="semaine",
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.13),
			aGraph2=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.14,
					icon="gWidgetsRGtk2-points",
					handler=hgraphBilanMigrationInterAnnuelle5,
					action="quinzaine",
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.15),
			aGraph3=gaction(label=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.16,
					icon="gWidgetsRGtk2-points",handler=hgraphBilanMigrationInterAnnuelle5,
					action="mois",
					tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.17)      
	)
	add(ggroupboutonsbas, gtoolbar(toolbarlist1))
	add(ggroupboutonsbas, gtoolbar(toolbarlist2)) 
	add(ggroupboutonsbas, gtoolbar(toolbarlist3))
	addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=.GlobalEnv) 
	x11()
	
# A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
	
}