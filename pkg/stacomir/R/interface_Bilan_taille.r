# Nom fichier :        interface_Bilan_taille.R    (interface)

# see interface_Bilancarlot for doc
interface_BilanTaille = function()
{
	quitte() # vidange de l'interface
	bilan_taille=new("Bilan_taille")
	assign("bilan_taille",bilan_taille,envir=envir_stacomi)
	#funout(gettext("Loading of vue_ope_lot view, cd and timesteps choices\n"))
	bilan_taille@dc=charge(bilan_taille@dc)
	#bilan_taille@taxons=charge(bilan_taille@taxons)
	#bilan_taille@stades=charge(bilan_taille@stades)
	#bilan_taille@par=charge(bilan_taille@par)

	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)
	add(ggroupboutons,group)
	gl=glabel(text=gettext("Size report",container=group))
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	
	
	choice(bilan_taille@horodate,
			label=gettext("First timestamp"),
			nomassign="bilan_taille_date_debut",
			funoutlabel=gettext("The beginning date has been chosen\n"),
			decal=-2,
			affichecal=FALSE)
	choice(bilan_taille@horodate,
			label=gettext("Last timestamp"),
			nomassign="bilan_taille_date_fin",
			funoutlabel=gettext("The ending date has been chosen\n"),
			decal=-1,
			affichecal=FALSE)
	
	choice(bilan_taille@dc,objectBilan=bilan_taille,is.enabled=TRUE)
	aGrint=gWidgets::gaction(label="ggplot",icon="gWidgetsRGtk2-bubbles",handler=fungraphInteract_tail,tooltip=gettext("dotplot"))
	aTable=gWidgets::gaction(label=gettext("table"),icon="dataframe",handler=funtableBilan_tail,tooltip=gettext("Table"))
	aQuit=gWidgets::gaction(label=gettext("Exit"),icon="close", handler=quitte,tooltip=gettext("Exit"))
	aCalc=gWidgets::gaction(handler=hcalculeBilanTaille,action=bilan_taille,icon = "new",label="calcul",tooltip=gettext("crossed query length / qualitative feature"))
	toolbarlist <- list(
			#	barchart=aBarchart, 
			Calc=aCalc,
			Grint=aGrint,
			table=aTable,
			Quit = aQuit)
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)  
	enabled(toolbarlist[["Grint"]])<-FALSE
	enabled(toolbarlist[["table"]])<-FALSE
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=envir_stacomi)
	dev.new()	
	assign("toolbarlist",toolbarlist,envir=.GlobalEnv)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir =.GlobalEnv)
}
