# Nom fichier :        interface_Bilan_taille.R    (interface)

# see interface_BilanLot for doc
interface_BilanTaille = function()
{
    bilan_taille=new("Bilan_taille")
    assign("bilan_taille",bilan_taille,envir=.GlobalEnv)
    #funout("chargement de la vue (vue_ope_lot) et choix du dc et des pas de temps\n")
    bilan_taille@dc=charge(bilan_taille@dc)
    #bilan_taille@taxons=charge(bilan_taille@taxons)
    #bilan_taille@stades=charge(bilan_taille@stades)
    #bilan_taille@par=charge(bilan_taille@par)
     quitte() # vidange de l'interface
    group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    assign("group",group,envir = .GlobalEnv)
    add(ggroupboutons,group)
    gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_taille.1,container=group)
    # dans l'ordre 
    # dans le handler, modifier le contenu de l'objet fils si il existe
    # supprimer les widgets fils si ils existent (appel de la methode delete)
    # appeller la methode choix pour l'affichage du fils si il existe
    
    
    choix(bilan_taille@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilan_taille_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
    choix(bilan_taille@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilan_taille_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
    
    choix(bilan_taille@dc,objetBilan=bilan_taille,is.enabled=TRUE)
    aGrint=gaction(label="ggplot",icon="gWidgetsRGtk2-bubbles",handler=fungraphInteract_tail,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7)
    aTable=gaction(label="table",icon="dataframe",handler=funtableBilan_tail,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.8)
    aQuit=gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9)
    aCalc=gaction(handler=hcalculeBilanTaille,action=bilan_taille,icon = "new",label="calcul",tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_taille.2)
    toolbarlist <- list(
    	#	barchart=aBarchart, 
    		Calc=aCalc,
    		Grint=aGrint,
    		table=aTable,
    		Quit = aQuit)
	ggroupboutonsbas = ggroup(horizontal=FALSE)
    add(ggroupboutons,ggroupboutonsbas)  
	enabled(toolbarlist[["Grint"]])<-FALSE
	enabled(toolbarlist[["table"]])<-FALSE
    add(ggroupboutonsbas, gtoolbar(toolbarlist))
    addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    #assign("graphes",graphes,envir=.GlobalEnv)
	x11()
	assign("toolbarlist",toolbarlist,envir=.GlobalEnv)
	assign("ggroupboutonsbas",ggroupboutonsbas,envir = .GlobalEnv)
}
