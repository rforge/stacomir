# Nom fichier :        interface_Bilan_lot.R    (interface)

#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
interface_BilanLot = function()
{  
    bilan_lot=new("Bilan_lot")
    assign("bilan_lot",bilan_lot,envir = .GlobalEnv)
    
    funout(get("msg",envir=envir_stacomi)$interface_Bilan_lot.1)
    bilan_lot@dc=charge(bilan_lot@dc)
    bilan_lot@taxons=charge(bilan_lot@taxons)
    bilan_lot@stades=charge(bilan_lot@stades)
    bilan_lot@par=charge(bilan_lot@par)    
     
    group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte() # vidange de l'interface
    assign("group",group,envir = .GlobalEnv)
    add(ggroupboutons,group)
    gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_lot.2,container=group)
    # dans l'ordre 
    # dans le handler, modifier le contenu de l'objet fils si il existe
    # supprimer les widgets fils si ils existent (appel de la methode delete)
    # appeller la methode choix pour l'affichage du fils si il existe
    
    
    choix(bilan_lot@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilan_lot_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
    choix(bilan_lot@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilan_lot_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
    
    choix(bilan_lot@dc,objetBilan=bilan_lot,is.enabled=TRUE)
    # Les methodes choix suivantes sont passees en cascade � l'interieur des methodes choix
    #choix(bilan_lot@taxons,is.enabled=FALSE)
    #choix(bilan_lot@stades,is.enabled=FALSE)
    #choix(bilan_lot@par,is.enabled=FALSE)
    #toolbarlist$Calc$handler = connect(fonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())
    # aBarchart=gaction(label="barchart",icon="barplot",handler=funbarchartBilan_lot,tooltip="Graphe mensuel")         
    #aBox=gaction(label="boites",icon="graph2",handler=funboxBilan_lot,tooltip="Boites � developper")
    aGrint=gaction(label="ggplot",icon="gWidgetsRGtk2-bubbles",handler=fungraphInteract_lot,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7)
    aTable=gaction(label="table",icon="dataframe",handler=funtableBilan_lot,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.8)
    aQuit=gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9)
    
    toolbarlist <- list(
    	#	barchart=aBarchart, 
    	#	box= aBox,
    		Grint=aGrint,
    		table=aTable,
    		Quit = aQuit)
    ggroupboutonsbas = ggroup(horizontal=FALSE)
    add(ggroupboutons,ggroupboutonsbas)
    add(ggroupboutonsbas, gtoolbar(toolbarlist))
    assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
    
    addSpring(group)
    graphes=ggraphics(width=800,height=650)
    add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal       
    assign("graphes",graphes,envir=.GlobalEnv) 
    # A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
}
