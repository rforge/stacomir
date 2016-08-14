# Nom fichier :        interface_poids_moyen.R    (interface)

#' interface for Bilan_poids_moyen class 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanPoidsMoyen = function()
{
    quitte()  # vidange de l'interface
	bilan_poids_moyen=new("Bilan_poids_moyen")
    assign("bilan_poids_moyen",bilan_poids_moyen,envir = .GlobalEnv)
    bilan_poids_moyen@dc=charge(bilan_poids_moyen@dc)
    bilan_poids_moyen@anneedebut=charge(bilan_poids_moyen@anneedebut)
    bilan_poids_moyen@anneefin=charge(bilan_poids_moyen@anneefin)
    bilan_poids_moyen@liste=charge(object=bilan_poids_moyen@liste,vecteur=c("=1",">1","tous"),label=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.5)# choix de la cat�gorie d'effectif
    #bilan_poids_moyen@taxons=charge(bilan_poids_moyen@taxons)
    #bilan_poids_moyen@stades=charge(bilan_poids_moyen@stades)
    group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    assign("group",group,envir = .GlobalEnv)
    add(ggroupboutons,group)
    gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.1,container=group)
    # dans l'ordre 
    # dans le handler, modifier le contenu de l'object fils si il existe
    # supprimer les widgets fils si ils existent (appel de la methode delete)
    # appeller la methode choix pour l'affichage du fils si il existe
    ### premiere toobar

    
    addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    #assign("graphes",graphes,envir=.GlobalEnv)
	x11()
    # A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
    choix(bilan_poids_moyen@liste)
    choix(bilan_poids_moyen@dc,objectBilan=NULL,is.enabled=TRUE)
    choix(bilan_poids_moyen@anneedebut,
			nomassign="refAnneeDebut",
			titleFrame=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.3)#annee debut
    choix(bilan_poids_moyen@anneefin,
			nomassign="refAnneeFin",
			titleFrame=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.5)#annee fin
	aGraph=gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.2,icon="lines",handler=fungraphBilan_poids_moyen,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.2)#load
	aTable=gaction(label="table",icon="dataframe",handler=funtableBilan_poids_moyen,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.3)
	aQuit=gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.4,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.4)
	toolbarlist <- list(barchart=aGraph,table=aTable,Quit = aQuit)
	add(group, gmenu(toolbarlist))
    
    # Les methodes choix suivantes sont passees en cascade � l'interieur des methodes choix
    #choix(bilan_lot@taxons,is.enabled=FALSE)
    #choix(bilan_lot@stades,is.enabled=FALSE)
    #choix(bilan_lot@par,is.enabled=FALSE)
    
    # TODO ajouter une fonction eval ou la liste du combo box est diminuee � l'aide d'un vecteur qu'on lui passe : les caracteristiques existant pour ce taxon et stade
    
    #toolbarlist$Calc$handler = connect(fonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())
        
    ###########################################################
    # Liste de dates de debut et de fin de fonctionnement
    #############################################################
    #vectannee=1996:2006
    #vectdebut=vector()
    #vectfin=vector()
    #for (j in 1:length(vectannee)){
    #tempsannee<-tempsdebut[annee==as.character(vectannee[j])]
    #fonctannee<-t_periodefonctdispositif_per$per_etat_fonctionnement[annee==as.character(vectannee[j])] 
    #vectdebut[j]<-strftime(min(tempsannee[fonctannee==1]),"%Y-%m-%d %H:%M:%S")
    #vectfin[j]<-strftime(max(tempsannee[fonctannee==1]),"%Y-%m-%d %H:%M:%S") }
    #
    #toto<-cbind(vectdebut,vectfin)
}