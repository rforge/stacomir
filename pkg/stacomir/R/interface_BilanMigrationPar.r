# Nom fichier :        interface_BilanMigrationPar 
# pour le developpement load("devt.Rdata")
# chargement des classes
#' interface for BilanMigrationPar class
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @export
interface_BilanMigrationPar = function()
{
    bilanMigrationPar=new("BilanMigrationPar")
    assign("bilanMigrationPar",bilanMigrationPar,envir=.GlobalEnv)
    
    fonctionnementDC=new("BilanFonctionnementDC") # appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
    assign("fonctionnementDC",fonctionnementDC,envir=.GlobalEnv)
    
    funout(get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.1)
    bilanMigrationPar@taxons=charge(bilanMigrationPar@taxons)
    bilanMigrationPar@stades=charge(bilanMigrationPar@stades)
    bilanMigrationPar@dc=charge(bilanMigrationPar@dc)
    bilanMigrationPar@parquan=charge(bilanMigrationPar@parquan)
    bilanMigrationPar@parqual=charge(bilanMigrationPar@parqual)
    bilanMigrationPar@echantillon=charge(bilanMigrationPar@echantillon,vecteur=c("avec","sans"),label=get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.2, selected=as.integer(1))
    #######################
    # Interface Graphique 
    ##########################
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte()
    assign("group",group,envir = .GlobalEnv)
    
   gWidgets::add(ggroupboutons,group)
    choice(bilanMigrationPar@pasDeTemps)
    choice(bilanMigrationPar@echantillon)
    choice(bilanMigrationPar@dc,objetBilan=bilanMigrationPar,is.enabled=TRUE)
    
    
    ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
    gWidgets::add(ggroupboutons,ggroupboutonsbas)
    assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
    toolbarlist = list(
    Calc=gWidgets::gaction(handler = hbilanMigrationParcalc,icon = "new",label="calcul",action=bilanMigrationPar,tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.3),
    Graph=gWidgets::gaction(handler = hbilanMigrationPargraph,icon = "graph",label="graph",tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.4),
    Graph2=gWidgets::gaction(handler = hbilanMigrationPargraph2,icon = "graph2",label="grjour",tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.5),
    Stat =gWidgets::gaction(handler= hbilanMigrationParstat,icon = "matrix",label="stat",tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.6),
    annuler=gWidgets::gaction(handler= quitte,icon = "close",label=get("msg",envir=envir_stacomi)$interface_BilanMigrationPar.7))
    gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
    gWidgets::addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    #assign("graphes",graphes,envir=.GlobalEnv)
	grDevices::X11()

}