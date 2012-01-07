# Nom fichier :        interface_BilanConditionEnv    (classe)

#' Interface for class conditionEnv
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
interface_ConditionEnv = function()
{
    bilanConditionEnv=new("BilanConditionEnv")
    funout(get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.1)
    bilanConditionEnv@stationMesure=charge(bilanConditionEnv@stationMesure)
    assign("bilanConditionEnv",bilanConditionEnv,envir=.GlobalEnv)
    
    group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte()
    assign("group",group,envir = .GlobalEnv)
    add(ggroupboutons,group)
    
    # date de debut et de fin
    choix(bilanConditionEnv@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilanConditionEnv_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
    choix(bilanConditionEnv@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilanConditionEnv_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
    choix(bilanConditionEnv@stationMesure)
    
    ggroupboutonsbas = ggroup(horizontal=FALSE)
    add(ggroupboutons,ggroupboutonsbas)
    assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
    
    toolbarlist = list(
    #Calc=gaction(handler=hbilanConditionEnvcalc , action=bilanConditionEnv,icon = "new",label="calcul",tooltip="calcul des conditions environnementales entre deux dates"),
    Graph=gaction(handler=hbilanConditionEnvgraph , icon = "graph",label="graph",tooltip=get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.2),
    Stat =gaction(handler=hbilanConditionEnvstat , icon = "matrix",label="stat",tooltip=get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.3),
    annuler=gaction(handler= quitte,icon = "close",label=get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.4))
    add(ggroupboutonsbas, gtoolbar(toolbarlist))
    addSpring(group)
    graphes=ggraphics(width=650,height=650)
    add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    assign("graphes",graphes,envir=.GlobalEnv)
}