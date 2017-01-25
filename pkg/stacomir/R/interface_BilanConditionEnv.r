# Nom fichier :        interface_BilanConditionEnv    (classe)

#' Interface for class conditionEnv
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_ConditionEnv = function()
{
	quitte()
	bilanConditionEnv=new("BilanConditionEnv")
    funout(gettext(get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.1))
    bilanConditionEnv@stationMesure=charge(bilanConditionEnv@stationMesure)
    assign("bilanConditionEnv",bilanConditionEnv,envir=envir_stacomi)
    
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group

    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    
    # date de debut et de fin
    choice(bilanConditionEnv@horodate,label=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.3),
			nomassign="bilanConditionEnv_date_debut",
			funoutlabel=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.5),
			decal=-2,
			affichecal=FALSE)
    choice(bilanConditionEnv@horodate,
			label=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.4),
			nomassign="bilanConditionEnv_date_fin",
			funoutlabel=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.6),
			decal=-1,
			affichecal=FALSE)
    choice(bilanConditionEnv@stationMesure)
    
    ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
    gWidgets::add(ggroupboutons,ggroupboutonsbas)
    assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
    
    toolbarlist = list(
    #Calc=gWidgets::gaction(handler=hbilanConditionEnvcalc , action=bilanConditionEnv,icon = "new",label="calcul",tooltip="calcul des conditions environnementales entre deux dates"),
    Graph=gWidgets::gaction(handler=hbilanConditionEnvgraph , icon = "graph",label="graph",tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.2)),
    Stat =gWidgets::gaction(handler=hbilanConditionEnvstat , icon = "matrix",label="stat",tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.3)),
    annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext(get("msg",envir=envir_stacomi)$interface_BilanConditionEnv.4)))
    gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
    gWidgets::addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    #assign("graphes",graphes,envir=envir_stacomi)
	dev.new()
}