# Nom fichier :        interface_BilanMigrationMult

#' Interface for BilanMigrationMult class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
interface_BilanMigrationMult=function(){
	bilanMigrationMult=new("BilanMigrationMult")
	assign("bilanMigrationMult",bilanMigrationMult,envir = .GlobalEnv)
	fonctionnementDC=new("BilanFonctionnementDC")
	# appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
	assign("fonctionnementDC",fonctionnementDC,envir = .GlobalEnv)
#TODO addmsg
	#funout(get("msg",envir=envir_stacomi)$interface_BilanMigration.1)
	bilanMigrationMult@taxons=charge(bilanMigrationMult@taxons)
	bilanMigrationMult@stades=charge(bilanMigrationMult@stades)
	bilanMigrationMult@dc=charge(bilanMigrationMult@dc)   
	quitte()
	group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	add(ggroupboutons,group)
	choix(bilanMigrationMult@pasDeTemps)
	choixmult(bilanMigrationMult@dc,objetBilan=bilanMigrationMult,is.enabled=TRUE)
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	add(ggroupboutons,ggroupboutonsbas)
	# todo addmsg
	toolbarlist = list(
			Calc=gaction(handler=hbilanMigrationMultcalc, action=bilanMigrationMult, icon="new", label="calcul", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.2),
			Graph=gaction(handler=hbilanMigrationMultgraph, icon="graph", label="graph", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			Graph2=gaction(handler=hbilanMigrationMultgraph2, icon="graph2", label="grcum", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.4),
			Stat=gaction(handler=hTableBilanMigrationMult, icon="dataframe", label="stat", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.5),    
			annuler=gaction(handler= quitte,icon = "close",label="quitter")
	)    
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
	addSpring(group)
	#graphes=ggraphics(width=650,height=650)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=.GlobalEnv) 
	x11()
}