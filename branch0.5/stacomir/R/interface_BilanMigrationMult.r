# Nom fichier :        interface_BilanMigrationMult

#' graphical interface for BilanMigrationMult class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @describeIn BilanMigrationMult
#' @export
interface_BilanMigrationMult=function(){
	quitte()
	objetBilan="bilanMigrationMult"
	assign("objetBilan",objetBilan,envir=envir_stacomi)
	bilanMigrationMult=new("BilanMigrationMult")
	assign("bilanMigrationMult",bilanMigrationMult,envir = envir_stacomi)
	fonctionnementDC=new("BilanFonctionnementDC")
	# appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
	assign("fonctionnementDC",fonctionnementDC,envir = envir_stacomi)
#TODO addmsg
	#funout(get("msg",envir=envir_stacomi)$interface_BilanMigration.1)
	bilanMigrationMult@taxons=charge(bilanMigrationMult@taxons)
	bilanMigrationMult@stades=charge(bilanMigrationMult@stades)
	bilanMigrationMult@dc=charge(bilanMigrationMult@dc)   	
	group = ggroup(horizontal=TRUE)   # doit toujours s'appeller group
	assign("group",group,envir = envir_stacomi)  
	# the notebook will contain all elements from 
	#######################
	#TODO TESTER ICI ICI ICI
	# ajout avec choixmult pour pas de temps.
	########################
	notebook <<- gnotebook(container=group)	
	size(notebook)<-c(400,300)
	add(ggroupboutons,group)
	choixmult(bilanMigrationMult@pasDeTemps)
	choixmult(bilanMigrationMult@dc,objetBilan=bilanMigrationMult,is.enabled=TRUE)
	svalue(notebook)<-1
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)
	add(ggroupboutons,ggroupboutonsbas)
	# todo addmsg
	# getStockIcons()
	toolbarlist = list(
			Calc=gaction(handler=hbilanMigrationMultcalc, action=bilanMigrationMult, icon="new", label="calcul", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.2),
			Graph=gaction(handler=hbilanMigrationMultgraph, icon="graph", label="graph", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.3),
			Graph2=gaction(handler=hbilanMigrationMultgraph2, icon="graph2", label="grcum", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.4),
			Stat=gaction(handler=hTableBilanMigrationMult, icon="dataframe", label="stat", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigration.5),    
			Out=gaction(handler=houtBilanMigrationMult, icon="gtk-info", label="code", tooltip=get("msg",envir=envir_stacomi)$interface_BilanMigrationMult.1),    
			annuler=gaction(handler= quitte,icon = "close",label="quitter")
	)    
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
	addSpring(group)
	#graphes=ggraphics(width=650,height=650)
	#add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	#assign("graphes",graphes,envir=.GlobalEnv) 
	x11()
}

