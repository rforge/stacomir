#' handler function used by the main interface
#' 
#' 
#' @param h A handler
#' @param ... Other parameters
hDF=function(h,...){
	funout(gettext("Calculation of the operating fishway\n"),wash=TRUE)
	eval(interface_BilanFonctionnementDF(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hDC=function(h,...){
	funout(gettext("Calculation of the operating counting device\n"),wash=TRUE)
	eval(interface_BilanFonctionnementDC(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hOPE=function(h,...){
	# TODO a developper
	funout(text=gettext("Summary of the operations of a device ... to do\n"),wash=TRUE)
}
#' handler function used by the main interface#' 
#' @param h handler
#' @param ... additional parameters
hDFDC=function(h,...){
	# TODO developper cette fonction
	funout(gettext("Summary between the operating fishway and the counting device … to do\n"),wash=TRUE)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanMigration=function(h,...){
	funout(gettext("Migration summary (for a species and a stage)\n"),wash=TRUE)
	eval(interface_BilanMigration(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanMigrationMult=function(h,...){
	funout(gettext("For help, contact Cédric Briand - 0033 29 99 08 844 - cedric.briand@lavilaine.com\n"),wash=TRUE)
	eval(interface_BilanMigrationMult(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanMigrationInterAnnuelle=function(h,...){
	funout(gettext("Summary of interannual migration\n"),wash=TRUE)
	eval(interface_BilanMigrationInterAnnuelle(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanMigrationConditionEnv=function(h,...){
	funout(gettext("Summary of migration environnemental conditions\n"),wash=TRUE)
	eval(interface_BilanMigrationConditionEnv(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanMigrationPar=function(h,...){
	funout(gettext("Summary of migration with parameters\n"),wash=TRUE)
	eval(interface_BilanMigrationPar(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanConditionEnv=function(h,...){
	funout(gettext("Summary of the environnemental conditions\n"),wash=TRUE)
	eval(interface_ConditionEnv(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilan_carlot=function(h,...){
	funout(gettext("Summary of batch by calling the application vue lot ope\n"),wash=TRUE)
	eval(interface_Bilan_carlot(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hTail=function(h,...){
	funout(gettext("Lengths summary\n"),wash=TRUE)
	eval(interface_BilanTaille(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hpds=function(h,...){
	eval(interface_BilanPoidsMoyen(),envir = .GlobalEnv)
	funout(gettext("Summary of average weight for the calculation of the relation between length and number.\n"),wash=TRUE) 
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hSt=function(h,...){
	funout(gettext("Calculation of the pigmentary stages\n"),wash=TRUE)
	eval(interface_Bilan_stades_pigm(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hbilA=function(h,...){
	eval(interface_BilanAnnuels(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hsilver=function(h,...){
	eval(interface_BilanArgentee(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
htodo=function(h,...){
	funout(gettext("TODO to develop"),wash=TRUE)
	eval(interface_BilanAnnuels(),envir = .GlobalEnv)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
hBilanEspeces=function(h,...){
	funout(gettext("Species summary of the counting device\n"),wash=TRUE)
	eval(interface_BilanEspeces(),envir = .GlobalEnv)
}



#' Internal function, tests the connection and if it works loads the stacomi interface
#' @note \code{gr_interface} is copied by stacomi into envir_stacomi. Same for \code{database_expected}
#' 
#' @param h A handler
#' @param ... Other arguments
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
husr=function(h,...){
	baseODBC<-get("baseODBC",envir=envir_stacomi)
	# assigned when passing through stacomi
	gr_interface<-get("gr_interface",envir_stacomi) # logical true or false
	database_expected<-get("database_expected",envir_stacomi) # logical true or false
	login_window<-get("login_window",envir_stacomi) # logical true or false
	# test de la connection
	if (login_window & gr_interface&database_expected){	
			baseODBC[2]<-svalue(usrname)
			baseODBC[3]<-svalue(usrpwd)
			assign("sch",paste(baseODBC[2],".",sep=""),envir=envir_stacomi)
			assign("baseODBC",baseODBC,envir=envir_stacomi)
			dispose(logw)
		} else {
			# nothing sch and baseODBC have been assigned from default value in stacomi()	
		}
		
		# we dispose loginwindow
	
	
	if (database_expected){
		con=new("ConnectionODBC")
		con@baseODBC=get("baseODBC",envir=envir_stacomi)
		e=expression(con<-connect(con))
		con=tryCatch(eval(e),error=gettext("Error when using the method connect of the ConnectionODBC class") )
		test<-con@etat==gettext("Connection in progress")
		odbcCloseAll()
	}
	# if the test is OK launches the stacomi interface
	# function handler called by gmessage
	hgmessage=function(h,...){
		stacomi(gr_interface=TRUE)
		# if there is an error, re-launches and asks for a password
	}
	#############################
	# second test to check that the database is working well
	############################
	if (database_expected){
		if (test) { # il existe un lien ODBC mais qui pointe peut etre ailleurs
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql="select count(*) from ref.tr_taxon_tax"
			requete<-stacomirtools::connect(requete)
			if (nrow(requete@query)==0){
				# the odbc link does not work and might be pointing to a wrong schema
				# this time the argument login_window will be ignored
				gWidgets::gmessage(message=paste(gettext("\"\"Problem during the test, the ODBC link works but doesn't point to the database 0.5, check the ODBC link\"\""),
								"\n",
								gettext("ODBC link"),
								" :",
								baseODBC[1],
								"\n",
								gettext("User"),
								" :",
								baseODBC[2],
								"\n",
								gettext("Password"),
								" :",
								baseODBC[3]),						
						title=gettext("Error title of the frame"),
						icon = "error",
						handler=hgmessage)		
			} else {
				assign("baseODBC",baseODBC,envir=envir_stacomi)
				if (gr_interface){
					interface_graphique()
				}
			}# end else nrow(>0)
		} else {
			# the test has failed and the user will be prompted to another login window
			# this time the argument loginwindow will be ignored
			gWidgets::gmessage(message=paste(gettext("Problem when testing the ODBC connection"),
							"\n",
							gettext("ODBC link"),
							" :",
							baseODBC[1],
							"\n",
							gettext("User"),
							" :",
							baseODBC[2],
							"\n",
							gettext("Password"),
							" :",
							baseODBC[3]),						
					title=gettext("Error title of the frame"),
					icon = "error",
					handler=hgmessage)
		} # end else test (else == the test didn't pass, we have to change the name and password	
	} else {
		# here : database_expected=FALSE
		# we don't want to check the connection at all...
		if (gr_interface){
			interface_graphique()
		}
	}
}
hhelp=function(h,...){
	funout(gettext("For help, contact Cédric Briand - 0033 29 99 08 844 - cedric.briand@lavilaine.com\n"),wash=TRUE)
}
hlang=function(h,...){
	eval(interface_chooselang(),envir = .GlobalEnv)
}
hX11=function(h,...){
	dev.new()
}





#' Function that loads the loginwindow, tests connection, and then destroys the
#' window
#' 
#' @note The defaut behaviour of the program is to run through the following elements
#'  \itemize{
#'      \item{login window}{ The program opens a login window to prompt the user to give his usernames and passwords.
#' 			default values will proposed from "C:/program files/stacomi/calcmig.csv" and if this file does not exists, 
#' 			from \code{file.path(.libPaths(),"stacomiR","config","calcmig.csv")} as a default. If \code{login_window=FALSE}
#' 			the program will skip the login window and use calcmig values for user (\code{uid}) and password(\code{pwd}) as a default.}
#'      \item{tests for connection}{ Test for the existence of a calcmig.csv file, and then the existence of the file
#' 			\code{usr.tr_taxon_tax} where usr is the username extracted from calcmig. These tests are only done if 
#' 			\code{database_expected=TRUE}. If the test don't pass, then the user is prompted for a "login window" even if argument
#' 			\code{login_window} was set to \code{FALSE} at launch.}
#'       \item{graphical interface}{ When either, previous tests have been run successfully, or the value for
#'          \code{database_expected=FALSE} the program will launch. If \code{graphical_interface} is \code{TRUE}, the program will use
#'          a graphical interface \code{\link{interface_graphique}} to build the graphical interface, otherwise the program is expected to run
#' 			through the command line.}
#'  }
#' When \code{database_expected=FALSE} a connection to the database is not expected. Therefore test are run by calling examples object stored in Rdata.
#'  And also messages are downloaded from the database in several languages. These are loaded from the data directory of the package instead, and
#' are only avalaible in english. 
#' 
#' @param gr_interface Boolean, if \code{TRUE} the program will launch the graphical interface
#' @param login_window Boolean, if \code{TRUE} a login window will be displayed asking the user to specify
#' user name.
#' @param database_expected Boolean, if \code{TRUE} pre launch tests will be run to test the connection validity
#' @usage stacomi(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE)
#' @import stringr
#' @import RColorBrewer
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import RGtk2
#' @import ggplot2
#' @import RPostgreSQL
#' @import sqldf
#' @import methods
#' @import stacomirtools
#' @import RODBC
#' @import RGtk2
#' @import dplyr
#' @import xtable
#' @importFrom intervals Intervals
#' @importFrom intervals closed<-
#' @importFrom intervals interval_overlap
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid grid.newpage
#' @importFrom grid grid.layout
#' @importFrom utils read.csv
#' @importFrom utils stack
#' @importFrom utils globalVariables
#' @importFrom utils select.list write.table data
#' @importFrom stats ftable
#' @importFrom stats xtabs
#' @importFrom grDevices dev.new
#' @importFrom stats sd
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom lattice barchart trellis.par.get trellis.par.set simpleKey
#' @importFrom grid gpar
#' @importFrom graphics layout matplot mtext points polygon segments par axis text legend rect axis.Date abline arrows hist
#' @importFrom stats as.formula coef na.fail nls pbeta predict sd coefficients
#' @importFrom grDevices gray rainbow adjustcolor
#' @importFrom lubridate round_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate %m+%
#' @importFrom mgcv gam
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' 
#'  require(stacomiR)
#' #launch stacomi with the graphical interface
#'  \dontrun{ 	
#' 	stacomi()
#' }
#'  # launch stacomi but do not prompt for password
#'  \dontrun{	
#' 	stacomi(login_window=FALSE)
#' }  
#' #launch stacomi without connection to the database
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
#' @export
stacomi=function(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE){
	# first loading of connection and odbc info using chargexml()
	envir_stacomi <- new.env(parent = emptyenv())
	assign("envir_stacomi",envir_stacomi,.GlobalEnv)
	# trois variables passees a interface graphique via envir_stacomi :
	assign("gr_interface",gr_interface,envir=envir_stacomi)	
	assign("database_expected",database_expected,envir=envir_stacomi)
	assign("login_window",login_window,envir=envir_stacomi)
   # the first messages are necessary for the first access to the database, they are in French
	msg<-messages()
	mylinks=chargecsv(database_expected)
	baseODBC=mylinks[["baseODBC"]]
	datawd=mylinks[["datawd"]]
	lang=mylinks[["lang"]]	
	sqldf.options=mylinks[["sqldf.options"]]	
	# values assigned in the envir_stacomi
	assign("lang",lang,envir=envir_stacomi)	
	assign("datawd",datawd,envir=envir_stacomi)
	assign("sqldf.options",sqldf.options,envir=envir_stacomi)
	
	# the following values may be overridden later in husr()
	assign("baseODBC",baseODBC,envir=envir_stacomi)
	assign("sch",paste(baseODBC[2],".",sep=""),envir=envir_stacomi)
	
	refMsg=new("RefMsg")
	createmessage(refMsg,database_expected)
	
	msg=get("msg",envir=envir_stacomi)
	#libraries()
	options(sqldf.RPostgreSQL.user = sqldf.options["sqldf.uid"], 
			sqldf.RPostgreSQL.password =sqldf.options["sqldf.pwd"],
			sqldf.RPostgreSQL.dbname = sqldf.options["sqldf.dbname"],
			sqldf.RPostgreSQL.host = sqldf.options["sqldf.host"],#  1.100.1.6
			sqldf.RPostgreSQL.port = sqldf.options["sqldf.port"])
	# loginWindow, will call the husr handler
	# user login
	if (gr_interface&login_window&database_expected){
		logw <- gWidgets::gwindow(msg$interface_graphique_log.1, 
				name="log",
				parent=c(0,0),
				width=100,height=100)
		assign("logw",logw,envir=.GlobalEnv)
		logly=gWidgets::glayout(container=logw)
		usrname<- gWidgets::gedit(text = baseODBC[2], 
				width = 10, 
				container = logly)
		assign("usrname",usrname,.GlobalEnv)
		usrpwd<- gWidgets::gedit(text = baseODBC[3], 
				width = 10, 
				container = logly)
		assign("usrpwd",usrpwd,.GlobalEnv)
		but=gWidgets::gbutton(text =  gettext("Login"),
				border=TRUE, 
				handler = husr, 
				container = logly)
		logly[1,1]<-gettext("User")
		logly[2,1]<-gettext("Password")
		logly[1,2]<-usrname
		logly[2,2]<-usrpwd
		logly[3,2]<-but
	} else {
		husr(h=NULL)
	}
	invisible(NULL)
}




#' Program launch, this function launches the GwidgetRgtk graphical
#' interface to stacomi. To be able to run, some widgets (win, grouptotal, group...) 
#' are assigned in the user environment \code{.GlobalEnv}. 
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_graphique=function(){
	msg=get("msg",envir=envir_stacomi) # appel dans chaque sous fonction
	if (exists("graphes"))  rm(list=c("graphes"),envir=.GlobalEnv)
	if (exists("ggroupboutonsbas"))  rm(list=c("ggroupboutonsbas"),envir=.GlobalEnv) 
	if (exists("group"))  rm(list=c("group"),envir=.GlobalEnv)
	if (!file.exists(path.expand(get("datawd",envir=envir_stacomi)))) {
		dir.create(path.expand(get("datawd",envir=envir_stacomi)))
	}
	
	col.sortie=rep(c("pink","purple","red","orange","green","blue","cyan","magenta"),20)  # couleurs pour le texte
	# beware these must be standard colors usable by gWidgets
	assign("col.sortie",col.sortie,.GlobalEnv)
	nbligne=0
	assign("nbligne",nbligne,.GlobalEnv)
	
	win <- gWidgets::gwindow(gettext("Migratory treatment"), name="main",parent=c(0,0),width=100,height=100)
	assign("win",win,envir=.GlobalEnv)
	
	## Menubar is defined by a list
	menubarlist = list()
	
	menubarlist[[gettext("Station")]][[gettext("Fishway")]]$handler =hDF
	menubarlist[[gettext("Station")]][[gettext("Fishway")]]$icon="gWidgetsRGtk2-rarrow"
	menubarlist[[gettext("Station")]][[gettext("Counting Device")]]$handler =hDC
	menubarlist[[gettext("Station")]][[gettext("Counting Device")]]$icon = "gWidgetsRGtk2-plot"
	menubarlist[[gettext("Station")]][[gettext("Operation (TODO)")]]$handler=hOPE
	menubarlist[[gettext("Station")]][[gettext("Operation (TODO)")]]$icon="gtk-cancel"#"gtk-go-forward"
	menubarlist[[gettext("Station")]][[gettext("Fishway without counting device (TODO)")]]$handler=hDFDC
	menubarlist[[gettext("Station")]][[gettext("Fishway without counting device (TODO)")]]$icon="gtk-cancel"
	
	menubarlist[[gettext("Summary")]][[gettext("Migration")]]$handler=hBilanMigration
	menubarlist[[gettext("Summary")]][[gettext("Migration")]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[gettext("Summary")]][[gettext("Multiple migrations")]]$handler=hBilanMigrationMult
	menubarlist[[gettext("Summary")]][[gettext("Multiple migrations")]]$icon="gtk-dnd-multiple"
	menubarlist[[gettext("Summary")]][[gettext("Yearly")]]$handler=hbilA
	menubarlist[[gettext("Summary")]][[gettext("Yearly")]]$icon="gWidgetsRGtk2-barplot"
	menubarlist[[gettext("Summary")]][[gettext("Inter annual migration")]]$handler=hBilanMigrationInterAnnuelle
	menubarlist[[gettext("Summary")]][[gettext("Inter annual migration")]]$icon="gWidgetsRGtk2-hist"
	menubarlist[[gettext("Summary")]][[gettext("")]]$handler=hBilan_carlot
	menubarlist[[gettext("Summary")]][[gettext("")]]$icon="gWidgetsRGtk2-newplot"#"gWidgetsRGtk2-logical"
	
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.2]]$handler=hBilanConditionEnv
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.2]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.3]]$handler=hBilanMigrationConditionEnv
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.3]]$icon="gWidgetsRGtk2-plot1"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.4]]$handler=hBilanMigrationPar
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.4]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.13]]$handler=hBilanEspeces	
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.13]]$icon="gWidgetsRGtk2-polar"#"gWidgetsRGtk2-boxplot"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.7]]$handler=hpds
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.7]]$icon="gWidgetsRGtk2-cloud"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.8]]$handler=hTail
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.8]]$icon="gWidgetsRGtk2-scatterplot3d"#"gWidgetsRGtk2-boxplot"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.9]]$handler=hSt
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.9]]$icon="gWidgetsRGtk2-contour"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.10]]$handler=hsilver
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.10]]$icon="gWidgetsRGtk2-bubbles"
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.11]]$handler=htodo
	menubarlist[[gettext("Summary")]][[msg$interface_graphique_menu.2.11]]$icon="gtk-cancel"

	menubarlist[[gettext("Help")]]$About$handler = hX11
	menubarlist[[gettext("Help")]]$About$icon="newplot"
	menubarlist[[gettext("Help")]]$About$handler = hhelp
	menubarlist[[gettext("Help")]]$About$icon="dialog-info"
	menubarlist[[gettext("Help")]]$lang$handler = hlang
	menubarlist[[gettext("Help")]]$lang$icon="dialog-info"
	gWidgets::add(win, gmenu(menubarlist))
	ggrouptotal<- gWidgets::ggroup(horizontal=FALSE)         # this one stacks from bottom to up
	# gsortie is above the window
	assign("ggrouptotal",ggrouptotal,envir=.GlobalEnv) 
	
	gWidgets::add(win,ggrouptotal)
	
	gSortie=gWidgets::gtext(gettext("Output of the program\n"),width =100 , height = 100,font.attr=list(style="italic", col="blue",family="monospace",sizes="medium"))
	assign("gSortie",gSortie,envir=.GlobalEnv) 
	
	gWidgets::add(ggrouptotal,  gSortie,  expand=FALSE)
# un groupe en dessous mais cette fois horizontal
	ggrouptotal1<- gWidgets::ggroup(horizontal=TRUE) 
	assign("ggrouptotal1",ggrouptotal1,envir=.GlobalEnv) 
	
	gWidgets::add(ggrouptotal,ggrouptotal1,expand=FALSE)
	
# De nouveau un groupe vertical de boutons qui sera pousse a gauche quand le graphique sera insere
	ggroupboutons=gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutons",ggroupboutons,envir=.GlobalEnv)
	
	gWidgets::add(ggrouptotal1,ggroupboutons,expand=FALSE)
}
# Variables used in aes arguments generate a note as being assigned to .GlobalEnv, either use aes_string,
# or listing them below removes the warning in Rcheck. 
utils::globalVariables(c("quinzaine", "mois","val_quant","time.sequence","Effectifs",
				"..density..","Cumsum","Date","Effectif","Effectif_total",
				"annee","car_val_identifiant","car_valeur_quantitatif","coef","date_format",
				"debut_pas","effectif","effectif_CALCULE","effectif_EXPERT","effectif_MESURE","effectif_PONCTUEL",
				"effectif_total","fonctionnement","bilanFonctionnementDF","quantite_CALCULE",
				"quantite_EXPERT","quantite_MESURE","quantite_PONCTUEL","libelle","null","type",
				'val_libelle','lot_effectif','lot_identifiant','ope_dic_identifiant','ope_identifiant','dev_code',
				'dev_libelle','ope_date_fin','bilan_stades_pigm','ope_date_debut','p','g','poids_moyen',
				'taxon_stades','jour',"valeur","mintab","maxtab","moyenne","jour","total_annuel",
				"taxon_stades","time.sequence","sum","variable","duree","Hdeb","Hfin","per_tar_code",
				"per_etat_fonctionnement","std_libelle","sumduree","dc","stade","taxon","stage","ouv"))

# variable used by dplyr
utils::globalVariables(c("n0","newid","xmin","xmax"))

# dataset used in the package
utils::globalVariables(c("coef_Durif"))
# Assignation in global environment for the use of gWidget interface (there is no way arround this)
utils::globalVariables(c("win","group","nbligne","ggrouptotal","ggrouptotal1","gSortie",
				"col.sortie","ggroupboutons","ggroupboutonsbas","groupdate","groupdc","graphes",
				"frame_annee","frame_check","frame_choice","frame_par","frame_parqual","frame_parquan",
				"frame_std","frame_tax","frame_annee","frame_check","frame_choice","refAnnee",
				"logw","bilan_stades_pigm","usrname","usrpwd","notebook","values","ind","progress_bar","progres"))
# Progressbar
utils::globalVariables(c("progres"))
# environment
utils::globalVariables(c("envir_stacomi"))
# reoxygenize fails if data are not loaded
#setwd("F:/workspace/stacomir/branch0.5/stacomir")
#data("bMM_Arzal")
