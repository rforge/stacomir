# Name:        		ggplot2usr.R    (proto)
# Projet :          ggplot interface 
# Author :          Cedric Briand
# mail :            cedric.briand00@gmail.com
# state :         	development
# note : 			This package is intented to be used by beginners who will be able to access to the different layers of the ggplot2 package
#					see http://had.co.nz/ggplot2/
# 					It is yet far from providing the full capability of the ggplot2,
#					but will help in building the first graphs and print their formula 
# 					Note : this package is still in developpement and full of bugs,
#					so far intended only for showing the aim of the project.
#					It has been initially developped within an open source French project, 
#					which builds a database for migratory fishes control stations, 
#					along with several graphical tools to help the users to 'view' their data

#' this function uses gfile, edits a text with info and changing colors
#' @note should be working without stacomi interface as well
#' @param text 
#' @param arret 
#' @param wash 
#' @returnType 
#' @return nblignes assigned in .Global
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
funout<-function(text,arret=FALSE,wash=FALSE){
	if (exists("gSortie")) {
		if (wash) dispose(gSortie)
		nbligne=nbligne+1
		text<-fun_char_spe(text)
		add(gSortie,text,do.newline=FALSE,font.attr=list(style="italic", 
						col=col.sortie[nbligne],family="monospace",sizes="medium"),where="beginning")
		nbligne<<-nbligne
	} 
	# this is printed anyway
	if(arret) stop(text) else print(text)
}
#' Function used to list all elements belonging to a class within an environment
#' @param name the name of the environment
#' @param all.names see ls() for use
#' @param pattern an optional regular expression see ls() for use
#' @param class 
#' @returnType character
#' @return vector containing elements listed as belonging to a class
#' @author Cedric Briand
ls.class<-function(name=.GlobalEnv, all.names = FALSE, pattern,class="data.frame") {
	lst<-ls(name=name,  all.names = FALSE, pattern=pattern)
	lst<-lst[unlist(mapply(function(X) {class(get(X))[1]==class},lst))]
	return (lst)
}

#' Function used to build layer calls and write print their formula
#'  
#' transfers arguments " " into NULL and the others in variable of class call
#' a character vector is also returned to write the proper formula
#' 
#' @param layer_name name of the layer 
#' @param layer_type currently only "geom" or "stat" for layer type object 
#' @param position 
#' @param param 
#' @param aes 
#' @returnType list
#' @return list list("char"=layer expression to be printed, "call" = a call to the layer object
#' @author Cedric Briand
layer_to_call<-function(layer_name,    
		layer_type,                   
		position,
		param,
		aes) {         
	# ex : geomlist  
	if (layer_name==" ") {layer<-NULL
		char<-NULL
	}	else {
		aes_expression<-"aes("
		for (i in 1:length(aes)){
			aes_expression<-paste(aes_expression,names(aes)[i],"=",aes[[i]],",",sep="")
		}
		# replace the last coma by ")"
		substr(aes_expression,nchar(aes_expression),nchar(aes_expression))<-")"
		if (length(param>0)){
			
			for (i in 1:length(param)){
				param_expression<-paste(names(param)[i],"='",param[[i]],"',",sep="")
			}
		param_expression<-substr(param_expression,1,nchar(param_expression)-1)
		param_expression<-paste(",",param_expression,sep="")
		} else param_expression=""
		
		if (layer_type=="geom") {
			layer_call<-do.call(Geom$find(layer_name)$my_name(),args=list("position"=position,"geom_param"=param))
			layer_expression<-ps(Geom$find(layer_name)$my_name(),"(",aes_expression,",",param_expression,"position='",position,"')")
			
		} else if (layer_type=="stat"){
			layer_call<-do.call(Stat$find(layer_name)$my_name(),args=list("position"=position,"param"=param))
			layer_expression<-ps(Stat$find(layer_name)$my_name(),"(",aes_expression,",",param_expression,"position='",position,"')")
		}
	}
	return(list("char"=layer_expression,"eval"=layer_call))
}

#########################
# handler functions
# those are called from gtk2
#########################
# note handlers are programmed to work on an instanciation of Ggploti 
# that will always be called ggi (bad programming there)
# the following function diplays required aes (for which a variable must be given)
# and the default values for other aes 

#' handler function
#' 
#' @param h a handler 
#' @param ... 
#' @author Cedric Briand
hplot=function(h,...){
	ggploti_build()
}
#' Build a list of aes and param by comparing values in the droplist and default and only using those different from default
#' @param list_aes :the list of defautl geom and params written in .RglobalEnv when selecting stat or geom in aes_frame
#' @returnType list
#' @return list of list param and aes containing the values to be used in the layers
#' @author Cedric Briand
extract_aes_param <- function(list_aes) {
	if (!exists("list_aes")) stop ("internal error, no list_aes")
	aes<-list()
	param<-list()
	for (i in 1: length(list_aes)){
		label<-list_aes[[i]]$droplist$label
		val_default<-list_aes[[i]]$edit$value
		val_current_droplist<-list_aes[[i]]$droplist$getValue()
		# first if a value is selected in the droplist, choice goes to aes.
		# the choice of a default label is aes (not related to variable in the data frame) is not possible there
		if (val_current_droplist!=" ") {
			aes[[label]]<-as.name(val_current_droplist)
			# if there is a value selected from column in the droplist, it will override any value in the edit boxes (on the right)
		} else {
			# no droplist filled with required so there is an edit
			val_current_edit<-list_aes[[i]]$edit$getValue()
			if (val_current_edit=="NA") {
				# nothing happens !
			} else if (val_current_edit!="NA" & is.na(val_default)){ 
				# this loop is to avoid NA in the following test
				# some values filled with na do not allow for the following test, but appear as "NA" in the box 
				# here they have been changed so I still display the result
				param[[label]]<-val_current_edit 
			} else if (val_current_edit!=val_default) {
				# else any value in the edit box will be accounted for if it is different from the default value
				# the following transforms character (issued from the edit box) into numeric if possible 
				if(!is.na(as.numeric(val_current_edit))) {
					param[[label]]<-as.numeric(val_current_edit)
				}else { 
					param[[label]]<-val_current_edit
				} # end ifelse  (numeric/character)
			} # end else
		} # end else  
	}
	return(list("aes"=aes,"param"=param))
}
#' constructs the graph and prints the results
#' 
#' @author Cedric Briand 
ggploti_build<-function(){
# building first aesthetics
	list_aes_withoutdefault=extract_aes_param(list_aes = list_aes)
	aes<-list_aes_withoutdefault[["aes"]]
	param<-list_aes_withoutdefault[["param"]]
	AES<-structure(as.list(aes),class="uneval")
	position<-ggi$posBox$getValue()
	group_x<-ggi$groupBox_X$getValue()
	group_y<-ggi$groupBox_Y$getValue()
	if (!exists("p", envir=.GlobalEnv)) {
	p<<-ggplot(ggi$data)
	if (exists("formul")){
	funout(paste(formul,"\n"))
	}
	}
	if (!exists("layerformula",envir=.GlobalEnv)) layerformula=""
	if (stat_is_selected) {	
		stat<-layer_to_call(layer_name=ggi$statBox$getValue(),layer_type="stat",position=position,param=param,aes=aes)
		p1<-p+stat[["eval"]]+AES+facet_grid(as.formula(paste(group_x,"~",group_y)))
		layerformula<-ps(layerformula,'+',stat[["char"]])
		formul<-ps("p",layerformula,"+facet_grid(",paste(group_x,"~",group_y),")")
		funout(paste(formul,"\n"))
	} else {
		geom<-layer_to_call(layer_name=ggi$geomBox$getValue(),layer_type="geom",position=position,param=param,aes=aes) 
		p1<-p+AES+geom[["eval"]]+facet_grid(as.formula(paste(group_x,"~",group_y)))
		layerformula<-ps(layerformula,"+",geom[["char"]])
		formul<-ps("p",layerformula,"+facet_grid(",paste(group_x,"~",group_y),")")
		funout(paste(formul,"\n"))
		
	}
	print(p1)
	#assign("p",p1,envir=envgg)     #       
	if ( svalue(ggi$a$b1)) {  
		p<<-p1
		assign("layerformula",layerformula,envir=.GlobalEnv)
	
	}# build layers is true        
}

#' This function restarts with the default geom, when button Undo or 'Retablir' is pushed
#' @param h handler 
#' @param ... 
#' @author Cedric Briand
hretablir=function(h,...){
	p<<-ggplot(ggi$data)
	p1<-p+AES+layer(geom="point")
	ggi$geomBox$setValue(val="point")
	print(p1)                 
}
#' handler allowing the switch between stat and geom when button geom<>stat is triggered
#' @param h 
#' @param ... 
#' @author Cedric Briand
hsw=function(h,...){
	stat_is_selected <<- ! stat_is_selected
	enabled(ggi$statBox$widget) <- stat_is_selected
	enabled(ggi$geomBox$widget) <- !stat_is_selected
	layer_choice<-ifelse(stat_is_selected,"stat","geom")
	load_aes(layer_choice)          
}

#' Handler called from button col which calls ScaleBrewer not yet functional
#' @param h 
#' @param ... 
#' @author Cedric Briand
#hScaleBrewer =function(h,...){
#	sc= gwindow(title="Echelles de couleur",width=100,height=100)
#	GraphScaleBrewer$display(window=sc)}

#' this handler first analyses the action (which is the name of the BoxinLayout)
#' then calls the load_aes function
#' @param h a handler
#' @param ... 
#' @author Cedric Briand
haes<-function(h,...){
	#print(h$name)
	load_aes(layer_choice=h$name)
}
#' This handler updates the list of data avalaible for drawing the graph
#' If you want for instance to do some calculations in the dataframe and then reload it
#' It uses the add and delete methods applicable to gframe to change the content of the combobox by re-building it
#' @param h handler for gdroplist data 
#' @param ... 
#' @author Cedric Briand
hUpdatedata<-function(h,...){
	delete(ggi$frame_data,ggi$tbl_data)
	add(ggi$frame_data,ggi$tbl_data)
	ggi$dataBox<-Box_in_Layout$proto(vect=ls.class(),name="data",label="data",container=ggi$tbl_data)
	print("the list of data frame has been updated")
}

#' Confirmation dialog gtkwindow when selecting a different dataframe
#' This is directly taken from gWidget vignette and slightly modified
#' @param message 
#' @param handlerok the handler triggered when ok is clicked
#' @param handlercancel the handler triggered when cancel is clicked
#' @author John Verzani
confirmDialog <- function(message, handlerok=NULL,handlercancel=NULL) {
	window <-gwindow("Change data ?",height=7)
	group <- ggroup(container = window)
	gimage("info", dirname="stock", size="dialog", container=group)
	## A group for the message and buttons
	inner.group <- ggroup(horizontal=FALSE, container = group)
	glabel(message, container=inner.group, expand=TRUE)
	## A group to organize the buttons
	button.group <- ggroup(container = inner.group)
	## Push buttons to right
	addSpring(button.group)
	gbutton("ok", handler=handlerok, container=button.group)
	gbutton("cancel", handler = handlercancel,container=button.group)
	return()
}
#' This function will check if data has the same column than previously, if not
#' It will rebuild the graphical interface with the new data
#' @param h a handler for gdroplist data
#' @param ... 
#' @author Cedric Briand
hChangedata=function(h,...){
	newdataname<-ggi$dataBox$getValue()
	data<-ggi$data
	options(warn=-1)
	if (!is.null(newdataname)){
		newdata<-get(newdataname,envir=.GlobalEnv)
		if (colnames(data)!=colnames(newdata)){
			confirmDialog("The columns names have been changed, do you want to reload ggplot2 ? ",
					handlerok <- function(h,...) {
						#in this case the new data frame is used and loaded into the interface
						ggi$data<-newdata
						ggi$dataname<-newdataname
						stat_is_selected <<- FALSE
						enabled(ggi$statBox$widget) <- stat_is_selected
						enabled(ggi$geomBox$widget) <- !stat_is_selected
						layer_choice<-ifelse(stat_is_selected,"stat","geom")
						load_aes(layer_choice)          
						## In this instance dispose finds its parent window and closes it
						dispose(h$obj)
					},
					handlercancel<-function(h,...){
						dispose(h$obj)
					}			
			)			
		}
	} 
	ggi$dataBox$setValue(val=ggi$dataname)
	options(warn=0)
}



#' this functions loads the graphical interface with default elements from layer
#' @param layer_choice 
#' @author Cedric Briand
load_aes<-function(layer_choice){
	if (layer_choice=="geom"){
		grgeom_chosen<-ggi$geomBox$getValue() # the name
		grGeom_chosen<-Geom$find(grgeom_chosen) # the class
		default_aes<-grGeom_chosen$default_aes() # default aesthetics
		required_aes<-grGeom_chosen$required_aes   # required aesthetic
	} else if (layer_choice=="stat"){
		grstat_chosen<-ggi$statBox$getValue() # the name
		grStat_chosen<-Stat$find(grstat_chosen) # the class
		required_aes<-grStat_chosen$required_aes  # required aesthetic
		default_aes<-grStat_chosen$default_aes() # default aesthetics
		# pb no default aes in some stat whereas those are provided in boxplot
		if (length(default_aes)==0) {
			options(show.error.messages = FALSE)
			grGeom_chosen<-try(Geom$find(grstat_chosen))
			if (class(grGeom_chosen)[1] =="proto"){
				default_aes<-grGeom_chosen$default_aes()
				print("geom default used instead of stat defaut")
			} 
		}
		options(show.error.messages = TRUE)
		
	} else stop("internal error, should be stat or geom")
# below it is not possible to replace values in gdroplist so we simply drop the
# layout containing the droplists when change is made
	if ( exists("tbl_aes",.GlobalEnv))    {
		delete(ggi$frame_aes,tbl_aes) 
		rm(tbl_aes,envir= .GlobalEnv)
	}
	tbl_aes<-glayout(homogeneous = FALSE, spacing = 10) # layout that can be deleted
	# function display in  Box_in_Layout or Edit_in_Layout require a glayout as container
	add(ggi$frame_aes,tbl_aes)  # using method add allows for further deletion
	assign("tbl_aes",tbl_aes,.GlobalEnv) # probably not the best but for now will do
	x=1
	list_aes<-list()# just a list to store the gedit and gdroplist objects before display methods are called 
	# for each aes we use both a droplist (with the names of the dataframe) and a gedit with the default values.
	for (req in required_aes){
		list_aes[[x]]<-list()
		list_aes[[x]][["droplist"]]<-Box_in_Layout$proto(vect=colnames(ggi$data),name=req,label=req,container=tbl_aes,handler=NULL)       
		list_aes[[x]][["droplist"]]$display(i=x,j=1)
		x=x+1
	} 
	if (length(default_aes)>0) {           
		for (i in 1:length(default_aes)){
			list_aes[[x]]<-list()
			list_aes[[x]][["droplist"]]<-Box_in_Layout$proto(vect=c(" ",colnames(ggi$data),"..density..","..count..","..scaled.."),name=names(default_aes)[i],label=names(default_aes)[i],container=tbl_aes)
			list_aes[[x]][["droplist"]]$display(i=x,j=1)
			list_aes[[x]][["edit"]]<-Edit_in_Layout$proto(value=unlist(default_aes[i]),name=names(default_aes)[i],label=names(default_aes)[i],container=tbl_aes)
			list_aes[[x]][["edit"]]$display(i=x,j=3)
			x=x+1
		}
	}
	assign("list_aes",list_aes,.GlobalEnv)
}

#########################
# Proto Widgets    (proto objets see proto help file for more explaination)
# this will build the user interface with proto objects
#########################
#' The function build_proto is called to build the proto widgets
#' 
#' @author Cedric Briand
#' @title builds proto widgets
#' @note Unlike S3 or S4 class, the structure of proto objects is lost when the package is built embedded them in a function which will be called to create them. 
#' @references \url{http://wiener.math.csi.cuny.edu/pmg/gWidgets/index.html/} especially ProtoExample
#' @export
build_proto=function(){
	if (exists("tbl_aes",envir=.GlobalEnv)) rm (tbl_aes,envir=.GlobalEnv) # to avoid bugs
	WidgetBase <- proto(expr={
			display = function(.) {
				if(!is.null(.$widget) &&!is.null(.$container)) add(.$container, .$widget, expand=TRUE)
			}
			widget <- NULL
			container <- NULL
			handler<- NULL
			getValue <- function(., ...)  svalue(.$widget)
			setValue <- function (.,val,...)  svalue(.$widget)<-val
			#getIndexedValue <- function(., ...)  svalue(.$widget,index=TRUE),
			#setIndexedValue <- function (.,val,...)  svalue(.$widget,index=TRUE)<-val,
			}
	)
	assign("WidgetBase",WidgetBase,envir=.GlobalEnv)
	
# Builds a droplist dialog type list
	Box_in_Layout=WidgetBase$proto(
			expr={
			vect<-""
			name<-""
			labe<-"" 
			container<-NULL 
			handler<-NULL
			display<-function(.,i,j,...){
				.$widget<-gdroplist(items=.$vect,container=.$container,action = .$name,handler=.$handler)
				.$container[i,j]<-.$label
				.$container[i,j+1,expand = FALSE]<-.$widget     
			}
			}
	)
	assign("Box_in_Layout",Box_in_Layout,envir=.GlobalEnv)
# Builds a gdroplist=gedit dialog type list
	Edit_in_Layout<-WidgetBase$proto(
			expr={
			value<-""
			name<-""
			label<-"" 
			container<-NULL 
			handler<-NULL
			display=function(.,i,j,value,...){
				.$widget<-gedit(text=.$value,width=10,container=.$container,action = .$name,handler=.$handler)
				.$container[i,j,expand = FALSE]<-.$widget     
			}
			}
	)
	assign("Edit_in_Layout",Edit_in_Layout,envir=.GlobalEnv)
# Builds a slider		
#	Slider_in_Layout=WidgetBase$proto(
#			from=0,
#			to=1,
#			by=0.1,
#			value=1,
#			name="",
#			label="" ,
#			handler=NULL,
#			container=NULL,
#			display=function(.,i,j,...){
#				.$widget=gslider(from = .$from, to = .$to, by = .$by, value = .$value, horizontal = TRUE,action = .$name,handler=.$handler)
#				.$container[i,j]<-.$label
#				.$container[i,j+1,expand = FALSE]<-.$widget     
#			}
#	)     
#	assign("Slider_in_Layout",.GlobalEnv)
# Ajoute une case à cocher    
	AddCheck <- proto(
			expr={
			Check <- function(.,...) {              # ... passed to add(,...)
				horizontal = FALSE
				if(.$pos %% 2 == 0) horizontal = TRUE
				g = ggroup(horizontal=.$pos %% 2 == 0, cont = .$container)
				.$b1 <- gcheckbox(.$name, container=g,  action = .)
			}
			pos = 2                             
			container = NULL                     # parent container
			b1 = NULL
		}
	)
	assign("AddCheck",AddCheck,.GlobalEnv)
# ajoute un bouton
	AddBut <- proto(
			expr={
			But <- function(.,...) {              # ... passed to add(,...)
				horizontal <- FALSE
				if(.$pos %% 2 == 0) horizontal <- TRUE
				g <- ggroup(horizontal=.$pos %% 2 == 0, cont = .$container)
				.$b1 <- gbutton(.$name, container=g,  action = .,handler=.$handler)
			}
			pos <- 2                              # 1=S,2=W,3=N,4=E
			container <- NULL                     # parent container
			b1 <- NULL
			name<-NULL
			handler<-NULL
			}
			)
	assign("AddBut",AddBut,envir=.GlobalEnv)
# main device printing

#geomlist=c("point","boxplot","path","line","smooth","quantile","area","density2d","bar","contour","crossbar","errorbar","pointrange","ribbon","quantile","tile","step","text","histogram","density")
#statlist=c("identity","bin", "boxplot", "contour","density","density 2d", "function", "qq", "quantile", "smooth", "spoke", "step ","sum","summary", "unique")
positionlist<-sapply(ggplot2::Position$find_all(),function(X) X$objname)
positionlist<-c(positionlist[positionlist=="identity"],positionlist[positionlist!="identity"])
	Ggploti<-proto(expr= {
				data<-NULL
				dataname<-NULL
				window<-NULL
				geomlist<-sapply(ggplot2::Geom$find_all(),function(X) X$objname)
				statlist<-sapply(ggplot2::Stat$find_all(),function(X) X$objname)
				positionlist<-positionlist
				display=function(.,...) {
					.$tbl<-glayout(homogeneous = FALSE, spacing = 10,container=.$window)
					.$frame_but<-gframe(text="options",horizontal=TRUE,container=.$tbl)
					.$frame_geom<-gframe(text="geom/stat",horizontal=TRUE,container=.$tbl)
					.$tbl_geom<-glayout(homogeneous = FALSE, spacing = 10,container=.$frame_geom)
					.$frame_aes<-gframe(text="aes",horizontal=TRUE,container=.$tbl)
					.$frame_data<-gframe(text="data",horizontal=TRUE,container=.$tbl)
					.$tbl_data<-glayout(homogeneous = FALSE, spacing = 10)
					add(.$frame_data,.$tbl_data)  # using method add allows for further deletion
					.$frame_facets<-gframe(text="layout",horizontal=TRUE,container=.$tbl)
					.$tbl_facets<-glayout(homogeneous = FALSE, spacing = 10,container=.$frame_facets)
					.$frame_position<-gframe(text="position",horizontal=TRUE,container=.$tbl)
					.$tbl_position<-glayout(homogeneous = FALSE, spacing = 10,container=.$frame_position)            			
					.$geomBox<-Box_in_Layout$proto(vect=c(.$geomlist),name="geom",label="geom",container=.$tbl_geom,handler=haes)
					.$statBox<-Box_in_Layout$proto(vect=c(.$statlist),name="stat",label="stat",container=.$tbl_geom,handler=haes)
					.$groupBox_X<-Box_in_Layout$proto(vect=c(".",colnames(ggi$data)),name="group_X",label="lignes",container=.$tbl_facets)
					.$groupBox_Y<-Box_in_Layout$proto(vect=c(".",colnames(ggi$data)),name="group_Y",label="colonnes",container=.$tbl_facets)
					.$posBox<-Box_in_Layout$proto(vect=positionlist,name="position",label="Position",container=.$tbl_position)
					.$dataBox<-Box_in_Layout$proto(vect=ls.class(class="data.frame"),name="data",label="data",container=.$tbl_data,handler=hChangedata)
					.$a <- AddCheck$proto(pos = 4,container = .$frame_but, name="build layers ?")
					.$b <- AddBut$proto(pos=4, container= .$frame_but,name="gtk-revert-to-saved",handler=hretablir)
					.$c<- AddBut$proto(pos=4, container= .$frame_but,name="geom<>stat",handler=hsw)
					#.$d<- AddBut$proto(pos=4, container= .$frame_but,name="ColBrewer",handler=hScaleBrewer)
					.$e<- AddBut$proto(pos=4, container= .$frame_but,name="Graph",handler=hplot)
					.$f<-AddBut$proto(pos=4,container=.$frame_but,name="Data",handler=hUpdatedata)
					.$c$But()
					.$b$But()
					#.$d$But()
					.$a$Check()
					.$e$But()
					.$f$But()
					# boutons en haut
#.$group=ggroup(horizontal=FALSE,use.scrollwindow=FALSE,spacing=4)
					.$tbl[1,1:5]<-glabel("GGPLOT 2 INTERFACE")
					.$tbl[2:3,1:5]<-.$frame_but
					.$tbl[4:5,1:2]<-.$frame_geom
					.$tbl[6:16,1:2]<-.$frame_aes
					.$geomBox$display(i=1,j=1)
					.$statBox$display(i=2,j=1)
					.$tbl[4:5,3:5]<-.$frame_data
					.$dataBox$display(i=1,j=1)
					.$dataBox$setValue(val=dataname)		
					.$tbl[6:7,3:5]<-.$frame_facets			
#			.$tbl[4,3:5]<-"faceting"
					.$groupBox_X$display(i=1,j=1)
					.$groupBox_Y$display(i=2,j=1)
					.$tbl[8:9,3:5]<-.$frame_position			
					.$posBox$display(i=1,j=1)
					stat_is_selected<<-FALSE
					enabled(.$statBox$widget) <- stat_is_selected
					.$geomBox$setValue(val="point")
				}
				new<-function(.,data,window) {
					if (class(data)!="character") stop("data should be a character")
					proto(.,dataname=data,data=get(data,envir=.GlobalEnv),window=window)
				}
			})
	assign("Ggploti",Ggploti,envir=.GlobalEnv)
}	



