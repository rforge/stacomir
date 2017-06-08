library(stacomiR)

stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
## launches the application in the command line
## here an example of loading
## not run as the program is possibly not installed
## this example generates the bMM_Arzal dataset
\dontrun{
	stacomi(gr_interface=FALSE,
			login_window=FALSE,
			database_expected=TRUE)	
	bMM_Arzal=new("BilanMigrationMult")
	bMM_Arzal=choice_c(bMM_Arzal,
			dc=c(5,6,12),
			taxons=c("Anguilla anguilla"),
			stades=c("AGG","AGJ","CIV"),datedebut="2011-01-01",datefin="2011-12-31")
	bMM_Arzal<-charge(bMM_Arzal) 
	# launching charge will also load classes associated with the bilan
	# e.g. BilanOperation, BilanFonctionnementDF, BilanFonctionnementDC
	bMM_Arzal<-connect(bMM_Arzal)
}


data("bMM_Arzal")
# The following re-create the object at the time of loading
# All three classes were created by the charge and connect 
# method of BilanMigrationMult in the previous example
data("bilanOperation")
assign("bilanOperation",bilanOperation,envir=envir_stacomi)
data("bilanFonctionnementDF")
assign("bilanFonctionnementDF",bilanFonctionnementDF,envir=envir_stacomi)
data("bilanFonctionnementDC")
assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
# use the following to get the raw data loaded by the connect method
# not shown there as the database and program might not be installed

# calculations
bMM_Arzal<-calcule(bMM_Arzal,silent=TRUE)
#Individual plot for all DC, taxon and stage where data present
#Silent=TRUE to turn off messages
# not run because of multiple graphical devices

plot(bMM_Arzal,plot.type="standard",silent=TRUE)
\dontrun{
# colors in the following order (glass eel)
# working, stopped, 1...5 types of operation,numbers, weight, 2 unused colors
# for yellow eel and other taxa
# stopped, 1...5 types of operation, ponctuel, expert, calcule,mesure,working,
plot(bMM_Arzal,plot.type="standard",
		color=c("#DEF76B","#B950B5","#9ABDDA","#781A74","#BF9D6E","#FFC26E",
				"#A66F24","#012746","#6C3E00","#DC7ED8","#8AA123"),
		color_ope=c("#5589B5","#FFDB6E","#FF996E","#1C4D76"),
		silent=TRUE)


#cumulated migration at the station (all stages and DC grouped)

plot(bMM_Arzal,plot.type="step",silent=TRUE)

#some plots are a mixture of french and english
#changing the reference table names to english should shift all to english.
#same for spanish.
# combined plot for ggplot2
plot(bMM_Arzal,plot.type="multiple",silent=TRUE)
# data will be written in the data directory specified in 
# the stacomi/calcmig.csv file
summary(bMM_Arzal,silent=TRUE)
}
