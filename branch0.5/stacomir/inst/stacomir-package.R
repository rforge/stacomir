

#' Class "Bilan_lot"
#' 
#' Bilan_lot Bilan class calls the content of the postgres view vue_lot_ope_car
#' 
#' 
#' @name Bilan_lot
#' @aliases Bilan_lot-class Bilan_lot
#' @docType class
#' @note This class is displayed by interface_bilan_lot
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_lot", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi/wiki/Recette%20BilanLot}
#' @keywords classes
#' @examples
#' 
#' showClass("Bilan_lot")
#' object=new("Bilan_lot")
#' 
NULL





#' Class "Bilan_poids_moyen"
#' 
#' Bilan_poids_moyen class The objective is to calculate mean weight of glass
#' eel which are counted from weight measurements and to reintegrate weight to
#' number coefficients
#' 
#' 
#' @name Bilan_poids_moyen-class
#' @aliases Bilan_poids_moyen-class Bilan_poids_moyen
#' @docType class
#' @note We have also tools available to import glass eel measurement from
#' experimental fishing in the estuary For the charge method dates for the
#' request are from august to august (a glass eel season)
#' @section Slots: \describe{ \item{list("data")}{Object of class
#' \code{"data.frame"} data for bilan lot }\item{:}{Object of class
#' \code{"data.frame"} data for bilan lot } \item{list("dc")}{Object of class
#' \code{"RefDC"} refDC an instantiation of the counting device
#' class}\item{:}{Object of class \code{"RefDC"} refDC an instantiation of the
#' counting device class} \item{list("anneedebut")}{Object of class
#' \code{"RefAnnee"} refAnnee allows to choose year of beginning
#' }\item{:}{Object of class \code{"RefAnnee"} refAnnee allows to choose year
#' of beginning } \item{list("anneefin")}{Object of class \code{"RefAnnee"}
#' refAnnee allows to choose year of ending }\item{:}{Object of class
#' \code{"RefAnnee"} refAnnee allows to choose year of ending }
#' \item{list("coe")}{Object of class \code{"RefCoe"} class loading coefficient
#' of conversion between quantity (weights or volumes of glass eel) and numbers
#' }\item{:}{Object of class \code{"RefCoe"} class loading coefficient of
#' conversion between quantity (weights or volumes of glass eel) and numbers }
#' \item{list("liste")}{Object of class \code{"RefListe"} RefListe referential
#' class choose within a list, here do you want subsamples or not
#' }\item{:}{Object of class \code{"RefListe"} RefListe referential class
#' choose within a list, here do you want subsamples or not } }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("Bilan_poids_moyen")
#' object=new("Bilan_poids_moyen")
#' 
NULL





#' Class "Bilan_stades_pigm" Bilan class
#' 
#' The pigment stages analysis has been developed to allow to analyze the
#' change in pigment stage structure for glass eel (Anguilla anguilla).  The
#' class uses the parameters calibrated by Briand et al. , 2005 to
#' backcalculate the probable date when the glass eels arrived in the estuary
#' (i.e. at a fully transparent stage VB. The evolution of pigment stages is
#' modeled with gamma functions which use a pigment time calculated from daily
#' temperatures and salinities.  LThe temperatures has a major influence on the
#' glass eel pigment stage evolution.
#' 
#' 
#' @name Bilan_stades_pigm-class
#' @aliases Bilan_stades_pigm-class Bilan_stades_pigm fntablestade
#' @docType class
#' @note This class is displayed by interface_bilan_stades_pigm, The class uses
#' temperature (from an abiotic measure station) and mean salinity to calculate
#' the change towards one stage
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_stades_pigm", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}} \code{\link{fnstade}}
#' @references BRIAND C., FATIN D., CICCOTTI E. and LAMBERT P., 2005. A
#' stage-structured model to predict the effect of temperature and salinity on
#' glass eel Anguilla anguilla pigmentation development. J Fish Biol, 67,
#' 995-1009.
#' \url{http://www3.interscience.wiley.com/journal/118686679/abstract}
#' \url{http://www.eptb-vilaine.fr/site/index.php/publications-scientifiques/46-publications-migrateurs/60-dynamique-de-population-et-de-migration-des-civelles-en-estuaire-de-vilaine.}
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("Bilan_stades_pigm")
#' 
NULL





#' Class "Bilan_taille" Bilan class
#' 
#' Bilan_taille class This class allows to load the following quantitative
#' sample characteristics : full size, mostly used for yellow eels.
#' 
#' Eels trapped are usually screened and separated into groups, this is not
#' good practise but still in use The groups used in stacomi are based on the
#' selectivity curve according to the mesh, roughly the separation takes place
#' at the L50 for Arzal So individuals might refer to one size class (eg the
#' size group of large eels) This affiliation is sought at the subsample level
#' by a query to display on a single line two different characteristics size
#' and size group Once the data are loaded, the program checks if the size
#' class group is indicated at the sample or susample level
#' 
#' @name Bilan_taille-class
#' @aliases Bilan_taille-class Bilan_taille
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_taille", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("Bilan_taille")
#' 
NULL





#' class BilanConditionEnv simple output of one or several environmental
#' conditions...
#' 
#' Annual overview of environmental conditions. Enables to draw charts and
#' write files.
#' 
#' 
#' @name BilanConditionEnv-class
#' @aliases BilanConditionEnv-class BilanConditionEnv
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanConditionEnv", horodate=new("Horodate"),
#' stationMesure=new("RefStationMesure"), data=data.frame(),
#' requete=new("RequeteODBCwheredate"))}.
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanConditionEnv")
#' 
NULL





#' Class "BilanEspeces" Report of the species present at a counting device for
#' a given period
#' 
#' this class is used to make the assessment of all species, and their number,
#' per month it writes either an histogram or a pie chart of number per
#' year/week/month
#' 
#' 
#' @name BilanEspeces-class
#' @aliases BilanEspeces-class BilanEspeces
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanEspeces", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanEspeces")
#' 
NULL





#' Class "BilanFonctionnementDC" Bilan du fonctionnement du dispositif de
#' comptage
#' 
#' The counting device is not always working. It may me stopped either
#' following a monitoring protocol, or due to misfunction of the device, this
#' class allows to draw graphics allowing an overview of the device operation
#' 
#' 
#' @name BilanFonctionnementDC-class
#' @aliases BilanFonctionnementDC-class BilanFonctionnementDC
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanFonctionnementDC", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanFonctionnementDC")
#' 
NULL





#' Class "BilanFonctionnementDF" Report fishway work
#' 
#' The DF (Dispositif de Franchissement) is a fishway. It may be automated and
#' work only at certain times This report allows to see the detail of its work.
#' 
#' 
#' @name BilanFonctionnementDF-class
#' @aliases BilanFonctionnementDF-class BilanFonctionnementDF
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanFonctionnementDF")}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references ~put references to the literature/web site here ~
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanFonctionnementDF")
#' 
NULL





#' Class "BilanMigration"
#' 
#' Balance of fish migrations
#' 
#' 
#' @name BilanMigration-class
#' @aliases BilanMigration BilanMigration-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigration",
#' dc=new("RefDC"),taxons=("RefTaxon"),stades=("RefStades"),pasDeTemps=("PasDeTempsJournalier"),data=data.frame(),
#' duree=new(POSIXct) )}.  \describe{ \item{list("dc")}{Object of class
#' \code{"RefDC"}: the control device }\item{:}{Object of class \code{"RefDC"}:
#' the control device } \item{list("taxons")}{Object of class
#' \code{"RefTaxon"}: the taxa of the fish}\item{:}{Object of class
#' \code{"RefTaxon"}: the taxa of the fish} \item{list("stades")}{Object of
#' class \code{"RefStades"} : the stage of the fish}\item{:}{Object of class
#' \code{"RefStades"} : the stage of the fish} \item{list("pasDeTemps")}{Object
#' of class \code{"PasDeTempsJournalier"} : the time step constrained to daily
#' value and 365 days}\item{:}{Object of class \code{"PasDeTempsJournalier"} :
#' the time step constrained to daily value and 365 days}
#' \item{list("data")}{Object of class \code{"data.frame"} :
#' data}\item{:}{Object of class \code{"data.frame"} : data}
#' \item{list("duree")}{Object of class \code{"POSIXct"} : duration of the
#' analysis}\item{:}{Object of class \code{"POSIXct"} : duration of the
#' analysis} }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanMigration")
#' bilanMigration= new("BilanMigration")
#' 
NULL





#' Function to calculate statistics per month or per day within class
#' \code{\linkS4class{BilanMigration}}
#' 
#' \code{funstat} will calculate statistics per month \code{funstatJournalier}
#' calculates daily values \code{funtable} function to print and save
#' statistics in .csv and .html formats \code{funtraitement_poids} returns a
#' table where weights and number are calculated from number and weights
#' respectively. Performs a query to collect the conversion coefficients, this
#' function is used for glass eel which are most often weighted, not counted
#' 
#' 
#' @aliases funstat funstatJournalier funtable funtraitement_poids
#' @param tableau the table generated by \code{funSousListeBilanMigration} in
#' class \code{\linkS4class{BilanMigration}}
#' @param duree a POSIXt sequence generated for the range of the period
#' selected
#' @param taxon the latin name of the selected taxa
#' @param stade the stage selected
#' @param DC the code of the counting device
#' @param resum A monthly summary of data generated by \code{funstat}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' Class "BilanMigrationConditionEnv"
#' 
#' Enables to compute an annual overview of fish migration and environmental
#' conditions in the same chart
#' 
#' 
#' @name BilanMigrationConditionEnv-class
#' @aliases BilanMigrationConditionEnv BilanMigrationConditionEnv-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationConditionEnv",
#' bilanMigration=new("BilanMigration"),
#' bilanConditionEnv=new("BilanConditionEnv"))}.  \describe{
#' \item{list("bilanMigration")}{Object of class \code{"BilanMigration"} The
#' migration overview }\item{:}{Object of class \code{"BilanMigration"} The
#' migration overview } \item{list("bilanConditionEnv")}{Object of class
#' \code{"BilanConditionEnv"} The environmental overview}\item{:}{Object of
#' class \code{"BilanConditionEnv"} The environmental overview} }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanMigrationConditionEnv")
#' 
NULL





#' Class "BilanMigrationInterAnnuelle"
#' 
#' Several year overview of fish migrations. It enables to compare migration
#' for one year with statistics (min, max, mean) from several year, and to
#' extract data This class need prior call from bilanMigration to fill in the
#' t_bilanmigrationjour_bjo as it loads the data in this table
#' 
#' 
#' @name BilanMigrationInterAnnuelle-class
#' @aliases BilanMigrationInterAnnuelle-class BilanMigrationInterAnnuelle
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationInterAnnuelle", dc=new("RefDC"), data=data.frame(),
#' taxons=new("RefTaxon"),stades=new("RefStades"), anneeDebut=new("RefAnnee"),
#' anneeFin=new("RefAnnee")) }.
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanMigrationInterAnnuelle")
#' 
NULL





#' Class BilanMigrationPar, Migration report along with quantitative and
#' qualitative characteristics
#' 
#' Migration along with qualitative or quantitative characteristics or both
#' (e.g.) weight of eels according to the size class per period of time, weight
#' of fish according to gender...
#' 
#' 
#' @name BilanMigrationPar-class
#' @aliases BilanMigrationPar-class BilanMigrationPar
#' @docType class
#' @note program : default two parameter choice, checking box "aucun" will
#' allow the program to ignore the parameter
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationPar", ...)}.  they are loaded by the interface
#' using interface_BilanMigrationPar function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @keywords classes dynamic
#' @examples
#' 
#' showClass("BilanMigrationPar")
#' 
NULL





#' Methods for Function calcule
#' 
#' calcule is called when a class needs computation and not only data collected
#' from the database, it sometimes does the work of the charge method
#' 
#' 
#' @name calcule-methods
#' @aliases calcule-methods calcule calcule,BilanMigration-method
#' calcule,BilanMigrationConditionEnv-method calcule,BilanMigrationPar-method
#' calcule,Bilan_taille-method calcule,Bilan_lot-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(object =
#' \"BilanMigration\")")}{ to be refined, performs both query and calculation,
#' the calcul calls for \link{funBilanMigrationAnnuel} and
#' \link{funSousListeBilanMigration} } \item{list("signature(object =
#' \"BilanMigrationConditionEnv\")")}{ collects object assigned to environement
#' \code{envir_stacomi} and loads the \link{BilanConditionEnv} }
#' \item{list("signature(object = \"BilanMigrationPar\")")}{collects object
#' assigned to environment \code{envir_stacomi} and checks for validity}
#' \item{list("signature(object = \"BilanTaille\")")}{calls charge, does some
#' computation on samples and subsamples and calls function funtraitementdate}
#' \item{list("signature(object = \"Bilan_Lot\")")}{performs all calculations
#' before use in graphical functions \code{fundensityBilan_lot}
#' \code{funboxplotBilan_lot} \code{funpointBilan_lot}} }
#' @keywords methods manip
NULL





#' Charge method for referential (ref) and report (bilan) object
#' 
#' This methods loads the data, it is in fact sometimes redundant with connect
#' which should be reserved for instances of RequeteODBC and daughter classes.
#' See individual .r files for help and examples
#' 
#' 
#' @name charge-methods
#' @aliases charge-methods charge charge,Bilan_lot-method
#' charge,Bilan_poids_moyen-method charge,Bilan_stades_pigm-method
#' charge,Bilan_taille-method charge,BilanConditionEnv-method
#' charge,BilanEspeces-method charge,BilanFonctionnementDC-method
#' charge,BilanFonctionnementDF-method
#' charge,BilanMigrationInterAnnuelle-method charge,RefAnnee-method
#' charge,RefCheckBox-method charge,RefChoix-method charge,RefCoe-method
#' charge,RefDC-method charge,RefDF-method charge,RefListe-method
#' charge,Refpar-method charge,Refparqual-method charge,Refparquan-method
#' charge,RefPoidsMoyenPeche-method charge,RefStades-method
#' charge,RefStationMesure-method charge,RefTaxon-method
#' charge,RefTextBox-method charge,RefMsg-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"Bilan_lot\")")}{ loading method for
#' Bilan_lot class }
#' 
#' \item{list("signature(object = \"Bilan_poids_moyen\")")}{ loading method for
#' Bilan_poids_moyen class }
#' 
#' \item{list("signature(object = \"Bilan_stades_pigm\")")}{loading method for
#' Bilan_stades_pigm class }
#' 
#' \item{list("signature(object = \"Bilan_taille\")")}{ loading method for
#' Bilan_taille }
#' 
#' \item{list("signature(object = \"BilanConditionEnv\")")}{ loading method for
#' BilanCondtionEnv class }
#' 
#' \item{list("signature(object = \"BilanEspeces\")")}{ loading method for
#' BilanEspeces verifies the content of objects and calls the connect method }
#' 
#' \item{list("signature(object = \"BilanFonctionnementDC\")")}{ loading method
#' for BilanFonctionnementDC class }
#' 
#' \item{list("signature(object = \"BilanFonctionnementDF\")")}{ loading method
#' for BilanFonctionnementDF class }
#' 
#' \item{list("signature(object = \"BilanMigrationInterAnnuelle\")")}{ loading
#' method for BilanMigrationInterannuelle class }
#' 
#' \item{list("signature(object = \"RefAnnee\")")}{ Loading method for RefAnnee
#' referential objects, selects year avalaible in the t_operation_ope table }
#' 
#' \item{list("signature(object = \"RefCheckBox\")")}{ Loading method for
#' ReCheckBox referential objects}
#' 
#' \item{list("signature(object = \"RefChoix\")")}{ Loading method for Rechoix
#' referential objects}
#' 
#' \item{list("signature(object = \"RefCoe\")")}{ loads the coefficients for the
#' period defined in class The slots datedebut and datefin have to be filled
#' before using charge }
#' 
#' \item{list("signature(object = \"RefDC\")")}{ loads the counting devices of
#' the control station }
#' 
#' \item{list("signature(object = \"RefDF\")")}{ Loading method for DF
#' referential objects}
#' 
#' \item{list("signature(object = \"RefListe\")")}{ Loading method for Refliste
#' referential objects}
#' 
#' \item{list("signature(object = \"Refpar\")")}{ Loading method for Repar
#' referential objects }
#' 
#' \item{list("signature(object = \"Refparqual\")")}{ Loading method for
#' Reparqual referential objects }
#' 
#' \item{list("signature(object = \"Refparquan\")")}{ Loading method for
#' Reparquan referential objects }
#' 
#' \item{list("signature(object = \"RefPoidsMoyenPeche\")")}{ Loading method for
#' RefPoidsMoyenPeche referential objects}
#' 
#' \item{list("signature(object = \"RefStades\")")}{ Loading method for
#' RefStades referential objects}
#' 
#' \item{list("signature(object = \"RefStationMesure\")")}{ Loading method for
#' RefStationMesure referential object }
#' 
#' \item{list("signature(object = \"RefTaxon\")")}{ Loading method for RefTaxon
#' referential objects }
#' 
#' \item{list("signature(object = \"RefTextBox\")")}{ Loading method for
#' ReTextBox referential objects }
#' 
#' \item{list("signature(object = \"RefMsg\")")}{ Loading method for RefMsg
#' referential objects loads the common table ts_messager_msr } }
#' @keywords methods database
NULL





#' Methods for Function charge_avec_filtre
#' 
#' Methods for function \code{charge_avec_filtre}
#' 
#' 
#' @name charge_avec_filtre-methods
#' @aliases charge_avec_filtre-methods charge_avec_filtre
#' charge_avec_filtre,Refpar-method charge_avec_filtre,Refparqual-method
#' charge_avec_filtre,Refparquan-method charge_avec_filtre,RefStades-method
#' charge_avec_filtre,RefTaxon-method charge_avec_filtre,RefMsg-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"Refpar\")")}{ Loading method for Repar
#' referential objects searching only those parameters existing for a DC, a
#' Taxon, and a stade}
#' 
#' \item{list("signature(object = \"Refparqual\")")}{ Loading method for
#' Reparqual referential objects searching only those parameters existing for a
#' DC, a Taxon, and a stade }
#' 
#' \item{list("signature(object = \"Refparquan\")")}{ Loading method for
#' Reparquan referential objects searching only those parameters existing for a
#' DC, a Taxon, and a stade }
#' 
#' \item{list("signature(object = \"RefStades\")")}{ Loading method for
#' RefStades referential objects searching only those stages existing for a DC
#' and a Taxon}
#' 
#' \item{list("signature(object = \"RefTaxon\")")}{ Loading method for RefTaxon
#' referential objects searching only those stages existing for a DC and a
#' Taxon }
#' 
#' \item{list("signature(object = \"RefMsg\")")}{ Loading method for RefMsg
#' referential objects searching ref.ts_messagerlang_mrl for the lines
#' corresponding to lang, the language chosen for the interface} }
#' @keywords methods database
NULL





#' Methods for Function chargecomplement
#' 
#' Methods for function \code{chargecomplement} method chargecomplement this
#' method is called after selecting the object (data only counts one line) and
#' allows a request to obtain a complement, for instance possible values for a
#' qualitative parameter
#' 
#' 
#' @name chargecomplement-methods
#' @aliases chargecomplement chargecomplement-methods
#' chargecomplement,Refparqual-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"Refparqual\")")}{collexts possible values
#' for a qualitative parameter, for instance for parameter sex would be values
#' male & female } }
#' @keywords methods database
#' @examples
#' 
#' \dontrun{
#' dc_selectionne=6
#' taxon_selectionne=2038
#' stade_selectionne="AGJ"
#' object=new("Refparqual")
#' object<-charge(object)
#' chargecomplement(object)
#' }
#' 
NULL





#' Methods for Function choix
#' 
#' These methods are used within the graphical interface to load the
#' referential elements of each class (ex Reftaxon) and copy the user choice in
#' the environment "envir_stacomi"
#' 
#' 
#' @name choix-methods
#' @aliases choix-methods choix choix,PasDeTemps-method
#' choix,PasDeTempsJournalier-method choix,RefAnnee-method
#' choix,RefCheckBox-method choix,RefChoix-method choix,RefDC-method
#' choix,RefDF-method choix,RefHorodate-method choix,RefListe-method
#' choix,Refpar-method choix,Refparqual-method choix,RefStades-method
#' choix,RefStationMesure-method choix,RefTaxon-method choix,RefTextBox-method
#' @docType methods
#' @param objectBilan un object bilan
#' @param is.enabled a boolean indincating # see if deprecated
#' @param nomassign the name to be asssigned in envir_stacomi
#' @param funoutlabel the label that appears in funout
#' @param titleFrame title for the frame
#' @param preselect the number of the year selected in the gdroplist (integer)
#' @note The choice method has for arguments a report (bilan) object (e.g) is
#' called from a report Bilan(e.g Bilan_lot).  By default, the value of the
#' objectbilan is null.  When it is not the method calls the daughter widgets
#' (e.g. the dc widget will call species) and fills it with the method
#' \code{\link{charge_avec_filtre-methods}} \enumerate{ \item
#' \code{signature(object = "RefDC")}the choice method assigns an object of
#' class refDF in the environment envir_stacomi.  \item \code{signature(object =
#' "RefStades")} the method tests if the load is called from within a "bilan"
#' object, and loads par, parqual, or parquan objects accordingly.  \item
#' \code{signature(object = "RefPar")} the choice method assigns an object of
#' class Refpar named refpar in the environment envir_stacomi \item
#' \code{signature(object = "RefParquan")} this method choix is also on sons
#' objects Refparquan, hence the parameters,however it was redefined in
#' refparqual to load the possible values of qualitative parameters \item
#' \code{signature(object = "RefParqual")} the choice method assigns an object
#' of class Refparqual named refparqual in the environment envir_stacomi this
#' method rewrites the method from Refpar, as it integrates a request of the
#' possible values of qualitative parameters, hence the parameters, however it
#' was redefined in refparqual to load the possible values of qualitative
#' parameters.  \item \code{signature(object = "RefList")} the choice method
#' assigns an object of class refList named refListe in the environment
#' envir_stacomi }
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"PasDeTemps\")")}{ choice method for class
#' PasdeTemps, allows to choose the timestep }
#' 
#' \item{list("signature(object = \"PasDeTempsJournalier\")")}{ choice method
#' for daily timestep }
#' 
#' \item{list("signature(object = \"RefAnnee\")")}{ choice method for RefAnnee
#' referential objects assign the object in envir_stacomi }
#' 
#' \item{list("signature(object = \"RefCheckBox\")")}{ choice method for
#' RefCheckBox referential objects }
#' 
#' \item{list("signature(object = \"RefChoix\")")}{ Choice method for Refchoix
#' referential objects }
#' 
#' \item{list("signature(object = \"RefDC\")")}{ choice method for RefDC }
#' 
#' \item{list("signature(object = \"RefDF\")")}{ Choice method for DF
#' referential objects }
#' 
#' \item{list("signature(object = \"RefHorodate\")")}{ Choice method for
#' RefHorodate referential objects }
#' 
#' \item{list("signature(object = \"RefListe\")")}{ Choice method for RefListe
#' referential objects }
#' 
#' \item{list("signature(object = \"Refpar\")")}{ Choice method for Refpar
#' referential objects }
#' 
#' \item{list("signature(object = \"Refparqual\")")}{ Choice method for
#' Refparqual referential objects }
#' 
#' \item{list("signature(object = \"RefStades\")")}{ Choice method for RefStades
#' referential objects }
#' 
#' \item{list("signature(object = \"RefStationMesure\")")}{ Choice method for
#' RefStationMesure referential object }
#' 
#' \item{list("signature(object = \"RefTaxon\")")}{ Choice method for Reftaxon
#' referential objects }
#' 
#' \item{list("signature(object = \"RefTextBox\")")}{ Choice method for
#' ReTextBox referential objects } }
#' @keywords methods dynamic
NULL





#' Methods for Function coerce
#' 
#' Methods for function \code{coerce} ie inherited from a mother class
#' 
#' 
#' @name coerce-methods
#' @aliases coerce-methods
#' coerce,BilanMigration,BilanMigrationInterAnnuelle-method
#' coerce,PasDeTempsChar,PasDeTemps-method
#' coerce,RequeteODBCwhere,RequeteODBC-method
#' coerce,RequeteODBCwheredate,RequeteODBCwhere-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(from = \"BilanMigration\", to =
#' \"BilanMigrationInterAnnuelle\")")}{ }
#' 
#' \item{list("signature(from = \"PasDeTempsChar\", to = \"PasDeTemps\")")}{ }
#' 
#' \item{list("signature(from = \"RequeteODBCwhere\", to = \"RequeteODBC\")")}{
#' }
#' 
#' \item{list("signature(from = \"RequeteODBCwheredate\", to =
#' \"RequeteODBCwhere\")")}{ } }
#' @keywords methods
NULL





#' connect method for referential and bilan class
#' 
#' see individual .r files for help and examples connect method loads a request
#' to the database and returns either an error or a data.frame
#' 
#' 
#' @name connect-methods
#' @aliases connect,RequeteODBC-method connect,RequeteODBCwhere-method
#' connect,RequeteODBCwheredate-method connect,BilanConditionEnv-method
#' connect,BilanEspeces-method connect,BilanFonctionnementDC-method
#' connect,BilanFonctionnementDF-method
#' connect,BilanMigrationInterAnnuelle-method connect,Bilan_lot-method
#' connect,Bilan_poids_moyen-method connect,Bilan_stades_pigm-method
#' connect,Bilan_taille-method
#' @docType methods
#' @section Methods:
#' 
#' \describe{
#' 
#' \item{list("signature(object = \"BilanMigrationInterannuelle\")")}{}
#' 
#' \item{list("signature(object = \"BilanConditionEnv\")")}{}
#' 
#' \item{list("signature(object = \"Bilan_stades_pigm\")")}{will try to get data
#' for the temperature (refstation) only if retrocalcul is checked by default
#' it is note when launching}
#' 
#' \item{list("signature(object = \"Bilan_taille\")")}{}
#' 
#' \item{list("signature(object = \"Bilan_Poids_Moyen\")")}{}
#' 
#' \item{list("signature(object = \"BilanEspeces\")")}{} }
#' @keywords methods
NULL





#' createmessage method for RefMsg referential objects
#' 
#' createmessage method for RefMsg referential objects
#' 
#' 
#' @name createmessage
#' @aliases createmessage createmessage-method
#' @docType methods
#' @return An S4 object of class RefMsg
#' @note When coming from the database, " are now /", those at the beginning
#' and end are turned into ", the others are single quote when they are to be
#' pasted within the text as code example. The remainder "c("a","b","c") are
#' rebuilt into vectors by the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' Methods for Function getvalue
#' 
#' Methods for class Refperiod, returns the POSIXt value of a given name
#' \code{data=data.frame("id"=c("jour","semaine","quinzaine","mois"),
#' "pgval"=c("day","week","2 week","month"))}
#' 
#' 
#' @name getvalue-methods
#' @aliases getvalue getvalue-methods getvalue,Refperiode-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(object =
#' \"Refperiode\")")}{ Returns the POSIXt value of a given name }}
#' @keywords methods
NULL





#' Methods for Function graphe
#' 
#' method with a parameter choix to join several graphes called by several
#' handlers (buttons)
#' 
#' 
#' @name graphe-methods
#' @aliases graphe-methods graphe graphe,BilanMigrationPar-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(object =
#' \"BilanMigrationPar\")")}{ method with a parameter choix to join several
#' graphes called by several handlers (buttons) } }
#' @keywords methods
NULL





#' function called by handler which displays a graph
#' 
#' The handler functions are linked to S4 classes, but due to gwidget
#' programming cannot be programmed as S4 methods \enumerate{ \item
#' \code{\linkS4class{BilanConditionEnv}} \code{hbilanConditionEnvgraph}
#' function called by handler which displays a graph if environmental
#' conditions are in the database during the selected period \item
#' \code{\linkS4class{BilanFonctionnementDC}}\code{funbarchartDC} a barchart
#' showing the work of a monitoring device \item
#' \code{\linkS4class{BilanFonctionnementDF}}\code{funbarchartDF} a barchart
#' showing the work of the fishway \item
#' \code{\linkS4class{BilanFonctionnementDC}}\code{funboxDC} draws rectangles
#' to show the work of a monitoring device \item
#' \code{\linkS4class{BilanFonctionnementDF}}\code{funboxDF} draws rectangles
#' to show the work of a fishway \item
#' \code{\linkS4class{BilanEspeces}}\code{hCamembert} builds pie charts showing
#' the species present for a given period \item
#' \code{\linkS4class{BilanEspeces}}\code{hHistogramme} builds and histogram
#' showing the species present for a given period \item
#' \code{\linkS4class{BilanMigrationConditionEnv}}\code{hbilanMigrationConditionEnvgraph}
#' graph of both environmental conditions and migrations \item
#' \code{\linkS4class{BilanMigration}}\code{hbilanMigrationgraph} calls the
#' function \code{fungraph}to plot the results of bilanmigration and saves
#' daily migrations into the database \item
#' \code{\linkS4class{BilanMigration}}\code{hbilanMigrationgraph2} calls
#' function \code{fungraph} and then draws a calculation of cumulated migration
#' along the year, this plot might help seeing changes in seasonal migration
#' patterns \item
#' \code{\linkS4class{BilanMigrationPar}}\code{hbilanMigrationPargraph} Calls
#' the generic method \code{graphe,BilanMigrationPar-method} with choix=1 \item
#' \code{\linkS4class{BilanMigrationPar}}\code{hbilanMigrationPargraph2} Calls
#' the generic method \code{graphe,BilanMigrationPar-method} with choix=2 \item
#' \code{\linkS4class{Bilan_stades_pigm}}\code{hfungraphstades} Calls the
#' function \code{fungraphstades} \item
#' \code{\linkS4class{Bilan_lot}}\code{fundensityBilan_lot} uses ggplot to draw
#' ribbon plots
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi/wiki/Recette%20BilanLot}
#' \item \code{\linkS4class{Bilan_lot}}\code{funBoxplotBilan_lot} uses ggplot
#' to create a boxplot, help in
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi/wiki/Recette%20BilanLot}
#' \item \code{\linkS4class{Bilan_lot}}\code{funpointBilan_lot} uses ggplot to
#' draw dot plots
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi/wiki/Recette%20BilanLot}
#' 
#' \item \code{\linkS4class{Bilan_taille}}\code{fungraphInteract_tail}
#' interactive plot for size \item
#' \code{\linkS4class{Bilan_stades_pigm}}\code{fungraphgg} interactive plot for
#' ggplot2 \item \code{\linkS4class{Bilan_stades_pigm}}\code{hfungraphstades}
#' interactive plot for ggplot2
#' 
#' }
#' 
#' 
#' @aliases hbilanConditionEnvgraph hbilanEspeces funbarchartDC funbarchartDF
#' fundensityBilan_lot funboxplotBilan_lot funpointBilan_lot funboxBilan_lot
#' funboxDC funboxDF hCamembert hHistogramme hbilanMigrationConditionEnvgraph
#' hbilanMigrationgraph hbilanMigrationgraph2 hbilanMigrationPargraph
#' hbilanMigrationPargraph2 hfungraphstades fungraphgg fungraphInteract_tail
#' hgraphBilanMigrationInterAnnuelle hgraphBilanMigrationInterAnnuelle2
#' hgraphBilanMigrationInterAnnuelle3 hgraphBilanMigrationInterAnnuelle4
#' hgraphBilanMigrationInterAnnuelle5 hgraphBilanMigrationInterAnnuelle7
#' @param h the handler
#' @param \dots additional arguments passed to the function
#' @note \enumerate{ \item \code{\linkS4class{BilanEspeces}}\code{hCamembert}
#' <0 numbers are transformed into positive for counts \item
#' \code{\linkS4class{BilanMigration}}\code{hbilanMigrationgraph} this function
#' has been developped with daily time step and will not adapt to larger ones,
#' the function calls \code{calcule,BilanMigration-method} and then for glass
#' eel will call \code{\link{fungraph_civelle}} as there is the special case of
#' weight being measured there instead of numbers, for other species will call
#' \code{\link{fungraph}}
#' 
#' }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' handler function used by the main interface to call bilan classes
#' 
#' Handler functions used by interface_graphique the main interface
#' 
#' 
#' @aliases hbilanMigration hBilanEspeces hBilanConditionEnv hBilanLots
#' hBilanMigrationPar hDC hOPE hDFDC hBilanMigration
#' hBilanMigrationInterAnnuelle hBilanMigrationConditionEnv hDF hhelp hpds hSt
#' hTail htodo hlang husr hx11
#' @param h the handler
#' @param \dots additional arguments passed to the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' function called by handler which do the main calculations for a Bilan class
#' 
#' The handler functions are linked to S4 classes, but due to gwidget
#' programming cannot be programmed as S4 methods function called by handler
#' which perform calculations. See each bilan class for detail \enumerate{
#' \item \code{\linkS4class{BilanMigration}} \code{hbilanMigrationcalc}
#' calculates the migration report, write it in \env{envir_stacomi} and might
#' treat apart sample quantities(If taxa is eel and stage is glass eel) \item
#' \code{\linkS4class{BilanEspeces}} \code{hBilanEspecescalc(h, \dots{})} \item
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{hbilanMigrationConditionEnvcalc(h, \dots{})} handler for graph
#' calculates migration report with environmental conditions, write it in
#' envir_stacomi \item \code{\linkS4class{BilanMigrationPar}}
#' \code{hbilanMigrationParcalc(h, \dots{})} }
#' 
#' 
#' @aliases hbilanMigrationcalc calchBilanEspecescalc
#' hbilanMigrationConditionEnvcalc hbilanMigrationParcalc hBilanEspecescalc
#' hcalculeBilanTaille
#' @param h the handler
#' @param \dots additional arguments passed to the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' function called by handler which display statistical results
#' 
#' The handler functions are linked to S4 classes, but due to gwidget
#' programming cannot be programmed as S4 methods function called by handler
#' which display table or stats either as .csv or html the stats are stored in
#' the \strong{Calcmig} folder which location can be changed by changing the
#' calcmig.xml function in the \file{c:\program files\stacomi} folder
#' \enumerate{ \item \code{\linkS4class{BilanConditionEnv}}
#' \code{hbilanConditionEnvstat} \item \code{\linkS4class{BilanMigrationPar}}
#' \code{hbilanMigrationParstat} Calls the generic method
#' \code{graphe,BilanMigrationPar-method} with choix=3 \item
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{htableBilanMigrationInterAnnuelle} \item
#' \code{\linkS4class{BilanEspeces}} \code{hTableBilanEspeces} \item
#' \code{\linkS4class{BilanMigration}} \code{hTableBilanMigration} calls
#' functions \code{funstat} and \code{funtable} to perform calculations and
#' store them in .csv and .html files \item
#' \code{\linkS4class{BilanFonctionnementDC}} \code{funtableDC} calls functions
#' \code{funstat} and \code{funtable} to perform calculations and store them in
#' .csv and .html files \item \code{\linkS4class{BilanFonctionnementDF}}
#' \code{funtableDF} calls functions \code{funstat} and \code{funtable} to
#' perform calculations and store them in .csv and .html files \item
#' \code{\linkS4class{Bilan_taille}} \code{funtableBilan_tail} \item
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{funtableBilan_poids_moyen}function for showing table in gdf \item
#' \code{\linkS4class{Bilan_lot}} \code{funtableBilan_lot}function for showing
#' table in gdf }
#' 
#' 
#' @aliases hbilanConditionEnvstat hbilanMigrationParstat
#' htableBilanMigrationInterAnnuelle hTableBilanEspeces hTableBilanMigration
#' hbilanMigrationstat funtableDC funtableDF funtableBilan_tail
#' funtableBilan_poids_moyen funtableBilan_lot
#' @param h the handler
#' @param \dots additional arguments passed to the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' An interface that calls the object to build the user interface, one for each
#' class
#' 
#' An interface that calls the object to build the user interface
#' 
#' 
#' @aliases interface_Bilan_stades_pigm interface_BilanEspeces
#' interface_BilanFonctionnementDC interface_BilanFonctionnementDF
#' interface_BilanLot interface_BilanMigration
#' interface_BilanMigrationConditionEnv interface_BilanMigrationInterAnnuelle
#' interface_BilanMigrationPar interface_BilanPoidsMoyen interface_BilanTaille
#' interface_chooselang interface_ConditionEnv
#' @note always has to be called within a group constructed and deleted using
#' quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
NULL





#' Class "PasDeTemps"
#' 
#' Describes a time step
#' 
#' 
#' @name PasDeTemps-class
#' @aliases PasDeTemps PasDeTemps-class currentDateDebut,PasDeTemps-method
#' currentDateDebut currentDateFin,PasDeTemps-method currentDateFin
#' DateFin,PasDeTemps-method DateFin LesPasDeTemps ValeurPasDeTemps
#' getAnnees,PasDeTemps-method getAnnees getdateDebut,PasDeTemps-method
#' getdateDebut getLibellesPas,PasDeTemps-method getLibellesPas
#' getnoPasCourant,PasDeTemps-method getnoPasCourant
#' setdateDebut,PasDeTemps-method setdateDebut suivant,PasDeTemps-method
#' suivant validite_PasDeTemps dureePas
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PasDeTemps",
#' dateDebut="POSIXt",dureePas=numeric(),nbPas=numeric(),noPasCourant=integer())}.
#' \describe{ \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }\item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("dureePas")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nbPas")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("noPasCourant")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{PasDeTempsJournalier}}
#' @keywords classes
#' @examples
#' 
#' showClass("PasDeTemps")
#' 
NULL





#' Class "PasDeTempsChar"
#' 
#' Character to represent a PasDeTemps
#' 
#' 
#' @name PasDeTempsChar-class
#' @aliases PasDeTempsChar PasDeTempsChar-class validite_PasDeTempsChar
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PasDeTempsChar", \dots{})}
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{PasDeTemps}}
#' @keywords classes
#' @examples
#' 
#' showClass("PasDeTempsChar")
#' 
NULL





#' Class "PasDeTempsJournalier"
#' 
#' Representation of a PasDeTemps object with a step length equal to one day.
#' It receives an heritance from PasDeTemps
#' 
#' 
#' @name PasDeTempsJournalier-class
#' @aliases PasDeTempsJournalier PasDeTempsJournalier-class
#' validite_PasDeTempsJournalier
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PasDeTempsJournalier",
#' dateDebut="POSIXt",dureePas=numeric(),nbPas=numeric(),noPasCourant=integer())}.
#' \describe{ \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }\item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("dureePas")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nbPas")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("noPasCourant")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{PasDeTemps}}
#' @keywords classes
#' @examples
#' 
#' showClass("PasDeTempsJournalier")
#' 
NULL





#' Class "RefAnnee"
#' 
#' RefAnnee referential class to choose years
#' 
#' 
#' @name RefAnnee-class
#' @aliases RefAnnee-class RefAnnee
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefAnnee", data=data.frame(), annee_selectionnee=numeric())}.
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @examples
#' 
#' showClass("RefAnnee")
#' 
NULL





#' Class "RefCheckBox" RefCheckBox
#' 
#' referential class allows to choose for several parms with checkbox
#' 
#' 
#' @name RefCheckBox-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefCheckBox", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @references ~put references to the literature/web site here ~
#' @keywords classes
#' @examples
#' 
#' showClass("RefCheckBox")
#' 
NULL



