#' An object of class bilanMigrationMult with data loaded
#' 
#' This data corresponds to the data collected from three fishways
#' and correspond to the migration station at Arzal in 2011 for all
#' stages of eel (Anguilla anguilla)
#'
#' @format An object of class bilanMigrationMult with slots:
#' \describe{
#'   \item{dc}{the \code{RefDC} object filled with data}
#'   \item{taxons}{the \code{RefTaxon} object filled in with data corresponding to dc}
#'   \item{stades}{the \code{RefStades} object filled in with data corresponding to dc, and taxon}
#'   \item{pasDeTemps}{the \code{PasDeTempsJournalier} calculated for all 2011}
#'   \item{data}{ A dataframe with 400 rows and 11 variables
#' 			\describe{
#'   			\item{ope_identifiant}{operation id}
#'   			\item{lot_identifiant}{sample id}
#'   			\item{lot_identifiant}{sample id}
#'   			\item{ope_dic_identifiant}{dc id}
#'   			\item{lot_tax_code}{species id}
#'   			\item{lot_std_code}{stage id}
#'   			\item{value}{the value}
#'   			\item{type_de_quantite}{either effectif (number) or poids (weights)}
#'   			\item{lot_dev_code}{destination of the fishes}
#'   			\item{lot_methode_obtention}{method of data collection, measured, calculated...} 
#'   			}
#'   }
#'   \item{calcdata}{slot to be filled with the calcule method} 
#'   \item{coef_conversion}{A data frame with 364 observations with daily coefficients to convert from weight to numbers}
#'   \item{time.sequence}{A time sequence generated for the bilan, used internally by the object}
#' }
#' @keywords data
"bMM_Arzal"

#' Messages
#' 
#' In english
#'
#' 
#' @keywords data
"msg"

#' Working environment for stacomiR created when launching stacomi()
#' 
#' This is where the graphical interface stores its objects
#' try \code{ls(envir=envir_stacomi)}
#' @keywords environment
"envir_stacomi"


#' An object of class bilanMigration with data loaded
#' 
#' This data corresponds to the data collected at the vertical slot fishway
#' in 2015, video recording of the thin lipped mullet Liza ramada migration
#'
#' @format An object of class bilanMigration with 8 slots:
#' \describe{
#'   \item{dc}{the \code{RefDC} object with 4 slots filled with data corresponding to the iav postgres schema}
#'   \item{taxons}{the \code{RefTaxon} the taxa selected}
#'   \item{stades}{the \code{RefStades} the stage selected}
#'   \item{pasDeTemps}{the \code{PasDeTempsJournalier} calculated for all 2015}
#'   \item{data}{ A dataframe with 10304 rows and 11 variables
#' 			\describe{
#'   			\item{ope_identifiant}{operation id}
#'   			\item{lot_identifiant}{sample id}
#'   			\item{lot_identifiant}{sample id}
#'   			\item{ope_dic_identifiant}{dc id}
#'   			\item{lot_tax_code}{species id}
#'   			\item{lot_std_code}{stage id}
#'   			\item{value}{the value}
#'   			\item{type_de_quantite}{either effectif (number) or poids (weights)}
#'   			\item{lot_dev_code}{destination of the fishes}
#'   			\item{lot_methode_obtention}{method of data collection, measured, calculated...} 
#'   			}
#'   }
#'   \item{coef_conversion}{A data frame with 0 observations : no quantity are reported for video recording of mullets, only numbers}
#'   \item{time.sequence}{A time sequence generated for the bilan, used internally}
#' }
#' @keywords data
"bM_Arzal"

#' An object of class \link{BilanFonctionnementDF-class} with data loaded
#' 
#' This dataset corresponds to the data collected at three different fishways
#' it is loaded along with \link{bMM_Arzal}
#' @format An object of class BilanFonctionnementDF 
#' @keywords data
"bilanFonctionnementDF"


#' An object of class \link{BilanFonctionnementDC-class} with data loaded
#' 
#' This dataset corresponds to the data collected at three different control devices
#' it is loaded along with \link{bMM_Arzal}
#' @format An object of class BilanFonctionnementDC 
#' @keywords data
"bilanFonctionnementDC"

#' An object of class \link{BilanOperation-class} with data loaded
#' 
#' This dataset corresponds to the data collected at three different control devices
#' it is loaded along with \link{bMM_Arzal}
#' @format An object of class BilanOperation
#' @keywords data
"bilanOperation"

#' dataframe containing the default connection arguments
#' the program will use a file installed in c:/program files/stacomi but
#' if not found will switch to the default
"calcmig"


#' An object of class \link{BilanFonctionnementDF-class} with data loaded
#' 
#' This data corresponds to the data collected at the vertical slot fishway
#' in 2015, the fishway is working daily with a cycle depending on tide.
#'
#' @format An object of class BilanFonctionnementDF with 4 slots:
#' \describe{
#' #'   \item{data}{ A dataframe with 4261 obs. of  7 variables
#' 			\describe{
#'   			\item{per_dis_identifiant}{The number of the DF}
#'   			\item{per_date_debut}{Starting time a POSIXct}
#'   			\item{per_date_fin }{Ending time a POSIXct}
#'   			\item{ope_dic_identifiant}{DF id}
#'   			\item{per_commentaires }{A comment}
#'   			\item{per_etat_fonctionnement}{Integer 1= working, 0 not working}
#'   			\item{per_tar_code}{The type of operation ("1"=normal operation,
#'              "2"=Device stopped in normal operation (ie lift ascending, high tide...),
#'				"3"="Stopped for maintenance or other problem",
#'              "4"="Works but not fully operational, ie flow problem, flood, clogged with debris...",
#'              "5"="Not known")}
#' 				\item{libelle}{label corresponding to per_tar_code}
#'            }
#'        }	
#'   \item{df}{the \code{RefDF} object with 3 slots filled with data corresponding to the iav postgres schema}
#'   \item{horodatedebut}{the \code{RefHorodate} with horodate set for starting date}
#'   \item{horodatefin}{the \code{RefHorodate} with horodate set for ending date} #'   
#' }
#' @keywords data
"bfDF"

#' An object of class \link{BilanFonctionnementDC-class} with data loaded
#' 
#' This data corresponds to the data collected at the vertical slot fishway camera
#' from 2000 to 2015.
#'
#' @format An object of class BilanFonctionnementDC with 4 slots:
#' \describe{
#' #'   \item{data}{ A dataframe with 544 obs. of  7 variables
#' 			\describe{
#'   			\item{per_dis_identifiant}{The number of the DC}
#'   			\item{per_date_debut}{Starting time a POSIXct}
#'   			\item{per_date_fin }{Ending time a POSIXct}
#'   			\item{ope_dic_identifiant}{DC id}
#'   			\item{per_commentaires }{A comment}
#'   			\item{per_etat_fonctionnement}{Integer 1= working, 0 not working}
#'   			\item{per_tar_code}{The type of operation ("1"=normal operation,
#'              "2"=Device stopped in normal operation (e.g. the trap is disactivated for the duration of the
#' 				fish sorting and counting by operators),
#'				"3"="Stopped for maintenance or other problem",
#'              "4"="Works but not fully operational, i.e. the camera is not working properly because of high turbidity...",
#'              "5"="Not known")}
#' 				\item{libelle}{label corresponding to per_tar_code}
#'            }
#'        }	
#'   \item{df}{the \code{RefDC} object with 3 slots filled with data corresponding to the iav postgres schema}
#'   \item{horodatedebut}{the \code{RefHorodate} with horodate set for starting date}
#'   \item{horodatefin}{the \code{RefHorodate} with horodate set for ending date} #'   
#' }
#' @keywords data
"bfDC"

#' An object of class \link{BilanFonctionnementDF-class} with data loaded
#' 
#' This dataset corresponds to the data collected at three different fishways
#' it is loaded along with \link{bM_Arzal}
#' @format An object of class BilanFonctionnementDF 
#' @keywords data
"bilanFonctionnementDF_bM"


#' An object of class \link{BilanFonctionnementDC-class} with data loaded
#' 
#' This dataset corresponds to the data collected at three different control devices
#' it is loaded along with \link{bM_Arzal}
#' @format An object of class BilanFonctionnementDC 
#' @keywords data
"bilanFonctionnementDC_bM"

#' An object of class \link{BilanOperation-class} with data loaded
#' 
#' This dataset corresponds to the data collected at three different control devices
#' it is loaded along with \link{bM_Arzal}
#' @format An object of class BilanOperation
#' @keywords data
"bilanOperation_bM"

#' An object of class \link{Bilan_carlot-class} with data loaded
#' 
#' This dataset corresponds to the data collected at two different control devices
#' at the Arzal control station (see example in \link{Bilan_carlot-class}), all body size 
#' parameters (total size, size converted from pixel in video control) are used in example
#' @format An object of class Bilan_carlot
#' @keywords data
"b_carlot"

#' An object of class \link{BilanMigrationInterAnnuelle-class} with data loaded
#' 
#' The first eel trapping ladder in France was built by Antoine Legault and the team from Rennes
#' in the Sevre Niortaise, Marais Poitevin. Also refurbished several times since 1984 it has been 
#' operational at the same location and provides one of the longest series of eel migration. For this reason,
#' the dataset has been loaded as an example for the BilanMigrationInterAnnuelle-class. It has been
#' kindly given by the parc du Marais Poitevin.
#' @format An object of class BilanMigrationInterAnnuelle-class with data slot loaded.
#' @keywords data
"bmi"

#' An object of class \link{BilaAnnuels-class} with data loaded
#' 
#' The dataset corresponds to the three fishways located on the Arzal dam, filled with annual data
#' @format An object of class \link{BilanAnnuels-class} with data slot loaded.
#' @keywords data
"bilA"

#' Annual migration of salmon by migradour
#' 
#' The dataset corresponds to the fishways DC=33:40 of the Adour for adult migrant salmons
#' from 1996 to 2005 (annual counts). It has been kinly provided as an example set by the Migradour
#' association.
#' @format An object of class \link{BilanAnnuels-class} with data slot loaded.
#' @keywords data
"bilAM"


#' Silver eel migration in the Somme
#' 
#' The dataset corresponds to the silver eel traps ("anguilleres) for 2015-2016.
#' This dataset has been kindly provided by the Federation de Peche de la Somme,
#' given the upstream location of the trap, all individuals are female
#' 
#' @format An object of class \link{BilanAnnuels-class} with data slot loaded.
#' @keywords data
"bilanArg"

#' Silvering index coefficients from Caroline Durif (2009) to predict silvering stage from morphological parameters
#' Classification scores are calculated by multiplying the metrics 
#' BL = body length, W = weight, MD = mean eye diameter (Dv+Dh)/2, and FL length of the pectoral fin,
#' with each parameter p as S=Constant+BL*p(bl)+W*p(W)... The stage chosen is the one achieving the 
#' highest score
#' @references Durif, C.M., Guibert, A., and Elie, P. 2009. 
#' Morphological discrimination of the silvering stages of the European eel. 
#' In American Fisheries Society Symposium. pp. 103–111.
#'  \url{http://fishlarvae.org/common/SiteMedia/durif\%20et\%20al\%202009b.pdf}
"coef_Durif"

