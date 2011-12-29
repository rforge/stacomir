\name{interface_BilanLot}
\Rdversion{1.1}
\alias{interface_BilanLot}
\alias{interface_BilanMigration}
\alias{interface_BilanFonctionnementDC}
\alias{interface_BilanFonctionnementDF}      
\alias{interface_BilanLot}       
\alias{interface_BilanMigrationConditionEnv}
\alias{interface_BilanMigrationInterAnnuelle}
\alias{interface_BilanMigrationPar}
\alias{interface_BilanPoidsMoyen}            
\alias{interface_BilanTaille}
\alias{interface_BilanEspeces}
\alias{interface_Bilan_stades_pigm}
\alias{interface_ConditionEnv}
\title{Interface used to instantiate objects and call display methods}
\description{
These interfaces are called from handlers in stacomi menu and used to create objects and 
use "choix" methods to create the user interface. 
}
\usage{
interface_BilanLot()
}
%- maybe also 'usage' for other objects documented here.
\details{
These functions are created to instantiate the Bilans objects named in the title.
For instance, interface_BilanLot will instantiate an object of class Bilan_lot.
Those instances are created in the environment .GlobalEnv but the methods choix store their
results (after choice) in envir_stacomi having for parent .GlobalEnv. On closure or opening of 
another method, the method quitte is called and deletes the frame just created.
A ggroup {gWidgets} (see ? ggroup) named "group" was always used in the interface as the classes as the frames of 
the referential objects (for instance taxon) are always added to group
}
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}
\note{
always has to be called within a group constructed and deleted using quitte()
}
\examples{
# no examples provided
}
\keyword{ interface}
