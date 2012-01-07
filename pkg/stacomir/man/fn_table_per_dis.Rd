\name{fn_table_per_dis}
\alias{fn_table_per_dis}
\title{functions called in DF and DC}
\usage{
  fn_table_per_dis(typeperiode, tempsdebut, tempsfin,
    libelle, date = TRUE)
}
\arguments{
  \item{typeperiode}{ref.tr_typearretdisp_tar(per_tar_code) the code of the period (see table ref.tr_typearretdisp_tar)}
  \item{tempsdebut}{ref.tr_typearretdisp_tar(per_date_debut) starting timestamp of the period}
  \item{tempsfin}{ref.tr_typearretdisp_tar(per_date_fin) ending timestamp of the period}
  \item{libelle}{ref.tr_typearretdisp_tar(libelle )description of the period type}
  \item{date}{boolean, should the function return a POSIXt or date value}
}
\value{
  listeg
}
\description{
  functions called in DF and DC
}
\note{
returns either POSIXt or date if date=TRUE
}

\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

