\name{showfile}
\Rdversion{1.1}
\alias{showfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{showfile}
\description{
Display of a table stored in a file
}
\usage{
showfile(donnees, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{donnees}{
  The table which is displayed
}
  \item{filename}{
  The file name
}
}

\author{
 cedric.briand@lavilaine.com
}

\examples{
showfile(filename="fichierTailleVideo.csv")

## The function is currently defined as
function(donnees,filename){
	tt <-tktoplevel ()
	yscr <- tkscrollbar(tt,
			command=function(...)tkyview(txt,...))
	xscr <- tkscrollbar(tt,
			command=function(...)tkxview(txt,...),orient='horiz')
	txt <- tktext(tt,bg="white",font="courier",
			yscrollcommand=function(...)tkset(yscr,...),
			xscrollcommand=function(...)tkset(xscr,...),
			wrap='none',
			font=tkfont.create(family='lucidatypewriter',size=10))
	tkwm.title(tt,paste('aperçu du fichier',filename)) # titre général
	
	tkpack(yscr,side='right',fill='y')
	tkpack(xscr,side='bottom',fill='x')
	tkpack(txt,fill='both',expand=1)
	thestring = capture.output(print(donnees))
	thestring = paste(thestring,collapse='\n')
	tkinsert(txt,"end",thestring)
	tkfocus(txt)
	tkpack(tkbutton(tt,command=function() {tkdestroy(tt)},text='OK'),side='bottom')}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
