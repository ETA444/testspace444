\name{ezbanner}
\alias{ezbanner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ezbanner
}
\description{
The function outputs a nice comment banenr in the console and copies it to you clipboard to use as a comment in your script. Provide it the desired text/title.
}
\usage{
ezbanner(txt, ftype, bChar, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{String that will be made into a comment banner.}
  \item{ftype}{Takes a value of 1 or 2 (default: 1). This changes the appearance of the banner.}
  \item{bChar}{You can specify a character (e.g, ":") that will be used to construct the banner.}
  \item{s}{Makes banner "snug" if True; non-snug if False.}
}
\details{
(!) All values have defaults aside from txt.

(!) This function is based on the amazing package bannerCommenter by Venables (2021).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Venables B (2021). _bannerCommenter: Make Banner Comments with a Consistent Format_. R
  package version 1.0.0, <https://CRAN.R-project.org/package=bannerCommenter>.
}
\author{
George Dreemer (ETA444)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%% # define the name
%% name <- "Homer"

%% # test function
%% say_hello(name)

%% # use it in a for loop
%% friends <- c("Bob", "Mary", "Richard", "Sue")
%% for (friend in friends) {
%% say_hello(friend)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
