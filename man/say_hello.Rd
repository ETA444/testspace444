\name{say_hello}
\alias{say_hello}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
say_hello
}
\description{
The functions says hello to the provided string (name).
}
\usage{
say_hello(name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{String to serve as the name to say hello to.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
 # define the name
 name <- "Homer"

 # test function
 say_hello(name)

 # use it in a for loop
 friends <- c("Bob", "Mary", "Richard", "Sue")
 for (friend in friends) {
 say_hello(friend)
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
