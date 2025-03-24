\name{GetLetterGrade}
\alias{GetLetterGrade}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetLetterGrade(score)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score}{
%%     ~~Describe \code{score} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (score) 
{
    grade <- case_when(score >= 0.99 ~ "A+", score >= 0.92 ~ 
        "A", score >= 0.9 ~ "A-", score >= 0.89 ~ "B+", score >= 
        0.82 ~ "B", score >= 0.8 ~ "B-", score >= 0.79 ~ "C+", 
        score >= 0.72 ~ "C", score >= 0.7 ~ "C-", score >= 0.69 ~ 
            "D+", score >= 0.62 ~ "D", score >= 0.6 ~ "D-", TRUE ~ 
            "F")
    return(grade)
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
