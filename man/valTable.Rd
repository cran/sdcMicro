\name{valTable}
\alias{valTable}
\title{ Comparison of different microaggregation methods }
\description{
A Function for the comparison of different perturbation methods.
}
\usage{
valTable(x, method = c("simple", "single", "onedims", "pca", "pppca", "clustpca", "clustpppca", "mdav"), measure = "mean", clustermethod = "Mclust", aggr = 3, nc = 8, transf = "log")
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{method}{ microaggregation methods }
  \item{measure}{ FUN for aggregation. Possible values are mean (default), median, trim, onestep. }
  \item{clustermethod}{ clustermethod, if a method will need a clustering procedure }
  \item{aggr}{ aggregation level (default=3) }
  \item{nc}{ number of clusters. Necessary, if a method will need a clustering procedure }
  \item{transf}{ Transformation of variables before clustering. }
}
\details{
Tabelarise the output from summary.micro. Will be enhanced to all perturbation methods in future versions.
}
\value{
Measures of information loss splitted for the comparison of different methods.
}
\author{ Matthias Templ }
\seealso{ \code{\link{microaggregation}}, \code{\link{summary.micro}} }
\examples{
data(Tarragona)
valTable(Tarragona[100:200,], method=c("simple","onedims","pca","clustpppca"))
## valTable(Tarragona, method=c("simple","onedims","pca","clustpppca","mdav"))
## clustpppca outperforms the other algorithms for this data set...
}
\keyword{ print }

