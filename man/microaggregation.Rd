\name{microaggregation}
\alias{microaggregation}
\title{ Microaggregation }
\description{
  Function to perform various methods of microaggregation.
}
\usage{
microaggregation(x, method = "pca", aggr = 3, nc = 8, clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log", blow = TRUE, blowxm = 0)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{method}{ pca, onedims, single, simple, clustpca, pppca, clustpppca, mdav, clustmcdpca, influence, mcdpca }
  \item{aggr}{ aggregation level (default=3)}
  \item{nc}{ number of cluster, if the chosen method performs cluster analysis }
  \item{clustermethod}{ clustermethod, if necessary }
  \item{opt}{  }
  \item{measure}{ aggregation statistic, mean, median, trim, onestep (default = mean) }
  \item{trim}{ trimming percentage, if measure=trim }
  \item{varsort}{ variable for sorting, if method= single }
  \item{transf}{ transformation for data x }
  \item{blow}{ if TRUE, the microaggregated data will have the same dimension as the original data set }
  \item{blowxm}{ the microaggregated data with the same dimension as the original one. }
}
\details{
On \url{http://neon.vb.cbs.nl/casc/Glossary.htm} one can found the 
\dQuote{official} definition of microaggregation:

Records are grouped based on a proximity measure of variables of interest, and the same small groups of records are used in calculating aggregates
 for those variables. The aggregates are released instead of the individual record values.
 
While for the proximity measure very different concepts can be used, 
the aggregation itself is naturally done with the arithmetic mean. 
Nevertheless, other 
measures of location can be used for aggregation, 
especially when the group size for aggregation has been taken higher than 3. 
Since the median
seems to be unsuitable for microaggregation because of being highly robust, 
other mesures which are included can be chosen.

This function contains also a method with which the data can be clustered 
with a variety of different clustering algorithms. Clustering 
observations before applying microaggregation might be useful. 
Note, that the data are automatically standardised before
clustering.

The usage of clustering method \sQuote{Mclust} requires package mclust02, 
which must be loaded first.
The package is not loaded automatically, since the package is not under GPL 
but comes with a different licence. 

The are also some projection methods for microaggregation included. 
The robust version \sQuote{pppca} or 
\sQuote{clustpppca} (clustering at first)
are fast implementations and provide almost everytime the best results.

Univariate statistics are preserved best with the individual 
ranking method (we called them \sQuote{onedims}, however, often this method is 
named \sQuote{individual ranking}), but multivariate statistics 
are strong affected. 

With method \sQuote{simple} one can apply microaggregation 
directly on the (unsorted) data. It is useful for the comparison with other methods
as a benchmark, i.e.
replies the question how much better is a sorting of the data before aggregation.

If blow is set to FALSE, the result will be a data set with dimension n divided by aggr. 
}
\value{
  \item{x }{original data}
  \item{method  }{method  }
  \item{clustering  }{ TRUE, if a clustering is done before microaggregation }
  \item{aggr  }{aggregation level  }
  \item{nc  }{ number of clusters, if a clustering method is chosen }
  \item{xm  }{ aggregated data set }
  \item{roundxm  }{ rounded aggregated data set (to integers) }
  \item{clustermethod  }{ clustermethod, if a cluster method is chosen }
  \item{measure  }{ proximity measure for aggregation }   
  \item{trim  }{ trimming, if proximity measure \sQuote{trim} is chosen }   
  \item{varsort  }{ information about the variable which is chosen when using method \sQuote{single} }   
  \item{transf  }{ transformation used, when clustering is applied first }   
  \item{blow  }{ TRUE, blowxm is calculated }   
  \item{blowxm  }{ microaggregated data with the same dimension as the original data set }  
  \item{fot  }{ correction factor, necessary if totals calculated and n divided by aggr is not an integer. }   
}
\references{ \url{http://www.springerlink.com/content/v257655u88w2/?sortorder=asc&p\_o=20} 

Templ, M. and Meindl, B., 
               \emph{Robust Statistics Meets {SDC}: New Disclosure Risk Measures for 
               Continuous Microdata Masking}, 
               Lecture Notes in Computer Science, Privacy in Statistical Databases, 
               vol. 5262, pp. 113-126, 2008.       
 
Templ, M.  
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  url = {http://www.tdp.cat/issues/abs.a004a08.php}
  
  Templ, M. 
\emph{New Developments in Statistical Disclosure Control and Imputation:
Robust Statistics Applied to Official Statistics}, Suedwestdeutscher Verlag fuer Hochschulschriften, 
2009, ISBN: 3838108280, 264 pages. 
    
    
}
\author{ Matthias Templ }
\seealso{ \code{\link{summary.micro}}, \code{\link{plotMicro}}, \code{\link{valTable}} }
\examples{
data(Tarragona)
m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
## summary(m1)
}
\keyword{ manip}
