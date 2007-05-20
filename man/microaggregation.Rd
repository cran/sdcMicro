\name{microaggregation}
\alias{microaggregation}
\title{ Microaggregation }
\description{
  Funktion to perform various methods of microaggregation.
}
\usage{
microaggregation(x, method = "pca", aggr = 3, nc = 8, clustermethod = "Mclust", opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log", blow = TRUE, blowxm = 0)
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
  \item{blowxm}{  }
}
\details{
}
\value{
  \item{x }{original data}
  \item{method  }{method  }
  \item{clustering  }{  }
  \item{aggr  }{aggregation level  }
  \item{nc  }{  }
  \item{xm  }{  }
  \item{roundxm  }{  }
  \item{clustermethod  }{  }
  \item{measure  }{  }   
  \item{trim  }{  }   
  \item{varsort  }{  }   
  \item{transf  }{  }   
  \item{blow  }{  }   
  \item{blowxm  }{ microaggregated data }  
  \item{fot  }{ correction factor }   
}
\references{ \url{http://www.springerlink.com/content/v257655u88w2/?sortorder=asc&p\_o=20} }
\author{ Matthias Templ }
\note{  
}
\seealso{ \code{\link{summary.micro}} }
\examples{

}
\keyword{ manip}
