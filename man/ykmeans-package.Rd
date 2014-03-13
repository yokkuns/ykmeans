\name{ykmeans-package}
\alias{ykmeans-package}
\docType{package}
\title{
utility of k-means
}
\description{
In k-means, a package that automatic determination of the number of clusters using intra-cluster variance
}
\details{
\tabular{ll}{
Package: \tab ykmeans\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-14\cr
License: \tab GPL\cr
}

}
\author{
Yohei Sato
}

\examples{
data(actData)
act.ykm <- ykmeans(actData, paste0("x",1:17),"y",3:6)
table(act.ykm$cluster)
}
