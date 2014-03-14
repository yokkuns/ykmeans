\name{ykmeans-package}
\alias{ykmeans-package}
\docType{package}
\title{
K-means using a target variable
}
\description{
The clustering by k-means of using the target variable.
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
\dontrun{
data(actData)
act.ykm <- ykmeans(actData, paste0("x",1:17),"y",3:6)
table(act.ykm$cluster)
}
}
