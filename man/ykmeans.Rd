\name{ykmeans}
\alias{ykmeans}

\title{
ykmeans
}
\description{
In k-means, a package that automatic determination of the number of clusters using intra-cluster variance
}
\usage{
ykmeans(x, variable.names = "x", objective.name = "y", k.list = 3:6, cluster.name = "cluster", n = 100)
}

\arguments{
  \item{x}{
A data.frame
}
  \item{variable.names}{
variable names
}
  \item{objective.name}{
objective variable name
}
  \item{k.list}{
number of cluster
}
  \item{cluster.name}{
cluster variable name
}
  \item{n}{
number of trials
}
}

\value{
A data.frame
}

\author{
Yohei Sato
}

\examples{
data(actData)
act.ykm <- ykmeans(actData, paste0("x",1:17),"y",3:6)
table(act.ykm$cluster)
}

