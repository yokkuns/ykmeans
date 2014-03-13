\name{kmeansN2}
\alias{kmeansN2}

\title{
kmeansN2
}

\description{
run N times the number of clusters in kmeans of multiple
}
\usage{
kmeansN2(x, variable.names = "x", objective.name = "y", k.list = 3:6, cluster.name = "cluster", n = 100)
}

\arguments{
  \item{x}{
A data.frame
}
  \item{variable.names}{
variable names
}
  \item{objective.name}{
objective names
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
act.kmn2 <- kmeansN2(actData, paste0("x",1:17),"y", 3:6)
head(act.kmn2)
}

