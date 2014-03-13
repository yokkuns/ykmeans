\name{kmeansN}
\alias{kmeansN}

\title{
kmeansN
}
\description{
N times run the k-means
}
\usage{
kmeansN(x, k, variable.names = "x", objective.name = "y", cluster.name = "cluster", n = 100)
}

\arguments{
  \item{x}{
A data.frame
}
  \item{k}{
number of cluster
}
  \item{variable.names}{
variable names
}
  \item{objective.name}{
objective variable name
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
act.kmn <- kmeansN(actData, 3, paste0("x",1:17),"y")
table(act.kmn$cluster)
}

