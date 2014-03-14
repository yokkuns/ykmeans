\name{ykmeans}
\alias{ykmeans}

\title{
K-means using a target variable
}
\description{
The clustering by k-means of using the target variable.
}
\usage{
ykmeans(x, variable.names = "x",target.name = "y",
              k.list = 3:6,cluster.name = "cluster", n = 100)
}

\arguments{
  \item{x}{
A data.frame
}
  \item{variable.names}{
variable names
}
  \item{target.name}{
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
\dontrun{
data(actData)
act.ykm <- ykmeans(actData, paste0("x",1:17),"y",3:6)
table(act.ykm$cluster)
}
}

