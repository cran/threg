\name{bmt}
\docType{data}
\alias{bmt}
\title{Bone Marrow Transplantation Data}
\description{
  Survival of 137 acute leukemia patients treated with bone marrow transplants which are a standard treatment for acute leukemia.
}
\usage{
bmt
}
\format{
  \tabular{ll}{
    time:\tab time to relapse, death or end of study (in days)\cr
    indicator:\tab censoring indicator variable -  1 = dead or replapse, 0 = otherwise\cr
    recipient_age:\tab patient age\cr
    group:\tab  risk categories based on their status at the time of transplantation\cr
    fab:\tab French-American-British (FAB) classification based on standard morphological criteria\cr
  }
}

\references{
    Klein JP, Moeschberger ML. 
    Survival analysis: techniques for censored and truncated data. 2 edition, 2003 
    Springer-Verlag New York, Inc.}
\keyword{datasets}
