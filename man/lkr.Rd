\name{lkr}
\docType{data}
\alias{lkr}
\title{Leukemia Remission Data}
\description{
  Data of a leukemia remission study. There are 42 patients in the dataset, who were monitored for whether they relapsed and for how long (in weeks) they remained in remission.
}
\usage{
lkr
}
\format{
  \tabular{ll}{
    weeks:\tab the time that a patient was remained in remission (in weeks)\cr
    relapse:\tab whether the patient was relapsed -  1 = yes, 0 = no\cr
    treatment1:\tab 1=drug A, 0 = standard drug for treatment1\cr
    treatment2:\tab 1=drug B, 0 = standard drug for treatment2\cr
    wbc3cat:\tab  white blood cell count, recorded in three categories - 1 = normal, 2= moderate, 3 = high\cr
  }
}
\references{
  Garrett JM.
  sbe14: Odds Ratios and Confidence Intervals for Logistic Regression
  Models with Effect Modification. Stata Technical Bulletin, 36, 15-22. 
  Reprinted in Stata Technical Bulletin Reprints, vol. 6, pp. 104-114, 1997
  College Station, TX: Stata Press}
\keyword{datasets}
