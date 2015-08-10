\name{hr}
\alias{hr}
\alias{hr.threg}
\title{Hazard ratio calculation for threshold regression model}
\description{
 This function can be used to estimate hazard ratios at selected time
points for specified scenarios (based on given categories or value settings of covariates).
 }
 \usage{
hr(object,var,timevalue,scenario) 
\method{hr}{threg}(object,var,timevalue,scenario) 
}

\arguments{
  \item{object}{a threg object.}
  \item{var}{specifies the categorical variable for the calculation of hazard ratios. Such categorical variable must be a factor variable that has been used in threg() that returns the threg object.}
  \item{timevalue}{specifies a value of time at which the hazard ratios are calculated. A vector is allowed. }
  \item{scenario}{specifies a scenario where the hazard ratios are calculated.}
}


\examples{
#load the data "lkr"
data("lkr")

#Transform the "treatment2" variable into factor variable "f.treatment2" .
lkr$f.treatment2=factor(lkr$treatment2)

#fit the threshold regression model on the factor variable "f.treatment2", 
fit<-threg(Surv(weeks, relapse)~ f.treatment2|f.treatment2,data = lkr)
fit

#calculate the hazard ratio of the drug B group v.s. the standard group at 
#week 5 (this hazard ratio is calculated as 2.08)
hr.threg(fit,var=f.treatment2,timevalue=5)

#calculate the hazard ratio of the drug B group v.s. the standard group at 
#week 20 (this hazard ratio is calculated as 0.12)
hr.threg(fit,var=f.treatment2,timevalue=20)

#As a comparison, fit the Cox proportion hazards model on "f.treatment2", 
#and the Cox model gives a constant hazard ratio, 0.73.
summary(coxph(Surv(weeks, relapse) ~ f.treatment2, data = lkr))



#load the data "bmt"
data("bmt")

#Transform the "group" and "fab" variables into factor variables 
#"f.group" and "f.fab".
bmt$f.group=factor(bmt$group)
bmt$f.fab=factor(bmt$fab)

#fit a threshold regression model on the "bmt" dataset, by using "recipient_age" and 
#"f.fab" as the predictors for ln(y0), and "f.group" and "f.fab" as predictors for mu.
fit<-threg(Surv(time, indicator)~ recipient_age+f.fab|f.group+f.fab, data = bmt)
fit

#Calculate the hazard ratio for 
#"f.group" for the specified scenario that "the patient age is 18 years old and 
#the FAB classification is 0", at the time ``500 days''.
hr.threg(fit,var=f.group,timevalue=500,scenario=recipient_age(18)+f.fab1(0))


}

\keyword{models}
\keyword{survival analysis}
\keyword{threshold regression}
\keyword{hazard ratio}
