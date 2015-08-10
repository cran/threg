\name{predict.threg}
\alias{predict.threg}
\title{predictions for threshold regression model}
\description{
This function can be used to predict the initial health status value \eqn{y_0}, the drift value of the health process \eqn{\mu}, the probability density function of the survival time \eqn{f(t\mid \mu,y_0)}, the survival function \eqn{S(t\mid \mu, y_0)} and the hazard function \eqn{h(t\mid \mu,y_0)} for a specified scenario and time value. The scenario specified here is similar to those in hr.threg and plot.threg functions. The only difference is that we need to provide the scenario values for all variables in the model, while for hr.threg and plot.threg functions we do not need to provide scenario values of the dummy variables expanded from the factor variable specified in the var argument.
 }
\usage{
\method{predict}{threg}(object,timevalue,scenario,...)
}

\arguments{
  \item{object}{a threg object.}
  \item{timevalue}{specifies the desired time value at which the predicted values are to be calculated. Vector is allowed for this argument. If this argument is omitted, then the predicted values for the study time of all subjects would be calculated.}
  \item{scenario}{specifies the values of all predictors. If this argument is omitted, then the predicted values at a specified time value for all subjects would be calculated, and in this case the covariate values for each subject are used as their corresponding scenario values.}
  \item{\dots}{for future methods}
}

\examples{

#load the data "bmt"
data("bmt")

#Transform the "group" and "fab" variables into factor variables 
#"f.group" and "f.fab".
bmt$f.group=factor(bmt$group)
bmt$f.fab=factor(bmt$fab)

#fit a threshold regression model on the "bmt" dataset, by using "recipient_age" and 
#"f.fab" as the predictors for ln(y0), and "f.group" and "f.fab" as predictors for mu.
fit<-threg(Surv(time, indicator)~ recipient_age+f.fab|f.group+f.fab, data = bmt)

#predict lny0, y0, mu, f, S and h for the specified scenario that "the patient age is 
#18 years old, the FAB classification is 0 and the risk category is 3", at the 
#time ``2000 days''
predict.threg(fit,timevalue=2000,scenario=recipient_age(18)+f.fab1(0)+f.group2(0)+f.group3(1))

}



\keyword{models}
\keyword{survival analysis}
\keyword{threshold regression}
\keyword{prediction}
