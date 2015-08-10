\name{plot.threg}
\alias{plot.threg}
\title{Plot curves of the estimated survival, hazard or density functions for threshold regression model}
\description{
 This function can be used to display the graphs of the estimated survival, hazard or density functions at different levels of a factor predictor variable which has been included in the threshold regression by threg() function.
 }
\usage{
\method{plot}{threg}(x,var,scenario,graph,nolegend=0,nocolor=0,...)
}
\arguments{
  \item{x}{a threg object.}
  \item{var}{specifies the categorical variable for each level of which the curves are plotted. Such categorical variable must be a factor variable that has been used in threg() that returns the threg object.}
  \item{scenario}{specifies a scenario where the predicted plots are based on.}
  \item{graph}{specifies the type of curves to be generated. The ``hz'' option is to plot hazard function curves, the ``sv'' option is to plot survival function curves, and the ``ds'' option is to plot density function curves.}
  \item{nolegend}{set the ``nolegend'' argument to 1 if users do not want the ``threg'' package to generate legends for the picture. Note that even if ``nolegend'' is set to 1, users can still generate legends by themselves after the picture is generated, by using the ``legend'' function in R.}
  \item{nocolor}{set the ``nocolor'' argument to 1 if users want to depict all curves in black.}
  \item{\dots}{for future methods}
}


\examples{
#load the data "lkr"
data("lkr")

#Transform the "treatment2" variable into factor variable "f.treatment2" .
lkr$f.treatment2=factor(lkr$treatment2)

#fit the threshold regression model on the factor variable "f.treatment2", 
fit<-threg(Surv(weeks, relapse)~ f.treatment2|f.treatment2,data = lkr)
fit

#generate the predicted survival curves for the drug B group and 
#the standard group.
plot(fit,var=f.treatment2,graph=sv,nolegend=1,nocolor=1)
legend(20, 1, c("Standard","Drug B"), lty = 1:2) 



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

#fit the same model as above, but additionally overlay curves of survival functions 
#corresponding to different levels of "f.group'.
plot.threg(fit,var=f.group,scenario=recipient_age(18)+f.fab1(0),graph=sv,nocolor=1)


#fit the same model as above, but additionally overlay curves of hazard functions 
#corresponding to different levels of "f.group'.
plot.threg(fit,var=f.group,scenario=recipient_age(18)+f.fab1(0),graph=hz,nocolor=1)

#fit the same model as above, but additionally overlay curves of probability density 
#functions corresponding to different levels of "f.group'.
plot.threg(fit,var=f.group,scenario=recipient_age(18)+f.fab1(0),graph=ds,nocolor=1)

}


\keyword{models}
\keyword{survival analysis}
\keyword{threshold regression}
\keyword{plot}
