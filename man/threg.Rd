\name{threg}
\alias{threg}
\title{fit a threshold regression model}
\description{
 This function can be used to fit a threshold regressio model based on the first-hitting-time of a boundary by the sample path of a Wiener diffusion process. It uses maximum likelihood estimation method for calculating regression coefficient estimates, asymptotic standard errors and p-values.
 }
\usage{
threg(formula, data)
}

\arguments{
  \item{formula}{a formula object, with the response on the left of a ~ operator, and the independent variables on the right. The response must be a survival object as returned by the Surv function. On the right of the ~ operator, a | operator must be used: on the left of the | operator, users specify independent variables that will be used in the linear regression function for \eqn{\ln{y_0}} in the threshold regression model; on the right of the | operator, users specify independent variables that will be used in the linear regression function for \eqn{\mu} in the threshold regression model. If users just want to use a constant \eqn{\ln{y_0}} or \eqn{\mu}, he or she can put 0 or 1 as a placeholder on the left or right of the | operator, instead of listing the independent variables for \eqn{\ln{y_0}} or \eqn{\mu}.}
  \item{data}{input dataset. Such dataset must be a survival dataset including at least the survival time variable and censoring variable. For the censoring variable, 1 should be used to indicate the subjects with failure observed, and 0 should be used to indicate the subjects that are right censored. The dataset can also include other independent variables that will be used in the threshold regression model.}
}
\details{
Threshold regression is a recently developed regression methodology to analyze time to event data. For a review of this regression model, see Lee and Whitmore (2006, 2010). A unique feature of threshold regression is that the event time is considered as the time when an underlying stochastic process first hits a boundary threshold. In the context of survival data, for example, the event can be death. The death time of an individual is considered as the time when his/her latent health status  decreases to the zero boundary.



In the \code{threg} package, a Wiener process \eqn{Y(t)} is used to model the latent health status process. An event is observed when \eqn{Y(t)} reaches \eqn{0} for the first time. Three parameters of the Wiener process are involved: \eqn{\mu}, \eqn{y_0} and \eqn{\sigma}. Parameter \eqn{\mu}, called the drift of the Wiener process, is the rate per unit time at which the level of the sample path is changing. The sample path approaches the threshold if \eqn{\mu <0}. Parameter \eqn{y_0} is the initial value of the process and is taken as positive. Parameter \eqn{\sigma} represents the variability per unit time of the process (Lee and Whitmore 2006). The first hitting time (FHT) of a Wiener process with \eqn{\mu}, \eqn{y_0} and \eqn{\sigma} is an inverse Gaussian distribution with probability density function (p.d.f):
\deqn{f(t|\mu,{\sigma}^2,y_0)=\frac{y_0}{\sqrt{2\pi{\sigma^2}t^3}}\exp\left[-\frac{(y_0+\mu t)^2}{2\sigma^2 t}\right],}
where \eqn{-\infty <\mu <\infty , \sigma^2 >0, \mbox{ and } y_0>0}. The p.d.f. is proper if \eqn{\mu \leq 0}. The cumulative distribution function of the FHT is:
\deqn{F(t|\mu,{\sigma}^2,y_0)=\Phi\left[-\frac{(y_0+\mu t)^2}{\sqrt{\sigma^2 t}}\right]+\exp \left(-\frac{2y_0 \mu}{\sigma^2}\right)\Phi\left[\frac{\mu t - y_0}{\sqrt{\sigma^2 t}}\right],}
where \eqn{\Phi(\cdot)} is the cumulative distribution function of the standard normal distribution. Note that if \eqn{\mu>0}, the Wiener process may never hit the boundary at zero and hence there is a probability that the FHT is
\eqn{\infty}, that is, \eqn{P(FHT=\infty)=1-\exp(-2y_0\mu/\sigma^2)}.



Since the health status process is usually latent (i.e., unobserved), an arbitrary unit can be used to measure such a process. Hence the variance parameter \eqn{\sigma^2} of the process is set to 1 in the \code{threg} package to fix the measurement unit of the process. Then we can regress the other two process parameters, \eqn{y_0} and \eqn{\mu} on covariate data. We assume that \eqn{\mu} and \eqn{\ln(y_0)} are linear in regression coefficients.


Suppose that the covariate vector is \eqn{\bm{Z'}=(1, Z_1, \cdots, Z_k)}, where \eqn{Z_1, \cdots, Z_k} are covariates and the leading 1 in \eqn{\bm{Z'}} allows for a constant term in the regression relationship. Then \eqn{\ln(y_0)} can be linked to the covariates with the following regression form:
\deqn{\ln(y_0)=\gamma_0+\gamma_1 Z_1+\cdots + \gamma_k Z_k=\bm{Z' \gamma}} and \eqn{\mu} can be linked to the covariates with the following regression form:
\deqn{\mu=\beta_0+\beta_1 Z_1+\cdots + \beta_k Z_k=\bm{Z' \beta}}
Vectors \eqn{\gamma} and \eqn{\beta} are regression coefficients for \eqn{\ln(y_0)} and \eqn{\mu}, respectively, with \eqn{\gamma'=(\gamma_0,\cdots,\gamma_k)} and \eqn{\beta'=(\beta_0,\cdots,\beta_k)}. Note that researchers can set some elements in \eqn{\gamma} or \eqn{\beta} to zero if they feel the corresponding covariates are not important in predicting \eqn{\ln(y_0)} or \eqn{\mu}. For example, if covariate \eqn{Z_1} in the vector \eqn{Z'} is considered not important to predict \eqn{\ln(y_0)}, we can remove the \eqn{Z_1} term by setting \eqn{\gamma_1} to zero.




}


\author{
Tao Xiao

Maintainer: Tao Xiao <taoxiao1@gmail.com>
}
\references{
Lee, M-L. T., Whitmore, G. A. (2010)
  Proportional hazards and threshold regression: their theoretical and practical connections., \emph{Lifetime Data Analysis
} 16, 2: 196-214.

Lee, M-L. T., Whitmore, G. A. (2006)
  Threshold regression for survival analysis: modeling event times by a stochastic process, \emph{Statistical Science
} 21: 501-513.

Klein, J. P., Moeschberger, M. L. (2003)
  \emph{Survival Analysis: Techniques for Censored and Truncated Data}. 2 edition. Springer-Verlag New York, Inc.


Garrett, J. M. (1997)
  Odds ratios and confidence intervals for logistic regression
models with effect modification. \emph{Stata Technical Bulletin}, 36, 1522. Reprinted in \emph{Stata
Technical Bulletin Reprints}, vol. 6, pp. 104-114. College Station, TX: Stata Press.

Xiao, T., Whitmore, G. A., He, Xin, Lee, M-L. T. (2015)
 The R Package threg to Implement Threshold Regression Models. \emph{Journal of Statistical Software}, 66(8), 1-16. URL http://www.jstatsoft.org/v66/i08/.

}


\examples{
#load the data "lkr"
data("lkr")

#Transform the "treatment2" variable into factor variable "f.treatment2" .
lkr$f.treatment2=factor(lkr$treatment2)

#fit the threshold regression model on the factor variable "f.treatment2", 
fit<-threg(Surv(weeks, relapse)~ f.treatment2|f.treatment2,data = lkr)
fit

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

}

\keyword{models}
\keyword{survival analysis}
\keyword{threshold regression}
\keyword{hazard ratio}
