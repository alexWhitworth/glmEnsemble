glmEnsemble: Ensemble methods for Generalized Linear Models
====

[![Build Status](https://travis-ci.org/alexWhitworth/glmEnsemble.svg?branch=master)](https://travis-ci.org/alexWhitworth/glmEnsemble.svg?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/alexWhitworth/glmEnsemble/badge.svg?branch=master)](https://coveralls.io/github/alexWhitworth/glmEnsemble?branch=master)


This package trains and tests GLM ensemble models with backward variable  selection via AIC. Holdout testing data is first selected and then ensemble elements are trained on training datasets via bootstrap resampling from the non-holdout data. The following family-link functions are supported: binomial-logit, binomial-probit, poisson-log, gaussian-identity. Resulting ensemble coefficients are weighted by accuracy on test data. By default, ensemble elements are built in parallel.

Ensemble models are expected to work because different models (ensemble elements) will not make the same error. Consider the set of *k* models where each model makes iid errors $\epsilon_i \sim MVN(0, \sigma^2), i=1, \ldots, n$ on each observation. Then the average prediction error of the ensemble is $\frac{1}{k}\sum_i \epsilon_i$.

**Note:** A future version of this package will include penalized GLMs via the penalized package.

## Release notes:
- v0.2 - Unit tests passed for supported GLM types (logit, probit, poisson-log, gaussian-identity)
- v0.1.2 - first push to github. Still in process of writing tests.

## Installation
- From Github
```
library("devtools");
install_github("alexwhitworth/glmEnsemble",dependencies=TRUE)
```

## Example uses:

```
# **** Binomial data (logit link) ****
# load an example dataset, create a binary flag
data(hprice, package= "faraway"); hprice$msa <- NULL
hprice$high_price <- factor(ifelse(hprice$narsp > quantile(hprice$narsp, .8), 1, 0))

# model fitting in one-step
# ** many defaults are used here, please see the documentation for more expressive use
glm1 <- glm_ensemble(hprice, dep_var= "high_price", n= 10L, family= binomial(link= "logit"))

# **** Poisson data ****
data(gala, package= "faraway")
glm2 <- glm_ensemble(gala, dep_var= "Species", n= 10L, family= poisson(link= "log"))

# **** Gaussian data ****
data(prostate, package= "faraway")
glm3 <- glm_ensemble(prostate, dep_var= "lcavol", n= 10L, family= gaussian(link= "identity"))

# **** predictions (type= response) ****
pred1 <- predict(glm1, hprice, type= "response")
pred2 <- predict(glm2, gala, type= "response")
pred3 <- predict(glm3, prostate, type= "response")
```
