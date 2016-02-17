glmEnsemble: Ensemble methods for Generalized Linear Models
====

This package trains and tests GLM ensemble models with backward variable  selection via AIC. Holdout testing data is first selected and then ensemble elements are trained on training datasets via resampling from the non-holdout data. The following family-link functions are supported: binomial-logit, binomial-probit, poisson-log, gaussian-identity. Resulting ensemble coefficients are weighted by accuracy on test data. By default, ensemble elements are built in parallel.

**Note:** A future version of this package will include penalized GLMs via the penalized package.

## Release notes:
- v0.2 - [NOT COMPLETE YET] Unit tests passed for supported GLM types (logit, probit, poisson-log, gaussian-identity)
- v0.1.2 - first push to github. Still in process of writing tests.

## Installation
- From Github
```
library("devtools");
install_github("alexwhitworth/glmEnsemble",dependencies=TRUE)
```

## Example uses:

```
# load an example dataset, create a binary flag
data(prostate, package= "faraway")
prostate$high_cavol <- factor(ifelse(prostate$lcavol > quantile(prostate$lcavol, .8), 1, 0))

# model fitting in one-step
# many defaults are explicitly specified here, the same call could be shortened to
# glm1 <- glm_ensemble(prostate, "high_cavol")
glm1 <- glm_ensemble(prostate, dep_var= "high_cavol", n= 100L, major_class_wt= 1,
          test_pct= 0.33, direction= "backward", family= binomial(link= "logit"))

```
