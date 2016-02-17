glmEnsemble: Ensemble methods for Generalized Linear Models
====

Package for fitting GLM ensemble models. Currently still writing tests. only binomial-logit models have been used so far.

## Release notes:
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
