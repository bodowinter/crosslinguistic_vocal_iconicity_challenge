## Bodo Winter
## August 9, 2019
## Bayesian analysis for both experiments

## This is the only script where the paths aren't relative, please reset working directories

## Load packages:

library(tidyverse)
library(brms)

field <- read_csv('../data/field_cleaned.csv')
web <- read_csv('../data/online_cleaned.csv')

## For parallel processing:

options(mc.cores=parallel::detectCores())

## Set weakly informative prior for animacy effect and uniform prior for intercept:

my_priors <- c(prior('normal(0,2)', class = 'b'),
               prior('uniform(-10, 10)', class = 'Intercept'))

## Convert "Animacy" column to factor:

field <- mutate(field,
                Animacy = factor(Animacy))

## Sum-code the animacy predictor, so that the intercept is irrespective of animacy:

contrasts(field$Animacy) <- contr.sum(2)

## Check:

contrasts(field$Animacy)

## Fit the main model:

main_mdl <- brm(ACC ~ 1 +
                  (1|ID) +
                  (1|Meaning) + (1|UniqueItem) +
                  (1|Team) +
                  (1|Language) +
                  (1|Genus),
                data = web,
                family = bernoulli,
                init = 0,
                cores = 4,
                chains = 4,
                warmup = 2000,
                iter = 4000,
                prior = prior('uniform(-10, 10)',
                              class = 'Intercept'),
                control = list(adapt_delta = 0.99,
                               max_treedepth = 13),
                seed = 42)
save(main_mdl, file = 'main_mdl.RData')

## Fit the main model with animacy as predictor:

main_animacy_mdl <- brm(ACC ~ Animacy +
                          (1 + Animacy|ID) +
                          (1|Meaning) + (1|UniqueItem) +
                          (1|Team) +
                          (1 + Animacy|Language) +
                          (1|Genus),
                        data = web,
                        family = bernoulli,
                        init = 0,
                        cores = 4,
                        chains = 4,
                        warmup = 2000,
                        iter = 4000,
                        prior = my_priors,
                        control = list(adapt_delta = 0.99,
                                       max_treedepth = 13),
                        seed = 42)
save(main_animacy_mdl, file = 'main_animacy_mdl.RData')

## Fit the model without animacy:

field_mdl <- brm(ACC ~ 1 +
                           (1|ID) +
                           (1|Meaning) + (1|UniqueItem) +
                           (1|Team) +
                           (1|Language),
                         data = field,
                         family = bernoulli,
                         init = 0,
                         cores = 4,
                         chains = 4,
                         warmup = 2000,
                         iter = 4000,
                         prior = prior('uniform(-10, 10)',
                                       class = 'Intercept'),
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 13),
                         seed = 42)
save(field_mdl, file = 'field_animacy_mdl.Rdata')

## Fit the model (treatment-coded):

field_animacy_mdl <- brm(ACC ~ 1 +
                   Animacy +
                   (1 + Animacy|ID) +
                   (1|Meaning) + (1|UniqueItem) +
                   (1|Team) +
                   (1 + Animacy|Language),
                data = field,
                family = bernoulli,
                init = 0,
                cores = 4,
                chains = 4,
                warmup = 2000,
                iter = 4000,
                prior = my_priors,
                control = list(adapt_delta = 0.99),
                seed = 42)
save(field_animacy_mdl, file = 'field_animacy_mdl.RData')

