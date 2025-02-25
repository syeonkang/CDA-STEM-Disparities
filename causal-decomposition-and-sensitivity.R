# load library
library(PSweight)       # for fitting exposure model
library(causal.decomp)  # for CDA and sensitivity analysis
citation("causal.decomp")

#-------------------------------------------------------------------------------#
# Data
#-------------------------------------------------------------------------------#
# Note: all continuous variables are standardized.
dim(DATA)     # 10469    34
# DATA[, "major"]     # Y (outcome 1): Major in eng., comp. sci., phys. sci. 
# DATA[, "X2MTHID"]   # Y (outcome 2): Mathematics identity
# DATA[, "X2MTHEFF"]  # M (mediator): Mathematics self-efficacy
# DATA[, "female"]    # R (group status): gender
# DATA[, "race"]      # C (baseline covariate): race
# DATA[, c(6:34)]     # X (intermediate confounders)

# take a subset of data due to missing outcome
# missing rate (outcome 1): 1174/10469 = 11.21406 %
sum(is.na(DATA[, "major"]))/nrow(DATA)    
# missing rate (outcome 2): 0 %
sum(is.na(DATA[, "X2MTHID"]))/nrow(DATA)  
DATA1 <- DATA[!is.na(DATA[, "major"]), ]; dim(DATA1)    # 9295   34
DATA2 <- DATA[!is.na(DATA[, "X2MTHID"]), ]; dim(DATA2)  # 10469  34

#-------------------------------------------------------------------------------#
# Outcome 1: Major (binary)
#-------------------------------------------------------------------------------#
## Estimation
# exposure model
fit.r <- SumStat(female ~ race, data = DATA1, weight = "IPW")
# outcome model: Y ~ R + M + X + C + R*M
fit.y.var <- c("female", "X2MTHEFF", colnames(DATA1[, c(6:34)]), "race",
               "female * X2MTHEFF")
fit.y.fm <- as.formula(paste("major", paste(fit.y.var, collapse = "+"),
                             sep = "~"))
fit.y <- glm(fit.y.fm, data = DATA1, family = binomial(link = "logit"))
# mediator model: M ~ R + C
fit.m <- lm(X2MTHEFF ~ female + race, data = DATA1)
# estimation
smires <- smi(fit.r = fit.r, fit.m = fit.m, fit.y = fit.y,
              sims = 1000, conditional = FALSE,
              covariates = "race", treat = "female", seed = 1)
# check result
smires
round(smires$result * 100, 2)

#-------------------------------------------------------------------------------#
# Outcome 2: Math identity (continuous)   
#-------------------------------------------------------------------------------#
## Estimation
# exposure model
fit.r <- SumStat(female ~ race, data = DATA2, weight = "IPW")
# outcome model: Y ~ R + M + X + C + R*M
fit.y.var <- c("female", "X2MTHEFF", colnames(DATA2[, c(6:34)]), "race",
               "female * X2MTHEFF")
fit.y.fm <- as.formula(paste("X2MTHID", paste(fit.y.var, collapse = "+"),
                             sep = "~"))
fit.y <- lm(fit.y.fm, data = DATA2)
# mediator model: M ~ R + C
fit.m <- lm(X2MTHEFF ~ female + race, data = DATA2)
# estimation
smires <- smi(fit.r = fit.r, fit.m = fit.m, fit.y = fit.y,
              sims = 1000, conditional = FALSE,
              covariates = "race", treat = "female", seed = 1)
# check result
smires
round(smires$result, 2)

## Sensitivity analysis
sensRes <- sensitivity(boot.res = smires, fit.m = fit.m, fit.y = fit.y,
                       mediator = "X2MTHEFF", covariates = "race",
                       treat = "female", sel.lev.treat = "female",
                       max.rsq = 0.5)
# create sensitivity contour plots
plot(sensRes, xlim = c(0, 0.5), ylim = c(0, 0.5))

