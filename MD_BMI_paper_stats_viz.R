#Clear the environment
rm(list = ls())

#Load necessary libraries
library('car') #Partial plot
library('tidyverse') #Data handling
library('rsq') #R-squareds
#library('mgcv') # Used for the GAMs
#library('mgcViz') # Visualizing GAMs
#library('leaps') # Best subsets

#Read in the dataset: 
# hth_md ~ hypothalamus (PV>0.59) mean diffusivity
# wm_md ~ white matter mean diffusivity
# ga ~ gest. age at birth, in weeks
# sa ~ postnatal age at scan, in days
# bmi ~ maternal prepregancy BMI
# sexBinary ~ male==1
# qc_mean_fd ~ quality control, mean displacement via fsl quad
# ethnicity_mom ~ mom's ethnicity
# ses ~ two factor (education, income) index of socioeconomic status
# OBrisk_cat ~ categorical, 1==yes
hypoth_data = read.csv('./uci_bmi_hth_covars.csv')

# #Continuous BMI, NOT restricting
# #Linear model 
# bmi_linear_all <- lm(hth_md ~ ga + sa + sexBinary + bmi + wm_md + qc_mean_fd, data=hypoth_data)
# #Model statistics
# summary(bmi_linear_all)
# #R-partial squared
# rsq.partial(bmi_linear_all)
# #Residual plot
# crPlots(bmi_linear_all,hth_md ~ bmi, smooth = FALSE)

#Continuous BMI, Restricting to BMI threshold (visual inspection from FinnBrain)
bmi_threshold=36
bmi_linear_thr_36 <- lm(hth_md ~ ga + sa + sexBinary + bmi + wm_md + qc_mean_fd, data=filter(hypoth_data, bmi < bmi_threshold))
summary(bmi_linear_thr_36)
rsq.partial(bmi_linear_thr_36)
crPlots(bmi_linear_thr_36,hth_md ~ bmi, smooth = FALSE)

# #Categorical cut, WHO: healthy vs. overweight
# hypoth_data$bmi_category <- cut(hypoth_data$bmi, breaks=c(0, 25, 100), labels=c("healthy","overweight"))
# bmi_categorical_2_cats <- lm(hth_md ~ ga + sa + sexBinary + bmi_category + wm_md + qc_mean_fd, data=hypoth_data)
# summary(bmi_categorical_2_cats)

# #Non-linear Generalized Additive Model
# mod_gam1_bmi <-  gam(hth_md ~ ga + sa + sexBinary + s(bmi) + wm_md + qc_mean_fd, data=hypoth_data)
# summary(mod_gam1_bmi)
# b <- getViz(mod_gam1_bmi)
# o <- plot( sm(b, 1) )
# o + l_fitLine(colour = "red", size = 2) + l_ciLine(mul = 5, colour = "blue", linetype = 2, size = 2) + l_points(shape = 19, size = 3, alpha = 0.7) + theme_classic()
# anova(bmi_linear_all, mod_gam1_bmi, test="Chisq")