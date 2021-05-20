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
bmi_threshold=35
bmi_linear_thr <- lm(scale(hth_md) ~ ga + sa + sexBinary + bmi + wm_md + qc_mean_fd, data=filter(hypoth_data, bmi < bmi_threshold))
summary(bmi_linear_thr)
rsq.partial(bmi_linear_thr)
# #Residual plot, not used because of formatting option
# crPlots(bmi_linear_thr,hth_md ~ bmi, smooth = FALSE)
#Create dataset wioth fixed BMI
hypoth_data_fixed_bmi=hypoth_data
hypoth_data_fixed_bmi$bmi=mean(hypoth_data$bmi, na.rm=TRUE)
#Get partial residual
hypoth_data$model_partial_residual=scale(hypoth_data_fixed_bmi$hth_md)-predict.lm(bmi_linear_thr,hypoth_data_fixed_bmi)
#Filter using the above threshold
hypoth_data_thresh=filter(hypoth_data, bmi < bmi_threshold)
#Plot it, creating x and y just for brevity
hypoth_data_thresh$x=hypoth_data_thresh$bmi
hypoth_data_thresh$y=hypoth_data_thresh$model_partial_residual

reg <- lm(y ~ x, hypoth_data_thresh)
tiff("bmi_hth_uci.tiff", units="in", width=8, height=4, res=300)
g <- ggplot(hypoth_data_thresh, aes(x = x, y = y))
g +
  geom_abline(intercept = coefficients(reg)[1], slope = coefficients(reg)[2], color = "gray", size = 1.5, linetype = "dashed") +
  ggtitle("BMI vs. Hypothalamus") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(color = rgb(0.1875,0.4375,0.7148), alpha = 1, size = 4) +
  labs(x = "Maternal BMI", y = "Offspring Hypothalamus MD (z-score)") + 
  theme(panel.background = element_rect(fill = "white", color = "gray", size = 0.5)) +
  coord_fixed(ratio = 2) +
  xlim(17.4, 35) +
  ylim(-2,2.9)
dev.off()

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