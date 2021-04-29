
setwd("/Users/rasmussj/Documents/PaperSubmission/FFA_Hypothalamus/Analysis/")
rm(list = ls())

library('tidyverse')
library('mgcv')
library('mediation')
library('mgcViz')
library('rsq')
library('leaps')

#hypoth_data = read.csv('R_stats_out.csv')
hypoth_data = read.csv('ffa_multiverse.csv')

#mod_lm_ffa <- gam(z_hth ~ z_ga + z_sa + sex + z_ffa + z_wm_md + z_qc, data=hypoth_data)
mod_lm_ffa <- lm(MD_HTH_59 ~ ga + sa + sexBin + ffas_z_z_main + WB_MD + mean_fd_rms_2, data=hypoth_data)
summary(mod_lm_ffa)

mod_lm_ffa <- lm(MD_HTH_59 ~ ga + sa + sexBin + ffas_z_z_main + WB_MD + mean_fd_rms_2 + SES_2 + OBrisk_cat + ethnicity_mom, data=hypoth_data)
summary(mod_lm_ffa)

subsets <- regsubsets(MD_HTH_59 ~ ga + sa + sexBin + ffas_z_z_main + WB_MD + mean_fd_rms_2 + SES_2 + OBrisk_cat + ethnicity_mom, hypoth_data, nbest=1)

rsq.partial(mod_lm_ffa)

