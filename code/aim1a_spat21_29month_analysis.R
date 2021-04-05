# ---------------------------------------- #
#           Spat21/Mozzie Study            #
# Manuscript figures and 29 month analysis #
#                 Aim 1A                   #
#               Human Data                 #
#            Mozzie Phase 3                #
#                K. Sumner                 #
#           November 24, 2020              #
# ---------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)
library(ggalluvial)
library(gridExtra)
library(coxme)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_19NOV2020.rds")


# change the age category coding
# for primary data
table(survival_data_primary$age_cat_baseline)
survival_data_primary$new_age_cat_baseline = ifelse(survival_data_primary$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_primary$new_age_cat_baseline,useNA = "always")
survival_data_primary$new_age_cat_baseline = as.factor(survival_data_primary$new_age_cat_baseline)
survival_data_primary$new_age_cat_baseline = relevel(survival_data_primary$new_age_cat_baseline,ref="15 years or less")
# for secondary stringent data
table(survival_data_primary$age_cat_baseline)
survival_data_secondary_stringent$new_age_cat_baseline = ifelse(survival_data_secondary_stringent$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_secondary_stringent$new_age_cat_baseline,useNA = "always")
survival_data_secondary_stringent$new_age_cat_baseline = as.factor(survival_data_secondary_stringent$new_age_cat_baseline)
survival_data_secondary_stringent$new_age_cat_baseline = relevel(survival_data_secondary_stringent$new_age_cat_baseline,ref="15 years or less")
# for secondary permissive data
table(survival_data_secondary_permissive$age_cat_baseline)
survival_data_secondary_permissive$new_age_cat_baseline = ifelse(survival_data_secondary_permissive$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_secondary_permissive$new_age_cat_baseline,useNA = "always")
survival_data_secondary_permissive$new_age_cat_baseline = as.factor(survival_data_secondary_permissive$new_age_cat_baseline)
survival_data_secondary_permissive$new_age_cat_baseline = relevel(survival_data_secondary_permissive$new_age_cat_baseline,ref="15 years or less")




#### ------- figure 2: descriptives --------- ####


##  make a plot of how malaria exposure infection status changes over time (from having an asymptomatic infection to having no infection during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_primary %>%
  select(main_exposure_primary_case_def,month_year,unq_memID) %>%
  group_by(month_year,main_exposure_primary_case_def,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# reorder so asymptomatic infections are on the bottom
plot_human_data_withperc$main_exposure_primary_case_def = relevel(as.factor(plot_human_data_withperc$main_exposure_primary_case_def),"no infection")

# now make an alluvial plot of how infection status changes over time in BW
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = main_exposure_primary_case_def, alluvium = unq_memID,
                           y = perc_n,fill = main_exposure_primary_case_def, label = main_exposure_primary_case_def)) +
  geom_flow(na.rm=T,alpha=0.4) +
  geom_stratum(width = 8) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_fill_manual(values = c("#cccccc","#000000")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months") +
  theme(legend.position="bottom")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/primary_alluvial_exposure.png", device="png",
       height=6, width=11, units="in", dpi=500)


# now make an alluvial plot of how infection status changes over time but with colors
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = main_exposure_primary_case_def, alluvium = unq_memID,
                           y = perc_n,fill = main_exposure_primary_case_def, label = main_exposure_primary_case_def)) +
  geom_flow(na.rm=T,alpha=0.4) +
  geom_stratum(width = 8) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months") +
  theme(legend.position="bottom")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/primary_alluvial_exposure_color.png", device="png",
       height=5, width=11, units="in", dpi=500)


# now make an alluvial plot of how infection status changes over time but with colors - dissertation colors
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = main_exposure_primary_case_def, alluvium = unq_memID,
                           y = perc_n,fill = main_exposure_primary_case_def, label = main_exposure_primary_case_def)) +
  geom_flow(na.rm=T,alpha=0.4) +
  geom_stratum(width = 8) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months") +
  theme(legend.position="bottom")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/primary_alluvial_exposure_color.png", device="png",
       height=5, width=11, units="in", dpi=500)


# now make an alluvial plot of how infection status changes over time but with colors but remove ribbons - dissertation colors
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = main_exposure_primary_case_def, alluvium = unq_memID,
                           y = perc_n,fill = main_exposure_primary_case_def, label = main_exposure_primary_case_def)) +
  geom_flow(na.rm=T,alpha=0) +
  geom_stratum(width = 8) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months") +
  theme(legend.position="bottom")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/primary_alluvial_exposure_color_dis_no_ribbons.png", device="png",
       height=5, width=11, units="in", dpi=500)



# make a violin plot of follow-up time for asymptomatic compared to no infection for symptomatic infections only
symp_infections = survival_data_primary %>% filter(status=="symptomatic infection")
violin_plot = ggplot(symp_infections, aes(x = main_exposure_primary_case_def,y=days_until_event)) + 
  geom_violin(aes(fill=main_exposure_primary_case_def),alpha=0.8) +
  coord_flip() + 
  xlab("Main exposure") +  ylab("Time to symptomatic malaria") + 
  scale_fill_manual(values = c("#cccccc","#000000")) +
  theme_bw() +
  geom_boxplot(width=0.1) + 
  theme(legend.position="none")
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_exposure_plot.png", device="png",
       height=4, width=9, units="in", dpi=500)


#### ------ make table 1: comparison of covariates and symptomatic infections ------ ####

## first look at total number of observations across covariates
table(survival_data_primary$main_exposure_primary_case_def)
table(survival_data_primary$age_cat_baseline)
table(survival_data_primary$gender)
table(survival_data_primary$slept_under_net_regularly)
table(survival_data_primary$village_name)

# now look at the number of symptomatic infections across covariates
symp_infections = survival_data_primary %>% filter(status=="symptomatic infection")
table(symp_infections$main_exposure_primary_case_def)
table(symp_infections$age_cat_baseline)
table(symp_infections$gender)
table(symp_infections$slept_under_net_regularly)
table(symp_infections$village_name)

# now look at the median time to symptoms
symp_infections %>% group_by(main_exposure_primary_case_def) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(age_cat_baseline) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(gender) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(slept_under_net_regularly) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(village_name) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))

# do the wilcoxon rank sum test to compare median time to symptoms across each covariates
# multiply each p-value by 29 which is the maximum number of time a person could have been in the data set
# main exposure
wilcox.test(days_until_event ~ main_exposure_primary_case_def,data = symp_infections)
2.166e-07*29
# age
kruskal.test(days_until_event ~ age_cat_baseline,data = symp_infections)
0.0005124*29
# sex
wilcox.test(days_until_event ~ gender, data=symp_infections)
0.02685*29
# bed net usage
wilcox.test(days_until_event ~ slept_under_net_regularly, data=symp_infections)
0.5336*29
# village
kruskal.test(days_until_event ~ village_name,data = symp_infections)
2.893e-05*29



#### -------- now make figure 3: model output and EMM by age and gender -------- ####

## ------ first run the full model

# run a multi-level coxph model with random intercepts for the participant level 
# primary data set
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph
# make a forest plot of the model results
table1 = exp(confint(fit.coxph,method="Wald"))
estimates = c(1.1132469,NA,2.5157362,0.9693182,NA,0.6288850,NA,0.5160118,NA,1.0882527,0.6980237)
lower_ci = c(table1[1,1],NA,table1[3,1],table1[2,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[3,2],table1[2,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,0.2,0.3,0.4,0.6,0.8,1.0,2.0,3.0,4.0,5.0)) +
  theme_bw() +
  theme(text = element_text(size=11)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/primary_forest_plot_coxph_1levels.png", device="png",
       height=3, width=6, units="in", dpi=400)


## ---- check effect measure modification by age

# primary data set
# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph) # does appear to be significant interaction

# now run stratified models
# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                       data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_over15)
fit.coxph.over15

# make a forest plot of the results
names = c("Participant age <5 years","Asymptomatic infection","","Participant age 5-15 years"," Asymptomatic infection","   ","Participant age >15 years","  Asymptomatic infection")
estimate = c(NA,1.3757364,NA,NA,1.1596422,NA,NA,0.9568495)
lower_ci = c(NA,1.04687889,NA,NA,1.0220723,NA,NA,0.8087136)
upper_ci = c(NA,1.807898,NA,NA,1.315729,NA,NA,1.132120)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Participant age <5 years","Asymptomatic infection","","Participant age 5-15 years"," Asymptomatic infection","   ","Participant age >15 years","  Asymptomatic infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Participant age <5 years","Asymptomatic infection","","Participant age 5-15 years"," Asymptomatic infection","   ","Participant age >15 years","  Asymptomatic infection"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#000000",NA,NA,"#000000",NA,NA,"#000000"),size=c(2,2,2,2,2,2,2,2)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","bold","plain","plain","bold","plain","plain","bold"))) +
  scale_y_continuous(trans="log10",breaks=c(0,0.8,1.0,1.2,1.4,1.6,1.8,2))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/primary_age_stratified_forest_plot_coxph_1levels.png", device="png",
       height=4.5, width=6.5, units="in", dpi=400)




## ---- check effect measure modification by gender

# primary data set
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph) # does not appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_male)
fit.coxph.male

# make a forest plot of the results
names = c("Male","Asymptomatic infection","","Female"," Asymptomatic infection")
estimate = c(NA,1.0768265,NA,NA,1.1443571)
lower_ci = c(NA,0.9368085,NA,NA,1.0054618)
upper_ci = c(NA,1.237772,NA,NA,1.3024395)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Male","Asymptomatic infection","","Female"," Asymptomatic infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Male","Asymptomatic infection","","Female"," Asymptomatic infection"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#000000",NA,NA,"#000000"),size=c(2,2,2,2,2)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","bold","plain","plain","bold","plain","plain","bold"))) +
  scale_y_continuous(trans="log10",breaks=c(0,0.9,1.0,1.1,1.2,1.3,1.4,2))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/primary_gender_stratified_forest_plot_coxph_1levels.png", device="png",
       height=4.5, width=6.5, units="in", dpi=400)



#### ------- now make kaplan-meier curves for each of the follow-up times ------- ####

## ----- for the primary case definition
# KM curve stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#cccccc","#000000"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="29-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(30),
                     xlim=c(0,810))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier.png", device="png",
       height=6, width=11, units="in", dpi=300)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary)
sd <- survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary)
1 - pchisq(sd$chisq, length(sd$n) - 1)

# now make this same plot but with only the first 30 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#cccccc","#000000"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="1-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(5),
                     xlim=c(0,30),
                     ylim=c(0.85,1.00))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_short30.png", device="png",
       height=3, width=4, units="in", dpi=300)



# now make these same plots in color
# KM curve stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#1b9e77","#d95f02"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="29-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(30),
                     xlim=c(0,810))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_color.png", device="png",
       height=5, width=11, units="in", dpi=300)
# now make this same plot but with only the first 30 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#1b9e77","#d95f02"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="1-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(5),
                     xlim=c(0,30),
                     ylim=c(0.85,1.00))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_short30_color.png", device="png",
       height=3, width=4, units="in", dpi=300)



# now make these same plots in color for the dissertation document
# KM curve stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days to symptomatic malaria", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#4daf4a","#ff7f00"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="29-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(30),
                     xlim=c(0,810))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_color_dis.png", device="png",
       height=5, width=11, units="in", dpi=300)
# now make this same plot but with only the first 30 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days to symptomatic malaria", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#4daf4a","#ff7f00"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="1-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(5),
                     xlim=c(0,30),
                     ylim=c(0.85,1.00))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_short30_color_dis.png", device="png",
       height=3, width=4, units="in", dpi=300)
# now make this same plot but with only the first 90 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days to symptomatic malaria", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#4daf4a","#ff7f00"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="3-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(10),
                     xlim=c(0,90),
                     ylim=c(0.8,1.00))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_short90_color_dis.png", device="png",
       height=3, width=4, units="in", dpi=300)
# now make this same plot but with only the first 180 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days to symptomatic malaria", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#4daf4a","#ff7f00"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="6-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(20),
                     xlim=c(0,180),
                     ylim=c(0.7,1.00))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_short180_color_dis.png", device="png",
       height=3, width=4, units="in", dpi=300)
# now make this same plot but with only the first 365 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days to symptomatic malaria", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#4daf4a","#ff7f00"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="12-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(30),
                     xlim=c(0,365),
                     ylim=c(0.5,1.00))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_short365_color_dis.png", device="png",
       height=3, width=4, units="in", dpi=300)
# now make this same plot but with only the first 90 days of follow-up
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                     xlab = "Time in days to symptomatic malaria", 
                     ylab = "Survival probability",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = F,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#4daf4a","#ff7f00"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title="29-month follow-up",
                     font.title = c(11, "bold"),
                     break.x.by = c(100))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_full29_color_dis.png", device="png",
       height=3, width=4, units="in", dpi=300)




## ----- for the secondary permissive case definition
# KM curve stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def, data = survival_data_secondary_permissive), 
                     xlab = "Time in days", 
                     ylab = "Survival probability",
                     surv.median.line = "hv",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = T,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#cccccc","#000000"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title = "Symptomatic malaria (secondary permissive)",
                     font.title = c(11, "bold"))
km_plot
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_kaplan_meier.png", device="png",
       height=5, width=4, units="in", dpi=300)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def, data = survival_data_secondary_permissive)
sd <- survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def, data = survival_data_secondary_permissive)
1 - pchisq(sd$chisq, length(sd$n) - 1)



## ----- for the secondary stringent case definition
# KM curve stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def, data = survival_data_secondary_stringent), 
                     xlab = "Time in days", 
                     ylab = "Survival probability",
                     surv.median.line = "hv",
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend = "none",
                     pval = T,
                     ggtheme = theme_bw(),
                     risk.table = T,
                     ncensor.plot = F,
                     palette = c("#cccccc","#000000"),
                     conf.int.style = "step",
                     risk.table.y.text = FALSE,
                     risk.table.y.text.col = T,
                     title = "Symptomatic malaria (secondary stringent)",
                     font.title = c(11, "bold"))
km_plot
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_kaplan_meier.png", device="png",
       height=5, width=4, units="in", dpi=300)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def, data = survival_data_secondary_stringent)
sd <- survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def, data = survival_data_secondary_stringent)
1 - pchisq(sd$chisq, length(sd$n) - 1)



## MISCLASSIFICATION SENSITIVITY ANALYSIS BELOW

#### ------- now subset the data set to remove all monthly visits up to 14 days before the symptomatic infection --------- ####

# remove monthly visits up to 14 days before the symptomatic infection
# primary data set
remove = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(survival_data_primary)){
  if (survival_data_primary$days_until_event[i] < 15 & survival_data_primary$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_primary$days_until_event < 15 & survival_data_primary$status == "symptomatic infection"))
survival_data_primary$remove = remove
survival_data_primary = survival_data_primary %>% filter(is.na(remove))
survival_data_primary$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary stringent data set
remove = rep(NA,nrow(survival_data_secondary_stringent))
for (i in 1:nrow(survival_data_secondary_stringent)){
  if (survival_data_secondary_stringent$days_until_event[i] < 15 & survival_data_secondary_stringent$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_stringent$days_until_event < 15 & survival_data_secondary_stringent$status == "symptomatic infection"))
survival_data_secondary_stringent$remove = remove
survival_data_secondary_stringent = survival_data_secondary_stringent %>% filter(is.na(remove))
survival_data_secondary_stringent$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary permissive data set
remove = rep(NA,nrow(survival_data_secondary_permissive))
for (i in 1:nrow(survival_data_secondary_permissive)){
  if (survival_data_secondary_permissive$days_until_event[i] < 15 & survival_data_secondary_permissive$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_permissive$days_until_event < 15 & survival_data_secondary_permissive$status == "symptomatic infection"))
survival_data_secondary_permissive$remove = remove
survival_data_secondary_permissive = survival_data_secondary_permissive %>% filter(is.na(remove))
survival_data_secondary_permissive$remove <- NULL


#### --------- now rerun the cox porportional hazards model across outcome definitions ---------- ####

# run a model using the primary case definition
fit.coxph.primary <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                           data = survival_data_primary)
fit.coxph.primary
exp(confint(fit.coxph.primary))


# run a model using the secondary stringent case definition
fit.coxph.secondarystringent <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                                      data = survival_data_secondary_stringent)
fit.coxph.secondarystringent
exp(confint(fit.coxph.secondarystringent))


# run a model using the secondary permissive case definition
fit.coxph.secondarypermissive <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                                       data = survival_data_secondary_permissive)
fit.coxph.secondarypermissive
exp(confint(fit.coxph.secondarypermissive))

#### -------- explore EMM by age using primary case definition ------- ####

# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15



# now create a forest plot comparing these results to the main model
# make a forest plot of the model results
estimates = c(1.38,1.16,0.96,1.23,1.06,0.88)
lower_ci = c(1.05,1.02,0.81,0.92,0.93,0.74)
upper_ci = c(1.81,1.32,1.13,1.64,1.21,1.05)
type = c("Main model","Main model","Main model","Misclassification analysis","Misclassification analysis","Misclassification analysis")
names = c("<5 years","5-15 years",">15 years","<5 years","5-15 years",">15 years")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$names = factor(forest_plot_df$names, levels=c("<5 years","5-15 years",">15 years"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.5,0.6,0.7,0.75,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") +
  theme(text = element_text(size=10.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure4_maintomisclassification_fp.png", device="png",
       height=4, width=5.5, units="in", dpi=400)



# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.primary) # does appear to be significant interaction



#### ----- look at EMM by sex using primary case definition ------- ####

# primary data set
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.primary) # does not appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male

