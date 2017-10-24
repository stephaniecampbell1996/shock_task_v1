############## Import data and load packages ##############  

set.seed(112)

# clear workspace
rm(list = ls())

# set workspace directory 
setwd("C:/Users/StephanieCampbell/Dropbox/ShockGameAnalysis")


# install packages
library(data.table)
library(foreign)
library(lme4)
library(ggplot2)
library(doBy)
library(cowplot)
library(gridExtra)
library("ggExtra")
library("psych")
library("plyr")
library('corrplot')
library(lmerTest)
library(lsmeans)
library(multcomp)
library(ggsignif)
library(optimx)
library(HH)
library(gmodels)
library(effsize)
library(sjPlot)
library(sjmisc)
library(memisc)
library("PerformanceAnalytics")
library(RODBC)
library(robustlmm)
library(Hmisc)
library(WRS2)
library(robust)
library(ordinal)
library(ggpubr)
library(BSDA)
library(grid)

# import data from a csv file  
empathy_df <- utils::read.csv("empathy_pool.csv", header = TRUE, na.strings = c("","NA"))
twostep_df <- utils::read.csv("two_step_results.csv", header = TRUE, na.strings = c("","NA"))

############## Prepare data ##############  

# rename column name for subid in twostep_df
twostep_df <- plyr::rename(twostep_df, c("w" = "w_empathy"))

# sort dataframes
twostep_df <- twostep_df[ order(twostep_df$subid), ] 
empathy_df <- empathy_df[ order(empathy_df$subid), ] 

# merge two dataframes
combined_df <- merge(empathy_df, twostep_df, by = "subid")
combined_df <- combined_df[ order(combined_df$subid), ] # sort the dataframe

# compare our w with w from prior non-social study's dataset
AK_df <- utils::read.csv("Amy_Krosch_database.csv", header = TRUE, na.strings = c("","NA"))

z.test = function(a, mu, var){
  zeta = (mean(a) - mu) / (sqrt(var / length(a)))
  return(zeta)
}

mean_w_nonsocial <- mean(AK_df$w)
var_w_nonsocial <- var(AK_df$w)

# check
utils::head(combined_df)
utils::str(combined_df)
base::summary(combined_df)
dplyr::glimpse(combined_df)

############## Data visualization ##############  

# histograms for mixture parameters from social vs. non-social two-step tasks
w_empa_histogram <- ggplot2::ggplot(data = combined_df, aes(combined_df$w_empathy)) + 
  coord_cartesian(xlim = c(0, 1)) + scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_histogram(col = "red", alpha = 0.7, breaks = seq(0, 1, by = 0.1), 
                 aes(fill = ..density..)) +
  #stat_bin(aes(y = ..ndensity.., label = ..ndensity..), geom = "text", vjust = -.5) +
  ggtitle(paste("Empathy two-step task (n = ",length(combined_df$w_empathy),")")) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        plot.title = element_text(color = "blue", size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  theme(legend.key.height = unit(1,"line")) +
  theme(legend.key.width = unit(1,"line")) +
  scale_fill_gradient("density", low = "green", high = "red") +
  labs(x = "weighting parameter")

w_nonsocial_histogram <- ggplot2::ggplot(data = AK_df, aes(AK_df$w)) + 
  coord_cartesian(xlim = c(0, 1)) + scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_histogram(col = "red", alpha = 0.7, breaks = seq(0, 1, by = 0.1), 
                 aes(fill = ..density..)) +
  #stat_bin(aes(y = ..ndensity.., label = ..ndensity..), geom = "text", vjust = -.5) +
  ggtitle(paste("Non-social two-step task (n = ",length(AK_df$w),")")) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        plot.title = element_text(color = "blue", size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  theme(legend.key.height = unit(1,"line")) +
  theme(legend.key.width = unit(1,"line")) +
  scale_fill_gradient("density", low = "green", high = "red") +
  labs(x = "weighting parameter")

cowplot::plot_grid(w_nonsocial_histogram, w_empa_histogram, labels = c("(a)", "(b)"), ncol = 2, nrow = 1)

# correlation plots
# scatterplot for w_empathy and reward_corr
theme_set(theme_grey())  # pre-set the bw theme.
g_w_reward <- ggplot2::ggplot(combined_df, aes(x = w_empathy, y = reward_corr)) + 
  geom_count(show.legend = FALSE, color = "black", size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold")) +
  labs(y = "reward rate (corrected)", x = "weighting parameter")

ggExtra::ggMarginal(g_w_reward, type = "histogram", fill = "transparent")
#ggExtra::ggMarginal(g_w_reward, type = "boxplot", fill = "transparent")
#ggExtra::ggMarginal(g_w_reward, type = "density", fill = "transparent")

# scatterplot for video_measure and w_empathy
theme_set(theme_grey())  # pre-set the bw theme.
g_w_empa_video <- ggplot2::ggplot(combined_df, aes(x = video_measure, y = w_empathy)) + 
  geom_count(show.legend = FALSE, color = "black", size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold")) +
  labs(x = "empathy measure (video task)", y = "weighting parameter")

g_w_empa_video <- ggExtra::ggMarginal(g_w_empa_video, type = "histogram", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_video, type = "boxplot", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_video, type = "density", fill = "transparent")

# scatterplot for IRI_PD and w_empathy
theme_set(theme_grey())  # pre-set the bw theme.
g_w_empa_PD <- ggplot2::ggplot(combined_df, aes(x = IRI_PD, y = w_empathy)) + 
  geom_count(show.legend = FALSE, color = "black", size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold")) +
  labs(x = "personal distress (IRI)", y = "weighting parameter")

g_w_empa_PD <- ggExtra::ggMarginal(g_w_empa_PD, type = "histogram", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_PD, type = "boxplot", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_PD, type = "density", fill = "transparent")

# scatterplot for EI_emo and w_empathy
theme_set(theme_grey())  # pre-set the bw theme.
g_w_empa_emo <- ggplot2::ggplot(combined_df, aes(x = EI_emo, y = w_empathy)) + 
  geom_count(show.legend = FALSE, color = "black", size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold")) +
  labs(x = "affective empathy (sharing + contagion)", y = "weighting parameter")

g_w_empa_emo <- ggExtra::ggMarginal(g_w_empa_emo, type = "histogram", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_emo, type = "boxplot", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_emo, type = "density", fill = "transparent")

# scatterplot for IRI_cog and w_empathy
theme_set(theme_grey())  # pre-set the bw theme.
g_w_empa_cog <- ggplot2::ggplot(combined_df, aes(x = IRI_cog, y = w_empathy)) + 
  geom_count(show.legend = FALSE, color = "black", size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold")) +
  labs(x = "cognitive empathy (IRI)", y = "weighting parameter")

g_w_empa_cog <- ggExtra::ggMarginal(g_w_empa_cog, type = "histogram", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_cog, type = "boxplot", fill = "transparent")
#ggExtra::ggMarginal(g_w_empa_cog, type = "density", fill = "transparent")

cowplot::plot_grid(g_w_empa_video, g_w_empa_emo, g_w_empa_PD, g_w_empa_cog, 
                   labels = c("(a)", "(b)", "(c)", "(d)"), 
                   ncol = 2, nrow = 2)

############## Statistical analysis ##############  

# plot correlalogram for different dependent variables and see if data can be reduced

# simple correlations on the wide format data

attach(combined_df)
# dv_corr_df <- data.frame(beta, alpha, w_empathy, V_aff_empathy, V_cog_empathy, V_EC,
#                      IRI_FS, IRI_EC, IRI_PT, IRI_PD, affective_sharing, emo_contagion)
dv_corr_df <- data.frame(beta, alpha, w_empathy, IRI_cog, EI_emo, IRI_PD, video_measure)

# correlation matrix and associated p values
dv_corr_para <- psych::corr.test(dv_corr_df, method = "pearson", adjust = "none", ci = TRUE)
dv_corr_nonpara <- psych::corr.test(dv_corr_df, method = "spearman", adjust = "none", ci = TRUE)

# plot correlogram
col12 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                 "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(200)


corrplot::corrplot(dv_corr_para$r, method = "color", col = col12,  
                   order = "hclust", addgrid.col = "black", 
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col = "black", tl.srt = 45, #Text label color and rotation
                   # Combine with significance
                   p.mat = dv_corr_para$p, sig.level = 0.05, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag = TRUE )

corrplot::corrplot(dv_corr_nonpara$r, method = "color", col = col12,  
                   order = "hclust", addgrid.col = "black",
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col = "black", tl.srt = 45, #Text label color and rotation
                   # Combine with significance
                   p.mat = dv_corr_nonpara$p, sig.level = 0.05, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag = TRUE )


# one sample z test to compare w from current sample with w from non-social two step task
z <- z.test(combined_df$w_empathy, mu = mean_w_nonsocial, var = var_w_nonsocial)
2*pnorm(-abs(z))

############## Regression analysis ##############  

# IRI-PD
w_empa_IRI_PD_lm <- stats::lm(scale(IRI_PD) ~ scale(w_empathy), data = combined_df)
summary(w_empa_IRI_PD_lm)

w_empa_IRI_PD_lmRob <- robust::lmRob(scale(IRI_PD) ~ scale(w_empathy), data = combined_df)
summary(w_empa_IRI_PD_lmRob)

w_empa_IRI_PD_lmer <- lmerTest::lmer(scale(IRI_PD) ~ scale(w_empathy) + (w_empathy | face) + 
                                       (w_empathy | color) + (w_empathy | ethnicity),
                                     data = combined_df,
                                     REML = FALSE,
                                     control = lmerControl(optimizer = "Nelder_Mead",
                                                         	restart_edge = TRUE, 
                                                           	boundary.tol = 1e-5, 
                                                           	calc.derivs = TRUE, 																		use.last.params = FALSE,
                                                           	optCtrl = list(maxfun = 2e7)))
summary(w_empa_IRI_PD_lmer)

# EI-emo
w_empa_EI_emo_lm <- stats::lm(scale(EI_emo) ~ scale(w_empathy), data = combined_df)
summary(w_empa_EI_emo_lm)

w_empa_EI_emo_lmRob <- robust::lmRob(scale(EI_emo) ~ scale(w_empathy), data = combined_df)
summary(w_empa_EI_emo_lmRob)

w_empa_EI_emo_lmer <- lmerTest::lmer(scale(EI_emo) ~ scale(w_empathy) + (w_empathy | face) + 
                                       (w_empathy | color) + (w_empathy | ethnicity),
                                     data = combined_df,
                                     REML = FALSE,
                                     control = lmerControl(optimizer = "Nelder_Mead",
                                                           	restart_edge = TRUE, 
                                                           	boundary.tol = 1e-5, 
                                                           	calc.derivs = TRUE, 																		use.last.params = FALSE,
                                                           	optCtrl = list(maxfun = 2e7)))
summary(w_empa_EI_emo_lmer)

# video measure
w_empa_video_measure_lm <- stats::lm(scale(video_measure) ~ scale(w_empathy), data = combined_df)
summary(w_empa_video_measure_lm)

w_empa_video_measure_lmRob <- robust::lmRob(scale(video_measure) ~ scale(w_empathy), data = combined_df)
summary(w_empa_video_measure_lmRob)

w_empa_video_measure_lmer <- lmerTest::lmer(scale(video_measure) ~ scale(w_empathy) +                       						(w_empathy | face) + (w_empathy | color) + (w_empathy | ethnicity),
                                            data = combined_df,
                                            REML = FALSE,
                                            control = lmerControl(optimizer = "bobyqa",
                                                               restart_edge = TRUE, 
                                                               boundary.tol = 1e-5, 
                                                               calc.derivs = TRUE, 																	   	   use.last.params = FALSE,
                                                               optCtrl = list(maxfun = 2e7)))
summary(w_empa_video_measure_lmer)

# IRI-cog
w_empa_IRI_cog_lm <- stats::lm(scale(IRI_cog) ~ scale(w_empathy), data = combined_df)
summary(w_empa_IRI_cog_lm)

w_empa_IRI_cog_lmRob <- robust::lmRob(scale(IRI_cog) ~ scale(w_empathy), data = combined_df)
summary(w_empa_IRI_cog_lmRob)

w_empa_IRI_cog_lmer <- lmerTest::lmer(scale(IRI_cog) ~ scale(w_empathy) + (w_empathy | face) + (w_empathy | color) + (w_empathy | ethnicity),
                                      data = combined_df,
                                      REML = FALSE,
                                      control = lmerControl(optimizer = "bobyqa",
                                                            restart_edge = TRUE, 
                                                            boundary.tol = 1e-5, 
                                                            calc.derivs = TRUE, 																		use.last.params = FALSE,
                                                            optCtrl = list(maxfun = 2e7)))
summary(w_empa_IRI_cog_lmer)

############## Median split analysis ##############  

w_median <- median(combined_df$w_empathy)
i <- 1 

for (i in 1:length(combined_df$w_empathy)) {
  if  (combined_df$w_empathy[i] >= w_median) {
    combined_df$w_medsplit[i] = "above"  
  }
  else {
    combined_df$w_medsplit[i] = "below"
  }
}

combined_df$w_medsplit <- as.factor(combined_df$w_medsplit)

jmv::ttestIS(
  data = combined_df,
  vars = c(
    "IRI_cog",
    "video_measure",
    "EI_emo",
    "IRI_PD"),
  group = "w_medsplit",
  welchs = TRUE,
  mann = TRUE,
  norm = TRUE,
  eqv = TRUE,
  meanDiff = TRUE,
  effectSize = TRUE,
  ci = TRUE,
  desc = TRUE,
  plots = TRUE)

# write.csv(combined_df, "two_step_empathy_analysis_df.csv", row.names = FALSE)

ggplot2::ggplot(data = combined_df, aes(combined_df$alpha)) + 
  coord_cartesian(xlim = c(0, 1)) + scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_histogram(col = "red", alpha = 0.7, breaks = seq(0, 1, by = 0.1), 
                 aes(fill = ..density..)) +
  #stat_bin(aes(y = ..ndensity.., label = ..ndensity..), geom = "text", vjust = -.5) +
  ggtitle(paste("Empathy two-step task (n = ",length(combined_df$alpha),")")) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        plot.title = element_text(color = "blue", size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  theme(legend.key.height = unit(1,"line")) +
  theme(legend.key.width = unit(1,"line")) +
  scale_fill_gradient("density", low = "green", high = "red") +
  labs(x = "alpha")

ggplot2::ggplot(data = combined_df, aes(combined_df$alpha)) + 
  coord_cartesian(xlim = c(0, 20)) + scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  geom_histogram(col = "red", alpha = 0.7, breaks = seq(0, 20, by = 1), 
                 aes(fill = ..density..)) +
  #stat_bin(aes(y = ..ndensity.., label = ..ndensity..), geom = "text", vjust = -.5) +
  ggtitle(paste("Empathy two-step task (n = ",length(combined_df$alpha),")")) + 
  theme(axis.title.x = element_text(size = 14, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 14, face = "bold"), 
        plot.title = element_text(color = "blue", size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  theme(legend.key.height = unit(1,"line")) +
  theme(legend.key.width = unit(1,"line")) +
  scale_fill_gradient("density", low = "green", high = "red") +
  labs(x = "beta")
