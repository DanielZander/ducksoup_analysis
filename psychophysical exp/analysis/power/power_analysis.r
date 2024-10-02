library(glme)
library(brms)
library(dplyr)
library(lme4)
library(simr)
library(sjPlot)
library(glmmTMB)
library(WebPower)
library(ggplot2)
library(RColorBrewer)
library(lsr)
library(epitools)
library(ggdark)

# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#
#          Looking at pilot 3 raw data to get an idea of effect sizes
# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#

pilot_data     = read.csv("analysis/power/postprocc_df.csv") #data from the processed and analysed notebook, with all the additional variables (conf_change, grp_follows_indv, etc.)
conflict_trials = pilot_data[pilot_data$condition == "Conflict", ]

conflict_trials$group_follows_individual <- as.integer(as.logical(as.character(conflict_trials$group_follows_individual)))
aggregate(group_follows_individual ~ manipulation, conflict_trials, mean)
#  manipulation         group_follows_individual
#1     dominant                0.5952381
#2   submissive                0.4285714
#difference in probability = 0.167

dom_data = conflict_trials[conflict_trials$manipulation == "dominant", "group_follows_individual"]
sub_data = conflict_trials[conflict_trials$manipulation == "submissive", "group_follows_individual"]
cohensD(dom_data, sub_data)
# Raw pilot data suggest a small cohends d = 0.3341103

conflict_trials$manipulation = factor(conflict_trials$manipulation, levels = c("submissive", "dominant"))
model = glmer(group_follows_individual ~ manipulation + (manipulation|player),
             data = conflict_trials, family = "binomial")
summary(model)

#Random effects:
# Groups Name                 Variance Std.Dev. Corr
# player (Intercept)          0.290    0.5385
#        manipulationdominant 0.013    0.1140   1.00
#Number of obs: 84, groups:  player, 6

#Fixed effects:
#                     Estimate  Std. Error z value  Pr(>|z|)
#(Intercept)           -0.3085     0.3912  -0.788    0.430
#manipulationdominant   0.7322     0.4664   1.570    0.116


vcov_random <- VarCorr(model)
vcov_category <- vcov_random$player
#variance-covariance matrix
#                       (Intercept)     manipulationdominant
#(Intercept)           0.29000500           0.06139747
#manipulationdominant  0.06139747           0.01299857

#I would assume a priori for their to be more variability in the effects of our manipulation accross individuals. 
#Participants in this pilot just happen to have quite similar manipulation-slopes....



#accounting for the effect of confidence
conflict_trials$higher_conf = factor(conflict_trials$higher_conf, levels = c("False", "True"))
model_2 = glm(group_follows_individual ~ manipulation + higher_conf,
             data = conflict_trials, family = "binomial")
summary(model_2)

#Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)
#(Intercept)           -1.4903     0.4634  -3.216   0.0013 **
#manipulationdominant   0.6608     0.5287   1.250   0.2114
#higher_confTrue        2.4587     0.5290   4.647 3.36e-06 ***

# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#
#                   Create df for conflict trials
# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#

nsubjects = 100 #max sample size for analysis 
ntrial = 14 #trials in conflict trials
subject_ID = (1:nsubjects)


df = data.frame()
for (i in subject_ID){
  player       = rep(i, ntrial)
  manipulation = sample(c(rep("dominant", ntrial/2), rep("submissive", ntrial/2)))
  tmp          = data.frame(player,  manipulation)
  df           = rbind(df, tmp)
}

df = df%>%mutate(player      = as.factor(player),
                manipulation = as.factor(manipulation))

# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#
#                   Define fixed and random effects
# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#

intercept  = qlogis(0.4285714) #log-odds intercept based on raw data.

#Select a slope 
#manip_beta = qlogis(0.5952381)-qlogis(0.4285714) #log-odds slope based on raw data.
manip_beta = qlogis(0.53)-qlogis(0.4285714) #log-odds slope for 10% prob increase
#manip_beta = qlogis(0.49)-qlogis(0.4285714) #log-odds slope for 6% prob increase

fixed_effects = c(intercept,manip_beta)


#variance-covariance matrix
#                       (Intercept)     manipulationdominant
#(Intercept)           0.29000500           0.06139747
#manipulationdominant  0.06139747           0.01299857
#variance_covariance = list(matrix(c(0.35,0.235,0.235,0.4533),2))

# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#
#                              Simulation
# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#


df$manipulation = factor(df$manipulation, levels = c("submissive","dominant"))

df_sim = makeGlmer(group_follows_individual ~ manipulation + (manipulation | player),
                   family = "binomial", fixef = fixed_effects,
                   VarCorr =  list(matrix(c(0.35), 1, 1)), data = df) #participant variance based on pilot data.
                  
summary(df_sim)
plot_model( df_sim, type = "pred", terms = c("manipulation"))
plot_model( df_sim, type = "slope") #odds scale. 
plot_model( df_sim, type = "re",transform = NULL)

# -------------------------------Compute power curve-------------------------#
pcurves=powerCurve(df_sim,breaks=c(10,20,30,40,50,60,70,80,90,100),nsim=100, along="player", test = fixed("manipulation"))
pcurves
plot(pcurves)

single_sim = powerSim(df_sim , nsim = 100, test = fixed("manipulation"))
single_sim

#based on pilot slope - 0.167 diff in probability between dom and sub in conflict trials. 
#Power for predictor 'manipulation', (95% confidence interval),
#by number of levels in player:
#     10: 47.00% (36.94, 57.24) - 140 rows
#     20: 76.00% (66.43, 83.98) - 280 rows
#     30: 89.00% (81.17, 94.38) - 420 rows
#     40: 96.00% (90.07, 98.90) - 560 rows
#     50: 99.00% (94.55, 99.97) - 700 rows
#     60: 100.0% (96.38, 100.0) - 840 rows
#     70: 100.0% (96.38, 100.0) - 980 rows
#     80: 100.0% (96.38, 100.0) - 1120 rows
#     90: 100.0% (96.38, 100.0) - 1260 rows
#    100: 100.0% (96.38, 100.0) - 1400 rows


#based on smaller slope - 0.1 diff in probability between dom and sub in conflict trials. 
#Power for predictor 'manipulation', (95% confidence interval),
#by number of levels in player:
#     10: 15.00% ( 8.65, 23.53) - 140 rows
#     20: 37.00% (27.56, 47.24) - 280 rows
#     30: 44.00% (34.08, 54.28) - 420 rows
#     40: 61.00% (50.73, 70.60) - 560 rows
#     50: 74.00% (64.27, 82.26) - 700 rows
#     60: 77.00% (67.51, 84.83) - 840 rows
#     70: 85.00% (76.47, 91.35) - 980 rows
#     80: 91.00% (83.60, 95.80) - 1120 rows
#     90: 94.00% (87.40, 97.77) - 1260 rows
#    100: 96.00% (90.07, 98.90) - 1400 rows


#based on even smaller slope - 0.06 diff in probability between dom and sub in conflict trials. 
#by number of levels in player:
#     10:  8.00% ( 3.52, 15.16) - 140 rows
#     20: 14.00% ( 7.87, 22.37) - 280 rows
#     30: 23.00% (15.17, 32.49) - 420 rows
#     40: 22.00% (14.33, 31.39) - 560 rows
#     50: 28.00% (19.48, 37.87) - 700 rows
#     60: 37.00% (27.56, 47.24) - 840 rows
#     70: 44.00% (34.08, 54.28) - 980 rows
#     80: 45.00% (35.03, 55.27) - 1120 rows
#     90: 59.00% (48.71, 68.74) - 1260 rows
#    100: 62.00% (51.75, 71.52) - 1400 rows
#    110: 67.00% (56.88, 76.08) - 1540 rows # run analysis for 7 additional sample sizes (range 110-200).
#    120: 72.00% (62.13, 80.52) - 1680 rows
#    130: 72.00% (62.13, 80.52) - 1820 rows
#    140: 72.00% (62.13, 80.52) - 1960 rows
#    150: 77.00% (67.51, 84.83) - 2100 rows
#    160: 80.00% (70.82, 87.33) - 2240 rows
#    200: 86.00% (77.63, 92.13) - 2800 rows

#FIND ODDS RATIOS FOR DIFFERENT SLOPES
odds_ratio_convert = function(p0,p1){
    p0_odds = p0/(1-p0)
    p1_odds = p1/(1-p1)
    
    return(p1_odds/p0_odds)
    }

print(odds_ratio_convert(0.4285714, 0.49))


# Create a dataframe for each slope's power analysis results
power_data <- data.frame(
  sample_size = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, # slope = 0.167
                  10, 20, 30, 40, 50, 60, 70, 80, 90, 100, # slope = 0.1
                  10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 200),
  power = c(47.0, 76.0, 89.0, 96.0, 99.0, 100.0, 100.0, 100.0, 100.0, 100.0, # slope = 0.167
            15.0, 37.0, 44.0, 61.0, 74.0, 77.0, 85.0, 91.0, 94.0, 96.0, # slope = 0.1
            8.0, 14.0, 23.0, 22.0, 28.0, 37.0, 44.0, 45.0, 59.0, 62.0, 67.0, 72.0, 72.0, 72.0, 77.0, 80.0, 86.0), # slope = 0.06
  min = c(36.94, 66.43, 81.17, 90.07, 94.55, 96.38, 96.38, 96.38, 96.38, 96.38, # slope = 0.167
          8.65, 27.56, 34.08, 50.73, 64.27, 67.51, 76.47, 83.60, 87.40, 90.07, # slope = 0.1
          3.52, 7.87, 15.17, 14.33, 19.48, 27.56, 34.08, 35.03, 48.71, 51.75, 56.88, 62.13, 62.13, 62.13, 67.51, 70.82, 77.63), # slope = 0.06
  max = c(57.24, 83.98, 94.38, 98.90, 99.97, 100.0, 100.0, 100.0, 100.0, 100.0, # slope = 0.167
          23.53, 47.24, 54.28, 70.60, 82.26, 84.83, 91.35, 95.80, 97.77, 98.90, # slope = 0.1
          15.16, 22.37, 32.49, 31.39, 37.87, 47.24, 54.28, 55.27, 68.74, 71.52, 76.08, 80.52, 80.52, 80.52, 84.83, 87.33, 92.13), # slope = 0.06
  odds_ratio = c(rep("1.96 - Large", 10), rep("1.5 - Medium", 10), rep("1.28 - Small", 17)) # repeating odds_ratio values for each power analysis
)

palette <- brewer.pal(n = length(unique(power_data$slope)), name = "Set2")
power_data$slope = as.factor(power_data$slope)
power_plot = ggplot(data = power_data, aes(sample_size, power, ymin = min, ymax = max, color =  odds_ratio, group =  odds_ratio))+
  geom_pointrange(position = position_dodge2(0.2), size = .8)+
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,90,120,150,180,200))+
  scale_y_continuous(breaks = c(25,50,65, 80, 100))+
  geom_line(position = position_dodge2(0.3), color = "red", alpha = 0.6, linetype = 1, size = 1)+
  geom_hline(yintercept = 80, color = "red", linetype = 2)+
  labs(x = "Sample Size",y = "Power (%)", title = "Power estimates - subject random intercepts model", color = "Odds ratios (effect size)")+
  # Customize labels and title
  labs(x = "Sample Size", y = "Power (%)", 
       title = "Power Estimates - Subject Random Intercepts Model",
       color = "Odds ratios (effect size)") +
         scale_color_brewer(palette = "Set2") +
   theme(
    text = element_text(family = "serif"),  # Set font to serif
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title and make it bold
    axis.title.x = element_text(size = 14, face = "italic"),  # Italicize and resize axis labels
    axis.title.y = element_text(size = 14, face = "italic"),
    axis.text = element_text(size = 12),  # Resize axis text for better readability
    legend.position = "top",  # Move legend to the top
    legend.text = element_text(size = 12),  # Resize legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold legend title
  )+
  dark_theme_bw()

ggsave("power_plot.png",plot = power_plot, width = 8, height = 6)

result = wp.logistic(n = seq(100,3000,50), p0 = 0.4285714, p1 = 0.49, alpha = 0.05,
                    power = NULL, family = 'Bernoulli', parameter = 0.5)
result

