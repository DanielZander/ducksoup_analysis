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
pilot_data$group_follows_individual <- as.integer(as.logical(as.character(pilot_data$group_follows_individual)))
pilot_data$manipulation = factor(pilot_data$manipulation, levels = c("submissive", "dominant"))

conflict_trials = pilot_data[pilot_data$condition == "Conflict", ]
aggregate(group_follows_individual ~ manipulation, conflict_trials, mean)
#  manipulation         group_follows_individual
#1     dominant                0.5952381
#2   submissive                0.4285714
#difference in probability = 0.167

model = glmer(group_follows_individual ~ manipulation + (1|player),
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

plogis(-0.3085+0.7322) - plogis(-0.3085)

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

intercept = plogis(-1.4903)
high_conf_sub = plogis(-1.4903 + 2.4587)
high_conf_dom = plogis(-1.4903 + 2.4587 + 0.6608)

high_conf_dom-high_conf_sub
#Increase due to dom manip = 0.1112595. 

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

#SELECT A SLOPE

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

df_sim = makeGlmer(group_follows_individual ~ manipulation + (1 | player),
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
power_data$odds_ratio = factor(power_data$odds_ratio, levels = c("1.28 - Small", "1.5 - Medium", "1.96 - Large"))


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
       title = "Power Estimates - Subject random intercepts model",
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
  theme_bw()

ggsave("power_plot.png",plot = power_plot, width = 8, height = 6)

result = wp.logistic(n = seq(100,3000,50), p0 = 0.4285714, p1 = 0.49, alpha = 0.05,
                    power = NULL, family = 'Bernoulli', parameter = 0.5)
result
# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#
#                  Simulation with all trials - interaction model 
# ---------------------------------------------------------------------------#
# ---------------------------------------------------------------------------#


# -----------------------Get slope estimates---------------------------------#

pilot_data$condition = factor(pilot_data$condition, levels=c( "Non-conflict", "Conflict"))
model_interact = glmer(group_follows_individual ~ manipulation * condition + (1 | player),
             data = pilot_data, family = "binomial")
summary(model_interact)

#Fixed effects:
#                                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                              2.4649     0.5034   4.896 9.78e-07 ***
#manipulationdominant                    -0.2033     0.6378  -0.319    0.750
#conditionConflict                       -2.7640     0.5742  -4.814 1.48e-06 ***
#manipulationdominant:conditionConflict   0.9037     0.7821   1.155    0.248


plogis(2.4649 + -0.2033 + -2.7640 + 0.9037) - plogis(2.4649 + -0.2033 + -2.7640)
#0.2220231
#The interaction between being in the dominant condition and conflict trials 
#increases the probability of the group following the individual by about 22% compared to when this interaction is not present.

# Create a new dataset for predictions based on unique combinations of manipulation and condition
new_data <- expand.grid(
  manipulation = levels(pilot_data$manipulation),
  condition = levels(pilot_data$condition),
  player = unique(pilot_data$player)[1]  # Use one player for prediction
)

# Get predicted probabilities (type="response" gives probabilities rather than log-odds)
new_data$predicted_prob <- predict(model_interact, newdata = new_data, type = "response")

# Plot interaction effect
ggplot(new_data, aes(x = condition, y = predicted_prob, fill = manipulation, group = manipulation)) +
  geom_col(position = position_dodge(width = 0.7)) + 
  labs(title = "Interaction Effect of Manipulation and Condition",
       y = "Predicted Probability", x = "Condition") +
  theme_minimal()



# -----------------------generate new data frame for all trials---------------------------------#
nsubjects = 200  # max sample size for analysis
ntrial_nonconflict = 20  # 20 trials for Non-conflict
ntrial_conflict = 14     # 14 trials for Conflict
subject_ID = 1:nsubjects

df = data.frame()
for (i in subject_ID) {
  player = rep(i, ntrial_nonconflict + ntrial_conflict)
  
  # Create 20 Non-conflict trials: 10 dominant, 10 submissive
  condition_nonconflict = rep("Non-conflict", ntrial_nonconflict)
  manipulation_nonconflict = c(rep("dominant", 10), rep("submissive", 10))
  
  # Create 14 Conflict trials: 7 dominant, 7 submissive
  condition_conflict = rep("Conflict", ntrial_conflict)
  manipulation_conflict = c(rep("dominant", 7), rep("submissive", 7))
  
  # Combine non-conflict and conflict trials for this participant
  condition = c(condition_nonconflict, condition_conflict)
  manipulation = c(manipulation_nonconflict, manipulation_conflict)
  
  # Shuffle the rows to randomize the order
  tmp = data.frame(player, condition, manipulation)
  tmp = tmp[sample(1:nrow(tmp)), ]
  
  # Add to the main dataframe
  df = rbind(df, tmp)
}

# Convert columns to factors
df = df %>%
  mutate(player = as.factor(player), 
         condition = as.factor(condition),
         manipulation = as.factor(manipulation))

df_summary_per_participant <- df %>%
  group_by(player, condition, manipulation) %>%
  summarise(count = n()) %>%
  ungroup()  # Optional: to remove grouping after summarizing

# Display the summary
df_summary_per_participant

#SELECT FIXED EFFECTS 
#fixed_effects = c(2.4649, -0.2033, -2.7640, 0.9037) #LARGE FROM PILOT DATA
#fixed_effects = c(2.4649, -0.2033, -2.7640, 0.6037) #MEDIUM INTERACTION SLOPE
fixed_effects = c(2.4649, -0.2033, -2.7640, 0.4037) #SMALLer INTERACTION SLOPE


df$condition = factor(df$condition, levels=c( "Non-conflict", "Conflict"))
df$manipulation = factor(df$manipulation, levels = c("submissive", "dominant"))
df_sim = makeGlmer(group_follows_individual ~ manipulation * condition + (1 | player),
                   family = "binomial", fixef = fixed_effects,
                   VarCorr =  list(matrix(c(0.5), 1, 1)), data = df) #participant variance based on pilot data.
summary(df_sim)


# Create a new dataset for predictions based on unique combinations of manipulation and condition
#new_data <- expand.grid(
 # manipulation = levels(df_sim$manipulation),
 # condition = levels(df_sim$condition),
 # player = unique(df_sim$player)[1]  # Use one player for prediction
#)

# Get predicted probabilities (type="response" gives probabilities rather than log-odds)
#new_data$predicted_prob <- predict(df_sim, newdata = new_data, type = "response")

# Plot interaction effect
#ggplot(new_data, aes(x = condition, y = predicted_prob, fill = manipulation, group = manipulation)) +
#  geom_col(position = position_dodge(width = 0.7)) + 
#  labs(title = "Interaction Effect of Manipulation and Condition",
#       y = "Predicted Probability", x = "Condition") +
#  theme_minimal()



pcurves=powerCurve(df_sim,breaks=c(60, 100, 130, 150, 170, 200),nsim=100, along="player", test = fixed("manipulationdominant:conditionConflict"))
pcurves
plot(pcurves)

#Based on pilot interaction-slope = 0.9037
#Power for predictor 'manipulationdominant:conditionConflict', (95% confidence interval),
#by number of levels in player:
#     10: 37.00% (27.56, 47.24) - 340 rows
#     20: 59.00% (48.71, 68.74) - 680 rows
#     30: 81.00% (71.93, 88.16) - 1020 rows
#     40: 89.00% (81.17, 94.38) - 1360 rows
#     60: 96.00% (90.07, 98.90) - 2040 rows
#     70: 98.00% (92.96, 99.76) - 2380 rows


#Based on medium interaction-slope = 0.6037
#Power for predictor 'manipulationdominant:conditionConflict', (95% confidence interval),
#by number of levels in player:
#     10: 13.00% ( 7.11, 21.20) - 340 rows
#     20: 25.00% (16.88, 34.66) - 680 rows
#     30: 32.00% (23.02, 42.08) - 1020 rows
#     40: 45.00% (35.03, 55.27) - 1360 rows
#     60: 69.00% (58.97, 77.87) - 2040 rows
#     70: 79.00% (69.71, 86.51) - 2380 rows
#     80: 81.00% (71.93, 88.16) - 2720 rows
#     90: 90.00% (82.38, 95.10) - 3060 rows
#    100: 91.00% (83.60, 95.80) - 3400 rows

#Based on small interaction-slope = 0.4037
#Power for predictor 'manipulationdominant:conditionConflict', (95% confidence interval),
#by number of levels in player:
#     60: 39.00% (29.40, 49.27) - 2040 rows
#    100: 62.00% (51.75, 71.52) - 3400 rows
#    130: 66.00% (55.85, 75.18) - 4420 rows
#    150: 75.00% (65.34, 83.12) - 5100 rows
#    170: 80.00% (70.82, 87.33) - 5780 rows
#    200: 86.00% (77.63, 92.13) - 6800 rows

# Create a dataframe for each slope's power analysis results based on the provided data
power_data_interaction <- data.frame(
  sample_size = c(10, 20, 30, 40, 60, 70, # pilot interaction-slope = 0.9037
                  10, 20, 30, 40, 60, 70, 80, 90, 100, # medium interaction-slope = 0.6037
                  60, 100, 130, 150, 170, 200), # small interaction-slope = 0.4037
  power = c(37.0, 59.0, 81.0, 89.0, 96.0, 98.0,  # pilot slope
            13.0, 25.0, 32.0, 45.0, 69.0, 79.0, 81.0, 90.0, 91.0,  # medium slope
            39.0, 62.0, 66.0, 75.0, 80.0, 86.0), # small slope
  min = c(27.56, 48.71, 71.93, 81.17, 90.07, 92.96,  # pilot slope
          7.11, 16.88, 23.02, 35.03, 58.97, 69.71, 71.93, 82.38, 83.60,  # medium slope
          29.40, 51.75, 55.85, 65.34, 70.82, 77.63), # small slope
  max = c(47.24, 68.74, 88.16, 94.38, 98.90, 99.76,  # pilot slope
          21.20, 34.66, 42.08, 55.27, 77.87, 86.51, 88.16, 95.10, 95.80,  # medium slope
          49.27, 71.52, 75.18, 83.12, 87.33, 92.13), # small slope
  slope = c(rep("Pilot (Slope = 0.9037)", 6), 
                 rep("Medium (Slope = 0.6037)", 9), 
                 rep("Small (Slope = 0.4037)", 6))
)

power_data_interaction$slope = factor(power_data_interaction$slope, levels = c("Small (Slope = 0.4037)", "Medium (Slope = 0.6037)", "Pilot (Slope = 0.9037)"))


palette <- brewer.pal(n = length(unique(power_data_interaction$slope)), name = "Set2")
power_data_interaction$slope = as.factor(power_data_interaction$slope)
power_plot_interaction = ggplot(data = power_data_interaction, aes(sample_size, power, ymin = min, ymax = max, color =  slope, group =  slope))+
  geom_pointrange(position = position_dodge2(0.2), size = .8)+
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,90,120,150,180,200))+
  scale_y_continuous(breaks = c(25,50,65, 80, 100))+
  geom_line(position = position_dodge2(0.3), color = "red", alpha = 0.6, linetype = 1, size = 1)+
  geom_hline(yintercept = 80, color = "red", linetype = 2)+
  labs(x = "Sample Size",y = "Power (%)", title = "Power estimates - subject random intercepts model", color = "Interaction Slope (effect size)")+
  # Customize labels and title
  labs(x = "Sample Size", y = "Power (%)", 
       title = "Power Estimates - Interaction model with subject random intercepts",
       color = "Interaction Slope (Log-odds scale)") +
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
  theme_bw()

  ggsave("power_plot_interaction.png",plot = power_plot_interaction, width = 8, height = 6)