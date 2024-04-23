################################################################################
##########################LOAD PACAKGES#########################################
################################################################################
library(readr)
library(ggplot2)
library(dplyr)

################################################################################
##########################FURTHER PREPROC#######################################
################################################################################
df = read_csv("clean_data.csv ")

df$unfair = ifelse(df$sent_amount >= 5, 1, 0)
df$manipulation = ifelse(df$manipulation == 1, "dom", "sub")
df$other_manip = ifelse(df$manipulation == "dom", "sub", "dom") #Manipulation of other player in group.


################################################################################
##########################DESCRIPTIVE STATS AND PLOTTING########################
#################################################################################
#acceptance rate and stuff
1-mean(df$unfair, na.rm = TRUE) #37.9% of offers were unfair, i.e. <5 coins. 

mean(df$offer_response, na.rm = TRUE) #92.72727% of offers are accepted.
aggregate(offer_response~unfair,df, mean)
#     unfair         offer_response
#1      0              0.8095238
#2      1              1.0000000

resp_df = df[df$role == "responder", ]
#acceptance rate of responders depending on the manipulation of proposer voice.
aggregate(offer_response ~ other_manip, resp_df, mean)
#     other_manip   offer_response
#1         dom        0.9285714
#2         sub        0.9259259

aggregate(offer_response ~ other_manip, resp_df, sum)
#       other_manip     offer_response
#1         dom              26
#2         sub              25


# 1 = fair, 0 = unfair.
aggregate(offer_response ~ unfair + other_manip, resp_df ,mean)
#     unfair    other_manip    offer_response
#1      0           dom         0.8181818
#2      1           dom         1.0000000
#3      0            sub        0.8000000
#4      1           sub         1.0000000

# 1 = fair, 0 = unfair.
aggregate(offer_response ~ unfair + other_manip, resp_df ,sum)
#     unfair    other_manip     offer_response
#1      0           dom                9
#2      1           dom                17
#3      0           sub                8
#4      1           sub                17



#PROPOSER SENT AMOUNT
mean(df$sent_amount, na.rm = TRUE) #4.551724
aggregate(sent_amount~round_nb, df , mean)

# Calculate mean of sent_amount by round_nb
means <- df %>%
  group_by(round_nb) %>%
  summarise(mean_sent_amount = mean(sent_amount))
#plot
ggplot(means, aes(round_nb, mean_sent_amount))+
  geom_col(color = "black")+
  geom_hline(yintercept = mean(df$sent_amount, na.rm = TRUE), color = "red", linetype = 2, size = 1)


#TRIAL PAYOFF
trial_payoff_mean = mean(df$trial_payoff)#4.25
sd(df$trial_payoff) #1.9

only_accept_df = df[df$offer_response == 1, ]
ggplot(only_accept_df, aes(trial_payoff))+
  geom_histogram(binwidth = 1, color = "black")

# Calculate mean of trial_payoff by player
means <- df %>%
  group_by(player) %>%
  summarise(trial_payoff_mean = mean(trial_payoff))
sd <- df %>%
  group_by(player) %>%
  summarise(trial_payoff_sd = sd(trial_payoff))
plot1_df = data.frame(trial_payoff_mean = means, trial_payoff_sd = sd)

#plot
ggplot(plot1_df, aes(trial_payoff_mean.player,trial_payoff_mean.trial_payoff_mean ))+
  geom_col(color = "black")+
  geom_hline(yintercept = mean(df$trial_payoff, na.rm = TRUE), color = "red", linetype = 2, size = 1)+
  geom_errorbar(aes(ymin = trial_payoff_mean.trial_payoff_mean  - trial_payoff_sd.trial_payoff_sd, ymax = trial_payoff_mean.trial_payoff_mean  + trial_payoff_sd.trial_payoff_sd), width = 0.2, color = "blue")+
  scale_y_continuous(limits = c(0, 10))

df_total_payoff = aggregate(trial_payoff ~ player, df, sum)
ggplot(df_total_payoff, aes(player, trial_payoff))+
  geom_col()

#What is special about player 1 and player 2?

#Trial_payoff grouped according to manipulation
aggregate(trial_payoff ~ manipulation, df, mean)
#       manipulation  trial_payoff
#1          dom         4.283333
#2          sub         4.216667


aggregate(trial_payoff ~ role, df, mean)

#Prediction 1: Responders will agree to receive less amount of money when faced with a vocally dominant Proposer. 
#Grouped according to both manip and role
aggregate(trial_payoff ~ manipulation + role, df , mean)
#      manipulation      role       trial_payoff
#1          dom        proposer       4.633333
#2          sub        proposer       4.400000
#3          dom        responder      3.933333
#4          sub        responder      4.033333


# Calculate mean of trial_payoff grouped by manipulation and role
means <- df %>%
  group_by(manipulation, role) %>%
  summarise(mean_trial_payoff = mean(trial_payoff))

sd <- df %>%
  group_by(manipulation, role) %>%
  summarise(sd_trial_payoff = sd(trial_payoff))
# Plot
ggplot(data = means, aes(x = manipulation, y = mean_trial_payoff, fill = role)) +
  geom_col(position = position_dodge(width = .5), color = "black") +
  labs(x = "Manipulation", y = "Mean trial_payoff") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 5))+
  geom_hline(yintercept = mean(df$trial_payoff), color = "red", linetype = 2, size = 1.2)+
  scale_fill_manual(values = c("darkgreen", "darkolivegreen1"))


#Reaction time 
mean(df$rt, na.rm = TRUE) #6140ms 
sd(df$rt, na.rm = TRUE) #5275ms
subset_df <- subset(df, rt <= 30000)
# Plot the histogram
hist(subset_df$rt)
ggplot(subset_df, aes(rt))+
  geom_histogram(color = "black")+
  scale_x_continuous(breaks = seq(0, 30000, by = 2000))  

aggregate(rt ~ other_manip, resp_df, mean)
#       other_manip       rt
#1         dom          6255.813
#2         sub          5202.975










