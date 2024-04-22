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
df$other_manip = ifelse(df$manipulation == "dom", "sub", "dom")


################################################################################
##########################DESCRIPTIVE STATS AND PLOTTING########################
################################################################################
#acceptance rate and stuff
1-mean(df$unfair, na.rm = TRUE) #37.9% of offers were unfair, i.e. <5 coins. 

mean(df$offer_response, na.rm = TRUE) #0.9272727

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


# Calculate mean of sent_amount by player
means <- df %>%
  group_by(player) %>%
  summarise(mean_sent_amount = mean(sent_amount))
#plot
ggplot(means, aes(player, mean_sent_amount))+
  geom_col(color = "black")+
  geom_hline(yintercept = mean(df$sent_amount, na.rm = TRUE), color = "red", linetype = 2, size = 1)



#TRIAL PAYOFF
mean(df$sent_amount, na.rm = TRUE) #4.551724
trial_payoff_mean = mean(df$trial_payoff)#4.25
sd(df$trial_payoff) #1.9



#Trial_payoff grouped according to manipulation
aggregate(trial_payoff ~ manipulation, df, mean)
#       manipulation  trial_payoff
#1          dom         4.283333
#2          sub         4.216667

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
# Plot
ggplot(data = means, aes(x = manipulation, y = mean_trial_payoff, fill = role)) +
  geom_col(position = position_dodge(width = .5), color = "black") +
  labs(x = "Manipulation", y = "Mean trial_payoff") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 5))+
  geom_hline(yintercept = trial_payoff_mean, color = "red", linetype = 2, size = 1.2)+
  scale_fill_manual(values = c("blue", "green"))




#Reaction time 
mean(df$rt, na.rm = TRUE) #6140ms 
sd(df$rt, na.rm = TRUE) #5275ms
hist(df$rt)

resp_df = df[df$role == "responder", ]
aggregate(rt ~ other_manip, resp_df, mean)
aggregate(trial_payoff ~ manipulation + role, df, mean)
#       other_manip       rt
#1         dom          6255.813
#2         sub          5202.975



df_dominant = df[df$role == "proposer",]
mean(df_dominant$sent_amount, na.rm = TRUE)
sum(df_dominant$offer_response, na.rm = TRUE )

aggregate(offer_response ~ unfair_1 + manipulation, df ,mean)


df_sub = df[df$role == "responder",]
df_sub$dom_proposer = ifelse(df_sub$manipulation == "sub", "prop_dom", "prop_sub")
aggregate(offer_response ~ dom_proposer + unfair, df_sub, sum)




aggregate(offer_response ~ manipulation, df_sub, sum)


#     dom_proposer          unfair          offer_response
#1     prop_dom         fairorbetter              17
#2     prop_sub         fairorbetter              17
#3     prop_dom             unfair                9
#4     prop_sub             unfair                8


