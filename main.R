library(readr)
library(ggplot2)
library(dplyr)

df = read_csv("clean_data.csv ")

df$unfair = ifelse(df$sent_amount >= 5, 1, 0)
df$manipulation = ifelse(df$manipulation == 1, "dom", "sub")

mean(df$unfair, na.rm = TRUE)


mean(df$sent_amount, na.rm = TRUE) #4.551724
mean(df$offer_response, na.rm = TRUE) #0.9272727
mean(df$trial_payoff)#4.25


aggregate(trial_payoff ~ manipulation + role, df, mean)

# Calculate mean of trial_payoff grouped by manipulation and role
means <- df %>%
  group_by(manipulation, role) %>%
  summarise(mean_trial_payoff = mean(trial_payoff))

# Plot
ggplot(data = means, aes(x = manipulation, y = mean_trial_payoff, fill = role)) +
  geom_col(position = "dodge") +
  labs(x = "Manipulation", y = "Mean trial_payoff") +
  theme_minimal()


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


