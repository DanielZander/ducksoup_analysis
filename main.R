library(readr)

df = read_csv("clean_data.csv ")

df$unfair = ifelse(df$sent_amount >= 5, "fairorbetter", "unfair")
df$unfair_1 = ifelse(df$sent_amount >= 5, 1, 0)
df$manipulation = ifelse(df$manipulation == 1, "dom", "sub")


mean(df$sent_amount, na.rm = TRUE) #4.551724
mean(df$offer_response, na.rm = TRUE) #0.9272727

df_dominant = df[df$role == "proposer",]
mean(df_dominant$sent_amount, na.rm = TRUE)
sum(df_dominant$offer_response, na.rm = TRUE )
mean(df_dominant$unfair_1, na.rm = TRUE)


df_sub = df[df$role == "responder",]
df_sub$dom_proposer = ifelse(df_sub$manipulation == "sub", "prop_dom", "prop_sub")
aggregate(offer_response ~ dom_proposer + unfair, df_sub, sum)




aggregate(offer_response ~ manipulation, df_sub, sum)


#     dom_proposer          unfair          offer_response
#1     prop_dom         fairorbetter              17
#2     prop_sub         fairorbetter              17
#3     prop_dom             unfair                9
#4     prop_sub             unfair                8


