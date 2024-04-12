library(readr)

df = read_csv("clean_data.csv ")


aggregate(sent_amount ~ is_dom + role, df, mean)

mean(df$sent_amount, na.rm = TRUE)

