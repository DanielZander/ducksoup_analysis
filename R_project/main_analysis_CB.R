library(brms)
library(readxl)
library(BMS)
library(lme4)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(RColorBrewer)
library(ggdark)
library(patchwork)
library(ggthemes)
library(gridExtra)
library(car)
library(corrplot)
library(sjPlot)
library(bayestestR)
library(irr)
library(psych)
library(e1071)
library(beeswarm)
################################################################################
################################################################################
################################################################################
#Import data and difine functions
string_to_number = function(string){
  if (grepl("X", string)){string = 1}
  else if(grepl("Y",string)){string=2}
  else{string=3}
  return(string)
}

string_to_num_pair = function(pair){
  if (grepl("X",pair) & grepl("Y", pair)){pair = 12}
  else if(grepl("Y",pair) & grepl("Z", pair)){pair = 23}
  else{pair = 13}
  return(pair)
}

std = function(a) {
  sqrt(sum((a-mean(a))^2/(length(a)-1)))/sqrt(length(a))}


bernoulli_sd <- function(a) {
  a = mean(a)
  sqrt(a*(1-a))
}

df = read.csv("raw_data_main/tidy_data_main.csv")
df = df[, -c(1,3, 22:24)]

#pilot
# df = read.csv("raw_data_main/tidy_data_PILOT.csv")
# df = df[, -c(1,3, 22:24)]

all_faces = c("1 AF-224&1 AF-226", "1 AF-226&1 AF-205", "1 AF-224&1 AF-205", 
              "2 AF-230&2 AF-216", "2 AF-216&2 AF-256", "2 AF-230&2 AF-256",
              "3 BF-004&3 BF-025", "3 BF-025&3 BF-039", "3 BF-004&3 BF-039", 
              "4 BM-045&4 BM-210", "4 BM-210&4 BM-229", "4 BM-045&4 BM-229", 
              "5 BM-022&5 BM-041", "5 BM-041&5 BM-043", "5 BM-022&5 BM-043", 
              "6 LF-209&6 LF-214", "6 LF-214&6 LF-210", "6 LF-209&6 LF-210", 
              "7 LF-219&7 LF-225", "7 LF-225&7 LF-254", "7 LF-219&7 LF-254",
              "8 LM-243&8 LM-246", "8 LM-246&8 LM-251", "8 LM-243&8 LM-251", 
              "9 LM-220&9 LM-226", "9 LM-226&9 LM-235", "9 LM-220&9 LM-235", 
              "10 WF-212&10 WF-220", "10 WF-220&10 WF-243", "10 WF-212&10 WF-243", 
              "11 WF-021&11 WF-031", "11 WF-031&11 WF-230", "11 WF-021&11 WF-230", 
              "12 WM-010&12 WM-011", "12 WM-011&12 WM-232", "12 WM-010&12 WM-232", 
              "13 BF-217&13 BF-243", "13 BF-243&13 BF-244", "13 BF-217&13 BF-244", 
              "14 AM-211&14 AM-218", "14 AM-211&14 AM-253", "14 AM-218&14 AM-253", 
              "15 WM-029&15 WM-231", "15 WM-029&15 WM-257", "15 WM-231&15 WM-257",
              "16 BF-210&16 BF-212", "16 BF-210&16 BF-249", "16 BF-212&16 BF-249", 
              "17 WF-024&17 WF-205", "17 WF-024&17 WF-219", "17 WF-205&17 WF-219", 
              "18 LF-232&18 LF-249", "18 LF-232&18 LF-251", "18 LF-249&18 LF-251",
              "19 BM-239&19 BM-240","19 BM-240&19 BM-247","19 BM-239&19 BM-247",
              "20 AM-206&20 AM-213","20 AM-213&20 AM-230", "20 AM-206&20 AM-230")

df$unique_pair = NA
for (row in 1:nrow(df)){
  pair = c(df[row, "left_face"], df[row, "right_face"])
  face_1 = sub("\\_[^_]+$", "",pair[1])
  face_2 = sub("\\_[^_]+$", "",pair[2])
  idx = which(grepl(face_1, all_faces)& grepl(face_2, all_faces))
  if (length(idx) == 1){df[row, "unique_pair"] = all_faces[idx]}
}
remove(all_faces,face_1,face_2,idx,pair,row)
################################################################################
################################################################################
################################################################################
#Detection
df_main = df[df$part=="main_part", ]
df_main$condition = factor(ifelse(df_main$manipulated==0, "NM", "M"), levels = c("NM","M"))
df_main$face_pair = paste(df_main$left_face, "&", df_main$right_face)

df_main$triplet_idx = sapply(strsplit(df_main$left_face, " "), function(x) {
  as.numeric(x[1])
})
df_main <- df_main[complete.cases(df_main$triplet_idx),]
df_main = df_main[,colSums(is.na(df_main))<nrow(df_main)]
aggregate(detection ~ condition, df_main, mean)
#      condition     detection
# 1        NM        0.01995056
# 2        M         0.51059322


indv_detec = aggregate(detection ~ subject, df_main[df_main$condition=="M", ], mean)
indv_detec_std = aggregate(detection ~ subject, df_main[df_main$condition=="M", ], std)
indv_detec$std = indv_detec_std$detection

unique_pair_detec = aggregate(detection ~ unique_pair, df_main[df_main$condition=="M", ], mean)


df_main = merge(df_main, indv_detec[, c("subject", "detection")], by = "subject")
colnames(df_main)[c(22, 15)] = c("mean_detec","detection")
#pilot
#colnames(df_main)[c(22,16)] = c("mean_detec","detection")

prior = prior(normal(0, 1), class = b)
detection_model_bayes = brm(detection ~ condition + (condition|subject) + (condition|unique_pair),
                            data = df_main, family = "bernoulli",
                            control = list(adapt_delta = .95),
                            warmup = 2000, iter = 6000, chains = 4, cores = 4,
                            prior = prior,
                            save_all_pars = TRUE, file = "main_detection.rds")


bayesfactor_parameters(detection_model_bayes)
# Parameter   |       BF
# ----------------------
#   (Intercept) | 2.44e+31
# conditionM  | 1.02e+19
# 
# * Evidence Against The Null: 0

# When you call bayesfactor_parameters(detection_model_bayes), 
# the package brms computes the Bayes factor for each parameter
# in the model using a default alternative prior distribution,
# which is a normal distribution with a mean equal to the maximum 
# likelihood estimate of the coefficient and a standard deviation 
# equal to the standard error of the estimate. 


# Group-Level Effects: 
#   ~subject (Number of levels: 118) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                 0.98      0.16     0.69     1.31 1.00     5073     7837
# sd(conditionM)                2.22      0.23     1.80     2.72 1.01     1702     3824
# cor(Intercept,conditionM)    -0.45      0.13    -0.68    -0.15 1.01      790     2501
# 
# ~unique_pair (Number of levels: 60) 
#                             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                 0.23      0.15     0.01     0.56 1.00     3609     6305
# sd(conditionM)                0.72      0.17     0.41     1.08 1.00     1732     3151
# cor(Intercept,conditionM)    -0.51      0.44    -0.98     0.72 1.01      763      856
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     -4.28      0.17    -4.64    -3.96 1.00     7108     8390
# conditionM     4.13      0.28     3.60     4.67 1.00     4774     8460


detection_glme = glmer(detection ~ condition + (condition|subject) + (condition|unique_pair),
                       data = df_main, family = "binomial")
vcov(detection_glme)
getVarCov(detection_glme)
summary(detection_glme)

fixef = fitted(detection_model_bayes, newdata = data.frame(condition = unique(df_main$condition)), re_formula = NA)


new_main = expand.grid(condition = unique(df_main$condition), subject = unique(df_main$subject))
fitted_detection = fitted(detection_model_bayes, newdata = new_main, re_formula = '~(1|subject)', robust = TRUE)
pl = cbind(fitted_detection, new_main)
colnames(pl) = c("fit","er","lwr","upr","condition","subject")
indv_detec = aggregate(detection ~ subject + condition, df_main, mean)
pl = merge(pl, indv_detec, by = c("condition","subject"))

pl = pl[pl$condition=="M", ]





# 
# geom_col(color = "black", width = .75)+
#   scale_y_continuous(limits = c(0,90), breaks = c(0,25, 50,78, 90)) +
#   geom_hline(yintercept = 78, color = "black", linetype = 2, size = 1)+
#   labs(y = "Choice Consistency (%)", fill = "Condition", x = "")+
#   scale_fill_manual(values = c("#F7F7F7","#CCCCCC","#969696", "#464646"))+
#   theme_classic()+
#   geom_text(aes(label = paste0(c(82.1,82.2,87, 76), "%")), 
#             position = position_stack(vjust = 0.5), size = 6)+
#   theme(text=element_text(family="serif"),
#         axis.title.y = element_text(size = 22),
#         axis.text.y = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 12), 
#         legend.title = element_text(size = 14),
#         axis.text.x = element_blank(),
#         axis.ticks = element_blank())


ggplot(data = pl, aes(x = subject, y = fit, ymin = fit-er, ymax = fit+er))+
  geom_pointrange(linetype = 6, size = 1.5)+
  geom_point(data = pl, aes(subject, detection), color = "#838383", size = 5)+
  theme_classic()+ 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25,0.46, .50,.75, 1)) +
  geom_hline(yintercept = 0.46441860, linetype = 1, size = 2)+
  labs(x = "", y = "Detecion rate (probability scale)")+
  theme(text=element_text(family = "serif"),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.y = element_text(size = 33),
  axis.text.y = element_text(size = 25, face = "bold"))
ggsave(filename = "random_detec.png", width = 45, height = 18, units = 'cm', dpi = 1200)    



# Random effects:
#   Groups      Name        Variance Std.Dev. Corr 
# subject     (Intercept)   1.0800   1.0392        
# conditionM                4.7285   2.1745   -0.52 
# unique_pair (Intercept)   0.1382   0.3717        
# conditionM                0.6552   0.8094   -0.82
# Number of obs: 7080, groups:  subject, 118; unique_pair, 60


max(df_main$rt_choice)
max(df_main$rt_confidence)
max(df_main$rt_feedback)
#1015/60 = 16.93053

rt = aggregate(rt_choice ~ subject, df_main, max)
rt$rt_choice = rt$rt_choice/3600000
rt$rt_choice = rt$rt_choice*60

unique_detec = aggregate(detection ~ unique_pair, df_main, mean)
remove(unique_detec)
################################################################################
################################################################################
################################################################################
#Attr_phase

df_attr = df[df$part == "attractiveness_part", ]
df_attr = df_attr[,colSums(is.na(df_attr))<nrow(df_attr)]
df_attr = df_attr %>% mutate(triplet_idx = sub(".*/(\\d+).*", "\\1", face_attractiveness))

meta_df = read_xlsx("CFD 2.0.3 Norming Data and Codebook.xlsx")
meta_df = meta_df[-c(1:3), c(1:4,16)]
meta_df = meta_df[-c(1),-c(2:4) ]
colnames(meta_df) = c("new","rating")
meta_df$rating = as.numeric(meta_df$rating)

df_attr$new <- gsub("img/\\d+ |\\.jpg", "", df_attr$face_attractiveness)
idx = match(df_attr$new, meta_df$new)
df_attr["meta_rating"] = meta_df[idx, "rating"]


df_main$left_rating = NA
df_main$right_rating = NA
df_main$attr_consistent = NA
df_main$left_meta = NA
df_main$right_meta = NA
df_main$meta_cons = NA
df_attr[is.na(df_attr$attractiveness_rating), "attractiveness_rating" ] = 45


for (row in 1:nrow(df_main)){
  
  selected_face = df_main[row, "selected_face"]
  
  subject = df_main[row, "subject"]
  left_face = df_main[row, "left_face"]
  left_face = substring(left_face, 1, nchar(left_face)-2)
  rating_idx = which(grepl(left_face, df_attr[df_attr$subject == subject, "face_attractiveness"])&
                       df_attr$subject == subject)
  rating_left = df_attr[rating_idx, "attractiveness_rating"]
  df_main[row, "left_rating"] = rating_left 
  
  left_meta = df_attr[rating_idx, 15]
  df_main[row, "left_meta"] = left_meta
  
  
  right_face = df_main[row, "right_face"]
  right_face = substring(right_face, 1, nchar(right_face)-2)
  rating_idx = which(grepl(right_face, df_attr[df_attr$subject == subject, "face_attractiveness"])&
                       df_attr$subject == subject)
  rating_right = df_attr[rating_idx, "attractiveness_rating"]
  df_main[row, "right_rating"] = rating_right
  
  right_meta = df_attr[rating_idx, 15]
  df_main[row, "right_meta"] = right_meta
  
  if (rating_left > rating_right){
    if (grepl(left_face, df_main[row, "selected_face"])){df_main[row, "attr_consistent"] = 1}
    else{df_main[row, "attr_consistent"] = 0}
    
  }else if (rating_right > rating_left){
    if (grepl(right_face, df_main[row, "selected_face"])){df_main[row, "attr_consistent"] = 1}
    else{df_main[row, "attr_consistent"] = 0}
    
  }else if(rating_right == rating_left){
    print("indentical")
    df_main[row, "attr_consistent"] = NA
  }
  
  
  if(left_meta > right_meta){
    if (grepl(left_face, df_main[row, "selected_face"])){df_main[row, "meta_cons"] = 1}
    else{df_main[row, "meta_cons"] = 0}
  }
  else if (right_meta > left_meta){
    if (grepl(right_face, df_main[row, "selected_face"])){df_main[row, "meta_cons"] = 1}
    else{df_main[row, "meta_cons"] = 0}
  }
}
remove(left_face, rating_idx, rating_left, rating_right, right_face, row, selected_face,
       subject, idx, left_meta, meta_values, right_meta, meta_df)


df_main$diff = abs(df_main$left_rating-df_main$right_rating)
mean(df_main$diff) #13.68107
trip_sub_diff = aggregate(diff ~ subject + triplet_idx, df_main, mean)
# p = mean(df_main$attr_consistent, na.rm=TRUE)
# sd_p = sqrt(p*(1-p))
# mean(df_main$meta_cons)
# cor(as.numeric(df_main$attr_consistent), as.numeric(df_main$meta_cons),
#     method = "pearson", use = "complete.obs")


attr_con_mod = brm(attr_consistent ~ 1 + (1|subject), data = df)

summary(attr_con_mod)
fit = fitted(attr_con_mod, newdata = data.frame(attr_consistent = 1), re_formula = NA)




remove(indv_detec, indv_detec_std, trial_glm, detection_glm, detec_variance_raw)
################################################################################
################################################################################
################################################################################
#Create new df with 19 triplets for each subject, containing on each row,
#the 1st, 2nd and 3rd comparison and the respective selection on those comparisons. 

columns = c("subject","comparison_1","comparison_2","comparison_3",
            "select_1", "select_2", "select_3", "condition","feedback","detection", "triplet_idx")
triplet_df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(triplet_df) = columns

for (subject in unique(df_main$subject)){
  sub_df = df_main[df_main$subject == subject, ]
  
  for (triple in unique(sub_df$triplet_idx)){
    trial_df = sub_df[sub_df$triplet_idx == triple, ]
    
    selection_df = data.frame(subject = subject, comparison_1 = trial_df[1, "face_pair"],
                              comparison_2 =  trial_df[2, "face_pair"], 
                              comparison_3 = trial_df[3, "face_pair"],
                              select_1 = trial_df[1, "selected_face"],
                              select_2 = trial_df[2, "selected_face"],
                              select_3 = trial_df[3, "selected_face"],
                              condition = trial_df[3, "condition"],
                              triplet_idx = trial_df[1, "triplet_idx"],
                              detection = trial_df[3, "detection"],
                              feedback = trial_df[3, "feedbacked_face"])
    triplet_df = rbind(triplet_df, selection_df)
  }
  remove(sub_df, selection_df, trial_df)
}
remove(columns, subject, triple)
triplet_df = merge(triplet_df,trip_sub_diff, by = c("subject","triplet_idx"))
remove(trip_sub_diff)


triplet_df$CB_transitive = NA
triplet_df$pattern_r1 = NA
triplet_df$intrans_r1 = NA
triplet_df$target_comparison = NA
for (row in 1:nrow(triplet_df)){
  tmp = triplet_df[row, c(2:8)]
  X_Y = which(grepl("X", tmp) & grepl("Y", tmp))
  Y_Z = which(grepl("Y", tmp) & grepl("Z", tmp))
  X_Z = which(grepl("X", tmp) & grepl("Z", tmp))
  
  X_Y_select = string_to_number(tmp[X_Y+3])
  Y_Z_select = string_to_number(tmp[Y_Z+3]) 
  X_Z_select = string_to_number(tmp[X_Z+3])
  pattern = as.numeric(paste0(X_Y_select,Y_Z_select,X_Z_select))
  
  triplet_df[row, "pattern_r1"] = pattern
  
  if (pattern == 121 || pattern == 221){
    triplet_df[row, "CB_transitive"] = 1
  }else{triplet_df[row, "CB_transitive"] = 0}
  
  if (pattern == 123 || pattern == 231){
    triplet_df[row, "intrans_r1"] = 1
  }else{ triplet_df[row, "intrans_r1"] = 0}
  
}
remove(row, tmp, pattern, sel1,sel2,sel3, X_Y, X_Y_select, X_Z_select, Y_Z, Y_Z_select,X_Z)

aggregate(CB_transitive ~ condition, triplet_df, mean)
aggregate(diff ~ CB_transitive, triplet_df, mean)
aggregate(detection ~ CB_transitive + condition, triplet_df,mean)
mean(triplet_df$intrans_r1)
aggregate(intrans_r1 ~ condition, triplet_df, mean)
aggregate(diff ~ condition, triplet_df, mean)

pattern_means = table(triplet_df$pattern_r1, triplet_df$condition)
percentage <- prop.table(pattern_means, margin = 2) * 100



# condition natural_intrans
# 1        NM      0.06673729
# 2         M      0.04237288

tmp = triplet_df[, c("subject","triplet_idx", "condition", "CB_transitive", "pattern_r1", "intrans_r1")]
df_main= merge(df_main, tmp, by = c("subject","triplet_idx"))
colnames(df_main)[c(20, 30)] = c("trial_cond", "trip_cond")
remove(tmp)


aggregate(diff ~ trip_cond, df_main, mean)
aggregate(confidence_rating ~ trip_cond, df_main, mean)



df_main$CB_transitive = as.factor(df_main$CB_transitive )
df_main$attr_consistent = as.factor(df_main$attr_consistent)
prior = prior(normal(0, 1), class = b)
detection_model_bayes = brm(detection ~ trial_cond + (condition|subject) + (condition|unique_pair)+
                              CB_transitive + attr_consistent
                            data = df_main, family = "bernoulli",
                            control = list(adapt_delta = .95),
                            warmup = 2000, iter = 6000, chains = 4, cores = 4,
                            prior = prior(normal(0, 1), class = b),
                            save_all_pars = TRUE,
                            file = "main_detection.rds")

df_main_M = df_main[df_main$trial_cond == "M", ]
tmp = triplet_df2[triplet_df2$condition == "M", c("subject","pattern_con")]
idx = match(df_main_M$subject, tmp$subject)
df_main_M$pattern_con = tmp[idx, "pattern_con"]

aggregate(detection ~ pattern_con, df_main_M, mean)

tmp = consistency_df[consistency_df$trial_cond == "M", c("subject", "consistent")]
idx = match(df_main_M$subject, tmp$subject)
df_main_M$consistent = tmp[idx, "consistent"]
mean(df_main_M$consistent)

aggregate(detection ~ CB_transitive, df_main_M, mean)

# CB_transitive detection
# 1             0 0.4002999
# 2             1 0.6088117
aggregate(detection ~ CB_transitive + attr_consistent, df_main_M, mean)
# attr_consistent         CB_transitive       detection
# 1               0             0               0.3397683
# 2               1             0               0.4438642
# 3               1             1               0.6114911

cor(as.numeric(df_main_M$CB_transitive), as.numeric(df_main_M$attr_cons),use = "complete.obs")



aggregate(attr_consistent ~ meta_cons, df_main, mean)

mtx = as.matrix(table(df_main$CB_transitive, df_main$attr_consistent))
phi(mtx)
# 0.46. 





aggregate(detection ~ attr_consistent, df_main_M, mean)
aggregate(detection ~ meta_cons, df_main_M, mean)
aggregate(meta_cons ~ CB_transitive, df_main, mean)
aggregate(attr_consistent ~ CB_transitive, df_main, mean)
aggregate(meta_cons ~ pattern_r1, df_main, mean)
aggregate(attr_consistent ~ pattern_r1, df_main, mean)
aggregate(CB_transitive ~ meta_cons, df_main, mean)
aggregate(attr_consistent ~ CB_transitive, df_main, mean)
#        meta_cons              CB_transitive
# 1         0                    0.4864141
# 2         1                     0.5475711

#          CB_transitive      attr_consistent
# 1             0            0.4472854
# 2             1            0.8817416



df_main$comp = NA
for (row in 1:nrow(df_main)){
  idx = string_to_num_pair(df_main[row, "face_pair"])
  if (idx == 12 || idx == 21){df_main[row, "comp"] = "XY"}
  else if (idx == 23 || idx == 32){df_main[row, "comp"] = "YZ"}
  else {df_main[row, "comp"] = "XZ"}
}

attr = aggregate(attr_consistent ~ comp, df_main, mean)
colnames(attr) = c("comp", "fit")
attr$type = rep("attr", 3)
meta = aggregate(meta_cons ~ comp, df_main, mean)
meta$type = rep("meta", 3)
colnames(meta) = c("comp", "fit", "type")
pl = rbind(attr, meta)
comp_cc = ggplot(data = pl, aes(comp,fit, fill= type ))+
  geom_col(position = position_dodge2(0.5), color = "black")+
  scale_fill_brewer(palette = "Set1")+
  ylim(0,0.9)+
  ylab("First round choice CC")
ggsave(filename = "comp_cc.png", width = 18, height = 10, units = 'cm', dpi = 600)  

aggregate(CB_transitive ~ meta_cons + attr_consistent, df_main, mean)
x = df_main[df_main$meta_cons == 1, ]
y = df_main[df_main$attr_consistent == 1, ]
cor(as.numeric(df_main$attr_consistent), as.numeric(df_main$CB_transitive), method = "pearson", use = "complete.obs")

# df_main$weird = NA
# for (row in 1:nrow(df_main)){
#   left = string_to_number(df_main[row, "left_face"])
#   right =  string_to_number(df_main[row, "right_face"])
#   if(left < right){
#     if(df_main[row, "left_rating"] >= df_main[row, "right_rating"]){df_main[row, "weird"] = 0}
#     else{df_main[row, "weird"] = 1}
#   }
#   else if(right < left){
#     if(df_main[row, "right_rating"] >= df_main[row, "left_rating"]){df_main[row, "weird"] = 0}
#     else{df_main[row, "weird"] = 1}
#   }
# }
# weird = df_main_weird[df_main_weird$weird == 1, c(1, 7:10, 19:25, 35)]




df_main_M$CB_transitive = as.factor(df_main_M$CB_transitive)
df_main_M$attr_consistent = as.factor(df_main_M$attr_consistent)

detection_mod_CB = glmer(detection ~ CB_transitive + attr_consistent + diff + 
                   confidence_rating + (1|subject) + (1|unique_pair),
                   data = df_main_M,family = "binomial")

summary(detection_mod_CB)
fit = fitted(detection_mod_CB)
plot_model(detection_mod_CB, type = "pred")

#   Random effects:
#   Groups      Name        Variance Std.Dev.
# subject     (Intercept)   3.4019   1.8444  
# unique_pair (Intercept)   0.2269   0.4763  
# Number of obs: 1373, groups:  subject, 118; unique_pair, 60
# 
# Fixed effects:
#                     Estimate   Std. Error z value Pr(>|z|)    
# (Intercept)         -1.412716   0.313319  -4.509 6.52e-06 ***
#   CB_transitive1    0.711173    0.170608   4.168 3.07e-05 ***
#   attr_consistent1  0.241080    0.214993   1.121 0.262145    
# diff                0.005584    0.005916   0.944 0.345305    
# confidence_rating   0.015577    0.004140   3.762 0.000168 ***

vif(detection_mod_CB)
# 
# CB_transitive   attr_consistent        diff        confidence_rating 
# 1.300948          1.316470          1.070513           1.052583 

table(df_main_M$pattern_r1, df_main_M$CB_transitive)
#       nonCB    CB
# 121    0      469
# 131   268      0
# 133   135      0
# 221   0       280
# 223   124      0
# 233   80       0

tmp = triplet_df[, c("subject", "triplet_idx", "diff")]
df_main_221_131 <- df_main_M[df_main_M$pattern_r1 %in% c(221, 131), ]
aggregate(detection ~ pattern_r1, df_main_221_131, mean)
# pattern_r1 detection0.4701493
# 1        131 
# 2        221 0.5964286


df_main_221_131=merge(df_main_221_131, tmp, by = c("subject", "triplet_idx"))
colnames(df_main_221_131)[36] = "trip_diff"
colnames(df_main_221_131)[29] = "trial_diff"
# pattern_r1 trip_diff
# 1        131  13.49005
# 2        221  13.09762

aggregate(pattern_con ~ pattern_r1, df_main_221_131, mean)


table(df_main_221_131$pattern_r1)
aggregate(trial_diff~ df_main_221_131$pattern_r1,df_main_221_131, mean )
# df_main_221_131$pattern_r1     diff
# 1                        131 20.21642
# 2                        221 11.26071
aggregate(trip_diff ~ pattern_r1,df_main_221_131, mean )
aggregate(confidence_rating ~ df_main_221_131$pattern_r1,df_main_221_131, mean )

ggplot(data = df_main_221_131, aes(confidence_rating, detection))+
  geom_smooth(method = 'lm')

df_main_221_131$pattern_r1 = as.factor(df_main_221_131$pattern_r1)
mod_221_131 = glm(detection ~ pattern_r1 + trial_diff + confidence_rating, family = "binomial", 
                  data = df_main_221_131)
summary(mod_221_131)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)       -0.580629   0.262820  -2.209  0.02716 * 
#   pattern_r1221      0.562703   0.186230   3.022  0.00251 **
#   diff               0.003491   0.007534   0.463  0.64307   
# confidence_rating  0.007453   0.003784   1.970  0.04889 * 
x = lm(trial_diff ~ pattern_r1, df_main_221_131)

df_main_221_131$trial_diff_scale = scale(df_main_221_131$trial_diff)
df_main_221_131$confidence_scale = scale(df_main_221_131$confidence_rating)
df_main_221_131$pattern_r1 = as.factor(df_main_221_131$pattern_r1)
df_main_221_131$pattern_con = as.factor(df_main_221_131$pattern_con)
aggregate(confidence_rating ~ pattern_r1, df_main_221_131, mean)
aggregate(detection ~ pattern_con, df_main_221_131, mean)

prior = prior(normal(0, 1), class = b)
mod_221_131_bayes = brm(detection ~ pattern_r1 + trial_diff_scale +
                          confidence_scale + pattern_con + (pattern_r1 + confidence_scale + trial_diff_scale|subject) +
                          (1|unique_pair), data = df_main_221_131, family = "bernoulli",
                           control = list(adapt_delta = .95), file = "mod_221_131_bayes.rds",
                           warmup = 2000, iter = 4000, chains = 4, cores = 4, prior = prior)
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept           -0.29      0.32    -0.92     0.31 1.00     4262     5094
# pattern_r1221        1.00      0.30     0.42     1.60 1.00    10258     6447
# diff_scale           0.12      0.19    -0.25     0.48 1.00     8354     6225
# confidence_scale     0.52      0.21     0.12     0.95 1.00     6776     6091
                        
bayesfactor_parameters(mod_221_131_bayes)
# Parameter        |    BF
# ------------------------
#   (Intercept)      |  2.71
# pattern_r1221    | 48.61
# trial_diff_scale | 0.199
# confidence_scale |  9.74
# pattern_con1     | 13.74

new_data = expand.grid(pattern_r1 = unique(df_main_221_131$pattern_r1), confidence_scale = 0,
                      trial_diff_scale = 0, pattern_con = unique(df_main_221_131$pattern_con))

fit = fitted(mod_221_131_bayes, new_data, re_formula = NA)
pl = cbind(fit, new_data)


fit_nosum = fitted(mod_221_131_bayes, new_data, re_formula = NA, summary = FALSE)

av_CB = (fit_nosum[, 2] + fit_nosum[, 4])/2
av_noCB = (fit_nosum[, 1] + fit_nosum[, 3])/2
contrast = quantile(av_CB - av_noCB, probs = c(.5, .025, .975))
# 50%       2.5%      97.5% 
# 0.21438646 0.08505622 0.34180233 

contrastd = av_CB-av_noCB
mean(contrastd > 0)

d = data.frame(contrast = contrastd)

ggplot(d, aes(contrastd))+
  geom_histogram()+
  theme_classic()+
  theme(
    axis.line.y = element_blank()
    
  )

# 
# scale_y_continuous(limits = c(0,90), breaks = c(0,25, 50,78, 90)) +
#   geom_hline(yintercept = 78, color = "black", linetype = 2, size = 1)+
#   labs(y = "Choice Consistency (%)", fill = "Condition", x = "")+
#   scale_fill_manual(values = c("#F7F7F7","#CCCCCC","#969696", "#464646"))+
#   theme_classic()+
#   geom_text(aes(label = paste0(c(82.1,82.2,87, 76), "%")), 
#             position = position_stack(vjust = 0.5), size = 6)+
#   theme(text=element_text(family="serif"),
#         axis.title.y = element_text(size = 22),
#         axis.text.y = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 12), 
#         legend.title = element_text(size = 14),
#         axis.text.x = element_blank(),
#         axis.ticks = element_blank())



# posterior_diff = ggplot(data = d, aes(diff))+
#   geom_histogram(bins = 50, color = "black", alpha = 0.5)+
#   geom_vline(xintercept = 0, linetype = 2)+
#   scale_x_continuous(breaks=c(0,.05,.1, .15,.2,.25))+
#   theme_blank()+
#   xlim(-.15,.35)+
#   labs(x = "", y = "")+
#   theme(
#     axis.title.x = element_text(size = 14, color = "black"
#     ),
#     axis.title.y = element_text(size = 10, color = "black"),
#     axis.text.x = element_text(face = "bold", size = 7, color = "black", angle=25),
#     axis.line.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank())
# 
# c = both_posteriors + inset_element(posterior_diff, left = -0.1,
#                                     bottom = 0.1, right = 1, top = 1)
# ggsave(filename = "c.png", width = 18, height = 10, units = 'cm', dpi = 1000)

mean(df_main_221_131$detection)
obs = aggregate(detection ~ pattern_r1, df_main_221_131, mean)
# geom_text(aes(label = c("DR = 42.6% [28, 57]", "DR = 66.2% [52, 79]")), 
#           position = position_stack(vjust = 0.5), size = 6, family = "serif")+

colnames(pl) = c("fit","er","lwr","upr","Choice_pattern","conf","diff", "pattern_con")
pl["CB_Transitive"] = c("Non_CB_Transitive", "CB_Transitive")
ggplot(data = pl, aes(x = Choice_pattern, y = fit, ymin = lwr, ymax = upr, fill = CB_Transitive, color = pattern_con))+
  geom_col(position = position_dodge2(0.2), width = 0.2)+
  geom_errorbar(aes(Choice_pattern, fit, ymin = lwr, ymax = upr), width = 0.15, position = position_dodge2(0.2))+
  theme_classic()+
  geom_hline(yintercept = 0.5346715, linetype = 2, size = 1.2)+
  scale_fill_manual(values = c("#F7F7F7", "#464646"), name = "CB_Transitive", labels = c("CB-Transitive", "Non-CB-Transitive"))+
  ylim(0,.9)+
  labs(x = "", y = "Detection rate (probability scale)")+
  theme(
    text=element_text(family="serif"),
    axis.text.x = element_text(size =20, face = "bold"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 15, face = "bold"),
    legend.position = c(0.1, .7),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.text = element_text(size = 25, family = "serif"),
    legend.title = element_blank(),
    strip.text = element_text(size = 16, face = "bold")
  )
ggsave(filename = "CB_transplot.png", width = 20, height = 16, units = 'cm', dpi = 1200) 

cb_trans = ggplot(data = pl, aes(x = Choice_pattern, y = fit, ymin = lwr, ymax = upr, fill = CB_Transitive, linetype = pattern_con)) +
  geom_col(position = position_dodge2(0.1), width = 0.8, color = "black", size = 1.1) +
  geom_errorbar(aes(Choice_pattern, fit, ymin = lwr, ymax = upr),size = 1.1, width = 0.4, position = position_dodge(0.8)) +
  theme_classic() +
  geom_hline(yintercept = 0.5346715, linetype = 2, size = 1.2) +
  scale_fill_manual(values = c("#464646", "#969696"), name = "CB_Transitive", labels = c("CB-Transitive", "Non-CB-Transitive")) +
  ylim(0, 1) +
  labs(x = "", y = "Detection rate (probability scale)") +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 18, face = "bold"),  # Adjust the size of x-axis tick labels
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 15, face = "bold"),
    legend.position = c(.1, .9),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.text = element_text(size = 20, family = "serif"),
    legend.title = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    aspect.ratio = 5/3)
ggsave(filename = "cb_trans.png", width = 20, height = 22, units = 'cm', dpi = 1200) 
#969696
#F7F7F7


new = expand.grid(pattern_r1 = unique(df_main_221_131$pattern_r1), confidence_scale = 0,
                  trial_diff_scale = 0, pattern_con = unique(df_main_221_131$pattern_con))

fit = fitted(mod_221_131_bayes, new_data, re_formula = NA)
pl = cbind(fit, new_data)



df_main_M_nointrans = df_main_M[df_main_M$intrans_r1 == 0, ]
df_main_M_nointrans$pattern_r1 = factor(df_main_M_nointrans$pattern_r1, 
                                        levels = c(121,221,131,133,223,233))

pattern_detec = aggregate(detection ~ pattern_r1, df_main_M_nointrans, mean)
pattern_detec_std = aggregate(detection ~ pattern_r1, df_main_M_nointrans, std)
pattern_detec$std = pattern_detec_std$detection
pattern_detec$CB_trans = c(rep("CB_trans",2), rep("Non_CB_trans", 4))
pattern_detec$attr_incon = as.factor(c(0,1,1,2,2,3))

pattern_detec = ggplot(data = pattern_detec, aes(pattern_r1,detection, fill = CB_trans, color = attr_incon))+
  geom_col(width = .8)+
  geom_errorbar(aes(pattern_r1, detection, ymin = detection-std, ymax = detection+std),
                width = .3)+
  ylim(0,.8)+
  scale_fill_grey()+
  theme_bw()
#ggsave(filename = "pattern_detec.png", width = 18, height = 10, units = 'cm', dpi = 1000)  



df_main_M$CB_transitive = factor(ifelse(df_main_M$CB_transitive==1, "CB_trans","Non_CB_trans"), 
                                 levels = c("Non_CB_trans", "CB_trans"))
df_main_M$attr_consistent = as.factor(df_main_M$attr_consistent)

obs = aggregate(detection ~ CB_transitive + attr_consistent, df_main_M, mean)
prior = prior(normal(0, 1), class = b)
df_main_M["diff_scale"] = scale(df_main_M$diff)
df_main_M["confidence_scale"] = scale(df_main_M$confidence_rating)
colnames(df_main_M)[c(31,32)] = c("diff_scale", "confidence_scale")
detec_cbtrans_bayes = brm(detection ~ CB_transitive + attr_consistent + diff_scale +
                            confidence_scale + (diff_scale + confidence_scale + CB_transitive + attr_consistent|subject) +
                            (attr_consistent|unique_pair), prior = prior,
                          data = df_main_M, family = "bernoulli",
                          control = list(adapt_delta = .95), file = "detec_cbtrans_bayes.rds",
                          warmup = 2000, iter = 4000, chains = 4, cores = 4)

#plogis(-0.56 + 0.71+ 0.07 + 0.39)-plogis(-0.56+0.30+0.07+0.39) = ~.1
#SO CB_transitive conditions are 10% more likely to be detected. 

bayesfactor_parameters(detec_cbtrans_bayes, verbose = FALSE)
# Parameter             |    BF
# -----------------------------
#   (Intercept)           |  1.10
# CB_transitiveCB_trans | 87.98
# attr_consistent1      | 0.539
# diff_scale            | 0.119
# confidence_scale      | 16.90
#log10(135) #=2.130334. 

# An alternative table, widely cited, is provided by Kass and Raftery (1995)
# log10 K	K	Strength of evidence
# 0 to 1/2	1 to 3.2	Not worth more than a bare mention
# 1/2 to 1	3.2 to 10	Substantial
# 1 to 2	10 to 100	Strong
# > 2	> 100	Decisive

#So there is decisive evidence for CB_transitivity having an effect.

#Model Output:

# Group-Level Effects: 
#                   ~subject (Number of levels: 118) 
#                                               Estimate Est.Error l-95% CI u-95% CI
# sd(Intercept)                                   1.86      0.26     1.39     2.40 
# sd(CB_transitiveCB_trans)                       0.49      0.30     0.03     1.12 
# sd(attr_consistent1)                            0.35      0.25     0.02     0.92 
# cor(Intercept,CB_transitiveCB_trans)            0.29      0.41    -0.65     0.92
# cor(Intercept,attr_consistent1)                 0.09      0.47    -0.81     0.88 
# cor(CB_transitiveCB_trans,attr_consistent1)    -0.03      0.50    -0.89     0.87 
# 
# ~unique_pair (Number of levels: 60) 
#                                   Estimate Est.Error l-95% CI u-95% CI
# sd(Intercept)                       0.34      0.23     0.01     0.86 
# sd(attr_consistent1)                0.63      0.25     0.12     1.13 
# cor(Intercept,attr_consistent1)    -0.15      0.53    -0.91     0.92
# 
# Population-Level Effects: 
#                         Estimate Est.Error l-95% CI u-95% CI 
# Intercept                -0.54      0.25    -1.04    -0.03 
# CB_transitiveCB_trans     0.70      0.19     0.33     1.06 
# attr_consistent1          0.27      0.24    -0.18     0.74 
# diff_scale                0.08      0.09    -0.09     0.25 
# confidence_scale          0.37      0.10     0.17     0.58 

newdata = expand.grid(attr_consistent= unique(df_main_M$attr_consistent),
                     CB_transitive = unique(df_main_M$CB_transitive))
newdata = newdata[complete.cases(newdata$attr_consistent),]
newdata = cbind(newdata, data.frame(diff_scale = NA, confidence_scale= NA))
fit = fitted(detec_cbtrans_bayes, newdata, re_formula = NA)



mcmc = as_draws_df(detec_cbtrans_bayes)
mcmc = mcmc[, c(1:5)]
intercept = plogis(quantile(mcmc$b_Intercept, probs = c(.5, .025, .975))) 
# # 50%         2.5%     97.5% 
# 0.3616754 0.2547133 0.4865801 
attr1_non_trans = plogis(quantile(mcmc$b_Intercept + mcmc$b_diff_scale + mcmc$b_confidence_scale+ mcmc$b_attr_consistent1, probs = c(.5, .025, .975)))
attr1_CB_trans = plogis(quantile(mcmc$b_Intercept + mcmc$b_diff_scale + mcmc$b_confidence_scale+ mcmc$b_CB_transitiveCB_trans, probs = c(.5, .025, .975)))

pl = data.frame(fit = c(attr1_non_trans[1],attr1_CB_trans[1]),
                 lwr = c(attr1_non_trans[2],attr1_CB_trans[2]),
                 upr = c(attr1_non_trans[3],attr1_CB_trans[3]),
                 CB_trans = as.factor(c("nonCB","CB")))


ggplot(data = pl, aes(CB_trans, fit, ymin = lwr, ymax = upr, group = "CB_trans"))+
  geom_pointrange()+
  theme_bw()+
  ylim(0,1)


attr1_non_trans_fit = plogis(mcmc$b_Intercept + mcmc$b_diff_scale + mcmc$b_confidence_scale+ mcmc$b_attr_consistent1)
attr1_CB_trans_fit = plogis(mcmc$b_Intercept + mcmc$b_diff_scale + mcmc$b_confidence_scale+ mcmc$b_CB_transitiveCB_trans)
dx = data.frame(fit = c(attr1_non_trans_fit, attr1_CB_trans_fit), 
               cond = c(rep("Non-CB-transitive",length(attr1_non_trans_fit)),rep("CB-transitive", length(attr1_CB_trans_fit))))

both_posteriors = ggplot(data = dx, aes(x = fit, fill = cond))+
  geom_density(alpha = .85)+
  scale_fill_manual(values = c(grey(.2), grey(.9))) +
  ylim(0,8)+
  xlim(0.30,.9)+
  theme_bw()+
  labs(x = "Detection Rate",y = "Density", title = "Effect of CB-transitivity on Detection Rate", fill = "CB-Transitive comparison") +
  theme(plot.title = element_text(size = 18,hjust = 0.5, family = "serif"))+
  theme(
    axis.title.x = element_text(size = 14, color = "black"
    ),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    legend.text = element_text(face = "bold", size = 10))+
  theme(text=element_text(family="serif"))


nonCB = plogis(mcmc$b_Intercept + mcmc$b_diff_scale + mcmc$b_confidence_scale+ mcmc$b_attr_consistent1)
CB = plogis(mcmc$b_Intercept + mcmc$b_diff_scale + mcmc$b_confidence_scale+ mcmc$b_CB_transitiveCB_trans)
diff = CB-nonCB
mean(diff>0)

d = data.frame(diff = diff)
mean(d>0)
posterior_diff = ggplot(data = d, aes(diff))+
  geom_histogram(bins = 50, color = "black", alpha = 0.5)+
  geom_vline(xintercept = 0, linetype = 2)+
  scale_x_continuous(breaks=c(0,.05,.1, .15,.2,.25))+
  theme_blank()+
  xlim(-.15,.35)+
  labs(x = "", y = "")+
  theme(
    axis.title.x = element_text(size = 14, color = "black"
    ),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(face = "bold", size = 7, color = "black", angle=25),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
 
c = both_posteriors + inset_element(posterior_diff, left = -0.1,
    bottom = 0.1, right = 1, top = 1)
ggsave(filename = "c.png", width = 18, height = 10, units = 'cm', dpi = 1000)

mean(d>0.1)

remove(c,d,detec_cbtrans_bayes, mcmc, dx, posterior_diff, attr1_CB_trans, 
       attr1_CB_trans_fit, attr1_non_trans, attr1_non_trans_fit, CB, intercept,
       NonCB, both_posteriors)
################################################################################
################################################################################
################################################################################
#Second round choice
df_second = df[df$part=="second_part", ]
df_second = df_second[,colSums(is.na(df_second))<nrow(df_second)]
df_second$face_pair = paste(df_second$left_face, "&", df_second$right_face)
df_second$triplet_idx= sapply(strsplit(df_second$left_face, " "), function(x) {
  as.numeric(x[1])
})
columns = c("subject","comparison_1","comparison_2","comparison_3",
            "select_1", "select_2", "select_3", "condition", "triplet_idx")
triplet_df2 = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(triplet_df2) = columns
for (subject in unique(df_second$subject)){
  sub_df = df_second[df_second$subject == subject, ]
  
  for (triple in unique(sub_df$triplet_idx)){
    trial_df = sub_df[sub_df$triplet_idx == triple, ]
    
    selection_df = data.frame(subject = subject, comparison_1 = trial_df[1, "face_pair"],
                              comparison_2 =  trial_df[2, "face_pair"], 
                              comparison_3 = trial_df[3, "face_pair"],
                              select_1 = trial_df[1, "selected_face"],
                              select_2 = trial_df[2, "selected_face"],
                              select_3 = trial_df[3, "selected_face"],
                              triplet_idx = trial_df[1, "triplet_idx"]
                              
    )
    triplet_df2 = rbind(triplet_df2, selection_df)
  }
  remove(sub_df, trial_df, selection_df)
}
remove(columns, subject, triple)


triplet_df2$Intransitive_r2 = NA
triplet_df2$pattern_r2 = NA
for (row in 1:nrow(triplet_df2)){
  
  tmp = triplet_df2[row, c(2:7)]
  X_Y = which(grepl("X", tmp) & grepl("Y", tmp))
  Y_Z = which(grepl("Y", tmp) & grepl("Z", tmp))
  X_Z = which(grepl("X", tmp) & grepl("Z", tmp))
  
  X_Y_select = string_to_number(tmp[X_Y+3])
  Y_Z_select = string_to_number(tmp[Y_Z+3]) 
  X_Z_select = string_to_number(tmp[X_Z+3])
  pattern = as.numeric(paste0(X_Y_select,Y_Z_select,X_Z_select))
  
  triplet_df2[row, "pattern_r2"] = pattern
  
  if(pattern == 123 || pattern == 231){
    triplet_df2[row, "Intransitive_r2"]=1}else{triplet_df2[row, "Intransitive_r2"]=0}
  remove(tmp, X_Y, Y_Z, X_Z, X_Y_select, Y_Z_select, X_Z_select)
}
remove(row, pattern)

mean(triplet_df2$Intransitive_r2)

pattern_means = table(triplet_df2$pattern_r2, triplet_df$condition)
percentage <- prop.table(pattern_means, margin = 2) * 100


triplet_df2 = merge(triplet_df2, triplet_df, by = c("subject","triplet_idx"))
colnames(triplet_df2)[3:16] = c("round2_comp1","round2_comp2","round2_comp3","round2_sel1",
                                "round2_sel2","round2_sel3", "intrans_r2","pattern_r2", "round1_comp1",
                                "round1_comp2", "round1_comp3", "round1_sel1","round1_sel2","round1_sel3")

aggregate(intrans_r2 ~ condition + CB_transitive, triplet_df2, mean)
# condition intrans_r2
# 1        NM 0.04555085
# 2         M 0.05367232
aggregate(intrans_r2 ~ condition + detection, triplet_df2, mean)
#       condition intrans_r2
# 1        NM         43
# 2         M         76

aggregate(intrans_r1 ~ condition, triplet_df, mean)
# condition intrans_r1
# 1        NM 0.06673729
# 2         M 0.04237288

aggregate(intrans_r2 ~ condition, triplet_df2, mean)


triplet_df2$pattern_con = ifelse(triplet_df2$pattern_r1 == triplet_df2$pattern_r2, 1, 0)
mean(triplet_df2$pattern_con)
aggregate(pattern_con ~ intrans_r2, triplet_df2, sum)


intrans_patterns = triplet_df2[triplet_df2$intrans_r2 == 1, ]
sum(intrans_patterns$pattern_con)

triplet_df2$repeated_intrans = ifelse(triplet_df2$intrans_r1 == 1 & triplet_df2$intrans_r2 == 1, 1, 0)
mean(triplet_df2$repeated_intrans)
sum(triplet_df2$repeated_intrans)

triplet_df2$CB_induced = NA
for (row in 1:nrow(triplet_df2)){

    if ((triplet_df2[row, "pattern_r1"] == 121 & triplet_df2[row, "pattern_r2"] == 123)||
      (triplet_df2[row, "pattern_r1"] == 221 & triplet_df2[row, "pattern_r2"] == 231)){
      triplet_df2[row, "CB_induced"] = 1}
      
    else{triplet_df2[row, "CB_induced"] = 0}
}

aggregate(CB_induced ~ condition, triplet_df2, mean)
# condition  CB_induced
# 1        NM 0.012711864
# 2         M 0.009180791

prior = prior(normal(0, 1), class = b)
CB_ind = brm(CB_induced ~ condition, data = triplet_df2, family = "bernoulli", prior = prior)

fit = fitted(CB_ind, newdata = data.frame(condition = unique(triplet_df2$condition)))

bayesfactor_parameters(CB_ind)



aggregate(CB_induced ~ condition, triplet_df2, sum)

CB_induced_indv = aggregate(CB_induced ~ condition + subject, triplet_df2, sum)


triplet_df2_M = triplet_df2[triplet_df2$condition == "M", ]
aggregate(CB_induced ~ detection + condition, triplet_df2, mean)
aggregate(intrans_r2 ~ detection + condition, triplet_df2, mean)
x = triplet_df2[triplet_df2$CB_induced == 0, ]
aggregate(intrans_r2 ~ detection + condition, x, mean)

triplet_df2_intrans = triplet_df2_M[triplet_df2_M$intrans_r2 == 1, ]
sum(triplet_df2_intrans$CB_induced)
CB_subject = aggregate(CB_induced ~ subject, triplet_df2_intrans, sum)

idx = match(triplet_df2$subject, df_main$subject)
triplet_df2$mean_detec = df_main[idx, "mean_detec"]

prior = prior(normal(0, 1), class = b)
intrans_mod = brm(intrans_r2 ~ condition + (condition|subject) + 
                 (condition|triplet_idx), data = triplet_df2, family = "bernoulli",
                 control = list(adapt_delta = .95), file = "intrans_mod.rds",
                 prior = prior, warmup = 2000, iter = 6000, chains = 4, cores = 4)

bayesfactor_parameters(intrans_mod, verbose = FALSE)
#log-odds $\alpha$ = -3.33 [-3.84, -2.90])
# Group-Level Effects: ~subject (Number of levels: 118) 
#                             
#                             Estimate Est.Error l-95% CI u-95% CI 
# sd(Intercept)                 0.71      0.26     0.17     1.20     
# sd(conditionM)                0.60      0.34     0.04     1.29     
# cor(Intercept,conditionM)     0.08      0.50    -0.82     0.95   

# ~triplet_idx (Number of levels: 20) 
#                             Estimate Est.Error l-95% CI u-95% CI 
# sd(Intercept)                 0.34      0.21     0.02     0.83     
# sd(conditionM)                0.25      0.20     0.01     0.73    
# cor(Intercept,conditionM)    -0.26      0.57    -0.98     0.89 

# Population-Level Effects: 
#               Estimate Est.Error l-95% CI u-95% CI 
# Intercept     -3.34      0.25    -3.87    -2.90 
# conditionM     0.05      0.29    -0.55     0.62   


fixef = data.frame(condition = unique(triplet_df2$condition))
fit = fitted(intrans_mod, fixef, re_formula = NA, summary = FALSE)
sum = quantile((fit[, 1] + fit[, 2])/2, probs = c(.5, .025, .975))
pl = cbind(fit, fixef)



new_data = expand.grid(condition = unique(triplet_df2$condition), 
                       subject = unique(triplet_df2$subject))
fit = fitted(intrans_mod, new_data, re_formula = '~(condition|subject)')
pl = cbind(fit, new_data)
obs = aggregate(intrans_r2 ~ condition + subject, triplet_df2, mean)
obs$subject_n = sequence <- rep(1:118, each = 2)
idx = match(obs$subject, triplet_df2$subject)
obs$mean_detec = triplet_df2[idx, "mean_detec"]
pl$obs = obs$intrans_r2
pl$subject_n = as.character(sequence <- rep(1:118, each = 2))
colnames(pl) = c("fit", "er", "lwr", "upr", "condition", "subject", "Observed", "subject_n")

pl$above_.1 = NA
for (row in 1:nrow(pl)){
  if (pl[row, "condition"] == "M" & (pl[row, "Observed"]>.1)){
    pl[row, "above_.1"] = 1
    pl[row-1, "above_.1"] = 1}else{pl[row, "above_.1"] = 0}
}


outlier_df = pl[pl$above_.1 == 1, ]
idx = match(outlier_df$subject, df_main$subject)
outlier_df$mean_detec = df_main[idx, "mean_detec"]
mean(outlier_df$mean_detec)
CB_sum = aggregate(CB_induced ~ subject, triplet_df2_M, sum)
CB_sum = CB_sum[CB_sum$CB_induced > 0,]
idx = match(outlier_df$subject, CB_sum$subject)
outlier_df$CB_induced = CB_sum[idx, "CB_induced"]
total_intrns = aggregate(intrans_r2 ~ subject, triplet_df2_M, sum)
idx = match(outlier_df$subject,total_intrns$subject)
outlier_df$natural = total_intrns [idx, "intrans_r2"]



fix_data = data.frame(condition = unique(triplet_df2$condition))
fix_fit = fitted(intrans_mod, fix_data, re_formula = NA)
fixef = cbind(fix_fit, fix_data)
colnames(fixef) = c("fit", "er", "lwr", "upr", "condition")

Random_effects_intrans = ggplot(data = pl, aes(x = subject, y = fit, ymin = fit-er, ymax = fit+er))+
  geom_errorbar(color = "black", alpha = 0.8, linetype = 1, width = .5)+
  geom_point(data = pl, aes(x = subject, y = fit), color = "grey",size = 4)+
  geom_point(data = pl, aes(x = subject, y = Observed), inherit.aes = FALSE, size = 4)+
  geom_hline(data = fixef, aes(yintercept = fit, linetype = condition),
             size = .5, alpha = .8, color = "red")+
  labs(color = "Population effects", shape = "", linetype = "Population-level Effects")+
  facet_wrap(~condition, nrow = 2, ncol = 1, labeller = labeller(condition = c("NM" = "Non-manipulated triples", "M" = "Manipulated triples")))+
  labs(x = "", y = "", title = "Individual-level effects against sample means")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 24,hjust = 0.5),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    text=element_text(family="serif"),
    legend.position = c(0.01, .3),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.text = element_text(size = 15, family = "serif"),
    legend.title = element_text(size = 15, family = "serif"),
    strip.text = element_text(size = 16, face = "bold")
  )+
  scale_y_continuous(breaks = c(.03, seq(0, 1, by = 0.05)))+
  geom_text(data = outlier_df, aes(subject, obs, label = subject_n), vjust = -.8, inherit.aes = FALSE, size = 5)
ggsave(filename = "Random_effects_intrans.png", width = 55, height = 33, units = 'cm', dpi = 600)


table(triplet_df2$pattern_r2)


df_221_131 = df_main[df_main$pattern_r1 == 221 | df_main$pattern_r1 == 131, ]
tmp = triplet_df[, c(1:8)]
df_221_131 = merge(df_221_131,tmp, by = c("subject", "triplet_idx"))
df_221_131_last = df_221_131[df_221_131$face_pair == df_221_131$comparison_3, ]
df_221_131_last$pattern_r1 = factor(df_221_131_last$pattern_r1, levels = c(221,131))

mean(df_221_131_last$rt_choice)
aggregate(rt_choice ~ pattern_r1,df_221_131_last, mean )
df_221_131_last$confidence_scale = scale(df_221_131_last$confidence_rating)
df_221_131_last$diff_scale = scale(df_221_131_last$diff)

mod = lmer(rt_choice ~ pattern_r1 + diff_scale + confidence_scale + (pattern_r1|subject), data = df_221_131_last)




tmp = triplet_df[, c(1:8)]
df_main = merge(df_main, tmp, by = c("subject", "triplet_idx"))

df_main$comparison_order = NA
for (row in 1:nrow(df_main)){
  if(df_main[row, "comparison_1"] == df_main[row, "face_pair"]){
    df_main[row, "comparison_order"] = "first"
  }
  else if(df_main[row, "comparison_2"] == df_main[row, "face_pair"]){
    df_main[row, "comparison_order"] = "second"
  }else{df_main[row, "comparison_order"] = "third"}
}
df_main_x = df_main[df_main$rt_choice < 15000, ]
df_main_x$CB_transitive = as.factor(df_main_x$CB_transitive)
df_main_x$attr_consistent = as.factor(df_main_x$attr_consistent)
df_main_x$choice_scale = scale(df_main_x$rt_choice)
df_main_x$comparison_order = factor(df_main_x$comparison_order, levels = c("first","second","third"))
df_main_x$pattern_r1 = as.factor(df_main_x$pattern_r1)



ggplot(data = df_main_x, aes(x = comparison_order, y = rt_feedback, fill = CB_transitive))+
  geom_boxplot(width = 0.2, position = position_dodge(0.2))+
  ylim(209, 6000)

ggplot(data = df_main_x, aes(log(rt_choice)))+
  geom_histogram(bins = 30)


df_main_M = df_main[df_main$trial_cond == "M", ]
ggplot(data = df_main_M, aes(rt_choice, detection))+
  geom_smooth(method = 'lm')+
  xlim(209, 3500)

aggregate(rt_choice ~ CB_transitive, df_main_M, mean)


skewness(df_main_x$rt_choice, type = 2) #4.021006
prior = prior(normal(0, 1), class = b)
df_main_x$attr_consistent = as.factor(df_main_x$attr_consistent)
df_main_x$conf_scale = scale(df_main_x$confidence_rating)
df_main_x$diff_scale = scale(df_main_x$diff)
df_main_x$CB_transitive = factor(df_main_x$CB_transitive, levels = c(0,1))

mod = glm(rt_choice ~ comparison_order + conf_scale + diff_scale + 
            comparison_order*CB_transitive, data = df_main_x, 
          family = Gamma("log"))


pattern_221131 <- df_main_x[df_main_x$pattern_r1 %in% c(221, 223), ]
pattern_221131$pattern_r1 = factor(pattern_221131$pattern_r1, levels = c(223, 221, 131, 121))

dfa = aggregate(rt_choice ~ comparison_order + CB_transitive, pattern_221131, mean)

ggplot(dfa, aes(pattern_r1, rt_choice, fill = comparison_order))+
  geom_col(position = position_dodge(0.5))

skewness(pattern_221131$rt_choice)
hist(pattern_221131$rt_choice)
rt_mod = brm(rt_choice ~ comparison_order*CB_transitive + conf_scale + diff_scale +
              (comparison_order*CB_transitive|subject), family = Gamma(link = "log"),
             data = pattern_221131,
             prior = prior, file = "rt_mod.rds")

bayesfactor_parameters(rt_mod)

new_data = expand.grid(comparison_order = unique(df_main_x$comparison_order),
                       CB_transitive = unique(df_main$CB_transitive),
                        attr_consistent = unique(df_main_x$attr_consistent),
                                                 conf_scale = 0)
new_data = new_data[c(1:6, 10:12), ]
fit = fitted(rt_mod, new_data)
pl = cbind(fit, new_data)

mod = glm(rt_choice ~ comparison_order*CB_transitive + conf_scale + diff_scale, family = Gamma(link = "log"),  data = pattern_221131)
summary(mod)

#VAGUENESS AND INTRANSITIVITY

ggplot(triplet_df2, aes(confidence_r1, intrans_r2))+
  geom_smooth()

aggregate(detection ~ intrans_r1 + condition, triplet_df, mean)

##----------------------------------------------------------------------------####----------------------------------------------------------------------------##
##----------------------------------------------------------------------------####----------------------------------------------------------------------------##
##----------------------------------------------------------------------------####----------------------------------------------------------------------------##
##----------------------------------------------------------------------------####----------------------------------------------------------------------------##
##----------------------------------------------------------------------------####----------------------------------------------------------------------------##

#Choice consistency
#VERSION X
columns = c("subject","face_pair","unique_face", "selected_1st","selected_2nd",
            "triplet_idx","condition", "detection", "confidence_r1",
            "attr_cons","meta_cons","diff", "trip_cond")
consistency_df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(consistency_df) = columns

for (subject in unique(df$subject)){
  
  round_1 = df_main[df_main$subject == subject, ]
  round_2 = df_second[df_second$subject == subject, ]
  
  for (face_pair in unique(round_1$unique_pair)){
    face_pair_1 = round_1[round_1$unique_pair == face_pair, ]
    face_pair_2 = round_2[round_2$unique_pair == face_pair, ]
    
    tmp = data.frame(subject = subject, face_pair = face_pair_1$face_pair,
                     unique_pair = face_pair_1$unique_pair,
                     selected_1st = face_pair_1$selected_face,
                     selected_2nd = face_pair_2$selected_face,
                     triplet_idx = face_pair_1$triplet_idx,
                     condition = face_pair_1["trial_cond"],
                     detection = face_pair_1["detection"],
                     confidence_r1 = face_pair_1$confidence_rating,
                     attr_cons = face_pair_1$attr_consistent,
                     meta_cons = face_pair_1$meta_cons,
                     diff = face_pair_1$diff,
                     trip_cond = face_pair_1$trip_cond)
    
    consistency_df = rbind(consistency_df, tmp)
  }
}
consistency_df$consistent = ifelse(consistency_df$selected_1st == consistency_df$selected_2nd, 1, 0)
mean(consistency_df$consistent)


cc_mod = glmer(consistent ~ trial_cond + (trial_cond|subject) + (trial_cond|unique_pair),
          family = binomial(link = "logit"), data = consistency_df)

covar = VarCorr(cc_mod)
covar$subject

#VERSION Y
columns = c("subject","face_pair","selected_1st","selected_2nd",
            "triplet_idx","condition", "detection", "confidence_r1",
            "attr_cons","meta_cons","diff", "trip_cond")
consistency_df = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(consistency_df) = columns

for (subject in unique(df$subject)){
  
  round_1 = df_main[df_main$subject == subject, ]
  round_2 = df_second[df_second$subject == subject, ]
  
  for (face_pair in unique(round_1$unique_pair)){
    face_pair_1 = round_1[round_1$unique_pair == face_pair, ]
    face_pair_2 = round_2[round_2$unique_pair == face_pair, ]
    
    tmp = data.frame(subject = subject, face_pair = face_pair_1$face_pair,
                     selected_1st = face_pair_1$selected_face,
                     selected_2nd = face_pair_2$selected_face,
                     triplet_idx = face_pair_1$triplet_idx,
                     condition = face_pair_1["trial_cond"],
                     detection = face_pair_1["detection"],
                     confidence_r1 = face_pair_1$confidence_rating,
                     attr_cons = face_pair_1$attr_consistent,
                     meta_cons = face_pair_1$meta_cons,
                     diff = face_pair_1$diff,
                     trip_cond = face_pair_1$trip_cond)
                     
    consistency_df = rbind(consistency_df, tmp)
  }
}
remove(columns, face_pair,subject,face_pair_1, face_pair_2, round_1, round_2,tmp)

consistency_df$consistent = ifelse(consistency_df$selected_1st == consistency_df$selected_2nd, 1, 0)
mean(consistency_df$consistent) #0.78

aggregate(consistent~trial_cond, consistency_df, mean)

tmp = triplet_df2[, c(1:16, 21:22)]
consistency_df = merge(consistency_df, tmp, by = c("subject", "triplet_idx"))
remove(tmp)

consistency_df$pattern_consistent = ifelse(consistency_df$pattern_r1 == consistency_df$pattern_r2, 1, 0)
mean(consistency_df$pattern_consistent)
aggregate(pattern_consistent ~ pattern_r1 + trip_cond, consistency_df, mean)
aggregate(detection ~ pattern_consistent + trial_cond, consistency_df, mean)

consistency_df$same_order = NA
consistency_df$same_order_trial = NA
for (row in 1:nrow(consistency_df)){
  round2_comp1 = string_to_num_pair(consistency_df$round2_comp1[row])
  round2_comp2 = string_to_num_pair(consistency_df$round2_comp2[row])
  round2_comp3 = string_to_num_pair(consistency_df$round2_comp3[row])
  
  round1_comp1 = string_to_num_pair(consistency_df$round1_comp1[row])
  round1_comp2 = string_to_num_pair(consistency_df$round1_comp2[row])
  round1_comp3 = string_to_num_pair(consistency_df$round1_comp3[row])
  
  if (round2_comp1 == round1_comp1 & round2_comp2 == round1_comp2 & round2_comp3 == round1_comp3){
    consistency_df[row, "same_order"] = 1
  }else{consistency_df[row, "same_order"] = 0}
  
  round2_vec = c(round2_comp1,round2_comp2,round2_comp3)
  round1_vec = c(round1_comp1,round1_comp2,round1_comp3)
  comp = string_to_num_pair(consistency_df$face_pair[row])
  
  if (which(round2_vec == comp) ==  which(round1_vec == comp)){
    consistency_df[row, "same_order_trial"] = 1
  }else{consistency_df[row, "same_order_trial"] = 0}
}
remove(comp,idx,round1_comp1, round1_comp2, round1_comp3, round1_vec, round2_comp1,
       round2_comp2, round2_comp3, round2_vec, row)

aggregate(consistent ~ same_order + pattern_r1, consistency_df, mean)
aggregate(consistent ~ same_order_trial + comp, consistency_df, mean)

#scale_y_continuous(limits = c(0,90), breaks = c(0,25, 50,78, 90)) +
  #   geom_hline(yintercept = 78, color = "black", linetype = 2, size = 1)+
  #   labs(y = "Choice Consistency (%)", fill = "Condition", x = "")+
  #   scale_fill_manual(values = c("#F7F7F7","#CCCCCC","#969696", "#464646"))+
  #   theme_classic()+
  #   geom_text(aes(label = paste0(c(82.1,82.2,87, 76), "%")), 
  #             position = position_stack(vjust = 0.5), size = 6)+
  #   theme(text=element_text(family="serif"),
  #         axis.title.y = element_text(size = 22),
  #         axis.text.y = element_text(size = 12, face = "bold"),
  #         legend.text = element_text(size = 12), 
  #         legend.title = element_text(size = 14),
#         axis.text.x = element_blank(),
#         axis.ticks = element_blank())


sub = aggregate(intrans_r1 ~ subject + condition,  triplet_df2, sum)
sub2 = aggregate(intrans_r2 ~ subject + condition, triplet_df2, sum)
sub = merge(sub, sub2, by = c("subject", "condition"))
sub$sum = sub$intrans_r1 + sub$intrans_r2

consistency_df$inconsistent = ifelse(consistency_df$selected_1st == consistency_df$selected_2nd, 0, 1)
mean(consistency_df$inconsistent) + mean(consistency_df$consistent)
reversals = aggregate(inconsistent ~ subject, consistency_df, sum)
sub = merge(sub, reversals, by = "subject")

#reversals = aggregate(consistent ~)

sum(sub$inconsistent <= 12)/2
sum(sub$inconsistent > 12)/2
split = sub[sub$inconsistent <= 12, ]

mean(split$sum) #  0.9433962
aggregate(sum ~ condition, sub, mean)

split2 =  sub[sub$inconsistent > 12, ]
mean(split2$sum) #3.269231

sum(split2$sum)/nrow(split2)

sum(split2$sum)/sum(split$sum)
sum(split2$sum)
sum(split$sum)

nrow(split)
nrow(split2)

vari = ggplot(sub, aes(inconsistent, sum, size = sum, color = sum, fill = condition))+
  geom_point(fill = "darkgrey", shape = 21, color = "black")+
  geom_smooth(method = 'loess', se = FALSE, size = 2, color = "black")+
  theme_classic()+
  ylim(-0.2,12)+
  geom_vline(xintercept = 12, size = 1.5, linetype = 2)+
  labs(y = "Number of intransitive patterns", x = "")+
  theme(text = element_text(family = "serif"),
        legend.position = "none",
        axis.text.x = element_text(size =20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 30, color = "black"),
        axis.text.y = element_text(size = 15, face = "bold"))+
  scale_size(range = c(2, 10), trans = "sqrt")
ggsave(filename = "vari.png", width = 25, height = 18, units = 'cm', dpi = 1200)


df_main_M = df_main[df_main$trial_cond == "M", ]
sub = aggregate(detection ~ subject, df_main_M, mean)
idx = match(consistency_df$subject, sub$subject)
consistency_df$mean_detec = sub[idx, "detection"]

quantile(sub$inconsistent)

ggplot(consistency_df, aes(diff, consistent))+
  geom_smooth(method = 'lm')+
  ylim(0,1)


consistency_df$diff_scale = scale(consistency_df$diff)
consistency_df$confidence_scale = scale(consistency_df$confidence_r1)
mod = glm(consistent ~ diff_scale + confidence_scale, family = "binomial", data = consistency_df)

#Check CC according to M, NM, detected and undetected. 
consistency_last_comp = consistency_df[c(consistency_df$face_pair == consistency_df$round1_comp3),]

aggregate(consistent ~ CB_transitive + trial_cond, consistency_last_comp, mean)
aggregate(consistent ~ trial_cond, consistency_last_comp, mean)
aggregate(consistent ~ trial_cond+ detection, consistency_last_comp, mean)
consistency_last_comp$attr_cons = factor(consistency_last_comp$attr_cons, levels = c(0, 1))
consistency_last_comp$detection = factor(consistency_last_comp$detection,
                                                   levels = c(0,1))

aggregate(consistent ~ trip_cond, consistency_last_comp, mean)
aggregate(consistent ~ trip_cond + CB_transitive, consistency_last_comp, mean)

aggregate(consistent ~ trip_cond + detection, consistency_last_comp, mean)

aggregate(consistent ~ trip_cond + CB_transitive, consistency_last_comp, mean)
tmp = data.frame(fit = c(82.1,82.2, 87.6, 76.3),
                 condition = c("Manipulated", "Non-manipulated", "Detected manipulation", "Undetected manipulation"),
                 std = c(0.01018377, 0.01245540,0.013055347, 0.007109094))

gray_colors <- brewer.pal(4, "Greys")
cc = ggplot(data = tmp, aes(condition, fit, fill = condition))+
  geom_col(color = "black", width = .75)+
  scale_y_continuous(limits = c(0,90), breaks = c(0,25, 50,78, 90)) +
  geom_hline(yintercept = 78, color = "black", linetype = 2, size = 1)+
  labs(y = "Choice Consistency (%)", fill = "Condition", x = "")+
  scale_fill_manual(values = c("#F7F7F7","#CCCCCC","#969696", "#464646"))+
  theme_classic()+
  geom_text(aes(label = paste0(c(82.1,82.2,87, 76), "%")), 
            position = position_stack(vjust = 0.5), size = 6)+
  theme(text=element_text(family="serif"),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = "cc.png", width = 18, height = 12, units = 'cm', dpi = 1000)

aggregate(diff ~ trial_cond + detection, consistency_df, mean)
aggregate(diff ~ trial_cond + detection, consistency_df, sd)

##----------------------------------------------------------------------------##
#NON-target stuff
df_nontarget = consistency_df[!(consistency_df$face_pair == consistency_df$round1_comp3), ]
tmp = consistency_df[consistency_df$face_pair == consistency_df$round1_comp3, c("subject", "triplet_idx", "consistent")]
df_nontarget = merge(df_nontarget, tmp, by = c("subject", "triplet_idx"))
colnames(df_nontarget)[c(14, 31)] = c("consistent", "T_consistent")
df_nontarget$T_consistent = factor(df_nontarget$T_consistent, levels = c(1,0))
df_nontarget$CB_transitive = as.factor(df_nontarget$CB_transitive)
df_nontarget$diff_scale = scale(df_nontarget$diff)
df_nontarget$confidence_scale = scale(df_nontarget$confidence_r1)

df_nontarget$con_both = NA
for (row in 1:nrow(df_nontarget)){
  subject = df_nontarget[row, "subject"]
  trip = df_nontarget[row, "triplet_idx"]
  
  tmp = df_nontarget[df_nontarget$subject == subject & df_nontarget$triplet_idx == trip,]
  vec = tmp$consistent
  both = ifelse(sum(vec) == 2, 1, 0)
  idx = which(df_nontarget$subject == subject & df_nontarget$triplet_idx == trip)
  df_nontarget[idx, "con_both"] = both
}

aggregate(con_both ~ CB_transitive + T_consistent, df_nontarget, mean)

# CB_transitive T_consistent  con_both
# 1             0            0 0.5445545
# 2             1            0 0.2881356
# 3             0            1 0.5696822
# 4             1            1 0.6297948

CB_Tcon = aggregate(consistent ~ CB_transitive + T_consistent, df_nontarget, mean)




aggregate(consistent ~ CB_transitive, df_nontarget, mean)

aggregate(diff ~ T_consistent + CB_transitive, df_nontarget, median)
aggregate(diff ~ T_consistent + CB_transitive, df_nontarget,sd)
aggregate(confidence_r1 ~ T_consistent + CB_transitive, df_nontarget, median)
aggregate(confidence_r1 ~ T_consistent + CB_transitive, df_nontarget, sd)
aggregate(diff ~ CB_transitive, df_nontarget, mean)
aggregate(attr_cons ~ CB_transitive, df_nontarget, mean)
aggregate(attr_cons ~ T_consistent + CB_transitive, df_nontarget, mean)
aggregate(consistent ~ attr_cons, df_nontarget, mean)

# T_consistent CB_transitive attr_cons
# 1            1             0 0.3725361
# 2            0             0 0.4014085
# 3            1             1 0.8264151
# 4            0             1 0.7688889


mean(df_nontarget$diff)
df_nontarget$C3_consistent = ifelse(df_nontarget$T_consistent == 1, "Consistent", "Inconsistent")
diff_plot = ggplot(df_nontarget, aes(C3_consistent, diff, fill = CB_transitive))+
  geom_boxplot(width= 0.2, size = 0.8)+
  theme_classic()+
  ylim(.1, 70)+
  scale_y_continuous(
    breaks = c(0, 11.81, 25, 50, 70),
    labels = c("0", "11.81", "25", "50", "70"))+
  scale_fill_manual(values = c("grey50", "grey80"), labels=c('Non-CB-Transitive', 'CB-Transitive'))+
  labs(fill = "CB-Transitivity",y = expression(paste("Difference in  ", italic(f), "-ratings")), x = "")+
  geom_hline(yintercept = 11.81716, size = 1.2, linetype = 2)+
  theme(text=element_text(family="serif"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 14),
        legend.text = element_blank(), 
        legend.title = element_blank(),
        legend.justification = c("left", "bottom"),
        legend.box.just = "right",
        legend.position="none")
ggsave(filename = "diff_plot.png", width = 11, height = 11, units = 'cm', dpi = 1200)

mean(df_nontarget$confidence_r1, na.rm = TRUE) 
           
conf_plot = ggplot(df_nontarget, aes(C3_consistent, confidence_r1, fill = CB_transitive))+
  geom_boxplot(width= 0.2, size = .8, linetype = 1)+
  theme_classic()+
  scale_y_continuous(
    breaks = c(0, 25, 47.31723, 75, 100),
    labels = c("0","25","47", "75","100"))+
  scale_fill_manual(values = c("grey50", "grey80"), labels=c('Non-CB-Transitive', 'CB-Transitive'))+
  labs(fill = "CB-Transitivity",y = "Relative preference rating", x = "C[3] reversal/consistency")+
  geom_hline(yintercept = 47.31723, size = 1.2, linetype = 2)+
  theme(text=element_text(family="serif"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 15), 
        legend.title = element_blank(),
        legend.position = "none",
        legend.justification = c("left", "bottom"),
        legend.box.just = "right")
ggsave(filename = "conf_plot.png", width = 11, height = 11, units = 'cm', dpi = 1200)


comb = diff_plot/conf_plot
ggsave(filename = "comb.png", width = 11, height = 16, units = 'cm', dpi = 1200)

# gray_colors <- brewer.pal(4, "Greys")
# cc = ggplot(data = tmp, aes(condition, fit, fill = condition))+
#   geom_col(color = "black", width = .75)+
#   scale_y_continuous(limits = c(0,90), breaks = c(0,25, 50,78, 90)) +
#   geom_hline(yintercept = 78, color = "black", linetype = 2, size = 1)+
#   labs(y = "Choice Consistency (%)", fill = "Condition", x = "")+
#   scale_fill_manual(values = c("#F7F7F7","#CCCCCC","#969696", "#464646"))+
#   theme_classic()+
#   geom_text(aes(label = paste0(c(82.1,82.2,87, 76), "%")), 
#             position = position_stack(vjust = 0.5), size = 6)+
#   theme(text=element_text(family="serif"),
#         axis.title.y = element_text(size = 22),
#         axis.text.y = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 14), 
#         legend.title = element_text(size = 16),
#         axis.text.x = element_blank(),
#         axis.ticks = element_blank())

ggplot(df_nontarget, aes(CB_transitive, confidence_r1, fill = T_consistent))+
  geom_boxplot()

mod = lm(diff ~ T_consistent*CB_transitive, data = df_nontarget)



df_nontarget$diff_scale = scale(df_nontarget$diff)
df_nontarget$confidence_scale = scale(df_nontarget$confidence_r1)
tmp = df_main[, c("subject", "triplet_idx", "unique_pair")]
df_nontarget = merge(df_nontarget, tmp, by = c("subject", "triplet_idx"))


df_nontarget$attr_cons = as.factor(df_nontarget$attr_cons)


prior = prior(normal(0, 1), class = b)
CC_interaction = brm(consistent ~ CB_transitive*T_consistent +
                    (CB_transitive*T_consistent|subject),
                    control = list(adapt_delta = .95),
                    prior = prior, warmup = 2000, iter = 4000, chains = 4, cores = 4,
                    family = "bernoulli", data = df_nontarget, file = "CC_interaction.rds")
bayesfactor_parameters(CC_interaction)



new_data = expand.grid(CB_transitive = unique(df_nontarget$CB_transitive),
                       T_consistent = unique(df_nontarget$T_consistent))

fit = fitted(CC_interaction, new_data, re_formula = NA)
pl = cbind(fit, new_data)

colnames(pl) = c("fit", "er", "lwr", "upr", "CB_Transitive", "C3_Consistent")

pl$CB_Transitive = as.factor(pl$CB_Transitive)
pl$C3_Consistent = as.factor(pl$C3_Consistent)


#scale_fill_manual(values = c("#F7F7F7","#CCCCCC","#969696", "#464646"))+
pl$cb = ifelse(pl$CB_Transitive == 1, "CB-Transitive", "Non-CB-Transitive")
pl$c3 = ifelse(pl$C3_Consistent == 1, "Consistent", "Inconsistent")

mean(df_nontarget$consistent)

CC_interact = ggplot(pl, aes(c3, fit, ymin = lwr, ymax = upr, color = cb, group = cb))+
  geom_pointrange(position = position_dodge(0.1), size = 1)+
  geom_errorbar(inherit.aes = TRUE, width = 0.1, position = position_dodge(.09), size = 1)+
  geom_line(position = position_dodge(0.1), size = 1, linetype = 6)+
  geom_hline(yintercept = 0.7637712, color = "black", linetype = 2)+
  scale_color_manual(values = c("#464646", "#969696"))+
  theme_classic()+
  labs(y = "Choice consistency (probability scale)", x = "C[3] reversal/consistency")+
  theme(text=element_text(family="serif"),
        legend.position = c(0.25, 0.4),
        axis.text.x = element_text(size =12, face = "bold", family = "serif"),
        axis.title.x = element_text(size =18, family = "serif"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 15, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16))
ggsave(filename = "CC_interact.png", width = 15, height = 12, units = 'cm', dpi = 1000)

fit_nosum = fitted(CC_interaction, new_data, summary = FALSE, re_formula = NA)

#CB_trans
quantile(fit_nosum[, 1]-fit_nosum[, 2], probs = c(.5, .025, .975))

d = fit_nosum[, 1]-fit_nosum[, 2]
mean(d > 0)

hist(d)

quantile(fit_nosum[, 4]-fit_nosum[, 2], probs = c(.5, .025, .975))
d = fit_nosum[, 4]-fit_nosum[, 2]
mean(d<0)

#Interaction
quantile(fit_nosum[, 1]-fit_nosum[, 3], probs = c(.5, .025, .975))


colnames(fit) = c("CB1", "NCB1", "CB2","NCB2")
fit = as.data.frame(fit)
avg_M = (fit$CB1+fit$CB2)/2
avg_NM = (fit$NCB1 + fit$NCB2)/2






M_avg = mean(fit[, 1] + fit[, 3])

pl = cbind(fit, new_data)
colnames(pl) = c("fit", "er", "lwr", "upr", "CB_transitive", "CthreeConsistent", "Triplet_condition")

pl$new = ifelse(pl$Triplet_condition == "M", "Manipulated triple", "Non-manipulated triple")
pl$new2 = ifelse(pl$CB_transitive == 0, "CB-Transitive", "Non-CB-Transitive")
pl[,1:4 ] = pl[,1:4 ]*100
mean(df_nontarget$consistent)
pl$new = as.factor(pl$new)
pl$new2 = as.factor(pl$new2)
pl$CthreeConsistent = as.factor(pl$CthreeConsistent)
pl$CB_transitive = as.factor(pl$CB_transitive)
pl$Triplet_condition = as.factor(pl$Triplet_condition)

#  geom_line(position = position_dodge2(0.3), linetype = 1, color = "black", size = 1)+


##F7F7F7","#CCCCCC","#969696", "#464646"

CC_int = ggplot(pl, aes(CthreeConsistent, fit, fill = CB_transitive, 
               linetype = new, group =  CB_transitive))+
  geom_col(position = position_dodge2(0.2), color = "black")+
  geom_errorbar(position = position_dodge2(0.3), aes(ymin = lwr, ymax = upr))+
  scale_y_continuous(limits = c(0,90), breaks = c(0, 25,50, 76.3, 90)) +
  theme_classic()+
  scale_fill_manual(values = c("#F7F7F7", "#CCCCCC"), name = "CB_transitive", labels = c("CB-transitive", "Non-CB-Transitive"))+
  labs(y = "C[1] & C[2] Choice Consistency (%)", x = "")+
  theme(
    text=element_text(family="serif"),
    axis.text.x = element_text(size =25, face = "bold", family = "serif"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 15, face = "bold"),
    legend.position = c(0.1, .9),
    legend.justification = c("left", "bottom"),
    legend.title = element_blank(),
    legend.text = element_text(size = 27),  # Customize the legend symbol appearance
    legend.key.size = unit(2, "lines"),  # Adjust the size of the legend symbols
    legend.key.width = unit(2, "lines"))+
  scale_x_discrete(labels = c("C[3]-inconsistent", "C[3]-Consistent"))+
  scale_linetype_manual(values = c("solid", "dashed"), name = "New") +
  guides(linetype = guide_legend(override.aes = list(size = 2)))+
  guides(linetype = "none")
ggsave(filename = "CC_int.png", width = 28, height = 23, units = 'cm', dpi = 1500) 



df_nontarget_C3 = df_nontarget[df_nontarget$T_consistent == 0, ]
mean(df_nontarget_C3$consistent)
aggregate(consistent ~ attr_cons + CB_transitive, df_nontarget_C3, mean)

ggplot(df_nontarget, aes(diff, consistent, color = T_consistent))+
  geom_smooth(method = 'lm')




#Deeper analysis

df_nontarget = consistency_df[!(consistency_df$comp == "XY"), ]
tmp = consistency_df[consistency_df$comp == "XY", c("subject", "triplet_idx", "consistent") ]
df_nontarget = merge(df_nontarget, tmp, by = c("subject", "triplet_idx"))
colnames(df_nontarget)[c(14, 31)] = c("consistent", "XY_consistent")
df_nontarget$XY_consistent = as.factor(df_nontarget$XY_consistent)

df_nontarget$pattern131_223 = ifelse(df_nontarget$pattern_r1 == 131 | df_nontarget$pattern_r1 == 223, 1 , 0)
df_nontarget$pattern131_223 = as.factor(df_nontarget$pattern131_223)


p_131_223 = aggregate(consistent ~ XY_consistent + pattern131_223, df_nontarget, mean)

# T_consistent pattern131_223 consistent
# 1            0              0  0.7889610
# 2            1              0  0.8130469
# 3            0              1  0.5679012
# 4            1              1  0.7696429


plot1 = ggplot(p_131_223, aes(pattern131_223, consistent, fill = XY_consistent))+
  geom_col(position = position_dodge2(0.2))+
  labs(title = "patterns 131 and 223 with target pair XY", y = "non-target consistent (YZ & XZ)")



df_nontarget = consistency_df[!(consistency_df$comp == "YZ"), ]
tmp = consistency_df[consistency_df$comp == "YZ", c("subject", "triplet_idx", "consistent") ]
df_nontarget = merge(df_nontarget, tmp, by = c("subject", "triplet_idx"))
colnames(df_nontarget)[c(14, 31)] = c("consistent", "YZ_consistent")
df_nontarget$YZ_consistent = as.factor(df_nontarget$YZ_consistent )

df_nontarget$pattern133_221 = ifelse(df_nontarget$pattern_r1 == 133 | df_nontarget$pattern_r1 == 221, 1 , 0)
df_nontarget$pattern133_221= as.factor(df_nontarget$pattern133_221)

p_133_221 = aggregate(consistent ~ YZ_consistent + pattern133_221, df_nontarget, mean)

# T_consistent pattern133_221 consistent
# 1            0              0  0.7995781
# 2            1              0  0.8237232
# 3            0              1  0.5316456
# 4            1              1  0.7546374

plot2 = ggplot(p_133_221, aes(pattern133_221, consistent, fill = YZ_consistent))+
  geom_col(position = position_dodge2(0.2))+
  labs(title = "patterns 133 and 221 with target pair YZ", y = "non-target consistent (XY & XZ)")



df_nontarget = consistency_df[!(consistency_df$comp == "XZ"), ]
tmp = consistency_df[consistency_df$comp == "XZ", c("subject", "triplet_idx", "consistent") ]
df_nontarget = merge(df_nontarget, tmp, by = c("subject", "triplet_idx"))
colnames(df_nontarget)[c(14, 31)] = c("consistent", "XZ_consistent")
df_nontarget$XZ_consistent = as.factor(df_nontarget$XZ_consistent)

df_nontarget$pattern121_233 = ifelse(df_nontarget$pattern_r1 == 121 | df_nontarget$pattern_r1 == 233, 1 , 0)
df_nontarget$pattern121_233 = as.factor(df_nontarget$pattern121_233)


p_121_233 = aggregate(consistent ~ XZ_consistent + pattern121_233, df_nontarget, mean)
# T_consistent pattern121_233 consistent
# 1            0              0  0.7485465
# 2            1              0  0.7586996
# 3            0              1  0.5416667
# 4            1              1  0.8140097

plot3 = ggplot(p_121_233, aes(pattern121_233, consistent, fill = XZ_consistent))+
  geom_col(position = position_dodge2(0.2))+
  labs(title = "patterns 121 and 233 with target pair XZ",  y = "non-target consistent (XY & YZ)")

comb = plot1+plot2+plot3
ggsave(filename = "comb .png", width = 35, height = 23, units = 'cm', dpi = 1500) 


