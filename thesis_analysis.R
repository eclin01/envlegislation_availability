library(readxl)
library(ggplot2)
library(ggthemes)
library(aod)
library(stargazer)
library(haven)
library(car)
library(dplyr)
library(lubridate)

df_raw = readRDS("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/FINAL_ALLBILLS_DF.RDS")
envdf = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/EnvData/envdata_for_analysis_0504.csv")

envdf = envdf %>% mutate(Disaster.Group = factor(Disaster.Group), 
                         Disaster.Subgroup = factor(Disaster.Subgroup),
                         Disaster.Type = factor(Disaster.Type),
                         Disaster.Subtype = factor(Disaster.Subtype),
                         totdam = `Total.Damage..Adjusted...000.US..`,
                         totaffected = ifelse(is.na(Total.Affected), 0, Total.Affected))



trends = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/df_search_to_merge.csv")
df_raw2 = merge(df_raw, trends, on = c("bill_num", "year", "DisNo_BE"))

df = df_raw2 %>% mutate(author_affil_simp = case_when(author_affil == "D" ~ "D",
                                                 author_affil == "R" ~ "R",
                                                 TRUE ~ "O"))

df$BE_ProxDays_sq = df$BE_ProxDays ^ 2

df = df %>% mutate(
  year = factor(year), 
  state = factor(state), 
  author_affil = factor(author_affil),
  final_env_tag = factor(final_env_tag),
  author_affil_simp = factor(author_affil_simp, levels = c("R", "D", "O")),
  ProxFlag_AS = factor(ProxFlag_AS),
  ProxFlag_BE = factor(ProxFlag_BE, levels = c(1, 2, 0) ),
  ownst_BE = ifelse(ProxFlag_BE == 1, 1, 0),
  ownst_AS = ifelse(ProxFlag_BE == 1, 1, 0),
  neighbor_BE = ifelse(ProxFlag_BE == 2, 1, 0),
  neighbor_AS = ifelse(ProxFlag_BE == 2, 1, 0),
  status = factor(status),
  status_tag = factor(status_flag, labels = c("fail", "success")),
  addtl_authors_flag = factor(addtl_authors_flag),
  vote_data_present = factor(vote_data_present))

df$re_taged_prim_topic = case_when((df$primary_topic == "Climate Change") ~ "",
                                    (df$primary_topic == "Emissions") ~ "",
                                   (df$primary_topic == "Infrastructure and Construction") ~ "",
                                   (df$primary_topic == "Outdoor Recreation") ~ "",
                                   (df$primary_topic == "Public Programs") ~ "",
                                   (df$primary_topic == "Utilities") ~ "",
                                   TRUE ~ df$primary_topic)

df$re_taged_prim_topic = factor(df$re_taged_prim_topic)

df$proenv_outcome =  case_when(((df$status_tag == "success") & (df$final_env_tag == "pro")) ~ "pro",
                               ((df$status_tag == "fail") & (df$final_env_tag == "pro")) ~ "anti",
                               ((df$status_tag == "fail") & (df$final_env_tag == "anti")) ~ "pro",
                               ((df$status_tag == "success") & (df$final_env_tag == "anti")) ~ "anti")

df$proenv_outcome = factor(df$proenv_outcome) 
df$proenv_binary = ifelse(df$proenv_outcome == "pro", 1, 0)
df$status_binary = ifelse(df$status_flag == "1", 1, 0)
df$dem_author_bin = ifelse(df$author_affil_simp == "D", 1, 0)



limit = 365
seg = 14

grouped_df = df %>% mutate(group_tag_BE = cut(BE_ProxDays,seq(-limit,limit,seg)), 
                           group_tag_AS = cut(AS_ProxDays,seq(-limit,limit,seg)))

grouped_df$group_tag_AS <- factor(grouped_df$group_tag_AS, ordered = TRUE)
grouped_df$group_tag_BE <- factor(grouped_df$group_tag_BE, ordered = TRUE)

grouped_df_AS = grouped_df %>% group_by(group_tag_AS) %>% mutate(mean_outcome = mean(proenv_binary), mean_status = mean(status_flag))

grouped_df_BE = grouped_df %>% group_by(group_tag_BE) %>% mutate(mean_outcome = mean(proenv_binary), mean_status = mean(status_flag))


grouped_df_BE %>%
  ggplot( aes(x=BE_ProxDays, y=mean_status)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="darkslategrey", fill="#69b3a2", size=2) +
  ylim(0, 1) +
  xlim(-limit, limit-10) +
  xlab("Proximity to Last Action Date") + 
  ylab("Average Passage Rate") +
  theme_minimal() + 
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30))



fdf = merge(df, envdf, by.x = "DisNo_BE", by.y = "DisNo.")
#write.csv(fdf, "/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/FINAL_ALLBILLS_DF_MERGED.csv")


fdf_for_reg = fdf %>% filter((BE_ProxDays <= 0)) %>% mutate(abs_proxBE = abs(BE_ProxDays)) 
fdf_check = fdf %>% filter(BE_ProxDays > 0)

placebo_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
                  BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
                  + addtl_authors_flag + 
                  + author_affil_simp + state + year +
                  Disaster.Type + Total.Affected + re_taged_prim_topic, 
                family = binomial(link = "logit"),
                data = fdf_check)

summary(placebo_reg)

stargazer(placebo_reg, omit = c("state", "year", "Disaster.Type", "primary_topic"))



###regressions

fdf_for_reg$BE_ProxDays_reg = fdf_for_reg$BE_ProxDays / 100
fdf_for_reg$BE_ProxDays_sq_reg = fdf_for_reg$BE_ProxDays_sq / 100

full_spec = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
                  BE_ProxDays:ownst_BE +  BE_ProxDays_sq:ownst_BE
                  + addtl_authors_flag + 
                  + author_affil_simp + state + year + 
                    Disaster.Type + Total.Affected + re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + 
                  search_val_filled, 
                family = binomial(link = "logit"),
                data = fdf_for_reg)

summary(full_spec)

stargazer(full_spec, omit = c("state", "year", "Disaster.Type", "primary_topic"))

reg1 = glm(proenv_outcome ~  BE_ProxDays_reg, 
                family = binomial(link = "logit"),
                data = fdf_for_reg)

summary(reg1)



reg2 = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg, 
           family = binomial(link = "logit"),
           data = fdf_for_reg)
summary(reg2)

reg3 = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE, 
                family = binomial(link = "logit"),
                data = fdf_for_reg)
summary(reg3)


reg4 = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
                + author_affil_simp + addtl_authors_flag, 
                family = binomial(link = "logit"),
                data = fdf_for_reg)


reg5 = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
                + addtl_authors_flag + 
                  + author_affil_simp +
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg, 
                family = binomial(link = "logit"),
                data = fdf_for_reg)

summary(reg5)

reg6 = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
           + addtl_authors_flag + 
             + author_affil_simp +
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg +
             Disaster.Type + Total.Affected,
           family = binomial(link = "logit"),
           data = fdf_for_reg)


reg7 = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
           + addtl_authors_flag + 
             + author_affil_simp +
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg +
             Disaster.Type + Total.Affected + state + year,
           family = binomial(link = "logit"),
           data = fdf_for_reg)

full_spec = glm(proenv_outcome ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
                  BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
                + addtl_authors_flag + 
                  + author_affil_simp + state + year + 
                  Disaster.Type + Total.Affected + 
                  re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + search_val_filled, 
                family = binomial(link = "logit"),
                data = fdf_for_reg)



stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, full_spec, omit = c("state", "year", "re_taged_prim_topic", "Disaster.Type"))

stargazer(reg5, reg6, reg7, full_spec, omit = c("author", "state", "year", "ownst"))


##test balance
votes_only = df[,(!grepl("^hist",names(df))) & (!grepl("^dummy",names(df)))] %>%  filter(vote_data_present == 1)

t_outcome = t.test(votes_only$proenv_binary, df$proenv_binary)

t_status = t.test(votes_only$status_binary, df$status_binary)

t_dem= t.test(votes_only$dem_author_bin, df$dem_author_bin)
t_dem

t_BEProx = t.test(votes_only$BE_ProxDays, df$BE_ProxDays)
t_BEProx

sd(votes_only$BE_ProxDays, na.rm = TRUE)
sd(df$BE_ProxDays, na.rm = TRUE)

test_outcomes_diff1 = lm(proenv_binary ~ vote_data_present, data = df)
test_outcomes_diff2 = lm(proenv_binary ~ vote_data_present + state + year, data = df)

lh1 = linearHypothesis(test_outcomes_diff1, c("vote_data_present1=0"), white.adjust = "hc1")
lh2 = linearHypothesis(test_outcomes_diff2, c("vote_data_present1=0"), white.adjust = "hc1")

lh1
lh2




#pro env anti env only
pro_env_only = fdf_for_reg %>% filter(proenv_outcome == "pro")
anti_env_only = fdf_for_reg %>% filter(proenv_outcome == "anti")

pro_env_full = glm(status_flag ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
                  BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
                + addtl_authors_flag + 
                  + author_affil_simp + state + year + 
                  Disaster.Type + Total.Affected + 
                  re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + search_val_filled, 
                family = binomial(link = "logit"),
                data = pro_env_only)

summary(pro_env_full)




anti_env_full = glm(status_flag ~  BE_ProxDays_reg + BE_ProxDays_sq_reg + ownst_BE + 
                     BE_ProxDays_reg:ownst_BE +  BE_ProxDays_sq_reg:ownst_BE
                   + addtl_authors_flag + 
                     + author_affil_simp + state + year + 
                     Disaster.Type + Total.Affected + 
                     re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + search_val_filled, 
                   family = binomial(link = "logit"),
                   data = anti_env_only)

summary(anti_env_full)

stargazer(pro_env_full, anti_env_full, omit = c("state", "year", "re_taged_prim_topic", "Disaster.Type"))



#legislator related data
vdf = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/final_all_voters_df.csv")

vcdf = vdf[,(!grepl("^hist",names(vdf))) & (!grepl("^dummy",names(vdf)))]
vcdf = merge(vcdf, df, by = c("bill_num", "year", "state"), all.x = TRUE)
vcdf = vcdf[,(!grepl("^hist",names(vcdf))) & (!grepl("^dummy",names(vcdf)))]

vcdf = vcdf %>% mutate(
  year = factor(year), 
  state = factor(state), 
  author_affil = factor(author_affil.x),
  author_affil_simp = case_when(author_affil == "D" ~ "D",
                                author_affil == "R" ~ "R",
                                TRUE ~ "O"),
  author_affil_simp = factor(author_affil_simp, levels = c("R", "D", "O")),
  ProxFlag_BE = factor(ProxFlag_BE, levels = c(1, 2, 0) ),
  ownst_BE = ifelse(ProxFlag_BE == 1, 1, 0),
  ownst_AS = ifelse(ProxFlag_BE == 1, 1, 0),
  neighbor_BE = ifelse(ProxFlag_BE == 2, 1, 0),
  neighbor_AS = ifelse(ProxFlag_BE == 2, 1, 0),
  status = factor(status.x),
  status_tag = factor(status_flag, labels = c("fail", "success")),
  addtl_authors_flag = factor(addtl_authors_flag.x),
  vote_data_present = factor(vote_data_present),
  primary_topic = factor(primary_topic.x),
  BE_ProxDays_reg = BE_ProxDays / 100,
  BE_ProxDays_sq = BE_ProxDays ^ 2, 
  BE_ProxDays_reg_sq = BE_ProxDays_sq / 100,
  final_env_tag = factor(final_env_tag))

vdfa = vcdf %>% select(-contains(".x"))

vdfa$ca_votes = case_when(vdfa$option == "yes" ~ "Yea",
                          vdfa$option == "no" ~ "Nay",
                          vdfa$option == "not voting" ~ "NV",
                          vdfa$option == "other" ~ "Absent",
                          TRUE ~ NA)

vdfa$vote_text_x = ifelse(vdfa$vote_text_x == "", NA, vdfa$vote_text_x)

vdfa$leg_vote = ifelse(is.na(vdfa$vote_text_x), vdfa$ca_votes, vdfa$vote_text_x)

vdfa$leg_vote_f = factor(vdfa$leg_vote)

env_to_merge = envdf %>% select(DisNo., Disaster.Type, Total.Affected, start_date)


vdff = vdfa %>% select(bill_num, year, state, yea_x, nay_x, nv_x, absent_x, total_x, 
                       passed_x, party, name, last_name, DisNo_BE, author.y, author_affil.y, 
                       addtl_authors_flag.y, BE_ProxDays, status_flag, final_env_tag, 
                       author_affil_simp, BE_ProxDays_sq, BE_ProxDays_reg, BE_ProxDays_reg_sq, ownst_BE, leg_vote,
                       re_taged_prim_topic, search_val_filled)

vdff_m = merge(vdff, env_to_merge, by.x = "DisNo_BE", by.y = "DisNo.", all.x = TRUE)

vdff_m = vdff_m %>% mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"))
vdff_s = arrange(vdff_m, start_date)
vdff_s$first_encounter <- +(!duplicated(vdff_s$last_name))

vdff_s$first_encounter = factor(vdff_s$first_encounter)

vdff_filtered = vdff_s %>% filter(author_affil_simp == "D" | author_affil_simp == "R")

vdff_filtered$aligned_vote = case_when(((vdff_filtered$final_env_tag == "pro") & (vdff_filtered$leg_vote == "Yea") & (vdff_filtered$author_affil_simp == "D")) ~ "Aligned",
                                       ((vdff_filtered$final_env_tag == "pro") & (vdff_filtered$leg_vote == "Nay") & (vdff_filtered$author_affil_simp == "D")) ~ "NotAligned",
                                       ((vdff_filtered$final_env_tag == "pro") & (vdff_filtered$leg_vote == "Nay") & (vdff_filtered$author_affil_simp == "R")) ~ "Aligned",
                                       ((vdff_filtered$final_env_tag == "pro") & (vdff_filtered$leg_vote == "Yea") & (vdff_filtered$author_affil_simp == "R")) ~ "NotAligned",
                                       ((vdff_filtered$final_env_tag == "anti") & (vdff_filtered$leg_vote == "Nay") & (vdff_filtered$author_affil_simp == "D")) ~ "Aligned",
                                       ((vdff_filtered$final_env_tag == "anti") & (vdff_filtered$leg_vote == "Yea") & (vdff_filtered$author_affil_simp == "D")) ~ "NotAligned",
                                       ((vdff_filtered$final_env_tag == "anti") & (vdff_filtered$leg_vote == "Yea") & (vdff_filtered$author_affil_simp == "R")) ~ "Aligned",
                                       ((vdff_filtered$final_env_tag == "anti") & (vdff_filtered$leg_vote == "Nay") & (vdff_filtered$author_affil_simp == "R")) ~ "NotAligned",
                                       #((vdff_filtered$leg_vote == "NV") | (vdff_filtered$leg_vote == "Absent")) ~ "NV",
                                       TRUE ~ NA)


vdff_filtered$aligned_vote = factor(vdff_filtered$aligned_vote)
vdff_filtered$leg_vote = factor(vdff_filtered$leg_vote)

vote = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
                  BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
                + addtl_authors_flag.y + 
                  + author_affil_simp + state + year + 
                  Disaster.Type + Total.Affected + 
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + search_val_filled, 
                family = binomial(link = "logit"),
                data = vdff_filtered)

summary(vote)


vreg1 = glm(aligned_vote ~  BE_ProxDays_reg, 
           family = binomial(link = "logit"),
           data = vdff_filtered)

summary(vreg1)



vreg2 = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq, 
           family = binomial(link = "logit"),
           data = vdff_filtered)

summary(vreg2)



vreg3 = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE, 
           family = binomial(link = "logit"),
           data = vdff_filtered)


vreg4 = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
           + author_affil_simp + addtl_authors_flag.y, 
           family = binomial(link = "logit"),
           data = vdff_filtered)


vreg5 = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
           + addtl_authors_flag.y + 
             + author_affil_simp +
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg, 
           family = binomial(link = "logit"),
           data = vdff_filtered)

vreg6 = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
           + addtl_authors_flag.y + 
             + author_affil_simp +
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg +
             Disaster.Type + Total.Affected,
           family = binomial(link = "logit"),
           data = vdff_filtered)


vreg7 = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
             BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
           + addtl_authors_flag.y + 
             + author_affil_simp +
             re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg +
             Disaster.Type + Total.Affected + state + year,
           family = binomial(link = "logit"),
           data = vdff_filtered)

vfull_spec = glm(aligned_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
                  BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
                + addtl_authors_flag.y + 
                  + author_affil_simp + state + year + 
                  Disaster.Type + Total.Affected + 
                  re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + search_val_filled, 
                family = binomial(link = "logit"),
                data = vdff_filtered)


stargazer(vreg1, vreg2, vreg3, vreg4, vreg5, vreg6, vreg7, vfull_spec, omit = c("state", "year", "re_taged_prim_topic", "Disaster.Type"))



vdff_filtered_a = vdff_filtered %>% filter(aligned_vote == "Aligned")
vdff_filtered_na = vdff_filtered %>% filter(aligned_vote == "NotAligned")


vdff_filtered_votes = vdff_filtered %>% filter((leg_vote == "Yea") | (leg_vote == "Nay"))
vdff_filtered_votes$leg_vote = factor(vdff_filtered_votes$leg_vote )

vv_reg1 = glm(leg_vote ~  BE_ProxDays_reg, 
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)

summary(vv_reg1)

vv_reg2 = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq, 
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)

summary(vv_reg2)



vv_reg3 = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
              BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE, 
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)


summary(vv_reg3)

vv_reg4 = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
              BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
            + author_affil_simp + addtl_authors_flag.y, 
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)

summary(vv_reg4)


vv_reg5 = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
              BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
            + addtl_authors_flag.y + 
              + author_affil_simp +
              re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg, 
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)

summary(vv_reg5)

vv_reg6 = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
              BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
            + addtl_authors_flag.y + 
              + author_affil_simp +
              re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg +
              Disaster.Type + Total.Affected,
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)

summary(vv_reg6)


vv_reg7 = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
              BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
            + addtl_authors_flag.y + 
              + author_affil_simp +
              re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg +
              Disaster.Type + Total.Affected + state + year,
            family = binomial(link = "logit"),
            data = vdff_filtered_votes)

summary(vv_reg7)

vv_full_spec = glm(leg_vote ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
                   BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
                 + addtl_authors_flag.y + 
                   + author_affil_simp + state + year + 
                   Disaster.Type + Total.Affected + 
                   re_taged_prim_topic + re_taged_prim_topic:BE_ProxDays_reg + search_val_filled, 
                 family = binomial(link = "logit"),
                 data = vdff_filtered_votes)
summary(vv_full_spec)


stargazer(vv_reg1, vv_reg2, vv_reg3, vv_reg4, vv_reg5, vv_reg6, vv_reg7, vv_full_spec, omit = c("state", "year", "primary_topic", "Disaster.Type"))

models <- list(vv_reg1, vv_reg2, vv_reg3, vv_reg4, vv_reg5, vv_reg6, vv_reg7, vv_full_spec)
do.call(stargazer, c(models, list(type = "text")))




####placebo check
n_rows <- nrow(vdff_filtered_votes)

vv_leg1 = vdff_filtered %>% filter(leg_vote == "Yea")
vv_leg2 = vdff_filtered %>% filter(leg_vote == "Nay")

nrow(vv_leg2) / nrow(vdff_filtered)

vdff_filtered_votes <- vdff_filtered_votes %>%
  mutate(row_id = row_number())

fraction <- 0.2

# 3. Sample rows stratified by leg_votes
set.seed(123)
sampled_data <- vdff_filtered_votes %>%
  group_by(leg_vote) %>%
  sample_frac(size = fraction) %>%
  ungroup()

# 4. Tag the sampled rows in the original data
vdff_filtered_votes <- vdff_filtered_votes %>%
  mutate(samplerows = if_else(row_id %in% sampled_data$row_id, 1, 0),
         samplerows = factor(samplerows))

vv_full_spec_test = glm(samplerows ~  BE_ProxDays_reg + BE_ProxDays_reg_sq + ownst_BE + 
                     BE_ProxDays_reg:ownst_BE +  BE_ProxDays_reg_sq:ownst_BE
                   + addtl_authors_flag.y + 
                     + author_affil_simp + state + year + 
                     Disaster.Type + Total.Affected + 
                     primary_topic + primary_topic:BE_ProxDays + search_val_filled, 
                   family = binomial(link = "logit"),
                   data = vdff_filtered_votes)
summary(vv_full_spec_test)




# Hypothesis 2 : Voting outcomes of environmental legislation targeting environmental
# disasters and environmental risk (e.g. bills on disaster resilience and vdffmitigation) will be
# most affected by proximity to  disaster



# 
# # Hypothesis 3 : The passage rate of pro-environmental bills will be influenced by the overall
# # frequency of natural disasters in a given year and state, with more frequent natural disasters
# # being associated with higher bill passage rate regardless of proximity.
# 
# year_data = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/year_data.csv")
# year_state_data = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/year_state_data.csv")
# state_data = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/state_data.csv")
# 
# year_state_df = fdf_for_reg[,!grepl("^hist",names(fdf_for_reg))]
# year_state_df = left_join(year_state_df, year_data, by = c("end_year"))
# year_state_df = left_join(year_state_df, state_data, by = c("state" = "NewLocs"))
# year_state_df = left_join(year_state_df, year_state_data, by = c("end_year" = "end_year", "state" = "NewLocs"))
# 
# summary(year_state_df$year_state_count)
# 
# 
# year_regs = glm(proenv_outcome ~  year_count + state_count + year_count * state_count
#                   + addtl_authors_flag + 
#                   + author_affil_simp +
#                   Disaster.Type + Total.Affected,
#                family = binomial(link = "logit"),
#                data = year_state_df)
# summary(year_regs)
# 
# year_regs = glm(proenv_outcome ~  year_count + state_count + year_count * state_count +
#                   BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                   BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                   + addtl_authors_flag + 
#                   + author_affil_simp +
#                   Disaster.Type + Total.Affected,
#                 family = binomial(link = "logit"),
#                 data = year_state_df)
# summary(year_regs)
# 
# 
# 
# 




# trash -------------------------------------------------------------------

# waste = fdf_for_reg %>% filter(primary_topic == "Waste")
# water = fdf_for_reg %>% filter(primary_topic == "Water")
# pollution = fdf_for_reg %>% filter(primary_topic == "Pollution")
# disaster = fdf_for_reg %>% filter(primary_topic == "Disaster")
# toxics = fdf_for_reg %>% filter(primary_topic == "Toxics and Chemicals")
# land = fdf_for_reg %>% filter(primary_topic == "Land")
# wildlife = fdf_for_reg %>% filter(primary_topic == "Wildlife")
# food = fdf_for_reg %>% filter(primary_topic == "Food")
# disaster_mit = fdf_for_reg %>% filter(primary_topic == "Disaster Mitigation and Resilience")
# renewable = fdf_for_reg %>% filter(primary_topic == "Renewable Energy")
# wildlife = fdf_for_reg %>% filter(primary_topic == "Wildlife")
# utilities = fdf_for_reg %>% filter(primary_topic == "Utilities")
# climate = fdf_for_reg %>% filter(primary_topic == "Climate Change")
# infrastructure = fdf_for_reg %>% filter(primary_topic == "Infrastructure and Construction")
# emissions = fdf_for_reg %>% filter(primary_topic == "Emissions")
# pub_progs = fdf_for_reg %>% filter(primary_topic == "Public Programs")
# outdoor = fdf_for_reg %>% filter(primary_topic == "Outdoor Recreation")
# 
# all_disaster = fdf_for_reg %>% filter((primary_topic == "Disaster") | primary_topic == "Disaster Mitigation and Resilience")
# 
# 
# waste_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                  BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                  + addtl_authors_flag + 
#                  + author_affil_simp + state + year +
#                  Disaster.Type + Total.Affected, 
#                family = binomial(link = "logit"),
#                data = waste)
# summary(waste_reg)
# 
# water_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                   BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                   + addtl_authors_flag + 
#                   + author_affil_simp + state + year +
#                   Disaster.Type + Total.Affected, 
#                 family = binomial(link = "logit"),
#                 data = water)
# summary(water_reg)
# 
# pollution_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                   BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                   + addtl_authors_flag + 
#                   + author_affil_simp + state + year +
#                   Disaster.Type + Total.Affected, 
#                 family = binomial(link = "logit"),
#                 data = pollution)
# summary(pollution_reg)
# 
# disaster_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                       BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                       + addtl_authors_flag + 
#                       + author_affil_simp + state + year +
#                       Disaster.Type + Total.Affected, 
#                     family = binomial(link = "logit"),
#                     data = disaster)
# summary(disaster_reg)
# 
# disaster_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                      BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                      + addtl_authors_flag + 
#                      + author_affil_simp + state + year +
#                      Disaster.Type + Total.Affected, 
#                    family = binomial(link = "logit"),
#                    data = disaster)
# summary(disaster_reg)
# 
# disaster_mit_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                      BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                      + addtl_authors_flag + 
#                      + author_affil_simp + state + year +
#                      Disaster.Type + Total.Affected, 
#                    family = binomial(link = "logit"),
#                    data = disaster_mit)
# summary(disaster_mit_reg)
# 
# all_disaster_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                          BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                          + addtl_authors_flag + 
#                          + author_affil_simp + state + year +
#                          Disaster.Type + Total.Affected, 
#                        family = binomial(link = "logit"),
#                        data = all_disaster)
# summary(all_disaster_reg)
# 
# renewable_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                          BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                          + addtl_authors_flag + 
#                          + author_affil_simp + state, 
#                        family = binomial(link = "logit"),
#                        data = renewable)
# summary(renewable_reg)
# 
# toxics_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                          BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                          + addtl_authors_flag + 
#                          + author_affil_simp + state + year +
#                          Disaster.Type + Total.Affected, 
#                        family = binomial(link = "logit"),
#                        data = toxics)
# summary(toxics_reg)
# 
# land_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                    BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                    + addtl_authors_flag + 
#                    + author_affil_simp + state + year +
#                    Disaster.Type + Total.Affected, 
#                  family = binomial(link = "logit"),
#                  data = land)
# summary(land_reg)
# 
# wildlife_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                  BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                  + addtl_authors_flag + 
#                  + author_affil_simp + state + year +
#                  Disaster.Type + Total.Affected, 
#                family = binomial(link = "logit"),
#                data = wildlife)
# summary(wildlife_reg)
# 
# 
# food_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                      BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                      + addtl_authors_flag + 
#                      + author_affil_simp + state + year +
#                      Disaster.Type + Total.Affected, 
#                    family = binomial(link = "logit"),
#                    data = food)
# summary(food_reg)

# utilities_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                  BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                  + addtl_authors_flag + 
#                  + author_affil_simp + state + year +
#                  Disaster.Type + Total.Affected, 
#                family = binomial(link = "logit"),
#                data = utilities)
# summary(utilities_reg)

# 
# infrastructure_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE + 
#                  BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                  + addtl_authors_flag + 
#                  + author_affil_simp + state + year +
#                  Disaster.Type + Total.Affected, 
#                family = binomial(link = "logit"),
#                data = infrastructure)
# summary(infrastructure_reg)
# 
# emissions_reg = glm(proenv_outcome ~  BE_ProxDays + BE_ProxDays_sq + ownst_BE +
#                  BE_ProxDays * ownst_BE + BE_ProxDays_sq * ownst_BE +
#                  + addtl_authors_flag +
#                  + author_affil_simp + state + year +
#                  Disaster.Type + Total.Affected,
#                family = binomial(link = "logit"),
#                data = emissions)
# summary(emissions_reg)
# 




