library(haven)
library(stringr)
library(tm)
library(corpus)
library(tidytext)
library(textclean)
library(lubridate)
library(hunspell)
library(stopwords)
library(textmineR)
library(scales)
library(stopwords)
library(stargazer)
library(rstan)
library(rsample)
library(tidymodels)
library(textrecipes)
library(discrim)
library(naivebayes)
library(hardhat)

#prediction steps referenced from Hvitfeldt & Silge, 2022
#https://smltar.com/mlclassification#classfirstattemptlookatdata


#df = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/categorized_data_for_analysis_0428.csv")
        
#import pre-categorized data
df = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Data For Analysis/categorizedlegislation_wo_proflag.csv")
df$categorized_training = ifelse(is.na(df$pro_flag), 0, 1)
df$pro_flag = factor(df$pro_flag)
df$proenv = ifelse((df$pro_flag == 1), "pro", "anti")


clean_df = df %>% mutate(clean_summary = summary %>%
                           replace_non_ascii() %>% 
                           tolower() %>%
                           replace_emoticon() %>%
                           add_missing_endmark() %>%
                           replace_symbol() %>% 
                           replace_contraction() %>%
                           replace_word_elongation() %>%
                           str_replace_all("[[:punct:]]", " ") %>%
                           make_plural() %>%
                           str_squish() %>%
                           str_trim(), 
                         
                         clean_description = description %>%
                           replace_non_ascii() %>% 
                           tolower() %>%
                           replace_emoticon() %>%
                           add_missing_endmark() %>%
                           replace_symbol() %>% 
                           replace_contraction() %>%
                           replace_word_elongation() %>%
                           str_replace_all("[[:punct:]]", " ") %>%
                           make_plural() %>%
                           str_squish() %>%
                           str_trim()) 


#use already keyword categorized data to train and test model
cdf = clean_df %>% filter(categorized_training == 1)
ucdf = clean_df %>% filter(categorized_training == 0)


set.seed(46)

cdf_split = initial_split(cdf, strata = proenv)

c_train = training(cdf_split)
c_test = testing(cdf_split)

c_folds <- vfold_cv(c_train)


#create recipe using summary and topic
cat_rec = recipe(proenv ~ clean_summary + primary_topic, data = c_train)
cat_rec = cat_rec %>% 
  step_novel(primary_topic) %>% 
  step_unknown(primary_topic) %>% 
  step_dummy(primary_topic) 

cat_rec = cat_rec %>%
  step_tokenize(clean_summary) %>%
  step_tokenfilter(clean_summary, max_tokens = 1000) %>%
  step_tfidf(clean_summary)


tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")


cat_wf = workflow() %>%
  add_recipe(cat_rec, blueprint = sparse_bp) %>%
  add_model(tune_spec)

lambda_grid = grid_regular(penalty(range = c(-5, 0)), levels = 20)

cat_rs <- tune_grid(
  cat_wf,
  c_folds,
  control = control_resamples(save_pred = TRUE),
  grid = lambda_grid,
  metrics = metric_set(accuracy, sensitivity, specificity, roc_auc))

show_best(cat_rs, metric = "accuracy")
show_best(cat_rs, metric = "roc_auc")

best_params <- select_best(cat_rs, metric = "accuracy")

conf_mat_resampled(cat_rs, tidy = FALSE, parameters = best_params) %>%
  autoplot(type = "heatmap")


choose_acc <- cat_rs %>%
  select_by_pct_loss(metric = "accuracy", -penalty)

final_wf <- finalize_workflow(cat_wf, choose_acc)

final_fitted <- last_fit(final_wf, cdf_split)
collect_metrics(final_fitted)

collect_predictions(final_fitted) %>%
  conf_mat(truth = proenv, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

final_model <- fit(final_wf, cdf)


ucdf_preds_class <- predict(final_model, new_data = ucdf, type = "class")
ucdf_preds_prob <- predict(final_model, new_data = ucdf, type = "prob")


ucdf_with_preds <- ucdf %>%
  bind_cols(ucdf_preds_class %>% rename(pred_class = .pred_class),
            ucdf_preds_prob)

ucdf_with_preds$pred_class_text = ifelse(ucdf_with_preds$pred_class == "anti", "anti", "pro")


###recreate dataframe
catdf = bind_rows(cdf, ucdf_with_preds)
catdf$final_env_tag = ifelse(is.na(catdf$pro_flag), catdf$pred_class_text, catdf$proenv)

