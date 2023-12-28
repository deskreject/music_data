#.............................
# Author: Alexander Staub
# Description: Code for similarity change analysis based on years for aom 2024
# Date created: 21.12.2023
# dependencies:
## script "data_preparation_MB_spotify_aom_2024" under "data_creation"
#.............................

#load the necessary dataframe
labels_year_similarity_df  <- readRDS(here::here("data", "interim_data", "similarity_scores_label_years_aom_2023.rda")) %>% 
  ungroup()

#transform DVs to numeric
cols_to_convert <- c("mean", "median", "sd", "cv")
labels_year_similarity_df[, cols_to_convert] <- lapply(labels_year_similarity_df[, cols_to_convert], function(x) as.numeric(as.character(x)))


#remove irrelevant years and irrelevant labels (less than three songs)
labels_year_similarity_df <- labels_year_similarity_df %>% 
  filter(release_year > 1997, release_year < 2006 ) %>% 
  filter(n_songs > 2)

#add in the period variable
labels_year_similarity_df <- labels_year_similarity_df %>%
  group_by(release_year) %>%
  arrange(release_year) %>%
  dplyr::summarise(period = 0) %>%
  mutate(period = c(-3:4)) %>%
  left_join(labels_year_similarity_df, ., by="release_year") %>% 
  #remove NA labels otherwise they will be misclassified as NA_int and NA_ameria later on
  filter(is.na(label_name) == F)

#add in the indicator variable
labels_year_similarity_df <- labels_year_similarity_df %>% 
  group_by(label_name_new)%>%
  dplyr::summarise(label_id = 0) %>%
  mutate(label_id = row_number(.)) %>%
  left_join(labels_year_similarity_df, ., by = "label_name_new")

#add in a "post_treatment" binary variable
labels_year_similarity_df$post_treatment <- if_else(labels_year_similarity_df$release_year > 2001,
                                                    1,
                                                    0)


#####----------------- DiD analysis: US vs Europe similarity indiator analysis over 4 years ------ #####


#if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
#if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
#if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
#if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects
#if (!require(texreg)) install.packages("texreg"); library(texreg) #creating tables for latex

library(did)
library(fixest)
library(lfe)
library(alpaca)
library(texreg)

#..................................................
# looped estimation of all variables - DiD package
#..................................................

#extract the dependent variables

dependent_variables <- names(labels_year_similarity_df[,9:12])

# loop estimation - balanced

model_list_similarity <- lapply(dependent_variables, function(dv){
  
  att_gt(yname = dv,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = labels_year_similarity_df,
         allow_unbalanced_panel = TRUE,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)

names(model_list_similarity) <- paste0(dependent_variables)

# create plots 

event_plots_similarity <- lapply(seq_along(model_list_similarity), function(model){
  
  ggdid(model_list_similarity[[model]],
        title = dependent_variables[model],
        ylab = "change in similarity metric")
  
})

#rename
names(event_plots_similarity) <- paste0(dependent_variables)

#create grid - balanced plots
do.call(grid.arrange, c(event_plots_similarity, ncol = 2, nrow = 2))

# summarise models 

att_similarity <- lapply(model_list_similarity, function(model){
  
  aggte(model, type = 'simple')
  
})

# extract estimate and conf int by term 

extract_model_info <- function(model_list) {
  lapply(model_list, function(model) {
    data.frame(
      yname = model$DIDparams$yname,
      att = model$overall.att,
      ci_lower = model$overall.att - 1.96 * model$overall.se,
      ci_upper = model$overall.att + 1.96 * model$overall.se
    )
  })
}

info_similarity <- do.call(rbind, extract_model_info(att_similarity))

# plot the atts

#dodge_width <- 0.25

ggplot(info_similarity, aes(x = att, y = reorder(yname, att))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.2) +
  coord_flip() +  # Flips the axes before adding other components
  geom_vline(xintercept = 0.00 , color = "red", linewidth = 0.5, linetype = "solid") +  # Add a horizontal line, increase size for visibility
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Variable", x = "Average Treatment Effect (ATT)")

#..................................................
# looped estimation of all variables - ols and fixest packages package
#..................................................

ols_similarity_model <- lapply(dependent_variables, function(dv){
  
  formula <- as.formula(paste(dv, "~ is_US * post_treatment"))
    
    lm(formula, data = labels_year_similarity_df)
    
  }
  )


ols_fixed_effects_model <- lapply(dependent_variables, function(dv){
  
  formula <- as.formula(paste(dv, "~ is_US * post_treatment| label_id + release_year | 0 | label_id"))
  
  felm(formula , data = labels_year_similarity_df)
  
}
)

#moderator variables - ols

ols_similarity_model_mod_maj <- lapply(dependent_variables, function(dv){
  
  formula <- as.formula(paste(dv, "~ is_US * post_treatment*is_major_label"))
  
  lm(formula, data = labels_year_similarity_df)
  
}
)

#moderator variables - fixed effects ols

ols_fixed_effects_model_mod_maj <- lapply(dependent_variables, function(dv){
  
  formula <- as.formula(paste(dv, "~ is_US * post_treatment * is_major_label| label_id + release_year | 0 | label_id"))
  
  felm(formula , data = labels_year_similarity_df)
  
}
)



#.................
# tables
#.................

#ols results
texreg(list(ols_similarity_model[[1]],
       ols_similarity_model[[2]],
       ols_similarity_model[[3]],
       ols_similarity_model[[4]]))

#fixed effects
texreg(list(ols_fixed_effects_model[[1]],
            ols_fixed_effects_model[[2]],
            ols_fixed_effects_model[[3]],
            ols_fixed_effects_model[[4]]))

#ols results - moderators
texreg(list(ols_similarity_model_mod_maj[[1]],
       ols_similarity_model_mod_maj[[2]],
       ols_similarity_model_mod_maj[[3]],
       ols_similarity_model_mod_maj[[4]]))

#fixed effects - moderators
texreg(list(ols_fixed_effects_model_mod_maj[[1]],
            ols_fixed_effects_model_mod_maj[[2]],
            ols_fixed_effects_model_mod_maj[[3]],
            ols_fixed_effects_model_mod_maj[[4]]))

