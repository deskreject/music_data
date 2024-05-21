#.............................
# Author: Alexander Staub
# Description: Code for song, artist and similarity data analysis after sql export
# Date created: 21.12.2023
# dependencies:
## script "data_preparation_MB_sql_export_2024" under "data_creation"
#.............................

#load the necessary dataframe
#remix time period
#regular label name configuration
labels_year_similarity_df_remix_new  <- readRDS(here::here("data", "interim_data", "region_song_artists_releases_lab_new_98_05.rda")) %>% 
  ungroup()

#new label name configuration
labels_year_similarity_df_remix_close  <- readRDS(here::here("data", "interim_data", "region_song_artists_releases_lab_close_98_05.rda")) %>%
  ungroup()

#soundscan intro
#regular label name configuration
labels_year_similarity_df_soundscan_new <- readRDS(here::here("data", "interim_data", "region_song_artists_releases_lab_new_87_94.rda"))

#new label name configuration
labels_year_similarity_df_soundscan_close <- readRDS(here::here("data", "interim_data", "region_song_artists_releases_lab_close_87_94.rda"))

#transform DVs to numeric
#cols_to_convert <- c("mean", "median", "sd", "cv")
#labels_year_similarity_df[, cols_to_convert] <- lapply(labels_year_similarity_df[, cols_to_convert], function(x) as.numeric(as.character(x)))


#remove irrelevant years and irrelevant labels (less than three songs)
#labels_year_similarity_df <- labels_year_similarity_df %>% 
  #filter(release_year > 1997, release_year < 2006 ) %>% 
  #filter(n_songs > 2)

#add all the panel dataframes to a list
list_panel_data_new <- list(labels_year_similarity_df_remix_new,
                            labels_year_similarity_df_soundscan_new)


list_panel_data_close <- list(labels_year_similarity_df_remix_close,
                              labels_year_similarity_df_soundscan_close)

#........
# for the label names where they are split by where they release songs
#........

#add in the period variable
list_panel_data_new <- lapply(list_panel_data_new, function(x){

  x %>%
  group_by(release_year) %>%
  arrange(release_year) %>%
  dplyr::summarise(period = 0) %>%
  mutate(period = c(-3:4)) %>%
  left_join(x, ., by="release_year") %>% 
  #remove NA labels otherwise they will be misclassified as NA_int and NA_ameria later on
  filter(is.na(label_name_new) == F) %>% 
    ungroup()
})

#add in the indicator variable
list_panel_data_new <- lapply(list_panel_data_new, function(x){

  x %>% 
  group_by(label_name_new)%>%
  dplyr::summarise(label_id = 0) %>%
  mutate(label_id = row_number(.)) %>%
  left_join(x, ., by = "label_name_new") %>%
    
#add in a "post_treatment" binary variable
  mutate(post_treatment = if_else(release_year > 2001,
                                  1,
                                  0)) %>%
  ungroup()
})

#............
# for the label names where release location is based on majority of label releases
#............

#add in the period variable
list_panel_data_close <- lapply(list_panel_data_close, function(x){
  
  x %>%
    group_by(release_year) %>%
    arrange(release_year) %>%
    dplyr::summarise(period = 0) %>%
    mutate(period = c(-3:4)) %>%
    left_join(x, ., by="release_year") %>% 
    #remove NA labels otherwise they will be misclassified as NA_int and NA_ameria later on
    filter(is.na(label_name_close) == F) %>% 
    ungroup()
})

#add in the indicator variable
list_panel_data_close <- lapply(list_panel_data_close, function(x){
  
  x %>% 
    group_by(label_name_close)%>%
    dplyr::summarise(label_id = 0) %>%
    mutate(label_id = row_number(.)) %>%
    left_join(x, ., by = "label_name_close") %>%
    
    #add in a "post_treatment" binary variable
    mutate(post_treatment = if_else(release_year > 2001,
                                    1,
                                    0)) %>%
    ungroup()
})


#small adjustment to cv value that is above one
#labels_year_similarity_df$cv  <- if_else(labels_year_similarity_df$cv > 1,
 #                                        0.9999,
  #                                       labels_year_similarity_df$cv)

####------------------- Number of songs: US vs Europe ----------------- #####

# if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
# if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
# if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
# if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects
# if (!require(texreg)) install.packages("texreg"); library(texreg) #creating tables for latex
# if (!require(betareg)) install.packages("betareg"); library(betareg) #beta regression


library(did)
library(fixest)
library(lfe)
library(alpaca)
library(texreg)

#..................
# ATT varying over time for number of songs
#..................

#time sequence

time <- seq(1,4)

# loop estimation - balanced


model_list_songs_time <- lapply(list_panel_data_new, function(df){
  
 list_time <- lapply(time, function(t){
  
  df_new <- df %>% 
    filter(period <= t, period >= (t-1)*-1)
    
    att_gt(yname = "n_songs",
           tname = "period",
           idname = "label_id",
           gname = "is_US",
           data = df_new,
           allow_unbalanced_panel = TRUE,
           bstrap = T,
           cband = F,
           clustervars = "label_id")
  })
 
 #give the list items name according to time t
 names(list_time) <- paste0(time, "_years")
 
 #return the list
 return(list_time)
 
})

names(model_list_songs_time) <- c("remix_policy",
                                  "soundscan_policy")

#eventstudy
# create plots - no controls

event_plot_songs <- ggdid(model_list_songs_time$soundscan_policy$`4_years`,
        title = "Eventustdy number of songs 1998 to 2005",
        ylab = "change in number of songs compared to base period")


event_plot_songs

# summarise models 
#.............
#remix policy
#............
att_songs_time_remix_policy <- lapply(model_list_songs_time$remix_policy, function(model) {
    aggte(model, type = 'simple')
})

# Modify the data extraction function to include 'time'
extract_song_model_info <- function(model_list) {
  lapply(model_list, function(model) {
    data.frame(
      yname = model$DIDparams$yname,
      att = model$overall.att,
      ci_lower = model$overall.att - 1.96 * model$overall.se,
      ci_upper = model$overall.att + 1.96 * model$overall.se
    )
  })
}

info_songs_remix <- do.call(rbind, extract_song_model_info(att_songs_time_remix_policy))
info_songs_remix$yname <- paste0(time, "_years")

# Plot the ATTs with time as a grouping aesthetic

# Updated ggplot code without 'coord_flip' and with axis labels corrected
ggplot(info_songs_remix, aes(x = yname, y = att)) +
  geom_point()  +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0.00, color = "red", linewidth = 0.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Time around treatment", y = "Average Treatment Effect (ATT) - remix policy change") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#..................
#sound scan policy
#.................

att_song_time_soundscan_policy <- lapply(model_list_songs_time$soundscan_policy, function(model) {
  aggte(model, type = 'simple')
})

info_songs_soundscan <- do.call(rbind, extract_song_model_info(att_song_time_soundscan_policy))
info_songs_soundscan$yname <- paste0(time, "_years")

# Plot the ATTs with time as a grouping aesthetic

# Updated ggplot code without 'coord_flip' and with axis labels corrected
ggplot(info_songs_soundscan, aes(x = yname, y = att)) +
  geom_point()  +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0.00, color = "red", linewidth = 0.5, linetype = "solid") +
  theme_minimal() +
  labs(x = "Time around treatment", y = "Average Treatment Effect (ATT) - soundscan policy change") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#......
# fixest eventstudy plots
#......

#eventstudy with fixed effects on songs level
eventstudy_fe_n_songs = feols(n_songs ~  i(release_year, is_US, ref = 2001) |label_id ,cluster=~label_id,
                               labels_year_similarity_df)

#plot
iplot(eventstudy_fe_n_songs,
      ref.line = c(2001),
      ci.col='navy',
      ci_level=0.95,
      xlab="years since Billboard policy change",
      ylab="",
      main ="Event study number of songs")

#treatment effects
did_fe_n_songs <- feols(n_songs ~  post_treatment*is_US |label_id ,cluster=~label_id,
                         labels_year_similarity_df)

summary(did_fe_n_songs)

## triple interaction effects

#filter dataset
major_labels_year_similarity_df <- subset(labels_year_similarity_df, is_major_label == 1)
indie_labels_year_similarity_df <- subset(labels_year_similarity_df, is_major_label == 0)

#event study - major label
eventstudy_fe_major_n_songs = feols(n_songs ~  i(period, is_US, ref = 0) |label_id ,cluster=~label_id,
                                     major_labels_year_similarity_df)

eventstudy_fe_indie_major_n_songs = feols(n_songs ~  i(period, is_US, ref = 0) |label_id ,cluster=~label_id,
                                           indie_labels_year_similarity_df)


#event study - indie label


#plot
iplot(eventstudy_fe_major_n_songs,
      ref.line = c(0),
      ci.col='navy',
      ci_level=0.95,
      main="",
      xlab="years since Billboard policy change",
      ylab="")

did_trip_fe_n_songs <- feols(n_songs ~  post_treatment*is_US*is_major_label |label_id ,cluster=~label_id,
                              labels_year_similarity_df)

summary(did_trip_fe_n_songs)


####------------------- Number of artists: US vs Europe ----------------- #####

#if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
#if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
#if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
#if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects
#if (!require(texreg)) install.packages("texreg"); library(texreg) #creating tables for latex
#if (!require(betareg)) install.packages("betareg"); library(betareg) #beta regression


library(did)
library(fixest)
library(lfe)
library(alpaca)
library(texreg)

#..................
# ATT varying over time for number of artists
#..................

#time sequence

time <- seq(1,4)

# loop estimation - balanced


model_list_artists_time <- lapply(time, function(t){
  
  df_new <- labels_year_similarity_df %>% 
    filter(period <= t, period >= (t-1)*-1)
  
  models <- att_gt(yname = "n_artists",
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = df_new,
         allow_unbalanced_panel = TRUE,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
  print(t)
  
  return(models)
}
)

names(model_list_artists_time) <- paste0(time, "_years")

#eventstudy
# create plots - no controls

event_plot_artists <- ggdid(model_list_artists_time[[4]],
                          title = "Eventustdy number of artists",
                          ylab = "change in number of artists compared to base period")


event_plot_artists

# summarise models 

att_artists_time <- lapply(model_list_artists_time, function(model) {
  aggte(model, type = 'simple')
})

# Modify the data extraction function to include 'time'
extract_artists_model_info <- function(model_list) {
  lapply(model_list, function(model) {
    data.frame(
      yname = model$DIDparams$yname,
      att = model$overall.att,
      ci_lower = model$overall.att - 1.96 * model$overall.se,
      ci_upper = model$overall.att + 1.96 * model$overall.se
    )
  })
}

info_artists <- do.call(rbind, extract_artists_model_info(att_artists_time))
info_artists$yname <- paste0(time, "_years")

# Plot the ATTs with time as a grouping aesthetic

# Updated ggplot code without 'coord_flip' and with axis labels corrected
ggplot(info_artists, aes(x = yname, y = att)) +
  geom_point()  +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0.00, color = "red", linewidth = 0.5, linetype = "solid") +
  theme_minimal() +
  labs(title = "Number of Artists",
       x = "Time around treatment",
       y = "Average Treatment Effect (ATT)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#......
# fixest eventstudy plots
#......

#eventstudy with fixed effects on artist level
eventstudy_fe_n_artist = feols(n_artists ~  i(release_year, is_US, ref = 2001) |label_id ,cluster=~label_id,
                        labels_year_similarity_df)

#plot
iplot(eventstudy_fe_n_artist,
      ref.line = c(2001),
      ci.col='navy',
      ci_level=0.95,
      main="Event study of number of artists",
      xlab="years since Billboard policy change",
      ylab="")

#treatment effects
did_fe_n_artist <- feols(n_artists ~  post_treatment*is_US |label_id ,cluster=~label_id,
                         labels_year_similarity_df)

summary(did_fe_n_artist)

## triple interaction effects

#filter dataset
major_labels_year_similarity_df <- subset(labels_year_similarity_df, is_major_label == 1)
indie_labels_year_similarity_df <- subset(labels_year_similarity_df, is_major_label == 0)

#event study - major label
eventstudy_fe_major_n_artist = feols(n_artists ~  i(period, is_US, ref = 0) |label_id ,cluster=~label_id,
                               major_labels_year_similarity_df)

eventstudy_fe_indie_major_n_artist = feols(n_artists ~  i(period, is_US, ref = 0) |label_id ,cluster=~label_id,
                                           indie_labels_year_similarity_df)


#event study - indie label


#plot
iplot(eventstudy_fe_indie_major_n_artist,
      ref.line = c(0),
      ci.col='navy',
      ci_level=0.95,
      main="",
      xlab="years since Billboard policy change",
      ylab="")

did_trip_fe_n_artist <- feols(n_artists ~  post_treatment*is_US*is_major_label |label_id ,cluster=~label_id,
                         labels_year_similarity_df)

summary(did_trip_fe_n_artist)

####------------------- Number of releases: US vs Europe ------------------------ ####

#if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
#if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
#if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
#if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects
#if (!require(texreg)) install.packages("texreg"); library(texreg) #creating tables for latex
#if (!require(betareg)) install.packages("betareg"); library(betareg) #beta regression


library(did)
library(fixest)
library(lfe)
library(alpaca)
library(texreg)

#..................
# ATT varying over time for number of artists
#..................

#time sequence

time <- seq(1,4)

# loop estimation - balanced


model_list_releases_time <- lapply(time, function(t){
  
  df_new <- labels_year_similarity_df %>% 
    filter(period <= t, period >= (t-1)*-1)
  
  models <- att_gt(yname = "n_releases",
                   tname = "period",
                   idname = "label_id",
                   gname = "is_US",
                   data = df_new,
                   allow_unbalanced_panel = TRUE,
                   bstrap = T,
                   cband = F,
                   clustervars = "label_id")
  
  print(t)
  
  return(models)
}
)

names(model_list_releases_time) <- paste0(time, "_years")

#eventstudy
# create plots - no controls

event_plot_releases <- ggdid(model_list_releases_time[[4]],
                            title = "Eventustdy number of releases",
                            ylab = "change in number of releases compared to base period")


event_plot_releases

# summarise models 

att_releases_time <- lapply(model_list_releases_time, function(model) {
  aggte(model, type = 'simple')
})

# Modify the data extraction function to include 'time'
extract_releases_model_info <- function(model_list) {
  lapply(model_list, function(model) {
    data.frame(
      yname = model$DIDparams$yname,
      att = model$overall.att,
      ci_lower = model$overall.att - 1.96 * model$overall.se,
      ci_upper = model$overall.att + 1.96 * model$overall.se
    )
  })
}

info_releases <- do.call(rbind, extract_releases_model_info(att_releases_time))
info_releases$yname <- paste0(time, "_years")

# Plot the ATTs with time as a grouping aesthetic

# Updated ggplot code without 'coord_flip' and with axis labels corrected
ggplot(info_releases, aes(x = yname, y = att)) +
  geom_point()  +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0.00, color = "red", linewidth = 0.5, linetype = "solid") +
  theme_minimal() +
  labs(title = "Number of Releases",
       x = "Time around treatment",
       y = "Average Treatment Effect (ATT)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#......
# fixest eventstudy plots
#......

#eventstudy with fixed effects on artist level
eventstudy_fe_n_releases = feols(n_releases ~  i(release_year, is_US, ref = 2001) |label_id ,cluster=~label_id,
                               labels_year_similarity_df)

#plot
iplot(eventstudy_fe_n_releases,
      ref.line = c(2001),
      ci.col='navy',
      ci_level=0.95,
      main="Event study of number of releases",
      xlab="years since Billboard policy change",
      ylab="")

#treatment effects
did_fe_n_releases <- feols(n_releases ~  post_treatment*is_US |label_id ,cluster=~label_id,
                         labels_year_similarity_df)

summary(did_fe_n_releases)

####----------------- DiD analysis: US vs Europe similarity indiator analysis over 4 years ------ ####


#if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
#if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
#if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
#if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects
#if (!require(texreg)) install.packages("texreg"); library(texreg) #creating tables for latex
#if (!require(betareg)) install.packages("betareg"); library(betareg) #beta regression


library(did)
library(fixest)
library(lfe)
library(alpaca)
library(texreg)
library(betareg)

#..................................................
# looped estimation of all variables - DiD package
#..................................................

#extract the dependent variables

#AOM based - all including the coefficient of variance
#dependent_variables <- names(labels_year_similarity_df[,10:13])

#SMS based - excluding coefficients of variance
dependent_variables <- names(labels_year_similarity_df[,10:12])

# loop estimation - no controls

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

# loop estimation - controls

model_list_similarity_controls <- lapply(dependent_variables, function(dv){
  
  att_gt(yname = dv,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = labels_year_similarity_df,
         allow_unbalanced_panel = TRUE,
         xformla = ~n_songs,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)

names(model_list_similarity) <- paste0(dependent_variables)
names(model_list_similarity_controls) <- paste0(dependent_variables)

# create plots - no controls

event_plots_similarity <- lapply(seq_along(model_list_similarity), function(model){
  
  ggdid(model_list_similarity[[model]],
        title = dependent_variables[model],
        ylab = "change in similarity metric")
  
})

# create plots - controls

event_plots_similarity_controls <- lapply(seq_along(model_list_similarity), function(model){
  
  ggdid(model_list_similarity_controls[[model]],
        title = dependent_variables[model],
        ylab = "change in similarity metric")
  
})

#rename
names(event_plots_similarity) <- paste0(dependent_variables)
names(event_plots_similarity_controls) <- paste0(dependent_variables)

#create grid - balanced plots
do.call(grid.arrange, c(event_plots_similarity, ncol = 3, nrow = 1))
do.call(grid.arrange, c(event_plots_similarity_controls, ncol = 3, nrow = 1))

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

# Fractional Logit Models using glm with quasibinomial
fractional_logit_models <- lapply(dependent_variables, function(dv) {
  formula <- as.formula(paste(dv, "~ is_US * post_treatment"))
  glm(formula, family = quasibinomial(link = "logit"), data = labels_year_similarity_df)
})

# Beta Regression Models
beta_regression_models <- lapply(dependent_variables, function(dv) {
  formula <- as.formula(paste(dv, "~ is_US * post_treatment "))
  betareg(formula, data = labels_year_similarity_df)
})

#.................
# tables
#.................

#ols results
texreg(list(ols_similarity_model[[1]],
       ols_similarity_model[[2]],
       ols_similarity_model[[3]],
       ols_similarity_model[[4]]),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("mean", "median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment") 
       
       )

#fixed effects
texreg(list(ols_fixed_effects_model[[1]],
            ols_fixed_effects_model[[2]],
            ols_fixed_effects_model[[3]],
            ols_fixed_effects_model[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("mean", "median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment"))

#fractional logit models
texreg(list(fractional_logit_models[[1]],
            fractional_logit_models[[2]],
            fractional_logit_models[[3]],
            fractional_logit_models[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("mean", "median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment"))

#beta regression models
texreg(list(beta_regression_models[[1]],
            beta_regression_models[[2]],
            beta_regression_models[[3]],
            beta_regression_models[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("mean", "median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment",
                             "Precision: (phi)"))






#### -------------------- moderator variable estimation: Triple Interaction -----------------####

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

#extract the dependent variables

dependent_variables <- names(labels_year_similarity_df[,9:12])

#.............
# triple interaction - major vs indie
#....................

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

# Fractional Logit Models using glm with quasibinomial
fractional_logit_models_mod_maj <- lapply(dependent_variables, function(dv) {
  formula <- as.formula(paste(dv, "~ is_US * post_treatment*is_major_label"))
  glm(formula, family = quasibinomial(link = "logit"), data = labels_year_similarity_df)
})

# Beta Regression Models
beta_regression_models_mod_maj <- lapply(dependent_variables, function(dv) {
  formula <- as.formula(paste(dv, "~ is_US * post_treatment *is_major_label"))
  betareg(formula, data = labels_year_similarity_df)
})

#.............
#sample split - major vs indie
#.............


#.................
# tables
#.................

#ols results - moderators
texreg(list(ols_similarity_model_mod_maj[[1]],
            ols_similarity_model_mod_maj[[2]],
            ols_similarity_model_mod_maj[[3]],
            ols_similarity_model_mod_maj[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       digits = 3)

#fixed effects - moderators
texreg(list(ols_fixed_effects_model_mod_maj[[1]],
            ols_fixed_effects_model_mod_maj[[2]],
            ols_fixed_effects_model_mod_maj[[3]],
            ols_fixed_effects_model_mod_maj[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1) )

#fractional logit models
texreg(list(fractional_logit_models_mod_maj[[1]],
            fractional_logit_models_mod_maj[[2]],
            fractional_logit_models_mod_maj[[3]],
            fractional_logit_models_mod_maj[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1) )

#beta regression models
texreg(list(beta_regression_models_mod_maj[[1]],
            beta_regression_models_mod_maj[[2]],
            beta_regression_models_mod_maj[[3]],
            beta_regression_models_mod_maj[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("mean", "median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "is major label",
                             "US based*post treatment",
                             "US based*major label",
                             "post treatment*major label",
                             "Triple interaction",
                             "Precision: (phi)"))



#### -------------- moderator variable estimation:  sample split ---------------------------------- ####

split_major <- split(labels_year_similarity_df, labels_year_similarity_df$is_major_label)
names(split_major) <- c("indie", "major")

#ols
OLS_similarity_sample_split <- lapply(split_major, function(label){
  
  models <- lapply(dependent_variables, function(dv){
    
    formula <- as.formula(paste(dv, "~ is_US * post_treatment"))
    
    lm(formula, data = label)
  
  })
  
  names(models) <- paste0(dependent_variables)
  return(models)
  
})

#fractional logit

frac_logit_similarity_sample_split <- lapply(split_major, function(label){
  
  models <- lapply(dependent_variables, function(dv){
    
    formula <- as.formula(paste(dv, "~ is_US * post_treatment"))
    
    glm(formula, family = quasibinomial(link = "logit"), data = label)
    
  })
  
  names(models) <- paste0(dependent_variables)
  return(models)
  
})

#beta regression

beta_reg_similarity_sample_split <- lapply(split_major, function(label){
  
  models <- lapply(dependent_variables, function(dv){
    
    formula <- as.formula(paste(dv, "~ is_US * post_treatment"))
    
    betareg(formula, data = label)
    
  })
  
  names(models) <- paste0(dependent_variables)
  return(models)
  
})

#.......
#DR DiD
#......

#Group attgt

DRDID_similarity_sample_split_temp <- lapply(split_major, function(label){
  
  models <- lapply(dependent_variables, function(dv){
    
    att_gt(yname = dv,
           tname = "period",
           idname = "label_id",
           gname = "is_US",
           data = label,
           allow_unbalanced_panel = TRUE,
           bstrap = T,
           cband = F,
           clustervars = "label_id")
    
  })
  
  names(models) <- paste0(dependent_variables)
  return(models)
  
})

# the aggregated ATTs

DRDID_similarity_sample_split <- lapply(DRDID_similarity_sample_split_temp, function(list){
  
  lapply(list, function(model){
  
  aggte(model, type = 'simple')
  
})
    
})

#........
#tables
#.......

#OLS
texreg(list(OLS_similarity_sample_split$indie$mean,
            OLS_similarity_sample_split$major$mean,
            OLS_similarity_sample_split$indie$median,
            OLS_similarity_sample_split$major$median,
            OLS_similarity_sample_split$indie$sd,
            OLS_similarity_sample_split$major$sd,
            OLS_similarity_sample_split$indie$cv,
            OLS_similarity_sample_split$major$cv),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1) )

#fract logit

texreg(list(frac_logit_similarity_sample_split$indie$mean,
            frac_logit_similarity_sample_split$major$mean,
            frac_logit_similarity_sample_split$indie$median,
            frac_logit_similarity_sample_split$major$median,
            frac_logit_similarity_sample_split$indie$sd,
            frac_logit_similarity_sample_split$major$sd,
            frac_logit_similarity_sample_split$indie$cv,
            frac_logit_similarity_sample_split$major$cv),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1) )

#beta regression

texreg(list(beta_reg_similarity_sample_split$indie$mean,
            beta_reg_similarity_sample_split$major$mean,
            beta_reg_similarity_sample_split$indie$median,
            beta_reg_similarity_sample_split$major$median,
            beta_reg_similarity_sample_split$indie$sd,
            beta_reg_similarity_sample_split$major$sd,
            beta_reg_similarity_sample_split$indie$cv,
            beta_reg_similarity_sample_split$major$cv),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1) )

# DR DID

# Function to create LaTeX table from a given list of models
# ---> FUNCTION NECESSARY FROM "AOM 2024 TABLES" 

#indie labels
create_latex_table(DRDID_similarity_sample_split$indie)
# major labels
create_latex_table(DRDID_similarity_sample_split$major)



####--------------------- Varying time windows --------------------------####

#extract the dependent variables
#AOM 2024 based
#dependent_variables <- names(labels_year_similarity_df[,10:13])
#SMS 2024 based - no CV
dependent_variables <- names(labels_year_similarity_df[,10:12])

#time sequence

time <- seq(1,4)

# loop estimation - balanced

model_list_similarity_time <- lapply(time, function(t){
  
  df_new <- labels_year_similarity_df %>% 
    filter(period <= t, period >= (t-1)*-1)
  
 models <- lapply(dependent_variables, function(dv){
  
  att_gt(yname = dv,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = df_new,
         allow_unbalanced_panel = TRUE,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)
 names(models) <- paste0(dependent_variables) 
 return(models)
}
)

names(model_list_similarity_time) <- paste0(time, "_years")


# summarise models 

att_similarity_time <- lapply(model_list_similarity_time, function(time_list) {
  lapply(time_list, function(model) {
    aggte(model, type = 'simple')
  })
})

# Modify the data extraction function to include 'time'
extract_model_info <- function(model_list, time_values) {
  lapply(seq_along(model_list), function(i) {
    lapply(model_list[[i]], function(model) {
      data.frame(
        yname = model$DIDparams$yname,
        time = time_values[i],
        att = model$overall.att,
        ci_lower = model$overall.att - 1.96 * model$overall.se,
        ci_upper = model$overall.att + 1.96 * model$overall.se
      )
    })
  })
}

info_similarity <- do.call(rbind, do.call(rbind, extract_model_info(att_similarity_time, time)))

# Plot the ATTs with time as a grouping aesthetic

# Sort the data frame by 'yname' alphabetically
info_similarity <- info_similarity[order(info_similarity$yname),]

# Updated ggplot code without 'coord_flip' and with axis labels corrected
ggplot(info_similarity, aes(x = yname, y = att, color = as.factor(time))) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.25), width = 0.2) +
  geom_hline(yintercept = 0.00, color = "red", linewidth = 0.5, linetype = "solid") +
  theme_minimal() +
  labs(title = "ATT plots for similarity coefficients with varying time windows",
       x = "Variable",
       y = "Average Treatment Effect (ATT)",
       color = "Time Interval in years") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#...................
# effect size check
#...................

descriptives_by_group_treatment <- labels_year_similarity_df %>%
  group_by(is_US, post_treatment) %>%
  summarise(
    across(c(mean, median, sd, cv),
           list(mean = ~mean(.x, na.rm = TRUE),
                median = ~median(.x, na.rm = TRUE),
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}") # to create new column names
  ) %>%
  ungroup()
  

#.............
# non DiD models
#...............

#### ------------------- AOM 2024 TABLES: US vs Europe similarity indiator analysis 4 years  ------- ####

#...........................................
# Descriptives of the similarity variables
#..........................................

table_for_descriptives <- labels_year_similarity_df %>%
  dplyr::select(n_songs,
                n_artists,
                mean,
                median,
                sd,
                is_US) %>%
  rename("1 Number of Songs" = "n_songs",
         "2 Number of Artists" = "n_artists",
         "3 Mean Cosine Similarity" = "mean",
         "4 Median Cosine Similarity" = "median",
         "5 Standard Deviation" = "sd",
         "6 Label Released Songs in US" = "is_US")


# Load necessary libraries
#if (!require(Hmisc)) install.packages("Hmisc"); library(Hmisc) #for corr table
library(psych)
library(Hmisc)
library(xtable)


# Assuming 'table_for_descriptives' is your dataframe
# Calculate descriptive statistics
descriptives <- as.data.frame(psych::describe(table_for_descriptives))

# Select relevant statistics
selected_stats <- descriptives %>% dplyr::select(mean, median, min, max, n)

# Calculate the correlation matrix
cor_matrix <- rcorr(as.matrix(table_for_descriptives))$r  # If you want p-values or n, use '$P' or '$n' respectively

# Convert statistics dataframe to matrix and transpose it, so variables become columns
stats_matrix <- as.matrix(t(selected_stats))

# Make sure the row names of both matrices match and are in the same order
rownames(cor_matrix) <- colnames(stats_matrix)

# Remove off-diagonal elements by replacing them with NA
cor_matrix[upper.tri(cor_matrix)] <- NA

# Replace column names with numbers for cor_matrix
colnames(cor_matrix) <- seq_len(ncol(cor_matrix))

# Combine the matrices below eachother
combined_matrix <- rbind(cor_matrix, stats_matrix)

# Convert the combined matrix to LaTeX code
latex_table <- xtable(combined_matrix, 
                      caption = "Note: observations at the record label level. The number of observations vary as some observations had to be dropped in order to calculate central tendency and dispersion parameter values",
                      align = c("l", rep("r", ncol(combined_matrix))))
print(latex_table, 
      type = "latex",
      include.rownames = TRUE, 
      floating = T,
      #make sideways
      #tabular.environment = 'sidewaystable',
      sanitize.text.function = function(x) {x <- gsub("NA", "", x); return(x)})

# Output the LaTeX code to a file


#.........
# DRDiD
#.........

#install package xtable
library(xtable)

#check the model summary of the DiD by C & S'A
summary.MP(att_similarity_time$`2_years`$mean)


# Function to create LaTeX table from a given list of models
create_latex_table <- function(years_list) {
  # Initialize a matrix to store results (7 rows for different metrics, 4 columns for models)
  results <- matrix(nrow = 7, ncol = 4)
  rownames(results) <- c("ATT", "SE", "Conf Int", "Number of observations", "Number of units", "Number of periods", "Bootstrap iterations")
  colnames(results) <- c("mean", "median", "sd", "cv")
  
  # Fill the matrix with extracted values
  for (model_name in colnames(results)) {
    model <- years_list[[model_name]]
    results["ATT", model_name] <- sprintf("%.3f", model$overall.att)
    results[2, model_name] <- sprintf("(%.3f)", model$overall.se)  # SE
    
    # Confidence intervals
    lower_ci_bound <- model$overall.att - 1.96 * model$overall.se
    upper_ci_bound <- model$overall.att + 1.96 * model$overall.se
    results[3, model_name] <- sprintf("[%.3f,%.3f]", lower_ci_bound, upper_ci_bound)
    
    # Other details
    results["Number of observations", model_name] <- nrow(model$DIDparams$data)
    results["Number of units", model_name] <- model$DIDparams$n
    results["Number of periods", model_name] <- model$DIDparams$nT
    results["Bootstrap iterations", model_name] <- model$DIDparams$biters
  }
  
  # Create LaTeX table
  latex_table <- xtable(results, 
                        caption = "Based on cluster bootstrapped standard errors. Estimation method \"Doubly Robust\" based on Sant'Anna and Zhao, 2020. Unit of observation is the record label, the treated group refers to record labels in the US and the control group refers to record labels in Europe (DE, FR, GB, IT) that released songs during the time period of relevance",
                        label = "table:label_here")
  print(latex_table, include.rownames = TRUE, hline.after = c(-1,0,3,7), comment = FALSE)
  
}

# Example usage
create_latex_table(att_similarity_time$`4_years`)

#................
#ols results
#................

texreg(list(ols_similarity_model[[1]],
            ols_similarity_model[[2]],
            ols_similarity_model[[3]],
            ols_similarity_model[[4]]),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("Mean", "Median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment (ATT)") 
       
)

#........................
#fractional logit models
#........................


texreg(list(fractional_logit_models[[1]],
            fractional_logit_models[[2]],
            fractional_logit_models[[3]],
            fractional_logit_models[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("Mean", "Median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment (ATT)"))
#.......................
#beta regression models
#.......................

texreg(list(beta_regression_models[[1]],
            beta_regression_models[[2]],
            beta_regression_models[[3]],
            beta_regression_models[[4]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("Mean", "Median", "Standard Dev.", "Variance Coef"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment (ATT) ",
                             "Precision: (phi)"))


#### -------- ISTO regression tables: By DV rather than model --------------------------- #####

#if (!require(modelsummary)) install.packages("modelsummary"); library(modelsummary) # mainly for the tabyl package

library(modelsum)

#......................
# Coefficients of songs, artists and releases
#......................

texreg(list(did_fe_n_artist,
            did_fe_n_songs,
            did_fe_n_releases),
       digits = 3,
       #include the mean and median for the dependent variables
       custom.model.names = c("Number of Artists", "Number of Songs", "Number of Releases"),
       custom.coef.names = c("post treatment",
                             "US based*post treatment (ATT)"))


#......................
# MAIN ANALYSIS Looped - by DV
#......................

lapply(seq_along(1:4), function(i){

texreg(list(ols_similarity_model[[i]],
            fractional_logit_models[[i]],
            beta_regression_models[[i]],
            beta_regression_models[[i]]),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("OLS", "Frac. Log.", "Beta", "DR DID"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment (ATT)",
                             "precision: (phi)")
)
})

#.........................
# MODERATOR: Major v indie
#.........................

# triple DiD

#beta regression models
texreg(list(ols_similarity_model_mod_maj[[3]],
            fractional_logit_models_mod_maj[[3]],
            beta_regression_models_mod_maj[[3]]),
       stars = c(0.001, 0.01, 0.05, 0.1),
       digits = 3,
       custom.model.names = c("OLS", "Frac. Log.", "Beta"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "is major label",
                             "US based*post treatment",
                             "US based*major label",
                             "post treatment*major label",
                             "Triple interaction",
                             "Precision: (phi)"))

# sample split
texreg(list(OLS_similarity_sample_split$indie$sd,
            OLS_similarity_sample_split$major$sd,
            frac_logit_similarity_sample_split$indie$sd,
            frac_logit_similarity_sample_split$major$sd,
            beta_reg_similarity_sample_split$indie$sd,
            beta_reg_similarity_sample_split$major$sd,
            beta_reg_similarity_sample_split$indie$sd,
            beta_reg_similarity_sample_split$major$sd),
       digits = 3,
       stars = c(0.001, 0.01, 0.05, 0.1),
       custom.model.names = c("Indie", "Major",
                              "Indie", "Major",
                              "Indie", "Major",
                              "Indie", "Major"),
       custom.coef.names = c("Intercept",
                             "US based",
                             "post treatment",
                             "US based*post treatment (ATT)",
                             "precision: (phi)"))

#Dr DiD
# ---> FUNCTION NECESSARY FROM "AOM 2024 TABLES" 

#indie labels
create_latex_table(DRDID_similarity_sample_split$indie)
# major labels
create_latex_table(DRDID_similarity_sample_split$major)

