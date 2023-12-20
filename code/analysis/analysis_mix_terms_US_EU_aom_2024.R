#.............................
# Author: Alexander Staub
# Description: Code for remix analysis with models for aom 2024
# Date created: 11.12.2023
# dependencies:
## script "data_preparation_MB_spotify_aom_2024" under "data_creation"
#.............................

#load the necessary dataframe
release_labels_merged_unique_tracks  <- readRDS(here::here("data", "interim_data", "unique_tracks_mix_terms_countries_aom_2023.rda"))

#### --------------------------- preparing data for plotting and analysis country --------------------------- #####



#.......
# create proprotions of terms by year by company
#.......

##IMPORTANT - MAKE SURE THERE ARE MAX 30 COLUMNS
# Generate "_found" versions of terms for column names
term_cols <- colnames(release_labels_merged_unique_tracks[15:30])

# Step 1: Calculate count of each term
counts <- release_labels_merged_unique_tracks %>%
  group_by(release_year, release_country, label_name) %>%
  summarise(across(all_of(term_cols), sum, .names = "{.col}_count"))

# Step 2: Create a total count column
counts <- counts %>%
  rowwise() %>%
  mutate(count_total = sum(c_across(ends_with("_count")), na.rm = TRUE))

# Step 3: Calculate proportions for each term, handling division by zero and rounding to 3 decimal places
proportions <- counts %>%
  rowwise() %>%
  mutate(across(ends_with("_count"),
                ~round(ifelse(count_total == 0, 0, .x / count_total), 3),
                .names = "{.col}_prop"))


# Combine counts and proportions
final_df <- counts %>%
  bind_cols(proportions %>% select(ends_with("_prop")))

# Clean up for final output
df_combined <- final_df %>%
  ungroup()

# add in total number of songs
df_combined$total_songs = round(ifelse(df_combined$any_term_count_prop == 0,
                                 0,
                                 df_combined$any_term_count/df_combined$any_term_count_prop),1)

#add in the is_us variable

df_combined$is_US <- ifelse(df_combined$release_country == "US",
                                                    1,
                                                    0)

#remove irrelevant years
df_combined <- df_combined %>% 
  filter(release_year > 1997, release_year < 2006)

#add in a treatment variable
df_combined$post_treatment <- ifelse(df_combined$release_year > 2001,
                                     1,
                                     0)

#...........
#probably not relevant
#...........

# Reshape the data for plotting
df_plot_us <- df_combined %>%
  pivot_longer(cols = ends_with("_prop"), names_to = "term", values_to = "proportion")

# Remove the "_prop" suffix from the term names
df_plot_us$term <- gsub("_prop$", "", df_plot_us$term)

#round to three decimal places
df_plot_us$proportion <- round(df_plot_us$proportion, 3)

# merge in the "is_US" etc indicators

df_country_indicators <- release_labels_merged_unique_tracks %>%
  group_by(release_country, is_US)  %>%
  dplyr::summarise(count = n())  %>%
  dplyr::select(!count)

df_plot_us <- left_join(df_plot_us, 
                         df_country_indicators,
                         by = "release_country") 

# Remove the "any_term" from df_plot
df_plot_no_anyterm_us <- df_plot_us %>%
  filter(term != "any_terms")

# keep only the terms that were of relevance for remixing
df_plot_remixing_us <- df_plot_us %>%
  filter(term %in% c("edit_found", "mix_found", "radio_found", "remix_found"))

#### ------------------- preparing data for DiD package (C & S'A) ------------------------------ ####



#just US vs non-us - plain OLS - CAllaway and Sant'anna

## need to create a treatment period indicator that starts at "1" in period of the shock apparantly (https://bcallaway11.github.io/did/articles/pre-testing.html)
df_combined <- df_combined %>%
  group_by(release_year) %>%
  arrange(release_year) %>%
  dplyr::summarise(period = 0) %>%
  mutate(period = c(-3:4)) %>%
  left_join(df_combined, ., by="release_year") %>% 
  #remove NA labels otherwise they will be misclassified as NA_int and NA_ameria later on
  filter(is.na(label_name) == F)

# make the panel balanced

library(data.table)

##create a table with names for labels that release in two states
#STEP 1: get names of the labels that operate in both regions
international_labels_table <- df_combined %>% 
  group_by(label_name, is_US) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(label_name) %>% 
  dplyr::summarise(count = n()) %>% 
  filter(count > 1)

#STEP 2: filter the df_combined table by those label names and then create the new names
int_labels_table_new_names <- df_combined %>% 
  filter(label_name %in% international_labels_table$label_name) %>% 
  rowwise() %>% 
  mutate(label_name_new = ifelse(is_US == 1,
                                  paste0(label_name,"_america"),
                                  paste0(label_name,"_international"))
  ) %>% 
  dplyr::select(label_name, is_US, label_name_new) %>% 
  distinct(label_name_new, .keep_all = T)

#STEP3: left join to original table

df_combined <- left_join(df_combined,
                         int_labels_table_new_names,
                         by= c("label_name", "is_US"))

#STEP4: ifelse NAs to label name
df_combined$label_name_new <- ifelse(is.na(df_combined$label_name_new) == T,
                                     df_combined$label_name,
                                     df_combined$label_name_new)

##need to create a numeric id
df_combined <-df_combined %>%
  group_by(label_name_new)%>%
  dplyr::summarise(label_id = 0) %>%
  mutate(label_id = row_number(.)) %>%
  left_join(df_combined, ., by = "label_name_new")

# Assuming df_combined is your original data frame

# Step 1: Create all combinations of id and time period
all_combinations <- expand.grid(label_id = unique(df_combined$label_id),
                                period = unique(df_combined$period))

# Step 2: Left join with original data
balanced_panel <- all_combinations %>% 
  left_join(df_combined, by = c("label_id", "period"))

# Step 3: Fill certain columns and impute 0 for others - doesn't map release_country properly
balanced_panel <- balanced_panel %>%
  filter(is.na(label_id) ==F) %>%
  group_by(label_id) %>%
  fill(release_year, label_name, release_country, is_US, .direction = "downup") %>%
  ungroup() %>%
  mutate_at(vars(-label_id, -period, -release_year, -label_name, -release_country),
            list(~replace(., is.na(.), 0)))


#####------------------------ Analysis od term counts using DiD package --------------------------#####

#if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
#if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
#if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
#if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects

library(did)
library(fixest)
library(lfe)
library(alpaca)

#.......................
# individual estimation
#.......................

#create list of models for each term + any term

test_unbalanced <- att_gt(yname = "any_term_count",
       tname = "period",
       idname = "label_id",
       gname = "is_US",
       data = df_combined,
       allow_unbalanced_panel = TRUE,
       bstrap = T,
       cband = F,
       clustervars = "label_id")

test_balanced <- att_gt(yname = "any_term_count",
                        tname = "period",
                        idname = "label_id",
                        gname = "is_US",
                        data = balanced_panel,
                        bstrap = T,
                        cband = F,
                        clustervars = "label_id")


#event study summaries
summary(test_unbalanced)
summary(test_balanced)

#plots
ggdid(test_balanced)

#treatment effects - aggregated
# simple = weighted average of all group-time avereage treatment effects with weights prop to group size
aggte(test_unbalanced, type = 'simple') 
aggte(test_balanced, type = "simple")


#....................................
# looping through terms - counts
#....................................

# Step 1: Create a vector of variable names

variable_names <- names(balanced_panel)

# Variables that end with "_count"

all_count_variables <- grep("_count$", variable_names, value = TRUE) 

# Variables that end with "_count_prop"

count_prop_variables <- grep("_count_prop$", variable_names, value = TRUE)

# Exclude "_count_prop" variables

count_variables <- setdiff(all_count_variables, count_prop_variables) 


# loop estimation - balanced

model_list_count_balanced <- lapply(count_variables, function(term){
  
  att_gt(yname = term,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = balanced_panel,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)

names(model_list_count_balanced) <- paste0(count_variables, "_model_balanced")

# loop estimation - unbalanced


model_list_count_unbalanced <- lapply(count_variables, function(term){
  
  att_gt(yname = term,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = df_combined,
         allow_unbalanced_panel = TRUE,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)

names(model_list_count_unbalanced) <- paste0(count_variables, "_model_unbalanced")

# create plots - balanced

event_plots_balanced <- lapply(seq_along(model_list_count_balanced), function(model){
  
  ggdid(model_list_count_balanced[[model]],
        title = count_variables[model],
        ylab = "change in count")
  
})

#rename
names(event_plots_balanced) <- paste0(count_variables, "_plots_balanced")

#create grid - balanced plots
do.call(grid.arrange, c(event_plots_balanced, ncol = 4, nrow = 4))


# create plots - ununbalanced

event_plots_unbalanced <- lapply(seq_along(model_list_count_unbalanced), function(model){
  
  ggdid(model_list_count_unbalanced[[model]],
        title = count_variables[model],
        ylab = "change in count")
  
})

#rename
names(event_plots_unbalanced) <- paste0(count_variables, "_plots_unbalanced")

#create grid - unbalanced plots
do.call(grid.arrange, c(event_plots_unbalanced, ncol = 4, nrow = 4))

# summarise models - balanced

att_counts_balanced <- lapply(model_list_count_balanced, function(model){
  
  aggte(model, type = 'simple')
  
})

# summarise models - unbalanced

att_counts_unbalanced <- lapply(model_list_count_unbalanced, function(model){
  
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

info_balanced <- do.call(rbind, extract_model_info(att_counts_balanced))
info_unbalanced <- do.call(rbind, extract_model_info(att_counts_unbalanced))

# Add a column to differentiate between balanced and unbalanced
info_balanced$type <- "Balanced"
info_unbalanced$type <- "Unbalanced"

# Combine the data
atts_count_combined_plotting <- rbind(info_balanced, info_unbalanced)


# plot the atts

dodge_width <- 0.25

ggplot(atts_count_combined_plotting, aes(x = att, y = reorder(yname, att), color = type)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = dodge_width)) +
  coord_flip() +  # Flips the axes before adding other components
  geom_vline(xintercept = 0.00 , color = "red", linewidth = 0.5, linetype = "solid") +  # Add a horizontal line, increase size for visibility
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Variable", x = "Average Treatment Effect (ATT)", color = "Model Type")



####---------------------- Analysis term intensity DiD package C & S'A ----------------#####

#if (!require(fixest)) install.packages("fixest"); library(fixest) #for the point estimate and error bar plots for pretrends
#if (!require(lfe)) install.packages("lfe"); library(lfe) #for linear models with multiway clustering and fixed effects
#if (!require(alpaca)) install.packages("alpaca"); library(alpaca) #for non-linear models with multiway clustering and fixed effects
#if (!require(did)) install.packages("did"); library(did) #for non-linear models with multiway clustering and fixed effects

library(did)
library(fixest)
library(lfe)
library(alpaca)

#....................................
# looping through terms - counts
#....................................

# Step 1: Create a vector of variable names

variable_names <- names(balanced_panel)

# Variables that end with "_count"

all_count_variables <- grep("_count$", variable_names, value = TRUE) 

# Variables that end with "_count_prop"

count_prop_variables <- grep("_count_prop$", variable_names, value = TRUE)

# Exclude "_count_prop" variables

count_variables <- setdiff(all_count_variables, count_prop_variables) 


# loop estimation - balanced

model_list_prop_balanced <- lapply(count_prop_variables, function(term){
  
  att_gt(yname = term,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = balanced_panel,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)

names(model_list_prop_balanced) <- paste0(count_prop_variables, "_model_balanced")

# loop estimation - unbalanced


model_list_prop_unbalanced <- lapply(count_prop_variables, function(term){
  
  att_gt(yname = term,
         tname = "period",
         idname = "label_id",
         gname = "is_US",
         data = df_combined,
         allow_unbalanced_panel = TRUE,
         bstrap = T,
         cband = F,
         clustervars = "label_id")
  
}
)

names(model_list_prop_unbalanced) <- paste0(count_prop_variables, "_model_unbalanced")

# create plots - balanced

event_plots_prop_balanced <- lapply(seq_along(model_list_prop_balanced), function(model){
  
  ggdid(model_list_prop_balanced[[model]],
        title = count_prop_variables[model],
        ylab = "change in count")
  
})

#rename
names(event_plots_prop_balanced) <- paste0(count_prop_variables, "_plots_balanced")

#create grid - balanced plots
do.call(grid.arrange, c(event_plots_prop_balanced, ncol = 4, nrow = 4))


# create plots - ununbalanced

event_plots_prop_unbalanced <- lapply(seq_along(model_list_prop_unbalanced), function(model){
  
  ggdid(model_list_prop_unbalanced[[model]],
        title = count_prop_variables[model],
        ylab = "change in count")
  
})

#rename
names(event_plots_prop_unbalanced) <- paste0(count_prop_variables, "_plots_unbalanced")

#create grid - unbalanced plots
do.call(grid.arrange, c(event_plots_prop_unbalanced, ncol = 4, nrow = 4))

# summarise models - balanced

att_prop_balanced <- lapply(model_list_prop_balanced, function(model){
  
  aggte(model, type = 'simple')
  
})

# summarise models - unbalanced

att_prop_unbalanced <- lapply(model_list_prop_unbalanced, function(model){
  
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

info_prop_balanced <- do.call(rbind, extract_model_info(att_prop_balanced))
info_prop_unbalanced <- do.call(rbind, extract_model_info(att_prop_unbalanced))

# Add a column to differentiate between balanced and unbalanced
info_prop_balanced$type <- "Balanced"
info_prop_unbalanced$type <- "Unbalanced"

# Combine the data
atts_prop_combined_plotting <- rbind(info_prop_balanced, info_prop_unbalanced)


# plot the atts

dodge_width <- 0.25

ggplot(atts_prop_combined_plotting, aes(x = att, y = reorder(yname, att), color = type)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = dodge_width)) +
  coord_flip() +  # Flips the axes before adding other components
  geom_vline(xintercept = 0.00 , color = "red", linewidth = 0.5, linetype = "solid") +  # Add a horizontal line, increase size for visibility
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Variable", x = "Average Treatment Effect (ATT)", color = "Model Type")




