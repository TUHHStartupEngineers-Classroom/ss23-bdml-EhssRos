# # Load data
# library(tidyverse)
# library(readxl)
# 
# employee_attrition_tbl <- read_csv("content/01_journal/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.txt")
# definitions_raw_tbl    <- read_excel("content/01_journal/data_definitions.xlsx", sheet = 1, col_names = FALSE)
# View(definitions_raw_tbl)
# 
# employee_attrition_tbl %>% 
#   ggplot(aes(Education)) +
#   geom_bar()
# 
# # Data preparation ----
# # Human readable
# 
# definitions_tbl <- definitions_raw_tbl %>% 
#   fill(...1, .direction = "down") %>%
#   filter(!is.na(...2)) %>%
#   separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
#   rename(column_name = ...1) %>%
#   mutate(key = as.numeric(key)) %>%
#   mutate(value = value %>% str_replace(pattern = "'", replacement = "")) 
# definitions_tbl
# 
# # DATA PREPARATION ----
# # Human readable ----
# 
# definitions_list <- definitions_tbl %>% 
#   
#   # Mapping over lists
#   
#   # Split into multiple tibbles
#   split(.$column_name) %>%
#   # Remove column_name
#   map(~ select(., -column_name)) %>%
#   # Convert to factors because they are ordered an we want to maintain that order
#   map(~ mutate(., value = as_factor(value))) 
# 
# # definitions_list[[1]]
# definitions_list[["Education"]]
# ## # A tibble: 5 x 2
# ##     key value        
# ##   <dbl> <fct>        
# ## 1     1 Below College
# ## 2     2 College      
# ## 3     3 Bachelor     
# ## 4     4 Master       
# ## 5     5 Doctor  
# 
# # Rename columns
# for (i in seq_along(definitions_list)) {
#   list_name <- names(definitions_list)[i]
#   colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
# }
# 
# definitions_list[["Education"]]
# ## # A tibble: 5 x 2
# ##   Education Education_value
# ##       <dbl> <fct>          
# ## 1         1 Below College  
# ## 2         2 College        
# ## 3         3 Bachelor       
# ## 4         4 Master         
# ## 5         5 Doctor     
# 
# data_merged_tbl <- list(HR_Data = employee_attrition_tbl) %>%
#   
#   # Join everything
#   append(definitions_list, after = 1) %>%
#   reduce(left_join) %>%
#   
#   # Remove unnecessary columns
#   select(-one_of(names(definitions_list))) %>%
#   
#   # Format the "_value"
#   set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
#   
#   # Resort
#   select(sort(names(.))) 
# 
# # Return only unique values of BusinessTravel
# data_merged_tbl %>% 
#   distinct(BusinessTravel)
# 
# data_merged_tbl %>%
#   mutate_if(is.character, as.factor) %>%
#   glimpse()
# 
# data_merged_tbl %>%
#   mutate_if(is.character, as.factor) %>%
#   select_if(is.factor) %>%
#   glimpse()
# 
# data_merged_tbl %>%
#   mutate_if(is.character, as.factor) %>%
#   select_if(is.factor) %>%
#   map(levels)
# ## $Attrition
# ##[1] "No"  "Yes"
# ## 
# ## $BusinessTravel
# ## [1] "Non-Travel"        "Travel_Frequently" "Travel_Rarely"    
# ## ...
# 
# data_processed_tbl <- data_merged_tbl %>%        
#   mutate_if(is.character, as.factor) %>%
#   mutate(
#     BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
#                                                     "Travel_Rarely", 
#                                                     "Travel_Frequently"),
#     MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
#                                                    "Married", 
#                                                    "Divorced")
#   )
# 
# data_processed_tbl %>% 
#   select_if(is.factor) %>% 
#   map(levels)

process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}
# process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl) %>% 
#   glimpse()