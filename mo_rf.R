library(terra)
library(raster)
# library(sfxlsx)
library(ggplot2)
library(stars)
library(tidyverse)
# library(xlsx)
library(e1071)
library(caret)
library(cowplot)
library(ggpmisc)


# scale_bands <- function(data) {
#     data <- data %>%
#       mutate(
#         blue = blue / 1000,
#         green = green / 1000,
#         red = red / 1000,
#         nir = nir / 1000
#       )
#   return(data)
# }

# SOC ANALYSIS ORDER
# COMPLETE MODELS FOR EACH GROUP + ALL
# RF GRID SEARCH
# PREDICTOR IMPORTANCE AND SELECTION ( recursive feature elimination (RFE) )
# ANALISIS DE SENSITIVIAAD 
# set de features, solo rotacion, solo suelos, solo           climaa
# PDP PARTIAL DEPEDECES PLOTS
# 

soc <- c("OCC")

all_features <- c( 
                   # "OCC",
                   #"GROUPS" 
                   # "mode_crop",
                    # "ELV", 
                   "potato_count", "barley_count", "tarwi_count", 
                   "oak_count", "fallow_count", "faba_count", "pasture_count",
                   
                   "pH", "EC",  "P", "K", "Sa", "Si", "Cy", "CEC", 
                   "eCa", "eMg", "eK", "eNa", "eAC", "Bd", "N15",             
                   
                   "mean.wc2.1_30s_prec_01", "mean.wc2.1_30s_prec_02", "mean.wc2.1_30s_prec_03",
                   "mean.wc2.1_30s_prec_04", "mean.wc2.1_30s_prec_05", "mean.wc2.1_30s_prec_06",
                   "mean.wc2.1_30s_prec_07", "mean.wc2.1_30s_prec_08", "mean.wc2.1_30s_prec_09",
                   "mean.wc2.1_30s_prec_10", "mean.wc2.1_30s_prec_11", "mean.wc2.1_30s_prec_12",
                   "mean.wc2.1_30s_srad_01", "mean.wc2.1_30s_srad_02", "mean.wc2.1_30s_srad_03",
                   "mean.wc2.1_30s_srad_04", "mean.wc2.1_30s_srad_05", "mean.wc2.1_30s_srad_06",
                   "mean.wc2.1_30s_srad_07", "mean.wc2.1_30s_srad_08", "mean.wc2.1_30s_srad_09",
                   "mean.wc2.1_30s_srad_10", "mean.wc2.1_30s_srad_11", "mean.wc2.1_30s_srad_12",
                   "mean.wc2.1_30s_tavg_01", "mean.wc2.1_30s_tavg_02", "mean.wc2.1_30s_tavg_03",
                   "mean.wc2.1_30s_tavg_04", "mean.wc2.1_30s_tavg_05", "mean.wc2.1_30s_tavg_06",
                   "mean.wc2.1_30s_tavg_07", "mean.wc2.1_30s_tavg_08", "mean.wc2.1_30s_tavg_09",
                   "mean.wc2.1_30s_tavg_10", "mean.wc2.1_30s_tavg_11", "mean.wc2.1_30s_tavg_12",
                   "mean.wc2.1_30s_tmax_01", "mean.wc2.1_30s_tmax_02", "mean.wc2.1_30s_tmax_03",
                   "mean.wc2.1_30s_tmax_04", "mean.wc2.1_30s_tmax_05", "mean.wc2.1_30s_tmax_06",
                   "mean.wc2.1_30s_tmax_07", "mean.wc2.1_30s_tmax_08", "mean.wc2.1_30s_tmax_09",
                   "mean.wc2.1_30s_tmax_10", "mean.wc2.1_30s_tmax_11", "mean.wc2.1_30s_tmax_12"
)


soil_features <- c(
                   # "OCC",
                   # "ELV", 
                   "pH", "EC",  "P", "K", "Sa", "Si", "Cy", "CEC", 
                   "eCa", "eMg", "eK", "eNa", "eAC", "Bd", "N15"
)

# elv <- c("ELV")

crop_features <- c(
                   # "OCC", #"GROUPS" 
                   # "mode_crop",
                   "potato_count", "barley_count", "tarwi_count", 
                   "oak_count", "fallow_count", "faba_count", "pasture_count"
)


clim_features <- c(
  # "OCC",
  "mean.wc2.1_30s_prec_01", "mean.wc2.1_30s_prec_02", "mean.wc2.1_30s_prec_03",
  "mean.wc2.1_30s_prec_04", "mean.wc2.1_30s_prec_05", "mean.wc2.1_30s_prec_06",
  "mean.wc2.1_30s_prec_07", "mean.wc2.1_30s_prec_08", "mean.wc2.1_30s_prec_09",
  "mean.wc2.1_30s_prec_10", "mean.wc2.1_30s_prec_11", "mean.wc2.1_30s_prec_12",
  "mean.wc2.1_30s_srad_01", "mean.wc2.1_30s_srad_02", "mean.wc2.1_30s_srad_03",
  "mean.wc2.1_30s_srad_04", "mean.wc2.1_30s_srad_05", "mean.wc2.1_30s_srad_06",
  "mean.wc2.1_30s_srad_07", "mean.wc2.1_30s_srad_08", "mean.wc2.1_30s_srad_09",
  "mean.wc2.1_30s_srad_10", "mean.wc2.1_30s_srad_11", "mean.wc2.1_30s_srad_12",
  "mean.wc2.1_30s_tavg_01", "mean.wc2.1_30s_tavg_02", "mean.wc2.1_30s_tavg_03",
  "mean.wc2.1_30s_tavg_04", "mean.wc2.1_30s_tavg_05", "mean.wc2.1_30s_tavg_06",
  "mean.wc2.1_30s_tavg_07", "mean.wc2.1_30s_tavg_08", "mean.wc2.1_30s_tavg_09",
  "mean.wc2.1_30s_tavg_10", "mean.wc2.1_30s_tavg_11", "mean.wc2.1_30s_tavg_12",
  "mean.wc2.1_30s_tmax_01", "mean.wc2.1_30s_tmax_02", "mean.wc2.1_30s_tmax_03",
  "mean.wc2.1_30s_tmax_04", "mean.wc2.1_30s_tmax_05", "mean.wc2.1_30s_tmax_06",
  "mean.wc2.1_30s_tmax_07", "mean.wc2.1_30s_tmax_08", "mean.wc2.1_30s_tmax_09",
  "mean.wc2.1_30s_tmax_10", "mean.wc2.1_30s_tmax_11", "mean.wc2.1_30s_tmax_12"
)




####
#### DATA
####


soc_features <- read.csv("./OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/soc_features.csv")
# soc_features <- soc_features %>% dplyr::select(-mode_crop)

soc_features <- soc_features %>% drop_na()


### GROUPS NO ESTA BALANCEADO
# MEJORAR CONSIDERANDO EL PERIODO DE ROTACION
# P.E. WWW 3 AÑOS
# PTP 3 AÑOS

soc_features %>% 
  group_by(GROUPS) %>% 
  dplyr::summarise(n = n())

# A tibble: 5 × 2
# GROUPS                       n
# <chr>                    <int>
#   1 Cereals                     77
# 2 Legume                      66
# 3 Pasture/Fallow Intensive    38
# 4 Potato Intensive             3
# 5 NA                          21



glimpse(soc_features)


# mo_extracted_composite_rotation <- read.csv('./OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rota_sysi_om_01.csv',
#                                             sep = ";" )  %>% 
#                                    drop_na() %>% 
#                                    select(-POL_ID, -CROP_SE, -ID, -STANDAR)
# 


###
### CASE ALL
###

soc_all_features <- soc_features %>%
  dplyr::select(soc, all_features)

###
### SIN ROT F
###

soc_no_crop_features <- soc_all_features %>%
  dplyr::select(-crop_features)

###
### SIN SOIL F
###


soc_no_soil_features <- soc_all_features %>%
  dplyr::select(-soil_features)


###
### SIN CLIM F
###


soc_no_clim_features <- soc_all_features %>%
  dplyr::select(-clim_features)


####
#### RF
####

# install.packages(c("sperrorest", "Metrics", "mccr", "caret", "ranger"))
# install.packages('randomForest')



library(sperrorest)
library(Metrics)
library(mccr)
library(caret)
library(ranger)
library(randomForest)



#########
######### CORRELACIONES
#########


#########
######### MODELS
#########


FORMULA <- OCC ~ .  # Adjust the formula as necessary

evaluate_model <- function(df) {
  
  # if ("GROUP" %in% colnames(df)) {
  #   df$GROUP <- as.factor(df$GROUP)
  # }
  
  nvars <- ncol(df) -1
  
  set.seed(123)  
  # trainIndex <- createDataPartition(df$GROUP, p = 0.7, list = FALSE)
  trainIndex <- sample(1:nrow(df), size = 0.7 * nrow(df)) # 30-70 % SPLIT
  train_data <- df[trainIndex, ]
  validation_data <- df[-trainIndex, ]
  
  mtry <- sqrt(nvars)
  model=randomForest::randomForest(FORMULA, data = train_data, mtry = mtry, ntree=1000) #ntree=500
  
  # predictions <- predict(model, validation_data)$predicted
  # accuracy <- mean(predictions == validation_data$clase)
  # return(list(model = model, accuracy = accuracy))
  
  
  predictions <- predict(model, validation_data)
  
  # ERROR CLASIFICATION
  
  # confusion_mat <- confusionMatrix(predictions, validation_data$OM)
  # accuracy <- confusion_mat$overall['Accuracy']
  # f1_score <- caret::F_meas(predictions, validation_data$OM)
  
  # ERROR REGRESION
  rmse <- sqrt(mean((predictions - validation_data$OCC)^2))  # Replace 'OM' with the actual name of your target variable
  
  
  ### R2
  
  ss_total <- sum((validation_data$OCC - mean(validation_data$OCC))^2) # Total sum of squares
  ss_residual <- sum((validation_data$OCC - predictions)^2)           # Residual sum of squares
  r_squared <- 1 - (ss_residual / ss_total)      
  
  
  
  results <- data.frame(
    Observed = validation_data$OCC,  # Actual values
    Predicted = predictions            # Predicted values
  )
  
  
  return(list(
    model = model,
    rmse = rmse,
    r_squared = r_squared,
    results = results
    # f1_score = f1_score
  ))
  
}



# evaluate_model <- function(df, seed = 123) {
#   set.seed(seed)  
#   nvars <- ncol(df) - 1
#   
#   trainIndex <- sample(1:nrow(df), size = 0.7 * nrow(df))
#   train_data <- df[trainIndex, ]
#   validation_data <- df[-trainIndex, ]
#   
#   mtry <- sqrt(nvars)
#   
#   set.seed(seed) 
#   model <- randomForest::randomForest(FORMULA, data = train_data, mtry = mtry, ntree = 1000)
#   
#   predictions <- predict(model, validation_data)
#   
#   rmse <- sqrt(mean((predictions - validation_data$OCC)^2))
#   ss_total <- sum((validation_data$OCC - mean(validation_data$OCC))^2)
#   ss_residual <- sum((validation_data$OCC - predictions)^2)
#   r_squared <- 1 - (ss_residual / ss_total)
#   
#   results <- data.frame(Observed = validation_data$OCC, Predicted = predictions)
#   
#   return(list(model = model, rmse = rmse, r_squared = r_squared, results = results))
# }



#### FITING  RF

rf_soc_all_features <- evaluate_model(soc_all_features)
rf_soc_no_crop_features <- evaluate_model(soc_no_crop_features)
rf_soc_no_soil_features <- evaluate_model(soc_no_soil_features)
rf_soc_no_clim_features <- evaluate_model(soc_no_clim_features)




saveRDS(rf_soc_all_features,
        file = './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_all_features.rds')
saveRDS(rf_soc_no_crop_features,
        file = './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_no_crop_features.rds')
saveRDS(rf_soc_no_soil_features,
        file = './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_no_soil_features.rds')
saveRDS(rf_soc_no_clim_features,
        file = './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_no_clim_features.rds')

stats_results <- data.frame(
  rmse = c(
    
    mean(rf_soc_all_features$rmse),      
    mean(rf_soc_no_crop_features$rmse),        
    mean(rf_soc_no_soil_features$rmse),      
    mean(rf_soc_no_clim_features$rmse)
    
  ),
  
  r2 = c(
    
    mean(rf_soc_all_features$r_squared),      
    mean(rf_soc_no_crop_features$r_squared),        
    mean(rf_soc_no_soil_features$r_squared),      
    mean(rf_soc_no_clim_features$r_squared)
    
  ),
  
  
  input_data = c(
    'All features', 
    'Without rotation features', 
    'Without soil features',
    'Without Climatic features'
  )
)


write.csv(stats_results,
          './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/stats_results.csv')



combined_results <- rbind(
  rf_soc_all_features$results %>% mutate(data_group = 'All features'), 
  rf_soc_no_crop_features$results %>% mutate(data_group = 'Without rotation features') ,
  rf_soc_no_soil_features$results%>% mutate(data_group = 'Without soil features') , 
  rf_soc_no_clim_features$results%>% mutate(data_group = 'Without Climatic features')
                          )

write.csv(combined_results,
          './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_cv_reuslts.csv')


# combined_results <- read.csv( './OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_cv_reuslts.csv')


####
#### KNN
####



stats_results <- read.csv('./OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/stats_results.csv')
combined_results <- read.csv('./OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/rf_soc_cv_reuslts.csv')




#########
######### PRED VS OBS SCATTER PLOTS
#########


merged_data <- combined_results %>%
  rename(input_data = data_group) %>%
  left_join(stats_results, by = "input_data")


facet_labels <- c(
  "All features" = "Case 1:\nAll features",
  "Without Climatic features" = "Case 2:\nWithout Climatic features",
  "Without rotation features" = "Case 3:\nWithout rotation features",
  "Without soil features" = "Case 4:\nWithout soil features"
)


# plot <- ggplot(merged_data, aes(x = Observed, y = Predicted)) +
#   geom_point(color = "blue", fill = "blue", size = 3, shape = 21)+ # Blue points
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black", size = 1) + # 1:1 line
#   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.5) + # Regression line
#   stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")),
#                formula = y ~ x, parse = TRUE, color = "black", size = 4) + # Equation and R²
#   
#   geom_text(
#     aes(label = paste0("RMSE: ", round(rmse, 2))),
#     data = stats_results, 
#     x = -Inf, y = Inf, hjust = -0.5, vjust = 6, inherit.aes = FALSE, size = 4, color = "black"
#   )  + 
# 
#   
#   facet_wrap(~ input_data) + # Facets for each input_data
#   scale_x_continuous(limits = c(0, 20)) +
#   scale_y_continuous(limits = c(0, 20)) +
#   coord_equal() +
#   theme_bw() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_line(colour = "gray", size = 0.5),
#     panel.border = element_rect(colour = "black", fill = NA),
#     strip.background = element_blank(),
#     strip.text = element_text(size = 15),
#     plot.title = element_text(hjust = 0.5, size = 20),
#     axis.text = element_text(size = 12.5),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title = element_text(size = 15),
#     legend.text = element_text(size = 15),
#     legend.title = element_text(size = 15)
#   ) +
#   labs(
#     title = " ",
#     x = expression(Observed ~ SOC ~ (g ~ kg^{-1})), 
#     y = expression(Predicted ~ SOC ~ (g ~ kg^{-1})) 
#   )
# 
# 
# ggsave('./OUTPUT_CIP_ROT_01_09_2024/PLOTS_21_11/SOC_PRED_SCATTER_1000TREES.svg', #svg
#        plot = plot,  width = 12.5, height = 12.5, units = "in", dpi = 1000)



plot <- ggplot(merged_data, aes(x = Observed, y = Predicted)) +
  geom_point(color = "blue", fill = "blue", size = 3, shape = 21)+ # Blue points
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black", size = 1) + # 1:1 line
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.5) + # Regression line
  
  # stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")),
  #              formula = y ~ x, parse = TRUE, color = "black", size = 2.5) + # Equation and R²
  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    color = "black",
    size = 2.5
  ) +
  
  geom_text(
    aes(label = paste(
      "RMSE:", round(rmse, 2), "~g~kg^{-1}", "\n"
      # "R^2:", round(r2, 2)
    )),
    data = stats_results,
    x = -Inf, y = Inf, hjust = -0.25, vjust = 5,
    inherit.aes = FALSE, size = 2.5, color = "black",
    parse = TRUE
  ) + 
  
  
  facet_wrap(~ input_data,labeller = as_labeller(facet_labels)) + # Facets for each input_data
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20)) +
  coord_equal() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", size = 0.5),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12.5),
    axis.text = element_text(size = 12.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = " ",
    x = expression(Observed ~ SOC ~ (g ~ kg^{-1})), 
    y = expression(Predicted ~ SOC ~ (g ~ kg^{-1})) 
  )


ggsave('./OUTPUT_CIP_ROT_01_09_2024/PLOTS_21_11/SOC_PRED_SCATTER_1000TREES.svg', #svg
       plot = plot,  width = 12.5, height = 12.5, units = "in", dpi = 1000)





###########
########### FEATURE IMPORTANCE (RECURSIVE FEATURE ELIMINATION)
###########

# https://rdrr.io/github/talegari/forager/man/forest_rfe.html


source('./OUTPUT_CIP_ROT_01_09_2024/MO_MODELING/forest_rfe.R')


importance_soc_all_features<- forest_rfe(
                soc_all_features, 
                responseVarName = 'OCC', 
                # sizes = c(10,20),
                # sampleprop = 0.2,
                # nsamples = 184, 
                seed = 123
                )

importance_soc_all_features <- importance_soc_all_features$oobchangeTable

importance_soc_no_crop_features <- forest_rfe(
  soc_no_crop_features, 
  responseVarName = 'OCC', 
  # sizes = c(10,20),
  # sampleprop = 0.2,
  # nsamples = 184, 
  seed = 123
)
importance_soc_no_crop_features <- importance_soc_no_crop_features$oobchangeTable


importance_soc_no_soil_features <- forest_rfe(
  soc_no_soil_features, 
  responseVarName = 'OCC', 
  # sizes = c(10,20),
  # sampleprop = 0.2,
  # nsamples = 184, 
  seed = 123
)
importance_soc_no_soil_features <- importance_soc_no_soil_features$oobchangeTable


importance_soc_no_clim_features <- forest_rfe(
  soc_no_clim_features, 
  responseVarName = 'OCC', 
  # sizes = c(10,20),
  # sampleprop = 0.2,
  # nsamples = 184, 
  seed = 123
)
importance_soc_no_clim_features <- importance_soc_no_clim_features$oobchangeTable



importance_soc_all_features$Group <- 'All features'
importance_soc_no_crop_features$Group <- 'Without rotation features'
importance_soc_no_soil_features$Group <- 'Without soil features'
importance_soc_no_clim_features$Group <- 'Without climatic features'


merged_importance <- rbind(
  importance_soc_all_features,
  importance_soc_no_crop_features,
  importance_soc_no_soil_features,
  importance_soc_no_clim_features
)



var_type_mapping <- data.frame(
  variable = c(soil_features, crop_features, clim_features),
  var_type = c(
    rep("Soil", length(soil_features)),
    rep("Rotation", length(crop_features)),
    rep("Climate", length(clim_features))
  )
)

merged_importance <- merged_importance %>%
  left_join(var_type_mapping, by = "variable")

merged_importance <- merged_importance %>%
  mutate(variable = recode(variable,
                           "CEC" = "CEC",
                           "Cy" = "Cy",
                           "eAC" = "eAC",
                           "EC" = "EC",
                           "eCa" = "eCa",
                           "eK" = "eK",
                           "eMg" = "eMg",
                           "eNa" = "eNa",
                           "faba_count" = "Beans frequency",
                           "fallow_count" = "Fallow frequency",
                           "K" = "K",
                           "mean.wc2.1_30s_prec_01" = "Precipitation mean Jan",
                           "mean.wc2.1_30s_prec_02" = "Precipitation mean Feb",
                           "mean.wc2.1_30s_prec_03" = "Precipitation mean Mar",
                           "mean.wc2.1_30s_prec_04" = "Precipitation mean Apr",
                           "mean.wc2.1_30s_prec_05" = "Precipitation mean May",
                           "mean.wc2.1_30s_prec_06" = "Precipitation mean Jun",
                           "mean.wc2.1_30s_prec_07" = "Precipitation mean Jul",
                           "mean.wc2.1_30s_prec_08" = "Precipitation mean Aug",
                           "mean.wc2.1_30s_prec_09" = "Precipitation mean Sep",
                           "mean.wc2.1_30s_prec_10" = "Precipitation mean Oct",
                           "mean.wc2.1_30s_prec_11" = "Precipitation mean Nov",
                           "mean.wc2.1_30s_prec_12" = "Precipitation mean Dec",
                           "mean.wc2.1_30s_srad_01" = "Solar radiation mean Jan",
                           "mean.wc2.1_30s_srad_02" = "Solar radiation mean Feb",
                           "mean.wc2.1_30s_srad_03" = "Solar radiation mean Mar",
                           "mean.wc2.1_30s_srad_04" = "Solar radiation mean Apr",
                           "mean.wc2.1_30s_srad_05" = "Solar radiation mean May",
                           "mean.wc2.1_30s_srad_06" = "Solar radiation mean Jun",
                           "mean.wc2.1_30s_srad_07" = "Solar radiation mean Jul",
                           "mean.wc2.1_30s_srad_08" = "Solar radiation mean Aug",
                           "mean.wc2.1_30s_srad_09" = "Solar radiation mean Sep",
                           "mean.wc2.1_30s_srad_10" = "Solar radiation mean Oct",
                           "mean.wc2.1_30s_srad_11" = "Solar radiation mean Nov",
                           "mean.wc2.1_30s_srad_12" = "Solar radiation mean Dec",
                           "mean.wc2.1_30s_tavg_01" = "Temperature mean Jan",
                           "mean.wc2.1_30s_tavg_02" = "Temperature mean Feb",
                           "mean.wc2.1_30s_tavg_03" = "Temperature mean Mar",
                           "mean.wc2.1_30s_tavg_04" = "Temperature mean Apr",
                           "mean.wc2.1_30s_tavg_05" = "Temperature mean May",
                           "mean.wc2.1_30s_tavg_06" = "Temperature mean Jun",
                           "mean.wc2.1_30s_tavg_07" = "Temperature mean Jul",
                           "mean.wc2.1_30s_tavg_08" = "Temperature mean Aug",
                           "mean.wc2.1_30s_tavg_09" = "Temperature mean Sep",
                           "mean.wc2.1_30s_tavg_10" = "Temperature mean Oct",
                           "mean.wc2.1_30s_tavg_11" = "Temperature mean Nov",
                           "mean.wc2.1_30s_tavg_12" = "Temperature mean Dec",
                           "mean.wc2.1_30s_tmax_01" = "Max temperature mean Jan",
                           "mean.wc2.1_30s_tmax_02" = "Max temperature mean Feb",
                           "mean.wc2.1_30s_tmax_03" = "Max temperature mean Mar",
                           "mean.wc2.1_30s_tmax_04" = "Max temperature mean Apr",
                           "mean.wc2.1_30s_tmax_05" = "Max temperature mean May",
                           "mean.wc2.1_30s_tmax_06" = "Max temperature mean Jun",
                           "mean.wc2.1_30s_tmax_07" = "Max temperature mean Jul",
                           "mean.wc2.1_30s_tmax_08" = "Max temperature mean Aug",
                           "mean.wc2.1_30s_tmax_09" = "Max temperature mean Sep",
                           "mean.wc2.1_30s_tmax_10" = "Max temperature mean Oct",
                           "mean.wc2.1_30s_tmax_11" = "Max temperature mean Nov",
                           "mean.wc2.1_30s_tmax_12" = "Max temperature mean Dec",
                           "N15" = "N15",
                           "oak_count" = "Oats frequency",
                           "P" = "P",
                           "pasture_count" = "Pasture frequency",
                           "pH" = "pH",
                           "potato_count" = "Potato frequency",
                           "Sa" = "Sa",
                           "Si" = "Si",
                           "tarwi_count" = "Tarwi frequency"
  ))




#####
##### PLOT IMPORTANCE BARS
#####

var_type_colors <- c(
  "Soil" = "#FF8C00",  # Orange
  "Rotation" = "#6A5ACD",  # Purple
  "Climate" = "#2E8B57"   # Marine green
)



split_data <- split(merged_importance, merged_importance$Group)

# Create individual plots for each group
plots <- lapply(names(split_data), function(group) {
  ggplot(split_data[[group]], aes(x = importance, y = reorder(variable, importance), fill = var_type)) +
    geom_col() +
    scale_fill_manual(values = var_type_colors)  +
    labs(
      # title = paste("Variable Importance:", group),
      title = group,
      x = "Importance",
      y = NULL 
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(colour = "gray", size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 15),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.position = "bottom"
    )
})

# Combine all plots into a single layout using cowplot
final_plot <- plot_grid(plotlist = plots, ncol = 4) # Adjust `ncol` as needed

# Save the combined plot (optional)
ggsave("./OUTPUT_CIP_ROT_01_09_2024/PLOTS_21_11/combined_importance_plotv2.svg", 
       final_plot, width = 20, height = 10)

#####
##### PARTIAL DEPENDECE PLOTS
#####
## FIG VERGOPOLN
  # install.packages("pdp")
# https://cran.r-project.org/web/packages/pdp/index.html
# https://rpubs.com/vishal1310/QuickIntroductiontoPartialDependencePlots
# https://journal.r-project.org/archive/2017/RJ-2017-016/index.html
# https://journal.r-project.org/archive/2017/RJ-2017-016/index.html



# 
# data(iris)
# colnames(iris)
#   
# x <- subset(iris, select=-Species)
# y <- iris$Species
# # Building model
# model.svm <- svm(Species~., data = iris, probability = TRUE)
# # Predicting
# pred <- predict(model.svm, iris, probability = TRUE)
# # Confusion Matrix
# table(y, pred)
# 
# 
# library(pdp)
# 
# 
# # Single Variable
# par.Petal_W <- partial(model.svm, pred.var = c("Petal.Width"), chull = TRUE)
# plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)
# 
# # Single Variable
# par.Sepal_W  <- partial(model.svm, pred.var = c("Sepal.Width"), chull = TRUE)
# plot.Sepal_W  <- autoplot(par.Sepal_W , contour = TRUE)
# 
# # Single Variable
# par.Petal_L <- partial(model.svm, pred.var = c("Petal.Length"), chull = TRUE)
# plot.Petal_L <- autoplot(par.Petal_L, contour = TRUE)
# 
# # Single Variable
# par.Sepal_L  <- partial(model.svm, pred.var = c("Sepal.Length"), chull = TRUE)
# plot.Sepal_L  <- autoplot(par.Sepal_L , contour = TRUE)


# par.Petal_W.Sepal_W <- partial(model.svm, pred.var = c("Petal.Width", "Sepal.Width"), chull = TRUE)
# plot.Petal_W.Sepal_W <- autoplot(par.Petal_W.Sepal_W, contour = TRUE, 
#                                  legend.title = "Partial\ndependence")
# grid.arrange(plot.Petal_W, plot.Sepal_W, plot.Petal_L, plot.Sepal_L, plot.Petal_W.Sepal_W)



####
#### PARTIAL PLOTS
####


####
#### RANGER
####


fit_rf <- function(df){
  nvars <- ncol(df) -1
  mtry <- sqrt(nvars)
  FORMULA <- OCC ~ .  
  model=randomForest::randomForest(FORMULA, 
                                   data = df, 
                                   mtry = mtry, 
                                   ntree=500) 
  return(model)
  
} 


fr_model_soc_all_features <- fit_rf(soc_all_features)
fr_model_soc_no_crop_features <- fit_rf(soc_no_crop_features)
fr_model_soc_no_soil_features <- fit_rf(soc_no_soil_features)
fr_model_soc_no_clim_features <- fit_rf(soc_no_clim_features)


# cy,  CEC

library(pdp)

par_fr_model_soc_all_features_cy <- partial(fr_model_soc_all_features, 
                                         train = soc_all_features,
                                         pred.var = c("Cy"), 
                                         chull = TRUE)

par_fr_model_soc_all_features_CEC <- partial(fr_model_soc_all_features, 
                                            train = soc_all_features,
                                            pred.var = c("CEC"), 
                                            chull = TRUE)


# cy, EAC, 


par_fr_model_soc_no_crop_features_cy <- partial(fr_model_soc_no_crop_features, 
                                            train = soc_no_crop_features,
                                            pred.var = c("Cy"), 
                                            chull = TRUE)
par_fr_model_soc_no_crop_features_eCa <- partial(fr_model_soc_no_crop_features, 
                                             train = soc_no_crop_features,
                                             pred.var = c("eCa"), 
                                             chull = TRUE)


#frequency of fallow and mean precipitation in January respect 


par_fr_model_soc_no_soil_features_fallow_count <- partial(fr_model_soc_no_soil_features, 
                                                train = soc_no_soil_features,
                                                pred.var = c("fallow_count"), 
                                                chull = TRUE)
par_fr_model_soc_no_soil_features_mean.wc2.1_30s_prec_01 <- partial(fr_model_soc_no_soil_features, 
                                                 train = soc_no_soil_features,
                                                 pred.var = c("mean.wc2.1_30s_prec_01"), 
                                                 chull = TRUE)



#####
##### PLOT
#####

# create_custom_plot <- function(partial_plot) {
#   autoplot(partial_plot) +  
#     theme_bw() +
#     theme(
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_line(colour = "gray", size = 0.5),
#       panel.border = element_rect(colour = "black", fill = NA),
#       plot.title = element_text(hjust = 0.5, size = 20),
#       axis.text = element_text(size = 20),
#       axis.title = element_text(size = 20),
#       legend.text = element_text(size = 20),
#       legend.title = element_text(size = 20),
#       legend.position = "bottom"
#     )
# }

create_custom_plot <- function(partial_plot, x_label, y_label) {
  autoplot(partial_plot) +  
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(colour = "gray", size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 12.5),
      axis.text = element_text(size = 12.5),
      axis.title = element_text(size = 12.5),
      legend.text = element_text(size = 12.5),
      legend.title = element_text(size = 12.5),
      legend.position = "bottom"
    ) +
    labs(x = x_label, y = y_label)  # Set x and y labels
}





plot_cy <- create_custom_plot(par_fr_model_soc_all_features_cy,"Clay content", "SOC")
plot_cec <- create_custom_plot(par_fr_model_soc_all_features_CEC,"CEC", "SOC")
plot_no_crop_cy <- create_custom_plot(par_fr_model_soc_no_crop_features_cy,"Clay content", "SOC")
plot_no_crop_eCa <- create_custom_plot(par_fr_model_soc_no_crop_features_eCa,"Exchangeable Calcium ", "SOC")
plot_fallow_count <- create_custom_plot(par_fr_model_soc_no_soil_features_fallow_count,"Fallow frecuency", "SOC")
plot_mean_prec <- create_custom_plot(par_fr_model_soc_no_soil_features_mean.wc2.1_30s_prec_01,"Mean Precipitation Jan", "SOC")


combined_plot <- plot_grid(
  plot_cy, plot_cec,
  plot_no_crop_cy, plot_no_crop_eCa,
  plot_fallow_count, plot_mean_prec,
  ncol = 2, nrow = 3,
  labels = c("A", " ", "B",' ', 'C'), # Specify one label per row
  label_size = 15 # Adjust label size if needed
)



ggsave("./OUTPUT_CIP_ROT_01_09_2024/PLOTS_21_11/PDP_v2.svg", 
       combined_plot, width = 15, height = 15)



  
## FIG VERGOPOLN
#https://stackoverflow.com/questions/37279964/variable-importance-with-ranger


importance(mod)


#####
##### VARIATION PARTITIONING 
#####


# library(vegan)
# library(plyr)
# library(funrar) 
# library(ggplot2)
# library(ggforce)
# library(ggpubr)






