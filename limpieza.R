# Libraries
library(dplyr)
library(ggplot2)
library(RCT)
library(purrr)
library(tidyr)
library(fastDummies)

# Directorio
setwd("/Users/adrian_martinez/Dropbox/Maestría/Maestría Clases/Segundo_Semestre/Proyecto_Final_Eco_Compu/Progresa-Targeting")

# Read data
data <- readRDS("./base")

###############################################################################
# Análisis de variables "missing"
missings <- map_dbl(data %>% select_all(), 
                    ~100*sum(is.na(.)/nrow(data)))

(missings <- missings[missings>0])
missings <- data.frame("pct_miss" = missings, 
                          "variable" = names(missings)) 
missings <- missings %>% 
  arrange(variable)

# Quitar variables que tienen más de 99 porciento de missing values
# remove <- missings %>% 
#   filter(pct_miss >= 99) %>% 
#   pull(variable)

# data <- data %>% 
#   select(-{{remove}})

missings <- missings %>% 
  filter(! variable %in% remove)

aux <- data %>% 
  summarise_at(missings$variable, list(min = min), na.rm = TRUE) %>% 
  pivot_longer(everything(), names_to = "variables", values_to = "valores")
  
names <- colnames(data)[grepl("acc_alim", colnames(data))]

data <- data %>% 
  mutate(across(names, ~ replace_na(., 0)))

data <- data %>% 
  select(-starts_with("anio"))

data <- data %>% 
  rowwise() %>%
  mutate(habito = sum(c_across(starts_with("habito")), na.rm = TRUE)) %>% 
  select(-starts_with("habito_")) %>% 
  ungroup()

data <- data %>% 
  rowwise() %>%
  mutate(segvol = sum(c_across(starts_with("segvol")), na.rm = TRUE)) %>% 
  select(-starts_with("segvol_")) %>% 
  ungroup()

data <- data %>% 
  rowwise() %>%
  mutate(segvol = sum(c_across(starts_with("inscr")), na.rm = TRUE)) %>% 
  select(-starts_with("inscr_")) %>% 
  ungroup()
  
data <- data %>% 
  drop_na(alfabetism)

data <- data %>% 
  select(-contains("licon"), -contains("dicon"))

data <- data %>% 
  mutate(act_pnea2 = replace_na(act_pnea2, 0), 
         tipoact2 = replace_na(tipoact2, 0))

data <- data %>% 
  select(-pagoaten_5, -num_dueno1, -num_due2, -hog_dueno1, -hog_dueno2, -min_7,
         -tipoact2, -lugar, -noatenc_15, -noatenc_16, -starts_with("causa"))

data <- data %>% 
  mutate(across(starts_with("num_"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(across(starts_with("min_"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(across(starts_with("bano_"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(disc1 = if_else(disc1 == "&" | disc1 == "8", 0, 1))

data <- data %>% 
  mutate(across(starts_with("disc"), ~ as.numeric(.))) %>% 
  mutate(across(starts_with("disc"), ~ replace_na(., 0))) %>% 
  mutate(across(starts_with("disc"), ~ ifelse(. != 0, 1, 0)))

data <- data %>% 
  mutate(disc_tot = disc1 + disc2 + disc3 + disc4 + disc5 + disc6 + disc7) %>% 
  select(-starts_with("disc"), disc_tot)

data <- data %>% 
  select(-starts_with("usotiempo"))

data <- data %>% 
  select(-starts_with("causa"), -starts_with("er"))

data <- data %>% 
  mutate(across(starts_with("medtrab"), ~ replace_na(., 0))) %>% 
  mutate(across(starts_with("medtrab"), ~ if_else(. != 0, 1, 0)))
 
data <- data %>% 
  mutate(across(starts_with("hijo"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(act_pnea1 = replace_na(act_pnea1, 0))

data <- data %>% 
  select(-conf_pers)

data <- data %>% 
  mutate(across(starts_with("forma"), ~ replace_na(., 0)))

data <- data %>% 
  mutate(diabetes = case_when(diabetes == 1 ~ 1, diabetes == 2 ~ 0,
         TRUE ~ 3), pres_alta = case_when(pres_alta == 1 ~ 1, 
                                          pres_alta == 2 ~ 0 ,
                                          TRUE ~ 3), 
         peso = case_when(peso == 1 ~ 1, peso == 2 ~ 0, 
                          TRUE ~ 3))

data <- data %>% 
  mutate(across(starts_with("hor_"), ~ replace_na(., 0)))

data <- data %>% 
  select(-starts_with("hog_dueno"))

data <- data %>% 
  mutate(across(starts_with("inst_"), ~ replace_na(., 0)))

data_aux <- map_df(colnames(data)[grepl("noatenc_", colnames(data))], function(x){
  data %>% 
    mutate({{x}} = if_else(is.na(prob_sal), replace_na({{x}}, "No Aplica"),
                           if_else(is.na({{x}}), 0, {{x}})))
}

data <- data %>% 
  mutate(across(starts_with("noatenc_"), ~ if_else(is.na(prob_sal), replace_na(., "No Aplica"),
                                                   if_else(is.na(.), 0))))

data <- data %>% 
  mutate(across(starts_with("norecib_"), ~ replace_na(., "No Aplica")))

data <- data %>% 
  mutate(across(starts_with("pagoaten_"), ~ replace_na(., "No Aplica")))

data <- data %>% 
  mutate(across(starts_with("redsoc_"), ~ replace_na(., 0)))

# Variables importantes que debemos escoger qué hacer con los missing values
# est_trans, cambiar por dummies las de habito_1,2,3,4,5,6
# lo ismo que habito para er_... 
# Las variables que tienen "num" como num_refri tienen NA en los ceros

# Quitar variables de...
# dicon, licon 

###############################################################################

# Dividir la base de datos en entrenamiento y valdiación
data_validacion <- treatment_assign(data = data, share_control = 0.8,
                                    n_t = 1, seed = 1996, key = "foliohog",
                                    strata_varlist = "foliohog")
data_validacion <- data_validacion$data %>% 
  ungroup() %>% 
  select(treat)

data$tratamiento <- data_validacion$treat

# Creamos la base de entrenamiento
data_entrenamiento <- data %>% 
  filter(tratamiento == 0)

# Creamos la base de validación 
data_validacion <- data %>% 
  filter(tratamiento == 1)




