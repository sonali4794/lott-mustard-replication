state_crime = read.csv("D:/UT/Causal Inference/Lott n Mustard/UpdatedStateLevelData-2010.csv")

state_crime = state_crime %>% 
  filter(year >= 1977 & year <= 1992) %>% 
  group_by(state) %>% 
  mutate(treatment = max(shalll))

maxyear =  max(state_crime$year)

treatment_table = state_crime %>% 
  group_by(state) %>% 
  summarize(count = sum(shalll)) %>% 
  mutate(treatment_year = ifelse(count > 0, maxyear + 1 - count, 0))

state_treatment = merge(treatment_table, state_crime,  by = "state")


outcomes = c("lvio", "lpro", "lmur", "lrap", "laga", "lrob", "lbur", "llar", "laut")
acontrols = c("aovio", "aopro", "aomur", "aorap", "aoaga", "aorob", "aobur", "aolar", "aoaut")
Y = length(outcomes)

foreach(y = 1:Y) %do% {
  yval = outcomes[y]
  acon = acontrols[y]
  fixed = "| state + year"
  
  twfe_spec = as.formula(paste0(yval, " ~ shalll + density + 
                               rpcpi + rpcim + rpcui + rpcrpo + 
                               ppwm1019 + ppwm2029 + ppwm3039 + 
                               ppwm4049 + ppwm5064 + ppwf1019 + 
                               ppwf2029 + ppwf3039 + ppwf4049 + 
                               ppwf5064 + ppbm1019 + ppbm2029 + 
                               ppbm3039 + ppbm4049 + ppbm5064 + 
                               ppbf1019 + ppbf2029 + ppbf3039 + 
                               ppbf4049 + ppbf5064 +ppnm1019 + 
                               ppnm2029 + ppnm3039 + ppnm4049 + 
                               ppnm5064 + ppnf1019 + ppnf1019 + 
                               ppnf2029 + ppnf3039 + ppnf4049 + 
                               ppnf5064 + ", acon, fixed))
  
  twfe_model =  feols(fml = twfe_spec, 
                       data = state_treatment)

  mod_name = paste("twfe", yval, sep = "_")
  assign(mod_name, twfe_model)
  
  fname_mod = paste0(mod_name, ".RDs")
  save(file = file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_mod), list = "twfe_model")
}


# bacon decomposition
foreach(y = 1:Y) %do% {
  yval = outcomes[y]
  
  bacon_formula = as.formula(paste(yval, "shalll", sep = " ~ "))
  
  bacon_decomp = bacon(bacon_formula, 
                       data = state_treatment, 
                       id_var = "fipsstat", 
                       time_var = "year") %>%
    group_by(type) %>% 
    summarise(avg_est = weighted.mean(estimate, weight), 
              weight = sum(weight))
  

  bacon_name = paste("bacon", yval, sep = "_")
  assign(bacon_name, bacon_decomp)
  
  fname_mod = paste0(bacon_name, ".RDs")
  save(file = file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_mod), list = "bacon_decomp")
}

# calloway santana
foreach(y = 1:Y) %do% {
  yval = outcomes[y]
  x = acontrols[y]
  
  rhs = as.formula(paste('~ + ', x))
  atts = att_gt(yname = yval, # LHS variable
                 tname = 'year', # panel time variable
                 idname = 'fipsstat', # firms' panel id variable
                 gname = 'treatment_year', 
                 data = state_treatment, # data
                 xformla = rhs,
                 est_method = "dr",
                 control_group = "notyettreated",
                 bstrap = TRUE, # if TRUE compute bootstrapped SE
                 biters = 1000, # number of bootstrap iterations
                 print_details = FALSE, # if TRUE, print detailed results
                 clustervars = 'fipsstat', # cluster level
                 panel = TRUE)
  
  cs_model = aggte(atts, type = "group", balance_e = TRUE, na.rm = TRUE)
  
  cs_name = paste("CS", yval, sep = "_")
  assign(cs_name, cs_model)
  
  fname_mod = paste0(cs_name, ".RDs")
  save(file = file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_mod), list = "cs_model")
}

# sun and abraham
foreach(y = 1:Y) %do% {
  yval = outcomes[y]
  acon = acontrols[y]
  
  twfe_spec = as.formula(paste0(yval, "~ sunab(treatment_year, year) + density + 
                               rpcpi + rpcim + rpcui + rpcrpo + 
                               ppwm1019 + ppwm2029 + ppwm3039 + 
                               ppwm4049 + ppwm5064 + ppwf1019 + 
                               ppwf2029 + ppwf3039 + ppwf4049 + 
                               ppwf5064 + ppbm1019 + ppbm2029 + 
                               ppbm3039 + ppbm4049 + ppbm5064 + 
                               ppbf1019 + ppbf2029 + ppbf3039 + 
                               ppbf4049 + ppbf5064 +ppnm1019 + 
                               ppnm2029 + ppnm3039 + ppnm4049 + 
                               ppnm5064 + ppnf1019 + ppnf1019 + 
                               ppnf2029 + ppnf3039 + ppnf4049 + 
                               ppnf5064 + ", acon))
  SA_mod = feols(fml = twfe_spec, 
                  data = state_treatment,
                  subset = ~ year < 1992,
                  vcov = ~ fipsstat + year)
  
  sa_name = paste("SA", yval, sep = "_")
  assign(sa_name, SA_mod)
  
  fname_mod = paste0(sa_name, ".RDs")
  save(file = file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_mod), list = "SA_mod")
}