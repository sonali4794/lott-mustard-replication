state_crime = read_dta("D:/UT/Causal Inference/Lott n Mustard/UpdatedStateLevelData-2010.csv")

state_crime = state_crime %>% 
  filter(year>=1977 & year<=1992)

state_crime = state_crime %>%
  select(shalll, everything())

state_crime = state_crime %>% 
  group_by(state) %>% 
  mutate(treat = ifelse(min(shalll) ==  1, 1, (ifelse(min(shalll) == max(shalll), 0, 1))))

state_crime = state_crime %>%
  select(treat, everything())

treat_table = state_crime %>% 
  group_by(state) %>% 
  summarize(cnt = sum(shalll)) %>% 
  na.omit()

state_crime = left_join(state_crime, treat_table, by = c("state" = "state")) 

state_crime = state_crime %>%
  select(cnt, everything())

state_crime = state_crime %>% 
  mutate(treat_year = ifelse(cnt <= 16 & cnt > 0, 1992 - (cnt - 1), 0)) 

state_crime = state_crime %>% 
  mutate(time_to_treat = ifelse(treat_year == 0, 0,year - treat_year))

state_crime = state_crime %>%
  select(time_to_treat, everything())

state_crime = state_crime %>%
  select(year, everything()) %>% 
  select(aftr, everything()) %>% 
  select(shalll, everything()) %>% 
  select(treat, everything()) %>% 
  select(time_to_treat, everything())

sapply(state_crime, function(x) sum(is.na(x)))

mod_twfe = feols(lvio ~ shalll + aovio +  density + rpcpi + rpcim + rpcui + rpcrpo + ppwm1019 + ppwm2029 + ppwm3039 + ppwm4049 + ppwm5064 + ppwf1019 + ppwf2029 + ppwf3039 + ppwf4049 + ppwf5064 + ppbm1019 + ppbm2029 + ppbm3039 + ppbm4049 + ppbm5064 + ppbf1019 + ppbf2029 + ppbf3039 + ppbf4049 + ppbf5064 +ppnm1019 + ppnm2029 + ppnm3039 + ppnm4049 + ppnm5064 + ppnf1019 + ppnf1019 + ppnf2029 + ppnf3039 + ppnf4049 + ppnf5064 | state + year, state_crime)

summary(mod_twfe)


#bacon decomp

mod_bacon = bacon(lvio ~ shalll, 
                  data = state_crime, 
                  id_var = "fipsstat", 
                  time_var = "year")

#calloway santana

atts = att_gt(yname = "lvio", # LHS variable
               tname = "year", # time variable
               idname = "fipsstat", # id variable
               gname = "treat_year", # first treatment period variable
               data = state_crime, # data
               xformla = NULL, # no covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "fipsstat", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional


#average treatment effects overt years for avg treatment effect

agg_effects = aggte(atts, type = "group", balance_e=TRUE)
summary(agg_effects)


#sun and abraham

agg_effects = aggte(atts, type = 'group', balance_e = T)
summary(agg_effects)

mod_sa = feols(lvio ~ sunab(treat_year, year) + aovio +  density + rpcpi + rpcim + rpcui + rpcrpo + ppwm1019 + ppwm2029 + ppwm3039 + ppwm4049 + ppwm5064 + ppwf1019 + ppwf2029 + ppwf3039 + ppwf4049 + ppwf5064 + ppbm1019 + ppbm2029 + ppbm3039 + ppbm4049 + ppbm5064 + ppbf1019 + ppbf2029 + ppbf3039 + ppbf4049 + ppbf5064 +ppnm1019 + ppnm2029 + ppnm3039 + ppnm4049 + ppnm5064 + ppnf1019 + ppnf1019 + ppnf2029 + ppnf3039 + ppnf4049 + ppnf5064 | fipsstat + year,
               data = state_crime,
               vcov = ~ fipsstat + year)

iplot(mod_sa, sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = "Sun & Abraham (2020)")

summary(mod_sa)

foreach(y = 1:Y) %do% {
  # index outcome
  yval = outcomes[y]
  acon = acontrols[y]
  
  # formula for index outcome
  sa_spec = as.formula(paste0(yval, "~ sunab(treat_year, year) + density + 
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
  # twfe model spec using formula
  mod_SA = feols(fml = sa_spec, 
                  data = state_crime,
                  subset = ~ year < 1992,
                  vcov = ~ fipsstat + year)
  
  # assign model name according to index outcome
  
  mod_name = paste("SA", yval, sep = "_")
  assign(mod_name, mod_SA)
  
  fname_mod = paste0(mod_name, ".RDs")
  save(file = file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_mod))
  
}

par(mfrow=c(1,1))
iplot(SA_lvio,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Violent Crime")
par(mfrow=c(1,1))
iplot(SA_lpro,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Property Crime")
iplot(SA_lmur,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Murder")
par(mfrow=c(1,1))
iplot(SA_lrap,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Rape")
iplot(SA_laga,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Aggravated Assault")
par(mfrow=c(1,1))
iplot(SA_lrob,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Robbery")
iplot(SA_lbur,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Burglary")
par(mfrow=c(1,1))
iplot(SA_llar,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Larceny")
iplot(SA_laut,sep =.5,ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment',
      sub = "Auto Theft")