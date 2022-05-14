outcomes = c("lvio", "lpro", "lmur", "lrap", "laga", "lrob", "lbur", "llar", "laut")
acontrols = c("aovio", "aopro", "aomur", "aorap", "aoaga", "aorob", "aobur", "aolar", "aoaut")
Y = length(outcomes)

effects = foreach(y = 1:Y, .combine = rbind) %do% {
  yval = outcomes[y]
  
  twfe_name = paste("twfe", yval, sep = "_")
  fname_twfe = paste0(twfe_name, ".RDs")
  
  cs_name = paste("CS", yval, sep = "_")
  fname_cs = paste0(cs_name, ".RDs")
  
  sa_name = paste("SA", yval, sep = "_")
  fname_sa = paste0(sa_name, ".RDs")
  
  
  load(file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_twfe))
  load(file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_cs))
  load(file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_sa))

  estimates = c(yval, twfe_model[["coefficients"]][["shalll"]], cs_model[["overall.att"]])
  se = c(yval, twfe_model[["se"]][["shalll"]], cs_model[["overall.se"]])
  p = c(yval, twfe_model[["coeftable"]][["Pr(>|t|)"]][1], cs_model[["overall.se"]])
  
  rbind(estimates, se)
} %>% 
  as.data.frame()

L = length(effects$V1)
colnames(effects) = c("outcome", "TWFE", "CallowaySantAnna")
rownames(effects) = NULL
effects$TWFE = round(as.numeric(effects$TWFE), 3)
effects$CallowaySantAnna = round(as.numeric(effects$CallowaySantAnna), 3)

effects$TWFE = as.character(effects$TWFE)
effects$CallowaySantAnna = as.character(effects$CallowaySantAnna)


effects = effects %>% 
  mutate(outcome = ifelse(row_number()%%2 == 0, "", outcomes)) %>% 
  mutate(TWFE = ifelse(row_number()%%2 == 0, paste0("(", TWFE, ")"), TWFE), 
         CallowaySantAnna = ifelse(row_number()%%2 == 0, paste0("(", CallowaySantAnna, ")"), CallowaySantAnna))
effects %>%
  kbl(caption = "Effects") %>%
  kable_classic(full_width = F, html_font = "Cambria")


Y = length(outcomes)
bacontab = foreach(y = 1:Y, .combine = rbind) %do% {
  yval = outcomes[y]
  
  bacon_name = paste("bacon", yval, sep="_")
  fname_bacon = paste0(bacon_name, ".RDs")
  
  load(file.path("D:/UT/Causal Inference/Lott n Mustard/output/models", fname_bacon))
  
  estimates1 = c(yval, bacon_decomp[1,])
  estimates3 = c(yval, bacon_decomp[3,])
  
  rbind(estimates1, estimates3)
} %>% 
  data.frame()
colnames(bacontab) = c("Crime Variable", "Type", "Average Estimate", "Weight")
rownames(bacontab) = NULL
bacontab %>%
  kbl(caption = "Bacon Decomposition Estimates") %>%
  kable_classic(full_width = F, html_font = "Cambria") 
