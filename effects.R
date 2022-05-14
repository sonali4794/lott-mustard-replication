outcomes <- c("lvio", "lpro", "lmur", "lrap", "laga", "lrob", "lbur", "llar", "laut")
acontrols <- c("aovio", "aopro", "aomur", "aorap", "aoaga", "aorob", "aobur", "aolar", "aoaut")
Y <- length(outcomes)

effects <- foreach(y = 1:Y, .combine = rbind) %do% {
  # point to outcome index
  yval = outcomes[y]
  
  # make model strings in order to load
  twfe_name <- paste("twfe", yval, sep = "_")
  fname_twfe <- paste0(twfe_name, ".RDs")
  
  cs_name <- paste("CS", yval, sep = "_")
  fname_cs <- paste0(cs_name, ".RDs")
  
  sa_name <- paste("SA", yval, sep = "_")
  fname_sa <- paste0(sa_name, ".RDs")
  
  # laod models into envi 
  # they will load as what they were named in the envi when saved
  load(file.path(path, 'output', 'models', fname_twfe))
  load(file.path(path, 'output', 'models', fname_cs))
  load(file.path(path, 'output', 'models', fname_sa))
  
  # access estimates, se, tvals, and pvals from each model
  estimates <- c(yval, twfe_model[["coefficients"]][["shalll"]], cs_model[["overall.att"]])
  se <- c(yval, twfe_model[["se"]][["shalll"]], cs_model[["overall.se"]])
  p <- c(yval, twfe_model[["coeftable"]][["Pr(>|t|)"]][1], cs_model[["overall.se"]])
  
  rbind(estimates, se)
} %>% 
  as.data.frame()
# might do something with this to get numbers in the left margin of the table, but for now :(
# mutate(rnames = ifelse(row_number()%%2 == 0, "", as.character(row_number()%/%2 +1)))

# doctor up the effecs a lil bit
L <- length(effects$V1)
colnames(effects) <- c("outcome", "TWFE", "CallowaySantAnna")
rownames(effects) <- NULL
effects$TWFE <- round(as.numeric(effects$TWFE), 3)
effects$CallowaySantAnna <- round(as.numeric(effects$CallowaySantAnna), 3)
effects$TWFE <- as.character(effects$TWFE)
effects$CallowaySantAnna <- as.character(effects$CallowaySantAnna)

effects <- effects %>% 
  mutate(outcome = ifelse(row_number()%%2 == 0, "", outcomes)) %>% 
  mutate(TWFE = ifelse(row_number()%%2 == 0, paste0("(", TWFE, ")"), TWFE), 
         CallowaySantAnna = ifelse(row_number()%%2 == 0, paste0("(", CallowaySantAnna, ")"), CallowaySantAnna))

#making a table 
effects_table <- stargazer(effects, 
                           type = "latex", 
                           summary = FALSE, 
                           rownames = FALSE)

write(effects_table, file = file.path(path, "output", "tables", "effext.tex"))