crime = read.csv("D:/UT/Causal Inference/Lott n Mustard/UpdatedStateLevelData-2010.csv") %>%
  as.data.frame() %>%
  filter(year>=1977 & year<=1992) 

treatment_grp = crime %>% 
  group_by(state) %>% 
  summarize(tot_count = sum(shalll)) %>% 
  na.omit()

state_implementation = left_join(crime, treatment_grp, by = c("state" = "state")) 

state_implementation = state_implementation %>% 
  mutate(treatment_year = ifelse(tot_count == 16, 'before 1977', ifelse(tot_count <= 16 & tot_count > 0, 1992 - (tot_count - 1), 0))) %>%
  mutate(pre_treatment = ifelse(treatment_year == 'before 1977',1,0))

rollout = state_implementation %>%
  distinct(state, treatment_year) %>% 
  arrange(treatment_year)

pre_treatment = rollout %>% 
  filter(treatment_year == 'before 1977')

post_treatment = rollout %>% 
  filter(treatment_year != 'before 1977') %>% 
  filter(treatment_year != 0)

rollout_final = rbind(pre_treatment, post_treatment)
colnames(rollout_final) = c("State", "Year")

df = rollout_final %>% 
  kbl(caption = "State-wise implementation dates of the right to carry guns law") %>%
  kable_classic(full_width = F, html_font = "Cambria")
df

