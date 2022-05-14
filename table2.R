table2 = crime %>%
  select(state, year, ratvio, ratpro, ratmur, ratrap, rataga, ratrob, rataut, ratbur, ratlar) %>%
  filter(year >= 1977 & year <= 1992)

ratvio_mean = mean(table2$ratvio)
ratvio_sd = sd(table2$ratvio)
ratvio_max = max(table2$ratvio)
ratvio_min = min(table2$ratvio)

ratpro_mean = mean(table2$ratpro)
ratpro_sd = sd(table2$ratpro)
ratpro_max = max(table2$ratpro)
ratpro_min = min(table2$ratpro)

ratmur_mean = mean(table2$ratmur)
ratmur_sd = sd(table2$ratmur)
ratmur_max = max(table2$ratmur)
ratmur_min = min(table2$ratmur)

ratrap_mean = mean(table2$ratrap)
ratrap_sd = sd(table2$ratrap)
ratrap_max = max(table2$ratrap)
ratrap_min = min(table2$ratrap)

rataga_mean = mean(table2$rataga)
rataga_sd = sd(table2$rataga)
rataga_max = max(table2$rataga)
rataga_min = min(table2$rataga)

ratrob_mean = mean(table2$ratrob)
ratrob_sd = sd(table2$ratrob)
ratrob_max = max(table2$ratrob)
ratrob_min = min(table2$ratrob)

rataut_mean = mean(table2$rataut)
rataut_sd = sd(table2$rataut)
rataut_max = max(table2$rataut)
rataut_min = min(table2$rataut)

ratbur_mean = mean(table2$ratbur)
ratbur_sd = sd(table2$ratbur)
ratbur_max = max(table2$ratbur)
ratbur_min = min(table2$ratbur)

ratlar_mean = mean(table2$ratlar)
ratlar_sd = sd(table2$ratlar)
ratlar_max = max(table2$ratlar)
ratlar_min = min(table2$ratlar)

df1 = data.frame(
  Crime_Type = c("Violent Crime", "Property Crime", "Murder", "Rape", "Aggravated Assault", "Robbery","Auto Theft", "Burglary", "Larceny"),
  Mean = round(c(ratvio_mean, ratpro_mean, ratmur_mean, ratrap_mean, rataga_mean, ratrob_mean, rataut_mean, ratbur_mean, ratlar_mean),2),
  SD = round(c(ratvio_sd, ratpro_sd, ratmur_sd, ratrap_sd, rataga_sd, ratrob_sd, rataut_sd, ratbur_sd, ratlar_sd),2),
  Max = round(c(ratvio_max, ratpro_max, ratmur_max, ratrap_max, rataga_max, ratrob_max, rataut_max, ratbur_max, ratlar_max),2),
  Min = round(c(ratvio_min, ratpro_min, ratmur_min, ratrap_min, rataga_min, ratrob_min, rataut_min, ratbur_min, ratlar_min),2)
) 

df1 = df1 %>%
  kbl(caption = "Summary of crime rate statistics categorized by the type of crime") %>%
  kable_classic(full_width = F, html_font = "Cambria")
df1


#table3
table3 = crime %>%
  select(state, year, aovio, aopro, aomur, aorap, aoaga, aorob, aoaut, aobur, aolar) %>%
  filter(year >= 1977 & year <= 1992) %>%
  na.omit()

aovio_mean = mean(table3$aovio)
aovio_sd = sd(table3$aovio)
aovio_max = max(table3$aovio)
aovio_min = min(table3$aovio)

aopro_mean = mean(table3$aopro)
aopro_sd = sd(table3$aopro)
aopro_max = max(table3$aopro)
aopro_min = min(table3$aopro)

aomur_mean = mean(table3$aomur)
aomur_sd = sd(table3$aomur)
aomur_max = max(table3$aomur)
aomur_min = min(table3$aomur)

aorap_mean = mean(table3$aorap)
aorap_sd = sd(table3$aorap)
aorap_max = max(table3$aorap)
aorap_min = min(table3$aorap)

aoaga_mean = mean(table3$aoaga)
aoaga_sd = sd(table3$aoaga)
aoaga_max = max(table3$aoaga)
aoaga_min = min(table3$aoaga)

aorob_mean = mean(table3$aorob)
aorob_sd = sd(table3$aorob)
aorob_max = max(table3$aorob)
aorob_min = min(table3$aorob)

aoaut_mean = mean(table3$aoaut)
aoaut_sd = sd(table3$aoaut)
aoaut_max = max(table3$aoaut)
aoaut_min = min(table3$aoaut)

aobur_mean = mean(table3$aobur)
aobur_sd = sd(table3$aobur)
aobur_max = max(table3$aobur)
aobur_min = min(table3$aobur)

aolar_mean = mean(table3$aolar)
aolar_sd = sd(table3$aolar)
aolar_max = max(table3$aolar)
aolar_min = min(table3$aolar)

df2 = data.frame(
  Crime_Type = c("Violent Crime", "Property Crime", "Murder", "Rape", "Aggravated Assault", "Robbery","Auto Theft", "Burglary", "Larceny"),
  Mean = round(c(aovio_mean, aopro_mean, aomur_mean, aorap_mean, aoaga_mean, aorob_mean, aoaut_mean, aobur_mean, aolar_mean),2),
  SD = round(c(aovio_sd, aopro_sd, aomur_sd, aorap_sd, aoaga_sd, aorob_sd, aoaut_sd, aobur_sd, aolar_sd),2),
  Max = round(c(aovio_max, aopro_max, aomur_max, aorap_max, aoaga_max, aorob_max, aoaut_max, aobur_max, aolar_max),2),
  Min = round(c(aovio_min, aopro_min, aomur_min, aorap_min, aoaga_min, aorob_min, aoaut_min, aobur_min, aolar_min),2)
) 

df2 = df2 %>%
  kbl(caption = "Summary of no of arrests statistics categorized by the type of crime") %>%
  kable_classic(full_width = F, html_font = "Cambria") 
df2


#plot1
plot1 = crime %>%
  select(year, vio, prop, murder, rape, assault, rob, auto, burg, larc) %>%
  na.omit()
plot1 = plot1 %>%
  mutate(total_crime = rowSums(across(where(is.numeric))))
plot1 = plot1 %>%
  select(year, total_crime) %>%
  group_by(year) %>%
  summarise(total = sum(total_crime))
ggplot(plot1) +
  geom_line(aes(x=year,y=total))+
  labs(xlab = "Year",
       ylab = "Total number of crime cases",
       title = "Total crime over the years")
