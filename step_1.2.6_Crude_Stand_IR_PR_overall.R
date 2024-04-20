
presentation_data %>%
  group_by(myIMD) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_PR_1000 = numer/denom*1000) %>%
  ungroup()
