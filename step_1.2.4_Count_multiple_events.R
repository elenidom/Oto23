
ele <- kk %>%
  filter(cal_year <= 2019) %>%
  group_by(patid) %>%
  mutate(
    all_events = sum(myevents)
  ) %>% ungroup() %>%
  filter(all_events > 0) %>%
  select(c(patid, all_events)) %>% 
  distinct()


summary(ele$all_events)

hist(ele$all_events, breaks = 100)

ele <- ele %>%
  mutate(
    po_rank = case_when(
      all_events == 1 ~ "1",
      all_events == 2 ~ "2",
      all_events == 3 ~ "3",
      TRUE ~ "> 3"
    )
  )

ele %>% group_by(po_rank) %>% summarise(Patients = n_distinct(patid))
