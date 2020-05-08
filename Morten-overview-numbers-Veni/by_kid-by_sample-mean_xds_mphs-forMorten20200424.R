YD.all <- read_csv("YD-quantity-scores_rand-and-tt.csv") %>%
  mutate(aclew_child_id = as.character(aclew_child_id),
    community = "YD")
TS.all <- read_csv("TS-quantity-scores_rand-and-tt.csv") %>%
  mutate(community = "TS")

veni.all <- bind_rows(YD.all, TS.all)

veni.by.kid.by.sample <- veni.all %>%
  group_by(community, aclew_child_id, age_mo_round, Sample) %>%
  summarize(
    mean.ods_mph = mean(ods_mph),
    mean.tds_mph = mean(tds_mph)
  )

write_csv(veni.by.kid.by.sample,
  "by_kid-by_sample-mean_xds_mphs-forMorten20200424.csv")
