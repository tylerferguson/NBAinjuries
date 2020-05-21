raw_player_totals <- read_csv("inst/extdata/bref_player_totals.csv")

group_positions <- raw_player_totals %>%
  count(slugPlayerBREF, groupPosition) %>%
  arrange(desc(n)) %>%
  group_by(slugPlayerBREF) %>%
  summarise(
    GroupPosition = first(groupPosition)
  )

player_totals <- raw_player_totals %>%
  left_join(group_positions, by = "slugPlayerBREF")

usethis::use_data(player_totals, overwrite = TRUE)
