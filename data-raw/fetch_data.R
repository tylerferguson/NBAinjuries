library(tidyverse)
library(dtplyr)
library(data.table)
library(nbastatR)
library(assertr)
library(lubridate)
library(future)

fetch_game_logs <- function() {
  plan(multiprocess)
  game_logs(
    seasons = 2007:2020,
    league = "NBA",
    result_types = "player",
    season_types = "Regular Season"
  ) %>%
    mutate(
      Name = namePlayer %>% str_trim() %>% stringi::stri_trans_general("Latin-ASCII") %>% str_replace_all(c("\\((.*)\\)" = "", "\\sI+V?" = "", "[JS]r\\." = "")) %>% str_trim(),
      id = Name %>% str_replace_all(regex("\\s|\\."), "") %>% tolower()
    ) %>%
    write_csv("inst/extdata/game_logs.csv")
}

fetch_bref_player_profiles <- function(player_slugsBREF) {
  # very nasty side effect!
  bref_bios(player_ids = player_slugsBREF)

  # result of side effect
  dataBREFPlayersBiography %>%
    write_csv("inst/extdata/bref_bios.csv")

  dataBREFPlayersTransactions %>%
    write_csv("inst/extdata/bref_transactions.csv")
}

fetch_nba_player_profiles <- function(ids) {
  player_profiles(player_ids = ids) %>%
    write_csv("inst/extdata/nba_bios.csv")
}

fetch_player_totals <- function() {
  bref_players_stats(seasons = 2007:2020, tables = c("totals"), assign_to_environment = FALSE) %>%
    select(-c(urlPlayerThumbnail, urlPlayerHeadshot, urlPlayerPhoto, urlPlayerStats, urlPlayerBREF, countTeamsPlayerSeasonTotals)) %>%
    write_csv("inst/extdata/bref_players_totals.csv")
}

fetch_franchise_index_page <- function() {
  sportsreferenceR::teams.franchise_index() %>%
    write_csv("inst/extdata/franchise_index.csv")
}
