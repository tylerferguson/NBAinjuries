library(tidyverse)
library(dtplyr)
library(data.table)
library(nbastatR)
library(assertr)
library(lubridate)

add_profile_info <- function(df, bref_players) {
  bbref_profiles <- read_csv("inst/extdata/bref_bios.csv") %>%
    select(slugPlayerBREF, dateBirth, heightInches, weightLBS, namePosition)

  nba_profiles <- read_csv("inst/extdata/nba_bios.csv") %>%
    select(idPlayer, yearSeasonFirst, yearSeasonLast, countSeasonsPlayed)

  df %>%
    left_join(bbref_profiles, by = "slugPlayerBREF") %>%
    left_join(bref_players, by = "slugPlayerBREF") %>%
    left_join(nba_profiles, by = "idPlayer") %>%
    assert(not_na, countSeasonsPlayed, yearSeasonFirst, yearSeasonLast)
}

add_playoff_indicator <- function(df) {
  franchise_index <- read_csv("inst/extdata/franchise_index.csv") %>%
    select(Team, MadePlayoffs, Season) %>%
    separate(Season, into=c("SeasonStartYear", "SeasonEndYear")) %>%
    mutate(
      SeasonEndYear = paste0(substr(SeasonStartYear, 1, 2), SeasonEndYear),
      Team = if_else(Team == "New Orleans/Oklahoma City Hornets", "NO/Ok. City Hornets", Team)
    ) %>%
    separate(Team, into=c("Location", "Name"), remove = FALSE, sep = " (?=[^ ]*$)")

  df %>%
    rename(TeamName = Team) %>%
    mutate(
      TeamName = case_when(
        TeamName == "Sonics" ~ "SuperSonics",
        # Correct mistaken use of Pelicans name
        TeamName == "Pelicans" & season == "2012-2013" ~ "Hornets",
        TRUE ~ TeamName
      )
    ) %>%
    separate(season, into=c("SeasonStartYear", "SeasonEndYear"), remove = FALSE) %>%
    left_join(franchise_index, by = c("TeamName" = "Name", "SeasonEndYear", "SeasonStartYear")) %>%
    left_join(sportsreferenceR::TEAM_IDS, by = c("Team" = "Franchise"))
}

add_group_position <- function(df) {
  utils::data(player_totals, package = "NBAinjuries")

  group_positions <- player_totals %>%
    distinct(slugPlayerBREF, MostPlayedGroupPosition)

  df %>%
    left_join(group_positions, by = "slugPlayerBREF")
}

add_previous_transaction <- function(df) {
  player_transactions <- read_csv("inst/extdata/bref_transactions.csv") %>%
    select(
      slugPlayerBREF,
      LastTransactionDate = dateTransaction,
      LastTransactionDescription = descriptionTransaction,
      isGLeagueMovement, isDraft, isSigned, isWaived, isTraded) %>%
    mutate(
      LastTransactionType = case_when(
        isGLeagueMovement & str_detect(LastTransactionDescription, "Assigned to") ~ "Assigned to G-League",
        isGLeagueMovement & str_detect(LastTransactionDescription, "Recalled from") ~ "Recalled from G-League",
        isDraft ~ "Draft",
        isSigned ~ "Signing",
        isWaived ~ "Waived",
        isTraded ~ "Trade",
        str_detect(LastTransactionDescription, "Suspended by the league.") ~ "League Suspension",
        str_detect(LastTransactionDescription, "Suspended from") ~ "Team Suspension",
        str_detect(LastTransactionDescription, "Claimed on waivers") ~ "Claimed on waivers",
        str_detect(LastTransactionDescription, "Announced retirement.|Retired from") ~ "Retired",
        str_detect(LastTransactionDescription, "Sold") ~ "Sold",
        str_detect(LastTransactionDescription, "Converted from") ~ "Converted from two-way",
        str_detect(LastTransactionDescription, "Released") ~ "Released",
        str_detect(LastTransactionDescription, "Sent") ~ "Sent",
        str_detect(LastTransactionDescription, "Moved from the ABA to the NBA ") ~ "ABA merger",
        TRUE ~ NA_character_
      )
    ) %>%
    add_row(tibble_row(
      slugPlayerBREF = "anticpe01",
      LastTransactionDate = date("2013-07-25"),
      LastTransactionDescription = "Signed a multi-year contract with the Atlanta Hawks",
      LastTransactionType = "Signing"
    ))

  last_transactions <- df %>%
    left_join(player_transactions, by = "slugPlayerBREF") %>%
    filter(LastTransactionDate <= DateInjured & !(LastTransactionType %in% c("League Suspension", "Team Suspension"))) %>%
    arrange(desc(LastTransactionDate)) %>%
    group_by(slugPlayerBREF, DateInjured) %>%
    summarise(
      LastTransactionDate = first(LastTransactionDate),
      LastTransactionDescription = first(LastTransactionDescription),
      LastTransactionType = first(LastTransactionType)
    )

  df %>%
    left_join(last_transactions, by = c("slugPlayerBREF", "DateInjured"))
}

to_output <- function(df) {
  df %>%
    select(
      PlayerName = OGName,
      PlayerDOB = dateBirth,
      Height = heightInches,
      Weight = weightLBS,
      Positions = namePosition,
      GroupPosition = MostPlayedGroupPosition,
      IsActive = isActive,
      FirstSeason = yearSeasonFirst,
      LastSeason = yearSeasonLast,
      SeasonsPlayed = countSeasonsPlayed,
      TeamNameFull = Team,
      TeamLocation = Location,
      TeamName,
      Season = season,
      SeasonStartYear,
      SeasonEndYear,
      TeamMadePlayoffs = MadePlayoffs,
      DateInjured,
      DateReturned,
      GamesMissed,
      DaysInjured = DaysBetweenInjuredReturned,
      DaysBetweenGames = DaysBetweenLastAndReturnGame,
      BodyPartInjured = body_part,
      InjuryDescription = Notes,
      LastTransactionDate,
      LastTransactionDescription,
      LastTransactionType
    )
}

NA_date_ <- structure(NA_real_, class = "Date")

injury_reports <- read_csv("inst/extdata/basketball_2007.csv") %>%
  mutate(
    Date = dmy(Date),
    Name = if_else(is.na(Acquired), Relinquished, Acquired),
    OGName = Name,
    DateInjured = if_else(is.na(Relinquished), NA_date_, Date),
    DateReturned = if_else(is.na(Acquired), NA_date_, Date)
  ) %>%
  filter(!is.na(DateInjured)) %>%
  assert(not_na, Name, OGName) %>%
  select(Name, DateInjured, DateReturned, everything())

bref_players <- dictionary_bref_players() %>%
  mutate(
    Name = namePlayerBREF %>% str_trim() %>% stringi::stri_trans_general("Latin-ASCII") %>% str_replace_all(c("\\((.*)\\)" = "", "\\sI+V?" = "", "[JS]r\\." = "")) %>% str_trim(),
    id = Name %>% str_replace_all(regex("\\s|\\."), "") %>% tolower()
  ) %>% filter(!(slugPlayerBREF %in% c('bookede02', 'willire01', 'leeda01', 'johnsch04', 'smithch03', 'hendege01', "smithgr01", "tayloje01", "hamilju02", "jackslu01", "willima04", "jamesmi02", "trentga01", "ricegl01", "jacksja01", "nancela01", "hardati01", "dunlemi01", "robingl01", "lucasjo01")))

names <- injury_reports %>%
  # keep only injuries not returns
  filter(!is.na(DateInjured)) %>%
  distinct(Name) %>%
  # filter out coaches
  filter(!(Name %in% c("Hank Egan", "Phil Jackson", "Mike Dunleavy Sr.", "Steve Clifford"))) %>%
  mutate(OGName = Name) %>%
  mutate(
    Name = case_when(
      Name == "D.J. Augustine" ~ "D.J. Augustin",
      Name == "Bill Walker" ~ "Henry Walker",
      Name == "Jakob Poeltl" ~ "Jakob Poltl",
      Name == "rence Kinsey" ~ "Tarence Kinsey",
      Name == "Vitor Faverani" ~ "Vitor Luiz Faverani",
      TRUE ~ Name
    )
  ) %>%
  separate(Name, c("Name1", "Name2", "Name3"), sep = "/") %>%
  pivot_longer(c(Name1, Name2, Name3), names_to = "Temp", values_to = "Name", values_drop_na = TRUE) %>%
  select(-Temp) %>%
  mutate(
    Name = Name %>% str_trim() %>% str_replace_all(c("\\((.*)\\)" = "", "\\sI+V?" = "", "[JS]r\\." = "")) %>% str_trim(),
    id = Name %>% str_replace_all(regex("\\s|\\."), "") %>% tolower()
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  left_join(bref_players %>% select(slugPlayerBREF, id), by = "id") %>%
  group_by(OGName) %>%
  fill(slugPlayerBREF, .direction = "downup")

game_logs <- read_csv("inst/extdata/game_logs.csv")

games <- names %>%
  mutate(
    Name = if_else(Name == "Jakob Poltl", "Jakob Poeltl", Name),
    id = Name %>% str_replace_all(regex("\\s|\\."), "") %>% tolower()
  ) %>%
  left_join(game_logs, by = "id")

injuries <- injury_reports %>%
  lazy_dt() %>%
  left_join(games, by = "OGName") %>%
  select(OGName, slugPlayerBREF, idPlayer, DateInjured, dateGame, DateReturned, everything()) %>%
  group_by(OGName, slugPlayerBREF, idPlayer, DateInjured) %>%
  arrange(DateInjured, dateGame) %>%
  mutate(
    PreviousGameNumberTeam = lag(numberGameTeamSeason),
    PreviousGameSeason = lag(yearSeason),
    LastGameDate = lag(dateGame)
  ) %>%
  filter(dateGame > DateInjured) %>%
  summarise(
    DateReturned = dateGame %>% first(),
    GamesMissed = numberGameTeamSeason - PreviousGameNumberTeam - 1 + 82 * (yearSeason - PreviousGameSeason),
    LastGameDate = LastGameDate %>% first(),
  ) %>%
  group_by(OGName, slugPlayerBREF, idPlayer, DateReturned) %>%
  summarise(
    DateInjured = DateInjured %>% first(),
    GamesMissed = unique(GamesMissed),
    DaysBetweenLastAndReturnGame = unique(interval(LastGameDate, DateReturned) / ddays(1))
  ) %>%
  as_tibble() %>%
  select(OGName, slugPlayerBREF, idPlayer, DateInjured, DateReturned, GamesMissed, DaysBetweenLastAndReturnGame) %>%
  assert(within_bounds(0, Inf), DaysBetweenLastAndReturnGame) %>%
  mutate(
    DaysBetweenInjuredReturned = interval(DateInjured, DateReturned) / ddays(1)
  ) %>%
  assert(within_bounds(0, Inf), DaysBetweenInjuredReturned) %>%
  left_join(injury_reports, by=c("OGName", "DateInjured")) %>%
  rename(DateReturned = DateReturned.x) %>%
  select(-DateReturned.y, -Acquired, - Relinquished, -Date_2, -Date, -Name) %>%
  arrange(body_part, second, extra) %>%
  distinct(OGName, DateInjured, .keep_all = TRUE) %>%
  mutate(
    # handle case where player traded during injury and next team is behind schedule
    GamesMissed = if_else(GamesMissed < 0, 0, GamesMissed)
  ) %>%
  assert(within_bounds(0, Inf), GamesMissed) %>%
  verify(is.na(DaysBetweenLastAndReturnGame) | DaysBetweenLastAndReturnGame >= DaysBetweenInjuredReturned) %>%
  add_profile_info(bref_players %>% select(slugPlayerBREF, isActive)) %>%
  add_playoff_indicator()  %>%
  add_group_position() %>%
  add_previous_transaction() %>%
  to_output()

usethis::use_data(injuries, overwrite = TRUE)
