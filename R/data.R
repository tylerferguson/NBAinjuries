#' Details of over 6000 unique injuries of NBA players from the 2006-2007
#' season to the 2019-2020 season.
#'
#' A dataset containing injury details, time missed due to injury, player
#' biographical information and most recent player transaction details.
#'
#' @format A data frame with 6541 rows and 39 variables:
#' \describe{
#'   \item{PlayerName}{full name of player}
#'   \item{PlayerDOB}{date of birth}
#'   \item{Height}{height, in inches}
#'   \item{Weight}{weight, in pounds}
#'   \item{Positions}{all positions played during career according to
#'     Basketball Reference}
#'   \item{GroupPosition}{most frequently played group position during study
#'     period according to Basketball Reference (G, F or C)}
#'   \item{IsActive}{boolean indicating if player is active according to
#'     Basketball Reference}
#'   \item{FirstSeason}{end year of rookie season e.g. 2020 for 2019-2020 season}
#'   \item{LastSeason}{end year of final season e.g. 2020 for 2019-2020 season}
#'   \item{SeasonsPlayed}{number of seasons player actually participated in.
#'     Not just the number of years between their first and last season}
#'   \item{TeamNameFull}{full team name e.g. Brooklyn Nets}
#'   \item{TeamLocation}{location part of full team name e.g. Brooklyn}
#'   \item{TeamName}{second part of full team name e.g. Nets}
#'   \item{Season}{season in which injury occurred e.g. 2019-2020}
#'   \item{SeasonStartYear}{start year of Season e.g. 2019 for 2019-2020}
#'   \item{SeasonEndYear}{end year of Season e.g. 2020 for 2019-2020}
#'   \item{TeamMadePlayoffs}{boolen indicating if team made the playoffs that season}
#'   \item{DateInjured}{date injury was reported}
#'   \item{DateReturned}{date of player's first return game after injury}
#'   \item{GamesMissed}{number of games missed due to injury}
#'   \item{DaysInjured}{number of days between DateInjured and DateReturned}
#'   \item{DaysBetweenGames}{number of days between last game prior to
#'     injury and DateReturned}
#'   \item{BodyPartInjured}{body part mentioned in InjuryDescription}
#'   \item{InjuryDescription}{free text notes describing projected duration
#'     and type of injury}
#'   \item{LastTransactionDate}{date of most recent transaction involving
#'     player prior to injury (ignoring suspensions)}
#'   \item{LastTransactionDescription}{description of the transaction
#'     on LastTransactionDate}
#'   \item{LastTransactionType}{type of the transaction on LastTransactionDate
#'     e.g. Signing, Trade, etc.}
#' }
"injuries"
