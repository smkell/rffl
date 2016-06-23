#' Reads projections from ESPN.
#'
#' @export
#' @examples
#' read.espn.projections()
read.espn.projections <- function() {
  library("data.table")
  library("rvest")
  library("stringr")

  print("Loading projections from ESPN")

  positions <- c(QB=0, RB=2, WR=4, TE=6)
  pages <- seq(0, 200, 40)
  base_url <- "http://games.espn.go.com/ffl/tools/projections"

  urls <- paste0(base_url, "?&slotCategoryId=", rep(positions, each=length(pages)), "&startIndex=", pages)
  projections <- lapply(urls, function(x) { read_html(x) %>% html_node("#playertable_0") %>% html_table() })
  projections <- lapply(projections, FUN = function(x) { x[-1,] })
  projections <- rbindlist(projections, use.names = TRUE, fill = TRUE)

  column_names <-
    c("RNK", "PLAYER",
      "passing.C/A", "passing.YDS", "passing.TD", "passing.INT",
      "rushing.RUSH", "rushing.YDS", "rushing.TD",
      "receiving.REC", "receiving.YDS", "receiving.TD",
      "PTS")
  colnames(projections) <- column_names

  # Clean up player names
  player_team_pos <- str_split(projections$PLAYER, ",")
  player_names <- sapply(player_team_pos, "[[", 1)
  team_pos <- sapply(player_team_pos, "[[", 2)
  teams <- sapply(str_split(str_replace(str_trim(team_pos), "Â", " "), " "), "[[", 1)
  pos <- sapply(str_trim(str_split(sapply(str_split(str_replace(str_trim(team_pos), "Â", " "), " "), "[[", 2), "Â")), "[[", 1)

  projections$PLAYER <- player_names
  projections <- cbind(projections, POS=pos)
  projections <- cbind(projections, TEAM=teams)

  projections$RNK <- as.numeric(projections$RNK)

  # Replace empty stats with zeros
  projections$`passing.C/A` <- str_replace(projections$`passing.C/A`, "--/--", "0/0")

  comp <- sapply(str_split(projections$`passing.C/A`, "/"), "[[", 1) %>% as.numeric()
  projections <- cbind(projections, `passing.C` = comp)

  attmpt <- sapply(str_split(projections$`passing.C/A`, "/"), "[[", 2) %>% as.numeric()
  projections <- cbind(projections, `passing.A` = attmpt)

  as.numeric_handle_dash <- function(x) {
    return(x %>% str_replace("--", "0") %>% as.numeric())
  }

  projections$passing.YDS <- as.numeric_handle_dash(projections$passing.YDS)
  projections$passing.TD <- as.numeric_handle_dash(projections$passing.TD)
  projections$passing.INT <- as.numeric_handle_dash(projections$passing.INT)

  projections$rushing.RUSH <- as.numeric_handle_dash(projections$rushing.RUSH)
  projections$rushing.YDS <- as.numeric_handle_dash(projections$rushing.YDS)
  projections$rushing.TD <- as.numeric_handle_dash(projections$rushing.TD)

  projections$receiving.REC <- as.numeric_handle_dash(projections$receiving.REC)
  projections$receiving.YDS <- as.numeric_handle_dash(projections$receiving.YDS)
  projections$receiving.TD <- as.numeric_handle_dash(projections$receiving.TD)

  # Factor the POS
  projections$POS <- as.factor(projections$POS)
  projections$TEAM <- as.factor(projections$TEAM)

  league_rules <-
    c(passing.YDS.per.pt = 25, passing.TD.pt = 4, passing.INT.pt = -2,
      rushing.YDS.per.pt = 10, rushing.TD.pt = 6,
      receiving.YDS.per.pt = 10, receiving.TD.pt = 6, receiving.REC.pt = 1)

  projections$PTS <-
    (projections$passing.YDS / league_rules["passing.YDS.per.pt"]) +
    (projections$passing.TD * league_rules["passing.TD.pt"]) +
    (projections$passing.INT * league_rules["passing.INT.pt"]) +
    (projections$rushing.YDS / league_rules["rushing.YDS.per.pt"]) +
    (projections$rushing.TD * league_rules["rushing.TD.pt"]) +
    (projections$receiving.REC * league_rules["receiving.REC.pt"]) +
    (projections$receiving.YDS / league_rules["receiving.YDS.per.pt"]) +
    (projections$receiving.TD * league_rules["receiving.TD.pt"])

  return(projections)
}
