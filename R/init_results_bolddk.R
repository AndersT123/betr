bold_link <- bold_link <- xml2::read_html("https://www.bold.dk/fodbold/england/premier-league/")
dates <- bold_link %>% rvest::html_nodes(".small a") %>% rvest::html_text()
time <- bold_link %>% rvest::html_nodes(".time a") %>% rvest::html_text()
teams  <- bold_link %>% rvest::html_nodes(".name a") %>% rvest::html_text()
scores <- bold_link %>% rvest::html_nodes(".score a") %>% rvest::html_text()
games <- tibble::tibble(dates = dates, time = time, teams = teams, scores = scores)

games <- games %>% tidyr::unite(col = game_time, dates, time, sep=" ", remove = T) %>% 
  dplyr::mutate(game_time = lubridate::with_tz(lubridate::dmy_hm(game_time, tz = "Europe/Copenhagen"), tzone = "UTC")) %>%
  tidyr::separate(col = teams, into = c("home_team", "away_team"), sep = "[:space:]+[:punct:]+[:space:]", remove = T) %>%
  dplyr::mutate(home_team = stringr::str_trim(home_team), away_team = stringr::str_trim(away_team)) %>%
  # Warnings are produced by games that have empty scores, i.e. games that have not been played yet. This is ok as they produce NAs
  # in home_score and away_score
  tidyr::separate(col = scores, into = c("home_score", "away_score"), remove = T, convert = T)

