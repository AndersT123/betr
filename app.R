library(betr)

league_games <- scrape_games("27-01-2020", "01-03-2020")
#run_app(to_database = system.file("extdata", "test-league", package = "betr"))
run_app(to_database = path.expand("~/league-juni"))