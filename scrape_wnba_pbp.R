wnba_data_pbp <- function(game_id = "1022200034",
                          ...){
  
  league_id <- substr(game_id, 1, 2)
  season_id <- substr(game_id, 4, 5)
  season <- ifelse(substr(season_id,1,1) == "9", paste0('19', season_id), paste0('20', season_id))
  league <- dplyr::case_when(
    substr(game_id, 1, 2) == '00' ~ 'nba',
    substr(game_id, 1, 2) == '10' ~ 'wnba',
    substr(game_id, 1, 2) == '20' ~ 'dleague',
    TRUE ~ 'NBA'
  )
  full_url <- glue::glue("https://data.{league}.com/data/10s/v2015/json/mobile_teams/{league}/{season}/scores/pbp/{game_id}_full_pbp.json")
  
  tryCatch(
    expr = {
      res <- httr::RETRY("GET", full_url, ...)
      
      # Check the result
      check_status(res)
      
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      
      data <- resp %>% 
        jsonlite::fromJSON() %>% 
        purrr::pluck("g")
      
      plays <- data %>% 
        purrr::pluck("pd") %>% 
        jsonlite::toJSON() %>% 
        jsonlite::fromJSON(flatten = TRUE)
      plays_df <- data.frame()
      plays_df <- purrr::map_df(plays[[1]],function(x){
        plays_df <- plays[[2]][[x]] %>%
          dplyr::mutate(period = x) %>%
          dplyr::select("period", tidyr::everything())
      }) 
      
      plays_df <- plays_df %>%
        dplyr::select(dplyr::any_of(c(
          "period" = "period",
          "event_num" = "evt",
          "clock" = "cl",
          "description" = "de",
          "locX" = "locX",
          "locY" = "locY",
          "opt1" = "opt1",
          "opt2" = "opt2",
          "event_action_type" = "mtype",
          "event_type" = "etype",
          "team_id" = "tid",
          "offense_team_id" = "oftid",
          "player1_id" = "pid",
          "player2_id" = "epid",
          "player3_id" = "opid",
          "home_score" = "hs",
          "away_score" = "vs",
          "order" = "ord"))) %>%
        dplyr::mutate(
          player2_id = as.integer(.data$player2_id),
          player3_id = as.integer(.data$player3_id),
          game_id = game_id,
          league = dplyr::case_when(
            substr(.data$game_id, 1, 2) == '00' ~ 'NBA',
            substr(.data$game_id, 1, 2) == '10' ~ 'WNBA',
            substr(.data$game_id, 1, 2) == '20' ~ 'G-League',
            TRUE ~ 'NBA')) %>%
        dplyr::select("game_id", "league", tidyr::everything()) %>%
        make_wehoop_data("WNBA Play-by-Play Information from data.WNBA.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no play-by-play data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(plays_df)
}