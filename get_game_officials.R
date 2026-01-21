get_game_officials = function(team,season){
  
  newschedule = bigballR::get_team_schedule(team.name=team,season=season)
  game_ids = newschedule$Game_ID
  
  game_officials = data.frame()
  
  for (j in seq_along(game_ids)) {
    officials = paste0("https://stats.ncaa.org/contests/", game_ids[j], "/officials")
    
    webpage = tryCatch({
      rvest::read_html(officials)
    }, error = function(e) return(NULL))  
    
    if (is.null(webpage)) next
    
    tables = rvest::html_nodes(webpage, "table") %>%
      rvest::html_table(fill = TRUE)
    
    if (length(tables) < 4) next 
    
    official_table = tryCatch({
      as.data.frame(t(tables[[4]]))
    }, error = function(e) return(NULL))
    
    if (is.null(official_table) || ncol(official_table) < 3) next 
    
    colnames(official_table) = c("Official 1", "Official 2", "Official 3")
    official_table$Game_ID = game_ids[j]
    
    game_officials = rbind(game_officials, official_table)
  }
  
  schedule = merge(newschedule,game_officials)
  schedule$Date = as.Date(schedule$Date, format = "%m/%d/%Y")
  schedule = schedule[order(schedule$Date), ]
  
  return(schedule)
}

# Example
# officials = get_game_officials(team = "Purdue",season="2025-26")


