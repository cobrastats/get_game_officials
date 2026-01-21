
get_team_schedule_wbb <- function(
    team.id = NA,
    team.name = NA,
    season = NA,
    ids_df = NULL,
    use_file = FALSE,
    save_file = FALSE,
    base_path = NA,
    overwrite = FALSE,
    use_chromote = FALSE
) 
{
  if (is.na(team.id) & !is.na(team.name) & !is.na(season)) {
    team.id <- ids_df$team_id[which(ids_df$team == 
                                            team.name & ids_df$season == season)]
  }
  else if (is.na(team.id) & is.na(team.name) & is.na(season)) {
    message("Improper Request")
    return(NULL)
  }
  url_text <- paste0("https://stats.ncaa.org/teams/", team.id)
  file_dir <- paste0(base_path, "team_schedule/")
  file_path <- paste0(file_dir, team.id, ".html")
  if (save_file & !is.na(base_path) & (!file.exists(file_path) | 
                                       overwrite)) {
    isUrlRead <- T
    file_url <- url(url_text)
    html <- readLines(con = file_url, warn = F)
    close(file_url)
    dir.create(file_dir, recursive = T, showWarnings = F)
    writeLines(html, file_path)
  }
  else if (file.exists(file_path) & use_file) {
    html <- readLines(file_path, warn = F)
  }
  else {
    isUrlRead <- T
    if (use_chromote == TRUE) {
      html <- try(scrape_dynamic_tables(url_text, session = NULL), 
                  silent = T)
      if (class(html)[1] == "try-error") {
        message("Retry again")
        html <- try(scrape_dynamic_tables(url_text, session = NULL), 
                    silent = T)
        if (class(html)[1] == "try-error") {
          message("Retry again")
          html <- scrape_dynamic_tables(url_text, session = NULL)
        }
      }
    }
    else {
      file_url <- url(url_text, headers = c(UserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)\",\n      \"AppleWebKit/537.36 (KHTML, like Gecko)\",\n      \"Chrome/124.0.0.0 Safari/537.36"))
      html <- tryCatch(readLines(con = file_url, warn = F), 
                       error = function(e) {
                         message("Error occurred: Access might be denied. Try setting the `use_chromote` argument to `TRUE`")
                         return(NULL)
                       })
      if (is.null(html)) {
        return(data.frame())
      }
      close(file_url)
    }
  }
  if (class(html)[1] == "xml_document") {
    tables <- rvest::html_table(html, header = TRUE)
    tables <- lapply(tables, as.data.frame)
    html <- as.character(html)
  }
  else {
    tables <- XML::readHTMLTable(html)
  }
  if (!is.null(tables[[1]])) {
    df <- data.frame(as.matrix(tables[[1]]), stringsAsFactors = F)
  }
  else {
    message(paste(team.id, "has no schedule"))
    return(data.frame())
  }
  df <- df[seq(1, nrow(df), by = 2), ]
  df <- df[!is.na(df$Opponent), ]
  df$Opponent_with_detail <- str_remove(df$Opponent, " 202.*$")
  df$Opponent <- str_remove(df$Opponent_with_detail, " \\@[A-Z].*$")
  game_ids <- unlist(stringr::str_extract_all(html, "(?<=contests/)\\d+(?=[/])"))
  message("\nParsing Schedule")
  parsed <- lapply(df$Opponent, strsplit, "@")
  parsed <- lapply(parsed, function(x) {
    x <- unlist(x)
    t <- stringr::str_extract(x, "(?<=[\\#[0-9]+] ).*")
    t[is.na(t)] <- x[is.na(t)]
    if (!any(trimws(t) %in% ids_df$team)) {
      for (j in 1:length(t)) {
        i <- 1
        while (!substr(t[j], 1, i) %in% ids_df$team && 
               i <= nchar(t[j])) {
          i <- i + 1
        }
        t[j] = substr(t[j], 1, i)
      }
    }
    return(t)
  })
  parsed_detail <- lapply(df$Opponent_with_detail, strsplit, 
                          "@")
  parsed_detail <- lapply(parsed_detail, function(x) {
    x <- unlist(x)
    t <- stringr::str_extract(x, "(?<=[\\#[0-9]+] ).*")
    t[is.na(t)] <- x[is.na(t)]
    if (!any(trimws(t) %in% ids_df$team)) {
      for (j in 1:length(t)) {
        i <- 1
        while (!substr(t[j], 1, i) %in% ids_df$team && 
               i <= nchar(t[j])) {
          i <- i + 1
        }
        t[j] = substr(t[j], 1, i)
      }
    }
    return(t)
  })
  Home <- lapply(parsed, function(x) {
    if (x[1] == "" & !is.na(x[2])) 
      return(x[2])
  })
  Neutral <- lapply(parsed_detail, function(x) {
    if (length(x) == 2 & x[1] != "") 
      return(x[2])
  })
  home_team <- rep(NA, length(parsed))
  away_team <- rep(NA, length(parsed))
  is_neutral <- rep(F, length(parsed_detail))
  for (i in 1:length(parsed)) {
    if (!is.null(Home[[i]])) {
      home_team[i] <- trimws(Home[[i]])
    }
    else {
      away_team[i] <- trimws(parsed[[i]][[1]][1])
    }
    if (!is.null(Neutral[[i]])) {
      is_neutral[i] <- T
    }
  }
  team_name <- ids_df$team[which(ids_df$team_id == 
                                              team.id)]
  score <- strsplit(df$Result, "-")
  selected_score <- trimws(gsub("W", "", gsub("L", "", sapply(score, 
                                                              function(x) {
                                                                x[1]
                                                              }))))
  opponent_score <- trimws(sapply(score, function(x) {
    x[2]
  }))
  detail <- unname(sapply(opponent_score, function(x) {
    a <- gsub("\\)", "", strsplit(x, "\\(")[[1]][2])
  }))
  opponent_score <- unname(sapply(opponent_score, function(x) strsplit(x, 
                                                                       " \\(")[[1]][1]))
  detail <- ifelse(selected_score %in% c("Canceled", "Ppd"), 
                   selected_score, detail)
  selected_score <- ifelse(selected_score %in% c("Canceled", 
                                                 "Ppd"), NA, selected_score)
  new_game_ids <- rep(NA, nrow(df))
  if (length(game_ids) > 0) {
    new_game_ids[is.na(detail) | detail != "Canceled"][1:length(game_ids)] <- game_ids
  }
  team_data <- data.frame(Date = df$Date, Home = ifelse(!is.na(home_team), 
                                                        home_team, team_name), Home_Score = ifelse(!is.na(home_team), 
                                                                                                   opponent_score, selected_score), Away = ifelse(!is.na(away_team), 
                                                                                                                                                  away_team, team_name), Away_Score = ifelse(!is.na(away_team), 
                                                                                                                                                                                             opponent_score, selected_score), Box_ID = new_game_ids, 
                          Game_ID = new_game_ids, isNeutral = is_neutral, Detail = detail, 
                          Attendance = as.numeric(gsub(",", "", df$Attendance)), 
                          stringsAsFactors = F)
  team_data[team_data == "-"] <- NA
  message(paste0(team_name[1], " complete -- ", nrow(team_data), 
                 "/", length(game_ids), " games/ids found"))
  return(team_data)
}


get_game_officials_wbb = function(team,season,ids_df){
  
  newschedule = get_team_schedule_wbb(team.name=team,season=season,ids_df = ids_df)
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

officials = get_game_officials_wbb("Purdue","2025-26",teamids_wbb)
