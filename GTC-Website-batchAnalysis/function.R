loadGrowthCharts <- function(table, type = c("Weight", "Length", "HeadCircumference")) {
  require(duckdb)
  require(DBI)
  
  GAn_Sex <- unique(table %>% select(GAn, Sex))
  GTC_dt <- data.frame()
  con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
  for (i in c(1: nrow(GAn_Sex))) {
    temp <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn_Sex[[i,"GAn"]],"_", GAn_Sex[[i,"Sex"]],"_",type)) %>% 
      mutate(percentile = as.numeric(NA), GAn = GAn_Sex[[i,"GAn"]], Sex = GAn_Sex[[i,"Sex"]])
    GTC_dt <- rbind(GTC_dt, temp)
  }
  dbDisconnect(con, shutdown = TRUE)
  
  return(GTC_dt)
}

calculate_Z <- function(data = NULL, type = NULL) {
  factor <- ifelse(type == "Weight", 1000, 1)
  selected_col <- c("ID", "Sex", "GA_week", "GA_day", "GAn", "DOL", "Day", paste0("_", type), paste0("_", type, " Z"), "_Group")
  table <- data %>% 
    select(any_of(selected_col))

  if (paste0("_", type) %in% colnames(table)) {
    table <- table %>%
      mutate(Day = GA_week * 7 + GA_day + DOL, 
             GAn = round((GA_week * 7 + GA_day)/7)) 
    chou <- loadGrowthCharts(table, type)
    table <- table %>% left_join(chou)
    table[[paste0("_", type, " Z")]] <- round((table[[paste0("_", type)]] - table$Predicted_expected*factor) / (table$sigma*factor), digits = 3)
  }
  
  return(table)
}

findPercentile <- function(table, type, factor) {
  grid <- seq(0.1, 99.9, by = 0.1)
  RMQElowest <- NA
  lowestPercentile <- NA
  if (paste0("_", type) %in% colnames(table)) {
    table["type"] <- table[paste0("_", type)]
    for (i in c(1:length(grid))) {
      b <- table %>% 
        mutate(percentile = grid[i],
               expectedForPercentile = factor * (Predicted_expected + sigma * qnorm(grid[i]/100)),
               residual = type - expectedForPercentile)
      RMQE <- sqrt(sum(b$residual^2, na.rm = TRUE))
      RMQElowest <- ifelse(is.na(RMQElowest) | RMQE <= RMQElowest, RMQE, RMQElowest)
      lowestPercentile <- ifelse(RMQE == RMQElowest, i, lowestPercentile)
    }
  }
if (is.na(lowestPercentile)) {
  return(NA)
} else {
  return(grid[lowestPercentile])
}
  
}

FindPercentileForAll <- function(table, type, factor, colors) {
  df <- data.frame()
  withProgress(value = 0, message = paste0("Calculating ", type, " Trajectory Percentile"), {
    for (i in c(1:length(unique(table$ID)))) {
      incProgress(1/length(unique(table$ID)), detail = paste0("Now processing ID ", i))
      temp <- table %>% filter(ID %in% unique(table$ID)[i])
      percentile <- findPercentile(temp, type, factor)
      group <- unique(temp[["_Group"]])
      temp_df <- data.frame(ID = unique(table$ID)[i], Percentile = percentile, Group = group)
      df <- rbind(df, temp_df)
    }
  })

  colnames(df)[colnames(df) == "Percentile"] <- paste0("_", type, " Percentile")
  
  return(df)
}

reset_graph_box <- function(session, input, output) {
  updateSelectInput(session, "weight_type_analysis", selected = "")
  updateSelectInput(session, "length_type_analysis", selected = "")
  updateSelectInput(session, "HC_type_analysis", selected = "")
  
  output$weight_UI <- renderUI({})
  output$length_UI <- renderUI({})
  output$HC_UI <- renderUI({})
  
}
