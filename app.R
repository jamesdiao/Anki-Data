# title: "Data Exploration and Visualization Tools for Anki"
# author: "James Diao"
# date: "09 April 2019"
# web app: https://jamesdiao.shinyapps.io/ankidata/
# rsconnect::deployApp('/Users/jamesdiao/Documents/R/Anki-Data')

### To-do
# Optimize projection + smoother
# Explanation + math for suggested interval

### Code

# Install and load all required packages
pkg_list <- c("dplyr","tidyr","ggplot2","rjson","RSQLite", "DBI","anytime",
              "sqldf", "treemap", "plotly","shiny","shinycssloaders","shinyalert")
installed <- pkg_list %in% installed.packages()[,"Package"]
if (!all(installed)) install.packages(pkg_list[!installed])
sapply(pkg_list, require, character.only = T)

# Maximum upload size
options(shiny.maxRequestSize=50*1024^2)

ui <- fluidPage(
  useShinyalert(),
  titlePanel("Data Exploration and Visualization Tools for Anki"),
  p("Last updated: April 9, 2019"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload 'collection.anki2' file (50 MB limit)",
                multiple = FALSE),
      actionButton(inputId = "autofile", label = "Try with Test File"),
      h1(),
      selectizeInput("rm_decks", "Decks to exclude", 
                     choices = c("File Not Found"), selected = c("File Not Found"), multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 h1(),
                 helpText('Instructions: the app extracts data from a "collection.anki2" ',
                          'file, which should contain all of your relevant scheduling ',
                          'data. Start by exporting your Anki collection as a *.apkg or ',
                          '*.colpkg file. Rename the file to *.zip and unzip it ',
                          'to find the "collection.anki2" file. After ',
                          'uploading this file, you can explore the ',
                          'various analyses and visualizations in each',
                          'tab panel. To explore the app using preloaded sample data, ',
                          'hit "Try with Test File".'),
                 helpText('All source code is accessible at ',
                          'https://github.com/jamesdiao/Anki-Reviews.', 
                          'If the shinyapps.io web app is too slow, ',
                          'you can clone the repository and run the app',
                          'locally.'),
                 imageOutput("img", height = "500px")
        ),
        tabPanel("Summary", h1(), tableOutput("table") %>% withSpinner),
        tabPanel("Treemap", h1(), 
                 radioButtons(inputId = "tm_type", label = "Divisions", inline = TRUE,
                              choices = c("All categories","Learned/unlearned")),
                 plotOutput("treemap", height = "500px") %>% withSpinner
                 ),
        tabPanel("Over Time", h1(), 
                 radioButtons(inputId = "ot_type", label = "New vs. Review", inline = TRUE,
                              choices = c("New Cards","Review Cards")),
                 radioButtons(inputId = "ot_output", label = "Count vs. Time", inline = TRUE,
                              choices = c("Card Count","Time Spent")),
                 plotOutput("overtime") %>% withSpinner
                 ),
        tabPanel("Projection", h1(), 
                 radioButtons(inputId = "pr_output", label = "Review Count vs. Time", inline = TRUE,
                              choices = c("Review Card Count","Review Time")),
                 sliderInput(inputId = "span", label = "Smoothing factor", 
                             min = 0.1, max=1, value = 0.5, step = 0.1),
                 h5("Model generated for empirical values:"),
                 textOutput("params") %>% withSpinner, 
                 h2(),
                 plotOutput("projection") %>% withSpinner
                 )
      )
      
    )
    
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$autofile, {
    rv$COLLECTION_PATH <- "Files/JAD/collection.anki2"
    getData()
    updateSelectizeInput(session, "rm_decks",
                         choices = as.list(c(rv$decks_cat$category, 
                                             rv$decks_cat$subcategory))
                        )
    shinyalert("Success!", 
               'File successfully loaded. Browse the tab panels to explore the app!', 
               type = "success", timer = 6000)
  })
  
  observeEvent(input$file1, {
    rv$COLLECTION_PATH <- input$file1$datapath
    if (!grepl("anki2", rv$COLLECTION_PATH)) {
      shinyalert("Oops!", 'Please upload a "collection.anki" file', type = "error")
      rv$COLLECTION_PATH <- NULL
    } else {
      getData()
      updateSelectizeInput(session, "rm_decks",
                           choices = as.list(c(rv$decks_cat$category, 
                                               rv$decks_cat$subcategory)
                                             )
      )
    }
    shinyalert("Success!", 
               'File successfully loaded. Browse the tab panels to explore the app!', 
               type = "success", timer = 6000)
  })
  
  observeEvent(input$rm_decks, {
    req(rv$COLLECTION_PATH)
    getData()
  }, ignoreNULL = FALSE)
  
  getData <- reactive({
    
    RM_DECKS <- input$rm_decks
    con <- dbConnect(RSQLite::SQLite(), dbname=rv$COLLECTION_PATH)
    deckinfo <- dbGetQuery(con,'SELECT decks FROM col') %>% as.character %>% fromJSON
    rv$decks <- data.frame(did = names(deckinfo), 
                           name = sapply(deckinfo, function(d) d$name) %>% setNames(NULL), 
                           stringsAsFactors = FALSE
    )
    decks_cat <- rv$decks
    decks_cat$category <- gsub(":+.*$","",decks_cat$name) %>% trimws
    decks_cat$subcategory <- sub(":+",";;",decks_cat$name)
    decks_cat$subcategory <- sub(".*;;","",decks_cat$subcategory)
    decks_cat$subcategory <- gsub(":.*$","",decks_cat$subcategory) %>% trimws
    rv$decks_cat <- decks_cat
    rv$rev <- dbGetQuery(con,'SELECT CAST(id AS TEXT) AS id, CAST(cid AS TEXT) AS cid,
                         time, type, ease, factor FROM revlog')
    rv$cards <- dbGetQuery(con,'SELECT CAST(id AS TEXT) AS cid, 
                           CAST(did AS TEXT) AS did,
                           reps
                           FROM cards')
    dbDisconnect(con)
    rv$cards_w_decks <- merge(rv$cards, rv$decks,by="did")
    rv$rev$revdate <- anydate(as.numeric(rv$rev$id)/1000)
    # Assign deck info to reviews
    rev_w_decks <- merge(rv$rev, rv$cards_w_decks, by="cid")
    keep <- rep(TRUE, nrow(rev_w_decks))
    if (!is.null(RM_DECKS)) {
      keep <- sapply(rev_w_decks$name, function(n){
        !any(sapply(RM_DECKS, function(x) grepl(x, n, ignore.case = TRUE)))
      }) %>% setNames(NULL)
    }
    rev_w_decks <- rev_w_decks %>% filter(keep)
    rev_w_decks_unique <- rev_w_decks[!duplicated(
      paste(rev_w_decks$cid, rev_w_decks$type, rev_w_decks$revdate)),]
    rev_w_decks <- rev_w_decks
    rv$all_summary <- sqldf("SELECT revdate AS Date, 
                            COUNT(*) AS Count, 
                            SUM(time) AS Time,
                            type 
                            FROM rev_w_decks_unique 
                            GROUP BY revdate, type") %>% 
      mutate(Date = anydate(Date), 
             Time = Time/60000)
    rv$time_summary <- sqldf("SELECT revdate AS Date, 
                             COUNT(*) AS Count, 
                             SUM(time) AS Time,
                             type 
                             FROM rev_w_decks 
                             GROUP BY revdate, type") %>% 
      mutate(Date = anydate(Date), 
             Time = Time/60000)
    rv$error_summary <- sqldf("SELECT revdate AS Date, 
                              COUNT(*) AS Count 
                              FROM rev_w_decks 
                              WHERE ease==1 AND type==1 
                              GROUP BY revdate") %>% 
      mutate(Date = anydate(Date))
    rv$rev_w_decks <- rev_w_decks
    rv$rev_w_decks_unique <- rev_w_decks_unique
    cards_w_categories <- merge(rv$cards,rv$decks_cat,by="did") 
    keep <- TRUE
    if (!is.null(RM_DECKS)) {
      keep <- sapply(cards_w_categories$name, function(n){
        !any(sapply(RM_DECKS, function(x) grepl(x, n)))
      }) %>% setNames(NULL)
    }
    rv$cards_w_categories <- cards_w_categories %>% filter(keep)
  })
  
  
  
  
  
  output$table <- renderTable(
    colnames = FALSE, rownames = TRUE, {
    req(rv$COLLECTION_PATH)
    rev <- rv$rev
    cards <- rv$cards
    all_summary <- rv$all_summary
    time_summary <- rv$time_summary
    error_summary <- rv$error_summary
    
    new_summary <- all_summary %>% filter(type==0) %>% select(Date, Count) 
    rev_summary <- all_summary %>% filter(type==1) %>% select(Date, Count)
    new_time <- time_summary %>% filter(type==0) %>% select(Date, Time, Count)
    rev_time <- time_summary %>% filter(type==1) %>% select(Date, Time, Count)
    time_summary <- merge(new_time, rev_time, by="Date", all = TRUE) 
    time_summary[, 2:ncol(time_summary)] <- apply(time_summary[, 2:ncol(time_summary)], 2, 
                                                  function(col) {
                                                    if (class(col)=="numeric") replace(col, is.na(col), 0) 
                                                    else col
                                                  })
    colnames(time_summary) <- c("Date","New_Time","New_Count","Review_Time","Review_Count")
    
    rev_map <- rev_summary$Count %>% setNames(rev_summary$Date)
    error_rates = error_summary$Count/rev_map[as.character(error_summary$Date)]
    avg_int <- mean((rev$factor[rev$factor > 0]))/1000
    avg_error <- sum(error_summary$Count)/sum(rev_map[as.character(error_summary$Date)])
    new_int <- avg_int*log(0.9)/log(1-max(avg_error,0.01))
    
    avg_new = new_summary$Count %>% mean
    avg_rev = rev_summary$Count %>% mean
    daystocompletion <- ceiling(nrow(cards)/avg_new)-nrow(new_summary)
    completiondate <- ceiling(nrow(cards)/avg_new)-nrow(new_summary)+Sys.Date()
    
    tab <- data.frame(
      avg_new = avg_new %>% round(1),
      avg_rev = avg_rev %>% round(1),
      avg_error = sprintf("%s%%", 100*avg_error %>% signif(3)),
      avg_int = sprintf("%sx", avg_int %>% round(2)),
      new_int = sprintf("%sx", new_int %>% round(2)),
      daystocompletion = daystocompletion %>% round(1),
      completiondate = completiondate %>% round(1)
    ) %>% t
    
    rownames(tab) <- c("Average News/Day",
                       "Average Reviews/Day",
                       "Average Error",
                       "Average Interval",
                       "Suggested Base Interval", 
                       "Estimated Days to Completion",
                       "Estimated Completion Date")
    colnames(tab) <- c("Statistic")
    return(tab)
    
  })
  
  
  output$treemap <- renderPlot({
    req(rv$COLLECTION_PATH)
    cards_w_categories <- rv$cards_w_categories
    if (input$tm_type == "All categories") {
      deck_summary <- sqldf("SELECT category, subcategory, count(*) AS n_cards 
                             FROM cards_w_categories 
                             GROUP BY category, subcategory") 
    } else { 
      learned_deck_summary <- sqldf("SELECT category, subcategory, count(*) AS n_cards 
                                     FROM cards_w_categories 
                                     WHERE reps > 0
                                     GROUP BY category, subcategory")
      deck_summary <- rbind(sqldf("SELECT category, subcategory, count(*) AS n_cards 
                                   FROM cards_w_categories 
                                   WHERE reps > 0
                                   GROUP BY category, subcategory"), 
                            data.frame(category="Unlearned", 
                                       subcategory="Unlearned", 
                                       n_cards = nrow(rv$cards)-sum(learned_deck_summary$n_cards)
                                       )
                            )
    }
    tm <- treemap(deck_summary,
                  index=c("category","subcategory"),
                  vSize="n_cards", type="index", palette = "Set2",
                  title=sprintf("Card distribution by %s", 
                                tolower(input$tm_type)))
    tm
  })
  
  output$overtime <- renderPlot({
    
    req(rv$COLLECTION_PATH)
    all_summary <- rv$all_summary
    time_summary <- rv$time_summary
    new_summary <- all_summary %>% filter(type==0) %>% select(Date, Count) 
    rev_summary <- all_summary %>% filter(type==1) %>% select(Date, Count)
    new_time <- time_summary %>% filter(type==0) %>% select(Date, Time, Count)
    rev_time <- time_summary %>% filter(type==1) %>% select(Date, Time, Count)
    avg_new = new_summary$Count %>% mean
    avg_rev = rev_summary$Count %>% mean
    
    if (input$ot_output == "Card Count") {
      if (input$ot_type == "New Cards") {
        ggplot(new_summary,aes(x=Date,y=Count)) + 
          geom_bar(stat="identity",fill="darkblue") +
          ggtitle("New Cards Over Time") +
          xlab("Date") +
          ylab("New Cards") +
          geom_hline(yintercept = avg_new, lty = 2)
      } else {
        ggplot(rev_summary,aes(x=Date,y=Count)) + 
          geom_bar(stat="identity",fill="darkblue") +
          #geom_smooth(method='auto') + 
          ggtitle("Review Cards Over Time") +
          xlab("Date") +
          ylab("Review Cards") +
          geom_hline(yintercept = avg_rev, lty = 2)
      }
    } else {
      if (input$ot_type == "New Cards") {
        ggplot(new_time,aes(x=Date,y=Time)) + 
          geom_bar(stat="identity",fill="darkblue") +
          ggtitle("Time Spent on New Cards Over Time") +
          xlab("Date") +
          ylab("Time (min)") +
          geom_hline(yintercept = mean(new_time$Time), lty = 2)
      } else {
        ggplot(rev_time,aes(x=Date,y=Time)) + 
          geom_bar(stat="identity",fill="darkblue") +
          #geom_smooth(method='auto') + 
          ggtitle("Time Spent on Review Cards Over Time") +
          xlab("Date") +
          ylab("Time (min)") +
          geom_hline(yintercept = mean(rev_time$Time), lty = 2)
      }
    }
  })
  
  output$params <- renderText({
    req(rv$COLLECTION_PATH)
    new_summary <- rv$all_summary %>% filter(type==0) %>% select(Date, Count) 
    avg_new <- new_summary$Count %>% mean
    rev_summary <- rv$all_summary %>% filter(type==1) %>% select(Date, Count)
    rev_map <- rev_summary$Count %>% setNames(rev_summary$Date)
    avg_error <- sum(rv$error_summary$Count)/sum(rev_map[as.character(rv$error_summary$Date)])
    avg_int <- mean((rv$rev$factor[rv$rev$factor > 0]))/1000
    
    sprintf("New cards/day = %s, Total cards = %s, Error rate = %s%%, Multiplier = %sx", 
            avg_new %>% round(1), nrow(rv$cards),round(100*avg_error, 1), avg_int %>% round(2)
            )
  })
  
  output$projection <- renderPlot({
    req(rv$COLLECTION_PATH)
    span <- input$span
    RM_DECKS <- input$rm_decks
    rev <- rv$rev
    cards <- rv$cards
    all_summary <- rv$all_summary
    time_summary <- rv$time_summary
    error_summary <- rv$error_summary
    cards_w_categories <- rv$cards_w_categories
    
    all_deck_summary <- sqldf("SELECT category, subcategory, count(*) AS n_cards 
                              FROM cards_w_categories 
                              GROUP BY category, subcategory")
    
    new_summary <- all_summary %>% filter(type==0) %>% select(Date, Count) 
    rev_summary <- all_summary %>% filter(type==1) %>% select(Date, Count)
    
    
    new_time <- time_summary %>% filter(type==0) %>% select(Date, Time, Count)
    rev_time <- time_summary %>% filter(type==1) %>% select(Date, Time, Count)
    time_summary <- merge(new_time, rev_time, by="Date", all = TRUE) 
    time_summary[, 2:ncol(time_summary)] <- apply(time_summary[, 2:ncol(time_summary)], 2, 
                                                  function(col) {
                                                    if (class(col)=="numeric") replace(col, is.na(col), 0) 
                                                    else col
                                                  })
    colnames(time_summary) <- c("Date","New_Time","New_Count","Review_Time","Review_Count")
    
    rev_map <- rev_summary$Count %>% setNames(rev_summary$Date)
    error_rates <- error_summary$Count/rev_map[as.character(error_summary$Date)]
    avg_error <- sum(error_summary$Count)/sum(rev_map[as.character(error_summary$Date)])
    
    avg_new <- new_summary$Count %>% mean
    new_min_per_card <- sum(new_time$Time)/sum(new_time$Count)
    avg_rev <- rev_summary$Count %>% mean
    rev_min_per_card <- sum(rev_time$Time)/sum(rev_time$Count)
    avg_int <- mean((rev$factor[rev$factor > 0]))/1000
    new_int <- avg_int*log(0.9)/log(1-max(avg_error,0.01))
    
    multiplier <- avg_int
    cardsperday <- avg_new
    cutoff <- nrow(cards)
    days <- cutoff/cardsperday
    rlen <- 2*days
    forget <- avg_error
    
    terminal_interval <- 9999
    added_terms = 0
    max_pwr <- floor(log(terminal_interval, base = multiplier))
    intervals <- ceiling(multiplier^(0:max_pwr)) #c(1, 3, 7, 16, 40, 98)
    if (rlen > terminal_interval) 
      added_terms <- floor((rlen-sum(intervals))/terminal_interval)
    # Cumulative sum of interval gaps (1, 4, 11, ...)
    csintervals <- c(intervals, rep(terminal_interval, added_terms)) %>% cumsum
    # Add even-odd jitter to the cumulative interval after some number (keep)
    # to stabilize stacking effects
    keep <- 5
    n_csint <- length(csintervals)
    csintervals[keep:n_csint] <- csintervals[keep:n_csint] + 
      rep(c(0,1),length.out=n_csint-keep+1)
    # Scaling factor for adds
    scaling <- 1 # 1+ forget*(sum(days > csintervals)-2)
    # Power series of the forget decay factor (e.g., 1.0, 0.90, 0.81, ...)
    decay_0 <- (1-forget)^(0:length(csintervals))
    
    # Empty vector for number of reviews
    reviews <- rep(0, rlen)
    
    for (i in 1:days) {
      # `range` defines the index of consistently correct cards (2, 5, 12, ...)
      range <- i + csintervals
      # Make sure `range` fits inside the `reviews` vector
      range <- range[range <= rlen]
      # Make sure `decay` is the same size as `range`
      decay <- (1-forget)^(0:(length(range)-1))
      # Add reviews from "perfectly correct" cards initially added on day `i`
      # graded_scaling <- (1-(days-cut_csint[i]) / days) * (scaling-1) + 1
      reviews[range] <- reviews[range] + cardsperday * decay * scaling
      # For each added day (j), add back the series of incorrect cards. 
      # Assume that none of them are wrong more than twice.
      
      for (j in 1:(length(range))) {
        # `j_range` is the indexes of the series of incorrect cards to add back for each `j`
        j_range <- range - 1 + range[j]
        # Make sure `j_range` fits into the `reviews` vector
        j_range <- j_range[j_range <= rlen]
        if (length(j_range) == 0) break
        # Make sure `decay` is the same size as `range`
        j_decay <- (1-forget)^((j-1):(length(j_range)+j-2))
        # Add back the series of incorrect cards (2nd term = # of incorrect cards). 
        reviews[j_range] <- reviews[j_range] + forget*cardsperday*j_decay*scaling 
      }
      
    }
    
    
    reviews <- reviews %>% round(0)
    # Two-part smoothing process to simulate load balancing
    review_data <- data.frame(Day = 1:length(reviews), estimated = reviews)
    smoothed_1 <- reviews[1:4]
    smoothed_2 <- predict(loess(estimated ~ Day, 
                                data = review_data[5:nrow(review_data),], 
                                span = 0.1))
    smoothed <- c(smoothed_1, smoothed_2)
    
    if (input$pr_output == "Review Card Count") {
      # Final data frame
      obs <- rev_summary$Count
      obs_data <- data.frame(Day = 1:length(obs), estimated = obs)
      obs_smoothed <- predict(loess(estimated ~ Day, span = span, data = obs_data))
      
      f <- function(x) {
        sum((smoothed[1:nrow(rev_summary)]*x - obs_smoothed)^2)
      }
      rescale <- optimize(f=f, interval = c(0,2))$minimum
      
      all_data <- data.frame(rescale*review_data, 
                             observed = obs_smoothed[1:nrow(review_data)])
      
      plotdata <- all_data %>% gather(key = "Type", value = "Reviews", -Day) 
      ggp <- ggplot(plotdata, aes(x=Day, y=Reviews, color=Type)) + 
        ggtitle(sprintf("Projected Review Counts Over Time (rescale factor = %s)", signif(rescale,2))) + 
        geom_line(lwd = 0.8) + theme_bw() 
      
    } else {
      obs_time <- time_summary$New_Time + time_summary$Review_Time
      obs_time_data <- data.frame(Day = 1:length(obs_time), estimated = obs_time)
      obs_time_smoothed <- predict(loess(estimated ~ Day, span = span, data = obs_time_data))
      
      smoothed_time <- review_data*rev_min_per_card+cardsperday*new_min_per_card
      f <- function(x) {
        sum((smoothed_time[1:nrow(time_summary), "estimated"]*x - obs_time_smoothed)^2)
      }
      rescale <- optimize(f=f, interval = c(0,2))$minimum
      
      
      time_data <- data.frame(rescale*(review_data*rev_min_per_card+cardsperday*new_min_per_card), 
                              observed = obs_time_smoothed[1:nrow(review_data)])
      
      plotdata <- time_data %>% gather(key = "Type", value = "Time", -Day) 
      ggp <- ggplot(plotdata, aes(x=Day, y=Time, color=Type)) + 
        geom_line(lwd = 0.8) + ylab("Time (min)") + 
        ggtitle(sprintf("Projected Time Commitment (News + Reviews) Over Time (rescale factor = %s)", signif(rescale,2))) + theme_bw() 
    }
    ggp
  })
  
  output$img <- renderImage({
    return(list(
      src = "img/instructions.png",
      contentType = "image/png",
      alt = "Export"
    ))
  }, deleteFile = FALSE)
  
  
}

# Run the app ----
shinyApp(ui, server)