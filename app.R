# title: "Data Exploration and Visualization Tools for Anki"
# author: "James Diao"
# date: "13 April 2019"

# web app: https://jamesdiao.shinyapps.io/ankidata/
# rsconnect::deployApp('/Users/jamesdiao/Documents/R/Anki-Data')

### To-do

# download function for: tables, plots, files
# add predictive modeling tab
# explanation + math for suggested interval
# NLP on words

### Code

# Install and load all required packages
pkg_list <- c("dplyr","tidyr","ggplot2","rjson","RSQLite", "DBI","anytime","scales",
              "sqldf", "treemap", "plotly","shiny","shinycssloaders","shinyalert")
installed <- pkg_list %in% installed.packages()[,"Package"]
if (!all(installed)) install.packages(pkg_list[!installed])
sapply(pkg_list, require, character.only = T)

require(dplyr)
require(tidyr)
require(ggplot2)
require(rjson)
require(RSQLite)
require(DBI)
require(anytime)
require(sqldf)
require(treemap)
require(plotly)
require(scales)
require(shiny)
require(shinycssloaders)
require(shinyalert)

# Maximum upload size
options(shiny.maxRequestSize=50*1024^2)

ui <- fluidPage(
  useShinyalert(),
  titlePanel("Data Exploration and Visualization Tools for Anki"),
  
  

    column(4,
        wellPanel(
          fileInput("file1", "Upload 'collection.anki2' file (50 MB limit)",
                    multiple = FALSE),
          actionButton(inputId = "autofile", label = "Try with Test File", 
                       icon("paper-plane"), 
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          h1(),
          selectizeInput("rm_decks", "Decks to exclude:", 
                         choices = c("File Not Found"), selected = c("File Not Found"), multiple = TRUE),
          dateInput("ignore_before", "Ignore cards before (defaults to first day):"),
          h2(),
          tableOutput("stats_table") %>% withSpinner
        ),
        wellPanel(
        tags$div(
          tags$b("Web app:"), tags$a(href="https://jamesdiao.shinyapps.io/anki-data/", 
                                     "https://jamesdiao.shinyapps.io/anki-data/"),
          tags$br(),
          tags$b("Source code:"), tags$a(href="https://github.com/jamesdiao/Anki-Data/", 
                                         "https://github.com/jamesdiao/Anki-Data/"),
          tags$br(),
          tags$b("Last updated:"), "April 13, 2019"
          )
        )
    ),
    
    column(8,
      tabsetPanel(
        tabPanel("Upload", 
          column(5,
           tags$div(
             tags$br(),
             tags$b('Instructions: '), tags$br(),
             'Upload a ',tags$code('collection.anki2'), 'file (details below) or hit ',
             '"Try with Test File" to explore the app using preloaded sample data.',
             tags$br(), tags$br(),
             tags$b('Details: '), tags$br(), 
             'This app extracts scheduling data from a',tags$code('collection.anki2'),
             'file. Start by exporting your Anki collection as a ', 
             tags$code('*.apkg'), 'or ', tags$code('*.colpkg'),
             ' file. Rename the file to *.zip and unzip it ',
             'to find ',tags$code('collection.anki2')
          )),
          column(3,tags$br(),
                 imageOutput("img"))
        ),
        tabPanel("Treemap", h1(), 
                 radioButtons(inputId = "tm_type", label = "Divisions", inline = TRUE,
                              choices = c("All categories","Learned/unlearned")),
                 plotOutput("treemap", height = "500px") %>% withSpinner
                 ),
        tabPanel("Over Time", h1(), 
                 #radioButtons(inputId = "ot_type", label = "New vs. Review", inline = TRUE,
                #              choices = c("New Cards","Review Cards")),
                 radioButtons(inputId = "ot_output", label = "Count vs. Time", inline = TRUE,
                              choices = c("Card Count","Time Spent")),
                 plotOutput("newplot", height = "250px") %>% withSpinner,
                 plotOutput("revplot", height = "250px") %>% withSpinner
                 ),
        tabPanel("Projection (beta)", h1(), 
                 #h5("Predictive model generated from empirical values"),
                 radioButtons(inputId = "pr_output", label = "Review Count vs. Time", inline = TRUE,
                              choices = c("Review Card Count","Review Time")),
                 sliderInput(inputId = "span", label = "Smoothing factor", 
                             min = 0.1, max=1, value = 0.5, step = 0.1),
                 #textOutput("params") %>% withSpinner, 
                 h2(),
                 plotOutput("projection") %>% withSpinner
                 )
      )
    )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(IGNORE_BEFORE = NULL)
  
  observeEvent(input$autofile, {
    rv$COLLECTION_PATH <- "Files/JAD/collection.anki2"
    fromCon()
    adjustDate()
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
    #if (grepl("\\.[a-z]+pkg$", rv$COLLECTION_PATH)) {
    #  unzip(input$file1$datapath, exdir = "./Files/temp")
    #  rv$COLLECTION_PATH <- paste(getwd(),"collection.anki2",sep="/")
    #}
    if (!grepl("\\.anki2$", rv$COLLECTION_PATH)) { #& !grepl("\\.[a-z]+pkg$", rv$COLLECTION_PATH)) {
        shinyalert("Oops!", 'Please upload a "collection.anki2" file', type = "error") #"*.apkg", "*.colpkg", or 
        rv$COLLECTION_PATH <- NULL
    } else {
      fromCon()
      adjustDate()
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
    fromCon()
    getData()
  }, ignoreNULL = FALSE)
  
  observeEvent(input$ignore_before, {
    req(rv$COLLECTION_PATH)
    fromCon()
    getData()
  }, ignoreNULL = FALSE)
  
  adjustDate <- reactive({
    revdates <- rv$rev$revdate %>% unique %>% sort
    rv$IGNORE_BEFORE <- min(revdates)
    gaps <- revdates %>% diff %>% as.numeric
    if (max(gaps) > 6*30)
      rv$IGNORE_BEFORE <- revdates[which.max(gaps)+1]
    updateDateInput(session, "ignore_before",
                    value = rv$IGNORE_BEFORE, min = min(revdates), max = Sys.Date()
    )
  })
  
  fromCon <- reactive({
    con <- dbConnect(RSQLite::SQLite(), dbname=rv$COLLECTION_PATH)
    rv$rev <- dbGetQuery(con,'SELECT CAST(id AS TEXT) AS id, CAST(cid AS TEXT) AS cid,
                         time, type, ease, factor, ivl, lastivl FROM revlog') 
    rv$rev$revdate <- anydate(as.numeric(rv$rev$id)/1000 - 5*3600)
    rv$cards <- dbGetQuery(con,'SELECT CAST(id AS TEXT) AS cid, 
                           CAST(did AS TEXT) AS did,
                           reps,
                           mod
                           FROM cards')
    rv$deckinfo <- dbGetQuery(con,'SELECT decks FROM col') %>% as.character %>% fromJSON
    rv$decks <- data.frame(did = names(rv$deckinfo), 
                           name = sapply(rv$deckinfo, function(d) d$name) %>% setNames(NULL), 
                           stringsAsFactors = FALSE
    )
    dbDisconnect(con)
  })
  
  getData <- reactive({
    RM_DECKS <- input$rm_decks
    decks_cat <- rv$decks
    decks_cat$category <- gsub("::.*$","",decks_cat$name) %>% trimws
    decks_cat$subcategory <- sub("::",";;",decks_cat$name)
    decks_cat$subcategory <- sub(".*;;","",decks_cat$subcategory)
    decks_cat$subcategory <- gsub(":.*$","",decks_cat$subcategory) %>% trimws
    rv$decks_cat <- decks_cat
    rv$cards_w_decks <- merge(rv$cards, rv$decks,by="did")
    if (input$ignore_before != Sys.Date()){
      rv$IGNORE_BEFORE <- input$ignore_before
    }
    rv$rev <- rv$rev %>% filter(revdate >= rv$IGNORE_BEFORE)#-10)
    # Assign deck info to reviews
    rev_w_decks <- merge(rv$rev, rv$cards_w_decks, by="cid", all.x = TRUE)
    
    keep <- rep(TRUE, nrow(rev_w_decks))
    if (!is.null(RM_DECKS)) {
      keep <- sapply(rev_w_decks$name, function(n){
        !any(sapply(RM_DECKS, function(x) grepl(x, n, ignore.case = TRUE)))
      }) %>% setNames(NULL)
    }
    rev_w_decks <- rev_w_decks %>% filter(keep)
    rv$rev_w_decks <- rev_w_decks
    
    rv$new_cards <- rev_w_decks %>% 
      filter(ivl > 0, lastIvl < 0) %>% 
      arrange(cid, id) %>% 
      distinct(cid, .keep_all = TRUE) %>% 
      group_by(Date=revdate) %>% 
      summarize(New_Count=n())
    
    rv$new_time <- rev_w_decks %>% 
      filter((type == 0) | (ivl > 0 & lastIvl < 0)) %>% 
      arrange(cid, id) %>% 
      group_by(Date=revdate) %>% 
      summarize(New_Time=sum(time/60000))
    
    rv$all_summary <- merge(rv$new_cards, rv$new_time, "Date", all = TRUE)
    
    # Not new 
    rv$rev_summary <- rev_w_decks %>% 
      filter(type != 0) %>% 
      arrange(cid, id) %>% 
      group_by(Date=revdate) %>% 
      summarize(Rev_Count=n(), Rev_Time=sum(time/60000))
    
    rv$all_summary <- merge(rv$rev_summary, rv$all_summary, "Date", all = TRUE)
    
    rv$error_summary <- rev_w_decks %>% 
      filter(ease==1) %>% 
      arrange(cid, id) %>% 
      group_by(Date=revdate) %>% 
      summarize(Error_Count=n())
    
    rv$all_summary <- merge(rv$all_summary, rv$error_summary, "Date", all = TRUE)
    
    rv$total_summary <- rev_w_decks %>% 
      arrange(cid, id) %>% 
      group_by(Date=revdate) %>% 
      summarize(Total_Count=n())
    
    rv$all_summary <- merge(rv$all_summary, rv$total_summary, "Date", all = TRUE)
    
    rv$all_summary[,-1] <- sapply(rv$all_summary[,-1], function(col) 
      replace(col, is.na(col), 0)
    )
    
    rv$dates <- rv$all_summary$Date
    num_days <- rv$dates %>% range %>% diff %>% as.numeric
    
    
    rv$error_rates <- rv$all_summary$Error_Count/rv$all_summary$Total_Count
    rv$avg_error <- sum(rv$all_summary$Error_Count)/sum(rv$all_summary$Total_Count)
    
    #take non-trailing <4s
    real_news <- nrow(rv$all_summary) - which(diff(rev(rv$all_summary$New_Count < 4))==-1)[1]
    rv$avg_new <- sum(rv$all_summary$New_Count[1:real_news])/real_news
    #rv$avg_new <- sum(rv$all_summary$New_Count[-length(rv$dates)])/(num_days-1)
    
    rv$new_min_per_card <- sum(rv$all_summary$New_Time[-length(rv$dates)]) / 
      sum(rv$all_summary$New_Count[-length(rv$dates)])
    rv$avg_rev <- sum(rv$all_summary$Rev_Count[-length(rv$dates)])/(num_days-1)
    rv$rev_min_per_card <- sum(rv$all_summary$Rev_Time[-length(rv$dates)]) / 
      sum(rv$all_summary$Rev_Count[-length(rv$dates)])
    rv$avg_int <- mean((rv$rev$factor[rv$rev$factor > 0]))/1000
    rv$new_int <- rv$avg_int*log(0.9)/log(1-max(rv$avg_error,0.01))
    rv$totaldaystocompletion <- ceiling(nrow(rv$cards)/rv$avg_new)
    rv$daystocompletion <- rv$totaldaystocompletion-num_days
    rv$completiondate <- ceiling(nrow(rv$cards)/rv$avg_new)-num_days+Sys.Date()
    
    cards_w_categories <- merge(rv$cards,rv$decks_cat,by="did") 
    keep <- TRUE
    if (!is.null(RM_DECKS)) {
      keep <- sapply(cards_w_categories$name, function(n){
        !any(sapply(RM_DECKS, function(x) grepl(x, n)))
      }) %>% setNames(NULL)
    }
    rv$cards_w_categories <- cards_w_categories %>% filter(keep)
  })
  
  
  
  output$stats_table <- renderTable(
    colnames = TRUE, rownames = FALSE, {
    req(rv$COLLECTION_PATH)
    tab <- data.frame(
      total_cards = nrow(rv$cards),
      avg_new = rv$avg_new %>% round(1),
      avg_rev = rv$avg_rev %>% round(1),
      avg_error = sprintf("%s%%", 100*rv$avg_error %>% signif(3)),
      #avg_int = sprintf("%sx", rv$avg_int %>% round(2)),
      #new_int = sprintf("%sx", rv$new_int %>% round(2)),
      completiondate = rv$completiondate %>% format("%b %d, %Y"),
      daystocompletion = sprintf("%s/%s", 
                                 rv$daystocompletion %>% round(1), 
                                 rv$totaldaystocompletion %>% round(1)
                                 )
    ) %>% t
    
    data.frame(c("Total Cards",
                 "Average New Cards/Day",
                 "Average Review Cards/Day",
                 "Average Error Rate",
                 #"Average Interval",
                 #"Suggested Base Interval (targeting 90% error)", 
                 "Estimated Completion Date",
                 "Days to Completion (Remaining/Total)"),
               tab
    ) -> tab
    colnames(tab) <- c("Summary Table","")
    
    #tab <- rv$all_summary[,-1]; rownames(tab) <- rv$dates
    return(tab)
    
  })
  
  #output$summary_table <- renderDataTable({
  #    req(rv$COLLECTION_PATH)
  #    return(rv$all_summary)
  #})
  
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
  
  output$newplot <- renderPlot({
    req(rv$COLLECTION_PATH)
    if (input$ot_output == "Card Count") {
      ggplot(rv$all_summary,aes(x=Date,y=New_Count)) + 
        geom_bar(stat="identity",fill="darkgreen") +
        ggtitle("New Card Count Over Time") +
        xlab("Date") +
        ylab("New Cards") +
        geom_hline(yintercept = rv$avg_new, lty = 2)
    } else {
      ggplot(rv$all_summary,aes(x=Date,y=New_Time)) + 
        geom_bar(stat="identity",fill="darkgreen") +
        ggtitle("Time Spent on New Cards Over Time") +
        xlab("Date") +
        ylab("Time (min)") +
        geom_hline(yintercept = mean(rv$all_summary$New_Time), lty = 2)
    }
  })
  
  output$revplot <- renderPlot({
    req(rv$COLLECTION_PATH)
    if (input$ot_output == "Card Count") {
      ggplot(rv$all_summary,aes(x=Date,y=Rev_Count)) + 
        geom_bar(stat="identity",fill="darkblue") +
        #geom_smooth(method='auto') + 
        ggtitle("Review Card Count Over Time") +
        xlab("Date") +
        ylab("Review Cards") +
        geom_hline(yintercept = rv$avg_rev, lty = 2)
    } else {
      ggplot(rv$all_summary,aes(x=Date,y=Rev_Time)) + 
        geom_bar(stat="identity",fill="darkblue") +
        #geom_smooth(method='auto') + 
        ggtitle("Time Spent on Review Cards Over Time") +
        xlab("Date") +
        ylab("Time (min)") +
        geom_hline(yintercept = mean(rv$all_summary$Rev_Time), lty = 2)
    }
  })
  
  #output$params <- renderText({
  #  req(rv$COLLECTION_PATH)
  #  sprintf("New cards/day = %s, Total cards = %s, Error rate = %s%%, Multiplier = %sx", 
  #          rv$avg_new %>% round(1), nrow(rv$cards), round(100*rv$avg_error, 1), rv$avg_int %>% round(2)
  #          )
  #})
  
  output$projection <- renderPlot({
    req(rv$COLLECTION_PATH)
    span <- input$span
    RM_DECKS <- input$rm_decks
    rev <- rv$rev
    cards <- rv$cards
    cards_w_categories <- rv$cards_w_categories
    
    all_deck_summary <- sqldf("SELECT category, subcategory, count(*) AS n_cards 
                              FROM cards_w_categories 
                              GROUP BY category, subcategory")
    
    avg_error <- rv$avg_error
    avg_new = rv$avg_new
    new_min_per_card <- rv$new_min_per_card
    avg_rev <- rv$avg_rev
    rev_min_per_card <- rv$rev_min_per_card
    avg_int <- rv$avg_int
    new_int <- rv$new_int
    
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
    review_data <- data.frame(Day = 1:length(reviews), Predicted = reviews)
    smoothed_1 <- reviews[1:4]
    smoothed_2 <- predict(loess(Predicted ~ Day, 
                                data = review_data[5:nrow(review_data),], 
                                span = 0.1))
    smoothed <- c(smoothed_1, smoothed_2)
    use_dates <- nrow(review_data)>150
    
    
    if (input$pr_output == "Review Card Count") {
      # Final data frame
      obs <- rv$all_summary$Rev_Count
      obs_data <- data.frame(Day = 1:length(obs), Predicted = obs)
      obs_smoothed <- predict(loess(Predicted ~ Day, span = span, data = obs_data))
      
      f <- function(x) {
        min_len <- min(length(smoothed), length(obs_smoothed))
        sum((smoothed[1:min_len]*x - obs_smoothed[1:min_len])^2)
      }
      rescale <- optimize(f=f, interval = c(0,2))$minimum
      all_data <- data.frame(Day = review_data$Day,
                             Predicted = rescale * review_data$Predicted,
                             Observed = obs_smoothed[1:nrow(review_data)])
      if (use_dates) {
        all_data <- all_data %>% mutate(Day = as.POSIXct(rv$IGNORE_BEFORE + Day))
      }
      plotdata <- all_data %>% gather(key = "Type", value = "Reviews", -Day) 
      ggp <- ggplot(plotdata, aes(x=Day, y=Reviews, color=Type)) + 
        ggtitle(sprintf("Projected Review Counts Over Time (rescale factor = %s%%)", 
                        signif(100*rescale-100,2))) + 
        geom_line(lwd = 0.8) + theme_bw() 
      
    } else {
      obs_time <- rv$all_summary$New_Time + rv$all_summary$Rev_Time
      obs_time_data <- data.frame(Day = 1:length(obs_time), Predicted = obs_time)
      obs_time_smoothed <- predict(loess(Predicted ~ Day, span = span, data = obs_time_data))
      
      smoothed_time <- review_data*rev_min_per_card+cardsperday*new_min_per_card
      f <- function(x) {
        min_len <- min(nrow(smoothed_time), length(obs_time_smoothed))
        sum((smoothed_time[1:min_len, "Predicted"]*x -
               obs_time_smoothed[1:min_len])^2)
      }
      rescale <- optimize(f=f, interval = c(0,2))$minimum
      
      
      time_data <- data.frame(Day = review_data$Day,
                              Predicted = rescale*(review_data$Predicted*rev_min_per_card + 
                                                   cardsperday*new_min_per_card), 
                              Observed = obs_time_smoothed[1:nrow(review_data)])
      if (use_dates) {
        time_data <- time_data %>% mutate(Day = as.POSIXct(rv$IGNORE_BEFORE + Day))
      }
      plotdata <- time_data %>% gather(key = "Type", value = "Time", -Day) 
      ggp <- ggplot(plotdata, aes(x=Day, y=Time, color=Type)) + 
        geom_line(lwd = 0.8) + ylab("Time (min)") + 
        ggtitle(sprintf("Projected Time Commitment (News + Reviews) Over Time (rescale factor = %s%%)", 
                        signif(100*rescale-100,2))) + theme_bw() 
    }
    if (use_dates) {
      ggp <- ggp + scale_x_datetime(labels = date_format("%b %Y"), 
                                    minor_breaks = date_breaks("months"),
                                    breaks = date_breaks("months")) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggp
  })
  
  output$img <- renderImage({
    return(list(
      src = "img/instructions.png",
      width = 330,
      height = 340,
      contentType = "image/png",
      alt = "Export"
    ))
  }, deleteFile = FALSE)
  
  
}

# Run the app ----
shinyApp(ui, server)