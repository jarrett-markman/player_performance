# Player shiny app for atp/wta players performances w line plots for wins & losses
# Libraries
library(tidyverse)
library(shiny)
# Read in data and create a variable for league
atp <- read_csv("atp_matches_2023.csv") %>%
  mutate(league = "ATP")
wta <- read_csv("wta_matches_2023.csv") %>%
  mutate(league = "WTA")
# Combine df's for aggregating
data <- bind_rows(atp, wta)
# Get performance data based on columns for winners
winners <- data %>%
  select( # Select and rename certain columns
    tourney_id, tourney_level, tourney_date, tourney_name,
    player_id = winner_id,
    seed = winner_seed,
    player_entry = winner_entry,
    name = winner_name,
    hand = winner_hand,
    height = winner_ht,
    country = winner_ioc,
    age = winner_age,
    aces = w_ace,
    dfs = w_df,
    first_serve_points = w_svpt,
    first_in = w_1stIn,
    first_won = w_1stWon,
    second_won = w_2ndWon,
    serve_games = w_SvGms,
    break_points_saved = w_bpSaved,
    break_points = w_bpFaced,
    rank = winner_rank,
    rank_points = winner_rank_points,
    opp_hand = loser_hand,
    opp_height = loser_ht,
    opp_country = loser_ioc,
    opp_age = loser_age,
    opp_aces = l_ace,
    opp_dfs = l_df,
    opp_first_serve_points = l_svpt,
    opp_first_in = l_1stIn,
    opp_first_won = l_1stWon,
    opp_second_won = l_2ndWon,
    opp_serve_games = l_SvGms,
    opp_break_points_saved = l_bpSaved,
    opp_break_points = l_bpFaced,
    opp_rank = loser_rank,
    opp_rank_points = loser_rank_points,
    league, surface
  ) %>%
  mutate(
    result = "win" # Create a column for "win" - important later
  )
losers <- data %>%
  select( # Repeat process for the losers
    tourney_id, tourney_level, tourney_date, tourney_name,
    opp_hand = winner_hand,
    opp_height = winner_ht,
    opp_country = winner_ioc,
    opp_age = winner_age,
    opp_aces = w_ace,
    opp_dfs = w_df,
    opp_first_serve_points = w_svpt,
    opp_first_in = w_1stIn,
    opp_first_won = w_1stWon,
    opp_second_won = w_2ndWon,
    opp_serve_games = w_SvGms,
    opp_break_points_saved = w_bpSaved,
    opp_break_points = w_bpFaced,
    opp_rank = winner_rank,
    opp_rank_points = winner_rank_points,
    player_id = loser_id,
    seed = loser_seed,
    player_entry = loser_entry,
    name = loser_name,
    hand = loser_hand,
    height = loser_ht,
    country = loser_ioc,
    age = loser_age,
    aces = l_ace,
    dfs = l_df,
    first_serve_points = l_svpt,
    first_in = l_1stIn,
    first_won = l_1stWon,
    second_won = l_2ndWon,
    serve_games = l_SvGms,
    break_points_saved = l_bpSaved,
    break_points = l_bpFaced,
    rank = loser_rank,
    rank_points = loser_rank_points,
    league, surface
  ) %>%
  mutate(
    result = "loss" # Create a result = loss column for losers - important later
  )
# Combine winners and losers into one data frame
df <- bind_rows(winners, losers)
plot <- df %>%
  na.omit() %>% # Remove all NAs
  group_by(name, result, league, tourney_id, tourney_date) %>% # Group by player, match result, league and each tournament 
  summarise( # Summarise columns in the data set
    tourney_id, surface, 
    # Change the tourney_level column based on the level of tournament e.g., G -> Grand Slams
    tourney_level = ifelse(tourney_level == "G", "Grand Slams", tourney_level),
    tourney_level = ifelse(tourney_level == "M", "Masters 1000s", tourney_level),
    tourney_level = ifelse(tourney_level == "C", "Challengers", tourney_level),
    tourney_level = ifelse(tourney_level == "S", "ITFs", tourney_level),
    tourney_level = ifelse(tourney_level == "F", "Tour Finals", tourney_level),
    tourney_level = ifelse(tourney_level == "D", "Davis Cup", tourney_level),
    tourney_level = ifelse(tourney_level == "P", "Premier", tourney_level),
    tourney_level = ifelse(tourney_level == "PM", "Premier Mandatory", tourney_level),
    tourney_level = ifelse(tourney_level == "I", "International", tourney_level),
    tourney_level = ifelse(tourney_level == "A", "Other Tour Events (250 & 500's)", tourney_level),
    tourney_name,
    opp_rank,
    "Opponent Age" = opp_age,
    "Opponent Height" = opp_height,
    Aces = aces,
    DFs = dfs, 
    # Calculate player rate stats
    "First Serve In Percentage" = (first_in/first_serve_points) * 100,
    "First Serve Win Percentage" = (first_won/first_in) * 100,
    "Second Serve In Percentage" = ((first_serve_points - first_in - dfs)/(first_serve_points - first_in)) * 100,    
    "Second Serve Win Percentage" = (second_won/(first_serve_points - first_in)) * 100,
    "Break Points Saved Percentage" = (break_points_saved/break_points) * 100,
    "First Serve Return Win Percentage" = 100 - ((opp_first_won/opp_first_in) * 100),
    "Second Serve Return Win Percentage" = 100 - ((opp_second_won/(opp_first_serve_points - opp_first_in)) * 100),
    "Break Points Converted Percentage" = 100 - ((opp_break_points_saved/opp_break_points) * 100),
    year = substr(tourney_date, 1, 4),
    month = substr(tourney_date, 5, 6),
    day = substr(tourney_date, 7, 8)
  ) %>%
  ungroup() %>%
  select(-tourney_date) %>%
  mutate(match_date = paste0(month, day, year), 
         match_date = as.Date(lubridate::mdy(match_date))) # Create a column that stores the data as a mdy object
# Order the data frame based on the time of year (Jan - Dec)
plot <- plot[order(plot$match_date, decreasing = TRUE), ] %>%
# Rename vars
  mutate("Opponent Rank" = as.numeric(opp_rank),
         Result = ifelse(result == "win", "Win", "Loss")) %>% 
  select(-c(result, opp_rank, tourney_id, day)) # Drop unimportant columns
# Change up variable names in the data frame
colnames(plot) <- c("Player", "League", "Surface", "Tournament Type", "Tournament", "Opponent Age",
                    "Opponent Height", "Aces", "Double Faults", 
                    "First Serve In Percentage", "First Serve Win Percentage",
                    "Second Serve In Percentage", "Second Serve Win Percentage", "Break Points Saved Percentage",
                    "First Serve Return Win Percentage", "Second Serve Return Win Percentage",
                    "Break Points Converted Percentage", "Year", "Month", "match_date", "Opponent Rank", "Result"
                  )
# Arrange players in alphabetic order and filter for each league (ATP & WTA)
atp_plot <- plot %>%
  filter(League == "ATP") %>%
  select(-League) %>% 
  arrange(Player)
wta_plot <- plot %>%
  filter(League == "WTA") %>%
  select(-League) %>% 
  arrange(Player)
# Create shiny app
ui <- fluidPage(
  # Title shinyApp
  titlePanel("Evaluating Statistical Performance in Wins and Losses in the ATP and WTA"),
  tabsetPanel(
    id = "mainTab",
    # Create a home page that displays and image for atp and wta
    # Order the images set them off to the left/right
    tabPanel(
      "Home Page",
      fluidRow(
        column(6,
               actionLink("atp_link", # Hyperink the object
                          tags$img(src="https://upload.wikimedia.org/wikipedia/en/thumb/3/3f/ATP_Tour_logo.svg/1200px-ATP_Tour_logo.svg.png", 
                                   style="max-width: 100%; max-height: 100%; margin-top: 50px;"))
        ),
        column(6,
               actionLink("wta_link", # Hyperlink the object
                          tags$img(src="https://upload.wikimedia.org/wikipedia/en/thumb/5/5f/Women%27s_Tennis_Association_logo_%282020%29.svg/1200px-Women%27s_Tennis_Association_logo_%282020%29.svg.png", 
                                   style="max-width: 100%; max-height: 100%; margin-top: 50px;"))
      )
    )
  ) ,
  # Create an ATP tabPanel
  tabPanel(
    "ATP",
    id = "ATP",
    # Create inputs for graphs (player, stat, filters)
    sidebarPanel(
      selectInput("player_atp", "Player: ", choices = unique(atp_plot$Player)),
      selectInput("stat_atp", "Stat: ", choices = colnames(atp_plot[-c(1:4, 17, 19, 21)])),
      checkboxGroupInput("tournament_atp", "Tournament Type:", choices = unique(atp_plot$`Tournament Type`), selected = unique(atp_plot$`Tournament Type`)),
      checkboxGroupInput("surface_atp", "Surface: ", choices = unique(atp_plot$Surface), selected = unique(atp_plot$Surface)),
      actionButton("submit_atp", "Submit")
    ),
    mainPanel(
      plotOutput("plot_atp") # Display the plot
    )
  ),
    # Repeat inputs for WTA
  tabPanel(
    "WTA",
    id = "WTA",
    sidebarPanel(
      selectInput("player_wta", "Player: ", choices = unique(wta_plot$Player)),
      selectInput("stat_wta", "Stat: ", choices = colnames(wta_plot[-c(1:4, 17, 19, 21)])),
      checkboxGroupInput("tournament_wta", "Tournament Type:", choices = unique(wta_plot$`Tournament Type`), selected = unique(wta_plot$`Tournament Type`)),
      checkboxGroupInput("surface_wta", "Surface: ", choices = unique(wta_plot$Surface), selected = unique(wta_plot$Surface)),
      actionButton("submit_wta", "Submit")
    ),
    mainPanel(
      plotOutput("plot_wta")
    )
  ),
  tabPanel(
    "Sources",
    id = "Sources",
    # Cited source
    h1("All the data used is collected from ", 
       tags$a("Jeff Sackman's GitHub", href = "https://github.com/JeffSackmann"), 
       ".")
  )
))
# Create server
server <- function(input, output, session) {

  # Observe the event the atp/wta photo is clicked
  observeEvent(input$atp_link, {
    updateTabsetPanel(session, "mainTab", selected = "ATP")
  })
  
  observeEvent(input$wta_link, {
    updateTabsetPanel(session, "mainTab", selected = "WTA")
  })

  # Set df's as reactiveVal's
  atp_plot <- reactiveVal(atp_plot)
  wta_plot <- reactiveVal(wta_plot)
  
  observeEvent(input$submit_atp, { # If the submit button is pressed
    
    plot <- atp_plot() %>%
    # Filter the user inputs
      filter(Player == input$player_atp &
               `Tournament Type` %in% input$tournament_atp &
               Surface %in% input$surface_atp) %>%
    # Make a line and dot plot
      ggplot(aes(x = match_date, y = .data[[input$stat_atp]] , color = Result)) + # Get the stat input & scale color of line via result
      geom_point(aes(size = `Opponent Rank`)) + # Scale the dots based on opponent's atp/wta rank
      geom_line() +
      labs(
        x = "",
        title = glue::glue("{input$player_atp} {gsub('`', '', input$stat_atp)} in Wins and Losses Over the 2023 Season")
        # Use glue to create a title that displays the player and the stat chose
        # gsub command removes the ` from the display
      ) +
      scale_colour_manual(values = c("red2", "green2")) + # Change color scale
    # Edit plot display and backgroun
      theme_bw() +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "#002865"),
        axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()
      )
    
    output$plot_atp <- renderPlot({
      plot # Display plot
    })
  })

  # Repeat observeEvent for atp with wta tab
  observeEvent(input$submit_wta, {
    
    plot <- wta_plot() %>%
      filter(Player == input$player_wta &
               `Tournament Type` %in% input$tournament_wta &
               Surface %in% input$surface_wta) %>%
      ggplot(aes(x = match_date, y = .data[[input$stat_wta]], color = Result)) +
      geom_point(aes(size = `Opponent Rank`)) +
      geom_line() +
      labs(
        x = "",
        title = glue::glue("{input$player_wta} {gsub('`', '', input$stat_wta)} in Wins and Losses Over the 2023 Season")
      ) +
      scale_colour_manual(values = c("red2", "green2")) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "#7814FF"), # Set hex code for wta logo as background
        axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()
      )
    
    output$plot_wta <- renderPlot({
      plot
    })
  })
}

shinyApp(ui, server) # Display the shinyApp
