library(tidyverse)
library(readxl)
library(glue)
library(cubing)
library(shinyjs)
library(shinyWidgets)
library(googledrive)
library(googlesheets4)
library(vroom)
library(lubridate)
library(magick)
library(keys)
library(shinythemes)
library(reactable)
library(bslib)
library(dipsaus)
library(viridis)
library(plotly)
library(thematic)
library(reactablefmtr)
library(sparkline)
library(shinycssloaders)
library(kit)

# Re-Read Project-Level Environmental Variables
readRenviron(".Renviron")
gs4_e <- Sys.getenv("gs4_e")

# library(Rfast) # for fastest theoretical

# # Code to generate cases.csv and pll.csv
# # Alg is in standard notation, while Alg2 converts S' to B'Fz' and S to BF'z for cubing() to work
# # Cubing Package: "In particular, the S slice direction is different to what you
# # may find elsewhere; the definition used for S in this package is consistent
# # with the rotation directions." (?slice)
# PLL <- tibble(
#   PLL = c(
#     "Aa", "Ab", "E", "F", "Ga", "Gb", "Gc",
#     "Gd", "H", "Ja", "Jb", "Na", "Nb", "Ra", "Rb", "T", "Ua", "Ub",
#     "V", "Y", "Z"
#   ), Alg = c(
#     "(R' D' R) U2' (R' D R) U' (R' D' R) U' (R' D R)",
#     "(R' D' R) U (R' D R) U (R' D' R) U2 (R' D R)", "x' (R U' R' D) (R U R' D') (R U R' D) (R U' R' D') x",
#     "R' F R f' R' F R2 U R' U' R' F' R2 U R' S", "R2 U (R' U R' U') (R U' R2) D U' (R' U R D')",
#     "R' U' R U D' R2 U R' U R U' R U' R2' D", "R2 F2 R U2 R U2 R' F R U R' U' R' F R2",
#     "R U R' U' D R2 U' R U' R' U R' U R2 D'", "(M2' U M2') U2 (M2' U M2')",
#     "x R2 F R F' R U2 r' U r U2 x'", "(R U R' F') (R U R' U') R' F R2 U' R'",
#     "(R U R' U) (R U R' F') (R U R' U') (R' F R2 U') R' U2 (R U' R')",
#     "r' D' F (r U' r') F' D (r2 U r' U') (r' F r F')", "(R U' R' U') (R U R' U) (R' D' R U' R' D) (R2 U R')",
#     "R' U2 R' D' R U' R' D R U R U' R' U' R", "(R U R' U') (R' F R2 U') R' U' (R U R' F')",
#     "M2' U M U2' M' U M2'", "R2 U (R U R' U') R' U' (R' U R')", "(R' U R' U') y (R' F' R2 U') (R' U R' F) R F",
#     "F (R U' R' U') (R U R' F') (R U R' U') (R' F R F')", "M' U' M2' U' M2' U' M' U2' M2'"
#   )) %>%
#   mutate(Alg2 = str_replace_all(Alg, " ", ""),
#          Alg2 = str_replace_all(Alg2, "\\(|\\)", ""),
#          Alg2 = str_replace_all(Alg2, "S'", "B'Fz'"),
#          Alg2 = str_replace_all(Alg2, "S", "BF'z")) %>%
#   arrange(PLL)
#
# # Generate all combinations
# # Get every AUF solution (without offset)
# PreRot <- c("", "y", "y2", "y'") # doing the alg, doing the preauf, then rotating, starting with y'
# PreAUF <- c("", "U", "U2", "U'") # doing the alg then doing a move, startign with U', then U2'
# PostRot <- c("", "y", "y2", "y'") # this is rotating the cube before the case is setup (so it's really pre, and pre is post)
# CN <- c("", "x", "x2", "x'", "z", "z'") # white, blue, yellow, green, orange, red.. this is rotating the cube before anything else
# AUF <- expand_grid(PreRot, PreAUF, PostRot, CN) %>%
#   mutate(cross_color = case_when(
#     CN == "" ~ "white",
#     CN == "x" ~ "blue",
#     CN == "x2" ~ "yellow",
#     CN == "x'" ~ "green",
#     CN == "z" ~ "orange",
#     CN == "z'" ~ "red"))
# # moves is used for VisualCube, however invmoves is used to setup cases in
# # the cubing package, so S' and S have to be inverted or rewritten
# tsauf <- expand_grid(PLL, AUF) %>%
#   group_by(PLL) %>%
#   mutate(n = str_pad(row_number(), 2, pad = "0")) %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(moves = paste0(PreRot, PreAUF, Alg2, PostRot, CN)) %>%
#   mutate(CaseURL = paste0("visualcube.php?size=175&case=", moves)) %>%
#   mutate(invmoves = cubing::invMoves(moves, collapse = "")) %>%
#   mutate(CaseURL = str_replace_all(CaseURL, "'", "\\\\'")) %>% # Necessary for string insertion in VisualCube TS: this changes ' to \' (https://github.com/STAT545-UBC/Discussion/issues/394)
#   ungroup()
# cases <- tsauf %>%
#   select(PLL, invmoves, cross_color, TruncatedCaseURL = CaseURL)
# # Write to csv
# write_csv(cases, "cases.csv")
# write_csv(PLL, "pll.csv")

# Read data
cases <- vroom("cases.csv", show_col_types = F)
PLL <- vroom("pll.csv", show_col_types = F)

# Tips
tips <- c(
  "Tip: Input your name in settings to save your time!",
  "Tip: Use keyboard shortcuts for faster times!",
  "Tip: You can use 'enter' to start/stop the game."
)

# Function for shinycssloader
spin <- function(output) {
  shinycssloaders::withSpinner(output, color = "#7ad151", type = 1, size = 0.5)
}

# Function for splitting times and slowest and fastest PLL times
# Test Data: 83.71; # slowest T (9.91), Ja (7.16), Gb (6.38); fastest Z (1.61), H (1.64), V (1.84)
# Test Data: TT <- "5.70372366905212,3.64470863342285,2.83904957771301,2.27361273765564,5.62283086776733,6.38461422920227,4.66104412078857,5.86293292045593,1.64193105697632,7.16274070739746,2.17631793022156,2.98303079605103,2.7285373210907,3.41716957092285,3.50837707519531,9.91375327110291,3.75311493873596,2.14541482925415,1.84352159500122,3.82638025283813,1.61478662490845"
get_topn <- function(v) {
  if (is.na(v)) {
    return(list(
      Slowest = NA,
      Fastest = NA,
      Recognition = NULL
    ))
  }
  v2 <- as.numeric(str_split(v, ",")[[1]])
  idx_slowest <- kit::topn(v2, n = 3L, decreasing = T)
  idx_fastest <- kit::topn(v2, n = 3L, decreasing = F)
  s1 <- PLL$PLL[idx_slowest]
  s2 <- round(v2[idx_slowest], 2)
  f1 <- PLL$PLL[idx_fastest]
  f2 <- round(v2[idx_fastest], 2)
  return(list(
    Slowest = paste0(s1[1], " (", s2[1], "), ", s1[2], " (", s2[2], "), ", s1[3], " (", s2[3], ")"),
    Fastest = paste0(f1[1], " (", f2[1], "), ", f1[2], " (", f2[2], "), ", f1[3], " (", f2[3], ")"),
    Recognition = round(v2, 2)
  ))
}

# get_fastest_theoretical_time <- function(name) {
#   if(!(name %in% df_records$Name)) return(NA)
#   tmp <- matrix(unlist(df_records %>% filter(Name == name) %>% pull(Recognition)), ncol = 21)
#   return(sum(Rfast::colMins(tmp, value = T)))
# }
# get_fastest_theoretical_time("Guest")


# Call thematic_shiny() prior to launching the app, to change
# R plot theming defaults for all the plots generated in the app
thematic_shiny(font = "Open Sans")

# https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets",
  googledrive_quiet = T
)

gs4_auth(cache = ".secrets", email = gs4_e)

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
sheet_id <- drive_get("tsauf_records")$id

# Function to get alg for key inputs
get_alg <- function(df, pll_name) {
  df %>%
    filter(PLL == pll_name) %>%
    pull(Alg2)
}

# Function for plot
get_best <- function(df, username, mode) {
  df %>%
    select(-Crown) %>%
    filter(Name == username) %>%
    filter(Mode == mode) %>%
    select(-Mode) %>%
    arrange(Time) %>%
    dplyr::slice(1) %>%
    filter(!is.na(CaseTimes)) %>%
    separate(CaseTimes, into = PLL$PLL, sep = ",") %>%
    pivot_longer(-c(Name:Time),
      names_to = "PLL", values_to = "Seconds",
      values_transform = list(Seconds = as.numeric)
    ) %>%
    mutate(
      PLL = as.factor(PLL),
      PLL = fct_reorder(PLL, Seconds, min, .desc = T),
      Seconds = round(Seconds, 2)
    )
}
get_number_one <- function(df, mode) {
  df %>%
    select(-Crown) %>%
    filter(Mode == mode) %>%
    select(-Mode) %>%
    arrange(Time) %>%
    dplyr::slice(1) %>%
    filter(!is.na(CaseTimes)) %>%
    separate(CaseTimes, into = PLL$PLL, sep = ",") %>%
    pivot_longer(-c(Name:Time),
      names_to = "PLL", values_to = "Seconds",
      values_transform = list(Seconds = as.numeric)
    ) %>%
    mutate(
      PLL = as.factor(PLL),
      PLL = fct_reorder(PLL, Seconds, min, .desc = T),
      Seconds = round(Seconds, 2)
    )
}
get_plot <- function(df, username, mode) {
  p_pll <- bind_rows(get_best(df, username, mode), get_number_one(df, mode)) %>%
    ggplot(aes(x = PLL, y = Seconds, color = Name, group = Name)) +
    geom_point() +
    geom_line() +
    scale_color_viridis_d(option = "viridis", begin = 0.5, end = 0.8) +
    theme(legend.position = "none") +
    labs(x = "", y = "Seconds")
  ggplotly(p_pll,
    tooltip = c("x", "y", "group")
  ) %>%
    config(displayModeBar = F)
}
# get_number_one(df_recs, "PLL-Only")

# Function for table
get_table <- function(df, mode) {
  mydf <- df %>%
    filter(Mode == mode) %>%
    select(-Mode, -Crown) %>%
    arrange(Time) %>%
    rowid_to_column(var = "Rank")
  reactable(
    data = mydf,
    theme = reactableTheme(backgroundColor = "transparent"),
    borderless = T,
    pagination = T,
    columns = list(
      Slowest = colDef(minWidth = 200),
      Fastest = colDef(minWidth = 200),
      Recognition = colDef(cell = function(value, index) {
        sparkline(mydf$Recognition[[index]], fillColor = "#2c3036", lineColor = "#7ad151", spotColor = F, minSpotColor = "#7ad151", maxSpotColor = "#7ad151", disableInteraction = F)
      }),
      `Date (UTC)` = colDef(minWidth = 200)
    )
  )
}

library(shiny)

ui <-
  navbarPage("PLL Recognition Time Attack",
    id = "tabSwitch", fluid = T,
    theme = bs_theme(bootswatch = "slate"),
    tabPanel("",
      icon = icon("cube"), value = "Home", align = "center",

      # Include shinyjs and js libraries for VisualCube TS
      useShinyjs(),
      tags$script(src = "svg.min.js"),
      tags$script(src = "srVisualizer.min.js"),

      # Include javascript dependencies for keys
      useKeys(),
      keysInput(inputId = "keys", keys = c(
        "a a", "a b", "e", "f", "g a", "g b", "g c", "g d", "h", "j a", "j b", "n a", "n b",
        "r a", "r b", "t", "u a", "u b", "v", "y", "z", "A a", "A b", "E", "F", "G a", "G b",
        "G c", "G d", "H", "J a", "J b", "N a", "N b", "R a", "R b", "T", "U a", "U b", "V",
        "Y", "Z", "1", "2", "3", "4", "7", "8", "9", "0", "enter"
      ), global = F),
      br(),

      # Start button
      # shinyWidgets::actionBttn(inputId = "Start", label = "Start", style = "simple", color = "primary"),
      dipsaus::actionButtonStyled(inputId = "Start", label = "Start", type = "primary"),
      br(),
      br(),

      # Show if the solution was correct or not
      textOutput("output_final_time"),

      # Where the PLL case shows up
      HTML('<div id="visualcube" style="height: 175px;"></div>'),
      br(),

      # PLL Button Group
      shinyWidgets::radioGroupButtons(
        inputId = "pll_choice",
        choiceNames = PLL %>% pull(PLL),
        choiceValues = PLL %>% pull(Alg2),
        status = "outline-success",
        selected = character(0),
        size = "lg",
        individual = T
      ),

      # Pre-AUF
      shinyWidgets::radioGroupButtons(
        inputId = "pre_auf_choice",
        label = "Pre-AUF",
        choiceNames = c("None", "U", "U'", "U2/U2'"),
        choiceValues = c("UU'", "U", "U'", "U2"),
        status = "outline-primary",
        selected = "",
        size = "lg",
        individual = T
      ),

      # Post-AUF Button Group
      shinyWidgets::radioGroupButtons(
        inputId = "post_auf_choice",
        label = "Post-AUF",
        choiceNames = c("None", "U", "U'", "U2/U2'"),
        choiceValues = c("UU'", "U", "U'", "U2"),
        status = "outline-primary",
        selected = "",
        size = "lg",
        individual = T
      ),
    ), # End Trainer

    # Settings
    tabPanel("",
      icon = icon("cog"),
      sidebarLayout(
        sidebarPanel(
          align = "left",

          # Username
          textInput("username", label = "Name", value = "Guest"),

          # Select PLL cases
          pickerInput(
            inputId = "pll_case_selection",
            label = "Select PLL Cases",
            choices = PLL %>% pull(PLL),
            # direction = "vertical",
            options = list(
              `actions-box` = TRUE
            ),
            multiple = TRUE,
            selected = PLL %>% pull(PLL)
          ),
          textAreaInput("custom_algs",
            label = "Import Custom Algorithms", width = "100%",
            height = "200px", resize = "none"
          ),
          actionButton("import_button", "Import"),
          br(),
          br(),

          # Game mode
          radioButtons(
            inputId = "training_mode",
            label = "Training Mode",
            choiceNames = c("PLL-Only Speedrun (21 Cases)", "Classic Speedrun (21 cases)", "Color-Neutral Speedrun (21 Cases)", "Custom Practice"),
            choiceValues = c("mode_pll", "mode_classic", "mode_cn", "mode_untimed")
          ),
        ), # end sidebarpanel

        mainPanel(
          # List default PLL algs
          h2("Default PLL Algorithms"),
          "To use your own algorithms, paste them into the textbox on the left. The algorithms must be arranged in alphabetical order of their respective PLL names (Aa, Ab, E, etc.) and separated by commas.",
          reactableOutput("table"),
          h2("Custom PLL Algorithms"),
          reactableOutput("custom_table")
        ) # end mainpanel
      ) # End sidebarlayout
    ), # End Settings

    # tabPanel("Records",
    tabPanel("",
      icon = icon("crown"), value = "Records",
      h2("PLL-Only (Perfect Runs)"),
      plotlyOutput("plot_pll_master", height = "200px") %>% spin(),
      reactableOutput("table_pll_master"),
      h2("PLL-Only (All)"),
      plotlyOutput("plot_pll", height = "200px") %>% spin(),
      reactableOutput("table_pll"),
      h2("Classic Mode"),
      plotlyOutput("plot_classic", height = "200px") %>% spin(),
      reactableOutput("table_classic"),
      h2("CN Mode"),
      plotlyOutput("plot_cn", height = "200px") %>% spin(),
      reactableOutput("table_cn"),
    ), # End Records

    tabPanel("",
      icon = icon("info"), value = "About",
      # List default PLL algs
      h2("About"),
      "PLL Recognition Time Attack is a two-side PLL recognition and AUF trainer with the following modes:",
      tags$ul(
        HTML("<li>The <b>PLL-Only</b> Mode will randomly select one white-cross case for each PLL. No AUFs needed. Try to get a perfect run without any incorrect guesses!</li>"),
        HTML("The <b>Classic Speedrun</b> mode will randomly select one white-cross case for each PLL.</li>"),
        HTML("<li>The <b>Color-Neutral</b> mode will randomly select one color neutral case for each PLL.</li>"),
        HTML("<li>The <b>Custom Practice</b> mode will select all of the white-cross cases for the selected PLL(s) in the drop-down menu.</li>")
      ),
      h2("Keyboard Shortcuts"),
      "PLLs can be selected by typing the PLL names. For example, to select Ga, press 'g' and then 'a'. Pre-AUFs can be selected with the number keys 1, 2, 3, and 4 and post-AUFs with 7, 8, 9, and 0. Start/stop with 'enter.'",
      h2("Custom Algorithms"),
      "AUF execution depends on the PLL algorithms used. To use your own algorithms, paste them into the 'Import Custom Algorithms' textbox in the settings page (gear icon). The algorithms must be arranged in alphabetical order of their respective PLL names (Aa, Ab, E, etc.) and separated by commas.",
      h2("Statistics"),
      "For each mode, an interactive plot displays the top run of the given name in settings alongside the top record for comparison; and tables display the slowest and fastest execution times and sparklines of all recognition times in alphabetical order.",
      h2("Acknowledgements"),
      "I ('",
      a("OtterCuber", href = "https://ottercuber.github.io/", target = "_blank"),
      "') would like to thank tdecker91 for his VisualCube TS, which makes this app possible, members of the ZMS community for helpful suggestions, and also the following individuals:",
      tags$ul(
        HTML("<li>Tim Mosher ('tsmosher') for testing and design</li>"),
        HTML("<li>Ryan Hudgens ('OreKehStrah') for the name suggestion, keyboard shortcuts, and testing</li>"),
        HTML("<li>Matias Macaya ('SpeedCuber') for testing</li>"),
        HTML("<li>Alex Davison ('zzoomer') for design and testing</li>")
      ),
    ) # End About
  )

# https://stackoverflow.com/questions/50604908/image-output-in-shiny-app
server <- function(input, output, session) {

  # Create a reactive val that will always have the most updated PLL algs for key inputs
  pll_universal <- reactiveVal(PLL)

  # Create a reactive val for the custom algs
  pll_custom <- reactiveVal(tibble(PLL = character(0), Algorithm = character(0)))

  # Create a reactive val for the subset of pll cases
  cases_subset <- reactiveVal(tibble())
  cases_subset(cases)

  # Create a reactive val to determine if another case should be displayed
  case_counter <- reactiveVal(0)

  # Reactive val for spacebar start/stop toggle
  spacebar_toggle <- reactiveVal(0)

  # Create reactive val for storing time for each PLL case
  case_time <- reactiveVal(tibble(PLL = PLL$PLL, Start = NA, Diff = NA))

  # Create reactive val for storing number of records (for caching purposes)
  record_rows <- reactiveVal(numeric(0))

  # Provide tip
  output$output_final_time <- renderText({
    sample(tips, 1)
  })

  # Create reactive val for otter crown (perfect run)
  otter_crown <- reactiveVal(T)

  # Set number of cases
  num_cases <- reactiveVal(21)

  # Initialize cube image
  output$pll_ui <- renderUI({
    img(src = "cube.png")
  })

  # Listen to keys
  observeEvent(input$keys, {

    # Listen to start/stop all the time
    if (input$keys == "enter") start_stop_game(input, output, session)

    # If the game has started, then listen to PLL inputs
    if (case_counter() > 0) {
      switch(input$keys,
        "a a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Aa")),
        "a b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ab")),
        "e" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "E")),
        "f" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "F")),
        "g a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ga")),
        "g b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Gb")),
        "g c" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Gc")),
        "g d" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Gd")),
        "h" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "H")),
        "j a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ja")),
        "j b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Jb")),
        "n a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Na")),
        "n b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Nb")),
        "r a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ra")),
        "r b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Rb")),
        "t" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "T")),
        "u a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ua")),
        "u b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ub")),
        "v" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "V")),
        "y" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Y")),
        "z" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Z")),
        "A a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Aa")),
        "A b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ab")),
        "E" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "E")),
        "F" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "F")),
        "G a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ga")),
        "G b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Gb")),
        "G c" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Gc")),
        "G d" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Gd")),
        "H" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "H")),
        "J a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ja")),
        "J b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Jb")),
        "N a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Na")),
        "N b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Nb")),
        "R a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ra")),
        "R b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Rb")),
        "T" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "T")),
        "U a" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ua")),
        "U b" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Ub")),
        "V" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "V")),
        "Y" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Y")),
        "Z" = updateRadioGroupButtons(session, "pll_choice", selected = get_alg(pll_universal(), "Z")),
        "1" = updateRadioGroupButtons(session, "pre_auf_choice", selected = "UU'"),
        "2" = updateRadioGroupButtons(session, "pre_auf_choice", selected = "U"),
        "3" = updateRadioGroupButtons(session, "pre_auf_choice", selected = "U'"),
        "4" = updateRadioGroupButtons(session, "pre_auf_choice", selected = "U2"),
        "7" = updateRadioGroupButtons(session, "post_auf_choice", selected = "UU'"),
        "8" = updateRadioGroupButtons(session, "post_auf_choice", selected = "U"),
        "9" = updateRadioGroupButtons(session, "post_auf_choice", selected = "U'"),
        "0" = updateRadioGroupButtons(session, "post_auf_choice", selected = "U2")
      )
    }

    # # Always listen to spacebar toggles
    # if(input$keys == "space") {
    #   # If the game is in progress, stop it
    #   if(spacebar_toggle() == 1) {
    #     spacebar_toggle(0)
    #     print(spacebar_toggle())
    #     print("Shutting game down...")
    #   }
    #
    #   # If the game is not in progress, start it
    #   if(spacebar_toggle() == 0) {
    #     spacebar_toggle(1)
    #     print(spacebar_toggle())
    #     print("Starting game...")
    #   }
    # }
    # print(input$keys)
  })

  # Format custom algs
  observeEvent(input$import_button, {
    req(input$custom_algs)
    re_pll <- "[URFDLBEMSwurfdlbems'xyz\\d\\(\\)\\s]+"

    if (str_count(isolate(input$custom_algs), re_pll) == 21) {
      showNotification("Imported 21 algorithms!")
    } else {
      showNotification("Import failed!")

      # Reset reactivevals to empty tibble
      pll_custom(tibble(PLL = character(0), Algorithm = character(0)))
      pll_universal(PLL)

      # Reset to default algorithms
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "pll_choice",
        label = "PLL",
        choiceNames = PLL %>% pull(PLL),
        choiceValues = PLL %>% pull(Alg2),
        selected = character(0),
        status = "outline-success"
      )
    }

    # Validate the input
    validate(need(str_count(isolate(input$custom_algs), re_pll) == 21, "Import failed, sorry!"))

    # Format user input
    pll_custom_df <- str_match_all(isolate(input$custom_algs), re_pll)[[1]][, 1] %>%
      enframe(name = NULL, value = "Algorithm") %>%
      mutate(PLL = PLL$PLL) %>%
      rowwise() %>%
      mutate(Alg = str_replace_all(Algorithm, " ", "")) %>%
      mutate(
        Alg = str_replace_all(Alg, "\\(|\\)", ""),
        Alg = str_replace_all(Alg, "r'", "Rw'"),
        Alg = str_replace_all(Alg, "r", "Rw"),
        Alg = str_replace_all(Alg, "u", "Uw"),
        Alg = str_replace_all(Alg, "u", "Uw"),
        Alg = str_replace_all(Alg, "f", "Fw"),
        Alg = str_replace_all(Alg, "f'", "Fw'"),
        Alg = str_replace_all(Alg, "d", "Dw"),
        Alg = str_replace_all(Alg, "d", "Dw"),
        Alg = str_replace_all(Alg, "l", "Lw"),
        Alg = str_replace_all(Alg, "l", "Lw"),
        Alg = str_replace_all(Alg, "b", "Bw"),
        Alg = str_replace_all(Alg, "b", "Bw")
      ) %>%
      # Cubing Package: "In particular, the S slice direction is different to what you
      # may find elsewhere; the definition used for S in this package is consistent
      # with the rotation directions."
      mutate(
        Alg = str_replace_all(Alg, "S'", "B'Fz'"),
        Alg = str_replace_all(Alg, "S", "BF'z")
      )

    # Set reactive val to the user's list of algs
    # This reactive val is only for the display of algs in the table (original input)
    pll_custom(pll_custom_df %>% select(PLL, Algorithm))

    # Set key stroke reactive val to the formatted algs
    pll_universal(pll_custom_df %>% select(PLL, Alg2 = Alg))

    # Update algs in the buttons
    updateRadioGroupButtons(
      session = session,
      inputId = "pll_choice",
      label = "PLL",
      choiceNames = PLL %>% pull(PLL),
      choiceValues = pll_custom_df %>% arrange(PLL) %>% pull(Alg),
      selected = character(0),
      size = "lg",
      status = "outline-success"
    )
  })

  # Filter PLLs based on picker selection
  observeEvent(input$pll_case_selection, {

    # If the game has not started and training mode is UNTIMED PRACTICE, filter cases
    if (case_counter() == 0 & input$training_mode == "mode_untimed") {

      # If one or more PLL cases selected
      if (!is.null(input$pll_case_selection)) {
        cases_subset(
          cases %>%
            filter(cross_color == "white") %>%
            filter(PLL %in% input$pll_case_selection)
        )
        # print(glue("A total of {nrow(cases_subset())} PLL cases have been selected."))
        # If no PLL cases selected (not sure if this is ever run)
      } else {
        cases_subset(tibble())
        # print(glue("{nrow(cases_subset())} PLL cases have been selected."))
      }
    }
  })

  # Show Button Selections
  # output$total_user_input <- renderText({
  #   glue("{input$pre_auf_choice} {input$pll_choice} {input$post_auf_choice}", .na = "", .null = "")
  # })

  # Monitor the training mode selection
  observeEvent(input$training_mode, {

    # If classic, cn, or pll mode is selected, select all PLL cases and disable the picker
    if (input$training_mode %in% c("mode_classic", "mode_cn", "mode_pll")) {
      updatePickerInput(session = session, inputId = "pll_case_selection", selected = PLL$PLL)
      shinyjs::disable("pll_case_selection")

      # If untimed mode is selected, enable the picker (not sure why first line is needed)
    } else {
      updatePickerInput(session = session, inputId = "pll_case_selection", selected = PLL$PLL)
      shinyjs::enable("pll_case_selection")
    }

    # If the game has not started
    if (case_counter() == 0) {

      # If PLL mode, disable AUF buttons
      if (input$training_mode == "mode_pll") {
        shinyWidgets::updateRadioGroupButtons(session, "pre_auf_choice", disabled = T)
        shinyWidgets::updateRadioGroupButtons(session, "post_auf_choice", disabled = T)

        # Else (if any other mode), enable AUF buttons
      } else {
        shinyWidgets::updateRadioGroupButtons(session, "pre_auf_choice", disabled = F)
        shinyWidgets::updateRadioGroupButtons(session, "post_auf_choice", disabled = F)
      }
    }
  })

  start_stop_game <- function(input, output, session) {
    if (case_counter() == 0) {

      # Stop if no valid username input and notify (https://stackoverflow.com/questions/2053335/what-should-be-the-valid-characters-in-usernames)
      re_username <- "^\\w(?:\\w|[.-](?=\\w)){2,31}$" # 3-32
      if (!str_detect(input$username, re_username)) {
        showNotification("Please input your name (3-22 alphanumeric characters)!")
      }
      validate(need(str_detect(input$username, re_username), "No valid username"))

      # Stop if no PLL cases chosen and notify
      if (is.null(input$pll_case_selection)) {
        showNotification("No PLL cases selected!")
      }
      validate(need(!is.null(input$pll_case_selection), "No PLL"))

      # Clear all times, just in case
      case_time(tibble(PLL = PLL$PLL, Start = NA, Diff = NA))

      # Set crown to TRUE again
      otter_crown(T)
      # print("Crown set to T")

      # Make sure all PLL cases are selected for classic and CN modes
      # Note: This doesn't affect the actual case selection, but it reminds the user that
      # all PLL cases are used for these two modes)
      if (input$training_mode %in% c("mode_classic", "mode_cn", "mode_pll")) {

        # Set cases to 21 for counter
        num_cases(21)
        updatePickerInput(session = session, inputId = "pll_case_selection", selected = PLL$PLL)
      }

      # If classic mode or PLL mode, then select one white-cross case for each PLL (21)
      if (input$training_mode %in% c("mode_classic", "mode_pll")) {
        cases_subset(
          cases %>%
            filter(cross_color == "white") %>%
            group_by(PLL) %>%
            slice_sample(n = 1) %>%
            ungroup() %>%
            dplyr::slice(sample(1:n())) # randomly shuffle
        )

        # print(glue_collapse(cases_subset()$PLL, sep = ", ")) # Cheat
      }

      # If CN mode is selected, then select 21 random cross cases
      if (input$training_mode == "mode_cn") {
        # Select 21 random cases from all cross colors
        cases_subset(
          cases %>%
            group_by(PLL) %>%
            slice_sample(n = 1) %>%
            ungroup() %>%
            dplyr::slice(sample(1:n())) # randomly shuffle
        )
      }

      # If untimed mode, select all white cross cases using the PLL case(s) in the picker (as few as 64 cases)
      if (input$training_mode == "mode_untimed") {
        cases_subset(
          cases %>%
            filter(cross_color == "white") %>% # select white crosses only
            filter(PLL %in% input$pll_case_selection) %>% # select cases in the picker
            dplyr::slice(sample(1:n())) # randomly sort the dataframe
        )

        # Clear case info
        num_cases(nrow(cases_subset())) # set this to the maximum number of cases
      }

      # Set the continue flag to 1 (true)
      case_counter(1)

      # Change the button text to "stop"
      updateActionButtonStyled(session = session, inputId = "Start", label = "Stop")

      # Notify user that the first case image is being displayed
      output$output_final_time <- renderText({
        glue("Case {case_counter()}/{nrow(cases_subset())}")
      })

      # Debug
      # print(glue("=====Mode {input$training_mode} has started. Cases: {nrow(cases_subset())}====="))

      # If the start button is pressed, but the game has already started, then stop
    } else {

      # Reset case image
      shinyjs::runjs("$( document ).ready(function() {
      const element = document.getElementById('visualcube')
      const SRVisualizer = window['sr-visualizer'];
      element.removeChild(element.lastElementChild);
      SRVisualizer.cubeSVG(element, 'visualcube.php?size=175')})")

      # Provide tip
      output$output_final_time <- renderText({
        sample(tips, 1)
      })
      # Clear time output
      # output$output_final_time <- renderText({ "" })

      # Set crown to TRUE again
      otter_crown(T)
      # print("Crown set to T")

      # Change button text to "Start"
      updateActionButtonStyled(session = session, inputId = "Start", label = "Start")

      # Reset game counter
      # print("Counter reset to 0.")
      case_counter(0)

      # Reset times
      case_time(tibble(PLL = PLL$PLL, Start = NA, Diff = NA))



      # Reset PLL buttons for all training modes
      updateRadioGroupButtons(session = session, inputId = "pll_choice", selected = character(0))

      # Disable AUF buttons if pll mode, otherwise reset them
      if (input$training_mode == "mode_pll") {
        shinyWidgets::updateRadioGroupButtons(session, "pre_auf_choice", disabled = T)
        shinyWidgets::updateRadioGroupButtons(session, "post_auf_choice", disabled = T)
      } else {
        updateRadioGroupButtons(session = session, inputId = "pre_auf_choice", selected = "")
        updateRadioGroupButtons(session = session, inputId = "post_auf_choice", selected = "")
      }

      # print(input$pre_auf_choice)
      # print(input$pll_choice)
      # print(input$post_auf_choice)

      # If picker contains cases, reset the cases to the current selection
      if (!is.null(input$pll_case_selection)) {
        # print("Setting case_subset to current selection of pll's")
        cases_subset(
          cases %>%
            filter(PLL %in% input$pll_case_selection)
        )

        # If the picker does not have any cases selected, reset to full set of cases
      } else {
        cases_subset(cases) # setting this to an empty tibble() triggered an error
        # print("Resetting cases_subset to full set of cases")
      }
    }
  }

  # Start Button
  observeEvent(input$Start, {
    start_stop_game(input, output, session)
  })

  # If the game counter changes, do this:
  observeEvent(case_counter(), {

    # Reset PLL buttons for all training modes
    updateRadioGroupButtons(session = session, inputId = "pll_choice", selected = character(0))

    # Disable AUF buttons if pll mode, otherwise reset them
    if (input$training_mode == "mode_pll") {
      shinyWidgets::updateRadioGroupButtons(session, "pre_auf_choice", disabled = T)
      shinyWidgets::updateRadioGroupButtons(session, "post_auf_choice", disabled = T)
    } else {
      updateRadioGroupButtons(session = session, inputId = "pre_auf_choice", selected = "")
      updateRadioGroupButtons(session = session, inputId = "post_auf_choice", selected = "")
    }

    # Debug
    # print(glue(paste("Counter Updated To: {case_counter()}.",
    #                  "Total Cases: {num_cases()}.",
    #                  "Buttons: [{input$pre_auf_choice}] [{input$pll_choice}] [{input$post_auf_choice}]"), # not sure why pll_choice is always populated
    #            .na = "", .null = ""))
    # #if(input$training_mode == "mode_pll") print(glue_collapse(cases_subset()$PLL, sep = ", ")) # Cheat
    # print(glue_collapse(cases_subset()$PLL, sep = ", ")) # Cheat

    # If the case counter is > 0 and there are filtered cases,
    # And case counter doesn't exceed the number of cases
    # display a case image, indexed by the counter
    if (case_counter() > 0 & length(cases_subset()) != 0 & case_counter() <= num_cases()) {

      # Debug
      # print(glue("Displaying #{case_counter()}/{nrow(cases_subset())} case. Filename: {cases_subset()$CaseFilename[case_counter()]}. Invmoves: {cases_subset()$invmoves[case_counter()]}."))

      # Display case
      shinyjs::runjs(paste0(
        "
        $( document ).ready(function() {
          const element = document.getElementById('visualcube')
          const SRVisualizer = window['sr-visualizer'];
          element.removeChild(element.lastElementChild);",
        "SRVisualizer.cubeSVG(element, '",
        cases_subset()$TruncatedCaseURL[case_counter()],
        "')})"
      ))

      # If not untimed mode, set start time for the current case
      if (input$training_mode != "mode_untimed") {
        # print("Setting a timestamp for the start case")
        starttime <- list(Sys.time())
        case_time(
          case_time() %>%
            mutate(Start = ifelse(str_to_lower(PLL) == str_to_lower(cases_subset()$PLL[case_counter()]), starttime, Start))
        )

        # Debug
        # print(glue("Start timestamp for {cases_subset()$PLL[case_counter()]}: {as.character(starttime[[1]])}"))
      }
    }

    # If the counter is zero, reset
    if (case_counter() == 0) {
      case_time(tibble(PLL = PLL$PLL, Start = NA, Diff = NA))
      updateActionButtonStyled(session = session, inputId = "Start", label = "Start")
    }
  })

  endgame_procedure <- function(input, output, session) {
    # Reset counter and case_time
    case_counter(0)
    # print("Counter reset to 0.")
    case_time(tibble(PLL = PLL$PLL, Start = NA, End = NA, Diff = rep(as.numeric(0), 21)))

    # Reset case image
    shinyjs::runjs("$( document ).ready(function() {
            const element = document.getElementById('visualcube')
            const SRVisualizer = window['sr-visualizer'];
            element.removeChild(element.lastElementChild);
            SRVisualizer.cubeSVG(element, 'visualcube.php?size=175')})")

    # Reset crown
    otter_crown(T)
    # print("Crown set to T")
  }

  save_record <- function(input, output, session) {
    # Calculate and format times and notify user
    total_case_time <- sum(case_time()$Diff, na.rm = T)
    case_times <- glue_collapse(case_time() %>% arrange(PLL) %>% pull(Diff), sep = ",")
    output$output_final_time <- renderText({
      glue("Time: {round(total_case_time, 2)} seconds")
    })

    # Save times to Google Sheets
    score <- data.frame(
      Mode = input$training_mode,
      Name = input$username,
      Date = lubridate::now("UTC"),
      Time = total_case_time,
      CaseTimes = case_times,
      Crown = ifelse(isolate(otter_crown()), "T", "F")
    ) %>%
      mutate(Mode = case_when(
        Mode == "mode_classic" ~ "Classic",
        Mode == "mode_cn" ~ "CN",
        Mode == "mode_pll" ~ "PLL-Only"
      ))

    # Append data to google sheets
    sheet_append(ss = sheet_id, data = score, sheet = 1)
    # print("Record saved to Google sheets. Counter: {case_counter()}. Target {num_cases()}.")
    # print(glue("Crown saved to Google: {isolate(otter_crown())}"))
  }

  # If there are any changes to the button inputs, do this:
  observeEvent(c(input$pre_auf_choice, input$pll_choice, input$post_auf_choice), {

    # Run the following code only if a PLL button has been clicked
    req(!is.null(input$pll_choice))
    # print("Checking solution...")

    # If the game is active (case counter > 0)
    if (case_counter() > 0) {

      # If the game mode is anything BUT pll mode, check the entire AUF+PLL solution
      if (input$training_mode %in% c("mode_classic", "mode_cn", "mode_untimed")) {

        # Check solution (isolate fixed an issue towards the end of the game)
        full_solution <- glue("{input$pre_auf_choice}{input$pll_choice}{input$post_auf_choice}", .na = "", .null = "")
        c <- getMovesCube()
        # print(glue("Using invmoves to setup cube: {cases_subset()$invmoves[case_counter()]}"))
        c2 <- cubing::move(c, isolate(cases_subset()$invmoves[case_counter()]))
        # print(glue("Solving cube with the following solution: {full_solution}"))
        c3 <- cubing::move(c2, full_solution)
        result <- is.solved(c3)
        #
        # # get solution
        # print(solver(c3))

        # If the user selection/solution solves the cube
        if (result) {

          # Get the end time immediately
          endtime <- Sys.time()

          # For the timed classic and CN modes, save the end time
          if (input$training_mode %in% c("mode_classic", "mode_cn")) {
            starttime <- (case_time() %>% filter(PLL == cases_subset()$PLL[case_counter()]) %>% pull(Start))[[1]]
            mydiff <- as.numeric(difftime(endtime, starttime, units = "sec"))
            case_time(
              case_time() %>%
                mutate(Diff = ifelse(PLL == cases_subset()$PLL[case_counter()], mydiff, Diff))
            )
          }

          # If more cases, then increment counter
          if (case_counter() <= num_cases()) {
            case_counter_new <- case_counter() + 1
            case_counter(case_counter_new)
            output$output_final_time <- renderText({
              glue("Case {case_counter()}/{nrow(cases_subset())}")
            })
          }

          # If last case, calculate the total time and end game
          if (case_counter() > num_cases()) {
            # print(glue("Counter: {case_counter()}. Target {num_cases()}."))

            # If classic or CN mode, notify final time, and times in Google Sheets
            if (input$training_mode %in% c("mode_classic", "mode_cn")) {
              save_record(input, output, session)

              # Display "Nice work!" if untimed mode
            } else if (input$training_mode == "mode_untimed") {
              output$output_final_time <- renderText({
                glue("Nice work!")
              })
            }

            endgame_procedure(input, output, session)

            # Provide tip
            # output$output_final_time <- renderText({ sample(tips, 1) })
          } # End if (case_counter() > num_cases())
        } else {

          # If there is an incorrect solution, set the crown to FALSE
          otter_crown(F)
          # print("Crown set to F")
        } # End if(result)
      } # End if game mode is NOT mode_pll

      # If the game mode is mode_pll, we only need to check if the selected PLL is correct
      if (input$training_mode == "mode_pll") {

        # Get the user's attempted solution
        user_solution <- pll_universal() %>%
          filter(Alg2 == input$pll_choice) %>%
          pull(PLL)

        # Get the solution
        solution <- isolate(cases_subset()$PLL[case_counter()])

        # If the user selection/solution solves the cube
        if (user_solution == solution) {

          # Get the end time immediately
          endtime <- Sys.time()

          # Record the time
          starttime <- (case_time() %>% filter(PLL == cases_subset()$PLL[case_counter()]) %>% pull(Start))[[1]]
          mydiff <- as.numeric(difftime(endtime, starttime, units = "sec"))
          case_time(
            case_time() %>%
              mutate(Diff = ifelse(PLL == cases_subset()$PLL[case_counter()], mydiff, Diff))
          )

          # Debug: Print the time
          # print("Printing time...")
          # print(case_time())

          # If there are still more cases left, increment the counter
          if (case_counter() <= num_cases()) {
            case_counter_new <- case_counter() + 1
            case_counter(case_counter_new)
            output$output_final_time <- renderText({
              glue("Case {case_counter()}/{nrow(cases_subset())}")
            })
            # print(glue("Counter: {case_counter()}. Target {num_cases()}."))
          }

          # If after incrementing the counter, this is the last case, then
          if (case_counter() > num_cases()) {
            # print(glue(" Counter: {case_counter()}. Target {num_cases()}."))

            save_record(input, output, session)
            endgame_procedure(input, output, session)
          } # End if (case_counter() > num_cases())
        } else {

          # If there is an incorrect solution, set the crown to FALSE
          otter_crown(F)
          # print("Crown set to F")
        } # End if(result)
      } # end if training mode == mode_pll
    } # End if (case_counter() > 0)
  })

  # Display default PLL algorithms
  output$table <- renderReactable(reactable(PLL %>% select(PLL, Algorithm = Alg),
    theme = reactableTheme(
      backgroundColor = "transparent"
    ),
    sortable = F,
    pagination = F,
    columns = list(
      PLL = colDef(width = 100),
      Algorithm = colDef(width = 500)
    ),
    borderless = T
  ))

  # Display custom PLL algorithms
  output$custom_table <- renderReactable(reactable(pll_custom(),
    theme = reactableTheme(
      backgroundColor = "transparent"
    ),
    sortable = F,
    pagination = F,
    columns = list(
      PLL = colDef(width = 100),
      Algorithm = colDef(width = 500)
    ),
    borderless = T
  ))

  # Calculate and display records when crown icon is clicked
  observeEvent(input$tabSwitch, {
    if (input$tabSwitch == "Records") {

      # Read records from Google Sheets
      df_recs <- googlesheets4::read_sheet(sheet_id, col_types = "ccTdcc")

      # Update reactiveVal with number of rows
      # the bindCache() functions will save outputs with a unique username and record_rows()
      # If either of those values change (e.g., a new speedrun record generated or a different name specified), a new cache will be created
      record_rows(nrow(df_recs))

      # Calculate slowest, fastest, and format times for sparkline (Note that missing
      # data must be NULL for sparkline:
      # https://github.com/htmlwidgets/sparkline/issues/27)
      df_records <- df_recs %>%
        rowwise() %>%
        mutate(SFR = list(get_topn(CaseTimes))) %>%
        ungroup() %>%
        unnest_wider(col = SFR) %>%
        select(-CaseTimes) %>%
        mutate(Date = as.character(Date)) %>%
        select(Mode, Name, Time, Slowest, Fastest, Recognition, Date, Crown) %>%
        rename("Date (UTC)" = Date) %>%
        mutate(Time = round(Time, 2))

      # Plots
      output$plot_pll_master <- renderPlotly({
        get_plot(df_recs %>% filter(Crown == "T"), input$username, "PLL-Only")
      }) %>%
        bindCache(input$username, record_rows())
      output$plot_pll <- renderPlotly({
        get_plot(df_recs, input$username, "PLL-Only")
      }) %>%
        bindCache(input$username, record_rows())
      output$plot_classic <- renderPlotly({
        get_plot(df_recs, input$username, "Classic")
      }) %>%
        bindCache(input$username, record_rows())
      output$plot_cn <- renderPlotly({
        get_plot(df_recs, input$username, "CN")
      }) %>%
        bindCache(input$username, record_rows())

      # Tables
      output$table_classic <- renderReactable(get_table(df_records, "Classic")) %>%
        bindCache(input$username, record_rows())
      output$table_cn <- renderReactable(get_table(df_records, "CN")) %>%
        bindCache(input$username, record_rows())
      output$table_pll <- renderReactable(get_table(df_records, "PLL-Only")) %>%
        bindCache(input$username, record_rows())
      output$table_pll_master <- renderReactable(get_table(df_records %>% filter(Crown == "T"), "PLL-Only")) %>%
        bindCache(input$username, record_rows())
    }
  })

  # Initialize case image
  shinyjs::runjs("$( document ).ready(function() {
  const element = document.getElementById('visualcube')
  const SRVisualizer = window['sr-visualizer'];
  SRVisualizer.cubeSVG(element, 'visualcube.php?size=175')})")
}

# Run the application
shinyApp(ui = ui, server = server)