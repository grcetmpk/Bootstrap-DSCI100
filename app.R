library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# --- Global Variables ---
# Population parameter (True proportion of Lollipops)
P_POP_DEFAULT <- 0.50
N_SAMPLE_DEFAULT <- 100
BULK_SAMPLES_DEFAULT <- 1000
# Sample size (Candies per box)
# Colors for the candies and plots
COLOR_RED <- "#FF4B3E"
COLOR_GREEN <- "#5BE366"
COLOR_BOOTSTRAP <- "#0072B2" # Blue for Bootstrap
COLOR_SAMPLING <- "#D55E00" # Orange for Sampling
COLOR_CI <- "#009E73" # Greenish-blue for CI lines

# Define Emojis
CANDY_RED <- "🍭" # Red Candy/Lollipop
CANDY_GREEN <- "🍬" # Green Candy

# Define the colors and linetypes for the legend (moved here for global access, though only used in server)
LINE_COLORS <- c("True Population Proportion" = "red", "Mean of Sample Proportions" = "blue", "Original Sample Proportion" = "red", "Mean of Bootstrap Proportions" = "blue")
LINE_LINETYPES <- c("True Population Proportion" = "solid", "Mean of Sample Proportions" = "longdash", "Original Sample Proportion" = "solid", "Mean of Bootstrap Proportions" = "longdash")
LINE_SIZES <- c("True Population Proportion" = 0.5, "Mean of Sample Proportions" = 0.5, "Original Sample Proportion" = 0.5, "Mean of Bootstrap Proportions" = 0.5)


# --- UI (User Interface) ---
ui <- dashboardPage(
  # 1. Set the dashboard skin to a classic color, e.g., "blue".
  title = "Grace's Candy Factory 🍭 ",
  skin = "blue",
  dashboardHeader(title = "Grace's Candy Factory 🍭"),
  # --- MODIFICATION 1: Keep the sidebar for the settings, but remove the tab menu ---
  dashboardSidebar(
    tags$style(
      HTML("
/* Overall Light Theme Override: Sidebar background */
.main-sidebar, .left-side {
background-color: #f8f8f8 !important; /* Very light gray/white sidebar background */
}
/* 1. Set ALL default text in the sidebar (menu, headers) to BLACK */
.main-sidebar, .sidebar-menu a, .sidebar-menu {
color: #000000 !important; /* Explicitly black text */
}
/* 2. Target the labels for numeric and slider inputs in the sidebar to ensure they are black */
.sidebar .form-group label {
color: #000000 !important;
}
/* Target the slider range numbers (min/max indicators) to ensure they are black */
.irs-min, .irs-max {
color: #000000 !important;
}

/* Menu Item Styling */
/* Change menu item text color to black for contrast on light background */
.sidebar-menu a {
color: #000000 !important;
}

/* Change menu item background color on hover */
.sidebar-menu > li.active > a,
.sidebar-menu > li:hover > a {
border-left-color: #f39c12; /* Use a bright color like orange for active/hover */
background-color: #e6e6e6 !important; /* Light gray background on hover */
color: #3c8dbc !important; /* Re-set icon/text color to blue on hover for visibility */
}

/* Style for the currently selected menu item */
.sidebar-menu > li.active > a {
font-weight: bold;
color: #3c8dbc !important; /* Active link color (blue) */
background-color: #dddddd !important; /* Changed to LIGHT GREY */
}
/* Style for the candy emojis */
.candy-emoji {
font-size: 20px; /* Adjust size as needed */
margin: 2px;
display: inline-block;
line-height = 1; /* Helps with vertical alignment */
}
")
    ),
    # The sidebarMenu is REMOVED because the tabs are now in the body.
    # We keep the app settings and the action button in the sidebar.
    tags$hr(),
    # Ensuring the h5 header text is black
    h4(style = "padding-left: 20px; color: #000000;", "App Settings"),
    numericInput("n_sample_ui",
                 "Candies per Box (Sample Size)",
                 value = N_SAMPLE_DEFAULT,
                 min = 10,
                 max = 1000,
                 step = 10),
    sliderInput("p_pop_ui",
                "True Proportion of Lollipops",
                value = P_POP_DEFAULT,
                min = 0.05,
                max = 0.95,
                step = 0.01),
    numericInput("n_bulk_ui",
                 "Number of Repeated Samples",
                 value = BULK_SAMPLES_DEFAULT,
                 min = 10,
                 max = 10000,
                 step = 100),
    actionButton("reset_all", "Reset All", icon = icon("power-off"),
                 style="color: #fff; background-color: #dc3545; border-color: #dc3545; margin-left: 15px;")
  ),
  # --- MODIFICATION 2: Put the tab content inside a tabBox in the dashboardBody ---
  dashboardBody(
    tags$style(
      HTML("
/* Overall Light Theme Override: Main content area */
.content-wrapper, .right-side {
background-color: white !important;
}

/* Target the overall wrapper element that holds the entire dashboard */
.wrapper {
    background-color: white !important;
}

/* --- FIX: Ensure the box and its content area are explicitly white --- */
/* This targets the boxes that contain the distribution plots and info */
.box.box-solid.box-primary {
    background-color: white !important;
}

/* This targets the plot area inside those boxes, just in case */
.box.box-solid.box-primary > .box-body {
    background-color: white !important;
}
/* --- END FIX --- */

/* Custom colors for candy boxes */
.small-box.bg-red { background-color: #FF4B3E !important; color: white !important; }
.small-box.bg-green { background-color: #5BE366 !important; color: white !important; }

/* --- Tab Outline Color Modification --- */
/* Target the overall tab box border */
.nav-tabs-custom {
  border-top-color: #333333; /* Darker line above content area */
}
/* Target the border of the tab itself (at the bottom of the tab header) */
.nav-tabs-custom > .nav-tabs > li.active {
  border-bottom-color: #333333; /* Darker line under the active tab */
}
/* Target the border on the non-active tabs */
.nav-tabs-custom > .nav-tabs > li {
  border-top: 1px solid #333333; /* Darker top border for non-active tabs */
  border-left: 1px solid #333333; /* Darker left border for non-active tabs */
  border-right: 1px solid #333333; /* Darker right border for non-active tabs */
}
/* Target the border of the active tab specifically to set a dark border */
.nav-tabs-custom > .nav-tabs > li.active:hover > a,
.nav-tabs-custom > .nav-tabs > li.active > a {
  border-top-color: #333333; /* Darker top border for active tab */
  border-left-color: #333333; /* Darker left border for active tab */
  border-right-color: #333333; /* Darker right border for active tab */
}
/* Target the border of the content pane */
.tab-content {
  border: 1px solid #333333; /* Darker border around the content area */
  border-top-color: #333333; /* Ensure the top border is dark (to meet the tab) */
}
/* --- End Tab Outline Color Modification --- */
")
    ),
    # Start the tabBox to create the clickable tabs in the main area
    tabBox(
      title = NULL, # No title needed for the overall box
      id = "tabset1", # An ID for the tabBox
      width = 12, # Take up the full width
      
      # The content of the original tabItem('sampling') is now a tabPanel
      tabPanel("Sampling Distribution", icon = icon("boxes"), value = "sampling",
               
               # Content of the Sampling Distribution tab starts here
               fluidRow(
                 # Descriptive Box for Sampling Distribution Tab
                 box(title = "Welcome to Grace's Candy Factory! 🍭", status = "primary", solidHeader = TRUE, width = 12,
                     p("In this simulation, there is a candy factory where there is a machine that produces a set proportion of lollipops and candies.", strong("Use the slider on the right hand side to change the proportion of lollipops that the factory produces (the population proportion)!")),
                     
                     p("Once all of the candy is produced, a machine mixes them up and places a set amount in a box. You can", strong("control the number of candies per box (the sample size)"), " using the box on the left-hand side."),
                     p("Opening a box of candy is like taking a random sample!", strong("Click the \"Open 1 Box\" button to open a box to view the sample, and calculate the sample proportion of lollipops."), "This will vary from box-to-box... 
                       but by how much? To see, let's open more boxes and plot the sample proportion of lollipops for every box opened! You can open more boxes individually, or", strong("open and calculate the sample proportions of lollipops for many boxes all at once clicking the \"Open X Boxes\" button.")),
                     p("At the bottom of this page, you'll find the", strong("histogram of the Sampling Distribution."), "You can see the mean proportion of lollipops across all of the samples you took and compare it to the true value! Notice how the histogram changes as you change the sample size or number of samples."),
                     p("To restart the simulation, click the red \"Reset All\" button on the left-hand pane."),
                     p(em("Interested in learning about the", strong(" bootstrap distribution?"),  "Click the Bootstrap Distribution tab above to switch applets!"))
                 )
               ),
               fluidRow(
                 # Single Sample UI (Width 6) - NOW CONTAINS VISUALIZATION
                 box(title = "Sample Once ", status = "success", solidHeader = TRUE, width = 6, # Changed width from 4 to 6
                     h5("Click the button to open a box of candy, and determine the proportion of lollipops. You may click this button many times to look at many different samples!"),
                     actionButton("sample_one_box", "Open 1 Box", icon = icon("box-open")),
                     tags$hr(),
                     # ADDED: Single Sample Proportion Display
                     uiOutput("sample_prop_single_ui"),
                     uiOutput("candy_box_vis_sampling")
                     
                     # ADDED: Live Box Visualization (Sampling)
                 ),
                 
                 # Bulk Sample UI (Width 6)
                 box(title = "Same Many Times At Once", status = "success", solidHeader = TRUE, width = 6, # Changed width from 4 to 6
                     h5("Click this button to open many boxes of candy at once, and calculate the proportion of lollipops in each of them."),
                     actionButton("sample_many_boxes", textOutput("sampling_bulk_label", inline = TRUE), icon = icon("box-open")),
                     tags$hr()
                 )
               ),
               fluidRow(
                 box(title = "Sampling Distribution", status = "primary", solidHeader = TRUE, width = 12,
                     uiOutput("sampling_stats_ui"),
                     plotOutput("sampling_hist_plot"),
                 )
               )
               # End of Sampling Distribution tabPanel
      ),
      
      # The content of the original tabItem('bootstrap') is now a tabPanel
      tabPanel("Bootstrap Distribution", icon = icon("redo-alt"), value = "bootstrap",
               
               # Content of the Bootstrap Distribution tab starts here
               fluidRow(
                 box(title = "Estimating the Sampling Distribution with a Single Box of Candy", status = "primary", solidHeader = TRUE, width = 12,
                     p("Here, we assume", strong("we only have access to a single box of candy (one sample)."), "We can get a point estimate of the true proportion of lollipops the factor makes, but how \"good\" is this estimate?", strong("We may want to quantify how sure we are about this estimate by also looking at the uncertainty."), "To do so, we can use the bootstrap distribution."),
                     p("We treat our original box as a \"pseudo-population\" and", strong("repeatedly sample from this box of candy with replacement"),  "to create the Bootstrap Distribution. The bootstrap distribution is an", strong("approximation"),  "of the sampling distribution."),
                     p("Sampling with replacement means if I have a sample of 100 candies, I'm going to randomly sample a single candy from my candy box 100 times, but every time I sample the candy, I put it back into the box. This way, the same candy can be chosen multiple times!", strong("These bootstrap samples are estimates of what unique samples from the population may have looked like.")),
                     p("Adjust the parameters on the left-hand pane, and then ", strong("press the button to select a box of candy to use as your sample."), "Then, generate single bootstrap samples (samples from the original sample, with replacement) one-by-one using the", strong("\"Sample with Replacement from 1 Box\" button"), ", or generate many samples at once using the", strong("\"Sample with Replacement X Times\""),  "button on the right.", strong("See how the mean of the proportions of the bootstrap distribution compares to the proportion of lollipops in the sample, and the true population proportion!")),
                     p("You can also quantify the uncertainty in this estimate by looking at the", strong("95% plausible range."), "Check the box on the plot to calculate and view the plausible range.  A narrower range indicates that your estimate has less uncertainty (i.e., is a better estimate!). Notice how the plausible range changes as your sample size changes!"),
                     p("To restart the simulation, click the \"Reset All\" button on the left-hand pane."),
                     p(em("Looking to see the", strong("sampling distribution"), "when taking many samples from the population? Click on the", strong("Sampling Distribution"), "tab to switch applets!"))
                 )
               ),
               fluidRow(
                 # Original Sample Box Setup
                 box(title = "Step 1: Obtain a Random Sample", status = "success", solidHeader = TRUE, width = 4,
                     h5("Click this button to open a box of candy to use as your sample, and view its contents."),
                     actionButton("generate_original_sample", "Press to Choose a Random Box of Candy", icon = icon("box-open")),
                     tags$hr(),
                     uiOutput("original_sample_stats_ui"),
                     uiOutput("original_candy_vis")
                 ),
                 # Single Resample UI
                 box(title = "Step 2A: Simulate One Bootstrap Sample", status = "success", solidHeader = TRUE, width = 4,
                     h5("Click this button to create a bootstrap sample from the sample in Step 1."),
                     h5("You will randomly select a candy from the Sample in Step 1, record it, and place it back into the box. This will be repeated until you have a new sample with the same sample size as in Step 1."),
                     h5("You may click this button many times to generate many bootstrap samples one-by-one."),
                     h4(textOutput("resample_header_label", inline = TRUE)),
                     actionButton("resample_one_box", textOutput("resample_one_label", inline = TRUE), icon = icon("redo-alt")),
                     tags$hr(),
                     uiOutput("resample_prop_single_ui"),
                     tags$hr(),
                     h4("Contents of the Bootstrap Sample"),
                     uiOutput("resample_candy_vis")
                 ),
                 # Bulk Resample UI
                 box(title = "Step 2B: Simulate Many Bootstrap Samples", status = "success", solidHeader = TRUE, width = 4,
                     h5("Click this generate many bootstrap samples and and calculate the proportion of lollipops within each sample."),
                     actionButton("resample_many_boxes", textOutput("bootstrap_bulk_label", inline = TRUE), icon = icon("tachometer-alt")),
                     tags$hr()
                 )
               ),
               fluidRow(
                 box(title = "Bootstrap Distribution", status = "primary", solidHeader = TRUE, width = 12,
                     # NEW CHECKBOX ADDED HERE
                     checkboxInput("show_plausible_range",
                                   "Show 95% Plausible Range (Bootstrap CI)", FALSE),
                     uiOutput("bootstrap_stats_ui"),
                     plotOutput("bootstrap_hist_plot"),
                     # ADDED TEXT LINE
                     p("Because we are resampling from the original sample repeatedly, we see that the bootstrap distribution is centered at the original sample’s mean value (unlike the sampling distribution of the sample mean, which is centered at the population parameter value).")
                 )
               )
               # End of Bootstrap Distribution tabPanel
      ) # End of tabBox
    ), # End of tabBox
    
    # --- NEW AUTHOR SECTION ADDED HERE ---
    fluidRow(
      column(width = 12,
             p(style = "text-align: center; color: #888888; font-size: 0.8em; margin-top: 15px;",
               "Created by Grace Tompkins for DSCI100 at the University of British Columbia, using Gemini.",
               tags$a(href = "https://github.com/grcetmpk/Bootstrap-DSCI100",
                      target = "_blank", # Opens the link in a new tab
                      "Click here for the GitHub Repository."))
             
      )
    )
    # --- END NEW AUTHOR SECTION ---
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive labels for bulk sampling buttons
  output$sampling_bulk_label <- renderText({
    paste("Open", input$n_bulk_ui, "Boxes")
  })
  
  output$bootstrap_bulk_label <- renderText({
    paste("Sample with Replacement", input$n_bulk_ui, "Times")
  })
  
  # Reactive label for the Resample button and header
  output$resample_one_label <- renderText({
    paste("Sample with Replacement from 1 Box")
  })
  
  
  # Reactive values to store the simulation data
  rv <- reactiveValues(
    sampling_dist = data.frame(prop_red = numeric(0)),
    current_sample = NULL, # Current box for sampling
    
    # Bootstrap Tab
    original_sample = NULL, # The initial single sample
    bootstrap_dist = data.frame(prop_red = numeric(0)),
    current_resample = NULL # Current box for resampling
  )
  
  # -----------------------------------------------------------------------
  ### NEW LOGIC: RESET ON SAMPLE SIZE CHANGE (from previous request) ###
  # NOTE: The reference to 'input$sidebarMenu' is no longer valid since we
  # are using a tabBox. We must change it to reference the tabBox's ID: 'input$tabset1'.
  observeEvent(input$n_sample_ui, {
    # CHANGE: Replaced input$sidebarMenu with input$tabset1
    if (!is.null(rv$original_sample) && input$tabset1 == "bootstrap") {
      
      rv$original_sample <- NULL
      rv$bootstrap_dist <- data.frame(prop_red = numeric(0))
      rv$current_resample <- NULL
      
      updateActionButton(session, "generate_original_sample", label = "Press to Choose a Random Box of Candy")
      
      showNotification(
        "Sample Size Changed! Please generate a new Original Sample (Box) for N = ",
        duration = 5, type = "warning"
      )
    }
  })
  # -----------------------------------------------------------------------
  
  # --- General/Setup Logic ---
  # Reset all simulations
  observeEvent(input$reset_all, {
    rv$sampling_dist <- data.frame(prop_red = numeric(0))
    rv$current_sample <- NULL
    rv$original_sample <- NULL
    rv$bootstrap_dist <- data.frame(prop_red = numeric(0))
    rv$current_resample <- NULL
    
    updateActionButton(session, "generate_original_sample", label = "Press to Choose a Random Box of Candy")
  })
  
  # --- Tab 1: Sampling Distribution Logic ---
  
  # Function to draw one sample from the population
  draw_population_sample <- function(n) {
    p_pop_val <- input$p_pop_ui
    # 0 = Green, 1 = Red
    sample(c(0, 1), size = n, replace = TRUE, prob = c(1 - p_pop_val, p_pop_val))
  }
  
  # Single box button click
  observeEvent(input$sample_one_box, {
    sample_size <- input$n_sample_ui
    sample_draw <- draw_population_sample(sample_size)
    rv$current_sample <- sample_draw
    prop_red <- sum(sample_draw) / sample_size
    rv$sampling_dist <- rbind(rv$sampling_dist, data.frame(prop_red = prop_red))
  })
  
  # Bulk sampling button click
  observeEvent(input$sample_many_boxes, {
    sample_size <- input$n_sample_ui
    new_samples <- replicate(input$n_bulk_ui, draw_population_sample(sample_size))
    new_props <- apply(new_samples, 2, function(x) sum(x) / sample_size)
    rv$current_sample <- new_samples[, input$n_bulk_ui]
    new_data <- data.frame(prop_red = new_props)
    rv$sampling_dist <- rbind(rv$sampling_dist, new_data)
  })
  
  # Output: Single Sample Proportion Display
  output$sample_prop_single_ui <- renderUI({
    req(rv$current_sample)
    N_current <- length(rv$current_sample)
    n_red <- sum(rv$current_sample)
    prop_red <- n_red / N_current
    
    div(
      h4(paste0("Sample Proportion of Lollipops from this box: ", round(prop_red, 3)), style = " color: #FF4B3E;")
    )
  })
  
  # Output: Live Box Visualization (Sampling)
  output$candy_box_vis_sampling <- renderUI({
    req(rv$current_sample)
    N_current <- length(rv$current_sample)
    # Only display the first 100 for visual clarity
    vis_candies <- head(rv$current_sample, 100)
    
    candy_divs <- lapply(vis_candies, function(candy) {
      # 1 = Red Candy (🍭), 0 = Green Candy (🍬)
      emoji <- ifelse(candy == 1, CANDY_RED, CANDY_GREEN)
      
      tags$span(
        class = "candy-emoji",
        emoji
      )
    })
    
    tagList(
      div(candy_divs),
      if (N_current > 100) {
        p(paste0("...and ", N_current - 100, " more candies."))
      }
    )
  })
  
  # Output: Sampling Distribution Stats
  output$sampling_stats_ui <- renderUI({
    n_samples <- nrow(rv$sampling_dist)
    mean_prop <- mean(rv$sampling_dist$prop_red)
    
    div(
      h5(paste0("Total Samples: ", n_samples)),
      h5(paste0("Size of Each Sample: ", round(input$n_sample_ui, 3))),
      h5(paste0("True Population Proportion: ", input$p_pop_ui),style = " color: #FF4B3E;"),
      h5(paste0("Mean of Sample Proportions: ", round(mean(rv$sampling_dist$prop_red), 3)), style = " color: blue;")
    )
  })
  
  # Output: Sampling Distribution Histogram
  output$sampling_hist_plot <- renderPlot({
    req(nrow(rv$sampling_dist) > 0)
    
    if(input$n_sample_ui <= 50){
      usebinwidth = 0.05
    }else{
      usebinwidth = 0.01
    }
    
    ggplot(rv$sampling_dist, aes(x = prop_red)) +
      geom_histogram(binwidth = usebinwidth, fill = "grey", color = "white", alpha = 0.8) +
      # Change to use aes() and guide for legend
      geom_vline(aes(xintercept = input$p_pop_ui, linetype = "True Population Proportion", color = "True Population Proportion"), size = 0.5) +
      geom_vline(aes(xintercept = mean(rv$sampling_dist$prop_red), linetype = "Mean of Sample Proportions", color = "Mean of Sample Proportions"), size = 0.5) +
      # Use scale_color_manual and scale_linetype_manual for legend
      scale_color_manual(name = "Legend",
                         values = LINE_COLORS[c("True Population Proportion", "Mean of Sample Proportions")]) +
      scale_linetype_manual(name = "Legend",
                            values = LINE_LINETYPES[c("True Population Proportion", "Mean of Sample Proportions")]) +
      labs(
        x = "Proportion of Lollipops",
        y = "Frequency"
      ) + xlim(0, 1) +
      theme_minimal(base_size = 15) +
      # Annotations must be kept outside of the main ggplot call to avoid errors with legend mapping
      annotate("text", x = input$p_pop_ui, y = Inf, label = paste0(input$p_pop_ui), vjust = 2, hjust = 1.1, color = "red") +
      annotate("text", x = mean(rv$sampling_dist$prop_red), y = Inf, label = paste0( round(mean(rv$sampling_dist$prop_red),3)), vjust = 1, hjust = -0.1, color = "blue")
    
  })
  
  # --- Tab 2: Bootstrap Distribution Logic ---
  
  # Generate the single original sample
  observeEvent(input$generate_original_sample, {
    sample_size <- input$n_sample_ui
    original_sample_draw <- draw_population_sample(sample_size)
    rv$original_sample <- original_sample_draw
    
    rv$bootstrap_dist <- data.frame(prop_red = numeric(0))
    rv$current_resample <- NULL
    
    updateActionButton(session, "generate_original_sample", label = "Original Sample Generated!")
  })
  
  # Output: Original Sample Stats
  output$original_sample_stats_ui <- renderUI({
    req(rv$original_sample)
    N_current <- length(rv$original_sample)
    n_red <- sum(rv$original_sample)
    prop_red_orig <- n_red / N_current
    
    tagList(
      h4(paste0("Sample Proportion of Lollipops from This Box: ", round(prop_red_orig, 4)), style = "color: #FF4B3E;")
    )
  })
  
  # Output: Original Sample Visual (Bootstrap)
  output$original_candy_vis <- renderUI({
    req(rv$original_sample)
    N_current <- length(rv$original_sample)
    vis_candies <- head(rv$original_sample, 100)
    
    candy_divs <- lapply(vis_candies, function(candy) {
      # 1 = Red Candy (🍭), 0 = Green Candy (🍬)
      emoji <- ifelse(candy == 1, CANDY_RED, CANDY_GREEN)
      
      tags$span(
        class = "candy-emoji",
        emoji
      )
    })
    
    tagList(
      div(candy_divs),
      if (N_current > 100) {
        p(paste0("...and ", N_current - 100, " more candies."))
      }
    )
  })
  
  # Function to draw one bootstrap resample (with replacement)
  draw_bootstrap_sample <- function(original_sample, n) {
    sample(original_sample, size = n, replace = TRUE)
  }
  
  # Single resample button click
  observeEvent(input$resample_one_box, {
    # Check if original sample exists
    if (is.null(rv$original_sample)) {
      showNotification("STEP 1 REQUIRED: Please click 'Press to Choose a Random Box of Candy' first!",
                       duration = 5, type = "error")
      return()
    }
    sample_size <- input$n_sample_ui
    
    resample_draw <- draw_bootstrap_sample(rv$original_sample, sample_size)
    rv$current_resample <- resample_draw
    prop_red <- sum(resample_draw) / sample_size
    rv$bootstrap_dist <- rbind(rv$bootstrap_dist, data.frame(prop_red = prop_red))
  })
  
  # Bulk resampling button click
  observeEvent(input$resample_many_boxes, {
    # Check if original sample exists
    if (is.null(rv$original_sample)) {
      showNotification("STEP 1 REQUIRED: Please click 'Press to Choose a Random Box of Candy' first!",
                       duration = 5, type = "error")
      return()
    }
    sample_size <- input$n_sample_ui
    
    new_resamples <- replicate(input$n_bulk_ui, draw_bootstrap_sample(rv$original_sample, sample_size))
    new_props <- apply(new_resamples, 2, function(x) sum(x) / sample_size)
    rv$current_resample <- new_resamples[, input$n_bulk_ui]
    new_data <- data.frame(prop_red = new_props)
    rv$bootstrap_dist <- rbind(rv$bootstrap_dist, new_data)
  })
  
  # Output: Single Resample Proportion Display
  output$resample_prop_single_ui <- renderUI({
    req(rv$current_resample)
    N_current <- length(rv$current_resample)
    n_red <- sum(rv$current_resample)
    prop_red <- n_red / N_current
    
    div(
      h4(paste0("Proportion of Lollipops After Sampling With Replacement: ", round(prop_red, 3)), style = "color: #FF4B3E;")
    )
  })	
  
  # Output for Current Resample Visualization
  output$resample_candy_vis <- renderUI({
    req(rv$current_resample)
    N_current <- length(rv$current_resample)
    vis_candies <- head(rv$current_resample, 100)
    
    candy_divs <- lapply(vis_candies, function(candy) {
      # 1 = Red Candy (🍭), 0 = Green Candy (🍬)
      emoji <- ifelse(candy == 1, CANDY_RED, CANDY_GREEN)
      
      tags$span(
        class = "candy-emoji",
        emoji
      )
    })
    
    tagList(
      div(candy_divs),
      if (N_current > 100) {
        p(paste0("...and ", N_current - 100, " more candies."))
      }
    )
  })
  
  # Output: Bootstrap Distribution Stats (UPDATED CONTENT)
  output$bootstrap_stats_ui <- renderUI({
    req(rv$original_sample)
    n_resamples <- nrow(rv$bootstrap_dist)
    mean_prop_orig <- sum(rv$original_sample) / length(rv$original_sample)
    
    
    div(
      h5(paste("Total Resamples:", n_resamples)), # Changed to show actual count from rv$bootstrap_dist
      h5(paste("Sample Size:", input$n_sample_ui)),
      h5(paste("Original Sample Proportion:", round(sum(rv$original_sample) / length(rv$original_sample), 3)), style = "color: red"),
      h5(paste("Mean of Bootstrap Sample Proportions:", round(mean( rv$bootstrap_dist$prop_red), 3)), style = "color: blue")
    )
  })
  
  
  # Output: Bootstrap Distribution Histogram
  output$bootstrap_hist_plot <- renderPlot({
    # FIX: Change the minimum requirement from >= 20 to > 0 (or >= 1) to allow single-sample plotting.
    req(nrow(rv$bootstrap_dist) > 0)
    
    prop_data <- rv$bootstrap_dist$prop_red
    
    if(input$n_sample_ui <= 10){
      usebinwidth = 0.1
    }else if(input$n_sample_ui <= 25){
      usebinwidth = 0.05
    }else{
      usebinwidth = 0.01
    }
    
    # Get the original sample's proportion
    mean_prop_orig <- sum(rv$original_sample) / length(rv$original_sample)
    
    p <- ggplot(rv$bootstrap_dist, aes(x = prop_red)) +
      xlim(0,1) +
      geom_histogram(binwidth = usebinwidth, fill = "grey", color = "white", alpha = 0.8) +
      
      # Change to use aes() and guide for legend
      geom_vline(aes(xintercept = mean_prop_orig, linetype = "Original Sample Proportion", color = "Original Sample Proportion"), size = 0.5) +
      # Line for Mean of Bootstrap Proportions (Only plot if we have at least 1 sample)
      geom_vline(aes(xintercept = mean(prop_data), linetype = "Mean of Bootstrap Proportions", color = "Mean of Bootstrap Proportions"), size = 0.5) +
      # Use scale_color/linetype_manual for legend
      scale_color_manual(name = "Legend",
                         values = LINE_COLORS[c("Original Sample Proportion", "Mean of Bootstrap Proportions")]) +
      scale_linetype_manual(name = "Legend",
                            values = LINE_LINETYPES[c("Original Sample Proportion", "Mean of Bootstrap Proportions")]) +
      
      theme_minimal(base_size = 15) +
      labs(
        x = "Proportion of Lollipops in Bootstrap Sample",
        y = "Frequency"
      ) +
      # Annotations for main lines
      annotate("text", x = mean_prop_orig, y = Inf,
               label = paste0(round(mean_prop_orig, 3)),
               vjust = 2, hjust = 1.05, color = "red") +
      # Only show mean annotation if there are resamples
      if (length(prop_data) > 0) {
        annotate("text", x = mean(prop_data), y = Inf,
                 label = paste0(round(mean(prop_data), 3)),
                 vjust = 2.5, hjust = -0.02, color = "blue")
      } else {
        NULL
      }
    
    # Check if the user selected the 95% Plausible Range
    if (input$show_plausible_range) {
      if (length(prop_data) >= 20) { # Keep 20 as the threshold for CI calculation
        # Calculate the 95% Plausible Range (Percentile Method)
        ci_lower <- quantile(prop_data, 0.025)
        ci_upper <- quantile(prop_data, 0.975)
        
        # Add the Plausible Range visualization to the plot
        p <- p +
          # Lower CI boundary
          geom_vline(xintercept = ci_lower, linetype = "dotdash", color = COLOR_CI, size = 1) +
          # Upper CI boundary
          geom_vline(xintercept = ci_upper, linetype = "dotdash", color = COLOR_CI, size = 1) +
          
          # Add annotation for the range
          annotate("rect", xmin = ci_lower, xmax = ci_upper, ymin = -Inf, ymax = Inf,
                   fill = COLOR_CI, alpha = 0.1) +
          
          # Annotate the CI values
          annotate("text", x = ci_lower, y = max(ggplot_build(p)$data[[1]]$count) * 0.9,
                   label = paste0("2.5th quantile: ", round(ci_lower, 3)), vjust = 20, hjust = 1.1, color = COLOR_CI) +
          annotate("text", x = ci_upper, y = max(ggplot_build(p)$data[[1]]$count) * 0.9,
                   label = paste0("97.5th quantile: ", round(ci_upper, 3)), vjust = 20, hjust = -0.1, color = COLOR_CI) +
          annotate("text", x = (ci_lower + ci_upper) / 2, y = max(ggplot_build(p)$data[[1]]$count) * 0.8,
                   label = "", vjust = 0.5, color = COLOR_CI, fontface = "bold")
        
      } else {
        # Notification if too few resamples for CI
        showNotification("Need at least 20 resamples to calculate the 95% Plausible Range.",
                         duration = 3, type = "warning")
      }
    }
    
    return(p)
  })
  
}

# --- Run the application ---
shinyApp(ui = ui, server = server)