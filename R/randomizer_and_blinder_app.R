
library(shiny)
library(tidyverse)
library(shinythemes)

## Function to generate table
gen_table <- function(seed, block_sizes = c(2, 4), length = 10,
                      length.max = 1000,
                      treatment_names = c("Treatment", "Placebo"),
                      n_strata = 2, strata_names = NA) {

  if(is.na(strata_names) | strata_names == "") {
    strata_names = paste0("Strata ", 1:n_strata)
  } else {
    strata_names = strsplit(strata_names, ",")[[1]]
  }

  block_sizes <- as.numeric(block_sizes)
  possible_ids <- paste0("KIT-", apply(expand.grid(1:9, 0:9, 0:9, 0:9, 0:9), 1, paste0, collapse = ""))

  set.seed(seed)

  ## Get block sizes
  if(length(block_sizes) > 1) {
    ss <- 0
    bs <- NULL

    # Generate 10x too many blocks, in case more needed later
    while(ss < length.max) {
      bs <- c(bs, sample(block_sizes, 1))
      ss <- sum(bs)
    }


  } else {
    bs <- rep(block_sizes, ceiling((length.max)/block_sizes))
  }

  B <- length(bs)

  rtab <- lapply(seq(1, B, by = 1), function(b) {
    block_size <- bs[b]
    opts_b <- c(sapply(treatment_names, function(o) rep(o, block_size/2)))
    cbind(Block = c(rep(c(b), each = block_size*n_strata)), "Block SeqID" = 1:block_size,
          "Block Size" = block_size,
          "Stratum" = rep(strata_names, each = block_size),
          "Allocation" = c(sapply(strata_names, function(s) sample(opts_b))))
  }) %>%
      Reduce(rbind, .) %>%
      cbind(., "KitID" = sample(possible_ids, nrow(.)))


  data.frame(rtab, check.names = F, stringsAsFactors = F) %>%
    group_by(Stratum) %>%
    mutate(SeqID = 1:n()) %>%
    ungroup() %>%
    filter(SeqID <= length) %>%
    relocate(Stratum, SeqID) %>%
    arrange(Stratum, SeqID)
}


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),

    # Application title
    titlePanel("Randomizer and Blinder Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("seed", "Set the seed (pick an integer from 1 to 1e09, then click 'update')", 123456789),
            textInput("treatment_1", "Treatment 1 name", "Treatment"),
            textInput("treatment_2", "Treatment 2 name", "Placebo"),
            numericInput("length", "Number to randomize per strata", value = 100),
            numericInput("length.max", "Maximum conceivable sample size (warning: changes allocations)", value = 1000),
            numericInput("strata", "Number of strata", value = 4),
            textInput("strata_names", "Strata names (comma separated)", value = "Yale,Harvard,BU,CU"),
            submitButton("Update"), hr(),
            downloadButton("dl_blind", label = "Download Blinded Data"), br(),
            downloadButton("dl_unblind", label = "Download Unblinded Data"), br()
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Allocation Tables", DT::DTOutput("main_table")),
            tabPanel("Summary Plot", plotOutput("main_plot")),
            tabPanel("About", uiOutput("about"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    treatment_names <- reactive(c(input$treatment_1, input$treatment_2))
    rtab <- reactive(gen_table(input$seed, length = input$length,
                               length.max = input$length.max,
                               treatment_names = treatment_names(),
                               n_strata = input$strata,
                               strata_names = input$strata_names))

    output$main_table <- DT::renderDT(
      rtab(), options = list(autoWidth = T, pageLength = 50, ordering = F)
    )

    output$main_plot <- renderPlot({
      plot_data <- rtab() %>%
        group_by(Stratum) %>%
        mutate(Cumulative_Enrollment_1 = cumsum(Allocation == input$treatment_1),
               Cumulative_Enrollment_2 = cumsum(Allocation == input$treatment_2)) %>%
        ungroup()

        p1 <- plot_data %>%
          ggplot(aes(x = as.numeric(SeqID), color = Stratum, y = Cumulative_Enrollment_1)) +
          geom_line() +
          theme_minimal()
        p2 <- plot_data %>%
          ggplot(aes(x = as.numeric(SeqID), color = Stratum, y = Cumulative_Enrollment_2)) +
          geom_line() +
          theme_minimal()
        ggpubr::ggarrange(p1, p2)
    })

    output$dl_blind <- downloadHandler(
        filename = function() {
            paste('blinded_data-', Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            rtab() %>%
            select(Stratum, SeqID, KitID) %>%
            write_csv(con)
        }
    )

    output$dl_unblind <- downloadHandler(
        filename = function() {
            paste('unlinded_data-', Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            write_csv(rtab(), con)
        }
    )

    output$about <- renderUI({
      version <- as.character(utils::packageVersion("CIDAtools"))

      HTML(
        paste0(
          "<p>An unblinded statistician should coordinate with the study pharmacist ",
          "using unblinded spreadsheets, and send blinded spreadsheets to the study team. ",
          "The statisticians may also request a 'masked' version wherein the treatment ",
          "names are replaced with 'A' or 'B' at the discretion of the unblinded statistician.</p>",
          "<p>This app was built by Ryan Peterson. Code available on CIDAtools's GitHub (version ", version, ").</p>"))
    })

}

# Run the application
shinyApp(ui = ui, server = server)
