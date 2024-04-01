library(shiny)
library(shinyalert)
library(shinyWidgets)
library(tidyverse)
library(viridis)

source("helpers.R")

# application UI ----
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css",
      crossorigin = "anonymous"
    )
  ),
  
  fluidPage(
    # title panel ----
    titlePanel(
      windowTitle = "Detecting Lies Through Others' Eyes",
      tags$div(
        class = "title-text",
        tags$h1("Detecting Lies Through Others' Eyes:"),
        tags$span(
          class = "subtitle-text",
          "an interactive data visualization accompaniment to",
          tags$a(
            "Tay et al. (2024)",
            href = "https://doi.org/10.1016/j.jecp.2024.105863"
          )
        ),
        hr()
      )
    ),
    
    # body ----
    sidebarLayout(
      
      # sidebar panel ----
      sidebarPanel2(
        
        # both
        radioGroupButtons(
          inputId = "tabName",
          label = "Choose a plot:",
          choices = c("Experiment", "Covariates"),
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon")),
          status = "primary",
          justified = TRUE
        ),
        hr(),
        conditionalPanel(
          condition = "input.tabName == 'Experiment'",
          materialSwitch(
            inputId = "showMethod",
            label = strong("Show experimental design:"), 
            value = F,
            status = "primary"
          )
        ),
        pickerInput(
          inputId = "x_axis",
          label = "Select x-axis variable:", 
          choices = dropdown_choices,
          selected = "ageMonths"
        ),
        
        # experiment
        
        conditionalPanel(
          condition = 'input.tabName == "Experiment" && ["ageMonths","bds","tom_2orderTotal","tom_2orderBelief",
          "tomR_KnowWant","SC_total","SC_thinkTotal","SC_sayTotal"].includes(input.x_axis)',
          sliderTextInput(
            inputId = "x_breaks",
            label = "Select number of bins:",
            selected = 5,
            choices = seq(2, 7, 1),
            grid = T
          )
        ),
        
        # covariates
        conditionalPanel(
          condition = "input.tabName == 'Covariates'",
          pickerInput(
            inputId = "y_axis",
            label = "Select y-axis variable:",
            choices = dropdown_choices,
            selected = "tom_2orderTotal"
          )
        ),
        conditionalPanel(
          condition = 'input.tabName == "Covariates" &&
          ["ageMonths","bds","tom_2orderTotal","tom_2orderBelief","tomR_KnowWant",
          "SC_total","SC_thinkTotal","SC_sayTotal"].includes(input.x_axis) &&
          ["ageMonths","bds","tom_2orderTotal","tom_2orderBelief","tomR_KnowWant",
          "SC_total","SC_thinkTotal","SC_sayTotal"].includes(input.y_axis)',
          pickerInput(
            inputId = "fill_colour",
            label = "Select colour/fill variable:",
            choices = dropdown_choices,
            selected = "bds"
          )
        ),
        out = tags$div(
          class = "apa-citation",
          tags$strong("Reference:"),
          tags$p(
            "Tay, C., Ng, R., Ye, N. N., & Ding, X. P. (2024). Detecting lies through others’ eyes: Children use perceptual access cues to evaluate listeners’ beliefs about informants’ deception.",
            tags$em("Journal of Experimental Child Psychology, 241,"),
            "105863.",
            tags$a("https://doi.org/10.1016/j.jecp.2024.105863",
                 href = "https://doi.org/10.1016/j.jecp.2024.105863")
          )
        )
      ),
      
      # main panel ----
      mainPanel(
        
        tabsetPanel(
          id = "tabShown",
          type = "hidden",
          
          # experiment tab ----
          tabPanel(
            "Experiment",
            
            # method ----
            conditionalPanel(
              condition = "input.showMethod == true",
              tags$div(
                tags$link(href = "https://mfr.osf.io/static/css/mfr.css", media = "all", rel = "stylesheet"),
                tags$div(id = "mfrIframe", class = "mfr mfr-file"),
                tags$script(src = "https://mfr.osf.io/static/js/mfr.js"),
                tags$script(HTML("
                  function renderMfr() {
                    var mfrRender = new mfr.Render(\"mfrIframe\", \"https://mfr.ca-1.osf.io/render?url=https://osf.io/download/45qy7/?direct%26mode=render\");
                  }
                  if (window.$) {
                    renderMfr();
                  } else {
                    var jq = document.createElement('script');
                    document.head.appendChild(jq);
                    jq.onload = function() {
                      renderMfr();
                    }
                    jq.src = 'http://code.jquery.com/jquery-1.11.2.min.js';
                  }
                "))
              ),
              hr()
            ),
            # plot ----
            tags$div(
              class = "question-text",
              "Children's responses to the target question: ",
              em('"Does [the receiver] think that [the informant] is lying or telling the truth?"')
            ),
            tags$div(
              class = "response-options",
              tags$span(class = "option-lie"),
              tags$span("Lie"),
              tags$span(class = "option-truth"),
              tags$span("Truth")
            ),
            plotOutput("exptPlot"),
          ),
          
          # covariates tab ----
          tabPanel(
            "Covariates",
            plotOutput("covariatePlot")
          )
        )
      )
    ),
    tags$footer(
      tags$em("Last updated: Feb '24 | Built in R Shiny by "),
      tags$a("Cleo Tay", href = "https://cleotayzq.github.io"),
    )
  )
)
    
# application server logic ----
server <- function(input, output, session) {
  
  # update tab shown
  observeEvent(input$tabName, {
    updateTabsetPanel(session, "tabShown", selected = input$tabName)
  })
  
  # update max bin number based on variable
  observe({
    if (input$tabName == "Experiment" && input$x_axis %in% continuous_vars) {
      if (input$x_axis == "ageMonths") {
        num_distinct_values <- 12
      } else {
        num_distinct_values <- n_distinct(all_data[[input$x_axis]])
      }
      updateSliderTextInput(session, "x_breaks", choices = seq(2, num_distinct_values, 1))
    }
  })
  
  # if x_axis and y_axis are the same, update y_axis to a random variable
  observe({
    if (input$tabName == "Covariates" && input$y_axis == input$x_axis) {
      new_y_axis <- sample(setdiff(unlist(dropdown_choices, recursive = TRUE), input$x_axis), 1)
      updatePickerInput(session, "y_axis", selected = new_y_axis)
      shinyalert("The x-axis variable is the same as the y-axis variable!", "Picking a random variable for the y-axis...", type = "warning")
    }
  })
  
  # get labels for plots
  get_label <- function(selected_choice, choices_list) {
    for (group_name in names(choices_list)) {
      group <- choices_list[[group_name]]
      for (item_name in names(group)) {
        item <- group[[item_name]]
        if (item == selected_choice) {
          return(item_name)
          break
        }
      }
    }
  }
  
  x_axis_label <- reactive({
    get_label(input$x_axis, dropdown_choices)
  })
  y_axis_label <- reactive({
    get_label(input$y_axis, dropdown_choices)
  })
  fill_col_label <- reactive({
    get_label(input$fill_colour, dropdown_choices)
  })
  
  # clean data for experimental plot
  exptPlot_data <- reactive({
    x_axis <- input$x_axis
    
    long_data <- all_data %>%
      select(all_of(x_axis), exptNP_sThinkHlie, exptP_sThinkHlie)
    
    if (x_axis %in% continuous_vars){
      
      rename_cut <- function(my_input){
        my_string <- as.character(my_input)
        result <- str_match(my_string, "^(.)(.+),(.+).$")
        first_int <- ceiling(as.numeric(result[3]))
        if (result[2] == "["){
          first_int <- ceiling(as.numeric(result[3]))
        } else if (result[2] == "("){
          first_int <- ceiling(as.numeric(result[3])+.01)
        }
        second_int <- floor(as.numeric(result[4]))
        if (first_int == second_int){
          new_paste <- paste(first_int)
        } else {
          new_paste <- paste(first_int, "-", second_int)
        }
        return(new_paste)
      }
      
      x_breaks <- as.integer(input$x_breaks)
      long_data <- long_data %>%
        mutate_at(1, ~cut_interval(., n = x_breaks), closed = "left") %>%
        rowwise() %>%
        mutate_at(1, ~rename_cut(c_across(1)))
    }
    
    long_data <- long_data %>%
      pivot_longer(2:3, names_sep = "_",
                   names_to = c("expt_cond", ".value")) %>%
      na.omit() %>%
      mutate_all(as.factor) %>%
      mutate(expt_cond = ifelse(expt_cond == "exptNP", "Ignorant Receiver\n(Did Not Peek)",
                                "Knowledgeable Receiver\n(Peeked)"))
    return(long_data)
  })
  
  # ggplot script for experimental plot
  exptPlot_ggscript <- reactive({
    plot <- ggplot(exptPlot_data(), aes(x = stringr::str_wrap(.data[[input$x_axis]], 10), fill = sThinkHlie)) +
      geom_bar(position = "fill", colour = "black") +
      facet_grid(~expt_cond) +
      geom_hline(yintercept = .5, linetype = "dashed", linewidth = .25) +
      geom_label(aes(x = .data[[input$x_axis]], 
                     label = after_stat(count),
                     group = sThinkHlie),
                 stat = "count",
                 position = position_fill(vjust = .5),
                 label.size = NA, show.legend = F) +
      scale_y_continuous(name = "Proportion", labels = function(x) paste0(x*100, "%")) +
      theme_bw() +
      # labs(fill = stringr::str_wrap("Does [the receiver] think that [the informant] is lying or telling the truth?")) +
      xlab(x_axis_label()) +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 14),
            # legend.text = element_text(face = "bold", size = 14, hjust = 0.5),
            # legend.title = element_blank(),
            strip.text = element_text(face = "bold", size = 12))
      # scale_fill_discrete(guide = guide_legend(title.position = "top"))
    return(plot)
  })

  # clean data for covariates plot
  covPlot_data <- reactive({
    x_axis <- input$x_axis
    y_axis <- input$y_axis
    fill_colour <- input$fill_colour
    
    if (input$x_axis %in% continuous_vars & input$y_axis %in% continuous_vars){
      plot_data <- all_data %>%
        select(all_of(x_axis), all_of(y_axis), all_of(fill_colour))
    } else {
      plot_data <- all_data %>%
        select(all_of(x_axis), all_of(y_axis))
    }
    
    plot_data <- plot_data %>%
      na.omit()
    
    return(plot_data)
  })
    
  # ggplot script for covariates plot
  covariatePlot_ggscript <- reactive({
    
    # continuous y-axis
    if (input$y_axis %in% continuous_vars){
      plot <- ggplot(covPlot_data(), aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]])) +
        xlab(x_axis_label()) +
        ylab(y_axis_label()) +
        scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))))
      # continuous x-axis too
      if (input$x_axis %in% continuous_vars) {
        plot <- plot +
          geom_jitter(alpha = .75, size = 2, pch = 21, colour = "black",
                      height = .1, width = .1,
                      aes(fill = .data[[input$fill_colour]])) +
          geom_smooth(method = "lm", formula = y ~ x, colour = "black") +
          scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
          labs(fill = fill_col_label())
        if (input$fill_colour %in% continuous_vars){
          plot <- plot +
            scale_fill_viridis(option = "plasma")
        }
      # categorical x-axis
      } else {
        plot <- plot +
          geom_violin(aes(fill = .data[[input$x_axis]])) +
          geom_boxplot(width = .1) +
          guides(fill = "none") 
      }
    # categorical y-axis
    } else {
      # continuous x-axis - same as cat-x, cont-y but flipped
      if (input$x_axis %in% continuous_vars){
        plot <- ggplot(covPlot_data(), aes(y = .data[[input$x_axis]], x = .data[[input$y_axis]])) +
          geom_violin(aes(fill = .data[[input$y_axis]])) +
          geom_boxplot(width = .1) +
          xlab(y_axis_label()) +
          ylab(x_axis_label()) +
          guides(fill = "none") +
          scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
          coord_flip()
      # categorical x- and y-axis
      } else {
        plot <- ggplot(covPlot_data(), aes(x = .data[[input$x_axis]], fill = .data[[input$y_axis]])) +
          geom_bar(position = "fill", colour = "black") +
          geom_label(aes(x = .data[[input$x_axis]], 
                         label = after_stat(count),
                         group = .data[[input$y_axis]]),
                     stat = "count",
                     position = position_fill(vjust = .5),
                     label.size = NA, show.legend = F) +
          scale_y_continuous(name = "Proportion", labels = function(x) paste0(x*100, "%")) +
          labs(fill = y_axis_label()) +
          xlab(x_axis_label())
        
      }
    }
    
    plot <- plot +
      theme_bw() +
      theme(legend.position = "top",
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 14),
            legend.text = element_text(size = 12),
            legend.title = element_text(face = "bold", size = 14)) 
    return(plot)
  })
  
  # render plots from ggscript for main panel ----
  output$exptPlot <- renderPlot({
    exptPlot_ggscript()
  })
  output$covariatePlot <- renderPlot({
    covariatePlot_ggscript()
  })
}

# run the application ----
shinyApp(ui = ui, server = server)
