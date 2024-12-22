# Load packages
library(shiny)
library(tidyverse)
library(DT)

# Data Preparation (Light tasks, as most of the data preparation was done in the Data Cleaning file)
# Load in the cleaned dataset
gpa_data = read_csv("Data/UIUC Subjects by GPA.csv")
gpa_data = gpa_data %>%
  mutate(Performance = ifelse(GPA >= 3.67, "Excellent Performance",
                       ifelse(GPA < 3.67 & GPA >= 2.67, "Good Performance",
                       ifelse(GPA < 2.67 & GPA >= 1.67, "Satisfactory Performance",
                       ifelse(GPA < 1.67 & GPA >= 0.67, "Less than Satisfactory Performance", 
                       ifelse(GPA > 0.67, "Failing"))))),
         Performance = factor(Performance, levels = c("Excellent Performance", 
                                                      "Good Performance", 
                                                      "Satisfactory Performance", 
                                                      "Less than Satisfactory Performance", 
                                                      "Failing")))

# Define UI 
ui = fluidPage(
  titlePanel("UIUC Subjects & Courses GPA Analysis"),
  tabsetPanel(
    tabPanel(
      title = "UIUC Subjects & Courses GPA Analysis",
      sidebarLayout(
        sidebarPanel(
          # Define the different user features
          textInput("subject", "Enter Subject (e.g., CS, MATH):"),
          selectInput("color_level", "Select Color Variable (Course Level):", 
                      choices = c("No Variable", "Course Level"),
                      selected = "No Variable"),
          radioButtons("plot_type", "Select Plot Type:", 
                       choices = c("ggplot", "Base R"), 
                       selected = "ggplot"),
          DT::DTOutput("subject_title_and_subject_abbreviation")
        ),
        
        mainPanel(
          plotOutput("GPA_plot"),
          downloadButton("download_box_plot", "Download Plot as PNG")
        )
      )
    ),
    
    # Create "About" tab
    tabPanel(
      title = "About",
      fluidPage(
        titlePanel("About the Dataset and Shiny App"),
        h3("UIUC Subjects GPA"),
        p("The original data was downloaded from: ",
          tags$a(href = "https://github.com/wadefagen/datasets/blob/main/gpa/uiuc-gpa-dataset.csv",
                 "https://github.com/wadefagen/datasets/blob/main/gpa/uiuc-gpa-dataset.csv")),
        p("This app aims to explore the average GPAs across different course subjects and levels, 
          helping users identify trends and patterns in academic performance."),
        p("After the data cleaning and wrangling, the UIUC Subjects GPA dataset contains 2,592 observations
        and 7 variables (Year, Subject, Number, Course Title, GPA, Level, Subject Full Title)"),
        h3("Developer Information."),
        p("Shiny App Developed by: Dhruv Kaul"),
        p("Contact Information: dkaul4@illinois.edu")
      )
    )
  )
)

# Define server logic 
server = function(input, output) {
  subject_vector = reactive({
    unique(trimws(unlist(str_split(str_to_upper(input$subject), pattern = ",\\s*"))))
  })
  color_level = reactive(input$color_level)
  plot_type = reactive(input$plot_type)
  output$GPA_plot = renderPlot({
    req(subject_vector(), color_level(), plot_type())
    
    if(plot_type() == "ggplot") {
      ## ggplot graph:
      plot = gpa_data %>%
        filter(Subject %in% subject_vector()) %>%
        ggplot(aes(x = interaction(Subject, Level), y = GPA))
      if (color_level() == "Course Level") {
        plot = plot + geom_boxplot(aes(fill = Level)) +
          labs(fill = "Level")
      } else {
        plot = plot + geom_boxplot()
      }
      plot +
        labs(title = "GPA Variability by Subject and Level",
             x = "Subject and Level", y = "GPA") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # base r plot
      subset_gpa_data = gpa_data %>%
        filter(Subject %in% subject_vector())
      if (color_level() == "Course Level") {
        
        subset_gpa_data$Level = as.factor(subset_gpa_data$Level)
        number_of_colors = length(levels(subset_gpa_data$Level))
        colors = rainbow(number_of_colors)
        
        color_code = subset_gpa_data %>%
          arrange(Subject) %>%
          group_by(Subject) %>%
          distinct(Level) %>%
          pull(Level)
        
        par(mar = c(12, 4, 4, 2))
        
        boxplot(GPA ~ Subject * Level, data = subset_gpa_data, col = colors[color_code],
                xlab = "",
                ylab = "GPA",
                main = "GPA Variability by Subject",
                las = 2,
                cex.axis = 0.8
        )
        title(xlab = "Subject", line = 10)
        legend("bottomright", legend = levels(subset_gpa_data$Level), fill = colors, title = "Course Level", cex = 0.8)
      } else {
        subset_gpa_data$Level = as.factor(subset_gpa_data$Level)
        par(mar = c(12, 4, 4, 2))
        
        boxplot(GPA ~ Subject * Level, data = subset_gpa_data,
                xlab = "",
                ylab = "GPA",
                main = "GPA Variability by Subject",
                las = 2,
                cex.axis = 0.8
        )
        title(xlab = "Subject", line = 10)
      }
    }
  })
  
  output$download_box_plot = downloadHandler(
    filename = function() {
      paste("GPA Variability by Subject -", Sys.time(),
            ".png", sep = "")
    },
    
    content = function(file) {
      if(plot_type() == "ggplot") {
        ggsave(file, plot = last_plot(), bg = "white", width = 8, height = 8, dpi = 500)
      } else {
       png(file)
        subset_gpa_data = gpa_data %>%
          filter(Subject %in% subject_vector())
        if (color_level() == "Course Level") {
          subset_gpa_data$Level = as.factor(subset_gpa_data$Level)
          
          number_of_colors = length(levels(subset_gpa_data$Level))
          colors = rainbow(number_of_colors)
          
          color_code = subset_gpa_data %>%
            arrange(Subject) %>%
            group_by(Subject) %>%
            distinct(Level) %>%
            pull(Level)
          
          par(mar = c(20, 4, 4, 2))
          
          boxplot(GPA ~ Subject * Level, data = subset_gpa_data, col = colors[color_code],
                  xlab = "",
                  ylab = "GPA",
                  main = "GPA Variability by Subject",
                  las = 2,
                  cex.axis = 0.8
          )
          title(xlab = "Subject", line = 10)
          legend("bottomright", legend = levels(subset_gpa_data$Level), fill = colors, title = "Course Level", cex = 0.8)
        } else {
          boxplot(GPA ~ Subject * Level, data = subset_gpa_data,
                  xlab = "",
                  ylab = "GPA",
                  main = "GPA Variability by Subject",
                  las = 2,
                  cex.axis = 0.8
          )
          title(xlab = "Subject", line = 10)
        }
        dev.off()
      }
    }
  ) 
  
  output$subject_title_and_subject_abbreviation = DT::renderDataTable({
    gpa_data %>%
      select(Subject, `Subject Full Title`) %>%
      distinct()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

