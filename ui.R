# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Student Pairs Matcher"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose Excel File",
                multiple = FALSE,
                accept = NULL),
      
      p("If you are unsure how to format your input sheet, please use the template below."),
      
      # Button
      downloadButton("downloadData2", "Download template"),
      
      # Horizontal line ----
      tags$hr(),
      
      p("Please note the following: Excel files should be uploaded with preferences for each study on sheet 1, 2 and 3 respectively. The order of the student headings must be the same for each sheet. Please send any questions or comments to zxtonizx@hotmail.com. If you would like a similar app made, feel free to get in touch."),
      p("The problem this tool solves is famously known as the Stable Roommates Problem."),
      
      p("For developers: https://github.com/zxtonizx/stableRoommatesApp."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Button
      downloadButton("downloadData", "Download matches"), width = 4
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Study 1",     mainPanel(
                    
                    # Output: Data file ----
                    h4("Preferences"),
                    p("With any duplicates removed."),
                    tableOutput('contents1'), 
                    
                    h4("Matchings"),
                    tableOutput('matches1'), 
                    
                    h4("Scores"),
                    p("The lower the score, the better."),
                    tableOutput('scores1') 
                    
                  )),
                  tabPanel("Study 2",     mainPanel(
                    
                    # Output: Data file ----
                    h4("Preferences"),
                    p("With any duplicates removed and matches from Study 1 removed."),
                    tableOutput('contents2'), 
                    
                    h4("Matchings"),
                    tableOutput('matches2'), 
                    
                    h4("Scores"),
                    p("The lower the score, the better."),
                    tableOutput('scores2') 
                    
                  )),
                  tabPanel("Study 3",     mainPanel(
                    
                    # Output: Data file ----
                    h4("Preferences"),
                    p("With any duplicates removed and matches from Study 1 and 2 removed."),
                    tableOutput('contents3'), 
                    
                    h4("Matchings"),
                    tableOutput('matches3'), 
                    
                    h4("Scores"),
                    p("The lower the score, the better."),
                    tableOutput('scores3') 
                    
                  ))
      )
      
    )
    
  )
)