library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("spacelab"),
  
  tags$head(
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Lobster&display=swap');
    "))
  ),
  
  headerPanel(
    h1("3D qHTS Visualizer", 
       style = "font-family: 'Lobster';
        color: #4d3a7d;")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
                 
        sidebarPanel(
            fileInput("ifile", "Choose CSV File"),
            textInput("con", "Concentrations", placeholder = "Comma separated"),
            br(),
            helpText("All colors can be found ", a("Here", href = "https://www.r-graph-gallery.com/colors.html")),
            textInput("col_1", "Active points", placeholder = "royalblue3"),
            textInput("col_2", "Inactive points", placeholder = "gray"),
            textInput("col_3", "Active curves", placeholder = "darkgreen"),
            textInput("keyword_1", "Keyword", placeholder = "Active"),
            
            actionButton("go", "Go", style = " color: #7CFC00;"),

        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          h2("App Description", 
             style = "font-family: 'Lobster'"),
          p("Here we provide an overview of the program",
            "and its funcionalities. If you are interested in the pharmacological",
            "research and discovery purposes of qHTS please click", 
            a("here", href = "https://www.pnas.org/content/103/31/11473")),
          p(""),
          
          h2("Data Preparation",style = "font-family: 'Lobster';"),
          tags$ol(
            tags$li("Use ", a("PUBCHEM", href = "https://pubchem.ncbi.nlm.nih.gov/"), "to download data."),
            tags$li("Open the downloaded CSV file"),
            tags$li("Delete the rows labeled", strong("RESULT_TYPE, RESULT_DESCR, and RESULT_UNIT")),
            tags$li(strong("Sort"), "the data as desired (by compound ID, similarity, etc.)"),
            tags$li("Insert two empty columns at the leftmost side of excel."),
            tags$li("Label column A as", strong("Counter"), "- NOTE: This is for record"),
            tags$li("Label column B as", strong("Fit_Output"), "(exactly that name)."),
            tags$li("Put a value of", strong("1"), "under",strong("Fit_Output"),"for any compound for which to include a fit line"),
            tags$li("Put a value of", strong("0"), "under",strong("Fit_Output"),"for any compound for which to render points"),
            tags$li("Scroll to the right and find the 'Activity' points and rename them starting with ", strong("Data0"), "for the",
                    "first activity point and ", strong("Data1"), "for the second point and so on... until all activity points",
                    "have been renamed."),
            ),
          
          h2("Side Panel Key Words", style = "font-family: 'Lobster"),
          tags$ol(
            tags$li(strong("Concentrations: "), "Log base for the titration points"),
            tags$li(strong("Active points: "), "Color for titration points of compounds of interst"),
            tags$li(strong("Inactive points: "), "Color for titration points of inactive compounds"),
            tags$li(strong("Active curves: "), "Color for titration curves of compounds of interest"),
            tags$li(strong("Keywords: "), "Defining word to determine active curves")
            ),
          
          h2("Downloading",style = "font-family: 'Lobster'"),
          tags$ol(
            tags$li("Image is interactive - rotate however you please"),
            tags$li(strong("Right Click"),"on the image"),
            tags$li("Select", strong("Save as")),
            tags$li("Image will automatically start downloading")
          ),
          
          uiOutput("distPlot"),
            
          img(src = "image_app.png", height = 200, width = 200)
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    observeEvent(
        eventExpr = input[["go"]],
        output$distPlot <- renderUI({
            
            conc  <<- as.double(unlist(strsplit(input$con, ",")))
            color <- colors()
            
            if(any(color == input$col_1) == TRUE){
                col_1 <<- input$col_1
            } else col_1 <<- "royalblue3"
            
            if(any(color == input$col_2) == TRUE){
                col_2 <<- input$col_2
            } else col_2 <<- "gray"
            
            if(any(color == input$col_3) == TRUE){
                col_3 <<- input$col_3
            } else col_3 <<- "darkgreen"
            
            if(is.null(input$keyword_1) == TRUE){
                keyword_1 <<- input$keyword_1
            } else keyword_1 <<- "Active"
            
            inFile <<- input$ifile
            
            if(is.null(inFile))
                return(NULL)
            ifile <<- inFile$datapath
            
            source("main.R")
            
        })
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
