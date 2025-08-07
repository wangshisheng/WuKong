library(shiny)
library(shinydashboard)
library(httr)
library(ollamar)
library(markdown)
library(ggplot2)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(esquisse)
library(ggsci)
list_modelsx <- ollamar::list_models()

ui <- dashboardPage(
  title = "Correlation plot with LLM Assistance",
  dashboardHeader(title = "Correlation plot with LLM Assistance", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("I. Data Input", tabName = "uploaddata", icon = icon("file")),
      menuItem("II. Manual Analysis", tabName = "adjustparams", icon = icon("dashboard")),
      menuItem("III. LLM-Assisted Analysis", tabName = "references", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="busystyle.css"),
        tags$link(rel="stylesheet", type="text/css", href="mainstyle.css"),
        tags$script(type="text/javascript", src = "busy.js")
      ),
      tags$head(tags$style(HTML("
      body {
            min-width: 1400px;
            overflow-x: auto;
          }
    .user, .assistant {
      margin-bottom: 15px;
    }
    .user {
      text-align: right;
    }
    .assistant {
      text-align: left;
    }
    .user .message {
      background-color: #e0f7fa;
      margin-left: auto;
    }
    .assistant .message {
      background-color: #f1f8e9;
    }
    .input-container {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin-top: 10px;
    }
        .chat-box {
    max-height: 440px;
    height:440px;
    overflow-y: auto;
    border: 1px solid #ccc;
    padding: 0px;
    margin-bottom: 0px;
        }
#user_input {
            border-color: #6495ED; /* Change this to your desired border color */
}
          
  "))),
      # Enable Bootstrap tooltips
      tags$script(HTML("
        $(function () {
          $('[data-toggle=\"tooltip\"]').tooltip();
        });
      ")),
      tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#example_link').on('click', function(e) {
        e.preventDefault();
        Shiny.setInputValue('example_clicked', true, {priority: 'event'});
      });
    });
  "))
    ),
    div(class = "busy",
        h2(strong("Wukong is calculating, please wait...")),
        img(src="rmd_loader.gif")
    ),
    tabItems(
      tabItem(
        tabName = "uploaddata",
        fluidRow(
          column(
            4,
            box(
              title = "Step 1. Upload Data/Example Data",
              status = "primary",
              width =  12,
              solidHeader = TRUE,
              collapsible = FALSE,
              # loaddatatype
              div(
                style = "display: flex; align-items: center;",
                h5("Select data source:"),
                tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                       `data-toggle` = "tooltip", title = "Choose to upload your own data or use example data.")
              ),
              radioButtons(
                "loaddatatype",
                label = NULL,
                choices = list("A. Upload" = 1,"B. Load example data"=2),
                selected = 1,
                inline = TRUE
              ),
              tags$hr(style="border-color: grey80;"),
              conditionalPanel(
                condition = "input.loaddatatype==1",
                div(
                  style = "display: flex; align-items: center;",
                  h5("Select File Format:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Select the format of the file to upload.")
                ),
                radioButtons(
                  "fileType_Input",
                  label = NULL,
                  choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
                  selected = 1,
                  inline = TRUE
                ),
                div(
                  style = "display: flex; align-items: center;",
                  h5("Please import your data file:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Upload your data file in the selected format.")
                ),
                fileInput('file1', label = NULL, accept=c('text/csv','text/plain','.xlsx','.xls')),
                div(
                  style = "display: flex; align-items: center;",
                  h5("Is the first row names?"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Check if the first row contains column names.")
                ),
                checkboxInput('header', label = NULL, TRUE),
                div(
                  style = "display: flex; align-items: center;",
                  h5("Is the first column names?"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Check if the first column contains row names.")
                ),
                checkboxInput('firstcol', label = NULL, FALSE),
                conditionalPanel(condition = "input.fileType_Input==1",
                                 div(
                                   style = "display: flex; align-items: center;",
                                   h5("Which Sheet to read?"),
                                   tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                                          `data-toggle` = "tooltip", title = "Specify the sheet number to read from the Excel file.")
                                 ),
                                 numericInput("xlsxindex", label = NULL, value = 1)
                ),
                conditionalPanel(condition = "input.fileType_Input==2",
                                 div(
                                   style = "display: flex; align-items: center;",
                                   h5("Which Sheet to read?"),
                                   tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                                          `data-toggle` = "tooltip", title = "Specify the sheet number to read from the Excel file.")
                                 ),
                                 numericInput("xlsxindex", label = NULL, value = 1)
                ),
                conditionalPanel(condition = "input.fileType_Input==3",
                                 div(
                                   style = "display: flex; align-items: center;",
                                   h5("Data Separator (Comma/Semicolon/Tab/Space):"),
                                   tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                                          `data-toggle` = "tooltip", title = "Choose the delimiter used in your text file.")
                                 ),
                                 radioButtons('sep', label = NULL,
                                              c(Comma=',',
                                                Semicolon=';',
                                                Tab='\t',
                                                BlankSpace=' '),
                                              ',')
                )
              ),
              conditionalPanel(
                condition = "input.loaddatatype==2",
                div(
                  style = "display: flex; align-items: center;",
                  h5("Download example expression data:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Download a sample data file for demonstration.")
                ),
                downloadButton("loaddatadownload1", label = NULL,
                               style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              )
            )
          ),
          column(
            8,
            box(
              title = "Step 2. Display Uploaded Data/Example Data",
              status = "primary",
              width =  12,
              solidHeader = TRUE,
              collapsible = FALSE,
              div(style = "overflow-x: auto;overflow-y: auto;",dataTableOutput("rawdata"))
            )
          )
        )
      ),
      tabItem(
        tabName = "adjustparams",
        fluidRow(
          column(
            4,
            box(
              title = "Step 3. Adjust Parameters",
              status = "primary",
              width =  12,
              solidHeader = TRUE,
              collapsible = FALSE,
              conditionalPanel(
                condition = "input.loaddatatype==1",
                # 3.1 Color picker
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.1. Select three colors (low, middle, high):"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Select three colors for the correlation plot gradient.")
                ),
                colorPicker(
                  inputId = "colorx1",
                  label = NULL,
                  choices = list(
                    "npg"=pal_npg("nrc")(10),
                    "white"="white",
                    "aaas"=pal_aaas("default")(10),
                    "nejm"=pal_nejm("default")(8),
                    "pal"=scales::brewer_pal(palette = "Paired")(8)
                  ), 
                  selected=NULL,
                  plainColor = T,
                  multiple = TRUE
                ),
                # 3.2 Correlation method
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.2. Correlation method:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Choose the method for computing correlations.")
                ),
                selectInput("cormethodx1", label = NULL, choices = c("pearson","spearman","kendall")),
                # 3.3 Visualization type
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.3. The visualization type:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Choose the shape for visualizing the correlation matrix.")
                ),
                selectInput("methodx1",label = NULL,choices = c("square","circle")),
                # 3.4 Matrix type
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.4. The matrix type:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Choose full, upper, or lower triangle of the matrix.")
                ),
                selectInput("typex1",label = NULL,choices = c("full","upper","lower")),
                # 3.5 Order matrix
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.5. Whether correlation matrix will be ordered?"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "If TRUE, matrix is ordered using hierarchical clustering."),
                  div(style = "margin-left: 10px;", checkboxInput("hc.orderx1", label = NULL, value = TRUE))
                ),
                # 3.6 Limits of scale
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.6. Limits of the scale:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Set scale min and max, separated by ';', e.g. '-1;1'."),
                  div(style = "margin-left: 10px;", textInput("limitx1", label = NULL, value=""))
                ),
                # 3.7 Median value
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.7. Median value:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Set the middle value for the color gradient.")
                ),
                numericInput("midpointx1", label = NULL, value=NULL),
                # 3.8 Show correlation coefficients
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.8. Whether adding correlation coefficient on the plot?"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Show correlation coefficients as text on the plot."),
                  div(style = "margin-left: 10px;", checkboxInput('labx1', label = NULL, value = TRUE))
                )
              ),
              conditionalPanel(
                condition = "input.loaddatatype==2",
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.1. Select three colors (low, middle, high):"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Select three colors for the correlation plot gradient.")
                ),
                colorPicker(
                  inputId = "colorx2",
                  label = NULL,
                  choices = list(
                    "npg"=pal_npg("nrc")(10),
                    "white"="white",
                    "aaas"=pal_aaas("default")(10),
                    "nejm"=pal_nejm("default")(8),
                    "pal"=scales::brewer_pal(palette = "Paired")(8)
                  ), 
                  selected=c("white","#3B4992FF","#BB0021FF"),
                  plainColor = T, 
                  multiple = TRUE
                ),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.2. Correlation method:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Choose the method for computing correlations.")
                ),
                selectInput("cormethodx2", label = NULL, choices = c("pearson","spearman","kendall")),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.3. The visualization type:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Choose the shape for visualizing the correlation matrix.")
                ),
                selectInput("methodx2",label = NULL,choices = c("square","circle")),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.4. The matrix type:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Choose full, upper, or lower triangle of the matrix.")
                ),
                selectInput("typex2",label = NULL,choices = c("full","upper","lower")),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.5. Whether correlation matrix will be ordered?"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "If TRUE, matrix is ordered using hierarchical clustering."),
                  div(style = "margin-left: 10px;", checkboxInput("hc.orderx2", label = NULL, value = TRUE))
                ),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.6. Limits of the scale:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Set scale min and max, separated by ';', e.g. '-1;1'."),
                  div(style = "margin-left: 10px;", textInput("limitx2", label = NULL, value="0.9;1"))
                ),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.7. Middle value:"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Set the middle value for the color gradient.")
                ),
                numericInput("midpointx2", label = NULL, value=0.95),
                div(
                  style = "display: flex; align-items: center;",
                  h5("3.8. Whether adding correlation coefficient on the plot?"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                         `data-toggle` = "tooltip", title = "Show correlation coefficients as text on the plot."),
                  div(style = "margin-left: 10px;", checkboxInput('labx2', label = NULL, value = TRUE))
                )
              ),
              # 3.9 Figure Height
              div(
                style = "display: flex; align-items: center;",
                h5("3.9. Figure Height:"),
                tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                       `data-toggle` = "tooltip", title = "Set the height of the output plot (in pixels)."),
                div(style = "margin-left: 10px;", numericInput("heightx", label = NULL, value=600))
              ),
              # 3.10 Figure Width
              div(
                style = "display: flex; align-items: center;",
                h5("3.10. Figure Width:"),
                tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                       `data-toggle` = "tooltip", title = "Set the width of the output plot (in pixels)."),
                div(style = "margin-left: 10px;", numericInput("widthx", label = NULL, value=600))
              ),
              hr(),
              div(
                style = "display: flex; align-items: center;",
                actionButton("mcsbtn_Barplot","Start",icon("paper-plane"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 12px;",
                       `data-toggle` = "tooltip", title = "Click to generate the correlation plot with current settings.")
              )
            )
          ),
          column(
            8,
            box(
              title = "Step 4. Display and Download Results",
              status = "primary",
              width =  12,
              solidHeader = TRUE,
              collapsible = FALSE,
              div(
                style = "display: flex; align-items: center;",
                h5("Select output type:"),
                tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                       `data-toggle` = "tooltip", title = "Choose which result to display and download.")
              ),
              radioButtons(
                "resultsout",
                label = NULL,
                choices = list("4.1. Correlation Network Visualization" = 1),
                selected = 1,
                inline = TRUE
              ),
              tags$hr(style="border-color: grey90;"),
              conditionalPanel(
                condition = "input.resultsout==1",
                div(
                  style = "display: flex; align-items: center;",
                  downloadButton("DivergingBardefaultplotdl","Save Image"),
                  tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 12px;",
                         `data-toggle` = "tooltip", title = "Download the current correlation network visualization as an image.")
                ),
                div(style = "overflow-x: auto;overflow-y: auto;", plotOutput("ClusterCorNetplot",height = "550px"))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "references",
        column(
          7,
          style = "padding-right: 0px; padding-left: 0px;",
          box(
            title = "Step 5. LLMs Chat",
            status = "primary",
            width =  12,
            solidHeader = TRUE,
            collapsible = FALSE,
            div(class = "chat-box",htmlOutput("chat_output")),
            div(
              class = "input-container",
              div(style = "position: relative; width: 100%;",
                  div(
                    style = "display: flex; align-items: center;",
                    h5("Your message:"),
                    tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                           `data-toggle` = "tooltip", title = "Type your prompt for the LLM assistant.")
                  ),
                  textAreaInput("user_input", NULL, value = "", width = "100%", height = "100px",
                                placeholder = "Enter your message here...")
              )
            ),
            div(
              style = "display: flex; align-items: center;",
              h5("Choose a local LLM:"),
              tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                     `data-toggle` = "tooltip", title = "Select the local large language model to use for chat."),
              selectInput("llmmodel", label = NULL, choices = list_modelsx[[1]], width="40%"),
              actionButton("example_link", "Prompt example",
                           style = "color: white; background-color: grey;margin-left: 5px;margin-top: 0px;"),
              actionButton("send", "Send", class = "btn-primary",
                           style="color: white;font-weight: bold;margin-top:0px;margin-left:20px;width:100px;")
            )
          )
        ),
        column(
          5,
          style = "padding-left: 0px; padding-right: 0px;",
          box(
            title = "Step 6. Adjusted results by LLMs",
            status = "primary",
            width =  12,
            solidHeader = TRUE,
            collapsible = FALSE,
            fluidRow(
              column(2,
                     div(
                       style = "display: flex; align-items: center;",
                       downloadButton("DivergingBarllmplotdl", "Download"),
                       tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 12px;",
                              `data-toggle` = "tooltip", title = "Download the plot adjusted by LLM suggestions.")
                     )
              ),
              column(
                5,
                div(style = "display: flex; align-items: center;margin-left: 50px;margin-top: 0px;margin-bottom: 0px;",
                    h5("Height:"),
                    tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                           `data-toggle` = "tooltip", title = "Set the height of the LLM-adjusted plot (in pixels)."),
                    numericInput("heightx2", label = NULL, value = 600)
                )
              ),
              column(
                5,
                div(style = "display: flex; align-items: center;margin-left: 20px;margin-right: 40px;margin-top: 0px;margin-bottom: 0px;",
                    h5("Width:"),
                    tags$i(class = "fa fa-info-circle", style = "color:#6495ED; margin-left: 6px;",
                           `data-toggle` = "tooltip", title = "Set the width of the LLM-adjusted plot (in pixels)."),
                    numericInput("widthx2", label = NULL, value = 600)
                )
              )
            ),
            div(style = "overflow-x: auto;overflow-y: auto;height: 600px;", uiOutput("code_output"))
          )
        )
      )
    )
  )
)
###########
######server.R
###########
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  ##########################################
  examplepeakdatas<-reactive({
    library(writexl)
    dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F,row.names = 1)
    dataread<-log2(dataread)
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = T)
    }
  )
  peaksdataout<-reactive({
    if(input$loaddatatype==1){
      files <- input$file1
      if (is.null(files)){
        dataread<-data.frame(Description="No data loaded. Please upload your dataset or use the example data.")
      }else{
        if (input$fileType_Input == "1"){
          dataread<-read.xlsx(files$datapath,rowNames=input$firstcol,
                              colNames = input$header,sheet = input$xlsxindex)
        }
        else if(input$fileType_Input == "2"){
          if(sum(input$firstcol)==1){
            rownametf<-1
          }else{
            rownametf<-NULL
          }
          dataread<-read.xls(files$datapath,sheet = input$xlsxindex,header=input$header,
                             row.names = rownametf, sep=input$sep,stringsAsFactors = F)
        }
        else{
          if(sum(input$firstcol)==1){
            rownametf<-1
          }else{
            rownametf<-NULL
          }
          dataread<-read.csv(files$datapath,header=input$header,
                             row.names = rownametf, sep=input$sep,stringsAsFactors = F)
        }
        #colnames(dataread)<-c("IDs","Counts","P.values")
      }
    }else{
      dataread<-examplepeakdatas()
    }
    dataread
  })
  output$rawdata<-renderDataTable({
    datatable(peaksdataout(), options = list(pageLength = 10))
  })
  ##
  observeEvent(
    input$mcsbtn_Barplot,{
      heightx<-reactive({
        isolate(input$heightx)
      })
      widthx<-reactive({
        isolate(input$widthx)
      })
      output$ClusterCorNetplot<-renderPlot({
        #if(input$loaddatatype==1){
        #  grnames1<-strsplit(input$grnames,";")[[1]]
        #  grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
        #  grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
        #  grnames<-rep(grnames1,times=grnum2)
        #}else{
        #  grnames1<-strsplit(input$examgrnames,";")[[1]]
        #  grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        #  grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
        #  grnames<-rep(grnames1,times=grnum2)
        #}
        #DivergingBartextvalue1<<-grnames1
        #DivergingBarclassname<<-grnames
        #DivergingBar_leibie_num<<-grnum1
        #DivergingBar_fenzu_num<<-grnum2
        corrdata<<-peaksdataout()
        if(ncol(corrdata)==1){
          ggplot() + 
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') + 
            theme_void()
        }else{
          if(input$loaddatatype==1){
            colorx<<-isolate(input$colorx1)
            methodx<<-isolate(input$methodx1)
            typex<<-isolate(input$typex1)
            hc.orderx<<-isolate(input$hc.orderx1)
            limitx<<-as.numeric(strsplit(isolate(input$limitx1),";")[[1]])
            midpointx<<-isolate(input$midpointx1)
            cormethodx<<-isolate(input$cormethodx1)
            labelsx<<-isolate(input$labx1)
          }else{
            colorx<<-isolate(input$colorx2)
            methodx<<-isolate(input$methodx2)
            typex<<-isolate(input$typex2)
            hc.orderx<<-isolate(input$hc.orderx2)
            limitx<<-as.numeric(strsplit(isolate(input$limitx2),";")[[1]])
            midpointx<<-isolate(input$midpointx2)
            cormethodx<<-isolate(input$cormethodx2)
            labelsx<<-isolate(input$labx2)
          }
          library(ggcorrplot)
          library(psych)
          mcorr<-corr.test(corrdata, use = "pairwise",method=cormethodx,adjust="BH", alpha=.05,ci=TRUE)
          ggcorrplot(mcorr$r,method = methodx,type = typex,hc.order = hc.orderx,tl.cex = 2,tl.col="black",lab = labelsx)+
            ggplot2::scale_fill_gradient2(low = colorx[1],mid = colorx[2],high = colorx[3],midpoint=midpointx,limit = limitx)+
            xlab("")+ylab("")+
            theme_bw()
        }
      },height=heightx,width = widthx)
      ClusterCorNetplotout<-reactive({
        #if(input$loaddatatype==1){
        #  grnames1<-strsplit(input$grnames,";")[[1]]
        #  grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
        #  grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
        #  grnames<-rep(grnames1,times=grnum2)
        #}else{
        #  grnames1<-strsplit(input$examgrnames,";")[[1]]
        #  grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
        #  grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
        #  grnames<-rep(grnames1,times=grnum2)
        #}
        #DivergingBartextvalue1<<-grnames1
        #DivergingBarclassname<<-grnames
        #DivergingBar_leibie_num<<-grnum1
        #DivergingBar_fenzu_num<<-grnum2
        corrdata<<-peaksdataout()
        if(ncol(corrdata)==1){
          ggplot() + 
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') + 
            theme_void()
        }else{
          if(input$loaddatatype==1){
            colorx<<-isolate(input$colorx1)
            methodx<<-isolate(input$methodx1)
            typex<<-isolate(input$typex1)
            hc.orderx<<-isolate(input$hc.orderx1)
            limitx<<-as.numeric(strsplit(isolate(input$limitx1),";")[[1]])
            midpointx<<-isolate(input$midpointx1)
            cormethodx<<-isolate(input$cormethodx1)
            labelsx<<-isolate(input$labx1)
          }else{
            colorx<<-isolate(input$colorx2)
            methodx<<-isolate(input$methodx2)
            typex<<-isolate(input$typex2)
            hc.orderx<<-isolate(input$hc.orderx2)
            limitx<<-as.numeric(strsplit(isolate(input$limitx2),";")[[1]])
            midpointx<<-isolate(input$midpointx2)
            cormethodx<<-isolate(input$cormethodx2)
            labelsx<<-isolate(input$labx2)
          }
          library(ggcorrplot)
          library(psych)
          mcorr<-corr.test(corrdata, use = "pairwise",method=cormethodx,adjust="BH", alpha=.05,ci=TRUE)
          ggcorrplot(mcorr$r,method = methodx,type = typex,hc.order = hc.orderx,tl.cex = 2,tl.col="black",lab = labelsx)+
            ggplot2::scale_fill_gradient2(low = colorx[1],mid = colorx[2],high = colorx[3],midpoint=midpointx,limit = limitx)+
            xlab("")+ylab("")+
            theme_bw()
        }
      })
      output$DivergingBardefaultplotdl<-downloadHandler(
        filename = function(){paste("Default_Corrplot",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = widthx()/96,height = heightx()/96)
          print(ClusterCorNetplotout())
          dev.off()
        }
      )
    }
  )
  ##
  chat_history <- reactiveVal(list())
  code_result <- reactiveVal(NULL)
  typetimes<-1
  observeEvent(input$example_clicked, {
    #I want to process DivergingBar using an example data in R and and visualize individual scores using ggplot2, please put the whole R codes together
    #updateTextInput(session, "user_input", value = "The input data is named DivergingBardata, in which rows are proteins and columns are samples, the first four columns are Group A and the last four columns are Group B. I want to process DivergingBar in R to visualize individual scores and put all codes together.")
    updateTextInput(session, "user_input", value = "Please change the color for low values to 'blue', middle values to 'white', and high values to 'red'. Refer to inner codes.")# and point shapes are circle
  })
  heightx2<-reactive({
    input$heightx2
  })
  widthx2<-reactive({
    input$widthx2
  })
  observeEvent(input$send, {
    #if(input$loaddatatype==1){
    #  grnames1<-strsplit(input$grnames,";")[[1]]
    #  grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
    #  grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    #  grnames<-rep(grnames1,times=grnum2)
    #}else{
    #  grnames1<-strsplit(input$examgrnames,";")[[1]]
    #  grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
    #  grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
    #  grnames<-rep(grnames1,times=grnum2)
    #}
    #DivergingBartextvalue1<<-grnames1
    #DivergingBarclassname<<-grnames
    #DivergingBar_leibie_num<<-grnum1
    #DivergingBar_fenzu_num<<-grnum2
    corrdata<<-peaksdataout()
    user_inputx<<-input$user_input
    user_inputx1<-grep("Refer to inner codes(\\.)?|=>",user_inputx,ignore.case = T)
    #req(input$user_input)
    if(length(user_inputx1)>0){
      typetext1<-'
library(ggcorrplot)
library(psych)
mcorr<-corr.test(corrdata, use = "pairwise",method="pearson",adjust="BH", alpha=.05,ci=TRUE)
#hc.order: logical value. If TRUE, correlation matrix will be hc.ordered using hclust function.
#lab: logical value. If TRUE, add correlation coefficient on the plot.
#p.mat: matrix of p-value. If NULL, arguments sig.level, insig, pch, pch.col, pch.cex is invalid.
#low, high: colours for low and high ends of the gradient.
#mid: colour for mid point.
#midpoint: the midpoint (in data value) of the diverging scale. Defaults to 0.
#limit: a numeric vector of length two providing limits of the scale.
ggcorrplot(mcorr$r,method = "square",type = "full",hc.order = TRUE,tl.cex = 2,tl.col="black",lab = TRUE)+
  ggplot2::scale_fill_gradient2(low = "blue",mid = "white",high = "red",midpoint=0.95,limit = c(0.9,1))+
  xlab("")+ylab("")+
  theme_bw()
      '
      user_message <- paste0(input$user_input," Plaese just copy below codes to give me the R codes and no interpretation:","\n",typetext1)
    }else{
      user_message <- input$user_input
    }
    #user_message <- input$user_input
    typetimes<<-typetimes+1
    chat_history(c(chat_history(), list(list(role = "user", content = user_message))))
    messagesx <<- chat_history()
    llmmodelx<<-input$llmmodel
    response_message <- chat(llmmodelx, messagesx, output = "text",#llama3 llama3.1:405b llama3:70b gemma2:27b
                             keep_alive = "30m",temperature = 0.2, num_predict = 2048,
                             stream=TRUE)
    chat_history(c(chat_history(), list(list(role = "assistant", content = response_message))))
    # Convert Markdown to HTML
    convert_to_html <- function(text){
      markdownToHTML(text = text, fragment.only = TRUE)
    }
    messagesx1 <<- chat_history()
    new_chat <- lapply(chat_history(),function(x){
      if (x$role == "user"){
        paste0(
          '<div class="user"><div class="message"><strong>User:</strong> ', 
          convert_to_html(gsub(" Plaese just copy below codes to give me the R codes and no interpretation:\n\n.*","",x$content)), 
          '</div></div>'
        )
      }else{
        paste0('<div class="assistant"><div class="message"><strong>Assistant:</strong> ', convert_to_html(x$content), '</div></div>')
      }
    })
    
    updateTextInput(session, "user_input", value = "")
    output$chat_output <- renderUI({
      HTML(paste(new_chat, collapse = "\n"))
    })
    # Check for R code and execute
    Bardata<<-peaksdataout()
    response_messagex <<- response_message
    if (grepl("```R|```r", response_messagex) | grepl("```", response_messagex)) {
      #r_code <- sub(".*```R|.*\n\n```", "", response_messagex[length(response_messagex)])
      #r_code <- sub("```\n\n.*|```.*", "", r_code)
      matches1 <- regexpr("```(R|r)(.*?)```", response_messagex)
      extracted <- regmatches(response_messagex, matches1)
      r_code <- gsub("```","",gsub("^```(R|r)", "", extracted))
      r_code <- trimws(r_code)
      result <- tryCatch({
        eval(parse(text = r_code))
      }, error = function(e) {
        paste("Error in code:", e$message)
      })
      code_result(result)
    } else {
      code_result(NULL)
    }
    
    output$code_output <- renderUI({
      code_resultx<<-code_result()
      #if (!is.null(code_result())) {
      #  if (is.ggplot(code_result())) {
      #    plotOutput("plot_result")
      #  } else {
      #    HTML(paste("<pre>", code_result(), "</pre>"))
      #  }
      #} else {
      #  HTML("")
      #}
      if(is.ggplot(code_result())){
        #div(style = "overflow-x: auto;overflow-y: auto;", plotOutput("plot_result",width="100%",height = heightx2()))
        plotOutput("plot_result",width="100%",height = heightx2())
      }else if((is.data.frame(code_result())|is.matrix(code_result()))){
        #div(style = "overflow-x: auto;overflow-y: auto;", dataTableOutput("data_result"))
        dataTableOutput("data_result")
      }else{
        #div(style = "overflow-x: auto;overflow-y: auto;", HTML(paste("<pre>", code_result(), "</pre>")))
        HTML(paste("<pre>", code_result(), "</pre>"))
      }
    })
    
    output$plot_result <- renderPlot({
      if (!is.null(code_result()) && is.ggplot(code_result())) {
        code_result()
      }
    })
    plot_resultout<-reactive({
      if (!is.null(code_result()) && is.ggplot(code_result())) {
        code_result()
      }
    })
    output$data_result<-renderDataTable({
      if (!is.null(code_result()) && (is.data.frame(code_result())|is.matrix(code_result()))) {
        code_result()
      }
    })
    data_resultout<-reactive({
      if (!is.null(code_result()) && (is.data.frame(code_result())|is.matrix(code_result()))) {
        code_result()
      }
    })
    output$DivergingBarllmplotdl<-downloadHandler(
      filename = function(){
        if(is.ggplot(code_result())){
          paste("Adjusted.by.LLM_Corr.plot",usertimenum,".pdf",sep="")
        }else{
          paste("Adjusted.by.LLM_Corr.table",usertimenum,".csv",sep="")
        }
      },
      content = function(file){
        if(is.ggplot(code_result())){
          pdf(file, width = widthx2()/96,height = heightx2()/96)
          print(plot_resultout())
          dev.off()
        }else{
          write.csv(data_resultout(),file)
        }
      }
    )
    
  })
  
}
shinyApp(ui = ui, server = server)
