library(shiny)
library(httr)
library(ollamar)
library(markdown)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(rvest)
library(openxlsx)
list_modelsx <- ollamar::list_models()
# Define UI
ui <- fluidPage(
  style = "min-width:1400px;",
  shinyjs::useShinyjs(),
  # Page title
  titlePanel(div(
    style = "color: #2c3e50; font-weight: bold; text-align: center; margin-bottom: 20px;",
    "Webpage Content Analysis with LLMs"
  ),windowTitle="Webpage Content Analysis"),
  # Custom CSS for refined layout and styling
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
    tags$script(type="text/javascript", src = "busy.js"),
    tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f7f9fc;
        margin: 0;
        padding: 0;
      }
      .section-container {
        border: 1px solid #dfe6e9;
        border-radius: 8px;
        background-color: #ffffff;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
      .input-container {
      position: fixed;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: #f9f9f9;
      border-top: 1px solid #ddd;
      padding: 10px;
      box-shadow: 0 -2px 5px rgba(0, 0, 0, 0.1);
    }
      .section-header {
        font-size: 20px;
        font-weight: bold;
        color: #34495e;
        margin-bottom: 15px;
        padding: 10px;
        border-bottom: 2px solid #dfe6e9;
      }
      .btn-primary {
        background-color: #3498db;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 16px;
        width: 120px;
      }
      .btn-primary:hover {
        background-color: #2980b9;
      }
      .btn-danger {
        background-color: #e74c3c;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 16px;
        width: 120px;
      }
      .btn-danger:hover {
        background-color: #c0392b;
      }
      .form-control {
        width: 100%;
        padding: 10px;
        border: 1px solid #ced4da;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .chat-container, .result-container {
        max-height: 400px;
        overflow-y: auto;
        padding: 15px;
        border: 1px solid #dfe6e9;
        border-radius: 8px;
        background-color: #f9f9f9;
        margin-bottom: 20px;
      }
      .chat-message {
        margin-bottom: 15px;
      }
      .chat-message.user {
        text-align: left;
      }
      .chat-message.assistant {
        text-align: left;
      }
      .chat-message .message {
        display: inline-block;
        padding: 10px 15px;
        border-radius: 8px;
        font-size: 14px;
        line-height: 1.5;
      }
      .chat-message.user .message {
        background-color: #d1ecf1;
        color: #0c5460;
      }
      .chat-message.assistant .message {
        background-color: #d4edda;
        color: #155724;
      }
      .user, .assistant {
      margin-bottom: 15px;
    }
    .user {
      text-align: left;
    }
    .assistant {
      text-align: left;
    }
    .message {
      max-width: 70%;
      padding: 10px;
      border-radius: 10px;
      font-size: 16px;
      line-height: 1.5;
    }
    .user .message {
      background-color: #e0f7fa;
      margin-left: auto;
    }
    .assistant .message {
      background-color: #f1f8e9;
    }
    "))
  ),
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#example_link').on('click', function(e) {
        e.preventDefault();
        Shiny.setInputValue('example_clicked', true, {priority: 'event'});
      });
    });
  ")),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(h2(strong("Thinking......")),img(src="rmd_loader.gif"),id="loadmessage")),
  # Layout
  fluidRow(
    column(4,
           div(
             class = "section-container",
             div(class = "section-header", "1. Upload Data"),
             radioButtons(
               "loaddatatype",
               label = NULL,
               choices = list("I. Paste Webpage Link" = 1, "II. Example Webpage Link" = 2),
               selected = 1,
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.loaddatatype == 1",
               textInput("weblink", "Paste the webpage link here:", value = "", width = "100%")
             ),
             conditionalPanel(
               condition = "input.loaddatatype == 2",
               textInput("examweblink", "Example Webpage Link:", value = "https://www.geeksforgeeks.org/r-programming-language-introduction", width = "100%")
             )
           )
    ),
    column(8,
           div(
             class = "section-container",
             div(class = "section-header", "2. Conversation"),
             div(class = "chat-container", htmlOutput("chat_output"))
           )
    )
  ),
  fluidRow(
    column(12,
           div(
             class = "input-container",
             div(class = "section-header", "3. Interaction"),
             #selectInput("llmmodel", "Choose a Model:", choices = list_modelsx[[1]]),
             div(
               style = "display: flex; align-items: center;",
               tags$label("Choose a Model:", style = "margin-right: 10px;margin-top: -10px;"),
               selectInput("llmmodel", NULL, choices = list_modelsx[[1]], width = "280px"),
               actionButton("example_link", "Prompt example",
                            style = "color: white; background-color: grey;margin-left: 20px;margin-top: -15px;")
             ),
             div(
               style = "display: flex; align-items: center; justify-content: space-between;",
               # Text area on the left
               textAreaInput("user_input", NULL, value = "", width = "92%", height = "120px",
                             placeholder = "Type your message here..."),
               # Buttons on the right
               div(
                 style = "display: flex; flex-direction: column; align-items: flex-start;",
                 actionButton("send", "Send", class = "btn-primary", style = "margin-bottom: 10px;"),
                 #actionButton("clear", "Clear", class = "btn-danger")
               )
             )
           )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  examplepeakdatas<-reactive({
    library(writexl)
    dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F)
    colnames(dataread)<-c("IDs","Counts","P.values")
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = T)
    }
  )
  peaksdataout<-reactive({
    files1 <- input$file1
    if (is.null(files1)){
      dataread<-data.frame(Description="No data. Please upload the expression data, or load the example data to check first.")
    }else{
      #if(sum(input$firstcol)==1){
      #rownametf<-1
      #}else{
      #  rownametf<-NULL
      #}
      if(length(grep("\\.csv$",files1$datapath))>0){
        sepchar<-","
      }else{
        sepchar<-"\t"
      }
      dataread<-read.csv(files1$datapath,header=T,check.names = F,sep=sepchar,
                         stringsAsFactors = F)#row.names = rownametf,
    }
    dataread
  })
  
  observeEvent(input$mcsbtn_datainputview, {
    showModal(modalDialog(
      title = "Uploaded data",
      output$peaksdata<-renderDataTable({
        if(input$loaddatatype==1){
          peaksdatax<<-peaksdataout()
          datatable(peaksdatax, options = list(pageLength = 10,scrollX = TRUE))
        }else{
          datatable(examplepeakdatas(), options = list(pageLength = 10,scrollX = TRUE))
        }
      }),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  ######
  chat_history <- reactiveVal(list())
  code_result <- reactiveVal(NULL)
  typetimes<-1
  ######
  webpageout<-reactive({
    if(input$loaddatatype==1){
      url <- input$weblink
    }else{
      url <- input$examweblink
    }
    urlx<<-url
    response <- GET(url)
    if (status_code(response) == 200) {
      user_inputx1<-"Webpage fetched successfully! The extracted contents are shown as below:\n\n"
      webpage <- read_html(content(response, "text"))
      text_content <- webpage %>%
        html_elements("p") %>%
        html_text()
      user_inputx<<-paste0(input$user_input,"\n\n",user_inputx1,paste0(text_content,collapse = "\n"))
      user_message <- user_inputx
    } else {
      user_message <-paste("Failed to fetch webpage. Status code:", status_code(response))
    }
    user_message
  })
  observeEvent(input$example_clicked, {
    #I want to process DivergingBar using an example data in R and and visualize individual scores using ggplot2, please put the whole R codes together
    #updateTextInput(session, "user_input", value = "The input data is named DivergingBardata, in which rows are proteins and columns are samples, the first four columns are Group A and the last four columns are Group B. I want to process DivergingBar in R to visualize individual scores and put all codes together.")
    updateTextInput(session, "user_input", value = "Please extract all contents in this webpage and summary the keypoints of the contents.")# Refer to inner codes. and point shapes are circle
  })
  
  observeEvent(input$send, {
    if(typetimes==1){
      user_message<<-webpageout()
      typetimes<<-typetimes+1
    }else{
      user_message<-input$user_input
    }
    #user_message <- input$user_input
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
        plotOutput("plot_result")
      }else if((is.data.frame(code_result())|is.matrix(code_result()))){
        dataTableOutput("data_result")
      }else{
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
          paste("Adjusted.by.LLM_Bar.plot",usertimenum,".pdf",sep="")
        }else{
          paste("Adjusted.by.LLM_Bar.table",usertimenum,".csv",sep="")
        }
      },
      content = function(file){
        if(is.ggplot(code_result())){
          pdf(file, width = 10,height = 10)
          print(plot_resultout())
          dev.off()
        }else{
          write.csv(data_resultout(),file)
        }
      }
    )
    
  })
  
  observeEvent(input$clear, {
    chat_history(list())
    code_result(NULL)
    output$chat_output <- renderUI({
      HTML("")
    })
    output$code_output <- renderUI({
      HTML("")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
