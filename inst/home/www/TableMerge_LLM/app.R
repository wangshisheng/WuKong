library(shiny)
library(httr)
library(ollamar)
library(markdown)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(openxlsx)
list_modelsx <- ollamar::list_models()
# Define UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Two tables merging with the assistance of LLMs"),
  tagList(
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
      tags$script('
                            var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    '),
      tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),#F5F5DC
      tags$style(type="text/css", "
                   #tooltip2 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
      tags$style(type="text/css", "
                   #tooltip3 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
      tags$style(type="text/css", "
                   #tooltip4 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
      tags$style(type="text/css", "
                   #tooltip5 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
      tags$style(type="text/css", "
                   #tooltip6 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
    )
  ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(h2(strong("Thinking......")),img(src="rmd_loader.gif"),id="loadmessage")),
  fluidRow(
    column(2,
           h4("1. Upload expression data:"),
           radioButtons(
             "loaddatatype",
             label = NULL,
             choices = list("I. Upload" = 1,"II. Load example data"=2),
             selected = 1,
             inline = TRUE
           ),
           tags$hr(style="border-color: grey80;"),
           conditionalPanel(
             condition = "input.loaddatatype==1",
             fileInput('file1', h5('1.1. Import table 1：'),accept=c('.csv','.txt')),
             #checkboxInput('header', 'First row as column names ?', TRUE),
             #checkboxInput('firstcol', 'First column as row names ?', TRUE),
             tags$hr(style="border-color: grey80;"),
             fileInput('file2', h5('1.2. Import table 2：'),accept=c('.csv','.txt'))#,
             #h4("2. Samples information:"),
             #textInput("grnums",h5("2.1. Group and replicate number:"),value = ""),
             #bsTooltip("grnums",'Type in the group number and replicate number here. Please note, the group number and replicate number are linked with ";", and the replicate number of each group is linked with "-". For example, if you have two groups, each group has three replicates, then you should type in "2;3-3" here. Similarly, if you have 3 groups with 5 replicates in every groups, you should type in "3;5-5-5".',
             #          placement = "right",options = list(container = "body")),
             #textInput("grnames",h5("2.2. Group names:"),value = ""),
             #bsTooltip("grnames",'Type in the group names of your samples. Please note, the group names are linked with ";". For example, there are two groups, you can type in "Control;Experiment".',
             #          placement = "right",options = list(container = "body"))
           ),
           conditionalPanel(
             condition = "input.loaddatatype==2",
             downloadButton("loaddatadownload1","Download example table 1",style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
             tags$hr(style="border-color: grey80;"),
             downloadButton("loaddatadownload2","Download example table 2",style="color: #fff; background-color: #6495ED; border-color: #6495ED")#,
             #h4("2. Samples information:"),
             #textInput("examgrnums",h5("2.1. Group and replicate number:"),value = "2;4-4"),
             #textInput("examgrnames",h5("2.2. Group names:"),value = "A;B")
           ),
           tags$hr(style="border-color: grey80;"),
           actionButton("mcsbtn_datainputview1","Review table 1",icon("file-alt"),
                        style="color: black; background-color: #E6E6FA; border-color: #E6E6FA"),
           tags$hr(style="border-color: grey80;"),
           actionButton("mcsbtn_datainputview2","Review table 2",icon("file-alt"),
                        style="color: black; background-color: #E6E6FA; border-color: #E6E6FA"),
           tags$hr(style="border-color: grey80;"),
           selectInput("llmmodel", "Choose a Model:", choices = list_modelsx[[1]]),
    ),
    column(5,
           div(
             class = "chat-container",
             h4("2. Conversation:"),
             htmlOutput("chat_output")
           )
    ),
    column(5,
           div(
             class = "result-container",
             h4("3. Results:"),
             #h5("4.1. Default plot:"),
             #downloadButton("mergedefaultplotdl","Download"),
             #div(style = "overflow-x: auto;", plotOutput("mergeplot")),
             h5("3.1. Adjusted results by LLMs:"),
             downloadButton("mergellmplotdl","Download"),
             div(style = "overflow-x: auto;", uiOutput("code_output"))
           )
    )
  ),
  div(
    class = "input-container",
    div(style = "position: relative; width: 100%;",
        textAreaInput("user_input", NULL, value = "", width = "97%", height = "100px",
                      placeholder = "Type your message here..."),
        tags$a(href = "#", id = "example_link", "Prompt example",
               style = "position: absolute; top: 8px; right: -80px;font-size: 15px;")
    ),
    actionButton("send", "Send", class = "btn-primary")#,
    #actionButton("clear", "Clear Chat", class = "btn-danger")
  ),
  tags$head(tags$style(HTML("
    body {
      font-family: Arial, sans-serif;
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    .chat-container {
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 15px;
      background-color: #f9f9f9;
      max-height: calc(100vh - 250px);
      overflow-y: auto;
      margin-bottom: 20px;
    }
    .result-container {
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 15px;
      background-color: #fff;
      max-height: calc(100vh - 250px);
      overflow-y: auto;
      margin-bottom: 20px;
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
    .input-container {
      position: fixed;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: #f9f9f9;
      border-top: 1px solid #ddd;
      padding: 10px;
      box-shadow: 0 -2px 5px rgba(0, 0, 0, 0.1);
      display: flex;
      align-items: flex-end;
      justify-content: space-between;
    }
    .btn-primary, .btn-danger {
      margin-left: -10px;
      margin-right: 35px;
      margin-bottom: 30px;
    }
    .btn-primary {
      background-color: #007bff;
      color: white;
      border: none;
      padding: 10px 20px;
      border-radius: 5px;
    }
    .btn-primary:hover {
      background-color: #0056b3;
    }
    .btn-danger {
      background-color: #dc3545;
      color: white;
      border: none;
      padding: 10px 20px;
      border-radius: 5px;
    }
    .btn-danger:hover {
      background-color: #c82333;
    }
  "))),
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#example_link').on('click', function(e) {
        e.preventDefault();
        Shiny.setInputValue('example_clicked', true, {priority: 'event'});
      });
    });
  "))
)

# Define server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  examplepeakdatas1<-reactive({
    library(writexl)
    dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F,row.names = 1)
    dataread<-dataread[1:200,]
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData1_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas1(),file,row.names = T)
    }
  )
  examplepeakdatas2<-reactive({
    library(writexl)
    dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F,row.names = 1)
    dataread<-dataread[c(50:150,208:230),]
    dataread
  })
  output$loaddatadownload2<-downloadHandler(
    filename = function(){paste("Example_ExpressionData2_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas2(),file,row.names = T)
    }
  )
  peaksdataout1<-reactive({
    files1 <- input$file1
    if (is.null(files1)){
      dataread<-data.frame(Description="No data. Please upload the expression data, or load the example data to check first.")
    }else{
      #if(sum(input$firstcol)==1){
      rownametf<-1
      #}else{
      #  rownametf<-NULL
      #}
      if(length(grep("\\.csv$",files1$datapath))>0){
        sepchar<-","
      }else{
        sepchar<-"\t"
      }
      dataread<-read.csv(files1$datapath,header=T,check.names = F,sep=sepchar,
                         row.names = rownametf,stringsAsFactors = F)
    }
    dataread
  })
  peaksdataout2<-reactive({
    files2 <- input$file2
    if (is.null(files2)){
      dataread<-data.frame(Description="No data. Please upload the expression data, or load the example data to check first.")
    }else{
      #if(sum(input$firstcol)==1){
      rownametf<-1
      #}else{
      #  rownametf<-NULL
      #}
      if(length(grep("\\.csv$",files2$datapath))>0){
        sepchar<-","
      }else{
        sepchar<-"\t"
      }
      dataread<-read.csv(files2$datapath,header=T,check.names = F,sep=sepchar,
                         row.names = rownametf,stringsAsFactors = F)
    }
    dataread
  })
  
  observeEvent(input$mcsbtn_datainputview1,{
    showModal(modalDialog(
      title = "Uploaded table 1",
      output$peaksdata<-renderDataTable({
        if(input$loaddatatype==1){
          peaksdatax1<<-peaksdataout1()
          datatable(peaksdatax1, options = list(pageLength = 10,scrollX = TRUE))
        }else{
          datatable(examplepeakdatas1(), options = list(pageLength = 10,scrollX = TRUE))
        }
      }),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_datainputview2, {
    showModal(modalDialog(
      title = "Uploaded table 2",
      output$peaksdata<-renderDataTable({
        if(input$loaddatatype==1){
          peaksdatax2<<-peaksdataout2()
          datatable(peaksdatax2, options = list(pageLength = 10,scrollX = TRUE))
        }else{
          datatable(examplepeakdatas2(), options = list(pageLength = 10,scrollX = TRUE))
        }
      }),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  ######
  mergeplotdataout1<-reactive({
    if(input$loaddatatype==1){
      peaksdatax<-peaksdataout1()
    }else{
      peaksdatax<-examplepeakdatas1()
    }
    peaksdatax
  })
  mergeplotdataout2<-reactive({
    if(input$loaddatatype==1){
      peaksdatax<-peaksdataout2()
    }else{
      peaksdatax<-examplepeakdatas2()
    }
    peaksdatax
  })
  ######
  chat_history <- reactiveVal(list())
  code_result <- reactiveVal(NULL)
  typetimes<-1
  
  observeEvent(input$example_clicked, {
    #I want to process merge using an example data in R and and visualize individual scores using ggplot2, please put the whole R codes together
    #updateTextInput(session, "user_input", value = "The input data is named mergedata, in which rows are proteins and columns are samples, the first four columns are Group A and the last four columns are Group B. I want to process merge in R to visualize individual scores and put all codes together.")
    updateTextInput(session, "user_input", value = "Please merge the two uploaded tables by rownames. Refer to inner codes.")# and point shapes are circle
  })
  
  observeEvent(input$send, {
    mergedata1<<-mergeplotdataout1()
    mergedata2<<-mergeplotdataout2()
    user_inputx<<-input$user_input
    user_inputx1<-grep("Refer to inner codes(\\.)?|=>",user_inputx,ignore.case = T)
    #req(input$user_input)
    if(length(user_inputx1)>0){
      typetext1<-'
        mergedata1$ID<-rownames(mergedata1)
        mergedata2$ID<-rownames(mergedata2)
        mergedata<-base::merge(mergedata1,mergedata2,by.x = "ID", by.y = "ID", all = FALSE, 
                               all.x = FALSE, all.y = FALSE,sort=F)
        mergedata
      '
      user_message <- paste0(input$user_input," Plaese just copy below R codes:","\n",typetext1)
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
          convert_to_html(gsub(" Plaese just copy below R codes:\n\n.*","",x$content)), 
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
    mergedata1<<-mergeplotdataout1()
    mergedata2<<-mergeplotdataout2()
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
    output$mergellmplotdl<-downloadHandler(
      filename = function(){
        if(is.ggplot(code_result())){
          paste("Adjusted.by.LLM_merge.plot",usertimenum,".pdf",sep="")
        }else{
          paste("Adjusted.by.LLM_merge.table",usertimenum,".csv",sep="")
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
