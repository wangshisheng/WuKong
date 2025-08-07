# Load required libraries
library(shiny)
library(shinydashboard)
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
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Exploring gene/protein functions based on GO database with the assistance of LLMs",
    titleWidth = 860
  ),
  dashboardSidebar(
    width = 300,
    useShinyjs(),
    sidebarMenu(
      radioButtons(
        "loaddatatype", "1. Upload Data:",
        choices = list("1.1. Paste gene/protein names/UniProt IDs" = 1, "1.2. Example gene/protein names/UniProt IDs" = 2),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.loaddatatype==1",
        textAreaInput("IDzhantie", h5("Pasted gene/protein names/UniProt IDs:"), value = "", height = "250px")
      ),
      conditionalPanel(
        condition = "input.loaddatatype==2",
        textAreaInput("examIDzhantie", h5("Pasted gene/protein names/UniProt IDs:"), value = "TP53\nCTNNB1\nBCL6", height = "250px")
      ),
      selectInput("llmmodel", "1.3. Choose a Model:", choices = list_modelsx[[1]])
      # Removed Prompt example link and <br>s from sidebar
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="busystyle.css"),
      tags$link(rel="stylesheet", type="text/css", href="mainstyle.css"),
      tags$script(type="text/javascript", src = "busy.js"),
      tags$style(HTML("
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
        .chat-container {
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 15px;
          background-color: #f9f9f9;
          max-height: 60vh;
          height:60vh;
          overflow-y: auto;
          margin-bottom: 20px;
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
        }
        .assistant .message {
          background-color: #f1f8e9;
        }
        .message {
          max-width: 100%;
          padding: 10px;
          border-radius: 10px;
          font-size: 16px;
          line-height: 1.5;
        }
        .input-container {
          position: relative;
          width: 100%;
          background-color: #f9f9f9;
          border-top: 1px solid #ddd;
          padding: 10px 0;
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
        .prompt-send-row {
          display: flex;
          justify-content: flex-end;
          align-items: center;
          gap: 10px;
          margin-top: 10px;
        }
        .prompt-link {
          font-size: 15px;
          color: #007bff;
          cursor: pointer;
          text-decoration: none;
        }
      "))
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      tags$div(h2(strong("Thinking......")), img(src="rmd_loader.gif"), id="loadmessage")
    ),
    fluidRow(
      box(
        width = 12,
        title = "2. Conversation",
        status = "primary",
        solidHeader = TRUE,
        div(class = "chat-container", htmlOutput("chat_output"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = NULL,
        solidHeader = FALSE,
        div(class = "input-container",
            textAreaInput("user_input", NULL, value = "", width = "100%", height = "80px", placeholder = "Type your message here..."),
            # Add a flex row for prompt example and send button
            div(
              class = "prompt-send-row",
              tags$a(href = "#", id = "example_link", "Prompt example", class = "prompt-link"),
              actionButton("send", "Send", class = "btn-primary")
            )
        )
      )
    ),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('#example_link').on('click', function(e) {
          e.preventDefault();
          Shiny.setInputValue('example_clicked', true, {priority: 'event'});
        });
      });
    "))
  )
)


# Define server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  chat_history <- reactiveVal(list())
  code_result <- reactiveVal(NULL)
  typetimes<-1
  
  webpageout<-reactive({
    if(input$loaddatatype==1){
      url <- input$IDzhantie
    }else{
      url <- input$examIDzhantie
    }
    urlx<<-url
    zhantieidstr<-strsplit(url,"\n")[[1]]
    zhantieidstr<-zhantieidstr[zhantieidstr!=""]
    zhantieidstr
  })
  
  observeEvent(input$example_clicked, {
    updateTextInput(session, "user_input", value = "I have found some genes of interest, please summary the functions of every gene and tell me what I could do next.")
  })
  
  observeEvent(input$send, {
    if(typetimes==1){
      load(file = "GOTERMdf.rdata")
      load(file = "UNIPROTids1_9606.rdata")
      zhantieidstrx<<-toupper(webpageout())
      llmmodelx<<-input$llmmodel
      if(length(zhantieidstrx)>0){
        zhantieidstrx1<-unique(UNIPROTidsdf1[UNIPROTidsdf1$UNIPROT%in%zhantieidstrx,c(1,2)])
        colnames(zhantieidstrx1)[2]<-"Names"
        zhantieidstrx2<-unique(UNIPROTidsdf1[UNIPROTidsdf1$SYMBOL%in%zhantieidstrx,c(1,3)])
        colnames(zhantieidstrx2)[2]<-"Names"
        zhantieidstrx3<-rbind(zhantieidstrx1,zhantieidstrx2)
        zhantieidstrx4<-base::merge(zhantieidstrx3,GOTERMdf,by.x="GOALL",by.y="GOID",sort=F)
        zhantieidstrx5<-apply(zhantieidstrx4,1,function(x){paste0(paste0(x,collapse = "\t"),"\n")})
        zhantieidstrx6<-paste0(zhantieidstrx5,collapse = "")
        firstsentence<-"Please learn from below contents:"
        user_messagefirst<-paste0(firstsentence,"\n",zhantieidstrx6)
        chat_history(c(chat_history(), list(list(role = "user", content = user_messagefirst))))
        messagesxx1 <<- chat_history()
        response_messagefirst <- chat(llmmodelx, messagesxx1, output = "text",
                                      keep_alive = "30m",temperature = 0.2, num_predict = 2048,
                                      stream=TRUE)
        chat_history(c(chat_history(), list(list(role = "assistant", content = response_messagefirst))))
        user_message<<-paste0(input$user_input,"\n\n",paste0(zhantieidstrx,collapse = "\n"))
      }else{
        user_message<-input$user_input
      }
      typetimes<<-typetimes+1
    }else{
      user_message<-input$user_input
    }
    chat_history(c(chat_history(), list(list(role = "user", content = user_message))))
    messagesx <<- chat_history()
    response_message <- chat(llmmodelx, messagesx, output = "text",
                             keep_alive = "30m",temperature = 0.2, num_predict = 2048,
                             stream=TRUE)
    chat_history(c(chat_history(), list(list(role = "assistant", content = response_message))))
    convert_to_html <- function(text){
      markdownToHTML(text = text, fragment.only = TRUE)
    }
    if(length(zhantieidstrx)>0){
      messagesx1 <- chat_history()[-c(1,2)]
    }else{
      messagesx1 <- chat_history()
    }
    messagesx1x<<-messagesx1
    new_chat <- lapply(messagesx1,function(x){
      if (x$role == "user"){
        paste0(
          '<div class="user"><div class="message"><strong>User:</strong> ', 
          convert_to_html(x$content), 
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
    response_messagex <<- response_message
    if (grepl("```R|```r", response_messagex) | grepl("```", response_messagex)) {
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
