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
library(esquisse)
library(ggsci)
library(openxlsx)
library(preprocessCore)
list_modelsx <- ollamar::list_models()
###########
######ui.R
###########
ui <- dashboardPage(
  title = "Normalization with LLM Assistance",
  dashboardHeader(title = "Normalization with LLM Assistance", titleWidth = 380),
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
        tags$script(type="text/javascript", src = "busy.js"),
        tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #F5F5DC;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
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
      text-align: left;
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
                radioButtons(
                  "fileType_Input",
                  label = h5("Select File Format:"),
                  choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
                  selected = 1,
                  inline = TRUE
                ),
                fileInput('file1', h5('Please import your data file:'),
                          accept=c('text/csv','text/plain','.xlsx','.xls')),
                checkboxInput('header', 'Is the first row names?', TRUE),
                checkboxInput('firstcol', 'Is the first column names?', T),
                conditionalPanel(condition = "input.fileType_Input==1",
                                 numericInput("xlsxindex",h5("Which Sheet to read?"),value = 1)),
                conditionalPanel(condition = "input.fileType_Input==2",
                                 numericInput("xlsxindex",h5("Which Sheet to read?"),value = 1)),
                conditionalPanel(condition = "input.fileType_Input==3",
                                 radioButtons('sep', 'Data Separator (Comma/Semicolon/Tab/Space):',
                                              c(Comma=',',
                                                Semicolon=';',
                                                Tab='\t',
                                                BlankSpace=' '),
                                              ','))
              ),
              conditionalPanel(
                condition = "input.loaddatatype==2",
                downloadButton("loaddatadownload1","Download example expression data",
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
              #numericInput('naratio', h5('3.1. NA ratio:'), 0.5,max = 1,min = 0,step = 0.1),
              #bsTooltip("naratio",'The threshold of NA ratio. One protein/peptide with NA ratio above this threshold will be removed.',
              #          placement = "right",options = list(container = "body")),
              selectInput(
                "dataadjust",
                label=h5(
                  "3.1. Normalization Method:",
                  tags$span(
                    id = 'span1',
                    `data-toggle` = "tooltip",
                    title = '
                Method introduction: 1. Median, mean, total normalization divides each value by the median/mean/total of its column;
                  2. Centering subtracts the mean of the column from each value (mean=0 after transformation, variance unchanged);
                  3. Range normalization subtracts the mean and divides by the difference between the max and min of the column;
                  4. (Z-score) normalization subtracts mean and divides by standard deviation;
                  5. Quantile normalization, see Bolstad et al, Bioinformatics (2003);
                  6. Scaling to a range, rescales data to a specified range.
                ',
                    tags$span(class = "glyphicon glyphicon-question-sign")
                  )
                ),
                choices = c(
                  "Median",           # 中位数
                  "Mean",             # 均值
                  "Total Intensity",  # 总强度
                  "Centering",        # 中心化
                  "Range Normalization", # 极差标准化
                  "(Z-score) Normalization", # （正态）标准化
                  "Quantile Normalization",  # 分位数标准化
                  "Scale to a Range",        # 缩放到某一个范围内
                  "Variance Stabilizing Normalization",
                  "None"              # 无
                )
              ),
              conditionalPanel(
                condition = "input.dataadjust=='Scale to a Range'",
                textInput("suofangscale",h5("3.1.1. Enter the min and max for scaling, separated by semicolon (e.g., -5;5):"),value = "-5;5")
              ),
              #numericInput("heightx",h5("3.2. Figure Height:"),600),
              #numericInput("widthx",h5("3.3. Figure Width:"),900),
              hr(),
              actionButton("mcsbtn_Barplot","Start",icon("paper-plane"),
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
              radioButtons(
                "resultsout",
                label = NULL,
                choices = list("4.1. Tables" = 1),#,"4.2. Visualization"=2
                selected = 1,
                inline = TRUE
              ),
              tags$hr(style="border-color: grey90;"),
              conditionalPanel(
                condition = "input.resultsout==2",
                downloadButton("DivergingBardefaultplotdl","Save Image"),
                div(style = "overflow-x: auto;overflow-y: auto;", plotOutput("ClusterCorNetplot",height = "550px"))#
              ),
              conditionalPanel(
                condition = "input.resultsout==1",
                downloadButton("CVtableoutdl","Download"),
                div(style = "overflow-x: auto;overflow-y: auto;", dataTableOutput("CVtableout"))#
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
                  textAreaInput("user_input", NULL, value = "", width = "100%", height = "100px",
                                placeholder = "Enter your message here...")
              )
            ),
            div(
              selectInput("llmmodel", "Choose a local LLM:", choices = list_modelsx[[1]],width="40%"),
              actionButton("example_link", "Prompt example",
                           style = "color: white; background-color: grey;margin-left: 5px;margin-top: 0px;"),
              actionButton("send", "Send", class = "btn-primary",
                           style="color: white;font-weight: bold;margin-top:0px;margin-left:60%;width:100px;")
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
              column(2, downloadButton("DivergingBarllmplotdl", "Download")),
              column(
                5,
                div(style = "display: flex; align-items:left;padding:0px;margin-left: 50px;margin-top: 0px;margin-bottom: 0px;",
                    span(h5("Height:"), style = "margin-top: 0px;margin-bottom: 0px;"),
                    numericInput("heightx2", NULL, 600)
                )
              ),
              column(
                5,
                div(style = "display: flex; align-items: left;padding:0px;margin-left: 20px;margin-right: 40px;margin-top: 0px;margin-bottom: 0px;",
                    span(h5("Width:"), style = "margin-top: 0px;margin-bottom: 0px;"),
                    numericInput("widthx2", NULL, 600)
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
    #dataread<-log2(dataread)
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
      CVtableoutx<-reactive({
        library(limma)
        normdata<<-peaksdataout()
        if(ncol(normdata)==1){
          datareadx<-data.frame(Description="No Results here. Please upload your dataset or use the example data.")
        }else{
          biaozhunhuarawdata<-dataread<-normdata
          normmethod<-isolate(input$dataadjust)
          if(normmethod=="Median"){
            biaozhunhuarawdatamean<-sweep(biaozhunhuarawdata,2,apply(biaozhunhuarawdata, 2, median,na.rm=T),FUN = "/")
          }
          else if(normmethod=="Mean"){
            biaozhunhuarawdatamean<-sweep(biaozhunhuarawdata,2,apply(biaozhunhuarawdata, 2, mean,na.rm=T),FUN = "/")
          }
          else if(normmethod=="Total Intensity"){
            biaozhunhuarawdatamean<-sweep(biaozhunhuarawdata,2,apply(biaozhunhuarawdata, 2, sum,na.rm=T),FUN = "/")
          }
          else if(normmethod=="Centering"){
            biaozhunhuarawdatamean<-sweep(biaozhunhuarawdata,2,apply(biaozhunhuarawdata, 2, mean,na.rm=T),FUN = "-")
          }
          else if(normmethod=="Range Normalization"){
            biaozhunhuacenter<-sweep(biaozhunhuarawdata,2,apply(biaozhunhuarawdata, 2, mean,na.rm=T),FUN = "-")
            biaozhunhuaR<-apply(biaozhunhuarawdata, 2, max,na.rm=T) - apply(biaozhunhuarawdata,2,min,na.rm=T)
            biaozhunhuarawdatamean<-sweep(biaozhunhuacenter,2,biaozhunhuaR,FUN ="/")
          }
          else if(normmethod=="(Z-score) Normalization"){
            biaozhunhuacenter<-sweep(biaozhunhuarawdata,2,apply(biaozhunhuarawdata, 2, mean,na.rm=T),FUN = "-")
            biaozhunhuaR<-apply(biaozhunhuarawdata, 2, sd,na.rm=T)
            biaozhunhuarawdatamean<-sweep(biaozhunhuacenter,2,biaozhunhuaR,FUN ="/")
          }
          else if(normmethod=="Quantile Normalization"){
            biaozhunhuarawdatamean<-normalize.quantiles(as.matrix(biaozhunhuarawdata),copy=TRUE)
            rownames(biaozhunhuarawdatamean)<-rownames(biaozhunhuarawdata)
            colnames(biaozhunhuarawdatamean)<-colnames(biaozhunhuarawdata)
          }
          else if(normmethod=="Scale to a Range"){
            suofangscalex<-as.numeric(strsplit(input$suofangscale,";")[[1]])
            biaozhunhuarawdatamean<-scales::rescale(as.matrix(biaozhunhuarawdata),to=suofangscalex)
          }
          else if(normmethod=="Variance Stabilizing Normalization"){
            biaozhunhuarawdatamean<-normalizeVSN(as.matrix(biaozhunhuarawdata))
          }
          else{
            biaozhunhuarawdatamean<-biaozhunhuarawdata
          }
          datareadx<-as.data.frame(biaozhunhuarawdatamean)
        }
        datareadx
      })
      output$CVtableout<-renderDataTable({
        datatable(CVtableoutx(), options = list(pageLength = 10))
      })
      output$CVtableoutdl<-downloadHandler(
        filename = function(){paste("Default_norm.table",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(CVtableoutx(),file,row.names = F)
        }
      )
      output$ClusterCorNetplot<-renderPlot({
        missvaldata<<-peaksdataout()
        if(ncol(missvaldata)==1){
          ggplot() +
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') +
            theme_void()
        }else{
          #if(input$loaddatatype==1){
          #  colorx<<-isolate(input$colorx1)
          #  clusternumx<<-isolate(input$clusternumx1)
          #}else{
          #  colorx<<-isolate(input$colorx2)
          #  clusternumx<<-isolate(input$clusternumx2)
          #}
          missvaldata1<-as.data.frame(t(missvaldata))
          missmap(missvaldata1,y.labels=rev(colnames(missvaldata)),x.cex = 0.5)
        }
      },height=heightx,width = widthx)
      ClusterCorNetplotout<-reactive({
        normdata<<-peaksdataout()
        if(ncol(normdata)==1){
          ggplot() +
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') +
            theme_void()
        }else{
          #if(input$loaddatatype==1){
          #  colorx<<-isolate(input$colorx1)
          #  clusternumx<<-isolate(input$clusternumx1)
          #}else{
          #  colorx<<-isolate(input$colorx2)
          #  clusternumx<<-isolate(input$clusternumx2)
          #}
          missvaldata1<-as.data.frame(t(missvaldata))
          missmap(missvaldata1,y.labels=rev(colnames(missvaldata)),x.cex = 0.5)
        }
      })
      output$DivergingBardefaultplotdl<-downloadHandler(
        filename = function(){paste("Default_norm",usertimenum,".pdf",sep="")},
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
    updateTextInput(session, "user_input", value = "Please show me the normalization codes. Refer to inner codes.")# and point shapes are circle
  })
  heightx2<-reactive({
    input$heightx2
  })
  widthx2<-reactive({
    input$widthx2
  })
  observeEvent(input$send, {
    normdata<<-peaksdataout()
    user_inputx<<-input$user_input
    user_inputx1<-grep("Refer to inner codes(\\.)?|=>",user_inputx,ignore.case = T)
    #req(input$user_input)
    if(length(user_inputx1)>0){
      typetext1<-'
normmethod<-"Median"
if(normmethod=="Median"){
    normdata1<-sweep(normdata,2,apply(normdata, 2, median,na.rm=T),FUN = "/")
}else if(normmethod=="Mean"){
    normdata1<-sweep(normdata,2,apply(normdata, 2, mean,na.rm=T),FUN = "/")
}else if(normmethod=="Total Intensity"){
    normdata1<-sweep(normdata,2,apply(normdata, 2, sum,na.rm=T),FUN = "/")
}else if(normmethod=="Centering"){
    normdata1<-sweep(normdata,2,apply(normdata, 2, mean,na.rm=T),FUN = "-")
}else if(normmethod=="Range Normalization"){
    biaozhunhuacenter<-sweep(normdata,2,apply(normdata, 2, mean,na.rm=T),FUN = "-")
    biaozhunhuaR<-apply(normdata, 2, max,na.rm=T) - apply(normdata,2,min,na.rm=T)
    normdata1<-sweep(biaozhunhuacenter,2,biaozhunhuaR,FUN ="/")
}else if(normmethod=="(Z-score) Normalization"){
    biaozhunhuacenter<-sweep(normdata,2,apply(normdata, 2, mean,na.rm=T),FUN = "-")
    biaozhunhuaR<-apply(normdata, 2, sd,na.rm=T)
    normdata1<-sweep(biaozhunhuacenter,2,biaozhunhuaR,FUN ="/")
}else if(normmethod=="Quantile Normalization"){
    normdata1<-normalize.quantiles(as.matrix(normdata),copy=TRUE)
    rownames(normdata1)<-rownames(normdata)
    colnames(normdata1)<-colnames(normdata)
}else if(normmethod=="Scale to a Range"){
    suofangscalex<-as.numeric(strsplit(input$suofangscale,";")[[1]])
    normdata1<-scales::rescale(as.matrix(normdata),to=suofangscalex)
}else if(normmethod=="Variance Stabilizing Normalization"){
    normdata1<-limma::normalizeVSN(as.matrix(normdata))
}else{
    normdata1<-normdata
}
normdata1
      '
      user_message <- paste0(input$user_input," Plaese just copy below all codes to give me the R codes and no interpretation:","\n",typetext1)
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
          convert_to_html(gsub(" Plaese just copy below all codes to give me the R codes and no interpretation:\n\n.*","",x$content)),
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
          paste("Adjusted.by.LLM_norm.plot",usertimenum,".pdf",sep="")
        }else{
          paste("Adjusted.by.LLM_norm.table",usertimenum,".csv",sep="")
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
