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
list_modelsx <- ollamar::list_models()
###########
######ui.R
###########
ui <- dashboardPage(
  title = "GO enrichment analysis with LLM Assistance",
  dashboardHeader(title = "GO enrichment analysis with LLM Assistance", titleWidth = 470),
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
      $('#example_link1').on('click', function(e) {
        e.preventDefault();
        Shiny.setInputValue('example_clicked1', true, {priority: 'event'});
      });
    });
  ")),
      tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('#example_link2').on('click', function(e) {
        e.preventDefault();
        Shiny.setInputValue('example_clicked2', true, {priority: 'event'});
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
                checkboxInput('firstcol', 'Is the first column names?', F),
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
              uiOutput("goortspecies"),
              colorPicker(
                inputId = "pointcolx",
                label = h5("3.2. Select two colors for Barplot:"),
                choices = list(
                  "npg"=pal_npg("nrc")(10),
                  "white"="white",
                  "grey"="grey",
                  "aaas"=pal_aaas("default")(10),
                  "nejm"=pal_nejm("default")(8),
                  "pal"=scales::brewer_pal(palette = "Paired")(8)
                ), 
                selected=c("#3C5488FF","#DC0000FF"),
                plainColor = T,
                multiple = TRUE
              ),
              numericInput("idnumx",h5("3.3. Total number of GOs displayed in the Barplot:"),15),
              numericInput("heightx",h5("3.4. Figure Height:"),600),
              numericInput("widthx",h5("3.5. Figure Width:"),600),
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
                choices = list("4.1. Tables" = 1,"4.2. Visualization"=2),
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
              actionButton("example_link1", "Prompt example 1",
                           style = "color: white; background-color: grey;margin-left: 5px;margin-top: 0px;"),
              actionButton("example_link2", "Prompt example 2",
                           style = "color: white; background-color: grey;margin-left: 5px;margin-top: 0px;"),
              actionButton("send", "Send", class = "btn-primary",
                           style="color: white;font-weight: bold;margin-top:0px;margin-left:35%;width:100px;")
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
              column(2, downloadButton("goenrichllmplotdl", "Download")),
              column(
                5,
                div(style = "display: flex; align-items:left;padding:0px;margin-left: 50px;margin-top: 0px;margin-bottom: 0px;",
                    span(h5("Height:"), style = "margin-top: 0px;margin-bottom: 0px;"),
                    numericInput("heightx2", NULL, 900)
                )
              ),
              column(
                5,
                div(style = "display: flex; align-items: left;padding:0px;margin-left: 20px;margin-right: 40px;margin-top: 0px;margin-bottom: 0px;",
                    span(h5("Width:"), style = "margin-top: 0px;margin-bottom: 0px;"),
                    numericInput("widthx2", NULL, 700)
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
  output$goortspecies<-renderUI({
    goort_spedf<-read.csv("uniprot-species.csv",header = T,stringsAsFactors = F)
    goort_spedf_paste<-paste(goort_spedf$Organism.ID,goort_spedf$Organism,sep = "-")
    selectizeInput('goortspeciesselect', h5('3.1. Please choose a species:'), choices =goort_spedf_paste,options = list(maxOptions = 6000))
  })
  examplepeakdatas<-reactive({
    dataread<-read.csv("goortid.csv",stringsAsFactors = F,check.names = F)
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("ExampleData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = T)
    }
  )
  goenrichplotdataout<-reactive({
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
    datatable(goenrichplotdataout(), options = list(pageLength = 10))
  })
  ##
  goenrichdefaulttableout<-reactive({
    pronamevec<<-goenrichplotdataout()[[1]]
    if(length(grep("^No data",pronamevec))>0){
      datareadx<-data.frame(Description="No Results here. Please upload your dataset or use the example data.")
    }else{
      library(clusterProfiler)
      library(dplyr)
      library(tidyr)
      load(file="godbdata.rdata")
      idselect<<-input$goortspeciesselect
      idselect_strsplit<-base::strsplit(as.character(idselect),"-")
      idselect_filename1<-paste("uniprot2goid/",idselect_strsplit[[1]][1],".tab",sep = "")
      tabdata<-read.csv(idselect_filename1,header = F,stringsAsFactors = F,sep="\t",quote = "")
      tabdata<-tabdata[tabdata$V2!="",]
      tabdata1<-separate_rows(tabdata, V2, sep ="; ")
      tabdata2<<-tabdata1[,c(2,1)]
      yy<<-enricher(gene=pronamevec, TERM2GENE=tabdata2, minGSSize=1, maxGSSize = 50000, pvalueCutoff = 1, qvalueCutoff = 1)
      yydf<-yy@result
      yydf1<<-base::merge(yydf,godbdata,by.x = "ID", by.y = "GOID",sort-FALSE)
      yydf2<-yydf1[,c("ID","ONTOLOGY","TERM","Count","GeneRatio","BgRatio","pvalue","p.adjust","geneID")]#1,13,14,12,,11
      #yydf2$BgRatio<-as.numeric(unlist(lapply(yydf2$BgRatio,function(x)strsplit(x,"\\/")[[1]][1])))
      #colnames(yydf2)<-c("GOIDs","Ontology","Description","Counts","Annotated","pvalue","p.adjust","IDs")
      datareadx<-yydf2[order(yydf2$Count,decreasing = T),]
    }
    datareadx
  })
  observeEvent(
    input$mcsbtn_Barplot,{
      heightx<-reactive({
        isolate(input$heightx)
      })
      widthx<-reactive({
        isolate(input$widthx)
      })
      ##
      output$CVtableout<-renderDataTable({
        allRes.df<<-goenrichdefaulttableout()
        if(length(grep("^No data",allRes.df[[1]][1]))>0){
          allRes.df<-data.frame(Description="No data. Please upload the UniProt IDs, or load the example data to check first.")
          datatable(allRes.df)
        }else{
          allRes.df[[1]] <- paste0("<a href='http://amigo.geneontology.org/amigo/term/",allRes.df[[1]],"' target='_blank'>",allRes.df[[1]],"</a>")
          datatable(allRes.df,escape = FALSE,selection="single",class = "cell-border hover",options = list(pageLength = 10,columnDefs = list(list(className = 'dt-center', targets = 0:2))))
        }
      })
      output$CVtableoutdl<-downloadHandler(
        filename = function(){paste("Default_GOenrich.table",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(goenrichdefaulttableout(),file,row.names = F)
        }
      )
      output$ClusterCorNetplot<-renderPlot({
        allRes.df1<<-goenrichdefaulttableout()
        if(length(grep("^No data",allRes.df1[[1]][1]))>0){
          ggplot() + 
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') + 
            theme_void()
        }else{
          library(ggplot2)
          pointcolx<<-isolate(input$pointcolx)
          idnumx<<-isolate(input$idnumx)
          allRes.df1.new1<-allRes.df1[order(allRes.df1$Count,decreasing = T),]
          allRes.df1.new<-allRes.df1.new1[1:idnumx,]
          ggplot(allRes.df1.new, aes(x = reorder(TERM,Count), y = Count,fill=p.adjust))+geom_bar(stat = "identity")+
            coord_flip()+scale_fill_gradient(low=pointcolx[1], high=pointcolx[2])+theme_bw()+
            xlab("Description")+ylab("Significant Number")+theme(text = element_text(size = 12))
        }
      },height=heightx,width = widthx)
      ClusterCorNetplotout<-reactive({
        allRes.df1<<-goenrichdefaulttableout()
        if(length(grep("^No data",allRes.df1[[1]][1]))>0){
          ggplot() + 
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') + 
            theme_void()
        }else{
          library(ggplot2)
          pointcolx<<-isolate(input$pointcolx)
          idnumx<<-isolate(input$idnumx)
          allRes.df1.new1<-allRes.df1[order(allRes.df1$Count,decreasing = T),]
          allRes.df1.new<-allRes.df1.new1[1:idnumx,]
          ggplot(allRes.df1.new, aes(x = reorder(TERM,Count), y = Count,fill=p.adjust))+geom_bar(stat = "identity")+
            coord_flip()+scale_fill_gradient(low=pointcolx[1], high=pointcolx[2])+theme_bw()+
            xlab("Description")+ylab("Significant Number")+theme(text = element_text(size = 12))
        }
      })
      output$DivergingBardefaultplotdl<-downloadHandler(
        filename = function(){paste("Default_GOenrich.plot",usertimenum,".pdf",sep="")},
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
  observeEvent(input$example_clicked1, {
    #I am studying lung cancer, please based on the result table obtained from above R codes, give me the most relevant go functions and 5 proteins
    updateTextInput(session, "user_input", value = "Text: Now, I am studying lung cancer and care about inflammation, please provide me 3 most relative GO IDs, descriptions and relative protein uniprot ids based on the enrichment results.")
  })
  observeEvent(input$example_clicked2, {
    #I want to process goenrich using an example data in R and and visualize individual scores using ggplot2, please put the whole R codes together
    #updateTextInput(session, "user_input", value = "The input data is named goenrichdata, in which rows are proteins and columns are samples, the first four columns are Group A and the last four columns are Group B. I want to process goenrich in R to visualize individual scores and put all codes together.")
    updateTextInput(session, "user_input", value = "Plot: The low color is changed to yellow and high color is changed to darkred for barplot colors.")
  })
  heightx2<-reactive({
    input$heightx2
  })
  widthx2<-reactive({
    input$widthx2
  })
  observeEvent(input$send, {
    #req(input$user_input)
    user_inputx<-input$user_input
    if(length(grep("^Text:?",user_inputx,ignore.case = T))>0){
      #goenrichdata<<-goenrichplotdataout()
      allRes.df<<-goenrichdefaulttableout()
      allRes.dfx<-allRes.df[allRes.df$p.adjust<=0.1,]
      #allRes.dfx1<-paste0(allRes.dfx$GOIDs,": ",allRes.dfx$Description,", ",
      #                    allRes.dfx$Ontology,", ",allRes.dfx$IDs,collapse = "; ")
      allRes.dfx1<-paste0(allRes.dfx$ID,": ",allRes.dfx$TERM,", ",
                          allRes.dfx$ONTOLOGY,", ",allRes.dfx$geneID,collapse = "; ")
      user_message<-paste0("Please learn below contents as I tell you. Below are GO contents (GO id, GO description, GO category, protein uniprot ids), every GO content is pasted with a semicolon: ",allRes.dfx1,". ",input$user_input)
    }else if(length(grep("^Plot:?|Refer to inner codes(\\.)?|=>",user_inputx,ignore.case = T))>0){
      typetext1<-'
        allRes.df1.new1<-allRes.df[order(allRes.df$Count,decreasing = T),]
        allRes.df1.new<-allRes.df1.new1[1:15,]
        ggplot(allRes.df1.new, aes(x = reorder(TERM,Count), y = Count,fill=p.adjust))+geom_bar(stat = "identity")+
          coord_flip()+scale_fill_gradient(low="blue", high="red")+theme_bw()+
          xlab("Description")+ylab("Significant Number")+theme(text = element_text(size = 12))
        '
      user_message <- paste0(input$user_input," Plaese give me all R codes based on below codes:","\n",typetext1)
    }else{
      user_message <- input$user_input
    }
    typetimes<<-typetimes+1
    chat_history(c(chat_history(), list(list(role = "user", content = user_message))))
    messagesx <<- chat_history()
    llmmodelx<<-input$llmmodel
    response_message <- chat(llmmodelx, messagesx, output = "text",#llama3 llama3.1:405b llama3:70b gemma2:27b
                             keep_alive = "30m",temperature = 0.2, num_predict = 2048,
                             stream=TRUE)
    chat_history(c(chat_history(), list(list(role = "assistant", content = response_message))))
    # Convert Markdown to HTML
    convert_to_html <- function(text) {
      markdownToHTML(text = text, fragment.only = TRUE)
    }
    messagesxxx<<-chat_history()
    new_chat <- lapply(chat_history(), function(x) {
      if (x$role == "user") {
        paste0(
          '<div class="user"><div class="message"><strong>User:</strong> ', 
          convert_to_html(gsub("Please learn below contents as I tell you. Below are GO contents \\(GO id, GO description, GO category, protein uniprot ids\\), every GO content is pasted with a semicolon: .* Text: ","",
                               gsub(" Plaese give me all R codes based on below codes:\n\n.*","",x$content))), 
          '</div></div>'
        )
      } else {
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
        plotOutput("plot_result",width="100%",height = heightx2())
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
    output$goenrichllmplotdl<-downloadHandler(
      filename = function(){
        if(is.ggplot(code_result())){
          paste("Adjusted.by.LLM_goenrich.plot",usertimenum,".pdf",sep="")
        }else{
          paste("Adjusted.by.LLM_goenrich.table",usertimenum,".csv",sep="")
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
