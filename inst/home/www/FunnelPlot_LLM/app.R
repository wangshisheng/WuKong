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
  title = "Funnel Plot with LLM Assistance",
  dashboardHeader(title = "Funnel Plot with LLM Assistance", titleWidth = 330),
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
              conditionalPanel(
                condition = "input.loaddatatype==1",
                #numericInput("clusternumx1",h5("3.1. Cluster number:"),value=NULL),
                colorPicker(
                  inputId = "colorx1",
                  label = h5("3.1. Select colors for groups:"),
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
                #bsTooltip("colorx1", "Please select two colors, one for low values and the other one for high values!","right",options = list(container = "body")),
                #selectInput("cormethodx1",h5("3.2. Correlation method:"),choices = c("pearson","spearman","kendall")),
                #selectInput("methodx1",h5("3.3. The visualization type:"),choices = c("square","circle")),
                #selectInput("typex1",h5("3.4. The matrix type:"),choices = c("full","upper","lower")),
                #div(
                #  id = "hc.orderx1id",
                #  checkboxInput(
                #    inputId = "hc.orderx1",
                #    label = span("3.5. Whether correlation matrix will be ordered?", style = "font-size: 1em;"),
                #    value = TRUE
                #  )
                #),
                #bsTooltip("hc.orderx1id", "If TRUE, correlation matrix will be hc.ordered using hclust function.","right",options = list(container = "body")),
                #textInput("limitx1",h5("3.6. Limits of the scale:"),value=""),
                #bsTooltip("limitx1", "Two values here, one for the lowest value and the other for the largest value. The two values are pasted with a semicolon ';', for example, '-1;1'. ","right",options = list(container = "body")),
                #numericInput("midpointx1",h5("3.7. Median value:"),value=NULL),
                #bsTooltip("midpointx1", "The median value of all correlation coefficients. Users can also set a custom middle value if desired.","right",options = list(container = "body")),
                #div(
                #  id = "hc.orderx1id",
                #  checkboxInput(
                #    inputId = "labx1",
                #    label = span("3.8. Whether adding correlation coefficient on the plot?", style = "font-size: 1em;"),
                #    value = TRUE
                #  )
                #)#checkboxInput('labx1', h5('3.8. Whether adding correlation coefficient on the plot?'),TRUE)
              ),
              conditionalPanel(
                condition = "input.loaddatatype==2",
                #numericInput("clusternumx2",h5("3.1. Cluster number:"),value=2),
                colorPicker(
                  inputId = "colorx2",
                  label = h5("3.1. Select colors for groups:"),
                  choices = list(
                    "npg"=pal_npg("nrc")(10),
                    "white"="white",
                    "aaas"=pal_aaas("default")(10),
                    "nejm"=pal_nejm("default")(8),
                    "pal"=scales::brewer_pal(palette = "Paired")(8)
                  ), 
                  selected=c("#3B4992FF","#BB0021FF"),
                  plainColor = T, 
                  multiple = TRUE
                ),
                #bsTooltip("colorx2", "Please select two colors, one for low values and the other one for high values!","right",options = list(container = "body")),
                #selectInput("cormethodx2",h5("3.2. Correlation method:"),choices = c("pearson","spearman","kendall")),
                #selectInput("methodx2",h5("3.3. The visualization type:"),choices = c("square","circle")),
                #selectInput("typex2",h5("3.4. The matrix type:"),choices = c("full","upper","lower")),
                #div(
                #  id = "hc.orderx1id",
                #  checkboxInput(
                #    inputId = "hc.orderx1",
                #    label = span("3.5. Whether correlation matrix will be ordered?", style = "font-size: 1em;"),
                #    value = TRUE
                #  )
                #),
                #textInput("limitx2",h5("3.6. Limits of the scale:"),value="0.9;1"),
                #numericInput("midpointx2",h5("3.7. Middle value:"),value=0.95),
                #div(
                #  id = "hc.orderx1id",
                #  checkboxInput(
                #    inputId = "labx1",
                #    label = span("3.8. Whether adding correlation coefficient on the plot?", style = "font-size: 1em;"),
                #    value = TRUE
                #  )
                #)
              ),
              numericInput("heightx",h5("3.2. Figure Height:"),600),
              numericInput("widthx",h5("3.3. Figure Width:"),600),
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
                choices = list("4.1. Visualization" = 1),
                selected = 1,
                inline = TRUE
              ),
              tags$hr(style="border-color: grey90;"),
              conditionalPanel(
                condition = "input.resultsout==1",
                downloadButton("DivergingBardefaultplotdl","Save Image"),
                div(style = "overflow-x: auto;overflow-y: auto;", plotOutput("ClusterCorNetplot",height = "550px"))#
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
    dataread<-data.frame(Sample.id=c(1,2,3,4,5,6,7,8,9,10),
                         Groups=c('M','F','M','F','F','M','F','M','F','M'),
                         n=c(130,65,155,125,19,185,82,77,50,80),
                         d=c(150,200,300,250,50,220,100,90,400,425))
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
        Funneldata<<-peaksdataout()
        if(ncol(Funneldata)==1){
          ggplot() + 
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') + 
            theme_void()
        }else{
          if(input$loaddatatype==1){
            colorx<<-isolate(input$colorx1)
            #clusternumx<<-isolate(input$clusternumx1)
          }else{
            colorx<<-isolate(input$colorx2)
            #clusternumx<<-isolate(input$clusternumx2)
          }
          library(funnelR)
          library(ggplot2)
          colnames(Funneldata)<-c("id","Groups","n","d")
          colorx1<-c("#F39B7FFF","#8491B4FF",colorx)
          names(colorx1)<-c("95% CI","99% CI",unique(Funneldata$Groups))
          my_fpdata <- fundata(input=Funneldata,
                               benchmark=0.5,
                               alpha=0.95,
                               alpha2=0.99,
                               method='approximate',
                               step=0.5)
          ggplot() +
            geom_line(data = my_fpdata, aes(x = d, y = up, color = "95% CI"), size = 1) +
            geom_line(data = my_fpdata, aes(x = d, y = lo, color = "95% CI"), size = 1) +
            geom_line(data = my_fpdata, aes(x = d, y = up2, color = "99% CI"), 
                      linetype = "dashed", size = 1) +
            geom_line(data = my_fpdata, aes(x = d, y = lo2, color = "99% CI"), 
                      linetype = "dashed", size = 1) +
            geom_hline(yintercept = 0.5, color = "#003C67FF") +
            geom_point(data = Funneldata, aes(x = d, y = n/d, color = Groups), size = 2) +
            #scale_color_manual(values = c("95% CI" = "#F39B7FFF","99% CI" = "#8491B4FF","F" = "blue","M" = "red")) +
            scale_color_manual(values = colorx1) +
            guides(color = guide_legend(position = "bottom")) +
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
        Funneldata<<-peaksdataout()
        if(ncol(Funneldata)==1){
          ggplot() + 
            annotate("text", x = 4, y = 25, size=6,col="red", label = 'No uploaded data and No plot here.\n Please upload data or click the example data.') + 
            theme_void()
        }else{
          if(input$loaddatatype==1){
            colorx<<-isolate(input$colorx1)
            #clusternumx<<-isolate(input$clusternumx1)
          }else{
            colorx<<-isolate(input$colorx2)
            #clusternumx<<-isolate(input$clusternumx2)
          }
          library(funnelR)
          library(ggplot2)
          colnames(Funneldata)<-c("id","Groups","n","d")
          colorx1<-c("#F39B7FFF","#8491B4FF",colorx)
          names(colorx1)<-c("95% CI","99% CI",unique(Funneldata$Groups))
          my_fpdata <- fundata(input=Funneldata,
                               benchmark=0.5,
                               alpha=0.95,
                               alpha2=0.99,
                               method='approximate',
                               step=0.5)
          ggplot() +
            geom_line(data = my_fpdata, aes(x = d, y = up, color = "95% CI"), size = 1) +
            geom_line(data = my_fpdata, aes(x = d, y = lo, color = "95% CI"), size = 1) +
            geom_line(data = my_fpdata, aes(x = d, y = up2, color = "99% CI"), 
                      linetype = "dashed", size = 1) +
            geom_line(data = my_fpdata, aes(x = d, y = lo2, color = "99% CI"), 
                      linetype = "dashed", size = 1) +
            geom_hline(yintercept = 0.5, color = "#003C67FF") +
            geom_point(data = Funneldata, aes(x = d, y = n/d, color = Groups), size = 2) +
            #scale_color_manual(values = c("95% CI" = "#F39B7FFF","99% CI" = "#8491B4FF","F" = "blue","M" = "red")) +
            scale_color_manual(values = colorx1) +
            guides(color = guide_legend(position = "bottom")) +
            theme_bw()
        }
      })
      output$DivergingBardefaultplotdl<-downloadHandler(
        filename = function(){paste("Default_Funnel.plot",usertimenum,".pdf",sep="")},
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
    updateTextInput(session, "user_input", value = "Please change the 95% CI line color to blue, 99% CI line color to red, the point color to yellow and green. Refer to inner codes.")# and point shapes are circle
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
    Funneldata<<-peaksdataout()
    user_inputx<<-input$user_input
    user_inputx1<-grep("Refer to inner codes(\\.)?|=>",user_inputx,ignore.case = T)
    #req(input$user_input)
    if(length(user_inputx1)>0){
      typetext1<-'
library(funnelR)
library(ggplot2)
colnames(Funneldata)<-c("id","Groups","n","d")
colorx1<-c("#F39B7FFF","#8491B4FF","yellow","green")
names(colorx1)<-c("95% CI","99% CI",unique(Funneldata$Groups))
my_fpdata <- fundata(input=Funneldata,benchmark=0.5,alpha=0.95,alpha2=0.99,method="approximate",step=0.5)
ggplot() +
  geom_line(data = my_fpdata, aes(x = d, y = up, color = "95% CI"), size = 1) +
  geom_line(data = my_fpdata, aes(x = d, y = lo, color = "95% CI"), size = 1) +
  geom_line(data = my_fpdata, aes(x = d, y = up2, color = "99% CI"), 
  linetype = "dashed", size = 1) +
  geom_line(data = my_fpdata, aes(x = d, y = lo2, color = "99% CI"), 
  linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0.5, color = "#003C67FF") +
  geom_point(data = Funneldata, aes(x = d, y = n/d, color = Groups), size = 2) +
  scale_color_manual(values = colorx1) +
  guides(color = guide_legend(position = "bottom")) +
  theme_bw()
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
          paste("Adjusted.by.LLM_Funnel.plot",usertimenum,".pdf",sep="")
        }else{
          paste("Adjusted.by.LLM_Funnel.table",usertimenum,".csv",sep="")
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
