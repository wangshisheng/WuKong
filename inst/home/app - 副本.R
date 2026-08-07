library(shiny)
library(shinyBS)
library(shinyjqui)
library(openxlsx)
library(gdata)
library(DT)
#library(tidyllm)
library(ollamar)
library(markdown)
library(shinyAce)
library(zip)
library(commonmark)
library(preprocessCore)
list_modelsx <- ollamar::list_models()
load(file = "WKfuncslist1.rdata")
oneclick_summary_text <- reactiveVal("")
###########
######ui.R
###########
ui <- shinyUI(
  fluidPage(
    style = "min-width:1400px; background: linear-gradient(to bottom, #f9fafb, #e5e7eb); font-family: 'Roboto', sans-serif;", # Light gradient with modern font
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "busystyle.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "mainstyle.css"),
        tags$script(type = "text/javascript", src = "busy.js"),
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
                     ")
      ),#position: fixed;
      tags$style(HTML("
      .section-container {
        border: 1px solid #dfe6e9;
        border-radius: 8px;
        background-color: #ffffff;
        padding: 20px;
        margin-bottom: 20px;
        margin-top: 62px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
      .input-container {
    bottom: 0;
    left: 0;
    width: 100%;
    height:210px;
    margin-top: -10px;
    background-color: #f9f9f9;
    border-top: 1px solid #ddd;
    padding: 10px;
    box-shadow: 0 -2px 5px rgba(0, 0, 0, 0.1);
    z-index: 9999;
    }
      .section-header {
        font-size: 20px;
        font-weight: bold;
        color: #34495e;
        margin-bottom: 10px;
        padding: 10px;
        border-bottom: 2px solid #dfe6e9;
      }
      .btn-primary {
        background-color: #3498db;
        color: white;
        border: none;
        padding: 10px 0px;
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
      max-height: 550px;
        height: 500px;
        overflow-y: auto;
        padding: 15px;
        border: 1px solid #dfe6e9;
        border-radius: 8px;
        background-color: #f9f9f9;
        margin-bottom: 10px;
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
    tags$style(HTML("
                  #datapregallery {
                    height:900px;
                    overflow-y:scroll
                  }
                  ")),
    tags$style(HTML("
                  #analysisgallery {
                    height:900px;
                    overflow-y:scroll
                  }
                  ")),
    tags$style(HTML("
                  #functionalgallery {
                    height:900px;
                    overflow-y:scroll
                  }
                  ")),
    tags$style(HTML("
                  #visualizationgallery {
                    height:900px;
                    overflow-y:scroll
                  }
                  ")),
    #div(class = "busy",
    #    h2(strong("Thinking..."), style = "color: #374151; font-weight: 500;"), # Neutral text color
    #    img(src = "rmd_loader.gif")
    #),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",id="loadmessage",
                     tags$div(h2(strong("Thinking...")),img(src="rmd_loader.gif"))),
    navbarPage(
      title="",windowTitle="WuKong Platform", fluid = F, position = "fixed-top", id = "navbarid",
      tabPanel(
        "Home",
        uiOutput("welcomeui"),
        icon = icon("home")
      ),
      tabPanel(
        "Functions",
        value = "functionspanel",
        div(style = "margin-top:3px; margin-left:0%; z-index:9999; position:absolute;",
            img(src = "wukonglogo.png", width = "135px")),
        navlistPanel(
          id = "gongnengquanid",
          tabPanel(
            "1. Conversation",
            value = "Conversationpanel",
            fluidRow(
              div(
                class = "section-container",
                div(class = "section-header", "1.1. Chat contents"),
                div(class = "chat-container", htmlOutput("chat_output"))
              ),
              div(
                class = "input-container",
                div(class = "section-header", "1.2. Ask something..."),
                #selectInput("llmmodel", "Choose a Model:", choices = list_modelsx[[1]]),
                column(
                  8,
                  div(
                    style = "display: flex; align-items: center; justify-content: space-between;",
                    # Text area on the left
                    textAreaInput("user_input", NULL, value = "", width = "100%", height = "120px",
                                  placeholder = "Type your message here..."),

                  )
                ),
                column(
                  4,
                  div(
                    style = "margin-top: 0px;margin-left: 0px;",
                    fluidRow(
                      tags$label("Choose a Model:", style = "margin-bottom: 0px;")
                    ),
                    fluidRow(
                      selectInput("llmmodel", NULL, choices = list_modelsx[[1]], width = "100%"),
                    ),
                    fluidRow(
                      actionButton("send", "Send", class = "btn-primary")#,
                      #actionButton("example_link", "Prompt example",
                      #             style = "color: white; background-color: grey; margin-bottom: 0px;margin-left: 20px;")
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "2. Data Pre-processing",
            value = "datapregallerypanel",
            uiOutput("datapregallery")
          ),
          tabPanel(
            "3. Statistical Analysis",
            value = "analysisgallerypanel",
            uiOutput("analysisgallery")
          ),
          tabPanel(
            "4. Functional Annotation Analysis",
            value = "functionalgallerypanel",
            uiOutput("functionalgallery")
          ),
          tabPanel(
            "5. Data Visualization",
            value = "visualizationgallerypanel",
            uiOutput("visualizationgallery")
          ),
          tabPanel(
            "6. Workflow Designer",
            uiOutput("oneclickgallery")
          ),
          well = TRUE,
          fluid = F,
          widths = c(2, 10)
        ),
        icon = icon("cogs")
      ),
      tabPanel(
        "Help",
        div(style = "margin-top:-57px; margin-left:0%; z-index:9999; position:absolute;",
            img(src = "wukonglogo.png", width = "135px")),
        div(
          style = "margin-top:60px; padding: 20px; background-color: #f9f9f9; border-radius: 10px;",
          h2("WuKong Platform Help Center", style = "text-align:center; color: #1d4ed8;"),
          p("Welcome to the Help Center! Here, you can find information on how to use the WuKong Platform effectively.",
            style = "text-align:center; font-size: 16px; color: #4b5563;"),

          # General Overview Section
          div(
            style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
            h3("General Overview", style = "color: #2563eb;"),
            p("WuKong is a powerful platform designed to elevate the way researchers approach proteomics data analysis. It integrates over 72 well-established analysis modules that cover the entire range of data preprocessing, statistical analysis, visualization, functional annotation and workflow designer. On the one hand, these modules retain the classic analysis frameworks, allowing users to adjust parameters within each module to directly generate results tailored to their specific research needs. This flexibility ensures that both beginners and experienced bioinformaticians can efficiently conduct complex analyses without being constrained by rigid workflows. On the other hand, WuKong incorporates large language models (LLMs) to enhance user experience by facilitating conversational interactions with the platform. Researchers can select from a variety of locally installed LLMs, enabling them to conduct analyses through an intuitive chat interface. This approach not only simplifies the analytical process but also provides real-time guidance and support, making it easier for users to navigate complex datasets and analytical procedures."),
            p("Key features include:"),
            tags$ul(
              tags$li("Comprehensive Modular Coverage: WuKong integrates over 72 modular tools for data preprocessing, statistical analysis, functional enrichment, visualization, and workflow design, providing a versatile framework for complex proteomics researches."),
              tags$li("Dual-mode Analytical Interaction: Users can conduct analyses through either conventional GUI-based modules or dynamic, natural language-driven interactions powered by locally deployed LLMs, greatly enhancing accessibility for users with diverse computational backgrounds."),
              tags$li("Workflow Designer for Flexible Pipelines: The innovative workflow designer enables users to freely select, combine, and execute multiple analysis modules in custom pipelines, overcoming the step-by-step limitations of conventional tools and streamlining complex analyses."),
              tags$li("Prompt-aware Architecture for Enhanced Interpretability: WuKong’s unique prompt-aware system allows LLMs to reference internal code logic, ensuring that natural language queries yield precise, transparent and reproducible results tailored to user intentions."),
              tags$li("Scalable LLM Integration: The platform supports multiple local LLM backends via Ollama, ranging from lightweight to large-scale models. This flexibility balances resource efficiency with accuracy, enabling both exploratory tasks and publication-grade analyses.")
            )
          ),
          # Source codes and user manual
          div(
            style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
            h3("Source codes and user manual", style = "color: #2563eb;"),
            p("Source codes can be accessed at our ",
              tags$a(href = "https://github.com/wangshisheng/WuKong", "GitHub Repository", target = "_blank"),
              " and the detailed user manual can be downloaded from ",
              tags$a(href = "https://github.com/wangshisheng/WuKong/blob/master/UserManual.pdf", "User Manual", target = "_blank"), ".")
          ),
          # Navigation Section
          #div(
          #  style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
          #  h3("Navigation", style = "color: #2563eb;"),
          #  p("The platform is organized into the following sections:"),
          #  tags$ul(
          #    tags$li(tags$b("Home:"), " Provides an introduction to the platform and its features."),
          #    tags$li(tags$b("Functions:"), " Contains a variety of modules for data analysis, preprocessing, and visualization."),
          #    tags$li(tags$b("Help:"), " This section provides information on how to use the platform.")
          #  )
          #),

          # Using the Platform Section
          #div(
          #  style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
          #  h3("How to Use the Platform", style = "color: #2563eb;"),
          #  p("Follow these steps to get started:"),
          #  tags$ol(
          #    tags$li("Navigate to the ", tags$b("Functions"), " tab to explore available modules."),
          #    tags$li("Select a module that fits your analysis needs."),
          #    tags$li("Provide the required input data and parameters."),
          #    tags$li("Click ", tags$b("Start"), " to execute the module."),
          #    tags$li("View and download the results.")
          #  )
          #),

          # Model Descriptions Section
          #div(
          #  style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
          #  h3("Model Descriptions", style = "color: #2563eb;"),
          #  p("Below is a detailed description of each model available in the platform and how to use them:"),
          #
          #  # Model 1: Conversation
          #  div(
          #    style = "margin-top:10px;",
          #    h4("1. Conversation", style = "color: #1f2937;"),
          #    p("The Conversation module allows you to interact with local large language models (LLMs) for various queries and tasks. It supports models such as Llama, Gemma, and others."),
          #    p("How to use:"),
          #    tags$ol(
          #      tags$li("Select a model from the dropdown menu in the 'Conversation' section."),
          #      tags$li("Type your query or request in the text box."),
          #      tags$li("Click the ", tags$b("Send"), " button to receive a response.")
          #    ),
          #    p("Example use cases:"),
          #    tags$ul(
          #      tags$li("Ask for help with data preprocessing."),
          #      tags$li("Request code snippets for specific analyses."),
          #      tags$li("Seek guidance on statistical methods.")
          #    )
          #  ),
          #
          #  # Model 2: Data Preprocessing
          #  div(
          #    style = "margin-top:10px;",
          #    h4("2. Data Preprocessing", style = "color: #1f2937;"),
          #    p("This module provides tools for cleaning and preparing your data, including normalization, missing value imputation, and coefficient of variation calculation."),
          #    p("How to use:"),
          #    tags$ol(
          #      tags$li("Navigate to the 'Data Preprocessing' tab."),
          #      tags$li("Select the desired preprocessing task, such as 'Normalization' or 'Missing Value Imputation'."),
          #      tags$li("Upload your dataset and specify the required parameters."),
          #      tags$li("Click ", tags$b("Start"), " to process your data.")
          #    ),
          #    p("Example tasks:"),
          #    tags$ul(
          #      tags$li("Normalize a dataset for downstream analysis."),
          #      tags$li("Handle missing values in a large dataset."),
          #      tags$li("Calculate and visualize the coefficient of variation (CV).")
          #    )
          #  ),
          #
          #  # Model 3: Statistical Analysis
          #  div(
          #    style = "margin-top:10px;",
          #    h4("3. Statistical Analysis", style = "color: #1f2937;"),
          #    p("Perform advanced statistical analyses such as PCA, clustering, regression, and differential expression analysis."),
          #    p("How to use:"),
          #    tags$ol(
          #      tags$li("Navigate to the 'Statistical Analysis' tab."),
          #      tags$li("Select the statistical method you want to perform, such as PCA or ANOVA."),
          #      tags$li("Upload your dataset and configure the analysis settings."),
          #      tags$li("Click ", tags$b("Start"), " to run the analysis.")
          #    ),
          #    p("Example analyses:"),
          #    tags$ul(
          #      tags$li("Perform Principal Component Analysis (PCA) to reduce data dimensionality."),
          #      tags$li("Use clustering methods to group similar data points."),
          #      tags$li("Conduct ANOVA to identify significant differences between groups.")
          #    )
          #  ),
          #
          #  # Model 4: Functional Annotation
          #  div(
          #    style = "margin-top:10px;",
          #    h4("4. Functional Annotation", style = "color: #1f2937;"),
          #    p("Analyze gene and protein functions using tools like GO enrichment and KEGG pathway analysis."),
          #    p("How to use:"),
          #    tags$ol(
          #      tags$li("Navigate to the 'Functional Annotation' tab."),
          #      tags$li("Select a tool, such as 'GO Enrichment' or 'KEGG Pathway Analysis'."),
          #      tags$li("Upload your gene or protein list and specify the analysis parameters."),
          #      tags$li("Click ", tags$b("Start"), " to execute the analysis.")
          #    ),
          #    p("Example tasks:"),
          #    tags$ul(
          #      tags$li("Identify enriched biological processes using GO analysis."),
          #      tags$li("Explore pathways associated with a gene list using KEGG enrichment.")
          #    )
          #  ),
          #
          #  # Model 5: Data Visualization
          #  div(
          #    style = "margin-top:10px;",
          #    h4("5. Data Visualization", style = "color: #1f2937;"),
          #    p("Create publication-quality visualizations, including bar plots, PCA plots, and heatmaps."),
          #    p("How to use:"),
          #    tags$ol(
          #      tags$li("Navigate to the 'Data Visualization' tab."),
          #      tags$li("Select a visualization type, such as 'Bar Plot' or 'Heatmap'."),
          #      tags$li("Upload your data and customize the plot settings."),
          #      tags$li("Click ", tags$b("Start"), " to generate the plot.")
          #    ),
          #    p("Example visualizations:"),
          #    tags$ul(
          #      tags$li("Generate a heatmap to visualize data clusters."),
          #      tags$li("Create a bar plot to compare categorical data."),
          #      tags$li("Visualize PCA results in a scatter plot.")
          #    )
          #  )
          #),
          #
          ## FAQs Section
          #div(
          #  style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
          #  h3("Frequently Asked Questions (FAQs)", style = "color: #2563eb;"),
          #  tags$ul(
          #    tags$li(tags$b("Q: How do I select a model for the conversation module?"),
          #            tags$p("A: Use the dropdown menu in the 'Conversation' section to choose a model.")),
          #    tags$li(tags$b("Q: Can I download the results?"),
          #            tags$p("A: Yes, you can download results in various formats, including CSV and PDF.")),
          #    tags$li(tags$b("Q: What should I do if I encounter an error?"),
          #            tags$p("A: Clear the chat or restart the module. If the issue persists, contact support."))
          #  )
          #),

          # Contact Section
          div(
            style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
            h3("Contact Us", style = "color: #2563eb;"),
            p("For further assistance, please contact our support team at ",
              tags$a(href="mailto:shishengwang@wchscu.cn", "shishengwang@wchscu.cn"), ".")
          )
        ),
        icon = icon("binoculars")
      )
    )
  )
)

###########
######server.R
###########
server <- shinyServer(
  function(input, output, session) {
    options(shiny.maxRequestSize=100*1024^2)
    # welcomeui
    output$welcomeui <- renderUI({
      fluidRow(
        # Logo Section
        fluidRow(
          div(style = "margin-top:3px; margin-left:18%; z-index:9999; position: absolute;",
              img(src = "wukonglogo.png", width = "135px"))
        ),
        # Welcome Section
        div(
          style = "margin-top:60px; background: linear-gradient(to bottom, white,white); color: #111827; height:770px; padding: 30px 15px; border-radius: 10px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
          div(
            style = "padding-top:5px; text-align:center; font-size:280%; font-weight:bold; color: #2563eb;",
            HTML("Welcome to WuKong Platform!")
          ),
          div(
            style = "padding-top:5px; font-size:140%; font-weight:normal; max-width: 1000px; margin: auto; line-height: 1.8; color: #4b5563;",
            HTML("WuKong is an advanced and open-source platform for analyzing and interpreting proteomics data. It integrates over 72 modular tools with large language models (LLMs), bridging both classical data analysis and natural language interactions. WuKong enables data preprocessing, statistical analysis, functional enrichment, visualization and workflow design. Through the prompt-aware architecture, it streamlines analysis and expands accessibility for researchers with diverse computational expertise. WuKong offers versatility for both routine investigations and cutting-edge exploratory studies.<br />")
          ),
          div(
            style = "margin-top:10px; text-align:center;",
            img(src = "shouyefazhan.png", width = "1000px")
          )
        ),

        # Section Title
        div(
          style = "margin-top:30px; text-align:center; font-size:240%; font-weight:bold; color:#1d4ed8;",
          HTML("Function Modules")
        ),

        # Function Modules Section
        div(
          id = "moduleup1",
          fluidRow(
            column(
              6,
              div(
                style = "text-align:center; margin-top:20px; margin-left:30%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(
                  img(src = 'conversation.png', height = "300px")
                ),
                div(
                  style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                  HTML("Conversation")
                ),
                div(
                  style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;",
                  HTML("Chat with local large language models <br />that are deployed locally.")
                ),
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton("button_module1", "Learn More", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
                )
              )
            ),
            column(
              6,
              div(
                style = "text-align:center; margin-top:20px; margin-right:30%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(
                  img(src = 'datapreprocess.png', height = "300px")
                ),
                div(
                  style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                  HTML("Data Pre-processing")
                ),
                div(
                  style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;",
                  HTML("Normalize data, handle missing values,<br />calculate coefficients, and more.")
                ),
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton("button_module2", "Learn More", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
                )
              )
            )
          ),
          fluidRow(
            column(
              4,
              div(
                style = "text-align:center; margin-top:40px; margin-left:10%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(
                  img(src = 'danyuanduoyuan.png', height = "300px")
                ),
                div(
                  style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                  HTML("Statistical Analysis")
                ),
                div(
                  style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;",
                  HTML("Perform PCA, clustering, regression,<br />PLS-DA, OPLS-DA, and more.")
                ),
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton("button_module3", "Learn More", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
                )
              )
            ),
            column(
              4,
              div(
                style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(
                  img(src = 'gongneng.png', height = "300px")
                ),
                div(
                  style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                  HTML("Functional Analysis")
                ),
                div(
                  style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;",
                  HTML("ID conversion, enrichment analysis,<br />custom library enrichment, and more.")
                ),
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton("button_module4", "Learn More", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
                )
              )
            ),
            column(
              4,
              div(
                style = "text-align:center; margin-top:40px; margin-right:10%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(
                  img(src = 'huatu.png', height = "300px")
                ),
                div(
                  style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                  HTML("Data Visualization")
                ),
                div(
                  style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;",
                  HTML("Create diverse plots with ease.<br />Click for details.")
                ),
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton("button_module5", "Learn More", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
                )
              )
            )
          )
        ),

        # Footer Section
        div(
          style = "margin-top:50px; background:#1f2937; height:100px; padding: 30px 15px;",
          div(
            style = "text-align:center; font-size:100%; color:white;",
            HTML("&copy; 2025 WuKong Platform")#Shisheng Wang, Hao Yang and Chenpin Shen
          )
        )
      )
    })
    ###
    observeEvent(input$button_module1, {
      updateNavbarPage(session, "navbarid",selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "Conversationpanel")
    })
    observeEvent(input$button_module2, {
      updateNavbarPage(session, "navbarid",selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "datapregallerypanel")
    })
    observeEvent(input$button_module3, {
      updateNavbarPage(session, "navbarid",selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "analysisgallerypanel")
    })
    observeEvent(input$button_module4, {
      updateNavbarPage(session, "navbarid",selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "functionalgallerypanel")
    })
    observeEvent(input$button_module5, {
      updateNavbarPage(session, "navbarid",selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "visualizationgallerypanel")
    })
    ##
    output$datapregallery <- renderUI({
      fluidRow(
        style = "margin-top:60px;",
        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(
              img(src = "CV_LLM.png", width = "200px", height = "200px")
            ),
            div(
              style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
              HTML("Coefficient of variation")
            ),
            div(
              style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
              HTML("Calculate and visualize the coefficient of variation (CV) with LLM support. This tool is essential for assessing data dispersion relative to the mean.")
            ),
            div(
              style = "text-align:center; margin-top:15px;",
              actionButton("btnCV_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
            )
          )
        ),
        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(
              img(src = "MissingValue_LLM.png", width = "200px", height = "200px")
            ),
            div(
              style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
              HTML("Missing value imputation")
            ),
            div(
              style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
              HTML("Handle missing data effectively with LLM-guided imputation methods. This tool ensures data completeness and reliability for analysis.")
            ),
            div(
              style = "text-align:center; margin-top:15px;",
              actionButton("btnMissingValue_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
            )
          )
        ),
        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(
              img(src = "Norm_LLM.png", width = "200px", height = "200px")
            ),
            div(
              style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
              HTML("Normalization")
            ),
            div(
              style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
              HTML("Normalize datasets effectively with LLM guidance. This tool ensures data consistency and comparability for downstream analysis.")
            ),
            div(
              style = "text-align:center; margin-top:15px;",
              actionButton("btnNorm_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
            )
          )
        ),
        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(
              img(src = "TableMerge_LLM.png", width = "200px", height = "200px")
            ),
            div(
              style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
              HTML("Two tables merging")
            ),
            div(
              style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
              HTML("Merge two tables efficiently with LLM support. This tool ensures seamless integration of datasets for further analysis.")
            ),
            div(
              style = "text-align:center; margin-top:15px;",
              actionButton("btnTableMerge_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
            )
          )
        ),
        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(
              img(src = "AnalyzeWebPage_LLM.png", width = "200px", height = "200px")
            ),
            div(
              style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
              HTML("Webpage content analysis")
            ),
            div(
              style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
              HTML("This tool enables the analysis of webpage content using LLMs. It extracts meaningful insights of the textual data present on web pages.")
            ),
            div(
              style = "text-align:center; margin-top:15px;",
              actionButton("btnAnalyzeWebPage_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
            )
          )
        )
      )
    })
    observeEvent(input$btnAnalyzeWebPage_LLM,{
      rstudioapi::jobRunScript(system.file('home/btnAnalyzeWebPage_LLM.R', package='WuKong'))
    })
    observeEvent(input$btnCV_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnCV_LLM.R', package='WuKong'))
    })
    observeEvent(input$btnMissingValue_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnMissingValue_LLM.R', package='WuKong'))
    })
    observeEvent(input$btnNorm_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnNorm_LLM.R', package='WuKong'))
    })
    observeEvent(input$btnTableMerge_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnTableMerge_LLM.R', package='WuKong'))
    })
    ##
    output$analysisgallery<-renderUI({
      div(
        style="margin-top:60px;",
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ConsensusClustering_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/ConsensusClustering_LLM", target = "_blank",
                #       #tags$img(src = "ConsensusClustering_LLM.png",alt = "Consensus clustering", width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Consensus clustering")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Achieve robust clustering results through consensus clustering with LLM support. This tool enhances data reliability by combining multiple clustering results for improved accuracy.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnConsensusClustering_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DEPannova_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/DEPannova_LLM", target = "_blank",
                #       #tags$img(src = "DEPannova_LLM.png",alt = "One-way ANOVA",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("One-way ANOVA")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform differential expression analysis using one-way ANOVA tests with the help of LLMs. This tool is valuable for identifying significant differences between groups in datasets.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDEPannova_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DEPlimma_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/DEPlimma_LLM", target = "_blank",
                #       #tags$img(src = "DEPlimma_LLM.png",alt = "Limma",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Limma")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct differential expression analysis with the Limma package, guided by LLMs. This method is widely used in omics research.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDEPlimma_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DEPsamr_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/DEPsamr_LLM", target = "_blank",
                #       #tags$img(src = "DEPsamr_LLM.png",alt = "SAMR",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("SAMR")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Utilize SAMR (Significance Analysis of Microarrays) for differential expression analysis. LLMs provide step-by-step support for accurate and efficient analysis.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDEPsamr_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DEPttest_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/DEPttest_LLM", target = "_blank",
                #       #tags$img(src = "DEPttest_LLM.png",alt = "Student's t-Test",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Student's t-Test")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform differential expression analysis using Student's t-test with LLM guidance. This statistical method identifies significant differences between two groups.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDEPttest_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DEPwilcox_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/DEPwilcox_LLM", target = "_blank",
                #       #tags$img(src = "DEPwilcox_LLM.png",alt = "Wilcoxon Rank Sum and Signed Rank Tests",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Wilcoxon Rank Sum and Signed Rank Tests")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct non-parametric differential expression analysis using Wilcoxon tests. LLMs assist in ensuring accurate and reliable results.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDEPwilcox_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DNB_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/DNB_LLM", target = "_blank",
                #       #tags$img(src = "DNB_LLM.png",alt = "Dynamic Network Biomarkers",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Dynamic Network Biomarkers")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Identify dynamic network biomarkers (DNBs) with LLM support. This tool is crucial for understanding critical transitions in biological systems.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDNB_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'FactorAnalysis_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/FactorAnalysis_LLM", target = "_blank",
                #       #tags$img(src = "FactorAnalysis_LLM.png",alt = "Factor Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Factor Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform factor analysis to uncover underlying variables in datasets. LLMs guide users through the process, ensuring accurate and interpretable results.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnFactorAnalysis_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'GRA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/GRA_LLM", target = "_blank",
                #       #tags$img(src = "GRA_LLM.png",alt = "Grey Relational Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Grey Relational Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct Grey Relational Analysis (GRA) to evaluate relationships between variables. LLMs provide insights and simplify the analysis process for better decision-making.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnGRA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'HCA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/HCA_LLM", target = "_blank",
                #       #tags$img(src = "HCA_LLM.png",alt = "Hierarchical Cluster Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Hierarchical Cluster Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform hierarchical cluster analysis (HCA) with LLM support. This tool helps organize data into meaningful groups based on similarity.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnHCA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Kmeans_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/Kmeans_LLM", target = "_blank",
                #       #tags$img(src = "Kmeans_LLM.png",alt = "K-means clustering",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("K-means Clustering")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct K-means clustering with LLM support. This tool helps partition datasets into clusters based on similarity, simplifying data segmentation.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnKmeans_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'LackofFit_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/LackofFit_LLM", target = "_blank",
                #       #tags$img(src = "LackofFit_LLM.png",alt = "Lack of Fit F-test",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Lack of Fit F-test")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Lack of Fit F-tests to evaluate the adequacy of regression models. LLMs assist in interpreting results and identifying model improvements.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnLackofFit_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'LinearRegression_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/LinearRegression_LLM", target = "_blank",
                #       #tags$img(src = "LinearRegression_LLM.png",alt = "Linear Regression",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Linear Regression")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct linear regression analysis with the help of LLMs. This tool models relationships between variables and provides insights into predictive trends.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnLinearRegression_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'LogisticRegression_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/LogisticRegression_LLM", target = "_blank",
                #       #tags$img(src = "LogisticRegression_LLM.png",alt = "Logistic Regression",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Logistic Regression")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform logistic regression analysis with LLM support. This statistical method is ideal for modeling binary outcomes and identifying significant predictors.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnLogisticRegression_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Mfuzz_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/Mfuzz_LLM", target = "_blank",
                #       #tags$img(src = "Mfuzz_LLM.png",alt = "Mfuzz",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Mfuzz")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform fuzzy clustering using the Mfuzz package with LLM guidance. This tool is particularly useful for analyzing time-series data in biological studies.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnMfuzz_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'NDM_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/NDM_LLM", target = "_blank",
                #       #tags$img(src = "NDM_LLM.png",alt = "Network Degree Matrix",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Network Degree Matrix")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Analyze network structures using the Network Degree Matrix (NDM). LLMs assist in interpreting network connectivity and relationships.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnNDM_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'OPLSDA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/OPLSDA_LLM", target = "_blank",
                #       #tags$img(src = "OPLSDA_LLM.png",alt = "OPLS-DA",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("OPLS-DA")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA) with LLM support. This tool is widely used for classification and biomarker discovery.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnOPLSDA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'PCA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/PCA_LLM", target = "_blank",
                #       #tags$img(src = "PCA_LLM.png",alt = "PCA",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("PCA")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct Principal Component Analysis (PCA) with LLM guidance. This tool reduces data dimensionality while preserving important information.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnPCA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'PCoA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/PCoA_LLM", target = "_blank",
                #       #tags$img(src = "PCoA_LLM.png",alt = "PCoA",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("PCoA")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Principal Coordinates Analysis (PCoA) to explore similarities or dissimilarities in data. LLMs guide users through the process for effective visualization.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnPCoA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'PLSDA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/PLSDA_LLM", target = "_blank",
                #       #tags$img(src = "PLSDA_LLM.png",alt = "PLS-DA",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("PLS-DA")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct Partial Least Squares Discriminant Analysis (PLS-DA) with LLM support. This tool is ideal for classification and feature selection in high-dimensional data.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnPLSDA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'PowerAnalysis_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/PowerAnalysis_LLM", target = "_blank",
                #       #tags$img(src = "PowerAnalysis_LLM.png",alt = "Power Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Power Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform power analysis to determine the sample size needed for statistical tests. LLMs provide insights and ensure accurate calculations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnPowerAnalysis_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RCS_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/RCS_LLM", target = "_blank",
                #       #tags$img(src = "RCS_LLM.png",alt = "Restricted Cubic Spline Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Restricted Cubic Spline Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Restricted Cubic Spline (RCS) analysis to model non-linear relationships. LLMs assist in interpreting results and creating visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRCS_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RDA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/RCS_LLM", target = "_blank",
                #       #tags$img(src = "RCS_LLM.png",alt = "Restricted Cubic Spline Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Redundancy Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct Redundancy Analysis (RDA) to explore relationships between datasets. LLMs provide guidance for accurate and interpretable results.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRDA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RRHO_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/RRHO_LLM", target = "_blank",
                #       #tags$img(src = "RRHO_LLM.png",alt = "Rank Rank Hypergeometric Overlap Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Rank Rank Hypergeometric Overlap Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Rank Rank Hypergeometric Overlap Analysis (RRHO) analysis to identify overlaps between ranked datasets. LLMs guide users in uncovering meaningful intersections.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRRHO_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'SIMCA_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/SIMCA_LLM", target = "_blank",
                #       #tags$img(src = "SIMCA_LLM.png",alt = "Soft independent modelling by class analogy",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Soft independent modelling by class analogy")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform SIMCA for classification analysis. LLMs provide guidance in creating robust and interpretable models.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnSIMCA_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'timecourse_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/timecourse_LLM", target = "_blank",
                #       #tags$img(src = "timecourse_LLM.png",alt = "Time Course Data Analysis",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Time Course Data Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Analyze time-course data effectively with LLM guidance. This tool is ideal for exploring trends and changes over time.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btntimecourse_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'tsne_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/tsne_LLM", target = "_blank",
                #       #tags$img(src = "tsne_LLM.png",alt = "t-SNE",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("t-SNE")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform t-Distributed Stochastic Neighbor Embedding (t-SNE) for dimensionality reduction and data visualization. LLMs assist in creating interpretable results.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btntsne_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'umap_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/umap_LLM", target = "_blank",
                #       #tags$img(src = "umap_LLM.png",alt = "UMAP",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("UMAP")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Conduct Uniform Manifold Approximation and Projection (UMAP) for dimensionality reduction. LLMs guide users in creating clear visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnumap_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'TumorPurity_LLM.png', width="200px",height = "200px")
                ##tags$a(href = "http://localhost:3838/TumorPurity_LLM", target = "_blank",
                #      #tags$img(src = "TumorPurity_LLM.png",alt = "Estimate Tumor Purity",
                #                width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Estimate Tumor Purity")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Estimate tumor purity levels with LLM support. This tool aids in cancer research by providing insights into tumor composition.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnTumorPurity_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        )
      )
    })
    observeEvent(input$btnConsensusClustering_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnConsensusClustering_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDEPannova_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDEPannova_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDEPlimma_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDEPlimma_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDEPsamr_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDEPsamr_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDEPttest_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDEPttest_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDEPwilcox_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDEPwilcox_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDNB_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDNB_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnFactorAnalysis_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnFactorAnalysis_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnGRA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnGRA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnHCA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnHCA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnKmeans_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnKmeans_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnLackofFit_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnLackofFit_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnLinearRegression_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnLinearRegression_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnLogisticRegression_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnLogisticRegression_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnMfuzz_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnMfuzz_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnNDM_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnNDM_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnOPLSDA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnOPLSDA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnPCA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnPCA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnPCoA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnPCoA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnPLSDA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnPLSDA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnPowerAnalysis_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnPowerAnalysis_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRCS_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRCS_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRDA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRDA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRRHO_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRRHO_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnSIMCA_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnSIMCA_LLM.R', package='WuKong'))
    })

    observeEvent(input$btntimecourse_LLM, {
      rstudioapi::jobRunScript(system.file('home/btntimecourse_LLM.R', package='WuKong'))
    })

    observeEvent(input$btntsne_LLM, {
      rstudioapi::jobRunScript(system.file('home/btntsne_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnTumorPurity_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnTumorPurity_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnumap_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnumap_LLM.R', package='WuKong'))
    })
    ##
    output$functionalgallery<-renderUI({
      div(
        style="margin-top:60px;",
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Celltype_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Celltype_LLM", target = "_blank",
                #tags$img(src = "Celltype_LLM.png",alt = "Cell Type Annotation", width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Cell Type Annotation")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Annotate and classify cell types effectively using LLMs. This tool is particularly useful for biological and medical research, ensuring accurate and efficient cell-type identification.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnCelltype_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ExploreGO_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ExploreGO_LLM", target = "_blank",
                #tags$img(src = "ExploreGO_LLM.png",alt = "Exploring Gene/Protein Functions Based On GO Database",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Exploring Gene/Protein Functions Based On GO Database")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Explore gene and protein functions using the Gene Ontology (GO) database. LLMs provide insights and aid in functional annotation and analysis.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnExploreGO_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'GOenrich_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/GOenrich_LLM", target = "_blank",
                #tags$img(src = "GOenrich_LLM.png",alt = "GO Enrichment Analysis",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("GO Enrichment Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Gene Ontology (GO) enrichment analysis with the guidance of LLMs. This tool identifies significantly enriched biological processes, molecular functions, and cellular components.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnGOenrich_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'KEGGenrich_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/KEGGenrich_LLM", target = "_blank",
                #tags$img(src = "KEGGenrich_LLM.png",alt = "KEGG Enrichment Analysis",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("KEGG Enrichment Analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Kyoto Encyclopedia of Genes and Genomes (KEGG) enrichment analysis with LLM guidance. This tool identifies enriched pathways and biological processes.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnKEGGenrich_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'gseaGO_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/KEGGenrich_LLM", target = "_blank",
                #tags$img(src = "KEGGenrich_LLM.png",alt = "KEGG Enrichment Analysis",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Gene Set Enrichment Analysis of Gene Ontology")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Gene Set Enrichment Analysis of Gene Ontology (GO) with LLM guidance.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btngseaGO_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'gseaKEGG_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/KEGGenrich_LLM", target = "_blank",
                #tags$img(src = "KEGGenrich_LLM.png",alt = "KEGG Enrichment Analysis",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Gene Set Enrichment Analysis of KEGG")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform Gene Set Enrichment Analysis of Kyoto Encyclopedia of Genes and Genomes (KEGG) with LLM guidance.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btngseaKEGG_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        )
      )
    })
    observeEvent(input$btnCelltype_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnCelltype_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnExploreGO_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnExploreGO_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnGOenrich_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnGOenrich_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnKEGGenrich_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnKEGGenrich_LLM.R', package='WuKong'))
    })
    observeEvent(input$btngseaGO_LLM, {
      rstudioapi::jobRunScript(system.file('home/btngseaGO_LLM.R', package='WuKong'))
    })
    observeEvent(input$btngseaKEGG_LLM, {
      rstudioapi::jobRunScript(system.file('home/btngseaKEGG_LLM.R', package='WuKong'))
    })
    ##
    output$visualizationgallery<-renderUI({
      div(
        style="margin-top:60px;",
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Barplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Barplot_LLM", target = "_blank",
                #tags$img(src = "Barplot_LLM.png",alt = "Barplot", width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Barplot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create and customize bar plots with the help of large language models. This tool simplifies the visualization of categorical data, making it easier to analyze patterns and trends.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnBarplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'BarPointplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/BarPointplot_LLM", target = "_blank",
                #tags$img(src = "BarPointplot_LLM.png",alt = "Bar and Point Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Bar and Point Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("This feature combines bar and point plots to provide a comprehensive visualization of data. It uses LLMs to guide users in creating detailed and informative plots that highlight key data points.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnBarPointplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'BoxPointplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/BoxPointplot_LLM", target = "_blank",
                #tags$img(src = "BoxPointplot_LLM.png",alt = "Box and Point Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Box and Point Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate box and point plots effortlessly with the support of LLMs. This tool is ideal for visualizing distributions and comparing individual data points within a dataset.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnBoxPointplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ClusterCorNetwork_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ClusterCorNetwork_LLM", target = "_blank",
                #tags$img(src = "ClusterCorNetwork_LLM.png",alt = "Clustering using Correlation Network",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Clustering using Correlation Network")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Perform clustering analysis using correlation networks with the guidance of LLMs. This method helps uncover relationships and groupings within complex datasets.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnClusterCorNetwork_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ContourPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ContourPlot_LLM", target = "_blank",
                #tags$img(src = "ContourPlot_LLM.png",alt = "Contour Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Contour Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create contour plots to visualize three-dimensional data in two dimensions. LLMs assist in generating clear and informative plots for advanced data analysis.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnContourPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'CorPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/CorPlot_LLM", target = "_blank",
                #tags$img(src = "CorPlot_LLM.png",alt = "Correlation Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Correlation Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Visualize relationships between variables using correlation plots. LLMs simplify the process, ensuring accurate representation and interpretation of data correlations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnCorPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'CorrelationNetwork_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/CorrelationNetwork_LLM", target = "_blank",
                #tags$img(src = "CorrelationNetwork_LLM.png",alt = "Correlation Network Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Correlation Network Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Construct correlation network plots with ease using LLMs. This tool helps visualize complex relationships and interactions between variables in a network format.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnCorrelationNetwork_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'CrossErrorbarplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/CrossErrorbarplot_LLM", target = "_blank",
                #tags$img(src = "CrossErrorbarplot_LLM.png",alt = "Cross Error Bar Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Cross Error Bar Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Design cross error bar plots to represent data variability. LLMs provide guidance in creating precise and visually appealing plots for statistical analysis.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnCrossErrorbarplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Dendrogram_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Dendrogram_LLM", target = "_blank",
                #tags$img(src = "Dendrogram_LLM.png",alt = "Dendrogram",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Dendrogram")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate dendrograms for hierarchical clustering analysis. LLMs assist in creating detailed and interpretable tree-like diagrams for data classification.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDendrogram_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'DivergingBarplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/DivergingBarplot_LLM", target = "_blank",
                #tags$img(src = "DivergingBarplot_LLM.png",alt = "Diverging Bar Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Diverging Bar Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create diverging bar plots to represent data deviations. LLMs simplify the process, enabling clear visualization of positive and negative trends.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnDivergingBarplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'FunnelPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/FunnelPlot_LLM", target = "_blank",
                #tags$img(src = "FunnelPlot_LLM.png",alt = "Funnel Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Funnel Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create funnel plots to assess data variability and bias. LLMs assist in generating clear and informative visualizations for meta-analysis.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnFunnelPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ggseqlogo_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ggseqlogo_LLM", target = "_blank",
                #tags$img(src = "ggseqlogo_LLM.png",alt = "Protein/DNA Sequence Logo Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Protein/DNA Sequence Logo Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate sequence logo plots for protein or DNA sequences. LLMs simplify the process, highlighting conserved regions and sequence patterns.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnggseqlogo_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ggtreeDendrogram_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ggtreeDendrogram_LLM", target = "_blank",
                #tags$img(src = "ggtreeDendrogram_LLM.png",alt = "Dendrogram using ggtree Package",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Dendrogram using ggtree Package")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create dendrograms using the ggtree package for advanced hierarchical clustering visualization. LLMs assist in generating detailed and publication-ready dendrograms.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnggtreeDendrogram_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Heatscatter_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Heatscatter_LLM", target = "_blank",
                #tags$img(src = "Heatscatter_LLM.png",alt = "Colored Scatter Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Colored Scatter Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate heatscatter plots that combine scatterplots with color gradients to represent data density. LLMs assist in creating visually appealing and informative plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnHeatscatter_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'HistgramDensity_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/HistgramDensity_LLM", target = "_blank",
                #tags$img(src = "HistgramDensity_LLM.png",alt = "Histgram and Density Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Histgram and Density Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create combined histogram and density plots to visualize data distributions. LLMs provide guidance in customizing and interpreting these plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnHistgramDensity_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'LollipopChart_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/LollipopChart_LLM", target = "_blank",
                #tags$img(src = "LollipopChart_LLM.png",alt = "Lollipop Chart",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Lollipop Chart")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create lollipop charts for visualizing data comparisons. LLMs simplify the process, ensuring clear and effective data representation.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnLollipopChart_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'MarginalPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/MarginalPlot_LLM", target = "_blank",
                #tags$img(src = "MarginalPlot_LLM.png",alt = "Marginal Histogram/Boxplot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Marginal Histogram/Boxplot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate marginal histogram or boxplots to visualize data distributions alongside scatterplots. LLMs provide assistance in creating detailed visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnMarginalPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'NightingalePlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/NightingalePlot_LLM", target = "_blank",
                #tags$img(src = "NightingalePlot_LLM.png",alt = "Nightingale Rose Diagram",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Nightingale Rose Diagram")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create Nightingale Rose Diagrams to visualize circular data distributions. LLMs simplify the process, ensuring accurate and visually appealing plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnNightingalePlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'PairPointPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/PairPointPlot_LLM", target = "_blank",
                #tags$img(src = "PairPointPlot_LLM.png",alt = "Pair Point Line Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Pair Point Line Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create pair point line plots to visualize paired data relationships. LLMs assist in generating clear and interpretable plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnPairPointPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'PiePlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/PiePlot_LLM", target = "_blank",
                #tags$img(src = "PiePlot_LLM.png",alt = "Pie Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Pie Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create pie charts to visualize categorical data distributions. LLMs simplify the customization and interpretation of these plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnPiePlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RadarChart_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/RadarChart_LLM", target = "_blank",
                #tags$img(src = "RadarChart_LLM.png",alt = "Radar Chart",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Radar Chart")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create radar charts to compare multivariate data. LLMs assist in generating informative and visually appealing plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRadarChart_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RainCloud_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/RainCloud_LLM", target = "_blank",
                #tags$img(src = "RainCloud_LLM.png",alt = "Rain Cloud Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Rain Cloud Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate rain cloud plots to visualize data distributions. LLMs guide users in creating combined density and scatter plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRainCloud_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RankPointPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/RankPointPlot_LLM", target = "_blank",
                #tags$img(src = "RankPointPlot_LLM.png",alt = "Rank Point Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Rank Point Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create rank point plots to highlight data rankings. LLMs simplify the process, ensuring clear and effective visualization.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRankPointPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'RidgePlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/RidgePlot_LLM", target = "_blank",
                #tags$img(src = "RidgePlot_LLM.png",alt = "Ridge Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Ridge Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create ridge plots to visualize distributions across multiple categories. LLMs assist in generating aesthetically pleasing and informative plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnRidgePlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ROCplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ROCplot_LLM", target = "_blank",
                #tags$img(src = "ROCplot_LLM.png",alt = "ROC Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("ROC Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate Receiver Operating Characteristic (ROC) plots to evaluate classification model performance. LLMs provide insights for accurate interpretation.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnROCplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'SankeyPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/SankeyPlot_LLM", target = "_blank",
                #tags$img(src = "SankeyPlot_LLM.png",alt = "Sankey Chart",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Sankey Chart")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create Sankey charts to visualize data flows and relationships. LLMs simplify the process, ensuring accurate and engaging visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnSankeyPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'ScatterEllipsePlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/ScatterEllipsePlot_LLM", target = "_blank",
                #tags$img(src = "ScatterEllipsePlot_LLM.png",alt = "Scatter Ellipse Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Scatter Ellipse Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate scatter plots with ellipses to highlight data groupings. LLMs assist in creating detailed and informative visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnScatterEllipsePlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'SurvivalPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/SurvivalPlot_LLM", target = "_blank",
                #tags$img(src = "SurvivalPlot_LLM.png",alt = "Survival analysis",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Survival analysis")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create survival plots to analyze time-to-event data. LLMs simplify the process, ensuring accurate and clear visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnSurvivalPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'TernaryPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/TernaryPlot_LLM", target = "_blank",
                #tags$img(src = "TernaryPlot_LLM.png",alt = "Ternary Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Ternary Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create ternary plots to visualize three-component data. LLMs provide guidance in generating accurate and aesthetically pleasing plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnTernaryPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'UpsetPlot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/UpsetPlot_LLM", target = "_blank",
                #tags$img(src = "UpsetPlot_LLM.png",alt = "UpSet Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("UpSet Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create UpSet plots to visualize intersections between sets. LLMs simplify the process, ensuring accurate and engaging visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnUpsetPlot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Venn_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Venn_LLM", target = "_blank",
                #tags$img(src = "Venn_LLM.png",alt = "Venn Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Venn Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate Venn plots to display overlaps between sets. LLMs assist in creating clear and informative diagrams.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnVenn_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Violinplot_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Violinplot_LLM", target = "_blank",
                #tags$img(src = "Violinplot_LLM.png",alt = "Violin Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Violin Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create violin plots to visualize data distributions. LLMs guide users in generating detailed and interpretable plots.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnViolinplot_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          ),
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'Volcano_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/Volcano_LLM", target = "_blank",
                #tags$img(src = "Volcano_LLM.png",alt = "Volcano Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Volcano Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Generate volcano plots to identify significant changes in data. LLMs simplify the process, ensuring accurate and visually appealing results.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnVolcano_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
              class = "module-box",
              div(
                img(src = 'WorldCloud_LLM.png', width="200px",height = "200px")
                #tags$a(href = "http://localhost:3838/WorldCloud_LLM", target = "_blank",
                #tags$img(src = "WorldCloud_LLM.png",alt = "Word Cloud Plot",
                #width="200px",height = "200px"))
              ),
              div(
                style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                HTML("Word Cloud Plot")
              ),
              div(
                style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;word-wrap: break-word;",
                HTML("Create word clouds to visualize text data. LLMs assist in generating aesthetically pleasing and informative visualizations.")
              ),
              div(
                style = "text-align:center; margin-top:15px;",
                actionButton("btnWorldCloud_LLM", "Start", style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;")
              )
            )
          )
        )
      )
    })
    observeEvent(input$btnBarplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnBarplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnBarPointplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnBarPointplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnBoxPointplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnBoxPointplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnClusterCorNetwork_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnClusterCorNetwork_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnContourPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnContourPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnCorPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnCorPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnCorrelationNetwork_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnCorrelationNetwork_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnCrossErrorbarplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnCrossErrorbarplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDendrogram_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDendrogram_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnDivergingBarplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnDivergingBarplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnFunnelPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnFunnelPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnggseqlogo_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnggseqlogo_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnggtreeDendrogram_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnggtreeDendrogram_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnHeatscatter_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnHeatscatter_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnHistgramDensity_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnHistgramDensity_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnLollipopChart_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnLollipopChart_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnMarginalPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnMarginalPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnNightingalePlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnNightingalePlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnPairPointPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnPairPointPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnPiePlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnPiePlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRadarChart_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRadarChart_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRainCloud_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRainCloud_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRankPointPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRankPointPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnRidgePlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnRidgePlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnROCplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnROCplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnSankeyPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnSankeyPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnScatterEllipsePlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnScatterEllipsePlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnSurvivalPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnSurvivalPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnTernaryPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnTernaryPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnUpsetPlot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnUpsetPlot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnVenn_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnVenn_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnViolinplot_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnViolinplot_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnVolcano_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnVolcano_LLM.R', package='WuKong'))
    })

    observeEvent(input$btnWorldCloud_LLM, {
      rstudioapi::jobRunScript(system.file('home/btnWorldCloud_LLM.R', package='WuKong'))
    })
    ##one-click
    # Define classified modules as a named list
    oneclick_module_categories <- list(
      "I. Data Pre-processing" = c(
        "Coefficient of variation",
        "Logarithm with base 2",
        "Missing value imputation",
        "Normalization"#,
        #"Two tables merging"
      ),
      "II. Statistical Analysis" = c(
        "Consensus clustering",
        "One-way ANOVA",
        "Limma",
        "SAMR",
        "Student's t-Test",
        "Wilcoxon Rank Sum and Signed Rank Tests",
        #"Dynamic Network Biomarkers",
        #"Factor Analysis",
        #"Grey Relational Analysis",
        "HCA",
        "K-means Clustering",
        #"Lack of Fit F-test",
        #"Linear Regression",
        #"Logistic Regression",
        "Mfuzz",
        #"Network Degree Matrix",
        "OPLS-DA",
        "PCA",
        "PCoA",
        "PLS-DA",
        #"Power Analysis",
        #"Restricted Cubic Spline Analysis",
        #"Redundancy Analysis",
        #"Rank Rank Hypergeometric Overlap Analysis",
        #"Soft independent modelling by class analogy",
        #"Time Course Data Analysis",
        "t-SNE",
        "UMAP"#,
        #"Estimate Tumor Purity"
      ),
      "III. Functional Annotation" = c(
        #"Cell Type Annotation",
        "GO Enrichment Analysis",
        "KEGG Enrichment Analysis",
        "Gene Set Enrichment Analysis of GO",
        "Gene Set Enrichment Analysis of KEGG"#,
        #"Exploring Protein Functions",
      ),
      "IV. Data Visualization" = c(
        "Barplot",
        "Bar and Point Plot",
        "Box and Point Plot",
        #"Clustering using Correlation Network",
        #"Contour Plot",
        "Correlation Plot",
        #"Correlation Network Plot",
        #"Cross Error Bar Plot",
        #"Dendrogram",
        #"Diverging Bar Plot",
        #"Funnel Plot",
        #"Protein/DNA Sequence Logo Plot",
        #"Dendrogram using ggtree Package",
        #"Colored Scatter Plot",
        "Histgram and Density Plot",
        #"Lollipop Chart",
        #"Marginal Histogram/Boxplot",
        #"Nightingale Rose Diagram",
        #"Pair Point Line Plot",
        #"Pie Plot",
        #"Radar Chart",
        "Rain Cloud Plot",
        "Rank Point Plot",
        #"Ridge Plot",
        "ROC Plot",
        #"Sankey Chart",
        #"Scatter Ellipse Plot",
        #"Survival Plot",
        #"Ternary Plot",
        #"UpSet Plot",
        #"Venn Plot",
        #"Violin Plot",
        "Volcano Plot"#,
        #"Word Cloud Plot"
      )
    )
    # Flatten all modules for the orderInput source
    all_oneclick_modules <- unlist(oneclick_module_categories, use.names = FALSE)
    # Assign a color for each category
    category_colors <- c(
      "I. Data Pre-processing" = "#e0f7fa",      # light cyan
      "II. Statistical Analysis" = "#fff3e0",     # light orange
      "III. Functional Annotation" = "#e8f5e9",    # light green
      "IV. Data Visualization" = "#f3e5f5"        # light purple
    )

    output$oneclickgallery <- renderUI({
      tags$div(
        style = "margin-top: 70px;",
        tags$head(
          tags$style(HTML("
        #wellpanelid1, #wellpanelid2 {
          background-color: #f5f5f5 !important;
          padding: 15px;
          border-radius: 4px;
        }
        .nav-tabs > li > a {
          font-weight: bold !important;
        }
      "))
        ),
        div(
          mainPanel(
            width=12,
            tabsetPanel(
              tabPanel(
                "Step 1: Upload Data",
                fluidRow(
                  column(
                    4,
                    wellPanel(
                      id="wellpanelid1",
                      tags$div(
                        style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
                        h4("I. Upload Data/Example Data"),
                        radioButtons(
                          "loaddatatype",
                          label = NULL,
                          choices = list("A. Upload" = 1, "B. Load example data"=2),
                          selected = 1,
                          inline = TRUE
                        ),
                        tags$hr(style="border-color: grey80;"),
                        conditionalPanel(
                          condition = "input.loaddatatype==1",
                          radioButtons(
                            "fileType_Input",
                            label = h5("Select File Format:"),
                            choices = list(".xlsx" = 1, ".xls"=2, ".csv/txt" = 3),
                            selected = 1,
                            inline = TRUE
                          ),
                          fileInput('file1', h5('Please import your data file:'),
                                    accept=c('text/csv','text/plain','.xlsx','.xls')),
                          checkboxInput('header', 'Is the first row names?', TRUE),
                          checkboxInput('firstcol', 'Is the first column names?', TRUE),
                          conditionalPanel(condition = "input.fileType_Input==1",
                                           numericInput("xlsxindex",h5("Which Sheet to read?"),value = 1)),
                          conditionalPanel(condition = "input.fileType_Input==2",
                                           numericInput("xlsxindex",h5("Which Sheet to read?"),value = 1)),
                          conditionalPanel(condition = "input.fileType_Input==3",
                                           radioButtons('sep', 'Data Separator (Comma/Semicolon/Tab/Space):',
                                                        c(Comma=',', Semicolon=';', Tab='\t', BlankSpace=' '),
                                                        ',')),
                          tags$hr(style="border-color: grey80;"),
                          h4("Sample information:"),
                          textInput("grnums",h5("1. Group and replicate number:"),value = ""),
                          bsTooltip("grnums",'Type in the group number and replicate number here. Please note, the group number and replicate number are linked with ";", and the replicate number of each group is linked with "-". For example, if you have two groups, each group has three replicates, then you should type in "2;3-3" here. Similarly, if you have 3 groups with 5 replicates in every groups, you should type in "3;5-5-5".',
                                    placement = "right",options = list(container = "body")),
                          textInput("grnames",h5("2. Group names:"),value = ""),
                          bsTooltip("grnames",'Type in the group names of your samples. Please note, the group names are linked with ";". For example, there are two groups, you can type in "Control;Experiment".',
                                    placement = "right",options = list(container = "body"))
                        ),
                        conditionalPanel(
                          condition = "input.loaddatatype==2",
                          downloadButton("loaddatadownload1","Download example expression data",
                                         style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
                          tags$hr(style="border-color: grey80;"),
                          h4("Sample information:"),
                          textInput("examgrnums",h5("1. Group and replicate number:"),value = "2;4-4"),
                          textInput("examgrnames",h5("2. Group names:"),value = "A;B")
                        ),
                        uiOutput("goortspecies")
                      )
                    )
                  ),
                  column(
                    8,
                    wellPanel(
                      id="wellpanelid2",
                      tags$div(
                        style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
                        h4("II. Display Uploaded Data/Example Data"),
                        div(style = "overflow-x: auto;overflow-y: auto;", dataTableOutput("rawdata"))
                      )
                    )
                  )
                )
              ),
              # UI for Step 2: Select Modules
              tabPanel(
                "Step 2: Select Modules",
                tags$style(HTML("
                .step2-scroll-panel {
      max-height: 750px;
      overflow-y: auto;
      padding-right: 16px;
    }
    .module-panel {
      border-radius: 8px;
      margin-bottom: 18px;
      padding: 10px 12px 12px 12px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.07);
    }
    .category-header {
      font-weight: bold;
      font-size: 17px;
      margin-bottom: 6px;
      margin-top: 8px;
      padding-left: 2px;
    }
    .module-btn {
      margin: 3px 5px 3px 0;
      min-width: 180px;
      text-align: left;
      border-radius: 5px;
      border: 1px solid #d1d5db;
      background-color: #fff;
      transition: background 0.2s, color 0.2s;
      font-size: 15px;
    }
    .module-btn.selected {
      background-color: #3b82f6 !important;
      color: #fff !important;
      font-weight: bold;
      border: 2px solid #2563eb;
    }
    .module-btn:hover {
      background-color: #f3f4f6;
      color: #2563eb;
    }
    .selected-list-box {
      min-height: 310px;
      background: #e0e7ef;
      border-radius: 8px;
      padding: 16px 10px 10px 16px;
      margin-bottom: 10px;
    }
  ")),
                div(
                  class = "step2-scroll-panel",
                  fluidRow(
                    column(
                      6,
                      tags$h4("Available Modules"),
                      helpText("Click a module to select or deselect it. Modules are grouped and colored by their function."),
                      # For each category, render a colored panel with modules as buttons
                      lapply(names(oneclick_module_categories), function(cat) {
                        # Assign a specific min-height based on item count
                        item_count <- length(oneclick_module_categories[[cat]])
                        # Heuristic: 44px per button + 16px padding
                        min_height <- paste0(16 + 10 * item_count, "px")
                        div(
                          class = "module-panel",
                          style = sprintf("background-color:%s; border-left: 8px solid %s;",
                                          category_colors[cat], category_colors[cat]),
                          div(class = "category-header", cat),
                          div(
                            id = paste0("module_list_", gsub(" ", "_", tolower(cat))),
                            class = "module-list-box",
                            style = sprintf("min-height:%s;", min_height),
                            uiOutput(paste0("module_btns_", gsub(" ", "_", tolower(cat))))
                          )
                        )
                      })
                    ),
                    column(
                      6,
                      tags$h4("Your Workflow (Selected Modules, in order)"),
                      div(
                        style = "margin-top: 10px; margin-bottom: 12px; text-align: left;",
                        div(
                          style = "display: flex; gap: 20px; align-items: center; padding: 10px 10px;margin-bottom: 10px;",
                          actionButton("example_pipeline1", "Example Pipeline 1", class = "btn-primary",width="150px",title = "This pipeline demonstrates a typical proteomics data analysis workflow. It starts with normalization and log2 transformation, followed by missing value imputation. Differential expression analysis is performed using SAMR, and results are visualized with a volcano plot. Only the differentially expressed proteins are used for PCA, HCA, and subsequent GO/KEGG enrichment analyses, providing a focused exploration of significant biological changes."),
                          actionButton("example_pipeline2", "Example Pipeline 2", class = "btn-primary",width="150px",title = "This workflow follows a similar preprocessing and differential analysis strategy as Pipeline 1 but highlights gene set enrichment approaches. After normalization, log2 transformation, and missing value imputation, SAMR identifies differentially expressed proteins. The pipeline then visualizes results and uses all proteins for Gene Set Enrichment Analysis (GSEA) of both GO and KEGG, offering a broader view of functional enrichment beyond just the significant hits.")
                        )
                      ),
                      div(class = "selected-list-box",
                          uiOutput("selected_module_list")
                      ),
                      div(
                        style = "margin-top: 16px; margin-bottom: 8px;",
                        textAreaInput(
                          "workflow_description",
                          label = "Workflow Description",
                          value = "",
                          width = "100%",
                          height = "290px",
                          placeholder = "Describe your workflow here. Please click the 'Example Pipeline' button to have a check."
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                "Step 3: LLM-assisted Workflow Designer",
                tags$h4("Run Workflow"),
                helpText("Please select a local LLM and click 'Run Workflow' to execute your pipeline. Results will appear below."),
                div(
                  style = "display: flex; gap: 20px; align-items: center; margin-bottom: 10px;",
                  selectInput("llmmodeloneclick", NULL, choices = list_modelsx[[1]], width = "300px"),
                  actionButton("oneclick_run", "Run Workflow", class = "btn-primary",style = "margin-left: 10px;"),
                  downloadButton("oneclick_download_results", "Download Results",
                                 style = "margin-left: 10px;background-color: #3498db;color: white;border: none;padding: 10px 10px;border-radius: 5px;cursor: pointer;font-size: 16px;",
                                 width = '360px')
                ),
                fluidRow(
                  column(
                    6,
                    #div(
                    #  class = "result-container",
                    #  style = "margin-top:20px; min-height:500px; background:#f8fafc;",
                    #  tags$h5("Refined Prompts (LLM-generated R code or instructions)"),
                    #  uiOutput("oneclick_prompts")
                    #)
                    h5("Refined Prompts (LLM-generated R code or instructions)"),
                    uiOutput("oneclick_prompts")
                  ),
                  column(
                    6,
                    div(
                      class = "result-container",
                      style = "margin-top:20px; min-height:500px; background:#f9f9f9;",
                      tags$h5("Analysis Results"),
                      uiOutput("oneclick_results")
                    )
                  )
                )
              )
            )
          )
        )
      )
    })
    ##
    output$goortspecies<-renderUI({
      goort_spedf<-read.csv("uniprot-species.csv",header = T,stringsAsFactors = F)
      goort_spedf_paste<-paste(goort_spedf$Organism.ID,goort_spedf$Organism,sep = "-")
      selectizeInput('speciesx', h5('3. Please choose a species:'), choices =goort_spedf_paste,options = list(maxOptions = 6000))
    })
    examplepeakdatas<-reactive({
      library(writexl)
      dataread<-read.csv("Exampledata1.csv",stringsAsFactors = F,check.names = F,row.names = 1)
      #dataread<-log2(dataread[1:5,])
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
    # Example pipeline modules (in order)
    example_pipeline_modules1 <- c(
      "Normalization",
      "Logarithm with base 2",
      "Missing value imputation",
      "SAMR",
      "Volcano Plot",
      "PCA",
      "HCA",
      "GO Enrichment Analysis"
    )#"Exploring Gene/Protein Functions" "KEGG Enrichment Analysis"
    observeEvent(input$example_pipeline1, {
      selected_modules(example_pipeline_modules1)
    })
    ##
    example_pipeline_modules2 <- c(
      "Normalization",
      "Logarithm with base 2",
      "Missing value imputation",
      "SAMR",
      "Volcano Plot",
      "PCA",
      "HCA",
      "Gene Set Enrichment Analysis of GO",
      "Gene Set Enrichment Analysis of KEGG"
    )#"Exploring Gene/Protein Functions"
    observeEvent(input$example_pipeline2, {
      selected_modules(example_pipeline_modules2)
    })
    # At the top of server function
    workflow_description <- reactiveVal("")
    # Keep workflow_description in sync with the text area
    observeEvent(input$workflow_description, {
      workflow_description(input$workflow_description)
    }, ignoreInit = TRUE)
    # When Example Pipeline is clicked, update modules and description
    observeEvent(input$example_pipeline1, {
      selected_modules(example_pipeline_modules1)
      workflow_description(
        "This workflow includes the following steps:
1. Normalization using the median method for the input data.
2. Log2 transformation.
3. Imputation of missing values.
4. SAMR for differential expression analysis.
5. Visualization of results with a volcano plot.
6. PCA using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5. Do NOT use all proteins.
7. HCA using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5.
8. GO Enrichment Analysis using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5.
"
      )#9. KEGG Enrichment Analysis using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5.
    })
    observeEvent(input$example_pipeline2, {
      selected_modules(example_pipeline_modules2)
      workflow_description(
        "This workflow includes the following steps:
1. Normalization using the Median method for the input data.
2. Log2 transformation.
3. Imputation of missing values.
4. SAMR for differential expression analysis.
5. Visualization of results with a volcano plot.
6. PCA using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5. Do NOT use all proteins.
7. HCA using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5.
8. Gene Set Enrichment Analysis of GO Enrichment Analysis using the whole proteins identified in Step 4.
9. Gene Set Enrichment Analysis of KEGG Enrichment Analysis using the whole proteins identified in Step 4."
      )
    })
    # Sync text area input with workflow_description()
    observe({
      updateTextAreaInput(session, "workflow_description", value = workflow_description())
    })
    # Holds selected modules in order of selection
    selected_modules <- reactiveVal(character())
    # Render module buttons for each category
    observe({
      for (cat in names(oneclick_module_categories)) {
        local({
          catname <- cat
          catid <- gsub(" ", "_", tolower(catname))
          output[[paste0("module_btns_", catid)]] <- renderUI({
            modules <- oneclick_module_categories[[catname]]
            sel <- selected_modules()
            div(
              lapply(modules, function(mod) {
                btnid <- paste0("modbtn_", gsub("[^a-zA-Z0-9]", "_", mod))
                actionButton(
                  btnid, mod,
                  class = paste("module-btn", ifelse(mod %in% sel, "selected", "")),
                  style = "margin-bottom:6px;"
                )
              })
            )
          })
        })
      }
    })

    # Listen for clicks on any module button
    observe({
      for (mod in all_oneclick_modules) {
        local({
          modname <- mod
          btnid <- paste0("modbtn_", gsub("[^a-zA-Z0-9]", "_", modname))
          observeEvent(input[[btnid]], {
            sel <- selected_modules()
            if (modname %in% sel) {
              # Deselect (remove)
              selected_modules(sel[sel != modname])
            } else {
              # Select (add to end)
              selected_modules(c(sel, modname))
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    # Render selected module list (in order)
    output$selected_module_list <- renderUI({
      sel <- selected_modules()
      if (length(sel) == 0) {
        tags$em("No modules selected yet.")
      } else {
        tags$ol(
          lapply(sel, function(mod) tags$li(mod))
        )
      }
    })
    #oneclick_results
    # 3. Show refined prompts in left panel
    #chat_oneclick <- reactiveVal(list())
    oneclick_summary_text("")
    observeEvent(input$oneclick_run, {
      output$oneclick_prompts <- renderUI({
        if(input$loaddatatype==1){
          grnames1<-strsplit(input$grnames,";")[[1]]
          grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
          grnum2<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
          grnames<-rep(grnames1,times=grnum2)
        }else{
          grnames1<-strsplit(input$examgrnames,";")[[1]]
          grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
          grnum2<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
          grnames<-rep(grnames1,times=grnum2)
        }
        inputdata<<-peaksdataout()
        llmmodeloneclickx<<-input$llmmodeloneclick
        speciesx<<-input$speciesx
        WKfuncslist2<<-WKfuncslist1
        #
        # === NEW: Incorporate workflow description ===
        workflow_desc <- input$workflow_description
        # If empty, fallback to the reactiveVal (sync with UI)
        if(is.null(workflow_desc) || workflow_desc == "") {
          workflow_desc <- workflow_description()
        }
        workflow_descx<<-workflow_desc
        #
        selx <<- selected_modules()
        funclistx<-list()
        if(length(selx)>0 & ncol(inputdata)>1){
          for(i in 1:length(selx)){
            funclistx[i]<-WKfuncslist2[selx[i]]
          }
          names(funclistx)<-selx
          explorego2<-which(selx=="Logarithm with base 2")
          if(length(explorego2)>0){
            funclistx[[explorego2]]<-"logdata<-log2(inputdata)"
            names(funclistx)[explorego2]<-"Logarithm with base 2"
          }
          funclistx1<-funclistx
          names(funclistx1)<-paste0("Step ",1:length(names(funclistx)),": ",names(funclistx))
          # Compose the prompt
          oneclick_user_message1 <- "
Act as a senior bioinformatician, based on below STRICT RULES and reference codes, Your task is to refine the reference codes step by step in strict accordance with the requirements outlined in the WORKFLOW DESCRIPTION and output the final refined R codes in a single merged R code block marked with ```R and ```.

**STRICT RULES:**
1. **Input Objects:** You may only use these variables as inputs:
   - `inputdata`: the primary input data matrix or dataframe
   - `grnames1`: vector of group names
   - `grnum1`: total number of groups
   - `grnum2`: vector with the number of replicates per group
   - `grnames`: vector of group names, each repeated according to its replicate count
   - `speciesx`: sample species id or name
2. **Stepwise Chaining:**
   - **Each step must clearly specify its input and output object names.**
   - **Each output object from a step must serve as the input for the next step.**
   - **No intermediate manual changes are allowed.**
3. **Code Execution:**
   - All codes must be executable as a single, ordered script without manual intervention.
   - All necessary `library()` calls for each step must be included as below REFERENCE CODES (even if repeated).
   - Do **not** load libraries for basic R functions (e.g., `sweep`, `log2`).
4. **Logic:**
   - **Do not change the underlying logic or calculations as below WORKFLOW DESCRIPTION.**
5. **Output:**
   - Output only the final, refined R code (no explanations inline with code).
   - Before the code, provide concise, bullet-point explanations (max 1–2 lines each) for:
     - `inputdata`
     - `grnames1`
     - `grnum1`
     - `grnum2`
     - `grnames`
     - `speciesx`

WORKFLOW DESCRIPTION:
"
          # Add workflow description if present
          if(!is.null(workflow_desc) && nchar(trimws(workflow_desc)) > 0){
            oneclick_user_message1 <- paste0(oneclick_user_message1, workflow_desc, "\n\n")
          } else {
            oneclick_user_message1 <- paste0(oneclick_user_message1, "[No workflow description provided]\n\n")
          }
          oneclick_user_message1xx <<- paste0(oneclick_user_message1, "OUTPUT FORMAT:\n# Step 1: [Short Step Name]\n[refined R code]\n# Step 2: [Short Step Name]\n[refined R code]\n...\n\nREFERENCE CODES:\n")

          # Dynamically combine all steps from funclistx1
          steps_formatted <- sapply(seq_along(funclistx1), function(i) {
            step_name <- names(funclistx1)[i]
            step_code <- funclistx1[[i]]
            paste0("# ", step_name, "\n", step_code, "\n")
          })
          oneclick_user_message2 <<- paste0(
            oneclick_user_message1xx, "\n\n",
            paste(steps_formatted, collapse = "\n")
          )
          #chat_oneclick(c(chat_oneclick(), list(list(role = "user", content = oneclick_user_message2))))
          oneclick_messagesx <<- list(list(role = "user", content = oneclick_user_message2))#chat_oneclick()
          oneclick_response_message <<- chat(llmmodeloneclickx, oneclick_messagesx, output = "text",
                                             keep_alive = "0m",temperature = 0, num_predict = 16384,
                                             num_ctx = 8192,stream=TRUE)
          aceEditor(outputId = "rcode",value = oneclick_response_message,mode = "r",theme = "chrome",
                    readOnly = TRUE,height = "500px",fontSize = 14)
        }else{
          oneclick_response_message<<-""
          #HTML(paste("<pre> Please upload data in Step 1: Upload Data or select one module at least in Step 2: Select Modules!</pre>"))
          aceEditor(outputId = "rcode",value = "Warning: Please upload data in <Step 1: Upload Data> or \nselect one module at least in <Step 2: Select Modules>!",
                    mode = "r",theme = "chrome",
                    readOnly = TRUE,height = "500px",fontSize = 14)
        }
      })
      #
      oneclick_step_results <- reactiveVal(list())
      oneclick_step_titles <- reactiveVal(list())
      output$oneclick_results <- renderUI({
        if (input$loaddatatype == 1) {
          grnames1 <- strsplit(input$grnames, ";")[[1]]
          grnum1 <- as.numeric(strsplit(input$grnums, ";")[[1]][1])
          grnum2 <- as.numeric(strsplit(strsplit(input$grnums, ";")[[1]][2], "-")[[1]])
          grnames <- rep(grnames1, times = grnum2)
        } else {
          grnames1 <- strsplit(input$examgrnames, ";")[[1]]
          grnum1 <- as.numeric(strsplit(input$examgrnums, ";")[[1]][1])
          grnum2 <- as.numeric(strsplit(strsplit(input$examgrnums, ";")[[1]][2], "-")[[1]])
          grnames <- rep(grnames1, times = grnum2)
        }
        grnames1 <<- grnames1
        grnum1 <<- grnum1
        grnum2 <<- grnum2
        grnames <<- grnames
        inputdata <<- peaksdataout()
        speciesx <<- input$speciesx

        if (oneclick_response_message != "") {
          matches1 <- regexpr("```(R|r)(.*?)```", oneclick_response_message)
          extracted <- regmatches(oneclick_response_message, matches1)
          r_code <- gsub("```", "", gsub("^```(R|r)", "", extracted))
          code_text <- trimws(r_code)

          # Parse code into steps by '# Step n:' comments
          step_locs <- gregexpr("# Step [0-9]+:", code_text)[[1]]
          n_steps <- length(step_locs)
          if (n_steps == 1 && step_locs[1] == -1) {
            step_locs <- 1
            n_steps <- 1
          }
          step_splits <- character(n_steps)
          for (i in seq_along(step_locs)) {
            start_pos <- step_locs[i]
            end_pos <- if (i < n_steps) step_locs[i + 1] - 1 else nchar(code_text)
            step_splits[i] <- substr(code_text, start_pos, end_pos)
          }
          step_splitsx <<- step_splits
          results_ui <- list()
          envx <- new.env(parent = globalenv())
          # Pre-populate required objects
          envx$inputdata <- inputdata
          envx$grnames1 <- grnames1
          envx$grnum1 <- grnum1
          envx$grnum2 <- grnum2
          envx$grnames <- grnames
          envx$speciesx <- speciesx
          step_results <- list()
          step_titles <- list()
          step_prints <- list() # for LLM summary

          for (i in seq_along(step_splits)) {
            step_code <- step_splits[i]
            step_code1 <- unlist(strsplit(step_code, "\n"))
            step_title <- grep("^# Step [0-9]+:", step_code1, value = TRUE)
            lines_no_step <- step_code1[!grepl("^# Step [0-9]+:", step_code1)]
            code_body <- paste(lines_no_step, collapse = "\n")

            local({
              my_i <- i
              my_title <- if (length(step_title) > 0) step_title else paste("Step", my_i)
              my_code <- code_body

              result <- tryCatch({
                eval(parse(text = my_code), envir = envx)
              }, error = function(e) {
                structure(list(error = TRUE, message = e$message), class = "oneclick_error")
              })
              step_results[[i]] <<- result
              step_titles[[i]] <<- my_title

              # Dynamically allocate output IDs for UI
              outputId_plot <- paste0("oneclick_plot_", my_i)
              outputId_table <- paste0("oneclick_table_", my_i)
              outputId_text <- paste0("oneclick_text_", my_i)

              if (inherits(result, "ggplot") | inherits(result, "pheatmap")) {
                output[[outputId_plot]] <- renderPlot({ result })
                results_ui[[my_i]] <<- tagList(
                  tags$h5(my_title),
                  plotOutput(outputId_plot, height = "400px")
                )
              } else if (is.data.frame(result) || is.matrix(result)) {
                output[[outputId_table]] <- renderDataTable({
                  datatable(result, options = list(pageLength = 5, scrollX = TRUE))
                })
                results_ui[[my_i]] <<- tagList(
                  tags$h5(my_title),
                  dataTableOutput(outputId_table)
                )
              } else if (inherits(result, "oneclick_error")) {
                results_ui[[my_i]] <<- tagList(
                  tags$h5(my_title),
                  tags$pre(style = "color:red;", paste("Error:", result$message))
                )
              } else if (!is.null(result)) {
                output[[outputId_text]] <- renderUI({
                  tags$pre(capture.output(print(result)))
                })
                results_ui[[my_i]] <<- tagList(
                  tags$h5(my_title),
                  uiOutput(outputId_text)
                )
              }
            }) # end local
          }
          envxx <<- envx
          oneclick_step_results(step_results)
          oneclick_step_titles(step_titles)
          step_prints<<-step_prints

          step_resultsxx<<-oneclick_step_results()
          step_titlesxx<<-oneclick_step_titles()
          step_resnames<-unlist(step_titlesxx)
          step_resnames1<-grep("Limma|SAMR|t-test|Wilcoxon",step_resnames,ignore.case = T)
          if (length(step_resnames1)>0) {
            resultdep<<-step_resultsxx[[step_resnames1]]
            resultx1<-resultdep[abs(resultdep$Fold.Change)>log2(1.5) & resultdep$p.adjust<0.05,]
            dep_prints <<- paste(rownames(resultx1), collapse = ", ")
          }else{
            dep_prints<<-""
          }

          # ====== LLM DETAILED STEP-BY-STEP SUMMARY GENERATION ======
          if (is.null(oneclick_summary_text()) || nchar(oneclick_summary_text()) < 50) {
            # Compose LLM prompt that asks for a scientific, step-by-step interpretation
            llmmodeloneclickx <- input$llmmodeloneclick
            summary_prompt1 <- paste0(
              "You are a senior bioinformatician. The following is an R code-based step-by-step proteomics analysis, including all code, comments, and outputs.\n\n",
              "Your tasks are:\n",
              "1. Write a clear, detailed, and scientifically rigorous summary (300–500 words) for the Results section of a research paper. This summary should:\n",
              "   - Clearly describe the overall analysis workflow, step by step, based on the comments and outputs (do NOT repeat or paraphrase the code itself).\n",
              "   - For each step, interpret the main statistical and biological findings, focusing on their significance and implications.\n",
              "2. Ensure your summary is fluent, logical, and stepwise, reflecting the analysis progression.\n",
              "3. Structure your output with clear headings for each major section (e.g., 'Step-by-step Workflow Summary').\n",
              "Step-by-step analysis:\n\n",
              oneclick_response_message
            )
            summary_response1 <- chat(
              llmmodeloneclickx,
              list(list(role = "user", content = summary_prompt1)),
              output = "text",
              keep_alive = "0m",
              temperature = 0.2,
              num_ctx = 16384,
              num_predict = 8192,
              stream = TRUE
            )
            #
            # Function to check if string is UniProt ID format
            is_uniprot_id <- function(id) {
              grepl("^[OPQ][0-9][A-Z0-9]{3}[0-9]$|^[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}$", id)
            }

            if(dep_prints!=""){
              # Split dep_prints into vector
              dep_vector <- unlist(strsplit(dep_prints, ",\\s+"))
              # Check if majority are UniProt IDs
              is_id_flags <- sapply(dep_vector, is_uniprot_id)
              if (mean(is_id_flags) > 0.5) {
                # They're likely UniProt IDs – ask LLM to translate to protein names and classify
                #list_for_llm <- paste(dep_vector, collapse = ", ")
                library(UniProt.ws)
                speciesx1<-as.numeric(strsplit(speciesx,"-")[[1]][1])
                speciesx2<-UniProt.ws(taxId=speciesx1)
                list_for_llm1 <- select(x = speciesx2,keys = dep_vector,to = "Gene_Name")
                list_for_llm <- paste(list_for_llm1[[2]], collapse = ", ")
                dep_input <- list_for_llm
              } else {
                # They are likely protein names – use as-is
                dep_input <- paste(dep_vector, collapse = ", ")
              }
              # Construct full LLM prompt
              summary_prompt2 <- paste0(
                "You are given a list of differentially expressed proteins (gene names) identified from statistical analyses (e.g., SAMR, Limma, t-test, Wilcoxon).\n",
                "Your tasks are:\n",
                "1. Assign each gene name to a biological function category (e.g., Signaling, Metabolism, Immune Response, Structural Proteins, Transporters, etc.).\n",
                "2. For each functional category, select up to the top 10 core gene names that are most representative or central to that category. If fewer than 10 genes fit, list all available.\n",
                "3. Present your results in a markdown table with columns:\n",
                "   - Functional Category\n",
                "   - Top 10 Core Gene Names\n",
                "   - Biological Role & Potential Implications\n",
                "4. If a gene cannot be confidently assigned to a category, place it in 'Unknown/Other'.\n",
                "5. After the table, provide a brief interpretation for each functional category, explaining the biological significance of the observed changes in these genes.\n\n",
                "Here is the list of differentially expressed proteins (DEPs):\n",
                dep_input, "\n"
              )
              summary_response2 <- chat(
                llmmodeloneclickx,
                list(list(role = "user", content = summary_prompt2)),
                output = "text",
                keep_alive = "0m",
                temperature = 0.2,
                num_ctx = 16384,
                num_predict = 8192,
                stream = TRUE
              )
              summary_response<-paste0(summary_response1,"\n",summary_response2)
            }else{
              summary_response<-summary_response1
            }
            oneclick_summary_text(summary_response)
          } else {
            summary_response <- oneclick_summary_text()
          }

          summary_responsex<<-summary_response
          # Convert markdown summary to HTML
          summary_html <- HTML(
            commonmark::markdown_html(summary_response, hardbreaks = TRUE)
          )

          summary_ui <- tags$div(
            style = "background: #f8fafc; border-radius: 10px; margin-top: 24px; padding: 24px 32px; box-shadow: 0 4px 16px rgba(0,0,0,0.08);",
            tags$h4("LLM-Generated Scientific Summary", style = "color:#2563eb; margin-bottom: 18px;font-size: 30px;font-weight: bold;"),
            tags$div(
              summary_html,
              style = "
      font-size: 16px;
      color: #374151;
      margin-bottom: 0;
      white-space: normal;
    "
            ),
            tags$style(HTML("
    /* Style markdown tables */
    table {
      width: 100%;
      border-collapse: collapse;
      margin-bottom: 20px;
      background: #fff;
      border-radius: 6px;
      overflow: hidden;
      box-shadow: 0 1px 2px rgba(0,0,0,0.03);
    }
    th, td {
      border: 1px solid #e5e7eb;
      padding: 10px 12px;
      text-align: left;
      vertical-align: top;
      font-size: 15px;
    }
    th {
      background: #eff6ff;
      color: #2563eb;
      font-weight: 600;
    }
    tr:nth-child(even) td {
      background: #f1f5f9;
    }
    /* Style headings */
    h2, h3, h4 {
      color: #2563eb;
      margin-top: 18px;
      margin-bottom: 10px;
    }
    /* Style lists */
    ul, ol {
      margin-left: 18px;
      margin-bottom: 12px;
    }
  "))
          )

          do.call(tagList, c(list(summary_ui),results_ui))
        } else {
          HTML("<pre> There is nothing here. Please upload data in Step 1: Upload Data or select at least one module in Step 2: Select Modules!</pre>")
        }
      })

      output$oneclick_download_results <- downloadHandler(
        filename = function() {
          paste0("WuKong_OneClick_Results_", Sys.Date(), ".zip")
        },
        content = function(file) {
          tmpdir <- tempdir()
          oldwd <- setwd(tmpdir)    # 切换到临时目录
          on.exit(setwd(oldwd))     # 保证后续切回原目录

          results <- oneclick_step_results()
          titles <- oneclick_step_titles()
          resultsx<<-results
          titlesx<<-titles
          file_list <- c()
          if (length(results) != 0) {
            for (i in seq_along(results)) {
              res <- results[[i]]
              # 文件名仅用字母数字下划线
              title <- gsub("[^a-zA-Z0-9]", "_", titles[[i]])
              if (is.data.frame(res) || is.matrix(res)) {
                fname <- paste0("Step", i, "_", title, ".csv")
                write.csv(res, fname, row.names = TRUE)
                file_list <- c(file_list, fname)
              } else if (inherits(res, "ggplot") || inherits(res, "pheatmap") || inherits(res, "recordedplot")) {
                fname <- paste0("Step", i, "_", title, ".pdf")
                pdf(fname, width = 8, height = 6)
                print(res)
                dev.off()
                file_list <- c(file_list, fname)
              } else if (is.character(res) || is.numeric(res) || is.logical(res) || is.list(res)) {
                fname <- paste0("Step", i, "_", title, ".txt")
                capture.output(print(res), file = fname)
                file_list <- c(file_list, fname)
              } else if (inherits(res, "oneclick_error")) {
                fname <- paste0("Step", i, "_", title, "_error.txt")
                writeLines(paste("Error:", res$message), fname)
                file_list <- c(file_list, fname)
              }
            }
          } else {
            notefile <- "README.txt"
            writeLines("No results found. Please run the workflow first.", notefile)
            file_list <- c(file_list, notefile)
          }
          # 只用文件名，不用全路径
          zip::zip(zipfile = file, files = file_list)
        }
      )

    })



    ##
    #chatConversation
    observeEvent(input$example_clicked, {
      updateTextInput(session, "user_input", value = "I want to do PCA analysis and How could I do in this platform?")# Refer to inner codes. and point shapes are circle
    })
    ######
    chat_history <- reactiveVal(list())
    code_result <- reactiveVal(NULL)
    typetimes<-1
    observeEvent(input$send,{
      #if(typetimes==1){
      #  user_message<-""
      #  typetimes<<-typetimes+1
      #}else{
      #  user_message<-input$user_input
      #}
      #wkfuncdf<<-read.xlsx("WKfunctions.xlsx",sheet = 2)
      #user_messagexx1<<-stringr::str_c(readLines("wkfuncdf.txt"), collapse = "\n")
      #user_messagexx2<-paste0("Below contents have two columns, separated by '\t', the first one is module name, the other is module description. If users ask 'How to do in this platform' or any similar questions, you must answer based on below contents, otherwise, you can ignore below contents.\n",user_messagexx1)
      #user_messagexx2<-paste0("Please learn from below contents and if users ask similar questions, answer based on below contents:\n",user_messagexx1)
      #chat_history(c(chat_history(), list(list(role = "user", content = user_messagexx2))))
      #user_messagexx3<<-chat_history()
      #llmmodelx<<-input$llmmodel
      #response_messagex1 <- chat(llmmodelx, user_messagexx3, output = "text",
      #                         keep_alive = "0m",temperature = 0.2, num_predict = 2048,
      #                         stream=TRUE)
      #chat_history(c(chat_history(), list(list(role = "assistant", content = response_messagex1))))
      ##
      user_message<-input$user_input
      user_messagex <<- user_message
      chat_history(c(chat_history(), list(list(role = "user", content = user_message))))
      messagesx <<- chat_history()
      llmmodelx<<-input$llmmodel
      response_message <- chat(llmmodelx, messagesx, output = "text",#llama3 llama3.1:405b llama3:70b gemma2:27b
                               keep_alive = "0m",temperature = 0.2, num_predict = 2048,
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
)

shinyApp(ui = ui, server = server)
