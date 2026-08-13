library(shiny)
library(shinyBS)
library(shinyjqui)
library(openxlsx)
library(gdata)
library(DT)
# library(tidyllm)
library(ollamar)
library(markdown)
library(shinyAce)
library(zip)
library(commonmark)
library(preprocessCore)

# ===== New libraries for optimized Conversation and WuKongmini =====
library(shinyjs)
library(httr2)
library(jsonlite)
library(htmltools)
library(rmarkdown)
library(knitr)

# ============================================================
# Global helpers
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_filename <- function(x) {
  x <- gsub("[^a-zA-Z0-9一-龥_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  substr(x, 1, 120)
}

extract_r_code <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")

  pattern <- "```(?:r|R)?\\s*([\\s\\S]*?)```"
  m <- regmatches(text, regexpr(pattern, text, perl = TRUE))

  if (length(m) == 0 || identical(m, character(0)) || m == "") {
    return(trimws(text))
  }

  code <- sub("^```(?:r|R)?\\s*", "", m, perl = TRUE)
  code <- sub("```$", "", code, perl = TRUE)
  trimws(code)
}

is_ggplot_object <- function(x) {
  inherits(x, "ggplot")
}

is_pheatmap_object <- function(x) {
  inherits(x, "pheatmap")
}

summarize_result_object <- function(obj, max_rows = 8, max_cols = 8) {
  if (inherits(obj, "oneclick_error")) {
    return(paste0("ERROR: ", obj$message))
  }

  if (is.data.frame(obj) || is.matrix(obj)) {
    df <- as.data.frame(obj)
    nr <- nrow(df)
    nc <- ncol(df)

    if (nr == 0 || nc == 0) {
      return(paste0("Object type: table/data.frame; dimensions: ", nr, " x ", nc))
    }

    df_small <- df[
      seq_len(min(nr, max_rows)),
      seq_len(min(nc, max_cols)),
      drop = FALSE
    ]

    return(paste(
      paste0("Object type: table/data.frame; dimensions: ", nr, " x ", nc),
      paste(capture.output(print(df_small)), collapse = "\n"),
      sep = "\n"
    ))
  }

  if (is_ggplot_object(obj)) {
    return("Object type: ggplot figure. A plot was generated successfully.")
  }

  if (is_pheatmap_object(obj)) {
    return("Object type: pheatmap figure. A heatmap was generated successfully.")
  }

  if (inherits(obj, "recordedplot")) {
    return("Object type: recordedplot figure. A base R plot was generated successfully.")
  }

  if (is.character(obj) || is.numeric(obj) || is.logical(obj)) {
    return(paste(capture.output(print(obj)), collapse = "\n"))
  }

  if (is.list(obj)) {
    return(paste(capture.output(str(obj, max.level = 2)), collapse = "\n"))
  }

  paste(capture.output(print(obj)), collapse = "\n")
}

# ============================================================
# LLM model registry and callers:
# DeepSeek API, Moonshot/Kimi API, OpenAI Responses API, Local Ollama
# ============================================================

llm_model_registry <- data.frame(
  provider = c(
    rep("deepseek", 2),
    rep("kimi", 3),
    rep("openai", 4),
    rep("ollama", 4)
  ),
  provider_label = c(
    rep("DeepSeek API", 2),
    rep("Kimi / Moonshot API", 3),
    rep("OpenAI / ChatGPT API", 4),
    rep("Local Ollama", 4)
  ),
  model = c(
    "deepseek-v4-pro",
    "deepseek-v4-flash",

    "kimi-k3",
    "kimi-k2-0905-preview",
    "kimi-latest",

    "gpt-5.6-luna",
    "gpt-5.6",
    "gpt-5.5",
    "gpt-4.1",

    "qwen3.6:35b",
    "gemma3:27b",
    "llama3.3:70b",
    "qwen2.5:72b"
  ),
  label = c(
    "DeepSeek V4 Pro",
    "DeepSeek V4 Flash",

    "Kimi K3",
    "Kimi K2 0905 Preview",
    "Kimi Latest",

    "GPT-5.6 Luna",
    "GPT-5.6",
    "GPT-5.5",
    "GPT-4.1",

    "Qwen 3.6 35B",
    "Gemma 3 27B",
    "Llama 3.3 70B",
    "Qwen 2.5 72B"
  ),
  max_context = c(
    128000,
    128000,

    128000,
    128000,
    128000,

    256000,
    256000,
    256000,
    128000,

    131072,
    131072,
    131072,
    131072
  ),
  default_num_predict = c(
    8192,
    8192,

    8192,
    8192,
    8192,

    16384,
    16384,
    16384,
    8192,

    8192,
    8192,
    8192,
    8192
  ),
  stringsAsFactors = FALSE
)

get_provider_models <- function(provider) {
  llm_model_registry[llm_model_registry$provider == provider, , drop = FALSE]
}

get_model_info <- function(provider, model) {
  df <- llm_model_registry[
    llm_model_registry$provider == provider &
      llm_model_registry$model == model,
    ,
    drop = FALSE
  ]

  if (nrow(df) == 0) {
    provider_label <- switch(
      provider,
      deepseek = "DeepSeek API",
      kimi = "Kimi / Moonshot API",
      openai = "OpenAI / ChatGPT API",
      ollama = "Local Ollama",
      provider
    )

    df <- data.frame(
      provider = provider,
      provider_label = provider_label,
      model = model,
      label = paste0(model, " (custom)"),
      max_context = ifelse(provider == "openai", 128000, 131072),
      default_num_predict = 8192,
      stringsAsFactors = FALSE
    )
  }

  df[1, , drop = FALSE]
}

get_model_max_context <- function(provider, model) {
  info <- get_model_info(provider, model)
  as.integer(info$max_context[1] %||% 128000)
}

get_model_default_num_predict <- function(provider, model) {
  info <- get_model_info(provider, model)
  as.integer(info$default_num_predict[1] %||% 8192)
}

default_ollama_models <- get_provider_models("ollama")$model
deepseek_models <- get_provider_models("deepseek")$model
kimi_models <- get_provider_models("kimi")$model
openai_models <- get_provider_models("openai")$model

# Keep the original object name for compatibility with existing code
list_modelsx <- default_ollama_models

# ============================================================
# Ollama helpers
# ============================================================

get_ollama_selected_model <- function(
    ollama_model_mode = "registered",
    ollama_model = "qwen3.6:35b",
    ollama_custom_model = ""
) {
  if (identical(ollama_model_mode, "custom")) {
    custom_model <- trimws(ollama_custom_model %||% "")

    if (!nzchar(custom_model)) {
      stop(
        paste(
          "Custom Ollama model name is empty.",
          "Please input a valid local Ollama model name,",
          "such as qwen2.5:32b, llama3.1:8b, or deepseek-r1:70b."
        )
      )
    }

    return(custom_model)
  }

  ollama_model %||% "qwen3.6:35b"
}

get_local_ollama_models <- function() {
  res <- tryCatch({
    resp <- request("http://localhost:11434/api/tags") |>
      req_timeout(10) |>
      req_perform()

    obj <- resp_body_json(resp, simplifyVector = FALSE)

    if (!is.null(obj$models) && length(obj$models) > 0) {
      models <- vapply(
        obj$models,
        function(x) x$name %||% "",
        character(1)
      )

      models <- models[nzchar(models)]
      unique(models)
    } else {
      character()
    }
  }, error = function(e) {
    character()
  })

  res
}

get_ollama_model_choices <- function() {
  registered <- get_provider_models("ollama")$model
  local_models <- get_local_ollama_models()
  all_models <- unique(c(registered, local_models))

  if (length(all_models) == 0) {
    all_models <- registered
  }

  setNames(all_models, all_models)
}

messages_to_openai_input <- function(messages) {
  if (is.null(messages) || length(messages) == 0) {
    return("")
  }

  paste(
    vapply(messages, function(m) {
      role <- m$role %||% "user"
      content <- m$content %||% ""
      paste0(toupper(role), ":\n", content)
    }, character(1)),
    collapse = "\n\n"
  )
}

extract_openai_responses_text <- function(obj) {
  if (!is.null(obj$output_text)) {
    return(obj$output_text %||% "")
  }

  if (!is.null(obj$output) && length(obj$output) > 0) {
    txt <- c()

    for (item in obj$output) {
      if (!is.null(item$content) && length(item$content) > 0) {
        for (cc in item$content) {
          if (!is.null(cc$text)) {
            txt <- c(txt, cc$text)
          }
        }
      }
    }

    if (length(txt) > 0) {
      return(paste(txt, collapse = "\n"))
    }
  }

  ""
}

# ============================================================
# LLM callers
# ============================================================

call_ollama <- function(
    model,
    messages,
    temperature = 0.2,
    num_predict = NULL,
    num_ctx = NULL
) {
  max_ctx <- get_model_max_context("ollama", model)

  if (is.null(num_ctx) || is.na(num_ctx)) {
    num_ctx <- max_ctx
  }

  if (is.null(num_predict) || is.na(num_predict)) {
    num_predict <- get_model_default_num_predict("ollama", model)
  }

  req_body <- list(
    model = model,
    messages = messages,
    stream = FALSE,
    options = list(
      temperature = temperature,
      num_predict = num_predict,
      num_ctx = num_ctx
    )
  )

  resp <- request("http://localhost:11434/api/chat") |>
    req_body_json(req_body, auto_unbox = TRUE) |>
    req_timeout(900) |>
    req_perform()

  obj <- resp_body_json(resp, simplifyVector = FALSE)
  obj$message$content %||% ""
}

call_deepseek <- function(
    api_key,
    model,
    messages,
    temperature = 0.2,
    reasoning_effort = "high",
    thinking_enabled = TRUE
) {
  if (is.null(api_key) || trimws(api_key) == "") {
    stop("DeepSeek API Key 为空。请填写 API Key 或设置环境变量 DEEPSEEK_API_KEY。")
  }

  req_body <- list(
    model = model,
    messages = messages,
    stream = FALSE,
    temperature = temperature
  )

  if (!is.null(reasoning_effort) && nzchar(reasoning_effort)) {
    req_body$reasoning_effort <- reasoning_effort
  }

  if (isTRUE(thinking_enabled)) {
    req_body$extra_body <- list(
      thinking = list(type = "enabled")
    )
  }

  resp <- request("https://api.deepseek.com/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(req_body, auto_unbox = TRUE) |>
    req_timeout(900) |>
    req_perform()

  obj <- resp_body_json(resp, simplifyVector = FALSE)
  obj$choices[[1]]$message$content %||% ""
}

call_kimi <- function(
    api_key,
    model,
    messages,
    temperature = 0.2
) {
  if (is.null(api_key) || trimws(api_key) == "") {
    stop("Moonshot / Kimi API Key 为空。请填写 API Key 或设置环境变量 MOONSHOT_API_KEY。")
  }

  has_system <- any(
    vapply(messages, function(m) identical(m$role, "system"), logical(1))
  )

  if (!has_system) {
    messages <- c(
      list(
        list(
          role = "system",
          content = "你是 Kimi，由 Moonshot AI 提供的人工智能助手。你擅长中英文科研写作、生物信息学分析、R 编程和严谨的数据解释。请提供安全、有帮助、准确的回答。"
        )
      ),
      messages
    )
  }

  req_body <- list(
    model = model,
    messages = messages,
    stream = FALSE,
    temperature = temperature
  )

  resp <- request("https://api.moonshot.cn/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(req_body, auto_unbox = TRUE) |>
    req_timeout(900) |>
    req_perform()

  obj <- resp_body_json(resp, simplifyVector = FALSE)
  obj$choices[[1]]$message$content %||% ""
}

call_openai <- function(
    api_key,
    model,
    messages,
    temperature = 0.2,
    max_output_tokens = NULL
) {
  if (is.null(api_key) || trimws(api_key) == "") {
    stop("OpenAI API Key 为空。请填写 API Key 或设置环境变量 OPENAI_API_KEY。")
  }

  if (is.null(max_output_tokens) || is.na(max_output_tokens)) {
    max_output_tokens <- get_model_default_num_predict("openai", model)
  }

  input_text <- messages_to_openai_input(messages)

  req_body <- list(
    model = model,
    input = input_text,
    temperature = temperature,
    max_output_tokens = max_output_tokens
  )

  resp <- request("https://api.openai.com/v1/responses") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(req_body, auto_unbox = TRUE) |>
    req_timeout(900) |>
    req_perform()

  obj <- resp_body_json(resp, simplifyVector = FALSE)
  extract_openai_responses_text(obj)
}

call_llm <- function(
    backend = c("deepseek", "kimi", "openai", "ollama"),
    messages,
    deepseek_api_key = "",
    deepseek_model = "deepseek-v4-pro",
    kimi_api_key = "",
    kimi_model = "kimi-k3",
    openai_api_key = "",
    openai_model = "gpt-5.6-luna",
    ollama_model = "qwen3.6:35b",
    temperature = 0.2,
    num_predict = NULL,
    num_ctx = NULL,
    reasoning_effort = "high",
    thinking_enabled = TRUE
) {
  backend <- match.arg(backend)

  if (backend == "deepseek") {
    call_deepseek(
      api_key = deepseek_api_key,
      model = deepseek_model,
      messages = messages,
      temperature = temperature,
      reasoning_effort = reasoning_effort,
      thinking_enabled = thinking_enabled
    )
  } else if (backend == "kimi") {
    call_kimi(
      api_key = kimi_api_key,
      model = kimi_model,
      messages = messages,
      temperature = temperature
    )
  } else if (backend == "openai") {
    call_openai(
      api_key = openai_api_key,
      model = openai_model,
      messages = messages,
      temperature = temperature,
      max_output_tokens = num_predict
    )
  } else {
    call_ollama(
      model = ollama_model,
      messages = messages,
      temperature = temperature,
      num_predict = num_predict,
      num_ctx = num_ctx
    )
  }
}

# ============================================================
# LLM connection test helper
# ============================================================

get_selected_model_name <- function(
    backend,
    deepseek_model = "deepseek-v4-pro",
    kimi_model = "kimi-k3",
    openai_model = "gpt-5.6-luna",
    ollama_model = "qwen3.6:35b",
    ollama_model_mode = "registered",
    ollama_custom_model = ""
) {
  if (backend == "deepseek") {
    return(deepseek_model)
  }

  if (backend == "kimi") {
    return(kimi_model)
  }

  if (backend == "openai") {
    return(openai_model)
  }

  get_ollama_selected_model(
    ollama_model_mode = ollama_model_mode,
    ollama_model = ollama_model,
    ollama_custom_model = ollama_custom_model
  )
}

get_backend_label <- function(backend) {
  switch(
    backend,
    deepseek = "DeepSeek API",
    kimi = "Kimi / Moonshot API",
    openai = "OpenAI / ChatGPT API",
    ollama = "Local Ollama",
    backend
  )
}

test_llm_connection <- function(
    backend = c("deepseek", "kimi", "openai", "ollama"),
    deepseek_api_key = "",
    deepseek_model = "deepseek-v4-pro",
    kimi_api_key = "",
    kimi_model = "kimi-k3",
    openai_api_key = "",
    openai_model = "gpt-5.6-luna",
    ollama_model = "qwen3.6:35b",
    ollama_model_mode = "registered",
    ollama_custom_model = "",
    temperature = 0
) {
  backend <- match.arg(backend)

  selected_model <- get_selected_model_name(
    backend = backend,
    deepseek_model = deepseek_model,
    kimi_model = kimi_model,
    openai_model = openai_model,
    ollama_model = ollama_model,
    ollama_model_mode = ollama_model_mode,
    ollama_custom_model = ollama_custom_model
  )

  max_ctx <- get_model_max_context(backend, selected_model)
  default_num_predict <- min(512, get_model_default_num_predict(backend, selected_model))

  test_messages <- list(
    list(
      role = "system",
      content = "You are a concise connection-test assistant."
    ),
    list(
      role = "user",
      content = paste0(
        "Please reply only with: WuKong AI connection OK. ",
        "Model name: ",
        selected_model,
        "."
      )
    )
  )

  start_time <- Sys.time()

  res <- tryCatch({
    txt <- call_llm(
      backend = backend,
      messages = test_messages,
      deepseek_api_key = deepseek_api_key,
      deepseek_model = deepseek_model,
      kimi_api_key = kimi_api_key,
      kimi_model = kimi_model,
      openai_api_key = openai_api_key,
      openai_model = openai_model,
      ollama_model = selected_model,
      temperature = temperature,
      num_predict = default_num_predict,
      num_ctx = max_ctx,
      reasoning_effort = "high",
      thinking_enabled = TRUE
    )

    elapsed <- round(
      as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      2
    )

    list(
      ok = TRUE,
      backend = backend,
      backend_label = get_backend_label(backend),
      model = selected_model,
      max_context = max_ctx,
      elapsed = elapsed,
      message = txt
    )
  }, error = function(e) {
    elapsed <- round(
      as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      2
    )

    list(
      ok = FALSE,
      backend = backend,
      backend_label = get_backend_label(backend),
      model = selected_model,
      max_context = max_ctx,
      elapsed = elapsed,
      message = e$message
    )
  })

  res
}

show_llm_test_modal <- function(test_res) {
  if (isTRUE(test_res$ok)) {
    showModal(
      modalDialog(
        title = div(
          style = "color:#1F4E5F;font-weight:700;",
          icon("circle-check"),
          " AI Model Connection Successful"
        ),
        div(
          style = "font-size:15px;line-height:1.8;",
          tags$p(tags$b("Backend: "), test_res$backend_label %||% test_res$backend),
          tags$p(tags$b("Model: "), test_res$model),
          tags$p(tags$b("Default max context: "), paste0(test_res$max_context, " tokens")),
          tags$p(tags$b("Elapsed time: "), paste0(test_res$elapsed, " seconds")),
          tags$p(tags$b("Model response:")),
          tags$pre(
            style = "
              background:#F7F5F0;
              border:1px solid #D8D2C4;
              border-radius:8px;
              padding:12px;
              color:#1F2937;
              white-space:pre-wrap;
            ",
            test_res$message
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  } else {
    showModal(
      modalDialog(
        title = div(
          style = "color:#B55245;font-weight:700;",
          icon("triangle-exclamation"),
          " AI Model Connection Failed"
        ),
        div(
          style = "font-size:15px;line-height:1.8;",
          tags$p(tags$b("Backend: "), test_res$backend_label %||% test_res$backend),
          tags$p(tags$b("Model: "), test_res$model),
          tags$p(tags$b("Default max context: "), paste0(test_res$max_context, " tokens")),
          tags$p(tags$b("Elapsed time: "), paste0(test_res$elapsed, " seconds")),
          tags$p(tags$b("Error message:")),
          tags$pre(
            style = "
              background:#FFF1F0;
              border:1px solid #F2B8B5;
              border-radius:8px;
              padding:12px;
              color:#8A1F11;
              white-space:pre-wrap;
            ",
            test_res$message
          ),
          tags$hr(),
          tags$p(
            style = "color:#6B7280;",
            "Please check API key, model name, network access, request quota, or local Ollama service status."
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }
}

# ============================================================
# Load WuKong internal function list
# ============================================================

if (file.exists("WKfuncslist1.rdata")) {
  load(file = "WKfuncslist1.rdata")
} else {
  WKfuncslist1 <- list()
  warning("WKfuncslist1.rdata not found. WuKongmini will need this object to generate module workflows.")
}

oneclick_summary_text <- reactiveVal("")

# ============================================================
# UI helper functions for optimized Conversation and WuKongmini
# ============================================================

llm_backend_ui <- function(
    prefix,
    ollama_select_id,
    deepseek_key_id,
    deepseek_model_id,
    backend_id,
    kimi_key_id = paste0(prefix, "_kimi_api_key"),
    kimi_model_id = paste0(prefix, "_kimi_model"),
    openai_key_id = paste0(prefix, "_openai_api_key"),
    openai_model_id = paste0(prefix, "_openai_model")
) {
  tagList(
    radioButtons(
      backend_id,
      label = NULL,
      choices = c(
        "DeepSeek API" = "deepseek",
        "Kimi / Moonshot API" = "kimi",
        "OpenAI / ChatGPT API" = "openai",
        "Local Ollama" = "ollama"
      ),
      selected = "deepseek",
      inline = TRUE
    ),

    conditionalPanel(
      condition = sprintf("input.%s == 'deepseek'", backend_id),
      passwordInput(
        deepseek_key_id,
        label = "DeepSeek API Key:",
        value = Sys.getenv("DEEPSEEK_API_KEY"),
        width = "100%"
      ),
      selectInput(
        deepseek_model_id,
        label = "DeepSeek Model:",
        choices = setNames(
          get_provider_models("deepseek")$model,
          get_provider_models("deepseek")$label
        ),
        selected = "deepseek-v4-pro",
        width = "100%"
      )
    ),

    conditionalPanel(
      condition = sprintf("input.%s == 'kimi'", backend_id),
      passwordInput(
        kimi_key_id,
        label = "Moonshot / Kimi API Key:",
        value = Sys.getenv("MOONSHOT_API_KEY"),
        width = "100%"
      ),
      selectInput(
        kimi_model_id,
        label = "Kimi Model:",
        choices = setNames(
          get_provider_models("kimi")$model,
          get_provider_models("kimi")$label
        ),
        selected = "kimi-k3",
        width = "100%"
      )
    ),

    conditionalPanel(
      condition = sprintf("input.%s == 'openai'", backend_id),
      passwordInput(
        openai_key_id,
        label = "OpenAI API Key:",
        value = Sys.getenv("OPENAI_API_KEY"),
        width = "100%"
      ),
      selectInput(
        openai_model_id,
        label = "OpenAI / ChatGPT Model:",
        choices = setNames(
          get_provider_models("openai")$model,
          get_provider_models("openai")$label
        ),
        selected = "gpt-5.6-luna",
        width = "100%"
      )
    ),

    conditionalPanel(
      condition = sprintf("input.%s == 'ollama'", backend_id),

      radioButtons(
        inputId = paste0(prefix, "_ollama_model_mode"),
        label = "Ollama Model Source:",
        choices = c(
          "Use registered/local model" = "registered",
          "Use custom model name" = "custom"
        ),
        selected = "registered",
        inline = TRUE
      ),

      conditionalPanel(
        condition = sprintf(
          "input.%s == 'ollama' && input.%s == 'registered'",
          backend_id,
          paste0(prefix, "_ollama_model_mode")
        ),
        selectInput(
          ollama_select_id,
          label = "Registered / Local Ollama Model:",
          choices = get_ollama_model_choices(),
          selected = get_provider_models("ollama")$model[1],
          width = "100%"
        )
      ),

      conditionalPanel(
        condition = sprintf(
          "input.%s == 'ollama' && input.%s == 'custom'",
          backend_id,
          paste0(prefix, "_ollama_model_mode")
        ),
        textInput(
          inputId = paste0(prefix, "_ollama_custom_model"),
          label = "Custom Ollama Model Name:",
          value = "",
          placeholder = "Example: qwen2.5:32b, llama3.1:8b, deepseek-r1:70b",
          width = "100%"
        ),
        helpText(
          "The model name must match a model already available in your local Ollama service. You can check it with: ollama list"
        )
      )
    )
  )
}
###########
######ui.R
###########

ui <- shinyUI(
  fluidPage(
    useShinyjs(),

    style = "min-width:1400px; background: linear-gradient(to bottom, #f9fafb, #e5e7eb); font-family: 'Roboto', sans-serif;",

    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "busystyle.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "mainstyle.css"),
        tags$script(type = "text/javascript", src = "busy.js"),

        tags$style(type = "text/css", "
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
      ),

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
          height: 380px;
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
          background-color: #1F4E5F;
          color: white;
          border: none;
          padding: 10px 0px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 16px;
          width: 120px;
          transition: all 0.2s ease;
        }

        .btn-primary:hover {
          background-color: #173B49;
          color: #ffffff;
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
          max-height: 450px;
          height: 380px;
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
          text-align: left;
        }

        .message {
          max-width: 70%;
          padding: 10px;
          border-radius: 10px;
          font-size: 16px;
          line-height: 1.5;
          display: inline-block;
        }

        .user .message {
          background-color: #E7F0F2;
          margin-left: auto;
        }

        .assistant .message {
          background-color: #F3F0E8;
        }

        #datapregallery {
          height: 900px;
          overflow-y: scroll;
        }

        #analysisgallery {
          height: 900px;
          overflow-y: scroll;
        }

        #functionalgallery {
          height: 900px;
          overflow-y: scroll;
        }

        #visualizationgallery {
          height: 900px;
          overflow-y: scroll;
        }

        .wukongmini-title-box {
          background: linear-gradient(135deg, #F7F5F0, #E9EEF0);
          border-left: 8px solid #1F4E5F;
          border-radius: 16px;
          padding: 24px 30px;
          margin-bottom: 24px;
          box-shadow: 0 6px 20px rgba(31,78,95,0.13);
        }

        .wukongmini-title {
          color: #1F4E5F;
          font-size: 31px;
          font-weight: 800;
          margin-bottom: 8px;
          letter-spacing: 0.2px;
        }

        .wukongmini-subtitle {
          color: #40545A;
          font-size: 16px;
          line-height: 1.65;
        }

        .warm-card {
          background: #FFFDF8 !important;
          border: 1px solid #D8D2C4 !important;
          border-radius: 14px !important;
          box-shadow: 0 4px 16px rgba(31,78,95,0.10);
        }

        .warm-section-title {
          color: #1F4E5F;
          font-weight: 750;
          letter-spacing: 0.1px;
        }

        .module-panel {
          border-radius: 12px;
          margin-bottom: 18px;
          padding: 12px 14px 14px 14px;
          box-shadow: 0 3px 10px rgba(31,78,95,0.08);
          border: 1px solid rgba(31,78,95,0.10);
        }

        .category-header {
          font-weight: bold;
          font-size: 17px;
          margin-bottom: 8px;
          margin-top: 8px;
          padding-left: 2px;
          color: #1F2937;
        }

        .module-btn {
          margin: 4px 6px 4px 0;
          min-width: 180px;
          text-align: left;
          border-radius: 7px;
          border: 1px solid #C9C3B6;
          background-color: #FFFDF8;
          color: #263238;
          transition: background 0.2s, color 0.2s, border 0.2s, box-shadow 0.2s;
          font-size: 15px;
        }

        .module-btn.selected {
          background-color: #1F4E5F !important;
          color: #FFFFFF !important;
          font-weight: bold;
          border: 2px solid #173B49;
          box-shadow: 0 3px 10px rgba(31,78,95,0.20);
        }

        .module-btn:hover {
          background-color: #E7F0F2;
          color: #1F4E5F;
          border-color: #1F4E5F;
        }

        .selected-list-box {
          min-height: 310px;
          background: #F7F5F0;
          border: 1px solid #D8D2C4;
          border-radius: 12px;
          padding: 16px 12px 12px 18px;
          margin-bottom: 10px;
          color: #263238;
        }

        .step2-scroll-panel {
          max-height: 750px;
          overflow-y: auto;
          padding-right: 16px;
        }

        .nature-test-btn {
          background-color:#B55245 !important;
          color:white !important;
          border:none !important;
          padding:9px 12px !important;
          border-radius:6px !important;
          font-size:15px !important;
          transition:all 0.2s ease;
        }

        .nature-test-btn:hover {
          background-color:#923F35 !important;
          color:white !important;
        }

        .nature-download-btn {
          background-color:#7A9E7E !important;
          color:white !important;
          border:none !important;
          padding:10px 12px !important;
          border-radius:6px !important;
          font-size:16px !important;
        }

        .nature-download-btn:hover {
          background-color:#628468 !important;
          color:white !important;
        }

        .oneclick-scroll-box {
          max-height: 650px;
          overflow-y: auto;
          overflow-x: auto;
          padding-right: 8px;
        }

        .oneclick-scroll-box::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }

        .oneclick-scroll-box::-webkit-scrollbar-track {
          background: #F3F0E8;
          border-radius: 8px;
        }

        .oneclick-scroll-box::-webkit-scrollbar-thumb {
          background: #B8B0A2;
          border-radius: 8px;
        }

        .oneclick-scroll-box::-webkit-scrollbar-thumb:hover {
          background: #8F8678;
        }
      ")),

      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          $('#example_link').on('click', function(e) {
            e.preventDefault();
            Shiny.setInputValue('example_clicked', true, {priority: 'event'});
          });
        });
      ")),

      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        id = "loadmessage",
        tags$div(h2(strong("Thinking...")), img(src = "rmd_loader.gif"))
      ),

      navbarPage(
        title = "",
        windowTitle = "WuKong Platform",
        fluid = FALSE,
        position = "fixed-top",
        id = "navbarid",

        tabPanel(
          "Home",
          uiOutput("welcomeui"),
          icon = icon("home")
        ),

        tabPanel(
          "Functions",
          value = "functionspanel",
          div(
            style = "margin-top:3px; margin-left:0%; z-index:9999; position:absolute;",
            img(src = "wukonglogo.png", width = "135px")
          ),

          navlistPanel(
            id = "gongnengquanid",

            # =====================================================
            # Replaced Part: 1. Conversation
            # =====================================================
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

                  column(
                    8,
                    div(
                      style = "display:flex; align-items:center; justify-content:space-between;",
                      textAreaInput(
                        "user_input",
                        NULL,
                        value = "",
                        width = "100%",
                        height = "250px",
                        placeholder = "Type your message here..."
                      )
                    )
                  ),

                  column(
                    4,
                    div(
                      style = "margin-top:0px; margin-left:0px;",
                      tags$label(
                        "LLM Backend Settings:",
                        style = "margin-bottom:0px; font-weight:bold; color:#1F4E5F;"
                      ),

                      llm_backend_ui(
                        prefix = "chat",
                        ollama_select_id = "llmmodel",
                        deepseek_key_id = "deepseek_api_key_chat",
                        deepseek_model_id = "deepseek_model_chat",
                        backend_id = "chat_llm_backend",
                        kimi_key_id = "chat_kimi_api_key",
                        kimi_model_id = "chat_kimi_model",
                        openai_key_id = "chat_openai_api_key",
                        openai_model_id = "chat_openai_model"
                      ),

                      div(
                        style = "display:flex; gap:10px; align-items:center;",
                        actionButton("send", "Send", class = "btn-primary"),
                        actionButton(
                          "test_ai_connection_chat",
                          "Test AI Connection",
                          class = "nature-test-btn",
                          style = "width:170px;"
                        )
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

            # =====================================================
            # Replaced Part: 6. WuKongmini
            # =====================================================
            tabPanel(
              "6. WuKongmini",
              value = "oneclickpanel",
              uiOutput("oneclickgallery")
            ),

            well = TRUE,
            fluid = FALSE,
            widths = c(2, 10)
          ),

          icon = icon("cogs")
        ),

        tabPanel(
          "Help",
          div(
            style = "margin-top:-57px; margin-left:0%; z-index:9999; position:absolute;",
            img(src = "wukonglogo.png", width = "135px")
          ),

          div(
            style = "margin-top:60px; padding: 20px; background-color: #f9f9f9; border-radius: 10px;",
            h2(
              "WuKong Platform Help Center",
              style = "text-align:center; color: #1d4ed8;"
            ),

            p(
              "Welcome to the Help Center! Here, you can find information on how to use the WuKong Platform effectively.",
              style = "text-align:center; font-size: 16px; color: #4b5563;"
            ),

            div(
              style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
              h3("General Overview", style = "color: #2563eb;"),
              p("WuKong is a powerful platform designed to elevate the way researchers approach proteomics data analysis. It integrates over 72 well-established analysis modules that cover the entire range of data preprocessing, statistical analysis, visualization, functional annotation and workflow designer. On the one hand, these modules retain the classic analysis frameworks, allowing users to adjust parameters within each module to directly generate results tailored to their specific research needs. This flexibility ensures that both beginners and experienced bioinformaticians can efficiently conduct complex analyses without being constrained by rigid workflows. On the other hand, WuKong incorporates large language models (LLMs) to enhance user experience by facilitating conversational interactions with the platform. Researchers can select from a variety of locally installed LLMs or cloud LLM APIs, enabling them to conduct analyses through an intuitive chat interface. This approach not only simplifies the analytical process but also provides real-time guidance and support, making it easier for users to navigate complex datasets and analytical procedures."),

              p("Key features include:"),

              tags$ul(
                tags$li("Comprehensive Modular Coverage: WuKong integrates over 72 modular tools for data preprocessing, statistical analysis, functional enrichment, visualization, and workflow design, providing a versatile framework for complex proteomics researches."),
                tags$li("Dual-mode Analytical Interaction: Users can conduct analyses through either conventional GUI-based modules or dynamic, natural language-driven interactions powered by locally deployed or cloud-based LLMs, greatly enhancing accessibility for users with diverse computational backgrounds."),
                tags$li("Workflow Designer for Flexible Pipelines: The innovative workflow designer enables users to freely select, combine, and execute multiple analysis modules in custom pipelines, overcoming the step-by-step limitations of conventional tools and streamlining complex analyses."),
                tags$li("Prompt-aware Architecture for Enhanced Interpretability: WuKong’s unique prompt-aware system allows LLMs to reference internal code logic, ensuring that natural language queries yield precise, transparent and reproducible results tailored to user intentions."),
                tags$li("Scalable LLM Integration: The platform supports multiple LLM backends including DeepSeek, Kimi/Moonshot, OpenAI/ChatGPT, and local Ollama models. This flexibility balances resource efficiency with accuracy, enabling both exploratory tasks and publication-grade analyses.")
              )
            ),

            div(
              style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
              h3("Source codes and user manual", style = "color: #2563eb;"),
              p(
                "Source codes can be accessed at our ",
                tags$a(
                  href = "https://github.com/wangshisheng/WuKong",
                  "GitHub Repository",
                  target = "_blank"
                ),
                " and the detailed user manual can be downloaded from ",
                tags$a(
                  href = "https://github.com/wangshisheng/WuKong/blob/master/UserManual.pdf",
                  "User Manual",
                  target = "_blank"
                ),
                "."
              )
            ),

            div(
              style = "margin-top:20px; padding: 20px; background-color: #ffffff; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
              h3("Contact Us", style = "color: #2563eb;"),
              p(
                "For further assistance, please contact our support team at ",
                tags$a(
                  href = "mailto:shishengwang@wchscu.cn",
                  "shishengwang@wchscu.cn"
                ),
                "."
              )
            )
          ),

          icon = icon("binoculars")
        )
      )
    )
  )
)
###########
######server.R
###########

server <- shinyServer(
  function(input, output, session) {
    options(shiny.maxRequestSize = 100 * 1024^2)

    session$onSessionEnded(function() {
      print("Session ended cleanly")
    })

    # ============================================================
    # LLM connection tests for optimized Conversation and WuKongmini
    # ============================================================

    observeEvent(input$test_ai_connection_chat, {
      showModal(
        modalDialog(
          title = div(
            style = "color:#1F4E5F;font-weight:700;",
            icon("spinner"),
            " Testing AI Connection..."
          ),
          div(
            style = "font-size:15px;line-height:1.8;",
            tags$p("WuKong is testing the selected AI backend and model with its default maximum context setting.")
          ),
          footer = NULL,
          easyClose = FALSE
        )
      )

      test_res <- test_llm_connection(
        backend = input$chat_llm_backend,
        deepseek_api_key = input$deepseek_api_key_chat,
        deepseek_model = input$deepseek_model_chat,
        kimi_api_key = input$chat_kimi_api_key,
        kimi_model = input$chat_kimi_model,
        openai_api_key = input$chat_openai_api_key,
        openai_model = input$chat_openai_model,
        ollama_model = input$llmmodel,
        ollama_model_mode = input$chat_ollama_model_mode %||% "registered",
        ollama_custom_model = input$chat_ollama_custom_model %||% "",
        temperature = 0
      )

      removeModal()
      show_llm_test_modal(test_res)
    })

    observeEvent(input$test_ai_connection_oneclick, {
      showModal(
        modalDialog(
          title = div(
            style = "color:#1F4E5F;font-weight:700;",
            icon("spinner"),
            " Testing AI Connection..."
          ),
          div(
            style = "font-size:15px;line-height:1.8;",
            tags$p("WuKongmini is testing the selected AI backend and model with its default maximum context setting.")
          ),
          footer = NULL,
          easyClose = FALSE
        )
      )

      test_res <- test_llm_connection(
        backend = input$oneclick_llm_backend,
        deepseek_api_key = input$deepseek_api_key_oneclick,
        deepseek_model = input$deepseek_model_oneclick,
        kimi_api_key = input$kimi_api_key_oneclick,
        kimi_model = input$kimi_model_oneclick,
        openai_api_key = input$openai_api_key_oneclick,
        openai_model = input$openai_model_oneclick,
        ollama_model = input$llmmodeloneclick,
        ollama_model_mode = input$oneclick_ollama_model_mode %||% "registered",
        ollama_custom_model = input$oneclick_ollama_custom_model %||% "",
        temperature = 0
      )

      removeModal()
      show_llm_test_modal(test_res)
    })

    # ============================================================
    # Home page
    # ============================================================

    output$welcomeui <- renderUI({
      fluidRow(
        fluidRow(
          div(
            style = "margin-top:3px; margin-left:18%; z-index:9999; position: absolute;",
            img(src = "wukonglogo.png", width = "135px")
          )
        ),

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

        div(
          style = "margin-top:30px; text-align:center; font-size:240%; font-weight:bold; color:#1d4ed8;",
          HTML("Function Modules")
        ),

        div(
          id = "moduleup1",
          fluidRow(
            column(
              6,
              div(
                style = "text-align:center; margin-top:20px; margin-left:30%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(img(src = "conversation.png", height = "300px")),
                div(
                  style = "margin-top:15px; text-align:center; font-size:120%; font-weight:bold; color:#1f2937;",
                  HTML("Conversation")
                ),
                div(
                  style = "margin-top:10px; text-align:center; font-size:110%; color:#6b7280;",
                  HTML("Chat with local or cloud large language models <br />that are deployed locally or called by APIs.")
                ),
                div(
                  style = "text-align:center; margin-top:15px;",
                  actionButton(
                    "button_module1",
                    "Learn More",
                    style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
                  )
                )
              )
            ),

            column(
              6,
              div(
                style = "text-align:center; margin-top:20px; margin-right:30%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(img(src = "datapreprocess.png", height = "300px")),
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
                  actionButton(
                    "button_module2",
                    "Learn More",
                    style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
                  )
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
                div(img(src = "danyuanduoyuan.png", height = "300px")),
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
                  actionButton(
                    "button_module3",
                    "Learn More",
                    style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
                  )
                )
              )
            ),

            column(
              4,
              div(
                style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(img(src = "gongneng.png", height = "300px")),
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
                  actionButton(
                    "button_module4",
                    "Learn More",
                    style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
                  )
                )
              )
            ),

            column(
              4,
              div(
                style = "text-align:center; margin-top:40px; margin-right:10%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
                class = "module-box",
                div(img(src = "huatu.png", height = "300px")),
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
                  actionButton(
                    "button_module5",
                    "Learn More",
                    style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
                  )
                )
              )
            )
          )
        ),

        div(
          style = "margin-top:50px; background:#1f2937; height:100px; padding: 30px 15px;",
          div(
            style = "text-align:center; font-size:100%; color:white;",
            HTML("&copy; 2026 WuKong Platform")
          )
        )
      )
    })

    # ============================================================
    # Home module buttons
    # ============================================================

    observeEvent(input$button_module1, {
      updateNavbarPage(session, "navbarid", selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "Conversationpanel")
    })

    observeEvent(input$button_module2, {
      updateNavbarPage(session, "navbarid", selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "datapregallerypanel")
    })

    observeEvent(input$button_module3, {
      updateNavbarPage(session, "navbarid", selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "analysisgallerypanel")
    })

    observeEvent(input$button_module4, {
      updateNavbarPage(session, "navbarid", selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "functionalgallerypanel")
    })

    observeEvent(input$button_module5, {
      updateNavbarPage(session, "navbarid", selected = "functionspanel")
      updateNavlistPanel(session, "gongnengquanid", selected = "visualizationgallerypanel")
    })

    # ============================================================
    # Original Data Pre-processing gallery
    # ============================================================

    output$datapregallery <- renderUI({
      fluidRow(
        style = "margin-top:60px;",
        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(img(src = "CV_LLM.png", width = "200px", height = "200px")),
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
              actionButton(
                "btnCV_LLM",
                "Start",
                style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
              )
            )
          )
        ),

        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(img(src = "MissingValue_LLM.png", width = "200px", height = "200px")),
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
              actionButton(
                "btnMissingValue_LLM",
                "Start",
                style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
              )
            )
          )
        ),

        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(img(src = "Norm_LLM.png", width = "200px", height = "200px")),
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
              actionButton(
                "btnNorm_LLM",
                "Start",
                style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
              )
            )
          )
        ),

        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(img(src = "TableMerge_LLM.png", width = "200px", height = "200px")),
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
              actionButton(
                "btnTableMerge_LLM",
                "Start",
                style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
              )
            )
          )
        ),

        column(
          4,
          div(
            style = "text-align:center; margin-top:40px; margin-left:0%; padding:20px; border-radius:10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); transition: transform 0.2s; background: #f9fafb;",
            class = "module-box",
            div(img(src = "AnalyzeWebPage_LLM.png", width = "200px", height = "200px")),
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
              actionButton(
                "btnAnalyzeWebPage_LLM",
                "Start",
                style = "background-color: #3b82f6; color: white; border-radius: 5px; padding: 8px 15px;"
              )
            )
          )
        )
      )
    })

    observeEvent(input$btnAnalyzeWebPage_LLM, {
      rstudioapi::jobRunScript(system.file("home/btnAnalyzeWebPage_LLM.R", package = "WuKong"))
    })

    observeEvent(input$btnCV_LLM, {
      rstudioapi::jobRunScript(system.file("home/btnCV_LLM.R", package = "WuKong"))
    })

    observeEvent(input$btnMissingValue_LLM, {
      rstudioapi::jobRunScript(system.file("home/btnMissingValue_LLM.R", package = "WuKong"))
    })

    observeEvent(input$btnNorm_LLM, {
      rstudioapi::jobRunScript(system.file("home/btnNorm_LLM.R", package = "WuKong"))
    })

    observeEvent(input$btnTableMerge_LLM, {
      rstudioapi::jobRunScript(system.file("home/btnTableMerge_LLM.R", package = "WuKong"))
    })
    # ============================================================
    # Helper: render original-style module gallery
    # ============================================================

    render_wukong_module_gallery <- function(modules) {
      # Split modules into rows of 3 to avoid Bootstrap float misalignment
      module_rows <- split(
        modules,
        ceiling(seq_along(modules) / 3)
      )

      div(
        style = "margin-top:60px;",

        lapply(module_rows, function(row_modules) {
          fluidRow(
            style = "margin-bottom:30px; display:flex; align-items:stretch;",

            lapply(row_modules, function(m) {
              column(
                4,
                style = "display:flex;",

                div(
                  style = "
                text-align:center;
                width:100%;
                margin-top:0px;
                padding:20px;
                border-radius:10px;
                box-shadow:0px 4px 6px rgba(0, 0, 0, 0.1);
                transition:transform 0.2s;
                background:#f9fafb;
                display:flex;
                flex-direction:column;
                justify-content:space-between;
                min-height:390px;
              ",
                  class = "module-box",

                  div(
                    img(
                      src = m$img,
                      width = "200px",
                      height = "200px",
                      style = "object-fit:contain;"
                    )
                  ),

                  div(
                    style = "
                  margin-top:15px;
                  text-align:center;
                  font-size:120%;
                  font-weight:bold;
                  color:#1f2937;
                  min-height:44px;
                  display:flex;
                  align-items:center;
                  justify-content:center;
                ",
                    HTML(m$title)
                  ),

                  div(
                    style = "
                  margin-top:10px;
                  text-align:center;
                  font-size:110%;
                  color:#6b7280;
                  word-wrap:break-word;
                  min-height:76px;
                  display:flex;
                  align-items:center;
                  justify-content:center;
                ",
                    HTML(m$description)
                  ),

                  div(
                    style = "text-align:center; margin-top:15px;",
                    actionButton(
                      m$button,
                      "Start",
                      style = "
                    background-color:#3b82f6;
                    color:white;
                    border-radius:5px;
                    padding:8px 15px;
                    border:none;
                  "
                    )
                  )
                )
              )
            })
          )
        })
      )
    }

    register_wukong_job_buttons <- function(modules) {
      lapply(modules, function(m) {
        local({
          mx <- m
          observeEvent(input[[mx$button]], {
            rstudioapi::jobRunScript(
              system.file(
                paste0("home/", mx$script),
                package = "WuKong"
              )
            )
          })
        })
      })
    }

    # ============================================================
    # Original Statistical Analysis gallery
    # ============================================================

    analysis_modules_original <- list(
      list(
        button = "btnConsensusClustering_LLM",
        script = "btnConsensusClustering_LLM.R",
        img = "ConsensusClustering_LLM.png",
        title = "Consensus clustering",
        description = "Achieve robust clustering results through consensus clustering with LLM support. This tool enhances data reliability by combining multiple clustering results for improved accuracy."
      ),
      list(
        button = "btnDEPannova_LLM",
        script = "btnDEPannova_LLM.R",
        img = "DEPannova_LLM.png",
        title = "One-way ANOVA",
        description = "Perform differential expression analysis using one-way ANOVA tests with the help of LLMs. This tool is valuable for identifying significant differences between groups in datasets."
      ),
      list(
        button = "btnDEPlimma_LLM",
        script = "btnDEPlimma_LLM.R",
        img = "DEPlimma_LLM.png",
        title = "Limma",
        description = "Conduct differential expression analysis with the Limma package, guided by LLMs. This method is widely used in omics research."
      ),
      list(
        button = "btnDEPsamr_LLM",
        script = "btnDEPsamr_LLM.R",
        img = "DEPsamr_LLM.png",
        title = "SAMR",
        description = "Utilize SAMR (Significance Analysis of Microarrays) for differential expression analysis. LLMs provide step-by-step support for accurate and efficient analysis."
      ),
      list(
        button = "btnDEPttest_LLM",
        script = "btnDEPttest_LLM.R",
        img = "DEPttest_LLM.png",
        title = "Student's t-Test",
        description = "Perform differential expression analysis using Student's t-test with LLM guidance. This statistical method identifies significant differences between two groups."
      ),
      list(
        button = "btnDEPwilcox_LLM",
        script = "btnDEPwilcox_LLM.R",
        img = "DEPwilcox_LLM.png",
        title = "Wilcoxon Rank Sum and Signed Rank Tests",
        description = "Conduct non-parametric differential expression analysis using Wilcoxon tests. LLMs assist in ensuring accurate and reliable results."
      ),
      list(
        button = "btnDNB_LLM",
        script = "btnDNB_LLM.R",
        img = "DNB_LLM.png",
        title = "Dynamic Network Biomarkers",
        description = "Identify dynamic network biomarkers (DNBs) with LLM support. This tool is crucial for understanding critical transitions in biological systems."
      ),
      list(
        button = "btnFactorAnalysis_LLM",
        script = "btnFactorAnalysis_LLM.R",
        img = "FactorAnalysis_LLM.png",
        title = "Factor Analysis",
        description = "Perform factor analysis to uncover underlying variables in datasets. LLMs guide users through the process, ensuring accurate and interpretable results."
      ),
      list(
        button = "btnGRA_LLM",
        script = "btnGRA_LLM.R",
        img = "GRA_LLM.png",
        title = "Grey Relational Analysis",
        description = "Conduct Grey Relational Analysis (GRA) to evaluate relationships between variables. LLMs provide insights and simplify the analysis process for better decision-making."
      ),
      list(
        button = "btnHCA_LLM",
        script = "btnHCA_LLM.R",
        img = "HCA_LLM.png",
        title = "Hierarchical Cluster Analysis",
        description = "Perform hierarchical cluster analysis (HCA) with LLM support. This tool helps organize data into meaningful groups based on similarity."
      ),
      list(
        button = "btnKmeans_LLM",
        script = "btnKmeans_LLM.R",
        img = "Kmeans_LLM.png",
        title = "K-means Clustering",
        description = "Conduct K-means clustering with LLM support. This tool helps partition datasets into clusters based on similarity, simplifying data segmentation."
      ),
      list(
        button = "btnLackofFit_LLM",
        script = "btnLackofFit_LLM.R",
        img = "LackofFit_LLM.png",
        title = "Lack of Fit F-test",
        description = "Perform Lack of Fit F-tests to evaluate the adequacy of regression models. LLMs assist in interpreting results and identifying model improvements."
      ),
      list(
        button = "btnLinearRegression_LLM",
        script = "btnLinearRegression_LLM.R",
        img = "LinearRegression_LLM.png",
        title = "Linear Regression",
        description = "Conduct linear regression analysis with the help of LLMs. This tool models relationships between variables and provides insights into predictive trends."
      ),
      list(
        button = "btnLogisticRegression_LLM",
        script = "btnLogisticRegression_LLM.R",
        img = "LogisticRegression_LLM.png",
        title = "Logistic Regression",
        description = "Perform logistic regression analysis with LLM support. This statistical method is ideal for modeling binary outcomes and identifying significant predictors."
      ),
      list(
        button = "btnMfuzz_LLM",
        script = "btnMfuzz_LLM.R",
        img = "Mfuzz_LLM.png",
        title = "Mfuzz",
        description = "Perform fuzzy clustering using the Mfuzz package with LLM guidance. This tool is particularly useful for analyzing time-series data in biological studies."
      ),
      list(
        button = "btnNDM_LLM",
        script = "btnNDM_LLM.R",
        img = "NDM_LLM.png",
        title = "Network Degree Matrix",
        description = "Analyze network structures using the Network Degree Matrix (NDM). LLMs assist in interpreting network connectivity and relationships."
      ),
      list(
        button = "btnOPLSDA_LLM",
        script = "btnOPLSDA_LLM.R",
        img = "OPLSDA_LLM.png",
        title = "OPLS-DA",
        description = "Perform Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA) with LLM support. This tool is widely used for classification and biomarker discovery."
      ),
      list(
        button = "btnPCA_LLM",
        script = "btnPCA_LLM.R",
        img = "PCA_LLM.png",
        title = "PCA",
        description = "Conduct Principal Component Analysis (PCA) with LLM guidance. This tool reduces data dimensionality while preserving important information."
      ),
      list(
        button = "btnPCoA_LLM",
        script = "btnPCoA_LLM.R",
        img = "PCoA_LLM.png",
        title = "PCoA",
        description = "Perform Principal Coordinates Analysis (PCoA) to explore similarities or dissimilarities in data. LLMs guide users through the process for effective visualization."
      ),
      list(
        button = "btnPLSDA_LLM",
        script = "btnPLSDA_LLM.R",
        img = "PLSDA_LLM.png",
        title = "PLS-DA",
        description = "Conduct Partial Least Squares Discriminant Analysis (PLS-DA) with LLM support. This tool is ideal for classification and feature selection in high-dimensional data."
      ),
      list(
        button = "btnPowerAnalysis_LLM",
        script = "btnPowerAnalysis_LLM.R",
        img = "PowerAnalysis_LLM.png",
        title = "Power Analysis",
        description = "Perform power analysis to determine the sample size needed for statistical tests. LLMs provide insights and ensure accurate calculations."
      ),
      list(
        button = "btnRCS_LLM",
        script = "btnRCS_LLM.R",
        img = "RCS_LLM.png",
        title = "Restricted Cubic Spline Analysis",
        description = "Perform Restricted Cubic Spline (RCS) analysis to model non-linear relationships. LLMs assist in interpreting results and creating visualizations."
      ),
      list(
        button = "btnRDA_LLM",
        script = "btnRDA_LLM.R",
        img = "RDA_LLM.png",
        title = "Redundancy Analysis",
        description = "Conduct Redundancy Analysis (RDA) to explore relationships between datasets. LLMs provide guidance for accurate and interpretable results."
      ),
      list(
        button = "btnRRHO_LLM",
        script = "btnRRHO_LLM.R",
        img = "RRHO_LLM.png",
        title = "Rank Rank Hypergeometric Overlap Analysis",
        description = "Perform Rank Rank Hypergeometric Overlap Analysis (RRHO) analysis to identify overlaps between ranked datasets. LLMs guide users in uncovering meaningful intersections."
      ),
      list(
        button = "btnSIMCA_LLM",
        script = "btnSIMCA_LLM.R",
        img = "SIMCA_LLM.png",
        title = "Soft independent modelling by class analogy",
        description = "Perform SIMCA for classification analysis. LLMs provide guidance in creating robust and interpretable models."
      ),
      list(
        button = "btntimecourse_LLM",
        script = "btntimecourse_LLM.R",
        img = "timecourse_LLM.png",
        title = "Time Course Data Analysis",
        description = "Analyze time-course data effectively with LLM guidance. This tool is ideal for exploring trends and changes over time."
      ),
      list(
        button = "btntsne_LLM",
        script = "btntsne_LLM.R",
        img = "tsne_LLM.png",
        title = "t-SNE",
        description = "Perform t-Distributed Stochastic Neighbor Embedding (t-SNE) for dimensionality reduction and data visualization. LLMs assist in creating interpretable results."
      ),
      list(
        button = "btnumap_LLM",
        script = "btnumap_LLM.R",
        img = "umap_LLM.png",
        title = "UMAP",
        description = "Conduct Uniform Manifold Approximation and Projection (UMAP) for dimensionality reduction. LLMs guide users in creating clear visualizations."
      ),
      list(
        button = "btnTumorPurity_LLM",
        script = "btnTumorPurity_LLM.R",
        img = "TumorPurity_LLM.png",
        title = "Estimate Tumor Purity",
        description = "Estimate tumor purity levels with LLM support. This tool aids in cancer research by providing insights into tumor composition."
      )
    )

    output$analysisgallery <- renderUI({
      render_wukong_module_gallery(analysis_modules_original)
    })

    register_wukong_job_buttons(analysis_modules_original)

    # ============================================================
    # Original Functional Annotation gallery
    # ============================================================

    functional_modules_original <- list(
      list(
        button = "btnCelltype_LLM",
        script = "btnCelltype_LLM.R",
        img = "Celltype_LLM.png",
        title = "Cell Type Annotation",
        description = "Annotate and classify cell types effectively using LLMs. This tool is particularly useful for biological and medical research, ensuring accurate and efficient cell-type identification."
      ),
      list(
        button = "btnExploreGO_LLM",
        script = "btnExploreGO_LLM.R",
        img = "ExploreGO_LLM.png",
        title = "Exploring Gene/Protein Functions Based On GO Database",
        description = "Explore gene and protein functions using the Gene Ontology (GO) database. LLMs provide insights and aid in functional annotation and analysis."
      ),
      list(
        button = "btnGOenrich_LLM",
        script = "btnGOenrich_LLM.R",
        img = "GOenrich_LLM.png",
        title = "GO Enrichment Analysis",
        description = "Perform Gene Ontology (GO) enrichment analysis with the guidance of LLMs. This tool identifies significantly enriched biological processes, molecular functions, and cellular components."
      ),
      list(
        button = "btnKEGGenrich_LLM",
        script = "btnKEGGenrich_LLM.R",
        img = "KEGGenrich_LLM.png",
        title = "KEGG Enrichment Analysis",
        description = "Perform Kyoto Encyclopedia of Genes and Genomes (KEGG) enrichment analysis with LLM guidance. This tool identifies enriched pathways and biological processes."
      ),
      list(
        button = "btngseaGO_LLM",
        script = "btngseaGO_LLM.R",
        img = "gseaGO_LLM.png",
        title = "Gene Set Enrichment Analysis of Gene Ontology",
        description = "Perform Gene Set Enrichment Analysis of Gene Ontology (GO) with LLM guidance."
      ),
      list(
        button = "btngseaKEGG_LLM",
        script = "btngseaKEGG_LLM.R",
        img = "gseaKEGG_LLM.png",
        title = "Gene Set Enrichment Analysis of KEGG",
        description = "Perform Gene Set Enrichment Analysis of Kyoto Encyclopedia of Genes and Genomes (KEGG) with LLM guidance."
      )
    )

    output$functionalgallery <- renderUI({
      render_wukong_module_gallery(functional_modules_original)
    })

    register_wukong_job_buttons(functional_modules_original)

    # ============================================================
    # Original Data Visualization gallery
    # ============================================================

    visualization_modules_original <- list(
      list(
        button = "btnBarplot_LLM",
        script = "btnBarplot_LLM.R",
        img = "Barplot_LLM.png",
        title = "Barplot",
        description = "Create and customize bar plots with the help of large language models. This tool simplifies the visualization of categorical data, making it easier to analyze patterns and trends."
      ),
      list(
        button = "btnBarPointplot_LLM",
        script = "btnBarPointplot_LLM.R",
        img = "BarPointplot_LLM.png",
        title = "Bar and Point Plot",
        description = "This feature combines bar and point plots to provide a comprehensive visualization of data. It uses LLMs to guide users in creating detailed and informative plots that highlight key data points."
      ),
      list(
        button = "btnBoxPointplot_LLM",
        script = "btnBoxPointplot_LLM.R",
        img = "BoxPointplot_LLM.png",
        title = "Box and Point Plot",
        description = "Generate box and point plots effortlessly with the support of LLMs. This tool is ideal for visualizing distributions and comparing individual data points within a dataset."
      ),
      list(
        button = "btnClusterCorNetwork_LLM",
        script = "btnClusterCorNetwork_LLM.R",
        img = "ClusterCorNetwork_LLM.png",
        title = "Clustering using Correlation Network",
        description = "Perform clustering analysis using correlation networks with the guidance of LLMs. This method helps uncover relationships and groupings within complex datasets."
      ),
      list(
        button = "btnContourPlot_LLM",
        script = "btnContourPlot_LLM.R",
        img = "ContourPlot_LLM.png",
        title = "Contour Plot",
        description = "Create contour plots to visualize three-dimensional data in two dimensions. LLMs assist in generating clear and informative plots for advanced data analysis."
      ),
      list(
        button = "btnCorPlot_LLM",
        script = "btnCorPlot_LLM.R",
        img = "CorPlot_LLM.png",
        title = "Correlation Plot",
        description = "Visualize relationships between variables using correlation plots. LLMs simplify the process, ensuring accurate representation and interpretation of data correlations."
      ),
      list(
        button = "btnCorrelationNetwork_LLM",
        script = "btnCorrelationNetwork_LLM.R",
        img = "CorrelationNetwork_LLM.png",
        title = "Correlation Network Plot",
        description = "Construct correlation network plots with ease using LLMs. This tool helps visualize complex relationships and interactions between variables in a network format."
      ),
      list(
        button = "btnCrossErrorbarplot_LLM",
        script = "btnCrossErrorbarplot_LLM.R",
        img = "CrossErrorbarplot_LLM.png",
        title = "Cross Error Bar Plot",
        description = "Design cross error bar plots to represent data variability. LLMs provide guidance in creating precise and visually appealing plots for statistical analysis."
      ),
      list(
        button = "btnDendrogram_LLM",
        script = "btnDendrogram_LLM.R",
        img = "Dendrogram_LLM.png",
        title = "Dendrogram",
        description = "Generate dendrograms for hierarchical clustering analysis. LLMs assist in creating detailed and interpretable tree-like diagrams for data classification."
      ),
      list(
        button = "btnDivergingBarplot_LLM",
        script = "btnDivergingBarplot_LLM.R",
        img = "DivergingBarplot_LLM.png",
        title = "Diverging Bar Plot",
        description = "Create diverging bar plots to represent data deviations. LLMs simplify the process, enabling clear visualization of positive and negative trends."
      ),
      list(
        button = "btnFunnelPlot_LLM",
        script = "btnFunnelPlot_LLM.R",
        img = "FunnelPlot_LLM.png",
        title = "Funnel Plot",
        description = "Create funnel plots to assess data variability and bias. LLMs assist in generating clear and informative visualizations for meta-analysis."
      ),
      list(
        button = "btnggseqlogo_LLM",
        script = "btnggseqlogo_LLM.R",
        img = "ggseqlogo_LLM.png",
        title = "Protein/DNA Sequence Logo Plot",
        description = "Generate sequence logo plots for protein or DNA sequences. LLMs simplify the process, highlighting conserved regions and sequence patterns."
      ),
      list(
        button = "btnggtreeDendrogram_LLM",
        script = "btnggtreeDendrogram_LLM.R",
        img = "ggtreeDendrogram_LLM.png",
        title = "Dendrogram using ggtree Package",
        description = "Create dendrograms using the ggtree package for advanced hierarchical clustering visualization. LLMs assist in generating detailed and publication-ready dendrograms."
      ),
      list(
        button = "btnHeatscatter_LLM",
        script = "btnHeatscatter_LLM.R",
        img = "Heatscatter_LLM.png",
        title = "Colored Scatter Plot",
        description = "Generate heatscatter plots that combine scatterplots with color gradients to represent data density. LLMs assist in creating visually appealing and informative plots."
      ),
      list(
        button = "btnHistgramDensity_LLM",
        script = "btnHistgramDensity_LLM.R",
        img = "HistgramDensity_LLM.png",
        title = "Histgram and Density Plot",
        description = "Create combined histogram and density plots to visualize data distributions. LLMs provide guidance in customizing and interpreting these plots."
      ),
      list(
        button = "btnLollipopChart_LLM",
        script = "btnLollipopChart_LLM.R",
        img = "LollipopChart_LLM.png",
        title = "Lollipop Chart",
        description = "Create lollipop charts for visualizing data comparisons. LLMs simplify the process, ensuring clear and effective data representation."
      ),
      list(
        button = "btnMarginalPlot_LLM",
        script = "btnMarginalPlot_LLM.R",
        img = "MarginalPlot_LLM.png",
        title = "Marginal Histogram/Boxplot",
        description = "Generate marginal histogram or boxplots to visualize data distributions alongside scatterplots. LLMs provide assistance in creating detailed visualizations."
      ),
      list(
        button = "btnNightingalePlot_LLM",
        script = "btnNightingalePlot_LLM.R",
        img = "NightingalePlot_LLM.png",
        title = "Nightingale Rose Diagram",
        description = "Create Nightingale Rose Diagrams to visualize circular data distributions. LLMs simplify the process, ensuring accurate and visually appealing plots."
      ),
      list(
        button = "btnPairPointPlot_LLM",
        script = "btnPairPointPlot_LLM.R",
        img = "PairPointPlot_LLM.png",
        title = "Pair Point Line Plot",
        description = "Create pair point line plots to visualize paired data relationships. LLMs assist in generating clear and interpretable plots."
      ),
      list(
        button = "btnPiePlot_LLM",
        script = "btnPiePlot_LLM.R",
        img = "PiePlot_LLM.png",
        title = "Pie Plot",
        description = "Create pie charts to visualize categorical data distributions. LLMs simplify the customization and interpretation of these plots."
      ),
      list(
        button = "btnRadarChart_LLM",
        script = "btnRadarChart_LLM.R",
        img = "RadarChart_LLM.png",
        title = "Radar Chart",
        description = "Create radar charts to compare multivariate data. LLMs assist in generating informative and visually appealing plots."
      ),
      list(
        button = "btnRainCloud_LLM",
        script = "btnRainCloud_LLM.R",
        img = "RainCloud_LLM.png",
        title = "Rain Cloud Plot",
        description = "Generate rain cloud plots to visualize data distributions. LLMs guide users in creating combined density and scatter plots."
      ),
      list(
        button = "btnRankPointPlot_LLM",
        script = "btnRankPointPlot_LLM.R",
        img = "RankPointPlot_LLM.png",
        title = "Rank Point Plot",
        description = "Create rank point plots to highlight data rankings. LLMs simplify the process, ensuring clear and effective visualization."
      ),
      list(
        button = "btnRidgePlot_LLM",
        script = "btnRidgePlot_LLM.R",
        img = "RidgePlot_LLM.png",
        title = "Ridge Plot",
        description = "Create ridge plots to visualize distributions across multiple categories. LLMs assist in generating aesthetically pleasing and informative plots."
      ),
      list(
        button = "btnROCplot_LLM",
        script = "btnROCplot_LLM.R",
        img = "ROCplot_LLM.png",
        title = "ROC Plot",
        description = "Generate Receiver Operating Characteristic (ROC) plots to evaluate classification model performance. LLMs provide insights for accurate interpretation."
      ),
      list(
        button = "btnSankeyPlot_LLM",
        script = "btnSankeyPlot_LLM.R",
        img = "SankeyPlot_LLM.png",
        title = "Sankey Chart",
        description = "Create Sankey charts to visualize data flows and relationships. LLMs simplify the process, ensuring accurate and engaging visualizations."
      ),
      list(
        button = "btnScatterEllipsePlot_LLM",
        script = "btnScatterEllipsePlot_LLM.R",
        img = "ScatterEllipsePlot_LLM.png",
        title = "Scatter Ellipse Plot",
        description = "Generate scatter plots with ellipses to highlight data groupings. LLMs assist in creating detailed and informative visualizations."
      ),
      list(
        button = "btnSurvivalPlot_LLM",
        script = "btnSurvivalPlot_LLM.R",
        img = "SurvivalPlot_LLM.png",
        title = "Survival analysis",
        description = "Create survival plots to analyze time-to-event data. LLMs simplify the process, ensuring accurate and clear visualizations."
      ),
      list(
        button = "btnTernaryPlot_LLM",
        script = "btnTernaryPlot_LLM.R",
        img = "TernaryPlot_LLM.png",
        title = "Ternary Plot",
        description = "Create ternary plots to visualize three-component data. LLMs provide guidance in generating accurate and aesthetically pleasing plots."
      ),
      list(
        button = "btnUpsetPlot_LLM",
        script = "btnUpsetPlot_LLM.R",
        img = "UpsetPlot_LLM.png",
        title = "UpSet Plot",
        description = "Create UpSet plots to visualize intersections between sets. LLMs simplify the process, ensuring accurate and engaging visualizations."
      ),
      list(
        button = "btnVenn_LLM",
        script = "btnVenn_LLM.R",
        img = "Venn_LLM.png",
        title = "Venn Plot",
        description = "Generate Venn plots to display overlaps between sets. LLMs assist in creating clear and informative diagrams."
      ),
      list(
        button = "btnViolinplot_LLM",
        script = "btnViolinplot_LLM.R",
        img = "Violinplot_LLM.png",
        title = "Violin Plot",
        description = "Create violin plots to visualize data distributions. LLMs guide users in generating detailed and interpretable plots."
      ),
      list(
        button = "btnVolcano_LLM",
        script = "btnVolcano_LLM.R",
        img = "Volcano_LLM.png",
        title = "Volcano Plot",
        description = "Generate volcano plots to identify significant changes in data. LLMs simplify the process, ensuring accurate and visually appealing results."
      ),
      list(
        button = "btnWorldCloud_LLM",
        script = "btnWorldCloud_LLM.R",
        img = "WorldCloud_LLM.png",
        title = "Word Cloud Plot",
        description = "Create word clouds to visualize text data. LLMs assist in generating aesthetically pleasing and informative visualizations."
      )
    )

    output$visualizationgallery <- renderUI({
      render_wukong_module_gallery(visualization_modules_original)
    })

    register_wukong_job_buttons(visualization_modules_original)
    # ============================================================
    # WuKongmini configuration
    # ============================================================

    oneclick_module_categories <- list(
      "I. Data Pre-processing" = c(
        "Coefficient of variation",
        "Logarithm with base 2",
        "Missing value imputation",
        "Normalization"
      ),
      "II. Statistical Analysis" = c(
        "Consensus clustering",
        "One-way ANOVA",
        "Limma",
        "SAMR",
        "Student's t-Test",
        "Wilcoxon Rank Sum and Signed Rank Tests",
        "HCA",
        "K-means Clustering",
        "Mfuzz",
        "OPLS-DA",
        "PCA",
        "PCoA",
        "PLS-DA",
        "t-SNE",
        "UMAP"
      ),
      "III. Functional Annotation" = c(
        "GO Enrichment Analysis",
        "KEGG Enrichment Analysis",
        "Gene Set Enrichment Analysis of GO",
        "Gene Set Enrichment Analysis of KEGG"
      ),
      "IV. Data Visualization" = c(
        "Barplot",
        "Bar and Point Plot",
        "Box and Point Plot",
        "Correlation Plot",
        "Histgram and Density Plot",
        "Rain Cloud Plot",
        "Rank Point Plot",
        "ROC Plot",
        "Volcano Plot"
      )
    )

    all_oneclick_modules <- unlist(oneclick_module_categories, use.names = FALSE)

    category_colors <- c(
      "I. Data Pre-processing" = "#E7F0F2",
      "II. Statistical Analysis" = "#F3F0E8",
      "III. Functional Annotation" = "#E9F0EA",
      "IV. Data Visualization" = "#F2E8E4"
    )

    category_border_colors <- c(
      "I. Data Pre-processing" = "#1F4E5F",
      "II. Statistical Analysis" = "#B55245",
      "III. Functional Annotation" = "#7A9E7E",
      "IV. Data Visualization" = "#8A6F91"
    )

    selected_modules <- reactiveVal(character())
    workflow_description <- reactiveVal("")

    oneclick_step_results <- reactiveVal(list())
    oneclick_step_titles <- reactiveVal(list())
    oneclick_step_codes <- reactiveVal(list())
    oneclick_step_interpretations <- reactiveVal(list())
    oneclick_report_html <- reactiveVal("")
    oneclick_refined_code_text <- reactiveVal("")
    oneclick_summary_text <- reactiveVal("")

    # ============================================================
    # WuKongmini UI
    # ============================================================

    output$oneclickgallery <- renderUI({
      tags$div(
        style = "margin-top:70px;",

        div(
          class = "wukongmini-title-box",
          div(
            class = "wukongmini-title",
            "WuKongmini: A workflow designer for building custom pipelines"
          ),
          div(
            class = "wukongmini-subtitle",
            "Select analysis modules, describe your workflow intention, and let an LLM refine executable R code, run each step, interpret results, and package all outputs into a downloadable report."
          )
        ),

        div(
          mainPanel(
            width = 12,

            tabsetPanel(
              tabPanel(
                "Step 1: Upload Data",
                fluidRow(
                  column(
                    4,
                    wellPanel(
                      id = "wellpanelid1",
                      class = "warm-card",

                      h4("I. Upload Data/Example Data", class = "warm-section-title"),

                      radioButtons(
                        "loaddatatype",
                        label = NULL,
                        choices = list("A. Upload" = 1, "B. Load example data" = 2),
                        selected = 1,
                        inline = TRUE
                      ),

                      tags$hr(style = "border-color:#D8D2C4;"),

                      conditionalPanel(
                        condition = "input.loaddatatype==1",

                        radioButtons(
                          "fileType_Input",
                          label = h5("Select File Format:"),
                          choices = list(".xlsx" = 1, ".xls" = 2, ".csv/txt" = 3),
                          selected = 1,
                          inline = TRUE
                        ),

                        fileInput(
                          "file1",
                          h5("Please import your data file:"),
                          accept = c("text/csv", "text/plain", ".xlsx", ".xls")
                        ),

                        checkboxInput("header", "Is the first row names?", TRUE),
                        checkboxInput("firstcol", "Is the first column names?", TRUE),

                        conditionalPanel(
                          condition = "input.fileType_Input==1",
                          numericInput("xlsxindex", h5("Which Sheet to read?"), value = 1)
                        ),

                        conditionalPanel(
                          condition = "input.fileType_Input==2",
                          numericInput("xlsxindex", h5("Which Sheet to read?"), value = 1)
                        ),

                        conditionalPanel(
                          condition = "input.fileType_Input==3",
                          radioButtons(
                            "sep",
                            "Data Separator (Comma/Semicolon/Tab/Space):",
                            c(
                              Comma = ",",
                              Semicolon = ";",
                              Tab = "\t",
                              BlankSpace = " "
                            ),
                            ","
                          )
                        ),

                        tags$hr(style = "border-color:#D8D2C4;"),

                        h4("Sample information:", class = "warm-section-title"),

                        textInput(
                          "grnums",
                          h5("1. Group and replicate number:"),
                          value = ""
                        ),

                        bsTooltip(
                          "grnums",
                          'Type group number and replicate number. Example: "2;3-3".',
                          placement = "right",
                          options = list(container = "body")
                        ),

                        textInput(
                          "grnames",
                          h5("2. Group names:"),
                          value = ""
                        ),

                        bsTooltip(
                          "grnames",
                          'Type group names separated by ";". Example: "Control;Experiment".',
                          placement = "right",
                          options = list(container = "body")
                        )
                      ),

                      conditionalPanel(
                        condition = "input.loaddatatype==2",

                        downloadButton(
                          "loaddatadownload1",
                          "Download example expression data",
                          class = "nature-download-btn"
                        ),

                        tags$hr(style = "border-color:#D8D2C4;"),

                        h4("Sample information:", class = "warm-section-title"),

                        textInput(
                          "examgrnums",
                          h5("1. Group and replicate number:"),
                          value = "2;4-4"
                        ),

                        textInput(
                          "examgrnames",
                          h5("2. Group names:"),
                          value = "A;B"
                        )
                      ),

                      uiOutput("goortspecies")
                    )
                  ),

                  column(
                    8,
                    wellPanel(
                      id = "wellpanelid2",
                      class = "warm-card",

                      h4(
                        "II. Display Uploaded Data/Example Data",
                        class = "warm-section-title"
                      ),

                      div(
                        style = "overflow-x:auto; overflow-y:auto;",
                        dataTableOutput("rawdata")
                      )
                    )
                  )
                )
              ),

              tabPanel(
                "Step 2: Select Modules",

                div(
                  class = "step2-scroll-panel",

                  fluidRow(
                    column(
                      6,

                      tags$h4("Available Modules", class = "warm-section-title"),

                      helpText(
                        "Click a module to select or deselect it. Modules are grouped and colored by function."
                      ),

                      lapply(names(oneclick_module_categories), function(cat) {
                        div(
                          class = "module-panel",
                          style = sprintf(
                            "background-color:%s; border-left:8px solid %s;",
                            category_colors[cat],
                            category_border_colors[cat]
                          ),

                          div(class = "category-header", cat),

                          div(
                            id = paste0(
                              "module_list_",
                              gsub(" ", "_", tolower(cat))
                            ),
                            class = "module-list-box",
                            uiOutput(
                              paste0(
                                "module_btns_",
                                gsub(" ", "_", tolower(cat))
                              )
                            )
                          )
                        )
                      })
                    ),

                    column(
                      6,

                      tags$h4(
                        "Your Workflow (Selected Modules, in order)",
                        class = "warm-section-title"
                      ),

                      div(
                        style = "display:flex; gap:20px; align-items:center; padding:10px 10px; margin-bottom:10px;",

                        actionButton(
                          "example_pipeline1",
                          "Example Pipeline 1",
                          class = "btn-primary",
                          width = "150px",
                          title = "A typical proteomics data analysis workflow using DEPs for PCA, HCA, GO, and KEGG."
                        ),

                        actionButton(
                          "example_pipeline2",
                          "Example Pipeline 2",
                          class = "btn-primary",
                          width = "150px",
                          title = "A workflow highlighting GSEA using all proteins."
                        )
                      ),

                      div(
                        class = "selected-list-box",
                        uiOutput("selected_module_list")
                      ),

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
              ),

              tabPanel(
                "Step 3: LLM-assisted WuKongmini",

                tags$h4("Run Workflow", class = "warm-section-title"),

                helpText(
                  "Please configure the LLM backend and click 'Run Workflow'. Refined code, stepwise outputs, bilingual interpretation, and downloadable reports will appear below."
                ),

                div(
                  class = "warm-card",
                  style = "padding:18px 20px; margin-bottom:16px;",

                  tags$h4("LLM Settings", class = "warm-section-title"),

                  helpText(
                    "DeepSeek API is shown first. You may switch to Kimi, OpenAI, or local Ollama if preferred."
                  ),

                  fluidRow(
                    column(
                      3,
                      radioButtons(
                        "oneclick_llm_backend",
                        label = NULL,
                        choices = c(
                          "DeepSeek API" = "deepseek",
                          "Kimi / Moonshot API" = "kimi",
                          "OpenAI / ChatGPT API" = "openai",
                          "Local Ollama" = "ollama"
                        ),
                        selected = "deepseek",
                        inline = FALSE
                      )
                    ),

                    column(
                      5,

                      conditionalPanel(
                        condition = "input.oneclick_llm_backend == 'deepseek'",

                        passwordInput(
                          "deepseek_api_key_oneclick",
                          label = "DeepSeek API Key:",
                          value = Sys.getenv("DEEPSEEK_API_KEY"),
                          width = "100%"
                        ),

                        selectInput(
                          "deepseek_model_oneclick",
                          label = "DeepSeek Model:",
                          choices = setNames(
                            get_provider_models("deepseek")$model,
                            get_provider_models("deepseek")$label
                          ),
                          selected = "deepseek-v4-pro",
                          width = "100%"
                        ),

                        helpText(
                          "DeepSeek requests use reasoning_effort='high' and thinking enabled by default."
                        )
                      ),

                      conditionalPanel(
                        condition = "input.oneclick_llm_backend == 'kimi'",

                        passwordInput(
                          "kimi_api_key_oneclick",
                          label = "Moonshot / Kimi API Key:",
                          value = Sys.getenv("MOONSHOT_API_KEY"),
                          width = "100%"
                        ),

                        selectInput(
                          "kimi_model_oneclick",
                          label = "Kimi Model:",
                          choices = setNames(
                            get_provider_models("kimi")$model,
                            get_provider_models("kimi")$label
                          ),
                          selected = "kimi-k3",
                          width = "100%"
                        )
                      ),

                      conditionalPanel(
                        condition = "input.oneclick_llm_backend == 'openai'",

                        passwordInput(
                          "openai_api_key_oneclick",
                          label = "OpenAI API Key:",
                          value = Sys.getenv("OPENAI_API_KEY"),
                          width = "100%"
                        ),

                        selectInput(
                          "openai_model_oneclick",
                          label = "OpenAI / ChatGPT Model:",
                          choices = setNames(
                            get_provider_models("openai")$model,
                            get_provider_models("openai")$label
                          ),
                          selected = "gpt-5.6-luna",
                          width = "100%"
                        ),

                        helpText("OpenAI models are called through the Responses API.")
                      ),

                      conditionalPanel(
                        condition = "input.oneclick_llm_backend == 'ollama'",

                        radioButtons(
                          "oneclick_ollama_model_mode",
                          label = "Ollama Model Source:",
                          choices = c(
                            "Use registered/local model" = "registered",
                            "Use custom model name" = "custom"
                          ),
                          selected = "registered",
                          inline = TRUE
                        ),

                        conditionalPanel(
                          condition = "input.oneclick_llm_backend == 'ollama' && input.oneclick_ollama_model_mode == 'registered'",

                          selectInput(
                            "llmmodeloneclick",
                            label = "Registered / Local Ollama Model:",
                            choices = get_ollama_model_choices(),
                            selected = get_provider_models("ollama")$model[1],
                            width = "100%"
                          )
                        ),

                        conditionalPanel(
                          condition = "input.oneclick_llm_backend == 'ollama' && input.oneclick_ollama_model_mode == 'custom'",

                          textInput(
                            "oneclick_ollama_custom_model",
                            label = "Custom Ollama Model Name:",
                            value = "",
                            placeholder = "Example: qwen2.5:32b, llama3.1:8b, deepseek-r1:70b",
                            width = "100%"
                          ),

                          helpText(
                            "The model name must match a model already available in your local Ollama service. You can check local models with: ollama list"
                          )
                        ),

                        helpText(
                          "For custom Ollama models, WuKongmini uses a safe default context setting unless the model is registered in the internal model registry."
                        )
                      )
                    ),

                    column(
                      4,

                      numericInput(
                        "oneclick_temperature",
                        label = "Temperature:",
                        value = 0.1,
                        min = 0,
                        max = 1,
                        step = 0.1,
                        width = "100%"
                      ),

                      div(
                        style = "display:flex; gap:12px; margin-top:8px; flex-wrap:wrap;",

                        actionButton(
                          "oneclick_run",
                          "Run Workflow",
                          class = "btn-primary",
                          style = "width:160px;"
                        ),

                        actionButton(
                          "test_ai_connection_oneclick",
                          "Test AI Connection",
                          class = "nature-test-btn",
                          style = "width:180px;"
                        ),

                        downloadButton(
                          "oneclick_download_results",
                          "Download Results",
                          class = "nature-download-btn",
                          style = "width:170px;"
                        )
                      )
                    )
                  )
                ),

                fluidRow(
                  column(
                    6,
                    div(
                      class = "warm-card",
                      style = "padding:16px; min-height:560px;",
                      h5(
                        "Refined Prompts / Optimized R Code",
                        class = "warm-section-title"
                      ),
                      div(
                        class = "oneclick-scroll-box",
                        uiOutput("oneclick_prompts")
                      )
                    )
                  ),

                  column(
                    6,
                    div(
                      class = "warm-card",
                      style = "padding:16px; min-height:560px;",
                      h5(
                        "Analysis Results with Bilingual Interpretation",
                        class = "warm-section-title"
                      ),
                      div(
                        class = "oneclick-scroll-box",
                        uiOutput("oneclick_results")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    })

    # ============================================================
    # Species selector
    # ============================================================

    output$goortspecies <- renderUI({
      if (file.exists("uniprot-species.csv")) {
        goort_spedf <- read.csv(
          "uniprot-species.csv",
          header = TRUE,
          stringsAsFactors = FALSE
        )

        goort_spedf_paste <- paste(
          goort_spedf$Organism.ID,
          goort_spedf$Organism,
          sep = "-"
        )

        selectizeInput(
          "speciesx",
          h5("3. Please choose a species:"),
          choices = goort_spedf_paste,
          options = list(maxOptions = 6000)
        )
      } else {
        textInput(
          "speciesx",
          h5("3. Please input species ID/name:"),
          value = "9606-Homo sapiens"
        )
      }
    })

    # ============================================================
    # Data loading
    # ============================================================

    examplepeakdatas <- reactive({
      if (file.exists("Exampledata1.csv")) {
        read.csv(
          "Exampledata1.csv",
          stringsAsFactors = FALSE,
          check.names = FALSE,
          row.names = 1
        )
      } else {
        data.frame(
          A1 = rnorm(10, 10, 1),
          A2 = rnorm(10, 10, 1),
          A3 = rnorm(10, 10, 1),
          A4 = rnorm(10, 10, 1),
          B1 = rnorm(10, 12, 1),
          B2 = rnorm(10, 12, 1),
          B3 = rnorm(10, 12, 1),
          B4 = rnorm(10, 12, 1),
          row.names = paste0("Prot", 1:10),
          check.names = FALSE
        )
      }
    })

    output$loaddatadownload1 <- downloadHandler(
      filename = function() {
        paste0(
          "Example_ExpressionData_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        write.csv(examplepeakdatas(), file, row.names = TRUE)
      }
    )

    peaksdataout <- reactive({
      if (input$loaddatatype == 1) {
        files <- input$file1

        if (is.null(files)) {
          data.frame(
            Description = "No data loaded. Please upload your dataset or use the example data."
          )
        } else {
          if (input$fileType_Input == "1") {
            read.xlsx(
              files$datapath,
              rowNames = input$firstcol,
              colNames = input$header,
              sheet = input$xlsxindex
            )
          } else if (input$fileType_Input == "2") {
            rownametf <- if (isTRUE(input$firstcol)) 1 else NULL

            read.xls(
              files$datapath,
              sheet = input$xlsxindex,
              header = input$header,
              row.names = rownametf,
              stringsAsFactors = FALSE
            )
          } else {
            rownametf <- if (isTRUE(input$firstcol)) 1 else NULL

            read.csv(
              files$datapath,
              header = input$header,
              row.names = rownametf,
              sep = input$sep,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }
      } else {
        examplepeakdatas()
      }
    })

    output$rawdata <- renderDataTable({
      datatable(
        peaksdataout(),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    # ============================================================
    # WuKongmini module selection
    # ============================================================

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
                btnid <- paste0(
                  "modbtn_",
                  gsub("[^a-zA-Z0-9]", "_", mod)
                )

                actionButton(
                  btnid,
                  mod,
                  class = paste(
                    "module-btn",
                    ifelse(mod %in% sel, "selected", "")
                  ),
                  style = "margin-bottom:6px;"
                )
              })
            )
          })
        })
      }
    })

    observe({
      for (mod in all_oneclick_modules) {
        local({
          modname <- mod
          btnid <- paste0(
            "modbtn_",
            gsub("[^a-zA-Z0-9]", "_", modname)
          )

          observeEvent(input[[btnid]], {
            sel <- selected_modules()

            if (modname %in% sel) {
              selected_modules(sel[sel != modname])
            } else {
              selected_modules(c(sel, modname))
            }
          }, ignoreInit = TRUE)
        })
      }
    })

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

    observeEvent(input$workflow_description, {
      workflow_description(input$workflow_description)
    }, ignoreInit = TRUE)

    example_pipeline_modules1 <- c(
      "Normalization",
      "Logarithm with base 2",
      "Missing value imputation",
      "SAMR",
      "Volcano Plot",
      "PCA",
      "HCA",
      "GO Enrichment Analysis",
      "KEGG Enrichment Analysis"
    )

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
    )

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
9. KEGG Enrichment Analysis using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5."
      )

      updateTextAreaInput(
        session,
        "workflow_description",
        value = workflow_description()
      )
    })

    observeEvent(input$example_pipeline2, {
      selected_modules(example_pipeline_modules2)

      workflow_description(
        "This workflow includes the following steps:
1. Normalization using the median method for the input data.
2. Log2 transformation.
3. Imputation of missing values.
4. SAMR for differential expression analysis.
5. Visualization of results with a volcano plot.
6. PCA using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5. Do NOT use all proteins.
7. HCA using ONLY the differentially expressed proteins (Up and Down proteins) identified in Step 5.
8. Gene Set Enrichment Analysis of GO using the whole proteins identified in Step 4.
9. Gene Set Enrichment Analysis of KEGG using the whole proteins identified in Step 4."
      )

      updateTextAreaInput(
        session,
        "workflow_description",
        value = workflow_description()
      )
    })
    # ============================================================
    # WuKongmini run workflow
    # ============================================================

    observeEvent(input$oneclick_run, {
      oneclick_summary_text("")
      oneclick_step_results(list())
      oneclick_step_titles(list())
      oneclick_step_codes(list())
      oneclick_step_interpretations(list())
      oneclick_report_html("")
      oneclick_refined_code_text("")

      output$oneclick_prompts <- renderUI({
        aceEditor(
          outputId = "rcode",
          value = "WuKongmini is preparing selected module codes and asking the LLM to refine the workflow...\nPlease wait.",
          mode = "r",
          theme = "chrome",
          readOnly = TRUE,
          height = "620px",
          fontSize = 14
        )
      })

      output$oneclick_results <- renderUI({
        HTML("<pre>Workflow is running. Results and bilingual interpretations will be displayed here.</pre>")
      })

      if (input$loaddatatype == 1) {
        shiny::validate(
          need(!is.null(input$file1), "Please upload a data file first."),
          need(nzchar(input$grnames), "Please provide group names."),
          need(nzchar(input$grnums), "Please provide group and replicate numbers.")
        )

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

      inputdata <- peaksdataout()
      speciesx <- input$speciesx
      WKfuncslist2 <- WKfuncslist1
      selx <- selected_modules()
      workflow_desc <- input$workflow_description %||% workflow_description()

      shiny::validate(
        need(length(selx) > 0, "Please select at least one module in Step 2."),
        need(!is.null(inputdata) && ncol(inputdata) > 1, "Input data is invalid or empty.")
      )

      funclistx <- list()

      for (i in seq_along(selx)) {
        if (!is.null(WKfuncslist2[[selx[i]]])) {
          funclistx[[i]] <- WKfuncslist2[[selx[i]]]
        } else {
          funclistx[[i]] <- paste0(
            "# Reference code for module '",
            selx[i],
            "' was not found in WKfuncslist1.\n",
            "# Please generate a robust implementation based on the module name and workflow description.\n"
          )
        }
      }

      names(funclistx) <- selx

      log_step <- which(selx == "Logarithm with base 2")

      if (length(log_step) > 0) {
        funclistx[[log_step]] <- "
# Input: the output object from the previous preprocessing step, or inputdata if this is the first step.
# Output: logdata
logdata <- log2(as.matrix(inputdata))
logdata[is.infinite(logdata)] <- NA
logdata <- as.data.frame(logdata, check.names = FALSE)
logdata
"
        names(funclistx)[log_step] <- "Logarithm with base 2"
      }

      funclistx1 <- funclistx
      names(funclistx1) <- paste0(
        "Step ",
        seq_along(funclistx),
        ": ",
        names(funclistx)
      )

      steps_formatted <- sapply(seq_along(funclistx1), function(i) {
        step_name <- names(funclistx1)[i]
        step_code <- funclistx1[[i]]
        paste0("# ", step_name, "\n", step_code, "\n")
      })

      oneclick_user_message <- paste0(
        "Act as a senior bioinformatician and expert R programmer.

Your task is to refine a selected WuKongmini proteomics workflow into a single executable R script.

STRICT RULES:
1. Input objects:
   - inputdata: primary expression matrix or data frame.
   - grnames1: vector of group names.
   - grnum1: total number of groups.
   - grnum2: vector of replicate numbers per group.
   - grnames: sample-level group vector.
   - speciesx: species id/name selected by user.

2. Stepwise chaining:
   - Each step must start with a comment exactly like: # Step N: Short Step Name
   - Each step must clearly define its input object and output object in comments.
   - Each step must use the previous step's major output as its input unless the workflow description explicitly says otherwise.
   - Do not use undefined objects.
   - Do not require manual intervention.

3. Accuracy:
   - Carefully read every selected module's reference code.
   - Preserve the original calculation logic from the reference code.
   - Respect all requirements in WORKFLOW DESCRIPTION.
   - If the workflow says to use only differentially expressed proteins for PCA/HCA/enrichment, subset accordingly.
   - If the workflow says to use all proteins for GSEA, do not subset only DEPs.

4. KEGG Enrichment Analysis species handling:
   - This rule applies ONLY when the selected modules include \"KEGG Enrichment Analysis\".
   - For all other modules, including GO enrichment modules, keep the original reference-code behavior unless the workflow description explicitly requires otherwise.
   - The user-facing object speciesx may be a species string such as:
     * \"9606-Homo sapiens\"
     * \"10090-Mus musculus\"
     * \"10116-Rattus norvegicus\"
     * \"3702-Arabidopsis thaliana\"
     * \"4932-Saccharomyces cerevisiae\"
     * \"559292-Saccharomyces cerevisiae\"
     * or it may already be a KEGG organism code such as \"hsa\".
   - When generating or refining code for KEGG Enrichment Analysis, do NOT directly pass speciesx in formats such as \"9606-Homo sapiens\" to KEGG functions.
   - For KEGG Enrichment Analysis, infer a KEGG organism code from speciesx and use that KEGG code in KEGG-related function arguments.
   - Use a separate local object such as idselect instead of overwriting the original speciesx.
   - The original speciesx object must remain unchanged.
   - Common mappings:
     * \"9606-Homo sapiens\", \"9606\", or \"Homo sapiens\" -> \"hsa\"
     * \"10090-Mus musculus\", \"10090\", or \"Mus musculus\" -> \"mmu\"
     * \"10116-Rattus norvegicus\", \"10116\", or \"Rattus norvegicus\" -> \"rno\"
     * \"7227-Drosophila melanogaster\", \"7227\", or \"Drosophila melanogaster\" -> \"dme\"
     * \"6239-Caenorhabditis elegans\", \"6239\", or \"Caenorhabditis elegans\" -> \"cel\"
     * \"7955-Danio rerio\", \"7955\", or \"Danio rerio\" -> \"dre\"
     * \"9031-Gallus gallus\", \"9031\", or \"Gallus gallus\" -> \"gga\"
     * \"9823-Sus scrofa\", \"9823\", or \"Sus scrofa\" -> \"ssc\"
     * \"9913-Bos taurus\", \"9913\", or \"Bos taurus\" -> \"bta\"
     * \"9615-Canis lupus familiaris\", \"9615\", or \"Canis lupus familiaris\" -> \"cfa\"
     * \"9544-Macaca mulatta\", \"9544\", or \"Macaca mulatta\" -> \"mcc\"
     * \"3702-Arabidopsis thaliana\", \"3702\", or \"Arabidopsis thaliana\" -> \"ath\"
     * \"39947-Oryza sativa Japonica Group\", \"39947\", or \"Oryza sativa\" -> \"osa\"
     * \"4577-Zea mays\", \"4577\", or \"Zea mays\" -> \"zma\"
     * \"4932-Saccharomyces cerevisiae\", \"559292-Saccharomyces cerevisiae\", \"4932\", \"559292\", or \"Saccharomyces cerevisiae\" -> \"sce\"
     * \"511145-Escherichia coli str. K-12 substr. MG1655\", \"511145\", or \"Escherichia coli\" -> \"eco\"
   - If speciesx is already a valid KEGG organism code, such as \"hsa\", \"mmu\", \"rno\", \"dme\", \"cel\", \"dre\", \"gga\", \"ssc\", \"bta\", \"cfa\", \"mcc\", \"ath\", \"osa\", \"zma\", \"sce\", or \"eco\", use it unchanged as idselect.
   - If the KEGG organism code cannot be inferred, stop with a clear error message that includes the original speciesx value.
   - Do not modify GO enrichment species handling in this rule.

5. Robustness:
   - Include required library calls.
   - Important: because WuKongmini may execute each step independently, include the package-loading statements needed by each step either in the global preamble and, when necessary, also inside the corresponding step.
   - Prefer suppressPackageStartupMessages(library(packageName)) for package loading.
   - Use safe object names.
   - Check and handle missing values where appropriate.
   - Return a meaningful object at the end of each step so Shiny can display it.
   - For plots, ensure the ggplot/pheatmap/plot object is returned.
   - For tables, ensure a data.frame or matrix is returned.

6. Output format:
   - First provide concise bullet explanations for:
     inputdata, grnames1, grnum1, grnum2, grnames, speciesx.
   - Then provide only one complete R code block.
   - The code block must contain all workflow steps.
   - Do not include non-R text inside the code block except R comments.

WORKFLOW DESCRIPTION:
",
        workflow_desc %||% "[No workflow description provided]",
        "

SELECTED MODULES:
",
        paste(seq_along(selx), selx, sep = ". ", collapse = "\n"),
        "

REFERENCE CODES:
",
        paste(steps_formatted, collapse = "\n\n")
      )

      oneclick_user_messagexx <<- oneclick_user_message
      oneclick_messagesx <- list(
        list(
          role = "user",
          content = oneclick_user_message
        )
      )

      selected_oneclick_model <- get_selected_model_name(
        backend = input$oneclick_llm_backend,
        deepseek_model = input$deepseek_model_oneclick,
        kimi_model = input$kimi_model_oneclick,
        openai_model = input$openai_model_oneclick,
        ollama_model = input$llmmodeloneclick,
        ollama_model_mode = input$oneclick_ollama_model_mode %||% "registered",
        ollama_custom_model = input$oneclick_ollama_custom_model %||% ""
      )

      selected_oneclick_max_ctx <- get_model_max_context(
        input$oneclick_llm_backend,
        selected_oneclick_model
      )

      selected_oneclick_num_predict <- get_model_default_num_predict(
        input$oneclick_llm_backend,
        selected_oneclick_model
      )

      oneclick_response_message <- tryCatch({
        call_llm(
          backend = input$oneclick_llm_backend,
          messages = oneclick_messagesx,
          deepseek_api_key = input$deepseek_api_key_oneclick,
          deepseek_model = input$deepseek_model_oneclick,
          kimi_api_key = input$kimi_api_key_oneclick,
          kimi_model = input$kimi_model_oneclick,
          openai_api_key = input$openai_api_key_oneclick,
          openai_model = input$openai_model_oneclick,
          ollama_model = selected_oneclick_model,
          temperature = input$oneclick_temperature,
          num_predict = selected_oneclick_num_predict,
          num_ctx = selected_oneclick_max_ctx,
          reasoning_effort = "high",
          thinking_enabled = TRUE
        )
      }, error = function(e) {
        paste0("LLM 调用失败：", e$message)
      })

      oneclick_response_messagex <<- oneclick_response_message

      refined_code <- extract_r_code(oneclick_response_message)
      oneclick_refined_code_text(refined_code)

      output$oneclick_prompts <- renderUI({
        aceEditor(
          outputId = "rcode",
          value = oneclick_response_message,
          mode = "r",
          theme = "chrome",
          readOnly = TRUE,
          height = "620px",
          fontSize = 14
        )
      })

      code_text <- refined_code
      code_textx <<- code_text

      if (!nzchar(code_text) || grepl("^LLM 调用失败", code_text)) {
        output$oneclick_results <- renderUI({
          tags$pre(style = "color:red;", code_text)
        })
        return(NULL)
      }

      # ============================================================
      # Split refined R code into preamble and stepwise executable blocks
      # ============================================================
      # Important optimization:
      # The LLM often places library() / require() calls at the top of the script,
      # before '# Step 1:'. If we only split from '# Step 1:', those package-loading
      # statements will be discarded, causing errors such as:
      # 'could not find function impute.knn'.
      #
      # Therefore, we extract the code before the first '# Step N:' as preamble_code,
      # execute it once in envx, and also prepend it to each step execution.
      # This makes every step robust when executed independently.

      step_locs <- gregexpr("# Step [0-9]+:", code_text, perl = TRUE)[[1]]

      preamble_code <- ""

      if (length(step_locs) == 1 && step_locs[1] == -1) {
        step_locs <- 1
        n_steps <- 1
        step_splits <- code_text
      } else {
        if (step_locs[1] > 1) {
          preamble_code <- substr(code_text, 1, step_locs[1] - 1)
        }

        n_steps <- length(step_locs)
        step_splits <- character(n_steps)

        for (i in seq_along(step_locs)) {
          start_pos <- step_locs[i]
          end_pos <- if (i < n_steps) {
            step_locs[i + 1] - 1
          } else {
            nchar(code_text)
          }

          step_splits[i] <- substr(code_text, start_pos, end_pos)
        }
      }

      step_locsx <<- step_locs
      step_splitsx <<- step_splits
      preamble_codex <<- preamble_code

      # ============================================================
      # Create execution environment
      # ============================================================

      envx <- new.env(parent = globalenv())

      envx$inputdata <- inputdata
      envx$grnames1 <- grnames1
      envx$grnum1 <- grnum1
      envx$grnum2 <- grnum2
      envx$grnames <- grnames
      envx$speciesx <- speciesx

      envx$WKfuncslist1 <- WKfuncslist1
      envx$selected_modules <- selx
      envx$workflow_description <- workflow_desc

      # ============================================================
      # Execute preamble code once before stepwise execution
      # ============================================================

      preamble_result <- NULL
      preamble_error <- NULL

      if (nzchar(trimws(preamble_code))) {
        preamble_result <- tryCatch({
          eval(parse(text = preamble_code), envir = envx)
        }, error = function(e) {
          preamble_error <<- e$message
          NULL
        })
      }

      step_results <- list()
      step_titles <- list()
      step_codes <- list()
      results_ui <- list()
      result_summaries_for_llm <- list()

      if (!is.null(preamble_error)) {
        result_summaries_for_llm[[length(result_summaries_for_llm) + 1]] <- paste0(
          "Preamble execution warning/error:\n",
          preamble_error,
          "\n\n",
          "This may affect downstream steps if required packages or helper functions were not loaded."
        )
      }

      # ============================================================
      # Execute each workflow step
      # ============================================================

      for (i in seq_along(step_splits)) {
        step_code <- step_splits[i]
        step_code_lines <- unlist(strsplit(step_code, "\n"))

        step_title <- grep("^# Step [0-9]+:", step_code_lines, value = TRUE)
        step_title <- if (length(step_title) > 0) {
          step_title[1]
        } else {
          paste("Step", i)
        }

        lines_no_step <- step_code_lines[
          !grepl("^# Step [0-9]+:", step_code_lines)
        ]

        code_body <- paste(lines_no_step, collapse = "\n")

        executable_step_code <- paste(
          preamble_code,
          "\n\n",
          code_body,
          sep = ""
        )

        result <- tryCatch({
          eval(parse(text = executable_step_code), envir = envx)
        }, error = function(e) {
          structure(
            list(
              error = TRUE,
              message = e$message,
              step_title = step_title,
              step_code = step_code,
              executable_step_code = executable_step_code
            ),
            class = "oneclick_error"
          )
        })

        step_results[[i]] <- result
        step_titles[[i]] <- step_title
        step_codes[[i]] <- step_code

        result_summaries_for_llm[[i]] <- paste0(
          step_title,
          "\n",
          summarize_result_object(result)
        )
      }

      oneclick_step_results(step_results)
      oneclick_step_titles(step_titles)
      oneclick_step_codes(step_codes)

      workflow_descx <<- workflow_desc
      selxx <<- selx
      code_textx <<- code_text
      step_resultsx <<- step_results
      # ============================================================
      # Generate bilingual scientific interpretation
      # ============================================================

      interpretation_prompt <- paste0(
        "You are a senior multi-omics bioinformatician.

Based on the following WuKongmini workflow, selected modules, workflow description, refined R code, and stepwise outputs, write detailed bilingual interpretations.

Requirements:
1. For each module/step:
   - Provide an English explanation.
   - Provide a Chinese explanation.
   - Explain what the step does, what the result means, and how it informs downstream analysis.
2. Be scientifically rigorous and suitable for a Results section.
3. If a step generated an error, explain the likely reason and how to fix it.
4. Do not invent numeric findings that are not present in the outputs.
5. Use clear markdown headings.

Workflow Description:
",
        workflow_desc %||% "[No workflow description provided]",
        "

Selected Modules:
",
        paste(seq_along(selx), selx, sep = ". ", collapse = "\n"),
        "

Refined R Code:
```r
",
        code_text,
        "
Stepwise Outputs:
",
        paste(unlist(result_summaries_for_llm), collapse = "\n\n---\n\n")
      )
      interpretation_promptx <<- interpretation_prompt

      interpretation_response <- tryCatch({
        call_llm(
          backend = input$oneclick_llm_backend,
          messages = list(
            list(
              role = "user",
              content = interpretation_prompt
            )
          ),
          deepseek_api_key = input$deepseek_api_key_oneclick,
          deepseek_model = input$deepseek_model_oneclick,
          kimi_api_key = input$kimi_api_key_oneclick,
          kimi_model = input$kimi_model_oneclick,
          openai_api_key = input$openai_api_key_oneclick,
          openai_model = input$openai_model_oneclick,
          ollama_model = selected_oneclick_model,
          temperature = 0.2,
          num_predict = selected_oneclick_num_predict,
          num_ctx = selected_oneclick_max_ctx,
          reasoning_effort = "high",
          thinking_enabled = TRUE
        )
      }, error = function(e) {
        paste0("结果解读生成失败：", e$message)
      })

      interpretation_responsex <<- interpretation_response
      oneclick_summary_text(interpretation_response)

      # ============================================================
      # Render stepwise results
      # ============================================================

      for (i in seq_along(step_results)) {
        local({
          my_i <- i
          result <- step_results[[my_i]]
          my_title <- step_titles[[my_i]]

          outputId_plot <- paste0("oneclick_plot_", my_i)
          outputId_table <- paste0("oneclick_table_", my_i)
          outputId_text <- paste0("oneclick_text_", my_i)

          if (is_ggplot_object(result)) {
            output[[outputId_plot]] <- renderPlot({
              print(result)
            })

            results_ui[[my_i]] <<- tagList(
              tags$div(
                style = "background:#F7F5F0;border-left:5px solid #1F4E5F;border-radius:10px;padding:12px;margin-bottom:18px;",
                tags$h5(
                  my_title,
                  style = "color:#1F4E5F;font-weight:bold;"
                ),
                plotOutput(outputId_plot, height = "420px")
              )
            )
          } else if (is_pheatmap_object(result)) {
            output[[outputId_plot]] <- renderPlot({
              print(result)
            })

            results_ui[[my_i]] <<- tagList(
              tags$div(
                style = "background:#F7F5F0;border-left:5px solid #1F4E5F;border-radius:10px;padding:12px;margin-bottom:18px;",
                tags$h5(
                  my_title,
                  style = "color:#1F4E5F;font-weight:bold;"
                ),
                plotOutput(outputId_plot, height = "420px")
              )
            )
          } else if (is.data.frame(result) || is.matrix(result)) {
            output[[outputId_table]] <- renderDataTable({
              datatable(
                as.data.frame(result),
                options = list(pageLength = 8, scrollX = TRUE)
              )
            })

            results_ui[[my_i]] <<- tagList(
              tags$div(
                style = "background:#FFFDF8;border-left:5px solid #B55245;border-radius:10px;padding:12px;margin-bottom:18px;",
                tags$h5(
                  my_title,
                  style = "color:#1F4E5F;font-weight:bold;"
                ),
                dataTableOutput(outputId_table)
              )
            )
          } else if (inherits(result, "oneclick_error")) {
            results_ui[[my_i]] <<- tagList(
              tags$div(
                style = "background:#FFF1F0;border-left:5px solid #B55245;border-radius:10px;padding:12px;margin-bottom:18px;",
                tags$h5(
                  my_title,
                  style = "color:#8A1F11;font-weight:bold;"
                ),
                tags$pre(
                  style = "color:#8A1F11;white-space:pre-wrap;",
                  paste("Error:", result$message)
                )
              )
            )
          } else {
            output[[outputId_text]] <- renderUI({
              tags$pre(
                paste(
                  capture.output(print(result)),
                  collapse = "\n"
                )
              )
            })

            results_ui[[my_i]] <<- tagList(
              tags$div(
                style = "background:#FFFDF8;border-left:5px solid #7A9E7E;border-radius:10px;padding:12px;margin-bottom:18px;",
                tags$h5(
                  my_title,
                  style = "color:#1F4E5F;font-weight:bold;"
                ),
                uiOutput(outputId_text)
              )
            )
          }
        })
      }

      interpretation_html <- HTML(
        commonmark::markdown_html(
          interpretation_response,
          hardbreaks = TRUE
        )
      )

      summary_ui <- tags$div(
        style = "background:#F7F5F0;border-radius:12px;margin-bottom:22px;padding:22px 26px;box-shadow:0 4px 14px rgba(31,78,95,0.12);border:1px solid #D8D2C4;",
        tags$h4(
          "LLM-Generated Bilingual Scientific Interpretation",
          style = "color:#1F4E5F;font-size:26px;font-weight:bold;"
        ),
        tags$div(
          interpretation_html,
          style = "font-size:15px;color:#374151;line-height:1.7;"
        )
      )

      output$oneclick_results <- renderUI({
        do.call(tagList, c(list(summary_ui), results_ui))
      })

      # ============================================================
      # Build HTML report
      # ============================================================

      report_html <- paste0(
        "<!DOCTYPE html>
<html> <head> <meta charset='UTF-8'> <title>WuKongmini Workflow Report</title> <style> body { font-family: Arial, 'Microsoft YaHei', sans-serif; margin: 32px; background: #F7F5F0; color: #374151; } h1 { color: #1F4E5F; border-bottom: 3px solid #1F4E5F; padding-bottom: 10px; } h2, h3 { color: #1F4E5F; } pre { background: #FFFDF8; border: 1px solid #D8D2C4; border-radius: 8px; padding: 14px; overflow-x: auto; white-space: pre-wrap; } table { border-collapse: collapse; width: 100%; margin-bottom: 20px; background: white; } th, td { border: 1px solid #D8D2C4; padding: 8px 10px; } th { background: #E7F0F2; color: #1F4E5F; } .section { background: #FFFDF8; border-radius: 12px; padding: 20px; margin-bottom: 22px; box-shadow: 0 3px 12px rgba(31,78,95,0.12); border: 1px solid #D8D2C4; } </style> </head> <body> <h1>WuKongmini: Workflow Analysis Report</h1> <div class='section'> <h2>Workflow Description</h2> <pre>", htmlEscape(workflow_desc %||% ""), "</pre> </div> <div class='section'> <h2>Selected Modules</h2> <ol>", paste0("<li>", htmlEscape(selx), "</li>", collapse = ""), "</ol> </div> <div class='section'> <h2>Refined R Code</h2> <pre>", htmlEscape(code_text), "</pre> </div> <div class='section'> <h2>Bilingual Scientific Interpretation</h2>", commonmark::markdown_html( interpretation_response, hardbreaks = TRUE ), "</div> <div class='section'> <h2>Stepwise Result Summary</h2> <pre>", htmlEscape( paste( unlist(result_summaries_for_llm), collapse = "\n\n---\n\n" ) ), "</pre> </div> </body> </html>" )
      oneclick_report_html(report_html)
    })

    # ============================================================
    # WuKongmini download all results
    # ============================================================

    output$oneclick_download_results <- downloadHandler(
      filename = function() {
        paste0(
          "WuKongmini_Results_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".zip"
        )
      },

      content = function(file) {
        tmpdir <- tempfile("WuKongmini_results_")
        dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

        oldwd <- setwd(tmpdir)
        on.exit(setwd(oldwd), add = TRUE)

        results <- oneclick_step_results()
        titles <- oneclick_step_titles()
        codes <- oneclick_step_codes()
        interpretation <- oneclick_summary_text()
        report_html <- oneclick_report_html()
        refined_code <- oneclick_refined_code_text()

        file_list <- c()

        code_file <- "WuKongmini_refined_R_code.R"
        writeLines(refined_code %||% "", code_file, useBytes = TRUE)
        file_list <- c(file_list, code_file)

        interpretation_file <- "WuKongmini_bilingual_interpretation.txt"
        writeLines(interpretation %||% "", interpretation_file, useBytes = TRUE)
        file_list <- c(file_list, interpretation_file)

        html_file <- "WuKongmini_Report.html"
        writeLines(
          report_html %||%
            "<html><body><h1>No report available</h1></body></html>",
          html_file,
          useBytes = TRUE
        )
        file_list <- c(file_list, html_file)

        if (length(results) > 0) {
          for (i in seq_along(results)) {
            res <- results[[i]]
            title <- if (length(titles) >= i) {
              titles[[i]]
            } else {
              paste0("Step_", i)
            }

            title_clean <- safe_filename(title)

            step_code_file <- paste0(
              "Step",
              i,
              "_",
              title_clean,
              "_code.R"
            )

            writeLines(
              codes[[i]] %||% "",
              step_code_file,
              useBytes = TRUE
            )

            file_list <- c(file_list, step_code_file)

            if (inherits(res, "oneclick_error")) {
              fname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                "_error.txt"
              )

              writeLines(
                paste("Error:", res$message),
                fname,
                useBytes = TRUE
              )

              file_list <- c(file_list, fname)

            } else if (is.data.frame(res) || is.matrix(res)) {
              fname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                ".csv"
              )

              write.csv(
                as.data.frame(res),
                fname,
                row.names = TRUE,
                fileEncoding = "UTF-8"
              )

              file_list <- c(file_list, fname)

              txtname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                "_summary.txt"
              )

              capture.output({
                cat("Result type: table/data.frame\n")
                cat(
                  "Dimensions:",
                  nrow(as.data.frame(res)),
                  "x",
                  ncol(as.data.frame(res)),
                  "\n\n"
                )
                print(utils::head(as.data.frame(res), 20))
              }, file = txtname)

              file_list <- c(file_list, txtname)

            } else if (is_ggplot_object(res)) {
              pdfname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                ".pdf"
              )

              pngname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                "_300dpi.png"
              )

              ggplot2::ggsave(
                pdfname,
                plot = res,
                width = 8,
                height = 6,
                units = "in"
              )

              ggplot2::ggsave(
                pngname,
                plot = res,
                width = 8,
                height = 6,
                units = "in",
                dpi = 300
              )

              file_list <- c(file_list, pdfname, pngname)

            } else if (is_pheatmap_object(res)) {
              pdfname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                ".pdf"
              )

              pngname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                "_300dpi.png"
              )

              pdf(pdfname, width = 8, height = 6)
              print(res)
              dev.off()

              png(
                pngname,
                width = 8,
                height = 6,
                units = "in",
                res = 300
              )

              print(res)
              dev.off()

              file_list <- c(file_list, pdfname, pngname)

            } else if (inherits(res, "recordedplot")) {
              pdfname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                ".pdf"
              )

              pngname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                "_300dpi.png"
              )

              pdf(pdfname, width = 8, height = 6)
              replayPlot(res)
              dev.off()

              png(
                pngname,
                width = 8,
                height = 6,
                units = "in",
                res = 300
              )

              replayPlot(res)
              dev.off()

              file_list <- c(file_list, pdfname, pngname)

            } else {
              fname <- paste0(
                "Step",
                i,
                "_",
                title_clean,
                ".txt"
              )

              capture.output(print(res), file = fname)
              file_list <- c(file_list, fname)
            }
          }
        } else {
          notefile <- "README.txt"

          writeLines(
            "No results found. Please run WuKongmini workflow first.",
            notefile
          )

          file_list <- c(file_list, notefile)
        }

        zip::zip(zipfile = file, files = file_list)
      }
    )
    # ============================================================
    # Conversation module
    # ============================================================

    observeEvent(input$example_clicked, {
      updateTextAreaInput(
        session,
        "user_input",
        value = "I want to do PCA analysis and How could I do in this platform?"
      )
    })

    chat_history <- reactiveVal(list())
    code_result <- reactiveVal(NULL)

    observeEvent(input$send, {
      user_message <- input$user_input

      if (is.null(user_message) || trimws(user_message) == "") {
        return(NULL)
      }

      chat_history(
        c(
          chat_history(),
          list(
            list(
              role = "user",
              content = user_message
            )
          )
        )
      )

      messagesx <- chat_history()
      messagesx_global <<- messagesx

      selected_chat_model <- get_selected_model_name(
        backend = input$chat_llm_backend,
        deepseek_model = input$deepseek_model_chat,
        kimi_model = input$chat_kimi_model,
        openai_model = input$chat_openai_model,
        ollama_model = input$llmmodel,
        ollama_model_mode = input$chat_ollama_model_mode %||% "registered",
        ollama_custom_model = input$chat_ollama_custom_model %||% ""
      )

      selected_chat_max_ctx <- get_model_max_context(
        input$chat_llm_backend,
        selected_chat_model
      )

      selected_chat_num_predict <- get_model_default_num_predict(
        input$chat_llm_backend,
        selected_chat_model
      )

      response_message <- tryCatch({
        call_llm(
          backend = input$chat_llm_backend,
          messages = messagesx,
          deepseek_api_key = input$deepseek_api_key_chat,
          deepseek_model = input$deepseek_model_chat,
          kimi_api_key = input$chat_kimi_api_key,
          kimi_model = input$chat_kimi_model,
          openai_api_key = input$chat_openai_api_key,
          openai_model = input$chat_openai_model,
          ollama_model = selected_chat_model,
          temperature = 0.2,
          num_predict = selected_chat_num_predict,
          num_ctx = selected_chat_max_ctx,
          reasoning_effort = "high",
          thinking_enabled = TRUE
        )
      }, error = function(e) {
        paste0("LLM 调用失败：", e$message)
      })

      response_messagex <<- response_message

      chat_history(
        c(
          chat_history(),
          list(
            list(
              role = "assistant",
              content = response_message
            )
          )
        )
      )

      convert_to_html <- function(text) {
        markdownToHTML(
          text = text,
          fragment.only = TRUE
        )
      }

      new_chat <- lapply(chat_history(), function(x) {
        if (x$role == "user") {
          paste0(
            '<div class="user"><div class="message"><strong>User:</strong> ',
            convert_to_html(x$content),
            '</div></div>'
          )
        } else {
          paste0(
            '<div class="assistant"><div class="message"><strong>Assistant:</strong> ',
            convert_to_html(x$content),
            '</div></div>'
          )
        }
      })

      updateTextAreaInput(session, "user_input", value = "")

      output$chat_output <- renderUI({
        HTML(paste(new_chat, collapse = "\n"))
      })

      # Automatically detect and execute R code blocks in the assistant response
      if (grepl("```R|```r|```", response_message)) {
        r_code <- extract_r_code(response_message)
        r_codex <<- r_code

        result <- tryCatch({
          eval(parse(text = r_code))
        }, error = function(e) {
          paste("Error in code:", e$message)
        })

        code_result(result)
      } else {
        code_result(NULL)
      }

      # Keep compatibility with original code_output, if it exists in downstream UI/module
      output$code_output <- renderUI({
        code_resultx <<- code_result()

        if (is_ggplot_object(code_result())) {
          plotOutput("plot_result")
        } else if (is.data.frame(code_result()) || is.matrix(code_result())) {
          dataTableOutput("data_result")
        } else {
          HTML(paste("<pre>", code_result(), "</pre>"))
        }
      })

      output$plot_result <- renderPlot({
        if (!is.null(code_result()) && is_ggplot_object(code_result())) {
          print(code_result())
        }
      })

      plot_resultout <- reactive({
        if (!is.null(code_result()) && is_ggplot_object(code_result())) {
          code_result()
        }
      })

      output$data_result <- renderDataTable({
        if (!is.null(code_result()) &&
            (is.data.frame(code_result()) || is.matrix(code_result()))) {
          datatable(
            as.data.frame(code_result()),
            options = list(pageLength = 10, scrollX = TRUE)
          )
        }
      })

      data_resultout <- reactive({
        if (!is.null(code_result()) &&
            (is.data.frame(code_result()) || is.matrix(code_result()))) {
          code_result()
        }
      })

      output$DivergingBarllmplotdl <- downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

          if (is_ggplot_object(code_result())) {
            paste0("Adjusted.by.LLM_Bar.plot_", timestamp, ".pdf")
          } else {
            paste0("Adjusted.by.LLM_Bar.table_", timestamp, ".csv")
          }
        },

        content = function(file) {
          if (is_ggplot_object(code_result())) {
            pdf(file, width = 10, height = 10)
            print(plot_resultout())
            dev.off()
          } else {
            write.csv(data_resultout(), file)
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
