# WuKong <img src="wukonglogo.png" align="right" height="120" width="135"/>

WuKong: An LLM-Enhanced Framework for Adaptable Proteomics Analysis

## Brief Description

**WuKong** is an open-source and LLM-enhanced platform designed for adaptable proteomics data analysis. It integrates more than 72 modular analytical tools with multiple large language model (LLM) backends, enabling researchers to perform data preprocessing, statistical analysis, functional annotation, data visualization, and workflow design.

WuKong was developed to bridge the analytical gap between high-throughput proteomics data generation and biological interpretation. Modern mass spectrometry-based proteomics can generate increasingly large and complex datasets, but downstream analysis often requires coding skills, statistical expertise, and manual integration of multiple software tools. WuKong addresses this challenge by embedding LLMs into a modular, code-constrained R-based analytical framework. Users can translate natural-language instructions into transparent, executable, and reproducible bioinformatic workflows.

Unlike general chatbot-based analysis, WuKong uses a prompt-aware execution strategy. The LLM does not work as an unconstrained code generator; instead, it can refer to internal module-specific R code logic and established analytical procedures. This design improves reproducibility, transparency, and methodological reliability while lowering the computational barrier for experimental researchers.

A key feature of WuKong is its flexible LLM integration. The current version supports both cloud-based LLM APIs and local LLM deployment. Users can select from **DeepSeek API**, **Kimi / Moonshot API**, **OpenAI / ChatGPT API**, or **Local Ollama** backends directly within the Conversation module and WuKongmini workflow designer. This design allows users to balance reasoning capability, cost, speed, privacy, and local hardware availability.

For privacy-sensitive projects, WuKong supports **local LLM deployment** through [Ollama](https://ollama.com/). This allows sensitive proteomics datasets to remain within local computing environments, making WuKong suitable for clinical proteomics, unpublished datasets, hospital-based cohorts, and privacy-sensitive biomedical research. For users who prefer stronger cloud-based reasoning or do not have sufficient local hardware, WuKong can call external LLM APIs after users provide the corresponding API keys in the interface.

WuKong supports both routine proteomics analysis and complex exploratory workflows, including bulk proteomics, single-cell proteomics, phosphoproteomics, pathway interpretation, biomarker discovery, survival analysis, and publication-quality visualization.

## Key Features

- **Comprehensive modular coverage:** WuKong integrates more than 72 analytical modules covering data preprocessing, statistical analysis, functional enrichment, biomarker discovery, visualization, and workflow design.

- **Dual-mode analytical interaction:** Users can perform analyses through conventional GUI-based modules or natural-language interactions powered by cloud-based LLM APIs or locally deployed LLMs.

- **Multi-backend LLM support:** WuKong supports DeepSeek API, Kimi / Moonshot API, OpenAI / ChatGPT API, and local Ollama models. Users can switch backends in the interface according to their analysis needs, privacy requirements, and computing resources.

- **Secure API-key input:** For cloud LLM APIs, users only need to paste the required API key into the corresponding input box in WuKong. The API key is used only for the current request/session and is not saved by WuKong.

- **Prompt-aware architecture:** WuKong allows LLMs to reference internal R code logic, helping natural-language prompts generate precise, transparent, and reproducible outputs.

- **Privacy-preserving local AI:** WuKong supports local LLM backends through Ollama, so sensitive data do not need to be uploaded to external cloud servers.

- **LLM connection testing:** The Conversation module and WuKongmini include built-in AI connection tests. Users can verify the selected backend, model name, default context length, response status, and elapsed time before running analyses.

- **Workflow designer:** WuKongmini enables users to upload data, define groups, select modules, describe workflow requirements, generate refined R scripts, execute analyses step by step, obtain bilingual scientific interpretations, and download results.

## Function Modules

WuKong is organized into six major functional sections.

### 1. Conversation

The Conversation module allows users to chat with either cloud-based or locally deployed LLMs. Users can ask questions about data analysis, statistical methods, proteomics workflows, R code, visualization adjustment, functional interpretation, or WuKong platform usage.

The current Conversation module supports the following LLM backends:

- **DeepSeek API**
- **Kimi / Moonshot API**
- **OpenAI / ChatGPT API**
- **Local Ollama**

Users can select the preferred backend directly in the interface. For API-based backends, users only need to paste the corresponding API key into the password input box in WuKong. The API key is used for model access during the current interaction and is not saved by WuKong. For local Ollama, users can choose from registered models, automatically detected local models, or manually enter a custom local model name.

The module also includes a **Test AI Connection** button. This function sends a lightweight test request to the selected backend and reports whether the connection is successful, which model is being used, the default maximum context length, elapsed time, and the model response or error message.

The Conversation module can also detect R code blocks returned by the assistant and attempt to execute them within the R session, allowing users to iteratively refine analysis code and inspect generated tables or plots.

### 2. Data Pre-processing

WuKong provides preprocessing modules for preparing proteomics data before downstream analysis. These modules help users evaluate technical reproducibility, handle missing values, reduce systematic bias, and format datasets for statistical analysis.

Main modules include:

- Coefficient of variation calculation
- Missing value imputation
- Normalization
- Logarithm with base 2 transformation
- Two-table merging
- Webpage content analysis

### 3. Statistical Analysis

WuKong supports a broad range of statistical and machine-learning methods for proteomics data analysis, including differential expression analysis, clustering, dimensionality reduction, regression, classification, biomarker discovery, and advanced modeling.

Main modules include:

- Consensus clustering
- One-way ANOVA
- Limma differential expression analysis
- SAMR differential expression analysis
- Student's t-test
- Wilcoxon rank sum and signed rank tests
- Dynamic Network Biomarkers
- Factor analysis
- Grey Relational Analysis
- Hierarchical Cluster Analysis
- K-means clustering
- Lack of Fit F-test
- Linear regression
- Logistic regression
- Mfuzz soft clustering
- Network Degree Matrix
- OPLS-DA
- PCA
- PCoA
- PLS-DA
- Power analysis
- Restricted Cubic Spline analysis
- Redundancy analysis
- Rank Rank Hypergeometric Overlap analysis
- Soft independent modelling by class analogy
- Time-course data analysis
- t-SNE
- UMAP
- Tumor purity estimation

### 4. Functional Annotation Analysis

WuKong provides functional interpretation modules to connect protein-level results with biological meaning. These modules support pathway interpretation, biological process annotation, gene/protein function exploration, and enrichment analysis of differentially expressed proteins or ranked protein lists.

Main modules include:

- Cell type annotation
- Exploring gene/protein functions based on GO database
- GO enrichment analysis
- KEGG enrichment analysis
- Gene Set Enrichment Analysis of GO
- Gene Set Enrichment Analysis of KEGG

### 5. Data Visualization

WuKong provides publication-quality visualization modules, mainly based on R plotting ecosystems such as `ggplot2`. Users can create standard statistical plots, complex biological visualizations, network graphs, survival plots, and customized figures with LLM assistance.

Main visualization modules include:

- Barplot
- Bar and point plot
- Box and point plot
- Correlation plot
- Correlation network plot
- Clustering using correlation network
- Contour plot
- Cross error bar plot
- Dendrogram
- Diverging bar plot
- Funnel plot
- Protein/DNA sequence logo plot
- Dendrogram using `ggtree`
- Colored scatter plot
- Histogram and density plot
- Lollipop chart
- Marginal histogram/boxplot
- Nightingale rose diagram
- Pair point line plot
- Pie plot
- Radar chart
- Rain cloud plot
- Rank point plot
- Ridge plot
- ROC plot
- Sankey chart
- Scatter ellipse plot
- Survival analysis
- Ternary plot
- UpSet plot
- Venn plot
- Violin plot
- Volcano plot
- Word cloud plot

### 6. WuKongmini: LLM-assisted Workflow Designer

WuKongmini is an interactive workflow designer for building customized proteomics pipelines.

It supports:

1. Uploading `.xlsx`, `.xls`, `.csv`, or `.txt` files.
2. Defining sample group names and replicate numbers.
3. Selecting ordered modules from preprocessing, statistics, functional annotation, and visualization.
4. Describing workflow requirements in natural language.
5. Selecting an LLM backend from DeepSeek API, Kimi / Moonshot API, OpenAI / ChatGPT API, or Local Ollama.
6. Entering the required API key in WuKong when a cloud LLM API is selected. The API key is not saved by WuKong.
7. Testing the selected AI backend before workflow execution.
8. Using the selected LLM to refine module-specific reference codes into a single executable R script.
9. Running the workflow step by step in an isolated R execution environment.
10. Displaying tables, plots, error messages, and result summaries.
11. Generating bilingual English-Chinese scientific interpretations for each workflow step.
12. Downloading refined R code, stepwise outputs, interpretation text, and an HTML report as a `.zip` file.

WuKongmini uses a prompt-aware and stepwise execution strategy. The selected modules are converted into reference-code blocks, and the LLM is instructed to preserve the original analytical logic, define clear input and output objects for each step, load required packages, and return meaningful tables or plots. This makes the generated workflow more transparent and easier to debug.

For functional annotation, WuKongmini also includes logic to help the LLM handle species information carefully. For example, KEGG enrichment analysis requires KEGG organism codes such as `hsa`, `mmu`, or `rno`, while the user-facing species value may be provided as `9606-Homo sapiens` or another species string.

Two example pipelines are provided in WuKongmini.

Example Pipeline 1:

1. Normalization
2. Log2 transformation
3. Missing value imputation
4. SAMR differential expression analysis
5. Volcano plot
6. PCA using differentially expressed proteins
7. HCA using differentially expressed proteins
8. GO enrichment analysis using differentially expressed proteins
9. KEGG enrichment analysis using differentially expressed proteins

Example Pipeline 2:

1. Normalization
2. Log2 transformation
3. Missing value imputation
4. SAMR differential expression analysis
5. Volcano plot
6. PCA using differentially expressed proteins
7. HCA using differentially expressed proteins
8. Gene Set Enrichment Analysis of GO using all proteins
9. Gene Set Enrichment Analysis of KEGG using all proteins

## Software Manual

A detailed user manual can be found here:

[https://github.com/wangshisheng/WuKong/blob/master/UserManual.pdf](https://github.com/wangshisheng/WuKong/blob/master/UserManual.pdf)

## Preparation for Local Installation

WuKong is developed with R and Shiny. If you want to run WuKong locally, please complete the following preparatory steps.

### 1. Install R

Please install R from:

[https://www.r-project.org/](https://www.r-project.org/)

R version `>= 4.3.0` is recommended.

### 2. Install RStudio

RStudio is recommended for running WuKong locally, especially because WuKong uses `rstudioapi::jobRunScript()` to launch module scripts.

You can download RStudio from:

[https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

### 3. Configure LLM Backends

WuKong supports both cloud API backends and local Ollama backends.

#### Option A: Cloud LLM APIs

WuKong currently supports:

- **DeepSeek API**
- **Kimi / Moonshot API**
- **OpenAI / ChatGPT API**

When using a cloud LLM API, users only need to select the corresponding backend in WuKong and paste the API key into the API-key input box shown in the interface.

The API key is used only to send requests to the selected LLM provider during the current WuKong session. WuKong does **not** save, export, or permanently store the API key. This makes API-based usage convenient while keeping key management safe and transparent.

#### Option B: Local Ollama

WuKong also supports locally deployed LLMs through Ollama.

Please install Ollama from:

[https://ollama.com/](https://ollama.com/)

After installation, please pull at least one local model. For example:

```bash
ollama run qwen3.8:27b
```

You can check installed models by running:

```bash
ollama list
```

WuKong communicates with the local Ollama service through:

```text
http://localhost:11434/api/chat
```

Please make sure the Ollama service is running before selecting the Local Ollama backend in WuKong.

## Supported LLM Backends and Models

WuKong provides an internal LLM model registry and supports four backend types.

| Backend | Default / Registered Models | Access Mode | Notes |
|---|---|---|---|
| DeepSeek API | `deepseek-v4-pro`, `deepseek-v4-flash` | Cloud API | Uses DeepSeek chat completions. WuKong enables high reasoning effort and thinking mode by default where supported. |
| Kimi / Moonshot API | `kimi-k3` | Cloud API | Suitable for bilingual scientific writing, long-context interpretation, and workflow refinement. |
| OpenAI / ChatGPT API | `gpt-5.6-luna` | Cloud API | Called through the OpenAI Responses API. |
| Local Ollama | `qwen3.8:27b` | Local service | Keeps data in the local computing environment. Custom local model names are also supported. |

WuKong assigns model-specific default context lengths and output token limits in its internal registry. Registered cloud models use large context settings, while registered local Ollama models are configured with long-context defaults when supported.

For Local Ollama, WuKong can use:

1. Registered models defined in WuKong.
2. Models detected from the local Ollama service.
3. Custom model names manually entered by the user.

When using a custom Ollama model, the model name must match a model already available in your local Ollama service. You can check available local models with:

```bash
ollama list
```

Smaller models are suitable for lightweight conversation and simple code editing, while larger models usually provide stronger reasoning, code refinement, and workflow planning performance. Large local models may require substantial RAM, VRAM, and CPU/GPU resources.

## Required R Packages

Before running WuKong, please install the required R packages.

Based on the WuKong source code and function modules, the following packages are required or recommended.

### Core Shiny and interface packages

```r
shiny
shinyBS
shinyjqui
shinyjs
shinyAce
DT
markdown
commonmark
htmltools
rmarkdown
knitr
rstudioapi
zip
```

### Data input, output, and preprocessing packages

```r
openxlsx
gdata
writexl
data.table
reshape2
preprocessCore
MSnbase
impute
```

### LLM and API interface packages

```r
ollamar
httr2
jsonlite
```

WuKong uses `httr2` and `jsonlite` to communicate with DeepSeek, Kimi / Moonshot, OpenAI, and local Ollama endpoints. The `ollamar` package is also loaded for compatibility with Ollama-related workflows.

### Statistical analysis packages

```r
limma
samr
ConsensusClusterPlus
Mfuzz
FactoMineR
vegan
Rtsne
umap
ropls
pheatmap
survival
survminer
rms
pROC
```

### Functional annotation packages

```r
clusterProfiler
GO.db
org.Hs.eg.db
UniProt.ws
Seurat
ceLLama
```

Depending on your species of interest, additional organism annotation packages may be required. For example:

```r
org.Mm.eg.db
org.Rn.eg.db
```

### Visualization packages

```r
ggplot2
ggpubr
ggrepel
ggsci
ggdist
ggsankey
ggradar
ggupset
ggExtra
ggcorrplot
ggtree
ggseqlogo
ggtern
wordcloud2
igraph
tidygraph
ggraph
VennDiagram
ComplexUpset
RColorBrewer
cowplot
patchwork
```

Please note that the main WuKong interface directly loads the following packages in the provided source code:

```r
shiny
shinyBS
shinyjqui
openxlsx
gdata
DT
ollamar
markdown
shinyAce
zip
commonmark
preprocessCore
shinyjs
httr2
jsonlite
htmltools
rmarkdown
knitr
```

Other packages are required by individual analysis modules, downstream statistical functions, functional annotation, or visualization scripts.

## One-click Installation of R Packages

You may run the following R code to install and load most required packages.

```r
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

cran_pkgs <- c(
  "shiny",
  "shinyBS",
  "shinyjqui",
  "shinyjs",
  "shinyAce",
  "DT",
  "markdown",
  "commonmark",
  "htmltools",
  "rmarkdown",
  "knitr",
  "openxlsx",
  "gdata",
  "writexl",
  "zip",
  "rstudioapi",
  "data.table",
  "reshape2",
  "ggplot2",
  "ggpubr",
  "ggrepel",
  "ggsci",
  "ggdist",
  "ggradar",
  "ggExtra",
  "ggcorrplot",
  "pheatmap",
  "FactoMineR",
  "vegan",
  "Rtsne",
  "umap",
  "survival",
  "survminer",
  "rms",
  "pROC",
  "igraph",
  "tidygraph",
  "ggraph",
  "VennDiagram",
  "RColorBrewer",
  "cowplot",
  "patchwork",
  "wordcloud2",
  "ggupset",
  "ggtern",
  "ComplexUpset",
  "ollamar",
  "httr2",
  "jsonlite"
)

for (pkg in cran_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

bioc_pkgs <- c(
  "preprocessCore",
  "MSnbase",
  "impute",
  "limma",
  "samr",
  "ConsensusClusterPlus",
  "Mfuzz",
  "ropls",
  "clusterProfiler",
  "GO.db",
  "org.Hs.eg.db",
  "UniProt.ws",
  "Seurat",
  "ggtree",
  "ggseqlogo"
)

for (pkg in bioc_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg, ask = FALSE, update = FALSE)
  }
}

devtools::install_github("CelVoxes/ceLLama")
devtools::install_github("anspiess/propagate")
devtools::install_github("gpli/DNB")
devtools::install_github("Nisus-Liu/GRA")
devtools::install_github("ricardo-bion/ggradar")
devtools::install_github("kunhuo/plotRCS")
devtools::install_github("gavinsimpson/ggvegan")
devtools::install_github("RRHO2/RRHO2", build_opts = c("--no-resave-data", "--no-manual"))
devtools::install_github("davidsjoberg/ggsankey")
devtools::install_github("nicolash2/ggvenn")

```

Please note that some packages may require system-level dependencies, especially on Linux or macOS. If installation fails, please check the error message and install the corresponding system libraries.

## Run WuKong Locally

After completing the preparatory work, you can install and run WuKong locally.

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("wangshisheng/WuKong")

library(WuKong)

WuKong_app()
```

Alternatively, if you have downloaded the source code manually, you may run the Shiny application from the project directory:

```r
setwd("path/to/WuKong")
shiny::runApp()
```

Please replace `"path/to/WuKong"` with the actual local path of the WuKong folder.

## Input Data Format

WuKong supports common tabular data formats, including:

- `.xlsx`
- `.xls`
- `.csv`
- `.txt`

For proteomics expression data, rows usually represent proteins or genes, and columns usually represent samples.

A typical input table should look like:

| Protein ID | Sample1 | Sample2 | Sample3 | Sample4 |
|---|---:|---:|---:|---:|
| Protein_A | 12.5 | 13.2 | 15.1 | 14.8 |
| Protein_B | 8.1 | 7.9 | 10.2 | 9.8 |
| Protein_C | 20.4 | 19.7 | 18.5 | 17.9 |

When uploading data in WuKongmini, users need to specify:

1. Whether the first row contains column names.
2. Whether the first column contains row names.
3. Sample group number and replicate number.
4. Group names.
5. Species information for functional annotation.

For example, if there are two groups and each group has three replicates:

```text
Group and replicate number: 2;3-3
Group names: Control;Experiment
```

If there are three groups and each group has five replicates:

```text
Group and replicate number: 3;5-5-5
Group names: Control;Treatment1;Treatment2
```

## Prompt-aware Usage

WuKong supports prompt-aware interaction with internal module code.

Users can append:

```text
=>
```

or write:

```text
Refer to inner codes
```

at the end of a prompt to guide the LLM to refer to WuKong internal R code logic.

Example:

```text
Please make all point sizes bigger and remove the point labels. =>
```

In this case, WuKong will guide the LLM to identify relevant plotting code such as `geom_point()` and `geom_text()` and modify the parameters accordingly.

This mechanism improves:

- Code relevance
- Reproducibility
- Transparency
- Module-specific customization

## Example Applications

WuKong has been validated using both bulk and single-cell proteomics datasets.

### Bulk Proteomics: Lung Squamous Cell Carcinoma

WuKong was applied to the CPTAC lung squamous cell carcinoma dataset `PDC000234`, containing tumor and adjacent non-tumor tissues.

The workflow included:

1. Data preprocessing
2. Missing value imputation
3. Normalization
4. PCA
5. Differential expression analysis
6. GSEA
7. Survival analysis

WuKong identified **GPRC5A** as significantly upregulated in tumor tissues. LLM-guided interpretation highlighted the G protein-coupled receptor signaling pathway. Kaplan-Meier survival analysis showed that high GPRC5A expression was associated with poor prognosis.

### Single-cell Proteomics: nanoSPLITS Dataset

WuKong was also applied to the nanoSPLITS dataset `MSV000090828`, including untreated C10 cells and cells treated with the CDK1 inhibitor RO-3306.

The workflow included:

1. Quality control
2. Protein and peptide count assessment
3. UMAP dimensionality reduction
4. Protein-level differential analysis
5. Phosphopeptide-level analysis
6. Integrated proteome and phosphoproteome interpretation

WuKong separated untreated and G2/M-arrested cells and identified treatment-associated phosphorylation changes in proteins including **Vimentin** and **hnRNP U**.

## Friendly Suggestions

1. Please use Chrome, Firefox, or Microsoft Edge for the best user experience.
2. RStudio is recommended because WuKong launches module scripts through `rstudioapi::jobRunScript()`.
3. Please configure at least one LLM backend before using LLM-related functions.
4. For cloud LLM APIs, please select the corresponding backend and paste the API key into the WuKong interface. WuKong does not save the API key.
5. For local Ollama, please make sure Ollama is installed and running before selecting the Local Ollama backend.
6. At least one Ollama model should be installed before using Local Ollama.
7. Larger LLMs usually require stronger hardware and more memory.
8. For large proteomics datasets, sufficient RAM is recommended.
9. Use the built-in **Test AI Connection** button before running long WuKongmini workflows.

Suggested minimum hardware:

- RAM: 8 GB or higher
- Disk space: 100 GB or higher
- CPU: modern multi-core processor
- GPU: optional but recommended for large local LLMs

For larger local LLMs such as `qwen3.8:27b`, `gemma3:27b`, higher memory and GPU acceleration are recommended.

## Troubleshooting

### 1. No model is shown in the Ollama model selection box

Please check whether Ollama is installed and running.

Run the following command in terminal:

```bash
ollama list
```

If no model is available, pull a model:

```bash
ollama pull qwen3.8:27b
```

Then restart WuKong.

### 2. Local Ollama connection fails

Please make sure the Ollama service is active.

You can test Ollama using:

```bash
ollama run qwen3.8:27b
```

You can also check the local Ollama API endpoint:

```text
http://localhost:11434/api/tags
```

If WuKong reports a connection error, please check:

- Whether Ollama is installed.
- Whether the Ollama service is running.
- Whether the selected model exists locally.
- Whether the custom model name exactly matches the result of `ollama list`.
- Whether the local firewall or proxy blocks access to `localhost:11434`.

### 3. Cloud API connection fails

Please check:

- Whether the selected backend is correct.
- Whether the API key was correctly pasted into the corresponding WuKong input box.
- Whether the API key is valid.
- Whether the selected model name is supported by your account or endpoint.
- Whether your network can access the API provider.
- Whether the account has sufficient quota or billing access.

WuKong does not save the API key entered by the user. The built-in **Test AI Connection** button can help diagnose backend, model, context length, elapsed time, and error messages.

### 4. `ollamar::list_models()` returns an error

Please make sure the Ollama service is active.

You can test Ollama using:

```bash
ollama run qwen3.8:27b
```

Although WuKong now communicates with Ollama mainly through HTTP requests to the local Ollama API, `ollamar` is still loaded for compatibility with Ollama-related workflows.

### 5. A module cannot be launched

WuKong uses:

```r
rstudioapi::jobRunScript()
```

to launch module scripts. Please run WuKong in RStudio and make sure the WuKong package has been correctly installed.

### 6. Some R packages cannot be installed

Some Bioconductor packages require compatible R and Bioconductor versions.

Please install or update Bioconductor first:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install()
```

Then install the failed packages one by one to identify dependency issues.

### 7. Functional annotation fails

Please check:

- Whether the selected species is correct.
- Whether the input IDs are compatible with the selected annotation database.
- Whether `clusterProfiler`, `GO.db`, `UniProt.ws`, and organism annotation packages are installed.
- Whether KEGG-related analysis uses a valid KEGG organism code, such as `hsa`, `mmu`, or `rno`.

### 8. The workflow designer produces code but no result

Please check:

- Whether input data were uploaded correctly.
- Whether sample group numbers match the number of sample columns.
- Whether selected modules are logically connected.
- Whether the generated R code contains a complete code block marked by triple backticks.
- Whether all required packages for the selected modules are installed.
- Whether the selected LLM backend returned valid R code instead of an API error message.
- Whether package-loading statements are included in the generated workflow code.

### 9. WuKongmini result interpretation fails

WuKongmini generates bilingual scientific interpretation through a second LLM request after stepwise execution. If interpretation fails, please check:

- Whether the selected LLM backend is still reachable.
- Whether the model context length is sufficient for the workflow code and result summaries.
- Whether the API quota or local model memory is sufficient.
- Whether the stepwise output is extremely large.

For large workflows, using a stronger long-context model is recommended.

## Contact

You can submit issues through this GitHub repository.

For further assistance, please contact:

Shisheng Wang: [wssomics@outlook.com](mailto:wssomics@outlook.com)
