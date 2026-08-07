# WuKong <img src="wukonglogo.png" align="right" height="120" width="135"/>

WuKong: an LLM-enhanced framework for adaptable, reproducible, and privacy-preserving proteomics analysis

## Brief Description

**WuKong** is an open-source and LLM-enhanced platform designed for adaptable proteomics data analysis. It integrates more than 72 modular analytical tools with locally deployed large language models (LLMs), enabling researchers to perform data preprocessing, statistical analysis, functional annotation, data visualization, and workflow design through either graphical modules or natural-language interactions.

WuKong was developed to bridge the analytical gap between high-throughput proteomics data generation and biological interpretation. Modern mass spectrometry-based proteomics can generate increasingly large and complex datasets, but downstream analysis often requires coding skills, statistical expertise, and manual integration of multiple software tools. WuKong addresses this challenge by embedding LLMs into a modular, code-constrained R-based analytical framework. Users can translate natural-language instructions into transparent, executable, and reproducible bioinformatic workflows.

Unlike general chatbot-based analysis, WuKong uses a prompt-aware execution strategy. The LLM does not work as an unconstrained code generator; instead, it can refer to internal module-specific R code logic and established analytical procedures. This design improves reproducibility, transparency, and methodological reliability while lowering the computational barrier for experimental researchers.

A key feature of WuKong is its support for **local LLM deployment** through [Ollama](https://ollama.com/). This allows sensitive proteomics datasets to remain within local computing environments, making WuKong suitable for clinical proteomics, unpublished datasets, hospital-based cohorts, and privacy-sensitive biomedical research.

WuKong supports both routine proteomics analysis and complex exploratory workflows, including bulk proteomics, single-cell proteomics, phosphoproteomics, pathway interpretation, biomarker discovery, survival analysis, and publication-quality visualization.

## Key Features

- **Comprehensive modular coverage:** WuKong integrates more than 72 analytical modules covering data preprocessing, statistical analysis, functional enrichment, biomarker discovery, visualization, and workflow design.

- **Dual-mode analytical interaction:** Users can perform analyses through conventional GUI-based modules or natural-language interactions powered by locally deployed LLMs.

- **Prompt-aware architecture:** WuKong allows LLMs to reference internal R code logic, helping natural-language prompts generate precise, transparent, and reproducible outputs.

- **Privacy-preserving local AI:** WuKong supports local LLM backends through Ollama, so sensitive data do not need to be uploaded to external cloud servers.

- **Workflow designer:** WuKongmini enables users to upload data, define groups, select modules, describe workflow requirements, generate refined R scripts, execute analyses, and download results.

## Function Modules

WuKong is organized into six major functional sections.

### 1. Conversation

The Conversation module allows users to chat with locally deployed LLMs. Users can ask questions about data analysis, statistical methods, proteomics workflows, R code, visualization adjustment, functional interpretation, or WuKong platform usage.

The available LLMs are automatically detected from locally installed Ollama models.

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
5. Using local LLMs to refine module-specific reference codes into a single executable R script.
6. Running the workflow step by step.
7. Displaying tables, plots, and LLM-generated scientific summaries.
8. Downloading workflow results as a `.zip` file.

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

### 3. Install Ollama

WuKong uses locally deployed LLMs through Ollama.

Please install Ollama from:

[https://ollama.com/](https://ollama.com/)

After installation, please pull at least one supported local model. For example:

```bash
ollama pull gemma3:4b
```

or

```bash
ollama pull qwen3:4b
```

or

```bash
ollama pull deepseek-r1:7b
```

You can check installed models by running:

```bash
ollama list
```

WuKong will automatically detect available local Ollama models through the `ollamar` R package.

## Supported Local LLMs

WuKong supports multiple locally deployed LLM backends through Ollama.

Examples include:

| Model family | Supported models |
|---|---|
| Gemma | `gemma3:4b`, `gemma3:27b`, `gemma2:9b`, `gemma2:27b` |
| DeepSeek | `deepseek-r1:7b`, `deepseek-r1:32b` |
| Qwen | `qwen3:4b`, `qwen3:30b` |
| Phi | `phi4:14b` |
| Devstral | `devstral:24b` |

Smaller models are suitable for lightweight tasks, while larger models usually provide stronger reasoning and code refinement performance.

## Required R Packages

Before running WuKong, please install the required R packages.

Based on the WuKong source code and function modules, the following packages are required or recommended.

### Core Shiny and interface packages

```r
shiny
shinyBS
shinyjqui
shinyAce
DT
markdown
commonmark
htmltools
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

### LLM interface packages

```r
ollamar
```

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
  "shinyAce",
  "DT",
  "markdown",
  "commonmark",
  "htmltools",
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
  "ollamar"
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

if (!requireNamespace("ggsankey", quietly = TRUE)) {
  devtools::install_github("davidsjoberg/ggsankey")
}

if (!requireNamespace("ceLLama", quietly = TRUE)) {
  message("The ceLLama package is required for selected cell-type annotation functions. Please install it according to its official instructions if needed.")
}
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
3. Please make sure Ollama is installed and running before using LLM-related functions.
4. At least one Ollama model should be installed before starting WuKong.
5. Larger LLMs usually require stronger hardware and more memory.
6. For large proteomics datasets, sufficient RAM is recommended.

Suggested minimum hardware:

- RAM: 8 GB or higher
- Disk space: 100 GB or higher
- CPU: modern multi-core processor
- GPU: optional but recommended for large local LLMs

For larger LLMs such as `deepseek-r1:32b`, `gemma3:27b`, or `qwen3:30b`, higher memory and GPU acceleration are recommended.

## Troubleshooting

### 1. No model is shown in the model selection box

Please check whether Ollama is installed and running.

Run the following command in terminal:

```bash
ollama list
```

If no model is available, pull a model:

```bash
ollama pull gemma3:4b
```

Then restart WuKong.

### 2. `ollamar::list_models()` returns an error

Please make sure the Ollama service is active.

You can test Ollama using:

```bash
ollama run gemma3:4b
```

### 3. A module cannot be launched

WuKong uses:

```r
rstudioapi::jobRunScript()
```

to launch module scripts. Please run WuKong in RStudio and make sure the WuKong package has been correctly installed.

### 4. Some R packages cannot be installed

Some Bioconductor packages require compatible R and Bioconductor versions.

Please install or update Bioconductor first:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install()
```

Then install the failed packages one by one to identify dependency issues.

### 5. Functional annotation fails

Please check:

- Whether the selected species is correct.
- Whether the input IDs are compatible with the selected annotation database.
- Whether `clusterProfiler`, `GO.db`, `UniProt.ws`, and organism annotation packages are installed.

### 6. The workflow designer produces code but no result

Please check:

- Whether input data were uploaded correctly.
- Whether sample group numbers match the number of sample columns.
- Whether selected modules are logically connected.
- Whether the generated R code contains a complete code block marked by triple backticks.
- Whether all required packages for the selected modules are installed.

## Citation

If you use WuKong in your research, please cite:

WuKong: An LLM-Enhanced Framework for Adaptable Proteomics Analysis.

Citation information will be updated after publication.

## Contact

You can submit issues through this GitHub repository.

For further assistance, please contact:

Shisheng Wang: [wssomics@outlook.com](mailto:wssomics@outlook.com)
