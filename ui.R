#under GNU General Public License
#ui.R
library(shiny)
library(ggplot2)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("mtDNA power estimation"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput("min.cases", "Minimum number of cases (num.cases):", 
                min=50, max=1000, value = 100, step=50),
    sliderInput("max.cases", "Maximum number of cases (num.cases):", 
                min=50, max=1000, value = 600, step=50),
    sliderInput("p0", "Frequency of the haplogroup in controls (p0):", 
                min=0, max=1, value=0.5),
    sliderInput("OR.cas.ctrl", "Odds ratio to detect (OR):", 
                min=1.05, max=4, value=2.3, step=0.05),
    sliderInput("Nh", "Number of haplogroup categories (Nh):", 
                min=4, max=20, value=11),
    selectInput("sig.level", "Significance level:", 
                choices = c("0.05", "0.01", "0.001")),  
    HTML("<h4>Instructions:</h4>"),
    helpText("Power and sample size estimates for genetic association 
             studies on mitochondrial DNA haplogroups.  
             Formulae in Samuels et al., AJHG 2006 ",
             a("PMC1424681", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1424681/", target="_blank"), "."),
    
    helpText("Can be used when multiple Chi-square tests are involved 
             (one case-control comparison for each haplogroup category). 
             Cases correspond to the same number of controls."),
    helpText("Written in R/Shiny by A. Baluja."),
    HTML("<h5>&nbsp;</h5>"),
    HTML("<h4>R package via GitHub:</h4>"),
    HTML("<pre>devtools::install_github('aurora-mareviv/mthapower')</pre>"),
    HTML("<h4> </h4>"),
    HTML("<h4>Shiny app (local) via Gist:</h4>"),
    HTML("<pre>shiny::runGist('5895082')</pre>"),
    downloadButton('downloadData', 'Download dataset'),
    downloadButton('downloadPlot', 'Download plot')
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    plotOutput("powHap"),
    div(dataTableOutput("powTable"), style = "font-size:90%")
  )
)
) 

# To execute this script from inside R, you need to have the ui.R and server.R files into the session's working directory. Then, type:
# runApp()
# To execute directly this Gist, from the Internet to your browser, type:
# shiny::runGist('5895082')
