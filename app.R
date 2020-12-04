# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(DT)
library(tidyverse)
library(mice)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Missing Values"
APP_DESCP  <<- paste(
  "This app provides an opportunity to examine the impact of missing values on",
  "the data analysis by visualizations and quantification."
)
## End App Meta Data------------------------------------------------------------

# Define UI ----
ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  ## Create the app page
  dashboardPage(
    skin = "green",
    ### Create the app header
    dashboardHeader(
      titleWidth = 250,
      title = "Missing Values",
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Missing_Values"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
      )
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ##First tab - Overview Tab----
        tabItem(
          tabName = "Overview",
          h1("Missing Values"), # This should be the full name.
          p("In this app, you will understand the meaning of missing values and figure out
            how important they are for machine learning."),
          br(),
          h2("Instructions"),
          p("Take a tour and see how imputations affect your dataset"),
          tags$ol(
            tags$li("Explore prerequisites and understand enough to proceed next step."),
            tags$li("You'll explore the dataset in Level 1"),
            tags$li("You'll explore the visualizations from the various imputations in Level 2"),
            tags$li("You'll explore the quantification from the advanced imputation in Level 3")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app is coded and developed by Daehoon Gwak in November 2020.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/30/2020 by DG")
          )
        ),
        #Second tab - Prerequisite Tab ----
        tabItem(
          tabName = "Prerequisites",
          h2("Prerequisites"),
          br(),
          box(
            title = "What does missing value mean?",
            p("Missing data means that one or more variables (features) values are 
              missing generally encoded by -999 , nan , null"),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          box(
            title = "Type of Missing data - Missing Completely at Random (MCAR)",
            p("There’s no relationship between whether a data point is missing
              and any values in the data set (missing or observed). 
              The missing data are just a random subset of the data.
              The missingness is nothing to do with any other variable. 
              Also, data are rarely MCAR."),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          box(
            title = "Type of Missing data - Missing at Random (MAR)",
            p("The missing data here is affected only by the complete observed)
              variables and not by the characteristics of the missing data itself.
              In other words, for a data point, to be missing is not related to
              the missing data, but it is related to some of (or all) the observed data"),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          box(
            title = "Type of Missing data - Missing Not at Random (MNAR)",
            p("It is nor Type I neither Type II, and the data will be missing 
              based on the missing column itself"),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          box(
            title = "Advanced imputation method: MICE",
            p("MICE is a multiple imputation method used to replace missing data
              values in a data set under certain assumptions about the data
              missingness mechanism. It imputes missing values in the variables
              of a data set by using a divide and conquer approach -
              in other words, by focusing on one variable at a time.
              Once the focus is placed on one variable, 
              MICE uses all the other variables in the data set 
              (or a sensibly chosen subset of these variables) to predict 
              missingness in that variable. The prediction is based on a 
              regression model, with the form of the model depending on 
              the nature of the focus variable"),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          br(),
          div(
            style = "text-align:center",
            bsButton(
              inputId = "go2",
              label = "GO!",
              icon("bolt"),
              size = "large"
            )
          )
        ),
        tabItem(
          tabName = "explore",
          tabsetPanel(
            # Level 1
            tabPanel('LEVEL 1',
                     selectInput(inputId = "inputLevel1",
                                 label = "SELECT DATASET",
                                 choices = c('Diabetes', 'Iris')
                     ),
                     conditionalPanel(
                       condition = "input.inputLevel1 == 'Diabetes'",
                       fluidRow(
                         column(width = 8,
                                box(
                                  title = strong("Display Dataset"),
                                  status = "primary",
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = '100%',
                                  DT::DTOutput("Diabetes_analysis"))
                                ),
                         column(width = 4,
                                box(
                                  title = strong("Variable Information"),
                                  status = "primary",
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = '100%',
                                  uiOutput("dataTableVariables_D")
                                )
                                )
                         )
                     ),
                     conditionalPanel(
                       condition = "input.inputLevel1 == 'Iris'",
                       fluidRow(
                         column(width = 8,
                                box(
                                  title = strong("Display Dataset"),
                                  status = "primary",
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = '100%',
                                  DT::DTOutput("Iris_analysis"))
                                ),
                         column(width = 4,
                                box(
                                  title = strong("Variable Information"),
                                  status = "primary",
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = '100%',
                                  uiOutput("dataTableVariables")
                                )
                         )
                       )
                     )
            ),
            # Level 2 ----
            tabPanel('LEVEL 2',
                     h2("Visualizations"),
                     # Choose dataset
                     column(
                       width = 3,
                       selectInput(
                         inputId = "inputLevel2",
                         label = "Select Data Set",
                         choices = c('Diabetes'
                                     #'Iris'
                                     )
                       )
                     ),
                     column(
                       width = 3,
                       selectInput(
                         inputId = "imputation_method",
                         label = "Select Imputation Method",
                         choices = c('Complete Case Analysis', 'Mean', 'Fill in 0s')
                       )
                     ),
                     # Choose Independent variable ----
                     column(
                       width = 3,
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Diabetes'",
                         selectInput(
                           inputId = "Ydiabetes",
                           label = "Select a dependent variable",
                           choices = list("Pregnancies", "Glucose", "BloodPressure",
                                          "SkinThickness", "Insulin", "BMI",
                                          "DiabetesPedigreeFunction"
                           ),
                           selected = "SkinThickness"
                         )
                       )
                     ),
                     # Choose Dependent variable ----
                     column(
                       width = 3,
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Diabetes'",
                         selectInput(
                           inputId = "Xdiabetes",
                           label = "Select an Independent variable",
                           choices = list("Age"),
                           selected = "Age"
                         )
                       )
                     ),
                     # Count NAs in the dataset
                     conditionalPanel(
                       condition = "input.inputLevel2 == 'Diabetes'",
                       checkboxInput(
                         inputId = "count_chekced_diabetes",
                         label = strong("Count NAs by variables"),
                         value = FALSE
                       ),
                       verbatimTextOutput("countNasDiabetes")
                     ),
                     br(),
                     fluidRow(
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Diabetes'",
                         h3("Scatter Plot", align = 'center'),
                         plotOutput("diabetesPlot"),
                         br(),
                         h3('Statistic Summary', align = 'center'),
                         DT::DTOutput('values')
                       )
                     )
            ),
            # Level 3 ----
            tabPanel('LEVEL 3',
                     h2("Advanced Imputation Method: MICE"),
                     fluidRow(column(
                       width = 4,
                       selectInput(
                         inputId = "inputLevel3",
                         label = "Select Data Set",
                         choices = c('Iris'
                                     #'Diabetes'
                                     )
                       )
                     ),
                     column(
                       width = 4,
                       selectInput(
                         inputId = "inputeMethods",
                         label = "Select Method",
                         choices = c('MICE'
                                     #'Complete Analysis Case',
                                     #'Mean', 'Fill in 0s' 
                                     )
                       )
                     )),
                     br(),
                     conditionalPanel(
                       condition = "input.inputLevel3 == 'Iris'",
                       fluidRow(
                         checkboxInput(
                           inputId = "count_chekced_sampleDT",
                           label = ("Observe random generated datatable"),
                           value = FALSE
                         ),
                         DT::DTOutput(outputId = "manipulated_DT")),
                       br(),
                       fluidRow(h3("Compare outputs"), align = "center"),
                       br(),
                       fluidRow(
                         column(
                           width = 2,
                           p("original output")),
                         column(
                           width = 2,
                           offset = 4,
                           p("MICE output")
                         )
                       ),
                       br(),
                       fluidRow(
                         column(
                           width = 6,
                           verbatimTextOutput("iris0_output")
                         ),
                         column(
                           width = 6,
                           verbatimTextOutput("iris1_output")
                         )),
                       br(),
                       fluidRow(
                         column(
                           width = 2,
                           p("Original values")
                         ),
                         column(
                           width = 2,
                           offset = 4,
                           p("MICE values")
                         )
                       ), br(),
                       fluidRow(
                         column(
                           width = 6,
                           DT::DTOutput(outputId = "original_DT")
                         ),
                         column(
                           width = 6,
                           verbatimTextOutput("iris_real_values")
                         )
                       )
                     ), br()
            )
          )
        ),
        ### References ----
        tabItem(
          tabName = "References",
          h2("References"),
          p( # shinyBS
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
             R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p( # mice
            class = "hangingindent",
            "Buuren, S.V., Groothuis-Oudshoorn, K., Vink, G., Schouten, R.,
            Robitzsch, A., Rockenschaub, P., Doove, L., Jolani, S., 
            Moreno-Betancur, M., White, I., Gaffert, P., Meinfelder, F., Gray, B.,
            ArelBundock, V., (2020), mice: Multivariate Imputation by Chained Equations,
            R Package. Available from
            https://cran.r-project.org/web/packages/mice/index.html"
          ),
          p( # boastUtils
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities,
            R Package. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p( # shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p( # shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p( # Prerequisites - Definitions of missing values
            class = "hangingindent",
            "Keskes, R., (2019) Missing Data, its types, and statistical
            methods to deal with it. Available from 
            https://blog.usejournal.com/missing-data-its-types-and-statistical-methods-to-deal-with-it-5cf8b71a443f"
          ),
          p( # MICE part(Level3) Idea
            class = "hangingindent",
            "Moon, K.W., (2016) Handling of missing data Available from 
            https://rstudio-pubs-static.s3.amazonaws.com/192402_012091b9adac42dbbd22c4d07cb00d36.html"),
          p( # Prerequisites - Definitions of MICE method
            class = "hangingindent",
            "https://stats.stackexchange.com/questions/421545/multiple-imputation-by-chained-equations-mice-explained"
          ),
          p( # Dataset - Diabetes
            class = "hangingindent",
            "Pima Indians Diabetes Database. Available from 
            https://www.kaggle.com/uciml/pima-indians-diabetes-database"
          ),
          p( # tidyverse
            class = "hangingindent",
            "Wickham, H., (2019), tidyverse: Easily Install 
            and Load the 'Tidyverse', R Package. Available from
            https://cran.r-project.org/web/packages/tidyverse/index.html"
          ),
          p( # DT
            class = "hangingindent",
            "Xie, Y., Cheng, J., Tan, X., Allaire, J., Girlich, M., Ellis, G.F.,
            and Rauh, J. (2020), DT: A Wrapper of the JavaScript Library
            'DataTables', R Package. Available from 
            https://cran.r-project.org/web/packages/DT/index.html"
          )
        )
      )
    )
  )
)


## server ----
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$go1, { #to prerequisite page
    updateTabItems(session, "pages", "Prerequisites")
  })
  observeEvent(input$go2, { #to explore page
    updateTabItems(session, "pages", "explore")
  })
  
  # Adding in Data
  #setwd('./Documents/GitHub/Sample_APP')
  data(iris)
  read.csv('diabetes.csv') -> diabetes
  
  #Replace 0 to NAs
  diabetes <- diabetes %>% 
    mutate(Insulin = ifelse(diabetes$Insulin == "0", NA, Insulin),
           BMI= ifelse(diabetes$BMI == "0", NA, BMI),
           BloodPressure = ifelse(diabetes$BloodPressure == "0", NA, BloodPressure),
           SkinThickness = ifelse(diabetes$SkinThickness == "0", NA, SkinThickness),
           Glucose = ifelse(diabetes$Glucose == "0", NA, Glucose))
  diabetes$Outcome<-NULL
  
  ######################## Level Start #########################################
  ## Level 1 ----
  # Show dataset
  # Diabetes
  output$Diabetes_analysis <- DT::renderDT(
    expr = diabetes,
    caption = "Predict the onset of diabetes based on diagnostic measures",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE, # allows the data table to be mobile friendly
      scrollX = TRUE, # allows the user to scroll through a wide table
      columnDefs = list(  # These will set alignment of data values
        # Notice the use of ncol on your data frame; leave the 1 as is.
        list(className = 'dt-center', targets = 1:ncol(diabetes))
      )
    )
  )
  # Iris
  output$Iris_analysis <- DT::renderDT(
    expr = iris,
    caption = "Iris Dataset",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE, # allows the data table to be mobile friendly
      scrollX = TRUE, # allows the user to scroll through a wide table
      columnDefs = list(  # These will set alignment of data values
        # Notice the use of ncol on your data frame; leave the 1 as is.
        list(className = 'dt-center', targets = 1:ncol(iris))
      )
    )
  )
  
  ## Level 1 - dictionary table
  output$dataTableVariables_D <- renderText({
    "
    </li><li>Pregnancies: Number of times pregnant</li><li>
    Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test</li><li>
    BloodPressure: Diastolic blood pressure (mm Hg)</li><li>
    Diastolic blood pressure (mm Hg): Triceps skin fold thickness (mm)</li><li>
    Insulin: 2-Hour serum insulin (mu U/ml)</li><li>
    BMI: Body mass index (weight in kg/(height in m)^2)</li><li>
    DiabetesPedigreeFunction: Diabetes pedigree function</li><li>
    Age: Age (years)
    "
  })
  output$dataTableVariables <- renderText({
    "
    </li><li>Sepal.Length: Length of the sepal in cm</li><li>
    Sepal.Width: Length of the sepal in cm</li><li>
    Petal.Length: Length of the sepal in cm</li><li>
    Petal.Width: Length of the sepal in cm</li><li>
    Species: Either Setosa, Versicolour or Virginica
    "
  })
  
  ######################## Level Start #########################################
  ## Level 2 ----
  #show NAs by variable; Melbourne Dataset
  output$countNasDiabetes <- renderPrint({
    if(input$count_chekced_diabetes) {
      sapply(diabetes, function(x) sum(is.na(x)))
    }
  })
  
  ## Level 3 - Imputations  
  observeEvent(input$inputLevel2, {
    # Melbourne Dataset
    if (input$inputLevel2 == 'Diabetes')
    {
      output$values <- DT::renderDT({
        # prepare for the new dataset(by imputation method)
        lm(diabetes[, input$Ydiabetes] ~ diabetes$Age) -> testing
        # mean
        diabetes2 = diabetes
        round(mean(diabetes2[, input$Ydiabetes], na.rm = TRUE), digits = 0) -> avg
        replace_na(diabetes2[, input$Ydiabetes], replace = avg) -> diabetes2[, input$Ydiabetes]
        lm(diabetes2[, input$Ydiabetes] ~ diabetes2$Age) -> testing2
        # fill in 0s
        diabetes3 = diabetes
        replace_na(diabetes3[, input$Ydiabetes], replace = 0) -> diabetes3[, input$Ydiabetes]
        lm(diabetes3[, input$Ydiabetes] ~ diabetes3$Age) -> testing3
        
        # Build dataframe
        ImputeMethod <- c('Complete Case Analysis', 'Mean', 'Fill in zeros')
        Coef <- c(format(summary(testing)$coefficients[2], digits = 4),
                  format(summary(testing2)$coefficients[2], digits = 4),
                  format(summary(testing3)$coefficients[2], digits = 4))
        Pvalue <- c(format(summary(testing)$coefficients[8], digits = 4),
                    format(summary(testing2)$coefficients[8], digits = 4),
                    format(summary(testing3)$coefficients[8], digits = 4))
        R_Squared <- c(format(summary(testing)$r.squared, digits = 4),
                       format(summary(testing2)$r.squared, digits = 4),
                       format(summary(testing3)$r.squared, digits = 4))
        Adj_R_Squared <- c(format(summary(testing)$adj.r.squared, digits = 4),
                           format(summary(testing2)$adj.r.squared, digits = 4),
                           format(summary(testing3)$adj.r.squared, digits = 4))
        data.frame(ImputeMethod, Coef, Pvalue, R_Squared, Adj_R_Squared)
      },
      style = "bootstrap4", # You must use this style
      rownames = TRUE,
      options = list(
        responsive = TRUE,
        scrollX = TRUE,
        paging = FALSE, # Set to False for small tables
        searching = FALSE, # Set to False to turn of the search bar
        ordering = FALSE
      )
      )
      # Plot for Complete Case Analysis
      output$diabetesPlot = renderPlot({
        {
          if (input$imputation_method == 'Complete Case Analysis')
          {
            data.lm = lm(diabetes[, input$Ydiabetes]~ diabetes$Age)
            ggplot(diabetes, aes(Age, diabetes[, input$Ydiabetes])) +
              geom_point() +
              labs(x='Age',y=input$Ydiabetes) +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm)[[2]], intercept = coef(data.lm)[[1]],
                          col = 'blue')
          }
          else if (input$imputation_method == 'Mean')
          {
            diabetes2 = diabetes
            round(mean(diabetes2[, input$Ydiabetes], na.rm = TRUE), digits = 0) -> avg
            replace_na(diabetes2[, input$Ydiabetes], replace = avg) -> diabetes2[, input$Ydiabetes]
            data.lm2 = lm(diabetes2[, input$Ydiabetes]~ diabetes2$Age)
            ggplot(diabetes2, aes(Age, diabetes2[, input$Ydiabetes])) +
              geom_point() +
              labs(x='Age',y=input$Ydiabetes) +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm2)[[2]], intercept = coef(data.lm2)[[1]],
                          col = 'red')
          }
          else if (input$imputation_method == 'Fill in 0s')
          {
            diabetes3 = diabetes
            replace_na(diabetes3[, input$Ydiabetes], replace = 0) -> diabetes3[, input$Ydiabetes]
            data.lm3 = lm(diabetes3[, input$Ydiabetes]~ diabetes3$Age)
            ggplot(diabetes3, aes(Age, diabetes3[, input$Ydiabetes])) +
              geom_point() +
              labs(x='Age',y=input$Ydiabetes) +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm3)[[2]], intercept = coef(data.lm3)[[1]],
                          col = 'green')
          }
        }
      })
    }
  }) # Level 2 ends
  
  ######################## Level Start #########################################
  ## Level 3 ----
  output$iris0_output <- renderPrint({
    reg0=lm(Sepal.Length~Sepal.Width+Petal.Length+Species, data=iris)
    summary(reg0)[4]
  })
  
  #Data manipulation to drop some values
  iris1=iris
  set.seed(999)
  #choose 20 out of 150
  random1=sample(1:150,10)
  #decide which row to drop
  random2=sample(1:5,10,replace=TRUE)
  #convert them to NA
  for(i in 1:10) iris1[random1[i],random2[i]]<-NA
  
  output$original_DT <- DT::renderDT({
    #original dataset
    iris[random1,]
  },
  style = "bootstrap4", # You must use this style
  rownames = TRUE,
  options = list(
    responsive = TRUE,
    scrollX = TRUE,
    paging = FALSE, # Set to False for small tables
    searching = FALSE, # Set to False to turn of the search bar
    ordering = FALSE,
    dom = 't' # Remove 'showing 1 to 1 of 1 entries' element
  ))
  
  output$manipulated_DT <- DT::renderDT({
    if(input$count_chekced_sampleDT) {
      #check the manipulated dataset
      iris1[random1, ]
    }
  },
  caption = "NA generated",
  style = "bootstrap4", # You must use this style
  rownames = TRUE,
  options = list(
    responsive = TRUE,
    scrollX = TRUE,
    paging = FALSE, # Set to False for small tables
    searching = FALSE, # Set to False to turn of the search bar
    ordering = FALSE,
    dom = 't' # Remove 'showing 1 to 1 of 1 entries' element
  ))
  
  #Implement the MICE
  require(mice)
  imp = mice(iris1,seed=1275)
  #check out the fit ----
  output$iris1_output <- renderPrint({
    fit1 = with(imp,lm(Sepal.Length~Sepal.Width+Petal.Length+Species,data = iris))
    pooled = pool(fit1)
    summary(pooled)
  })
  
  #check out the values converted from the MICE
  output$iris_real_values <- renderPrint({
    imp$imp
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)




