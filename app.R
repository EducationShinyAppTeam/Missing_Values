# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(DT)
library(tidyverse)
library(mice)


#### Define UI ----
ui <- list(
  dashboardPage(
    skin = "green", 
    ### Dashboard Header ----
    dashboardHeader(
      titleWidth = 250, 
      title = "Missing Values",
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
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
    ### DashboardSiderbar ----
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
    ### Body ----
    dashboardBody(
      tabItems(
        ## Overview Tab----
        tabItem(
          tabName = "Overview", 
          h1("Missing Values"), 
          p("In this app, you will understand the meaning and importance of 
            missing values and learn about replacement methods."), 
          br(), 
          h2("Instructions"), 
          p("Take a tour and see how imputations affect the data analysis"), 
          tags$ol(
            tags$li("Explore the prerequisites and then move to the explore page"), 
            tags$li("You'll explore the datasets in the observe tab"), 
            tags$li("You'll explore the correlation plot from the various 
                    imputation methods in plot tab"), 
            tags$li("You'll explore the regression summary and the imputed 
                    values from the various imputation methods in methods tab")
          ), 
          div(
            style = "text-align: center", 
            bsButton(
              inputId = "prereqButton", 
              label = "Prerequisites", 
              size = "large", 
              icon = icon("book"), 
            )
          ), 
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Daehoon Gwak in
            July 2020. The app was updated in 2023 by Robert Chappell.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 06/12/2023 by RWC.")
          )
        ), 
        ## Prerequisite Tab ----
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
              the missing data, but it is related to some of (or all) the 
              observed data"), 
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
              the nature of the focus variable",
              br(),
              "NOTE: On Level 3, you can compare imputed values with the
              actual value. However, for this MICE imputation method, 
              you will observe 5 imputed values for each NA because of the
              property of the MICE method."), 
            collapsible = TRUE, 
            collapsed = TRUE, 
            width = 12
          ), 
          br(), 
          div(
            style = "text-align:center", 
            bsButton(
              inputId = "exploreButton", 
              label = "Explore!", 
              icon("bolt"), 
              size = "large"
            )
          )
        ), 
        ## Explore tab ----
        tabItem(
          tabName = "explore", 
          tabsetPanel(
            # Level 1 ----
            tabPanel(title = 'Observe', 
                     selectInput(inputId = "inputLevel1", 
                                 label = "Select Dataset", 
                                 choices = c('Diabetes', 'Iris')
                     ),
                      fluidRow(
                         column(width = 8, 
                                DT::DTOutput("analysis")
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
                              ))), 
            # Level 2 ----
            tabPanel(title = 'Plot', 
                     h2("Visualizations"), 
                     fluidRow(
                     # Choose dataset
                     column(
                       width = 3, 
                       selectInput(
                         inputId = "inputLevel2", 
                         label = "Select Dataset", 
                         choices = c('Diabetes','Iris')
                       )
                     ), 
                     column(
                       width = 3, 
                       selectInput(
                         inputId = "imputation_method", 
                         label = "Select Imputation Method", 
                         choices = c('Complete Case Analysis', 'Mean', 
                                     'Fill in 0s', 'MICE')
                       )
                     ), 
                     # Choose Independent variable
                     column(
                       width = 3, 
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Diabetes'", 
                         selectInput(
                           inputId = "Ydiabetes", 
                           label = "Select a Dependent Variable", 
                           choices = list("Pregnancies", "Glucose", 
                                          "BloodPressure", "SkinThickness", 
                                          "Insulin", "BMI", 
                                          "DiabetesPedigreeFunction"), 
                           selected = "SkinThickness"
                         )
                       ), 
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Iris'", 
                         selectInput(
                           inputId = "Yiris", 
                           label = "Select an Independent Variable", 
                           choices = list("Sepal.Length"), 
                           selected = "Sepal.Length"
                         )
                       )
                     ), 
                     # Choose Dependent variable
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
                       ), 
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Iris'", 
                         selectInput(
                           inputId = "Xiris", 
                           label = "Select a dependent variable", 
                           choices = list("Sepal.Width", "Petal.Length", 
                                          "Petal.Width"), 
                           selected = "Sepal.Width"
                         )
                       )
                     )
                     ), 
                     # Count NAs in the dataset
                     conditionalPanel(
                       condition = "input.inputLevel2 == 'Diabetes'", 
                       box(
                         title = strong("Count NAs by variables"), 
                         status = "primary", 
                         collapsible = TRUE, 
                         collapsed = FALSE, 
                         width = '100%', 
                         DT::DTOutput("countNasDiabetes"))
                      ), 
                     conditionalPanel(
                       condition = "input.inputLevel2 == 'Iris'", 
                       box(
                         title = strong("Count NAs by variables"), 
                         status = "primary", 
                         collapsible = TRUE, 
                         collapsed = FALSE, 
                         width = '100%', 
                         DT::DTOutput("countNasIris"))
                     ), 
                     br(), 
                     fluidRow(
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Diabetes'", 
                         h3("Scatter Plot", align = 'center'), 
                         plotOutput("diabetesPlot")
                       ), 
                       conditionalPanel(
                         condition = "input.inputLevel2 == 'Iris'", 
                         h3("Scatter Plot", align = 'center'), 
                         br(), 
                         fluidRow(
                           column(
                             width = 2, 
                             p("Plot from original dataset")
                           ), 
                           column(
                             width = 2, 
                             offset = 4, 
                             p("Plot from imputed dataset")
                           )
                         ), 
                         br(),
                         column(
                           width = 6, 
                           plotOutput("irisPlot0")
                         ), 
                         column(
                           width = 6, 
                           plotOutput("irisPlot")
                         )
                       )
                     )
            ), 
            # Level 3 ----
            tabPanel(title = 'Methods', 
                     h2("Quantifications"), 
                     fluidRow(column(
                       width = 4, 
                       selectInput(
                         inputId = "inputLevel3", 
                         label = "Select Data Set", 
                         choices = c('Iris', 'Diabetes')
                       )
                     ), 
                     column(
                       width = 4, 
                       selectInput(
                         inputId = "imp_methods", 
                         label = "Select Method", 
                         choices = c('Complete Case Analysis','Mean', 
                                     'Fill in 0s','MICE')
                       )
                     )), 
                     br(), 
                     conditionalPanel(
                       condition = "input.inputLevel3 == 'Iris'", 
                       fluidRow(
                         checkboxInput(
                           inputId = "count_chekced_sampleDT", 
                           label = ("Observe sampled datatable"), 
                           value = FALSE
                         ), 
                         DT::DTOutput(outputId = "manipulated_DT")), 
                       br(), 
                       fluidRow(h3("Compare Outputs"), align = "center"), 
                       br(), 
                       fluidRow(
                         column(
                           width = 4,
                           offset = 2,
                           p("Original Full Fit Output")), 
                         column(
                           width = 4, 
                           offset = 1, 
                           p("Different Imputation Methods")
                         )
                       ), 
                       br(), 
                       fluidRow(
                         column(
                           width = 6, 
                           DT::DTOutput("original_reg_summary")
                         ), 
                         column(
                           width = 6, 
                           DT::DTOutput("reg_summary")
                         )
                       ), 
                       br(), 
                       fluidRow(h3("Compare values"), align = "center"), 
                       br(), 
                       fluidRow(
                         column(
                           width = 2, 
                           p("Original values")
                         ), 
                         column(
                           width = 2, 
                           offset = 4, 
                           p("Imputed values")
                         )
                       ), 
                       br(),
                       fluidRow(
                         column(
                           width = 6, 
                           DT::DTOutput(outputId = "original_DT")
                         ), 
                         column(
                           width = 6, 
                           DT::DTOutput("imp_values")
                         )
                       )
                     ), 
                     conditionalPanel(
                       condition = "input.inputLevel3 == 'Diabetes'", 
                       fluidRow(
                         checkboxInput(
                           inputId = "count_chekced_sampleDT_Diabetes", 
                           label = ("Observe sampled datatable"), 
                           value = FALSE
                         ), 
                         DT::DTOutput(outputId = "manipulated_DT_Diabetes")), 
                       br(), 
                       fluidRow(h3("Compare outputs"), align = "center"), 
                       br(), 
                       fluidRow(
                         column(
                           width = 4, 
                           p("original full fit output")), 
                         column(
                           width = 4, 
                           offset = 2, 
                           p("Different Imputation methods")
                         )
                       ), 
                       br(), 
                       fluidRow(
                         column(
                           width = 6, 
                           DT::DTOutput("original_reg_summary2")
                         ), 
                         column(
                           width = 6, 
                           DT::DTOutput("reg_summary2")
                         )
                       ), 
                       br(), 
                       fluidRow(h3("Compare values"), align = "center"), 
                       br(), 
                       fluidRow(
                         column(
                           width = 2, 
                           p("Original values")
                         ), 
                         column(
                           width = 2, 
                           offset = 4, 
                           p("Imputed values")
                         )
                       ), 
                       br(),
                       fluidRow(
                         column(
                           width = 6, 
                           DT::DTOutput(outputId = "original_DT2")
                         ), 
                         column(
                           width = 6, 
                           DT::DTOutput("imp_values2")
                         )
                       )
                     ),br()
            )
          )
        ), 
        ### References ----
        tabItem(
          tabName = "References", 
          h2("References"),
          p( # Prerequisites - Definitions of MICE method
            class = "hangingindent", 
            "Anon. n.d. “Multiple Imputation by Chained Equations (MICE) 
            Explained.” Cross Validated. Retrieved June 13, 2023 
            (https://stats.stackexchange.com/questions/421545/multiple-
            imputation-by-chained-equations-mice-explained)."            
          ), 
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
            ArelBundock, V., (2020), mice: Multivariate Imputation by Chained 
            Equations, R Package. Available from
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
            https://rstudio-pubs-static.s3.amazonaws.com/192402_012091b9adac42dbbd22c4d07cb00d36.html"
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
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

#### Define Server ----
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Head to the Explore page to learn how to deal with hidden values."
      )
    }
  )
  
  observeEvent(input$prereqButton, { #to prerequisite page
    updateTabItems(session, "pages", "Prerequisites")
  })
  observeEvent(input$exploreButton, { #to explore page
    updateTabItems(session, "pages", "explore")
  })
  # Adding in Data
  #setwd('./Documents/GitHub/Missing_Values')
  data(iris)
  read.csv('diabetes.csv') -> diabetes
  #Replace 0 to NA for Diabetes
  diabetes <- diabetes %>% 
    mutate(Insulin = ifelse(diabetes$Insulin == "0", NA, Insulin),
           BMI= ifelse(diabetes$BMI == "0", NA, BMI),
           BloodPressure = ifelse(diabetes$BloodPressure == "0", NA, BloodPressure),
           SkinThickness = ifelse(diabetes$SkinThickness == "0", NA, SkinThickness),
           Glucose = ifelse(diabetes$Glucose == "0", NA, Glucose))
  diabetes$Outcome <- NULL
  ## Level 1(Server) ----
  # Diabetes
  output$analysis <- DT::renderDT({
    if (input$inputLevel1 == "Diabetes") {
      DT::datatable(
        diabetes, 
        caption = "Predict the onset of diabetes based on diagnostic measures", 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          columnDefs = list(
            list(className = 'dt-center', targets = 1:ncol(diabetes))
          )
        )
      )
    }
  })
  
  ## Iris
  output$analysis <- DT::renderDT({
    if (input$inputLevel1 == "Iris") {
      DT::datatable(
        iris, 
        caption = "Iris Dataset", 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          columnDefs = list(
            list(className = 'dt-center', targets = 1:ncol(iris))
          )
        )
      )
    }
  })
  ## Dictionary
  output$dataTableVariables <- renderUI({
    if (input$inputLevel1 == "Diabetes") {
      tags$ul(
        tags$li("Pregnancies: Number of times pregnant"),
        tags$li("Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test"),
        tags$li("BloodPressure: Diastolic blood pressure (mm Hg)"),
        tags$li("SkinThickness: Triceps skin fold thickness (mm)"),
        tags$li("Insulin: 2-Hour serum insulin (mu U/ml)"),
        tags$li("BMI: Body mass index (weight in kg/(height in m)^2)"),
        tags$li("DiabetesPedigreeFunction: Diabetes pedigree function"),
        tags$li("Age: Age (years)")
      )
    } else if (input$inputLevel1 == "Iris") {
      tags$ul(
        tags$li("Sepal.Length: Length of the sepal in cm"),
        tags$li("Sepal.Width: Width of the sepal in cm"),
        tags$li("Petal.Length: Length of the pedal in cm"),
        tags$li("Petal.Width: Width of the sepal in cm"),
        tags$li("Species: Either Setosa, Versicolour or Virginica")
      )
    } else {
      NULL
    }
  })
  
  ## Level 2(Server) ----
  #Data manipulation - generate sample dataset with NA included
  #For Iris
  iris1=iris
  set.seed(999)
  #choose 10 out of 150
  random1=sample(1:150,10)
  #decide which row to drop
  random2=sample(1:5,10,replace=TRUE)
  #convert them to NA
  for(i in 1:10) iris1[random1[i],random2[i]]<-NA
  
  #For Iris2 - specifically for Level2 for better visualization
  iris20=iris
  set.seed(999)
  #choose 10 out of 150
  random1=sample(1:150,20)
  #decide which row to drop
  random2=sample(1:5,20,replace=TRUE)
  #convert them to NA
  for(i in 1:20) iris20[random1[i],random2[i]]<-NA
  
  #For Diabetes
  diabetes0=diabetes
  set.seed(999)
  #choose 10 out of 150
  random3=sample(1:768,10)
  #decide which row to drop
  random4=sample(1:5,10,replace=TRUE)
  
  #show NAs by variable; Diabetes Dataset
  output$countNasDiabetes <- renderDT({
      df1<-data.frame(sapply(diabetes, function(x) sum(is.na(x))))
      Variables <-
        c('Pregnancies','Glucose','BloodPressure','SkinThickness', 
          'Insulin','BMI','DiabetesPedigreeFunction','Age')
      Number_of_NAs <- c(
        df1$sapply.diabetes..function.x..sum.is.na.x...[1], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[2], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[3], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[4], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[5], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[6], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[7], 
        df1$sapply.diabetes..function.x..sum.is.na.x...[8]
      )
      as.data.frame(rbind(Variables, Number_of_NAs))
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    # Set to False for small tables
    searching = FALSE, 
    # Set to False to turn of the search bar
    ordering = FALSE, 
    dom = 't' # Remove 'showing 1 to 1 of 1 entries' element
  ))
  output$countNasIris <- renderDT({
      df2<-data.frame(sapply(iris20, function(x) sum(is.na(x))))
      Variables <-
        c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width', 'Species')
      Number_of_NAs <- c(
        df2$sapply.iris20..function.x..sum.is.na.x...[1], 
        df2$sapply.iris20..function.x..sum.is.na.x...[2], 
        df2$sapply.iris20..function.x..sum.is.na.x...[3], 
        df2$sapply.iris20..function.x..sum.is.na.x...[4]
      )
      as.data.frame(rbind(Variables, Number_of_NAs))
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  
  observeEvent(input$inputLevel2, {
    # Diabetes Dataset
    if (input$inputLevel2 == 'Diabetes')
    {
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
              geom_abline(slope = coef(data.lm)[[2]], 
                          intercept = coef(data.lm)[[1]], col = 'blue')
          }
          else if (input$imputation_method == 'Mean')
          {
            diabetes2 = diabetes
            round(mean(diabetes2[, input$Ydiabetes], 
                       na.rm = TRUE), digits = 0) -> avg
            replace_na(diabetes2[, input$Ydiabetes], 
                       replace = avg) -> diabetes2[, input$Ydiabetes]
            data.lm2 = lm(diabetes2[, input$Ydiabetes]~ diabetes2$Age)
            ggplot(diabetes2, aes(Age, diabetes2[, input$Ydiabetes])) +
              geom_point() + labs(x='Age',y=input$Ydiabetes) +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm2)[[2]], 
                          intercept = coef(data.lm2)[[1]], col = 'red')
          }
          else if (input$imputation_method == 'Fill in 0s')
          {
            diabetes3 = diabetes
            replace_na(diabetes3[, input$Ydiabetes], 
                       replace = 0) -> diabetes3[, input$Ydiabetes]
            data.lm3 = lm(diabetes3[, input$Ydiabetes]~ diabetes3$Age)
            ggplot(diabetes3, aes(Age, diabetes3[, input$Ydiabetes])) +
              geom_point() + labs(x='Age',y=input$Ydiabetes) +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm3)[[2]], 
                          intercept = coef(data.lm3)[[1]], col = 'green')
          }
        }
      },
      alt = "Scatterplot of chosen dataset and variables")
    }
    else if (input$inputLevel2 == 'Iris')
    {
      output$irisPlot0 <- renderPlot({
        data.lm0 = lm(iris$Sepal.Length~ iris[, input$Xiris])
        ggplot(iris, aes(iris[, input$Xiris], iris$Sepal.Length)) +
          geom_point() +
          labs(x=input$Xiris, y='Sepal.Length') +
          theme_bw(base_size = 20) +
          geom_abline(slope = coef(data.lm0)[[2]], 
                      intercept = coef(data.lm0)[[1]], col = 'black')
      },
      alt = "Scatterplot of iris dataset")
      output$irisPlot <- renderPlot({
        {
          if (input$imputation_method == 'Complete Case Analysis')
          {
            data.lm = lm(iris20$Sepal.Length~ iris20[, input$Xiris])
            ggplot(iris20, aes(iris20[, input$Xiris], iris20$Sepal.Length)) +
              geom_point() + labs(x=input$Xiris, y='Sepal.Length') +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm)[[2]], 
                          intercept = coef(data.lm)[[1]], col = 'blue')
          }
          else if (input$imputation_method == 'Mean')
          {
            iris21 = iris20
            round(mean(iris21[, input$Xiris], na.rm = TRUE), digits = 0) -> avg
            replace_na(iris21[, input$Xiris], 
                       replace = avg) -> iris21[, input$Xiris]
            data.lm2 = lm(iris21$Sepal.Length~ iris21[, input$Xiris])
            ggplot(iris21, aes(iris21[, input$Xiris], iris21$Sepal.Length)) +
              geom_point() + labs(x=input$Xiris, y='Sepal.Length') +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm2)[[2]], 
                          intercept = coef(data.lm2)[[1]], col = 'red')
          }
          else if (input$imputation_method == 'Fill in 0s')
          {
            iris22 = iris20
            replace_na(iris22[, input$Xiris], replace = 0) -> iris22[, input$Xiris]
            data.lm3 = lm(iris22$Sepal.Length~ iris22[, input$Xiris])
            ggplot(iris22, aes(iris22[, input$Xiris], iris22$Sepal.Length)) +
              geom_point() + labs(x=input$Xiris, y='Sepal.Length') +
              theme_bw(base_size = 20) +
              geom_abline(slope = coef(data.lm3)[[2]], 
                          intercept = coef(data.lm3)[[1]], col = 'green')
          }
        }
      },
      alt = "Scatterplot of chosen variables")
    }
  }) # Level 2 ends
  
  ## Level 3 ----
  # Full Iris Dataset
  output$original_reg_summary <- DT::renderDT({
    reg0 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, data = iris)
    sum_coef <- summary(reg0)[4]
    Variables <-
      c('(intercept)', 'Sepal.Width', 'Petal.Length', 'Speciesversicolor', 
        'Speciesvirginica')
    Estimates <- c(format(sum_coef$coefficients[1], digits = 2), 
                   format(sum_coef$coefficients[2], digits = 2), 
                   format(sum_coef$coefficients[3], digits = 2), 
                   format(sum_coef$coefficients[4], digits = 2), 
                   format(sum_coef$coefficients[5], digits = 2))
    Std.Error <- c(format(sum_coef$coefficients[6], digits = 2), 
                   format(sum_coef$coefficients[7], digits = 2), 
                   format(sum_coef$coefficients[8], digits = 2), 
                   format(sum_coef$coefficients[9], digits = 2), 
                   format(sum_coef$coefficients[10], digits = 2))
    t_value <- c(format(sum_coef$coefficients[11], digits = 2), 
                 format(sum_coef$coefficients[12], digits = 2), 
                 format(sum_coef$coefficients[13], digits = 2), 
                 format(sum_coef$coefficients[14], digits = 2), 
                 format(sum_coef$coefficients[15], digits = 2))
    p_value <- c(format(sum_coef$coefficients[16], digits = 2), 
                 format(sum_coef$coefficients[17], digits = 2), 
                 format(sum_coef$coefficients[18], digits = 2), 
                 format(sum_coef$coefficients[19], digits = 2), 
                 format(sum_coef$coefficients[20], digits = 2))
    as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
  }, 
  style = "bootstrap4", # You must use this style
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  output$original_reg_summary2 <- DT::renderDT({
    reg0 = lm(Age ~ Pregnancies + Glucose + BloodPressure + SkinThickness 
              + Insulin + BMI + DiabetesPedigreeFunction, data = diabetes)
    sum_coef <- summary(reg0)[4]
    Variables <-
      c('(intercept)', 'Pregnancies', 'Glucose', 'BloodPressure', 
        'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction')
    Estimates <- c(format(sum_coef$coefficients[1], digits = 2), 
                   format(sum_coef$coefficients[2], digits = 2), 
                   format(sum_coef$coefficients[3], digits = 2), 
                   format(sum_coef$coefficients[4], digits = 2), 
                   format(sum_coef$coefficients[5], digits = 2), 
                   format(sum_coef$coefficients[6], digits = 2), 
                   format(sum_coef$coefficients[7], digits = 2), 
                   format(sum_coef$coefficients[8], digits = 2))
    Std.Error <- c(format(sum_coef$coefficients[9], digits = 2), 
                   format(sum_coef$coefficients[10], digits = 2), 
                   format(sum_coef$coefficients[11], digits = 2), 
                   format(sum_coef$coefficients[12], digits = 2), 
                   format(sum_coef$coefficients[13], digits = 2), 
                   format(sum_coef$coefficients[14], digits = 2), 
                   format(sum_coef$coefficients[15], digits = 2), 
                   format(sum_coef$coefficients[16], digits = 2))
    t_value <- c(format(sum_coef$coefficients[17], digits = 2), 
                 format(sum_coef$coefficients[18], digits = 2), 
                 format(sum_coef$coefficients[19], digits = 2), 
                 format(sum_coef$coefficients[20], digits = 2), 
                 format(sum_coef$coefficients[21], digits = 2), 
                 format(sum_coef$coefficients[22], digits = 2), 
                 format(sum_coef$coefficients[23], digits = 2), 
                 format(sum_coef$coefficients[24], digits = 2))
    p_value <- c(format(sum_coef$coefficients[25], digits = 2), 
                 format(sum_coef$coefficients[26], digits = 2), 
                 format(sum_coef$coefficients[27], digits = 2), 
                 format(sum_coef$coefficients[28], digits = 2), 
                 format(sum_coef$coefficients[29], digits = 2), 
                 format(sum_coef$coefficients[30], digits = 2), 
                 format(sum_coef$coefficients[31], digits = 2), 
                 format(sum_coef$coefficients[32], digits = 2))
    as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  #Generate sample DT from Iris
  output$original_DT <- DT::renderDT({
    iris[random1,]
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  #Generate sample DT from Diabetes
  output$original_DT2 <- DT::renderDT({
    diabetes[random3,]
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  #Count NAs
  output$manipulated_DT <- DT::renderDT({
    if(input$count_chekced_sampleDT) {
      iris1[random1, ]
    }
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  output$manipulated_DT_Diabetes <- DT::renderDT({
    if(input$count_chekced_sampleDT_Diabetes) {
      diabetes[random3, ]
    }
  }, 
  style = "bootstrap4", 
  rownames = TRUE, 
  options = list(
    responsive = TRUE, 
    scrollX = TRUE, 
    paging = FALSE, 
    searching = FALSE, 
    ordering = FALSE, 
    dom = 't'
  ))
  #Level3 - Imputation method ----
  observeEvent(input$imp_methods, {
    #Iris dataset
    if (input$inputLevel3 == 'Iris') {
      if (input$imp_methods == 'Complete Case Analysis') {
        output$reg_summary <- DT::renderDT({
          reg2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species,
                    data = iris1)
          sum_coef3 <- summary(reg2)[4]
          Variables <-
            c('(intercept)', 'Sepal.Width', 'Petal.Length', 'Speciesversicolor', 
              'Speciesvirginica')
          Estimates <- c(format(sum_coef3$coefficients[1], digits = 2), 
                         format(sum_coef3$coefficients[2], digits = 2), 
                         format(sum_coef3$coefficients[3], digits = 2), 
                         format(sum_coef3$coefficients[4], digits = 2), 
                         format(sum_coef3$coefficients[5], digits = 2))
          Std.Error <- c(format(sum_coef3$coefficients[6], digits = 2), 
                         format(sum_coef3$coefficients[7], digits = 2), 
                         format(sum_coef3$coefficients[8], digits = 2),
                         format(sum_coef3$coefficients[9], digits = 2),
                         format(sum_coef3$coefficients[10], digits = 2))
          t_value <- c(format(sum_coef3$coefficients[11], digits = 2), 
                       format(sum_coef3$coefficients[12], digits = 2), 
                       format(sum_coef3$coefficients[13], digits = 2), 
                       format(sum_coef3$coefficients[14], digits = 2), 
                       format(sum_coef3$coefficients[15], digits = 2))
          p_value <- c(format(sum_coef3$coefficients[16], digits = 2), 
                       format(sum_coef3$coefficients[17], digits = 2), 
                       format(sum_coef3$coefficients[18], digits = 2), 
                       format(sum_coef3$coefficients[19], digits = 2), 
                       format(sum_coef3$coefficients[20], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        }, 
        style = "bootstrap4",
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values <- DT::renderDT({
          iris1[random1, ]
        }, 
        style = "bootstrap4",
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
      else if (input$imp_methods == 'Mean') {
        output$reg_summary <- DT::renderDT({
          irisMean = iris1
          Species <- irisMean$Species
          irisMean$Species <- NULL
          for (i in 1:ncol(irisMean)) {
            irisMean[is.na(irisMean[, i]), i] <-
              mean(irisMean[, i], na.rm = TRUE)
          }
          irisMean2 <- round(irisMean, 1)
          irisMean2$Species <- Species
          reg_mean = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species,
                        data = irisMean2)
          sum_coef4 <- summary(reg_mean)[4]
          Variables <-
            c('(intercept)', 'Sepal.Width', 'Petal.Length', 'Speciesversicolor', 
              'Speciesvirginica')
          Estimates <- c(format(sum_coef4$coefficients[1], digits = 2), 
                         format(sum_coef4$coefficients[2], digits = 2), 
                         format(sum_coef4$coefficients[3], digits = 2), 
                         format(sum_coef4$coefficients[4], digits = 2), 
                         format(sum_coef4$coefficients[5], digits = 2))
          Std.Error <- c(format(sum_coef4$coefficients[6], digits = 2), 
                         format(sum_coef4$coefficients[7], digits = 2), 
                         format(sum_coef4$coefficients[8], digits = 2),
                         format(sum_coef4$coefficients[9], digits = 2),
                         format(sum_coef4$coefficients[10], digits = 2))
          t_value <- c(format(sum_coef4$coefficients[11], digits = 2), 
                       format(sum_coef4$coefficients[12], digits = 2), 
                       format(sum_coef4$coefficients[13], digits = 2), 
                       format(sum_coef4$coefficients[14], digits = 2), 
                       format(sum_coef4$coefficients[15], digits = 2))
          p_value <- c(format(sum_coef4$coefficients[16], digits = 2), 
                       format(sum_coef4$coefficients[17], digits = 2), 
                       format(sum_coef4$coefficients[18], digits = 2), 
                       format(sum_coef4$coefficients[19], digits = 2), 
                       format(sum_coef4$coefficients[20], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values <- DT::renderDT({
          irisMean = iris1
          Species <- irisMean$Species
          irisMean$Species <- NULL
          for (i in 1:ncol(irisMean)) {
            irisMean[is.na(irisMean[, i]), i] <-
              mean(irisMean[, i], na.rm = TRUE)
          }
          irisMean2 <- round(irisMean, 1)
          irisMean2$Species <- Species
          irisMean2[random1, ]
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
      else if (input$imp_methods == 'Fill in 0s') {
        output$reg_summary <- DT::renderDT({
          iris_zeros = iris1
          Species <- iris_zeros$Species
          iris_zeros$Species <- NULL
          for (i in 1:ncol(iris_zeros)) {
            iris_zeros[is.na(iris_zeros[, i]), i] <- 0
          }
          iris_zeros2 <- round(iris_zeros, 1)
          iris_zeros2$Species <- Species
          reg_zeros = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, 
                         data = iris_zeros2)
          sum_coef5 <- summary(reg_zeros)[4]
          Variables <-
            c('(intercept)', 'Sepal.Width', 'Petal.Length', 'Speciesversicolor', 
              'Speciesvirginica')
          Estimates <- c(format(sum_coef5$coefficients[1], digits = 2), 
                         format(sum_coef5$coefficients[2], digits = 2), 
                         format(sum_coef5$coefficients[3], digits = 2), 
                         format(sum_coef5$coefficients[4], digits = 2), 
                         format(sum_coef5$coefficients[5], digits = 2))
          Std.Error <- c(format(sum_coef5$coefficients[6], digits = 2), 
                         format(sum_coef5$coefficients[7], digits = 2), 
                         format(sum_coef5$coefficients[8], digits = 2),
                         format(sum_coef5$coefficients[9], digits = 2),
                         format(sum_coef5$coefficients[10], digits = 2))
          t_value <- c(format(sum_coef5$coefficients[11], digits = 2), 
                       format(sum_coef5$coefficients[12], digits = 2), 
                       format(sum_coef5$coefficients[13], digits = 2), 
                       format(sum_coef5$coefficients[14], digits = 2), 
                       format(sum_coef5$coefficients[15], digits = 2))
          p_value <- c(format(sum_coef5$coefficients[16], digits = 2), 
                       format(sum_coef5$coefficients[17], digits = 2), 
                       format(sum_coef5$coefficients[18], digits = 2), 
                       format(sum_coef5$coefficients[19], digits = 2), 
                       format(sum_coef5$coefficients[20], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        },
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values <- DT::renderDT({
          iris_zeros = iris1
          Species <- iris_zeros$Species
          iris_zeros$Species <- NULL
          for (i in 1:ncol(iris_zeros)) {
            iris_zeros[is.na(iris_zeros[, i]), i] <- 0
          }
          iris_zeros2 <- round(iris_zeros, 1)
          iris_zeros2$Species <- Species
          iris_zeros2[random1, ]
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
      else if (input$imp_methods == 'MICE') {
        #MICE method ----
        require(mice)
        imp = mice(iris1, seed = 1275)
        output$reg_summary <- DT::renderDT({
          fit1 = with(imp, lm(Sepal.Length ~ Sepal.Width + 
                                Petal.Length + Species))
          pooled = pool(fit1)
          sum_coef2 <- summary(pooled)
          Variables <-
            c('(intercept)', 'Sepal.Width', 'Petal.Length', 'Speciesversicolor',
              'Speciesvirginica')
          Estimates <- c(format(sum_coef2$estimate[1], digits = 2), 
                         format(sum_coef2$estimate[2], digits = 2), 
                         format(sum_coef2$estimate[3], digits = 2), 
                         format(sum_coef2$estimate[4], digits = 2), 
                         format(sum_coef2$estimate[5], digits = 2))
          Std.Error <- c(format(sum_coef2$std.error[1], digits = 2), 
                         format(sum_coef2$std.error[2], digits = 2), 
                         format(sum_coef2$std.error[3], digits = 2), 
                         format(sum_coef2$std.error[4], digits = 2), 
                         format(sum_coef2$std.error[5], digits = 2))
          t_value <- c(format(sum_coef2$statistic[1], digits = 2), 
                       format(sum_coef2$statistic[2], digits = 2), 
                       format(sum_coef2$statistic[3], digits = 2), 
                       format(sum_coef2$statistic[4], digits = 2), 
                       format(sum_coef2$statistic[5], digits = 2))
          p_value <- c(format(sum_coef2$p.value[1], digits = 2), 
                       format(sum_coef2$p.value[2], digits = 2), 
                       format(sum_coef2$p.value[3], digits = 2), 
                       format(sum_coef2$p.value[4], digits = 2), 
                       format(sum_coef2$p.value[5], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        },
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values <- DT::renderDT({
          imp_table <- rbind(imp$imp$Sepal.Length, imp$imp$Sepal.Width, 
                             imp$imp$Petal.Length, imp$imp$Petal.Width, 
                             imp$imp$Species)
          imp_table
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
    }
    else if (input$inputLevel3 == 'Diabetes') {
      #Diabetes Dataset
      if (input$imp_methods == 'Complete Case Analysis') {
        output$reg_summary2 <- DT::renderDT({
          diabetes1 = diabetes
          reg2 = lm(Age ~ Pregnancies +Glucose + BloodPressure + SkinThickness
                    + Insulin + BMI + DiabetesPedigreeFunction, data = diabetes1)
          sum_coef3 <- summary(reg2)[4]
          Variables <-
            c('(intercept)', 'Pregnancies', 'Glucose', 'BloodPressure',
              'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction')
          Estimates <- c(format(sum_coef3$coefficients[1], digits = 2), 
                         format(sum_coef3$coefficients[2], digits = 2), 
                         format(sum_coef3$coefficients[3], digits = 2), 
                         format(sum_coef3$coefficients[4], digits = 2), 
                         format(sum_coef3$coefficients[5], digits = 2), 
                         format(sum_coef3$coefficients[6], digits = 2), 
                         format(sum_coef3$coefficients[7], digits = 2), 
                         format(sum_coef3$coefficients[8], digits = 2))
          Std.Error <- c(format(sum_coef3$coefficients[9], digits = 2), 
                         format(sum_coef3$coefficients[10], digits = 2), 
                         format(sum_coef3$coefficients[11], digits = 2), 
                         format(sum_coef3$coefficients[12], digits = 2), 
                         format(sum_coef3$coefficients[13], digits = 2), 
                         format(sum_coef3$coefficients[14], digits = 2), 
                         format(sum_coef3$coefficients[15], digits = 2), 
                         format(sum_coef3$coefficients[16], digits = 2))
          t_value <- c(format(sum_coef3$coefficients[17], digits = 2), 
                       format(sum_coef3$coefficients[18], digits = 2), 
                       format(sum_coef3$coefficients[19], digits = 2), 
                       format(sum_coef3$coefficients[20], digits = 2), 
                       format(sum_coef3$coefficients[21], digits = 2), 
                       format(sum_coef3$coefficients[22], digits = 2), 
                       format(sum_coef3$coefficients[23], digits = 2), 
                       format(sum_coef3$coefficients[24], digits = 2))
          p_value <- c(format(sum_coef3$coefficients[25], digits = 2), 
                       format(sum_coef3$coefficients[26], digits = 2), 
                       format(sum_coef3$coefficients[27], digits = 2), 
                       format(sum_coef3$coefficients[28], digits = 2), 
                       format(sum_coef3$coefficients[29], digits = 2), 
                       format(sum_coef3$coefficients[30], digits = 2), 
                       format(sum_coef3$coefficients[31], digits = 2), 
                       format(sum_coef3$coefficients[32], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values2 <- DT::renderDT({
          diabetes[random3, ]
        }, 
        style = "bootstrap4",
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
      else if (input$imp_methods == 'Mean') {
        output$reg_summary2 <- DT::renderDT({
          diabetes_mean = diabetes
          for (i in 1:ncol(diabetes_mean)) {
            diabetes_mean[is.na(diabetes_mean[, i]), i] <-
              mean(diabetes_mean[, i], na.rm = TRUE)
          }
          diabetes_mean2 <- round(diabetes_mean, 1)
          reg_mean = lm(Age ~ Pregnancies + Glucose + BloodPressure +
                          SkinThickness + Insulin + BMI + 
                          DiabetesPedigreeFunction, data = diabetes_mean2)
          sum_coef4 <- summary(reg_mean)[4]
          Variables <-
            c('(intercept)', 'Pregnancies', 'Glucose', 'BloodPressure',
              'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction')
          Estimates <- c(format(sum_coef4$coefficients[1], digits = 2), 
                         format(sum_coef4$coefficients[2], digits = 2), 
                         format(sum_coef4$coefficients[3], digits = 2), 
                         format(sum_coef4$coefficients[4], digits = 2), 
                         format(sum_coef4$coefficients[5], digits = 2), 
                         format(sum_coef4$coefficients[6], digits = 2), 
                         format(sum_coef4$coefficients[7], digits = 2), 
                         format(sum_coef4$coefficients[8], digits = 2))
          Std.Error <- c(format(sum_coef4$coefficients[9], digits = 2), 
                         format(sum_coef4$coefficients[10], digits = 2), 
                         format(sum_coef4$coefficients[11], digits = 2), 
                         format(sum_coef4$coefficients[12], digits = 2), 
                         format(sum_coef4$coefficients[13], digits = 2), 
                         format(sum_coef4$coefficients[14], digits = 2), 
                         format(sum_coef4$coefficients[15], digits = 2), 
                         format(sum_coef4$coefficients[16], digits = 2))
          t_value <- c(format(sum_coef4$coefficients[17], digits = 2), 
                       format(sum_coef4$coefficients[18], digits = 2), 
                       format(sum_coef4$coefficients[19], digits = 2), 
                       format(sum_coef4$coefficients[20], digits = 2), 
                       format(sum_coef4$coefficients[21], digits = 2), 
                       format(sum_coef4$coefficients[22], digits = 2), 
                       format(sum_coef4$coefficients[23], digits = 2), 
                       format(sum_coef4$coefficients[24], digits = 2))
          p_value <- c(format(sum_coef4$coefficients[25], digits = 2), 
                       format(sum_coef4$coefficients[26], digits = 2), 
                       format(sum_coef4$coefficients[27], digits = 2), 
                       format(sum_coef4$coefficients[28], digits = 2), 
                       format(sum_coef4$coefficients[29], digits = 2), 
                       format(sum_coef4$coefficients[30], digits = 2), 
                       format(sum_coef4$coefficients[31], digits = 2), 
                       format(sum_coef4$coefficients[32], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values2 <- DT::renderDT({
          diabetes_mean = diabetes
          for (i in 1:ncol(diabetes_mean)) {
            diabetes_mean[is.na(diabetes_mean[, i]), i] <-
              mean(diabetes_mean[, i], na.rm = TRUE)
          }
          diabetes_mean2 <- round(diabetes_mean, 1)
          diabetes_mean2[random3, ]
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
      else if (input$imp_methods == 'Fill in 0s') {
        output$reg_summary2 <- DT::renderDT({
          diabetes_zeros = diabetes
          for (i in 1:ncol(diabetes_zeros)) {
            diabetes_zeros[is.na(diabetes_zeros[, i]), i] <- 0
          }
          diabetes_zeros2 <- round(diabetes_zeros, 1)
          reg_zeros = lm(Age ~ Pregnancies + Glucose + BloodPressure +
                           SkinThickness + Insulin + BMI +
                           DiabetesPedigreeFunction, data = diabetes_zeros2)
          sum_coef5 <- summary(reg_zeros)[4]
          Variables <-
            c('(intercept)', 'Pregnancies', 'Glucose', 'BloodPressure',
              'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction')
          Estimates <- c(format(sum_coef5$coefficients[1], digits = 2), 
                         format(sum_coef5$coefficients[2], digits = 2), 
                         format(sum_coef5$coefficients[3], digits = 2), 
                         format(sum_coef5$coefficients[4], digits = 2), 
                         format(sum_coef5$coefficients[5], digits = 2), 
                         format(sum_coef5$coefficients[6], digits = 2), 
                         format(sum_coef5$coefficients[7], digits = 2), 
                         format(sum_coef5$coefficients[8], digits = 2))
          Std.Error <- c(format(sum_coef5$coefficients[9], digits = 2), 
                         format(sum_coef5$coefficients[10], digits = 2), 
                         format(sum_coef5$coefficients[11], digits = 2), 
                         format(sum_coef5$coefficients[12], digits = 2), 
                         format(sum_coef5$coefficients[13], digits = 2), 
                         format(sum_coef5$coefficients[14], digits = 2), 
                         format(sum_coef5$coefficients[15], digits = 2), 
                         format(sum_coef5$coefficients[16], digits = 2))
          t_value <- c(format(sum_coef5$coefficients[17], digits = 2), 
                       format(sum_coef5$coefficients[18], digits = 2), 
                       format(sum_coef5$coefficients[19], digits = 2), 
                       format(sum_coef5$coefficients[20], digits = 2), 
                       format(sum_coef5$coefficients[21], digits = 2), 
                       format(sum_coef5$coefficients[22], digits = 2), 
                       format(sum_coef5$coefficients[23], digits = 2), 
                       format(sum_coef5$coefficients[24], digits = 2))
          p_value <- c(format(sum_coef5$coefficients[25], digits = 2), 
                       format(sum_coef5$coefficients[26], digits = 2), 
                       format(sum_coef5$coefficients[27], digits = 2), 
                       format(sum_coef5$coefficients[28], digits = 2), 
                       format(sum_coef5$coefficients[29], digits = 2), 
                       format(sum_coef5$coefficients[30], digits = 2), 
                       format(sum_coef5$coefficients[31], digits = 2), 
                       format(sum_coef5$coefficients[32], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values2 <- DT::renderDT({
          diabetes_zeros = diabetes
          for (i in 1:ncol(diabetes_zeros)) {
            diabetes_zeros[is.na(diabetes_zeros[, i]), i] <- 0
          }
          diabetes_zeros2 <- round(diabetes_zeros, 1)
          diabetes_zeros2[random3, ]
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
      else if (input$imp_methods == 'MICE') {
        require(mice)
        imp = mice(diabetes, seed = 1275)
        output$reg_summary2 <- DT::renderDT({
          fit1 = with(imp, lm(Age ~ Pregnancies + Glucose + BloodPressure +
                                SkinThickness + Insulin + BMI + 
                                DiabetesPedigreeFunction))
          pooled = pool(fit1)
          sum_coef2 <- summary(pooled)
          Variables <-
            c('(intercept)','Pregnancies','Glucose','BloodPressure',
              'SkinThickness', 'Insulin', 'BMI','DiabetesPedigreeFunction')
          Estimates <- c(format(sum_coef2$estimate[1], digits = 2), 
                         format(sum_coef2$estimate[2], digits = 2), 
                         format(sum_coef2$estimate[3], digits = 2), 
                         format(sum_coef2$estimate[4], digits = 2), 
                         format(sum_coef2$estimate[5], digits = 2), 
                         format(sum_coef2$estimate[6], digits = 2), 
                         format(sum_coef2$estimate[7], digits = 2), 
                         format(sum_coef2$estimate[8], digits = 2))
          Std.Error <- c(format(sum_coef2$std.error[1], digits = 2), 
                         format(sum_coef2$std.error[2], digits = 2), 
                         format(sum_coef2$std.error[3], digits = 2), 
                         format(sum_coef2$std.error[4], digits = 2), 
                         format(sum_coef2$std.error[5], digits = 2), 
                         format(sum_coef2$std.error[6], digits = 2), 
                         format(sum_coef2$std.error[7], digits = 2), 
                         format(sum_coef2$std.error[8], digits = 2))
          t_value <- c(format(sum_coef2$statistic[1], digits = 2), 
                       format(sum_coef2$statistic[2], digits = 2), 
                       format(sum_coef2$statistic[3], digits = 2), 
                       format(sum_coef2$statistic[4], digits = 2), 
                       format(sum_coef2$statistic[5], digits = 2), 
                       format(sum_coef2$statistic[6], digits = 2), 
                       format(sum_coef2$statistic[7], digits = 2), 
                       format(sum_coef2$statistic[8], digits = 2))
          p_value <- c(format(sum_coef2$p.value[1], digits = 2), 
                       format(sum_coef2$p.value[2], digits = 2), 
                       format(sum_coef2$p.value[3], digits = 2), 
                       format(sum_coef2$p.value[4], digits = 2), 
                       format(sum_coef2$p.value[5], digits = 2), 
                       format(sum_coef2$p.value[6], digits = 2), 
                       format(sum_coef2$p.value[7], digits = 2), 
                       format(sum_coef2$p.value[8], digits = 2))
          as.data.frame(cbind(Variables, Estimates, Std.Error, t_value, p_value))
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
        #Imputed values
        output$imp_values2 <- DT::renderDT({
          imp_table <- rbind(imp$imp$Glucose, imp$imp$BloodPressure, 
                             imp$imp$SkinThickness, imp$imp$Insulin, 
                             imp$imp$BMI)
          head(imp_table,20)
        }, 
        style = "bootstrap4", 
        rownames = TRUE, 
        options = list(
          responsive = TRUE, 
          scrollX = TRUE, 
          paging = FALSE, 
          searching = FALSE, 
          ordering = FALSE, 
          dom = 't'
        ))
      }
    }
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)




