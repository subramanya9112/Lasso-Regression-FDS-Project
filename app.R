library(DT)
library(ggplot2)
library(glmnet)
library(hrbrthemes)
library(Metrics)
library(RColorBrewer)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)

orginal_dataset <- read.csv("kc_house_data.csv")
developers <- read.csv("Developers.csv")

columuns_to_drop <- c('id',"date", 'yr_built', 'yr_renovated', 'zipcode', 'lat', 'long')
colomuns_to_keep <- c('price', 'bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors',
                      'waterfront', 'view', 'condition', 'grade', 'sqft_above', 'sqft_basement',
                      'sqft_living15', 'sqft_lot15')
dataset <- orginal_dataset[ , -which(names(orginal_dataset) %in% columuns_to_drop)]

ui <- fluidPage(
    tags$head(tags$style("
                  #getMSE, #getCVM, #getRMSE, #getR2Score, #getAdjustedR2Score, #getPrice {
                    display: inline
                  }")),
    tags$head(tags$style("
                  .price {
                      text-align: center
                  }")),
    navbarPage("FDS Project", theme = shinytheme("lumen"),
        tabPanel("Home", icon=icon("fas fa-home"),
            HTML("<center><h2>Lasso Regression</h2></center>"),
            p("In statistics and machine learning, lasso (least absolute shrinkage and selection operator; also Lasso or LASSO) is a regression analysis method that performs both variable selection and regularization in order to enhance the prediction accuracy and interpretability of the statistical model it produces. It was originally introduced in geophysics literature in 1986, and later independently rediscovered and popularized in 1996 by Robert Tibshirani, who coined the term and provided further insights into the observed performance."), 
            br(),
            p("Lasso was originally formulated for linear regression models and this simple case reveals a substantial amount about the behavior of the estimator, including its relationship to ridge regression and best subset selection and the connections between lasso coefficient estimates and so-called soft thresholding. It also reveals that (like standard linear regression) the coefficient estimates do not need to be unique if covariates are collinear."),
            br(),
            p("Though originally defined for linear regression, lasso regularization is easily extended to a wide variety of statistical models including generalized linear models, generalized estimating equations, proportional hazards models, and M-estimators, in a straightforward fashion. Lasso’s ability to perform subset selection relies on the form of the constraint and has a variety of interpretations including in terms of geometry, Bayesian statistics, and convex analysis."),
            br(),
            p("The LASSO is closely related to basis pursuit denoising."),
        ),
        navbarMenu("Plots", icon = icon("fas fa-project-diagram"),
            tabPanel("One Param Plots", fluid = TRUE,
                sidebarLayout(
                    sidebarPanel(
                        titlePanel("One Param Plots"),
                        # Plot type
                        selectInput("oneParamPlotType", h3("Plot type"), 
                            choices = list("Histogram" = 1, "Box Plot" = 2), selected = 1),
                        selectInput("paramInput", h3("Param"), 
                            choices = as.list(colomuns_to_keep), selected = 1)
                    ),
                    mainPanel(
                        withSpinner(plotOutput("oneParamPlot", width="auto")),
                    )
                )
            ),
            tabPanel("Two Param Plots", fluid = TRUE,
                sidebarLayout(
                    sidebarPanel(
                        titlePanel("Two Param Plots"),
                        # Plot type
                        selectInput("twoParamPlotType", h3("Plot type"), 
                            choices = list("Scatter Plot" = 1, "Density 2D" = 2), selected = 1),
                        fluidRow(
                            column(6, 
                                selectInput("fistparamInput", h3("First Param"), 
                                    choices = as.list(colomuns_to_keep), selected = 1)
                            ),
                            column(6,
                                selectInput("secondparamInput", h3("Second Param"), 
                                    choices = as.list(colomuns_to_keep), selected = 1)
                            )
                        ),
                        hr(),
                        titlePanel("Parameters"),
                        sliderInput("price", h4("Price"), min=78000, max=7700000, step=1000, 
                            value=c(78000, 7700000)),
                        fluidRow(
                            column(6,
                                sliderInput("bedrooms", h4("Bedrooms"), min=1, max=33, step=1, 
                                    value=c(1, 33)),
                                sliderInput("sqft_living", h4("Sqft living"), min=370, max=13540, 
                                    step=1, value=c(370, 13540)),
                                pickerInput(
                                    inputId = "floors",
                                    label = "Floors",
                                    choices = c(1, 1.5, 2, 2.5, 3, 3.5),
                                    selected = c(1, 1.5, 2, 2.5, 3, 3.5),
                                    options = list(
                                        'actions-box' = TRUE,
                                        size = 5,
                                        'selected-text-format' = "count > 3"
                                    ),
                                    multiple = TRUE
                                ),
                                pickerInput(
                                    inputId = "view",
                                    label = "View",
                                    choices = c(0, 1, 2, 3, 4),
                                    selected = c(0, 1, 2, 3, 4),
                                    options = list(
                                        'actions-box' = TRUE,
                                        'selected-text-format' = "count > 3"
                                    ),
                                    multiple = TRUE
                                ),
                                pickerInput(
                                    inputId = "condition",
                                    label = "Conditions",
                                    choices = c(1, 2, 3, 4, 5),
                                    selected = c(1, 2, 3, 4, 5),
                                    options = list(
                                        'actions-box' = TRUE,
                                        'selected-text-format' = "count > 3"
                                    ),
                                    multiple = TRUE
                                ),
                                sliderInput("sqft_basement", h4("Sqft basement"), min=0, max=4820, 
                                    step=1, value=c(0, 4820)),
                                sliderInput("sqft_lot15", h4("Sqft lot15"), min=651, max=871200, 
                                    step=1, value=c(651, 871200))
                            ),
                            column(6,
                                sliderInput("bathrooms", h4("Bathrooms"), min=0.5, max=8, step=0.5, 
                                    value=c(0.5, 8)),
                                sliderInput("sqft_lot", h4("Sqft lot"), min=520, max=16513598, step=1, 
                                    value=c(520, 16513598)),
                                pickerInput(
                                    inputId = "waterfront",
                                    label = "Waterfront",
                                    choices = c(0, 1),
                                    selected = c(0, 1),
                                    options = list(
                                        'actions-box' = TRUE
                                    ),
                                    multiple = TRUE
                                ),
                                pickerInput(
                                    inputId = "grade",
                                    label = "Grade",
                                    choices = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                                    selected = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                                    options = list(
                                        'actions-box' = TRUE,
                                        size = 5,
                                        'selected-text-format' = "count > 3"
                                    ),
                                    multiple = TRUE
                                ),
                                sliderInput("sqft_above", h4("Sqft above"), min=370, max=9410, step=1, 
                                    value=c(370, 9410)),
                                sliderInput("sqft_living15", h4("Sqft living15"), min=399, max=6210, 
                                    step=1, value=c(399, 6210))
                            )
                        )
                    ),
                    mainPanel(
                        withSpinner(plotOutput("twoParamPlot", width="auto")),
                        hr(),
                        withSpinner(dataTableOutput("dataset"))
                    )
                )
            ),
            tabPanel("Heat map", fluid = TRUE,
                fluidRow(
                    h4(p("Heat map")),
                    withSpinner(plotOutput("heatmap", width="auto"))
                )
            )
        ),
        tabPanel("Model", icon=icon("fas fa-calculator"), 
            h1("Model accuaracy"),
            br(),
            p("The model mse(mean squared error) =", style="display:inline"),
            textOutput("getMSE"),
            p(", cvm(mean cross-validation error) = ", style="display:inline"),
            textOutput("getCVM"),
            p(", rmse(root mean sqared error) = ", style="display:inline"),
            textOutput("getRMSE"),
            p(", R2 = ", style="display:inline"),
            textOutput("getR2Score"),
            p(" and adjusted R2 = ", style="display:inline"),
            textOutput("getAdjustedR2Score"),
            hr(),
            h1("Price Prediction"),
            fluidRow(
                column(3, 
                    sliderInput("predictbedrooms", label="Bedrooms", 1, 33, value=1),
                    pickerInput(
                        inputId = "predictfloors",
                        label = "Floors",
                        choices = c(1, 1.5, 2, 2.5, 3, 3.5),
                        selected = 1,
                    ),
                    pickerInput(
                        inputId = "predictgrade",
                        label = "Grade",
                        choices = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                        selected = 3,
                    ),
                    sliderInput("predictsqft_lot15", label="Sqft lot15", 651, 871200, value=651)
                ),
                column(3, 
                    sliderInput("predictbathrooms", label="Bathrooms", 0.5, 8, value=0.5),
                    pickerInput(
                        inputId = "predictwaterfront",
                        label = "Waterfront",
                        choices = c(0, 1),
                        selected = 0,
                    ),
                    sliderInput("predictsqft_above", label="Sqft above", 370, 9410, value=370)
                ),
                column(3, 
                    sliderInput("predictsqft_living", label="Sqft living", 370, 13540, value=370),
                    pickerInput(
                        inputId = "predictview",
                        label = "View",
                        choices = c(0, 1, 2, 3, 4),
                        selected = 0,
                    ),
                    sliderInput("predictsqft_basement", label="Sqft basement", 0, 4820, value=0)
                ),
                column(3, 
                    sliderInput("predictsqft_lot", label="Sqft lot", 520, 16513598, value=520),
                    pickerInput(
                        inputId = "predictcondition",
                        label = "Condition",
                        choices = c(1, 2, 3, 4, 5),
                        selected = 1,
                    ),
                    sliderInput("predictsqft_living15", label="Sqft living15", 399, 6210, value=399)
                )
            ),
            br(),
            div(class="price",
                br(),
                br(),
                h4("The price is ", style="display:inline"),
                textOutput("getPrice")
            )
        ),
        tabPanel("About", fluid = TRUE, icon = icon("info-circle"),
            fluidRow(
                column(6,
                    h4(p("About this Project")),
                    h5(p("This project is our Foundation of DataScience project. This project include the", a("KC House Dataset", href = "https://www.kaggle.com/swathiachath/kc-housesales-data"), "from the Kaggle")),
                    h5(p("The project began as an attempt to combine the lasso regression and the Shiny app.")),
                    h5(p("I hope you find it interesting and useful."),
                        p("The source code for this Shiny app is available on ", a("github", href = "https://github.com/subramanya9112/Lasso-Regression-FDS-Project"), "."))
                ),
                column(6,
                    h4(p("About the Creators:")),
                    dataTableOutput('developers')
                )
            ),
            br(),
            hr(),
            h5("Sources:"),
            h6(
                p("Dataset from ", a("Kaggle", href = "https://www.kaggle.com/swathiachath/kc-housesales-data"))
            ),
            h5("Built with",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                "by",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
            ".")
        )
    )
)

server <- function(input, output) {
    getModel <- function() {
        model <- cv.glmnet(data.matrix(dataset[2:14]), data.matrix(dataset[1]),
                                type.measure="mse", alpha = 1, family = "gaussian",
                                nfolds = 100)
        return(model)
    }

    vars <- reactiveValues(data=dataset, model=getModel(), predict=0)

    # Print model accuracy
    # Mean Square Error
    output$getMSE <- reactive({
        format(mse(predict(vars$model, data.matrix(dataset[2:14]), s = vars$model$lambda.1se), 
            data.matrix(dataset[1])), nsmall = 3)
    })

    # Mean Cross Validation Error
    output$getCVM <- reactive({
        format(vars$model$cvm[vars$model$lambda == vars$model$lambda.min], nsmall = 3)
    })

    # Root Mean Squared Error
    output$getRMSE <- reactive({
        mse_value <- mse(predict(vars$model, data.matrix(dataset[2:14]), s = vars$model$lambda.1se), 
            data.matrix(dataset[1]))
        format(sqrt(mse_value), nsmall = 3)
    })

    # R2 score
    output$getR2Score <- reactive({
        rss <- sum((predict(vars$model, data.matrix(dataset[2:14])) - data.matrix(dataset[1])) ^ 2)
        tss <- sum((data.matrix(dataset[1]) - mean(data.matrix(dataset[1]))) ^ 2)
        format(1 - rss / tss, nsmall = 3)
    })

    # Adjusted R2
    output$getAdjustedR2Score <- reactive({
        rss <- sum((predict(vars$model, data.matrix(dataset[2:14])) - data.matrix(dataset[1])) ^ 2)
        tss <- sum((data.matrix(dataset[1]) - mean(data.matrix(dataset[1]))) ^ 2)
        r2 <- 1 - rss / tss
        n <- ncol(dataset[2:14])
        k <- nrow(dataset)
        format(1 - (1 - r2) * (n - 1) / (n - k - 1))
    })

    getPredict <- function() {
        return(predict(getModel(), data.matrix(data.frame(
            bedrooms = input$predictbedrooms,
            bathrooms = input$predictbathrooms,
            sqft_living = input$predictsqft_living,
            sqft_lot = input$predictsqft_lot,
            floors = input$predictfloors,
            waterfront = input$predictwaterfront,
            view = input$predictview,
            condition = input$predictcondition,
            grade = input$predictgrade,
            sqft_above = input$predictsqft_above,
            sqft_basement = input$predictsqft_basement,
            sqft_living15 = input$predictsqft_living15,
            sqft_lot15 = input$predictsqft_lot15
        ))))
    }

    observeEvent(input$predictbedrooms, {vars$predict <- getPredict()})
    observeEvent(input$predictbathrooms, {vars$predict <- getPredict()})
    observeEvent(input$predictsqft_living, {vars$predict <- getPredict()})
    observeEvent(input$predictsqft_lot, {vars$predict <- getPredict()})
    observeEvent(input$predictfloors, {vars$predict <- getPredict()})
    observeEvent(input$predictwaterfront, {vars$predict <- getPredict()})
    observeEvent(input$predictview, {vars$predict <- getPredict()})
    observeEvent(input$predictcondition, {vars$predict <- getPredict()})
    observeEvent(input$predictgrade, {vars$predict <- getPredict()})
    observeEvent(input$predictsqft_above, {vars$predict <- getPredict()})
    observeEvent(input$predictsqft_basement, {vars$predict <- getPredict()})
    observeEvent(input$predictsqft_living15, {vars$predict <- getPredict()})
    observeEvent(input$predictsqft_lot15, {vars$predict <- getPredict()})

    output$getPrice <- reactive({vars$predict})

    # One Param Plot
    output$oneParamPlot <- renderPlot({
        switch(input$oneParamPlotType,
            "1" = hist(
                dataset[, input$paramInput],
                main=paste("Histogram of ", input$paramInput),
                xlab=input$paramInput
            ),
            "2" = boxplot(
                dataset[, input$paramInput],
                main=paste("Box Plot of ", input$paramInput),
                horizontal = TRUE,
                notch = TRUE
            )
        )
    })

    # Two Param Plot
    getData <- function() {
        `%!in%` = Negate(`%in%`)
        newdata <- dataset
        newdata <- newdata[newdata$price >= input$price[1] & newdata$price <=input$price[2],]
        newdata <- newdata[newdata$bedrooms >= input$bedrooms[1] & newdata$bedrooms <=input$bedrooms[2],]
        newdata <- newdata[newdata$sqft_living >= input$sqft_living[1] & newdata$sqft_living <=input$sqft_living[2],]
        newdata <- newdata[newdata$floors %in% input$floors,]
        newdata <- newdata[newdata$view %in% input$view,]
        newdata <- newdata[newdata$sqft_above >= input$sqft_above[1] & newdata$sqft_above <=input$sqft_above[2],]
        newdata <- newdata[newdata$sqft_living15 >= input$sqft_living15[1] & newdata$sqft_living15 <=input$sqft_living15[2],]
        newdata <- newdata[newdata$bathrooms >= input$bathrooms[1] & newdata$bathrooms <=input$bathrooms[2],]
        newdata <- newdata[newdata$sqft_lot >= input$sqft_lot[1] & newdata$sqft_lot <=input$sqft_lot[2],]
        newdata <- newdata[newdata$waterfront %in% input$waterfront,]
        newdata <- newdata[newdata$condition %in% input$condition,]
        newdata <- newdata[newdata$grade %in% input$grade,]
        newdata <- newdata[newdata$sqft_basement >= input$sqft_basement[1] & newdata$sqft_basement <=input$sqft_basement[2],]
        newdata <- newdata[newdata$sqft_lot15 >= input$sqft_lot15[1] & newdata$sqft_lot15 <=input$sqft_lot15[2],]
        return(newdata)
    }

    observeEvent(input$price, {vars$data = getData()})
    observeEvent(input$bedrooms, {vars$data = getData()})
    observeEvent(input$sqft_living, {vars$data = getData()})
    observeEvent(input$floors, {vars$data = getData()})
    observeEvent(input$view, {vars$data = getData()})
    observeEvent(input$sqft_above, {vars$data = getData()})
    observeEvent(input$sqft_living15, {vars$data = getData()})
    observeEvent(input$bathrooms, {vars$data = getData()})
    observeEvent(input$sqft_lot, {vars$data = getData()})
    observeEvent(input$waterfront, {vars$data = getData()})
    observeEvent(input$condition, {vars$data = getData()})
    observeEvent(input$grade, {vars$data = getData()})
    observeEvent(input$sqft_basement, {vars$data = getData()})
    observeEvent(input$sqft_lot15, {vars$data = getData()})

    output$twoParamPlot <- renderPlot({
        switch(input$twoParamPlotType,
            "1" = ggplot(vars$data, aes(x=vars$data[,input$fistparamInput], 
                y=vars$data[,input$secondparamInput])) +
                geom_point(
                    color="black",
                    fill="#69b3a2",
                    shape=22,
                    alpha=0.5,
                    size=6,
                    stroke = 1
                ) +
                theme_ipsum() +
                xlab(input$fistparamInput) +
                ylab(input$secondparamInput),
            "2" = ggplot(vars$data, aes(x=vars$data[,input$fistparamInput], 
                y=vars$data[,input$secondparamInput])) +
                geom_bin2d() +
                theme_bw() +
                xlab(input$fistparamInput) +
                ylab(input$secondparamInput)
        )
    })

    output$dataset <- renderDataTable(
        vars$data,
        options = list(
            pageLength=25,
            searching = FALSE
        )
    )

    # Heat map
    output$heatmap <- renderPlot({
        heatmap(cor(orginal_dataset[3:19]), scale="column", cexRow=1.5, col= colorRampPalette(brewer.pal(8, "Blues"))(25))
    })

    # Developers print
    output$developers <- renderDataTable(developers, options = list(dom = 't'))
}

shinyApp(ui = ui, server = server)
