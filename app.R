library(shiny)
library(ggplot2)

# https://en.wikipedia.org/wiki/Sensitivity_and_specificity

formatPct = function(number, rounding=1){
    return(paste0(as.character(round(number*100,rounding)),"%"))
}
ui <- fluidPage(
    titlePanel("Sensitivity and Specificity of tests"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("significance",
                        "Significance:",
                        min = 1,
                        max = 100,
                        value = 4.4),
            sliderInput("power",
                        "Power:",
                        min = 1,
                        max = 100,
                        value = 6.2),
            # sliderInput("nsize",
            #             "Population size:",
            #             min = 1000,
            #             max = 10000,
            #             value = 2030),
            numericInput("incidence", label = "Prevalence", value = 10)
        ),

        mainPanel(
            tableOutput("values"),
            plotOutput("ggplot"),
            plotOutput("ranges")
        )
    )
)

server <- function(input, output) {
    
    populationSize = 5000 # as.double(input$nsize)
    pts = runif(populationSize,0, 100)
    
    allValues <- reactive({
        #
        specificity = 1 - as.double(input$significance) / 100
        sensitivity = 1 - as.double(input$power) / 100
        prevalence = as.double(input$incidence) / 100
        truePositiveRatio = prevalence * sensitivity
        falsePositiveRatio = (1 - prevalence) * (1 - specificity)
        falseNegativeRatio = prevalence * (1 - sensitivity)
        trueNegativeRatio = (1 - prevalence) * specificity
        #
        truePositive = truePositiveRatio * populationSize
        falsePositive = falsePositiveRatio * populationSize
        falseNegative = falseNegativeRatio * populationSize
        trueNegative = trueNegativeRatio * populationSize
        #
        positivePredictiveValue = truePositiveRatio / (truePositiveRatio + falsePositiveRatio)
        falseDiscoveryRate = falsePositiveRatio / (truePositiveRatio + falsePositiveRatio)
        negativePredictiveValue = trueNegativeRatio / (trueNegativeRatio + falseNegativeRatio)
        falseOmissionRate = falseNegativeRatio / (trueNegativeRatio + falseNegativeRatio)
        #
        estimatedSensitivity = truePositiveRatio / (truePositiveRatio + falseNegativeRatio)
        fallOut = falsePositiveRatio / (falsePositiveRatio + trueNegativeRatio)
        missRate = falseNegativeRatio / (truePositiveRatio + falseNegativeRatio)
        estimatedSpecificity = trueNegativeRatio / (falsePositiveRatio + trueNegativeRatio)
        #
        likelihoodRatioPositive = sensitivity / (1 - specificity)
        likelihoodRatioNegative = (1-sensitivity)/specificity
        #
        diagnosticOddsRatio = likelihoodRatioPositive / likelihoodRatioNegative
        f1Score = 2*(positivePredictiveValue * truePositiveRatio)/(positivePredictiveValue + truePositiveRatio)
        
        ## ranges
        prevalenceRange = seq(from=0.1, to=100, by=0.1)/100
        truePositiveRangeRatio = prevalenceRange * sensitivity
        falsePositiveRangeRatio = (1 - prevalenceRange) * (1 - specificity)
        falseNegativeRangeRatio = prevalenceRange * (1 - sensitivity)
        trueNegativeRangeRatio = (1 - prevalenceRange) * specificity
        positivePredictiveRangeValue = truePositiveRangeRatio / (truePositiveRangeRatio + falsePositiveRangeRatio)
        negativePredictiveRangeValue = trueNegativeRangeRatio / (trueNegativeRangeRatio + falseNegativeRangeRatio)
        ranges = data.frame(prevalence = prevalenceRange, positivePredictive = positivePredictiveRangeValue, negativePredictive = negativePredictiveRangeValue)
        current = data.frame(prevalence = prevalence, positivePredictiveValue = positivePredictiveValue, negativePredictiveValue = negativePredictiveValue)
        graphRanges = ggplot(ranges) +
            geom_line(aes(x=prevalence, y = positivePredictive, color="Positive Predictive")) + 
            geom_line(aes(x=prevalence, y=negativePredictive, color="Negative Predictive")) +
            geom_point(data = current, aes(x=prevalence, y=positivePredictiveValue, color="positive predictive value", size=10)) +
            geom_point(data = current, aes(x=prevalence, y=negativePredictiveValue, color="negative predictive value", size=10)) + theme(legend.position = "bottom")
        
        ##
        numbers = data.frame(
            Name = c("True positive",
                     "False positive",
                     "False negative",
                     "True negative"),
            Numbers = as.character(c(round(truePositive),
                                   round(falsePositive),
                                   round(falseNegative),
                                   round(trueNegative))),
            Rates = c("Positive Prediction Value",
                     "False Discovery Rate",
                     "Negative Prediction Value",
                     "False Omission Rate"),
            Values = c(formatPct(positivePredictiveValue),
                           formatPct(falseDiscoveryRate),
                           formatPct(negativePredictiveValue),
                           formatPct(falseOmissionRate)),
            stringsAsFactors = FALSE)
        ##
        n = round(populationSize*prevalence)
        n1 = round(truePositive) # truePositive
        n2 = round(falseNegative) #n - n1 # falseNegative
        n3 = round(falsePositive) # falsePositive
        n4 = round(trueNegative) # populationSize - n1 - n2 -n3 #trueNegative
        status1 = ifelse(1:n %in% sample(1:n,n1),"True Positive","False Negative")
        status2 = ifelse((n+1):populationSize %in% sample((n+1):populationSize,n4),"True Negative","False Positive")
        status = c(status1, status2)
        df = data.frame(index = 1:populationSize, pts = pts, status = status)
        graph = ggplot(df, aes_string(x="index", y="pts", color="status")) + geom_point() +
            annotate("rect", xmin = 0, xmax = n, ymin = 0, ymax = 100, alpha = .2) + theme(legend.position = "bottom") +
            scale_color_manual(values=c("#333333","#F8766D","#619CFF","#00BA38"))
        
        list(numbers = numbers, graph = graph, ranges = graphRanges)
        
    })
    
    output$values <- renderTable({
        allValues()$numbers
    })
    
    output$ggplot <- renderPlot({
        allValues()$graph
    })
    
    output$ranges <- renderPlot({
        allValues()$ranges
    })
}

shinyApp(ui = ui, server = server)