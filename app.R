library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(magrittr)
library(caret)
library(factoextra)

#---build model--------------------------------------------------------
data <- read.csv('breast-cancer-wisconsin-cleaned.csv', header=TRUE)
#split into train/test
set.seed(42)
trainIndex <- createDataPartition(data$outcome, p = .8, list = FALSE, times = 1)
traind <- data.frame(data[ trainIndex, ])
testd <- data.frame(data[ -trainIndex, ])
#fit a Random Forest model
fit_rf <- train(as.factor(outcome) ~ ., data = traind, metric = "Accuracy")
fit_rf$finalModel
#fit metrics
train_predict<- predict(fit_rf,traind)
test_predict<- predict(fit_rf,testd)
results_train <- confusionMatrix(train_predict, as.factor(traind$outcome))
results_test <- confusionMatrix(test_predict, as.factor(testd$outcome))
acc_train <- round(as.numeric(results_train$overall[1])*100,2)
acc_test <- round(as.numeric(results_test$overall[1])*100,2)
oob <- round(100*mean(fit_rf$finalModel$err.rate[, 'OOB']),2)
#feature importance
varImportance <- varImp(fit_rf, scale = FALSE)
varImportanceScores <- data.frame(varImportance$importance)
varImportanceScores <- data.frame(Name = row.names(varImportanceScores), Score = varImportanceScores$Overall)
varImportanceScores <- arrange(varImportanceScores,desc(Score))
train_features <- traind %>% select(- outcome)

ui <- fluidPage(
    
    titlePanel("A Random Forest model for the Wisconsin Cancer Dataset"),
    
    useShinydashboard(),
    fluidRow(
        valueBox(
            uiOutput('acc_train'), 'Accuracy (train dataset)', color= 'navy', width = 4),
        valueBox(
            uiOutput('acc_test'), 'Accuracy (test dataset)', color= 'navy', width = 4),
        valueBox(
            uiOutput('oob'), 'Out Of Bag error rate ', color= 'navy', width = 4),
    ),
    fluidRow(
        box(title = "Feature importance",
            status = "primary",solidHeader = TRUE, collapsible = FALSE, width = 6,
            tableOutput('feature_importance')),
        box(title = 'Confusion maxtrix (test dataset)', 
            status = "primary",solidHeader = TRUE, collapsible = FALSE, width = 6,
            tableOutput('confusion_matrix'), " 0 = Benign ", br(), "1 = Malignant ")
    ),
    fluidRow(
        box(title  = "K means clustering ",
            status = "primary",solidHeader = TRUE, collapsible = FALSE, width = 12,
            numericInput('clusters', 'How many clusters?', 3, min = 1, max = 10),
            plotOutput('cluster_plot')
            )
    ),
    fluidRow(
        box(title = "Feature mean per cluster",
            status = "primary",solidHeader = TRUE, collapsible = FALSE, width = 12,
            tableOutput("summary")
        )
    )
)

    
server <- function(input, output, session) {
    output$acc_train <- renderText(paste(acc_train,"%"))
    output$acc_test <- renderText(paste(acc_test,"%"))
    output$oob <- renderText(paste(oob,"%"))
    output$feature_importance <- renderTable(varImportanceScores)
    output$confusion_matrix <- renderTable (results_test$table)
    #cluster teh training data 
    k <- reactive(input$clusters)
    kclusters <- reactive(kmeans(train_features, centers = k(), nstart = 25))
    output$cluster_plot <- renderPlot(fviz_cluster(kclusters(), data = train_features)+xlab('PCA dim 1')+ylab('PCA dim 2'))
    #summarize features per cluster
    summary_cluster<- reactive({traind %>%
        mutate(Cluster = kclusters()$cluster) %>%
        group_by(Cluster) %>%
        summarise_all("mean")})
    output$summary <- renderTable(summary_cluster())
}

shinyApp(ui, server)
























