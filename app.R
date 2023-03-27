
#Loading packages 

library(shiny)
library(randomForest)
library(caret)
library(shinythemes)

#Setting the seed for reproducibility 
set.seed(123)

#Loading iris data set, and splitting the data into training and test sets. 

data(iris)
trainIndex <- createDataPartition(iris$Species, p = .5,  list = FALSE, times = 1)
train_data <- iris[trainIndex,]
test_data <- iris[-trainIndex,]

#UI

ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Iris Species Prediction"),
                
                sidebarLayout(
                    sidebarPanel(
                        h4("Model Training", align = "center"),
                        sliderInput("ntrees", "Number of trees:", value = 500, min = 1, max = 1000),
                        selectInput("cv_method", "Cross Validation Method:", c("oob", "cv", "LOOCV", "LGOCV", "repeatedcv"), selected = "oob"),
                        h4("Prediction Input", align = "center"),
                        numericInput("sepal_length", "Sepal Length:", value = 5),
                        numericInput("sepal_width", "Sepal Width:", value = 1.3),
                        numericInput("petal_length", "Petal Length:", value = 6),
                        numericInput("petal_width", "Petal Width:", value = 0.5)
                    ),
                    
                    
                    
                    
                    mainPanel(
                        h3("Predicted Species:"),
                        verbatimTextOutput("prediction"),
                        h3("Out of bag error rate (%):"),
                        verbatimTextOutput("OOBrate"),
                        h3("Accuracy (%):"),
                        verbatimTextOutput("accuracy"),
                        
                        h3("Welcome", align = "center"),
                        p("Welcome to the Iris species prediction app!"),
                        p("In this application, you can predict the species of your iris flower using a random forest model! You can also alter the parameters of your generated random forest model."),
                        p("The Iris dataset it used, please read the iris documentation for more details."),
                        h3("How to use", align = "center"),
                        p("1. Firstly, use the slider to select the amount of trees you want to use in the random forest model (If this is not of use to you, it can be left at its default of 500)."),
                        p("2. Next, select the cross validation method for training the model using the drop down menu (If this is not of use to you, it can be left at its default of oob).") ,
                        p ("Note: cv uses 10 folds by default, repeatedcv has 1 repeat by default. Please read the caret and randomForests documentation for more information on random forests and cross validation"),
                        
                        p("3. Now, enter your measurment values for your iris flower to predict its species. If of interest to you, the Out of Bag (OOB) error rate is also returned as a percentage, 
                          in short, The OOB error rate is the proportion of training observations that were misclassified. The accuracy is also returned, which is the accuracy of the model against a test set of iris data.")
                        
                        
                        
                    )
                )
)


#UI 

server <- function(input, output) {
    
#This creates the random forest model. The ntrees parameter is equal to what the user selects, 
    #as well as the choice of cross validation in the train control function.     
    
    rf_model <- reactive({
        n_trees <- input$ntrees; 
        cv_method <- input$cv_method; ctrl <- trainControl(method = cv_method)
        rf <- train(Species ~ ., method = "rf", data = train_data, 
                    ntree = n_trees, 
                    trControl = ctrl)
        
            return(rf)
       
    })
    
#This creates a new data set to predict a species from, using the data the user inputted.
    predicted_species <- reactive({
        new_data <- data.frame(Sepal.Length = input$sepal_length, 
                               Sepal.Width = input$sepal_width, 
                               Petal.Length = input$petal_length, 
                               Petal.Width = input$petal_width)

#Using the user inputted data, the previously trained RF model is used to predict the species.   
        
        rf <- rf_model()
        predicted_codes <- predict(rf, newdata = new_data)
        predicted_species <- levels(iris$Species)[predicted_codes] #This returns the Species as its text rather than as its level number. 
        return(predicted_species)
    })

#This links the prediction to the UI
    
    output$prediction <- renderText({
        predicted_species()
    })
    
#This calculates and returns the OOB rate of the generated model to the UI.
    output$OOBrate <- renderText({
        OOB <- mean(rf_model()$finalModel$err.rate)*100
        return(OOB)
    })
    
 #This calculates and returns the accuracy of the iris test set against the generated model to the UI.
    output$accuracy <- renderText({
        test <- predict(rf_model(), newdata = test_data)
        mean <- mean(test == test_data$Species) * 100
        return(mean)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

