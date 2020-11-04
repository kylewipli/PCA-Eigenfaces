#call library###############################
library(shiny)
library(shinythemes)
library(EBImage)
#app interface##############################
ui <- fluidPage(theme = shinytheme("slate"),
  titlePanel("PCA & Eigen faces"),
  
  navbarPage("Assignment 2 Kyle Wipfli :)",
             tabPanel( "PCA Analysis",
                       #define the sidebar with the inputs
                       sidebarLayout(
                         sidebarPanel(
                           #input types:
                            selectInput('givens', 'Data to use PCA on', choices = c(
                               "Rock", "Iris", "MtCars"
                            ), multiple = FALSE),
                            #Slider
                            sliderInput("numb",
                                        "Number of Eigenvectors to keep",
                                        min=1,
                                        max=30, value=1),
                       ),
                         #define the main layout
                         mainPanel(
                           plotOutput("pca"),
                           plotOutput("pca2")
                         )
                       ),
             ),
    tabPanel( "Eigenface Data Input",
              #define the sidebar with the inputs
              sidebarLayout(
                sidebarPanel(
                  #input types:
                  #choosing the file from the computer
                  fileInput("images", 
                            "Choose your data set:",multiple = TRUE),
                  
                  fileInput("test_image", 
                            "Choose your face to recognize:",multiple = TRUE),
                  
                  #Slider
                  sliderInput("th",
                              "0.1 Percent of total Eigenfaces to keep",
                              min=1,
                              max=1000, value=1),
                
                    #find the eigenfaces
                  actionButton("eigenprocess", 
                               "Run")
                  
                ),
                #define the main layout
                mainPanel(
                   plotOutput("value_plot"),
                  column(6, tableOutput("error_output")) ,
                   column(6,plotOutput("error_plot",width ="50%",height = "250px"))
                        )
                       ),
            )
   )
)


#server#########################################
server <- function(input, output, session) {
  
###storing the files###########################
 faces_output <- eventReactive(input$eigenprocess,{
   f<- input$images
   g<- input$test_image
   int<-input$integer
   test<-as.numeric(readImage(g$datapath))
   faces <- as.list(f$datapath)
   image_list <- lapply(faces,readImage)
   dime_image <- dim(image_list[[1]])
   image_matr <- do.call("cbind",lapply(image_list,as.numeric))
   image_matrix <- as.matrix(image_matr)
   eigenstuff<-list()
   eigenstuff<- pcawip(image_matrix,dime_image,test,input$th)
   return(eigenstuff)
})
 
 #Output handling###################################
 output$pca <- renderPlot({
    if (input$givens == "Rock"){
       
       pca_mat <- cbind( rock$area , rock$peri)
       
       plot(pca_mat, main = "Rock data", xlab = "Area  [pixels^2]", ylab = "Perimeter [pixels]")
    }else if(input$givens == "Iris"){
       
       pca_mat <- cbind( iris$Petal.Width , iris$Sepal.Width )
       
       plot(pca_mat, main = "Iris data", xlab = "Petal Width", ylab = "Sepal Width")
    }else if(input$givens == "MtCars"){
       
       pca_mat<- cbind( mtcars$mpg , mtcars$qsec )
       
       plot(pca_mat, main = "Mtcars data", xlab = "Fuel Effiency [mpg]", ylab = "Quarter Mile Time [s]")
    }
 })
 output$pca2 <-renderPlot({
    if (input$givens == "Rock"){
       pca_mat <- cbind(rock$area,rock$peri)
       trfm_mat <- pcasimple(pca_mat,input$numb)
       plot(trfm_mat, main = "Projected Rock data", xlab = "mixed x", ylab = "mixed y")
    }else if(input$givens == "Iris"){
       pca_mat <- cbind(iris$Petal.Width,iris$Sepal.Width)
       trfm_mat <- pcasimple(pca_mat,input$numb)
       plot(trfm_mat, main = "Projected Iris data", xlab = "mixed x", ylab = "mixed y")
    }else if(input$givens == "MtCars"){
       pca_mat<- cbind(mtcars$mpg,mtcars$qsec)
       trfm_mat <- pcasimple(pca_mat,input$numb)
       plot(trfm_mat, main = "Projected Mtcars data", xlab = "mixed x", ylab = "mixed y")
    }
 })
 
 output$error_output <- renderTable({
    print(faces_output()$error)
 })
 output$error_plot <- renderPlot({
    plot(faces_output()$error_plot)
 })   
 output$value_plot <- renderPlot({
    plot(faces_output()$values[1:100]/sum(faces_output()$values), xlab = 'First 100 Eigenvalues', ylab = 'Eigenvalue Size', main = 'Relative amount of information in eigenvector #_')
    lines(faces_output()$values/sum(faces_output()$values))

 })
 #defining the functions#####################
 pcasimple <- function(x, n){
    x_temp <- scale(x, center = TRUE,scale = TRUE)
    cov_mat <- x_temp%*%t(x_temp)/(nrow(x_temp)-1)
    eigen_temp <- eigen(cov_mat)
    proj_matr <- cbind(eigen_temp$vectors)
    scaled <- proj_matr[,n]%*%t(proj_matr[,n])%*%x
    return(scaled)
 }
 
 pcawip <- function(x,dime,tester,thresh){
    x_data_temp <-scale(x, center= TRUE, scale = FALSE)
    avg_face <- rowSums(x_data_temp)/ncol(x_data_temp)
   x_data_mat <- scale(x, center = TRUE, scale = TRUE)
   Cov_matrix <- x_data_mat%*%t(x_data_mat)/(nrow(x_data_mat)-1)
   eigen_cov <- eigen(t(Cov_matrix))
   eigen_val <- eigen_cov$values
   eigen_vect <-eigen_cov$vectors
   eigen_mat <-cbind(eigen_cov$vectors)
   eigen_cov$mat <-eigen_mat
   testing <- scale(tester,center = TRUE,scale = FALSE) -avg_face
   choice <- ncol(eigen_mat)*thresh/1000
   w <-(eigen_mat[,1:choice]%*%t(eigen_mat[,1:choice])%*%testing)
   u <- testing - w
   a <- testing*testing
   b <- eigen_mat*eigen_mat
   q <- which.min(colSums(sweep(b,2,a,"-")))
   v <- sqrt(max(b[,q]-a))
eigen_cov$error <- c("Threshold value",v,
                      "Normalized distance bewteen face and projected face",norm(u,"F")/norm(testing,"F"),
                      "Test 1 if test_image is face",norm(u, "F")/norm(testing,"F") < 0.8*v,
                      "Test 1 if test_image is face",norm(u, "F")/norm(testing,"F")< v)
eigen_cov$error_plot <- as.Image(array(w,dim=dime))
   return(eigen_cov)
 }
}
#create the app################################
shinyApp(ui, server)