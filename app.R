
library(shiny)
library(ggplot2)
library(ggforce)
library(dqrng)
library(data.table)

ui <- fluidPage(
  titlePanel("Calculating ellipse square"),
 
 sidebarPanel(
  numericInput('a', 'a=', 5, min = 1, max = 9,step = 1),
  numericInput('b', 'b=', 3, min = 1, max = 9,step = 1),
  numericInput('e', 'epsilon=', 0.01, min = 0.00001, max = 1),
  submitButton("Run calculation")),
  
     mainPanel(
       titlePanel("Square was calculated by Monte-Carlo method"),
       tableOutput("table"),
     uiOutput('text')
  )
)
    



server <- function(input, output) {

 Mydata <- reactive({
   validate(
     need(input$a>0, 'a should be more than zero'),
     need(input$b>0, 'b should be more than zero'),
     need(input$e<1 & input$e>=0.00001, 'ellipson should be in [0.00001;1)')
   )
 m <- 0
 n <- round(1 / input$e) ^ 2  
 
 calcfunc0 <- function(l){
   
   x <- dqrunif(l, min =  -input$a, max = input$a)  
   y <- dqrunif(l, min =  -input$b, max = input$b) 
   return (sum(ifelse((x / input$a)^2+(y / input$b)^2 < 1, 1 , 0)))   
 }   
 calc <- function() {
 if (n > 10000) {
nof <- round(n / 10000)
 ost <- n %% 10000;
 for (i in 1:nof){
   m <- m + calcfunc0(10000)
 }
 m <- m + calcfunc0(ost)
 }else{
   m <- calcfunc0(n)
 } 
   return (m)
   } 
   time <-  system.time(calc())[3] 

 
 return (c(calc(),time))
 })


  
 
    output$table <- renderTable({
      decimalplaces <- function(x) {
        k <- 0
        while (x%%1!=0){
          k <- k+1
          x <- x*10
        }
        return (k)
      }
      dig <- decimalplaces(input$e)
      
      n <- round((1 / input$e) ^ 2)
      square_monte_carlo <- (Mydata()[1]/n)*4*input$a*input$b 
      theory_square <- pi * input$a * input$b 
      real_error <- abs(theory_square - square_monte_carlo)
      isGoodAttempt <- real_error<input$e
      as.data.table(list(" "=c("calculated area", "real area", "relative error", "real error","isGoodAttempt?","time of working"),
           " "=list(round(square_monte_carlo,digits=dig),
                    round(theory_square,digits = dig), 
                    input$e,
                    round(real_error,digits = dig),
                    as.character(isGoodAttempt), 
                    round(Mydata()[2],digits = 3))))
    })
  
    output$text <- renderUI({
      withMathJax(
        helpText('Formula for calculating square:$$S = \\frac{m}n \\cdot S_0$$ \n 
                 Formula for calculating real square:   $$S = \\pi\\ \\cdot a \\cdot b $$  \n
                 Formula for calculating n:$$n = (\\frac{1}e)^2  $$ \n
                 Plot shows how method Monte-Carlo works. If n is not such big (n < 1000), you can see real points. In another case you can see only example. The reason of this 
decision was wasting time for painting this plot.
                  Attempt is good, when Real Error less than Relative Error \n
                 '))
      
    })
   
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
