
library(shiny)
library(pheatmap)

data(airquality)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Comparing Copula matrix with correlation matix in PCA"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
     
      sliderInput(inputId = "cexs",
                  label = "Percent of variance explained",
                  min = .85,
                  max = 1,
                  value = .9, step=.01),
      sliderInput(inputId = "components",
                  label = "Number of components",
                  min = 1,
                  max = 5,
                  value = 2),
      numericInput("count", label = "sampe size", value = 100)
      
      
    ),
    
    # Main panel for displaying outputs ----
    
    mainPanel("HeatmMaps",
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
              ),
              "PCA",
              plotOutput("themap")
    )
    # mainPanel(
    # 
    #   # Output: Histogram ----
    #   plotOutput(outputId = "distPlot")
    # 
    # )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  #output$distPlot <- renderPlot({
    
    
    
    
    library(MASS)
    #cc=input$count
    x1=rnorm(5000)
    xlin=2*x1-1+rnorm(5000,0, 0.001)
    #cor(x[,1],xx, method = 'kendall')
    xx=(x1+1)^2+rnorm(5000,0, 0.001)
    #BiCopSelect(pobs(x1), pobs(xx), familyset = NA)
    #cor(x1, xx)
    xxx=x1^3+rnorm(5000,0, 0.001)
    #BiCopSelect(pobs(x1), pobs(xxx), familyset = NA)
    #cor(x1, xxx)
    xf=pnorm(x1)+rnorm(5000,0, 0.001)
    #BiCopSelect(pobs(x1), pobs(xf), familyset = NA)
    #cor(x1, xf)
    xe=exp(x1)+rnorm(5000,0, 0.001)
    #BiCopSelect(pobs(xf), pobs(xe), familyset = NA)
    t=cbind(xlin,xx, xxx, xf, xe)
    
    X=cbind(x1,t)
    colnames(X)=c("X", "X1","X2","X3","Xf", "Xe")
    tau=cor(X, method = 'kendall')
    rho=cor(X)
        #heatmap(tau)
    #par(mfrow=c(1,3))
    #plot(xx,xlin)
    #heatmap(tau)
    #pheatmap(rho)
    pt1=pheatmap(tau, main="
                 Correlation")
    pt2= pheatmap(rho, main= "Association")
   #  
    
    output$plotgraph1 = renderPlot({pt1})
    output$plotgraph2 = renderPlot({pt2})
    
    output$themap = renderPlot({        
      
  #      main="Scatter plot of Ozone and Temp")
    
    ef=eigen(cor(X))$vectors
    eff=eigen(cor(X))$values
    tot=cumsum(eff/sum(eff))

    ef1=eigen(tau)$vectors
    eff1=eigen(tau)$values
    tot1=cumsum(eff1/sum(eff1))
    #tot2[1]=0.53
    
   pt3=plot(tot,  type = "b", pch = 19,
         col = "red", xlab = "Componenets", ylab = "% variance (cumulative)", 
         lty = 1, lwd = 2)
    
    # 3. Add a second line
    lines(tot1, pch = 18, col = "blue", type = 'b', 
          lty = 2, lwd = 2)
    
    
    # 4. Add a legend to the plot and set legend lty
    legend("bottomright", legend = c("Linear PCA", "Copula PCA" ),
           col = c("red", "blue"), lty = 1:2, cex = 0.9 )
    abline(h=input$cexs, lty=4, lwd=3, col='green')
    abline(v=input$components , lwd=2, lty=3, col="brown")

    
    })
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
