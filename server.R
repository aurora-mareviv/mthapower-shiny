#under GNU General Public License
#server.R
library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  
  ## My function:
  f <- function(min.cases, max.cases, p0, OR.cas.ctrl, Nh, sig.level) {
    if (min.cases <= max.cases){
      num.cases <- seq(min.cases, max.cases, by=50)}
    if (min.cases > max.cases){
      num.cases <- seq(max.cases, min.cases, by=50)}
    ncases <- num.cases
    p0 <- p0
    Nh <- Nh
    OR.cas.ctrl <- OR.cas.ctrl
    sig.level <- sig.level
    # Parameters related to sig.level, from [Table 2] of Samuels et al.
    # For 90% power and alpha = .05, Nscaled = 8.5
    if (sig.level == 0.05){
      A <- -28 # Parameter A for alpha=.05
      x0 <- 2.6 # Parameter x0 for alpha=.05
      d <- 2.4 # Parameter d for alpha=.05
    }
    if (sig.level == 0.01){
      A <- -13 # Parameter A for alpha=.01
      x0 <- 5 # Parameter x0 for alpha=.01
      d <- 2.5 # Parameter d for alpha=.01
    }
    if (sig.level == 0.001){
      A <- -7 # Parameter A for alpha=.001
      x0 <- 7.4 # Parameter x0 for alpha=.001
      d <- 2.8 # Parameter d for alpha=.001
    }
    out.pow <- NULL # initialize vector
    for(ncases in ncases){
      OR.ctrl.cas <- 1 / OR.cas.ctrl # 1. CALCULATE P1 FROM A PREDEFINED P0, AND A DESIRED OR
      OR <- OR.ctrl.cas  
      bracket.pw <- p0 / (OR - OR*p0) # obtained after isolating p1 in OR equation [3].
      p1 <- bracket.pw / (1 + bracket.pw)
      Nh037 <- Nh^0.37 # 2. CALCULATE NSCALED
      num.n <- num.cases*((p1-p0)^2)
      den.n <- (p1*(1-p1) + p0*(1-p0))*Nh037
      Nscaled <- num.n/den.n
      num.power <- A - 100 # 3. CALCULATE POWER
      den.power <- 1 + exp((Nscaled - x0)/d)
      power <- 100 + (num.power/den.power) # The power I have to detect a given OR with my data, at a given alpha  
    }
    OR <- OR.cas.ctrl
    sig.level <- as.numeric(sig.level)
    power <- round(power, 2)
    Nscaled <- round(Nscaled, 3)
    out.pow <- data.frame(num.cases, Nh, Nscaled, p0, OR, sig.level, power)
    out.pow 
  }  
  
  mydata <- reactive(f(input$min.cases,
                       input$max.cases,
                       input$p0,
                       input$OR.cas.ctrl,
                       input$Nh,
                       input$sig.level))
  

  
  #   Show the final calculated value 
  output$powTable <- renderDataTable(
    {mydata <- f(input$min.cases,
                 input$max.cases,
                 input$p0,
                 input$OR.cas.ctrl,
                 input$Nh,
                 input$sig.level)
    mydata},
    options = list(paging = FALSE, searching = FALSE)
  )
  
  #   Show the Download handlers:
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$mydata, 'mtDNA_power_table.csv', sep='') },
    content = function(file) {
      write.csv(mydata(), file, na="")
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$mydata, 'mtDNA_power_plot.pdf', sep='') },
    content = function(file) {
      pdf(file)
      df <- mydata()
      p <-   print(ggplot(data = mydata(), aes(num.cases, power)) + 
                     theme_bw() + 
                     theme(text=element_text(size=12)) + 
                     labs(title = paste("Power estimates; ", "p0 =", " ", df[1,4], ", ", "Nh =", " ", df[1,2], ", ", "p =", " ", df[1,6], sep = "")) +
                     scale_color_brewer(palette = "Dark2", guide = guide_legend(reverse=TRUE)) +
                     xlab(paste("Number of Cases, Controls")) +
                     ylab("Power (%)") +
                     xlim(50, 1000) +
                     ylim(0, 100) +
                     # scale_x_log10() +
                     geom_line(alpha=0.8, size=0.2) +
                     geom_hline(alpha = 0.5,
                                yintercept=80) +
                     geom_point(aes(shape = factor(OR)), colour="black"))
      dev.off()
    }
  )
  
  #   Show the plot with a statistical power curve
  output$powHap <- renderPlot(
    {
      df <- mydata()
      p <-   print(ggplot(data = mydata(), aes(num.cases, power)) + 
                     theme_bw() + 
                     theme(text=element_text(size=12)) + 
                     labs(title = paste("Power estimates; ", "p0 =", " ", df[1,4], ", ", "Nh =", " ", df[1,2], ", ", "p =", " ", df[1,6], sep = "")) +
                     scale_color_brewer(palette = "Dark2", guide = guide_legend(reverse=TRUE)) +
                     xlab(paste("Number of Cases, Controls")) +
                     ylab("Power (%)") +
                     xlim(50, 1000) +
                     ylim(0, 100) +
                     # scale_x_log10() +
                     geom_line(alpha=0.8, size=0.2) +
                     geom_hline(alpha = 0.5,
                                yintercept=80) +
                     geom_point(aes(shape = factor(OR)), colour="black"))
    })
  
})