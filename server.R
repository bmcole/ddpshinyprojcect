library(shiny)
library(ggplot2)

shinyServer(function(input, output){
    
    # generating binomial data
    
    
    
    set.seed(10)
    withMathJax()
    
    
    
    output$extext1 <- renderText({ 
        "Binomial Example: If a new drug is introduced to cure a disease, it either cures 
         the disease (success) or it does not cure the disease
         (failure)."
    })
    
    output$extext2 <- renderText({ 
        "Poisson Example: Number of patient arrivals at an emergency room during a fixed amount
         of time."
    })
    
    output$extext3 <- renderText({ 
        "Gamma Example: K people are waiting in line at a pharmacy. How long until they've all
         been served?"
    })
    
    
    output$popplot1 <- renderPlot({

     n <- as.numeric(input$trials)
     p <- as.numeric(input$sprob)
     data <- as.numeric(rbinom(100000,n,p))
     hist(data,main="Histogram: Binomial Population", xlab = "",
          col="lightblue")
    })
    
    output$popplot2 <- renderPlot({
        
        lambda <- as.numeric(input$lambda)
        data2 <- as.numeric(rpois(100000,lambda))
        hist(data2,main="Histogram: Poisson Population", xlab="",
             col = "lightblue")
        
    })
    output$popplot3 <- renderPlot({
        
       gamalpha <- as.numeric(input$gamalpha)
       gambeta <- as.numeric(input$gambeta)
       data3 <- rgamma(100000,shape=gamalpha,scale=gambeta)
       plot(density(data3),main="Desity Plot: Gamma Population",
            xlab="",col="lightblue")
    })

    output$sampmean1 <- renderPlot({
        mns1 <- NULL
        trials <- as.numeric(input$trials)
        p <- as.numeric(input$sprob)
        n <- as.numeric(input$sampsize)
        
        for(i in 1:1000){
            mns1 <- c(mns1,mean(rbinom(n,trials,p)))
        }
        
        mns1 <- as.data.frame(mns1)
        names(mns1) <- c("means")
        
        
        # make histogram
        ggplot(mns1,aes(x=means)) +
        geom_histogram(aes(y=..density..),binwidth=density(mns1$means)$bw) +
        geom_vline(xintercept=mean(mns1$means),colour="gold") +
        geom_density(fill="red",alpha=0.2) + xlab("Mean Value") + ylab("Density") +
        ggtitle("Histogram & Density Plot of Sample Means")
        
   
    })
    output$sampmean2 <- renderPlot({
        
        mns2 <- NULL
        lambda <- as.numeric(input$lambda)
        n <- as.numeric(input$sampsize)
        
        for(i in 1:1000){
            mns2 <- c(mns2,mean(rpois(n,lambda)))
        }
        
        mns2 <- as.data.frame(mns2)
        names(mns2) <- c("means")
        
        
        # make histogram
        ggplot(mns2,aes(x=means)) +
            geom_histogram(aes(y=..density..),binwidth=density(mns2$means)$bw) +
            geom_vline(xintercept=mean(mns2$means),colour="gold") +
            geom_density(fill="red",alpha=0.2) + xlab("Mean Value") + ylab("Density") +
            ggtitle("Histogram & Density Plot of Sample Means")
        
    })
    output$sampmean3 <- renderPlot({
        
        mns3 <- NULL
        gamalpha <- as.numeric(input$gamalpha)
        gambeta <- as.numeric(input$gambeta)
        n <- as.numeric(input$sampsize)
        
        for(i in 1:1000){
            mns3 <- c(mns3,mean(rgamma(n,shape=gamalpha,scale=gambeta)))
        }
        
        mns3 <- as.data.frame(mns3)
        names(mns3) <- c("means")
        
        
        # make histogram
        ggplot(mns3,aes(x=means)) +
            geom_histogram(aes(y=..density..),binwidth=density(mns3$means)$bw) +
            geom_vline(xintercept=mean(mns3$means),colour="gold") +
            geom_density(fill="red",alpha=0.2) + xlab("Mean Value") + ylab("Density") +
            ggtitle("Histogram & Density Plot of Sample Means")
    })
    
    
    output$theomean1 <- renderText({ 
        n <- as.numeric(input$trials)
        p <- as.numeric(input$sprob)
        as.character(n*p)
    })
    
    output$theomean2 <- renderText({ 
        lambda <- as.numeric(input$lambda)
        as.character(lambda)
    })
    
    output$theomean3 <- renderText({ 
        gamalpha <- as.numeric(input$gamalpha)
        gambeta <- as.numeric(input$gambeta)
        as.character(gamalpha*gambeta)
    })
    
    
    
    output$actmean1 <- renderText({ 
        trials <- as.numeric(input$trials)
        n <- as.numeric(input$sampsize)
        p <- as.numeric(input$sprob)
        
        mns1 <- NULL
        for(i in 1:1000){
            mns1 <- c(mns1,mean(rbinom(n,trials,p)))
        }
        as.character(mean(mns1)) 
    })
    
    output$actmean2 <- renderText({ 
        lambda <- as.numeric(input$lambda)
        n <- as.numeric(input$sampsize)
        mns2 <- NULL
        
        for(i in 1:1000){
            mns2 <- c(mns2,mean(rpois(n,lambda)))
        }
        as.character(mean(mns2)) 
        
    })
    
    output$actmean3 <- renderText({ 
        mns3 <- NULL
        gamalpha <- as.numeric(input$gamalpha)
        gambeta <- as.numeric(input$gambeta)
        n <- as.numeric(input$sampsize)
        
        for(i in 1:1000){
            mns3 <- c(mns3,mean(rgamma(n,shape=gamalpha,scale=gambeta)))
        }
        
        as.character(mean(mns3))
    })
    
    
    output$theovar1 <- renderText({ 
        p <- as.numeric(input$sprob)
        n <- as.numeric(input$sampsize)
        t <- as.numeric(input$trials)
        as.character(((t*p)-(t*p^2))/n)
        
    })
    
    output$theovar2 <- renderText({
        lambda <- as.numeric(input$lambda)
        n <- as.numeric(input$sampsize)
        as.character(lambda/n)
        
    })
    
    
    output$theovar3 <- renderText({ 
        
        gamalpha <- as.numeric(input$gamalpha)
        gambeta <- as.numeric(input$gambeta)
        n <- as.numeric(input$sampsize)
        as.character((gamalpha*gambeta^2)/n)
        
       
    })
    
    
    output$actvar1 <- renderText({ 
        
        trials <- as.numeric(input$trials)
        n <- as.numeric(input$sampsize)
        p <- as.numeric(input$sprob)
        
        mns1 <- NULL
        for(i in 1:1000){
            mns1 <- c(mns1,mean(rbinom(n,trials,p)))
        }
    
        as.character(var(mns1))
        
    
    })
    
    output$actvar2 <- renderText({ 
        
        mns2 <- NULL
        lambda <- as.numeric(input$lambda)
        n <- as.numeric(input$sampsize)
        
        for(i in 1:1000){
            mns2 <- c(mns2,mean(rpois(n,lambda)))
        }
        
        as.character(var(mns2))
        
        
    })
    
    output$actvar3 <- renderText({ 
        
        mns3 <- NULL
        gamalpha <- as.numeric(input$gamalpha)
        gambeta <- as.numeric(input$gambeta)
        n <- as.numeric(input$sampsize)
        
        for(i in 1:1000){
            mns3 <- c(mns3,mean(rgamma(n,shape=gamalpha,scale=gambeta)))
        }
        
        as.character(var(mns3))
    })
    
    
})