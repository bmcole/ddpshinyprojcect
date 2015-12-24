library(shiny)
library(shinythemes)

shinyUI(
    fluidPage(
        
        title="Central Limit Theorem Explorer",
        theme = shinytheme("readable"),
        
        tags$head(
            tags$style(HTML(" @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
            h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #33BEFF;
        }

    "))
        ),
        
        headerPanel(
            
            h1("Central Limit Theorem Explorer",align="center")
            
            ), # end headerPanel
        
        fluidRow(
            
            column(5,
            
        
                p("Statistics is used primarily when it's impractical to collect 
                  all of the data from an entire population. We randomly sample 
                  the population and use statistics on that random sample to make 
                  inferences (draw conclusions) about the entire population."),
                
                p("There are several methods in statistics that require the assumption of
                  an approximately normal distribution of the data. In other words,
                  the data must roughly follow a bell-shaped curve that is symmetric
                  about its mean/median, which are located in the center."),
                
                p("So, what happens when the distribution of the population isn't normal?
                  The answer is the ",strong("Central Limit Theorem, "),"and it's 
                  one of the most fundamental results of statistics. 
                  In leyman's terms, the Central Limit Theorem says that if you have
                  a ", strong("sufficient number "), "of randomly selected, independent 
                  samples of size",strong("n"),"from a population, then when ",strong("n"),
                  "is large enough, the distribution of the sample means 
                  will approach a normal distribution ", 
                  strong("even when the population isn't normal"),".
                  A general rule of thumb for the sample size is",strong("n > 30."))
            ), # end Column
                  
                withMathJax(),    
            
            column(7,
                
                p("More formally, the Central Limit Theorem states that if \\(\\bar X\\)
                   is the mean of a random sample of size \\(n\\) taken from an 
                   arbitrary population with mean \\(\\mu\\) and finite variance
                   \\(\\sigma^2\\), then as \\(n\\) \\(\\rightarrow\\)\\(\\infty\\),"),
        
                p(strong("$$Z = \\frac{\\bar X - \\mu}{\\sigma / \\sqrt{n}}
                         \\sim N(0,1).$$"), "In other words,",
                  strong("$$\\bar X \\sim N(\\mu,\\frac{\\sigma^2}{n})$$ ")),
                
                
                p("This Shiny application allows the user to interactively explore
                   the phenomenon of the Central Limit Theorem. The user has control 
                   over the population distribution, the corresponding population 
                   parameters, and the size of the random samples taken. The 
                   population size is always 100000, and the number of random 
                   samples taken for the specified sample size is always 1000.
                   Furthermore, the theoretical mean and actual mean 
                   of the distribution of sample means is shown, as well as the 
                   theoretical and actual variance. A plot of the chosen population
                   distribution is shown for the corresponding parameters, as well
                   as a plot of the distribution of sample means for the specified 
                   sample size. All results are reactive (updated as the user 
                   makes changes).")
                 
                
            )# end column
        
            
        ), #end fluidRow
                
        hr(),
        
        fluidRow(
        
            withMathJax(),
            column(4,
                   selectInput("select", label = h3("Population Distribution"), 
                               choices = list("Binomial (Discrete)" = 1, 
                                              "Poisson (Discrete)" = 2,
                                              "Gamma (Continuous)" = 3), 
                                              selected = 3),
                  
                   sliderInput("sampsize", label = h3("Sample Size n"), min = 1, 
                               max = 100, value = 10)
                   
                   ), #end column
          
            
            column(4,
                   
                   h3("Population Parameters"),
                  
                   
                   conditionalPanel(
                       condition = "input.select == 1", # if binomial
                       sliderInput("trials",label=h4("Numer of trials t"),
                                   min=1,max=100,step=1,value=10),
                       sliderInput("sprob",label=h4("Success Probability p"),
                                   min = 0.05, max = 1, step = 0.05,value=0.5)  
        
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 2", # if poisson
                       sliderInput("lambda",label=h4("Lambda"),
                                   min=1,max=10,step=1,value=3)
                       
                      
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 3", # if gamma
                       sliderInput("gamalpha",label=h4("shape parameter Alpha "),
                                   min=0.5,max=5,step=0.5,value=1.5),
                       sliderInput("gambeta",label=h4("scale parameter Beta"),
                                   min=0.5,max=2.0,step=0.5,value=1)
                       
                   )
                   
                   
                   ),# end column
            
            column(4,
                   
                   h3("Distribution of Sample Means"),
                   br(),
                   
                   conditionalPanel(
                       condition = "input.select == 1",
                       h5("Theoretical Mean \\(\\mu\\) = \\(t\\)\\(p\\) :",
                       textOutput("theomean1")),
                       h5("Actual Mean: ",textOutput("actmean1")),
                       h5("Theoretical Variance  \\(\\frac{\\sigma^2}{n}\\) = 
                          \\(\\frac{tp(1-p)}{n}\\) :", textOutput("theovar1")),
                       h5("Actual Variance: ",textOutput("actvar1"))
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 2",
                       h5("Theoretical Mean \\(\\mu\\) = \\(\\lambda\\) :",
                          textOutput("theomean2")),
                       h5("Actual Mean: ", textOutput("actmean2")), 
                       h5("Theoretical Variance \\(\\frac{\\sigma^2}{n}\\) =
                          \\(\\lambda\\) :", textOutput("theovar2")),
                       h5("Actual Variance: ",textOutput("actvar2"))
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 3",
                       h5("Theoretical Mean  \\(\\mu\\) = \\(\\alpha\\)\\(\\beta\\) :",
                          textOutput("theomean3")),
                       h5("Actual Mean: ", textOutput("actmean3")),
                       h5("Theoretical Variance \\(\\frac{\\sigma^2}{n}\\) =
                          \\(\\frac{\\alpha\\beta^2}{n}\\):", textOutput("theovar3")),
                       h5("Actual Variance: ",textOutput("actvar3"))
                   )
                
            )
                   
            
        ), # end fluidRow
        
        hr(),
        
        fluidRow(
            
            column(6,
                   
                   conditionalPanel(
                       condition = "input.select == 1",
                       plotOutput("popplot1")
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 2",
                       plotOutput("popplot2")
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 3",
                       plotOutput("popplot3")
                   )
 
            ), # end column
            
            column(6,
                   
                   conditionalPanel(
                       condition = "input.select == 1",
                       plotOutput("sampmean1")
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 2",
                       plotOutput("sampmean2")
                   ),
                   
                   conditionalPanel(
                       condition = "input.select == 3",
                       plotOutput("sampmean3")
                   )
            ) #end column
            
        ), # end fluidRow
        
       hr(),
        # row for real-life example of chosen distribution
        fluidRow(
            column(12,
        
        conditionalPanel(
            condition = "input.select == 1", # if binomial
            h4(textOutput("extext1"))
        ),
        
        conditionalPanel(
            condition = "input.select == 2", # if poisson
            h4(textOutput("extext2"))
        ),
        
        conditionalPanel(
            condition = "input.select == 3", # if gamma
            h4(textOutput("extext3"))
        )
        
    )) #end fluidRow
        
                                                                             
            
                
        ) # end fluidPage
    ) # end shinyUI