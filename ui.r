shinyUI(
  
  fluidPage(    
    
   sidebarPanel(
    	radioButtons("viewInput", "Select what to view", choices = c("Non-UK countries", "UK Constituencies", "Number of signatures v number of votes for each party"), selected = "UK Constituencies"),
    	conditionalPanel(condition="input.viewInput == 'UK Constituencies'", 
    		selectInput("colBy", "Select what to colour by", choices = c("First Party", "Second Party"), selected = "First Party", multiple = FALSE)
    	)
    ),
    
    conditionalPanel(condition="input.viewInput == 'Non-UK countries'", 
    	titlePanel("Citizens of Non-UK countries signing the xenophobic petition"),
    	mainPanel(
    		plotOutput("coutriesPlot", height = 1000)
		)
	), 
	conditionalPanel(condition="input.viewInput == 'UK Constituencies'", 
    	titlePanel("Residents by UK Constituencies signing the xenophobic petition"),
    	mainPanel(
    		plotOutput("constituenciesPlot", height = 4000) 
		)
	), 
	conditionalPanel(condition="input.viewInput == 'Number of signatures v number of votes for each party'", 
    	titlePanel("Relationship between number of signatures for petition \n and number of votes for each party in each constituency"),
    	mainPanel(
    		plotOutput("UKIP"), 
    		plotOutput("con"), 
    		plotOutput("lab"), 
    		plotOutput("libDem"), 
    		plotOutput("green")
		)
	)
  )
)
    