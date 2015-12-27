
library(shiny)
library(jsonlite)
library(httr)
library(ggplot2)

shinyServer(function(input, output) {
  
output$coutriesPlot <- renderPlot({
    

#get data
data1 <- fromJSON("https://petition.parliament.uk/petitions/107516.json")

signByCountry <- as.data.frame(data1$data$attributes$signatures_by_country)

signByCountry <- signByCountry[which(signByCountry$name != "United Kingdom"), ]

signByCountry <- transform(signByCountry, 
                          name = reorder(name, signature_count))

c <- ggplot(signByCountry, aes(x = factor(name), y = signature_count))
c + geom_bar(stat="identity") + coord_flip() + 
  xlab("Name of country") +
  ylab("Number of signatures from country")

  })
   
  
signAndVote <- reactive({  #get data
data1 <- fromJSON("https://petition.parliament.uk/petitions/107516.json")

signByConst <- as.data.frame(data1$data$attributes$signatures_by_constituency)

X <- read.csv(url("http://researchbriefings.files.parliament.uk/documents/CBP-7186/hocl-ge2015-results-summary.csv"))

names(X)[names(X)=="ons_id"] <- "ons_code"

signAndVote <- merge(signByConst, X, by="ons_code")

signAndVote <- transform(signAndVote, 
                          name = reorder(name, signature_count))
                          
})

output$constituenciesPlot <- renderPlot({
	
	signAndVote <- as.data.frame(signAndVote())
    

if (input$colBy == "First Party") {

#c <- qplot(signAndVote, aes(x = factor(name), y = signature_count))
c <- qplot(name, signature_count, data=signAndVote, fill = factor(first_party))
c + geom_bar(stat="identity") + coord_flip() + 
  xlab("Name of constituency") +
  ylab("Number of signatures from constituency")+
  scale_fill_manual(values = c("Con" = "#0087dc",
		"DUP" = "#D46A4C",
		"Green" = "#008066",
		"Ind" = "#DDDDDD",
		"Lab" = "#d50000",
		"LD" = "#FDBB30",
		"PC" = "#3F8428",
		"SDLP" = "#99FF66",
		"SF" = "#008800",
		"SNP" = "#FFF95D",
		"Spk" = "white",
		"UKIP" = "#EFE600",
		"UUP" = "#9999FF"))
  } else {
  	c <- qplot(name, signature_count, data=signAndVote, fill = factor(second_party))
	c + geom_bar(stat="identity") + coord_flip() + 
  	xlab("Name of constituency") +
  	ylab("Number of signatures from constituency")+
  	scale_fill_manual(values = c( "Alliance" = "#F6CB2F",
		"Con" = "#0087dc",
		"DUP" = "#D46A4C",
		"Green" = "#008066",
		"Ind" = "#DDDDDD",
		"Lab" = "#d50000",
		"LD" = "#FDBB30",
		"PBPA" = "#9400d3",
		"PC" = "#3F8428",
		"Respect" = "#FF3300",
		"SF" = "#008800",
		"SNP" = "#FFF95D",
		"TUV" = "#0095B6",
		"UKIP" = "#EFE600",
		"UUP" = "#9999FF"))

  }

  })
  
output$UKIP <- renderPlot({
		signAndVote <- as.data.frame(signAndVote())
	
	plot(signAndVote$signature_count, signAndVote$ukip, main="UKIP", xlab="Signature count in constituency", ylab="Number of votes for UKIP in consticuency", pch=19)
	abline(lm(signAndVote$ukip~signAndVote$signature_count), col="red") # regression line (y~x)

	
	})
	
output$con <- renderPlot({
		signAndVote <- as.data.frame(signAndVote())
	
	plot(signAndVote$signature_count, signAndVote$con, main="Conservatives", xlab="Signature count in constituency", ylab="Number of votes for Conservatives in consticuency", pch=19)
	abline(lm(signAndVote$con~signAndVote$signature_count), col="red") # regression line (y~x)

	
	})
	
	output$lab <- renderPlot({
			signAndVote <- as.data.frame(signAndVote())
	
	plot(signAndVote$signature_count, signAndVote$lab, main="Labour", xlab="Signature count in constituency", ylab="Number of votes for Labour in consticuency", pch=19)
	abline(lm(signAndVote$lab~signAndVote$signature_count), col="red") # regression line (y~x)

	
	})
	
	output$libDem <- renderPlot({
			signAndVote <- as.data.frame(signAndVote())
	
	plot(signAndVote$signature_count, signAndVote$ld, main="LibDem", xlab="Signature count in constituency", ylab="Number of votes for LibDem in consticuency", pch=19)
	abline(lm(signAndVote$ld~signAndVote$signature_count), col="red") # regression line (y~x)

	
	})
	
	output$green <- renderPlot({
			signAndVote <- as.data.frame(signAndVote())
	
	plot(signAndVote$signature_count, signAndVote$green, main="Green", xlab="Signature count in constituency", ylab="Number of votes for Green in consticuency", pch=19)
	abline(lm(signAndVote$green~signAndVote$signature_count), col="red") # regression line (y~x)

	
	})


})