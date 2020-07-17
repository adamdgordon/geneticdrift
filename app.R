library(shiny)
library(png)
library(ggplot2)
library(gridGraphics)
library(ggimage)
library(gridExtra)
nmax <- 1000
nlarge <- 100
nsmall <- 10
# Generate massive population
nbluemax <- sample(ceiling(0.1*nmax):floor(0.4*nmax), 1)
nyellowmax <- sample(ceiling(0.1*nmax):floor(0.4*nmax), 1)
nredmax <- nmax-(nbluemax+nyellowmax)
beadsmax <- c(rep("blue", nbluemax), rep("red", nredmax), rep("yellow", nyellowmax))
beadsmax <- sample(beadsmax, length(beadsmax))
# Generate large population
nbluelarge   <- round(nbluemax/nmax*nlarge, 0)
nyellowlarge <- round(nyellowmax/nmax*nlarge, 0)
nredlarge    <- round(nredmax/nmax*nlarge, 0)
difference <- nlarge - sum(c(nbluelarge, nyellowlarge, nredlarge))
color <- sample(1:3, 1)
if (color==1) nbluelarge <- nbluelarge + difference
if (color==2) nyellowlarge <- nyellowlarge + difference 
if (color==3) nredlarge <- nredlarge + difference 
beadslarge <- c(rep("blue", nbluelarge), rep("red", nredlarge), rep("yellow", nyellowlarge))
beadslarge <- sample(beadslarge, length(beadslarge))
# Generate small population
nbluesmall   <- round(nbluemax/nmax*nsmall, 0)
nyellowsmall <- round(nyellowmax/nmax*nsmall, 0)
nredsmall    <- round(nredmax/nmax*nsmall, 0)
difference <- nsmall - sum(c(nbluesmall, nyellowsmall, nredsmall))
color <- sample(1:3, 1)
if (color==1) nbluesmall <- nbluesmall + difference
if (color==2) nyellowsmall <- nyellowsmall + difference 
if (color==3) nredsmall <- nredsmall + difference 
beadssmall <- c(rep("blue", nbluesmall), rep("red", nredsmall), rep("yellow", nyellowsmall))
beadssmall <- sample(beadssmall, length(beadssmall))
# Generate plotting data
dmax <- data.frame(x = rnorm(nmax),
                y = rnorm(nmax),
                image = paste0("www/", beadsmax[1:nmax], "bead.png")
               )
dmax[,1][dmax[,1]>3] <- 3
dmax[,1][dmax[,1]< -3] <- -3
dmax[,2][dmax[,2]>3] <- 3
dmax[,2][dmax[,2]< -3] <- -3
dlarge <- data.frame(x = rnorm(nlarge),
                y = rnorm(nlarge),
                image = paste0("www/", beadslarge[1:nlarge], "bead.png")
               )
dlarge[,1][dlarge[,1]>3] <- 3
dlarge[,1][dlarge[,1]< -3] <- -3
dlarge[,2][dlarge[,2]>3] <- 3
dlarge[,2][dlarge[,2]< -3] <- -3
dsmall <- data.frame(x = rnorm(nsmall),
                y = rnorm(nsmall),
                image = paste0("www/", beadssmall[1:nsmall], "bead.png")
               )
dsmall[,1][dsmall[,1]>3] <- 3
dsmall[,1][dsmall[,1]< -3] <- -3
dsmall[,2][dsmall[,2]>3] <- 3
dsmall[,2][dsmall[,2]< -3] <- -3
plotmax   <- ggplot(dmax, aes(x, y)) + geom_image(aes(image=image)) + coord_fixed() +
	                xlim(-3,3) + ylim(-3,3) +
					ggtitle("Population") +
					theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
					      plot.title = element_text(size=18, face="bold"))
plotlarge <- ggplot(dlarge, aes(x, y)) + geom_image(aes(image=image)) + coord_fixed() +
	                xlim(-3,3) + ylim(-3,3) +
					ggtitle("Population") +
					theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
					      plot.title = element_text(size=18, face="bold"))
plotsmall <- ggplot(dsmall, aes(x, y)) + geom_image(aes(image=image)) + coord_fixed() +
	                xlim(-3,3) + ylim(-3,3) + 
					ggtitle("Population") +
					theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
					      plot.title = element_text(size=18, face="bold"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Genetic Drift Simulation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",
    # Sidebar panel for inputs ----
    sidebarPanel( 
		  radioButtons("popsize", h3("Population size"),
                        #choices = list(paste0("Large (n=", nlarge, ")") = nlarge,
                        #               paste0("Small (n=", nsmall, ")") = nsmall, selected = nlarge),
                        choices = list("Massive (n=1,000)" = nmax,
                                       "Large (n=100)" = nlarge,
									   "Small (n=10)" = nsmall), selected = nlarge),
	sliderInput("nsample", h3("Sample size"),
                     min = 1, max =nlarge, value = ceiling(nlarge/2))
	),
    # Main panel for displaying outputs ----
    mainPanel(
	  h4("This exercise is meant to simulate the effects of genetic drift.  At left you can choose a massive (n=1,000), large (n=100), or small (n=10) population size, which in this case is a population of colored beads.  You can then use the slider to choose a number of beads to sample from that population.  The simulation starts out with a large population and a sample size of 50.  Following your lab exercise, use the controls at left to adjust the simulation, then record the results."),
	  br(),
	  h4(textOutput("selected_popsize")),
	  fluidRow(
	    column(4, plotOutput("pop")),
	    column(4, plotOutput("sample"))
	  ),
	  h4(textOutput("selected_samplesize"))
	)
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  #snb <- reactiveVal(0)
  #sny <- reactiveVal(0)
  #snr <- reactiveVal(0)
  nb <- reactiveVal()
  ny <- reactiveVal()
  nr <- reactiveVal()
  nb(0)
  ny(0)
  nr(0)
  #observeEvent(input,  {
  #  if(input$popsize==100) beads <- beadsmax
  #	if(input$popsize==10) beads <- beadssmall
  #  smp <- beads[1:input$nsample]
  #  snb(sum(smp=="blue"))
  #  sny(sum(smp=="yellow"))
  #  snr(sum(smp=="red"))
  #})
  observeEvent(input$popsize,  {
    updateSliderInput(session = session, inputId = "nsample", max = as.integer(input$popsize))
    updateSliderInput(session = session, inputId = "nsample", value = ceiling(as.integer(input$popsize)/2))
  })
  output$sampcolors <- renderText(paste0("Your sample contains ", sny, " yellow beads, ",
	           snb, " blue beads, and ", snr, " red beads."))
  output$samp <- renderTable(samp())
  output$selected_popsize <- renderText({
    if(input$popsize==nmax) desc <-paste0("Your massive population contains ", nyellowmax, " yellow beads, ",
	           nbluemax, " blue beads, and ", nredmax, " red beads.")
    if(input$popsize==nlarge) desc <-paste0("Your large population contains ", nyellowlarge, " yellow beads, ",
	           nbluelarge, " blue beads, and ", nredlarge, " red beads.")
    if(input$popsize==nsmall) desc <-paste0("Your small population contains ", nyellowsmall, " yellow beads, ",
	           nbluesmall, " blue beads, and ", nredsmall, " red beads.")
    paste0("You have selected a population of n=", input$popsize, ". ", desc)
  })
  output$selected_samplesize <- renderText({
    paste0("You have selected a sample of n=", input$nsample, ". Your sample contains ", ny(), " yellow beads, ",
	           nb(), " blue beads, and ", nr(), " red beads.")
  })
  output$pop <- renderPlot({
	npop <- as.integer(input$popsize)
	if(input$popsize==nmax) {
	  plot1 <- plotmax
	  beads <- beadsmax}
	if(input$popsize==nlarge) {
	  plot1 <- plotlarge
	  beads <- beadslarge}
	if(input$popsize==nsmall) {
	  plot1 <- plotsmall
	  beads <- beadssmall}
    plot1
  })
  output$sample <- renderPlot({
	npop <- as.integer(input$popsize)
	nsample <- input$nsample
	if(input$popsize==nmax) {
	  beads <- beadsmax}
	if(input$popsize==nlarge) {
	  beads <- beadslarge}
	if(input$popsize==nsmall) {
	  beads <- beadssmall}
	dsample <- data.frame(x = rnorm(nsample),
                y = rnorm(nsample),
               image = paste0("www/", sample(beads, input$nsample), "bead.png")
               )
	#nb <- sum(dsample$image=='www/bluebead.png')
	#ny <- sum(dsample$image=='www/yellowbead.png')
	#nr <- sum(dsample$image=='www/redbead.png')
	nb(sum(dsample$image=='www/bluebead.png'))
	ny(sum(dsample$image=='www/yellowbead.png'))
	nr(sum(dsample$image=='www/redbead.png'))
	stext <- paste0("yellow: ", ny(), ", blue: ", nb(), ", red: ", nr())
	dsample[,1][dsample[,1]>3] <- 3
	dsample[,1][dsample[,1]< -3] <- -3
	dsample[,2][dsample[,2]>3] <- 3
	dsample[,2][dsample[,2]< -3] <- -3
    plot2 <- ggplot(dsample, aes(x, y)) + geom_image(aes(image=image)) + coord_fixed() +
	                xlim(-3,3) + ylim(-3,3) + ggtitle("Sample") +
					theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
					      plot.title = element_text(size=18, face="bold"))
    plot2
  })
}
shinyApp(ui = ui, server = server)
