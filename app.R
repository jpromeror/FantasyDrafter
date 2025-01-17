library(shiny)
library(purrr)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(rvest)
library(stringr)

# Negate %in% operator
'%!in%' <- function(x,y)!('%in%'(x,y))


ProcessDraft<-function()
{
  # Specify the URL of the webpage
  URL<-"https://draftwizard.fantasypros.com/football/adp/mock-drafts/overall/default-std-8-teams/"
  
  # Read the webpage
  webpage <- read_html(URL)
  
  
  div_content <- webpage %>%
    html_table() 
  
  div_content <- div_content[[1]]
  
  div_content$Position<-gsub("[()0-9]","",div_content$Position)
  div_content$Round<-floor(div_content$`Avg Pick`)
  div_content$Subround<-unlist(lapply(split(div_content$Overall,div_content$Round),order))
  
  FullTable<-data.frame(Overall=div_content$Overall,
                        Round=floor(div_content$`Avg Pick`),
                        SubRound=unlist(lapply(split(div_content$Overall,div_content$Round),order)),
                        Name=div_content$Player,
                        Player=div_content$Player,
                        Team=sapply(strsplit(div_content$`Team (Bye)`,"  "),function(X){return(X[1])}),
                        Position=gsub("[()0-9]","",div_content$Position),
                        Bye=as.numeric(gsub("[()]","",sapply(strsplit(div_content$`Team (Bye)`,"  "),function(X){return(X[2])}))))
  
  FullTable<-cbind(FullTable,div_content[,5:9])
  
  return(FullTable)
  
}

## Generate Data For App


#players<-ProcessDraft()
players<-readRDS("/Users/juanpablo.romerorioj/Desktop/Algoritmo/PlayerData.rdata")

players$Position<-factor(players$Position,levels = c("QB","WR","RB","TE","K","DST"))

playersComplete<-players

DraftStructure<-list(c("RB","WR"),
                     c("RB","WR"),
                     c("RB","WR","TE"),
                     c("RB","WR","QB"),
                     c("QB","WR","TE","QB"),
                     c("RB","WR","QB","TE"),
                     c("RB","WR","QB","TE"),
                     c("RB","WR"),
                     c("RB","WR"),
                     c("RB","WR"),
                     c("RB","WR"),
                     c("DST","K","WR","RB"),
                     c("K","DST"),
                     "COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE")

DraftProp<-c("WR or RB",
             "WR or RB",
             "Travis Kelce or Sam Laporta or WR or RB",
             "Top 3 QB or WR or RB",
             "QB or WR or TE",
             "WR or RB or TE",
             "WR or RB",
             "WR or RB",
             "WR or RB",
             "WR or RB",
             "WR or RB",
             "K or DST",
             "K or DST",
             "COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE","COMPLETE")

Delta<-c(1,3,3,1,1,1)
MaxPositions<-c(1,3,3,1,1,1)
names(Delta)<-levels(players$Position)
#Byes<-read.csv("nfl_bye_weeks_2024.csv",header=T)
###
Round<-1
SelectedPlayers<-NULL
Suggs<-NULL
#TypeLast<-NA


polychrome <- c("#F6222E","#3283FE","#FEAF16","#1C8356", "#85660D", "#822E1C", "#683B79")


# Define the UI for the app
ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel(HTML(paste("<b>NFL Fantasy DrafteR</b>"))),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("player", "Players:", choices = c("",as.character(players$Name)), multiple = FALSE),
                    actionButton("draft", "Draft!"),
                    actionButton("remove", "Remove!"),
                    p(""),
                    img(src="RobotChicken.png",height = '200px', width = '200px',style="display: block; margin-left: auto; margin-right: auto;"),
                    p(""),
                    actionButton("undo", "Undo!"),
                  ),
                  mainPanel(tabsetPanel(
                    tabPanel("Draft",
                             textOutput("\n"),
                             htmlOutput("RoundX"),
                             textOutput("\n"),
                             htmlOutput("StratX",style = "color:red"),
                             h2(strong("Drafted Players")),
                             tableOutput("TeamSelection"),
                             h2(strong("Suggested Picks")),
                             tableOutput("SuggestedPick"),
                             h2(strong("Top Available Players per Position")),
                             tableOutput("Tops")),
                    tabPanel("Team Analyzer",
                             plotOutput("Position"),
                             plotOutput("Bye"),
                             plotOutput("AnalysisTeam")))
                  )),
                hr(),
                print("© Jp.Romero, 2024")
)

# Define the server logic
server <- function(input, output, session) {
  
  rv <- reactiveValues(players=players,Team=NULL,Deltas=Delta,TypeLast=NA,LastMovement=NA)
  Round <- reactiveVal(1)
  
  
  observeEvent(input$draft, {
    
    if(input$player!="")
    {
      iix<-match(input$player,rv$players$Name)
      Selection<-rv$players[iix,c("Player","Position","Team","Bye")]
      rv$LastMovement<-rv$players[iix,]
      rv$TypeLast<-"DRAFT"
      Selection$Round<-Round()
      rv$Team<-rbind(rv$Team,Selection)
      rv$players<-rv$players[-iix,]
      updateSelectInput(session, inputId = "player", choices = c("",rv$players$Name), selected = "")
      
      # Filter Positions
      if(nrow(rv$Team)>0)
      {
        CurrentPositions<-table(rv$Team$Position)
        
        ValueX<-as.vector(MaxPositions-CurrentPositions)
        names(ValueX)<-levels(players$Position)
        rv$Deltas<-ValueX
        
      }
      
      Round(Round()+1)
    }
    
  })
  
  observeEvent(input$remove, {
    
    if(input$player!="")
    {
      iix<-match(input$player,rv$players$Name)
      rv$LastMovement<-rv$players[iix,]
      rv$TypeLast<-"REMOVE"
      rv$players<-rv$players[-iix,]
      updateSelectInput(session, inputId = "player", choices = c("",rv$players$Name[order(rv$players$Overall)]), selected = "")
      output$Suggestions <- renderTable({Sugs})
    }
    
  })
  
  observeEvent(input$undo, {
    
    #browser()
    if(rv$TypeLast=="DRAFT")
    {
      rv$players<-rbind(rv$players,rv$LastMovement)
      updateSelectInput(session, inputId = "player", choices = c("",rv$players$Name[order(rv$players$Overall)]), selected = "")

      iix<-match(rv$LastMovement$Name,rv$Team$Player)
      rv$Team<-rv$Team[-iix,,drop=FALSE]
      
      iiy<-match(rv$LastMovement$Position,names(rv$Deltas))
      rv$Deltas[iiy]<-rv$Deltas[iiy]+1
      
      if (Round()>1)
      {
        Round(Round()-1)
      }else{
        Round(1)
      }
      
      
    }else if(rv$TypeLast=="REMOVE")
    {
      rv$players<-rbind(rv$players,rv$LastMovement)
      updateSelectInput(session, inputId = "player", choices = c("",rv$players$Name[order(rv$players$Overall)]), selected = "")
     
    }
    
  })
  
  observe({
    
    #browser()
    PosforRound<-DraftStructure[[Round()]]
    if(any(rv$Deltas<=0) & Round() < 8)
    {
      PosFull<-names(which(rv$Deltas<=0))
      Sugg<-rv$players %>% filter(Position %in% PosforRound & Position %!in% PosFull ) %>% group_by(Position) %>% arrange(Overall) %>% slice_head(n=1)
      
    }else{
      
      Sugg<-rv$players %>% filter(Position %in% PosforRound) %>% group_by(Position) %>% arrange(Overall) %>% slice_head(n=1)
      
    }
    
    
    Sugg<-Sugg[,c("Player","Position","Team")]
    
    output$SuggestedPick <- renderTable({Sugg})
    
    TopSuggested<-rv$players %>% group_by(Position) %>% arrange(Overall) %>% slice_head(n=1)
    TopSuggested<-TopSuggested[,c("Player","Position","Team")]
    
    
    output$Tops <- renderTable({TopSuggested})
    
    if(Round() > 1)
    {
      DataPlotPosition<-as.data.frame(table(rv$Team$Position))
      
      PlotPosition<-ggplot(DataPlotPosition,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat="identity")+
        theme_classic()+scale_fill_manual(values=polychrome)+xlab("Position")+ylab("# of Players")+ guides(fill="none")+
        coord_flip()+geom_text(aes(label=Freq), hjust=0)+ggtitle("Positions")
      
      output$Position<-renderPlot({PlotPosition})
      
      ByeWeekInfo<-as.data.frame(table(rv$Team$Bye))
      ByeWeekInfo$Var1<-factor(ByeWeekInfo$Var1,levels=sort(unique(ByeWeekInfo$Var1)))
      
      ByeWeekPlot<-ggplot(ByeWeekInfo,aes(x=Var1,y=Freq,fill=Freq))+geom_bar(stat="identity")+
        theme_classic()+scale_fill_gradient(low = "lightgray",high="firebrick1")+xlab("Week")+ylab("# of Players")+ guides(fill="none")+
        coord_flip()+geom_text(aes(label=Freq), hjust=0)+ggtitle("Bye week")
      
      
      output$Bye<-renderPlot({ByeWeekPlot})
      
      TeamAnalysis<-playersComplete[match(rv$Team$Player,playersComplete$Name),]


      PlotAnalysis<-ggplot(TeamAnalysis,aes(x=Position,y=`Avg Pick`,fill=Position))+geom_boxplot()+theme_classic()+
        scale_fill_manual(values=polychrome)+guides(fill="none")+ylab("Average Pick Round")
      
      output$AnalysisTeam<-renderPlot({PlotAnalysis})
      
      
    }
    
  })
  output$RoundX <- renderText({
    paste0("<b>Current Round = ",Round(),"</b>")
  })
  
  output$StratX <- renderText({
    paste0("<b>Strategy: ",DraftProp[Round()],"</b>")
  })
  
  output$TeamSelection<-renderTable(rv$Team)
  
  
  
  
}

# Run the app
shinyApp(ui, server)