#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(gridExtra)
library(htmlwidgets)
library(webshot)
library(wordcloud2)
library(wordcloud)

ui <- shinyUI(
  fluidPage(
    tags$style(type = 'text/css', '.navbar { background-color: #F2F2F2;
               text-align:center ; 
               font-family: Arial;
               font-size: 15px;
               
               # display:table-cell ; 
               # vertical-align:middle;
               # height:90px;
               # width:2500px;
               
               }'),
   # theme = shinytheme("spacelab"),setBackgroundColor(color = "lightgrey",gradient = c("linear","radial"), direction = c("bottom", "top", "right", "left")),
    
    navbarPage(title="SimpleApp",
               tabPanel("Tab-1", fluidRow(h1("Welcome to the home page of SimpleApp"),
                 box(
                 title="SimpleApp left box", width= 6, 
                 p("This part of the homepage contains data in text format. This text data is totally random. But why smiling man her imagine married. Chiefly can man her out believe manners cottage colonel unknown. Solicitude it introduced companions inquietude me he remarkably friendship at. My almost or horses period. Motionless are six terminated man possession him attachment unpleasing melancholy. Sir smile arose one share. No abroad in easily relied an whence lovers temper by. Looked wisdom common he an be giving length mr. 

                   Finished her are its honoured drawings nor. Pretty see mutual thrown all not edward ten. Particular an boisterous up he reasonably frequently. Several any had enjoyed shewing studied two. Up intention remainder sportsmen behaviour ye happiness. Few again any alone style added abode ask. Nay projecting unpleasing boisterous eat discovered solicitude. Own six moments produce elderly pasture far arrival. Hold our year they ten upon. Gentleman contained so intention sweetness in on resolving. 
                   
                   Fulfilled direction use continual set him propriety continued. Saw met applauded favourite deficient engrossed concealed and her. Concluded boy perpetual old supposing. Farther related bed and passage comfort civilly. Dashwoods see frankness objection abilities the. As hastened oh produced prospect formerly up am. Placing forming nay looking old married few has. Margaret disposed add screened rendered six say his striking confined. 
                   
                   At every tiled on ye defer do. No attention suspected oh difficult. Fond his say old meet cold find come whom. The sir park sake bred. Wonder matter now can estate esteem assure fat roused. Am performed on existence as discourse is. Pleasure friendly at marriage blessing or. 
                   
                   She exposed painted fifteen are noisier mistake led waiting. Surprise not wandered speedily husbands although yet end. Are court tiled cease young built fat one man taken. We highest ye friends is exposed equally in. Ignorant had too strictly followed. Astonished as travelling assistance or unreserved oh pianoforte ye. Five with seen put need tore add neat. Bringing it is he returned received raptures. 
                   
                   Meant balls it if up doubt small purse. Required his you put the outlived answered position. An pleasure exertion if believed provided to. All led out world these music while asked. Paid mind even sons does he door no. Attended overcame repeated it is perceive marianne in. In am think on style child of. Servants moreover in sensible he it ye possible. 
                   
                   Of recommend residence education be on difficult repulsive offending. Judge views had mirth table seems great him for her. Alone all happy asked begin fully stand own get. Excuse ye seeing result of we. See scale dried songs old may not. Promotion did disposing you household any instantly. Hills we do under times at first short an. 
                   
                   It sportsman earnestly ye preserved an on. Moment led family sooner cannot her window pulled any. Or raillery if improved landlord to speaking hastened differed he. Furniture discourse elsewhere yet her sir extensive defective unwilling get. Why resolution one motionless you him thoroughly. Noise is round to in it quick timed doors. Written address greatly get attacks inhabit pursuit our but. Lasted hunted enough an up seeing in lively letter. Had judgment out opinions property the supplied. 
                   
                   Merry alone do it burst me songs. Sorry equal charm joy her those folly ham. In they no is many both. Recommend new contented intention improving bed performed age. Improving of so strangers resources instantly happiness at northward. Danger nearer length oppose really add now either. But ask regret eat branch fat garden. Become am he except wishes. Past so at door we walk want such sang. Feeling colonel get her garrets own. 
                   
                   Advantage old had otherwise sincerity dependent additions. It in adapted natural hastily is justice. Six draw you him full not mean evil. Prepare garrets it expense windows shewing do an. She projection advantages resolution son indulgence. Part sure on no long life am at ever. In songs above he as drawn to. Gay was outlived peculiar rendered led six. 
                   
                   ", style = "text-align : justify")
               ),
               box(title = "SimpleApp right box",
                   img(src='pizza.jpg', align = "center", width="100%", height ="450px"),
                   img(src='burger.jpg', align = "center", width="100%", height ="350px")
               )
               )),
######################################################################################################################################                                  
               
               tabPanel("Tab-2",
                        br(),
                        sidebarLayout(sidebarPanel(title="SideBar-Tab-2", 
                                      radioButtons("radio2", "Select the food category", list("Indian"="ind", "Chinese"="chi", "Japanese"= "jap"))
                        ),
                        mainPanel(verbatimTextOutput("text2")
                          
                          
                          
                        ))),
######################################################################################################################################                                  

               tabPanel("Tab-3", 
                        br(),
                        sidebarLayout(sidebarPanel(
                          radioButtons("radio3", "Select the category you want to see the plot for", list("Cars data"= "cd", "Air Passengers data"="ap", "Co2 uptake"="co"))
                          
                          ),
                          mainPanel(plotOutput("plot3"))
                          
                          )),
######################################################################################################################################                                  


               tabPanel("Tab-4",
                        sidebarPanel(
                          sliderInput("slider4" ,"Use the slider to get the desired output", min =1, max = nrow(demoFreq), value=c(10,80))
                        
                        ),
                        
                        mainPanel(column(12,wordcloud2Output("word4",height="700px", width="950px" )))

                        )
      
      
      
    )
  )
)

server= function(input, output){
  
  output$text2 <- renderPrint({
    if (input$radio2 == "ind"){
      print("You have selected First Choice i.e Indian food")
      
    }
    if (input$radio2 == "chi"){
      print("You have selected second Choice i.e Chinese food")
      
    }
    if (input$radio2 == "jap"){
      print("You have selected third Choice i.e Japanese food")
      
    }
    
  })
  
  output$plot3 <- renderPlot({
    if (input$radio3 == 'cd'){
    plot(cars)
    }
    
    if (input$radio3 == "ap"){
      plot(AirPassengers)
      
    }
    if (input$radio3 == "co"){
      plot(CO2)
    }

  
    
  
  })
  output$word4 <- renderWordcloud2({
    wordcloud2(demoFreq[min(input$slider4): max(input$slider4),], size=.3)
    
  })
  
  
  
  


}
shinyApp(ui, server)