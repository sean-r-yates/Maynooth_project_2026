# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#--------------NOTES----------------
#FILES OF INTEREST; 
#   file name               = Marquee 
#   variable "artistName"   = name of artist 
#   variable "segment"      = status of listening 
#                          (previously active listeners)
#                          (light listeners)
#                          (Moderate listeners)
#                          (Super Listeners)
#   Notes // this is a file storing the level of listening per artist
    
#   file name               = StreamingHistory_music_[number 0-N]
#   variable "endTime"      = time the user stopped listened to a song
#   variable "artistName"   = name of artist
#   variable "trackName"    = name of the song
#   variable "msPlayed"     = is time played in ms per song (songs repeat in the file)
#   Notes // this is a file storing each time user plays a song not total amount played

#   file name               = StreamingHistory_podcast_[number 0-N]
#   variable "endTime"      = time the user stopped listened to a podcast
#   variable "podcastName"  = name of podcast
#   variable "episodeName"  = name of episode
#   variable "msPlayed"     = is time played in ms per episode (songs repeat in the file)
#   Notes // file is pretty much the same as the music alternative 

#   file name               = YourSoundCapsule
#   variable "highlights"   = is the category
#   variable "highlightType"= is the type of unique 
#   Notes // take inspo from this file as its premium users only and dont actually use data from it


library(shiny)
library(shinythemes)
library(utils)

ui <- fluidPage(theme = shinytheme("flatly"),#theme of the letters
    navbarPage(
      
      "Extra Spotify",#top left of the ui name
      tabPanel("Home",
               sidebarPanel(
                 tags$h3("Import Zip:"),
                 fileInput("zip_",#this is the data variable name
                           label =NULL,#text
                           multiple = FALSE,
                           accept=".zip")
                
                ),#side pannel
               mainPanel(
                        h2("Import your Spotify data"),
                        
                        h4("it might be called \"my_spotify_data.zip\" "),
                      
               )#main pannel
        ),#navbar 1, tab panel
        tabPanel("Wrapped", "temp1"),
        tabPanel("Help", "temp2")
    
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$zip_,{
    
    req(input$zip_)#confirms that the zip is uploaded before going ahead
    temp_dir <- tempdir()
    unzip(input$zip_$datapath,exdir = temp_dir)
    files <- list.files(temp_dir, recursive = TRUE, full.names = F)
    #file directory is temp_dir(base) and continued by files 
    print(temp_dir)
    print(files)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
