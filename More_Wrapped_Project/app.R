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
library(shinyWidgets)
library(shinycssloaders)
library(utils)
library(jsonlite)
library(dplyr)
library(highcharter)

ui <- fluidPage(
    theme = shinytheme("flatly"),#theme of the letters
    navbarPage(
      "Extra Spotify",#top left of the ui name
      
      tabPanel(
        "Home",
        sidebarLayout(
          sidebarPanel(
            tags$h3("Import Zip:"),
            fileInput(
              "zip_",#this is the data variable name
              label =NULL,#text
              multiple = F,
              accept=".zip"
              )
          ),#side pannel
          mainPanel(
            h2("Import your Spotify data"),
            h4("it might be called \"my_spotify_data.zip\" ")
          )#main pannel
        )#navbar 1, tab panel
      ),
      ## TEMP TAB2                                               
      tabPanel(
        "artist",
        sidebarLayout(
          sidebarPanel(
            #asks for user input
            selectInput(
              "artistpicker",
              "pick a artist to filter by:",
              choices=c(),
              multiple=T
            ),
            actionButton("back_to_artists","Back to artists")      
          ),
          mainPanel(
            highchartOutput("artist_track_treemap",height = "650px")%>%withSpinner()
          )
        )
      ),
      ##TEMP TAB3
      tabPanel(
        "Help",
        "temp2"
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #has full merged streaming history after zip upload
  merged_music_history <- reactiveVal(NULL)
  
  #which artist we currently have drilled into 
  current_artist <- reactiveVal(NULL)
  
  observeEvent(input$zip_,{
    
    ####################################################################
    ##            Unloading the zip file into usable data            ##
    ####################################################################
    
    req(input$zip_)#confirms that the zip is uploaded before going ahead
    
    temp_dir <- tempdir()
    unzip(input$zip_$datapath,exdir = temp_dir)
    
    #gets the file names for music history files
    music.files <- list.files(
      temp_dir,
      pattern="StreamingHistory_music_[0-9]+\\.json$",
      recursive = T, 
      full.names = T
    )
    
    req(length(music.files) > 0)
    
    #merging the music files 
    merged.music.files <- bind_rows(lapply(music.files,function(i){
      fromJSON(i,flatten = TRUE)
    }))
    
    #saving the full data set
    merged_music_history(merged.music.files)
    
    #gets the file names for podcast history files
    podcast.history.file.names <- list.files(
      temp_dir,
      pattern="StreamingHistory_podcast_[0-9]+\\.json$",
      recursive = T, 
      full.names = T
    )
    
    #merging the podcast files
    merged.podcast.history <- bind_rows(lapply(podcast.history.file.names,function(i){
      fromJSON(i,flatten = TRUE)
    }))
    
    #getting the files for Marquee
    listening.levels.file.dir <- file.path(temp_dir,"Spotify Account Data/Marquee.json")
    listening.level <- fromJSON(listening.levels.file.dir,flatten = TRUE)
    
    #filtering for 100 top artists
    top.artists <- merged.music.files %>%
      group_by(artistName)%>%
      summarise(total_ms = sum(msPlayed), .groups="drop")%>%
      arrange(desc(total_ms))%>%
        slice_head(n=100)%>%#change for more or less top artists
        pull(artistName)
    
    
    #this is returning to the drop down 
    updateSelectInput(
      session,
      "artistpicker",
      choices = sort(unique(top.artists)),
      selected = character(0)
    )

    
    current_artist(NULL)#incase user uploads another zip file
  })  
    ####################################################################
    ##            dealing with the treemap on page temp2              ##
    ####################################################################
  
  #back button
  observeEvent(input$back_to_artists, {
    current_artist(NULL)
  })
  
  #capture clicks on artist tile
  observeEvent(input$artist_clicked, {
    req(input$artist_clicked)
    current_artist(input$artist_clicked)
  })
  
  #render treemap
  output$artist_track_treemap <- renderHighchart({
    df <- merged_music_history()
    req(df)
    
    #top 100 artists
    top.artists <- df %>%
      group_by(artistName)%>%
      summarise(total_ms = sum(msPlayed), .groups="drop")%>%
      arrange(desc(total_ms))%>%
      slice_head(n=100)%>%#change for more or less top artists
      pull(artistName)
    
    df <- df %>% filter(artistName %in% top.artists)
    
    #artists only treemap
    if (is.null(current_artist())){
      artists.only <- df %>%
        group_by(artistName)%>%
        summarise(value = sum(msPlayed), .groups="drop")%>%
        mutate(id = artistName, name = artistName)%>%
        select(id,name,value)
      
      #making the graph
      highchart()%>%
        hc_chart(type= "treemap") %>%
        hc_title(text= "Top 100 artists")%>%
        hc_plotOptions(
          series = list(
            point = list(
              events = list(
                click = JS("
                           function (){
                           Shiny.setInputValue('artist_clicked', this.name, {priority: 'event'});
                           }
                          ")
              )
            )
          )
        )%>%#adding more details to the graph
        hc_add_series(
          data = list_parse(artists.only),
          type = "treemap",
          layoutAlgorithm= "squarified",
          colorByPoint = T, #unique colour per artist
          borderWidth = 0,
          dataLabels = list(enabled = T)
        )%>%
        hc_tooltip(
          pointFormatter= JS("
                             function(){
                             var mins = this.value/60000;
                             return '<b>' +this.name+ '</b><br/>'+
                                    'Listening time: '+ mins.toFixed(1) + 'minutes';
                             }
                            ")
        )
      
    } else{
        # songs for selected artist
        artist <- current_artist()
        
        songs<- df%>%
          filter(artistName == artist) %>%
          group_by(trackName) %>%
          summarise(value = sum(msPlayed), .groups = "drop") %>%
          mutate(id = trackName, name = trackName)%>%
          select(id,name,value)
        
        highchart()%>%
          hc_chart(type = "treemap")%>%
          hc_title(text = paste0("Songs for: ", artist)) %>%
          hc_add_series(
            data= list_parse(songs),
            type="treemap",
            layoutAlgorithm = "squarified",
            colorByPoint = T,
            borderWidth = 0,
            dataLabels = list(enabled = T) #keeps song labels hidden
          )%>%
          hc_tooltip(
            pointFormatter = JS("
                                function(){
                                var mins = this.value/60000;
                                return '<b>' + this.name + '</b><br/>' +
                                       'Listening time: '+ mins.toFixed(1) + 'minutes';
                                }
                                ")
          )
        }
  })

}
# Run the application 
shinyApp(ui = ui, server = server)

