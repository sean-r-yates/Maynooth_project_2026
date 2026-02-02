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
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(jsonlite)
library(dplyr)
library(highcharter)
library(tidyr)
library(bslib)

ui <- fluidPage(
  useShinyjs(),#added html for main 
  tags$style(HTML("
                  /* main panel in analytics */
                  .main-bg {
                    background: #ECF0F1;
                    padding: 12px;
                    border-radius: 10px;
                  }
                
                  .gap-col { 
                    padding-left: 10px !important; 
                    padding-right: 10px !important; 
                    }
                  
                  .chart-card {
                    background: #F7F9FA;
                    border-radius: 10px;
                    padding: 10px;
                    box-shadow: 0 1px 2px rgba(0,0,0,0.06);
                  } 
                  ")
  ),
  theme = shinytheme("flatly"),#theme of the letters
  navbarPage(
    "Extra Spotify",#top left of the ui name
    id = "main_nav",
    
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
          ),
          
          #submit button
          #submitButton(text = "Submit", icon = "", width = 250),
          
        ),#side pannel
        mainPanel(
          h2("Import your Spotify data"),
          h4("it might be called \"my_spotify_data.zip\" ")
        )#main pannel
      )#navbar 1, tab panel
      
    ),
    ## TEMP TAB2                                               
    tabPanel(
      "Analytics",
      
      tabsetPanel(##add a subsection where the analytics is the father page and the treemap is the son page
        type= "tabs",
        tabPanel(
          "Home",
          sidebarLayout(
            sidebarPanel(
              width=3, ##reverted back as the graph isnt the main focus of the page to give insight
              ## thats for stuff in main
              
              #asks for user input
              selectizeInput(
                "artistpicker",
                "pick a artist to filter by:",
                choices=NULL,
                multiple=F
              ),
              actionButton("back_to_artists","Go Back"),    
              highchartOutput("artist_track_treemap",height = "600px",width = "100%")%>%withSpinner()
            ),
            mainPanel(##added div for aesthetic reasons on page
              div(class="main-bg",
                  fluidRow(
                    column(4,class="gap-col",
                           div(class="chart-card",highchartOutput("graph_1_chart",height = "180px"))
                    ),
                    
                    column(4,class="gap-col",
                           div(class="chart-card", highchartOutput("graph_2_chart", height = "180px"))
                    ),
                    
                    column(4, class="gap-col",
                           div(class="chart-card",highchartOutput("graph_3_chart", height = "180px"))
                    )
                  ),
                  tags$hr(),
                  fluidRow(
                    column(8, class="gap-col",
                           div(class="chart-card",highchartOutput("graph_4_plot", height = "380px"))
                    ),
                    column(4,class="gap-col",
                           div(class="chart-card", highchartOutput("graph_5_heatmap", height = "380px"))
                    )
                  ),
                  tags$hr(),
                  uiOutput("text_area")
                  
                  
              )
              #details on main page
              
              
            )
          )
        )
      )
    ),
    ##TEMP TAB3
    tabPanel(
      "Help",
      "me"
    ),
    
    ##TEMP TAB4
    
    tabPanel(
      "About", "more on this later"
      
    ),
    nav_spacer(),nav_spacer()
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #hides the analytics page until zip is uploaded
  hideTab(inputId = "main_nav",target = "Analytics")
  
  ####################################################################
  ##                     "public variable"                          ##
  ####################################################################
  
  #drill state 
  current_artist <- reactiveVal(NULL)
  
  #drill state
  current_song <- reactiveVal(NULL)
  
  #the artist chosen
  artist_choice <- reactiveVal(NULL)
  
  #has full merged streaming history after zip upload
  merged_music_history <- reactiveVal(NULL)
  
  
  ####################################################################
  ##            Unloading the zip file into usable data            ##
  ####################################################################
  
  observeEvent(input$zip_,{
    
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
    
    
    #unhides Analytics tab once up has been uploaded
    showTab(inputId = "main_nav", target = "Analytics")
    
    
    ####################################################################
    ##              once execution after zip upload                   ##
    ####################################################################
    
    
    
    #all artists from files 
    all_artists <- merged.music.files %>%
      distinct(artistName) %>%
      arrange(artistName) %>%
      pull(artistName)
    
    #filtering for 50 top artists sorted by listening time
    top50_artists <- merged.music.files %>%
      group_by(artistName)%>%
      summarise(total_ms = sum(msPlayed), .groups="drop")%>%
      arrange(desc(total_ms))%>%
      slice_head(n=50)%>%#change for more or less top artists
      pull(artistName)
    
    #top 50 songs + rest 
    ordered_top50and_artists <- c(top50_artists,setdiff(all_artists,top50_artists))
    
    artist_choice(ordered_top50and_artists)
    
    #server side drop down in selection 
    updateSelectizeInput(
      session,
      "artistpicker",
      choices = artist_choice(),
      selected = character(0),
      server = T
    )
    
    #reset drill state on new upload
    current_artist(NULL)
    current_song(NULL)
  })  
  ####################################################################
  ##              controls and drilling state updates               ##
  ####################################################################
  page_state <- reactive({
    if (!is.null(current_song()))  return("song")
    if (!is.null(current_artist())) return("artist")
    "none"
  })
  #back button
  observeEvent(input$back_to_artists, {
    if(!is.null(current_song())){
      current_song(NULL)
    } else{
      current_artist(NULL)
      current_song(NULL)
      
      updateSelectizeInput(
        session,
        "artistpicker",
        choices = artist_choice(),
        selected=character(0),
        server=T
      )
    }
    
  })
  
  #capture clicks on artist tile
  observeEvent(input$artist_clicked, {
    current_artist(input$artist_clicked)
    current_song(NULL) #reset song when changing artist
    
    #push clicked artist to picker bar
    updateSelectizeInput(
      session,
      "artistpicker",
      choices = artist_choice(),
      selected = input$artist_clicked,
      server=T
    )
  },ignoreInit = T) #this make it so that when app first runs it doesnt fire this chunk
  
  
  observeEvent(input$song_clicked,{
    req(input$song_clicked)
    current_song(input$song_clicked)
    
  },ignoreInit = T)
  
  observeEvent(input$artistpicker, {
    selected <- input$artistpicker
    
    if (is.null(selected)||selected==""){
      current_artist(NULL)
      current_song(NULL)
      
    } else{
      current_artist(selected)
      current_song(NULL)
      
    }
  },ignoreInit = T)
  
  
  
  ####################################################################
  ##                          Main panel                            ##
  ####################################################################
  
  ms.to.hours <- function(ms) ms/1000/60/60
  
  #the function to make the graph for the charts 1-2-3
  make_chart <- function(title, value, max_value, suffix = ""){
    value <- as.numeric(value)
    max_value<-max(1e-9,as.numeric(max_value)) #avoids 0 as max
    
    highchart()%>%
      hc_chart(type = "solidgauge")%>%
      hc_title(text = NULL) %>%
      hc_pane(
        center=c("50%","85%"),
        size= "100%",
        startAngle = -100,
        endAngle = 100,
        background = list(
          borderRadius =5,
          outerRadius = "100%",
          innerRadius = "85%",
          shape = "arc"
        ))%>%
      hc_tooltip(enabled =F)%>%
      hc_exporting(enabled=F)%>%
      hc_yAxis(
        min=0,
        max=max_value,
        tickAmount=0,
        lineWidth =0,
        tickWidth =0,
        minorTickInterval=NULL,
        labels = list(enabled=F),
        title = list(
          text=title,
          y=-70,
          style=list(fontWeight="bold")
        ),
        stops = list(
          list(0.1,"#4caf50"),
          list(0.5,"#FDD835"),
          list(0.9,"#F44336")
        )
      )%>%
      hc_plotOptions(
        solidgauge = list(
          rounded=T,
          dataLabels = list(
            y=5,
            borderWidth=0,
            useHTML=T,
            format = paste0(
              "<div style='text-align:center'>",
              "<span style='font-size:26px'>{y:.1f}</span>",
              "<span style='font-size:12px;opacity:0.6'>", suffix, "</span>",
              "</div>"
            )
          )
        )
      )%>%
      hc_add_series(
        name = title,
        data = list(value),
        type="solidgauge",
        radius="100%",
        innerRadius = "85%"
      )
  }
  
  #getting the date into a useable format
  base_df <- reactive({
    df<- merged_music_history()
    req(df)
    
    df %>%
      mutate(
        endTime = as.POSIXct(endTime,format = "%Y-%m-%d %H:%M", tz=""),
        date = as.Date(endTime),
        hour = as.integer(format(endTime,"%H")),
        hoursPlayed = ms.to.hours(msPlayed)
      )%>%
      filter(!is.na(date),!is.na(hour))
  })
  
  filtered_df <- reactive({
    df <- base_df()
    
    if(!is.null(current_artist())){
      df<- df%>% filter(artistName == current_artist())
    }
    if(!is.null(current_song())){
      df<- df%>% filter(trackName == current_song())
    }
    
    df
  })
  
  #changing the back button on analytics home page to depend on state
  observe({
    state <- page_state()
    
    if(state =="none"){
      disable("back_to_artists")
      updateActionButton(session, "back_to_artists", label = "Go Back")
    } else if (state == "artist"){
      enable("back_to_artists")
      updateActionButton(session, "back_to_artists", label = "Back to Artists")
    } else{
      enable("back_to_artists")
      updateActionButton(session, "back_to_artists", label = "Back to Songs")
    }
  })
  
  output$analytics_home_page <- renderUI({
    switch(
      page_state(),
      none = tags$h3("no specific artist selected"),
      artist = tags$h3("artist selected"),
      song = tags$h3("song selected")
    )
  })
  
  ####################################################################
  ##            rendering charts1,2,3,4/stat details                ##
  ####################################################################
  ###
  # calculating the stats
  ###
  overall_totals <- reactive({
    df <- base_df()
    req(df)
    
    
    total.hours.all <- sum(df$hoursPlayed)
    
    top.artist.hours <- df %>%
      group_by(artistName) %>%
      summarise(h= sum(hoursPlayed), .groups = "drop")%>%
      summarise(max_ = max(h))%>%
      pull(max_)
    
    top.song.hours <- df%>%
      group_by(trackName)%>%
      summarise(h= sum(hoursPlayed), .groups = "drop")%>%
      summarise(max_ = max(h), .groups = "drop") %>%
      pull(max_)
    
    date.min <-min(df$date)
    date.max <-max(df$date)
    total.days.range <- as.integer(date.max-date.min)+1L
    
    list(
      total_hours_all = total.hours.all,
      top_artist_hours = top.artist.hours,
      top_song_hours  = top.song.hours,
      date_min        = date.min,
      date_max        = date.max,
      total_days_range= total.days.range
    )
    
  })
  
  ##stats for graph3
  rank_stats <- reactive({
    df<-base_df()
    req(df)
    
    artist_rank<-df%>%
      group_by(artistName)%>%
      summarise(h=sum(hoursPlayed),.groups = "drop")%>%
      arrange(desc(h))%>%
      mutate(rank=row_number())
    
    song_rank<-df%>%
      group_by(trackName)%>%
      summarise(h=sum(hoursPlayed), .groups = "drop")%>%
      arrange(desc(h))%>%
      mutate(rank=row_number())
    
    list(
      artist_rank = artist_rank,
      song_rank   = song_rank,
      n_artists   = n_distinct(df$artistName)
    )
  })
  ###
  # rendering the charts
  ###
  
  ##chart 1
  output$graph_1_chart <- renderHighchart({
    totals <- overall_totals()
    df_f <- filtered_df()
    
    #value = hours in current state 
    hours_in_state <- sum(df_f$hoursPlayed)
    
    #max value 
    max_val <- if (is.null(current_artist()) && is.null(current_song())){
      totals$total_hours_all ## default max == total hours
    } else if (!is.null(current_song())){
      totals$top_song_hours  ## song max == top song hours
    } else {
      totals$top_artist_hours     ## artists max == top artists hours
    }
    
    make_chart("Total Hours Listened", hours_in_state,max_val,suffix = "h")
  })
  
  ##chart 2
  output$graph_2_chart <- renderHighchart({
    totals <- overall_totals()
    df_f <- filtered_df()
    
    #share of total listening time 
    hours_in_state <- sum(df_f$hoursPlayed)
    share <- 100 * hours_in_state / max(1e-9, totals$total_hours_all) 
    
    
    make_chart("% of total time",share,100,suffix = "%")
  })
  
  ##chart 3
  output$graph_3_chart <- renderHighchart({
    rs <- rank_stats()
    
    if(is.null(current_artist())&& is.null(current_song())){
      #number of unique artists in data
      make_chart("Artists Played", rs$n_artists,rs$n_artists,suffix="")
    }else if (!is.null(current_song())){
      #song rank 
      r<- rs$song_rank %>% filter(trackName == current_song())%>%pull(rank)
      r<- ifelse(length(r)==0,NA,r)
      make_chart("Song Rank",r,nrow(rs$song_rank),suffix= "")
    }else{
      #artists rank
      r<- rs$artist_rank %>% filter(artistName == current_artist())%>%pull(rank)
      r<- ifelse(length(r)==0,NA,r)
      make_chart("Artists Rank",r,nrow(rs$artist_rank),suffix= "")
      
    }
  })
  
  ##chart 4
  output$graph_4_plot <-renderHighchart({
    df_f <- filtered_df()
    req(nrow(df_f)>0)
    
    daily <- df_f %>%
      group_by(date) %>%
      summarise(hours = sum(hoursPlayed), .groups ="drop")%>%
      arrange(date)
    
    
    series<- Map(function(d,h){
      list(datetime_to_timestamp(as.POSIXct(d)),h)
    },daily$date,daily$hours)
    
    
    title_text<- if (!is.null(current_song())){
      paste0("Daily hours: ",current_song())
    }else if (!is.null(current_artist())){
      paste0("Daily hours: ",current_artist())
    }else {
      "Daily hours"
    }
    
    highchart()%>%
      hc_chart(type ="line",zoomType ="x")%>%
      hc_title(text = title_text) %>%
      hc_xAxis(type="datetime")%>%
      hc_yAxis(title=list(text="Hours")) %>%
      hc_add_series(
        name= "Hours",
        data= series
      )%>%
      hc_plotOptions(
        line=list(
          marker=list(enabled=F),
          lineWidth=3
        )
      )%>%
      hc_tooltip(
        pointFormat = "<b>{point.y:.2f} hours</b>"
      )
  })
  ####################################################################
  ##                      Heatmap render                            ##
  ####################################################################
  
  ##chart 5
  output$graph_5_heatmap <- renderHighchart({
    df_f <- filtered_df()
    req(nrow(df_f)>0)
    
    # 1 hour bins 
    df_h <- df_f %>%
      mutate(
        bin_label = sprintf("%02d:00",hour),
        weekday = format(date,"%a")
      )
    
    #mon - sun order
    weekday_levels <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
    df_h$weekday <-factor(df_h$weekday,levels = weekday_levels)
    
    # 2 hour bins in order
    bin_levels <- sprintf("%02d:00", 0:23)
    df_h$bin_label <-factor(df_h$bin_label, levels = bin_levels)
    
    heat <- df_h %>%
      group_by(weekday,bin_label)%>%
      summarise(z=sum(hoursPlayed),.groups="drop")%>%
      complete(weekday,bin_label,fill =list(z=0))%>%
      mutate(
        x= as.integer(bin_label) - 1L,
        y= as.integer(weekday) - 1L
      )
    
    title_text <- if (!is.null(current_song())){
      "Listening Heatmap (Song)"
    } else if (!is.null(current_artist())){
      "Listening Heatmap (Artist)"
    } else{
      "Listening Heatmap"
    }
    
    highchart()%>%
      hc_chart(type="heatmap")%>%
      hc_title(text = title_text) %>%
      hc_xAxis(categories = bin_levels,title= list(text="Time of day"))%>%
      hc_yAxis(categories = weekday_levels,title=list(text=NULL),reversed =T)%>%
      hc_colorAxis(
        stops = list(
          list(0.0, "#FFFFFF"),
          list(0.35, "#4caf50"),
          list(0.7, "#FDD835"),
          list(1.0, "#F44336")
        )
      )%>%
      hc_add_series(
        name="Hours",
        data=Map(function(x,y,z) list(x,y,z),heat$x,heat$y,heat$z),
        borderWidth = 0
      )%>%
      hc_tooltip(
        formatter = JS("
                       function(){
                       var day = this.series.yAxis.categories[this.point.y];
                       var time = this.series.xAxis.categories[this.point.x];
                       return '<b>' +day+ '</b><br/>'+time+' : <b>' + this.point.value.toFixed(2) + 'h</b>';
                       }
                       ")
      )
  })
  
  output$text_area<- renderUI({
    df_all <- base_df()
    df_f <- filtered_df()
    req(nrow(df_all)>0)
    
    total_hours_all<- sum(df_all$hoursPlayed)
    hours_in_state<- sum(df_f$hoursPlayed)
    
    state<- page_state()
    header <- if (state =="none"){
      "Default"
    } else if (state=="artist"){
      paste0("Artist selected: ",current_artist())
    } else{
      paste0("Song selected: ", current_song(), " (",current_artist(),")")
    }
    
    share <- 100*hours_in_state / max(1e-9,total_hours_all)
    
    tags$div(
      tags$h4(header),
      tags$p(paste0("Hours Listened: ", round(hours_in_state,2),"h")),
      tags$p(paste0("Precentage of Listening Time: ", round(share,2),"%")),
      tags$p(paste0("Date range from: ",
                    as.character(min(df_f$date)),
                    " To: ",
                    as.character(max(df_f$date))))
      
    )
  })
  
  
  
  
  ####################################################################
  ##                      Treemap render                            ##
  ####################################################################
  
  #render treemap
  output$artist_track_treemap <- renderHighchart({
    df <- merged_music_history()
    req(df)
    
    ####
    #   Song has been selected
    ####
    if(!is.null(current_song())){
      artist <- current_artist()
      track <- current_song()
      
      song.df<- df%>%
        filter(artistName == artist, trackName == track)%>%
        summarise(
          value = sum(msPlayed),
          .groups = "drop"
        )
      
      single.song<- data.frame(
        id= track,
        name=track,
        artist=artist,
        value=song.df$value
      )
      
      return(
        highchart() %>%
          hc_chart(type="treemap")%>%
          hc_title(text = paste0("Selected Song: ", track))%>%
          hc_add_series(
            data= list_parse(single.song),
            type= "treemap",
            layoutAlgorithm = "squarified",
            colorByPoint=T,
            borderWidth=0,
            dataLabels = list(enabled = T)
          )%>%
          hc_tooltip(
            pointFormatter= JS("
                               function(){
                               var mins = this.value / 60000;
                               return '<b>' + this.name + ' -By '+this.artist+ '</b><br/>'
                               +'Listening time: '+mins.toFixed(1) + 'minutes';
                               }
                               ")
          )
      )
      
    }
    ####
    #   artist has been selected
    ####
    if (!is.null(current_artist())){
      # songs for selected artist
      #state 2 (artist)showing selected artists's songs
      artist <- current_artist()
      
      songs<- df%>%
        filter(artistName == artist) %>%
        group_by(trackName) %>%
        summarise(value = sum(msPlayed), .groups = "drop") %>%
        mutate(id = trackName, name = trackName)%>%
        select(id,name,value)
      
      return(
        highchart()%>%
          hc_chart(type = "treemap")%>%
          hc_title(text = paste0("Selected Artist: ", artist)) %>%
          hc_plotOptions(
            series = list(
              point = list(
                events = list(
                  click = JS("
                             function(){
                             Shiny.setInputValue('song_clicked',this.id,{priority: 'event'});
                             }
                             ")
                )
              )
            )
          )%>%
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
      )
    }
    ####
    #   Nothing has been selected (default)
    ####
    #top 50 artists
    #state 1 (none) showing top 50 artists
    
    top.artists <- df %>%
      group_by(artistName)%>%
      summarise(total_ms = sum(msPlayed), .groups="drop")%>%
      arrange(desc(total_ms))%>%
      slice_head(n=50)%>%#change for more or less top artists
      pull(artistName)
    
    artists.only <- df %>%
      filter(artistName %in% top.artists)%>%
      group_by(artistName)%>%
      summarise(value = sum(msPlayed), .groups="drop")%>%
      mutate(id = artistName, name = artistName)%>%
      select(id,name,value)
    
    #making the graph
    
    highchart()%>%
      hc_chart(type= "treemap") %>%
      hc_title(text= "Top 50 artists")%>%
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
    
    
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
