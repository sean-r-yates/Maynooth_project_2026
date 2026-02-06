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
library(htmltools)

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel="icon",type="image/x-icon" ,href="light_favicon.ico"),
    tags$link(rel="icon",type="image/png",size="32x32",href="light_favicon-32x32.png")
  ),
  tags$style(HTML("
                  .navbar .container-fluid,
                  .navbar .container {
                    width: 100% !important;
                    max-width: 100% !important;
                    }
                  
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
                  
                  .statsight-brand{
                    font-weight: 700;
                    letter-spacing: 1px;
                    font-size: 20px;
                    color: #FFFFFF !important;
                    cursor: pointer;
                  }
                  
                  .statsight-brand:hover,
                  .statsight-brand:focus{
                    colour: #FFFFFF !important;
                    text-decoration: none !important;
                  }
                  
                  .navbar-nav li a[data-value='landing']{
                    display: none!important;
                  }
                  /* drill buttons */
                  .drill-wrap {margin-bottom: 10px;}
                  .drill-title{font-weight: 700; margin-bottom: 6px;}
                    
                  .drill-row{
                    display: flex;
                    gap: 6px
                  }
                  
                  .drill-btn{
                    flex: 1;
                    width:100%;
                    margin-bottom:0;
                    background: #d6dbe0;
                    border: 1px solid #c7cdd3;
                    color: #2c3e50;
                  } 
                   /*created this for comparison tab*/
                .drill--btn{
                  flex: 1;
                  width: 100%;
                  margin-bottom: 0;
                  background: #d6dbe0;
                  border: 1px solid #c7cdd3;
                  color: #2c3e50;
                  
                  .drill-btn.is-active{
                    background: #2c7be5;
                    border-color: #2c7be5;
                    color: #fff;
                  }
                  /*created this for comparison tab*/
                  .drill--btn.is-active{
                    background: #2c7be5;
                    border-color: #2c7be5;
                    color: #fff;
                  }
                  
                   /*main panel in help and about*/ 
                  .haa-bg{
                    background: #ECF0F1
                  }
                  ")
  ),
  theme = shinytheme("flatly"),#theme of the letters
  navbarPage(
    title = tags$a(
      "StatSight.fm",#top left of the ui name
      class="statsight-brand",
      href="#",
      onclick="Shiny.setInputValue('home_click',Date.now(),{priority: 'event'});return false;"
    ),
    id = "main_nav",
    
    tabPanel(
      "Landing_page",
      value="landing",
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
          HTML("<h2><b>How to get your Spotify zip file</b></h2>
                          <h4>
                              First, you need to request your data. To do that, go to the Spotify Privacy page and log in.<br>
                              Select Extended streaming history and request the data.<br>
                              You will receive a confirmation email instantly; click the link in that email to start the request.<br>
                              It can take up to 30 days for Spotify to gather and send your data, though it may arrive sooner.<br>
                              Once you receive the second email, click the download link to save the .zip file to your computer or phone.<br><br>
                              Click on link below to be redirected to the Spotify Privacy Page<br>
                        </h4>
                        <a href='https://www.spotify.com/account/privacy/' target='_blank'>Click here</a>  <br><br>
                        <h2><b>How to Start</b></h2>
                        <h4>Upload your zip file to unlock the analytics tab and explore your listening insights.</h4><br>
                        <h6><u>Important: Remember you can only upload zip files, other type of files will not be accepted</u></h6>
                        ")
        )#main pannel
      )#navbar 1, tab panel
      
    ),
    ## TEMP TAB2                                               
    tabPanel(
      "Analytics",
      value="analytics",
      
      tabsetPanel(##add a subsection where the analytics is the father page and the treemap is the son page
        type= "tabs",
        navset_tab(
          nav_panel(
          "Home",
        
          sidebarLayout(
            sidebarPanel(
              width=2, ##reverted back as the graph isnt the main focus of the page to give insight
              ## thats for stuff in main
              
              div(
                class="drill-wrap",
                div(class="drill-title","Stats about:"),
                div(
                  class="drill-row",
                  actionButton("btn_user","User",class="drill-btn"),
                  actionButton("btn_artist","Artist",clas="drill-btn"),
                  actionButton("btn_song","Song", class="drill-btn"),
        ),
                
                uiOutput("filter_picker_ui")
              )
            ),
            
            mainPanel(##added div for aesthetic reasons on page
              div(
                class="main-bg",
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
                         div(class="chart-card",
                             uiOutput("heatmap_controls_ui"),
                             highchartOutput("graph_5_heatmap", height = "380px"))
                  )
                ),
                tags$hr(),
                uiOutput("text_area")
                
                
              )
            ))),
            nav_panel(
              "Comparison",
              sidebarLayout(
                sidebarPanel(
                  width = 4,
                  div(
                    class="drill-wrap",
                    div(class="drill-title","Comparing "),
                    div(
                      class="drill-row",
                      
                      actionButton("btn_-artist","Artists", class = "drill--btn"),
                      actionButton("btn_-song","Songs", class = "drill--btn")
                    )
                    #uiOutput("filter_picker_ui")
                  )),
                mainPanel(##added div for aesthetic reasons on page
                  div(
                    class="main-bg"),
                  fluidRow(
                    column(4,class="gap-col"),
                    
                    
                    column(4,class="gap-col")
                    
                  ))
                
                
              )
            )
          
        )
      )
    ),
    
    ##TEMP TAB3
    #temp 
    
    tabPanel(
      "Help", 
      navset_tab(
        nav_panel(title = "Usage of the Zip File", p(""),
                  
                  tags$head(
                    tags$style(HTML("
                                    body {
                                      background-color: #Ecf0f1; /* Use a specific HEX code or color name (e.g., 'lightgray') */
                                      
                                    }
                                  "))
                  ),
                  #how to download and use the file
                  HTML("<h2><b>How to get your Spotify zip file</b></h2>
                          <h4>
                              First, you need to request your data. To do that, go to the Spotify Privacy page and log in.<br>
                              Select Extended streaming history and request the data.<br>
                              You will receive a confirmation email instantly; click the link in that email to start the request.<br>
                              It can take up to 30 days for Spotify to gather and send your data, though it may arrive sooner.<br>
                              Once you receive the second email, click the download link to save the .zip file to your computer or phone.<br><br>
                              Click on link below to be redirected to the Spotify Privacy Page<br>
                        </h4>
                        <a href='https://www.spotify.com/account/privacy/' target='_blank'>Click here</a>  <br><br>
                        <h2><b>How to Start</b></h2>
                        <h4>Upload your zip file to unlock the analytics tab and explore your listening insights.</h4><br>
                        <h6><u>Important: Remember you can only upload zip files, other type of files will not be accepted</u></h6>
                        ")
                  
                  
        ),
        
        #gives a brief info on alaytics tab
        nav_panel(title = "Analytics Tab",p(""),
                  tags$head(
                    tags$style(HTML("
                                    body {
                                      background-color: #Ecf0f1; /* Use a specific HEX code or color name (e.g., 'lightgray') */
                                      
                                    }
                                  "))
                  ),
                  HTML("<h1><b>Visual and Interactive overview of the Analytics Tab</b></h1>
                        <h2><b>Pick an artist to filter by :</b></h2>
                         <h4>                
                              Here you can pick an artist of your choosing -  you only select one artist at a time.<br>           
                              The artists in the drop-down list are sorted from most to least listened to.<br>          
                              You can also pick artists from the TreeMap.<br><br>
                               
                         <h2><b>TreeMaps :</b></h2>
                         <h4>
                             Shows the artists/songs that dominate your listening.<br> 
                             (1)When no artist is picked/default: shows the overall treemap.<br>
                             (2)When an artist is picked: treemap of the artist picked.<br>
                             (3)When a song is selected: detailed block of the song picked.<br>
                             Overall:larger treemap blocks = more plays.<br><br>
                             
                         <h2><b>Gauge Charts : </b></h2>
                         <h4>
                             The gauge shows overall listening activity - higher values indicate more listening.<br>
                             It displays three things: (1)totals, (2)percenatges,(3) rankings<br>
                             The numbers change based on artist or song selection.<br><br>
                             
                         <h2><b>Line Graphs : </b></h2>
                         <h4>   
                             Listening trends over time - rising lines means increased listening.<br>
                             The line graph change based on aritst or song selection.<br><br>
                            
                         <h2><b>HeatMap : </b></h2>
                         <h4>Visualizes your listening intensity across days and times; darker heatmap areas means peak listening times.<br>
                             The heatmap changes based on artist or song selection as well.<br>
                         
                         <h2><b>Summary : </b></h2>
                         <h4>There is also a brief summary of the data below the line graph.<br>
                             The summary stats also depend on artist or song selection
                         
                                         ")
                  
        ))),
    #first tab gives info on how to download downlaod the zip file
    ##TEMP TAB4
    
    tabPanel(
      "About", 
      tags$head(
        tags$style(HTML("
                        body {
                          background-color: #Ecf0f1; /* Use a specific HEX code or color name (e.g., 'lightgray') */
                          
                        }
                    "))
      ),
      HTML("<h2><b>Authors</b></h2>
              <h3><b>Sean Ryan Yates<b></h3>
              <h4>Profession : Student(current)</h4> <h4>2nd year BSc in Data Science, Maynooth University</h4>
              <h4>Github: 
                      <a href='https://github.com/sean-r-yates' target='_blank'>
                        Personal GitHub
                      </a>
              </h4>
              <br><h3><b>Ankush Janak Katira<b></h3>
              <h4>Profession : Student(current)</h4> <h4>2nd year BSc in Data Science, Maynooth University</h4>
              <h4>Github: 
                      <a href='https://github.com/AnkushJK3' target='_blank'>
                        Personal GitHub
                      </a>
              </h4>
              <br><br><br>
              <h2>About the Project : </h2>
              <h4>Our mission was to compute the analytics of our favourite artists and their songs. <br>With this we can't not only see our favourite 
              artists and their songs but also look at different visualizations to understand the data better. The different visualizations include TreeMaps, 
              Gauge Charts, Line graphs and HeatMap.<br>
              In order to know more about the different visualizations you can visit the Analytics tab of the 
              Help tab.
              </h4>
             "))
  ) )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #hides the analytics page until zip is uploaded
  hideTab(inputId = "main_nav",target = "analytics")
  
  observeEvent(input$home_click,{
    updateNavbarPage(session,"main_nav",select="landing")
  }, ignoreInit = T)
  
  observe({set_active_button(drill_level())})
  
  
  ####################################################################
  ##                     "public variable"                          ##
  ####################################################################
  
  #
  drill_level<-reactiveVal("user") # user | artist | song
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
    showTab(inputId = "main_nav", target = "analytics")
    
    updateNavbarPage(session, "main_nav", selected = "analytics")
    
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
    
    #setting defaults on analytics page
    drill_level("user")
    set_active_button("user")
    
  })  
  
  set_active_button <- function(level){
    runjs(sprintf("
                  $('#btn_user').removeClass('is-active');
                  $('#btn_artist').removeClass('is-active');
                  $('#btn_song').removeClass('is-active');
                  $('#btn_%s').addClass('is-active');
                  ",
                  #$('#btn_diff_artist').removeClass('is-active');
                  #$('#btn_diff_songs').rmeoveClass('is-active');
                  
                  level))
  }
  
  
  ####################################################################
  ##              controls and drilling state updates               ##
  ####################################################################
  
  top_artist<-reactive({
    df<-base_df()
    req(df)
    df%>%
      group_by(artistName) %>%
      summarise(v=sum(hoursPlayed), .groups="drop") %>%
      arrange(desc(v)) %>%
      slice_head(n=1) %>%
      pull(artistName)
  })
  
  top_song_overall <- reactive({
    df <- base_df()
    req(df)
    df %>%
      group_by(artistName,trackName) %>%
      summarise(v =  sum(hoursPlayed), .groups="drop")%>%
      arrange(desc(v)) %>%
      slice_head(n=1)
  })
  
  
  top_song_for_artist <- reactive({
    df <- base_df()
    req(df)
    req(current_artist())
    df%>%
      filter(artistName == current_artist()) %>%
      group_by(trackName) %>%
      summarise(v = sum(hoursPlayed), .groups="drop") %>%
      arrange(desc(v)) %>%
      slice_head(n=1) %>%
      pull(trackName)
  })
  
  artist_for_current_song <- reactive({
    df<- base_df()
    req(df)
    req(current_song())
    df %>%
      filter(trackName==current_song()) %>%
      group_by(artistName) %>%
      summarise(v=sum(hoursPlayed), .groups="drop") %>%
      arrange(desc(v)) %>%
      slice_head(n=1) %>%
      pull(artistName)
  })
  
  
  page_state <- reactive({
    if (!is.null(current_song()))  return("song")
    if (!is.null(current_artist())) return("artist")
  })
  
  observeEvent(input$picker, {
    selected <- input$picker
    
    if (is.null(selected)||selected==""){
      current_artist(selected)
      current_song(NULL)
      
    } else if(drill_level()=="song") { 
      current_song(selected)
      #keep current_artist equal with selected song
      current_artist({
        df<- base_df()
        df%>%
          filter(trackName == selected) %>%
          group_by(artistName) %>%
          summarise(v=sum(hoursPlayed), .groups="drop") %>%
          arrange(desc(v)) %>%
          slice_head(n=1) %>%
          pull(artistName)
      })
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
  output$heatmap_controls_ui <- renderUI({
    req(base_df())
    tagList(
      selectInput("heatmap_view",NULL, choices = c("Weekly","Monthly"), selected="Weekly"),
      uiOutput("month_picker_ui")
    )
  })
  
  output$filter_picker_ui <- renderUI({
    df<- base_df()
    req(df)
    
    if(drill_level()=="user"){
      return(NULL) # no dropdown visible on default
    }
    
    if(drill_level()=="artist"){
      selectizeInput(
        "picker",
        "Pick artist to filter by:",
        choices = artist_choice(),
        selected = current_artist() %||% "",
        multiple = F,
        options = list(placeholder="Seach artists...")
      )
    } else{
      #song dropdown: songs of selected artist first, but whole dataset is searchable
      req(current_artist())
      songs_for_artist <- df%>%
        filter(artistName == current_artist())%>%
        group_by(trackName)%>%
        summarise(v=sum(hoursPlayed), .groups="drop")%>%
        arrange(desc(v)) %>%
        pull(trackName)
      
      all_songs<-df %>%
        distinct(trackName)%>%
        pull(trackName)
      
      ordered <-c(songs_for_artist,setdiff(all_songs,songs_for_artist))
    
      selectizeInput(
        "picker",
        "Pick song to filter by:",
        choices = ordered,
        selected = current_song() %||% "",
        multiple = F,
        options = list(placeholder = "Search songs...")
      )
    }
  })
  
  
  
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
    
    if(drill_level()=="user"){
      return(df)
    }
    #added this to see if it works
    #if(drill_level() =="artist" && !is.null(current_artist()))
     # {
      # df <- df %>% filter(artistName == current_artist())
    #}
    #added first
    
   if(!is.null(current_artist())){
    df<- df%>% filter(artistName == current_artist())
    }
    if(drill_level()=="song" && !is.null(current_song())){
      df<- df%>% filter(trackName == current_song())
    }
    
    df
  })
  
  #buttons 3 on analysis home 
  observeEvent(input$btn_user,{
    drill_level("user")
    current_artist(NULL)
    current_song(NULL)
    set_active_button("user")
  }, ignoreInit = T)
  
  observeEvent(input$btn_artist,{
    if (drill_level()=="song" && !is.null(current_song())){
      #user clicking artist from song (artist who made the song)
      current_artist(artist_for_current_song())
      current_song(NULL)
    } else{
      #user clicking artist from default (top 1 artist)
      current_artist(top_artist())
      current_song(NULL)
    }
    drill_level("artist")
    set_active_button("artist")
  }, ignoreInit = T)
  
  observeEvent(input$btn_song,{
    if (drill_level()=="artist"&&!is.null(current_artist())){
      # user clicks song from artist (top song of that artist)
      current_song(top_song_for_artist())
    } else{
      # user clicks song from default (top song overall)
      ts<- top_song_overall()
      if (nrow(ts)>0){
        current_artist(ts$artistName[[1]]) # keep the artist from top 1 song
        current_song(ts$trackName[[1]])
      }
    }
    drill_level("song")
    set_active_button("song")
  }, ignoreInit = T)
  
  
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
    
    
    make_chart("% of total time Listened", share, 100, suffix = "%")
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
    
    in_song <- (drill_level()=="song"&&!is.null(current_song()))
    
    if(in_song){
      daily <- df_f %>%
        group_by(date)%>%
        summarise(y= n(), .groups="drop") %>%
        arrange(date)
      
      y_title <- "Plays"
      series_name <- "Plays"
    } else{
      daily <- df_f %>%
        group_by(date)%>%
        summarise(y= sum(hoursPlayed), .groups="drop") %>%
        arrange(date)
      
      y_title <- "Hours"
      series_name <- "Hours"
    }
    
    
    series<- Map(function(d,h){
      list(datetime_to_timestamp(as.POSIXct(d)),h)
    },daily$date,daily$y)
    
    
    title_text<- if (in_song){
      paste0("Daily plays: ",current_song())
    }else if (!is.null(current_artist()) && drill_level()=="artist"){
      paste0("Daily hours: ",current_artist())
    }else {
      "Daily hours"
    }
    
    highchart()%>%
      hc_chart(type ="line",zoomType ="x")%>%
      hc_title(text = title_text) %>%
      hc_xAxis(type ="datetime")%>%
      hc_yAxis(title =list(text=y_title)) %>%
      hc_add_series(
        name= series_name,
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
  
  
  output$month_picker_ui <- renderUI({
    req(input$heatmap_view)
    if (input$heatmap_view != "Monthly") return(NULL)
    
    df <- base_df()
    req(df)
    months <- sort(unique(format(df$date, "%Y-%m")))
    
    selectInput(
      "month_choice", NULL,
      choices = c("All (mean across months)"="ALL", months),
      selected = "ALL"
    )
  })
  
  ##chart 5
  output$graph_5_heatmap <- renderHighchart({
    df_f <- filtered_df()
    req(nrow(df_f)>0)
    req(input$heatmap_view)
    
    #metric hours normally, plays in song drill level
    use_plays <- (drill_level() == "song" && !is.null(current_song()))
    df0<- df_f %>%
      mutate(
        metric = if(use_plays) 1 else hoursPlayed
      )
    
    if(input$heatmap_view == "Weekly"){
      #mean per weekday/hour across all dates
      df_h <-df0%>%
        mutate(
          weekday = format(date,"%a"),
          hour_label = sprintf("%02d:00", hour)
        )%>%
        group_by(date, weekday, hour_label) %>%
        summarise(v=sum(metric), .groups="drop") %>%
        group_by(weekday, hour_label) %>%
        summarise(z= mean(v), .groups = "drop")
      
      weekday_levels <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
      hour_levels <- sprintf("%02d:00", 0:23)
      
      df_h$weekday <- factor(df_h$weekday, levels= weekday_levels)
      df_h$hour_label <- factor(df_h$hour_label,levels=hour_levels)
      
      heat<- df_h %>%
        complete(weekday,hour_label, fill=list(z=0))%>%
        mutate(
          x = as.integer(hour_label) - 1L,
          y = as.integer(weekday) - 1L
        )
      
      title_text <- if (use_plays) "Mean listening Weekly" else "Mean listening Weekly"
      suffix <- if (use_plays) " plays" else " h"
      
      return (
        highchart() %>%
          hc_chart(type="heatmap") %>%
          hc_title(text= title_text) %>%
          hc_xAxis(categories= hour_levels,title=list(text="Time of day")) %>%
          hc_yAxis(categories= weekday_levels,title=list(text=NULL),reversed=T) %>%
          hc_add_series(
            name= if(use_plays) "Mean plays" else "Mean hours",
            data= Map(function(x,y,z) list(x,y,z), heat$x,heat$y,heat$z),
            borderWidth=0
          ) %>%
          hc_tooltip(
            formatter = JS(sprintf("
                                   function(){
                                      var day = this.series.yAxis.categories[this.point.y];
                                      var time= this.series.xAxis.categories[this.point.x];
                                      var val = this.point.value;
                                      return '<b>' + day + '</b><br/>' + time + ' : <b>' + (val).toFixed(2) + '%s</b>';
                                   }", suffix))
          )
      )
    } else {
      # monthly: 
      # - all: mean per day of month across all months
      # - specific month : show that months values of day of month 
      req(input$month_choice)
      
      df_m <- df0 %>% mutate(
        day= as.integer(format(date,"%d")),
        ym = format(date,"%Y-%m")
      ) %>%
        group_by(date,day,ym)%>%
        summarise(v=sum(metric), .groups="drop")
      
      if(input$month_choice != "ALL"){
        df_m <- df_m %>% filter(ym==input$month_choice)
      }
      
      df_m2 <- df_m %>%
        group_by(day) %>%
        summarise(z=mean(V), .groups = "drop") %>%
        complete(day = 1:31, fill = list(z=0))%>%
        mutate(x=day-1L, y= 0L)
      
      day_levels <- as.character(1:31)
      suffix <- if(use_plays) " plays" else " h"
      
      return (      
        highchart()%>%
          hc_chart(type = "heatmap")%>%
          hc_title(text = "Mean listening Monthly") %>%
          hc_xAxis(categories = day_levels, title = list(text= "Day of month")) %>%
          hc_yAxis(categories = c(""), title=list(text=NULL),reversed=T)%>%
          hc_add_series(
            name = if (use_plays) "Mean plays" else "Mean hours",
            data = Map(function(x,y,z) list(x,y,z), df_m2$x, df_m2$y, df_m2$z),
            borderWidth = 0
          ) %>%
          hc_tooltip(
            formatter = JS(sprintf("
                                   function(){
                                      var day = this.series.xAxis.categories[this.point.x];
                                      var val = this.point.value;
                                      return '<b>Day ' + day + '</b><br/><b>' + (val).toFixed(2) + '%s</b>';
                                   }", suffix))
          )
      )
    }
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
