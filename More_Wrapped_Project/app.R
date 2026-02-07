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
library(rlang)
library(DT)


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
                  .drill-btn.is-active{
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
          h2("Import your Spotify data"),
          h4("it might be called \"my_spotify_data.zip\" ")
        )#main pannel
      )#navbar 1, tab panel
      
    ),
    ## TEMP TAB2                                               
    tabPanel(
      "Analytics",
      value="analytics",
      
      tabsetPanel(##add a subsection where the analytics is the father page and the treemap is the son page
        type= "tabs",
        tabPanel(
          "Home",
          sidebarLayout(
            sidebarPanel(
              width=3, ##reverted back as the graph isnt the main focus of the page to give insight
              ## thats for stuff in main
              
              div(
                class="drill-wrap",
                div(class="drill-title","Stats about:"),
                div(
                  class="drill-row",
                  actionButton("btn_user","User",class="drill-btn"),
                  actionButton("btn_artist","Artist",class="drill-btn"),
                  actionButton("btn_song","Song", class="drill-btn")
                ),
                
                uiOutput("filter_picker_ui"),
                uiOutput("date_range_ui"),
                DTOutput("rank_table")          
              )
            ),
            
            mainPanel(##added div for aesthetic reasons on page
              div(
                class="main-bg",
                fluidRow(
                  column(4,class="gap-col",
                         div(class="chart-card",highchartOutput("graph_3_chart", height = "180px"))
                         
                  ),
                  
                  column(4,class="gap-col",
                         div(class="chart-card", highchartOutput("graph_2_chart", height = "180px"))
                  ),
                  
                  column(4, class="gap-col",
                         div(class="chart-card",uiOutput("text_area"))#,height = "180px"))
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
                tags$hr()
                
                
                
              )
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
                  ", level))
  }
  ####################################################################
  ##                          data table                            ##
  ####################################################################
  
  output$date_range_ui <- renderUI({
    df<-base_df()
    req(df)
    
    min_d <- min(df$date)
    max_d <- max(df$date)
    
    sliderInput(
      "date_range",
      label="Date range",
      min = min_d,
      max= max_d,
      value = c(min_d,max_d),
      timeFormat = "%Y-%m-%d"
    )
  })
  
  rank_table_df <- reactive({
    df <- base_df()
    req(df)
    
    #apply date range to table too
    if (!is.null(input$date_range)){
      df<- df%>% filter(date>= input$date_range[1],date<=input$date_range[2])
    }
    
    if(drill_level()=="user"){
      return(df%>%
               group_by(artistName)%>%
               summarise(playtime= sum(hoursPlayed), .groups="drop")%>%
               arrange(desc(playtime))%>%
               mutate(
                 rank = row_number(),
                 artist= artistName,
                 song=""
               )%>%
               select(rank,artist,song,playtime)
      )
    }
    
    if(drill_level()=="artist"){
      req(current_artist())
      return(
        df%>%
          filter(artistName == current_artist())%>%
          group_by(trackName)%>%
          summarise(playtime = sum(hoursPlayed),.groups = "drop")%>%
          arrange(desc(playtime))%>%
          mutate(
            rank=row_number(),
            artist = current_artist(),
            song= trackName
          )%>%
          select(rank,artist,song,playtime)
      )
    }
    
    df %>%
      group_by(artistName, trackName)%>%
      summarise(playtime = sum(hoursPlayed), .groups ="drop")%>%
      arrange(desc(playtime))%>%
      mutate(rank=row_number(),
             artist=artistName,
             song=trackName)%>%
      select(rank,artist,song,playtime)
    
    
    
  })
  
  
  output$rank_table<-renderDT({
    datatable(
      rank_table_df(),
      rownames=F,
      selection = "single",
      options = list(
        order = list(list(0,"asc")),
        pageLength=10,
        lengthChange=F
      )
    )%>%
      formatRound("playtime",2)
  })
  
  observeEvent(input$rank_table_rows_selected,{
    i <- input$rank_table_rows_selected
    if (length(i) == 0) return()
    
    row <- rank_table_df()[i, , drop = FALSE]
    
    if (drill_level() == "user") {
      current_artist(row$artist[[1]])
      current_song(NULL)
      drill_level("artist")
      set_active_button("artist")
      return()
    }
    
    #in artist or song drill
    if (!is.null(row$song[[1]]) && row$song[[1]] != "") {
      current_artist(row$artist[[1]])
      current_song(row$song[[1]])
      drill_level("song")
      set_active_button("song")
    }
  })
  
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
    "none"
  })
  
  observeEvent(input$picker, {
    selected <- input$picker
    
    if (is.null(selected)||selected=="") return ()
    
    if (drill_level()=="artist"){
      current_artist(selected)
      current_song(NULL)
      return()
    }
    
    if (drill_level() == "song") {
      current_song(selected)
      
      df <- base_df()
      a <- df %>%
        filter(trackName == selected) %>%
        group_by(artistName) %>%
        summarise(v = sum(hoursPlayed), .groups = "drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 1) %>%
        pull(artistName)
      
      current_artist(if (length(a) == 0) NULL else a[[1]])
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  
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
        choices = NULL,
        selected = NULL,
        options = list(placeholder="Seach artists...")
      )
    } else{
      selectizeInput(
        "picker",
        "Pick song to filter by:",
        choices = NULL,
        selected = NULL,
        options = list(placeholder = "Search songs...")
      )
    }
  })
  observeEvent(drill_level(),{
    df<- base_df()
    req(df)
    
    if (drill_level()=="artist"){
      updateSelectizeInput(
        session,"picker",
        choices = artist_choice(),
        selected = isolate(current_artist()) %||%"",
        server = T
      )
    }
    
    if (drill_level()== "song"){
      
      if(!is.null(input$date_range)){
        df<- df%>%filter(date>=input$date_range[1],date<=input$date_range[2])
      }
      
      ordered <- df%>%
        group_by(artistName,trackName) %>%
        summarise(v=sum(hoursPlayed),.groups="drop")%>%
        arrange(desc(v))%>%
        pull(trackName)
      
      all_songs<- df%>%distinct(trackName) %>% pull(trackName)
      choices <- c(ordered, setdiff(all_songs, ordered))
      
      updateSelectizeInput(
        session, "picker",
        choices = choices,
        selected = isolate(current_song())%||%"",
        server=T
      )
    }
  }, ignoreInit = T)
  
  
  
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
    
    if (!is.null(input$date_range)){
      df <- df%>% filter(date >= input$date_range[1],date<=input$date_range[2])
    }
    if(drill_level()=="user"){
      return(df)
    }
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
    
    if (drill_level() == "user") {
      df_u <- filtered_df()
      n <- n_distinct(df_u$artistName)
      return(make_chart("Artists Played", n, n, suffix = ""))
    }
    if (!is.null(current_song())){
      #song rank 
      r<- rs$song_rank %>% filter(trackName == current_song())%>%pull(rank)
      r<- ifelse(length(r)==0,NA,r)
      return(make_chart("Song Rank",r,nrow(rs$song_rank),suffix= ""))
    }
    #artists rank
    r<- rs$artist_rank %>% filter(artistName == current_artist())%>%pull(rank)
    r<- ifelse(length(r)==0,NA,r)
    make_chart("Artists Rank",r,nrow(rs$artist_rank),suffix= "")
      
    
  })
  
  ##chart 4
  output$graph_4_plot <-renderHighchart({
    df_f <- filtered_df()
    req(nrow(df_f)>0)
    req(input$heatmap_view)
    req(input$heatmap_agg)
    
    view<-input$heatmap_view
    agg<- input$heatmap_agg
    
    use_plays <- (drill_level() == "song" && !is.null(current_song()))
    
    if(in_song){
      daily <- df_f %>%
        group_by(date)%>%
        summarise(y= n(), .groups="drop") %>%
        arrange(date)
      
      y_title <- "Plays"
      series_name <- "Plays"
      unit<- " Plays"
      decimals <- 0
    } else{
      daily <- df_f %>%
        group_by(date)%>%
        summarise(y= sum(hoursPlayed), .groups="drop") %>%
        arrange(date)
      
      y_title <- "Hours"
      series_name <- "Hours"
      unit<- " Hours"
      decimals <- 2
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
          marker=list(enabled=T, radius =4 ),# radius is the size of the points on graph
          lineWidth=3
        )
      )%>%
      hc_tooltip(
        formatter = JS(sprintf("
                              function(){
                                var d = Highcharts.dateFormat('%%e %%b %%Y', this.x);
                                var v = this.y;
                                return '<b>' + d + '</b><br/><b>' + v.toFixed(%d) + '%s</b>';
                              }", decimals, unit))
      )
  })
  ####################################################################
  ##                      Heatmap stuff                            ##
  ####################################################################
  output$month_picker_ui <- renderUI({
    req(input$heatmap_view)
    if (input$heatmap_view != "Monthly") return(NULL)
    
    df <- filtered_df()
    req(nrow(df) > 0)
    
    months <- df %>%
      mutate(ym = format(date, "%Y-%m")) %>%
      distinct(ym) %>%
      arrange(desc(ym)) %>%
      pull(ym)
    
    selectInput(
      "month_choice",
      "Month:",
      choices = c("ALL", months),
      selected = if (!is.null(input$month_choice)) input$month_choice else "ALL"
    )
  })
  
  
  


  
  output$heatmap_controls_ui <- renderUI({
    tagList(
      div(style="margin-bottom:6px;",
          radioButtons(
            "heatmap_view", label = NULL,
            choices = c("Weekly", "Monthly"),
            selected = isolate(input$heatmap_view %||% "Weekly"),
            inline = T
          )
      ),
      div(style="margin-bottom:6px;",
          radioButtons(
            "heatmap_agg", label = NULL,
            choices = c("Sum", "Mean"),
            selected = isolate(input$heatmap_agg %||% "Mean"),
            inline = T
          )
      ),
      uiOutput("month_picker_ui")
    )
  })
  
  
  ##chart 5
  output$graph_5_heatmap <- renderHighchart({##not closing correctly
    df_f <- filtered_df()
    req(nrow(df_f) > 0)
    req(input$heatmap_view)
    req(input$heatmap_agg)
    
    view <- input$heatmap_view
    agg  <- input$heatmap_agg

    #metric hours normally, plays in song drill level
    use_plays <- (drill_level() == "song" && !is.null(current_song()))

    df0 <- df_f %>%
      mutate(
        metric = if (use_plays) 1 else hoursPlayed,
        weekday = factor(format(date, "%a"), levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
        hour_label = factor(sprintf("%02d:00", hour), levels = sprintf("%02d:00", 0:23)),
        ym = format(date, "%Y-%m"),
        day = as.integer(format(date, "%d"))
      )
    
    ######################################################
    ####-----------------------WEEKLY
    ####################################################
    
    if(view == "Weekly"){#mean per weekday/hour across all dates
      if(agg == "Mean"){
        #mean logic heatmap
        df_h <- df0 %>%
          group_by(date, weekday, hour_label) %>%
          summarise(v = sum(metric), .groups = "drop") %>%
          group_by(weekday, hour_label) %>%
          summarise(z = mean(v), .groups = "drop")
        title_text <- "Mean Listening Weekly"
        
      }else{
        #sum logic heatmap
        df_h <- df0 %>%
          group_by(weekday, hour_label) %>%
          summarise(z = sum(metric), .groups = "drop")
        title_text <- "Sum Listening Weekly"
        
      }
      
      

      heat <- df_h %>%
        complete(weekday, hour_label, fill = list(z = 0)) %>%
        mutate(
          x = as.integer(hour_label) - 1L,
          y = as.integer(weekday) - 1L
        )
      
      suffix <- if (use_plays) " plays" else " h"
      
      return (
        highchart() %>%
          hc_chart(type = "heatmap") %>%
          hc_title(text = title_text) %>%
          hc_xAxis(categories = levels(df0$hour_label), title = list(text = "Time of day")) %>%
          hc_yAxis(categories = levels(df0$weekday), title = list(text = NULL), reversed = TRUE) %>%
          hc_colorAxis(stops = list(
              list(0.0, "#FFFFFF"),
              list(0.35, "#4caf50"),
              list(0.7, "#FDD835"),
              list(1.0, "#F44336")
            ))%>%
          hc_add_series(
            name = if (use_plays) paste(agg, "plays") else paste(agg, "hours"),
            data = Map(function(x, y, z) list(x, y, z), heat$x, heat$y, heat$z),
            borderWidth = 0
          ) %>%
          hc_tooltip(formatter = JS(sprintf("
                                            function(){
                                              var day = this.series.yAxis.categories[this.point.y];
                                              var time = this.series.xAxis.categories[this.point.x];
                                              var val = this.point.value || 0;
                                              return '<b>' + day + '</b><br/>' + time + ' : <b>' + val.toFixed(2) + '%s</b>';
                                            }", suffix)))
            )
    } 
    # monthly: 
    # - all: mean per day of month across all months
    # - specific month : show that months values of day of month 
  
    
    ######################################################
    ####-----------------------Monthly
    ####################################################
    #daily total per date
    m_choice <- if (is.null(input$month_choice) || input$month_choice == "") "ALL" else input$month_choice
    
    df_daily <- df0 %>%
      group_by(date) %>%
      summarise(v = sum(metric), .groups = "drop") %>%
      mutate(
        ym = format(date, "%Y-%m"),
        day = as.integer(format(date, "%d"))
      )
   
    req(nrow(df_daily) > 0)
    
    ym_show <- if (m_choice == "ALL") max(df_daily$ym) else m_choice
    
    first_date <- as.Date(paste0(ym_show, "-01"))
    last_date  <- seq(first_date, by = "1 month", length.out = 2)[2] - 1
    dates_in_month <- seq(first_date, last_date, by = "day")
    
    
    #value per day per current month
    if (m_choice == "ALL") {
      by_dom <- df_daily %>%
        group_by(day) %>%
        summarise(z = if (agg == "Mean") mean(v) else sum(v), .groups = "drop")
      
      month_vals <- tibble(
        date = dates_in_month,
        day = as.integer(format(dates_in_month, "%d"))
      ) %>%
        left_join(by_dom, by = "day") %>%
        mutate(z = coalesce(z, 0)) %>%
        select(date, z)
      
      title_text <- paste0(agg, " Listening (Monthly, All Months)")
      
    } else {
      month_vals <- tibble(date = dates_in_month) %>%
        left_join(df_daily %>% filter(ym == ym_show) %>% select(date, v), by = "date") %>%
        mutate(z = coalesce(v, 0)) %>%
        select(date, z)
      
      title_text <- paste0(agg, " Listening: ", format(first_date, "%b-%Y"))
    }
    
    
    #get day
    wd_levels <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
    wd_index <- function(d) as.integer(format(d, "%u")) - 1L
    
    #how many blank spots before day 1
    offset <- wd_index(first_date)
    n_month <- length(dates_in_month)
    
    
  
    #creating 6x7 grid of 42 cells
    grid <- tibble(cell = 0:41) %>%
      mutate(
        day_index = cell - offset,
        idx = day_index + 1L,
        valid = idx >= 1L & idx <= n_month,
        x = cell %% 7,
        y = cell %/% 7,
        date = as.Date(NA)
      ) %>%
      mutate(
        date = replace(date, valid, dates_in_month[idx[valid]]),
        day_label = if_else(!is.na(date), as.character(as.integer(format(date, "%d"))), "")
      ) %>%
      select(-idx, -valid, -day_index) %>%
      left_join(month_vals, by = "date") %>%
      mutate(z = coalesce(z, 0))
    

    #build points with labels and the tool tip
    pts <- Map(function(x, y, z, day_label, date){
      list(
        x = x, y = y, value = z,
        day_label = day_label,
        date_str = if (is.na(date)) "" else format(date, "%Y-%m-%d")
      )
    }, grid$x, grid$y, grid$z, grid$day_label, grid$date)
    
    suffix <- if(use_plays)" plays" else " h"
    
    highchart() %>%
      hc_chart(type = "heatmap") %>%
      hc_title(text = title_text) %>%
      hc_xAxis(categories = wd_levels, title = list(text = NULL)) %>%
      hc_yAxis(categories = rep("", 6), title = list(text = NULL), reversed = T) %>%
      hc_colorAxis(
        stops = list(
          list(0.0, "#FFFFFF"),
          list(0.35, "#4caf50"),
          list(0.7, "#FDD835"),
          list(1.0, "#F44336")
        ),
        min = 0
      ) %>%
      hc_plotOptions(
        heatmap = list(
          borderWidth = 1,
          borderColor = "#e6e6e6",
          dataLabels = list(
            enabled = T,
            useHTML = T,
            formatter = JS("
                          function(){
                            return '<span style=\"font-size:11px;color:#2c3e50;opacity:0.85\">' +
                                   (this.point.day_label || '') + '</span>';
                          }")
          )
        )
      ) %>%
      hc_add_series(
        name = if (use_plays) paste(agg, "plays") else paste(agg, "hours"),
        data = pts
      ) %>%
      hc_tooltip(formatter = JS(sprintf("
                                          function(){
                                            if(!this.point.date_str) return '';
                                            var v = this.point.value;
                                            if (v === null || v === undefined) v = 0;
                                            return '<b>' + this.point.date_str + '</b><br/><b>' + v.toFixed(2) + '%s</b>';
                                          }", suffix)))
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
