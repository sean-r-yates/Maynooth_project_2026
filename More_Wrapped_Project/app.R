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
library(shinyjs)
library(jsonlite)
library(dplyr)
library(highcharter)
library(tidyr)
library(DT)
library(rlang)
library(bslib)

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel="icon",type="image/x-icon" ,href="light_favicon.ico"),
    tags$link(rel="icon",type="image/png",size="32x32",href="light_favicon-32x32.png"),
    tags$script(HTML("
                      window.compPickedIds = [];
                      Shiny.addCustomMessageHandler('comp_sel_ids', function(ids){
                        window.compPickedIds = ids || [];
                        var el = $('#comp_table table');
                        if(!el.length) return;
                        var t = el.DataTable();
                        if(!t) return;
                        t.rows().every(function(){
                          var d = this.data();
                          var n = this.node();
                          if(!d || !n) return;
                          var id = d[d.length-1];
                          if(window.compPickedIds.indexOf(id) >= 0){
                            $(n).addClass('comp-picked');
                          }else{
                            $(n).removeClass('comp-picked');
                          }
                        });
                      });
                    ")
    )
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
                    color: #FFFFFF !important;
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
                  
                  
                  table.dataTable tbody tr.comp-picked td{
                    background:#2c7be5 !important;
                    color:#fff !important;
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
      
      fluidRow(
        
        #ZIP upload
        column(
          4,
          wellPanel(
            tags$h3("Import Zip:"),
            fileInput(
              "zip_",
              label = NULL,
              multiple = F,
              accept = ".zip"
            )
          )
        ),
        
        #manual file upload
        column(
          4,
          wellPanel(
            tags$h3("Import Files:"),
            tags$p(tags$u("Required:")),
            tags$p("• StreamingHistory_music_[0-9].json (1 or more)"),
            tags$p("• Marquee.json (exact name)"),
            
            fileInput(
              "music_files",
              label = "StreamingHistory files",
              multiple = TRUE,
              accept = ".json"
            ),
            
            fileInput(
              "marquee_file",
              label = "Marquee.json",
              multiple = FALSE,
              accept = ".json"
            )
          )
        ),
        
        #text area 
        column(
          4,
          wellPanel(
            HTML("<h2><b>How to Start</b></h2>
            
                <h4>Upload your zip file <i>or</i> upload the required files to unlock the analytics tab and explore your listening insights.</h4><br>
                
                <h6><u>Important: Only .zip or the exact .json files are accepted</u></h6>
                
                <p> for more help visit the help page
                        ")
          )
        )
        
      )
    ),
    
    ## analytics tab                                               
    tabPanel(
      "Analytics",
      value = "analytics",
      navset_tab(##add a subsection where the analytics is the father page and the treemap is the son page
        id = "analytics_tabs",
        nav_panel(
          "Home",
          sidebarLayout(
            sidebarPanel(
              width = 3,##reverted back as the graph isnt the main focus of the page to give insight
              ## thats for stuff in main
              div(
                class = "drill-wrap",
                div(class = "drill-title", "Stats about:"),
                div(
                  class = "drill-row",


                  
                  actionButton("btn_artist", "Artist", class = "drill-btn"),
                  actionButton("btn_song", "Song", class = "drill-btn"),
                  actionButton("btn_user", "Reset", class = "drill-btn")
                ),

                uiOutput("filter_picker_ui"),
                uiOutput("date_range_ui"),
                DTOutput("rank_table")
              )
            ),
            mainPanel(##added div for aesthetic reasons on page
              div(
                class = "main-bg",
                fluidRow(
                  column(
                    4, class = "gap-col",
                    div(class = "chart-card", highchartOutput("graph_3_chart", height = "180px"))
                  ),
                  column(
                    4, class = "gap-col",
                    div(class = "chart-card", highchartOutput("graph_2_chart", height = "180px"))
                  ),
                  column(
                    4, class = "gap-col",
                    div(class = "chart-card", uiOutput("text_area"))
                  )
                ),
                tags$hr(),
                fluidRow(
                  column(
                    8, class = "gap-col",
                    div(class = "chart-card", highchartOutput("graph_4_plot", height = "380px"))
                  ),
                  column(
                    4, class = "gap-col",
                    div(
                      class = "chart-card",
                      uiOutput("heatmap_controls_ui"),
                      highchartOutput("graph_5_heatmap", height = "380px")
                    )
                  )
                ),
                tags$hr()
              )
            )
          )
        ),
        nav_panel(
          "Comparison",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              
              # simple heading + mode
              div(style="margin-bottom:8px;font-weight:700;","Compare:"),
              div(
                class = "drill-row", 
                actionButton("comp_btn_artist","Artists",class = "drill-btn"),
                actionButton("comp_btn_song","Songs", class = "drill-btn")
              ),
              
              tags$div(style="margin-top:8px;"),
              DTOutput("comp_table"),
              
            ),
            
            mainPanel(
              div(
                class = "main-bg",
                div(
                  style = "margin-bottom:8px; display:flex; align-items:center; gap:10px;",
                  tags$span(tags$b("Metric:")),
                  radioButtons(
                    "comp_metric", label=NULL,
                    choices=c("Hours","Plays"),
                    selected="Hours",
                    inline=T
                  )
                ),
                div(
                  class="chart-card",
                  highchartOutput("comp_month_bar", height = "300px")
                ),
                tags$div(style="height:10px;"),
                fluidRow(
                  column(
                    6, class = "gap-col",
                    div(
                      class="chart-card",
                      highchartOutput("comp_month_line", height = "300px")
                    )
                  ),
                  column(
                    3, class = "gap-col",
                    div(
                      class="chart-card",
                      uiOutput("comp_text_area_a")
                    )
                  ),
                  column(
                    3, class = "gap-col",
                    div(
                      class="chart-card",
                      uiOutput("comp_text_area_b")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    #help tab
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
                        <h4>Upload your zip file to unlock the analytics tab and explore your listening insights.</h4>
                        <h4>You could also upload the required files to unlock the analytics tab and explore </h4>
                        <h6><u>Important: Remember you can only upload zip files, other type of files will not be accepted</u></h6>
                        <h6><u>Important: Only .zip or the exact .json files are accepted</u></h6>
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
    #about page
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
              <h4>2nd year BSc in Data Science, Maynooth University</h4>
               
                      <a href='https://github.com/sean-r-yates' target='_blank'>
                        GitHub
                      </a>
              
              <br><h3><b>Ankush Janak Katira<b></h3>
              <h4>2nd year BSc in Data Science, Maynooth University</h4>

                      <a href='https://github.com/AnkushJK3' target='_blank'>
                        GitHub
                      </a>
              
              <br><br><br>
              <h2>About the Project : </h2>
              <h4>Our mission was to compute the analytics of our favourite artists and their songs. <br>With this we can't not only see our favourite 
              artists and their songs but also look at different visualizations to understand the data better. The different visualizations include TreeMaps, 
              Gauge Charts, Line graphs and HeatMap.<br>
              In order to know more about the different visualizations you can visit the Analytics tab of the 
              Help tab.
              </h4>
             "))
  ) 
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #hides the analytics page until zip is uploaded
  hideTab(inputId = "main_nav",target = "analytics")
  
  observeEvent(input$home_click,{
    updateNavbarPage(session,"main_nav",select="landing")
  }, ignoreInit = T)
  
  observe({set_active_button(drill_level())})
  
  #warning message toast (fyummy)
  toast <- function(msg, type="message"){
    showNotification(msg, type = type, duration = 6)
  }
  
  #validate streaminghistory
  observeEvent(input$music_files,{
    
    if(!is.null(input$zip_) || identical(data_source(),"zip")) return()
    
    mf <- input$music_files
    if(is.null(mf)) return()
    
    mf_names <- mf$name %||% character(0)
    ok <- grepl("^StreamingHistory_music_[0-9]+\\.json$", mf_names)
    
    if(length(mf_names) < 1 || any(!ok)){
      toast('Missing "StreamingHistory_music_[0-9].json" (example: StreamingHistory_music_0.json).', type="error")
      reset("music_files")
      return()
    }
    
  }, ignoreInit = T)
  
  #validate marquee
  observeEvent(input$marquee_file,{
    
    if(!is.null(input$zip_) || identical(data_source(),"zip")) return()
    
    mq <- input$marquee_file
    if(is.null(mq)) return()
    
    mq_name <- mq$name %||% ""
    if(!identical(mq_name, "Marquee.json")){
      toast('Missing "Marquee.json" (example: Marquee.json).', type="error")
      reset("marquee_file")
      return()
    }
    
  }, ignoreInit = T)
  
  
  ####################################################################
  ##                     "public variable"                          ##
  ####################################################################
  
  #keep the type of data source NULL zip manual
  data_source <- reactiveVal(NULL)
  
  #YYYY-MM
  comp_selected_month <- reactiveVal(NULL)
  
  #user artist or song
  drill_level<-reactiveVal("user") 
  #drill state 
  current_artist <- reactiveVal(NULL)
  
  #drill state
  current_song <- reactiveVal(NULL)
  
  #the artist chosen
  artist_choice <- reactiveVal(NULL)
  
  #has full merged streaming history after zip upload
  merged_music_history <- reactiveVal(NULL)
  
  #analytics compare button var
  comp_mode_rv <- reactiveVal("Artist")
  
  ####################################################################
  ##                          manual files                         ##
  ####################################################################
  
  observeEvent(
    {
      list(input$music_files, input$marquee_file)
    },
    {
      
      # if zip exists ignore manual
      if(!is.null(input$zip_) || identical(data_source(),"zip")) return()
      
      mf <-input$music_files
      mq <-input$marquee_file
      
      # run only if both exist
      if(is.null(mf) || is.null(mq)) return()
      
      #process manual files
      data_source("manual")
      
      merged.music.files <-bind_rows(lapply(mf$datapath, function(p){
        fromJSON(p, flatten = T)
      }))
      
      merged_music_history(merged.music.files)
      
      
      listening.level <- fromJSON(mq$datapath, flatten = T)
      
      #once execution after upload 
      all_artists <- merged.music.files %>%
        distinct(artistName) %>%
        arrange(artistName) %>%
        pull(artistName)
      
      
      
      top50_artists <- merged.music.files %>%
        group_by(artistName) %>%
        summarise(total_ms = sum(msPlayed), .groups="drop") %>%
        arrange(desc(total_ms)) %>%
        slice_head(n=50) %>%
        pull(artistName)
      
      
      ordered_top50and_artists <- c(top50_artists, setdiff(all_artists, top50_artists))
      artist_choice(ordered_top50and_artists)
      
      current_artist(NULL)
      current_song(NULL)
      
      
      drill_level("user")
      set_active_button("user")
      
      
      showTab(inputId = "main_nav", target = "analytics")
      updateNavbarPage(session, "main_nav", selected = "analytics")
      
    },ignoreInit = T)
  
  
  
  
  
  
  
  
  
  
  ####################################################################
  ##            Unloading the zip file into usable data            ##
  ####################################################################
  
  
  
  observeEvent(input$zip_,{
    
    req(input$zip_)#confirms that the zip is uploaded before going ahead
    data_source("zip")
    
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
      fromJSON(i,flatten = T)
    }))
    
    #saving the full data set
    merged_music_history(merged.music.files)
    
    
    #getting the files for Marquee
    listening.levels.file.dir <- file.path(temp_dir,"Spotify Account Data/Marquee.json")
    listening.level <- fromJSON(listening.levels.file.dir,flatten = T)
    
    
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
               select(rank,artist,playtime)
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
  },server =T)
  
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
  ##                       Main panel Analytics                     ##
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
        options = list(placeholder="Search artists...")
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
  
  
  
  ####################################################################
  ##            rendering charts1,2,3,4/stat details                ##
  ####################################################################
  ###
  # calculating the stats
  ###
  overall_totals <- reactive({
    df <- base_df()
    req(df)
    list(
      total_hours_all = sum(df$hoursPlayed)
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
      n_current <- n_distinct(filtered_df()$artistName)
      n_all <- n_distinct(base_df()$artistName)
      
      return(make_chart("Artists Played", n_current, n_all, suffix = ""))
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
    
    in_song <- (drill_level()=="song"&&!is.null(current_song()))
    
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
      paste0("(Dragable) Daily plays: ",current_song())
    }else if (!is.null(current_artist()) && drill_level()=="artist"){
      paste0("(Dragable) Daily hours: ",current_artist())
    }else {
      "(Dragable) Daily hours"
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
    
    month_labbels <- format(as.Date(paste0(months,"-01")), "%b %Y")
    month_choice <- c("ALL"= "ALL", as.list(setNames(months, month_labbels)))
    
    selectInput(
      "month_choice",
      "Month:",
      choices = month_choice,
      selected = input$month_choice %||% "ALL"
    )
  })
  
  observe({
    if (!is.null(input$heatmap_view) &&
        input$heatmap_view == "Monthly" &&
        !is.null(input$month_choice) &&
        input$month_choice != "ALL") {
      updateRadioButtons(session, "heatmap_agg", selected = "Sum")
    }
  })
  
  
  output$heatmap_controls_ui <- renderUI({
    show_agg <- T
    if (!is.null(input$heatmap_view) && input$heatmap_view == "Monthly") {
      m_choice <- input$month_choice %||% "ALL"
      show_agg <- identical(m_choice, "ALL")
    }
    
    tagList(
      div(style="margin-bottom:6px;",
          radioButtons(
            "heatmap_view", label = NULL,
            choices = c("Weekly", "Monthly"),
            selected = isolate(input$heatmap_view %||% "Weekly"),
            inline = T
          )
      ),
      if (show_agg) {
        div(style="margin-bottom:6px;",
            radioButtons(
              "heatmap_agg", label = NULL,
              choices = c("Sum", "Mean"),
              selected = isolate(input$heatmap_agg %||% "Sum"),
              inline = T
            )
        )},
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
    
    wd_levels <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
    
    if (m_choice == "ALL") {
      # Aggregate across all months by day-of-month (1..31)
      by_dom <- df_daily %>%
        group_by(day) %>%
        summarise(
          z = if (agg == "Mean") mean(v) else sum(v),
          .groups = "drop"
        )
      
      # force ALL to 31 days
      dom_tbl <- tibble(day = 1:31) %>%
        left_join(by_dom, by = "day") %>%
        mutate(z = coalesce(z, 0))
      
      # fixed calendar anchor so day positions are stable (Mon start)
      first_date <- as.Date("2001-01-01")  # Monday
      offset <- 0L
      n_month <- 31L
      
      title_text <- paste0("All ",agg, " Listening")
      
      grid <- tibble(cell = 0:41) %>%
        mutate(
          day_index = cell - offset,
          idx = day_index + 1L,
          valid = idx >= 1L & idx <= n_month,
          x = cell %% 7,
          y = cell %/% 7,
          day = if_else(valid, idx, NA_integer_),
          date = as.Date(NA),
          day_label = if_else(valid, sprintf("%02d", idx), "")
        ) %>%
        left_join(dom_tbl %>% select(day, z), by = "day") %>%
        mutate(z = coalesce(z, 0))
      
      pts <- Map(function(x, y, z, day_label){
        list(
          x = x, y = y, value = z,
          day_label = day_label,
          date_str = if (day_label == "") "" else paste0("Day ", day_label)
        )
      }, grid$x, grid$y, grid$z, grid$day_label)
      
    } else {
      ym_show <- m_choice
      first_date <- as.Date(paste0(ym_show, "-01"))
      last_date  <- seq(first_date, by = "1 month", length.out = 2)[2] - 1
      dates_in_month <- seq(first_date, last_date, by = "day")
      
      month_vals <- tibble(date = dates_in_month) %>%
        left_join(df_daily %>% filter(ym == ym_show) %>% select(date, v), by = "date") %>%
        mutate(z = coalesce(v, 0)) %>%
        select(date, z)
      
      offset <- as.integer(format(first_date, "%u")) - 1L
      n_month <- length(dates_in_month)
      
      title_text <- paste0("Sum listening: ", format(first_date, "%b-%Y"))
      
      
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
          day_label = if_else(!is.na(date), sprintf("%02d", as.integer(format(date, "%d"))), "")
        ) %>%
        select(-idx, -valid, -day_index) %>%
        left_join(month_vals, by = "date") %>%
        mutate(z = coalesce(z, 0))
    }
    
    
    
    
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
        series = list(
          states = list(
            hover = list(
              halo = list(
                size = 0
              )
            ) 
          )
        ),
        heatmap = list(
          borderWidth = 1,
          borderColor = "#e6e6e6",
          dataLabels = list(
            enabled = T,
            useHTML = T,
            align = "left",
            verticalAlign = "top",
            padding= 1,
            style = list(
              fontSize = "9px",
              textOutline= "none",
              color = "#2c3e50",
              opacity = .75
            ),
            formatter = JS("
                          function(){
                            if (this.point && this.point.state === 'hover') return '';
                            return this.point.day_label || '';
                          }")
          )
        )
      ) %>%
      hc_add_series(
        name = if (use_plays) paste(agg, "plays") else paste(agg, "hours"),
        data = pts
      ) %>%
      hc_tooltip(
        useHTML = T,
        outside = T,
        hideDelay =40,
        formatter = JS(sprintf("
                                function () {
                                  var label = this.point.date_str || ('Day ' + (this.point.day_label || ''));
                                  var v = this.point.value;
                                  if (v === null || v === undefined) v = 0;
                                  return '<b>' + label + '</b><br/><b>' + Number(v).toFixed(2) + '%s</b>';
                                }
                              ", suffix))
      )
    
  })
  
  ######################################################
  ####-----------------------text on screen 
  ####################################################
  
  output$text_area<- renderUI({
    df_f <- filtered_df()
    req(nrow(df_f)>0)
    
    hours_in_state <- sum(df_f$hoursPlayed)
    
    state<- page_state()
    header <- if (state =="none"){
      "Default"
    } else if (state=="artist"){
      paste0("Artist selected: ",current_artist())
    } else{
      paste0("Song selected: ", current_song(), " (",current_artist(),")")
    }
    
    
    #time of day weighted by hours
    by_period <- df_f %>%
      mutate(
        period = case_when(
          hour>= 5 & hour <12 ~ "Morning",
          hour>= 21 |hour <5 ~ "Night",
          TRUE ~"Day"
        )
      )%>%
      group_by(period)%>%
      summarise(h = sum(hoursPlayed), .groups="drop")
    
    morning_h <- by_period$h[by_period$period == "Morning"]; if(length(morning_h)==0) morning_h<-0
    night_h <- by_period$h[by_period$period == "Night"]; if(length(night_h)==0) night_h<-0
    day_h <- by_period$h[by_period$period == "Day"]; if(length(day_h)==0) day_h<-0
    
    
    #decide on label
    #mixed when top 2 are close (10%)
    vals<- sort(c(Morning = morning_h,Night=night_h,Day=day_h), decreasing =T)
    primary_time<- if(length(vals)>= 2&& vals[1]>0 &&(vals[1] - vals[2])/vals[1]<=.10){
      "Mixed"
    } else{
      names(vals)[1]
    }
    
    #calander difference from first to last date 
    d_min <-min(df_f$date)
    d_max <-max(df_f$date)
    
    d_start <- as.Date(d_min)
    d_end <- as.Date(d_max)
    
    #full years
    years<- as.integer(format(d_end,"%Y"))-as.integer(format(d_start,"%Y"))
    anniv_year <- as.Date(sprintf("%04d-%02d-%02d",
                                  as.integer(format(d_start,"%Y"))+years,
                                  as.integer(format(d_start,"%m")),
                                  as.integer(format(d_start,"%d"))))
    while(anniv_year > d_end){
      years<-years -1
      anniv_year<- as.Date(sprintf("%04d-%02d-%02d",
                                   as.integer(format(d_start,"%Y"))+years,
                                   as.integer(format(d_start,"%m")),
                                   as.integer(format(d_start,"%d"))))
    }
    #full months after years
    cursor<- anniv_year
    months <- 0L
    repeat{
      next_month<- seq(cursor,by="1 month",length.out=2)[2]
      if(next_month<= d_end){
        months<-months+1L
        cursor<- next_month
      }else break
    }
    
    days <- as.integer(d_end-cursor)
    
    #longest streak 
    listen_days  <- sort(unique(as.Date(df_f$date)))
    
    if(length(listen_days)==0){
      longest_streak<-0L
    } else{
      gaps<-as.integer(diff(listen_days))
      streak_id<- cumsum(c(1L,ifelse(gaps == 1L,0L,1L)))
      streak_lengths<-as.integer(table(streak_id))
      longest_streak<-max(streak_lengths)
    }
    
    #finding most listened to month
    month_summary <- df_f%>%
      mutate(month_key = format(date,"%Y-%m"))%>%
      group_by(month_key)%>%
      summarise(total_h = sum(hoursPlayed), .groups="drop")%>%
      arrange(desc(total_h),month_key)
    top_month_key <- if(nrow(month_summary)>0) month_summary$month_key[[1]] else NA_character_
    top_month_hours<-if(nrow(month_summary)>0) month_summary$total_h[[1]] else 0
    
    top_month_label <- if(!is.na(top_month_key)){
      format(as.Date(paste0(top_month_key,"-01")),"%b %Y")
    } else{
      "N/A"
    }
    
    
    tags$div(
      tags$h4(header),
      tags$p(paste0("Hours Listened: ", round(hours_in_state,2),"h")),
      tags$p(paste0("Primary Listening time of day: ",primary_time)),
      tags$p(paste0(
        "Listening period: ",
        years," Year", if(years==1)"" else "s"," ",
        months," Month",if(months==1)"" else "s"," ",
        days," Day",if(days==1)"" else "s"
      )),
      tags$p(paste0(
        "Longest Listening Streak: ",
        longest_streak, " Day", if(longest_streak==1) "" else "s"
      )),
      tags$p(paste0(
        "Most Listened Month: ",
        top_month_label, " (",round(top_month_hours,2),"h)"
      ))
    )
    
  })
  
  ####################################################################
  ##                     Analytics comparison                       ##
  ####################################################################
  
  #one-time init first time user opens Comparison
  comp_init_done      <- reactiveVal(F)
  comp_song_seed_done <- reactiveVal(F)
  
  # to keep order 
  comp_selected_artists <- reactiveVal(character(0))  
  comp_selected_songs   <- reactiveVal(
    tibble(
      artistName = character(0),
      trackName  = character(0)
    )
  )

  
  selected_date_range <- reactive({
    b <- base_df() 
    req(b)
    
    min_d <- min(b$date)
    max_d <- max(b$date)
    
    r_home <- input$date_range
    
    if(!is.null(r_home) && length(r_home)==2) return(r_home)
    
    c(min_d,max_d)
  })
  

  # first time only: seed top 2 artists by HOURS
  observeEvent(input$analytics_tabs,{
    if( identical(input$analytics_tabs,"Comparison") && !comp_init_done() ){
      df <-base_df() 
      req(df)
      
      # newest month drilled 
      newest_key <- format(max(df$date), "%Y-%m")
      comp_selected_month(newest_key)
      
      top2<- df %>%
        group_by(artistName) %>%
        summarise(playtime = sum(hoursPlayed), .groups="drop") %>%
        arrange(desc(playtime)) %>%
        slice_head(n=2) %>%
        pull(artistName)
      
      comp_selected_artists(top2)
      # keep songs empty until user switches to Song mode
      comp_selected_songs(tibble(artistName=character(0), trackName=character(0)))
      
      comp_song_seed_done(F)
      comp_init_done(T)
    }
  }, ignoreInit = T)
  
  
  
  # table source
  comp_table_df <- reactive({
    df <- base_df() 
    req(df)
    
    dr <- selected_date_range()
    df <- df %>% filter(date >= dr[1], date <= dr[2])
    
    metric<-input$comp_metric %||% "Hours"
    mode <- comp_mode_rv()
    
    if(mode == "Artist"){
      return(
        df %>%
          group_by(artistName) %>%
          summarise(
            playtime = if(metric == "Plays") n() else sum(hoursPlayed),.groups = "drop"
          ) %>%
          arrange(desc(playtime)) %>%
          mutate(rank = row_number()) %>%
          transmute(
            id = paste0("A|",artistName),
            rank = rank,
            artist = artistName,
            playtime = playtime
          )
      )
    }
    
    df %>%
      group_by(artistName, trackName) %>%
      summarise(
        playtime = if(metric=="Plays") n() else sum(hoursPlayed), .groups="drop"
      ) %>%
      arrange(desc(playtime)) %>%
      mutate(rank=row_number()) %>%
      transmute(
        id = paste0("S|",artistName,"|",trackName),
        rank = rank,
        artist = artistName,
        song = trackName,
        playtime = playtime
      )
  })
  
  # DT render (server-side to avoid big client warning)
  output$comp_table <- renderDT({
    df <- comp_table_df()
    req(nrow(df) > 0)
    
    show_song <- identical(comp_mode_rv(), "Song")
    metric    <- input$comp_metric %||% "Hours"
    if (show_song) {
      view <- df %>% select(rank, artist, song, playtime, id)
    } else {
      view <- df %>% select(rank, artist, playtime, id)
    }
    
    if (metric == "Plays") {
      names(view)[names(view) == "playtime"] <- "playcount"
    }
    
    datatable(
      view,
      rownames = F,
      selection = "none",
      options = list(
        pageLength = 10,       # keep 10 visible
        lengthChange = T,
        order = list(list(0,"asc")),
        columnDefs = list(
          list(targets = which(names(view)=="id") - 1, visible = F)
        )
      ),
      callback = JS("
                    function repaintPicked(){
                      var ids = window.compPickedIds || [];
                      table.rows().every(function(){
                        var d = this.data();
                        var n = this.node();
                        if(!d || !n) return;
                        var id = d[d.length - 1];
                        if(ids.indexOf(id) >= 0){
                          $(n).addClass('comp-picked');
                        } else {
                          $(n).removeClass('comp-picked');
                        }
                      });
                    }
              
                    table.on('click', 'tbody tr', function(){
                      var row = table.row(this);
                      var data = row.data();
                      if(!data) return;
                      var id = data[data.length - 1];
                      Shiny.setInputValue('comp_row_click', {id:id, nonce:Date.now()}, {priority:'event'});
                    });
              
                    table.on('draw.dt order.dt search.dt page.dt', repaintPicked);
                    repaintPicked();
                  ")
    ) %>% {
      if(metric == "Plays"){
        formatRound(., "playcount", 0)
      } else{
        formatRound(., "playtime", 2)
      }
    }
  }, server = T)
  
  # row click toggles selected set (multi-select preserved)
  observeEvent(input$comp_row_click,{
    info <- input$comp_row_click
    req(!is.null(info$id))
    
    id <- as.character(info$id)#force a character
    if (!nzchar(id)) return()
    
    mode <- comp_mode_rv()
    
    # artist click
    if (startsWith(id, "A|")) {
      
      if(!identical(mode,"Artist")) return()
      
      a   <- sub("^A\\|", "", id)
      cur <- comp_selected_artists()
    
      if(a %in% cur){
        # stops you from clicking the same artist twice
        return()
      }
      # replace-oldest
      if(length(cur) >= 2){
        cur <- cur[-1]
      }
      comp_selected_artists(c(cur, a))
      return()
    }
    # song click
    if (startsWith(id, "S|")) {
      
      if(!identical(mode,"Song")) return()
      
      parts <- strsplit(id, "\\|")[[1]]
      if(length(parts) < 3) return()
      
      a <- parts[2]
      t <- paste(parts[3:length(parts)], collapse="|")
      
      cur <- comp_selected_songs()
     
      already_picked<-any(cur$artistName == a & cur$trackName == t)
      if(already_picked){
        return()
      }
      
      # add with replace-oldest if needed
      if(nrow(cur) >= 2){
        cur <- cur[-1, , drop=FALSE]
      }
      comp_selected_songs(bind_rows(cur, tibble(artistName=a, trackName=t)))
      return()
    }
  }, ignoreInit = T)
  
  #first click on song / based on toggle metric 
  observeEvent(comp_mode_rv(),{
    
    if(!identical(comp_mode_rv(),"Song")) return()
    if(isTRUE(comp_song_seed_done()))     return()
    
    df <- base_df()
    req(df)
    
    
    metric <- input$comp_metric %||% "Hours"
    
    
    top2songs <- df%>%
      group_by(artistName, trackName) %>%
      summarise(
        playtime = if(metric=="Plays") n() else sum(hoursPlayed),.groups="drop") %>%
      arrange(desc(playtime)) %>%
      slice_head(n=2)
    
    if(nrow(top2songs) < 2){
      comp_selected_songs(top2songs %>% transmute(artistName, trackName))
    } else{
      comp_selected_songs(top2songs %>% transmute(artistName, trackName))
    }
    
    comp_song_seed_done(T)
    
  }, ignoreInit = T)
  
  
  
  
  
  # add blue highlight class after every table draw using ids
  observe({
    sel_ids <- c(
      paste0("A|", comp_selected_artists()),
      if(nrow(comp_selected_songs())>0)
        paste0("S|", comp_selected_songs()$artistName, "|", comp_selected_songs()$trackName)
      else character(0)
    )
    
    session$sendCustomMessage("comp_sel_ids", sel_ids)
  })
  
  #buttons 
  
  set_comp_mode_btn <- function(mode){
    runjs(sprintf("
                  $('#comp_btn_artist').removeClass('is-active');
                  $('#comp_btn_song').removeClass('is-active');
                  if('%s'=='Artist'){ $('#comp_btn_artist').addClass('is-active'); }
                  if('%s'=='Song')  { $('#comp_btn_song').addClass('is-active'); }
                ", mode, mode))
  }
  
  observe({
    set_comp_mode_btn(comp_mode_rv())
  })
  
  observeEvent(input$comp_btn_artist,{
    comp_mode_rv("Artist")
  },ignoreInit=T)
  
  observeEvent(input$comp_btn_song,{
    comp_mode_rv("Song")
  }, ignoreInit = T)
  
  #month
  observeEvent(input$comp_month_pick,{
    info <- input$comp_month_pick
    req(!is.null(info$key))
    comp_selected_month(as.character(info$key))
  }, ignoreInit = T)
  
  
  
  
  ####################################################################
  ##                Analytics comparison Graphs                     ##
  ####################################################################
  
  
  output$comp_month_bar <- renderHighchart({
    
    df <- base_df() 
    req(df)
    metric <- input$comp_metric %||% "Hours"
    mode <- comp_mode_rv()
    
    #newest month
    if(is.null(comp_selected_month())){
      comp_selected_month(format(max(df$date), "%Y-%m"))
    }
    
    df <- df %>%mutate(ym = format(date, "%Y-%m"))
    
    
    month_keys <- sort(unique(df$ym))
    month_labels <- format(as.Date(paste0(month_keys,"-01")), "%b %Y")
    
    #build stacked values depending on mode and selected metric
    if(identical(mode,"Artist")){
      
      sel<- comp_selected_artists()
      req(length(sel)== 2)   # enforce exactly 2 to show
      
      m <- df%>%
        filter(artistName%in%sel)%>%
        group_by(ym,artistName)%>%
        summarise(
          y = if(metric=="Plays") n() else sum(hoursPlayed),
          .groups="drop"
        ) %>%
        complete(ym = month_keys, artistName = sel, fill = list(y=0))%>%
        arrange(match(ym, month_keys), match(artistName, sel))
      
      s1 <- m %>%filter(artistName ==sel[1])%>%pull(y)
      s2 <- m %>%filter(artistName ==sel[2])%>%pull(y)
      
      series <- list(
        list(name =sel[1] ,data = as.list(s1),type="column"),
        list(name =sel[2] ,data = as.list(s2),type="column")
      )
      
    } else {
      
      sel <- comp_selected_songs()
      req(nrow(sel) == 2)
      
      #stable naming
      n1 <- paste0(sel$trackName[[1]]," (",sel$artistName[[1]],")")
      n2 <- paste0(sel$trackName[[2]]," (",sel$artistName[[2]],")")
      
      
      m <- df %>%
        inner_join(sel, by=c("artistName","trackName"))%>%
        group_by(ym, artistName, trackName) %>%
        summarise(
          y = if(metric=="Plays") n() else sum(hoursPlayed),.groups="drop") %>%
        mutate(series_id = paste0(trackName,"|||",artistName))%>%
        complete(
          ym = month_keys,
          series_id = paste0(sel$trackName,"|||",sel$artistName),
          fill = list(y=0)
        ) %>%
        arrange(match(ym, month_keys))
      
      
      id1 <- paste0(sel$trackName[[1]],"|||",sel$artistName[[1]])
      id2 <- paste0(sel$trackName[[2]],"|||",sel$artistName[[2]])
      
      s1 <- m %>%filter(series_id == id1) %>%pull(y)
      s2 <- m %>%filter(series_id == id2) %>%pull(y)
      
      
      series <- list(
        list(name = n1, data = as.list(s1), type="column"),
        list(name = n2, data = as.list(s2), type="column")
      )
    }
    
    month_keys_json <- toJSON(month_keys, auto_unbox = T)
    
    highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text = "Monthly comparison (Clickable)") %>%
      hc_xAxis(categories = month_labels) %>%
      hc_yAxis(
        title = list(text = if(metric=="Plays") "Plays" else "Hours"),
        stackLabels = list(enabled = F)
      ) %>%
      hc_plotOptions(
        column = list(stacking = "normal"),
        series = list(
          
          # click on segment drills to that month
          point = list(
            events = list(
              click = JS(sprintf("
                                  function(){
                                    var monthKeys = %s;
                                    var key = monthKeys[this.x];
                                    Shiny.setInputValue('comp_month_pick', {key:key, nonce:Date.now()}, {priority:'event'});
                                  }
                                ", month_keys_json))
            )
          ),
          
          # legend click dims other
          events = list(
            legendItemClick = JS("
                                  function(e){
                                    var chart = this.chart;
                                    var clicked = this;
                                    var already = chart._dimSeriesName || null;
                      
                                    if(already && already === clicked.name){
                                      // reset
                                      chart.series.forEach(function(s){
                                        if(s.group) s.group.attr({ opacity: 1 });
                                        if(s.dataLabelsGroup) s.dataLabelsGroup.attr({ opacity: 1 });
                                      });
                                      chart._dimSeriesName = null;
                                      return false;
                                    }
                      
                                    chart.series.forEach(function(s){
                                      var op = (s.name === clicked.name) ? 1 : 0.2;
                                      if(s.group) s.group.attr({ opacity: op });
                                      if(s.dataLabelsGroup) s.dataLabelsGroup.attr({ opacity: op });
                                    });
                                    chart._dimSeriesName = clicked.name;
                                    return false;
                                  }
                                ")
          )
        )
      ) %>%
      hc_tooltip(shared =T) %>%
      hc_add_series_list(series)
    
  })
  
  
  output$comp_month_line <- renderHighchart({
    
    df <- base_df()
    req(df)
    
    metric <- input$comp_metric%||% "Hours"
    mode <- comp_mode_rv()
    
    #newest month 
    mkey <-comp_selected_month()
    if(is.null(mkey) || !nzchar(mkey)){
      mkey <- format(max(df$date), "%Y-%m")
      comp_selected_month(mkey)
    }
    
    
    first_date <- as.Date(paste0(mkey,"-01"))
    last_date <- seq(first_date, by="1 month", length.out=2)[2] - 1
    month_dates <- seq(first_date, last_date, by="day")
    
    df_m <- df %>%
      filter(date >= first_date, date <= last_date)
    
    make_ts <- function(dates, vals){
      m <- tibble(date = month_dates) %>%
        left_join(tibble(date=dates, y=vals), by="date")%>%
        arrange(date)
      
      Map(function(d,y){
        list(datetime_to_timestamp(as.POSIXct(d)), if(is.na(y)) 0 else y)
      }, m$date, m$y)
    }
    
    
    series <- list()
    
    
    if(identical(mode,"Artist")){
      sel <- comp_selected_artists()
      req(length(sel) == 2)
      
      for(a in sel){
        tmp <- df_m %>%
          filter(artistName == a) %>%
          group_by(date) %>%
          summarise(
            y = if(metric=="Plays") n() else sum(hoursPlayed),.groups="drop")
        series[[length(series)+1]] <- list(
          name = a,
          type = "line",
          data = make_ts(tmp$date, tmp$y)
        )
      }
    } else {
      sel <- comp_selected_songs()
      req(nrow(sel)== 2)
      
      for(i in 1:2){
        a <- sel$artistName[[i]]
        t <- sel$trackName[[i]]
        
        tmp <- df_m %>%
          filter(artistName==a, trackName==t)%>%
          group_by(date)%>%
          summarise(
            y = if(metric=="Plays") n() else sum(hoursPlayed),.groups="drop"
          )
        
        series[[length(series)+1]] <- list(
          name = paste0(t," (",a,")"),
          type = "line",
          data = make_ts(tmp$date, tmp$y)
        )
      }
    }
    
    highchart() %>%
      hc_chart(type="line")%>%
      hc_title(text = paste0("Daily drilldown: ", format(first_date, "%b %Y")))%>%
      hc_xAxis(type="datetime")%>%
      hc_yAxis(title = list(text = if(metric=="Plays") "Plays" else "Hours"))%>%
      hc_tooltip(shared = T, valueDecimals = if(metric=="Plays") 0 else 2)%>%
      hc_add_series_list(series)
    
  })
  
  ####################################################################
  ##                Analytics comparison text area                  ##
  ####################################################################
  comp_df_filtered <- reactive({
    df<-base_df()
    req(df)
    
    dr <- selected_date_range()
    df <- df%>% filter(date>= dr[1],date<=dr[2])
    
    #follows the graph above
    mkey <- comp_selected_month()
    if (!is.null(mkey) && nzchar(mkey)) {
      df <- df %>% filter(format(date, "%Y-%m") == mkey)
    }
    
    df
  })
  
  #setting oldest to newest
  comp_selected_pair <- reactive({
    mode <- comp_mode_rv()
    
    if(identical(mode,"Artist")){
      cur <- comp_selected_artists()
      if(length(cur)<2)return(NULL)
      
      tibble(
        type = "Artist",
        artistName = cur,
        trackName = NA_character_
      )
    } else{
      cur <- comp_selected_songs()
      if(nrow(cur)<2) return(NULL)
      
      cur%>%
        mutate(type="Song")%>%
        select(type,artistName,trackName)
    }
  })
  
  #adding stats
  compute_comp_entity_stats<- function(df,type,artistName,trackName =NA_character_){
    if(identical(type,"Artist")){
      d<- df%>% filter(artistName== !!artistName)
      header <- paste0("Artist :", artistName)
      
    }else{
      d<- df%>% filter(artistName==!!artistName,trackName==!!trackName)
      header <- paste0("Song: ",trackName," (",artistName,")")
    
    }
    
    if(nrow(d)==0){
      return(list(
        header = header,
        total_plays = 0L,
        total_minutes = 0,
        avg_per_day_m = 0,
        active_days = 0L,
        active_days_total = 0L,
        primary_time = "N/A",
        peak_hour_label = "N/A",
        weekend_share = 0,
        skip_rate = 0,
        longest_streak = 0L,
        best_day_label = "N/A",
        best_day_minutes = 0
      ))
    }
    
    #total plays and minutes
    total_plays <- nrow(d)
    total_minutes <- sum(d$hoursPlayed) * 60
    
    
    #average amount of time perday
    by_day <- d%>%
      group_by(date)%>%
      summarise(m=sum(hoursPlayed)*60,.groups="drop")
    
    avg_per_day_m<- mean(by_day$m)
    
    #active days vs period
    active_days <- n_distinct(d$date)
    active_days_total <- n_distinct(df$date)
    
    
    #mostly day or night listener 
    by_period <- d%>%
      mutate(
        period = case_when(
          hour >= 21 | hour < 5 ~ "Night",
          TRUE ~ "Day"
        )
      )%>%
      group_by(period)%>%
      summarise(h=sum(hoursPlayed),.groups = "drop")
    
    day_h <- by_period$h[by_period$period=="Day"]; if(length(day_h)==0) day_h <-0
    night_h <- by_period$h[by_period$period=="Night"]; if(length(night_h)==0) night_h <-0
    
    primary_time <- if(day_h == 0 && night_h == 0){
      "N/A"
    } else if (abs(day_h-night_h) /max(day_h,night_h)<= .1){
      "Mixed"
    } else if (day_h >= night_h){
      "Day"
    } else{
      "Night"
    }
    
    #peak hour
    peak_hour <- d %>%
      group_by(hour) %>%
      summarise(h = sum(hoursPlayed), .groups = "drop") %>%
      arrange(desc(h), hour) %>%
      slice_head(n = 1) %>%
      pull(hour)
    
    peak_hour_label <- if(length(peak_hour) == 0) "N/A" else sprintf("%02d:00", peak_hour[[1]])
    
    #weekend share
    wk <- d %>%
      mutate(is_weekend = weekdays(date) %in% c("Saturday", "Sunday")) %>%
      summarise(
        w = sum(hoursPlayed[is_weekend]),
        t = sum(hoursPlayed)
      )
    
    weekend_share <- ifelse(wk$t[[1]] > 0, 100 * wk$w[[1]] / wk$t[[1]], 0)
    
    #skip rate
    skip_rate <- 100 * mean(d$msPlayed < 30000, na.rm = TRUE)

    
    #longest streak
    listen_days <- sort(unique(as.Date(d$date)))
    if(length(listen_days) == 0){
      longest_streak <- 0L
    } else{
      gaps <- as.integer(diff(listen_days))
      streak_id <- cumsum(c(1L, ifelse(gaps==1L, 0L, 1L)))
      streak_lengths <- as.integer(table(streak_id))
      longest_streak <- max(streak_lengths)
    }

    i_best <- which.max(by_day$m)
    best_day_label   <- format(by_day$date[[i_best]], "%d %b %Y")
    best_day_minutes <- by_day$m[[i_best]]
    
    
    list(
      header = header,
      total_plays = total_plays,
      total_minutes = total_minutes,
      avg_per_day_m = avg_per_day_m,
      active_days = active_days,
      active_days_total = active_days_total,
      primary_time = primary_time,
      peak_hour_label = peak_hour_label,
      weekend_share = weekend_share,
      skip_rate = skip_rate,
      longest_streak = longest_streak,
      best_day_label = best_day_label,
      best_day_minutes= best_day_minutes
    )
  }
    
  comp_entity_stats_a <- reactive({
    df <- comp_df_filtered()
    sel <- comp_selected_pair()
    req(!is.null(sel), nrow(sel) >= 2)
    
    newest <- sel[2, , drop=F]
    
    compute_comp_entity_stats(
      df = df,
      type = newest$type[[1]],
      artistName = newest$artistName[[1]],
      trackName = newest$trackName[[1]]
    )
  })
  
  comp_entity_stats_b <- reactive({
    df <- comp_df_filtered()
    sel <- comp_selected_pair()
    req(!is.null(sel), nrow(sel) >= 2)
    
    oldest <- sel[1, , drop=F]
  
    compute_comp_entity_stats(
      df = df,
      type = oldest$type[[1]],
      artistName = oldest$artistName[[1]],
      trackName = oldest$trackName[[1]]
    )
  })
  
  output$comp_text_area_a <- renderUI({
    s <- comp_entity_stats_a()
    tags$div(
      tags$h4(s$header),
      tags$p( 
        paste0(
          "Month: ",
          ifelse(is.null(comp_selected_month()) || !nzchar(comp_selected_month()),
                 "N/A",
                 format(as.Date(paste0(comp_selected_month(), "-01")), "%b %Y"))
        )),
      tags$p(paste0("Total plays: ", s$total_plays)),
      tags$p(paste0("Total minutes: ", round(s$total_minutes, 1), " min")),
      tags$p(paste0("Avg time per day: ", round(s$avg_per_day_m, 2), "min")),
      tags$p(paste0("Active days: ", s$active_days, "/", s$active_days_total)),
      tags$p(paste0("Skip proxy (<30s): ", round(s$skip_rate, 1), "%")),
      tags$p(paste0("Best day: ", s$best_day_label, " (", round(s$best_day_minutes, 1), " min)")),
      tags$p(paste0("Mostly listened: ", s$primary_time)),
      tags$p(paste0("Weekend share: ", round(s$weekend_share, 1), "%")),
      tags$p(paste0("Longest streak: ", s$longest_streak, " day", if(s$longest_streak==1) "" else "s")),
      tags$p(paste0("Peak hour: ", s$peak_hour_label))
      
      )
  })
  
  output$comp_text_area_b <- renderUI({
    s <- comp_entity_stats_b()
    tags$div(
      tags$h4(s$header),
      tags$p( 
        paste0(
        "Month: ",
        ifelse(is.null(comp_selected_month()) || !nzchar(comp_selected_month()),
               "N/A",
               format(as.Date(paste0(comp_selected_month(), "-01")), "%b %Y"))
      )),
      tags$p(paste0("Total plays: ", s$total_plays)),
      tags$p(paste0("Total minutes: ", round(s$total_minutes, 1), " min")),
      tags$p(paste0("Avg time per day: ", round(s$avg_per_day_m, 2), "min")),
      tags$p(paste0("Active days: ", s$active_days, "/", s$active_days_total)),
      tags$p(paste0("Skip proxy (<30s): ", round(s$skip_rate, 1), "%")),
      tags$p(paste0("Best day: ", s$best_day_label, " (", round(s$best_day_minutes, 1), " min)")),
      tags$p(paste0("Mostly listened: ", s$primary_time)),
      tags$p(paste0("Weekend share: ", round(s$weekend_share, 1), "%")),
      tags$p(paste0("Longest streak: ", s$longest_streak, " day", if(s$longest_streak==1) "" else "s")),
      tags$p(paste0("Peak hour: ", s$peak_hour_label))
      
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)