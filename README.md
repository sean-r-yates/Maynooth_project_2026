# StatSight.fm <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Shiny_hex_logo.svg/1200px-Shiny_hex_logo.svg.png" align="right" width=120 height=139 alt="Shiny Logo" />

## Goal  
Build an interactive **R Shiny** application that helps users explore and better understand their Spotify listening data through clear visuals, filtering tools, and insightful stats.

Click [here](https://statsight-fm.shinyapps.io/Statsight-fm/) for a demo of the app.

---

## Inspiration  
We’ve always felt that spotify wrapped could be squeezed for more data,<br>
so we wanted to build something that gives deeper more insightful dive into Spotify data.  
This project was created as our submission for the **2025/2026 Maynooth Data Science Shiny App Development Competition**.

---

## Features  
- Upload Spotify data export (`.zip` or required `.json` files)
- Interactive analytics dashboard
  - Analytics Home Page:
    - Drill-down views by:
      - Reset/default/user will produce stats about the user at highest level
      - Artist will produce stats about a specific artist
      - Song will produce stats about a specfic song
    - Date range controls and dynamic filtering
    - Listening pattern visualizations:
      - Weekly heatmap sum/mean
      - Monthly heatmap sum/mean
      - gauge charts
      - Line Chart
  - Analytics Comparison Page:
    - Comarison visualizations:
      - Stacked monthly barchart
      - two veriable line chart
         

---
## Data Files Used  
The app expects Spotify export files in this format or simply a zip file:

- `StreamingHistory_music_{0-N}.json`
- `Marquee.json`

### Key fields
**StreamingHistory_music files**
- `endTime` – when playback ended
- `artistName` – artist name
- `trackName` – song name
- `msPlayed` – milliseconds played per stream


**Marquee file**
not actually used but required to run
- `artistName` – artist name
- `segment` – listener segment label

---
## Known Issues/Errors
- sometimes analytics comparison song stats will be 0 but that is because by default we pick the newest month, so if user hasn't listenned to anything that month itll be 0's

  
## Improvements since Deadline
- NA



### Resources used in development
##### Videos 
- https://www.youtube.com/watch?v=emAemGzma7o
- https://youtu.be/u7JN2hyH1CU
- https://youtu.be/u7JN2hyH1CU
- https://youtu.be/41jmGq7ALMY
- https://youtu.be/GesK8W9xSPU
- https://youtu.be/B83lZVXGjqM
- https://youtu.be/JUop-YfRAuw?si=UwEn3QQ_Hyewv54i
- https://youtu.be/789ZcPHlg7w
- https://youtu.be/q4O6bRL1f4w
- https://youtu.be/KrcoQY8AQy0

##### Articals/websites 
- https://www.quantargo.com/help/r/latest/packages/DT/0.18
- https://www.geeksforgeeks.org/r-language/data-visualization-with-highcharter-in-r/
- https://rstudio.github.io/DT/
- https://r-graph-gallery.com/package/dt.html
- https://www.datacamp.com/tutorial/data-visualization-highcharter-r
- https://www.highcharts.com/blog/integration/highcharts-for-r-users/
- https://medium.com/@amitmangal2203/data-visualization-with-highcharter-in-r-programming-2453ae4b30ad
- https://www.geeksforgeeks.org/r-language/working-with-json-files-in-r-programming/

and https://chatgpt.com for trouble shooting



