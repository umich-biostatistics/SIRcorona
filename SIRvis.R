
library(shiny)
library(png)
library(stringr)
library(tidyverse)
library(exdata)

ui = fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 3px solid #000000;}"))
  ),
  
  fluidRow(
    headerPanel(h3("Explore eSIR Corona Virus results", style="color: #02169B; font-weight: bold;")),
    div(style = "height:72px; background-color: #F1F1F1;"),
    wellPanel(
      helpText(h4("State-space SIR models with time-varying quarantine protocols"))
    ),
  ),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Use the drop-down menus to view model results by day, location, and model.'),
      selectInput('day', 'Select day', c("2020-03-05")),
      checkboxGroupInput('calib', 'With or without calibration?', 
                         choices = c('With calibration' = 'w_calib', 'Without calibration' = 'wout_calib'),
                         selected = 'wout_calib'),
      selectInput('location', 'Choose location', c('Hubei', 'Outside Hubei')),
      selectInput('func', 'Choose function', c('No intervention', 'Time-varying transmission rate, exponential',
                                               'Time-varying transmission rate, step', 'Time-varying quarantine, Dirac delta')),
      submitButton("Update View", icon("refresh"))
    ),
    mainPanel(
      hr(),
      p(h3(strong('Infected'))),
      imageOutput('infected', width = 900, height = 800),
      hr(),
      p(h3(strong('Removed'))),
      imageOutput('removed', width = 900, height = 800),
      hr(),
      p(h3(strong('Spaghetti'))),
      imageOutput('spaghetti', width = 900, height = 800)
    )
  )
)

server = function(input, output, session) {
  
  URL = 'ftp://xfer1.bio.sph.umich.edu/ncov2019/'
  
  linkfile = tempfile(fileext='.csv')
  download.file(url = paste0(URL, 'link.csv'), destfile = linkfile, mode = 'wb')
  link = read_csv(linkfile) 
  
  observe({
    req(link)
    daychoices = link %>% pull(Date) %>% unique()
    updateSelectInput(session, 'day', 
                      choices = daychoices,
                      selected = daychoices[1])
  })
  
  observe({
    req(link)
    locationchoices = link %>% pull(Location) %>% unique()
    updateSelectInput(session, 'location', 
                      choices = locationchoices,
                      selected = locationchoices[1])
  })
  
  observe({
    req(link)
    funcchoices = link %>% pull(Function) %>% unique()
    updateSelectInput(session, 'func', 
                      choices = funcchoices,
                      selected = funcchoices[1])
  })
  
  files = reactive({
    date = input$day
    location = input$location
    calibrate = input$calib
    func = input$func
    dat = NULL
    if(!is.null(calibrate)) {
      calibrate = ifelse(input$calib == 'w_calib', 'with', 'without')
      print(calibrate)
      dat = link %>% filter(Date == date & Location == location & Function == func & Calibration == calibrate) %>% pull(File)
    } else {
      dat = link %>% filter(Date == date & Location == location & Function == func) %>% pull(File)
    }
    
    return(dat)
  })
  
  output$files = renderText({files()})
  
  output$infected = renderImage({
    fls = files()
    fls = fls[str_detect(string = fls, pattern = fixed('forecast.'))]
    calibrate = ifelse(input$calib == 'wout_calib', 'without_calibration', 'with_calibration')
    middle_path = paste(paste(input$day, calibrate, sep = '/'), '', sep = '/')
    fls = paste0('ftp://xfer1.bio.sph.umich.edu/ncov2019/', middle_path, fls)
    outfile = tempfile(fileext='.png')
    download.file(url = fls, destfile = outfile, mode = 'wb')
    print(outfile)
    print(fls)
    list(src = outfile,
         alt = "Plot not found",
         width = 900)
  }, deleteFile = TRUE)
  
  output$removed = renderImage({
    fls = files()
    fls = fls[str_detect(string = fls, pattern = fixed('forecast2.'))]
    calibrate = ifelse(input$calib == 'wout_calib', 'without_calibration', 'with_calibration')
    middle_path = paste(paste(input$day, calibrate, sep = '/'), '', sep = '/')
    fls = paste0('ftp://xfer1.bio.sph.umich.edu/ncov2019/', middle_path, fls)
    outfile = tempfile(fileext='.png')
    download.file(url = fls, destfile = outfile, mode = 'wb')
    print(outfile)
    print(fls)
    list(src = outfile,
         alt = "Plot not found",
         width = 900)
  }, deleteFile = TRUE)
  
  output$spaghetti = renderImage({
    fls = files()
    fls = fls[str_detect(string = fls, pattern = fixed('spaghetti.'))]
    calibrate = ifelse(input$calib == 'wout_calib', 'without_calibration', 'with_calibration')
    middle_path = paste(paste(input$day, calibrate, sep = '/'), '', sep = '/')
    fls = paste0('ftp://xfer1.bio.sph.umich.edu/ncov2019/', middle_path, fls)
    outfile = tempfile(fileext='.png')
    download.file(url = fls, destfile = outfile, mode = 'wb')
    print(outfile)
    print(fls)
    list(src = outfile,
         alt = "Plot not found",
         width = 900)
  }, deleteFile = TRUE)
  
}

shinyApp(ui, server)
