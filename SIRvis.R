
library(shiny)
library(png)

ui = fluidPage(
  titlePanel('State-space SIR model with Time-varying quarantine protocols'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('model', 'Choose model', c('Model 1', 'Model 2')),
      selectInput('func', 'Choose function', c('exp', 'step', 'qhSIR')),
      selectInput('location', 'Choose location', c('HUBEI')),
      selectInput('day', 'Select day', c('2020-02-26', '2020-02-27'))
    ),
    mainPanel(
      imageOutput('infected'),
      imageOutput('removed'),
      imageOutput('spaghetti')
    )
  )
)

server = function(input, output) {
  drec = 'data/2020-02-26/'
  dirpng = dir(drec)
  pngs = dirpng[str_which(dir('data/2020-02-26/'), ".png")]
  pngs_spl = str_split(pngs, pattern = '_')
  
  soptions = reactive({
    model = input$model
    func = input$func
    location = input$location
    day = input$day
    
    png_list =
      lapply(pngs_spl, function(char) {
        if ((day == char[1]) & (location == char[2]) & (func == char[3])) {
          picture_path = paste(drec, paste(char, collapse = '_'), sep = '')
          picture = readPNG(picture_path)
          type = str_sub(char[5], end = -5)
          return(c('picture' = picture, 'type' = type))
        }
      })
    
    return(png_list)
  })
  
  output$infected = renderImage({
    pics = soptions()
    
    return(png_list['picture'][str_which(pics[['type']], 'forecast')][1])
  })
  
  output$removed = renderImage({
    pics = soptions()
    
    return(png_list['picture'][str_which(pics[['type']], 'forecast2')][1])
  })
  
  output$spaghetti = renderImage({
    pics = soptions()
    
    return(png_list['picture'][str_which(pics[['type']], 'spaghetti')][1])
  })
}

shinyApp(ui, server)
