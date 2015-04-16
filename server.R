shinyServer(function(input, output) {
  
#  rp.data <- reactive({
#            if(input$dataset == "rock") {return(rock)}
#            if(input$dataset == "pressure") {return(pressure)}
#            if(input$dataset == "cars") {return(mtcars)}
#           "rock" = rock,
#           "pressure" = pressure,
#           "cars" = mtcars)
#  })
  

  
#  output$selectDV <- renderUI({selectInput("DV", "Select Dependent Variable", names(rp.data()))})
  
#  output$selectPreds <- renderUI({checkboxGroupInput("preds", "Select Predictors", 
#                                                     names(rp.data())[!names(rp.data()) %in% paste(input$DV)])})
  
  output$selectDV <- renderUI({selectInput("DV", "Select Dependent Variable", names(mtcars))})
  
  output$selectPreds <- renderUI({checkboxGroupInput("preds", "Select Predictors", 
                                                       names(mtcars)[!names(mtcars) %in% paste(input$DV)], 
                                                       selected = names(mtcars)[!names(mtcars) %in% paste(input$DV)], inline = T)})
  
  
  
  
  #set up a little rpart as an example
  rp <- reactive ({
#    print(length(input$preds))
#    print(input$preds[2])
  #  print(str(input$preds))
  #  split <- strsplit(input$preds)
  #  print(unlist(split))
  # print(paste(unlist(split), sep = "+"))
    split <- input$minsplit
    dv <- paste(input$DV)
    if(length(input$preds) > 1) {
      preds <- paste(input$preds[1])
      for(i in 2:length(input$preds)) {         
        iv <- paste(" + ", input$preds[i], sep = "")
        preds <- paste(preds, iv, sep = "")}} else {
    preds <- paste(input$preds[1])}
    form <- print(paste(dv, preds, sep = " ~ "), quote = F)
    rpart(form, data = mtcars,
    method = "anova",
    control = rpart.control(minsplit = split))
  })

output$test <- renderTable(rp()$frame)  
  
rpk <- reactive({print("rpk")
  as.party(rp())})


rpk.text <- reactive({print("rpk.text")
  (rpk.text <- capture.output( print(rpk()) ) %>>%
                               ( .[grep( x = ., pattern = "(\\[)([0-9]*)(\\])")] ) %>>%
                               strsplit( "[\\[\\|\\]]" , perl = T) %>>%
                               list.map(
                                 tail(.,2) %>>%
                                   (
                                     data.frame(
                                       "id" = as.numeric(.[1])
                                       , description = .[2]
                                       , stringsAsFactors = F )
                                   )
                               ) %>>% list.stack)
  #root <- row
  rpk.text <- rpk.text[!duplicated(rpk.text$id, fromLast = T),]
  print(rpk.text)
  print(rp()$frame)
  print("rpk.text2")
                             rpk.text <- cbind(rpk.text, rp()$frame)
                             
                             # rounding the mean DV value
                             rpk.text$yval <- round(rpk.text$yval, 2)
                             
                             # terminal nodes have descriptive stats in their names, so I stripped these out
                             # so the final plot wouldn't have duplicate data
                             rpk.text$description <- sapply(strsplit(rpk.text[,2], ":"), "[", 1)
                             rpk.text
                             })
output$rpk.text <- renderTable(rpk.text())

output$plot <- renderChart2({
  rChartsRpart <- setRefClass(
    "rChartsRpart",
    contains = "Dimple",
    methods = list(
      initialize = function(){
        callSuper();
      },
      getPayload = function (chartId) {
        data = jsonlite::toJSON(
          rapply(params$data$rpk$node,unclass,how="replace")
          ,
          auto_unbox = T
        )
        data = gsub( x=data, pattern = "kids", replacement="children")
        data = gsub ( x=data, pattern = '"id":([0-9]*)', replacement = '"name":"node\\1"' )
        # calling the root node by the dataset name, but it might make more sense to call it
        # "root" so that the code can be generalized
        # data = sub (x = data, pattern = "node1", replacement = "mtcars")
        # replacing the node names from node1, node2, etc., with the extracted node names and metadata from
        # rpk.text, and rp$table. 
        for (i in 1:nrow(params$data$rpk.text)) {
          data = sub (x = data, pattern = paste("node", i, sep = ""), 
                      replacement = paste(params$data$rpk.text[i,2])
                                          #,", mean = ", params$data$rpk.text[i,7], ", n = ", params$data$rpk.text[i,4], sep = ""), fixed = T
                      )}
        chart = toChain(params$chart, "myChart")
        controls_json = toJSON(params$controls)
        controls = setNames(params$controls, NULL)
        opts = toJSON2(params[!(names(params) %in% c("data", "chart",
                                                     "controls"))])
        list(opts = opts, data = data, chart = chart, chartId = chartId,
             controls = controls, controls_json = controls_json)
      }
    )
  )
  rpRc <- rChartsRpart$new()
  #rpRc$setLib(".") 
  rpRc$setLib("http://timelyportfolio.github.io/rCharts_rpart")
  rpRc$lib = "rpart_tree"
  rpRc$LIB$name = "rpart_tree"
  #rpRc$setTemplate(
  #  chartDiv = "<{{container}} id = '{{ chartId }}' class = '{{ lib }}' style = 'height:100%;width:100%;'></{{ container}}>"
  #)
  rpRc$set(
    data = list(rpk = rpk(), rpk.text = rpk.text())
    , height = 700
    , width = 1200
    , nodeHeight = 180
    , maxLabelLength = 15
  )
  return(rpRc)})

})
    
  