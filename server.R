library(shiny)
library(Zelig)

cats<-as.data.frame(matrix(c(.001, "very very low", .01, "very low", .1, "low", .2, "moderate", .3, "high", .4, "very high"), byrow = T, ncol = 2))
cats$V1<-as.numeric(as.character(cats$V1))
if(!exists("resultss")) {print("resetting resultss")
                         resultss<-""}


shinyServer(function(input, output) {
  
#   updateqs<-reactive({
#     ps<-input$mydata
#     print(ps)
#     ps<-paste("hi hi hi", ps)
# #    ps$waited<-c(input$q1, input$q2, input$q3, input$q4, input$q5, input$q6, input$q7, input$q8, input$q9, input$q10)
#     output$results<-renderText({ps})
# })
  output$distPlot <- renderPlot({
      print(resultss)
      ps<-input$mydata
#    output$results<-renderText({ps})
    print(ps)
    toks<-"cats"
    group<-"very very low discounter"
    if(is.null(ps)) {
            res<-"waiting"
            pqs<-NA
            kparam<-"waiting"
            interp<-"Answer at least 3 questions, then submit to see your results."        
            resultss<-"Answer at least 3 questions, then submit to see your results."            
    }
    else {
      pqs<-unserializeJSON(ps)
      pqs$waited<-as.numeric(c(input$q1, input$q2, input$q3, input$q4, input$q5, input$q6, input$q7, input$q8, input$q9, input$q10))
      global.pqs<<-pqs[pqs$waited!=-1,]
      print(global.pqs)
      res<-zelig(waited~0+rparam+d, model="logit", data=global.pqs)
      print(res$result)
      kparam<-res$result$coefficients[2]/res$result$coefficients[1]
      toks<-pqs$token[1]
      for(i in 1:nrow(cats)) {
          th<-cats$V1[i]
          if(kparam>=th) { group<-cats$V2[i] }
      }
      interp<-paste0(input$results, "<div style='font-size: 1.5em; background-color: lightgreen; padding: 20px; margin: 10px; border: 1px solid blue; line-height: 120%'>When it comes to ", toks, " you are a ", group, " discounter. (<I>k</I> value ", round(kparam, 4), ")</div>") 
        resultss<-paste0(resultss, interp)
    }
    print(interp)
    print(resultss)
    output$results<-renderText(resultss)
#     x    <- faithful[, 2]  # Old Faithful Geyser data
#     bins <- seq(min(x), max(x), length.out = as.numeric(20) + 1)
#     
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
#   output$text1<-renderText({"this is simple text from output"})
#   output$text2<-renderText({input$q1})
#   output$text3<-renderText(pqs[1,2])
#   ns<<-pqs
#   print(pqs)
#   print(ns)
})
