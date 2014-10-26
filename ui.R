library(shiny)
library(jsonlite)

excuses<-"<p>This is really primitive! Here's what I wish I'd had time/expertise to do:<ul>
<li>Learn more about embedding shiny objects in plain HTML, so the layout could be a little less cramped and primitive
</li><li>Generate more questions, so the estimates aren't so wonky
</li><li>Allow users to see a graph of their discounting ratio over different delays
</li><li>Save results across multiple tests</li></ul>"

instructions<-"<p>Delay Discounting refers to the tendency to value something in the future less than the same thing in the present. For example, most of us would rather have $100 than $95, but if we had to wait a year to get the $100 and could have the $95 right now, we'd take it. Some people are very high discounters and others are low discounters. Additionally, people tend to discount money less than almost any other good (that is, they're more willing to wait on money than on other things). </p>

<p>Low discounting basically means valuing the future -- saving for retirement instead of having fun now, staying in school instead of dropping out and getting a bad job, or waiting for your coffee to cool down instead of drinking it right away and burning your mouth. Yet, some degree of discounting is rational:<ul><li>Money you have now can be invested and earn value</li><li>The future is uncertain. You might choose to wait for a later reward and then get hit by a bus the next day. Or, the person who was going to give you the reward might go back on their word.</li><li>If you have immediate needs (you're starving, you're injured and can't pay for medical care), then you need resources right now, even if the opportunity cost is very high.</ul></p>

<p>People differ in how much they discount the future. The delay discounting task asks people to choose between a current reward and a future reward, and uses these responses to infer a general 'discounting level,' <I>k</I>. k has no direct interpretation, but you can compare responses between people, situations, or goods. Higher values of k mean more discounting and lower values mean less. This page computes k using logistic regression following the method of Wylieto et al (2004). </p>

<p>The estimate on this page is extremely rough and may behave unpredictably due to the small number of automatically generated questions. If you don't like your results, just reload the page and try again!</p>"

items<-c("taco", "kitten", "kiss", "flan", "sardine", "penny", "diamond", "hat", "marshmallow")
itemss<-c("tacos", "kittens", "kisses", "flans", "sardines", "pennies", "diamonds", "hats", "marshmallows")
times<-list(
  list(1, "1 day", c(1.1, 1.2, 1.4, 1.7, 2, 4, 6, 10)), 
  list(2, "2 days", c(1.1, 1.2, 1.4, 1.7, 2, 4, 6, 10)), 
  list(7, "1 week", c(1.1, 1.2, 1.4, 1.7, 2, 4, 6, 10)),  
  list(14, "2 weeks", 2:20), 
  list(60, "2 months", 2:20), 
  list(365, "1 year", 2:100), 
  list(730, "2 years", seq(10,1000,10)), 
  list(1825, "5 years", seq(10,1000,10))
)

dollarsign<-"$"
qs<-data.frame(n=numeric(), token=character(), vd=numeric(), vi=numeric(), d=numeric(), dname=character(), r=numeric(), rparam=numeric(), waited=numeric())
tokn<-sample(1:length(items),1)
toks<-itemss[tokn]
tok<-items[tokn]

for(n in 1:10) {
  temp<-data.frame(n=0, token=toks, vd=0, vi=0, d=0, dname="x", r=0, rparam=0, waited=-1)
  temp['n']<-n
  timei<-sample(times, 1)
  temp["d"]<-as.numeric(timei[[1]][[1]])
  temp['dname']<-timei[[1]][[2]]
  temp["vd"]<-sample(timei[[1]][[3]],1)
  temp["vi"]<-1
  temp["r"]<-temp["vi"]/temp["vd"]
  temp["rparam"]<-(1-(temp["vd"]/temp["vi"]))
  qs<-rbind(qs,temp)
}
qs<-qs
jqs<-serializeJSON(qs)
print(jqs)
print(class(jqs))
t<-"hi"

shinyUI(fluidPage(
  # Application title
  titlePanel("Delay Discounting Quiz: How much do you value the future?"),

 # this is a clumsy method for passing invisible information to the server page without it being input per se.
    tags$div(jqs, id="datadiv", name="datadivname", style="display: none;"),
    
  sidebarLayout(
    sidebarPanel( 
        tabsetPanel(
            tabPanel("Regular quiz", 
        p("This version of the quiz tests your discounting on goods other than money. Reload the page if you'd like to try a different object."),
        lapply(1:nrow(qs), function(i) {
        optc<-c(-1,0,1)
        names(optc)<-c("no response", paste(qs$vi[i], tok, "right now", sep=" "),paste(qs$vd[i], toks, "in", qs$dname[i], sep=" "))
        radioButtons(
          paste("q",qs$n[i], sep=""),
          paste("Which would you choose?"),
          optc
        )
      }
      )),
      tabPanel("Silly quiz", HTML(instructions))
      )
      
      
      #       p("simple text"),
#       textOutput('text1'),
#       p("global var"),
#       textOutput('text2'),
#       p("input var?!"),
#       textOutput('text3')
    ),

    mainPanel(
        tabsetPanel(
            tabPanel("Quiz", 
                     p("Take the quiz on the left to find out what kind of discounter you are!", style="margin: 20px; font-size: 1.3em; line-height: 120%;"),
                     
                     p("To learn more about delay discounting and how people value the future, see the 'background' tab.", style="margin: 20px; font-size: 1.3em; line-height: 120%;"),
                     
                     p("To learn more about how I'd really like this app to work, see the 'excuses' tab.", style="margin: 20px; color: #777777; font-style: italic; font-size: 1.3em; line-height: 120%;"),
                     
                     
                     
            tags$div("Answer at least 3 questions, then click here to calculate!", id="mydiv", style="padding: 20px; line-height: 120%; text-align:center; vertical-align:middle; width: 400px; height: 60px; 
           background-color: #eeeeFF; border: 1px solid blue; moz-corner-radius: 4px; webkit-corner-radius: 4px; font-size: 150%; font-weight:bold; margin-left:auto; margin-right:auto;"),
        
        p("After submitting, you can answer more questions and the calculation will update automatically, becoming more accurate (but still not all that accurate, really).", style="margin: 20px; font-size: 1.3em; line-height: 120%;"),
        htmlOutput("results")
      ),
      tabPanel("Background", HTML(instructions)),
      tabPanel("Excuses", HTML(excuses))
        ),
        plotOutput("distPlot")
      
    )
  ),
# javascript code to send data to shiny server
tags$script('
    document.getElementById("mydiv").onclick = function() {
      var jqs = document.getElementById("datadiv").innerHTML;
      Shiny.onInputChange("mydata", jqs);
    };
  ')


)
)

