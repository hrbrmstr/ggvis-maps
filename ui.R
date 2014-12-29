
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)

me_crime <- read.csv("data/me_crime.csv", stringsAsFactors=FALSE)

shinyUI(fluidPage(
  titlePanel("ggvis shiny maps"),
  sidebarLayout(
    sidebarPanel(
      p(strong("NOTE: "), "Since this loads many maps, it can take a few seconds to startup. Even after that, there may be some delay with more complex maps due to how ggvis renders them."),
      hr(),
      p("This is an example of making static & interactive maps with ggvis and wiring them up interactively in a Shiny app."),
      p("The first two render static Maine state maps (with & without county labels). The second two let you interactively explore Maine county crime data for 2013 (by 1K popualtion)."),
      p("The U.S. Drought Map interactively shows drought levels as of 2014-12-23 and is also an example of using a projection (Albers) & also custom colors outside of any ggvis scale). It also shows that many polygons can take a while to render (initially)."),
      p("The final example is a world map with a Winkel-Tripel projection and also shows how to add projected points to the map (no interactivity)."),
      hr(),
      p("Written by ", a(href="http://twitter.com/hrbrmstr", "@hrbrmstr")),
      p("Source on ", a(href="https://github.com/hrbrmstr/ggvis-maps", "github")),
      p("Static version on ", a(href="http://rpubs.com/hrbrmstr/ggvis-maps", "RPubs")),
      width=3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Maine", wellPanel(p("This is just a basic map outline. Since ggvis has no ", code("coord_map"), " we have to get creative with a fixed aspect ratio and good choices for height & width.")), ggvisOutput("maine")),
        tabPanel("Maine (Labels)", wellPanel(p("Again, no interaction here, but this example shows how to annotate a static map with points and labels.")), ggvisOutput("maine_labels")),
        tabPanel("Maine Crime", wellPanel(p("We finally get some interaction and also add tooltips. We use ", code("input_select"), " to enable exploration between different crimes.")), uiOutput("maine_crime_1_ui"), ggvisOutput("maine_crime_1")),
        tabPanel("Maine Crime (log)", wellPanel(p("This is the same as the previous one, but uses the full range of all crimes for the fill color (log scale).")), uiOutput("maine_crime_2_ui"), ggvisOutput("maine_crime_2")),
        tabPanel("US Drought", wellPanel(p("This shows how to use a projection and also a custom color scale without using anh of the built-in ", code("scale_") , " functions. It also shows the use of a tooltip where there is no data for some polygons.")), ggvisOutput("drought")),
        tabPanel("Global Launch Sites", wellPanel(p("The last example shows how to both use a projection and also how to project other points to display on the map.")) ,ggvisOutput("launch"))
      ),
      width=9
    )
  )
))