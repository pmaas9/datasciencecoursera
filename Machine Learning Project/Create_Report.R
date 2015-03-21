library(slidify)
library(knitr)

setwd("C:/Users/Paige/Documents/GitHubStuff/datasciencecoursera/Machine Learning Project")
#author("Report")  ## gets mostly blank one started

slidify("Report.Rmd")
browseURL("Report.HTML")

## To publish, can to to gitHub and make repo
##  Then do:  publish_github(user,repo)
## call when R is looking at directory with your slidify project

## To get Latex for math equations
## must update the YAML header to have
## widget: [mathjax]