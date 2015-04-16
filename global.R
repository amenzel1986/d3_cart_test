detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

installPackages <- function (x) {
  if (!x %in% row.names(installed.packages()))
  {install.packages(x, dependencies = T)}}

package_names <- c("shiny", "rpart", "partykit", "rCharts", "RJSONIO", 'rlist', 'pipeR', 'jsonlite', 'base64enc')

for (package in package_names) installPackages(package)

for (i in 1:length(package_names)) {library(package_names[i], character.only = T)}