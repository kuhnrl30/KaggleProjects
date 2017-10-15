[![Build Status](https://travis-ci.org/kuhnrl30/RandomActsofPizza.svg?branch=master)](https://travis-ci.org/kuhnrl30/RandomActsofPizza)

There was a thread on Reddit where people could ask other people to buy them pizza for free. In their request, the often gave a reason why they deserved the pizza- lost a job, celebrating a new job, or perhaps just for fun.  Not everyone got a free pizza and a PhD student was studying the requests to find out why.  After he published his paper, he passed the data to Kaggle and a competition was launched to see if the community could do better than the expert.  The objective of this competition was to develop a model and understand what features will lead to a successful request.  I've wrapped my analysis into an R package and used the vignettes to document my workflow.  You can download the R package using the instructions below.  You can also view the project site at [http://ryankuhn.net/RandomActsofPizza/](http://ryankuhn.net/RandomActsofPizza/) and with these links:

- [Exploratory Analysis](docs/ExploratoryAnalysis.html)
- [Model Building](docs/ModelBuilding.html)  


```
devtools::install_github("kuhnrl30/RandomActsofPizza")
library(RandomActsofPizza)
vignette("ExploratoryAnalysis")
vignette("ModelBuilding")
```





