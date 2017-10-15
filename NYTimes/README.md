[![Build Status](https://travis-ci.org/kuhnrl30/NYTimes.svg?branch=master)](https://travis-ci.org/kuhnrl30/NYTimes)

# New York Times Blog Analysis  
This project was the capstone project for the Analytics Edge course. The course was created by MIT offered on 
by edX. The competition itself was hosted on Kaggle. The objecive of the analysis was to predict whether an 
article would be popular. My model predicted the correct outcome with 89% accuracy which was only 3 
percent behind the competition winner. See the project site at [http://www.ryankuhn.net/NYTimes/](http://www.ryankuhn.net/NYTimes/) and [view the analysis](docs/NYTimes.html).

```{r}
devtools::install_github("kuhnrl30/NYTimes")
library(NYTimes)
vignettes(NYTImes)
```
