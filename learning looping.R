head(gapminder)
dat <- gapminder
head(dat)


# using subdetting in R
x <- dat[(dat$year == 2002),]

nrow(x)

# using for loops
counter <- 0
for (i in 1:nrow(dat)){
if (dat[i,3] == 2002){
   
   counter <- counter + 1
}
 } 
cat(counter, "records from 2002")


# using tidyverse
library(tidyverse)

dat %>%
  filter(year == 2002) %>%
  nrow()
