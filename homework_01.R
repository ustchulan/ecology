#homework_01
rm(list = ls())               #clear output
#the first answer
install.packages("ade4")      #install package ade4
install.packages("tidyverse") #install package tidyverse
library(ade4)                 #Load the package ade4
library(tidyverse)            #load the package tidyverse
data(doubs,package="ade4")    #extract data “doubs” from the ade4 package
doubs
head(doubs)                   #checking what the data looks like  
str(doubs)                    
class(doubs) 

#the second answer
doubs <- doubs %>% rownames_to_column(var = "site")
env_tb <- as_tibble(doubs)

#the third answer
#3.1
env_final <- doubs %>%
  rownames_to_column(var = "site") %>%       # Add a site column
  as_tibble() %>%                            # Convert to tibble format
  filter(dfs > 1000)                         # Only retain data for dfs > 1000
#3.2
select(site, dfs, slo, flo, pH, nit, oxy)  # Select the columns of interest for further analysis
#3.3
rename(distsour = dfs, slope = slo, flowrate = flo, nitrogen = nit, oxy = oxy)  # Rename Columns
#3.4
arrange(slope, desc(pH))  # Sorted by slope in ascending order, pH in descending order
