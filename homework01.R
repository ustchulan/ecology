#homework_01
rm(list = ls())               #clear output
#the first answer
install.packages("ade4")      #install package ade4
install.packages("tidyverse") #install package tidyverse
library(ade4)                 #Load the package ade4
library(tidyverse)            #load the package tidyverse
data(doubs)                   #extract data “doubs” from the ade4 package
doubs
head(doubs)                   #checking what the data looks like  
str(doubs)                    
class(doubs) 

#the second answer
env <-doubs$env
env_df <- tibble::rownames_to_column(env, var = "site")   #Turn the row name into a column named "site"
env_tb <- as_tibble(env_df)                               #Convert to tibble and rename
class(env_tb)                                             #checking the class of the data.
env_tb

#the third answer
# Rename environment data box
env_df <- as.data.frame(doubs$env)
env_tb <- env_df %>% 
  # Filter data for distances over 1000 km
  rownames_to_column(var = "site") %>%
  as_tibble() %>% 
  filter(dfs > 1000) %>% 
  # Select the columns of interest for further analysis
  select(site, dfs, slo, flo, pH, nit, oxy) %>% 
  # Rename Columns
  rename(distsour = dfs, slope = slo, flowrate = flo, nitrogen = nit, oxy = oxy) %>% 
  # Sorted by slope in ascending order, pH in descending order
  arrange(slope, desc(pH)) 
# Assign the final result to env_final
env_final <- env_tb
print(env_final)
