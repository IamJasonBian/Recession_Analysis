library(dplyr)
library(ggplot2)

#Job Openings: Jobs for all Professional and Business Services
#Hires: 
#Rollup=Hierarchy : Opening, Hirings, Seperations
#Seperations -> Quits, Layouts + Discharges

Read_Historicals <- function (path){
  
  #Function to transform Factset data into R list
  
  #List collapse logic below
  #time_series_list[[header_names[1]]] <- dropped_df %>% select(c(2,3))
  #list[[""]] <-dropped_df %>% select(c(4,5))
  #list[[""]] <-dropped_df %>% select(c(6,7))
  #list[[""]] <-dropped_df %>% select(c(8,9))
  #list[[""]] <-dropped_df %>% select(c(10,11))
  
  
  df <-read.csv(path)
  
  remove_col_ind <- seq(2, 16, by = 3)
  
  Dates <- df %>% select(1) 
  colnames(Dates) <-c("Dates")
  dropped_df <- df %>% select(-c(remove_col_ind)) 
  header_names <- df %>% select(c(remove_col_ind)) %>% colnames()
  
  
  time_series_list <- list()
  
  counter = 0
  for(i in 1:5){
    
    time_series_list[[header_names[i]]] <- dropped_df %>% 
      select(c(i+1 + counter,i+2 + counter))
    counter = counter + 1
  }

  return(list(time_series_list, Dates))
}

path <- "./Data/Time_Series_Hires.csv"

time_series_list <- Read_Historicals(path)[[1]]
Dates <- Read_Historicals(path)[[2]]

names <- names(time_series_list)


for(i in 1:length(names)){
  
  df <- data.frame(Date = Dates, Value = time_series_list[[names[i]]][, 1])
  
  plot(df$Value)
}






