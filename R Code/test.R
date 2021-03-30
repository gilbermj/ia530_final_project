library(gridExtra)
library(ggplot2)
library(dplyr)

temp_list <- list() 

for(i in 1:12){
  
  a <- 1:13+i
  
  print(a)
  
  temp_plot <- ggplot2::ggplot(data=NULL,aes(x=1:13, y=a)) + 
    ggplot2::geom_point()
  
  print(temp_plot)
  
  temp_list <- append(temp_list, list(temp_plot), after = length(temp_list))
  
  
}

grid <- grid.arrange(grobs=temp_list)

