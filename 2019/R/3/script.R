
library(magrittr)


### modyfied after seeing the implementation from here
### https://github.com/akulumbeg/adventofcode/blob/master/2019/Day3.R
### my original implementation was stupid and slow
transform_to_path <- function(path) {
  
  steps = stringr::str_extract(pattern = "[:digit:]+",
                               string = path) %>% as.numeric()
  
  direction = stringr::str_extract(pattern = "^[R|L|U|D]",
                                   string = path)
  dx =  dplyr::case_when(
    direction == "R" ~ 1,
    direction == "L" ~ -1,
    direction == "U" ~ 0,
    direction == "D" ~ 0
  )
  
  dy =  dplyr::case_when(
    direction == "R" ~ 0,
    direction == "L" ~ 0,
    direction == "U" ~ 1,
    direction == "D" ~ -1
  )
  
  x = cumsum(rep(dx,steps))
  y = cumsum(rep(dy,steps))
  
  data.frame(x,y)
}

manhattan <- function(v1,v2) {
  sum(abs(v1 - v2))
}

crossed_wires <- function(s1,s2) {
  # I think this really don't matter
  st_point = c(1,1)
  
  mat1 = transform_to_path(s1)
  mat2 = transform_to_path(s2)
  
  dplyr::inner_join(mat1,mat2) %>%
    dplyr::mutate(manhattan = abs(x - 0) + abs(y-0)) %>% 
    dplyr::pull(manhattan) %>% 
    min()
}

plot_paths <- function(s1,s2){
  
  mat1 = transform_to_path(s1)
  mat2 = transform_to_path(s2)
  
  library(ggplot2)
  mat1 %>% as.data.frame() %>% dplyr::mutate(lbl = 1:nrow(.),type = 1) -> m1df 
  mat2 %>% as.data.frame() %>% dplyr::mutate(lbl = 1:nrow(.),type = 2) -> m2df 
  
  fp = ggplot(rbind(m1df,m2df),
              aes(x = x,y = y,
                  color = factor(type))) +
    geom_text(aes(label = lbl))
  
  return(fp)
}


## EXAMPLES TEST

# define the starting point
string_1 = c("R8","U5","L5","D3")
string_2 = c("U7","R6","D4","L4")

crossed_wires(s1 = string_1,s2 = string_2)
plot_paths(s1 = string_1,s2 = string_2)

string_1 = c("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72")
string_2 = c("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")
crossed_wires(s1 = string_1,
              s2 = string_2)
plot_paths(s1 = string_1,
           s2 = string_2)

string_1 = c("R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51")
string_2 = c("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7")

crossed_wires(s1 = string_1,
              s2 = string_2)
plot_paths(s1 = string_1,
           s2 = string_2)


## INPUT

dat = readr::read_lines("input.txt")
dat %>% purrr::map(stringr::str_split,
                   pattern = ",") %>% unlist(recursive = F) -> dat_list

crossed_wires(s1 = dat_list[[1]],
              s2 = dat_list[[2]])

#' ploting takes foreevr so we are not gonna run that.
#' 
#' ## Problem 2
#' 
#' We are asked to return the shortest distance to the first crossing point.

shortest_path <- function(s1,s2) {
  
  mat1 = transform_to_path(s1)
  mat1$steps = 1:nrow(mat1)
  mat2 = transform_to_path(s2)
  mat2$steps = 1:nrow(mat2)
  
  dplyr::inner_join(mat1,mat2,by = c("x","y"),suffix = c("_1","_2")) %>% 
    dplyr::mutate(total = steps_1 + steps_2) %>% 
    dplyr::pull(total) %>% 
    min()
}

string_1 = c("R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51")
string_2 = c("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7")

shortest_path(s1 = string_1,
           s2 = string_2)


shortest_path(s1 = dat_list[[1]],
              s2 = dat_list[[2]])
