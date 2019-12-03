
library(magrittr)

transform_to_path <- function(path,start) {

  values = stringr::str_extract(pattern = "[:digit:]+",
                                string = path) %>% as.numeric()
  total_points = sum(values)*2
  final_idx = cumsum(values)
  direction = stringr::str_extract(pattern = "^[R|L|U|D]",
                                   string = path)
  sign =  dplyr::case_when(
    direction == "R" ~ 1,
    direction == "L" ~ -1,
    direction == "U" ~ 1,
    direction == "D" ~ -1
  )
  dimension = dplyr::case_when(
    direction == "R" ~ 1,
    direction == "L" ~ 1,
    direction == "U" ~ 2,
    direction == "D" ~ 2
  )

  # long shot
  values = values * sign

  path_m = matrix(integer(total_points),ncol = 2)
  initial_idx = 1
  cpoint = start
  for (i in 1:length(values)){
    initial_point = cpoint[dimension[i]] +  sign[i]
    final_points = initial_point:(initial_point + values[i] - sign[i] )

    iidx = ifelse(i == 1,1,final_idx[i-1] + 1)
    fidx = final_idx[i]
    path_m[iidx:fidx,dimension[i]] = final_points

    dim = c(1,2)
    not_dim = dim[!dim == dimension[i]]
    path_m[iidx:fidx,not_dim] = cpoint[not_dim]

    cpoint = path_m[fidx,]
  }

  return(path_m)
}

manhattan <- function(v1,v2) {
  sum(abs(v1 - v2))
}

crossed_wires <- function(s1,s2) {
  # I think this really don't matter
  st_point = c(1,1)

  mat1 = transform_to_path(s1,start = st_point)
  mat2 = transform_to_path(s2,start = st_point)

  match = apply(mat1,1, function(x){
    which(apply(mat2,1, function(y){
      all(y == x)}))}) %>% unlist()

  min(apply(mat2[match,], 1, manhattan,v2 = st_point))

}

plot_paths <- function(s1,s2){
  st_point = c(1,1)
  
  mat1 = transform_to_path(s1,start = st_point)
  mat2 = transform_to_path(s2,start = st_point)
  
  library(ggplot2)
  mat1 %>% as.data.frame() %>% dplyr::mutate(lbl = 1:nrow(.),type = 1) -> m1df 
  mat2 %>% as.data.frame() %>% dplyr::mutate(lbl = 1:nrow(.),type = 2) -> m2df 
  
  fp = ggplot(rbind(m1df,m2df),
         aes(x = V1,y = V2,
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


plot_paths(s1 = dat_list[[1]],
           s2 = dat_list[[2]])
