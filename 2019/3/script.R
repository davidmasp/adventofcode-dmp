
library(magrittr)

transform_to_path <- function(path,start) {


  string_split = stringr::str_split(path,pattern = "")
  values = string_split %>% purrr::map(2) %>% unlist %>% as.numeric()
  total_points = sum(values)*2
  final_idx = cumsum(values)
  direction = string_split %>% purrr::map(1) %>% unlist()
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
  for (i in 1:length(string_split)){
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

  match = apply(mat1,1, function(x){which(apply(mat2,1, function(y){all(y == x)}))}) %>% unlist()

  min(apply(mat2[match,], 1, manhattan,v2 = st_point))

}

# test 1

# define the starting point
string_1 = c("R8","U5","L5","D3")
string_2 = c("U7","R6","D4","L4")

crossed_wires(s1 = string_1,s2 = string_2)

crossed_wires(s1 = c("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"),
              s2 = c("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83"))


