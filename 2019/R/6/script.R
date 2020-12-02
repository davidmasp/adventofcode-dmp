#' # Day 6

#' * author: David Mas-Ponte
#' * date: "10/12/2019"

#' ## Usage

#' Convert script to README.md, run from console in the 2019/4 directory.
#' There might be a better way to do this.

#+ eval=FALSE, include=TRUE
if (interactive()){
  rmd_file = knitr::spin("script.R", knit = FALSE)
  knitr::knit(rmd_file,output = "README.md")
  fs::file_delete(rmd_file)
}


#' Basically the problem is to resolve a orbit map. I chose to do so in
#' a matrix because I thought it would be fast enough. I guess there is
#' faster algorithms and I guess also faster implementations.
#'
#' There is 3 steps in this one:
#' 1 - I construct the interaction matrix
#' 2 - I order the levels from left to right
#' 3 - I propagate the rows so I can just count in the end
#'
#' Rows represent the paths from X and columns the paths to X. So, if
#' COM is the Center Of Mass, the COM column will be 0. The edges of
#' the network however, will have a 0 row.

#' ## General Functions

# imports ----------------------------------------------------------------
library(magrittr)

# funs --------------------------------------------------------------------

orbit_map <- function(orbit_list){
  levels_unsorted = unique(unlist(orbit_list,recursive = T))

  total_el = length(levels_unsorted)
  n_comb = total_el**2
  mat = matrix(double(n_comb),
               ncol = total_el,
               dimnames = list(levels_unsorted,levels_unsorted))


  for (i in 1:length(orbit_list)){
    up_val = orbit_list[[i]][1]
    down_val = orbit_list[[i]][2]
    mat[up_val,down_val] = 1
  }

  # sort the matrix levels
  levels = character(length(levels_unsorted))
  tmp_mat = mat
  tmp_levels = levels_unsorted
  for ( i in 1:length(levels_unsorted)){
    if (i == length(levels_unsorted)){
      curr = levels_unsorted[!levels_unsorted %in% levels]
      levels[i] = curr
    }else {
      curr =  colnames(tmp_mat)[which.min(apply(tmp_mat,2,sum))]
      tmp_levels = tmp_levels[!tmp_levels == curr]
      tmp_mat = tmp_mat[tmp_levels,tmp_levels]
      levels[i] = curr
    }
  }

  # now the proper sorting
  mat = mat[levels,levels]
  orbit_list_1 = orbit_list %>% purrr::map_chr(1)
  dat_fct = factor(orbit_list_1,levels = levels)
  orbit_list = orbit_list[order(dat_fct)]

  # propagation
  for (i in length(orbit_list):1){
    up_val = orbit_list[[i]][1]
    down_val = orbit_list[[i]][2]
    mat[up_val,] = mat[down_val,] + mat[up_val,]
  }

  return(mat)

}

sum_indirect_orbits <- function(orbit_list) {
  orbit_mat =orbit_map(orbit_list)
  return(sum(orbit_mat))
}

get_dist_from <- function(orbit_map, x, y) {
  shared_steps = sum(apply(orbit_map[,c(x,y)], 1, sum) == 2)
  steps_to_x = sum(orbit_map[,x])
  steps_to_y = sum(orbit_map[,y])

  dist = (steps_to_x + steps_to_y) - (shared_steps * 2)
  return(dist)

}

#' ## Problem 1

# script -----------------------------------------------------------------

dat = readr::read_lines("test.txt")
# I do this because in my previous implemetation I relied in the order
# of the original vector. now this should work if vector is not ordered
dat = sample(dat)
dat %>% stringr::str_split("\\)") -> dat_split
sum_indirect_orbits(dat_split)

# no
dat = readr::read_lines("input.txt")
dat %>% stringr::str_split("\\)") -> dat_split
sum_indirect_orbits(dat_split)


#' ## Problem 2
#
dat = readr::read_lines("test2.txt")
dat %>% stringr::str_split("\\)") -> dat_split
test_map = orbit_map(dat_split)
get_dist_from(test_map,"YOU","SAN")



dat = readr::read_lines("input2.txt")
dat %>% stringr::str_split("\\)") -> dat_split
input_map = orbit_map(dat_split)
get_dist_from(input_map,"YOU","SAN")
