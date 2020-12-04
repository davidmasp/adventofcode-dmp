
# README

I convert to matrix then I count the ones in the indices that I get.

``` r
readLines("test.tst") -> lines

transform_to_matrix <- function(lines) {
  
  n_cols = nchar(lines[1])
  n_rows = length(lines)
  
  mat = matrix(0,nrow = n_rows,ncol = n_cols)
  
  for (i in seq_len(length(lines))){
    idx = which(grepl("#",unlist(strsplit(lines[i],""))))
    mat[i,idx] = 1
  }
  
  mat
  
}

calculate_max_cols <- function(mat,right = 3, down = 1) {
  max_cols = right*((nrow(mat)-1)/down)
  mult_val = ceiling((max_cols) / ncol(mat))
  matrix(rep(mat,mult_val),
         nrow = nrow(mat),
         ncol= ncol(mat)*mult_val) -> exp_mat
  
  exp_mat
}

calculate_trees <- function(mat,right = 3, down = 1){
  #browser()
  n_steps = (nrow(mat)-1)/down
  x_pos = 1:n_steps * right + 1 
  y_pos = 1:n_steps * down + 1
  
  res = diag(mat[y_pos,x_pos])
  sum(res)
}

trees <- function(lines, right_step = 3, down_step = 1){
  #browser()
  mat_in = transform_to_matrix(lines)
  exp_mat_in = calculate_max_cols(mat_in,right_step,down_step)
  calculate_trees(exp_mat_in,right_step,down_step)
}

trees(lines)
```

    ## [1] 7

``` r
readLines("input.txt") -> lines

trees(lines)
```

    ## [1] 220

``` r
## this will be "much" faster if I only process the matrix once
slopes = list(
  right_step = c(1,3,5,7,1),
  down_step = c(1,1,1,1,2)
)

trees2 <- function(lines,slopes){
  mat_in = transform_to_matrix(lines)
  purrr::pmap(.l = slopes,.f = function(right_step,down_step){
      exp_mat_in = calculate_max_cols(mat_in,right_step,down_step)
      calculate_trees(exp_mat_in,right_step,down_step)
  }) -> res
  prod(unlist(res))
}


trees2(lines,slopes)
```

    ## [1] 2138320800
