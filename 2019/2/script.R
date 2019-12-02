#' # Day 2

#' author: "David Mas"
#' date: "2/12/2019"

#' ## Usage

#' Convert script to README.md, run from console in the 2019 directory.
#' There might be a better way to do this.

#+ eval=FALSE, include=TRUE
if (interactive()){
  rmd_file = knitr::spin("script.R", knit = FALSE)
  knitr::knit(rmd_file,output = "README.md")
  fs::file_delete(rmd_file)
}

#' ## Problem 1

#' Because R is 1-based, positions are actually +1.

#' Non-recursive solution.

gravity_assist <- function(x,iteration=1) {
  while (TRUE) {
    opt = x[iteration]
    if (opt == 99){
      return(x)
    } else {
      quartet = iteration:(iteration+3)
      # remember that R is 1-based
      idx_x = x[quartet][2] + 1
      idx_y = x[quartet][3] + 1
      idx_out = x[quartet][4] + 1

      if (opt == 1){
        x[idx_out] = x[idx_x] + x[idx_y]
      } else if (opt == 2){
        x[idx_out] = x[idx_x] * x[idx_y]
      } else{
        stop("error")
      }
      iteration = iteration + 4
    }
  }
}

#' Recursive solution

gravity_assist_recursive <- function(x,iteration=1) {
  opt = x[iteration]
  if (opt == 99){
    return(x)
  } else {
    quartet = iteration:(iteration+3)
    # remember that R is 1-based
    idx_x = x[quartet][2] + 1
    idx_y = x[quartet][3] + 1
    idx_out = x[quartet][4] + 1

    if (opt == 1){
      x[idx_out] = x[idx_x] + x[idx_y]
    } else if (opt == 2){
      x[idx_out] = x[idx_x] * x[idx_y]
    } else{
      stop("error")
    }
    return(gravity_assist_recursive(x,iteration = iteration + 4))
  }
}

#' Now we use the test values from the problem

test = list(c(1,0,0,0,99),
            c(2,3,0,3,99),
            c(2,4,4,5,99,0),
            c(1,1,1,4,99,5,6,0,99))

lapply(test, gravity_assist)
lapply(test, gravity_assist_recursive)


#' Finally process the input
#' To do this, before running the program,
#' *replace position 1 with the value 12*
#' and replace *position 2 with the value 2*.
#' What value is left at position 0 after the program halts?

dat = readr::read_lines("input.txt")
x <- stringr::str_split(dat[[1]],pattern = ",")[[1]]
x <- as.numeric(x)
x[2] = 12
x[3] = 2
gravity_assist_recursive(x = x) -> res

clipr::write_clip(res[1])

res[1]

#' ## Problem 2

#' Brute force solution

reverse_gravity_assist <- function(x,res) {
  x_original = x
  for (i in 1:99){
    for (j in 1:99){
      x[2] = i # noun
      x[3] = j # verb
      res_out = gravity_assist_recursive(x = x)
      if (res_out[1] == res){
        return(100 * i + j)
      } else{
        x = x_original
      }
    }
  }
}

reverse_gravity_assist(x,res = 19690720)


#' I think there might be a better solution (non-brute forcing for this),
#' however, in the solutions I found there was none.
