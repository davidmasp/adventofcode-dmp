#' ---
#' title: "Day 2"
#' author: "David Mas"
#' date: "2/12/2019"
#' output: html_document
#' ---

#' ## Usage

#' Convert script to README.md, run from console in the 2019 directory.
#' There might be a better way to do this.

#+ eval=FALSE, include=TRUE
rmd_file = knitr::spin("2/script.R", knit = FALSE)
knitr::knit(rmd_file,output = "2/README.md")
fs::file_delete(rmd_file)

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

dat = readr::read_lines("input.txt")
x <- stringr::str_split(dat[[1]],pattern = ",")[[1]]
x <- as.numeric(x)
gravity_assist_recursive(x = x) -> res
res = paste0(res,collapse = ",")
clipr::write_clip(res)

#' ## Problem 2
