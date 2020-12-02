#' # Day 5

#' * author: David Mas
#' * date: 21/12/2019

#' ## Usage

#' Convert script to README.md, run from console in the 2019 directory.
#' There might be a better way to do this.

#+ eval=FALSE, include=TRUE
if (interactive()){
  rmd_file = knitr::spin("script.R", knit = FALSE)
  knitr::knit(rmd_file,output = "README.md")
  fs::file_delete(rmd_file)
}

#' ## Problem 1 & 2
#' 
#' Today I have included both solutions in the same function because it was
#' simply too big. Basically it´s a modification of day4 computer.
#' I had to modify the get_digits function because it was giving
#' wrong values for 10, 100 etc.
#' 
#' I now introduced a optcode parser to separate that part from the computer 
#' one. The computer now is basically a set of else ifs that will make the 
#' computer commands. As always the 0-based (puzzle) to 1-based (puzzle) gave
#' me a lot of headaches.
#' 
#' Also I had a stupid bug in the output part that was driving me crazy, 
#' the result was okay but the output was failing for whatever reason...
#' 

# from day 4, modified
# from here https://stackoverflow.com/questions/19764244
get_digits <- function(x){
  dig <- nchar(x)
  vec1 <- 10^(dig:1)
  vec2 <- vec1/10
  (x%%vec1)%/%vec2
}

parse_optcode <- function(int){
  
  opt = int%%100
  params_opt = int%/%100
  params_opt_dig = get_digits(params_opt)
  res = list()
  # first number of parameters
  if (opt %in% c( 1,2,7,8)){
    nparams = 3
  } else if (opt %in% c(5,6)) {
    nparams = 2
  } else { # this is 3 and 4
    nparams = 1
  }
  
  # generate params modes
  param_modes = integer(nparams)
  idx_1 = length(param_modes) - length(params_opt_dig) + 1
  param_modes[idx_1:length(param_modes)] = params_opt_dig
  param_modes = rev(param_modes) # the order is reversed
  
  return(list(opt = opt,
              nparams = nparams,
              param_modes = param_modes))
}
  
gravity_assist_recursive <- function(x,
                                     iteration=1,
                                     input,
                                     output = NULL) {
  

  opt = parse_optcode(x[iteration])
  
  if (opt$opt == 99){
    return(list(x = x,
                output = output))
  } else {
    param_modes = opt$param_modes
    params_idx = (iteration+1):(iteration+opt$nparams)
    params = x[params_idx]
    params_values = params
    params_values[param_modes == 0] = x[params[param_modes == 0]+1]
    # this is not needed, but might be in the future
    # params[param_modes == 1] = params[param_modes == 1]
    
    # this is the general part 
    ipointer = iteration + opt$nparams + 1
    
    ## the computer part
    if (opt$opt == 1){
      x[params[3]+1] = sum(params_values[1:2])
    } else if (opt$opt == 2){
      x[params[3]+1] = prod(params_values[1:2])
    } else if (opt$opt == 3){
      stopifnot(length(params) == 1)
      x[params+1] = input
    } else if (opt$opt == 4){
      # maybe check params is length 1
      stopifnot(length(params) == 1)
      if (is.na(params_values)){
         browser()
      }
      output = c(output,params_values)
    } else if (opt$opt == 5){
      if(params_values[1] != 0){
        ipointer = params_values[2] + 1
      }
    } else if (opt$opt == 6){
      if(params_values[1] == 0){
        ipointer = params_values[2] + 1
      }
    } else if (opt$opt == 7){
      if(params_values[1] < params_values[2]){
        x[params[3]+1] = 1 # params because position will always be mode 0
      } else {
        x[params[3]+1] = 0
      }
    } else if (opt$opt == 8){
      if(params_values[1] == params_values[2]){
        x[params[3]+1] = 1 # position will always be mode 0
      } else {
        x[params[3]+1] = 0
      }
    } else{
      browser()
      stop("error")
    }

    return(gravity_assist_recursive(x,
                                    iteration = ipointer,
                                    input = input,
                                    output = output))
  }
}

#' Here are the activities of problem 1

gravity_assist_recursive(c(1002,4,3,4,33),input = 1)
readLines("input.txt") -> program
program = as.numeric(unlist(stringr::str_split(program,",")))
gravity_assist_recursive(program,input = 1)

#' Here are the activities of problem 2
test = c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,
         1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,
         1105,1,46,98,99)
gravity_assist_recursive(test,input = 7)
gravity_assist_recursive(test,input = 8)
gravity_assist_recursive(test,input = 9)

gravity_assist_recursive(program,input = 5)

