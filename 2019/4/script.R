
#' # Day1
#'
#' Brute forcing again because I don't have enough time to think...
#'


#' ## Problem 1

# from here https://stackoverflow.com/questions/19764244
get_digits <- function(x){
  dig <- ceiling(log10(x))
  vec1 <- 10^(dig:1)
  vec2 <- vec1/10
  (x%%vec1)%/%vec2
}

is_valid_pwd <- function(x){
  vec = get_digits(x)
  !is.unsorted(vec) & any(duplicated(vec))
}

# range should be a string such as "382345-843167"
secure_container <- function(range){
  dat = stringr::str_split(range,"-")
  all_pos = dat[[1]][1]:dat[[1]][2]
  mask = sapply(all_pos, is_valid_pwd)
  return(length(unique(all_pos[mask])))
}

secure_container("382345-843167")

#' ## Problem 2

is_valid_pwd2 <- function(x,k){
  vec = get_digits(x)
  !is.unsorted(vec) & any(table(vec) ==k)
}

secure_container2 <- function(range){
  dat = stringr::str_split(range,"-")
  all_pos = dat[[1]][1]:dat[[1]][2]
  mask = sapply(all_pos, is_valid_pwd2, k =2)
  return(length(unique(all_pos[mask])))
}

secure_container2("382345-843167")
