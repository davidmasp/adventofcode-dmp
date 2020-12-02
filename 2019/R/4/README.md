# Day 4
author: David Mas-Ponte
date: "4/12/2019"
## Usage
Convert script to README.md, run from console in the 2019/4 directory.
There might be a better way to do this.


```r
if (interactive()){
  rmd_file = knitr::spin("script.R", knit = FALSE)
  knitr::knit(rmd_file,output = "README.md")
  fs::file_delete(rmd_file)
}
```

## Problem 1


Brute forcing again because I don't have enough time to think...



```r
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
```

```
## [1] 460
```

## Problem 2


```r
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
```

```
## [1] 290
```

