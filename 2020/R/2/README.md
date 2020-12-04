
# README

``` r
library(magrittr)
vroom::vroom(file = "test.tst", col_names = F) -> dat
```

    ## Rows: 3
    ## Columns: 3
    ## Delimiter: " "
    ## chr [3]: X1, X2, X3
    ## 
    ## Use `spec()` to retrieve the guessed column specification
    ## Pass a specification to the `col_types` argument to quiet this message

``` r
toboggan <- function(dat) {
  dat %>%
  tidyr::separate(X1, into = c("min","max")) %>% 
  dplyr::mutate(min = as.integer(min),
                max = as.integer(max)) %>% 
  dplyr::mutate(letter = stringr::str_extract(X2,"[:alnum:](?=:)")) %>% 
  dplyr::mutate(count = stringr::str_count(X3,pattern = letter)) %>% 
  dplyr::mutate(isValid = count <= max & count >=min ) -> res
  
  sum(res$isValid)
}

toboggan(dat)
```

    ## [1] 2

``` r
vroom::vroom(file = "input.txt", col_names = F) -> dat
```

    ## Rows: 1,000
    ## Columns: 3
    ## Delimiter: " "
    ## chr [3]: X1, X2, X3
    ## 
    ## Use `spec()` to retrieve the guessed column specification
    ## Pass a specification to the `col_types` argument to quiet this message

``` r
toboggan(dat)
```

    ## [1] 500

``` r
toboggan2 <- function(dat) {
  dat %>%
  tidyr::separate(X1, into = c("p1","p2")) %>% 
  dplyr::mutate(p1 = as.integer(p1),
                p2 = as.integer(p2)) %>% 
  dplyr::mutate(letter = stringr::str_extract(X2,"[:alnum:](?=:)")) %>% 
  split(rownames(.)) %>% 
    purrr::map_lgl(function(x){
    
      x$X3 %>% stringr::str_split("") -> tmp
      mask = tmp[[1]][c(x$p1,x$p2)] == x$letter
      sum(mask) == 1
    }) -> res

  sum(res)
}


toboggan2(dat)
```

    ## [1] 313
