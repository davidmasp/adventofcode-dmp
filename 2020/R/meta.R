# metaa
library(magrittr)
render_all <- function() {
  files = fs::dir_ls(".",recurse = T,glob = "*/README.Rmd")
  exc = fs::path_dir(files)
  purrr::map2_df(files,exc,function(x,exc){
    bench::mark(
      rmarkdown::render(input = x,quiet = TRUE),iterations = 1
    ) -> times
    times %>% 
      dplyr::select(min,median,mem_alloc) %>%
      tibble::as_tibble() -> res
    res$exc = exc
    res
  })
}


## exec
render_all() -> bench_mark

saveRDS(object = bench_mark, file = "benchM.rds")

