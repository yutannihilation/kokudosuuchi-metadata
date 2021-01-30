Download raw data
================

``` r
library(polite)
library(rvest)
library(stringr)

session <- bow("https://nlftp.mlit.go.jp/ksj/")

result <- scrape(session)
links <- result %>% 
  html_elements("main ul.collapsible a") %>% 
  html_attr("href")

links <- str_replace(links, fixed("./"), "/ksj/")
```

``` r
dir.create(here::here("data-raw", "datalist"), showWarnings = FALSE)

for (l in links) {
  out_file <- here::here("data-raw", "datalist", basename(l))
  
  # If the file is already downloaded, skip.
  if (file.exists(out_file)) {
    message(glue::glue("{l} is already downloaded. Skipped."))
    next
  }

  message(glue::glue("Getting {l}..."))
  
  nod(session, l) %>% 
    scrape(verbose = TRUE) %>% 
    xml2::write_html(out_file)
}
```
