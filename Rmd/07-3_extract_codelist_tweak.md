Extract codelist: 微調整
================

### `LandUseCd`

`L03-b-c`の`L03b_c_002`はコードリストが2つあるが、重複はないのでまとめてしまえばいい

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)

l <- fs::dir_ls(here::here("data", "codelist"), regexp = "LandUseCd")
l <- str_subset(l, "tweak_", negate = TRUE)
names(l) <- tools::file_path_sans_ext(basename(l))

d <- l %>% 
  purrr::map_dfr(readr::read_csv, col_types = "cc", .id = "file")

d_distinct <- d %>% 
  arrange(code) %>% 
  filter(label != "-") %>% 
  distinct(code, label) %>% 
  arrange(as.integer(code))

knitr::kable(d_distinct)
```

| code | label              |
|:-----|:-------------------|
| 100  | 田                 |
| 200  | その他の農用地     |
| 500  | 森林               |
| 600  | 荒地               |
| 700  | 建物用地           |
| 701  | 高層建物           |
| 702  | 工場               |
| 703  | 低層建物           |
| 704  | 低層建物（密集地） |
| 901  | 道路               |
| 902  | 鉄道               |
| 1000 | その他の用地       |
| 1001 | 公共施設等用地     |
| 1002 | 空地               |
| 1003 | 公園・緑地         |
| 1100 | 河川地及び湖沼     |
| 1400 | 海浜               |
| 1500 | 海水域             |
| 1600 | ゴルフ場           |

``` r
readr::write_csv(
  d_distinct,
  here::here("data", "codelist", "tweak_LandUseCd.csv")
)
```

### `*AreaStationCd`

`S05-c`の`*AreaStationCd`は手動でまとめる

``` r
l <- fs::dir_ls(here::here("data", "codelist"), regexp = "AreaStationCd")
l <- str_subset(l, "tweak_", negate = TRUE)
names(l) <- tools::file_path_sans_ext(basename(l))

d <- l %>% 
  purrr::map_dfr(readr::read_csv, col_types = "cc", .id = "file")

stopifnot(!anyDuplicated(d$code))

readr::write_csv(
  d,
  here::here("data", "codelist", "tweak_AreaStationCd.csv")
)
```
