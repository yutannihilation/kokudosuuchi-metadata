Extract data
================

``` r
library(rvest)
library(dplyr, warn.conflicts = FALSE)

dir.create(here::here("data", "attrs"), recursive = TRUE, showWarnings = FALSE)
```

``` r
datalist_files <- list.files(
  here::here("data-raw", "datalist"),
  pattern = ".*\\.html",
  full.names = TRUE,
)
id <- stringr::str_replace(basename(datalist_files), "(?:KsjTmplt-)(.*?)(?:-v.*)?(?:\\.html)", "\\1")

idx_exclude <- id %in% c("A10", "A11", "A12", "A13", "C23", "A09", "A20", "A21", "A31", "S10a")
datalist_files <- datalist_files[!idx_exclude]
id <- id[!idx_exclude]

names(datalist_files) <- id

tables <- purrr::map(datalist_files, ~ {
  read_html(.x) %>% 
    html_table(convert = FALSE) %>% 
    purrr::keep(~ "地物情報" == colnames(.x)[1])
})

# テーブル一つだけなことを確認
stopifnot(all(lengths(tables) == 1L))

# 確認したら、アクセスしやすくするために一段階減らす
tables <- purrr::map(tables, 1L)
```

属性情報をもたないテーブルは取り除く

``` r
no_attr_info <- names(tables)[purrr::map_lgl(tables, ~ !"属性情報" %in% .x[[1]])]
no_attr_info
```

    ## [1] "L03-b_r" "P03"     "P12"     "P33"

``` r
id <- id[!id %in% no_attr_info]
tables <- tables[!names(tables) %in% no_attr_info]
```

``` r
tables <- tables %>% 
  purrr::imap(~ {
    # 属性情報の行だけにする
    .x <- .x[startsWith(.x[[1]], "属性情報"), , drop = FALSE]
    # 1行目はヘッダ
    nm <- .x[1, ]
    .x <- .x[-1, ]
    colnames(.x) <- nm
    # すべてNAの列は取り除く
    idx <- purrr::map_lgl(.x, ~ any(!is.na(.)))
    .x[, idx]
  })
```

属性情報の行が1行だけのテーブルも取り除く

``` r
no_rows <- names(tables)[purrr::map_lgl(tables, ~ nrow(.x) == 0)]
no_rows
```

    ## [1] "mesh1000h30" "mesh500h30"  "P22"         "P23"

``` r
id <- id[!id %in% no_rows]
tables <- tables[!names(tables) %in% no_rows]
```

カラム数は4のはずだが、ちがうやつがある。この辺はあとで個別に処理することにする。

``` r
names(tables)[lengths(tables) != 4L]
```

    ##  [1] "A29"   "A30b"  "L01"   "L02"   "L05"   "N08"   "P20"   "P26"   "S05-a"
    ## [10] "S05-b"

``` r
tables <- tables[lengths(tables) == 4L]
```

``` r
# 2番めの列が「属性名」または「属性名（...）」のようなフォーマットであることを確認
stopifnot(all(startsWith(purrr::map_chr(tables, ~ colnames(.)[2]), "属性名")))

tables %>% 
  purrr::iwalk(~ {
    .x %>% 
      `colnames<-`(c("_", "name", "description", "type")) %>% 
      filter(
        `_` == "属性情報",
        # テーブルのヘッダは取り除く
        !startsWith(name, "属性名"),
        !startsWith(name, "地物名"),
        !startsWith(name, "関連役割名"),
        type != "説明",
        # A42 には変な行がある
        name != "歴史的風土保存区域",
        # 地物は属性情報ではない
        !stringr::str_detect(stringr::str_remove_all(type, "\\s+"), "^(地物|曲?面型|曲?線型|点型|GM_Surface|GM_Curve|GM_Point)")
      ) %>%
      # name に改行が入ってるとうまくいかないので取り除く
      mutate(name = stringr::str_squish(name)) %>% 
      # 「※シェープファイルのみ」はP32のみのため
      tidyr::extract(name, into = c("name", "code"), regex = "(.*?)([（〈\\()][A-Za-z0-9]+_[A-Za-z0-9_]+[）\\)])?(?:$|※シェープファイルのみ)") %>% 
      transmute(
        name,
        code = if_else(code != '', stringr::str_remove_all(code, "[（〈\\(）\\)]"), NA_character_),
        description,
        type
      ) %>% 
      readr::write_csv(here::here("data", "attrs", glue::glue("{.y}.csv")))
  })
```
