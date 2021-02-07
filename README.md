
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kokudosuuchi-metadata

<!-- badges: start -->
<!-- badges: end -->

## Coverages

``` r
test_data <- fs::dir_ls(here::here("data-raw", "zip"))
cache_dir <- here::here("cache")

dir.create(cache_dir, showWarnings = FALSE)

source(here::here("scripts", "read_ksj_data.R"))
```

``` r
translate_columns_safe <- purrr::safely(translate_columns)
read_zip_with_cache_safe <- purrr::safely(read_zip_with_cache)

options(warn = 2)
result <- purrr::map(test_data, ~ {
  l <- read_zip_with_cache_safe(.x, cache_dir = cache_dir)
  if (!is.null(l$error)) {
    return(l)
  }
  translate_columns_safe(l$result)
})

errors <- purrr::map_chr(result, ~ {
  if (is.null(.$error)) {
    return(NA_character_)
  }
  
  as.character(.$error)
})

library(dplyr, warn.conflicts = FALSE)

tibble::enframe(errors) %>% 
  mutate(
    name = basename(names(errors))
  ) %>% 
  knitr::kable()
```

| name                                    | value                                                                                                                                                                                          |
|:----------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1km\_mesh\_suikei\_2018\_shape\_06.zip  | Error: Not implemented                                                                                                                                                                         |
| 500m\_mesh\_suikei\_2018\_shape\_23.zip | Error: Not implemented                                                                                                                                                                         |
| A03-03\_SYUTO-tky\_GML.zip              | NA                                                                                                                                                                                             |
| A09-18\_22\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| A10-15\_13\_GML.zip                     | NA                                                                                                                                                                                             |
| A11-15\_42\_GML.zip                     | NA                                                                                                                                                                                             |
| A12-15\_08\_GML.zip                     | NA                                                                                                                                                                                             |
| A13-15\_21\_GML.zip                     | Error: There are some columns yet to be translated: ET\_ID,ET\_Source                                                                                                                          |
| A15-15\_14\_GML.zip                     | Error: There are some columns yet to be translated: ORIG\_FID                                                                                                                                  |
| A16-15\_04\_GML.zip                     | Error: The numbers of columns don’t match. expected: 12, actual: 11                                                                                                                            |
| A17-17\_01\_GML.zip                     | NA                                                                                                                                                                                             |
| A18-16\_12\_GML.zip                     | NA                                                                                                                                                                                             |
| A18-16\_24\_GML.zip                     | NA                                                                                                                                                                                             |
| A18s-a-10\_GML.zip                      | Error: Not implemented                                                                                                                                                                         |
| A19-001001\_01\_GML.zip                 | Error: The numbers of columns don’t match. expected: 9, actual: 13                                                                                                                             |
| A19s-a-10\_28\_GML.zip                  | Error: The numbers of columns don’t match. expected: 5, actual: 112                                                                                                                            |
| A20-540621\_46\_GML.zip                 | Error: Not implemented                                                                                                                                                                         |
| A20s-10\_46\_GML.zip                    | Error: The numbers of columns don’t match. expected: 5, actual: 109                                                                                                                            |
| A21-070402\_13\_GML.zip                 | Error: The numbers of columns don’t match. expected: 2, actual: 7                                                                                                                              |
| A21s-10\_13\_GML.zip                    | Error: The numbers of columns don’t match. expected: 5, actual: 108                                                                                                                            |
| A22-16\_17\_GML.zip                     | NA                                                                                                                                                                                             |
| A22-16\_19\_GML.zip                     | NA                                                                                                                                                                                             |
| A22-m-14\_34\_GML.zip                   | Error in nchar(dsn): invalid multibyte string, element 1                                                                                                                                       |
| A22s-10\_10\_GML.zip                    | Error: The numbers of columns don’t match. expected: 3, actual: 113                                                                                                                            |
| A23-16\_31\_GML.zip                     | NA                                                                                                                                                                                             |
| A24-16\_03\_GML.zip                     | NA                                                                                                                                                                                             |
| A25-16\_04\_GML.zip                     | NA                                                                                                                                                                                             |
| A26-10\_40\_GML.zip                     | NA                                                                                                                                                                                             |
| A27-16\_08\_GML.zip                     | NA                                                                                                                                                                                             |
| A28-11\_GML.zip                         | NA                                                                                                                                                                                             |
| A29-19\_07\_GML.zip                     | NA                                                                                                                                                                                             |
| A30a5-11\_5338-jgd\_GML.zip             | NA                                                                                                                                                                                             |
| A30b-11\_GML.zip                        | Error: There are some columns yet to be translated: A30b\_027,A30b\_028,A30b\_029,A30b\_030,A30b\_031,A30b\_032,A30b\_033,A30b\_034                                                            |
| A31-19\_86\_SHP.zip                     | NA                                                                                                                                                                                             |
| A32-16\_15\_GML.zip                     | NA                                                                                                                                                                                             |
| A33-19\_05\_GML.zip                     | NA                                                                                                                                                                                             |
| A34-180316\_GML.zip                     | NA                                                                                                                                                                                             |
| A35a-14\_02\_GML.zip                    | NA                                                                                                                                                                                             |
| A35b-14\_26\_GML.zip                    | NA                                                                                                                                                                                             |
| A35c-14\_01\_GML.zip                    | NA                                                                                                                                                                                             |
| A37-15\_45\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| A38-14\_14\_GML.zip                     | NA                                                                                                                                                                                             |
| A39-15\_GML.zip                         | NA                                                                                                                                                                                             |
| A40-16\_39\_GML.zip                     | NA                                                                                                                                                                                             |
| A42-18\_GML.zip                         | NA                                                                                                                                                                                             |
| A43-18\_GML.zip                         | NA                                                                                                                                                                                             |
| A44-18\_GML.zip                         | NA                                                                                                                                                                                             |
| A45-19\_18\_GML.zip                     | NA                                                                                                                                                                                             |
| C02-14\_GML.zip                         | Error: The numbers of columns don’t match. expected: 14, actual: 2                                                                                                                             |
| C09-06\_GML.zip                         | Error: The numbers of columns don’t match. expected: 12, actual: 10                                                                                                                            |
| C23-06\_32\_GML.zip                     | Error: The numbers of columns don’t match. expected: 6, actual: 7                                                                                                                              |
| C28-19\_GML.zip                         | NA                                                                                                                                                                                             |
| G02-12\_4229-jgd\_GML.zip               | Error: The numbers of columns don’t match. expected: 58, actual: 84                                                                                                                            |
| G04-a-11\_3927-jgd\_GML.zip             | Error: Not implemented                                                                                                                                                                         |
| G04-c-11\_6742-jgd\_GML.zip             | Error: Not implemented                                                                                                                                                                         |
| G04-d-11\_4530-jgd\_GML.zip             | Error: Not implemented                                                                                                                                                                         |
| G08-15\_44\_GML.zip                     | NA                                                                                                                                                                                             |
| L01-20\_30\_GML.zip                     | Error: The numbers of columns don’t match. expected: 55, actual: 130                                                                                                                           |
| L02-20\_33\_GML.zip                     | Error: The numbers of columns don’t match. expected: 53, actual: 128                                                                                                                           |
| L03-a-16\_3622-jgd\_GML.zip             | Error: Not implemented                                                                                                                                                                         |
| L03-b-14\_5536.zip                      | Error: Not implemented                                                                                                                                                                         |
| L03-b-16\_3623-tky\_GML.zip             | Error: Not implemented                                                                                                                                                                         |
| L03-b-c-16\_5440\_GML.zip               | Error: Not implemented                                                                                                                                                                         |
| L03-b-u-16\_3927-jgd\_GML.zip           | Error: Not implemented                                                                                                                                                                         |
| L05-2-09\_33\_GML.zip                   | NA                                                                                                                                                                                             |
| N02-19\_GML.zip                         | NA                                                                                                                                                                                             |
| N03-20200101\_04\_GML.zip               | NA                                                                                                                                                                                             |
| N04-10\_4934-jgd\_GML.zip               | Error: The numbers of columns don’t match. expected: 17, actual: 56                                                                                                                            |
| N05-19\_GML.zip                         | NA                                                                                                                                                                                             |
| N06-19\_GML.zip                         | NA                                                                                                                                                                                             |
| N07-11\_41\_GML.zip                     | NA                                                                                                                                                                                             |
| N08-19\_GML.zip                         | NA                                                                                                                                                                                             |
| N09-12\_GML.zip                         | Error in CPL\_read\_ogr(dsn, layer, query, as.character(options), quiet, : (converted from warning) GDAL Message 1: Value ‘0.14’ of field N09-12\_l.N09\_022 parsed incompletely to integer 0. |
| N10-15\_40\_GML.zip                     | NA                                                                                                                                                                                             |
| N11-13\_39.zip                          | NA                                                                                                                                                                                             |
| P02-06\_38\_GML.zip                     | NA                                                                                                                                                                                             |
| P03-13.zip                              | Error in nchar(dsn): invalid multibyte string, element 1                                                                                                                                       |
| P04-14\_37\_GML.zip                     | NA                                                                                                                                                                                             |
| P05-10\_36\_GML.zip                     | NA                                                                                                                                                                                             |
| P07-15\_35\_GML.zip                     | NA                                                                                                                                                                                             |
| P09-10\_5032-jgd\_GML.zip               | Error: The numbers of columns don’t match. expected: 13, actual: 14                                                                                                                            |
| P11-10\_36\_GML.zip                     | Error: The numbers of columns don’t match. expected: 4, actual: 40                                                                                                                             |
| P12-14\_35\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| P13-11\_34\_GML.zip                     | NA                                                                                                                                                                                             |
| P14-15\_32\_GML.zip                     | NA                                                                                                                                                                                             |
| P15-12\_31\_GML.zip                     | NA                                                                                                                                                                                             |
| P16-12\_32\_GML.zip                     | NA                                                                                                                                                                                             |
| P17-12\_29\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| P18-12\_27\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| P19-12\_30\_GML.zip                     | NA                                                                                                                                                                                             |
| P20-12\_28\_GML.zip                     | Error: There are some columns yet to be translated: レベル,備考,緯度,経度,NO                                                                                                                   |
| P21-12\_27\_GML.zip                     | Error: There are some columns yet to be translated: 検査ID                                                                                                                                     |
| P22-12\_14\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| P23-12\_17\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| P24-12\_GML.zip                         | Error in CPL\_read\_ogr(dsn, layer, query, as.character(options), quiet, : (converted from warning) GDAL Message 1: Value ‘1.0’ of field P24-12.P24\_011 parsed incompletely to integer 1.     |
| P26-13\_34.zip                          | NA                                                                                                                                                                                             |
| P27-13\_38.zip                          | NA                                                                                                                                                                                             |
| P28-13\_41.zip                          | NA                                                                                                                                                                                             |
| P29-13\_47.zip                          | NA                                                                                                                                                                                             |
| P30-13\_47.zip                          | NA                                                                                                                                                                                             |
| P31-13\_46.zip                          | NA                                                                                                                                                                                             |
| P32-14\_45\_GML.zip                     | NA                                                                                                                                                                                             |
| P33-14\_44\_GML.zip                     | Error: Not implemented                                                                                                                                                                         |
| P34-14\_42\_GML.zip                     | NA                                                                                                                                                                                             |
| P35-18\_GML.zip                         | NA                                                                                                                                                                                             |
| S05-a-13\_CHUBU-g.zip                   | Error: Not implemented                                                                                                                                                                         |
| S05-b-13\_CHUBU-g.zip                   | Error: Not implemented                                                                                                                                                                         |
| S05-c-12\_KINKI\_GML.zip                | Error: Not implemented                                                                                                                                                                         |
| S05-d-16\_GML.zip                       | Error: Not implemented                                                                                                                                                                         |
| S10a-16\_GML.zip                        | NA                                                                                                                                                                                             |
| S10b-14\_GML.zip                        | NA                                                                                                                                                                                             |
| S12-19\_GML.zip                         | NA                                                                                                                                                                                             |
| W01-14\_GML.zip                         | NA                                                                                                                                                                                             |
| W05-07\_45\_GML.zip                     | Error: The numbers of columns don’t match. expected: 7, actual: 3                                                                                                                              |
| W07-09\_6841-jgd\_GML.zip               | Error: The numbers of columns don’t match. expected: 5, actual: 6                                                                                                                              |
| W09-05\_GML.zip                         | Error: There are some columns yet to be translated: W09\_001,W09\_002,W09\_003,W09\_004                                                                                                        |
| m1000-17\_39\_GML.zip                   | Error: Not implemented                                                                                                                                                                         |
| m500-17\_14\_GML.zip                    | Error: Not implemented                                                                                                                                                                         |
