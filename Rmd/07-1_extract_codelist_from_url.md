Extract codelist: リンク先に詳細があるタイプ
================

## 方針

コードリストは、2つタイプがある。
そのページに直接列挙されているタイプと、リンク先に詳細があるタイプ。
リンク先に詳細があるタイプは、`html_table()`を使った時点で失われてしまうので、別途収集する。

## データのダウンロード

``` r
library(readr)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(stringr)
library(dplyr, warn.conflicts = FALSE)

datalist_files <- list.files(here::here("data-raw", "datalist"), full.names = TRUE)
names(datalist_files) <- stringr::str_replace(basename(datalist_files), "(?:KsjTmplt-)(.*?)(?:-v.*)?(?:\\.html)", "\\1")

extract_links <- function(f) {
  links <- read_html(f) %>% 
    # href に codelist を含む a タグを取得
    html_elements(xpath = "//td/a[contains(@href, 'codelist')]")
  
  tibble(
    name = html_text2(links),
    url  = html_attr(links, "href")
  ) %>% 
    distinct()
}

links_df <- purrr::map_dfr(datalist_files, extract_links, .id = "id")
```

とりあえずこのコードリストとテキストの対応表は別ファイルに書き出しておく。
あとで紐付けるときに使う。

``` r
links_df %>% 
  mutate(
    codelist_id = tools::file_path_sans_ext(basename(url)),
    .keep = "unused"
  ) %>% 
  readr::write_csv(here::here("data", "codelist_list.csv"))
```

ダウンロードする。

``` r
library(polite)

out_dir <- here::here("data-raw", "codelist")
dir.create(out_dir, showWarnings = FALSE)

links <- unique(links_df$url)
session <- bow("https://nlftp.mlit.go.jp/ksj/", delay = 10)

# URL間違い

#（パスに /jpgis/ が入る）
links[which(str_detect(links, "SectionCd_syuto"))] <- "/ksj/jpgis/codelist/SectionCd_syuto.html"

# 相対パス（../） は動かないので /ksj/gml/ に置き換え
dotdot_idx <- which(str_detect(links, "^../"))
links[dotdot_idx] <- str_replace(links[dotdot_idx], fixed("../"), "/ksj/gml/")

links <- unique(links)

for (l in links) {
  out_file <- file.path(out_dir, basename(l))
  
  # すでにダウンロードしてたら
  if (file.exists(out_file)) {
    message(glue::glue("{l} is already downloaded. Skipped."))
    next
  }

  message(glue::glue("Getting {l}..."))
  
  if (str_ends(l, "\\.html")) {
    nod(session, l) %>% 
      # 念のため、HTMLへの変換をはさまずテキストで受け取ることにする
      scrape(content = "text/plain;charset=UTF-8", verbose = TRUE) %>% 
      brio::write_file(out_file)
  } else {
    # バイナリのものは R のセッションに読み込まずそのままダウンロード
    curl::curl_download(glue::glue("https://nlftp.mlit.go.jp{l}"), destfile = out_file)
  }
}

# 数バイトのファイルはおかしい
codelist_files <- fs::dir_ls(out_dir)
stopifnot(all(fs::file_size(codelist_files) > 10))
```

## 確認

``` r
codelist_files
```

    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AdminAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AdminAreaCd_R105.xlsx
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AdminCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AdminConAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AdminHarborCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AdminSeaLineCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AggUnitFlagEmerTransCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AggregateUnitFlag.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AgriculturalAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AirJetCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AirportCatCd..html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AirportTransitionCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AirportUseCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/AviationActCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/BiomassType.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/BusClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/BusinessTechCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ChukyoAreaZoneCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CityParkCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ClassFishPortCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ClassHarbor1Cd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ClassHarbor2Cd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ClimateCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CodeDesignationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CodeNoncombustibleCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CodeOfPhenomenon.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CodeOfUnSpecification.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CodeOfZone_h27.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/CultureFacCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/DamInstitutionCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/DamPurposeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/DamTypeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/DistributionCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/DistributionCenterCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/EntrepreneurCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/EstClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/FacClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/FacilitiesClassificationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/FishPortAdminCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ForestAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/FuelStoreCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/FurnaceType.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/HeavySnowTypeCode.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/HighwayCatCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/HighwayConCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/HighwayTransitionCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/HighwayUseCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/IndexNumL01-v1_1.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/IndexNumL02-v2_4.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/InstallAdminCd-v2_3.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/InstallAirPortCd-v2_3.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/InstitutionTypeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/KasoCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/KeihanshinAreaStationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/KeihanshinAreaZoneCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/KinkiAreaStationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/KinkiAreaZoneCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseCd-09-u.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseCd-09.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseCd-77.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseCd-88.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseCd-YY.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseProperty-07.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseProperty-09.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseProperty-77.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseProperty-88.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LandUseProperty-92_98.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LargeClassificationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/LocationAccuracyCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/MaritimeOrgCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/MedClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/NaturalParkAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/NaturalfeatureCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/NaturalsceneCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/NatureConservationAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/OriginalDataCodeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PTAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PointClassificationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PoliceStationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PortRouteCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PosSpecificLevel.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PrefCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PrefCdA33.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubFacAdminCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubFacMaclassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubFacMiclassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubFacMiclassCd_wf.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubFacMinclassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubOfficeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/PubOfficeClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RailwayClass2Cd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RailwayClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RailwayDuplicateCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RailwayExistenceCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RailwayTransitionCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ReferenceDataCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RegularFlightCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ResearchInstitutionCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RiverCodeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/RoadCategoryCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SchoolClassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SectionCd_cyubu.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SectionCd_kinki.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SectionCd_syuto.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SectionTypeCd_cyubu.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SectionTypeCd_kinki.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SectionTypeCd_syuto.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SedimentDisastersProneAreaCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SelectLandStatus-v1_1.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SelectLandStatus-v2_4.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SettingFlag.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SmallClassificationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/SubprefectureNameCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/ThermalPowerEngine.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/TokusyudojyoCd-v3_0.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/TokyoAreaStationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/TokyoAreaZoneCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/TripGenerationCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/UrbanPlanningAreaCd_2019.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/UrgentRoadCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/UseDistrictCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/WaterSystemCodeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/WelfareFacMiclassCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/WorldHeritageCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/communityCenterType.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/facilityTypeCode.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/flood_duration_code.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/hazardous_area_classification_code.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/hoanrinCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/hogorinCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/jushuCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/kinouruikeiCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/midorinokairoCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/pointClassificationCode.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/postOfficeCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/rinshunosaibunCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/shinrinkanriCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/shouhanshubanCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/tourismResourceCategoryCd.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/transition_use.pdf
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/underConstruction.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/useDistrict.html
    ## /home/yutani/repo/kokudosuuchi-metadata/data-raw/codelist/water_depth_code.html
