
rm(list = ls())
gc(reset = T)

if(!require(dplyr)) install.packages('dplyr'); require(dplyr)
if(!require(data.table)) install.packages('data.table'); require(data.table)
if(!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if(!require(sf)) install.packages('sf'); require(sf)
if(!require(httr)) install.packages('httr'); require(httr)
if(!require(rvest)) install.packages('rvest'); require(rvest)
if(!require(av)) install.packages('av'); require(av)

setwd('/Users/moon/Documents/GitHub/Rtips/map/')

#----  MAC ggplot2 한글 깨짐 방지 ----#
if(!require(extrafont)) install.packages('extrafont'); require(extrafont)
theme_set(theme_gray(base_family='NanumGothic'))
#-------------------------------------#

#### 1. Visualization .SHP file ####
# link: https://data.seoul.go.kr/dataVisual/seoul/seoulLivingPopulation.do
mapp = st_read("행정구역.shp") 
head(mapp)
p = ggplot(mapp) +
  geom_sf(aes(fill = SIGUNGU_NM), colour = 'white') ## 서울시 지도, 자치구별 색상 다르게
p

# 행정구별 코드 정보 http://data.seoul.go.kr/dataVisual/seoul/seoulLivingPopulation.do
sdng_df = fread('sdng_cd.csv') 
sdng_df = sdng_df %>% 
  mutate(H_SDNG_CD = as.character(H_SDNG_CD),
         H_DNG_CD = as.character(H_DNG_CD))


#### 2. 유동인구 API를 활용한 시각화  ####
# link: http://data.seoul.go.kr/dataList/OA-14991/S/1/datasetView.do
get_df_fun = function(date_val_, tz_val_, sdng_df_, api_key_)
{
  url = paste0('http://openapi.seoul.go.kr:8088/', api_key_,
               '/xml/SPOP_LOCAL_RESD_DONG/',
               1, '/',
               500, '/', 
               date_val_, '/',
               tz_val_, '/')
  tmp_lines = readLines(url)
  df = lapply(c('stdr_de_id', 'tmzon_pd_se', 'adstrd_code_se', 'tot_lvpop_co'), function(x)
    read_html(paste0(tmp_lines, collapse = '')) %>%
      html_nodes('body row') %>% html_nodes(x) %>% html_text()) %>% 
    do.call('cbind', .) %>%
    data.frame()
  colnames(df) = c('date', 'tz', 'H_DNG_CD', 'TOT')
  return(df %>% left_join(sdng_df, by = 'H_DNG_CD'))
}

vis_fun = function(mapp, date_val, tz_val, path = getwd(), sdng = sdng_df, api = api_key)
{
  df = get_df_fun(date_val_ = date_val, tz_val_ = tz_val, sdng_df_ = sdng, api_key_ = api)
  vis_df = mapp %>% left_join(df, by = c('ADM_CD' = 'H_SDNG_CD'))
  g = ggplot(vis_df) + geom_sf(aes(fill = as.numeric(TOT))) + 
    scale_fill_viridis_c() + labs(fill = "생활인구")
  
  ggsave(paste0(getwd(), '/spopvis_', date_val, tz_val, '.jpg'), 
         width = 10, height = 8, g)
  
}

## exec 
api_key = '***'
date_val = '20210610'
tz_val = '12'

vis_fun(mapp, date_val, tz_val, sdng = sdng_df, api = api_key)


##### Visualization as Video
tz_seq = as.character(0:23)
tz_seq[1:10] = paste0(0, tz_seq[1:10])

for (i in 1:length(tz_seq))
  vis_fun(mapp, date_val = '20210606', tz_val = tz_seq[i])

png_files = sprintf("spopvis_20210606%s.jpg", tz_seq)
av::av_encode_video(png_files, 'output.mp4', framerate = 2)


