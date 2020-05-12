setwd("/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles")

library(rvest)
library(readr)
library(dplyr)
library(RCurl)
library(tidyr)
library(stringr)
library(feather)

read_html_cache <- function(url, cache.dir = 'cache') {
  fn <- tail(strsplit(url, '/')[[1]], 1)
  fn.path <- paste(cache.dir, fn, sep = '/')
  if (!file.exists(fn.path)) {
    text <- getURL(url)
    write(text, fn.path)
  }
  read_html(fn.path)
}

draft.header <- c('rk',	'pk',	'tm', 'player',	'college',	'yrs', 'g',	'mp',	'pts',	'trb',	'ast',	'fg_pct',	'fg3_pct',	'ft_pct',	'mp_per_g',	'pts_per_g',	'trb_per_g',	'ast_per_g',	'ws',	'ws_per_48',	'bpm',	'vorp')
#draft2.header <- c('rk',	'pk',	'tm', 'player',	'college',	'yrs', 'g',	'mp',	'pts',	'trb',	'ast',	'fg_pct',	'fg3_pct',	'ft_pct',	'mp_per_g',	'pts_per_g',	'trb_per_g',	'ast_per_g',	'ws',	'ws_per_48',	'bpm',	'vorp')
#ncaa.header <- c('player', 'school')
salary.header <- c('Pick', '1st Year', '2nd Year', '3rd Year', '4th Year', 'Qualifying Offer')


url.extract <- function(tds) {
  results <- c()
  for(td in tds) {
    children <- html_children(td)
    if (length(children) == 0) {
      results <- c(results, NA)
    } else{
      results <- c(results, (html_attr(html_children(td), 'href')))
    }
  }
  results
}

headers <- list()
headers[['pergame']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp_per_g', 'fg_per_g', 'fga_per_g',	'fg_pct',	'fg2_per_g',	'fg2a_per_g',	'fg2_pct', 'fg3_per_g',	'fg3a_per_g',	'fg3_pct',	'ft_per_g',	'fta_per_g',	'ft_pct', 'orb_per_g', 'drb_perg',	'trb_per_g',	'ast_per_g',	'stl_per_g',	'blk_per_g',	'tov_per_g', 'pf_per_g',	'pts_per_g', 'sos-dum', 'sos')
headers[['totals']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp', 'fg', 'fga',	'fg_pct',	'fg2',	'fg2a',	'fg2_pct', 'fg3',	'fg3a',	'fg3_pct',	'ft',	'fta',	'ft_pct',	'trb',	'ast',	'stl',	'blk',	'tov', 'pf',	'pts')
headers[['per40']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp', 'fg_per_min', 'fga_per_min',	'fg_pct',	'fg2_per_min',	'fg2a_per_min',	'fg2_pct', 'fg3_per_min',	'fg3a_per_min',	'fg3_pct',	'ft_per_min',	'fta_per_min',	'ft_pct',	'trb_per_min',	'ast_per_min',	'stl_per_min',	'blk_per_min',	'tov_per_min', 'pf_per_min',	'pts_per_min')
headers[['per100']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp', 'fg_per_poss', 'fga_per_poss',	'fg_pct',	'fg2_per_poss',	'fg2a_per_poss',	'fg2_pct', 'fg3_per_poss',	'fg3a_per_poss',	'fg3_pct',	'ft_per_poss',	'fta_per_poss',	'ft_pct',	'trb_per_min',	'ast_per_poss',	'stl_per_poss',	'blk_per_poss',	'tov_per_poss', 'pf_per_poss',	'pts_per_poss', 'poss-dum', 'off_rtg', 'def_rtg')
headers[['advanced']] <- c('season', 'school_name', 'conf_abbr', 'g', 'gs', 'mp', 'per', 'ts_pct', 'efg_pct', 'fg3a_per_fga_pct', 'fta_per_fta_pct', 'orb_pct', 'drb_pct', 'trb_pct', 'ast_pct', 'ast_pct', 'stl_pct', 'blk_pct', 'tov_pct', 'usg_pct', 'ows', 'dws', 'ws', 'ws_per_40', 'bpm-dum', 'obpm', 'dbpm', 'bpm')
headers[['confrenceperg']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp_per_g', 'fg_per_g', 'fga_per_g',	'fg_pct',	'fg2_per_g',	'fg2a_per_g',	'fg2_pct', 'fg3_per_g',	'fg3a_per_g',	'fg3_pct',	'ft_per_g',	'fta_per_g',	'ft_pct', 'orb_per_g', 'drb_perg',	'trb_per_g',	'ast_per_g',	'stl_per_g',	'blk_per_g',	'tov_per_g', 'pf_per_g',	'pts_per_g')
headers[['confrencetotals']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp', 'fg', 'fga',	'fg_pct',	'fg2',	'fg2a',	'fg2_pct', 'fg3',	'fg3a',	'fg3_pct',	'ft',	'fta',	'ft_pct',	'trb',	'ast',	'stl',	'blk',	'tov', 'pf',	'pts')
headers[['confrenceper40']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp', 'fg_per_min', 'fga_per_min',	'fg_pct',	'fg2_per_min',	'fg2a_per_min',	'fg2_pct', 'fg3_per_min',	'fg3a_per_min',	'fg3_pct',	'ft_per_min',	'fta_per_min',	'ft_pct',	'trb_per_min',	'ast_per_min',	'stl_per_min',	'blk_per_min',	'tov_per_min', 'pf_per_min',	'pts_per_min')
headers[['confrenceper100']] <- c('season',	'school_name',	'conf_abbr',	'g',	'gs', 'mp', 'fg_per_poss', 'fga_per_poss',	'fg_pct',	'fg2_per_poss',	'fg2a_per_poss',	'fg2_pct', 'fg3_per_poss',	'fg3a_per_poss',	'fg3_pct',	'ft_per_poss',	'fta_per_poss',	'ft_pct',	'trb_per_min',	'ast_per_poss',	'stl_per_poss',	'blk_per_poss',	'tov_per_poss', 'pf_per_poss',	'pts_per_poss', 'poss-dum', 'off_rtg', 'def_rtg')
headers[['confrenceadvanced']] <- c('season', 'school_name', 'conf_abbr', 'g', 'gs', 'mp', 'per', 'ts_pct', 'efg_pct', 'fg3a_per_fga_pct', 'fta_per_fta_pct', 'orb_pct', 'drb_pct', 'trb_pct', 'ast_pct', 'ast_pct', 'stl_pct', 'blk_pct', 'tov_pct', 'usg_pct', 'ows', 'dws', 'ws', 'ws_per_40', 'bpm-dum', 'obpm', 'dbpm', 'bpm')

parse_pfr_tables <- function(tables) {
  results = list()
  for (tbl in tables) {
    id <- html_attr(tbl, 'id')
    if (id %in% names(headers)) {
      
      df <- html_table(tbl) %>%
        head(-1) %>% tail(-1)
      
      if(ncol(df) == length(headers[[id]])) {
        colnames(df) <- headers[[id]]
      } else {
        next;
      }
      
      melted <- df %>%
        select(-season, -school, -conf_abbr, -rk) %>%
        mutate(seasons = 1) %>%
        gather(stat, value) %>%
        mutate(stat = as.character(stat)) %>%
        filter(value != '') %>%
        mutate(value = as.numeric(value),
               section = id)
      
      
      results[[id]] <- melted
    }
  }
  bind_rows(results)
}

if (!file.exists('/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles/draft.feather')) {
  
  draft.table <- data_frame(year = 2000:2019) %>%
    group_by(year) %>% do({
      url <- paste('https://www.basketball-reference.com/draft/NBA_', .$year,'.html', sep ='')
      doc <- read_html(url)
      html.table <- doc %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(29)') %>%
        url.extract
      my.table <- html_table(html.table)
      colnames(my.table) <- draft.header
      my.table <- my.table %>%
        filter(pk != 'pk') %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup
  write_feather(draft.table, '/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles/draft.feather')
  
}

#if (!file.exists('/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles/salary.feather')) {
  
 # salary.table <- data_frame(year = 2000:2019) %>%
  #  group_by(year) %>% do({
   #   url <- paste('https://basketball.realgm.com/nba/info/rookie_scale/', .$year, sep ='')
    #  doc <- read_html(url)
     # html.table <- doc %>%
      #  html_nodes('table') %>%
       # first
#      urls <- html.table %>%
 #       html_nodes('tr td:nth-child(29)') %>%
  #      url.extract
   #   my.table <- html_table(html.table)
    #  colnames(my.table) <- salary.header
     # my.table <- my.table %>%
      #  filter(Pick != 'Pick') %>%
       # mutate(url = urls)
#      my.table
 #   }) %>%
  #  ungroup
#  write_feather(salary.table, '/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles/salary.feather')
  
#}
#write.csv(file = "salary.csv", salary.table)




#all.urls <- draft2.table %>%
 # select(url) %>%
  #full_join(draft.table %>% select(url)) %>%
  #filter(!is.na(url))

#college.stats <- draft.table %>%
 # group_by(url) %>% do({
   # doc <- read_html_cache(.$url)
    #stats <- doc %>%
#      html_nodes('table') %>%
 #     parse_pfr_tables
  #  if (nrow(stats) > 0){
   #   stats <- stats %>%
    #    group_by(section, stat) %>%
     #   summarise(value = sum(value))
    #}
    #stats
  #})

#write_feather(college.stats, '/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles/college_stats.feather')



