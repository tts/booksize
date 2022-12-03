library(httr)
library(tidyverse)

# List of possible field[] values to return
# https://www.kiwi.fi/display/Finna/Finnan+avoin+rajapinta#Finnanavoinrajapinta-Tuetutkent%C3%A4t
#
# List of possible filter[] values in the query (column Core Fields)
# https://www.kiwi.fi/display/Finna/Kenttien+mappaukset+eri+formaateista+Finnan+indeksiin

base <- "https://api.finna.fi/api/v1/search"
search <- "?lookfor=major_genre_str_mv:fiction"
fields <- "&field[]=languages&field[]=publicationDates&field[]=physicalDescriptions&field[]=originalLanguages&field[]=classifications"
filters <- '&filter[]=format:"0/Book/"&filter[]=language:"fin"&filter[]=publishDate:'
sort <- "&sort=relevance,id asc"
other <- "&prettyPrint=false&lng=fi"

years <- as.character(seq(2000, 2022))

# Book count by year
allr <- map(years, ~{
  print(paste0("Fetching year ", .x))
  q <- utils::URLencode(paste0(base, search, fields, filters, .x, 
                               sort, "&page=1&limit=0", other))
  r <- httr::GET(q) 
  content(r, as = "parsed")
})

counts <- map_df(allr, ~{
  magrittr::extract(.x, "resultCount")
})

counts <- counts %>% 
  mutate(year = years,
         pagemax = ceiling(resultCount/100)) %>%
  rename(count = resultCount) %>% 
  select(year, count, pagemax)

write.csv(counts, "bookcounts.csv", row.names = FALSE)

# Records by year
recs <- map_dfr(.x = years, ~{
  pmax <- counts[counts$year == .x , "pagemax"]
  year <- .x
  map(seq(1:pmax), ~{
    print(paste0("Fetching year ", year, " page ", .x, "/", pmax))
    q <- utils::URLencode(paste0(base, search, fields, filters, year,
                                 sort, "&page=", .x, "&limit=100", other))
    r <- httr::GET(q)
    cont <- content(r, as = "text")
    jsonlite::fromJSON(cont, flatten = TRUE)
  })
})

saveRDS(recs, "recs2000-2022.RDS")

recs <- recs %>%
  pull(2)

# Clean
data <- recs %>% 
  filter(lengths(physicalDescriptions) > 0) %>% 
  mutate(pages = gsub("([0-9]+) [a-z].*", "\\1", physicalDescriptions)) %>% 
  filter(!grepl('\\[|c(")|s.|\\(|\\.|verkkoaineisto|DVD', pages)) %>% 
  filter(!pages %in% c("0", "A3", "kuv", "Kuv", "KUV", "nid")) %>%
  mutate(pages = as.numeric(pages),
         year = as.numeric(publicationDates)) %>% 
  filter(pages < 2000) %>% # max is 1521, bigger (3 items) are bogus values
  filter(year <= 2022 & year >= 2000) %>% 
  select(year, pages) 

saveRDS(data, "recs2000-2022_cleaned.RDS")

p <- ggplot(data, aes(year, pages))
p + geom_jitter(aes(alpha = 0.3)) + geom_smooth() +
  theme_minimal() + 
  theme(axis.title = element_blank(), legend.position = "none") +
  labs(title = "Average size of fiction in Finnish (pages)",
       subtitle = "Includes also children's fiction",
       caption = "Data by Finna API | @ttso")

ggsave("pages2000-2022.png", device = "png",
       dpi = 320, height = 12, width = 9)

