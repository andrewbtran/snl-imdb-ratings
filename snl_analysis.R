library(dplyr)
library(ggplot2)
rankings <- read.csv("ratings_raw.csv", stringsAsFactors=F)

rankings$year <- gsub("\\..*", "", rankings$ep)
rankings$year <- as.numeric(rankings$year) +1974
rankings$avg_vote <- as.numeric(rankings$avg_vote)

rankings_year <- rankings %>%
  group_by(year) %>%
  summarise(Avg=mean(avg_vote, na.rm=T))

ggplot(rankings_year, aes(x=year, y=Avg)) + geom_line(stat="identity")

rankings$votes <- as.numeric(rankings$votes)
rankings$rating <- as.numeric(rankings$rating)

rankings_year_cat <- rankings %>%
  mutate(rawish=votes*rating) %>%
  group_by(year, category) %>%
  summarise(Avg=sum(rawish, na.rm=T)/sum(votes, na.rm=T)) 

rankings_year_cat$college_start <- 0
rankings_year_cat$college_end <- 0

rankings_year_cat$college_start <- ifelse(grepl("under", rankings_year_cat$category), 2015, rankings_year_cat$college_start)
rankings_year_cat$college_end <- ifelse(grepl("under", rankings_year_cat$category), 2016, rankings_year_cat$college_end)
rankings_year_cat$college_start <- ifelse(grepl("29", rankings_year_cat$category), 2005, rankings_year_cat$college_start)
rankings_year_cat$college_end <- ifelse(grepl("29", rankings_year_cat$category), 2015, rankings_year_cat$college_end)
rankings_year_cat$college_start <- ifelse(grepl("30", rankings_year_cat$category), 1990, rankings_year_cat$college_start)
rankings_year_cat$college_end <- ifelse(grepl("30", rankings_year_cat$category), 2004, rankings_year_cat$college_end)
rankings_year_cat$college_start <- ifelse(grepl("\\+", rankings_year_cat$category), 1975, rankings_year_cat$college_start)
rankings_year_cat$college_end <- ifelse(grepl("\\+", rankings_year_cat$category), 1989, rankings_year_cat$college_end)

rankings_year_cat_aged <- rankings_year_cat %>%
#  filter(category=="Aged 18-29" | category=="Aged 30-44" | category=="Aged 45+" | category=="Aged under 18")
   filter(category=="Aged 18-29" | category=="Aged 30-44" | category=="Aged 45+")
  
gg <- ggplot(rankings_year_cat_aged)
gg <- gg + geom_rect(data=data.frame(category="Aged 18-29"), aes(ymin = -Inf, ymax = Inf, 
                                                 xmin = 2005, xmax = 2015), alpha=0.5, fill = 'grey')
gg <- gg + geom_rect(data=data.frame(category="Aged 30-44"), aes(ymin = -Inf, ymax = Inf, 
                                                                 xmin = 1990, xmax = 2005), alpha=0.5, fill = 'grey')
gg <- gg + geom_rect(data=data.frame(category="Aged 45+"), aes(ymin = -Inf, ymax = Inf, 
                                                                 xmin = 1975, xmax = 1990), alpha=0.5, fill = 'grey')
#gg <- gg + geom_rect(data=data.frame(category="Aged under 18"), aes(ymin = -Inf, ymax = Inf, 
#                                                                 xmin = 2015, xmax = 2016), alpha=0.5, fill = 'grey')
gg <- gg + geom_line(aes(year, Avg, group=category, color=category)) 
gg <- gg + facet_wrap(~category, ncol=3, scales = "free_x")
gg <- gg + labs(x=NULL, y="Average IMDB user rating", title="When Saturday Night Live was last funny",
                subtitle="It depends on when viewers were in college. Groups rated the \"best\" season in the timeframe they were college-aged.",
                caption="SOURCE: IMDB \nAndrew Ba Tran/TrendCT.org")
gg <- gg + theme_bw(base_family="Lato Regular")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=9, color="gray28", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(strip.text.x = element_text(size = 8, colour = "orange"))



gg
