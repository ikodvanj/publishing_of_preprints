library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(stringdist)
library(devEMF)
library(ggpubr)
library(survival)
library(ggsurvplot)
library(ggsci)
library(logbin)
library(survminer)

customdatefloor <- function(mindate, n){
  counter <- 1
  x <- data.frame()
  start_date <- mindate
  biweek <- 1
  for(i in 0:(n-1)){
    x <- bind_rows(x, data.frame(date = start_date, biweek = biweek))
    if(counter == 15){
      counter <- 0
      biweek <- biweek + 1
    } 
    counter <- counter + 1
    start_date <- start_date + 1
  }
  return(x)
} 



theme_set(theme_bw() + theme( plot.subtitle = element_text(vjust = 1), 
                              plot.caption = element_text(vjust = 1), 
                              panel.grid.major = element_line(colour = "gray97"), 
                              panel.grid.minor = element_line(size = 0.25), 
                              axis.title = element_text(size = 9), 
                              axis.text = element_text(size = 8, colour = "black"), 
                              panel.background = element_rect(fill = NA)))
colors <- scale_color_nejm()
fill <- scale_fill_nejm()


## LOADING DATA ##############################################################################
df <- readRDS("data.RDS")
usage <- readRDS("usage.rds")

## USAGE & DEPOSITING OF PREPRINTS ###########################################################
usage <- usage %>%
  as_tibble() %>%
  tidyr::gather(category, value, - month) %>%
  mutate(category = recode(category, abstract_cumulative = "Abstract cumulative",
         abstract_views = "Abstract views", full_text_cumulative = "Full-text cumulative",
         full_text_views = "Full-text views", pdf_cumulative = "PDF cumulative",
         pdf_downloads = "PDF downloads"))

p1 <- df %>%
  mutate(Date_month = cut(as.Date(firstdate), "week")) %>%
  ggplot(aes(x = as.Date(Date_month), fill = covid)) + geom_bar() +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y", limits = c(ymd("2020-01-01"), ymd("2020-12-01"))) + 
  xlab("Date") + ylab("Number of preprints") +
  labs(tag = "A") + facet_wrap(~db) + scale_fill_nejm() + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", legend.position = "top")

p2 <- usage %>%
  mutate(month == ymd(month)) %>%
  filter(month >= ymd("2020-01-01"), month < ymd("2020-12-01")) %>%
  filter(category %in% c("Abstract views", "Full-text views", "PDF downloads"))  %>%
  ggplot(aes(x = as.Date(month), y = value, fill = "one")) + geom_col() +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 months",
               date_labels = "%b\n%Y") + xlab("Date") + ylab("Count") +
  labs(tag = "B") + facet_wrap(~ category, scales = "free") +
  theme(legend.position = "none") + scale_fill_manual(values = c("#20854f"))

ggarrange(p1,p2, ncol = 1)
emf("Figure3.emf", height = 7, width = 8.8)
print(ggarrange(p1,p2, ncol = 1))
dev.off()

##############################################################################################


##### Disqus ##### ##### 
df$has_post <- "No comments"
df$has_post[df$Disqus > 0] <- "Has comments"

table(df$has_post)

pd <- df %>% ggplot(aes(x = covid, fill = has_post)) + geom_bar(position = "fill") + facet_wrap(~ db) + fill + theme_bw() + 
  labs(tag = "A") + theme(legend.position = "top", legend.title = element_blank(), legend.direction = "horizontal") +
  xlab("")+ylab("Proportion of articles")

df$prop <- "0"
df$prop[df$Disqus == 2] <- "2"
df$prop[df$Disqus == 1] <- "1"
df$prop[df$Disqus] <- "3+"
df$prop[df$Disqus > 5] <- "5+"
df$prop[df$Disqus > 10] <- "10+"


pd2 <- df %>% group_by(prop) %>%
  summarise(count = n()) %>%
  mutate(perc = count / dim(df)[1]) %>%
  filter(prop != "0") %>%
  ggplot(aes(x = factor(prop, levels = c("0","1", "2", "3+", "5+", "10+")), y = count, fill = "1")) + geom_col() +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) + fill + 
  ylab("Preprint count") + xlab("Number of comments") + labs(tag = "B") + theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~. /44496,labels = scales::percent,name = "Percentage of preprints (%)"))
  


### Altmetric #####
alt01 <- df %>% select(colnames(df)[str_detect(colnames(df), "cited")])
alt01 <- naniar::as_shadow(alt01)

pa1 <- alt01 %>% 
  select(-cited_by_posts_count_NA, -cited_by_accounts_count_NA) %>% 
  tidyr::gather(key, value) %>%
  mutate(key = recode(key, cited_by_tweeters_count_NA = "Twitter",
                      cited_by_fbwalls_count_NA = "Facebook", 
                      cited_by_msm_count_NA = "News", 
                      cited_by_feeds_count_NA = "Blogs",
                      cited_by_wikipedia_count_NA = "Wikipedia",
                      cited_by_videos_count_NA = "YouTube",
                      cited_by_rh_count_NA = "Research\nhighlight platforms",
                      cited_by_rdts_count_NA = "Reddit",
                      cited_by_qna_count_NA = "QNA",
                      cited_by_policies_count_NA = "Policies",
                      cited_by_peer_review_sites_count_NA = "Peer review sites"),
         value = recode(value, `!NA` = "Mentioned", `NA` = "Not Mentioned")) %>%
  ggplot(aes(x = key, fill = as.factor(value))) + geom_bar(position = "fill") + coord_flip() + fill +
  xlab("") + ylab("") + labs(tag = "D") + theme(legend.position = "top", legend.title = element_blank(), legend.direction = "horizontal")



pa2 <- df %>% 
  ggplot(aes(x = covid, y = score, fill = db)) + 
  geom_violin(position = position_dodge(width = 1), alpha = 0.6, color = "transparent")  + 
  geom_boxplot(width=.1, position = position_dodge(width = 1), alpha = 1)+ 
  scale_y_log10() + fill + xlab("") + ylab("Altmetric score") + labs(tag = "C") + 
  theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal")



pa3 <- df %>% 
  select(covid, db, cited_by_tweeters_count, cited_by_fbwalls_count, cited_by_msm_count, cited_by_feeds_count) %>%
  tidyr::gather(key, value, -covid, -db) %>%
  mutate(key = recode(key, cited_by_tweeters_count = "Cited by tweeters",
                           cited_by_fbwalls_count = "Cited by facebook users", 
                           cited_by_msm_count = "Cited by news outlets", 
                           cited_by_feeds_count = "Cited by blogs")) %>%
  ggplot(aes(x = covid, y = value, fill = db)) + 
  geom_violin(position = position_dodge(width = 1), alpha = 0.6, color = "transparent")  + 
  geom_boxplot(width=.1, position = position_dodge(width = 1), alpha = 1)+ 
  scale_y_log10() + fill + facet_wrap(~ key, scales = "free") + xlab("") + ylab("Counts") + labs(tag = "E") + 
  theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal")


p4full <- ggarrange(
  ggarrange(pd, pd2),
  ggarrange(pa2, pa1),
  pa3,
  nrow = 3,
  heights = c(0.8,1,1.15)
)
emf("Figure4.emf", height = 14, width = 10)
print(p4full)
dev.off()

##############################################################################################

## Summary
df %>% summarise(min(firstdate), max(firstdate), n())
df %>% group_by(db) %>% summarise(n())
df %>% group_by(db) %>% summarise(n = n()) %>% mutate(sum = sum(n), perc = n / sum * 100)
df %>% group_by(db, covid) %>% summarise(n = n()) %>% mutate(sum = sum(n), perc = n / sum * 100)



### Primary objective #######################################################################
## Followup is till 2020-11-01. For primary measure outcome, preprints are included till 2020-06-29;
## for secondary outcome measure, preprints are included till 2020-09-27
df$peer_reviewed[df$published_date > ymd("2020-11-01")] <- "Not peer-reviewed" 
df$published_date[df$peer_reviewed == "Not peer-reviewed"] <- ymd("2020-11-01") 

table(is.na(df$published_date))

df <- df[!is.na(df$published_date),]

df$firstpub = df$published_date - df$firstdate 
df$lastpub = df$published_date - df$lastdate

dim(df)
df <- df %>%
  filter(firstpub >= 0, lastpub >= 0) %>%
  mutate(month = month(firstdate))
dim(df)


modeldata <- df %>% select(doi, db, peer_reviewed, covid, firstdate, lastdate, 
                           published_date, month, published, maxver, InReview, YearReceived)
biweeks <- customdatefloor(ymd("2020-01-01"), n = 365)
biweeks$biweek <- biweeks$biweek - 1
biweeks$biweek[biweeks$biweek == 0] <- 1

modeldata <- left_join(modeldata, biweeks, by = c("firstdate"="date"))
modeldata$diff <- modeldata$published_date - modeldata$firstdate
modeldata$covid <- factor(modeldata$covid)
modeldata$peer_reviewed <- factor(modeldata$peer_reviewed)

modeldata$covid <- forcats::fct_rev(modeldata$covid)
modeldata$db <- forcats::fct_rev(modeldata$db)

modeldata$maxversion  <- "1"
modeldata$maxversion[modeldata$maxver > 1]  <- "2+"

## 2. outcome measure
modeldata2 <- modeldata
modeldata2$status <- 0
modeldata2$status[modeldata2$peer_reviewed == "Peer-reviewed"] <- 1
modeldata2 <- modeldata2[modeldata2$firstdate > ymd("2020-01-01") & modeldata2$firstdate < ymd("2020-09-27"),] 

model2s <- clogit(status ~ covid + db + maxversion + strata(biweek), data = modeldata2, method = "approximate")
sjPlot::tab_model(model2s)

# summary of a dataset used for secondary outcome measure 
modeldata2 %>% group_by(db) %>% summarise(n())
modeldata2 %>% group_by(db, covid) %>% summarise(n())
modeldata2 %>%
  group_by(db, covid) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n), freq = n / sum * 100) %>% as.data.frame()
modeldata2 %>% group_by(db, covid, status) %>% summarise(n())
modeldata2 %>% group_by(db, status) %>% summarise(n = n()) %>%
  ungroup() %>% group_by(db) %>% mutate(sum = sum(n), pct = n / sum)
modeldata2 %>% group_by(db, covid, status) %>% summarise(n = n()) %>%
  ungroup() %>% group_by(covid, db) %>% mutate(sum = sum(n), pct = n / sum)


## 1. outcome measure
modeldata1 <- modeldata
modeldata1 <- modeldata1[modeldata1$firstdate < ymd("2020-06-29"),] 
modeldata1$status <- 0
modeldata1$status[modeldata1$peer_reviewed == "Peer-reviewed"] <- 1
modeldata1$status[as.numeric(modeldata1$diff) > 120] <- 0

model1s <- clogit(status ~ covid + db + maxversion + strata(biweek), data = modeldata1, method = "approximate")
summary(model1s)
sjPlot::tab_model(model1s)


# summary of a dataset used for primary outcome measure 
modeldata1 %>% group_by(db) %>% summarise(n())
modeldata1 %>% group_by(db, covid) %>% summarise(n())
modeldata1 %>%
  group_by(db, covid) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n), freq = n / sum * 100) %>% as.data.frame()
modeldata1 %>% group_by(db, covid, status) %>% summarise(n())
modeldata1 %>% group_by(db, status) %>% summarise(n = n()) %>%
  ungroup() %>% group_by(db) %>% mutate(sum = sum(n), pct = n / sum)
modeldata1 %>% group_by(db, covid, status) %>% summarise(n = n()) %>%
  ungroup() %>% group_by(covid, db) %>% mutate(sum = sum(n), pct = n / sum)




## 3. outcome measure
lmdata2 <- modeldata2 %>% filter(peer_reviewed == "Peer-reviewed")
lmdata2 %>% group_by(peer_reviewed, db, covid) %>% summarise(n()) ## SUMMARY 
lmdata2 <- lmdata2[!is.na(lmdata2$InReview),]
lmdata2 %>% group_by(peer_reviewed, db, covid) %>% summarise(n()) 
lmdata2 <- lmdata2 %>% filter(YearReceived == 2020)
lmdata2 %>% group_by(peer_reviewed, db, covid) %>% summarise(n(), median(InReview), min(InReview), max(InReview)) ## SUMMARY 
lmdata2 <- lmdata2[!(lmdata2$InReview %in% c(-1)),]
lmdata2 %>% group_by(peer_reviewed, db, covid) %>% summarise(n(), median(InReview), min(InReview), max(InReview)) ## SUMMARY 


lmerm <- lme4::lmer(as.numeric(InReview) ~ covid + db + maxversion + (1 | biweek), data = lmdata2)
sjPlot::tab_model(lmerm)

# set.seed(19)
# sample <- sample(1:dim(lmdata2)[1], 100)
# sample <- lmdata2[sample,"published"]
# saveRDS(sample, "sample.RDS")
## For article under no. 52 submission and acceptance time could not be identified.
## For article under no. 90, acceptance time is off by 1 day.

## Retractions 
# cov <- RISmed::EUtilsSummary('(Retracted Publication[PT]) AND (COVID-19 OR SARS-CoV-2 OR "Coronavirus disease 2019" OR 2019-nCoV) AND ("2020/01/01"[Date - Publication] : "2020/12/05"[Date - Publication])')
# ctr <- RISmed::EUtilsSummary('(Retracted Publication[PT]) AND ("2020/01/01"[Date - Publication] : "2020/12/05"[Date - Publication]) NOT (COVID-19 OR SARS-CoV-2 OR "Coronavirus disease 2019" OR 2019-nCoV) ')
# covtot <- RISmed::EUtilsSummary('(COVID-19 OR SARS-CoV-2 OR "Coronavirus disease 2019" OR 2019-nCoV) AND ("2020/01/01"[Date - Publication] : "2020/12/05"[Date - Publication])')
# ctrtot <- RISmed::EUtilsSummary('("2020/01/01"[Date - Publication] : "2020/12/05"[Date - Publication]) NOT (COVID-19 OR SARS-CoV-2 OR "Coronavirus disease 2019") OR 2019-nCoV')
# 
# m <- matrix(c(cov@count,covtot@count-cov@count,ctr@count,ctrtot@count-ctr@count), nrow = 2)
# colnames(m)<-c("COVID", "NON COVID")
# rownames(m)<-c("retracted", "not retracted")
# 
# saveRDS(m, file = "pubmed_retracted.rds")
m <- readRDS("pubmed_retracted.rds")
m[1,] / (m[1,] + m[2,]) * 1000

