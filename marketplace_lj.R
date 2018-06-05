library(tidyverse)

df <- read_csv('twitter_marketplace_data.csv')
df$campaign_id <- as.factor(df$campaign_id)
df$seen_by_user <- as.logical(df$seen_by_user)

df12 <- df %>% filter(campaign_id %in% c(1,2))

ggplot(data = df12) + 
  geom_point(aes(x = datetime, y=campaign_spend, color=campaign_id))

## campaign spending and duration
df12 %>% group_by(campaign_id) %>%
  summarize(duration = max(datetime) - min(datetime),
            spent = max(campaign_spend),
            budget = first(campaign_budget))

## engament rate
df12 %>% group_by(campaign_id) %>%
  summarise(n_impressions = n(), 
            success_rate = sum(charged!=0)/n(),
            view_rate = sum(seen_by_user)/n(),
            n_views = sum(seen_by_user),
            success_rate_view = sum(charged!=0)/sum(seen_by_user))

# bid and matched targeting
unique(df12[,c('campaign_id', 'bid', 'matched_targeting')])
df12 %>% group_by(campaign_id) %>% 
  summarize(perc_target_na = sum(is.na(matched_targeting)/n()) )

# part two: video vs app-installs
dfav <- df %>% filter(objective!='WEBSITE_CLICKS')
unique(dfav %>% select(campaign_id, bid, matched_targeting,campaign_budget,
                       objective))

ggplot(data = dfav) + 
  geom_point(aes(x = datetime, y=campaign_spend, color=campaign_id))

## campaign spending and duration
dfav %>% group_by(objective) %>%
  summarise(n_impressions = n(), 
            engament_rate = sum(charged!=0)/n(),
            view_rate = sum(seen_by_user)/n(),
            success_rate_view = sum(charged!=0)/sum(seen_by_user))

dfav %>% group_by(campaign_id) %>%
  summarize(duration = round(max(datetime) - min(datetime),2),
            spend = max(campaign_spend),
            budget = first(campaign_budget),
            perc_spent = spend/budget*100,
            bid = first(bid),
            target = first(matched_targeting))


dfav %>% group_by(campaign_id) %>%
  summarise(n_impressions = n(), 
            success_rate = sum(charged!=0)/n(),
            view_rate = sum(seen_by_user)/n())


