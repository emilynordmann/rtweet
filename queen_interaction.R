library(rtweet)
library(tidyverse)
library(gridExtra)

queens <- c("manila", "trinity", "naomi", "monique", "latrice",
            "monet", "gia", "farrah", "jasmine")
handles <- c("manilaluzon", "TrinityTheTuck", "naomismallsduh", "IAmMoniqueHeart", "LatriceRoyale",
            "monetxchange","GiaGunn", "farrahrized", "Jasminejmasters")
ids <- c(190371395,780412420527382529, NA, 3317220703, 966380856972541953, 407042279, 3129181760, 594542811,
        780931796502323201, 1067913956864315392)

manila <- get_timeline(user = "manilaluzon", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
trinity <- get_timeline(user = "TrinityTheTuck", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
naomi <- get_timeline(user = "naomismallsduh", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
monique <- get_timeline(user = "IAmMoniqueHeart", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
latrice <- get_timeline(user = "LatriceRoyale", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
monet <- get_timeline(user = "monetxchange", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
gia <- get_timeline(user = "GiaGunn", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
farrah <- get_timeline(user = "farrahrized", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)
jasmine <- get_timeline(user = "Jasminejmasters", n = 3000)%>%
  select(created_at, screen_name, text, reply_to_screen_name, mentions_screen_name)

# filter responses to only include tweets where there is a response to a queen or that mentions a queen

manila_mentions <- data.frame(as.factor(unlist(manila$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.manila.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "manilaluzon")%>%
  mutate(queen = "manila")

trinity_mentions <- data.frame(as.factor(unlist(trinity$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.trinity.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "TrinityTheTuck")%>%
  mutate(queen = "trinity")

naomi_mentions <- data.frame(as.factor(unlist(naomi$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.naomi.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "naomismallsduh")%>%
  mutate(queen = "naomi")

monique_mentions <- data.frame(as.factor(unlist(monique$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.monique.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "IAmMoniqueHeart")%>%
  mutate(queen = "monique")

latrice_mentions <- data.frame(as.factor(unlist(latrice$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.latrice.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "LatriceRoyale")%>%
  mutate(queen = "latrice")

monet_mentions <- data.frame(as.factor(unlist(monet$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.monet.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "monetxchange")%>%
  mutate(queen = "monet")

gia_mentions <- data.frame(as.factor(unlist(gia$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.gia.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "GiaGunn")%>%
  mutate(queen = "gia")

farrah_mentions <- data.frame(as.factor(unlist(farrah$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.farrah.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "farrahrized")%>%
  mutate(queen = "farrah")

jasmine_mentions <- data.frame(as.factor(unlist(jasmine$mentions_screen_name)))%>%
  rename(mentions = as.factor.unlist.jasmine.mentions_screen_name..)%>%
  filter(mentions %in% handles)%>%
  filter(mentions != "Jasminejmasters")%>%
  mutate(queen = "jasmine")


mentions <- bind_rows(manila_mentions,trinity_mentions,naomi_mentions,monique_mentions,latrice_mentions,
                      monet_mentions,gia_mentions, farrah_mentions, jasmine_mentions)

mentions %>%
  group_by(queen)%>%
  count(mentions, sort = TRUE) %>%
  ggplot(aes(x = mentions, y = n, fill = mentions))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~queen)

tbl <- mentions %>%
  group_by(queen)%>%
  count(mentions, sort = TRUE)%>%
  spread(mentions, n)
grid.table(tbl)





