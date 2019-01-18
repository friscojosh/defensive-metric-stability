### -----------------------------------------------------------------------------------------
### Testing the year to year stability of QB Hits,
### sacks, interceptions and fumbles.
### All data from nflscrapR. https://github.com/ryurko/nflscrapR-data
### -----------------------------------------------------------------------------------------


library(tidyverse)
library(broom)
library(lubridate)

### Data munging for reference --------------------------------------------------------------

pbp_2018 <- read_csv("data/pbp_2018.csv") %>%
   mutate(yrdln = as.numeric(yrdln),
          DefensiveTeam = defteam,
          EPA = epa,
          GameID = game_id,
          PlayType = play_type,
          QBHit = qb_hit,
          ScoreDiff = score_differential,
          Sack = sack,
          Safety = safety,
          Fumble = fumble_lost,
          Season = year(game_date))
pbp_2017 <- read_csv("data/pbp_2017.csv")
pbp_2016 <- read_csv("data/pbp_2016.csv")
pbp_2015 <- read_csv("data/pbp_2015.csv")
pbp_2014 <- read_csv("data/pbp_2014.csv")
pbp_2013 <- read_csv("data/pbp_2013.csv")
pbp_2012 <- read_csv("data/pbp_2012.csv")
pbp_2011 <- read_csv("data/pbp_2011.csv")
pbp_2010 <- read_csv("data/pbp_2010.csv")
pbp_2009 <- read_csv("data/pbp_2009.csv")

all_epa <- bind_rows(pbp_2009,
                     pbp_2010,
                     pbp_2011,
                     pbp_2012,
                     pbp_2013,
                     pbp_2014,
                     pbp_2015,
                     pbp_2016,
                     pbp_2017,
                     pbp_2018)

### Lots of extraneous data, so selecting just the important bits ---------------------------

all_epa_filtered <- all_epa %>%
   filter(down %in% c(1:4)) %>%
   mutate(season_year = year(game_date),
          interception = ifelse(str_detect(desc, "INTERCEPTED") == TRUE, 1, 0),
          penalty = ifelse(str_detect(desc, "PENALTY") == TRUE, 1, 0),
          spike = ifelse(str_detect(desc, "spiked") == TRUE, 1, 0),
          knee = ifelse(str_detect(desc, "kneels") == TRUE, 1, 0),
          DefensiveTeam = ifelse(Season == 2016 & DefensiveTeam == "JAC",
                                 "JAX", DefensiveTeam)) %>%
   select(GameID, DefensiveTeam,
          qtr, down, PlayType,
          EPA, ScoreDiff, Fumble, Safety,
          Sack, QBHit, interception, penalty,
          desc, Season, spike, knee)

### We'll use this for sanity checks from here on out ---------------------------------------
### When reproducing the code quickest to just load the
### csv and start from there.

write_csv(all_epa_filtered, "data/defensive_pbp_09-18.csv")

### Need to remove penalties and

team_seasons_defense <- all_epa_filtered %>%
   filter(PlayType != "no_play",
          spike == 0,
          knee == 0) %>%
   na.omit() %>%
   group_by(DefensiveTeam, Season) %>%
   summarize(count = n(),
             epa = sum(EPA),
             epa_play = sum(EPA) / count,
             fumble_lost = sum(Fumble),
             qb_hit = sum(QBHit),
             sack = sum(Sack),
             interception = sum(interception)) %>%
   distinct() %>%
   mutate(Season2 = Season + 1)

team_defense_joined <-team_seasons_defense %>%
   inner_join(team_seasons_defense, by = c("Season2" = "Season", "DefensiveTeam"))

colnames(team_defense_joined)

### ------ Models go here ---------------------------------------------------------------////

### QB Hits first ---------------------------------------------------------------------------

qb_hits_model <- lm(data = team_defense_joined, qb_hit.y ~ qb_hit.x)
summary(qb_hits_model)

qb_hits_stability <- glance(qb_hits_model) %>%
   mutate(metric = "QB Hits",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### This will go in the README as a visual --------------------------------------------------

ggplot(data = team_defense_joined, aes(x = qb_hit.x, y = qb_hit.y)) +
   geom_point() +
   geom_smooth(method = "lm") +
   theme_bw() +
   labs(x = "Year Y Total QB Hits",
        y = "Year Y + 1 Total QB Hits",
        title = "QB Hits are Relatively Stable Year-to-Year",
        subtitle = paste("r-squared:", qb_hits_stability$r.squared),
        caption = "Source: NFL")

### magic incantation to save the plot to disk ----------------------------------------------

dev.copy(png,'qb_hits.png')
dev.off()

### Fumbles ---------------------------------------------------------------------------------

fumbles_model <- lm(data = team_defense_joined, fumble_lost.y ~ fumble_lost.x)
summary(fumbles_model)

fumbles_stability <- glance(fumbles_model) %>%
   mutate(metric = "Fumbles",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### Sacks -----------------------------------------------------------------------------------

sacks_model <- lm(data = team_defense_joined, sack.y ~ sack.x)
summary(sacks_model)

sacks_stability <- glance(sacks_model) %>%
   mutate(metric = "Sacks",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### Interceptions ---------------------------------------------------------------------------

interception_model <- lm(data = team_defense_joined, interception.y ~ interception.x)
summary(interception_model)

interception_stability <- glance(interception_model) %>%
   mutate(metric = "Interceptions",
          r.squared = round(r.squared, 3)) %>%
   select(metric, r.squared)

### Bind up the results with some twine.

defensive_metric_results <- qb_hits_stability %>%
   bind_rows(fumbles_stability,
             sacks_stability,
             interception_stability)

write_csv(defensive_metric_results, "results.csv")
