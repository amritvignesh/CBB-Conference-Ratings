library(hoopR)
library(dplyr)
library(gt)
library(gtExtras) 

box <- load_mbb_player_box(2022:2024) # loading box scores

teams <- data.frame()

for (year in 2022:2024) {
  team_year <- espn_mbb_teams(year) %>% mutate(year = year)
  teams <- rbind(teams, team_year)
} # all teams and conferences in respective years

na_teams <- teams %>% filter(is.na(conference_short_name))
teams <- teams %>% filter(!is.na(conference_short_name) | group_id == 21)
teams <- teams %>% select(year, team_id, conf = conference_short_name)
teams$conf <- ifelse(is.na(teams$conf), "Pac-12", teams$conf) # manually adding pac12 as conference

confs_unique <- unique(teams$conf)

box <- box %>% left_join(teams, by = c("season"="year", "team_id"))
box <- box %>% left_join(teams, by = c("season"="year", "opponent_team_id"="team_id"))
box <- box %>% filter(!is.na(conf.x) & !is.na(conf.y)) %>% mutate(conf = conf.x, opp_conf = conf.y, field_goals_missed = field_goals_attempted - field_goals_made, free_throws_missed = free_throws_attempted - free_throws_made) # generating conference and opponent conference in game

player_stats <- box %>%
  filter(conf == opp_conf) %>%
  group_by(year = season, id = athlete_id) %>%
  summarize(name = first(athlete_display_name), team = first(team_display_name), conf = first(conf), 
            fgm = sum(field_goals_made, na.rm = TRUE),
            steals = sum(steals, na.rm = TRUE),
            three_ptm = sum(three_point_field_goals_made, na.rm = TRUE),
            ftm = sum(free_throws_made, na.rm = TRUE),
            blocks = sum(blocks, na.rm = TRUE),
            orbs = sum(offensive_rebounds, na.rm = TRUE),
            asts = sum(assists, na.rm = TRUE),
            drbs = sum(defensive_rebounds, na.rm = TRUE),
            fouls = sum(fouls, na.rm = TRUE),
            ftmiss = sum(free_throws_missed, na.rm = TRUE),
            fgmiss = sum(field_goals_missed, na.rm = TRUE),
            turnovers = sum(turnovers, na.rm = TRUE),
            mins = sum(minutes, na.rm = TRUE)) # player stats in a season in conference-play games

player_stats <- player_stats %>% mutate(per = (85.910 * fgm + 53.897 * steals + 51.757 * three_ptm + 46.845 + ftm + 39.190 * blocks + 39.190 * orbs + 34.677 * asts + 14.707 * drbs - 17.174 * fouls - 20.091 * ftmiss - 39.190 * fgmiss - 53.897 * turnovers)/mins)
player_stats <- player_stats %>% filter(mins >= 100) # generating per stat and filtering for stability to 100+ minutes

transfers <- player_stats %>%
  arrange(id, year) %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  mutate(prev_conf = lag(conf), prev_year = lag(year), new_conf = conf, per_change = per - lag(per)) %>%
  filter(!is.na(prev_conf) & prev_conf != new_conf & year == prev_year + 1) %>%
  select(id, name, prev_conf, new_conf, per_change) # finding transfers in consecutive years cross-conference

for (conf in confs_unique) {
  transfers[[conf]] <- 0  
} 

transfers <- transfers %>% mutate(across(confs_unique, 
                                         ~ case_when(prev_conf == cur_column() ~ -1,
                                                     new_conf == cur_column() ~ 1,
                                                     TRUE ~ 0), .names = "{.col}")) # if a player leaves conference, its -1m if they join one, its +1

model_data <- transfers[, c(5:37)]

transfer_model <- lm(per_change ~ ., data = model_data) # per change across years vs all conferences status

coefs <- data.frame(coef(transfer_model))
colnames(coefs) <- c("coef")
coefs$conf <- rownames(coefs)
coefs <- coefs %>% select(conf, coef)
coefs$coef <- ifelse(is.na(coefs$coef), 0, coefs$coef)
coefs <- coefs %>% arrange(coef)
rownames(coefs) <- NULL
coefs <- coefs %>% filter(conf != "(Intercept)")
coefs$coef <- scale(coefs$coef)
max_coef <- max(coefs$coef)
min_coef <- min(coefs$coef)
coefs <- coefs %>% mutate(rating = (max_coef - coef)/(max_coef - min_coef) * 100) %>% arrange(-rating) # creating ratings based on coefficients
conf_logos <- espn_mbb_teams(2024) %>%
  distinct(conf = conference_short_name, name = conference_name, logo = conference_logo) %>%
  filter(!is.na(conf))

conf_logos$logo[which(conf_logos$conf == "ASUN")] <- "https://loodibee.com/wp-content/uploads/Atlantic-Sun-Conference-ASUN-logo.png"
conf_logos <- rbind(conf_logos, data.frame(conf = "Pac-12", name = "Pac-12 Conference", logo = "https://a.espncdn.com/i/teamlogos/ncaa_conf/500/pac_12.png"))

coefs$conf <- gsub("`", "", coefs$conf)
coefs <- coefs %>% left_join(conf_logos, by = "conf") %>% select(logo, name, rating)
coefs$rating <- round(coefs$rating, 1)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption_1 = gt_align_caption("Data from <b>hoopR</b>", "")
caption_2 = gt_align_caption("", "Amrit Vignesh | <b>@avsportsanalyst</b>")

domain <- range(coefs$rating)

table_1 <- coefs %>% head(16) %>% gt() %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(logo, name, rating)
  ) %>%
  data_color(
    columns = rating,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = domain
    )
  ) %>%
  cols_label(
    logo = "",
    name = md("**Conference**"),
    rating = md("**Rating**")
  ) %>%
  tab_header(
    title = "D1 College Basketball Conference Ratings",
    subtitle = md("*Based on Cross-Conference Transfer Data From **2022-2024***")
  ) %>% 
  tab_source_note(html(caption_1)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name, rating)
    )
  ) 

table_2 <- coefs %>% filter(row_number() > 16) %>% gt() %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(logo, name, rating)
  ) %>%
  data_color(
    columns = rating,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = domain
    )
  ) %>%
  cols_label(
    logo = "",
    name = md("**Conference**"),
    rating = md("**Rating**")
  ) %>%
  tab_header(
    title = "(Pre-NCAA Conf. Realignment in 2024)",
    subtitle = md("*Ratings Scaled From **0** to **100** Based on Best and Worst Confs.*")
  ) %>% 
  tab_source_note(html(caption_2)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name, rating)
    )
  ) 

gtsave(table_1, "table_1.png")
gtsave(table_2, "table_2.png")