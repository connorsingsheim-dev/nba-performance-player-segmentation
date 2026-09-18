library(hoopR)
library(tidyverse)

# STEP 1

nba_2025 <- load_nba_team_box(seasons = 2025) |>
  filter(season_type == 2)

nba_2026 <- load_nba_team_box(seasons = 2026) |>
  filter(season_type == 2)

four_factors <- function(data) {
  
  opponent_rebounds <- data |>
    select(game_id, team_id, defensive_rebounds) |>
    rename(
      opponent_team_id = team_id,
      opponent_defensive_rebounds = defensive_rebounds
    )
  
  data |>
    left_join(
      opponent_rebounds,
      by = c("game_id", "opponent_team_id")
    ) |>
    group_by(
      team_id,
      team_abbreviation,
      team_display_name
    ) |>
    summarise(
      games = n(),
      wins = sum(team_winner),
      fgm = sum(field_goals_made),
      fga = sum(field_goals_attempted),
      fg3m = sum(three_point_field_goals_made),
      fta = sum(free_throws_attempted),
      orb = sum(offensive_rebounds),
      opp_drb = sum(opponent_defensive_rebounds),
      tov = sum(turnovers),
      .groups = "drop"
    ) |>
    filter(games >= 80) |>
    mutate(
      efg_pct = (fgm + 0.5 * fg3m) / fga,
      possessions = fga - orb + tov + (0.44 * fta),
      tov_pct = tov / possessions,
      oreb_pct = orb / (orb + opp_drb),
      fta_rate = fta / fga,
      efg_rank = rank(-efg_pct),
      tov_rank = rank(tov_pct),
      oreb_rank = rank(-oreb_pct),
      fta_rank = rank(-fta_rate)
    )
}

nba_2025_factors <- four_factors(nba_2025)
nba_2026_factors <- four_factors(nba_2026)

bulls_2025 <- nba_2025_factors |>
  filter(team_abbreviation == "CHI") |>
  select(
    team_display_name,
    games,
    wins,
    efg_pct,
    efg_rank,
    tov_pct,
    tov_rank,
    oreb_pct,
    oreb_rank,
    fta_rate,
    fta_rank
  )

bulls_2026 <- nba_2026_factors |>
  filter(team_abbreviation == "CHI") |>
  select(
    team_display_name,
    games,
    wins,
    efg_pct,
    efg_rank,
    tov_pct,
    tov_rank,
    oreb_pct,
    oreb_rank,
    fta_rate,
    fta_rank
  )

bulls_2025
bulls_2026


# STEP 2

nba_model_data <- load_nba_team_box(seasons = 2021:2025) |>
  filter(season_type == 2)

opponent_rebounds <- nba_model_data |>
  select(game_id, team_id, defensive_rebounds) |>
  rename(
    opponent_team_id = team_id,
    opponent_defensive_rebounds = defensive_rebounds
  )

nba_games <- nba_model_data |>
  left_join(
    opponent_rebounds,
    by = c("game_id", "opponent_team_id")
  ) |>
  mutate(
    win = if_else(team_winner == TRUE, 1, 0),
    efg_pct =
      (field_goals_made +
         0.5 * three_point_field_goals_made) /
      field_goals_attempted,
    possessions =
      field_goals_attempted -
      offensive_rebounds +
      turnovers +
      (0.44 * free_throws_attempted),
    tov_pct = turnovers / possessions,
    oreb_pct =
      offensive_rebounds /
      (offensive_rebounds + opponent_defensive_rebounds),
    fta_rate =
      free_throws_attempted / field_goals_attempted
  ) |>
  drop_na(
    win,
    efg_pct,
    tov_pct,
    oreb_pct,
    fta_rate
  )

win_model <- glm(
  win ~ efg_pct + oreb_pct + tov_pct + fta_rate,
  data = nba_games,
  family = "binomial"
)

summary(win_model)

bulls_games_2025 <- nba_games |>
  filter(
    season == 2025,
    team_abbreviation == "CHI"
  )

bulls_games_2025$win_probability <- predict(
  win_model,
  newdata = bulls_games_2025,
  type = "response"
)

sum(bulls_games_2025$win)
sum(bulls_games_2025$win_probability)


# STEP 3

nba_players <- load_nba_player_box(seasons = 2021:2026)

nba_players_reg <- nba_players |>
  filter(
    season_type == 2,
    did_not_play == FALSE
  )

player_seasons <- nba_players_reg |>
  group_by(
    season,
    athlete_id,
    athlete_display_name
  ) |>
  summarise(
    minutes = sum(minutes, na.rm = TRUE),
    fgm = sum(field_goals_made, na.rm = TRUE),
    fga = sum(field_goals_attempted, na.rm = TRUE),
    fg3m = sum(three_point_field_goals_made, na.rm = TRUE),
    fg3a = sum(three_point_field_goals_attempted, na.rm = TRUE),
    fta = sum(free_throws_attempted, na.rm = TRUE),
    orb = sum(offensive_rebounds, na.rm = TRUE),
    ast = sum(assists, na.rm = TRUE),
    tov = sum(turnovers, na.rm = TRUE),
    pts = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(
    minutes >= 500,
    fga > 0
  ) |>
  mutate(
    efg_pct = (fgm + 0.5 * fg3m) / fga,
    three_rate = fg3a / fga,
    fta_rate = fta / fga,
    tov_rate = tov / (fga + 0.44 * fta + tov),
    orb_per36 = (orb / minutes) * 36,
    ast_per36 = (ast / minutes) * 36,
    pts_per36 = (pts / minutes) * 36
  )

cluster_data <- player_seasons |>
  select(
    efg_pct,
    three_rate,
    fta_rate,
    tov_rate,
    orb_per36,
    ast_per36,
    pts_per36
  ) |>
  drop_na()

cluster_scaled <- scale(cluster_data)

set.seed(9019)

wss <- numeric(9)

for (k in 2:10) {
  
  model <- kmeans(
    cluster_scaled,
    centers = k,
    nstart = 25,
    iter.max = 100
  )
  
  wss[k - 1] <- model$tot.withinss
}

elbow_data <- data.frame(
  clusters = 2:10,
  wss = wss
)

ggplot(
  elbow_data,
  aes(x = clusters, y = wss)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Selecting Number of NBA Player Types",
    x = "Number of Clusters",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

set.seed(9019)

player_kmeans <- kmeans(
  cluster_scaled,
  centers = 4,
  nstart = 25,
  iter.max = 100
)

player_results <- player_seasons |>
  drop_na(
    efg_pct,
    three_rate,
    fta_rate,
    tov_rate,
    orb_per36,
    ast_per36,
    pts_per36
  ) |>
  mutate(
    cluster = factor(player_kmeans$cluster)
  )

cluster_summary <- player_results |>
  group_by(cluster) |>
  summarise(
    players = n(),
    efg_pct = mean(efg_pct),
    three_rate = mean(three_rate),
    fta_rate = mean(fta_rate),
    tov_rate = mean(tov_rate),
    orb_per36 = mean(orb_per36),
    ast_per36 = mean(ast_per36),
    pts_per36 = mean(pts_per36)
  )

cluster_summary


# STEP 4

bulls_players_2026 <- nba_players |>
  filter(
    season == 2026,
    season_type == 2,
    team_abbreviation == "CHI"
  ) |>
  distinct(
    athlete_id,
    athlete_display_name
  )

bulls_roster <- player_results |>
  filter(season == 2026) |>
  inner_join(
    bulls_players_2026,
    by = c(
      "athlete_id",
      "athlete_display_name"
    )
  ) |>
  mutate(
    player_type = case_when(
      cluster == "1" ~ "Primary Scorer",
      cluster == "2" ~ "Interior Finisher",
      cluster == "3" ~ "Perimeter Role Player",
      cluster == "4" ~ "Secondary Facilitator"
    )
  ) |>
  select(
    athlete_display_name,
    minutes,
    cluster,
    player_type,
    efg_pct,
    three_rate,
    fta_rate,
    tov_rate,
    orb_per36,
    ast_per36,
    pts_per36
  ) |>
  arrange(cluster, desc(minutes))

bulls_roster

bulls_roster |>
  count(
    player_type,
    sort = TRUE
  )
