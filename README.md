# MLBtrial
# Prueba de series de tiempo para equipos que jugaron la serie mundial en 2019 y 2020
```R
library(dplyr)

#Leer los archivos CSV sobre MLB y COVID19
batters_2019 <-  read.csv("batter_2019.csv") #Bateadores en 2019
mlb_2020 <- read.csv("mlb_elo_latest.csv")   # MLB en 2020
mlb_historic <- read.csv("mlb_elo.csv")      # MLB Histórico
mlb_teams <- read.csv("MLB_Teams2.csv")      # MLB Teams
pitchers_2019 <- read.csv("pithcher_2019.csv") #pitchers 2019

# determinar clase de cada una de las bases de datos
class(batters_2019)
class(mlb_2020)
class(mlb_historic)
class(mlb_teams)
class(pitchers_2019)


#Información sobre cada una de las bases de datos para determinar qué datos cambiar o preservar
str(batters_2019)
str(mlb_2020)
str(mlb_historic)
str(mlb_teams)
str(pitchers_2019)

# Cambiando formato de fechas para los bases de datos de mlb_2020 y mlb_historic
mlb_2020 <- mutate(mlb_2020, date = as.Date(date, "%Y-%m-%d"))
mlb_historic <- mutate(mlb_historic, date = as.Date(date, "%Y-%m-%d"))

# Se analizarán 4 equipos, los que compitieron en las series mundiales de 2019 y 2020
# En 2019 la Serie Mundial se jugó entre los Washington Nationals y los Houston Astros, siendo The Washington Nationals el equipo ganador
# En 2020 La Serie Mundial se jugó entre Los Angeles Dodgers y Tampa Rays, donde Los Angeles Dodgers resultaron ganadores
# Therefore, the aim of this work is to show whether there was any relationship between the COVID-19 pandemic and the performance of the teams taken as study subject.



# **************************** THE WASHINGTON NATIONALS / WINNERS WORLD SERIES 2019 ************************************

# Encontrando el código para The Washington Nationals del data frame "mlb_teams" y selccionando únicamente
# la columna de código (tm) y su nombre en un nuevo data frame

nationals_code <- mlb_teams %>% select(team_name, tm) %>%
  filter(team_name == "Washington Nationals") %>%
  rename(team_code = tm)

dodgers_code <- mlb_teams %>% select(team_name, tm) %>%
  filter (team_name == "Los Angeles Dodgers") %>%
  rename(team_code = tm)

rays_code <- mlb_teams %>% select(team_name, tm) %>%
  filter (team_name == "Tampa Bay Rays") %>%
  rename(team_code = tm)

astros_code <- mlb_teams %>% select(team_name, tm) %>%
  filter (team_name == "Houston Astros") %>%
  rename(team_code = tm)

# Para construir serie de tiempo histórica de The Washington Nationals con base en las probabilidades del sistema Elo

# Extrayendo los datos de Washington Nationals como equipo local
wsn_hist_home_elo <- mlb_historic %>% select(date, season, team1, elo_prob1) %>%
  filter(team1 == "WSN") %>%
  rename(team = team1, elo_prob = elo_prob1)

# Extrayendo los datos de Washington Nationals como equipo visitante
wsn_hist_away_elo <- mlb_historic %>% select(date, season, team2, elo_prob2) %>%
  filter(team2 == "WSN") %>%
  rename(team = team2, elo_prob = elo_prob2)
  
# Datos Históricos de probabilidad Elo de Washington Nationals completos, tanto como equipo local como visitante
wsn_hist_complete_elo <- rbind(wsn_hist_home_elo, wsn_hist_away_elo)

# Agrupar por mes y año
wsn_elo <- wsn_hist_complete_elo %>% group_by(season) %>% summarise(elo_prob_avg=mean(elo_prob))

# Serie de tiempo de probabilidad promedio de ser ganadores anualmente

wsn_elo.ts <- ts(wsn_elo$elo_prob_avg, st = 1969, end = 2020, frequency = 1)

ts.plot(wsn_elo.ts, main = "Washington Nationals Elo probability of Winning",
ylab = "Elo probability of Winning")
abline(h = mean(wsn_elo.ts), lwd = 2, col = 2, lty = 2)



# Serie de Tiempo con las probabilidades de ganar 
# con base en el rating del equipo  y los pitchers que comienzan el juego rating_prob
# Extrayendo los datos de Washington Nationals como equipo local
wsn_hist_home_rating <- mlb_historic %>% select(date, season, team1, rating_prob1) %>%
  filter(team1 == "WSN") %>%
  rename(team = team1, rating_prob = rating_prob1)

# Extrayendo los datos de Washington Nationals como equipo visitante
wsn_hist_away_rating <- mlb_historic %>% select(date, season, team2, rating_prob2) %>%
  filter(team2 == "WSN") %>%
  rename(team = team2, rating_prob = rating_prob2)

# Datos Históricos de probabilidad Elo de Washington Nationals completos, tanto como equipo local como visitante
wsn_hist_complete_rating <- rbind(wsn_hist_home_rating, wsn_hist_away_rating)

# Agrupar por mes y año
wsn_rating <- wsn_hist_complete_rating %>% group_by(season) %>% summarise(rating_prob_avg=mean(rating_prob))

# Serie de tiempo de probabilidad promedio de ser ganadores anualmente

wsn_rating.ts <- ts(wsn_rating$rating_prob_avg, st = 1969, end = 2020, frequency = 1)

ts.plot(wsn_rating.ts, main = "Washington Nationals probability of Winning based on Team's Rating",
        ylab = "Probability of Winning based on Team's Rating")
abline(h = mean(wsn_rating.ts), lwd = 2, col = 2, lty = 2)



# Serie de Tiempo con las carreras promedio de The Nationals por temporada
# Extrayendo los datos de Washington Nationals como equipo local
wsn_hist_home_score <- mlb_historic %>% select(date, season, team1, score1) %>%
  filter(team1 == "WSN") %>%
  rename(team = team1, score = score1)

# Extrayendo los datos de Washington Nationals como equipo visitante
wsn_hist_away_score <- mlb_historic %>% select(date, season, team2, score2) %>%
  filter(team2 == "WSN") %>%
  rename(team = team2, score = score2)

# Datos Históricos de resultados de Washington Nationals completos, tanto como equipo local como visitante
wsn_hist_complete_score <- rbind(wsn_hist_home_score, wsn_hist_away_score)

# Agrupar por mes y año
wsn_hist_complete_score <- mutate(wsn_hist_complete_score, YearMonth = format(date, "%Y-%m"))
wsn_score <- wsn_hist_complete_score %>% group_by(YearMonth) %>% summarise(score_avg=mean(score))

# Serie de tiempo de carreras anotadas por The Nationals 

wsn_score.ts <- ts(wsn_score$score_avg, st = c(1969,4) , end = c (2020,9), frequency = 12)

ts.plot(wsn_score.ts, main = "Washington Nationals Scores",
        ylab = "Score")
abline(h = mean(wsn_score.ts), lwd = 2, col = 2, lty = 2)


latest_wsn_score <- window(wsn_score.ts, start = 2015, end = 2019)
latest_wsn_time <- time(latest_wsn_score)
plot(latest_wsn_score, xlab = "Tiempo", ylab ="Carreras anotadas", main ="Serie de Carreras Anotadas",
     sub = "Serie anual: 2010 a 2018"); abline(reg = lm(latest_wsn_score ~ latest_wsn_time))







# *********** HOUSTON ASTROS WORLD SERIES 2019 COMPETITORS ************************

# Datos de probabilidad de ganas con el método ELO 
# Extrayendo los datos de Probabilidad ELO de los Houston Astros como equipo local
astros_hist_home_elo <- mlb_historic %>% select(date, season, team1, elo_prob1) %>%
  filter(team1 == "HOU") %>%
  rename(team = team1, elo_prob = elo_prob1)

# Extrayendo los datos de Houston Astros como equipo visitante
astros_hist_away_elo <- mlb_historic %>% select(date, season, team2, elo_prob2) %>%
  filter(team2 == "HOU") %>%
  rename(team = team2, elo_prob = elo_prob2)

# Datos Históricos de probabilidad Elo de Houston Astros completos, tanto como equipo local como visitante
astros_hist_complete_elo <- rbind(astros_hist_home_elo, astros_hist_away_elo)

# Agrupar por año
astros_elo <- astros_hist_complete_elo %>% group_by(season) %>% summarise(elo_prob_avg=mean(elo_prob))

# Serie de tiempo de probabilidad promedio de ser ganadores anualmente

astros_elo.ts <- ts(astros_elo$elo_prob_avg, st = 1969, end = 2020, frequency = 1)

ts.plot(astros_elo.ts,
        ylab = "Elo probability of Winning", main = "Houston Astros ")
abline(h = mean(astros_elo.ts), lwd = 2, col = 2, lty = 2)

# Serie de Tiempo con las carreras promedio de Houston Astros por temporada
# Extrayendo los datos de Washington Nationals como equipo local
astros_hist_home_score <- mlb_historic %>% select(date, season, team1, score1) %>%
  filter(team1 == "HOU") %>%
  rename(team = team1, score = score1)

# Extrayendo los datos de Houston Astros como equipo visitante
astros_hist_away_score <- mlb_historic %>% select(date, season, team2, score2) %>%
  filter(team2 == "HOU") %>%
  rename(team = team2, score = score2)

# Datos Históricos de resultados de Houston Astros completos, tanto como equipo local como visitante
astros_hist_complete_score <- rbind(astros_hist_home_score, astros_hist_away_score)

# Agrupar por mes y año 
astros_hist_complete_score <- mutate(astros_hist_complete_score, YearMonth = format(date, "%Y-%m"))
astros_score <- astros_hist_complete_score %>% group_by(YearMonth) %>% summarise(score_avg=mean(score))

# Serie de tiempo de carreras anotadas por Houston Astros

astros_score.ts <- ts(astros_score$score_avg, st = c(1962,4) , end = c (2020,10), frequency = 12)

ts.plot(astros_score.ts, main = "Houston Astros Scores Time Series",
        ylab = "Scores")
abline(h = mean(astros_score.ts), lwd = 2, col = 2, lty = 2)


latest_astros_score <- window(astros_score.ts, start = 2015, end = 2019)
latest_astros_time <- time(latest_astros_score)
plot(latest_astros_score, xlab = "Tiempo", ylab ="Carreras anotadas", main ="Serie de Carreras Anotadas por Houston Astros",
     sub = "Serie anual: 2010 a 2018"); abline(reg = lm(latest_astros_score ~ latest_astros_time))





# *********** LOS ANGELES DODGERS WORLD SERIES 2020 WINNERS ************************

# Datos de probabilidad de ganar con el método ELO de Los Angeles Dodgers
# Extrayendo los datos de Probabilidad ELO de Los Angeles Dodgers como equipo local
lad_hist_home_elo <- mlb_historic %>% select(date, season, team1, elo_prob1) %>%
  filter(team1 == "LAD") %>%
  rename(team = team1, elo_prob = elo_prob1)

# Extrayendo los datos de Los Angeles Dodgers como equipo visitante
lad_hist_away_elo <- mlb_historic %>% select(date, season, team2, elo_prob2) %>%
  filter(team2 == "LAD") %>%
  rename(team = team2, elo_prob = elo_prob2)

# Datos Históricos de probabilidad Elo de LADston lad completos, tanto como equipo local como visitante
lad_hist_complete_elo <- rbind(lad_hist_home_elo, lad_hist_away_elo)

# Agrupar por año
lad_elo <- lad_hist_complete_elo %>% group_by(season) %>% summarise(elo_prob_avg=mean(elo_prob))

# Serie de tiempo de probabilidad promedio de ser ganadores anualmente

lad_elo.ts <- ts(lad_elo$elo_prob_avg, st = 1962, end = 2020, frequency = 1)

ts.plot(lad_elo.ts,
        ylab = "Elo probability of Winning", main = "Los Angeles Dodgers ")
abline(h = mean(lad_elo.ts), lwd = 2, col = 2, lty = 2)



# Serie de Tiempo con las carreras promedio de Los Angeles Dodgers por temporada
# Extrayendo los datos de Los Angeles Dodgers como equipo local
lad_hist_home_score <- mlb_historic %>% select(date, season, team1, score1) %>%
  filter(team1 == "LAD") %>%
  rename(team = team1, score = score1)

# Extrayendo los datos de Los Angeles Dodgers como equipo visitante
lad_hist_away_score <- mlb_historic %>% select(date, season, team2, score2) %>%
  filter(team2 == "LAD") %>%
  rename(team = team2, score = score2)

# Datos Históricos de resultados de Los Angeles Dodgers completos, tanto como equipo local como visitante
lad_hist_complete_score <- rbind(lad_hist_home_score, lad_hist_away_score)

# Agrupar por mes y año 
lad_hist_complete_score <- mutate(lad_hist_complete_score, YearMonth = format(date, "%Y-%m"))
lad_score <- lad_hist_complete_score %>% group_by(YearMonth) %>% summarise(score_avg=mean(score))

# Serie de tiempo de carreras anotadas por Los Angeles Dodgers

lad_score.ts <- ts(lad_score$score_avg, st = c(1962,4) , end = c (2020,10), frequency = 12)

ts.plot(lad_score.ts, main = "Los Angeles Dodgers ",
        ylab = "lad Scores")
abline(h = mean(lad_score.ts), lwd = 2, col = 2, lty = 2)

#Serie de Tiempo de carreras anotadas por Los Angeles Dodgers sólo de 2015 a 2019
latest_lad_score <- window(lad_score.ts, start = 2015, end = 2019)
latest_lad_time <- time(latest_lad_score)
plot(latest_lad_score, xlab = "Tiempo", ylab ="Carreras anotadas", main ="Serie de Carreras Anotadas por LAD",
     sub = "Serie anual: 2010 a 2018"); abline(reg = lm(latest_lad_score ~ latest_lad_time))






# *********** TAMPA BAY RAYS 2020 COMPETITORS ************************

# Datos de probabilidad de ganar con el método ELO de Los Tampa Bay Rays po local
rays_hist_home_elo <- mlb_historic %>% select(date, season, team1, elo_prob1) %>%
  filter(team1 == "TBD") %>%
  rename(team = team1, elo_prob = elo_prob1)

# Extrayendo los datos de Los Tampa Bay Rays como equipo visitante
rays_hist_away_elo <- mlb_historic %>% select(date, season, team2, elo_prob2) %>%
  filter(team2 == "TBD") %>%
  rename(team = team2, elo_prob = elo_prob2)

# Datos Históricos de probabilidad Elo de Los Tampa Bay Rays completos, tanto como equipo local como visitante
rays_hist_complete_elo <- rbind(rays_hist_home_elo, rays_hist_away_elo)

# Agrupar por año
rays_elo <- rays_hist_complete_elo %>% group_by(season) %>% summarise(elo_prob_avg=mean(elo_prob))

# Serie de tiempo de probabilidad promedio de ser ganadores anualmente

rays_elo.ts <- ts(rays_elo$elo_prob_avg, st = 1969, end = 2020, frequency = 1)

ts.plot(rays_elo.ts,  main ="Serie Probabilidad ELO de ganar de Tampa Bay Rays",
        ylab = "Elo probability of Winning")
abline(h = mean(rays_elo.ts), lwd = 2, col = 2, lty = 2)



# Serie de Tiempo con las carreras promedio de Los Tampa Bay Rays por temporada
# Extrayendo los datos de Los Tampa Bay Rays como equipo local
rays_hist_home_score <- mlb_historic %>% select(date, season, team1, score1) %>%
  filter(team1 == "TBD") %>%
  rename(team = team1, score = score1)

# Extrayendo los datos de Los Tampa Bay Rays como equipo visitante
rays_hist_away_score <- mlb_historic %>% select(date, season, team2, score2) %>%
  filter(team2 == "TBD") %>%
  rename(team = team2, score = score2)

# Datos Históricos de resultados de Los Tampa Bay Rays completos, tanto como equipo local como visitante
rays_hist_complete_score <- rbind(rays_hist_home_score, rays_hist_away_score)

# Agrupar por mes y año 
rays_hist_complete_score <- mutate(rays_hist_complete_score, YearMonth = format(date, "%Y-%m"))
rays_score <- rays_hist_complete_score %>% group_by(YearMonth) %>% summarise(score_avg=mean(score))

# Serie de tiempo de carreras anotadas por Los Tampa Bay Rays

rays_score.ts <- ts(rays_score$score_avg, st = c(1962,4) , end = c (2020,10), frequency = 12)

ts.plot(rays_score.ts, main ="Serie de Carreras Anotadas por Tampa Bay Rays", sub = "Serie anual: 1962 a 2018", 
        ylab = "rays Scores")
abline(h = mean(rays_score.ts), lwd = 2, col = 2, lty = 2)

#Serie de Tiempo de carreras anotadas por Los Tampa Bay Rays sólo de 2015 a 2019
latest_rays_score <- window(rays_score.ts, start = 2015, end = 2019)
latest_rays_time <- time(latest_rays_score)
plot(latest_rays_score, xlab = "Tiempo", ylab ="Carreras anotadas", main ="Serie de Carreras Anotadas por Tampa Bay Rays",
     sub = "Serie anual: 2010 a 2018"); abline(reg = lm(latest_rays_score ~ latest_rays_time))


```` 
