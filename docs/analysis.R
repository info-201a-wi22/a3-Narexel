library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(maps)

prison_csv <- read.csv(file = "source/incarceration_trends.csv", header = T)

prison_data <- prison_csv %>%
  select(
    year,
    state,
    total_pop_15to64,
    aapi_pop_15to64,
    black_pop_15to64,
    latinx_pop_15to64,
    native_pop_15to64,
    white_pop_15to64,
    urbanicity,
    total_jail_pop,
    aapi_jail_pop,
    black_jail_pop,
    latinx_jail_pop,
    native_jail_pop,
    white_jail_pop
  ) %>%
  replace_na(
    list(
      total_pop_15to64 = 0,
      aapi_pop_15to64 = 0,
      black_pop_15to64 = 0,
      latinx_pop_15to64 = 0,
      native_pop_15to64 = 0,
      white_pop_15to64 = 0,
      total_jail_pop = 0,
      aapi_jail_pop = 0,
      black_jail_pop = 0,
      latinx_jail_pop = 0,
      native_jail_pop = 0,
      white_jail_pop = 0
    )
  ) %>%
  filter(year >= 1990)

top_10_pop_state <- prison_data %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarise(total_pop_15to64 = sum(total_pop_15to64)) %>%
  arrange(desc(total_pop_15to64)) %>%
  top_n(10) %>%
  pull(state)

top_10_state_pri_rate <- prison_data %>%
  filter(state %in% top_10_pop_state) %>%
  group_by(year, state) %>% 
  summarise(
    total_jail_pop = sum(total_jail_pop),
    total_pop_15to64 = sum(total_pop_15to64)
  ) %>%
  mutate(prison_rate = total_jail_pop / total_pop_15to64)

# figure 1
p <- ggplot(data=top_10_state_pri_rate, aes(x=year, y=prison_rate, color=state)) +
  geom_line() + 
  geom_point() +
  labs(title="Population top 10 states prison rate by year", x="Year", y="Prison rate")
p

jail_rate_by_urbanicity <- prison_data %>%
  filter(urbanicity != "") %>%
  group_by(urbanicity, year) %>%
  summarise(
    total_pop_15to64 = sum(total_pop_15to64),
    aapi_pop_15to64 = sum(aapi_pop_15to64),
    black_pop_15to64 = sum(black_pop_15to64),
    latinx_pop_15to64 = sum(latinx_pop_15to64),
    native_pop_15to64 = sum(native_pop_15to64),
    white_pop_15to64 = sum(white_pop_15to64),
    total_jail_pop = sum(total_jail_pop),
    aapi_jail_pop = sum(aapi_jail_pop),
    black_jail_pop = sum(black_jail_pop),
    latinx_jail_pop = sum(latinx_jail_pop),
    native_jail_pop = sum(native_jail_pop),
    white_jail_pop = sum(white_jail_pop)
  ) %>%
  mutate(
    total_rate = total_jail_pop / total_pop_15to64,
    aapi_rate = aapi_jail_pop / aapi_pop_15to64,
    black_rate = black_jail_pop / black_pop_15to64,
    latinx_rate = latinx_jail_pop / latinx_pop_15to64,
    native_rate = native_jail_pop / native_pop_15to64,
    white_rate = white_jail_pop / white_pop_15to64
  ) %>%
  group_by(urbanicity) %>%
  summarise(
    total_rate = mean(total_rate),
    aapi_rate = mean(aapi_rate),
    black_rate = mean(black_rate),
    latinx_rate = mean(latinx_rate),
    native_rate = mean(native_rate),
    white_rate = mean(white_rate)
  )

aapi_jail_rate_by_state <- jail_rate_by_urbanicity %>%
  mutate(
    rate = aapi_rate,
    type = "aapi"
  ) %>%
  select(urbanicity, rate, type)

black_jail_rate_by_state <- jail_rate_by_urbanicity %>%
  mutate(
    rate = black_rate,
    type = "black"
  ) %>%
  select(urbanicity, rate, type)

latinx_jail_rate_by_state <- jail_rate_by_urbanicity %>%
  mutate(
    rate = latinx_rate,
    type = "latinx"
  ) %>%
  select(urbanicity, rate, type)

native_jail_rate_by_state <- jail_rate_by_urbanicity %>%
  mutate(
    rate = native_rate,
    type = "native"
  ) %>%
  select(urbanicity, rate, type)

white_jail_rate_by_state <- jail_rate_by_urbanicity %>%
  mutate(
    rate = white_rate,
    type = "white"
  ) %>%
  select(urbanicity, rate, type)

jail_rate_by_urbanicity <- aapi_jail_rate_by_state %>%
  bind_rows(black_jail_rate_by_state) %>%
  bind_rows(latinx_jail_rate_by_state) %>%
  bind_rows(white_jail_rate_by_state) %>%
  bind_rows(native_jail_rate_by_state) 
# fig 2 
g <- ggplot(data=jail_rate_by_urbanicity, aes(x=type, y=rate, fill=urbanicity)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5) + 
  labs(title="Jail rate by race", x="Race", y="Jail rate")
g

jail_rate_by_state <- prison_data %>%
  filter(year >= 2000) %>%
  group_by(year, state) %>%
  summarise(
    total_pop_15to64 = sum(total_pop_15to64),
    total_jail_pop = sum(total_jail_pop)
  ) %>%
  mutate(
    total_rate = total_jail_pop / total_pop_15to64
  ) %>%
  group_by(state) %>%
  summarise(
    total_rate = mean(total_rate)
  )


# fig 3
MainStates <- map_data("state")
StateToCode <- read.csv(file = "source/csvData.csv", header = T)
StateToCode$region = tolower(StateToCode$State)

MainStates <- MainStates %>%
  left_join(StateToCode, by = c("region" = "region"))
MergedStates <- inner_join(MainStates, jail_rate_by_state, by = c("Code" = "state"))

g <- ggplot() +
  geom_polygon( data=MergedStates, 
                aes(x=long, y=lat, group=group, fill = total_rate), 
                color="white", size = 0.2)  +
  labs(title="Jail rate by state")
g
state_top_jail_rate <- jail_rate_by_state %>%
  arrange(desc(total_rate)) %>%
  top_n(1) %>%
  pull(state)

jail_rate_race_year <- prison_data %>%
  filter(year >= 2000, state == state_top_jail_rate) %>%
  group_by(year, state) %>%
  summarise(
    total_pop_15to64 = sum(total_pop_15to64),
    aapi_pop_15to64 = sum(aapi_pop_15to64),
    black_pop_15to64 = sum(black_pop_15to64),
    latinx_pop_15to64 = sum(latinx_pop_15to64),
    native_pop_15to64 = sum(native_pop_15to64),
    white_pop_15to64 = sum(white_pop_15to64),
    total_jail_pop = sum(total_jail_pop),
    aapi_jail_pop = sum(aapi_jail_pop),
    black_jail_pop = sum(black_jail_pop),
    latinx_jail_pop = sum(latinx_jail_pop),
    native_jail_pop = sum(native_jail_pop),
    white_jail_pop = sum(white_jail_pop)
  ) %>%
  mutate(
    total_rate = total_jail_pop / total_pop_15to64,
    aapi_rate = aapi_jail_pop / aapi_pop_15to64,
    black_rate = black_jail_pop / black_pop_15to64,
    latinx_rate = latinx_jail_pop / latinx_pop_15to64,
    native_rate = native_jail_pop / native_pop_15to64,
    white_rate = white_jail_pop / white_pop_15to64
  )


aapi_jail_rate_by_state <- jail_rate_race_year %>%
  mutate(
    rate = aapi_rate,
    type = "aapi"
  ) %>%
  select(year, rate, type)

black_jail_rate_by_state <- jail_rate_race_year %>%
  mutate(
    rate = black_rate,
    type = "black"
  ) %>%
  select(year, rate, type)

latinx_jail_rate_by_state <- jail_rate_race_year %>%
  mutate(
    rate = latinx_rate,
    type = "latinx"
  ) %>%
  select(year, rate, type)

native_jail_rate_by_state <- jail_rate_race_year %>%
  mutate(
    rate = native_rate,
    type = "native"
  ) %>%
  select(year, rate, type)

white_jail_rate_by_state <- jail_rate_race_year %>%
  mutate(
    rate = white_rate,
    type = "white"
  ) %>%
  select(year, rate, type)

jail_rate_race_year <- aapi_jail_rate_by_state %>%
  bind_rows(black_jail_rate_by_state) %>%
  bind_rows(latinx_jail_rate_by_state) %>%
  bind_rows(white_jail_rate_by_state) %>%
  bind_rows(native_jail_rate_by_state) 

# fig 4

p <- ggplot(data=jail_rate_race_year, aes(x=year, y=rate, color=type)) +
  geom_line() + 
  geom_point() +
  labs(title="Prison rate top 1 state (LA) by time", x="Year", y="Prison rate")
p

