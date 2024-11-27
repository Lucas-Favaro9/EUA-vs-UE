setwd('d:\\Giovanni\\Downloads\\USA vs EU')

library(tidyverse)
library(readxl)

# Importando dados
prod <- read.csv('productivity.csv', header = T, sep = ',')

paises <- read_excel('países.xlsx') %>%
  filter(!is.na(Country)) %>%
  mutate(EU = 1)

USA <- prod %>%
  filter(Code == 'USA') %>%
  select(-Code)

# Mergeando os dataframes de produtividade e de países
base <- left_join(prod, paises, by = c('Entity' = 'Country')) %>%
  filter(EU == 1) %>%
  select(-EU)

# Calculando a média da produtividade da União Europeia
media <- base %>%
  group_by(Year) %>%
  summarise(Productivity = mean(Productivity, na.rm = TRUE)) %>%
  mutate(Entity = 'EU')

# Mergeando os dataframes da UE e dos EUA
base_1 <- rbind(USA, media) %>%
  filter(Year >= 2000)

# Calculando a razão entre a produtividade das duas regiões
result <- base_1 %>%
  pivot_wider(names_from = Entity, values_from = Productivity) %>%
  mutate(razao = `United States` / EU) %>%
  select(Year, razao)

# Gráfico
ggplot(result, aes(x = Year, y = razao)) +
  geom_line(size = 1.1, color = 'steelblue') +
  geom_point(size = 1.4, color = 'steelblue') +
  labs(title = "Razão do PIB por hora trabalhada entre os EUA e a UE",
       y = "Razão", x = "") +
  scale_x_continuous(
    limits = c(2000, 2020),
    breaks = seq(2000, 2020, by = 2)) +
  scale_y_continuous(
    limits = c(1.4, 1.7),
    breaks = seq(1.3, 1.7, by = 0.1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13))

ggsave(plot = last_plot(), file = "razao.png", width = 10, height = 5, bg = 'white')
