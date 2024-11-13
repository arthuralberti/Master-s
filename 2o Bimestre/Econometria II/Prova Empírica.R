# Prova Empírica - Arthur Alberti - 15514794
rm(list = ls())

## Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
require(devtools) 
install_version("mlogit", version = "0.4.2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(xtable)
library(stargazer)
library(data.table)

## Parameters
file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file_directory)

theme_set(theme_minimal(base_family = "Times New Roman"))

source("m_predict.R")
source("m_effects.R")

## Collecting the DataFrame
load("Prova Empírica.RData")

write.csv(Dat, "Prova Intermediária.csv", row.names = TRUE)

df <- read.csv("Prova Intermediária.csv")

## Cleaning the DataFrame
df <- as.data.frame(df)

lenght <- nrow(df)

colnames(df)

df <- df[complete.cases(df), ]

cat("Por conta da retirada de NAs foram retiradas ", lenght - nrow(df), " observações.")

df$year <- as.numeric(substring(df$erim_wk, 1, 4))
df$week <- as.numeric(substring(df$erim_wk, 5, 6))

df <- df[, !(names(df) %in% c("X", "erim_wk", "UPC_nr", "hh_id"))]

## Analysing the DataFrame
table <- summary(df[c("erim_mkt", "store_id", "Code", "year", "week")])

table <- xtable(table, caption = "Summary Statistics", label = "tab:summary_stats")
print.xtable(table, include.rownames = FALSE, include.colnames = TRUE, booktabs = TRUE,
             caption.placement = "top", floating = TRUE, float.placement = "ht")

table1 <- describe(df[c("erim_mkt", "store_id", "Code", "year", "week")])
table1 <- table1[, c("mean", "sd", "median", "trimmed", "min", "max")]
table1 <- rownames_to_column(table1, var = "index")
table1 <- xtable(table1, caption = "Summary Statistics", label = "tab:summary_stats")
print.xtable(table1, include.rownames = FALSE, include.colnames = TRUE, booktabs = TRUE,
             caption.placement = "top", floating = TRUE, float.placement = "ht")


print(sapply(df[c("erim_mkt", "store_id", "id", "Code", "year", "week", "time", "avg_price")], 
             function(x) length(unique(x))))

grouped_data <- df %>%
  group_by(erim_mkt, year, week, time, store_id) %>%
  summarise(count_unique_target = sum(chid)) %>%
  ungroup()

print(grouped_data)

is_unique <- all(grouped_data$count_unique_target == 1)

# Exercício a)
## Histogramas
### erim_mkt
ggplot(df, aes_string(x = df$erim_mkt)) +
  geom_histogram(bins = 2, fill = "steelblue", color = "black") +
  ggtitle(paste("Histograma de Localização")) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

### year
ggplot(df, aes_string(x = df$year)) +
  geom_histogram(bins = 3, fill = "steelblue", color = "black") +
  ggtitle(paste("Histograma de Ano")) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

### week
ggplot(df, aes_string(x = df$week)) +
  geom_histogram(bins = 52, fill = "steelblue", color = "black") +
  ggtitle(paste("Histograma de Semana do Ano")) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

### store_id
ggplot(df, aes_string(x = df$store_id)) +
  geom_histogram(bins = 45, fill = "steelblue", color = "black") +
  ggtitle(paste("Histograma de Loja")) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

#### Code
ggplot(df, aes_string(x = df$Code)) +
  geom_histogram(bins = 6, fill = "steelblue", color = "black") +
  ggtitle(paste("Histograma de Código do Produto")) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

### Sioux Falls
## Share of Sales by Product and Market
df_analysing <- df %>%
  filter(erim_mkt == 1) %>%
  group_by(Code) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

ggplot(df_analysing, aes(x = Code, y = share)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Código da Loja", y = "Share") +
  ggtitle("Share de Vendas - Sioux Falls") +
  theme_minimal()

## Share of Sales by Store and Market
df_analysing <- df %>%
  filter(erim_mkt == 1) %>%
  group_by(store_id) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

ggplot(df_analysing, aes(x = store_id, y = share)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Código do Produto", y = "Share") +
  ggtitle("Share de Vendas - Sioux Falls") +
  theme_minimal()

## Number of Products in Display by Store and Code
### Display_1
df_analysing <- df %>%
  filter(erim_mkt == 1) %>%
  group_by(Code, display_1) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_1)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Sioux Falls") +
  guides(fill = guide_legend(title = "Display 1")) +
  theme_minimal()

### Display_2
df_analysing <- df %>%
  filter(erim_mkt == 1) %>%
  group_by(Code, display_2) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_2)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Sioux Falls") +
  guides(fill = guide_legend(title = "Display 2")) +
  theme_minimal()

### Display_3
df_analysing <- df %>%
  filter(erim_mkt == 1) %>%
  group_by(Code, display_3) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_3)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Sioux Falls") +
  guides(fill = guide_legend(title = "Display 3")) +
  theme_minimal()

### Display_4
df_analysing <- df %>%
  filter(erim_mkt == 1) %>%
  group_by(Code, display_4) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_4)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Sioux Falls") +
  guides(fill = guide_legend(title = "Display 4")) +
  theme_minimal()

### Springfield
## Share of Sales by Product and Market
df_analysing <- df %>%
  filter(erim_mkt == 2) %>%
  group_by(Code) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

ggplot(df_analysing, aes(x = Code, y = share)) +
  geom_bar(stat = "identity", fill = "#800020", color = "black") +
  labs(x = "Código da Loja", y = "Share") +
  ggtitle("Share de Vendas - Springfield") +
  theme_minimal()

## Share of Sales by Store and Market
df_analysing <- df %>%
  filter(erim_mkt == 2) %>%
  group_by(store_id) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count))

ggplot(df_analysing, aes(x = store_id, y = share)) +
  geom_bar(stat = "identity", fill = "#800020", color = "black") +
  labs(x = "Código da Loja", y = "Share") +
  ggtitle("Share de Vendas - Springfield") +
  theme_minimal()

## Number of Products in Display by Store and Code
### Display_1
df_analysing <- df %>%
  filter(erim_mkt == 2) %>%
  group_by(Code, display_1) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_1)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Springfield") +
  guides(fill = guide_legend(title = "Display 1")) +
  theme_minimal()

### Display_2
df_analysing <- df %>%
  filter(erim_mkt == 2) %>%
  group_by(Code, display_2) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_2)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Springfield") +
  guides(fill = guide_legend(title = "Display 2")) +
  theme_minimal()

### Display_3
df_analysing <- df %>%
  filter(erim_mkt == 2) %>%
  group_by(Code, display_3) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_3)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Springfield") +
  guides(fill = guide_legend(title = "Display 3")) +
  theme_minimal()

### Display_4
df_analysing <- df %>%
  filter(erim_mkt == 2) %>%
  group_by(Code, display_4) %>%
  summarise(count = n())

ggplot(df_analysing, aes(x = as.factor(Code), y = count, fill = display_4)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = c("N" = "#DC143C", "Y" = "#1E90FF")) +
  labs(x = "Código do Produto", y = "Contagem", title = "Contagem de Produtos em 'Display' - Springfield") +
  guides(fill = guide_legend(title = "Display 4")) +
  theme_minimal()

### On Display
df$display_1 <- ifelse(df$display_1 == "N", 0, 1)
df$display_2 <- ifelse(df$display_2 == "N", 0, 1)
df$display_3 <- ifelse(df$display_3 == "N", 0, 1)
df$display_4 <- ifelse(df$display_4 == "N", 0, 1)

display <- table(df$display_1)[2] + table(df$display_2)[2] + table(df$display_3)[2] + table(df$display_4)[2]
total <- nrow(df) - display

dados <- data.frame(type = c("Not On Display", "On Display"), value = c(total, display))

ggplot(dados, aes(x = type, y = value, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "Contagem de 'On Display'", x = NULL, y = NULL) +
  theme_minimal() +
  scale_fill_manual(values = c("#DC143C", "#1E90FF")) + 
  theme(legend.position = "none") 

# Exercício b)
## Sem Constantes
logit_nasc <- mlogit(chid ~ 0 + avg_price + display_1 + display_2 + display_3 + display_4, 
            data=Dat)

## Com Constantes
logit_asc <- mlogit(chid ~ 1 + avg_price + display_1 + display_2 + display_3 + display_4, 
            data=Dat)
summary(logit_asc)

stargazer(logit_nasc, logit_asc, type = "latex")

# Exercício c)
Dat$display_1 <- as.integer(factor(Dat$display_1, levels = c("N", "Y"))) - 1
Dat$display_2 <- as.integer(factor(Dat$display_2, levels = c("N", "Y"))) - 1
Dat$display_3 <- as.integer(factor(Dat$display_3, levels = c("N", "Y"))) - 1
Dat$display_4 <- as.integer(factor(Dat$display_4, levels = c("N", "Y"))) - 1

mean_matrix <- with(Dat[!is.na(Dat$chid),], 
                  data.frame(avg_price = tapply(avg_price, index(logit_asc)$alt, mean, na.rm=TRUE),
                             display_1 = tapply(display_1, index(logit_asc)$alt, mean, na.rm=TRUE),
                             display_2 = tapply(display_2, index(logit_asc)$alt, mean, na.rm=TRUE),
                             display_3 = tapply(display_3, index(logit_asc)$alt, mean, na.rm=TRUE),
                             display_4 = tapply(display_4, index(logit_asc)$alt, mean, na.rm=TRUE)))

matrix <- as.matrix(mean_matrix)
stargazer(matrix, type = "latex")

# Efeitos Marginais na Média do Display 2 - Logit Com Constantes
matrix_asc <- m.effects(logit_asc, covariate = "display_2", type="aa", data=mean_matrix)
matrix_asc[1, 1]
matrix_asc[3, 3]

# Efeitos Marginais na Média do Display 2 - Logit Sem Constantes
matrix_nasc <- m.effects(logit_nasc, covariate = "display_2", type="aa", data=mean_matrix)
matrix_nasc[1, 1]
matrix_nasc[3, 3]

# Tabela de Efeitos Marginais
matrix_me <- matrix(1:4, 2, byrow = TRUE)
dimnames(matrix_me) <- list(c("wo/ Constant", "Alternative-Specific Constant"),
                    c("Marginal Effect: Product 1", "Marginal Effect: Product 3"))
matrix_me[1,1] <- matrix_nasc[1,1]
matrix_me[2,1] <- matrix_asc[1,1]
matrix_me[1,2] <- matrix_nasc[3,3]
matrix_me[2,2] <- matrix_asc[3,3]

stargazer(matrix_me, title = "Marginal Effects", type = "latex")

# d)
## Assumindo coeficiente pro preço normalmente distribuído
logit_RC <- mlogit(chid ~ avg_price + display_1 + display_2 + display_3 + display_4, 
               data=Dat, panel = FALSE, rpar = c(avg_price = "n"), R = 100, method = "bhhh")
summary(logit_RC)

## Assumindo coeficiente pro preço lognormalmente distribuído
Dat2 <- Dat
Dat2$avg_price <- Dat2$avg_price*-1

logit_lnRC <- mlogit(chid ~ avg_price + display_1 + display_2 + display_3 + display_4, 
                     data = Dat2,
                     rpar = c(avg_price = "ln"), R = 100, halton = NA, method = "bhhh")
summary(logit_lnRC)

stargazer(logit_RC, logit_lnRC, type = "latex")

# Elasticidades Preço e Cruzadas - Random Coefficient Logit (Normal)
matrix_RC <- m.effects(logit_RC, covariate = "avg_price", type="rr", data=mean_matrix)
matrix_RC[1, 1]

matrix <- as.matrix(matrix_RC)
stargazer(matrix, type = "latex")

# Elasticidades Preço e Cruzadas - Random Coefficient Logit (lnNormal)
matrix_lnRC <- m.effects(logit_lnRC, covariate = "avg_price", type="rr", data=mean_matrix)
matrix_lnRC[1, 1]

matrix <- as.matrix(matrix_lnRC)
stargazer(matrix, type = "latex")


# Exercício e)
# Efeitos Marginais na Média do Display 2 - Random Coefficient Logit (Normal)
matrix_RC <- m.effects(logit_RC, covariate = "display_2", type="aa", data=mean_matrix)
matrix_RC[1, 1]

matrix <- as.matrix(matrix_RC)
stargazer(matrix, type = "latex")

# Efeitos Marginais na Média do Display 2 - Random Coefficient Logit (lnNormal)
matrix_lnRC <- m.effects(logit_lnRC, covariate = "display_2", type="aa", data=mean_matrix)
matrix_lnRC[1, 1]

matrix <- as.matrix(matrix_lnRC)
stargazer(matrix, type = "latex")

# g)


