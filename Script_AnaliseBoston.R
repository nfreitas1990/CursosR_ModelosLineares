# Analise Modelos Lineares
# Curso-R
# Natália Freitas de Souza


# Pacotes -----------------------------------------------------------------
library(tidyverse) 
library(broom)
library(purrr)

# Dados -------------------------------------------------------------------
library(MASS)
glimpse(Boston)
view(Boston)

#   Descriçao das variáves
help(Boston)
str(Boston)
summary(Boston)
table(Boston$rad)

# Ajuste: variaveis
Boston <- Boston |> 
            mutate(chas= as.factor(chas))

# Analise gráfica
plot(Boston)

graficos <- Boston|> 
              map( ~ {ggplot(Boston, aes(y = medv, x = .))+ geom_point()})

graficos$crim   # taxa de crime percapita por cidade
graficos$zn     # proporção de terrenos residenciais zoneados
graficos$indus  # proporção de acres de negócios não varejistas por cidade
graficos$chas   #Variável dummy de Charles River (= 1 se o trecho limita o rio; 0 caso contrário).
graficos$nox    #concentração de óxidos de nitrogênio (partes por 10 milhões)
  