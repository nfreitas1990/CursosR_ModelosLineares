
# TCC Modelos Lineares - CursoR 
# setembro 2022
# Natália Freitas



# Pacotes -----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(corrplot)


# Dados -------------------------------------------------------------------
data(mtcars)


# Guia da Análise ---------------------------------------------------------

# 1. Analise Descritiva dos Dados
#   a. Conhecer os dados
#   b. Avaliar natureza das variáveis
#   c. Avaliar colinearidade entre variáveis
#   d. verificar linearidade

# 2. Modelagem
# 4. Análise dos Pressupostos
# 5. Conclusão



# 1. Análise Descritiva ---------------------------------------------------

#   a. Conhecer os dados
str(mtcars)
names(mtcars)
summary(mtcars)
?mtcars
skimr::skim(mtcars)

Tabela <- data.frame(Siglas = colnames(mtcars), Significado = c("Consumo Milhas/galão","Número de cilindros",
                                                                "Deslocamento","Potência bruta",
                                                                "Relação do eixo traseiro", 
                                                                "Peso (1000 libras)",
                                                                "Tempo de 1/4 de milha",
                                                                "Motor (0|FormaV; 1|Reto",
                                                                "Transmissão (0|Automática, 1|Manual)",
                                                                "Número de marchas",
                                                                "Número de carburadores"))




#   a.1. Avaliando NAs
#   Conclusão: nenhum NA identificado
  table(map(mtcars, is.na))


#   b. Avaliar natureza das variáveis
# Conclusão: Transformar as variáveis para o tipo adequado 
view(mtcars)

mtcars <- mtcars |> 
            mutate(vs = as.factor(vs),
                   am = as.factor(am),
                   cyl = as.integer(cyl),
                   gear = as.integer(gear),
                   carb = as.integer(carb))



#   c. Avaliar colinearidade entre variáveis
# Conclusão: Variáveis correlacionadas: cyl|disp|hp|wt
dados_cor <- mtcars  |> 
              select (where(is.numeric))

corrplot.mixed(cor(dados_cor, method = "spearman"),lower = "number", upper = 'color')


#   d. verificar linearidade
  par(mfrow = c (5,2))    
  plot(mpg ~ . , data = mtcars, pch= 16)
    
 
  # Junção dos gráficos
  par(mfrow = c (3,1))
  
  # disp - transformar para log()
  modelo_disp <- lm (mpg ~ disp, mtcars)
  MASS::boxcox (modelo_disp)
  
  # hp - transformar log
  modelo_hp <- lm (mpg ~ hp, mtcars)
  MASS::boxcox (modelo_hp)
  
  # wt
  modelo_wt <- lm (mpg ~ wt, mtcars)
  MASS::boxcox (modelo_wt)
  
# Conclusão: o lambda próximo a zero é um indicativo de que o log ()
# resolveria o problema de linearidade.
  
mtcars |> 
  mutate (log_disp = log(disp),
         log_hp = log(hp),
         log_wt = log(wt)) |> 
  select (mpg,disp,hp,wt,log_disp,log_hp,log_wt) 

  
  
# 2. Modelagem 
    
# Modelagem individual

for(i in 1:length(mtcars)){
      modelo <- lm(mpg ~ mtcars[,i], data = mtcars)
      lista_resultado[[i]] <- modelo
    }
names(lista_resultado) <- names(mtcars)      

# Visualização

# cyl
summary(lista_resultado$cyl)
plot(lista_resultado$cyl) 

# disp - não está bom os residuos
summary(lista_resultado$disp)  
plot(lista_resultado$disp)

# hp - não está bom os residuos
summary(lista_resultado$hp)  
plot(lista_resultado$hp)

# drat 
summary(lista_resultado$drat)  
plot(lista_resultado$drat)

# wt -  não está bom os residuos | ponto influente
summary(lista_resultado$wt)  
plot(lista_resultado$wt)

# qsec 
summary(lista_resultado$qsec)  
plot(lista_resultado$qsec)

# vs 
summary(lista_resultado$vs)  
plot(lista_resultado$vs)

# am 
summary(lista_resultado$am)  
plot(lista_resultado$am)

# gear
summary(lista_resultado$gear)  
plot(lista_resultado$gear)

# carb - residuo/ ponto influente
summary(lista_resultado$carb)  
plot(lista_resultado$carb)

# Visualização Automática - mas não consegui colocar o nome de cada variável
for (i in 1:length(lista_resultado)){
  summary(lista_resultado[[i]])
  par(mfrow = c (2,2))
  plot(lista_resultado[[i]])
  title(main = vetor_nomes[i])
} 
