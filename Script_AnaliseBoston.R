# Analise Modelos Lineares
# Curso-R
# Natália Freitas de Souza


# Pacotes -----------------------------------------------------------------
library(tidyverse) 
library(broom)
library(purrr)
library(dplyr)
library(corrplot)

# Dados -------------------------------------------------------------------
library(MASS)  # cuidado pq o select() existe no MASS e no dplyr, acaba dando conflito
glimpse(Boston)
view(Boston)
#   Descriçao das variáves
help(Boston)

# Objetivo ----------------------------------------------------------------
# Analise preditiva dos preços das habitações em Boston


# Metodologia -------------------------------------------------------------
# Para fazer uma análise preditiva dos preços da residencia. Utilizaremos
# as variáveis disponíveis no banco de dados para verificar quais variáveis, 
# estão relacionadas e influenciam no preço das residencias, para estas serem 
# utilizadas como variáveis prediditivas.


# Estratégia --------------------------------------------------------------

# 1. Análises descritivas
# 2. Avaliar as variáveis preditivas a serem utilizadas
# 3. Elaborar o modelo preditivo



# 1. Analise Descritivas -----------------------------------------------------

      # a. Avaliar natureza das variáveis
      # b. Avaliar colinearidade entre variáveis
      # c. verificar linearidade
      
      

# a. Natureza das variáveis
  # Disponibilidade de 14 variáveis, com 506 observações
  str(Boston)
  
  # Ajuste: variaveis categoricas
  Boston <- Boston |> 
    mutate(chas= as.factor(chas),
           rad = as.factor(rad))
  
    summary(Boston)

  

# b. Avaliar a colinearidade das variáveis 
# DUVIDA: Incluo a variável resposta na correlação?
# DUVIDA: Como seleciono?

  # Teste de correlação entre variáveis 
  # Escolha do método spearman foi para não assumir pressuposto de normalidade
  # que ia requerer para usar pearson.
  dados_cor <- Boston  |> 
                dplyr::select (where(is.numeric)) 
  
  corrplot(cor(dados_cor, method = "spearman"), method="number")
  
  
 # Conclusão da correlação (>0.7)
  
  # crim: indus | nox | age | dis | tax     (seleciono)
  # zn: -                                   (seleciono)
  # indus: crim | nox | dis                            
  # nox: crim | indus | age | dis                      
  # rm: -                                   (seleciono)
  # age: crim | nox | dis                              
  # dis: crim | indus | nox | age                      
  # tax: crim                                          
  # Istat: -                                (seleciono)
  

# Conclusão: indus | nox | crim | dis estão relacionados.
# DUVIDA: Seria melhor descartar alguma dessas variáveis, 
# ou seria melhor redimensionar através de uma PCA e conseguir
# e usar os eixos significativos da PCA?
  
  
# c. verificar se as variáveis são lineares
  
  # Opção 1 - simplificada
  par(mfrow = c (2,2))
  plot(medv ~ . , data=Boston)

  
  # Verificar transformações
 
  #crim 
  modelo <- lm (medv ~ log(crim), Boston)
  MASS::boxcox (modelo)
  par(mfrow = c (2,2))
  plot(modelo)
  
  #indus
  modelo <- lm (medv ~ log(indus), Boston)
  MASS::boxcox (modelo)
  par(mfrow = c (2,2))
  plot(modelo)
  
  #nox
  modelo <- lm (medv ~ log(nox), Boston)
  MASS::boxcox (modelo)
  par(mfrow = c (2,2))
  plot(modelo)

  
  #rm
  modelo <- lm (medv ~ age, Boston)
  MASS::boxcox (modelo)
  par(mfrow = c (2,2))
  plot(modelo)
  plot(medv ~ log(age), Boston)
  
  
  
  Boston |> 
    
    dplyr::select (where(is.numeric)) |>
           mutate(logcrim = log(crim),
                  logindus = log(indus),
                  lognox = log(nox))
  
  
  # Opção 2
  graficos <- Boston|> 
              dplyr::select (where(is.numeric)) |>
              mutate(logcrim = log(crim),
                     )  |>   
                map( ~ {ggplot(Boston, aes(y = medv, x = .))+ geom_point()})
  
  
  
  
  
  
  # Olhar a var. resposta vs var. explicativas,individualmente
  # Var. resposta > medv - preço mediano das habitações do bairro
  
    graficos$logcrim   # taxa de crime percapita por cidade
    graficos$zn     # proporção de terrenos residenciais zoneados
    graficos$indus  # proporção de acres de negócios não varejistas por cidade
    graficos$nox    # concentração de óxidos de nitrogênio (partes por 10 milhões)
    graficos$rm     # número médio de quartos por habitação.
    graficos$age    # proporção de unidades ocupadas pelos proprietários construídas antes de 1940
    graficos$dis    # média ponderada das distâncias para cinco centros de emprego de Boston.
    graficos$tax    # taxa de imposto de propriedade de valor total por $ 10.000.
    graficos$ptratio # relação aluno-professor por cidade.
    graficos$black   # 1000(Bk - 0,63)^2 onde Bk é a proporção de negros por cidade.
    graficos$lstat   # status mais baixo da população (porcentagem).
  
  # variáveis categoricas
    boxplot(medv ~ chas, Boston) # Variável dummy de Charles River (= 1 se o trecho limita o rio; 0 caso contrário).
    boxplot(medv ~ rad, Boston)  # índice de acessibilidade às rodovias radiais.
  
  
  # não parece linear: crim, indus, dis, black, zn
  # parece linear: nox, rm, age, ptratio, lstat
  # sem conclusões:tax
    
  # Conclusoes a partir dos graficos:
    # Parece que:
    
  # 1. Onde a taxa de crime é baixa temos desde casas baratas até casas caras, mas 
  #    conforme a taxa de crime aumenta, temos uma diminuição no valor da casa.
  #    relação não parece linear.
  
  # 2. A medida em que os acres de negócios aumentam, diminui o preço das habitações.
  
  # 3. O preço das casas é maior nos trechos q limitam o rio;
  
  # 4. O aumento da concentração de oxidos de nitrogenio, diminuem o 
  #    preço da habitação
  
  # 5. Aumento do numero de quartos, aumenta o valor da casa. Parece uma relação linear
  
  # 6. Distancias reduzidas para os centros de emprego possuem casas com preços mais reduzidos,
  #    conforme aumenta a distancia, os preços aumentam até um ponto mediano de valor
  #    onde se concentram apos aumentar mais de 2.5
  
  # 7.Onde temos um percentual mais baixo em status da população, são os locais com 
  #   o menor preço das habitações. Relação não parece linear 
    


# Análise -----------------------------------------------------------------

  # a. Regressão Linear Simples  
    
    mod_simples <- lm (medv ~ rm, Boston) 
    summary(mod_simples)
    
    # Conclusão: O número de quarto por habitação (rm) explica 48% da variação
    # no preço das habitações. O aumento no número de quarto em 1 unidade eleva
    # o preço em aproximadademente $9 mil.

    # resultados 
    coef(mod_simples)       # coeficientes
    confint(mod_simples)    # intervalo confianca
    predict(mod_simples)    # valores preditos
    predict(mod_simples, interval = "confidence") # valores preditos c/ intervalos
   res <-  augment(mod_simples)    # resíduos|cook distance|erro padrão|intervalos de confiança
    
    
  # Grafico da regressao 
   ggplot(Boston, mapping = aes(y = medv, x=rm))+
      geom_point()+
      geom_line(res, mapping = aes(y =.fitted , x=rm))+
      theme_classic()
  # ou
   plot(medv ~ rm, Boston)
   abline(mod_simples)
   
   # Grafico dos resíduos
   ggplot(res, mapping = aes(y =.resid, x=.fitted))+
     geom_point()+
     theme_classic()
    
   # b. Regressão Multipla
   
   
   
   
      