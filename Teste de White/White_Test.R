# Bibliotecas que precisamos
library(wooldridge)
library(ggplot2)
library(lmtest)

# Dados
data("card")
data("hprice2")

# Olhando nosso dataset
card

# Visualizando a relação entre "wage" e "educ" no dataset
ggplot(card, aes(x = educ, y = wage)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

# Olhando o dataset do Teste de Hipótese
knitr::kable(hprice2[c("crime", "dist", "lprice")][1:10, ], "latex")

# Criando um modelo para lprice
model <- lm(lprice ~ crime + dist, data = hprice2)

# Performando o teste
bptest(model, ~ crime + dist + I(crime*dist) + I(crime^2) + I(dist^2), data = hprice2)
