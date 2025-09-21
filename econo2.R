library(stargazer)
library(tidyr)
library(dplyr)

pnad_transform <- readr::read_csv("C:/Users/tjrgi/OneDrive/Documentos/Painel 8 Transformado Rivers.csv")

pnad_transform$UF <- case_match(pnad_transform$UF,
                            21~"MA",
                            22~"PI",
                            23~"CE",
                            24~"RN",
                            25~"PB",
                            26~"PE",
                            27~"AL",
                            28~"SE",
                            29~"BA")

# criar a variável salário hora
pnad_transform <- pnad_transform %>% mutate(log_sal_h = log( rend_mensal_tds/(4*hrs_sem_tds)) )



#Questão 2, parte 1

mqo.i <- lm(log_sal_h ~ anos_estudo + I(idade-6) + I((idade-6)^2) + PPI + sexo + casado + UF + date_text,
       data=pnad_transform)

#summary(mqo.i)


mqo.ii <- lm(log_sal_h ~ anos_estudo + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) +
           sexo + casado + PPI + UF + date_text,
         data=pnad_transform)

mqo.iii <- lm(log_sal_h ~ anos_estudo + PPI + sexo + I(idade-6) + I((idade-6)^2) + I((idade-6)^3)  + casado + UF +
           date_text + dom_area, data=pnad_transform)

mqo.iv <- lm(log_sal_h ~ anos_estudo + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
           UF + date_text + factor(grp_ocup), data=pnad_transform)

mqo.v <- lm(log_sal_h ~ anos_estudo + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
          UF + date_text + factor(grp_ocup), data=pnad_transform, weights=pnad_transform$V1028)

mqo.v.hccm <- coeftest(mqo.v, vcov.=vcovHC, type="HC1")
summary(mqo.v.hccm)

stargazer(mqo.v.hccm, type="latex")

stargazer(mqo.i, mqo.ii, mqo.iii, mqo.iv, type="text") 
stargazer(mqo.iv, mqo.v, type="latex") #sem e com peso do modelo preferido - lembrar do erro robusto


#----------------------------------------------------------------------------------------------------------------------------------------
# Teste de Chow
library(strucchange)
sctest(mqo.v, type="Chow")

#----------------------------------------------------------------------------------------------------------------------------------------
# Teste de Heterocedasticidade
# BPtest or White Test
library(lmtest)
bptest(mqo.v)

# Test Ramsey-RESET
resettest(mqo.v)
resettest(mqo.iv)

#----------------------------------------------------------------------------------------------------------------------------------------
# Variável instrumental 
# Fazer a de trimestre - pesquisar

library(AER)
pnad_transform <- pnad_transform %>% filter( casado==1)

rdef <- lm(anos_estudo ~ educ_conj + I(log(num_escolas_nasc/pop_nasc)) + part_pib_nasc, data=pnad_transform)

mq2e <- ivreg(log_sal_h ~ anos_estudo + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
                UF + date_text + factor(grp_ocup) | educ_conj + I(log(num_escolas_nasc/pop_nasc)) + part_pib_nasc + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
                UF + date_text + factor(grp_ocup),
               data=pnad_transform, weights=pnad_transform$V1028)

stargazer(rdef, type="latex")
stargazer(mqo.v, mq2e, type="latex")

summary(mq2e, diagnostics = TRUE)

#----------------------------------------------------------------------------------------------------------------------------------------
#Questão 3 - Paineis

library(plm)
#Generate pdata.frame
pnad.p <- pdata.frame(pnad_transform, index=c("idind_num", "date"))

#-------------------------------------------------------------------------------------------------------------
# Avaliar o painel
# xtdescribe
pdim(pnad.p)

# Avaliar educ
summary(pnad.p$anos_estudo)
table(pnad.p$anos_estudo)


#-------------------------------------------------------------------------------------------------------------
# Retiramos de FD e FE as constantes
reg.fd <- plm(log_sal_h ~ anos_estudo  + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
                UF + date_text + factor(grp_ocup), data=pnad.p, model="fd")
  
reg.fe <- plm(log_sal_h ~ anos_estudo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
                UF + date_text + factor(grp_ocup), data=pnad.p, model="within")

summary(reg.fe)

summary(reg.fd)
  
reg.re <- plm(log_sal_h ~ anos_estudo + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
                UF + date_text + factor(grp_ocup), data=pnad.p, model="random")

summary(reg.re)

stargazer(reg.fd, reg.fe, reg.re, type="latex")

#-------------------------------------------------------------------------------------------------------------
# Consertando erros de sexo e raça
argmode <- function(v) {
  uniqv <- unique(v)
  return( which.max(tabulate(match(v, uniqv))) )
}
pnad.p <- pnad.p %>% group_by(idind_num) %>% mutate(PPI = PPI[argmode(PPI)])
pnad.p <- pnad.p %>% group_by(idind_num) %>% mutate(sexo = sexo[argmode(sexo)])

# Novas regressões tendo consertado
reg.fd.out <- plm(log_sal_h ~ anos_estudo + PPI + sexo + dom_area  + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
  UF + date_text + factor(grp_ocup), data=pnad.p, model="fd")

summary(reg.fd.out)

reg.fe.out <- plm(log_sal_h ~ anos_estudo + PPI + sexo + dom_area  + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + casado + 
  UF + date_text + factor(grp_ocup), data=pnad.p, model="within")

summary(reg.fe.out)

stargazer(reg.fd.out, reg.fe.out, reg.re, type="text",
          column.labels=c("FD", "FE", "RE"))

#----------------------------------------------------------------------------------------------------------------------------------------
#Questão 3 - Paineis
# Hausman test of RE vs FE
phtest(reg.fe, reg.re)
phtest(reg.fe.out, reg.re)


# Teste de autocorrelação FE vs FD
pbgtest(reg.fd)
pbgtest(reg.fe)


#---------------------------------------------------------------------------------------------------
# Tabela MQO x MQ2E x PD X FD X RE
stargazer(mqo.v, mq2e, reg.fd, reg.fe, reg.re, type="latex")


#----------------------------------------------------------------------------------------------------------------------------------------
# Tobit
# left + right é a soma de desocupado + fora da força que tem que ser censurado

pnad_transform$rend_mensal_tds <- case_when(
  (is.na(pnad_transform$rend_mensal_tds==TRUE)) ~ 0
) 


library(AER)

TobitRes <- censReg(log_sal_h ~ anos_estudo + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + UF + date_text + casado
                     + factor(grp_ocup), left = -3, right = 7, data = pnad_transform)



summary(TobitRes)
stargazer(mqo.v, TobitRes, type="latex")

margEff(TobitRes)

library(margins)
m1 <- margins(TobitRes)

stargazer(m1, type="text")

# Vies de Seleção Amostral (procedimento de Heckman)
# verificar se o lambda é significante
library(sampleSelection)





# Simulated data
set.seed(93876553)
x <- sort(rnorm(100)+4)
xb <- -4 + 1*x
ystar <- xb + rnorm(100)
y <- ystar
y[ystar<0]<- 0
# Conditional means
Eystar <- xb
Ey <- pnorm(xb/1)*xb+1*dnorm(xb/1)
# Graph
plot(x,ystar,ylab="y", pch=3)
points(x,y, pch=1)
lines(x,Eystar, lty=2,lwd=2)
lines(x,Ey , lty=1,lwd=2)
abline(h=0,lty=3) # horizontal line at 0
legend("topleft",c(expression(y^"*"),"y",expression(E(y^"*")),"E(y)"),
       lty=c(NA,NA,2,1),pch=c(3,1,NA,NA),lwd=c(1,1,2,2))








pnad_transform <- pnad_transform %>% filter(is.na(pnad_transform$sexo)==F)
res <- selection(empregado ~ anos_estudo + PPI + casado + num_filhos_menor10  + num_filhos_10a18 + num_filhos_maior18 + np_income + part_pib + pop + dom_area + date_text, 
                 log_sal_h ~ anos_estudo + PPI  + dom_area + date_text + casado,
                   data=pnad_transform, method="m2")

summary(res)

res2 <- heckit(empregado ~ anos_estudo + PPI + casado + num_filhos_menor10  + num_filhos_10a18 + num_filhos_maior18 + np_income + part_pib + pop + dom_area + date_text, 
               log_sal_h ~ anos_estudo + PPI + sexo + dom_area + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + UF + date_text + casado,
               data=pnad_transform)


summary(res2)




summary(pnad_transform$empregado)

pnad_transform <- pnad_transform %>% filter(pnad_transform$empregado==1)

library(sampleSelection)
heckMJH = heckit(empregado ~ anos_estudo + PPI + casado + np_income + num_filhos_menor10  + num_filhos_10a18 + num_filhos_maior18 + np_income + I(log(pop)) + part_pib + dom_area + date_text, 
                 log_sal_h ~ anos_estudo + PPI  + dom_area + casado + I(idade-6) + I((idade-6)^2) + I((idade-6)^3) + UF + date_text  + factor(grp_ocup),
                 data=pnad_transform)                   

summary(heckMJH)
stargazer(mqo.v, heckMJH,  type="latex")
  