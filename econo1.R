install.packages("haven")
install.packages("stargazer")
install.packages("dplyr")
install.packages("tydverse")
install.packages("ggplot2")
library(dplyr)
library(haven)
library(tidyr)
library(stargazer)


pnad <- read_stata("C:/Users/tjrgi/OneDrive/Documentos/Pnad Rios.dta") %>% select(c(Ano, Trimestre, UF, V2007, V2009, VD3005, V3001, V3007, V3008, V3009A, V3014, V4001, V4005, VD4011,
                                                                                    VD4001, V2010, V403311, V403312, VD4019, V4039, VD4031, V4071, V4003, VD4009, V4029, V4078A, VD4030, V4033, V40121, VD4008, VD2002, VD3004, idind, iddom, qdata,
                                                                                    V4010, V1022, V1023, posest, posest_sxi)) 
pnad %>% write_csv("C:/Users/tjrgi/OneDrive/Documentos/Painel 8 Limpo Gios.csv")


#Lendo o arquivo
pnad_clean <- readr::read_csv("C:/Users/tjrgi/OneDrive/Documentos/Painel 8 Limpo Gios.csv")
pnad_clean <- pnad_clean[pnad_clean$UF < 30 & pnad_clean$UF > 20,]


#PNAD domicilios unicos
pnad_sdom <- pnad_clean[!duplicated(pnad_clean$iddom_num), ] 

#UFs
table_UF <- table(pnad_sdom$UF)
table_UF


sum <- c(0,0)
for(x in 1:9){
  sum = sum + table_UF[x]
  print(x)
}
sum

num_dom_perUF <- list(NA)
estados <- list("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia")
for (i in 1:length(estados)) {
  num_dom_perUF[[i]] <- table_UF[x]
}


stargazer(table_UF, type = "ascii")


col <- c(10666, 3949, 8907, 3529, 4508, 7265, 6810, 3412, 9121, 58167)
col_freq <- col/58167
col_ibge <- c(2590426, 1419460, 3824577, 1501657, 1805260, 4094790, 1322185, 1007165, 6873605, 24439134)
col_freq_ibge <- col_ibge/24439134
A <- cbind(col, col_freq, col_ibge, col_freq_ibge)
colnames(A) <- c("Domícilios PNAD", "Freq. Rel.", "Domícilios Censo 2023", "Freq. Rel.")
rownames(A) <- estados
stargazer(A, type = "latex")

#V1022 e V1023 - Situação e Região Metropolitana
table(pnad_sdom$V1022, pnad_sdom$V1023)/58167

row1 <- c(0.195, 0.078, 0.008, 0.359)
row2 <- c(0.001, 0.021, 0.004, 0.330)
B <- rbind(col1, col2)
colnames(B) <- c("Capital", "RM", "RIDE", "Restante da UF")
rownames(B) <- c("Urbana", "Rural")
stargazer(B, type = "latex")




#-------------------------------------------------------------------------------------------------------------
# Mudando os nomes das variaveis - Análise Descritiva
pnad_clean$UF <- case_match(pnad_clean$UF,
                            21~"Maranhão",
                            22~"Piauí",
                            23~"Ceará",
                            24~"Rio Grande do Norte",
                            25~"Paraíba",
                            26~"Pernambuco",
                            27~"Alagoas",
                            28~"Sergipe",
                            29~"Bahia")

index = 1
sexo_list <- list("Homem", "Mulher")
yes_no_list <- list("Sim", "Não")
pnad_clean <- transform(pnad_clean,
  V2007 = ifelse(V2007==index, sexo_list[index], sexo_list[index+1], missing=NA),
  V4001 = ifelse(V4001==index, yes_no_list[index], yes_no_list[index+1], missing=NA),
  VD4001 = ifelse(VD4001==index, yes_no_list[index], yes_no_list[index+1], missing=NA),
  V4071 = ifelse(V4071==index, yes_no_list[index], yes_no_list[index+1], missing=NA),
  V4003 = ifelse(V4003==index, yes_no_list[index], yes_no_list[index+1], missing=NA),
  V4005 = ifelse(V4005==index, yes_no_list[index], yes_no_list[index+1], missing=NA))


pnad_clean$VD4011 <- case_match(pnad_clean$VD4011,
                            1~1,
                            2~2,
                            3~3,
                            4~4,
                            5~5,
                            6~6,
                            7~7,
                            8~8,
                            9~9,
                            10~10,
                            11~11)

pnad_clean$V403311 <- case_match(pnad_clean$V403311,
                            1~"Até 0,5SM",
                            2~"De 0,5SM até 1SM",
                            3~"De 1SM até 2SM",
                            4~"De 2SM até 3SM",
                            5~"De 3SM até 5SM",
                            6~"De 5SM até 10SM",
                            7~"De 10SM até 20SM",
                            8~"Acima de 20SM")

pnad_clean$V2007 <- case_match(pnad_clean$V2007,
                               "Homem"~1,
                               "Mulher"~2)

# Criando variável RPG, RDGu, RDGr

pnad_clean$Dom_Area <- ifelse(pnad_clean$V1022==1 & pnad_clean$V1023==4, "RDGu", 0)
pnad_clean$Dom_Area <- ifelse(pnad_clean$V1022==2 & pnad_clean$V1023==4, "RDGr", pnad_clean$Dom_Area)
pnad_clean$Dom_Area <- ifelse((pnad_clean$V1022==1 | pnad_clean$V1022==2) & (pnad_clean$V1023==1 | pnad_clean$V1023==2 | pnad_clean$V1023==3), "RPG", pnad_clean$Dom_Area)

# criar data em texto
subst <- data.frame(
  date = 238:246, 
  date_text = c("2019-Q3","2019-Q4",
                "2020-Q1","2020-Q2",
                "2020-Q3","2020-Q4",
                "2021-Q1","2021-Q2",
                "2021-Q3")
)
pnad_clean <- left_join(pnad_clean,subst)

pnad_clean$V2010 <- case_match(pnad_clean$V2010,
                          c(1,3) ~ "Branco",
                          c(2,4,5) ~ "Não Branco")

#------------------------------------------------- PNAD INDIVÍDUOS SINGULARES --------------------------------
pnad_sind <- pnad_clean[!duplicated(pnad_clean$idind_num), ] 
pnad_pea <- pnad_clean %>% filter(V2009 <= 65 & V2009 >= 14)


table(pnad_pea$UF, pnad_pea$VD4001) #estados x trabalha

table(pnad_sind$V2007, pnad_sind$VD4001) #sexo x trabalho

table(pnad_sind$V2010, pnad_sind$VD4001) #raça x trabalho # está faltando


qplot(data=pnad_pea, x=V2009, y=V403312)

#ano x trabalho
levels(pnad_clean$V2007)
#-------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(hrbrthemes)
library(ggridges)
library(viridis)
library(scales)

#Relação entre território e RDA - ok
df_Dom_Area <- read.csv("C:/Users/tjrgi/OneDrive/Documentos/RDA_pnad.csv")
RDA_matrix <- cbind(unlist(df_Dom_Area$UF), unlist(df_Dom_Area["RPG"]), unlist(df_Dom_Area["RDGu"]), unlist(df_Dom_Area["RDGr"]))
stargazer(RDA_matrix, type="latex")


# Boxplot de salário por região - ok
ggplot(pnad_clean, aes(y=Dom_Area, x=VD4019, fill=Dom_Area)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01) + 
  theme_ipsum() +
  xlim(0, 5000) +
  scale_fill_brewer(palette = "Greens") +
  labs(x="Região por Domínio e Área (RDA)", y="Rendimento Habitual Mensal", fill="RDA") 
  

# Densidade Horas trabalhadas por sexo - ok
ggplot(pnad_clean,
       aes(V4039, fill=factor(V2007))) +
  geom_density(alpha = 0.8, linewidth=0.2)  +
  theme_ipsum() +
  scale_fill_discrete(direction=-1, labels=c("Homem", "Mulher")) +
  labs(x="Horas trabalhadas semanais", y="Densidade", fill="Sexo") 

# Densidade Horas trabalhadas por raça - ok
ggplot(pnad_clean %>% drop_na(V2010),
       aes(V4039, fill=factor(V2010))) +
  geom_density(alpha = 1, linewidth=0.6)  +
  theme_ipsum() +
  scale_fill_discrete(direction=1, name="Raça", labels=c("Branco", "Não Branco")) +
  scale_fill_brewer(palette="Blues") +
  xlim(0, 100) +
  labs(x="Horas trabalhadas semanais", y="Densidade", fill="Raça") 

# Ocupação por rendimento médio
ggplot(wage_ocup_colour_df, aes(x=as.double(Salario), y=factor(Grupamento_Ocupacional))) +
  geom_point(size=3, alpha=1, aes(color=Raça)) +
  geom_line(size=.2, line_width=.8, alpha=.7, aes(group=Grupamento_Ocupacional)) +
  scale_y_discrete(limits=c(1:11)) +
  theme(panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major.x = element_line(colour = "gray")) +
  xlim(0, 6000) +
  scale_colour_manual(values=c("#08306B", "#4292C6")) +
  labs(x="Média de Rendimento Habitual Mensal", y="Grupamentos Ocupacionais")

# Rendimentos por unidade federativa - boxplot - ok
ggplot(pnad_clean, aes(x=UF, y=VD4019, fill=UF)) +
  geom_boxplot() + 
  scale_fill_discrete(direction=-1) +
  scale_fill_brewer(palette="Blues") +
  theme_ipsum() +
  ylim(0, 2500) +
  labs(x="Unidade Federativa", y="Rendimento Habitual Mensal") +
  scale_x_discrete(labels=c(
    "Alagoas" = "AL",
    "Bahia" = "BA",
    "Ceará" = "CE",
    "Maranhão" = "MA",
    "Paraíba" = "PB",
    "Pernambuco" = "PE",
    "Piauí" = "PI",
    "Rio Grande do Norte" = "RN",
    "Sergipe" = "SE"
  ))


# Rendimentos por faixa de salário - ok
ggplot(pnad_clean %>% drop_na(V403311, V2010), aes(y=V403311, fill=V2010)) +
  geom_histogram(binwidth = 0.5, stat="count") +
  scale_y_discrete(limits=c(
    "Até 0,5SM",
    "De 0,5SM até 1SM",
    "De 1SM até 2SM",
    "De 2SM até 3SM",
    "De 3SM até 5SM",
    "De 5SM até 10SM",
    "Acima de 20SM"
  ))  +
  scale_fill_discrete(direction=-1) +
  scale_fill_brewer(palette="Blues") +
  labs(x="Faixa de Rendimento Habitual Mensal", y="Contagem", fill="Raça") 

# Rendimento por faixa de salário e raça - ok
pnad_clean %>% drop_na(V403311,V2010) %>%
  ggplot(aes(y=V403311)) + 
  geom_bar(aes(x=..count../sum(..count..),fill=V2010),
           position="fill",color="black") + theme_ipsum() + 
  scale_x_continuous(labels = percent) +
  scale_y_discrete(limits=c(
    "Até 0,5SM",
    "De 0,5SM até 1SM",
    "De 1SM até 2SM",
    "De 2SM até 3SM",
    "De 3SM até 5SM",
    "De 5SM até 10SM",
    "De 10SM até 20SM",
    "Acima de 20SM"
  )) +
  scale_fill_brewer(palette="Blues") +
  labs(x="Faixa de Rendimento Habitual", y="Frequência Relativa", fill="Raça")

# Scatter plot com relação idade x raça - brancos
gp1 <- ggplot(pnad_clean[pnad_clean$V2010=="Branco",] %>% drop_na(V2010), aes(x=V2009, y=VD4019)) +
  geom_point(color="#08306B") +
  geom_smooth(method="gam", color=c("darkred"), fullrange=TRUE) +
  ylim(0, 2500) +
  xlim(0, 80) +
  theme_ipsum() +
  labs(x="Idade", y="Rendimento Habitual Mensal", color="Raça", 
       title="Brancos")

#
# Scatter plot com relação idade x raça - não brancos
gp2 <- ggplot(pnad_clean[pnad_clean$V2010=="Não Branco",] %>% drop_na(V2010), aes(x=V2009, y=VD4019)) +
  geom_point(color="#4292C6") +
  geom_smooth(method="gam", color=c("darkred"), fullrange=TRUE) +
  ylim(0, 2500) +
  xlim(0, 80) +
  theme_ipsum() +
  labs(x="Idade", y="Rendimento Habitual Mensal", color="Raça",
       title="Não Brancos")


ggplot(pnad_clean %>% drop_na(V2010), aes(x=V2009, y=VD4019, color=V2010)) +
  geom_point(size=0.5, alpha=0.4) +
  geom_smooth(method="gam", span=0.4) +
  ylim(0, 2500) +
  xlim(12, 80) +
  theme_ipsum() +
  scale_color_manual(values=c("#08519C", "#4292C6")) +
  labs(x="Idade", y="Rendimento Habitual Mensal", color="Raça")


library("gridExtra")                        # Load gridExtra package
grid.arrange(gp1, gp2, ncol=2)


grupo_ocup <- list("Diretores e gerentes",
                   "Profissionais das ciências e intelectuais",
                   "Técnicos e profissionais de nível médio",
                   "Trabalhadores de apoio administrativo",
                   "Trabalhadores dos serviços, vendedores dos comércios e mercados",
                   "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca",
                   "Trabalhores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios",
                   "Operadores de instalações e máquinas e montadores",
                   "Ocupações elementares",
                   "Membros das forças armadas, policiais e bombeiros militares",
                   "Ocupações maldefinidas")

grupo_ocup_index <- list("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")


mean_wage <- c(
  mean(pnad_clean$VD4019[pnad_clean$VD4011==1 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==1 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==2 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==2 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==3 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==3 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==4 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==4 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==5 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==5 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==6 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==6 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==7 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==7 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==8 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==8 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==9 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==9 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==10 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==10 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==11 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
  mean(pnad_clean$VD4019[pnad_clean$VD4011==11 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE)
)
round(mean_wage,2)
colour <- c("Branco", "Não Branco", "Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco","Branco", "Não Branco") 
ocupation <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11)
wage_ocup_colour <- cbind(round(mean_wage,2), colour, ocupation)
colnames(wage_ocup_colour) <- c("Salario", "Raça", "Grupamento_Ocupacional")
wage_ocup_colour_df = as.data.frame(wage_ocup_colour)

# tabela ocupações x raça
mean_wage_white <- c(mean(pnad_clean$VD4019[pnad_clean$VD4011==1 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==2 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==3 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==4 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==5 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==6 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==7 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==8 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==9 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==10 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                        mean(pnad_clean$VD4019[pnad_clean$VD4011==11 & pnad_clean$V2010=="Branco"], na.rm = TRUE))

mean_wage_non_white <- c(mean(pnad_clean$VD4019[pnad_clean$VD4011==1 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==2 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==3 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==4 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==5 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==6 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==7 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==8 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==9 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==10 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                            mean(pnad_clean$VD4019[pnad_clean$VD4011==11 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE))

mean_wage <- c(mean(pnad_clean$VD4019[pnad_clean$VD4011==1], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==2], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==3], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==4], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==5], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==6], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==7], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==8], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==9], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==10], na.rm=TRUE),
                  mean(pnad_clean$VD4019[pnad_clean$VD4011==11], na.rm=TRUE))

sum_wage <- c(sum(pnad_clean$VD4019[pnad_clean$VD4011==1], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==2], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==3], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==4], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==5], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==6], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==7], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==8], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==9], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==10], na.rm=TRUE),
              sum(pnad_clean$VD4019[pnad_clean$VD4011==11], na.rm=TRUE))


sum_wage_white <- c(sum(pnad_clean$VD4019[pnad_clean$VD4011==1 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==2 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==3 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==4 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==5 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==6 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==7 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==8 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==9 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==10 & pnad_clean$V2010=="Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==11 & pnad_clean$V2010=="Branco"], na.rm = TRUE))

sum_wage_non_white <- c(sum(pnad_clean$VD4019[pnad_clean$VD4011==1 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==2 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==3 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==4 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==5 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==6 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==7 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==8 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==9 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==10 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE),
                    sum(pnad_clean$VD4019[pnad_clean$VD4011==11 & pnad_clean$V2010=="Não Branco"], na.rm = TRUE))


count_total <- sum_wage/mean_wage

percentage_white <- sum_wage_white/(mean_wage_white*count_total)
percentage_non_white <- sum_wage_non_white/(mean_wage_non_white*count_total)

table_ocupation <- cbind(ocupation, percentage_white, mean_wage_white, percentage_non_white, mean_wage_non_white, mean_wage)
colnames(table_ocupation) <- c("#", "% Brancos", "Média Rendimentos Brancos", "% Não Brancos", "Média Rendimentos Não Brancos", "Média de Rendimentos")
stargazer(table_ocupation, type = "latex")

table_ocupation2 <- cbind(ocupation, grupo_ocup)
colnames(table_ocupation2) <- c("#", "Grupamento Ocupacional")
stargazer(table_ocupation2, type = "latex")

# Curva de Lorenz

