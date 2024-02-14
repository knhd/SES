#-----------------------Définition espace de travail et chargement des données-----------
library(tidyverse)
library(haven)
library(questionr)
library(GGally)
library(readxl)
library(patchwork)
library(esquisse)
library(MetBrewer)
library(readr)
library(gtsummary)
library(gghighlight)
library(ggrepel)
library(ggeasy)

#---------------------Données------------------
##Windows
stat_ses <- read_excel("C:/Users/Kevin/Cozy Drive/GitHub/SES/data/statistiques_ses.xlsx")
concours_ses <- read_excel("C:/Users/Kevin/Cozy Drive/GitHub/SES/data/statistiques_ses.xlsx", sheet = "concours")
series <- read_excel("C:/Users/Kevin/Cozy Drive/Recherche/Histoire education/kh-sl-evolution_nb_eleves.xlsx", 
                     sheet = "detail_2d_2c_GT")
bac <- read_excel("C:/Users/Kevin/Cozy Drive/GitHub/SES/data/statistiques_ses.xlsx", sheet = "baccalaureat")
pcs <- read_excel("C:/Users/Kevin/Cozy Drive/GitHub/SES/data/statistiques_ses.xlsx", sheet = "eleves_pcs_reg")
pcs_detail <- read_excel("C:/Users/Kevin/Cozy Drive/GitHub/SES/data/statistiques_ses.xlsx", sheet = "eleves_pcs_detail")
age <- read_excel("C:/Users/Kevin/Cozy Drive/GitHub/SES/data/statistiques_ses.xlsx", sheet = "ens_age")

#------------Tidying et reordonner data -------------
ses <- stat_ses %>% 
  pivot_longer(cols = starts_with("eleves_"), names_to="niveau", names_prefix="eleves_", values_to="nb_eleves")
ses <- ses %>% 
  pivot_longer(cols = starts_with("ens_"), names_to=c("statut","sexe"), names_prefix="ens_", names_sep="_", values_to="nb_ens")
ses$statut_rec <- fct_recode(ses$statut,  "Total" = "total",   "Non Titulaires" = "nontit",  "Agrégé·e·s et certifié·e·s" = "agregcert", "Agrégé·e·s" = "agreg",
                             "Certifié·e·s" = "cert",   "Stagiaires" = "stag",   "Adjoint·e·s et chargé·e·s d'enseignement" = "adjce")
ses$statut <- fct_relevel(ses$statut,"total", "nontit", "agreg", "cert", "stag", "adjce")
ses$niveau <- fct_relevel(ses$niveau, "total", "term_option", "term", "premiere_option", "premiere", "seconde")

series <- series %>% pivot_longer(cols = starts_with(c("Seconde", "Premiere", "Terminale", "LyceeGT")), names_to=c("classe", "serie"), names_sep="_", values_to="nb_eleves")
series$serie <- factor(series$serie)
series$filiere <- series$serie %>%  fct_recode("Général" = "A","Général" = "A/L","Général" = "AB","Général" = "B/ES","Général" = "C",
    "Général" = "clm","Général" = "D","Général" = "E/S-SI","Technologique" = "F","Technologique" = "G-H/STMG","Général" = "general",
    "Général & Technologique" = "GT","Général" = "S","Général" = "S-SVT","Technologique" = "ST2S","Technologique" = "STI","Technologique" = "STL",
    "Technologique" = "T","Technologique" = "techno","Technologique" = "TMD","Général & Technologique" = "total")

bac <- bac %>% 
  pivot_longer(cols = starts_with("bac"), names_to=c("diplome", "serie","sexe"), names_sep="_", values_to="nb_admis")
bac$filiere <- bac$serie %>%
  fct_recode("Général" = "C","Général" = "D","Général" = "E","Général" = "ES", "Général" = "GEN","Général" = "L","Professionnel" = "PRO",
    "Général" = "S","Technologique" = "ST2S", "Technologique" = "STAV","Technologique" = "STHR","Technologique" = "STI2D","Technologique" = "STL",
    "Technologique" = "STMD","Technologique" = "STMG","Technologique" = "TECH","Toutes" = "TOUS")
bac <- bac %>% filter(serie != "GEN" & serie != "PRO" & serie != "TECH")

#---Graphiques élèves GT-----------
ses %>% filter(secteur=="tous", niveau=="2GT", statut=="total", sexe=="HF") %>%
  ggplot() + aes(x=annee, y=nb_eleves) + geom_line()

ggplot() + geom_line(data=filter(series, statut=="public+privé", classe=="LyceeGT"), aes(x=annee, y=nb_eleves)) +
  geom_line(data=filter(ses, secteur=="tous", niveau=="2GT", statut=="total", sexe=="HF"), aes(x=annee, y=nb_eleves), color="red", alpha=0.6)

series %>% filter(statut=="public+privé", serie=="total", classe != "LyceeGT") %>% ggplot() + geom_bar(aes(x=annee, weight=nb_eleves, fill=classe))

eleves <- read_excel("C:/Users/Kevin/Cozy Drive/Recherche/Histoire education/kh-sl-evolution_nb_eleves.xlsx", 
                     sheet = "ens_1d_2d_sup")
eleves %>% ggplot() + geom_line(aes(x=annee, y=eleves_1d_total/eleves_1d_total[eleves$annee==1967]*100,linetype=champ_geo)) + geom_line(aes(x=annee, y=eleves_2d_1c/eleves_2d_1c[eleves$annee==1967]*100,linetype=champ_geo), color="red") + 
  geom_line(aes(x=annee, y=eleves_2d_2c_gt/eleves_2d_2c_gt[eleves$annee==1967]*100, linetype=champ_geo), color="blue") + 
  geom_line(aes(x=annee, y=eleves_2d_2c_pro/eleves_2d_2c_pro[eleves$annee==1967]*100, linetype=champ_geo), color="green") + 
  scale_x_continuous(limits=c(1967, 1980)) +
  theme_bw()

#----Graphiques enseignants----
#Graphique Enseignant selon le statut
ggplot() +  geom_bar(data=filter(ses, annee <= "2013", secteur=="public", niveau=="total", statut != "total", statut != "agregcert", statut != "apses"),aes(x=annee, weight=nb_ens, fill=statut_rec), position=position_stack()) +
  geom_line(data=filter(ses, secteur=="public", champ=="France métro", statut=="total", sexe=="HF"), aes(x=annee, y=nb_ens, color="#4A6973"), size=3) +
  geom_line(data=filter(ses, secteur=="public", champ != "France métro", statut=="total", sexe=="HF"), aes(x=annee, y=nb_ens, color="#4A6973"), size=3) +
  geom_line(data=filter(ses, annee >="2000", secteur=="public", niveau=="total", statut=="apses"), aes(x=annee, y=nb_ens, color="#233C4A"), size=3) +
  scale_fill_manual(values=met.brewer("Pillement", 6)) +
  scale_color_manual(values=c("#233C4A", "#4A6973"), label=c("Enseignant·e·s à l'APSES", "Total des enseignant·e·s")) +
  scale_x_continuous(limits=c(1970,2022), breaks= seq(1970,2022, by=2)) +
  scale_y_continuous(limits=c(0,4600), breaks= seq(0,4600, by=200)) +
  annotate("text", x = 2018.5, y = 2300, label = "Nombre d'adhérent·e·s à l'APSES", size=5, color="#233C4A", fontface=2) +
  annotate("text", x = 2018, y = 4600, label = "Nombre total d'enseignant·e·s", size=5, color="#4A6973", fontface=2) +
  theme_bw(base_size =18) + theme(legend.position="top", legend.title=element_blank()) +
  guides(colour="none") + 
  labs(title = "Evolution du nombre d'enseignant·e·s de SES en charge d'élèves à l'année selon le statut (1971-2021)",
         x="", y="Nombre d'enseignant·e·s",
         caption = "Graphique : K.Hédé (@knhede)")

ggsave("mon_graphique.png", width = 15, height = 10, dpi=300)

#Graphique proportion non titulaires
ses %>% filter(secteur=="public", niveau=="total", statut %in% c("agregcert", "nontit", "adjce")) %>% ggplot() +  geom_bar(aes(x=annee, weight=nb_ens, fill=statut_rec), position="fill")+
  scale_fill_manual(values=c("#a9845b", "#2b4655", "#738e8e")) +
  scale_x_continuous(limits=c(1970,2023), breaks= seq(1970,2023, by=2)) +
  scale_y_continuous("", breaks= seq(0,1, by=0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_bw(base_size =18) + theme(legend.position="top", legend.title=element_blank()) +
  labs(title = "Evolution de la proportion de titulaires et de non titulaires parmis les enseignant·e·s de SES en charge d'élèves à l'année (1971-2021)",
       x="", y="",
       caption = "Graphique : K.Hédé (@knhede)")

#Graphiques proportion femmes
ses %>% filter(secteur=="public", niveau=="total") %>% ggplot() +  geom_line(aes(x=annee, y=part_f), size=3, color="#2b4655") + 
  scale_x_continuous(limits=c(1970,2022), breaks= seq(1970,2022, by=4)) +
  scale_y_continuous("", limits=c(0.3,0.6), breaks= seq(0,1, by=0.02), labels = scales::percent_format(accuracy = 1)) +
  theme_bw(base_size =16)+
  labs(title = "Evolution de la part des femmes parmis les enseignant·e·s de SES en \ncharge d'élèves à l'année (1971-2021)",
       x="", y="",
       caption = "Graphique : K.Hédé (@knhede)")

#Graphiques proportion age
age$annee <- as.character(age$annee)
age$age <- factor(age$age)
iorder(age$age)
age$age <- age$age %>%
  fct_relevel("Moins de 30 ans", "Moins de 40 ans", "Entre 30 et 40 ans",
              "Entre 40 et 50 ans", "Entre 30 et 50 ans", "50 ans et plus")
iorder(age$age)
a1 <- age %>% filter(annee %in% c("1979", "1991", "1995", "1999", "2003", "2007")) %>% ggplot() + geom_bar(aes(annee, weight=part_age, fill=age), position = position_fill(reverse = TRUE)) +
  scale_fill_manual("", values=c("#a9845b", "#738e8e", "#44636f", "#2b4655", "#697852")) +
  scale_y_continuous("", limits=c(0,1), breaks= seq(0,1, by=0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_bw(base_size = 16) + theme(legend.position="top", plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(0,0,-10,0)) +
    labs(title = "Structure par âge des enseignant·e·s titulaires de SES (1979-2007)",
       x="", y="",
       caption = "Graphique : K.Hédé (@knhede)")

f1 + a1

#---Graphique Nombre d'élèves----------

ggplot() +  geom_col(data=filter(ses, secteur == "tous", niveau !="total" & niveau != "2GT", statut=="total", sexe=="HF"),aes(x=annee, y=nb_eleves, fill=niveau), position=position_stack()) + 
  scale_fill_manual(values=met.brewer("Pillement", 5), labels=c("Terminale (option hors B)", "Terminale","Première (option hors B)", "Première", "Seconde")) +
  scale_x_continuous(limits=c(1970,2023), breaks= seq(1970,2023, by=2)) +
  scale_y_continuous(limits=c(0,900000), breaks= seq(0,900000, by=50000)) +
  theme_bw(base_size = 20) + theme(legend.position="top", legend.title=element_blank()) +
  labs(title = "Evolution du nombre d'élèves suivant un enseignement de SES selon le niveau de classe (1971-2021)",
       x="", y="Nombre d'élèves",
       caption = "Graphique : K.Hédé (@knhede)")
ggsave("eleves_ses.png", width = 18, height = 12, dpi=400)


#----Graphique Nb élèves par enseignant------

ggplot() +  geom_col(data=filter(ses, secteur == "public", niveau =="total", statut=="total", sexe=="HF"),aes(x=annee, y=nb_eleves/3000, fill="Nombre d'élèves")) +
  geom_line(data=filter(ses, secteur=="public", statut=="total"), aes(x=annee, y=ratio_el_ens, color="Nombre d'élèves par enseignant·e"), size=3) +
  scale_fill_manual("", values=c("#6C8789")) +
  scale_color_manual("", values=c("#233C4A"))+
  scale_x_continuous(limits=c(1970,2022), breaks= seq(1970,2022, by=2)) +
  scale_y_continuous(limits=c(0,250), breaks= seq(0,250, by=10), sec.axis=sec_axis(~ . *3000, name="Nombre d'élèves", breaks=seq(0, 720000, by=60000), labels=scales::number_format())) +
  theme_bw(base_size =18) + theme(legend.position="top", legend.title=element_blank(), panel.grid.minor.y = element_blank(), axis.text.y.right=element_text(vjust=0.3)) +
  labs(title = "Evolution du nombre d'élèves moyen par enseignant·e de SES en charge d'élèves à l'année (1971-2021)",
       x="", y="Nombre d'élèves moyen par enseignant·e",
       caption = "Graphique : K.Hédé (@knhede)")

#-----Graphique Evolution nombre postes/présents/admis au concours-----
concours_ses$concours <- factor(concours_ses$concours)
concours_ses$concours <- concours_ses$concours %>% fct_relevel("CAPES", "Agrégation", "Tous")
concours_ses$type_concours <- factor(concours_ses$type_concours)


concours_ses %>% filter (concours != "Tous" & type_concours != "tous" & type_concours != "Reservé") %>% 
  ggplot() + geom_bar(aes(x=annee, weight=postes_concours,fill="Nombre de postes")) +
  geom_bar(aes(x=annee, weight=admis_concours, fill="Nombre d'admis")) +
  scale_fill_manual("", values=c("#233C4A","#6C8789")) +
  scale_y_continuous("", breaks = seq(0,280, by=20)) +
  scale_x_continuous("",limits=c(1971,2022), breaks= seq(1970,2021, by=2)) +
  facet_grid(concours~type_concours, scales = "free_y") +
  theme_bw(base_size=20) + theme(legend.position="bottom", axis.text.x=element_text(angle=90, size=18, vjust=0.5), strip.text.x=element_text(size=18),
                                   plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-25,0,0,0))

capes_labs <- as_labeller(c(`Externe` = "CAPES Externe", `Interne` = "CAPES Interne"))
agreg_labs <- as_labeller(c(`Externe` = "Agrégation Externe", `Interne` = "Agrégation Interne"))


concours_ses %>% filter (concours != "tous" & type_concours != "tous" & type_concours != "reserve") %>% 
  ggplot() + geom_line(aes(x=annee, y=presents_concours, color="Nombre de présents au concours"), size=2) +
  geom_bar(aes(x=annee, weight=postes_concours*5, fill="Nombre de postes au concours ")) +
  scale_fill_manual("", values=c("#233C4A"))+
  scale_color_manual("", values=c("#6C8789")) +
  scale_y_continuous("", breaks=scales::pretty_breaks(6)) +
  scale_x_continuous("",limits=c(1971,2022), breaks= seq(1970,2021, by=2)) +
  facet_wrap(concours~type_concours, scales = "free_y") +
  theme_bw(base_size=20) + theme(legend.position="bottom", axis.text.x=element_text(angle=90, size=18, vjust=0.5), strip.text.x=element_text(size=18),
                                 plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-25,0,0,0))

my_breaks <- function(x) { if (max(x) > 1000) seq(0, 3000, 200) else (if (max(x) > 400) seq(0, 900, 100) else (if (max(x) >150) seq(0, 400, 50) else seq(0,50,10)))}

cap_c
cap_c <- concours_ses %>% filter (concours == "CAPES" & type_concours != "tous" & type_concours != "Reservé") %>% 
  ggplot() + geom_bar(aes(x=annee, weight=postes_concours*5, fill="Nombre de postes au concours ")) +
    geom_bar(aes(x=annee, weight=admis_concours*5, fill="Nombre d'admis")) +
  geom_line(aes(x=annee, y=presents_concours, color="Nombre de présents au concours"), size=2) +
  scale_fill_manual("", values=c("#233C4A","#6C8789"))+
  scale_color_manual("", values=c("#4A6973")) +
  scale_x_continuous("",limits=c(1971,2022), breaks= seq(1970,2021, by=2)) +
  facet_wrap(~type_concours, scales = "free_y", labeller = capes_labs) +
  scale_y_continuous("Nombre de présents", breaks=my_breaks, sec.axis = sec_axis(~ . /5, name="Nombre de postes et d'admis·e·s")) +
  theme_bw(base_size=18) + theme(legend.position="none", axis.text.x=element_text(angle=90, size=18, vjust=0.5), strip.text.x=element_text(size=18),
                                 plot.margin = margin(0, 0, 0, 0, "cm"), legend.margin=margin(-25,0,0,0))

agr_c <- concours_ses %>% filter (concours == "Agrégation" & type_concours != "tous" & type_concours != "Reservé") %>% 
  ggplot() + geom_bar(aes(x=annee, weight=postes_concours*5, fill="Nombre de postes au concours (échelle de droite)")) +
  geom_bar(aes(x=annee, weight=admis_concours*5, fill="Nombre d'admis au concours (échelle de droite")) +
  geom_line(aes(x=annee, y=presents_concours, color="Nombre de présents au concours (échelle de gauche)"), size=2) +
  scale_fill_manual("", values=c("#233C4A", "#6C8789"))+
  scale_color_manual("", values=c("#4A6973")) +
  scale_x_continuous("",limits=c(1971,2022), breaks= seq(1970,2021, by=2)) +
  facet_wrap(~type_concours, scales = "free_y", labeller = agreg_labs) +
  scale_y_continuous("Nombre de présents", breaks=my_breaks, sec.axis = sec_axis(~ . /5, name="Nombre de postes et d'admis·e·s")) +
  theme_bw(base_size=18) + theme(legend.position="bottom", axis.text.x=element_text(angle=90, size=18, vjust=0.5), strip.text.x=element_text(size=18),
                                 plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-25,0,0,0))  

  
cap_c/agr_c + plot_layout(heights=c(2,1)) + 
  plot_annotation(title = "Evolution du nombre de candidat·e·s, de postes et d'admis·e·s aux concours de recrutement des enseignant·e·s de SES (1971-2021)", 
                  theme = theme(plot.title = element_text(size = 18)))
  
#---Graphique Nombre bacheliers----------

bac$serie <- factor(bac$serie)
iorder(bac$serie)  
bac$serie <- bac$serie %>%  fct_relevel("TOUS", "STMD", "STHR", "STAV", "STL", "ST2S", "STI2D", "STMG","E", "D", "C", "S", "L", "ES")
bac <- bac %>% group_by(annee, serie) %>% mutate(pct_sexe = nb_admis/nb_admis[sexe=="total"])

bac %>% filter(sexe != "total" & serie=="ES") %>% ggplot() + aes(x=annee, y=nb_admis, fill=sexe) + geom_col(position="dodge")+
    geom_line(aes(x=annee, y=pct_sexe*100000, color=sexe), size=2) + 
    scale_color_manual(values=c("#697852", "NA"), labels=c("Pourcentage de filles (échelle de droite)", "")) +
    scale_fill_manual(values=met.brewer("Pillement", 2), labels=c("Filles","Garçons")) +
    scale_x_continuous(limits=c(1969.5,2020.5), breaks= seq(1970,2020, by=2)) +
    scale_y_continuous(limits=c(0,90000), breaks= seq(0,90000, by=5000), sec.axis = sec_axis(~ . /100000, name="", breaks=seq(0, 1, by=0.05), labels=scales::percent_format(accuracy = 1))) +
    theme_bw(base_size = 18) + theme(legend.position="top", plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), 
                                     legend.margin=margin(0,0,0,0), legend.title=element_blank()) +
    labs(title = "Evolution du nombre de bacheliers et de bachelières titulaires d'un bac B/ES (1970-2020)",
         x="", y="Nombre d'élèves",
         caption = "Graphique : K.Hédé (@knhede)")

bac2 %>% filter(sexe=="F") %>% ggplot() + aes(x=annee, y=pct_sexe, color=serie) + geom_line() +
  scale_x_continuous(limits=c(1969.5,2020.5), breaks= seq(1970,2020, by=2)) + facet_wrap(~filiere)



bac %>% filter(sexe == "total" & filiere=="Général") %>% ggplot() + aes(x=annee, y=nb_admis, fill=serie) + geom_col(position="fill") +
  scale_fill_manual("Série", values=c("#738e8e","#44636f","#2b4655","#0f252f", "#697852","#a9845b"), guide = guide_legend(reverse=TRUE, nrow=1)) +
  scale_x_continuous(limits=c(1969,2021), breaks= seq(1970,2021, by=2)) +
  scale_y_continuous("", breaks= seq(0,1, by=0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_bw(base_size = 18) + theme(legend.position="bottom",  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-30,0,0,0)) +
  labs(title = "Evolution de la part des bacheliers généraux selon la série (1970-2020)",
       x="", y="",
       caption = "Graphique : K.Hédé (@knhede)")

bac %>% filter(sexe == "total" & filiere!="Toutes") %>% ggplot() + aes(x=annee, y=nb_admis, color=serie, linetype=champ) + geom_line(size=2) +
  scale_color_manual("Série", values=met.brewer("OKeeffe1", 16), guide = guide_legend(reverse=TRUE, nrow=1)) +
  scale_x_continuous("",limits=c(1969,2021), breaks= seq(1970,2021, by=5)) +
  scale_linetype_discrete("", labels=NULL)+
  scale_y_continuous("",limits=c(0,200000), breaks= seq(0,200000, by=20000)) +
  theme_bw(base_size = 18) + theme(legend.position="bottom",  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-30,0,0,0)) +
  facet_wrap(~filiere)
  labs(title = "Evolution de la part des bacheliers généraux selon la série (1970-2020)",
       x="", y="",
       caption = "Graphique : K.Hédé (@knhede)")

#----------------------Graphique séries-----------------------------
seriesgraph <- c("A/L", "B/ES", "C", "D", "S-SVT", "E/S-SI", "F", "G-H/STMG")
levels(series$serie)
series$champ_geo2 <- series$champ_geo %>% fct_recode("France métropolitaine + DOM" = "France métropolitaine + DOM (y compris EREA)", "France métropolitaine + DOM" = "France métropolitaine + DOM (hors EREA)")

series %>% filter(classe == "Terminale", statut=="public+privé", serie != "total" & serie != "general" & serie != "techno" & serie != "autres") %>% ggplot() + aes(annee, nb_eleves, color=serie) + geom_line(size=2) +
  scale_x_continuous("",limits=c(1958,2024), breaks= seq(1958,2022, by=4)) +
  scale_color_manual(values=met.brewer("Pillement", 2)) +
  gghighlight(serie == "B/ES") +
theme_bw(base_size = 18) + theme(legend.position="bottom",  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-30,0,0,0))

series %>% filter(classe == "Terminale", statut=="public+privé", serie %in% seriesgraph) %>%
  ggplot() + aes(x=annee, y=nb_eleves, color=serie) + geom_line(size=2) + geom_label_repel(aes(label=serie)) +
  scale_x_continuous("",limits=c(1958,2020), breaks= seq(1958,2020, by=4)) +
  scale_color_manual(values=c("#697955","#6E898A","#395664","#0F252F","#405E6A","#1D3643","#877D56","#A9845B"))  +  
  theme_bw(base_size = 18) + theme(legend.position="bottom",  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-30,0,0,0))

ggplot() + 
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "A/L"), aes(x=annee, y=nb_eleves, linetype=champ_geo2),color = "#6D816B", size=2.5, alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "C"), aes(x=annee, y=nb_eleves), color ="#4A6973", size=2.5, alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "D"), aes(x=annee, y=nb_eleves), color="#6C8789",size=2.5, alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "F"), aes(x=annee, y=nb_eleves, linetype=champ_geo2), color="#7B7B54",size=2.5, alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "G-H/STMG"), aes(x=annee, y=nb_eleves, linetype=champ_geo2), color="#A9845B", size=2.5,alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "S-SVT"), aes(x=annee, y=nb_eleves), color="#0F252F", size=2.5,alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "E/S-SI"), aes(x=annee, y=nb_eleves,linetype=champ_geo2), color="#355260", size=2.5, alpha=0.6) +
  geom_line(data=filter(series, classe == "Terminale", statut=="public+privé", serie == "B/ES"), aes(x=annee, y=nb_eleves, linetype=champ_geo2), color= "#233C4A",size=3.5, alpha=2) +
  scale_x_continuous("",limits=c(1960,2021), breaks= seq(1960,2020, by=2)) +
  scale_linetype_manual(values=c(1,1,1)) +
  scale_y_continuous("",limits=c(0,180000), breaks= seq(0,180000, by=10000)) +
  annotate("label", x = 1969.35, y = 55500, label = "D", size=5, color="#6C8789", fontface=2) +
  annotate("label", x = 1969.35, y = 34000, label = "C", size=5, color="#4A6973", fontface=2) +
  annotate("label", x = 1969.35, y = 9000, label = "E", size=5, color="#355260", fontface=2) +
  annotate("label", x = 2019.8, y = 22530, label = "S-SI", size=5, color="#355260", fontface=2) +
  annotate("label", x = 2020, y = 176452, label = "S-SVT", size=5, color="#0F252F", fontface=2) +
  annotate("label", x = 1969.35, y = 28000, label = "G-H", size=5, color="#A9845B", fontface=2) +
  annotate("label", x = 2021, y = 75000, label = "STMG", size=5, color="#A9845B", fontface=2) +
  annotate("label", x = 1969.35, y = 15500, label = "B", size=5, color="#233C4A", fontface=2) +
  annotate("label", x = 2019.8, y = 132034, label = "ES", size=5, color="#233C4A", fontface=2) +
  annotate("label", x = 1969.35, y = 80000, label = "A", size=5, color="#6D816B", fontface=2) +
  annotate("label", x = 2019.35, y = 55646, label = "L", size=5, color="#6D816B" , fontface=2) +
  annotate("label", x = 2021, y = 63000, label = "STI/STL\n/ST2S", size=4, color="#6D816B" , fontface=2) +
  annotate("label", x = 1969.35, y = 22000, label = "F", size=5, color="#6D816B" , fontface=2) +
  theme_bw(base_size = 18) + theme(legend.position="none",  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-30,0,0,0)) +
  labs(title = "Evolution de l'effectif des classes de terminale selon la série (1970-2020)",
       x="", y="Nombre d'élèves",
       caption = "Graphique : K.Hédé (@knhede)")

#---------------Graphique PCS élèves----------
pcs$pcs_reg <- factor(pcs$pcs_reg)
pcs$serie <- factor(pcs$serie)
pcs$annee <- as.character(pcs$annee)
pcs$pcs_reg <- pcs$pcs_reg %>%  fct_relevel("favorisées", "moyennes", "défavorisés")
pcs$serie<- pcs$serie %>%  fct_relevel("A", "B", "C", "D", "E", "L", "ES", "S", "GT")
pcs_detail$pcs <- factor(pcs_detail$pcs)
iorder(pcs_detail)
pcs_detail$pcs <- pcs_detail$pcs %>%
  fct_relevel(
    "agriculteurs exploitants", "Agriculteurs exploitants", "Artisan, commerçant",
    "Patrons de l'industrie et du commerce", "Professions libérales et cadres supérieurs",
    "Cadre et profession intellectuelle supérieure", "Enseignant",
    "Cadres moyens", "Profession intermédiaire", "Employé", "Employés",
    "Personnel de service", "Autres CSP", "Ouvrier", "Ouvriers",
    "ouvriers agricoles", "Inactif", "Sans profession", "Retraité"
  )

annee_labs <- as_labeller(c(`1976` = "Bacheliers 1976", `1994` = "Terminales 1994", `2001` = "Terminales 2001", `2018` = "Premières et Terminales 2018"))

pcs %>% filter(eleves !="seconde", serie !="Techno", annee!="1995") %>% ggplot() + geom_bar(aes(serie, weight=part, fill=pcs_reg), position="fill") + 
  scale_fill_manual("Origine sociale", values=met.brewer("Pillement", 3)) +
  scale_x_discrete("") +
  scale_y_continuous("", breaks= seq(0,1, by=0.1), labels = scales::percent_format(accuracy = 1))+ facet_grid(~annee, labeller = annee_labs) + theme_bw(base_size = 18) +
  theme(legend.position="bottom") +
  labs(title = "Origine sociale des élèves inscrits dans les différentes séries (1976-2018)")


pcs %>% filter(eleves !="seconde", serie !="Techno") %>% ggplot() + geom_bar(aes(annee, weight=part, fill=pcs_reg)) + facet_grid(~serie)

pcs$title76 <- "Bacheliers 1976"
pcs$title94 <- "Terminales 1994"
pcs$title01 <- "Terminales 2001"
pcs$title18 <- "Premières et Terminales 2018"

g76 <- pcs %>% filter(eleves !="seconde", serie !="Techno", annee=="1976") %>% ggplot() + geom_bar(aes(serie, weight=part, fill=pcs_reg), position="fill") +
  scale_fill_manual("Origine sociale", labels=c("Favorisée (CPIS & PI)", "Moyenne (Agriculteurs, ACCE & Employés)", "Défavorisée (Ouvriers & inactifs)"), values=met.brewer("Pillement", 3)) +  scale_x_discrete("") + scale_y_continuous("", breaks= seq(0,1, by=0.1), labels = scales::percent_format(accuracy = 1)) + theme_bw(base_size = 20) +
  facet_grid(~title76)
g94 <- pcs %>% filter(eleves !="seconde", serie !="Techno", annee=="1994") %>% ggplot() + geom_bar(aes(serie, weight=part, fill=pcs_reg), position="fill") +
  scale_fill_manual("Origine sociale", labels=c("Favorisée (CPIS & PI)", "Moyenne (Agriculteurs, ACCE & Employés)", "Défavorisée (Ouvriers & inactifs)"), values=met.brewer("Pillement", 3)) + scale_x_discrete("") + theme_bw(base_size = 20) +  facet_grid(~title94) + easy_remove_y_axis()
g01 <- pcs %>% filter(eleves !="seconde", serie !="Techno", annee=="2001") %>% ggplot() + geom_bar(aes(serie, weight=part, fill=pcs_reg), position="fill") +
  scale_fill_manual("Origine sociale", labels=c("Favorisée (CPIS & PI)", "Moyenne (Agriculteurs, ACCE & Employés)", "Défavorisée (Ouvriers & inactifs)"), values=met.brewer("Pillement", 3)) +  scale_x_discrete("") + theme_bw(base_size = 20) +
  facet_grid(~title01) + easy_remove_y_axis()
g18 <- pcs %>% filter(eleves !="seconde", serie !="Techno", annee=="2018") %>% ggplot() + geom_bar(aes(serie, weight=part, fill=pcs_reg), position="fill") +
  scale_fill_manual("Origine sociale", labels=c("Favorisée (CPIS & PI)", "Moyenne (Agriculteurs, ACCE & Employés)", "Défavorisée (Ouvriers & inactifs)"), values=met.brewer("Pillement", 3)) +  scale_x_discrete("") + scale_y_continuous("", breaks= seq(0,1, by=0.1), labels = scales::percent) + theme_bw(base_size = 20) +
  facet_grid(~title18) + easy_remove_y_axis()

(g76|g94|g01|g18) + plot_annotation("Origine sociale des élèves selon la série") + plot_layout(guides="collect") & theme(legend.position="bottom", plot.title = element_text(size = 24))

pcs_detail %>% filter(eleves !="seconde", serie !="Techno", annee=="1976") %>% ggplot() + geom_bar(aes(serie, weight=part, fill=pcs), position="fill") +
  scale_fill_manual("Origine sociale", values=met.brewer("Pillement", 10)) +  scale_x_discrete("") + scale_y_continuous("", breaks= seq(0,1, by=0.05), labels = scales::percent) + theme_bw(base_size = 16) +
  ggtitle("Bacheliers 1976")

#--------------Graphique massification----------------

eleves %>% filter(secteur=="public+privé") %>% ggplot() + geom_line(aes(annee, `2d_c1_total`, linetype=champ_geo), size=2, color="red") +
  geom_line(aes(annee, `2d_c2_gt`, linetype=champ_geo), size=2, color="blue") +
  geom_line(aes(annee, `2d_c2_pro`, linetype=champ_geo), size=2, color="green") +
  scale_x_continuous("",limits=c(1965,1981), breaks= seq(1965,1981, by=2)) +
  scale_y_continuous(limits=c(0,4000000), breaks= seq(0,4000000, by=500000))
  
 

eleves$`2d_c1_total`
  scale_x_continuous("",limits=c(1958,2024), breaks= seq(1958,2022, by=4))

  scale_color_manual(values=met.brewer("Pillement", 2)) +
  gghighlight(serie == "B/ES") +
  theme_bw(base_size = 18) + theme(legend.position="bottom",  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), legend.margin=margin(-30,0,0,0))
