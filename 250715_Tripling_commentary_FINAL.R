library(tidyverse)
library(zoo)
library(patchwork)
library(readxl)
library(janitor)
library(scales)
options(scipen =999)
library(showtext)
library(extrafont)
library(ggtext)
library(ggalt)
library(here)
library(cowplot)
library(ggsci)
library(ggtern)
library(ggrepel)
library(ggalt)
library(ggh4x)
library(ggridges)
library(PupillometryR)


font_add_google('Nanum Myeongjo', 'Nanum Myeongjo')
font_add_google("Nanum Gothic", "nanumgothic")
font_add_google("Poor Story", "poorstory")
showtext_auto()





file_path <- rstudioapi::getActiveDocumentContext()$path
dir_path <- dirname(file_path)


setwd(dir = dir_path)


getwd()


font.size <- 14

theme_set(theme_cowplot(font_size = font.size))
theme_update(
  axis.line = element_line(linewidth = 0.25),
  axis.ticks = element_line(linewidth = 0.25),
  plot.title.position="plot"
)






####Figure S5.

### Korea's net emission (Source :GIR statistics, IPCC0 6 guideline)


KOR_GHG<- data.frame(
    year = c(1990L,1991L,1992L,1993L,1994L,1995L,
             1996L,1997L,1998L,1999L,2000L,2001L,2002L,2003L,2004L,
             2005L,2006L,2007L,2008L,2009L,2010L,2011L,2012L,2013L,
             2014L,2015L,2016L,2017L,2018L,2019L,2020L,2021L,2022L),
    value = c(271615.09,306225.6,334622.72,374169.23,
              397753.22,431236.45,464394.65,484380.56,410168.46,
              442315.49,473061.31,490089.66,512616.42,526402.43,536330.06,
              536941.52,548343.58,554265.5,568477.01,572586.64,632427.78,
              665101.19,669678.63,682146.3,677165.03,678331.9,689192.24,
              716258.96,742309.94,720742.44,674120.27,701974.88,686461.91)
) %>% 
  mutate(value = round(value/1000, 1))


KOR_GHG_SECTOR <- data.frame(
                         sector = c(1990L,
                                    1991L,1992L,1993L,1994L,1995L,1996L,
                                    1997L,1998L,1999L,2000L,2001L,2002L,
                                    2003L,2004L,2005L,2006L,2007L,2008L,
                                    2009L,2010L,2011L,2012L,2013L,2014L,2015L,
                                    2016L,2017L,2018L,2019L,2020L,2021L,
                                    2022L),
                            에너지 = c(234475.75,257328.69,273752.07,303458.93,
                                    323158.1,347962.44,381519.41,398735.84,
                                    340200.65,372901.35,402242.55,415071.6,
                                    432455.54,442261.22,451612.65,448168.88,
                                    458739.46,477326.58,490655.07,499155.99,
                                    550333.53,580221.09,575024.48,569701.72,
                                    560877.46,562925.48,575019.94,587475.36,
                                    612119.82,586435.2,544600.21,566782.43,
                                    551889.33),
                    산업공정.및.제품사용 = c(37797.76,44174.92,53463.44,60361.66,64280.22,
                                    69703.89,72555.73,79615.63,73189.31,
                                    80348.79,83108.75,86822.58,90233.61,
                                    94211.61,97817.79,99926.29,101972.1,91393.83,
                                    93917.03,88946.77,95049.33,97782.36,
                                    101298.97,115019.04,120162.46,119854.48,
                                    119254.25,127861.3,128910.62,130928.88,
                                    126784.1,132777.64,131262.16),
                             농업 = c(24865.47,24915.57,25214.8,25547.29,25767.65,
                                    25875.04,26228.8,26183.83,26132.97,
                                    25140.98,24712.12,24305.69,24157.95,23899.68,
                                    23826.85,23677.75,23682.51,23928.17,
                                    23898.73,24310.89,24667.69,23448.76,
                                    24053.65,23898.59,24031.2,23633.53,23539.25,
                                    23658.68,23489.03,23263.86,23091.12,23077,
                                    22953.41),
                         LULUCF = c(-38963.25,-35015.67,-34186.85,-32632.92,
                                    -35016.07,-33261.19,-36684.49,-41683.37,
                                    -50051.23,-58254.13,-60392.16,-60722.81,
                                    -58361.35,-58073.58,-59434.53,-57480.58,
                                    -58664.62,-59363.35,-60288.95,-60068.69,
                                    -57361.37,-56537.01,-50562.52,-46262.82,
                                    -47142.61,-47772.9,-48215.72,-42848.81,
                                    -41563.63,-38654.32,-38838.5,-39001.22,
                                    -37832.15),
                            폐기물 = c(13439.35,14822.09,16379.27,17434.27,19563.31,
                                    20956.26,20775.2,21528.63,20696.75,
                                    22178.5,23390.05,24612.61,24130.67,24103.49,
                                    22507.3,22649.19,22614.12,20980.27,
                                    20295.13,20241.68,19738.61,20186,19864.05,
                                    19789.77,19236.51,19691.31,19594.52,
                                    20112.44,19354.1,18768.82,18483.35,18339.03,
                                    18189.17)
                  ) %>% 
  pivot_longer(-sector) %>% 
  mutate(value = value/1000) %>% 
  rename(year= sector, sector = name)

 
NDC_2018 <- 686.3 ## 2018 net emission (total in 20218 is 727.6)
NDC_2030 <- round(NDC_2018*(1-0.4), 1)  ## 2030 is net-emission

KOR_NDC<- data.frame(
  year = c(2022, 2030, 2050),
  value = c(NDC_2018, NDC_2030, 0)
)


intro_A<-KOR_GHG %>% 
  ggplot()+
  geom_line(aes(x = year, y= value), linewidth = 2, color ='#0b272f')+
  geom_line(data = KOR_NDC, aes( x = year, y = value), linewidth =2, linetype = "dashed" ,alpha = .9, color ='#0b272f')+
geom_point(data = KOR_NDC %>% filter(year !=2022), aes( x = year, y = value), size =8 ,alpha = .7, color ='#f4481a')+
  scale_x_continuous(limits = c(1990, 2050), breaks = seq(1990, 2050, 10))+
  scale_y_continuous(limits = c(0, 800))+
  geom_segment(
    aes(x = 2030,
        xend = 2030,
        yend = KOR_NDC %>% filter(year== 2030) %>% pull(value),
        y = KOR_GHG %>% filter(year== 2018) %>% pull(value)),
    arrow = arrow(length = unit(0.6, "cm")),
    color = '#303d9b',
    linewidth = 1)+
  geom_segment(
    aes(x = 2018,
        xend = 2031,
        y = KOR_GHG %>% filter(year== 2018) %>% pull(value),
        yend =  KOR_GHG %>% filter(year== 2018) %>% pull(value)),
    color = '#303d9b',
    linewidth = 0.5,
    linetype ='dashed')+
  geom_segment(
    aes(x = 2018,
        xend = 2031,
        y = 436.6,
        yend =  436.6),
    color = '#303d9b',
    linewidth = 0.5,
    linetype ='dashed')+
  theme_bw()+
  theme_minimal()+
  geom_text(
    aes(x = 2032,
        y = KOR_NDC %>% filter(year== 2030) %>% pull(value)*1.5,
        label = "2030 NDC\n(40% below compared to 2018)"),
    size = 5,
    hjust = 0,
   # vjust = 0.5,
   # nudge_x = 0.08,
   # lineheight = 2,
    #fontface = "bold"
  ) +
  labs(title ="A",
       x= "",
       y = "MTCO2eq")+
  theme(   plot.title = element_text(face="bold"),
              plot.title.position = "plot")

 


NDC_2018_2030<-
  data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    sector = c("Energy Transformation",
             "Industries","Buildings","Transport",
             "Others","Others","Others",
             "Others","Absorption and Removal",
             "Absorption and Removal","Absorption and Removal"),
    `2018` = c(269.6,260.5,52.1,98.1,24.7,
               17.1,NA,5.6,-41.3,NA,NA),
    `2030` = c(145.9,230.7,35,61,18,9.1,
               8.4,3.9,-26.7,-11.2,-37.5)
  ) %>% pivot_longer(-sector, names_to='year', values_to = 'value') %>% 
  group_by(sector,year) %>% 
  summarise(value = sum(value,na.rm=T)) 


intro_B<-NDC_2018_2030 %>% 
  filter(year%in% c(2018, 2030)) %>% 
  ggplot(aes(x = year, y = value, fill = reorder(sector, value)))+
  geom_col()+
    theme_bw()+
  theme_minimal()+

  stat_summary(size =6,
    fun = sum,
    aes(label = ..y.., group = year),
    geom = "text",
    vjust = -1
  )+
 geom_point(x = '2018', y = 686.4,shape = 23, size =3, show.legend = FALSE, fill ='#303d9b')+
 geom_point(x = '2030', y = 436.6, shape = 23, size = 3, show.legend = FALSE, fill ='#303d9b')+
  geom_segment(aes(x = '2018', y = 686.4, xend = '2030', yend = 436.6),
               arrow = arrow(length = unit(0.3, "cm")), color = '#303d9b')+
  scale_y_continuous(limits = c(-100,900), breaks =c(-100,0,200,400,600,800))+
  
  scale_fill_brewer(palette="Set2")+
  #theme(legend.position ="none")+
  labs(title ='B',
       x="",
       y="MTCO2eq")+
  guides(fill=guide_legend(title='Sector'))+
  theme(   plot.title = element_text(face="bold"),
           plot.title.position = "plot",
           panel.grid.minor.y = element_blank())



######  11th Elec basic plan for intro
elec_11th_plan<-data.frame(
  stringsAsFactors = FALSE,
              year = c(2023L, 2023L, 2030L, 2030L, 2038L, 2038L),
                type = c("발전량", "비중", "발전량", "비중", "발전량", "비중"),
                원전 = c(180.5, 30.7, 204.2, 31.8, 248.3, 35.2),
                석탄 = c(184.9, 31.4, 110.5, 17.2, 70.9, 10.1),
               LNG = c(157.7, 26.8, 161, 25.1, 74.3, 10.6),
               재생e = c(49.4, 8.4, 120.9, 18.8, 205.7, 29.2),
                신e = c(7.2, 1.2, 18.7, 2.9, 26.4, 3.8),
          청정수소암모니아 = c(0, 0, 15.5, 2.4, 43.9, 6.2),
                기타 = c(8.3, 1.4, 11.8, 1.8, 34.9, 5)
) %>% 
  pivot_longer(-c(year, type), names_to="source", values_to = 'value')



### 11th Plan - Renewable capacity plan

elec_11th_renewable<-data.frame(
        year = c(2025L, 2030L, 2036L, 2038L),
         태양광 = c(32, 55.7, 72.9, 77.2),
          풍력 = c(3, 18.3, 35.5, 40.7)
) %>% 
  pivot_longer(-year, names_to = 'type', values_to = 'GW')



#### Korea's historical capacity from EPSIS
Capa_2012_2024<- read_excel('EPSIS_capacity_by_source.xlsx') %>%
  mutate(석탄 = 유연탄+무연탄) %>% 
  pivot_longer(-year, names_to = 'type', values_to = 'MW') %>% 
  mutate(GW= MW/1000,
         year = as.numeric(year),
         type = gsub('태양', '태양광', type)) %>% 
  filter(type %in% c('원자력', '석탄', '태양광', '풍력', 'LNG')) %>% 
  select(-MW)


## Only Coal and Lng from EPSIS

elec_11th_nuc_coal_lng <-data.frame(
  year = c(2023L, 2030L, 2038L),
  원자력 = c(24.7, 28.9, 35.2),
  석탄 = c(39.2, 31.7, 22.2),
  LNG = c(43.2,58.8, 69.2)) %>% 
      pivot_longer(-year, names_to = 'type', values_to = 'GW')


intro_C<-rbind(Capa_2012_2024,elec_11th_nuc_coal_lng, elec_11th_renewable) %>% 
  mutate(type = gsub('태양광','Solar', type),
         type = gsub('풍력', 'Wind', type),
         type =gsub('원자력', 'Nuclear', type),
         type =gsub('석탄', 'Coal', type)) %>% 
  ggplot(aes(x = year, y = GW, color = type))+
  geom_line(data =. %>% filter(year<=2024), linewidth = 1.7, alpha = .9)+
  geom_line(data =. %>% filter(year>=2023), linewidth = 1.3,alpha = .7, linetype='dashed')+
  scale_color_manual(values = c('black','brown','orange','#2d9301','#1f5c99'))+
  geom_vline(xintercept = 2024, alpha =.8, color=  'gray')+
  scale_x_continuous(limits = c(2012, 2051), breaks =c(2012,2016,2020,2024,2028,2032,2038))+
  theme_bw()+
  theme_minimal()+
  geom_text(aes(label = type), data =. %>% filter(year==2038), hjust = -.1)+
  theme(text = element_text(size = 14),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        panel.grid.minor.x = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1),
        legend.position = "none"
        )+
  labs(title = "C",caption = "Coal includes ammonia co-firing, LNG includes hydrogen blending/co-firing",
       x="")


layout <- "
AAAAAAAAA
AAAAAAAAA
BBCCCCCCC
"

intro_A/intro_B+intro_C+plot_layout(design=layout)

ggsave("Figure_S5.svg",  width = 30, height = 20, units = "cm", dpi = 200)








#########################################  Figure 1

KOR_1961_2023_GEN<- read_excel('./250413_EPSIS_electricity_generation_by_source.xlsx') %>% 
  select(-총계) %>%
  transmute(year = year,
            Coal = 무연탄+유연탄,
            Hydro = 수력,
            Oil = 중유,
            Gas = 가스 + 복합화력,
            Nuclear = 원자력,
            Renewable = 신재생) %>% 
  pivot_longer(-year, names_to = 'type', values_to="MWh") %>% 
  mutate(year = as.numeric(year),
         TWh = MWh/1000000)


KOR_1961_2023_GEN %>% 
  filter(year ==2022) %>% 
  arrange(desc(TWh)) %>% pull(type) ->gen_order


KOR_1961_2023_GEN<- KOR_1961_2023_GEN %>% 
  mutate(type = factor(type, gen_order))

gen1<-KOR_1961_2023_GEN %>% 
  ggplot(aes(x = year, y = TWh, fill= type))+
  geom_area()+
  scale_x_continuous(limits = c(1960, 2035), 
                     breaks = c(seq(1960, 2010, 10), 2023))+
  theme_bw()+
  scale_y_continuous(limits = c(0,800), breaks = seq(0,800,200))+
  theme(text = element_text(size = 16, family = 'nanumgothic'),
    #legend.position = c(0.1, 0.5),
    #legend.background = element_rect(fill=alpha(0.4)),
    legend.position ="none",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face="bold"))+
  geom_text(data =. %>% filter (year == 2023 & type %in% c('Coal','Nuclear','Gas','Renewable')), 
            aes(label =type), position =position_stack(vjust = 0.5), hjust = -.1, size =5,
            family = 'nanumgothic')+
  scale_fill_manual(values = c('#533600',
                               '#e88431',
                               '#575e63',
                               '#24bf0c',
                               '#005d9a',
                               '#282422'))+
  labs(x='',
       y = 'Generation (TWh)',
       title ='A')




gen_2023_label<-KOR_1961_2023_GEN %>% 
  filter(year ==2023) %>% 
  mutate(total = sum(TWh),
         label = paste0(round(TWh/total*100,1), "%")) 



elec_11th_plan_eng<-elec_11th_plan %>% 
  mutate(source= gsub('원전','Nuclear', source),
         source= gsub('석탄','Coal', source),
         source= gsub('LNG','Gas', source),
         source= gsub('재생e','Renewable', source),
         source= gsub('신e','Renewable', source),
         source= gsub('청정수소암모니아','Hydrogen-ammonia co-firing', source),
         source= gsub('기타','Others', source),
         source = factor(source, levels=c('Coal', 'Nuclear',
                                          'Gas','Renewable',
                                          'Hydrogen-ammonia co-firing',
                                          'Others')))

gen4<-elec_11th_plan_eng %>% 
  mutate(year = factor(year)) %>%
  filter(type =='발전량' & year %in% c(2030, 2038)) %>% 
  group_by(year, source) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(x = year, y = value, fill = source))+
  geom_col(width = .7)+
  theme_bw()+
  scale_fill_manual(values = c('#533600',
                               '#e88431',
                               '#575e63',
                               '#24bf0c',
                               '#b7a001',
                               'gray80'))+
  scale_y_continuous(limits = c(0,800), breaks = seq(0,800,200))+
  theme(text = element_text(size = 16, family = 'nanumgothic'),
        #legend.position = c(0.1, 0.5),
        #legend.background = element_rect(fill=alpha(0.4)),
        #  legend.position ="none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())+
  labs(y = '',
       x = '',
       fill = "Source",
       title = 'B')+
  stat_summary(fun.y =sum, aes(label=round(..y..,0), group = year),  geom="text", size=6, 
               vjust = -0.5)



KOR_1961_2023_GEN_5year<-KOR_1961_2023_GEN %>% 
  mutate(year = case_when(year<=1965 ~ '61-65',
                          year<=1970 ~ '66-70',
                          year<=1975 ~ '71-75',
                          year<=1980 ~ '76-80',
                          year<=1985 ~ '81-85',
                          year<=1990 ~ '86-90',
                          year<=1995 ~ '91-95',
                          year<=2000 ~ '96-00',
                          year<=2005 ~ '01-05',
                          year<=2010 ~ '06-10',
                          year<=2015 ~ '11-15',
                          year<=2020 ~ '16-20',
                          TRUE ~ '21-23'
                          )) %>% 
  group_by(year, type) %>% 
  summarise(TWh = sum(TWh)) %>%
  group_by(year) %>% 
  mutate(total = sum(TWh),
         share = TWh/total*100)



KOR_1961_2023_GEN_5year$year<-factor(KOR_1961_2023_GEN_5year$year,
                                     levels= c('61-65',
'66-70',
'71-75',
'76-80',
'81-85',
'86-90',
'91-95',
'96-00',
'01-05',
'06-10',
'11-15',
'16-20',
'21-23'))




gen3_5years<-KOR_1961_2023_GEN_5year %>% 
  ggplot(aes(x = year, y = share, fill= type))+
  geom_col()+
  theme_bw()+
#  scale_y_continuous(labels = scales::percent_format())+
  theme(text = element_text(size = 16, family = 'nanumgothic'),
        #legend.position = c(0.1, 0.5),
        #legend.background = element_rect(fill=alpha(0.4)),
        legend.position ="none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle =45, hjust =1))+
  scale_fill_manual(values = c('#533600',
                               '#e88431',
                               '#575e63',
                               '#24bf0c',
                               '#005d9a',
                               '#282422'))+
  labs(x= '',
       y = 'Share (%)',
       title = 'C')+
  guides(fill=guide_legend(title=''))+
  geom_text(data =. %>% filter(type=='Oil' & year %in% c('66-70','71-75', '76-80','81-85')), 
            aes(label = round(share,0)), color = 'white',size= 6,
            position = position_stack(vjust = .5))+
  geom_text(aes(label = ifelse(type=='Coal' & year %in% c('61-65','66-70','71-75', '76-80','81-85'), round(share,0), '')), color = 'white',size= 6,
            position = position_stack(vjust = .5))+
  geom_text(aes(label = ifelse(type%in% c('Coal', 'Nuclear', 'Gas') &
                                 year %in% c('86-90', '91-95','96-00','01-05','06-10','11-15',
                                                                           '16-20','21-23'),
                               round(share,0),"")), color = 'white',size= 6,
            position = position_stack(vjust = .5))+
  geom_text(aes(label = ifelse(type%in% c('Renewable') & year %in% c('06-10','11-15','16-20','21-23'), 
                               round(share,0),"")), color = 'white',size= 6,
            position = position_stack(vjust = .5))


gen5<- elec_11th_plan_eng %>% 
  mutate(year = factor(year)) %>%
  filter(type =='비중' & year %in% c(2030, 2038)) %>% 
  group_by(year, source) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(x = year, y = value, fill = source))+
  geom_col(width = .7)+
  geom_text(aes(label = ifelse(!source %in% c('Hydrogen-ammonia co-firing', 'Others'),round(value,0), "")),
            position =position_stack(vjust= .5),
            color = "white", size = 6)+
  theme_bw()+
  scale_fill_manual(values = c('#533600',
                               '#e88431',
                               '#575e63',
                               '#24bf0c',
                               '#b7a001',
                            
                               'gray80'))+
  #scale_y_continuous(limits = c(0,100), breaks = seq(0,800,200))+
  theme(text = element_text(size = 16, family = 'nanumgothic'),
        #legend.position = c(0.1, 0.5),
        #legend.background = element_rect(fill=alpha(0.4)),
        legend.position ="none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())+
  labs(y = '',
       x = '',
       title = 'D')



gen1+gen4+gen3_5years+gen5+plot_layout(widths = c(4,1))


ggsave("Figure_1.svg",  width = 40, height = 20, units = "cm", dpi = 200)



######################################### Figure2


AR6_meta<- read_excel('AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx', sheet="meta") %>% 
  rename_with("tolower")


AR6_meta_filtered <-AR6_meta %>% 
    select(1,2,3)


## According to a study(https://www.nature.com/articles/s41467-023-41105-z)
## we only consider C1~C3 scenarios that can achieve Paris-Agreement goal

AR6_KOR<- read_csv('AR6_Scenarios_Database_ISO3_v1.1.csv') %>% 
    rename_with("tolower") %>% 
    filter(region =="KOR") %>% 
  mutate(model_scenario = paste(model, scenario)) %>% 
      relocate(model_scenario, .before =model)


### There exists 767 Scenarios which includes Korea as a single region
AR6_KOR %>% 
  distinct(model, scenario)



AR6_KOR %>% 
    group_by(model) %>% 
    summarise(count = n_distinct(scenario)) %>% 
    mutate(total_scenario = sum(count)) %>% 
    arrange(desc(count)) %>%
    ggplot(aes(x = reorder(model, -count), y = count))+
    geom_col()+
    geom_text(aes(label = count), vjust= -.5)+
    theme( axis.text.x = element_text(angle = 60, hjust = 1))


### 767 scenarios by different model family

AR6_KOR %>% 
    filter(region =="KOR") %>% 
    group_by(model) %>% 
    summarise(count = n_distinct(scenario)) %>% 
    mutate(total_scenario = sum(count)) %>% 
    arrange(desc(count)) %>% 
    print(n = 32)

######## Creating dataset for before vetting


AR6_KOR_with_category_before_vetting<- left_join(AR6_KOR, AR6_meta_filtered, by = c("model","scenario")) %>% 
    filter(!str_detect(model_scenario, "GEM-E3")) %>%  ########################### exclude GEM-E3 model, it's historical coal generation is unrealistic 
    pivot_longer(-c('model_scenario', 'model', 'scenario', 'category',
                    'region', 'variable', 'unit'), names_to = "year", values_to ="value") %>% 
  filter(category %in% c('C1','C2','C3','C4','C5','C6','C7','C8')) %>% 
    mutate(year = as.numeric(year),
           model_family = 
               case_when(grepl("AIM", model) ~ "AIM",
                         grepl("MESSAGE", model) ~ "MESSAGE",
                         grepl("POLES", model) ~ "POLES",
                         grepl("REMIND", model) ~ "REMIND",
                         grepl("WITCH", model) ~ "WITCH",
                         grepl("COFFEE", model) ~ "COFFEE",
                         grepl("ENGAGE", model) ~ "ENGAGE",
                         grepl("GCAM", model) ~ "GCAM",
                         grepl("TIAM", model) ~ "TIAM",
                         grepl("IMAGE", model) ~ "IMAGE",
                         grepl("TIMES", model) ~ "TIMES",
                         grepl("GEM-E3", model) ~ "GEM-E3",
                         grepl("IMACLIM", model) ~ "IMACLIM",
                         grepl("DNE21+", model) ~ "DNE21+",
                         grepl("EPPA", model) ~"EPPA",
                         TRUE~"Others")) %>% 
    mutate(category2 = ifelse(category %in% c('C1', 'C2', 'C3'), "Paris-compliant", "Non-compliant"),
           category2 = fct_relevel(category2, c("Paris-compliant", "Non-compliant"))) %>% 
  filter(category %in% c('C1','C2','C3','C4','C5','C6','C7','C8')) 





########### variable setting for capacity
sort(unique(AR6_KOR_with_category_before_vetting$variable))

variable_capacity<-
  c( "Capacity|Electricity|Coal|w/o CCS",
     "Capacity|Electricity|Gas|w/o CCS",
     "Capacity|Electricity|Nuclear",
     "Capacity|Electricity|Solar|PV",
     "Capacity|Electricity|Wind"
     )


####### capacity dataset
AR6_KOR_with_category_capa<-AR6_KOR_with_category_before_vetting %>% 
    filter(variable %in% variable_capacity) %>% 
    mutate(variable = ifelse(str_detect(variable, 'Coal'), 'Coal', variable),
         variable = ifelse(str_detect(variable, 'Gas'), 'Gas', variable),
         variable = ifelse(str_detect(variable, 'Nuclear'), 'Nuclear', variable),
         variable = ifelse(str_detect(variable, 'Solar'), 'Solar', variable),
         variable = ifelse(str_detect(variable, 'Wind'), 'Wind', variable)) %>% 
  filter(year %in% seq(2010, 2100, 5)) %>%
  group_by(variable, model_scenario) %>% 
  mutate(value = na.approx(value, na.rm=F, rule=2)) 



## 198 scenarios
unique(AR6_KOR_with_category_capa$model_scenario)


AR6_KOR_with_category_capa %>% 
  group_by(model_family) %>% 
  summarise(count = n_distinct(model_scenario)) %>% 
  arrange(desc(count)) %>% pull(model_family) ->model_family_level


##### Scenarios list

AR6_KOR_with_category_capa %>% 
    ungroup() %>% 
    distinct(model, scenario,category, category2) %>%
    arrange(category,model) %>% 
    write.csv("Supplementary_Scenario_list.csv")
    

#########Figure S2.

AR6_KOR_with_category_capa %>% 
  group_by(model_family, category) %>% 
  summarise(count = n_distinct(model_scenario)) %>%
  ungroup() %>% 
  mutate(total_scenario = sum(count),
         model_family = fct_relevel(model_family, model_family_level)) %>% 
  arrange(desc(count)) %>%
  ggplot(aes(x = category, y = count, fill = category))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(0, 25))+
  scale_fill_manual(values = c("#97CEE4", "#778663", "#6F7899", "#A7C682", 
                               "#8CA7D0", "#FAC182", "#F18872", "black","#ff268d"),
                    na.value = "gray")+
  #theme_minimal()+
  theme_bw()+
  theme(text = element_text(size = 14, family = 'nanumgothic'),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        panel.grid.major.y =element_blank(),
        legend.position ="none"
  )+
  stat_summary(aes(label=..y.., group = model_family), fun.y=sum, geom="text", size=4,
               hjust = -0.4)+ ## Showing total (sum) values each group on the top of stacked bar chart in ggplot2
  labs(title = "Korean natioanl scenarios for various warming levels(n = 198)",
       y = "Number of scenario",
       x = "Category")+
  facet_wrap(~model_family, nrow = 1)


ggsave("Figure_S2.svg",  width = 20, height = 10, units = "cm", dpi = 200)



######### number of scenarios Paris-compliant = 65, Non-compliant = 133
AR6_KOR_with_category_capa %>% 
    ungroup() %>% 
    distinct(model_scenario, category2) %>% 
    group_by(category2) %>% 
    summarise(count = n())


###### Figure_S3

AR6_KOR_with_category_capa %>%
    rename(Category=category2) %>% 
    filter(year<=2050  & year>=2015) %>% 
    filter(!str_detect(model_scenario, "GEM-E3")) %>% 
    ggplot(aes(x = year, y = value, group =model_scenario, color = Category))+
    geom_line(
        alpha =.3,
        color ="grey",
        linewidth = 1)+
    scale_x_continuous(limits = c(2015,2050), breaks = seq(2015,2050, 5))+
    geom_smooth(aes(group = Category, fill = Category), 
                stat = 'summary', alpha = 0.4, linewidth =1.2,
                fun.data = median_hilow, 
                fun.args = list(conf.int = .5) ### 여기서 confidence level 조절 가능
    )+
    facet_grid2(Category~variable, 
               scales="free_y",
               independent = "y"
    )+
    scale_fill_manual(values = c("#3498db", '#f4481a'))+
    scale_color_manual(values = c('blue', 'red'))+
    theme_bw()+
    #scale_x_continuous(limits =c(2020, 2100), breaks =seq(2020, 2100, 20))+
    theme(text = element_text(size = 16),
          panel.grid.minor = element_blank(),
          legend.position ="top",
          axis.text.x = element_text(angle = 90),
          plot.title.position = "plot",
          plot.title = element_text(face="bold"),
          strip.background = element_blank(),
          strip.text = element_text(face='bold'),
          panel.grid.major.x = element_line(linetype=2, linewidth =.2, color ='gray'))+
    
    labs(y ="Capacity (GW)",
         x="",
         title="A"
         #title = "Korea's electricity capacity pathways in line with the Paris Agreement",
         # subtitle = "- individual pathways : grey lines, - median pathways : dark lines, - the interquartile range(IQR) : shadings"
    )

ggsave("Figure_S3.svg",  width = 30, height = 20, units = "cm", dpi = 200)



### Electricity capacity from 11th basic plan

capacity_11th_2024_2038<-data.frame(
        year = c(2024L,2025L,2026L,2027L,2028L,2029L,
                 2030L,2031L,2032L,2033L,2034L,2035L,2036L,2037L,2038L),
     Nuclear = c(26.1,26.1,28.9,28.9,28.9,28.9,28.9,
                 28.9,30.3,31.7,32,32.4,32.4,33.8,35.2),
        Coal = c(39.8,40.3,38.8,36.2,34.7,32.3,31.3,
                 30.3,29.3,29.3,27.7,27.7,26.7,24.7,21.8),
         Gas = c(46.4,47.3,50.3,53.1,55.4,57.8,58.8,
                 60,63.1,63.1,64.7,64.7,65.7,67.6,69.2),
       Solar = c(28.15,31.964,36.09,40.417,44.822,
                 49.272,55.675,58.711,61.739,64.763,67.784,70.811,72.941,
                 75.072,77.203),
        Wind = c(2.293,3.017,3.958,5.983,8.719,14.661,
                 18.281,22.321,24.868,27.419,29.973,32.972,35.532,38.101,
                 40.671)
) %>% 
  pivot_longer(-year, names_to ='variable', values_to = 'value') %>% 
  mutate(Category ='The 11th Basic Plan')


########## Table S2
capacity_11th_2024_2038 %>%
    mutate(value = round(value,1)) %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    mutate(variable = fct_relevel(variable, c('Coal', 'Gas', 'Nuclear','Solar','Wind'))) %>%
    arrange(variable) %>% 
    write.csv('Table_S2.csv')
               


##########################Figure2

commentary_fig_elec1<-AR6_KOR_with_category_capa %>%
  rename(Category=category2) %>% 
  filter(year<=2050  & year>=2015) %>% 
  filter(!str_detect(model_scenario, "GEM-E3")) %>% 
  ggplot(aes(x = year, y = value, group =model_scenario, color = Category))+
  geom_line(
    alpha =.3,
    color ="grey",
    linewidth = 1)+
  scale_x_continuous(limits = c(2015,2050), breaks = seq(2015,2050, 5))+
  geom_smooth(aes(group = Category, fill = Category), 
              stat = 'summary', alpha = 0.4, linewidth =1.2,
              fun.data = median_hilow, 
              fun.args = list(conf.int = .5) ### 여기서 confidence level 조절 가능
  )+
  facet_wrap(~variable, nrow =1, 
             scales="free_y"
  )+
  scale_fill_manual(values = c("#3498db", '#f4481a'))+
  scale_color_manual(values = c('blue', 'red'))+
  #scale_x_continuous(limits =c(2020, 2100), breaks =seq(2020, 2100, 20))+
  theme(text = element_text(size = 16),
        panel.grid.minor = element_blank(),
        legend.position ="top",
        axis.text.x = element_text(angle = 90),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        strip.background = element_blank(),
        strip.text = element_text(face='bold'),
        panel.grid.major.x = element_line(linetype=2, linewidth =.2, color ='gray'))+
  labs(y ="Capacity (GW)",
       x="",
       title="A"
       #title = "Korea's electricity capacity pathways in line with the Paris Agreement",
       # subtitle = "- individual pathways : grey lines, - median pathways : dark lines, - the interquartile range(IQR) : shadings"
  )


commentary_fig_elec2<-AR6_KOR_with_category_capa %>%
  rename(Category=category2) %>% 
  filter(Category =='Paris-compliant') %>% 
  filter(year<=2050  & year>=2015) %>% 
  ggplot(aes(x = year, y = value, group =model_scenario, color = Category))+
  scale_x_continuous(limits = c(2015,2050), breaks = seq(2015,2050, 5))+
  geom_smooth(aes(group = Category),
              fill = '#3498db',
              stat = 'summary', alpha = 0.4, linewidth =1.2,
              fun.data = median_hilow, 
              fun.args = list(conf.int = .5) ### 여기서 confidence level 조절 가능
  )+
  scale_color_manual(values=c('blue', 'black'))+
  geom_line(data =capacity_11th_2024_2038,
            aes(x = year, y = value, color = Category),
            inherit.aes = FALSE, linetype = 'dashed')+

  facet_wrap(~variable, nrow =1, 
             scales="free_y"
  )+
  #scale_x_continuous(limits =c(2020, 2100), breaks =seq(2020, 2100, 20))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_blank(),
        legend.position ='top',
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        strip.text = element_text(face='bold'),
        strip.background = element_blank(), 
        panel.grid.major.x = element_line(linetype=2, linewidth =.2, color ='gray'))+
  labs(y = "Capacity (GW)",
       x="",
       title="B"
       #title = "Korea's electricity capacity pathways in line with the Paris Agreement",
       # subtitle = "- individual pathways : grey lines, - median pathways : dark lines, - the interquartile range(IQR) : shadings"
  )


ggsave(plot = commentary_fig_elec1/commentary_fig_elec2, "Figure_2.svg",  width = 30, height = 22, units = "cm", dpi = 200)



####### Table_S1

AR6_KOR_with_category_capa %>% 
  filter(year %in% seq(2015,2050,5)) %>% 
  mutate(value = round(value, 1)) %>% 
  group_by(category2, year, variable) %>% 
  summarise(
    median = median(value),
    q25 = round(quantile(value, 0.25),1),
    median = quantile(value, 0.5),
    q75 = round(quantile(value, 0.75),1),
    .groups = "drop"
  ) %>% 
  mutate(IQR = paste0(median, '\n',' [', q25, '-', q75, ']'),
         median = as.character(round(median, 1))) %>% 
  select(-q25, -q75) %>% 
  pivot_longer(-c('category2', 'year','variable'), names_to = 'result', values_to = 'value') %>% 
  filter(result =='IQR') %>% 
  pivot_wider(names_from = c('year'), values_from = value) %>% 
#  mutate(category2 = ifelse(category2 =='Non-compliant', 'Non compliant', 'Paris-compliant')) %>% 
  arrange(result, variable) %>% 
  relocate(variable, .before= category2) %>% 
  relocate(result, .before=variable) %>% 
    select(-result) %>% 
    write.csv('Table_S1.csv')



#################2015-2023 Korea's capicity (source:EPSIS)


## source :EPSIS , 연료원별 발전설비
kor_capa_2015_2023 <-data.frame(
  year = c(2023L,2022L,2021L,2020L,2019L,2018L,
           2017L,2016L,2015L,2013L,2012L),
  Nuclear = c(24.7,24.7,23.3,23.3,23.3,21.9,22.5,
              23.1,21.7,20.7,20.7),
  Coal = c(39.2, 38.1, 37.3, 36.9, 37, 36.9, 36.7, 32, 27.3, 25.2, 25.1),
  Gas = c(43.2,41.2,41.7,41.2,39.7,37.8,37.8,
          32.6,32.2,25.8,21.9),
  Solar = c(23.9, 21, 18.2, 14.6, 10.5, 7.1, 5.1, 3.7, 2.5, 1.1, 0.7),
  Wind = c(2.2, 1.9, 1.7, 1.6, 1.5, 1.4, 1.2, 1.1, 0.8, 0.6, 0.5)
) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "value")

## 240330
## In 2035, - Solar 62.5GW, Wind 32.1GW, Nuclear 31.7GW, Coal 28.1GW, LNG,63.6GW
##작업중


#### Tripling level dataset
dataset<-kor_capa_2015_2023 %>% 
  filter(year ==2023) %>% ##########전기본은 11년 기준으로 함 
  add_row(year =2030, variable = "Solar", value = 23.9*3) %>% 
  add_row(year =2030, variable = "Wind", value = 2.2*3) %>% 
  add_row(year =2050, variable = "Nuclear", value = 24.7*3) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(Renewable = Solar+Wind) %>% 
  pivot_longer(-year, names_to ='variable', values_to ='value')



########## Dataset for calculating value in 2023

AR6_KOR_with_category_add_row_by_2050<-AR6_KOR_with_category_capa %>%
    ungroup() %>% 
  select(-model_scenario,-region,-unit,-category, -model_family) %>% 
  filter(year %in% c(2020,2025, 2030, 2035, 2040, 2050, 2100)) %>% 
  group_by(model, scenario,variable,category2) %>% 
  group_modify(~ add_row(., year = c(2021,2022,2023,2024,2036,2037,2038,2039))) %>% 
  arrange(model, scenario, variable, category2, year) %>% 
  mutate(value = na.approx(value, na.rm=F, rule=2)) %>% 
  mutate(variable = ifelse(str_detect(variable, 'Coal'), 'Coal', variable),
         variable = ifelse(str_detect(variable, 'Gas'), 'Gas', variable),
         variable = ifelse(str_detect(variable, 'Nuclear'), 'Nuclear', variable),
         variable = ifelse(str_detect(variable, 'Solar'), 'Solar', variable),
         variable = ifelse(str_detect(variable, 'Wind'), 'Wind', variable)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = 'variable', values_from = 'value') %>% 
  mutate(Renewable = Solar+Wind) %>% 
  pivot_longer(-c('model','scenario','category2','year'), names_to ="variable", values_to ="value") %>% 
  mutate(category2 = factor(category2, levels = c('Paris-compliant', 'Non-compliant')))
  



#################### Figure3

commentary_fig3<-AR6_KOR_with_category_add_row_by_2050%>% 
  filter((year == 2023 & variable %in% c('Renewable', 'Nuclear'))
         |(year ==2030 & variable =='Renewable')
         |(year ==2050 & variable =='Nuclear'))%>%
  ggplot(aes(x = factor(year), y = value))+
  geom_point(aes(color =category2),
             position = position_jitterdodge(
               #jitter.width = .4
               ),
             size = 1,
             alpha = .1,
             stroke = .9
  )+
  geom_boxplot(aes(fill = category2),
               #position =position_dodge(), 
               outlier.shape = NA, alpha = .6, 
               #width =1,
  )+
  geom_flat_violin(data =. %>% filter(year %in% c(2030, 2050)), 
                   aes(fill =category2),
                   position = position_nudge(x = .5), 
                   width= .5,
                   alpha = .5) +
  facet_wrap(~variable,scales="free_x")+
  scale_fill_manual(values = c('#3498db', '#f4481a'))+
  scale_color_manual(values = c('blue', 'red'))+
  geom_hline(data= . %>% filter(variable =='Nuclear'),aes(yintercept=24.7*3),
             color ='#2d9301', linetype='dashed')+
  geom_hline(data= . %>% filter(variable =='Renewable'), aes(yintercept=26.1*3), 
             color ='#2d9301', linetype='dashed')+
  geom_point(data = dataset %>% drop_na()%>% filter(variable %in% c('Nuclear', 'Renewable') & year ==2023), 
             size = 15,shape = 95,  color ="#0b843d")+
  geom_segment(data= . %>% filter(variable =='Renewable'), aes(x=as.factor(2023), y=26.1, xend=as.factor(2030), yend=26.1*3), 
               arrow = arrow(length=unit(.5, 'cm')), linewidth = .7, color = '#2d9301')+
  geom_segment(data= . %>% filter(variable =='Nuclear'),aes(x=as.factor(2023), y=24.7, xend=as.factor(2050), yend=24.7*3), 
               # label ='Renewable tripling',
               arrow = arrow(length=unit(.5, 'cm')), linewidth = .7, color ='#2d9301')+

  labs(y = 'Capacity (GW)',
       x = '',
       color ="",
       title = 'A'
  )+
  guides(fill=guide_legend(title="Category"))+
  guides(color=guide_legend(title="Category"))+
  theme(
    #legend.position="none",
    text = element_text(family = 'nanumgothic', size = 14),
    
    plot.title = element_text(face="bold"),
    plot.title.position = "plot",
    strip.background = element_blank(),
    strip.text = element_text(face='bold', size = 16)
    )

ggsave(plot = commentary_fig3, "Figure_3A.svg",  width = 25, height = 10, units = "cm", dpi = 200)


############## Figure_3B

commentary_Fig3_b<-AR6_KOR_with_category_add_row_by_2050 %>%
  filter(year %in% c(2023,2030,2050)) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(Fossil = Coal + Gas) %>% 
  ggtern(aes(x=Nuclear,y=Fossil, z=Renewable, color = category2, fill = category2)) +
  scale_color_manual(values = c('blue','red' ))+
  scale_fill_manual(values = c('blue','red' ))+
  geom_encircle(alpha=.2,s_shape=1, expand=0) +  ### ggalt
  geom_point(data = . %>% filter(year %in% c(2023,2030,2050)), size = 1,alpha = .3)+
  facet_wrap(~year)+
  theme_rgbw()+
  theme(legend.position = 'none',
        plot.title = element_text(face="bold"),

            text = element_text(size = 14),
        tern.axis.title.T = element_blank(),  # Top
        tern.axis.title.L = element_blank(),  # Left
        tern.axis.title.R = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face='bold', size = 16))+
  labs(title ='B')


ggsave(plot= commentary_Fig3_b, "Figure_3B.svg",  width = 24, height = 10, units = "cm", dpi = 200)




######### How many scenarios are able to reach tripling goal for renewable energy by 2030?
AR6_KOR_with_category_add_row_by_2050%>% 
  filter(variable %in% c('Renewable') & year == 2030) %>% 
  arrange(desc(value)) %>% 
  mutate(tripling =(23.9+2.2)*3) %>% 
  mutate(goal = ifelse(value>=tripling, 'Y', 'N')) %>% 
  group_by(goal,category2) %>% 
  summarise(count = n())

### 28.8
round(57/198*100,1)


### Out of Paris-compliant, 46.2 % scenarios reach renewable tripling goals
round(30/(30+35)*100,1)


### Out of Non-compliant scenarios, 20.3% scenarios reach renewable tripling goals
round(27/(106+27)*100,1)

AR6_KOR_with_category_add_row_by_2050%>% 
    filter(variable %in% c('Renewable') & year == 2030) %>% 
    arrange(desc(value)) %>% 
    mutate(tripling =(23.9+2.2)*3) %>% 
    mutate(goal = ifelse(value>=tripling, 'Y', 'N')) %>% 
    group_by(category2) %>% 
    summarise(median = median(value))


AR6_KOR_with_category_add_row_by_2050%>% 
  filter(variable %in% c('Nuclear') & year == 2050) %>% 
  arrange(desc(value)) %>% 
  mutate(tripling =24.7*3) %>% 
  mutate(goal = ifelse(value>=tripling, 'Y', 'N')) %>% 
  group_by(goal,category2) %>% 
  summarise(count = n())

### Out of Paris-compliant scenarios,  10.8% scenarios reach nuclear tripling goals
round(7/(58+7)*100, 1)





########### variable setting for electricity generation share
sort(unique(AR6_KOR_with_category_before_vetting$variable))

variable_gen_share<-
  c("Secondary Energy|Electricity",
    "Secondary Energy|Electricity|Coal|w/o CCS",
    "Secondary Energy|Electricity|Gas|w/o CCS",
    "Secondary Energy|Electricity|Nuclear",
    "Secondary Energy|Electricity|Solar|PV", 
    "Secondary Energy|Electricity|Wind" 
    )

####### generation dataset


unique(AR6_KOR_with_category_before_vetting$model_scenario)


AR6_KOR_with_category_gen_share<-AR6_KOR_with_category_before_vetting %>% 
  filter(category %in% c('C1','C2','C3','C4','C5','C6','C7','C8')) %>% 
  filter(variable %in% variable_gen_share)  %>%
  group_by(variable) %>% 
  mutate(value = na.approx(value, na.rm=F, rule=2)) %>% 
  select(-model, -scenario, -region, -model_family, -unit, -category) %>% 
  pivot_wider(names_from = variable, values_from = value)

names(AR6_KOR_with_category_gen_share)[4]<-'Electricity'
names(AR6_KOR_with_category_gen_share)[5]<-'Coal'
names(AR6_KOR_with_category_gen_share)[6]<-'Gas'
names(AR6_KOR_with_category_gen_share)[7]<-'Nuclear'
names(AR6_KOR_with_category_gen_share)[8]<-'Solar'
names(AR6_KOR_with_category_gen_share)[9]<-'Wind'






############# fossill fuel generation share without carbon capture

EPSIS_fossil_fuel<-data.frame(
         pct = c(0.509347193,0.52609292,0.5591207,
                 0.547126378,0.589448008,0.621804599,0.609065327,0.59150992,
                 0.600886526,0.619251644,0.66538753,0.644789554,0.630310091,
                 0.644680876,0.62627979,0.612331309,0.620989177,0.585735495,
                 0.572670908,0.587694392,0.568827089,0.586910023,0.585138134,
                 0.561035394,0.533316141,0.545262928,0.62605685,0.607934662,
                 0.599513651,0.611129534,0.546606201,0.521656734,0.476020845,
                 0.444312367,0.445824443,0.484972422,0.392740756,
                 0.497740748,0.646746742,0.73542898,0.759843063,0.864847821,
                 0.859627825,0.851958019,0.840639006,0.853874741,0.928261588,
                 0.91198474,0.904918428,0.876001228,0.90992858,0.878672573,
                 0.858622405,0.82758533,0.727530036,0.715640418,0.736926498,
                 0.732164001,0.767782955,0.694282491,0.640244161,0.606021918,
                 0.630755121),
        year = c(2023L,2022L,2021L,2020L,2019L,2018L,
                 2017L,2016L,2015L,2014L,2013L,2012L,2011L,2010L,2009L,
                 2008L,2007L,2006L,2005L,2004L,2003L,2002L,2001L,2000L,
                 1999L,1998L,1997L,1996L,1995L,1994L,1993L,1992L,1991L,
                 1990L,1989L,1988L,1987L,1986L,1985L,1984L,1983L,1982L,
                 1981L,1980L,1979L,1978L,1977L,1976L,1975L,1974L,1973L,
                 1972L,1971L,1970L,1969L,1968L,1967L,1966L,1965L,1964L,
                 1963L,1962L,1961L)
) %>% 
  mutate(pct = pct*100)



########### Figure S4


AR6_KOR_with_category_gen_share_completed<- AR6_KOR_with_category_gen_share %>% 
    pivot_longer(-c('model_scenario', 'year', 'category2', 'Electricity'), names_to='variable', values_to = 'value') %>% 
    mutate(pct = round(value/Electricity*100,1))


AR6_KOR_with_category_gen_share_completed %>%
  select(-Electricity, -value) %>% 
  pivot_wider(names_from = variable, values_from = pct) %>%
  mutate(Fossil = Coal + Gas,
         Renewable = Solar + Wind) %>% 
  pivot_longer(-c(model_scenario, year, category2), names_to = 'variable', values_to ='pct') %>% 
  filter(variable =='Fossil') %>% 
  filter(year >= 2015, year <= 2050) %>% 
  ggplot()+
  geom_line(aes(x = year, y = pct, group = model_scenario, color = category2),size = 1, alpha =.1)+
  geom_line(data = EPSIS_fossil_fuel, aes(x = year, y= pct), size = 2)+
    scale_color_manual(values = c('blue','red' ))+
    labs(y = "Share of electricity generation from fossil fuel (%)",
         x= "")+
    theme(legend.position = "none")+
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    annotate("text", x = 2000, y = 70, label = "Historical", size = 5, fontface = "bold")+
    annotate("text", x = 2020, y = 30, label = "Paris-compliant", size = 5, fontface = "bold", color = 'blue')+
    annotate("text", x = 2040, y = 70, label = "Non-compliant", size = 5, fontface = "bold", color = 'red')
    

ggsave("Figure_S4.svg",   width =10, height = 5,  dpi = 700, bg="white")


############## Table S3

AR6_KOR_with_category_gen_share_completed %>%
    select(-Electricity, -value) %>% 
    pivot_wider(names_from = variable, values_from = pct) %>%
    mutate(Fossil = Coal + Gas,
           Renewable = Solar + Wind) %>% 
    pivot_longer(-c(model_scenario, year, category2), names_to = 'variable', values_to ='pct') %>% 
    filter(variable =='Fossil') %>% 
    filter(year %in% c(2015,2020,2025,2030,2035,2040,2045,2050)) %>% 
    group_by(category2, year) %>% 
    summarise(    median = round(median(pct),1),
                  q25 = round(quantile(pct, 0.25),1),
                  median = quantile(pct, 0.5),
                  q75 = round(quantile(pct, 0.75),1),
                  .groups = "drop"
    ) %>% 
    mutate(value = paste0(median, '\n',' [', q25, '-', q75, ']')) %>% 
    select(1,2,6) %>% 
    pivot_wider(names_from = c('year'), values_from = value) %>% 

    write.csv("Table_S3.csv")



### dataset for GHG
### Figure S6

AR6_KOR_with_category_ghg_emission<-left_join(model_scenario_analysis, AR6_KOR_with_category_before_vetting, by =c('model_scenario')) %>% 
    filter(variable=="Emissions|Kyoto Gases")



figure_s6<- AR6_KOR_with_category_ghg_emission %>%
    filter(year %in% seq(2020, 2050, 5)) %>% 
    filter(year >=2020) %>% 
    #mutate(value = na.approx(value, na.rm=F, rule=2)) %>% 
    ggplot(aes(x = year, y = value)) +
    #stat_lineribbon() +   
    
    #scale_color_manual(values = c('blue', "red", 'black', 'black'))+
    scale_x_continuous(limits = c(1990, 2050), 
                       breaks = c(seq(1990, 2010, 10), 2020, 2030, 2040, 2050))+
    #geom_line(aes(group = interaction(model, scenario)), color ='gray', alpha = .4, size =.7)+
    geom_line(data = . %>% filter(category2 =="Non-compliant"),aes(group = interaction(model, scenario)), color ='red', alpha = .1, size =1)+
    geom_line(data = . %>% filter(category2 == "Paris-compliant"),aes(group = interaction(model, scenario)), color ='blue', alpha = .1, size =1)+
    geom_smooth(data =. %>% filter(category %in% c('C4','C5','C6','C7','C8')),
                fill = "#F18872", color="red", 
                stat = 'summary', alpha = 0.7, linetype ='dashed',
                fun.data = median_hilow, 
                fun.args = list(conf.int = .5))+
    geom_smooth(data =. %>% filter(category %in% c('C1','C2','C3')),
                color ="blue", fill = "#8CA7D0", 
                stat = 'summary', alpha = 0.7, linetype ="dashed",
                fun.data = median_hilow, 
                fun.args = list(conf.int = .5))+ 
    geom_line(data = KOR_GHG,linewidth = 1.5)+
    #geom_area(data = GHG_KOR_1990_2021_by_sector, aes(fill =sector), alpha = .7)+    
    
    #geom_line(data = GHG_KOR_1990_2021_net, color = 'white', size = 1, linetype ='dashed')+  
    

    #geom_line(data = NDC , color ="white", size = 3) +    
    #geom_line(data = NDC , color ="#25396c", size = 1.5,
    #          arrow = arrow(length=unit(0.30,"cm")))+    
    #geom_point(data = data.frame(year=c(2050),value=c(133)), color = "black", size = 3, shape = 4)+
    #geom_point(data = data.frame(year=c(2030),value=c(436)), color = "#25396c", size = 2,  shape =4)+
    scale_fill_manual(values = c('black','coral4',
                                 'darkgreen','darkred','yellowgreen'))+
    #geom_text(data =GHG_KOR_1990_2021_by_sector %>% filter (year == 2021), 
    #          aes(x = 2021.5, label =sector, group = sector), position =position_stack(vjust = 0.4), hjust = 0)+
    #theme_bw()+
    guides(colour = guide_legend(override.aes = list(alpha=1, fill=NA)))+ # Change alpha level of geom_point in legend on top of stat_smooth
    theme(plot.subtitle = element_markdown(margin=margin(0,0,0,0)),
          legend.position = 'none',
          legend.background = element_rect(fill=alpha('NA', 0.4)),
          # axis.text.x = element_text(angle = 90),panel.grid.minor.x = element_blank(),
          plot.title.position = "plot",
          plot.title=element_text(face="bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank()
    )+
    labs(y = "GHG emissions (MtCO2eq)",
         x = '',
         #title ="a",
         #title = "GHG emissions 2015-2100",
         #subtitle = "Black line : NDC goal by 2030"
    )+
    annotate("text", x = 2030, y = 200, label = "Paris-compliant", size = 5, fontface = "bold", color = 'blue')+
    annotate("text", x = 2045, y = 600, label = "Non-compliant", size = 5, fontface = "bold", color = 'red')
    
figure_s6

ggsave(plot = figure_s6, file= "Figure_S6.svg",  width =10, height = 5,  dpi = 700, bg="white")



########## Table S4
AR6_KOR_with_category_ghg_emission %>%
    filter(year %in% seq(2020, 2050, 5)) %>% 
    filter(year >=2020) %>% 
    group_by(category2, year) %>% 
    summarise(    median = round(median(value),1),
                  q25 = round(quantile(value, 0.25),1),
                  median = quantile(value, 0.5),
                  q75 = round(quantile(value, 0.75),1),
                  .groups = "drop"
    ) %>% 
    mutate(value = paste0(round(median,0), '\n',' [', q25, '-', q75, ']')) %>% 
    select(1,2,6) %>% 
    pivot_wider(names_from = c('year'), values_from = value) %>% 
    write.csv("Table_S4.csv")





 
