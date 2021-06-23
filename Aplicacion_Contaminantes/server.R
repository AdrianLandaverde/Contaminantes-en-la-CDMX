

library(shiny)
library(tidyverse)
library(dplyr, quietly = TRUE)
library(gapminder)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$regresionLinealGrafico <- renderPlot({
        contaminante = "SO2"
        periodo_Regresion_Lineal<- input$Periodo # dia, mes, annio
        
        contam_csv_2021 <- "2021/promedios_2021_so2.csv"
        contam_csv_2020 <- "2020/promedios_2020_so2.csv"
        contam_csv_2019 <- "2019/promedios_2019_so2.csv"
        contam_csv_2018 <- "2018/promedios_2018_so2.csv"
        contam_csv_2017 <- "2017/promedios_2017_so2.csv"
        contam_csv_2016 <- "2016/promedios_2016_so2.csv"
        contam_csv_2015 <- "2015/promedios_2015_so2.csv"
        contam_csv_2014 <- "2014/promedios_2014_so2.csv"
        contam_csv_2013 <- "2013/promedios_2013_so2.csv"
        contam_csv_2012 <- "2012/promedios_2012_so2.csv"
        contam_csv_2011 <- "2011/promedios_2011_so2.csv"
        contam_csv_2010 <- "2010/promedios_2010_so2.csv"
        contam_csv_2009 <- "2009/promedios_2009_so2.csv"
        
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2021.
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2021 <- read_csv(contam_csv_2021, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2021 <- contaminantes_2021 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2020.
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2020 <- read_csv(contam_csv_2020, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2020 <- contaminantes_2020 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2019
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2019 <- read_csv(contam_csv_2019, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2019 <- contaminantes_2019 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2018
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2018 <- read_csv(contam_csv_2018, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2018 <- contaminantes_2018 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2017
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2017 <- read_csv(contam_csv_2017, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2017 <- contaminantes_2017 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2016
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2016 <- read_csv(contam_csv_2016, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2016 <- contaminantes_2016 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2015
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2015 <- read_csv(contam_csv_2015, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2015 <- contaminantes_2015 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2014
        #--------------------------------------------------------------------------
        
        
        # Contaminantes.
        contaminantes_2014 <- read_csv(contam_csv_2014, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2014 <- contaminantes_2014 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2013
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2013 <- read_csv(contam_csv_2013, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2013 <- contaminantes_2013 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2012
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2012 <- read_csv(contam_csv_2012, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2012 <- contaminantes_2012 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2011
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2011 <- read_csv(contam_csv_2011, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2011 <- contaminantes_2011 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2010
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2010 <- read_csv(contam_csv_2010, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2010 <- contaminantes_2010 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2009
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2009 <- read_csv(contam_csv_2009, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2009 <- contaminantes_2009 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        
        #--------------------------------------------------------------------------
        # JUNTAR TODOS
        #--------------------------------------------------------------------------
        contaminantes_2020 %>%
            #  union_all(contaminantes_2020) %>%
            union_all(contaminantes_2019) %>%
            union_all(contaminantes_2018) %>%
            union_all(contaminantes_2017) %>%
            union_all(contaminantes_2016) %>%
            union_all(contaminantes_2015) %>%
            union_all(contaminantes_2014) %>%
            union_all(contaminantes_2013) %>%
            union_all(contaminantes_2012) %>%
            union_all(contaminantes_2011) %>%
            union_all(contaminantes_2010) %>%  
            union_all(contaminantes_2009) -> df_todos
        
        #----------------------------------------------------------------------------
        #*********************-ARREGLAR FECHA****************************************
        
        #Quitar datos con fecha erronea
        df_todos<- df_todos[substring(df_todos$date,3,3)=="/", ]
        
        df_todos_RL<- df_todos
        
        if(periodo_Regresion_Lineal== "annio"){
            df_todos_RL$date<- paste("01/01/",substring(df_todos_RL$date,7,8),sep = "") 
        }else if(periodo_Regresion_Lineal=="mes"){
            df_todos_RL$date<- paste("01/",substring(df_todos_RL$date,4,8),sep = "")
        }else if(periodo_Regresion_Lineal=="dia"){
            df_todos_RL$date<- substring(df_todos_RL$date,1,8)
        }
        
        df_todos_RL$date<- as.Date(df_todos_RL$date, format="%d/%m/%y")
        
        df_todos %>%
            group_by(id_station) %>%
            tally()
        
        id_station <- df_todos %>% 
            group_by(id_station) %>% tally()
        
        estacion=input$Estacion
        
        if(estacion==1){
            df_AJM_RL <- df_todos_RL[df_todos_RL$id_station=="AJM", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA AJM
            #--------------------------------------------------------------------------
            df_AJM_RL<- select(df_AJM_RL, date, partes_por_billon)
            df_AJM_RL %>%
                group_by(date) %>%
                tally()
            df_AJM_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_AJM_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL AJM
            #--------------------------------------------------------------------------
            regresion=lm(df_AJM_RL$partes_por_billon~df_AJM_RL$date, data=df_AJM_RL)
            summary(regresion)
            df_AJM_RL<- cbind(df_AJM_RL,predict(regresion))
            ggplot(df_AJM_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación AJM: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
        }else if(estacion==2){
            df_CUT_RL <- df_todos_RL[df_todos_RL$id_station=="CUT", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA CUT
            #--------------------------------------------------------------------------
            df_CUT_RL<- select(df_CUT_RL, date, partes_por_billon)
            df_CUT_RL %>%
                group_by(date) %>%
                tally()
            df_CUT_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_CUT_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL CUT
            #--------------------------------------------------------------------------
            regresion=lm(df_CUT_RL$partes_por_billon~df_CUT_RL$date, data=df_CUT_RL)
            summary(regresion)
            df_CUT_RL<- cbind(df_CUT_RL,predict(regresion))
            ggplot(df_CUT_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación CUT: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
        }else if(estacion==3){
            df_HGM_RL <- df_todos_RL[df_todos_RL$id_station=="HGM", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA HGM
            #--------------------------------------------------------------------------
            df_HGM_RL<- select(df_HGM_RL, date, partes_por_billon)
            df_HGM_RL %>%
                group_by(date) %>%
                tally()
            df_HGM_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_HGM_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL HGM
            #--------------------------------------------------------------------------
            regresion=lm(df_HGM_RL$partes_por_billon~df_HGM_RL$date, data=df_HGM_RL)
            summary(regresion)
            df_HGM_RL<- cbind(df_HGM_RL,predict(regresion))
            ggplot(df_HGM_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación HGM: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
        }else if(estacion==4){
            df_INN_RL <- df_todos_RL[df_todos_RL$id_station=="INN", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA INN
            #--------------------------------------------------------------------------
            df_INN_RL<- select(df_INN_RL, date, partes_por_billon)
            df_INN_RL %>%
                group_by(date) %>%
                tally()
            df_INN_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_INN_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL INN
            #--------------------------------------------------------------------------
            regresion=lm(df_INN_RL$partes_por_billon~df_INN_RL$date, data=df_INN_RL)
            summary(regresion)
            df_INN_RL<- cbind(df_INN_RL,predict(regresion))
            ggplot(df_INN_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación INN: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
            
        }else if(estacion==5){
            df_MER_RL <- df_todos_RL[df_todos_RL$id_station=="MER", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA MER
            #--------------------------------------------------------------------------
            df_MER_RL<- select(df_MER_RL, date, partes_por_billon)
            df_MER_RL %>%
                group_by(date) %>%
                tally()
            df_MER_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_MER_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL MER
            #--------------------------------------------------------------------------
            regresion=lm(df_MER_RL$partes_por_billon~df_MER_RL$date, data=df_MER_RL)
            summary(regresion)
            df_MER_RL<- cbind(df_MER_RL,predict(regresion))
            ggplot(df_MER_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación MER: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
            
        }else if(estacion==6){
            df_MON_RL <- df_todos_RL[df_todos_RL$id_station=="MON", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA MON
            #--------------------------------------------------------------------------
            df_MON_RL<- select(df_MON_RL, date, partes_por_billon)
            df_MON_RL %>%
                group_by(date) %>%
                tally()
            df_MON_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_MON_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL MON
            #--------------------------------------------------------------------------
            regresion=lm(df_MON_RL$partes_por_billon~df_MON_RL$date, data=df_MON_RL)
            summary(regresion)
            df_MON_RL<- cbind(df_MON_RL,predict(regresion))
            ggplot(df_MON_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación MON: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
            
        }else if(estacion==7){
            df_MPA_RL <- df_todos_RL[df_todos_RL$id_station=="MPA", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA MPA
            #--------------------------------------------------------------------------
            df_MPA_RL<- select(df_MPA_RL, date, partes_por_billon)
            df_MPA_RL %>%
                group_by(date) %>%
                tally()
            df_MPA_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_MPA_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL MPA
            #--------------------------------------------------------------------------
            regresion=lm(df_MPA_RL$partes_por_billon~df_MPA_RL$date, data=df_MPA_RL)
            summary(regresion)
            df_MPA_RL<- cbind(df_MPA_RL,predict(regresion))
            ggplot(df_MPA_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación MPA: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
            
        }else if(estacion==8){
            df_TLA_RL <- df_todos_RL[df_todos_RL$id_station=="TLA", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA TLA
            #--------------------------------------------------------------------------
            df_TLA_RL<- select(df_TLA_RL, date, partes_por_billon)
            df_TLA_RL %>%
                group_by(date) %>%
                tally()
            df_TLA_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_TLA_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL TLA
            #--------------------------------------------------------------------------
            regresion=lm(df_TLA_RL$partes_por_billon~df_TLA_RL$date, data=df_TLA_RL)
            summary(regresion)
            df_TLA_RL<- cbind(df_TLA_RL,predict(regresion))
            ggplot(df_TLA_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación TLA: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
        }else if(estacion==9){
            df_XAL_RL <- df_todos_RL[df_todos_RL$id_station=="XAL", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA XAL
            #--------------------------------------------------------------------------
            df_XAL_RL<- select(df_XAL_RL, date, partes_por_billon)
            df_XAL_RL %>%
                group_by(date) %>%
                tally()
            df_XAL_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_XAL_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL XAL
            #--------------------------------------------------------------------------
            regresion=lm(df_XAL_RL$partes_por_billon~df_XAL_RL$date, data=df_XAL_RL)
            summary(regresion)
            df_XAL_RL<- cbind(df_XAL_RL,predict(regresion))
            ggplot(df_XAL_RL, aes(x=date, y=partes_por_billon)) +
                geom_smooth(method="lm", se=FALSE, color="lightgrey") +
                geom_segment(aes(xend=date, yend=`predict(regresion)`), col='red', lty='dashed') +
                geom_point() +
                geom_point(aes(y=`predict(regresion)`), col='red') +
                theme_light() +
                ggtitle ("Estación MPA: Concentración del dióxido de azufre.") + # Título del gráfico.
                theme(plot.title = element_text(hjust = 0.5)) + # Centrar título
                labs(x = "Año", 
                     y = "Concentración SO2, ppb")
            
        }
    })
    output$resumen <- renderPrint({
        
        contaminante = "SO2"
        periodo_Regresion_Lineal<- input$Periodo # dia, mes, annio
        
        contam_csv_2021 <- "2021/promedios_2021_so2.csv"
        contam_csv_2020 <- "2020/promedios_2020_so2.csv"
        contam_csv_2019 <- "2019/promedios_2019_so2.csv"
        contam_csv_2018 <- "2018/promedios_2018_so2.csv"
        contam_csv_2017 <- "2017/promedios_2017_so2.csv"
        contam_csv_2016 <- "2016/promedios_2016_so2.csv"
        contam_csv_2015 <- "2015/promedios_2015_so2.csv"
        contam_csv_2014 <- "2014/promedios_2014_so2.csv"
        contam_csv_2013 <- "2013/promedios_2013_so2.csv"
        contam_csv_2012 <- "2012/promedios_2012_so2.csv"
        contam_csv_2011 <- "2011/promedios_2011_so2.csv"
        contam_csv_2010 <- "2010/promedios_2010_so2.csv"
        contam_csv_2009 <- "2009/promedios_2009_so2.csv"
        
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2021.
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2021 <- read_csv(contam_csv_2021, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2021 <- contaminantes_2021 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2020.
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2020 <- read_csv(contam_csv_2020, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2020 <- contaminantes_2020 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2019
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2019 <- read_csv(contam_csv_2019, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2019 <- contaminantes_2019 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2018
        #--------------------------------------------------------------------------
        # Contaminantes.
        contaminantes_2018 <- read_csv(contam_csv_2018, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2018 <- contaminantes_2018 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2017
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2017 <- read_csv(contam_csv_2017, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2017 <- contaminantes_2017 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2016
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2016 <- read_csv(contam_csv_2016, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2016 <- contaminantes_2016 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2015
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2015 <- read_csv(contam_csv_2015, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2015 <- contaminantes_2015 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2014
        #--------------------------------------------------------------------------
        
        
        # Contaminantes.
        contaminantes_2014 <- read_csv(contam_csv_2014, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2014 <- contaminantes_2014 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2013
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2013 <- read_csv(contam_csv_2013, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2013 <- contaminantes_2013 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2012
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2012 <- read_csv(contam_csv_2012, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2012 <- contaminantes_2012 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2011
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2011 <- read_csv(contam_csv_2011, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2011 <- contaminantes_2011 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2010
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2010 <- read_csv(contam_csv_2010, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2010 <- contaminantes_2010 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        #--------------------------------------------------------------------------
        # LECTURA DE LOS DATOS DEL 2009
        #--------------------------------------------------------------------------
        
        # Contaminantes.
        contaminantes_2009 <- read_csv(contam_csv_2009, 
                                       col_types = cols(
                                           date = col_character(),
                                           id_station = col_character(),
                                           id_parameter = col_character(),
                                           value = col_double(),
                                           unit = col_double())
        )
        
        contaminantes_2009 <- contaminantes_2009 %>% 
            filter(id_parameter==contaminante) %>%
            dplyr::select(date,id_station, id_parameter, value, unit) %>% 
            rename(partes_por_billon=value) %>%
            drop_na() %>%
            select(-c(unit))
        
        #--------------------------------------------------------------------------
        # JUNTAR TODOS
        #--------------------------------------------------------------------------
        contaminantes_2020 %>%
            #  union_all(contaminantes_2020) %>%
            union_all(contaminantes_2019) %>%
            union_all(contaminantes_2018) %>%
            union_all(contaminantes_2017) %>%
            union_all(contaminantes_2016) %>%
            union_all(contaminantes_2015) %>%
            union_all(contaminantes_2014) %>%
            union_all(contaminantes_2013) %>%
            union_all(contaminantes_2012) %>%
            union_all(contaminantes_2011) %>%
            union_all(contaminantes_2010) %>%  
            union_all(contaminantes_2009) -> df_todos
        
        #----------------------------------------------------------------------------
        #*********************-ARREGLAR FECHA****************************************
        
        #Quitar datos con fecha erronea
        df_todos<- df_todos[substring(df_todos$date,3,3)=="/", ]
        
        df_todos_RL<- df_todos
        
        if(periodo_Regresion_Lineal== "annio"){
            df_todos_RL$date<- paste("01/01/",substring(df_todos_RL$date,7,8),sep = "") 
        }else if(periodo_Regresion_Lineal=="mes"){
            df_todos_RL$date<- paste("01/",substring(df_todos_RL$date,4,8),sep = "")
        }else if(periodo_Regresion_Lineal=="dia"){
            df_todos_RL$date<- substring(df_todos_RL$date,1,8)
        }
        
        df_todos_RL$date<- as.Date(df_todos_RL$date, format="%d/%m/%y")
        
        df_todos %>%
            group_by(id_station) %>%
            tally()
        
        id_station <- df_todos %>% 
            group_by(id_station) %>% tally()
        
        estacion=input$Estacion
        
        if(estacion==1){
            df_AJM_RL <- df_todos_RL[df_todos_RL$id_station=="AJM", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA AJM
            #--------------------------------------------------------------------------
            df_AJM_RL<- select(df_AJM_RL, date, partes_por_billon)
            df_AJM_RL %>%
                group_by(date) %>%
                tally()
            df_AJM_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_AJM_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL AJM
            #--------------------------------------------------------------------------
            regresion=lm(df_AJM_RL$partes_por_billon~df_AJM_RL$date, data=df_AJM_RL)
            summary(regresion)

        }else if(estacion==2){
            df_CUT_RL <- df_todos_RL[df_todos_RL$id_station=="CUT", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA CUT
            #--------------------------------------------------------------------------
            df_CUT_RL<- select(df_CUT_RL, date, partes_por_billon)
            df_CUT_RL %>%
                group_by(date) %>%
                tally()
            df_CUT_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_CUT_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL CUT
            #--------------------------------------------------------------------------
            regresion=lm(df_CUT_RL$partes_por_billon~df_CUT_RL$date, data=df_CUT_RL)
            summary(regresion)

        }else if(estacion==3){
            df_HGM_RL <- df_todos_RL[df_todos_RL$id_station=="HGM", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA HGM
            #--------------------------------------------------------------------------
            df_HGM_RL<- select(df_HGM_RL, date, partes_por_billon)
            df_HGM_RL %>%
                group_by(date) %>%
                tally()
            df_HGM_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_HGM_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL HGM
            #--------------------------------------------------------------------------
            regresion=lm(df_HGM_RL$partes_por_billon~df_HGM_RL$date, data=df_HGM_RL)
            summary(regresion)
  
        }else if(estacion==4){
            df_INN_RL <- df_todos_RL[df_todos_RL$id_station=="INN", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA INN
            #--------------------------------------------------------------------------
            df_INN_RL<- select(df_INN_RL, date, partes_por_billon)
            df_INN_RL %>%
                group_by(date) %>%
                tally()
            df_INN_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_INN_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL INN
            #--------------------------------------------------------------------------
            regresion=lm(df_INN_RL$partes_por_billon~df_INN_RL$date, data=df_INN_RL)
            summary(regresion)

            
        }else if(estacion==5){
            df_MER_RL <- df_todos_RL[df_todos_RL$id_station=="MER", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA MER
            #--------------------------------------------------------------------------
            df_MER_RL<- select(df_MER_RL, date, partes_por_billon)
            df_MER_RL %>%
                group_by(date) %>%
                tally()
            df_MER_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_MER_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL MER
            #--------------------------------------------------------------------------
            regresion=lm(df_MER_RL$partes_por_billon~df_MER_RL$date, data=df_MER_RL)
            summary(regresion)
            
        }else if(estacion==6){
            df_MON_RL <- df_todos_RL[df_todos_RL$id_station=="MON", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA MON
            #--------------------------------------------------------------------------
            df_MON_RL<- select(df_MON_RL, date, partes_por_billon)
            df_MON_RL %>%
                group_by(date) %>%
                tally()
            df_MON_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_MON_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL MON
            #--------------------------------------------------------------------------
            regresion=lm(df_MON_RL$partes_por_billon~df_MON_RL$date, data=df_MON_RL)
            summary(regresion)
            
        }else if(estacion==7){
            df_MPA_RL <- df_todos_RL[df_todos_RL$id_station=="MPA", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA MPA
            #--------------------------------------------------------------------------
            df_MPA_RL<- select(df_MPA_RL, date, partes_por_billon)
            df_MPA_RL %>%
                group_by(date) %>%
                tally()
            df_MPA_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_MPA_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL MPA
            #--------------------------------------------------------------------------
            regresion=lm(df_MPA_RL$partes_por_billon~df_MPA_RL$date, data=df_MPA_RL)
            summary(regresion)
            
        }else if(estacion==8){
            df_TLA_RL <- df_todos_RL[df_todos_RL$id_station=="TLA", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA TLA
            #--------------------------------------------------------------------------
            df_TLA_RL<- select(df_TLA_RL, date, partes_por_billon)
            df_TLA_RL %>%
                group_by(date) %>%
                tally()
            df_TLA_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_TLA_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL TLA
            #--------------------------------------------------------------------------
            regresion=lm(df_TLA_RL$partes_por_billon~df_TLA_RL$date, data=df_TLA_RL)
            summary(regresion)
        }else if(estacion==9){
            df_XAL_RL <- df_todos_RL[df_todos_RL$id_station=="XAL", ]
            #--------------------------------------------------------------------------
            # PROMEDIAR POR FECHA XAL
            #--------------------------------------------------------------------------
            df_XAL_RL<- select(df_XAL_RL, date, partes_por_billon)
            df_XAL_RL %>%
                group_by(date) %>%
                tally()
            df_XAL_RL%>%group_by(date)%>%
                summarise_all(mean)-> df_XAL_RL
            #--------------------------------------------------------------------------
            # REGRESIÓN LINEAL XAL
            #--------------------------------------------------------------------------
            regresion=lm(df_XAL_RL$partes_por_billon~df_XAL_RL$date, data=df_XAL_RL)
            summary(regresion)
            
        }
        
        
        
        
    })

})
