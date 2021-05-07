##########################################################################################
#RCP_26
data_26_ts <- lapply(
    data_26, 
    function(data = "") subset(data, Date >= "2017-01-01" & Date <= "2050-12-31")
) %>% 
    lapply(sum_mean_work)


#RCP_85
data_85_ts <- lapply(
    data, 
    function(data = "") subset(data, Year >= "2017-01-01" & Year <= "2050-12-31")
) %>% 
    lapply(sum_mean_work)


#Printing the following to Screen
observed_1
data_26_ts
data_85_ts

#Bole
Bole <- list(
    observed = observed_1$bo_1,
    RCP26 = data_26_ts$BoleRCP26,
    RCP85 = data_85_ts$BoleRCP85
)

#Navrongo
Navrongo <- list(
    observed = observed_1$na_1,
    RCP26 = data_26_ts$NavrongoRCP26,
    RCP85 = data_85_ts$NavrongoRCP85
)

#Tamale
Tamale <- list(
    observed = observed_1$tm_1,
    RCP26 = data_26_ts$TamaleRCP26,
    RCP85 = data_85_ts$TamaleRCP85
)

#Wa
Wa <- list(
    observed = observed_1$wa_1,
    RCP26 = data_26_ts$WaRCP26,
    RCP85 = data_85_ts$WaRCP85
)

#Yendi
Yendi <- list(
    observed = observed_1$ye_1,
    RCP26 = data_26_ts$YendiRCP26,
    RCP85 = data_85_ts$YendiRCP85
)



#Plotting
require(tidyverse)

#Plotting All Stations with their respective mean annual observed-projected Rainfall and Temperature
gridExtra::grid.arrange(
    ggplot(
        data = data.frame(Date = 1981:2016, Prcp = Bole$observed$prcp),
        aes(x = Date, y = Prcp)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Bole$RCP85[,1]), 
            Prcp = Bole$RCP85[ ,"Rain"]), 
            aes(x = Date, y = Prcp, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Bole$RCP26[,1]),
            Prcp = Bole$RCP26$Rain),
            aes(x = Date,y = Prcp, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Bole Rainfall", y = "Rainfall(mm)") +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmax = Bole$observed$tmax),
        aes(x = Date, y = Tmax)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Bole$RCP85[,1]), 
            Tmax = Bole$RCP85[ ,"Tmax"]), 
            aes(x = Date, y = Tmax, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Bole$RCP26[,1]),
            Tmax = Bole$RCP26$Tmax),
            aes(x = Date,y = Tmax, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Bole Maximum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmin = Bole$observed$tmin),
        aes(x = Date, y = Tmin)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Bole$RCP85[,1]), 
            Tmin = Bole$RCP85[ ,"Tmin"]), 
            aes(x = Date, y = Tmin, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Bole$RCP26[,1]),
            Tmin = Bole$RCP26$Tmin),
            aes(x = Date,y = Tmin, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Bole Minimum Temperature", 
             y = expression("Temperature("*degree*C*")")) + 
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    nrow = 2, ncol = 2
)
dev.copy(png, filename = "Bole_observed_26_85.png", width = 1500, height = 800)
dev.off()


#Navrongo Prcp + RCP26
#date_4 <- c(
#    format(Bole$observed$data...1., "%Y"), 
#    Bole$RCP26$format.data....Date......Y..
#) %>% 
#    as.numeric

gridExtra::grid.arrange(
    ggplot(
        data = data.frame(Date = 1981:2016, Prcp = Navrongo$observed$prcp),
        aes(x = Date, y = Prcp)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Navrongo$RCP85[,1]), 
            Prcp = Navrongo$RCP85[ ,"Rain"]), 
            aes(x = Date, y = Prcp, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Navrongo$RCP26[,1]),
            Prcp = Navrongo$RCP26$Rain),
            aes(x = Date,y = Prcp, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Navrongo Rainfall", y = "Rainfall(mm)") +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmax = Navrongo$observed$tmax),
        aes(x = Date, y = Tmax)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Navrongo$RCP85[,1]), 
            Tmax = Navrongo$RCP85[ ,"Tmax"]), 
            aes(x = Date, y = Tmax, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Navrongo$RCP26[,1]),
            Tmax = Navrongo$RCP26$Tmax),
            aes(x = Date,y = Tmax, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Navrongo Maximum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmin = Navrongo$observed$tmin),
        aes(x = Date, y = Tmin)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Navrongo$RCP85[,1]), 
            Tmin = Navrongo$RCP85[ ,"Tmin"]), 
            aes(x = Date, y = Tmin, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Navrongo$RCP26[,1]),
            Tmin = Navrongo$RCP26$Tmin),
            aes(x = Date,y = Tmin, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Navrongo Minimum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    nrow = 2, ncol = 2
)
dev.copy(png, filename = "Navrongo_observed_26_85.png", width = 1500, height = 800)
dev.off()



#Tamale Prcp + RCP26
#date_4 <- c(
#    format(Bole$observed$data...1., "%Y"), 
#    Bole$RCP26$format.data....Date......Y..
#) %>% 
#    as.numeric

gridExtra::grid.arrange(
    ggplot(
        data = data.frame(Date = 1981:2016, Prcp = Tamale$observed$prcp),
        aes(x = Date, y = Prcp)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Tamale$RCP85[,1]), 
            Prcp = Tamale$RCP85[ ,"Rain"]), 
            aes(x = Date, y = Prcp, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Tamale$RCP26[,1]),
            Prcp = Tamale$RCP26$Rain),
            aes(x = Date,y = Prcp, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Tamale Rainfall", y = "Rainfall(mm)") + 
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmax = Tamale$observed$tmax),
        aes(x = Date, y = Tmax)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Tamale$RCP85[,1]), 
            Tmax = Tamale$RCP85[ ,"Tmax"]), 
            aes(x = Date, y = Tmax, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Tamale$RCP26[,1]),
            Tmax = Tamale$RCP26$Tmax),
            aes(x = Date,y = Tmax, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Tamale Maximum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmin = Tamale$observed$tmin),
        aes(x = Date, y = Tmin)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Tamale$RCP85[,1]), 
            Tmin = Tamale$RCP85[ ,"Tmin"]), 
            aes(x = Date, y = Tmin, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Tamale$RCP26[,1]),
            Tmin = Tamale$RCP26$Tmin),
            aes(x = Date,y = Tmin, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Tamale Minimum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    nrow = 2, ncol = 2
)
dev.copy(png, filename = "Tamale_observed_26_85.png", width = 1500, height = 800)
dev.off()



#Wa Prcp + RCP26
#date_4 <- c(
#    format(Bole$observed$data...1., "%Y"), 
#    Bole$RCP26$format.data....Date......Y..
#) %>% 
#    as.numeric

gridExtra::grid.arrange(
    ggplot(
        data = data.frame(Date = 1981:2016, Prcp = Wa$observed$prcp),
        aes(x = Date, y = Prcp)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Wa$RCP85[,1]), 
            Prcp = Wa$RCP85[ ,"Rain"]), 
            aes(x = Date, y = Prcp, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Wa$RCP26[,1]),
            Prcp = Wa$RCP26$Rain),
            aes(x = Date,y = Prcp, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Wa Rainfall", y = "Rainfall(mm)") +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmax = Wa$observed$tmax),
        aes(x = Date, y = Tmax)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Wa$RCP85[,1]), 
            Tmax = Wa$RCP85[ ,"Tmax"]), 
            aes(x = Date, y = Tmax, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Wa$RCP26[,1]),
            Tmax = Wa$RCP26$Tmax),
            aes(x = Date,y = Tmax, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Wa Maximum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmin = Wa$observed$tmin),
        aes(x = Date, y = Tmin)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Wa$RCP85[,1]), 
            Tmin = Wa$RCP85[ ,"Tmin"]), 
            aes(x = Date, y = Tmin, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Wa$RCP26[,1]),
            Tmin = Wa$RCP26$Tmin),
            aes(x = Date,y = Tmin, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Wa Minimum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    nrow = 2, ncol = 2
)
dev.copy(png, filename = "Wa_observed_26_85.png", width = 1500, height = 800)
dev.off()



#Yendi Prcp + RCP26
#date_4 <- c(
#    format(Bole$observed$data...1., "%Y"), 
#    Bole$RCP26$format.data....Date......Y..
#) %>% 
#    as.numeric

gridExtra::grid.arrange(
    ggplot(
        data = data.frame(Date = 1981:2016, Prcp = Yendi$observed$prcp),
        aes(x = Date, y = Prcp)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Yendi$RCP85[,1]), 
            Prcp = Yendi$RCP85[ ,"Rain"]), 
            aes(x = Date, y = Prcp, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Yendi$RCP26[,1]),
            Prcp = Yendi$RCP26$Rain),
            aes(x = Date,y = Prcp, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Yendi Rainfall", y = "Rainfall(mm)") +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmax = Yendi$observed$tmax),
        aes(x = Date, y = Tmax)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Yendi$RCP85[,1]), 
            Tmax = Yendi$RCP85[ ,"Tmax"]), 
            aes(x = Date, y = Tmax, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Yendi$RCP26[,1]),
            Tmax = Yendi$RCP26$Tmax),
            aes(x = Date,y = Tmax, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Tamale Maximum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    
    ggplot(
        data = data.frame(Date = 1981:2016, Tmin = Yendi$observed$tmin),
        aes(x = Date, y = Tmin)
    ) +
        geom_line(aes(col = "OBSERVED"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Yendi$RCP85[,1]), 
            Tmin = Yendi$RCP85[ ,"Tmin"]), 
            aes(x = Date, y = Tmin, col = "RCP85"), lwd = 1.5) +
        geom_line(data = data.frame(
            Date = as.numeric(Yendi$RCP26[,1]),
            Tmin = Yendi$RCP26$Tmin),
            aes(x = Date,y = Tmin, col = "RCP26"), lwd = 1.5) +
        geom_vline(xintercept = 2016, lwd = 6, col = "grey") +
        scale_colour_manual("",
                            values = c("OBSERVED" = "darkblue", 
                                       "RCP85" = "red",
                                       "RCP26" = "darkgreen")) + 
        labs(subtitle = "Yendi Minimum Temperature", 
             y = expression("Temperature("*degree*C*")")) +
        theme(plot.subtitle = element_text(size = 21, hjust = 0, face = "bold"),
              axis.text = element_text(size = 17),
              axis.title = element_text(size = 19),
              legend.text = element_text(size = 15)),
    nrow = 2, ncol = 2
)
dev.copy(png, filename = "Yendi_observed_26_85.png", width = 1500, height = 800)
dev.off()

