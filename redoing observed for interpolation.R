####################################################################################
#Loading in all datasets in the working directory satisfying the expresion "_1.csv"
observed <- dir(pattern = "_1.csv") %>% 
    rio::import_list()


imput <- function(data = ""){
    suppressMessages(require(imputeTS))
    
    data[ -1] <- replace(data[ -1], data[ -1] == 0.0, NA)
    apply(data[ -1], 2, na_kalman, model = "StructTS", smooth = TRUE, nit = -1) %>% 
        data.frame(data[ ,1], .)

}

#Filling missing values on the observed data
observed_1 <- lapply(observed, imput)

#Applying the seam funcction defined in the script "Comparing means of observed and projected"
Stations_observed <- seam(data = observed_1$tm_1, station = "Tamale") %>% 
    rbind(. ,seam(data = observed_1$bo_1, station = "Bole")) %>% 
    rbind(. ,seam(data = observed_1$wa_1, station = "Wa")) %>% 
    rbind(. ,seam(data = observed_1$da_1, station = "Damongo")) %>% 
    rbind(. ,data.frame(seam(data = observed_1$wale_1, station = "Walewale"), tmax = NA, tmin = NA)) %>% 
    rbind(. ,seam(data = observed_1$ye_1, station = "Yendi")) %>% 
    rbind(. ,seam(data = observed_1$ba_1, station = "Babile")) %>% 
    rbind(. ,seam(data = observed_1$na_1, station = "Navrongo")) %>% 
    rbind(. ,seam(data = observed_1$ve_1, station = "Vea")) %>% 
    rbind(. ,seam(data = observed_1$zu_1, station = "Zuarungu")) %>% 
    rbind(. ,seam(data = observed_1$bg_1, station = "Bolgatanga"))


#Adding geographic coordinates the the dataframe "Stations_observed" above
Stations_observed <- dplyr::mutate(
    Stations_observed, 
    lon = c(-0.83806,-2.4863,-2.50448,-1.82111,-0.79767, 
            -0.01094, -2.83525, -1.09005,-0.83987,-0.80919,-0.86331),
    lat = c(9.40499,9.03306,10.06622,9.13921,10.36501,
            9.48360,10.53002,10.92792,11.01255,10.82845,10.85145)
)
