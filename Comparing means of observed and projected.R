###################################################################################
#Differneces between Observed (1980-2016) and Projected (2021-2050)

#Function to compute the sum of Rainfall and mean of Temperature as a function of Years
sum_mean_work <- function(data = ""){
    data.frame(
        aggregate(
            Rain ~ format(data[,grep("^([Dd][Aa][Tt][Ee]|[Yy][Ee][Aa][Rr])", names(data), value = T)], "%Y"),
            data = data, 
            FUN =  sum, 
            na.rm = T
        ),
        aggregate(
            cbind(Tmax,Tmin) ~ format(data[,grep("^([Dd][Aa][Tt][Ee]|[Yy][Ee][Aa][Rr])", names(data), value = T)], "%Y"),
            data = data, 
            FUN =  mean, 
            na.rm = T
        )[,-1]
        
    )
    
} 


#Evaluating the function sum_mean_work on RCP 85 and 26 projected data which has NA's
#(All missing values and 0s are not filled)
#RCP_26
data_26_sub <- lapply(
    data_26, 
    function(data = "") subset(data, Date >= "2021-01-01" & Date <= "2050-12-31")
) %>% 
    lapply(sum_mean_work)


#RCP_85
data_85_sub <- lapply(
    data, 
    function(data = "") subset(data, Year >= "2021-01-01" & Year <= "2050-12-31")
) %>% 
    lapply(sum_mean_work)


#Joining all Stations into a Singularity
#sum-mean stats function for all variables 
seam <- function(data = "", station = ""){
    require(magrittr)
    apply(
        data[-1], 
        2, 
        function(x) {
            #ifelse(
            #    min(x, na.rm = T) <= 5, 
            #    sum(x, na.rm = T), 
            mean(x, na.rm = T)
            #)
        }
    ) %>% 
        t() %>% 
        data.frame(station, .)
}


#Implemetation (Evaluating seam on all appropriate columns of datasets in the lists: data_26 and data_85)
#RCP26
Stations_26 <- seam(data = data_26_sub$BoleRCP26, station = "Bole") %>% 
    rbind(. ,seam(data = data_26_sub$NavrongoRCP26, station = "Navrongo")) %>% 
    rbind(. ,seam(data = data_26_sub$TamaleRCP26, station = "Tamale")) %>% 
    rbind(. ,seam(data = data_26_sub$WaRCP26, station = "Wa")) %>% 
    rbind(. ,seam(data = data_26_sub$YendiRCP26, station = "Yendi"))

#RCP85
Stations_85 <- seam(data = data_85_sub$BoleRCP85, station = "Bole") %>% 
    rbind(. ,seam(data = data_85_sub$NavrongoRCP85, station = "Navrongo")) %>% 
    rbind(. ,seam(data = data_85_sub$TamaleRCP85, station = "Tamale")) %>% 
    rbind(. ,seam(data = data_85_sub$WaRCP85, station = "Wa")) %>% 
    rbind(. ,seam(data = data_85_sub$YendiRCP85, station = "Yendi"))




