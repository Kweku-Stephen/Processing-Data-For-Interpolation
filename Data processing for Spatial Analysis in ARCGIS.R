##########################################################################################
setwd("F:\\R_PROJECTS\\North_Climate_Github")

#Required Packages
pkgs <- c(
    "parallel", "magrittr", "profmem", "rio"
)


#Checking packages availability
if(
    length(
        setdiff(pkgs, installed.packages())) > 0 
    & 
    length(
        setdiff(pkgs, installed.packages())) < 4){
    
    install.packages(
        setdiff(
            pkgs,
            dependencies = TRUE,
            installed.packages,
        )
    )
} else {
    
    sapply(
        pkgs,
        require,
        character.only = TRUE
    )
}


##Snapshot of Packages To Ease Reproducibility:This logs the packages and package versions
##used in this code
#renv::snapshot(

#)

#Loading datasets
system.time(
    data <- grep(
        "^[A-Za-z](.*)xlsx", 
        dir(), 
        value = T
    ) %>% 
        import_list() %>% 
        lapply(
            data.frame
        )
)

#system.time(
#    data <- grep("^[A-Za-z](.*)xlsx", dir(), value = T) %>% 
#    lapply(
#        import
#    ) %>% 
#    lapply(
#        data.table
#   )
#)


#Function for Data Prcessing
proc <- function(station = ""){
    
    #Data
    data <- data[[station]]
    
    #Conditional for checking class of time vector
    ifelse(
        class(
            data[, grep(
                "^([Dd][Aa][Tt][Ee]|[Yy][Ee][Aa][Rr])", 
                names(data),
                value = T
            )]
        ) != "Date"
        ,
        
        {#Printing date conversion message
            message("Converting/Creating Date Class....")
            
            #Converting to Date
            data[, grep("^([Dd][Aa][Tt][Ee]|[Yy][Ee][Aa][Rr])",names(data), value = T)] <- as.Date(
                data[ ,grep(
                    "^([Dd][Aa][Tt][Ee]|[Yy][Ee][Aa][Rr])", 
                    names(data), 
                    value = T
                )],
                format = "%Y-%m-%d %hh:%mm:%ss"
            )
        }
        ,
        
        #Print message if date class present
        print("data is with date object")
    )
    
    
    #renaming date class column
    names(data)[grep("^([Dd][Aa][Tt][Ee]|[Yy][Ee][Aa][Rr])", names(data))] <- "Date"
    
    #Subsetting observations from 1981 to 2050
    data <- data[
        data[ ,"Date"] >= "1981-01-01" & data[ ,"Date"] <= "2050-12-31", 
    ]
    
    #Averaging 
    sub_func <- function(vec = "" ){
        
        #dividing vec into halves by date and Subtracting their means of vec
        dt <- data.frame(
            Date = data[ ,"Date"],
            vec = vec
        )
        
        #subtracting mean of former time period (<= 2014-12-31) from later (>= 2015-01-01)
        mean(
            subset(dt, Date >= "2015-01-01")[,2],
            na.rm = T
        ) - 
            mean(
                subset(dt, Date <= "2014-12-31")[,2],
                na.rm = T
            ) %>% 
            return()
    }  
    
    
    #running sub_fun sequentially on an input dataframe
    sapply(
        data[ ,-grep(
            "Date", 
            names(data)
        )],
        sub_func
    )
    
}


##Parallelizing Computation##

#Detecting available Cores on PC
cores <- detectCores(logical = TRUE) #My PC runs 4 threads

#Creating a Cluster of size 4
cl <- makeCluster(
    spec = cores,
    type = "PSOCK"
)


#Loading dplyr package on all nodes
clusterEvalQ(
    cl,
    require(magrittr)
)

#Exporting Data and function to all nodes
clusterExport(
    cl,
    c("data","proc")
)

#Splitting/Chunking Tasks on worker's side
tasks <- list(
    "BoleRCP85",
    "NavrongoRCP85",
    "TamaleRCP85","WaRCP85",
    "YendiRCP85"
)

#tasks <- list(
#    "BoleRCP85",
#    "NavrongoRCP85",
#    c("TamaleRCP85","WaRCP85"),
#    "YendiRCP85"
#)

#tasks <- splitIndices(
#    length(data),
#    cores
#)
#tasks[c(1,2,4)] <- grep("^[A-Za-z](.*)xlsx", dir(),value = T)[c(1,2,5)]

#Running the function "proc" on the data in parallel
res <- clusterApply(
    cl,
    tasks,
    proc
) %>% 
    do.call(
        rbind,
        .
    )

#Renaming rows
rownames(res) <- gsub(".xlsx","",grep("^[A-Za-z](.*)xlsx", dir(), value = T))

#Stopiing Cluster to save resources
stopCluster(cl)

#Exporting to Excel
export(res, "2_RCP85_FOR_INTER.xlsx", row.names = T)











