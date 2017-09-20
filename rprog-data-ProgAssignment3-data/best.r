best  <- function(state, outcome){
    #Read outcome
    setwd( "/Users/gabrielsoares/Documents/Curso Data Analysis/rprog-data-ProgAssignment3-data")
    hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #state list 
    state_list <- uninque(hospital_data[,7])
    
    #outcome list
    outcome_list  <- c("heart attack","heart failure", "pneumonia")
    
    #checking variables
    if (!state %in% state_list){
        stop(print("invalid state"))
    }
    else if (!outcome %in% outcome_list){
        stop(print("invalid outcome"))
    }
    
    #filtering hospital dataset by state name
    hospital_filtered  <- subset(hospital_data, State == state)  
    
    if (outcome == "heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else if (outcome == "pneumonia") {
        outcome_column <- 23
    }
    
    #taking out NA values for the column
    column_desired  <- as.numeric(hospital_filtered[, outcome_column])
    is_Na <- is.na(column_desired)
    desired_hospital_filtered <- hospital_filtered[!is_Na,]
    
    #find the hospitals in the rows with the minimum outcome value
    columns_considered <- as.numeric(desired_hospital_filtered[, outcome_column])
    desired_rows <- which(columns_considered == min(columns_considered))
    desired_hospitals <- desired_hospital_filtered[desired_rows, 2]
    
    #if there are multiple hospitals with the minimum outcome value, then
    #return the first hospital name from the alphabetically sorted hospital
    #names list
    if (length(desired_hospitals) > 1) {
        hospitals_sorted <- sort(desired_hospitals)
        hospitals_sorted[1]
    }
    else {
        desired_hospitals
    }
    
}