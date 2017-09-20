rankhospital  <- function(state, outcome, num = "best")
{
    #Read outcome
    setwd( "/Users/gabrielsoares/Documents/Curso Data Analysis/rprog-data-ProgAssignment3-data")
    hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #state list 
    state_list <- unique(hospital_data[,7])
    
    #outcome list
    outcome_list  <- c("heart attack","heart failure", "pneumonia")
    
    #checking variables
    #first, state
    if (!state %in% state_list){
        stop(print("invalid state"))
    }
    #second, outcome
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
    
    #converting to a numeric to order
    desired_hospital_filtered[,outcome_column]  <- sapply(desired_hospital_filtered[,outcome_column], as.numeric)
    
    #find the hospitals in the rows with the minimum outcome value
    columns_considered <- as.numeric(desired_hospital_filtered[, outcome_column])
    columns_names  <- as.character(desired_hospital_filtered[, 2])
    
    desired_hospitals_ordered  <- desired_hospital_filtered[with(desired_hospital_filtered,order(desired_hospital_filtered[outcome_column],desired_hospital_filtered[2])),]
    
    #if there are multiple hospitals with the minimum outcome value, then
    #return the first hospital name from the alphabetically sorted hospital
    #names list
    if (num == "best") {
        desired_hospitals_ordered[1,2]
    }
    else if (num == "worst") {
        desired_hospitals_ordered[length(desired_hospitals_ordered[,outcome_column]),2]
    }
    else {
        desired_hospitals_ordered[num,2]
    } 
    
}