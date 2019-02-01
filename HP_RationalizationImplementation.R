#Load necessary packages
library(readxl)
library(dplyr)
library(writexl)
library(taRifx)

#Set working directory
setwd("/Users/shwetachopra/Desktop/Rationalization")

#Load overall data for HP Schools, initialize new columns to be created
hp_all <- read_excel("All_Schools_File.xlsx", sheet="Chamba Updated")
hp_all$Required_Teachers <- 0
hp_all$Difference <- 0
hp_all$Category <- 0
hp_all$Single_Teacher <- 0
hp_all$Rank <- 0
hp_all <- hp_all[complete.cases(hp_all),]

#For reach row Calculate Teacher Requirement based on PTR norms, Calculate Surplus/Deficit, assign a Category, Replace Medium of Instruction

#Calculating Teacher Requirement
for (i in 1:nrow(hp_all)) {
  
  if (hp_all[i,]$Total_Enrollment <= 60) {
    hp_all[i,]$Required_Teachers <- 2
  }
  
  else if (hp_all[i,]$Total_Enrollment <= 90) {
    hp_all[i,]$Required_Teachers <- 3
  }
  
  else if (hp_all[i,]$Total_Enrollment <= 120) {
    hp_all[i,]$Required_Teachers <- 4
  }
  
  else if (hp_all[i,]$Total_Enrollment <= 150) {
    hp_all[i,]$Required_Teachers <- 5
  }
  
  else if (hp_all[i,]$Total_Enrollment <= 200) {
    hp_all[i,]$Required_Teachers <- 6
  }
  
  else if (hp_all[i,]$Total_Enrollment >= 201) {
    hp_all[i,]$Required_Teachers <- round((hp_all[i,]$Total_Enrollment)/40) + 1
  }
  
  #Calculate Difference
  hp_all[i,]$Difference <- hp_all[i,]$Current_Teachers - hp_all[i,]$Required_Teachers
  
  #Assign Category
  if (hp_all[i,]$Difference == 0) {
    hp_all[i,]$Category <- "Sufficient"
  }
  
  else if (hp_all[i,]$Difference < 0) {
    hp_all[i,]$Category <- "Deficit"
  }
  
  else if (hp_all[i,]$Difference > 0) {
    hp_all[i,]$Category <- "Surplus"
  }
  
  #Replace Medium of Instruction
  if (hp_all[i,]$Medium == 19 || hp_all[i,]$Med2 == 19 || hp_all[i,]$Med3 == 19 || hp_all[i,]$Med4 == 19){
    hp_all[i,]$Medium <- "English"
  }
  
  else {
    hp_all[i,]$Medium <- "Hindi"
  }
  
  #Add column for Single Teacher schools
    if (hp_all[i,]$`Current_Teachers` == 1)        #why inverted commas
    {
      hp_all[i,]$Single_Teacher = "Yes"
    } else {
      hp_all[i,]$Single_Teacher = "No"
    }
    
i=i+1
  
}


hp_all <- hp_all[,-7]
hp_all <- hp_all[,-7]
hp_all <- hp_all[,-7]


#Create subset tables for Surplus and Deficit Schools
hp_surplus <- filter(hp_all, Category == "Surplus")
hp_deficit <- filter(hp_all, Category == "Deficit")
hp_sufficient <- filter(hp_all, Category == "Sufficient")

#Export hp_all as pre-rationalization overall picture
hp_all$PTR <- 0
hp_all$PTR <- (hp_all$Total_Enrollment)/(hp_all$Current_Teachers)
View(hp_all)
write_xlsx(hp_all, "HP_Initial.xlsx", col_names=TRUE)

#Order Surplus data by column - Cluster, Descending order of Difference, Enrolment Size
hp_surplus <- arrange(hp_surplus, desc(Difference), Total_Enrollment, UDISE)

#Convert table to data frame
hp_surplus <- as.data.frame(hp_surplus)

#Assign rank = row number in a new column "Rank"
i=0
for (i in 1:nrow(hp_surplus)) {
  hp_surplus[i,]$Rank <- i
  i=i+1
}

#Arrange by Block and Cluster
hp_surplus <- arrange(hp_surplus, Block, Cluster)

#Create output table - hp_surplus
write_xlsx(hp_surplus, "Ranked_Surplus.xlsx", col_names = TRUE)


#Order Deficit data by column Single Teacher, then Deficit Size, then Enrolment Size, then UDISE
hp_deficit <- arrange(hp_deficit, desc(Single_Teacher), Difference, desc(Total_Enrollment), UDISE)

#Assign rank = row number in a new column
i=0
for (i in 1:nrow(hp_deficit)) {
  hp_deficit[i,]$Rank <- i
  i=i+1
}

#Arrange by Block and Cluster
hp_deficit <- arrange(hp_deficit, Block, Cluster)

#Create output table - hp_deficit
write_xlsx(hp_deficit, "Ranked_Deficit.xlsx", col_names = TRUE)


#BEGIN IMPLEMENTATION

#Order both files by Rank
hp_surplus <- arrange(hp_surplus, Rank)
hp_deficit <- arrange(hp_deficit, Rank)

#Create a Pairing table to hold results of Rationalization
Pair_Table <- 0
new_sufficient_schools <- 0

#Carry out Cluster level Rationalization

#Pick row 1 from Surplus Schools
for (i in 1:nrow(hp_surplus)) {
  for (j in 1:nrow(hp_deficit)) {
    #If Cluster Matches then reduce Difference for Surplus school by 1, increase Difference for Deficit school by 1
    if (identical(hp_surplus[i,]$Cluster,hp_deficit[j,]$Cluster) && identical(hp_surplus[i,]$Medium,hp_deficit[j,]$Medium)) {
      hp_surplus[i,]$Current_Teachers <- hp_surplus[i,]$Current_Teachers - 1
      hp_deficit[j,]$Current_Teachers <- hp_deficit[j,]$Current_Teachers + 1
      
      hp_surplus[i,]$Difference <- hp_surplus[i,]$Difference - 1
      hp_deficit[j,]$Difference <- hp_deficit[j,]$Difference + 1
      
      #Edit Single Teacher details
      if (hp_deficit[j,]$Current_Teachers != 1) {
        hp_deficit[j,]$Single_Teacher <- "No"
      }
      
      #Enter From and To details in Pairing Table
      pairing <- 0
      pairing <- c(hp_surplus[i,]$District, hp_surplus[i,]$Block, hp_surplus[i,]$Cluster, hp_surplus[i,]$UDISE, hp_surplus[i,]$School_Name, hp_deficit[j,]$District, hp_deficit[j,]$Block, hp_deficit[j,]$Cluster, hp_deficit[j,]$UDISE, hp_deficit[j,]$School_Name, hp_surplus[i,]$Medium, hp_surplus[i,]$Cycle, "Cluster")
      Pair_Table <- rbind(pairing, Pair_Table)
      
      #If Deficit School difference = 0, then change Category to Sufficient
      if (identical(hp_deficit[j,]$Difference,0)) {
        hp_deficit[j,]$Category <- "Sufficient"
      }
      
      #If Surplus School difference = 0, then change Category to Sufficient
      if (identical(hp_surplus[i,]$Difference,0)) {
        hp_surplus[i,]$Category <- "Sufficient"
      }
      
      #Add Schools with Category = Sufficient to new data frame - Newly Sufficient Schools
      
      #Deficit schools
      sufficient_deficit <- 0
      if (hp_deficit[j,]$Category == "Sufficient") {
        sufficient_deficit <- c(hp_deficit[j,]$District, hp_deficit[j,]$Block, hp_deficit[j,]$Cluster, hp_deficit[j,]$UDISE, hp_deficit[j,]$School_Name, hp_deficit[j,]$Medium, hp_deficit[j,]$Total_Enrollment, hp_deficit[j,]$Current_Teachers, hp_deficit[j,]$Cycle, hp_deficit[j,]$Required_Teachers, hp_deficit[j,]$Difference, hp_deficit[j,]$Category, hp_deficit[j,]$Single_Teacher, hp_deficit[j,]$Rank, "Deficit", "Cluster")
        new_sufficient_schools <- rbind(sufficient_deficit, new_sufficient_schools)
        hp_sufficient <- rbind(sufficient_deficit, hp_sufficient)
        #remove Sufficient schools from current files
        hp_deficit <- hp_deficit[-j,]
      }
      
      #Re-ranking hp_deficit
      hp_deficit <- arrange(hp_deficit, desc(Single_Teacher), Difference, desc(Total_Enrollment), UDISE)
      r=0
      for (r in 1:nrow(hp_deficit)) {
        hp_deficit[r,]$Rank <- r
        r=r+1
      }
      
      hp_deficit <- arrange(hp_deficit, Rank)
      
      #Surplus schools
      sufficient_surplus <- 0
      if (hp_surplus[i,]$Category == "Sufficient") {
        sufficient_surplus <- c(hp_surplus[i,]$District,hp_surplus[i,]$Block,hp_surplus[i,]$Cluster,hp_surplus[i,]$UDISE, hp_surplus[i,]$School_Name, hp_surplus[i,]$Medium, hp_surplus[i,]$Total_Enrollment, hp_surplus[i,]$Current_Teachers, hp_surplus[i,]$Cycle, hp_surplus[i,]$Required_Teachers,hp_surplus[i,]$Difference,hp_surplus[i,]$Category, hp_surplus[i,]$Single_Teacher, hp_surplus[i,]$Rank, "Surplus", "Cluster")
        new_sufficient_schools <- rbind(sufficient_surplus, new_sufficient_schools)
        hp_sufficient <- rbind(sufficient_surplus, hp_sufficient)
        #remove Sufficient schools from current files
        hp_surplus <- hp_surplus[-i,]
        
        break
      }
      
    }
    j=j+1
  }
  i=i+1
}



#Re-ranking hp_deficit
hp_deficit <- arrange(hp_deficit, desc(Single_Teacher), Difference, desc(Total_Enrollment), UDISE)
r=0
for (r in 1:nrow(hp_deficit)) {
  hp_deficit[r,]$Rank <- r
  r=r+1
}

#Order both files by Rank
hp_surplus <- arrange(hp_surplus, Rank)
hp_deficit <- arrange(hp_deficit, Rank)

#Carry out Block level Rationalization

#Pick row 1 from Surplus Schools
for (i in 1:nrow(hp_surplus)) {
  for (j in 1:nrow(hp_deficit)) {
    #If Block Matches then reduce Difference for Surplus school by 1, increase Difference for Deficit school by 1
    if (identical(hp_surplus[i,]$Block,hp_deficit[j,]$Block) && identical(hp_surplus[i,]$Medium,hp_deficit[j,]$Medium) && identical(hp_surplus[i,]$Cycle,hp_deficit[j,]$Cycle)) {
      hp_surplus[i,]$Current_Teachers <- hp_surplus[i,]$Current_Teachers - 1
      hp_deficit[j,]$Current_Teachers <- hp_deficit[j,]$Current_Teachers + 1
      
      hp_surplus[i,]$Difference <- hp_surplus[i,]$Difference - 1
      hp_deficit[j,]$Difference <- hp_deficit[j,]$Difference + 1
      
      #Edit Single Teacher details
      if (hp_deficit[j,]$Current_Teachers != 1) {
        hp_deficit[j,]$Single_Teacher <- "No"
      }
      
      #Enter From and To details in Pairing Table
      pairing <- 0
      pairing <- c(hp_surplus[i,]$District, hp_surplus[i,]$Block, hp_surplus[i,]$Cluster, hp_surplus[i,]$UDISE, hp_surplus[i,]$School_Name, hp_deficit[j,]$District, hp_deficit[j,]$Block, hp_deficit[j,]$Cluster, hp_deficit[j,]$UDISE, hp_deficit[j,]$School_Name, hp_surplus[i,]$Medium, hp_surplus[i,]$Cycle, "Block")
      Pair_Table <- rbind(pairing, Pair_Table)
      
      #If Deficit School difference = 0, then change Category to Sufficient
      if (identical(hp_deficit[j,]$Difference,0)) {
        hp_deficit[j,]$Category <- "Sufficient"
      }
      
      #If Surplus School difference = 0, then change Category to Sufficient
      if (identical(hp_surplus[i,]$Difference,0)) {
        hp_surplus[i,]$Category <- "Sufficient"
      }
      
      #Add Schools with Category = Sufficient to new data frame - Newly Sufficient Schools
      
      #Deficit schools
      sufficient_deficit <- 0
      if (hp_deficit[j,]$Category == "Sufficient") {
        sufficient_deficit <- c(hp_deficit[j,]$District, hp_deficit[j,]$Block, hp_deficit[j,]$Cluster, hp_deficit[j,]$UDISE, hp_deficit[j,]$School_Name, hp_deficit[j,]$Medium, hp_deficit[j,]$Total_Enrollment, hp_deficit[j,]$Current_Teachers, hp_deficit[j,]$Cycle, hp_deficit[j,]$Required_Teachers, hp_deficit[j,]$Difference, hp_deficit[j,]$Category, hp_deficit[j,]$Single_Teacher, hp_deficit[j,]$Rank, "Deficit", "Block")
        new_sufficient_schools <- rbind(sufficient_deficit, new_sufficient_schools)
        hp_sufficient <- rbind(sufficient_deficit, hp_sufficient)
        #remove Sufficient schools from current files
        hp_deficit <- hp_deficit[-j,]
      }
      
      #Re-ranking hp_deficit
      hp_deficit <- arrange(hp_deficit, desc(Single_Teacher), Difference, desc(Total_Enrollment), UDISE)
      r=0
      for (r in 1:nrow(hp_deficit)) {
        hp_deficit[r,]$Rank <- r
        r=r+1
      }
      
      hp_deficit <- arrange(hp_deficit, Rank)
      
      #Surplus schools
      sufficient_surplus <- 0
      if (hp_surplus[i,]$Category == "Sufficient") {
        sufficient_surplus <- c(hp_surplus[i,]$District,hp_surplus[i,]$Block,hp_surplus[i,]$Cluster,hp_surplus[i,]$UDISE, hp_surplus[i,]$School_Name, hp_surplus[i,]$Medium, hp_surplus[i,]$Total_Enrollment, hp_surplus[i,]$Current_Teachers, hp_surplus[i,]$Cycle, hp_surplus[i,]$Required_Teachers,hp_surplus[i,]$Difference,hp_surplus[i,]$Category, hp_surplus[i,]$Single_Teacher, hp_surplus[i,]$Rank, "Surplus", "Block")
        new_sufficient_schools <- rbind(sufficient_surplus, new_sufficient_schools)
        hp_sufficient <- rbind(sufficient_surplus, hp_sufficient)
        #remove Sufficient schools from current files
        hp_surplus <- hp_surplus[-i,]
        
        break
      }
      
    }
    j=j+1
  }
  i=i+1
}

  #Convert Pair_Table and new_sufficient schools to data frame
  Pair_Table <- as.data.frame(Pair_Table)
  new_sufficient_schools <- as.data.frame(new_sufficient_schools)
  
  
  #Remove the initialization row from the above
  y <- nrow(Pair_Table)
  Pair_Table <- Pair_Table[-y,]
  
  x <- nrow(new_sufficient_schools)
  new_sufficient_schools <- new_sufficient_schools[-x,]

  #Add column names to new_sufficient_schools and Pairings
  colnames(Pair_Table) <- c("From_District", "From_Block", "From_Cluster", "From_UDISE", "From_School", "To_District", "To_Block", "To_Cluster", "To_UDISE", "To_School", "Medium", "Cycle", "Rationalized_At" )
  colnames(new_sufficient_schools) <- c("District", "Block", "Cluster", "UDISE", "School_Name", "Medium", "Total_Enrollment", "Current_Teachers", "Cycle", "Required_Teachers", "Difference", "Category", "Single_Teacher", "Rank", "Prior_Condition", "Rationalized_At")
  
  
  #Export output for new_sufficient_schools and Pairings
    write_xlsx(Pair_Table, "Pairing_Sheet.xlsx")
    write_xlsx(new_sufficient_schools, "New_Sufficient_Schools.xlsx")

  #Create Final total schools list
    hp_final <- 0
    hp_final <- rbind(hp_deficit,hp_surplus,hp_sufficient)
    hp_final$PTR <- 0
    hp_final$Total_Enrollment <- as.numeric(hp_final$Total_Enrollment)
    hp_final$Current_Teachers <- as.numeric(hp_final$Current_Teachers)
    hp_final$PTR <- (hp_final$Total_Enrollment)/(hp_final$Current_Teachers)
    write_xlsx(hp_final, "HP_Final.xlsx")

 

