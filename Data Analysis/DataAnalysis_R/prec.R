# Zeynep G. Özkan, Ayşe C. Şimşek, Tolgahan Aydın, 2022

## This function is coded to make the data received by PsychoPy usable and 
## to parse only the video memory related parts from it.

preproc <- function(data_dir = "Results"){
  library(readr)
  library(stringr)
  get_files<- function(dir= "", file_ext= ".csv"){
    
    if(dir== ""){
      dir= getwd()
    }
    
    # get a list of all file in dir:
    all_files<- list.files(dir)
    # remove non-asc files (if present)
    all_files<- all_files[grepl(file_ext, all_files)]
    
    # sort files by number in string
    get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
    num<- get_num(all_files) #change
    
    if(!is.na(num[1])){
      all_files<- all_files[order(num, all_files)]
    }
    # convert to directory string for each data file:
    if(length(all_files)>0){
      all_dirs<- NULL
      for(i in 1:length(all_files)){
        all_dirs[i]<- paste(dir, "/", all_files[i], sep = "")
      }
      
      message(paste("Found", toString(length(all_files)), file_ext, "files in the specified directory!", "\n"))
      return(all_dirs)
    }else{
      stop(paste("Found no", file_ext, "files in the specified directory!"))
    }
  } # end of get_files()
  data = NULL
  dataALL<- get_files(data_dir)
  for(i in 1:length(dataALL)){
    data1 <- read_csv(dataALL[i])
    data1 <- data1[ , -which(names(data1) %in% c("instr_Text_key.keys","instr_Text_key.rt",
                                                 "mouse.x","mouse.y","mouse.leftButton","mouse.midButton","mouse.rightButton","shots",
                                                 "mouse.time","mouse.clicked_name","trials.thisRepN","trials.thisTrialN","trials.ran","trials.thisN",
                                                 "date","expName","psychopyVersion","OS","frameRate",
                                                 "trials_3.thisRepN","trials_3.thisN","trials_3.thisIndex","trials_3.ran","trials_3.thisTrialN",
                                                 "trials_2.thisRepN","trials_2.thisN","trials_2.thisIndex","trials_2.ran","trials_2.thisTrialN", "Okul Numaranız*",
                                                 "Ders kodu / ders adı(Deneye hangi dersten ulaştınız? *"))]
    data2 <- subset(data1,!is.na(data1$movie_resp.corr))
    data2 <- data2[ , -which(names(data2) %in% c("corrAnsOM","Target Position","Distractor Position","OM_key.keys",
                                                 "OM_key.corr","OM_key.rt","target_image","distract_image","targetX",
                                                 "distractX","corrAns","scenes","videolocation","end_first_part_key.keys","end_first_part_key.rt",
                                                 "textboX.text","mouse_2.x","mouse_2.y","mouse_2.leftButton","mouse_2.midButton",
                                                 "mouse_2.rightButton","mouse_2.time","mouse_2.clicked_name","trials_4.thisRepN",
                                                 "trials_4.thisTrialN","trials_4.thisN","trials_4.thisIndex","trials_4.ran", "Okul Numaranız*",
                                                 "Ders kodu / ders adı(Deneye hangi dersten ulaştınız? *"
                                                ))]
    colnames(data2)[16] <- "Participant"
    
    data2$Participant <- i

    colnames(data2)[15] <- "ColorBlind"
    colnames(data2)[14] <- "Glasses"
    colnames(data2)[13] <- "Education"
    colnames(data2)[12] <- "Gender"
    colnames(data2)[11] <- "Age"
    
    for(j in 1:nrow(data2)){
      if(data2$Perspective[j] == "first person"){
        data2$Perspective[j] <- "Single"
      }else{
        data2$Perspective[j] <- "Shot-Reverse-Shot"
      }
    }

    
      
    data <- rbind(data2, data)
  }#end of for
  return(data)
}# end of prec
  