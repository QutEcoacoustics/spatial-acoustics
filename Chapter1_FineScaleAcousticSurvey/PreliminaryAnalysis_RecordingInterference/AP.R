
#Set directory containing the files----

directory <- "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/PreliminaryAnalysis_RecordingInterferences/InspectedRecodings"

#The directOry to store the results----

base_output_directory <- "C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project/Fieldwork_Bowra/PreliminaryAnalysis_RecordingInterferences/Outputs"

#Get a list of audio files inside the directory----

files <- list.files(directory, pattern = "*.WAV", full.names = TRUE, recursive = TRUE)

#selecting a subset of data for testing
#files <- files[1:1]

#iterate through each file----

for (file in files) {
  message("Processing", file)
  
  #get the name and directory of the file
  
  output_fragment <- gsub(x = file, pattern = directory, replacement = "")
  
#output_fragment <- gsub(x = output_fragment, pattern = "__", replacement = "_")
  
  #make a folder for the results
  
  output_directory <- (file.path(base_output_directory, output_fragment))
  dir.create(output_directory, recursive = TRUE)
  
  #prepare command
  
  command <- sprintf('audio2csv "%s" "C:/AP/ConfigFiles/Towsey.Acoustic.yml" "%s" -p ', file, output_directory)
  
  #finally, execute the command
  shell(paste('C:/AP/AnalysisPrograms.exe', command))


}
