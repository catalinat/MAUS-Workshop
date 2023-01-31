#Own stop func to avoid printing stacktrace
stop_and_exit <- function(x){
  cat(paste0(x,"\n"), file=stderr())
  q(status = 1)
}
#Check for packages
this_script_needs = c("wrassp", "emuR", "ggplot2", "dplyr", "tidyr", "argparse", "gridExtra", "docopt")
packages_needed = this_script_needs[!(this_script_needs %in% installed.packages()[,"Package"])]
if(length(packages_needed)){
  stop_and_exit(sprintf("ERROR: FormantAnalysis : Missing required packages: %s.", paste(packages_needed, collapse = ",")))
}
#Load packages without warnings
load_quiet <- function(lib){
  suppressPackageStartupMessages(library(lib, character.only = TRUE))
}
for(i in this_script_needs){
  lapply(i, load_quiet)
} 

#Avoid warnings from imported packages
options(dplyr.summarise.inform = FALSE)
options(warn=-1)


output_func <- function(text, mode, verbosityThresh=1){
  if (mode == "debug"){
    if(args$verbosity>=verbosityThresh){
      print(sprintf("DEBUG: FormantAnalysis : %s", text))
    }
  } else if (mode == "warning"){
    write(sprintf("WARNING: FormantAnalysis : %s", text), stderr())
  } else if(mode == "error"){
    stop_and_exit(sprintf("ERROR: FormantAnalysis : %s", text))
  }
}

#############ARGUMENTS####################
'Formant_analysis

Usage:
  Formant_analysis.R -i <PATH> -o <PATH> (-s <SOUNDS> | -e <EXPR>) -g <GEN> [options]
  
Options:
  -i <PATH>, --input <PATH>         emuDB input folder location
  -o <PATH>, --output <PATH>        Output folder to write the data to (will be created, if not exits). Will create subdirectories for plots and csv
  -s <SOUNDS>, --sounds <SOUNDS>    Phonemes to be analysed in SAMPA or IPA separated by ",". NOTE: Whole sequence has to be escaped in "" quotations. Example: "A,ŋ,ɬ,o"
  -e <EXPR>, --emuExpr <EXPR>       EmuQueryLanguage expression for the desired phonemes to be analysed. NOTE: Has to be escaped in "" quotations. Example: "[MAU==A|V ^ #Syllable =~ .*]"
  -g <GEN>, --gender <GEN>          Gender of the data. If set to "unknown"|"u" supplied gender will be inferred throught the average F0 values found in the data. [default: "u"]
  --outlierMetric <METRIC>          Distance metric in the F1/F2 Space to use for determining outliers. Currently supports Euclid|Mahalanobis. [default: "euclid"]
  --outlierThreshold <THRESH>       Threshold for determining outliers. Everything further away from the respective vowel mean in the provided "--outlierMetric" gets classified as outlier. [default: 250]
  --midpoint <BOOL>                 Boolean whether the absolute midpoint or the mid 50% median will be used in computations. [default: false]
  --eRatio <BOOL>                   Boolean whether the eRatio for all possible phoneme triples is computed. [default: false]
  --saveTracks <BOOL>               Boolean whether the program should try to remove existing formant tracks and save the computations in the emuDB. NOTE: This will delete existing formant Data so be sure to check twice! [default: false]
  -v <LEVEL>, --verbosity <LEVEL>   Verbosity Level [default: 0]
  -h, --help                        Show this screen.
  --version                         Show version.
' -> arg_definition

tryCatch({
  args <- docopt(arg_definition, version='1.2')
},
error=function(e){
  x= paste0("ERROR: FormantAnalysis : Wrong Arguments \n", substring(e, 25))
  stop_and_exit(sprintf("%s", x))
})

args$outlierThreshold = as.numeric(args$outlierThreshold)
args$verbosity = as.numeric(args$verbosity)

args$midpoint = as.logical(args$midpoint)
args$eRatio = as.logical(args$eRatio)
args$saveTracks = as.logical(args$saveTracks)
tryCatch({
  if(args$midpoint == TRUE){
    args$midpoint = args$midpoint #Do nothing. Just checking for type
  }
  if(args$eRatio == TRUE){
    args$eRatio = args$eRatio #Do nothing. Just checking for type
  }
  if(args$saveTracks == TRUE){
    args$saveTracks = args$saveTracks #Do nothing. Just checking for type
  }
},error=function(e){
  output_func("Could not convert arguments to valid logical vectors. Please provide a boolean value for --midpoint|--eRatio|--saveTracks." ,"error") 
})

###########LOAD DB#############
db_path = normalizePath(args$input)

if(!is.null(args$sounds)){
  args$sounds = gsub("ː" , ":", args$sounds) #Take out long symbol ː for : so easier to process
  emuR_operands = c("#", ",", "=", "!=", "=~", "!~", ">", ">=", "<", "<=", "|", "&", "^", "->")
  sound_list = as.character(unlist(strsplit(args$sounds, split=","))) #Convert to array
  if(any(sound_list %in% emuR_operands)){ #Put potential emu Operands in '' so the query engine takes them
    sound_list[sound_list %in% emuR_operands] = paste0("'",sound_list[sound_list %in% emuR_operands],"'")
    }
  } else if(!is.null(args$emuExpr)){
    #Shouldn't matter. If $emuExpr sound_list should not be used
  sound_list = c("i", "'&'", "y","e","2","E", "9", "{", "a", "1", "}", "@\\", "@","8","3","3\\","6","M","u","7","o","V","O","A","Q","I","Y","U","i:", "'&:'", "y:", "e:", "2:", "E:", "9:", "{:", "a:", "1:", "}:", "@\\:", "@:","8:","3:","3\\:","6:","M:","u:","7:","o:","V:","O:","A:","Q:","I:","Y:","U:") 
} else{
  output_func("Please supply a valid collection of phonemes to be parsed or a valid emu query.", "error")
}

#DB = load_emuDB("./FORMANTANALYSISOUTPUT_emuDB/", verbose = FALSE)


tryCatch({
  DB = load_emuDB(db_path, verbose = FALSE)
},
error=function(e){
  message = sprintf("An error occured while loading the emuDB: %s", e)
  output_func(message, "error")
})



#####VOWEL LIST QUERY AUS IPATABLE.JS#######
#all_vowels_sampa = c("i", "y","e","2","E", "9", "{", "a", "1", "}", "@\\", "@","8","3","3\\","6","M","u","7","o","V","O","A","Q","I","Y","U")
#all_vowels_ipa = c("i","y","e","ø","ɛ","œ","æ","a","ɶ","ɨ","ʉ","ɘ","ə","ɵ","ɜ","ɞ","ɐ","ɯ","u","ɤ","o","ʌ","ɔ","ɑ","ɒ","ɪ","ʏ","ʊ")

#######FORAMNT VLAUES GERMAN DRAXLER######
# front_order = c('a:', 'E', 'e', 'E:', 'i', 'Y', 'y')
# back_order = c('A', 'O', 'o', 'U', 'V', 'u')
# front_mean_F3 = c(2533, 2633, 2704, 2660, 2698, 2350, 2292)
# back_mean_F3 = c(2540, 2505, 2497, 2424, 2249, 2416)
# front_mean_F2 = c(1339, 1784, 2076, 2022, 2071, 1541, 1588)
# back_mean_F2 = c(1362, 1162, 927, 1093, 1464, 946)
# front_mean_F1 = c(691, 486, 383, 443, 324, 383, 337)
# back_mean_F1 = c(673, 529, 416, 413, 395, 328)

#########QUERIES###############
if(!is.null(args$emuExpr)){
  tryCatch({
    all_sounds_query = query(DB, query = args$emuExpr)
    },
    error=function(e){
      message = sprintf("An error occured while processing the emuQuery: %s", e)
      output_func(message, "error")
    }
  )
} else {
  all_sounds_query = query(DB, query = sprintf("[MAU == %s]",paste(sound_list, collapse= "|")))
}

#all_vowels_query_start = query(DB, query = sprintf("[Phonetic == %s && [Start(word, Phonetic) == TRUE]]",paste(all_vowels_sampa, collapse= "|")))
#all_vowels_query_start = query(DB, query = "[Phonetic == @ && [start(Word, Phonetic) == TRUE]]")

if(plyr::empty(all_sounds_query)){
  if(!is.null(args$emuExpr)){
    output_func("Your EMU query expression returned no elements.", "error")
  } else if(!is.null(args$sounds)){
    output_func("The provided phoneme(s) do not exist in your database", "error")
  } else{
    output_func("The provided database contains no vowels.", "error") 
  }
} else{
  for(queried_phoneme in sound_list){
    if(!queried_phoneme %in% all_sounds_query$labels){
      output_func(sprintf("One of the phonemes you queried does not exist in the database: %s", queried_phoneme), "warning")
    }
  }
}

################Gender Detect#############
if(!is.null(args$gender)){
  if(is.element(tolower(args$gender), c('male', 'm'))){
    gender = "m"
  } else if(is.element(tolower(args$gender), c('female', 'f'))){
    gender = "f"
  } else if(is.element(tolower(args$gender),c('unknown','u'))){
    
    output_func("Unknown gender specified. Starting gender detection.", "debug", 1)
    
    ksv_track = get_trackdata(
      emuDBhandle = DB,
      seglist = all_sounds_query,
      onTheFlyFunctionName = "ksvF0",
      onTheFlyParams = list(gender = "u", toFile = F),
      resultType = "tibble",
      verbose = FALSE
    )
    
    ksv_track %>%
      filter(T1 != 0) -> ksv_track_no_outliers
    
    ksv_track_no_outliers %>%
      summarise(mean(T1)) -> mean_F0
    
    ksv_track_no_outliers %>% group_by(bundle) %>%
      summarise(median(T1)) -> median_F0
    
    #Levitan, Sarah Ita, Taniya Mishra, and Srinivas Bangalore. "Automatic identification of gender from speech." Proceeding of speech prosody. 2016.
    #gender minf0 maxf0 medianf0 meanf0 stdvf0
    #male 98.6 249.3 129.23 135.9 29.9
    #female 124.6 300.0 197.4 200.1 33.0
    male_mean = 135.9
    female_mean = 200.1
    
    if(abs(mean_F0-male_mean) <= abs(mean_F0-female_mean)){
      gender = "m"
    } else{
      gender = "f"
    }
    
    #   It is well known that formant measurements are particularly difficult to
    #   perform in high-pitched voices, due to the large distance between adjacent harmonics, leading to
    #   undersampled spectra. This is especially important for LPC analyses, in which formant measures are greatly
    #   influenced by the closest harmonic (Atal & Schroeder, 1974). According to Lindblom (1972), the measurement
    #   error could correspond to a value of 7F0/4 (Menard et al jphon 2007, 35:1)
    
    output_func(sprintf("Determined gender to be: %s", gender), "debug", 1)
  } else {
    output_func('Expected a valid gender option.', "error")
  }
}


output_func("Starting Formant estimation.", "debug", 1)

############GET TRACK DATA#################
if(args$saveTracks==TRUE){
  output_func("--saveTracks is TRUE. Checking for existing Formant tracks.", "debug", 2)
  
  formant_track_names = c("fm", "formant", "fmt", "formants", "forest")
  #THIS BREAKS IF THERE ARE MORE THAN ONE TRACKS WITH THE ABOVE NAMES BUT THAT SHOULDN'T HAPPEN
  existing_ssff = list_ssffTrackDefinitions(DB)
  existing_track_name = formant_track_names[which(formant_track_names %in% existing_ssff["name"])]
  
  
  if(!identical(existing_track_name, character(0))){
    output_func(sprintf("Found existing formant track %s. Removing...", existing_track_name), "debug", 3)
    
    tryCatch({
      remove_ssffTrackDefinition(DB, name=existing_track_name, deleteFiles = TRUE)
    },
    error=function(e){
      message = sprintf("An error occured while trying to remove the track '%s' from emuDB. error: %s. Maybe try using --saveTracks FALSE", existing_track_name, e)
      output_func(message, "error")
    })
  }
  tryCatch({
    add_ssffTrackDefinition(
      DB,
      name = "formants",
      onTheFlyFunctionName = "forest",
      onTheFlyParams = list(gender = gender, toFile = T),
      verbose = FALSE,
      interactive = FALSE
    )
  },
  error=function(e){
    message = sprintf("An error occured while trying to add the formant track to emuDB. error: %s.", e)
    output_func(message, "error")
  })

  
  vowel_track = get_trackdata(
    emuDBhandle = DB,
    seglist = all_sounds_query,
    ssffTrackName = "formants",
    resultType = "tibble",
    verbose = FALSE
  )
} else{
  vowel_track = get_trackdata(
    emuDBhandle = DB,
    seglist = all_sounds_query,
    onTheFlyFunctionName = "forest",
    onTheFlyParams = list(gender = gender, toFile = F),
    resultType = "tibble",
    verbose = FALSE
  )
}

##################NORMALIZE################^

vowel_track %>%
  do(na.omit(.)) %>%
  filter(T1!=0&T2!=0&T3!=0&T4!=0) %>% 
  group_by(sl_rowIdx) %>%
  count() %>% filter(n<2)-> counts

vowel_track %>%
  do(na.omit(.)) %>%
  filter(T1!=0&T2!=0&T3!=0&T4!=0) %>% 
  filter(!sl_rowIdx %in% counts$sl_rowIdx) %>% 
  normalize_length() ->
  normalized_vowel_track
output_func(sprintf("Ommited %s phones due to having not enough clean data points.", nrow(counts)), "debug", 1)


output_func("Median smoothing Formant tracks.", "debug", 1)
normalized_vowel_track$T1 = runmed(normalized_vowel_track$T1, 3, algorithm = "Turlach")
normalized_vowel_track$T2 = runmed(normalized_vowel_track$T2, 3, algorithm = "Turlach")
normalized_vowel_track$T3 = runmed(normalized_vowel_track$T3, 3, algorithm = "Turlach")
normalized_vowel_track$T4 = runmed(normalized_vowel_track$T4, 3, algorithm = "Turlach")


if(args$midpoint == TRUE){
  #medF* = T* to not break other functions that need median values
  normalized_vowel_track %>% 
    filter(times_norm == 0.5) %>% 
    mutate(medF1=T1, medF2=T2, medF3=T3, medF4=T4)-> normalized_vowel_track_mid
} else{
  normalized_vowel_track %>% 
    filter(times_norm > 0.25, times_norm<0.75) %>% 
    group_by(sl_rowIdx) %>% 
    dplyr::summarise(meanF1 = mean(T1), meanF2 = mean(T2), meanF3 = mean(T3), meanF4 = mean(T4),
                     medianF1 = median(T1), medianF2 = median(T2), medianF3 = median(T3), medianF4 = median(T4)
                     )->F_values
  
  normalized_vowel_track %>% 
    filter(times_norm==0.5) %>% 
    mutate(T1 = F_values$meanF1, T2 = F_values$meanF2, T3 = F_values$meanF3, T4 = F_values$meanF4) %>% 
    mutate(medF1 = F_values$medianF1, medF2 = F_values$medianF2, medF3 = F_values$medianF3, medF4 = F_values$medianF4) -> normalized_vowel_track_mid
    
    #Normalized vowel track mid T* = mean of mid 50%| medF* = median of mid 50%
}


#################Remove Outliers#################

remove_outliers_from_vowel_track <- function(vowel_track_with_outliers, threshold=250, metric="euclid", mode="mean"){
  #Takes normalized vowel_track
  #Removes all values that are farther than 'threshold' from their respective phonemes average using the distance metric 'metric'. 

  output_func(sprintf("Starting outlier removal. Mode: %s", mode), "debug", 1)

  vowel_track_with_dist = get_distances(vowel_track_with_outliers, metric, mode) 
  
  vowel_track_with_dist %>% 
    filter(dist < threshold) -> normalized_vowel_track_no_outliers
  
  
  
  outlier_number = nrow(vowel_track_with_outliers)-nrow(normalized_vowel_track_no_outliers)
    
  output_func(sprintf("Removed %s datapoints that were too far from the respective phonemes center.", outlier_number), "debug", 1)

  completely_removed = setdiff(unique(vowel_track_with_outliers$labels), unique(normalized_vowel_track_no_outliers$labels))
  if(!identical(completely_removed, character(0))){
    output_func(sprintf("Removed %s phonemes completely as they were to far spread out.", completely_removed), "warning")
  }
  
  
  return(normalized_vowel_track_no_outliers)
}



get_distances<- function(data, metric="euclid", mode="mean"){
  #Get centroids of vowel spaces.
  if(mode=="mean"){
  data %>%
    group_by(labels) %>%
    dplyr::summarise(summarisedF1 = mean(T1), summarisedF2 = mean(T2), summarisedF3 = mean(T3), summarisedF4 = mean(T4), n=n()) -> centroidsFormants
    
  } else if(mode=="median"){
    data %>%
      group_by(labels) %>%
      dplyr::summarise(summarisedF1 = median(medF1), summarisedF2 = median(medF2), summarisedF3 = median(medF3), summarisedF4 = median(medF4), n=n()) -> centroidsFormants   
    
  }
  data = left_join(data, centroidsFormants, by="labels")
  #Get covariances of formants per phoneme(Only needed for mahalanobis but needs whole DF for distribution).
  #MAYBE REWRITE TO TAKE ARBITRARY FORMANT COUNT INSTEAD OF ONLY FIRST 2
  if(mode=="mean"){
    data %>% 
      group_by(labels) %>%
      dplyr::summarise(covF1F2=cov(T1,T2), covWithinF1=cov(T1,T1), covWithinF2=cov(T2,T2)) -> covariances
  } else if(mode=="median"){
    data %>% 
      group_by(labels) %>%
      dplyr::summarise(covF1F2=cov(medF1,medF2), covWithinF1=cov(medF1,medF1), covWithinF2=cov(medF2,medF2)) -> covariances
    
  }
   
  data = left_join(data, covariances, by="labels")
  if(mode=="mean"){
    data %>% 
      # mutate("eucDist" = sqrt((meanF1-T1)^2 + (meanF2-T2)^2 + (meanF3-T3)^2 + (meanF4-T4)^2)) %>% 
      # filter(eucDist < 500) -> vowel_track_with_outliers_noOutliers
      group_by(sl_rowIdx) %>%
      mutate("dist" = calc_dist(c(summarisedF1,summarisedF2), c(T1,T2),  metric, tibble(T1=c(covWithinF1,covF1F2),T2=c(covF1F2,covWithinF2)))) -> data_with_dist
  } else if(mode=="median"){
    data %>% 
      group_by(sl_rowIdx) %>%
      mutate("dist" = calc_dist(c(summarisedF1,summarisedF2), c(medF1,medF2),  metric, tibble(T1=c(covWithinF1,covF1F2),T2=c(covF1F2,covWithinF2)))) -> data_with_dist
  }
  return(data_with_dist)
}

calc_dist <- function(x, y, metric="euclid", covariance=NA){
  if(length(x) != length(y)){
    output_func("Distance vectors must be of same length.", "error")
  }
  # print(x)
  # print(y)
  # print(covariance)
  output_func(sprintf("Computing distance of %s to %s using metric '%s'",x ,y, metric), "debug", 4)
  
  if(metric=="cosine"){
    output_func("Cosine distance is not yet implemented.", "error")
  } else if(metric=="mahalanobis"){
    if(all(is.na(covariance))){
      output_func("Got empty covariance matrix for one of the sounds. Distance will be set to 0.", "warning")
      return(0)
    }
    dist_vec = mahalanobis(x,y, covariance, tol=1e-20)
  } else{
    #Euclid
    dist_vec = sqrt(sum((x-y)^2))
  }
  return(dist_vec)
}

#############ERATIO COMPUTATIONS############
# see Kleber, F., Harrington, J., & Reubold, U. (2012). The relationship between the perception and production of coarticulation during a sound change in progress. Language and speech, 55(3), 383-405.
# and https://www.phonetik.uni-muenchen.de/~jmh/research/pasc010808/old/ch3.pdf

get_eRatio <-function(sound, ref_sound_1, ref_sound_2, data, col_name, mode="mean"){
  if(mode == "mean"){
    data %>%
      group_by(labels) %>%
      dplyr::summarise(sumF1 = mean(T1), sumF2 = mean(T2), sumF3 = mean(T3), sumF4 = mean(T4)) -> centroidsFormants
  } else if(mode == "median"){
    data %>%
      group_by(labels) %>%
      dplyr::summarise(sumF1 = median(medF1), sumF2 = median(medF2), sumF3 = median(medF3), sumF4 = median(medF4)) -> centroidsFormants
  } else{
    output_func(sprintf("Averaging mode: %s not supported. Please use 'mean' or 'median'.", mode), "error")
  }
 
   centroidsFormants %>%
    filter(labels==ref_sound_1) %>%
    select(sumF1,sumF2)->F_avg_ref1
  centroidsFormants %>%
    filter(labels==ref_sound_2) %>%
    select(sumF1,sumF2)->F_avg_ref2

  if(mode == "mean"){
    data %>%
      filter(labels==sound) %>%
      group_by(sl_rowIdx) %>%
      mutate("ref1dist" = calc_dist(F_avg_ref1[1,], c(T1,T2),  "euclid")) %>%
      mutate("ref2dist" = calc_dist(F_avg_ref2[1,], c(T1,T2),  "euclid")) -> data_with_ref_dists
  } else if(mode == "median"){
    data %>%
      filter(labels==sound) %>%
      group_by(sl_rowIdx) %>%
      mutate("ref1dist" = calc_dist(F_avg_ref1[1,], c(medF1,medF2),  "euclid")) %>%
      mutate("ref2dist" = calc_dist(F_avg_ref2[1,], c(medF1,medF2),  "euclid")) -> data_with_ref_dists
  } else{
    output_func(sprintf("Averaging mode: %s not supported. Please use 'mean' or 'median'.", mode), "error")
  }

  data_with_ref_dists[[col_name]] = with(data_with_ref_dists, log(ref1dist/ref2dist))

  return(data_with_ref_dists %>% select(sl_rowIdx, all_of(col_name)))
}



all_eRatios <- function(data, mode="mean"){
  output_func("Starting eRatio computation.", "debug", 1)
  existing_phonemes=unique(data$labels)
  done_already = c()
  #All triples of "sound_ref1_ref2" but exclude "sound_ref1_sound" and "sound_ref1_ref1", as well as all inverses as they are same value with switched sign
  for(sound in existing_phonemes){
    for(ref1 in existing_phonemes){
        for(ref2 in existing_phonemes){
          if(ref2 != sound && ref2 != ref1){
            inverse_ident = sprintf("%s_%s_%s", sound, ref2, ref1)
            if(!inverse_ident %in% done_already){
              
              col_name = sprintf("%s_and_%s", ref1, ref2)
               #print(col_name)
               #print(sound)
              
              e_ratio_tib = get_eRatio(sound, ref1, ref2, data, col_name, mode)
              #print(e_ratio_tib)
              data = left_join(data, e_ratio_tib, by="sl_rowIdx")

              # data %>%
              #   select(-sl_rowIdx, -db_uuid, -session, -bundle, -start_item_id, -end_item_id, -level, -attribute, -start_item_seq_idx, -end_item_seq_idx, -type, -sample_start, -sample_end, -sample_rate, -times_orig, -times_rel, -times_norm, -T1, -T2, -T3, -T4) %>% print(.)
              
              if(!col_name %in% colnames(data)){
                # If the column doesn't exist because of duplicate in join
                # Unite both removing NAs in the column name.
                data %>% unite(!!col_name, c(paste0(col_name, ".x"),paste0(col_name,".y")), na.rm = TRUE) -> data
                data[[col_name]][data[[col_name]] == ''] <- NA
                data[[col_name]] = as.numeric(data[[col_name]])
              } 
              inverse_col_name = sprintf("%s_and_%s", ref2, ref1)
              if(inverse_col_name %in% colnames(data)){
                #If the inverse column exists. Merge the inverse with flipped sign.
                data[[inverse_col_name]] = data[[inverse_col_name]]*(-1) #Flip sign cause the ratio is reversed
                data %>% unite(!!col_name, c(all_of(col_name), all_of(inverse_col_name)), na.rm = TRUE) -> data
                data[[col_name]][data[[col_name]] == ''] <- NA
                data[[col_name]] = as.numeric(data[[col_name]])
                
              }
              
              ident = sprintf("%s_%s_%s", sound, ref1, ref2)
              done_already = c(done_already, ident)
            } 
          }
        }
      }
    }
  #print(done_already)
  return(data)
}

##################PLOTS##############

generate_standard_FxFy <- function(data, metric, x_formant="T2", y_formant="T1", mode="mean") {
  #MAYBE INSTEAD USE: Typical dispersion ellipses (with radii corresponding to 71.5 standard error of the individual data around the mean) (Menard et al jphon 35:1 2007)
  #Takes normalized vowel track midpoints
  formant_strX = sprintf("F%s",substr(x_formant,2,3))
  formant_strY = sprintf("F%s",substr(y_formant,2,3))
  
  title = paste0(formant_strY, "/", formant_strX, " Plot")
  ylab_str = paste0(formant_strY, " in ", metric)
  xlab_str = paste0(formant_strX, " in ", metric)
  
  if(mode=="median"){
    #If median change column names to medF* (see normalized_vowel_track_mid)
    x_formant = paste0("med", formant_strX)
    y_formant = paste0("med", formant_strY)
  }
  
  plot = ggplot(data = data) +
      {if(tolower(metric)=="bark")aes(y = bark(.data[[y_formant]]), x = bark(.data[[x_formant]]),  col = labels)} +
      {if(tolower(metric)=="hertz" || tolower(metric)=="hz")aes(y = .data[[y_formant]], x = .data[[x_formant]],  col = labels)} +
      ylab(ylab_str) + xlab(xlab_str) + ggtitle(title) +
      geom_text(aes(label=labels)) +
      scale_y_reverse() + scale_x_reverse() +
      theme(legend.position = "none") + stat_ellipse()

  return(plot)
}

generate_dplot_norm <- function(data, Formant, mode) {
  #Takes normalized vowel track 
  #Valid values for Formant are T1, T2, etc...
  formant_str = sprintf("F%s",substr(Formant,2,3))
  #print(deparse(substitute(Formant)))
  if(mode=="median"){
    data %>% 
      group_by(times_norm, labels) %>% 
      dplyr::summarise(formantValues = median(.data[[Formant]]), n=n()) -> intermediate_tibble 
    #rename("formantValues" = sprintf("mean(%s)",deparse(substitute(Formant)))) 
    #print(intermediate_tibble)
  }
  else{
    data %>% 
      group_by(times_norm, labels) %>% 
      dplyr::summarise(formantValues = mean(.data[[Formant]]), n=n()) -> intermediate_tibble 
  }
  intermediate_tibble[!intermediate_tibble$times_norm==0,]$n = NA
  
  plot = ggplot(intermediate_tibble) +
    aes(x=times_norm,y=formantValues, col=labels) +
    geom_line() +
    geom_text(aes(label=n), hjust=1, show.legend = FALSE) +
    scale_x_continuous(label=scales::percent) +
    {if(mode=="mean")labs(x = "Time (normalized)", y = sprintf("Mean %s (Hz)", formant_str), title=sprintf("%s Formant Tracks", formant_str))} +
    {if(mode=="median")labs(x = "Time (normalized)", y = sprintf("Median %s (Hz)", formant_str), title=sprintf("%s Formant Tracks", formant_str))}
  return(plot)
}

generate_vowel_trapezoid <- function(data, method){
  #Takes normalized vowel tack midpoints
  
  cardinals_ipa_unrounded = c("i","e","ɛ","a","ɑ","ʌ","ɤ","ɯ","ɨ","i:","e:","ɛ:","a:","ɑ:","ʌ:","ɤ:","ɯ:","ɨ:")
  cardinals_ipa_rounded = c("y", "ʉ", "u", "o", "ɒ", "ɔ", "œ", "ø", "ɶ","y:", "ʉ:", "u:", "o:", "ɒ:", "ɔ:", "œ:", "ø:", "ɶ:")
  cardinals_sampa_unrounded = c("i","e","E","a","A","V","7","M","1","i:","e:","E:","a:","A:","V:","7:","M:","1:")
  cardinals_sampa_rounded = c("y", "}", "&", "2", "9", "O", "o", "u", "Q", "y:", "}:", "&:", "2:", "9:", "O:", "o:", "u:", "Q:")
  
  
  #Assuming Helmholtz resonator, the lowest possible frequency is dependent on the area and length of constriction.
  #Minimal frequency response because walls of vocal tract are soft.
  #We denote this minimum vocal-tract resonance as f 1 min , with a default value of 180 Hz
  #(Hanson and Stevens 2002,) American english
  
  #Fundamental frequency values were set to 300, 270, and 110 Hz, respectively, for the 4-, 8-year old, and adult stage. (Menard et. al jphon 35:1 2007)
  #Canadian french
  
#   the four cardinal vowels /i/, /u/, /]/, and /y/ are always produced at
# the extreme limits of the F1 vs. F2 vs. F3 acoustic space. Indeed, /i/ defines the upper left corner of the space
# (lowest F1, highest F3), /u/ corresponds to the upper right corner (lowest F1, lowest F2 and low F3), /]/ is
# produced at the lower limit of this space (highest F1), and /y/ is located at the lower left corner of the F2 vs. F3
# space (lowest F1). (Menard et ak S.10)
  if(gender=="m"){
    Formant_limits = data.frame(F1Lim=c(180,180,900,900), F2Lim=c(500,3000,2000,850))
  } else if (gender=="f"){
    Formant_limits = data.frame(F1Lim=c(180,180,1100,1100), F2Lim=c(500,3000,2000,850))
  } else{
    #Child. Not yet implemented
  }
  if(method=="median"){
    data %>%
      group_by(labels) %>%
      dplyr::summarise(midF1 = median(medF1), midF2 = median(medF2), midF3 = median(medF3), midF4 = median(medF4), n=n()) -> centroidsFormants
  }
  else if(method=="mean"){
    data %>%
      group_by(labels) %>%
      dplyr::summarise(midF1 = mean(T1), midF2 = mean(T2), midF3 = mean(T3), midF4 = mean(T4), n=n()) -> centroidsFormants
  }
  centroidsFormants %>% 
    filter(labels %in% cardinals_sampa_rounded | labels %in% cardinals_ipa_rounded | labels %in% cardinals_sampa_unrounded | labels %in% cardinals_ipa_unrounded) %>% 
    ggplot()  + 
    {if(method=="mean")ggtitle("Mean Formant Position in Cardinal Vowels Trapezoid")}+
    {if(method=="median")ggtitle("Median Formant Position in Cardinal Vowels Trapezoid")}+
    aes(y = midF1, x  = midF2) +
    geom_label(aes(label=labels), col="red") +
    geom_text(aes(label=n), vjust=2.5) +
    geom_point(data=Formant_limits, aes(y=F1Lim, x=F2Lim)) +
    geom_polygon(data=Formant_limits, aes(y=F1Lim, x=F2Lim), fill="transparent", linetype="solid", color="pink") +
    scale_y_reverse() + scale_x_reverse() +
    labs(x = "F2(Hz)", y = "F1(Hz)") -> plot
  return(plot)
}

generate_center_Max <- function(data, Formant) {
  #Takes normalized vowel track
  #Valid values for Formant are T1, T2, etc...

  formant_str = sprintf("F%s",substr(Formant,2,3))
  data %>%
    filter(times_norm >= 0.25, times_norm<= 0.75)%>% #Don't take possible measurement errors at start/end as maximum
    group_by(sl_rowIdx, labels) %>%
    slice(which.max(.data[[Formant]])) %>%
    select(sl_rowIdx, times_norm, labels) -> max_times
  
  data_with_max = left_join(data, max_times %>% rename("maxTime"=times_norm), by="sl_rowIdx")
  
  data_with_max %>%
    select(-labels.y) %>% 
    rename(labels = labels.x) %>% 
    mutate("relative_time_to_max" = maxTime-times_norm) %>%
    mutate(absoluteDur = end-start) %>% 
    mutate(relative_abs_time = absoluteDur * relative_time_to_max) -> 
    data_relative_max
  
  title_str = sprintf("%s Tracks Assemble Plot", formant_str)
  
  max_plot = ggplot(data_relative_max) +
    aes(x=relative_abs_time,y=.data[[Formant]],group=sl_rowIdx, col=labels) +
    geom_line() +
    geom_vline(xintercept = 0) +
    labs(x = sprintf("Absolute time (ms) centered at the %s maximum", formant_str),
         y = sprintf("%s (Hz)", formant_str),
         title=title_str)
  return(max_plot)
  
}

save_plot <- function(path, plot){
  pdf(path)
  suppressMessages(print(plot))
  garbage = dev.off()
}

##########OUTPUT############
mainDir = args$output
plotDir = file.path(mainDir, "plots")
csvDir = file.path(mainDir, "csv_output")

dir.create(mainDir, showWarnings = FALSE)
dir.create(plotDir, showWarnings = FALSE)
dir.create(csvDir, showWarnings = FALSE)

Formants = colnames(normalized_vowel_track[,grepl("T",names(normalized_vowel_track))])


##########CSV OUT###########
output_func("Writing out csv data.", "debug", 1)

#Standard Data
write.csv(normalized_vowel_track %>% rename(F1=T1,F2=T2,F3=T3,F4=T4), file.path(csvDir,"./TimeNormalizedMedianSmoothedFormantTracks.csv"), row.names = FALSE)


#Outliers
no_outliers = remove_outliers_from_vowel_track(normalized_vowel_track_mid, threshold = args$outlierThreshold, metric=args$outlierMetric, mode="mean")
no_outliers_median = remove_outliers_from_vowel_track(normalized_vowel_track_mid, threshold = args$outlierThreshold, metric=args$outlierMetric, mode="median")

#Group values
phoneme_summary = get_distances(normalized_vowel_track_mid, metric=args$outlierMetric, mode="mean")
phoneme_summary_median = get_distances(normalized_vowel_track_mid, metric=args$outlierMetric, mode="median")
phoneme_summary %>% 
  group_by(labels) %>% 
  select(labels, n, summarisedF1, summarisedF2, summarisedF3, summarisedF4, covF1F2, covWithinF1, covWithinF2) %>% 
  rename("meanF1" = summarisedF1, "meanF2" = summarisedF2, "meanF3" = summarisedF3, "meanF4"=summarisedF4, "count"=n, "covF1F2Mean"=covF1F2, "covWithinF1Mean"=covWithinF1, "covWithinF2Mean"=covWithinF2)->groups_output
phoneme_summary_median %>% 
  group_by(labels) %>% 
  select(labels, n, summarisedF1, summarisedF2, summarisedF3, summarisedF4, covF1F2, covWithinF1, covWithinF2) %>% 
  rename("medianF1" = summarisedF1, "medianF2" = summarisedF2, "medianF3" = summarisedF3, "medianF4"=summarisedF4, "count"=n, "covF1F2Median"=covF1F2, "covWithinF1Median"=covWithinF1, "covWithinF2Median"=covWithinF2)->groups_median

groups_no_duplicate = left_join(groups_output[!duplicated(groups_output), ], groups_median[!duplicated(groups_median), ] %>% select(medianF1, medianF2, medianF3, medianF4, covF1F2Median, covWithinF1Median, covWithinF2Median, labels), by="labels")

write.csv(groups_no_duplicate %>%
            rename(phoneme=labels)%>%
            relocate(phoneme, count, meanF1, meanF2, meanF3, meanF4, medianF1, medianF2, medianF3, medianF4),
          file.path(csvDir,"./PhonemeAverages.csv"), row.names = FALSE)

#Eratios
if(args$eRatio == TRUE){
  e_dists_mean = all_eRatios(normalized_vowel_track_mid, "mean")
  e_dists_median = all_eRatios(normalized_vowel_track_mid, "median")
  
  e_dists_mean %>%
    mutate(filename=paste0(session,bundle)) %>% 
    select(-sl_rowIdx, -db_uuid, -session, -bundle, -start_item_id, -end_item_id, -level, -attribute, -start_item_seq_idx, -end_item_seq_idx, -type, -sample_start, -sample_end, -sample_rate, -times_orig, -times_rel, -times_norm, -medF1, -medF2, -medF3, -medF4, -T3, -T4) %>% 
    relocate(filename) %>%
    rename("Phoneme" = labels, "F1" = T1, "F2" = T2) %>% 
    arrange(filename, start)->e_dists_mean_clean
  
  e_dists_median %>%
    mutate(filename=paste0(session,bundle)) %>% 
    select(-sl_rowIdx, -db_uuid, -session, -bundle, -start_item_id, -end_item_id, -level, -attribute, -start_item_seq_idx, -end_item_seq_idx, -type, -sample_start, -sample_end, -sample_rate, -times_orig, -times_rel, -times_norm, -T1, -T2, -T3, -T4, -medF3, -medF4) %>% 
    relocate(filename) %>%
    rename("Phoneme" = labels, "F1" = medF1, "F2" = medF2) %>% 
    arrange(filename, start)->e_dists_median_clean
  
  
  write.csv(e_dists_mean_clean, file.path(csvDir,"./eRatiosMean.csv"), row.names = FALSE)
  write.csv(e_dists_median_clean, file.path(csvDir,"./eRatiosMedian.csv"), row.names = FALSE)
}


#Simple CSV
normalized_vowel_track_mid_with_dist = get_distances(normalized_vowel_track_mid)
normalized_vowel_track_mid_with_dist$distMed = get_distances(normalized_vowel_track_mid, mode="median")$dist

#MEANS HERE ARE JUST TO REMOVE DUPLICATE VALUE NOT REAL MEANS 
#1 Row per Phoneme token.
normalized_vowel_track_mid_with_dist %>%
  mutate(filename=paste(session,bundle,sep="_")) %>% 
  group_by(filename, labels, sl_rowIdx) %>% 
  dplyr::summarise(medianF1Hz=median(medF1), medianF2Hz=median(medF2), medianF3Hz=median(medF3), medianF4Hz=median(medF4),
                   meanF1Hz=mean(T1), meanF2Hz=mean(T2), meanF3Hz=mean(T3), meanF4Hz=mean(T4),
                   medianF1Bark=median(bark(medF1)), medianF2Bark=median(bark(medF2)), medianF3Bark=median(bark(medF3)), medianF4Bark=median(bark(medF4)),
                   meanF1Bark=mean(bark(T1)), meanF2Bark=mean(bark(T2)), meanF3Bark=mean(bark(T3)), meanF4Bark=mean(bark(T4)),
                   start=mean(start), end=mean(end), midDistanceMean=mean(dist), midDistanceMedian=mean(distMed)) %>%
  select(-sl_rowIdx) %>% 
  rename("Phoneme" = labels, "StartMs"=start, "EndMs"=end) %>% 
  arrange(filename,StartMs)-> simple_track

no_outliers %>%
  mutate(filename=paste(session,bundle,sep="_")) %>% 
  group_by(filename, labels, sl_rowIdx) %>% 
  dplyr::summarise(medianF1Hz=median(medF1), medianF2Hz=median(medF2), medianF3Hz=median(medF3), medianF4Hz=median(medF4),
                   meanF1Hz=mean(T1), meanF2Hz=mean(T2), meanF3Hz=mean(T3), meanF4Hz=mean(T4),
                   medianF1Bark=median(bark(medF1)), medianF2Bark=median(bark(medF2)), medianF3Bark=median(bark(medF3)), medianF4Bark=median(bark(medF4)),
                   meanF1Bark=mean(bark(T1)), meanF2Bark=mean(bark(T2)), meanF3Bark=mean(bark(T3)), meanF4Bark=mean(bark(T4)),
                   start=mean(start), end=mean(end), midDistanceMean=mean(dist)) %>%
  select(-sl_rowIdx) %>% 
  rename("Phoneme" = labels, "StartMs"=start, "EndMs"=end) %>% 
  arrange(filename,StartMs)-> simple_track_no_out

no_outliers_median %>%
  mutate(filename=paste(session,bundle,sep="_")) %>% 
  group_by(filename, labels, sl_rowIdx) %>% 
  dplyr::summarise(medianF1Hz=median(medF1), medianF2Hz=median(medF2), medianF3Hz=median(medF3), medianF4Hz=median(medF4),
                   meanF1Hz=mean(T1), meanF2Hz=mean(T2), meanF3Hz=mean(T3), meanF4Hz=mean(T4),
                   medianF1Bark=median(bark(medF1)), medianF2Bark=median(bark(medF2)), medianF3Bark=median(bark(medF3)), medianF4Bark=median(bark(medF4)),
                   meanF1Bark=mean(bark(T1)), meanF2Bark=mean(bark(T2)), meanF3Bark=mean(bark(T3)), meanF4Bark=mean(bark(T4)),
                   start=mean(start), end=mean(end), midDistanceMedian=mean(dist)) %>%
  select(-sl_rowIdx) %>% 
  rename("Phoneme" = labels, "StartMs"=start, "EndMs"=end) %>% 
  arrange(filename,StartMs)-> simple_track_no_out_med


write.csv(simple_track %>% mutate(across(where(is.numeric), ~round(., digits=4))), file.path(csvDir,"./FormantSummary.csv"), row.names = FALSE)
write.csv(simple_track_no_out %>% mutate(across(where(is.numeric), ~round(., digits=4))), file.path(csvDir,"./FormantSummaryNoOutliersMean.csv"), row.names = FALSE)
write.csv(simple_track_no_out_med %>% mutate(across(where(is.numeric), ~round(., digits=4))), file.path(csvDir,"./FormantSummaryNoOutliersMedian.csv"), row.names = FALSE)


#############PLOTS############
output_func("Generating plots.", "debug", 1)

for(formant in Formants){
  plot_center_max = generate_center_Max(normalized_vowel_track, formant)
  
  mean_dplot_norm = generate_dplot_norm(normalized_vowel_track, formant, "mean")
  median_dplot_norm = generate_dplot_norm(normalized_vowel_track, formant, "median")

  formant_str = sprintf("F%s",substr(formant,2,3))
  
  save_plot(file.path(plotDir,sprintf("./%sTrackTimeNormalizedMean.pdf", formant_str)), mean_dplot_norm)
  save_plot(file.path(plotDir,sprintf("./%sTrackTimeNormalizedMedian.pdf", formant_str)), median_dplot_norm)
  save_plot(file.path(plotDir,sprintf("./%sCenteredOnMax.pdf", formant_str)), plot_center_max)
}

F1F2_plot_mean = generate_standard_FxFy(normalized_vowel_track_mid, "Hz", x="T2", y="T1", mode="mean")
F1F2_plot_bark_mean = generate_standard_FxFy(normalized_vowel_track_mid, "bark", x="T2", y="T1", mode="mean")
F1F2_plot_mean_noout = generate_standard_FxFy(no_outliers, "Hz", x="T2", y="T1", mode="mean")
F1F2_plot_mean_bark_noout = generate_standard_FxFy(no_outliers, "bark", x="T2", y="T1", mode="mean")

F1F2_plot_median = generate_standard_FxFy(normalized_vowel_track_mid, "Hz", x="T2", y="T1", mode="median")
F1F2_plot_bark_median = generate_standard_FxFy(normalized_vowel_track_mid, "bark", x="T2", y="T1", mode="median")
F1F2_plot_median_noout = generate_standard_FxFy(no_outliers_median, "Hz", x="T2", y="T1", mode="median")
F1F2_plot_median_bark_noout = generate_standard_FxFy(no_outliers_median, "bark", x="T2", y="T1", mode="median")


save_plot(file.path(plotDir,"./VowelSpacePlotMeanNoOutliers.pdf"),
          plot = F1F2_plot_mean_noout
)
save_plot(file.path(plotDir,"./VowelSpacePlotMeanBarkNoOutliers.pdf"),
          plot = F1F2_plot_mean_bark_noout
)
save_plot(file.path(plotDir,"./VowelSpacePlotMean.pdf"),
          plot = F1F2_plot_mean
)
save_plot(file.path(plotDir,"./VowelSpacePlotMeanBark.pdf"),
          plot = F1F2_plot_bark_mean
)


save_plot(file.path(plotDir,"./VowelSpacePlotMedianNoOutliers.pdf"),
          plot = F1F2_plot_median_noout
)
save_plot(file.path(plotDir,"./VowelSpacePlotMedianBarkNoOutliers.pdf"),
          plot = F1F2_plot_median_bark_noout
)
save_plot(file.path(plotDir,"./VowelSpacePlotMedian.pdf"),
          plot = F1F2_plot_median
)
save_plot(file.path(plotDir,"./VowelSpacePlotMedianBark.pdf"),
          plot = F1F2_plot_bark_median
)


F3F2_plot_mean = generate_standard_FxFy(normalized_vowel_track_mid, "Hz", x="T2", y="T3")
F3F2_plot_bark_mean = generate_standard_FxFy(normalized_vowel_track_mid, "bark", x="T2", y="T3")
F3F2_plot_mean_noout = generate_standard_FxFy(no_outliers, "Hz", x="T2", y="T3")
F3F2_plot_mean_bark_noout = generate_standard_FxFy(no_outliers, "bark", x="T2", y="T3")
save_plot(file.path(plotDir,"./F3F2PlotMeanNoOutliers.pdf"),
          plot = F3F2_plot_mean_noout
)
save_plot(file.path(plotDir,"./F3F2PlotMeanBarkNoOutliers.pdf"),
          plot = F3F2_plot_mean_bark_noout
)
save_plot(file.path(plotDir,"./F3F2PlotMean.pdf"),
          plot = F3F2_plot_mean
)
save_plot(file.path(plotDir,"./F3F2PlotMeanBark.pdf"),
          plot = F3F2_plot_bark_mean
)


F3F2_plot_median = generate_standard_FxFy(normalized_vowel_track_mid, "Hz", x="T2", y="T3", mode="median")
F3F2_plot_bark_median = generate_standard_FxFy(normalized_vowel_track_mid, "bark", x="T2", y="T3", mode="median")
F3F2_plot_median_noout = generate_standard_FxFy(no_outliers_median, "Hz", x="T2", y="T3", mode="median")
F3F2_plot_median_bark_noout = generate_standard_FxFy(no_outliers_median, "bark", x="T2", y="T3", mode="median")
save_plot(file.path(plotDir,"./F3F2PlotMedianNoOutliers.pdf"),
          plot = F3F2_plot_median_noout
)
save_plot(file.path(plotDir,"./F3F2PlotMedianBarkNoOutliers.pdf"),
          plot = F3F2_plot_median_bark_noout
)
save_plot(file.path(plotDir,"./F3F2PlotMedian.pdf"),
          plot = F3F2_plot_median
)
save_plot(file.path(plotDir,"./F3F2PlotMedianBark.pdf"),
          plot = F3F2_plot_bark_median
)


vowel_trapezoid = generate_vowel_trapezoid(normalized_vowel_track_mid, "mean")
vowel_trapezoid_median = generate_vowel_trapezoid(normalized_vowel_track_mid, "median")
vowel_trapezoid_noout = generate_vowel_trapezoid(no_outliers, "mean")
vowel_trapezoid_noout_median = generate_vowel_trapezoid(no_outliers_median, "median")


save_plot(file.path(plotDir,"./VowelTrapezoidMeanNoOutliers.pdf"),
          plot = vowel_trapezoid_noout
)
save_plot(file.path(plotDir,"./VowelTrapezoidMedianNoOutliers.pdf"),
           plot = vowel_trapezoid_noout_median
)
save_plot(file.path(plotDir,"./VowelTrapezoidMean.pdf"),
          plot = vowel_trapezoid
)
save_plot(file.path(plotDir,"./VowelTrapezoidMedian.pdf"),
          plot = vowel_trapezoid_median
)
###############OUTPUT PARAMS##########
param_path = file.path(mainDir, "SelectedParameters.txt")

sink(param_path)
for(name in names(args[0:13])){
  cat(paste0(name, " : ", args[[name]], "\n"))
}
cat(paste0("Automatic gender detection result: " , gender))
sink()

################COPY README#########
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script_path = sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script_dir = dirname(script_path)
readme_path_plot = paste(script_dir, "README_PLOTS.asc", sep="/")
readme_path_csv = paste(script_dir, "README_CSV.asc", sep="/")
dump = file.copy(readme_path_csv, csvDir)
dump = file.copy(readme_path_plot, plotDir)

###########COPY THIS SCRIPT########
dump = file.copy(script_path, mainDir)
