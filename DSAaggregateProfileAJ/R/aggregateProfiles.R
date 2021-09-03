

# Purpose: Given a list of IDs, load interval data and filter to the given
# range. Then construct the average profile for that set of IDs.

# Optionally include frequency weights, treatment indicator and option
# to reshape the data long

# Assumed structure:
#   data long by id(s)
# id(s) | segment var(s) | [treat | weight]

# Required inputs:

#   [if]: 			optionally specified
# id: 			any combination of string and numeric unique identifiers.
# must be unique ID of the dataset (no duplicates by ID)
# segment:		List of segmentation variables
# sourcefile:		the path to the interval data. The interval data is
# assumed to have all the IDs listed in the id option
# specified, long by date and wide by hour.
# usage:			the stub of the interval data value. If the data is wide
# by kwh, the 'usage' variable is 'kwh'. If it's kwh_*,
# 						then usage is 'kwh_'

# 	Optional inputs:

# 		start:			stata date of the beginning of the interval data that
# you want to query. If start is left empty but end is
# 						populated, all data on and before end is returned
# 		end:			stata date of the end of the interval data that
# 						you want to query. If end is left empty but start is
# 						populated, all data on and after start is returned
# 		wght:			weight of the ID for the collapse. Collapsed using
# 						frequency weights
# 		treat: 			binary treatment variable 0/1.
# 		long:			optionally specified if you want the data returned in
# 						long format

# 	Outputs:
# 		The dataset returned has the following outputs:

# 		segment:		the list of segmentation variables that were provided
# 						originally by user
# 		treat:			the treatment indicator provided by user (if treatment
# 						option specified
# 		_count:			the number of IDs (unique) in each interval
# 		_weightcount:	the number of IDs (frequency weighted) in each interval
# 		date:			the date variable
# 		hour:			the hour variable (if user specifies 'long' option)
# 		usage:			the usage variables (either wide or long)

# Purpose: Given a list of IDs, load interval data and filter to the given
# range. Then construct the average profile for that set of IDs.

# Optionally include frequency weights, treatment indicator and option
# to reshape the data long

# Assumed structure:
#   data long by id(s)
# id(s) | segment var(s) | [treat | weight]

# Required inputs:

#   [if]: 			optionally specified
# id: 			any combination of string and numeric unique identifiers.
# must be unique ID of the dataset (no duplicates by ID)
# segment:		List of segmentation variables
# sourcefile:		the path to the interval data. The interval data is
# assumed to have all the IDs listed in the id option
# specified, long by date and wide by hour.
# usage:			the stub of the interval data value. If the data is wide
# by kwh, the 'usage' variable is 'kwh'. If it's kwh_*,
# 						then usage is 'kwh_'

# 	Optional inputs:

# 		start:			stata date of the beginning of the interval data that
# you want to query. If start is left empty but end is
# 						populated, all data on and before end is returned
# 		end:			stata date of the end of the interval data that
# 						you want to query. If end is left empty but start is
# 						populated, all data on and after start is returned
# 		wght:			weight of the ID for the collapse. Collapsed using
# 						frequency weights
# 		treat: 			binary treatment variable 0/1.
# 		long:			optionally specified if you want the data returned in
# 						long format

# 	Outputs:
# 		The dataset returned has the following outputs:

# 		segment:		the list of segmentation variables that were provided
# 						originally by user
# 		treat:			the treatment indicator provided by user (if treatment
# 						option specified
# 		_count:			the number of IDs (unique) in each interval
# 		_weightcount:	the number of IDs (frequency weighted) in each interval
# 		date:			the date variable
# 		hour:			the hour variable (if user specifies 'long' option)
# 		usage:			the usage variables (either wide or long)


aggregate_profiles = function(load,
                              ID="", SEGMENT= list(c("")),  USAGE="", START="",
                              END="", WGHT="", TREAT="", long="", DATE="", hour="")
{

  ##format usa
  usage=  sub("^", "^", USAGE )

  SEGMENT = as.character(SEGMENT)


  # 1.Packages for code

  pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }

  #load lib.
  pkgTest('haven')

  pkgTest('data.table')

  pkgTest('doBy')

  pkgTest('dplyr')

  pkgTest('tidyr')

  # 2. bring in and format data sets



  load = rename(load, id = ID)
  id = load$id





  seg = unique(load[c(SEGMENT)])

  seg  <- dplyr::mutate(seg, segment = row_number())







  if(START != "") {
    load = rename(load, start = START )
    start = load$start } else {load$start = NA
    start = load$start}

  if(TREAT != "") {
    load = rename(load,treat = TREAT)
    treat = load$treat } else {load$treat = ""
    treat = load$treat}

  if(DATE != ""){
    load = rename(load, date = DATE )
    date =load$date} else {load$date = NA
    date =load$date}

  if(END != ""){
    load = rename(load,end = END )
    end = load$end} else {load$end = NA
    end = load$end}


  if(is.numeric(WGHT) != TRUE) {
    load$weight = 1
    weight = load$weight
  }


  if(is.numeric(WGHT) == TRUE) {

    load$weight = WGHT
    weight = load$weight
  }





  common_col_names <- intersect(names(load), names(seg))

  load = merge(load, seg, by=common_col_names, all.x=TRUE)

  sample = load

  segment = sample$segment



  Sample = rename_with(sample,  tolower)

  usage = Sample[,grep(usage, names(Sample), value=TRUE)]

  usage = usage[,order(colnames(usage))]


  colnames(usage)  <- c("usage1","usage10","usage11","usage12",
                        "usage13","usage14","usage15","usage16","usage17","usage18",
                        "usage19","usage2","usage20","usage21","usage22", "usage23",
                        "usage24",
                        "usage3","usage4","usage5",    "usage6" ,
                        "usage7" , "usage8","usage9")



  id = as.data.frame(id)
  segment = as.data.frame(segment)
  usage = as.data.frame(usage)
  start = as.data.frame(start)
  end = as.data.frame(end)
  weight = as.data.frame(weight)
  treat = as.data.frame(treat)
  date = as.data.frame(date)

  ##working data
  df = cbind(id,segment,usage,start,end,weight,treat,date)


  #B. Keep the relevant variables only
  #marksample	 touse
  #keep if 	`touse'

  #Flag treatment variables (if applicable)
  ifelse((df$treat != ""),(df$treatVar = df$treat),(df$treatVar = ""))



  #Flag weight variables (if applicable)
  ifelse((df$weight != ""),(df$weight_flag = weight),(df$weight_flag = 1))





  # Filter to relevant variables

  df_subset <- df[,c("id", "treatVar", "segment", "weight_flag")]



  #C. Merge the interval data (this function assumes that it comes in wide)


  deduped.data <- unique( df_subset[ , 1:4 ] )


  deduped.data$id2 = as.numeric(deduped.data$id)
  df$id2 = as.numeric(df$id)



  #df_interval =  data.table(deduped.data, key="id2")[
  # data.table(df, key="id2"),
  # allow.cartesian=TRUE
  #  ]

  df_interval = merge( deduped.data, df, by.x = "id2", by.y = "id2", all)




  df_interval = subset(df_interval, select = c( id.x ,
                                                treatVar.x, segment.x,
                                                weight_flag.x, usage1,
                                                usage10, usage11,
                                                usage12 ,  usage13,usage14,
                                                usage15 , usage16,usage17 ,
                                                usage18 ,usage19 ,usage2,
                                                usage20 ,usage21 ,  usage22
                                                ,usage23 , usage24 , usage3
                                                ,usage4 ,  usage5 ,usage6,
                                                usage7 , usage8, usage9
                                                ,start  , end, weight ,
                                                treat , date    ) )



  df_interval <- df_interval  %>%
    rename(id = id.x ,
           treatVar = treatVar.x,
           segment = segment.x,
           weight_flag = weight_flag.x
    )


  if ( sum(is.na(df_interval[])) >= 1) {

    print("Warning: unmatched IDs not matched")

    ### output mismatched


  }


  #D. Keep appropriate date ranges

  df = as.data.frame( df_interval)







  #missing/na 0

  df$start[is.na( df$start)] <- "1900-01-01"
  df$end[is.na( df$end)] <- "1900-01-01"

  # if both start & end are populated
  ifelse( (df$start != "1900-01-01" & df$end != "1900-01-01"),
          (df = df[df$date >= df$start & df$date <= df$end, ]),( g=5) )


  #	if only start is populated
  ifelse( (df$start != "1900-01-01" & df$end != "1900-01-01"),
          ( df = df[df$date >= df$start , ]), (gg=5))


  #	if only endis populated
  ifelse( (df$start != "1900-01-01" & df$end != "1900-01-01"),
          ( df = df[df$date <= df$end , ]), (ggg=5))



  #E. Collapse to date/hour level

  df$count = 1/df$weight_flag
  df$weightcount = 1


  ls = as.vector ( grep("usage", names(df), value=TRUE))

  id = as.data.frame(df$id)
  wght = as.data.frame(df$weight_flag)

  usage = subset(df, select=ls)

  usage = cbind(usage,wght,id)

  usage$id = usage$`df$id`
  usage$wght = usage$`df$weight_flag`


  df2 = df
  df2[is.na(df2)] <- 0





  weighted_df= df2 %>% group_by(segment, date ) %>%
    summarise((weighted_count =  sum(count)),
              (weighted_usage1 = weighted.mean(usage1, weight)),
              (weighted_usage2 = weighted.mean(usage2, weight)),
              (weighted_usage3 = weighted.mean(usage3, weight)),
              (weighted_usage4 = weighted.mean(usage4, weight)),
              (weighted_usage5 = weighted.mean(usage5, weight)),
              (weighted_usage6 = weighted.mean(usage6, weight)),
              (weighted_usage7 = weighted.mean(usage7, weight)),
              (weighted_usage8 = weighted.mean(usage8, weight)),
              (weighted_usage9 = weighted.mean(usage9, weight)),
              (weighted_usage10 = weighted.mean(usage10, weight)),
              (weighted_usage11 = weighted.mean(usage11, weight)),
              (weighted_usage12 = weighted.mean(usage12, weight)),
              (weighted_usage13 = weighted.mean(usage13, weight)),
              (weighted_usage14 = weighted.mean(usage14, weight)),
              (weighted_usage15 = weighted.mean(usage15, weight)),
              (weighted_usage16 = weighted.mean(usage16, weight)),
              (weighted_usage17 = weighted.mean(usage17, weight)),
              (weighted_usage18 = weighted.mean(usage18, weight)),
              (weighted_usage19 = weighted.mean(usage19, weight)),
              (weighted_usage20 = weighted.mean(usage20, weight)),
              (weighted_usage21 = weighted.mean(usage21, weight)),
              (weighted_usage22 = weighted.mean(usage22, weight)),
              (weighted_usage23 = weighted.mean(usage23, weight)),
              (weighted_usage24 = weighted.mean(usage24, weight)),
              (weight = sum(weight)))

  #rename variables



  names(weighted_df) <- c('Segment', 'Date',  'Count',
                          'weighted_usage1',
                          'weighted_usage2',
                          'weighted_usage3',
                          'weighted_usage4',
                          'weighted_usage5',
                          'weighted_usage6',
                          'weighted_usage7',
                          'weighted_usage8',
                          'weighted_usage9',
                          'weighted_usage10',
                          'weighted_usage11',
                          'weighted_usage12',
                          'weighted_usage13',
                          'weighted_usage14',
                          'weighted_usage15',
                          'weighted_usage16',
                          'weighted_usage17',
                          'weighted_usage18',
                          'weighted_usage19',
                          'weighted_usage20',
                          'weighted_usage21',
                          'weighted_usage22',
                          'weighted_usage23',
                          'weighted_usage24',
                          'Weight' )



  ##### collapse

  dt <- data.table(weighted_df)

  weighted_df = dt[,list(usage1=mean(weighted_usage1),usage2=mean(weighted_usage2),
                         usage3=mean(weighted_usage3),usage4=mean(weighted_usage4),
                         usage5=mean(weighted_usage5),usage6=mean(weighted_usage6),
                         usage7=mean(weighted_usage7),usage8=mean(weighted_usage8),
                         usage9=mean(weighted_usage9),usage10=mean(weighted_usage10),
                         usage11=mean(weighted_usage11),usage12=mean(weighted_usage12),
                         usage13=mean(weighted_usage13),usage14=mean(weighted_usage14),
                         usage15=mean(weighted_usage15),usage16=mean(weighted_usage16),
                         usage17=mean(weighted_usage17),usage18=mean(weighted_usage18),
                         usage19=mean(weighted_usage19),usage20=mean(weighted_usage20),
                         usage21=mean(weighted_usage21),usage22=mean(weighted_usage22),
                         usage23=mean(weighted_usage23),usage24=mean(weighted_usage24),
                         Count = sum(Count), Weight = sum(Weight)),by=list(Date, Segment) ]



  ##export wide
  #' @export
  wide_df <<-((weighted_df))

  #F. If the user specifies long, reshape the file long

  if (long == "Yes")  {
    long <- melt(setDT(weighted_df),
                 id.vars = c('Segment', 'Date',  'Count','Weight'),
                 variable.name = "Usage_Hour")
    long$Usage_Hour = extract_numeric(long$Usage_Hour)
    long =  rename(long, Weighted_Avg = value )
    weighted_df =long

    mergee = data.table(weighted_df, key="Segment")[
      data.table(seg, key="segment"),
      allow.cartesian=TRUE
      ]


    #' @export
    ##export long df
    long_df <<-((mergee))

  }



  mergee = data.table(weighted_df, key="Segment")[
    data.table(seg, key="segment"),
    allow.cartesian=TRUE
    ]






  ##export wide
  #' @export
  wide_df <<-((mergee))


}

