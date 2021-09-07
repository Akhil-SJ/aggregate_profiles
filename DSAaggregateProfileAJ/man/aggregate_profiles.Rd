\name{aggregate_profiles}
\alias{aggregate_profiles}
\title{Aggregate Profiles}
\usage{

aggregate_profiles = function(load,
                              ID="", SEGMENT= list(c("")),  USAGE="", START="",
                              END="", WGHT="", TREAT="", long="", DATE="", hour="")
}
\description{


Purpose: Given a list of IDs, load interval data and filter to the given
 range. Then construct the average profile for that set of IDs.

 Optionally include frequency weights, treatment indicator and option
 to reshape the data long

 Assumed structure:
   data long by id(s)
 id(s) | segment var(s) | [treat | weight]

 Required inputs:

   [if]: 			optionally specified
 id: 			any combination of string and numeric unique identifiers.
 must be unique ID of the dataset (no duplicates by ID)
 segment:		List of segmentation variables
 sourcefile:		the path to the interval data. The interval data is
 assumed to have all the IDs listed in the id option
 specified, long by date and wide by hour.
 usage:			the stub of the interval data value. If the data is wide
 by kwh, the 'usage' variable is 'kwh'. If it's kwh_*,
 						then usage is 'kwh_'

 	Optional inputs:

 		start:			stata date of the beginning of the interval data that
 you want to query. If start is left empty but end is
 						populated, all data on and before end is returned
 		end:			stata date of the end of the interval data that
 						you want to query. If end is left empty but start is
						populated, all data on and after start is returned
 		wght:			weight of the ID for the collapse. Collapsed using
 						frequency weights
 		treat: 			binary treatment variable 0/1.
 		long:			optionally specified if you want the data returned in
 						long format
 	Outputs:
 		The dataset returned has the following outputs:
 		segment:		the list of segmentation variables that were provided
 						originally by user
 		treat:			the treatment indicator provided by user (if treatment
 						option specified
 		_count:			the number of IDs (unique) in each interval
 		_weightcount:	the number of IDs (frequency weighted) in each interval
 		date:			the date variable
 		hour:			the hour variable (if user specifies 'long' option)
 		usage:			the usage variables (either wide or long)
 Purpose: Given a list of IDs, load interval data and filter to the given
 range. Then construct the average profile for that set of IDs.

 Optionally include frequency weights, treatment indicator and option
 to reshape the data long

 Assumed structure:
   data long by id(s)
 id(s) | segment var(s) | [treat | weight]

 Required inputs:


 id: 			any combination of string and numeric unique identifiers.
 must be unique ID of the dataset (no duplicates by ID)
 segment:		List of segmentation variables
 sourcefile:		the path to the interval data. The interval data is
 assumed to have all the IDs listed in the id option
 specified, long by date and wide by hour.
 usage:			the stub of the interval data value. If the data is wide
 by kwh, the 'usage' variable is 'kwh'. If it's kwh_*,
 						then usage is 'kwh_'

 	Optional inputs:

 		start:			stata date of the beginning of the interval data that
 you want to query. If start is left empty but end is
 						populated, all data on and before end is returned
 		end:			stata date of the end of the interval data that
 						you want to query. If end is left empty but start is
 						populated, all data on and after start is returned
 		wght:			weight of the ID for the collapse. Collapsed using
 						frequency weights
 		treat: 			binary treatment variable 0/1.
 		long:			optionally specified if you want the data returned in
 						long format

 	Outputs:
 		The dataset returned has the following outputs:

 		segment:		the list of segmentation variables that were provided
 						originally by user
 		treat:			the treatment indicator provided by user (if treatment
 						option specified
 		_count:			the number of IDs (unique) in each interval
 		_weightcount:	the number of IDs (frequency weighted) in each interval
 		date:			the date variable
 		hour:			the hour variable (if user specifies 'long' option)
		usage:			the usage variables (either wide or long)

}
