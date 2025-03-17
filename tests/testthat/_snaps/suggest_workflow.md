# suggest_workflow prints table and results

    Code
      msgs
    Output
       [1] "\n"                                                                                                                                     
       [2] "-- Matching Darwin Core terms --------------------------------------------------\n"                                                     
       [3] "Matched 6 of 9 column names to DwC terms:\n"                                                                                            
       [4] "v Matched: continent, country, decimalLatitude, decimalLongitude, month,\n  scientificName\n"                                           
       [5] "x Unmatched: date, n, time\n"                                                                                                           
       [6] "\n"                                                                                                                                     
       [7] "-- Minimum required Darwin Core terms ------------------------------------------\n"                                                     
       [8] "\n"                                                                                                                                     
       [9] "-- Suggested workflow ----------------------------------------------------------\n"                                                     
      [10] "df |>\n"                                                                                                                                
      [11] "  set_occurrences() |> \n"                                                                                                              
      [12] "  set_datetime() |> \n"                                                                                                                 
      [13] "  set_coordinates()\n"                                                                                                                  
      [14] "\n"                                                                                                                                     
      [15] "-- Additional functions \n"                                                                                                             
      [16] "Based on your matched terms, you can also add to your pipe:\n"                                                                          
      [17] "* `set_datetime()` and `set_locality()`\n"                                                                                              
      [18] "i See all `set_` functions at\n  http://corella.ala.org.au/reference/index.html#add-rename-or-edit-columns-to-match-darwin-core-terms\n"

# suggest_workflow celebrates when data meets Darwin Core Standard

    Code
      msgs
    Output
       [1] "\n"                                                                                                                                                     
       [2] "-- Matching Darwin Core terms --------------------------------------------------\n"                                                                     
       [3] "Matched 8 of 8 column names to DwC terms:\n"                                                                                                            
       [4] "v Matched: basisOfRecord, coordinateUncertaintyInMeters, decimalLatitude,\n  decimalLongitude, eventDate, geodeticDatum, occurrenceID, scientificName\n"
       [5] "x Unmatched:\n"                                                                                                                                         
       [6] "\n"                                                                                                                                                     
       [7] "-- Minimum required Darwin Core terms ------------------------------------------\n"                                                                     
       [8] "\n"                                                                                                                                                     
       [9] "-- Suggested workflow ----------------------------------------------------------\n"                                                                     
      [10] "Run checks, or use your dataframe to build a Darwin Core Archive with galaxias:\n"                                                                      
      [11] "df |>\n"                                                                                                                                                
      [12] "  check_dataset()\n"                                                                                                                                    
      [13] "\n"                                                                                                                                                     
      [14] "-- Additional functions \n"                                                                                                                             
      [15] "i See all `set_` functions at\n  http://corella.ala.org.au/reference/index.html#add-rename-or-edit-columns-to-match-darwin-core-terms\n"                

