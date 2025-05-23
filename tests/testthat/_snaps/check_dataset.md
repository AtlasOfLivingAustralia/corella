# check_dataset prints table and results

    Code
      msgs
    Output
      [1] "i Testing data\n"                                                                  
      [2] "\r\\ | 0  scientificName \033[K\r"                                                 
      [3] "\rv | 0 v | scientificName  [1s]\033[K\r"                                          
      [4] "\n"                                                                                
      [5] "== Results =====================================================================\n"
      [6] "i Checking Darwin Core compliance\n"                                               
      [7] "i Use `suggest_workflow()` to see more information.\n"                             

# check_dataset errors in table and results

    Code
      msgs
    Output
      [1] "i Testing data\n"                                                                  
      [2] "\r\\ | 0  scientificName \033[K\r"                                                 
      [3] "\rv | 0 v | scientificName  [1s]\033[K\r"                                          
      [4] "\n"                                                                                
      [5] "== Results =====================================================================\n"
      [6] "i Checking Darwin Core compliance\n"                                               
      [7] "i Use `suggest_workflow()` to see more information.\n"                             

# check_dataset notifies when data meets minimum Darwin Core column requirements

    Code
      msgs
    Output
       [1] "i Testing data\n"                                                                  
       [2] "\r\\ | 0  scientificName                \033[K\r"                                  
       [3] "\rv | 0 v | scientificName                 [1s]\033[K\r"                           
       [4] "\n"                                                                                
       [5] "\r\\ | 0  decimalLatitude               \033[K\r"                                  
       [6] "\rv | 0 v | decimalLatitude                [1s]\033[K\r"                           
       [7] "\n"                                                                                
       [8] "\r\\ | 0  decimalLongitude              \033[K\r"                                  
       [9] "\rv | 0 v | decimalLongitude               [1s]\033[K\r"                           
      [10] "\n"                                                                                
      [11] "\r\\ | 0  eventDate                     \033[K\r"                                  
      [12] "\rv | 0 v | eventDate                      [1s]\033[K\r"                           
      [13] "\n"                                                                                
      [14] "\r\\ | 0  occurrenceID                  \033[K\r"                                  
      [15] "\rv | 0 v | occurrenceID                   [1s]\033[K\r"                           
      [16] "\n"                                                                                
      [17] "\r\\ | 0  basisOfRecord                 \033[K\r"                                  
      [18] "\rv | 0 v | basisOfRecord                  [1s]\033[K\r"                           
      [19] "\n"                                                                                
      [20] "\r\\ | 0  coordinateUncertaintyInMeters \033[K\r"                                  
      [21] "\rv | 0 v | coordinateUncertaintyInMeters  [1s]\033[K\r"                           
      [22] "\n"                                                                                
      [23] "\r\\ | 0  geodeticDatum                 \033[K\r"                                  
      [24] "\rv | 0 v | geodeticDatum                  [1s]\033[K\r"                           
      [25] "\n"                                                                                
      [26] "== Results =====================================================================\n"
      [27] "i Checking Darwin Core compliance\n"                                               

# check_dataset handles `set_measurements()`

    Code
      msgs
    Output
       [1] "ℹ Testing data\n"                                                                  
       [2] "\r⠙ | 0  measurementValue \033[K\r"                                                
       [3] "\r✔ | 0 ✔ | measurementValue  [1s]\033[K\r"                                        
       [4] "\n"                                                                                
       [5] "\r⠙ | 0  measurementID    \033[K\r"                                                
       [6] "\r✔ | 0 ✔ | measurementID     [1s]\033[K\r"                                        
       [7] "\n"                                                                                
       [8] "\r⠙ | 0  measurementUnit  \033[K\r"                                                
       [9] "\r✔ | 0 ✔ | measurementUnit   [1s]\033[K\r"                                        
      [10] "\n"                                                                                
      [11] "\r⠙ | 0  measurementType  \033[K\r"                                                
      [12] "\r✔ | 0 ✔ | measurementType   [1s]\033[K\r"                                        
      [13] "\n"                                                                                
      [14] "══ Results ═════════════════════════════════════════════════════════════════════\n"
      [15] "ℹ Checking Darwin Core compliance\n"                                               
      [16] "ℹ Use `suggest_workflow()` to see more information.\n"                             

