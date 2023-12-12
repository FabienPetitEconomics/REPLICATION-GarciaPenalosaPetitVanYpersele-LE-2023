# Load inflation data
inflation = read.csv(file.path(loc_data, "_clean", "_oecd", "inflation_uk.csv"))

# Global function that generates the dataframe
compute_deflator = function(ref_year){
  # Function
  compute_deflator0 = function(current_year, ref_year){
    
    if(ref_year == current_year){
      deflator = 1
    }
    
    if(ref_year < current_year){
      inf = inflation %>% 
        subset(year %in% c(ref_year:(current_year-1))) %>% 
        pull(inflation_rate)
      
      deflator = 1/prod(1+inf)
    }
    
    if(ref_year > current_year){
      inf = inflation %>% 
        subset(year %in% c(ref_year:(current_year-1))) %>% 
        pull(inflation_rate)
      
      deflator = prod(1+inf)
    }
    
    return(deflator)
    
  }
  # Compute deflator according to the reference year
  def = NULL
  yr = NULL
  ref = 1970
  for(i in c(1970:2018)){
    
    yr = c(yr, i)
    def = c(def, compute_deflator0(i, ref))
    
  }
  
  # Dataframe
  result = data.frame(year = yr, deflator = def)
  return(result)
}