my_na_rm = function(x){
  if (is.numeric(x)){
    if (shapiro.test(x)$p.value > 0.05){
      print('Normal, using mean')
      x[is.na(x)] = mean(x, na.rm = T)
    } else {
      print('Not normal, using median')
      x[is.na(x)] = median(x, na.rm = T)
    }
    return(x)
  } else print('X is not numeric')
  
}