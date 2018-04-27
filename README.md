# standardize
Standardization implemented in scala

# How to
    
    val data = Vector(
      Vector(2d, 100d),
      Vector(10d, 200d),
      Vector(4d, 120d),
      Vector(6d, 130d)
    ) 
    val model = StandardScaler(withStd = true, withMean = true).fit(data)
    val transformed = model.transform(data)
