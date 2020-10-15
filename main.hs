
validPosition totalRows totalColumns targetRow targetColumn = 
    targetRow >= 0 &&
    targetRow < totalRows &&
    targetColumn >= 0 &&
    targetColumn < totalColumns

main = do
    print (validPosition 10 10 (-10) (-8))
