type Name = String
type Color = String

showInfos :: Name -> Color -> String
showInfos name color = "Name: " ++ name ++ ", Color: " ++ color

name :: Name
name = "Robin"
color :: Color
color = "Green"
main = putStrLn $ showInfos name color

