PI = 3.142
puts "Dieses kleine Programm berechnet den Flaecheninhalt eines Kreises!"
puts "Bitte nun den Radius eingeben:"
radius = gets.to_f
flaeche = radius**2 * PI
puts "Der Flaecheninhalt betraegt: "+flaeche.to_s
