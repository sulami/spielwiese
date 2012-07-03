def is_prime (i)
a = 2
b = 0
	while (a < i && b == 0)
		if (i % a == 0)
			b = i / a
			puts i.to_s+" = " + b.to_s + " x " + (i / b).to_s
		else
			a = a + 1
		end		
	end
	if b == 0
		puts "is prime"
	end
end

puts "Primetest"
puts "Type in a number:"
input = gets.to_i
is_prime(input)
