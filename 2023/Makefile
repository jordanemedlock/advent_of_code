
day_1/main.o: day_1/main.cpp
	clang++ day_1/main.cpp -o day_1/main.o
	
day-1-%: day_1/main.o
	cat day_1/input.txt | ./day_1/main.o --part-$*

day-2-%: 
	cd day_2; \
		cargo run --quiet -- --part-$*

day-3-%:
	cd day_3; \
		php main.php --part-$*