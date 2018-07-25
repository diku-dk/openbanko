all: bins staticlib example

bins:
	cd banko-bins && cargo build --release
	cp banko-bins/target/release/encoder .
	cp banko-bins/target/release/decoder .

staticlib:
	cd banko-static && cargo build --release
	cp banko-static/target/release/libbanko_static.a .

example:
	gcc static_example.c -o static_example ./libbanko_static.a  -lpthread -ldl -lm

clean:
	cd banko-bins && cargo clean
	cd banko-lib && cargo clean
	cd banko-static && cargo clean
	rm -f encoder decoder libbanko_static.a static_example

.PHONY: all bins staticlib clean
