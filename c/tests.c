void show_bin(word_t w) {

	for (int i=0; i<64; i++) {
		int ind = 63 - i;
		word_t mask = (word_t) 1ULL << ind;
		int bval = (w & mask) > 0;
		printf("%d", bval);
	}
	printf("\n");
}

void test_bv_3() {

	word_t inp = 15;
	word_t x = inp;
	word_t y = 0;

	while (x) {
		word_t mask = x & -x;
		y ^= mask;
		x ^= mask;
		x ^= (x & -x);
	}

	show_bin(inp);
	show_bin(y);
	show_bin(inp-y);

}