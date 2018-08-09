
void show_32bit(uint w, const char* postfix) {

	// Print binary representation of 32-bit number.

	for (int i=0; i<32; i++) {
		word_t mask = (word_t) 1ULL << (31 - i);
		printf("%d", (w & mask) > 0);
	}

	printf("%s", postfix);

}

void show_64bit(unsigned long long w, const char* postfix) {

	// Print binary representation of 64-bit number.

	uint hi = (w >> 32) & 0xffffffff; // 32-bit word (lower)
	uint lo = (w      ) & 0xffffffff; // 32-bit word (higher)

	show_32bit(hi, "");
	show_32bit(lo, "");

	printf("%s", postfix);

}

int find_zeros(unsigned long long y, uint* indices, uint shift) {

	// Print indices of low bits in a 64-bit number (y).
	//
	// Based on the bit hack in:
	//
	// https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightModLookup

	unsigned int v;
	int r;
	static const int Mod37BitPosition[] = {
	  32, 0, 1, 26, 2, 23, 27, 0, 3, 16, 24, 30, 28, 11, 0, 13, 4,
	  7, 17, 0, 25, 22, 31, 15, 29, 10, 12, 6, 0, 21, 14, 9, 5,
	  20, 8, 19, 18};

	int counter = 0; // indices counter

	uint y_hi = (y >> 32) & 0xffffffff; // 32-bit word (lower)
	uint y_lo = (y      ) & 0xffffffff; // 32-bit word (higher)

	uint pos;
	uint x;

	// Find indices of zeros in lower 32-bit word

	x = ~y_lo;
	pos = 0;

	while (1) {
		r = Mod37BitPosition[(-x & x) % 37];
		if (r == 32) break;
		x >>= r;
		x >>= 1;
		pos += r + 1;
		indices[counter++] = pos - 1 + shift;
	}

	// Find indices of zeros in higher 32-bit word

	x = ~y_hi;
	pos = 32;

	while (1) {
		r = Mod37BitPosition[(-x & x) % 37];
		if (r == 32) break;
		x >>= r;
		x >>= 1;
		pos += r + 1;
		indices[counter++] = pos - 1 + shift;
	}

	return counter;

}

void print_zeros(unsigned long long y) {

	// Print indices of zero bits in 64-bit number (y).

	uint indices[64];
	uint zero_count = find_zeros(y, indices, 0);

	// Print results

	show_64bit(y, "\n");

	for (int i=0; i<zero_count; i++)
		printf("%d%s", indices[i], i == zero_count-1 ? "\n" : ", ");

}