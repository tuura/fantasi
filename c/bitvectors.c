// Simple Bit vector Library
//
// Uses arrays of 64-bit words as the underlying data structure

#include <stdlib.h>


typedef unsigned long long word_t; // 64-bit type


struct bv_t {
	word_t* words;
	uint nwords;
}; // bit vector type


ushort rand16() {

	// Generate 16-bit random number.

	return rand() % (1<<16);
}


word_t rand64() {

	// Generate 64-bit random number.

	return rand16()
		+ rand16() * (1ULL<<16)
		+ rand16() * (1ULL<<32)
		+ rand16() * (1ULL<<48)
	;
}


int get_bit(struct bv_t bv, int index) {

	// Return bit at given index.

	uint wrd_ind = index / 64;
	uint bit_ind = index % 64;
	word_t mask = (word_t) 1ULL << bit_ind;
	return (bv.words[wrd_ind] & mask) > 0;
}


uint count_bits(struct bv_t bv) {

	// Return number of high bits.

	uint count = 0;
	uint nbits = bv.nwords * 64;

	for (int i=0; i<nbits; i++)
		count += get_bit(bv, i);

	return count;
}


struct bv_t create_bv(long nwords) {

	// Initialize bit vector.

	struct bv_t bv;
	bv.nwords = nwords;
	bv.words = (word_t*) malloc(sizeof(word_t) * nwords);

	return bv;

}

void show_bv(struct bv_t bv, const char* prefix) {

	// Print bit vector contents.

	int nbytes = bv.nwords * sizeof(word_t);

	char* bv_bytes = (char*) bv.words;

	printf("%s: ", prefix);

	for (int i=0; i<nbytes; i++)
		printf("%02x%s", bv_bytes[i] & 0xFF, i < nbytes - 1 ? "," : "");

	printf(" [words = %d, high bits = %d]\n", bv.nwords, count_bits(bv));

}

void assign_bit(struct bv_t bv, int index, int bval) {

	// Assign individual bit.

	uint wrd_ind = index / 64;
	uint bit_ind = index % 64;

	word_t mask = (word_t) 1ULL << bit_ind;

	if (bval)
		bv.words[wrd_ind] |= mask;
	else
		bv.words[wrd_ind] &= ~mask;

}

void fill_bv(struct bv_t bv, int bval) {

	// Fill bit vector with given bit value.

	uint nbits = bv.nwords * 64;

	for (int i=0; i<nbits; i++)
		assign_bit(bv, i, bval);

}

void rand_bv(struct bv_t bv) {

	// Fill bit vector with random values.

	for (int i=0; i<bv.nwords; i++)
		bv.words[i] = rand64();
}

void crossover_bv_odd_event(
		struct bv_t src1,
		struct bv_t src2,
		struct bv_t dst1,
		struct bv_t dst2
	) {

	// Crossover two source bit vectors into two destination bit vectors.

	// Note: this function does not preserve the number of 0 bits (i.e.
	// disabled nodes).

	uint nwords = src1.nwords; // note: all args must have same nwords

	for (int i=0; i<nwords; i++) {

		word_t mask = rand64();

		dst1.words[i] = (src1.words[i] &  mask) + (src2.words[i] & ~mask);
		dst2.words[i] = (src1.words[i] & ~mask) + (src2.words[i] &  mask);
	}
}

void crossover_equal_naive(
		struct bv_t src1,
		struct bv_t src2,
		struct bv_t dst1,
		struct bv_t dst2
	) {

	// Crossover two source bit vectors into two destination bit vectors.

	// Iterates over bits and is therefore inefficient, included here for
	// testing.

	uint nwords = src1.nwords; // note: all args must have same nwords
	uint nbits = nwords * 64;
	uint turn = 0;

	fill_bv(dst1, 0);
	fill_bv(dst2, 0);

	for (int i=0; i<nbits; i++) {

		uint b1 = get_bit(src1, i);
		uint b2 = get_bit(src2, i);

		if (b1 & b2) {
			assign_bit(dst1, i, 1);
			assign_bit(dst2, i, 1);
		} else if (b1 | b2) {

			if (turn)
				assign_bit(dst1, i, 1);
			else
				assign_bit(dst2, i, 1);

			turn = 1 - turn;
		}

	}

}

void copy_bv(struct bv_t src, struct bv_t dst) {

	for (int i=0; i<src.nwords; i++)
		dst.words[i] = src.words[i];
}

void show_bin(word_t w) {

	for (int i=0; i<64; i++) {
		int ind = 63 - i;
		word_t mask = (word_t) 1ULL << ind;
		int bval = (w & mask) > 0;
		printf("%d", bval);
	}
	// printf("\n");
}


word_t split_word(word_t inp) {

	word_t x = inp;
	word_t y = 0;

	while (x) {
		word_t mask = x & -x;
		y ^= mask;
		x ^= mask;
		x ^= (x & -x);
	}

	return y;
}


void show_bin_bv(struct bv_t bv, const char* prefix) {

	printf("%s : ", prefix);

	for (int i=0; i<bv.nwords; i++)
		show_bin(bv.words[i]);

	printf("\n");

}


void crossover_bv(
		struct bv_t p1,
		struct bv_t p2,
		struct bv_t c1,
		struct bv_t c2
	) {

	int turn = 0;

	for (int i=0; i<p1.nwords; i++) {

		word_t diff = p1.words[i] ^ p2.words[i];
		word_t x = diff;
		word_t y = 0;

		int odd_bits;

		while (x) {
			word_t mask = x & -x;
			y ^= mask;
			x ^= mask;
			odd_bits = x == 0;
			x ^= (x & -x);
		}

		if (odd_bits)
			turn = 1 - turn;

		word_t z = diff - y;
		word_t in = p1.words[i] & p2.words[i];

		c1.words[i] = in | (turn ? z : y);
		c2.words[i] = in | (turn ? y : z);
	}

}


void test_bv_3() {

	uint nwords = 3;

	struct bv_t p1 = create_bv(nwords);
	struct bv_t p2 = create_bv(nwords);

	rand_bv(p1);
	rand_bv(p2);

	struct bv_t c1 = create_bv(nwords);
	struct bv_t c2 = create_bv(nwords);

	crossover_bv(p1, p2, c1, c2);

	show_bin_bv(p1, "p1");
	show_bin_bv(p2, "p2");

	printf("=\n");

	show_bin_bv(c1, "c1");
	show_bin_bv(c2, "c2");

	printf("Bits in c1[] = %d\n", count_bits(c1));
	printf("Bits in c2[] = %d\n", count_bits(c2));

}

void test_bv_2() {

	struct bv_t bv1 = create_bv(2);
	struct bv_t bv2 = create_bv(2);

	struct bv_t re1 = create_bv(2);
	struct bv_t re2 = create_bv(2);

	rand_bv(bv1);
	rand_bv(bv2);

	for (int i=91; i<128; i++) {
		assign_bit(bv1, i, 0);
		assign_bit(bv2, i, 0);
	}

	show_bv(bv1, "bv1");
	show_bv(bv2, "bv2");

	crossover_bv(bv1, bv2, re1, re2);

	show_bv(re1, "re1");
	show_bv(re2, "re2");
}