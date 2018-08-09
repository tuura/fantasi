// Roulette: functions for sampling without replacement

#include <stdlib.h>

int *roulette;
int roulette_ptr; // points to last item
int roulette_size;

void reset_roulette() {

	roulette_ptr = roulette_size - 1;
}

void init_roulette(int size) {

	if (roulette != NULL)
		free(roulette);

	roulette = (int*) malloc(sizeof(int) * size);
	roulette_size = size;

	for (int i=0; i<size; i++)
		roulette[i] = i;

	reset_roulette();
}

int sample_roulette() {

	if (roulette_ptr < 0) {
		printf("Tried to sample empty roulette\n");
		return -1;
	}

	int index = rand() % (roulette_ptr + 1);

	int result = roulette[index];

	// Swap index with last item and decrease roulette size.

	roulette[index] = roulette[roulette_ptr];

	roulette[roulette_ptr] = result;

	roulette_ptr--;

	return result;
}
