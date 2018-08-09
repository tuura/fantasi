#include "roulette.c"
#include "bitvectors.c"
#include "bithacks.c"

int pop_size = 0;
int ind_size = 0;

struct ind_t {
	float fitness;
	struct bv_t bv;
};

struct ind_t *pop; // population

uint divide_ceil(uint x, uint y) {

	// Calculate ceil(x/y).

	return x / y + (x % y != 0);
}


void init_ind(struct ind_t* ind, uint nodes, int isize) {

	// Initialize individual data structure.

	uint nwords = divide_ceil(nodes, 64);
	ind->fitness = 0;
	ind->bv = create_bv(nwords);
	fill_bv(ind->bv, 0);
}

float eval_dummy(struct ind_t ind) {

	// Evaluate individual (dummy).

	float fitness = 0;
	uint nbits = ind.bv.nwords * 64 / 8;
	uint last_high_bit = -1;

	for (int i=0; i<nbits; i++) {
		// if (get_bit(ind.bv, i)) {
			// uint distance = i - last_high_bit;
		fitness += 0.0001;
			// fitness += 1.0 / distance;
			// last_high_bit = i;
		// }
	}

	return fitness;
}

void create_population(int psize, int isize) {

	// Initialize population.

	if (pop_size > 0) {
		printf("Re-creation of population is not support atm.\n");
		return;
	}

	pop_size = psize;
	ind_size = isize;

	uint nodes = 1628; // network nodes

	pop = (struct ind_t*) malloc(sizeof(struct ind_t) * pop_size);

	init_roulette(nodes);

	for (int i=0; i<pop_size; i++) {

		init_ind(pop + i, nodes, isize);

		reset_roulette();

		for (int j=0; j<isize; j++)
			assign_bit(pop[i].bv, sample_roulette(), 1);

		pop[i].fitness = eval_dummy(pop[i]);

	}

}

void show_ind(struct ind_t ind, const char* prefix) {

	// Print individual.

	printf("%s [fitness = %1.2f, high bits = %d]\n",
		prefix, ind.fitness, count_bits(ind.bv));
}

void show_pop() {

	// Print population.

	for (int i=0; i<pop_size; i++)
		printf("Individual %d [fitness = %1.2f, high bits = %d]\n",
			i, pop[i].fitness, count_bits(pop[i].bv));

}

int compete(float p) {

	// Run a selection tournment match.

	// Contestants are two individuals that are randomly selected from the
	// population. Returns index of winner.

	// Based on https://watchmaker.uncommons.org/manual/ch03s04.html

	int c1 = rand() % pop_size;  // contestant 1
	int c2 = rand() % pop_size;  // contestant 2

	float rand_float = (float) rand() / (float) (RAND_MAX);  // random float in [0, 1]

	float fitness_diff = pop[c1].fitness - pop[c2].fitness;  // fitness(c1) - fitness(c2)

	int fittest = fitness_diff > 0 ? c1 : c2;
	int weakest = fitness_diff > 0 ? c2 : c1;

	int winner = rand_float < p ? fittest : weakest;

	return winner;

}

void copy_ind(struct ind_t* src, struct ind_t* dst) {

	// Copy individual.

	dst->fitness = src->fitness;
	copy_bv(src->bv, dst->bv);
}

// turn off the nodes in the array

// void insert_ones(int n) {
// 	printf("Inserted %d ones\n", n);
// }

// void insert_zero() {
// 	printf("Inserted zero\n");
// }

// void test_insert(int *nodes_to_disable, int n){
// 	int i = 0;
// 	int nodes = 100;
// 	int c = nodes-1;

// 	for(i=0; i<n; i++) {
// 		insert_ones(c-nodes_to_disable[i]);
// 		insert_zero();
// 		c = nodes_to_disable[i]-1;
// 	}

// 	if(c) {
// 		insert_ones(c+1);
// 	} else {
// 		insert_ones(1);
// 	}
// }


void run_ga() {

	print_zeros(0); return;

	// int arr[] = {99};

	// test_insert(arr, 3);

	// return;

	// test_bv_3(); return;

	create_population(40, 680);

	uint nodes = 1628; // network nodes
	uint rounds = 1000;

	uint checkpoint = 100;

	float p = 0.7;

	struct ind_t child1;
	struct ind_t child2;

	struct ind_t* c1 = &child1;
	struct ind_t* c2 = &child2;

	init_ind(c1, nodes, ind_size);
	init_ind(c2, nodes, ind_size);

	int ind = 0; // index of best individual

	for (int r=0; r<rounds; r++) {

		// Select parents by tournament matches

		struct ind_t *p1 = pop + compete(p);
		struct ind_t *p2 = pop + compete(p);

		// Generate children by crossing over parents

		crossover_bv(
			p1->bv,
			p2->bv,
			c1->bv,
			c2->bv
		);

		// Evaluate children

		c1->fitness = eval_dummy(*c1);
		c2->fitness = eval_dummy(*c2);

		// Overwrite parents with (better) children

		if (c1->fitness > p1->fitness)
			copy_ind(c1, p1);

		if (c1->fitness > p2->fitness)
			copy_ind(c1, p2);

		if (c2->fitness > p1->fitness)
			copy_ind(c2, p1);

		if (c2->fitness > p2->fitness)
			copy_ind(c2, p2);

		// Get stats of updated population.

		float min = pop[0].fitness;
		float max = pop[0].fitness;
		float sum = pop[0].fitness;

		for (int i=1; i<pop_size; i++) {
			float ft = pop[i].fitness;
			ind = ft>max ? i : ind;
			max = ft>max ? ft : max;
			min = ft<min ? ft : min;
			sum = sum + ft;
		}

		// Print progress

		if (r % checkpoint)
			continue;

		printf("Round %d: mean %1.2f [%1.2f, %1.2f]\n",
			r, sum / pop_size, min, max);

	}

	printf("Best individual = %d\n", ind);

	show_bv(pop[ind].bv, "Best bits");

}
