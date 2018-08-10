#include "roulette.c"

typedef ushort node_t;

struct ind_t {
	float fitness;
	node_t *disabled; // array of disabled nodes
};

float evaluate(struct ind_t *ind, int ind_size, int nodes); // must be provided by user

void show_individual(struct ind_t *ind, const char* name, int ind_size) {

	printf("%s (fitness %f): ", name, ind->fitness);

	for (int j=0; j<ind_size; j++)
		printf("%d%s", ind->disabled[j], j == ind_size-1 ? "\n" : ", ");

}

int comp_nodes(const void *a, const void *b) {

	// Quick sort compare.

	return ( *(node_t*)b - *(node_t*)a );
}

int compete(struct ind_t* pop, float p, int pop_size) {

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

void bubble_new_element(node_t *arr, int n, int pos) {

	// Given a (previously) sorted array in which a new element has been
	// inserted, move the element up/down as necessary until the array is
	// sorted again.

	// `n` is the length of the array and `pos` is the index of the inserted
	// `element.

	if (pos > 0 && (arr[pos] > arr[pos-1])) {

		// Move up

		while (1) {
			if (pos < 1) return;
			if (arr[pos] < arr[pos-1]) return;
			node_t temp = arr[pos-1];
			arr[pos-1] = arr[pos];
			arr[pos] = temp;
			pos--;
		}

	} else if (pos < n-1 && (arr[pos] < arr[pos+1])) {

		// Move down

		while (1) {
			if (pos > n-2) return;
			if (arr[pos] > arr[pos+1]) return;
			node_t temp = arr[pos+1];
			arr[pos+1] = arr[pos];
			arr[pos] = temp;
			pos++;
		}

	}

}

void point_mutate(struct ind_t* ind, int ind_size, int nodes, char* temp1) {

	// Point mutate an individual.

	for (int i=0; i<nodes; i++)
	    temp1[i] = 0;

	for (int i=0; i<ind_size; i++)
		temp1[ind->disabled[i]] = 1;

	while (1) {

		node_t new_node = rand() % nodes;

		if (temp1[new_node] == 1)
			continue;

		int pos = rand() % ind_size;

		ind->disabled[pos] = new_node;

		bubble_new_element(ind->disabled, ind_size, pos);

		break;
	}

}

void init_ind(struct ind_t* ind, int ind_size) {

	// Initialize individual data structures.

	ind->fitness = 0;
	ind->disabled = (node_t*) malloc(sizeof(node_t) * ind_size);

	if (ind->disabled == NULL)
		printf("Error, out of memory\n");
}

void copy_ind(struct ind_t* src, struct ind_t* dst, int ind_size) {

	// Copy individual.

	dst->fitness = src->fitness;

	for (int i=0; i<ind_size; i++)
		dst->disabled[i] = src->disabled[i];

}

struct ind_t* create_pop(int pop_size, int ind_size, int nodes) {

	// Create population.

	struct ind_t* pop = (struct ind_t*) malloc(sizeof(struct ind_t) * pop_size);

	if (pop == NULL)
		return NULL;

	init_roulette(nodes);

	for (int i=0; i<pop_size; i++) {

		init_ind(pop + i, ind_size);
		reset_roulette();

		for (int j=0; j< ind_size; j++)
			pop[i].disabled[j] = sample_roulette();

		qsort(pop[i].disabled, ind_size, sizeof(node_t), comp_nodes);

		pop[i].fitness = evaluate(pop + i, ind_size, nodes);
	}

	return pop;

}

void free_pop(struct ind_t* pop, int pop_size) {

	// Free memory used by population.

	for (int i=0; i<pop_size; i++)
		free(pop[i].disabled);

	free(pop);

}

void crossover(
        node_t *p1,    // parent 1
        node_t *p2,    // parent 2
        node_t *c1,    // child 1
        node_t *c2,    // child 2
        char *temp1,   // temporary buffer (length = n)
        char *temp2,   // temporary buffer (length = n)
        uint n,        // number of items in individual
        uint item_max  // max item value
    ) {

    // Subperformant crossover of two parents.

    // Uncompress parent disabled nodes into bit vectors.

    for (int i=0; i<item_max; i++) {
        temp1[i] = 0;
        temp2[i] = 0;
    }

    for (int i=0; i<n; i++) {
        temp1[p1[i]] = 1;
        temp2[p2[i]] = 1;
    }

    // Scan through bitvectors and add nodes to children.

    int nc1 = 0; // number of nodes in child1
    int nc2 = 0; // number of nodes in child2

    for (int i=item_max-1; i>=0; i--) {

        if ((temp1[i] == 1) && (temp2[i] == 1)) {

            c1[nc1++] = i;
            c2[nc2++] = i;

        } else if ((temp1[i] == 1) || (temp2[i] == 1)) {

            if (nc1 < nc2) c1[nc1++] = i;
            else           c2[nc2++] = i;

        }

    }

}

struct ind_t* run_ga(int pop_size, int ind_size, int nodes, int rounds) {

	uint checkpoint = 100; // result printout interval

	float p = 0.7; // selection parameter

	// Prepare population

	struct ind_t* pop = create_pop(pop_size, ind_size, nodes);

	if (pop == NULL) {
		printf("Cannot create population, insufficient memory.\n");
		return NULL;
	}

	// Prepare children

	struct ind_t child1;
	struct ind_t child2;

	init_ind(&child1, ind_size);
	init_ind(&child2, ind_size);

	struct ind_t* c1 = &child1;
	struct ind_t* c2 = &child2;

	// Prepare temporary buffers

	char *temp1 = (char*) malloc(sizeof(char) * nodes);
	char *temp2 = (char*) malloc(sizeof(char) * nodes);

	for (int i=0; i<nodes; i++) {
		temp1[i] = 0;
		temp2[i] = 0;
	}

	// Start GA loop

	struct ind_t* top = pop; // best solution

	for (int r=0; r<rounds; r++) {

		// Select parents by tournament matches

		struct ind_t *p1 = pop + compete(pop, p, pop_size);
		struct ind_t *p2 = pop + compete(pop, p, pop_size);

		// Generate children by crossing over parents

		crossover(
			p1->disabled,
			p2->disabled,
			c1->disabled,
			c2->disabled,
			temp1,
			temp2,
			ind_size,
			nodes
		);

		// Mutate children

		point_mutate(c1, ind_size, nodes, temp1);
		point_mutate(c2, ind_size, nodes, temp1);

		// Evaluate fitness of children.

		c1->fitness = evaluate(c1, ind_size, nodes);
		c2->fitness = evaluate(c2, ind_size, nodes);

		// Overwrite parents with (better) children

		if (c1->fitness > p1->fitness) copy_ind(c1, p1, ind_size);
		if (c1->fitness > p2->fitness) copy_ind(c1, p2, ind_size);
		if (c2->fitness > p1->fitness) copy_ind(c2, p1, ind_size);
		if (c2->fitness > p2->fitness) copy_ind(c2, p2, ind_size);

		// Get updated population stats

		float min = pop[0].fitness;
		float max = pop[0].fitness;
		float sum = pop[0].fitness;

		for (int i=1; i<pop_size; i++) {
			float ft = pop[i].fitness;
			top = ft>max ? pop+i : top;
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

	free_pop(pop, pop_size);
	free(temp1);
	free(temp2);

	return top;

}
