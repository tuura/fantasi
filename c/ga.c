// globals

#include "crossover.c"
#include "roulette.c"

typedef ushort node_t;

struct individual {
	float fitness;
	node_t *disabled; // array of disabled nodes
};

struct individual *pop; // population

// functions

void show_individual(struct individual *ind, char* name) {

	printf("%s (fitness %f):", name, ind->fitness);

	for (int j=0; j<ind_size; j++)
		printf("  %4d", ind->disabled[j]);

	printf("\n");
}

// for quick sort
int comp_nodes (const void * a, const void * b) {
	return ( *(node_t*)b - *(node_t*)a );
}


// turn off the nodes in the array
void turnoff_nodes_b(node_t *nodes_to_disable, int n){

	int i = 0;
	int c = nodes - 1;

	for(i=0; i<n; i++) {
		insert_ones(c - nodes_to_disable[i]);
		insert_zero();
		c = nodes_to_disable[i] - 1;
	}

	if (c)
		insert_ones(c + 1);
	else
		insert_ones(1);
}


float eval_ind(struct individual *ind) {

	reset_network();
	turnoff_nodes_b(ind->disabled, ind_size);
	start_test();
	int sp = read_result();
	return avg_path(sp, ind_size);
}


void show_ga_pop() {

	// Print population table.

	printf("Population: %d individuals of %d nodes each.\n\n", pop_size, ind_size);

	for (int i=0; i<pop_size; i++) {

		printf("Individual %3d (fitness %f):", i, pop[i].fitness);

		for (int j=0; j<ind_size; j++) printf("  %4d", pop[i].disabled[j]);

			printf("\n");
	}
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

void point_mutate(struct individual* ind) {

	int new_node = -1;

	while (new_node == -1) {

		new_node = rand() % nodes;

		for (int i=0; i<ind_size; i++) {

			if (ind->disabled[i] == new_node) {
				new_node = -1;
				break;
			}
		}

	}

	int pos = rand() % ind_size;

	ind->disabled[pos] = new_node;

	qsort(ind->disabled, ind_size, sizeof(node_t), comp_nodes);
}

void init_ind(struct ind_t* ind, int ind_size) {

	// Initialize individual data structures.

	ind->fitness = 0;
	ind->disabled = malloc(sizeof(node_t) * ind_size);

	if (ind->disabled == NULL)
		printf("Error, out of memory\n");
}

void copy_ind(struct ind_t* src, struct ind_t* dst, int ind_size) {

	// Copy individual.

	dst->fitness = src->fitness;

	for (int i=0; i<ind_size; i++)
		dst->disabled[i] = src->disabled[i];

}

ind_t* create_pop(int pop_size) {

	// Create population.

	struct ind_t* pop = malloc(sizeof(struct ind_t) * pop_size);

	if (pop == NULL) {
		printf("Not enough memory.\n");
		return;
	}

	for (int i=0; i<pop_size; i++) {

		init_ind(pop + i);
		reset_roulette();

		for (int j=0; j< ind_size; j++)
			pop[i].disabled[j] = sample_roulette();;

		pop[i].fitness = eval_ind(pop + i);
	}

	return pop;

}


void run_ga(int pop_size, int ind_size, int nodes) {

	uint rounds = 1000;
	uint checkpoint = 100; // result printout interval

	float p = 0.7; // selection parameter

	// Prepare population

	ind_t* pop = create_pop(pop_size, ind_size);

	// Prepare children

	struct ind_t child1;
	struct ind_t child2;

	init_ind(child1);
	init_ind(child2);

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

	for (int r=0; r<rounds; r++) {

		// Select parents by tournament matches

		struct ind_t *p1 = pop + compete(p);
		struct ind_t *p2 = pop + compete(p);

		// Generate children by crossing over parents

		crossover_etx(
			p1->bv,
			p2->bv,
			c1->bv,
			c2->bv,
			temp1,
			temp2,
			ind_size,
			nodes
		);

		// Mutate children

		point_mutate(c1);
		point_mutate(c2);

		// Evaluate fitness of children.

		c1->fitness = eval_ind(c1);
		c2->fitness = eval_ind(c2);

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

	// TODO: dealloc population disabled nodes
	// TODO: deallow population

	free(temp1);
	free(temp2);

}
