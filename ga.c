// globals

int pop_size;  // number of individuals (solutions) in population
int ind_size;  // number of nodes in an individual (solution)

typedef ushort node_t;

struct individual {
	float fitness;
	node_t *disabled; // array of disabled nodes
};

struct individual *pop; // population

int *disabled_nodes;

// functions

void init_ga(int psize, int isize) {

	// Initialize globals

	pop_size = psize;
	ind_size = isize;

	if (disabled_nodes != NULL)
		free(disabled_nodes);

	disabled_nodes = (int*) malloc(sizeof(int) * ind_size);

}

void create_ga_population() {

	// Create population.

	if (pop != NULL)
		free(pop);

	pop = malloc(sizeof(struct individual) * pop_size);

	if (pop == NULL) {
		printf("Not enough memory.\n");
		return;
	}

	for (int i=0; i<pop_size; i++) {

		pop[i].disabled = malloc(sizeof(node_t) * ind_size);

		if (pop[i].disabled == NULL) {
			printf("Not enough memory.\n");
			return;
		}

		for (int j=0; j< ind_size; j++)
			pop[i].disabled[j] = rand() % nodes;
	}

	printf("Created %d individuals of %d nodes each.\n", pop_size, ind_size);
}

float _eval_ind(struct individual ind) {

	// Evaluate individual.

	reset_network();

	// int *disabled_nodes = (int*) malloc(sizeof(int) * ind_size);

	for (int i=0; i<ind_size; i++) disabled_nodes[i] = ind.disabled[i];

	qsort(disabled_nodes, ind_size, sizeof(int), cmpfunc);

	turnoff_nodes(disabled_nodes, ind_size);

	// free(disabled_nodes);

	start_test();

	int sp = read_result();

	return avg_path(sp, ind_size);
}

void eval_population() {

	for (int i=0; i<pop_size; i++) pop[i].fitness = _eval_ind(pop[i]);
}

void show_ga_pop() {

	// Print population table.

	eval_population();

	printf("Population: %d individuals of %d nodes each.\n\n", pop_size, ind_size);

	for (int i=0; i<pop_size; i++) {

		printf("Individual %3d (fitness %f):", i, pop[i].fitness);

		for (int j=0; j<ind_size; j++) printf("  %4d", pop[i].disabled[j]);

		printf("\n");
	}
}

float get_ga_best() {

	// Return fitness of best individual.

	eval_population();

	float best_fitness = pop[0].fitness;

	for (int i=1; i<pop_size; i++) {

		float fitness = pop[i].fitness;

		best_fitness = fitness > best_fitness ? fitness : best_fitness;

	}

	return best_fitness;

}

void mate(int p1, int p2) {

	// Shuffle contents of parents p1 and p2.

	node_t* dis1 = pop[p1].disabled;
	node_t* dis2 = pop[p2].disabled;

	for (int i=0; i<ind_size; i++) {

		if (rand() % 2) continue;

		node_t temp = dis1[i]; dis1[i] = dis2[i]; dis2[i] = temp; // Swap

	}

}

float cmpIndividuals(const void* ind1, const void* ind2) {

	// Return fitness difference between two individuals;

	float f1 = ((struct individual*) ind1)->fitness;
	float f2 = ((struct individual*) ind2)->fitness;

	return f2 - f1;
}

void qsort_pop() {

	// Quicksort population by fitness.

	qsort(pop, pop_size, sizeof(struct individual), cmpIndividuals);

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