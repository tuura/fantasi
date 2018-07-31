// globals

int pop_size = 10;  // number of individuals (solutions) in population
int ind_size = 10;  // number of nodes in an individual (solution)
int *pop;  // population

// functions

void init_ga_pop(int psize, int isize) {

	// Initialize population.

	pop_size = psize;
	ind_size = isize;

	int int_count = pop_size * ind_size;
	int bytes = sizeof(int) * int_count;

	pop = (int *) malloc(bytes);

	if (pop == NULL) {
		printf("Not enough memory.\n");
		return;
	}

	printf("Initializing %d integers ...\n", int_count);

	for (int i=0; i< int_count; i++)
		pop[i] = rand() % nodes;
}

float eval_ind(int ind) {

	// Evaluate individual.

	reset_network();

	int *disabled_nodes = (int*) malloc(sizeof(int) * ind_size);

	for (int i=0; i<ind_size; i++) disabled_nodes[i] = pop[ind * ind_size + i];

	qsort(disabled_nodes, ind_size, sizeof(int), cmpfunc);

	turnoff_nodes(disabled_nodes, ind_size);

	free(disabled_nodes);

	start_test();

	int sp = read_result();

	return avg_path(sp, ind_size);
}

void show_ga_pop() {

	// Print population table.

	printf("Population: %d individuals of %d nodes each.\n\n", pop_size, ind_size);

	for (int i=0; i<pop_size; i++) {

		printf("Individual %3d (fitness %f):", i, eval_ind(i));

		for (int j=0; j<ind_size; j++) printf("  %4d", pop[i * ind_size + j]);

		printf("\n");
	}
}

float get_ga_best() {

	// Return fitness of best individual.

	float best_fitness = eval_ind(0);

	for (int i=1; i<pop_size; i++) {

		float fitness = eval_ind(i);

		best_fitness = fitness > best_fitness ? fitness : best_fitness;

	}

	return best_fitness;

}