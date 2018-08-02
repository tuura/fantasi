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

char* temp1;
char* temp2;

struct individual child1;
struct individual child2;

node_t* roulette;
int roulette_ptr; // points to last item

// functions

void init_ga(int psize, int isize) {

	// Initialize globals

	pop_size = psize;
	ind_size = isize;

	if (disabled_nodes != NULL) free(disabled_nodes);

	if (child1.disabled != NULL) free(child1.disabled);
	if (child2.disabled != NULL) free(child2.disabled);

	if (roulette != NULL) free(roulette);
	if (temp1 != NULL) free(temp1);
	if (temp2 != NULL) free(temp2);

	disabled_nodes = (int*) malloc(sizeof(int) * ind_size);

	temp1 = (char*) malloc(sizeof(char) * nodes);
	temp2 = (char*) malloc(sizeof(char) * nodes);

	child1.disabled = malloc(sizeof(node_t) * ind_size);
	child2.disabled = malloc(sizeof(node_t) * ind_size);

	// Initialize roulette

	roulette = malloc(sizeof(node_t) * nodes);
	for (int i=0; i<nodes; i++) roulette[i] = i;
	roulette_ptr = nodes - 1;

}

node_t sample_roulette() {

	if (roulette_ptr < 0) return -1;

	int index = rand() % (roulette_ptr + 1);

	node_t result = roulette[index];

	// Swap index with last item and decrease roulette size.

	roulette[index] = roulette[roulette_ptr];

	roulette_ptr--;

	return result;

}

void reset_roulette() {
	roulette_ptr = nodes - 1;
}

// for quick sort
int comp_nodes (const void * a, const void * b) {
   return ( *(node_t*)b - *(node_t*)a );
}


void _eval_ind(struct individual *ind) {

	// Evaluate individual.

	qsort(ind->disabled, ind_size, sizeof(node_t), comp_nodes);

	reset_network();

	for (int i=0; i<ind_size; i++) disabled_nodes[i] = ind->disabled[i];

	qsort(disabled_nodes, ind_size, sizeof(int), cmpfunc);

	turnoff_nodes(disabled_nodes, ind_size);

	start_test();

	int sp = read_result();

	ind->fitness = avg_path(sp, ind_size);
}

void create_ga_population() {

	// Create population.

	if (pop != NULL) {

		for (int i=0; i<pop_size; i++)
			free(pop[i].disabled);

		free(pop);
	}

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

		reset_roulette();

		for (int j=0; j< ind_size; j++)
			pop[i].disabled[j] = sample_roulette();

		_eval_ind(pop + i);

	}

	// printf("Created %d individuals of %d nodes each.\n", pop_size, ind_size);
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

float get_ga_best() {

	// Return fitness of best individual.

	float best_fitness = pop[0].fitness;

	for (int i=1; i<pop_size; i++) {

		float fitness = pop[i].fitness;

		best_fitness = fitness > best_fitness ? fitness : best_fitness;

	}

	return best_fitness;

}

float get_ga_mean() {

	// Return mean fitness of population.

	float sum = 0;

	for (int i=1; i<pop_size; i++)
		sum += pop[i].fitness;

	return sum / pop_size;
}

float get_ga_min() {

	// Return min fitness of population.

	float min = pop[0].fitness;

	for (int i=1; i<pop_size; i++)
		min = pop[i].fitness < min ? pop[i].fitness : min;

	return min;
}

float get_ga_max() {

	// Return max fitness of population.

	float max = pop[0].fitness;

	for (int i=1; i<pop_size; i++)
		max = pop[i].fitness > max ? pop[i].fitness : max;

	return max;
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

	c1 = 0;
	c2 = 1;

	float rand_float = (float) rand() / (float) (RAND_MAX);  // random float in [0, 1]

	float fitness_diff = pop[c1].fitness - pop[c2].fitness;  // fitness(c1) - fitness(c2)

	int fittest = fitness_diff > 0 ? c1 : c2;
	int weakest = fitness_diff > 0 ? c2 : c1;

	int winner = rand_float < p ? fittest : weakest;

	return winner;

}

void crossover_shuffle(struct individual *p1, struct individual *p2) {

	// Shuffle contents of parents p1 and p2.

	for (int i=0; i<ind_size; i++) {

		if (rand() % 2) continue;

		node_t temp = p1->disabled[i];
		p1->disabled[i] = p2->disabled[i];
		p2->disabled[i] = temp;

	}

	_eval_ind(p1);
	_eval_ind(p2);

}


void crossover_etx(struct individual *p1, struct individual *p2) {

	// Subperformant crossover of two parents.

	// Uncompress parent disabled nodes into bit vectors.

	for (int i=0; i<nodes; i++) {
		temp1[i] = 0;
		temp2[i] = 0;
	}

	for (int i=0; i<ind_size; i++) {

		temp1[p1->disabled[i]] = 1;
		temp2[p2->disabled[i]] = 1;
	}

	// Scan through bitvectors and add nodes to children.

	int nc1 = 0; // number of nodes in child1
	int nc2 = 0; // number of nodes in child2

	for (int i=0; i<nodes; i++) {

		if (temp1[i] && temp2[i]) {

			child1.disabled[nc1++] = i;
			child2.disabled[nc2++] = i;

		} else if (temp1[i] || temp2[i]) {

			if (nc1 < nc2)
				child1.disabled[nc1++] = i;
			else
				child2.disabled[nc2++] = i;

		}

	}

	// printf("nc1 = %d\n", nc1);
	// printf("nc2 = %d\n", nc2);

	// Evaluate fitness of children.

	_eval_ind(&child1);
	_eval_ind(&child2);

	if (child1.fitness > p1->fitness) {

		p1->fitness = child1.fitness;

		for (int i=0; i<ind_size; i++)
			p1->disabled[i] = child1.disabled[i];

	} else if (child1.fitness > p2->fitness) {

		p2->fitness = child1.fitness;

		for (int i=0; i<ind_size; i++)
			p2->disabled[i] = child1.disabled[i];

	}

	if (child2.fitness > p2->fitness) {

		p2->fitness = child2.fitness;

		for (int i=0; i<ind_size; i++)
			p2->disabled[i] = child2.disabled[i];

	} else if (child2.fitness > p1->fitness) {

		p1->fitness = child2.fitness;

		for (int i=0; i<ind_size; i++)
			p1->disabled[i] = child2.disabled[i];

	}

	return;

	// Print children.

	printf("Child 1 (fitness %f):", child1.fitness);

	for (int j=0; j<ind_size; j++) printf("  %4d", child1.disabled[j]);

	printf("\n");

	printf("Child 2 (fitness %f):", child2.fitness);

	for (int j=0; j<ind_size; j++) printf("  %4d", child2.disabled[j]);

	printf("\n");

}

void step(int rounds) {

	float p = 0.7;  // selection parameter

	float mean_1 = get_ga_mean();

	int checkpoint = 100;  // number of rounds between outputs

	for (int i=0; i<rounds; i++) {

		int p1 = compete(p);
		int p2 = compete(p);

		crossover_etx(pop + p1, pop + p2);

		printf("p1 fitness = %f\n", pop[p1].fitness);

		float min = get_ga_min();
		float max = get_ga_max();
		float mean_2 = get_ga_mean();

		if (i % checkpoint == 0)
			printf("Round %5d, delta mean fitness = %f [min = %f, max = %f]\n", i, mean_2 - mean_1, min, max);
	}

	// float min = get_ga_min();
	// float max = get_ga_max();
	// float mean_2 = get_ga_mean();

	// printf("Delta mean fitness = %f [min = %f, max = %f]\n", mean_2 - mean_1, min, max);


}