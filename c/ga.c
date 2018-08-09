// globals

typedef ushort cross_item_t;

#include "crossover.c"

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

void show_individual(struct individual *ind, char* name) {

	printf("%s (fitness %f):", name, ind->fitness);

	for (int j=0; j<ind_size; j++)
		printf("  %4d", ind->disabled[j]);

	printf("\n");
}

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

	for (int i=0; i<nodes; i++) {
		temp1[i] = 0;
		temp2[i] = 0;
	}

	child1.disabled = malloc(sizeof(node_t) * ind_size);
	child2.disabled = malloc(sizeof(node_t) * ind_size);

	// Initialize roulette

	roulette = malloc(sizeof(node_t) * nodes);
	for (int i=0; i<nodes; i++) roulette[i] = i;
		roulette_ptr = nodes - 1;

}

node_t sample_roulette() {

	if (roulette_ptr < 0) {
		printf("Tried to sample empty roulette\n");
		return -1;
	}

	int index = rand() % (roulette_ptr + 1);

	node_t result = roulette[index];

	// Swap index with last item and decrease roulette size.

	roulette[index] = roulette[roulette_ptr];

	roulette[roulette_ptr] = result;

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


// turn off the nodes in the array
void turnoff_nodes_b(node_t *nodes_to_disable, int n){
	int i = 0;
	int c = nodes-1;

	for(i=0; i<n; i++) {
		insert_ones(c-nodes_to_disable[i]);
		insert_zero();
		c = nodes_to_disable[i]-1;
	}

	if(c) {
		insert_ones(c+1);
	} else {
		insert_ones(1);
	}
}


void _eval_sorted_ind(struct individual *ind) {

	reset_network();

	// for (int i=0; i<ind_size; i++) disabled_nodes[i] = ind->disabled[i];

	turnoff_nodes_b(ind->disabled, ind_size);

	start_test();

	int sp = read_result();

	ind->fitness = avg_path(sp, ind_size);
}


void _eval_ind(struct individual *ind) {

	// Evaluate individual.

	qsort(ind->disabled, ind_size, sizeof(node_t), comp_nodes);

	_eval_sorted_ind(ind);

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

		for (int j=0; j< ind_size; j++) {

			// printf("Roulette: ");

			// for (int k=0; k<nodes; k++) {
			// 	printf("%d%s", roulette[k], k == roulette_ptr ? ",": " ");
			// }

			node_t popped = sample_roulette();

			// printf(" (popped %d)\n", popped);

			pop[i].disabled[j] = popped;
		}

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

int compare_child_parent(struct individual *parent) {

	struct individual better_child = child1.fitness > child2.fitness ? child1 : child2;

	if (better_child.fitness > parent->fitness) {

		// printf("Found better child\n");

		parent->fitness = better_child.fitness;

		for (int i=0; i<ind_size; i++)
			parent->disabled[i] = better_child.disabled[i];

		return 1;

	} else {

		return 0;

	}

}

int crossover_etx_fast(struct individual *p1, struct individual *p2) {

	crossover_sorted(
		p1->disabled,
		p2->disabled,
		child1.disabled,
		child2.disabled,
		ind_size
	);

	// point_mutate_sorted(child1.disabled, ind_size, nodes-1);
	// point_mutate_sorted(child2.disabled, ind_size, nodes-1);

	point_mutate(&child1);
	point_mutate(&child2);

	_eval_sorted_ind(&child1);
	_eval_sorted_ind(&child2);

	// printf("p1 = %f, p2 = %f, c1 = %f, c2 = %f\n", p1->fitness, p2->fitness, child1.fitness, child2.fitness);

	int result1 = compare_child_parent(p1);
	int result2 = compare_child_parent(p2);

	return result1 + result2;

}

int crossover_etx(struct individual *p1, struct individual *p2) {

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

	for (int i=nodes-1; i>=0; i--) {

		if ((temp1[i] == 1) && (temp2[i] == 1)) {

			child1.disabled[nc1++] = i;
			child2.disabled[nc2++] = i;

		} else if ((temp1[i] == 1) || (temp2[i] == 1)) {

			if (nc1 < nc2) child1.disabled[nc1++] = i;
			else 		   child2.disabled[nc2++] = i;

		}

	}

	check_sorted(child1.disabled, ind_size);
	check_sorted(child2.disabled, ind_size);

	// Note: generated disabled nodes of child1/child2 are ordered

	for (int i=0; i<1; i++) {
		// point_mutate(&child1);
		// point_mutate(&child2);
		point_mutate_sorted(child1.disabled, ind_size, nodes-1);
		point_mutate_sorted(child2.disabled, ind_size, nodes-1);
	}

	// Evaluate fitness of children.

	_eval_sorted_ind(&child1);
	_eval_sorted_ind(&child2);

	compare_child_parent(p1);
	compare_child_parent(p2);

	return 0;

}

void benchmark(long rounds) {

	int checkpoint = 100;  // number of rounds between outputs

	for (int i=0; i<rounds; i++) {

		_eval_sorted_ind(pop);

		if (i % checkpoint == 0)
			printf("Round %5d\n", i);
	}

}

void check_population() {

	// Check that all individuals are sorted.

	for (int i = 0; i<pop_size; i++) {

		node_t *arr = pop[i].disabled;

		for (int j=0; j<ind_size-1; j++) {

		    if (arr[j] < arr[j+1]) {

		        printf("Sort error (ind %d): node[%d] = %d but node[%d] = %d\n",
		            i, j, arr[j], j+1, arr[j+1]);

		    }
		}

	}

}

void step(long rounds) {

	float p = 0.7;  // selection parameter

	float mean_1 = get_ga_mean();

	int checkpoint = 100;  // number of rounds between outputs

	int total_overwrites = 0;

	float min = 0, max = 0, mean_2 = 0;

	for (int i=0; i<rounds; i++) {

		int p1 = compete(p);
		int p2 = compete(p);

		int overwrites = crossover_etx_fast(pop + p1, pop + p2);

		total_overwrites += overwrites;

		min = get_ga_min();
		max = get_ga_max();
		mean_2 = get_ga_mean();

		if (i % checkpoint == 0)
			printf("Round %5d, delta mean fitness = %f [min = %f, max = %f, overwrites = %d]\n", i, mean_2 - mean_1, min, max, total_overwrites);
	}

}