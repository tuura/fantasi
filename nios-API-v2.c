#include <altera_avalon_pio_regs.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/reent.h>
#include <time.h>

// port addresses
#define OUTPUT0 0x00101040
#define OUTPUT1 0x00101050
#define OUTPUT2 0x00101060
#define OUTPUT3 0x00101070

#define INPUT0 0x00101000
#define INPUT1 0x00101010
#define INPUT2 0x00101020
#define INPUT3 0x00101030

// configuration
#define MAX_LENGTH 255

// global variables
int *network;
int nodes;
char cmd[MAX_LENGTH];

// function prototypes
void print_help();
short str_starts_with(char*, const char*);
void get_command(char*, short);
int cmpfunc (const void*, const void*);
void reset_network();
void start_test();
int read_result();
void insert_ones(int);
void insert_zero();
void turnoff_nodes(int*, int);
void generate_random_nodes(int*, int);
float avg_path(int, int);
float impact(float, float);
void custom_random_analysis(int, int);
void random_set(int, int, int);
void growing_set(int, int);
void brute_force(int);
int get_number_nodes();

#include "ga.c"

int main() {

	int lim, nd;
	int from, to, n_sim;

	// random seed
	srand(time(NULL));

	nodes = get_number_nodes();
	network = (int*) malloc(sizeof(int) * nodes);

	// print_help();

	// init_ga(100, 5);
	// create_ga_population();
	// qsort_pop();
	// step(1000);

	show_ga_pop();

	// while(1);

	// crossover_etx(pop[0], pop[1]);
	// crossover_shuffle(pop + 0, pop + 1);
	// show_ga_pop();
	// mate(0, 1);
	// show_ga_pop();

	while (1) {

		printf("\n>> ");
		get_command(cmd, MAX_LENGTH);
		printf("\n");

		// Test board connection
		if (str_starts_with(cmd, "ping")) {
			printf("pong\n");
			continue;
		}

		// Run analysis with all nodes enabled
		if (str_starts_with(cmd, "start")) {

			float avg;
			int sp;

			reset_network();
			start_test();
			sp = read_result();
			printf("Result: %d\n", sp);
			avg = avg_path(sp,0);
			printf("Average: %.5f\n", avg);

			continue;

		}

		if (str_starts_with(cmd, "d3")) {

				init_ga(50, 400);
				create_ga_population();
				qsort_pop();
				step(10);
				continue;

		}

		if (str_starts_with(cmd, "debug2")) {

				init_ga(10, 1400);
				create_ga_population();
				qsort_pop();
				step(1000);
				continue;

		}


		if (str_starts_with(cmd, "debug1")) {

			// sscanf(cmd, "t1 %d", &n);

			for (int n=0; n<1800; n += 50) {

				init_ga(1, n);
				create_ga_population();
				qsort_pop();

				float min = get_ga_min();
				float max = get_ga_max();
				float mean_2 = get_ga_mean();

				printf("n = %3d, [min = %f, max = %f, first = %f]\n", n, min, max, pop[0].fitness);

				// step(1000);

			}

			continue;

		}



		// Compute ASP with some nodes disabled
		if (str_starts_with(cmd, "disable")) {

			float avg;
			int sp;
			int *disabled_nodes;
			int n;
			int numchar = 0, num = 0;

			sscanf(cmd, "disable %d %n", &n, &numchar);

			if (n < 1) continue;

			disabled_nodes = (int*) malloc(sizeof(int) * n);
			for(int i=0; i<n; i++) {
				sscanf(cmd+numchar, "%d%n", &disabled_nodes[i], &num);
				numchar+=num;
			}
			qsort(disabled_nodes, n, sizeof(int), cmpfunc);

			reset_network();
			turnoff_nodes(disabled_nodes, n);
			start_test();
			sp = read_result();
			printf("Result: %d\n", sp);
			avg = avg_path(sp,n);
			printf("Average: %.5f\n", avg);

			free(disabled_nodes);
			continue;

		}

		// random set analysis
		if (str_starts_with(cmd, "sweep rand")) {

			sscanf(cmd, "sweep rand %d %d %d", &from, &to, &n_sim);

			random_set(from, to, n_sim);
			continue;
		}

		// growing set analysis
		if (str_starts_with(cmd, "sweep grow")) {

			sscanf(cmd, "sweep grow %d %d", &to, &n_sim);

			growing_set(to, n_sim);
			continue;
		}

		// brute-force analysis
		if (str_starts_with(cmd, "sweep max")) {

			sscanf(cmd, "sweep max %d", &to);

			brute_force(to);
			continue;
		}

		// Custom random analysis
		if (str_starts_with(cmd, "random")) {

			sscanf(cmd, "random %d %d", &lim, &nd);

			custom_random_analysis(lim, nd);
			continue;

		}

		// run an analysis with current configuration
		if (str_starts_with(cmd, "run")) {
			int sp;

			start_test();
			sp = read_result();
			printf("Result: %d\n", sp);

			continue;

		}

		// print number of nodes of the network
		if (str_starts_with(cmd, "stat")) {

			printf("The network contains %d nodes ", nodes);
			printf("with undirected connections.\n");

			continue;

		}

		// print help of the tool
		if (str_starts_with(cmd, "help")) {

			print_help();
			continue;

		}

		// reset network and shift register
		if (str_starts_with(cmd, "reset")) {

			reset_network();
			printf("Network reset.");

			continue;

		}

		// insert 'n' ones inside the shift register
		if (str_starts_with(cmd, "one")) {
			int value;

			sscanf(cmd, "one %d", &value);
			insert_ones(value);

			continue;

		}

		// insert a zero into the shift register
		if (str_starts_with(cmd, "zero")) {

			sscanf(cmd, "zero");
			insert_zero();

			continue;

		}

		// read and display the result computed previously
		if (str_starts_with(cmd, "result")) {
			int sp;
			sp = IORD_ALTERA_AVALON_PIO_DATA(INPUT1);
			printf("Result: %d\n", sp);

			continue;

		}

		if (str_starts_with(cmd, "ga init")) {

			int psize;  // population size
			int isize;  // individual size

			sscanf(cmd, "ga init %d %d", &psize, &isize);

			init_ga(psize, isize);
			create_ga_population();
			continue;
		}

		if (str_starts_with(cmd, "ga show")) {

			show_ga_pop();
			continue;
		}

		if (str_starts_with(cmd, "ga best")) {

			printf("Best fitness: %f\n", get_ga_best(nodes));
			continue;
		}

		printf("Unrecognized command \n");

	}

	return 0;
}

// remove 'n_sim' independently chosen sets of size k, for k = 'from' .. 'to'
void random_set(int from, int to, int n_sim) {

	int i, j;
	int sp;
	float im;
	float avg_sp0, avg_spn;
	int *disabled_nodes;
	float media_im;
	float media_avg_sp;
	float min_im;
	float max_im;

	// first analysis
	reset_network();
	start_test();
	sp = read_result();
	avg_sp0 = avg_path(sp,0);

	for (i=from; i<=to;i++){

		min_im = nodes;
		max_im = -1000;
		media_im = 0;
		media_avg_sp = 0;
		disabled_nodes = (int*) malloc(sizeof(int) * i);

		for (j=0; j<n_sim; j++) {

			// turn off random nodes
			reset_network();
			generate_random_nodes(disabled_nodes, i);
			qsort(disabled_nodes, i, sizeof(int), cmpfunc);
			turnoff_nodes(disabled_nodes, i);

			// start test and read result
			start_test();
			sp = read_result();

			// compute impact and avg sh path
			avg_spn = avg_path(sp,i);
			im = impact(avg_sp0,avg_spn);
			media_im += im;
			media_avg_sp +=avg_spn;
			if ( im < min_im) min_im = im;
			if ( im > max_im) max_im = im;
		}
		media_im /= n_sim;
		media_avg_sp /= n_sim;
		printf("%.5f,%.5f,%.5f,\n",min_im, media_im, max_im);
		free(disabled_nodes);
	}

	return;
}

// remove 'n_sim' growing sets of size k, for k = 'from' .. 'to', starting
// with fixed random permutations as seeds for the growing sets
void growing_set(int to, int n_sim){

	int i, j;
	int sp;
	float im;
	float avg_sp0, avg_spn;
	int *disabled_nodes;
	float *res_avg, *res_min, *res_max, *res_path;

	// first analysis
	reset_network();
	start_test();
	sp = read_result();
	avg_sp0 = avg_path(sp,0);

	// instantiate counters
	res_avg = (float *) malloc(sizeof(float) * to);
	res_min = (float *) malloc(sizeof(float) * to);
	res_max = (float *) malloc(sizeof(float) * to);
	res_path = (float *) malloc(sizeof(float) * to);

	for (i=0; i<to; i++) {
		res_min[i] = nodes;
		res_max[i] = -1000;
		res_avg[i] = 0;
		res_path[i] = 0;
	}

	for (i=0; i<n_sim; i++) {

		disabled_nodes = (int*) malloc(sizeof(int) * to);
		generate_random_nodes(disabled_nodes,to);
		printf("Simulation %d.. ", i+1);

		for (j=0; j<to; j++) {
			int *dis;
			dis = (int*) malloc(sizeof(int) * j+1);
			for (int k=0;k< j+1; k++) {
				dis[k] = disabled_nodes[k];
			}
			qsort(dis, j+1, sizeof(int), cmpfunc);

			// turn off random nodes
			reset_network();
			turnoff_nodes(dis, j+1);

			// start test and read result
			start_test();
			sp = read_result();

			// compute impact and avg sh path
			avg_spn = avg_path(sp,j+1);
			im = impact(avg_sp0,avg_spn);
			res_avg[j] += im;
			res_path[j] +=avg_spn;
			if ( im < res_min[j]) res_min[j] = im;
			if ( im > res_max[j]) res_max[j] = im;

			free(dis);
		}

		free(disabled_nodes);
		printf("Done.\n");
	}

	printf("\nResults:\n\n");
	for(j=0; j<to; j++){
		res_avg[j] /= n_sim;
		res_path[j] /= n_sim;
		printf("%.5f,%.5f,%.5f,\n",res_min[j], res_avg[j], res_max[j]);
	}

	free(res_avg);
	free(res_max);
	free(res_min);
	free(res_path);

	return;
}

// Remove sets of size k, for k = 1 .. 'to', where each node added into the
// set maximises the impact of the network
void brute_force(int to) {

	int i, j;
	int sp;
	float im;
	float avg_sp0, avg_spn;
	int *disabled_nodes;
	float max_im;
	int index;
	int size;
	int node;

	// first analysis
	reset_network();
	start_test();
	sp = read_result();
	avg_sp0 = avg_path(sp,0);

	disabled_nodes = (int *) malloc(sizeof(int) * to);
	size = nodes;
	for(int k=0; k<nodes;k++) {
		network[k]=k;
	}

	for (i=0; i<to; i++) {
		max_im = -1000;
		int *dis;
		dis = (int*) malloc(sizeof(int) * i+1);
		for (j=0; j<size; j++) {

			disabled_nodes[i] = network[j];

			for (int k=0;k< i+1; k++) {
				dis[k] = disabled_nodes[k];
			}
			qsort(dis, i+1, sizeof(int), cmpfunc);

			// turn off random nodes
			reset_network();
			turnoff_nodes(dis, i+1);

			// start test and read result
			start_test();
			sp = read_result();

			// compute impact and avg sh path
			avg_spn = avg_path(sp,i+1);
			im = impact(avg_sp0,avg_spn);
			if ( im > max_im){
				max_im = im;
				node = network[j];
				index = j;
			}
		}
		free(dis);
		disabled_nodes[i] = node;
		network[index] = network[size-1];
		size--;
		printf("%.5f,\n", max_im);
	}
	free(disabled_nodes);
	return;
}

// custom random analysis
void custom_random_analysis(int lim, int nd) {
	int sp = 0;
	int spd = 0;
	int *disabled_nodes;
	float im;
	float im_opt;
	float avg_sp0;
	float avg_spn;

	disabled_nodes = (int*) malloc(sizeof(int) * nd);

	// first analysis
	reset_network();
	start_test();
	sp = read_result();
	avg_sp0 = avg_path(sp,0);
	printf("Average shortest path with all vertices: %.4f\n\n", avg_sp0);
	im_opt = 0;

	for (int i = 0; i<lim; i++) {

		// reset network
		reset_network();

		// disable random nodes
		generate_random_nodes(disabled_nodes, nd);
		qsort(disabled_nodes, nd, sizeof(int), cmpfunc);
		turnoff_nodes(disabled_nodes, nd);

		// start analysis and compute results
		start_test();
		spd = read_result();
		avg_spn = avg_path(spd,nd);
		im = impact(avg_sp0,avg_spn);

		// if better result is reached, keep it
		if(im > im_opt){
			im_opt = im;
			printf("Nodes disabled: ");
			for(int j=nd-1; j>=0; j--)
				printf("%d ", disabled_nodes[j]+1);
			printf("\n");
			printf("Resulting average shortest path: %.5f\n", avg_spn);
			printf("%d) Impact of these nodes: %.5f\n\n", i, im_opt);
		}

	}

	free(disabled_nodes);
	return;
}

// helper functions
void print_help() {

	char usage[][96] = {
		"FANTASI Terminal Ver 2.0",
		"",
		"Commands:",
		"",
		"  start                       Compute network Average Shortest Path (ASP).",
		"  disable <n> <nodes>         Compute ASP with <n> disabled <nodes>.",
		"  random <n> <m>              Compute <n> ASP with <m> random nodes removed.",
		"  sweep rand <from> <to> <n>  Compute <n> ASP with [<from>, <to>] nodes removed.",
		"  sweep grow <to> <n>         Compute <n> ASP with [1, <to>] nodes removed.",
		"  sweep max <to>              Compute max ASP for [1, <to>] removes nodes.",
		"  ga init <n> <nodes>         Initialize population of <n> individuals.",
		"  ga show                     Print population table.",
		"  result                      Display the result.",
		"  reset                       Reset network and result/shift registers.",
		"  stat                        Print node count.",
		"  help                        Print command help.",
		"",
		"Examples:",
		"",
		"  disable 3 5 10 946          Compute ASP with 3 nodes removed: 5, 10 and 946",
	};

	size_t nlines = sizeof(usage) / sizeof(usage[0]);

	for (int i=0; i<nlines; i++)
		printf("%s\n", usage[i]);

}

short str_starts_with(char *str1, const char *str2) {

	// returns 1 if str1 starts with str2, 0 otherwise
	short i = 0;

	while (str2[i] != '\0') {
		if (str1[i] != str2[i]) return 0;
		i++;
	}

	return 1;
}

void get_command(char *str, short max_length) {

	// uses getchar() to read stdin, returns when newline is read
	char v;
	short ind = 0;

	while ((v = getchar()) != '\n') {
		str[ind++] = v;
		if (ind > max_length-1) break;
	}

	str[ind] = '\0';

}

// for quick sort
int cmpfunc (const void * a, const void * b) {
   return ( *(int*)b - *(int*)a );
}

// reset graph and execute 'master set' to the shift register
void reset_network(){

	// initial state of start and reset
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 0);

	// reset the graph
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 5);
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 0);
}

// start test
void start_test(){
	int n = 0;

	// start the test
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 2);

	// read result
	while ( !n ) {
		n = IORD_ALTERA_AVALON_PIO_DATA(INPUT2) & 1;
	}
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 0);
}

// read the result
int read_result(){
	return IORD_ALTERA_AVALON_PIO_DATA(INPUT1);
}

// insert 'n' 1 into the shift registers
void insert_ones(int n) {
	int done = 0;

	// a positive number of nodes have to be enabled
	if (n <= 0) return;

	// shift in 'n' ones
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT2, n);
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 16);
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 0);

	// wait for the completion
	while ( !done ) {
		done = IORD_ALTERA_AVALON_PIO_DATA(INPUT2) & 2;
	}
}

// insert 'n' zeros into the shift register
void insert_zero() {
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 8);
	IOWR_ALTERA_AVALON_PIO_DATA(OUTPUT1, 0);
}

// turn off the nodes in the array
void turnoff_nodes(int *nodes_to_disable, int n){
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

void generate_random_nodes(int *random_array, int length) {
	int i,j,r,c=0;

	// initialize pool of node indexes
	for(i=0; i<nodes;i++){
		network[i] = i;
	}

	// pick randomly 'n' nodes
	j=nodes-1;
	for(i=0; i<length; i++){
		r = rand() % j;
		random_array[c++] = network[r];
		network[r] = network[j--];
	}
}

// compute average shortest path
float avg_path(int sh_paths, int nodes_disabled) {
	return (((float)sh_paths) / ((nodes-nodes_disabled) * (nodes-nodes_disabled-1)));
}

// compute impact of the modified network
float impact(float avg0, float avgn) {
	float sub = avgn-avg0;
	//if (sub < 0) sub = -sub;
	return sub/avg0;
}

// get number of nodes by testing the HW architecture
int get_number_nodes() {
	int n = 0;
	int sp = 1;

	while( sp ) {
		reset_network();
		for(int i = 0; i<n; i++) {
			insert_zero();
		}
		start_test();
		sp = read_result();
		n++;
	}

	return n;
}
