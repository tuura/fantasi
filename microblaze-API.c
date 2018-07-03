#include <stdlib.h>
#include "platform.h"
#include "xil_printf.h"
#include "xuartlite_l.h"

// Libs for accessing GPIO
#include "xparameters.h"
#include "xgpio.h"
#include "xstatus.h"

// Definitions
#define LED_CHANNEL 1
#define printf xil_printf

// GPIOs
XGpio gpio_out1;
XGpio gpio_out2;
XGpio gpio_in1;
XGpio gpio_in2;

// configuration
#define MAX_CMD 128
#define NODES 1628

// expression definitions
#define INTEGER(x) x
#define DECIMAL(x) ((x - whole) * 1000)

// global variables
int network[NODES];
char cmd[MAX_CMD];
int whole, thousandths;

// function prototypes
void print_help();
void get_command(char*, short);
short str_starts_with(char*, const char*);
void get_command(char*, short);
int cmpfunc (const void*, const void*);
void reset_network();
void start_test();
u32 read_result();
float avg_path(int, int);
float impact(float, float);
void random_analysis(int, int);
void generate_random_nodes(int*, int);
void turnoff_nodes(int*, int);
int rsscanf(const char*, const char*, ...);
void average_analysis(int, int, int);
int get_number_nodes();

int main() {

	init_platform();

	int status;
	int lim, nd;

	// initialize gpios
	status = XGpio_Initialize(&gpio_out1, XPAR_AXI_GPIO_0_DEVICE_ID); // RESET START TEST
	if (status != XST_SUCCESS) return -1;
	status = XGpio_Initialize(&gpio_out2, XPAR_AXI_GPIO_2_DEVICE_ID); // reconfiguration
	if (status != XST_SUCCESS) return -1;
	status = XGpio_Initialize(&gpio_in1, XPAR_AXI_GPIO_1_DEVICE_ID); // complete
	if (status != XST_SUCCESS) return -1;
	status = XGpio_Initialize(&gpio_in2, XPAR_AXI_GPIO_3_DEVICE_ID); // result
	if (status != XST_SUCCESS) return -1;

	// set GPIOs to be output
	XGpio_SetDataDirection(&gpio_out1, LED_CHANNEL, 0x0);
	XGpio_SetDataDirection(&gpio_out2, LED_CHANNEL, 0x0);

	// set GPIOs to be input
	XGpio_SetDataDirection(&gpio_in1, LED_CHANNEL, 0xFFFFFFFF);
	XGpio_SetDataDirection(&gpio_in2, LED_CHANNEL, 0xFFFFFFFF);

	// random seed
	srand(0);

	print_help();

	while (1) {

		printf("\n>> ");
		get_command(cmd, MAX_CMD);
		printf("\n\r");

		// Test board connection
		if (str_starts_with(cmd, "ping")) {
			printf("pong\n\r");
			continue;
		}

		// Run analysis with all nodes enabled
		if (str_starts_with(cmd, "start")) {

			float avg;
			int sp;

			reset_network();
			start_test();
			sp = read_result();
			printf("Sum of all paths: %d\n\r", sp);
			avg = avg_path(sp,0);
			whole = INTEGER(avg);
			thousandths = DECIMAL(avg);
			printf("Average shortest path: %d.%03d\n\r", whole, thousandths);

			continue;
		}

		// Custom random analysis
		if (str_starts_with(cmd, "random")) {

			rsscanf(cmd, "random %d %d", &lim, &nd);

			random_analysis(lim, nd);
			continue;

		}

		// Average random set analysis
		if (str_starts_with(cmd, "average")) {
			int from, to, n_sim;

			rsscanf(cmd, "average %d %d %d", &from, &to, &n_sim);

			average_analysis(from, to, n_sim);
			continue;
		}

		// print number of nodes of the network
		if (str_starts_with(cmd, "stat")) {

			int nodes = get_number_nodes();

			printf("The network contains %d nodes ", nodes);
			printf("with undirected connections.\n\r");

			continue;

		}

		// print help of the tool
		if (str_starts_with(cmd, "help")) {

			print_help();
			continue;

		}

		printf("Unrecognized command\n\r");

	}

	free(network);

	cleanup_platform();
	return 0;
}

// remove 'n_sim' independently chosen sets of size k, for k = 'from' .. 'to'
void average_analysis(int from, int to, int n_sim) {

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

	printf(" NODES-OFF\t  MIN\t AVG\t MAX\t (impact to average shortest path of the network)\n\r");
	printf("-------------------------------------\n\r");
	for (i=from; i<=to;i++){

		min_im = NODES;
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
		printf("   %d\t\t", i);
		media_im /= n_sim;
		media_avg_sp /= n_sim;
		whole = INTEGER(min_im);
		thousandths = DECIMAL(min_im);
		if (thousandths < 0) thousandths = 0;
		printf(" %d.%03d\t", whole, thousandths);
		whole = INTEGER(media_im);
		thousandths = DECIMAL(media_im);
		printf("%d.%03d\t", whole, thousandths);
		whole = INTEGER(max_im);
		thousandths = DECIMAL(max_im);
		printf("%d.%03d\n\r", whole, thousandths);
		free(disabled_nodes);
	}

	return;
}

// random analysis
void random_analysis(int lim, int nd) {
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
	whole = INTEGER(avg_sp0);
	thousandths = DECIMAL(avg_sp0);
	printf("Average shortest path with all vertices:  %d.%03d\n\n\r", whole, thousandths);
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
			printf("(Attempt %d) Nodes disabled: ", i+1);
			for(int j=nd-1; j>=0; j--)
				printf("%d ", disabled_nodes[j]+1);
			printf("\n\r");
			whole = INTEGER(avg_spn);
			thousandths = DECIMAL(avg_spn);
			printf("Resulting average shortest path:  %d.%03d\n\r", whole, thousandths);
			whole = INTEGER(im_opt);
			thousandths = DECIMAL(im_opt);
			printf("Impact of these nodes to the average shortest path of the network: %d.%03d\n\n\r", whole, thousandths);
		}

	}

	free(disabled_nodes);
	return;
}

void generate_random_nodes(int *random_array, int length) {
	int i,j,r,c=0;

	// initialize pool of node indexes
	for(i=0; i<NODES;i++){
		network[i] = i;
	}

	// pick randomly 'n' nodes
	j=NODES-1;
	for(i=0; i<length; i++){
		r = rand() % j;
		random_array[c++] = network[r];
		network[r] = network[j--];
	}
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

// reset graph and execute 'master set' to the shift register
void reset_network(){

	// initial state of start and reset
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 0);


	// reset the graph
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 5);
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 0);
}

// start test
void start_test(){
	u32 n = 0;

	// start the test
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 2);

	// read result
	while ( !n ) {
		n = XGpio_DiscreteRead(&gpio_in1, LED_CHANNEL) & 1;
	}
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 0);
}

// read the result
u32 read_result(){
	return XGpio_DiscreteRead(&gpio_in2, LED_CHANNEL);
}

// insert 'n' 1 into the shift registers
void insert_ones(int n) {
	u32 done = 0;

	// a positive number of nodes have to be enabled
	if (n <= 0) return;

	// shift in 'n' ones
	XGpio_DiscreteWrite(&gpio_out2, LED_CHANNEL, n);
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 16);
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 0);

	// wait for the completion
	while ( !done ) {
		done = XGpio_DiscreteRead(&gpio_in1, LED_CHANNEL) & 2;
	}
}

// insert 'n' zeros into the shift register
void insert_zero() {
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 8);
	XGpio_DiscreteWrite(&gpio_out1, LED_CHANNEL, 0);
}

int cmpfunc (const void * a, const void * b) {
   return ( *(int*)b - *(int*)a );
}

// compute average shortest path
float avg_path(int sh_paths, int nodes_disabled) {
	return (((float)sh_paths) / ((NODES-nodes_disabled) * (NODES-nodes_disabled-1)));
}

// compute impact of the modified network
float impact(float avg0, float avgn) {
	float sub = avgn-avg0;
	//if (sub < 0) sub = -sub;
	return sub/avg0;
}

void turnoff_nodes(int *nodes_to_disable, int n){
	int i = 0;
	int c = NODES-1;

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

void get_command(char *str, short max_length) {

	// uses getchar() to read stdin, returns when newline is read
	unsigned char v;
	short ind = 0;

	while ((v = XUartLite_RecvByte(XPAR_UARTLITE_1_BASEADDR)) != '\r') {
		printf("%c", v);
		str[ind++] = v;
		if (ind > max_length-1) break;
	}

	str[ind] = '\0';
	printf("\r");
}

// helper functions
void print_help() {
	printf("FANTASI Terminal Ver 1.0\n\r");
	printf("Number of nodes: %d\n\n\r", NODES);
	printf("Commands:\n\r");
	printf("  start\t\t\t\t");
	printf("Compute the average shortest path of the network\n\r");
	printf("  random 'n' 'm'\t\t");
	printf("Run 'n' analysis, removing 'm' random nodes at every run\n\r");
	printf("  average 'from' 'to' 'n'\t");
	printf("Remove 'n' random sets of nodes of size k, for k = 'from' .. 'to'\n\r");
	printf("  stat\t\t\t\t");
	printf("Print number of nodes of the network\n\r");
	printf("  help\t\t\t\t");
	printf("Print help of the tool\n\r");
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

// custom implementation of the sscanf function
int rsscanf(const char* str, const char* format, ...)
{
        va_list ap;
        int value, tmp;
        int count;
        int pos;
        char neg, fmt_code;

        va_start(ap, format);

        for (count = 0; *format != 0 && *str != 0; format++, str++)
        {
                while (*format == ' ' && *format != 0)
                        format++;
                if (*format == 0)
                        break;

                while (*str == ' ' && *str != 0)
                        str++;
                if (*str == 0)
                        break;

                if (*format == '%')
                {
                        format++;
                        if (*format == 'n')
                        {
                if (str[0] == '0' && (str[1] == 'x' || str[1] == 'X'))
                {
                    fmt_code = 'x';
                    str += 2;
                }
                else
                if (str[0] == 'b')
                {
                    fmt_code = 'b';
                    str++;
                }
                else
                    fmt_code = 'd';
                        }
                        else
                                fmt_code = *format;

                        switch (fmt_code)
                        {
                        case 'x':
                        case 'X':
                                for (value = 0, pos = 0; *str != 0; str++, pos++)
                                {
                                        if ('0' <= *str && *str <= '9')
                                                tmp = *str - '0';
                                        else
                                        if ('a' <= *str && *str <= 'f')
                                                tmp = *str - 'a' + 10;
                                        else
                                        if ('A' <= *str && *str <= 'F')
                                                tmp = *str - 'A' + 10;
                                        else
                                                break;

                                        value *= 16;
                                        value += tmp;
                                }
                                if (pos == 0)
                                        return count;
                                *(va_arg(ap, int*)) = value;
                                count++;
                                break;

            case 'b':
                                for (value = 0, pos = 0; *str != 0; str++, pos++)
                                {
                                        if (*str != '0' && *str != '1')
                        break;
                                        value *= 2;
                                        value += *str - '0';
                                }
                                if (pos == 0)
                                        return count;
                                *(va_arg(ap, int*)) = value;
                                count++;
                                break;

                        case 'd':
                                if (*str == '-')
                                {
                                        neg = 1;
                                        str++;
                                }
                                else
                                        neg = 0;
                                for (value = 0, pos = 0; *str != 0; str++, pos++)
                                {
                                        if ('0' <= *str && *str <= '9')
                                                value = value*10 + (int)(*str - '0');
                                        else
                                                break;
                                }
                                if (pos == 0)
                                        return count;
                                *(va_arg(ap, int*)) = neg ? -value : value;
                                count++;
                                break;

                        case 'c':
                                *(va_arg(ap, char*)) = *str;
                                count++;
                                break;

                        default:
                                return count;
                        }
                }
                else
                {
                        if (*format != *str)
                                break;
                }
        }

        va_end(ap);

        return count;
}
