#include <stdlib.h>

typedef ushort cross_item_t;

// #define CROSSOVER_MUTATE_DBG 1

#ifdef CROSSOVER_MUTATE_DBG
    #define cprintf(...) printf(__VA_ARGS__)
#else
    #define cprintf(...) (__VA_ARGS__);
#endif

void crossover_sorted(
        cross_item_t *p1,      // parent 1
        cross_item_t *p2,      // parent 2
        cross_item_t *child1,  // child 1
        cross_item_t *child2,  // child 2
        // int ival1,    // value to insert into child 1
        // int ival2,    // value to insert into child 2
        // uint dp1,      // index of child 1 item to delete
        // uint dp2,      // index of child 1 item to delete
        uint n         // number of items in individual
    ) {

    // Crossover two parents and return point-mutated children.

    // Perform few sanity checks

    // if (dp1 >= n || dp2 >= n) {
    //     cprintf("Error, dp1/dp2 is out of range.\n");
    //     return;
    // }

    int p1_c = 0;
    int p2_c = 0;

    cross_item_t *child1_head = child1;
    cross_item_t *child2_head = child2;

    int child1_c = 0;
    int child2_c = 0;

    int turn = 0;

    // int is_inserted_1 = 0;  // 1 iff ival1 has been inserted in child 1
    // int is_inserted_2 = 0;  // 1 iff ival2 has been inserted in child 2

    // cprintf("Deletion point 1 : %d\n", dp1);
    // cprintf("Deletion point 2 : %d\n", dp2);

    for (int i=0; i<n*2; i++) {

        int item;

        if (p1_c == n) {

            // Processed all items in p1, pick next from p2 ...

            item = p2[p2_c++];

        } else if (p2_c == n) {

            // Processed all items in p2, pick next from p1 ...

            item = p1[p1_c++];

        } else {

            // Pick largest item from p1[p1_c] and p2[p2_c] ...

            if (p1[p1_c] > p2[p2_c]) {
                item = p1[p1_c];
                p1_c++;
            } else {
                item = p2[p2_c];
                p2_c++;
            }

        }

        // Add picked item to a child ...

        if (turn == 0) {

            // Add to child 1

            // if (child1_c != dp1) {

                // if (item > ival1 && is_inserted_1 == 0) {
                //     cprintf("Inserting %2d in child 1\n", ival1);
                //     *(child1_head++) = ival1;
                //     is_inserted_1 = 1;
                // }

                *(child1_head++) = item;

                cprintf("Assigning %2d to child 1\n", item);

            // } else {
            //     cprintf("Skipping assigning %d to child 1\n", item);
            // }

            child1_c++;

        } else {

            // Add to child 2

            // if (child2_c != dp2) {

                // if (item > ival2 && is_inserted_2 == 0) {
                //     cprintf("Inserting %2d in child 2\n", ival2);
                //     *(child2_head++) = ival2;
                //     is_inserted_2 = 1;
                // }

                *(child2_head++) = item;

                cprintf("Assigning %2d to child 2\n", item);

            // } else {
            //     cprintf("Skipping assigning %d to child 2\n", item);
            // }

            child2_c++;

        }

        turn = 1 - turn;

    }

    // Append ival1/ival2 if they haven't been inserted already

    // if (is_inserted_1 == 0) {
    //     *(child1_head++) = ival1;
    //     cprintf("Inserting %2d in child 1\n", ival1);
    // }

    // if (is_inserted_2 == 0) {
    //     *(child2_head++) = ival2;
    //     cprintf("Inserting %2d in child 2\n", ival2);
    // }

}

int rand_range(int mn, int mx) {

    // Return random number in [mn, mx].

    if (mn == mx) {
        cprintf("Error, mn is equal to mx");
        return -1;
    }

    return mn + rand() % (mx - mn + 1);
}

int check_sorted(cross_item_t* arr, int n) {

    int result = 0;

    for (int i=0; i<n-1;i++) {

        if (arr[i] < arr[i+1]) {

            printf("sort error: node[%d] = %d but node[%d] = %d\n",
                i, arr[i], i+1, arr[i+1]);

            result = 1;

        }
    }

    return result;
}

void point_mutate_sorted(cross_item_t* arr, int n, int max_val) {

    while (1) {

        int pos = rand() % n;

        int mn, mx;

        if (pos == 0) {

            // first item in array

            mx = max_val;
            mn = arr[1] + 1;

        } else if (pos == n-1) {

            // last element in array

            mx = arr[n-2] - 1;
            mn = 0;

        } else {

            mn = arr[pos+1] + 1;
            mx = arr[pos-1] - 1;

        }

        if (mx == mn) continue;

        cross_item_t new_val = rand_range(mn, mx);

        arr[pos] = new_val;

        cprintf("Finding a value in [%d, %d]\n", mn, mx);

        cprintf("Successfully inserted %d at pos %d\n", new_val, pos);

        return;

    }
}

void crossover_etx(
        cross_item_t *p1,      // parent 1
        cross_item_t *p2,      // parent 2
        cross_item_t *child1,  // child 1
        cross_item_t *child2,  // child 2
        char *temp1,           // temporary buffer (length = n)
        char *temp2,           // temporary buffer (length = n)
        uint n,                // number of items in individual
        uint item_max          // max item value
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

    for (int i=nodes-1; i>=0; i--) {

        if ((temp1[i] == 1) && (temp2[i] == 1)) {

            c1[nc1++] = i;
            c2[nc2++] = i;

        } else if ((temp1[i] == 1) || (temp2[i] == 1)) {

            if (nc1 < nc2) c1[nc1++] = i;
            else           c2[nc2++] = i;

        }

    }

}