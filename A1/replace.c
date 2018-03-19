#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "traversal.h"
#include "report.h"

/*
 * Main
 */

/**
 * Program entry point
 * @param argc Argument count, should be 2 for normal execution
 * @param argv Program arguments, the second argument should be the word to search for
 */
int main(int argc, char* argv[])
{
    //If there are not two arguments, exit immediately
    if (argc != 2)
    {
        printf("Please supply one and only one search argument\nExiting process now\n");
        return EXIT_FAILURE;
    }
    
    //Get the word to search for
    const char* word = argv[1];
    char currDir[100];
    printf("===== Word Capitalizer =====\n\n");
    printf("Starting from folder %s\n", getcwd(currDir, sizeof(currDir)));
    printf("Looking for instances of the word \"%s\".\n\n", word);
    
    //Initialize the report system
    initializeReports();
    
    //Traverse the current directory
    traverseDir(".", word);
    
    //Print reports, then free memory
    printReport();
    freeReports();
    
    //Exit successfully
    return EXIT_SUCCESS;
}