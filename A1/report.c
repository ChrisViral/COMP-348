#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "report.h"

/*
 * Constants
 */

#define BASE_SIZE 4
#define MULT 2

/**
 * Structs
 */

/**
 * Report struct
 */
typedef struct
{
    char* fileName;
    size_t count;
} Report;

/**
 * Report List struct
 */
typedef struct
{
    Report* list;
    size_t length;
    size_t capacity;
} ReportList;


/*
 * Static variables
 */

/**
 * Report list
 */
static ReportList reports;

/*
 * Forward declarations
 */

static void sortReports();

/*
 * Functions
 */

/**
 * Initialize the reports data structure
 */
void initializeReports()
{
    reports.list = (Report*)malloc(BASE_SIZE * sizeof(Report));
    reports.length = 0;
    reports.capacity = BASE_SIZE;
}

/**
 * Free the allocated reports memory
 */
void freeReports()
{
    for (int i = 0; i < reports.length; i++)
    {
        free(reports.list[i].fileName);
    }
    free(reports.list);
    reports.list = NULL;
    reports.length = 0;
    reports.capacity = 0;
}

/**
 * Print the recorded reports
 */
void printReport()
{
    //Print header
    printf("===== File Changes Report =====\n\n");
    
    //If nothing to report, print correct message
    if (reports.length == 0)
    {
        printf("No file chances executed.");
    }
    else
    {
        //Sort reports
        sortReports();
        printf("Updates\t\tFile\n");
        
        //Print each report
        for (int i = 0; i < reports.length; i++)
        {
            Report r = reports.list[i];
            printf("%zu\t\t\t%s\n", r.count, r.fileName);
        }
    }
}

/**
 * Add file Report
 * @param fileName File name to report
 * @param amount   Amount of changes made
 */
void addModification(const char* fileName, size_t amount)
{
    //Initialize new report
    Report r;
    size_t len = strlen(fileName);
    r.fileName = (char*)malloc(len * sizeof(char) + 1);
    strcpy(r.fileName, fileName);
    r.count = amount;
    
    //If insufficient capacity, expend the list
    if (reports.length == reports.capacity)
    {
        reports.capacity *= MULT;
        reports.list = (Report*)realloc(reports.list, reports.capacity * sizeof(Report));
    }
    
    //Add the report
    reports.list[reports.length++] = r;
}

/**
 * Static functions
 */

/**
 * Descending sort comparing function
 * @param a First element
 * @param b Second element
 * @return  The sort order
 */
static int cmpRep(const void* a, const void* b)
{
    return ((int)((Report*)b)->count) - ((int)((Report*)a)->count);
}

/**
 * Sorts the current report list
 */
static void sortReports()
{
    qsort(reports.list, reports.length, sizeof(Report), cmpRep);
}