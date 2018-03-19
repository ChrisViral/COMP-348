#ifndef REPORT_H
#define REPORT_H

/**
 * Initialize the reports data structure
 */
void initializeReports();

/**
 * Free the allocated reports memory
 */
void freeReports();

/**
 * Print the recorded reports
 */
void printReport();

/**
 * Add file Report
 * @param fileName File name to report
 * @param amount   Amount of changes made
 */
void addModification(const char* fileName, size_t amount);

#endif //REPORT_H