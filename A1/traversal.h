#ifndef TRAVERSAL_H
#define TRAVERSAL_H

/**
 * Traverses a directory and lists all the files/directories present, then capitalizes
 * all instances of the specified word, and analyzes nested directories recursively
 * @param directory Directory to analyze
 * @param word      Word to capitalize
 */
void traverseDir(const char* directory, const char* word);

#endif //TRAVERSAL_H