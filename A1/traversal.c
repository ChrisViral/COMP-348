#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include "traversal.h"
#include "text.h"

/*
 * Forward declarations
 */

static bool isDir(const char*);

/*
 * Functions
 */

/**
 * Traverses a directory and lists all the files/directories present, then capitalizes
 * all instances of the specified word, and analyzes nested directories recursively
 * @param directory Directory to analyze
 * @param word      Word to capitalize
 */
void traverseDir(const char* directory, const char* word)
{
    //Open the directory for reading
    DIR* d = opendir(directory);
    if (d != NULL)
    {
        //While there are more files to read in the directory
        for (struct dirent* dir = readdir(d); dir != NULL; dir = readdir(d))
        {
            //Skip dot folders and executable
            char* name = dir->d_name;
            if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0 || strcmp(name, "a.out") == 0) { continue; }
            
            //Get full name
            char* fullName;
            char buff[strlen(directory) + strlen(name) + 2];
            if (strcmp(directory, ".") == 0) { fullName = name; }
            else
            {
                //Join the paths and get the pointer to the first character
                snprintf(buff, sizeof(buff), "%s/%s", directory, name);
                fullName = &buff[0];
            }
            
            //Determine if this is a directory
            bool isd = isDir(fullName);
            
            //Examine the directory
            if (isd)
            {
                //printf("Entering the directory %s\n\n", name);
                traverseDir(fullName, word);
            }
            //Replace in file
            else
            {
                //printf("Looking at the file %s\n\n", name);
                replaceFile(fullName, word);
            }
            
        }
        //Close the directory
        closedir(d);
    }
}

/*
 * Static Functions
 */

/**
 * Determines if a specific path is a directory or file
 * @param path Path to examine
 * @return     True if the path is a directory, false otherwise
 */
static bool isDir(const char* path)
{
    struct stat path_stat;
    stat(path, &path_stat);
    return S_ISDIR(path_stat.st_mode);
}