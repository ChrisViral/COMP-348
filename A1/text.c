#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "text.h"
#include "report.h"

/*
 * Constants
 */

#define BASE_SIZE 100
#define MULT 2

/*
 * Forward Declarations
 */

static char* strcasestr(const char*, const char*);

/*
 * Functions
 */

/**
 * Capitalizes all the instances of a given word in the file
 * @param file File to search in
 * @param word Word to capitalize
 */
void replaceFile(const char* file, const char* word)
{
    //Open the file for read
    FILE* f = fopen(file, "r");
    
    //If it failed to open, exit and fail
    if (f == NULL)
    {
        fclose(f);
        exit(EXIT_FAILURE);
    }
    
    //Allocate file text
    char* text = (char*)malloc(BASE_SIZE * sizeof(char));
    *text = '\0';
    size_t wLen = strlen(word), size = 0, capacity = BASE_SIZE, modifications = 0;
    
    //Allocate read variables
    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    
    //While there are lines to read in the file
    while ((read = getline(&line, &len, f)) != -1)
    {
        //Look for all instances of the word in the line
        for (char* p = strcasestr(line, word); p != NULL; p = strcasestr(p, word))
        {
            for (int i = 0; i < wLen; i++, p++)
            {
                *p = (char)toupper(*p);
            }
            modifications++;
        }
        
        //If the file text buffer is too small, reallocate it larger
        while (capacity < size + read - 1)
        {
            capacity *= MULT;
            text = (char*)realloc(text, capacity * sizeof(char));
        }
        
        //Add the line to the file text
        size += read;
        strcat(text, line);
    }
    
    //Free the memory from the line and close the file
    free(line);
    line = NULL;
    fclose(f);
    
    //If modifications were made, overwrite the file with the new text
    if (modifications > 0)
    {
        f = fopen(file, "w");
        fprintf(f, text);
        fclose(f);
        addModification(file, modifications);
    }
    
    //Free the file text
    free(text);
}

/*
 * Static Functions
 */

/**
 * Case insensitive strstr implementation
 * @param haystack String to search in
 * @param needle   String to find
 * @return         A pointer to the first occurrence of the needle, or NULL if none was found
 */
static char* strcasestr(const char* haystack, const char* needle)
{
    if (strlen(haystack) == 0) { return NULL; }
    if (strlen(needle) == 0) { return (char*)haystack; }
    
    do
    {
        const char* h = haystack;
        const char* n = needle;
        while (*n != '\0' && tolower(*h) == tolower(*n))
        {
            h++;
            n++;
        }
        if (*n == '\0')
        {
            return (char*)haystack;
        }
    }
    while (*(haystack++) != '\0');
    
    return NULL;
}