#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void printInt(int number) {
    printf("%d\n", number);
}

void printString(char* str) {
    printf("%s\n", str);
}


void error() {
    fprintf(stderr, "runtime error\n");
    exit(EXIT_FAILURE);
}

void clearInputBuffer() {
    int c;
    while ((c = getchar()) != '\n' && c != EOF) { }
}


int readInt() {
    int number;
    scanf("%d", &number);
    clearInputBuffer(); 
    return number;
}


char* readString() {
    char* buffer = NULL;
    size_t bufsize = 0;
    getline(&buffer, &bufsize, stdin);

    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
    }

    return buffer;
}

char* doNotUseThatNameConcat(const char* str1, const char* str2) {
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    size_t newStrLen = len1 + len2;

    char* result = malloc(newStrLen + 1);
    if (result == NULL) {
        fprintf(stderr, "Failed to allocate memory\n");
        exit(EXIT_FAILURE);
    }

    strcpy(result, str1);
    strcat(result, str2);

    return result;
}

