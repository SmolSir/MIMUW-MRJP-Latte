#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error() {
    printf("runtime error");
    printf("\n");
    exit(1);
}

int readInt() {
    int value;
    char* string = NULL;
    size_t length = 0;

    if (getline(&string, &length, stdin) == -1) {
        printf("An error occured during getline in readInt");
        printf("\n");
        exit(1);
    }

    sscanf(string, "%d", &value);

    return value;
}

char* readString() {
    char* string = NULL;
    size_t length = 0;

    if (getline(&string, &length, stdin) == -1) {
        printf("An error occured during getline in readString");
        printf("\n");
        exit(1);
    }

    length = strlen(string);
    if (string[length - 1] == '\n') {
        string[length - 1] = '\0';
    }

    return string;
}

extern void printInt(int value) {
    printf("%d", value);
    printf("\n");
}

extern void printString(const char* string) {
    if (string == NULL) {
        printf("\n");
    }
    else {
        printf("%s", string);
        printf("\n");
    }
}

char* concatString(char* stringL, char* stringR) {
    if (stringL == NULL) {
        return stringR;
    }
    if (stringR == NULL) {
        return stringL;
    }

    size_t lengthL = strlen(stringL);
    size_t lengthR = strlen(stringR);

    char* concat = malloc(lengthL + lengthR + 1);

    memcpy(concat, stringL, lengthL);
    memcpy(concat + lengthL, stringR, lengthR);

    concat[lengthL + lengthR] = '\0';

    return concat;
}

int compareString(char* stringL, char* stringR) {
    int result;

    if (stringL == NULL && stringR != NULL) {
        result = strcmp("", stringR);
    }
    else if (stringL != NULL && stringR == NULL) {
        result = strcmp(stringL, "");
    }
    else if (stringL == NULL && stringR == NULL) {
        result = 0;
    }
    else {
        result = strcmp(stringL, stringR);
    }

    return result;
}
