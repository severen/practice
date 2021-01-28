#ifndef COMMON_H
#define COMMON_H

int strtoi(const char *restrict str, char **restrict endptr, int base);

int parse_int(const char str[]);
long parse_long(const char str[]);

#endif /* COMMON_H */
