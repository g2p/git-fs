
#ifndef SUBPROCESS_H__
#define SUBPROCESS_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct subprocess_s subprocess_t;

subprocess_t *
subprocess_alloc(void);

void
subprocess_free(subprocess_t * subprocess);

int
subprocess_spawn(
    subprocess_t * subprocess,
    char * const argv[],
    const char * executable,
    char * const env[]);

#ifdef __cplusplus
}
#endif

#endif /* SUBPROCESS_H__ */

