
#include <subprocess.h>
#include <ev.h>

#include <stddef.h>

int
main(int argc, char ** argv) {
    struct ev_loop *loop = ev_default_loop(EVFLAG_AUTO);
    subprocess_t * subprocess = subprocess_alloc();
    // char * cmd_line[] = { "echo", "Hello world!", NULL, };
    char * cmd_line[] = { "touch", "/tmp/subprocess-demo", NULL, };

    argc = argc, argv = argv;
    subprocess_spawn(subprocess, cmd_line, NULL, NULL);
    ev_loop(loop, 0);

    return 0;
}

