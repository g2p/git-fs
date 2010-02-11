
#include <subprocess.h>

#include <ev.h>

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


struct subprocess_s {
    int stdin_fd;
    int stdout_fd;
    int stderr_fd;
    int pid;
    int got_chld;
    int exit_code;
    ev_child child_watcher;
};


void
subprocess_shutdown(subprocess_t * subprocess);


static int
subprocess_set_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    int r = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    if (r != 0) {
        perror("subprocess_set_nonblocking()");
        abort();
    }
    return fd;
}

static void
subprocess_require_mainloop() {
    if (! ev_backend(EV_DEFAULT_UC)) {
        perror("event loop uninitialised");
        abort();
    }
}

subprocess_t *
subprocess_alloc(void) {
    subprocess_t * r;

    r = (subprocess_t *) calloc(1, sizeof(*r));
    if (! r) {
        perror("subprocess_alloc: out of memory");
        abort();
    }
    return r;
}

void
subprocess_free(subprocess_t * subprocess) {
    if (subprocess)
        free(subprocess);
}

static void
subprocess_on_sigchld(EV_P_ ev_child * watcher, int revents) {
    ev_child_stop(EV_A_ watcher);
    subprocess_t * subprocess = (subprocess_t *) watcher->data;

    assert(revents == EV_CHILD);
    assert(subprocess);
    assert(subprocess->pid == watcher->rpid);
    assert(&subprocess->child_watcher == watcher);

    subprocess->got_chld = 1;
    subprocess->exit_code = watcher->rstatus;

    // XXX if (child->stdin_fd  >= 0) evcom_writer_close(&child->stdin_writer);

    subprocess_shutdown(subprocess);
}


int
subprocess_spawn(
    subprocess_t * subprocess,
    char * const argv[],
    const char * executable,
    char * const env[])
{
    int stdout_pipe[2], stdin_pipe[2], stderr_pipe[2];


    if (! argv || ! argv[0]) {
        perror("subprocess_spawn: argv");
        return -1;
    }

    if (! executable)
        executable = argv[0];


    subprocess_require_mainloop();

    if (pipe(stdout_pipe) < 0) {
        perror("pipe()");
        return -3;
    }

    if (pipe(stderr_pipe) < 0) {
        perror("pipe()");
        return -4;
    }

    if (pipe(stdin_pipe) < 0) {
        perror("pipe()");
        return -5;
    }

    switch (subprocess->pid = vfork()) {
        case -1:  // Error.
            subprocess_shutdown(subprocess);
            return -6;

        case 0:  // Child.
            close(stdout_pipe[0]);  // close read end
            dup2(stdout_pipe[1], STDOUT_FILENO);

            close(stderr_pipe[0]);  // close read end
            dup2(stderr_pipe[1], STDERR_FILENO);

            close(stdin_pipe[1]);  // close write end
            dup2(stdin_pipe[0],  STDIN_FILENO);

            if (env) {
                /* gnu extension, glibc 2.11 */
                // XXX execvpe(executable, argv, env);
                perror("execvpe()");
            } else {
                execvp(executable, argv);
                perror("execvp()");
            }
            _exit(127);
    }

    // Parent.

    ev_child_init(&subprocess->child_watcher,
            subprocess_on_sigchld, subprocess->pid, 0);
    subprocess->child_watcher.data = subprocess;
    ev_child_start(EV_DEFAULT_UC_ &subprocess->child_watcher);

    close(stdout_pipe[1]);
    subprocess->stdout_fd = subprocess_set_nonblocking(stdout_pipe[0]);

    close(stderr_pipe[1]);
    subprocess->stderr_fd = subprocess_set_nonblocking(stderr_pipe[0]);

    close(stdin_pipe[0]);
    subprocess->stdin_fd = subprocess_set_nonblocking(stdin_pipe[1]);

    // XXX evcom_reader_set(&stdout_reader_, stdout_fd_);
    // XXX evcom_reader_attach(EV_DEFAULT_UC_ &stdout_reader_);

    // XXX evcom_reader_set(&stderr_reader_, stderr_fd_);
    // XXX evcom_reader_attach(EV_DEFAULT_UC_ &stderr_reader_);

    // XXX evcom_writer_set(&stdin_writer_, stdin_fd_);
    // XXX evcom_writer_attach(EV_DEFAULT_UC_ &stdin_writer_);

    return 0;
}


void
subprocess_shutdown(subprocess_t * subprocess) {
    if (! subprocess->pid) {
        perror("subprocess_shutdown: already shut down or never started");
        return;
    }

    subprocess_require_mainloop();

    if (subprocess->stdin_fd >= 0) {
        // XXX evcom_writer_close(&stdin_writer_);
    }

    if (subprocess->stdin_fd  >= 0) close(subprocess->stdin_fd);
    if (subprocess->stdout_fd >= 0) close(subprocess->stdout_fd);
    if (subprocess->stderr_fd >= 0) close(subprocess->stderr_fd);

    subprocess->stdin_fd = -1;
    subprocess->stdout_fd = -1;
    subprocess->stderr_fd = -1;

    // XXX evcom_writer_detach(&stdin_writer_);
    // XXX evcom_reader_detach(&stdout_reader_);
    // XXX evcom_reader_detach(&stderr_reader_);

    ev_child_stop(EV_DEFAULT_UC_ &subprocess->child_watcher);

    /* XXX Kill the PID? */
    subprocess->pid = 0;
}


