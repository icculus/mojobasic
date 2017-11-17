#include <stdio.h>
#include "mojobasic.h"

int main(int argc, char **argv)
{
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    for (int i = 1; i < argc; i++)
    {
        FILE *io = fopen(argv[i], "rb");
        if (!io)
        {
            fprintf(stderr, "failed to open '%s'\n", argv[i]);
            continue;
        } // if

        static char buf[1024 * 64];
        const ssize_t br = fread(buf, 1, sizeof (buf), io);
        fclose(io);

        if (br <= 0)
        {
            fprintf(stderr, "no read from '%s'\n", argv[i]);
            continue;
        } // if

        printf("; compiling '%s'...\n", argv[i]);

        MOJOBASIC_program *program = MOJOBASIC_compileProgram(argv[i], buf, (unsigned int) br, NULL, NULL);
        if (program == NULL)
        {
            printf("massive failure!\n");
            continue;
        } // if

        const unsigned int numerrs = MOJOBASIC_getProgramErrorCount(program);
        for (unsigned int i = 0; i < numerrs; i++)
        {
            const MOJOBASIC_error &err = *MOJOBASIC_getProgramError(program, i);
            printf("%s:%d %s: %s\n", err.filename, err.error_position, err.is_fatal ? "error" : "warning", err.error);
        } // for

        MOJOBASIC_freeProgram(program);
    } // for

    return 0;
} // main

// end of test.cpp ...

