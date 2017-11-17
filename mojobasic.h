/**
 * MojoBASIC; a modern reimplementation of QuickBASIC.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// This started as a copy/paste from MojoShader, which was written by Ryan and
//  is also under the zlib license.  https://icculus.org/mojoshader/

#ifndef _INCL_MOJOBASIC_H_
#define _INCL_MOJOBASIC_H_

#ifdef __cplusplus
extern "C" {
#endif

/* You can define this if you aren't generating mojobasic_version.h */
#ifndef MOJOBASIC_NO_VERSION_INCLUDE
#include "mojobasic_version.h"
#endif

#ifndef MOJOBASIC_VERSION
#define MOJOBASIC_VERSION -1
#endif

#ifndef MOJOBASIC_CHANGESET
#define MOJOBASIC_CHANGESET "???"
#endif

/*
 * For determining the version of MojoBASIC you are using:
 *    const int compiled_against = MOJOBASIC_VERSION;
 *    const int linked_against = MOJOBASIC_version();
 *
 * The version is a single integer that increments, not a major/minor value.
 */
int MOJOBASIC_version(void);

/*
 * For determining the revision control changeset of MojoBASIC you are using:
 *    const char *compiled_against = MOJOBASIC_CHANGESET;
 *    const char *linked_against = MOJOBASIC_changeset();
 *
 * The version is an arbitrary, null-terminated ASCII string. It is probably
 *  a hash that represents a revision control changeset, and can't be
 *  compared to any other string to determine chronology.
 *
 * Do not attempt to free this string; it's statically allocated.
 */
const char *MOJOBASIC_changeset(void);

/*
 * These are used with MOJOBASIC_error as special case positions.
 */
#define MOJOBASIC_POSITION_NONE (-3)
#define MOJOBASIC_POSITION_BEFORE (-2)
#define MOJOBASIC_POSITION_AFTER (-1)

typedef struct MOJOBASIC_error
{
    /*
     * non-zero if a fatal error, zero if it's just a warning.
     */
    int is_fatal;

    /*
     * Human-readable error, if there is one. Will be NULL if there was no
     *  error. The string will be UTF-8 encoded, and English only. Most of
     *  these shouldn't be shown to the end-user anyhow.
     */
    const char *error;

    /*
     * Filename where error happened. This can be NULL if the information
     *  isn't available.
     */
    const char *filename;

    /*
     * Position of error, if there is one. Will be MOJOBASIC_POSITION_NONE if
     *  there was no error, MOJOBASIC_POSITION_BEFORE if there was an error
     *  before processing started, and MOJOBASIC_POSITION_AFTER if there was
     *  an error during final processing. If >= 0, MOJOBASIC_compile() sets
     *  this to a a line number in the source code you supplied (starting
     *  at one).
     */
    int error_position;
} MOJOBASIC_error;



/* Preprocessor interface... */

/*
 * Structure used to pass predefined macros.
 *  You can have macro arguments: set identifier to "a(b, c)" or whatever.
 */
typedef struct MOJOBASIC_preprocessorDefine
{
    const char *identifier;
    const char *definition;
} MOJOBASIC_preprocessorDefine;

/*
 * Used with the MOJOBASIC_includeOpen callback.
 */
typedef enum
{
    MOJOBASIC_INCLUDETYPE_LOCAL,   /* local header: #include "blah.h" */
    MOJOBASIC_INCLUDETYPE_SYSTEM   /* system header: #include <blah.h> */
} MOJOBASIC_includeType;


/*
 * This callback allows an app to handle #include statements for the
 *  preprocessor. When the preprocessor sees an #include, it will call this
 *  function to obtain the contents of the requested file. This is optional;
 *  the preprocessor will open files directly if no callback is supplied, but
 *  this allows an app to retrieve data from something other than the
 *  traditional filesystem (for example, headers packed in a .zip file or
 *  headers generated on-the-fly).
 *
 * (inctype) specifies the type of header we wish to include.
 * (fname) specifies the name of the file specified on the #include line.
 * (parent) is a string of the entire source file containing the include, in
 *  its original, not-yet-preprocessed state. Note that this is just the
 *  contents of the specific file, not all source code that the preprocessor
 *  has seen through other includes, etc.
 * (outdata) will be set by the callback to a pointer to the included file's
 *  contents. The callback is responsible for allocating this however they
 *  see fit. This pointer must remain valid until the includeClose callback
 *  runs. This string does not need to be NULL-terminated.
 * (outbytes) will be set by the callback to the number of bytes pointed to
 *  by (outdata).
 *
 * The callback returns zero on error, non-zero on success.
 *
 * If you supply an includeOpen callback, you must supply includeClose, too.
 */
typedef int (*MOJOBASIC_includeOpen)(MOJOBASIC_includeType inctype,
                            const char *fname, const char *parent,
                            const char **outdata, unsigned int *outbytes);

/*
 * This callback allows an app to clean up the results of a previous
 *  includeOpen callback.
 *
 * (data) is the data that was returned from a previous call to includeOpen.
 *  It is now safe to deallocate this data.
 *
 * If you supply an includeClose callback, you must supply includeOpen, too.
 */
typedef void (*MOJOBASIC_includeClose)(const char *data);

/* Compiler interface... */

/* this is an opaque type produced by compiling BASIC code. */
typedef struct MOJOBASIC_program MOJOBASIC_program;

/*
 * Use this to compile BASIC programs.
 *
 * (filename) is a NULL-terminated UTF-8 filename. It can be NULL. We do not
 *  actually access this file, as we obtain our data from (source). This
 *  string is copied when we need to report errors while processing (source),
 *  as opposed to errors in a file referenced via the #include directive in
 *  (source). If this is NULL, then errors will report the filename as NULL,
 *  too.
 *
 * (source) is an UTF-8 string of valid BASIC source code.
 *  It does not need to be NULL-terminated.
 *
 * (sourcelen) is the length of the string pointed to by (source), in bytes.
 *
 * (include_open) and (include_close) let the app control the preprocessor's
 *  behaviour for '$INCLUDE metacommands. Both are optional and can be NULL,
 *  but both must be specified if either is specified.
 *
 * This function is thread safe, so long as the various callback functions
 *  are, too, and that the parameters remains intact for the duration of the
 *  call. This allows you to compile several programs on separate CPU cores
 *  at the same time.
 */
MOJOBASIC_program *MOJOBASIC_compileProgram(
                                    const char *fname, const char *source,
                                    unsigned int sourcelen,
                                    MOJOBASIC_includeOpen include_open,
                                    MOJOBASIC_includeClose include_close);


/*
 * Call this to dispose of compile results when you are done with them.
 *  Passing a NULL here is a safe no-op.
 *
 * This function is thread safe.
 */
void MOJOBASIC_freeProgram(MOJOBASIC_program *program);

unsigned int MOJOBASIC_getProgramErrorCount(const MOJOBASIC_program *program);
const MOJOBASIC_error *MOJOBASIC_getProgramError(const MOJOBASIC_program *program, const unsigned int idx);

#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of mojobasic.h ... */

