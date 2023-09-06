/*
 * r_fgeneric.c - R wrapper of the C fgeneric interface.
 *
 * Author:
 *   Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 */

#include <stdio.h>
#include <stdarg.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

#include "bbobStructures.h"

#define CHECK_ARG_IS_REAL_VECTOR(A)			 \
    if (!isReal(A) || !isVector(A))			 \
	error("Argument '" #A "' is not a real vector.");

#define CHECK_ARG_IS_INT_VECTOR(A)			     \
    if (!isInteger(A) || !isVector(A))			     \
	error("Argument '" #A "' is not an integer vector.");

#define CHECK_ARG_IS_STRING(A)                             \
    if (!isString(A) || !isVector(A))		           \
	error("Argument '" #A "' is not a string vector.");

#define UNPACK_STRING(S, C)               \
    CHECK_ARG_IS_STRING(S)                \
    const char *C = CHAR(STRING_ELT(S, 0));

#define UNPACK_INT(S, I)	\
    CHECK_ARG_IS_INT_VECTOR(S);	\
    int I = INTEGER(S)[0];

#define UNPACK_REAL_VECTOR(S, D, N) \
    CHECK_ARG_IS_REAL_VECTOR(S);    \
    double *D = REAL(S);            \
    const R_len_t N = length(S);              

typedef enum {initialized_state, 
              running_state,
              uninitialized_state,
              error_state} state_t;

static state_t current_state = uninitialized_state;
static ParamStruct params;

/* Clean up preprocessor namespace */
#ifdef ERROR
#undef ERROR
#endif
#ifdef WARNING
#undef WARNING
#endif

/*
 * ERROR - issue formated error message through the R facilities for
 *   error handling. Also mark the state as 'error_state' thereby
 *   blocking any further use until a restart. There is no clean way
 *   to recover from an ERROR() call.
 */
void ERROR(const char *fmt, ...) {
    char msg[1024];
    va_list argp;
    va_start(argp, fmt);
    vsnprintf(msg, 1023, fmt, argp);
    va_end(argp);
    current_state = error_state;
    error(msg);
}

/*
 * WARNING - issue formated warning message through the R facilities
 *   for error handling.
 */
void WARNING(const char *fmt, ...) {
    char msg[1024];
    va_list argp;
    va_start(argp, fmt);
    vsnprintf(msg, 1023, fmt, argp);
    va_end(argp);
    warning(msg);
}

/*
 * do_bbob_setup_experiment - setup framework for an experiment.
 *
 * NOTE: The R code is responsible for creating the directory
 *   structure below 's_datapath'.
 */
SEXP do_bbob_setup_experiment(SEXP s_algorithm, SEXP s_datapath, SEXP s_comment,
                              SEXP s_function_id, SEXP s_instance_id, SEXP s_dimension) {

    if (current_state == running_state)
        error("Experiment still live. Please call 'do_bbob_end_experiment' "
              "before starting the next experiment.");

    if (current_state == initialized_state) {
        fgeneric_finalize();
        warning("Overwriting previously initialized experiment.");
    }

    if (current_state != uninitialized_state) {
        error("Framework not in uninitialized state. Maybe a fatal error "
              "occured in a previous run?");
    }

    UNPACK_STRING(s_algorithm, algorithm);
    UNPACK_STRING(s_comment, comment);
    UNPACK_STRING(s_datapath, datapath);
    UNPACK_INT(s_function_id, function_id);
    UNPACK_INT(s_instance_id, instance_id);
    UNPACK_INT(s_dimension, dimension);

    current_state = initialized_state;
    params = fgeneric_getDefaultPARAMS();
    strcpy(params.dataPath, datapath);
    strcpy(params.algName, algorithm);
    strcpy(params.comments, comment);
    
    params.DIM = dimension;
    params.funcId = function_id;
    params.instanceId = instance_id;
    double f_opt = fgeneric_initialize(params);
    
    return ScalarReal(f_opt);
}

/*
 * do_bbob_log_restart - log a restart in the result file.
 */
SEXP do_bbob_log_restart(SEXP s_reason) {
    if (current_state != initialized_state &&
        current_state != running_state) {
        error("Need to initialize framework before logging a restart.");
    }
    UNPACK_STRING(s_reason, reason);
    fgeneric_restart(reason);
    return R_NilValue; /* return nothing */
}

/*
 * do_bbob_end_experiment - finish up an experiment.
 */
SEXP do_bbob_end_experiment() {
    if (current_state == running_state ||
        current_state == initialized_state) {
        fgeneric_finalize();
        current_state = uninitialized_state;
    }
    return R_NilValue; /* return nothing */
}

/*
 * do_bbob_f_eval - perform a single function evaluation.
 *
 * Calling code is responsible to first call do_bbob_setup_experiment
 * to initialize the framework.
 */
SEXP do_bbob_f_eval(SEXP s_x) {
    UNPACK_REAL_VECTOR(s_x, x, n_x);
    if (current_state != initialized_state &&
        current_state != running_state) {
        error("Need to initialize framework before first function evaluation.");
    }
    if (n_x != params.DIM) {
        error("Dimensionality of 'x' does not fit current experimental setup.");
    }
    current_state = running_state;
    double res = fgeneric_evaluate(x);
    return ScalarReal(res);
}

SEXP do_bbob_n_evaluations() {
    if (current_state != initialized_state &&
        current_state != running_state) {
        error("Need to initialize framework before querying # of "
              "performed function evaluations.");
    }
    int nevals = fgeneric_evaluations();
    return ScalarInteger(nevals);
}

SEXP do_bbob_optimality_gap() {
    double gap = fgeneric_best() - fgeneric_ftarget();
    return ScalarReal(gap);
}

