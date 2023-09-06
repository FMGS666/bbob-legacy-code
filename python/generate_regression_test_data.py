#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Runs an entire experiment for benchmarking PURE_RANDOM_SEARCH on a testbed.

CAPITALIZATION indicates code adaptations to be made.
This script as well as files bbobbenchmarks.py and fgeneric.py need to be
in the current working directory.

Under unix-like systems: 
    nohup nice python exampleexperiment.py [data_path [dimensions [functions [instances]]]] > output.txt &

"""
import sys # in case we want to control what to run via command line args
import time
import numpy as np
import fgeneric
import pickle
import bbobbenchmarks

argv = sys.argv[1:] # shortcut for input arguments

datapath = 'PUT_MY_BBOB_DATA_PATH' if len(argv) < 1 else argv[0]

dimensions = (2, 3, 5, 10, 20, 40) if len(argv) < 2 else eval(argv[1])
#function_ids = bbobbenchmarks.nfreeIDs if len(argv) < 3 else eval(argv[2])  
function_ids = bbobbenchmarks.noisyIDs if len(argv) < 3 else eval(argv[2])
instances = range(1,16) if len(argv) < 4 else eval(argv[3])

opts = dict(algid='PUT ALGORITHM NAME',
            comments='PUT MORE DETAILED INFORMATION, PARAMETER SETTINGS ETC')
maxfunevals = '10 * dim' # 10*dim is a short test-experiment taking a few minutes 
# INCREMENT maxfunevals SUCCESSIVELY to larger value(s)
minfunevals = 'dim + 2'  # PUT MINIMAL sensible number of EVALUATIONS before to restart
maxrestarts = 10000      # SET to zero if algorithm is entirely deterministic 


def run_optimizer(f, fun, dim, maxfunevals, id_string, ftarget=-np.Inf):
    """start the optimizer, allowing for some preparation. 
    This implementation is an empty template to be filled 
    
    """
    # prepare
    x_start = 8. * np.random.rand(dim) - 4
    
    # call, REPLACE with optimizer to be tested
    return PURE_RANDOM_SEARCH(f, fun, x_start, maxfunevals, ftarget, id_string)

def PURE_RANDOM_SEARCH(f, fun, x, maxfunevals, ftarget, id_string):
    """samples new points uniformly randomly in [-5,5]^dim and evaluates
    them on fun until maxfunevals or ftarget is reached, or until
    1e8 * dim function evaluations are conducted.

    """
    dim = len(x)
    maxfunevals = min(1e8 * dim, maxfunevals)
    popsize = 1
    fbest = np.inf
    history = []
    for _ in range(0, int(np.ceil(maxfunevals / popsize))):
        xpop = 10. * np.random.rand(popsize, dim) - 5.
        fvalues, ftrue = fun(xpop)
        for idx, x in enumerate(xpop):
            assert(len(x.shape) == 1 and x.shape[0] == dim), x.shape
            history_ = {"f_id": id_string, "x": x, "fvalue": fvalues[idx], "ftrue": ftrue[idx], "_randomnseed": bbobbenchmarks._randomnseed, "_randomseed": bbobbenchmarks._randomseed, "noise_value": f.noise_value, "boundary_handling": f.boundaryhandling(x)}
            history.append(history_)
        idx = np.argsort(fvalues)
        if fbest > fvalues[idx[0]]:
            fbest = fvalues[idx[0]]
            xbest = xpop[idx[0]]
        if fbest < ftarget:  # task achieved 
            break
    return xbest, history

if __name__ == "__main__":
    t0 = time.time()
    np.random.seed(int(t0))

    f = fgeneric.LoggingFunction(datapath, **opts)
    results = []
    for dim in dimensions:  # small dimensions first, for CPU reasons
        for fun_id in function_ids:
            for iinstance in instances:
                f.setfun(*bbobbenchmarks.instantiate(fun_id, iinstance=iinstance))
                id_string = "f%d_i%02d_d%02d" % (fun_id, iinstance, dim) 
                # independent restarts until maxfunevals or ftarget is reached
                for restarts in xrange(maxrestarts + 1):
                    if restarts > 0:
                        f.restart('independent restart')  # additional info
                    xbest, history_ = run_optimizer(f.fun, f.evalfun, dim,  eval(maxfunevals) - f.evaluations,
                                id_string, f.ftarget)
                    for eval_ in history_:
                        results.append(eval_)
                    if (f.fbest < f.ftarget
                        or f.evaluations + eval(minfunevals) > eval(maxfunevals)):
                        break

    dump_path = "python/bbob_noisy_regression_benchmark_data/bbob_noisy_regression_data.json"
    print("writing file to " + dump_path)
    with open(dump_path, "wb") as file_:
        pickle.dump(results, file_)
    print("file written successfully")
