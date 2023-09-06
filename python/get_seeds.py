
import sys # in case we want to control what to run via command line args
import time
import pickle
import numpy as np
import fgeneric
import bbobbenchmarks
import re

argv = sys.argv[1:] # shortcut for input arguments

datapath = 'PUT_MY_BBOB_DATA_PATH' if len(argv) < 1 else argv[0]

dimensions = (2, 3, 5, 10, 20, 40) if len(argv) < 2 else eval(argv[1])
#function_ids = bbobbenchmarks.nfreeIDs if len(argv) < 3 else eval(argv[2])  
function_ids = bbobbenchmarks.noisyIDs if len(argv) < 3 else eval(argv[2])
instances = range(1, 16) if len(argv) < 4 else eval(argv[3])
maxrestarts = 10000      # SET to zero if algorithm is entirely deterministic 

maxfunevals = '10 * dim' # 10*dim is a short test-experiment taking a few minutes 
# INCREMENT maxfunevals SUCCESSIVELY to larger value(s)
minfunevals = 'dim + 2'  # PUT MINIMAL sensible number of EVALUATIONS before to restart

opts = dict(algid='PUT ALGORITHM NAME',
            comments='PUT MORE DETAILED INFORMATION, PARAMETER SETTINGS ETC')

def run_optimizer(fun, dim, maxfunevals, ftarget=-np.Inf):
    """start the optimizer, allowing for some preparation. 
    This implementation is an empty template to be filled 
    
    """
    # prepare
    x_start = 8. * np.random.rand(dim) - 4
    
    # call, REPLACE with optimizer to be tested
    return PURE_RANDOM_SEARCH(fun, x_start, maxfunevals, ftarget)

def PURE_RANDOM_SEARCH(fun, x, maxfunevals, ftarget):
    """samples new points uniformly randomly in [-5,5]^dim and evaluates
    them on fun until maxfunevals or ftarget is reached, or until
    1e8 * dim function evaluations are conducted.

    """
    dim = len(x)
    maxfunevals = min(1e8 * dim, maxfunevals)
    popsize = int(min(maxfunevals, 200))
    fbest = np.inf
    history = dict()
    for _ in range(0, int(np.ceil(maxfunevals / popsize))):
        xpop = 10. * np.random.rand(popsize, dim) - 5.
        fvalues, ftrue = fun(xpop)
        for idx, x in enumerate(xpop):
            history[tuple(x)] = (fvalues[idx], ftrue[idx])
        idx = np.argsort(fvalues)
        if fbest > fvalues[idx[0]]:
            fbest = fvalues[idx[0]]
            xbest = xpop[idx[0]]
        if fbest < ftarget:  # task achieved 
            break
    return history

if __name__ == "__main__":
    t0 = time.time()
    np.random.seed(int(t0))
    f = fgeneric.LoggingFunction(datapath, **opts)
    seeds = dict()
    for dim in dimensions:  # small dimensions first, for CPU reasons
        for fun_id in function_ids:
            for iinstance in instances:

                id_string = "f%d_i%02d_d%02d" % (fun_id, iinstance, dim) 
                f.setfun(*bbobbenchmarks.instantiate(fun_id, iinstance=iinstance))
                history_full = []
                for restarts in xrange(maxrestarts + 1):
                    if restarts > 0:
                        f.restart('independent restart')  # additional info
                    history_ = run_optimizer(f.evalfun, dim,  eval(maxfunevals) - f.evaluations,
                                f.ftarget)
                    if (f.fbest < f.ftarget
                        or f.evaluations + eval(minfunevals) > eval(maxfunevals)):
                        break
                seeds[id_string] = f.fun.rseed


    dump_path = "python/bbob_noisy_regression_benchmark_data/bbob_noisy_seeds.json"
    with open(dump_path, "wb") as file_:
        pickle.dump(seeds, file_)
    print("file written successfully")