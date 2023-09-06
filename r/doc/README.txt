Quick Installation
~~~~~~~~~~~~~~~~~~

Run the following commands in your R session:

.. sourcecode:: r

    install.packages(c("BBmisc", "stringr"), 
                     repos="http://cran.at.r-project.org")
    fn <- file.path(tempdir(), "bbob_current.tar.gz")
    download.file("http://coco.lri.fr/downloads/downloadVERSIONNUM/bbobr.tar.gz",
                  destfile=fn)
    install.packages(fn, repos=NULL)
    file.remove(fn)     

You should now be able to load the package by running

.. sourcecode:: r

    library("bbob")

If all went well, you can skip down to the next section. Otherwise
consulte the detailed instructions which follow.

Detailed Installation
~~~~~~~~~~~~~~~~~~~~~

Before you start, install the required dependencies. At the R prompt
enter

.. sourcecode:: r

    install.packages(c("BBmisc", "stringr"))

This should download and install the two packages. Now download the
current BBOB R source package and save it somewhere on your hard
drive. The most up-to-date version is always available `here
<http://coco.lri.fr/downloads/downloadVERSIONNUM/bbobr.tar.gz>`_, but
there should also be a stable version available on the `COCO website
<http://coco.gforge.inria.fr/doku.php?id=bbob-2012-downloads>`_. Now
it's time to install the package. In R, run the following command:

.. sourcecode:: r

    install.packages("/path/to/bbob_current.tar.gz", repos=NULL)

Note that you will have to adjust the path and possibly the filename
to match the location where you stored the downloaded package on your
hard drive. On Windows you will also need to have the Rtools_
installed for this to work since the package contains C code. If you
have any problems with this step, do not hesitate to contact me_ for
assistance.

After completing the above steps, you should be able to load the
package in R

.. sourcecode:: r

    library("bbob")
     
The help page for the `bbo_benchmark` function should get you started
if you do not want to continue reading this introduction.

.. sourcecode:: R

    ?bbo_benchmark

Simple experiment
~~~~~~~~~~~~~~~~~

If you already have an optimizer ready to go in R and all you want to
do is produce a BBOB dataset for post-processing, then this section
will walk you through the required steps. We will use the high-level
interface `bbo_benchmark` in this example. If you want to parallelize
your optimization runs, need to perform complex initializations or
just want full control, skip down to the next section for a brief tour
of the low-level interface.

In this example we will use the L-BFGS-B optimizer included in base
R. You will need to adapt parts of the code to suit your
optimizer. The first step is to wrap your optimization algorithm in a
function with the following signature

.. sourcecode:: r

    function(par, fun, lower, upper, max_eval)

where `par` will be a numeric vector with the starting point for the
optimization, `fun` is the function to be minimized, `lower` and
`upper` are the bounds of the box constraints and `max_eval` is the
number of function evaluations left in the allocated budget. What
these five parameters mean is best conveyed by an example. Here we
wrap the `optim` function included in base R

.. sourcecode:: r

    my_optimizer <- function(par, fun, lower, upper, max_eval) {
      optim(par, fun, method="L-BFGS-B", 
            lower=lower, upper=upper, control=list(maxit=max_eval))
    }

If your algorithm does not have the notion of an initial parameter
setting, you can safely ignore the `par` parameter in your
implementation. You might also notice, that we do not strictly adhere
to the `max_eval` limit because the number of iterations is generally
not equal to the number of function evaluations for L-BFGS-B. This is
OK. `max_eval` is only a hint to the optimizer how much effort it
should put into optimizing the function.

Should your algorithm perform restarts internally it is possible to
log these using the `bbob_log_restart` function. The function takes
exactly one argument, a string describing the reason for the restart.

We are now ready to run our experiment. This is done by calling the
`bbo_benchmark` function

.. sourcecode:: r

    bbo_benchmark(my_optimizer, "l-bfgs-b", "optim_l-bfgs-b")

which will perform the BBOB experiments (caution, may take many
hours). The first argument passed to `bbo_benchmark` is our
optimization wrapper from the previous step. Make sure that it has the
correct function signature! Next we supply the so called *algorithm
id*. This is a short descriptive name for our optimization
algorithm. Ideally it should include the package name and version
which contains the algorithm. So for for `genoud` from the `rgenoud`
package, we might use `rgenoud::genoud (5.7-3)` as the algorithm
id. The last required argument is the name of the base directory where
the result files will be stored. Again, it is a good idea to include
the algorithm name in the directory name. If no other arguments are
given, this runs a complete BBO benchmark on the noiseless test
functions. This includes all instances (1-5 and 21-30), all dimensions
(2, 3, 5, 10, 20, 40). If you do not want to include runs in 40
dimensions or want to use different instances you can change the
defaults using the `dimensions` and `instances` arguments to
`bbo_benchmark`. For details, see the manual page for `bbo_benchmark`.

If no other budget is specified, `bbo_benchmark` will perform random
independent restarts of your algorithm until the desired target
precision (1e-8) is reached or the default budget of 10000000 function
evaluations is exhausted. If you want to reduce the budget, you can by
specifiying it as the `budget` argument to `bbo_benchmark`.

To run the required timing experiment, execute the following code:

.. sourcecode:: r

    bbo_timing(my_optimizer)

It will return a data frame with the relevant timing information.

Low-level interface
~~~~~~~~~~~~~~~~~~~

Will follow soon.

.. _Rtools: http://cran.r-project.org/bin/windows/Rtools/
.. _me: mailto:olafm@p-value.net
