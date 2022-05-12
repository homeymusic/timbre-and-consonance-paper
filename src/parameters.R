RESOLUTION_BEHAVIOURAL_1D <- 1000
RESOLUTION_BEHAVIOURAL_2D <- 500

# This bandwidth is passed directly to MASS::kde2d.
# Warning: you have to divide this value by 4 to get the 'real' value,
# because kde2d is weird.
# So if you see that BANDWIDTH_GSP_2D = 1.5, 
# the true value to report is 0.375.
BANDWIDTH_GSP_2D <- 1.5

DEFAULT_RESOLUTION_MODEL_1D <- 1000 
DEFAULT_RESOLUTION_MODEL_2D <- 500

ROLL_OFF_SMOOTH <- 1.5
BEHAVIOURAL_SMOOTH_BROAD <- 0.2
BEHAVIOURAL_SMOOTH_NARROW <- 0.035

COHERENT_WAVES <- TRUE

BOOTSTRAP_REPS <- 1000
