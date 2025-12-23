# Pooling optimization monte carlo simulation

Two phases of simulation are in this repository to compare pooling optimization methods in their relative performance for test conservations (MCS) and processing time under different laboratory capacities (DES).
The monte carlo simulation uses historical data from surveillance and clinical testing in binary form to simulate different pooling optimization methods.
the MCS outputs the test count to complete each historical sample submission using different pooling methods to determine which achieves the greatest savings.
The median test counts of each category (pools, individual tests, subsampled pools for prevalence estimation, retested samples from positive pools) are summed for each day of testing and used as input for the processing time DES.


# Discrete event simulation for lab sample processing

This is a code repository for simulations estimating the sample throughput for surveillance labs. 
The current definitions of the type of samples and the number of lab stations are hard-coded in the workflow, although work is ongoing to make a more generalized version.
Upon release of an updated version, this would become legacy code.

There are two simulations included in this repo that run across the sample requirements for different days and pooling methodologies.
The input of these simulations is the output of the Pooling Optimization Monte Carlo simulation, which is also available in this repo. 

The first (main_single_input_simulation.py) estimates the completion time and plate requirements of samples for a given day, not considering any pre-existing or future queues. 
The output is the number and completion time of each plate needed to process the given samples.

The second (main_multiple_input_simulation.py) considers the samples on a day to day basis and carries over unfinished samples to the next day. 
In this scenario, large inputs of samples can pile up leading to slower completion time for new submissions. 
This simulation was only ran for one week at a time, with several different weeks subset for analysis. 

Simulations can be ran through a terminal by navigating to the 'python_des' path and running:

python main_single_input_simulation.py
or
python main_multiple_input_simulation.py
