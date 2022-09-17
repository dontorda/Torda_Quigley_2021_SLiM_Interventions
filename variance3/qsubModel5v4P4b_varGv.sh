#!/bin/bash
#PBS -j oe
#PBS -N qsubModel5v4P4b_varGv
#PBS -l select=1:ncpus=1:mem=1gb
#PBS -l walltime=1:00:00
#PBS -m ae
#PBS -M gergely.torda@jcu.edu.au

echo "------------------------------------------------------"
echo "PBS: Submitted to $PBS_QUEUE@$PBS_O_HOST"
echo "PBS: Working directory is $PBS_O_WORKDIR"
echo "PBS: Job identifier is $PBS_JOBID"
echo "PBS: Job name is $PBS_JOBNAME"
echo "------------------------------------------------------"
for no in {2..10}
do
      for U in 60 70 80 90 120 150 200
      do
        #constant Gv and He
        qsub Model5v4P4b_varGv.sh -v no=$no,K=10000,aIoC=50,MS=0.01,IM=1,I=0.5,S=100,U=$U,AGV=0,mr=0,EN=10000,burnin=100,WoFF=0.1
      done
done
