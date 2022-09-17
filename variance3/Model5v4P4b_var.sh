#!/bin/bash
#PBS -j oe
#PBS -N Model5v4P4b_var
#PBS -l select=1:ncpus=1:mem=2gb
#PBS -l walltime=2:00:00
#PBS -m ae
#PBS -M gergely.torda@jcu.edu.au


cd $PBS_O_WORKDIR
shopt -s expand_aliases
source /etc/profile.d/modules.sh
echo "Job identifier is $PBS_JOBID"
echo "Working directory is $PBS_O_WORKDIR"

  module load SLiM/3.6
  slim -t -m -d 'no='$no -d 'K='$K -d 'MS='$MS -d 'IM='$IM -d 'aIoC='$aIoC -d 'I='$I -d 'S='$S -d 'U='$U -d 'AGV='$AGV -d 'me='$me -d 'mr='$mr -d 'EN='$EN -d 'burnin'=$burnin -d 'WoFF'=$WoFF Model5v4P4b_var.slim
  wait    # Wait for background jobs to finish.
