#!/bin/bash
#SBATCH -N 2
#SBATCH -C haswell
#SBATCH -q regular
#SBATCH -J Initial_MAESTRO_flamewave
#SBATCH --mail-user=cjdegrendele@gmail.com
#SBATCH --mail-type=ALL
#SBATCH -t 00:25:00

#OpenMP settings:
export OMP_NUM_THREADS=1
export OMP_PLACES=threads
export OMP_PROC_BIND=spread


#run the application:
srun -n 64 -c 2 --cpu_bind=cores /global/homes/c/chrisdeg/2020/MAESTROeX/Exec/science/sdctest/Maestro2d.gnu.haswell.MPI.SDC.ex inputs_2d_smallscale_sdc_4
