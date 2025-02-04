#!/bin/bash

#PBS -q l                      #Specify batch que [l:Medium-scale calc., s:Small-scale calc.]
#PBS -T intmpi                 #Required option for Intel MPI

#PBS -b 4                      #Node number employed by a jobt      [for "l" scale job: 1-64]
#PBS -l cpunum_job=40          #CPU core number employed by a node  [1-40]
#PBS -l elapstim_req=00:30:00  #Max time for a job (hh:mm:dd)       [for "l" scale job: max 48 hours]

#PBS -o /work/G10203/hsato/log/stdout.%s.txt #Standard output in      "stdout.<requiest ID>.txt"
#PBS -e /work/G10203/hsato/log/stderr.%s.txt #Standar Error output in "stderr.<requiest ID>.txt"

#リクエスト投入ディレクトリに移動
cd $PBS_O_WORKDIR

#Execute MPI job using 4nodes * 40cores = 160cores
mpirun -f ${NQSII_MPINODES} -ppn 40 -np 160 ./go.out


#### Personal memo for DA-system
#### Execution command of "s" (small scale) job: "qsub -q s go_s.bat"
#### Execution command of "l" (large scale) job: "qsub -q l go_l.bat"

