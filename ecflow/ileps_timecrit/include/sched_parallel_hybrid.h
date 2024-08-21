#SBATCH --qos=tp
#SBATCH --time=01:00:00
#SBATCH --mem-bind=local
#SBATCH --nodes=3
#SBATCH --ntasks-per-node=8
#SBATCH --sockets-per-node=2
#SBATCH --ntasks-per-socket=128
#SBATCH --cpus-per-task=32
#SBATCH --ntasks-per-core=2

export OMP_NUM_THREADS=32
