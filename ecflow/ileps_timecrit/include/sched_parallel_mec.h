#SBATCH --qos=np
#SBATCH --nodes=8
#SBATCH --ntasks-per-node=8
#SBATCH --hint=nomultithread
#SBATCH --export=ALL,STHOST=ws1
