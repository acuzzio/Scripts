#!/bin/bash
# oooh what a shitty script is this !!!

if [ -z $5 ] ; then
echo ""
echo "You can launch this script with:"
echo ""
echo " ./BOMB.sh etilene.xyz charge multiplicity temperature trajNumber"
echo "for example:"
echo " ./BOMB.sh name.xyz 1 1 298 100"
echo ""
echo "But you did not. So, which geometry .xyz file?"
read ans
echo "which charge?"
read char
echo "which multiplicity?"
read mult
echo "which temperature?"
read tem
echo "How many output trajectory?"
read traN
else
ans=$1
char=$2
mult=$3
tem=$4
traN=$5
fi

echo "working on: $ans"

i=${ans%.*}

mkdir $i

cp $ans $i
cd $i

################# Gaussian Job ###############
cat > $i.com.temp << MORO
%chk=$i.chk
%mem=2Gb
%nproc=8
#p HF/6-31G* bomd(maxpoints=1,ntraj=$traN,rtemp=$tem)

$i bomd 

$char $mult 
MORO
##############################################

sed -i '1,2d' $i.xyz

cat $i.com.temp $i.xyz > $i.com

### Samer says: GAUSSIAN NEED SPACES AT THE END BLA BLA BL
echo "" >> $i.com
echo "" >> $i.com

################ Shell Gaussian ##############
cat > gaus09.submit << MORO
#!/bin/bash
#PBS -N $i
#PBS -S /bin/sh
#PBS -l walltime=10:00:00
#PBS -l nodes=1:ppn=8
#PBS -l mem=3000mb
#PBS -l file=20GB 
#--------------------------------------------------------------#
NPROCS=\$(wc -l < \$PBS_NODEFILE)
cd \$PBS_O_WORKDIR
module load g09 
#--------------------------------------------------------------#
# Settings of the directories
#--------------------------------------------------------------#
export Project=\$PBS_JOBNAME
export WorkDir=\$TMPDIR
mkdir -p \$WorkDir
export InpDir=\$PBS_O_WORKDIR
echo \$HOSTNAME > \$InpDir/nodename
#--------------------------------------------------------------#
# Start job
#--------------------------------------------------------------#
cd \$WorkDir
cp \$InpDir/\$Project.chk \$WorkDir/
g09 < \$InpDir/\$Project.com >\$InpDir/\$Project.log
cp \$WorkDir/\$Project.chk \$InpDir/

MORO
##############################################
rm $i.xyz $i.com.temp
#qsub gaus09.submit


cd ..



