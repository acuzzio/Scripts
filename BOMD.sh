#!/bin/bash

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

################# GetInitial.sh ##############
cat > GetInitial.sh << MORO
#!/bin/bash
#
# extract coordinates and velocities from Gaussian BOMD job
# 10/10/2012

if [ -z \$1 ]; then
echo ""
echo "Use this script in a Gaussian BOMD job folder and it will extract calculated coordinates and velocities:"
echo ""
echo "Use it like this:"
echo " \$ ./GetInitial.sh jobname.log"
echo ""
exit 0
fi

rm atomType 2> /dev/null

Project=\$1
atomsnum=\$(grep "NAtoms=" \$Project | awk '{print \$2}'| head -1)
splfile=tosplit
aT=atomType

grep -A\$((\$atomsnum+1)) "Symbolic Z-matrix:" \$Project | awk '{print \$1}' | tail -\$atomsnum  > \$aT

grep -A\$((\$atomsnum+9)) "Start point information" \$Project \\
 | grep -A\$((\$atomsnum-1)) " I=    1"  \\
 | grep -v '\-\-' \\
 | sed 's/D/E/g'  \\
 | awk '{printf "%10.6f %10.6f %10.6f \n" , \$4*0.529177249 , \$6*0.529177249 , \$8*0.529177249 }' > \$splfile
split -l\$atomsnum -a3 -d \$splfile geom

mkdir geoms
for i in \$(ls -d geom???)
do 
  paste \$aT \$i | sed "1s/^/ \$atomsnum\n\n/" > geoms/\$i.xyz 
done

rm geom??? \$splfile \$aT

grep -A\$((\$atomsnum*2+20)) "Start point information" \$Project \\
   | grep -A\$((\$atomsnum)) "MW cartesian velocity" \\
   | grep "I=" \\
   | sed 's/D/E/g' \\
   | awk '{printf "%17E %17E %17E \n" , \$4*2.4188843265E-17 , \$6*2.4188843265E-17 , \$8*2.4188843265E-17 }'  > \$splfile
split -l\$atomsnum -a3 -d \$splfile geom

mkdir velos
for i in \$(ls -d geom???)
do
   mv \$i velos/\$i.velocity.xyz
done

rm \$splfile 

mkdir initcond
mv geoms initcond/
mv velos initcond/

tar -zcf initialcondition.tgz initcond/

MORO
##############################################
chmod 744 GetInitial.sh

cd ..



