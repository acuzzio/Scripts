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
cat > GetInitial.sh <<MORO
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

rm lead 2> /dev/null

Project=\$1
atomsnum=\$(grep "NAtoms=" \$Project | awk '{print \$2}'| head -1)

grep -A\$atomsnum "Symbolic Z-Matrix:" \$Project  | awk '{print \$1}' | tail -\$atomsnum  > lead

    mkdir tempfolder
    cp lead tempfolder/
    cp \$1 tempfolder/
    cd tempfolder
    grep -A\$((\$atomsnum+9)) "Start point information" \$Project | grep -A\$((\$atomsnum-1)) " I=    1" | sed 's/D/E/g' > all
    awk '{printf "%10.6f %10.6f %10.6f \\n " , \$4*0.529177249 , \$6*0.529177249 , \$8*0.529177249 }' all >tosplit
    split -l\$((\$atomsnum+1)) -a3 -d tosplit geom
    for i in \$(ls -d geom*) ; do paste lead \$i > \$i.temp ; done
    for i in \$(ls -d geom*.temp) ; do sed '1i\\\\' \$i >\$i.good ; done
    for i in \$(ls -d geom*.temp.good) ; do sed "1i\\\\\$atomsnum\\\\" \$i >\$i.cool ; done
   for i in \$(ls -d geom*.temp.good.cool) ; do head -n\$((\$atomsnum+2)) \$i > \$i.great ; done
    rename temp.good.cool.great xyz geom*.temp.good.cool.great
    cd ../
    mkdir geoms
    cp tempfolder/geom*.xyz geoms/
    rm -r tempfolder

   mkdir tempfolder2
   cp lead tempfolder2/
   cp \$1 tempfolder2/
   cd tempfolder2
   grep -A\$((\$atomsnum*2+20)) "Start point information" \$Project | grep -A\$((\$atomsnum)) "MW cartesian velocity" | grep -A\$((\$atomsnum-1)) " I=    1" | sed 's/D/E/g' >all
   awk '{printf "%17E %17E %17E \\n " , \$4*2.4188843265E-17 , \$6*2.4188843265E-17 , \$8*2.4188843265E-17 }' all >tosplit
   split -l\$((\$atomsnum+1)) -a3 -d tosplit geom
   for i in \$(ls -d geom*) ; do head -n\$((\$atomsnum)) \$i > \$i.velocity.xyz ; done
   cd ../
   mkdir velos
   cp tempfolder2/geom*.xyz velos/
   rm -r tempfolder2
   cd velos
   cp ../lead ./
   for i in \$(ls -d geom*) ; do paste lead \$i > \$i.1 ; done
   for i in \$(ls -d geom*.1) ; do sed -i 's/C/12/g' \$i ; done
   for i in \$(ls -d geom*.1) ; do sed -i 's/N/14/g' \$i ; done
   for i in \$(ls -d geom*.1) ; do sed -i 's/H/1/g' \$i ; done
   for i in \$(ls -d geom*.1) ; do awk '{printf "%17E %17E %17E \\n " , \$2/sqrt(\$1), \$3/sqrt(\$1), \$4/sqrt(\$1) }' \$i > \$i.2 ; done
   mkdir ../tempfolder3
   mv geom*.2 ../tempfolder3
   cd ../tempfolder3
   rename .1.2 "" geom*
   cd ../velos
   rm ./*
   mv ../tempfolder3/* .
   rmdir ../tempfolder3
   cd ..
   rm lead
   mkdir initcond
   mv geoms initcond/
   mv velos initcond/
   tar -zcf initialcondition.tgz initcond/
   rm -r geoms velos
   sss initialcondition.tgz

MORO
##############################################
chmod 744 GetInitial.sh

cd ..



