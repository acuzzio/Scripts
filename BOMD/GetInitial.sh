#!/bin/bash
#
# extract coordinates and velocities from Gaussian BOMD job
# 10/10/2012

if [ -z $1 ]; then
echo ""
echo "Use this script in a Gaussian BOMD job folder and it will extract calculated coordinates and velocities:"
echo ""
echo "Use it like this:"
echo " $ ./GetInitial.sh jobname.log"
echo ""
exit 0
fi

rm atomType 2> /dev/null

Project=$1
prj=${Project%.*}
atomsnum=$(grep "NAtoms=" $Project | awk '{print $2}'| head -1)
splfile=tosplit
aT=atomType

grep -A$(($atomsnum+1)) "Symbolic Z-matrix:" $Project | awk '{print $1}' | tail -$atomsnum  > $aT

grep -A$(($atomsnum+9)) "Start point information" $Project \
 | grep -A$(($atomsnum-1)) " I=    1"  \
 | grep -v '\-\-' \
 | sed 's/D/E/g'  \
 | awk '{printf "%10.6f %10.6f %10.6f \n" , $4*0.529177249 , $6*0.529177249 , $8*0.529177249 }' > $splfile
split -l$atomsnum -a3 -d $splfile geom

mkdir geoms
for i in $(ls -d geom???)
do 
  paste $aT $i | sed "1s/^/ $atomsnum\n\n/" > geoms/$i.xyz 
done

rm geom??? $splfile $aT

grep -A$(($atomsnum*2+20)) "Start point information" $Project \
   | grep -A$(($atomsnum)) "MW cartesian velocity" \
   | grep "I=" \
   | sed 's/D/E/g' \
   | awk '{printf "%17E %17E %17E \n" , $4*2.4188843265E-17 , $6*2.4188843265E-17 , $8*2.4188843265E-17 }'  > $splfile
split -l$atomsnum -a3 -d $splfile geom

mkdir velos
for i in $(ls -d geom???)
do
   mv $i velos/$i.velocity.xyz
done

rm $splfile 

fol=${prj}_initCond

mkdir ${fol}
mv geoms ${fol}
mv velos ${fol}



