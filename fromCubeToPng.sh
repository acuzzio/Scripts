#!/bin/bash
for i in $(ls *.cube)
do
ii=${i%.*}
cat > ScriptVmd$ii << MORO
mol new $i
mol new $i
mol delete 1
display projection Orthographic
color Display Background white
menu graphics on
mol modstyle 0 0 CPK 1.000000 0.300000 10.000000 10.000000
mol color Name
mol addrep 0
mol modmaterial 1 0 Transparent
material change opacity Transparent 0.800000
mol modstyle 1 0 Isosurface 0.018000 0 0 0 1 1 
mol modcolor 1 0 ColorID 15
mol addrep 0
mol modstyle 2 0 Isosurface -0.018000 0 0 0 1 1
mol modmaterial 2 0 Transparent
mol modcolor 2 0 ColorID 1
axes location Off
scale by 1.700000
scale by 1.700000
render TachyonInternal ${ii}.tga display %s
exit
MORO
vmd -size 800 400 -dispdev text -eofexit -e ScriptVmd$ii
rm ScriptVmd$ii
convert ${ii}.tga ${ii}.png
done
rm *.tga
montage -label '%f' *.png -tile 3x4 -geometry 800x400 allOrbitals.png
rm *.png

