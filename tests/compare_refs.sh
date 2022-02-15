#!/bin/sh

wget https://qoiformat.org/qoi_test_images.zip
unzip qoi_test_images.zip

for qoi in `ls qoi_test_images/*.qoi`; do
    src=${qoi}
    ref=${qoi%.qoi}.png
    out=out.png

    ./bin/tests ${qoi} ${out}
done

for png in `ls qoi_test_images/*.png`; do
    src=${png}
    ref=${png%.png}.qoi
    out=out.qoi

    ./bin/tests ${png} ${out}
done
