#! /usr/bin/make -f

dams:=https://sandbox.dams.library.ucdavis.edu/fcrepo/rest/collection/sherry-lehmann/catalogs
template:=template.qlr
now:=$(shell date +%s)
ark:=
qlr:=$(patsubst %,%.qlr,${ark})


# Catalog function
cat=$1
catalog=$(word 1,$(subst -, ,$1))

INFO:
	$(foreach a,${ark},$(warning $a,$(call catalog,$a)))

qlr:${qlr}

define qlr
qlr:$1.qlr
$1.qlr: ark_87287/$(call catalog,$1)/media/images/$1.jpg
	cat ${template} | sed -e 's/\[\[ark\]\]/$1/' -e 's/\[\[catalog\]\]/$(call catalog,$1)/g' -e 's/\[\[timestamp\]\]/${now}/g' < ${template} > $1.qlr

ark_87287/$(call catalog,$1)/media/image/$1.jpg:
	[[ -d ark_87287/$(call catalog,$1)/media/images ]] | mkdir -p ark_87287/$(call catalog,$1)/media/images
	wget -O ark_87287/$(call catalog,$1)/media/images/$1.jpg ${dams}/$(call catalog,$1)/media/images/$1
endef

$(foreach a,${ark},$(eval $(call qlr,$a)))
