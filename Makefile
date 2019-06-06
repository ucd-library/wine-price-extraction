#! /usr/bin/make -f

dams:=https://sandbox.dams.library.ucdavis.edu/fcrepo/rest/collection/sherry-lehmann/catalogs
template:=template.qlr
now:=$(shell date +%s)
ark:=
qlr:=$(patsubst %,%.qlr,${ark})


define pod

=pod

=head1 SYNOPSIS

  make [-n] <command>
  where command is one of: alias

This Makefile is used manage some files in an archive.

  eval $(make alias)

Autogenerates some TTL files if they are missing.  This is for the images and the image directories.

  make import

Will use tesseract to autogenerate OCR text versions of the images.

=cut

endef

.PHONY: INFO check alias

INFO::
	@pod2usage -exit 0 ${MAKEFILE_LIST}

check::
	@podchecker ${MAKEFILE_LIST}


alias:
	@echo "alias sloan-dc='docker-compose -f ${PWD}/sloan.yml -p sloan'"

# Catalog function
cat=$1
catalog=$(word 1,$(subst -, ,$1))

INFO::
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
