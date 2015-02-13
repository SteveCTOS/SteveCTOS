project (ctools_installs)

file (MAKE_DIRECTORY
  /ctools/bin
  /ctools/cr
  /ctools/data01
  /ctools/data02
  /ctools/data03
  /ctools/data04
  /ctools/data05
  /ctools/data06
  /ctools/data07
  /ctools/data08
  /ctools/data09
  /ctools/data10
  /ctools/data11
  /ctools/data12
  /ctools/debtor
  /ctools/dr
  /ctools/ecredt
  /ctools/einvoc
  /ctools/epordr
  /ctools/eremit
  /ctools/estate
  /ctools/fax
  /ctools/fx
  /ctools/kbd
  /ctools/lib
  /ctools/lock
  /ctools/ps
  /ctools/spl
  )

file (COPY ddexport.sh  DESTINATION /ctools/bin FILE_PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ)
file (COPY b20codes.kbd DESTINATION /ctools/kbd FILE_PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
file (INSTALL .bashrc   DESTINATION ~           FILE_PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)

