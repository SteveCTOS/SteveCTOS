#project (ctools ascii)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/programs)
set (takeon ${CMAKE_CURRENT_BINARY_DIR}/programs/takeon.sh)
file(WRITE  ${takeon} "if [ \"$2\" = \"\" ]\nthen\n")
file(APPEND ${takeon} "  DATACO2=\"data01\"\n")
file(APPEND ${takeon} "else\n")
file(APPEND ${takeon} "  DATACO2=$2\n")
file(APPEND ${takeon} "fi\n")
file(APPEND ${takeon} "if [ \"$1\" = \"\" ]\nthen\n")
file(APPEND ${takeon} "  DATACO1=\"data01\"\n")
file(APPEND ${takeon} "else\n")
file(APPEND ${takeon} "  DATACO1=$1\n")
file(APPEND ${takeon} "fi\n")
file(APPEND ${takeon} "#-------------------\n")

set (bdb_list
  BmMaster 
  CbMaster CbMasterLy CbTrans CbTransLy 
  CoCashSales CoCompany CoDataName CoFaxParam CoMenu CoPrinters CoPrintersRemote3
  CoPrintersRemote7 CoPrintersRemote8 CoPullBy CoPullers CoStaffInOut 
  CrAlias CrCamsTrans CrCurrency CrFBCTrans CrJrn CrMaster CrMasterOld CrRemittance
  CrRemitTrans CrTrans 
  DrContact DrDeliver DrMaster DrMasterOld DrTrans
  GlJrn GlMaster GlMasterLy GlParameter GlTrans GlTransLy
  SlDailyEx  SlDistTot SlInvRev SlMaster SlParameter SlRegister SlRegLy SlSbRep SlSoldBy SlSpecials
  StAlternative StBranchCat StCatalogue StChanges StDiscAcc StImports StMaster StNewPrices
  StockLookup StOrderGen StOrders StReceipt StReceiptLy StSpecPr
  StTrans StTransLy
  )

foreach (name ${bdb_list})
  file(APPEND ${takeon} "export DD_${name}=${CMAKE_INSTALL_PREFIX}/$DATACO1/${name}\n")
endforeach()  
file(APPEND ${takeon} "#-------------------\n")

set (data_list
  BmMasterASCII 
  CbMasterASCII CbMasterLyASCII CbTransASCII CbTransLyASCII
  CoCashSales CoCompanyASCII CoDataNameASCII CoFaxParamASCII CoMenuASCII CoPrintersASCII CoPrintersRemote3ASCII
  CoPrintersRemote7ASCII CoPrintersRemote8ASCII CoPullByASCII CoPullersASCII CoStaffInOutASCII
  CrAliasASCII CrCamsTransASCII CrCurrencyASCII CrFBCTransASCII CrJrnASCII CrMasterASCII  CrMasterOldASCII CrRemittanceASCII
  CrRemitTransASCII CrTransASCII
  DrContactASCII DrDeliverASCII DrMasterASCII DrMasterOldASCII DrTransASCII
  GlJrnASCII GlMasterASCII GlMasterLyASCII GlParameterASCII GlTransASCII GlTransLyASCII
  SlDailyExASCII SlDistTotASCII SlInvRevASCII SlMasterASCII SlParameterASCII SlRegisterASCII SlRegLyASCII SlSbRepASCII SlSoldByASCII SlSpecialsASCII
  StAlternativeASCII StBranchCatASCII StCatalogueASCII StChangesASCII StDiscAccASCII StImportsASCII StMasterASCII StNewPricesASCII
  StockLookupASCII StOrderGenASCII StOrdersASCII StReceiptASCII StReceiptLyASCII StSpecPrASCII
  StTransASCII StTransLyASCII
  )

foreach (name ${data_list})
  string(TOUPPER ${name} UPPERNAME)
  file(APPEND ${takeon} "export DD_${name}=${CMAKE_CURRENT_SOURCE_DIR}/$DATACO2/${UPPERNAME}\n")
endforeach()  
file(APPEND ${takeon} "#-------------------\n")

function (cobol_executable name)
  set (source ${CMAKE_CURRENT_SOURCE_DIR}/programs/${name}.cob)
  set (output ${CMAKE_CURRENT_BINARY_DIR}/programs/${name})
  file(APPEND ${takeon} "COB_SYNC=n submitp ${CMAKE_CURRENT_BINARY_DIR}/programs/${name} I/0A\n")
  add_custom_command (
    OUTPUT ${output} 
    DEPENDS ${source}
    COMMAND cobc -g -I ${CMAKE_CURRENT_SOURCE_DIR} -I ${CMAKE_CURRENT_SOURCE_DIR}/copy -x ${source} -o ${output}
  )
  add_custom_target (${name}_project ALL
    DEPENDS ${output}
  )
endfunction()

set (source_list
  BMMASTXX 
  CBMALYXX CBMASTXX CBTRANXX CBTRLYXX 
  COCOMPXX CODATAXX COMENUXX COPRINXX COPUBYXX COPULLXX COSTFFXX 
  CRALIAXX CRCATRXX CRCURRXX CRJRNLXX CRMASTXX CRMAOLXX CRREMIXX CRRETRXX CRTRANXX
  DRCONTXX DRDELVXX DRMAOLXX DRMASTXX DRTRANXX 
  GLJRNLXX GLMALYXX GLMASTXX GLPARMXX GLTRANXX GLTRLYXX 
  SLDISTXX SLMASTXX SLPARMXX SLREGIXX SLRELYXX SLSBRPXX SLSOBYXX 
  STALTEXX STBRNCXX STCATAXX STDISCXX STIMPOXX STLOOKXX STMASTXX 
  STORDEXX STRECEXX STRELYXX STSPECXX STTRANXX STTRLYXX
)

#CRMAOLXX  Error: CHLFDCREDITOROLD, CHLFDCREDITOROLDASCII: No such file or directory
#CRRETRXX  In paragraph 'BE-010':
#          95: Error: syntax error, unexpected $undefined
#SLPARMXX  22: Error: CHLFDPARAM: No such file or directory
#          23: Error: CHLFDPARAMASCII: No such file or directory  
    
#STMASTXX  In paragraph 'BE-010'
#          89: Error: INVALID KEY clause invalid with this file type
#stockxx   27: Error: ChlfdStockASCII: No such file or directory

foreach(name ${source_list})
  cobol_executable(${name})
endforeach ()

file(APPEND ${takeon} "#-------------------\n")
file(APPEND ${takeon} "chmod g+rw /ctools/$DATAC01/*\n")

file (INSTALL ${takeon}  DESTINATION /ctools/bin  FILE_PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ)
