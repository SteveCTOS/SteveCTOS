#project (ctools ascii)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/programs)
set (takeon ${CMAKE_CURRENT_BINARY_DIR}/programs/takeon.sh)
file(WRITE ${takeon})

set (bdb_list
  BmMaster CbMaster CbMasterLy CbTrans CbTransLy CoCashSales CoCompany
  CoDataName CoDataNamex CoFaxParam CoMenu CoPrinters CoPrintersRemote3
  CoPrintersRemote7 CoPrintersRemote8 CoPullBy CoPullers CoStaffInOut CrAlias
  CrCAMSTrans CrCurrency CrFBCTrans CrJrn CrMaster CrMasterOld CrRemittance
  CrRemitTrans CrTrans DrContact DrDeliver DrMaster DrMasterOld DrTrans GlJrn
  GlMaster GlMasterLy GlParameter SlDailyEx  SlDistTot SlInvRev SlMaster
  SlParameter SlRegister SlRegLy SlSbRep SlSoldBy SlSpecials StAlternative
  StBranchCat StCatalogue StChanges StDiscAcc StImports StMaster StNewPrices
  StockLookup StOrderGen StOrders StReceipt StReceiptLy StReceiptsLy StSpecPr
  StTrans StTransLy
  )

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/data)
foreach (name ${bdb_list})
  file(APPEND ${takeon} "export DD_${name}=${CMAKE_CURRENT_BINARY_DIR}/data/${name}\n")
endforeach()  
file(APPEND ${takeon} "#-------------------\n")

set (data_list
  BmMasterASCII CbMasterASCII CbMasterLyASCII CbTransASCII CbTransLyASCII
  CoCompanyASCII CoDataNameASCII CoMenuASCII CoPrintersASCII CoPullByASCII
  CoPullersASCII CoStaffInOutASCII CrAliasASCII CrCamsTransASCII CrJrnASCII
  CrMasterASCII CrRemittanceASCII CrRemitTransASCII CrTransASCII
  DrContactASCII DrDeliverASCII DrMasterASCII DrMasterOldASCII DrTransASCII
  GlJrnASCII GlMasterASCII GlMasterLyASCII GlParameterASCII GlTransASCII
  GlTransLyASCII SlDistTotASCII SlMasterASCII SlRegisterASCII SlRegLyASCII
  SlSbRepASCII SlSoldByASCII StAlternativeASCII StBranchCatASCII
  StCatalogueASCII StDiscAccASCII StImportsASCII StMasterASCII
  StockLookupASCII StOrdersASCII StReceiptASCII StReceiptLyASCII StSpecPrASCII
  StTransASCII StTransLyASCII
  )

foreach (name ${data_list})
  file(APPEND ${takeon} "export DD_${name}=${CMAKE_CURRENT_SOURCE_DIR}/data/${name}\n")
endforeach()  
file(APPEND ${takeon} "#-------------------\n")

function (cobol_executable name)
  set (source ${CMAKE_CURRENT_SOURCE_DIR}/programs/${name}.cob)
  set (output ${CMAKE_CURRENT_BINARY_DIR}/programs/${name})
  file(APPEND ${takeon} "${CMAKE_CURRENT_BINARY_DIR}/programs/${name}\n")
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
  BMMASTXX CBMALYXX CBMASTXX CBTRANXX CBTRLYXX COCOMPXX CODATAXX COMENUXX 
  COPRINXX COPUBYXX COPULLXX COSTFFXX CRALIAXX CRCATRXX CRJRNLXX CRMASTXX
  CRREMIXX CRTRANXX DRCONTXX DRDELVXX DRMAOLXX DRMASTXX DRTRANXX GLJRNLXX
  GLMALYXX GLMASTXX GLPARMXX GLTRANXX GLTRLYXX SLDISTXX SLMASTXX SLREGIXX
  SLRELYXX SLSBRPXX SLSOBYXX STALTEXX STBRNCXX STCATAXX STDISCXX STIMPOXX
  STLOOKXX STORDEXX STRECEXX STRELYXX STSPECXX STTRANXX STTRLYXX
)

set (source_list_with_error
   
  #CRMAOLXX  Error: CHLFDCREDITOROLD, CHLFDCREDITOROLDASCII: No such file or directory
  #CRRETRXX  In paragraph 'BE-010':
  #          95: Error: syntax error, unexpected $undefined
  #SLPARMXX  22: Error: CHLFDPARAM: No such file or directory
  #          23: Error: CHLFDPARAMASCII: No such file or directory  
    
  #STMASTXX  In paragraph 'BE-010'
  #          89: Error: INVALID KEY clause invalid with this file type
  #stockxx   27: Error: ChlfdStockASCII: No such file or directory
)

foreach(name ${source_list})
  cobol_executable(${name})
endforeach ()
file(APPEND ${takeon} "#-------------------\n")
