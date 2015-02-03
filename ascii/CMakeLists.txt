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

foreach (name ${bdb_list})
  set (ENV{DD_${name}} ${CMAKE_CURRENT_BINARY_DIR}/data/${name})
endforeach()  

set (ascii_list
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

foreach (name ${ascii_list})
  set (ENV{DD_${name}} ${CMAKE_CURRENT_SOURCE_DIR}/data/${name})
endforeach()  

function (cobol_executable name)
  set (source ${CMAKE_CURRENT_SOURCE_DIR}/programs/${name}.SRC)
  set (output ${CMAKE_CURRENT_BINARY_DIR}/programs/${name})
  add_custom_command (
    OUTPUT ${output} 
    DEPENDS ${source}
    COMMAND cobc -g -I ${CMAKE_CURRENT_SOURCE_DIR} -x ${source} -o ${output}
    COMMAND ${SUBMIT} ${output} ${SUBMIT_CODES}
  )
  add_custom_target (${name}_project ALL
    DEPENDS ${output}
  )
endfunction()

set (source_list
  BMMASTXX CBMALYXX CBMASTXX CBTRANXX CBTRLYXX COCOMPXX CODATAXX 
  COMENUXX COPUBYXX COPULLXX COSTFFXX CRALIAXX CRCATRXX CRJRNLXX CRMASTXX
  CRREMIXX CRTRANXX DRCONTXX DRDELVXX DRMAOLXX DRMASTXX DRTRANXX GLJRNLXX
  GLMALYXX GLMASTXX GLPARMXX GLTRANXX GLTRLYXX SLDISTXX SLMASTXX SLREGIXX
  SLRELYXX SLSBRPXX SLSOBYXX STALTEXX STBRNCXX STCATAXX STDISCXX STIMPOXX
  STORDEXX STRECEXX STRELYXX STSPECXX STTRANXX STTRLYXX
)

set (source_list_with_error
  COPRINXX  CRMAOLXX  CRRETRXX SLPARMXX  STLOOKXX  STMASTXX STOCKXX
)

foreach(name ${source_list})
  cobol_executable(${name})
endforeach ()
