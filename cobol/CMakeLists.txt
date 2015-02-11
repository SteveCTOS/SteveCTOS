project (ctools_cobol)

set (COBOL_SOURCE ${CMAKE_SOURCE_DIR}/cobol)
set (COBOL_BINARY ${CMAKE_BINARY_DIR}/cobol)

function (cobol_executable name)
  set (source ${COBOL_SOURCE}/${name}.cob)
  set (output ${COBOL_BINARY}/${name})
  add_custom_command (
    OUTPUT ${output}
    DEPENDS ${source} ${COBOL_BINARY}/cobcalls.o
    COMMAND cobc -g -I ${COBOL_BINARY} -I ${COBOL_SOURCE} -I ${COBOL_SOURCE}/copy 
                 -I ${COBOL_SOURCE}/fd -x ${source} -l rt ${COBOL_BINARY}/cobcalls.o 
                 -L ${CMAKE_BINARY_DIR}/lib -l b20forms
  )
  add_custom_target (${name} ALL
    DEPENDS ${output} b20forms
    SOURCES ${source}
  )
  install (PROGRAMS ${output} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin COMPONENT cobol_executable)
endfunction()

function (cobol_module name)
  set (source ${COBOL_SOURCE}/${name}.cob)
  set (output ${COBOL_BINARY}/${name}.so)
  add_custom_command (
    OUTPUT ${output}
    DEPENDS ${source} ${COBOL_BINARY}/cobcalls.o
    COMMAND cobc -g -I ${COBOL_BINARY} -I ${COBOL_SOURCE} -I ${COBOL_SOURCE}/copy 
                 -I ${COBOL_SOURCE}/fd -m ${source} -l rt ${COBOL_BINARY}/cobcalls.o 
                 -L ${CMAKE_BINARY_DIR}/lib -l b20forms
  )
  add_custom_target (${name} ALL
    DEPENDS ${output} b20forms
    SOURCES ${source}
  )
  install (FILES ${output} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin COMPONENT cobol_module)
endfunction()

set (executable_list 
  MainCont TestMenu TestStuff 
)

foreach (name ${executable_list})
  cobol_executable(${name})
endforeach()  

set (module_list 

  CoCsParm  CoFxParm  Company  ContMenu   CoPrinter  CoPrntMt  CoPuByMt CoQuParm
  CoRpParm  CoStffIq  CoStffMt CrPrinter  DataName   DrPrinter MenuPass
  PassWrds

  Dr1Menu   Dr2Menu Dr3Menu Dr4Menu

  St1Menu   St2Menu St3Menu St4Menu

  Sl1Menu   Sl2Menu Sl3Menu Sl4Menu Sl5Menu Sl6Menu

  Cr1Menu   Cr2Menu Cr3Menu Cr4Menu

  Gl1Menu   Gl2Menu Gl3Menu Gl4Menu

  BaUpMenu  Bm1Menu Cb1Menu MastMenu

  BmAnalCh  BmKitCop  BmKitsIq  BmKitsMt  BmKitsRp  BmKtAnRp BmKtChMt BmKtItIq
  BmKtLiRp  BmKtQyMt  BmNewItm

  #/ctools/dev/source/cobol/copy/SelectStOrders:5: Error: 'WS-OUTORD-STATUS' undefined
  # BmKitMfg BmKtMfIq BmKtMfLy 
  
  #/ctools/dev/source/cobol/copy/SelectSlRegister:6: Error: 'WS-INCR-STATUS' undefined
  # BmKtRgRp 

  CbBankTr CbCAMSAl  CbCAMSMt  CbGlPost  CbMaLyIq  CbMastIq  CbMastMt
  CbPerEnd CbStAlMt  CbStatRp  CbTranIq  CbTrans   CbTrFuIq  CbTrLyIq  CbTrOsRp

  CrAdjuIq  CrAgeARp  CrAgeTmp  CrAgeTRp  CrAliaRp  CrAlias   CrCAMSCt  CrCAMSDl
  CrCAMSIq  CrCAMSMt  CrCheqMt  CrCheqRp  CrCurrMt  CrFBCMt   CrFBCPRp  CrFBCRp
  CrFBCTrs  CrInCrMt  CrInCrRp  CrJnAnRp  CrJrnMt   CrLablRp  CrLaPuRp  CrMasOld
  CrMaster  CrMastIq  CrMastMt  CrMastRp  CrNameIq  CrNamOld  CrNmFxRp
  CrNoChMt  CrPayDMt  CrPaymnt  CrPaymRp  CrPerEnd  CrReFrAl  CrRemiEM CrRemiIq
  CrRemiMt  CrRemiRp  CrReTrMt  CrTrans

  #/ctools/dev/source/cobol/CrJrnIq.cob: In paragraph 'RDPREV-005':
  #/ctools/dev/source/cobol/CrJrnIq.cob:510: Error: syntax error, unexpected PREVIOUS
  # CrJrnIq

  #/ctools/dev/source/cobol/CrTranIq.cob: In paragraph 'RDPREV-005':
  #/ctools/dev/source/cobol/CrTranIq.cob:484: Error: syntax error, unexpected PREVIOUS
  # CrTranIq 

  DrAcStIq  DrAcStRp  DrAdjuIq  DrAdSlAc  DrAgeARp  DrAgeTmp  DrAgeTRp
  DrAlDiRp  DrAnChck  DrArJhRp  DrArSaRp  DrArToRp  DrCorpRp  DrCredRp
  DrCrLiMt  DrDateMt  DrDelvMt  DrDiAcRp  DrDiscRp  DrDiStRp  DrDuFxRp
  DrInteMt  DrLablRp  DrLaSoRp  DrMailRp  DrMasOld  DrMasSMt  DrMaster
  DrMastIq  DrMastMt  DrMastRp  DrNameIq  DrNamOld  DrNmFxRp  DrNoChMt
  DrNumber  DrPaymnt  DrPrAcRp  DrRepRst  DrSaCoMt  DrSaCoRp  DrSaMxRp 
  DrStatRp  DrTrans

  GlAsInRp  GlAsRgRp  GlBalSRp  GlBlGpRp  GlBudgMt  GlInGpRp  GlInStRp
  GlJrnARp  GlJrnBat  GlJrnlMt  GlJrnLRp  GlMaLyIq  GlMaster  GlMastIq
  GlMastLy  GlMastMt  GlMastRp  GlNameIq  GlNameLy  GlNoChMt  GlParmMt
  GlPerEnd  GlRandom  GlTbLyRp  GlTranIq  GlTranLy  GlTrans   GlTrBaRp  
  GlTrbSRp  GlTrFuIq  GlTrLyIq  GlTrSrch

  SlAnCoRp SlAreaMt SlBrOrdr SlBrchMt SlCashRp SlChckRp SlCredit SlCrdRev
  SlDelvMt SlDelvRp SlDistMt SlDistIq SlDistRp SlDyExRp SlGstxRp SlGstxIq
  SlInAcIq SlInAcLy SlInCoMt SlInCrLy SlInCrRp SlInDlLy SlInDlMt SlInOrIq
  SlInOrLy SlInRgRp SlInStIq SlInStLy SlIRegLy SlInvoic SlInvRev SlIRegIq
  SlIRegMt SlMastMt SlMastIq SlMastRp SlOrChMt SlOrChRp SlOrFiRp SlOrRgRp
  SlOrSuRp SlOrders SlPackRp SlParmMt SlPerEnd SlPOrdRp SlPoShLy SlPoShIq
  SlPSlpIq SlPuByMt SlPuByRp SlQuAcIq SlQuRgRp SlQuote  SlQuStIq
  SlRgAcRp SlRgName SlRgNaIq SlRgNaMy SlRgNaLy SlRgLyMt SlRgChLy SlRgAcLy
  SlRgChMt SlSBChMt SlSaleRp SlSManRp SlSoByMt SlSoByRp SlSpecRp SlSRepMt
  SlTermMt

  #/ctools/dev/source/cobol/SlPsPuMt.cob:12: Error: Redefinition of 'PULLER-MASTER'
  #/ctools/dev/source/cobol/copy/SelectCoPullers:1: Error: 'PULLER-MASTER' previously defined here 
  # SlPsPuMt 

  StAlteIq  StAlteMt  StAlteRp  StAnalRp  StAnOhRp  StAveLst  StBinNRp  StBoAcIq
  StBoAcRp  StBoLeRp  StBoReMt  StBoStIq  StBoStRp  StBranch  StBrchIq  StBrCtMt
  StByGdRp  StCataMt  StCataRp  StCatPIq  StChngMt  StCounRp  StDescIq
  StDiAcBt  StDiAcIq  StDiAcMt  StHiSlRp  StImport  StInStRp  StLablRp  StLookMt
  StLookRp  StMasBMt  StMasFMt  StMasLMt  StMasOMt  StMasPMt  StMaster
  StMastIq  StMastMt  StMastRp  StMiMxMt  StMiMxRp  StMvPeRp  StNoChMt
  StNoCopy  StNumber  StOrdrMt  StOrDtMt  StOrGnMt  StOrGnRp  StOrOrIq  StOrOrRp
  StOrPrRp  StOrPsMt  StOrStIq  StOrStRp  StOrSuIq  StOrSuRp  StPrChMt
  StPrCoRp  StPrFoRp  StPricMt  StPrLoRp  StPrNwMt  StPrSpMt  StPrSpRp  StQyAdRp
  StQyChRp  StReDtRp  StReduRp  StReImMt  StReImRp  StReLoIq  StReLoMt
  StReLoRp  StRepair  StRpAcIq  StRpRgRp  StRpStIq  StSGmRp   StSplit   StStQyMt
  StTariRp  StTranLy  StTrans   StTrChMt  StTrDuMt  StTrDuRp  StValuRp

)

foreach (name ${module_list})
  cobol_module(${name})
endforeach()  
