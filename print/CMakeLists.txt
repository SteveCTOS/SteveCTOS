project (ctools_print)

set (copy_list
  fohtotext.py
  PrintCredit
  PrintInvoice
  PrintStatement
  PrintStatementNoMail
  PrintQuote1
  PrintQuote2
  PrintMergeQuote
  PrintDrOverdue1
  PrintDrOverdue2
  PrintMergeDrOverdue
#  creditnote01
#  invoice01
#  statement01
)

foreach (name ${copy_list})
  file (COPY ${name} DESTINATION /ctools/bin FILE_PERMISSIONS OWNER_WRITE OWNER_READ OWNER_EXECUTE GROUP_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ)
endforeach ()
