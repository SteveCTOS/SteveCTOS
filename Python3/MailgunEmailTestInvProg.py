         MOVE CONCATENATE('python3 MailGunEmail.py ', 

          ' ', '-f', ' ', TRIM(18492061),
          ' ', '-F', ' ', TRIM(Steve@Christensen.co.za),
          ' ', '-r', ' ', TRIM(Steve@Christensen.co.za),
          ' ', '-s', ' ', TRIM(ctj invoicing department),
          ' ', '-t', ' ', 'MGMessageInvStatus.html',
          ' ', '-T', ' ', TRIM(Nordane1958@gmail.com))
                TO WS-COMMAND-LINE.
