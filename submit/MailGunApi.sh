curl -s --user 'api:key-14a564ed4dc9242de3b4f09b9fd72587' \
https://api.mailgun.net/v3/pos.christensen.co.za/messages \
-F from='Linux Dev <sc@christensen.co.za>' \
-F to=Steve@Christensen.co.za \
-F subject='Mailgun test' \
-F template='mailgunctjtemplate2.html' \
-F attachment=@/ctools/bin/Invoice.pdf
