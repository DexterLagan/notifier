# Notifier

  Notifier is a generic SQL match notification system written in Racket.

Usage:

1) Configure your MySQL server settings in notifier.conf;
2) Configure the item listing query in the [Notifier] section of the same file;
3) Add one section per receiver in the configuration file (see below for sample config);
4) Run Notifier and each keyword matched in the configuration file will result in an e-mail sent to 
the specified address with the matches.

Sample configuration file:

<pre>
[Notifier]
hostname=my-hostname
database=my-db
username=my-user
password=my-pass
query=SELECT product_refcom, product_qty, produit_descript_designation, product_price ...

[Dexter]
name=Dexter
email=dexterlagan@gmail.com
keywords=Lenovo X1 i7, Lenovo T420, Lenovo T420s, Dell M4800
greeting=Hi [name],;;  The following products have appeared in Nodixia's feed:;;
footer=;;Regards,;;Notifier

[Sadok]
name=Sadok
email=sadok@xxxxx.com
keywords=Lenovo W520, Lenovo W530, Lenovo W540, Lenovo x240, Lenovo T420, Dell M4800
greeting=Salut [name];;  J'ai trouvé les machines suivantes sur le stock:;;
footer=;;Bonne journée!,;;Notifier
</pre>

Regards,

Dexter Santucci - March 2019
