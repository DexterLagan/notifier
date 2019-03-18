# Notifier

Notifier is a generic SQL match notification system.
1) Configure your server in the notifier.conf;
2) Add one section per user in the configuration file;
3) Configure the SQL query to scan the target database table;
4) Run Notifier and each keyword matched in the configuration file will result in an e-mail sent to the specified address with the matches.

Sample configuration:

[romain]
name=Romain
email=romain@gmail.com
keywords=Lenovo X260, Lenovo X270, Lenovo X1

[dexter]
name=
; email=dexterlagan@gmail.com
; keywords=Lenovo X1 i7, Lenovo T420 i7, Lenovo T420s
