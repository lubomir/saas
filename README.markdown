# [ShellCheck as a Service](http://saas-lsedlar.rhcloud.com/)

> You have surely seen it as recommended way to install a project. Just curl a
> script and pipe it to bash. It's so simple.

But what happens if the script does something malicious? Easy answer: you're
screwed.

But don't panic. We've got you covered. This is a service that will check the
script with [ShellCheck] and give you information on what problems are there
with the script.


## Disclaimer

This is a joke. Don't use this service for anything security related. Heck,
don't run random script from the net without making sure they are what they
claim to be.

Checking a local file might be useful for situations where you can't install
[ShellCheck] directly. If you can, do it! It's most likely packaged by your
distribution already.

[shellcheck]: http://www.shellcheck.net/
