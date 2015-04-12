-*- markdown -*-

xxx-xxx 0.4.0.0

* Compatible with GHC 7.10.

* New withAPISession and withSessionControl functions make talking to
  REST services more efficient.

* Added support for AWS S3 virtual-host style URLs.

* Added signing support for region specific calls to the AWS Security
  Token Service (AWS STS).

* The introduction of AWS support accidentally introduced unwanted AWS
  headers and computation into all requests. This has been fixed.


2014-12-11 0.3.0.1

* Bump lower bound on http-client to 0.3.0.1


2014-12-02 0.3.0.0

* Support for Amazon Web Services request signing

* New customMethod, customMethodWith functions allow use of arbitrary
  HTTP verbs

* httpProxy, basicAuth, oauth2Bearer, oauth2Token: removed Maybe from
  result types, changed documentation to suggest use of (?~)


2014-08-25 0.2.0.0

* Support for lens 4.4


2014-04-22 0.1.0.0

* Initial release.
