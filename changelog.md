-*- markdown -*-

2018-11-16 0.5.3.0

* Added Postable/Putable on aeson encoding

* Added better AWS signing for urls without region

2018-03-01 0.5.2.1

* Fixed some building issues with older versions

* Removed dependency on cryptohash, using cryptonite instead

2018-01-01 0.5.2.0

* Added some HistoriedResponse support

* Deprecated withSession, added newSession (to be inline with upstream http-client)

* Added same instances for Putable as for Postable (might be merged?)

* Added getSessionCookieJar to get cookies from a Session

* Fixed customPayloadMethod to follow the method (it was sometimes POST)

2017-12-23 0.5.1.1

* Add awsSessionTokenAuth (in addition to the existing awsAuth) to
  support AWS Session Token Service (AWS STS) credentials. These look
  like regular AWS credentials but have an additional session token as
  a 3rd element. This mechanism is needed to be able to (a) use EC2
  instance profiles, (b) make calls form AWS Lambda, (c) is useful for
  delegated role access (assumeRole within and across accounts), and (d)
  enables MFA-protected access scenarios.
  See tests/AWS/IAM.hs for a test and simple example.

2017-01-09 0.5.1.0

* Add Session-specific version of Network.Wreq.customPayloadMethodWith

* 8.2 GHC compatibility

2017-01-09 0.5.0.0

* Compatible with `http-client` >= 0.5

* This compatibility change required a small API change: `checkStatus`
  is now named `checkResponse` for compatibility with the
  `http-client` package

2015-05-10 0.4.0.0

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
