# ACME (Let's Encrypt protocol) component for Delphi Tokyo, Rio and Sydney
(c)2018-2021 by [Execute SARL](http://www.execute.fr)

## Purpose

TExecuteACME component let you request a "[Let's Encrypt](https://letsencrypt.org/)" certificate for you domain.

The component supports HTTP Challenge, you can use a self hosted WebServer (TidHTTPServer) to validate the certificate or use the OnHttpChallenge event to store the challenge reply on your website.

In both case you need to manage the domain's HTTP (not HTTPS) server.

## Installation

this component is compatible with Delphi Berlin 10.1, Tokyo 10.2.3, Rio 10.3 and Sydney 10.4.2 (even the Community Edition) .

**New**: this repository contains Sydney compatible DCU (Win32, Win64 and Linux) for testing purpose only, they accepts only Staging environment.

1. Download the repository
2. Open ACMEGroup.groupproj
3. Right click on the project "Execute.ACMEDesign.bpl" and install it
4. Select the ACMEDemo project
5. Compile and run the Application

the application requires OpenSSL (for Windows : libeay32.dll and ssleay32.dll)
on the first start, the application can create Account.key and Domain.key for you, (generating a 4096bits take some time, this is normal).

Account.key will be your Let's Encrypt account private key, place it on a safe place (with a password).

Domain.key is the private key for the domain (same recommandations).

if you have an error like "SSL routunes:ssl3_read_bytes:tslv1 alert protocol version", you should update the component to version 1.4.
The component used Indy socket, but TLS 1.3 is not supported, so in version 1.4 I've swtiched to System.Net.HTTPClient
	
## How does it work ?

(you have to do this periodically  because the certificate lives only for 90 days !)

### 1. Registration request
	TExecuteACME.RegisterDomain();
	 -> Account.key     -> register a Let's Encrypt account.
	 -> Domain.key      -> send a Certification Signing Request.
	 -> OnHttpChallenge -> store the challenge Token & Thumbprint.
	 -> OnDone          -> the certificate is not ready yet, try later.
### 2. HTTP Challenge
	Let's Encrypt -> http://(domain)/.well-known/acme-challenge/(token) <- Thumbprint
### 3. Retrieve the certificate
	TExecuteACME.FinalizeDomain();
	 -> Account.key     -> Retrieve account status.
	 -> Domain.key      -> Retrieve the Certificate from Let's Encrypt.
	 -> OnCertificate   -> store the updated certificate.
### 4. Revoke certificate (if required)
	 TExecuteACME.UnRegisterDomain();
	 -> Domain.key      -> Revoke Certificat
	 -> OnDone          -> the certificate is revoked.
	 
## version 1.1

Better error handling with error report

SubjectAltNames let you define alternative names for the DomainName

new OrderURL and OrderStatus properties

FinalizeDomain is now used (with OrderURL) to retrieve the last requested Certificat

OnHttpChallenge has a new parameter Processed to let you delay the challenge (call FinalizeDomain when the challenge is ready, the event will be fired again and then you can set Processed to True)

## version 1.2

Support for [POST-as-GET](https://community.letsencrypt.org/t/acme-breaking-change-most-gets-become-posts/71025) protocol change.

## version 1.3

Support for 64bits and Linux

Suport for synchronous  calls with *Now methods

## version 1.4

Swith from TidHTTP to System.Net.HTTPClient for TLS 1.3 support. 

## Licence

this repository contains the full source code of the demo application for the CLOSED SOURCE component TExecuteACME.

you'll find the compiled Execute.ACME.dcu unit in the [lib](/lib) folder and the Interface part of the Unit in [Execute.ACME.Interface.pas](lib/Execute.ACME.Interface.pas).

![screen](ACME-Component.png)

This component is NOT FREE !

You have to [register a licence](https://store.execute.fr) to use it in any commercial product

you are NOT allowed to use this component to register a commercial website
certificate without a registered licence.

You can [buy the component source code](https://store.execute.fr) for 50&euro; per developper up to 200&euro; (site licence).

Minor version updates indefinitely and major version updates for 1 year from date of purchase.

If I sell 100 licences (5.000&euro;), the component will be released under GPL :)

Even then you'll still be able to buy a commercial licence for my part of the component.

Feedbacks are welcome.