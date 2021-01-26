---
title: Packages & Transports
category: reference
order: 75
---

abapGit repositories do *not* store any SAP package names. The top (or root) SAP package for a repository is defined when creating a repository in your system 
("[New Online](guide-online-install.html)" or "[New Offline](guide-offline-install.html)"). You have two options to provide the SAP package:

1. Let abapGit create the package automatically
2. Create the package manually 

## Automatic Creation of Package

Just enter the name of the SAP package in the "New Online" or "New Offline" dialogs. The package does not have to exist. Then select "Clone Repo" or "Create Rep".

## Manually Creation of Package

You can use an existing SAP package, create the package beforehand in transaction SE80, or create the package in the "New Online" or "New Offline" dialogs.

## Transports

Based on the type of SAP package you use, abapGit will determine what type of transport will be required. When pulling or uninstalling the repo, abapGit will then prompt
for a corresponding transport request.

The following table lays out if abapGit can automatically create the package and how it determines the type of transport.

Change & Transport System | Local $-Package | Local Z-Package | Transportable Z-Package | Namespace-Package
Configuration             | $PACK           | ZPACK           | ZPACK                   | /NAMESPC/PACK
------------------------------------------------------------------------------------------------------------
Not configured            | (1)             | (2)             | n/a                     | n/a
One transport layer       | (1)             | (3)             | (4)                     | (4)     
Multiple transport layers | (1)             | (3)             | (5)                     | (5)

(1) Automatically create package, no transport 
(2) Automatically create package, local workbench request
(3) Manually create package, local workbench request
(4) Automatically create package, transportable workbench request
(5) Automatically create package for default transport layer, transportable workbench request. Manually create package, if you want to use a different layer
