---
title: Packages & Transports
category: reference
order: 75
---

## Packages

abapGit repositories do *not* store any SAP package names. The top (or root) SAP package for a repository is defined when creating a repository in your system 
("[New Online](guide-online-install.html)" or "[New Offline](guide-offline-install.html)"). You have two options to provide the SAP package:

1. Let abapGit create the package automatically
2. Create the package manually 

If the repository contains sub packages, abapGit will create them automatically with the same settings as the top SAP package (see also [Folder Logic](ref-dot-abapgit.html#folder-logic)).

### Automatic Creation of Package

Just enter the name of the SAP package in the "New Online" or "New Offline" dialogs. The package does not have to exist. Then select "Clone Online Repo" or "Create Offline Repo".

### Manually Creation of Package

You can use an existing SAP package, create the package beforehand in transaction SE80, or create the package in the "New Online" or "New Offline" dialogs.

## Transports

Based on the type of SAP package you use, abapGit will determine what type of transport will be required. When pulling or uninstalling the repo, abapGit will then prompt
for a corresponding transport request.

The following table lays out if abapGit can automatically create the package and how it determines the type of transport.

Change & Transport System<br>Configuration | Local $-Package<br>$PACK | Local Z-Package<br>ZPACK | Transportable Z-Package<br>ZPACK | Namespace-Package<br>/NAMESPC/PACK
--------------------------|-----------------|-----------------|-------------------------|-------------------
Not configured            | <span style="color:green">(1)</span> | <span style="color:green">(2)</span> | n/a | n/a
One transport layer       | <span style="color:green">(1)</span>  | <span style="color:blue">(3)</span> | <span style="color:green">(4)</span> | <span style="color:green">(4)</span>     
Multiple transport layers | <span style="color:green">(1)</span>  | <span style="color:blue">(3)</span> | (5) | (5)

<span style="color:green">(1) Automatically create package, no transport</span><br> 
<span style="color:green">(2) Automatically create package, local workbench request</span><br>
<span style="color:blue">(3) Manually create package (without transport layer), local workbench request</span><br>
<span style="color:green">(4) Automatically create package, transportable workbench request</span><br>
<span style="color:green">(5) Automatically create package for default transport layer, transportable workbench request.</span> <span style="color:blue">Manually create package, if you want to use a different layer than the default.</span>
