---
title: Compare RFC
category: offline projects
order: 25
---

For write-protected offline repositories, you can compare a package with all objects supported by abapGit via an RFC connection to another SAP system. 

Pre-requisites:

- abapGit Developer Version is installed on remote SAP system
- Offline repository is write-protected 
- Same package names on both systems
- Authorizations to run RFC function Z_ABAPGIT_SERIALIZE_PACKAGE and abapGit serialization process on remote system

Note:

The general recommendation is to use **online** repositories and a Git server for comparing and synchronizing systems. We do not recommend to create RFC destinations between different tier systems or creating RFC users with wide authorizations. 
