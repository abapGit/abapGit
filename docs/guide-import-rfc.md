---
title: Import ZIP
category: offline projects
order: 25
---

For offline repositories, you can import a package with all objects supported by abapGit via an RFC connection from another SAP system. This is a shortcut for doing
"Export ZIP" in one system, and "Import ZIP" in the other.

Pre-requisites:

- abapGit Developer Version is installed on remote SAP system
- Same package names on both systems
- Authorizations to run RFC function Z_ABAPGIT_SERIALIZE_PACKAGE and abapGit serialization process on remote system

Note:

The general recommendation is to use **online** repositories and a git server for comparing and synchronizing systems.
