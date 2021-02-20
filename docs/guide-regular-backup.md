---
title: Regular backup
category: other
order: 10
---

Using abapGit and abapGitServer for regular backup. Instead of abapGitServer, github or similar can also be used.

Steps:

1. Install abapGit

2. Use abapGit to install [abapGitServer](https://github.com/larshp/abapGitServer)

3. Create repository in abapGitServer via the web interface

4. Clone the repository using abapGit, into the package that should be backed up. abapGit will not delete any objects in the package

5. Enable write protection

6. Test backup by staging + commit + pushing from abapGit manually

7. Configure "background mode" for the repository, Advanced -> Background mode.
* Set to "Automatic push, auto author" this will find the user which last changed the objects and use these in the commits.
* ... or set to "Automatic push, fixed author" this will use the user of the background job
* Enter username and password(note: password will be stored in clear text). If abapGit and abapGitServer runs on the same ABAP server, then no password is required, as it will automatically use logon tickets.

8. On the background mode page: click "Run background logic" to test the setup works

9. Configure ZABAPGIT to run as a background job(SM36/SM37). Note: a dummy variant has to be created for the program, use SE38 to create the variant, values in the variant are not used for anything in background mode.
