---
title: Contributing
order: 5
---

General contribution guidelines can be found [here](https://github.com/abapGit/abapGit/blob/master/CONTRIBUTING.md).

How to submit a pull request:

*******************************

1: Install the development version of abapGit by cloning the repository using an online repository, or downloading the zip file and installing it using an offline repository.

2: The abapGit report installed in your system will now consist of multiple classes

3: Do the required changes to classes and/or main program

4: Create the pull request with the changes

5: After the pull request is merged, abapmerge will automatically run to build the report version of abapGit.

Every time a commit is pushed to the master branch, Github Actions will be triggered to perform the build. It will use [abapmerge](https://github.com/larshp/abapmerge) to merge all the includes into a single file, the build can be downloaded from [https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap)

Alternatively, use the GitHub webinterface to change the files and submit a pull request.


If you already have an old fork and would like to create a new pull request,
you can sync your old fork to current state following this guide:
[Sync your fork to original repository](https://github.com/KirstieJane/STEMMRoleModels/wiki/Syncing-your-fork-to-the-original-repository-via-the-browser)
