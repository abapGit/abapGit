---
title: Contributing
order: 5
---

General contribution guidelines can be found [here](https://github.com/abapGit/abapGit/blob/master/CONTRIBUTING.md).

How to submit a pull request:

*******************************
Having already [installed](https://docs.abapgit.org/guide-install.html#install-standalone-version) the [abapGit standalone](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap) program,

1: [Install the development version](https://docs.abapgit.org/guide-install.html#install-developer-version) of abapGit by running the standalone program and either cloning the [abapGit repository](https://github.com/abapGit/abapGit) using the "New Online" option, or [downloading the zip file and installing it](https://docs.abapgit.org/guide-install.html#install-developer-version) using the "New Offline" option. In your SAP system, assign a different (new, local, e.g. "$ZABAPGIT") package to the one abapGit Standalone is in.

2: Your system will now contain the abapGit package you created in step 1, along with several subpackages and development objects such as classes, interfaces, function groups, etc. There will also be the development version of the abapGit program, run with transaction "ZABAPGIT". The standalone version, "ZABAPGIT_STANDALONE" will remain unaffected, to serve as [fallback](https://docs.abapgit.org/guide-upgrade.html#troubleshooting).

3: Make your changes to abapGit development objects and test locally.

4: Using abapGit on your system, create a pull request with the changes.

Alternatively to the steps above, use the GitHub webinterface to change the files directly on a GitHub branch of your forked abapGit repository and then submit a pull request. This might make sense when, e.g., contributing to the documentation files such as the one you are reading right now (through the "Improve this page" link above).

If you already have an old fork and would like to create a new pull request, you can sync your old fork to current state following this guide: [Sync your fork to original repository](https://github.com/KirstieJane/STEMMRoleModels/wiki/Syncing-your-fork-to-the-original-repository-via-the-browser).

After your pull request is merged to the abapGit master branch, abapmerge will automatically run to build the standalone report version of abapGit. Every time a commit is pushed to the master branch, Github Actions is be triggered to perform this build. It uses [abapmerge](https://github.com/larshp/abapmerge) to merge all the includes into a single file, the standalone version already mentioned, which can be downloaded from [https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap)
