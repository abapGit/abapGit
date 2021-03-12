---
title: Contributing
order: 5
---

General contribution guidelines can be found [here](https://github.com/abapGit/abapGit/blob/main/CONTRIBUTING.md).

How to submit a pull request:

*******************************
Having already [installed](https://docs.abapgit.org/guide-install.html#install-standalone-version) the abapGit standalone program,

1. [Install the development version](https://docs.abapgit.org/guide-install.html#install-developer-version) of abapGit, by running `ZABAPGIT_STANDALONE` and either:
   1. cloning your *own [fork](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/about-collaborative-development-models)* of the [abapGit repository](https://github.com/abapGit/abapGit) using abapGit's "New Online" option or
   2. [downloading the zip file and installing it](https://docs.abapgit.org/guide-install.html#install-developer-version) using the "New Offline" option
2. When specifying a package for the repository in step 1, assign a different (new, e.g. `$ZABAPGIT`) package to the one `ZABAPGIT_STANDALONE` is in

3. Your system will now contain the abapGit package, along with several subpackages and development objects such as classes, interfaces, function groups, etc. There will also be the development version of the abapGit program (transaction `ZABAPGIT`). The standalone program, `ZABAPGIT_STANDALONE` will remain unaffected, to serve as [fallback](https://docs.abapgit.org/guide-upgrade.html#troubleshooting).

4. Make your changes to abapGit development objects and test locally.

5. Using abapGit on your system, commit the changes to your online fork (so if you had chosen the "offline" option in step 1, you will need to have switched to online).

6. Submit a [pull request](https://docs.github.com/en/free-pro-team@latest/articles/creating-a-pull-request-from-a-fork) with the changes.

Alternatively to the steps above, use the GitHub webinterface to change the files directly on a GitHub branch of your forked abapGit repository and then submit a pull request. This might make sense when, e.g., contributing to the documentation files such as the one you are reading right now (where contributions may be initiated through the "Improve this page" link above).

If you already have an old fork and would like to create a new pull request, you can sync your old fork to current state following this guide: [Sync your fork to original repository](https://github.com/KirstieJane/STEMMRoleModels/wiki/Syncing-your-fork-to-the-original-repository-via-the-browser).

After your pull request is merged to the abapGit default branch, abapmerge will automatically run to build the standalone report version of abapGit. Every time a commit is pushed to the default branch, Github Actions is triggered to perform this build. It uses [abapmerge](https://github.com/larshp/abapmerge) to merge all the includes into a single file (the standalone version) which can be downloaded from [https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap](https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap)