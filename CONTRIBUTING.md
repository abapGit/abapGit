## Bug Reports

A bug is a _demonstrable problem_ that is caused by the code in the repository. Good bug reports are extremely helpful - thank you!

Guidelines for bug reports:

1. **Use the GitHub issue search** &mdash; check if the issue has already been
   reported.

2. **Check if the issue has been fixed** &mdash; try to reproduce it using the
   latest `master` or development branch in the repository.

3. **Demonstrate the problem** &mdash; provide clear steps that can be reproduced.

A good bug report should not leave others needing to chase you up for more information. Please try to be as detailed as possible in your report. What is your environment? What steps will reproduce the issue? What would you expect to be the outcome? All these details will help to fix any potential bugs.

## Development Guidelines

a. Make sure there is an open issue for the stuff that you want to work on. When starting the development add a comment in the issue. This will allow for discussing possible solutions, and avoid having multiple people look into the same issue.

b. Keep the commits/PR as small as possible, try not to fix multiple issues in one commit/PR if possible. Smaller changes are easier to review, and are more likely to be accepted.

c. Variables are prefixed using the standard setting in Code Inspector check "Extended Naming Conventions for Programs"

d. abapGit is targeted for version 702, so the code should only contain expressions/statements that works on 702

e. commit often, whenever something is working, and is a step in the right direction do a commit or PR. This way other contributors can see the changes, and it will minimize the risk of merge conflicts.

f. Use pretty printer, keywords upper case + indentation

External Links:
https://guides.github.com/activities/contributing-to-open-source/

## Building/Creating a PR

1: Install the development version of abapGit by cloning the repository using an online repository, or downloading the zip file and installing it using an offline repository.

2: The abapGit report installed in your system will now consist of multiple includes

3: Do the required changes to includes and/or main program

4: Create the pull request with the changes to the includes

5: After the pull request is merged, abapmerge will automatically run to build the non-include version of abapGit.

Every time a commit is pushed to the master branch, https://travis-ci.org/ will be triggered to perform the build. It will use [abapmerge](https://github.com/larshp/abapmerge) to merge all the includes into a single file, the build can be downloaded from https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap

Alternatively, use the GitHub webinterface to change the files and submit a pull request.
