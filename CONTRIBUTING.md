## Development Guidelines

a. Make sure there is an open issue for the stuff that you want to work on. When starting the development add a comment in the issue. This will allow for discussing possible solutions, and avoid having multiple people look into the same issue.

b. Keep the commits/PR as small as possible, try not to fix multiple issues in one commit/PR if possible. Smaller changes are easier to review, and are more likely to be accepted.

c. Variables are prefixed using the standard setting in Code Inspector check "Extended Naming Conventions for Programs"

d. abapGit is targeted for version 702, so the code should only contain expressions/statements that works on 702

e. commit often, whenever something is working, and is a step in the right direction do a commit or PR. This way other contributors can see the changes, and it will minimize the risk of merge conflicts.

External Links:
https://guides.github.com/activities/contributing-to-open-source/

## Building/Creating a PR

1: Install the development version of abapGit by cloning the repository using an online repository, or downloading the zip file and installing it using an offline repository.

2: The abapGit report installed in your system will now consist of multiple includes

3: Do the required changes to includes and/or main program

4: Create the pull request with the changes to the includes

5: After the pull request is merged, abapmerge will automatically run to build the non-include version of abapGit.

Every time a commit is pushed to the master branch, https://travis-ci.org/ will be triggered to perform the build. It will use [abapmerge](https://github.com/larshp/abapmerge) to merge all the includes into a single file, the build can be downloaded from http://larshp.github.io/abapGit/build/zabapgit.txt

Alternatively, use the GitHub webinterface to change the files and submit a pull request.
