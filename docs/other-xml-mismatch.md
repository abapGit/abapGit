---
title: XML Mismatch
category: other
order: 70
---

### For end-users
Make sure you have the latest version of abapGit installed, see [upgrading](guide-upgrade.html), after updating try again.

If the error persists, submit an issue to the maintainer of the repository requesting to update to latest version of abapGit serialized files pointing to the guide below.

Or alternatively install an old version of abapGit to allow for installing the files, see next section "For maintainers"

### For maintainers
If the latest version of abapGit is installed and the erorr message is shown, then the serialized files should be updated to the latest version.

The steps are:

1. Install the old abapGit version, matching the serialized files

2. Install the objects into the ABAP system

3. Install the newest abapGit version

4. Use the new abapGit to save the files to the git repository

This will overwrite the files in the repository to have the latest format, compatible with the newest abapGit release.

| abapGit       | Download | XML Serialization |
| :------------- |:------------- |:-------------|
| v1.0.0 to current | [Link](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap) | v1.0.0 |
| v0.0.0 to v0.113.0 | [Link](https://raw.githubusercontent.com/abapGit/abapGit/v0.113.0/zabapgit.prog.abap) | v0.2-alpha |
