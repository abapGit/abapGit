---
title: Upgrading
category: getting-started
order: 20
---


## Standalone Version

To update to the current version, upload the code from [zabapgit.abap](https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap) into the ZABAPGIT_STANDALONE report (formerly ZABAPGIT_FULL) and activate the program.

## Developer Version

If you have installed the abapGit developer version (in a separate package like $ABAPGIT), the you can update the code automatically.

### Online project

If your system is connected to the Internet, create an online repository for your abapGit package. When you view the repository and update are available, a "Pull" link will appear in the menu. When you select "Pull", the system will download and install the latest version of the code.

### Offline project

If your system is not connected to the Internet, create an offline repository. To update abapGit download the ZIP file from [https://github.com/abapGit/abapGit/archive/main.zip](https://github.com/abapGit/abapGit/archive/main.zip), and select "Import ZIP" in your abapGit repository.

### Troubleshooting

Updates to abapGit can on occasion be quite significant and cause issues like inactive objects, diffs, or even dumps. If you run into such issues, please proceed as follow:
1. Install and activate the latest standalone version (see above).
2. Run the standalone version to update the abapGit developer version.
3. If the update does not complete, delete any inactive objects or objects with diffs completely (like inactive classes in SE24).
4. Restart the standalone version and perform a new "Pull" or "Import ZIP".
5. If this does not resolve the problem, open an issue on [GitHub](https://github.com/abapGit/abapGit/issues).

