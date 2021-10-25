---
title: Moving package into git
category: online projects
order: 50
---

You have an existing package in your system that you want to copy to a new git repository:

![](img/existing_package.png)

Initialize the git repository, make sure it is not emtpy (typically you add README and LICENSE files from a template):

![](img/existing_git_before.png)

## New Repository

In abapGit, create a repository via the "New Online" button:

![](img/existing_clone.png)

The existing objects will show up in the worklist making it possible to commit the objects to the git repository:

![](img/existing_repo.png)

## Stage and Commit

Select "Stage", "Add All and Commit", and finally "Commit" to transfer all changes to your git repository. For details, see 
[Committing changes to git](https://docs.abapgit.org/guide-stage_commit.html).

Now your local abapGit repo and the remote git repository are in sync. 

![](img/existing_result.png)

You can view the updated repository, which will contains a [.abapgit.xml](https://docs.abapgit.org/settings-dot-abapgit.html) file
and a `/src/` folder with all your objects. 

![](img/existing_git_after.png)


