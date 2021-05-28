---
title: User Exits
category: reference
order: 40
---

abapGit contains predefined user exits which can be used to modify the standard behavior.

## Overview

If the standalone version is installed, create include `ZABAPGIT_USER_EXIT` and add local class `ZCL_ABAPGIT_USER_EXIT` implementing `ZIF_ABAPGIT_EXIT`.

If the development version is installed create global class `ZCL_ABAPGIT_USER_EXIT` implementing `ZIF_ABAPGIT_EXIT`.

To support both versions with the same code, proceed as follows: 

1. Implement `ZCL_ABAPGIT_USER_EXIT` as global class and test with the developer version.
2. Cut & paste complete code of `ZCL_ABAPGIT_USER_EXIT` into include `ZABAPGIT_USER_EXIT` and change the beginning to a local class.
```abap
CLASS zcl_abapgit_user_exit DEFINITION
  FINAL
  CREATE PUBLIC.
```
3. Activate the include.

In either cases, add the object in a package different from the main abapGit code.

The list of user exits can change at any time, make sure to syntax check user exits after updating abapGit.

## Exits

### CHANGE_LOCAL_HOST

If the hostnames are not properly configured, this exit can be used to modify the settings.
This is especially useful when running abapGitServer on the local system.

### ALLOW_SAP_OBJECTS

Force allowing serialization of SAP objects.

### CHANGE_PROXY_URL

Determine the proxy URL from the repository URL.

### CHANGE_PROXY_PORT

Determine the proxy port from the repository URL.

### CHANGE_PROXY_AUTHENTICATION

Determine based on the repository URL if authentication is required when accessing the proxy.

### CREATE_HTTP_CLIENT

Store username and password in RFC connection setup (see [#1841](https://github.com/abapGit/abapGit/issues/1841)).

### HTTP_CLIENT

Can be used for setting logon tickets eg. in connection with abapGitServer connections between SAP systems ([Example](https://gist.github.com/larshp/71609852a79aa1e877f8c4020d18feac)).

### CHANGE_TADIR

Can be used to skip certain objects, or force a different object setup than currently in TADIR ([Example](https://gist.github.com/larshp/cca0ce0ba65efcde5dfcae416b0484f7).

### GET_SSL_ID

Possibility to change the default `ANONYM` ssl id to something system specific.

### CUSTOM_SERIALIZE_ABAP_CLIF

Allows for a custom serializer to be used for global classes' CLIF sources. See [#2321](https://github.com/abapGit/abapGit/issues/2321) and [#2491](https://github.com/abapGit/abapGit/pull/2491) for use cases.
This [example implementation](https://gist.github.com/flaiker/999c8165b89131608b05cd371529fef5) forces the old class serializer to be used for specific packages.

### DESERIALIZE_POSTPROCESS

Can be used for any postprocessing operation for deserialized objects. Since it is a postprocessing step, only logs can be added to II_LOG and one should not terminate the process by raising exception, which may lead to inconsistencies.

### ADJUST_DISPLAY_COMMIT_URL

Can be used to set the URL to display a commit. There are default implementations for some providers:

| Provider  | Repo URL | Show Commit URL |
|-----------|----------|-----------------|
| github    | http(s)://github.com/<user\>/\<repo\>.git    | http(s)://github.com/<user\>/\<repo\>/commit/<sha1\>     |
| bitbucket | http(s)://bitbucket.org/<user\>/\<repo\>.git | http(s)://bitbucket.org/<user\>/\<repo\>/commits/<sha1\> |
| gitlab    | http(s)://gitlab.com/<user\>/\<repo\>.git    | http(s)://gitlab.com/\<user\>/\<repo\>/-/commit/<sha1\>  |

### PRE_CALCULATE_REPO_STATUS

Can be used to modify local and remote files before calculating diff status. Useful to remove diffs which are caused by deployment between different system version (see also [abapgit xml stripper plugin](https://github.com/sbcgua/abapgit_xml_stripper_plugin)).

![diff sample](./img/deployment_diff_difference_sample.png)

The exit also receives a repo meta data snapshot (`zif_abapgit_persistence=>ty_repo`) to identify the repo and it's attributes in the current system (e.g. package). This can be used to enable/disable the exit for specific repos.

### WALL_MESSAGE_LIST

Can be used to add a message at list level (repository overview, see [#4653](https://github.com/abapGit/abapGit/issues/4653)).

### WALL_MESSAGE_REPO

Can be used to add a message at repo level (repository view, see [#4653](https://github.com/abapGit/abapGit/issues/4653)).

### ON_EVENT

This exit allows you to extend abapGit with new features that are not suitable for abapGit itself. For example, you can link to a new page from a wall message. Another use case is redirecting menu items to a custom page rather than standard abapGit, for example using a company-specific solution to replace "Advanced > Run Code Inspector" (see [#4722](https://github.com/abapGit/abapGit/issues/4722)).
