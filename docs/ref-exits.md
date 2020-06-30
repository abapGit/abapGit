---
title: User Exits
category: reference
order: 40
---

abapGit contains predefined user exits which can be used to modify the standard behavior.


If the compiled report is installed, create include `ZABAPGIT_USER_EXIT` and add local class `ZCL_ABAPGIT_USER_EXIT` implementing `ZIF_ABAPGIT_EXIT`.

If the development version is installed create global class `ZCL_ABAPGIT_USER_EXIT` implementing `ZIF_ABAPGIT_EXIT`.

In both cases add the object in a package different from the main abapGit code.

The list of user exits can change at any time, make sure to syntax check user exits after upgrade of abapGit.

### CHANGE_LOCAL_HOST
If the hostnames are not properly configured, this exit can be used to modify the settings.
This is especially useful when running abapGitServer on the local system.

### ALLOW_SAP_OBJECTS
Force allowing serialization of SAP objects.

### CHANGE_PROXY_URL
todo

### CHANGE_PROXY_PORT
todo

### CHANGE_PROXY_AUTHENTICATION
todo

### CREATE_HTTP_CLIENT
Store username and password in RFC connection setup, see [https://github.com/larshp/abapGit/issues/1841](https://github.com/larshp/abapGit/issues/1841)

### HTTP_CLIENT
Can be used for setting logon tickets eg. in connection with abapGitServer connections between SAP systems.
[https://gist.github.com/larshp/71609852a79aa1e877f8c4020d18feac](https://gist.github.com/larshp/71609852a79aa1e877f8c4020d18feac)

### CHANGE_TADIR
Can be used to skip certian objects, or force a different object setup than currently in TADIR.

### GET_SSL_ID
Possibility to change the default `ANONYM` ssl id to something system specific

### CUSTOM_SERIALIZE_ABAP_CLIF
Allows for a custom serializer to be used for global classes' CLIF sources. See [#2321](https://github.com/larshp/abapGit/issues/2321) and [#2491](https://github.com/larshp/abapGit/pull/2491) for use cases.  
This [example implementation](https://gist.github.com/flaiker/999c8165b89131608b05cd371529fef5) forces the old class serializer to be used for specific packages.

### DESERIALIZE_POSTPROCESS
Can be used for any postprocessing operation for deserialized objects. Since it is a postprocessing step, only logs can be added to II_LOG and one should not terminate the process by raising exception, which may lead to inconsistencies.
