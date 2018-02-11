*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_USER_EXITS
*&---------------------------------------------------------------------*

* add class ZCL_ABAPGIT_USER_EXIT implementing ZIF_ABAPGIT_EXIT in following include,
* place the include in a different package than ZABAPGIT
INCLUDE zabapgit_user_exit IF FOUND.
