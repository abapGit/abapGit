INTERFACE zif_abapgit_git_definitions
  PUBLIC .
* this interface is self contained
* only references to built in types
* git does not know the concept of TADIR objects, only knows files
  TYPES:
    ty_type    TYPE c LENGTH 6 .
  TYPES:
    ty_bitbyte TYPE c LENGTH 8 .
  TYPES:
    ty_sha1    TYPE c LENGTH 40 .
  TYPES: ty_sha1_tt TYPE STANDARD TABLE OF ty_sha1 WITH DEFAULT KEY .
  TYPES:
    ty_adler32 TYPE x LENGTH 4 .

  TYPES ty_item_state TYPE c LENGTH 1.
  TYPES:
    BEGIN OF ty_file_signature,
      path     TYPE string,
      filename TYPE string,
      sha1     TYPE ty_sha1,
    END OF ty_file_signature .
  TYPES:
    ty_file_signatures_tt TYPE STANDARD TABLE OF
           ty_file_signature WITH DEFAULT KEY .
  TYPES:
    ty_file_signatures_ts TYPE SORTED TABLE OF
           ty_file_signature WITH UNIQUE KEY path filename .
  TYPES:
    BEGIN OF ty_file.
      INCLUDE TYPE ty_file_signature.
  TYPES: data TYPE xstring,
    END OF ty_file .
  TYPES:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY
                     WITH UNIQUE SORTED KEY file_path COMPONENTS path filename
                     WITH NON-UNIQUE SORTED KEY file COMPONENTS filename.

  TYPES ty_git_branch_type TYPE c LENGTH 2 .
  TYPES:
    BEGIN OF ty_git_branch,
      sha1         TYPE ty_sha1,
      name         TYPE string,
      type         TYPE ty_git_branch_type,
      is_head      TYPE abap_bool,
      display_name TYPE string,
    END OF ty_git_branch .
  TYPES:
    ty_git_branch_list_tt TYPE STANDARD TABLE OF ty_git_branch WITH DEFAULT KEY
                               WITH NON-UNIQUE SORTED KEY name_key
                               COMPONENTS name.
  TYPES:
    BEGIN OF ty_git_tag,
      sha1         TYPE ty_sha1,
      object       TYPE ty_sha1,
      name         TYPE string,
      type         TYPE ty_git_branch_type,
      display_name TYPE string,
      tagger_name  TYPE string,
      tagger_email TYPE string,
      message      TYPE string,
      body         TYPE string,
    END OF ty_git_tag .
  TYPES:
    BEGIN OF ty_git_user,
      name  TYPE string,
      email TYPE string,
    END OF ty_git_user .
  TYPES:
    BEGIN OF ty_comment,
      committer TYPE ty_git_user,
      author    TYPE ty_git_user,
      comment   TYPE string,
    END OF ty_comment .

ENDINTERFACE.
