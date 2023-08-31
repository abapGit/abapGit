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

  TYPES:
    ty_chmod TYPE c LENGTH 6 .

  CONSTANTS:
    BEGIN OF c_chmod,
      file       TYPE ty_chmod VALUE '100644',
      executable TYPE ty_chmod VALUE '100755',
      dir        TYPE ty_chmod VALUE '40000 ',
      submodule  TYPE ty_chmod VALUE '160000',
    END OF c_chmod .

  TYPES:
    BEGIN OF ty_expanded,
      path  TYPE string,
      name  TYPE string,
      sha1  TYPE ty_sha1,
      chmod TYPE ty_chmod,
    END OF ty_expanded .
  TYPES:
    ty_expanded_tt TYPE STANDARD TABLE OF ty_expanded WITH DEFAULT KEY
      WITH NON-UNIQUE SORTED KEY path_name COMPONENTS path name.

  TYPES:
    BEGIN OF ty_create,
      name   TYPE string,
      parent TYPE string,
    END OF ty_create .
  TYPES:
    BEGIN OF ty_commit,
      sha1       TYPE ty_sha1,
      parent1    TYPE ty_sha1,
      parent2    TYPE ty_sha1,
      author     TYPE string,
      email      TYPE string,
      time       TYPE string,
      message    TYPE string,
      body       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      branch     TYPE string,
      merge      TYPE string,
      tags       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      create     TYPE STANDARD TABLE OF ty_create WITH DEFAULT KEY,
      compressed TYPE abap_bool,
    END OF ty_commit .
  TYPES:
    ty_commit_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY .

  CONSTANTS:
    BEGIN OF c_type,
      commit TYPE ty_type VALUE 'commit',                   "#EC NOTEXT
      tree   TYPE ty_type VALUE 'tree',                     "#EC NOTEXT
      ref_d  TYPE ty_type VALUE 'ref_d',                    "#EC NOTEXT
      tag    TYPE ty_type VALUE 'tag',                      "#EC NOTEXT
      blob   TYPE ty_type VALUE 'blob',                     "#EC NOTEXT
    END OF c_type .

  CONSTANTS:
    BEGIN OF c_git_branch_type,
      branch          TYPE ty_git_branch_type VALUE 'HD',
      lightweight_tag TYPE ty_git_branch_type VALUE 'TG',
      annotated_tag   TYPE ty_git_branch_type VALUE 'AT',
      other           TYPE ty_git_branch_type VALUE 'ZZ',
    END OF c_git_branch_type .
  CONSTANTS c_head_name TYPE string VALUE 'HEAD' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_git_branch,
      main         TYPE string VALUE 'refs/heads/main',
      prefix       TYPE string VALUE 'refs/',
      heads_prefix TYPE string VALUE 'refs/heads/',
      heads        TYPE string VALUE 'refs/heads/*',
      tags_prefix  TYPE string VALUE 'refs/tags/',
      tags         TYPE string VALUE 'refs/tags/*',
      peel         TYPE string VALUE '^{}',
    END OF c_git_branch.

ENDINTERFACE.
