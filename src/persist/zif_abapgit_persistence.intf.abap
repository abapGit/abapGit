INTERFACE zif_abapgit_persistence PUBLIC.

  TYPES:
    ty_type  TYPE c LENGTH 12 .
  TYPES:
    ty_value TYPE c LENGTH 12 .
  TYPES:
    BEGIN OF ty_content,
      type     TYPE ty_type,
      value    TYPE ty_value,
      data_str TYPE string,
    END OF ty_content .
  TYPES:
    tt_content TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY type value .

  TYPES: BEGIN OF ty_local_checksum,
           item  TYPE zif_abapgit_definitions=>ty_item,
           files TYPE zif_abapgit_definitions=>ty_file_signatures_tt,
         END OF ty_local_checksum.

  TYPES:
    BEGIN OF ty_local_settings,
      ignore_subpackages TYPE abap_bool,
      write_protected    TYPE abap_bool,
      only_local_objects TYPE abap_bool,
    END OF ty_local_settings.

  TYPES: ty_local_checksum_tt TYPE STANDARD TABLE OF ty_local_checksum WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_repo_xml,
           url             TYPE string,
           branch_name     TYPE string,
           sha1            TYPE zif_abapgit_definitions=>ty_sha1,
           package         TYPE devclass,
           offline         TYPE sap_bool,
           local_checksums TYPE ty_local_checksum_tt,
           dot_abapgit     TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
           head_branch     TYPE string,   " HEAD symref of the repo, master branch
           local_settings  TYPE ty_local_settings,
         END OF ty_repo_xml.

  TYPES: BEGIN OF ty_repo,
           key TYPE zif_abapgit_persistence=>ty_value.
      INCLUDE TYPE ty_repo_xml.
  TYPES: END OF ty_repo.
  TYPES: tt_repo TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY.
  TYPES: tt_repo_keys TYPE STANDARD TABLE OF ty_repo-key WITH DEFAULT KEY.

ENDINTERFACE.
