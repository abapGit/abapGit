CLASS zcl_abapgit_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    CONSTANTS:
      gc_artifact_id TYPE string VALUE 'abapGit' ##NO_TEXT,
      gc_group_id    TYPE string VALUE 'https://github.com/larshp' ##NO_TEXT,
      gc_repository  TYPE string VALUE 'https://github.com/larshp/abapGit.git' ##NO_TEXT.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_apack IMPLEMENTATION.


  METHOD constructor.

    if_apack_manifest~descriptor-group_id    = gc_group_id.
    if_apack_manifest~descriptor-artifact_id = gc_artifact_id.
    if_apack_manifest~descriptor-version     = zif_abapgit_version=>gc_abap_version.
    if_apack_manifest~descriptor-git_url     = gc_repository.

  ENDMETHOD.


ENDCLASS.
