INTERFACE zif_abapgit_apack_definitions PUBLIC.

  TYPES:
    BEGIN OF ty_dependency,
      group_id       TYPE string,
      artifact_id    TYPE string,
      version        TYPE string,
      git_url        TYPE string,
      target_package TYPE devclass,
    END OF ty_dependency,

    tt_dependencies    TYPE STANDARD TABLE OF ty_dependency
                    WITH NON-UNIQUE DEFAULT KEY,

    ty_repository_type TYPE string,

    BEGIN OF ty_descriptor_wo_dependencies,
      group_id        TYPE string,
      artifact_id     TYPE string,
      version         TYPE string,
      repository_type TYPE ty_repository_type,
      git_url         TYPE string,
    END OF ty_descriptor_wo_dependencies,

    BEGIN OF ty_descriptor.
      INCLUDE TYPE ty_descriptor_wo_dependencies.
  TYPES:
    dependencies TYPE tt_dependencies,
    END OF ty_descriptor.

  CONSTANTS c_dot_apack_manifest TYPE string VALUE '.apack-manifest.xml' ##NO_TEXT.
  CONSTANTS c_repository_type_abapgit TYPE ty_repository_type VALUE 'abapGit' ##NO_TEXT.

ENDINTERFACE.
