INTERFACE zif_abapgit_flow_logic
  PUBLIC .

  TYPES:
    BEGIN OF ty_path_name,
      path        TYPE string,
      filename    TYPE string,
      remote_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1,
      local_sha1  TYPE zif_abapgit_git_definitions=>ty_sha1,
    END OF ty_path_name.
  TYPES:
    ty_path_name_tt TYPE HASHED TABLE OF ty_path_name WITH UNIQUE KEY path filename.

  TYPES: BEGIN OF ty_branch,
           display_name TYPE string,
           sha1         TYPE zif_abapgit_git_definitions=>ty_sha1,
           up_to_date   TYPE abap_bool,
           first_commit TYPE zif_abapgit_git_definitions=>ty_sha1,
         END OF ty_branch.

  TYPES: BEGIN OF ty_feature,
           BEGIN OF repo,
             name    TYPE string,
             key     TYPE zif_abapgit_persistence=>ty_repo-key,
             package TYPE devclass,
           END OF repo,
           branch          TYPE ty_branch,
           BEGIN OF pr,
             title  TYPE string,
             url    TYPE string,
             number TYPE i,
             draft  TYPE abap_bool,
           END OF pr,
           BEGIN OF transport,
             trkorr TYPE trkorr,
             title  TYPE string,
           END OF transport,
           full_match      TYPE abap_bool,
           changed_files   TYPE ty_path_name_tt,
           changed_objects TYPE zif_abapgit_definitions=>ty_items_ts,
         END OF ty_feature.
  TYPES ty_features TYPE STANDARD TABLE OF ty_feature WITH DEFAULT KEY.

  TYPES: ty_transport_duplicates_tt TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_item_signature WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_information,
            features             TYPE ty_features,
            errors               TYPE string_table,
            transport_duplicates TYPE ty_transport_duplicates_tt,
         END OF ty_information.

  CONSTANTS c_main TYPE string VALUE 'main'.

**************************************

  TYPES: BEGIN OF ty_consolidate,
           errors         TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           warnings       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           success        TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           missing_remote TYPE ty_path_name_tt,
           only_remote    TYPE ty_path_name_tt,
         END OF ty_consolidate.

ENDINTERFACE.
