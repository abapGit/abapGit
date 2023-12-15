*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF ty_path_name,
    path        TYPE string,
    filename    TYPE string,
    remote_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1,
    local_sha1  TYPE zif_abapgit_git_definitions=>ty_sha1,
  END OF ty_path_name.
TYPES:
  ty_path_name_tt TYPE HASHED TABLE OF ty_path_name WITH UNIQUE KEY path filename.

TYPES: BEGIN OF ty_feature,
         BEGIN OF repo,
           name    TYPE string,
           key     TYPE zif_abapgit_persistence=>ty_repo-key,
           package TYPE devclass,
         END OF repo,
         BEGIN OF branch,
           display_name TYPE string,
           sha1         TYPE zif_abapgit_git_definitions=>ty_sha1,
           up_to_date   TYPE abap_bool,
         END OF branch,
         BEGIN OF pr,
           title TYPE string,
           url   TYPE string,
           draft TYPE abap_bool,
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
