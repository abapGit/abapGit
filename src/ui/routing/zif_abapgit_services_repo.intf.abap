INTERFACE zif_abapgit_services_repo
  PUBLIC .

  TYPES:
    BEGIN OF ty_repo_params,
      url                TYPE string,
      package            TYPE devclass,
      branch_name        TYPE string,
      display_name       TYPE string,
      folder_logic       TYPE string,
      labels             TYPE string,
      ignore_subpackages TYPE abap_bool,
      main_lang_only     TYPE abap_bool,
    END OF ty_repo_params .

ENDINTERFACE.
