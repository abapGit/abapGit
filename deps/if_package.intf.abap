INTERFACE if_package PUBLIC.
  DATA wbo_korr_flag         TYPE c LENGTH 1 READ-ONLY.
  DATA software_component    TYPE string READ-ONLY.
  DATA application_component TYPE string READ-ONLY.
  DATA transport_layer       TYPE string READ-ONLY.
  DATA changed_by            TYPE string READ-ONLY.

  METHODS save
    IMPORTING
      i_transport_request TYPE trkorr OPTIONAL
    EXCEPTIONS
      object_invalid
      object_not_changeable
      cancelled_in_corr
      permission_failure
      unexpected_error
      intern_err.

  METHODS delete
    EXCEPTIONS
      object_not_empty
      object_not_changeable
      object_invalid
      intern_err.

  METHODS set_changeable
    IMPORTING
      i_changeable TYPE abap_bool.

  METHODS get_changeable
    EXPORTING
      e_changeable TYPE abap.

  METHODS get_all_attributes
    EXPORTING
      e_package_data TYPE scompkdtln
    EXCEPTIONS
      object_invalid
      package_deleted
      intern_err.

  METHODS save_generic
    IMPORTING
      i_save_sign         TYPE paksavsign
      i_transport_request TYPE trkorr OPTIONAL
      i_suppress_dialog   TYPE abap_bool DEFAULT ' '
    EXPORTING
      e_transport_request TYPE trkorr
    EXCEPTIONS
      cancelled_in_corr
      permission_failure
      object_not_changeable
      object_invalid.

  METHODS set_all_attributes
    IMPORTING
      i_package_data TYPE any
      i_data_sign TYPE any
    EXCEPTIONS
      object_not_changeable
      object_deleted
      object_invalid
      short_text_missing
      author_not_existing
      local_package
      software_component_invalid
      layer_invalid
      korrflag_invalid
      component_not_existing
      component_missing
      authorize_failure
      prefix_in_use
      unexpected_error
      intern_err
      wrong_mainpack_value
      superpackage_invalid.

  METHODS set_permissions_changeable
    IMPORTING
      i_changeable              TYPE abap_bool
      i_suppress_dialog         TYPE abap_bool DEFAULT abap_false
      i_suppress_language_check TYPE abap_bool DEFAULT abap_false
    EXPORTING
      e_deleted_permissions     TYPE any
    EXCEPTIONS
      object_already_changeable
      object_already_unlocked
      object_locked_by_other_user
      object_modified
      object_just_created
      object_deleted
      permission_failure
      object_invalid
      unexpected_error.

  METHODS get_permissions_to_use
    EXPORTING
      e_permissions TYPE tpak_permission_to_use_list
    EXCEPTIONS
      object_invalid
      unexpected_error.

  METHODS add_permission_to_use
    IMPORTING
      i_pkg_permission_data TYPE any
    EXPORTING
      e_pkg_permission TYPE REF TO if_package_permission_to_use
    EXCEPTIONS
      object_not_changeable
      object_access_error
      object_already_existing
      object_invalid
      unexpected_error.
ENDINTERFACE.
