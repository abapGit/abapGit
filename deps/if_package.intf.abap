INTERFACE if_package PUBLIC.
  DATA wbo_korr_flag         TYPE c LENGTH 1 READ-ONLY.
  DATA software_component    TYPE string READ-ONLY.
  DATA application_component TYPE string READ-ONLY.
  DATA transport_layer       TYPE string READ-ONLY.

  METHODS save
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
ENDINTERFACE.
