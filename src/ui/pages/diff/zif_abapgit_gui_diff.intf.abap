INTERFACE zif_abapgit_gui_diff PUBLIC.

  TYPES:
    BEGIN OF ty_file_diff,
      path       TYPE string,
      filename   TYPE string,
      obj_type   TYPE string,
      obj_name   TYPE string,
      lstate     TYPE c LENGTH 1,
      rstate     TYPE c LENGTH 1,
      fstate     TYPE c LENGTH 1, " FILE state - Abstraction for shorter ifs
      o_diff     TYPE REF TO zif_abapgit_diff,
      changed_by TYPE syuname,
      type       TYPE string,
    END OF ty_file_diff.
  TYPES:
    ty_file_diffs TYPE STANDARD TABLE OF ty_file_diff
                        WITH NON-UNIQUE DEFAULT KEY
                        WITH NON-UNIQUE SORTED KEY secondary
                             COMPONENTS path filename.

ENDINTERFACE.
