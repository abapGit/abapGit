INTERFACE zif_abapgit_auth PUBLIC.

  TYPES: ty_authorization TYPE string.

  CONSTANTS: BEGIN OF gc_authorization,
               uninstall             TYPE ty_authorization VALUE 'UNINSTALL',
               transport_to_branch   TYPE ty_authorization VALUE 'TRANSPORT_TO_BRANCH',
               update_local_checksum TYPE ty_authorization VALUE 'UPDATE_LOCAL_CHECKSUM',
             END OF gc_authorization.

  METHODS:
    is_allowed
      IMPORTING iv_authorization  TYPE ty_authorization
                iv_param          TYPE string OPTIONAL
      RETURNING VALUE(rv_allowed) TYPE abap_bool.

ENDINTERFACE.
