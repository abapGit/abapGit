INTERFACE zif_abapgit_auth PUBLIC.

  TYPES: ty_authorization TYPE string.

  CONSTANTS: BEGIN OF c_authorization,
               startup               TYPE ty_authorization VALUE 'STARTUP',
               uninstall             TYPE ty_authorization VALUE 'UNINSTALL',
               create_repo           TYPE ty_authorization VALUE 'CREATE_REPO',
               transport_to_branch   TYPE ty_authorization VALUE 'TRANSPORT_TO_BRANCH',
               update_local_checksum TYPE ty_authorization VALUE 'UPDATE_LOCAL_CHECKSUM',
             END OF c_authorization.

  METHODS:
    is_allowed
      IMPORTING iv_authorization  TYPE ty_authorization
                iv_param          TYPE string OPTIONAL
      RETURNING VALUE(rv_allowed) TYPE abap_bool.

ENDINTERFACE.
