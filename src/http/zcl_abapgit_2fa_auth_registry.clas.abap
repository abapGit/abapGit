"! Static registry class to find {@link ZIF_ABAPGIT_2FA_AUTHENTICATOR} instances
CLASS zcl_abapgit_2fa_auth_registry DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      "! Retrieve an authenticator instance by url
      "! @parameter iv_url | Url of the repository / service
      "! @parameter ri_authenticator | Found authenticator instance
      "! @raising zcx_abapgit_2fa_unsupported | No authenticator found that supports the service
      get_authenticator_for_url IMPORTING iv_url                  TYPE string
                                RETURNING VALUE(ri_authenticator) TYPE REF TO zif_abapgit_2fa_authenticator
                                RAISING   zcx_abapgit_2fa_unsupported,
      "! Check if there is a two factor authenticator available for the url
      "! @parameter iv_url | Url of the repository / service
      "! @parameter rv_supported | 2FA is supported
      is_url_supported IMPORTING iv_url              TYPE string
                       RETURNING VALUE(rv_supported) TYPE abap_bool,
      "! Offer to use two factor authentication if supported and required
      "! <p>
      "! This uses GUI functionality to display a popup to request the user to enter a two factor
      "! token. Also an dummy authentication request might be used to find out if two factor
      "! authentication is required for the account.
      "! </p>
      "! @parameter iv_url | Url of the repository / service
      "! @parameter cv_username | Username
      "! @parameter cv_password | Password, will be replaced by an access token if two factor
      "!                          authentication succeeds
      "! @raising zcx_abapgit_exception | Error in two factor authentication
      use_2fa_if_required IMPORTING iv_url      TYPE string
                          CHANGING  cv_username TYPE string
                                    cv_password TYPE string
                          RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
    CLASS-DATA:
      "! All authenticators managed by the registry
      gt_registered_authenticators TYPE HASHED TABLE OF REF TO zif_abapgit_2fa_authenticator
                                        WITH UNIQUE KEY table_line.

  PRIVATE SECTION.
    CLASS-METHODS:
      popup_token
        RETURNING VALUE(rv_token) TYPE string
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_2FA_AUTH_REGISTRY IMPLEMENTATION.


  METHOD class_constructor.

    DATA: lt_sub           TYPE seo_relkeys,
          ls_sub           LIKE LINE OF lt_sub,
          li_authenticator TYPE REF TO zif_abapgit_2fa_authenticator,
          lo_class         TYPE REF TO cl_oo_class.


    TRY.
        lo_class ?= cl_oo_class=>get_instance( 'ZCL_ABAPGIT_2FA_AUTH_BASE' ).
        lt_sub = lo_class->get_subclasses( ).
        SORT lt_sub BY clsname ASCENDING AS TEXT.
        LOOP AT lt_sub INTO ls_sub.
          CREATE OBJECT li_authenticator TYPE (ls_sub-clsname).
          INSERT li_authenticator INTO TABLE gt_registered_authenticators.
        ENDLOOP.
      CATCH cx_class_not_existent.
* class in local report
        CREATE OBJECT li_authenticator TYPE zcl_abapgit_2fa_github_auth.
        INSERT li_authenticator INTO TABLE gt_registered_authenticators.
    ENDTRY.

  ENDMETHOD.


  METHOD get_authenticator_for_url.
    FIELD-SYMBOLS: <li_authenticator> LIKE LINE OF gt_registered_authenticators.

    LOOP AT gt_registered_authenticators ASSIGNING <li_authenticator>.
      IF <li_authenticator>->supports_url( iv_url ) = abap_true.
        ri_authenticator = <li_authenticator>.
        RETURN.
      ENDIF.
    ENDLOOP.

    RAISE EXCEPTION TYPE zcx_abapgit_2fa_unsupported.
  ENDMETHOD.


  METHOD is_url_supported.
    TRY.
        get_authenticator_for_url( iv_url ).
        rv_supported = abap_true.
      CATCH zcx_abapgit_2fa_unsupported ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD popup_token.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TADIR'.
    <ls_field>-fieldname = 'OBJ_NAME'.
    <ls_field>-fieldtext = 'Two factor auth. token'.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Two factor auth. token'
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2. "#EC NOTEXT
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_returncode = 'A'.
      zcx_abapgit_exception=>raise( 'Authentication cancelled' ).
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rv_token = <ls_field>-value.

  ENDMETHOD.


  METHOD use_2fa_if_required.
    DATA: li_authenticator TYPE REF TO zif_abapgit_2fa_authenticator,
          lv_2fa_token     TYPE string,
          lv_access_token  TYPE string,
          lx_ex            TYPE REF TO cx_root.

    IF is_url_supported( iv_url ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        li_authenticator = get_authenticator_for_url( iv_url ).
        li_authenticator->begin( ).

        " Is two factor authentication required for this account?
        IF li_authenticator->is_2fa_required( iv_url      = iv_url
                                              iv_username = cv_username
                                              iv_password = cv_password ) = abap_true.

          lv_2fa_token = popup_token( ).

          " Delete an old access token if it exists
          li_authenticator->delete_access_tokens( iv_url       = iv_url
                                                  iv_username  = cv_username
                                                  iv_password  = cv_password
                                                  iv_2fa_token = lv_2fa_token ).

          " Get a new access token
          lv_access_token = li_authenticator->authenticate( iv_url       = iv_url
                                                            iv_username  = cv_username
                                                            iv_password  = cv_password
                                                            iv_2fa_token = lv_2fa_token ).

          " Use the access token instead of the password
          cv_password = lv_access_token.
        ENDIF.

        li_authenticator->end( ).

      CATCH zcx_abapgit_2fa_error INTO lx_ex.
        TRY.
            li_authenticator->end( ).
          CATCH zcx_abapgit_2fa_illegal_state ##NO_HANDLER.
        ENDTRY.

        zcx_abapgit_exception=>raise( |2FA error: { lx_ex->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
