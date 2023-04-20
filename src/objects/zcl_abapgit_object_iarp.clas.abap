CLASS zcl_abapgit_object_iarp DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING is_item     TYPE zif_abapgit_definitions=>ty_item
                  iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ms_name     TYPE w3resokey.

    METHODS:
      read
        EXPORTING es_attributes TYPE w3resoattr
                  et_parameters TYPE w3resopara_tabletype
        RAISING   zcx_abapgit_exception,

      save
        IMPORTING is_attributes TYPE w3resoattr
                  it_parameters TYPE w3resopara_tabletype
        RAISING   zcx_abapgit_exception,

      w3_api_load
        RETURNING VALUE(ri_resource) TYPE REF TO if_w3_api_resource
        RAISING   zcx_abapgit_exception,

      w3_api_get_attributes
        IMPORTING ii_resource          TYPE REF TO if_w3_api_resource
        RETURNING VALUE(rs_attributes) TYPE w3resoattr
        RAISING   zcx_abapgit_exception,

      w3_api_get_parameters
        IMPORTING ii_resource          TYPE REF TO if_w3_api_resource
        RETURNING VALUE(rt_parameters) TYPE w3resopara_tabletype
        RAISING   zcx_abapgit_exception,

      w3_api_create_new
        IMPORTING is_attributes      TYPE w3resoattr
        RETURNING VALUE(ri_resource) TYPE REF TO if_w3_api_resource
        RAISING   zcx_abapgit_exception,

      w3_api_set_attributes
        IMPORTING ii_resource   TYPE REF TO if_w3_api_resource
                  is_attributes TYPE w3resoattr
        RAISING   zcx_abapgit_exception,

      w3_api_set_parameters
        IMPORTING ii_resource   TYPE REF TO if_w3_api_resource
                  it_parameters TYPE w3resopara_tabletype
        RAISING   zcx_abapgit_exception,

      w3_api_save
        IMPORTING ii_resource TYPE REF TO if_w3_api_resource
        RAISING   zcx_abapgit_exception,

      w3_api_set_changeable
        IMPORTING ii_resource   TYPE REF TO if_w3_api_resource
                  iv_changeable TYPE abap_bool DEFAULT abap_true
        RAISING   zcx_abapgit_exception,

      w3_api_delete
        IMPORTING ii_resource TYPE REF TO if_w3_api_resource
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_iarp IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    ms_name = ms_item-obj_name.

  ENDMETHOD.


  METHOD read.

    DATA: li_resource TYPE REF TO if_w3_api_resource.

    li_resource = w3_api_load( ).
    es_attributes = w3_api_get_attributes( li_resource ).

    CLEAR: es_attributes-chname,
           es_attributes-tdate,
           es_attributes-ttime,
           es_attributes-devclass.

    et_parameters = w3_api_get_parameters( li_resource ).

  ENDMETHOD.


  METHOD save.

    DATA: li_resource TYPE REF TO if_w3_api_resource.

    li_resource = w3_api_create_new( is_attributes ).

    w3_api_set_attributes(
        ii_resource   = li_resource
        is_attributes = is_attributes ).

    w3_api_set_parameters(
        ii_resource   = li_resource
        it_parameters = it_parameters ).

    w3_api_save( li_resource ).

    " Release locks
    w3_api_set_changeable(
      ii_resource   = li_resource
      iv_changeable = abap_false ).

  ENDMETHOD.


  METHOD w3_api_create_new.

    cl_w3_api_resource=>if_w3_api_resource~create_new(
      EXPORTING
        p_resource_data         = is_attributes
      IMPORTING
        p_resource              = ri_resource
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        OTHERS                  = 8 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_resource~create_new. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_delete.

    ii_resource->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_object~delete. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_get_attributes.

    ii_resource->get_attributes(
      IMPORTING
        p_attributes     = rs_attributes
      EXCEPTIONS
        object_invalid   = 1
        resource_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_resource~get_attributes. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_get_parameters.

    ii_resource->get_parameters(
      IMPORTING
        p_parameters     = rt_parameters
      EXCEPTIONS
        object_invalid   = 1
        resource_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_resource~get_parameters. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_load.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ms_name
      IMPORTING
        p_resource          = ri_resource
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from w3api_resource~load. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_save.

    ii_resource->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_object~save. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_set_attributes.

    ii_resource->set_attributes(
      EXPORTING
        p_attributes          = is_attributes
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_resource~set_attributes. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_set_changeable.

    ii_resource->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changeable
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        OTHERS                       = 12 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_object~set_changeable. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_set_parameters.

    ii_resource->set_parameters(
      EXPORTING
        p_parameters          = it_parameters
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        authorize_failure     = 4
        invalid_parameter     = 5
        error_occured         = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error from if_w3_api_resource~set_parameters. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: li_resource TYPE REF TO if_w3_api_resource.

    li_resource = w3_api_load( ).
    w3_api_set_changeable( li_resource ).
    w3_api_delete( li_resource ).
    w3_api_save( li_resource ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attributes       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        w3_api_load( ).
        rv_bool = abap_true.

      CATCH zcx_abapgit_exception INTO lx_error.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attributes       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
ENDCLASS.
