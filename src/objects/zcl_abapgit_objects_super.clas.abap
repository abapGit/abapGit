CLASS zcl_abapgit_objects_super DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS c_user_unknown TYPE syuname VALUE 'UNKNOWN'.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL.

    METHODS get_accessed_files
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt.
  PROTECTED SECTION.

    DATA:
      ms_item        TYPE zif_abapgit_definitions=>ty_item,
      mv_language    TYPE spras,
      mo_files       TYPE REF TO zcl_abapgit_objects_files,
      mo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.

    METHODS get_metadata
      RETURNING
        VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
    METHODS corr_insert
      IMPORTING
        !iv_package      TYPE devclass
        !ig_object_class TYPE any OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS tadir_insert
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS tadir_delete
      RAISING
        zcx_abapgit_exception .
    METHODS exists_a_lock_entry_for
      IMPORTING
        !iv_lock_object               TYPE string
        !iv_argument                  TYPE csequence OPTIONAL
        !iv_prefix                    TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_exists_a_lock_entry) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS set_default_package
      IMPORTING
        !iv_package TYPE devclass .
    METHODS set_default_transport
      IMPORTING
        !iv_transport TYPE trkorr.
    METHODS serialize_longtexts
      IMPORTING
        !ii_xml           TYPE REF TO zif_abapgit_xml_output
        !iv_longtext_id   TYPE dokil-id OPTIONAL
        !it_dokil         TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
        !iv_longtext_name TYPE string DEFAULT 'LONGTEXTS'
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_longtexts
      IMPORTING
        !ii_xml           TYPE REF TO zif_abapgit_xml_input
        !iv_longtext_id   TYPE dokil-id OPTIONAL
        !iv_longtext_name TYPE string DEFAULT 'LONGTEXTS'
      RAISING
        zcx_abapgit_exception .
    METHODS delete_longtexts
      IMPORTING
        !iv_longtext_id TYPE dokil-id
      RAISING
        zcx_abapgit_exception .
    METHODS is_active
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS delete_ddic
      IMPORTING
        !iv_objtype              TYPE string
        !iv_no_ask               TYPE abap_bool DEFAULT abap_true
        !iv_no_ask_delete_append TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    METHODS get_abap_language_version
      RETURNING
        VALUE(rv_abap_language_version) TYPE uccheck
      RAISING
        zcx_abapgit_exception .
    METHODS set_abap_language_version
      CHANGING
        !cv_abap_language_version TYPE uccheck
      RAISING
        zcx_abapgit_exception .
    METHODS clear_abap_language_version
      CHANGING
        !cv_abap_language_version TYPE uccheck
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_objects_super IMPLEMENTATION.


  METHOD clear_abap_language_version.

    " Used during serializing of objects
    IF ms_item-abap_language_version = zcl_abapgit_abap_language_vers=>c_no_abap_language_version.
      " Ignore ABAP language version
      CLEAR cv_abap_language_version.
    ELSEIF ms_item-abap_language_version <> zcl_abapgit_abap_language_vers=>c_any_abap_language_version.
      " Check if ABAP language version matches repository setting
      zcl_abapgit_abap_language_vers=>check_abap_language_version(
        iv_abap_language_version = cv_abap_language_version
        is_item                  = ms_item ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.

    IF io_files IS NOT INITIAL.
      mo_files = io_files.
    ELSE.
      mo_files = zcl_abapgit_objects_files=>new( is_item ). " New file collection
    ENDIF.

    IF io_i18n_params IS NOT INITIAL.
      mo_i18n_params = io_i18n_params.
    ELSE.
      mo_i18n_params = zcl_abapgit_i18n_params=>new( ). " All defaults
    ENDIF.

  ENDMETHOD.


  METHOD corr_insert.

    DATA: lv_object       TYPE trobj_name,
          lv_object_class TYPE tadir-object.

    IF ig_object_class IS NOT INITIAL.
      lv_object_class = ig_object_class.
      IF ig_object_class = 'DICT'.
        CONCATENATE ms_item-obj_type ms_item-obj_name INTO lv_object.
      ELSE.
        lv_object = ms_item-obj_name.
      ENDIF.
    ELSE.
      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.
    ENDIF.

    zcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = lv_object_class
      iv_obj_name = lv_object
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.


  METHOD delete_ddic.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_objtype TYPE rsedd0-ddobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = iv_objtype.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
            no_ask_delete_append = iv_no_ask_delete_append
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6 ##FM_SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
        TRY.
            " try to force deletion for APPENDs
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = lv_objname
                objtype              = lv_objtype
                aie_force_deletion   = iv_no_ask_delete_append
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6 ##FM_SUBRC_OK.
          CATCH cx_sy_dyn_call_param_not_found.
            " no_ask_delete_append and aie_force_deletion not available in lower releases
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = lv_objname
                objtype              = lv_objtype
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6 ##FM_SUBRC_OK.
        ENDTRY.
    ENDTRY.

    IF sy-subrc = 5.
      zcx_abapgit_exception=>raise( |Object { ms_item-obj_type } { ms_item-obj_name
                                    } has dependencies and must be deleted manually| ).
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_longtexts.

    zcl_abapgit_factory=>get_longtexts( )->delete(
      iv_longtext_id = iv_longtext_id
      iv_object_name = ms_item-obj_name ).

  ENDMETHOD.


  METHOD deserialize_longtexts.

    zcl_abapgit_factory=>get_longtexts( )->deserialize(
      ii_xml           = ii_xml
      iv_longtext_name = iv_longtext_name
      iv_object_name   = ms_item-obj_name
      iv_longtext_id   = iv_longtext_id
      iv_main_language = mv_language ).

  ENDMETHOD.


  METHOD exists_a_lock_entry_for.

    DATA: lt_lock_entries TYPE STANDARD TABLE OF seqg3.
    DATA: lv_argument TYPE seqg3-garg.

    IF iv_prefix IS INITIAL.
      lv_argument = iv_argument.
    ELSE.
      lv_argument = |{ iv_prefix  }{ iv_argument }|.
      OVERLAY lv_argument WITH '                                          '.
      lv_argument = lv_argument && '*'.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname                = '*'
        garg                  = lv_argument
      TABLES
        enq                   = lt_lock_entries
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_lock_entries TRANSPORTING NO FIELDS
                               WITH KEY gobj = iv_lock_object.
    IF sy-subrc = 0.
      rv_exists_a_lock_entry = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_abap_language_version.

    " This is limited to DDIC objects
    TRY.
        CALL METHOD ('CL_DD_ABAP_LANGUAGE_VERSION')=>get_abap_language_version
          EXPORTING
            iv_object_type           = ms_item-obj_type
            iv_object_name           = ms_item-obj_name
          RECEIVING
            rv_abap_language_version = rv_abap_language_version.
      CATCH cx_root.
        " does not exist in lower releases
        RETURN.
    ENDTRY.

    clear_abap_language_version( CHANGING cv_abap_language_version = rv_abap_language_version ).

  ENDMETHOD.


  METHOD get_accessed_files.
    rt_files = mo_files->get_accessed_files( ).
  ENDMETHOD.


  METHOD get_metadata.

    DATA: lv_class TYPE string.

    lv_class = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).

    REPLACE FIRST OCCURRENCE OF 'ZCL_ABAPGIT' IN lv_class WITH 'LCL'.

    rs_metadata-class = lv_class.
    rs_metadata-version = 'v1.0.0'.

  ENDMETHOD.


  METHOD is_active.

    rv_active = zcl_abapgit_objects_activation=>is_active( ms_item ).

  ENDMETHOD.


  METHOD serialize_longtexts.

    zcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name   = ms_item-obj_name
      iv_longtext_name = iv_longtext_name
      iv_longtext_id   = iv_longtext_id
      it_dokil         = it_dokil
      io_i18n_params   = mo_i18n_params
      ii_xml           = ii_xml ).

  ENDMETHOD.


  METHOD set_abap_language_version.

    " Used during deserializing of objects
    IF ms_item-abap_language_version = zcl_abapgit_abap_language_vers=>c_no_abap_language_version.
      " ABAP language version is derived from object type and target package (see zcl_abapgit_objects->deserialize)
      cv_abap_language_version = ms_item-abap_language_version.
    ELSEIF ms_item-abap_language_version <> zcl_abapgit_abap_language_vers=>c_any_abap_language_version.
      " Check if ABAP language version matches repository setting
      zcl_abapgit_abap_language_vers=>check_abap_language_version(
        iv_abap_language_version = cv_abap_language_version
        is_item                  = ms_item ).
    ENDIF.

  ENDMETHOD.


  METHOD set_default_package.

    " In certain cases we need to set the package via ABAP memory
    " because we can't supply it via the APIs.
    "
    " Set default package, see function module RS_CORR_INSERT FORM get_current_devclass.
    "
    " We use ABAP memory instead the SET parameter because it is
    " more reliable. SET parameter doesn't work when multiple objects
    " are deserialized which uses the ABAP memory mechanism.
    " We don't need to reset the memory as it is done in above mentioned form routine.

    EXPORT current_devclass FROM iv_package TO MEMORY ID 'EUK'.

  ENDMETHOD.


  METHOD set_default_transport.

    " In certain cases we need to set the transport via ABAP memory
    " because we can't supply it via the APIs.
    "
    " See function module RS_CORR_INSERT

    EXPORT tasknr FROM iv_transport TO MEMORY ID 'EUT'.

  ENDMETHOD.


  METHOD tadir_delete.

    zcl_abapgit_factory=>get_tadir( )->delete_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).

  ENDMETHOD.


  METHOD tadir_insert.

    zcl_abapgit_factory=>get_tadir( )->insert_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.
ENDCLASS.
