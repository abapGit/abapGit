CLASS zcl_abapgit_object_xinx DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_extension_index,
        dd12v   TYPE dd12v,
        t_dd17v TYPE STANDARD TABLE OF dd17v
                   WITH NON-UNIQUE DEFAULT KEY,
      END OF ty_extension_index.

    CONSTANTS:
      c_objtype_extension_index   TYPE trobjtype VALUE 'XINX'.

    DATA:
      mv_name TYPE ddobjname,
      mv_id   TYPE ddobjectid.

    METHODS:
      xinx_delete_docu
        IMPORTING
          iv_objname TYPE ddobjname
          iv_id      TYPE ddobjectid.

ENDCLASS.



CLASS zcl_abapgit_object_xinx IMPLEMENTATION.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    cl_wb_object_type=>get_key_components_from_id(
      EXPORTING
        p_key                   = |{ ms_item-obj_name }|
        p_external_id           = swbm_c_type_ddic_db_tabxinx
      IMPORTING
        p_key_component1        = mv_name
        p_key_component2        = mv_id
      EXCEPTIONS
        too_many_key_components = 1
        objecttype_not_existing = 2
        OTHERS                  = 3 ).

    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    " Reimplement FM RS_DD_INDX_DELETE as it calls the UI

    DATA: lv_enqueue      TYPE ddenqs,
          lv_protname     TYPE tstrf01-file,
          lv_del_concname LIKE lv_enqueue-objname,
          lv_concname     TYPE rsdxx-objname,
          lv_transp_key   TYPE trkey,
          ls_e071         TYPE e071,
          lv_clm_corrnum  TYPE e070-trkorr,
          lv_message      TYPE string.

    CONCATENATE mv_name '-' mv_id INTO lv_concname.
    lv_enqueue-objtype = c_objtype_extension_index.

    CALL FUNCTION 'INT_INDX_DEL_LOCK'
      EXPORTING
        i_trobjtype        = lv_enqueue-objtype
        i_tabname          = mv_name
        i_indexname        = mv_id
      EXCEPTIONS
        not_executed       = 1
        error_occured      = 2
        permission_failure = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_enqueue-objname = mv_name.
    lv_enqueue-secname = mv_id.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object        = lv_enqueue
        object_class  = 'DICT'
        mode          = 'DELETE'
      IMPORTING
        transport_key = lv_transp_key
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      " & was not deleted (correction entry not possible or canceled)
      MESSAGE s015(e2) WITH lv_concname INTO lv_message.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DD_LOGNPROT_NAME_GET'
      EXPORTING
        task        = 'DEL'
        obj_type    = lv_enqueue-objtype
        obj_name    = lv_enqueue-objname
        ind_name    = lv_enqueue-secname
      IMPORTING
        protname    = lv_protname
      EXCEPTIONS
        input_error = 0.

    PERFORM logdelete IN PROGRAM rddu0001 USING lv_protname.

    lv_del_concname = lv_enqueue-objname.
    lv_del_concname+16 = lv_enqueue-secname.
    CALL FUNCTION 'DD_OBJ_DEL'
      EXPORTING
        object_name = lv_del_concname
        object_type = lv_enqueue-objtype
        del_state   = 'M'
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DD_DD_TO_E071'
      EXPORTING
        type          = lv_enqueue-objtype
        name          = lv_enqueue-objname
        id            = lv_enqueue-secname
      IMPORTING
        obj_name      = ls_e071-obj_name
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      " Internal error & in & (contact person in charge)
      MESSAGE i008(e2) WITH 'DD_DD_TO_E071' 'RS_DD_INDX_DELETE' INTO lv_message.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_e071-object = lv_enqueue-objtype.
    CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
      EXPORTING
        object                 = ls_e071-object
        obj_name               = ls_e071-obj_name
        immediate              = 'X'
        actualize_working_area = 'X'.

    xinx_delete_docu(
        iv_objname = mv_name
        iv_id      = mv_id ).

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = ls_e071-obj_name
        operation = 'DELETE'
        type      = c_objtype_extension_index.

    IF mv_id(1) CA 'YZ'.
      CALL FUNCTION 'CLM_INDX_MODIFICATION_DELETE'
        EXPORTING
          idxobj_name   = lv_enqueue-objname
          idx_type      = lv_enqueue-objtype
          idx_name      = mv_id
          transport_key = lv_transp_key
          corrnum       = lv_clm_corrnum.
    ENDIF.

    CALL FUNCTION 'RS_DD_DEQUEUE'
      EXPORTING
        objtype = lv_enqueue-objtype
        objname = lv_enqueue-objname
        secname = lv_enqueue-secname.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_extension_index TYPE ty_extension_index,
          lv_rc              TYPE sy-subrc.

    io_xml->read(
      EXPORTING
        iv_name = 'XINX'
      CHANGING
        cg_data = ls_extension_index ).

    tadir_insert( iv_package ).

    CALL FUNCTION 'DDIF_INDX_PUT'
      EXPORTING
        name              = mv_name
        id                = mv_id
        dd12v_wa          = ls_extension_index-dd12v
      TABLES
        dd17v_tab         = ls_extension_index-t_dd17v
      EXCEPTIONS
        indx_not_found    = 1
        name_inconsistent = 2
        indx_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DDIF_INDX_PUT { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'DDIF_INDX_ACTIVATE'
      EXPORTING
        name        = mv_name
        id          = mv_id
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DDIF_INDX_ACTIVATE { sy-subrc }| ).
    ENDIF.

    IF lv_rc <> 0.
      zcx_abapgit_exception=>raise( |Cannot activate extension index { mv_id } of table { mv_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_dd12v TYPE dd12v.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
      IMPORTING
        dd12v_wa      = lv_dd12v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    rv_bool = boolc( lv_dd12v IS NOT INITIAL ).

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

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from RS_TOOL_ACCESS { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_extension_index TYPE ty_extension_index.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
        langu         = sy-langu
      IMPORTING
        dd12v_wa      = ls_extension_index-dd12v
      TABLES
        dd17v_tab     = ls_extension_index-t_dd17v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from DDIF_INDX_GET { sy-subrc }| ).
    ENDIF.

    CLEAR: ls_extension_index-dd12v-as4user,
           ls_extension_index-dd12v-as4date,
           ls_extension_index-dd12v-as4time.

    io_xml->add( iv_name = 'XINX'
                 ig_data = ls_extension_index ).

  ENDMETHOD.

  METHOD xinx_delete_docu.

    DATA: lv_docuid  TYPE dokhl-id,
          lv_doctype TYPE dokhl-typ,
          lv_docname TYPE dokhl-object.

    lv_docname    = iv_objname.
    lv_docname+30 = iv_id.
    CALL FUNCTION 'INTERN_DD_DOCU_ID_MATCH'
      EXPORTING
        p_trobjtype  = c_objtype_extension_index
      IMPORTING
        p_docu_id    = lv_docuid
        p_doctype    = lv_doctype
      EXCEPTIONS
        illegal_type = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id            = lv_docuid
        doku_object        = lv_docname
        doku_typ           = lv_doctype
        suppress_authority = 'X'
        suppress_enqueue   = 'X'
        suppress_transport = 'X'
      EXCEPTIONS
        no_docu_found      = 1
        OTHERS             = 2.

  ENDMETHOD.

ENDCLASS.
