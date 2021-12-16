CLASS zcl_abapgit_repo_pre_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_pre_filter.
    ALIASES: filter_files FOR zif_abapgit_repo_pre_filter~filter_files,
             get_local_filter FOR zif_abapgit_repo_pre_filter~get_local_filter,
             set_filter_values_via_dialog FOR zif_abapgit_repo_pre_filter~set_filter_values_via_dialog,
             set_filter_values FOR zif_abapgit_repo_pre_filter~set_filter_values,
             get_filter_values FOR zif_abapgit_repo_pre_filter~get_filter_values.
    TYPES:
      ty_file_filter_tt TYPE RANGE OF string .
    TYPES:
      ty_file_filter TYPE LINE OF ty_file_filter_tt .

    TYPES:
      BEGIN OF ty_e071_filter,
        pgmid    TYPE pgmid,
        object   TYPE  trobjtype,
        obj_name TYPE trobj_name,
      END OF ty_e071_filter.
    TYPES ty_e071_filter_tt TYPE STANDARD TABLE OF ty_e071_filter.

  PROTECTED SECTION.


  PRIVATE SECTION.

    DATA mt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt .
    DATA mt_r_trkorr TYPE trrngtrkor_tab .
    DATA mt_r_file_filter TYPE ty_file_filter_tt .

    METHODS generate_local_filter
      IMPORTING
        it_r_trkorr      TYPE trrngtrkor_tab
      RETURNING
        VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .

    METHODS generate_file_filter
      IMPORTING
        it_filter               TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_r_file_filter) TYPE ty_file_filter_tt
      RAISING
        zcx_abapgit_exception .

    METHODS init .

    METHODS adjust_local_filter
      IMPORTING
                it_e071_filter   TYPE ty_e071_filter_tt
      RETURNING VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
                zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_repo_pre_filter IMPLEMENTATION.

  METHOD zif_abapgit_repo_pre_filter~filter_files.
    IF mt_r_file_filter IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ct_files WHERE filename NOT IN mt_r_file_filter.
  ENDMETHOD.

  METHOD generate_file_filter.
    DATA lr_filter TYPE REF TO zif_abapgit_definitions=>ty_tadir.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_pattern TYPE string.
    DATA ls_r_file_filter TYPE ty_file_filter.

    CLEAR rt_r_file_filter.

    IF it_filter IS INITIAL.
      RETURN.
    ENDIF.

    ls_r_file_filter-sign = 'I'.
    ls_r_file_filter-option = 'EQ'.
    ls_r_file_filter-low = zif_abapgit_definitions=>c_dot_abapgit.
    INSERT ls_r_file_filter INTO TABLE rt_r_file_filter.

    LOOP AT it_filter REFERENCE INTO lr_filter.
      CLEAR ls_item.
      CLEAR ls_r_file_filter.
      ls_item-obj_type = lr_filter->object.
      ls_item-obj_name = lr_filter->obj_name.

      lv_pattern = zcl_abapgit_filename_logic=>object_to_file(
        is_item  = ls_item
        iv_ext   = '' ).
      CONCATENATE lv_pattern '*' INTO lv_pattern.
      " Escape special characters for use with 'covers pattern' (CP)
      REPLACE ALL OCCURRENCES OF '#' IN lv_pattern WITH '##'.
      REPLACE ALL OCCURRENCES OF '+' IN lv_pattern WITH '#+'.

      ls_r_file_filter-sign = 'I'.
      ls_r_file_filter-option = 'CP'.
      ls_r_file_filter-low = lv_pattern.
      INSERT ls_r_file_filter INTO TABLE rt_r_file_filter.
    ENDLOOP.

  ENDMETHOD.


  METHOD generate_local_filter.
    DATA lt_e071_filter TYPE ty_e071_filter_tt.

    SELECT DISTINCT pgmid
                object
                obj_name
           INTO CORRESPONDING FIELDS OF TABLE lt_e071_filter
           FROM e071
     WHERE trkorr IN it_r_trkorr.

    rt_filter = adjust_local_filter( lt_e071_filter ).
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~get_filter_values.
    rt_r_trkorr = mt_r_trkorr.
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~get_local_filter.
    rt_filter = mt_filter.
  ENDMETHOD.

  METHOD init.
    CLEAR mt_filter.
    CLEAR mt_r_file_filter.
    CLEAR mt_r_trkorr.
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~set_filter_values.

    init( ).
    IF it_r_trkorr IS NOT INITIAL.
      mt_filter = generate_local_filter( it_r_trkorr ).
      mt_r_file_filter = generate_file_filter( mt_filter ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_repo_pre_filter~set_filter_values_via_dialog.

    DATA ls_selection  TYPE trwbo_selection.
    DATA lt_r_trkorr  TYPE trrngtrkor_tab.
    DATA ls_r_trkorr  TYPE trrngtrkor.
    DATA lr_request TYPE REF TO trwbo_request_header.
    DATA lt_request TYPE trwbo_request_headers.

    ls_selection-trkorrpattern = space.
    ls_selection-connect_req_task_conditions = 'X'.
    ls_selection-reqfunctions = 'KTRXS'.
    ls_selection-reqstatus = 'RNODL'.
    ls_selection-taskstatus = 'RNODL'.
    CONDENSE ls_selection-reqfunctions NO-GAPS.
    ls_selection-taskfunctions = 'QRSX'.
    CONCATENATE sy-sysid '*' INTO ls_selection-trkorrpattern.

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = '*'
        is_selection           = ls_selection
        iv_complete_projects   = abap_false
        iv_via_selscreen       = 'X'
*       IS_POPUP               =
        iv_title               = 'Select Transports / Tasks'(001)
      IMPORTING
        et_requests            = lt_request
* CHANGING
*       CS_RANGES              =
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Pre-Filter Canceled'(002) ).
    ENDIF.

    IF lt_request IS INITIAL.
      zcx_abapgit_exception=>raise( 'No Request Found'(003) ).
    ENDIF.

    LOOP AT lt_request REFERENCE INTO lr_request.
      ls_r_trkorr-sign = 'I'.
      ls_r_trkorr-option = 'EQ'.
      ls_r_trkorr-low = lr_request->trkorr.
      INSERT ls_r_trkorr INTO TABLE lt_r_trkorr.
    ENDLOOP.

    set_filter_values( lt_r_trkorr[] ).
  ENDMETHOD.


  METHOD adjust_local_filter.

    DATA lt_e071_filter TYPE ty_e071_filter_tt.
    data lr_e071_filter type ref to ty_e071_filter.
    data ls_e071_filter type ty_e071_filter.
    DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lv_trobj_name_new TYPE trobj_name.
    DATA lv_trobj_type_new TYPE trobjtype.

    lt_e071_filter = it_e071_filter.

    LOOP AT lt_e071_filter REFERENCE INTO lr_e071_filter.
      IF lr_e071_filter->pgmid <> 'LIMU' AND lr_e071_filter->pgmid <> 'R3TR'.

        "I don't know how to determine the R3TR Object for other PGMID (like LANG)
        "Workaround: I add also filter R3TR and try also with LIMU to get the R3TR Object
        ls_filter-object = lr_e071_filter->object.
        ls_filter-obj_name = lr_e071_filter->obj_name.
        ls_filter-pgmid = 'R3TR'.
        INSERT ls_filter INTO TABLE rt_filter.
        ls_e071_filter = lr_e071_filter->*.
        "Try with LIMU to get R3TR Object
        ls_e071_filter-pgmid = 'LIMU'.
        INSERT ls_e071_filter INTO TABLE lt_e071_filter.
      ENDIF.

      IF lr_e071_filter->pgmid = 'LIMU'.
        "Get Main Object from LIMU Object (Example the Class (R3TR) of a Method (LIMU))
        "Could also work for example for LANGU

        CLEAR lv_trobj_type_new.
        CLEAR lv_trobj_name_new.

        CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
          EXPORTING
            p_limu_objtype = lr_e071_filter->object
            p_limu_objname = lr_e071_filter->OBJ_NAME
          IMPORTING
            p_r3tr_objtype = lv_trobj_type_new
            p_r3tr_objname = lv_trobj_name_new
          EXCEPTIONS
            no_mapping     = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF lv_trobj_type_new IS INITIAL.
          CONTINUE.
        ENDIF.

        clear ls_filter.
        ls_filter-pgmid = 'R3TR'.
        ls_filter-object = lv_trobj_type_new.
        ls_filter-obj_name = lv_trobj_name_new.
        INSERT ls_filter INTO TABLE rt_filter.
      else.
        ls_filter-pgmid = lr_e071_filter->pgmid.
        ls_filter-object = lr_e071_filter->object.
        ls_filter-obj_name = lr_e071_filter->obj_name.
        INSERT ls_filter INTO TABLE rt_filter.
      ENDIF.

    ENDLOOP.

    SORT rt_filter.
    DELETE ADJACENT DUPLICATES FROM rt_filter.

    IF rt_filter IS INITIAL.

      zcx_abapgit_exception=>raise( 'No objects found for transport filter'(004) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
