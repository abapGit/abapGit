CLASS zcl_abapgit_repo_pre_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      ty_file_filter_tt TYPE RANGE OF string .
    TYPES:
      ty_file_filter TYPE LINE OF ty_file_filter_tt .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter RR_FILTER | <p class="shorttext synchronized" lang="en">Repository Pre Filter</p>
    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_filter) TYPE REF TO zcl_abapgit_repo_pre_filter .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter IT_R_TRKORR | <p class="shorttext synchronized" lang="en">Tab of Range Struct for E070/E071-TRKORR</p>
    METHODS set_filter_values
      IMPORTING
        !it_r_trkorr TYPE trrngtrkor_tab
      RAISING
        zcx_abapgit_exception .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter RT_R_TRKORR | <p class="shorttext synchronized" lang="en">Tab of Range Struct for E070/E071-TRKORR</p>
    METHODS get_filter_values
      RETURNING
        VALUE(rt_r_trkorr) TYPE trrngtrkor_tab .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter RT_FILTER | <p class="shorttext synchronized" lang="en">Repository Filter</p>
    METHODS get_local_filter
      RETURNING
        VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter RT_R_FILE_FILTER | <p class="shorttext synchronized" lang="en">File Filter</p>
    METHODS get_file_filter
      RETURNING
        VALUE(rt_r_file_filter) TYPE ty_file_filter_tt .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter CT_FILES | <p class="shorttext synchronized" lang="en">Files</p>
    METHODS filter_files
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_files_tt .
    "! <p class="shorttext synchronized" lang="en"></p>
    METHODS init .
    "! <p class="shorttext synchronized" lang="en"></p>
    METHODS set_filter_values_via_dialog
      RAISING
        zcx_abapgit_exception .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter iv_action | <p class="shorttext synchronized" lang="en">Action</p>
    METHODS set_latest_action
      IMPORTING
        !iv_action TYPE string .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter rv_required | <p class="shorttext synchronized" lang="en">Required</p>
    METHODS is_filter_required
      RETURNING
        VALUE(rv_required) TYPE abap_bool .
  PROTECTED SECTION.


  PRIVATE SECTION.

    CLASS-DATA gr_filter TYPE REF TO zcl_abapgit_repo_pre_filter .
    DATA mt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt .
    DATA mt_r_trkorr TYPE trrngtrkor_tab .
    DATA mt_r_file_filter TYPE ty_file_filter_tt .
    DATA mv_latest_action TYPE string .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    METHODS generate_local_filter
      RAISING
        zcx_abapgit_exception .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    METHODS generate_file_filter
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_REPO_PRE_FILTER IMPLEMENTATION.


  METHOD filter_files.
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

    CLEAR mt_r_file_filter.

    IF mt_filter IS INITIAL.
      RETURN.
    ENDIF.

    ls_r_file_filter-sign = 'I'.
    ls_r_file_filter-option = 'EQ'.
    ls_r_file_filter-low = zif_abapgit_definitions=>c_dot_abapgit.
    INSERT ls_r_file_filter INTO TABLE mt_r_file_filter.


    LOOP AT mt_filter REFERENCE INTO lr_filter.
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
      INSERT ls_r_file_filter INTO TABLE mt_r_file_filter.
    ENDLOOP.

  ENDMETHOD.


  METHOD generate_local_filter.
    DATA lr_filter TYPE REF TO zif_abapgit_definitions=>ty_tadir.
    DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_trobj_name TYPE trobj_name.
    DATA lv_trobj_name_new TYPE trobj_name.
    DATA lv_trobj_type_new TYPE trobjtype.


    IF mt_r_trkorr IS NOT INITIAL.

      SELECT DISTINCT pgmid
                      object
                      obj_name
                 INTO CORRESPONDING FIELDS OF TABLE lt_filter
                 FROM e071
           WHERE trkorr IN mt_r_trkorr.
    ENDIF.

    LOOP AT lt_filter REFERENCE INTO lr_filter.
      IF lr_filter->pgmid <> 'LIMU' AND lr_filter->pgmid <> 'R3TR'.

        "I don't know how to determine the R3TR Object for other PGMID (like LANGU)
        "Workaround: I add also filter R3TR and try also with LIMU to get the R3TR Object
        ls_filter = lr_filter->*.
        ls_filter-pgmid = 'R3TR'.
        INSERT ls_filter INTO TABLE mt_filter.
        "Try with LIMU to get R3TR Object
        ls_filter-pgmid = 'LIMU'.
        INSERT ls_filter INTO TABLE lt_filter.
      ENDIF.

      IF lr_filter->pgmid = 'LIMU'.
        "Get Main Object from LIMU Object (Example the Class (R3TR) of a Method (LIMU))
        "Could also work for example for LANGU

        lv_trobj_name = lr_filter->obj_name.
        CLEAR lv_trobj_type_new.
        CLEAR lv_trobj_name_new.

        CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
          EXPORTING
            p_limu_objtype = lr_filter->object
            p_limu_objname = lv_trobj_name
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

        lr_filter->object = lv_trobj_type_new.
        lr_filter->obj_name = lv_trobj_name_new.

      ENDIF.
      INSERT lr_filter->* INTO TABLE mt_filter.
    ENDLOOP.

    SORT mt_filter.
    DELETE ADJACENT DUPLICATES FROM mt_filter.

    IF mt_filter IS INITIAL.

      zcx_abapgit_exception=>raise( 'No objects found for transport filter'(004) ).

    ENDIF.
  ENDMETHOD.


  METHOD get_file_filter.
    rt_r_file_filter = mt_r_file_filter.
  ENDMETHOD.


  METHOD get_filter_values.
    rt_r_trkorr = mt_r_trkorr.
  ENDMETHOD.


  METHOD get_instance.
    "Singleton
    IF gr_filter IS INITIAL.
      CREATE OBJECT gr_filter.
    ENDIF.
    rr_filter = gr_filter.
  ENDMETHOD.


  METHOD get_local_filter.

    rt_filter = mt_filter.

  ENDMETHOD.


  METHOD init.
    CLEAR mt_filter.
    CLEAR mt_r_file_filter.
    CLEAR mt_r_trkorr.
  ENDMETHOD.


  METHOD is_filter_required.
    CLEAR rv_required.
    case mv_latest_action .
    when zif_abapgit_definitions=>c_action-go_stage_transport
      or zif_abapgit_definitions=>c_action-zip_export_transport.
      rv_required = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD set_filter_values.

    init( ).
    IF it_r_trkorr IS NOT INITIAL.
      mt_r_trkorr = it_r_trkorr.
      generate_local_filter( ).
      generate_file_filter( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_filter_values_via_dialog.

    DATA: ls_selection  TYPE trwbo_selection.
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


  METHOD set_latest_action.
    "Set latest action
    mv_latest_action = iv_action.
  ENDMETHOD.
ENDCLASS.
