CLASS zcl_abapgit_cloud DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_diff.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_diff) TYPE REF TO zif_abapgit_diff.

  PRIVATE SECTION.
    DATA mo_diff3 TYPE REF TO zif_abapgit_diff3.
    DATA mt_diff  TYPE zif_abapgit_definitions=>ty_diffs_tt.
    DATA ms_stats TYPE zif_abapgit_definitions=>ty_count.

    METHODS build_diff
      IMPORTING
        iv_new                TYPE xstring
        iv_old                TYPE xstring
        iv_ignore_indentation TYPE abap_bool
        iv_ignore_comments    TYPE abap_bool
        iv_ignore_case        TYPE abap_bool.
ENDCLASS.

CLASS zcl_abapgit_cloud IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ri_diff TYPE zcl_abapgit_cloud.
  ENDMETHOD.

  METHOD zif_abapgit_diff~create.
    DATA lo_obj TYPE REF TO zcl_abapgit_cloud.
    CREATE OBJECT lo_obj.
    lo_obj->build_diff( iv_new                = iv_new
                        iv_old                = iv_old
                        iv_ignore_indentation = iv_ignore_indentation
                        iv_ignore_comments    = iv_ignore_comments
                        iv_ignore_case        = iv_ignore_case ).
    ri_diff = lo_obj.
  ENDMETHOD.

  METHOD build_diff.
    " Simple placeholder implementation creating a naive line based diff.
    DATA lv_new_string TYPE string.
    DATA lv_old_string TYPE string.
    DATA lt_new        TYPE string_table.
    DATA lt_old        TYPE string_table.
    DATA lv_index      TYPE i.
    DATA ls_diff       TYPE zif_abapgit_definitions=>ty_diff.
    DATA lv_new_line   TYPE string.
    DATA lv_old_line   TYPE string.
    DATA lv_max        TYPE i.

    IF iv_new IS NOT INITIAL.
      lv_new_string = cl_abap_conv_in_ce=>create(
        input    = iv_new
        encoding = 'UTF-8' )->read( ).
    ENDIF.
    IF iv_old IS NOT INITIAL.
      lv_old_string = cl_abap_conv_in_ce=>create(
        input    = iv_old
        encoding = 'UTF-8' )->read( ).
    ENDIF.

    SPLIT lv_new_string AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_new.
    IF lines( lt_new ) = 0 AND lv_new_string IS NOT INITIAL.
      SPLIT lv_new_string AT cl_abap_char_utilities=>newline INTO TABLE lt_new.
    ENDIF.
    SPLIT lv_old_string AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_old.
    IF lines( lt_old ) = 0 AND lv_old_string IS NOT INITIAL.
      SPLIT lv_old_string AT cl_abap_char_utilities=>newline INTO TABLE lt_old.
    ENDIF.

    CLEAR mt_diff.
    CLEAR ms_stats.

    IF lines( lt_new ) > lines( lt_old ).
      lv_max = lines( lt_new ).
    ELSE.
      lv_max = lines( lt_old ).
    ENDIF.

    lv_index = 0.
    WHILE lv_index < lv_max.
      lv_index = lv_index + 1.

      CLEAR: lv_new_line, lv_old_line.
      READ TABLE lt_new INDEX lv_index INTO lv_new_line.
      READ TABLE lt_old INDEX lv_index INTO lv_old_line.

      IF lv_new_line IS INITIAL AND lv_old_line IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR ls_diff.
      ls_diff-new_num = lv_index.
      ls_diff-old_num = lv_index.

      IF lv_new_line = lv_old_line.
        ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
        ls_diff-new = lv_new_line.
        ls_diff-old = lv_old_line.
      ELSEIF lv_old_line IS INITIAL AND lv_new_line IS NOT INITIAL.
        ls_diff-result = zif_abapgit_definitions=>c_diff-insert.
        ls_diff-new = lv_new_line.
        ms_stats-insert = ms_stats-insert + 1.
      ELSEIF lv_new_line IS INITIAL AND lv_old_line IS NOT INITIAL.
        ls_diff-result = zif_abapgit_definitions=>c_diff-delete.
        ls_diff-old = lv_old_line.
        ms_stats-delete = ms_stats-delete + 1.
      ELSE.
        ls_diff-result = zif_abapgit_definitions=>c_diff-update.
        ls_diff-new = lv_new_line.
        ls_diff-old = lv_old_line.
        ms_stats-update = ms_stats-update + 1.
      ENDIF.
      APPEND ls_diff TO mt_diff.
    ENDWHILE.

    mo_diff3 = zcl_abapgit_diff3=>create( ).

    " suppress unused parameter warnings for now (planned future use)
    IF iv_ignore_indentation = abap_true OR
       iv_ignore_comments    = abap_true OR
       iv_ignore_case        = abap_true.
      " no-op
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_diff~get.
    rt_diff = mt_diff.
  ENDMETHOD.

  METHOD zif_abapgit_diff~stats.
    rs_count = ms_stats.
  ENDMETHOD.

  METHOD zif_abapgit_diff~set_patch_new.
    FIELD-SYMBOLS <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.
    READ TABLE mt_diff INDEX iv_line_new ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.
    <ls_diff>-patch_flag = iv_patch_flag.
  ENDMETHOD.

  METHOD zif_abapgit_diff~set_patch_old.
    FIELD-SYMBOLS <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.
    READ TABLE mt_diff INDEX iv_line_old ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.
    <ls_diff>-patch_flag = iv_patch_flag.
  ENDMETHOD.

  METHOD zif_abapgit_diff~get_beacons.
    CLEAR rt_beacons.
  ENDMETHOD.

  METHOD zif_abapgit_diff~is_line_patched.
    FIELD-SYMBOLS <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.
    READ TABLE mt_diff INDEX iv_index ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.
    rv_patched = <ls_diff>-patch_flag.
  ENDMETHOD.

  METHOD zif_abapgit_diff~set_patch_by_old_diff.
    FIELD-SYMBOLS <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.
    LOOP AT mt_diff ASSIGNING <ls_diff>.
      IF <ls_diff>-old = is_diff_old-old AND <ls_diff>-new = is_diff_old-new.
        <ls_diff>-patch_flag = iv_patch_flag.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
