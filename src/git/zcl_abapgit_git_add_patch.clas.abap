CLASS zcl_abapgit_git_add_patch DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          it_diff TYPE zif_abapgit_definitions=>ty_diffs_tt,

      get_patch
        RETURNING
          VALUE(rt_patch) TYPE string_table
        RAISING
          zcx_abapgit_exception,

      get_patch_binary
        RETURNING
          VALUE(rv_patch_binary) TYPE xstring
        RAISING
          zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mt_diff  TYPE zif_abapgit_definitions=>ty_diffs_tt,
      mt_patch TYPE string_table.

    METHODS:
      calculate_patch
        RETURNING
          VALUE(rt_patch) TYPE string_table
        RAISING
          zcx_abapgit_exception,

      without_marker
        IMPORTING
          iv_line        TYPE string
        RETURNING
          VALUE(rv_line) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_GIT_ADD_PATCH IMPLEMENTATION.


  METHOD calculate_patch.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CASE <ls_diff>-result.
        WHEN zif_abapgit_definitions=>c_diff-unchanged.

          INSERT without_marker( <ls_diff>-old ) INTO TABLE rt_patch.

        WHEN zif_abapgit_definitions=>c_diff-insert.

          IF <ls_diff>-patch_flag = abap_true.
            INSERT without_marker( <ls_diff>-new ) INTO TABLE rt_patch.
          ENDIF.

        WHEN zif_abapgit_definitions=>c_diff-delete.

          IF <ls_diff>-patch_flag = abap_false.
            INSERT without_marker( <ls_diff>-old ) INTO TABLE rt_patch.
          ENDIF.

        WHEN zif_abapgit_definitions=>c_diff-update.

          IF <ls_diff>-patch_flag = abap_true.
            INSERT without_marker( <ls_diff>-new ) INTO TABLE rt_patch.
          ELSE.
            INSERT without_marker( <ls_diff>-old ) INTO TABLE rt_patch.
          ENDIF.

        WHEN OTHERS.

          zcx_abapgit_exception=>raise( |Unknown result| ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD without_marker.

    " zcl_abapgit_diff_std appends a form feed to a last line without newline
    " when only the other side has one. It marks the diff, not the file
    DATA lv_length TYPE i.

    rv_line = iv_line.
    lv_length = strlen( rv_line ) - 1.
    IF lv_length >= 0 AND rv_line+lv_length(1) = cl_abap_char_utilities=>form_feed.
      rv_line = rv_line(lv_length).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mt_diff = it_diff.

  ENDMETHOD.


  METHOD get_patch.

    IF mt_patch IS INITIAL.
      mt_patch = calculate_patch( ).
    ENDIF.

    rt_patch = mt_patch.

  ENDMETHOD.


  METHOD get_patch_binary.

    DATA: lv_string TYPE string.

    IF mt_patch IS INITIAL.
      mt_patch = calculate_patch( ).
    ENDIF.

    CONCATENATE LINES OF mt_patch INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.
    lv_string = lv_string && cl_abap_char_utilities=>newline.

    rv_patch_binary = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
ENDCLASS.
