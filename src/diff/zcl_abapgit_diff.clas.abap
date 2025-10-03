CLASS zcl_abapgit_diff DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_diff_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_diff.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_starting_beacon TYPE i VALUE 1.

    TYPES:
      ty_regexset_tt TYPE STANDARD TABLE OF REF TO cl_abap_regex WITH KEY table_line.

    DATA mt_beacons TYPE zif_abapgit_definitions=>ty_string_tt.
    DATA mt_diff TYPE zif_abapgit_definitions=>ty_diffs_tt.
    DATA ms_stats TYPE zif_abapgit_definitions=>ty_count.

    METHODS map_beacons.
    METHODS shortlist.
    METHODS create_regex_set
      RETURNING
        VALUE(rt_regex_set) TYPE ty_regexset_tt.

    METHODS calculate_stats.

ENDCLASS.



CLASS zcl_abapgit_diff IMPLEMENTATION.

  METHOD calculate_stats.

    FIELD-SYMBOLS <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.
      CASE <ls_diff>-result.
        WHEN zif_abapgit_definitions=>c_diff-insert.
          ms_stats-insert = ms_stats-insert + 1.
        WHEN zif_abapgit_definitions=>c_diff-delete.
          ms_stats-delete = ms_stats-delete + 1.
        WHEN zif_abapgit_definitions=>c_diff-update.
          ms_stats-update = ms_stats-update + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_abapgit_diff~create.

    IF zcl_abapgit_factory=>get_function_module( )->function_exists( 'RS_CMP_COMPUTE_DELTA' ) = abap_true.
      mt_diff = zcl_abapgit_diff_std=>compute(
        iv_new                = iv_new
        iv_old                = iv_old
        iv_ignore_indentation = iv_ignore_indentation
        iv_ignore_comments    = iv_ignore_comments
        iv_ignore_case        = iv_ignore_case ).
    ELSE.
      mt_diff = zcl_abapgit_diff_diff3=>compute(
        iv_new                = iv_new
        iv_old                = iv_old ).
        " iv_ignore_indentation = iv_ignore_indentation
        " iv_ignore_comments    = iv_ignore_comments
        " iv_ignore_case        = iv_ignore_case
    ENDIF.

    calculate_stats( ).
    map_beacons( ).
    shortlist( ).

    ri_diff = me.

  ENDMETHOD.


  METHOD create_regex_set.

    DATA: lo_regex TYPE REF TO cl_abap_regex,
          lt_regex TYPE zif_abapgit_definitions=>ty_string_tt,
          lv_regex LIKE LINE OF lt_regex.

    APPEND '^\s*(CLASS|FORM|MODULE|REPORT|METHOD|INTERFACE|FUNCTION)\s[^=]' TO lt_regex.
    APPEND '^\s*(PUBLIC|PROTECTED|PRIVATE)\sSECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*(CLASS|INTERFACE|FUNCTION|TYPE)-POOL\s' TO lt_regex.
    APPEND '^\s*(START|END)-OF-SELECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*INITIALIZATION(\s|\.)' TO lt_regex.
    APPEND '^\s*(TOP-OF-PAGE|END-OF-PAGE)(\s|\.)' TO lt_regex.
    APPEND '^\s*AT\s*(SELECTION-SCREEN|LINE-SELECTION|USER-COMMAND|PF\d+)(\s|\.)' TO lt_regex.
    APPEND '^\s*(DEFINE|ENHANCEMENT)\s' TO lt_regex.

    LOOP AT lt_regex INTO lv_regex.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern     = lv_regex
          ignore_case = abap_true.
      APPEND lo_regex TO rt_regex_set.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff~get.
    rt_diff = mt_diff.
  ENDMETHOD.


  METHOD zif_abapgit_diff~get_beacons.
    rt_beacons = mt_beacons.
  ENDMETHOD.


  METHOD zif_abapgit_diff~is_line_patched.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff INDEX iv_index
                       ASSIGNING <ls_diff>.
    IF sy-subrc = 0.
      rv_patched = <ls_diff>-patch_flag.
    ELSE.
      zcx_abapgit_exception=>raise( |Diff line not found { iv_index }| ).
    ENDIF.

  ENDMETHOD.


  METHOD map_beacons.

    DATA: lv_beacon_idx  TYPE i VALUE c_starting_beacon,
          lv_offs        TYPE i,
          lv_beacon_str  TYPE string,
          lv_beacon_2lev TYPE string,
          lv_submatch    TYPE string,
          lo_regex       TYPE REF TO cl_abap_regex,
          lt_regex       TYPE ty_regexset_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    lt_regex = create_regex_set( ).
    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CLEAR lv_offs.
      <ls_diff>-beacon = lv_beacon_idx.

      LOOP AT lt_regex INTO lo_regex.
        FIND FIRST OCCURRENCE OF REGEX lo_regex IN <ls_diff>-new SUBMATCHES lv_submatch.
        IF sy-subrc = 0. " Match
          lv_beacon_str = <ls_diff>-new.
          lv_submatch = to_upper( lv_submatch ).

          " Get rid of comments and end of line
          FIND FIRST OCCURRENCE OF '.' IN lv_beacon_str MATCH OFFSET lv_offs.
          IF sy-subrc <> 0.
            FIND FIRST OCCURRENCE OF '"' IN lv_beacon_str MATCH OFFSET lv_offs.
          ENDIF.

          IF lv_offs > 0.
            lv_beacon_str = lv_beacon_str(lv_offs).
          ENDIF.
          lv_beacon_str = condense( val = lv_beacon_str
                                    del = ` ` ).

          IF lv_submatch = 'CLASS'.
            lv_beacon_2lev = replace( val   = lv_beacon_str
                                      regex = '\s+(DEFINITION|IMPLEMENTATION)'
                                      with  = ''
                                      occ   = 0 ).
          ELSEIF lv_submatch = 'METHOD'.
            lv_beacon_str = lv_beacon_2lev && ` => ` && lv_beacon_str.
          ELSEIF lv_submatch = 'PUBLIC' OR lv_submatch = 'PROTECTED' OR lv_submatch = 'PRIVATE'.
            lv_beacon_str = lv_beacon_2lev && ` ` && lv_beacon_str.
          ENDIF.

          APPEND lv_beacon_str TO mt_beacons.
          lv_beacon_idx    = sy-tabix.
          <ls_diff>-beacon = lv_beacon_idx.
          EXIT. "Loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff~set_patch_by_old_diff.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>
                    USING KEY new_num
                    WHERE old     = is_diff_old-old
                      AND new     = is_diff_old-new
                      AND new_num = is_diff_old-new_num
                      AND old_num = is_diff_old-old_num.

      <ls_diff>-patch_flag = iv_patch_flag.
      EXIT.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff~set_patch_new.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY new_num
                       COMPONENTS new_num = iv_line_new
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid new line number { iv_line_new }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.


  METHOD zif_abapgit_diff~set_patch_old.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY old_num
                       COMPONENTS old_num = iv_line_old
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid old line number { iv_line_old }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.


  METHOD shortlist.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    IF lines( mt_diff ) < 20.
      LOOP AT mt_diff ASSIGNING <ls_diff>.
        <ls_diff>-short = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT mt_diff TRANSPORTING NO FIELDS
          WHERE NOT result IS INITIAL AND short = abap_false.
        lv_index = sy-tabix.

        DO 8 TIMES. " Backward
          READ TABLE mt_diff INDEX ( lv_index - sy-index ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0 OR <ls_diff>-short = abap_true. " tab bound or prev marker
            EXIT.
          ENDIF.
          <ls_diff>-short = abap_true.
        ENDDO.

        DO 8 TIMES. " Forward
          READ TABLE mt_diff INDEX ( lv_index + sy-index - 1 ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0. " tab bound reached
            EXIT.
          ENDIF.
          CHECK <ls_diff>-short = abap_false. " skip marked
          <ls_diff>-short = abap_true.
        ENDDO.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_diff~stats.
    rs_count = ms_stats.
  ENDMETHOD.

ENDCLASS.
