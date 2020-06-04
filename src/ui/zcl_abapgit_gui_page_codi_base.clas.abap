CLASS zcl_abapgit_gui_page_codi_base DEFINITION PUBLIC ABSTRACT INHERITING FROM zcl_abapgit_gui_page.
  PUBLIC SECTION.
    METHODS:
      zif_abapgit_gui_event_handler~on_event
        REDEFINITION.

  PROTECTED SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA mt_result TYPE scit_alvlist .

    METHODS render_result
      IMPORTING
        !io_html   TYPE REF TO zcl_abapgit_html
        !it_result TYPE scit_alvlist .
    METHODS render_result_line
      IMPORTING
        !io_html   TYPE REF TO zcl_abapgit_html
        !is_result TYPE scir_alvlist .
    METHODS build_nav_link
      IMPORTING
        !is_result TYPE scir_alvlist
      RETURNING
        VALUE(rv_link) TYPE string.
    METHODS jump
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item    TYPE zif_abapgit_definitions=>ty_item
        !iv_line_number TYPE i
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    CONSTANTS c_object_separator TYPE char1 VALUE '|'.
    CONSTANTS c_ci_sig TYPE string VALUE 'cinav:'.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_CODI_BASE IMPLEMENTATION.


  METHOD build_nav_link.

    rv_link = |{ c_ci_sig }| &&
      |{ is_result-objtype }{ is_result-objname }| &&
      |{ c_object_separator }{ is_result-sobjtype }{ is_result-sobjname }| &&
      |{ c_object_separator }{ is_result-line }|.

  ENDMETHOD.


  METHOD jump.

    DATA: lo_test               TYPE REF TO cl_ci_test_root,
          ls_info               TYPE scir_rest,
          lo_result             TYPE REF TO cl_ci_result_root,
          lv_adt_jump_enabled   TYPE abap_bool,
          lv_line_number        TYPE i,
          ls_item               TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item           TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_result> TYPE scir_alvlist.


    IF is_sub_item IS NOT INITIAL.
      READ TABLE mt_result WITH KEY objtype  = is_item-obj_type
                                    objname  = is_item-obj_name
                                    sobjtype = is_sub_item-obj_type
                                    sobjname = is_sub_item-obj_name
                                    line     = iv_line_number
                           ASSIGNING <ls_result>.
    ELSE.
      READ TABLE mt_result WITH KEY objtype = is_item-obj_type
                                    objname = is_item-obj_name
                                    line    = iv_line_number
                           ASSIGNING <ls_result>.
    ENDIF.
    ASSERT <ls_result> IS ASSIGNED.
    ls_item-obj_name = <ls_result>-objname.
    ls_item-obj_type = <ls_result>-objtype.

    ls_sub_item-obj_name = <ls_result>-sobjname.
    ls_sub_item-obj_type = <ls_result>-sobjtype.

    " see SCI_LCL_DYNP_530 / HANDLE_DOUBLE_CLICK

    lv_adt_jump_enabled = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_adt_jump_enabled( ).

    TRY.
        IF lv_adt_jump_enabled = abap_true.

          lv_line_number = <ls_result>-line.

          zcl_abapgit_objects_super=>jump_adt( iv_obj_name     = ls_item-obj_name
                                               iv_obj_type     = ls_item-obj_type
                                               iv_sub_obj_name = ls_sub_item-obj_name
                                               iv_sub_obj_type = ls_sub_item-obj_type
                                               iv_line_number  = lv_line_number ).
          RETURN.

        ENDIF.
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        CALL METHOD ('CL_CI_TESTS')=>('GET_TEST_REF')
          EXPORTING
            p_test   = <ls_result>-test
          RECEIVING
            p_result = lo_test.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Jump to object not supported in your NW release| ).
    ENDTRY.

    lo_result = lo_test->get_result_node( <ls_result>-kind ).

    MOVE-CORRESPONDING <ls_result> TO ls_info.

    lo_result->set_info( ls_info ).
    lo_result->if_ci_test~navigate( ).

  ENDMETHOD.


  METHOD render_result.

    CONSTANTS: lc_limit TYPE i VALUE 500.
    FIELD-SYMBOLS: <ls_result> TYPE scir_alvlist.

    io_html->add( '<div class="ci-result">' ).

    LOOP AT it_result ASSIGNING <ls_result> TO lc_limit.
      render_result_line(
        io_html = io_html
        is_result = <ls_result> ).
    ENDLOOP.

    io_html->add( '</div>' ).

    IF lines( it_result ) > lc_limit.
      io_html->add( '<div class="dummydiv warning">' ).
      io_html->add( zcl_abapgit_html=>icon( 'exclamation-triangle' ) ).
      io_html->add( |Only first { lc_limit } findings shown in list!| ).
      io_html->add( '</div>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_result_line.

    DATA: lv_class   TYPE string,
          lv_obj_txt TYPE string,
          lv_msg     TYPE string.

    CASE is_result-kind.
      WHEN 'E'.
        lv_class = 'ci-error'.
      WHEN 'W'.
        lv_class = 'ci-warning'.
      WHEN OTHERS.
        lv_class = 'ci-info'.
    ENDCASE.

    lv_msg = escape( val = is_result-text
                     format = cl_abap_format=>e_html_attr ).

    IF is_result-sobjname IS INITIAL OR
       ( is_result-sobjname = is_result-objname AND
         is_result-sobjtype = is_result-sobjtype ).
      lv_obj_txt = |{ is_result-objtype } { is_result-objname }|.
    ELSE.
      lv_obj_txt = |{ is_result-objtype } { is_result-objname } &gt; { is_result-sobjtype } { is_result-sobjname }|.
    ENDIF.
    lv_obj_txt = |{ lv_obj_txt } [ @{ zcl_abapgit_convert=>alpha_output( is_result-line ) } ]|.

    io_html->add( |<li class="{ lv_class }">| ).
    io_html->add_a(
      iv_txt = lv_obj_txt
      iv_act = build_nav_link( is_result )
      iv_typ = zif_abapgit_html=>c_action_type-sapevent ).
    io_html->add( |<span>{ lv_msg }</span>| ).
    io_html->add( '</li>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    DATA: ls_item          TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item      TYPE zif_abapgit_definitions=>ty_item,
          lv_temp          TYPE string,
          lv_main_object   TYPE string,
          lv_sub_object    TYPE string,
          lv_line_number_s TYPE string,
          lv_line_number   TYPE i.

    lv_temp = replace( val   = iv_action
                       regex = |^{ c_ci_sig }|
                       with  = `` ).

    IF lv_temp <> iv_action. " CI navigation request detected

      SPLIT lv_temp AT c_object_separator INTO lv_main_object lv_sub_object lv_line_number_s.
      ls_item-obj_type = lv_main_object(4).
      ls_item-obj_name = lv_main_object+4(*).

      IF lv_sub_object IS NOT INITIAL.
        ls_sub_item-obj_type = lv_sub_object(4).
        ls_sub_item-obj_name = lv_sub_object+4(*).
      ENDIF.

      lv_line_number = lv_line_number_s.

      jump( is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_line_number = lv_line_number ).

      ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
