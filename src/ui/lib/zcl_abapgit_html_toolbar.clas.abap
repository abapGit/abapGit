CLASS zcl_abapgit_html_toolbar DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_id TYPE string OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_html_toolbar.
    METHODS constructor
      IMPORTING
        !iv_id TYPE string OPTIONAL .
    METHODS add
      IMPORTING
        !iv_txt        TYPE string
        !io_sub        TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
        !iv_typ        TYPE c DEFAULT zif_abapgit_html=>c_action_type-sapevent
        !iv_act        TYPE string OPTIONAL
        !iv_ico        TYPE string OPTIONAL
        !iv_cur        TYPE abap_bool OPTIONAL
        !iv_opt        TYPE c OPTIONAL
        !iv_chk        TYPE abap_bool DEFAULT abap_undefined
        !iv_aux        TYPE string OPTIONAL
        !iv_id         TYPE string OPTIONAL
        !iv_title      TYPE string OPTIONAL
        !iv_class      TYPE string OPTIONAL
        !iv_li_class   TYPE string OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_toolbar .
    METHODS count_items
      RETURNING
        VALUE(rv_count) TYPE i .
    METHODS render
      IMPORTING
        !iv_right      TYPE abap_bool OPTIONAL
        !iv_sort       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_as_droplist
      IMPORTING
        !iv_label      TYPE string
        !iv_right      TYPE abap_bool OPTIONAL
        !iv_sort       TYPE abap_bool OPTIONAL
        !iv_corner     TYPE abap_bool OPTIONAL
        !iv_action     TYPE string OPTIONAL
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_item,
        txt      TYPE string,
        act      TYPE string,
        ico      TYPE string,
        sub      TYPE REF TO zcl_abapgit_html_toolbar,
        opt      TYPE c LENGTH 1,
        typ      TYPE c LENGTH 1,
        cur      TYPE abap_bool,
        chk      TYPE abap_bool,
        aux      TYPE string,
        id       TYPE string,
        title    TYPE string,
        class    TYPE string,
        li_class TYPE string,
      END OF ty_item .
    TYPES:
      ty_items TYPE STANDARD TABLE OF ty_item .

    DATA mt_items TYPE ty_items .
    DATA mv_id TYPE string .

    METHODS render_items
      IMPORTING
        !iv_sort       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
ENDCLASS.



CLASS zcl_abapgit_html_toolbar IMPLEMENTATION.


  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_typ = zif_abapgit_html=>c_action_type-separator  " sep doesn't have action
      OR iv_typ = zif_abapgit_html=>c_action_type-onclick      " click may have no action (assigned in JS)
      OR iv_typ = zif_abapgit_html=>c_action_type-dummy        " dummy may have no action
      OR iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ASSERT NOT ( iv_chk <> abap_undefined AND io_sub IS NOT INITIAL ).

    ls_item-txt   = iv_txt.
    ls_item-act   = iv_act.
    ls_item-ico   = iv_ico.
    ls_item-sub   = io_sub.
    ls_item-opt   = iv_opt.
    ls_item-typ   = iv_typ.
    ls_item-cur   = iv_cur.
    ls_item-chk   = iv_chk.
    ls_item-aux   = iv_aux.
    ls_item-id    = iv_id.
    ls_item-title = iv_title.
    ls_item-class = iv_class.
    ls_item-li_class = iv_li_class.

    APPEND ls_item TO mt_items.

    ro_self = me.

  ENDMETHOD.


  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.

  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_id = iv_id.
  ENDMETHOD.


  METHOD count_items.
    rv_count = lines( mt_items ).
  ENDMETHOD.


  METHOD render.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lv_class = 'nav-container'.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_as_droplist.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lv_class = 'nav-container'.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.
    IF iv_corner = abap_true.
      lv_class = lv_class && ' corner'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( '<ul><li>' ).
    ri_html->add_a( iv_txt = iv_label
                    iv_typ = zif_abapgit_html=>c_action_type-sapevent
                    iv_act = iv_action ).
    ri_html->add( '<div class="minizone"></div>' ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</li></ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_items.

    DATA: lv_class       TYPE string,
          lv_class_value TYPE string,
          lv_icon        TYPE string,
          lv_id          TYPE string,
          lv_check       TYPE string,
          lv_aux         TYPE string,
          lv_has_icons   TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    " Check has icons or check boxes
    LOOP AT mt_items ASSIGNING <ls_item> WHERE ico IS NOT INITIAL OR chk <> abap_undefined.
      lv_has_icons = abap_true.
      lv_class     = ' class="with-icons"'.
      EXIT.
    ENDLOOP.

    IF mv_id IS NOT INITIAL.
      lv_id = | id="{ mv_id }"|.
    ENDIF.

    ri_html->add( |<ul{ lv_id }{ lv_class }>| ).

    " Render items
    LOOP AT mt_items ASSIGNING <ls_item>.
      CLEAR: lv_class, lv_class_value, lv_icon.

      IF <ls_item>-typ = zif_abapgit_html=>c_action_type-separator.
        ri_html->add( |<li class="separator">{ <ls_item>-txt }</li>| ).
        CONTINUE.
      ENDIF.

      IF lv_has_icons = abap_true.
        IF <ls_item>-chk = abap_true.
          lv_icon  = ri_html->icon( 'check/blue' ).
          lv_check = ' data-check="X"'.
        ELSEIF <ls_item>-chk = abap_false.
          lv_icon = ri_html->icon( 'check/grey' ).
          lv_check = ' data-check=""'.
        ELSE. " abap_undefined -> not a check box
          lv_icon = ri_html->icon( <ls_item>-ico ).
        ENDIF.
      ENDIF.


      IF <ls_item>-cur = abap_true.
        IF <ls_item>-li_class IS INITIAL.
          lv_class_value =  'current-menu-item'.
        ELSE.
          lv_class_value =  |current-menu-item { <ls_item>-li_class }|.
        ENDIF.
      ELSE.
        lv_class_value =  <ls_item>-li_class.
      ENDIF.
      IF lv_class_value IS NOT INITIAL.
        lv_class =   | class="{ lv_class_value }"|.
      ENDIF.
      IF <ls_item>-aux IS NOT INITIAL.
        lv_aux = | data-aux="{ <ls_item>-aux }"|.
      ENDIF.

      ri_html->add( |<li{ lv_class }{ lv_check }{ lv_aux }>| ).

      IF <ls_item>-sub IS INITIAL.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = <ls_item>-typ
                        iv_act   = <ls_item>-act
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title
                        iv_class = <ls_item>-class ).
      ELSE.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = zif_abapgit_html=>c_action_type-dummy
                        iv_act   = ''
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title
                        iv_class = <ls_item>-class ).
        ri_html->add( <ls_item>-sub->render_items( iv_sort ) ).
      ENDIF.
      ri_html->add( '</li>' ).

    ENDLOOP.

    ri_html->add( '</ul>' ).

  ENDMETHOD.
ENDCLASS.
