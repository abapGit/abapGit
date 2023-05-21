CLASS zcl_abapgit_gui_in_page_modal DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !ii_child      TYPE REF TO zif_abapgit_gui_renderable
        !iv_width      TYPE i OPTIONAL
        !iv_height     TYPE i OPTIONAL
      RETURNING
        VALUE(ro_wrap) TYPE REF TO zcl_abapgit_gui_in_page_modal
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !ii_child  TYPE REF TO zif_abapgit_gui_renderable
        !iv_width  TYPE i OPTIONAL
        !iv_height TYPE i OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_child TYPE REF TO zif_abapgit_gui_renderable.

    DATA:
      BEGIN OF ms_attrs,
        width  TYPE i,
        height TYPE i,
      END OF ms_attrs.

ENDCLASS.



CLASS zcl_abapgit_gui_in_page_modal IMPLEMENTATION.


  METHOD constructor.

    ms_attrs-width  = iv_width.
    ms_attrs-height = iv_height.
    mi_child        = ii_child.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_wrap
      EXPORTING
        ii_child  = ii_child
        iv_width  = iv_width
        iv_height = iv_height.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lo_style TYPE REF TO zcl_abapgit_string_buffer.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_style.

    IF ms_attrs-width IS NOT INITIAL.
      lo_style->add( |width:{ ms_attrs-width }px;| ).
    ENDIF.
    IF ms_attrs-height IS NOT INITIAL.
      lo_style->add( |height:{ ms_attrs-height }px;| ).
    ENDIF.

    ri_html->add( |<div class="modal" style="{ lo_style->join_w_space_and_flush( ) }">| ).
    ri_html->add( |<div class="modal-guts">| ).
    ri_html->add( mi_child->render( ) ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).
    ri_html->add( |<div class="modal-overlay"></div>| ).

  ENDMETHOD.
ENDCLASS.
