class zcl_abapgit_i18n_manager definition
  public
  final
  create public .

  public section.

    class-methods class_constructor.

    methods add_object
      importing
        !iv_object_type   type tadir-object
        !iv_object_name   type tadir-obj_name.

    methods build_po_files
      importing
        !is_i18n_params   type zif_abapgit_definitions=>ty_i18n_params
      returning
        value(rt_po_files) type zif_abapgit_i18n_file=>ty_table_of
      raising
        zcx_abapgit_exception.


  protected section.
  private section.
    class-data mt_supported_obj_types type hashed table of tadir-object with unique key table_line.
    data mt_objects type standard table of zif_abapgit_definitions=>ty_item_signature.

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_MANAGER IMPLEMENTATION.


  method add_object.

    field-symbols <ls_obj> like line of mt_objects.

    read table mt_supported_obj_types with key table_line = iv_object_type transporting no fields.
    if sy-subrc = 0.
      append initial line to mt_objects assigning <ls_obj>.
      <ls_obj>-obj_type = iv_object_type.
      <ls_obj>-obj_name = iv_object_name.
    endif.

  endmethod.


  method build_po_files.

    " TODO refactor

    types:
      begin of ty_pos,
        lang type laiso,
        po type ref to zcl_abapgit_po_file,
      end of ty_pos.

    data li_lxe type ref to zif_abapgit_lxe_texts.
    data li_po type ref to zif_abapgit_i18n_file.
    data lo_po type ref to zcl_abapgit_po_file.
    data lt_po_files_portion type zif_abapgit_i18n_file=>ty_table_of.
    data lt_po_files type standard table of ty_pos.
    data ls_po_wl like line of lt_po_files.

    field-symbols <ls_obj> like line of mt_objects.

    create object li_lxe type zcl_abapgit_lxe_texts.

    loop at mt_objects assigning <ls_obj>.

      lt_po_files_portion = li_lxe->serialize_as_po(
        iv_object_type = <ls_obj>-obj_type
        iv_object_name = <ls_obj>-obj_name
        is_i18n_params = is_i18n_params ).

      if lt_po_files is initial.

        loop at lt_po_files_portion into li_po.
          ls_po_wl-lang = li_po->lang( ).
          ls_po_wl-po  ?= li_po.
          append ls_po_wl to lt_po_files.
        endloop.

      else.

        loop at lt_po_files_portion into li_po.
          read table lt_po_files into ls_po_wl with key lang = li_po->lang( ).
          assert sy-subrc = 0.
          lo_po ?= li_po.
          ls_po_wl-po->union_with( lo_po ).
        endloop.

      endif.

    endloop.

    loop at lt_po_files into ls_po_wl.
      append ls_po_wl-po to rt_po_files.
    endloop.

  endmethod.


  method class_constructor.

    data mv_types type string.
    data mt_types type standard table of tadir-object.

    mv_types = 'PROG CLAS INTF MSAG DOMA DTEL PARA TRAN TABL SHI3'.
    split mv_types at space into table mt_types.
    mt_supported_obj_types = mt_types.

  endmethod.
ENDCLASS.
