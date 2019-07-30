class lcl_tadir definition create public.

  public section.
    INTERFACES lif_tadir.
    methods:
      constructor
        importing
          outer type ref to zcl_abapgit_object_ddlx.
  protected section.
  private section.
    data: mo_outer type ref to zcl_abapgit_object_ddlx.

endclass.

class zcl_abapgit_object_ddlx definition local friends lcl_tadir.
class lcl_tadir implementation.


  method lif_tadir~tadir_insert.
    mo_outer->tadir_insert( iv_package = iv_package ).
  endmethod.

  method constructor.
    mo_outer = outer.
  endmethod.

endclass.
