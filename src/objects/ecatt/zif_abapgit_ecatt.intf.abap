INTERFACE zif_abapgit_ecatt
  PUBLIC .

  " downport missing types

  TYPES:
    ty_invert_validation TYPE c LENGTH 1,
    ty_error_prio        TYPE n LENGTH 1,
    ty_impl_name         TYPE c LENGTH 30,
    ty_impl_type         TYPE c LENGTH 1,
    ty_impl_subtype      TYPE c LENGTH 4,
    ty_package           TYPE c LENGTH 255,
    BEGIN OF ty_impl_det,
      impl_name    TYPE ty_impl_name,
      impl_type    TYPE ty_impl_type,
      impl_subtype TYPE ty_impl_subtype,
      impl_package TYPE ty_package,
    END OF ty_impl_det.

  TYPES:
    BEGIN OF ty_bus_msg.
      INCLUDE TYPE etobj_key.
  TYPES:
      bus_msg_no   TYPE c LENGTH 1, " ty_msg_no
      arbgb        TYPE arbgb,
      msgnr        TYPE msgnr,
      bus_msg_text TYPE string, "ty_bus_msg_text
      otr_key      TYPE sotr_conc,
      msg_type     TYPE c LENGTH 4, "ty_msg_type
    END OF ty_bus_msg,

    ty_bus_msgs TYPE STANDARD TABLE OF ty_bus_msg.

ENDINTERFACE.
