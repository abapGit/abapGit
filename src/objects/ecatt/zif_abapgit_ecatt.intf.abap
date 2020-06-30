INTERFACE zif_abapgit_ecatt
  PUBLIC .

  " downport missing types

  TYPES:
    etvo_invert_validation TYPE c LENGTH 1,
    etvo_error_prio        TYPE n LENGTH 1,
    etvo_impl_name         TYPE c LENGTH 30,
    etvo_impl_type         TYPE c LENGTH 1,
    etvo_impl_subtype      TYPE c LENGTH 4,
    etvo_package           TYPE c LENGTH 255,
    BEGIN OF etvoimpl_det,
      impl_name    TYPE etvo_impl_name,
      impl_type    TYPE etvo_impl_type,
      impl_subtype TYPE etvo_impl_subtype,
      impl_package TYPE etvo_package,
    END OF etvoimpl_det.

  TYPES:
    BEGIN OF ecvo_bus_msg.
      INCLUDE TYPE etobj_key.
  TYPES:
    bus_msg_no   TYPE c LENGTH 1, " etvo_msg_no
    arbgb        TYPE arbgb,
    msgnr        TYPE msgnr,
    bus_msg_text TYPE string, "etvo_bus_msg_text
    otr_key      TYPE sotr_conc,
    msg_type     TYPE c LENGTH 4, "etvo_msg_type
    END OF ecvo_bus_msg,

    etvo_bus_msg_tabtype TYPE STANDARD TABLE OF ecvo_bus_msg.

ENDINTERFACE.
