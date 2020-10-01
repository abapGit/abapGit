INTERFACE lif_task_definition.

  TYPES: BEGIN OF ty_task_data,
           short_text                 TYPE hr_mcshort,
           plvar                      TYPE plvar,
           wi_text                    TYPE witext,
           method                     TYPE hrs1201,
           method_binding             TYPE hrsmtbind,
           starting_events            TYPE hrsevtab,
           starting_events_binding    TYPE hrsevbind,
           terminating_events         TYPE hrsetmtab,
           terminating_events_binding TYPE hrsevbind,
           descriptions               TYPE wstexts,
         END OF ty_task_data.

  METHODS clear_origin_data.
  METHODS get_definition RETURNING VALUE(rs_result) TYPE ty_task_data.
  METHODS get_container RETURNING VALUE(ri_result) TYPE REF TO if_swf_cnt_container.
  METHODS get_user_container RETURNING VALUE(ri_result) TYPE REF TO if_swf_cnt_container.
  METHODS import_container IMPORTING iv_xml_string TYPE xstring
                           RAISING   zcx_abapgit_exception.
  METHODS create_task RAISING zcx_abapgit_exception.
  METHODS save IMPORTING iv_package TYPE devclass OPTIONAL
               RAISING   zcx_abapgit_exception.
  METHODS change_wi_text RAISING zcx_abapgit_exception.
  METHODS change_method RAISING zcx_abapgit_exception.
  METHODS change_start_events RAISING zcx_abapgit_exception.
  METHODS change_terminating_events RAISING zcx_abapgit_exception.
  METHODS change_text RAISING zcx_abapgit_exception.

ENDINTERFACE.
