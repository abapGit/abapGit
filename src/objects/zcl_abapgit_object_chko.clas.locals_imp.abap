*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

class lcl_chko_aff_persistence definition
  final.

  public section.
    types: "TODO replace with if_chko_aff_types
      begin of ty_content,
        category           type string,
        implementing_class type string,
        remote_enabled     type flag,
        parameters         type cl_chko_db_api=>ty_parameters,
      end of ty_content.

    types: ty_chko type if_aff_chko_v1=>ty_main.

    methods:
      get_content
        importing object   type ref to if_aff_obj
                  language type spras
                  version  type r3state
        exporting data     type data
        raising   cx_aff_root,

      save_content
        importing data     type data
                  object   type ref to if_aff_obj
                  language type spras
                  version  type r3state
                  saved_by type as4user
        raising   cx_aff_root.
  protected section.
  private section.
endclass.



class lcl_chko_aff_persistence implementation.

  method get_content.
    clear data.

    data(db_api) = new cl_chko_db_api( ).
    data(chko_name) = object->get_name( ).
    data(master_language) = cl_aff_object_utility=>get_master_language_from_tadir( object ).

    data(chko_header) = db_api->get_header( name     = conv #( chko_name )
                                            version  = version
                                            language = language ).
    data(chko_content) = db_api->get_content( name     = conv #( chko_name )
                                              version  = version
                                              language = language ).

    data(properties) = value ty_chko(
      format_version             = '1'
      header-description         = chko_header-description
      header-original_language   = master_language
      header-abap_language_version  = chko_header-abap_language_version
      category           = chko_content-category-name
      implementing_class = chko_content-implementing_class-name
      parameters         = corresponding #( chko_content-parameters mapping hidden = modifiable )
      remote_enabled     = chko_content-remote_enabled ).

    data = properties.
  endmethod.

  method save_content.
    data content  type cl_chko_db_api=>ty_content .

    data(properties) = conv ty_chko( data ).
    data(db_api) = new cl_chko_db_api( ).
    data(chko_name) = object->get_name( ).

    data(header) = db_api->get_header( name     = conv #( chko_name )
                                       version  = version
                                       language = language ).

    if header is initial and version = 'I'.
      "try to load the active header
      header = db_api->get_header( name     = conv #( chko_name )
                                   version  = 'A'
                                   language = language ).
    endif.

    if header is initial.
      header-name = chko_name.
      header-created_by = saved_by.
      get time stamp field header-created_at.
    endif.

    header-version = version.
    header-description = properties-header-description.
    header-abap_language_version = properties-header-abap_language_version.
    header-changed_by = saved_by.
    get time stamp field header-changed_at.

    content-category-name = properties-category.
    content-implementing_class-name = properties-implementing_class.
    content-parameters = corresponding #( properties-parameters mapping modifiable = hidden ).
    content-remote_enabled = properties-remote_enabled.

    db_api->update( header   = header
                    content  = content ).
  endmethod.

endclass.
