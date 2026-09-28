INTERFACE zif_abapgit_hist_aff_tabt_v1 PUBLIC.

* technical table settings, <name>.tabl.settings.json, copied from zif_aff_tabt_v1 in
* https://github.com/SAP/abap-file-formats as the abapGit dependency does not provide
* it, replace with zif_abapgit_aff_tabt_v1 once abapGit ships that

  TYPES ty_data_class_category TYPE c LENGTH 5.

  TYPES ty_size_category TYPE c LENGTH 2.

  CONSTANTS:
    BEGIN OF co_size_category,
      undefined TYPE ty_size_category VALUE space,
      cat_0     TYPE ty_size_category VALUE '0',
      cat_1     TYPE ty_size_category VALUE '1',
      cat_2     TYPE ty_size_category VALUE '2',
      cat_3     TYPE ty_size_category VALUE '3',
      cat_4     TYPE ty_size_category VALUE '4',
      cat_5     TYPE ty_size_category VALUE '5',
      cat_6     TYPE ty_size_category VALUE '6',
      cat_7     TYPE ty_size_category VALUE '7',
      cat_8     TYPE ty_size_category VALUE '8',
      cat_9     TYPE ty_size_category VALUE '9',
    END OF co_size_category.

  TYPES ty_buffer_state TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_buffer_state,
      not_allowed              TYPE ty_buffer_state VALUE 'N',
      switched_on              TYPE ty_buffer_state VALUE 'X',
      allowed_but_switched_off TYPE ty_buffer_state VALUE 'A',
    END OF co_buffer_state.

  TYPES ty_buffer_type TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_buffer_type,
      no_buffer TYPE ty_buffer_type VALUE space,
      single    TYPE ty_buffer_type VALUE 'P',
      generic   TYPE ty_buffer_type VALUE 'G',
      full      TYPE ty_buffer_type VALUE 'X',
    END OF co_buffer_type.

  TYPES:
    BEGIN OF ty_buffering,
      state                         TYPE ty_buffer_state,
      type                          TYPE ty_buffer_type,
      nr_of_key_flds_4_generic_buff TYPE i,
    END OF ty_buffering.

  TYPES ty_translation_type TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_translation_type,
      no_language_key TYPE ty_translation_type VALUE space,
      standard        TYPE ty_translation_type VALUE 'X',
      load_table      TYPE ty_translation_type VALUE 'L',
      object_specific TYPE ty_translation_type VALUE 'T',
      not_relevant    TYPE ty_translation_type VALUE 'N',
    END OF co_translation_type.

  TYPES ty_storage_type TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_storage_type,
      column_store TYPE ty_storage_type VALUE 'C',
      row_store    TYPE ty_storage_type VALUE 'R',
      undefined    TYPE ty_storage_type VALUE space,
    END OF co_storage_type.

  TYPES ty_load_unit TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF co_load_unit,
      column_preferred TYPE ty_load_unit VALUE space,
      page_preferred   TYPE ty_load_unit VALUE 'P',
      column_enforced  TYPE ty_load_unit VALUE 'A',
      page_enforced    TYPE ty_load_unit VALUE 'Q',
    END OF co_load_unit.

  TYPES:
    BEGIN OF ty_db_specific_settings,
      storage_type TYPE ty_storage_type,
      load_unit    TYPE ty_load_unit,
    END OF ty_db_specific_settings.

  TYPES:
    BEGIN OF ty_technical_table_settings,
      data_class_category TYPE ty_data_class_category,
      size_category       TYPE ty_size_category,
      log_changes         TYPE abap_bool,
      writable_by_amdp    TYPE abap_bool,
      translation         TYPE ty_translation_type,
    END OF ty_technical_table_settings.

  TYPES:
    BEGIN OF ty_main,
      format_version       TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      general_information  TYPE ty_technical_table_settings,
      buffering            TYPE ty_buffering,
      db_specific_settings TYPE ty_db_specific_settings,
    END OF ty_main.

ENDINTERFACE.
