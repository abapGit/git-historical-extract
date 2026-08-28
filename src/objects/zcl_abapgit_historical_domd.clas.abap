CLASS zcl_abapgit_historical_domd DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_dd07v_tt TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_domain,
        dd01v TYPE dd01v,
        dd07v TYPE ty_dd07v_tt,
      END OF ty_domain .

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS read_domd
      IMPORTING
        is_vrsd          TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_domain) TYPE ty_domain
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_aff
      IMPORTING
        is_domain      TYPE ty_domain
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS map_to_aff
      IMPORTING
        is_domain     TYPE ty_domain
      RETURNING
        VALUE(rs_aff) TYPE zif_abapgit_aff_doma_v1=>ty_main
      RAISING
        zcx_abapgit_exception .

    METHODS map_data_type_to_aff
      IMPORTING
        iv_ddic_type       TYPE dd01v-datatype
        iv_length          TYPE dd01v-leng
      RETURNING
        VALUE(rv_aff_type) TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type
      RAISING
        zcx_abapgit_exception .

    METHODS get_data_type_mappings
      RETURNING
        VALUE(rt_mappings) TYPE zcl_abapgit_json_handler=>ty_json_abap_mappings .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_DOMD IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'DOMD'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD get_data_type_mappings.

* the AFF enum values are the component names of the constant structure,
* the DDIC values are the values of the constants
    DATA(ls_data_types) = zif_abapgit_aff_ddic_types_v1=>co_data_type.

    DATA(lo_structure) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( ls_data_types ) ).

    LOOP AT lo_structure->components INTO DATA(ls_component).
      ASSIGN COMPONENT ls_component-name OF STRUCTURE ls_data_types TO FIELD-SYMBOL(<lv_value>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      APPEND VALUE #(
        abap = <lv_value>
        json = ls_component-name ) TO rt_mappings.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_data_type_to_aff.

* most DDIC data types are named identically in the AFF format
    CASE iv_ddic_type.
      WHEN 'DF16'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
      WHEN 'DF34'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
      WHEN 'D16D'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_dec.
      WHEN 'D16R'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_raw.
      WHEN 'D16S'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df16_scl.
      WHEN 'D16N'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
      WHEN 'D34D'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_dec.
      WHEN 'D34R'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_raw.
      WHEN 'D34S'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-df34_scl.
      WHEN 'D34N'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
      WHEN 'DECF'.
        IF iv_length <= 16.
          rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16.
        ELSE.
          rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34.
        ENDIF.
      WHEN 'GEOM'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-geom_ewkb.
      WHEN 'RAWS'.
        rv_aff_type = zif_abapgit_aff_ddic_types_v1=>co_data_type-rawstring.
      WHEN OTHERS.
        rv_aff_type = iv_ddic_type.
    ENDCASE.

    DATA(lv_aff_type) = CONV string( rv_aff_type ).
    DATA(lt_mappings) = get_data_type_mappings( ).
    IF NOT line_exists( lt_mappings[ abap = lv_aff_type ] ).
      zcx_abapgit_exception=>raise(
        |Unsupported DDIC data type { iv_ddic_type } in domain { ms_tadir-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD map_to_aff.

    rs_aff-format_version = '1'.

    rs_aff-header-description = is_domain-dd01v-ddtext.
    IF is_domain-dd01v-ddlanguage IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in domain { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-header-original_language = is_domain-dd01v-ddlanguage.
    rs_aff-header-abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.

    rs_aff-format-data_type = map_data_type_to_aff(
      iv_ddic_type = is_domain-dd01v-datatype
      iv_length    = is_domain-dd01v-leng ).
    rs_aff-format-length = is_domain-dd01v-leng.
    IF is_domain-dd01v-decimals IS NOT INITIAL.
      rs_aff-format-decimals = is_domain-dd01v-decimals.
    ENDIF.

    IF is_domain-dd01v-outputlen IS NOT INITIAL.
      rs_aff-output_characteristics-length = is_domain-dd01v-outputlen.
    ENDIF.
    IF is_domain-dd01v-outputstyle IS NOT INITIAL.
      rs_aff-output_characteristics-style = is_domain-dd01v-outputstyle.
    ENDIF.
    IF is_domain-dd01v-convexit IS NOT INITIAL.
      rs_aff-output_characteristics-conversion_routine = is_domain-dd01v-convexit.
    ENDIF.
    IF is_domain-dd01v-lowercase IS NOT INITIAL.
      rs_aff-output_characteristics-case_sensitive = abap_true.
    ENDIF.
    IF is_domain-dd01v-signflag IS NOT INITIAL.
      rs_aff-output_characteristics-negative_values = abap_true.
    ENDIF.
    IF is_domain-dd01v-ampmformat IS NOT INITIAL.
      rs_aff-output_characteristics-am_pm_time_format = abap_true.
    ENDIF.

    IF is_domain-dd01v-appendname IS NOT INITIAL.
      APPEND VALUE #( name = is_domain-dd01v-appendname ) TO rs_aff-fixed_value_appends.
    ENDIF.
    LOOP AT is_domain-dd07v INTO DATA(ls_dd07v).
      IF ls_dd07v-appval IS NOT INITIAL.
        IF ls_dd07v-domname IS NOT INITIAL
            AND NOT line_exists( rs_aff-fixed_value_appends[ name = ls_dd07v-domname ] ).
          APPEND VALUE #( name = ls_dd07v-domname ) TO rs_aff-fixed_value_appends.
        ENDIF.
        CONTINUE.
      ENDIF.
      IF ls_dd07v-domvalue_h IS INITIAL OR ls_dd07v-domvalue_h = ls_dd07v-domvalue_l.
        APPEND VALUE #(
          fixed_value = ls_dd07v-domvalue_l
          description = ls_dd07v-ddtext ) TO rs_aff-fixed_values.
      ELSE.
        APPEND VALUE #(
          low_limit   = ls_dd07v-domvalue_l
          high_limit  = ls_dd07v-domvalue_h
          description = ls_dd07v-ddtext ) TO rs_aff-fixed_value_intervals.
      ENDIF.
    ENDLOOP.
    SORT rs_aff-fixed_value_appends BY name.
    DELETE ADJACENT DUPLICATES FROM rs_aff-fixed_value_appends COMPARING name.

    IF is_domain-dd01v-entitytab IS NOT INITIAL.
      rs_aff-value_table-name = is_domain-dd01v-entitytab.
    ENDIF.

  ENDMETHOD.


  METHOD read_domd.

    DATA lt_dd01v  TYPE STANDARD TABLE OF dd01v WITH DEFAULT KEY.
    DATA lt_dd01tv TYPE STANDARD TABLE OF dd01tv WITH DEFAULT KEY.
    DATA lt_dd07v  TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY.
    DATA lt_dd07tv TYPE STANDARD TABLE OF dd07tv WITH DEFAULT KEY.

    CALL FUNCTION 'SVRS_GET_VERSION_DOMD_40'
      EXPORTING
        object_name = is_vrsd-objname
        versno      = is_vrsd-versno
      TABLES
        dd01v_tab   = lt_dd01v
        dd07v_tab   = lt_dd07v
        dd01tv_tab  = lt_dd01tv
        dd07tv_tab  = lt_dd07tv
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical DOMA { is_vrsd-objname } version { is_vrsd-versno }| ).
    ENDIF.

    READ TABLE lt_dd01v INTO rs_domain-dd01v INDEX 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Historical DOMA { is_vrsd-objname } version { is_vrsd-versno } has no header| ).
    ENDIF.

* the descriptions are kept in the language dependent tables
    READ TABLE lt_dd01tv INTO DATA(ls_dd01tv) WITH KEY ddlanguage = rs_domain-dd01v-ddlanguage.
    IF sy-subrc <> 0.
      READ TABLE lt_dd01tv INTO ls_dd01tv INDEX 1.
    ENDIF.
    IF sy-subrc = 0.
      IF rs_domain-dd01v-ddlanguage IS INITIAL.
        rs_domain-dd01v-ddlanguage = ls_dd01tv-ddlanguage.
      ENDIF.
      IF rs_domain-dd01v-ddtext IS INITIAL.
        rs_domain-dd01v-ddtext = ls_dd01tv-ddtext.
      ENDIF.
    ENDIF.

* only the fixed values of the original language, translations are not extracted
    LOOP AT lt_dd07v INTO DATA(ls_dd07v)
        WHERE ddlanguage = rs_domain-dd01v-ddlanguage
        OR ddlanguage IS INITIAL.
      IF ls_dd07v-ddtext IS INITIAL.
        READ TABLE lt_dd07tv INTO DATA(ls_dd07tv)
          WITH KEY ddlanguage = rs_domain-dd01v-ddlanguage
                   valpos     = ls_dd07v-valpos.
        IF sy-subrc = 0.
          ls_dd07v-ddtext = ls_dd07tv-ddtext.
        ENDIF.
      ENDIF.
      APPEND ls_dd07v TO rs_domain-dd07v.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_aff.

* the JSON handler skips these paths if they carry the given default value
    DATA(lt_skip_paths) = VALUE zcl_abapgit_json_handler=>ty_skip_paths(
      ( path = '/format/decimals'             value = '0' )
      ( path = '/outputCharacteristics/style' value = '00' ) ).

    DATA(lt_enum_mappings) = VALUE zcl_abapgit_json_handler=>ty_enum_mappings(
      ( path     = '/format/dataType'
        mappings = get_data_type_mappings( ) ) ).

    DATA(ls_aff) = map_to_aff( is_domain ).

    TRY.
        DATA(lv_json) = NEW zcl_abapgit_json_handler( )->serialize(
          iv_data          = ls_aff
          iv_enum_mappings = lt_enum_mappings
          iv_skip_paths    = lt_skip_paths ).
      CATCH cx_root INTO DATA(lx_error).
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_json = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json ).

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA ls_file LIKE LINE OF rt_files.


    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

    SORT lt_vrsd BY objtype versno DESCENDING.
    READ TABLE lt_vrsd INTO DATA(ls_vrsd) WITH KEY objtype = 'DOMD'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* the version database knows DOMD, abapGit and the AFF know DOMA
    ls_file-filename = |{ to_lower( ms_tadir-obj_name ) }.doma.json|.
    ls_file-source = serialize_aff( read_domd( ls_vrsd ) ).

    INSERT ls_file INTO TABLE rt_files.

  ENDMETHOD.

  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.doma.json|
      deleted  = abap_true ) TO rt_files.

  ENDMETHOD.
ENDCLASS.
