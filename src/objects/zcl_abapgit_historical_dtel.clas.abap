CLASS zcl_abapgit_historical_dtel DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS read_dtel
      IMPORTING
        is_vrsd        TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_dtel) TYPE dd04v
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_aff
      IMPORTING
        is_dtel        TYPE dd04v
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS map_to_aff
      IMPORTING
        is_dtel       TYPE dd04v
      RETURNING
        VALUE(rs_aff) TYPE zif_abapgit_aff_dtel_v1=>ty_main
      RAISING
        zcx_abapgit_exception .

    METHODS map_data_type_to_aff
      IMPORTING
        iv_ddic_type       TYPE dd04v-datatype
        iv_length          TYPE dd04v-leng
      RETURNING
        VALUE(rv_aff_type) TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type
      RAISING
        zcx_abapgit_exception .

    METHODS get_data_type_mappings
      RETURNING
        VALUE(rt_mappings) TYPE zcl_abapgit_json_handler=>ty_json_abap_mappings .

    METHODS get_category_mappings
      RETURNING
        VALUE(rt_mappings) TYPE zcl_abapgit_json_handler=>ty_json_abap_mappings .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_DTEL IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'DTED'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD get_category_mappings.

    rt_mappings = VALUE #(
      ( abap = zif_abapgit_aff_dtel_v1=>co_category-domain
        json = 'domain' )
      ( abap = zif_abapgit_aff_dtel_v1=>co_category-predefined_type
        json = 'predefinedType' )
      ( abap = zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type
        json = 'referenceToPredefinedType' )
      ( abap = zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type
        json = 'referenceDictionaryType' )
      ( abap = zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type
        json = 'referenceClasIntType' ) ).

  ENDMETHOD.


  METHOD get_data_type_mappings.

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
        |Unsupported DDIC data type { iv_ddic_type } in data element { ms_tadir-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD map_to_aff.

    rs_aff-format_version = '1'.
    rs_aff-header-description = is_dtel-ddtext.
    IF is_dtel-ddlanguage IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in data element { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-header-original_language = is_dtel-ddlanguage.
    rs_aff-header-abap_language_version = zif_abapgit_aff_types_v1=>co_abap_language_version-standard.

    CASE is_dtel-refkind.
      WHEN 'D'.
        rs_aff-data_type_information-category = zif_abapgit_aff_dtel_v1=>co_category-domain.
        rs_aff-data_type_information-type_name = is_dtel-domname.
      WHEN space.
        rs_aff-data_type_information-category = zif_abapgit_aff_dtel_v1=>co_category-predefined_type.
        rs_aff-data_type_information-predefined_type-data_type = map_data_type_to_aff(
          iv_ddic_type = is_dtel-datatype
          iv_length    = is_dtel-leng ).
        rs_aff-data_type_information-predefined_type-length = is_dtel-leng.
        IF is_dtel-decimals IS NOT INITIAL.
          rs_aff-data_type_information-predefined_type-decimals = is_dtel-decimals.
        ENDIF.
      WHEN 'R'.
        CASE is_dtel-reftype.
          WHEN 'B'.
            rs_aff-data_type_information-category =
              zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
            rs_aff-data_type_information-predefined_type-data_type = map_data_type_to_aff(
              iv_ddic_type = is_dtel-datatype
              iv_length    = is_dtel-leng ).
            rs_aff-data_type_information-predefined_type-length = is_dtel-leng.
            IF is_dtel-decimals IS NOT INITIAL.
              rs_aff-data_type_information-predefined_type-decimals = is_dtel-decimals.
            ENDIF.
          WHEN 'E' OR 'S' OR 'L'.
            rs_aff-data_type_information-category =
              zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type.
            rs_aff-data_type_information-type_name = is_dtel-domname.
          WHEN 'C' OR 'I'.
            rs_aff-data_type_information-category =
              zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type.
            rs_aff-data_type_information-type_name = is_dtel-domname.
          WHEN 'A'.
            rs_aff-data_type_information-category =
              zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
            rs_aff-data_type_information-type_name = 'ANY'.
          WHEN 'D'.
            rs_aff-data_type_information-category =
              zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
            rs_aff-data_type_information-type_name = 'DATA'.
          WHEN 'O'.
            rs_aff-data_type_information-category =
              zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type.
            rs_aff-data_type_information-type_name = 'OBJECT'.
          WHEN OTHERS.
            zcx_abapgit_exception=>raise(
              |Unsupported reference type { is_dtel-reftype } in data element { ms_tadir-obj_name }| ).
        ENDCASE.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise(
          |Unsupported type category { is_dtel-refkind } in data element { ms_tadir-obj_name }| ).
    ENDCASE.

    rs_aff-field_labels-short = is_dtel-scrtext_s.
    rs_aff-field_labels-short_length = is_dtel-scrlen1.
    rs_aff-field_labels-medium = is_dtel-scrtext_m.
    rs_aff-field_labels-medium_length = is_dtel-scrlen2.
    rs_aff-field_labels-long = is_dtel-scrtext_l.
    rs_aff-field_labels-long_length = is_dtel-scrlen3.
    rs_aff-field_labels-heading = is_dtel-reptext.
    rs_aff-field_labels-heading_length = is_dtel-headlen.

    IF is_dtel-shlpname IS NOT INITIAL.
      rs_aff-additional_properties-search_help-name = is_dtel-shlpname.
      rs_aff-additional_properties-search_help-parameter = is_dtel-shlpfield.
    ENDIF.
    rs_aff-additional_properties-parameter_id = is_dtel-memoryid.
    rs_aff-additional_properties-default_component_name = is_dtel-deffdname.
    rs_aff-additional_properties-change_document_relevant = is_dtel-logflag.
    rs_aff-additional_properties-bidirectional_options-basic_direction = is_dtel-ltrflddis.
    rs_aff-additional_properties-bidirectional_options-no_filtering = is_dtel-bidictrlc.
    rs_aff-additional_properties-no_input_history = is_dtel-nohistory.

  ENDMETHOD.


  METHOD read_dtel.

    DATA lt_dd04v  TYPE STANDARD TABLE OF dd04v WITH DEFAULT KEY.
    DATA lt_dd04tv TYPE STANDARD TABLE OF dd04tv WITH DEFAULT KEY.

    CALL FUNCTION 'SVRS_GET_VERSION_DTED_40'
      EXPORTING
        object_name           = is_vrsd-objname
        versno                = is_vrsd-versno
      TABLES
        dd04v_tab             = lt_dd04v
        dd04tv_tab            = lt_dd04tv
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical DTEL { is_vrsd-objname } version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_dd04v INTO rs_dtel INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF rs_dtel-dtelmaster IS NOT INITIAL.
      rs_dtel-ddlanguage = rs_dtel-dtelmaster.
    ENDIF.
    READ TABLE lt_dd04tv INTO DATA(ls_dd04tv) WITH KEY ddlanguage = rs_dtel-ddlanguage.
    IF sy-subrc <> 0 AND rs_dtel-ddlanguage IS INITIAL.
      READ TABLE lt_dd04tv INTO ls_dd04tv INDEX 1.
    ENDIF.
    IF sy-subrc = 0.
      IF rs_dtel-ddlanguage IS INITIAL.
        rs_dtel-ddlanguage = ls_dd04tv-ddlanguage.
      ENDIF.
      rs_dtel-ddtext = ls_dd04tv-ddtext.
      rs_dtel-reptext = ls_dd04tv-reptext.
      rs_dtel-scrtext_s = ls_dd04tv-scrtext_s.
      rs_dtel-scrtext_m = ls_dd04tv-scrtext_m.
      rs_dtel-scrtext_l = ls_dd04tv-scrtext_l.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_aff.

    DATA(lt_skip_paths) = VALUE zcl_abapgit_json_handler=>ty_skip_paths(
      ( path = '/dataTypeInformation/predefinedType/decimals' value = '0' )
      ( path = '/additionalProperties/bidirectionalOptions/basicDirection' value = 'leftToRight' ) ).

    DATA(lt_enum_mappings) = VALUE zcl_abapgit_json_handler=>ty_enum_mappings(
      ( path     = '/dataTypeInformation/category'
        mappings = get_category_mappings( ) )
      ( path     = '/dataTypeInformation/predefinedType/dataType'
        mappings = get_data_type_mappings( ) )
      ( path     = '/additionalProperties/bidirectionalOptions/basicDirection'
        mappings = VALUE #(
          ( abap = zif_abapgit_aff_dtel_v1=>co_bidi_basic_direction-left_to_right
            json = 'leftToRight' )
          ( abap = zif_abapgit_aff_dtel_v1=>co_bidi_basic_direction-right_to_left
            json = 'rightToLeft' ) ) ) ).

    DATA(ls_aff) = map_to_aff( is_dtel ).

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
    READ TABLE lt_vrsd INTO DATA(ls_vrsd) WITH KEY objtype = 'DTED'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ls_file-filename = |{ to_lower( ms_tadir-obj_name ) }.dtel.json|.
    DATA(ls_dtel) = read_dtel( ls_vrsd ).
    IF ls_dtel IS INITIAL.
      RETURN.
    ENDIF.
    ls_file-source = serialize_aff( ls_dtel ).

    INSERT ls_file INTO TABLE rt_files.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.dtel.json|
      deleted  = abap_true ) TO rt_files.

  ENDMETHOD.
ENDCLASS.
