CLASS zcl_abapgit_historical_intf DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES BEGIN OF ty_intf.
    TYPES vseointerf TYPE vseointerf.
    INCLUDE TYPE zcl_abapgit_historical_oo=>ty_components.
    TYPES END OF ty_intf.

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS read_intf
      IMPORTING
        is_vrsd        TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_intf) TYPE ty_intf
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_aff
      IMPORTING
        is_intf        TYPE ty_intf
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS map_to_aff
      IMPORTING
        is_intf       TYPE ty_intf
      RETURNING
        VALUE(rs_aff) TYPE zif_abapgit_aff_intf_v1=>ty_main
      RAISING
        zcx_abapgit_exception .

    METHODS get_category_mappings
      RETURNING
        VALUE(rt_mappings) TYPE zcl_abapgit_json_handler=>ty_json_abap_mappings .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_INTF IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    APPEND VALUE #(
      objtype  = 'INTF'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.

  ENDMETHOD.


  METHOD get_category_mappings.

* same mapping as abapGit's INTF serializer, the AFF schema has no value for 51/52 instance components
    rt_mappings = VALUE #(
      ( abap = zif_abapgit_aff_intf_v1=>co_category-general
        json = 'standard' )
      ( abap = zif_abapgit_aff_intf_v1=>co_category-classic_badi
        json = 'classicBadi' )
      ( abap = zif_abapgit_aff_intf_v1=>co_category-business_static_components
        json = 'businessStaticComponents' )
      ( abap = zif_abapgit_aff_intf_v1=>co_category-db_procedure_proxy
        json = 'dbProcedureProxy' )
      ( abap = zif_abapgit_aff_intf_v1=>co_category-web_dynpro_runtime
        json = 'webDynproRuntime' )
      ( abap = zif_abapgit_aff_intf_v1=>co_category-enterprise_service
        json = 'enterpriseService' ) ).

  ENDMETHOD.


  METHOD map_to_aff.

    rs_aff-format_version = '1'.
    rs_aff-header-description = is_intf-vseointerf-descript.
    IF is_intf-vseointerf-langu IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in interface { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-header-original_language = is_intf-vseointerf-langu.
    rs_aff-header-abap_language_version = zcl_abapgit_historical_oo=>map_abap_language_version(
      is_intf-vseointerf-unicode ).

    rs_aff-category = is_intf-vseointerf-category.
    DATA(lt_mappings) = get_category_mappings( ).
    DATA(lv_category) = CONV string( rs_aff-category ).
    IF NOT line_exists( lt_mappings[ abap = lv_category ] ).
      zcx_abapgit_exception=>raise(
        |Unsupported category { is_intf-vseointerf-category } in interface { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-proxy = is_intf-vseointerf-clsproxy.

    rs_aff-descriptions = zcl_abapgit_historical_oo=>map_descriptions(
      is_components = CORRESPONDING #( is_intf )
      iv_language   = is_intf-vseointerf-langu ).

  ENDMETHOD.


  METHOD read_intf.

    DATA lt_vseointerf TYPE STANDARD TABLE OF vseointerf WITH DEFAULT KEY.

* the interface source is read separately via SVRS_GET_REPS_FROM_OBJECT, this reads the metadata.
* The type descriptions are not read, the line type of TYPE_TAB is not known yet
    CALL FUNCTION 'SVRS_GET_VERSION_INTF_40'
      EXPORTING
        object_name           = is_vrsd-objname
        versno                = is_vrsd-versno
      TABLES
        pvseointerf           = lt_vseointerf
        pvseoattrib           = rs_intf-attributes
        pvseomethod           = rs_intf-methods
        pvseoevent            = rs_intf-events
        pvseoparam            = rs_intf-parameters
        pvseoexcep            = rs_intf-exceptions
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc = 1.
      CLEAR rs_intf.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical INTF { is_vrsd-objname } version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_vseointerf INTO rs_intf-vseointerf INDEX 1.
    IF sy-subrc <> 0.
      CLEAR rs_intf.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_aff.

    DATA(ls_aff) = map_to_aff( is_intf ).

    DATA(lt_enum_mappings) = VALUE zcl_abapgit_json_handler=>ty_enum_mappings(
      ( path     = '/category'
        mappings = get_category_mappings( ) ) ).

    DATA(lt_skip_paths) = VALUE zcl_abapgit_json_handler=>ty_skip_paths(
      ( path = '/category' value = 'standard' ) ).

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

    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

* the newest version of the transport is the state it was released with
    SORT lt_vrsd BY objtype versno DESCENDING.
    READ TABLE lt_vrsd INTO DATA(ls_vrsd) WITH KEY objtype = 'INTF'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* the source and the metadata only make sense together, emit both or none
    DATA(lv_source) = zcl_abapgit_historical_source=>read_reps( ls_vrsd ).
    IF lv_source IS INITIAL.
      RETURN.
    ENDIF.
    DATA(ls_intf) = read_intf( ls_vrsd ).
    IF ls_intf IS INITIAL.
      RETURN.
    ENDIF.

* abapGit ends every source file with a newline
    INSERT VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.intf.abap|
      source   = |{ lv_source }\n| ) INTO TABLE rt_files.
    INSERT VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.intf.json|
      source   = serialize_aff( ls_intf ) ) INTO TABLE rt_files.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.intf.abap|
      deleted  = abap_true ) TO rt_files.
    APPEND VALUE #(
      filename = |{ to_lower( ms_tadir-obj_name ) }.intf.json|
      deleted  = abap_true ) TO rt_files.

  ENDMETHOD.
ENDCLASS.
