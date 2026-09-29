CLASS zcl_abapgit_historical_clas DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES BEGIN OF ty_clsd.
    TYPES vseoclass TYPE vseoclass.
    INCLUDE TYPE zcl_abapgit_historical_oo=>ty_components.
    TYPES END OF ty_clsd.

    TYPES:
      BEGIN OF ty_include,
        objname TYPE vrsd-objname,
        extra   TYPE string,
      END OF ty_include .
    TYPES ty_includes TYPE STANDARD TABLE OF ty_include WITH EMPTY KEY .

    TYPES:
      BEGIN OF ty_interfaces,
        names    TYPE zcl_abapgit_hist_clas_source=>ty_names,
        complete TYPE abap_bool,
      END OF ty_interfaces .

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .

    METHODS get_includes
      RETURNING
        VALUE(rt_includes) TYPE ty_includes .

    METHODS get_file_name
      IMPORTING
        iv_extra           TYPE string OPTIONAL
        iv_ext             TYPE string DEFAULT 'abap'
      RETURNING
        VALUE(rv_filename) TYPE string .

    METHODS read_part
      IMPORTING
        it_snapshot      TYPE zif_abapgit_historical_object=>ty_vrsd_tt
        iv_objtype       TYPE vrsd-objtype
        iv_objname       TYPE vrsd-objname
      RETURNING
        VALUE(rv_source) TYPE string .

    METHODS read_include
      IMPORTING
        it_snapshot      TYPE zif_abapgit_historical_object=>ty_vrsd_tt
        iv_objname       TYPE vrsd-objname
      RETURNING
        VALUE(rv_source) TYPE string .

    METHODS read_methods
      IMPORTING
        it_snapshot       TYPE zif_abapgit_historical_object=>ty_vrsd_tt
        is_declarations   TYPE zcl_abapgit_hist_clas_source=>ty_declarations
        iv_korrnum        TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_methods) TYPE zcl_abapgit_hist_clas_source=>ty_methods .

    METHODS resolve_interfaces
      IMPORTING
        it_interfaces        TYPE zcl_abapgit_hist_clas_source=>ty_names
        iv_korrnum           TYPE vrsd-korrnum
      RETURNING
        VALUE(rs_interfaces) TYPE ty_interfaces .

    METHODS read_clsd
      IMPORTING
        is_vrsd        TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_clsd) TYPE ty_clsd
      RAISING
        zcx_abapgit_exception .

    METHODS map_to_aff
      IMPORTING
        is_clsd       TYPE ty_clsd
      RETURNING
        VALUE(rs_aff) TYPE zif_abapgit_aff_clas_v1=>ty_main
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_aff
      IMPORTING
        is_clsd        TYPE ty_clsd
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS get_category_mappings
      RETURNING
        VALUE(rt_mappings) TYPE zcl_abapgit_json_handler=>ty_json_abap_mappings .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_CLAS IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

    DATA(lv_class) = CONV vrsd-objname( ms_tadir-obj_name ).

    LOOP AT VALUE zif_abapgit_historical_object=>ty_parts_tt(
        ( objtype = 'CLSD' objname = lv_class )
        ( objtype = 'CPUB' objname = lv_class )
        ( objtype = 'CPRO' objname = lv_class )
        ( objtype = 'CPRI' objname = lv_class ) ) INTO DATA(ls_part).
      ls_part-type = ms_tadir-object.
      ls_part-name = ms_tadir-obj_name.
      ls_part-devclass = ms_tadir-devclass.
      APPEND ls_part TO rt_parts.
    ENDLOOP.

* the local definitions are versioned as CDEF, and as CINC on some releases, like the other includes
    LOOP AT get_includes( ) INTO DATA(ls_include).
      APPEND VALUE #(
        objtype  = 'CINC'
        objname  = ls_include-objname
        type     = ms_tadir-object
        name     = ms_tadir-obj_name
        devclass = ms_tadir-devclass ) TO rt_parts.
      IF ls_include-extra = 'definitions'.
        APPEND VALUE #(
          objtype  = 'CDEF'
          objname  = ls_include-objname
          type     = ms_tadir-object
          name     = ms_tadir-obj_name
          devclass = ms_tadir-devclass ) TO rt_parts.
      ENDIF.
    ENDLOOP.

* every method ever versioned, removed ones are dropped later against the declarations.
* The name is the class name padded to 30 characters followed by the method name
    DATA(lv_pattern) = |{ ms_tadir-obj_name WIDTH = 30 }%|.
    SELECT DISTINCT objtype, objname FROM vrsd INTO TABLE @DATA(lt_methods)
      WHERE objtype = 'METH'
      AND objname LIKE @lv_pattern
      ORDER BY objtype, objname.
    LOOP AT lt_methods INTO DATA(ls_method).
* an underscore in the class name is a LIKE wildcard, so other classes can match
      IF ls_method-objname(30) <> lv_class(30).
        CONTINUE.
      ENDIF.
      APPEND VALUE #(
        objtype  = ls_method-objtype
        objname  = ls_method-objname
        type     = ms_tadir-object
        name     = ms_tadir-obj_name
        devclass = ms_tadir-devclass ) TO rt_parts.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_category_mappings.

    rt_mappings = VALUE #(
      ( abap = zif_abapgit_aff_clas_v1=>co_category-general_object_type
        json = 'generalObjectType' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-exit_class
        json = 'exitClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-testclass_abap_unit
        json = 'testclassAbapUnit' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-behavior_class
        json = 'behaviorClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-entity_event_handler
        json = 'entityEventHandler' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-persistent_class
        json = 'persistentClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-factory_for_persistent_class
        json = 'factoryForPersistentClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-status_class_for_persist_class
        json = 'statusClassForPersistClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-rfc_proxy_class
        json = 'rfcProxyClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-communication_connection_class
        json = 'communicationConnectionClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-exception_class
        json = 'exceptionClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-area_class_shared_objects
        json = 'areaClassSharedObjects' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-business_class
        json = 'businessClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-bsp_application_class
        json = 'bspApplicationClass' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-basis_class_bsp_element_hdlr
        json = 'basisClassBspElementHdlr' )
      ( abap = zif_abapgit_aff_clas_v1=>co_category-web_dynpro_runtime_object
        json = 'webDynproRuntimeObject' ) ).

  ENDMETHOD.


  METHOD get_file_name.

    rv_filename = to_lower( ms_tadir-obj_name ) && '.clas'.
    IF iv_extra IS NOT INITIAL.
      rv_filename = |{ rv_filename }.{ iv_extra }|.
    ENDIF.
    rv_filename = |{ rv_filename }.{ iv_ext }|.

  ENDMETHOD.


  METHOD get_includes.

* file names of the ABAP file format, abapGit's classic format calls the first two locals_def and locals_imp
    DATA(lv_class) = CONV seoclsname( ms_tadir-obj_name ).
    rt_includes = VALUE #(
      ( objname = cl_oo_classname_service=>get_ccdef_name( lv_class ) extra = 'definitions' )
      ( objname = cl_oo_classname_service=>get_ccimp_name( lv_class ) extra = 'implementations' )
      ( objname = cl_oo_classname_service=>get_ccmac_name( lv_class ) extra = 'macros' )
      ( objname = cl_oo_classname_service=>get_ccau_name( lv_class ) extra = 'testclasses' ) ).

  ENDMETHOD.


  METHOD map_to_aff.

    rs_aff-format_version = '1'.
    rs_aff-header-description = is_clsd-vseoclass-descript.
    IF is_clsd-vseoclass-langu IS INITIAL.
      zcx_abapgit_exception=>raise( |Original language is missing in class { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-header-original_language = is_clsd-vseoclass-langu.
    rs_aff-header-abap_language_version = zcl_abapgit_historical_oo=>map_abap_language_version(
      is_clsd-vseoclass-unicode ).

    rs_aff-category = is_clsd-vseoclass-category.
    DATA(lt_mappings) = get_category_mappings( ).
    DATA(lv_category) = CONV string( rs_aff-category ).
    IF NOT line_exists( lt_mappings[ abap = lv_category ] ).
      zcx_abapgit_exception=>raise(
        |Unsupported category { is_clsd-vseoclass-category } in class { ms_tadir-obj_name }| ).
    ENDIF.
    rs_aff-fix_point_arithmetic = is_clsd-vseoclass-fixpt.
    rs_aff-message_class = is_clsd-vseoclass-msg_id.

    rs_aff-descriptions = zcl_abapgit_historical_oo=>map_descriptions(
      is_components = CORRESPONDING #( is_clsd )
      iv_language   = is_clsd-vseoclass-langu ).

  ENDMETHOD.


  METHOD read_clsd.

    DATA lt_vseoclass TYPE STANDARD TABLE OF vseoclass WITH DEFAULT KEY.
    DATA lv_subrc     TYPE sy-subrc.

* the signature of this reader is not confirmed on a system, it follows the naming of
* SVRS_GET_VERSION_INTF_40. A wrong guess raises CX_SY_DYN_CALL_*, first the component
* tables are dropped, then the metadata file is skipped
    TRY.
        CALL FUNCTION 'SVRS_GET_VERSION_CLSD_40'
          EXPORTING
            object_name           = is_vrsd-objname
            versno                = is_vrsd-versno
          TABLES
            pvseoclass            = lt_vseoclass
            pvseoattrib           = rs_clsd-attributes
            pvseomethod           = rs_clsd-methods
            pvseoevent            = rs_clsd-events
            pvseoparam            = rs_clsd-parameters
            pvseoexcep            = rs_clsd-exceptions
          EXCEPTIONS
            no_version            = 1
            system_failure        = 2
            communication_failure = 3
            OTHERS                = 4.
        lv_subrc = sy-subrc.
      CATCH cx_sy_dyn_call_error.
        CLEAR rs_clsd.
        CLEAR lt_vseoclass.
        TRY.
            CALL FUNCTION 'SVRS_GET_VERSION_CLSD_40'
              EXPORTING
                object_name           = is_vrsd-objname
                versno                = is_vrsd-versno
              TABLES
                pvseoclass            = lt_vseoclass
              EXCEPTIONS
                no_version            = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.
            lv_subrc = sy-subrc.
          CATCH cx_sy_dyn_call_error.
            RETURN.
        ENDTRY.
    ENDTRY.

    IF lv_subrc = 1.
      CLEAR rs_clsd.
      RETURN.
    ELSEIF lv_subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical CLSD { is_vrsd-objname } version { is_vrsd-versno }, subrc { lv_subrc }| ).
    ENDIF.

* a header without language means the guessed reader returned something else
    READ TABLE lt_vseoclass INTO rs_clsd-vseoclass INDEX 1.
    IF sy-subrc <> 0 OR rs_clsd-vseoclass-langu IS INITIAL.
      CLEAR rs_clsd.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD read_include.

* the local definitions can have both a CDEF and a CINC version, the newest one wins
    DATA ls_newest LIKE LINE OF it_snapshot.

    LOOP AT it_snapshot INTO DATA(ls_vrsd) WHERE objname = iv_objname.
      IF ls_vrsd-datum > ls_newest-datum
          OR ( ls_vrsd-datum = ls_newest-datum AND ls_vrsd-zeit >= ls_newest-zeit ).
        ls_newest = ls_vrsd.
      ENDIF.
    ENDLOOP.
    IF ls_newest IS NOT INITIAL.
      rv_source = zcl_abapgit_historical_source=>read_reps( ls_newest ).
    ENDIF.

  ENDMETHOD.


  METHOD read_methods.

    DATA ls_interfaces TYPE ty_interfaces.
    DATA lv_resolved   TYPE abap_bool.

    LOOP AT it_snapshot INTO DATA(ls_vrsd) WHERE objtype = 'METH'.
      DATA(lv_method) = condense( CONV string( ls_vrsd-objname+30 ) ).

      IF zcl_abapgit_hist_clas_source=>is_declared(
          is_declarations = is_declarations
          iv_method       = lv_method ) = abap_false.
* a method of neither the class nor a declared interface was removed before this transport
        IF lv_method NS '~'.
          CONTINUE.
        ENDIF.
* it can still belong to an interface that a declared interface includes
        IF lv_resolved = abap_false.
          ls_interfaces = resolve_interfaces(
            it_interfaces = is_declarations-interfaces
            iv_korrnum    = iv_korrnum ).
          lv_resolved = abap_true.
        ENDIF.
        DATA(lv_interface) = segment( val   = lv_method
                                      sep   = '~'
                                      index = 1 ).
        IF ls_interfaces-complete = abap_true
            AND NOT line_exists( ls_interfaces-names[ table_line = lv_interface ] ).
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA(lv_source) = zcl_abapgit_historical_source=>read_reps( ls_vrsd ).
      IF lv_source IS NOT INITIAL.
        INSERT VALUE #(
          name   = lv_method
          source = lv_source ) INTO TABLE rt_methods.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_part.

    READ TABLE it_snapshot INTO DATA(ls_vrsd) WITH KEY objtype = iv_objtype objname = iv_objname.
    IF sy-subrc = 0.
      rv_source = zcl_abapgit_historical_source=>read_reps( ls_vrsd ).
    ENDIF.

  ENDMETHOD.


  METHOD resolve_interfaces.

* the interfaces included by the declared ones, read as of the same transport. When one of them
* has no version, for example a SAP interface, membership cannot be decided and nothing is dropped
    DATA lt_todo TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    rs_interfaces-names = it_interfaces.
    rs_interfaces-complete = abap_true.
    APPEND LINES OF it_interfaces TO lt_todo.

    WHILE lines( lt_todo ) > 0.
      DATA(lv_interface) = lt_todo[ 1 ].
      DELETE lt_todo INDEX 1.

      DATA(lt_snapshot) = zcl_abapgit_historical_source=>read_snapshot(
        it_parts   = VALUE #( ( objtype = 'INTF' objname = lv_interface ) )
        iv_korrnum = iv_korrnum ).
      DATA(lv_source) = read_part(
        it_snapshot = lt_snapshot
        iv_objtype  = 'INTF'
        iv_objname  = CONV #( lv_interface ) ).
      IF lv_source IS INITIAL.
        rs_interfaces-complete = abap_false.
        CONTINUE.
      ENDIF.

      LOOP AT zcl_abapgit_hist_clas_source=>parse_declarations( lv_source )-interfaces INTO DATA(lv_nested).
        INSERT lv_nested INTO TABLE rs_interfaces-names.
        IF sy-subrc = 0.
          APPEND lv_nested TO lt_todo.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.


  METHOD serialize_aff.

    DATA(ls_aff) = map_to_aff( is_clsd ).

    DATA(lt_enum_mappings) = VALUE zcl_abapgit_json_handler=>ty_enum_mappings(
      ( path     = '/category'
        mappings = get_category_mappings( ) ) ).

    DATA(lt_skip_paths) = VALUE zcl_abapgit_json_handler=>ty_skip_paths(
      ( path = '/category' value = 'generalObjectType' ) ).

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

    DATA(lv_class) = CONV vrsd-objname( ms_tadir-obj_name ).

* a transport usually carries only the parts that changed, so the class is rebuilt from the
* newest version of every part as of this transport
    DATA(lt_snapshot) = zcl_abapgit_historical_source=>read_snapshot(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).
    IF NOT line_exists( lt_snapshot[ korrnum = iv_korrnum ] ).
      RETURN.
    ENDIF.

    DATA(lv_public) = read_part(
      it_snapshot = lt_snapshot
      iv_objtype  = 'CPUB'
      iv_objname  = lv_class ).
    IF lv_public IS INITIAL.
* without the public section, which holds the CLASS DEFINITION statement, there is no class
      RETURN.
    ENDIF.
    DATA(lv_protected) = read_part(
      it_snapshot = lt_snapshot
      iv_objtype  = 'CPRO'
      iv_objname  = lv_class ).
    DATA(lv_private) = read_part(
      it_snapshot = lt_snapshot
      iv_objtype  = 'CPRI'
      iv_objname  = lv_class ).

    DATA(ls_declarations) = zcl_abapgit_hist_clas_source=>parse_declarations(
      |{ lv_public }\n{ lv_protected }\n{ lv_private }| ).

    INSERT VALUE #(
      filename = get_file_name( )
      source   = zcl_abapgit_hist_clas_source=>build_main_source(
        iv_class_name = CONV #( ms_tadir-obj_name )
        iv_public     = lv_public
        iv_protected  = lv_protected
        iv_private    = lv_private
        it_methods    = read_methods(
          it_snapshot     = lt_snapshot
          is_declarations = ls_declarations
          iv_korrnum      = iv_korrnum ) ) ) INTO TABLE rt_files.

* an include that is empty at this point in history removes the file of an earlier transport
    LOOP AT get_includes( ) INTO DATA(ls_include).
      DATA(lv_source) = read_include(
        it_snapshot = lt_snapshot
        iv_objname  = ls_include-objname ).
      IF zcl_abapgit_hist_clas_source=>has_content( lv_source ) = abap_true.
        INSERT VALUE #(
          filename = get_file_name( iv_extra = ls_include-extra )
          source   = |{ lv_source }\n| ) INTO TABLE rt_files.
      ELSE.
        INSERT VALUE #(
          filename = get_file_name( iv_extra = ls_include-extra )
          deleted  = abap_true ) INTO TABLE rt_files.
      ENDIF.
    ENDLOOP.

* the metadata file is written only when the class definition reader delivers a header
    READ TABLE lt_snapshot INTO DATA(ls_clsd_vrsd) WITH KEY objtype = 'CLSD' objname = lv_class.
    IF sy-subrc = 0.
      DATA(ls_clsd) = read_clsd( ls_clsd_vrsd ).
      IF ls_clsd IS NOT INITIAL.
        INSERT VALUE #(
          filename = get_file_name( iv_ext = 'json' )
          source   = serialize_aff( ls_clsd ) ) INTO TABLE rt_files.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

    APPEND VALUE #(
      filename = get_file_name( )
      deleted  = abap_true ) TO rt_files.
    APPEND VALUE #(
      filename = get_file_name( iv_ext = 'json' )
      deleted  = abap_true ) TO rt_files.
    LOOP AT get_includes( ) INTO DATA(ls_include).
      APPEND VALUE #(
        filename = get_file_name( iv_extra = ls_include-extra )
        deleted  = abap_true ) TO rt_files.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
