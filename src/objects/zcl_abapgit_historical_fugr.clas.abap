CLASS zcl_abapgit_historical_fugr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_historical_object .

    METHODS constructor
      IMPORTING
        is_tadir TYPE zif_abapgit_definitions=>ty_tadir .

  PROTECTED SECTION.
  PRIVATE SECTION.

* a function module as zcl_abapgit_object_fugr writes it
    TYPES:
      BEGIN OF ty_function,
        funcname          TYPE rs38l_fnam,
        global_flag       TYPE rs38l-global,
        remote_call       TYPE rs38l-remote,
        update_task       TYPE rs38l-utask,
        short_text        TYPE tftit-stext,
        remote_basxml     TYPE rs38l-basxml_enabled,
        rfcscope          TYPE c LENGTH 1,
        rfcvers           TYPE c LENGTH 10,
        import            TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY,
        changing          TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY,
        export            TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY,
        tables            TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY,
        exception         TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY,
        documentation     TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY,
        exception_classes TYPE abap_bool,
      END OF ty_function .
    TYPES ty_functions TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY .
* the attributes of a function module version, read by name as they differ between releases
    TYPES:
      BEGIN OF ty_attributes,
        area     TYPE string,
        pname    TYPE string,
        global   TYPE string,
        exten3   TYPE string,
        fmode    TYPE string,
        utask    TYPE string,
        rfcscope TYPE string,
        rfcvers  TYPE string,
      END OF ty_attributes .
    TYPES:
      BEGIN OF ty_module,
        vrsd     TYPE zif_abapgit_historical_object=>ty_vrsd,
        area     TYPE string,
        program  TYPE string,
        source   TYPE string,
        function TYPE ty_function,
      END OF ty_module .
    TYPES ty_modules TYPE STANDARD TABLE OF ty_module WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_include,
        name    TYPE string,
        program TYPE zcl_abapgit_historical_source=>ty_program,
        tpool   TYPE textpool_table,
      END OF ty_include .
    TYPES ty_includes TYPE STANDARD TABLE OF ty_include WITH EMPTY KEY .
    TYPES ty_sobj_names TYPE STANDARD TABLE OF sobj_name WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_deleted_function,
        funcname TYPE rs38l_fnam,
        area     TYPE string,
      END OF ty_deleted_function .
    TYPES ty_deleted_functions TYPE SORTED TABLE OF ty_deleted_function WITH UNIQUE KEY funcname .

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .
    DATA mv_group TYPE string .
    DATA mv_main TYPE string .

    CLASS-DATA gt_deleted_functions TYPE ty_deleted_functions .
    CLASS-DATA gv_deleted_functions_read TYPE abap_bool .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt
      RAISING
        zcx_abapgit_exception .

    METHODS get_function_candidates
      RETURNING
        VALUE(rt_functions) TYPE ty_deleted_functions
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS get_deleted_functions
      RETURNING
        VALUE(rt_functions) TYPE ty_deleted_functions
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS read_function
      IMPORTING
        is_vrsd          TYPE zif_abapgit_historical_object=>ty_vrsd
        iv_language      TYPE sy-langu
      RETURNING
        VALUE(rs_module) TYPE ty_module
      RAISING
        zcx_abapgit_exception .

    METHODS read_functions
      IMPORTING
        it_snapshot       TYPE zif_abapgit_historical_object=>ty_vrsd_tt
        iv_korrnum        TYPE vrsd-korrnum
        iv_language       TYPE sy-langu
      RETURNING
        VALUE(rt_modules) TYPE ty_modules
      RAISING
        zcx_abapgit_exception .

    METHODS remove_deleted_functions
      IMPORTING
        iv_korrnum TYPE vrsd-korrnum
      CHANGING
        ct_modules TYPE ty_modules .

    METHODS read_includes
      IMPORTING
        it_snapshot        TYPE zif_abapgit_historical_object=>ty_vrsd_tt
        iv_korrnum         TYPE vrsd-korrnum
        it_sources         TYPE string_table
      RETURNING
        VALUE(rt_includes) TYPE ty_includes
      RAISING
        zcx_abapgit_exception .

    METHODS filter_own_programs
      CHANGING
        ct_names TYPE zcl_abapgit_hist_fugr_source=>ty_names .

    METHODS read_areat
      IMPORTING
        iv_language     TYPE sy-langu
      RETURNING
        VALUE(rv_areat) TYPE tlibt-areat .

    METHODS get_original_language
      IMPORTING
        is_trdir           TYPE trdir
      RETURNING
        VALUE(rv_language) TYPE sy-langu .

    METHODS get_file_name
      IMPORTING
        iv_extra           TYPE csequence OPTIONAL
        iv_ext             TYPE string
      RETURNING
        VALUE(rv_filename) TYPE string .

    METHODS build_file_prefix
      RETURNING
        VALUE(rv_prefix) TYPE string .

    METHODS serialize_xml
      IMPORTING
        iv_areat      TYPE tlibt-areat
        it_includes   TYPE ty_includes
        it_modules    TYPE ty_modules
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS serialize_include_xml
      IMPORTING
        is_include    TYPE ty_include
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_FUGR IMPLEMENTATION.


  METHOD build_file_prefix.

    rv_prefix = |{ get_file_name( `` ) }.|.

  ENDMETHOD.


  METHOD constructor.

    ms_tadir = is_tadir.
    mv_group = to_upper( is_tadir-obj_name ).
    mv_main = zcl_abapgit_hist_fugr_source=>get_main_program( mv_group ).

  ENDMETHOD.


  METHOD determine_parts.

    DATA(lv_main) = CONV vrsd-objname( mv_main ).

* the main program carries the attributes of the group and its text pool
    LOOP AT VALUE zif_abapgit_historical_object=>ty_parts_tt(
        ( objtype = 'REPS' objname = lv_main )
        ( objtype = 'REPT' objname = lv_main ) ) INTO DATA(ls_part).
      ls_part-type = ms_tadir-object.
      ls_part-name = ms_tadir-obj_name.
      ls_part-devclass = ms_tadir-devclass.
      INSERT ls_part INTO TABLE rt_parts.
    ENDLOOP.

* every include ever versioned under the name prefix of the group, those not included as of
* the transport are dropped later
    DATA(lv_prefix) = zcl_abapgit_hist_fugr_source=>get_include_prefix( mv_group ).
    DATA(lv_pattern) = |{ lv_prefix }%|.
    SELECT DISTINCT objtype, objname FROM vrsd INTO TABLE @DATA(lt_includes)
      WHERE objtype = 'REPS'
      AND objname LIKE @lv_pattern
      ORDER BY objtype, objname.
    LOOP AT lt_includes INTO DATA(ls_include).
* an underscore in the group name is a LIKE wildcard, so includes of other groups can match
      IF strlen( ls_include-objname ) < strlen( lv_prefix )
          OR substring( val = ls_include-objname
                        len = strlen( lv_prefix ) ) <> lv_prefix.
        CONTINUE.
      ENDIF.
      INSERT VALUE #(
        objtype  = ls_include-objtype
        objname  = ls_include-objname
        type     = ms_tadir-object
        name     = ms_tadir-obj_name
        devclass = ms_tadir-devclass ) INTO TABLE rt_parts.
    ENDLOOP.

    LOOP AT get_function_candidates( ) INTO DATA(ls_function).
      INSERT VALUE #(
        objtype  = 'FUNC'
        objname  = ls_function-funcname
        type     = ms_tadir-object
        name     = ms_tadir-obj_name
        devclass = ms_tadir-devclass ) INTO TABLE rt_parts.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_own_programs.

    DATA lt_names    TYPE STANDARD TABLE OF tadir-obj_name WITH EMPTY KEY.
    DATA lt_programs TYPE SORTED TABLE OF tadir-obj_name WITH UNIQUE KEY table_line.

* an include with its own TADIR entry is a program of its own, abapGit leaves it out of the group
    IF lines( ct_names ) = 0.
      RETURN.
    ENDIF.

    lt_names = ct_names.
    SELECT obj_name FROM tadir INTO TABLE @lt_programs
      FOR ALL ENTRIES IN @lt_names
      WHERE pgmid = 'R3TR'
      AND object = 'PROG'
      AND obj_name = @lt_names-table_line.
    LOOP AT lt_programs INTO DATA(lv_program).
      DELETE ct_names WHERE table_line = lv_program.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_deleted_functions.

    DATA lt_names    TYPE STANDARD TABLE OF tfdir-funcname WITH EMPTY KEY.
    DATA lt_existing TYPE SORTED TABLE OF tfdir-funcname WITH UNIQUE KEY table_line.
    DATA lt_objnames TYPE STANDARD TABLE OF vrsd-objname WITH EMPTY KEY.
    DATA lt_versions TYPE SORTED TABLE OF zif_abapgit_historical_object=>ty_vrsd
      WITH NON-UNIQUE KEY objname versno.
    DATA lt_newest   TYPE SORTED TABLE OF zif_abapgit_historical_object=>ty_vrsd
      WITH UNIQUE KEY objname.
    DATA lt_deleted  TYPE ty_deleted_functions.

* read once per run: the function modules recorded in a request that do not exist anymore,
* each with the group of its newest version
    IF gv_deleted_functions_read = abap_true.
      rt_functions = gt_deleted_functions.
      RETURN.
    ENDIF.

    SELECT DISTINCT obj_name FROM e071 INTO TABLE @DATA(lt_recorded)
      WHERE pgmid = 'LIMU'
      AND object = 'FUNC'
      ORDER BY obj_name.
    LOOP AT lt_recorded INTO DATA(lv_recorded).
      INSERT CONV tfdir-funcname( lv_recorded ) INTO TABLE lt_names.
    ENDLOOP.

    IF lines( lt_names ) > 0.
      SELECT funcname FROM tfdir INTO TABLE @lt_existing
        FOR ALL ENTRIES IN @lt_names
        WHERE funcname = @lt_names-table_line.
      LOOP AT lt_existing INTO DATA(lv_existing).
        DELETE lt_names WHERE table_line = lv_existing.
      ENDLOOP.
    ENDIF.

* FOR ALL ENTRIES needs the type of the column, which differs between TFDIR and VRSD
    lt_objnames = lt_names.
    IF lines( lt_objnames ) > 0.
      SELECT objtype, objname, versno, korrnum FROM vrsd INTO CORRESPONDING FIELDS OF TABLE @lt_versions
        FOR ALL ENTRIES IN @lt_objnames
        WHERE objtype = 'FUNC'
        AND objname = @lt_objnames-table_line.
    ENDIF.

* rows come in ascending version order, so the last row kept per function module is its newest version
    LOOP AT lt_versions INTO DATA(ls_version).
      DELETE TABLE lt_newest WITH TABLE KEY objname = ls_version-objname.
      INSERT ls_version INTO TABLE lt_newest.
    ENDLOOP.

    LOOP AT lt_newest INTO ls_version.
      DATA(ls_module) = read_function( is_vrsd     = ls_version
                                       iv_language = sy-langu ).
      IF ls_module-area IS NOT INITIAL.
        INSERT VALUE #(
          funcname = ls_version-objname
          area     = ls_module-area ) INTO TABLE lt_deleted.
      ENDIF.
    ENDLOOP.

* a reader failure raises before the cache is filled, so it is not taken as complete
    gt_deleted_functions = lt_deleted.
    gv_deleted_functions_read = abap_true.
    rt_functions = gt_deleted_functions.

  ENDMETHOD.


  METHOD get_file_name.

    rv_filename = zcl_abapgit_filename_logic=>object_to_file(
      is_item  = VALUE #(
        obj_type = ms_tadir-object
        obj_name = ms_tadir-obj_name )
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

  ENDMETHOD.


  METHOD get_function_candidates.

* the current members, plus the function modules deleted since that belonged to the group. A
* function module moved to another group since is not found
    SELECT funcname FROM enlfdir INTO TABLE @DATA(lt_current)
      WHERE area = @ms_tadir-obj_name
      ORDER BY funcname.
    LOOP AT lt_current INTO DATA(lv_current).
      INSERT VALUE #(
        funcname = to_upper( lv_current-funcname )
        area     = mv_group ) INTO TABLE rt_functions.
    ENDLOOP.

    LOOP AT get_deleted_functions( ) INTO DATA(ls_deleted) WHERE area = mv_group.
      INSERT ls_deleted INTO TABLE rt_functions.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_original_language.

* a group without original language is taken as English, as programs are
    rv_language = is_trdir-rload.
    IF rv_language IS INITIAL.
      rv_language = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD read_areat.

* the short text of the group is not versioned, so it is the current one
    SELECT SINGLE areat FROM tlibt INTO @rv_areat
      WHERE spras = @iv_language
      AND area = @ms_tadir-obj_name.
    IF sy-subrc <> 0.
      CLEAR rv_areat.
    ENDIF.

  ENDMETHOD.


  METHOD read_function.

    DATA lt_source  TYPE STANDARD TABLE OF abaptxt255 WITH DEFAULT KEY.
    DATA lt_enlfdir TYPE STANDARD TABLE OF enlfdir WITH DEFAULT KEY.
    DATA lt_tfdir   TYPE STANDARD TABLE OF tfdir WITH DEFAULT KEY.
    DATA lt_tftit   TYPE STANDARD TABLE OF tftit WITH DEFAULT KEY.
    DATA lt_funct   TYPE STANDARD TABLE OF funct WITH DEFAULT KEY.
    DATA lt_fupar   TYPE STANDARD TABLE OF sfupararef WITH DEFAULT KEY.
    DATA ls_attributes TYPE ty_attributes.
    DATA lt_texts   TYPE zcl_abapgit_hist_fugr_source=>ty_texts.


* the table parameters are taken from public code calling this reader, not confirmed on a system
    CALL FUNCTION 'SVRS_GET_VERSION_FUNC_40'
      EXPORTING
        object_name           = is_vrsd-objname
        versno                = is_vrsd-versno
      TABLES
        uincl_tab             = lt_source
        enlfd_tab             = lt_enlfdir
        funct_tab             = lt_funct
        fupar_tab             = lt_fupar
        tfdir_tab             = lt_tfdir
        tftit_tab             = lt_tftit
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Unable to read historical FUNC { is_vrsd-objname } version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_enlfdir INTO DATA(ls_enlfdir) INDEX 1.
    IF sy-subrc = 0.
      ls_attributes = CORRESPONDING #( ls_enlfdir ).
    ENDIF.
    READ TABLE lt_tfdir INTO DATA(ls_tfdir) INDEX 1.
    IF sy-subrc = 0.
      ls_attributes = CORRESPONDING #( BASE ( ls_attributes ) ls_tfdir ).
    ENDIF.

    rs_module-vrsd = is_vrsd.
    rs_module-area = to_upper( ls_attributes-area ).
    rs_module-program = to_upper( ls_attributes-pname ).
    rs_module-source = zcl_abapgit_hist_fugr_source=>strip_generation_comments(
      concat_lines_of( table = lt_source
                       sep   = |\n| ) ).

    rs_module-function-funcname = is_vrsd-objname.
    rs_module-function-global_flag = ls_attributes-global.
    rs_module-function-remote_call = ls_attributes-fmode.
    rs_module-function-update_task = ls_attributes-utask.
    rs_module-function-rfcscope = ls_attributes-rfcscope.
    rs_module-function-rfcvers = ls_attributes-rfcvers.
    rs_module-function-exception_classes = ls_attributes-exten3.

* only the original language is extracted, translations are out of scope
    LOOP AT lt_tftit INTO DATA(ls_tftit) WHERE spras = iv_language.
      rs_module-function-short_text = ls_tftit-stext.
    ENDLOOP.
    LOOP AT lt_funct INTO DATA(ls_funct) WHERE spras = iv_language.
      INSERT CORRESPONDING zcl_abapgit_hist_fugr_source=>ty_text( ls_funct ) INTO TABLE lt_texts.
    ENDLOOP.

    DATA(ls_interface) = zcl_abapgit_hist_fugr_source=>map_interface(
      it_parameters = CORRESPONDING #( lt_fupar )
      it_texts      = lt_texts ).
    rs_module-function-import = CORRESPONDING #( ls_interface-import ).
    rs_module-function-changing = CORRESPONDING #( ls_interface-changing ).
    rs_module-function-export = CORRESPONDING #( ls_interface-export ).
    rs_module-function-tables = CORRESPONDING #( ls_interface-tables ).
    rs_module-function-exception = CORRESPONDING #( ls_interface-exception ).
    rs_module-function-documentation = CORRESPONDING #( ls_interface-documentation ).

  ENDMETHOD.


  METHOD read_functions.

    LOOP AT it_snapshot INTO DATA(ls_vrsd) WHERE objtype = 'FUNC'.
      DATA(ls_module) = read_function( is_vrsd     = ls_vrsd
                                       iv_language = iv_language ).
* a function module that belonged to another group at this point in history
      IF ls_module IS INITIAL
          OR ( ls_module-area <> mv_group AND ls_module-program <> mv_main ).
        CONTINUE.
      ENDIF.
      INSERT ls_module INTO TABLE rt_modules.
    ENDLOOP.

    remove_deleted_functions(
      EXPORTING
        iv_korrnum = iv_korrnum
      CHANGING
        ct_modules = rt_modules ).

    SORT rt_modules BY function-funcname.

  ENDMETHOD.


  METHOD read_includes.

    DATA lt_found     TYPE zcl_abapgit_hist_fugr_source=>ty_names.
    DATA lt_done      TYPE zcl_abapgit_hist_fugr_source=>ty_names.
    DATA lt_new       TYPE zcl_abapgit_hist_fugr_source=>ty_names.
    DATA lt_parts     TYPE zif_abapgit_historical_object=>ty_parts_tt.
    DATA ls_include   TYPE ty_include.

    DATA(lt_snapshot) = it_snapshot.

* the includes reachable through INCLUDE statements as of the transport, as RS_GET_ALL_INCLUDES
* returns them, plus the include of a maintenance view which abapGit always adds
    LOOP AT it_sources INTO DATA(lv_source).
      INSERT LINES OF zcl_abapgit_hist_fugr_source=>parse_includes( lv_source ) INTO TABLE lt_found.
    ENDLOOP.
    INSERT |{ zcl_abapgit_hist_fugr_source=>get_include_prefix( mv_group ) }T00| INTO TABLE lt_found.

    DO.
      CLEAR lt_new.
      LOOP AT lt_found INTO DATA(lv_name).
        INSERT lv_name INTO TABLE lt_done.
        IF sy-subrc = 0
            AND lv_name <> mv_main
            AND zcl_abapgit_hist_fugr_source=>is_group_include(
              iv_group   = mv_group
              iv_include = lv_name ) = abap_true.
          INSERT lv_name INTO TABLE lt_new.
        ENDIF.
      ENDLOOP.
      CLEAR lt_found.
      filter_own_programs( CHANGING ct_names = lt_new ).
      IF lines( lt_new ) = 0.
        EXIT.
      ENDIF.

* an include outside the name prefix of the group has no version in the snapshot yet
      CLEAR lt_parts.
      LOOP AT lt_new INTO lv_name.
        IF NOT line_exists( lt_snapshot[ objtype = 'REPS' objname = lv_name ] ).
          INSERT VALUE #( objtype = 'REPS' objname = lv_name ) INTO TABLE lt_parts.
        ENDIF.
      ENDLOOP.
      INSERT LINES OF zcl_abapgit_historical_source=>read_snapshot(
        it_parts   = lt_parts
        iv_korrnum = iv_korrnum ) INTO TABLE lt_snapshot.

      LOOP AT lt_new INTO lv_name.
        READ TABLE lt_snapshot INTO DATA(ls_vrsd) WITH KEY objtype = 'REPS' objname = lv_name.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        CLEAR ls_include.
        ls_include-name = lv_name.
        ls_include-program = zcl_abapgit_historical_source=>read_program( ls_vrsd ).
        IF ls_include-program IS INITIAL.
          CONTINUE.
        ENDIF.
        ls_include-program-source = zcl_abapgit_hist_fugr_source=>strip_generation_comments(
          ls_include-program-source ).
        INSERT ls_include INTO TABLE rt_includes.
        INSERT LINES OF zcl_abapgit_hist_fugr_source=>parse_includes( ls_include-program-source )
          INTO TABLE lt_found.
      ENDLOOP.
    ENDDO.

  ENDMETHOD.


  METHOD remove_deleted_functions.

    TYPES:
      BEGIN OF ty_name,
        obj_name TYPE e071-obj_name,
        objname  TYPE vrsd-objname,
      END OF ty_name.
    TYPES:
      BEGIN OF ty_request,
        obj_name TYPE e071-obj_name,
        trkorr   TYPE e070-trkorr,
        as4date  TYPE e070-as4date,
        as4time  TYPE e070-as4time,
      END OF ty_request.
    DATA lt_names    TYPE STANDARD TABLE OF ty_name WITH EMPTY KEY.
    DATA lt_requests TYPE SORTED TABLE OF ty_request WITH NON-UNIQUE KEY obj_name trkorr.
    DATA lt_versions TYPE SORTED TABLE OF zif_abapgit_historical_object=>ty_vrsd
      WITH NON-UNIQUE KEY objname korrnum.
    DATA lt_removed  TYPE SORTED TABLE OF vrsd-objname WITH UNIQUE KEY table_line.

    IF lines( ct_modules ) = 0.
      RETURN.
    ENDIF.

    LOOP AT ct_modules INTO DATA(ls_module).
      INSERT VALUE #(
        obj_name = ls_module-vrsd-objname
        objname  = ls_module-vrsd-objname ) INTO TABLE lt_names.
    ENDLOOP.

* releasing a request versions every function module it records that still exists, so a
* request recording the function module without a version of it deleted the function module
    SELECT e071~obj_name, e070~trkorr, e070~as4date, e070~as4time
      FROM e071 INNER JOIN e070 ON e070~trkorr = e071~trkorr
      INTO TABLE @lt_requests
      FOR ALL ENTRIES IN @lt_names
      WHERE e071~pgmid = 'LIMU'
      AND e071~object = 'FUNC'
      AND e071~obj_name = @lt_names-obj_name
      AND e070~strkorr = ''
      AND e070~trstatus = @zif_abapgit_cts_api=>c_transport_status-released
      AND e070~trfunction = @zif_abapgit_cts_api=>c_transport_type-wb_request.
    IF lines( lt_requests ) = 0.
      RETURN.
    ENDIF.

    SELECT objtype, objname, versno, korrnum FROM vrsd INTO CORRESPONDING FIELDS OF TABLE @lt_versions
      FOR ALL ENTRIES IN @lt_names
      WHERE objtype = 'FUNC'
      AND objname = @lt_names-objname.

    DATA(ls_transport) = zcl_abapgit_historical_source=>read_release( iv_korrnum ).

    LOOP AT ct_modules INTO ls_module.
      DATA(ls_version) = VALUE zcl_abapgit_historical_source=>ty_release(
        trkorr  = ls_module-vrsd-korrnum
        as4date = ls_module-vrsd-as4date
        as4time = ls_module-vrsd-as4time ).
      LOOP AT lt_requests INTO DATA(ls_request) WHERE obj_name = ls_module-vrsd-objname.
        DATA(ls_release) = CORRESPONDING zcl_abapgit_historical_source=>ty_release( ls_request ).
        IF zcl_abapgit_historical_source=>is_released_after(
              is_release   = ls_release
              is_reference = ls_version ) = abap_true
            AND zcl_abapgit_historical_source=>is_released_after(
              is_release   = ls_release
              is_reference = ls_transport ) = abap_false
            AND NOT line_exists( lt_versions[ objname = ls_module-vrsd-objname korrnum = ls_request-trkorr ] ).
          INSERT ls_module-vrsd-objname INTO TABLE lt_removed.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_removed INTO DATA(lv_removed).
      DELETE ct_modules WHERE vrsd-objname = lv_removed.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_include_xml.

    DATA(ls_progdir) = zcl_abapgit_historical_program=>normalize_progdir(
      CORRESPONDING #( is_include-program-trdir ) ).
    IF ls_progdir-name IS INITIAL.
      ls_progdir-name = is_include-name.
    ENDIF.

* abapGit writes the include files without serializer metadata
    DATA(li_xml) = CAST zif_abapgit_xml_output( NEW zcl_abapgit_xml_output( ) ).
    zcl_abapgit_historical_program=>add_program(
      ii_xml     = li_xml
      is_progdir = ls_progdir
      it_tpool   = zcl_abapgit_historical_program=>normalize_textpool( is_include-tpool ) ).

    rv_xml = zcl_abapgit_historical_program=>render(
      ii_xml  = li_xml
      iv_name = |include { is_include-name } of function group { ms_tadir-obj_name }| ).

  ENDMETHOD.


  METHOD serialize_xml.

    DATA lt_includes  TYPE ty_sobj_names.
    DATA lt_functions TYPE ty_functions.


* the file is written as zcl_abapgit_object_fugr serializes it, the screens and the GUI status
* abapGit adds are not extracted, nor are documentation and translations
    LOOP AT it_includes INTO DATA(ls_include).
      INSERT CONV sobj_name( ls_include-name ) INTO TABLE lt_includes.
    ENDLOOP.
    SORT lt_includes.

    LOOP AT it_modules INTO DATA(ls_module).
      INSERT ls_module-function INTO TABLE lt_functions.
    ENDLOOP.

    DATA(li_xml) = CAST zif_abapgit_xml_output( NEW zcl_abapgit_xml_output( ) ).
    li_xml->add( iv_name = 'AREAT'
                 ig_data = iv_areat ).
    li_xml->add( iv_name = 'INCLUDES'
                 ig_data = lt_includes ).
    li_xml->add( iv_name = 'FUNCTIONS'
                 ig_data = lt_functions ).

    rv_xml = zcl_abapgit_historical_program=>render(
      ii_xml      = li_xml
      is_metadata = VALUE #(
        class   = 'LCL_OBJECT_FUGR'
        version = 'v1.0.0' )
      iv_name     = |function group { ms_tadir-obj_name }| ).

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_deleted_files.

* the member files are named after includes and function modules, which are only known from
* the repository once the group is gone
    INSERT VALUE #(
      filename = build_file_prefix( )
      deleted  = abap_true
      prefix   = abap_true ) INTO TABLE rt_files.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA lt_sources TYPE string_table.
    DATA ls_main    TYPE ty_include.


* a transport usually carries only the parts that changed, so the group is rebuilt from the
* newest version of every part as of this transport
    DATA(lt_snapshot) = zcl_abapgit_historical_source=>read_snapshot(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).
    IF NOT line_exists( lt_snapshot[ korrnum = iv_korrnum ] ).
      RETURN.
    ENDIF.

* the main program carries the attributes of the group, without it there is no group
    READ TABLE lt_snapshot INTO DATA(ls_reps) WITH KEY objtype = 'REPS' objname = mv_main.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    ls_main-name = mv_main.
    ls_main-program = zcl_abapgit_historical_source=>read_program( ls_reps ).
    IF ls_main-program IS INITIAL.
      RETURN.
    ENDIF.
    ls_main-program-source = zcl_abapgit_hist_fugr_source=>strip_generation_comments(
      ls_main-program-source ).
    DATA(lv_language) = get_original_language( ls_main-program-trdir ).

    READ TABLE lt_snapshot INTO DATA(ls_rept) WITH KEY objtype = 'REPT' objname = mv_main.
    IF sy-subrc = 0.
      ls_main-tpool = zcl_abapgit_historical_program=>read_textpool(
        is_vrsd     = ls_rept
        iv_language = lv_language ).
    ENDIF.

    DATA(lt_modules) = read_functions(
      it_snapshot = lt_snapshot
      iv_korrnum  = iv_korrnum
      iv_language = lv_language ).

    INSERT ls_main-program-source INTO TABLE lt_sources.
    LOOP AT lt_modules INTO DATA(ls_module).
      INSERT ls_module-source INTO TABLE lt_sources.
    ENDLOOP.
    DATA(lt_includes) = read_includes(
      it_snapshot = lt_snapshot
      iv_korrnum  = iv_korrnum
      it_sources  = lt_sources ).
    INSERT ls_main INTO TABLE lt_includes.
    SORT lt_includes BY name.

* the members are only known as of this transport, so every file of the group goes first and
* the current members are added again, which removes includes and function modules dropped since
    INSERT VALUE #(
      filename = build_file_prefix( )
      deleted  = abap_true
      prefix   = abap_true ) INTO TABLE rt_files.

    INSERT VALUE #(
      filename = get_file_name( 'xml' )
      source   = serialize_xml(
        iv_areat    = read_areat( lv_language )
        it_includes = lt_includes
        it_modules  = lt_modules ) ) INTO TABLE rt_files.

* abapGit ends every source file with a newline
    LOOP AT lt_includes INTO DATA(ls_include).
      INSERT VALUE #(
        filename = get_file_name( iv_extra = ls_include-name
                                  iv_ext   = 'abap' )
        source   = |{ ls_include-program-source }\n| ) INTO TABLE rt_files.
      INSERT VALUE #(
        filename = get_file_name( iv_extra = ls_include-name
                                  iv_ext   = 'xml' )
        source   = serialize_include_xml( ls_include ) ) INTO TABLE rt_files.
    ENDLOOP.

    LOOP AT lt_modules INTO ls_module.
      INSERT VALUE #(
        filename = get_file_name( iv_extra = ls_module-function-funcname
                                  iv_ext   = 'abap' )
        source   = |{ ls_module-source }\n| ) INTO TABLE rt_files.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
