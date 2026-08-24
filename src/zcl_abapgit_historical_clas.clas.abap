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

    DATA ms_tadir TYPE zif_abapgit_definitions=>ty_tadir .

    METHODS determine_parts
      RETURNING
        VALUE(rt_parts) TYPE zif_abapgit_historical_object=>ty_parts_tt .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_CLAS IMPLEMENTATION.


  METHOD constructor.

    ms_tadir = is_tadir.

  ENDMETHOD.


  METHOD determine_parts.

* note that the CLSD is not needed
* 4 x CINC, dont serialize if empty, CCAU + CCDEF + CCIMP + CCMAC
    APPEND VALUE #(
      objtype  = 'CINC'
      objname  = cl_oo_classname_service=>get_ccau_name( CONV #( ms_tadir-obj_name ) )
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
    APPEND VALUE #(
      objtype  = 'CINC'
      objname  = cl_oo_classname_service=>get_ccimp_name( CONV #( ms_tadir-obj_name ) )
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
    APPEND VALUE #(
      objtype  = 'CINC'
      objname  = cl_oo_classname_service=>get_ccdef_name( CONV #( ms_tadir-obj_name ) )
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
    APPEND VALUE #(
      objtype  = 'CINC'
      objname  = cl_oo_classname_service=>get_ccmac_name( CONV #( ms_tadir-obj_name ) )
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
    APPEND VALUE #(
      objtype  = 'CPUB'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
    APPEND VALUE #(
      objtype  = 'CPRO'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
    APPEND VALUE #(
      objtype  = 'CPRI'
      objname  = ms_tadir-obj_name
      type     = ms_tadir-object
      name     = ms_tadir-obj_name
      devclass = ms_tadir-devclass ) TO rt_parts.
* ? x METH
    DATA(lv_objname) = |{ ms_tadir-obj_name WIDTH = 30 }%|.
    SELECT DISTINCT objtype, objname FROM vrsd INTO TABLE @DATA(lt_methods)
      WHERE objtype = 'METH'
      AND objname LIKE @lv_objname
      ORDER BY objtype, objname.
    LOOP AT lt_methods INTO DATA(ls_method).
      APPEND VALUE #(
        objtype  = ls_method-objtype
        objname  = ls_method-objname
        type     = ms_tadir-object
        name     = ms_tadir-obj_name
        devclass = ms_tadir-devclass ) TO rt_parts.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_historical_object~build_files.

    DATA ls_file     LIKE LINE OF rt_files.
    DATA ls_extended TYPE zif_abapgit_historical_object=>ty_vrsd.


    DATA(lt_vrsd) = zcl_abapgit_historical_source=>read_versions(
      it_parts   = determine_parts( )
      iv_korrnum = iv_korrnum ).

    LOOP AT lt_vrsd ASSIGNING FIELD-SYMBOL(<ls_vrsd>).
      <ls_vrsd>-source = zcl_abapgit_historical_source=>read_reps( <ls_vrsd> ).
    ENDLOOP.

    ls_file-filename = |{ to_lower( ms_tadir-obj_name ) }.clas.abap|.

    READ TABLE lt_vrsd INTO ls_extended WITH KEY objtype = 'CPUB'.
    IF sy-subrc = 0.
      ls_file-source = |{ ls_extended-source }\n|.
    ENDIF.

    READ TABLE lt_vrsd INTO ls_extended WITH KEY objtype = 'CPRO'.
    IF sy-subrc = 0.
      ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
    ENDIF.

    READ TABLE lt_vrsd INTO ls_extended WITH KEY objtype = 'CPRI'.
    IF sy-subrc = 0.
      ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
    ENDIF.

    ls_file-source = |{ ls_file-source }CLASS { to_lower( ms_tadir-obj_name ) } IMPLEMENTATION.\n|.
    LOOP AT lt_vrsd INTO ls_extended WHERE objtype = 'METH'.
* todo, this seems wrong, the LOOP might find too much?
      ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
    ENDLOOP.

    ls_file-source = |{ ls_file-source }ENDCLASS.|.

    INSERT ls_file INTO TABLE rt_files.

  ENDMETHOD.
ENDCLASS.
