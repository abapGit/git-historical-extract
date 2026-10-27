# Historical object support plan

Each object handler should implement `zif_abapgit_historical_object`, select the version that belongs to the processed transport, emit files compatible with the abapGit serializer used by this project, and describe every file that must be removed when the R3TR object is deleted. Existing `INTF`, `CLAS`, and `PROG` handlers are treated as baselines to harden rather than finished implementations.

Translations are out of scope for all object types. Extract language-dependent texts only in the object's original language. All file sets, deletion requirements, and import/serialize-back checks below apply to this original-language-only scope.

## DTEL

- **Required output:** AFF JSON (`<object-name>.dtel.json`). Classic abapGit DTEL XML is out of scope.
- [x] Confirm the VRSD component type and released-system function module used to read a historical data element version (including the `DTEL` to version-object translation).
- [x] Add `zcl_abapgit_historical_dtel` and its abapGit class XML, following the constructor, `determine_parts`, `build_files`, and `build_deleted_files` pattern used by `DOMD`.
- [x] Read the historical definition and original-language texts into a stable internal model, including domain references, built-in types, search-help/parameter references, headings, and field labels.
- [x] Map the historical model to the DTEL AFF schema supported by the project's abapGit dependency, including the AFF header, format version, original language, description, type information, references, and field labels.
- [x] Serialize the AFF model as deterministic UTF-8 JSON in `<object-name>.dtel.json`, matching the schema's defaults, enum mappings, property names, and omission rules so that a re-serialization produces no diff.
- [x] Return no file when the requested historical version does not exist, and raise a contextual exception for other version-reader failures.
- [x] Add `DTEL` routing to `zcl_abapgit_historical_objects`, including the R3TR/version-object name translation in both normal and deleted-object paths.
- [x] Emit `<object-name>.dtel.json` as the DTEL deletion file set and ensure no `.dtel.xml` file is generated.
- [ ] Test domain-based and built-in data elements, original-language labels, optional references, missing versions, and deletion.
- [ ] Run abaplint and verify an extracted AFF DTEL can be imported by abapGit and serialized back without a content diff.

## TABL

- **Required output:** the AFF file set `<object-name>.tabl.json` and `<object-name>.tabl.ddic`, plus the optional `<object-name>.tabl.settings.json`, where the DDL source is produced by abapGit's `zcl_abapgit_object_tabl_ddl->serialize( )`, merged to abapGit `main` as #7864. Classic abapGit TABL XML is out of scope. Both `zcl_abapgit_object_tabl_ddl` and `zif_abapgit_aff_tabl_v1` are already reachable through the existing abapGit dependency, so nothing new has to be pulled in.
- [x] Confirm the VRSD component type and the released-system version reader, including the `TABL` to version-object translation: the component type is `TABD` and the reader is `SVRS_GET_VERSION_TABD_40`, which returns `DD02V`, `DD03V`, `DD05V`, `DD08V`, `DD35V`, and `DD36V` plus the text tables `DD02TV`, `DD03TV`, and `DD08TV`.
- [x] Confirm the version reader returns the whole table in a single record, so no per-component merge across VRSD rows is needed as for `CLAS` and `PROG`.
- [x] Add `zcl_abapgit_historical_tabl` and its abapGit class XML, following the constructor, `determine_parts`, `build_files`, and `build_deleted_files` pattern used by `DOMD` and `DTEL`.
- [x] Read the historical version into `zif_abapgit_object_tabl=>ty_internal`. The reader hands back the dictionary view structures while the DDL serializer works on the prepared ones, so `DD03V`, `DD05V`, and `DD36V` are moved component-wise into `DD03P`, `DD05M`, and `DD36M`; `DD02V`, `DD08V`, and `DD35V` are taken over as they are.
- [ ] Confirm on a system that `DD03V`, `DD05V`, and `DD36V` name every component the serializer reads the same way their prepared counterparts do, because a renamed component would silently drop a field attribute, a foreign key condition, or a value help parameter rather than fail.
- [x] Merge the original-language texts returned by the version reader into the structures the serializer reads them from: `DD02TV` into `DD02V-DDTEXT` for the table description, `DD03TV` into `DD03P-DDTEXT` for the field labels, and `DD08TV` into `DD08V-DDTEXT` for the foreign key labels.
- [x] Call `zcl_abapgit_object_tabl_ddl->serialize( )` instead of reimplementing DDL syntax: abapGit owns the annotation set, field order, colon alignment, type mapping, and the serialize/deserialize round-trip contract, and formatting defects are fixed there rather than forked here.
- [x] Preserve the field, foreign-key, and value-help order delivered by the version reader, because the serializer emits in table order and any resorting changes the output.
- [x] Map the historical model to `zif_abapgit_aff_tabl_v1=>ty_main` for `<object-name>.tabl.json` — format version, original-language description, original language, ABAP language version — and serialize it through `zcl_abapgit_json_handler` exactly as `DOMD` and `DTEL` already do.
- [x] Reject up front, naming object and version in the message, each input that `zcl_abapgit_object_tabl_ddl` refuses: an `EXCLASS` outside `0` to `4`, an empty `CONTFLAG`, an unknown `AUTHCLASS`, and a `MAINFLAG` that is neither `X`, `N`, nor empty. A `TABCLASS` other than `TRANSP` is skipped rather than raised, so one unsupported table cannot fail the extraction of its whole transport.
- [x] Cover transparent tables including global temporary tables, `.INCLU` includes with name suffixes, and include extensions for foreign keys and value helps. Skip structures (`INTTAB`), append structures (`APPEND`), pooled and cluster tables, and IDoc segment tables, none of which the DDL format describes.
- [x] Document what is deliberately not extracted: the version reader returns no indexes (`DD12V`/`DD17V`); long texts and IDoc segment definitions are dropped as well.
- [x] Read the technical settings from their own `TABT` version sub object and emit them as `<object-name>.tabl.settings.json`, typed by `zif_abapgit_hist_aff_tabt_v1`, a copy of the AFF `zif_aff_tabt_v1` kept here because the abapGit dependency has no `TABT` type; write the file only when the transport has a `TABT` version, so an earlier settings file stays in place otherwise, and add it to the deletion file set.
- [x] Map `TABART`, `TABKAT`, `PROTOKOLL`, `UEBERSETZ`, `BUFALLOW`, `PUFFERUNG`, and `SCHFELDANZ` to the AFF settings, and read `ROWORCOLST` and `LOAD_UNIT` dynamically because they do not exist on every release; skip every value equal to its schema default, checked against the transpiled abapGit JSON handler.
- [ ] Confirm on a system that `SVRS_GET_VERSION_TABT_40` exists with a `DD09V_TAB` table parameter and raises `NO_VERSION`, and that `DD09V` names its components like `DD09L`.
- [ ] Find the `DD09L` component behind `writableByAmdp`; it is not written today, so a table writable by AMDP loses that flag.
- [ ] Replace `zif_abapgit_hist_aff_tabt_v1` with `zif_abapgit_aff_tabt_v1` once abapGit ships that type.
- [x] Record as a known fidelity limit that currency and quantity fields whose reference points at another table resolve through `DDIF_FIELDINFO_GET` against the active dictionary, so the emitted type reflects today's DDIC rather than its state at that transport; raise it upstream in abapGit if it turns out to matter.
- [x] Return no file when the requested historical version does not exist, and raise a contextual exception for other version-reader failures.
- [x] Add `TABL` routing to `zcl_abapgit_historical_objects`, including the R3TR/version-object name translation in both the normal and the deleted-object paths.
- [x] Emit `<object-name>.tabl.json` and `<object-name>.tabl.ddic` as the `TABL` deletion file set, and ensure no `.tabl.xml` file is generated.
- [x] Test fields typed by `DTEL` and by built-in types, key and `not null` flags, foreign keys with cardinalities, value helps, rejected table classes and attribute values, and deletion.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: includes with and without suffix, `remove foreign key`, currency and quantity reference fields, and a missing version.
- [x] Run abaplint, and check round-trip stability by feeding an extracted `.tabl.ddic` back through `zcl_abapgit_object_tabl_ddl->deserialize( )` and serializing again to identical text, driving both from the transpiled abapGit build.
- [ ] Revisit end-to-end abapGit import and serialize-back equivalence once `zcl_abapgit_object_tabl` itself reads and writes the DDL file set; today it still serializes `TABL` as XML and never calls `zcl_abapgit_object_tabl_ddl`, so an extracted DDL table cannot round-trip through abapGit yet.

## TTYP

- **Required output:** `<object-name>.ttyp.xml` exactly as abapGit's `zcl_abapgit_object_ttyp` serializes it, with `DD40V`, `DD42V`, and `DD43V`. Neither abapGit nor the AFF specification has a file format for table types, so this is the one object type extracted as classic abapGit XML.
- [x] Confirm the VRSD component type: `TTYD`, as listed next to `TABD` and `DTED` in abapGit's `zcl_abapgit_objects_activation`.
- [x] Add `zcl_abapgit_historical_ttyp` and its abapGit class XML, then register `TTYD` in the object factory with the `TTYP` to `TTYD` translation in both the normal and the deleted-object paths.
- [x] Read the historical version with `SVRS_GET_VERSION_TTYD_40`, signature confirmed on a system: `OBJECT_NAME` and `VERSNO`, the tables `DD40V_TAB`, `DD40TV_TAB` (`DD40TV`), `DD42V_TAB`, `DD43V_TAB`, and `DD43TV_TAB` (`DD43T`), and the exceptions `NO_VERSION`, `SYSTEM_FAILURE`, and `COMMUNICATION_FAILURE`.
- [x] Merge the original-language texts into the structures abapGit writes: `DD40TV-DDTEXT` into `DD40V-DDTEXT`, and `DD43T-KEYDESCRIPTION` into `DD43V-KEYDESCRIPTION` per secondary key; a key without a description in the original language is written without one.
- [x] Support every line type, access mode, and key definition the version reader returns: the XML carries `DD40V` as it is, so elementary, structured, reference, range, and DDIC-object line types and explicit versus default keys need no mapping and nothing is rejected beyond a missing original language.
- [x] Normalize as abapGit does: clear `AS4USER`, `AS4DATE`, `AS4TIME`, and `ACTFLAG`, and clear `TYPELEN` when a row kind is set; sort `DD42V` by secondary key name and key field position, primary key first, and `DD43V` by secondary key name, so the output does not depend on the reader's row order.
- [x] Render through `zcl_abapgit_xml_output` with the `LCL_OBJECT_TTYP` serializer metadata and the `utf-8` declaration abapGit writes; the git stage adds the UTF-8 byte order mark to every `.xml` file, as abapGit's `add_xml( )` does.
- [ ] Decide on `DD40L_EXTRA`: abapGit writes the ABAP language version there when the repository asks for it, the version reader is not known to return it, so it is never written.
- [x] Return no file when the requested historical version does not exist, and raise a contextual exception for other version-reader failures.
- [x] Emit `<object-name>.ttyp.xml` as the TTYP deletion file set.
- [x] Test the normalization, key field and secondary key order, a missing original language, the serialized XML for default and explicit keys with a secondary key, and deletion.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: sorted and hashed types, reference line types, original-language texts, and a missing version.
- [x] Run abaplint.
- [ ] Verify abapGit import plus serialize-back equivalence, after the referenced DOMA/DTEL/TABL objects are available.

## INTF

- **Required output:** the AFF file set `<object-name>.intf.abap` and `<object-name>.intf.json`, matching abapGit's INTF serializer with AFF enabled. Classic abapGit INTF XML is out of scope.
- [x] Baseline handler, factory routing, source extraction, and main-file deletion exist.
- [x] Select the newest `INTF` version of the transport deterministically when a transport holds more than one.
- [ ] Verify on a system that the `INTF` version sub object carries both the source and the metadata on every supported release.
- [x] Read the historical metadata with `SVRS_GET_VERSION_INTF_40` into `VSEOINTERF`, `VSEOATTRIB`, `VSEOMETHOD`, `VSEOEVENT`, `VSEOPARAM`, and `VSEOEXCEP`; the parameter names are taken from existing SAPlink code that calls it with `PVSEOCOMPRI`.
- [ ] Confirm on a system that the `P*` table parameters of `SVRS_GET_VERSION_INTF_40` are typed with the `VSEO*` views named above, and that the views carry `LANGU` and `DESCRIPT`; a wrong line type raises `CX_SY_DYN_CALL_ILLEGAL_TYPE`, which `read_intf( )` does not catch, so the extraction stops.
- [ ] Find the line type of the `TYPE_TAB` parameter and add the type descriptions; they are not read today, so `descriptions/types` is always missing from the JSON.
- [x] Map the historical model to `zif_abapgit_aff_intf_v1=>ty_main` — format version, original-language description, original language, ABAP language version, category, proxy flag, and the original-language descriptions of attributes, methods, method parameters and exceptions, and events with their parameters — dropping components without text as abapGit does, and serialize it through `zcl_abapgit_json_handler` with the category enum mapping and default skipping copied from abapGit's INTF serializer; checked against the transpiled abapGit JSON handler.
- [x] Reject a category without AFF value, such as `52` business instance components, naming the interface in the message.
- [x] Emit either both files or none: skip the interface when the source is empty, when the metadata version does not exist, or when the reader returns no header, and raise a contextual exception for other metadata reader failures.
- [ ] Distinguish a missing version from a reader error in `zcl_abapgit_historical_source=>read_reps( )` too; today it returns an empty source for both, and the interface is then skipped silently.
- [x] End the `.intf.abap` file with a newline as abapGit's `add_abap( )` does; line endings are `\n` and trailing blanks of the 255 character source lines are dropped by `concat_lines_of`.
- [ ] Preserve interface annotations, aliases, events, types, constants, method signatures, and ABAP Doc contained in the historical source; the source is taken over verbatim, so this only needs confirming on a system.
- [x] Emit `<object-name>.intf.abap` and `<object-name>.intf.json` as the INTF deletion file set.
- [x] Test the header, ABAP language version, and category mapping, attribute, method, parameter, exception, and event descriptions, original-language filtering, rejected categories, the serialized JSON, and deletion.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: inheritance, aliases, ABAP Doc, empty optional sections, and a missing version.
- [x] Run abaplint.
- [ ] Verify an extracted interface imports into abapGit with AFF enabled and serializes back without a content diff.

## CLAS

- **Required output:** the AFF file set `<object-name>.clas.abap` and `<object-name>.clas.json`, plus `<object-name>.clas.definitions.abap`, `.clas.implementations.abap`, `.clas.macros.abap`, and `.clas.testclasses.abap` for the local includes that have content. The metadata file is typed by `zif_abapgit_aff_clas_v1`, which abapGit ships but does not register for AFF yet, so the file set follows the AFF specification rather than abapGit's classic `locals_def`/`locals_imp` names. Classic abapGit CLAS XML is out of scope.
- [x] Baseline handler and factory routing exist for `CPUB`, `CPRO`, `CPRI`, `METH`, and the four `CINC` names.
- [x] Route `CLAS` straight to its handler in `zcl_abapgit_historical_objects=>read( )`; VRSD has no `CLAS` version, only the parts, so the generic version lookup never let a class through.
- [x] Rebuild the class as of each transport from the newest version of every part released up to that transport, through `zcl_abapgit_historical_source=>read_snapshot( )`, which orders releases by `E070` date, time, and request exactly as the extraction does and ignores versions without a released request; skip the class when the transport carries no version of any part.
- [x] Restrict `METH` discovery to the exact class, filtering out classes matched by the `_` wildcard of the `LIKE` pattern, and keep only methods declared by `METHODS` or `CLASS-METHODS` in the historical sections, or belonging to a declared interface or an interface that one of those includes as of the same transport; when an included interface has no version, such as a SAP interface, interface methods are kept rather than guessed away.
- [x] Assemble the sections and the method implementations in the SE24 layout, methods in alphabetical order, closing the definition with `ENDCLASS` only when the private section include does not already end with it.
- [ ] Confirm on a system that the definition's `ENDCLASS` is not part of the `CPRI` include, and compare the assembled main source with abapGit's `serialize_abap( )` output, including the `*"*` comment lines SAP writes into the section includes, which are kept verbatim.
- [x] Read the local definitions from `CDEF` or `CINC`, whichever is newer, and the local implementations, macros, and test classes from `CINC`; skip an include that holds only `*"*` comments, as abapGit does, and mark its file deleted so a file from an earlier transport goes away.
- [ ] Confirm on a system which of `CDEF` and `CINC` the local definitions are versioned as; abapTimeMachine reads `CDEF`, the baseline read `CINC`.
- [x] Read the class metadata with `SVRS_GET_VERSION_CLSD_40` into `VSEOCLASS` and the `VSEO*` component views, and map it to `zif_abapgit_aff_clas_v1=>ty_main` — description, original language, ABAP language version, category, fix point arithmetic, message class, and the original-language component descriptions, shared with INTF in `zcl_abapgit_historical_oo`.
- [ ] Confirm the signature of `SVRS_GET_VERSION_CLSD_40` on a system. It is guessed from `SVRS_GET_VERSION_INTF_40`: a `CX_SY_DYN_CALL_*` error drops the component tables first and then the metadata file, and a header without language is taken as a wrong guess too, so today a class can be extracted without `.clas.json` or without descriptions and nobody is told.
- [ ] Find where class component texts are versioned if `CLSD` does not return them, possibly the `CPUB`, `CPRO`, and `CPRI` readers, and read type descriptions, which are not read for INTF either.
- [x] Reject a category without AFF value, naming the class in the message.
- [x] Treat missing sections, includes, and methods as absent, and skip the class when the public section, which holds the `CLASS ... DEFINITION` statement, cannot be read.
- [x] Emit the main source, the metadata file, and all four include files as the CLAS deletion file set.
- [ ] Decide whether the test class include should also honour `VSEOCLASS-WITH_UNIT_TESTS` as abapGit does; today only its content decides.
- [x] Test the declaration parser, including chains, comments, literals, and interfaces, method membership, the include content rule, and the main source layout; these tests run in the transpiler because `zcl_abapgit_hist_clas_source` has no database access. Test the metadata mapping, the serialized JSON checked against the transpiled abapGit JSON handler, file names, and deletion.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: snapshots across several transports, method additions and deletions, nested interfaces, `CDEF` versus `CINC`, and a missing version.
- [x] Run abaplint.
- [ ] Verify that several incremental class transports extract to the same files as serializing the class at each point, once abapGit can import CLAS AFF.

## PROG

- **Required output:** `<object-name>.prog.abap` and `<object-name>.prog.xml` exactly as abapGit's `zcl_abapgit_object_prog` serializes them. abapGit has no AFF type for programs, so the classic XML is the format. For executable programs and module pools abapGit also writes `DYNPROS`, `CUA`, and `VARIS` into the XML and one `<object-name>.prog.screen_<dynnr>.abap` file per screen; those are deferred, see below.
- [x] Baseline handler, factory routing, `REPS` source extraction, and main-file deletion exist.
- [x] Inventory the VRSD components of an R3TR `PROG`: `REPS` carries the source and, through the `TRDIR_TAB` of `SVRS_GET_REPS_FROM_OBJECT`, the program attributes; `REPT` the text pool with text symbols, selection texts, and the title; `DYNP` one screen each, named by the program padded to 40 characters plus the screen number; `CUAD` the GUI status; documentation and variants are versioned as objects of their own.
- [x] Route `PROG` straight to its handler in `zcl_abapgit_historical_objects=>read( )` as for `CLAS`; VRSD has no `PROG` version, only the parts, so the generic version lookup never let a program through.
- [x] Reconstruct the program as of the transport from the newest `REPS` and `REPT` versions released up to it, through `zcl_abapgit_historical_source=>read_snapshot( )`; skip the program when the transport carries neither part or when no source version exists.
- [x] Harden `REPS` reading in `zcl_abapgit_historical_source=>read_program( )`: a missing version skips the program instead of writing an empty `.prog.abap`, and a reader failure or a version without attributes raises with object and version in the message.
- [x] Map `TRDIR` to abapGit's `PROGDIR` and clear the same volatile fields abapGit's `read_progdir( )` clears; an empty original language `RLOAD` defaults to `E`, which also selects the text pool language; `UCCHECK` is kept, as abapGit writes it for repositories without an ABAP language version setting.
- [ ] Confirm on a system that `TRDIR_TAB` is filled for every `REPS` version; today a version without attributes stops the extraction with an exception.
- [ ] Decide on the `PROGDIR` fields that `TRDIR` does not carry — `SQLX`, `DBAPL`, `DBNA`, `CLAS`, and `OCCURS` — which are empty for ordinary programs and therefore never written.
- [x] Read the text pool with `SVRS_GET_VERSION_REPT_40` into `TEXTPOOLT`, keep the original language only, drop a title without text, sort by ID and key as `READ TEXTPOOL` returns it, and move the dictionary reference flag of selection texts into `SPLIT` as abapGit's `add_tpool( )` does.
- [ ] Confirm the signature of `SVRS_GET_VERSION_REPT_40` on a system. It is taken from public code calling it with `REPOT_TAB` typed `TEXTPOOLT` and the exceptions `NO_VERSION`, `SYSTEM_FAILURE`, and `COMMUNICATION_FAILURE`; whether `TEXTPOOLT` has a `LANGUAGE` column is not known, so it is read dynamically and every row is kept when it has none.
- [x] Render through `zcl_abapgit_xml_output` with the `LCL_OBJECT_PROG` serializer metadata and the `utf-8` declaration abapGit writes.
- [ ] Extract screens: `SVRS_GET_VERSION_DYNP_40` is only known to return the flow logic in `D022S_TAB`; abapGit writes the screen as `RPY_DYNPRO_READ` returns it (`RPY_DYHEAD`, containers, fields) plus a `.prog.screen_<dynnr>.abap` flow logic file, so the native `D020S`/`D021S` tables have to be converted the way `RPY_DYNPRO_READ` does. Until then a program with screens is extracted without them.
- [ ] Extract the GUI status: the signature of `SVRS_GET_VERSION_CUAD_40` is not known; abapGit writes the `RSMPE_*` tables of `RS_CUA_INTERNAL_FETCH`.
- [ ] Decide whether to extract `VARIS` (system variants `SAP&`/`CUS&` only) and the `RE` documentation abapGit writes as `LONGTEXTS`.
- [ ] Extend deletion/staging so a deleted program removes the `.prog.screen_<dynnr>.abap` files once screens are extracted; today the deletion file set is `.prog.abap` and `.prog.xml`, which is every file written.
- [x] Test the attribute normalization, original language, text pool order and title handling, selection text splitting, the serialized XML for an executable program and an include, and deletion.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: snapshots across transports with only `REPS` or only `REPT`, translated texts, and a missing version.
- [x] Run abaplint.
- [ ] Verify a multi-transport PROG history through abapGit import and serialize-back comparison.

## FUGR

- **Required output:** `<object-name>.fugr.xml` with `AREAT`, `INCLUDES`, and `FUNCTIONS`, `<object-name>.fugr.<include>.abap` and `.xml` for the main program and every include, and `<object-name>.fugr.<function>.abap` per function module, exactly as abapGit's `zcl_abapgit_object_fugr` serializes them; file names come from `zcl_abapgit_filename_logic`, so namespaces are escaped as abapGit does. abapGit has no AFF type for function groups. For groups with screens abapGit also writes `DYNPROS`, `CUA`, and `VARIS`; those are deferred, see below.
- [x] Map an R3TR `FUGR` to its VRSD parts: the main program `SAPL<group>` as `REPS` with its attributes and as `REPT` with the text pool, every include under the `L<group>` name prefix as `REPS`, and every function module as `FUNC`. Screens (`DYNP`), the GUI status (`CUAD`), and documentation are versioned too but not extracted.
- [x] Add `zcl_abapgit_historical_fugr` and its abapGit class XML, register `FUGR` in the object factory, and route it straight to its handler in `zcl_abapgit_historical_objects=>read( )` as for `CLAS` and `PROG`, since VRSD has no `FUGR` version.
- [x] Rebuild the group as of each transport from the newest version of every part released up to it through `zcl_abapgit_historical_source=>read_snapshot( )`, so a transport carrying one function module does not erase the rest; skip the group when the transport carries none of its parts or the main program has no version.
- [x] Reconstruct include membership from history: follow the `INCLUDE` statements of the historical main program, includes, and function modules, as `RS_GET_ALL_INCLUDES` does on the active sources, plus the `T00` maintenance view include abapGit always adds. Leave out what abapGit leaves out: the function module includes and `UXX`, `LSVIM*` and `XTI` includes, includes named like another group's, and includes with their own `R3TR PROG` entry. The `INCLUDE` parsing lives in `zcl_abapgit_hist_fugr_source`, free of database access.
- [x] Reconstruct function module membership from history: the candidates are today's members from `ENLFDIR` plus the function modules recorded as `LIMU FUNC` in any request that no longer exist, each with the group of its newest version, read once per run. A candidate is a member when its newest version as of the transport names the group in `ENLFDIR-AREA` or `TFDIR-PNAME`, and no released workbench request between that version and the transport records it without a version of it.
- [ ] Confirm on a system that releasing a request versions every function module it records that still exists, so a request recording a function module without a version of it really marks the deletion; check `E071-OBJFUNC` of such entries as well.
- [x] Read function modules with `SVRS_GET_VERSION_FUNC_40`: the source from `UINCL_TAB`, the attributes from `ENLFD_TAB` and `TFDIR_TAB` (`GLOBAL`, `EXTEN3`, `FMODE`, `UTASK`, and `RFCSCOPE`/`RFCVERS` where the release has them, all taken over by name), the original-language short text from `TFTIT_TAB`, the parameters from `FUPAR_TAB`, and their original-language texts from `FUNCT_TAB`.
- [ ] Confirm the signature of `SVRS_GET_VERSION_FUNC_40` on a system. The table parameters and their line types (`ABAPTXT255`, `ENLFDIR`, `TFDIR`, `TFTIT`, `SFUPARAREF`, `FUNCT`) are taken from public code calling it; a wrong line type raises `CX_SY_DYN_CALL_ILLEGAL_TYPE`, which is not caught, so the extraction stops.
- [x] Map the `SFUPARAREF` rows to abapGit's `RSIMP`, `RSCHA`, `RSEXP`, `RSTBL`, `RSEXC`, and `RSFDO` lists in parameter position order: a set `TYPE` flag writes the reference to `TYP`, otherwise to `DBFIELD` or `DBSTRUCT`; the documentation lists every parameter, importing, exporting, changing, tables, then exceptions, which matches the order abapGit wrote for 22,725 of 23,265 function modules in local repositories.
- [ ] Confirm on a system how `RPY_FUNCTIONMODULE_READ_NEW` derives the interface from `FUPARAREF`: the component names of `SFUPARAREF`, the meaning of `TYPES`, which abapGit sometimes writes as `O` and which is copied from the `TYPE` flag when it is neither empty nor `X`, whether `REMOTE_CALL` is `TFDIR-FMODE` as taken over today, where the basXML flag `REMOTE_BASXML` is stored, as it is never written, and the order of the documentation rows for the remaining function modules.
- [x] Normalize as abapGit does: clear the volatile `PROGDIR` fields shared with PROG through `zcl_abapgit_historical_program`, write the include files without serializer metadata, give only the main program a text pool, drop the generation stamps of maintenance view sources as `strip_generation_comments( )` does, sort `INCLUDES` and `FUNCTIONS` by name, and end every source file with a newline.
- [x] Read the group short text `AREAT` from today's `TLIBT` in the original language of the main program; it is not known to be versioned.
- [ ] Find out whether the group short text is versioned, as `LIMU FUGT` suggests, and read it from there.
- [x] Handle member additions, renames, and removals: every transport first removes all files whose name starts with `<object-name>.fugr.` and then writes the members again; the stage keeps an addition over an earlier removal of the same file, so only files of dropped members go away.
- [x] Extend whole-object deletion to the dynamically named member files with a prefix deletion in `zif_abapgit_historical_extract=>ty_file`, resolved by `zcl_abapgit_historical_git` against the files in the repository.
- [ ] Map a request that only deletes a function module to its group; abapGit's `list_r3tr_by_request( )` cannot resolve a deleted function module, so the deletion is only committed with the next transport changing the group.
- [ ] Find function modules that were moved to another group since; today they are only extracted with their current group.
- [ ] Extract screens and the GUI status, sharing the open PROG work on `DYNP` and `CUAD`.
- [ ] Decide whether to extract the `RE`, `FU`, and `FX` documentation abapGit writes as `LONGTEXTS`.
- [x] Test the main program and include prefix with and without namespace, `INCLUDE` parsing with chains, comments, literals, and `INCLUDE STRUCTURE`/`TYPE`, the include membership rules, generation comment stripping, and the interface and documentation mapping, all run in the transpiler; test file names with namespaces, the group, include, and main program XML, and the deletion file set, which need a system.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: snapshots across transports, member addition, rename, and deletion, deleted function modules, nested includes, and a missing version.
- [x] Run abaplint.
- [ ] Verify the extracted group imports, activates, and serializes back through abapGit without missing members or spurious diffs.
