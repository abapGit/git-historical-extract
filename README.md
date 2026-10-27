# WIP, not working yet, git_historical_extract
Extract historical ABAP objects to git

* Objects will be placed according to current package structure, not historical
* Deleted objects are not taken into account?
* Traditional SAP GUI
* Folder logic = 'FULL'
* Full R3TR objects, doesnt respect LIMU
* Only custom objects
* Language dependent texts are extracted in the original language only, translations are out of scope

## INTF

Interfaces are written as the ABAP file format pair `<name>.intf.abap` and `<name>.intf.json`,
as abapGit does with AFF enabled. The source comes from the `INTF` version, the metadata and the
component descriptions from `SVRS_GET_VERSION_INTF_40`. An interface whose source or metadata
cannot be read at that transport is skipped, so both files are always written together.

Not extracted:

* descriptions of types declared in the interface, the version reader's line type for them is not known yet
* long texts and translations

## CLAS

Classes are written as the ABAP file format set `<name>.clas.abap` and `<name>.clas.json`, plus
`.clas.definitions.abap`, `.clas.implementations.abap`, `.clas.macros.abap` and
`.clas.testclasses.abap` for the local includes that have content. abapGit has the AFF type for
classes but cannot import them in this format yet.

A transport usually carries only the parts of a class that changed, so each transport rebuilds the
class from the newest version of every part released up to it. Methods that were removed before a
transport are left out by checking the versioned methods against the declarations in the historical
sections.

The metadata comes from `SVRS_GET_VERSION_CLSD_40`, whose signature is not confirmed on a system
yet. When the call does not fit the release, the class is extracted without component descriptions,
or without `.clas.json` at all.

Not extracted:

* descriptions of types declared in the class
* text pools, documentation, long texts and translations

## PROG

Programs are written as `<name>.prog.abap` and `<name>.prog.xml` in the format of abapGit's PROG
serializer, as there is no ABAP file format for programs. Like a class, a program is versioned in
parts, so each transport rebuilds it from the newest source (`REPS`) and text pool (`REPT`)
versions released up to it. The program attributes come with the source version, the text pool
from `SVRS_GET_VERSION_REPT_40`, whose signature is not confirmed on a system yet.

Not extracted:

* screens and GUI status, so a program with screens is written without `DYNPROS`, `CUA` and the `.prog.screen_<dynnr>.abap` files
* variants, documentation, long texts and translations

## FUGR

Function groups are written in the format of abapGit's FUGR serializer: `<name>.fugr.xml` with the
short text, the include list and the function module interfaces, `<name>.fugr.<include>.abap` and
`.xml` for the main program and every include, and `<name>.fugr.<function>.abap` for every function
module. A function group is versioned in parts, the main program and the includes as `REPS`, the
text pool as `REPT` and every function module as `FUNC`, so each transport rebuilds the group from
the newest version of every part released up to it. The function modules are read through
`SVRS_GET_VERSION_FUNC_40`, whose signature is not confirmed on a system yet.

The members are taken from history rather than from today's state. The includes are found by
following the `INCLUDE` statements of the historical sources, starting at the main program. The
function modules are the current ones plus those deleted since, each kept as long as its version
names the group and no later request recorded it without versioning it, which is what deleting it
leaves behind. Every transport first removes all files of the group and then writes the members
again, so includes and function modules dropped since disappear in the same commit.

Not extracted:

* screens and GUI status, so a group with screens is written without `DYNPROS` and `CUA`
* documentation, long texts and translations
* the short text of the group is not versioned, it is today's text in the original language
* a function module moved to another group since is only found in its current group

A transport that only deletes a function module carries no part abapGit can map to the group, so the
deletion shows up with the next transport that changes the group.

## TABL

Tables are written as the ABAP file format pair `<name>.tabl.json` and `<name>.tabl.ddic`,
the DDL source being produced by abapGit's `zcl_abapgit_object_tabl_ddl`. That format only
describes transparent tables, so structures, append structures, pooled and cluster tables
and IDoc segment tables are skipped rather than extracted.

The technical settings are versioned as the separate `TABT` sub object, read through
`SVRS_GET_VERSION_TABT_40` and written as `<name>.tabl.settings.json`. The abapGit dependency
has no AFF type for that file, so a copy of `zif_aff_tabt_v1` lives here as
`zif_abapgit_hist_aff_tabt_v1`. The settings file is only written when the transport carries a
settings version next to the table definition, otherwise the file from an earlier transport is kept.
`writableByAmdp` is never written, and the storage type is written as `undefined` on releases
without a row or column store setting.

Not extracted:

* table indexes, which `SVRS_GET_VERSION_TABD_40` does not return at all
* long texts and IDoc segment definitions

Known fidelity limit: a currency or quantity field whose reference points at another table
is classified through `DDIF_FIELDINFO_GET` against the active dictionary, so the emitted
type follows today's state of the referenced object rather than its state at that transport.

## TTYP

Table types are written as `<name>.ttyp.xml` in the format of abapGit's TTYP serializer, as neither
abapGit nor the ABAP file formats have an AFF type for them. The definition, the description and
the secondary key descriptions come from `SVRS_GET_VERSION_TTYD_40`.

Not extracted:

* the ABAP language version, which abapGit writes as `DD40L_EXTRA` for repositories that ask for it
* long texts and translations
