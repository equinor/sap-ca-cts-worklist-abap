*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_oo_class_version_provider DEFINITION INHERITING FROM CL_WB_SOURCE_OBJECT_PROVIDER.

public section.
    methods get_version_source_public
        importing
      !OBJECT_NAME type VERSOBJNAM
      !OBJECT_TYPE type VERSOBJTYP
      !VERSNO type VRSD-VERSNO
    returning
      !value(ro_SOURCE) type RSWSOURCET
    raising
      CX_WB_OBJECT_VERSIONING .

endclass.

class lcl_oo_class_version_provider IMPLEMENTATION.

  METHOD get_version_source_public.

    me->get_version_source(
      EXPORTING
        object_name = object_name
        object_type = object_type
        versno      = versno
        IMPORTING
        source = ro_source
    ).

  ENDMETHOD.

endclass.
