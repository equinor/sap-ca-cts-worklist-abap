CLASS zctsw_transport_dao DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " The regex that will be used to collect and group transports in the view. The regex should be changed
    " to match the conventions for naming transports in your company
    CONSTANTS gc_grouping_regex TYPE string VALUE '(CHG\d+)|(C\d+)|(S\s\d+)' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_user_full_name,
        bname TYPE bname,
        name  TYPE name_text,
      END OF ty_user_full_name .
    TYPES:
      BEGIN OF ty_user_short_name,
        bname     TYPE bname,
        shortname TYPE name_text,
      END OF ty_user_short_name .
    TYPES:
      ty_user_full_name_tab TYPE STANDARD TABLE OF ty_user_full_name WITH KEY bname .
    TYPES:
      ty_use_short_name_tab TYPE STANDARD TABLE OF ty_user_short_name WITH KEY bname .
    TYPES:
      BEGIN OF ty_documentation,
        text TYPE string,
      END OF ty_documentation .
    TYPES:
      BEGIN OF ty_update,
        description   TYPE string,
        documentation TYPE string,
      END OF ty_update .

    CLASS-DATA:
      gt_object_types    TYPE STANDARD TABLE OF ko100 .
    CLASS-DATA gt_user_full_name TYPE ty_user_full_name_tab .
    CLASS-DATA gt_user_short_name TYPE ty_user_full_name_tab .

    CLASS-METHODS get_username_from_email
      IMPORTING
        i_email           TYPE string
      RETURNING
        VALUE(r_username) TYPE string.

    METHODS constructor .
    METHODS get_modifiable_transports
      IMPORTING
        !it_users            TYPE zctsw_user_range_t OPTIONAL
        !it_transports       TYPE zctsw_transport_range_t OPTIONAL
      RETURNING
        VALUE(rt_transports) TYPE zctsw_transport_t .
    METHODS get_requests
      IMPORTING
        !it_users            TYPE zctsw_user_range_t OPTIONAL
        !it_transports       TYPE zctsw_transport_range_t OPTIONAL
        !it_dates            TYPE zctsw_date_range_t OPTIONAL
        !it_text             TYPE /iwbep/t_cod_select_options OPTIONAL
        !it_change           TYPE /iwbep/t_cod_select_options OPTIONAL
        !i_expand_selection  TYPE abap_bool
      RETURNING
        VALUE(rt_transports) TYPE zctsw_transport_t .
    METHODS get_tasks
      IMPORTING
        !i_parent_request    TYPE trkorr OPTIONAL
        !it_users            TYPE zctsw_user_range_t OPTIONAL
        !it_transports       TYPE zctsw_transport_range_t OPTIONAL
        !it_dates            TYPE zctsw_date_range_t OPTIONAL
      RETURNING
        VALUE(rt_transports) TYPE zctsw_transport_t .
    METHODS get_released_transports
      IMPORTING
        !it_users            TYPE zctsw_user_range_t OPTIONAL
        !it_transports       TYPE zctsw_transport_range_t OPTIONAL
      RETURNING
        VALUE(rt_transports) TYPE zctsw_transport_t .
    METHODS get_objects
      IMPORTING
        !it_transports    TYPE zctsw_transport_range_t OPTIONAL
        !it_objects       TYPE /iwbep/t_cod_select_options OPTIONAL
      RETURNING
        VALUE(rt_objects) TYPE zctsw_object_t .
    METHODS get_migration_status
      IMPORTING
        !i_transport              TYPE trkorr
      RETURNING
        VALUE(r_migration_status) TYPE ctslg_cofile .
    METHODS get_user_full_name
      IMPORTING
        !i_user       TYPE zctsw_transport-as4user
      RETURNING
        VALUE(r_name) TYPE zctsw_transport-user_full_name .
    CLASS-METHODS release_transport
      IMPORTING
        !i_trkorr     TYPE trkorr
      EXPORTING
        !e_error_code TYPE string
        !e_error_text TYPE string .
    CLASS-METHODS get_adt_url_for_object
      IMPORTING
        !i_object_type TYPE trobjtype
        !i_object_name TYPE trobj_name
      EXPORTING
        !e_abs_url     TYPE string
        !e_rel_url     TYPE string .
    CLASS-METHODS get_description_for_object
      IMPORTING
        !i_pgmid             TYPE pgmid
        !i_object            TYPE trobjtype
      RETURNING
        VALUE(r_description) TYPE string .
    CLASS-METHODS get_package_for_object
      IMPORTING
        !i_object        TYPE trobjtype
        !i_obj_name      TYPE sobj_name
      RETURNING
        VALUE(r_package) TYPE devclass.
    CLASS-METHODS get_app_component_for_package
      IMPORTING
        !i_package             TYPE devclass
      RETURNING
        VALUE(r_app_component) TYPE ufps_posid.
    METHODS get_description_for_package
      IMPORTING
        i_packcage_name TYPE zctsw_package_s-package
      RETURNING
        VALUE(r_result) TYPE zctsw_package_s-description.
    METHODS get_description_for_change
      IMPORTING
        i_change_id     TYPE /iwbep/s_mgw_tech_pair-value
      RETURNING
        VALUE(r_result) TYPE zctsw_change_s-description.
    METHODS get_packages
      IMPORTING
        it_packages        TYPE /iwbep/t_cod_select_options
      RETURNING
        VALUE(rt_packages) TYPE gakh_t_tdevc.


  PROTECTED SECTION.
  PRIVATE SECTION.


    METHODS get_user_shortname
      IMPORTING
        i_as4user         TYPE as4user
      RETURNING
        VALUE(r_username) TYPE string .
    METHODS get_change_from_description
      IMPORTING
        i_as4text       TYPE zctsw_transport-as4text
      RETURNING
        VALUE(r_change) TYPE zctsw_transport-change.
    METHODS transport_is_released
      IMPORTING
                i_trkorr             TYPE trkorr
      RETURNING VALUE(r_is_released) TYPE abap_bool.

ENDCLASS.



CLASS zctsw_transport_dao IMPLEMENTATION.


  METHOD constructor.

    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      EXPORTING
        iv_complete  = abap_true
      TABLES
        tt_types_out = gt_object_types.

  ENDMETHOD.

  METHOD get_username_from_email.
*-----------------------------------------------------------------------
* Get username from email
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
*
*-----------------------------------------------------------------------

    IF NOT i_email IS INITIAL.
      SPLIT i_email AT '@' INTO r_username DATA(lv_domain).
    ENDIF.

  ENDMETHOD.

  METHOD get_adt_url_for_object.
*-----------------------------------------------------------------------
* Get ADT URL for an object
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_wb_object  TYPE REF TO cl_wb_object,
          lo_adt_object TYPE REF TO cl_adt_object_reference.

    TRY.
        "Get workbench object
        cl_wb_object=>create_from_transport_key(
          EXPORTING
            p_object                =  i_object_type
            p_obj_name              =  i_object_name
           RECEIVING
             p_wb_object            =  lo_wb_object
           EXCEPTIONS
             objecttype_not_existing = 1
             empty_object_key        = 2
             key_not_available       = 3
             OTHERS                  = 4
        ).

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        "Get URI mapper
        lo_adt_object = cl_adt_uri_mapper=>get_instance( )->if_adt_uri_mapper~map_wb_object_to_objref( lo_wb_object ).

        "Get relative ADT URI for object
        e_rel_url = lo_adt_object->ref_data-uri.

*        " Get server URL
        e_abs_url = cl_http_server=>get_location( protocol = 'HTTPS' ).
        e_abs_url = e_abs_url && e_rel_url.

      CATCH cx_adt_uri_mapping.
        e_abs_url = ''.
        e_rel_url = ''.
    ENDTRY.

  ENDMETHOD.


  METHOD get_change_from_description.
*-----------------------------------------------------------------------
* Get change number from description
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0140918 fstod\906575 14.06.2018 Method created
*-----------------------------------------------------------------------

    DATA: ls_results TYPE match_result.

    TRY.
        FIND PCRE gc_grouping_regex IN i_as4text RESULTS ls_results IGNORING CASE.
        IF NOT ls_results IS INITIAL.
          r_change = i_as4text+ls_results-offset(ls_results-length).
        ENDIF.

        IF r_change IS INITIAL.
          FIND PCRE gc_grouping_regex IN i_as4text RESULTS ls_results IGNORING CASE.
          IF NOT ls_results IS INITIAL.
            r_change = i_as4text+ls_results-offset(ls_results-length).
          ENDIF.
        ENDIF.

        IF r_change IS INITIAL.
          FIND PCRE gc_grouping_regex IN i_as4text RESULTS ls_results IGNORING CASE.
          IF NOT ls_results IS INITIAL.
            r_change = i_as4text+ls_results-offset(ls_results-length).
          ENDIF.
        ENDIF.


      CATCH cx_sy_range_out_of_bounds.
        "No op - we don't care about these errors
    ENDTRY.


  ENDMETHOD.


  METHOD get_description_for_object.
*-----------------------------------------------------------------------
* Get a friendly description for an object
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: ls_object_type TYPE ko100.

    READ TABLE gt_object_types WITH KEY pgmid = i_pgmid
                                        object = i_object
                                   INTO ls_object_type.

    IF NOT ls_object_type-text IS INITIAL.
      r_description = ls_object_type-text.
    ELSE.
      r_description = '<unknown>'.
    ENDIF.

  ENDMETHOD.


  METHOD get_migration_status.
*-----------------------------------------------------------------------
* Get migration status (list of target systems and return codes) for a transport
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: ls_settings TYPE ctslg_settings.

    ls_settings-point_to_missing_steps = abap_true.
    ls_settings-detailed_depiction = abap_true.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr   = i_transport
        is_settings = ls_settings
      IMPORTING
        es_cofile   = r_migration_status.

  ENDMETHOD.


  METHOD get_modifiable_transports.
*-----------------------------------------------------------------------
* Get list of modifiable transports
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: ls_transport TYPE zctsw_transport.

    SELECT  a~trkorr
            a~trfunction
            a~trstatus
            a~tarsystem
            a~as4user
            a~as4date
            a~as4time
            a~strkorr
            b~as4text
            a~client
      FROM e070v AS a                                     "#EC CI_SUBRC
      JOIN e070v AS b
      ON b~trkorr = a~strkorr
      INTO CORRESPONDING FIELDS OF TABLE rt_transports
      WHERE ( a~trfunction = 'S' OR a~trfunction = 'Q' ) "Workbench and customizing tasks
      AND   ( b~trstatus = 'D' OR b~trstatus = 'L' )     "Request Modifiable
      AND   a~as4user IN it_users
      AND   a~trkorr IN it_transports
      ORDER BY a~trkorr.

    "Get documentation for transport
    LOOP AT rt_transports INTO ls_transport.
      IF ls_transport-trfunction = 'S'.
        ls_transport-task_type = 'W'.
      ELSE.
        ls_transport-task_type = 'C'.
      ENDIF.
      ls_transport-sysid = sy-sysid.
      ls_transport-user_name = to_upper( get_user_shortname( ls_transport-as4user ) ).
      ls_transport-user_full_name = get_user_full_name( ls_transport-as4user ).
      CASE ls_transport-trstatus.
        WHEN 'O' OR 'R' OR 'N'.
          ls_transport-task_released = abap_true.
      ENDCASE.

      MODIFY rt_transports FROM ls_transport.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_objects.
*-----------------------------------------------------------------------
* Get a list of objects in transport(s)
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    "Get all objects in transport
    SELECT
      trkorr
      as4pos
      pgmid
      object AS objecttype
      obj_name
      objfunc
      lockflag
      gennum
      lang
      activity
    FROM e071
        INTO CORRESPONDING FIELDS OF TABLE rt_objects     "#EC CI_SUBRC
        WHERE trkorr IN it_transports
        AND   obj_name IN it_objects
        ORDER BY PRIMARY KEY.

    "For requests, get all objects in tasks
    SELECT
      c~trkorr
      c~as4pos
      c~pgmid
      c~object AS objecttype
      c~obj_name
      c~objfunc
      c~lockflag
      c~gennum
      c~lang
      c~activity
    FROM e070 AS a
    JOIN e070 AS b
     ON b~strkorr = a~trkorr
    JOIN e071 AS c
     ON c~trkorr = b~trkorr
    APPENDING CORRESPONDING FIELDS OF TABLE rt_objects    "#EC CI_SUBRC
    WHERE a~trkorr IN it_transports
    AND   obj_name IN it_objects.


    "Fill ADT link and description for objects
    LOOP AT rt_objects INTO DATA(ls_object).
      get_adt_url_for_object(
      EXPORTING
        i_object_type = ls_object-objecttype
        i_object_name = ls_object-obj_name
      IMPORTING
        e_abs_url = ls_object-adt_abs_url
        e_rel_url = ls_object-adt_rel_url ).

      ls_object-dev_package = get_package_for_object( i_object = ls_object-objecttype i_obj_name = ls_object-obj_name(40) ).

      " Get source code ADT link
      CASE ls_object-objecttype.
        WHEN 'CLAS' OR 'METH' OR 'CLSD' OR 'CPRI' OR 'CPUB'. "Class objects
          ls_object-objecttype = 'CLAS'.
          ls_object-obj_name = ls_object-obj_name(30). "For classes, only interested in the class name, not method name.
          get_adt_url_for_object(
            EXPORTING
              i_object_type = 'CLAS'
              i_object_name = ls_object-obj_name
            IMPORTING
              e_abs_url = ls_object-adt_abs_url
              e_rel_url = ls_object-adt_rel_url ).

          ls_object-adt_abs_url = ls_object-adt_abs_url && '/source/main'.
          ls_object-adt_rel_url = ls_object-adt_rel_url && '/source/main'.
          ls_object-adt_eclipse_url = 'adt://' && sy-sysid && ls_object-adt_rel_url.
          ls_object-object_description = 'Class'.
          ls_object-is_source = abap_true.
          ls_object-dev_package = get_package_for_object( i_object = 'CLAS' i_obj_name = ls_object-obj_name(40) ).
        WHEN 'WDYV'. "Web Dynpro view objects
          ls_object-objecttype = ls_object-objecttype.
          ls_object-obj_name = ls_object-obj_name.
          get_adt_url_for_object(
            EXPORTING
              i_object_type = 'WDYV'
              i_object_name = ls_object-obj_name
            IMPORTING
              e_abs_url = ls_object-adt_abs_url
              e_rel_url = ls_object-adt_rel_url ).

          ls_object-adt_abs_url = ls_object-adt_abs_url && '/source'.
          ls_object-adt_rel_url = ls_object-adt_rel_url && '/source'.
          ls_object-adt_eclipse_url = 'adt://' && sy-sysid && ls_object-adt_rel_url.
          ls_object-object_description = get_description_for_object( i_pgmid = ls_object-pgmid i_object = ls_object-objecttype ).
          ls_object-is_source = abap_true.
        WHEN 'WDYC'. "Web Dynpro controller
          ls_object-objecttype = ls_object-objecttype.
          ls_object-obj_name = ls_object-obj_name.
          get_adt_url_for_object(
            EXPORTING
              i_object_type = 'WDYC'
              i_object_name = ls_object-obj_name
            IMPORTING
              e_abs_url = ls_object-adt_abs_url
              e_rel_url = ls_object-adt_rel_url ).

          ls_object-adt_abs_url = ls_object-adt_abs_url && '/source'.
          ls_object-adt_rel_url = ls_object-adt_rel_url && '/source'.
          ls_object-adt_eclipse_url = 'adt://' && sy-sysid && ls_object-adt_rel_url.
          ls_object-object_description = get_description_for_object( i_pgmid = ls_object-pgmid i_object = ls_object-objecttype ).
          ls_object-is_source = abap_true.
          ls_object-dev_package = get_package_for_object( i_object = 'WDYA' i_obj_name = ls_object-obj_name(40) ).
        WHEN 'REPS' OR 'PROG'. " Report
          ls_object-objecttype = ls_object-objecttype.
          ls_object-obj_name = ls_object-obj_name.
          get_adt_url_for_object(
            EXPORTING
              i_object_type = 'REPS'
              i_object_name = ls_object-obj_name
            IMPORTING
              e_abs_url = ls_object-adt_abs_url
              e_rel_url = ls_object-adt_rel_url ).

          ls_object-adt_abs_url = ls_object-adt_abs_url && '/source/main'.
          ls_object-adt_rel_url = ls_object-adt_rel_url && '/source/main'.
          ls_object-adt_eclipse_url = 'adt://' && sy-sysid && ls_object-adt_rel_url.
          ls_object-object_description = get_description_for_object( i_pgmid = ls_object-pgmid i_object = ls_object-objecttype ).
          ls_object-is_source = abap_true.
          ls_object-dev_package = get_package_for_object( i_object = 'PROG' i_obj_name = ls_object-obj_name(40) ).
        WHEN 'FUNC'.
          ls_object-objecttype = ls_object-objecttype.
          ls_object-obj_name = ls_object-obj_name.
          get_adt_url_for_object(
            EXPORTING
              i_object_type = 'FUNC'
              i_object_name = ls_object-obj_name
            IMPORTING
              e_abs_url = ls_object-adt_abs_url
              e_rel_url = ls_object-adt_rel_url ).

          ls_object-adt_abs_url = ls_object-adt_abs_url.
          ls_object-adt_rel_url = ls_object-adt_rel_url.
          ls_object-adt_eclipse_url = 'adt://' && sy-sysid && ls_object-adt_rel_url.
          ls_object-object_description = get_description_for_object( i_pgmid = ls_object-pgmid i_object = ls_object-objecttype ).
          ls_object-is_source = abap_true.
          ls_object-dev_package = get_package_for_object( i_object = 'FUNC' i_obj_name = ls_object-obj_name(40) ).
        WHEN OTHERS.
          ls_object-object_description = get_description_for_object( i_pgmid = ls_object-pgmid i_object = ls_object-objecttype ).
      ENDCASE.
      ls_object-app_component = get_app_component_for_package( ls_object-dev_package ).


      MODIFY rt_objects FROM ls_object.

    ENDLOOP.

    SORT rt_objects BY obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_objects COMPARING obj_name.


  ENDMETHOD.


  METHOD get_released_transports.
*-----------------------------------------------------------------------
* Get a list of released transports
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lv_trkorr TYPE trkorr.

    SELECT * FROM e070v                                   "#EC CI_SUBRC
          INTO CORRESPONDING FIELDS OF TABLE rt_transports
          WHERE ( trfunction = 'K' OR trfunction = 'W' )     "Workbench and customizing tasks
          AND   ( trstatus = 'R' OR trstatus = 'N' )      " Non Modifiable
          AND   as4user IN it_users
          ORDER BY PRIMARY KEY.

    "Get migration steps
    LOOP AT rt_transports INTO DATA(ls_transport).
      ls_transport-sysid = sy-sysid.
      IF ls_transport-strkorr IS INITIAL.
        lv_trkorr = ls_transport-trkorr.
      ELSE.
        lv_trkorr = ls_transport-strkorr.
      ENDIF.
      IF ls_transport-trfunction = 'K'.
        ls_transport-task_type = 'W'.
      ELSE.
        ls_transport-task_type = 'C'.
      ENDIF.
      MODIFY rt_transports FROM ls_transport.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_requests.
*-----------------------------------------------------------------------
* Get a list of requests
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    TYPES: BEGIN OF ty_change_list,
             change(30) TYPE c,
           END OF ty_change_list.

    TYPES: ty_change_list_tab TYPE STANDARD TABLE OF ty_change_list.


    DATA: r_change TYPE /iwbep/t_cod_select_options,
          ls_range TYPE /iwbep/s_cod_select_option.

    DATA: ls_transport   TYPE zctsw_transport,
          lt_change_list TYPE ty_change_list_tab,
          lv_change      TYPE string.


    " Get requests
    SELECT  trkorr
            trfunction
            trstatus
            tarsystem
            as4user
            as4date
            as4time
            strkorr
            as4text
            client
      FROM e070v                                          "#EC CI_SUBRC
      INTO CORRESPONDING FIELDS OF TABLE rt_transports
      WHERE ( trfunction = 'K' OR trfunction = 'W' )   "Workbench and customizing requests
      AND   strkorr = space
      AND   as4user IN it_users
      AND   trkorr IN it_transports
      AND   as4date IN it_dates
      AND   as4text IN it_text
      AND   as4text IN it_change.

    IF i_expand_selection = abap_true.
      " Get requests where user has a task
      IF NOT it_users IS INITIAL.
        SELECT  b~trkorr
                b~trfunction
                b~trstatus
                b~tarsystem
                b~as4user
                b~as4date
                b~as4time
                b~strkorr
                b~as4text
                b~client
          FROM e070v AS a   "Task                    "#EC CI_SUBRC
          JOIN e070v AS b   "Request
           ON   a~strkorr = b~trkorr
          APPENDING CORRESPONDING FIELDS OF TABLE rt_transports
          WHERE   a~as4user IN it_users
          AND     a~trkorr  IN it_transports
          AND     a~as4date IN it_dates
          AND     b~as4text IN it_text.
      ENDIF.

      "Expand selection to all request belonging to same change
      IF NOT rt_transports IS INITIAL.
        LOOP AT rt_transports INTO ls_transport.
          APPEND get_change_from_description( ls_transport-as4text ) TO lt_change_list.
        ENDLOOP.
        SORT lt_change_list.
        DELETE ADJACENT DUPLICATES FROM lt_change_list.

        LOOP AT lt_change_list INTO lv_change .
          FIND PCRE gc_grouping_regex IN lv_change.
          IF sy-subrc = 0.
            ls_range-low = |{ lv_change }*|.
            ls_range-sign = 'I'.
            ls_range-option = 'CP'.
            APPEND ls_range TO r_change.
          ENDIF.
        ENDLOOP.

        SELECT  b~trkorr
                b~trfunction
                b~trstatus
                b~tarsystem
                b~as4user
                b~as4date
                b~as4time
                b~strkorr
                b~as4text
                b~client
          FROM e070v AS a   "Task                    "#EC CI_SUBRC
          JOIN e070v AS b   "Request
           ON   a~strkorr = b~trkorr
          APPENDING CORRESPONDING FIELDS OF TABLE rt_transports
          WHERE   a~as4date IN it_dates
          AND     a~as4text IN r_change.
      ENDIF.
    ENDIF.

    "Delete duplicates
    SORT rt_transports BY trkorr.
    DELETE ADJACENT DUPLICATES FROM rt_transports COMPARING trkorr.

    "Get additional data
    LOOP AT rt_transports INTO ls_transport.
      IF ls_transport-trfunction = 'K'.
        ls_transport-task_type = 'W'. "Workbench
      ELSE.
        ls_transport-task_type = 'C'. "Customizing
      ENDIF.
      ls_transport-sysid = sy-sysid.
      ls_transport-user_name = to_upper( get_user_shortname( ls_transport-as4user ) ).
      ls_transport-user_full_name = get_user_full_name( ls_transport-as4user ).
      CASE ls_transport-trstatus.
        WHEN 'O' OR 'R' OR 'N'.
          ls_transport-task_released = abap_true.
      ENDCASE.
      ls_transport-change = get_change_from_description( ls_transport-as4text ).
      ls_transport-transport_date_time = ls_transport-as4date(4)        &&
                                         '-'                            &&
                                         ls_transport-as4date+4(2)      &&
                                         '-'                            &&
                                         ls_transport-as4date+6(2)      &&
                                         'T'                            &&
                                         ls_transport-as4time(2)        &&
                                         ':'                            &&
                                         ls_transport-as4time+2(2)      &&
                                         ':'                            &&
                                         ls_transport-as4time+4(2)      &&
                                         'Z'.


      MODIFY rt_transports FROM ls_transport.
    ENDLOOP.

    SORT rt_transports BY change DESCENDING as4date DESCENDING as4time DESCENDING.


  ENDMETHOD.


  METHOD get_tasks.
*-----------------------------------------------------------------------
* Get a list of tasks
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------


    DATA: ls_transport TYPE zctsw_transport.

    IF i_parent_request IS INITIAL.
      SELECT  trkorr
              trfunction
              trstatus
              tarsystem
              as4user
              as4date
              as4time
              strkorr
              as4text
              client
        FROM e070v AS a                                   "#EC CI_SUBRC
        INTO CORRESPONDING FIELDS OF TABLE rt_transports
        WHERE   as4user IN it_users
        AND     ( trfunction = 'S' OR trfunction = 'Q' ) "Tasks only
        AND     trkorr  IN it_transports
        AND     as4date IN it_dates
        ORDER BY PRIMARY KEY.

    ELSE.
      SELECT  trkorr
              trfunction
              trstatus
              tarsystem
              as4user
              as4date
              as4time
              strkorr
              client
              as4text
        FROM e070v                                        "#EC CI_SUBRC
        INTO CORRESPONDING FIELDS OF TABLE rt_transports
        WHERE strkorr = i_parent_request
        AND   as4user IN it_users
        AND   trkorr  IN it_transports
        AND   as4date IN it_dates
        ORDER BY PRIMARY KEY.
    ENDIF.

    "Get additional data
    LOOP AT rt_transports INTO ls_transport.
      IF ls_transport-trfunction = 'S'.
        ls_transport-task_type = 'W'.
      ELSE.
        ls_transport-task_type = 'C'.
      ENDIF.
      ls_transport-sysid = sy-sysid.
      ls_transport-user_name = to_upper( get_user_shortname( ls_transport-as4user ) ).
      ls_transport-user_full_name = get_user_full_name( ls_transport-as4user ).
      CASE ls_transport-trstatus.
        WHEN 'O' OR 'R' OR 'N'.
          ls_transport-task_released = abap_true.
      ENDCASE.
      ls_transport-change = get_change_from_description( ls_transport-as4text ).

      MODIFY rt_transports FROM ls_transport.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_user_full_name.

    DATA: ls_user TYPE ty_user_full_name.

    READ TABLE gt_user_full_name WITH KEY bname = i_user INTO ls_user.
    IF sy-subrc = 0.
      r_name = ls_user-name.
    ELSE.
      SELECT SINGLE name_textc INTO r_name              "#EC CI_NOORDER
      FROM user_addr
      WHERE bname = i_user.

      IF sy-subrc = 0.
        ls_user-bname = i_user.
        ls_user-name = r_name.
        APPEND ls_user TO gt_user_full_name.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_user_shortname.
*-----------------------------------------------------------------------
* Get user shortname
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: ls_user TYPE ty_user_short_name.

    READ TABLE gt_user_short_name WITH KEY bname = i_as4user INTO ls_user.
    IF sy-subrc = 0.
      r_username = ls_user-shortname.
    ELSE.
      SELECT SINGLE smtp_addr INTO @DATA(lv_email) FROM "#EC CI_NOORDER
          usr21 AS a
          JOIN adr6 AS b
           ON a~persnumber = b~persnumber
           WHERE a~bname   = @i_as4user.

      r_username = get_username_from_email( CONV #( lv_email ) ).

      ls_user-bname = i_as4user.
      ls_user-shortname = r_username.
      APPEND ls_user TO gt_user_short_name.

    ENDIF.


  ENDMETHOD.


  METHOD release_transport.
*-----------------------------------------------------------------------
* Release the transport
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0156853 fstod\906575 08.03.2019 Method created
*-----------------------------------------------------------------------


    DATA: lt_messages TYPE ctsgerrmsgs.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = i_trkorr
        iv_dialog                   = ' '
        iv_success_message          = ' '
        iv_called_by_adt            = 'X'
        iv_display_export_log       = ' '    " Boolean
      IMPORTING
        et_messages                 = lt_messages
      EXCEPTIONS
        cts_initialization_failure  = 1
        enqueue_failed              = 2
        no_authorization            = 3
        invalid_request             = 4
        request_already_released    = 5
        repeat_too_early            = 6
        object_lock_error           = 7
        object_check_error          = 8
        docu_missing                = 9
        db_access_error             = 10
        action_aborted_by_user      = 11
        export_failed               = 12
        execute_objects_check       = 13
        release_in_bg_mode          = 14
        release_in_bg_mode_w_objchk = 15
        error_in_export_methods     = 16
        object_lang_error           = 17
        OTHERS                      = 18.


    e_error_code = sy-subrc.

    CASE sy-subrc.
      WHEN 1.
        e_error_text = 'CTS_INITIALIZATION_FAILURE'.
      WHEN 2.
        e_error_text = 'ENQUEUE_FAILED'.
      WHEN 3.
        e_error_text = 'NO_AUTHORIZATION'.
      WHEN 4.
        e_error_text = 'INVALID_REQUEST'.
      WHEN 5.
        e_error_text = 'REQUEST_ALREADY_RELEASED'.
      WHEN 6.
        e_error_text = 'REPEAT_TOO_EARLY'.
      WHEN 7.
        e_error_text = 'ERROR_IN_EXPORT_METHODS'.
      WHEN 8.
        e_error_text = 'OBJECT_CHECK_ERROR'.
      WHEN 9.
        e_error_text = 'DOCU_MISSING'.
      WHEN 10.
        e_error_text = 'DB_ACCESS_ERROR'.
      WHEN 11.
        e_error_text = 'ACTION_ABORTED_BY_USER'.
      WHEN 12.
        e_error_text = 'EXPORT_FAILED'.
      WHEN 13.
        e_error_text = 'OTHERS'.
    ENDCASE.

  ENDMETHOD.


  METHOD transport_is_released.
*-----------------------------------------------------------------------
* Is transport released?
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0156853 fstod\906575 20.05.2019 Method created
*-----------------------------------------------------------------------

    DATA: lv_status TYPE trstatus.

    SELECT SINGLE trstatus INTO lv_status                 "#EC CI_SUBRC
      FROM e070
      WHERE trkorr = i_trkorr.

    IF lv_status = 'O'
        OR lv_status = 'R'
        OR lv_status = 'N'.
      r_is_released = abap_true.
    ENDIF.


  ENDMETHOD.

  METHOD get_package_for_object.
*-----------------------------------------------------------------------
* Get package for object
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
*            fstod\906575 05.11.2019 Created object
*-----------------------------------------------------------------------

    SELECT SINGLE devclass FROM tadir INTO r_package
     WHERE object = i_object
     AND   obj_name = i_obj_name.

  ENDMETHOD.

  METHOD get_app_component_for_package.
*-----------------------------------------------------------------------
* Get application component for package
* Get the application component of the package - if no
* component is found, the component of the super-package is returned
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
*            fstod\906575 05.11.2019 Created object
*-----------------------------------------------------------------------

    SELECT SINGLE b~ps_posid, a~parentcl
      FROM tdevc AS a
      INNER JOIN df14l AS b
        ON fctr_id = a~component
      INTO  ( @DATA(lv_app_component), @DATA(lv_super_package) )
      WHERE devclass = @i_package
      AND   as4local = 'A'.

    IF NOT lv_app_component IS INITIAL.
      r_app_component = lv_app_component.
    ELSEIF NOT lv_super_package IS INITIAL.
      get_app_component_for_package( lv_super_package ).
    ENDIF.


  ENDMETHOD.

  METHOD get_description_for_package.

    SELECT SINGLE ctext INTO r_result FROM tdevct
        WHERE devclass = i_packcage_name.

  ENDMETHOD.


  METHOD get_description_for_change.



  ENDMETHOD.

  METHOD get_packages.

    SELECT * FROM tdevc INTO TABLE rt_packages
        WHERE devclass IN it_packages.

  ENDMETHOD.

ENDCLASS.
