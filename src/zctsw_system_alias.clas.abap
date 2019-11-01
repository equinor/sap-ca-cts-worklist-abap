class ZCTSW_SYSTEM_ALIAS definition
  public
  final
  create public .

public section.

  interfaces /IWFND/IF_MGW_DEST_FINDER_BADI .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCTSW_SYSTEM_ALIAS IMPLEMENTATION.


  method /IWFND/IF_MGW_DEST_FINDER_BADI~GET_SYSTEM_ALIASES.
*-----------------------------------------------------------------------
* Redefine system aliases for local development purposes.
* This is implemented as local object only for development engineering
* purposes: we do not want a new change for adjustments to tools like
* CTS and code review tools, which are owned by developers and not by the business.
* The question is - should local alias not be defaulted if not defined
* for service?
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* $ctsworklist ADKRA\774888 31.10.2019 Method created in local package.
*   Added implementation for ZCTSW_TRASNPORT_SRV_0001 so that CTS
*   code review tool works
*-----------------------------------------------------------------------
    DATA ls_system_alias like line of ct_system_aliases.
    case iv_service_id.
       when 'ZCTSW_TRANSPORT_SRV_0001'.
        read table ct_system_aliases with key system_alias = 'LOCAL' ASSIGNING FIELD-SYMBOL(<fs_alias>).
        if ( sy-subrc <> 0 ).
            ls_system_alias-is_default = abap_true.
            ls_system_alias-system_alias = 'LOCAL'.
            append ls_system_alias to ct_system_aliases.
        endif.
    endcase.


  endmethod.
ENDCLASS.
