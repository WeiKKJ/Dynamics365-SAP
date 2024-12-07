FUNCTION-POOL zfg_crm.                      "MESSAGE-ID ..

* INCLUDE LZFG_CRMD...                       " Local class definition
INCLUDE zfmdatasave.

DEFINE fillmsg.
  rtype = &1.
  rtmsg = &2.
  IF rtype = 'E'.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.
END-OF-DEFINITION.

DEFINE checkinitial.
  IF &1 IS INITIAL.
    rtype = 'E'.
    rtmsg = &2 && '为空'.
    CONDENSE rtmsg NO-GAPS.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.
END-OF-DEFINITION.

DATA:ls_partner          TYPE bus_ei_extern,
     ls_customer         TYPE cmds_ei_extern,
     lt_addresses        TYPE bus_ei_bupa_address_t,
     ls_addresses        TYPE bus_ei_bupa_address,
     lt_phone            TYPE bus_ei_bupa_telephone_t,
     ls_phone            TYPE bus_ei_bupa_telephone,
     lt_remarks_a        TYPE bus_ei_bupa_addressremark_t,
     ls_remarks_a        TYPE bus_ei_bupa_addressremark,
     lt_bank             TYPE bus_ei_bupa_bankdetail_t,
     ls_bank             TYPE bus_ei_bupa_bankdetail,
     it_taxnumbers       TYPE bus_ei_bupa_taxnumber_t,
     wa_taxnumbers       TYPE bus_ei_bupa_taxnumber,
     lt_tax_ind          TYPE cmds_ei_tax_ind_t,
     ls_tax_ind          TYPE cmds_ei_tax_ind,
     ls_header           TYPE cmds_ei_header,
     ls_central_data     TYPE cmds_ei_central_data,
     ls_company_data     TYPE cmds_ei_cmd_company,
     lt_company          TYPE cmds_ei_company_t,
     ls_company          TYPE cmds_ei_company,
     ls_sales_data       TYPE cmds_ei_cmd_sales,
     lt_sales            TYPE cmds_ei_sales_t,
     ls_sales            TYPE cmds_ei_sales,
     ls_data             TYPE cvis_ei_extern,
     lt_data             TYPE cvis_ei_extern_t,
     lt_return           TYPE bapiretm,
     ls_return           TYPE bapireti,
     ls_msg              TYPE bapiretc,
     lt_partnerguid_list TYPE bu_partner_guid_t,
     ls_partnerguid_list LIKE LINE OF lt_partnerguid_list,
     lt_smtp             TYPE bus_ei_bupa_smtp_t,
     ls_smtp             TYPE bus_ei_bupa_smtp,
     lt_roles            TYPE bus_ei_bupa_roles_t,
     ls_roles            TYPE bus_ei_bupa_roles.
DATA:mdm_intab  TYPE TABLE OF zssd_010 WITH HEADER LINE,
     mdm_outtab TYPE TABLE OF zfm_sd_mdm_custom_s1 WITH HEADER LINE.
DATA:mdm_rtype TYPE bapi_mtype,
     mdm_rtmsg TYPE bapi_msg.
DATA:crmurl   TYPE string,
     crmappId TYPE string.
TYPES:BEGIN OF ty_header,
        name  TYPE string,
        value TYPE string,
        cdata TYPE string,
        xdata TYPE xstring,
      END OF ty_header.
DATA:header TYPE TABLE OF ty_header.
FIELD-SYMBOLS:<setbapikey>  TYPE any,
              <setbapixkey> TYPE any.
DATA:setbapi  TYPE char60,
     setbapix TYPE char60.
DEFINE setbapix.
  UNASSIGN:<setbapikey>,<setbapixkey>.
  CLEAR:setbapi,setbapix.
  setbapi = to_upper( |{ &1 }-{ &2 }| ).
  ASSIGN (setbapi) TO <setbapikey>.
  IF sy-subrc EQ 0.
    <setbapikey> = &3.
    setbapix = to_upper( |{ &1 }X-{ &2 }| ).
    ASSIGN (setbapix) TO <setbapixkey>.
    IF sy-subrc EQ 0.
      <setbapixkey> = 'X'.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.
