*&---------------------------------------------------------------------*
*& Include ZSDCRM001_SUB_PAI
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'SO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE so_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_so-tab1.
      g_so-pressed_tab = c_so-tab1.
    WHEN c_so-tab2.
      g_so-pressed_tab = c_so-tab2.
    WHEN c_so-tab3.
      g_so-pressed_tab = c_so-tab3.
    WHEN c_so-tab4.
      g_so-pressed_tab = c_so-tab4.
    WHEN c_so-tab5.
      g_so-pressed_tab = c_so-tab5.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZPROVINCEF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zprovincef4 INPUT.
  PERFORM zf4_province.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZCITYF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zcityf4 INPUT.
  PERFORM zf4_city.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZCOUNTYF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zcountyf4 INPUT.
  PERFORM zf4_county.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form zf4_province
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf4_province .
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval.
*  IF gs_out-province IS INITIAL.
    SELECT
      zid AS province,
      zname AS provincet
      FROM ztsd226
      WHERE zlevel = '1'
      ORDER BY province
      INTO TABLE @DATA(lt_province)
       .
    ASSIGN lt_province TO <tab>.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = 'PROVINCE'            "筛选内表里面的字段
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = 'PROVINCE'            "ALV内表字段
        value_org        = 'S'
        callback_program = sy-repid
        callback_form    = 'PROVINCET_FORM'
      TABLES
        value_tab        = <tab>        "需要显示帮助的值内表
      " FIELD_TAB        = FIELD_TAB
        return_tab       = lt_return          "返回值
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

    IF sy-subrc = 0.
      READ TABLE lt_return INTO ls_return INDEX 1.
      gs_out-province = ls_return-fieldval.
    ENDIF.
*  ENDIF.
ENDFORM.
FORM provincet_form TABLES record_tab STRUCTURE seahlpres
           CHANGING shlp TYPE shlp_descr_t
                    callcontrol LIKE ddshf4ctrl.
  DATA: interface LIKE LINE OF shlp-interface.
  CLEAR:interface.
  READ TABLE shlp-interface INTO interface INDEX 1.
*选中后自动带出(SHLPFIELD字段结构F0001)
  interface-shlpfield+4(1) = '2'.
  interface-valfield = 'GS_OUT-PROVINCE_DES'.
  APPEND interface TO shlp-interface.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf4_city
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf4_city .
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval.
*  IF gs_out-city IS INITIAL.
    IF gs_out-province IS NOT INITIAL.
      SELECT
        zid AS city,
        zname AS cityt
        FROM ztsd226
        WHERE zlevel = '2'
        AND zpid = @gs_out-province
        ORDER BY city
        INTO TABLE @DATA(lt_city)
          .
      ASSIGN lt_city TO <tab>.
    ELSE.
      SELECT
        province~zid AS province,
        province~zname AS provincet,
        city~zid AS city,
        city~zname AS cityt
        FROM ztsd226 AS province
        JOIN ztsd226 AS city ON city~zpid = province~zid
        WHERE city~zlevel = '2'
        AND province~zlevel = '1'
        ORDER BY province,city
        INTO TABLE @DATA(lt_city1)
        .
      ASSIGN lt_city1 TO <tab>.
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = 'CITY'            "筛选内表里面的字段
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = 'CITY'             "ALV内表字段
        value_org        = 'S'
        callback_program = sy-repid
        callback_form    = 'CITYT_FORM'
      TABLES
        value_tab        = <tab>           "需要显示帮助的值内表
        return_tab       = lt_return          "返回值
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

    IF sy-subrc = 0.
      READ TABLE lt_return INTO ls_return INDEX 1.
      gs_out-city = ls_return-fieldval.
    ENDIF.
*  ENDIF.
ENDFORM.
FORM cityt_form TABLES record_tab STRUCTURE seahlpres
           CHANGING shlp TYPE shlp_descr_t
                    callcontrol LIKE ddshf4ctrl.
  READ TABLE shlp-interface INTO DATA(interface) INDEX 1.
  IF gs_out-province IS NOT INITIAL.
    interface-shlpfield+4(1) = '2'.
    interface-valfield = 'GS_OUT-CITY_DES'.
    APPEND interface TO shlp-interface.
  ELSE.
    interface-shlpfield+4(1) = '1'.
    interface-valfield = 'GS_OUT-PROVINCE'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '2'.
    interface-valfield = 'GS_OUT-PROVINCE_DES'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '4'.
    interface-valfield = 'GS_OUT-CITY_DES'.
    APPEND interface TO shlp-interface.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf4_county
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf4_county .
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval.
*  IF gs_out-county IS INITIAL.
    IF gs_out-city IS NOT INITIAL.
      SELECT
        zid AS county,
        zname AS countyt
        FROM ztsd226
        WHERE zlevel = '3'
        AND zpid = @gs_out-city
        ORDER BY county
        INTO TABLE @DATA(lt_county)
          .
      ASSIGN lt_county TO <tab>.
    ELSEIF gs_out-province IS NOT INITIAL.
      SELECT
        city~zid AS city,
        city~zname AS cityt,
        county~zid AS county,
        county~zname AS countyt
        FROM ztsd226 AS city
        JOIN ztsd226 AS county ON county~zpid = city~zid
        WHERE city~zpid = @gs_out-province
        AND county~zlevel = '3'
        AND city~zlevel = '2'
        ORDER BY city,county
        INTO TABLE @DATA(lt_county1)
        .
      ASSIGN lt_county1 TO <tab>.
    ELSE.
      SELECT
        province~zid AS province,
        province~zname AS provincet,
        city~zid AS city,
        city~zname AS cityt,
        county~zid AS county,
        county~zname AS countyt
        FROM ztsd226 AS province
        JOIN ztsd226 AS city ON city~zpid = province~zid
        JOIN ztsd226 AS county ON county~zpid = city~zid
        WHERE county~zlevel = '3'
        AND city~zlevel = '2'
        AND province~zlevel = '1'
        ORDER BY province,city,county
        INTO TABLE @DATA(lt_county2)
         .
      ASSIGN lt_county2 TO <tab>.
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = 'COUNTY'            "筛选内表里面的字段
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = 'COUNTY'      "ALV内表字段
        value_org        = 'S'
        callback_program = sy-repid
        callback_form    = 'COUNTYT_FORM'
      TABLES
        value_tab        = <tab>         "需要显示帮助的值内表
        return_tab       = lt_return          "返回值
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

    IF sy-subrc = 0.
      READ TABLE lt_return INTO ls_return INDEX 1.
      gs_out-county = ls_return-fieldval.
    ENDIF.
*  ENDIF.
ENDFORM.
FORM countyt_form TABLES record_tab STRUCTURE seahlpres
           CHANGING shlp TYPE shlp_descr_t
                    callcontrol LIKE ddshf4ctrl.
  DATA: interface LIKE LINE OF shlp-interface.
  CLEAR:interface.
  READ TABLE shlp-interface INTO interface INDEX 1.
  IF gs_out-city IS NOT INITIAL.
    interface-shlpfield+4(1) = '2'.
    interface-valfield = 'GS_OUT-COUNTY_DES'.
    APPEND interface TO shlp-interface.
  ELSEIF gs_out-province IS NOT INITIAL.
    interface-shlpfield+4(1) = '1'.
    interface-valfield = 'GS_OUT-CITY'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '2'.
    interface-valfield = 'GS_OUT-CITY_DES'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '4'.
    interface-valfield = 'GS_OUT-COUNTY_DES'.
    APPEND interface TO shlp-interface.
  ELSE.
    interface-shlpfield+4(1) = '1'.
    interface-valfield = 'GS_OUT-PROVINCE'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '2'.
    interface-valfield = 'GS_OUT-PROVINCE_DES'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '3'.
    interface-valfield = 'GS_OUT-CITY'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '4'.
    interface-valfield = 'GS_OUT-CITY_DES'.
    APPEND interface TO shlp-interface.
    interface-shlpfield+4(1) = '6'.
    interface-valfield = 'GS_OUT-COUNTY_DES'.
    APPEND interface TO shlp-interface.
  ENDIF.
ENDFORM.
