*----------------------------------------------------------------------*
***INCLUDE LZFG_CRMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form checkdom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> ACTION
*&      <-- RTMSG
*&---------------------------------------------------------------------*
*检查值域
FORM checkdom USING indomain invalue CHANGING dommsg.
  DATA:it_dd07v TYPE TABLE OF dd07v WITH HEADER LINE,
       wa_dd01t TYPE dd01t.
  CHECK invalue IS NOT INITIAL.
  CLEAR dommsg.
  PERFORM getdomain(zpubform) TABLES it_dd07v USING indomain.
  READ TABLE it_dd07v WITH KEY domvalue_l = invalue.
  IF sy-subrc NE 0.
    SELECT SINGLE *
    INTO wa_dd01t
    FROM dd01t
    WHERE ddlanguage = sy-langu
    AND   as4local = 'A'
    AND   domname = indomain.
    CONCATENATE wa_dd01t-ddtext invalue '不在可输入范围内' INTO dommsg.
  ENDIF.
ENDFORM.
