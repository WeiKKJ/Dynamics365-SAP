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
*&---------------------------------------------------------------------*
*& Form i_longtext
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_HEAD_NEW_CONTRACTI
*&      --> <LT_TTXIT>-TDID
*&      --> <ZLONGTEXT>
*&---------------------------------------------------------------------*
FORM i_longtext  USING    p_sapno p_tdid
                          p_zlongtext.
  DATA:text_string TYPE string,
       text_table  TYPE TABLE OF zslongtext.
  CHECK p_zlongtext IS NOT INITIAL.
  text_string = p_zlongtext.
  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = p_zlongtext
      i_tabline_length = 132
*     I_UNICODE        =
    TABLES
      et_table         = text_table.
  CALL FUNCTION 'ZFM_DEALLONGTEXT'
    EXPORTING
      intype = 'I'
      tdid   = p_tdid
      sapno  = p_sapno
      sapmk  = 'SD'
    TABLES
      t_text = text_table.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ezsdr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> DATA_NEW_CONTRACTID
*&      <-- RTMSG
*&---------------------------------------------------------------------*
FORM ezsdr  USING    VALUE(p_unlock)
                     p_new_contractid
            CHANGING p_rtmsg.
  IF p_unlock NE 'X'."加锁.
    CALL FUNCTION 'ENQUEUE_EZNEW_CONTRACTID'
      EXPORTING
*       MODE_ZTCRM_SO_HEAD       = 'E'
*       MANDT          = SY-MANDT
        new_contractid = p_new_contractid
*       X_NEW_CONTRACTID         = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
*查找别名
      SELECT SINGLE *
      INTO @DATA(l_usrefus)
      FROM usrefus
      WHERE bname = @sy-msgv1.
      p_rtmsg = |用户{ sy-msgv1 }({ l_usrefus-useralias })正在处理{ p_new_contractid }|.
    ENDIF.
  ELSE.
    CALL FUNCTION 'DEQUEUE_EZNEW_CONTRACTID'
      EXPORTING
*       MODE_ZTCRM_SO_HEAD       = 'E'
*       MANDT          = SY-MANDT
        new_contractid = p_new_contractid
*       X_NEW_CONTRACTID         = ' '
        _scope         = '1'
*       _SYNCHRON      = ' '
*       _COLLECT       = ' '
      .
  ENDIF.
ENDFORM.
