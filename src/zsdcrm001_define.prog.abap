*&---------------------------------------------------------------------*
*& 包含               ZSDCRM001_DEFINE
*&---------------------------------------------------------------------*
DEFINE destroy_control_object.
  IF &1 IS NOT INITIAL .
    CALL METHOD &1->free
      EXCEPTIONS
        OTHERS = 1.
    FREE: &1 .
  ENDIF.
END-OF-DEFINITION.
DEFINE getdomdes.
  READ TABLE lt_domdes INTO wa_domdes WITH KEY domname = &1 domvalue_l = &2 BINARY SEARCH.
  IF sy-subrc = 0.
    &3 = wa_domdes-ddtext.
  ENDIF.
END-OF-DEFINITION.

FORM checkmenge USING p_a CHANGING p_p TYPE p.
  DATA:p_flg   TYPE char1,
       p_atwrt TYPE atwrt.
  CLEAR p_p.
  p_atwrt = p_a.
  PERFORM checkmenge(zpubform) CHANGING p_atwrt p_flg.
  IF p_flg NE 'E'.
    PERFORM delqfw(zpubform) CHANGING p_atwrt.
    p_p = p_atwrt.
  ENDIF.
ENDFORM.
