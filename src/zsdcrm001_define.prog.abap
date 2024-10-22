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
