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
