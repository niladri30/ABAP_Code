  METHOD UPDATE_TABLE.

    DATA: VARKEY TYPE RSTABLE-VARKEY.
    DATA: LT_ZP2P_LOG_WH_TAX TYPE STANDARD TABLE OF ZP2P_LOG_WH_TAX.

    VARKEY = SY-MANDT.
    "Add Lock
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        TABNAME        = IV_TABLE
        VARKEY         = VARKEY
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC <> 0.
      "Do nothing
    ENDIF.

    IF IV_TABLE = LC_LFBW.

      MODIFY LFBW FROM TABLE IT_V3_FILTERED.
      IF SY-SUBRC = 0.
        COMMIT WORK AND WAIT.
        WAIT UP TO 1 SECONDS.

        "Udpate ZP2P_LOG_WH_TAX  table
        ZCL_WITHOLDING_TAX_AUTOUPD=>UPDATE_TABLE(
                                     EXPORTING
                                       IV_TABLE           = LC_ZP2P_LOG_WH_TAX
                                       IT_V3_FILTERED     = IT_V3_FILTERED
                                       IV_V2              = IV_V2
                                                ).

      ENDIF.

    ELSEIF IV_TABLE = LC_ZP2P_LOG_WH_TAX.

      REFRESH: LT_ZP2P_LOG_WH_TAX.

      LT_ZP2P_LOG_WH_TAX = VALUE #( FOR <FS_ZTAB> IN IT_V3_FILTERED
                                       (
                                        LIFNR_V2     = IV_V2
                                        LIFNR        = <FS_ZTAB>-LIFNR
                                        BUKRS        = <FS_ZTAB>-BUKRS
                                        WITHT        = <FS_ZTAB>-WITHT
                                        WT_SUBJCT    = <FS_ZTAB>-WT_SUBJCT
                                        QSREC        = <FS_ZTAB>-QSREC
                                        WT_WTSTCD    = <FS_ZTAB>-WT_WTSTCD
                                        WT_WITHCD    = <FS_ZTAB>-WT_WITHCD
                                        WT_EXNR      = <FS_ZTAB>-WT_EXNR
                                        WT_EXRT      = <FS_ZTAB>-WT_EXRT
                                        WT_EXDF      = <FS_ZTAB>-WT_EXDF
                                        WT_EXDT      = <FS_ZTAB>-WT_EXDT
                                        WT_WTEXRS    = <FS_ZTAB>-WT_WTEXRS
                                        UPDATE_DATE  = CL_ABAP_CONTEXT_INFO=>GET_SYSTEM_DATE( )
                                       )
                                  ).

      MODIFY ZP2P_LOG_WH_TAX FROM TABLE LT_ZP2P_LOG_WH_TAX.
      IF SY-SUBRC = 0.
        COMMIT WORK AND WAIT.
        WAIT UP TO 1 SECONDS.
      ENDIF.

    ENDIF."IV_TABLE

    "Remove Lock
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
*       MODE_RSTABLE       = 'E'
        TABNAME = IV_TABLE
        VARKEY  = VARKEY.

    IF SY-SUBRC <> 0.
      " Implement suitable error handling here
    ENDIF.

    REFRESH: LT_ZP2P_LOG_WH_TAX.

  ENDMETHOD.