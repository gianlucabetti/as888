000100000000     H*--------------------------------------------------------------------------------------------*
000200080904     H* Reperimento/impostazione stato log "multi-spool" - X STORED-PROCEDURE
000300000000     H*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*--------------------------------------------------------------------------------------------*
000600050214     D* DS di procedura
000700050214     D*------------------
000800100219     D TRBMM9DS      E DS
000900080904     D In_LibFil       S             10A
001000080904     D In_LibObj       S             10A
001100080904     D In_IDProg       S              7A
001200080904     D In_Stato        S              1A
001300080904     D In_Messaggio    S            128A
001400080904     D Out_EsitoSP     S              1A
001500050214     D*------------------
001600000000     C*--------------------------------------------------------------------------------------------*
001700000000     C* Main lines
001800000000     C*--------------------------------------------------------------------------------------------*
001900100219     C                   CLEAR                   TRBMM9DS
002000080904     C                   EVAL      �CM9PROG   = In_IDProg
002100080904     C                   EVAL      �CM9STSIN  = In_Stato
002200080904     C                   EVAL      �CM9MSGIN  = In_Messaggio
002300080904     C*
002400100219     C                   CALL(e)   'TRBMM9C'
002500080904     C                   PARM                    In_LibFil
002600080904     C                   PARM                    In_LibObj
002700100219     C                   PARM                    TRBMM9DS
002800050222     C*
002900080904     C                   IF        �CM9ERR = '1'
003000080904     C                   EVAL      Out_EsitoSP = 'E'
003100050214     C                   ENDIF
003200080904     C*
003300080904     C                   SETON                                        LR
003400040406     C*--------------------------------------------------------------------------------------------*
003500000000     C* *inzsr - operazioni iniziali
003600040406     C*--------------------------------------------------------------------------------------------*
003700000000     C     *inzsr        BEGSR
003800040406     C*
003900000000     C* ricevimento parametri
004000040406     C     *ENTRY        PLIST
004100100219     C                   PARM                    TRBMM9DS
004200050214     C*
004300000000     C                   ENDSR
