000100960221     H DECEDIT('0,') DATEDIT(*DMY.)
000200960221?     *--------------------------------------------------------------*
000300020924      * BCH10U1BA                                                    *
000400960221      *  *---------------------------------------------------------* *
000500020923      *    IMPOSTA CODA DA PROFILO                                   *
000600020923      *    - SOLO SE S.I. FIL                                        *
000700020923      *      (SE NUOVA CODA NON ESISTE LASCIA INVARIATA            * *
000800020924      *      se profilo EDPCEDXXX forza le code non come terminal    *
000900020924      *      ma KXXX......                                           *
001000020923     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            CHKOBJ
001100960221?     *  DS                                                          *
001200960221     D KPJBA         E DS
001300021113     D TIBS36        E DS                  EXTNAME(TIBS36DS)
001400020923     D                 DS
001500020923     D  WIJOBQ                 1     10
001600020923     D  WIJB1                  1      1
001700020923     D  WIJB27                 2      7
001800020923     D                 DS
001900020923     D  WOJOBQ                 1     10
002000020923     D  WOJB1                  1      1
002100020923     D  WOJB24                 2      4
002200020923     D  WOJB50                 5     10
002300960221?     *--------------------------------------------------------------*
002400960221?     *  CICLO PRINCIPALE                                            *
002500960221?     *--------------------------------------------------------------*
002600020923      *  SE SISTEMA INFORMATIVO FIL ....
002700020924     c                   setoff                                       2131
002800020923     C                   MOVEL     KNSIF         WWSIF             3
002900020923     C     WWSIF         IFEQ      'FIL'
003000020923     C                   MOVEL     KQEBI         WIJOBQ
003100020923     C     KQIEB         IFNE      *BLANK
003200020923     C                   MOVEL     KQIEB         WIJOBQ
003300020923     C                   ENDIF
003400020923     C     wijobq        IFeq      *BLANK
003500020923     C                   MOVEL     'KSDIFFE   '  WIJOBQ
003600020923     C                   ENDIF
003700020924     c     KNMUS         ifne      'EDPCEDXXX'                                  forza coda generica
003800021113     C                   CLEAR                   TIBS36
003900021113     C                   MOVEL     KNMUS         I36ute
004000021113     C                   MOVEL     'L'           I36TLA
004100021113     C                   CALL      'TIBS36R'                            31
004200021113     C                   PARM                    TIBS36
004300020924     c                   endif
004400020923     C* IMPOSTO CODA CON TERMINAL UTENTE
004500020923     C                   MOVEL     WIJB1         WOJB1
004600020923     C                   MOVEL     WIJB27        WOJB50
004700021113     C                   MOVEL     O36TFP        WOJB24
004800020924     c     KNMUS         ifeq      'EDPCEDXXX'                                  forza coda generica
004900020924     C                   MOVEL     'XXX'         WOJB24
005000020924     c                   endif
005100020923     C* CONTROLLO ESISTENZA "NUOVA" CODA
005200020923     C                   MOVEA     WOJOBQ        CMD(12)                        *
005300020923     C                   CALL      'QCMDEXC'                            21      *
005400020923     C                   PARM                    CMD                            *
005500020923     C                   PARM      80            LEN              15 5          *
005600020924     C  N21              DO
005700020923     C* SE OK IMPOSTO NUOVA CODA (SEMPRE ANCHE COME FORZATA)
005800020923     C                   MOVEL     WOJOBQ        KQEBI
005900020923     C                   MOVEL     WOJOBQ        KQIEB
006000020923     C                   ENDDO
006100020923     C*
006200960221     C                   END
006300960221      *
006400960221     C                   SETON                                        LR
006500960221?     *--------------------------------------------------------------*
006600960221?     *  *INZSR: Operazioni iniziali                                 *
006700960221?     *--------------------------------------------------------------*
006800960221     C     *INZSR        BEGSR
006900960221     C     *ENTRY        PLIST
007000960221     C                   PARM                    KPJBA
007100960221     C                   ENDSR
007200020923** CHKOBJ
007300020923CHKOBJ OBJ(A234567890) OBJTYPE(*JOBQ)
