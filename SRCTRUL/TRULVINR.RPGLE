000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500021016     FWFVIN00F  IF   E           K DISK
000600060925     D*------------*
000700060925     D* DS X DISPLAY
000800060925     D*------------*
000900060925     D  DS_DSPLY       DS
001000060925     D    dConta                      5  0 INZ(*zeros)
001100060925     D    dFiller1                    2    INZ(*blanks)
001200060925     D    dLIB                       10    INZ(*blanks)
001300060925     D    dFiller2                    1    INZ('/')
001400060925     D    dFILE                      10    INZ(*blanks)
001500000000     C*------------------------------------------------------------------------*
001600000000     C* MAIN LINE
001700000000     C*------------------------------------------------------------------------*
001800021016     C*
001900021016     C* Inizializzo contatore dei membri rimossi
002000021016     C                   Z-ADD     *zeros        cntMBRdel         5 0
002100000000     C*
002200020919     C* Leggo tutto il file
002300021016     C                   READ      WFVIN00F
002400021016     C                   DOW       not %eof(WFVIN00F)
002500021016     C                   MOVEL     *blanks       chkDAT            8
002600021016     C                   EVAL      chkDAT  = '20' + MBCDAT
002700021016     C                   IF        chkDAT <= InDATA
002800021016     C                   CALL(e)   'TRULVINC1'
002900021016     C                   PARM                    InLIB
003000021016     C                   PARM                    InFILE
003100021016     C                   PARM                    MBNAME
003200021016     C                   IF        not %error
003300021016     C                   EVAL      cntMBRdel = cntMBRdel + 1
003400021016     C                   ENDIF
003500020919     C                   ENDIF
003600021016     C                   READ      WFVIN00F
003700020919     C                   ENDDO
003800021016     C*
003900021016     C* Visualizzo il numero di membri rimossi dal file TIVIN00R
004000060925     C                   EVAL      dConta = cntMBRdel
004100060925     C                   EVAL      dLIB   = InLIB
004200060925     C                   EVAL      dFILE  = InFILE
004300060925     C     DS_DSPLY      DSPLY
004400991118     C*
004500000000     C                   SETON                                        LR
004600000000     C*------------------------------------------------------------------------*
004700980403     C* *INZSR - OPERAZIONI INIZIALI
004800000000     C*------------------------------------------------------------------------*
004900980403     C     *INZSR        BEGSR
005000020919     C*
005100020919     C     *ENTRY        PLIST
005200021016     C                   PARM                    InDATA            8
005300021016     C                   PARM                    InLIB            10
005400021016     C                   PARM                    InFILE           10
005500000000     C*
005600000000     C                   ENDSR
