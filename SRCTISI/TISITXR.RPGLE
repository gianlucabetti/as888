000100980904     H*------------------------------------------------------------------------*
000200980904     H DECEDIT('0,') DATEDIT(*DMY.)
000300980904     F*------------------------------------------------------------------------*
000400980904     F* DATA BASE
000500040325     F*------------------------------------------------------------------------*
000600040325     FAZDSS01L  IF   E           K DISK    RENAME(AZDSS000:AZDSS0IN)
000700040325     FAZDSS03L  UF   E           K DISK    PREFIX(W_)
000800020416     D*-------------------
000900020416     D* ARCHITETTURA
001000020416     D*-------------------
001100020416     D KPJBA         E DS
001200040325     D*------------------
001300040325     D* DS "XSRDA8" - INVERSIONE DATA (8)
001400040325     D*------------------
001500040325     D WLBDA8          DS                  INZ
001600040325     D  G08DAT                 1      8  0
001700040325     D  G08INV                 9     16  0
001800040325     D  G08ERR                17     17
001900040325     D  G08TGI                18     22  0
002000980904     C*------------------------------------------------------------------------*
002100980904     C* MAIN LINE
002200980904     C*------------------------------------------------------------------------*
002300980904     C*
002400040325     C* POSIZIONAMENTO E PRIMA LETTURA SINONIMI
002500980904     C                   EXSR      SETFIL
002600980904     C*
002700980904     C* CICLO FINO A FINE FILE
002800020416     C     $FINE         DOWEQ     'N'
002900980904     C*
003000040325     C* ELIMINA IL SINONIMO NN + NECESSARIO (IN QUANTO "SERVITO" GIA' DA 1 SINONIMO "GENERICO")
003100040325     C                   EXSR      DLTSIN
003200980904     C*
003300040325     C* LETTURA SUCCESSIVA SINONIMI
003400980904     C                   EXSR      LETFIL
003500020416     C                   ENDDO                                                  *FINE FILE
003600020410     C*
003700980904     C                   SETON                                        LR
003800980904     C*------------------------------------------------------------------------*
003900980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
004000980904     C*------------------------------------------------------------------------*
004100980904     C     SETFIL        BEGSR
004200980904     C*
004300040319     C                   SETOFF                                       99
004400040325     C     *LOVAL        SETLL     AZDSS01L                                     *FINE ARCHIVIO
004500040319     C     *IN99         IFEQ      *ON
004600980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
004700040319     C                   ELSE
004800980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
004900040319     C                   ENDIF
005000980904     C*
005100980904     C                   ENDSR
005200980904     C*------------------------------------------------------------------------*
005300980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
005400980904     C*------------------------------------------------------------------------*
005500980904     C     LETFIL        BEGSR
005600980904     C*
005700980904     C* LETTURA FINO A:
005800980904     C*  - FINE FILE
005900980904     C*  - TROVATO RECORD VALIDO
006000980904     C                   MOVEL     'N'           WRECOK
006100040319     C     $FINE         DOWEQ     'N'
006200980904     C     WRECOK        ANDEQ     'N'
006300040325     C                   READ      AZDSS01L                               99
006400040319     C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
006500980904     C                   MOVEL     'S'           $FINE
006600040319     C                   ELSE
006700980904     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
006800040319     C                   ENDIF
006900040319     C                   ENDDO
007000980904     C*
007100980904     C                   ENDSR
007200980904     C*------------------------------------------------------------------------*
007300040319     C* CHKREC - CONTROLLA VALIDITA' RECORD
007400980904     C*------------------------------------------------------------------------*
007500980904     C     CHKREC        BEGSR
007600020410     C*
007700020410     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
007800040319     C*
007900040325     C* SE IL RECORD NN E' GIA' ANNULLATO VERIFICO SE TRATTASI D SINONIMO "GENERICO" (CON '*')
008000040325     C                   IF        DSSATB = *blanks
008100040325     C     '*'           SCAN      DSSSIN        wPosAst           2 0
008200040325     C                   IF        %found
008300040325     C                   ELSE
008400040325     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
008500040325     C                   ENDIF
008600040325     C                   ELSE
008700040325     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
008800040319     C                   ENDIF
008900020416     C*
009000980904     C                   ENDSR
009100980904     C*------------------------------------------------------------------------*
009200040325     C* DLTSIN - ELIMINA I SINONIMI GIA' "SERVITI" DA SINONIMI GENERICO (CON '*')
009300980904     C*------------------------------------------------------------------------*
009400040325     C     DLTSIN        BEGSR
009500980904     C*
009600040325     C     KEYDSS03_P    SETLL     AZDSS03L
009700040325     C                   IF        %equal(AZDSS03L)
009800040325     C     KEYDSS03_P    READE     AZDSS03L
009900040325     C                   DOW       not %eof(AZDSS03L)
010000040325     C* Ignoro il sinonimo corrente
010100040325     C                   IF        DSSSIN = W_DSSSIN
010200040325     C                   ELSE
010300040325     C                   IF        %subst(W_DSSSIN:1:wPosAst-1) =
010400040325     C                             %subst(DSSSIN:1:wPosAst-1)
010500040325     C* SE IL RECORD NON E' MAI STATO TRASMESSO DELETO FISICAMENTE
010600040325     C                   IF        W_DSSDTR = *ZEROS
010700040325     C                   DELETE    AZDSS000
010800040325     C                   ELSE
010900040325     C* ALTRIMENTI LO ANNULLO LOGICAMENTE
011000040325     C                   MOVEL     'A'           W_DSSATB                     *FLAG ANNULLAMENTO
011100040325     C                   MOVEL     *BLANKS       W_DSSFTR                       *FLAG TRASMISSIONE
011200040325     C                   MOVEL     KNMUS         W_DSSUTE                       *UTENTE INS/VAR
011300040325     C                   Z-ADD     DATCOR        W_DSSDIV                       *DATA INS/VAR
011400040325     C                   UPDATE    AZDSS000
011500040325     C                   ENDIF
011600040325     C                   ENDIF
011700040325     C                   ENDIF
011800040325     C     KEYDSS03_P    READE     AZDSS03L
011900040325     C                   ENDDO
012000040325     C                   ENDIF
012100020416     C*
012200980904     C                   ENDSR
012300980904     C*------------------------------------------------------------------------*
012400980904     C* *INZSR - OPERAZIONI INIZIALI
012500980904     C*------------------------------------------------------------------------*
012600980904     C     *INZSR        BEGSR
012700020416     C*
012800020416     C* RICEVIMENTO PARAMETRI
012900020416     C     *ENTRY        PLIST
013000020416     C                   PARM                    KPJBA
013100040325     C*
013200040325     C* CALCOLA LA DATA CORRENTE
013300040325     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
013400040325     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
013500040325     C                   Z-ADD     WN8           G08DAT
013600040325     C                   Z-ADD     *ZEROS        G08INV
013700040325     C                   MOVEL     '0'           G08ERR
013800040325     C                   CALL      'XSRDA8'
013900040325     C                   PARM                    WLBDA8
014000040325     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
014100040325     C*
014200040325     C* DEFINIZIONE CHIAVI
014300040325     C*
014400040325     C* Chiave su AZDSS01L
014500040325     C     KEYDSS03_P    KLIST
014600040325     C                   KFLD                    DSSNAZ
014700040325     C                   KFLD                    DSSPRV
014800040325     C                   KFLD                    DSSCAP
014900040325     C                   KFLD                    DSSLOC
015000040325     C                   KFLD                    DSSRAG
015100040325     C                   KFLD                    DSSKWI
015200020410     C*
015300020410     C* VARIABILI DI WRK
015400020410     C                   MOVEL     'N'           $FINE             1            *FINE PGM
015500020410     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
015600020416     C*
015700980904     C                   ENDSR
