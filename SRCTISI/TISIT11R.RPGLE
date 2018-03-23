000100970526     H*------------------------------------------------------------------------*
000200020412     H* Gestione file anagrafico Destinatari disagiati/supermercati (AZDST00F)
000300040601     H* - Copia sinonimi su altro destinatario -
000400970526     H*------------------------------------------------------------------------*
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900040601     FTISIT11D  CF   E             WORKSTN
001000970526     F*------------
001100040601     Fazdst04l  IF   E           K DISK
001200040601     Fazdss03l  IF   E           K DISK
001300040601     Fazdss00f  UF A E             DISK
001400040601     F                                     RENAME(azdss000:azdssxxf)
001500970526     D*------------------------------------------------------------------------*
001600970526     D* ARCHITETTURA
001700970526     D*------------
001800970526     D KPJBA         E DS
001900040601     D*------------------
002000040601     D* DS "XSRDA8" - INVERSIONE DATA (8)
002100040601     D*------------------
002200040601     D WLBDA8          DS                  INZ
002300040601     D  G08DAT                 1      8  0
002400040601     D  G08INV                 9     16  0
002500040601     D  G08ERR                17     17
002600040601     D  G08TGI                18     22  0
002700040601     D*------------
002800040601     D* VARIABILI D WRK
002900040601     D*------------
003000040601     D SAVKPJBU        S                   LIKE(KPJBU)
003100040601     D AZDST         E DS                  EXTNAME(AZDST00F)
003200040601     D DA_DST        E DS                  EXTNAME(AZDST00F) PREFIX(DA_)
003300970526     C*------------------------------------------------------------------------*
003400970526     C* MAIN LINES
003500970526     C*------------------------------------------------------------------------*
003600970526     C*
003700040601     C* CICLO FINCHE' NN PREMUTI O F12=Ritorno o F06=Conferma
003800040601     C                   DO        *HIVAL
003900970526     C*
004000040601     C* EMETTO LA VIDEATA D RICHIESTA DESTINATARIO "SU"
004100040601     C                   EXFMT     SIT11DW1
004200040601     C*
004300040601     C* VERIFICO IL DESTINATARIO SCELTO
004400040601     C                   EXSR      CHKDST
004500970526     C*
004600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
004700040601     C                   IF        *INKF = *ON AND                              *F06 PREMUTO
004800040601     C                             ERRORE = '0'                                 *INPUT OK
004900040601     C                   EXSR      ELABORA
005000040601     C                   LEAVE
005100040601     C                   ENDIF
005200040601     C*
005300040601     C                   IF        *INKL = *ON                                  *F12 PREMUTO
005400040603     C                   EVAL      ERRORE = '1'
005500040601     C                   LEAVE
005600040601     C                   ENDIF
005700040601     C                   ENDDO
005800991108     C*
005900970526     C* FINE PGM
006000970526     C                   SETON                                        LR
006100970526     C*------------------------------------------------------------------------*
006200040601     C* ELABORA - ROUTINE DI ELABORAZIONE
006300970526     C*------------------------------------------------------------------------*
006400040601     C     ELABORA       BEGSR
006500040601     C*
006600040601     C* Effettuo una 1° lettura x reperire i dati del destinatario "DA"
006700040601     C     INCODDST      CHAIN     azdst04l
006800040601     C                   IF        %found(azdst04l)
006900040601     C                   EVAL      DA_DST = AZDST
007000040601     C*
007100040601     C* Effettuo una 2° lettura x reperire i dati del destinatario "A"
007200040601     C                   MOVEL     VIWCOD        VIWCOD_NUM        7 0
007300040601     C     VIWCOD_NUM    CHAIN     azdst04l
007400040601     C                   IF        %found(azdst04l)
007500040601     C     KEYDSS3       SETLL     azdss03l
007600040601     C                   IF        %equal(azdss03l)
007700040601     C     KEYDSS3       READE     azdss03l
007800040601     C* CICLO X TUTTI I SINONIMI DEL DESTINATARIO "DA"
007900040601     C                   DOW       not %eof(azdss03l)
008000040601     C                   EVAL      DSSNAZ = DSTNAZ
008100040601     C                   EVAL      DSSPRV = DSTPRV
008200040601     C                   EVAL      DSSCAP = DSTCAP
008300040601     C                   EVAL      DSSLOC = DSTLOC
008400040601     C                   EVAL      DSSRAG = DSTRAG
008500040601     C                   EVAL      DSSKWI = DSTKWI
008600040601     C                   MOVEL     *BLANKS       DSSATB
008700040601     C                   MOVEL     *BLANKS       DSSFTR
008800040601     C                   MOVEL     KNMUS         DSSUTE
008900040601     C                   Z-ADD     DATCOR        DSSDIV
009000040601     C                   WRITE(e)  azdssxxf
009100040601     C     KEYDSS3       READE     azdss03l
009200040601     C                   ENDDO
009300040601     C                   ENDIF
009400040601     C*
009500040601     C                   ELSE
009600040601     C                   EVAL      ERRORE = '1'
009700040601     C                   EVAL      VIWMSG = 'Destinatario su cui copiare i ' +
009800040601     C                                      'sinonimi non trovato.'
009900040601     C                   ENDIF
010000040601     C                   ELSE
010100040601     C                   EVAL      ERRORE = '1'
010200040601     C                   EVAL      VIWMSG = 'Destinatario da cui copiare i ' +
010300040601     C                                      'sinonimi non trovato.'
010400040601     C                   ENDIF
010500000407     C*
010600970526     C                   ENDSR
010700040601     C*------------------------------------------------------------------------*
010800040601     C* CHKDST - CONTROLLA CODICE DESTINATARIO
010900040601     C*------------------------------------------------------------------------*
011000040601     C     CHKDST        BEGSR
011100040601     C*
011200040601     C                   EVAL      ERRORE = '0'
011300040601     C*
011400040601     C* RICHIESTO ELENCO
011500040601     C                   SETOFF                                           28
011600040601     C     '?'           SCAN      VIWCOD                                 28
011700040601     C     *IN28         IFEQ      *ON
011800040601     C*
011900040601     C* RICHIESTO ELENCO DESTINATARI
012000040601     C                   MOVEL     KPJBU         SAVKPJBU
012100040601     C                   MOVEL(P)  'P01'         KPJBU
012200040601     C                   CALL      'TISIT1R'
012300040601     C                   PARM                    KPJBA
012400040601     C                   EVAL      VIWCOD = %subst(KPJBU:4:7)
012500040601     C                   MOVEL     SAVKPJBU      KPJBU
012600040601     C                   ENDIF
012700040601     C                   MOVE(P)   VIWCOD        CHKDS2            7 0
012800040601     C     CHKDS2        CHAIN     azdst04l
012900040601     C                   IF        not %found(azdst04l)
013000040601     C                   MOVEL     '1'           ERRORE
013100040601     C                   EVAL      VIWMSG = 'Destinatario indicato errato.'
013200040601     C                   ELSE
013300040601     C                   EVAL      VIWRAG = DSTRAG
013400040601     C                   EVAL      VIWLOC = DSTLOC
013500040601     C                   EVAL      VIWPRV = DSTPRV
013600040601     C                   EVAL      VIWKWI = DSTKWI
013700040601     C                   ENDIF
013800040601     C*
013900040601     C                   ENDSR
014000970526     C*------------------------------------------------------------------------*
014100970526     C* *INZSR - OPERAZIONI INIZIALI
014200970526     C*------------------------------------------------------------------------*
014300970526     C     *INZSR        BEGSR
014400970526     C*---
014500970526     C* RICEVIMENTO PARAMETRI
014600970526     C*---
014700970526     C     *ENTRY        PLIST
014800970612     C                   PARM                    KPJBA
014900040601     C                   PARM                    INCODDST          7 0
015000040601     C                   PARM                    ERRORE            1
015100040601     C*
015200040601     C* CALCOLA LA DATA CORRENTE
015300040601     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
015400040601     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
015500040601     C                   Z-ADD     WN8           G08DAT
015600040601     C                   Z-ADD     *ZEROS        G08INV
015700040601     C                   MOVEL     '0'           G08ERR
015800040601     C                   CALL      'XSRDA8'
015900040601     C                   PARM                    WLBDA8
016000040601     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
016100970526     C*---
016200970526     C* CHIAVI DI LETTURA
016300970526     C*---
016400040601     C* LETTURA azdss03l
016500020415     C     KEYDSS3       KLIST
016600040601     C                   KFLD                    DA_DSTNAZ
016700040601     C                   KFLD                    DA_DSTPRV
016800040601     C                   KFLD                    DA_DSTCAP
016900040601     C                   KFLD                    DA_DSTLOC
017000040601     C                   KFLD                    DA_DSTRAG
017100040601     C                   KFLD                    DA_DSTKWI
017200040601     C*
017300040601     C                   ENDSR
