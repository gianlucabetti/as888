000100980904     H*------------------------------------------------------------------------*
000200980904     H DECEDIT('0,') DATEDIT(*DMY.)
000300980904     F*------------------------------------------------------------------------*
000400980904     F* DATA BASE
000500980904     F*------------------------------------------------------------------------*
000600040621     FAZDSTP0F  UF   E             DISK
000700020416     D*-------------------
000800020416     D* ARCHITETTURA
000900020416     D*-------------------
001000020416     D KPJBA         E DS
001100020416     D*------------------
001200020416     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
001300020416     D*------------------
001400020416     D TISIT0DS      E DS
001500980904     C*------------------------------------------------------------------------*
001600980904     C* MAIN LINE
001700980904     C*------------------------------------------------------------------------*
001800980904     C*
001900980904     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
002000980904     C                   EXSR      SETFIL
002100980904     C*
002200980904     C* CICLO FINO A FINE FILE
002300020416     C     $FINE         DOWEQ     'N'
002400980904     C*
002500040319     C* ELIMINA LA PROPOSTA SE TROVATA CORRISPONDENDE IN ANAGRAFICA DESTINATARI
002600040319     C                   EXSR      DLTPROP
002700980904     C*
002800980904     C* LETTURA SUCCESSIVA BOLLE
002900980904     C                   EXSR      LETFIL
003000020416     C                   ENDDO                                                  *FINE FILE
003100040116     C*
003200040116     C* LANCIO IL DRIVER TISIT5R IN MODALITA' "SOLO CHIUSURA FILES"
003300040116     C*
003400040116     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
003500040116     C                   CLEAR                   TISIT0DS
003600040116     C                   MOVEL     'C'           IT0TLA
003700040116     C                   CALL      'TISIT5R'
003800040116     C                   PARM                    TISIT0DS
003900020410     C*
004000980904     C                   SETON                                        LR
004100980904     C*------------------------------------------------------------------------*
004200980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
004300980904     C*------------------------------------------------------------------------*
004400980904     C     SETFIL        BEGSR
004500980904     C*
004600040319     C                   SETOFF                                       99
004700040621     C     *START        SETLL     AZDSTP0F                                     *FINE ARCHIVIO
004800040319     C     *IN99         IFEQ      *ON
004900980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
005000040319     C                   ELSE
005100980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
005200040319     C                   ENDIF
005300980904     C*
005400980904     C                   ENDSR
005500980904     C*------------------------------------------------------------------------*
005600980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
005700980904     C*------------------------------------------------------------------------*
005800980904     C     LETFIL        BEGSR
005900980904     C*
006000980904     C* LETTURA FINO A:
006100980904     C*  - FINE FILE
006200980904     C*  - TROVATO RECORD VALIDO
006300980904     C                   MOVEL     'N'           WRECOK
006400040319     C     $FINE         DOWEQ     'N'
006500980904     C     WRECOK        ANDEQ     'N'
006600040621     C                   READ      AZDSTP0F                               99
006700040319     C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
006800980904     C                   MOVEL     'S'           $FINE
006900040319     C                   ELSE
007000980904     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
007100040319     C                   ENDIF
007200040319     C                   ENDDO
007300980904     C*
007400980904     C                   ENDSR
007500980904     C*------------------------------------------------------------------------*
007600040319     C* CHKREC - CONTROLLA VALIDITA' RECORD
007700980904     C*------------------------------------------------------------------------*
007800980904     C     CHKREC        BEGSR
007900020410     C*
008000020410     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
008100040621     C*
008200040621     C* SE PROPOSTA E' D TIPO 'A' => NN CANCELLO MAI
008300040621     C                   IF        WRECOK = 'S'
008400040621     C                   IF        DSTDOS = 'A'
008500040621     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
008600040621     C                   ENDIF
008700040621     C                   ENDIF
008800040319     C*
008900040319     C* SE IL RECORD NN E' GIA' ANNULLATO VERIFICO CORRISPONDENZA OK IN ANAGRAFICA DESTINATARI
009000040621     C                   IF        WRECOK = 'S'
009100040319     C                   IF        DSTATB = *blanks
009200980904     C*
009300040319     C* CONTROLLO TRAMITE TISIT5R SE PROPOSTA GIA' PRESENTE COME DISAG/SUPERM
009400020416     C                   EXSR      CHKDST
009500040319     C                   ENDIF
009600040621     C                   ENDIF
009700020416     C*
009800980904     C                   ENDSR
009900020416     C*------------------------------------------------------------------------*
010000020416     C* CHKDST - VERIFICA X REPERIMENTO DESTINATARIO DISAG./SUPERMERC.
010100020416     C*------------------------------------------------------------------------*
010200040621     C     CHKDST        BEGSR
010300020416     C*
010400020416     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
010500020416     C                   EXSR      IMPCAM
010600020416     C*
010700020416     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
010800020416     C                   CALL      'TISIT5R'
010900020416     C                   PARM                    TISIT0DS
011000020416     C*
011100040319     C* SE GIA' PRESENTE NELL'ANAGRAFICA DESTINATARI LO ELIMINO DAL FILE PROPOSTE
011200040319     C                   IF        OT0ERR = *blanks
011300020422     C                   ELSE
011400020422     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
011500020416     C                   ENDIF
011600020416     C*
011700020416     C                   ENDSR
011800020416     C*------------------------------------------------------------------------*
011900040319     C* IMPCAM - IMPOSTA I CAMPI DELLA DS DAL FILE
012000020416     C*------------------------------------------------------------------------*
012100020416     C     IMPCAM        BEGSR
012200020416     C*
012300040116     C                   MOVEL     'E'           IT0TLA
012400040319     C                   MOVEL     DSTRAG        IT0RAG
012500040319     C                   MOVEL     DSTNAZ        IT0NAZ
012600040319     C                   MOVEL     DSTPRV        IT0PRV
012700040319     C                   MOVEL     DSTCAP        IT0CAP
012800040319     C                   MOVEL     DSTLOC        IT0LOC
012900040319     C                   MOVEL     DSTIND        IT0IND
013000020416     C*
013100020416     C                   ENDSR
013200980904     C*------------------------------------------------------------------------*
013300040319     C* DLTPROP - ELIMINA LA PROPOSTA CORRENTE SE TROVATA CORRISPONDENZA IN ANAGRAFICA
013400980904     C*------------------------------------------------------------------------*
013500040319     C     DLTPROP       BEGSR
013600980904     C*
013700040621     C                   DELETE    AZDSTP00
013800020416     C*
013900980904     C                   ENDSR
014000980904     C*------------------------------------------------------------------------*
014100980904     C* *INZSR - OPERAZIONI INIZIALI
014200980904     C*------------------------------------------------------------------------*
014300980904     C     *INZSR        BEGSR
014400020416     C*
014500020416     C* RICEVIMENTO PARAMETRI
014600020416     C     *ENTRY        PLIST
014700020416     C                   PARM                    KPJBA
014800020410     C*
014900020410     C* VARIABILI DI WRK
015000020410     C                   MOVEL     'N'           $FINE             1            *FINE PGM
015100020410     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
015200020416     C*
015300980904     C                   ENDSR
