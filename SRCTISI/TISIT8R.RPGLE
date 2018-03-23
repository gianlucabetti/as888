000100980904     H*------------------------------------------------------------------------*
000200980904     H DECEDIT('0,') DATEDIT(*DMY.)
000300980904     F*------------------------------------------------------------------------*
000400980904     F* DATA BASE
000500980904     F*------------------------------------------------------------------------*
000600060711     FTABEL00F  IF   E           K DISK
000700020416     FTITAS38C  IF   E           K DISK
000800040617     FFIAR531C  IF   E           K DISK
000900060725     fAZDST01L  if   e           k disk    prefix(A_)
001000060725     f                                     rename(AZDST000:AZDST001)
001100060718     fAZDST99J  if   e           k disk    prefix(D_)
001200060711     fAZDSTU1L  Uf A e           k disk    prefix(U_)
001300040618     FAZDSTP1L  UF A E           K DISK
001400050222     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001500040909     D*-------------------
001600040909     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
001700040909     D*-------------------
001800040909     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
001900040909     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
002000040909     D FRI             S              1    DIM(5)                               FLAG RICAVO
002100040909     D*-------------------
002200040909     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
002300040909     D*-------------------
002400040909     D TB              S              2    DIM(20)                              TIP0 BOLLA
002500040909     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
002600040909     D RI              S              1    DIM(20)                              FLAG RICAVO
002700040909     D*-------------------
002800040909     D* DS TIPO BOLLA
002900040909     D*-------------------
003000040909     D DSTB          E DS
003100040909     D*-------------------
003200040909     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
003300040909     D*-------------------
003400040909     D DS2W          E DS
003500020416     D*-------------------
003600020416     D* ARCHITETTURA
003700020416     D*-------------------
003800020416     D KPJBA         E DS
003900060725      *------------------
004000060725      * Struttura dati trk AZDST00F
004100060725     d AZDSTds       e ds                  inz
004200060725     d                                     extname(AZDST00F)
004300060725     d                                     prefix(W_)
004400040621     D*------------------
004500040621     D* DS "XSRDA8" - INVERSIONE DATA (8)
004600040621     D*------------------
004700040621     D WLBDA8          DS                  INZ
004800040621     D  G08DAT                 1      8  0
004900040621     D  G08INV                 9     16  0
005000040621     D  G08ERR                17     17
005100040621     D  G08TGI                18     22  0
005200020416     D*-------------------
005300020416     D* DS PER PASSAGGIO PARAMETRI A *PGM DI ELABORAZIONE
005400020416     D*-------------------
005500020416     D PARAM           DS
005600060711     d  PARtla                        1    inz
005700020416     D  PARDAI                        8S 0 inz(*zeros)
005800020416     D  PARDAF                        8S 0 inz(*zeros)
005900040609     D  PARDLF                        8S 0 inz(*zeros)
006000040625     D  PARPOU                        3S 0 inz(*zeros)
006100020416     D*-------------------
006200020416     D* DS PER RIDEFINIZIONE DATA BOLLE
006300020416     D*-------------------
006400020416     D                 DS
006500040617     D  TASAAS                 1      4  0
006600040617     D  TASMGS                 5      8  0
006700040617     D  TASDAT                 1      8  0
006800040617     D*-------------------
006900040617     D* DS PER RIDEFINIZIONE DATI SUSCETTIBILI DI "BONIFICA" CARATTERI NN VALIDI
007000040617     D*-------------------
007100040617     D                 DS
007200040617     D  TASNZD                 1      3
007300040617     D  TASPRD                 4      5
007400040617     D  TASCAD                 6     14
007500040617     D  TASLOD                15     49
007600040617     D  TASIND                50     84
007700040617     D  TASRSD                85    119
007800040617     D  TASCHKBON              1    119
007900020416     D*------------------
008000020416     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
008100020416     D*------------------
008200020416     D TISIT0DS      E DS
008300040531     D*------------------
008400040531     D* DS X TIPO RECORD "GEN" SU FIAR5
008500040531     D*------------------
008600040601     D DAR5GEN       E DS
008700050218     D*------------------
008800050218     D* VARIABILI D WRK
008900050218     D*------------------
009000050218     D DATCORISO       S               D
009100050218     D DATISO          S               D
009200980904     C*------------------------------------------------------------------------*
009300980904     C* MAIN LINE
009400980904     C*------------------------------------------------------------------------*
009500040909     C*
009600040909     C* CARICAMENTO TABELLE
009700040909     C                   EXSR      CARTAB
009800040609     C*
009900040609     C* OPERAZIONI PRELIMINARI
010000060712sel 1c                   select
010100060712      * Se PARtla=" ": cancellazione preliminare dei dati in AZDSTP1L
010200060712w   1c                   when      PARtla = *blanks
010300040609     C                   EXSR      PREOPE
010400060712      * Se PARtla="U": caricamento iniziale del file AZDSTU1L
010500060712     c                   when      PARtla = 'U'
010600060712     c                   exsr      Prep_DSTU
010700060712e   1c                   endsl
010800980904     C*
010900980904     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
011000980904     C                   EXSR      SETFIL
011100980904     C*
011200980904     C* CICLO FINO A FINE FILE
011300020416     C     $FINE         DOWEQ     'N'
011400980904     C*
011500020416     C* ELABORA LE PROPOSTE DI DESTINATARI DISAGIATI/SUPERMERCATI
011600020410     C                   EXSR      EXETAS
011700980904     C*
011800980904     C* LETTURA SUCCESSIVA BOLLE
011900980904     C                   EXSR      LETFIL
012000020416     C                   ENDDO                                                  *FINE FILE
012100040116     C*
012200040116     C* LANCIO IL DRIVER TISIT5R IN MODALITA' "SOLO CHIUSURA FILES"
012300040116     C*
012400040116     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
012500040116     C                   CLEAR                   TISIT0DS
012600040116     C                   MOVEL     'C'           IT0TLA
012700040116     C                   CALL      'TISIT5R'
012800040116     C                   PARM                    TISIT0DS
012900020410     C*
013000980904     C                   SETON                                        LR
013100980904     C*------------------------------------------------------------------------*
013200980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
013300980904     C*------------------------------------------------------------------------*
013400980904     C     SETFIL        BEGSR
013500980904     C*
013600020416     C                   MOVEL     PARDAI        KASAAS                         *ANNO INIZIALE
013700020416     C                   MOVE      PARDAI        KASMGS                         *MM/GG INIZIALE
013800990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
013900980904IF  1C     *IN99         IFEQ      *ON
014000980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
014100980904X   1C                   ELSE
014200980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
014300980904E   1C                   ENDIF
014400980904     C*
014500980904     C                   ENDSR
014600980904     C*------------------------------------------------------------------------*
014700980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
014800980904     C*------------------------------------------------------------------------*
014900980904     C     LETFIL        BEGSR
015000980904     C*
015100980904     C* LETTURA FINO A:
015200980904     C*  - FINE FILE
015300980904     C*  - TROVATO RECORD VALIDO
015400980904     C                   MOVEL     'N'           WRECOK
015500980904DO  1C     $FINE         DOWEQ     'N'
015600980904     C     WRECOK        ANDEQ     'N'
015700990802     C                   READ      TITAS38C                               99
015800980904IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
015900980904     C                   MOVEL     'S'           $FINE
016000980904X   2C                   ELSE
016100980904     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
016200980904E   2C                   ENDIF
016300980904E   1C                   ENDDO
016400980904     C*
016500980904     C                   ENDSR
016600980904     C*------------------------------------------------------------------------*
016700980904     C* CHKREC - CONTROLLA VALIDITA' RECORD                              -BOLLE-
016800980904     C*------------------------------------------------------------------------*
016900980904     C     CHKREC        BEGSR
017000020410     C*
017100020410     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
017200020403     C*
017300020403     C* PERIODO OLTRE IL RICHIESTO, ESCE
017400040604     C                   IF        WRECOK = 'S'
017500040617     C                   IF        TASDAT >= PARDAI AND
017600040617     C                             TASDAT <= PARDAF
017700020410     C                   ELSE
017800020403     C                   MOVEL     'S'           $FINE
017900020410     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
018000020403     C                   ENDIF
018100040604     C                   ENDIF
018200040909     C*
018300040909     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
018400040909     C     WRECOK        IFEQ      'S'
018500040909     C                   Z-ADD     1             J
018600040909     C                   SETOFF                                           28
018700040909     C     TASTBL        LOOKUP    TB(J)                                  28
018800040909     C     SP(J)         IFEQ      'N'
018900040909     C                   MOVEL     'N'           WRECOK
019000040909     C                   ENDIF
019100040909     C                   ENDIF
019200040604     C*
019300040604     C* CONTROLLO CHE LA BOLLA NN ABBIA CARATTERI "STRANI", SE SI => IGNORO LA BOLLA
019400040604     C                   IF        WRECOK = 'S'
019500040604     C                   EXSR      EXEBON
019600040604     C                   IF        EsitoBon = '1'
019700040604     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019800040604     C                   ENDIF
019900040604     C                   ENDIF
020000020416     C*
020100040531     C* SOLO BOLLE DICHIARATE "AI SUPERMERCATI" O "DISAGIATE" O "SU APPUNTAMENTO"
020200040604     C                   IF        WRECOK = 'S'
020300060726     C                   IF        PARTLA = *blanks
020400020416     C                   IF        TASFTC = 'S' OR
020500020422     C                             TASTC2 = 'S' OR
020600020422     C                             TASFTC = 'X' OR
020700040609     C                             TASTC2 = 'X'
020800020416     C                   ELSE
020900020416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021000060726     C                   ENDIF
021100020416     C                   ENDIF
021200060726     C                   IF        PARTLA = 'U'
021300060726     C                   IF        TASFTC = 'S' OR
021400060726     C                             TASTC2 = 'S' OR
021500060726     C                             TASFTC = 'X' OR
021600060726     C                             TASTC2 = 'X' OR
021700060726     C                             TASFTC = 'A' OR
021800060726     C                             TASTC2 = 'A'
021900060726     C                   ELSE
022000060726     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022100060726     C                   ENDIF
022200060726     C                   ENDIF
022300040604     C                   ENDIF
022400980904     C*
022500020416     C* CONTROLLO TRAMITE TISIT5R SE DESTINATATIO BOLLA GIA' PRESENTE COME DISAG/SUPERM
022600040604     C                   IF        WRECOK = 'S'
022700020416     C                   EXSR      CHKDST
022800040604     C                   ENDIF
022900020416     C*
023000980904     C                   ENDSR
023100020416     C*------------------------------------------------------------------------*
023200020416     C* CHKDST - VERIFICA X REPERIMENTO DESTINATARIO DISAG./SUPERMERC.
023300020416     C*------------------------------------------------------------------------*
023400020416     C     CHKDST        BEGSR
023500020416     C*
023600020416     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
023700020416     C                   EXSR      IMPCAM
023800020416     C*
023900020416     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
024000020416     C                   CALL      'TISIT5R'
024100020416     C                   PARM                    TISIT0DS
024200020416     C*
024300020416     C* SE GIA' PRESENTE NN LO SCRIVO NEL WRK-FILE
024400060711      * (ma ne aggiorno il "conteggio" nel file AZDSTU0F,
024500060711      * se?richiesto)
024600060712      * Anche se NON presente: NON lo scrivo nel WrkFile
024700060712      * se?richiesto il "conteggio" nel file AZDSTU0F
024800020603     C                   IF        OT0ERR <> *blanks
024900060712     c                   if        PARtla = 'U'
025000060712     c                   movel     'N'           wRecOK                         *RECORD NON VALIDO
025100060712     c                   endif
025200020422     C                   ELSE
025300020422     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025400060711     c                   if        PARtla = 'U'
025500060711     c                   exsr      ExeDST
025600060711     c                   endif
025700020416     C                   ENDIF
025800020416     C*
025900020416     C                   ENDSR
026000020416     C*------------------------------------------------------------------------*
026100020416     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
026200020416     C*------------------------------------------------------------------------*
026300020416     C     IMPCAM        BEGSR
026400020416     C*
026500040116     C                   MOVEL     'E'           IT0TLA
026600020416     C                   MOVEL     TASRSD        IT0RAG
026700020416     C                   MOVEL     TASNZD        IT0NAZ
026800020416     C                   MOVEL     TASPRD        IT0PRV
026900020416     C                   MOVEL     TASCAD        IT0CAP
027000020416     C                   MOVEL     TASLOD        IT0LOC
027100020416     C                   MOVEL     TASIND        IT0IND
027200020416     C*
027300020416     C                   ENDSR
027400980904     C*------------------------------------------------------------------------*
027500020416     C* EXETAS - SCRIVE UNA NUOVA PROPOSTA DI DESTINATARIO DISAGIATO/SUPERMERCATO
027600980904     C*------------------------------------------------------------------------*
027700020410     C     EXETAS        BEGSR
027800040531     C*
027900020416     C                   EVAL      KDSTIP = 'P'
028000020410     C*
028100040531     C* Verifico se la proposta già esiste nel file appunto "proposte"
028200040618     C     KEYDST1C      CHAIN     AZDSTP1L
028300040618     C                   IF        %found(AZDSTP1L)
028400040609     C* Se trovato già la proposta in archivio verifico se è stata annullata, se annullata
028500040609     C* gestisco in maniera particolare
028600040609     C                   IF        DSTATB = 'A'
028700040609     C                   EVAL      DSTATB = *blanks
028800040609     C                   Z-ADD     *zeros        DSTNUM
028900040609     C                   Z-ADD     PARDAI        DSTDAI
029000040609     C                   ENDIF
029100040609     C* ...quindi eseguo le operazioni valide in ogni caso
029200020416     C                   ADD       1             DSTNUM
029300020416     C                   Z-ADD     PARDAF        DSTDAF
029400040618     C                   UPDATE    AZDSTP00
029500020416     C                   ELSE
029600040618     C                   CLEAR                   AZDSTP00
029700020416     C                   EVAL      DSTTIP = KDSTIP
029800030910     C                   EVAL      DSTNAZ = IT0NAZ
029900030910     C                   EVAL      DSTPRV = IT0PRV
030000030910     C                   EVAL      DSTCAP = IT0CAP
030100030910     C                   EVAL      DSTLOC = IT0LOC
030200030910     C                   EVAL      DSTIND = IT0IND
030300030910     C                   EVAL      DSTRAG = IT0RAG
030400020422     C                   IF        TASFTC = 'S' OR
030500020422     C                             TASTC2 = 'S'
030600020416     C                   EVAL      DSTDOS = 'S'
030700040531     C                   ELSE
030800020422     C                   IF        TASFTC = 'X' OR
030900020422     C                             TASTC2 = 'X'
031000020422     C                   EVAL      DSTDOS = 'D'
031100040531     C                   ELSE
031200040609     C*                  IF        TASFTC = 'A' OR
031300040609     C*                            TASTC2 = 'A'
031400040609     C*                  EVAL      DSTDOS = 'A'
031500040609     C*                  ENDIF
031600040531     C                   ENDIF
031700040531     C                   ENDIF
031800020416     C                   Z-ADD     1             DSTNUM
031900020416     C                   Z-ADD     PARDAI        DSTDAI
032000020416     C                   Z-ADD     PARDAF        DSTDAF
032100040531     C* Reperisco il "referente" e/o il "telefono" della bolla dal FIAR5 tipo record 'GEN'
032200040531     C                   EVAL      AR5TRD = 'GEN'
032300040617     C     KEYAR530P     CHAIN     FIAR531C
032400040601     C                   CLEAR                   DAR5GEN
032500040617     C                   IF        %found(FIAR531C)
032600040531     C                   EVAL      DAR5GEN = AR5UNI
032700040531     C                   ENDIF
032800040625     C* Considero il TELEFONO e il REFERENTE solo se valorizzati almeno 3 caratteri
032900040625     C                   IF        %len(%trim(§AR5TELD)) >= 3
033000040601     C                   EVAL      DSTTEL = §AR5TELD
033100040625     C                   ENDIF
033200040625     C                   IF        %len(%trim(§AR5REF)) >= 3
033300040601     C                   EVAL      DSTREF = §AR5REF
033400040625     C                   ENDIF
033500040618     C*
033600040618     C                   EVAL      DSTUTE = KNMUS
033700040621     C                   EVAL      DSTPOU = PARPOU
033800040621     C                   EVAL      DSTDIV = DATCOR
033900040618     C                   EVAL      DSTLNA = TASLNA
034000040618     C*
034100040531     C* Quindi scarico il buffer sul file
034200040618     C                   WRITE     AZDSTP00
034300020416     C                   ENDIF
034400020416     C*
034500980904     C                   ENDSR
034600060711      *------------------------------------------------------------------------*
034700060711      * EXEDST - AGGIORNA DATI RELATIVI AL DESTINATARIO DISAGIATO/SUPERMERCATO
034800060711      *------------------------------------------------------------------------*
034900060711     c     ExeDST        BEGSR
035000060711      *
035100060725      * Aggiorno il conteggio delle destinazioni nel file AZDSTU1L
035200060725if  1c                   if        OT0liv =  '1'
035300060725     c                   clear                   OT0sin
035400060725e   1c                   endif
035500060718     c     K09DSTU1      chain     AZDSTU00
035600060711      *
035700060711if  1c                   if        %found(AZDSTU1L)
035800060711      *
035900060711      * (non può essere annullato)
036000060711      * ...quindi eseguo le operazioni valide in ogni caso
036100060711if  2c                   if        U_DSTnum = *zeros
036200060711     c                   eval      U_DSTliv = OT0liv
036300060711e   2c                   endif
036400060711     c                   add       1             U_DSTnum
036500060711     c                   UPDATE    AZDSTU00
036600060711      *                  ¯¯¯¯¯¯
036700060711x   1c                   else
036800060711      *
036900060711     c                   clear                   AZDSTU00
037000060711     c                   eval      U_DSTtip = PARtla
037100060718     c                   eval      U_DSTdos = OT0dos
037200060718     c                   eval      U_DSTliv = OT0liv
037300060711     c                   eval      U_DSTcod = OT0cod
037400060718     c                   eval      U_DSTnaz = OT0naz
037500060718     c                   eval      U_DSTprv = OT0prv
037600060718     c                   eval      U_DSTcap = OT0cap
037700060718     c                   eval      U_DSTloc = OT0loc
037800060718     c                   eval      U_DSTrag = OT0rag
037900060718     c                   eval      U_DSTkwi = OT0kwi
038000060718     c                   eval      U_DSSsin = OT0sin
038100060711     c                   eval      U_DSTnum = 1
038200060711     c                   eval      U_DSTdai = PARdai
038300060711     c                   eval      U_DSTdaf = PARdaf
038400060922     c*** NO flo:        eval      U_DSTflo = OT0flo
038500060927     c*** NO:            eval      U_DSTatb = '?'
038600060711     c                   WRITE     AZDSTU00
038700060711      *                  ¯¯¯¯¯
038800060711e   1c                   endif
038900060711      *
039000060711     c                   ENDSR
039100060711      *------------------------------------------------------------------------*
039200060711      * Prep_DSTU
039300060711      *------------------------------------------------------------------------*
039400060711     c     Prep_DSTU     BEGSR
039500060725      *
039600060725      * Ciclo di lettura 01L per caricare tutti i clienti
039700060725      *   (non solo i sinonimi)
039800060725     c                   eval      A_DSTtip = 'C'
039900060725     c     A_DSTtip      setll     AZDST001
040000060725     c     A_DSTtip      reade     AZDST001
040100060725      *
040200060725do  1c                   DOW       NOT %eof(AZDST01L)
040300060725      *
040400060725if  2c                   IF        A_DSTatb = *blanks
040500060725      *
040600060725     c                   clear                   AZDSTU00
040700060725     c                   eval      U_DSTtip = PARtla
040800060725     c                   eval      U_DSTdos = A_DSTdos
040900060725     c***                eval      U_DSTliv = ???
041000060725     c                   eval      U_DSTcod = A_DSTcod
041100060725     c                   eval      U_DSTnaz = A_DSTnaz
041200060725     c                   eval      U_DSTprv = A_DSTprv
041300060725     c                   eval      U_DSTcap = A_DSTcap
041400060725     c                   eval      U_DSTloc = A_DSTloc
041500060725     c                   eval      U_DSTrag = A_DSTrag
041600060725     c                   eval      U_DSTkwi = A_DSTkwi
041700060725     c***                eval      U_DSSsin = *blanks
041800060725     c***                eval      U_DSTnum = *zeros
041900060725     c                   eval      U_DSTdai = PARdai
042000060725     c                   eval      U_DSTdaf = PARdaf
042100060922     c*** NO flo:        eval      U_DSTflo = D_DSTflo
042200060725     c***                eval      U_DSTatb = D_DSTatb
042300060725     c                   WRITE     AZDSTU00
042400060725      *                  ¯¯¯¯¯
042500060725e   2c                   ENDIF
042600060725      *
042700060725     c     A_DSTtip      reade     AZDST001
042800060725e   1c                   ENDDO
042900060711      *
043000060725      * Ciclo di lettura 99J per caricare (solo) tutti i sinonimi
043100060711     c                   eval      D_DSTtip = 'C'
043200060711     c     D_DSTtip      setll     AZDST000
043300060711     c     D_DSTtip      reade     AZDST000
043400060711      *
043500060718do  1c                   DOW       NOT %eof(AZDST99J)
043600060711      *
043700060725if  2c                   IF             D_DSTatb =  *blanks
043800060725if  2c                             and  D_DSSsin <> *blanks
043900060711      *
044000060711     c                   clear                   AZDSTU00
044100060711     c                   eval      U_DSTtip = PARtla
044200060718     c                   eval      U_DSTdos = D_DSTdos
044300060718     c***                eval      U_DSTliv = ???
044400060711     c                   eval      U_DSTcod = D_DSTcod
044500060718     c                   eval      U_DSTnaz = D_DSTnaz
044600060718     c                   eval      U_DSTprv = D_DSTprv
044700060718     c                   eval      U_DSTcap = D_DSTcap
044800060718     c                   eval      U_DSTloc = D_DSTloc
044900060718     c                   eval      U_DSTrag = D_DSTrag
045000060718     c                   eval      U_DSTkwi = D_DSTkwi
045100060718     c                   eval      U_DSSsin = D_DSSsin
045200060717     c***                eval      U_DSTnum = *zeros
045300060711     c                   eval      U_DSTdai = PARdai
045400060711     c                   eval      U_DSTdaf = PARdaf
045500060922     c*** NO flo:        eval      U_DSTflo = D_DSTflo
045600060711     c***                eval      U_DSTatb = D_DSTatb
045700060711     c                   WRITE     AZDSTU00
045800060711      *                  ¯¯¯¯¯
045900060711e   2c                   ENDIF
046000060711      *
046100060711     c     D_DSTtip      reade     AZDST000
046200060711e   1c                   ENDDO
046300060711      *
046400060711     c                   ENDSR
046500040609     C*------------------------------------------------------------------------*
046600040618     C* PREOPE - OPERAZIONI PERLIMINARI SUL FILE AZDSTP0F
046700040609     C*------------------------------------------------------------------------*
046800040609     C     PREOPE        BEGSR
046900040609     C*
047000040609     C* SE NN RICHIESTA CLEAR ELIMINO I LANCI PRECEDENTI ALLA DATA FINALE MANTENIMENTO LANCI
047100040609     C* PRECEDENTI INDICATA NEL LANCIO CORRENTE
047200040618     C     *LOVAL        SETLL     AZDSTP1L
047300040618     C                   READ      AZDSTP1L
047400040618     C                   DOW       not %eof(AZDSTP1L)
047500040618     C                   IF        DSTDAF <  PARDLF AND
047600040618     C                             DSTDOS <> 'A'
047700040618     C                   DELETE    AZDSTP1L
047800040609     C                   ENDIF
047900040618     C                   READ      AZDSTP1L
048000040609     C                   ENDDO
048100040609     C*
048200040609     C                   ENDSR
048300040609     C*------------------------------------------------------------------------*
048400040609     C* EXEBON - OPERAZIONI DI "BONIFICA" CARTTERI NN VALIDI DALLE BOLLE
048500040604     C*------------------------------------------------------------------------*
048600040604     C     EXEBON        BEGSR
048700040604     C*
048800040604     C* Imposto i parametri x il *pgm d "bonifica"
048900040617     C                   EVAL      TxtInOut   = TASCHKBON
049000040604     C                   EVAL      ElencoChar = *blanks
049100040604     C                   EVAL      TipoElenco = *blanks
049200040604     C                   EVAL      CharSost   = *blanks
049300040604     C                   EVAL      UpperCase  = *blanks
049400040604     C                   EVAL      ChkNull    = '1'
049500040604     C                   EVAL      CharNull   = *blanks
049600040604     C                   EVAL      EsitoBon   = *blanks
049700040604     C*
049800040604     C* Effettuo la chiamata al *pgm d "bonifica"
049900040604     C                   CALL      'XCHKCHAR'
050000040604     C                   PARM                    TxtInOut       2048
050100040604     C                   PARM                    ElencoChar      256
050200040604     C                   PARM                    TipoElenco        1
050300040604     C                   PARM                    CharSost          1
050400040604     C                   PARM                    UpperCase         1
050500040604     C                   PARM                    ChkNull           1
050600040604     C                   PARM                    CharNull          1
050700040604     C                   PARM                    EsitoBon          1
050800040604     C*
050900040604     C                   ENDSR
051000040909     C*------------------------------------------------------------------------*
051100040909     C* CARTAB - CARICAMENTO DATI TABELLATI
051200040909     C*------------------------------------------------------------------------*
051300040909     C     CARTAB        BEGSR
051400040909     C*--------------------
051500040909     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
051600040909     C*--------------------
051700040909     C                   Z-ADD     0             I                 3 0
051800040909     C                   Z-ADD     1             TBLKUT
051900040909     C                   MOVEL     '2W'          TBLCOD
052000040909     C                   MOVEL     *BLANKS       TBLKEY
052100040909     C     KEYTAB        SETLL     TABEL00F
052200040909     C                   READ      TABEL00F                               99
052300040909     C     *IN99         DOWEQ     *OFF
052400040909     C     TBLCOD        ANDEQ     '2W'
052500040909     C     TBLFLG        IFEQ      *BLANKS
052600040909     C                   ADD       1             I
052700040909     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
052800040909     C                   MOVEL     TBLUNI        DS2W
052900040909     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
053000040909     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
053100040909     C                   ENDIF
053200040909     C                   READ      TABEL00F                               99
053300040909     C                   ENDDO
053400040909     C*--------------------
053500040909     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
053600040909     C*--------------------
053700040909     C                   Z-ADD     0             I
053800040909     C                   Z-ADD     1             TBLKUT
053900040909     C                   MOVEL     'TB'          TBLCOD
054000040909     C                   MOVEL     *BLANKS       TBLKEY
054100040909     C     KEYTAB        SETLL     TABEL00F
054200040909     C                   READ      TABEL00F                               99
054300040909     C     *IN99         DOWEQ     *OFF
054400040909     C     TBLCOD        ANDEQ     'TB'
054500040909     C     TBLFLG        IFEQ      *BLANKS
054600040909     C                   ADD       1             I
054700040909     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
054800040909     C                   MOVEL     TBLUNI        DSTB
054900040909     C                   Z-ADD     1             J                 4 0
055000040909     C                   SETOFF                                       28
055100040909     C     §TBRBL        LOOKUP    RBL(J)                                 28
055200040909     C     *IN28         IFEQ      *ON
055300040909     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
055400040909     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
055500040909     C                   ELSE
055600040909     C                   MOVEL     'N'           SP(I)
055700040909     C                   MOVEL     'N'           RI(I)
055800040909     C                   ENDIF
055900040909     C                   ENDIF
056000040909     C                   READ      TABEL00F                               99
056100040909     C                   ENDDO
056200040909     C*
056300040909     C                   ENDSR
056400980904     C*------------------------------------------------------------------------*
056500980904     C* *INZSR - OPERAZIONI INIZIALI
056600980904     C*------------------------------------------------------------------------*
056700980904     C     *INZSR        BEGSR
056800020416     C*
056900020416     C* RICEVIMENTO PARAMETRI
057000020416     C     *ENTRY        PLIST
057100020416     C                   PARM                    KPJBA
057200020416     C                   MOVEL     KPJBU         PARAM
057300040621     C*
057400040621     C* CALCOLA LA DATA CORRENTE
057500040621     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
057600040621     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
057700040621     C                   Z-ADD     WN8           G08DAT
057800040621     C                   Z-ADD     *ZEROS        G08INV
057900040621     C                   MOVEL     '0'           G08ERR
058000040621     C                   CALL      'XSRDA8'
058100040621     C                   PARM                    WLBDA8
058200040621     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
058300050218     C*
058400050218     C* SE KPJBU E' VUOTA => ASSUMO VALORI IN AUTOMATICO:
058500060713      * - Tipo lancio ............ = *blanks
058600050218     C* - Data iniziale bolle      = data odierna - 14 gg lavorativi
058700050218     C* - Data finale   bolle      = data odierna -  7 gg lavorativi
058800060403     C* - Data finale mantenimento = data odierna - 35 gg lavorativi
058900050218     C                   IF        KPJBU = *BLANKS
059000060713     c                   clear                   PARtla
059100060713      *
059200050218     C                   MOVE      *date         DATCORISO
059300050218     C     DATCORISO     SUBDUR    14:*DAYS      DATISO
059400050218     C                   MOVE(P)   DATISO        PARDAI
059500050218     C*
059600050218     C                   MOVE      *date         DATCORISO
059700050218     C     DATCORISO     SUBDUR    7:*DAYS       DATISO
059800050218     C                   MOVE(P)   DATISO        PARDAF
059900050218     C*
060000050218     C                   MOVE      *date         DATCORISO
060100060403     C***  DATCORISO     SUBDUR    49:*DAYS      DATISO
060200060403     C     DATCORISO     SUBDUR    35:*DAYS      DATISO
060300050218     C                   MOVE(P)   DATISO        PARDLF
060400050218     C*
060500050218     C                   Z-ADD     046           PARPOU
060600050218     C                   ENDIF
060700050222     C*
060800050222     C* STAMPO I PARAMETRI RICEVUTI IN INPUT O QUELLI CALCOLATI A RUN-TIME
060900050222     C                   EXCEPT    StampaPar
061000980904     C*
061100980904     C* VARIABILI RIFERITE AL DATABASE
061200990802     C     *LIKE         DEFINE    TASAAS        KASAAS                         *TITAS00F
061300981215     C     *LIKE         DEFINE    TASMGS        KASMGS
061400040618     C     *LIKE         DEFINE    DSTTIP        KDSTIP                         *AZDSTP0F
061500020410     C*
061600020410     C* VARIABILI DI WRK
061700020410     C                   MOVEL     'N'           $FINE             1            *FINE PGM
061800020410     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
061900980904     C*
062000020416     C* CHAIVI DI LETTURA - PARZIALE
062100020416     C     KEYTAS        KLIST
062200020416     C                   KFLD                    KASAAS
062300020416     C                   KFLD                    KASMGS
062400040531     C*
062500040531     C* CHAIVI DI LETTURA - PARZIALE
062600040531     C     KEYAR530P     KLIST
062700040531     C                   KFLD                    TASAAS
062800040531     C                   KFLD                    TASLNP
062900040531     C                   KFLD                    TASNRS
063000040531     C                   KFLD                    TASNSP
063100040531     C                   KFLD                    AR5TRD
063200020416     C*
063300020416     C* CHAIVI DI LETTURA - COMPLETA
063400020416     C     KEYDST1C      KLIST
063500020416     C                   KFLD                    KDSTIP
063600030910     C                   KFLD                    IT0NAZ
063700030910     C                   KFLD                    IT0PRV
063800030910     C                   KFLD                    IT0LOC
063900030910     C                   KFLD                    IT0CAP
064000030910     C                   KFLD                    IT0RAG
064100030910     C                   KFLD                    IT0IND
064200040618     C                   KFLD                    TASLNA
064300060711      *
064400060711      * chiave completa per file AZDSTU1L
064500060718     c     K09DSTU1      klist
064600060711     c                   kfld                    PARtla
064700060718     c                   kfld                    OT0cod
064800060718     c                   kfld                    OT0naz
064900060718     c                   kfld                    OT0prv
065000060718     c                   kfld                    OT0cap
065100060718     c                   kfld                    OT0loc
065200060718     c                   kfld                    OT0rag
065300060718     c                   kfld                    OT0kwi
065400060718     c                   kfld                    OT0sin
065500040909     C*
065600040909     C* CHIAVE LETTURA TABEL00F - COMPLETA
065700040909     C     KEYTAB        KLIST
065800040909     C                   KFLD                    TBLKUT
065900040909     C                   KFLD                    TBLCOD
066000040909     C                   KFLD                    TBLKEY
066100020416     C*
066200980904     C                   ENDSR
066300050222     O*--------------------------------------------------------------------------------------------*
066400050222     OPRTF198   E            StampaPar   2
066500060713     o                                              'Tipo lancio:'
066600060713     o                       PARtla           +   1
066700060713     o                                        +   8 ' '
066800060713     O                                        +   1 'Data iniziale:'
066900050222     O                       PARDAI           +   1
067000060713     O                                        +  10 'Data finale:'
067100050222     O                       PARDAF           +   1
067200060713     O                                        +  10 'Data mantenim. proposte:'
067300050222     O                       PARDLF           +   1
067400060713     O                                        +  10 'Utente generazione:'
067500050222     O                       PARPOU           +   1
