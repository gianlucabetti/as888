000100000306      ***********************************************************************
000200960605      *   RICERCA TABELLE PER  E.D.I. ESTERO                                *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500000306      *---------------------------------------------------------------------*
000600000306      *  DATA BASE                                                          *
000700000306      *---------------------------------------------------------------------*
000800040216     FCNACO00F  IF   E           K DISK
000900960605     FEDTAB01L  IF   E           K DISK
001000000306     FTRTC91D   CF   E             WORKSTN SFILE(TC91S02:NRR2)
001100000306      *---------------------------------------------------------------*
001200100618$003 D WSfl            S                   Like(sfnr2)
001300960605     D KPJBA         E DS
001400000306     D TRTC91DS      E DS
001500040216     D DSPT          E DS                  EXTNAME(EDIDSPT)
001600110111     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
001700040216     D DSCE          E DS                  EXTNAME(EDIDSCE)
001800040216     D DSEA          E DS                  EXTNAME(EDIDSEA)
001900040216     D DSEM          E DS                  EXTNAME(EDIDSEM)
002000050223     D DSCL          E DS                  EXTNAME(EDIDSCL)
002100040216     D CNCR80        E DS
002200000306     D UT§DSE0F      E DS
002300040216     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
002400040216     D  KCU                  698    847P 0 DIM(50)   PACKEVEN                   PARAM CAPI CONTO
002500040216      *---------------------------------------------------------------*
002600040216     D TCUDS           DS
002700040216     D  F34                    3      4
002800040216     D  F56                    5      6
002900960605     D                SDS
003000960605     D  NOMPGM                 1     10
003100100618      **********************************************************************
003200100618      * Constants
003300100618      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
003400100618      **********************************************************************
003500100618$xxx D Ord_Codice...
003600100618$xxx D                 C                   1
003700100618$xxx D Ord_Descrizione...
003800100618$xxx D                 C                   2
003900100618$xxx D Ord_CodiceDisc...
004000100618$xxx D                 C                   3
004100100618$xxx D Ord_DescrizioneDisc...
004200100618$xxx D                 C                   4
004300100930$xxx D Ord_Cliente...
004400100930$xxx D                 C                   5
004500100618      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
004600100618     D MaxKey          C                   9
004700100618     D Ascendente      C                   1
004800100618     D Discendente     C                   2
004900100618     D Carattere       C                   6
005000100618     D Put             C                   1
005100100618     D EndPut          C                   2
005200100618     D Get             C                   3
005300100618     D Numerico        C                   8
005400100618      **********************************************************************
005500100618      * Campi utili:
005600100618      *     SizeList   - Dimensione della lista
005700100618      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
005800100618      **********************************************************************
005900100618     D NotUsed         S             16A
006000100618     D ReturnSize      S              9B 0
006100100618     D SizeList        S              9B 0
006200100618     D RrnLast         S              5I 0
006300100618     D WrkSort         S              1  0 inz(0)
006400100618     D WindSort        S              3I 0 inz(0)
006500100618      **********************************************************************
006600100618      * Data Structures
006700100618      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
006800100618      *     QLGSCB     - The sort request block for the QLGSORT API
006900100618      *     QLGSCB00   - The sort request block for the QLGSRTIO API
007000100618      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
007100100618      *     QUSEC      - Error structure for the QLGSORT API
007200100618      **********************************************************************
007300100618     D SflRcd          DS
007400100618     D  V2CKEY
007500100618     D  V2DDES
007600100618     D  V2CANN
007700100618     D  V2CSCE
007800100618     D  V2DKEY
007900100618     D  Selected                      1A
008000100618
008100100618      /COPY QSYSINC/QRPGLESRC,QLGSORT
008200100618     D QLGKL                         16    DIM(MaxKey)
008300100618     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
008400100618     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
008500100618     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
008600100618     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008700100618
008800100618      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008900100618      /COPY QSYSINC/QRPGLESRC,QUSEC
009000100618      *---------------------------------------------------------------*
009100000306      *  CICLO PRINCIPALE
009200000306      *---------------------------------------------------------------*
009300960605      * Inizializzo videate
009400960605     C                   EXSR      CARSF2                                       le key del cod.
009500100618      *
009600960605      * Loop gestione videate
009700960605     C     WFINE         DOWEQ     'N'
009800960605     C     WTPVID        CASEQ     '2S'          GESS02                         Sbf.scelta key.
009900960605     C     WTPVID        CASEQ     '2D'          GESD02                         no key tabella
010000960605     C                   END
010100960605     C                   END
010200100618      *
010300960605      * Fine Lavoro
010400960605     C     FINE          TAG
010500960605     C                   SETON                                        LR
010600000306      *---------------------------------------------------------------*
010700000306      *  CARSF2: Caricamento subfile delle chiavi
010800000306      *---------------------------------------------------------------*
010900960605     C     CARSF2        BEGSR
011000960605      *
011100960605      *  Pulisco dati SFL
011200960605     C                   SETOFF                                       2021
011300960605     C                   WRITE     TC91C02
011400960605     C                   SETON                                        2021
011500100930     c                   if        D91cod = 'PT' or D91cod = 'CL'
011600100930     C                   SETON                                        07
011700100930     c                   end
011800100618      *
011900960606     C                   MOVEL     '2S'          WTPVID            2
012000960605      *  Eseguo caricamento prima pagina
012100960605     C                   MOVEL     D91COD        KCOD
012200960605     C                   MOVEL     *LOVAL        WINKEY
012300960612     C                   Z-ADD     1             SFNR2
012400100618      *
012500100618     c     avanti        tag
012600960605     C                   EXSR      ROLL02
012700100618     c  n23              goto      avanti
012800100618      *
012900960605      *  Se non ho dati da caricare emetto videata vuota
013000960605     C                   EXSR      INZ02
013100960605      *  SFL vuoto
013200000306     C     NRR2          IFEQ      0
013300960605     C                   MOVEL     '2D'          WTPVID
013400960605     C                   END
013500960605     C*
013600100618     C*  accendo l'indicatore di ordinamento x descrizione
013700100618     C                   seton                                        0809
013800100618     C*
013900960605     C                   ENDSR
014000960605     C*---------------------------------------------------------------*
014100960605     C*  GESS02: Gestione singola tabella
014200960605     C*---------------------------------------------------------------*
014300960605     C     GESS02        BEGSR
014400960605     C*
014500960605     C                   WRITE     TC91T02
014600960605     C                   EXFMT     TC91C02
014700960612     C                   Z-ADD     1             SFNR2
014800100618      *  ORDINAMENTO ascendente
014900100618     c   kH              if        *in08
015000100618     c                   setoff                                       0809
015100100618     C                   eval      WrkSort = Ord_CodiceDisc
015200100618     c                   exsr      Ordina_SFL2
015300100618     c                   else
015400100618     c                   seton                                        0809
015500100618     C                   eval      WrkSort = Ord_Codice
015600100618     c                   exsr      Ordina_SFL2
015700100618     c                   end
015800100930      *  ORDINAMENTO x c.cliente
015900100930     c   KG              if        D91cod = 'PT' or D91cod = 'CL'
016000100930     C                   eval      WrkSort = Ord_Cliente
016100100930     c                   exsr      Ordina_SFL2
016200100930     c                   end
016300100618      *  ORDINAMENTO Discendente
016400100618     c   ki              if        *in09
016500100618     c                   setoff                                       0908
016600100618     C                   eval      WrkSort = Ord_DescrizioneDisc
016700100618     c                   exsr      Ordina_SFL2
016800100618     c                   else
016900100618     c                   seton                                        0908
017000100618     C                   eval      WrkSort = Ord_Descrizione
017100100618     c                   exsr      Ordina_SFL2
017200100618     c                   end
017300960605     C* Ritorno
017400110321     c                   if        *inKL
017500110321     c                               or
017600110321     c                             *inKC and D91cod = 'PT'
017700110321     c                               or
017800110321     c                             *inKC and D91cod = 'CL'
017900960605     C                   MOVEL     '1'           WFINE
018000960606     C                   MOVEL     '12'          D91CMD
018100000306     C                   MOVEL     TRTC91DS      KPJBU
018200960605     C                   GOTO      FINSF2
018300960605     C                   END
018400960605     C* ROLLUP
018500960605     C     *IN25         IFEQ      '1'
018600960612     C                   EXSR      ROLL02
018700960612     C                   Z-ADD     NRR2          SFNR2
018800960605     C                   GOTO      FINSF2
018900960605     C                   END
019000960605     C* Seleziono codice
019100960605     C                   READC     TC91S02                                30
019200960606     C     *IN30         IFEQ      '0'
019300960606     C     V2CSCE        ANDEQ     '1'
019400960605     C                   MOVEL     V2CKEY        D91KEY
019500960605     C                   MOVEL     V2DKEY        D91UNI
019600000306     C                   MOVEL     TRTC91DS      KPJBU
019700960605     C                   MOVEL     'S'           WFINE
019800960605     C                   END
019900960605     C*
020000960605     C     FINSF2        ENDSR
020100960605     C*---------------------------------------------------------------*
020200960605     C*  GESD02: Gestione tabelle inesistente
020300960605     C*---------------------------------------------------------------*
020400960605     C     GESD02        BEGSR
020500960605     C*
020600960605     C                   EXFMT     TC91D02
020700960605     C* Ritorno
020800960605     C     *INKL         IFEQ      '1'
020900960605     C                   MOVEL     '1'           WFINE
021000960606     C                   MOVEL     '12'          D91CMD
021100000306     C                   MOVEL     TRTC91DS      KPJBU
021200960605     C                   END
021300960605     C*
021400960605     C                   ENDSR
021500960605     C*---------------------------------------------------------------*
021600960605     C*  INZ02: Inizializzazione dati ricerca singola tabella
021700960605     C*---------------------------------------------------------------*
021800960605     C     INZ02         BEGSR
021900960605     C*
022000960605     C                   MOVEL     '99'          KCOD
022100960605     C                   MOVEL     *BLANKS       KKEY
022200960606     C                   MOVEL     D91COD        KKEY
022300960605     C     KTAB          CHAIN     EDTAB01L                           30
022400960605     C     *IN30         IFEQ      '0'
022500960606     C                   MOVEL     D91COD        V2CCOD
022600960605     C                   MOVEL     TABUNI        V2DCOD
022700960605     C                   END
022800100930     C*
022900100930     c                   if        D91cod = 'PT'
023000100930     C                   EVAL      V2TANN ='C.CLI.'
023100110214     C                   EVAL      V2TDES ='  DATI           *=NO trad Std'
023200100930     C                   END
023300100930     C*
023400100930     c                   if        D91cod = 'CL'
023500100930     C                   EVAL      V2TANN ='C.CLI.'
023600100930     C                   EVAL      V2TDES ='  DATI'
023700100930     C                   END
023800960605     C*
023900960605     C                   ENDSR
024000960605     C*---------------------------------------------------------------*
024100960605     C*  ROLL02: Caricamento una pagina sufile chiavi singola tabella
024200960605     C*---------------------------------------------------------------*
024300960605     C     ROLL02        BEGSR
024400960605     C*
024500960606     C                   Z-ADD     0             WNRIG2            2 0
024600960605     C                   MOVEL     WINKEY        KKEY
024700960612     C                   MOVEL     D91COD        KCOD
024800000306      *
024900960605     C     KTAB          SETGT     EDTAB01L                           30
025000960605     C     KCOD          READE     EDTAB01L                               30
025100000306      *
025200000306     C                   DOW        *IN30 = *off  and  Wnrig2 < 16
025300960606     C                   ADD       1             NRR2              4 0
025400960606     C                   ADD       1             WNRIG2
025500960605     C                   MOVEL     TABKEY        V2CKEY
025600960605     C                   MOVEL     TABUNI        V2DKEY
025700000306     C                   If         TABFLG = *blanks
025800000306     C                   clear                   V2CANN
025900100929      * viene visualizzato il codice del cliente/Partner (PT)
026000100929     c                   if        D91cod = 'PT'
026100100929     C                   eval      V2CANN = %subst(TABuni:1:7)
026200100929     C                   Endif
026300100930     c                   if        D91cod = 'CL'
026400110606     C*******            eval      V2CANN = %subst(TABKEY:1:7)
026500110606     C                   eval      V2CANN = %subst(TABUNI:1:7)
026600100930     C                   Endif
026700000306     C                   Else
026800000306     c                   eval      V2Cann = 'ANNULLATO'
026900000306     C                   Endif
027000000306      *
027100040216     c                   clear                   v2ddes
027200040216      * solo x la PT decodifica il Partner/Cliente
027300040216     c                   if        D91cod = 'PT'
027400040216     C                   MOVEL     TABUNI        dspt
027500040216     C                   Z-ADD     1             KKUT
027600040216     C                   Z-ADD     KCI           KKCC
027700040216     C                   Z-ADD     §PTKSC        KKSC
027800040216     C     KACO          CHAIN     CNACO00F                           30
027900040216     C  N30              MOVEL     ACORAG        V2DDES
028000110111      *
028100110111     c                   move      tabcod        savcod
028200110111     c                   move      tabkey        savkey
028300110111      *
028400110111      *
028500110111     c                   move      'PZ'          NewCOD
028600110111     C     NTAB          CHAIN     EDTAB01L
028700110111     c                   if        %Found(EDTAB01L)
028800110111     c                   eval      dspz = tabUNI
028900110111     c                   if        §PZPGMRCZB = 'INTESA'  or
029000110111     c                             §PZPGMRCZB = *blank
029100110214     c                   move      '|*'          v2dDES
029200110214     c                   else
029300110214     c                   move      '| '          v2dDES
029400110214     c                   end
029500110111     c                   end
029600110111     C     STAB          SETGT     EDTAB01L                           30
029700040216     c                   end
029800040216      *
029900040216      * solo x la CE decodifica
030000040216     c                   if        D91cod = 'CE'
030100040216     C                   MOVEL     TABUNI        dsce
030200040216     C                   MOVEL     §CEdec        V2DDES
030300040216     c                   end
030400040216      *
030500040216      * solo x la EA decodifica
030600040216     c                   if        D91cod = 'EA'
030700040216     C                   MOVEL     TABUNI        dsea
030800040216     C                   MOVEL     §EAdes        V2DDES
030900040216     c                   end
031000040216      *
031100040216      * solo x la EM decodifica
031200040216     c                   if        D91cod = 'EM'
031300040216     C                   MOVEL     TABUNI        dsem
031400040216     C                   MOVEL     §EMdes        V2DDES
031500040216     c                   end
031600050223      *
031700050223      * solo x la CL decodifica
031800050223     c                   if        D91cod = 'CL'
031900050223     C                   MOVEL     TABUNI        dsCL
032000050223     C                   MOVEL     §CLdes        V2DDES
032100050223     c                   end
032200040216      *
032300960606     C                   WRITE     TC91S02
032400000306      *
032500000306     C                   If        Wnrig2 < 16
032600960606     C     KCOD          READE     EDTAB01L                               30
032700000306     c                   Endif
032800000306      *
032900000306     C                   ENDDO
033000000306      *
033100960606     C                   MOVEL     TABKEY        WINKEY
033200100618      *
033300100618     C*  Se sono a fine file accendo 23
033400960606     C     *IN30         COMP      '1'                                    23
033500960605     C*
033600100618     C                   EVAL      RrnLast = nrr2
033700100618     C*
033800960605     C                   ENDSR
033900960605     C*---------------------------------------------------------------*
034000960605     C*  *INZSR: Operazioni iniziali                                  *
034100960605     C*---------------------------------------------------------------*
034200960605     C     *INZSR        BEGSR
034300960605     C*
034400960605     C     *ENTRY        PLIST
034500960605     C                   PARM                    KPJBA
034600000306     C                   MOVEL     KPJBU         TRTC91DS
034700960605      *  Call a XPARUT
034800960605     C                   Z-ADD     1             CODUT
034900960605     C                   CALL      'X§PARUT'
035000000306     C                   PARM                    UT§DSE0F
035100960605     C                   MOVEL     REC80         CNCR80
035200960605     C                   MOVEL     RAGUT         RSUT
035300040216     C*--- RICERCA CAPOCONTI------------------------------------------*
035400040216     C                   DO        50            X                 3 0
035500040216     C                   MOVE      TCU(X)        TCUDS
035600040216     C     F56           IFEQ      'CG'
035700040216     C     F34           ANDEQ     '01'
035800040216     C                   Z-ADD     KCU(X)        KCI               4 0
035900040216     C                   END
036000040216     C                   END
036100960605      * Definizione chiavi
036200960605     C     KTAB          KLIST
036300960605     C                   KFLD                    KCOD
036400960605     C                   KFLD                    KKEY
036500110111     C     NTAB          KLIST
036600110111     C                   KFLD                    NewCOD
036700110111     C                   KFLD                    savkey
036800110111     C     STAB          KLIST
036900110111     C                   KFLD                    SavCOD
037000110111     C                   KFLD                    SavKEY
037100040216     C     KACO          KLIST
037200040216     C                   KFLD                    KKUT
037300040216     C                   KFLD                    KKCC
037400040216     C                   KFLD                    KKSC
037500960605      * Definizione varibili
037600040216     C     *LIKE         DEFINE    ACOKUT        KKUT
037700040216     C     *LIKE         DEFINE    ACOKCC        KKCC
037800040216     C     *LIKE         DEFINE    ACOKSC        KKSC
037900960605     C     *LIKE         DEFINE    TABCOD        KCOD
038000960605     C     *LIKE         DEFINE    TABKEY        KKEY
038100960606     C     *LIKE         DEFINE    TABKEY        WINKEY
038200110111     c     *LIKE         DEFINE    tabcod        savcod
038300110111     c     *LIKE         DEFINE    tabkey        savkey
038400110111     C     *LIKE         DEFINE    tabcod        NewCOD
038500110111     C     *LIKE         DEFINE    tabkey        savkey
038600110111      *
038700960605      * Inizializzazione variabili
038800960605     C                   MOVEL     NOMPGM        V2CPGM
038900960605     C                   MOVEL     'N'           WFINE             1
039000960605     C*
039100960605     C                   ENDSR
039200100618     C/EJECT
039300100618      * ?-------------------------------------------------------------*?
039400100618      *? Riordina comunque il SFL                                     ?
039500100618      * ?-------------------------------------------------------------*?
039600100618     C     Ordina_SFL2   BEGSR
039700100618     C*
039800100618     C*  Totale Records Caricati nel 2°SFL
039900100618     c                   eval      wsfl = 1
040000100618     C*
040100100618     C*  se richiesto Ordinamento sul SFL da operatore --> F08 e F09
040200100618     C*
040300100618      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
040400100618      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
040500100618      * Il campo è usato per selezionare i records dando un ordine a quelli
040600100618      * selezionati davanti ai non selezionati.
040700100618     C                   CLEAR                   QLGSCB
040800100618     C                   CLEAR                   QLGSCB00
040900100618      *
041000100618     c                   select
041100100618      *
041200100618      *?  Ordinamento per Codice             ?
041300100618     C                   when      WrkSort = Ord_Codice  or
041400100618     C                             WrkSort = Ord_CodiceDisc
041500100618
041600100618     ** 1 campi chiave.   x Codice
041700100618     C                   EVAL      QLGNBRK    = 1
041800100618
041900100618     ** Il GIRO è in posizione (1) 10 Bytes char ascending
042000100618     C                   EVAL      QLGSP      = 1
042100100618     C                   EVAL      QLGSS      = %SIZE(V2CKEY)
042200100618     C                   EVAL      QLGDT      = Carattere
042300100618     C                   EVAL      QLGSO      = Ascendente
042400100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
042500100618     C                             WrkSort = Ord_CodiceDisc
042600100618     C                   EVAL      QLGSO      = Discendente
042700100618     c                   end
042800100618     C                   EVAL      QLGKL(1)   = QLGSKL
042900100618      *
043000100618      *?  Ordinamento per Descrizione     ?
043100100618     C                   when      WrkSort = Ord_DEscrizione  or
043200100618     C                             WrkSort = Ord_DEscrizioneDisc
043300100618      *
043400100618     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
043500100618     C                   EVAL      QLGNBRK    = 1
043600100618
043700100618     ** La Priorità se richiesta ricerca di stringa
043800100618     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)
043900100618     C                   EVAL      QLGSS    = %SIZE(V2DDES)
044000100618     C                   EVAL      QLGDT      = Carattere
044100100618     C                   EVAL      QLGSO      = Ascendente
044200100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
044300100618     C                             WrkSort = Ord_CodiceDisc
044400100618     C                   EVAL      QLGSO      = Discendente
044500100618     c                   end
044600100618     C                   EVAL      QLGKL(1) = QLGSKL
044700100930      *
044800100930      *?  Ordinamento per Cliente         ?
044900100930     C                   when      WrkSort = Ord_Cliente
045000100930      *
045100100930     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
045200100930     C                   EVAL      QLGNBRK    = 1
045300100930
045400100930     ** La Priorità se richiesta ricerca di stringa
045500100930     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)+ %SIZE(V2DDES)
045600100930     C                   EVAL      QLGSS    = %SIZE(V2CANN)
045700100930     C                   EVAL      QLGDT      = Carattere
045800100930     C                   EVAL      QLGSO      = Ascendente
045900100930     C                   EVAL      QLGKL(1) = QLGSKL
046000100618      *
046100100618     c                   endSL
046200100618      *------------
046300100618      * Load other sort parameters.
046400100618     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
046500100618     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
046600100618     C                   EVAL      QLGRT     = 8
046700100618     C                   EVAL      QLGOKL    = 80
046800100618     C                   EVAL      QLGLKE    = 16
046900100618     C                   EVAL      QLGLSS    = 290
047000100618
047100100618      * Initialize Sort I/O API fields.
047200100618     C                   EVAL      QLGRL00  = QLGRL
047300100618     C                   EVAL      QLGRC00  = 1
047400100618     C                   CLEAR                   QUSEI
047500100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
047600100618
047700100618      * First step - Initialize the sort routine.
047800100618     C                   CALL      'QLGSORT'
047900100618     C                   PARM                    QLGSCB
048000100618     C                   PARM                    NotUsed
048100100618     C                   PARM                    NotUsed
048200100618     C                   PARM                    SizeList
048300100618     C                   PARM                    ReturnSize
048400100618     C                   PARM                    QUSEC
048500100618
048600100618      * Next step - Write records to I/O routine.
048700100618     C                   EVAL      QLGRT00 = Put
048800100618
048900100618     C                   DO        RrnLast       S2nrr_xx          5 0
049000100618     C     S2nrr_xx      CHAIN     TC91S02
049100100618
049200100618     ** Solo le righe con Selected = 'Y' sono riordinate,
049300100618     ** quindi per fare un ordinamento di tutte le righe
049400100618     ** metto 'Y' sempre.
049500100618     C                   EVAL      Selected  = 'Y'
049600100618
049700100618     C                   CLEAR                   QUSEI
049800100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
049900100618      *
050000100618     C                   CALL      'QLGSRTIO'
050100100618     C                   PARM                    QLGSCB00
050200100618     C                   PARM                    SflRcd
050300100618     C                   PARM                    NotUsed
050400100618     C                   PARM                    SizeList
050500100618     C                   PARM                    NotUsed
050600100618     C                   PARM                    QUSEC
050700100618
050800100618     C                   ENDDO
050900100618
051000100618      * Next step - Signal end of input, clear subfile for reload.
051100100618     C                   EVAL      QLGRT00 = EndPut
051200100618     C                   CLEAR                   QUSEI
051300100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051400100618
051500100618     C                   CALL      'QLGSRTIO'
051600100618     C                   PARM                    QLGSCB00
051700100618     C                   PARM                    SflRcd
051800100618     C                   PARM                    NotUsed
051900100618     C                   PARM                    SizeList
052000100618     C                   PARM                    NotUsed
052100100618     C                   PARM                    QUSEC
052200100618      *
  pulizia SFL 2  ?
052300100618     C                   SETOFF                                         2021
052400100618     C                   WRITE     TC91C02
052500100618     C                   SETON                                          2120
052600100618
052700100618      * Final step - Write the records back to the subfile.
052800100618     C                   EVAL      QLGRT00 = Get
052900100618
053000100618     C                   DO        RrnLast       S2nrr_xx
053100100618     c                   eval      nrr2  = S2nrr_xx
053200100618     C                   CLEAR                   QUSEI
053300100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053400100618     C                   CALL      'QLGSRTIO'
053500100618     C                   PARM                    QLGSCB00
053600100618     C                   PARM                    NotUsed
053700100618     C                   PARM                    SflRcd
053800100618     C                   PARM                    QLGRL00
053900100618     C                   PARM                    NotUsed
054000100618     C                   PARM                    QUSEC
054100100618     C                   WRITE     TC91S02
054200100618     C                   ENDDO
054300100618      *
054400100618      *  All'uscita di queste chiamate a routine di sistema il SFL record
054500100618      *   si incrementa stranamente quindi lo reimposto correttamente
054600100618      *    prima di lasciare la routine.
054700100618     C                   EVAL      nrr2 = RrnLast
054800100618     C*
054900100618     C                   ENDSR
055000100618     C/EJECT
055100100618      * ?-------------------------------------------------------------*?
