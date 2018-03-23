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
014700130219     c                   if        nrr2 = 0
014800130219     C                   Z-ADD     1             SFNR2
014900130219     c                   else
015000130219     C                   Z-ADD     SFLRIGA       SFNR2
015100130219     c                   end
015200100618      *  ORDINAMENTO ascendente
015300100618     c   kH              if        *in08
015400100618     c                   setoff                                       0809
015500100618     C                   eval      WrkSort = Ord_CodiceDisc
015600100618     c                   exsr      Ordina_SFL2
015700100618     c                   else
015800100618     c                   seton                                        0809
015900100618     C                   eval      WrkSort = Ord_Codice
016000100618     c                   exsr      Ordina_SFL2
016100100618     c                   end
016200100930      *  ORDINAMENTO x c.cliente
016300100930     c   KG              if        D91cod = 'PT' or D91cod = 'CL'
016400100930     C                   eval      WrkSort = Ord_Cliente
016500100930     c                   exsr      Ordina_SFL2
016600100930     c                   end
016700100618      *  ORDINAMENTO Discendente
016800100618     c   ki              if        *in09
016900100618     c                   setoff                                       0908
017000100618     C                   eval      WrkSort = Ord_DescrizioneDisc
017100100618     c                   exsr      Ordina_SFL2
017200100618     c                   else
017300100618     c                   seton                                        0908
017400100618     C                   eval      WrkSort = Ord_Descrizione
017500100618     c                   exsr      Ordina_SFL2
017600100618     c                   end
017700960605     C* Ritorno
017800110321     c                   if        *inKL
017900110321     c                               or
018000110321     c                             *inKC and D91cod = 'PT'
018100110321     c                               or
018200110321     c                             *inKC and D91cod = 'CL'
018300960605     C                   MOVEL     '1'           WFINE
018400960606     C                   MOVEL     '12'          D91CMD
018500000306     C                   MOVEL     TRTC91DS      KPJBU
018600960605     C                   GOTO      FINSF2
018700960605     C                   END
018800960605     C* ROLLUP
018900130219     C***  *IN25         IFEQ      '1'
019000130219     C***                EXSR      ROLL02
019100130219     C****               Z-ADD     NRR2          SFNR2
019200130219     C****               GOTO      FINSF2
019300130219     C***                END
019400960605     C* Seleziono codice
019500960605     C                   READC     TC91S02                                30
019600960606     C     *IN30         IFEQ      '0'
019700960606     C     V2CSCE        ANDEQ     '1'
019800960605     C                   MOVEL     V2CKEY        D91KEY
019900960605     C                   MOVEL     V2DKEY        D91UNI
020000000306     C                   MOVEL     TRTC91DS      KPJBU
020100960605     C                   MOVEL     'S'           WFINE
020200960605     C                   END
020300960605     C*
020400960605     C     FINSF2        ENDSR
020500960605     C*---------------------------------------------------------------*
020600960605     C*  GESD02: Gestione tabelle inesistente
020700960605     C*---------------------------------------------------------------*
020800960605     C     GESD02        BEGSR
020900960605     C*
021000960605     C                   EXFMT     TC91D02
021100960605     C* Ritorno
021200960605     C     *INKL         IFEQ      '1'
021300960605     C                   MOVEL     '1'           WFINE
021400960606     C                   MOVEL     '12'          D91CMD
021500000306     C                   MOVEL     TRTC91DS      KPJBU
021600960605     C                   END
021700960605     C*
021800960605     C                   ENDSR
021900960605     C*---------------------------------------------------------------*
022000960605     C*  INZ02: Inizializzazione dati ricerca singola tabella
022100960605     C*---------------------------------------------------------------*
022200960605     C     INZ02         BEGSR
022300960605     C*
022400960605     C                   MOVEL     '99'          KCOD
022500960605     C                   MOVEL     *BLANKS       KKEY
022600960606     C                   MOVEL     D91COD        KKEY
022700960605     C     KTAB          CHAIN     EDTAB01L                           30
022800960605     C     *IN30         IFEQ      '0'
022900960606     C                   MOVEL     D91COD        V2CCOD
023000960605     C                   MOVEL     TABUNI        V2DCOD
023100960605     C                   END
023200100930     C*
023300100930     c                   if        D91cod = 'PT'
023400100930     C                   EVAL      V2TANN ='C.CLI.'
023500110214     C                   EVAL      V2TDES ='  DATI           *=NO trad Std'
023600100930     C                   END
023700100930     C*
023800100930     c                   if        D91cod = 'CL'
023900100930     C                   EVAL      V2TANN ='C.CLI.'
024000100930     C                   EVAL      V2TDES ='  DATI'
024100100930     C                   END
024200960605     C*
024300960605     C                   ENDSR
024400960605     C*---------------------------------------------------------------*
024500960605     C*  ROLL02: Caricamento una pagina sufile chiavi singola tabella
024600960605     C*---------------------------------------------------------------*
024700960605     C     ROLL02        BEGSR
024800960605     C*
024900960606     C                   Z-ADD     0             WNRIG2            2 0
025000960605     C                   MOVEL     WINKEY        KKEY
025100960612     C                   MOVEL     D91COD        KCOD
025200000306      *
025300960605     C     KTAB          SETGT     EDTAB01L                           30
025400960605     C     KCOD          READE     EDTAB01L                               30
025500000306      *
025600000306     C                   DOW        *IN30 = *off  and  Wnrig2 < 16
025700960606     C                   ADD       1             NRR2              4 0
025800960606     C                   ADD       1             WNRIG2
025900960605     C                   MOVEL     TABKEY        V2CKEY
026000960605     C                   MOVEL     TABUNI        V2DKEY
026100000306     C                   If         TABFLG = *blanks
026200000306     C                   clear                   V2CANN
026300100929      * viene visualizzato il codice del cliente/Partner (PT)
026400100929     c                   if        D91cod = 'PT'
026500100929     C                   eval      V2CANN = %subst(TABuni:1:7)
026600100929     C                   Endif
026700100930     c                   if        D91cod = 'CL'
026800110606     C*******            eval      V2CANN = %subst(TABKEY:1:7)
026900110606     C                   eval      V2CANN = %subst(TABUNI:1:7)
027000100930     C                   Endif
027100000306     C                   Else
027200000306     c                   eval      V2Cann = 'ANNULLATO'
027300000306     C                   Endif
027400000306      *
027500040216     c                   clear                   v2ddes
027600040216      * solo x la PT decodifica il Partner/Cliente
027700040216     c                   if        D91cod = 'PT'
027800040216     C                   MOVEL     TABUNI        dspt
027900040216     C                   Z-ADD     1             KKUT
028000040216     C                   Z-ADD     KCI           KKCC
028100040216     C                   Z-ADD     §PTKSC        KKSC
028200040216     C     KACO          CHAIN     CNACO00F                           30
028300040216     C  N30              MOVEL     ACORAG        V2DDES
028400110111      *
028500110111     c                   move      tabcod        savcod
028600110111     c                   move      tabkey        savkey
028700110111      *
028800110111      *
028900110111     c                   move      'PZ'          NewCOD
029000110111     C     NTAB          CHAIN     EDTAB01L
029100110111     c                   if        %Found(EDTAB01L)
029200110111     c                   eval      dspz = tabUNI
029300110111     c                   if        §PZPGMRCZB = 'INTESA'  or
029400110111     c                             §PZPGMRCZB = *blank
029500110214     c                   move      '|*'          v2dDES
029600110214     c                   else
029700110214     c                   move      '| '          v2dDES
029800110214     c                   end
029900110111     c                   end
030000110111     C     STAB          SETGT     EDTAB01L                           30
030100040216     c                   end
030200040216      *
030300040216      * solo x la CE decodifica
030400040216     c                   if        D91cod = 'CE'
030500040216     C                   MOVEL     TABUNI        dsce
030600040216     C                   MOVEL     §CEdec        V2DDES
030700040216     c                   end
030800040216      *
030900040216      * solo x la EA decodifica
031000040216     c                   if        D91cod = 'EA'
031100040216     C                   MOVEL     TABUNI        dsea
031200040216     C                   MOVEL     §EAdes        V2DDES
031300040216     c                   end
031400040216      *
031500040216      * solo x la EM decodifica
031600040216     c                   if        D91cod = 'EM'
031700040216     C                   MOVEL     TABUNI        dsem
031800040216     C                   MOVEL     §EMdes        V2DDES
031900040216     c                   end
032000050223      *
032100050223      * solo x la CL decodifica
032200050223     c                   if        D91cod = 'CL'
032300050223     C                   MOVEL     TABUNI        dsCL
032400050223     C                   MOVEL     §CLdes        V2DDES
032500050223     c                   end
032600040216      *
032700960606     C                   WRITE     TC91S02
032800000306      *
032900000306     C                   If        Wnrig2 < 16
033000960606     C     KCOD          READE     EDTAB01L                               30
033100000306     c                   Endif
033200000306      *
033300000306     C                   ENDDO
033400000306      *
033500960606     C                   MOVEL     TABKEY        WINKEY
033600100618      *
033700100618     C*  Se sono a fine file accendo 23
033800960606     C     *IN30         COMP      '1'                                    23
033900960605     C*
034000100618     C                   EVAL      RrnLast = nrr2
034100100618     C*
034200960605     C                   ENDSR
034300960605     C*---------------------------------------------------------------*
034400960605     C*  *INZSR: Operazioni iniziali                                  *
034500960605     C*---------------------------------------------------------------*
034600960605     C     *INZSR        BEGSR
034700960605     C*
034800960605     C     *ENTRY        PLIST
034900960605     C                   PARM                    KPJBA
035000000306     C                   MOVEL     KPJBU         TRTC91DS
035100960605      *  Call a XPARUT
035200960605     C                   Z-ADD     1             CODUT
035300960605     C                   CALL      'X§PARUT'
035400000306     C                   PARM                    UT§DSE0F
035500960605     C                   MOVEL     REC80         CNCR80
035600960605     C                   MOVEL     RAGUT         RSUT
035700040216     C*--- RICERCA CAPOCONTI------------------------------------------*
035800040216     C                   DO        50            X                 3 0
035900040216     C                   MOVE      TCU(X)        TCUDS
036000040216     C     F56           IFEQ      'CG'
036100040216     C     F34           ANDEQ     '01'
036200040216     C                   Z-ADD     KCU(X)        KCI               4 0
036300040216     C                   END
036400040216     C                   END
036500960605      * Definizione chiavi
036600960605     C     KTAB          KLIST
036700960605     C                   KFLD                    KCOD
036800960605     C                   KFLD                    KKEY
036900110111     C     NTAB          KLIST
037000110111     C                   KFLD                    NewCOD
037100110111     C                   KFLD                    savkey
037200110111     C     STAB          KLIST
037300110111     C                   KFLD                    SavCOD
037400110111     C                   KFLD                    SavKEY
037500040216     C     KACO          KLIST
037600040216     C                   KFLD                    KKUT
037700040216     C                   KFLD                    KKCC
037800040216     C                   KFLD                    KKSC
037900960605      * Definizione varibili
038000040216     C     *LIKE         DEFINE    ACOKUT        KKUT
038100040216     C     *LIKE         DEFINE    ACOKCC        KKCC
038200040216     C     *LIKE         DEFINE    ACOKSC        KKSC
038300960605     C     *LIKE         DEFINE    TABCOD        KCOD
038400960605     C     *LIKE         DEFINE    TABKEY        KKEY
038500960606     C     *LIKE         DEFINE    TABKEY        WINKEY
038600110111     c     *LIKE         DEFINE    tabcod        savcod
038700110111     c     *LIKE         DEFINE    tabkey        savkey
038800110111     C     *LIKE         DEFINE    tabcod        NewCOD
038900110111     C     *LIKE         DEFINE    tabkey        savkey
039000110111      *
039100960605      * Inizializzazione variabili
039200960605     C                   MOVEL     NOMPGM        V2CPGM
039300960605     C                   MOVEL     'N'           WFINE             1
039400960605     C*
039500960605     C                   ENDSR
039600100618     C/EJECT
039700100618      * ?-------------------------------------------------------------*?
039800100618      *? Riordina comunque il SFL                                     ?
039900100618      * ?-------------------------------------------------------------*?
040000100618     C     Ordina_SFL2   BEGSR
040100100618     C*
040200100618     C*  Totale Records Caricati nel 2°SFL
040300100618     c                   eval      wsfl = 1
040400100618     C*
040500100618     C*  se richiesto Ordinamento sul SFL da operatore --> F08 e F09
040600100618     C*
040700100618      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
040800100618      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
040900100618      * Il campo è usato per selezionare i records dando un ordine a quelli
041000100618      * selezionati davanti ai non selezionati.
041100100618     C                   CLEAR                   QLGSCB
041200100618     C                   CLEAR                   QLGSCB00
041300100618      *
041400100618     c                   select
041500100618      *
041600100618      *?  Ordinamento per Codice             ?
041700100618     C                   when      WrkSort = Ord_Codice  or
041800100618     C                             WrkSort = Ord_CodiceDisc
041900100618
042000100618     ** 1 campi chiave.   x Codice
042100100618     C                   EVAL      QLGNBRK    = 1
042200100618
042300100618     ** Il GIRO è in posizione (1) 10 Bytes char ascending
042400100618     C                   EVAL      QLGSP      = 1
042500100618     C                   EVAL      QLGSS      = %SIZE(V2CKEY)
042600100618     C                   EVAL      QLGDT      = Carattere
042700100618     C                   EVAL      QLGSO      = Ascendente
042800100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
042900100618     C                             WrkSort = Ord_CodiceDisc
043000100618     C                   EVAL      QLGSO      = Discendente
043100100618     c                   end
043200100618     C                   EVAL      QLGKL(1)   = QLGSKL
043300100618      *
043400100618      *?  Ordinamento per Descrizione     ?
043500100618     C                   when      WrkSort = Ord_DEscrizione  or
043600100618     C                             WrkSort = Ord_DEscrizioneDisc
043700100618      *
043800100618     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
043900100618     C                   EVAL      QLGNBRK    = 1
044000100618
044100100618     ** La Priorità se richiesta ricerca di stringa
044200100618     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)
044300100618     C                   EVAL      QLGSS    = %SIZE(V2DDES)
044400100618     C                   EVAL      QLGDT      = Carattere
044500100618     C                   EVAL      QLGSO      = Ascendente
044600100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
044700100618     C                             WrkSort = Ord_CodiceDisc
044800100618     C                   EVAL      QLGSO      = Discendente
044900100618     c                   end
045000100618     C                   EVAL      QLGKL(1) = QLGSKL
045100100930      *
045200100930      *?  Ordinamento per Cliente         ?
045300100930     C                   when      WrkSort = Ord_Cliente
045400100930      *
045500100930     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
045600100930     C                   EVAL      QLGNBRK    = 1
045700100930
045800100930     ** La Priorità se richiesta ricerca di stringa
045900100930     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)+ %SIZE(V2DDES)
046000100930     C                   EVAL      QLGSS    = %SIZE(V2CANN)
046100100930     C                   EVAL      QLGDT      = Carattere
046200100930     C                   EVAL      QLGSO      = Ascendente
046300100930     C                   EVAL      QLGKL(1) = QLGSKL
046400100618      *
046500100618     c                   endSL
046600100618      *------------
046700100618      * Load other sort parameters.
046800100618     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
046900100618     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
047000100618     C                   EVAL      QLGRT     = 8
047100100618     C                   EVAL      QLGOKL    = 80
047200100618     C                   EVAL      QLGLKE    = 16
047300100618     C                   EVAL      QLGLSS    = 290
047400100618
047500100618      * Initialize Sort I/O API fields.
047600100618     C                   EVAL      QLGRL00  = QLGRL
047700100618     C                   EVAL      QLGRC00  = 1
047800100618     C                   CLEAR                   QUSEI
047900100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
048000100618
048100100618      * First step - Initialize the sort routine.
048200100618     C                   CALL      'QLGSORT'
048300100618     C                   PARM                    QLGSCB
048400100618     C                   PARM                    NotUsed
048500100618     C                   PARM                    NotUsed
048600100618     C                   PARM                    SizeList
048700100618     C                   PARM                    ReturnSize
048800100618     C                   PARM                    QUSEC
048900100618
049000100618      * Next step - Write records to I/O routine.
049100100618     C                   EVAL      QLGRT00 = Put
049200100618
049300100618     C                   DO        RrnLast       S2nrr_xx          5 0
049400100618     C     S2nrr_xx      CHAIN     TC91S02
049500100618
049600100618     ** Solo le righe con Selected = 'Y' sono riordinate,
049700100618     ** quindi per fare un ordinamento di tutte le righe
049800100618     ** metto 'Y' sempre.
049900100618     C                   EVAL      Selected  = 'Y'
050000100618
050100100618     C                   CLEAR                   QUSEI
050200100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
050300100618      *
050400100618     C                   CALL      'QLGSRTIO'
050500100618     C                   PARM                    QLGSCB00
050600100618     C                   PARM                    SflRcd
050700100618     C                   PARM                    NotUsed
050800100618     C                   PARM                    SizeList
050900100618     C                   PARM                    NotUsed
051000100618     C                   PARM                    QUSEC
051100100618
051200100618     C                   ENDDO
051300100618
051400100618      * Next step - Signal end of input, clear subfile for reload.
051500100618     C                   EVAL      QLGRT00 = EndPut
051600100618     C                   CLEAR                   QUSEI
051700100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051800100618
051900100618     C                   CALL      'QLGSRTIO'
052000100618     C                   PARM                    QLGSCB00
052100100618     C                   PARM                    SflRcd
052200100618     C                   PARM                    NotUsed
052300100618     C                   PARM                    SizeList
052400100618     C                   PARM                    NotUsed
052500100618     C                   PARM                    QUSEC
052600100618      *
  pulizia SFL 2  ?
052700100618     C                   SETOFF                                         2021
052800100618     C                   WRITE     TC91C02
052900100618     C                   SETON                                          2120
053000100618
053100100618      * Final step - Write the records back to the subfile.
053200100618     C                   EVAL      QLGRT00 = Get
053300100618
053400100618     C                   DO        RrnLast       S2nrr_xx
053500100618     c                   eval      nrr2  = S2nrr_xx
053600100618     C                   CLEAR                   QUSEI
053700100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053800100618     C                   CALL      'QLGSRTIO'
053900100618     C                   PARM                    QLGSCB00
054000100618     C                   PARM                    NotUsed
054100100618     C                   PARM                    SflRcd
054200100618     C                   PARM                    QLGRL00
054300100618     C                   PARM                    NotUsed
054400100618     C                   PARM                    QUSEC
054500100618     C                   WRITE     TC91S02
054600100618     C                   ENDDO
054700100618      *
054800100618      *  All'uscita di queste chiamate a routine di sistema il SFL record
054900100618      *   si incrementa stranamente quindi lo reimposto correttamente
055000100618      *    prima di lasciare la routine.
055100100618     C                   EVAL      nrr2 = RrnLast
055200100618     C*
055300100618     C                   ENDSR
055400100618     C/EJECT
055500100618      * ?-------------------------------------------------------------*?
