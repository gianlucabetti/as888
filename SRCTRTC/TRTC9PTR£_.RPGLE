000100000306      ***********************************************************************
000200960605      *   RICERCA TABELLE PER  E.D.I. ESTERO                                *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500000306      *---------------------------------------------------------------------*
000600000306      *  DATA BASE                                                          *
000700000306      *---------------------------------------------------------------------*
000800040216     FCNACO00F  IF   E           K DISK
000900960605     FEDTAB01L  IF   E           K DISK
001000110421     FTRTC9PTD  CF   E             WORKSTN SFILE(TCB9S02:NRR2)
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
003100110422     D Vis_Dati        S              1s 0 inz(1)
003200100618      **********************************************************************
003300100618      * Constants
003400100618      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
003500100618      **********************************************************************
003600110421$xxx D Ord_UNB...
003700100618$xxx D                 C                   1
003800100618$xxx D Ord_Descrizione...
003900100618$xxx D                 C                   2
004000110421$xxx D Ord_UNBDisc...
004100100618$xxx D                 C                   3
004200100618$xxx D Ord_DescrizioneDisc...
004300100618$xxx D                 C                   4
004400100930$xxx D Ord_Cliente...
004500100930$xxx D                 C                   5
004600110421$xxx D Ord_ClienteDisc...
004700110421$xxx D                 C                   6
004800110421      **********************************************************************
004900100618      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
005000100618     D MaxKey          C                   9
005100100618     D Ascendente      C                   1
005200100618     D Discendente     C                   2
005300100618     D Carattere       C                   6
005400100618     D Put             C                   1
005500100618     D EndPut          C                   2
005600100618     D Get             C                   3
005700100618     D Numerico        C                   8
005800110421      **********************************************************************
005900110422     D UpBolle         C                   'Mod.scambio Bolle UPL'
006000110422     D DwBolle         C                   '                  DWN'
006100110422     D UpStati         C                   '                  UPL'
006200110422     D DwStati         C                   'Mod.scambio Esiti DWN'
006300110422     D UpFile1         C                   'File UPL     '
006400110422     D UpFile2         C                   '     UPL     '
006500110422     D DwFile1         C                   '     DWN     '
006600110422     D DwFile2         C                   '     DWN     '
006700110422     D UpTrsm1         C                   'Trsm UPL     '
006800110422     D UpTrsm2         C                   '     UPL     '
006900110422     D DwTrsm1         C                   '     DWN     '
007000110422     D DwTrsm2         C                   '     DWN     '
007100100618      **********************************************************************
007200100618      * Campi utili:
007300100618      *     SizeList   - Dimensione della lista
007400100618      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
007500100618      **********************************************************************
007600100618     D NotUsed         S             16A
007700100618     D ReturnSize      S              9B 0
007800100618     D SizeList        S              9B 0
007900100618     D RrnLast         S              5I 0
008000100618     D WrkSort         S              1  0 inz(0)
008100100618     D WindSort        S              3I 0 inz(0)
008200100618      **********************************************************************
008300100618      * Data Structures
008400100618      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
008500100618      *     QLGSCB     - The sort request block for the QLGSORT API
008600100618      *     QLGSCB00   - The sort request block for the QLGSRTIO API
008700100618      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
008800100618      *     QUSEC      - Error structure for the QLGSORT API
008900100618      **********************************************************************
009000100618     D SflRcd          DS
009100100618     D  V2CKEY
009200100618     D  V2DDES
009300100618     D  V2CANN
009400100618     D  V2CSCE
009500100618     D  V2DKEY
009600110421     D  V2UPLD
009700110421     D  V2UPLS
009800110421     D  V2DWLD
009900110421     D  V2DWLS
010000110422     D  V2tes1
010100110422     D  V2tes2
010200110422     D  V2tes3
010300110422     D  V2tes4
010400100618     D  Selected                      1A
010500100618
010600100618      /COPY QSYSINC/QRPGLESRC,QLGSORT
010700100618     D QLGKL                         16    DIM(MaxKey)
010800100618     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
010900100618     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
011000100618     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
011100100618     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
011200100618
011300100618      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
011400100618      /COPY QSYSINC/QRPGLESRC,QUSEC
011500100618      *---------------------------------------------------------------*
011600000306      *  CICLO PRINCIPALE
011700000306      *---------------------------------------------------------------*
011800960605      * Inizializzo videate
011900960605     C                   EXSR      CARSF2                                       le key del cod.
012000100618      *
012100960605      * Loop gestione videate
012200960605     C     WFINE         DOWEQ     'N'
012300110421     C                   exsr      GESS02                                      Sbf.scelta key.
012400960605     C                   END
012500960605      * Fine Lavoro
012600960605     C     FINE          TAG
012700960605     C                   SETON                                        LR
012800000306      *---------------------------------------------------------------*
012900000306      *  CARSF2: Caricamento subfile delle chiavi
013000000306      *---------------------------------------------------------------*
013100960605     C     CARSF2        BEGSR
013200960605      *
013300960605      *  Pulisco dati SFL
013400960605     C                   SETOFF                                       2021
013500110421     C                   WRITE     TCB9C02
013600960605     C                   SETON                                        2021
013700110421     C                   SETOFF                                       071011
013800100618      *
013900960605      *  Eseguo caricamento prima pagina
014000110421     C                   MOVEL     'PT'          KCOD
014100960605     C                   MOVEL     *LOVAL        WINKEY
014200960612     C                   Z-ADD     1             SFNR2
014300100618      *
014400100618     c     avanti        tag
014500110421      *
014600960605     C                   EXSR      ROLL02
014700110421      *
014800100618     c  n23              goto      avanti
014900100618      *
015000960605      *  Se non ho dati da caricare emetto videata vuota
015100960605     C                   EXSR      INZ02
015200110421      *
015300110421     C*  reset degli ordinamenti
015400110421     C                   SETOFF                                       071011
015500100618     C*
015600960605     C                   ENDSR
015700960605     C*---------------------------------------------------------------*
015800960605     C*  GESS02: Gestione singola tabella
015900960605     C*---------------------------------------------------------------*
016000960605     C     GESS02        BEGSR
016100960605     C*
016200110421     C                   WRITE     TCB9T02
016300110421     C                   EXFMT     TCB9C02
016400110422     C*-------
016500960612     C                   Z-ADD     1             SFNR2
016600110421     C*
016700100618      *  ORDINAMENTO ascendente
016800110421     c   kJ              if        *in10
016900110421     c                   setoff                                       1011
017000110421     C                   eval      WrkSort = Ord_UNB
017100110421     c                   exsr      Ordina_SFL2
017200110421     c                   else
017300110421     c                   seton                                        1011
017400110421     C                   eval      WrkSort = Ord_UNBDisc
017500100618     c                   exsr      Ordina_SFL2
017600100618     c                   end
017700110421     C*
017800100618      *  ORDINAMENTO Discendente
017900110421     c   kK              if        *in11
018000110421     c                   setoff                                       1110
018100110421     C                   eval      WrkSort = Ord_Descrizione
018200100618     c                   exsr      Ordina_SFL2
018300100618     c                   else
018400110421     c                   seton                                        1110
018500110421     C                   eval      WrkSort = Ord_DescrizioneDisc
018600100618     c                   exsr      Ordina_SFL2
018700100618     c                   end
018800110421     C*
018900110421      *  ORDINAMENTO x c.cliente
019000110422     c   kG              if        *in07
019100110421     c                   setoff                                       1110
019200110421     c                   setoff                                       07
019300110421     C                   eval      WrkSort = Ord_Cliente
019400110421     c                   exsr      Ordina_SFL2
019500110421     c                   else
019600110421     c                   setoff                                       1110
019700110421     c                   seton                                        07
019800110421     C                   eval      WrkSort = Ord_ClienteDisc
019900110421     c                   exsr      Ordina_SFL2
020000110421     c                   end
020100960605     C* Ritorno
020200110321     c                   if        *inKL
020300110321     c                               or
020400110421     c                             *inKC
020500960605     C                   MOVEL     '1'           WFINE
020600960606     C                   MOVEL     '12'          D91CMD
020700110421     C                   MOVEL     TRTC91DS      KPJBU
020800960605     C                   GOTO      FINSF2
020900960605     C                   END
021000110421     C*
021100960605     C* ROLLUP
021200110422     C     *IN25         IFEQ      '1'
021300110422     C*******            EXSR      ROLL02
021400110422     C                   Z-ADD     NRR2          SFNR2
021500110422     C                   GOTO      FINSF2
021600110422     c                   END
021700110421     C*
021800960605     C* Seleziono codice
021900110421     C                   READC     TCB9S02                                30
022000110421     C*
022100960606     C     *IN30         IFEQ      '0'
022200960606     C     V2CSCE        ANDEQ     '1'
022300960605     C                   MOVEL     V2CKEY        D91KEY
022400960605     C                   MOVEL     V2DKEY        D91UNI
022500000306     C                   MOVEL     TRTC91DS      KPJBU
022600960605     C                   MOVEL     'S'           WFINE
022700960605     C                   END
022800960605     C*
022900960605     C     FINSF2        ENDSR
023000960605     C*---------------------------------------------------------------*
023100960605     C*  GESD02: Gestione tabelle inesistente
023200960605     C*---------------------------------------------------------------*
023300960605     C     GESD02        BEGSR
023400960605     C*
023500110421     C                   EXFMT     TCB9D02
023600960605     C* Ritorno
023700960605     C     *INKL         IFEQ      '1'
023800960605     C                   MOVEL     '1'           WFINE
023900960606     C                   MOVEL     '12'          D91CMD
024000110421     C                   MOVEL     TRTC91DS      KPJBU
024100960605     C                   END
024200960605     C*
024300960605     C                   ENDSR
024400960605     C*---------------------------------------------------------------*
024500960605     C*  INZ02: Inizializzazione dati ricerca singola tabella
024600960605     C*---------------------------------------------------------------*
024700960605     C     INZ02         BEGSR
024800100930     C*
024900100930     C                   EVAL      V2TANN ='C.CLI.'
025000110422     C                   EVAL      V2TDES ='  DATI     UPL.(*)=NO trad Std'
025100960605     C*
025200960605     C                   ENDSR
025300960605     C*---------------------------------------------------------------*
025400960605     C*  ROLL02: Caricamento una pagina sufile chiavi singola tabella
025500960605     C*---------------------------------------------------------------*
025600960605     C     ROLL02        BEGSR
025700960605     C*
025800960606     C                   Z-ADD     0             WNRIG2            2 0
025900960605     C                   MOVEL     WINKEY        KKEY
026000960612     C                   MOVEL     D91COD        KCOD
026100000306      *
026200110422     C     KTAB          SETGT     EDTAB01L
026300960605     C     KCOD          READE     EDTAB01L                               30
026400000306      *
026500110422     C                   DOW        *IN30 = *off  and  Wnrig2 < 10
026600960606     C                   ADD       1             NRR2              4 0
026700960606     C                   ADD       1             WNRIG2
026800960605     C                   MOVEL     TABKEY        V2CKEY
026900960605     C                   MOVEL     TABUNI        V2DKEY
027000000306     C                   If         TABFLG = *blanks
027100000306     C                   clear                   V2CANN
027200100929      * viene visualizzato il codice del cliente/Partner (PT)
027300100929     C                   eval      V2CANN = %subst(TABuni:1:7)
027400000306     C                   Else
027500000306     c                   eval      V2Cann = 'ANNULLATO'
027600000306     C                   Endif
027700000306      *
027800040216     c                   clear                   v2ddes
027900040216     C                   MOVEL     TABUNI        dspt
028000040216     C                   Z-ADD     1             KKUT
028100040216     C                   Z-ADD     KCI           KKCC
028200040216     C                   Z-ADD     §PTKSC        KKSC
028300040216     C     KACO          CHAIN     CNACO00F                           30
028400040216     C  N30              MOVEL     ACORAG        V2DDES
028500110111      *
028600110111     c                   move      tabcod        savcod
028700110111     c                   move      tabkey        savkey
028800110111      *
028900110111     c                   move      'PZ'          NewCOD
029000110111     C     NTAB          CHAIN     EDTAB01L
029100110422      *
029200110111     c                   if        %Found(EDTAB01L)
029300110111     c                   eval      dspz = tabUNI
029400110422      *
029500110422     c                   if        §PZPGMRCZB = 'INTESA'  or
029600110422     c                             §PZPGMRCZB = 'TRADUT'
029700110421     c                   move      '|*'          v2dDES
029800110214     c                   else
029900110214     c                   move      '| '          v2dDES
030000110214     c                   end
030100110422      *
030200110422     c                   if         vis_dati = 1
030300110421     c                   move(p)   UpBolle       v2tes1
030400110421     c                   move(p)   DwBolle       v2tes2
030500110422     c                   move(p)   UpStati       v2tes4
030600110422     c                   move(p)   DwStati       v2tes3
030700110422     c                   movel(p)  §PZFILRCZB    v2uplD
030800110422     c                   movel(p)  §PZFILRCZS    v2uplS
030900110422     c                   movel(p)  §PZFILTRSB    v2dwlD
031000110422     c                   movel(p)  §PZFILTRSS    v2dwlS
031100110422     c                   elseIf     vis_dati = 2
031200110422     c                   move(p)   UpFile1       v2tes1
031300110422     c                   move(p)   DwFile1       v2tes2
031400110422     c                   move(p)   UpFile2       v2tes4
031500110422     c                   move(p)   DwFile2       v2tes3
031600110422     c                   movel(p)  §PZPGMRCZB    v2uplD
031700110422     c                   movel(p)  §PZPGMRCZS    v2uplS
031800110422     c                   movel(p)  §PZPGMTRSB    v2dwlD
031900110422     c                   movel(p)  §PZPGMTRSS    v2dwlS
032000110422     c                   elseIf     vis_dati = 3
032100110422     c                   move(p)   UpTrsm1       v2tes1
032200110422     c                   move(p)   DwTrsm1       v2tes2
032300110422     c                   move(p)   UpTrsm2       v2tes4
032400110422     c                   move(p)   DwTrsm2       v2tes3
032500110422     c                   movel(p)  §PZTIPRCZB    v2uplD
032600110422     c                   movel(p)  §PZTIPRCZS    v2uplS
032700110422     c                   movel(p)  §PZTIPTRSB    v2dwlD
032800110422     c                   movel(p)  §PZTIPTRSS    v2dwlS
032900110422     c                   end
033000110422      *
033100110111     c                   end
033200110422      *
033300110111     C     STAB          SETGT     EDTAB01L                           30
033400110421     C                   WRITE     TCB9S02
033500000306      *
033600110422     C                   If        Wnrig2 < 10
033700960606     C     KCOD          READE     EDTAB01L                               30
033800000306     c                   Endif
033900000306      *
034000000306     C                   ENDDO
034100000306      *
034200960606     C                   MOVEL     TABKEY        WINKEY
034300100618      *
034400100618     C*  Se sono a fine file accendo 23
034500960606     C     *IN30         COMP      '1'                                    23
034600960605     C*
034700100618     C                   EVAL      RrnLast = nrr2
034800100618     C*
034900960605     C                   ENDSR
035000960605     C*---------------------------------------------------------------*
035100960605     C*  *INZSR: Operazioni iniziali                                  *
035200960605     C*---------------------------------------------------------------*
035300960605     C     *INZSR        BEGSR
035400960605     C*
035500960605     C     *ENTRY        PLIST
035600960605     C                   PARM                    KPJBA
035700000306     C                   MOVEL     KPJBU         TRTC91DS
035800110422     c                   movel     'PT'          D91COD
035900960605      *  Call a XPARUT
036000960605     C                   Z-ADD     1             CODUT
036100960605     C                   CALL      'X§PARUT'
036200000306     C                   PARM                    UT§DSE0F
036300960605     C                   MOVEL     REC80         CNCR80
036400110421     C                   MOVEL     RAGUT         RSUT
036500040216     C*--- RICERCA CAPOCONTI------------------------------------------*
036600040216     C                   DO        50            X                 3 0
036700040216     C                   MOVE      TCU(X)        TCUDS
036800040216     C     F56           IFEQ      'CG'
036900040216     C     F34           ANDEQ     '01'
037000040216     C                   Z-ADD     KCU(X)        KCI               4 0
037100040216     C                   END
037200040216     C                   END
037300960605      * Definizione chiavi
037400960605     C     KTAB          KLIST
037500960605     C                   KFLD                    KCOD
037600960605     C                   KFLD                    KKEY
037700110111     C     NTAB          KLIST
037800110111     C                   KFLD                    NewCOD
037900110111     C                   KFLD                    savkey
038000110111     C     STAB          KLIST
038100110111     C                   KFLD                    SavCOD
038200110111     C                   KFLD                    SavKEY
038300040216     C     KACO          KLIST
038400040216     C                   KFLD                    KKUT
038500040216     C                   KFLD                    KKCC
038600040216     C                   KFLD                    KKSC
038700960605      * Definizione varibili
038800040216     C     *LIKE         DEFINE    ACOKUT        KKUT
038900040216     C     *LIKE         DEFINE    ACOKCC        KKCC
039000040216     C     *LIKE         DEFINE    ACOKSC        KKSC
039100960605     C     *LIKE         DEFINE    TABCOD        KCOD
039200960605     C     *LIKE         DEFINE    TABKEY        KKEY
039300960606     C     *LIKE         DEFINE    TABKEY        WINKEY
039400110111     c     *LIKE         DEFINE    tabcod        savcod
039500110111     c     *LIKE         DEFINE    tabkey        savkey
039600110111     C     *LIKE         DEFINE    tabcod        NewCOD
039700110111     C     *LIKE         DEFINE    tabkey        savkey
039800110111      *
039900960605      * Inizializzazione variabili
040000960605     C                   MOVEL     NOMPGM        V2CPGM
040100960605     C                   MOVEL     'N'           WFINE             1
040200960605     C*
040300960605     C                   ENDSR
040400100618     C/EJECT
040500100618      * ?-------------------------------------------------------------*?
040600100618      *? Riordina comunque il SFL                                     ?
040700100618      * ?-------------------------------------------------------------*?
040800100618     C     Ordina_SFL2   BEGSR
040900100618     C*
041000100618     C*  Totale Records Caricati nel 2°SFL
041100100618     c                   eval      wsfl = 1
041200100618     C*
041300100618     C*  se richiesto Ordinamento sul SFL da operatore --> F08 e F09
041400100618     C*
041500100618      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
041600100618      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
041700100618      * Il campo è usato per selezionare i records dando un ordine a quelli
041800100618      * selezionati davanti ai non selezionati.
041900100618     C                   CLEAR                   QLGSCB
042000100618     C                   CLEAR                   QLGSCB00
042100100618      *
042200100618     c                   select
042300100618      *
042400100618      *?  Ordinamento per Codice             ?
042500110421     C                   when      WrkSort = Ord_UNB     or
042600110421     C                             WrkSort = Ord_UNBDisc
042700100618
042800110421     ** 1 campi chiave.   x UNB
042900100618     C                   EVAL      QLGNBRK    = 1
043000100618
043100100618     ** Il GIRO è in posizione (1) 10 Bytes char ascending
043200100618     C                   EVAL      QLGSP      = 1
043300100618     C                   EVAL      QLGSS      = %SIZE(V2CKEY)
043400100618     C                   EVAL      QLGDT      = Carattere
043500100618     C                   EVAL      QLGSO      = Ascendente
043600100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
043700110421     C                             WrkSort = Ord_UNBDisc
043800100618     C                   EVAL      QLGSO      = Discendente
043900100618     c                   end
044000100618     C                   EVAL      QLGKL(1)   = QLGSKL
044100100618      *
044200100618      *?  Ordinamento per Descrizione     ?
044300100618     C                   when      WrkSort = Ord_DEscrizione  or
044400100618     C                             WrkSort = Ord_DEscrizioneDisc
044500100618      *
044600100618     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
044700100618     C                   EVAL      QLGNBRK    = 1
044800100618
044900100618     ** La Priorità se richiesta ricerca di stringa
045000100618     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)
045100100618     C                   EVAL      QLGSS    = %SIZE(V2DDES)
045200100618     C                   EVAL      QLGDT      = Carattere
045300100618     C                   EVAL      QLGSO      = Ascendente
045400100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
045500110421     C                             WrkSort = Ord_UNBDisc
045600100618     C                   EVAL      QLGSO      = Discendente
045700100618     c                   end
045800100618     C                   EVAL      QLGKL(1) = QLGSKL
045900100930      *
046000100930      *?  Ordinamento per Cliente         ?
046100110422     C                   when      WrkSort = Ord_Cliente or
046200110422     C                             WrkSort = Ord_ClienteDisc
046300100930      *
046400100930     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
046500100930     C                   EVAL      QLGNBRK    = 1
046600100930
046700100930     ** La Priorità se richiesta ricerca di stringa
046800100930     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)+ %SIZE(V2DDES)
046900100930     C                   EVAL      QLGSS    = %SIZE(V2CANN)
047000100930     C                   EVAL      QLGDT      = Carattere
047100100930     C                   EVAL      QLGSO      = Ascendente
047200110422     C                   if        WrkSort = Ord_ClienteDisc
047300110422     C                   EVAL      QLGSO      = Discendente
047400110422     c                   end
047500100930     C                   EVAL      QLGKL(1) = QLGSKL
047600100618      *
047700100618     c                   endSL
047800100618      *------------
047900100618      * Load other sort parameters.
048000100618     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
048100100618     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
048200100618     C                   EVAL      QLGRT     = 8
048300100618     C                   EVAL      QLGOKL    = 80
048400100618     C                   EVAL      QLGLKE    = 16
048500100618     C                   EVAL      QLGLSS    = 290
048600100618
048700100618      * Initialize Sort I/O API fields.
048800100618     C                   EVAL      QLGRL00  = QLGRL
048900100618     C                   EVAL      QLGRC00  = 1
049000100618     C                   CLEAR                   QUSEI
049100100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
049200100618
049300100618      * First step - Initialize the sort routine.
049400100618     C                   CALL      'QLGSORT'
049500100618     C                   PARM                    QLGSCB
049600100618     C                   PARM                    NotUsed
049700100618     C                   PARM                    NotUsed
049800100618     C                   PARM                    SizeList
049900100618     C                   PARM                    ReturnSize
050000100618     C                   PARM                    QUSEC
050100100618
050200100618      * Next step - Write records to I/O routine.
050300100618     C                   EVAL      QLGRT00 = Put
050400100618
050500100618     C                   DO        RrnLast       S2nrr_xx          5 0
050600110421     C     S2nrr_xx      CHAIN     TCB9S02
050700100618
050800100618     ** Solo le righe con Selected = 'Y' sono riordinate,
050900100618     ** quindi per fare un ordinamento di tutte le righe
051000100618     ** metto 'Y' sempre.
051100100618     C                   EVAL      Selected  = 'Y'
051200100618
051300100618     C                   CLEAR                   QUSEI
051400100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051500100618      *
051600100618     C                   CALL      'QLGSRTIO'
051700100618     C                   PARM                    QLGSCB00
051800100618     C                   PARM                    SflRcd
051900100618     C                   PARM                    NotUsed
052000100618     C                   PARM                    SizeList
052100100618     C                   PARM                    NotUsed
052200100618     C                   PARM                    QUSEC
052300100618
052400100618     C                   ENDDO
052500100618
052600100618      * Next step - Signal end of input, clear subfile for reload.
052700100618     C                   EVAL      QLGRT00 = EndPut
052800100618     C                   CLEAR                   QUSEI
052900100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053000100618
053100100618     C                   CALL      'QLGSRTIO'
053200100618     C                   PARM                    QLGSCB00
053300100618     C                   PARM                    SflRcd
053400100618     C                   PARM                    NotUsed
053500100618     C                   PARM                    SizeList
053600100618     C                   PARM                    NotUsed
053700100618     C                   PARM                    QUSEC
053800100618      *
  pulizia SFL 2  ?
053900100618     C                   SETOFF                                         2021
054000110421     C                   WRITE     TCB9C02
054100100618     C                   SETON                                          2120
054200100618
054300100618      * Final step - Write the records back to the subfile.
054400100618     C                   EVAL      QLGRT00 = Get
054500100618
054600100618     C                   DO        RrnLast       S2nrr_xx
054700100618     c                   eval      nrr2  = S2nrr_xx
054800100618     C                   CLEAR                   QUSEI
054900100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
055000100618     C                   CALL      'QLGSRTIO'
055100100618     C                   PARM                    QLGSCB00
055200100618     C                   PARM                    NotUsed
055300100618     C                   PARM                    SflRcd
055400100618     C                   PARM                    QLGRL00
055500100618     C                   PARM                    NotUsed
055600100618     C                   PARM                    QUSEC
055700110421     C                   WRITE     TCB9S02
055800100618     C                   ENDDO
055900100618      *
056000100618      *  All'uscita di queste chiamate a routine di sistema il SFL record
056100100618      *   si incrementa stranamente quindi lo reimposto correttamente
056200100618      *    prima di lasciare la routine.
056300100618     C                   EVAL      nrr2 = RrnLast
056400100618     C*
056500100618     C                   ENDSR
056600100618     C/EJECT
056700100618      * ?-------------------------------------------------------------*?
