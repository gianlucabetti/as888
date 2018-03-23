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
001600110714     D DSPU          E DS                  EXTNAME(EDIDSPU)
001700110111     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
001800040216     D DSCE          E DS                  EXTNAME(EDIDSCE)
001900040216     D DSEA          E DS                  EXTNAME(EDIDSEA)
002000040216     D DSEM          E DS                  EXTNAME(EDIDSEM)
002100050223     D DSCL          E DS                  EXTNAME(EDIDSCL)
002200040216     D CNCR80        E DS
002300000306     D UT§DSE0F      E DS
002400040216     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
002500040216     D  KCU                  698    847P 0 DIM(50)   PACKEVEN                   PARAM CAPI CONTO
002600040216      *---------------------------------------------------------------*
002700040216     D TCUDS           DS
002800040216     D  F34                    3      4
002900040216     D  F56                    5      6
003000960605     D                SDS
003100960605     D  NOMPGM                 1     10
003200110422     D Vis_Dati        S              1s 0 inz(1)
003300100618      **********************************************************************
003400100618      * Constants
003500100618      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
003600100618      **********************************************************************
003700110421$xxx D Ord_UNB...
003800100618$xxx D                 C                   1
003900100618$xxx D Ord_Descrizione...
004000100618$xxx D                 C                   2
004100110421$xxx D Ord_UNBDisc...
004200100618$xxx D                 C                   3
004300100618$xxx D Ord_DescrizioneDisc...
004400100618$xxx D                 C                   4
004500100930$xxx D Ord_Cliente...
004600100930$xxx D                 C                   5
004700110421$xxx D Ord_ClienteDisc...
004800110421$xxx D                 C                   6
004900110715$xxx D Ord_PriPartner...
005000110715$xxx D                 C                   7
005100110715$xxx D Ord_DopPartner...
005200110715$xxx D                 C                   8
005300110421      **********************************************************************
005400100618      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
005500100618     D MaxKey          C                   9
005600100618     D Ascendente      C                   1
005700100618     D Discendente     C                   2
005800100618     D Carattere       C                   6
005900100618     D Put             C                   1
006000100618     D EndPut          C                   2
006100100618     D Get             C                   3
006200100618     D Numerico        C                   8
006300110421      **********************************************************************
006400110422     D UpBolle         C                   'Mod.scambio Bolle UPL'
006500110422     D DwBolle         C                   '                  DWN'
006600110422     D UpStati         C                   '                  UPL'
006700110715     D DwStati         C                   '            Stati DWN'
006800110422     D UpFile1         C                   'File UPL     '
006900110422     D UpFile2         C                   '     UPL     '
007000110422     D DwFile1         C                   '     DWN     '
007100110422     D DwFile2         C                   '     DWN     '
007200110422     D UpTrsm1         C                   'Trsm UPL     '
007300110422     D UpTrsm2         C                   '     UPL     '
007400110422     D DwTrsm1         C                   '     DWN     '
007500110422     D DwTrsm2         C                   '     DWN     '
007600100618      **********************************************************************
007700100618      * Campi utili:
007800100618      *     SizeList   - Dimensione della lista
007900100618      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
008000100618      **********************************************************************
008100100618     D NotUsed         S             16A
008200100618     D ReturnSize      S              9B 0
008300100618     D SizeList        S              9B 0
008400100618     D RrnLast         S              5I 0
008500100618     D WrkSort         S              1  0 inz(0)
008600100618     D WindSort        S              3I 0 inz(0)
008700100618      **********************************************************************
008800100618      * Data Structures
008900100618      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
009000100618      *     QLGSCB     - The sort request block for the QLGSORT API
009100100618      *     QLGSCB00   - The sort request block for the QLGSRTIO API
009200100618      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
009300100618      *     QUSEC      - Error structure for the QLGSORT API
009400100618      **********************************************************************
009500100618     D SflRcd          DS
009600100618     D  V2CKEY
009700100618     D  V2DDES
009800100618     D  V2CANN
009900110715     D  H2ptn
010000100618     D  V2CSCE
010100100618     D  V2DKEY
010200110421     D  V2UPLD
010300110421     D  V2UPLS
010400110421     D  V2DWLD
010500110421     D  V2DWLS
010600110422     D  V2tes1
010700110422     D  V2tes2
010800110422     D  V2tes3
010900110422     D  V2tes4
011000100618     D  Selected                      1A
011100100618
011200100618      /COPY QSYSINC/QRPGLESRC,QLGSORT
011300100618     D QLGKL                         16    DIM(MaxKey)
011400100618     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
011500100618     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
011600100618     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
011700100618     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
011800100618
011900100618      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
012000100618      /COPY QSYSINC/QRPGLESRC,QUSEC
012100100618      *---------------------------------------------------------------*
012200000306      *  CICLO PRINCIPALE
012300000306      *---------------------------------------------------------------*
012400960605      * Inizializzo videate
012500960605     C                   EXSR      CARSF2                                       le key del cod.
012600100618      *
012700960605      * Loop gestione videate
012800960605     C     WFINE         DOWEQ     'N'
012900110421     C                   exsr      GESS02                                      Sbf.scelta key.
013000960605     C                   END
013100960605      * Fine Lavoro
013200960605     C     FINE          TAG
013300960605     C                   SETON                                        LR
013400000306      *---------------------------------------------------------------*
013500000306      *  CARSF2: Caricamento subfile delle chiavi
013600000306      *---------------------------------------------------------------*
013700960605     C     CARSF2        BEGSR
013800960605      *
013900960605      *  Pulisco dati SFL
014000960605     C                   SETOFF                                       2021
014100110421     C                   WRITE     TCB9C02
014200110715     C                   SETON                                        202108
014300110421     C                   SETOFF                                       071011
014400100618      *
014500960605      *  Eseguo caricamento prima pagina
014600110421     C                   MOVEL     'PT'          KCOD
014700960605     C                   MOVEL     *LOVAL        WINKEY
014800960612     C                   Z-ADD     1             SFNR2
014900100618      *
015000100618     c     avanti        tag
015100110421      *
015200960605     C                   EXSR      ROLL02
015300110421      *
015400100618     c  n23              goto      avanti
015500100618      *
015600960605      *  Se non ho dati da caricare emetto videata vuota
015700960605     C                   EXSR      INZ02
015800110421      *
015900110421     C*  reset degli ordinamenti
016000110421     C                   SETOFF                                       071011
016100100618     C*
016200960605     C                   ENDSR
016300960605     C*---------------------------------------------------------------*
016400960605     C*  GESS02: Gestione singola tabella
016500960605     C*---------------------------------------------------------------*
016600960605     C     GESS02        BEGSR
016700960605     C*
016800110421     C                   WRITE     TCB9T02
016900110421     C                   EXFMT     TCB9C02
017000110422     C*-------
017100960612     C                   Z-ADD     1             SFNR2
017200110421     C*
017300100618      *  ORDINAMENTO ascendente
017400110421     c   kJ              if        *in10
017500110421     c                   setoff                                       1011
017600110421     C                   eval      WrkSort = Ord_UNB
017700110421     c                   exsr      Ordina_SFL2
017800110421     c                   else
017900110421     c                   seton                                        1011
018000110421     C                   eval      WrkSort = Ord_UNBDisc
018100100618     c                   exsr      Ordina_SFL2
018200100618     c                   end
018300110421     C*
018400110715      *  ORDINAMENTO prima i Partner
018500110715     c   kH              if        *in08
018600110715     c                   setoff                                       08
018700110715     C                   eval      WrkSort = Ord_PriPartner
018800110715     c                   exsr      Ordina_SFL2
018900110715     c                   else
019000110715     c                   seton                                        08
019100110715     C                   eval      WrkSort = Ord_DopPartner
019200110715     c                   exsr      Ordina_SFL2
019300110715     c                   end
019400110715     C*
019500100618      *  ORDINAMENTO Discendente
019600110421     c   kK              if        *in11
019700110421     c                   setoff                                       1110
019800110421     C                   eval      WrkSort = Ord_Descrizione
019900100618     c                   exsr      Ordina_SFL2
020000100618     c                   else
020100110421     c                   seton                                        1110
020200110421     C                   eval      WrkSort = Ord_DescrizioneDisc
020300100618     c                   exsr      Ordina_SFL2
020400100618     c                   end
020500110421     C*
020600110421      *  ORDINAMENTO x c.cliente
020700110422     c   kG              if        *in07
020800110421     c                   setoff                                       1110
020900110421     c                   setoff                                       07
021000110421     C                   eval      WrkSort = Ord_Cliente
021100110421     c                   exsr      Ordina_SFL2
021200110421     c                   else
021300110421     c                   setoff                                       1110
021400110421     c                   seton                                        07
021500110421     C                   eval      WrkSort = Ord_ClienteDisc
021600110421     c                   exsr      Ordina_SFL2
021700110421     c                   end
021800960605     C* Ritorno
021900110321     c                   if        *inKL
022000110321     c                               or
022100110421     c                             *inKC
022200960605     C                   MOVEL     '1'           WFINE
022300960606     C                   MOVEL     '12'          D91CMD
022400110421     C                   MOVEL     TRTC91DS      KPJBU
022500960605     C                   GOTO      FINSF2
022600960605     C                   END
022700110421     C*
022800960605     C* ROLLUP
022900110422     C     *IN25         IFEQ      '1'
023000110422     C*******            EXSR      ROLL02
023100110422     C                   Z-ADD     NRR2          SFNR2
023200110422     C                   GOTO      FINSF2
023300110422     c                   END
023400110421     C*
023500960605     C* Seleziono codice
023600110421     C                   READC     TCB9S02                                30
023700110421     C*
023800960606     C     *IN30         IFEQ      '0'
023900960606     C     V2CSCE        ANDEQ     '1'
024000960605     C                   MOVEL     V2CKEY        D91KEY
024100960605     C                   MOVEL     V2DKEY        D91UNI
024200000306     C                   MOVEL     TRTC91DS      KPJBU
024300960605     C                   MOVEL     'S'           WFINE
024400960605     C                   END
024500960605     C*
024600960605     C     FINSF2        ENDSR
024700960605     C*---------------------------------------------------------------*
024800960605     C*  GESD02: Gestione tabelle inesistente
024900960605     C*---------------------------------------------------------------*
025000960605     C     GESD02        BEGSR
025100960605     C*
025200110421     C                   EXFMT     TCB9D02
025300960605     C* Ritorno
025400960605     C     *INKL         IFEQ      '1'
025500960605     C                   MOVEL     '1'           WFINE
025600960606     C                   MOVEL     '12'          D91CMD
025700110421     C                   MOVEL     TRTC91DS      KPJBU
025800960605     C                   END
025900960605     C*
026000960605     C                   ENDSR
026100960605     C*---------------------------------------------------------------*
026200960605     C*  INZ02: Inizializzazione dati ricerca singola tabella
026300960605     C*---------------------------------------------------------------*
026400960605     C     INZ02         BEGSR
026500100930     C*
026600100930     C                   EVAL      V2TANN ='C.CLI.'
026700110715     C                   EVAL      V2TDE1 ='                    Traduttore'
026800110715     C                   EVAL      V2TDES ='  DATI              NON STD->*'
026900960605     C*
027000960605     C                   ENDSR
027100960605     C*---------------------------------------------------------------*
027200960605     C*  ROLL02: Caricamento una pagina sufile chiavi singola tabella
027300960605     C*---------------------------------------------------------------*
027400960605     C     ROLL02        BEGSR
027500960605     C*
027600960606     C                   Z-ADD     0             WNRIG2            2 0
027700960605     C                   MOVEL     WINKEY        KKEY
027800960612     C                   MOVEL     D91COD        KCOD
027900000306      *
028000110422     C     KTAB          SETGT     EDTAB01L
028100960605     C     KCOD          READE     EDTAB01L                               30
028200000306      *
028300110422     C                   DOW        *IN30 = *off  and  Wnrig2 < 10
028400960606     C                   ADD       1             NRR2              4 0
028500960606     C                   ADD       1             WNRIG2
028600960605     C                   MOVEL     TABKEY        V2CKEY
028700960605     C                   MOVEL     TABUNI        V2DKEY
028800000306     C                   If         TABFLG = *blanks
028900000306     C                   clear                   V2CANN
029000100929      * viene visualizzato il codice del cliente/Partner (PT)
029100100929     C                   eval      V2CANN = %subst(TABuni:1:7)
029200000306     C                   Else
029300000306     c                   eval      V2Cann = 'ANNULLATO'
029400000306     C                   Endif
029500000306      *
029600040216     c                   clear                   v2ddes
029700040216     C                   MOVEL     TABUNI        dspt
029800040216     C                   Z-ADD     1             KKUT
029900040216     C                   Z-ADD     KCI           KKCC
030000040216     C                   Z-ADD     §PTKSC        KKSC
030100040216     C     KACO          CHAIN     CNACO00F                           30
030200040216     C  N30              MOVEL     ACORAG        V2DDES
030300110111      *
030400110111     c                   move      tabcod        savcod
030500110111     c                   move      tabkey        savkey
030600110111      *
030700110111     c                   move      'PZ'          NewCOD
030800110111     C     NTAB          CHAIN     EDTAB01L
030900110422      *
031000110111     c                   if        %Found(EDTAB01L)
031100110111     c                   eval      dspz = tabUNI
031200110422      *
031300110422     c                   if        §PZPGMRCZB = 'INTESA'  or
031400110422     c                             §PZPGMRCZB = 'TRADUT'
031500110421     c                   move      '|*'          v2dDES
031600110214     c                   else
031700110214     c                   move      '| '          v2dDES
031800110214     c                   end
031900110422      *
032000110422     c                   if         vis_dati = 1
032100110421     c                   move(p)   UpBolle       v2tes1
032200110421     c                   move(p)   DwBolle       v2tes2
032300110422     c                   move(p)   UpStati       v2tes4
032400110422     c                   move(p)   DwStati       v2tes3
032500110422     c                   movel(p)  §PZFILRCZB    v2uplD
032600110422     c                   movel(p)  §PZFILRCZS    v2uplS
032700110422     c                   movel(p)  §PZFILTRSB    v2dwlD
032800110422     c                   movel(p)  §PZFILTRSS    v2dwlS
032900110422     c                   elseIf     vis_dati = 2
033000110422     c                   move(p)   UpFile1       v2tes1
033100110422     c                   move(p)   DwFile1       v2tes2
033200110422     c                   move(p)   UpFile2       v2tes4
033300110422     c                   move(p)   DwFile2       v2tes3
033400110422     c                   movel(p)  §PZPGMRCZB    v2uplD
033500110422     c                   movel(p)  §PZPGMRCZS    v2uplS
033600110422     c                   movel(p)  §PZPGMTRSB    v2dwlD
033700110422     c                   movel(p)  §PZPGMTRSS    v2dwlS
033800110422     c                   elseIf     vis_dati = 3
033900110422     c                   move(p)   UpTrsm1       v2tes1
034000110422     c                   move(p)   DwTrsm1       v2tes2
034100110422     c                   move(p)   UpTrsm2       v2tes4
034200110422     c                   move(p)   DwTrsm2       v2tes3
034300110422     c                   movel(p)  §PZTIPRCZB    v2uplD
034400110422     c                   movel(p)  §PZTIPRCZS    v2uplS
034500110422     c                   movel(p)  §PZTIPTRSB    v2dwlD
034600110422     c                   movel(p)  §PZTIPTRSS    v2dwlS
034700110422     c                   end
034800110422      *
034900110111     c                   end
035000110714      *
035100110714     c                   move      'PU'          NewCOD
035200110714     C     NTAB          CHAIN     EDTAB01L
035300110714      *
035400110714     c                   if        %Found(EDTAB01L)
035500110714     c                   eval      dspu = tabUNI
035600110714     c                   eval      h2ptn = §puTPC
035700110714     c                   end
035800110714     c     'P'           comp      H2ptn                                  19
035900110715     C   19              eval      %subst(V2CANN:9:1) = H2ptn
036000110422      *
036100110111     C     STAB          SETGT     EDTAB01L                           30
036200110421     C                   WRITE     TCB9S02
036300000306      *
036400110422     C                   If        Wnrig2 < 10
036500960606     C     KCOD          READE     EDTAB01L                               30
036600000306     c                   Endif
036700000306      *
036800000306     C                   ENDDO
036900000306      *
037000960606     C                   MOVEL     TABKEY        WINKEY
037100100618      *
037200100618     C*  Se sono a fine file accendo 23
037300960606     C     *IN30         COMP      '1'                                    23
037400960605     C*
037500100618     C                   EVAL      RrnLast = nrr2
037600100618     C*
037700960605     C                   ENDSR
037800960605     C*---------------------------------------------------------------*
037900960605     C*  *INZSR: Operazioni iniziali                                  *
038000960605     C*---------------------------------------------------------------*
038100960605     C     *INZSR        BEGSR
038200960605     C*
038300960605     C     *ENTRY        PLIST
038400960605     C                   PARM                    KPJBA
038500000306     C                   MOVEL     KPJBU         TRTC91DS
038600110422     c                   movel     'PT'          D91COD
038700960605      *  Call a XPARUT
038800960605     C                   Z-ADD     1             CODUT
038900960605     C                   CALL      'X§PARUT'
039000000306     C                   PARM                    UT§DSE0F
039100960605     C                   MOVEL     REC80         CNCR80
039200110421     C                   MOVEL     RAGUT         RSUT
039300040216     C*--- RICERCA CAPOCONTI------------------------------------------*
039400040216     C                   DO        50            X                 3 0
039500040216     C                   MOVE      TCU(X)        TCUDS
039600040216     C     F56           IFEQ      'CG'
039700040216     C     F34           ANDEQ     '01'
039800040216     C                   Z-ADD     KCU(X)        KCI               4 0
039900040216     C                   END
040000040216     C                   END
040100960605      * Definizione chiavi
040200960605     C     KTAB          KLIST
040300960605     C                   KFLD                    KCOD
040400960605     C                   KFLD                    KKEY
040500110111     C     NTAB          KLIST
040600110111     C                   KFLD                    NewCOD
040700110111     C                   KFLD                    savkey
040800110111     C     STAB          KLIST
040900110111     C                   KFLD                    SavCOD
041000110111     C                   KFLD                    SavKEY
041100040216     C     KACO          KLIST
041200040216     C                   KFLD                    KKUT
041300040216     C                   KFLD                    KKCC
041400040216     C                   KFLD                    KKSC
041500960605      * Definizione varibili
041600040216     C     *LIKE         DEFINE    ACOKUT        KKUT
041700040216     C     *LIKE         DEFINE    ACOKCC        KKCC
041800040216     C     *LIKE         DEFINE    ACOKSC        KKSC
041900960605     C     *LIKE         DEFINE    TABCOD        KCOD
042000960605     C     *LIKE         DEFINE    TABKEY        KKEY
042100960606     C     *LIKE         DEFINE    TABKEY        WINKEY
042200110111     c     *LIKE         DEFINE    tabcod        savcod
042300110111     c     *LIKE         DEFINE    tabkey        savkey
042400110111     C     *LIKE         DEFINE    tabcod        NewCOD
042500110111     C     *LIKE         DEFINE    tabkey        savkey
042600110111      *
042700960605      * Inizializzazione variabili
042800960605     C                   MOVEL     NOMPGM        V2CPGM
042900960605     C                   MOVEL     'N'           WFINE             1
043000960605     C*
043100960605     C                   ENDSR
043200100618     C/EJECT
043300100618      * ?-------------------------------------------------------------*?
043400100618      *? Riordina comunque il SFL                                     ?
043500100618      * ?-------------------------------------------------------------*?
043600100618     C     Ordina_SFL2   BEGSR
043700100618     C*
043800100618     C*  Totale Records Caricati nel 2°SFL
043900100618     c                   eval      wsfl = 1
044000100618     C*
044100100618     C*  se richiesto Ordinamento sul SFL da operatore --> F08 e F09
044200100618     C*
044300100618      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
044400100618      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
044500100618      * Il campo è usato per selezionare i records dando un ordine a quelli
044600100618      * selezionati davanti ai non selezionati.
044700100618     C                   CLEAR                   QLGSCB
044800100618     C                   CLEAR                   QLGSCB00
044900100618      *
045000100618     c                   select
045100100618      *
045200100618      *?  Ordinamento per Codice             ?
045300110421     C                   when      WrkSort = Ord_UNB     or
045400110421     C                             WrkSort = Ord_UNBDisc
045500100618
045600110421     ** 1 campi chiave.   x UNB
045700110715     C                   EVAL      QLGNBRK    = 1
045800110715     C                   EVAL      QLGSP      = 1
045900100618     C                   EVAL      QLGSS      = %SIZE(V2CKEY)
046000100618     C                   EVAL      QLGDT      = Carattere
046100100618     C                   EVAL      QLGSO      = Ascendente
046200100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
046300110421     C                             WrkSort = Ord_UNBDisc
046400100618     C                   EVAL      QLGSO      = Discendente
046500100618     c                   end
046600100618     C                   EVAL      QLGKL(1)   = QLGSKL
046700100618      *
046800100618      *?  Ordinamento per Descrizione     ?
046900100618     C                   when      WrkSort = Ord_DEscrizione  or
047000100618     C                             WrkSort = Ord_DEscrizioneDisc
047100100618      *
047200100618     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
047300100618     C                   EVAL      QLGNBRK    = 1
047400110715     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)
047500100618     C                   EVAL      QLGSS    = %SIZE(V2DDES)
047600100618     C                   EVAL      QLGDT      = Carattere
047700100618     C                   EVAL      QLGSO      = Ascendente
047800100618     C                   if        WrkSort = Ord_DEscrizioneDisc or
047900110421     C                             WrkSort = Ord_UNBDisc
048000100618     C                   EVAL      QLGSO      = Discendente
048100100618     c                   end
048200100618     C                   EVAL      QLGKL(1) = QLGSKL
048300100930      *
048400100930      *?  Ordinamento per Cliente         ?
048500110422     C                   when      WrkSort = Ord_Cliente or
048600110422     C                             WrkSort = Ord_ClienteDisc
048700100930      *
048800100930     ** 1 campo chiave.   x SUBSTring e DESTINATARIO
048900100930     C                   EVAL      QLGNBRK    = 1
049000100930     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)+ %SIZE(V2DDES)
049100100930     C                   EVAL      QLGSS    = %SIZE(V2CANN)
049200100930     C                   EVAL      QLGDT      = Carattere
049300100930     C                   EVAL      QLGSO      = Ascendente
049400110422     C                   if        WrkSort = Ord_ClienteDisc
049500110422     C                   EVAL      QLGSO      = Discendente
049600110422     c                   end
049700100930     C                   EVAL      QLGKL(1) = QLGSKL
049800110715      *
049900110715      *?  Ordinamento prima i Partner        ?
050000110715     C                   when      WrkSort = Ord_PriPartner or
050100110715     C                             WrkSort = Ord_DopPartner
050200110715
050300110715     ** 2 campi chiave.   x UNB
050400110715     C                   EVAL      QLGNBRK    = 2
050500110715     C                   EVAL      QLGSP   = 1 + %SIZE(V2CKEY)+ %SIZE(V2DDES)
050600110715     C                                         + %SIZE(V2CANN)
050700110715     C                   EVAL      QLGSS      = %SIZE(h2PTN)
050800110715     C                   EVAL      QLGDT      = Carattere
050900110715     C                   if        WrkSort = Ord_PriPartner
051000110715     C                   EVAL      QLGSO      = Discendente
051100110715     c                   else
051200110715     C                   EVAL      QLGSO      = Ascendente
051300110715     c                   end
051400110715     C                   EVAL      QLGKL(1)   = QLGSKL
051500110715     ** 2 campo chiave.   x UNB
051600110715     C                   EVAL      QLGSP      = 1
051700110715     C                   EVAL      QLGSS      = %SIZE(V2CKEY)
051800110715     C                   EVAL      QLGDT      = Carattere
051900110715     C                   EVAL      QLGSO      = Ascendente
052000110715     C                   EVAL      QLGKL(2)   = QLGSKL
052100100618      *
052200100618     c                   endSL
052300100618      *------------
052400100618      * Load other sort parameters.
052500100618     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
052600100618     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
052700100618     C                   EVAL      QLGRT     = 8
052800100618     C                   EVAL      QLGOKL    = 80
052900100618     C                   EVAL      QLGLKE    = 16
053000100618     C                   EVAL      QLGLSS    = 290
053100100618
053200100618      * Initialize Sort I/O API fields.
053300100618     C                   EVAL      QLGRL00  = QLGRL
053400100618     C                   EVAL      QLGRC00  = 1
053500100618     C                   CLEAR                   QUSEI
053600100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053700100618
053800100618      * First step - Initialize the sort routine.
053900100618     C                   CALL      'QLGSORT'
054000100618     C                   PARM                    QLGSCB
054100100618     C                   PARM                    NotUsed
054200100618     C                   PARM                    NotUsed
054300100618     C                   PARM                    SizeList
054400100618     C                   PARM                    ReturnSize
054500100618     C                   PARM                    QUSEC
054600100618
054700100618      * Next step - Write records to I/O routine.
054800100618     C                   EVAL      QLGRT00 = Put
054900100618
055000100618     C                   DO        RrnLast       S2nrr_xx          5 0
055100110421     C     S2nrr_xx      CHAIN     TCB9S02
055200100618
055300100618     ** Solo le righe con Selected = 'Y' sono riordinate,
055400100618     ** quindi per fare un ordinamento di tutte le righe
055500100618     ** metto 'Y' sempre.
055600100618     C                   EVAL      Selected  = 'Y'
055700100618
055800100618     C                   CLEAR                   QUSEI
055900100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
056000100618      *
056100100618     C                   CALL      'QLGSRTIO'
056200100618     C                   PARM                    QLGSCB00
056300100618     C                   PARM                    SflRcd
056400100618     C                   PARM                    NotUsed
056500100618     C                   PARM                    SizeList
056600100618     C                   PARM                    NotUsed
056700100618     C                   PARM                    QUSEC
056800100618
056900100618     C                   ENDDO
057000100618
057100100618      * Next step - Signal end of input, clear subfile for reload.
057200100618     C                   EVAL      QLGRT00 = EndPut
057300100618     C                   CLEAR                   QUSEI
057400100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
057500100618
057600100618     C                   CALL      'QLGSRTIO'
057700100618     C                   PARM                    QLGSCB00
057800100618     C                   PARM                    SflRcd
057900100618     C                   PARM                    NotUsed
058000100618     C                   PARM                    SizeList
058100100618     C                   PARM                    NotUsed
058200100618     C                   PARM                    QUSEC
058300100618      *
  pulizia SFL 2  ?
058400100618     C                   SETOFF                                         2021
058500110421     C                   WRITE     TCB9C02
058600100618     C                   SETON                                          2120
058700100618
058800100618      * Final step - Write the records back to the subfile.
058900100618     C                   EVAL      QLGRT00 = Get
059000100618
059100100618     C                   DO        RrnLast       S2nrr_xx
059200100618     c                   eval      nrr2  = S2nrr_xx
059300100618     C                   CLEAR                   QUSEI
059400100618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
059500100618     C                   CALL      'QLGSRTIO'
059600100618     C                   PARM                    QLGSCB00
059700100618     C                   PARM                    NotUsed
059800100618     C                   PARM                    SflRcd
059900100618     C                   PARM                    QLGRL00
060000100618     C                   PARM                    NotUsed
060100100618     C                   PARM                    QUSEC
060200110714     c     'P'           comp      H2ptn                                  19
060300110421     C                   WRITE     TCB9S02
060400100618     C                   ENDDO
060500100618      *
060600100618      *  All'uscita di queste chiamate a routine di sistema il SFL record
060700100618      *   si incrementa stranamente quindi lo reimposto correttamente
060800100618      *    prima di lasciare la routine.
060900100618     C                   EVAL      nrr2 = RrnLast
061000100618     C*
061100100618     C                   ENDSR
061200100618     C/EJECT
061300100618      * ?-------------------------------------------------------------*?
