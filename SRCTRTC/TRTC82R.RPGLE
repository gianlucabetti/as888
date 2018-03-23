000100000405     H DECEDIT('0,') DATEDIT(*DMY.)
000200000405      * TRTC82R *----------------------------------------------------*
000300000405      *     FILTRO MANIFEST SPEDIZIONI PER  E.D.I. ESTERO
000400000405      *--------------------------------------------------------------*
000500000405     FTRTC82D   CF   E             WORKSTN SFILE(TC82S01:NRR1)
000600000405      *---------
000700150909     FAZCLN01L  IF   E           K DISK
000800941221     FFNFGV01L  IF   E           K DISK
000900000405      *---------
001000020808     FFNFGV06L  IF   E           K DISK    RENAME(FNFGV000:FGV06)
001100020808     F                                     prefix(F6V:3)
001200020808      *---------
001300000505     FFNFGW01L  IF   E           K DISK
001400000405      *---------
001500020808     FFNBLP79L  IF   E           K DISK
001600000405      *---------
001700960529     FEDTAB01L  IF   E           K DISK
001800000405      *---------
001900961112     FTABEL00F  IF   E           K DISK
002000961129     FCNACO00F  IF   E           K DISK
002100961213     FCNIND00F  IF   E           K DISK
002200970806     FFNSPE01L  IF   E           K DISK
002300970108     FAZORG01L  IF   E           K DISK
002400050523     FWFMAN01L  UF   E           K DISK
002500000405      *--------------------------------------------------------------*
002600000405      *  SCHIERE                                                     *
002700000405      *--------------------------------------------------------------*
002800121102     D TRUL0SDS      E DS
002900121102     D*
003000121102     D**** CPU             S             35    DIM(200)                             Cod.partner
003100121102     D**** CPT             S             35    DIM(200)                             Cod.partner
003200121102     D**** DPT             S             90    DIM(200)                             Tab.PT
003300121102     D**** DPU             S             90    DIM(200)                             Tab.PU
003400121102     D**** KSC             S              7  0 DIM(200)                             Cod.cliente
003500121102     D**** LNA             S              3  0 DIM(200)                             Lna
003600121102      **
003700121102     D CPT             S             35    DIM(200)                             Cod.partner
003800121102     D CPU             S             35    DIM(%elem(CPT))                      Cod.partner
003900121102     D DPT             S             90    DIM(%elem(CPT))                      Tab.PT
004000121102     D DPU             S             90    DIM(%elem(CPT))                      Tab.PU
004100121102     D KSC             S              7  0 DIM(%elem(CPT))                      Cod.cliente
004200121102     D LNA             S              3  0 DIM(%elem(CPT))                      Lna
004300121102      **
004400970218     D ERR             S             78    DIM(27) CTDATA PERRCD(1)             Errore
004500970117     D K78             S              1    DIM(78)                              DI COMODO
004600000405      *
004700000505     D WSFFV           S              3    DIM(301)                             LINEE DEL FV
004800010509     D DpDFFV          S              3    DIM(301)                             LINEE DEL FV
004900150909     d
005000150909     D lindef          s              3  0 DIM(30)
005100000405      *--------------------------------------------------------------*
005200000405      *  DS                                                          *
005300000405      *--------------------------------------------------------------*
005400000405      * PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - TRTC83C -
005500960529     D TRTC82        E DS                  EXTNAME(TRTC82DS)
005600000405      *
005700130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
005800130520     D fnlst3ds        DS
005900951108     D  PA5FLG                 1      1    INZ('R')
006000951108     D  PA5FL2                 2      2    INZ
006100951108     D  PA5NFV                 3      5P 0 INZ
006200960201     D  PA5DIN                 6     10P 0 INZ
006300951108     D  PA5DFI                11     15P 0 INZ(31129999)
006400951108     D  PA5TFV                16     16    INZ('2')
006500000614     D  PA5FGS               242    244  0
006600000405      *
006700900511     D KPJBA         E DS
006800120821     D TRUL09DS      E DS
006900120821     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
007000000405     D UT§DSE0F      E DS
007100000405     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
007200000405     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    PARAM CAPI CONTO
007300961129     D TCUDS           DS
007400961129     D  F3                     3      3
007500961129     D  F4                     4      4
007600961129     D  F56                    5      6
007700960529     D CNCR80        E DS
007800000405     D OG143         E DS
007900000405     D EDIDSPT       E DS
008000000405     D EDIDSPU       E DS
008100961112     D DS15          E DS
008200990301     D DSTV          E DS
008300000405     D TRUL42DS      E DS
008400020808     D*
008500041020     D* ds per ind. email
008600041020     Dtrul44ds       E DS                  INZ
008700050913     D trtcm1ds      E DS
008800041020      *
008900960701     D WLBDA8          DS
009000960701     D  G02DAT                 1      8  0
009100960701     D  G02INV                 9     16  0
009200960701     D  G02ERR                17     17
009300960701     D  G02TGI                18     22  0
009400970108     D* LINEE DI ARRIVO  PER FNFGV00F
009500970108     D                 DS
009600970108     D  FGVLNA                 1      3  0
009700970108     D  FGVFFV                 4    243
009800970108     D  FGVFF2               244    450
009900000505     D  FGWFF3               451    690
010000000505     D  FGWFF4               691    900
010100000505     D  VUOTO                901    903
010200000505     D  FFV                    1    903    DIM(301)                             LINEE DEL FV
010300150909     D                 DS
010400150909     D  CLNPOM                 1     31
010500150909     D  POM                    1     31
010600150909     D                                     DIM(31)
010700150909     D                 DS
010800150909     D  CLNMAT                 1     31
010900150909     D  MAT                    1     31
011000150909     D                                     DIM(31)
011100150909      *
011200020808      *
011300020808      * Definizione variabili
011400020808      *
011500020920     D kpjbus          S                   LIKE(kpjbu)
011600020808     D WTIME           S             14  0                                      ORA E DATA
011700020808     D WDATE           S              8  0                                      GG/MM/AAAA
011800020808     D Anno            S              4  0                                      AAAA
011900020808     D DAanno          S              4  0                                      AAAA
012000020808     D ADanno          S              4  0                                      AAAA
012100020808     D DATEU           S              8  0                                      AAAA/MM/GG
012200020808     D Anno40          S              4  0
012300040520     D DataISO         S               d   DATFMT(*iso)
012400040520     D DataISO1        S               d   DATFMT(*iso)
012500040520     D DataOggi        S               d   DATFMT(*iso)
012600040520     D DataCMR         S               d   DATFMT(*iso)
012700150909     d wdtaeur         s               d   datfmt(*eur)
012800150909     d wdtaiso         s               d   datfmt(*iso)
012900040520     D KCI             S              4  0
013000020808     D II              S              3  0 inz
013100020808     D XX              S              3  0 inz
013200020808     D XY              S              3  0 inz
013300020808     D YY              S              3  0 inz
013400020808     D NRR1            S              5  0 inz
013500020808     D WERR            S              1    inz
013600020808     D WTPVID          S              1    inz
013700020808     D WMBR            S              8    inz
013800020808     D WFFV            S              3  0 inz
013900020808     D WFFVA           S              3    inz
014000020808     D WFIRST          S              1    inz
014100020808     D WSIEDI          S              2  0 inz
014200020808     D WFINE           S              1    inz
014300020808     D WDOGSI          S              1    inz
014400020808     D WDOGNO          S              1    inz
014500020808     D W001A           S              1    inz
014600020808     D soloDPD         S              1    inz
014700150909     d wDEFmeno1       s              8  0
014800150909     D KANN            S                   LIKE(CLNANN)
014900150909     D KMES            S                   LIKE(CLNMES)
015000150909     D KTFP            S                   LIKE(FGVLNA)
015100150909     D KTFA            S                   LIKE(FGVLNA)
015200150909     d wgiorno         s              2  0
015300020808     D SAVNR1          S                   LIKE(NRR1)   inz
015400020808     D WSVMSG          S                   LIKE($MSG)   inz
015500020808     D KCOD            S                   LIKE(TABCOD) inz
015600020808     D KNFV            S                   LIKE(FGVNFV) inz
015700020808     D COD             S                   LIKE(TBLCOD) inz
015800020808     D KEY             S                   LIKE(TBLKEY) inz
015900020808     D KSCOD           S                   LIKE(SPECOD) inz('200')
016000020808      * ............................... -> 200 - Luogo invio fax a Dogana
016100020808     D KSFLS           S                   LIKE(SPEFLS) inz('L')
016200020808     D WTRC            S                   LIKE(TRETRC) inz('P')
016300020808     D WGVDFV          S                   LIKE(FGVDFV) inz
016400020808     D WGVNFV          S                   LIKE(FGVNFV) inz
016500020808     D WWVNFV          S                   LIKE(FGVNFV) inz
016600000405      ***************************************************************
016700000405      *  RIEPILOGO INDICATORI                                       *
016800000405      ***************************************************************
016900000405      **04    - ON --> SPROTEGGO FILIALE GESTIONE FOGLIO IMA        *
017000000405      **05    - OFF--> NUMERO/DATA CMR OBBLIGATORI                  *
017100000405      * 07    - ON=VISUALIZZA 'BIS' SE FV BIS                       *
017200000405      * 28    - Indicatore generico di errore                       *
017300000405      * 29    - Indicatore generico PER RIEMETTERE VIDEATA SENZA ERR*
017400000405      * 30    - Esecuzione Chain/read                               *
017500000405      * 31    - Indicatore di TESTN                                 *
017600000405      * 32    - Indicatore di risposta SCAN                         *
017700000405      * 40    - Errore su foglio viaggio                            *
017800000405      * 41    - ERRORE N. CMR                                       *
017900000405      * 42    - ERRORE DATA CMR                                     *
018000000405      * 43    - Errore su codice partner                            *
018100000405      * 44    - Errore su ELABORAZIONE PER DOGANA (SI/NO)           *
018200000405      * 45    - Errore targa motrice                                *
018300000405      * 46    - POSIZIONAMENTO CURSORE DATA CMR SE OBBLIGATORIA     *
018400000405      * 47    - ERRRORE SU MANIFEST PER DOGANA PER LNA              *
018500000405      ***************************************************************
018600000405
018700000405      *-------------------------------------------------------------*
018800000405      *  CICLO PRINCIPALE                                           *
018900000405      *-------------------------------------------------------------*
019000000405      *  Inizializzazione parametri a video
019100960529     C                   EXSR      INZ01
019200000405      *  Loop gestione prima videata
019300960529     C     WFINE         DOWNE     'S'
019400960530     C     WTPVID        CASEQ     '1'           GESD01
019500960530     C     WTPVID        CASEQ     '2'           GESD02
019600960529     C                   END
019700960530     C                   END
019800000405      *
019900960529     C     FINE          TAG
020000000405     C                   eval       *inlr = *on
020100960529     C***************************************************************
020200960529     C*  Inizializzazione prima videata                             *
020300960529     C***************************************************************
020400960529     C     INZ01         BEGSR
020500960529     C*
020600970218     C                   CLEAR                   TRTC82
020700970108     C                   SETOFF                                       402844
020800960529     C                   MOVEL     *BLANKS       $MSG
020900960529     C                   MOVEL     *BLANKS       V1CNFV
021000961111     C                   MOVE      '?'           V1CNFV
021100961111     C                   MOVEL     'NO'          V1CDEF
021200970107     C                   MOVEL     'NO'          V1CDOG
021300960530     C                   MOVEL     '1'           WTPVID
021400020808     c*
021500020808     c* Imposto il P.O. in gestione
021600020808     c                   z-add     SIMpou        V1Cpog
021700020808     c                   clear                   V1Dpog
021800020808     c     SIMpou        chain     AZORG
021900020808    1c                   if             %found(AZORG01L)
022000020808     c                             and  ORGfva = *blanks
022100020808     c                             and (ORGfag = 'A'
022200020808     c                              or  ORGfag = 'F')
022300020808     c                   movel     ORGdes        V1Dpog
022400020808    1c                   endif
022500960529     C*
022600960529     C                   ENDSR
022700960530     C***************************************************************
022800960530     C*  Inizializzazione seconda videata                           *
022900960530     C***************************************************************
023000960530     C     INZ02         BEGSR
023100960530     C*
023200970108     C                   SETOFF                                       414243
023300970115     C                   SETOFF                                       454628
023400970123     C                   SETOFF                                       47
023500960530     C                   MOVEL     *BLANKS       $MSG
023600020808     C                   MOVEL     '2'           WTPVID
023700970108     C*
023800970108     C                   MOVE      V1CNFV        VCCNFV
023900970108     C                   MOVE      V1CDEF        VCCDEF
024000970108     C                   MOVE      V1CDOG        VCCDOG
024100970108     C*
024200970108     C                   Z-ADD     0             VCCDAC
024300970108     C                   MOVEL     *BLANKS       VCCCMR
024400970109     C                   MOVE      FGVTRM        VCCTRM
024500970108     C* Pulizia subfile
024600970108     C                   SETOFF                                       2123
024700970109     C                   WRITE     TC82C01
024800970108     C                   SETON                                        2021
024900020808     C                   MOVE      *ZEROS        NRR1
025000970108     C                   MOVE      *ZEROS        V1CNBR
025100970124     C*
025200970124     C* MANIFEST PER LA DOGANA
025300970124     C     VCCDOG        IFEQ      'SI'
025400970124     C                   MOVE      'SI'          VSCDGN
025500970124     C                   ELSE
025600970124     C                   MOVE      'NO'          VSCDGN
025700970124     C                   END
025800970124     C* MANIFEST SPEDIZIONI C.O.D. E LISTA DISCORDANZE
025900970124     C     VCCDEF        IFEQ      'SI'
026000970124     C                   MOVE      'SI'          VSCCOD
026100970124     C                   MOVE      'SI'          VSCLDS
026200970124     C                   ELSE
026300970124     C                   MOVE      'NO'          VSCCOD
026400970124     C                   MOVE      'NO'          VSCLDS
026500970124     C                   END
026600970108     C*
026700970108     C* CARICAMENTO SUBFILE
026800020808     c     K02FGW01      chain     fnfgw01l                           39
026900000505     c                   if        *in39 or fgwatb<>' '
027000000505     c                   clear                   fgwff3
027100000505     c                   clear                   fgwff4
027200000505     c                   endif
027300970108     C                   CLEAR                   WSIEDI
027400020808     C                   Z-ADD     1             II
027500970108     C     FFV(II)       DOWGT     *ZEROS
027600970110     C                   SETOFF                                       22
027700970108     C                   MOVE      FFV(II)       VSCLNA
027800000405      *
027900000405      * Decodifico linea di arrivo
028000970108     C     VSCLNA        CHAIN     AZORG01L                           32
028100000405      *
028200970108     C  N32              MOVEL     ORGDES        VSDLNA
028300970109     C   32              MOVEL     *ALL'?'       VSDLNA
028400970108     C* Codice partner
028500970108     C                   Z-ADD     0             VSCCPT
028600970108     C*
028700020808     C                   Z-ADD     1             XY
028800020808     C                   MOVE      FFV(II)       WFFV
028900020808     C     WFFV          LOOKUP    LNA(XY)                                32
029000970108     C     *IN32         IFEQ      '1'
029100970108     C                   EXSR      SRCPT
029200970123     C* MEMORIZZO SE C'E' ATTRAVERSAMENTO DOGANA
029300970123     C                   MOVEL     §PTNAZ        VSHNAZ
029400970123     C                   MOVEL     '15'          COD
029500970123     C                   MOVEL(P)  VSHNAZ        KEY
029600970123     C     KTABE         CHAIN     TABEL00F                           30
029700970123     C  N30              MOVEL     TBLUNI        DS15
029800970123     C   30              CLEAR                   DS15
029900010405     C                   MOVE      §15DOG        VSHEFT
030000970110     C*
030100970110     C     VCCDEF        IFEQ      'SI'
030200970110     C     VSHFAP        ANDEQ     *BLANKS
030300970110     C     VSCSTM        IFEQ      'SI'
030400970110     C     VSCCOD        OREQ      'SI'
030500970110     C     VSCLDS        OREQ      'SI'
030600970110     C                   SETON                                        22
030700970110     C                   END
030800970110     C                   END
030900970110     C*
031000970108     C                   END
031100970108     C                   ADD       1             NRR1
031200970108     C                   WRITE     TC82S01
031300970108     C                   ADD       1             II
031400970108     C                   ENDDO
031500970218     C* CMR SEMPRE OBBLIGATORIO
031600020808     C*  Accendo indicatore di posizionamento cursore su Data CMR
031700970115     C                   SETON                                        46
031800970115     C*
031900970109     C                   SETON                                        23
032000970108     C                   Z-ADD     1             V1CNBR
032100020808     C                   MOVE      *BLANKS       WFIRST
032200960530     C*
032300960530     C                   ENDSR
032400961111     C***************************************************************
032500970108     C*  CAMBIATO CODICE PARTNER:REIMPOSTO CAMPI VIDEO              *
032600961111     C***************************************************************
032700961111     C     SRCPT         BEGSR
032800961111     C*
032900020808     C                   Z-ADD     KSC(XY)       VSCCPT
033000970108     C                   Z-ADD     VSCCPT        VSHCPT
033100961129     C* DECODIFICO COD. PARTNER DA P.D.C.
033200961129     C     KACO          CHAIN     CNACO00F                           31
033300970108     C  N31              MOVEL     ACORAG        VSDCPT
033400970108     C   31              CLEAR                   VSDCPT
033500961129     C*
033600020808     C                   MOVEL     DPT(XY)       EDIDSPT
033700020808     C                   MOVEL     DPU(XY)       EDIDSPU
033800970109     C* MEMORIZZO IN CAMPI HIDDEN
033900980320     C                   MOVEL     §PUBS1        VSHTRD
034000120117     C**** La lunghezza della nostra spedizione deve scomparire sostituita solo
034100120117     C*******   dal riferimento presente nell'AGE. La nostra bolla si identifica con
034200120117     C*******   16 o 14 caratteri.
034300120117     C*******   i 2 campi CNI e AGE hanno sempre riportato lo stesso riferimento
034400120117     C*******   quindi verrà eliminato quello inerente al CNI.
034500120117     C*******   Rimane un po' strano il discorso del Secolo.....sul CNI......
034600120117     C                   Z-ADD     §PTAGE        VSHNSP
034700970109     C                   Z-ADD     §PTAGE        VSHAGE
034800970109     C                   MOVEL     §PUNSA        VSHNSA
034900970109     C                   MOVEL     §PUSEC        VSHSEC
035000970109     C                   MOVEL     §PTFTT        VSHFTT
035100970109     C                   MOVEL     §PUSPC        VSHSPC
035200970109     C                   Z-ADD     §PTLNP        VSHLNP
035300970218     C                   MOVEL     §PTCM2        VSHCM2
035400980219     C                   MOVEL     §PUBS1        VSHTSC
035500050525     C                   MOVEL     §PUvma        VSHvma
035600060913     C                   MOVEL     §PUisc        VSHisc
035700970109     C*
035800970108     C* MEMORIZZO SE C'E' TRAMISSIONE DATI IN EDI
035900970108     C     §PTFTT        IFNE      *BLANKS
036000020808     C                   ADD       1             WSIEDI
036100970108     C                   END
036200961111     C*
036300970108     C* MANIFEST PER IL PARTNER
036400970218     C* SE PROVVISORIO                          --> NO
036500970108     C* SE DEFINITIVO E NO E.D.I.               --> SI
036600970205     C* SE DEFINITIVO E SI E.D.I.               --> SI
036700970108     C     VCCDEF        IFEQ      'NO'
036800970108     C                   MOVE      'NO'          VSCSTM
036900961111     C                   ELSE
037000970108     C                   MOVE      'SI'          VSCSTM
037100961112     C                   END
037200961213     C*
037300970108     C* CERCO NUMERO FAX PARTNER SE ELABORAZIONE DEFINITIVA
037400970108     C                   CLEAR                   VSHFAP
037500961213     C*
037600970108    1C     VCCDEF        IFEQ      'SI'
037700961213     C     KACO          CHAIN     CNIND00F                           31
037800041020      **
037900961213    2C     *IN31         IFEQ      *OFF
038000041020      **
038100970218     C* CONTROLLO N. FAX PARTNER SOLO SE DEVO INVIARE QUALCOSA VIA FAX
038200970121     C* AL PARTNER
038300041020      **  (adesso si deve considerare anche l'indirizzo e-mail)
038400970121    3C     VSCSTM        IFEQ      'SI'
038500010201     C     §PTFTT        ANDNE     'W'
038600970121     C     VSCLDS        OREQ      'SI'
038700970121     C     VSCCOD        OREQ      'SI'
038800041020      **
038900000405     C                   CLEAR                   TRUL42DS
039000961213     C                   MOVEL     INDTLF        D42FAX
039100961213     C                   CALL      'TRUL42R'
039200000405     C                   PARM                    TRUL42DS
039300041020      **
039400961213    4C     D42ERR        IFEQ      ' '
039500970108     C                   MOVEL     INDTLF        VSHFAP
039600961213    4C                   END
039700041020      **
039800041020      **  Attenzione: l'indirizzo E-mail ha priorità sul nr.FAX
039900041020      **       se c'è prende quello e non segnala l'errore.
040000041020     c                   clear                   trul44ds
040100050913     c                   clear                   trtcm1ds
040200041020      **** cerco solo sulle NOTE poichè è l'e-mail del Partner e non della Dogana
040300041021     c                   eval      d44pgm = 'TRTC82R'
040400041021     c                   move      vsccpt        d44ksc
040500041020     c                   eval      d44apl  = 'C'
040600041118     c                   eval      d44nk1  = '0151' + %editc(VSCCPT:'X')
040700041020     c                   eval      d44tnt  = '86'
040800041020     c                   call      'TRUL44R'
040900041020     c                   parm                    trul44ds
041000050913     c                   parm                    trtcm1ds
041100041020     c                   if        d44err = *blanks
041200041020     C                   MOVEL     'OK'          VSHFAP
041300041020     c                   end
041400041020      **
041500961213   X3C                   ELSE
041600970108     C                   MOVEL     'OK'          VSHFAP
041700961213    3C                   END
041800961216    2C                   END
041900970108    1C                   END
042000961111     C*
042100961111     C                   ENDSR
042200960529     C***************************************************************
042300960529     C*  Gestione prima videata                                     *
042400960529     C***************************************************************
042500960529     C     GESD01        BEGSR
042600970108     C*
042700970108     C*
042800960528     C                   EXFMT     TC82D01
042900961111     C                   SETOFF                                       402829
043000970109     C                   SETOFF                                       4407
043100960529     C** Fine Lavoro
043200960529     C     *INKC         IFEQ      '1'
043300020808     C                   MOVEL     'S'           WFINE
043400960529     C                   GOTO      FINVD1
043500960529     C                   END
043600960529     C* Controlli formato
043700960529     C                   EXSR      CONTR1
043800961111     C   28
043900961111     COR 29              GOTO      FINVD1
044000960530     C* No errori: VID02
044100960530     C                   EXSR      INZ02
044200960529     C*
044300960529     C     FINVD1        ENDSR
044400960530     C***************************************************************
044500960530     C*  Gestione seconda videata                                   *
044600960530     C***************************************************************
044700960530     C     GESD02        BEGSR
044800960530     C*
044900020808     C                   MOVEL     $MSG          WSVMSG
045000970108     C                   WRITE     TC82Z01
045100010509     C                   setoff                                       20
045200010509      * solo se ci sono dei records nel SFL accendo (20)
045300010509     C     NRR1          ifgt      0
045400010509     C                   seton                                        20
045500010509     C                   end
045600010509      * se il SFL è vuoto e nella schiera DPD nel primo elemento c'è una linea
045700010509      * DPD occorre emettere il messaggio di avvertimento e conferma.
045800010509      * e imposto un flag per gestire in segiuto le cose.
045900020808     C                   clear                   soloDPD
046000010509      *  Emissione 2°Video
046100970109     C                   EXFMT     TC82C01
046200010509      *
046300961114     C                   SETOFF                                       434128
046400970109     C                   SETOFF                                       424529
046500970123     C                   SETOFF                                       47
046600960530     C** Fine Lavoro
046700960530     C     *INKC         IFEQ      '1'
046800020808     C                   MOVEL     'S'           WFINE
046900960530     C                   GOTO      FINVD2
047000960530     C                   END
047100960530     C** Ritorno
047200960530     C     *INKL         IFEQ      '1'
047300960530     C                   MOVEL     '1'           WTPVID
047400970108     C                   CLEAR                   WGVDFV
047500970108     C                   CLEAR                   WGVNFV
047600970117     C                   CLEAR                   WWVNFV
047700960530     C                   GOTO      FINVD2
047800960530     C                   END
047900960530     C* Controlli formato
048000960530     C                   EXSR      CONTR2
048100960530     C* Conferma
048200961111     C  N28
048300961111     CANN29*INKF         IFEQ      '1'
048400970127     C*
048500960530     C                   EXSR      SBMP83
048600960530     C                   EXSR      INZ01
048700960530     C                   END
048800960530     C*
048900960530     C     FINVD2        ENDSR
049000960529     C***************************************************************
049100960529     C*  Sottometto elaborazione                                    *
049200960529     C***************************************************************
049300960529     C     SBMP83        BEGSR
049400960529     C*
049500970109     C                   MOVEL     V1CNFV        D82NFV
049600970109     C                   MOVEL     FGVDFV        D82DFV
049700970109     C                   MOVEL     VCCCMR        D82CMR
049800970109     C                   MOVEL     VCCDEF        D82DEF
049900970109     C                   MOVEL     VCCTRM        D82TRM
050000970115     C                   MOVEL     FGVSNP        D82FVB
050100970127     C*
050200010509      * Se non si tratta solo di DPD e il sfl era carico
050300010509     C     SoloDPD       Ifeq      *Blank
050400010509     C     *in20         andeq     *ON
050500010509     C*
050600970127     C* Prima di sottomettere, se elaborazione provvisoria cancello
050700970127     c* eventuali records dal file di work
050800970127     C     D82DEF        IFEQ      'NO'
050900050523     C     KTRE0         SETLL     WFMAN01L
051000050523     C     KTRE0         READE     WFMAN01L                             3231
051100970127     C     *IN31         DOWEQ     *OFF
051200970127     C     *IN32         ANDEQ     *OFF
051300050523     C                   DELETE    TREXP000
051400050523     C     KTRE0         READE     WFMAN01L                               31
051500970127     C                   ENDDO
051600970127     C                   END
051700970109     C*
051800980212     C*
051900970109     C                   Z-ADD     1             V1CNBR
052000970109     C*
052100970109     C     V1CNBR        CHAIN     TC82S01                            30
052200970109     C     *IN30         DOWEQ     *OFF
052300020808     C                   MOVEL     'M'           WMBR
052400970109     C                   MOVE      VSCCPT        WMBR
052500970109     C                   MOVEL     WMBR          D82MBR
052600970109     C                   Z-ADD     VSCCPT        D82CPT
052700970109     C                   MOVEL     VSCSTM        D82STB
052800970109     C                   MOVEL     VSHLNP        D82LNP
052900970109     C                   MOVEL     VSHNAZ        D82NAZ
053000970109     C                   MOVEL     VSHTRD        D82DET
053100970109     C                   Z-ADD     VSHNSP        D82CNI
053200970109     C                   Z-ADD     VSHAGE        D82AGE
053300970109     C                   MOVEL     VSHNSA        D82NSA
053400970109     C                   MOVEL     VSHSEC        D82SEC
053500970109     C                   MOVEL     VSHFTT        D82FTT
053600970109     C                   MOVEL     VSCDGN        D82DGN
053700970109     C                   MOVEL     VSCCOD        D82COD
053800970109     C                   MOVEL     VSHSPC        D82SPC
053900970109     C                   MOVEL     VSCLDS        D82LDS
054000970124     C                   MOVEL     VSHEFT        D82EFT
054100980219     C                   MOVEL     VSHTSC        D82TSC
054200050525     C                   movel     VSHvma        D82vma
054300060913     C                   movel     VSHisc        D82isc
054400050525     c                   clear                   kpjbu
054500960529     C                   MOVEL     TRTC82        KPJBU
054600010509      *
054700961205     C                   MOVEL     'TC83'        KCOAZ
054800041021     c                   if        kcoaz = *blank
054900041021     C                   CALL      'TRTC83C'
055000041021     C                   PARM                    KPJBA
055100041021     c                   else
055200041021     C*
055300900523     C                   CALL      'BCH10'
055400900523     C                   PARM                    KPJBA
055500041021     c                   end
055600970109     C*
055700970109     C                   ADD       1             V1CNBR
055800970109     C     V1CNBR        CHAIN     TC82S01                            30
055900970109     C                   ENDDO
056000900522     C*
056100010509     C                   ELSE
056200010509     C*
056300010509     C* Se si tratta solo di un foglio con linee Dpd devo chiuderlo
056400010509     C*  senza fare tutti i passaggi che il Manifest fa normalmente.
056500010509     C     SoloDPD       Ifeq      'S'
056600010509     C*
056700010509     C* Attenzione utilizza il campo MEMBRO per pilotare il pgm
056800010509     C*  TRTC83R4 in sola chiusura del foglio viaggio.
056900010509     C                   MOVEL     'SOLO_X_DPD'  D82MBR
057000050525     C                   MOVEL     kpjbu         KPJBUs
057100050525     C                   clear                   KPJBU
057200010509     C                   MOVEL     TRTC82        KPJBU
057300010509     C                   CALL      'TRTC83R4'
057400010509     C                   PARM                    KPJBA
057500050525     C                   MOVEL     kpjbuS        KPJBU
057600010509     C*
057700010509     C                   Endif
057800010509     C*
057900010509     C                   END
058000010509     C*
058100010509     C                   ENDSR
058200960529     C***************************************************************
058300960530     C*  Controlli prima videata                                    *
058400960529     C***************************************************************
058500960529     C     CONTR1        BEGSR
058600960529     C*
058700961111     C* CONTROLLI NUMERO FOGLIO VIAGGIO
058800960529     C* Ricerca numero foglio
058900960529     C     '?'           SCAN      V1CNFV                                 32
059000970123    1C     *IN32         IFEQ      '1'
059100130520     C                   RESET                   fnlst3ds
059200000614     C                   Z-ADD     SIMFEL        PA5FGS
059300050525     C                   MOVEL     kpjbu         KPJBUs
059400050525     c                   clear                   kpjbu
059500130520     C                   MOVEL     fnlst3ds      KPJBU
059600130520     C                   CALL      'FNLST3R'
059700931229     C                   PARM                    KPJBA
059800130520     C                   MOVEL     KPJBU         fnlst3ds
059900050525     C                   MOVEL     kpjbus        KPJBU
060000960529     C                   MOVEL     PA5NFV        V1CNFV
060100961111     C                   SETON                                        29
060200961111     C                   CLEAR                   $MSG
060300961111     C                   GOTO      ENDCTR
060400970123    1C                   ENDIF
060500960529     C* Controllo valdità numerica
060600960529     C                   TESTN                   V1CNFV               31
060700970123    1C     *IN31         IFEQ      *ON
060800020808     C                   MOVE      V1CNFV        W001A
060900970123    2C     W001A         IFLT      '0'
061000961111     C                   SETOFF                                       31
061100970123    2C                   END
061200970123    1C                   END
061300961111     C*
061400970123    1C     *IN31         IFEQ      *OFF
061500961111     C                   MOVEL     ERR(4)        $MSG
061600961111     C                   SETON                                        2840
061700961111     C                   GOTO      ENDCTR
061800970123    1C                   END
061900960529     C* Numero foglio viaggio esiste
062000960529     C                   MOVEL     V1CNFV        KNFV
062100020808     C     K02FGV01      CHAIN     FNFGV01L                           30
062200970123    1C     *IN30         IFEQ      '1'
062300960529     C     *IN30         OREQ      '0'
062400931229     C     FGVATB        ANDNE     *BLANKS
062500960529     C                   SETON                                        4028
062600960529     C                   MOVEL     ERR(1)        $MSG
062700931229     C                   GOTO      ENDCTR
062800970123    1C                   ENDIF
062900961212     C* Il foglio viaggio deve essere di defluenza
063000990301     C                   MOVEL     'TV'          COD
063100990301     C                   MOVEL(P)  FGVTTR        KEY
063200990301     C     KTABE         CHAIN     TABEL                              83
063300990301     C     *IN83         IFEQ      *OFF
063400990301     C     TBLFLG        ANDEQ     ' '
063500990301     C                   MOVEL     TBLUNI        DSTV
063600990301     C                   ELSE
063700990301     C                   CLEAR                   DSTV
063800990301     C                   ENDIF
063900990301    1C     §TVDEF        IFNE      'S'
064000961212     C                   SETON                                        4028
064100961212     C                   MOVEL     ERR(19)       $MSG
064200961212     C                   GOTO      ENDCTR
064300970123    1C                   ENDIF
064400960529     C* Già  chiuso
064500970123    1C     FGVFCF        IFNE      ' '
064600961111     C                   SETON                                        4028
064700960529     C                   MOVEL     ERR(2)        $MSG
064800931229     C                   GOTO      ENDCTR
064900970123    1C                   ENDIF
065000970123     C* Esistono delle bolle borderizzate --> ERRORE
065100020808     C     K02BLP79      SETLL     FNBLP79L                               30
065200970123    1C     *IN30         IFEQ      *ON
065300960529     C                   SETON                                        4028
065400960529     C                   MOVEL     ERR(3)        $MSG
065500960529     C                   GOTO      ENDCTR
065600970123    1C                   END
065700970108     C* Se non è un bis non deve esserci un bis aperto con data <=
065800970123    1C     FGVSNP        IFEQ      *BLANKS
065900020808     C     SIMfel        setll     FNFGV06L
066000020808     C     SIMfel        reade     FNFGV06L                               30
066100970123    2C     *IN30         DOWEQ     *OFF
066200020808    3C                   IF            F6Vatb =  *blanks
066300020808     C                             and F6Vlna =  FGVlna
066400020808     C                             and F6Vsnp =  'B'
066500020808     C                             and F6Vdfv <= FGVdfv
066600970107     C                   SETON                                        4028
066700970107     C                   MOVEL     ERR(21)       $MSG
066800970107     C                   GOTO      ENDCTR
066900970123    3C                   END
067000020808     C     SIMfel        reade     FNFGV06L                               30
067100970123    2C                   ENDDO
067200970123   X1C                   ELSE
067300970109     C                   SETON                                        07
067400970123    1C                   END
067500000405      *
067600000405      * Le linee DPD vanno escluse dall'elaborazione del manifest
067700051223      * ?DAL 9/1/2006 non è più vero anzi si dovrà eseguire il manifest
067800051223      * ? x inviare gli SCAN con il Manifest anzichè dal FIEU25R e
067900051223      * ?  anche x eseguire l'EDI in parallelo x consegnare in Spagna
068000051223      * ?   tramite SEUR (Partner DPD spagnolo)
068100000405     C                   clear                   WSffv
068200020808     C                   clear                   XX
068300020808     C                   clear                   YY
068400000405     C                   eval      II = 1
068500000405      *
068600000405    1C                   DOW       FFV(II) > *zeros
068700000405     C                   move      FFV(II)       WFFV
068800000405     C     Wffv          chain     Azorg01l
068900000405      *
069000000405     C                   IF        not %found
069100020225     c                   clear                   og143
069200020225     C                   ELSE
069300020225     C                   movel     ORGDE3        OG143
069400020225     C                   ENDIF
069500020225      *
069600000405     c                   add       1             XX
069700000405     C                   eval      wsFFV(xx) = FFV(II)
069800051223      *
069900000405     c                   add       1             II
070000000405     C                   ENDDO
070100000405     C                   eval      FFV = wsFFV
070200051124     C*
070300051124     C* Solo se in provvisorio e si trattava di un foglio con linea DPD
070400051124     c                   if        V1CDEF = 'NO' and dpdFFV(1) <> ' '
070500051124     C                   eval      FFV = dpdFFV
070600051124     c                   end
070700970108     C*
070800970108     C* CONTROLLO ELABORAZIONE PER DOGANA (SI/NO)
070900970117     C*
071000970120     C                   CLEAR                   D82DGX
071100970120     C                   CLEAR                   D82RSX
071200970120     C                   CLEAR                   D82CAP
071300970120     C                   CLEAR                   D82LOC
071400970120     C                   CLEAR                   D82PRO
071500970108     C                   Z-ADD     1             II
071600970117     C                   CLEAR                   WERR
071700970117     C                   CLEAR                   WDOGSI
071800970117     C                   CLEAR                   WDOGNO
071900970123    1C     FFV(II)       DOWGT     *ZEROS
072000970117     C     WERR          ANDEQ     *BLANKS
072100020808     C                   Z-ADD     1             XY
072200020808     C                   MOVE      FFV(II)       WFFV
072300020808     C     WFFV          LOOKUP    LNA(XY)                                32
072400970123    2C     *IN32         IFEQ      '1'
072500020808     C                   MOVEL     DPT(XY)       EDIDSPT
072600970108     C                   MOVEL     '15'          COD
072700970108     C                   MOVEL(P)  §PTNAZ        KEY
072800970108     C     KTABE         CHAIN     TABEL00F                           30
072900970108     C  N30              MOVEL     TBLUNI        DS15
073000970108     C   30              CLEAR                   DS15
073100970123     C* SE LNA CON ATTRAVERSAMENTO DOGANA:
073200010405    3C     §15dog        IFEQ      '1'
073300010405     C     §15dog        OREQ      '2'
073400970123    4C     V1CDOG        IFEQ      'NO'
073500020808     C                   MOVE      '1'           WDOGSI
073600970123    4C                   END
073700041020      **
073800970108     C* CERCO NUMERO FAX DOGANA
073900970123    4C     D82DGX        IFEQ      *BLANKS
074000041020      **
074100041020     C*  Se non c'è il nr.FAX:
074200041020     C* Attenzione: adesso può esserci l'indirizzo E-mail che è più importante e ha
074300041020     C*  priorità di invio sul nr.FAX ossia: se ho l'E-mail spedisco tramite
074400041020     C*   E-mail e non faccio il FAX.
074500041020     C                   clear                   trasm_mail        2
074600041020      **
074700041020     c                   clear                   trul44ds
074800050913     c                   clear                   trtcm1ds
074900041020      **** cerco sui LUOGHI poichè è l'e-mail della Dogana e non del Partner
075000041020     c                   eval      d44pgm  = 'TRTC82R'
075100041025     c                   eval      d44cl   = '200'
075200041020     c                   move      KSC(XY)       d44ksc
075300041020     c                   call      'TRUL44R'
075400041020     c                   parm                    trul44ds
075500050913     c                   parm                    trtcm1ds
075600041020     c                   if        d44err = *blanks
075700041020     c                   move      'OK'          trasm_mail
075800041020     c                   end
075900041020      **
076000970806     C                   MOVEL     'L'           KSFLS
076100970806     C     KSPE          CHAIN     FNSPE01L                           31
076200970123    5C     *IN31         IFEQ      *OFF
076300000405     C                   CLEAR                   TRUL42DS
076400970807     C                   MOVEL(P)  SPEFAX        D42FAX
076500970108     C                   CALL      'TRUL42R'
076600000405     C                   PARM                    TRUL42DS
076700970108    6C     D42ERR        IFEQ      ' '
076800970807     C                   MOVEL     SPEFAX        D82DGX
076900970120     C                   MOVEL     SPERAG        D82RSX
077000970807     C                   MOVEL     SPECAP        D82CAP
077100970120     C                   MOVEL     SPELOC        D82LOC
077200970120     C                   MOVEL     SPEPRO        D82PRO
077300970108    6C                   END
077400041020      **
077500970108    5C                   END
077600970123    4C                   END
077700970123   X3C                   ELSE
077800970123     C* LNA SENZA ATTRAVERSAMENTO DOGANA
077900970123    4C     V1CDOG        IFEQ      'NO'
078000020808     C                   MOVE      '1'           WDOGNO
078100970123   X4C                   ELSE
078200020808     C                   MOVE      'E'           WERR
078300970123     C                   SETON                                        4428
078400970123     C                   MOVEA     ERR(22)       K78
078500020808     C                   MOVE      FFV(II)       WFFVA
078600970123     C                   MOVEA     WFFVA         K78(41)
078700970123     C                   MOVEA     K78           $MSG
078800970123     C                   GOTO      ENDCTR
078900970123    4C                   END
079000970123    3C                   END
079100970123   X2C                   ELSE
079200970117     C* LINEA ESTERA NON PRESENTE IN TABELLA
079300970117     C                   MOVE      'E'           WERR
079400970117     C                   SETON                                        28
079500970117     C                   MOVEA     ERR(25)       K78
079600020808     C                   MOVE      FFV(II)       WFFVA
079700970117     C                   MOVEA     WFFVA         K78(19)
079800970117     C                   MOVEA     K78           $MSG
079900970117     C                   GOTO      ENDCTR
080000970123    2C                   END
080100970108     C                   ADD       1             II
080200970123    1C                   ENDDO
080300041020     C*
080400970117     C* ERRORE FORZABILE SE NUMERO FAX DOGANA ERRATO E RICHIESTA
080500970117     C* ELABORAZIONE PER DOGANA
080600041020      * adesso se non c'è nemmeno l'indirizzo e-mail
080700970117     C     V1CDOG        IFEQ      'SI'
080800970120     C     D82DGX        ANDEQ     *BLANKS
080900041020     C     trasm_mail    andeq     *blanks
081000970117     C     FGVNFV        ANDNE     WGVNFV
081100970117     C                   MOVE      FGVNFV        WGVNFV
081200970121     C                   SETON                                        28
081300970117     C                   MOVEL     ERR(23)       $MSG
081400970117     C                   GOTO      ENDCTR
081500970117     C                   END
081600970117     C*
081700970117     C* ERRORE FORZABILE SE ALCUNE LINEE CON DOGANA ED ALTRE SENZA
081800970117     C     V1CDOG        IFEQ      'NO'
081900970117     C     WDOGSI        ANDNE     *BLANKS
082000970117     C     WDOGNO        ANDNE     *BLANKS
082100970117     C     FGVNFV        ANDNE     WWVNFV
082200970117     C                   MOVE      FGVNFV        WWVNFV
082300970121     C                   SETON                                        28
082400970117     C                   MOVEL     ERR(26)       $MSG
082500970117     C                   GOTO      ENDCTR
082600970117     C                   END
082700970108     C*
082800961212     C* ERRORE FORZABILE: SE DATA FOGLIO < DATA DI SISTEMA LI AVVERTO
082900970108     C*
083000961212     C     FGVDFV        IFNE      WGVDFV
083100961212     C                   MOVE      FGVDFV        WGVDFV
083200961212     C     FGVDFV        IFLT      DATEU
083300961212     C                   SETON                                        4028
083400961212     C                   MOVEL     ERR(18)       $MSG
083500961212     C                   GOTO      ENDCTR
083600961212     C                   END
083700961212     C                   END
083800970123     C* MEMORIZZO SE OCCORRE INVIARE VIA FAX TOTALI PER DOGANA
083900970127     C     V1CDOG        IFEQ      'SI'
084000970218     C                   MOVE      'F'           D82TFF
084100970123     C                   END
084200960530     C*
084300960530     C     ENDCTR        ENDSR
084400960530     C***************************************************************
084500960530     C*  Controlli seconda videata                                  *
084600960530     C***************************************************************
084700960530     C     CONTR2        BEGSR
084800960530     C*
084900970115     C                   SETOFF                                       46
085000010509     C*  Esegue la routine di controlli solo se ci sono dei records
085100010509     C*  nel SFL. Se per esempio il foglio Viaggio è fatto solo su
085200010509     C*  linee DpD, il SFL è sicuramente vuoto.
085300010509     C     *in20         Ifeq      *on
085400010509     C*
085500970108     C* CONTROLLI CAMPI DEL SUBFILE
085600970110     C                   SETON                                        22
085700970110     C                   READC     TC82S01                                31
085800970110    1C     *IN31         DOWEQ     *OFF
085900970123     C     VSCDGN        IFEQ      'SI'
086000970218     C                   MOVE      'F'           D82TFF
086100970123     C                   END
086200970108     C*  Controllo che codice partner > 0
086300970110    2C     VSCCPT        IFEQ      0
086400970109     C                   CLEAR                   VSHCPT
086500970110     C                   CLEAR                   VSDCPT
086600970108     C                   SETON                                        4328
086700970108     C                   MOVEL     ERR(10)       $MSG
086800970108     C                   Z-ADD     NRR1          V1CNBR
086900970108     C                   UPDATE    TC82S01
087000970108     C                   GOTO      ENDCT2
087100970110    2C                   END
087200970108     C* Se ho modificato il codice del Partner controllo se congruente
087300970108     C* con lna
087400970110    2C     VSCCPT        IFNE      VSHCPT
087500020808     C                   Z-ADD     1             XY
087600020808     C     VSCCPT        LOOKUP    KSC(XY)                                32
087700970110    3C     *IN32         IFEQ      '1'
087800020808    4C     VSCLNA        IFNE      LNA(XY)
087900970110     C                   CLEAR                   VSDCPT
088000970110     C                   CLEAR                   VSHCPT
088100970108     C                   SETON                                        4328
088200970108     C                   MOVEL     ERR(6)        $MSG
088300970108     C                   Z-ADD     NRR1          V1CNBR
088400970108     C                   UPDATE    TC82S01
088500970108     C                   GOTO      ENDCT2
088600970110    4C                   END
088700970110   X3C                   ELSE
088800970110     C                   CLEAR                   VSDCPT
088900970110     C                   CLEAR                   VSHCPT
089000970108     C                   SETON                                        4328
089100970108     C                   MOVEL     ERR(11)       $MSG
089200970108     C                   Z-ADD     NRR1          V1CNBR
089300970108     C                   UPDATE    TC82S01
089400970108     C                   GOTO      ENDCT2
089500970110    3C                   END
089600970108     c* reimposto campi ed indicatori in base al nuovo cod.partner
089700970110    3C     VSHFTT        IFNE      *BLANKS
089800970108     C                   SUB       1             WSIEDI
089900970110    3C                   END
090000970108     C                   EXSR      SRCPT
090100970110     C*
090200970110     C* MEMORIZZO SE DEVO VISUALIZZARE MESSAGGIO FORZABILE CHE N.FAX
090300970110     C* PARTNER ERRATO
090400970110    3C     VSHFAP        IFEQ      *BLANKS
090500970110     C     VCCDEF        ANDEQ     'SI'
090600970110     C     SAVNR1        ANDEQ     *ZEROS
090700970110    4C     VSCSTM        IFEQ      'SI'
090800970110     C     VSCCOD        OREQ      'SI'
090900970110     C     VSCLDS        OREQ      'SI'
091000970110     C                   Z-ADD     NRR1          SAVNR1
091100970110    4C                   END
091200970110   X3C                   ELSE
091300970110     C* SE NUMERO FAX CORRETTO CONTROLLO DI NON AVER MEMORIZZATO IN
091400970110     C* UN GIRO PRECEDENTE CHE IL FAX E' ERRATO ED IN QUESTO CASO
091500970110     C* PULISCO I CAMPI DI COMODO
091600970110    4C     SAVNR1        IFEQ      NRR1
091700970110     C                   CLEAR                   SAVNR1
091800970110     C                   MOVE      *BLANKS       WFIRST
091900970110    4C                   END
092000970110    3C                   END
092100970110     C*
092200970108     C                   SETON                                        29
092300970108     C                   Z-ADD     NRR1          V1CNBR
092400970108     C                   UPDATE    TC82S01
092500970110   X2C                   ELSE
092600970110     C* SOLO LA PRIMA VOLTA E NON E' STATO MODIFICATO IL COD. PARTNER:
092700970108     C* se elaborazione definitiva memorizzo se occorre inviare msg
092800970108     C* forzabile se numero fax partner errato
092900970110    3C     VCCDEF        IFEQ      'SI'
093000970108     C     VSHFAP        ANDEQ     *BLANKS
093100970109     C     SAVNR1        ANDEQ     *ZEROS
093200970110     C     WFIRST        ANDEQ     *BLANKS
093300970110    4C     VSCSTM        IFEQ      'SI'
093400970108     C     VSCCOD        OREQ      'SI'
093500970108     C     VSCLDS        OREQ      'SI'
093600970108     C                   Z-ADD     NRR1          SAVNR1
093700970110     C                   MOVE      '1'           WFIRST
093800970110    4C                   END
093900970110    3C                   END
094000970110    2C                   END
094100970123     C* SE ELABORAZIONE PER DOGANA SI E NON C'E ATTRAVERSAM. DOGANA ERR
094200970123     C     VSCDGN        IFEQ      'SI'
094300970123     C     VSHEFT        ANDEQ     *BLANKS
094400970123     C                   SETON                                        4728
094500970123     C                   MOVEA     ERR(22)       K78
094600020808     C                   MOVE      VSCLNA        WFFVA
094700970123     C                   MOVEA     WFFVA         K78(41)
094800970123     C                   MOVEA     K78           $MSG
094900970123     C                   Z-ADD     NRR1          V1CNBR
095000970123     C                   UPDATE    TC82S01
095100970123     C                   GOTO      ENDCT2
095200970123     C                   END
095300970110     C                   READC     TC82S01                                31
095400970110    1C                   ENDDO
095500010509      *
095600010509      * se il SFL era carico
095700010509    1C                   EndIF
095800010509      *
095900970108     C* CONTROLLO CAMPI DEL SUBFILE CONTROL
096000970108     C* Se data CMR > 0 controllo che sia corretta
096100970108     C     VCCDAC        IFGT      *ZEROS
096200970108     C                   CLEAR                   WLBDA8
096300970108     C                   MOVE      VCCDAC        G02DAT
096400970108     C                   CALL      'XSRDA8'
096500970108     C                   PARM                    WLBDA8
096600970109     C                   MOVE      G02INV        D82DCM
096700020808     C                   MOVEL     G02INV        anno40
096800970108     C                   MOVE      G02DAT        VCCDAC
096900040520     C     *ymd          move      g02inv        dataCMR
097000040520     C     dataOGGi      subdur    7:*d          dataISO
097100040520     C     dataOGGi      adddur    7:*d          dataISO1
097200040520     c
097300970108     C     G02ERR        IFEQ      '1'
097400010611     C     anno40        orLT      daanno
097500010611     C     anno40        orGT      adanno
097600040520     C     dataCMR       orLT      dataISO
097700040520     C     dataCMR       orgT      dataISO1
097800970108     C                   SETON                                        4228
097900970108     C                   MOVEL     ERR(9)        $MSG
098000970108     C                   GOTO      ENDCT2
098100970108     C                   END
098200970108     C                   END
098300970108     C* Se CMR obbligatorio controllo che data e numero siano stati im-
098400970108     C* messi
098500970109     C* data CMR
098600970109     C     VCCDAC        IFEQ      *ZEROS
098700970109     C                   SETON                                        4228
098800970109     C                   MOVEL     ERR(8)        $MSG
098900970109     C                   GOTO      ENDCT2
099000970109     C                   END
099100970108     c* numero CMR
099200970108     C     VCCCMR        IFEQ      *BLANKS
099300970108     C     VCCCMR        OREQ      *ZEROS
099400970108     C                   SETON                                        4128
099500970108     C                   MOVEL     ERR(7)        $MSG
099600970108     C                   GOTO      ENDCT2
099700970108     C                   END
099800970218     C* Targa motrice
099900970218     C     VCCTRM        IFEQ      *BLANKS
100000970218     C                   SETON                                        2845
100100970218     C                   MOVEL     ERR(27)       $MSG
100200970218     C                   GOTO      ENDCT2
100300970218     C                   END
100400961204     C*
100500970108     C* SE ELABORAZIONE DEFINITIVA E NON HO NUMERO FAX PARTNER INVIO
100600970108     C* MESSAGGIO FORZABILE
100700010509     C     *in20         Ifeq      *on
100800010509      *
100900970109     C     SAVNR1        IFGT      *ZEROS
101000970109     C     SAVNR1        CHAIN     TC82S01                            30
101100970109     C                   Z-ADD     SAVNR1        V1CNBR
101200970110     C                   CLEAR                   SAVNR1
101300961213     C                   SETON                                        28
101400961213     C                   MOVEL     ERR(20)       $MSG
101500970109     C                   UPDATE    TC82S01
101600961213     C                   GOTO      ENDCT2
101700010509     C                   END
101800010509     C*
101900010509     C                   End
102000961204     C*
102100961022     C* Controllo se devo avvisare x elaborazione definitiva
102200970108     C     WSIEDI        IFGT      *ZEROS
102300970108     C     VCCDEF        ANDEQ     'SI'
102400961023     C     WSVMSG        ANDNE     ERR(12)
102500961022     C                   SETON                                        28
102600961022     C                   MOVEL     ERR(12)       $MSG
102700961022     C                   END
102800960530     C*
102900970110     C     ENDCT2        ENDSR
103000000405      *---------------------------------------------------------------*
103100000405      * *INZSR: Operazioni iniziali                                  -*
103200000405      *---------------------------------------------------------------*
103300960529     C     *INZSR        BEGSR
103400960529     C*
103500960529     C     *ENTRY        PLIST
103600960529     C                   PARM                    KPJBA
103700960529     C* Richiamo XPARUT
103800960529     C                   Z-ADD     1             CODUT
103900960529     C                   CALL      'X§PARUT'
104000000405     C                   PARM                    UT§DSE0F
104100960529     C                   MOVEL     REC80         CNCR80
104200960529     C                   MOVEL     RAGUT         RSUT             20
104300020920      *
104400020920     C                   MOVEL     kpjbu         KPJBUs
104500050525     c                   clear                   kpjbu
104600150909     C* REPERISCO DATA DEL SISTEMA
104700150909     C                   TIME                    WTIME                          ORA E DATA
104800150909     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
104900150909     C                   MOVE      Wdate         anno                           AAAA
105000150909     C     anno          sub       1             daanno                         AAAA
105100150909     C     anno          add       1             adanno                         AAAA
105200150909     C                   Z-ADD     WDATE         G02DAT
105300150909     C                   Z-ADD     *ZERO         G02INV
105400150909     C                   MOVEL     *BLANKS       G02ERR
105500150909     C                   CALL      'XSRDA8'
105600150909     C                   PARM                    WLBDA8
105700150909     C                   Z-ADD     G02INV        DATEU
105800150909     c     *ymd          move      dateu         dataoggi
105900150909     c* Determino data foglio - 1 gg lavorativo
106000150909     c                   Clear                   Ktfp
106100150909     c                   Z-Add     simpou        Ktfa
106200150909     c                   Move      dataoggi      wdtaeur
106300150909      * meno 1 gg. lavorativo
106400150909     c                   Do        *Hival
106500150909     c                   Subdur    1:*d          wdtaeur
106600150909     c                   extrct    wdtaeur:*d    wgiorno
106700150909     c                   extrct    wdtaeur:*m    kmes
106800150909     c                   extrct    wdtaeur:*y    kann
106900150909     c     kcln1         Chain     Azcln01l
107000150909     c                   If        Not %Found(azcln01l)
107100150909     c                   Leave
107200150909     c                   EndIf
107300150909     c                   If        POM(wgiorno) = 'F'
107400150909     c                   Iter
107500150909     c                   EndIf
107600150909     c                   Leave
107700150909     c                   EndDo
107800150909     c
107900150909     c                   Move      wdtaeur       wdtaiso
108000150909     c                   Move      wdtaiso       wDEFmeno1
108100150909     C*
108200120821      **
108300120821     c* Carico x terminal  di arrivo
108400150909     c* alla data meno 1
108500120821     c                   clear                   trul09ds
108600120821     c                   movel     simPOU        i09tfa
108700120821     c                   movel     'TFA'         i09mod
108800150909     c                   movel     wDEFmeno1     i09dta
108900120821     c                   call      'TRUL09R'
109000120821     c                   parm                    trul09ds
109100150909     c                   movea     lin           lindef
109200150909     c
109300150909     c* Carico x terminal  di arrivo
109400150909     c                   clear                   trul09ds
109500150909     c                   movel     simPOU        i09tfa
109600150909     c                   movel     'TFA'         i09mod
109700150909     c                   call      'TRUL09R'
109800150909     c                   parm                    trul09ds
109900120821     c
110000020920     C                   MOVEL     kpjbus        KPJBU
110100020920      *
110200121102      *
110300961129     C*--- RICERCA CAPOCONTI------------------------------------------*
110400020808     C                   DO        50            XY
110500020808     C                   MOVE      TCU(XY)       TCUDS
110600961129     C     F56           IFEQ      'CG'
110700961129     C     F4            ANDEQ     '1'
110800961129     C     F3            ANDEQ     '0'
110900020808     C                   Z-ADD     KCU(XY)       KCI
111000961129     C                   END
111100961129     C                   END
111200960529     C*
111300960529     C* Definizione chiavi di accesso
111400961112     C     KTABE         KLIST
111500961112     C                   KFLD                    CODUT
111600961112     C                   KFLD                    COD
111700961112     C                   KFLD                    KEY
111800961129     C     KACO          KLIST
111900961129     C                   KFLD                    CODUT
112000961129     C                   KFLD                    KCI
112100970108     C                   KFLD                    VSCCPT
112200961216     C     KSPE          KLIST
112300970806     C                   KFLD                    KSFLS
112400020808     C                   KFLD                    KSC(XY)
112500961216     C                   KFLD                    KSCOD
112600970527     C     KTRE0         KLIST
112700970127     C                   KFLD                    D82NFV
112800970127     C                   KFLD                    SIMFEL
112900970127     C                   KFLD                    WTRC
113000020808     C     K02BLP79      klist
113100020808     C                   kfld                    SIMfel
113200020808     C                   kfld                    KNFV
113300020808     C     K02FGV01      klist
113400020808     C                   kfld                    KNFV
113500020808     C                   kfld                    SIMfel
113600020808     C     K02FGW01      klist
113700020808     C                   kfld                    FGVnfv
113800020808     C                   kfld                    FGVlnp
113900150909     C     KCLN1         KLIST
114000150909     C                   KFLD                    KTFP
114100150909     C                   KFLD                    KTFA
114200150909     C                   KFLD                    KANN
114300150909     C                   KFLD                    KMES
114400960529     C*
114500960529     C* Carico dati DS partner in schiera
114600020808     C                   Z-ADD     1             XY
114700960529     C                   MOVEL     'PT'          KCOD
114800961114     C     KCOD          CHAIN     EDTAB01L                           30
114900960529     C     *IN30         DOWEQ     '0'
115000960529     C     TABFLG        IFEQ      ' '
115100020920     C                   MOVEL     TABUNI        EDIDSPT
115200020920      * solo se compresa nelle linee di partenza del terminal
115300020920     c     §ptlnp        lookup    Lin                                    32
115400150909     c
115500150909     c                   if        not *in32
115600150909     c     §ptlnp        lookup    Lindef                                 32
115700150909     c                   endif
115800150909     c
115900020920     c                   if        *in32 = *on
116000020808     C                   ADD       1             XY
116100020808     C                   MOVEL     TABKEY        CPU(XY)
116200020808     C                   MOVEL     TABKEY        CPT(XY)
116300020808     C                   MOVE      '    '        CPT(XY)
116400020808     C                   MOVEL     TABUNI        DPT(XY)
116500020808     C                   MOVEL     §PTKSC        KSC(XY)
116600020808     C                   MOVEL     §PTLNP        LNA(XY)
116700020920     c                   endif
116800960529     C                   END
116900961114     C     KCOD          READE     EDTAB01L                               30
117000960529     C                   END
117100121102      *
117200121102      * Controlla riempimento della PT x limiti di schiera
117300121102      *  se deve inviare la mail di alert x limite quasi raggiunto.
117400121102     c                   exsr      ChecK_PT
117500960906     C*
117600960906     C                   MOVEL     'PU'          KCOD
117700961114     C     KCOD          CHAIN     EDTAB01L                           30
117800960906     C     *IN30         DOWEQ     '0'
117900960906     C     TABFLG        IFEQ      ' '
118000020808     C                   Z-ADD     1             XY
118100020808     C     TABKEY        LOOKUP    CPU(XY)                                32
118200960906     C     *IN32         IFEQ      '1'
118300020808     C                   MOVEL     TABUNI        DPU(XY)
118400000405     C                   MOVEL     TABUNI        EDIDSPU
118500961023     C* Se no partner metto zero in LNA
118600051124      *** Attenzione se è una Linea DPD deve lasciarla
118700051124     c     lna(xy)       chain     azorg01l
118800051124     c                   if        %Found(azorg01l)
118900051124     c                   move      orgde3        og143
119000051124     c                   end
119100051124      ***   pulisce la linea se è un cliente e non è un DPD
119200110913     C     §PUTPC        IFne      'P'
119300051124     c                   if        §ogntw <> 'DPD'
119400020808     C                   Z-ADD     0             LNA(XY)
119500051124     c                   end
119600961023     C                   END
119700051124      ***
119800960906     C                   END
119900960906     C                   END
120000961114     C     KCOD          READE     EDTAB01L                               30
120100960906     C                   END
120200961121     C*
120300960529     C*
120400960529     C                   ENDSR
120500121102      *---------------------------------------------------------------*
120600121102      * *INZSR: Operazioni iniziali                                  -*
120700121102      *---------------------------------------------------------------*
120800121102     C     Check_PT      begsr
120900121102     C*
121000121102     c* Controllo riempimento schiera
121100121102     c                   clear                   trul0sds
121200121102     c                   eval      i0sski='CPT'
121300121102     c                   eval      i0sele=%elem(CPT)
121400121102     c                   eval      i0spie=xy
121500121102     c                   eval      i0sfile='EDTAB00F'
121600121102     c                   eval      i0ssif=knsif
121700121102     c                   eval      i0spgm='TRTC82R'
121800121113     C                   MOVEL     kpjbu         KPJBUs
121900121113     C                   clear                   KPJBU
122000121102     c                   movel     trul0sds      kpjbu
122100121102     c                   call      'TRUL0SR'
122200121102     c                   parm                    kpjba
122300121113     C                   MOVEL     kpjbuS        KPJBU
122400121102     C*
122500121102     C                   ENDSR
122600970218**
122700961111Foglio Errato o Annullato                                                     01
122800961111Foglio già chiuso                                                             02
122900961111Presenti bolle borderizzate: un foglio per l'estero non contiene bolle        03
123000961111Numero foglio viaggio presenta dei caratteri non validi                       04
123100961111                                                                              05
123200961111Codice Partner non congruente con linea di arrivo                             06
123300961111Numero CMR obbligatorio                                                       07
123400961111Data CMR obbligatoria                                                         08
123500961111Data CMR non valida                                                           09
123600961111Codice Partner obbligatorio                                                   10
123700961111Codice Partner inesistente                                                    11
123800970117ATTENZIONE!!!! Se elab.definitiva si procederà alla scrittura msg.EDI         12
123900961111Numero foglio I.M.A. presenta dei caratteri non validi                        13
124000961111Data foglio IMA (XX/XX/XXXX) diversa da data foglio viaggio (XX/XX/XXXX)      14
124100961111Non esiste nessun collo per l'estero spuntato sull'IMA richiesto.ENTER X FORZ.15
124200961111Non è un inventario arrivi                                                    16
124300961111Numero foglio I.M.A. obbligatorio                                             17
124400961212ATTENZIONE:Richiesto foglio viaggio con data < di data odierna.ENTER PER FORZ.18
124500970121Immettere un foglio viaggio di DEFLUENZA                                      19
124600041020Nr.FAX Partner/Indirizzo E-MAIL ERRATO. trasm. NON POSSIBILE     ENTER X FORZ.20
124700970108Presente foglio viaggio BIS ancora da chiudere per questa linea di arrivo     21
124800970117Elaborazione per dogana='SI' ma per lna xxx non è previsto l'attravers.dogana 22
124900041020Nr.FAX DOGANA/Indirizzo E-MAIL ERRATO. trasm. NON POSSIBILE     ENTER X FORZ. 23
125000970109Targa motrice obbligatoria se elaborazione definitiva o per la dogana         24
125100970117Non trovata linea xxx in tabella partner                                      25
125200970117Tutte le linee di arrivo devono avere o non avere l'attrav.dogana.ENTER X FORZ26
125300970218Targa motrice obbligatoria                                                    27
