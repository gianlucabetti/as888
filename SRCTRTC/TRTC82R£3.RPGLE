000100000405     H DECEDIT('0,') DATEDIT(*DMY.)
000200000405      * TRTC82R *----------------------------------------------------*
000300000405      *     FILTRO MANIFEST SPEDIZIONI PER  E.D.I. ESTERO
000400000405      *--------------------------------------------------------------*
000500000405     FTRTC82D   CF   E             WORKSTN SFILE(TC82S01:NRR1)
000600000405      *---------
000700941221     FFNFGV01L  IF   E           K DISK
000800000405      *---------
000900020808     FFNFGV06L  IF   E           K DISK    RENAME(FNFGV000:FGV06)
001000020808     F                                     prefix(F6V:3)
001100020808      *---------
001200000505     FFNFGW01L  IF   E           K DISK
001300000405      *---------
001400020808     FFNBLP79L  IF   E           K DISK
001500000405      *---------
001600960529     FEDTAB01L  IF   E           K DISK
001700000405      *---------
001800961112     FTABEL00F  IF   E           K DISK
001900961129     FCNACO00F  IF   E           K DISK
002000961213     FCNIND00F  IF   E           K DISK
002100970806     FFNSPE01L  IF   E           K DISK
002200970108     FAZORG01L  IF   E           K DISK
002300050523     FWFMAN01L  UF   E           K DISK
002400000405      *--------------------------------------------------------------*
002500000405      *  SCHIERE                                                     *
002600000405      *--------------------------------------------------------------*
002700121102     D TRUL0SDS      E DS
002800121102     D*
002900121102     D**** CPU             S             35    DIM(200)                             Cod.partner
003000121102     D**** CPT             S             35    DIM(200)                             Cod.partner
003100121102     D**** DPT             S             90    DIM(200)                             Tab.PT
003200121102     D**** DPU             S             90    DIM(200)                             Tab.PU
003300121102     D**** KSC             S              7  0 DIM(200)                             Cod.cliente
003400121102     D**** LNA             S              3  0 DIM(200)                             Lna
003500121102      **
003600121102     D CPT             S             35    DIM(200)                             Cod.partner
003700121102     D CPU             S             35    DIM(%elem(CPT))                      Cod.partner
003800121102     D DPT             S             90    DIM(%elem(CPT))                      Tab.PT
003900121102     D DPU             S             90    DIM(%elem(CPT))                      Tab.PU
004000121102     D KSC             S              7  0 DIM(%elem(CPT))                      Cod.cliente
004100121102     D LNA             S              3  0 DIM(%elem(CPT))                      Lna
004200121102      **
004300970218     D ERR             S             78    DIM(27) CTDATA PERRCD(1)             Errore
004400970117     D K78             S              1    DIM(78)                              DI COMODO
004500000405      *
004600000505     D WSFFV           S              3    DIM(301)                             LINEE DEL FV
004700010509     D DpDFFV          S              3    DIM(301)                             LINEE DEL FV
004800000405      *--------------------------------------------------------------*
004900000405      *  DS                                                          *
005000000405      *--------------------------------------------------------------*
005100000405      * PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - TRTC83C -
005200960529     D TRTC82        E DS                  EXTNAME(TRTC82DS)
005300000405      *
005400000405      * PASSAGGIO DATI A INTERROGAZIONE FOGLI V. PARTENZE - FNLS53R -
005500931229     D PARAM5          DS
005600951108     D  PA5FLG                 1      1    INZ('R')
005700951108     D  PA5FL2                 2      2    INZ
005800951108     D  PA5NFV                 3      5P 0 INZ
005900960201     D  PA5DIN                 6     10P 0 INZ
006000951108     D  PA5DFI                11     15P 0 INZ(31129999)
006100951108     D  PA5TFV                16     16    INZ('2')
006200000614     D  PA5FGS               242    244  0
006300000405      *
006400900511     D KPJBA         E DS
006500120821     D TRUL09DS      E DS
006600120821     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
006700000405     D UT§DSE0F      E DS
006800000405     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
006900000405     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    PARAM CAPI CONTO
007000961129     D TCUDS           DS
007100961129     D  F3                     3      3
007200961129     D  F4                     4      4
007300961129     D  F56                    5      6
007400960529     D CNCR80        E DS
007500000405     D OG143         E DS
007600000405     D EDIDSPT       E DS
007700000405     D EDIDSPU       E DS
007800961112     D DS15          E DS
007900990301     D DSTV          E DS
008000000405     D TRUL42DS      E DS
008100020808     D*
008200041020     D* ds per ind. email
008300041020     Dtrul44ds       E DS                  INZ
008400050913     D trtcm1ds      E DS
008500041020      *
008600960701     D WLBDA8          DS
008700960701     D  G02DAT                 1      8  0
008800960701     D  G02INV                 9     16  0
008900960701     D  G02ERR                17     17
009000960701     D  G02TGI                18     22  0
009100970108     D* LINEE DI ARRIVO  PER FNFGV00F
009200970108     D                 DS
009300970108     D  FGVLNA                 1      3  0
009400970108     D  FGVFFV                 4    243
009500970108     D  FGVFF2               244    450
009600000505     D  FGWFF3               451    690
009700000505     D  FGWFF4               691    900
009800000505     D  VUOTO                901    903
009900000505     D  FFV                    1    903    DIM(301)                             LINEE DEL FV
010000020808      *
010100020808      * Definizione variabili
010200020808      *
010300020920     D kpjbus          S                   LIKE(kpjbu)
010400020808     D WTIME           S             14  0                                      ORA E DATA
010500020808     D WDATE           S              8  0                                      GG/MM/AAAA
010600020808     D Anno            S              4  0                                      AAAA
010700020808     D DAanno          S              4  0                                      AAAA
010800020808     D ADanno          S              4  0                                      AAAA
010900020808     D DATEU           S              8  0                                      AAAA/MM/GG
011000020808     D Anno40          S              4  0
011100040520     D DataISO         S               d   DATFMT(*iso)
011200040520     D DataISO1        S               d   DATFMT(*iso)
011300040520     D DataOggi        S               d   DATFMT(*iso)
011400040520     D DataCMR         S               d   DATFMT(*iso)
011500040520     D KCI             S              4  0
011600020808     D II              S              3  0 inz
011700020808     D XX              S              3  0 inz
011800020808     D XY              S              3  0 inz
011900020808     D YY              S              3  0 inz
012000020808     D NRR1            S              5  0 inz
012100020808     D WERR            S              1    inz
012200020808     D WTPVID          S              1    inz
012300020808     D WMBR            S              8    inz
012400020808     D WFFV            S              3  0 inz
012500020808     D WFFVA           S              3    inz
012600020808     D WFIRST          S              1    inz
012700020808     D WSIEDI          S              2  0 inz
012800020808     D WFINE           S              1    inz
012900020808     D WDOGSI          S              1    inz
013000020808     D WDOGNO          S              1    inz
013100020808     D W001A           S              1    inz
013200020808     D soloDPD         S              1    inz
013300020808     D SAVNR1          S                   LIKE(NRR1)   inz
013400020808     D WSVMSG          S                   LIKE($MSG)   inz
013500020808     D KCOD            S                   LIKE(TABCOD) inz
013600020808     D KNFV            S                   LIKE(FGVNFV) inz
013700020808     D COD             S                   LIKE(TBLCOD) inz
013800020808     D KEY             S                   LIKE(TBLKEY) inz
013900020808     D KSCOD           S                   LIKE(SPECOD) inz('200')
014000020808      * ............................... -> 200 - Luogo invio fax a Dogana
014100020808     D KSFLS           S                   LIKE(SPEFLS) inz('L')
014200020808     D WTRC            S                   LIKE(TRETRC) inz('P')
014300020808     D WGVDFV          S                   LIKE(FGVDFV) inz
014400020808     D WGVNFV          S                   LIKE(FGVNFV) inz
014500020808     D WWVNFV          S                   LIKE(FGVNFV) inz
014600000405      ***************************************************************
014700000405      *  RIEPILOGO INDICATORI                                       *
014800000405      ***************************************************************
014900000405      **04    - ON --> SPROTEGGO FILIALE GESTIONE FOGLIO IMA        *
015000000405      **05    - OFF--> NUMERO/DATA CMR OBBLIGATORI                  *
015100000405      * 07    - ON=VISUALIZZA 'BIS' SE FV BIS                       *
015200000405      * 28    - Indicatore generico di errore                       *
015300000405      * 29    - Indicatore generico PER RIEMETTERE VIDEATA SENZA ERR*
015400000405      * 30    - Esecuzione Chain/read                               *
015500000405      * 31    - Indicatore di TESTN                                 *
015600000405      * 32    - Indicatore di risposta SCAN                         *
015700000405      * 40    - Errore su foglio viaggio                            *
015800000405      * 41    - ERRORE N. CMR                                       *
015900000405      * 42    - ERRORE DATA CMR                                     *
016000000405      * 43    - Errore su codice partner                            *
016100000405      * 44    - Errore su ELABORAZIONE PER DOGANA (SI/NO)           *
016200000405      * 45    - Errore targa motrice                                *
016300000405      * 46    - POSIZIONAMENTO CURSORE DATA CMR SE OBBLIGATORIA     *
016400000405      * 47    - ERRRORE SU MANIFEST PER DOGANA PER LNA              *
016500000405      ***************************************************************
016600000405
016700000405      *-------------------------------------------------------------*
016800000405      *  CICLO PRINCIPALE                                           *
016900000405      *-------------------------------------------------------------*
017000000405      *  Inizializzazione parametri a video
017100960529     C                   EXSR      INZ01
017200000405      *  Loop gestione prima videata
017300960529     C     WFINE         DOWNE     'S'
017400960530     C     WTPVID        CASEQ     '1'           GESD01
017500960530     C     WTPVID        CASEQ     '2'           GESD02
017600960529     C                   END
017700960530     C                   END
017800000405      *
017900960529     C     FINE          TAG
018000000405     C                   eval       *inlr = *on
018100960529     C***************************************************************
018200960529     C*  Inizializzazione prima videata                             *
018300960529     C***************************************************************
018400960529     C     INZ01         BEGSR
018500960529     C*
018600970218     C                   CLEAR                   TRTC82
018700970108     C                   SETOFF                                       402844
018800960529     C                   MOVEL     *BLANKS       $MSG
018900960529     C                   MOVEL     *BLANKS       V1CNFV
019000961111     C                   MOVE      '?'           V1CNFV
019100961111     C                   MOVEL     'NO'          V1CDEF
019200970107     C                   MOVEL     'NO'          V1CDOG
019300960530     C                   MOVEL     '1'           WTPVID
019400020808     c*
019500020808     c* Imposto il P.O. in gestione
019600020808     c                   z-add     SIMpou        V1Cpog
019700020808     c                   clear                   V1Dpog
019800020808     c     SIMpou        chain     AZORG
019900020808    1c                   if             %found(AZORG01L)
020000020808     c                             and  ORGfva = *blanks
020100020808     c                             and (ORGfag = 'A'
020200020808     c                              or  ORGfag = 'F')
020300020808     c                   movel     ORGdes        V1Dpog
020400020808    1c                   endif
020500960529     C*
020600960529     C                   ENDSR
020700960530     C***************************************************************
020800960530     C*  Inizializzazione seconda videata                           *
020900960530     C***************************************************************
021000960530     C     INZ02         BEGSR
021100960530     C*
021200970108     C                   SETOFF                                       414243
021300970115     C                   SETOFF                                       454628
021400970123     C                   SETOFF                                       47
021500960530     C                   MOVEL     *BLANKS       $MSG
021600020808     C                   MOVEL     '2'           WTPVID
021700970108     C*
021800970108     C                   MOVE      V1CNFV        VCCNFV
021900970108     C                   MOVE      V1CDEF        VCCDEF
022000970108     C                   MOVE      V1CDOG        VCCDOG
022100970108     C*
022200970108     C                   Z-ADD     0             VCCDAC
022300970108     C                   MOVEL     *BLANKS       VCCCMR
022400970109     C                   MOVE      FGVTRM        VCCTRM
022500970108     C* Pulizia subfile
022600970108     C                   SETOFF                                       2123
022700970109     C                   WRITE     TC82C01
022800970108     C                   SETON                                        2021
022900020808     C                   MOVE      *ZEROS        NRR1
023000970108     C                   MOVE      *ZEROS        V1CNBR
023100970124     C*
023200970124     C* MANIFEST PER LA DOGANA
023300970124     C     VCCDOG        IFEQ      'SI'
023400970124     C                   MOVE      'SI'          VSCDGN
023500970124     C                   ELSE
023600970124     C                   MOVE      'NO'          VSCDGN
023700970124     C                   END
023800970124     C* MANIFEST SPEDIZIONI C.O.D. E LISTA DISCORDANZE
023900970124     C     VCCDEF        IFEQ      'SI'
024000970124     C                   MOVE      'SI'          VSCCOD
024100970124     C                   MOVE      'SI'          VSCLDS
024200970124     C                   ELSE
024300970124     C                   MOVE      'NO'          VSCCOD
024400970124     C                   MOVE      'NO'          VSCLDS
024500970124     C                   END
024600970108     C*
024700970108     C* CARICAMENTO SUBFILE
024800020808     c     K02FGW01      chain     fnfgw01l                           39
024900000505     c                   if        *in39 or fgwatb<>' '
025000000505     c                   clear                   fgwff3
025100000505     c                   clear                   fgwff4
025200000505     c                   endif
025300970108     C                   CLEAR                   WSIEDI
025400020808     C                   Z-ADD     1             II
025500970108     C     FFV(II)       DOWGT     *ZEROS
025600970110     C                   SETOFF                                       22
025700970108     C                   MOVE      FFV(II)       VSCLNA
025800000405      *
025900000405      * Decodifico linea di arrivo
026000970108     C     VSCLNA        CHAIN     AZORG01L                           32
026100000405      *
026200970108     C  N32              MOVEL     ORGDES        VSDLNA
026300970109     C   32              MOVEL     *ALL'?'       VSDLNA
026400970108     C* Codice partner
026500970108     C                   Z-ADD     0             VSCCPT
026600970108     C*
026700020808     C                   Z-ADD     1             XY
026800020808     C                   MOVE      FFV(II)       WFFV
026900020808     C     WFFV          LOOKUP    LNA(XY)                                32
027000970108     C     *IN32         IFEQ      '1'
027100970108     C                   EXSR      SRCPT
027200970123     C* MEMORIZZO SE C'E' ATTRAVERSAMENTO DOGANA
027300970123     C                   MOVEL     §PTNAZ        VSHNAZ
027400970123     C                   MOVEL     '15'          COD
027500970123     C                   MOVEL(P)  VSHNAZ        KEY
027600970123     C     KTABE         CHAIN     TABEL00F                           30
027700970123     C  N30              MOVEL     TBLUNI        DS15
027800970123     C   30              CLEAR                   DS15
027900010405     C                   MOVE      §15DOG        VSHEFT
028000970110     C*
028100970110     C     VCCDEF        IFEQ      'SI'
028200970110     C     VSHFAP        ANDEQ     *BLANKS
028300970110     C     VSCSTM        IFEQ      'SI'
028400970110     C     VSCCOD        OREQ      'SI'
028500970110     C     VSCLDS        OREQ      'SI'
028600970110     C                   SETON                                        22
028700970110     C                   END
028800970110     C                   END
028900970110     C*
029000970108     C                   END
029100970108     C                   ADD       1             NRR1
029200970108     C                   WRITE     TC82S01
029300970108     C                   ADD       1             II
029400970108     C                   ENDDO
029500970218     C* CMR SEMPRE OBBLIGATORIO
029600020808     C*  Accendo indicatore di posizionamento cursore su Data CMR
029700970115     C                   SETON                                        46
029800970115     C*
029900970109     C                   SETON                                        23
030000970108     C                   Z-ADD     1             V1CNBR
030100020808     C                   MOVE      *BLANKS       WFIRST
030200960530     C*
030300960530     C                   ENDSR
030400961111     C***************************************************************
030500970108     C*  CAMBIATO CODICE PARTNER:REIMPOSTO CAMPI VIDEO              *
030600961111     C***************************************************************
030700961111     C     SRCPT         BEGSR
030800961111     C*
030900020808     C                   Z-ADD     KSC(XY)       VSCCPT
031000970108     C                   Z-ADD     VSCCPT        VSHCPT
031100961129     C* DECODIFICO COD. PARTNER DA P.D.C.
031200961129     C     KACO          CHAIN     CNACO00F                           31
031300970108     C  N31              MOVEL     ACORAG        VSDCPT
031400970108     C   31              CLEAR                   VSDCPT
031500961129     C*
031600020808     C                   MOVEL     DPT(XY)       EDIDSPT
031700020808     C                   MOVEL     DPU(XY)       EDIDSPU
031800970109     C* MEMORIZZO IN CAMPI HIDDEN
031900980320     C                   MOVEL     §PUBS1        VSHTRD
032000120117     C**** La lunghezza della nostra spedizione deve scomparire sostituita solo
032100120117     C*******   dal riferimento presente nell'AGE. La nostra bolla si identifica con
032200120117     C*******   16 o 14 caratteri.
032300120117     C*******   i 2 campi CNI e AGE hanno sempre riportato lo stesso riferimento
032400120117     C*******   quindi verrà eliminato quello inerente al CNI.
032500120117     C*******   Rimane un po' strano il discorso del Secolo.....sul CNI......
032600120117     C                   Z-ADD     §PTAGE        VSHNSP
032700970109     C                   Z-ADD     §PTAGE        VSHAGE
032800970109     C                   MOVEL     §PUNSA        VSHNSA
032900970109     C                   MOVEL     §PUSEC        VSHSEC
033000970109     C                   MOVEL     §PTFTT        VSHFTT
033100970109     C                   MOVEL     §PUSPC        VSHSPC
033200970109     C                   Z-ADD     §PTLNP        VSHLNP
033300970218     C                   MOVEL     §PTCM2        VSHCM2
033400980219     C                   MOVEL     §PUBS1        VSHTSC
033500050525     C                   MOVEL     §PUvma        VSHvma
033600060913     C                   MOVEL     §PUisc        VSHisc
033700970109     C*
033800970108     C* MEMORIZZO SE C'E' TRAMISSIONE DATI IN EDI
033900970108     C     §PTFTT        IFNE      *BLANKS
034000020808     C                   ADD       1             WSIEDI
034100970108     C                   END
034200961111     C*
034300970108     C* MANIFEST PER IL PARTNER
034400970218     C* SE PROVVISORIO                          --> NO
034500970108     C* SE DEFINITIVO E NO E.D.I.               --> SI
034600970205     C* SE DEFINITIVO E SI E.D.I.               --> SI
034700970108     C     VCCDEF        IFEQ      'NO'
034800970108     C                   MOVE      'NO'          VSCSTM
034900961111     C                   ELSE
035000970108     C                   MOVE      'SI'          VSCSTM
035100961112     C                   END
035200961213     C*
035300970108     C* CERCO NUMERO FAX PARTNER SE ELABORAZIONE DEFINITIVA
035400970108     C                   CLEAR                   VSHFAP
035500961213     C*
035600970108    1C     VCCDEF        IFEQ      'SI'
035700961213     C     KACO          CHAIN     CNIND00F                           31
035800041020      **
035900961213    2C     *IN31         IFEQ      *OFF
036000041020      **
036100970218     C* CONTROLLO N. FAX PARTNER SOLO SE DEVO INVIARE QUALCOSA VIA FAX
036200970121     C* AL PARTNER
036300041020      **  (adesso si deve considerare anche l'indirizzo e-mail)
036400970121    3C     VSCSTM        IFEQ      'SI'
036500010201     C     §PTFTT        ANDNE     'W'
036600970121     C     VSCLDS        OREQ      'SI'
036700970121     C     VSCCOD        OREQ      'SI'
036800041020      **
036900000405     C                   CLEAR                   TRUL42DS
037000961213     C                   MOVEL     INDTLF        D42FAX
037100961213     C                   CALL      'TRUL42R'
037200000405     C                   PARM                    TRUL42DS
037300041020      **
037400961213    4C     D42ERR        IFEQ      ' '
037500970108     C                   MOVEL     INDTLF        VSHFAP
037600961213    4C                   END
037700041020      **
037800041020      **  Attenzione: l'indirizzo E-mail ha priorità sul nr.FAX
037900041020      **       se c'è prende quello e non segnala l'errore.
038000041020     c                   clear                   trul44ds
038100050913     c                   clear                   trtcm1ds
038200041020      **** cerco solo sulle NOTE poichè è l'e-mail del Partner e non della Dogana
038300041021     c                   eval      d44pgm = 'TRTC82R'
038400041021     c                   move      vsccpt        d44ksc
038500041020     c                   eval      d44apl  = 'C'
038600041118     c                   eval      d44nk1  = '0151' + %editc(VSCCPT:'X')
038700041020     c                   eval      d44tnt  = '86'
038800041020     c                   call      'TRUL44R'
038900041020     c                   parm                    trul44ds
039000050913     c                   parm                    trtcm1ds
039100041020     c                   if        d44err = *blanks
039200041020     C                   MOVEL     'OK'          VSHFAP
039300041020     c                   end
039400041020      **
039500961213   X3C                   ELSE
039600970108     C                   MOVEL     'OK'          VSHFAP
039700961213    3C                   END
039800961216    2C                   END
039900970108    1C                   END
040000961111     C*
040100961111     C                   ENDSR
040200960529     C***************************************************************
040300960529     C*  Gestione prima videata                                     *
040400960529     C***************************************************************
040500960529     C     GESD01        BEGSR
040600970108     C*
040700970108     C*
040800960528     C                   EXFMT     TC82D01
040900961111     C                   SETOFF                                       402829
041000970109     C                   SETOFF                                       4407
041100960529     C** Fine Lavoro
041200960529     C     *INKC         IFEQ      '1'
041300020808     C                   MOVEL     'S'           WFINE
041400960529     C                   GOTO      FINVD1
041500960529     C                   END
041600960529     C* Controlli formato
041700960529     C                   EXSR      CONTR1
041800961111     C   28
041900961111     COR 29              GOTO      FINVD1
042000960530     C* No errori: VID02
042100960530     C                   EXSR      INZ02
042200960529     C*
042300960529     C     FINVD1        ENDSR
042400960530     C***************************************************************
042500960530     C*  Gestione seconda videata                                   *
042600960530     C***************************************************************
042700960530     C     GESD02        BEGSR
042800960530     C*
042900020808     C                   MOVEL     $MSG          WSVMSG
043000970108     C                   WRITE     TC82Z01
043100010509     C                   setoff                                       20
043200010509      * solo se ci sono dei records nel SFL accendo (20)
043300010509     C     NRR1          ifgt      0
043400010509     C                   seton                                        20
043500010509     C                   end
043600010509      * se il SFL è vuoto e nella schiera DPD nel primo elemento c'è una linea
043700010509      * DPD occorre emettere il messaggio di avvertimento e conferma.
043800010509      * e imposto un flag per gestire in segiuto le cose.
043900020808     C                   clear                   soloDPD
044000010509      *  Emissione 2°Video
044100970109     C                   EXFMT     TC82C01
044200010509      *
044300961114     C                   SETOFF                                       434128
044400970109     C                   SETOFF                                       424529
044500970123     C                   SETOFF                                       47
044600960530     C** Fine Lavoro
044700960530     C     *INKC         IFEQ      '1'
044800020808     C                   MOVEL     'S'           WFINE
044900960530     C                   GOTO      FINVD2
045000960530     C                   END
045100960530     C** Ritorno
045200960530     C     *INKL         IFEQ      '1'
045300960530     C                   MOVEL     '1'           WTPVID
045400970108     C                   CLEAR                   WGVDFV
045500970108     C                   CLEAR                   WGVNFV
045600970117     C                   CLEAR                   WWVNFV
045700960530     C                   GOTO      FINVD2
045800960530     C                   END
045900960530     C* Controlli formato
046000960530     C                   EXSR      CONTR2
046100960530     C* Conferma
046200961111     C  N28
046300961111     CANN29*INKF         IFEQ      '1'
046400970127     C*
046500960530     C                   EXSR      SBMP83
046600960530     C                   EXSR      INZ01
046700960530     C                   END
046800960530     C*
046900960530     C     FINVD2        ENDSR
047000960529     C***************************************************************
047100960529     C*  Sottometto elaborazione                                    *
047200960529     C***************************************************************
047300960529     C     SBMP83        BEGSR
047400960529     C*
047500970109     C                   MOVEL     V1CNFV        D82NFV
047600970109     C                   MOVEL     FGVDFV        D82DFV
047700970109     C                   MOVEL     VCCCMR        D82CMR
047800970109     C                   MOVEL     VCCDEF        D82DEF
047900970109     C                   MOVEL     VCCTRM        D82TRM
048000970115     C                   MOVEL     FGVSNP        D82FVB
048100970127     C*
048200010509      * Se non si tratta solo di DPD e il sfl era carico
048300010509     C     SoloDPD       Ifeq      *Blank
048400010509     C     *in20         andeq     *ON
048500010509     C*
048600970127     C* Prima di sottomettere, se elaborazione provvisoria cancello
048700970127     c* eventuali records dal file di work
048800970127     C     D82DEF        IFEQ      'NO'
048900050523     C     KTRE0         SETLL     WFMAN01L
049000050523     C     KTRE0         READE     WFMAN01L                             3231
049100970127     C     *IN31         DOWEQ     *OFF
049200970127     C     *IN32         ANDEQ     *OFF
049300050523     C                   DELETE    TREXP000
049400050523     C     KTRE0         READE     WFMAN01L                               31
049500970127     C                   ENDDO
049600970127     C                   END
049700970109     C*
049800980212     C*
049900970109     C                   Z-ADD     1             V1CNBR
050000970109     C*
050100970109     C     V1CNBR        CHAIN     TC82S01                            30
050200970109     C     *IN30         DOWEQ     *OFF
050300020808     C                   MOVEL     'M'           WMBR
050400970109     C                   MOVE      VSCCPT        WMBR
050500970109     C                   MOVEL     WMBR          D82MBR
050600970109     C                   Z-ADD     VSCCPT        D82CPT
050700970109     C                   MOVEL     VSCSTM        D82STB
050800970109     C                   MOVEL     VSHLNP        D82LNP
050900970109     C                   MOVEL     VSHNAZ        D82NAZ
051000970109     C                   MOVEL     VSHTRD        D82DET
051100970109     C                   Z-ADD     VSHNSP        D82CNI
051200970109     C                   Z-ADD     VSHAGE        D82AGE
051300970109     C                   MOVEL     VSHNSA        D82NSA
051400970109     C                   MOVEL     VSHSEC        D82SEC
051500970109     C                   MOVEL     VSHFTT        D82FTT
051600970109     C                   MOVEL     VSCDGN        D82DGN
051700970109     C                   MOVEL     VSCCOD        D82COD
051800970109     C                   MOVEL     VSHSPC        D82SPC
051900970109     C                   MOVEL     VSCLDS        D82LDS
052000970124     C                   MOVEL     VSHEFT        D82EFT
052100980219     C                   MOVEL     VSHTSC        D82TSC
052200050525     C                   movel     VSHvma        D82vma
052300060913     C                   movel     VSHisc        D82isc
052400050525     c                   clear                   kpjbu
052500960529     C                   MOVEL     TRTC82        KPJBU
052600010509      *
052700961205     C                   MOVEL     'TC83'        KCOAZ
052800041021     c                   if        kcoaz = *blank
052900041021     C                   CALL      'TRTC83C'
053000041021     C                   PARM                    KPJBA
053100041021     c                   else
053200041021     C*
053300900523     C                   CALL      'BCH10'
053400900523     C                   PARM                    KPJBA
053500041021     c                   end
053600970109     C*
053700970109     C                   ADD       1             V1CNBR
053800970109     C     V1CNBR        CHAIN     TC82S01                            30
053900970109     C                   ENDDO
054000900522     C*
054100010509     C                   ELSE
054200010509     C*
054300010509     C* Se si tratta solo di un foglio con linee Dpd devo chiuderlo
054400010509     C*  senza fare tutti i passaggi che il Manifest fa normalmente.
054500010509     C     SoloDPD       Ifeq      'S'
054600010509     C*
054700010509     C* Attenzione utilizza il campo MEMBRO per pilotare il pgm
054800010509     C*  TRTC83R4 in sola chiusura del foglio viaggio.
054900010509     C                   MOVEL     'SOLO_X_DPD'  D82MBR
055000050525     C                   MOVEL     kpjbu         KPJBUs
055100050525     C                   clear                   KPJBU
055200010509     C                   MOVEL     TRTC82        KPJBU
055300010509     C                   CALL      'TRTC83R4'
055400010509     C                   PARM                    KPJBA
055500050525     C                   MOVEL     kpjbuS        KPJBU
055600010509     C*
055700010509     C                   Endif
055800010509     C*
055900010509     C                   END
056000010509     C*
056100010509     C                   ENDSR
056200960529     C***************************************************************
056300960530     C*  Controlli prima videata                                    *
056400960529     C***************************************************************
056500960529     C     CONTR1        BEGSR
056600960529     C*
056700961111     C* CONTROLLI NUMERO FOGLIO VIAGGIO
056800960529     C* Ricerca numero foglio
056900960529     C     '?'           SCAN      V1CNFV                                 32
057000970123    1C     *IN32         IFEQ      '1'
057100931229     C                   RESET                   PARAM5
057200000614     C                   Z-ADD     SIMFEL        PA5FGS
057300050525     C                   MOVEL     kpjbu         KPJBUs
057400050525     c                   clear                   kpjbu
057500931229     C                   MOVEL     PARAM5        KPJBU
057600951108     C                   CALL      'FNLS53R'
057700931229     C                   PARM                    KPJBA
057800931229     C                   MOVEL     KPJBU         PARAM5
057900050525     C                   MOVEL     kpjbus        KPJBU
058000960529     C                   MOVEL     PA5NFV        V1CNFV
058100961111     C                   SETON                                        29
058200961111     C                   CLEAR                   $MSG
058300961111     C                   GOTO      ENDCTR
058400970123    1C                   ENDIF
058500960529     C* Controllo valdità numerica
058600960529     C                   TESTN                   V1CNFV               31
058700970123    1C     *IN31         IFEQ      *ON
058800020808     C                   MOVE      V1CNFV        W001A
058900970123    2C     W001A         IFLT      '0'
059000961111     C                   SETOFF                                       31
059100970123    2C                   END
059200970123    1C                   END
059300961111     C*
059400970123    1C     *IN31         IFEQ      *OFF
059500961111     C                   MOVEL     ERR(4)        $MSG
059600961111     C                   SETON                                        2840
059700961111     C                   GOTO      ENDCTR
059800970123    1C                   END
059900960529     C* Numero foglio viaggio esiste
060000960529     C                   MOVEL     V1CNFV        KNFV
060100020808     C     K02FGV01      CHAIN     FNFGV01L                           30
060200970123    1C     *IN30         IFEQ      '1'
060300960529     C     *IN30         OREQ      '0'
060400931229     C     FGVATB        ANDNE     *BLANKS
060500960529     C                   SETON                                        4028
060600960529     C                   MOVEL     ERR(1)        $MSG
060700931229     C                   GOTO      ENDCTR
060800970123    1C                   ENDIF
060900961212     C* Il foglio viaggio deve essere di defluenza
061000990301     C                   MOVEL     'TV'          COD
061100990301     C                   MOVEL(P)  FGVTTR        KEY
061200990301     C     KTABE         CHAIN     TABEL                              83
061300990301     C     *IN83         IFEQ      *OFF
061400990301     C     TBLFLG        ANDEQ     ' '
061500990301     C                   MOVEL     TBLUNI        DSTV
061600990301     C                   ELSE
061700990301     C                   CLEAR                   DSTV
061800990301     C                   ENDIF
061900990301    1C     §TVDEF        IFNE      'S'
062000961212     C                   SETON                                        4028
062100961212     C                   MOVEL     ERR(19)       $MSG
062200961212     C                   GOTO      ENDCTR
062300970123    1C                   ENDIF
062400960529     C* Già  chiuso
062500970123    1C     FGVFCF        IFNE      ' '
062600961111     C                   SETON                                        4028
062700960529     C                   MOVEL     ERR(2)        $MSG
062800931229     C                   GOTO      ENDCTR
062900970123    1C                   ENDIF
063000970123     C* Esistono delle bolle borderizzate --> ERRORE
063100020808     C     K02BLP79      SETLL     FNBLP79L                               30
063200970123    1C     *IN30         IFEQ      *ON
063300960529     C                   SETON                                        4028
063400960529     C                   MOVEL     ERR(3)        $MSG
063500960529     C                   GOTO      ENDCTR
063600970123    1C                   END
063700970108     C* Se non è un bis non deve esserci un bis aperto con data <=
063800970123    1C     FGVSNP        IFEQ      *BLANKS
063900020808     C     SIMfel        setll     FNFGV06L
064000020808     C     SIMfel        reade     FNFGV06L                               30
064100970123    2C     *IN30         DOWEQ     *OFF
064200020808    3C                   IF            F6Vatb =  *blanks
064300020808     C                             and F6Vlna =  FGVlna
064400020808     C                             and F6Vsnp =  'B'
064500020808     C                             and F6Vdfv <= FGVdfv
064600970107     C                   SETON                                        4028
064700970107     C                   MOVEL     ERR(21)       $MSG
064800970107     C                   GOTO      ENDCTR
064900970123    3C                   END
065000020808     C     SIMfel        reade     FNFGV06L                               30
065100970123    2C                   ENDDO
065200970123   X1C                   ELSE
065300970109     C                   SETON                                        07
065400970123    1C                   END
065500000405      *
065600000405      * Le linee DPD vanno escluse dall'elaborazione del manifest
065700051223      * ?DAL 9/1/2006 non è più vero anzi si dovrà eseguire il manifest
065800051223      * ? x inviare gli SCAN con il Manifest anzichè dal FIEU25R e
065900051223      * ?  anche x eseguire l'EDI in parallelo x consegnare in Spagna
066000051223      * ?   tramite SEUR (Partner DPD spagnolo)
066100000405     C                   clear                   WSffv
066200020808     C                   clear                   XX
066300020808     C                   clear                   YY
066400000405     C                   eval      II = 1
066500000405      *
066600000405    1C                   DOW       FFV(II) > *zeros
066700000405     C                   move      FFV(II)       WFFV
066800000405     C     Wffv          chain     Azorg01l
066900000405      *
067000000405     C                   IF        not %found
067100020225     c                   clear                   og143
067200020225     C                   ELSE
067300020225     C                   movel     ORGDE3        OG143
067400020225     C                   ENDIF
067500020225      *
067600000405     c                   add       1             XX
067700000405     C                   eval      wsFFV(xx) = FFV(II)
067800051223      *
067900000405     c                   add       1             II
068000000405     C                   ENDDO
068100000405     C                   eval      FFV = wsFFV
068200051124     C*
068300051124     C* Solo se in provvisorio e si trattava di un foglio con linea DPD
068400051124     c                   if        V1CDEF = 'NO' and dpdFFV(1) <> ' '
068500051124     C                   eval      FFV = dpdFFV
068600051124     c                   end
068700970108     C*
068800970108     C* CONTROLLO ELABORAZIONE PER DOGANA (SI/NO)
068900970117     C*
069000970120     C                   CLEAR                   D82DGX
069100970120     C                   CLEAR                   D82RSX
069200970120     C                   CLEAR                   D82CAP
069300970120     C                   CLEAR                   D82LOC
069400970120     C                   CLEAR                   D82PRO
069500970108     C                   Z-ADD     1             II
069600970117     C                   CLEAR                   WERR
069700970117     C                   CLEAR                   WDOGSI
069800970117     C                   CLEAR                   WDOGNO
069900970123    1C     FFV(II)       DOWGT     *ZEROS
070000970117     C     WERR          ANDEQ     *BLANKS
070100020808     C                   Z-ADD     1             XY
070200020808     C                   MOVE      FFV(II)       WFFV
070300020808     C     WFFV          LOOKUP    LNA(XY)                                32
070400970123    2C     *IN32         IFEQ      '1'
070500020808     C                   MOVEL     DPT(XY)       EDIDSPT
070600970108     C                   MOVEL     '15'          COD
070700970108     C                   MOVEL(P)  §PTNAZ        KEY
070800970108     C     KTABE         CHAIN     TABEL00F                           30
070900970108     C  N30              MOVEL     TBLUNI        DS15
071000970108     C   30              CLEAR                   DS15
071100970123     C* SE LNA CON ATTRAVERSAMENTO DOGANA:
071200010405    3C     §15dog        IFEQ      '1'
071300010405     C     §15dog        OREQ      '2'
071400970123    4C     V1CDOG        IFEQ      'NO'
071500020808     C                   MOVE      '1'           WDOGSI
071600970123    4C                   END
071700041020      **
071800970108     C* CERCO NUMERO FAX DOGANA
071900970123    4C     D82DGX        IFEQ      *BLANKS
072000041020      **
072100041020     C*  Se non c'è il nr.FAX:
072200041020     C* Attenzione: adesso può esserci l'indirizzo E-mail che è più importante e ha
072300041020     C*  priorità di invio sul nr.FAX ossia: se ho l'E-mail spedisco tramite
072400041020     C*   E-mail e non faccio il FAX.
072500041020     C                   clear                   trasm_mail        2
072600041020      **
072700041020     c                   clear                   trul44ds
072800050913     c                   clear                   trtcm1ds
072900041020      **** cerco sui LUOGHI poichè è l'e-mail della Dogana e non del Partner
073000041020     c                   eval      d44pgm  = 'TRTC82R'
073100041025     c                   eval      d44cl   = '200'
073200041020     c                   move      KSC(XY)       d44ksc
073300041020     c                   call      'TRUL44R'
073400041020     c                   parm                    trul44ds
073500050913     c                   parm                    trtcm1ds
073600041020     c                   if        d44err = *blanks
073700041020     c                   move      'OK'          trasm_mail
073800041020     c                   end
073900041020      **
074000970806     C                   MOVEL     'L'           KSFLS
074100970806     C     KSPE          CHAIN     FNSPE01L                           31
074200970123    5C     *IN31         IFEQ      *OFF
074300000405     C                   CLEAR                   TRUL42DS
074400970807     C                   MOVEL(P)  SPEFAX        D42FAX
074500970108     C                   CALL      'TRUL42R'
074600000405     C                   PARM                    TRUL42DS
074700970108    6C     D42ERR        IFEQ      ' '
074800970807     C                   MOVEL     SPEFAX        D82DGX
074900970120     C                   MOVEL     SPERAG        D82RSX
075000970807     C                   MOVEL     SPECAP        D82CAP
075100970120     C                   MOVEL     SPELOC        D82LOC
075200970120     C                   MOVEL     SPEPRO        D82PRO
075300970108    6C                   END
075400041020      **
075500970108    5C                   END
075600970123    4C                   END
075700970123   X3C                   ELSE
075800970123     C* LNA SENZA ATTRAVERSAMENTO DOGANA
075900970123    4C     V1CDOG        IFEQ      'NO'
076000020808     C                   MOVE      '1'           WDOGNO
076100970123   X4C                   ELSE
076200020808     C                   MOVE      'E'           WERR
076300970123     C                   SETON                                        4428
076400970123     C                   MOVEA     ERR(22)       K78
076500020808     C                   MOVE      FFV(II)       WFFVA
076600970123     C                   MOVEA     WFFVA         K78(41)
076700970123     C                   MOVEA     K78           $MSG
076800970123     C                   GOTO      ENDCTR
076900970123    4C                   END
077000970123    3C                   END
077100970123   X2C                   ELSE
077200970117     C* LINEA ESTERA NON PRESENTE IN TABELLA
077300970117     C                   MOVE      'E'           WERR
077400970117     C                   SETON                                        28
077500970117     C                   MOVEA     ERR(25)       K78
077600020808     C                   MOVE      FFV(II)       WFFVA
077700970117     C                   MOVEA     WFFVA         K78(19)
077800970117     C                   MOVEA     K78           $MSG
077900970117     C                   GOTO      ENDCTR
078000970123    2C                   END
078100970108     C                   ADD       1             II
078200970123    1C                   ENDDO
078300041020     C*
078400970117     C* ERRORE FORZABILE SE NUMERO FAX DOGANA ERRATO E RICHIESTA
078500970117     C* ELABORAZIONE PER DOGANA
078600041020      * adesso se non c'è nemmeno l'indirizzo e-mail
078700970117     C     V1CDOG        IFEQ      'SI'
078800970120     C     D82DGX        ANDEQ     *BLANKS
078900041020     C     trasm_mail    andeq     *blanks
079000970117     C     FGVNFV        ANDNE     WGVNFV
079100970117     C                   MOVE      FGVNFV        WGVNFV
079200970121     C                   SETON                                        28
079300970117     C                   MOVEL     ERR(23)       $MSG
079400970117     C                   GOTO      ENDCTR
079500970117     C                   END
079600970117     C*
079700970117     C* ERRORE FORZABILE SE ALCUNE LINEE CON DOGANA ED ALTRE SENZA
079800970117     C     V1CDOG        IFEQ      'NO'
079900970117     C     WDOGSI        ANDNE     *BLANKS
080000970117     C     WDOGNO        ANDNE     *BLANKS
080100970117     C     FGVNFV        ANDNE     WWVNFV
080200970117     C                   MOVE      FGVNFV        WWVNFV
080300970121     C                   SETON                                        28
080400970117     C                   MOVEL     ERR(26)       $MSG
080500970117     C                   GOTO      ENDCTR
080600970117     C                   END
080700970108     C*
080800961212     C* ERRORE FORZABILE: SE DATA FOGLIO < DATA DI SISTEMA LI AVVERTO
080900970108     C*
081000961212     C     FGVDFV        IFNE      WGVDFV
081100961212     C                   MOVE      FGVDFV        WGVDFV
081200961212     C     FGVDFV        IFLT      DATEU
081300961212     C                   SETON                                        4028
081400961212     C                   MOVEL     ERR(18)       $MSG
081500961212     C                   GOTO      ENDCTR
081600961212     C                   END
081700961212     C                   END
081800970123     C* MEMORIZZO SE OCCORRE INVIARE VIA FAX TOTALI PER DOGANA
081900970127     C     V1CDOG        IFEQ      'SI'
082000970218     C                   MOVE      'F'           D82TFF
082100970123     C                   END
082200960530     C*
082300960530     C     ENDCTR        ENDSR
082400960530     C***************************************************************
082500960530     C*  Controlli seconda videata                                  *
082600960530     C***************************************************************
082700960530     C     CONTR2        BEGSR
082800960530     C*
082900970115     C                   SETOFF                                       46
083000010509     C*  Esegue la routine di controlli solo se ci sono dei records
083100010509     C*  nel SFL. Se per esempio il foglio Viaggio è fatto solo su
083200010509     C*  linee DpD, il SFL è sicuramente vuoto.
083300010509     C     *in20         Ifeq      *on
083400010509     C*
083500970108     C* CONTROLLI CAMPI DEL SUBFILE
083600970110     C                   SETON                                        22
083700970110     C                   READC     TC82S01                                31
083800970110    1C     *IN31         DOWEQ     *OFF
083900970123     C     VSCDGN        IFEQ      'SI'
084000970218     C                   MOVE      'F'           D82TFF
084100970123     C                   END
084200970108     C*  Controllo che codice partner > 0
084300970110    2C     VSCCPT        IFEQ      0
084400970109     C                   CLEAR                   VSHCPT
084500970110     C                   CLEAR                   VSDCPT
084600970108     C                   SETON                                        4328
084700970108     C                   MOVEL     ERR(10)       $MSG
084800970108     C                   Z-ADD     NRR1          V1CNBR
084900970108     C                   UPDATE    TC82S01
085000970108     C                   GOTO      ENDCT2
085100970110    2C                   END
085200970108     C* Se ho modificato il codice del Partner controllo se congruente
085300970108     C* con lna
085400970110    2C     VSCCPT        IFNE      VSHCPT
085500020808     C                   Z-ADD     1             XY
085600020808     C     VSCCPT        LOOKUP    KSC(XY)                                32
085700970110    3C     *IN32         IFEQ      '1'
085800020808    4C     VSCLNA        IFNE      LNA(XY)
085900970110     C                   CLEAR                   VSDCPT
086000970110     C                   CLEAR                   VSHCPT
086100970108     C                   SETON                                        4328
086200970108     C                   MOVEL     ERR(6)        $MSG
086300970108     C                   Z-ADD     NRR1          V1CNBR
086400970108     C                   UPDATE    TC82S01
086500970108     C                   GOTO      ENDCT2
086600970110    4C                   END
086700970110   X3C                   ELSE
086800970110     C                   CLEAR                   VSDCPT
086900970110     C                   CLEAR                   VSHCPT
087000970108     C                   SETON                                        4328
087100970108     C                   MOVEL     ERR(11)       $MSG
087200970108     C                   Z-ADD     NRR1          V1CNBR
087300970108     C                   UPDATE    TC82S01
087400970108     C                   GOTO      ENDCT2
087500970110    3C                   END
087600970108     c* reimposto campi ed indicatori in base al nuovo cod.partner
087700970110    3C     VSHFTT        IFNE      *BLANKS
087800970108     C                   SUB       1             WSIEDI
087900970110    3C                   END
088000970108     C                   EXSR      SRCPT
088100970110     C*
088200970110     C* MEMORIZZO SE DEVO VISUALIZZARE MESSAGGIO FORZABILE CHE N.FAX
088300970110     C* PARTNER ERRATO
088400970110    3C     VSHFAP        IFEQ      *BLANKS
088500970110     C     VCCDEF        ANDEQ     'SI'
088600970110     C     SAVNR1        ANDEQ     *ZEROS
088700970110    4C     VSCSTM        IFEQ      'SI'
088800970110     C     VSCCOD        OREQ      'SI'
088900970110     C     VSCLDS        OREQ      'SI'
089000970110     C                   Z-ADD     NRR1          SAVNR1
089100970110    4C                   END
089200970110   X3C                   ELSE
089300970110     C* SE NUMERO FAX CORRETTO CONTROLLO DI NON AVER MEMORIZZATO IN
089400970110     C* UN GIRO PRECEDENTE CHE IL FAX E' ERRATO ED IN QUESTO CASO
089500970110     C* PULISCO I CAMPI DI COMODO
089600970110    4C     SAVNR1        IFEQ      NRR1
089700970110     C                   CLEAR                   SAVNR1
089800970110     C                   MOVE      *BLANKS       WFIRST
089900970110    4C                   END
090000970110    3C                   END
090100970110     C*
090200970108     C                   SETON                                        29
090300970108     C                   Z-ADD     NRR1          V1CNBR
090400970108     C                   UPDATE    TC82S01
090500970110   X2C                   ELSE
090600970110     C* SOLO LA PRIMA VOLTA E NON E' STATO MODIFICATO IL COD. PARTNER:
090700970108     C* se elaborazione definitiva memorizzo se occorre inviare msg
090800970108     C* forzabile se numero fax partner errato
090900970110    3C     VCCDEF        IFEQ      'SI'
091000970108     C     VSHFAP        ANDEQ     *BLANKS
091100970109     C     SAVNR1        ANDEQ     *ZEROS
091200970110     C     WFIRST        ANDEQ     *BLANKS
091300970110    4C     VSCSTM        IFEQ      'SI'
091400970108     C     VSCCOD        OREQ      'SI'
091500970108     C     VSCLDS        OREQ      'SI'
091600970108     C                   Z-ADD     NRR1          SAVNR1
091700970110     C                   MOVE      '1'           WFIRST
091800970110    4C                   END
091900970110    3C                   END
092000970110    2C                   END
092100970123     C* SE ELABORAZIONE PER DOGANA SI E NON C'E ATTRAVERSAM. DOGANA ERR
092200970123     C     VSCDGN        IFEQ      'SI'
092300970123     C     VSHEFT        ANDEQ     *BLANKS
092400970123     C                   SETON                                        4728
092500970123     C                   MOVEA     ERR(22)       K78
092600020808     C                   MOVE      VSCLNA        WFFVA
092700970123     C                   MOVEA     WFFVA         K78(41)
092800970123     C                   MOVEA     K78           $MSG
092900970123     C                   Z-ADD     NRR1          V1CNBR
093000970123     C                   UPDATE    TC82S01
093100970123     C                   GOTO      ENDCT2
093200970123     C                   END
093300970110     C                   READC     TC82S01                                31
093400970110    1C                   ENDDO
093500010509      *
093600010509      * se il SFL era carico
093700010509    1C                   EndIF
093800010509      *
093900970108     C* CONTROLLO CAMPI DEL SUBFILE CONTROL
094000970108     C* Se data CMR > 0 controllo che sia corretta
094100970108     C     VCCDAC        IFGT      *ZEROS
094200970108     C                   CLEAR                   WLBDA8
094300970108     C                   MOVE      VCCDAC        G02DAT
094400970108     C                   CALL      'XSRDA8'
094500970108     C                   PARM                    WLBDA8
094600970109     C                   MOVE      G02INV        D82DCM
094700020808     C                   MOVEL     G02INV        anno40
094800970108     C                   MOVE      G02DAT        VCCDAC
094900040520     C     *ymd          move      g02inv        dataCMR
095000040520     C     dataOGGi      subdur    7:*d          dataISO
095100040520     C     dataOGGi      adddur    7:*d          dataISO1
095200040520     c
095300970108     C     G02ERR        IFEQ      '1'
095400010611     C     anno40        orLT      daanno
095500010611     C     anno40        orGT      adanno
095600040520     C     dataCMR       orLT      dataISO
095700040520     C     dataCMR       orgT      dataISO1
095800970108     C                   SETON                                        4228
095900970108     C                   MOVEL     ERR(9)        $MSG
096000970108     C                   GOTO      ENDCT2
096100970108     C                   END
096200970108     C                   END
096300970108     C* Se CMR obbligatorio controllo che data e numero siano stati im-
096400970108     C* messi
096500970109     C* data CMR
096600970109     C     VCCDAC        IFEQ      *ZEROS
096700970109     C                   SETON                                        4228
096800970109     C                   MOVEL     ERR(8)        $MSG
096900970109     C                   GOTO      ENDCT2
097000970109     C                   END
097100970108     c* numero CMR
097200970108     C     VCCCMR        IFEQ      *BLANKS
097300970108     C     VCCCMR        OREQ      *ZEROS
097400970108     C                   SETON                                        4128
097500970108     C                   MOVEL     ERR(7)        $MSG
097600970108     C                   GOTO      ENDCT2
097700970108     C                   END
097800970218     C* Targa motrice
097900970218     C     VCCTRM        IFEQ      *BLANKS
098000970218     C                   SETON                                        2845
098100970218     C                   MOVEL     ERR(27)       $MSG
098200970218     C                   GOTO      ENDCT2
098300970218     C                   END
098400961204     C*
098500970108     C* SE ELABORAZIONE DEFINITIVA E NON HO NUMERO FAX PARTNER INVIO
098600970108     C* MESSAGGIO FORZABILE
098700010509     C     *in20         Ifeq      *on
098800010509      *
098900970109     C     SAVNR1        IFGT      *ZEROS
099000970109     C     SAVNR1        CHAIN     TC82S01                            30
099100970109     C                   Z-ADD     SAVNR1        V1CNBR
099200970110     C                   CLEAR                   SAVNR1
099300961213     C                   SETON                                        28
099400961213     C                   MOVEL     ERR(20)       $MSG
099500970109     C                   UPDATE    TC82S01
099600961213     C                   GOTO      ENDCT2
099700010509     C                   END
099800010509     C*
099900010509     C                   End
100000961204     C*
100100961022     C* Controllo se devo avvisare x elaborazione definitiva
100200970108     C     WSIEDI        IFGT      *ZEROS
100300970108     C     VCCDEF        ANDEQ     'SI'
100400961023     C     WSVMSG        ANDNE     ERR(12)
100500961022     C                   SETON                                        28
100600961022     C                   MOVEL     ERR(12)       $MSG
100700961022     C                   END
100800960530     C*
100900970110     C     ENDCT2        ENDSR
101000000405      *---------------------------------------------------------------*
101100000405      * *INZSR: Operazioni iniziali                                  -*
101200000405      *---------------------------------------------------------------*
101300960529     C     *INZSR        BEGSR
101400960529     C*
101500960529     C     *ENTRY        PLIST
101600960529     C                   PARM                    KPJBA
101700960529     C* Richiamo XPARUT
101800960529     C                   Z-ADD     1             CODUT
101900960529     C                   CALL      'X§PARUT'
102000000405     C                   PARM                    UT§DSE0F
102100960529     C                   MOVEL     REC80         CNCR80
102200960529     C                   MOVEL     RAGUT         RSUT             20
102300020920      *
102400020920     C                   MOVEL     kpjbu         KPJBUs
102500050525     c                   clear                   kpjbu
102600120821      **
102700120821     c* Carico x terminal  di arrivo
102800120821     c                   clear                   trul09ds
102900120821     c                   movel     simPOU        i09tfa
103000120821     c                   movel     'TFA'         i09mod
103100120821     c                   call      'TRUL09R'
103200120821     c                   parm                    trul09ds
103300120821     c
103400020920     C                   MOVEL     kpjbus        KPJBU
103500020920      *
103600121102      *
103700961129     C*--- RICERCA CAPOCONTI------------------------------------------*
103800020808     C                   DO        50            XY
103900020808     C                   MOVE      TCU(XY)       TCUDS
104000961129     C     F56           IFEQ      'CG'
104100961129     C     F4            ANDEQ     '1'
104200961129     C     F3            ANDEQ     '0'
104300020808     C                   Z-ADD     KCU(XY)       KCI
104400961129     C                   END
104500961129     C                   END
104600960529     C*
104700960529     C* Definizione chiavi di accesso
104800961112     C     KTABE         KLIST
104900961112     C                   KFLD                    CODUT
105000961112     C                   KFLD                    COD
105100961112     C                   KFLD                    KEY
105200961129     C     KACO          KLIST
105300961129     C                   KFLD                    CODUT
105400961129     C                   KFLD                    KCI
105500970108     C                   KFLD                    VSCCPT
105600961216     C     KSPE          KLIST
105700970806     C                   KFLD                    KSFLS
105800020808     C                   KFLD                    KSC(XY)
105900961216     C                   KFLD                    KSCOD
106000970527     C     KTRE0         KLIST
106100970127     C                   KFLD                    D82NFV
106200970127     C                   KFLD                    SIMFEL
106300970127     C                   KFLD                    WTRC
106400020808     C     K02BLP79      klist
106500020808     C                   kfld                    SIMfel
106600020808     C                   kfld                    KNFV
106700020808     C     K02FGV01      klist
106800020808     C                   kfld                    KNFV
106900020808     C                   kfld                    SIMfel
107000020808     C     K02FGW01      klist
107100020808     C                   kfld                    FGVnfv
107200020808     C                   kfld                    FGVlnp
107300960529     C*
107400960529     C* Carico dati DS partner in schiera
107500020808     C                   Z-ADD     1             XY
107600960529     C                   MOVEL     'PT'          KCOD
107700961114     C     KCOD          CHAIN     EDTAB01L                           30
107800960529     C     *IN30         DOWEQ     '0'
107900960529     C     TABFLG        IFEQ      ' '
108000020920     C                   MOVEL     TABUNI        EDIDSPT
108100020920      * solo se compresa nelle linee di partenza del terminal
108200020920     c     §ptlnp        lookup    Lin                                    32
108300020920     c                   if        *in32 = *on
108400020808     C                   ADD       1             XY
108500020808     C                   MOVEL     TABKEY        CPU(XY)
108600020808     C                   MOVEL     TABKEY        CPT(XY)
108700020808     C                   MOVE      '    '        CPT(XY)
108800020808     C                   MOVEL     TABUNI        DPT(XY)
108900020808     C                   MOVEL     §PTKSC        KSC(XY)
109000020808     C                   MOVEL     §PTLNP        LNA(XY)
109100020920     c                   endif
109200960529     C                   END
109300961114     C     KCOD          READE     EDTAB01L                               30
109400960529     C                   END
109500121102      *
109600121102      * Controlla riempimento della PT x limiti di schiera
109700121102      *  se deve inviare la mail di alert x limite quasi raggiunto.
109800121102     c                   exsr      ChecK_PT
109900960906     C*
110000960906     C                   MOVEL     'PU'          KCOD
110100961114     C     KCOD          CHAIN     EDTAB01L                           30
110200960906     C     *IN30         DOWEQ     '0'
110300960906     C     TABFLG        IFEQ      ' '
110400020808     C                   Z-ADD     1             XY
110500020808     C     TABKEY        LOOKUP    CPU(XY)                                32
110600960906     C     *IN32         IFEQ      '1'
110700020808     C                   MOVEL     TABUNI        DPU(XY)
110800000405     C                   MOVEL     TABUNI        EDIDSPU
110900961023     C* Se no partner metto zero in LNA
111000051124      *** Attenzione se è una Linea DPD deve lasciarla
111100051124     c     lna(xy)       chain     azorg01l
111200051124     c                   if        %Found(azorg01l)
111300051124     c                   move      orgde3        og143
111400051124     c                   end
111500051124      ***   pulisce la linea se è un cliente e non è un DPD
111600110913     C     §PUTPC        IFne      'P'
111700051124     c                   if        §ogntw <> 'DPD'
111800020808     C                   Z-ADD     0             LNA(XY)
111900051124     c                   end
112000961023     C                   END
112100051124      ***
112200960906     C                   END
112300960906     C                   END
112400961114     C     KCOD          READE     EDTAB01L                               30
112500960906     C                   END
112600961121     C*
112700961121     C* REPERISCO DATA DEL SISTEMA
112800020808     C                   TIME                    WTIME                          ORA E DATA
112900020808     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
113000020808     C                   MOVE      Wdate         anno                           AAAA
113100020808     C     anno          sub       1             daanno                         AAAA
113200020808     C     anno          add       1             adanno                         AAAA
113300961121     C                   Z-ADD     WDATE         G02DAT
113400961121     C                   Z-ADD     *ZERO         G02INV
113500961121     C                   MOVEL     *BLANKS       G02ERR
113600961121     C                   CALL      'XSRDA8'
113700961121     C                   PARM                    WLBDA8
113800020808     C                   Z-ADD     G02INV        DATEU
113900040520     c     *ymd          move      dateu         dataoggi
114000960529     C*
114100960529     C                   ENDSR
114200121102      *---------------------------------------------------------------*
114300121102      * *INZSR: Operazioni iniziali                                  -*
114400121102      *---------------------------------------------------------------*
114500121102     C     Check_PT      begsr
114600121102     C*
114700121102     c* Controllo riempimento schiera
114800121102     c                   clear                   trul0sds
114900121102     c                   eval      i0sski='CPT'
115000121102     c                   eval      i0sele=%elem(CPT)
115100121102     c                   eval      i0spie=xy
115200121102     c                   eval      i0sfile='EDTAB00F'
115300121102     c                   eval      i0ssif=knsif
115400121102     c                   eval      i0spgm='TRTC82R'
115500121113     C                   MOVEL     kpjbu         KPJBUs
115600121113     C                   clear                   KPJBU
115700121102     c                   movel     trul0sds      kpjbu
115800121102     c                   call      'TRUL0SR'
115900121102     c                   parm                    kpjba
116000121113     C                   MOVEL     kpjbuS        KPJBU
116100121102     C*
116200121102     C                   ENDSR
116300970218**
116400961111Foglio Errato o Annullato                                                     01
116500961111Foglio già chiuso                                                             02
116600961111Presenti bolle borderizzate: un foglio per l'estero non contiene bolle        03
116700961111Numero foglio viaggio presenta dei caratteri non validi                       04
116800961111                                                                              05
116900961111Codice Partner non congruente con linea di arrivo                             06
117000961111Numero CMR obbligatorio                                                       07
117100961111Data CMR obbligatoria                                                         08
117200961111Data CMR non valida                                                           09
117300961111Codice Partner obbligatorio                                                   10
117400961111Codice Partner inesistente                                                    11
117500970117ATTENZIONE!!!! Se elab.definitiva si procederà alla scrittura msg.EDI         12
117600961111Numero foglio I.M.A. presenta dei caratteri non validi                        13
117700961111Data foglio IMA (XX/XX/XXXX) diversa da data foglio viaggio (XX/XX/XXXX)      14
117800961111Non esiste nessun collo per l'estero spuntato sull'IMA richiesto.ENTER X FORZ.15
117900961111Non è un inventario arrivi                                                    16
118000961111Numero foglio I.M.A. obbligatorio                                             17
118100961212ATTENZIONE:Richiesto foglio viaggio con data < di data odierna.ENTER PER FORZ.18
118200970121Immettere un foglio viaggio di DEFLUENZA                                      19
118300041020Nr.FAX Partner/Indirizzo E-MAIL ERRATO. trasm. NON POSSIBILE     ENTER X FORZ.20
118400970108Presente foglio viaggio BIS ancora da chiudere per questa linea di arrivo     21
118500970117Elaborazione per dogana='SI' ma per lna xxx non è previsto l'attravers.dogana 22
118600041020Nr.FAX DOGANA/Indirizzo E-MAIL ERRATO. trasm. NON POSSIBILE     ENTER X FORZ. 23
118700970109Targa motrice obbligatoria se elaborazione definitiva o per la dogana         24
118800970117Non trovata linea xxx in tabella partner                                      25
118900970117Tutte le linee di arrivo devono avere o non avere l'attrav.dogana.ENTER X FORZ26
119000970218Targa motrice obbligatoria                                                    27
