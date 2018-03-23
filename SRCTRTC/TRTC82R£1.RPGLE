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
002300050523     F***TREXP01L  UF   E           K DISK
002400050523     FWFMAN01L  UF   E           K DISK
002500000405      *--------------------------------------------------------------*
002600000405      *  SCHIERE                                                     *
002700000405      *--------------------------------------------------------------*
002800961023     D CPU             S             35    DIM(200)                             Cod.partner
002900961023     D CPT             S             35    DIM(200)                             Cod.partner
003000960906     D DPT             S             90    DIM(200)                             Tab.PT
003100960906     D DPU             S             90    DIM(200)                             Tab.PU
003200960906     D KSC             S              7  0 DIM(200)                             Cod.cliente
003300960906     D LNA             S              3  0 DIM(200)                             Lna
003400970218     D ERR             S             78    DIM(27) CTDATA PERRCD(1)             Errore
003500970117     D K78             S              1    DIM(78)                              DI COMODO
003600000405      *
003700000505     D WSFFV           S              3    DIM(301)                             LINEE DEL FV
003800010509     D DpDFFV          S              3    DIM(301)                             LINEE DEL FV
003900000405      *--------------------------------------------------------------*
004000000405      *  DS                                                          *
004100000405      *--------------------------------------------------------------*
004200000405      * PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - TRTC83C -
004300960529     D TRTC82        E DS                  EXTNAME(TRTC82DS)
004400000405      *
004500000405      * PASSAGGIO DATI A INTERROGAZIONE FOGLI V. PARTENZE - FNLS53R -
004600931229     D PARAM5          DS
004700951108     D  PA5FLG                 1      1    INZ('R')
004800951108     D  PA5FL2                 2      2    INZ
004900951108     D  PA5NFV                 3      5P 0 INZ
005000960201     D  PA5DIN                 6     10P 0 INZ
005100951108     D  PA5DFI                11     15P 0 INZ(31129999)
005200951108     D  PA5TFV                16     16    INZ('2')
005300000614     D  PA5FGS               242    244  0
005400000405      *
005500900511     D KPJBA         E DS
005600120821     D**** TRUL06DS      E DS
005700120821     D****  LIN                    1     90  0 DIM(30)                              P.O. COMODO
005800120821     D TRUL09DS      E DS
005900120821     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
006000000405     D UT§DSE0F      E DS
006100000405     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
006200000405     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    PARAM CAPI CONTO
006300961129     D TCUDS           DS
006400961129     D  F3                     3      3
006500961129     D  F4                     4      4
006600961129     D  F56                    5      6
006700960529     D CNCR80        E DS
006800000405     D OG143         E DS
006900000405     D EDIDSPT       E DS
007000000405     D EDIDSPU       E DS
007100961112     D DS15          E DS
007200990301     D DSTV          E DS
007300000405     D TRUL42DS      E DS
007400020808     D*
007500041020     D* ds per ind. email
007600041020     Dtrul44ds       E DS                  INZ
007700050913     D trtcm1ds      E DS
007800041020      *
007900960701     D WLBDA8          DS
008000960701     D  G02DAT                 1      8  0
008100960701     D  G02INV                 9     16  0
008200960701     D  G02ERR                17     17
008300960701     D  G02TGI                18     22  0
008400970108     D* LINEE DI ARRIVO  PER FNFGV00F
008500970108     D                 DS
008600970108     D  FGVLNA                 1      3  0
008700970108     D  FGVFFV                 4    243
008800970108     D  FGVFF2               244    450
008900000505     D  FGWFF3               451    690
009000000505     D  FGWFF4               691    900
009100000505     D  VUOTO                901    903
009200000505     D  FFV                    1    903    DIM(301)                             LINEE DEL FV
009300020808     D*
009400021017     D*PARAM           DS
009500021017     D**PARNFV                 1      5  0
009600021017     D**PARDEF                 6      6
009700020808      *
009800020808      * Definizione variabili
009900020808      *
010000020920     D kpjbus          S                   LIKE(kpjbu)
010100020808     D WTIME           S             14  0                                      ORA E DATA
010200020808     D WDATE           S              8  0                                      GG/MM/AAAA
010300020808     D Anno            S              4  0                                      AAAA
010400020808     D DAanno          S              4  0                                      AAAA
010500020808     D ADanno          S              4  0                                      AAAA
010600020808     D DATEU           S              8  0                                      AAAA/MM/GG
010700020808     D Anno40          S              4  0
010800040520     D DataISO         S               d   DATFMT(*iso)
010900040520     D DataISO1        S               d   DATFMT(*iso)
011000040520     D DataOggi        S               d   DATFMT(*iso)
011100040520     D DataCMR         S               d   DATFMT(*iso)
011200040520     D KCI             S              4  0
011300020808     D II              S              3  0 inz
011400020808     D XX              S              3  0 inz
011500020808     D XY              S              3  0 inz
011600020808     D YY              S              3  0 inz
011700020808     D NRR1            S              5  0 inz
011800020808     D WERR            S              1    inz
011900020808     D WTPVID          S              1    inz
012000020808     D WMBR            S              8    inz
012100020808     D WFFV            S              3  0 inz
012200020808     D WFFVA           S              3    inz
012300020808     D WFIRST          S              1    inz
012400020808     D WSIEDI          S              2  0 inz
012500020808     D WFINE           S              1    inz
012600020808     D WDOGSI          S              1    inz
012700020808     D WDOGNO          S              1    inz
012800020808     D W001A           S              1    inz
012900020808     D soloDPD         S              1    inz
013000020808     D SAVNR1          S                   LIKE(NRR1)   inz
013100020808     D WSVMSG          S                   LIKE($MSG)   inz
013200020808     D KCOD            S                   LIKE(TABCOD) inz
013300020808     D KNFV            S                   LIKE(FGVNFV) inz
013400020808     D COD             S                   LIKE(TBLCOD) inz
013500020808     D KEY             S                   LIKE(TBLKEY) inz
013600020808     D KSCOD           S                   LIKE(SPECOD) inz('200')
013700020808      * ............................... -> 200 - Luogo invio fax a Dogana
013800020808     D KSFLS           S                   LIKE(SPEFLS) inz('L')
013900020808     D WTRC            S                   LIKE(TRETRC) inz('P')
014000020808     D WGVDFV          S                   LIKE(FGVDFV) inz
014100020808     D WGVNFV          S                   LIKE(FGVNFV) inz
014200020808     D WWVNFV          S                   LIKE(FGVNFV) inz
014300020808     ***IFGV04
014400020808     ***I              FGVATB                      F4VATB
014500020808     ***I              FGVNFV                      F4VNFV
014600020808     ***I              FGVDFV                      F4VDFV
014700020808     ***I              FGVLNP                      F4VLNP
014800020808     ***I              FGVFCP                      F4VFCP
014900020808     ***I              FGVPDR                      F4VPDR
015000020808     ***I              FGVDPD                      F4VDPD
015100020808     ***I              FGVCCN                      F4VCCN
015200020808     ***I              FGVDCN                      F4VDCN
015300020808     ***I              FGVTRM                      F4VTRM
015400020808     ***I              FGVTRR                      F4VTRR
015500020808     ***I              FGVTMZ                      F4VTMZ
015600020808     ***I              FGVTTR                      F4VTTR
015700020808     ***I              FGVFRQ                      F4VFRQ
015800020808     ***I              FGVIPT                      F4VIPT
015900020808     ***I              FGVSNS                      F4VSNS
016000020808     ***I              FGVSNP                      F4VSNP
016100020808     ***I              FGVLNA                      F4VLNA
016200020808     ***I              FGVFFV                      F4VFFV
016300020808     ***I              FGVFF2                      F4VFF2
016400020808     ***I              FGVFLP                      F4VFLP
016500020808     ***I              FGVFL2                      F4VFL2
016600020808     ***I              FGVDTP                      F4VDTP
016700020808     ***I              FGVHMP                      F4VHMP
016800020808     ***I              FGVNT1                      F4VNT1
016900020808     ***I              FGVNT2                      F4VNT2
017000020808     ***I              FGVPMB                      F4VPMB
017100020808     ***I              FGVFCF                      F4VFCF
017200020808     ***I              FGVTRN                      F4VTRN
017300020808     ***I              FGVNFA                      F4VNFA
017400020808     ***I              FGVFTR                      F4VFTR
017500020808     ***I              FGVDTR                      F4VDTR
017600000405
017700000405      ***************************************************************
017800000405      *  RIEPILOGO INDICATORI                                       *
017900000405      ***************************************************************
018000000405      **04    - ON --> SPROTEGGO FILIALE GESTIONE FOGLIO IMA        *
018100000405      **05    - OFF--> NUMERO/DATA CMR OBBLIGATORI                  *
018200000405      * 07    - ON=VISUALIZZA 'BIS' SE FV BIS                       *
018300000405      * 28    - Indicatore generico di errore                       *
018400000405      * 29    - Indicatore generico PER RIEMETTERE VIDEATA SENZA ERR*
018500000405      * 30    - Esecuzione Chain/read                               *
018600000405      * 31    - Indicatore di TESTN                                 *
018700000405      * 32    - Indicatore di risposta SCAN                         *
018800000405      * 40    - Errore su foglio viaggio                            *
018900000405      * 41    - ERRORE N. CMR                                       *
019000000405      * 42    - ERRORE DATA CMR                                     *
019100000405      * 43    - Errore su codice partner                            *
019200000405      * 44    - Errore su ELABORAZIONE PER DOGANA (SI/NO)           *
019300000405      * 45    - Errore targa motrice                                *
019400000405      * 46    - POSIZIONAMENTO CURSORE DATA CMR SE OBBLIGATORIA     *
019500000405      * 47    - ERRRORE SU MANIFEST PER DOGANA PER LNA              *
019600000405      ***************************************************************
019700000405
019800000405      *-------------------------------------------------------------*
019900000405      *  CICLO PRINCIPALE                                           *
020000000405      *-------------------------------------------------------------*
020100000405      *  Inizializzazione parametri a video
020200960529     C                   EXSR      INZ01
020300000405      *  Loop gestione prima videata
020400960529     C     WFINE         DOWNE     'S'
020500960530     C     WTPVID        CASEQ     '1'           GESD01
020600960530     C     WTPVID        CASEQ     '2'           GESD02
020700960529     C                   END
020800960530     C                   END
020900000405      *
021000960529     C     FINE          TAG
021100000405     C                   eval       *inlr = *on
021200960529     C***************************************************************
021300960529     C*  Inizializzazione prima videata                             *
021400960529     C***************************************************************
021500960529     C     INZ01         BEGSR
021600960529     C*
021700970218     C                   CLEAR                   TRTC82
021800970108     C                   SETOFF                                       402844
021900960529     C                   MOVEL     *BLANKS       $MSG
022000960529     C                   MOVEL     *BLANKS       V1CNFV
022100961111     C                   MOVE      '?'           V1CNFV
022200961111     C                   MOVEL     'NO'          V1CDEF
022300970107     C                   MOVEL     'NO'          V1CDOG
022400960530     C                   MOVEL     '1'           WTPVID
022500020808     c*
022600020808     c* Imposto il P.O. in gestione
022700020808     c                   z-add     SIMpou        V1Cpog
022800020808     c                   clear                   V1Dpog
022900020808     c     SIMpou        chain     AZORG
023000020808    1c                   if             %found(AZORG01L)
023100020808     c                             and  ORGfva = *blanks
023200020808     c                             and (ORGfag = 'A'
023300020808     c                              or  ORGfag = 'F')
023400020808     c                   movel     ORGdes        V1Dpog
023500020808    1c                   endif
023600960529     C*
023700960529     C                   ENDSR
023800960530     C***************************************************************
023900960530     C*  Inizializzazione seconda videata                           *
024000960530     C***************************************************************
024100960530     C     INZ02         BEGSR
024200960530     C*
024300970108     C                   SETOFF                                       414243
024400970115     C                   SETOFF                                       454628
024500970123     C                   SETOFF                                       47
024600960530     C                   MOVEL     *BLANKS       $MSG
024700020808     C                   MOVEL     '2'           WTPVID
024800970108     C*
024900970108     C                   MOVE      V1CNFV        VCCNFV
025000970108     C                   MOVE      V1CDEF        VCCDEF
025100970108     C                   MOVE      V1CDOG        VCCDOG
025200970108     C*
025300970108     C                   Z-ADD     0             VCCDAC
025400970108     C                   MOVEL     *BLANKS       VCCCMR
025500970109     C                   MOVE      FGVTRM        VCCTRM
025600970108     C* Pulizia subfile
025700970108     C                   SETOFF                                       2123
025800970109     C                   WRITE     TC82C01
025900970108     C                   SETON                                        2021
026000020808     C                   MOVE      *ZEROS        NRR1
026100970108     C                   MOVE      *ZEROS        V1CNBR
026200970124     C*
026300970124     C* MANIFEST PER LA DOGANA
026400970124     C     VCCDOG        IFEQ      'SI'
026500970124     C                   MOVE      'SI'          VSCDGN
026600970124     C                   ELSE
026700970124     C                   MOVE      'NO'          VSCDGN
026800970124     C                   END
026900970124     C* MANIFEST SPEDIZIONI C.O.D. E LISTA DISCORDANZE
027000970124     C     VCCDEF        IFEQ      'SI'
027100970124     C                   MOVE      'SI'          VSCCOD
027200970124     C                   MOVE      'SI'          VSCLDS
027300970124     C                   ELSE
027400970124     C                   MOVE      'NO'          VSCCOD
027500970124     C                   MOVE      'NO'          VSCLDS
027600970124     C                   END
027700970108     C*
027800970108     C* CARICAMENTO SUBFILE
027900020808     ***c     fgvnfv        chain     fnfgw01l                           39
028000020808     c     K02FGW01      chain     fnfgw01l                           39
028100000505     c                   if        *in39 or fgwatb<>' '
028200000505     c                   clear                   fgwff3
028300000505     c                   clear                   fgwff4
028400000505     c                   endif
028500970108     C                   CLEAR                   WSIEDI
028600020808     C                   Z-ADD     1             II
028700970108     C     FFV(II)       DOWGT     *ZEROS
028800970110     C                   SETOFF                                       22
028900970108     C                   MOVE      FFV(II)       VSCLNA
029000000405      *
029100000405      * Decodifico linea di arrivo
029200970108     C     VSCLNA        CHAIN     AZORG01L                           32
029300000405      *
029400970108     C  N32              MOVEL     ORGDES        VSDLNA
029500970109     C   32              MOVEL     *ALL'?'       VSDLNA
029600970108     C* Codice partner
029700970108     C                   Z-ADD     0             VSCCPT
029800970108     C*
029900020808     C                   Z-ADD     1             XY
030000020808     C                   MOVE      FFV(II)       WFFV
030100020808     C     WFFV          LOOKUP    LNA(XY)                                32
030200970108     C     *IN32         IFEQ      '1'
030300970108     C                   EXSR      SRCPT
030400970123     C* MEMORIZZO SE C'E' ATTRAVERSAMENTO DOGANA
030500970123     C                   MOVEL     §PTNAZ        VSHNAZ
030600970123     C                   MOVEL     '15'          COD
030700970123     C                   MOVEL(P)  VSHNAZ        KEY
030800970123     C     KTABE         CHAIN     TABEL00F                           30
030900970123     C  N30              MOVEL     TBLUNI        DS15
031000970123     C   30              CLEAR                   DS15
031100010405     C                   MOVE      §15DOG        VSHEFT
031200970110     C*
031300970110     C     VCCDEF        IFEQ      'SI'
031400970110     C     VSHFAP        ANDEQ     *BLANKS
031500970110     C     VSCSTM        IFEQ      'SI'
031600970110     C     VSCCOD        OREQ      'SI'
031700970110     C     VSCLDS        OREQ      'SI'
031800970110     C                   SETON                                        22
031900970110     C                   END
032000970110     C                   END
032100970110     C*
032200970108     C                   END
032300970108     C                   ADD       1             NRR1
032400970108     C                   WRITE     TC82S01
032500970108     C                   ADD       1             II
032600970108     C                   ENDDO
032700970218     C* CMR SEMPRE OBBLIGATORIO
032800020808     C*  Accendo indicatore di posizionamento cursore su Data CMR
032900970115     C                   SETON                                        46
033000970115     C*
033100970109     C                   SETON                                        23
033200970108     C                   Z-ADD     1             V1CNBR
033300020808     C                   MOVE      *BLANKS       WFIRST
033400960530     C*
033500960530     C                   ENDSR
033600961111     C***************************************************************
033700970108     C*  CAMBIATO CODICE PARTNER:REIMPOSTO CAMPI VIDEO              *
033800961111     C***************************************************************
033900961111     C     SRCPT         BEGSR
034000961111     C*
034100020808     C                   Z-ADD     KSC(XY)       VSCCPT
034200970108     C                   Z-ADD     VSCCPT        VSHCPT
034300961129     C* DECODIFICO COD. PARTNER DA P.D.C.
034400961129     C     KACO          CHAIN     CNACO00F                           31
034500970108     C  N31              MOVEL     ACORAG        VSDCPT
034600970108     C   31              CLEAR                   VSDCPT
034700961129     C*
034800020808     C                   MOVEL     DPT(XY)       EDIDSPT
034900020808     C                   MOVEL     DPU(XY)       EDIDSPU
035000970109     C* MEMORIZZO IN CAMPI HIDDEN
035100980320     C                   MOVEL     §PUBS1        VSHTRD
035200120117     C*******            Z-ADD     §PTNSP        VSHNSP
035300120117     C**** La lunghezza della nostra spedizione deve scomparire sostituita solo
035400120117     C*******   dal riferimento presente nell'AGE. La nostra bolla si identifica con
035500120117     C*******   16 o 14 caratteri.
035600120117     C*******   i 2 campi CNI e AGE hanno sempre riportato lo stesso riferimento
035700120117     C*******   quindi verrà eliminato quello inerente al CNI.
035800120117     C*******   Rimane un po' strano il discorso del Secolo.....sul CNI......
035900120117     C                   Z-ADD     §PTAGE        VSHNSP
036000970109     C                   Z-ADD     §PTAGE        VSHAGE
036100970109     C                   MOVEL     §PUNSA        VSHNSA
036200970109     C                   MOVEL     §PUSEC        VSHSEC
036300970109     C                   MOVEL     §PTFTT        VSHFTT
036400970109     C                   MOVEL     §PUSPC        VSHSPC
036500970109     C                   Z-ADD     §PTLNP        VSHLNP
036600970218     C                   MOVEL     §PTCM2        VSHCM2
036700980219     C                   MOVEL     §PUBS1        VSHTSC
036800050525     C                   MOVEL     §PUvma        VSHvma
036900060913     C                   MOVEL     §PUisc        VSHisc
037000970109     C*
037100970108     C* MEMORIZZO SE C'E' TRAMISSIONE DATI IN EDI
037200970108     C     §PTFTT        IFNE      *BLANKS
037300020808     C                   ADD       1             WSIEDI
037400970108     C                   END
037500961111     C*
037600970108     C* MANIFEST PER IL PARTNER
037700970218     C* SE PROVVISORIO                          --> NO
037800970108     C* SE DEFINITIVO E NO E.D.I.               --> SI
037900970205     C* SE DEFINITIVO E SI E.D.I.               --> SI
038000970108     C     VCCDEF        IFEQ      'NO'
038100970108     C                   MOVE      'NO'          VSCSTM
038200961111     C                   ELSE
038300970108     C                   MOVE      'SI'          VSCSTM
038400961112     C                   END
038500961213     C*
038600970108     C* CERCO NUMERO FAX PARTNER SE ELABORAZIONE DEFINITIVA
038700970108     C                   CLEAR                   VSHFAP
038800961213     C*
038900970108    1C     VCCDEF        IFEQ      'SI'
039000961213     C     KACO          CHAIN     CNIND00F                           31
039100041020      **
039200961213    2C     *IN31         IFEQ      *OFF
039300041020      **
039400970218     C* CONTROLLO N. FAX PARTNER SOLO SE DEVO INVIARE QUALCOSA VIA FAX
039500970121     C* AL PARTNER
039600041020      **  (adesso si deve considerare anche l'indirizzo e-mail)
039700970121    3C     VSCSTM        IFEQ      'SI'
039800010201     C     §PTFTT        ANDNE     'W'
039900970121     C     VSCLDS        OREQ      'SI'
040000970121     C     VSCCOD        OREQ      'SI'
040100041020      **
040200000405     C                   CLEAR                   TRUL42DS
040300961213     C                   MOVEL     INDTLF        D42FAX
040400961213     C                   CALL      'TRUL42R'
040500000405     C                   PARM                    TRUL42DS
040600041020      **
040700961213    4C     D42ERR        IFEQ      ' '
040800970108     C                   MOVEL     INDTLF        VSHFAP
040900961213    4C                   END
041000041020      **
041100041020      **  Attenzione: l'indirizzo E-mail ha priorità sul nr.FAX
041200041020      **       se c'è prende quello e non segnala l'errore.
041300041020     c                   clear                   trul44ds
041400050913     c                   clear                   trtcm1ds
041500041020      **** cerco solo sulle NOTE poichè è l'e-mail del Partner e non della Dogana
041600041021     c                   eval      d44pgm = 'TRTC82R'
041700041021     c                   move      vsccpt        d44ksc
041800041020     c                   eval      d44apl  = 'C'
041900041118     c                   eval      d44nk1  = '0151' + %editc(VSCCPT:'X')
042000041020     c                   eval      d44tnt  = '86'
042100041020     c                   call      'TRUL44R'
042200041020     c                   parm                    trul44ds
042300050913     c                   parm                    trtcm1ds
042400041020     c                   if        d44err = *blanks
042500041020     C                   MOVEL     'OK'          VSHFAP
042600041020     c                   end
042700041020      **
042800961213   X3C                   ELSE
042900970108     C                   MOVEL     'OK'          VSHFAP
043000961213    3C                   END
043100961216    2C                   END
043200970108    1C                   END
043300961111     C*
043400961111     C                   ENDSR
043500960529     C***************************************************************
043600960529     C*  Gestione prima videata                                     *
043700960529     C***************************************************************
043800960529     C     GESD01        BEGSR
043900970108     C*
044000970108     C*
044100960528     C                   EXFMT     TC82D01
044200961111     C                   SETOFF                                       402829
044300970109     C                   SETOFF                                       4407
044400960529     C** Fine Lavoro
044500960529     C     *INKC         IFEQ      '1'
044600020808     C                   MOVEL     'S'           WFINE
044700960529     C                   GOTO      FINVD1
044800960529     C                   END
044900960529     C* Controlli formato
045000960529     C                   EXSR      CONTR1
045100961111     C   28
045200961111     COR 29              GOTO      FINVD1
045300960530     C* No errori: VID02
045400960530     C                   EXSR      INZ02
045500960529     C*
045600960529     C     FINVD1        ENDSR
045700960530     C***************************************************************
045800960530     C*  Gestione seconda videata                                   *
045900960530     C***************************************************************
046000960530     C     GESD02        BEGSR
046100960530     C*
046200020808     C                   MOVEL     $MSG          WSVMSG
046300970108     C                   WRITE     TC82Z01
046400010509     C                   setoff                                       20
046500010509      * solo se ci sono dei records nel SFL accendo (20)
046600010509     C     NRR1          ifgt      0
046700010509     C                   seton                                        20
046800010509     C                   end
046900010509      * se il SFL è vuoto e nella schiera DPD nel primo elemento c'è una linea
047000010509      * DPD occorre emettere il messaggio di avvertimento e conferma.
047100010509      * e imposto un flag per gestire in segiuto le cose.
047200020808     C                   clear                   soloDPD
047300051223     C**   NRR1          ifeq      0
047400051223     C**   *in20         andeq     *off
047500051223     C********           move      'S'           soloDPD
047600051223     C**                 WRITE     TC82S01X
047700051223     C***                end
047800010509      *  Emissione 2°Video
047900970109     C                   EXFMT     TC82C01
048000010509      *
048100961114     C                   SETOFF                                       434128
048200970109     C                   SETOFF                                       424529
048300970123     C                   SETOFF                                       47
048400960530     C** Fine Lavoro
048500960530     C     *INKC         IFEQ      '1'
048600020808     C                   MOVEL     'S'           WFINE
048700960530     C                   GOTO      FINVD2
048800960530     C                   END
048900960530     C** Ritorno
049000960530     C     *INKL         IFEQ      '1'
049100960530     C                   MOVEL     '1'           WTPVID
049200970108     C                   CLEAR                   WGVDFV
049300970108     C                   CLEAR                   WGVNFV
049400970117     C                   CLEAR                   WWVNFV
049500960530     C                   GOTO      FINVD2
049600960530     C                   END
049700960530     C* Controlli formato
049800960530     C                   EXSR      CONTR2
049900960530     C* Conferma
050000961111     C  N28
050100961111     CANN29*INKF         IFEQ      '1'
050200970127     C*
050300960530     C                   EXSR      SBMP83
050400960530     C                   EXSR      INZ01
050500960530     C                   END
050600960530     C*
050700960530     C     FINVD2        ENDSR
050800960529     C***************************************************************
050900960529     C*  Sottometto elaborazione                                    *
051000960529     C***************************************************************
051100960529     C     SBMP83        BEGSR
051200960529     C*
051300970109     C                   MOVEL     V1CNFV        D82NFV
051400970109     C                   MOVEL     FGVDFV        D82DFV
051500970109     C                   MOVEL     VCCCMR        D82CMR
051600970109     C                   MOVEL     VCCDEF        D82DEF
051700970109     C                   MOVEL     VCCTRM        D82TRM
051800970115     C                   MOVEL     FGVSNP        D82FVB
051900970127     C*
052000010509      * Se non si tratta solo di DPD e il sfl era carico
052100010509     C     SoloDPD       Ifeq      *Blank
052200010509     C     *in20         andeq     *ON
052300010509     C*
052400970127     C* Prima di sottomettere, se elaborazione provvisoria cancello
052500970127     c* eventuali records dal file di work
052600970127     C     D82DEF        IFEQ      'NO'
052700050523     C     KTRE0         SETLL     WFMAN01L
052800050523     C     KTRE0         READE     WFMAN01L                             3231
052900970127     C     *IN31         DOWEQ     *OFF
053000970127     C     *IN32         ANDEQ     *OFF
053100050523     C                   DELETE    TREXP000
053200050523     C     KTRE0         READE     WFMAN01L                               31
053300970127     C                   ENDDO
053400970127     C                   END
053500970109     C*
053600020225     C* SE EUROPOLITAN SOTTOMETTO AZIONE DI CARICAMENTO SPUNTE
053700021017     C**   FGVLNA        CHAIN     AZORG01L
053800021017     C**                 IF        %FOUND(AZORG01L)
053900021017     c**                 movel     ORGDE3        OG143
054000021017     C**                 IF        §OGntw = 'EUP'
054100021017     C**                 MOVEL     FGVNFV        PARNFV
054200021017     C**   D82DEF        IFEQ      'NO'
054300021017     C**                 MOVE      *BLANKS       PARDEF
054400021017     C**                 ELSE
054500021017     C**                 MOVE      'S'           PARDEF
054600021017     C**                 END
054700021017     C**                 MOVEL     PARAM         KPJBU
054800021017     C**                 MOVEL     'LS44'        KCOAZ
054900021017     C**                 CALL      'BCH10'
055000021017     C**                 PARM                    KPJBA
055100021017     C**                 EndIf
055200021017     C**                 EndIf
055300980212     C*
055400970109     C                   Z-ADD     1             V1CNBR
055500970109     C*
055600970109     C     V1CNBR        CHAIN     TC82S01                            30
055700970109     C     *IN30         DOWEQ     *OFF
055800020808     C                   MOVEL     'M'           WMBR
055900970109     C                   MOVE      VSCCPT        WMBR
056000970109     C                   MOVEL     WMBR          D82MBR
056100970109     C                   Z-ADD     VSCCPT        D82CPT
056200970109     C                   MOVEL     VSCSTM        D82STB
056300970109     C                   MOVEL     VSHLNP        D82LNP
056400970109     C                   MOVEL     VSHNAZ        D82NAZ
056500970109     C                   MOVEL     VSHTRD        D82DET
056600970109     C                   Z-ADD     VSHNSP        D82CNI
056700970109     C                   Z-ADD     VSHAGE        D82AGE
056800970109     C                   MOVEL     VSHNSA        D82NSA
056900970109     C                   MOVEL     VSHSEC        D82SEC
057000970109     C                   MOVEL     VSHFTT        D82FTT
057100970109     C                   MOVEL     VSCDGN        D82DGN
057200970109     C                   MOVEL     VSCCOD        D82COD
057300970109     C                   MOVEL     VSHSPC        D82SPC
057400970109     C                   MOVEL     VSCLDS        D82LDS
057500970124     C                   MOVEL     VSHEFT        D82EFT
057600980219     C                   MOVEL     VSHTSC        D82TSC
057700050525     C                   movel     VSHvma        D82vma
057800060913     C                   movel     VSHisc        D82isc
057900050525     c                   clear                   kpjbu
058000960529     C                   MOVEL     TRTC82        KPJBU
058100010509      *
058200961205     C                   MOVEL     'TC83'        KCOAZ
058300041021     c                   if        kcoaz = *blank
058400041021     C                   CALL      'TRTC83C'
058500041021     C                   PARM                    KPJBA
058600041021     c                   else
058700041021     C*
058800900523     C                   CALL      'BCH10'
058900900523     C                   PARM                    KPJBA
059000041021     c                   end
059100970109     C*
059200970109     C                   ADD       1             V1CNBR
059300970109     C     V1CNBR        CHAIN     TC82S01                            30
059400970109     C                   ENDDO
059500900522     C*
059600010509     C                   ELSE
059700010509     C*
059800010509     C* Se si tratta solo di un foglio con linee Dpd devo chiuderlo
059900010509     C*  senza fare tutti i passaggi che il Manifest fa normalmente.
060000010509     C     SoloDPD       Ifeq      'S'
060100010509     C*
060200010509     C* Attenzione utilizza il campo MEMBRO per pilotare il pgm
060300010509     C*  TRTC83R4 in sola chiusura del foglio viaggio.
060400010509     C                   MOVEL     'SOLO_X_DPD'  D82MBR
060500050525     C                   MOVEL     kpjbu         KPJBUs
060600050525     C                   clear                   KPJBU
060700010509     C                   MOVEL     TRTC82        KPJBU
060800010509     C                   CALL      'TRTC83R4'
060900010509     C                   PARM                    KPJBA
061000050525     C                   MOVEL     kpjbuS        KPJBU
061100010509     C*
061200010509     C                   Endif
061300010509     C*
061400010509     C                   END
061500010509     C*
061600010509     C                   ENDSR
061700960529     C***************************************************************
061800960530     C*  Controlli prima videata                                    *
061900960529     C***************************************************************
062000960529     C     CONTR1        BEGSR
062100960529     C*
062200961111     C* CONTROLLI NUMERO FOGLIO VIAGGIO
062300960529     C* Ricerca numero foglio
062400960529     C     '?'           SCAN      V1CNFV                                 32
062500970123    1C     *IN32         IFEQ      '1'
062600931229     C                   RESET                   PARAM5
062700000614     C                   Z-ADD     SIMFEL        PA5FGS
062800050525     C                   MOVEL     kpjbu         KPJBUs
062900050525     c                   clear                   kpjbu
063000931229     C                   MOVEL     PARAM5        KPJBU
063100951108     C                   CALL      'FNLS53R'
063200931229     C                   PARM                    KPJBA
063300931229     C                   MOVEL     KPJBU         PARAM5
063400050525     C                   MOVEL     kpjbus        KPJBU
063500960529     C                   MOVEL     PA5NFV        V1CNFV
063600961111     C                   SETON                                        29
063700961111     C                   CLEAR                   $MSG
063800961111     C                   GOTO      ENDCTR
063900970123    1C                   ENDIF
064000960529     C* Controllo valdità numerica
064100960529     C                   TESTN                   V1CNFV               31
064200970123    1C     *IN31         IFEQ      *ON
064300020808     C                   MOVE      V1CNFV        W001A
064400970123    2C     W001A         IFLT      '0'
064500961111     C                   SETOFF                                       31
064600970123    2C                   END
064700970123    1C                   END
064800961111     C*
064900970123    1C     *IN31         IFEQ      *OFF
065000961111     C                   MOVEL     ERR(4)        $MSG
065100961111     C                   SETON                                        2840
065200961111     C                   GOTO      ENDCTR
065300970123    1C                   END
065400960529     C* Numero foglio viaggio esiste
065500960529     C                   MOVEL     V1CNFV        KNFV
065600020808     ***C     KNFV          CHAIN     FNFGV01L                           30
065700020808     C     K02FGV01      CHAIN     FNFGV01L                           30
065800970123    1C     *IN30         IFEQ      '1'
065900960529     C     *IN30         OREQ      '0'
066000931229     C     FGVATB        ANDNE     *BLANKS
066100960529     C                   SETON                                        4028
066200960529     C                   MOVEL     ERR(1)        $MSG
066300931229     C                   GOTO      ENDCTR
066400970123    1C                   ENDIF
066500961212     C* Il foglio viaggio deve essere di defluenza
066600990301     C                   MOVEL     'TV'          COD
066700990301     C                   MOVEL(P)  FGVTTR        KEY
066800990301     C     KTABE         CHAIN     TABEL                              83
066900990301     C     *IN83         IFEQ      *OFF
067000990301     C     TBLFLG        ANDEQ     ' '
067100990301     C                   MOVEL     TBLUNI        DSTV
067200990301     C                   ELSE
067300990301     C                   CLEAR                   DSTV
067400990301     C                   ENDIF
067500990301    1C     §TVDEF        IFNE      'S'
067600961212     C                   SETON                                        4028
067700961212     C                   MOVEL     ERR(19)       $MSG
067800961212     C                   GOTO      ENDCTR
067900970123    1C                   ENDIF
068000960529     C* Già  chiuso
068100970123    1C     FGVFCF        IFNE      ' '
068200961111     C                   SETON                                        4028
068300960529     C                   MOVEL     ERR(2)        $MSG
068400931229     C                   GOTO      ENDCTR
068500970123    1C                   ENDIF
068600970123     C* Esistono delle bolle borderizzate --> ERRORE
068700020808     ***C     KNFV          SETLL     FNBLP29L                               30
068800020808     C     K02BLP79      SETLL     FNBLP79L                               30
068900970123    1C     *IN30         IFEQ      *ON
069000960529     C                   SETON                                        4028
069100960529     C                   MOVEL     ERR(3)        $MSG
069200960529     C                   GOTO      ENDCTR
069300970123    1C                   END
069400970108     C* Se non è un bis non deve esserci un bis aperto con data <=
069500970123    1C     FGVSNP        IFEQ      *BLANKS
069600020808     ***C     *LOVAL        SETLL     FNFGV04L
069700020808     ***C                   READ      FNFGV04L                               30
069800020808     C     SIMfel        setll     FNFGV06L
069900020808     C     SIMfel        reade     FNFGV06L                               30
070000970123    2C     *IN30         DOWEQ     *OFF
070100020808    3***C     F4VATB        IFEQ      *BLANKS
070200020808     ***C     F4VLNA        ANDEQ     FGVLNA
070300020808     ***C     F4VSNP        ANDEQ     'B'
070400020808     ***C     F4VDFV        ANDLE     FGVDFV
070500020808    3C                   IF            F6Vatb =  *blanks
070600020808     C                             and F6Vlna =  FGVlna
070700020808     C                             and F6Vsnp =  'B'
070800020808     C                             and F6Vdfv <= FGVdfv
070900970107     C                   SETON                                        4028
071000970107     C                   MOVEL     ERR(21)       $MSG
071100970107     C                   GOTO      ENDCTR
071200970123    3C                   END
071300020808     ***C                   READ      FNFGV04L                               30
071400020808     C     SIMfel        reade     FNFGV06L                               30
071500970123    2C                   ENDDO
071600970123   X1C                   ELSE
071700970109     C                   SETON                                        07
071800970123    1C                   END
071900000405      *
072000000405      * Le linee DPD vanno escluse dall'elaborazione del manifest
072100051223      * ?DAL 9/1/2006 non è più vero anzi si dovrà eseguire il manifest
072200051223      * ? x inviare gli SCAN con il Manifest anzichè dal FIEU25R e
072300051223      * ?  anche x eseguire l'EDI in parallelo x consegnare in Spagna
072400051223      * ?   tramite SEUR (Partner DPD spagnolo)
072500000405     C                   clear                   WSffv
072600020808     C                   clear                   XX
072700020808     C                   clear                   YY
072800000405     C                   eval      II = 1
072900000405      *
073000000405    1C                   DOW       FFV(II) > *zeros
073100000405     C                   move      FFV(II)       WFFV
073200000405     C     Wffv          chain     Azorg01l
073300000405      *
073400000405     C                   IF        not %found
073500020225     c                   clear                   og143
073600020225     C                   ELSE
073700020225     C                   movel     ORGDE3        OG143
073800020225     C                   ENDIF
073900020225      *
074000051223     C*******            If        §OGntw <> 'DPD'
074100051223      *
074200000405     c                   add       1             XX
074300000405     C                   eval      wsFFV(xx) = FFV(II)
074400051223      *
074500051223     C*******            Else
074600051223     c*******            add       1             YY
074700051223     C*******            eval      dpdFFV(YY) = FFV(II)
074800051223     C*******            Endif
074900000405      *
075000000405     c                   add       1             II
075100000405     C                   ENDDO
075200000405     C                   eval      FFV = wsFFV
075300051124     C*
075400051124     C* Solo se in provvisorio e si trattava di un foglio con linea DPD
075500051124     c                   if        V1CDEF = 'NO' and dpdFFV(1) <> ' '
075600051124     C                   eval      FFV = dpdFFV
075700051124     c                   end
075800051124     C*
075900970108     C*
076000970108     C* CONTROLLO ELABORAZIONE PER DOGANA (SI/NO)
076100970117     C*
076200970120     C                   CLEAR                   D82DGX
076300970120     C                   CLEAR                   D82RSX
076400970120     C                   CLEAR                   D82CAP
076500970120     C                   CLEAR                   D82LOC
076600970120     C                   CLEAR                   D82PRO
076700970108     C                   Z-ADD     1             II
076800970117     C                   CLEAR                   WERR
076900970117     C                   CLEAR                   WDOGSI
077000970117     C                   CLEAR                   WDOGNO
077100970123    1C     FFV(II)       DOWGT     *ZEROS
077200970117     C     WERR          ANDEQ     *BLANKS
077300020808     C                   Z-ADD     1             XY
077400020808     C                   MOVE      FFV(II)       WFFV
077500020808     C     WFFV          LOOKUP    LNA(XY)                                32
077600970123    2C     *IN32         IFEQ      '1'
077700020808     C                   MOVEL     DPT(XY)       EDIDSPT
077800970108     C                   MOVEL     '15'          COD
077900970108     C                   MOVEL(P)  §PTNAZ        KEY
078000970108     C     KTABE         CHAIN     TABEL00F                           30
078100970108     C  N30              MOVEL     TBLUNI        DS15
078200970108     C   30              CLEAR                   DS15
078300970123     C* SE LNA CON ATTRAVERSAMENTO DOGANA:
078400010405    3C     §15dog        IFEQ      '1'
078500010405     C     §15dog        OREQ      '2'
078600970123    4C     V1CDOG        IFEQ      'NO'
078700020808     C                   MOVE      '1'           WDOGSI
078800970123    4C                   END
078900041020      **
079000970108     C* CERCO NUMERO FAX DOGANA
079100970123    4C     D82DGX        IFEQ      *BLANKS
079200041020      **
079300041020     C*  Se non c'è il nr.FAX:
079400041020     C* Attenzione: adesso può esserci l'indirizzo E-mail che è più importante e ha
079500041020     C*  priorità di invio sul nr.FAX ossia: se ho l'E-mail spedisco tramite
079600041020     C*   E-mail e non faccio il FAX.
079700041020     C                   clear                   trasm_mail        2
079800041020      **
079900041020     c                   clear                   trul44ds
080000050913     c                   clear                   trtcm1ds
080100041020      **** cerco sui LUOGHI poichè è l'e-mail della Dogana e non del Partner
080200041020     c                   eval      d44pgm  = 'TRTC82R'
080300041025     c                   eval      d44cl   = '200'
080400041020     c                   move      KSC(XY)       d44ksc
080500041020     c                   call      'TRUL44R'
080600041020     c                   parm                    trul44ds
080700050913     c                   parm                    trtcm1ds
080800041020     c                   if        d44err = *blanks
080900041020     c                   move      'OK'          trasm_mail
081000041020     c                   end
081100041020      **
081200970806     C                   MOVEL     'L'           KSFLS
081300970806     C     KSPE          CHAIN     FNSPE01L                           31
081400970123    5C     *IN31         IFEQ      *OFF
081500000405     C                   CLEAR                   TRUL42DS
081600970807     C                   MOVEL(P)  SPEFAX        D42FAX
081700970108     C                   CALL      'TRUL42R'
081800000405     C                   PARM                    TRUL42DS
081900970108    6C     D42ERR        IFEQ      ' '
082000970807     C                   MOVEL     SPEFAX        D82DGX
082100970120     C                   MOVEL     SPERAG        D82RSX
082200970807     C                   MOVEL     SPECAP        D82CAP
082300970120     C                   MOVEL     SPELOC        D82LOC
082400970120     C                   MOVEL     SPEPRO        D82PRO
082500970108    6C                   END
082600041020      **
082700970108    5C                   END
082800970123    4C                   END
082900970123   X3C                   ELSE
083000970123     C* LNA SENZA ATTRAVERSAMENTO DOGANA
083100970123    4C     V1CDOG        IFEQ      'NO'
083200020808     C                   MOVE      '1'           WDOGNO
083300970123   X4C                   ELSE
083400020808     C                   MOVE      'E'           WERR
083500970123     C                   SETON                                        4428
083600970123     C                   MOVEA     ERR(22)       K78
083700020808     C                   MOVE      FFV(II)       WFFVA
083800970123     C                   MOVEA     WFFVA         K78(41)
083900970123     C                   MOVEA     K78           $MSG
084000970123     C                   GOTO      ENDCTR
084100970123    4C                   END
084200970123    3C                   END
084300970123   X2C                   ELSE
084400970117     C* LINEA ESTERA NON PRESENTE IN TABELLA
084500970117     C                   MOVE      'E'           WERR
084600970117     C                   SETON                                        28
084700970117     C                   MOVEA     ERR(25)       K78
084800020808     C                   MOVE      FFV(II)       WFFVA
084900970117     C                   MOVEA     WFFVA         K78(19)
085000970117     C                   MOVEA     K78           $MSG
085100970117     C                   GOTO      ENDCTR
085200970123    2C                   END
085300970108     C                   ADD       1             II
085400970123    1C                   ENDDO
085500041020     C*
085600970117     C* ERRORE FORZABILE SE NUMERO FAX DOGANA ERRATO E RICHIESTA
085700970117     C* ELABORAZIONE PER DOGANA
085800041020      * adesso se non c'è nemmeno l'indirizzo e-mail
085900970117     C     V1CDOG        IFEQ      'SI'
086000970120     C     D82DGX        ANDEQ     *BLANKS
086100041020     C     trasm_mail    andeq     *blanks
086200970117     C     FGVNFV        ANDNE     WGVNFV
086300970117     C                   MOVE      FGVNFV        WGVNFV
086400970121     C                   SETON                                        28
086500970117     C                   MOVEL     ERR(23)       $MSG
086600970117     C                   GOTO      ENDCTR
086700970117     C                   END
086800970117     C*
086900970117     C* ERRORE FORZABILE SE ALCUNE LINEE CON DOGANA ED ALTRE SENZA
087000970117     C     V1CDOG        IFEQ      'NO'
087100970117     C     WDOGSI        ANDNE     *BLANKS
087200970117     C     WDOGNO        ANDNE     *BLANKS
087300970117     C     FGVNFV        ANDNE     WWVNFV
087400970117     C                   MOVE      FGVNFV        WWVNFV
087500970121     C                   SETON                                        28
087600970117     C                   MOVEL     ERR(26)       $MSG
087700970117     C                   GOTO      ENDCTR
087800970117     C                   END
087900970108     C*
088000961212     C* ERRORE FORZABILE: SE DATA FOGLIO < DATA DI SISTEMA LI AVVERTO
088100970108     C*
088200961212     C     FGVDFV        IFNE      WGVDFV
088300961212     C                   MOVE      FGVDFV        WGVDFV
088400961212     C     FGVDFV        IFLT      DATEU
088500961212     C                   SETON                                        4028
088600961212     C                   MOVEL     ERR(18)       $MSG
088700961212     C                   GOTO      ENDCTR
088800961212     C                   END
088900961212     C                   END
089000970123     C* MEMORIZZO SE OCCORRE INVIARE VIA FAX TOTALI PER DOGANA
089100970127     C     V1CDOG        IFEQ      'SI'
089200970218     C                   MOVE      'F'           D82TFF
089300970123     C                   END
089400960530     C*
089500960530     C     ENDCTR        ENDSR
089600960530     C***************************************************************
089700960530     C*  Controlli seconda videata                                  *
089800960530     C***************************************************************
089900960530     C     CONTR2        BEGSR
090000960530     C*
090100970115     C                   SETOFF                                       46
090200010509     C*  Esegue la routine di controlli solo se ci sono dei records
090300010509     C*  nel SFL. Se per esempio il foglio Viaggio è fatto solo su
090400010509     C*  linee DpD, il SFL è sicuramente vuoto.
090500010509     C     *in20         Ifeq      *on
090600010509     C*
090700970108     C* CONTROLLI CAMPI DEL SUBFILE
090800970110     C                   SETON                                        22
090900970110     C                   READC     TC82S01                                31
091000970110    1C     *IN31         DOWEQ     *OFF
091100970123     C     VSCDGN        IFEQ      'SI'
091200970218     C                   MOVE      'F'           D82TFF
091300970123     C                   END
091400970108     C*  Controllo che codice partner > 0
091500970110    2C     VSCCPT        IFEQ      0
091600970109     C                   CLEAR                   VSHCPT
091700970110     C                   CLEAR                   VSDCPT
091800970108     C                   SETON                                        4328
091900970108     C                   MOVEL     ERR(10)       $MSG
092000970108     C                   Z-ADD     NRR1          V1CNBR
092100970108     C                   UPDATE    TC82S01
092200970108     C                   GOTO      ENDCT2
092300970110    2C                   END
092400970108     C* Se ho modificato il codice del Partner controllo se congruente
092500970108     C* con lna
092600970110    2C     VSCCPT        IFNE      VSHCPT
092700020808     C                   Z-ADD     1             XY
092800020808     C     VSCCPT        LOOKUP    KSC(XY)                                32
092900970110    3C     *IN32         IFEQ      '1'
093000020808    4C     VSCLNA        IFNE      LNA(XY)
093100970110     C                   CLEAR                   VSDCPT
093200970110     C                   CLEAR                   VSHCPT
093300970108     C                   SETON                                        4328
093400970108     C                   MOVEL     ERR(6)        $MSG
093500970108     C                   Z-ADD     NRR1          V1CNBR
093600970108     C                   UPDATE    TC82S01
093700970108     C                   GOTO      ENDCT2
093800970110    4C                   END
093900970110   X3C                   ELSE
094000970110     C                   CLEAR                   VSDCPT
094100970110     C                   CLEAR                   VSHCPT
094200970108     C                   SETON                                        4328
094300970108     C                   MOVEL     ERR(11)       $MSG
094400970108     C                   Z-ADD     NRR1          V1CNBR
094500970108     C                   UPDATE    TC82S01
094600970108     C                   GOTO      ENDCT2
094700970110    3C                   END
094800970108     c* reimposto campi ed indicatori in base al nuovo cod.partner
094900970110    3C     VSHFTT        IFNE      *BLANKS
095000970108     C                   SUB       1             WSIEDI
095100970110    3C                   END
095200970108     C                   EXSR      SRCPT
095300970110     C*
095400970110     C* MEMORIZZO SE DEVO VISUALIZZARE MESSAGGIO FORZABILE CHE N.FAX
095500970110     C* PARTNER ERRATO
095600970110    3C     VSHFAP        IFEQ      *BLANKS
095700970110     C     VCCDEF        ANDEQ     'SI'
095800970110     C     SAVNR1        ANDEQ     *ZEROS
095900970110    4C     VSCSTM        IFEQ      'SI'
096000970110     C     VSCCOD        OREQ      'SI'
096100970110     C     VSCLDS        OREQ      'SI'
096200970110     C                   Z-ADD     NRR1          SAVNR1
096300970110    4C                   END
096400970110   X3C                   ELSE
096500970110     C* SE NUMERO FAX CORRETTO CONTROLLO DI NON AVER MEMORIZZATO IN
096600970110     C* UN GIRO PRECEDENTE CHE IL FAX E' ERRATO ED IN QUESTO CASO
096700970110     C* PULISCO I CAMPI DI COMODO
096800970110    4C     SAVNR1        IFEQ      NRR1
096900970110     C                   CLEAR                   SAVNR1
097000970110     C                   MOVE      *BLANKS       WFIRST
097100970110    4C                   END
097200970110    3C                   END
097300970110     C*
097400970108     C                   SETON                                        29
097500970108     C                   Z-ADD     NRR1          V1CNBR
097600970108     C                   UPDATE    TC82S01
097700970110   X2C                   ELSE
097800970110     C* SOLO LA PRIMA VOLTA E NON E' STATO MODIFICATO IL COD. PARTNER:
097900970108     C* se elaborazione definitiva memorizzo se occorre inviare msg
098000970108     C* forzabile se numero fax partner errato
098100970110    3C     VCCDEF        IFEQ      'SI'
098200970108     C     VSHFAP        ANDEQ     *BLANKS
098300970109     C     SAVNR1        ANDEQ     *ZEROS
098400970110     C     WFIRST        ANDEQ     *BLANKS
098500970110    4C     VSCSTM        IFEQ      'SI'
098600970108     C     VSCCOD        OREQ      'SI'
098700970108     C     VSCLDS        OREQ      'SI'
098800970108     C                   Z-ADD     NRR1          SAVNR1
098900970110     C                   MOVE      '1'           WFIRST
099000970110    4C                   END
099100970110    3C                   END
099200970110    2C                   END
099300970123     C* SE ELABORAZIONE PER DOGANA SI E NON C'E ATTRAVERSAM. DOGANA ERR
099400970123     C     VSCDGN        IFEQ      'SI'
099500970123     C     VSHEFT        ANDEQ     *BLANKS
099600970123     C                   SETON                                        4728
099700970123     C                   MOVEA     ERR(22)       K78
099800020808     C                   MOVE      VSCLNA        WFFVA
099900970123     C                   MOVEA     WFFVA         K78(41)
100000970123     C                   MOVEA     K78           $MSG
100100970123     C                   Z-ADD     NRR1          V1CNBR
100200970123     C                   UPDATE    TC82S01
100300970123     C                   GOTO      ENDCT2
100400970123     C                   END
100500970110     C                   READC     TC82S01                                31
100600970110    1C                   ENDDO
100700010509      *
100800010509      * se il SFL era carico
100900010509    1C                   EndIF
101000010509      *
101100970108     C* CONTROLLO CAMPI DEL SUBFILE CONTROL
101200970108     C* Se data CMR > 0 controllo che sia corretta
101300970108     C     VCCDAC        IFGT      *ZEROS
101400970108     C                   CLEAR                   WLBDA8
101500970108     C                   MOVE      VCCDAC        G02DAT
101600970108     C                   CALL      'XSRDA8'
101700970108     C                   PARM                    WLBDA8
101800970109     C                   MOVE      G02INV        D82DCM
101900020808     C                   MOVEL     G02INV        anno40
102000970108     C                   MOVE      G02DAT        VCCDAC
102100040520     C     *ymd          move      g02inv        dataCMR
102200040520     C     dataOGGi      subdur    7:*d          dataISO
102300040520     C     dataOGGi      adddur    7:*d          dataISO1
102400040520     c
102500970108     C     G02ERR        IFEQ      '1'
102600010611     C     anno40        orLT      daanno
102700010611     C     anno40        orGT      adanno
102800040520     C     dataCMR       orLT      dataISO
102900040520     C     dataCMR       orgT      dataISO1
103000970108     C                   SETON                                        4228
103100970108     C                   MOVEL     ERR(9)        $MSG
103200970108     C                   GOTO      ENDCT2
103300970108     C                   END
103400970108     C                   END
103500970108     C* Se CMR obbligatorio controllo che data e numero siano stati im-
103600970108     C* messi
103700970109     C* data CMR
103800970109     C     VCCDAC        IFEQ      *ZEROS
103900970109     C                   SETON                                        4228
104000970109     C                   MOVEL     ERR(8)        $MSG
104100970109     C                   GOTO      ENDCT2
104200970109     C                   END
104300970108     c* numero CMR
104400970108     C     VCCCMR        IFEQ      *BLANKS
104500970108     C     VCCCMR        OREQ      *ZEROS
104600970108     C                   SETON                                        4128
104700970108     C                   MOVEL     ERR(7)        $MSG
104800970108     C                   GOTO      ENDCT2
104900970108     C                   END
105000970218     C* Targa motrice
105100970218     C     VCCTRM        IFEQ      *BLANKS
105200970218     C                   SETON                                        2845
105300970218     C                   MOVEL     ERR(27)       $MSG
105400970218     C                   GOTO      ENDCT2
105500970218     C                   END
105600961204     C*
105700970108     C* SE ELABORAZIONE DEFINITIVA E NON HO NUMERO FAX PARTNER INVIO
105800970108     C* MESSAGGIO FORZABILE
105900010509     C     *in20         Ifeq      *on
106000010509      *
106100970109     C     SAVNR1        IFGT      *ZEROS
106200970109     C     SAVNR1        CHAIN     TC82S01                            30
106300970109     C                   Z-ADD     SAVNR1        V1CNBR
106400970110     C                   CLEAR                   SAVNR1
106500961213     C                   SETON                                        28
106600961213     C                   MOVEL     ERR(20)       $MSG
106700970109     C                   UPDATE    TC82S01
106800961213     C                   GOTO      ENDCT2
106900010509     C                   END
107000010509     C*
107100010509     C                   End
107200961204     C*
107300961022     C* Controllo se devo avvisare x elaborazione definitiva
107400970108     C     WSIEDI        IFGT      *ZEROS
107500970108     C     VCCDEF        ANDEQ     'SI'
107600961023     C     WSVMSG        ANDNE     ERR(12)
107700961022     C                   SETON                                        28
107800961022     C                   MOVEL     ERR(12)       $MSG
107900961022     C                   END
108000960530     C*
108100970110     C     ENDCT2        ENDSR
108200000405      *---------------------------------------------------------------*
108300000405      * *INZSR: Operazioni iniziali                                  -*
108400000405      *---------------------------------------------------------------*
108500960529     C     *INZSR        BEGSR
108600960529     C*
108700960529     C     *ENTRY        PLIST
108800960529     C                   PARM                    KPJBA
108900960529     C* Richiamo XPARUT
109000960529     C                   Z-ADD     1             CODUT
109100960529     C                   CALL      'X§PARUT'
109200000405     C                   PARM                    UT§DSE0F
109300960529     C                   MOVEL     REC80         CNCR80
109400960529     C                   MOVEL     RAGUT         RSUT             20
109500020920      *
109600020920     C                   MOVEL     kpjbu         KPJBUs
109700050525     c                   clear                   kpjbu
109800020920      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
109900120821     C*******            CLEAR                   TRUL06DS
110000120821     C*******            MOVE      '£1'          D06COD
110100120821     C*******            MOVEL     SIMFEL        D06KEY
110200120821     C*******            MOVEL     'L'           D06TLA
110300120821     C*******            MOVEL     TRUL06DS      KPJBU
110400120821      *******
110500120821     C*******            CALL      'TRUL06R'
110600120821     C*******            PARM                    KPJBA
110700120821     C*******            MOVEL     KPJBU         TRUL06DS
110800120821      **
110900120821     c* Carico x terminal  di arrivo
111000120821     c                   clear                   trul09ds
111100120821     c                   movel     simPOU        i09tfa
111200120821     c                   movel     'TFA'         i09mod
111300120821     c                   call      'TRUL09R'
111400120821     c                   parm                    trul09ds
111500120821     c
111600020920     C                   MOVEL     kpjbus        KPJBU
111700020920      *
111800961129     C*--- RICERCA CAPOCONTI------------------------------------------*
111900020808     C                   DO        50            XY
112000020808     C                   MOVE      TCU(XY)       TCUDS
112100961129     C     F56           IFEQ      'CG'
112200961129     C     F4            ANDEQ     '1'
112300961129     C     F3            ANDEQ     '0'
112400020808     C                   Z-ADD     KCU(XY)       KCI
112500961129     C                   END
112600961129     C                   END
112700960529     C*
112800960529     C* Definizione chiavi di accesso
112900961112     C     KTABE         KLIST
113000961112     C                   KFLD                    CODUT
113100961112     C                   KFLD                    COD
113200961112     C                   KFLD                    KEY
113300961129     C     KACO          KLIST
113400961129     C                   KFLD                    CODUT
113500961129     C                   KFLD                    KCI
113600970108     C                   KFLD                    VSCCPT
113700961216     C     KSPE          KLIST
113800970806     C                   KFLD                    KSFLS
113900020808     C                   KFLD                    KSC(XY)
114000961216     C                   KFLD                    KSCOD
114100970527     C     KTRE0         KLIST
114200970127     C                   KFLD                    D82NFV
114300970127     C                   KFLD                    SIMFEL
114400970127     C                   KFLD                    WTRC
114500020808     C     K02BLP79      klist
114600020808     C                   kfld                    SIMfel
114700020808     C                   kfld                    KNFV
114800020808     C     K02FGV01      klist
114900020808     C                   kfld                    KNFV
115000020808     C                   kfld                    SIMfel
115100020808     C     K02FGW01      klist
115200020808     C                   kfld                    FGVnfv
115300020808     C                   kfld                    FGVlnp
115400970127     C*
115500020808     ***C                   MOVE      'P'           WTRC
115600020808     ***C* 200 - LUOGO INVIO FAX A DOGANA
115700020808     ***C                   MOVE      '200'         KSCOD
115800020808     ***C* Definizione variabili
115900020808     ***C     *LIKE         DEFINE    TABCOD        KCOD
116000020808     ***C     *LIKE         DEFINE    FGVNFV        KNFV
116100020808     ***C     *LIKE         DEFINE    TBLCOD        COD
116200020808     ***C     *LIKE         DEFINE    TBLKEY        KEY
116300020808     ***C     *LIKE         DEFINE    FGVDFV        WGVDFV
116400020808     ***C     *LIKE         DEFINE    FGVNFV        WGVNFV
116500020808     ***C     *LIKE         DEFINE    FGVNFV        WWVNFV
116600020808     ***C     *LIKE         DEFINE    SPECOD        KSCOD
116700020808     ***C     *LIKE         DEFINE    SPEFLS        KSFLS
116800020808     ***C     *LIKE         DEFINE    NRR1          SAVNR1
116900020808     ***C     *LIKE         DEFINE    TRETRC        WTRC
117000960529     C*
117100960529     C* Carico dati DS partner in schiera
117200020808     C                   Z-ADD     1             XY
117300960529     C                   MOVEL     'PT'          KCOD
117400961114     C     KCOD          CHAIN     EDTAB01L                           30
117500960529     C     *IN30         DOWEQ     '0'
117600960529     C     TABFLG        IFEQ      ' '
117700020920     C                   MOVEL     TABUNI        EDIDSPT
117800020920      * solo se compresa nelle linee di partenza del terminal
117900020920     c     §ptlnp        lookup    Lin                                    32
118000020920     c                   if        *in32 = *on
118100020808     C                   ADD       1             XY
118200020808     C                   MOVEL     TABKEY        CPU(XY)
118300020808     C                   MOVEL     TABKEY        CPT(XY)
118400020808     C                   MOVE      '    '        CPT(XY)
118500020808     C                   MOVEL     TABUNI        DPT(XY)
118600020808     C                   MOVEL     §PTKSC        KSC(XY)
118700020808     C                   MOVEL     §PTLNP        LNA(XY)
118800020920     c                   endif
118900960529     C                   END
119000961114     C     KCOD          READE     EDTAB01L                               30
119100960529     C                   END
119200960906     C*
119300960906     C                   MOVEL     'PU'          KCOD
119400961114     C     KCOD          CHAIN     EDTAB01L                           30
119500960906     C     *IN30         DOWEQ     '0'
119600960906     C     TABFLG        IFEQ      ' '
119700020808     C                   Z-ADD     1             XY
119800020808     C     TABKEY        LOOKUP    CPU(XY)                                32
119900960906     C     *IN32         IFEQ      '1'
120000020808     C                   MOVEL     TABUNI        DPU(XY)
120100000405     C                   MOVEL     TABUNI        EDIDSPU
120200961023     C* Se no partner metto zero in LNA
120300051124      *** Attenzione se è una Linea DPD deve lasciarla
120400051124     c     lna(xy)       chain     azorg01l
120500051124     c                   if        %Found(azorg01l)
120600051124     c                   move      orgde3        og143
120700051124     c                   end
120800051124      ***   pulisce la linea se è un cliente e non è un DPD
120900110913     C*****§PUTPC        IFEQ      'C'
121000110913     C     §PUTPC        IFne      'P'
121100051124     c                   if        §ogntw <> 'DPD'
121200020808     C                   Z-ADD     0             LNA(XY)
121300051124     c                   end
121400961023     C                   END
121500051124      ***
121600960906     C                   END
121700960906     C                   END
121800961114     C     KCOD          READE     EDTAB01L                               30
121900960906     C                   END
122000961121     C*
122100961121     C* REPERISCO DATA DEL SISTEMA
122200020808     C                   TIME                    WTIME                          ORA E DATA
122300020808     C                   MOVE      WTIME         WDATE                          GG/MM/AAAA
122400020808     C                   MOVE      Wdate         anno                           AAAA
122500020808     C     anno          sub       1             daanno                         AAAA
122600020808     C     anno          add       1             adanno                         AAAA
122700961121     C                   Z-ADD     WDATE         G02DAT
122800961121     C                   Z-ADD     *ZERO         G02INV
122900961121     C                   MOVEL     *BLANKS       G02ERR
123000961121     C                   CALL      'XSRDA8'
123100961121     C                   PARM                    WLBDA8
123200020808     C                   Z-ADD     G02INV        DATEU
123300040520     c     *ymd          move      dateu         dataoggi
123400960529     C*
123500960529     C                   ENDSR
123600970218**
123700961111Foglio Errato o Annullato                                                     01
123800961111Foglio già chiuso                                                             02
123900961111Presenti bolle borderizzate: un foglio per l'estero non contiene bolle        03
124000961111Numero foglio viaggio presenta dei caratteri non validi                       04
124100961111                                                                              05
124200961111Codice Partner non congruente con linea di arrivo                             06
124300961111Numero CMR obbligatorio                                                       07
124400961111Data CMR obbligatoria                                                         08
124500961111Data CMR non valida                                                           09
124600961111Codice Partner obbligatorio                                                   10
124700961111Codice Partner inesistente                                                    11
124800970117ATTENZIONE!!!! Se elab.definitiva si procederà alla scrittura msg.EDI         12
124900961111Numero foglio I.M.A. presenta dei caratteri non validi                        13
125000961111Data foglio IMA (XX/XX/XXXX) diversa da data foglio viaggio (XX/XX/XXXX)      14
125100961111Non esiste nessun collo per l'estero spuntato sull'IMA richiesto.ENTER X FORZ.15
125200961111Non è un inventario arrivi                                                    16
125300961111Numero foglio I.M.A. obbligatorio                                             17
125400961212ATTENZIONE:Richiesto foglio viaggio con data < di data odierna.ENTER PER FORZ.18
125500970121Immettere un foglio viaggio di DEFLUENZA                                      19
125600041020Nr.FAX Partner/Indirizzo E-MAIL ERRATO. trasm. NON POSSIBILE     ENTER X FORZ.20
125700970108Presente foglio viaggio BIS ancora da chiudere per questa linea di arrivo     21
125800970117Elaborazione per dogana='SI' ma per lna xxx non è previsto l'attravers.dogana 22
125900041020Nr.FAX DOGANA/Indirizzo E-MAIL ERRATO. trasm. NON POSSIBILE     ENTER X FORZ. 23
126000970109Targa motrice obbligatoria se elaborazione definitiva o per la dogana         24
126100970117Non trovata linea xxx in tabella partner                                      25
126200970117Tutte le linee di arrivo devono avere o non avere l'attrav.dogana.ENTER X FORZ26
126300970218Targa motrice obbligatoria                                                    27
