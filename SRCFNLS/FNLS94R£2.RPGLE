000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940920      * FNLS94R *----------------------------------------------------*
000300910916      *         - STAMPA FOGLI VIAGGIO                               *
000400900515      *--------------------------------------------------------------*
000500110603     Ftntlt01L  IF   E           K DISK
000600110603     Ftntlr01L  IF   E           K DISK
000700110603     FTNPSC01L  IF   E           K DISK
000800110601     FFNFGs01L  IF   E           K DISK
000900110601     FFNFGV01L  IF   E           K DISK
001000951031     FFNFV401L  IF   E           K DISK
001100000427     FFNFGW01L  IF   E           K DISK
001200960226     FFNFV201L  IF   E           K DISK
001300951027     FTABEL00F  IF   E           K DISK
001400900515     FAZORG01L  IF   E           K DISK
001500090714     Fanfrn01l  IF   E           K DISK
001600090714     Fanrco98j  IF   E           K DISK
001700090714     Faiats01l  IF   E           K DISK
001800090714
001900090713     Ffnls94p   O    e             PRINTER USROPN
002000900625      *
002100070619     D PLP             S              3    DIM(200)
002200070619     D Pora            S             12    DIM(200)
002300070619     D Oarr            S             15    DIM(200)
002400900517      *
002500900601     D ALF             S              1    DIM(60)
002600110603     D GFV             S              1    DIM(20) CTDATA PERRCD(20)
002700900516      *
002800910911     D KPJBA         E DS
002900020812     D UT§DSE0F      E DS
003000020812     D CNCR80        E DS
003100090714     d ds3i          e ds
003200090714     d dstu          e ds
003300110614     d dstC          e ds
003400110603     d TNTL35DS      e ds
003500900522      *
003600910201     D                 DS
003700910405     D  FGVFLP                 1    240
003800910201     D  FGVFL2               241    447
003900000427     D  FGWFL3               448    687
004000000427     D  FGWFL4               688    897
004100000427     D  FLP                    1    897
004200000427     D                                     DIM(299)
004300900607      *
004400960201     D* PARAMETRI PASSATI : FOGLIO VIAGGIO
004500960201     D*                     FLAG STAMPA/RISTAMPA
004600900706     D PARAM2          DS
004700910920     D  FGV2                  11     15  0
004800901214     D  FLG2                  16     16
004900020812     D  PA2FGS               103    105
005000900515      *
005100900601     D PARAM3          DS
005200900601     D  NUMER                  1     12  0
005300900601     D  LETTER                13    122
005400900601     D  DECIM                123    124  0
005500900601     D  BYTE                 125    125
005600900515     D WLBDAT          DS
005700940920     D  G02DAT                 1      8  0
005800940920     D  G02INV                 9     16  0
005900940920     D  G02ERR                17     17
006000940920     D  G02TGI                18     22  0
006100110603     D WGIDAT          DS
006200110603     D  GI8DAT                 1      8  0
006300110603     D  GI8INV                 9     16  0
006400110603     D  GI8TGI                17     21  0
006500020812      *
006600020812     D COD             S                   LIKE(TBLCOD) inz
006700020812     D KEY             S                   LIKE(TBLKEY) inz
006800020812     D KTDH            S                   LIKE(FV2TDH) INZ('T')
006900020812     D KLAI            S                   LIKE(FV2LAI) INZ
007000020812     D WEPA            S                   LIKE(FV4EPA) INZ('P')
007100020812     D WTRC            S                   LIKE(FV4TRC) inz
007200020812     D SALLET          S             60    inz
007300020812     D W0030           S              3  0 inz
007400020812     D POPOST          S              3  0 inz
007500020812     D COMPRO          S              3    inz
007600020812     D COMLIN          S              3  0 inz
007700020812     D INTERO          S              9  0 inz
007800020812     D CENTO           S              3    inz
007900020812     D XB              S              3  0 inz
008000020812     D XC              S              3  0 inz
008100020812     D XI              S              3  0 inz
008200020812     D XJ              S              3  0 inz
008300071010     D XX              S              3  0 inz
008400090715     d $primo          s               n
008500090715     d $conta          s              3  0 inz
008600110614     d wHposMax        s              5  3 inz(6.000)
008700090715     d wNrigheMax      s              2  0 inz(6)
008800090715     d wNrigheStp      s              2  0 inz
008900110614     d wCpi            s              2  0 inz(6)
009000090715     d wMeno           s              5  3 inz
009100110609     d dataiso         s               d   datfmt(*iso)
009200090714
009300110606     d Kddes           s                   like(tltdde)
009400110606     d Kdde            s                   like(tltdde)
009500110606     d KTRN            s                   like(fgvtrn)
009600110606     d codsoc          s                   like(atssoc)
009700090714     d ksocieta        s                   like(rcosocieta)
009800090714     d ksnatura        s                   like(rcosnatura)
009900090714     d                                     inz('F')
010000090714     d kksc            s                   like(rcoksc)
010100110603     D* DFV in GG --> mi serve per trovare la Data Traino
010200110603     D WGGDFV          S                   LIKE(G02TGI)
010300110603     D WDTn            S                   LIKE(tltdde)
010400090714     ***************************************************************************
010500090714     **
010600090714     ** Definizione costanti.
010700090714     **
010800090714     ***************************************************************************
010900090714     D tibsSocI0     e ds                  prefix(£) inz
011000090714     D tibsSocO0     e ds                  inz
011100090714     D prmRqsOpCode...
011200090714     D                 S             10A
011300090714     D prmRpyOpCode...
011400090714     D                 S             10A
011500090714     D prmRpyIdMsg...
011600090714     D                 S             10I 0
011700090714     D prmRqsFormato...
011800090714     D                 S             10A
011900090714     D prmRqsDataSize...
012000090714     D                 S             10I 0
012100090714     D prmRpyFormato...
012200090714     D                 S             10A
012300090714     D prmRpyDataSize...
012400090714     D                 S             10I 0
012500090714
012600900601      *---------------------------------------------------------------*
012700900601      * INDICATORI USATI
012800900601      *
012900900608      * 19/21 =DI COMODO
013000110608      * 22    =stampo riga vuota sotto allo scambio se non c'e' primo carico
013100010919      * 32    = CHAIN SU FNFGW
013200000615      * 34    = COMODO
013300010621      * 38    = NON VISUALIZZAZ. DEL CELL DEL CONDUCENTE SE NON ESISTE
013400900601      *--------------------------------------------------------------*
013500090714     C*
013600090714     C* STAMPO FOGLIO VIAGGIO in 2 copie
013700090714     C                   OPEN      fnls94p
013800900705     C*
013900090714     C* elaboro i dati per la stampa
014000910916     C                   EXSR      STAFV
014100090714
014200090714      * Stampo testata + luogo carico e scarico
014300090714     c                   exsr      stp1
014400090714      * Stampo dati vettore
014500090714     c                   exsr      stp4
014600090714      * Stampo dati committente/caricatore
014700090714     c                   exsr      stp5
014800090714      * Stampo dati proprietario merce
014900090714     c                   write     ls94prop
015000090714      * Stampo dati merce trasportata
015100090714     c                   if        §tupkg = 0
015200090714     c                   eval      speso = 12
015300090714     c                   else
015400090714     c                   eval      speso = §tupkg
015500090714     c                   endif
015600090714     c                   write     ls94mer
015700110601     c
015800110601     c* stampo tragitto autista
015900110601     c                   exsr      Tragittoaut
016000110601     c
016100090714      * Stampo osservazioni
016200090714     c                   write     ls94oss
016300110601      * Stampo compilazione
016400110601     c                   write     ls94comp
016500110601      * Stampo firma Pedini
016600110601     c                   exsr      stp7
016700110601      * Stampo riga
016800110601     c                   write     ls94riga
016900110601      * Testata foglio viaggio
017000110601     c                   write     ls94t1
017100110601      * stampa fermate sintetiche
017200110601     c                   exsr      stp8
017300110601      * Stampo note
017400090714     c                   exsr      stp6
017500090714      * Stampo piombi
017600090714     c                   eval      spiombi = fgvpmb
017700090715     c                   write     ls94pmb
017800090714      * Stampo riga per firma resp.operativo
017900090714     c                   write     ls94fro
018000090714      * Stampo disegno
018100090714     c                   write     ls94dis
018200090714      * Stampo note vettore
018300090714     c                   write     ls94note
018400090714
018500090714     C                   CLOSE     fnls94p
018600900529      *
018700110603     C* CHIUDO PGM TNTL35
018800110603     C                   MOVEL     'C'           D35TLA
018900110603     C                   MOVEL     TNTL35DS      KPJBU
019000110603     C                   CALL      'TNTL35R'
019100110603     C                   PARM                    KPJBA
019200110603     c
019300020812     C                   MOVEL     *ON           *INLR
019400940920     C**************************************************************************
019500940920     C*    STAMPA FOGLIO VIAGGIO
019600940920     C**************************************************************************
019700900601     C     STAFV         BEGSR
019800110608     c                   setoff                                       22
019900090714
020000090714     C                   move      PA2fgs        FGVlnp
020100090714     C     K02FGV01      chain     FNFGV01L                           19
020200090714
020300000427     C* AGGANCIO FILE ESTENSIONI
020400020812     C     K02FGW01      chain     FNFGW01L                           32
020500000615     C     *IN32         IFEQ      *OFF
020600020812     c     FGWatb        andeq     *blanks
020700000615     C     FGWFGS        ANDNE     0
020800020812     C                   Z-ADD     FGWFGS        POPOST
020900000615     C                   ELSE
021000020812     C                   Z-ADD     FGVLNP        POPOST
021100000615     C                   ENDIF
021200000615      *
021300000427     C     *IN32         IFEQ      *ON
021400000427     C     FGWATB        ORNE      ' '
021500000427     C                   CLEAR                   FGWFL3
021600000427     C                   CLEAR                   FGWFL4
021700000427     C                   ENDIF
021800940920     C*
021900000427     C* PRENDO PER STAMPARE LE LINEE DI TRANSITO UNA SOLA VOLTA
022000900601     C*        STAMPA LA RELATIVA PROVINCIA
022100020812     C                   Z-ADD     0             XI
022200020812     C                   Z-ADD     1             XB
022300070619     c                   clear                   plp
022400070619     c                   clear                   pora
022500910909     C*
022600070619     C                   DO        199           XB
022700020812     C     FLP(XB)       IFNE      *BLANKS
022800900601     C*
022900020812     C     FLP(XB)       LOOKUP    PLP                                    20
023000020812     C  N20              ADD       1             XI
023100020812     C  N20              MOVE      FLP(XB)       PLP(XI)
023200070619     c  n20              exsr      CarArrivo
023300070619     c
023400910909     C                   ELSE
023500910909     C*
023600070619     C                   Z-ADD     199           XB
023700900601     C                   END
023800910910     C                   END
023900090714
024000900601     C* AGGIUNGO ANCHE LA FILIALE DI ARRIVO FINALE
024100020812     C                   MOVE      FGVLNA        COMPRO
024200071010     c                   z-add     1             xx
024300071010     C     COMPRO        LOOKUP    PLP(XX)                                20
024400071010     c* Se già trovata la pulisco per metterla per ultima
024500071010     c* (mi serve nel caso non si abbinato a traino il foglio per
024600071010     c*  stampare come ultima fermata la filiale finale di arrivo)
024700071010     c                   if        *in20
024800071010     c                   clear                   plp(XX)
024900071010     c                   clear                   pora(XX)
025000071010     c                   endif
025100071010     C                   ADD       1             XI
025200071010     C                   MOVE      FGVLNA        PLP(XI)
025300071010     c                   exsr      CarArrivo
025400090714
025500951027     C*
025600910909     C* VEDO SE STAMPARE L'IMPORTO PATTUITO
025700910906     C     FGVSNS        COMP      'N'                                    15
025800951027     C*
025900900601     C* TRASFORMO L'IMPORTO PATTUITO IN LETTERE
026000010919     C     *IN15         IFEQ      *OFF
026100010919      * Se il file dell'estensione non esiste o i campi sono a zero
026200010919      * devo prendere quello che c'è nel file principale
026300010919     C     *IN32         IFEQ      *ON
026400010919     C     FGVIPT        IFGT      *ZEROS
026500010919     C                   Z-ADD     FGVIPT        PIPT             11 2
026600010919     C                   Z-ADD     FGVIPT        SALNUM           11 2
026700010919     C                   MOVEL     'ITL'         PVIP              3
026800010919     C                   ENDIF
026900010919      * Se il file dell'estensione esiste e i campi sono a zero
027000010919      * devo prendere quello che c'è nel file principale
027100010919     C                   ELSE
027200010919     C     FGWIPT        IFEQ      *ZEROS
027300010919     C     FGWIPT        ANDGT     *ZEROS
027400010919     C                   Z-ADD     FGWIPT        PIPT
027500010919     C                   Z-ADD     FGWIPT        SALNUM
027600010919     C                   MOVEL     'ITL'         PVIP
027700010919     C                   ENDIF
027800010919      * Se il file dell'estensione esiste e i campi sono > zero
027900010919      * devo prendere quello che c'è nel file dell'estensione
028000010919     C     FGWIPT        IFGT      *ZEROS
028100010919     C                   Z-ADD     FGWIPT        PIPT
028200010919     C                   Z-ADD     FGWIPT        SALNUM
028300010919     C                   MOVEL     FGWVIP        PVIP
028400010919     C                   ENDIF
028500010919     C                   ENDIF
028600010919     C                   EXSR      NUMLET
028700010919     C                   ENDIF
028800110601     c*
028900110601     C* AGGANCIO FILE scamkbio traino
029000110601     C     K02FGW01      chain     FNFGs01L                           32
029100110601     c                   if        not %found(fnfgs01l)
029200110601     c                   clear                   fgspsc
029300110601     c                   endif
029400110609     C* luogo di compilazione
029500110609     c                   clear                   loclnp
029600110609     C     FGVLNP        CHAIN     AZORG01l
029700110609     c                   if        %found(azorg01l)
029800110609     c                   eval      loclnp = orgloc
029900110609     c                   endif
030000110609     c
030100900601     C                   ENDSR
030200070619     C**************************************************************************
030300070619     c* Verifico se presente orario di arrivo
030400070619     C**************************************************************************
030500070619     c     CarArrivo     BEGSR
030600070619     c                   movel     'A'           WEPA
030700071010     c                   movel     plp(XI)       klai
030800070619     c     kfv2          chain     fnfv201l
030900070619    3c                   if        %found(fnfv201l)
031000070619     c                   movel     fv2dpa        pora(XI)
031100070619     c                   move      fv2hpa        pora(XI)
031200070619     c                   endif
031300070619     c                   ENDSR
031400090714
031500090714      *------------------------------------------------------------------------*
031600090714      *    stampo testata
031700090714      *------------------------------------------------------------------------*
031800090714     c     stp1          begsr
031900090714
032000090714     c                   if        popost = 0
032100090714     c                   clear                   stapos
032200090714     c                   else
032300090714     c                   move      popost        stapos
032400090714     c                   endif
032500090714
032600090714     c                   eval      stanfv = fgv2
032700090714
032800090714     C* GIRO DATA FOGLIO VIAGGIO
032900090714     C                   MOVE      FGVDFV        G02INV
033000090714     C                   MOVE      *ZEROS        G02DAT
033100090714     C                   MOVEL     3             G02ERR
033200090714     C                   CALL      'XSRDA8'
033300090714     C                   PARM                    WLBDAT
033400090714     C                   Z-ADD     G02DAT        STADFV
033500110603     C                   Z-ADD     G02TGI        WGGDFV
033600090714
033700110601     c                   write     ls94t2
033800110601     c
033900110601     c                   write     ls94t1
034000090714
034100090714     c                   endsr
034200090714
034300090714      *------------------------------------------------------------------------*
034400090714      *    stampo luogo carico
034500090714      *------------------------------------------------------------------------*
034600090714     c     stp2          begsr
034700090714
034800110603     c                   clear                   desarr
034900110603     c                   clear                   indarr
035000110603     c                   clear                   locarr
035100110603     c                   clear                   prvarr
035200090714     C     FGVLNP        CHAIN     AZORG01l
035300090714     c                   if        %found(azorg01l)
035400110603     c                   eval      desarr = orgdes
035500110603     c                   eval      indarr = orgind
035600110603     c                   eval      locarr = orgloc
035700110603     c                   eval      prvarr = orgpro
035800090714     c                   endif
035900090714
036000090714     C* GIRO DATA PARTENZA
036100090714     C* SE NON C'E' CERCO LA TEORICA
036200090714     C     FGVDTP        IFEQ      0
036300090714     C     FGVTRN        ANDGT     0
036400090714     C                   MOVEL     'P'           WEPA
036500090714     C                   CLEAR                   KLAI
036600090714     C     KFV2          CHAIN     FNFV201L                           31
036700090714     C     *IN31         IFEQ      *OFF
036800090714     C     FV2ATB        ANDEQ     ' '
036900090714     C                   Z-ADD     FV2DPA        FGVDTP
037000090714     C                   Z-ADD     FV2HPA        FGVHMP
037100090714     C                   ENDIF
037200090714     C                   ENDIF
037300090714     C*
037400090714     C                   MOVE      FGVDTP        G02INV
037500090714     C                   MOVE      *ZEROS        G02DAT
037600090714     C                   MOVEL     3             G02ERR
037700090714     C                   CALL      'XSRDA8'
037800090714     C                   PARM                    WLBDAT
037900110606     C                   Z-ADD     G02DAT        STADpa
038000090714
038100090714     C* ORA PARTENZA REALE
038200110606     C                   eval      stahpa = fgvhmp
038300090714
038400110608     c* Se ho un luogo di scambio e la data e ora è < del primo carico
038500110608     c*  --> non stampo nulla
038600121122     c*      Stampo invece se l'orario è vuoto (solo per BIS senza traino
038700121122     c*       di scambio ma solo con la fil arrivo finale)
038800121122     c                   if        fgspsc=*blanks or fgsdgs=0 or
038900110608     c                             (fgsdgs>fgvdtp or (fgsdgs=fgvdtp and
039000110608     c                             fgshhs>=fgvhmp))
039100090714     c                   write     ls94car
039200110608     c                   seton                                        22
039300110608     c                   endif
039400090714
039500090714     c                   endsr
039600090714
039700090714      *------------------------------------------------------------------------*
039800090714      *    stampo luoghi scarico
039900090714      *------------------------------------------------------------------------*
040000090714     c     stp3          begsr
040100090715
040200090715     c                   clear                   $conta
040300090715     c                   eval      $primo = *off
040400090715     c                   clear                   wNrigheStp
040500110603
040600110603     c                   eval      stsdat = 'Arrivo Previsto:'
040700090717
040800090717      * Se non ci sono gli orari, stampo senza
040900090717    1c                   if        pora(1)= *blanks and pora(2)=*blanks
041000090717     c                   seton                                        25
041100090717    1c                   endif
041200090714
041300070619     c* Ordino gli arrivi finali per data e ora
041400090714     C                   Z-ADD     1             XC
041500071010    1c                   do        200           xc
041600070619     c                   move      plp(XC)       Oarr(XC)
041700090714     c                   movel     pora(XC)      Oarr(XC)
041800070620    1c                   enddo
041900090714     c                   sorta     Oarr
042000090714     c
042100090714      * stampo le fermate
042200070619     C                   Z-ADD     1             XC
042300070620    0c                   do        200           xc
042400070619    1C     OArr(XC)      ifne      *BLANKS
042500070619     C                   MOVE      OArr(XC)      COMLIN
042600090715     c                   add       1             $conta
042700090714
042800090714     c                   clear                   desarr
042900090714     c                   clear                   indarr
043000090714     c                   clear                   locarr
043100090714     c                   clear                   prvarr
043200090714     C     COMLIN        CHAIN     AZORG01l
043300090714     c                   if        %found(azorg01l)
043400090714     c                   eval      desarr = orgdes
043500090714     c                   eval      indarr = orgind
043600090714     c                   eval      locarr = orgloc
043700090714     c                   eval      prvarr = orgpro
043800090714     c                   endif
043900090714     c* data/ora arrivo
044000070619     c                   movel     OArr(XC)      dataora          12 0
044100070619     c                   movel     dataora       fv2dpa
044200070619     c                   move      dataora       fv2hpa
044300070619     C                   MOVE      Fv2dpa        G02INV
044400070619     C                   MOVE      *ZEROS        G02DAT
044500070619     C                   MOVEL     3             G02ERR
044600070619     C                   CALL      'XSRDA8'
044700070619     C                   PARM                    WLBDAT
044800090714     C                   Z-ADD     G02DAT        STADpa
044900090714     C                   eval      stahpa = fv2hpa
045000090715      * stampo solo 4 fermate
045100090715      * un bel chiodo perchè se ne stampo più di 4
045200090715      * devo reimpostare i parametri per la stampa della firma di Pedini
045300090715     c                   if        $conta <= 4
045400090714
045500090714      * stampe diverse tra la prima fermata e le sucessive
045600090715     c                   if        not $primo
045700090714     c                   eval      stscar = 'Luogo di scarico'
045800090715     c                   eval      $primo = *on
045900090714     c                   else
046000090714     c                   clear                   stscar
046100090714     c                   endif
046200090714
046300090714     c                   write     ls94scar
046400090715     c                   add       1             wNrigheStp
046500090714     c                   endif
046600090715     c                   endif
046700070620    0C                   ENDdo
046800090714
046900090714     C                   endsr
047000090714
047100090714      *------------------------------------------------------------------------*
047200090714      *    stampo dati vettore
047300090714      *------------------------------------------------------------------------*
047400090714     c     stp4          begsr
047500090714
047600090714     C* DECODIFICA TRAZIONISTA da anagrafica fornitori
047700090714     c                   move      *zeros        kksc
047800090714     C                   move      fgvPDR        KKSC
047900090714     c                   eval      scodpdr = fgvpdr
048000090714     c                   clear                   srscpdr
048100090714     c                   clear                   sdatipdr
048200090714     C     Kfrn          CHAIN     anfrn01l
048300090714     c                   if        %found(anfrn01l)
048400090714     C     Krco          CHAIN     anrco98j
048500090714     c                   if        %found(anrco98j)
048600090714     c                   eval      srscpdr = sogdes
048700090714     c                   eval      sdatipdr = %trim(indindriz) + ' ' +
048800090714     c                             %trim(indcap) + ' ' +
048900090714     c                             %trim(indlocalit) + ' ' +
049000090715     c                             %trim(indprov) + ' ' + 'Tel.' + ' ' +
049100090715     c                             %trim(indtelefon) + ' ' + 'P.Iva' + ' ' +
049200090714     c                             %trim(sogpartiva)
049300090714     c                   endif
049400090714     c                   endif
049500090714
049600090714      * cerco iscrizione all'albo AUT
049700090714     c                   clear                   spialbo
049800090714     c                   clear                   snialbo
049900120521     c     kats          setgt     aiats01l
050000120521     c     kats          readpe    aiats01l
050100120521     c                   if        not%eof(aiats01l)
050200090714     c                   eval      spialbo = atspia
050300090714     c                   eval      snialbo = atsnia
050400090714     c                   endif
050500090714
050600090714      * nome conducente
050700090714     c                   eval      sdescon = fgvdcn
050800090714
050900090714      * Reperisco il numero del cellulare del conducente
051000090714     C                   MOVE      'P'           WEPA
051100090714     C                   MOVE      'L'           WTRC
051200090714     c                   clear                   scelcon
051300090714     C     KFV4          CHAIN     FNFV401L
051400090714     c                   if        %found(fnfv401l)
051500090714     C                   MOVEL     FV4NOT        scelcon
051600090714     c                   endif
051700090714      * targhe
051800090714     c                   eval      stargam = fgvtrm
051900090714     c                   eval      stargar = fgvtrr
052000090714
052100090714      * DECODIFICO TIPO AUTOMEZZO
052200090717     c                   clear                   dstu
052300090714     C     FGVTMZ        IFNE      ' '
052400090714     c                   eval      scodtmz = fgvtmz
052500090714     c                   clear                   sdestmz
052600090714     C                   Z-ADD     1             CODUT
052700090714     C                   MOVEL     'TU'          COD
052800090714     C                   MOVEL(P)  FGVTMZ        KEY
052900090714     C     KTAB          CHAIN     TABEL00f
053000090714     c                   if        %found(tabel00f)
053100090714     c                   eval      dstu = tbluni
053200090714     c                   endif
053300090714     C                   ENDIF
053400090714
053500090714     c                   if        §tudes <> *blanks
053600090714     c                   eval      sdestmz = §tudes
053700090714     c                   else
053800090714     c                   eval      sdestmz = '(Autotreno; Motrice; Rimorchio; -
053900090714     c                             Semirimorchio)'
054000090717     c                   eval      §tupkg = 12
054100090714     c                   endif
054200090714
054300090714     c                   write     ls94vet
054400090714
054500090714     c                   eval      speso = §tupkg
054600090714     c                   if        speso = 0
054700090714     c                   eval      speso = 12
054800090714     c                   endif
054900090714
055000090714     c                   endsr
055100090714
055200090714      *------------------------------------------------------------------------*
055300090714      *    stampo dati committente/caricatore
055400090714      *------------------------------------------------------------------------*
055500090714     c     stp5          begsr
055600090714
055700090714     ** Inizializzo il programma.
055800090714     C                   CALL      'TIBSSOCR'
055900090714     C                   PARM      'INIT'        prmRqsOpCode
056000090714     C                   PARM                    prmRpyOpCode
056100090714     C                   PARM                    prmRpyIdMsg
056200090714
056300090714     ** Chiamo passando un id società che esiste.
056400090714     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
056500090714     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
056600090714     C                   RESET                   tibsSocI0
056700090714     C                   EVAL      £idSocieta = ksocieta
056800110609     c     *iso          movel     fgvdfv        dataiso
056900110609     C                   EVAL      £dtvalidita = dataiso
057000090714     C
057100090714     C                   CALL      'TIBSSOCR'
057200090714     C                   PARM      'GETANAGRAF'  prmRqsOpCode
057300090714     C                   PARM      *BLANK        prmRpyOpCode
057400090714     C                   PARM      *ZERO         prmRpyIdMsg
057500090714     C                   PARM      'TIBSSOCI0'   prmRqsFormato
057600090714     C                   PARM                    tibsSocI0
057700090714     C                   PARM                    prmRqsDataSize
057800090714     C                   PARM      'TIBSSOCO0'   prmRpyFormato
057900090714     C                   PARM                    tibsSocO0
058000090714     C                   PARM                    prmRpyDataSize
058100090714
058200090714     ** Chiudo il programma.
058300090714     C                   CALL      'TIBSSOCR'
058400090714     C                   PARM      'FINALIZE'    prmRqsOpCode
058500090714     C                   PARM                    prmRpyOpCode
058600090714     C                   PARM                    prmRpyIdMsg
058700090714
058800090714      * riporto i dati societari in stampa
058900090714     c                   eval      srsccom = ragsociale
059000090714     c                   eval      sdaticom = %trim(sedlegind) + ' ' +
059100090714     c                             %trim(sedlegcap) + ' ' +
059200090714     c                             %trim(sedlegloc) + ' ' +
059300090715     c                             %trim(sedlegpro) + ' ' + 'Tel.' + ' ' +
059400090715     c                             %trim(sedlegtel) + ' ' + 'P.Iva' + ' ' +
059500090714     c                             %trim(partitaiva)
059600090714
059700090714     c                   write     ls94com
059800090714
059900090714     c                   eval      srscbcom = ragsocbrev
060000090714
060100090714     c                   endsr
060200090714
060300090714      *------------------------------------------------------------------------*
060400090714      *    stampo note
060500090714      *------------------------------------------------------------------------*
060600090714     c     stp6          begsr
060700090714
060800090714     c                   eval      stnote = 'Annotazioni:'
060900090714      * se ci sono stampo prima le note poi le annotazioni
061000090714     c                   if        fgvnt1 <> *blanks or fgvnt2 <> *blanks
061100090714     c                   eval      snote1 = fgvnt1
061200090714     c                   eval      snote2 = fgvnt2
061300090714     c                   write     ls94annot
061400090714     c                   clear                   stnote
061500090714     c                   exsr      sr_annot
061600090715     c                   if        snote1 <> *blanks or snote2 <> *blanks
061700090714     c                   write     ls94annot
061800090715     c                   endif
061900090714      * se non ci sono stampo solo le annotazioni
062000090714     c                   else
062100090714     c                   exsr      sr_annot
062200090714     c                   write     ls94annot
062300090714     c                   endif
062400090714
062500090714     c                   endsr
062600090715
062700090715      *------------------------------------------------------------------------*
062800090715      *    stampo firma Pedini
062900090715      *------------------------------------------------------------------------*
063000090715     c     stp7          begsr
063100110608     c* se non ho stampato la prima di carico, tolgo una riga alle stampe
063200110608     c*  perchè la finra deve "salire" di una riga avwendone stampata una in men
063300110608     c                   if        not *in22
063400110608     c                   sub       1             wNrigheStp
063500110608     c                   endif
063600090715
063700090715      * calcolo la posizione di stampa della firma (in verticale)
063800090715     c                   if        wNrigheStp = wNrigheMax
063900090715     c                   eval      hpos = wHposMax
064000090715     c                   else
064100090715     c                   eval      wMeno = (wNrigheMax - wNrigheStp + 1) / wCpi
064200090715     c                   eval      hpos = wHposMax - wMeno
064300090715     c                   endif
064400090715     c                   write     firmaimg
064500090715
064600090715     c                   endsr
064700090714
064800110601      *------------------------------------------------------------------------*
064900110601      *    stampa fermate sintetiche
065000110601      *------------------------------------------------------------------------*
065100110601     c     stp8          begsr
065200110601
065300110603     c                   clear                   desarr
065400110601     C     FGVLNP        CHAIN     AZORG01l
065500110601     c                   if        %found(azorg01l)
065600110603     C                   MOVEL     ORGDES        DESarr
065700110601     c                   endif
065800110601     c* Preparo stringa di tutte le fermate del foglio viaggio
065900110603     c                   eval      stafer=desarr
066000110601
066100110601     c* Ordina orari di arrivo
066200110601    1c                   do        200           xc
066300110601     c                   move      plp(XC)       Oarr(XC)
066400110601     c                   movel     pora(XC)      Oarr(XC)
066500110601    1c                   enddo
066600110601     c                   sorta     Oarr
066700110601     c
066800110601      *
066900110601     C                   Z-ADD     1             XC
067000110601    0c                   do        200           xc
067100110601    1C     OArr(XC)      ifne      *BLANKS
067200110601     C                   MOVE      OArr(XC)      COMLIN
067300110601     c                   add       1             $conta
067400110601
067500110601     c                   clear                   desarr
067600110601     C     COMLIN        CHAIN     AZORG01l
067700110601     c                   if        %found(azorg01l)
067800110601     c                   eval      desarr = orgdes
067900110601     c                   endif
068000110601     c
068100110601     c* Preparo stringa di tutte le fermate del foglio viaggio
068200110601     c                   eval      stafer=%trim(stafer) +'-'+desarr
068300110601     c                   endif
068400110601     c                   enddo
068500110614     c*
068600110614     c* Stampo la partenza reale se c'e'
068700110614     c                   setoff                                       2627
068800110614    1c                   if        fgvdtp=0
068900110614     c                   seton                                        26
069000110614   x1c                   else
069100110614     C*
069200110614     C                   MOVE      FGVDTP        G02INV
069300110614     C                   MOVE      *ZEROS        G02DAT
069400110614     C                   MOVEL     3             G02ERR
069500110614     C                   CALL      'XSRDA8'
069600110614     C                   PARM                    WLBDAT
069700110614     C                   Z-ADD     G02DAT        STADpa
069800110614
069900110614     C* ORA PARTENZA REALE
070000110614     C                   eval      stahpa = fgvhmp
070100110614     c* Verifico se c'e' ritardo
070200110614     C                   MOVEL     'P'           WEPA
070300110614     C                   MOVEL     'R'           ktdh
070400110614     C                   CLEAR                   KLAI
070500110614     C     KFV2          CHAIN     FNFV201L                           31
070600110614    2c                   if        not *in31 and fv2atb=' ' and
070700110614     c                             fv2rtc<>' '
070800110614     c                   eval      startc=fv2rtc
070900110614     c                   seton                                        27
071000110614     C                   MOVEL     'TC'          COD
071100110614     C                   MOVEL(P)  fv2rtc        KEY
071200110614     C     KTAB          CHAIN     TABEL                              83
071300110614    3C     *IN83         IFEQ      *OFF
071400110614     C     TBLFLG        ANDEQ     ' '
071500110614     C                   MOVEL     TBLUNI        DSTC
071600110614     C                   MOVEL     §TCDES        stadrtc
071700110614    3C                   ENDIF
071800110614    2c                   endif
071900110614    1c                   endif
072000110601     c
072100110601     c                   write     ls94ferm
072200110614     c                   setoff                                       2627
072300110601
072400110601     c                   ENDSR
072500110601      *------------------------------------------------------------------------*
072600110601      *    stampo luogo scambio e finale
072700110601      *------------------------------------------------------------------------*
072800110606     c     stp9s         begsr
072900110601     c                   clear                   dessca
073000110601     c                   clear                   indsca
073100110601     c                   clear                   prvsca
073200110601     c                   eval      stscar='Luogo di scambio'
073300110601     c     fgspsc        chain     tnpsc01l
073400110601     c                   if        %found(tnpsc01l)
073500110601     c                   eval      indsca= %trim(pscind) +' '+pscnar
073600110601     c                   eval      prvsca=pscprv
073700110601     c                   eval      dessca=pscloc
073800110601     c                   endif
073900110601     c* data e ora
074000110601     c                   if        fgsdgs=0
074100110601     c                   seton                                        25
074200110601     c                   else
074300110601     C                   MOVE      Fgsdgs        G02INV
074400110601     C                   MOVE      *ZEROS        G02DAT
074500110601     C                   MOVEL     3             G02ERR
074600110601     C                   CALL      'XSRDA8'
074700110601     C                   PARM                    WLBDAT
074800110601     C                   Z-ADD     G02DAT        STADps
074900110601     C                   eval      stahps = fgshhs
075000110601     c                   endif
075100110601     c
075200110601     c                   write     ls94scamb
075300110606     c                   add       1             wNrigheStp
075400110606     c
075500110606     c                   ENDSR
075600110606      *------------------------------------------------------------------------*
075700110606      *    stampo filiale finale
075800110606      *------------------------------------------------------------------------*
075900110606     c     stp9          begsr
076000110601     c* Fermata di scarico
076100110601     c                   seton                                        25
076200110601     C     fgslnar       CHAIN     AZORG01l
076300110601     c                   if        %found(azorg01l)
076400110601     c                   eval      desarr = orgdes
076500110601     c                   eval      indarr = orgind
076600110601     c                   eval      locarr = orgloc
076700110601     c                   eval      prvarr = orgpro
076800110601     c                   else
076900110601     c                   clear                   desarr
077000110601     c                   clear                   indarr
077100110601     c                   clear                   locarr
077200110601     c                   clear                   prvarr
077300110601     c                   endif
077400110601     c                   eval      stscar='Luogo di scarico'
077500110601     c                   write     ls94scar
077600110601     c
077700110606     c                   add       1             wNrigheStp
077800110601     c
077900110601     c                   ENDSR
078000110603      *------------------------------------------------------------------------*
078100110603      *    stampo tutte le fermate carico e scarico dal traino
078200110603      *------------------------------------------------------------------------*
078300110603     c     stp10         begsr
078400110606     c
078500110606     c                   clear                   $conta
078600110606     c                   eval      $primo = *off
078700110606     c                   clear                   wNrigheStp
078800110606     c                   clear                   Wdtn
078900110603     c
079000110603     c* Leggo le fermate e stampo
079100110603     c     ktlr          setll     tntlr01l
079200110603     c     ktlr          reade     tntlr01l
079300110603
079400110603    2c                   dow       not %eof(tntlr01l)
079500110606     c
079600110606     c* Decodifico fermata e calcolo data / oora teorica di arrivo
079700110606     c                   exsr      Decoferm
079800110606     c
079900110606     c* Se c'e' lo scambio controllo data e ora --> per passare al traino
080000110606     c*  di scambio
080100110606     c                   if        fgspsc<>*blanks
080200110606     c                   if        fgsdgs<d35dte or (fgsdgs=d35dte
080300110606     c                             and fgshhs<d35hte)
080400110606     c* Esco per stampare lo scambio
080500110606     c                   leavesr
080600110606     c                   endif
080700110606     c                   endif
080800110608
080900110608    3c                   if        tlrcar='S' and $conta=0
081000110608     c                   add       1             $conta
081100110608     c                   write     ls94car
081200110608     c                   seton                                        22
081300110608     c
081400110608   x3c                   else
081500110606     c
081600110606     c                   exsr      StampaFer
081700110606     c
081800110603    3c                   endif
081900110603     c
082000110603     c     ktlr          reade     tntlr01l
082100110603    2c                   enddo
082200110603     c
082300110606     c                   ENDSR
082400110606      *------------------------------------------------------------------------*
082500110606      *    stampo tutte le fermate dal traino di copia
082600110606      *------------------------------------------------------------------------*
082700110606     c     stp11         begsr
082800110608     c                   eval      $primo = *off
082900110606     c
083000110606     c* Leggo le fermate e stampo
083100110606     c     ktlr          setll     tntlr01l
083200110606     c     ktlr          reade     tntlr01l
083300110606
083400110606    1c                   dow       not %eof(tntlr01l)
083500110606     c
083600110606     c* Decodifico fermata e calcolo data / oora teorica di arrivo
083700110606     c                   exsr      Decoferm
083800110606     c
083900110606     c* Stampo solo l fermate con data/ora > dello scambio
084000110606     c*  di scambio
084100110606    2c                   if        fgsdgs>d35dte or (fgsdgs=d35dte
084200110606     c                             and fgshhs>d35hte)
084300110606    2c                   else
084400110606     c
084500110606     c                   exsr      StampaFer
084600110606    2c                   endif
084700110606     c
084800110606     c     ktlr          reade     tntlr01l
084900110606    1c                   enddo
085000110606     c
085100110606     c                   ENDSR
085200110606      *------------------------------------------------------------------------*
085300110606      * decodifico fermata da fermate traino
085400110606      *------------------------------------------------------------------------*
085500110606     c     DecoFerm      BEGSR
085600110606     c
085700110606     c* Calcolo data traino quando leggo la prima fermata
085800110606    3c                   if        wdtn=0
085900110606    4c                   if        tlrgfv='A'
086000110606     c                   eval      wdtn=fgvdfv
086100110606   x4c                   else
086200110606     c                   z-add     1             C                 3 0
086300110606     C     TLRGFV        LOOKUP    GFV(C)                                 31
086400110606     C     WGGDFV        SUB       C             GI8TGI
086500110606     C                   CALL      'XSRGI8'
086600110606     C                   PARM                    WGIDAT
086700110606     C                   z-add     GI8INV        WDTN
086800110606    4c                   endif
086900110606    3c                   endif
087000110606
087100110606     c                   clear                   desarr
087200110606     c                   clear                   indarr
087300110606     c                   clear                   locarr
087400110606     c                   clear                   prvarr
087500110606     C     TLRFIL        CHAIN     AZORG01l
087600110606    3c                   if        %found(azorg01l)
087700110606     c                   eval      desarr = orgdes
087800110606     c                   eval      indarr = orgind
087900110606     c                   eval      locarr = orgloc
088000110606     c                   eval      prvarr = orgpro
088100110606    3c                   endif
088200110606
088300110606     c* Letta la fermata richiamo pgm tntl35r per data e ora teorica
088400110606     c                   clear                   tntl35ds
088500110606     C                   MOVEL     'D'           D35TUP
088600110606     C                   MOVEL     KTRN          D35TRN
088700110606     C                   MOVEL     KDDE          D35DDE
088800110606     C                   MOVEL     WDTN          D35DTN
088900110606     C                   MOVEL     fgvfrq        D35TFM
089000110606
089100110614     c* Se è fermata di CARICO solo o anche --> scrivo CARICO
089200110606    3c                   if        tlrcar='S'
089300110606     C                   MOVEL     'P'           D35EPA
089400110606     c                   else
089500110606     C                   MOVEL     'A'           D35EPA
089600110606     C                   MOVEL     tlrfil        D35fil
089700110606    3c                   endif
089800110614     c
089900110606     C                   MOVEL     tlrnfm        D35NFM
090000110606     C                   MOVEL     TNTL35DS      KPJBU
090100110606     C                   CALL      'TNTL35R'
090200110606     C                   PARM                    KPJBA
090300110606     C                   MOVEL     KPJBU         TNTL35DS
090400110606     c
090500110606    3c                   if        d35err<>*blanks
090600110606     c                   seton                                        25
090700110606   x3c                   else
090800110606     c                   setoff                                       25
090900110606     C                   MOVE      d35dte        G02INV
091000110606     C                   MOVE      *ZEROS        G02DAT
091100110606     C                   MOVEL     3             G02ERR
091200110606     C                   CALL      'XSRDA8'
091300110606     C                   PARM                    WLBDAT
091400110606     C                   Z-ADD     G02DAT        STADPA
091500110606     C                   eval      stahpa = d35hte
091600110606    3c                   endif
091700110606     c                   ENDSR
091800110606      *------------------------------------------------------------------------*
091900110606      * Stampa fermata dda TLR
092000110606      *------------------------------------------------------------------------*
092100110606     c     StampaFer     BEGSR
092200110606     c                   add       1             $conta
092300110606    4c                   if        $conta <= 5
092400110606     c*
092500110606    5c                   if        tlrcar='S'
092600110606     c                   eval      stscar = 'Luogo di carico '
092700110614     c                   eval      stsdat = 'Partenza teorica'
092800110606     c                   write     ls94scar
092900110606     c                   add       1             wNrigheStp
093000110606    5c                   else
093100110606
093200110606      * stampe diverse tra la prima fermata e le sucessive
093300110606    6c                   if        not $primo
093400110606     c                   eval      stscar = 'Luogo di scarico'
093500110606     c                   eval      $primo = *on
093600110606     c                   else
093700110606     c                   clear                   stscar
093800110606    6c                   endif
093900110606
094000110606     c                   eval      stsdat = 'Arrivo Previsto:'
094100110606     c                   write     ls94scar
094200110606     c                   add       1             wNrigheStp
094300110606    5c                   endif
094400110606    4c                   endif
094500110606     c                   ENDSR
094600110606      *------------------------------------------------------------------------*
094700110606      * Verifico esistenza traino
094800110606      *------------------------------------------------------------------------*
094900110606     c     CtrTRN        BEGSR
095000110606     c                   clear                   Wtraino           1
095100110606     c     ktrn          setgt     tntlt01l
095200110606     c     ktrn          readpe    tntlt01l
095300110606     c
095400110606    1c                   dow       not %eof(tntlt01l)
095500110606    2c                   if        tltatb=' ' and fgvdfv>=tltdde
095600110606     c                             and fgvdfv<=tltdsc
095700110606     c                   eval      wtraino='S'
095800110606     c                   leave
095900110606    2c                   endif
096000110606     c
096100110606     c     ktrn          readpe    tntlt01l
096200110606    1c                   enddo
096300110606     c
096400110606     c                   ENDSR
096500110601      *------------------------------------------------------------------------*
096600110601      *    STAMPO IL TRAGITTO CHE FA L'AUT DIVERSO a seconda che ci sia lo
096700110601      *   scambio e/o il traino  sul foglio viaggio
096800110601      *------------------------------------------------------------------------*
096900110601     c     TragittoAut   BEGSR
097000110606     c
097100110606     c                   clear                   kddes
097200110606     c
097300110606     c* Verifico se trovo traino di copia
097400110606    1c                   if        fgstrns>0
097500110606     c                   eval      ktrn=fgstrns
097600110606     c                   exsr      CtrTRN
097700110606     c*  se non trovato prendo il primo che trovo
097800110606    2c                   if        wtraino<>'S'
097900110606     c     ktrn          setgt     tntlt01l
098000110606     c     ktrn          readpe    tntlt01l
098100110606     c
098200110606    3c                   if        not %eof(tntlt01l)   and
098300110606     c                             tltatb=' '
098400110606     c                   eval      kddes=tltdde
098500110606   x3c                   else
098600110606     c                   clear                   fgstrns
098700110606    3c                   endif
098800110606     c
098900110606   x2c                   else
099000110606     c                   eval      kddes=tltdde
099100110606    2c                   endif
099200110606    1c                   endif
099300110606     c
099400110606     c* Verifico se trovo traino --> se non trovato come se non vi fosse
099500110606     c                   if        fgvtrn>0
099600110606     c                   eval      ktrn=fgvtrn
099700110606     c                   exsr      CtrTRN
099800110606     c
099900110606     c* Non trovato traino --> stampo come se fosse sbinato
100000110606    1c                   if        wtraino<>'S'
100100110606     c                   clear                   fgvtrn
100200110606     c                   endif
100300110606     c                   endif
100400110606     c
100500110601     c* 1) FGV non abbinato  a traino e assenza di punto di scambio
100600110601     c                   select
100700110601     c                   when      fgvtrn=0 and fgspsc=*blanks
100800110601      * Stampo luogo carico
100900110601     c                   exsr      stp2
101000110601      * Stampo luogo scarico
101100110601     c                   exsr      stp3
101200110601     c
101300110601     c* 2) FGV non abbinato a traino ma presente Punti Scambio con fil ritorno
101400110601     c                   when      fgvtrn=0 and fgspsc<>*blanks and fgslnar>0
101500110606     c                   clear                   wNrigheStp
101600110601      * Stampo luogo carico
101700110601     c                   exsr      stp2
101800110603      * Stampo luogo scambio e finale
101900110606     c                   exsr      stp9s
102000110606     c                   exsr      stp9
102100110603     c
102200110603     c* 3) FGV non abbinato a traino ma presente traino di scambio
102300110603     c                   when      fgvtrn=0 and fgspsc<>*blanks and fgstrns>0
102400110606     c                   exsr      stp2
102500110608      * Stampo luogo scambio
102600110606     c                   clear                   $conta
102700110606     c                   eval      $primo = *off
102800110606     c                   clear                   wNrigheStp
102900110606     c                   clear                   Wdtn
103000110606     c                   exsr      stp9s
103100110608      * Stampo fermate finali
103200110608     c                   eval      ktrn=fgstrns
103300110608     c                   eval      kdde=kddes
103400110606     c                   exsr      stp11
103500110603
103600110603     c* 4) FGV     abbinato a traino ma non presente scambio
103700110603     c                   when      fgvtrn>0 and fgspsc= *blanks
103800110606     c                   eval      ktrn=tlttrn
103900110606     c                   eval      kdde=tltdde
104000110603     c                   exsr      stp10
104100110603
104200110606     c* 5) FGV     abbinato a traino    e presente scambio
104300110603     c                   when      fgvtrn>0 and fgspsc<>*blanks and fgstrns>0
104400110606     c                   eval      ktrn=tlttrn
104500110606     c                   eval      kdde=tltdde
104600110606     c                   exsr      stp10
104700110606     c                   exsr      stp9s
104800110606     c
104900110606     c* fermate finali da traino di scambio
105000110606     c                   eval      ktrn=fgstrns
105100110606     c                   eval      kdde=kddes
105200110606     c                   exsr      stp11
105300110601     c                   endsl
105400110601     c
105500110601     c                   ENDSR
105600090714      *------------------------------------------------------------------------*
105700090714      *    annotazioni
105800090714      *------------------------------------------------------------------------*
105900090714     c     sr_annot      begsr
106000090714
106100090715     c                   clear                   snote1
106200090715     c                   clear                   snote2
106300090714     C* CHAINO LE ANNOTAZIONI
106400090714     c                   MOVEL     'P'           WEPA
106500090714     C                   MOVEL     'N'           WTRC
106600090714     C     KFV4          SETLL     FNFV401L
106700090714     C     KFV4          READE     FNFV401L                               20
106800090714     C     *IN20         IFEQ      *OFF
106900110614     c     fv4atb        andeq     ' '
107000090714     C                   MOVEL     FV4NOT        snote1
107100090714     C     KFV4          READE     FNFV401L                               20
107200090714     C  N20              MOVEL     FV4NOT        snote2
107300090714     C                   ENDIF
107400090714
107500090714     c                   endsr
107600090714
107700940920     C**************************************************************************
107800900601     C* TRASFORMO L'IMPORTO IN LETTERE
107900940920     C**************************************************************************
108000900601     C     NUMLET        BEGSR
108100940920     C*
108200900601     C                   MOVE      *BLANKS       LETTER
108300900601     C                   MOVE      0             BYTE
108400020812     C                   MOVEL     SALNUM        INTERO
108500011016     C                   Z-ADD     INTERO        NUMER
108600020812     C                   MOVE      NUMER         CENTO
108700011016      *
108800011016     C     CENTO         IFNE      '000'
108900011016     C     PVIP          ANDEQ     'ITL'
109000900601     C                   MOVE      '000'         NUMER
109100011016     C                   END
109200010919     C                   MOVE      SALNUM        DECIM
109300900601     C                   MOVEL     PARAM3        KPJBU
109400900601     C                   CALL      'XNITAL'
109500900601     C                   PARM                    KPJBA
109600900601     C                   MOVEL     KPJBU         PARAM3
109700900601     C                   MOVE      *BLANK        ALF
109800900601     C                   MOVEA     LETTER        ALF
109900900601     C     CENTO         IFNE      '000'
110000010919     C     PVIP          ANDEQ     'ITL'
110100020812     C                   Z-ADD     2             XJ
110200020812     C     '*'           LOOKUP    ALF(XJ)                                21
110300900601     C   21              DO
110400020812     C                   MOVE      '/'           ALF(XJ)
110500020812     C                   ADD       1             XJ
110600020812     C                   MOVEA     CENTO         ALF(XJ)
110700900601     C                   END
110800900601     C                   END
110900020812     C                   MOVEA     ALF           SALLET
111000900601     C*
111100900601     C                   ENDSR
111200090714
111300090714      *****************************************************************
111400090714      * ROUTINE INIZIALE
111500090714      *****************************************************************
111600090714     C     *INZSR        BEGSR
111700090714
111800090714     C     *ENTRY        PLIST
111900090714     C                   PARM                    KPJBA
112000090714     C                   MOVEL     KPJBU         PARAM2
112100090714      *---------------------------------------------------------------*
112200090714      * CHIAVI                                                        *
112300090714      *---------------------------------------------------------------*
112400090714     C     KTAB          KLIST
112500090714     C                   KFLD                    CODUT             1 0
112600090714     C                   KFLD                    COD
112700090714     C                   KFLD                    KEY
112800090714     C     KFV4          KLIST
112900090714     C                   KFLD                    FGVLNP
113000090714     C                   KFLD                    FGVNFV
113100090714     C                   KFLD                    WEPA
113200090714     C                   KFLD                    W0030
113300090714     C                   KFLD                    WTRC
113400090714     C     KFV2          KLIST
113500090714     C                   KFLD                    FGVLNP
113600090714     C                   KFLD                    FGVNFV
113700090714     C                   KFLD                    WEPA
113800090714     C                   KFLD                    KLAI
113900090714     C                   KFLD                    KTDH
114000090714     C     K02FGV01      klist
114100090714     C                   kfld                    FGV2
114200090714     C                   kfld                    FGVlnp
114300090714     C     K02FGW01      klist
114400090714     C                   kfld                    FGVnfv
114500090714     C                   kfld                    FGVlnp
114600090714
114700090714     C     Kfrn          KLIST
114800090714     C                   KFLD                    Ksocieta
114900090714     C                   KFLD                    KKSC
115000090714     C     Krco          KLIST
115100090714     C                   KFLD                    Ksocieta
115200090714     C                   KFLD                    kSNatura
115300090714     C                   KFLD                    KKSC
115400090714     C     Kats          KLIST
115500090714     C                   KFLD                    codsoc
115600090714     C                   KFLD                    fgvpdr
115700110603     C     Ktlr          KLIST
115800110606     C                   KFLD                    Ktrn
115900110606     C                   KFLD                    Kdde
116000090714
116100090714      *
116200090714      * Reperimento dati aziendali
116300090714      * (se non passato il SIMFEL nel campo PA2FGS)
116400090714     C                   if        PA2fgs <= *zeros
116500090714     C                   call      'X§PARUT'
116600090714     C                   parm                    UT§DSE0F
116700090714     C                   movel     REC80         CNCR80
116800090714     C                   move      SIMfel        PA2fgs
116900090714     C                   endif
117000090714     C*
117100090714     c                   clear                   staris
117200090714     C     FLG2          IFEQ      'R'
117300090714     C                   eval      staris = '-RISTAMPA-'
117400090714     C                   ENDIF
117500090714
117600090714      * Recupero il codice Società che gestisce i traini
117700090714     c                   eval      codut = 1
117800090714     c                   clear                   ds3i
117900090714     c                   eval      cod = '3I'
118000090714     c                   eval      key = '1'
118100090714     c     ktab          chain     tabel00f
118200090714     c                   if        %found(tabel00f)
118300090714     c                   eval      ds3i = tbluni
118400090714     c                   endif
118500090714      * il codice della società sono gli ulti 3 caratteri del S.I. traini
118600090714     c                   eval      ksocieta = %subst(§3ibst:7:3)
118700090714     c                   eval      codsoc = %dec(%subst(§3ibst:7:3):3:0)
118800090714
118900090714     C                   ENDSR
119000110603** SCHIERA GFV
119100110603BCDEFGHILMNOPQRSTUVZ
