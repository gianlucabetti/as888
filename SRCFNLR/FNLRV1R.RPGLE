000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200040528     H* FNLRV1R *-----------------------------------------------------*
000300050616     H* 11 off -   Controllo colli di valore al terminal di arrivo
000400091026     H* 11 on  -   Statistica m.cubi in arrivo per linea terminal di arrivo
000500930126     H*---------------------------------------------------------------*
000600040528     FFNLRv1d   CF   E             WORKSTN
000700040528     F                                     SFILE(LRv1DSF:NRR)
000800040610     FFNFVV01L  IF   E           K DISK
000900060920     FFNFVV08L  IF   E           K DISK    RENAME(FNFvv000:FNFvv008)
001000060920     FFNFGV02L  IF   E           K DISK
001100941221     FFNFVA02L  IF   E           K DISK
001200040610     FFNFVA01L  IF   E           K DISK
001300941221     F                                     RENAME(FNFVA000:FNFVA001)
001400000628     FFNFWA01L  IF   E           K DISK
001500070110     FFNBRV02L  IF   E           K DISK
001600040610     FFNARB06L  IF   E           K DISK
001700040610     FFNBLP01L  IF   E           K DISK
001800040610     FFNANM01L  IF   E           K DISK
001900971223     FAZORG01L  IF   E           K DISK
002000040604     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002100040604     F                                     USROPN
002200921109     D*
002300050616     D* passaggio dati a batch - FNLRV5R -
002400050616     D*
002500050616     D PARA05          DS
002600050616     D  P05DFV                 1      8  0
002700050616     D  P05NFV                 9     13  0
002800050616     D  P05NPG                14     14  0
002900050616     D  P05fgs                15     17  0
003000060919     D  P05POF                18     20  0
003100060920     D  P05loc                21     45
003200091026     D  P05Tra                46     46
003300951114     D*
003400941221     D PARA04          DS
003500941221     D* PARA04: PARAMETRI IN INPUT/OUTPUT AL PGM FNLR04R
003600941221     D  P04NFV                 1      5
003700941221     D  P04LNP                 6      8  0
003800941221     D  P04LNA                 9     11  0
003900941221     D  P04NRS                12     13  0
004000941221     D  P04NCD                14     20  0
004100941221     D  P04NCA                21     27  0
004200941221     D  P04NPS                28     29  0
004300941221     D  P04NPG                30     30  0
004400941221     D  P04FLG                31     31
004500941221     D  P04PGM                32     38
004600941221     D  P04FGS                39     41  0
004700101202     d* bolla
004800101202     d  p04bolp               42     44  0
004900101202     d  p04bors               45     46  0
005000101202     d  p04bosp               47     53  0
005100101202     d  p04boan               54     57  0
005200101202     D  P04ERR                58     58
005300101202     d
005400040604     D* PASSAGGIO DATI A FNLR32R
005500040604     D PARA32          DS
005600040604     D  P32VALORE              1      1    INZ('S')
005700040604     D  P32fgs                 2      4  0
005800040604     D  P32Tla                 5      5
005900040604     D* CAMPO DI OUTPUT
006000040604     D  P32MANCA               5      6
006100040604     D  P32AAS                 7     10  0
006200040604     D  P32LNP                11     13  0
006300040604     D  P32NRS                14     15  0
006400040604     D  P32NSP                16     22  0
006500040604     D  P32bva                23     23
006600040604     D* CAMPO DI input
006700040604     d* data foglio arrivi
006800040604     D  P32dfvv               24     31  0
006900951115     D*
007000911010     D WLBDAT          DS
007100941221     D  G02DAT                 1      8  0
007200941221     D  G02INV                 9     16  0
007300941221     D  G02ERR                17     17
007400941221     D  G02TGI                18     22  0
007500960304     D WGIDAT          DS                  INZ
007600960304     D  GIODAT                 1      8  0
007700960304     D  GIOINV                 9     16  0
007800960304     D  GIOTGI                17     21  0
007900040604     D                 DS
008000040604     D  BLPAAS                 1      4  0
008100040604     D  BLPLNP                 5      7  0
008200040604     D  BLPNRS                 8      9  0
008300040604     D  BLPNSP                10     16  0
008400040604     D  BLPSPE                 1     16  0
008500040604     D                 DS
008600040604     D  COMAAS                 1      4  0
008700040604     D  BLPMGS                 5      8  0
008800040604     D  BLPDSP                 1      8  0
008900040604     D                 DS
009000040617     D  ALF3A                  1      3
009100040604     D  ALF1                   4      4    inz('-')
009200040604     D  ALF5                   5      9
009300040604     D  FVAALF                 1      9
009400040604     D*LINEE   DI ARRIVO
009500040604     D                 DS
009600040604     D  FVALNA                 1      3  0
009700040604     D  FVAFFV                 4    243
009800040604     D  FVAFF2               244    450
009900040604     D  FWAFF3               451    690
010000040604     D  FWAFF4               691    900
010100040604     D  VUOTO                901    903
010200040604     D  FFV                    1    903    DIM(301)                             LINEE FOGLIO
010300040604     D*FILIALE DI SCARICO
010400040604     D                 DS
010500040604     D  LNA2                   1      3  0 inz(0)
010600040604     D  FVAFLP                 4    243
010700040604     D  FVAFL2               244    450
010800040604     D  FWAFL3               451    690
010900040604     D  FWAFL4               691    900
011000040604     D  VUOTO2               901    903
011100040604     D  FLP                    1    903    DIM(301)                             LINEE TRANSITO
011200040604     D
011300040610     D STA             S             66    DIM(16) CTDATA PERRCD(1)             SKIERE STAMPA
011400040604     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
011500161013     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MESSAGGI ERRORE
011600040604     D slnp            S              3  0 DIM(250)                             FV GIA' SELEZ
011700040604     D sfva            S              5  0 DIM(250)                             FV GIA' SELEZ
011800040604     D STF             S              1    DIM(198)                             STAMPA FV ABB
011900060920     D nfvloc          s              5    DIM(5)                               LINEE TRANSITO
012000161013     D $pog            s              3    dim(250)
012100040604     d
012200070110     D DSBRV         E DS                  EXTNAME(FNBRV00F)
012300960304     D*
012400040604     D dslr01        E DS
012500161013     D dlat          E DS
012600040604     D KPJBA         E DS
012700030722     D UT§DSE0F      E DS
012800060919     D FNLV53DS      E DS
012900161012     d Tibs34ds      e ds                  inz
013000161013     D TRUL31DS      E DS
013100161013     D TRUL31DS2     E DS
013200161013     D tibs02ds      E DS
013300161012     d §azute        e ds                  Extname(Azute00f)
013400161012     d                                     dtaara
013500161012     d §Datiute      e ds                  EXTname(ddatiute)
013600161012     d                                     dtaara
013700060920     d savdfv          s                   Like(fvvdfv)
013800060920     d Kffv            s                   Like(fvalai)
013900060920     d Kdfv            s                   Like(fgvdfv)
014000040528     d Knpg            s                   Like(fvvnpg)
014100040604     d Kfva            s                   Like(brvnfv)
014200040528     d Klnp            s                   Like(brvlnp)
014300040528     d Klna            s                   Like(brvlna)
014400040528     d Knrs            s                   Like(brvnrs)
014500040528     d Knsc            s                   Like(brvnsc)
014600040528     d Kfgs            s                   Like(brvfgs)
014700040604     d comspe          s                   Like(blpspe)
014800050616     d comDFV          s                   Like(FVVDFV)
014900161013     d wabi            s                   like(UteAut)
015000040610     d KCAA            s                   Like(ANMCAA) INZ(005)
015100040604     d Wmanca          s              1
015200050616     d Wtipopgm        s              2
015300060919     d NOelabora       s              1
015400060920     d Trovalocal      s              1
015500050616     D*
015600050616     D CTESTAVA1       C                   CONST('** CONTROLLO COLLI DI VALORE -
015700050616     D                                     **')
015800050616     D CTESTAVA2       C                   CONST('**   al Terminal di Arrivo   -
015900050616     D                                     **')
016000050616     D CTESTAar1       C                   CONST('* STATISTICA M.CUBI IN ARRIVO-
016100050616     D                                      *')
016200091026     D CTESTAar2       C                   CONST('* per Linea/Terminal d''Arriv-
016300091026     D                                     o *')
016400161012      // ? PROTOTIPI ?
016500161012      /copy gaitrasrc/srcprotopr,tibs34r
016600000000     C*---------------------------------------------------------------*
016700911010     C* INDICATORI
016800911010     C*---------------------------------------------------------------*
016900921123     C*  01    - SI TRATTA SOLO DI UNA BUSTA
017000161012     C*  05    - protezione filiale gestione
017100060919     C*  10    - on  - proteggo il p.o. in gestione
017200060919     C*  11    - off - filtro per metri vubi di valore
017300050616     C*  11    - on  - filtro per statistica mcubi
017400951115     C*  28    - EMETTE CAMPO MESSAGGI
017500951115     C*  30/32 - DI COMOD0
017600941221     C*  33    - LETTURA FNFVA
017700951117     C*  34/35 - DI COMODO
017800951227     C*  38    - RECORD ALLOCATO IN FNARB06L
017900951117     C*  40/59 - ERRORE
018000971003     C*  60/63 - POSIZIONAMENTO CURSORE PER MESSAGGI DI ERRORE
018100911010     C*  72/73 - DI SUBFILE
018200911010     C*  90    - ERRORE GENERICO
018300911010     C*---------------------------------------------------------------*
018400000000     C     *ENTRY        PLIST
018500000000     C                   PARM                    KPJBA
018600050616     c                   movel     kpjbu         wtipopgm
018700050616     c                   if        wtipopgm='MC'
018800050616     c                   seton                                        11
018900050616     c                   eval      vidtes1=ctestaAR1
019000050616     c                   eval      vidtes2=ctestaAR2
019100050616     c                   else
019200050616     c                   eval      vidtes1=ctestaVA1
019300050616     c                   eval      vidtes2=ctestaVA2
019400050616     c                   endif
019500941221     C*
019600000000     C                   Z-ADD     1             CODUT
019700161012     c                   exsr      datijob
019800060919     c* lo imposto solo la prima volta
019900161012     C                   Z-ADD     dutPOU        VIDpof
020000900511     C*---------------------------------------------------------------*
020100911010     C     INIZIO        TAG
020200911008     C                   EXSR      PULIZ
020300971223     C*
020400921123     C     FOR01         TAG
020500040528     C   09              WRITE     LRv1D03
020600911008     C*
020700040528     C                   EXFMT     LRv1D01
020800161013     c* se il 90 è acceso significa che ci sono stati problemi nelle abilitazioni per cui
020900161013     c*  esco dal pgm
021000161013     c                   if        not *in95
021100040607     c* spengo indicatori
021200040607     c                   setoff                                       289040
021300161013     c                   setoff                                       49
021400030722     C** F3 - FINE LAVORO
021500040604     C   KC              GOTO      FINE
021600161012     c** F18- cambio filiale gestione
021700161012     C   KS              SETON                                        05
021800161012     C   KS              GOTO      FOR01
021900161012     c
022000161012     c                   if        *in05
022100161012     C                   EXSR      CTRFGS
022200161012     c                   if        not *in90
022300161012     c                   setoff                                       05
022400161012     c                   endif
022500161012     c
022600161012     C   90              goto      for01
022700161012     c                   endif
022800921117     C*
022900040528     C* CONTROLLO FOGLIO ARRIVI
023000911008     C                   EXSR      CONTR1
023100930127     C* SE ERRORE
023200930127     C   90              GOTO      FOR01
023300930127     C                   Z-ADD     1             REC
023400911008     C*
023500930127     C* EMISSIONE SFILE
023600911010     C     FORCT2        TAG
023700040528     C                   WRITE     LRv1D02
023800040528     C   72              WRITE     LRv1D04
023900911008     C     FORCT         TAG
024000040604     C                   Z-ADD     1             NRR               5 0
024100040528     C                   EXFMT     LRv1DCT
024200951115     C*
024300951115     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
024400951115     C                   CLEAR                   VCCMSG
024500040604     c                   setoff                                       28  90
024600030722     C** F3 - FINE
024700911030     C   KC              GOTO      FINE
024800930127     C*
024900030722     C** F12 - RITORNO
025000911008     C   KL              GOTO      INIZIO
025100930127     C*
025200040528     C** F7 - INTERROG. SPUNTE arrivo
025300941221     C   KG              DO
025400941221     C                   CLEAR                   PARA04
025500060919     C  n11              Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
025600060919     c   11              if        vidpof>0
025700060919     C                   Z-ADD     VIDpof        P04FGS                         FIL.GESTIONE
025800060919     c                   else
025900060919     C                   Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
026000060919     c                   endif
026100941221     C                   MOVEL     VIDNPG        P04NPG                         CATEGORIA
026200941221     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
026300941221     C                   Z-ADD     99            P04NRS
026400941221     C                   Z-ADD     99            P04NPS
026500941221     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
026600040528     C                   MOVEL     'FNLRV1R '    P04PGM                         RICHIAMATO
026700941221     C                   MOVEL     PARA04        KPJBU
026800941221     C                   CALL      'FNLR04R'
026900920225     C                   PARM                    KPJBA
027000941221     C                   GOTO      FORCT2
027100941221     C                   ENDDO
027200040528     C  NKF              GOTO      FORCT2
027300040528     c
027400030722     C* F6  - ELABORAZIONE
027500050616    1c                   if        *inkf
027600050616     c* COLLI DI VALORE
027700050616    2C                   IF        NOT *IN11
027800050616     C                   EXSR      ELAB
027900050616    3c                   if        *in90
028000040604     c                   goto      forct
028100050616    3c                   endif
028200050616     C
028300050616   x2C                   ELSE
028400050616     C*
028500050616     C* STATISTICA METRI CUBI
028600091026    3C***                IF        NOT *IN72
028700091026     c* Elaboro  anche se nessun foglio abbianto: stampa fogli ancora da abbinare
028800050616     C                   eval      p05DFV=COMDFV
028900050616     C                   eval      p05NFV=COMNFV
029000050616     C                   eval      p05NPG=VIDNPG
029100050616     C                   eval      p05fgs=VIDfgs
029200060919     C                   eval      p05pof=VIDpof
029300060920     c                   movea     nfvloc        p05loc
029400091026     c                   eval      p05tra=vidtra
029500050616     C                   MOVEL     para05        KPJBU
029600050616     C                   MOVEL     'LRV5'        KCOAZ
029700050616     C                   CALL      'BCH10'
029800050616     C                   PARM                    KPJBA
029900050616     C                   GOTO      FINE
030000050616     C*
030100091026   x3C***                ELSE
030200050616     C* SE Nessun foglio caricato, impossibile lancio
030300091026     c***                eval      vccmsg=msg(3)
030400091026     c***                seton                                        28
030500091026     c***                goto      forct
030600091026    3c***                endif
030700050616     c
030800050616    2c                   endif
030900050616    1c                   endif
031000960205     C*
031100040604     C* SE NON HO TROVATO NESSUNA MANCAZA STAMPO
031200040604     C                   IF        WMANCA=' '
031300040604     C                   EXSR      STAMPA
031400040604     C                   ENDIF
031500040604     c* chiudo pgm chiamato
031600040604     c                   clear                   para32
031700040604     c                   movel     'C'           p32tla
031800040604     c                   call      'FNLR32R'
031900040604     c                   parm                    kpjba
032000040604     c                   parm                    DSBRV
032100040604     c                   parm                    para32
032200040604     c*
032300000000     C     FINE          TAG
032400960202     C*
032500060919     c                   clear                   fnlv53ds
032600060919     c                   eval      d53tla='C'
032700060919     c                   call      'FNLV53R'
032800060919     c                   parm                    fnlv53ds
032900161013
033000161013     c                   endif
033100060919     c
033200000000     C                   SETON                                        LR
033300941216     C**************************************************************************
033400161013     C*    ìULIZIA CAMPI
033500941216     C**************************************************************************
033600911008     C     PULIZ         BEGSR
033700941216     C*
033800920709     C* PRIMA VIDEATA
033900911009     C                   MOVEL     *BLANKS       VIDNFV
034000161013     C                   MOVEL     '2'           VIDNPG
034100040528     c
034200040604     C                   TIME                    UTIME             6 0          ORA E DATA
034300040604     C                   TIME                    w0140            14 0          ORA E DATA
034400960205     C* DATA A 8
034500040604     C                   MOVE      w0140         UDATE8            8 0          GG/MM/AAAA
034600960227     C                   Z-ADD     UDATE8        G02DAT
034700960227     C                   MOVEL     *BLANK        G02ERR
034800960227     C                   CALL      'XSRDA8'
034900960227     C                   PARM                    WLBDAT
035000960227     C                   Z-ADD     G02INV        DATEU8            8 0          AAAA/MM/GG
035100040227     c*
035200911009     C* SUBFILE
035300961014     C                   SETON                                        720973
035400040528     C                   WRITE     LRv1DCT
035500961014     C                   SETOFF                                       720473
035600911008     C*
035700040604     C                   CLEAR                   COMSPE
035800091026     C                   CLEAR                   vidtra
035900911008     C                   ENDSR
036000960205     C**************************************************************************
036100040528     C*    CONTROLLI FOGLIO arrivi
036200960205     C**************************************************************************
036300960205     C     CONTR1        BEGSR
036400941216     C*
036500040607     C                   SETOFF                                       303109
036600930208     C*
036700040528     C* NUMERO FOGLIO arrivi
036800911008     C     VIDNFV        IFEQ      *BLANKS
036900911008     C     VIDNFV        OREQ      *ZEROS
037000040604     c                   movel     msg(1)        vidmsg
037100060919     C                   SETON                                        9028
037200911008     C                   GOTO      ENDCT1
037300911008     C*
037400911008     C                   ELSE
037500911008     C* RICERCA
037600950328     C     '?'           SCAN      VIDNFV                                 09
037700911010     C   09              DO
037800941221     C                   CLEAR                   DSLR01
037900941221     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
038000040528     C                   MOVEL     vidnpg        DLRNPG                         CATEGORIA FOGLIO
038100941221     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
038200040528     C                   MOVEL     'FNLRV1R'     DLRPGM                         NOME PROGRAMMA
038300941221     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
038400941221     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
038500060919     C  n11              MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
038600060919     C   11              IF        VIDPOF>0
038700060919     C                   MOVEL     VIDpof        DLRFGS                         FIL.GESTIONE
038800060919     C                   ELSE
038900060919     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
039000060919     C                   ENDIF
039100941221     C                   MOVEL     DSLR01        KPJBU
039200941221     C                   CALL      'FNLR02R'
039300921120     C                   PARM                    KPJBA
039400941221     C                   MOVEL     KPJBU         DSLR01
039500941221     C                   MOVEL     DLRNFV        VIDNFV
039600060919     C                   SETON                                        90
039700911008     C                   GOTO      ENDCT1
039800911008     C                   END
039900911010     C* CONTROLLI
040000911008     C                   TESTN                   VIDNFV               31
040100930127     C  N31              SETON                                        30
040200911008     C*
040300060919     c                   if        not *in30
040400060919     c  n11              eval      kfgs=vidfgs
040500060919     c   11              eval      kfgs=vidpof
040600060919     C                   MOVEL     VIDNFV        COMNFV
040700060919     C     KFVV1         CHAIN     FNFVV01L                           30
040800060919     c                   endif
040900970421     C* INESISTENTE
041000970421     C     *IN30         IFEQ      *ON
041100911008     C* ANNULLATO
041200970421     C     FVVATB        ORNE      *BLANKS
041300040604     c                   movel     msg(1)        vidmsg
041400060919     C                   SETON                                        9028
041500961010     C                   GOTO      ENDCT1
041600911008     C                   END
041700961010     C                   END
041800050616     C*
041900050616     C                   MOVEL     FVVDFV        COMDFV
042000930127     C*
042100930127     C* CARICAMENTO SFILE
042200930127     C                   EXSR      CARSFL
042300060919     c* Se non ho caricato nessun foglio, errore
042400060919     c                   if        nrr=0  and vidpof<>vidfgs
042500060919     c                   movel     msg(4)        vidmsg
042600060919     C                   SETON                                        9028
042700060919     C                   GOTO      ENDCT1
042800060919     C                   END
042900930127     C*
043000930127     C     ENDCT1        ENDSR
043100941216     C**************************************************************************
043200941216     C*    CARICAMENTO SFILE
043300941216     C**************************************************************************
043400930127     C     CARSFL        BEGSR
043500941216     C*
043600930519     C* DATA FOGLIO ARRIVI
043700920709     C                   MOVE      FVVDFV        G02INV
043800920709     C                   MOVE      *ZEROS        G02DAT
043900920709     C                   MOVEL     3             G02ERR
044000941221     C                   CALL      'XSRDA8'
044100920709     C                   PARM                    WLBDAT
044200920709     C                   Z-ADD     G02DAT        VIDDFA
044300911008     C*
044400040528     C* Seleziona per controllo
044500040528     C                   MOVEL     ' '           VI2Sel                          STAMPA ANOM
044600060920     C                   Z-ADD     0             NRR
044700060920     c
044800060920     c* Per mc in arrivo carico il locale con p.o. foglio arrivi
044900060920     c                   if        *in11
045000060920     c                   exsr      Cerloc
045100060920     c                   endif
045200060920     c
045300060920     C                   CLEAR                   VI2LOC
045400960205     C*
045500920428     C* CARICO FOGLI VIAGGIO GIA' ABBINATI SE ESISTONO
045600941221     C     KFVA2         SETLL     FNFVA02L
045700941221     C     KFVA2         READE     FNFVA02L                               30
045800911008     C*
045900040604    1C     *IN30         DOWEQ     '0'
046000921119     C                   MOVEL     FVANFV        VI2FVA                          NUM FOGLIO
046100921119     C                   MOVEL     FVALNP        VI2LNP                          LINEA PART
046200960205     C*
046300960205     C                   EXSR      DATARR
046400951115     C*
046500060919     c* solo se da elaborare
046600060919     c                   if        noelabora=' '
046700060919     c
046800040604    2c                   if        fvaatr<>*blanks
046900040604     c                   seton                                        41
047000040604     c                   else
047100040604     c                   setoff                                       41
047200040604    2c                   endif
047300040604     c
047400911010     C                   ADD       1             NRR
047500040604     C                   WRITE     LRV1DSF
047600060919    2c                   endif
047700911008     C*
047800941221     C     KFVA2         READE     FNFVA02L                               30
047900040604    1C                   ENDDO
048000040528    c
048100960626     C                   Z-ADD     NRR           SAVNRR            5 0
048200961014     C     NRR           COMP      0                                      72
048300960626     C*
048400930127     C                   ENDSR
048500960111     C**************************************************************************
048600960111     C* CONTROLLO COSA PROPORRE NELLA DATA/ORA ARRIVO REALE
048700000628     C* ... carica anche P.O. gestione POSTE
048800960111     C**************************************************************************
048900960111     C     DATARR        BEGSR
049000960205     C****
049100960205     C** D A T A / O R A     A R R I V O
049200960205     C*
049300960205    2C     FVAAED        IFGT      0
049400960205     C* DATA ARRIVO REALE
049500960205     C                   Z-ADD     FVAAED        G02INV
049600960205     C                   MOVE      *ZEROS        G02DAT
049700960205     C                   MOVEL     3             G02ERR
049800960205     C                   CALL      'XSRDA8'
049900960205     C                   PARM                    WLBDAT
050000960205     C                   MOVEL     G02DAT        VI2AED
050100960205     C* ORA ARRIVO REALE
050200960205     C                   MOVEL     FVAAEH        VI2AEH
050300040528     c                   else
050400040528     c                   clear                   vi2aed
050500040528     c                   clear                   vi2aeh
050600960205    2C                   ENDIF
050700960202     C*
050800960202     C* GIRO DATA PARTENZA
050900960202     C                   MOVE      FVADFV        G02INV
051000960202     C                   MOVE      *ZEROS        G02DAT
051100960202     C                   MOVEL     3             G02ERR
051200960202     C                   CALL      'XSRDA8'
051300040528     c
051400960202     C                   PARM                    WLBDAT
051500971223     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
051600960202     C                   MOVEL     G02DAT        VI2DFV
051700960202     C*
051800960202     C* SE E' SOLO BUSTA METTO CAMPO IN R.I.
051900960202    1C     FVAATR        IFEQ      'T'
052000960202     C                   SETON                                        01
052100960202   X1C                   ELSE
052200960202     C                   SETOFF                                       01
052300960202    1C                   END
052400960402     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
052500960402     C*  VISUALIZZO CON 2 ASTERISCHI
052600960402     C     KARB          SETLL     FNARB06L                               34
052700960402     C     *IN34         IFEQ      *ON
052800960402     C                   CLEAR                   VI2AST
052900960402     C                   ELSE
053000960402     C                   MOVEL     '**'          VI2AST
053100960402     C                   ENDIF
053200060919     c
053300060919     C* SE IL P.O. GESTIONE È DIVERSO DAL p.o. foglio
053400060919     c*  verifico se il foglio è relativo alla linea di arrivo richiesta
053500060919     c                   clear                   Noelabora
053600060919     c   11              if        vidpof<>vidfgs
053700060919     c                   clear                   fnlv53ds
053800060919     c                   eval      d53tfo='A'
053900060919     c                   eval      d53nfv=fvanfv
054000060919     c                   eval      d53npg=1
054100060919     c                   eval      d53fgs=fvalnp
054200060919     c                   eval      d53lai=fvalai
054300060919     c                   eval      d53abb='B'
054400060919     c                   eval      d53lna=vidfgs
054500060919     c                   call      'FNLV53R'
054600060919     c                   parm                    fnlv53ds
054700060919     c                   if        d53err<>' '
054800060919     c                   eval      noelabora='N'
054900060919     c                   endif
055000060919     c                   endif
055100000628     C* P.O.GESTIONE
055200000628     C     KFWA1         CHAIN     FNFWA01L                           74
055300000628     C     *IN74         IFEQ      *OFF
055400000628     C     FWAFGS        ANDNE     0
055500000628     C                   Z-ADD     FWAFGS        WORKPT            3 0
055600000628     C                   ELSE
055700000628     C                   Z-ADD     FVALNP        WORKPT            3 0
055800000628     C                   ENDIF
055900960111     C*
056000960111     C                   ENDSR
056100060920     C**************************************************************************
056200060920     C* Cerco foglio locale con p.o. foglio arrivi
056300060920     C**************************************************************************
056400060920     C     Cerloc        BEGSR
056500060920     c* salvo la data foglio arrivi
056600060920     c                   eval      savdfv=fvvdfv
056700060920     c                   clear                   trovalocal
056800060920     c                   clear                   yy
056900060920     c                   movel     *zeros        nfvloc
057000060920     c
057100060920     c     kfgv          setll     fnfgv02l
057200060920     c     vidpof        readpe    fnfgv02l
057300060920     c
057400060920     c* Cerco il primo foglio locale con data < data foglio arrivi
057500060920     c                   dow       not %eof(fnfgv02l)
057600060920     c* se trovato locale valido --> ok
057700060920     c                   if        fgvttr='L' and fgvatb=' '
057800060920     c                   leave
057900060920     c                   else
058000060920     c     vidpof        readpe    fnfgv02l
058100060920     c                   endif
058200060920     c                   enddo
058300060920     c
058400060920     c* se trovato controllo di non avere un foglio arrivi tra la data
058500060920     c*  foglio locale e la data foglio arrivi richiesta:
058600060920     c*  se c'e' non carico il locale altrimenti lo carico
058700060920     c                   if        not %eof(fnfgv02l)
058800060920     c     kfvv8         setgt     fnfvv08l
058900060920     c     kfvv8b        reade     fnfvv08l
059000060920     c                   dow       not %eof(fnfvv08l) and fvvatb<>' '
059100060920     c     kfvv8b        reade     fnfvv08l
059200060920     c                   enddo
059300060920     c* Verifico la data del fogliio arrivi trovato
059400060920     c                   if        not %eof(fnfvv08l) and
059500060920     c                             fvvdfv>=savdfv
059600060920     c                   eval      trovalocal='S'
059700060920     c                   endif
059800060920     c                   endif
059900060920     c
060000060920     c* Locale trovato: lo carico nel sfl
060100060920    1c                   if        trovalocal='S'
060200060920     c                   eval      kdfv=fgvdfv
060300060920     c     kfgv2         setll     fnfgv02l
060400060920     c     kfgv2         reade     fnfgv02l
060500060920     c
060600060920    2c                   dow       not %eof(fnfgv02l)
060700060920     c
060800060920    3c                   if        fgvttr='L' and fgvatb=' '
060900060920     c
061000060920     c* imposto ciclo di lettura per p.o. e data
061100060920     c
061200060920     c*
061300060920     C                   MOVEL     FgvNFV        VI2FVA                          NUM FOGLIO
061400060920     C                   MOVEL     FgvLNP        VI2LNP                          LINEA PART
061500060920     c                   clear                   vi2aed
061600060920     c                   clear                   vi2aeh
061700060920     c
061800060920     C                   MOVE      FgvDFV        G02INV
061900060920     C                   MOVE      *ZEROS        G02DAT
062000060920     C                   MOVEL     3             G02ERR
062100060920     C                   CALL      'XSRDA8'
062200060920     c
062300060920     C                   PARM                    WLBDAT
062400060920     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
062500060920     C                   MOVEL     G02DAT        VI2DFV
062600060920     c
062700060920     c                   setoff                                       01
062800060920     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
062900060920     C*  VISUALIZZO CON 2 ASTERISCHI
063000060920     C     KARB          SETLL     FNARB06L                               34
063100060920     C     *IN34         IFEQ      *ON
063200060920     C                   CLEAR                   VI2AST
063300060920     C                   ELSE
063400060920     C                   MOVEL     '**'          VI2AST
063500060920     C                   ENDIF
063600060920     c
063700060920     C* SE IL P.O. GESTIONE È DIVERSO DAL p.o. foglio
063800060920     c*  verifico se il foglio è relativo alla linea di arrivo richiesta
063900060920     c                   clear                   Noelabora
064000060920    4c                   if        vidpof<>vidfgs
064100060920     c                   clear                   fnlv53ds
064200060920     c                   eval      d53tfo='G'
064300060920     c                   eval      d53nfv=fgvnfv
064400060920     c                   eval      d53npg=1
064500060920     c                   eval      d53fgs=fgvlnp
064600060920     c                   eval      d53lai=vidpof
064700060920     c                   eval      d53abb='B'
064800060920     c                   eval      d53lna=vidfgs
064900060920     c                   call      'FNLV53R'
065000060920     c                   parm                    fnlv53ds
065100060920    5c                   if        d53err<>' '
065200060920     c                   eval      noelabora='N'
065300060920    5c                   endif
065400060920    4c                   endif
065500060920     C*
065600060920     c* solo se da elaborare
065700070529    4c                   if        noelabora=' '   and yy<5
065800060920     C                   EVAL      VI2LOC='LOCALE'
065900060920     C
066000060920     C                   ADD       1             NRR
066100060920     C                   WRITE     LRV1DSF
066200060920     c                   add       1             yy
066300060920     c                   movel     fgvnfv        nfvloc(yy)
066400060920    4c                   endif
066500060920    3c                   endif
066600060920     c
066700060920     c     kfgv2         reade     fnfgv02l
066800060920    2c                   enddo
066900060920    1c                   endif
067000060920     C*
067100060920     C                   ENDSR
067200941216     C**************************************************************************
067300941216     C*    ELABORAZIONE ABBINAMENTO
067400941216     C**************************************************************************
067500911008     C     ELAB          BEGSR
067600911010     C*
067700930129     C                   Z-ADD     1             NRR
067800930129     C*
067900040528     C* LEGGO TUTTI I FOGLI VIAGGIO ABBINATI  selezionati
068000040604     c* li carico in una schiera perchè li devo stampare
068100040604     c                   clear                   xx                3 0
068200040604     c                   clear                   slnp
068300040604     c                   clear                   sfva
068400951228    1C     NRR           DOWLE     SAVNRR
068500040604     C     NRR           CHAIN     LRv1DSF                            30
068600930129     C*
068700040528    2c                   if        not *in30  and vi2sel='I'
068800040604     c                   add       1             xx
068900040604     c                   z-add     vi2lnp        slnp(xx)
069000040604     c                   z-add     vi2fva        sfva(xx)
069100040604    2c                   endif
069200040604     c                   add       1             nrr
069300040604    1c                   enddo
069400040604     c
069500040604     c* Se non selezionato nessun foglio errore
069600040604    1c                   if        xx=0
069700040604     c                   movel     msg(2)        vccmsg
069800040604     c                   seton                                        2890
069900040604   x1c                   else
070000040604     c*
070100040604     C                   Z-ADD     80            LUNG             15 5
070200040604     C                   MOVEL     *BLANKS       COMMAN           80
070300040604     C                   MOVEA     CMD           COMMAN
070400040604     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
070500040604     C                   PARM                    COMMAN
070600040604     C                   PARM                    LUNG
070700040604     C*
070800040604RM*  C                   OPEN      PRTF198
070900040604     C                   SETON                                        OF
071000040528     c
071100040604     c* Leggo tutti i fogli per elaborare le linee giuste
071200040604     c                   z-add     1             xx
071300040604    2c                   dow       slnp(xx)>0
071400040604     c                   z-add     slnp(xx)      vi2lnp
071500040604     c                   z-add     sfva(xx)      vi2fva
071600040604     c
071700040528     C     KFVA1         CHAIN     FNFVA01L
071800040528    3C                   IF        %found(fnfva01l)
071900040528     C*
072000040528     C     KFWA1         CHAIN     FNFWA01L
072100040528    4C                   IF        NOT %FOUND(FNFWA01L)   OR  fwaatb<>' '
072200040528     C                   CLEAR                   FWAFF3
072300040528     C                   CLEAR                   FWAFF4
072400040528     C                   CLEAR                   FWAFL3
072500040528     C                   CLEAR                   FWAFL4
072600040528    4C                   ENDIF
072700040528     C*
072800040528     C                   Z-ADD     FVALNA        LNA2
072900040604     C                   Z-ADD     1             X                 4 0
073000040528     C                   MOVEL     FVALAI        ALF3              3
073100040528     C     ALF3          LOOKUP    FLP(X)                                 31
073200040528     C*
073300040528    4C                   DOW       *in31
073400040528     C                   MOVEL     FFV(X)        kffv
073500911010     C*
073600941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
073700040528     C                   Z-ADD     VI2FVA        kfva
073800070110     C     KBRV1         SETLL     FNBRV02L
073900070110     C     KBRV1         READE     FNBRV02L                               30
074000911010     C*
074100040528    5C     *IN30         DOWEQ     '0'
074200040528     c                   EXSR      ctrarr
074300911010     C*
074400070110     C     KBRV1         READE     FNBRV02L                               30
074500040528    5C                   ENDDO
074600040528     c*
074700040528     C                   ADD       1             x
074800040528     C     ALF3          LOOKUP    FLP(X)                                 31
074900040528    4C                   ENDdo
075000040604    3c                   endif
075100921120     C*
075200040604     C                   ADD       1             xx
075300040604    2C                   ENDDO
075400040604     c
075500040604    1c                   endif
075600911008     C                   ENDSR
075700040528     C**************************************************************************
075800040528     C*    vedo se collo arrivato per creare anomalia collo part non arr
075900040528     C**************************************************************************
076000040528     C     CTRARR        BEGSR
076100040603     c* richiamo FNLR32R per fare i controlli e far creare l'anomalia:
076200040603     c*  se creo l'anomalia, eseguo la stampa
076300040603     c
076400040604     c                   clear                   para32
076500040604     c                   movel     'S'           p32valore
076600040604     c                   z-add     vidfgs        p32fgs
076700040604     c                   eval      p32dfvv=fvvdfv
076800040603     c                   call      'FNLR32R'
076900040603     c                   parm                    kpjba
077000040604     c                   parm                    DSBRV
077100040604     c                   parm                    para32
077200040604     C* SE COLLO DI VALORE MANCANTE, STAMPO
077300040604     c                   if        p32manca='S'
077400040604     C* MEMORIZZO  DI AVER TROVATO ALMENO UNA MANCANZA
077500040604     C                   EVAL      WMANCA='S'
077600040604     C
077700040604     c                   exsr      stampa
077800040604     c                   endif
077900040604     c                   endsr
078000040604     C**************************************************************************
078100040604     C* STAMPO mNCANZA COLLO DI VALORE
078200040604     C**************************************************************************
078300040604     C     STAMPA        BEGSR
078400040604     C*
078500040604     C* STAMPA TESTATA
078600040604    1C   OF              DO
078700040604     C* PRIMA PARTE TESTATA
078800040604     C                   EXCEPT    TES
078900040604     C*
079000040604     C                   Z-ADD     1             YY                3 0
079100040604     C                   Z-ADD     1             II                3 0
079200040604     C                   MOVEA     *BLANKS       STF
079300040604     C*
079400040604    2C     slnp(YY)      DOWgt     0
079500040604     C*
079600040604    3C     II            IFEQ      191
079700040604     C                   EXCEPT    TES3
079800040604     C                   MOVEA     *BLANKS       STF
079900040604     C                   Z-ADD     1             II
080000040604    3C                   END
080100040604     C*
080200040617     C                   MOVEL     slnp(YY)      ALF3A
080300040604     C                   MOVEL     sfva(YY)      ALF5
080400040604     C                   MOVEA     FVAALF        STF(II)
080500040604     C                   ADD       10            II
080600040604     C**
080700040604     C                   ADD       1             YY
080800040604    2C                   ENDdo
080900040604     C*
081000040604    2C     II            IFGT      1
081100040604     C                   EXCEPT    TES3
081200040604    2C                   END
081300040604     C*
081400040604     C* STAMPO TERZA PARTE TESTATA
081500040604     C                   EXCEPT    TES2
081600040604     C*
081700040604     C                   SETOFF                                       OF
081800040604    1C                   END
081900040604     C* Stampo collo mancante
082000040604     C                   IF        WMANCA='S'
082100040604     C* CHAIN SPEDIZIONE
082200040604     C     KBLP          CHAIN     FNBLP01L
082300040604     C                   IF        %FOUND(FNBLP01L)
082400040604     C* SE C'E' BOLLA E A CAMBIO SPEDIZIONE STAMPO I DATI DELLA SPED
082500040604     C     COMSPE        IFNE      BLPSPE
082600040604     C                   SETON                                        05
082700040604     C                   MOVEL     BLPSPE        COMSPE
082800040604     C                   ENDIF
082900040604     C                   ELSE
083000040604     C                   SETOFF                                       05
083100040604     C                   ENDIF
083200040610     C* CHAIN ANOMALIE PER AVERE DATA E ORA DI CREAZIONE
083300040610     C                   CLEAR                   STADAN
083400040610     C                   CLEAR                   STAOAN
083500040610     C     KANM          CHAIN     FNANM01L
083600040610     C                   IF        %FOUND(FNANM01L)
083700040611     C                   MOVEL     ANMFLO        w0060             6 0          AAAA/MM/GG
083800040611     C                   z-add     w0060         G02INV                         AAAA/MM/GG
083900040610     C                   MOVE      *ZEROS        G02DAT
084000040610     C                   MOVEL     3             G02ERR
084100040610     C                   CALL      'XSRDA8'
084200040610     C                   PARM                    WLBDAT
084300040611     C                   MOVEL     G02DAT        STADAN            8 0          GG/MM
084400040610     C*
084500040610     C                   MOVE      ANMFLO        STAOAN            4 0
084600040610     C                   ENDIF
084700040604     C*
084800040604     C* DATA SPEDIZIONE
084900040604     C   05              DO
085000040604     C                   Z-ADD     BLPAAS        COMAAS                         ANNO (4 CIFRE)
085100040604     C                   MOVE      BLPDSP        G02INV                         AAAA/MM/GG
085200040604     C                   MOVE      *ZEROS        G02DAT
085300040604     C                   MOVEL     3             G02ERR
085400040604     C                   CALL      'XSRDA8'
085500040604     C                   PARM                    WLBDAT
085600040604     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
085700040604     C*
085800040604     C* SEGNACOLLI NON CONSECUTIVI
085900040604     C     BLPFNS        COMP      'S'                                    04
086000040604     C* SERIE>< 0
086100040604     C     BLPNRS        COMP      0                                  0303
086200040604     C                   END
086300040604     C* MITTENTE E DESTINATARIO DI 17 E 20
086400040604     C                   MOVEL     BLPRSM        STARSM           17
086500040604     C                   MOVEL     BLPRSD        STARSD           20
086600040604     C*
086700040604     C* STAMPO DETTAGLIO
086800040604     C                   EXCEPT    DET
086900040604     C                   SETOFF                                       05
087000040604     c                   else
087100040604     c                   except    OK
087200040604     c                   endif
087300040604     c                   endsr
087400161012      *-----------------------------------------------------***********
087500161012     C     CTRFGS        BEGSR
087600161012     c     vidfgs        chain     azorg01l
087700161012     c                   if        %found(azorg01l)
087800161013     c                   movel     orgdes        desfgs
087900161012     c                   else
088000161013     C                   MOVEL     msg(6)        VidMSG
088100161013     C                   SETON                                        499028
088200161013     C                   leavesr
088300161012     c                   endif
088400161012     c
088500161012     C                   SETOFF                                       90
088600161013     c                   movel     vidfgs        w003a             3
088700161013    2C                   if        %lookup(w003a:$pog)=0
088800161013     C                   MOVEL     msg(6)        VidMSG
088900161012     C                   SETON                                        499028
089000161012     C                   leavesr
089100161012     C                   ENDIF
089200161012
089300161012     c                   endsr
089400960202     C*
089500951115     C*--- SR INIZIALE -----------------------------------------------*
089600951115     C     *INZSR        BEGSR
089700951115     C*
089800951115     C****  KLIST  ****
089900040604     C     KARB          KLIST
090000040604     C                   KFLD                    vi2lnp
090100040604     C                   KFLD                    VI2FVA
090200060919     C     KARB1         KLIST
090300060919     C                   KFLD                    vi2lnp
090400060919     C                   KFLD                    VI2FVA
090500060919     C                   KFLD                    vidfgs
090600951115     C     KFVV1         KLIST
090700040604     C                   KFLD                    vidNPG
090800951115     C                   KFLD                    COMNFV            5 0
090900060919     C                   KFLD                    kFGS
091000060920     C     KFVV8         KLIST
091100060920     C                   KFLD                    vidpof
091200060920     C                   KFLD                    vidnpg
091300060920     C                   KFLD                    fgvdfv
091400060920     C     KFVV8b        KLIST
091500060920     C                   KFLD                    vidpof
091600060920     C                   KFLD                    vidnpg
091700060920     C     KFgv          KLIST
091800060920     C                   KFLD                    vidpof
091900060920     C                   KFLD                    fvvdfv
092000060920     C     KFgv2         KLIST
092100060920     C                   KFLD                    vidpof
092200060920     C                   KFLD                    kdfv
092300951115     C     KFVA1         KLIST
092400040528     C                   KFLD                    VI2LNP
092500951115     C                   KFLD                    VI2FVA
092600040528     C                   KFLD                    VIDFGS
092700951115     C     KFVA2         KLIST                                                  FNFVA02L
092800040604     C                   KFLD                    vidNPG                         ..CATEGORIA
092900951115     C                   KFLD                    COMNFV                         ..FOGLIO
093000060919     C                   KFLD                    kfgs
093100951115     C     KBRV1         KLIST                                                  FNBRV02L
093200951115     C                   KFLD                    KNPG                           ..CATEGORIA
093300040528     C                   KFLD                    kfva                           ..NR.FOGLIO
093400040528     C                   KFLD                    vi2LNP                         ..FIL.GESTIONE
093500040528     C                   KFLD                    kffv                           ..LNA
093600000628     C     KFWA1         KLIST
093700000628     C                   KFLD                    FVALNP
093800000628     C                   KFLD                    FVANFV
093900000628     C                   KFLD                    FVALAI
094000040604     C     Kblp          KLIST                                                  FIBRV09L
094100040604     C                   KFLD                    P32Aas                         ..LNP
094200040604     C                   KFLD                    P32lnp                         ..LNA
094300040604     C                   KFLD                    P32nrs                         ..SERIE
094400040604     C                   KFLD                    P32nsp                         ..SEGNACOLLO
094500040610     C     KANM          KLIST                                                  FIBRV09L
094600040610     C                   KFLD                    P32Aas                         ..LNP
094700040610     C                   KFLD                    P32lnp                         ..LNA
094800040610     C                   KFLD                    P32nrs                         ..SERIE
094900040610     C                   KFLD                    P32nsp                         ..SEGNACOLLO
095000040610     C                   KFLD                    KCAA                           ..SEGNACOLLO
095100951115     C*
095200951115     C                   ENDSR
095300161013     c*//--------------------------------------------------------------
095400161013     c*//?Reperimento Dati del job (Utente/Operativi).
095500161013     c*//--------------------------------------------------------------
095600161013     C     Datijob       BEGSR
095700161013     c                   in(E)     *dtaara
095800161013    1c                   If        %error  or RSUT = *blanks
095900161013     c                   Clear                   Tibs34ds
096000161013     c                   Call      'TIBS34R'
096100161013     c                   Parm                    Tibs34ds
096200161013     c                   In        *dtaara
096300161013    1c                   EndIf
096400161013
096500161013     c                   Clear                   wabi
096600161013     c                   Clear                   vidmsg
096700161013     c                   Clear                   dLat
096800161013     c                   setoff                                       9095
096900161013     c
097000161013     C                   Z-ADD     dutPOU        VIDFGS
097100161013     c     dutpou        chain     azorg01l
097200161013     c                   movel     orgdes        desfgs
097300161013
097400161013      * Verifica errori e autorità profilo
097500161013s   1c                   Select
097600161013      * se ho errori nei dati utente esco dal pgm
097700161013w   1c                   When      DutErr = 'E'
097800161013     C                   MOVEL     MSG(5)        vidmsg
097900161013     C                   SETON                                        902895
098000161013      * se non c'è l'abilitazione
098100161013      * --> se 1° livello, abilitazioni al terminal
098200161013      *     se 2° livello, abilitazioni al punto operativo
098300161013      *     se sede è impossibile ma se capita mando a fine il pgm
098400161013
098500161013w   1c                   When      UteAut = *Blanks
098600161013if  2c                   If        DutLpo = '1'
098700161013     c                   Eval      wabi   = 'TP'
098800161013e   2c                   EndIf
098900161013if  2c                   If        DutLpo = '2'
099000161013     c                   Eval      wabi   = 'PO'
099100161013e   2c                   EndIf
099200161013if  2c                   If        DutLpo = 'S'
099300161013     C                   MOVEL     MSG(5)        vidmsg
099400161013     C                   SETON                                        902895
099500161013e   2c                   EndIf
099600161013      * carica le abilitazioni del profilo
099700161013x   1c                   Other
099800161013     c                   Eval      wabi = UteAut
099900161013e   1c                   EndSl
100000161013
100100161013      * controllo se ok l'abilitazione dell'utente
100200161013     c                   Clear                   Tibs02ds
100300161013     c                   Eval      T02Mod = 'C'
100400161013     c                   Eval      T02tla = 'L'
100500161013     c                   Eval      T02Sif = knsif
100600161013     c                   Eval      T02Cod = 'LAT'
100700161013     c                   Movel(p)  wabi          T02Ke1
100800161013     c                   Call      'TIBS02R'
100900161013     c                   Parm                    kpjba
101000161013     c                   Parm                    Tibs02ds
101100161013if  1c                   If        T02Err = *Blanks
101200161013     c                   Eval      dLat = T02Uni
101300161013e   1c                   EndIf
101400161013      * errore o non abilitato
101500161013if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
101600161013     C                   MOVEL     MSG(5)        vidmsg
101700161013     C                   SETON                                        902895
101800161013   x1c                   Else
101900161013      * Reperimento dei P.O. gestibili dall'utente
102000161013     c                   clear                   TRUL31DS
102100161013     c                   clear                   TRUL31DS2
102200161013     c                   eval      I31abi = wabi
102300161013     c                   eval      I31cdi = DUTdis
102400161013     c                   eval      I31car = DUTare
102500161013     c                   eval      I31cpo = DUTpou
102600161013     c                   call      'TRUL31R'
102700161013     c                   parm                    KPJBA
102800161013     c                   parm                    TRUL31DS
102900161013     c                   parm                    TRUL31DS2
103000161013if  2c                   if        O31pog > *zeros
103100161013     c                   movea     O31pog        $pog
103200161013x   2c                   else
103300161013     C                   MOVEL     MSG(5)        vidMSG
103400161013     C                   SETON                                        902895
103500161013e   2c                   endif
103600161013    1c                   endif
103700161013     c
103800161013
103900161013     C                   ENDSR
104000040604     C**************************************************************************
104100040604RM*  OPRTF198   E            TES              02
104200040604     O                       STA(1)              66
104300040604     O                       STA(2)             132
104400040604     O                       STA(13)            198
104500040604     O                       RSUT                20
104600040604     O                       UDATE8             181 '  /  /    '
104700040604     O                       PAGE          Z    198
104800040604     O          E            TES         1
104900040604     O                       UTIME              179 '  :  :  '
105000040604     O          E            TES         2
105100040604     O                       STA(3)              66
105200040604RM*  O                       VIDFGS              22
105300040604     O                       DESFGS              43
105400040604     O                       COMNFV        Z     56
105500040604     O                       VIDDFA        8     73
105600040604     O          E            TES         2
105700040604     O                       STA(4)              66
105800040604     O          E            TES3        1
105900040604     O                       stf                198
106000040604     O          E            TES2        2
106100040604     O                       STA(5)              66
106200040604     O                       STA(8)             132
106300040610     O                       STA(14)            198
106400040604     O          E            TES2        1  1
106500040604     O                       STA(6)              66
106600040611     O                       STA(9)             132
106700040611     O                       STA(15)            198
106800040604     O*
106900040604     O          E            DET         1
107000040604     O                  05   BLPAAS        Z
107100040604     O                  05   BLPLNP        Z   +  1
107200040604     O                  05 03BLPNRS        Z   +  1
107300040604     O                  05 03                  +  0 '-'
107400040604     O                  05   BLPNSP        Z   +  0
107500040604     O                  05   STADSP            +  1 '  /  '
107600040604     O                  05   BLPLNA        Z   +  1
107700040604     O                  05   STARSM            +  1
107800040604     O                  05   STARSD            +  1
107900040604     O                  05N04BLPNCD        Z   +  1
108000040604     O                  05N04                  +  0 '/'
108100040604     O                  05N04BLPNCA        Z   +  0
108200040604     O                  05 04                    84 'NON SEQUENZIALI'
108300040604     O                  05   BLPZNC            +  1
108400040604     O                       BRVLNP            +  4
108500040604     O                       BRVLNA            +  2
108600040604     O                       BRVNRS        Z   +  3
108700040604     O                       BRVNSC        Z   +  4
108800040610     O                       STADAN            +  8 '  /  /    '
108900040610     O                       STAOAN            +  2 '  :  '
109000040604     O          E            OK          3
109100040604     O                       STA(11)             66
109200040604     O          E            OK          0  0
109300040604     O                       STA(11)             66
109400040604     O**************************************************************************
109500040604** STA
109600040617XXXXXXXXXXXXXXXXXXXX                                                 1
109700040604 ** COLLI DI VALORE MANCANTI AL TERMINAL DI ARRIVO **                2
109800040604FOGLIO ARRIVI: DI  XXX-XXXXXXXXXXXXXXXXXXXX   N.RO XXXXX   DEL XX/   3
109900040604FOGLI VIAGGIO PARTENZA SELEZIONATI:                                  4
110000040604    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO         5
110100040604ANNO LNP SER NUMERO  DATA ARR                                        6
110200040604XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx   7
110300040610     SEGNACOLLI    ZON   C O L L O   M A N C A N T E     CREAZIONE   8
110400040610      DAL /   AL   ARR   LNP  LNA  SERIE   NUMERO           DATA     9
110500040610XX XXXXXXX/XXXXXXX XX    XXX  XXX   XX    XXXXXXX        XX/XX/XXX  10
110600040604             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
110700040604 **  FINE  STAMPA  **                                               12
110800040604                         FNLRV1R                          PAG.XXXX  13
110900040610 ANOMALIA                                                           14
111000040610    ORA                                                             15
111100040610X  XX:XX                                                            16
111200040611**
111300040604OVRPRTF FILE(PRTF198) USRDTA('ColValore')
111400000606**  MSG
111500040604Foglio arrivi errato                                                          1
111600161013Effettuare almeno una selezione per la elaborazione                           2
111700161013Nessun foglio da elaborare                                                    3
111800161013Nessun foglio partenza abbinato relativo al p.o. in gestione indicato         4
111900161013Errori nelle abilitazioni utente                                              5
112000161013Filiale errata o non in gestione all'utente                                   6
