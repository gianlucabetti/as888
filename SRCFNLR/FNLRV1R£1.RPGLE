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
004700040604     D* PASSAGGIO DATI A FNLR32R
004800040604     D PARA32          DS
004900040604     D  P32VALORE              1      1    INZ('S')
005000040604     D  P32fgs                 2      4  0
005100040604     D  P32Tla                 5      5
005200040604     D* CAMPO DI OUTPUT
005300040604     D  P32MANCA               5      6
005400040604     D  P32AAS                 7     10  0
005500040604     D  P32LNP                11     13  0
005600040604     D  P32NRS                14     15  0
005700040604     D  P32NSP                16     22  0
005800040604     D  P32bva                23     23
005900040604     D* CAMPO DI input
006000040604     d* data foglio arrivi
006100040604     D  P32dfvv               24     31  0
006200951115     D*
006300911010     D WLBDAT          DS
006400941221     D  G02DAT                 1      8  0
006500941221     D  G02INV                 9     16  0
006600941221     D  G02ERR                17     17
006700941221     D  G02TGI                18     22  0
006800960304     D WGIDAT          DS                  INZ
006900960304     D  GIODAT                 1      8  0
007000960304     D  GIOINV                 9     16  0
007100960304     D  GIOTGI                17     21  0
007200040604     D                 DS
007300040604     D  BLPAAS                 1      4  0
007400040604     D  BLPLNP                 5      7  0
007500040604     D  BLPNRS                 8      9  0
007600040604     D  BLPNSP                10     16  0
007700040604     D  BLPSPE                 1     16  0
007800040604     D                 DS
007900040604     D  COMAAS                 1      4  0
008000040604     D  BLPMGS                 5      8  0
008100040604     D  BLPDSP                 1      8  0
008200040604     D                 DS
008300040617     D  ALF3A                  1      3
008400040604     D  ALF1                   4      4    inz('-')
008500040604     D  ALF5                   5      9
008600040604     D  FVAALF                 1      9
008700040604     D*LINEE   DI ARRIVO
008800040604     D                 DS
008900040604     D  FVALNA                 1      3  0
009000040604     D  FVAFFV                 4    243
009100040604     D  FVAFF2               244    450
009200040604     D  FWAFF3               451    690
009300040604     D  FWAFF4               691    900
009400040604     D  VUOTO                901    903
009500040604     D  FFV                    1    903    DIM(301)                             LINEE FOGLIO
009600040604     D*FILIALE DI SCARICO
009700040604     D                 DS
009800040604     D  LNA2                   1      3  0 inz(0)
009900040604     D  FVAFLP                 4    243
010000040604     D  FVAFL2               244    450
010100040604     D  FWAFL3               451    690
010200040604     D  FWAFL4               691    900
010300040604     D  VUOTO2               901    903
010400040604     D  FLP                    1    903    DIM(301)                             LINEE TRANSITO
010500040604     D
010600040610     D STA             S             66    DIM(16) CTDATA PERRCD(1)             SKIERE STAMPA
010700040604     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
010800060919     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MESSAGGI ERRORE
010900040604     D slnp            S              3  0 DIM(250)                             FV GIA' SELEZ
011000040604     D sfva            S              5  0 DIM(250)                             FV GIA' SELEZ
011100040604     D STF             S              1    DIM(198)                             STAMPA FV ABB
011200060920     D nfvloc          s              5    DIM(5)                               LINEE TRANSITO
011300040604     d
011400070110     D DSBRV         E DS                  EXTNAME(FNBRV00F)
011500960304     D*
011600040604     D dslr01        E DS
011700040604     D CNCR80        E DS
011800040604     D og143         E DS
011900040604     D KPJBA         E DS
012000030722     D UT§DSE0F      E DS
012100060919     D FNLV53DS      E DS
012200060920     d savdfv          s                   Like(fvvdfv)
012300060920     d Kffv            s                   Like(fvalai)
012400060920     d Kdfv            s                   Like(fgvdfv)
012500040528     d Knpg            s                   Like(fvvnpg)
012600040604     d Kfva            s                   Like(brvnfv)
012700040528     d Klnp            s                   Like(brvlnp)
012800040528     d Klna            s                   Like(brvlna)
012900040528     d Knrs            s                   Like(brvnrs)
013000040528     d Knsc            s                   Like(brvnsc)
013100040528     d Kfgs            s                   Like(brvfgs)
013200040604     d comspe          s                   Like(blpspe)
013300050616     d comDFV          s                   Like(FVVDFV)
013400040610     d KCAA            s                   Like(ANMCAA) INZ(005)
013500040604     d Wmanca          s              1
013600050616     d Wtipopgm        s              2
013700060919     d NOelabora       s              1
013800060920     d Trovalocal      s              1
013900050616     D*
014000050616     D CTESTAVA1       C                   CONST('** CONTROLLO COLLI DI VALORE -
014100050616     D                                     **')
014200050616     D CTESTAVA2       C                   CONST('**   al Terminal di Arrivo   -
014300050616     D                                     **')
014400050616     D CTESTAar1       C                   CONST('* STATISTICA M.CUBI IN ARRIVO-
014500050616     D                                      *')
014600091026     D CTESTAar2       C                   CONST('* per Linea/Terminal d''Arriv-
014700091026     D                                     o *')
014800000000     C*---------------------------------------------------------------*
014900911010     C* INDICATORI
015000911010     C*---------------------------------------------------------------*
015100921123     C*  01    - SI TRATTA SOLO DI UNA BUSTA
015200060919     C*  10    - on  - proteggo il p.o. in gestione
015300060919     C*  11    - off - filtro per metri vubi di valore
015400050616     C*  11    - on  - filtro per statistica mcubi
015500951115     C*  28    - EMETTE CAMPO MESSAGGI
015600951115     C*  30/32 - DI COMOD0
015700941221     C*  33    - LETTURA FNFVA
015800951117     C*  34/35 - DI COMODO
015900951227     C*  38    - RECORD ALLOCATO IN FNARB06L
016000951117     C*  40/59 - ERRORE
016100971003     C*  60/63 - POSIZIONAMENTO CURSORE PER MESSAGGI DI ERRORE
016200911010     C*  72/73 - DI SUBFILE
016300911010     C*  90    - ERRORE GENERICO
016400911010     C*---------------------------------------------------------------*
016500000000     C     *ENTRY        PLIST
016600000000     C                   PARM                    KPJBA
016700050616     c                   movel     kpjbu         wtipopgm
016800050616     c                   if        wtipopgm='MC'
016900050616     c                   seton                                        11
017000050616     c                   eval      vidtes1=ctestaAR1
017100050616     c                   eval      vidtes2=ctestaAR2
017200050616     c                   else
017300050616     c                   eval      vidtes1=ctestaVA1
017400050616     c                   eval      vidtes2=ctestaVA2
017500050616     c                   endif
017600941221     C*
017700000000     C                   Z-ADD     1             CODUT
017800911010     C                   CALL      'X§PARUT'
017900030722     C                   PARM                    UT§DSE0F
018000920901     C                   MOVEL     REC80         CNCR80
018100941221     C                   MOVEL     RAGUT         RSUT
018200060919     c* lo imposto solo la prima volta
018300060919     C                   Z-ADD     SIMPOU        VIDpof
018400900511     C*---------------------------------------------------------------*
018500911010     C     INIZIO        TAG
018600911008     C                   EXSR      PULIZ
018700971223     C*
018800921123     C     FOR01         TAG
018900040528     C   09              WRITE     LRv1D03
019000911008     C*
019100040528     C                   EXFMT     LRv1D01
019200040607     c* spengo indicatori
019300040607     c                   setoff                                       289040
019400030722     C** F3 - FINE LAVORO
019500040604     C   KC              GOTO      FINE
019600921117     C*
019700040528     C* CONTROLLO FOGLIO ARRIVI
019800911008     C                   EXSR      CONTR1
019900930127     C* SE ERRORE
020000930127     C   90              GOTO      FOR01
020100930127     C                   Z-ADD     1             REC
020200911008     C*
020300930127     C* EMISSIONE SFILE
020400911010     C     FORCT2        TAG
020500040528     C                   WRITE     LRv1D02
020600040528     C   72              WRITE     LRv1D04
020700911008     C     FORCT         TAG
020800040604     C                   Z-ADD     1             NRR               5 0
020900040528     C                   EXFMT     LRv1DCT
021000951115     C*
021100951115     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
021200951115     C                   CLEAR                   VCCMSG
021300040604     c                   setoff                                       28  90
021400030722     C** F3 - FINE
021500911030     C   KC              GOTO      FINE
021600930127     C*
021700030722     C** F12 - RITORNO
021800911008     C   KL              GOTO      INIZIO
021900930127     C*
022000040528     C** F7 - INTERROG. SPUNTE arrivo
022100941221     C   KG              DO
022200941221     C                   CLEAR                   PARA04
022300060919     C  n11              Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
022400060919     c   11              if        vidpof>0
022500060919     C                   Z-ADD     VIDpof        P04FGS                         FIL.GESTIONE
022600060919     c                   else
022700060919     C                   Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
022800060919     c                   endif
022900941221     C                   MOVEL     VIDNPG        P04NPG                         CATEGORIA
023000941221     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
023100941221     C                   Z-ADD     99            P04NRS
023200941221     C                   Z-ADD     99            P04NPS
023300941221     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
023400040528     C                   MOVEL     'FNLRV1R '    P04PGM                         RICHIAMATO
023500941221     C                   MOVEL     PARA04        KPJBU
023600941221     C                   CALL      'FNLR04R'
023700920225     C                   PARM                    KPJBA
023800941221     C                   GOTO      FORCT2
023900941221     C                   ENDDO
024000040528     C  NKF              GOTO      FORCT2
024100040528     c
024200030722     C* F6  - ELABORAZIONE
024300050616    1c                   if        *inkf
024400050616     c* COLLI DI VALORE
024500050616    2C                   IF        NOT *IN11
024600050616     C                   EXSR      ELAB
024700050616    3c                   if        *in90
024800040604     c                   goto      forct
024900050616    3c                   endif
025000050616     C
025100050616   x2C                   ELSE
025200050616     C*
025300050616     C* STATISTICA METRI CUBI
025400091026    3C***                IF        NOT *IN72
025500091026     c* Elaboro  anche se nessun foglio abbianto: stampa fogli ancora da abbinare
025600050616     C                   eval      p05DFV=COMDFV
025700050616     C                   eval      p05NFV=COMNFV
025800050616     C                   eval      p05NPG=VIDNPG
025900050616     C                   eval      p05fgs=VIDfgs
026000060919     C                   eval      p05pof=VIDpof
026100060920     c                   movea     nfvloc        p05loc
026200091026     c                   eval      p05tra=vidtra
026300050616     C                   MOVEL     para05        KPJBU
026400050616     C                   MOVEL     'LRV5'        KCOAZ
026500050616     C                   CALL      'BCH10'
026600050616     C                   PARM                    KPJBA
026700050616     C                   GOTO      FINE
026800050616     C*
026900091026   x3C***                ELSE
027000050616     C* SE Nessun foglio caricato, impossibile lancio
027100091026     c***                eval      vccmsg=msg(3)
027200091026     c***                seton                                        28
027300091026     c***                goto      forct
027400091026    3c***                endif
027500050616     c
027600050616    2c                   endif
027700050616    1c                   endif
027800960205     C*
027900040604     C* SE NON HO TROVATO NESSUNA MANCAZA STAMPO
028000040604     C                   IF        WMANCA=' '
028100040604     C                   EXSR      STAMPA
028200040604     C                   ENDIF
028300040604     c* chiudo pgm chiamato
028400040604     c                   clear                   para32
028500040604     c                   movel     'C'           p32tla
028600040604     c                   call      'FNLR32R'
028700040604     c                   parm                    kpjba
028800040604     c                   parm                    DSBRV
028900040604     c                   parm                    para32
029000040604     c*
029100000000     C     FINE          TAG
029200960202     C*
029300060919     c                   clear                   fnlv53ds
029400060919     c                   eval      d53tla='C'
029500060919     c                   call      'FNLV53R'
029600060919     c                   parm                    fnlv53ds
029700060919     c
029800000000     C                   SETON                                        LR
029900941216     C**************************************************************************
030000941216     C*    PULIZIA CAMPI
030100941216     C**************************************************************************
030200911008     C     PULIZ         BEGSR
030300941216     C*
030400920709     C* PRIMA VIDEATA
030500911009     C                   MOVEL     *BLANKS       VIDNFV
030600040528     C                   Z-ADD     SIMPOU        VIDFGS
030700040528     C                   MOVEL     '2'           VIDNPG
030800040528     c     simpou        chain     azorg01l
030900040528     c                   movel     orgdes        desfgs
031000040528     c
031100040604     C                   TIME                    UTIME             6 0          ORA E DATA
031200040604     C                   TIME                    w0140            14 0          ORA E DATA
031300960205     C* DATA A 8
031400040604     C                   MOVE      w0140         UDATE8            8 0          GG/MM/AAAA
031500960227     C                   Z-ADD     UDATE8        G02DAT
031600960227     C                   MOVEL     *BLANK        G02ERR
031700960227     C                   CALL      'XSRDA8'
031800960227     C                   PARM                    WLBDAT
031900960227     C                   Z-ADD     G02INV        DATEU8            8 0          AAAA/MM/GG
032000040227     c*
032100911009     C* SUBFILE
032200961014     C                   SETON                                        720973
032300040528     C                   WRITE     LRv1DCT
032400961014     C                   SETOFF                                       720473
032500911008     C*
032600040604     C                   CLEAR                   COMSPE
032700091026     C                   CLEAR                   vidtra
032800911008     C                   ENDSR
032900960205     C**************************************************************************
033000040528     C*    CONTROLLI FOGLIO arrivi
033100960205     C**************************************************************************
033200960205     C     CONTR1        BEGSR
033300941216     C*
033400040607     C                   SETOFF                                       303109
033500930208     C*
033600040528     C* NUMERO FOGLIO arrivi
033700911008     C     VIDNFV        IFEQ      *BLANKS
033800911008     C     VIDNFV        OREQ      *ZEROS
033900040604     c                   movel     msg(1)        vidmsg
034000060919     C                   SETON                                        9028
034100911008     C                   GOTO      ENDCT1
034200911008     C*
034300911008     C                   ELSE
034400911008     C* RICERCA
034500950328     C     '?'           SCAN      VIDNFV                                 09
034600911010     C   09              DO
034700941221     C                   CLEAR                   DSLR01
034800941221     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
034900040528     C                   MOVEL     vidnpg        DLRNPG                         CATEGORIA FOGLIO
035000941221     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
035100040528     C                   MOVEL     'FNLRV1R'     DLRPGM                         NOME PROGRAMMA
035200941221     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
035300941221     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
035400060919     C  n11              MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
035500060919     C   11              IF        VIDPOF>0
035600060919     C                   MOVEL     VIDpof        DLRFGS                         FIL.GESTIONE
035700060919     C                   ELSE
035800060919     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
035900060919     C                   ENDIF
036000941221     C                   MOVEL     DSLR01        KPJBU
036100941221     C                   CALL      'FNLR02R'
036200921120     C                   PARM                    KPJBA
036300941221     C                   MOVEL     KPJBU         DSLR01
036400941221     C                   MOVEL     DLRNFV        VIDNFV
036500060919     C                   SETON                                        90
036600911008     C                   GOTO      ENDCT1
036700911008     C                   END
036800911010     C* CONTROLLI
036900911008     C                   TESTN                   VIDNFV               31
037000930127     C  N31              SETON                                        30
037100911008     C*
037200060919     c                   if        not *in30
037300060919     c  n11              eval      kfgs=vidfgs
037400060919     c   11              eval      kfgs=vidpof
037500060919     C                   MOVEL     VIDNFV        COMNFV
037600060919     C     KFVV1         CHAIN     FNFVV01L                           30
037700060919     c                   endif
037800970421     C* INESISTENTE
037900970421     C     *IN30         IFEQ      *ON
038000911008     C* ANNULLATO
038100970421     C     FVVATB        ORNE      *BLANKS
038200040604     c                   movel     msg(1)        vidmsg
038300060919     C                   SETON                                        9028
038400961010     C                   GOTO      ENDCT1
038500911008     C                   END
038600961010     C                   END
038700050616     C*
038800050616     C                   MOVEL     FVVDFV        COMDFV
038900930127     C*
039000930127     C* CARICAMENTO SFILE
039100930127     C                   EXSR      CARSFL
039200060919     c* Se non ho caricato nessun foglio, errore
039300060919     c                   if        nrr=0  and vidpof<>vidfgs
039400060919     c                   movel     msg(4)        vidmsg
039500060919     C                   SETON                                        9028
039600060919     C                   GOTO      ENDCT1
039700060919     C                   END
039800930127     C*
039900930127     C     ENDCT1        ENDSR
040000941216     C**************************************************************************
040100941216     C*    CARICAMENTO SFILE
040200941216     C**************************************************************************
040300930127     C     CARSFL        BEGSR
040400941216     C*
040500930519     C* DATA FOGLIO ARRIVI
040600920709     C                   MOVE      FVVDFV        G02INV
040700920709     C                   MOVE      *ZEROS        G02DAT
040800920709     C                   MOVEL     3             G02ERR
040900941221     C                   CALL      'XSRDA8'
041000920709     C                   PARM                    WLBDAT
041100920709     C                   Z-ADD     G02DAT        VIDDFA
041200911008     C*
041300040528     C* Seleziona per controllo
041400040528     C                   MOVEL     ' '           VI2Sel                          STAMPA ANOM
041500060920     C                   Z-ADD     0             NRR
041600060920     c
041700060920     c* Per mc in arrivo carico il locale con p.o. foglio arrivi
041800060920     c                   if        *in11
041900060920     c                   exsr      Cerloc
042000060920     c                   endif
042100060920     c
042200060920     C                   CLEAR                   VI2LOC
042300960205     C*
042400920428     C* CARICO FOGLI VIAGGIO GIA' ABBINATI SE ESISTONO
042500941221     C     KFVA2         SETLL     FNFVA02L
042600941221     C     KFVA2         READE     FNFVA02L                               30
042700911008     C*
042800040604    1C     *IN30         DOWEQ     '0'
042900921119     C                   MOVEL     FVANFV        VI2FVA                          NUM FOGLIO
043000921119     C                   MOVEL     FVALNP        VI2LNP                          LINEA PART
043100960205     C*
043200960205     C                   EXSR      DATARR
043300951115     C*
043400060919     c* solo se da elaborare
043500060919     c                   if        noelabora=' '
043600060919     c
043700040604    2c                   if        fvaatr<>*blanks
043800040604     c                   seton                                        41
043900040604     c                   else
044000040604     c                   setoff                                       41
044100040604    2c                   endif
044200040604     c
044300911010     C                   ADD       1             NRR
044400040604     C                   WRITE     LRV1DSF
044500060919    2c                   endif
044600911008     C*
044700941221     C     KFVA2         READE     FNFVA02L                               30
044800040604    1C                   ENDDO
044900040528    c
045000960626     C                   Z-ADD     NRR           SAVNRR            5 0
045100961014     C     NRR           COMP      0                                      72
045200960626     C*
045300930127     C                   ENDSR
045400960111     C**************************************************************************
045500960111     C* CONTROLLO COSA PROPORRE NELLA DATA/ORA ARRIVO REALE
045600000628     C* ... carica anche P.O. gestione POSTE
045700960111     C**************************************************************************
045800960111     C     DATARR        BEGSR
045900960205     C****
046000960205     C** D A T A / O R A     A R R I V O
046100960205     C*
046200960205    2C     FVAAED        IFGT      0
046300960205     C* DATA ARRIVO REALE
046400960205     C                   Z-ADD     FVAAED        G02INV
046500960205     C                   MOVE      *ZEROS        G02DAT
046600960205     C                   MOVEL     3             G02ERR
046700960205     C                   CALL      'XSRDA8'
046800960205     C                   PARM                    WLBDAT
046900960205     C                   MOVEL     G02DAT        VI2AED
047000960205     C* ORA ARRIVO REALE
047100960205     C                   MOVEL     FVAAEH        VI2AEH
047200040528     c                   else
047300040528     c                   clear                   vi2aed
047400040528     c                   clear                   vi2aeh
047500960205    2C                   ENDIF
047600960202     C*
047700960202     C* GIRO DATA PARTENZA
047800960202     C                   MOVE      FVADFV        G02INV
047900960202     C                   MOVE      *ZEROS        G02DAT
048000960202     C                   MOVEL     3             G02ERR
048100960202     C                   CALL      'XSRDA8'
048200040528     c
048300960202     C                   PARM                    WLBDAT
048400971223     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
048500960202     C                   MOVEL     G02DAT        VI2DFV
048600960202     C*
048700960202     C* SE E' SOLO BUSTA METTO CAMPO IN R.I.
048800960202    1C     FVAATR        IFEQ      'T'
048900960202     C                   SETON                                        01
049000960202   X1C                   ELSE
049100960202     C                   SETOFF                                       01
049200960202    1C                   END
049300960402     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
049400960402     C*  VISUALIZZO CON 2 ASTERISCHI
049500960402     C     KARB          SETLL     FNARB06L                               34
049600960402     C     *IN34         IFEQ      *ON
049700960402     C                   CLEAR                   VI2AST
049800960402     C                   ELSE
049900960402     C                   MOVEL     '**'          VI2AST
050000960402     C                   ENDIF
050100060919     c
050200060919     C* SE IL P.O. GESTIONE È DIVERSO DAL p.o. foglio
050300060919     c*  verifico se il foglio è relativo alla linea di arrivo richiesta
050400060919     c                   clear                   Noelabora
050500060919     c   11              if        vidpof<>vidfgs
050600060919     c                   clear                   fnlv53ds
050700060919     c                   eval      d53tfo='A'
050800060919     c                   eval      d53nfv=fvanfv
050900060919     c                   eval      d53npg=1
051000060919     c                   eval      d53fgs=fvalnp
051100060919     c                   eval      d53lai=fvalai
051200060919     c                   eval      d53abb='B'
051300060919     c                   eval      d53lna=vidfgs
051400060919     c                   call      'FNLV53R'
051500060919     c                   parm                    fnlv53ds
051600060919     c                   if        d53err<>' '
051700060919     c                   eval      noelabora='N'
051800060919     c                   endif
051900060919     c                   endif
052000000628     C* P.O.GESTIONE
052100000628     C     KFWA1         CHAIN     FNFWA01L                           74
052200000628     C     *IN74         IFEQ      *OFF
052300000628     C     FWAFGS        ANDNE     0
052400000628     C                   Z-ADD     FWAFGS        WORKPT            3 0
052500000628     C                   ELSE
052600000628     C                   Z-ADD     FVALNP        WORKPT            3 0
052700000628     C                   ENDIF
052800000628     C                   CLEAR                   OG143
052900000628     C     WORKPT        CHAIN     AZORG                              74
053000000628     C   74              MOVE      '  '          VI2PTV
053100000628     C  N74              MOVEL     ORGDE3        OG143
053200020207     C  N74§OGNTW        IFEQ      'PPT'
053300000628     C                   MOVE      'PT'          VI2PTV
053400000628     C                   ELSE
053500000628     C                   MOVE      '  '          VI2PTV
053600000628     C                   ENDIF
053700960111     C*
053800960111     C                   ENDSR
053900060920     C**************************************************************************
054000060920     C* Cerco foglio locale con p.o. foglio arrivi
054100060920     C**************************************************************************
054200060920     C     Cerloc        BEGSR
054300060920     c* salvo la data foglio arrivi
054400060920     c                   eval      savdfv=fvvdfv
054500060920     c                   clear                   trovalocal
054600060920     c                   clear                   yy
054700060920     c                   movel     *zeros        nfvloc
054800060920     c
054900060920     c     kfgv          setll     fnfgv02l
055000060920     c     vidpof        readpe    fnfgv02l
055100060920     c
055200060920     c* Cerco il primo foglio locale con data < data foglio arrivi
055300060920     c                   dow       not %eof(fnfgv02l)
055400060920     c* se trovato locale valido --> ok
055500060920     c                   if        fgvttr='L' and fgvatb=' '
055600060920     c                   leave
055700060920     c                   else
055800060920     c     vidpof        readpe    fnfgv02l
055900060920     c                   endif
056000060920     c                   enddo
056100060920     c
056200060920     c* se trovato controllo di non avere un foglio arrivi tra la data
056300060920     c*  foglio locale e la data foglio arrivi richiesta:
056400060920     c*  se c'e' non carico il locale altrimenti lo carico
056500060920     c                   if        not %eof(fnfgv02l)
056600060920     c     kfvv8         setgt     fnfvv08l
056700060920     c     kfvv8b        reade     fnfvv08l
056800060920     c                   dow       not %eof(fnfvv08l) and fvvatb<>' '
056900060920     c     kfvv8b        reade     fnfvv08l
057000060920     c                   enddo
057100060920     c* Verifico la data del fogliio arrivi trovato
057200060920     c                   if        not %eof(fnfvv08l) and
057300060920     c                             fvvdfv>=savdfv
057400060920     c                   eval      trovalocal='S'
057500060920     c                   endif
057600060920     c                   endif
057700060920     c
057800060920     c* Locale trovato: lo carico nel sfl
057900060920    1c                   if        trovalocal='S'
058000060920     c                   eval      kdfv=fgvdfv
058100060920     c     kfgv2         setll     fnfgv02l
058200060920     c     kfgv2         reade     fnfgv02l
058300060920     c
058400060920    2c                   dow       not %eof(fnfgv02l)
058500060920     c
058600060920    3c                   if        fgvttr='L' and fgvatb=' '
058700060920     c
058800060920     c* imposto ciclo di lettura per p.o. e data
058900060920     c
059000060920     c*
059100060920     C                   MOVEL     FgvNFV        VI2FVA                          NUM FOGLIO
059200060920     C                   MOVEL     FgvLNP        VI2LNP                          LINEA PART
059300060920     c                   clear                   vi2aed
059400060920     c                   clear                   vi2aeh
059500060920     c
059600060920     C                   MOVE      FgvDFV        G02INV
059700060920     C                   MOVE      *ZEROS        G02DAT
059800060920     C                   MOVEL     3             G02ERR
059900060920     C                   CALL      'XSRDA8'
060000060920     c
060100060920     C                   PARM                    WLBDAT
060200060920     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
060300060920     C                   MOVEL     G02DAT        VI2DFV
060400060920     c
060500060920     c                   setoff                                       01
060600060920     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
060700060920     C*  VISUALIZZO CON 2 ASTERISCHI
060800060920     C     KARB          SETLL     FNARB06L                               34
060900060920     C     *IN34         IFEQ      *ON
061000060920     C                   CLEAR                   VI2AST
061100060920     C                   ELSE
061200060920     C                   MOVEL     '**'          VI2AST
061300060920     C                   ENDIF
061400060920     c
061500060920     C* SE IL P.O. GESTIONE È DIVERSO DAL p.o. foglio
061600060920     c*  verifico se il foglio è relativo alla linea di arrivo richiesta
061700060920     c                   clear                   Noelabora
061800060920    4c                   if        vidpof<>vidfgs
061900060920     c                   clear                   fnlv53ds
062000060920     c                   eval      d53tfo='G'
062100060920     c                   eval      d53nfv=fgvnfv
062200060920     c                   eval      d53npg=1
062300060920     c                   eval      d53fgs=fgvlnp
062400060920     c                   eval      d53lai=vidpof
062500060920     c                   eval      d53abb='B'
062600060920     c                   eval      d53lna=vidfgs
062700060920     c                   call      'FNLV53R'
062800060920     c                   parm                    fnlv53ds
062900060920    5c                   if        d53err<>' '
063000060920     c                   eval      noelabora='N'
063100060920    5c                   endif
063200060920    4c                   endif
063300060920     C*
063400060920     c* solo se da elaborare
063500070529    4c                   if        noelabora=' '   and yy<5
063600060920     C                   EVAL      VI2LOC='LOCALE'
063700060920     C
063800060920     C                   ADD       1             NRR
063900060920     C                   WRITE     LRV1DSF
064000060920     c                   add       1             yy
064100060920     c                   movel     fgvnfv        nfvloc(yy)
064200060920    4c                   endif
064300060920    3c                   endif
064400060920     c
064500060920     c     kfgv2         reade     fnfgv02l
064600060920    2c                   enddo
064700060920    1c                   endif
064800060920     C*
064900060920     C                   ENDSR
065000941216     C**************************************************************************
065100941216     C*    ELABORAZIONE ABBINAMENTO
065200941216     C**************************************************************************
065300911008     C     ELAB          BEGSR
065400911010     C*
065500930129     C                   Z-ADD     1             NRR
065600930129     C*
065700040528     C* LEGGO TUTTI I FOGLI VIAGGIO ABBINATI  selezionati
065800040604     c* li carico in una schiera perchè li devo stampare
065900040604     c                   clear                   xx                3 0
066000040604     c                   clear                   slnp
066100040604     c                   clear                   sfva
066200951228    1C     NRR           DOWLE     SAVNRR
066300040604     C     NRR           CHAIN     LRv1DSF                            30
066400930129     C*
066500040528    2c                   if        not *in30  and vi2sel='I'
066600040604     c                   add       1             xx
066700040604     c                   z-add     vi2lnp        slnp(xx)
066800040604     c                   z-add     vi2fva        sfva(xx)
066900040604    2c                   endif
067000040604     c                   add       1             nrr
067100040604    1c                   enddo
067200040604     c
067300040604     c* Se non selezionato nessun foglio errore
067400040604    1c                   if        xx=0
067500040604     c                   movel     msg(2)        vccmsg
067600040604     c                   seton                                        2890
067700040604   x1c                   else
067800040604     c*
067900040604     C                   Z-ADD     80            LUNG             15 5
068000040604     C                   MOVEL     *BLANKS       COMMAN           80
068100040604     C                   MOVEA     CMD           COMMAN
068200040604     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
068300040604     C                   PARM                    COMMAN
068400040604     C                   PARM                    LUNG
068500040604     C*
068600040604RM*  C                   OPEN      PRTF198
068700040604     C                   SETON                                        OF
068800040528     c
068900040604     c* Leggo tutti i fogli per elaborare le linee giuste
069000040604     c                   z-add     1             xx
069100040604    2c                   dow       slnp(xx)>0
069200040604     c                   z-add     slnp(xx)      vi2lnp
069300040604     c                   z-add     sfva(xx)      vi2fva
069400040604     c
069500040528     C     KFVA1         CHAIN     FNFVA01L
069600040528    3C                   IF        %found(fnfva01l)
069700040528     C*
069800040528     C     KFWA1         CHAIN     FNFWA01L
069900040528    4C                   IF        NOT %FOUND(FNFWA01L)   OR  fwaatb<>' '
070000040528     C                   CLEAR                   FWAFF3
070100040528     C                   CLEAR                   FWAFF4
070200040528     C                   CLEAR                   FWAFL3
070300040528     C                   CLEAR                   FWAFL4
070400040528    4C                   ENDIF
070500040528     C*
070600040528     C                   Z-ADD     FVALNA        LNA2
070700040604     C                   Z-ADD     1             X                 4 0
070800040528     C                   MOVEL     FVALAI        ALF3              3
070900040528     C     ALF3          LOOKUP    FLP(X)                                 31
071000040528     C*
071100040528    4C                   DOW       *in31
071200040528     C                   MOVEL     FFV(X)        kffv
071300911010     C*
071400941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
071500040528     C                   Z-ADD     VI2FVA        kfva
071600070110     C     KBRV1         SETLL     FNBRV02L
071700070110     C     KBRV1         READE     FNBRV02L                               30
071800911010     C*
071900040528    5C     *IN30         DOWEQ     '0'
072000040528     c                   EXSR      ctrarr
072100911010     C*
072200070110     C     KBRV1         READE     FNBRV02L                               30
072300040528    5C                   ENDDO
072400040528     c*
072500040528     C                   ADD       1             x
072600040528     C     ALF3          LOOKUP    FLP(X)                                 31
072700040528    4C                   ENDdo
072800040604    3c                   endif
072900921120     C*
073000040604     C                   ADD       1             xx
073100040604    2C                   ENDDO
073200040604     c
073300040604    1c                   endif
073400911008     C                   ENDSR
073500040528     C**************************************************************************
073600040528     C*    vedo se collo arrivato per creare anomalia collo part non arr
073700040528     C**************************************************************************
073800040528     C     CTRARR        BEGSR
073900040603     c* richiamo FNLR32R per fare i controlli e far creare l'anomalia:
074000040603     c*  se creo l'anomalia, eseguo la stampa
074100040603     c
074200040604     c                   clear                   para32
074300040604     c                   movel     'S'           p32valore
074400040604     c                   z-add     vidfgs        p32fgs
074500040604     c                   eval      p32dfvv=fvvdfv
074600040603     c                   call      'FNLR32R'
074700040603     c                   parm                    kpjba
074800040604     c                   parm                    DSBRV
074900040604     c                   parm                    para32
075000040604     C* SE COLLO DI VALORE MANCANTE, STAMPO
075100040604     c                   if        p32manca='S'
075200040604     C* MEMORIZZO  DI AVER TROVATO ALMENO UNA MANCANZA
075300040604     C                   EVAL      WMANCA='S'
075400040604     C
075500040604     c                   exsr      stampa
075600040604     c                   endif
075700040604     c                   endsr
075800040604     C**************************************************************************
075900040604     C* STAMPO mNCANZA COLLO DI VALORE
076000040604     C**************************************************************************
076100040604     C     STAMPA        BEGSR
076200040604     C*
076300040604     C* STAMPA TESTATA
076400040604    1C   OF              DO
076500040604     C* PRIMA PARTE TESTATA
076600040604     C                   EXCEPT    TES
076700040604     C*
076800040604     C                   Z-ADD     1             YY                3 0
076900040604     C                   Z-ADD     1             II                3 0
077000040604     C                   MOVEA     *BLANKS       STF
077100040604     C*
077200040604    2C     slnp(YY)      DOWgt     0
077300040604     C*
077400040604    3C     II            IFEQ      191
077500040604     C                   EXCEPT    TES3
077600040604     C                   MOVEA     *BLANKS       STF
077700040604     C                   Z-ADD     1             II
077800040604    3C                   END
077900040604     C*
078000040617     C                   MOVEL     slnp(YY)      ALF3A
078100040604     C                   MOVEL     sfva(YY)      ALF5
078200040604     C                   MOVEA     FVAALF        STF(II)
078300040604     C                   ADD       10            II
078400040604     C**
078500040604     C                   ADD       1             YY
078600040604    2C                   ENDdo
078700040604     C*
078800040604    2C     II            IFGT      1
078900040604     C                   EXCEPT    TES3
079000040604    2C                   END
079100040604     C*
079200040604     C* STAMPO TERZA PARTE TESTATA
079300040604     C                   EXCEPT    TES2
079400040604     C*
079500040604     C                   SETOFF                                       OF
079600040604    1C                   END
079700040604     C* Stampo collo mancante
079800040604     C                   IF        WMANCA='S'
079900040604     C* CHAIN SPEDIZIONE
080000040604     C     KBLP          CHAIN     FNBLP01L
080100040604     C                   IF        %FOUND(FNBLP01L)
080200040604     C* SE C'E' BOLLA E A CAMBIO SPEDIZIONE STAMPO I DATI DELLA SPED
080300040604     C     COMSPE        IFNE      BLPSPE
080400040604     C                   SETON                                        05
080500040604     C                   MOVEL     BLPSPE        COMSPE
080600040604     C                   ENDIF
080700040604     C                   ELSE
080800040604     C                   SETOFF                                       05
080900040604     C                   ENDIF
081000040610     C* CHAIN ANOMALIE PER AVERE DATA E ORA DI CREAZIONE
081100040610     C                   CLEAR                   STADAN
081200040610     C                   CLEAR                   STAOAN
081300040610     C     KANM          CHAIN     FNANM01L
081400040610     C                   IF        %FOUND(FNANM01L)
081500040611     C                   MOVEL     ANMFLO        w0060             6 0          AAAA/MM/GG
081600040611     C                   z-add     w0060         G02INV                         AAAA/MM/GG
081700040610     C                   MOVE      *ZEROS        G02DAT
081800040610     C                   MOVEL     3             G02ERR
081900040610     C                   CALL      'XSRDA8'
082000040610     C                   PARM                    WLBDAT
082100040611     C                   MOVEL     G02DAT        STADAN            8 0          GG/MM
082200040610     C*
082300040610     C                   MOVE      ANMFLO        STAOAN            4 0
082400040610     C                   ENDIF
082500040604     C*
082600040604     C* DATA SPEDIZIONE
082700040604     C   05              DO
082800040604     C                   Z-ADD     BLPAAS        COMAAS                         ANNO (4 CIFRE)
082900040604     C                   MOVE      BLPDSP        G02INV                         AAAA/MM/GG
083000040604     C                   MOVE      *ZEROS        G02DAT
083100040604     C                   MOVEL     3             G02ERR
083200040604     C                   CALL      'XSRDA8'
083300040604     C                   PARM                    WLBDAT
083400040604     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
083500040604     C*
083600040604     C* SEGNACOLLI NON CONSECUTIVI
083700040604     C     BLPFNS        COMP      'S'                                    04
083800040604     C* SERIE>< 0
083900040604     C     BLPNRS        COMP      0                                  0303
084000040604     C                   END
084100040604     C* MITTENTE E DESTINATARIO DI 17 E 20
084200040604     C                   MOVEL     BLPRSM        STARSM           17
084300040604     C                   MOVEL     BLPRSD        STARSD           20
084400040604     C*
084500040604     C* STAMPO DETTAGLIO
084600040604     C                   EXCEPT    DET
084700040604     C                   SETOFF                                       05
084800040604     c                   else
084900040604     c                   except    OK
085000040604     c                   endif
085100040604     c                   endsr
085200960202     C*
085300951115     C*--- SR INIZIALE -----------------------------------------------*
085400951115     C     *INZSR        BEGSR
085500951115     C*
085600951115     C****  KLIST  ****
085700040604     C     KARB          KLIST
085800040604     C                   KFLD                    vi2lnp
085900040604     C                   KFLD                    VI2FVA
086000060919     C     KARB1         KLIST
086100060919     C                   KFLD                    vi2lnp
086200060919     C                   KFLD                    VI2FVA
086300060919     C                   KFLD                    vidfgs
086400951115     C     KFVV1         KLIST
086500040604     C                   KFLD                    vidNPG
086600951115     C                   KFLD                    COMNFV            5 0
086700060919     C                   KFLD                    kFGS
086800060920     C     KFVV8         KLIST
086900060920     C                   KFLD                    vidpof
087000060920     C                   KFLD                    vidnpg
087100060920     C                   KFLD                    fgvdfv
087200060920     C     KFVV8b        KLIST
087300060920     C                   KFLD                    vidpof
087400060920     C                   KFLD                    vidnpg
087500060920     C     KFgv          KLIST
087600060920     C                   KFLD                    vidpof
087700060920     C                   KFLD                    fvvdfv
087800060920     C     KFgv2         KLIST
087900060920     C                   KFLD                    vidpof
088000060920     C                   KFLD                    kdfv
088100951115     C     KFVA1         KLIST
088200040528     C                   KFLD                    VI2LNP
088300951115     C                   KFLD                    VI2FVA
088400040528     C                   KFLD                    VIDFGS
088500951115     C     KFVA2         KLIST                                                  FNFVA02L
088600040604     C                   KFLD                    vidNPG                         ..CATEGORIA
088700951115     C                   KFLD                    COMNFV                         ..FOGLIO
088800060919     C                   KFLD                    kfgs
088900951115     C     KBRV1         KLIST                                                  FNBRV02L
089000951115     C                   KFLD                    KNPG                           ..CATEGORIA
089100040528     C                   KFLD                    kfva                           ..NR.FOGLIO
089200040528     C                   KFLD                    vi2LNP                         ..FIL.GESTIONE
089300040528     C                   KFLD                    kffv                           ..LNA
089400000628     C     KFWA1         KLIST
089500000628     C                   KFLD                    FVALNP
089600000628     C                   KFLD                    FVANFV
089700000628     C                   KFLD                    FVALAI
089800040604     C     Kblp          KLIST                                                  FIBRV09L
089900040604     C                   KFLD                    P32Aas                         ..LNP
090000040604     C                   KFLD                    P32lnp                         ..LNA
090100040604     C                   KFLD                    P32nrs                         ..SERIE
090200040604     C                   KFLD                    P32nsp                         ..SEGNACOLLO
090300040610     C     KANM          KLIST                                                  FIBRV09L
090400040610     C                   KFLD                    P32Aas                         ..LNP
090500040610     C                   KFLD                    P32lnp                         ..LNA
090600040610     C                   KFLD                    P32nrs                         ..SERIE
090700040610     C                   KFLD                    P32nsp                         ..SEGNACOLLO
090800040610     C                   KFLD                    KCAA                           ..SEGNACOLLO
090900951115     C*
091000951115     C                   ENDSR
091100040604     C**************************************************************************
091200040604RM*  OPRTF198   E            TES              02
091300040604     O                       STA(1)              66
091400040604     O                       STA(2)             132
091500040604     O                       STA(13)            198
091600040604     O                       RSUT                20
091700040604     O                       UDATE8             181 '  /  /    '
091800040604     O                       PAGE          Z    198
091900040604     O          E            TES         1
092000040604     O                       UTIME              179 '  :  :  '
092100040604     O          E            TES         2
092200040604     O                       STA(3)              66
092300040604RM*  O                       VIDFGS              22
092400040604     O                       DESFGS              43
092500040604     O                       COMNFV        Z     56
092600040604     O                       VIDDFA        8     73
092700040604     O          E            TES         2
092800040604     O                       STA(4)              66
092900040604     O          E            TES3        1
093000040604     O                       stf                198
093100040604     O          E            TES2        2
093200040604     O                       STA(5)              66
093300040604     O                       STA(8)             132
093400040610     O                       STA(14)            198
093500040604     O          E            TES2        1  1
093600040604     O                       STA(6)              66
093700040611     O                       STA(9)             132
093800040611     O                       STA(15)            198
093900040604     O*
094000040604     O          E            DET         1
094100040604     O                  05   BLPAAS        Z
094200040604     O                  05   BLPLNP        Z   +  1
094300040604     O                  05 03BLPNRS        Z   +  1
094400040604     O                  05 03                  +  0 '-'
094500040604     O                  05   BLPNSP        Z   +  0
094600040604     O                  05   STADSP            +  1 '  /  '
094700040604     O                  05   BLPLNA        Z   +  1
094800040604     O                  05   STARSM            +  1
094900040604     O                  05   STARSD            +  1
095000040604     O                  05N04BLPNCD        Z   +  1
095100040604     O                  05N04                  +  0 '/'
095200040604     O                  05N04BLPNCA        Z   +  0
095300040604     O                  05 04                    84 'NON SEQUENZIALI'
095400040604     O                  05   BLPZNC            +  1
095500040604     O                       BRVLNP            +  4
095600040604     O                       BRVLNA            +  2
095700040604     O                       BRVNRS        Z   +  3
095800040604     O                       BRVNSC        Z   +  4
095900040610     O                       STADAN            +  8 '  /  /    '
096000040610     O                       STAOAN            +  2 '  :  '
096100040604     O          E            OK          3
096200040604     O                       STA(11)             66
096300040604     O          E            OK          0  0
096400040604     O                       STA(11)             66
096500040604     O**************************************************************************
096600040604** STA
096700040617XXXXXXXXXXXXXXXXXXXX                                                 1
096800040604 ** COLLI DI VALORE MANCANTI AL TERMINAL DI ARRIVO **                2
096900040604FOGLIO ARRIVI: DI  XXX-XXXXXXXXXXXXXXXXXXXX   N.RO XXXXX   DEL XX/   3
097000040604FOGLI VIAGGIO PARTENZA SELEZIONATI:                                  4
097100040604    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO         5
097200040604ANNO LNP SER NUMERO  DATA ARR                                        6
097300040604XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx   7
097400040610     SEGNACOLLI    ZON   C O L L O   M A N C A N T E     CREAZIONE   8
097500040610      DAL /   AL   ARR   LNP  LNA  SERIE   NUMERO           DATA     9
097600040610XX XXXXXXX/XXXXXXX XX    XXX  XXX   XX    XXXXXXX        XX/XX/XXX  10
097700040604             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
097800040604 **  FINE  STAMPA  **                                               12
097900040604                         FNLRV1R                          PAG.XXXX  13
098000040610 ANOMALIA                                                           14
098100040610    ORA                                                             15
098200040610X  XX:XX                                                            16
098300040611**
098400040604OVRPRTF FILE(PRTF198) USRDTA('ColValore')
098500000606**  MSG
098600040604Foglio arrivi errato                                                          1
098700040604Effettuare almeno una selezione per la elaborazione                           1
098800050616Nessun foglio da elaborare                                                    1
098900060919Nessun foglio partenza abbinato relativo al p.o. in gestione indicato         1
