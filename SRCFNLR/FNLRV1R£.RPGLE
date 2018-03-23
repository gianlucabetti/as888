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
011500060919     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MESSAGGI ERRORE
011600040604     D slnp            S              3  0 DIM(250)                             FV GIA' SELEZ
011700040604     D sfva            S              5  0 DIM(250)                             FV GIA' SELEZ
011800040604     D STF             S              1    DIM(198)                             STAMPA FV ABB
011900060920     D nfvloc          s              5    DIM(5)                               LINEE TRANSITO
012000040604     d
012100070110     D DSBRV         E DS                  EXTNAME(FNBRV00F)
012200960304     D*
012300040604     D dslr01        E DS
012400040604     D CNCR80        E DS
012500040604     D og143         E DS
012600040604     D KPJBA         E DS
012700030722     D UT§DSE0F      E DS
012800060919     D FNLV53DS      E DS
012900060920     d savdfv          s                   Like(fvvdfv)
013000060920     d Kffv            s                   Like(fvalai)
013100060920     d Kdfv            s                   Like(fgvdfv)
013200040528     d Knpg            s                   Like(fvvnpg)
013300040604     d Kfva            s                   Like(brvnfv)
013400040528     d Klnp            s                   Like(brvlnp)
013500040528     d Klna            s                   Like(brvlna)
013600040528     d Knrs            s                   Like(brvnrs)
013700040528     d Knsc            s                   Like(brvnsc)
013800040528     d Kfgs            s                   Like(brvfgs)
013900040604     d comspe          s                   Like(blpspe)
014000050616     d comDFV          s                   Like(FVVDFV)
014100040610     d KCAA            s                   Like(ANMCAA) INZ(005)
014200040604     d Wmanca          s              1
014300050616     d Wtipopgm        s              2
014400060919     d NOelabora       s              1
014500060920     d Trovalocal      s              1
014600050616     D*
014700050616     D CTESTAVA1       C                   CONST('** CONTROLLO COLLI DI VALORE -
014800050616     D                                     **')
014900050616     D CTESTAVA2       C                   CONST('**   al Terminal di Arrivo   -
015000050616     D                                     **')
015100050616     D CTESTAar1       C                   CONST('* STATISTICA M.CUBI IN ARRIVO-
015200050616     D                                      *')
015300091026     D CTESTAar2       C                   CONST('* per Linea/Terminal d''Arriv-
015400091026     D                                     o *')
015500000000     C*---------------------------------------------------------------*
015600911010     C* INDICATORI
015700911010     C*---------------------------------------------------------------*
015800921123     C*  01    - SI TRATTA SOLO DI UNA BUSTA
015900060919     C*  10    - on  - proteggo il p.o. in gestione
016000060919     C*  11    - off - filtro per metri vubi di valore
016100050616     C*  11    - on  - filtro per statistica mcubi
016200951115     C*  28    - EMETTE CAMPO MESSAGGI
016300951115     C*  30/32 - DI COMOD0
016400941221     C*  33    - LETTURA FNFVA
016500951117     C*  34/35 - DI COMODO
016600951227     C*  38    - RECORD ALLOCATO IN FNARB06L
016700951117     C*  40/59 - ERRORE
016800971003     C*  60/63 - POSIZIONAMENTO CURSORE PER MESSAGGI DI ERRORE
016900911010     C*  72/73 - DI SUBFILE
017000911010     C*  90    - ERRORE GENERICO
017100911010     C*---------------------------------------------------------------*
017200000000     C     *ENTRY        PLIST
017300000000     C                   PARM                    KPJBA
017400050616     c                   movel     kpjbu         wtipopgm
017500050616     c                   if        wtipopgm='MC'
017600050616     c                   seton                                        11
017700050616     c                   eval      vidtes1=ctestaAR1
017800050616     c                   eval      vidtes2=ctestaAR2
017900050616     c                   else
018000050616     c                   eval      vidtes1=ctestaVA1
018100050616     c                   eval      vidtes2=ctestaVA2
018200050616     c                   endif
018300941221     C*
018400000000     C                   Z-ADD     1             CODUT
018500911010     C                   CALL      'X§PARUT'
018600030722     C                   PARM                    UT§DSE0F
018700920901     C                   MOVEL     REC80         CNCR80
018800941221     C                   MOVEL     RAGUT         RSUT
018900060919     c* lo imposto solo la prima volta
019000060919     C                   Z-ADD     SIMPOU        VIDpof
019100900511     C*---------------------------------------------------------------*
019200911010     C     INIZIO        TAG
019300911008     C                   EXSR      PULIZ
019400971223     C*
019500921123     C     FOR01         TAG
019600040528     C   09              WRITE     LRv1D03
019700911008     C*
019800040528     C                   EXFMT     LRv1D01
019900040607     c* spengo indicatori
020000040607     c                   setoff                                       289040
020100030722     C** F3 - FINE LAVORO
020200040604     C   KC              GOTO      FINE
020300921117     C*
020400040528     C* CONTROLLO FOGLIO ARRIVI
020500911008     C                   EXSR      CONTR1
020600930127     C* SE ERRORE
020700930127     C   90              GOTO      FOR01
020800930127     C                   Z-ADD     1             REC
020900911008     C*
021000930127     C* EMISSIONE SFILE
021100911010     C     FORCT2        TAG
021200040528     C                   WRITE     LRv1D02
021300040528     C   72              WRITE     LRv1D04
021400911008     C     FORCT         TAG
021500040604     C                   Z-ADD     1             NRR               5 0
021600040528     C                   EXFMT     LRv1DCT
021700951115     C*
021800951115     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
021900951115     C                   CLEAR                   VCCMSG
022000040604     c                   setoff                                       28  90
022100030722     C** F3 - FINE
022200911030     C   KC              GOTO      FINE
022300930127     C*
022400030722     C** F12 - RITORNO
022500911008     C   KL              GOTO      INIZIO
022600930127     C*
022700040528     C** F7 - INTERROG. SPUNTE arrivo
022800941221     C   KG              DO
022900941221     C                   CLEAR                   PARA04
023000060919     C  n11              Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
023100060919     c   11              if        vidpof>0
023200060919     C                   Z-ADD     VIDpof        P04FGS                         FIL.GESTIONE
023300060919     c                   else
023400060919     C                   Z-ADD     VIDFGS        P04FGS                         FIL.GESTIONE
023500060919     c                   endif
023600941221     C                   MOVEL     VIDNPG        P04NPG                         CATEGORIA
023700941221     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
023800941221     C                   Z-ADD     99            P04NRS
023900941221     C                   Z-ADD     99            P04NPS
024000941221     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
024100040528     C                   MOVEL     'FNLRV1R '    P04PGM                         RICHIAMATO
024200941221     C                   MOVEL     PARA04        KPJBU
024300941221     C                   CALL      'FNLR04R'
024400920225     C                   PARM                    KPJBA
024500941221     C                   GOTO      FORCT2
024600941221     C                   ENDDO
024700040528     C  NKF              GOTO      FORCT2
024800040528     c
024900030722     C* F6  - ELABORAZIONE
025000050616    1c                   if        *inkf
025100050616     c* COLLI DI VALORE
025200050616    2C                   IF        NOT *IN11
025300050616     C                   EXSR      ELAB
025400050616    3c                   if        *in90
025500040604     c                   goto      forct
025600050616    3c                   endif
025700050616     C
025800050616   x2C                   ELSE
025900050616     C*
026000050616     C* STATISTICA METRI CUBI
026100091026    3C***                IF        NOT *IN72
026200091026     c* Elaboro  anche se nessun foglio abbianto: stampa fogli ancora da abbinare
026300050616     C                   eval      p05DFV=COMDFV
026400050616     C                   eval      p05NFV=COMNFV
026500050616     C                   eval      p05NPG=VIDNPG
026600050616     C                   eval      p05fgs=VIDfgs
026700060919     C                   eval      p05pof=VIDpof
026800060920     c                   movea     nfvloc        p05loc
026900091026     c                   eval      p05tra=vidtra
027000050616     C                   MOVEL     para05        KPJBU
027100050616     C                   MOVEL     'LRV5'        KCOAZ
027200050616     C                   CALL      'BCH10'
027300050616     C                   PARM                    KPJBA
027400050616     C                   GOTO      FINE
027500050616     C*
027600091026   x3C***                ELSE
027700050616     C* SE Nessun foglio caricato, impossibile lancio
027800091026     c***                eval      vccmsg=msg(3)
027900091026     c***                seton                                        28
028000091026     c***                goto      forct
028100091026    3c***                endif
028200050616     c
028300050616    2c                   endif
028400050616    1c                   endif
028500960205     C*
028600040604     C* SE NON HO TROVATO NESSUNA MANCAZA STAMPO
028700040604     C                   IF        WMANCA=' '
028800040604     C                   EXSR      STAMPA
028900040604     C                   ENDIF
029000040604     c* chiudo pgm chiamato
029100040604     c                   clear                   para32
029200040604     c                   movel     'C'           p32tla
029300040604     c                   call      'FNLR32R'
029400040604     c                   parm                    kpjba
029500040604     c                   parm                    DSBRV
029600040604     c                   parm                    para32
029700040604     c*
029800000000     C     FINE          TAG
029900960202     C*
030000060919     c                   clear                   fnlv53ds
030100060919     c                   eval      d53tla='C'
030200060919     c                   call      'FNLV53R'
030300060919     c                   parm                    fnlv53ds
030400060919     c
030500000000     C                   SETON                                        LR
030600941216     C**************************************************************************
030700941216     C*    PULIZIA CAMPI
030800941216     C**************************************************************************
030900911008     C     PULIZ         BEGSR
031000941216     C*
031100920709     C* PRIMA VIDEATA
031200911009     C                   MOVEL     *BLANKS       VIDNFV
031300040528     C                   Z-ADD     SIMPOU        VIDFGS
031400040528     C                   MOVEL     '2'           VIDNPG
031500040528     c     simpou        chain     azorg01l
031600040528     c                   movel     orgdes        desfgs
031700040528     c
031800040604     C                   TIME                    UTIME             6 0          ORA E DATA
031900040604     C                   TIME                    w0140            14 0          ORA E DATA
032000960205     C* DATA A 8
032100040604     C                   MOVE      w0140         UDATE8            8 0          GG/MM/AAAA
032200960227     C                   Z-ADD     UDATE8        G02DAT
032300960227     C                   MOVEL     *BLANK        G02ERR
032400960227     C                   CALL      'XSRDA8'
032500960227     C                   PARM                    WLBDAT
032600960227     C                   Z-ADD     G02INV        DATEU8            8 0          AAAA/MM/GG
032700040227     c*
032800911009     C* SUBFILE
032900961014     C                   SETON                                        720973
033000040528     C                   WRITE     LRv1DCT
033100961014     C                   SETOFF                                       720473
033200911008     C*
033300040604     C                   CLEAR                   COMSPE
033400091026     C                   CLEAR                   vidtra
033500911008     C                   ENDSR
033600960205     C**************************************************************************
033700040528     C*    CONTROLLI FOGLIO arrivi
033800960205     C**************************************************************************
033900960205     C     CONTR1        BEGSR
034000941216     C*
034100040607     C                   SETOFF                                       303109
034200930208     C*
034300040528     C* NUMERO FOGLIO arrivi
034400911008     C     VIDNFV        IFEQ      *BLANKS
034500911008     C     VIDNFV        OREQ      *ZEROS
034600040604     c                   movel     msg(1)        vidmsg
034700060919     C                   SETON                                        9028
034800911008     C                   GOTO      ENDCT1
034900911008     C*
035000911008     C                   ELSE
035100911008     C* RICERCA
035200950328     C     '?'           SCAN      VIDNFV                                 09
035300911010     C   09              DO
035400941221     C                   CLEAR                   DSLR01
035500941221     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
035600040528     C                   MOVEL     vidnpg        DLRNPG                         CATEGORIA FOGLIO
035700941221     C                   MOVEL     '2'           DLRTFV                         TIPO FOGLIO
035800040528     C                   MOVEL     'FNLRV1R'     DLRPGM                         NOME PROGRAMMA
035900941221     C                   Z-ADD     31129999      DLRGAL                         AL GG/MM/AAAA
036000941221     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
036100060919     C  n11              MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
036200060919     C   11              IF        VIDPOF>0
036300060919     C                   MOVEL     VIDpof        DLRFGS                         FIL.GESTIONE
036400060919     C                   ELSE
036500060919     C                   MOVEL     VIDFGS        DLRFGS                         FIL.GESTIONE
036600060919     C                   ENDIF
036700941221     C                   MOVEL     DSLR01        KPJBU
036800941221     C                   CALL      'FNLR02R'
036900921120     C                   PARM                    KPJBA
037000941221     C                   MOVEL     KPJBU         DSLR01
037100941221     C                   MOVEL     DLRNFV        VIDNFV
037200060919     C                   SETON                                        90
037300911008     C                   GOTO      ENDCT1
037400911008     C                   END
037500911010     C* CONTROLLI
037600911008     C                   TESTN                   VIDNFV               31
037700930127     C  N31              SETON                                        30
037800911008     C*
037900060919     c                   if        not *in30
038000060919     c  n11              eval      kfgs=vidfgs
038100060919     c   11              eval      kfgs=vidpof
038200060919     C                   MOVEL     VIDNFV        COMNFV
038300060919     C     KFVV1         CHAIN     FNFVV01L                           30
038400060919     c                   endif
038500970421     C* INESISTENTE
038600970421     C     *IN30         IFEQ      *ON
038700911008     C* ANNULLATO
038800970421     C     FVVATB        ORNE      *BLANKS
038900040604     c                   movel     msg(1)        vidmsg
039000060919     C                   SETON                                        9028
039100961010     C                   GOTO      ENDCT1
039200911008     C                   END
039300961010     C                   END
039400050616     C*
039500050616     C                   MOVEL     FVVDFV        COMDFV
039600930127     C*
039700930127     C* CARICAMENTO SFILE
039800930127     C                   EXSR      CARSFL
039900060919     c* Se non ho caricato nessun foglio, errore
040000060919     c                   if        nrr=0  and vidpof<>vidfgs
040100060919     c                   movel     msg(4)        vidmsg
040200060919     C                   SETON                                        9028
040300060919     C                   GOTO      ENDCT1
040400060919     C                   END
040500930127     C*
040600930127     C     ENDCT1        ENDSR
040700941216     C**************************************************************************
040800941216     C*    CARICAMENTO SFILE
040900941216     C**************************************************************************
041000930127     C     CARSFL        BEGSR
041100941216     C*
041200930519     C* DATA FOGLIO ARRIVI
041300920709     C                   MOVE      FVVDFV        G02INV
041400920709     C                   MOVE      *ZEROS        G02DAT
041500920709     C                   MOVEL     3             G02ERR
041600941221     C                   CALL      'XSRDA8'
041700920709     C                   PARM                    WLBDAT
041800920709     C                   Z-ADD     G02DAT        VIDDFA
041900911008     C*
042000040528     C* Seleziona per controllo
042100040528     C                   MOVEL     ' '           VI2Sel                          STAMPA ANOM
042200060920     C                   Z-ADD     0             NRR
042300060920     c
042400060920     c* Per mc in arrivo carico il locale con p.o. foglio arrivi
042500060920     c                   if        *in11
042600060920     c                   exsr      Cerloc
042700060920     c                   endif
042800060920     c
042900060920     C                   CLEAR                   VI2LOC
043000960205     C*
043100920428     C* CARICO FOGLI VIAGGIO GIA' ABBINATI SE ESISTONO
043200941221     C     KFVA2         SETLL     FNFVA02L
043300941221     C     KFVA2         READE     FNFVA02L                               30
043400911008     C*
043500040604    1C     *IN30         DOWEQ     '0'
043600921119     C                   MOVEL     FVANFV        VI2FVA                          NUM FOGLIO
043700921119     C                   MOVEL     FVALNP        VI2LNP                          LINEA PART
043800960205     C*
043900960205     C                   EXSR      DATARR
044000951115     C*
044100060919     c* solo se da elaborare
044200060919     c                   if        noelabora=' '
044300060919     c
044400040604    2c                   if        fvaatr<>*blanks
044500040604     c                   seton                                        41
044600040604     c                   else
044700040604     c                   setoff                                       41
044800040604    2c                   endif
044900040604     c
045000911010     C                   ADD       1             NRR
045100040604     C                   WRITE     LRV1DSF
045200060919    2c                   endif
045300911008     C*
045400941221     C     KFVA2         READE     FNFVA02L                               30
045500040604    1C                   ENDDO
045600040528    c
045700960626     C                   Z-ADD     NRR           SAVNRR            5 0
045800961014     C     NRR           COMP      0                                      72
045900960626     C*
046000930127     C                   ENDSR
046100960111     C**************************************************************************
046200960111     C* CONTROLLO COSA PROPORRE NELLA DATA/ORA ARRIVO REALE
046300000628     C* ... carica anche P.O. gestione POSTE
046400960111     C**************************************************************************
046500960111     C     DATARR        BEGSR
046600960205     C****
046700960205     C** D A T A / O R A     A R R I V O
046800960205     C*
046900960205    2C     FVAAED        IFGT      0
047000960205     C* DATA ARRIVO REALE
047100960205     C                   Z-ADD     FVAAED        G02INV
047200960205     C                   MOVE      *ZEROS        G02DAT
047300960205     C                   MOVEL     3             G02ERR
047400960205     C                   CALL      'XSRDA8'
047500960205     C                   PARM                    WLBDAT
047600960205     C                   MOVEL     G02DAT        VI2AED
047700960205     C* ORA ARRIVO REALE
047800960205     C                   MOVEL     FVAAEH        VI2AEH
047900040528     c                   else
048000040528     c                   clear                   vi2aed
048100040528     c                   clear                   vi2aeh
048200960205    2C                   ENDIF
048300960202     C*
048400960202     C* GIRO DATA PARTENZA
048500960202     C                   MOVE      FVADFV        G02INV
048600960202     C                   MOVE      *ZEROS        G02DAT
048700960202     C                   MOVEL     3             G02ERR
048800960202     C                   CALL      'XSRDA8'
048900040528     c
049000960202     C                   PARM                    WLBDAT
049100971223     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
049200960202     C                   MOVEL     G02DAT        VI2DFV
049300960202     C*
049400960202     C* SE E' SOLO BUSTA METTO CAMPO IN R.I.
049500960202    1C     FVAATR        IFEQ      'T'
049600960202     C                   SETON                                        01
049700960202   X1C                   ELSE
049800960202     C                   SETOFF                                       01
049900960202    1C                   END
050000960402     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
050100960402     C*  VISUALIZZO CON 2 ASTERISCHI
050200960402     C     KARB          SETLL     FNARB06L                               34
050300960402     C     *IN34         IFEQ      *ON
050400960402     C                   CLEAR                   VI2AST
050500960402     C                   ELSE
050600960402     C                   MOVEL     '**'          VI2AST
050700960402     C                   ENDIF
050800060919     c
050900060919     C* SE IL P.O. GESTIONE È DIVERSO DAL p.o. foglio
051000060919     c*  verifico se il foglio è relativo alla linea di arrivo richiesta
051100060919     c                   clear                   Noelabora
051200060919     c   11              if        vidpof<>vidfgs
051300060919     c                   clear                   fnlv53ds
051400060919     c                   eval      d53tfo='A'
051500060919     c                   eval      d53nfv=fvanfv
051600060919     c                   eval      d53npg=1
051700060919     c                   eval      d53fgs=fvalnp
051800060919     c                   eval      d53lai=fvalai
051900060919     c                   eval      d53abb='B'
052000060919     c                   eval      d53lna=vidfgs
052100060919     c                   call      'FNLV53R'
052200060919     c                   parm                    fnlv53ds
052300060919     c                   if        d53err<>' '
052400060919     c                   eval      noelabora='N'
052500060919     c                   endif
052600060919     c                   endif
052700000628     C* P.O.GESTIONE
052800000628     C     KFWA1         CHAIN     FNFWA01L                           74
052900000628     C     *IN74         IFEQ      *OFF
053000000628     C     FWAFGS        ANDNE     0
053100000628     C                   Z-ADD     FWAFGS        WORKPT            3 0
053200000628     C                   ELSE
053300000628     C                   Z-ADD     FVALNP        WORKPT            3 0
053400000628     C                   ENDIF
053500000628     C                   CLEAR                   OG143
053600000628     C     WORKPT        CHAIN     AZORG                              74
053700000628     C   74              MOVE      '  '          VI2PTV
053800000628     C  N74              MOVEL     ORGDE3        OG143
053900020207     C  N74§OGNTW        IFEQ      'PPT'
054000000628     C                   MOVE      'PT'          VI2PTV
054100000628     C                   ELSE
054200000628     C                   MOVE      '  '          VI2PTV
054300000628     C                   ENDIF
054400960111     C*
054500960111     C                   ENDSR
054600060920     C**************************************************************************
054700060920     C* Cerco foglio locale con p.o. foglio arrivi
054800060920     C**************************************************************************
054900060920     C     Cerloc        BEGSR
055000060920     c* salvo la data foglio arrivi
055100060920     c                   eval      savdfv=fvvdfv
055200060920     c                   clear                   trovalocal
055300060920     c                   clear                   yy
055400060920     c                   movel     *zeros        nfvloc
055500060920     c
055600060920     c     kfgv          setll     fnfgv02l
055700060920     c     vidpof        readpe    fnfgv02l
055800060920     c
055900060920     c* Cerco il primo foglio locale con data < data foglio arrivi
056000060920     c                   dow       not %eof(fnfgv02l)
056100060920     c* se trovato locale valido --> ok
056200060920     c                   if        fgvttr='L' and fgvatb=' '
056300060920     c                   leave
056400060920     c                   else
056500060920     c     vidpof        readpe    fnfgv02l
056600060920     c                   endif
056700060920     c                   enddo
056800060920     c
056900060920     c* se trovato controllo di non avere un foglio arrivi tra la data
057000060920     c*  foglio locale e la data foglio arrivi richiesta:
057100060920     c*  se c'e' non carico il locale altrimenti lo carico
057200060920     c                   if        not %eof(fnfgv02l)
057300060920     c     kfvv8         setgt     fnfvv08l
057400060920     c     kfvv8b        reade     fnfvv08l
057500060920     c                   dow       not %eof(fnfvv08l) and fvvatb<>' '
057600060920     c     kfvv8b        reade     fnfvv08l
057700060920     c                   enddo
057800060920     c* Verifico la data del fogliio arrivi trovato
057900060920     c                   if        not %eof(fnfvv08l) and
058000060920     c                             fvvdfv>=savdfv
058100060920     c                   eval      trovalocal='S'
058200060920     c                   endif
058300060920     c                   endif
058400060920     c
058500060920     c* Locale trovato: lo carico nel sfl
058600060920    1c                   if        trovalocal='S'
058700060920     c                   eval      kdfv=fgvdfv
058800060920     c     kfgv2         setll     fnfgv02l
058900060920     c     kfgv2         reade     fnfgv02l
059000060920     c
059100060920    2c                   dow       not %eof(fnfgv02l)
059200060920     c
059300060920    3c                   if        fgvttr='L' and fgvatb=' '
059400060920     c
059500060920     c* imposto ciclo di lettura per p.o. e data
059600060920     c
059700060920     c*
059800060920     C                   MOVEL     FgvNFV        VI2FVA                          NUM FOGLIO
059900060920     C                   MOVEL     FgvLNP        VI2LNP                          LINEA PART
060000060920     c                   clear                   vi2aed
060100060920     c                   clear                   vi2aeh
060200060920     c
060300060920     C                   MOVE      FgvDFV        G02INV
060400060920     C                   MOVE      *ZEROS        G02DAT
060500060920     C                   MOVEL     3             G02ERR
060600060920     C                   CALL      'XSRDA8'
060700060920     c
060800060920     C                   PARM                    WLBDAT
060900060920     C* LA DATA FOGLIO VIAGGIO LA VISUALIZZO LUNGA 4
061000060920     C                   MOVEL     G02DAT        VI2DFV
061100060920     c
061200060920     c                   setoff                                       01
061300060920     C* VERIFICO SE IL FV HA DELLE BOLLE : SE NON NE HA
061400060920     C*  VISUALIZZO CON 2 ASTERISCHI
061500060920     C     KARB          SETLL     FNARB06L                               34
061600060920     C     *IN34         IFEQ      *ON
061700060920     C                   CLEAR                   VI2AST
061800060920     C                   ELSE
061900060920     C                   MOVEL     '**'          VI2AST
062000060920     C                   ENDIF
062100060920     c
062200060920     C* SE IL P.O. GESTIONE È DIVERSO DAL p.o. foglio
062300060920     c*  verifico se il foglio è relativo alla linea di arrivo richiesta
062400060920     c                   clear                   Noelabora
062500060920    4c                   if        vidpof<>vidfgs
062600060920     c                   clear                   fnlv53ds
062700060920     c                   eval      d53tfo='G'
062800060920     c                   eval      d53nfv=fgvnfv
062900060920     c                   eval      d53npg=1
063000060920     c                   eval      d53fgs=fgvlnp
063100060920     c                   eval      d53lai=vidpof
063200060920     c                   eval      d53abb='B'
063300060920     c                   eval      d53lna=vidfgs
063400060920     c                   call      'FNLV53R'
063500060920     c                   parm                    fnlv53ds
063600060920    5c                   if        d53err<>' '
063700060920     c                   eval      noelabora='N'
063800060920    5c                   endif
063900060920    4c                   endif
064000060920     C*
064100060920     c* solo se da elaborare
064200070529    4c                   if        noelabora=' '   and yy<5
064300060920     C                   EVAL      VI2LOC='LOCALE'
064400060920     C
064500060920     C                   ADD       1             NRR
064600060920     C                   WRITE     LRV1DSF
064700060920     c                   add       1             yy
064800060920     c                   movel     fgvnfv        nfvloc(yy)
064900060920    4c                   endif
065000060920    3c                   endif
065100060920     c
065200060920     c     kfgv2         reade     fnfgv02l
065300060920    2c                   enddo
065400060920    1c                   endif
065500060920     C*
065600060920     C                   ENDSR
065700941216     C**************************************************************************
065800941216     C*    ELABORAZIONE ABBINAMENTO
065900941216     C**************************************************************************
066000911008     C     ELAB          BEGSR
066100911010     C*
066200930129     C                   Z-ADD     1             NRR
066300930129     C*
066400040528     C* LEGGO TUTTI I FOGLI VIAGGIO ABBINATI  selezionati
066500040604     c* li carico in una schiera perchè li devo stampare
066600040604     c                   clear                   xx                3 0
066700040604     c                   clear                   slnp
066800040604     c                   clear                   sfva
066900951228    1C     NRR           DOWLE     SAVNRR
067000040604     C     NRR           CHAIN     LRv1DSF                            30
067100930129     C*
067200040528    2c                   if        not *in30  and vi2sel='I'
067300040604     c                   add       1             xx
067400040604     c                   z-add     vi2lnp        slnp(xx)
067500040604     c                   z-add     vi2fva        sfva(xx)
067600040604    2c                   endif
067700040604     c                   add       1             nrr
067800040604    1c                   enddo
067900040604     c
068000040604     c* Se non selezionato nessun foglio errore
068100040604    1c                   if        xx=0
068200040604     c                   movel     msg(2)        vccmsg
068300040604     c                   seton                                        2890
068400040604   x1c                   else
068500040604     c*
068600040604     C                   Z-ADD     80            LUNG             15 5
068700040604     C                   MOVEL     *BLANKS       COMMAN           80
068800040604     C                   MOVEA     CMD           COMMAN
068900040604     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
069000040604     C                   PARM                    COMMAN
069100040604     C                   PARM                    LUNG
069200040604     C*
069300040604RM*  C                   OPEN      PRTF198
069400040604     C                   SETON                                        OF
069500040528     c
069600040604     c* Leggo tutti i fogli per elaborare le linee giuste
069700040604     c                   z-add     1             xx
069800040604    2c                   dow       slnp(xx)>0
069900040604     c                   z-add     slnp(xx)      vi2lnp
070000040604     c                   z-add     sfva(xx)      vi2fva
070100040604     c
070200040528     C     KFVA1         CHAIN     FNFVA01L
070300040528    3C                   IF        %found(fnfva01l)
070400040528     C*
070500040528     C     KFWA1         CHAIN     FNFWA01L
070600040528    4C                   IF        NOT %FOUND(FNFWA01L)   OR  fwaatb<>' '
070700040528     C                   CLEAR                   FWAFF3
070800040528     C                   CLEAR                   FWAFF4
070900040528     C                   CLEAR                   FWAFL3
071000040528     C                   CLEAR                   FWAFL4
071100040528    4C                   ENDIF
071200040528     C*
071300040528     C                   Z-ADD     FVALNA        LNA2
071400040604     C                   Z-ADD     1             X                 4 0
071500040528     C                   MOVEL     FVALAI        ALF3              3
071600040528     C     ALF3          LOOKUP    FLP(X)                                 31
071700040528     C*
071800040528    4C                   DOW       *in31
071900040528     C                   MOVEL     FFV(X)        kffv
072000911010     C*
072100941221     C                   Z-ADD     1             KNPG                           1=ARRIVI
072200040528     C                   Z-ADD     VI2FVA        kfva
072300070110     C     KBRV1         SETLL     FNBRV02L
072400070110     C     KBRV1         READE     FNBRV02L                               30
072500911010     C*
072600040528    5C     *IN30         DOWEQ     '0'
072700040528     c                   EXSR      ctrarr
072800911010     C*
072900070110     C     KBRV1         READE     FNBRV02L                               30
073000040528    5C                   ENDDO
073100040528     c*
073200040528     C                   ADD       1             x
073300040528     C     ALF3          LOOKUP    FLP(X)                                 31
073400040528    4C                   ENDdo
073500040604    3c                   endif
073600921120     C*
073700040604     C                   ADD       1             xx
073800040604    2C                   ENDDO
073900040604     c
074000040604    1c                   endif
074100911008     C                   ENDSR
074200040528     C**************************************************************************
074300040528     C*    vedo se collo arrivato per creare anomalia collo part non arr
074400040528     C**************************************************************************
074500040528     C     CTRARR        BEGSR
074600040603     c* richiamo FNLR32R per fare i controlli e far creare l'anomalia:
074700040603     c*  se creo l'anomalia, eseguo la stampa
074800040603     c
074900040604     c                   clear                   para32
075000040604     c                   movel     'S'           p32valore
075100040604     c                   z-add     vidfgs        p32fgs
075200040604     c                   eval      p32dfvv=fvvdfv
075300040603     c                   call      'FNLR32R'
075400040603     c                   parm                    kpjba
075500040604     c                   parm                    DSBRV
075600040604     c                   parm                    para32
075700040604     C* SE COLLO DI VALORE MANCANTE, STAMPO
075800040604     c                   if        p32manca='S'
075900040604     C* MEMORIZZO  DI AVER TROVATO ALMENO UNA MANCANZA
076000040604     C                   EVAL      WMANCA='S'
076100040604     C
076200040604     c                   exsr      stampa
076300040604     c                   endif
076400040604     c                   endsr
076500040604     C**************************************************************************
076600040604     C* STAMPO mNCANZA COLLO DI VALORE
076700040604     C**************************************************************************
076800040604     C     STAMPA        BEGSR
076900040604     C*
077000040604     C* STAMPA TESTATA
077100040604    1C   OF              DO
077200040604     C* PRIMA PARTE TESTATA
077300040604     C                   EXCEPT    TES
077400040604     C*
077500040604     C                   Z-ADD     1             YY                3 0
077600040604     C                   Z-ADD     1             II                3 0
077700040604     C                   MOVEA     *BLANKS       STF
077800040604     C*
077900040604    2C     slnp(YY)      DOWgt     0
078000040604     C*
078100040604    3C     II            IFEQ      191
078200040604     C                   EXCEPT    TES3
078300040604     C                   MOVEA     *BLANKS       STF
078400040604     C                   Z-ADD     1             II
078500040604    3C                   END
078600040604     C*
078700040617     C                   MOVEL     slnp(YY)      ALF3A
078800040604     C                   MOVEL     sfva(YY)      ALF5
078900040604     C                   MOVEA     FVAALF        STF(II)
079000040604     C                   ADD       10            II
079100040604     C**
079200040604     C                   ADD       1             YY
079300040604    2C                   ENDdo
079400040604     C*
079500040604    2C     II            IFGT      1
079600040604     C                   EXCEPT    TES3
079700040604    2C                   END
079800040604     C*
079900040604     C* STAMPO TERZA PARTE TESTATA
080000040604     C                   EXCEPT    TES2
080100040604     C*
080200040604     C                   SETOFF                                       OF
080300040604    1C                   END
080400040604     C* Stampo collo mancante
080500040604     C                   IF        WMANCA='S'
080600040604     C* CHAIN SPEDIZIONE
080700040604     C     KBLP          CHAIN     FNBLP01L
080800040604     C                   IF        %FOUND(FNBLP01L)
080900040604     C* SE C'E' BOLLA E A CAMBIO SPEDIZIONE STAMPO I DATI DELLA SPED
081000040604     C     COMSPE        IFNE      BLPSPE
081100040604     C                   SETON                                        05
081200040604     C                   MOVEL     BLPSPE        COMSPE
081300040604     C                   ENDIF
081400040604     C                   ELSE
081500040604     C                   SETOFF                                       05
081600040604     C                   ENDIF
081700040610     C* CHAIN ANOMALIE PER AVERE DATA E ORA DI CREAZIONE
081800040610     C                   CLEAR                   STADAN
081900040610     C                   CLEAR                   STAOAN
082000040610     C     KANM          CHAIN     FNANM01L
082100040610     C                   IF        %FOUND(FNANM01L)
082200040611     C                   MOVEL     ANMFLO        w0060             6 0          AAAA/MM/GG
082300040611     C                   z-add     w0060         G02INV                         AAAA/MM/GG
082400040610     C                   MOVE      *ZEROS        G02DAT
082500040610     C                   MOVEL     3             G02ERR
082600040610     C                   CALL      'XSRDA8'
082700040610     C                   PARM                    WLBDAT
082800040611     C                   MOVEL     G02DAT        STADAN            8 0          GG/MM
082900040610     C*
083000040610     C                   MOVE      ANMFLO        STAOAN            4 0
083100040610     C                   ENDIF
083200040604     C*
083300040604     C* DATA SPEDIZIONE
083400040604     C   05              DO
083500040604     C                   Z-ADD     BLPAAS        COMAAS                         ANNO (4 CIFRE)
083600040604     C                   MOVE      BLPDSP        G02INV                         AAAA/MM/GG
083700040604     C                   MOVE      *ZEROS        G02DAT
083800040604     C                   MOVEL     3             G02ERR
083900040604     C                   CALL      'XSRDA8'
084000040604     C                   PARM                    WLBDAT
084100040604     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
084200040604     C*
084300040604     C* SEGNACOLLI NON CONSECUTIVI
084400040604     C     BLPFNS        COMP      'S'                                    04
084500040604     C* SERIE>< 0
084600040604     C     BLPNRS        COMP      0                                  0303
084700040604     C                   END
084800040604     C* MITTENTE E DESTINATARIO DI 17 E 20
084900040604     C                   MOVEL     BLPRSM        STARSM           17
085000040604     C                   MOVEL     BLPRSD        STARSD           20
085100040604     C*
085200040604     C* STAMPO DETTAGLIO
085300040604     C                   EXCEPT    DET
085400040604     C                   SETOFF                                       05
085500040604     c                   else
085600040604     c                   except    OK
085700040604     c                   endif
085800040604     c                   endsr
085900960202     C*
086000951115     C*--- SR INIZIALE -----------------------------------------------*
086100951115     C     *INZSR        BEGSR
086200951115     C*
086300951115     C****  KLIST  ****
086400040604     C     KARB          KLIST
086500040604     C                   KFLD                    vi2lnp
086600040604     C                   KFLD                    VI2FVA
086700060919     C     KARB1         KLIST
086800060919     C                   KFLD                    vi2lnp
086900060919     C                   KFLD                    VI2FVA
087000060919     C                   KFLD                    vidfgs
087100951115     C     KFVV1         KLIST
087200040604     C                   KFLD                    vidNPG
087300951115     C                   KFLD                    COMNFV            5 0
087400060919     C                   KFLD                    kFGS
087500060920     C     KFVV8         KLIST
087600060920     C                   KFLD                    vidpof
087700060920     C                   KFLD                    vidnpg
087800060920     C                   KFLD                    fgvdfv
087900060920     C     KFVV8b        KLIST
088000060920     C                   KFLD                    vidpof
088100060920     C                   KFLD                    vidnpg
088200060920     C     KFgv          KLIST
088300060920     C                   KFLD                    vidpof
088400060920     C                   KFLD                    fvvdfv
088500060920     C     KFgv2         KLIST
088600060920     C                   KFLD                    vidpof
088700060920     C                   KFLD                    kdfv
088800951115     C     KFVA1         KLIST
088900040528     C                   KFLD                    VI2LNP
089000951115     C                   KFLD                    VI2FVA
089100040528     C                   KFLD                    VIDFGS
089200951115     C     KFVA2         KLIST                                                  FNFVA02L
089300040604     C                   KFLD                    vidNPG                         ..CATEGORIA
089400951115     C                   KFLD                    COMNFV                         ..FOGLIO
089500060919     C                   KFLD                    kfgs
089600951115     C     KBRV1         KLIST                                                  FNBRV02L
089700951115     C                   KFLD                    KNPG                           ..CATEGORIA
089800040528     C                   KFLD                    kfva                           ..NR.FOGLIO
089900040528     C                   KFLD                    vi2LNP                         ..FIL.GESTIONE
090000040528     C                   KFLD                    kffv                           ..LNA
090100000628     C     KFWA1         KLIST
090200000628     C                   KFLD                    FVALNP
090300000628     C                   KFLD                    FVANFV
090400000628     C                   KFLD                    FVALAI
090500040604     C     Kblp          KLIST                                                  FIBRV09L
090600040604     C                   KFLD                    P32Aas                         ..LNP
090700040604     C                   KFLD                    P32lnp                         ..LNA
090800040604     C                   KFLD                    P32nrs                         ..SERIE
090900040604     C                   KFLD                    P32nsp                         ..SEGNACOLLO
091000040610     C     KANM          KLIST                                                  FIBRV09L
091100040610     C                   KFLD                    P32Aas                         ..LNP
091200040610     C                   KFLD                    P32lnp                         ..LNA
091300040610     C                   KFLD                    P32nrs                         ..SERIE
091400040610     C                   KFLD                    P32nsp                         ..SEGNACOLLO
091500040610     C                   KFLD                    KCAA                           ..SEGNACOLLO
091600951115     C*
091700951115     C                   ENDSR
091800040604     C**************************************************************************
091900040604RM*  OPRTF198   E            TES              02
092000040604     O                       STA(1)              66
092100040604     O                       STA(2)             132
092200040604     O                       STA(13)            198
092300040604     O                       RSUT                20
092400040604     O                       UDATE8             181 '  /  /    '
092500040604     O                       PAGE          Z    198
092600040604     O          E            TES         1
092700040604     O                       UTIME              179 '  :  :  '
092800040604     O          E            TES         2
092900040604     O                       STA(3)              66
093000040604RM*  O                       VIDFGS              22
093100040604     O                       DESFGS              43
093200040604     O                       COMNFV        Z     56
093300040604     O                       VIDDFA        8     73
093400040604     O          E            TES         2
093500040604     O                       STA(4)              66
093600040604     O          E            TES3        1
093700040604     O                       stf                198
093800040604     O          E            TES2        2
093900040604     O                       STA(5)              66
094000040604     O                       STA(8)             132
094100040610     O                       STA(14)            198
094200040604     O          E            TES2        1  1
094300040604     O                       STA(6)              66
094400040611     O                       STA(9)             132
094500040611     O                       STA(15)            198
094600040604     O*
094700040604     O          E            DET         1
094800040604     O                  05   BLPAAS        Z
094900040604     O                  05   BLPLNP        Z   +  1
095000040604     O                  05 03BLPNRS        Z   +  1
095100040604     O                  05 03                  +  0 '-'
095200040604     O                  05   BLPNSP        Z   +  0
095300040604     O                  05   STADSP            +  1 '  /  '
095400040604     O                  05   BLPLNA        Z   +  1
095500040604     O                  05   STARSM            +  1
095600040604     O                  05   STARSD            +  1
095700040604     O                  05N04BLPNCD        Z   +  1
095800040604     O                  05N04                  +  0 '/'
095900040604     O                  05N04BLPNCA        Z   +  0
096000040604     O                  05 04                    84 'NON SEQUENZIALI'
096100040604     O                  05   BLPZNC            +  1
096200040604     O                       BRVLNP            +  4
096300040604     O                       BRVLNA            +  2
096400040604     O                       BRVNRS        Z   +  3
096500040604     O                       BRVNSC        Z   +  4
096600040610     O                       STADAN            +  8 '  /  /    '
096700040610     O                       STAOAN            +  2 '  :  '
096800040604     O          E            OK          3
096900040604     O                       STA(11)             66
097000040604     O          E            OK          0  0
097100040604     O                       STA(11)             66
097200040604     O**************************************************************************
097300040604** STA
097400040617XXXXXXXXXXXXXXXXXXXX                                                 1
097500040604 ** COLLI DI VALORE MANCANTI AL TERMINAL DI ARRIVO **                2
097600040604FOGLIO ARRIVI: DI  XXX-XXXXXXXXXXXXXXXXXXXX   N.RO XXXXX   DEL XX/   3
097700040604FOGLI VIAGGIO PARTENZA SELEZIONATI:                                  4
097800040604    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO         5
097900040604ANNO LNP SER NUMERO  DATA ARR                                        6
098000040604XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx   7
098100040610     SEGNACOLLI    ZON   C O L L O   M A N C A N T E     CREAZIONE   8
098200040610      DAL /   AL   ARR   LNP  LNA  SERIE   NUMERO           DATA     9
098300040610XX XXXXXXX/XXXXXXX XX    XXX  XXX   XX    XXXXXXX        XX/XX/XXX  10
098400040604             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
098500040604 **  FINE  STAMPA  **                                               12
098600040604                         FNLRV1R                          PAG.XXXX  13
098700040610 ANOMALIA                                                           14
098800040610    ORA                                                             15
098900040610X  XX:XX                                                            16
099000040611**
099100040604OVRPRTF FILE(PRTF198) USRDTA('ColValore')
099200000606**  MSG
099300040604Foglio arrivi errato                                                          1
099400040604Effettuare almeno una selezione per la elaborazione                           1
099500050616Nessun foglio da elaborare                                                    1
099600060919Nessun foglio partenza abbinato relativo al p.o. in gestione indicato         1
