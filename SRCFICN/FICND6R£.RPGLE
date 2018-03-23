000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114     H* FICND6R *----------------------------------------------------*
000300020516     H*         - STAMPA RIEPILOGO VALORIZZAZIONI AUTOTRASPORTATORI
000400000000     H*--------------------------------------------------------------*
000500000808     FTNTLZ01L  IF   E           K DISK
000600021203     Ffiapd01L  IF   E           K DISK
000700020513     Ffiftt02L  IF   E           K DISK
000800021105     Ffiftt06L  IF   E           K DISK    rename(FIFTT000:FIFTT006)
000900931202     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000930707     F*
001100931129     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
001200981217     D ST              S             66    DIM(25) CTDATA PERRCD(1)             RIGHE STAMPA
001300931202     D*
001400071102     D SKey17          S             17    DIM(9999)                             PADR DA ELAB
001500071102     D cELP            S              7  0 DIM(9999)                             PADR DA ELAB
001600071102     D ELP             S              7  0 DIM(9999)                             PADR DA ELAB
001700071102     D PAD             S              7  0 DIM(9999)                             PADRONCINI
001800071102     D PDD             S              1    DIM(9999)                             PADRONCINI
001900071102     D DES             S             20    DIM(9999)                             DESCR  PADR
002000071102     D FRN             S              7    DIM(9999)                             FORNIT PADR
002100071102     D CSF             S              3    DIM(9999)                             FORNIT S.INFOR
002200071102     D KSC             S              7    DIM(9999)                             FORNIT RIORD
002300071102     D SK1             S              7  0 DIM(9999)
002400071102     D SK2             S             12  3 DIM(9999)
002500071102     D SK3             S             12  3 DIM(9999)
002600071102     D SK4             S             12  3 DIM(9999)
002700950125     D*
002800021104     D* per il cambio di fornitore:
002900021104     D* già stampati (ossia da non ristampare e sommare)
003000071102     D PRTED           S             15    DIM(9999)                             printed
003100021104     D*
003200021105     D wsoc            S                   like(Fttsoc)
003300021105     D wcdf            S                   like(Fttcdf)
003400021105     D*
003500021105     D savpad          S                   like(Fttpdr)
003600021104     D savcdf          S                   like(Fttcdf)
003700021104     D ptdpdr          S                   like(Fttpdr)
003800021104     D ptdcdf          S                   like(Fttcdf)
003900021104     D ptdcod          S             15
004000021104     D ptdcodftt       S             15
004100021104     D*
004200011114     D* PASSAGGIO DATI DA FILTRO                        - FICND5R -
004300931126     D PARAM           DS
004400931130     D  PARP1                 28     34
004500950126     D  PARPD1                31     34  0
004600931130     D  PARP2                 35     41
004700950126     D  PARPD2                38     41  0
004800931126     D  PARKSC                42     48  0
004900931129     D  PARDKS                49     78
005000931130     D  PARCSF                79     81
005100931210     D  PARSML                82     82
005200940328     D* TIPO STAMPA PROSPETTO (SINTETICO/ANALITICO)
005300940328     D  PARTSP                83     83
005400950125     D  PARDD1                84     91  0
005500950125     D  W01DD1                92     99  0
005600950125     D  PARDD2               100    107  0
005700950125     D  W01DD2               108    115  0
005800931126     D WLBDAT          DS
005900950125     D  G02DAT                 1      8  0
006000950125     D  G02INV                 9     16  0
006100950125     D  G02ERR                17     17
006200950125     D  G02TGI                18     22  0
006300931129     D                 DS
006400950126     D  AA                     1      4  0
006500950126     D  MM                     5      6  0
006600950126     D  GG                     7      8  0
006700950126     D  WDDC                   1      8  0
006800931202     D                 DS
006900950126     D  AAD                    1      4  0
007000950126     D  MMD                    5      6  0
007100950126     D  GGD                    7      8  0
007200950126     D  DATA                   1      8  0
007300931129     D KPJBA         E DS
007400931129     D CNCR80        E DS
007500020516     D dtlz01        E DS
007600931129     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007700000000     I/SPACE 3
007800900521     C****************************************************************
007900900521     C*  RIEPILOGO INDICATORI
008000900521     C***************************************************************
008100931130     C* 01    - IMMESSO PADRONCINO DAL
008200931130     C* 02    - IMMESSO PADRONCINO AL
008300931130     C* 03    - IMMESSO CODICE FORNITORE
008400931130     C* 04    - IMMESSA DATA AL
008500940328     C* 06    - RICHIESTA LA STAMPA SINTETICA
008600940330     C* 07    - USATO PER ESEGUIRE UNA EXCPT UNA SOLA VOLTA
008700950131     C* 08    - OFF   NON ESISTONO CONTEGGI DA STAMPARE
008800931130     C* 10    - DEL PADRONCINO C'E' FORNITORE
008900931130     C* 11    - CI SONO PIU' CODICI PADRONCINI
009000931209     C* 12    - ESEGUO DECOD SOLO UNA VOLTA
009100011115     C* 14    - Stampa a salto pagina
009200931209     C* 15    - HO STAMPATO ALMENO UN PADRONCINO
009300931210     C* 16    - SIMULAZIONE
009400931130     C* 30    - COMODO
009500931130     C* 31    - LETTURA
009600940328     C* 35    - LOKUP
009700900521     C*****************************************************************
009800000000     C     *ENTRY        PLIST
009900000000     C                   PARM                    KPJBA
010000931207     C                   MOVEL     KPJBU         PARAM
010100931123     C                   Z-ADD     1             CODUT             1 0
010200931129     C                   CALL      'X§PARUT'
010300931129     C                   PARM                    UT§DSE
010400931129     C                   MOVEL     REC80         CNCR80
010500900511     C*---------------------------------------------------------------*
010600000808     C     KTNTLZ        KLIST
010700000808     C                   KFLD                    WTIP              2
010800000808     C                   KFLD                    WKSC              7 0
010900000808     C                   KFLD                    WCSF              3
011000000808     C                   MOVEL     'P '          WTIP
011100020513     C     KFTT1         KLIST
011200020513     C                   KFLD                    WPDR
011300020513     C                   KFLD                    WFVL
011400020513     C     KFTT          KLIST
011500020513     C                   KFLD                    WPDR
011600931129     C                   KFLD                    WFVL
011700020513     C                   KFLD                    WDDC
011800931129     C     KFTT2         KLIST
011900020513     C                   KFLD                    WPDR
012000931129     C                   KFLD                    WFVL
012100931129     C                   KFLD                    WDDC
012200931129     C                   KFLD                    WTSR
012300021105     C     KFTT6         KLIST
012400021105     C                   KFLD                    WSOC
012500021105     C                   KFLD                    WCDF
012600021105     C                   KFLD                    WTSR
012700021105     C                   KFLD                    WFVL
012800021203     C     Kapdw         KLIST
012900021203     C                   KFLD                    apdtip
013000021203     C                   KFLD                    wpdr1
013100021203     C     Kapdf         KLIST
013200021203     C                   KFLD                    apdtip
013300021203     C                   KFLD                    fttpdr
013400021203     c                   movel     'A'           apdtip
013500931015     C*---------------------------------------------------------------*
013600931126     C*
013700931126     C* CONTROLLO LE SELEZIONI IMPOSTATE
013800931126     C                   EXSR      CTRSEL
013900931126     C*
014000931126     C* CARICO LA SCHIERA DEI PADRONCINI
014100931126     C                   EXSR      CARPAD
014200931130     C*
014300931130     C* STAMPA LA VIDEATA DELLE SELEZIONI
014400931130     C                   EXSR      STASEL
014500931129     C**
014600931129     C* 01 OFF 03 OFF- LEGGO LA SCHIERA RIORDINATA DEI FORNITORI
014700931129     C*                E DA QUI MI COLLEGO AI PADRONCINI
014800931129    1C     *IN01         IFEQ      *OFF
014900931129     C     *IN03         ANDEQ     *OFF
015000931129     C*
015100931130     C* E L A B O R O   Q U E L L I   S E N Z A   F O R N I T O R E
015200071102     C                   Z-ADD     1             C                 5 0
015300931130    2C     FRN(C)        DOWNE     *BLANKS
015400931129     C*
015500931130    3C     FRN(C)        IFEQ      *ZEROS
015600021106     C                   EXSR      ELAB4
015700931130    3C                   ENDIF
015800931130     C                   ADD       1             C
015900931130    2C                   ENDDO
016000931129     C*
016100931130     C*  E L A B O R O   Q U E L L I   C O N   F O R N I T O R E
016200071102    2C     K             DO        9999          K
016300931209     C* SE GIA' ELABORATO  FORNITORE A 0 --> LO SALTO
016400931130     C*
016500931130    3C     KSC(K)        IFGT      *ZEROS
016600931209     C     KSC(K)        ANDNE     WALF7
016700931209     C*
016800931130     C                   Z-ADD     1             C
016900931130     C     KSC(K)        LOOKUP    FRN(C)                                 30
017000931129     C*
017100931129     C* CARICO LA SCHIERA ELP - PADRONCINI CON STESSO FORNITORE
017200931129     C                   CLEAR                   ELP
017300931130     C                   CLEAR                   WNUM3
017400071102     C                   Z-ADD     C             D                 5 0
017500931209     C                   MOVEL     KSC(K)        WALF7             7
017600931129     C*
017700931129    4C     *IN30         DOWEQ     *ON
017800931129     C                   MOVEL     PAD(C)        ELP(C)
017900931129     C                   ADD       1             C
018000071102     C                   ADD       1             WNUM3             5 0
018100931129     C     KSC(K)        LOOKUP    FRN(C)                                 30
018200931129    4C                   ENDDO
018300931129     C* ELABORAZIONE
018400931130    4C     WNUM3         IFGT      1
018500021105     C                   EXSR      ELAB2
018600931130   X4C                   ELSE
018700931209     C                   Z-ADD     D             C
018800021105     C                   EXSR      ELAB3
018900931130    4C                   ENDIF
019000931129    3C                   ENDIF
019100931129    2C                   ENDDO
019200931129     C*
019300931129   X1C                   ELSE
019400931129     C*
019500931129     C* 01 ON OR 03 ON - RICHIESTO 1 FORNITORE O 1 CODICE PADRONCINO
019600931129     C                   Z-ADD     1             C
019700931129     C* CI SONO PIU' CODICI PADRONCINO PER 1 FORNITORE
019800931129    2C     PAD(2)        IFNE      0
019900021104    2C     *in01         andeq     *off
020000940504     C                   Z-ADD     1             D
020100931129     C                   MOVEA     PAD           ELP
020200021105     C*
020300021105     C* se richiesto un solo fornitore
020400021105     C                   Move      parksc        Walf7
020500931129     C                   EXSR      ELAB2
020600931129     C*
020700931129     C                   ELSE
020800931129     C*
020900931129     C* C'E' SOLO UN CODICE PADRONCINO
021000931129     C                   EXSR      ELAB1
021100931129    2C                   ENDIF
021200931129    1C                   ENDIF
021300931126     C*
021400950131     C* STAMPA TOTALI GENERALI
021500020611    1C*    WTOTG         IFGT      0
021600020611    1C     WTOTG         IFne      0
021700030508     C     WtotAM        ORNE      0
021800030508     C     WtotDM        ORNE      0
021900950131     C   OF              EXCEPT    TESTA
022000950131     C   OF              SETOFF                                       OF
022100950131     C                   EXCEPT    TOTGEN
022200950131    1C                   ENDIF
022300950131     C*
022400931209     C   15              EXCEPT    FINE
022500950131     C  N08              EXCEPT    MSG
022600950131     C*
022700931123     C                   SETON                                        LR
022800931126     C*
022900931126     C* CONTROLLO LE SELEZIONI IMPOSTATE -----------------------------*
023000931126     C     CTRSEL        BEGSR
023100931126     C* 01 ON - IMMESSO SOLTANTO PADRONCINO DAL
023200931130     C* 02 ON - IMMESSO PADRONCINO AL
023300931126     C* 03 ON - IMMESSO CODICE FORNITORE
023400931129     C* 04 ON - IMMESSA DATA AL
023500940328     C* 06 ON - RICHIESTA LA STAMPA SINTETICA
023600950126     C*
023700950126     C                   CLEAR                   WPDR1
023800950126     C                   CLEAR                   WPDR2
023900950126     C*
024000931126     C     PARPD2        IFGT      0
024100931126     C                   SETON                                        02
024200950131     C                   MOVEL     PARP1         WPDR1
024300950131     C                   MOVEL     PARP2         WPDR2
024400931126     C                   ELSE
024500931126     C     PARPD1        IFGT      0
024600931126     C                   SETON                                        01
024700950126     C                   MOVEL     PARP1         WPDR1
024800931126     C                   ENDIF
024900931126     C                   ENDIF
025000931126     C*
025100931126     C     PARKSC        IFGT      0
025200931126     C                   SETON                                        03
025300931126     C                   ENDIF
025400931129     C*
025500931129     C     PARDD2        IFGT      0
025600931129     C     PARDD2        ANDNE     PARDD1
025700931129     C                   SETON                                        04
025800931129     C                   ENDIF
025900931129     C*
026000940328     C     PARTSP        IFEQ      '2'
026100940328     C                   SETON                                        06
026200940328     C                   ENDIF
026300931129     C*
026400931129     C* DEFINIZIONE CAMPI
026500931129     C*
026600931129     C     *LIKE         DEFINE    FTTFVL        WFVL
026700931129     C     *LIKE         DEFINE    FTTPDR        WPDR
026800931129     C     *LIKE         DEFINE    FTTTSR        WTSR
026900931202     C     *LIKE         DEFINE    FTTDDC        STADDC
027000011115     C     *LIKE         DEFINE    FTTDDC        ST2DDC           -2
027100950126     C     *LIKE         DEFINE    FTTPDR        WPDR1
027200950126     C     *LIKE         DEFINE    FTTPDR        WPDR2
027300020517     c     *LIKE         DEFINE    fttpdr        savpdr
027400931129     C*
027500931129     C                   MOVEL     'C'           WFVL
027600931202     C* ORA
027700931202     C                   TIME                    UTIME             6 0
027800950126     C* LA DATA DEL GIORNO LA PRENDO DA TIME
027900950126     C                   TIME                    W0140            14 0
028000950126     C                   MOVE      W0140         UDATE8            8 0
028100931209     C*
028200931126     C                   ENDSR
028300931123     C*
028400931126     C* CARICO I PADRONCINI CON RELATIVI CODICI FORNITORI ------------*
028500931126     C     CARPAD        BEGSR
028600931126     C                   Z-ADD     1             C
028700931126     C*
028800931129     C* IMMESSO UN SOLO PADRONCINO
028900931126    1C     *IN01         IFEQ      *ON
029000021105     C*
029100021203     C     kapdw         CHAIN     fiapd000                           30
029200950126     C*
029300950126     C     *IN30         IFEQ      *OFF
029400021104      * deve leggere le testate per individuare se nel range di date c'è stato
029500021104      *  un cambio di fornitore.
029600021104     c                   exsr      LEG_FTT
029700021104      *
029800950126     C                   ENDIF
029900931126     C*
030000931126   X1C                   ELSE
030100931126     C*
030200021105     C* Un solo Fornitore
030300021105     C     *IN03         Ifeq      *On
030400021105     C*
030500021105     C*   deve partire dai conteggi per prendere i padroncini
030600021105     C*    relativi al fornitore impostato a video
030700021105     c                   exsr      LEG_FTX
030800021105     C*
030900021105     c                   else
031000021105     C*
031100021203     C     kapdw         SETLL     fiapd000
031200021203     C     apdtip        READe     fiapd000                               30
031300931126     C*
031400931126    2C     *IN30         DOWEQ     *OFF
031500931126     C* SE IMMESSO AL E DIVENTA > --> FINE LETTURA
031600931126     C*
031700931126    3C     *IN02         IFEQ      *ON
031800950126     C     APDPDR        ANDGT     WPDR2
031900931126     C                   SETON                                        30
032000021104     c                   leave
032100931126    3C                   ENDIF
032200931126     C*
032300020520    3C     *in30         IFEQ      *off
032400021104      * deve leggere le testate per individuare se nel range di date c'è stato
032500021104      *  un cambio di fornitore.
032600021104     c                   exsr      LEG_FTT
032700021104      *
032800021104     c                   if        pad(c) > 0
032900931126     C                   ADD       1             C
033000021104     C                   end
033100021105    4C*********          ENDIF
033200021105     C*
033300931126    3C                   ENDIF
033400931126     C*
033500021203     C  N30apdtip        READe     fiapd000                               30
033600931126    2C                   ENDDO
033700021105     C*
033800021105     c                   end
033900021105     C*
034000931126    1C                   ENDIF
034100910419     C*
034200931129     C* 01 OFF 03 OFF - RIORDINO LA SCHIERA DEI FORNITORI
034300931129     C     *IN01         IFEQ      *OFF
034400931129     C     *IN03         ANDEQ     *OFF
034500931129     C                   MOVEA     FRN           KSC
034600931129     C                   SORTA     KSC
034700931129     C*
034800931129     C* "K" CONTIENE LA 1 POSIZIONE PIENA DELLA SCHIERA RIORDINATA
034900931129     C* CERCANDOLA DAL  NUMERO DEI PADRONCINI PRESENTI
035000931129     C                   Z-ADD     1             C
035100950126     C     0000000       LOOKUP    PAD(C)                                 30
035200931129     C  N30              Z-ADD     1             K
035300030124      *
035400931129     C*
035500951204     C* 502 E' DATO DALL SOMMA DI :
035600071102     C* - 500  ELEMENTI DI SCHIERA
035700940120     C* -   1 PERCHE' L'ELEMENTO  TROVATO NELLA SKI PADR E' IL 1 VUOTO
035800940120     C* -   1 PERCHE' 100 C DAREBBE L'ULTIMA POSIZIONE VUOTA E NON LA
035900940120     C*       PRIMA PIENA
036000071102     C***30502           SUB       C             K                 5 0
036100071102     C   3010001         SUB       C             K                 5 0
036200931129     C                   ENDIF
036300021104     C*
036400021104     C                   ENDSR
036500021104     C*
036600021104     C*---------------------------------------------------------------*
036700021104     C* legge FTT per vedere se, nel range di periodo ci sono cambi   *
036800021104     C* di codice fornitore                                           *
036900021104     C*---------------------------------------------------------------*
037000021104     C     LEG_FTT       BEGSR
037100021104     C*
037200021104     C                   Z-ADD     APDPDR        WPDR
037300021104     C                   Z-ADD     W01DD1        WDDC
037400021104     C* CONTROLLO SE ANNO DISESTILE
037500021104     C                   EXSR      ANNOB
037600021104     C* D A T A   D A L
037700021104     C     KFTT2         setll     fiftt000
037800021104     C     KFTT1         reade     fiftt000                               31
037900021104     C*
038000021104     c     *in31         Doweq     *off
038100021104    2C     fttddc        andle     W01DD2
038200021104     C*
038300021104     C*  Imposta schiere
038400021104     c                   if        savcdf <> fttcdf or
038500021104     c                             savpad <> fttpdr
038600021210      *
038700021210     C*  non deve aggiungere PAD/FRN/SOC se già presente
038800021210     c                   move      apdpdr        pdr7A             7
038900021210     c                   move      fttcdf        frn7A             7
039000021210     c                   move      fttsoc        soc3A             3
039100021210      * imposta la chiave univoca fattacon i 3 campi.
039200021210     c                   clear                   key17a           17
039300021210     c                   eval      key17a = soc3a + frn7a + pdr7A
039400071102     c                   z-add     1             k17               5 0
039500021210     c     key17a        lookup    skey17(k17)                            88
039600021210     C*
039700030124     C*  flag x controllare se aggiunto in schiera
039800030124     c                   clear                   aggiunto          1
039900030124     C*
040000021210     C*  se non c'era sulla schiera lo salva e imposta i dati
040100021210     C*  nelle schiere che servono per l'elaborazione
040200021210     C*   altrimenti se già c'è deve saltare tutto.
040300021210     c                   if        *in88 = *off
040400021210      *
040500071102     c                   z-add     1             k17               5 0
040600021210     c     *blank        lookup    skey17(k17)                            88
040700021210     c   88              movel     key17a        skey17(k17)
040800021210      *
040900030124     c                   move      'S'           aggiunto
041000021104     C                   MOVEL     APDPDR        PAD(C)
041100021104     C                   MOVEL     APDPDD        PDD(C)
041200100223     C                   MOVEL     APDRSf        DES(C)
041300021104     C                   MOVE      FTTCDF        FRN(C)
041400021104     C                   MOVE      FTTSOC        CSF(C)
041500021104     c                   eval      savcdf = fttcdf
041600021104     c                   eval      savpad = fttpdr
041700021210      *
041800021210     c                   end
041900021210     C*
042000021104     c                   end
042100021104     C*
042200021104     C     KFTT1         reade     fiftt000                               31
042300021104     C*
042400021104     C* se è cambiato il fornitore ed è entreo il range incrementa
042500021104     C*  prima di impostare nuovamente le schiere
042600021104     c  N31savcdf        ifne      fttcdf
042700021104    2C     fttddc        andle     w01dd2
042800030124    2C     aggiunto      andeq     'S'
042900021104     c                   add       1             C
043000021104     c                   end
043100021104     C*
043200021104     c                   enddo
043300021104     C*
043400931123     C                   ENDSR
043500021105     C*---------------------------------------------------------------*
043600021105     C* legge FTT per vedere se, nel range di periodo ci sono cambi   *
043700021105     C* di codice fornitore                                           *
043800021105     C*---------------------------------------------------------------*
043900021105     C     LEG_FTX       BEGSR
044000021105     C*
044100021105     C* imposta le chiavi di ricerca
044200021105     c                   move      *zeros        wcdf
044300021105     c                   move      parksc        wcdf
044400021105     c                   move      parcsf        wsoc
044500021105     C*
044600021105     C* D A T A   D A L
044700021105     C     KFTT6         Chain     fiftt06L                           31
044800021105     C*
044900021105     c     *in31         Doweq     *off
045000021105     C*
045100021105    2C     fttddc        ifge      W01DD1
045200021105    2C     fttddc        andle     W01DD2
045300021105     C*
045400021105     C*  Imposta schiere
045500021105     c                   if        savcdf <> fttcdf or
045600021105     c                             savpad <> fttpdr
045700021203     c     kapdf         chain     fiapd01l                           77
045800021105     C  n77              MOVEL     APDPDR        PAD(C)
045900021105     C  n77              MOVEL     APDPDD        PDD(C)
046000100223     C  n77              MOVEL     APDRSf        DES(C)
046100021105     C                   MOVE      FTTCDF        FRN(C)
046200021105     C                   MOVE      FTTSOC        CSF(C)
046300021105     c                   eval      savcdf = fttcdf
046400021105     c                   eval      savpad = fttpdr
046500021105     c                   add       1             C
046600021105     c                   end
046700021105     C*
046800021105     c                   end
046900021105     C*
047000021105     C     KFTT6         reade     fiftt06L                               31
047100021105     C*
047200021105     c                   enddo
047300021105     C*
047400021105     C                   ENDSR
047500021104     C*---------------------------------------------------------------*
047600021104     C* ELABORAZIONE PER UN PADRONCINO -------------------------------*
047700931129     C     ELAB1         BEGSR
047800931209     C                   SETOFF                                       101112
047900931130     C* SE C'E' FORNITORE
048000931130     C     FRN(C)        IFGT      *ZEROS
048100931130     C                   SETON                                        10
048200931130     C                   ENDIF
048300931130     C*
048400021104     C*  stampa in base ai fornitori legati al padroncino nel periodo
048500071102     c                   do        9999          C
048600021104      *
048700021104      *  esce dal ciclo quando ha esaurito i fornitori
048800021104     c                   if        FRN(C) = *blank and PAD(C) = 0
048900021104     c                   leave
049000021104     c                   end
049100021104     C*
049200931129     C                   Z-ADD     PAD(C)        WPDR
049300931202     C                   Z-ADD     W01DD1        WDDC
049400931129     C* D A T A   D A L
049500011114     C     KFTT2         CHAIN     fiftt000                           31
049600021104     c* somma e stampa solo se il fornitore corrisponde con quello selezionato
049700021104     C*  in schiera
049800021104     c  n31              do
049900021104      * controlla se non è già stato trattato precedentemente (a causa di un cambio
050000021104      * codice fornitore durante il periodo richiesto)
050100021104     c                   movel     fttpdr        ptdcodftt
050200021104     c                   move      fttcdf        ptdcodftt
050300021104     c                   z-add     1             ptd
050400021104     c     ptdcodftt     lookup    prted(ptd)                             66
050500021104      *
050600021104     c     *in66         ifeq      *off
050700021104     c                   move      fttcdf        frn07             7
050800021104     c                   if        frn07 = frn(c)
050900021104     C                   EXSR      STAMPA                                        STAMPA
051000021104     c                   end
051100021104     c                   end
051200021104      *
051300021104     c                   end
051400931129     C*
051500931129     C* D A T A   D A L   /   A L
051600931129    1C     *IN04         IFEQ      *ON
051700931129     C* CONTROLLO SE ANNO DISESTILE
051800931129     C                   EXSR      ANNOB
051900931129     C*
052000931129    2C     WDDC          DOWLT     W01DD2
052100931129     C*
052200931129     C                   ADD       1             WDDC
052300931129     C*
052400931129    3C     GG            IFGT      XDTF(MM)
052500931129    4C     MM            IFEQ      12
052600931129     C                   ADD       1             AA
052700931129     C                   Z-ADD     1             MM
052800931129     C                   EXSR      ANNOB
052900931129   X4C                   ELSE
053000931129     C                   ADD       1             MM
053100931129    4C                   ENDIF
053200931129     C                   Z-ADD     1             GG
053300931129    3C                   ENDIF
053400931129     C*
053500011114     C     KFTT2         CHAIN     fiftt000                           31
053600021104     c* somma e stampa solo se il fornitore corrisponde con quello selezionato
053700021104     C*  in schiera
053800021104     c  n31              do
053900021104      * controlla se non è già stato trattato precedentemente (a causa di un cambio
054000021104      * codice fornitore durante il periodo richiesto)
054100021104     c                   movel     fttpdr        ptdcodftt
054200021104     c                   move      fttcdf        ptdcodftt
054300021104     c                   z-add     1             ptd
054400021104     c     ptdcodftt     lookup    prted(ptd)                             66
054500021104      *
054600021104     c     *in66         ifeq      *off
054700021104     c                   move      fttcdf        frn07             7
054800021104     c                   if        frn07 = frn(c)
054900021104     C                   EXSR      STAMPA                                        STAMPA
055000021104     c                   end
055100021104     c                   end
055200021104     C*
055300021104     c                   end
055400021104     C*
055500931129    2C                   ENDDO
055600931129    1C                   ENDIF
055700931202     C*
055800931202     C* S T A M P A   T O T A L E
055900931209     C* SE HO TROVATO QUALCOSA
056000981202     C     WPDRT         IFNE      0
056100020808     C     WPDRAM        ORNE      0
056200020808     C     WPDRDM        ORNE      0
056300021104      *
056400021104      * a totale fornitore:
056500021104      * salva in schiera per poi non ristampare in seguito la combinazione
056600021104      *  padroncino/fornitore già trattato.
056700021104     C                   movel     ptdpdr        ptdcod
056800021104     C                   move      ptdcdf        ptdcod
056900071102     c                   z-add     1             ptd               5 0
057000021104     c     *blank        lookup    prted(ptd)                             66
057100021104     c   66              movel     ptdcod        prted(ptd)
057200021104      *
057300940330     C     *IN06         IFEQ      *OFF
057400940330     C                   EXCEPT    TOTALE
057500940330     C                   ELSE
057600940330     C*
057700940330     C     *INOF         IFEQ      *ON
057800940330     C                   EXCEPT    TESTA
057900940330     C                   EXCEPT    TESTA2
058000940330     C                   SETOFF                                       OF
058100940330     C                   ENDIF
058200940330     C*
058300940330     C* DECODIFICA PADRONCINO E FORNITORE
058400940330     C                   EXSR      DECOD
058500940330     C* STAMPA DETTAGLIO
058600940330     C                   EXCEPT    DETT2
058700940330     C                   ENDIF
058800940330     C*
058900931202     C                   CLEAR                   WPDRT
059000011115     c                   clear                   wpdiv
059100981217     C                   CLEAR                   WPDRAM
059200981217     C                   CLEAR                   WPDRDM
059300931209     C                   ENDIF
059400950131     C*
059500021104     c                   enddo
059600021104     C*
059700940328     C  N06              SETON                                        OF
059800931202     C                   ENDSR
059900931129     C*
060000931129     C* ELABORAZIONE CON PIU' PADRONCINI ----------------------------*
060100931129     C     ELAB2         BEGSR
060200931130     C                   SETON                                        1011
060300950131     C                   SETOFF                                       1207
060400940328     C*
060500071102     C                   Z-ADD     0             Y                 5 0
060600940328     C                   CLEAR                   SK1
060700940328     C                   CLEAR                   SK2
060800981217     C                   CLEAR                   SK3
060900981217     C                   CLEAR                   SK4
061000020513     C*
061100020513     c                   movea     elp           celp
061200020513     C                   SORTA     cELP
061300020517     C*
061400071102     C                   DO        9999          Cc                5 0
061500020513     C                   IF        cELP(cC) = 0
061600020513     C                   ITER
061700020513     C                   END
061800020513     C                   MOVE      cELP(Cc)      WPDR
061900931129     C                   Z-ADD     W01DD1        WDDC
062000020516     C*
062100020513     C     KFTT          SETLL     fiftt000
062200020513     C  N04KFTT          READE     fiftt000                               31
062300020513     C   04KFTT1         READE     fiftt000                               31
062400931129     C*
062500950126    1C     *IN31         DOWEQ     *OFF
062600020517     c                   move      fttpdr        savpdr
062700020517     C*
062800020517    1C     *IN31         DOWEQ     *OFF
062900020517     c     fttpdr        andeq     savpdr
063000020517     C*
063100931129     C* SE DAL AL CONTROLLO DATA CHE NON SUPERI
063200950126    2C     *IN04         IFEQ      *ON
063300931129     C     FTTDDC        ANDGT     W01DD2
063400020516     C*
063500950126   X2C                   ELSE
063600020516     C*
063700020513     C                   Z-ADD     1             C
063800020513     C     FTTPDR        LOOKUP    ELP(C)                                 30
063900931129     C*
064000020513    3C     *IN30         IFEQ      *ON
064100020513     C     FTTTSR        ANDEQ     ' '
064200021105     C*-
064300021105     c                   move      fttcdf        frn07
064400021105      * per padroncino con più fornitori sullo stesso periodo
064500021105     c                   if        frn07 = walf7
064600931129     C                   EXSR      STAMPA
064700021105     c                   end
064800021105     C*-
064900950126    3C                   ENDIF
065000931129     C*
065100020516    3C                   ENDIF
065200011114     C  N04KFTT          READE     fiftt000                               31
065300020513     C   04KFTT1         READE     fiftt000                               31
065400020513     C*  04WFVL          READE     fiftt000                               31
065500020516     C*
065600950126    1C                   ENDDO
065700020517     C*
065800020517     c     *in06         ifeq      *off
065900020517     C* a rottura di padroncino esegue il totale
066000020517     c                   z-add     1             z
066100020517     c     savpdr        lookup    sk1(z)                                 21
066200020517     C   21              EXCEPT    TOTAUT
066300020517     c                   end
066400020517     C*
066500020517    1C                   ENDDO
066600020517     C*
066700020513    1C                   ENDDO
066800931202     C*
066900931202     C* S T A M P A   T O T A L E
067000931209     C* SE HO TROVATO QUALCOSA
067100020611    1C*    WPDRT         IFGT      0
067200020611    1C     WPDRT         IFne      0
067300030508     C     WPDRAM        ORNE      0
067400030508     C     WPDRDM        ORNE      0
067500950126     C*
067600950126     C* STAMPO RIGA TRATTEGGIATA
067700950126     C  N06              EXCEPT    TRAT
067800940328     C*
067900940330     C* MI RIPOSIZIONO SU SCHIERA PER PRENDERE DECODIFICA FORNITORE
068000950126     C   06              Z-ADD     D             C
068100950126     C   06              EXSR      DECOD
068200950126     C*
068300940328     C                   Z-ADD     1             Z
068400950126    2C     SK1(Z)        DOWGT     0
068500940328     C*
068600940328     C* DECODIFICO CODICE PADRONCINO
068700950126     C                   MOVE      SK1(Z)        COMPDR            7 0
068800940328     C                   Z-ADD     1             Y
068900940328     C     COMPDR        LOOKUP    PAD(Y)                                 35
069000940328     C*
069100950126     C* S I N T E T I C O
069200950126    3C     *IN06         IFEQ      *ON
069300950126     C*
069400950126    4C     *INOF         IFEQ      *ON
069500940330     C                   EXCEPT    TESTA
069600940330     C                   EXCEPT    TESTA2
069700940330     C                   SETOFF                                       OF
069800950126    4C                   ENDIF
069900940330     C*
070000940330     C  N07              EXCEPT    FIT
070100940330     C  N07              SETON                                        07
070200940328     C                   EXCEPT    DETT2
070300950126    3C                   ENDIF
070400950126     C*
070500950126     C* A N A L I T I C O
070600020517      *  non lo deve più stampare
070700020517     c                   goto      no_print
070800950126     C  N06              EXCEPT    TOTPAD
070900020517     c     no_print      tag
071000950126     C*
071100940328     C                   ADD       1             Z
071200950126    2C                   ENDDO
071300940328     C*
071400950126     C   06              EXCEPT    TOT2
071500950126     C  N06              EXCEPT    TOTALE
071600940328     C*
071700931202     C                   CLEAR                   WPDRT
071800011115     c                   clear                   wpdiv
071900981217     C                   CLEAR                   WPDRAM
072000981217     C                   CLEAR                   WPDRDM
072100950126    1C                   ENDIF
072200950130     C*
072300940328     C  N06              SETON                                        OF
072400931129     C                   ENDSR
072500021105     C*
072600021105     C*---------------------------------------------------------------*
072700021105     C* ELABORAZIONE PER UN PADRONCINO                                *
072800021105     C*---------------------------------------------------------------*
072900021105     C     ELAB3         BEGSR
073000021105     C*
073100021105     c                   move      *zeros        walf8             8
073200021105     c                   move      walf7         walf8             8
073300021105     C                   SETOFF                                       101112
073400021105     C* SE C'E' FORNITORE
073500021105     C     FRN(C)        IFGT      *ZEROS
073600021105     C                   SETON                                        10
073700021105     C                   ENDIF
073800021105     C*
073900021105     C                   Z-ADD     PAD(C)        WPDR
074000021105     C                   Z-ADD     W01DD1        WDDC
074100021105     C* D A T A   D A L
074200021105     C     KFTT2         CHAIN     fiftt000                           31
074300021105     c  n31              if        fttcdf = walf8
074400021105     C                   EXSR      STAMPA                                        STAMPA
074500021105     c                   end
074600021105     C*
074700021105     C* D A T A   D A L   /   A L
074800021105    1C     *IN04         IFEQ      *ON
074900021105     C* CONTROLLO SE ANNO DISESTILE
075000021105     C                   EXSR      ANNOB
075100021105     C*
075200021105    2C     WDDC          DOWLT     W01DD2
075300021105     C*
075400021105     C                   ADD       1             WDDC
075500021105     C*
075600021105    3C     GG            IFGT      XDTF(MM)
075700021105    4C     MM            IFEQ      12
075800021105     C                   ADD       1             AA
075900021105     C                   Z-ADD     1             MM
076000021105     C                   EXSR      ANNOB
076100021105   X4C                   ELSE
076200021105     C                   ADD       1             MM
076300021105    4C                   ENDIF
076400021105     C                   Z-ADD     1             GG
076500021105    3C                   ENDIF
076600021105     C*
076700021105     C     KFTT2         CHAIN     fiftt000                           31
076800021105     c  n31              if        fttcdf = walf8
076900021105     C                   EXSR      STAMPA                                        STAMPA
077000021105     c                   end
077100021105      *
077200021105    2C                   ENDDO
077300021105    1C                   ENDIF
077400021105     C*
077500021105     C* S T A M P A   T O T A L E
077600021105     C* SE HO TROVATO QUALCOSA
077700021105     C     WPDRT         IFNE      0
077800021105     C     WPDRAM        ORNE      0
077900021105     C     WPDRDM        ORNE      0
078000021105     C     *IN06         IFEQ      *OFF
078100021105     C                   EXCEPT    TOTALE
078200021105     C                   ELSE
078300021105     C*
078400021105     C     *INOF         IFEQ      *ON
078500021105     C                   EXCEPT    TESTA
078600021105     C                   EXCEPT    TESTA2
078700021105     C                   SETOFF                                       OF
078800021105     C                   ENDIF
078900021105     C*
079000021105     C* DECODIFICA PADRONCINO E FORNITORE
079100021105     C                   EXSR      DECOD
079200021105     C* STAMPA DETTAGLIO
079300021105     C                   EXCEPT    DETT2
079400021105     C                   ENDIF
079500021105     C*
079600021105     C                   CLEAR                   WPDRT
079700021105     c                   clear                   wpdiv
079800021105     C                   CLEAR                   WPDRAM
079900021105     C                   CLEAR                   WPDRDM
080000021105     C                   ENDIF
080100021105     C*
080200021105     C  N06              SETON                                        OF
080300021105     C*
080400021105     C                   ENDSR
080500021106     C*---------------------------------------------------------------*
080600021106     C* ELABORAZIONE PER UN PADRONCINO    che non ha fornitore        *
080700021106     C*---------------------------------------------------------------*
080800021106     C     ELAB4         BEGSR
080900021106      *
081000021106     C                   SETOFF                                       101112
081100021106     C* SE C'E' FORNITORE
081200021106     C     FRN(C)        IFGT      *ZEROS
081300021106     C                   SETON                                        10
081400021106     C                   ENDIF
081500021106     C*
081600021106     C                   Z-ADD     PAD(C)        WPDR
081700021106     C                   Z-ADD     W01DD1        WDDC
081800021106     C* D A T A   D A L
081900021106     C     KFTT2         CHAIN     fiftt000                           31
082000021106     C  N31              EXSR      STAMPA                                        STAMPA
082100021106     C*
082200021106     C* D A T A   D A L   /   A L
082300021106    1C     *IN04         IFEQ      *ON
082400021106     C* CONTROLLO SE ANNO DISESTILE
082500021106     C                   EXSR      ANNOB
082600021106     C*
082700021106    2C     WDDC          DOWLT     W01DD2
082800021106     C*
082900021106     C                   ADD       1             WDDC
083000021106     C*
083100021106    3C     GG            IFGT      XDTF(MM)
083200021106    4C     MM            IFEQ      12
083300021106     C                   ADD       1             AA
083400021106     C                   Z-ADD     1             MM
083500021106     C                   EXSR      ANNOB
083600021106   X4C                   ELSE
083700021106     C                   ADD       1             MM
083800021106    4C                   ENDIF
083900021106     C                   Z-ADD     1             GG
084000021106    3C                   ENDIF
084100021106     C*
084200021106     C     KFTT2         CHAIN     fiftt000                           31
084300021106     C  N31              EXSR      STAMPA                                        STAMPA
084400021106    2C                   ENDDO
084500021106    1C                   ENDIF
084600021106     C*
084700021106     C* S T A M P A   T O T A L E
084800021106     C* SE HO TROVATO QUALCOSA
084900021106     C     WPDRT         IFNE      0
085000021106     C     WPDRAM        ORNE      0
085100021106     C     WPDRDM        ORNE      0
085200021106     C     *IN06         IFEQ      *OFF
085300021106     C                   EXCEPT    TOTALE
085400021106     C                   ELSE
085500021106     C*
085600021106     C     *INOF         IFEQ      *ON
085700021106     C                   EXCEPT    TESTA
085800021106     C                   EXCEPT    TESTA2
085900021106     C                   SETOFF                                       OF
086000021106     C                   ENDIF
086100021106     C*
086200021106     C* DECODIFICA PADRONCINO E FORNITORE
086300021106     C                   EXSR      DECOD
086400021106     C* STAMPA DETTAGLIO
086500021106     C                   EXCEPT    DETT2
086600021106     C                   ENDIF
086700021106     C*
086800021106     C                   CLEAR                   WPDRT
086900021106     c                   clear                   wpdiv
087000021106     C                   CLEAR                   WPDRAM
087100021106     C                   CLEAR                   WPDRDM
087200021106     C                   ENDIF
087300021106     C*
087400021106     C  N06              SETON                                        OF
087500021106     C*
087600021106     C                   ENDSR
087700021106     C*
087800021105     C*--------------------------------------------------------------*
087900021105     C* CONTROLLO SE ANNO BISESTILE                                 -*
088000021105     C*--------------------------------------------------------------*
088100931129     C     ANNOB         BEGSR
088200931129     C     AA            DIV       4             XDTRES            2 0          * INDIVIDUA
088300931129     C                   MVR                     XDTRES                         * ANNO BISEST.
088400931129     C     XDTRES        IFEQ      0
088500931129     C                   Z-ADD     29            XDTF(2)                        * AGGIUSTA
088600931129     C                   ELSE                                                   * FINE FEBBRAIO
088700931129     C                   Z-ADD     28            XDTF(2)                        * IN ARRAY XDTF
088800931129     C                   END
088900931129     C                   ENDSR
089000931129     C*
089100931130     C* STAMPA LE SELEZIONI FATTE -----------------------------------*
089200931130     C     STASEL        BEGSR
089300931130     C*
089400931130     C* PADRONCINO DAL - AL
089500931130     C     PARPD1        IFGT      0
089600931130     C                   Z-ADD     1             C
089700950126     C     WPDR1         LOOKUP    PAD(C)                                 30
089800931130     C   30              MOVEL     DES(C)        DESPD1           20
089900931130     C  N30              CLEAR                   DESPD1
090000931130     C                   ENDIF
090100931130     C     PARPD2        IFGT      0
090200931130     C                   Z-ADD     1             C
090300950126     C     WPDR2         LOOKUP    PAD(C)                                 30
090400931130     C   30              MOVEL     DES(C)        DESPD2           20
090500931130     C  N30              CLEAR                   DESPD1
090600931130     C                   MOVEL     PARP2         STAP2             9
090700931130     C                   MOVE      '-'           STAP2
090800931130     C                   ENDIF
090900931130     C*
091000931130     C     *IN01         IFEQ      *ON
091100931130     C     *IN02         OREQ      *ON
091200931130     C                   MOVEL     PARP1         STAP1             9
091300931130     C                   MOVE      '-'           STAP1
091400931130     C                   ENDIF
091500931210     C* SIMULAZIONE
091600931210     C     PARSML        IFEQ      'S'
091700931210     C                   SETON                                        16
091800931210     C                   ENDIF
091900931130     C*
092000931130     C                   EXCEPT    TESTA
092100931130     C                   EXCEPT    STAPAR
092200931130     C                   SETON                                        OF
092300931130     C*
092400931130     C                   ENDSR
092500931130     C*
092600931130     C* STAMPA ------------------------------------------------------*
092700940330     C     STAMPA        BEGSR
092800950131     C                   SETON                                        08
092900940330     C*
093000940330     C* TOTALE
093100011115     C                   move      fttdiv        stadiv            3
093200011115     C                   Z-ADD     FTTITT        STATOT           12 3
093300940330     C                   ADD       FTTITA        STATOT
093400011114      * >>>>> divisa
093500940330     C                   ADD       FTTTPE        STATOT
093600940330     C                   ADD       FTTTBN        STATOT
093700940330     C                   ADD       FTTTIM        STATOT
093800940330     C                   ADD       FTTMNT        STATOT
093900981217     C*
094000011115     C                   Z-ADD     FTTMNT        STAMNT           12 3
094100011115     C                   Z-ADD     FTTTIM        STATIM           12 3
094200021104     C*
094300021104     C*  Salva i campi per la rottura di fornitore
094400021104     C*    su stesso periodo per lo stesso padroncino
094500021104     c                   setoFF                                       44
094600021104     c                   if        fttcdf <> savcdf
094700021104     c                   seton                                        44
094800021104     c                   eval      savcdf = fttcdf
094900021104     c                   eval      ptdcdf = fttcdf
095000021104     c                   eval      ptdpdr = fttpdr
095100021104     c                   end
095200940330     C*
095300940330     C* A N A L I T I C O
095400940330     C     *IN06         IFEQ      *OFF
095500940330     C* STAMPA TESTATA
095600931130     C     *INOF         IFEQ      *ON
095700021104     C     *IN44         oreq      *ON
095800931209     C* DECODIFICA TESTATA
095900940330     C  N12              EXSR      DECOD
096000931202     C                   EXCEPT    TESTA
096100940330     C                   EXCEPT    TES2
096200940330     C                   EXCEPT    TES3
096300931209     C                   SETON                                        15
096400011115     c                   seton                                        14
096500931209     C                   SETOFF                                       OF
096600931202     C                   ENDIF
096700931202     C* GIRO DATA
096800950126     C                   Z-ADD     FTTDDC        DATA
096900950126     C                   Z-ADD     MMD           GGMM              4 0
097000950126     C                   MOVEL     GGD           GGMM
097100950126     C                   Z-ADD     AAD           STADDC
097200950126     C                   MOVEL     GGMM          STADDC
097300011115     C                   Z-ADD     AAD           ST2DDC
097400011115     C                   MOVEL     GGMM          ST2DDC
097500931202     C* IMPORTI MANUALI
097600931202     C     FTTFIT        IFEQ      'M'
097700931202     C                   MOVEL     'MAN'         STAMN1            3
097800931202     C                   ELSE
097900931202     C                   CLEAR                   STAMN1
098000931202     C                   ENDIF
098100931202     C*
098200931202     C     FTTFIA        IFEQ      'M'
098300931202     C                   MOVEL     'MAN'         STAMN2            3
098400931202     C                   ELSE
098500931202     C                   CLEAR                   STAMN2
098600931202     C                   ENDIF
098700931202     C* STAMPO DETTAGLIO
098800011115     c                   if        fttpdr <> prtpdr or *in14 = *on
098900011115     c                   setoff                                       14
099000011115     c                   move      fttpdr        prtpdr            7 0
099100011115     c                   move      fttpdr        stapdr            7
099200011115     c                   movel     des(c)        desc             16
099300011115     c                   else
099400011115     c                   move      *blank        stapdr
099500011115     c                   movel     *blank        desc
099600011115     c                   end
099700940330     C                   EXCEPT    DETT
099800940330     C                   ENDIF
099900940328     C*
100000940330     C* CARICO PADRONCINI CON I RELATIVI TOTALI PRESTAZIONI
100100940330     C* SE + CODICI PADPRONCINI CON UNICO FORNITORE
100200950126     C     *IN11         IFEQ      *ON
100300950130     C*
100400071102     C                   Z-ADD     1             Z                 5 0
100500940328     C     FTTPDR        LOOKUP    SK1(Z)                                 35
100600940328     C*
100700940328     C     *IN35         IFEQ      *OFF
100800940328     C                   ADD       1             Y
100900940328     C                   MOVEL     FTTPDR        SK1(Y)
101000940330     C                   Z-ADD     STATOT        SK2(Y)
101100981217     C                   Z-ADD     STATIM        SK3(Y)
101200981217     C                   Z-ADD     STAMNT        SK4(Y)
101300940328     C                   ELSE
101400940328     C                   ADD       STATOT        SK2(Z)
101500981217     C                   ADD       STATIM        SK3(Z)
101600981217     C                   ADD       STAMNT        SK4(Z)
101700940328     C                   ENDIF
101800950131     C                   ENDIF
101900940328     C*
102000931202     C* MEMORIZZO IL TOTALE
102100011115     c                   move      stadiv        wpdiv             3
102200011115     c                   move      stadiv        wgdiv             3
102300011115     C                   ADD       STATOT        WPDRT            14 3
102400011115     C                   ADD       STATIM        WPDRAM           14 3
102500011115     C                   ADD       STAMNT        WPDRDM           14 3
102600011115     C* MEMORIZZO IL TOTALE GENERALE
102700011115     C                   ADD       STATOT        WTOTG            14 3
102800011115     C                   ADD       STATIM        WTOTAM           14 3
102900011115     C                   ADD       STAMNT        WTOTDM           14 3
103000931202     C                   ENDSR
103100931130     C*
103200931130     C* DECODIFICHE PER TESTATA -------------------------------------*
103300931130     C     DECOD         BEGSR
103400020703      *
103500020703     C                   CLEAR                   autesc           17
103600020703      * se non c'è il fornitore *in10 = *off
103700020703     c                   if        *in10 =*off or PDD(C)='S'
103800020703     c                   eval      autesc = 'AUTOFAT. ESCLUSA'
103900020703     c                   end
104000020703      *
104100931130     C* CODICE FORNITORE
104200931130     C     *IN10         IFEQ      *ON
104300000808     C                   MOVEL     CSF(C)        WCSF
104400931209     C                   MOVEL     FRN(C)        WKSC
104500000808     C     KTNTLZ        CHAIN     TNTLZ01L                           91
104600931202     C*
104700931202     C     *IN91         IFEQ      *OFF
104800020516     c                   movel     tlzflo        dtlz01
104900020516     c                   if        §tlzfl1 ='S'
105000020516     c                   eval      autesc = 'AUTOFAT. ESCLUSA'
105100020516     c                   end
105200011115     C                   MOVEL     TLZRSC        STADFR           25
105300931202     C                   ELSE
105400931202     C                   CLEAR                   STADFR
105500931130     C                   ENDIF
105600931202     C                   ENDIF
105700931209     C*
105800931209     C                   SETON                                        12
105900931130     C*
106000931130     C                   ENDSR
106100931130     C*
106200931129     OQSYSPRT   E            TESTA            02
106300931129     O                       ST(1)               66
106400931129     O                 N04   ST(3)              132
106500931129     O                  04   ST(2)              132
106600960607     O                     06                    41 'SINTETICO'
106700960607     O                    N06                    41 'ANALITICO'
106800950126     O                       PARDD1              83 '  /  /    '
106900950126     O                  04   PARDD2              97 '  /  /    '
107000950126     O                       UDATE8             120 '  /  /    '
107100931129     O                       PAGE          Z    132
107200931209     O          E            TESTA       1  1
107300931210     O                  16   ST(20)              99
107400950126     O                       UTIME              118 '  :  :  '
107500931202     O*
107600931209     O          E            STAPAR           15
107700931209     O                       ST(18)              86
107800950126     O                       PARDD1              63 '  /  /    '
107900950126     O                       PARDD2              78 '  /  /    '
108000960607     O          E            STAPAR           20
108100931130     O                       ST(5)               86
108200931130     O                       STAP1               63
108300931130     O                       DESPD1              84
108400960607     O          E            STAPAR           22
108500931130     O                       ST(6)               86
108600931130     O                       STAP2               63
108700931130     O                       DESPD2              84
108800960607     O          E            STAPAR           25
108900931130     O                       ST(7)               86
109000931209     O                  03   PARKSC              61
109100931209     O                  03                       63 '-'
109200931202     O                  03   PARDKS              94
109300960607     O          E            STAPAR           27
109400931202     O                       ST(8)               86
109500931209     O                  03                       60 'GAITRA'
109600931209     O                       PARCSF              63
109700960607     O          E            STAPAR           29
109800940330     O                                           52 'TIPO PROSPETTO ....:'
109900960607     O                     06                    63 'SINTETICO'
110000960607     O                    N06                    63 'ANALITICO'
110100081007     O*-----
110200081007     O          E            STAPAR           35
110300081007     O                                           35 'Nel prospetto, gli importi'
110400081007     O                                          + 1 'NON sono comprensivi del'
110500081007     O                                          + 1 'Costo Stimato adeguamento'
110600081007     O                                          + 1 'Carburante'
110700931202     O*
110800940328     O* S T A M P A    A N A L I T I C A
110900931202     O          E    10      TES2        0  0
111000931202     O                       ST(9)               66
111100940328     O                 N11   ST(10)             118
111200931202     O                       FRN(C)              18
111300931209     O                                           19 '-'
111400020513     O                       STADFR              44
111500931209     O                 N11   WPDR                71
111600931209     O                 N11                       72 '-'
111700931209     O                 N11   DES(C)              92
111800020516     o                       AUTESC             132
111900931209     O          E    10      TES2        0  1
112000931209     O                       ST(9)               66
112100931209     O                 N11   ST(10)             118
112200931209     O                       FRN(C)              18
112300020513     O                       STADFR              44
112400931209     O                 N11   WPDR                71
112500931209     O                 N11                       72 '-'
112600931209     O                 N11   DES(C)              92
112700020516     o                       AUTESC             132
112800931202     O          E   N10      TES2        0  0
112900931202     O                       ST(10)              66
113000931209     O                       WPDR                19
113100931209     O                       DES(C)              40
113200020703     o                       AUTESC             132
113300931209     O          E   N10      TES2        0  1
113400931209     O                       ST(10)              66
113500931209     O                       WPDR                19
113600931209     O                       DES(C)              40
113700020703     o                       AUTESC             132
113800931202     O*
113900931202     O          E            TES3        1
114000931202     O                       ST(12)              66
114100931202     O                       ST(14)             132
114200931202     O          E            TES3        1  1
114300931202     O                       ST(15)             132
114400931202     O*
114500931202     O          E            DETT        1
114600011115     O*****                  FTTPDR              16
114700011115     O                       stapdr               7
114800011115     O****                   DES(C)              37
114900011115     O                       desc                24
115000011115     O****                   STADDC              10 '  /  /    '
115100011115     O                       ST2DDC              33 '  /  /  '
115200011115     O                       FTTITT        Q     45
115300011115     O                       STAMN1              49
115400011115     O                       FTTITA        Q     61
115500011115     O                       STAMN2              65
115600011115     O                       FTTTPE        Q     77
115700011115     O                       FTTTBN        Q     90
115800011115     O                       FTTTIM        Q    102
115900011115     O                       FTTMNT        Q    114
116000011115     O                       STATOT        Q    128
116100011115     O                       fttdiv             132
116200931202     O*
116300950126     O          E          10TRAT        1
116400950126     O                       ST(17)              66
116500950126     O                       ST(17)             132
116600950126     O          E          10TOTPAD      1
116700011115     O                                           98 'TOTALE AUTOTRASP.:'
116800011115     O                       SK1(Z)             106
116900011115     O                       SK2(Z)        O    128
117000020517     O*
117100020517     O          E            TOTAUT      1
117200020517     O                                          103 '------------------'
117300020517     O                                          111 '------------------'
117400020517     O                                          132 '--------------------------'
117500020517     O*
117600020517     O          E            TOTAUT      1
117700020517     O                                          103 'TOTALE AUTOTRASP.:'
117800020517     O                       SK1(Z)             111
117900020517     O                       SK2(Z)        O    128
118000020517     O*
118100020517     O          E            TOTAUT      1
118200020517     O                                          103 '------------------'
118300020517     O                                          111 '------------------'
118400020517     O                                          132 '--------------------------'
118500950126     O*
118600931202     O          E            TOTALE      1
118700931202     O                       ST(17)              66
118800931202     O                       ST(17)             132
118900931202     O          E            TOTALE      1
119000011115     O                     10                   105 'TOTALE FORNITORE :'
119100011115     O                    N10                   105 'TOTALE AUTOTRASP.:'
119200011115     O                       WPDRT         O    128
119300011115      * divisa
119400011115     o                       wpdiv              132
119500931209     O          E            TOTALE      1
119600931209     O                       ST(17)              66
119700931209     O                       ST(17)             132
119800931209     O          E            FINE             65
119900931209     O                       ST(19)              99
120000950131     O          E            MSG              60
120100950131     O                       ST(23)              99
120200931130     O*
120300940328     O* S T A M P A    S I N T E T I C A
120400940328     O          E            TESTA2      1
120500940330     O                       ST(17)              66
120600940330     O                       ST(17)             132
120700940330     O          E            TESTA2      1
120800940330     O                       ST(21)              66
120900940330     O                       ST(22)             132
121000940330     O          E            TESTA2      1
121100940330     O                       ST(17)              66
121200940330     O                       ST(17)             132
121300940328     O*
121400940330     O          E      N11   DETT2       2
121500981217     O                     10FRN(C)               7
121600011115     O                     10STADFR              35
121700011115     O                       WPDR                44
121800011115     O                       DES(C)              66
121900011115     O                       WPDRAM        O     83
122000011115     O                       WPDRDM        O    103
122100011115     O                       WPDRT         O    128
122200011115      * divisa
122300011115     o                       wpdiv              132
122400940330     O          E       11   FIT         0  1
122500940330     O                                          132 ' '
122600940330     O          E       11   DETT2       1
122700981217     O                       FRN(C)               7
122800011115     O                       STADFR              35
122900011115     O                       SK1(Z)              44
123000011115     O                       DES(Y)              66
123100011115     O                       SK3(Z)        O     83
123200011115     O                       SK4(Z)        O    103
123300011115     O                       SK2(Z)        O    120
123400950131     O          E       11   TOT2        2
123500981217     O                       FRN(C)               7
123600011115     O                       STADFR              35
123700020419     O                                           64 ' TOTALE:'
123800011115     O                       WPDRAM        O     83
123900011115     O                       WPDRDM        O    103
124000011115     O                       WPDRT         O    128
124100011115      * divisa
124200011115     o                       wpdiv              132
124300950131     O*
124400950131     O* STAMPA TOTALE GENERALE
124500950704     O          E            TOTGEN      2
124600950131     O                       ST(17)              66
124700950131     O                       ST(17)             132
124800950704     O          E            TOTGEN      1
124900950131     O                       ST(11)              66
125000011115     O                       WTOTAM        O     83
125100011115     O                       WTOTDM        O    103
125200011115     O                       WTOTG         O    128
125300011115      * divisa
125400011115     o                       wgdiv              132
125500950704     O          E            TOTGEN      1
125600950131     O                       ST(17)              66
125700950131     O                       ST(17)             132
125800950130     O*
125900981217**   XDTF
126000931129312831303130313130313031
126100931129**
126200020516                    * RIEPILOGO XXXXXXXXX VALORIZZAZIONI AUTOTRAS.
126300020516   DAL XX/XX/XXXX AL XX/XX/XXXX *  FICND6R  XX.XX.XXXX   PAG. XXXX  2
126400020516   DEL XX/XX/XXXX *                FICND6R  XX.XX.XXXX   PAG. XXXX  3
126500960926PUNTO OPERAT. DI APPARTENENZA .:                                    4
126600960607AUTOTRASPORTATORE ........:  DAL                                    5
126700931129                              AL                                    6
126800960607FORNITORE .....................:                                    7
126900931202            SISTEMA INFORMATIVO:                                    8
127000020513Fornitore: XXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXX                        9
127100960607Autotrasp.: XXXXXXX-XXXXXXXXXXXXXXXXXXXX                           10
127200011115T O T . G E N E R A L E   DELLE   P R E S T A Z I O N I            11
127300011115   AUTOTRASPORTATORE       DATA        BASE        ACCESSORI       12
127400011115XXXXXXX-XXXXXXXXXXXXXXXX XX/XX/XX XXXXXXX.XXX MAN XXXXXXX.XXX MAN  13
127500011115  PIKING/        BONUS     AGGIUNTE   DELTA AL    T O T A L E  DIV 14
127600011115 ETICHETT.                 MANUALI     MINIMO                      15
127700011115XXXXXXX.XXX  XXXXXXX.XXX XXXXXXX.XXX XXXXXXX.XXX XXXXXXXXX.XXX VVV 16
127800931209------------------------------------------------------------------ 17
127900931209DATA VALORIZZAZIONE ......:  DAL             AL                    18
128000931209                  ***  F I N E     S T A M P A  ***                19
128100940328                        S I M U L A Z I O N E                      20
128200011115 F O R N I T O R E                   AUTOTRASPORTATORE             21
128300011115 AGGIUNTE MANUALI     DELTA AL MINIMO   ----TOTALE PRESTAZIONE---- 22
128400020516   **  NON ESISTONO VALORIZZAZIONI DA STAMPARE !! **
128500981217XXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXX  XXXXXXXXXXXXXXXX
128600011115XXXX      XXXXXXXXXXX     XXXXXXXXXXX    XXXXXXXXX     XXXXXXXXXXX
