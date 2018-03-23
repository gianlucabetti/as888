000100980717     H DECEDIT('0,') DATEDIT(*DMY.)
000200110908     H* FNLSW1R *----------------------------------------------------*
000300110908     H*       Invio anomalie ai clienti mittenti - filtro
000400161209     H*       Invio dettaglio esiti spediz.  KPI - filtro
000500000000     H*--------------------------------------------------------------*
000600110908     FFNLSW1D   CF   E             WORKSTN
000700110908     F                                     SFILE(LSW1DSF:NRR)
000800920730     FCNACO00F  IF   E           K DISK
000900920901     FTABEL00F  IF   E           K DISK
001000161212     Ffiabk02l  IF   E           K DISK
001100920901     FAZORG01L  IF   E           K DISK
001200920804     D L1              S              3  0 DIM(30)
001300950530     D*
001400170104     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001500980108     D*
001600980108     D* SCHIERA DI COMODO X CARICAMENTO LNP
001700110912     D LP              S              3  0 DIM(20)
001800110912     D CLP             S              3  0 DIM(20)
001900950530     D**
002000110912     D* PASSAGGIO PARAMETRI AL BATCH - FNLSW2R -
002100950530     D**
002200110913     D fnlsw1ds        DS
002300110912     D  COMdchd                1      8  0
002400110914     D  COMdcha                9     16  0
002500110914     D  LNP                   17     56P 0
002600110912     D                                     DIM(20)
002700110914     D  CLI                   57    136P 0
002800110912     D                                     DIM(20)
002900110914     D  NRS                  137    176
003000110912     D                                     DIM(20)
003100110914     D  videmail             177    201
003200111116     D  parGEST              202    202
003300161212     D  vidok                203    203
003400161212     D  videveok             204    204
003500161216     D  vidcons              205    205
003600161222     D  UNIF                 206    225    dim(20)
003700110912     d
003800961017     D                 DS
003900961017     D  VIDKLP                 1      3  0
004000961017     D  VIDKSC                 4      7  0
004100961017     D  VIDCLI                 1      7  0
004200941228     D*
004300111228     D dsemail       E DS
004400111228     D DS3C          E DS
004500920421     D KPJBA         E DS
004600161221     D tntaa1ds      E DS
004700161221       // -?Legami clienti
004800161221     d TIBS10DS      e ds                  INZ(*EXTDFT)
004900161221     d  skc                          11    dim(500) overlay(d10skc)
005000941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005100941228     D***
005200941228     D WLBDAT          DS                  INZ
005300941228     D  G02DAT                 1      8  0
005400941228     D  G02INV                 9     16  0
005500941228     D  G02ERR                17     17
005600941228     D  G02TGI                18     22  0
005700941228     D***
005800941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
005900941228     D***
006000941228     D WGIDAT          DS                  INZ
006100941228     D  GIODAT                 1      8  0
006200941228     D  GIOINV                 9     16  0
006300941228     D  GIOTGI                17     21  0
006400941228     D* DS PER TRUL06R - CARICAMENTO £X
006500941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006600941228     D  LIN                    1     90  0
006700941228     D                                     DIM(30)
006800961025     D*
006900021203
007000060310     D alfa3cnrs       S              2
007100941228     D*
007200110908     D TIBS34DS      E DS
007300110908     d Azuteds       e ds                  extname(Azute00f)
007400110908     d dDatiute      e ds
007500111003     d Dataiso         s               d   datfmt(*iso)
007600111003     d Datasys         s               d   datfmt(*iso)  inz(*sys)
007700111003     d dateu           s              8  0
007800161221     d wok             s              1
007900161221     d
008000161221      /copy gaitrasrc/srcprotopr,TIBS10R
008100900702     C****************************************************************
008200900702     C*  RIEPILOGO INDICATORI
008300900702     C***************************************************************
008400161207     C* 01    - invio anomalie ai clienti
008500161207     C* 02    - KPI   clienti
008600161207     C* 36    - SFLNXTCHG
008700920804     C* 37    - PULIZIA SUBFILE
008800920522     C* 30/35 - DI COMODO
008900920901     C* 40/57 - ERRORE
009000110912     C* 90    - ERRORE GENERICO
009100900702     C*****************************************************************
009200000000     C     *ENTRY        PLIST
009300000000     C                   PARM                    KPJBA
009400161209     c                   movel     kpjbu         tipo              1
009500950530     C*
009600110912     C                   Z-ADD     1             CODUT             1 0
009700110908     c     *dtaara       define    §azute        azuteds
009800110908     c     *dtaara       define    §datiute      ddatiute
009900110908     c                   in(E)     *dtaara
010000110908    1c                   If        %error  or RSUT = *blanks
010100110908     c                   Clear                   Tibs34ds
010200110908     c                   Call      'TIBS34R'
010300110908     c                   Parm                    Tibs34ds
010400110908     c                   In        *dtaara
010500110908    1c                   EndIf
010600110912     C                   MOVEL     rsut          PARDUT           30
010700161209     c* Verifico se si tratta di invio anomalie o kpi
010800161209     c                   if        tipo='K'
010900161209     c                   seton                                        02
011000161209     c                   else
011100161209     c                   seton                                        01
011200161209     c                   endif
011300920901     C*---------------------------------------------------------------*
011400111003     c                   eval      dataiso=datasys
011500111003     c                   eval      dateu= %dec(dataiso)
011600111003     C*---------------------------------------------------------------*
011700920730     C* ACCESSO CNACO
011800920730     C     KACO          KLIST
011900920730     C                   KFLD                    CODUT
012000110908     C                   KFLD                    dutKCI
012100920901     C                   KFLD                    KSC               7 0
012200920730     C* ACCESSO TABEL
012300920730     C     KTAB          KLIST
012400920730     C                   KFLD                    CODUT
012500920730     C                   KFLD                    COD               2
012600920730     C                   KFLD                    KEY               8
012700961030     C     KTAB2         KLIST
012800961030     C                   KFLD                    CODUT
012900961030     C                   KFLD                    COD               2
013000941228     C*---------------------------------------------------------------*
013100930125     C*
013200920730     C* CARICO TABELLA FILIALI GESTITE £1
013300920730     C                   EXSR      CARTAB
013400161213     c* Se non è utente EDP o della 166 --> uscita
013500161213     c                   if        dutpou<>166 and %subst(knmus:1:3)<>'EDP'
013600161219     c                             and *in02  and %subst(knmus:1:3)<>'ISP'
013700161213     c                   eval      vidmsg=msg(16)
013800161213     c                   seton                                        289099
013900161213     c                   endif
014000920730     C*
014100920424     C     INIZIO        TAG
014200920730     C* PULIZIA DATI SUBFILE CONTROL
014300920901     C                   SETOFF                                       04
014400110908     C                   clear                   VIddchd
014500110908     C                   clear                   VIDdcha
014600110912     C                   clear                   VIDemail
014700161212     C                   clear                   VIDok
014800161212     C                   clear                   VIDeveok
014900161216     C                   clear                   VIDcons
015000920423     C*
015100920730     C                   Z-ADD     0             NRR               4 0
015200920730     C* CARICO SUBFILE
015300920831     C                   EXSR      CARICA
015400920730     C*
015500110908     C                   WRITE     LSW1Z01
015600920730     C     FORCT         TAG
015700110908     C                   EXFMT     LSW1DCT
015800941228     C*
015900961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
016000961017     C                   CLEAR                   VIDMSG
016100110912     C                   SETOFF                                       289052
016200110912     C                   SETOFF                                       444560
016300110912     C                   SETOFF                                       5051
016400920730     C*
016500110908     C** F3 - FINE LAVORO
016600161213     c   99
016700161213     Cor KC              GOTO      FINE
016800920730     C*
016900920730     C* CONTROLLO DATI SUBFILE
017000110908     C                   EXSR      CTRSFL
017100920831     C*
017200110908     C   90              GOTO      FORCT
017300920730     C*
017400920730     C* CONTROLLO DATI SUBFILE CONTROL
017500110908     C                   EXSR      CTRCTL
017600920730     C*
017700111116     c                   if        not *inkf and not *inkh or *in90
017800111116     C                   GOTO      FORCT
017900111116     c                   endif
018000920730     C*
018100920807     C* CARICO SCHIERE PER PASSAGGIO
018200920807     C                   MOVEA     *ZEROS        LNP
018300920807     C                   MOVEA     *ZEROS        CLI
018400930520     C                   MOVEA     *ZEROS        NRS
018500920901     C                   Z-ADD     1             X                 3 0
018600920807     C                   Z-ADD     1             NRR
018700920807     C*
018800111116     C                   SETON                                        36
018900110912     C     NRR           DOWLE     20
019000920807     C*
019100110908     C     NRR           CHAIN     LSW1DSF                            31
019200920831     C     *IN31         IFEQ      '0'
019300920831     C     VIDLNP        ANDNE     0
019400920807     C                   MOVEL     VIDLNP        LNP(X)
019500930520     C                   MOVEL     VIDNRS        NRS(X)
019600920807     C                   MOVEL     VIDCLI        CLI(X)
019700161221     C                   MOVEL     VIDUNIF       UNIF(X)
019800920901     C                   ADD       1             X
019900111116     c                   update    lsw1dsf
020000111116     C                   ENDif
020100920807     C*
020200920807     C                   ADD       1             NRR
020300111116     C                   ENDdo
020400111116     C                   SETOff                                       36
020500920831     C*
020600110912     C     COMdcha       IFEQ      0
020700110912     C                   Z-ADD     COMDchd       COMDcha
020800980415     C                   END
020900161209     C* ANOMALIE
021000161209     c                   if        *in01
021100161209     c
021200111116     c                   if        *inkh
021300111116     c                   eval      pargest='G'
021400111116     c                   else
021500111116     c                   eval      pargest='I'
021600111116     c                   endif
021700110913     C                   MOVEL     fnlsw1ds      KPJBU
021800110912     C                   MOVEL     'LSW2'        KCOAZ
021900111116     c* gestione anomalie
022000111116     c                   if        *inkh
022100111116     c                   call      'FNLSW2R'
022200111116     C                   PARM                    KPJBA
022300111116     c
022400111116     C                   WRITE     LSW1Z01
022500111116     C                   GOTO      FORCT
022600111116     c
022700111116     c                   else
022800161221     C                   CALL      'BCH10'
022900161221     c***                call      'FNLSW2R'
023000900702     C                   PARM                    KPJBA
023100111116     c                   endif
023200161209     C
023300161209     C                   ELSE
023400161209     C* KPI
023500161209     C                   MOVEL     fnlsw1ds      KPJBU
023600170105     C                   MOVEL     'LSW7'        KCOAZ
023700170105     C                   CALL      'BCH10'
023800170105     c***                call      'FNLSW7R'
023900161209     C                   PARM                    KPJBA
024000161209     C                   ENDIF
024100920522     C*
024200950411     C                   GOTO      INIZIO
024300000000     C     FINE          TAG
024400000000     C                   SETON                                        LR
024500920730     C*
024600920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
024700920730     C     CARTAB        BEGSR
024800941228     C***
024900941228     C* CARICO TABELLA FILIALI GESTITE £1
025000941228     C***
025100941228     C                   CLEAR                   DSUL06
025200941228     C                   MOVE      '£1'          D06COD
025300941228     C                   MOVEL     SIMFEL        D06KEY
025400941228     C                   MOVEL     DSUL06        KPJBU
025500941228     C*
025600950323     C                   CALL      'TRUL06R'
025700941228     C                   PARM                    KPJBA
025800941228     C                   MOVEL     KPJBU         DSUL06
025900941228     C                   MOVEA     LIN           L1
026000920730     C                   ENDSR
026100920831     C*
026200920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
026300920730     C*
026400920831     C     CARICA        BEGSR
026500920831     C* PULIZIA SUBFILE
026600920804     C                   SETON                                        37
026700110908     C                   WRITE     LSW1DCT
026800920804     C                   SETOFF                                       37
026900920730     C*
027000110908     C* imposto sfl vuoto
027100920730     C*
027200920730     C                   Z-ADD     0             VIDLNP
027300920730     C                   MOVEL     *BLANKS       DESLNP
027400060310     C                   movel     99            VIDNRS
027500920730     C                   Z-ADD     0             VIDCLI
027600920730     C                   MOVEL     *BLANKS       DESCLI
027700920730     C*
027800110912     C                   DO        20            NRR
027900110908     C                   WRITE     LSW1DSF
028000941228     C                   ENDDO
028100920730     C*
028200920831     C                   Z-ADD     1             REC
028300920730     C                   ENDSR
028400920730     C*
028500920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
028600110908     C     CTRSFL        BEGSR
028700110908     C                   SETOFF                                       90
028800920730     C                   Z-ADD     1             NRR
028900110912     C                   clear                   sellnp            1
029000920730     C*
029100110908     C                   READC     LSW1DSF                                33
029200920730     C     *IN33         DOWEQ     '0'
029300930520     C*
029400961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
029500961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
029600961017    2C     VIDKLP        IFGT      0
029700961017     C     VIDKLP        CHAIN     AZORG01L                           34
029800961017    3C     *IN34         IFEQ      *ON
029900110908     C                   SETON                                        284490
030000961017     C                   MOVEL     MSG(1)        VIDMSG
030100961017     C                   GOTO      ENDSFL
030200961017    3C                   ENDIF
030300110908     c
030400961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
030500961017    3C     VIDKSC        IFEQ      0
030600961017     C     DESCLI        ANDEQ     *BLANKS
030700110908     C                   SETON                                        284590
030800961017     C                   MOVEL     MSG(3)        VIDMSG
030900961017     C                   GOTO      ENDSFL
031000961017    3C                   ENDIF
031100961017     C*
031200961017   X2C                   ELSE
031300961017    3C     VIDKSC        IFGT      0
031400110908     C                   SETON                                        284490
031500961017     C                   MOVEL     MSG(1)        VIDMSG
031600961017     C                   GOTO      ENDSFL
031700961017    3C                   ENDIF
031800961017    2C                   ENDIF
031900961017     C*
032000920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
032100920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
032200961017     C     VIDKSC        IFEQ      0
032300920730     C     DESCLI        ANDNE     *BLANKS
032400981109     C                   CLEAR                   PARDIT
032500920730     C*
032600920730     C* PARSTA = 9  ESCLUDO ANNULLATI
032700110908     C                   Z-ADD     dutKCI        PARKCC            4 0
032800920730     C                   Z-ADD     9             PARSTA
032900920902     C                   MOVEL     *BLANKS       DES              48
033000920902     C                   MOVEL     DESCLI        DES
033100961017     C                   MOVEL     VIDKLP        PARFLR
033200981109     C     VIDKLP        CHAIN     AZORG01L                           31
033300981109     C  N31              MOVEL     ORGDIT        PARDIT
033400000907     C                   Z-ADD     1             PAXNUM
033500000907     C                   CALL      'XALFA3BR'
033600920730     C                   PARM                    PARDUT
033700920730     C                   PARM                    CODUT
033800920902     C                   PARM                    DES
033900961016     C                   PARM                    PARKCC
034000920730     C                   PARM                    PARSTA            1 0
034100961016     C                   PARM                    PARFLR           90
034200981109     C                   PARM                    PARDIT            3
034300000907     C                   PARM                    PAXNUM            2 0
034400000907     C                   PARM                    PAXKCM           80
034500000907     C                   PARM                    PAXKSM          140
034600000907     C                   PARM                    PAXKDM           60
034700920831     C     PARSTA        IFNE      -1
034800000907     C                   MOVEL     PAXKSM        VIDCLI
034900920902     C                   MOVEL     DES           DESCLI
035000920831     C                   END
035100920831     C*
035200110908     C                   SETON                                        9045
035300110908     c                   write     lsw1Z01
035400920730     C                   GOTO      ENDSFL
035500920730     C                   END
035600170104
035700170104     c* se impostata la "U" di unificante, il cod cliente obbigatorio
035800170104     c                   if        vidunif='U' and vidcli=0
035900170104     c                   eval      vidmsg=msg(19)
036000170104     C                   SETON                                        459028
036100170104     C                   GOTO      ENDSFL
036200170104     c                   endif
036300920730     C*
036400920901     C*** LINEA DI PARTENZA
036500961025    1C     VIDLNP        IFNE      0
036600920901     C*
036700920901     C* CONTROLLO SE LINEA VALIDA
036800920901     C     VIDLNP        LOOKUP    L1                                     31
036900061212     c
037000061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
037100061212     c*  per poterla accettare
037200161221     c*  oppure può gestire l'unificante
037300110912    2c                   if        not *in31
037400161212
037500161221   2ac                   if        *in01
037600170104     c
037700110912    3c                   if        vidnrs='00' or vidcli=0
037800110912     c                             or vidnrs='SI' or vidnrs='99'
037900170104     c                             or  vidunif='U'
038000170104     c
038100170104    4c                   if        vidcli>0
038200161221     c                   clear                   tntaa1ds
038300161221     c                   eval      itaa1pru=knmus
038400161221     c                   eval      itaa1ksc=vidcli
038500161221     c                   movel     tntaa1ds      kpjbu
038600161221     c                   call      'TNTAA1C'
038700161221     c                   parm                    kpjba
038800161221     c                   movel     kpjbu         tntaa1ds
038900161221     c
039000161221     c* verifico
039100161221    5c                   if        OTAA1ERR='S' or OTAA1FABI='N'
039200161221     c                   eval      vidmsg=msg(18)
039300170104     C                   SETON                                        459028
039400061212     C                   GOTO      ENDSFL
039500161221    5c                   endif
039600170104
039700161221   x4c                   else
039800161221     c                   eval      vidmsg=msg(6)
039900161221     C                   SETON                                        609028
040000161221     C                   GOTO      ENDSFL
040100161221    4c                   endif
040200161212     c
040300161212    3c                   endif
040400161221   2ac                   endif
040500110912    2c                   endif
040600161221     c* Per ora per KPI  nessun controllo sulla lnp
040700920901     C*
040800920901     C* DECODIFICO LINEA DI PARTENZA
040900920901     C     VIDLNP        CHAIN     AZORG                              30
041000920901     C  N30              MOVEL     ORGDES        DESLNP
041100920901     C   30              MOVEL     *BLANKS       DESLNP
041200061212     c
041300110912    2c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
041400061212     c                             or orgfva<>' '
041500110912     c                   eval      vidmsg=msg(6)
041600110912     C                   SETON                                        609028
041700110912     C                   GOTO      ENDSFL
041800061212     C                   GOTO      ENDSFL
041900110912    2c                   endif
042000110912     c                   eval      Sellnp='S'
042100920901     C*
042200961025   X1C                   ELSE
042300920902     C* LINEA DI PARTENZA = 0
042400920902     C                   MOVEL     *BLANKS       DESLNP
042500961025     C*
042600920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
042700950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
042800920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
042900110912    2c                   if        vidnrs<>'99' or vidcli>0
043000110912     c                   eval      vidmsg=msg(10)
043100110912     C                   SETON                                        609028
043200110912     C                   GOTO      ENDSFL
043300110912    2c                   endif
043400961025     C**
043500961025    1C                   END
043600170104
043700060310     C*** Numero serie
043800060310     C* ESEGUO TESTN
043900110912    1c                   if        vidnrs<>'SI'
044000060310     C                   TESTN                   VIDnrs               30
044100060310     C                   MOVE      VIDnrs        W001A             1
044200060310    2C     *IN30         IFEQ      *OFF
044300060310     C     W001A         ORLT      '0'
044400110908     C                   SETON                                        599028
044500110912     C                   MOVEL     MSG(8)        VIDMSG
044600060310     C                   GOTO      ENDSFL
044700060310    2C                   ENDIF
044800110912    1C                   ENDIF
044900170104
045000170104    1c                   if        vidunif='U' and (vidnrs='99' or
045100170104     c                             vidnrs='SI')
045200170104     c                   eval      vidmsg=msg(20)
045300170104     C                   SETON                                        599028
045400170104     C                   GOTO      ENDSFL
045500170104    1c                   endif
045600920730     C*
045700961025    1C     VIDCLI        IFNE      0
045800930520     C*** NUMERO SERIE
045900060310    2C     VIDNRS        IFNE      '99'
046000161221     c     vidunif       andeq     ' '
046100060605     c                   clear                   ds3c
046200930521     C                   MOVEL     '3C'          COD
046300930521     C                   MOVEL     *BLANKS       KEY
046400930521     C                   MOVEL     VIDCLI        KEY
046500930521     C     KTAB          CHAIN     TABEL                              30
046600930521     C  N30              MOVEL     TBLUNI        DS3C
046700060605     c  N30              movel     §3cnrs        alfa3cnrs
046800930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
046900970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
047000980805     C     *IN30         IFEQ      *ON
047100161221
047200980805     C     §3COKD        OREQ      ' '
047300161221
047400060310    3C     alfa3cnrs     ORNE      VIDNRS
047500161221     c     vidnrs        andne     '00'
047600161221
047700110912     C                   SETON                                        599028
047800110912     C                   MOVEL     MSG(11)       VIDMSG
047900930521     C                   GOTO      ENDSFL
048000961025    3C                   END
048100961025    2C                   END
048200161221     c
048300161221     c* per l'unificante devo loopare su tutti i codici
048400161221    2C     VIDNRS        IFNE      '99'
048500161221     c     vidunif       andeq     'U'
048600161221
048700161221      /free
048800161221           clear tibs10ds ;
048900170112           d10tle='ST'  ;
049000161221           d10paf='F'    ;     // verifico se è padre
049100161221           d10cod=vidcli        ;
049200161221           TIBS10R (tibs10ds)   ;
049300161221  3        if d10err<>*blanks   ;
049400161221           // Se errore verifico se è figlio
049500161221              clear tibs10ds ;
049600170112              d10tle='ST'        ;
049700161221              d10paf='P'    ;
049800161221              d10cod=vidcli        ;
049900161221              TIBS10R (tibs10ds)   ;
050000161221  3        endif;
050100161221
050200161221  3        if d10err<>*blanks   ;
050300161221             // Imposto nella skiera e come padre solo se stesso
050400161221             skc(1)= '0000'+%editc(vidcli:'X');
050500161221             d10cop=d10cod      ;
050600161221  3        endif                ;
050700161221      /end-free
050800161221
050900161221     c                   z-add     1             ww                4 0
051000161221     c                   clear                   wok
051100161221    3c                   dow       skc(ww) >*zeros
051200161221     c                   clear                   ds3c
051300161221     C                   MOVEL     '3C'          COD
051400161221     C                   MOVEL     *BLANKS       KEY
051500161221     C                   MOVE      skc(ww)       w007a             7
051600161221     C                   MOVEL     w007a         KEY
051700161221     C     KTAB          CHAIN     TABEL                              30
051800161221     C  N30              MOVEL     TBLUNI        DS3C
051900161221     c  N30              movel     §3cnrs        alfa3cnrs
052000161221     c*
052100161221     c* se almeno uno OK --> va bene
052200161221     c                   if        not *in30 and §3COKD='*'
052300161221     c                   if        vidnrs='00' or
052400161221     c                             alfa3cnrs=vidnrs
052500161221     c                   eval      wok='S'
052600161221     c                   leave
052700161221     c                   endif
052800161221     c                   endif
052900161221     c
053000161221     c                   add       1             ww
053100161221    3c                   enddo
053200161221     c
053300161221     C* SE LA SERIE NON CORRISPONDE A nessun cliente della famiglia
053400161221     c* errore
053500161221    3C                   if        wok=' '
053600161221     C                   SETON                                        599028
053700161221     C                   MOVEL     MSG(11)       VIDMSG
053800161221     C                   GOTO      ENDSFL
053900161221    3C                   END
054000161221    2C                   END
054100930521     C*
054200961017     C****  CODICE CLIENTE  ****
054300920730     C                   MOVEL     VIDCLI        KSC
054400920901     C     KACO          CHAIN     CNACO000                           30
054500961025    2C  N30ACOFLG        IFNE      ' '
054600920730     C* CODICE CLIENTE ANNULLATO
054700110912     C                   SETON                                        284590
054800961017     C                   MOVEL     MSG(2)        VIDMSG
054900920730     C                   GOTO      ENDSFL
055000920730     C*
055100961025   X2C                   ELSE
055200920730     C* DECODIFICO CODICE CLIENTE
055300920730     C                   MOVEL     ACORAG        DESCLI
055400961025    2C                   END
055500920730     C*
055600920730     C* CODICE CLIENTE INESISTENTE
055700961025    2C     *IN30         IFEQ      *ON
055800110912     C                   SETON                                        284590
055900961017     C                   MOVEL     MSG(3)        VIDMSG
056000961017     C                   GOTO      ENDSFL
056100961025    2C                   ENDIF
056200920730     C*
056300920730     C* CODICE CLIENTE BLOCCATO
056400130320    2C***  ACOABL        IFEQ      '8'
056500130320    2C***  ACOABL        OREQ      '*'
056600130320     c                   if        acoabl<>*blanks
056700110912     C                   SETON                                        284590
056800961017     C                   MOVEL     MSG(4)        VIDMSG
056900920730     C                   GOTO      ENDSFL
057000961025    2C                   END
057100961025     C*
057200161212     c* Se si tratta della elaborazione kpi il codice deve essere presente
057300161212     c*  nel file fiabk00f
057400161212     c                   if        *in02
057500161212     c     acoksc        chain     fiabk02l
057600161212     c                   if        not %found(fiabk02l)
057700161212     C                   SETON                                        284590
057800161212     C                   MOVEL     MSG(14)       VIDMSG
057900161212     C                   GOTO      ENDSFL
058000161212    2C                   END
058100161212     c                   endif
058200961025     C**
058300961025    1C                   END
058400161212     c* Se si tratta della elaborazione kpi codice obbligatorio
058500161212     c                   if        *in02 and vidcli=0
058600161212     C                   SETON                                        284590
058700161212     C                   MOVEL     MSG(15)       VIDMSG
058800161212     C                   GOTO      ENDSFL
058900161212     c                   endif
059000161212     c*
059100920730     C*
059200920730     C     ENDSFL        TAG
059300110912     C   90              Z-ADD     NRR           REC
059400920730     C*
059500920730     C* 36 - SFLNXTCHG
059600920730     C                   SETON                                        36
059700110908     C                   UPDATE    LSW1DSF
059800920730     C                   SETOFF                                       36
059900920730     C*
060000110912     C   90              SETON                                        33
060100110912     C  N90              READC     LSW1DSF                                33
060200920730     C                   END
060300920730     C*
060400110912     c* se non ho selezionato nulla --> errore
060500110912     c  N90              if        sellnp=' '
060600110912     C                   Z-ADD     1             REC
060700110912     C                   Z-ADD     1             nrr
060800110912     C*
060900110912     C* 36 - SFLNXTCHG
061000110912     C     nrr           chain     LSW1DSF                            33
061100110912     C                   SETON                                        36
061200110912     C                   SETON                                        286090
061300110912     C                   MOVEL     MSG(12)       VIDMSG
061400110912     C                   UPDATE    LSW1DSF
061500110912     C                   SETOFF                                       36
061600110912     c                   endif
061700110912     c
061800920730     C                   ENDSR
061900920730     C*
062000920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
062100110908     C     CtrCTL        BEGSR
062200920731     C*
062300110908     c                   clear                   comdchd
062400110908     c                   clear                   comdcha
062500110908     c
062600110908     C*** DATA chiusura dal <=data chiusura al
062700110908     C     VIDdchd       IFgt      0
062800110912     C                   MOVE      VIDDchd       G02DAT
062900920903     C                   MOVEL     *BLANKS       G02ERR
063000941228     C                   CALL      'XSRDA8'
063100920903     C                   PARM                    WLBDAT
063200920903     C     G02ERR        IFEQ      '1'
063300110908     C                   SETON                                        509028
063400110908     c                   eval      vidmsg=msg(5)
063500920903     C                   GOTO      ENDCTL
063600920903     C                   END
063700110908     c
063800110908     C                   Z-ADD     G02INV        COMDchd
063900110908     c
064000950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
064100110908     C                   Z-ADD     G02DAT        VIDdchd
064200111003     c* no maggiore di udate
064300111003     c                   if        comdchd>dateu
064400111003     C                   SETON                                        509028
064500111003     c                   eval      vidmsg=msg(13)
064600111003     C                   GOTO      ENDCTL
064700111003     C                   END
064800110908     C                   endif
064900110908     c
065000110908     C     VIDdcha       IFgt      0
065100110912     C                   MOVE      VIDDcha       G02DAT
065200110908     C                   MOVEL     *BLANKS       G02ERR
065300110908     C                   CALL      'XSRDA8'
065400110908     C                   PARM                    WLBDAT
065500110908     C     G02ERR        IFEQ      '1'
065600110908     C                   SETON                                        519028
065700110908     c                   eval      vidmsg=msg(5)
065800110908     C                   GOTO      ENDCTL
065900110908     C                   END
066000110908     c
066100110908     C                   Z-ADD     G02INV        COMDcha
066200110908     c
066300110908     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
066400110908     C                   Z-ADD     G02DAT        VIDdcha
066500111003     c* no maggiore di udate
066600111003     c                   if        comdcha>dateu
066700111003     C                   SETON                                        519028
066800111003     c                   eval      vidmsg=msg(13)
066900111003     C                   GOTO      ENDCTL
067000111003     C                   END
067100110908     c
067200110908     c                   if        comdcha<comdchd
067300110908     C                   SETON                                        519028
067400110912     c                   eval      vidmsg=msg(9)
067500110908     C                   GOTO      ENDCTL
067600110908     C                   END
067700110929     C                   endif
067800161213     c
067900161213     c* Per KPI le date sono oblogatorie
068000161213     c                   if        *in02 and VIDdchd=0
068100161213     C                   SETON                                        509028
068200161213     c                   eval      vidmsg=msg(17)
068300161213     C                   GOTO      ENDCTL
068400161213     C                   END
068500110908     c
068600110929     c* Obbligatorio inserire indirizzo e-mail
068700161209     c   01
068800161209     can kf              if        videmail=*blanks
068900110912     C                   SETON                                        529028
069000110912     c                   eval      vidmsg=msg(7)
069100110912     C                   GOTO      ENDCTL
069200111228     c                   else
069300111228     c* controllo formale
069400111228     c                   clear                   dsemail
069500120305     c                   eval      §EMLINDI=%trim(videmail)+'@brt.it'
069600111228     c                   call      'XEMAIL'
069700111228     c                   parm                    dsemail
069800111228     c                   if        §EMLERRO<>*blanks and
069900111228     c                             §EMLERRO<> '0'
070000111228     C                   SETON                                        529028
070100111228     c                   eval      vidmsg=§EMLmsgO
070200111228     C                   GOTO      ENDCTL
070300110912     c                   endif
070400111228     c                   endif
070500110912     c
070600920731     C     ENDCTL        ENDSR
070700961017**  MSG
070800961017Immettere linea di partenza valida                                            1
070900961017Codice Cliente annullato                                                      2
071000961017Codice Cliente errato                                                         3
071100961017Codice Cliente bloccato                                                       4
071200110908Data errata                                                                   5
071300110908Linea partenza errata o non gestibile                                         6
071400110912Inserire indirizzo e-mail BRT                                                 7
071500110912Serie errata                                                                  8
071600110912Data  AL  deve essere maggiore della data  DAL                                9
071700111003Se immessa SERIE o CLIENTE immettere anche la LINEA di PARTENZA               10
071800111003La serie non è quella assegnata al cliente                                    11
071900110912Effettuare almeno una selezione
072000111003La data non può essere maggiore della data del giorno                         13
072100161212Cliente non abilitato alla elaborazione  in oggetto                           14
072200161212Cliente obbligatorio per l'elaborazine del KPI                                15
072300161213Programma in TEST soltanto alla filiale 166. Enter esce                       16
072400161213Immettere la data spedizione                                                  17
072500161221Cliente non in gestione non utilizzabile per la linea indicata                18
072600170104Se immessa la "U" di unificante, indicare anche in codice                     19
072700170104Per cliente Unificante serie"99"non ammessa:indicare OGNI serie a cui abilit. 20
