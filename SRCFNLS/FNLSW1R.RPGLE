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
013700171114     c                             and dutpou<>050
013800161213     c                   eval      vidmsg=msg(16)
013900161213     c                   seton                                        289099
014000161213     c                   endif
014100920730     C*
014200920424     C     INIZIO        TAG
014300920730     C* PULIZIA DATI SUBFILE CONTROL
014400920901     C                   SETOFF                                       04
014500110908     C                   clear                   VIddchd
014600110908     C                   clear                   VIDdcha
014700110912     C                   clear                   VIDemail
014800161212     C                   clear                   VIDok
014900161212     C                   clear                   VIDeveok
015000161216     C                   clear                   VIDcons
015100920423     C*
015200920730     C                   Z-ADD     0             NRR               4 0
015300920730     C* CARICO SUBFILE
015400920831     C                   EXSR      CARICA
015500920730     C*
015600110908     C                   WRITE     LSW1Z01
015700920730     C     FORCT         TAG
015800110908     C                   EXFMT     LSW1DCT
015900941228     C*
016000961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
016100961017     C                   CLEAR                   VIDMSG
016200110912     C                   SETOFF                                       289052
016300110912     C                   SETOFF                                       444560
016400110912     C                   SETOFF                                       5051
016500920730     C*
016600110908     C** F3 - FINE LAVORO
016700161213     c   99
016800161213     Cor KC              GOTO      FINE
016900920730     C*
017000920730     C* CONTROLLO DATI SUBFILE
017100110908     C                   EXSR      CTRSFL
017200920831     C*
017300110908     C   90              GOTO      FORCT
017400920730     C*
017500920730     C* CONTROLLO DATI SUBFILE CONTROL
017600110908     C                   EXSR      CTRCTL
017700920730     C*
017800111116     c                   if        not *inkf and not *inkh or *in90
017900111116     C                   GOTO      FORCT
018000111116     c                   endif
018100920730     C*
018200920807     C* CARICO SCHIERE PER PASSAGGIO
018300920807     C                   MOVEA     *ZEROS        LNP
018400920807     C                   MOVEA     *ZEROS        CLI
018500930520     C                   MOVEA     *ZEROS        NRS
018600920901     C                   Z-ADD     1             X                 3 0
018700920807     C                   Z-ADD     1             NRR
018800920807     C*
018900111116     C                   SETON                                        36
019000110912     C     NRR           DOWLE     20
019100920807     C*
019200110908     C     NRR           CHAIN     LSW1DSF                            31
019300920831     C     *IN31         IFEQ      '0'
019400920831     C     VIDLNP        ANDNE     0
019500920807     C                   MOVEL     VIDLNP        LNP(X)
019600930520     C                   MOVEL     VIDNRS        NRS(X)
019700920807     C                   MOVEL     VIDCLI        CLI(X)
019800161221     C                   MOVEL     VIDUNIF       UNIF(X)
019900920901     C                   ADD       1             X
020000111116     c                   update    lsw1dsf
020100111116     C                   ENDif
020200920807     C*
020300920807     C                   ADD       1             NRR
020400111116     C                   ENDdo
020500111116     C                   SETOff                                       36
020600920831     C*
020700110912     C     COMdcha       IFEQ      0
020800110912     C                   Z-ADD     COMDchd       COMDcha
020900980415     C                   END
021000161209     C* ANOMALIE
021100161209     c                   if        *in01
021200161209     c
021300111116     c                   if        *inkh
021400111116     c                   eval      pargest='G'
021500111116     c                   else
021600111116     c                   eval      pargest='I'
021700111116     c                   endif
021800110913     C                   MOVEL     fnlsw1ds      KPJBU
021900110912     C                   MOVEL     'LSW2'        KCOAZ
022000111116     c* gestione anomalie
022100111116     c                   if        *inkh
022200111116     c                   call      'FNLSW2R'
022300111116     C                   PARM                    KPJBA
022400111116     c
022500111116     C                   WRITE     LSW1Z01
022600111116     C                   GOTO      FORCT
022700111116     c
022800111116     c                   else
022900161221     C                   CALL      'BCH10'
023000161221     c***                call      'FNLSW2R'
023100900702     C                   PARM                    KPJBA
023200111116     c                   endif
023300161209     C
023400161209     C                   ELSE
023500161209     C* KPI
023600161209     C                   MOVEL     fnlsw1ds      KPJBU
023700170105     C                   MOVEL     'LSW7'        KCOAZ
023800170105     C                   CALL      'BCH10'
023900170105     c***                call      'FNLSW7R'
024000161209     C                   PARM                    KPJBA
024100161209     C                   ENDIF
024200920522     C*
024300950411     C                   GOTO      INIZIO
024400000000     C     FINE          TAG
024500000000     C                   SETON                                        LR
024600920730     C*
024700920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
024800920730     C     CARTAB        BEGSR
024900941228     C***
025000941228     C* CARICO TABELLA FILIALI GESTITE £1
025100941228     C***
025200941228     C                   CLEAR                   DSUL06
025300941228     C                   MOVE      '£1'          D06COD
025400941228     C                   MOVEL     SIMFEL        D06KEY
025500941228     C                   MOVEL     DSUL06        KPJBU
025600941228     C*
025700950323     C                   CALL      'TRUL06R'
025800941228     C                   PARM                    KPJBA
025900941228     C                   MOVEL     KPJBU         DSUL06
026000941228     C                   MOVEA     LIN           L1
026100920730     C                   ENDSR
026200920831     C*
026300920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
026400920730     C*
026500920831     C     CARICA        BEGSR
026600920831     C* PULIZIA SUBFILE
026700920804     C                   SETON                                        37
026800110908     C                   WRITE     LSW1DCT
026900920804     C                   SETOFF                                       37
027000920730     C*
027100110908     C* imposto sfl vuoto
027200920730     C*
027300920730     C                   Z-ADD     0             VIDLNP
027400920730     C                   MOVEL     *BLANKS       DESLNP
027500060310     C                   movel     99            VIDNRS
027600920730     C                   Z-ADD     0             VIDCLI
027700920730     C                   MOVEL     *BLANKS       DESCLI
027800920730     C*
027900110912     C                   DO        20            NRR
028000110908     C                   WRITE     LSW1DSF
028100941228     C                   ENDDO
028200920730     C*
028300920831     C                   Z-ADD     1             REC
028400920730     C                   ENDSR
028500920730     C*
028600920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
028700110908     C     CTRSFL        BEGSR
028800110908     C                   SETOFF                                       90
028900920730     C                   Z-ADD     1             NRR
029000110912     C                   clear                   sellnp            1
029100920730     C*
029200110908     C                   READC     LSW1DSF                                33
029300920730     C     *IN33         DOWEQ     '0'
029400930520     C*
029500961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
029600961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
029700961017    2C     VIDKLP        IFGT      0
029800961017     C     VIDKLP        CHAIN     AZORG01L                           34
029900961017    3C     *IN34         IFEQ      *ON
030000110908     C                   SETON                                        284490
030100961017     C                   MOVEL     MSG(1)        VIDMSG
030200961017     C                   GOTO      ENDSFL
030300961017    3C                   ENDIF
030400110908     c
030500961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
030600961017    3C     VIDKSC        IFEQ      0
030700961017     C     DESCLI        ANDEQ     *BLANKS
030800110908     C                   SETON                                        284590
030900961017     C                   MOVEL     MSG(3)        VIDMSG
031000961017     C                   GOTO      ENDSFL
031100961017    3C                   ENDIF
031200961017     C*
031300961017   X2C                   ELSE
031400961017    3C     VIDKSC        IFGT      0
031500110908     C                   SETON                                        284490
031600961017     C                   MOVEL     MSG(1)        VIDMSG
031700961017     C                   GOTO      ENDSFL
031800961017    3C                   ENDIF
031900961017    2C                   ENDIF
032000961017     C*
032100920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
032200920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
032300961017     C     VIDKSC        IFEQ      0
032400920730     C     DESCLI        ANDNE     *BLANKS
032500981109     C                   CLEAR                   PARDIT
032600920730     C*
032700920730     C* PARSTA = 9  ESCLUDO ANNULLATI
032800110908     C                   Z-ADD     dutKCI        PARKCC            4 0
032900920730     C                   Z-ADD     9             PARSTA
033000920902     C                   MOVEL     *BLANKS       DES              48
033100920902     C                   MOVEL     DESCLI        DES
033200961017     C                   MOVEL     VIDKLP        PARFLR
033300981109     C     VIDKLP        CHAIN     AZORG01L                           31
033400981109     C  N31              MOVEL     ORGDIT        PARDIT
033500000907     C                   Z-ADD     1             PAXNUM
033600000907     C                   CALL      'XALFA3BR'
033700920730     C                   PARM                    PARDUT
033800920730     C                   PARM                    CODUT
033900920902     C                   PARM                    DES
034000961016     C                   PARM                    PARKCC
034100920730     C                   PARM                    PARSTA            1 0
034200961016     C                   PARM                    PARFLR           90
034300981109     C                   PARM                    PARDIT            3
034400000907     C                   PARM                    PAXNUM            2 0
034500000907     C                   PARM                    PAXKCM           80
034600000907     C                   PARM                    PAXKSM          140
034700000907     C                   PARM                    PAXKDM           60
034800920831     C     PARSTA        IFNE      -1
034900000907     C                   MOVEL     PAXKSM        VIDCLI
035000920902     C                   MOVEL     DES           DESCLI
035100920831     C                   END
035200920831     C*
035300110908     C                   SETON                                        9045
035400110908     c                   write     lsw1Z01
035500920730     C                   GOTO      ENDSFL
035600920730     C                   END
035700170104
035800170104     c* se impostata la "U" di unificante, il cod cliente obbigatorio
035900170104     c                   if        vidunif='U' and vidcli=0
036000170104     c                   eval      vidmsg=msg(19)
036100170104     C                   SETON                                        459028
036200170104     C                   GOTO      ENDSFL
036300170104     c                   endif
036400920730     C*
036500920901     C*** LINEA DI PARTENZA
036600961025    1C     VIDLNP        IFNE      0
036700920901     C*
036800920901     C* CONTROLLO SE LINEA VALIDA
036900920901     C     VIDLNP        LOOKUP    L1                                     31
037000061212     c
037100061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
037200061212     c*  per poterla accettare
037300161221     c*  oppure può gestire l'unificante
037400110912    2c                   if        not *in31
037500161212
037600161221   2ac                   if        *in01
037700170104     c
037800110912    3c                   if        vidnrs='00' or vidcli=0
037900110912     c                             or vidnrs='SI' or vidnrs='99'
038000170104     c                             or  vidunif='U'
038100170104     c
038200170104    4c                   if        vidcli>0
038300161221     c                   clear                   tntaa1ds
038400161221     c                   eval      itaa1pru=knmus
038500161221     c                   eval      itaa1ksc=vidcli
038600161221     c                   movel     tntaa1ds      kpjbu
038700161221     c                   call      'TNTAA1C'
038800161221     c                   parm                    kpjba
038900161221     c                   movel     kpjbu         tntaa1ds
039000161221     c
039100161221     c* verifico
039200161221    5c                   if        OTAA1ERR='S' or OTAA1FABI='N'
039300161221     c                   eval      vidmsg=msg(18)
039400170104     C                   SETON                                        459028
039500061212     C                   GOTO      ENDSFL
039600161221    5c                   endif
039700170104
039800161221   x4c                   else
039900161221     c                   eval      vidmsg=msg(6)
040000161221     C                   SETON                                        609028
040100161221     C                   GOTO      ENDSFL
040200161221    4c                   endif
040300161212     c
040400161212    3c                   endif
040500161221   2ac                   endif
040600110912    2c                   endif
040700161221     c* Per ora per KPI  nessun controllo sulla lnp
040800920901     C*
040900920901     C* DECODIFICO LINEA DI PARTENZA
041000920901     C     VIDLNP        CHAIN     AZORG                              30
041100920901     C  N30              MOVEL     ORGDES        DESLNP
041200920901     C   30              MOVEL     *BLANKS       DESLNP
041300061212     c
041400110912    2c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
041500061212     c                             or orgfva<>' '
041600110912     c                   eval      vidmsg=msg(6)
041700110912     C                   SETON                                        609028
041800110912     C                   GOTO      ENDSFL
041900061212     C                   GOTO      ENDSFL
042000110912    2c                   endif
042100110912     c                   eval      Sellnp='S'
042200920901     C*
042300961025   X1C                   ELSE
042400920902     C* LINEA DI PARTENZA = 0
042500920902     C                   MOVEL     *BLANKS       DESLNP
042600961025     C*
042700920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
042800950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
042900920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
043000110912    2c                   if        vidnrs<>'99' or vidcli>0
043100110912     c                   eval      vidmsg=msg(10)
043200110912     C                   SETON                                        609028
043300110912     C                   GOTO      ENDSFL
043400110912    2c                   endif
043500961025     C**
043600961025    1C                   END
043700170104
043800060310     C*** Numero serie
043900060310     C* ESEGUO TESTN
044000110912    1c                   if        vidnrs<>'SI'
044100060310     C                   TESTN                   VIDnrs               30
044200060310     C                   MOVE      VIDnrs        W001A             1
044300060310    2C     *IN30         IFEQ      *OFF
044400060310     C     W001A         ORLT      '0'
044500110908     C                   SETON                                        599028
044600110912     C                   MOVEL     MSG(8)        VIDMSG
044700060310     C                   GOTO      ENDSFL
044800060310    2C                   ENDIF
044900110912    1C                   ENDIF
045000170104
045100170104    1c                   if        vidunif='U' and (vidnrs='99' or
045200170104     c                             vidnrs='SI')
045300170104     c                   eval      vidmsg=msg(20)
045400170104     C                   SETON                                        599028
045500170104     C                   GOTO      ENDSFL
045600170104    1c                   endif
045700920730     C*
045800961025    1C     VIDCLI        IFNE      0
045900930520     C*** NUMERO SERIE
046000060310    2C     VIDNRS        IFNE      '99'
046100161221     c     vidunif       andeq     ' '
046200060605     c                   clear                   ds3c
046300930521     C                   MOVEL     '3C'          COD
046400930521     C                   MOVEL     *BLANKS       KEY
046500930521     C                   MOVEL     VIDCLI        KEY
046600930521     C     KTAB          CHAIN     TABEL                              30
046700930521     C  N30              MOVEL     TBLUNI        DS3C
046800060605     c  N30              movel     §3cnrs        alfa3cnrs
046900930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
047000970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
047100980805     C     *IN30         IFEQ      *ON
047200161221
047300980805     C     §3COKD        OREQ      ' '
047400161221
047500060310    3C     alfa3cnrs     ORNE      VIDNRS
047600161221     c     vidnrs        andne     '00'
047700161221
047800110912     C                   SETON                                        599028
047900110912     C                   MOVEL     MSG(11)       VIDMSG
048000930521     C                   GOTO      ENDSFL
048100961025    3C                   END
048200961025    2C                   END
048300161221     c
048400161221     c* per l'unificante devo loopare su tutti i codici
048500161221    2C     VIDNRS        IFNE      '99'
048600161221     c     vidunif       andeq     'U'
048700161221
048800161221      /free
048900161221           clear tibs10ds ;
049000170112           d10tle='ST'  ;
049100161221           d10paf='F'    ;     // verifico se è padre
049200161221           d10cod=vidcli        ;
049300161221           TIBS10R (tibs10ds)   ;
049400161221  3        if d10err<>*blanks   ;
049500161221           // Se errore verifico se è figlio
049600161221              clear tibs10ds ;
049700170112              d10tle='ST'        ;
049800161221              d10paf='P'    ;
049900161221              d10cod=vidcli        ;
050000161221              TIBS10R (tibs10ds)   ;
050100161221  3        endif;
050200161221
050300161221  3        if d10err<>*blanks   ;
050400161221             // Imposto nella skiera e come padre solo se stesso
050500161221             skc(1)= '0000'+%editc(vidcli:'X');
050600161221             d10cop=d10cod      ;
050700161221  3        endif                ;
050800161221      /end-free
050900161221
051000161221     c                   z-add     1             ww                4 0
051100161221     c                   clear                   wok
051200161221    3c                   dow       skc(ww) >*zeros
051300161221     c                   clear                   ds3c
051400161221     C                   MOVEL     '3C'          COD
051500161221     C                   MOVEL     *BLANKS       KEY
051600161221     C                   MOVE      skc(ww)       w007a             7
051700161221     C                   MOVEL     w007a         KEY
051800161221     C     KTAB          CHAIN     TABEL                              30
051900161221     C  N30              MOVEL     TBLUNI        DS3C
052000161221     c  N30              movel     §3cnrs        alfa3cnrs
052100161221     c*
052200161221     c* se almeno uno OK --> va bene
052300161221     c                   if        not *in30 and §3COKD='*'
052400161221     c                   if        vidnrs='00' or
052500161221     c                             alfa3cnrs=vidnrs
052600161221     c                   eval      wok='S'
052700161221     c                   leave
052800161221     c                   endif
052900161221     c                   endif
053000161221     c
053100161221     c                   add       1             ww
053200161221    3c                   enddo
053300161221     c
053400161221     C* SE LA SERIE NON CORRISPONDE A nessun cliente della famiglia
053500161221     c* errore
053600161221    3C                   if        wok=' '
053700161221     C                   SETON                                        599028
053800161221     C                   MOVEL     MSG(11)       VIDMSG
053900161221     C                   GOTO      ENDSFL
054000161221    3C                   END
054100161221    2C                   END
054200930521     C*
054300961017     C****  CODICE CLIENTE  ****
054400920730     C                   MOVEL     VIDCLI        KSC
054500920901     C     KACO          CHAIN     CNACO000                           30
054600961025    2C  N30ACOFLG        IFNE      ' '
054700920730     C* CODICE CLIENTE ANNULLATO
054800110912     C                   SETON                                        284590
054900961017     C                   MOVEL     MSG(2)        VIDMSG
055000920730     C                   GOTO      ENDSFL
055100920730     C*
055200961025   X2C                   ELSE
055300920730     C* DECODIFICO CODICE CLIENTE
055400920730     C                   MOVEL     ACORAG        DESCLI
055500961025    2C                   END
055600920730     C*
055700920730     C* CODICE CLIENTE INESISTENTE
055800961025    2C     *IN30         IFEQ      *ON
055900110912     C                   SETON                                        284590
056000961017     C                   MOVEL     MSG(3)        VIDMSG
056100961017     C                   GOTO      ENDSFL
056200961025    2C                   ENDIF
056300920730     C*
056400920730     C* CODICE CLIENTE BLOCCATO
056500130320    2C***  ACOABL        IFEQ      '8'
056600130320    2C***  ACOABL        OREQ      '*'
056700170911     c***                if        acoabl<>*blanks
056800170911     C***                SETON                                        284590
056900170911     C***                MOVEL     MSG(4)        VIDMSG
057000170911     C***                GOTO      ENDSFL
057100170911    2C***                END
057200961025     C*
057300161212     c* Se si tratta della elaborazione kpi il codice deve essere presente
057400161212     c*  nel file fiabk00f
057500161212     c                   if        *in02
057600161212     c     acoksc        chain     fiabk02l
057700161212     c                   if        not %found(fiabk02l)
057800161212     C                   SETON                                        284590
057900161212     C                   MOVEL     MSG(14)       VIDMSG
058000161212     C                   GOTO      ENDSFL
058100161212    2C                   END
058200161212     c                   endif
058300961025     C**
058400961025    1C                   END
058500161212     c* Se si tratta della elaborazione kpi codice obbligatorio
058600161212     c                   if        *in02 and vidcli=0
058700161212     C                   SETON                                        284590
058800161212     C                   MOVEL     MSG(15)       VIDMSG
058900161212     C                   GOTO      ENDSFL
059000161212     c                   endif
059100161212     c*
059200920730     C*
059300920730     C     ENDSFL        TAG
059400110912     C   90              Z-ADD     NRR           REC
059500920730     C*
059600920730     C* 36 - SFLNXTCHG
059700920730     C                   SETON                                        36
059800110908     C                   UPDATE    LSW1DSF
059900920730     C                   SETOFF                                       36
060000920730     C*
060100110912     C   90              SETON                                        33
060200110912     C  N90              READC     LSW1DSF                                33
060300920730     C                   END
060400920730     C*
060500110912     c* se non ho selezionato nulla --> errore
060600110912     c  N90              if        sellnp=' '
060700110912     C                   Z-ADD     1             REC
060800110912     C                   Z-ADD     1             nrr
060900110912     C*
061000110912     C* 36 - SFLNXTCHG
061100110912     C     nrr           chain     LSW1DSF                            33
061200110912     C                   SETON                                        36
061300110912     C                   SETON                                        286090
061400110912     C                   MOVEL     MSG(12)       VIDMSG
061500110912     C                   UPDATE    LSW1DSF
061600110912     C                   SETOFF                                       36
061700110912     c                   endif
061800110912     c
061900920730     C                   ENDSR
062000920730     C*
062100920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
062200110908     C     CtrCTL        BEGSR
062300920731     C*
062400110908     c                   clear                   comdchd
062500110908     c                   clear                   comdcha
062600110908     c
062700110908     C*** DATA chiusura dal <=data chiusura al
062800110908     C     VIDdchd       IFgt      0
062900110912     C                   MOVE      VIDDchd       G02DAT
063000920903     C                   MOVEL     *BLANKS       G02ERR
063100941228     C                   CALL      'XSRDA8'
063200920903     C                   PARM                    WLBDAT
063300920903     C     G02ERR        IFEQ      '1'
063400110908     C                   SETON                                        509028
063500110908     c                   eval      vidmsg=msg(5)
063600920903     C                   GOTO      ENDCTL
063700920903     C                   END
063800110908     c
063900110908     C                   Z-ADD     G02INV        COMDchd
064000110908     c
064100950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
064200110908     C                   Z-ADD     G02DAT        VIDdchd
064300111003     c* no maggiore di udate
064400111003     c                   if        comdchd>dateu
064500111003     C                   SETON                                        509028
064600111003     c                   eval      vidmsg=msg(13)
064700111003     C                   GOTO      ENDCTL
064800111003     C                   END
064900110908     C                   endif
065000110908     c
065100110908     C     VIDdcha       IFgt      0
065200110912     C                   MOVE      VIDDcha       G02DAT
065300110908     C                   MOVEL     *BLANKS       G02ERR
065400110908     C                   CALL      'XSRDA8'
065500110908     C                   PARM                    WLBDAT
065600110908     C     G02ERR        IFEQ      '1'
065700110908     C                   SETON                                        519028
065800110908     c                   eval      vidmsg=msg(5)
065900110908     C                   GOTO      ENDCTL
066000110908     C                   END
066100110908     c
066200110908     C                   Z-ADD     G02INV        COMDcha
066300110908     c
066400110908     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
066500110908     C                   Z-ADD     G02DAT        VIDdcha
066600111003     c* no maggiore di udate
066700111003     c                   if        comdcha>dateu
066800111003     C                   SETON                                        519028
066900111003     c                   eval      vidmsg=msg(13)
067000111003     C                   GOTO      ENDCTL
067100111003     C                   END
067200110908     c
067300110908     c                   if        comdcha<comdchd
067400110908     C                   SETON                                        519028
067500110912     c                   eval      vidmsg=msg(9)
067600110908     C                   GOTO      ENDCTL
067700110908     C                   END
067800110929     C                   endif
067900161213     c
068000161213     c* Per KPI le date sono oblogatorie
068100161213     c                   if        *in02 and VIDdchd=0
068200161213     C                   SETON                                        509028
068300161213     c                   eval      vidmsg=msg(17)
068400161213     C                   GOTO      ENDCTL
068500161213     C                   END
068600110908     c
068700110929     c* Obbligatorio inserire indirizzo e-mail
068800161209     c   01
068900161209     can kf              if        videmail=*blanks
069000110912     C                   SETON                                        529028
069100110912     c                   eval      vidmsg=msg(7)
069200110912     C                   GOTO      ENDCTL
069300111228     c                   else
069400111228     c* controllo formale
069500111228     c                   clear                   dsemail
069600120305     c                   eval      §EMLINDI=%trim(videmail)+'@brt.it'
069700111228     c                   call      'XEMAIL'
069800111228     c                   parm                    dsemail
069900111228     c                   if        §EMLERRO<>*blanks and
070000111228     c                             §EMLERRO<> '0'
070100111228     C                   SETON                                        529028
070200111228     c                   eval      vidmsg=§EMLmsgO
070300111228     C                   GOTO      ENDCTL
070400110912     c                   endif
070500111228     c                   endif
070600110912     c
070700920731     C     ENDCTL        ENDSR
070800961017**  MSG
070900961017Immettere linea di partenza valida                                            1
071000961017Codice Cliente annullato                                                      2
071100961017Codice Cliente errato                                                         3
071200961017Codice Cliente bloccato                                                       4
071300110908Data errata                                                                   5
071400110908Linea partenza errata o non gestibile                                         6
071500110912Inserire indirizzo e-mail BRT                                                 7
071600110912Serie errata                                                                  8
071700110912Data  AL  deve essere maggiore della data  DAL                                9
071800111003Se immessa SERIE o CLIENTE immettere anche la LINEA di PARTENZA               10
071900111003La serie non è quella assegnata al cliente                                    11
072000110912Effettuare almeno una selezione
072100111003La data non può essere maggiore della data del giorno                         13
072200161212Cliente non abilitato alla elaborazione  in oggetto                           14
072300161212Cliente obbligatorio per l'elaborazine del KPI                                15
072400171114Utente non abilitato all'utilizzo di questo programma . enter per uscire      16
072500161213Immettere la data spedizione                                                  17
072600161221Cliente non in gestione non utilizzabile per la linea indicata                18
072700170104Se immessa la "U" di unificante, indicare anche in codice                     19
072800170104Per cliente Unificante serie"99"non ammessa:indicare OGNI serie a cui abilit. 20
