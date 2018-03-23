000100980717     H DECEDIT('0,') DATEDIT(*DMY.)
000200110908     H* FNLSW1R *----------------------------------------------------*
000300110908     H*       Invio anomalie ai clienti mittenti - filtro
000400000000     H*--------------------------------------------------------------*
000500110908     FFNLSW1D   CF   E             WORKSTN
000600110908     F                                     SFILE(LSW1DSF:NRR)
000700920730     FCNACO00F  IF   E           K DISK
000800920901     FTABEL00F  IF   E           K DISK
000900920901     FAZORG01L  IF   E           K DISK
001000920804     D L1              S              3  0 DIM(30)
001100950530     D*
001200060310     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
001300980108     D*
001400980108     D* SCHIERA DI COMODO X CARICAMENTO LNP
001500110912     D LP              S              3  0 DIM(20)
001600110912     D CLP             S              3  0 DIM(20)
001700950530     D**
001800110912     D* PASSAGGIO PARAMETRI AL BATCH - FNLSW2R -
001900950530     D**
002000110913     D fnlsw1ds        DS
002100110912     D  COMdchd                1      8  0
002200110914     D  COMdcha                9     16  0
002300110914     D  LNP                   17     56P 0
002400110912     D                                     DIM(20)
002500110914     D  CLI                   57    136P 0
002600110912     D                                     DIM(20)
002700110914     D  NRS                  137    176
002800110912     D                                     DIM(20)
002900110914     D  videmail             177    201
003000111116     D  parGEST              202    202
003100110912     d
003200961017     D                 DS
003300961017     D  VIDKLP                 1      3  0
003400961017     D  VIDKSC                 4      7  0
003500961017     D  VIDCLI                 1      7  0
003600941228     D*
003700111228     D dsemail       E DS
003800111228     D DS3C          E DS
003900920421     D KPJBA         E DS
004000941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004100941228     D***
004200941228     D WLBDAT          DS                  INZ
004300941228     D  G02DAT                 1      8  0
004400941228     D  G02INV                 9     16  0
004500941228     D  G02ERR                17     17
004600941228     D  G02TGI                18     22  0
004700941228     D***
004800941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
004900941228     D***
005000941228     D WGIDAT          DS                  INZ
005100941228     D  GIODAT                 1      8  0
005200941228     D  GIOINV                 9     16  0
005300941228     D  GIOTGI                17     21  0
005400941228     D* DS PER TRUL06R - CARICAMENTO £X
005500941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005600941228     D  LIN                    1     90  0
005700941228     D                                     DIM(30)
005800961025     D*
005900021203
006000060310     D alfa3cnrs       S              2
006100941228     D*
006200110908     D TIBS34DS      E DS
006300110908     d Azuteds       e ds                  extname(Azute00f)
006400110908     d dDatiute      e ds
006500111003     d Dataiso         s               d   datfmt(*iso)
006600111003     d Datasys         s               d   datfmt(*iso)  inz(*sys)
006700111003     d dateu           s              8  0
006800900702     C****************************************************************
006900900702     C*  RIEPILOGO INDICATORI
007000900702     C***************************************************************
007100920804     C* 36    - SFLNXTCHG
007200920804     C* 37    - PULIZIA SUBFILE
007300920522     C* 30/35 - DI COMODO
007400920901     C* 40/57 - ERRORE
007500110912     C* 90    - ERRORE GENERICO
007600900702     C*****************************************************************
007700000000     C     *ENTRY        PLIST
007800000000     C                   PARM                    KPJBA
007900950530     C*
008000110912     C                   Z-ADD     1             CODUT             1 0
008100110908     c     *dtaara       define    §azute        azuteds
008200110908     c     *dtaara       define    §datiute      ddatiute
008300110908     c                   in(E)     *dtaara
008400110908    1c                   If        %error  or RSUT = *blanks
008500110908     c                   Clear                   Tibs34ds
008600110908     c                   Call      'TIBS34R'
008700110908     c                   Parm                    Tibs34ds
008800110908     c                   In        *dtaara
008900110908    1c                   EndIf
009000110912     C                   MOVEL     rsut          PARDUT           30
009100920901     C*---------------------------------------------------------------*
009200111003     c                   eval      dataiso=datasys
009300111003     c                   eval      dateu= %dec(dataiso)
009400111003     C*---------------------------------------------------------------*
009500920730     C* ACCESSO CNACO
009600920730     C     KACO          KLIST
009700920730     C                   KFLD                    CODUT
009800110908     C                   KFLD                    dutKCI
009900920901     C                   KFLD                    KSC               7 0
010000920730     C* ACCESSO TABEL
010100920730     C     KTAB          KLIST
010200920730     C                   KFLD                    CODUT
010300920730     C                   KFLD                    COD               2
010400920730     C                   KFLD                    KEY               8
010500961030     C     KTAB2         KLIST
010600961030     C                   KFLD                    CODUT
010700961030     C                   KFLD                    COD               2
010800941228     C*---------------------------------------------------------------*
010900930125     C*
011000920730     C* CARICO TABELLA FILIALI GESTITE £1
011100920730     C                   EXSR      CARTAB
011200920730     C*
011300920424     C     INIZIO        TAG
011400920730     C* PULIZIA DATI SUBFILE CONTROL
011500920901     C                   SETOFF                                       04
011600110908     C                   clear                   VIddchd
011700110908     C                   clear                   VIDdcha
011800110912     C                   clear                   VIDemail
011900920423     C*
012000920730     C                   Z-ADD     0             NRR               4 0
012100920730     C* CARICO SUBFILE
012200920831     C                   EXSR      CARICA
012300920730     C*
012400110908     C                   WRITE     LSW1Z01
012500920730     C     FORCT         TAG
012600110908     C                   EXFMT     LSW1DCT
012700941228     C*
012800961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
012900961017     C                   CLEAR                   VIDMSG
013000110912     C                   SETOFF                                       289052
013100110912     C                   SETOFF                                       444560
013200110912     C                   SETOFF                                       5051
013300920730     C*
013400110908     C** F3 - FINE LAVORO
013500110908     C   KC              GOTO      FINE
013600920730     C*
013700920730     C* CONTROLLO DATI SUBFILE
013800110908     C                   EXSR      CTRSFL
013900920831     C*
014000110908     C   90              GOTO      FORCT
014100920730     C*
014200920730     C* CONTROLLO DATI SUBFILE CONTROL
014300110908     C                   EXSR      CTRCTL
014400920730     C*
014500111116     c                   if        not *inkf and not *inkh or *in90
014600111116     C                   GOTO      FORCT
014700111116     c                   endif
014800920730     C*
014900920807     C* CARICO SCHIERE PER PASSAGGIO
015000920807     C                   MOVEA     *ZEROS        LNP
015100920807     C                   MOVEA     *ZEROS        CLI
015200930520     C                   MOVEA     *ZEROS        NRS
015300920901     C                   Z-ADD     1             X                 3 0
015400920807     C                   Z-ADD     1             NRR
015500920807     C*
015600111116     C                   SETON                                        36
015700110912     C     NRR           DOWLE     20
015800920807     C*
015900110908     C     NRR           CHAIN     LSW1DSF                            31
016000920831     C     *IN31         IFEQ      '0'
016100920831     C     VIDLNP        ANDNE     0
016200920807     C                   MOVEL     VIDLNP        LNP(X)
016300930520     C                   MOVEL     VIDNRS        NRS(X)
016400920807     C                   MOVEL     VIDCLI        CLI(X)
016500920901     C                   ADD       1             X
016600111116     c                   update    lsw1dsf
016700111116     C                   ENDif
016800920807     C*
016900920807     C                   ADD       1             NRR
017000111116     C                   ENDdo
017100111116     C                   SETOff                                       36
017200920831     C*
017300110912     C     COMdcha       IFEQ      0
017400110912     C                   Z-ADD     COMDchd       COMDcha
017500980415     C                   END
017600980415     C*
017700111116     c                   if        *inkh
017800111116     c                   eval      pargest='G'
017900111116     c                   else
018000111116     c                   eval      pargest='I'
018100111116     c                   endif
018200110913     C                   MOVEL     fnlsw1ds      KPJBU
018300110912     C                   MOVEL     'LSW2'        KCOAZ
018400111116     c* gestione anomalie
018500111116     c                   if        *inkh
018600111116     c                   call      'FNLSW2R'
018700111116     C                   PARM                    KPJBA
018800111116     c
018900111116     C                   WRITE     LSW1Z01
019000111116     C                   GOTO      FORCT
019100111116     c
019200111116     c                   else
019300111207     C                   CALL      'BCH10'
019400111207     c***                call      'FNLSW2R'
019500900702     C                   PARM                    KPJBA
019600111116     c                   endif
019700920522     C*
019800950411     C                   GOTO      INIZIO
019900000000     C     FINE          TAG
020000000000     C                   SETON                                        LR
020100920730     C*
020200920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
020300920730     C     CARTAB        BEGSR
020400941228     C***
020500941228     C* CARICO TABELLA FILIALI GESTITE £1
020600941228     C***
020700941228     C                   CLEAR                   DSUL06
020800941228     C                   MOVE      '£1'          D06COD
020900941228     C                   MOVEL     SIMFEL        D06KEY
021000941228     C                   MOVEL     DSUL06        KPJBU
021100941228     C*
021200950323     C                   CALL      'TRUL06R'
021300941228     C                   PARM                    KPJBA
021400941228     C                   MOVEL     KPJBU         DSUL06
021500941228     C                   MOVEA     LIN           L1
021600920730     C                   ENDSR
021700920831     C*
021800920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
021900920730     C*
022000920831     C     CARICA        BEGSR
022100920831     C* PULIZIA SUBFILE
022200920804     C                   SETON                                        37
022300110908     C                   WRITE     LSW1DCT
022400920804     C                   SETOFF                                       37
022500920730     C*
022600110908     C* imposto sfl vuoto
022700920730     C*
022800920730     C                   Z-ADD     0             VIDLNP
022900920730     C                   MOVEL     *BLANKS       DESLNP
023000060310     C                   movel     99            VIDNRS
023100920730     C                   Z-ADD     0             VIDCLI
023200920730     C                   MOVEL     *BLANKS       DESCLI
023300920730     C*
023400110912     C                   DO        20            NRR
023500110908     C                   WRITE     LSW1DSF
023600941228     C                   ENDDO
023700920730     C*
023800920831     C                   Z-ADD     1             REC
023900920730     C                   ENDSR
024000920730     C*
024100920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
024200110908     C     CTRSFL        BEGSR
024300110908     C                   SETOFF                                       90
024400920730     C                   Z-ADD     1             NRR
024500110912     C                   clear                   sellnp            1
024600920730     C*
024700110908     C                   READC     LSW1DSF                                33
024800920730     C     *IN33         DOWEQ     '0'
024900930520     C*
025000961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
025100961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
025200961017    2C     VIDKLP        IFGT      0
025300961017     C     VIDKLP        CHAIN     AZORG01L                           34
025400961017    3C     *IN34         IFEQ      *ON
025500110908     C                   SETON                                        284490
025600961017     C                   MOVEL     MSG(1)        VIDMSG
025700961017     C                   GOTO      ENDSFL
025800961017    3C                   ENDIF
025900110908     c
026000961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
026100961017    3C     VIDKSC        IFEQ      0
026200961017     C     DESCLI        ANDEQ     *BLANKS
026300110908     C                   SETON                                        284590
026400961017     C                   MOVEL     MSG(3)        VIDMSG
026500961017     C                   GOTO      ENDSFL
026600961017    3C                   ENDIF
026700961017     C*
026800961017   X2C                   ELSE
026900961017    3C     VIDKSC        IFGT      0
027000110908     C                   SETON                                        284490
027100961017     C                   MOVEL     MSG(1)        VIDMSG
027200961017     C                   GOTO      ENDSFL
027300961017    3C                   ENDIF
027400961017    2C                   ENDIF
027500961017     C*
027600920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
027700920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
027800961017     C     VIDKSC        IFEQ      0
027900920730     C     DESCLI        ANDNE     *BLANKS
028000981109     C                   CLEAR                   PARDIT
028100920730     C*
028200920730     C* PARSTA = 9  ESCLUDO ANNULLATI
028300110908     C                   Z-ADD     dutKCI        PARKCC            4 0
028400920730     C                   Z-ADD     9             PARSTA
028500920902     C                   MOVEL     *BLANKS       DES              48
028600920902     C                   MOVEL     DESCLI        DES
028700961017     C                   MOVEL     VIDKLP        PARFLR
028800981109     C     VIDKLP        CHAIN     AZORG01L                           31
028900981109     C  N31              MOVEL     ORGDIT        PARDIT
029000000907     C                   Z-ADD     1             PAXNUM
029100000907     C                   CALL      'XALFA3BR'
029200920730     C                   PARM                    PARDUT
029300920730     C                   PARM                    CODUT
029400920902     C                   PARM                    DES
029500961016     C                   PARM                    PARKCC
029600920730     C                   PARM                    PARSTA            1 0
029700961016     C                   PARM                    PARFLR           90
029800981109     C                   PARM                    PARDIT            3
029900000907     C                   PARM                    PAXNUM            2 0
030000000907     C                   PARM                    PAXKCM           80
030100000907     C                   PARM                    PAXKSM          140
030200000907     C                   PARM                    PAXKDM           60
030300920831     C     PARSTA        IFNE      -1
030400000907     C                   MOVEL     PAXKSM        VIDCLI
030500920902     C                   MOVEL     DES           DESCLI
030600920831     C                   END
030700920831     C*
030800110908     C                   SETON                                        9045
030900110908     c                   write     lsw1Z01
031000920730     C                   GOTO      ENDSFL
031100920730     C                   END
031200920730     C*
031300920901     C*** LINEA DI PARTENZA
031400961025    1C     VIDLNP        IFNE      0
031500920901     C*
031600920901     C* CONTROLLO SE LINEA VALIDA
031700920901     C     VIDLNP        LOOKUP    L1                                     31
031800061212     c
031900061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
032000061212     c*  per poterla accettare
032100110912    2c                   if        not *in31
032200110912    3c                   if        vidnrs='00' or vidcli=0
032300110912     c                             or vidnrs='SI' or vidnrs='99'
032400110908     c                   eval      vidmsg=msg(6)
032500110908     C                   SETON                                        609028
032600061212     C                   GOTO      ENDSFL
032700110912    3c                   endif
032800110912    2c                   endif
032900920901     C*
033000920901     C* DECODIFICO LINEA DI PARTENZA
033100920901     C     VIDLNP        CHAIN     AZORG                              30
033200920901     C  N30              MOVEL     ORGDES        DESLNP
033300920901     C   30              MOVEL     *BLANKS       DESLNP
033400061212     c
033500110912    2c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
033600061212     c                             or orgfva<>' '
033700110912     c                   eval      vidmsg=msg(6)
033800110912     C                   SETON                                        609028
033900110912     C                   GOTO      ENDSFL
034000061212     C                   GOTO      ENDSFL
034100110912    2c                   endif
034200110912     c                   eval      Sellnp='S'
034300920901     C*
034400961025   X1C                   ELSE
034500920902     C* LINEA DI PARTENZA = 0
034600920902     C                   MOVEL     *BLANKS       DESLNP
034700961025     C*
034800920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
034900950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
035000920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
035100110912    2c                   if        vidnrs<>'99' or vidcli>0
035200110912     c                   eval      vidmsg=msg(10)
035300110912     C                   SETON                                        609028
035400110912     C                   GOTO      ENDSFL
035500110912    2c                   endif
035600961025     C**
035700961025    1C                   END
035800060310     C*** Numero serie
035900060310     C* ESEGUO TESTN
036000110912    1c                   if        vidnrs<>'SI'
036100060310     C                   TESTN                   VIDnrs               30
036200060310     C                   MOVE      VIDnrs        W001A             1
036300060310    2C     *IN30         IFEQ      *OFF
036400060310     C     W001A         ORLT      '0'
036500110908     C                   SETON                                        599028
036600110912     C                   MOVEL     MSG(8)        VIDMSG
036700060310     C                   GOTO      ENDSFL
036800060310    2C                   ENDIF
036900110912    1C                   ENDIF
037000920730     C*
037100961025    1C     VIDCLI        IFNE      0
037200930520     C*** NUMERO SERIE
037300060310    2C     VIDNRS        IFNE      '99'
037400060605     c                   clear                   ds3c
037500930521     C                   MOVEL     '3C'          COD
037600930521     C                   MOVEL     *BLANKS       KEY
037700930521     C                   MOVEL     VIDCLI        KEY
037800930521     C     KTAB          CHAIN     TABEL                              30
037900930521     C  N30              MOVEL     TBLUNI        DS3C
038000060605     c  N30              movel     §3cnrs        alfa3cnrs
038100930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
038200970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
038300980805     C     *IN30         IFEQ      *ON
038400980805     C     §3COKD        OREQ      ' '
038500060310    3C     alfa3cnrs     ORNE      VIDNRS
038600110912     C                   SETON                                        599028
038700110912     C                   MOVEL     MSG(11)       VIDMSG
038800930521     C                   GOTO      ENDSFL
038900961025    3C                   END
039000961025    2C                   END
039100930521     C*
039200961017     C****  CODICE CLIENTE  ****
039300920730     C                   MOVEL     VIDCLI        KSC
039400920901     C     KACO          CHAIN     CNACO000                           30
039500961025    2C  N30ACOFLG        IFNE      ' '
039600920730     C* CODICE CLIENTE ANNULLATO
039700110912     C                   SETON                                        284590
039800961017     C                   MOVEL     MSG(2)        VIDMSG
039900920730     C                   GOTO      ENDSFL
040000920730     C*
040100961025   X2C                   ELSE
040200920730     C* DECODIFICO CODICE CLIENTE
040300920730     C                   MOVEL     ACORAG        DESCLI
040400961025    2C                   END
040500920730     C*
040600920730     C* CODICE CLIENTE INESISTENTE
040700961025    2C     *IN30         IFEQ      *ON
040800110912     C                   SETON                                        284590
040900961017     C                   MOVEL     MSG(3)        VIDMSG
041000961017     C                   GOTO      ENDSFL
041100961025    2C                   ENDIF
041200920730     C*
041300920730     C* CODICE CLIENTE BLOCCATO
041400961025    2C     ACOABL        IFEQ      '8'
041500971021    2C     ACOABL        OREQ      '*'
041600110912     C                   SETON                                        284590
041700961017     C                   MOVEL     MSG(4)        VIDMSG
041800920730     C                   GOTO      ENDSFL
041900961025    2C                   END
042000961025     C*
042100961025     C**
042200961025    1C                   END
042300920730     C*
042400920730     C     ENDSFL        TAG
042500110912     C   90              Z-ADD     NRR           REC
042600920730     C*
042700920730     C* 36 - SFLNXTCHG
042800920730     C                   SETON                                        36
042900110908     C                   UPDATE    LSW1DSF
043000920730     C                   SETOFF                                       36
043100920730     C*
043200110912     C   90              SETON                                        33
043300110912     C  N90              READC     LSW1DSF                                33
043400920730     C                   END
043500920730     C*
043600110912     c* se non ho selezionato nulla --> errore
043700110912     c  N90              if        sellnp=' '
043800110912     C                   Z-ADD     1             REC
043900110912     C                   Z-ADD     1             nrr
044000110912     C*
044100110912     C* 36 - SFLNXTCHG
044200110912     C     nrr           chain     LSW1DSF                            33
044300110912     C                   SETON                                        36
044400110912     C                   SETON                                        286090
044500110912     C                   MOVEL     MSG(12)       VIDMSG
044600110912     C                   UPDATE    LSW1DSF
044700110912     C                   SETOFF                                       36
044800110912     c                   endif
044900110912     c
045000920730     C                   ENDSR
045100920730     C*
045200920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
045300110908     C     CtrCTL        BEGSR
045400920731     C*
045500110908     c                   clear                   comdchd
045600110908     c                   clear                   comdcha
045700110908     c
045800110908     C*** DATA chiusura dal <=data chiusura al
045900110908     C     VIDdchd       IFgt      0
046000110912     C                   MOVE      VIDDchd       G02DAT
046100920903     C                   MOVEL     *BLANKS       G02ERR
046200941228     C                   CALL      'XSRDA8'
046300920903     C                   PARM                    WLBDAT
046400920903     C     G02ERR        IFEQ      '1'
046500110908     C                   SETON                                        509028
046600110908     c                   eval      vidmsg=msg(5)
046700920903     C                   GOTO      ENDCTL
046800920903     C                   END
046900110908     c
047000110908     C                   Z-ADD     G02INV        COMDchd
047100110908     c
047200950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
047300110908     C                   Z-ADD     G02DAT        VIDdchd
047400111003     c* no maggiore di udate
047500111003     c                   if        comdchd>dateu
047600111003     C                   SETON                                        509028
047700111003     c                   eval      vidmsg=msg(13)
047800111003     C                   GOTO      ENDCTL
047900111003     C                   END
048000110908     C                   endif
048100110908     c
048200110908     C     VIDdcha       IFgt      0
048300110912     C                   MOVE      VIDDcha       G02DAT
048400110908     C                   MOVEL     *BLANKS       G02ERR
048500110908     C                   CALL      'XSRDA8'
048600110908     C                   PARM                    WLBDAT
048700110908     C     G02ERR        IFEQ      '1'
048800110908     C                   SETON                                        519028
048900110908     c                   eval      vidmsg=msg(5)
049000110908     C                   GOTO      ENDCTL
049100110908     C                   END
049200110908     c
049300110908     C                   Z-ADD     G02INV        COMDcha
049400110908     c
049500110908     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
049600110908     C                   Z-ADD     G02DAT        VIDdcha
049700111003     c* no maggiore di udate
049800111003     c                   if        comdcha>dateu
049900111003     C                   SETON                                        519028
050000111003     c                   eval      vidmsg=msg(13)
050100111003     C                   GOTO      ENDCTL
050200111003     C                   END
050300110908     c
050400110908     c                   if        comdcha<comdchd
050500110908     C                   SETON                                        519028
050600110912     c                   eval      vidmsg=msg(9)
050700110908     C                   GOTO      ENDCTL
050800110908     C                   END
050900110929     C                   endif
051000110908     c
051100110929     c* Obbligatorio inserire indirizzo e-mail
051200111228     c   kf              if        videmail=*blanks
051300110912     C                   SETON                                        529028
051400110912     c                   eval      vidmsg=msg(7)
051500110912     C                   GOTO      ENDCTL
051600111228     c                   else
051700111228     c* controllo formale
051800111228     c                   clear                   dsemail
051900120305     c                   eval      §EMLINDI=%trim(videmail)+'@brt.it'
052000111228     c                   call      'XEMAIL'
052100111228     c                   parm                    dsemail
052200111228     c                   if        §EMLERRO<>*blanks and
052300111228     c                             §EMLERRO<> '0'
052400111228     C                   SETON                                        529028
052500111228     c                   eval      vidmsg=§EMLmsgO
052600111228     C                   GOTO      ENDCTL
052700110912     c                   endif
052800111228     c                   endif
052900110912     c
053000920731     C     ENDCTL        ENDSR
053100961017**  MSG
053200961017Immettere linea di partenza valida                                            1
053300961017Codice Cliente annullato                                                      2
053400961017Codice Cliente errato                                                         3
053500961017Codice Cliente bloccato                                                       4
053600110908Data errata                                                                   5
053700110908Linea partenza errata o non gestibile                                         6
053800110912Inserire indirizzo e-mail BRT                                                 7
053900110912Serie errata                                                                  8
054000110912Data  AL  deve essere maggiore della data  DAL                                9
054100111003Se immessa SERIE o CLIENTE immettere anche la LINEA di PARTENZA               10
054200111003La serie non è quella assegnata al cliente                                    11
054300110912Effettuare almeno una selezione
054400111003La data non può essere maggiore della data del giorno                         13
