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
001400060310     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
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
003500110912     d
003600961017     D                 DS
003700961017     D  VIDKLP                 1      3  0
003800961017     D  VIDKSC                 4      7  0
003900961017     D  VIDCLI                 1      7  0
004000941228     D*
004100111228     D dsemail       E DS
004200111228     D DS3C          E DS
004300920421     D KPJBA         E DS
004400941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004500941228     D***
004600941228     D WLBDAT          DS                  INZ
004700941228     D  G02DAT                 1      8  0
004800941228     D  G02INV                 9     16  0
004900941228     D  G02ERR                17     17
005000941228     D  G02TGI                18     22  0
005100941228     D***
005200941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
005300941228     D***
005400941228     D WGIDAT          DS                  INZ
005500941228     D  GIODAT                 1      8  0
005600941228     D  GIOINV                 9     16  0
005700941228     D  GIOTGI                17     21  0
005800941228     D* DS PER TRUL06R - CARICAMENTO £X
005900941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006000941228     D  LIN                    1     90  0
006100941228     D                                     DIM(30)
006200961025     D*
006300021203
006400060310     D alfa3cnrs       S              2
006500941228     D*
006600110908     D TIBS34DS      E DS
006700110908     d Azuteds       e ds                  extname(Azute00f)
006800110908     d dDatiute      e ds
006900111003     d Dataiso         s               d   datfmt(*iso)
007000111003     d Datasys         s               d   datfmt(*iso)  inz(*sys)
007100111003     d dateu           s              8  0
007200900702     C****************************************************************
007300900702     C*  RIEPILOGO INDICATORI
007400900702     C***************************************************************
007500161207     C* 01    - invio anomalie ai clienti
007600161207     C* 02    - KPI   clienti
007700161207     C* 36    - SFLNXTCHG
007800920804     C* 37    - PULIZIA SUBFILE
007900920522     C* 30/35 - DI COMODO
008000920901     C* 40/57 - ERRORE
008100110912     C* 90    - ERRORE GENERICO
008200900702     C*****************************************************************
008300000000     C     *ENTRY        PLIST
008400000000     C                   PARM                    KPJBA
008500161209     c                   movel     kpjbu         tipo              1
008600950530     C*
008700110912     C                   Z-ADD     1             CODUT             1 0
008800110908     c     *dtaara       define    §azute        azuteds
008900110908     c     *dtaara       define    §datiute      ddatiute
009000110908     c                   in(E)     *dtaara
009100110908    1c                   If        %error  or RSUT = *blanks
009200110908     c                   Clear                   Tibs34ds
009300110908     c                   Call      'TIBS34R'
009400110908     c                   Parm                    Tibs34ds
009500110908     c                   In        *dtaara
009600110908    1c                   EndIf
009700110912     C                   MOVEL     rsut          PARDUT           30
009800161209     c* Verifico se si tratta di invio anomalie o kpi
009900161209     c                   if        tipo='K'
010000161209     c                   seton                                        02
010100161209     c                   else
010200161209     c                   seton                                        01
010300161209     c                   endif
010400920901     C*---------------------------------------------------------------*
010500111003     c                   eval      dataiso=datasys
010600111003     c                   eval      dateu= %dec(dataiso)
010700111003     C*---------------------------------------------------------------*
010800920730     C* ACCESSO CNACO
010900920730     C     KACO          KLIST
011000920730     C                   KFLD                    CODUT
011100110908     C                   KFLD                    dutKCI
011200920901     C                   KFLD                    KSC               7 0
011300920730     C* ACCESSO TABEL
011400920730     C     KTAB          KLIST
011500920730     C                   KFLD                    CODUT
011600920730     C                   KFLD                    COD               2
011700920730     C                   KFLD                    KEY               8
011800961030     C     KTAB2         KLIST
011900961030     C                   KFLD                    CODUT
012000961030     C                   KFLD                    COD               2
012100941228     C*---------------------------------------------------------------*
012200930125     C*
012300920730     C* CARICO TABELLA FILIALI GESTITE £1
012400920730     C                   EXSR      CARTAB
012500161213     c* Se non è utente EDP o della 166 --> uscita
012600161213     c                   if        dutpou<>166 and %subst(knmus:1:3)<>'EDP'
012601161213     c                             and *in02
012700161213     c                   eval      vidmsg=msg(16)
012800161213     c                   seton                                        289099
012900161213     c                   endif
013000920730     C*
013100920424     C     INIZIO        TAG
013200920730     C* PULIZIA DATI SUBFILE CONTROL
013300920901     C                   SETOFF                                       04
013400110908     C                   clear                   VIddchd
013500110908     C                   clear                   VIDdcha
013600110912     C                   clear                   VIDemail
013700161212     C                   clear                   VIDok
013800161212     C                   clear                   VIDeveok
013900920423     C*
014000920730     C                   Z-ADD     0             NRR               4 0
014100920730     C* CARICO SUBFILE
014200920831     C                   EXSR      CARICA
014300920730     C*
014400110908     C                   WRITE     LSW1Z01
014500920730     C     FORCT         TAG
014600110908     C                   EXFMT     LSW1DCT
014700941228     C*
014800961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
014900961017     C                   CLEAR                   VIDMSG
015000110912     C                   SETOFF                                       289052
015100110912     C                   SETOFF                                       444560
015200110912     C                   SETOFF                                       5051
015300920730     C*
015400110908     C** F3 - FINE LAVORO
015500161213     c   99
015600161213     Cor KC              GOTO      FINE
015700920730     C*
015800920730     C* CONTROLLO DATI SUBFILE
015900110908     C                   EXSR      CTRSFL
016000920831     C*
016100110908     C   90              GOTO      FORCT
016200920730     C*
016300920730     C* CONTROLLO DATI SUBFILE CONTROL
016400110908     C                   EXSR      CTRCTL
016500920730     C*
016600111116     c                   if        not *inkf and not *inkh or *in90
016700111116     C                   GOTO      FORCT
016800111116     c                   endif
016900920730     C*
017000920807     C* CARICO SCHIERE PER PASSAGGIO
017100920807     C                   MOVEA     *ZEROS        LNP
017200920807     C                   MOVEA     *ZEROS        CLI
017300930520     C                   MOVEA     *ZEROS        NRS
017400920901     C                   Z-ADD     1             X                 3 0
017500920807     C                   Z-ADD     1             NRR
017600920807     C*
017700111116     C                   SETON                                        36
017800110912     C     NRR           DOWLE     20
017900920807     C*
018000110908     C     NRR           CHAIN     LSW1DSF                            31
018100920831     C     *IN31         IFEQ      '0'
018200920831     C     VIDLNP        ANDNE     0
018300920807     C                   MOVEL     VIDLNP        LNP(X)
018400930520     C                   MOVEL     VIDNRS        NRS(X)
018500920807     C                   MOVEL     VIDCLI        CLI(X)
018600920901     C                   ADD       1             X
018700111116     c                   update    lsw1dsf
018800111116     C                   ENDif
018900920807     C*
019000920807     C                   ADD       1             NRR
019100111116     C                   ENDdo
019200111116     C                   SETOff                                       36
019300920831     C*
019400110912     C     COMdcha       IFEQ      0
019500110912     C                   Z-ADD     COMDchd       COMDcha
019600980415     C                   END
019700161209     C* ANOMALIE
019800161209     c                   if        *in01
019900161209     c
020000111116     c                   if        *inkh
020100111116     c                   eval      pargest='G'
020200111116     c                   else
020300111116     c                   eval      pargest='I'
020400111116     c                   endif
020500110913     C                   MOVEL     fnlsw1ds      KPJBU
020600110912     C                   MOVEL     'LSW2'        KCOAZ
020700111116     c* gestione anomalie
020800111116     c                   if        *inkh
020900111116     c                   call      'FNLSW2R'
021000111116     C                   PARM                    KPJBA
021100111116     c
021200111116     C                   WRITE     LSW1Z01
021300111116     C                   GOTO      FORCT
021400111116     c
021500111116     c                   else
021600111207     C                   CALL      'BCH10'
021700111207     c***                call      'FNLSW2R'
021800900702     C                   PARM                    KPJBA
021900111116     c                   endif
022000161209     C
022100161209     C                   ELSE
022200161209     C* KPI
022300161209     C                   MOVEL     fnlsw1ds      KPJBU
022400161209     C                   MOVEL     'LSW7'        KCOAZ
022500161213     C                   CALL      'BCH10'
022600161213     c****               call      'FNLSW7R'
022700161209     C                   PARM                    KPJBA
022800161209     C                   ENDIF
022900920522     C*
023000950411     C                   GOTO      INIZIO
023100000000     C     FINE          TAG
023200000000     C                   SETON                                        LR
023300920730     C*
023400920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
023500920730     C     CARTAB        BEGSR
023600941228     C***
023700941228     C* CARICO TABELLA FILIALI GESTITE £1
023800941228     C***
023900941228     C                   CLEAR                   DSUL06
024000941228     C                   MOVE      '£1'          D06COD
024100941228     C                   MOVEL     SIMFEL        D06KEY
024200941228     C                   MOVEL     DSUL06        KPJBU
024300941228     C*
024400950323     C                   CALL      'TRUL06R'
024500941228     C                   PARM                    KPJBA
024600941228     C                   MOVEL     KPJBU         DSUL06
024700941228     C                   MOVEA     LIN           L1
024800920730     C                   ENDSR
024900920831     C*
025000920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
025100920730     C*
025200920831     C     CARICA        BEGSR
025300920831     C* PULIZIA SUBFILE
025400920804     C                   SETON                                        37
025500110908     C                   WRITE     LSW1DCT
025600920804     C                   SETOFF                                       37
025700920730     C*
025800110908     C* imposto sfl vuoto
025900920730     C*
026000920730     C                   Z-ADD     0             VIDLNP
026100920730     C                   MOVEL     *BLANKS       DESLNP
026200060310     C                   movel     99            VIDNRS
026300920730     C                   Z-ADD     0             VIDCLI
026400920730     C                   MOVEL     *BLANKS       DESCLI
026500920730     C*
026600110912     C                   DO        20            NRR
026700110908     C                   WRITE     LSW1DSF
026800941228     C                   ENDDO
026900920730     C*
027000920831     C                   Z-ADD     1             REC
027100920730     C                   ENDSR
027200920730     C*
027300920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
027400110908     C     CTRSFL        BEGSR
027500110908     C                   SETOFF                                       90
027600920730     C                   Z-ADD     1             NRR
027700110912     C                   clear                   sellnp            1
027800920730     C*
027900110908     C                   READC     LSW1DSF                                33
028000920730     C     *IN33         DOWEQ     '0'
028100930520     C*
028200961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
028300961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
028400961017    2C     VIDKLP        IFGT      0
028500961017     C     VIDKLP        CHAIN     AZORG01L                           34
028600961017    3C     *IN34         IFEQ      *ON
028700110908     C                   SETON                                        284490
028800961017     C                   MOVEL     MSG(1)        VIDMSG
028900961017     C                   GOTO      ENDSFL
029000961017    3C                   ENDIF
029100110908     c
029200961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
029300961017    3C     VIDKSC        IFEQ      0
029400961017     C     DESCLI        ANDEQ     *BLANKS
029500110908     C                   SETON                                        284590
029600961017     C                   MOVEL     MSG(3)        VIDMSG
029700961017     C                   GOTO      ENDSFL
029800961017    3C                   ENDIF
029900961017     C*
030000961017   X2C                   ELSE
030100961017    3C     VIDKSC        IFGT      0
030200110908     C                   SETON                                        284490
030300961017     C                   MOVEL     MSG(1)        VIDMSG
030400961017     C                   GOTO      ENDSFL
030500961017    3C                   ENDIF
030600961017    2C                   ENDIF
030700961017     C*
030800920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
030900920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
031000961017     C     VIDKSC        IFEQ      0
031100920730     C     DESCLI        ANDNE     *BLANKS
031200981109     C                   CLEAR                   PARDIT
031300920730     C*
031400920730     C* PARSTA = 9  ESCLUDO ANNULLATI
031500110908     C                   Z-ADD     dutKCI        PARKCC            4 0
031600920730     C                   Z-ADD     9             PARSTA
031700920902     C                   MOVEL     *BLANKS       DES              48
031800920902     C                   MOVEL     DESCLI        DES
031900961017     C                   MOVEL     VIDKLP        PARFLR
032000981109     C     VIDKLP        CHAIN     AZORG01L                           31
032100981109     C  N31              MOVEL     ORGDIT        PARDIT
032200000907     C                   Z-ADD     1             PAXNUM
032300000907     C                   CALL      'XALFA3BR'
032400920730     C                   PARM                    PARDUT
032500920730     C                   PARM                    CODUT
032600920902     C                   PARM                    DES
032700961016     C                   PARM                    PARKCC
032800920730     C                   PARM                    PARSTA            1 0
032900961016     C                   PARM                    PARFLR           90
033000981109     C                   PARM                    PARDIT            3
033100000907     C                   PARM                    PAXNUM            2 0
033200000907     C                   PARM                    PAXKCM           80
033300000907     C                   PARM                    PAXKSM          140
033400000907     C                   PARM                    PAXKDM           60
033500920831     C     PARSTA        IFNE      -1
033600000907     C                   MOVEL     PAXKSM        VIDCLI
033700920902     C                   MOVEL     DES           DESCLI
033800920831     C                   END
033900920831     C*
034000110908     C                   SETON                                        9045
034100110908     c                   write     lsw1Z01
034200920730     C                   GOTO      ENDSFL
034300920730     C                   END
034400920730     C*
034500920901     C*** LINEA DI PARTENZA
034600961025    1C     VIDLNP        IFNE      0
034700920901     C*
034800920901     C* CONTROLLO SE LINEA VALIDA
034900920901     C     VIDLNP        LOOKUP    L1                                     31
035000061212     c
035100061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
035200061212     c*  per poterla accettare
035300110912    2c                   if        not *in31
035400161212
035500161212     c                   if        *in01
035600110912    3c                   if        vidnrs='00' or vidcli=0
035700110912     c                             or vidnrs='SI' or vidnrs='99'
035800110908     c                   eval      vidmsg=msg(6)
035900110908     C                   SETON                                        609028
036000061212     C                   GOTO      ENDSFL
036100110912    3c                   endif
036200161212     c
036300161212     c* Per ora per KPI  nessun controllo sulla lnp
036400161212    3c                   endif
036500110912    2c                   endif
036600920901     C*
036700920901     C* DECODIFICO LINEA DI PARTENZA
036800920901     C     VIDLNP        CHAIN     AZORG                              30
036900920901     C  N30              MOVEL     ORGDES        DESLNP
037000920901     C   30              MOVEL     *BLANKS       DESLNP
037100061212     c
037200110912    2c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
037300061212     c                             or orgfva<>' '
037400110912     c                   eval      vidmsg=msg(6)
037500110912     C                   SETON                                        609028
037600110912     C                   GOTO      ENDSFL
037700061212     C                   GOTO      ENDSFL
037800110912    2c                   endif
037900110912     c                   eval      Sellnp='S'
038000920901     C*
038100961025   X1C                   ELSE
038200920902     C* LINEA DI PARTENZA = 0
038300920902     C                   MOVEL     *BLANKS       DESLNP
038400961025     C*
038500920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
038600950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
038700920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
038800110912    2c                   if        vidnrs<>'99' or vidcli>0
038900110912     c                   eval      vidmsg=msg(10)
039000110912     C                   SETON                                        609028
039100110912     C                   GOTO      ENDSFL
039200110912    2c                   endif
039300961025     C**
039400961025    1C                   END
039500060310     C*** Numero serie
039600060310     C* ESEGUO TESTN
039700110912    1c                   if        vidnrs<>'SI'
039800060310     C                   TESTN                   VIDnrs               30
039900060310     C                   MOVE      VIDnrs        W001A             1
040000060310    2C     *IN30         IFEQ      *OFF
040100060310     C     W001A         ORLT      '0'
040200110908     C                   SETON                                        599028
040300110912     C                   MOVEL     MSG(8)        VIDMSG
040400060310     C                   GOTO      ENDSFL
040500060310    2C                   ENDIF
040600110912    1C                   ENDIF
040700920730     C*
040800961025    1C     VIDCLI        IFNE      0
040900930520     C*** NUMERO SERIE
041000060310    2C     VIDNRS        IFNE      '99'
041100060605     c                   clear                   ds3c
041200930521     C                   MOVEL     '3C'          COD
041300930521     C                   MOVEL     *BLANKS       KEY
041400930521     C                   MOVEL     VIDCLI        KEY
041500930521     C     KTAB          CHAIN     TABEL                              30
041600930521     C  N30              MOVEL     TBLUNI        DS3C
041700060605     c  N30              movel     §3cnrs        alfa3cnrs
041800930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
041900970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
042000980805     C     *IN30         IFEQ      *ON
042100980805     C     §3COKD        OREQ      ' '
042200060310    3C     alfa3cnrs     ORNE      VIDNRS
042300110912     C                   SETON                                        599028
042400110912     C                   MOVEL     MSG(11)       VIDMSG
042500930521     C                   GOTO      ENDSFL
042600961025    3C                   END
042700961025    2C                   END
042800930521     C*
042900961017     C****  CODICE CLIENTE  ****
043000920730     C                   MOVEL     VIDCLI        KSC
043100920901     C     KACO          CHAIN     CNACO000                           30
043200961025    2C  N30ACOFLG        IFNE      ' '
043300920730     C* CODICE CLIENTE ANNULLATO
043400110912     C                   SETON                                        284590
043500961017     C                   MOVEL     MSG(2)        VIDMSG
043600920730     C                   GOTO      ENDSFL
043700920730     C*
043800961025   X2C                   ELSE
043900920730     C* DECODIFICO CODICE CLIENTE
044000920730     C                   MOVEL     ACORAG        DESCLI
044100961025    2C                   END
044200920730     C*
044300920730     C* CODICE CLIENTE INESISTENTE
044400961025    2C     *IN30         IFEQ      *ON
044500110912     C                   SETON                                        284590
044600961017     C                   MOVEL     MSG(3)        VIDMSG
044700961017     C                   GOTO      ENDSFL
044800961025    2C                   ENDIF
044900920730     C*
045000920730     C* CODICE CLIENTE BLOCCATO
045100130320    2C***  ACOABL        IFEQ      '8'
045200130320    2C***  ACOABL        OREQ      '*'
045300130320     c                   if        acoabl<>*blanks
045400110912     C                   SETON                                        284590
045500961017     C                   MOVEL     MSG(4)        VIDMSG
045600920730     C                   GOTO      ENDSFL
045700961025    2C                   END
045800961025     C*
045900161212     c* Se si tratta della elaborazione kpi il codice deve essere presente
046000161212     c*  nel file fiabk00f
046100161212     c                   if        *in02
046200161212     c     acoksc        chain     fiabk02l
046300161212     c                   if        not %found(fiabk02l)
046400161212     C                   SETON                                        284590
046500161212     C                   MOVEL     MSG(14)       VIDMSG
046600161212     C                   GOTO      ENDSFL
046700161212    2C                   END
046800161212     c                   endif
046900961025     C**
047000961025    1C                   END
047100161212     c* Se si tratta della elaborazione kpi codice obbligatorio
047200161212     c                   if        *in02 and vidcli=0
047300161212     C                   SETON                                        284590
047400161212     C                   MOVEL     MSG(15)       VIDMSG
047500161212     C                   GOTO      ENDSFL
047600161212     c                   endif
047700161212     c*
047800920730     C*
047900920730     C     ENDSFL        TAG
048000110912     C   90              Z-ADD     NRR           REC
048100920730     C*
048200920730     C* 36 - SFLNXTCHG
048300920730     C                   SETON                                        36
048400110908     C                   UPDATE    LSW1DSF
048500920730     C                   SETOFF                                       36
048600920730     C*
048700110912     C   90              SETON                                        33
048800110912     C  N90              READC     LSW1DSF                                33
048900920730     C                   END
049000920730     C*
049100110912     c* se non ho selezionato nulla --> errore
049200110912     c  N90              if        sellnp=' '
049300110912     C                   Z-ADD     1             REC
049400110912     C                   Z-ADD     1             nrr
049500110912     C*
049600110912     C* 36 - SFLNXTCHG
049700110912     C     nrr           chain     LSW1DSF                            33
049800110912     C                   SETON                                        36
049900110912     C                   SETON                                        286090
050000110912     C                   MOVEL     MSG(12)       VIDMSG
050100110912     C                   UPDATE    LSW1DSF
050200110912     C                   SETOFF                                       36
050300110912     c                   endif
050400110912     c
050500920730     C                   ENDSR
050600920730     C*
050700920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
050800110908     C     CtrCTL        BEGSR
050900920731     C*
051000110908     c                   clear                   comdchd
051100110908     c                   clear                   comdcha
051200110908     c
051300110908     C*** DATA chiusura dal <=data chiusura al
051400110908     C     VIDdchd       IFgt      0
051500110912     C                   MOVE      VIDDchd       G02DAT
051600920903     C                   MOVEL     *BLANKS       G02ERR
051700941228     C                   CALL      'XSRDA8'
051800920903     C                   PARM                    WLBDAT
051900920903     C     G02ERR        IFEQ      '1'
052000110908     C                   SETON                                        509028
052100110908     c                   eval      vidmsg=msg(5)
052200920903     C                   GOTO      ENDCTL
052300920903     C                   END
052400110908     c
052500110908     C                   Z-ADD     G02INV        COMDchd
052600110908     c
052700950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
052800110908     C                   Z-ADD     G02DAT        VIDdchd
052900111003     c* no maggiore di udate
053000111003     c                   if        comdchd>dateu
053100111003     C                   SETON                                        509028
053200111003     c                   eval      vidmsg=msg(13)
053300111003     C                   GOTO      ENDCTL
053400111003     C                   END
053500110908     C                   endif
053600110908     c
053700110908     C     VIDdcha       IFgt      0
053800110912     C                   MOVE      VIDDcha       G02DAT
053900110908     C                   MOVEL     *BLANKS       G02ERR
054000110908     C                   CALL      'XSRDA8'
054100110908     C                   PARM                    WLBDAT
054200110908     C     G02ERR        IFEQ      '1'
054300110908     C                   SETON                                        519028
054400110908     c                   eval      vidmsg=msg(5)
054500110908     C                   GOTO      ENDCTL
054600110908     C                   END
054700110908     c
054800110908     C                   Z-ADD     G02INV        COMDcha
054900110908     c
055000110908     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
055100110908     C                   Z-ADD     G02DAT        VIDdcha
055200111003     c* no maggiore di udate
055300111003     c                   if        comdcha>dateu
055400111003     C                   SETON                                        519028
055500111003     c                   eval      vidmsg=msg(13)
055600111003     C                   GOTO      ENDCTL
055700111003     C                   END
055800110908     c
055900110908     c                   if        comdcha<comdchd
056000110908     C                   SETON                                        519028
056100110912     c                   eval      vidmsg=msg(9)
056200110908     C                   GOTO      ENDCTL
056300110908     C                   END
056400110929     C                   endif
056500161213     c
056600161213     c* Per KPI le date sono oblogatorie
056700161213     c                   if        *in02 and VIDdchd=0
056800161213     C                   SETON                                        509028
056900161213     c                   eval      vidmsg=msg(17)
057000161213     C                   GOTO      ENDCTL
057100161213     C                   END
057200110908     c
057300110929     c* Obbligatorio inserire indirizzo e-mail
057400161209     c   01
057500161209     can kf              if        videmail=*blanks
057600110912     C                   SETON                                        529028
057700110912     c                   eval      vidmsg=msg(7)
057800110912     C                   GOTO      ENDCTL
057900111228     c                   else
058000111228     c* controllo formale
058100111228     c                   clear                   dsemail
058200120305     c                   eval      §EMLINDI=%trim(videmail)+'@brt.it'
058300111228     c                   call      'XEMAIL'
058400111228     c                   parm                    dsemail
058500111228     c                   if        §EMLERRO<>*blanks and
058600111228     c                             §EMLERRO<> '0'
058700111228     C                   SETON                                        529028
058800111228     c                   eval      vidmsg=§EMLmsgO
058900111228     C                   GOTO      ENDCTL
059000110912     c                   endif
059100111228     c                   endif
059200110912     c
059300920731     C     ENDCTL        ENDSR
059400961017**  MSG
059500961017Immettere linea di partenza valida                                            1
059600961017Codice Cliente annullato                                                      2
059700961017Codice Cliente errato                                                         3
059800961017Codice Cliente bloccato                                                       4
059900110908Data errata                                                                   5
060000110908Linea partenza errata o non gestibile                                         6
060100110912Inserire indirizzo e-mail BRT                                                 7
060200110912Serie errata                                                                  8
060300110912Data  AL  deve essere maggiore della data  DAL                                9
060400111003Se immessa SERIE o CLIENTE immettere anche la LINEA di PARTENZA               10
060500111003La serie non è quella assegnata al cliente                                    11
060600110912Effettuare almeno una selezione
060700111003La data non può essere maggiore della data del giorno                         13
060800161212Cliente non abilitato alla elaborazione  in oggetto                           14
060900161212Cliente obbligatorio per l'elaborazine del KPI                                15
061000161213Programma in TEST soltanto alla filiale 166. Enter esce                       16
061100161213Immettere la data spedizione                                                  17
