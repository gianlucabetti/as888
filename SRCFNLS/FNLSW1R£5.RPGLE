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
003500161216     D  vidcons              205    205
003600110912     d
003700961017     D                 DS
003800961017     D  VIDKLP                 1      3  0
003900961017     D  VIDKSC                 4      7  0
004000961017     D  VIDCLI                 1      7  0
004100941228     D*
004200111228     D dsemail       E DS
004300111228     D DS3C          E DS
004400920421     D KPJBA         E DS
004500941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004600941228     D***
004700941228     D WLBDAT          DS                  INZ
004800941228     D  G02DAT                 1      8  0
004900941228     D  G02INV                 9     16  0
005000941228     D  G02ERR                17     17
005100941228     D  G02TGI                18     22  0
005200941228     D***
005300941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
005400941228     D***
005500941228     D WGIDAT          DS                  INZ
005600941228     D  GIODAT                 1      8  0
005700941228     D  GIOINV                 9     16  0
005800941228     D  GIOTGI                17     21  0
005900941228     D* DS PER TRUL06R - CARICAMENTO £X
006000941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006100941228     D  LIN                    1     90  0
006200941228     D                                     DIM(30)
006300961025     D*
006400021203
006500060310     D alfa3cnrs       S              2
006600941228     D*
006700110908     D TIBS34DS      E DS
006800110908     d Azuteds       e ds                  extname(Azute00f)
006900110908     d dDatiute      e ds
007000111003     d Dataiso         s               d   datfmt(*iso)
007100111003     d Datasys         s               d   datfmt(*iso)  inz(*sys)
007200111003     d dateu           s              8  0
007300900702     C****************************************************************
007400900702     C*  RIEPILOGO INDICATORI
007500900702     C***************************************************************
007600161207     C* 01    - invio anomalie ai clienti
007700161207     C* 02    - KPI   clienti
007800161207     C* 36    - SFLNXTCHG
007900920804     C* 37    - PULIZIA SUBFILE
008000920522     C* 30/35 - DI COMODO
008100920901     C* 40/57 - ERRORE
008200110912     C* 90    - ERRORE GENERICO
008300900702     C*****************************************************************
008400000000     C     *ENTRY        PLIST
008500000000     C                   PARM                    KPJBA
008600161209     c                   movel     kpjbu         tipo              1
008700950530     C*
008800110912     C                   Z-ADD     1             CODUT             1 0
008900110908     c     *dtaara       define    §azute        azuteds
009000110908     c     *dtaara       define    §datiute      ddatiute
009100110908     c                   in(E)     *dtaara
009200110908    1c                   If        %error  or RSUT = *blanks
009300110908     c                   Clear                   Tibs34ds
009400110908     c                   Call      'TIBS34R'
009500110908     c                   Parm                    Tibs34ds
009600110908     c                   In        *dtaara
009700110908    1c                   EndIf
009800110912     C                   MOVEL     rsut          PARDUT           30
009900161209     c* Verifico se si tratta di invio anomalie o kpi
010000161209     c                   if        tipo='K'
010100161209     c                   seton                                        02
010200161209     c                   else
010300161209     c                   seton                                        01
010400161209     c                   endif
010500920901     C*---------------------------------------------------------------*
010600111003     c                   eval      dataiso=datasys
010700111003     c                   eval      dateu= %dec(dataiso)
010800111003     C*---------------------------------------------------------------*
010900920730     C* ACCESSO CNACO
011000920730     C     KACO          KLIST
011100920730     C                   KFLD                    CODUT
011200110908     C                   KFLD                    dutKCI
011300920901     C                   KFLD                    KSC               7 0
011400920730     C* ACCESSO TABEL
011500920730     C     KTAB          KLIST
011600920730     C                   KFLD                    CODUT
011700920730     C                   KFLD                    COD               2
011800920730     C                   KFLD                    KEY               8
011900961030     C     KTAB2         KLIST
012000961030     C                   KFLD                    CODUT
012100961030     C                   KFLD                    COD               2
012200941228     C*---------------------------------------------------------------*
012300930125     C*
012400920730     C* CARICO TABELLA FILIALI GESTITE £1
012500920730     C                   EXSR      CARTAB
012600161213     c* Se non è utente EDP o della 166 --> uscita
012700161213     c                   if        dutpou<>166 and %subst(knmus:1:3)<>'EDP'
012800161219     c                             and *in02  and %subst(knmus:1:3)<>'ISP'
012900161213     c                   eval      vidmsg=msg(16)
013000161213     c                   seton                                        289099
013100161213     c                   endif
013200920730     C*
013300920424     C     INIZIO        TAG
013400920730     C* PULIZIA DATI SUBFILE CONTROL
013500920901     C                   SETOFF                                       04
013600110908     C                   clear                   VIddchd
013700110908     C                   clear                   VIDdcha
013800110912     C                   clear                   VIDemail
013900161212     C                   clear                   VIDok
014000161212     C                   clear                   VIDeveok
014100161216     C                   clear                   VIDcons
014200920423     C*
014300920730     C                   Z-ADD     0             NRR               4 0
014400920730     C* CARICO SUBFILE
014500920831     C                   EXSR      CARICA
014600920730     C*
014700110908     C                   WRITE     LSW1Z01
014800920730     C     FORCT         TAG
014900110908     C                   EXFMT     LSW1DCT
015000941228     C*
015100961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
015200961017     C                   CLEAR                   VIDMSG
015300110912     C                   SETOFF                                       289052
015400110912     C                   SETOFF                                       444560
015500110912     C                   SETOFF                                       5051
015600920730     C*
015700110908     C** F3 - FINE LAVORO
015800161213     c   99
015900161213     Cor KC              GOTO      FINE
016000920730     C*
016100920730     C* CONTROLLO DATI SUBFILE
016200110908     C                   EXSR      CTRSFL
016300920831     C*
016400110908     C   90              GOTO      FORCT
016500920730     C*
016600920730     C* CONTROLLO DATI SUBFILE CONTROL
016700110908     C                   EXSR      CTRCTL
016800920730     C*
016900111116     c                   if        not *inkf and not *inkh or *in90
017000111116     C                   GOTO      FORCT
017100111116     c                   endif
017200920730     C*
017300920807     C* CARICO SCHIERE PER PASSAGGIO
017400920807     C                   MOVEA     *ZEROS        LNP
017500920807     C                   MOVEA     *ZEROS        CLI
017600930520     C                   MOVEA     *ZEROS        NRS
017700920901     C                   Z-ADD     1             X                 3 0
017800920807     C                   Z-ADD     1             NRR
017900920807     C*
018000111116     C                   SETON                                        36
018100110912     C     NRR           DOWLE     20
018200920807     C*
018300110908     C     NRR           CHAIN     LSW1DSF                            31
018400920831     C     *IN31         IFEQ      '0'
018500920831     C     VIDLNP        ANDNE     0
018600920807     C                   MOVEL     VIDLNP        LNP(X)
018700930520     C                   MOVEL     VIDNRS        NRS(X)
018800920807     C                   MOVEL     VIDCLI        CLI(X)
018900920901     C                   ADD       1             X
019000111116     c                   update    lsw1dsf
019100111116     C                   ENDif
019200920807     C*
019300920807     C                   ADD       1             NRR
019400111116     C                   ENDdo
019500111116     C                   SETOff                                       36
019600920831     C*
019700110912     C     COMdcha       IFEQ      0
019800110912     C                   Z-ADD     COMDchd       COMDcha
019900980415     C                   END
020000161209     C* ANOMALIE
020100161209     c                   if        *in01
020200161209     c
020300111116     c                   if        *inkh
020400111116     c                   eval      pargest='G'
020500111116     c                   else
020600111116     c                   eval      pargest='I'
020700111116     c                   endif
020800110913     C                   MOVEL     fnlsw1ds      KPJBU
020900110912     C                   MOVEL     'LSW2'        KCOAZ
021000111116     c* gestione anomalie
021100111116     c                   if        *inkh
021200111116     c                   call      'FNLSW2R'
021300111116     C                   PARM                    KPJBA
021400111116     c
021500111116     C                   WRITE     LSW1Z01
021600111116     C                   GOTO      FORCT
021700111116     c
021800111116     c                   else
021900111207     C                   CALL      'BCH10'
022000111207     c***                call      'FNLSW2R'
022100900702     C                   PARM                    KPJBA
022200111116     c                   endif
022300161209     C
022400161209     C                   ELSE
022500161209     C* KPI
022600161209     C                   MOVEL     fnlsw1ds      KPJBU
022700161209     C                   MOVEL     'LSW7'        KCOAZ
022800161219     C                   CALL      'BCH10'
022900161219     c***                call      'FNLSW7R'
023000161209     C                   PARM                    KPJBA
023100161209     C                   ENDIF
023200920522     C*
023300950411     C                   GOTO      INIZIO
023400000000     C     FINE          TAG
023500000000     C                   SETON                                        LR
023600920730     C*
023700920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
023800920730     C     CARTAB        BEGSR
023900941228     C***
024000941228     C* CARICO TABELLA FILIALI GESTITE £1
024100941228     C***
024200941228     C                   CLEAR                   DSUL06
024300941228     C                   MOVE      '£1'          D06COD
024400941228     C                   MOVEL     SIMFEL        D06KEY
024500941228     C                   MOVEL     DSUL06        KPJBU
024600941228     C*
024700950323     C                   CALL      'TRUL06R'
024800941228     C                   PARM                    KPJBA
024900941228     C                   MOVEL     KPJBU         DSUL06
025000941228     C                   MOVEA     LIN           L1
025100920730     C                   ENDSR
025200920831     C*
025300920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
025400920730     C*
025500920831     C     CARICA        BEGSR
025600920831     C* PULIZIA SUBFILE
025700920804     C                   SETON                                        37
025800110908     C                   WRITE     LSW1DCT
025900920804     C                   SETOFF                                       37
026000920730     C*
026100110908     C* imposto sfl vuoto
026200920730     C*
026300920730     C                   Z-ADD     0             VIDLNP
026400920730     C                   MOVEL     *BLANKS       DESLNP
026500060310     C                   movel     99            VIDNRS
026600920730     C                   Z-ADD     0             VIDCLI
026700920730     C                   MOVEL     *BLANKS       DESCLI
026800920730     C*
026900110912     C                   DO        20            NRR
027000110908     C                   WRITE     LSW1DSF
027100941228     C                   ENDDO
027200920730     C*
027300920831     C                   Z-ADD     1             REC
027400920730     C                   ENDSR
027500920730     C*
027600920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
027700110908     C     CTRSFL        BEGSR
027800110908     C                   SETOFF                                       90
027900920730     C                   Z-ADD     1             NRR
028000110912     C                   clear                   sellnp            1
028100920730     C*
028200110908     C                   READC     LSW1DSF                                33
028300920730     C     *IN33         DOWEQ     '0'
028400930520     C*
028500961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
028600961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
028700961017    2C     VIDKLP        IFGT      0
028800961017     C     VIDKLP        CHAIN     AZORG01L                           34
028900961017    3C     *IN34         IFEQ      *ON
029000110908     C                   SETON                                        284490
029100961017     C                   MOVEL     MSG(1)        VIDMSG
029200961017     C                   GOTO      ENDSFL
029300961017    3C                   ENDIF
029400110908     c
029500961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
029600961017    3C     VIDKSC        IFEQ      0
029700961017     C     DESCLI        ANDEQ     *BLANKS
029800110908     C                   SETON                                        284590
029900961017     C                   MOVEL     MSG(3)        VIDMSG
030000961017     C                   GOTO      ENDSFL
030100961017    3C                   ENDIF
030200961017     C*
030300961017   X2C                   ELSE
030400961017    3C     VIDKSC        IFGT      0
030500110908     C                   SETON                                        284490
030600961017     C                   MOVEL     MSG(1)        VIDMSG
030700961017     C                   GOTO      ENDSFL
030800961017    3C                   ENDIF
030900961017    2C                   ENDIF
031000961017     C*
031100920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
031200920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
031300961017     C     VIDKSC        IFEQ      0
031400920730     C     DESCLI        ANDNE     *BLANKS
031500981109     C                   CLEAR                   PARDIT
031600920730     C*
031700920730     C* PARSTA = 9  ESCLUDO ANNULLATI
031800110908     C                   Z-ADD     dutKCI        PARKCC            4 0
031900920730     C                   Z-ADD     9             PARSTA
032000920902     C                   MOVEL     *BLANKS       DES              48
032100920902     C                   MOVEL     DESCLI        DES
032200961017     C                   MOVEL     VIDKLP        PARFLR
032300981109     C     VIDKLP        CHAIN     AZORG01L                           31
032400981109     C  N31              MOVEL     ORGDIT        PARDIT
032500000907     C                   Z-ADD     1             PAXNUM
032600000907     C                   CALL      'XALFA3BR'
032700920730     C                   PARM                    PARDUT
032800920730     C                   PARM                    CODUT
032900920902     C                   PARM                    DES
033000961016     C                   PARM                    PARKCC
033100920730     C                   PARM                    PARSTA            1 0
033200961016     C                   PARM                    PARFLR           90
033300981109     C                   PARM                    PARDIT            3
033400000907     C                   PARM                    PAXNUM            2 0
033500000907     C                   PARM                    PAXKCM           80
033600000907     C                   PARM                    PAXKSM          140
033700000907     C                   PARM                    PAXKDM           60
033800920831     C     PARSTA        IFNE      -1
033900000907     C                   MOVEL     PAXKSM        VIDCLI
034000920902     C                   MOVEL     DES           DESCLI
034100920831     C                   END
034200920831     C*
034300110908     C                   SETON                                        9045
034400110908     c                   write     lsw1Z01
034500920730     C                   GOTO      ENDSFL
034600920730     C                   END
034700920730     C*
034800920901     C*** LINEA DI PARTENZA
034900961025    1C     VIDLNP        IFNE      0
035000920901     C*
035100920901     C* CONTROLLO SE LINEA VALIDA
035200920901     C     VIDLNP        LOOKUP    L1                                     31
035300061212     c
035400061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
035500061212     c*  per poterla accettare
035600110912    2c                   if        not *in31
035700161212
035800161212     c                   if        *in01
035900110912    3c                   if        vidnrs='00' or vidcli=0
036000110912     c                             or vidnrs='SI' or vidnrs='99'
036100110908     c                   eval      vidmsg=msg(6)
036200110908     C                   SETON                                        609028
036300061212     C                   GOTO      ENDSFL
036400110912    3c                   endif
036500161212     c
036600161212     c* Per ora per KPI  nessun controllo sulla lnp
036700161212    3c                   endif
036800110912    2c                   endif
036900920901     C*
037000920901     C* DECODIFICO LINEA DI PARTENZA
037100920901     C     VIDLNP        CHAIN     AZORG                              30
037200920901     C  N30              MOVEL     ORGDES        DESLNP
037300920901     C   30              MOVEL     *BLANKS       DESLNP
037400061212     c
037500110912    2c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
037600061212     c                             or orgfva<>' '
037700110912     c                   eval      vidmsg=msg(6)
037800110912     C                   SETON                                        609028
037900110912     C                   GOTO      ENDSFL
038000061212     C                   GOTO      ENDSFL
038100110912    2c                   endif
038200110912     c                   eval      Sellnp='S'
038300920901     C*
038400961025   X1C                   ELSE
038500920902     C* LINEA DI PARTENZA = 0
038600920902     C                   MOVEL     *BLANKS       DESLNP
038700961025     C*
038800920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
038900950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
039000920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
039100110912    2c                   if        vidnrs<>'99' or vidcli>0
039200110912     c                   eval      vidmsg=msg(10)
039300110912     C                   SETON                                        609028
039400110912     C                   GOTO      ENDSFL
039500110912    2c                   endif
039600961025     C**
039700961025    1C                   END
039800060310     C*** Numero serie
039900060310     C* ESEGUO TESTN
040000110912    1c                   if        vidnrs<>'SI'
040100060310     C                   TESTN                   VIDnrs               30
040200060310     C                   MOVE      VIDnrs        W001A             1
040300060310    2C     *IN30         IFEQ      *OFF
040400060310     C     W001A         ORLT      '0'
040500110908     C                   SETON                                        599028
040600110912     C                   MOVEL     MSG(8)        VIDMSG
040700060310     C                   GOTO      ENDSFL
040800060310    2C                   ENDIF
040900110912    1C                   ENDIF
041000920730     C*
041100961025    1C     VIDCLI        IFNE      0
041200930520     C*** NUMERO SERIE
041300060310    2C     VIDNRS        IFNE      '99'
041400060605     c                   clear                   ds3c
041500930521     C                   MOVEL     '3C'          COD
041600930521     C                   MOVEL     *BLANKS       KEY
041700930521     C                   MOVEL     VIDCLI        KEY
041800930521     C     KTAB          CHAIN     TABEL                              30
041900930521     C  N30              MOVEL     TBLUNI        DS3C
042000060605     c  N30              movel     §3cnrs        alfa3cnrs
042100930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
042200970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
042300980805     C     *IN30         IFEQ      *ON
042400980805     C     §3COKD        OREQ      ' '
042500060310    3C     alfa3cnrs     ORNE      VIDNRS
042600110912     C                   SETON                                        599028
042700110912     C                   MOVEL     MSG(11)       VIDMSG
042800930521     C                   GOTO      ENDSFL
042900961025    3C                   END
043000961025    2C                   END
043100930521     C*
043200961017     C****  CODICE CLIENTE  ****
043300920730     C                   MOVEL     VIDCLI        KSC
043400920901     C     KACO          CHAIN     CNACO000                           30
043500961025    2C  N30ACOFLG        IFNE      ' '
043600920730     C* CODICE CLIENTE ANNULLATO
043700110912     C                   SETON                                        284590
043800961017     C                   MOVEL     MSG(2)        VIDMSG
043900920730     C                   GOTO      ENDSFL
044000920730     C*
044100961025   X2C                   ELSE
044200920730     C* DECODIFICO CODICE CLIENTE
044300920730     C                   MOVEL     ACORAG        DESCLI
044400961025    2C                   END
044500920730     C*
044600920730     C* CODICE CLIENTE INESISTENTE
044700961025    2C     *IN30         IFEQ      *ON
044800110912     C                   SETON                                        284590
044900961017     C                   MOVEL     MSG(3)        VIDMSG
045000961017     C                   GOTO      ENDSFL
045100961025    2C                   ENDIF
045200920730     C*
045300920730     C* CODICE CLIENTE BLOCCATO
045400130320    2C***  ACOABL        IFEQ      '8'
045500130320    2C***  ACOABL        OREQ      '*'
045600130320     c                   if        acoabl<>*blanks
045700110912     C                   SETON                                        284590
045800961017     C                   MOVEL     MSG(4)        VIDMSG
045900920730     C                   GOTO      ENDSFL
046000961025    2C                   END
046100961025     C*
046200161212     c* Se si tratta della elaborazione kpi il codice deve essere presente
046300161212     c*  nel file fiabk00f
046400161212     c                   if        *in02
046500161212     c     acoksc        chain     fiabk02l
046600161212     c                   if        not %found(fiabk02l)
046700161212     C                   SETON                                        284590
046800161212     C                   MOVEL     MSG(14)       VIDMSG
046900161212     C                   GOTO      ENDSFL
047000161212    2C                   END
047100161212     c                   endif
047200961025     C**
047300961025    1C                   END
047400161212     c* Se si tratta della elaborazione kpi codice obbligatorio
047500161212     c                   if        *in02 and vidcli=0
047600161212     C                   SETON                                        284590
047700161212     C                   MOVEL     MSG(15)       VIDMSG
047800161212     C                   GOTO      ENDSFL
047900161212     c                   endif
048000161212     c*
048100920730     C*
048200920730     C     ENDSFL        TAG
048300110912     C   90              Z-ADD     NRR           REC
048400920730     C*
048500920730     C* 36 - SFLNXTCHG
048600920730     C                   SETON                                        36
048700110908     C                   UPDATE    LSW1DSF
048800920730     C                   SETOFF                                       36
048900920730     C*
049000110912     C   90              SETON                                        33
049100110912     C  N90              READC     LSW1DSF                                33
049200920730     C                   END
049300920730     C*
049400110912     c* se non ho selezionato nulla --> errore
049500110912     c  N90              if        sellnp=' '
049600110912     C                   Z-ADD     1             REC
049700110912     C                   Z-ADD     1             nrr
049800110912     C*
049900110912     C* 36 - SFLNXTCHG
050000110912     C     nrr           chain     LSW1DSF                            33
050100110912     C                   SETON                                        36
050200110912     C                   SETON                                        286090
050300110912     C                   MOVEL     MSG(12)       VIDMSG
050400110912     C                   UPDATE    LSW1DSF
050500110912     C                   SETOFF                                       36
050600110912     c                   endif
050700110912     c
050800920730     C                   ENDSR
050900920730     C*
051000920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
051100110908     C     CtrCTL        BEGSR
051200920731     C*
051300110908     c                   clear                   comdchd
051400110908     c                   clear                   comdcha
051500110908     c
051600110908     C*** DATA chiusura dal <=data chiusura al
051700110908     C     VIDdchd       IFgt      0
051800110912     C                   MOVE      VIDDchd       G02DAT
051900920903     C                   MOVEL     *BLANKS       G02ERR
052000941228     C                   CALL      'XSRDA8'
052100920903     C                   PARM                    WLBDAT
052200920903     C     G02ERR        IFEQ      '1'
052300110908     C                   SETON                                        509028
052400110908     c                   eval      vidmsg=msg(5)
052500920903     C                   GOTO      ENDCTL
052600920903     C                   END
052700110908     c
052800110908     C                   Z-ADD     G02INV        COMDchd
052900110908     c
053000950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
053100110908     C                   Z-ADD     G02DAT        VIDdchd
053200111003     c* no maggiore di udate
053300111003     c                   if        comdchd>dateu
053400111003     C                   SETON                                        509028
053500111003     c                   eval      vidmsg=msg(13)
053600111003     C                   GOTO      ENDCTL
053700111003     C                   END
053800110908     C                   endif
053900110908     c
054000110908     C     VIDdcha       IFgt      0
054100110912     C                   MOVE      VIDDcha       G02DAT
054200110908     C                   MOVEL     *BLANKS       G02ERR
054300110908     C                   CALL      'XSRDA8'
054400110908     C                   PARM                    WLBDAT
054500110908     C     G02ERR        IFEQ      '1'
054600110908     C                   SETON                                        519028
054700110908     c                   eval      vidmsg=msg(5)
054800110908     C                   GOTO      ENDCTL
054900110908     C                   END
055000110908     c
055100110908     C                   Z-ADD     G02INV        COMDcha
055200110908     c
055300110908     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
055400110908     C                   Z-ADD     G02DAT        VIDdcha
055500111003     c* no maggiore di udate
055600111003     c                   if        comdcha>dateu
055700111003     C                   SETON                                        519028
055800111003     c                   eval      vidmsg=msg(13)
055900111003     C                   GOTO      ENDCTL
056000111003     C                   END
056100110908     c
056200110908     c                   if        comdcha<comdchd
056300110908     C                   SETON                                        519028
056400110912     c                   eval      vidmsg=msg(9)
056500110908     C                   GOTO      ENDCTL
056600110908     C                   END
056700110929     C                   endif
056800161213     c
056900161213     c* Per KPI le date sono oblogatorie
057000161213     c                   if        *in02 and VIDdchd=0
057100161213     C                   SETON                                        509028
057200161213     c                   eval      vidmsg=msg(17)
057300161213     C                   GOTO      ENDCTL
057400161213     C                   END
057500110908     c
057600110929     c* Obbligatorio inserire indirizzo e-mail
057700161209     c   01
057800161209     can kf              if        videmail=*blanks
057900110912     C                   SETON                                        529028
058000110912     c                   eval      vidmsg=msg(7)
058100110912     C                   GOTO      ENDCTL
058200111228     c                   else
058300111228     c* controllo formale
058400111228     c                   clear                   dsemail
058500120305     c                   eval      §EMLINDI=%trim(videmail)+'@brt.it'
058600111228     c                   call      'XEMAIL'
058700111228     c                   parm                    dsemail
058800111228     c                   if        §EMLERRO<>*blanks and
058900111228     c                             §EMLERRO<> '0'
059000111228     C                   SETON                                        529028
059100111228     c                   eval      vidmsg=§EMLmsgO
059200111228     C                   GOTO      ENDCTL
059300110912     c                   endif
059400111228     c                   endif
059500110912     c
059600920731     C     ENDCTL        ENDSR
059700961017**  MSG
059800961017Immettere linea di partenza valida                                            1
059900961017Codice Cliente annullato                                                      2
060000961017Codice Cliente errato                                                         3
060100961017Codice Cliente bloccato                                                       4
060200110908Data errata                                                                   5
060300110908Linea partenza errata o non gestibile                                         6
060400110912Inserire indirizzo e-mail BRT                                                 7
060500110912Serie errata                                                                  8
060600110912Data  AL  deve essere maggiore della data  DAL                                9
060700111003Se immessa SERIE o CLIENTE immettere anche la LINEA di PARTENZA               10
060800111003La serie non è quella assegnata al cliente                                    11
060900110912Effettuare almeno una selezione
061000111003La data non può essere maggiore della data del giorno                         13
061100161212Cliente non abilitato alla elaborazione  in oggetto                           14
061200161212Cliente obbligatorio per l'elaborazine del KPI                                15
061300161213Programma in TEST soltanto alla filiale 166. Enter esce                       16
061400161213Immettere la data spedizione                                                  17
