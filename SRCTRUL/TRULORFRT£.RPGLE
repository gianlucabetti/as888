000100980304     H*------------------------------------------------------------------------*
000200140210     H* verifica colli spuntati - TEST
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800140310     FFNARB01L  IF   E           K DISK
000900140311     FFNblp01L  IF   E           K DISK    PREFIX(arb:3)
001000140310     Ffipct02l  IF   E           K DISK
001100140310     Ftrulorsdt CF   E             WORKSTN INFDS(DEVDS1)
001200140310     F                                     SFILE(ULORSTDMS:MSGULT)
001300980304     D*------------------------------------------------------------------------*
001400980304     D* SCHIERE
001500980304     D*------------------------------------------------------------------------*
001600980304     d MSG             S             80    DIM(10) CTDATA PERRCD(1)
001700980304     D*------------
001800980304     D* DESCRIZIONI TASTI FUNZIONALI
001900980304     D*------------
002000980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100980304     D*------------
002200980304     D* FUNZIONI DA CARICARE A VIDEO
002300980304     D*------------
002400980304     D FUV             S             19    DIM(24)
002500980304     D*------------------------------------------------------------------------*
002600980304     D* INPUT
002700980304     D*------------------------------------------------------------------------*
002800980304     D*------------
002900980304     D* RIDEFINIZIONE FUNZIONI VIDEO
003000980304     D*------------
003100980304     D FUDDS           DS
003200980304     D  FUDDE1                 1      1
003300980304     D  FUDFUN                 2      3
003400980304     D  FUDDE2                 4     19
003500980304     D  FUDFI0                20     34
003600980304     D  FUDATT                35     35
003700980304     D  FUDFI1                36     80
003800980304     D  FUDDES                 1     19
003900980304     D  FUDRIG                 1     80
004000980304     D*------------
004100980304     D* DS GESTIONE TASTI FUNZIONALI
004200980304     D*------------
004300980304     D DSKEY         E DS
004400980304     D DEVDS1          DS
004500980304     D  KEY                  369    369
004600980304     D*------------
004700980304     D* DS "XMSG" - GESTIONE MESSAGGI
004800980304     D*------------
004900980304     D DSMSG         E DS
005000140317     D DSSI00        E DS                  EXTNAME(TISI00DS)
005100980304     D*------------------
005200980304     D* DS di procedura
005300980304     D*------------------
005400140317     d trulorsds     E DS
005500140310     d trulor2ds     E DS
005600140310     d fipctcords    e ds
005700140310     d tnsd99ds      e ds
005800140317     D DSSI95        E DS                  EXTNAME(TISI95DS)
005900140317     D fnlv13ds      E DS
006000140317     d
006100140310     d
006200140310     D wrkoggiiso      s               d
006300140317     d s_arbdei        S                   LIKE(d98dei)
006400140317     d s_arboei        S                   LIKE(d98oei)
006500140310     d
006600980304     D*------------------
006700980304     D* ARCHITETTURA
006800980304     D*------------------
006900980304     D KPJBA         E DS
007000980304     C*------------------------------------------------------------------------*
007100980304     C* MAIN LINES
007200980304     C*------------------------------------------------------------------------*
007300140310     c     kbolla        klist
007400140310     c                   kfld                    vidaas
007500140310     c                   kfld                    vidlnp
007600140310     c                   kfld                    vidnrs
007700140310     c                   kfld                    vidnsp
007800140310     C     Kcet          KLIST
007900140310     C                   KFLD                    pctfgs
008000140310     C                   KFLD                    pctNDC
008100140310     C                   KFLD                    arbpdc
008200140310     C                   KFLD                    TTRD              3
008300140310     C                   KFLD                    arbAAS
008400140310     C                   KFLD                    arbLNP
008500140310     C                   KFLD                    arbNRS
008600140310     C                   KFLD                    arbNSP
008700140310     c
008800980304     C*
008900980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009000980304DO  1C     F03L00        DOUEQ     '1'
009100980304     C     F12L00        OREQ      '1'
009200980304     C     FINL00        OREQ      '1'
009300980304     C*
009400980304     C* EMETTE FORMATO VIDEO
009500980304     C                   EXSR      BUTFMR
009600980304     C*
009700980304     C* GESTIONE TASTI FUNZIONALI
009800980304     C                   EXSR      FUNCON
009900980304E   1C                   ENDDO
010000140310     c
010100980304     C*
010200980304     C                   SETON                                        LR
010300980304     C*------------------------------------------------------------------------*
010400980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010500980304     C*------------------------------------------------------------------------*
010600980304     C     BUTFMR        BEGSR
010700980304     C*
010800980304     C* EMETTE LA TESTATA
010900140310     C                   WRITE     ULORSTD01
011000980304     C*
011100980304     C* SPEDISCE EVENTUALE MESSAGGIO
011200980304     C                   EXSR      SNDMSG
011300980304     C*
011400980304     C* EMETTE LE FUNZIONI
011500140310     C                   WRITE     ULORSTD02
011600980304     C*
011700980304     C* EMETTE IL VIDEO
011800140310     C                   EXFMT     ULORSTD03
011900980304     C*
012000980304     C* REIMPOSTA FLAG DI ERRORE
012100980304     C                   MOVEL     '0'           ERRL00
012200980304     C*
012300980304     C* REIMPOSTA INDICATORI DI ERRORE
012400980304     C                   SETOFF                                       59
012500980304     C                   SETOFF                                       606162
012600980304     C                   SETOFF                                       636465
012700980304     C                   SETOFF                                       666768
012800980304     C                   SETOFF                                       697071
012900980304     C                   SETOFF                                       727374
013000980304     C                   SETOFF                                       757677
013100980304     C                   SETOFF                                       787980
013200980304     C                   SETOFF                                       818283
013300980304     C                   SETOFF                                       848586
013400980304     C                   SETOFF                                       878889
013500980304     C*
013600980304     C                   ENDSR
013700980304     C*------------------------------------------------------------------------*
013800980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013900980304     C*------------------------------------------------------------------------*
014000980304     C     SNDMSG        BEGSR
014100980304     C*
014200980304     C* PULISCE IL SFILE MESSAGGI
014300980304     C                   MOVEL     '1'           *IN40
014400140310     C                   WRITE     ULORSTDMC
014500980304     C                   MOVEL     '0'           *IN40
014600980304     C*
014700980304     C* VISUALIZZA IL MESSAGGIO
014800980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
014900980304     C                   CALL      'XMSG'        XMSGPA
015000980304IF  1C     DSMSER        IFEQ      *ZEROS
015100980304     C                   MOVEL     DSMSKE        MSGKEY
015200980304E   1C                   ENDIF
015300980304     C                   Z-ADD     1             MSGULT
015400140310     C                   WRITE     ULORSTDMS
015500140310     C                   WRITE     ULORSTDMC
015600980304     C*
015700980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015800980304     C                   MOVEL     *BLANKS       DSMSMS
015900980304     C*
016000980304     C                   ENDSR
016100980304     C*------------------------------------------------------------------------*
016200980304     C* ENTGES - GESTIONE ENTER -> INVIO
016300980304     C*------------------------------------------------------------------------*
016400980304     C     ENTGES        BEGSR
016500140415     c                   setoff                                       1011
016600140317     c* controllo RITIRO o consegna
016700140317    1c                   if        (vidaas<>0 or vidlnp<>0 or vidnrs<>0 or
016800140317     c                                vidnsp<>0) and vidrit='S'
016900140317     C                   MOVEL     MSG(2)        DSMSMS
017000140317     c                   leavesr
017100140317    1c                   endif
017200140415     c                   if        vidaas=0 and vidlnp=0 and vidnsp=0 and
017300140415     c                             vidrit=' '
017400140415     C                   MOVEL     MSG(2)        DSMSMS
017500140415     c                   leavesr
017600140415    1c                   endif
017700140317     c
017800140317     c* Cerco località per i ritiri
017900140317    1c                   if        %scan('?':ioreloc) >0
018000140317     C                   CLEAR                   DSSI00
018100140317     C                   MOVEL     'L01'         D00OP0
018200140317     C                   MOVEL     iorecap       D00CAP
018300140317     C                   CALL      'TISI30R'
018400140317     C                   PARM                    KPJBA
018500140317     C                   PARM                    DSSI00
018600140317IF  2C     D00ERR        IFNE      '1'
018700140317     C                   MOVEL     D00CAP        iorecap                        *CAP
018800140317     C                   MOVEL     D00LOC        ioreloc                        *LOCALITA'
018900140317E   2C                   ENDIF
019000140317     c                   leavesr
019100140317    1c                   endif
019200140317     c
019300140317    1c                   if        iorecap<>*blanks or ioreloc<>*blanks
019400140317     C                   CLEAR                   DSSI95
019500140317     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
019600140317     C                   MOVEL     '3'           I95TCN                         *CONTROLLO
019700140317     C                   MOVEL     iorecap       I95CAP                         *CAP
019800140317     C                   MOVEL     ioreloc       I95LOC                         *LOCALITA'
019900140317     c* Tipo bolla
020000140317     C                   MOVEL     'F'           I95TPO
020100140317     C* ERRORE AFFIDABILITA = 0
020200140317     C                   MOVEL     ' '           I13AF0
020300140317     C                   CALL      'FNLV13R'
020400140317     C                   PARM                    KPJBA
020500140317     C                   PARM                    fnlv13ds
020600140317     C                   PARM                    DSSI95
020700140317     C                   PARM      ' '           flgbac            1
020800140317IF  2C     O95ERR        IFNE      *BLANKS
020900140317     c
021000140317IF   C     o13err        orNE      *BLANKS
021100140317     c     o13rop        andeq     *blanks
021200140317
021300140317     C     O95LIA        OREQ      '1'
021400140317     c
021500140317     C     ioreloc       ORNE      *BLANKS
021600140317     C     O95LID        ANDLT     '3'
021700140317     C                   MOVEL     MSG(3)        DSMSMS
021800140317     c                   leavesr
021900140317E   2C                   ENDIF
022000140317E   1C                   ENDIF
022100140317     c
022200140317     c* Se la bolla è uguale a quella di prima non pulisco video,
022300140317     c*  altrimenti pulisco campi di output
022400140317    1c                   if        vidaas<>arbaas or vidlnp<>arblnp or
022500140317     c                             vidnsp<>arbnsp  or vidnrs<>arbnrs
022600140317     c                   clear                   trulor2ds
022700140317    1c                   endif
022800140317     c
022900140317    1c                   if        vidnsp >0
023000140415     c                   seton                                        10
023100140317
023200140317    2c                   if        vidtbO<>'P'
023300140310     c     kbolla        chain     fnarb01l
023400140314     c                   else
023500140314     c     kbolla        chain     fnblp01l
023600140317    2c                   endif
023700140314     c
023800140317    2c                   if        (vidtbo<>'P' and %found(fnarb01l)) or
023900140314     c                             (vidtbo= 'P' and %found(fnblp01l))
024000140310     c                   clear                   tnsd99ds
024100140310     C                   Z-ADD     0             amgDCE            8 0
024200140310     C                   MOVEL     'L'           D98TLA
024300140314     c                   if        vidtbo<>'P'
024400140310     C                   MOVEL     'A'           D98TBO
024500140314     c                   else
024600140314     C                   MOVEL     'P'           D98TBO
024700140314     c                   endif
024800140310     C                   MOVEL     ARBAAS        D98AAS
024900140310     C                   MOVEL     ARBLNP        D98LNP
025000140310     C                   MOVEL     ARBNRS        D98NRS
025100140310     C                   MOVEL     ARBNSP        D98NSP
025200140310     C*
025300140310     C                   CALL      'TNSD99R'
025400140310     C                   PARM                    TNSD99DS
025500140310     c
025600140310     c* Mi salvo data teorica consegna
025700140310     c                   eval      s_arbdei=d98dei
025800140310     c                   eval      s_arboei=d98oei
025900140317    3c                   if        d98dce>0 and arbdce=0
026000140310     c                   eval      amgdce=d98dce
026100140317   x3C                   ELSE
026200140310     c                   eval      amgdce=arbdce
026300140317    3c                   endif
026400140310     C*
026500140317     c                   eval       IOREtfp = ARBtfp
026600140317     c                   eval       IOREtfa = ARBtfa
026700140317     c                   eval       IOREdti = ARBdti
026800140317     c                   eval       IOREhti = ARBhti
026900140317     c                   eval       IOREdcr = ARBdcr
027000140317     c                   eval       IOREhcr = ARBhcr
027100140317     c                   eval       IOREtcr = ARBtcr
027200140317     c                   eval       IOREDEI = s_ARBdei
027300140317     c                   eval       IOREOEI = s_ARBoei
027400140317     c                   eval       IOREfil = ARBlna
027500140317     c                   eval       IOREcap = ARBcad
027600140317     c                   eval       IOREloc = ARBlod
027700140317     c                   eval       IOREnar = ARBnzd
027800140310     c* Data di Riferimento: data consegna se consegnata
027900140317    3c                   select
028000140314     c                   when      vidtbo='P' and arbdcm>0
028100140317     c                   eval       ioredta=arbdcm
028200140314     c                   when      vidtbo='P' and arbddc>0 and arbndc>0
028300140317     c                   eval       ioredta=arbDDC
028400140314
028500140314     c                   when      vidtbo<>'P' and arbdcm>0 and arbica<>' ' and arbica<>'R'
028600140314     c                             arbica<>' ' and arbica<>'R'
028700140310     c                             and arbicc<>' ' and arbicc<>'R'
028800140317     c                   eval       ioredta=arbdcm
028900140314     c*
029000140314     c                   when      vidtbo<>'P' and arbddc>0 and arbndc>0
029100140317     c                   eval       ioredta=arbDDC
029200140310     c                   other
029300140310     c* altrimenti data prevista consegna se > = oggi
029400140310     C                   time                    wrkoggiiso
029500140317     C     *iso          move      wrkoggiiso    ioredta
029600140310     c* se < di oggi, imposto oggi
029700140317     c                   if        amgdce>ioredta
029800140317     c                   eval       ioredta=amgdce
029900140310     c                   endif
030000140317    3c                   endsl
030100140310     c
030200140317     c                   eval       IOREtser = 'C'
030300140317    3c                   if          vidtbo<>'P' and
030400140314     c                              (ARBfbc = *blanks and
030500140310     c                               ARBddc > 0 and
030600140310     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
030700140310     c                               arbica='R' or arbicc=' ' or arbicc='R'))
030800140317     c                   eval       IOREcons = 'S'
030900140317     c                   eval       IOREDDC=arbddc
031000140317     c                   eval       IOREnDC=arbndc
031100140310     c* Orario presunto ritiro/consegna
031200140310     c                   move      arbndc        pctndc
031300140310     c                   move      arbifp        pctfgs
031400140310     c                   move      'COR'         ttrd
031500140310     c     kcet          setgt     fipct02l
031600140310     c     kcet          readpe    fipct02l
031700140317    4c                   if        not %eof(fipct02l)
031800140310     c                   movel     pctdati       fipctcords
031900140317     c                   eval      ioreorp=§PCTORASTI
032000140317    4c                   endif
032100140310     c
032200140317    3c                   ENDIF
032300140310     c
032400140317     c                   eval       IOREtsp = ARBtsp
032500140311     c* se impostato tipo servizio prendo quello
032600140317    3c                   if        vidtsp<>' '
032700140317     c                   eval       IOREtsp = vidtsp
032800140317    3c                   endif
032900140317     c* ora stimata forzata
033000140317    3c                   if        vidorp>0
033100140317     c                   eval      ioreorp=vidorp
033200140317    3c                   endif
033300140317     c
033400140311     c* se non in cosnegna azzero i campi legati
033500140317    3c                   if        vidnoc='N'
033600140317     c                   clear                   iorecons
033700140317     c                   clear                   iorendc
033800140317     c                   clear                   ioreddc
033900140317    3c                   endif
034000140310     c
034100980304     C*
034200140317    3c                   if        vidtipo='V'
034300140317     c                   eval      IOREcol = 35
034400140317     c                   eval      IORErig = 8
034500140317     c
034600140317     c                   write     ULORSTD03
034700140317     c
034800140310     c                   CALL      'TRULORSR'
034900140310     c                   PARM                    kpjba
035000140310     c                   PARM                    trulorsds
035100140311     c
035200140317   x3c                   else
035300140311     c*
035400140310     c                   CALL      'TRULORSR'
035500140310     c                   PARM                    kpjba
035600140310     c                   PARM                    trulorsds
035700140311     c                   PARM                    trulor2ds
035800140317    3c                   endif
035900140310     c
036000140317   x2c                   else
036100140310     c                   eval      OOR2MSG='non trovata bolla in ARB'
036200140317    2c                   endif
036300140317   x1c                   else
036400140317     c
036500140415     c                   seton                                        11
036600140317     c                   clear                   oor2msg
036700140317     c                   eval       IOREtser = 'R'
036800140317     c* se impostato tipo servizio prendo quello
036900140317    2c                   if        vidtsp<>' '
037000140317     c                   eval       IOREtsp = vidtsp
037100140317    2c                   endif
037200140317     c* ora stimata forzata
037300140317    2c                   if        vidorp>0
037400140317     c                   eval      ioreorp=vidorp
037500140317    2c                   endif
037600140317     c
037700140317    2c                   if        vidtipo='V'
037800140317     c                   eval      IOREcol = 35
037900140317     c                   eval      IORErig = 8
038000140317     c                   CALL      'TRULORSR'
038100140317     c                   PARM                    kpjba
038200140317     c                   PARM                    trulorsds
038300140317     c
038400140317   x2c                   else
038500140317     c*
038600140317     c                   CALL      'TRULORSR'
038700140317     c                   PARM                    kpjba
038800140317     c                   PARM                    trulorsds
038900140317     c                   PARM                    trulor2ds
039000140317    2c                   endif
039100140317    1c                   endif
039200980304     C*
039300980304     C                   ENDSR
039400980304     C*------------------------------------------------------------------------*
039500980304     C* F03GES - GESTIONE F03 -> FINE
039600980304     C*------------------------------------------------------------------------*
039700980304     C     F03GES        BEGSR
039800980304     C*
039900980304     C* IMPOSTA "PREMUTO F03"
040000980304     C                   MOVEL     '1'           F03L00
040100980304     C*
040200980304     C                   ENDSR
040300980304     C*------------------------------------------------------------------------*
040400980304     C* F12GES - GESTIONE F12 -> RITORNO
040500980304     C*------------------------------------------------------------------------*
040600980304     C     F12GES        BEGSR
040700980304     C*
040800980304     C* IMPOSTA "PREMUTO F12"
040900980304     C                   MOVEL     '1'           F12L00
041000980304     C*
041100980304     C                   ENDSR
041200140317     C*------------------------------------------------------------------------*
041300140317     C* F10GES - pulisce
041400140317     C*------------------------------------------------------------------------*
041500140317     C     F10GES        BEGSR
041600140317     C*
041700140415     c                   setoff                                       1011
041800140317     c                   clear                   trulorsds
041900140317     c                   clear                   trulor2ds
042000140317     c                   clear                   vidtsp
042100140317     c                   clear                   vidtbo
042200140317     c                   clear                   vidaas
042300140317     c                   clear                   vidlnp
042400140317     c                   clear                   vidnrs
042500140317     c                   clear                   vidnsp
042600140317     c                   clear                   vidrit
042700140317     c                   clear                   vidtipo
042800140317     c                   clear                   vidnoc
042900140317     c                   clear                   vidorp
043000140317     C*
043100140317     C                   ENDSR
043200980304     C*------------------------------------------------------------------------*
043300980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
043400980304     C*------------------------------------------------------------------------*
043500980304     C     F24GES        BEGSR
043600980304     C*
043700980304     C                   EXSR      FUNGES
043800980304     C*
043900980304     C                   ENDSR
044000980304     C*------------------------------------------------------------------------*
044100980304     C* *INZSR - OPERAZIONI INIZIALI
044200980304     C*------------------------------------------------------------------------*
044300980304     C     *INZSR        BEGSR
044400980304     C*---
044500980304     C* RICEVIMENTO PARAMETRI
044600980304     C*---
044700980304     C     *ENTRY        PLIST
044800980304     C                   PARM                    KPJBA
044900980304     C*
045000980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
045100980304     C     XMSGPA        PLIST
045200980304     C                   PARM                    DSMSMS
045300980304     C                   PARM                    DSMSPG
045400980304     C                   PARM                    DSMSKE
045500980304     C                   PARM                    DSMSER
045600980304     C*
045700980304     C* IMPOSTA LA DS -TASTI PREMUTI-
045800980304     C                   CALL      'XKEY'
045900980304     C                   PARM                    DSKEY
046000980304     C*---
046100980304     C* VARIABILI NON RIFERITE AL DATA BASE
046200980304     C*---
046300980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
046400980304     C*---
046500980304     C* VARIABILI DI CONTROLLO
046600980304     C*---
046700980304     C                   MOVEL     '0'           FINL00            1
046800980304     C                   MOVEL     '0'           F03L00            1
046900980304     C                   MOVEL     '0'           F12L00            1
047000980304     C                   MOVEL     '0'           ERRL00            1
047100980304     C*
047200980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
047300140310     C                   MOVEL     'TRULORSR'    DSMSPG
047400140310     C                   MOVEL     'TRULORSR'    MSGQUE
047500980304     C                   MOVEL     *BLANKS       MSGKEY
047600980304     C                   MOVEL     *BLANKS       DSMSMS
047700980304     C*
047800980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
047900980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
048000980304     C                   EXSR      FUNGES
048100980304     C*
048200980304     C* IMPOSTA IL VIDEO
048300140310     c                   CLEAR                   TRULORSds
048400140310     c                   CLEAR                   TRULOR2ds
048500140317     c                   CLEAR                   arbaas
048600140317     c                   CLEAR                   arblnp
048700140317     c                   CLEAR                   arbnrs
048800140317     c                   CLEAR                   arbnsp
048900980304     C*
049000980304     C                   ENDSR
049100980304     C*------------------------------------------------------------------------*
049200980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
049300980304     C*------------------------------------------------------------------------*
049400980304     C     FUNGES        BEGSR
049500980304     C*
049600980304     C* PULISCO CAMPI DI LAVORO
049700980304     C                   Z-ADD     0             W                 5 0
049800980304     C*
049900980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
050000980304     C* E CARICO TABELLA FUNZIONI ATTIVE
050100980304     C     1             DO        24            I
050200980304     C                   MOVEL     FUD(I)        FUDRIG
050300980304     C*
050400980304     C* PULIZIA FUNZIONI PRECEDENTI
050500980304     C                   MOVEL     *BLANKS       FUV(I)
050600980304     C*
050700980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
050800980304     C* (SALTA F24)
050900980304     C     FUDATT        IFEQ      'S'
051000980304     C     FUDFUN        ANDNE     '24'
051100980304     C                   ADD       1             W
051200980304     C                   MOVEL     FUDDES        FUV(W)
051300980304     C                   END
051400980304     C                   END
051500980304     C*
051600980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
051700980304     C* ALTRIMENTI DISATTIVO F24
051800980304     C     1             DO        24            I
051900980304     C                   MOVEL     FUD(I)        FUDRIG
052000980304     C*
052100980304     C     FUDFUN        IFEQ      '24'
052200980304     C     W             IFGT      6
052300980304     C                   MOVEL     'S'           FUDATT
052400980304     C                   ELSE
052500980304     C                   MOVEL     *BLANKS       FUDATT
052600980304     C                   END
052700980304     C                   MOVEL     FUDRIG        FUD(I)
052800980304     C                   END
052900980304     C                   END
053000980304     C*
053100980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
053200980304     C     W             DIV       6             I
053300980304     C                   MVR                     Z
053400980304     C*
053500980304     C     Z             IFNE      0
053600980304     C     I             ADD       1             Z
053700980304     C                   ELSE
053800980304     C                   Z-ADD     I             Z                 5 0
053900980304     C                   END
054000980304     C*
054100980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
054200980304     C                   ADD       1             FUNGRU
054300980304     C     FUNGRU        IFGT      Z
054400980304     C     FUNGRU        ORLT      1
054500980304     C                   Z-ADD     1             FUNGRU
054600980304     C                   END
054700980304     C*
054800980304     C* PRIMO GRUPPO DI 6 FUNZIONI
054900980304     C     FUNGRU        IFEQ      1
055000980304     C                   MOVEL     FUV(1)        FUN001
055100980304     C                   MOVEL     FUV(2)        FUN002
055200980304     C                   MOVEL     FUV(3)        FUN003
055300980304     C                   MOVEL     FUV(4)        FUN004
055400980304     C                   MOVEL     FUV(5)        FUN005
055500980304     C                   MOVEL     FUV(6)        FUN006
055600980304     C                   END
055700980304     C*
055800980304     C* SECONDO GRUPPO DI 6 FUNZIONI
055900980304     C     FUNGRU        IFEQ      2
056000980304     C                   MOVEL     FUV(7)        FUN001
056100980304     C                   MOVEL     FUV(8)        FUN002
056200980304     C                   MOVEL     FUV(9)        FUN003
056300980304     C                   MOVEL     FUV(10)       FUN004
056400980304     C                   MOVEL     FUV(11)       FUN005
056500980304     C                   MOVEL     FUV(12)       FUN006
056600980304     C                   END
056700980304     C*
056800980304     C* TERZO GRUPPO DI 6 FUNZIONI
056900980304     C     FUNGRU        IFEQ      3
057000980304     C                   MOVEL     FUV(13)       FUN001
057100980304     C                   MOVEL     FUV(14)       FUN002
057200980304     C                   MOVEL     FUV(15)       FUN003
057300980304     C                   MOVEL     FUV(16)       FUN004
057400980304     C                   MOVEL     FUV(17)       FUN005
057500980304     C                   MOVEL     FUV(18)       FUN006
057600980304     C                   END
057700980304     C*
057800980304     C* QUARTO GRUPPO DI 6 FUNZIONI
057900980304     C     FUNGRU        IFEQ      4
058000980304     C                   MOVEL     FUV(19)       FUN001
058100980304     C                   MOVEL     FUV(20)       FUN002
058200980304     C                   MOVEL     FUV(21)       FUN003
058300980304     C                   MOVEL     FUV(22)       FUN004
058400980304     C                   MOVEL     *BLANKS       FUN005
058500980304     C                   MOVEL     *BLANKS       FUN006
058600980304     C                   END
058700980304     C*
058800980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
058900980304     C                   MOVEL     *BLANKS       FUN007
059000980304     C                   MOVEL     *BLANKS       FUN008
059100980304     C*
059200980304     C     1             DO        24            I
059300980304     C                   MOVEL     FUD(I)        FUDRIG
059400980304     C     FUDFUN        IFEQ      '24'
059500980304     C     FUDATT        ANDEQ     'S'
059600980304     C                   MOVEL     FUDDES        FUN008
059700980304     C                   END
059800980304     C                   END
059900980304     C*
060000980304     C                   ENDSR
060100980304     C*------------------------------------------------------------------------*
060200980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
060300980304     C*------------------------------------------------------------------------*
060400980304     C     FUNCON        BEGSR
060500980304     C*
060600980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
060700980304     C                   EXSR      FUNVAR
060800980304     C*
060900980304     C* PULISCO CAMPI DI LAVORO
061000980304     C                   Z-ADD     0             I                 5 0
061100980304     C                   Z-ADD     0             W                 5 0
061200980304     C*
061300980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
061400980304     C     FUNKEY        IFNE      *BLANKS
061500980304     C     1             DO        24            I
061600980304     C                   MOVEL     FUD(I)        FUDRIG
061700980304     C*
061800980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
061900980304     C     FUNKEY        IFEQ      FUDFUN
062000980304     C     FUDATT        ANDEQ     'S'
062100980304     C                   Z-ADD     I             W
062200980304     C                   END
062300980304     C                   END
062400980304     C*
062500980304     C* FUNZIONE NON ATTIVA (ERRORE)
062600980304     C     W             IFEQ      0
062700980304     C*
062800980304     C* EMISSIONE MESSAGGIO DI ERRORE
062900980304     C                   MOVEL     MSG(1)        DSMSMS
063000980304     C*
063100980304     C                   END
063200980304     C                   END
063300980304     C*
063400980304     C* FUNZIONE ATTIVA (PROCEDI ...)
063500980304     C     W             IFNE      0
063600980304     C     FUNKEY        OREQ      *BLANKS
063700980304     C     FUNKEY        IFEQ      *BLANKS
063800980304     C                   EXSR      ENTGES
063900980304     C                   END
064000980304     C     FUNKEY        IFEQ      '03'
064100980304     C                   EXSR      F03GES
064200980304     C                   END
064300140317     C     FUNKEY        IFEQ      '10'
064400140317     C                   EXSR      F10GES
064500980304     C                   END
064600140317     C     FUNKEY        IFEQ      '12'
064700140317     C                   EXSR      F12GES
064800140317     C                   END
064900980304     C                   END
065000980304     C*
065100980304     C                   ENDSR
065200980304     C*------------------------------------------------------------------------*
065300980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
065400980304     C*------------------------------------------------------------------------*
065500980304     C     AZZVID        BEGSR
065600980304     C*
065700980304     C                   ENDSR
065800980304     C*------------------------------------------------------------------------*
065900980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
066000980304     C*------------------------------------------------------------------------*
066100980304     C     FUNVAR        BEGSR
066200980304     C*
066300980304     C* INIZIALIZZA VARIABILE -FUNKEY-
066400980304     C*
066500980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
066600980304     C     KEY           IFEQ      ENTER
066700980304     C                   MOVEL     *BLANKS       FUNKEY
066800980304     C                   END
066900980304     C     KEY           IFEQ      F1
067000980304     C                   MOVEL     '01'          FUNKEY
067100980304     C                   END
067200980304     C     KEY           IFEQ      F2
067300980304     C                   MOVEL     '02'          FUNKEY
067400980304     C                   END
067500980304     C     KEY           IFEQ      F3
067600980304     C                   MOVEL     '03'          FUNKEY
067700980304     C                   END
067800980304     C     KEY           IFEQ      F4
067900980304     C                   MOVEL     '04'          FUNKEY
068000980304     C                   END
068100980304     C     KEY           IFEQ      F5
068200980304     C                   MOVEL     '05'          FUNKEY
068300980304     C                   END
068400980304     C     KEY           IFEQ      F6
068500980304     C                   MOVEL     '06'          FUNKEY
068600980304     C                   END
068700980304     C     KEY           IFEQ      F7
068800980304     C                   MOVEL     '07'          FUNKEY
068900980304     C                   END
069000980304     C     KEY           IFEQ      F8
069100980304     C                   MOVEL     '08'          FUNKEY
069200980304     C                   END
069300980304     C     KEY           IFEQ      F9
069400980304     C                   MOVEL     '09'          FUNKEY
069500980304     C                   END
069600980304     C     KEY           IFEQ      F10
069700980304     C                   MOVEL     '10'          FUNKEY
069800980304     C                   END
069900980304     C     KEY           IFEQ      F11
070000980304     C                   MOVEL     '11'          FUNKEY
070100980304     C                   END
070200980304     C     KEY           IFEQ      F12
070300980304     C                   MOVEL     '12'          FUNKEY
070400980304     C                   END
070500980304     C     KEY           IFEQ      F13
070600980304     C                   MOVEL     '13'          FUNKEY
070700980304     C                   END
070800980304     C     KEY           IFEQ      F14
070900980304     C                   MOVEL     '14'          FUNKEY
071000980304     C                   END
071100980304     C     KEY           IFEQ      F15
071200980304     C                   MOVEL     '15'          FUNKEY
071300980304     C                   END
071400980304     C     KEY           IFEQ      F16
071500980304     C                   MOVEL     '16'          FUNKEY
071600980304     C                   END
071700980304     C     KEY           IFEQ      F17
071800980304     C                   MOVEL     '17'          FUNKEY
071900980304     C                   END
072000980304     C     KEY           IFEQ      F18
072100980304     C                   MOVEL     '18'          FUNKEY
072200980304     C                   END
072300980304     C     KEY           IFEQ      F19
072400980304     C                   MOVEL     '19'          FUNKEY
072500980304     C                   END
072600980304     C     KEY           IFEQ      F20
072700980304     C                   MOVEL     '20'          FUNKEY
072800980304     C                   END
072900980304     C     KEY           IFEQ      F21
073000980304     C                   MOVEL     '21'          FUNKEY
073100980304     C                   END
073200980304     C     KEY           IFEQ      F22
073300980304     C                   MOVEL     '22'          FUNKEY
073400980304     C                   END
073500980304     C     KEY           IFEQ      F23
073600980304     C                   MOVEL     '23'          FUNKEY
073700980304     C                   END
073800980304     C     KEY           IFEQ      F24
073900980304     C                   MOVEL     '24'          FUNKEY
074000980304     C                   END
074100980304     C     KEY           IFEQ      ROLLUP
074200980304     C                   MOVEL     'RU'          FUNKEY
074300980304     C                   END
074400980304     C     KEY           IFEQ      HELP
074500980304     C                   MOVEL     'HP'          FUNKEY
074600980304     C                   END
074700980304     C                   ENDSR
074800980304     C*------------------------------------------------------------------------*
074900980304     C* NOTE COSTRUZIONE SCHIERE
075000980304     C*------------------------------------------------------------------------*
075100980304     C*
075200980304     C* -MSG-MESSAGGI
075300980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
075400980304     C* (NON TOCCARE MSG,1 E MSG,2)
075500980304     C*
075600980304     C* -CMD-COMANDI
075700980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
075800980304     C*
075900980304     C* -OPD-OPZIONI
076000980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
076100980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
076200980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
076300980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
076400980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
076500980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
076600980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
076700980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
076800980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
076900980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
077000980304     C*
077100980304     C* -FUD-FUNZIONI
077200980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
077300980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
077400980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
077500980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
077600980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
077700980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
077800980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
077900980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
078000980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
078100980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
078200980304     o*-------------------------------------------------------------------------
078300980304** msg - messaggi
078400140317Funzione non attiva
078500140317Immettere per consegne o per ritiri
078600140317CAP o località errati
078700140210fnlrj6R- 4
078800140210fnlrj6R- 5
078900140210fnlrj6R- 6
079000140210fnlrj6R- 7
079100140210LRLRj6R- 8
079200140210LRLRj6R- 9
079300140210LRLRj6R- 10
079400980304** FUD - FUNZIONI
079500980304                   -        (S/N)=N                        funz.01¬
079600980304                   -        (S/N)=N                        funz.02¬
079700980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
079800980304                   -        (S/N)=N                        funz.04¬
079900980304                   -        (S/N)=N                        funz.05¬
080000980304                   -        (S/N)=N                        funz.06¬
080100980304                   -        (S/N)=N                        funz.07¬
080200980304                   -        (S/N)=N                        funz.08¬
080300980304                   -        (S/N)=N                        funz.09¬
080400140317F10=Pulisce        -        (S/N)=S                        funz.10¬
080500980304                   -        (S/N)=N                        funz.11¬
080600980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
080700980304                   -        (S/N)=N                        funz.13¬
080800980304                   -        (S/N)=N                        funz.14¬
080900980304                   -        (S/N)=N                        funz.15¬
081000980304                   -        (S/N)=N                        funz.16¬
081100980304                   -        (S/N)=N                        funz.17¬
081200980304                   -        (S/N)=N                        funz.18¬
081300980304                   -        (S/N)=N                        funz.19¬
081400980304                   -        (S/N)=N                        funz.20¬
081500980304                   -        (S/N)=N                        funz.21¬
081600980304                   -        (S/N)=N                        funz.22¬
081700980304                   -        (S/N)=N                        funz.23¬
081800980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
