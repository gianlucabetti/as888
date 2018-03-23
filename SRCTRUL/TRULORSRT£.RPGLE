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
006000150729     D diore01       E DS
006100140317     d
006200140310     d
006300140310     D wrkoggiiso      s               d
006400140317     d s_arbdei        S                   LIKE(d98dei)
006500140317     d s_arboei        S                   LIKE(d98oei)
006600150729     d s_arbttc        S                   LIKE(d98ttc)
006700150729     d s_arbtcc        S                   LIKE(d98tcc)
006800140310     d
006900980304     D*------------------
007000980304     D* ARCHITETTURA
007100980304     D*------------------
007200980304     D KPJBA         E DS
007300980304     C*------------------------------------------------------------------------*
007400980304     C* MAIN LINES
007500980304     C*------------------------------------------------------------------------*
007600140310     c     kbolla        klist
007700140310     c                   kfld                    vidaas
007800140310     c                   kfld                    vidlnp
007900140310     c                   kfld                    vidnrs
008000140310     c                   kfld                    vidnsp
008100140310     C     Kcet          KLIST
008200140310     C                   KFLD                    pctfgs
008300140310     C                   KFLD                    pctNDC
008400140310     C                   KFLD                    arbpdc
008500140310     C                   KFLD                    TTRD              3
008600140310     C                   KFLD                    arbAAS
008700140310     C                   KFLD                    arbLNP
008800140310     C                   KFLD                    arbNRS
008900140310     C                   KFLD                    arbNSP
009000140310     c
009100980304     C*
009200980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009300980304DO  1C     F03L00        DOUEQ     '1'
009400980304     C     F12L00        OREQ      '1'
009500980304     C     FINL00        OREQ      '1'
009600980304     C*
009700980304     C* EMETTE FORMATO VIDEO
009800980304     C                   EXSR      BUTFMR
009900980304     C*
010000980304     C* GESTIONE TASTI FUNZIONALI
010100980304     C                   EXSR      FUNCON
010200980304E   1C                   ENDDO
010300140310     c
010400980304     C*
010500980304     C                   SETON                                        LR
010600980304     C*------------------------------------------------------------------------*
010700980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010800980304     C*------------------------------------------------------------------------*
010900980304     C     BUTFMR        BEGSR
011000980304     C*
011100980304     C* EMETTE LA TESTATA
011200140310     C                   WRITE     ULORSTD01
011300980304     C*
011400980304     C* SPEDISCE EVENTUALE MESSAGGIO
011500980304     C                   EXSR      SNDMSG
011600980304     C*
011700980304     C* EMETTE LE FUNZIONI
011800140310     C                   WRITE     ULORSTD02
011900980304     C*
012000980304     C* EMETTE IL VIDEO
012100140310     C                   EXFMT     ULORSTD03
012200980304     C*
012300980304     C* REIMPOSTA FLAG DI ERRORE
012400980304     C                   MOVEL     '0'           ERRL00
012500980304     C*
012600980304     C* REIMPOSTA INDICATORI DI ERRORE
012700980304     C                   SETOFF                                       59
012800980304     C                   SETOFF                                       606162
012900980304     C                   SETOFF                                       636465
013000980304     C                   SETOFF                                       666768
013100980304     C                   SETOFF                                       697071
013200980304     C                   SETOFF                                       727374
013300980304     C                   SETOFF                                       757677
013400980304     C                   SETOFF                                       787980
013500980304     C                   SETOFF                                       818283
013600980304     C                   SETOFF                                       848586
013700980304     C                   SETOFF                                       878889
013800980304     C*
013900980304     C                   ENDSR
014000980304     C*------------------------------------------------------------------------*
014100980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014200980304     C*------------------------------------------------------------------------*
014300980304     C     SNDMSG        BEGSR
014400980304     C*
014500980304     C* PULISCE IL SFILE MESSAGGI
014600980304     C                   MOVEL     '1'           *IN40
014700140310     C                   WRITE     ULORSTDMC
014800980304     C                   MOVEL     '0'           *IN40
014900980304     C*
015000980304     C* VISUALIZZA IL MESSAGGIO
015100980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
015200980304     C                   CALL      'XMSG'        XMSGPA
015300980304IF  1C     DSMSER        IFEQ      *ZEROS
015400980304     C                   MOVEL     DSMSKE        MSGKEY
015500980304E   1C                   ENDIF
015600980304     C                   Z-ADD     1             MSGULT
015700140310     C                   WRITE     ULORSTDMS
015800140310     C                   WRITE     ULORSTDMC
015900980304     C*
016000980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016100980304     C                   MOVEL     *BLANKS       DSMSMS
016200980304     C*
016300980304     C                   ENDSR
016400980304     C*------------------------------------------------------------------------*
016500980304     C* ENTGES - GESTIONE ENTER -> INVIO
016600980304     C*------------------------------------------------------------------------*
016700980304     C     ENTGES        BEGSR
016800140415     c                   setoff                                       1011
016900140317     c* controllo RITIRO o consegna
017000140317    1c                   if        (vidaas<>0 or vidlnp<>0 or vidnrs<>0 or
017100140317     c                                vidnsp<>0) and vidrit='S'
017200140317     C                   MOVEL     MSG(2)        DSMSMS
017300140317     c                   leavesr
017400140317    1c                   endif
017500140415     c                   if        vidaas=0 and vidlnp=0 and vidnsp=0 and
017600140415     c                             vidrit=' '
017700140415     C                   MOVEL     MSG(2)        DSMSMS
017800140415     c                   leavesr
017900140415    1c                   endif
018000140317     c
018100140317     c* Cerco località per i ritiri
018200140317    1c                   if        %scan('?':ioreloc) >0
018300140317     C                   CLEAR                   DSSI00
018400140317     C                   MOVEL     'L01'         D00OP0
018500140317     C                   MOVEL     iorecap       D00CAP
018600140317     C                   CALL      'TISI30R'
018700140317     C                   PARM                    KPJBA
018800140317     C                   PARM                    DSSI00
018900140317IF  2C     D00ERR        IFNE      '1'
019000140317     C                   MOVEL     D00CAP        iorecap                        *CAP
019100140317     C                   MOVEL     D00LOC        ioreloc                        *LOCALITA'
019200140317E   2C                   ENDIF
019300140317     c                   leavesr
019400140317    1c                   endif
019500140317     c
019600140317    1c                   if        iorecap<>*blanks or ioreloc<>*blanks
019700140317     C                   CLEAR                   DSSI95
019800140317     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
019900140317     C                   MOVEL     '3'           I95TCN                         *CONTROLLO
020000140317     C                   MOVEL     iorecap       I95CAP                         *CAP
020100140317     C                   MOVEL     ioreloc       I95LOC                         *LOCALITA'
020200140317     c* Tipo bolla
020300140317     C                   MOVEL     'F'           I95TPO
020400140317     C* ERRORE AFFIDABILITA = 0
020500140317     C                   MOVEL     ' '           I13AF0
020600140317     C                   CALL      'FNLV13R'
020700140317     C                   PARM                    KPJBA
020800140317     C                   PARM                    fnlv13ds
020900140317     C                   PARM                    DSSI95
021000140317     C                   PARM      ' '           flgbac            1
021100140317IF  2C     O95ERR        IFNE      *BLANKS
021200140317     c
021300140317IF   C     o13err        orNE      *BLANKS
021400140317     c     o13rop        andeq     *blanks
021500140317
021600140317     C     O95LIA        OREQ      '1'
021700140317     c
021800140317     C     ioreloc       ORNE      *BLANKS
021900140317     C     O95LID        ANDLT     '3'
022000140317     C                   MOVEL     MSG(3)        DSMSMS
022100140317     c                   leavesr
022200140317E   2C                   ENDIF
022300140317E   1C                   ENDIF
022400140317     c
022500140317     c* Se la bolla è uguale a quella di prima non pulisco video,
022600140317     c*  altrimenti pulisco campi di output
022700140317    1c                   if        vidaas<>arbaas or vidlnp<>arblnp or
022800140317     c                             vidnsp<>arbnsp  or vidnrs<>arbnrs
022900140317     c                   clear                   trulor2ds
023000140317    1c                   endif
023100140317     c
023200140317    1c                   if        vidnsp >0
023300140415     c                   seton                                        10
023400140317
023500140317    2c                   if        vidtbO<>'P'
023600140310     c     kbolla        chain     fnarb01l
023700140314     c                   else
023800140314     c     kbolla        chain     fnblp01l
023900140317    2c                   endif
024000140314     c
024100140317    2c                   if        (vidtbo<>'P' and %found(fnarb01l)) or
024200140314     c                             (vidtbo= 'P' and %found(fnblp01l))
024300140310     c                   clear                   tnsd99ds
024400140310     C                   Z-ADD     0             amgDCE            8 0
024500140310     C                   MOVEL     'L'           D98TLA
024600140314     c                   if        vidtbo<>'P'
024700140310     C                   MOVEL     'A'           D98TBO
024800140314     c                   else
024900140314     C                   MOVEL     'P'           D98TBO
025000140314     c                   endif
025100140310     C                   MOVEL     ARBAAS        D98AAS
025200140310     C                   MOVEL     ARBLNP        D98LNP
025300140310     C                   MOVEL     ARBNRS        D98NRS
025400140310     C                   MOVEL     ARBNSP        D98NSP
025500140310     C*
025600140310     C                   CALL      'TNSD99R'
025700140310     C                   PARM                    TNSD99DS
025800140310     c
025900140310     c* Mi salvo data teorica consegna
026000140310     c                   eval      s_arbdei=d98dei
026100140310     c                   eval      s_arboei=d98oei
026200150729     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
026300150729     c                   eval      s_arbttc=d98ttc
026400150729     c                   eval      s_arbtcc=d98tcc
026500140317    3c                   if        d98dce>0 and arbdce=0
026600140310     c                   eval      amgdce=d98dce
026700140317   x3C                   ELSE
026800140310     c                   eval      amgdce=arbdce
026900140317    3c                   endif
027000140310     C*
027100140317     c                   eval       IOREtfp = ARBtfp
027200140317     c                   eval       IOREtfa = ARBtfa
027300140317     c                   eval       IOREdti = ARBdti
027400140317     c                   eval       IOREhti = ARBhti
027500140317     c                   eval       IOREdcr = ARBdcr
027600140317     c                   eval       IOREhcr = ARBhcr
027700140317     c                   eval       IOREtcr = ARBtcr
027800140317     c                   eval       IOREDEI = s_ARBdei
027900140317     c                   eval       IOREOEI = s_ARBoei
028000140317     c                   eval       IOREfil = ARBlna
028100140317     c                   eval       IOREcap = ARBcad
028200140317     c                   eval       IOREloc = ARBlod
028300140317     c                   eval       IOREnar = ARBnzd
028400150729     c                   clear                   diore01
028500150729     c                   movel     s_arbttc      §ioretrazC
028600150729     c                   movel     s_arbtcc      §ioreconsC
028700150729     c                   eval       IOREflo = diore01
028800150729     c
028900140310     c* Data di Riferimento: data consegna se consegnata
029000140317    3c                   select
029100140314     c                   when      vidtbo='P' and arbdcm>0
029200140317     c                   eval       ioredta=arbdcm
029300140314     c                   when      vidtbo='P' and arbddc>0 and arbndc>0
029400140317     c                   eval       ioredta=arbDDC
029500140314
029600140314     c                   when      vidtbo<>'P' and arbdcm>0 and arbica<>' ' and arbica<>'R'
029700140314     c                             arbica<>' ' and arbica<>'R'
029800140310     c                             and arbicc<>' ' and arbicc<>'R'
029900140317     c                   eval       ioredta=arbdcm
030000140314     c*
030100140314     c                   when      vidtbo<>'P' and arbddc>0 and arbndc>0
030200140317     c                   eval       ioredta=arbDDC
030300140310     c                   other
030400140310     c* altrimenti data prevista consegna se > = oggi
030500140310     C                   time                    wrkoggiiso
030600140317     C     *iso          move      wrkoggiiso    ioredta
030700140310     c* se < di oggi, imposto oggi
030800140317     c                   if        amgdce>ioredta
030900140317     c                   eval       ioredta=amgdce
031000140310     c                   endif
031100140317    3c                   endsl
031200140310     c
031300140317     c                   eval       IOREtser = 'C'
031400140317    3c                   if          vidtbo<>'P' and
031500140314     c                              (ARBfbc = *blanks and
031600140310     c                               ARBddc > 0 and
031700140310     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
031800140310     c                               arbica='R' or arbicc=' ' or arbicc='R'))
031900140317     c                   eval       IOREcons = 'S'
032000140317     c                   eval       IOREDDC=arbddc
032100140317     c                   eval       IOREnDC=arbndc
032200140310     c* Orario presunto ritiro/consegna
032300140310     c                   move      arbndc        pctndc
032400140310     c                   move      arbifp        pctfgs
032500140310     c                   move      'COR'         ttrd
032600140310     c     kcet          setgt     fipct02l
032700140310     c     kcet          readpe    fipct02l
032800140317    4c                   if        not %eof(fipct02l)
032900140310     c                   movel     pctdati       fipctcords
033000140317     c                   eval      ioreorp=§PCTORASTI
033100140317    4c                   endif
033200140310     c
033300140317    3c                   ENDIF
033400140310     c
033500140317     c                   eval       IOREtsp = ARBtsp
033600140311     c* se impostato tipo servizio prendo quello
033700140317    3c                   if        vidtsp<>' '
033800140317     c                   eval       IOREtsp = vidtsp
033900140317    3c                   endif
034000140317     c* ora stimata forzata
034100140317    3c                   if        vidorp>0
034200140317     c                   eval      ioreorp=vidorp
034300140317    3c                   endif
034400140317     c
034500140311     c* se non in cosnegna azzero i campi legati
034600140317    3c                   if        vidnoc='N'
034700140317     c                   clear                   iorecons
034800140317     c                   clear                   iorendc
034900140317     c                   clear                   ioreddc
035000140317    3c                   endif
035100140310     c
035200980304     C*
035300140317    3c                   if        vidtipo='V'
035400140317     c                   eval      IOREcol = 35
035500140317     c                   eval      IORErig = 8
035600140317     c
035700140317     c                   write     ULORSTD03
035800140317     c
035900140310     c                   CALL      'TRULORSR'
036000140310     c                   PARM                    kpjba
036100140310     c                   PARM                    trulorsds
036200140311     c
036300140317   x3c                   else
036400140311     c*
036500140310     c                   CALL      'TRULORSR'
036600140310     c                   PARM                    kpjba
036700140310     c                   PARM                    trulorsds
036800140311     c                   PARM                    trulor2ds
036900140317    3c                   endif
037000140310     c
037100140317   x2c                   else
037200140310     c                   eval      OOR2MSG='non trovata bolla in ARB'
037300140317    2c                   endif
037400140317   x1c                   else
037500140317     c
037600140415     c                   seton                                        11
037700140317     c                   clear                   oor2msg
037800140317     c                   eval       IOREtser = 'R'
037900140317     c* se impostato tipo servizio prendo quello
038000140317    2c                   if        vidtsp<>' '
038100140317     c                   eval       IOREtsp = vidtsp
038200140317    2c                   endif
038300140317     c* ora stimata forzata
038400140317    2c                   if        vidorp>0
038500140317     c                   eval      ioreorp=vidorp
038600140317    2c                   endif
038700140317     c
038800140317    2c                   if        vidtipo='V'
038900140317     c                   eval      IOREcol = 35
039000140317     c                   eval      IORErig = 8
039100140317     c                   CALL      'TRULORSR'
039200140317     c                   PARM                    kpjba
039300140317     c                   PARM                    trulorsds
039400140317     c
039500140317   x2c                   else
039600140317     c*
039700140317     c                   CALL      'TRULORSR'
039800140317     c                   PARM                    kpjba
039900140317     c                   PARM                    trulorsds
040000140317     c                   PARM                    trulor2ds
040100140317    2c                   endif
040200140317    1c                   endif
040300980304     C*
040400980304     C                   ENDSR
040500980304     C*------------------------------------------------------------------------*
040600980304     C* F03GES - GESTIONE F03 -> FINE
040700980304     C*------------------------------------------------------------------------*
040800980304     C     F03GES        BEGSR
040900980304     C*
041000980304     C* IMPOSTA "PREMUTO F03"
041100980304     C                   MOVEL     '1'           F03L00
041200980304     C*
041300980304     C                   ENDSR
041400980304     C*------------------------------------------------------------------------*
041500980304     C* F12GES - GESTIONE F12 -> RITORNO
041600980304     C*------------------------------------------------------------------------*
041700980304     C     F12GES        BEGSR
041800980304     C*
041900980304     C* IMPOSTA "PREMUTO F12"
042000980304     C                   MOVEL     '1'           F12L00
042100980304     C*
042200980304     C                   ENDSR
042300140317     C*------------------------------------------------------------------------*
042400140317     C* F10GES - pulisce
042500140317     C*------------------------------------------------------------------------*
042600140317     C     F10GES        BEGSR
042700140317     C*
042800140415     c                   setoff                                       1011
042900140317     c                   clear                   trulorsds
043000140317     c                   clear                   trulor2ds
043100140317     c                   clear                   vidtsp
043200140317     c                   clear                   vidtbo
043300140317     c                   clear                   vidaas
043400140317     c                   clear                   vidlnp
043500140317     c                   clear                   vidnrs
043600140317     c                   clear                   vidnsp
043700140317     c                   clear                   vidrit
043800140317     c                   clear                   vidtipo
043900140317     c                   clear                   vidnoc
044000140317     c                   clear                   vidorp
044100140317     C*
044200140317     C                   ENDSR
044300980304     C*------------------------------------------------------------------------*
044400980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
044500980304     C*------------------------------------------------------------------------*
044600980304     C     F24GES        BEGSR
044700980304     C*
044800980304     C                   EXSR      FUNGES
044900980304     C*
045000980304     C                   ENDSR
045100980304     C*------------------------------------------------------------------------*
045200980304     C* *INZSR - OPERAZIONI INIZIALI
045300980304     C*------------------------------------------------------------------------*
045400980304     C     *INZSR        BEGSR
045500980304     C*---
045600980304     C* RICEVIMENTO PARAMETRI
045700980304     C*---
045800980304     C     *ENTRY        PLIST
045900980304     C                   PARM                    KPJBA
046000980304     C*
046100980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
046200980304     C     XMSGPA        PLIST
046300980304     C                   PARM                    DSMSMS
046400980304     C                   PARM                    DSMSPG
046500980304     C                   PARM                    DSMSKE
046600980304     C                   PARM                    DSMSER
046700980304     C*
046800980304     C* IMPOSTA LA DS -TASTI PREMUTI-
046900980304     C                   CALL      'XKEY'
047000980304     C                   PARM                    DSKEY
047100980304     C*---
047200980304     C* VARIABILI NON RIFERITE AL DATA BASE
047300980304     C*---
047400980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
047500980304     C*---
047600980304     C* VARIABILI DI CONTROLLO
047700980304     C*---
047800980304     C                   MOVEL     '0'           FINL00            1
047900980304     C                   MOVEL     '0'           F03L00            1
048000980304     C                   MOVEL     '0'           F12L00            1
048100980304     C                   MOVEL     '0'           ERRL00            1
048200980304     C*
048300980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
048400140310     C                   MOVEL     'TRULORSR'    DSMSPG
048500140310     C                   MOVEL     'TRULORSR'    MSGQUE
048600980304     C                   MOVEL     *BLANKS       MSGKEY
048700980304     C                   MOVEL     *BLANKS       DSMSMS
048800980304     C*
048900980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
049000980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
049100980304     C                   EXSR      FUNGES
049200980304     C*
049300980304     C* IMPOSTA IL VIDEO
049400140310     c                   CLEAR                   TRULORSds
049500140310     c                   CLEAR                   TRULOR2ds
049600140317     c                   CLEAR                   arbaas
049700140317     c                   CLEAR                   arblnp
049800140317     c                   CLEAR                   arbnrs
049900140317     c                   CLEAR                   arbnsp
050000980304     C*
050100980304     C                   ENDSR
050200980304     C*------------------------------------------------------------------------*
050300980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
050400980304     C*------------------------------------------------------------------------*
050500980304     C     FUNGES        BEGSR
050600980304     C*
050700980304     C* PULISCO CAMPI DI LAVORO
050800980304     C                   Z-ADD     0             W                 5 0
050900980304     C*
051000980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
051100980304     C* E CARICO TABELLA FUNZIONI ATTIVE
051200980304     C     1             DO        24            I
051300980304     C                   MOVEL     FUD(I)        FUDRIG
051400980304     C*
051500980304     C* PULIZIA FUNZIONI PRECEDENTI
051600980304     C                   MOVEL     *BLANKS       FUV(I)
051700980304     C*
051800980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
051900980304     C* (SALTA F24)
052000980304     C     FUDATT        IFEQ      'S'
052100980304     C     FUDFUN        ANDNE     '24'
052200980304     C                   ADD       1             W
052300980304     C                   MOVEL     FUDDES        FUV(W)
052400980304     C                   END
052500980304     C                   END
052600980304     C*
052700980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
052800980304     C* ALTRIMENTI DISATTIVO F24
052900980304     C     1             DO        24            I
053000980304     C                   MOVEL     FUD(I)        FUDRIG
053100980304     C*
053200980304     C     FUDFUN        IFEQ      '24'
053300980304     C     W             IFGT      6
053400980304     C                   MOVEL     'S'           FUDATT
053500980304     C                   ELSE
053600980304     C                   MOVEL     *BLANKS       FUDATT
053700980304     C                   END
053800980304     C                   MOVEL     FUDRIG        FUD(I)
053900980304     C                   END
054000980304     C                   END
054100980304     C*
054200980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
054300980304     C     W             DIV       6             I
054400980304     C                   MVR                     Z
054500980304     C*
054600980304     C     Z             IFNE      0
054700980304     C     I             ADD       1             Z
054800980304     C                   ELSE
054900980304     C                   Z-ADD     I             Z                 5 0
055000980304     C                   END
055100980304     C*
055200980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
055300980304     C                   ADD       1             FUNGRU
055400980304     C     FUNGRU        IFGT      Z
055500980304     C     FUNGRU        ORLT      1
055600980304     C                   Z-ADD     1             FUNGRU
055700980304     C                   END
055800980304     C*
055900980304     C* PRIMO GRUPPO DI 6 FUNZIONI
056000980304     C     FUNGRU        IFEQ      1
056100980304     C                   MOVEL     FUV(1)        FUN001
056200980304     C                   MOVEL     FUV(2)        FUN002
056300980304     C                   MOVEL     FUV(3)        FUN003
056400980304     C                   MOVEL     FUV(4)        FUN004
056500980304     C                   MOVEL     FUV(5)        FUN005
056600980304     C                   MOVEL     FUV(6)        FUN006
056700980304     C                   END
056800980304     C*
056900980304     C* SECONDO GRUPPO DI 6 FUNZIONI
057000980304     C     FUNGRU        IFEQ      2
057100980304     C                   MOVEL     FUV(7)        FUN001
057200980304     C                   MOVEL     FUV(8)        FUN002
057300980304     C                   MOVEL     FUV(9)        FUN003
057400980304     C                   MOVEL     FUV(10)       FUN004
057500980304     C                   MOVEL     FUV(11)       FUN005
057600980304     C                   MOVEL     FUV(12)       FUN006
057700980304     C                   END
057800980304     C*
057900980304     C* TERZO GRUPPO DI 6 FUNZIONI
058000980304     C     FUNGRU        IFEQ      3
058100980304     C                   MOVEL     FUV(13)       FUN001
058200980304     C                   MOVEL     FUV(14)       FUN002
058300980304     C                   MOVEL     FUV(15)       FUN003
058400980304     C                   MOVEL     FUV(16)       FUN004
058500980304     C                   MOVEL     FUV(17)       FUN005
058600980304     C                   MOVEL     FUV(18)       FUN006
058700980304     C                   END
058800980304     C*
058900980304     C* QUARTO GRUPPO DI 6 FUNZIONI
059000980304     C     FUNGRU        IFEQ      4
059100980304     C                   MOVEL     FUV(19)       FUN001
059200980304     C                   MOVEL     FUV(20)       FUN002
059300980304     C                   MOVEL     FUV(21)       FUN003
059400980304     C                   MOVEL     FUV(22)       FUN004
059500980304     C                   MOVEL     *BLANKS       FUN005
059600980304     C                   MOVEL     *BLANKS       FUN006
059700980304     C                   END
059800980304     C*
059900980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
060000980304     C                   MOVEL     *BLANKS       FUN007
060100980304     C                   MOVEL     *BLANKS       FUN008
060200980304     C*
060300980304     C     1             DO        24            I
060400980304     C                   MOVEL     FUD(I)        FUDRIG
060500980304     C     FUDFUN        IFEQ      '24'
060600980304     C     FUDATT        ANDEQ     'S'
060700980304     C                   MOVEL     FUDDES        FUN008
060800980304     C                   END
060900980304     C                   END
061000980304     C*
061100980304     C                   ENDSR
061200980304     C*------------------------------------------------------------------------*
061300980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
061400980304     C*------------------------------------------------------------------------*
061500980304     C     FUNCON        BEGSR
061600980304     C*
061700980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
061800980304     C                   EXSR      FUNVAR
061900980304     C*
062000980304     C* PULISCO CAMPI DI LAVORO
062100980304     C                   Z-ADD     0             I                 5 0
062200980304     C                   Z-ADD     0             W                 5 0
062300980304     C*
062400980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
062500980304     C     FUNKEY        IFNE      *BLANKS
062600980304     C     1             DO        24            I
062700980304     C                   MOVEL     FUD(I)        FUDRIG
062800980304     C*
062900980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
063000980304     C     FUNKEY        IFEQ      FUDFUN
063100980304     C     FUDATT        ANDEQ     'S'
063200980304     C                   Z-ADD     I             W
063300980304     C                   END
063400980304     C                   END
063500980304     C*
063600980304     C* FUNZIONE NON ATTIVA (ERRORE)
063700980304     C     W             IFEQ      0
063800980304     C*
063900980304     C* EMISSIONE MESSAGGIO DI ERRORE
064000980304     C                   MOVEL     MSG(1)        DSMSMS
064100980304     C*
064200980304     C                   END
064300980304     C                   END
064400980304     C*
064500980304     C* FUNZIONE ATTIVA (PROCEDI ...)
064600980304     C     W             IFNE      0
064700980304     C     FUNKEY        OREQ      *BLANKS
064800980304     C     FUNKEY        IFEQ      *BLANKS
064900980304     C                   EXSR      ENTGES
065000980304     C                   END
065100980304     C     FUNKEY        IFEQ      '03'
065200980304     C                   EXSR      F03GES
065300980304     C                   END
065400140317     C     FUNKEY        IFEQ      '10'
065500140317     C                   EXSR      F10GES
065600980304     C                   END
065700140317     C     FUNKEY        IFEQ      '12'
065800140317     C                   EXSR      F12GES
065900140317     C                   END
066000980304     C                   END
066100980304     C*
066200980304     C                   ENDSR
066300980304     C*------------------------------------------------------------------------*
066400980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
066500980304     C*------------------------------------------------------------------------*
066600980304     C     AZZVID        BEGSR
066700980304     C*
066800980304     C                   ENDSR
066900980304     C*------------------------------------------------------------------------*
067000980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
067100980304     C*------------------------------------------------------------------------*
067200980304     C     FUNVAR        BEGSR
067300980304     C*
067400980304     C* INIZIALIZZA VARIABILE -FUNKEY-
067500980304     C*
067600980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
067700980304     C     KEY           IFEQ      ENTER
067800980304     C                   MOVEL     *BLANKS       FUNKEY
067900980304     C                   END
068000980304     C     KEY           IFEQ      F1
068100980304     C                   MOVEL     '01'          FUNKEY
068200980304     C                   END
068300980304     C     KEY           IFEQ      F2
068400980304     C                   MOVEL     '02'          FUNKEY
068500980304     C                   END
068600980304     C     KEY           IFEQ      F3
068700980304     C                   MOVEL     '03'          FUNKEY
068800980304     C                   END
068900980304     C     KEY           IFEQ      F4
069000980304     C                   MOVEL     '04'          FUNKEY
069100980304     C                   END
069200980304     C     KEY           IFEQ      F5
069300980304     C                   MOVEL     '05'          FUNKEY
069400980304     C                   END
069500980304     C     KEY           IFEQ      F6
069600980304     C                   MOVEL     '06'          FUNKEY
069700980304     C                   END
069800980304     C     KEY           IFEQ      F7
069900980304     C                   MOVEL     '07'          FUNKEY
070000980304     C                   END
070100980304     C     KEY           IFEQ      F8
070200980304     C                   MOVEL     '08'          FUNKEY
070300980304     C                   END
070400980304     C     KEY           IFEQ      F9
070500980304     C                   MOVEL     '09'          FUNKEY
070600980304     C                   END
070700980304     C     KEY           IFEQ      F10
070800980304     C                   MOVEL     '10'          FUNKEY
070900980304     C                   END
071000980304     C     KEY           IFEQ      F11
071100980304     C                   MOVEL     '11'          FUNKEY
071200980304     C                   END
071300980304     C     KEY           IFEQ      F12
071400980304     C                   MOVEL     '12'          FUNKEY
071500980304     C                   END
071600980304     C     KEY           IFEQ      F13
071700980304     C                   MOVEL     '13'          FUNKEY
071800980304     C                   END
071900980304     C     KEY           IFEQ      F14
072000980304     C                   MOVEL     '14'          FUNKEY
072100980304     C                   END
072200980304     C     KEY           IFEQ      F15
072300980304     C                   MOVEL     '15'          FUNKEY
072400980304     C                   END
072500980304     C     KEY           IFEQ      F16
072600980304     C                   MOVEL     '16'          FUNKEY
072700980304     C                   END
072800980304     C     KEY           IFEQ      F17
072900980304     C                   MOVEL     '17'          FUNKEY
073000980304     C                   END
073100980304     C     KEY           IFEQ      F18
073200980304     C                   MOVEL     '18'          FUNKEY
073300980304     C                   END
073400980304     C     KEY           IFEQ      F19
073500980304     C                   MOVEL     '19'          FUNKEY
073600980304     C                   END
073700980304     C     KEY           IFEQ      F20
073800980304     C                   MOVEL     '20'          FUNKEY
073900980304     C                   END
074000980304     C     KEY           IFEQ      F21
074100980304     C                   MOVEL     '21'          FUNKEY
074200980304     C                   END
074300980304     C     KEY           IFEQ      F22
074400980304     C                   MOVEL     '22'          FUNKEY
074500980304     C                   END
074600980304     C     KEY           IFEQ      F23
074700980304     C                   MOVEL     '23'          FUNKEY
074800980304     C                   END
074900980304     C     KEY           IFEQ      F24
075000980304     C                   MOVEL     '24'          FUNKEY
075100980304     C                   END
075200980304     C     KEY           IFEQ      ROLLUP
075300980304     C                   MOVEL     'RU'          FUNKEY
075400980304     C                   END
075500980304     C     KEY           IFEQ      HELP
075600980304     C                   MOVEL     'HP'          FUNKEY
075700980304     C                   END
075800980304     C                   ENDSR
075900980304     C*------------------------------------------------------------------------*
076000980304     C* NOTE COSTRUZIONE SCHIERE
076100980304     C*------------------------------------------------------------------------*
076200980304     C*
076300980304     C* -MSG-MESSAGGI
076400980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
076500980304     C* (NON TOCCARE MSG,1 E MSG,2)
076600980304     C*
076700980304     C* -CMD-COMANDI
076800980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
076900980304     C*
077000980304     C* -OPD-OPZIONI
077100980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
077200980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
077300980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
077400980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
077500980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
077600980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
077700980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
077800980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
077900980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
078000980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
078100980304     C*
078200980304     C* -FUD-FUNZIONI
078300980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
078400980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
078500980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
078600980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
078700980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
078800980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
078900980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
079000980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
079100980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
079200980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
079300980304     o*-------------------------------------------------------------------------
079400980304** msg - messaggi
079500140317Funzione non attiva
079600140317Immettere per consegne o per ritiri
079700140317CAP o località errati
079800140210fnlrj6R- 4
079900140210fnlrj6R- 5
080000140210fnlrj6R- 6
080100140210fnlrj6R- 7
080200140210LRLRj6R- 8
080300140210LRLRj6R- 9
080400140210LRLRj6R- 10
080500980304** FUD - FUNZIONI
080600980304                   -        (S/N)=N                        funz.01¬
080700980304                   -        (S/N)=N                        funz.02¬
080800980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
080900980304                   -        (S/N)=N                        funz.04¬
081000980304                   -        (S/N)=N                        funz.05¬
081100980304                   -        (S/N)=N                        funz.06¬
081200980304                   -        (S/N)=N                        funz.07¬
081300980304                   -        (S/N)=N                        funz.08¬
081400980304                   -        (S/N)=N                        funz.09¬
081500140317F10=Pulisce        -        (S/N)=S                        funz.10¬
081600980304                   -        (S/N)=N                        funz.11¬
081700980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
081800980304                   -        (S/N)=N                        funz.13¬
081900980304                   -        (S/N)=N                        funz.14¬
082000980304                   -        (S/N)=N                        funz.15¬
082100980304                   -        (S/N)=N                        funz.16¬
082200980304                   -        (S/N)=N                        funz.17¬
082300980304                   -        (S/N)=N                        funz.18¬
082400980304                   -        (S/N)=N                        funz.19¬
082500980304                   -        (S/N)=N                        funz.20¬
082600980304                   -        (S/N)=N                        funz.21¬
082700980304                   -        (S/N)=N                        funz.22¬
082800980304                   -        (S/N)=N                        funz.23¬
082900980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
