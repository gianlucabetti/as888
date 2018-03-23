000100980408      ***********************************************************************
000200000119      *   IMPORTA STATO DELLE CONSEGNE DA E.D.I.   ESTERO                   *
000300930128      ***********************************************************************
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500000119
000600960531     FFNCDE02L  UF A E           K DISK
000700980319     FFNCDS02L  UF A E           K DISK
000800980226     FFNCDA02L  UF A E           K DISK
000900990810     FFNEVB00F  O    E             DISK
001000960531     FEDIFTSTA  UF   E             DISK
001100960528     FEDTAB01L  IF   E           K DISK
001200961112     FEDRDE01L  UF A E           K DISK
001300961007     FFNARB01L  IF   E           K DISK
001400051017     FFIARS10L  IF   E           K DISK
001500961125     FFNART01L  IF   E           K DISK
001600000119     FFNART27L  IF   E           K DISK    RENAME(FNART000:FNART027)
001700961119     FFNLBL02L  IF   E           K DISK
001800980407     FEDRDA01L  UF A E           K DISK
001900960531     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000960628     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
002100980312      *---------------------------------------------------------------------*
002200980312      * Schiere
002300980312      *---------------------------------------------------------------------*
002400980312      * Schiera per scrittura dati località record ST10
002500980312      * Schiera per scrittura testi liberi record ST15
002600980312      * Schiera per scrittura testi liberi record SD15
002700980312      * Schiera per reperimento numero segnacollo
002800980312     D SGN             S             35    DIM(300)
002900980312      * Tabella partner
003000960531     D CPT             S             35    DIM(100)
003100000120     D CPTparz         S             35    DIM(100)
003200960531     D DPT             S             90    DIM(100)
003300961007     D CPU             S             35    DIM(100)
003400961007     D DPU             S             90    DIM(100)
003500980312      * Tabella codici eventi
003600960612     D CEV             S              6    DIM(200)
003700961118     D DEV             S             90    DIM(200)
003800980312      * Nr. documento alfanumerico da già decodificato
003900960531     D NDN             S              1    DIM(15)
004000000119      * Nr. documento alfanumerico da decodificare
004100960531     D NDA             S              1    DIM(35)
004200000119      * Errori
004300000119     D ERR             S             70    DIM(8) CTDATA PERRCD(1)
004400000119      *---------------------------------------------------------------------*
004500000119      * DS
004600000119      *---------------------------------------------------------------------*
004700000119      * .. per scompozione dati da spedire a seconda del tipo record
004800000119     D EDST00DS      E DS
004900000119     D EDST05DS      E DS
005000000119     D EDST10DS      E DS
005100000119     D  T10                   48    103    DIM(2)
005200000119     D EDST15DS      E DS
005300000119     D  T15                   13    362    DIM(5)
005400000119     D EDSD00DS      E DS
005500000119     D EDSD05DS      E DS
005600000119     D EDSD10DS      E DS
005700000119     D EDSD15DS      E DS
005800000119     D  D15                   13    362    DIM(5)
005900000119     D EDSD20DS      E DS
006000000119     D  D20                   33    732    DIM(20)
006100000119      *  Ds per scrittura dati località
006200960528     D WST10           DS
006300960528     D  WTPL                   1      3
006400960528     D  WLOC                   4     28
006500000119      *---------------------------------------------------------------------*
006600000119      * .. per reperimento tipo record di EDIFTSTA da scrivere
006700000119      *                    .. 5 - 8  : tipo record
006800960528     D WDAT            DS           382
006900960528     D  STATPR                 5      8
007000000119      *---------------------------------------------------------------------*
007100960528     D KPJBA         E DS
007200000119     D UT§DSE0F      E DS
007300020911      *
007400021114     D Tibs36ds      E DS
007500960528     D CNCR80        E DS
007600000119     D EDIDSCE       E DS
007700000119     D EDIDSPT       E DS
007800000119     D EDIDSPU       E DS
007900960531     D DS2A          E DS
008000980226      *
008100980407      *  DS x composizione errore nr.spedizione
008200961120     D DSERR           DS
008300961120     D  W9011                 50     52
008400961120     D  W9013                 56     58
008500961127     D  DATAEV                64     71  0
008600960528     D*  DS x scomposizione segnacollo
008700980312     D DSSGNE          DS
008800980312     D  SGFLS                  1      3  0
008900980312     D  SGNRS                  4      5  0
009000980312     D  SGNSC                  6     12  0
009100960528     D DSSGN           DS
009200961029     D  SGNFLS                 1      3  0
009300960528     D  SGNLNA                 4      6  0
009400960528     D  SGNNRS                 7      8  0
009500960528     D  SGNNSC                 9     15  0
009600961007     D DSSPE           DS
009700960531     D  WAAS                   1      4  0
009800960531     D  WLNP                   5      7  0
009900960531     D  WNRS                   8      9  0
010000960715     D  WNSP                  10     16  0
010100961007     D DSSPE2          DS
010200961007     D  WAAS2                  1      2  0
010300961007     D  WLNP2                  3      5  0
010400961007     D  WNRS2                  6      7  0
010500961007     D  WNSP2                  8     14  0
010600960528     D WLBDA8          DS
010700960528     D  G02DAT                 1      8  0
010800960528     D  G02INV                 9     16  0
010900960528     D  G02ERR                17     17
011000960528     D  G02TGI                18     22  0
011100960701     D*  DS x stampa segnacollo
011200960701     D                 DS
011300960701     D  SGN1                   1     35
011400960701     D  SGN2                  36     70
011500960701     D  SGN3                  71    105
011600000120     D  SGT                    1    105    DIM(3)
011700961125     D*  Ds per composizione errore <> nr.colli
011800961127     D ERRCOL          DS            35
011900961203     D  COLCON                 7     11  0
012000961203     D  COLSPE                16     20  0
012100000120      *---------------                                                      *
012200000120     D W035A           s             35
012300000120     D Wptparz         s              1
012400000119      *---------------------------------------------------------------------*
012500960528     IIFTSTA00
012600960528     I              SSIFTSTA                    STADAT
012700980225      *---------------------------------------------------------------------*
012800980225      * Ciclo principale
012900980225      *---------------------------------------------------------------------*
013000960528      * Posizionamento iniziale sul file  (prima lettura)
013100960528     C                   EXSR      REDFIL
013200980225      *
013300960531      * Lettura fino a fine file
013400960531     C     WFINE         DOWNE     'S'                                          - Fine file -
013500980225      *
013600980225      *  Controllo tipo record letto per eseguire decodifica
013700980225      *
013800980225      * Comuni
013900960715     C     STATPR        CASEQ     'ST00'        DRET00
014000960715     C     STATPR        CASEQ     'ST05'        DRET05
014100960715     C     STATPR        CASEQ     'ST10'        DRET10
014200960531     C     STATPR        CASEQ     'ST15'        DRET15
014300960531     C     STATPR        CASEQ     'SD00'        DRED00
014400980225     C                   END
014500980225      *
014600980225      * Cargo manifest
014700980402     C     TIPDOC        IFEQ      '784'
014800960531     C     STATPR        CASEQ     'SD05'        DRED05
014900960531     C     STATPR        CASEQ     'SD10'        DRED10
015000960531     C     STATPR        CASEQ     'SD15'        DRED15
015100960531     C     STATPR        CASEQ     'SD20'        DRED20
015200980225     C                   END
015300000119     C                   ENDIF
015400980225      *
015500980225      * Arrival notice
015600980225     C     TIPDOC        IFEQ      '781'
015700980225     C     STATPR        CASEQ     'SD05'        DREA05
015800980225     C     STATPR        CASEQ     'SD10'        DREA10
015900980225     C     STATPR        CASEQ     'SD15'        DREA15
016000980225     C     STATPR        CASEQ     'SD20'        DREA20
016100980225     C                   END
016200000119     C                   ENDIF
016300980225      *
016400960531      * Cancello record
016500960531     C                   DELETE    EDIFTSTA
016600980225      *
016700960531      * Lettura successiva
016800960531     C                   EXSR      REDFIL
016900960531     C                   END                                                    - Fine file -
017000980225      *
017100980225      * Ultimo record
017200960715     C     WPRIMA        IFEQ      'N'
017300980403     C     TIPDOC        IFEQ      '784'
017400980225     C                   EXSR      WRTCDE
017500980225     C                   ELSE
017600980225     C                   EXSR      WRTCDA
017700000119     C                   ENDIF
017800000119     C                   ENDIF
017900980225      *
018000980225      * Fine pgm.
018100960531     C                   SETON                                        LR
018200960528     C*----------------------------------------------------------------
018300960528     C*? REDFIL - LETTURA PROSSIMO RECORD
018400960531     C*----------------------------------------------------------------
018500960531     C     REDFIL        BEGSR
018600960531     C*
018700960531     C* Lettura file
018800960531     C                   READ      EDIFTSTA                               30
018900960531     C                   MOVEL     STADAT        WDAT
019000960531     C     *IN30         IFEQ      '1'
019100960531     C                   MOVEL     'S'           WFINE
019200960531     C                   END
019300960531     C*
019400960531     C                   ENDSR
019500960531     C*----------------------------------------------------------------
019600960531     C*? DRET00 - DECODIFICA TIPO RECORD ST00
019700960531     C*----------------------------------------------------------------
019800960531     C     DRET00        BEGSR
019900980226      *
020000970206     C                   SETON                                        OF
020100000119     C                   MOVEL     STADAT        EDST00DS
020200000119     C                   clear                   WDADOC            8 0
020300000119     C                   clear                   WHHDOC            4 0
020400980421      *
020500980421      * Se leggo nuovo record scrivo FNCDE o FNDCA
020600980421     C     WPRIMA        IFEQ      'N'
020700980421      *
020800980421     C     TIPDOC        IFEQ      '784'
020900980421     C                   EXSR      WRTCDE
021000980421     C                   ELSE
021100980421     C                   EXSR      WRTCDA
021200000119     C                   ENDIF
021300980421      *
021400980421     C                   EXSR      AZZBOL
021500980421     C                   MOVEL     'S'           WPRIMA            1
021600000119     C                   ENDIF
021700980226      *
021800000119      * Se è stato modificato il codice partner controllo se è codificato
021900000119     C                   IF        WCODPT <> TA0004  or  WQUAPT <> TA0007
022000960531     C                   MOVEL     TA0004        WCODPT           35
022100961021     C                   MOVEL     TA0007        WQUAPT
022200000119     C                   eval      *inOF = *on
022300000119     C                   eval      *inOA = *on
022400980420      *
022500980420      *  In base al codice del partner determino la linea d'arrivo
022600000119      *  A livello di spedizione riaggancio la tabella con la nazione come
022700000119      *  qualificatore.
022800000119     C                   CLEAR                   WLNA
022900960531     C                   Z-ADD     1             X
023000000119      * Primo tentativo rep. da codice + qualificatore
023100961021     C                   MOVEL     TA0004        WCOD             35
023200961021     C                   MOVE      TA0007        WCOD
023300961021     C     WCOD          LOOKUP    CPT(X)                                 32
023400000120     C                   If        *IN32 = *on
023500000120     c                   clear                   WPTparz
023600000119      * Secondo tentativo rep. da codice
023700000120     C                   Else
023800000119     C                   eval      x = 1
023900000120     C                   eval      W035A = %subst(TA0004:1:31)
024000000120     C     W035A         lookup    CPTparz(X)                             32
024100000120     c   32              eval      WPTparz = 'S'
024200000120     C                   Endif
024300000119      *
024400960531     C     *IN32         IFEQ      '1'
024500000119     C                   MOVEL     DPT(X)        EDIDSPT
024600960531     C                   MOVEL     §PTLNP        WLNA              3 0
024700000119     C                   MOVEL     DPU(X)        EDIDSPU
024800000119     C                   ENDIF
024900980225      *
025000000119     C                   ENDIF
025100980225      *
025200980225      *  Converto data e ora DOCUMENNTO
025300960703     C                   MOVEL     TA2380        WDATA            35
025400960703     C                   MOVEL     TA2379        WFORM             3
025500960703     C                   EXSR      CNVDAT
025600980225      *  Data corretta
025700960703     C     WERRO         IFNE      'S'
025800971031     C                   MOVEL     WCAMG         WAA2              4 0
025900971031     C                   MOVE      WCAMG         WMMGG             4 0
026000971031     C                   MOVE      WMMGG         WGG2              2 0
026100971031     C                   MOVEL     WMMGG         WMM2              2 0
026200971031     C                   MOVEL     WGG2          WGGMM             4 0
026300971031     C                   MOVE      WMM2          WGGMM             4 0
026400971031     C                   MOVEL     WGGMM         WDADOC
026500971031     C                   MOVE      WAA2          WDADOC
026600960703     C                   MOVE      WHHMM         WHHDOC
026700960703     C                   END
026800970923     C*  Giro la data messaggio
026900971031     C***§                 MOVELTA0017    WAA2    40
027000970923     C***§                 MOVELTA0017    WDTMSG  80
027100970923     C***§                 MOVE TA0017    WGG2    20
027200970923     C***§                 MOVELWGG2      WDTMSG
027300970923     C***§                 MOVE WAA2      WDTMSG
027400970923     C                   MOVEL     TA0017        WDATA
027500970923     C                   MOVEL     '102'         WFORM
027600970923     C                   EXSR      CNVDAT
027700980225      *
027800980225      *  Data corretta imposto DATA PREPARAZ. MSG
027900970923     C     WERRO         IFNE      'S'
028000971031     C                   MOVEL     WCAMG         WAA2              4 0
028100971031     C                   MOVE      WCAMG         WMMGG             4 0
028200971031     C                   MOVE      WMMGG         WGG2              2 0
028300971031     C                   MOVEL     WMMGG         WMM2              2 0
028400971031     C                   MOVEL     WGG2          WGGMM             4 0
028500971031     C                   MOVE      WMM2          WGGMM             4 0
028600971031     C                   MOVEL     WGGMM         WDTMSG            8 0
028700971031     C                   MOVE      WAA2          WDTMSG
028800970923     C                   END
028900980225      *
029000980225      *  ORA corretta imposto WORMSG
029100970923     C                   CLEAR                   WORMSG
029200970923     C                   MOVE      TA0019        WCOM4A            4
029300970923     C                   TESTN                   WCOM4A               98
029400970923     C     *IN98         IFEQ      '1'
029500970923     C                   MOVE      TA0019        WORMSG            4 0
029600970923     C                   ELSE
029700970923     C                   CLEAR                   WORMSG
029800970923     C                   END
029900000119      *
030000040608     c                   If        In_Stampa ='S'
030100960701     C   OA              EXCEPT    TEST1
030200960701     C                   EXCEPT    DATMSG
030300040608     c                   end
030400980225      *
030500980225     C                   MOVEL     TA1001        TIPDOC            3
030600980225      *
030700960531     C                   ENDSR
030800980225      *----------------------------------------------------------------
030900980225      *? DRET05 - DECODIFICA TIPO RECORD ST05
031000980225      *----------------------------------------------------------------
031100960715     C     DRET05        BEGSR
031200000119      *
031300000119      *  Imposto dati ST05
031400000119     C                   MOVEL     STADAT        EDST05DS
031500000119      *  Stampa
031600040608     c                   If        In_Stampa ='S'
031700960715     C   OA              EXCEPT    TEST1
031800000119      *
031900000119     C                   SELECT
032000000119     C                   WHEN       TE3035 = 'SF'
032100000119     C                   except    DATPAM
032200000119     C                   WHEN       TE3035 = 'ST'
032300000119     C                   except    DATPAD
032400000119     C                   ENDSL
032500040608     c                   end
032600000119      *
032700960715     C                   ENDSR
032800000119      *----------------------------------------------------------------
032900000119      *? DRET10 - DECODIFICA TIPO RECORD ST10
033000000119      *----------------------------------------------------------------
033100960531     C     DRET10        BEGSR
033200000119      *
033300000119     C                   MOVEL     STADAT        EDST10DS
033400000119      *
033500960531     C                   ENDSR
033600000119      *----------------------------------------------------------------
033700000119      *? DRET15 - DECODIFICA TIPO RECORD ST15
033800000119      *----------------------------------------------------------------
033900960531     C     DRET15        BEGSR
034000000119      *
034100000119     C                   MOVEL     STADAT        EDST15DS
034200000119      *  Stampo note documento se ci sono
034300960701     C     TC4451        IFNE      *BLANKS
034400960701     C                   Z-ADD     1             X
034500040608     c                   If        In_Stampa ='S'
034600960701     C   OA              EXCEPT    TEST1
034700960701     C                   EXCEPT    NOTMSG
034800040608     c                   end
034900960701     C                   DO        4             X
035000960701     C     T15(X)        IFNE      *BLANKS
035100960703     C                   MOVEL     T15(X)        WNOT             70
035200040608     c                   If        In_Stampa ='S'
035300960701     C                   EXCEPT    NOTMS1
035400040608     c                   end
035500000119     C                   ENDIF
035600000119     C                   ENDDO
035700000119      *
035800000119     C                   ENDIF
035900000119      *
036000960531     C                   ENDSR
036100000119      *----------------------------------------------------------------
036200000119      *? DRED00 - DECODIFICA TIPO RECORD SD00
036300000119      *----------------------------------------------------------------
036400960531     C     DRED00        BEGSR
036500980225      *
036600980225      * Se leggo nuovo record scrivo FNCDE o FNCDA
036700960715     C     WPRIMA        IFEQ      'S'
036800960715     C                   MOVEL     'N'           WPRIMA            1
036900960531     C                   ELSE
037000980225      *
037100980403     C     TIPDOC        IFEQ      '784'
037200980225     C                   EXSR      WRTCDE
037300980225     C                   ELSE
037400980225     C                   EXSR      WRTCDA
037500000119     C                   ENDIF
037600980225      *
037700960715     C                   EXSR      AZZBOL
037800000119     C                   ENDIF
037900980226      *
038000980226     C                   CLEAR                   WANNO
038100980226     C                   CLEAR                   WLINPA
038200980226     C                   CLEAR                   WSERIE
038300980226     C                   CLEAR                   WSPEDI
038400980225      *
038500980225      * Recupero il numero di spedizione
038600000119     C                   MOVEL     STADAT        EDSD00DS
038700980225      * CONTROLLO SE DEVO FORZARE IL SECOLO
038800961007     C     §PUSEC        IFEQ      'S'
038900970227     C                   MOVEL     DA1004        WCOM16           16
039000970227     C                   MOVEL     DA1004        W1004            35
039100961022     C                   TESTN                   WCOM16               98
039200000119     C                   If          *IN98 = *ON
039300960531     C                   MOVEL     DA1004        DSSPE
039400961022     C                   ELSE
039500961120     C***                  MOVE *ZEROS    DSSPE
039600961120     C***                  MOVELERR,4     DESERR 78        Data/
039700961120     C***                  MOVEL*BLANKS   WVALOR 35        Ora
039800961120     C***                  MOVE DA1004    WVALOR           evento
039900961120     C***                  EXSR STPERR
040000000119     C                   Endif
040100000119      *
040200961007     C                   ELSE
040300961022     C                   MOVEL     DA1004        WCOM14           14
040400961022     C                   TESTN                   WCOM14               98
040500961022     C     *IN98         IFEQ      '1'
040600961022     C                   MOVEL     DA1004        DSSPE2
040700961022     C                   MOVE      DSSPE2        DSSPE
040800961022     C     WAAS2         IFLT      50
040900961022     C                   MOVEL     20            WAAS
041000961022     C                   ELSE
041100961022     C                   MOVEL     19            WAAS
041200000119     C                   ENDIF
041300000119      *
041400961111     C                   ELSE
041500961111     C                   MOVE      *ZEROS        DSSPE2
041600961111     C                   MOVE      *ZEROS        DSSPE
041700000119     C                   ENDIF
041800961007     C                   ENDIF
041900960528     C*
042000960528     C                   ENDSR
042100000120      *----------------------------------------------------------------
042200000120      *? DRED05 - DECODIFICA TIPO RECORD SD05
042300000120      *----------------------------------------------------------------
042400960531     C     DRED05        BEGSR
042500960531     C*
042600961120     C                   SETON                                        34
042700000119     C                   MOVEL     STADAT        EDSD05DS
042800960701     C                   Z-ADD     0             WDTEVE            8 0
042900960701     C                   Z-ADD     0             WHHEVE            4 0
043000961120     C                   MOVEL     DB9011        W9011
043100961120     C                   MOVEL     DB9013        W9013
043200961127     C*  Converto data e ora evento
043300961127     C                   MOVEL     DB2380        WDATA            35
043400961127     C                   MOVEL     DB2379        WFORM             3
043500961127     C                   EXSR      CNVDAT
043600961127     C                   MOVEL     *BLANKS       WCAREV            1
043700961127     C* giro data evento
043800961127     C                   MOVE      WCAMG         G02INV
043900961127     C                   MOVEL     '3'           G02ERR
044000961127     C                   CALL      'XSRDA8'
044100961127     C                   PARM                    WLBDA8
044200961127     C                   Z-ADD     G02DAT        DATAEV            8 0
044300980225      *
044400980225      * Reperisco nr.spedizione da 'AGE'
044500980225     C     DB1153        IFEQ      'AGE'
044600980225     C                   EXSR      AGGSPE
044700980225     C                   ELSE
044800980225      *
044900980225     C                   MOVE      *ZEROS        DSSPE2
045000980225     C                   MOVE      *ZEROS        DSSPE
045100980225     C                   MOVEL     ERR(4)        DESERR           78            Data/
045200980225     C                   MOVEL     *BLANKS       WVALOR           35            Ora
045300980225     C                   MOVE      DA1004        WVALOR                         evento
045400980225     C                   EXSR      STPERR
045500980225     C                   END
045600000119      *
045700000119      * Se ho trovato FNARB:
045800000119     C     *IN34         IFEQ      '0'
045900000119      *
046000000120      * 1) Se a livello testata ho agganciato con chiave parziale
046100000120      *    riaggancio la tabella PT per determinare la linea d'arrivo
046200000120      *    impostando come qualificatore la nazione destinatario
046300000120     C                   IF        WPTparz = 'S'
046400000119     C                   eval      x =1
046500000120     C                   eval      W035A = %subst(TA0004:1:31) + ARBnzd
046600000120     C     W035a         LOOKUP    CPT(X)                                 32
046700000119      *
046800000120     C                   If          *IN32 = *ON
046900000120     C                   movel     DPT(X)        EDIDSPT
047000000120     C                   movel     §PTLNP        WLNA
047100000120     C                   movel     DPU(X)        EDIDSPU
047200000120     C                   Endif
047300000120     C                   ENDIF
047400980225      *
047500000119      * 2) Cancello le vecchie note
047600961119     C                   MOVEL     'DD4440'      KNMC
047700961119     C     KRDE          DELETE    EDRDE01L                           35
047800000119     C     *IN35         DOWEQ     '0'
047900000119     C     KRDE          delete    EDRDE01L                           35
048000000119     C                   ENDDO
048100000119      *
048200000119     C                   ENDIF
048300980225      *
048400961119     C                   Z-ADD     0             RDENRP
048500980225      *  Imposto dati spedizione in stampa
048600961112     C                   Z-ADD     KAAS          WANNO             4 0
048700961112     C                   Z-ADD     KLNP          WLINPA            3 0
048800961112     C                   Z-ADD     KNRS          WSERIE            2 0
048900961112     C                   Z-ADD     KNSP          WSPEDI            7 0
049000980225      *
049100980225      *  In base al codice dell'evento determino cosa eseguire
049200960531     C                   Z-ADD     1             X
049300960612     C                   MOVEL     DB9011        WCEV
049400960612     C                   MOVE      DB9013        WCEV              6
049500980225      *
049600961121     C     WCAMG         IFGE      CDEDCM
049700961121     C     WCAMG         ANDGE     CDEDAG
049800961121     C     WCAMG         ANDGE     CDEDLA
049900980225      *
050000961112     C                   MOVEL     'S'           WCAREV            1
050100960612     C     WCEV          LOOKUP    CEV(X)                                 32
050200980225      *
050300960531     C     *IN32         IFEQ      '1'
050400000119     C                   MOVEL     DEV(X)        EDIDSCE
050500980318     C                   MOVEL     §CECCA        CD§CCA
050600980318     C                   MOVEL     §CEERR        CD§ERR
050700980318     C                   MOVEL     §CEDEC        CD§MOT
050800961125     C                   MOVEL     §CEMEM        WMEMCO            1
050900980318     C                   MOVEL     DB9011        CD§STS
051000980318     C                   MOVEL     DB9013        CD§COD
051100980225      *
051200980225      *  Se EVENTO A.T.R. stampo errore e lo tratto come evento da ignorare
051300980226     C     §CETPE        IFEQ      'V'
051400980225     C                   MOVEL     ERR(6)        DESERR                         Evento
051500980225     C                   MOVEL     *BLANKS       WVALOR                         non
051600980225     C                   MOVEL     DB9011        WCODE5            5            codifi.
051700980225     C                   MOVE      '-'           WCODE5            5            codifi.
051800980225     C                   MOVEL     WCODE5        WCODEV            9            codifi.
051900980225     C                   MOVE      DB9013        WCODEV            9            codifi.
052000980225     C                   MOVEL     WCODEV        WVALOR
052100980225     C                   EXSR      STPERR
052200980225     C                   MOVEL     'S'           §CECAR
052300980225     C                   END
052400980225      *
052500980318      *  Se EVENTO da non ignorare elaboro
052600980318     C     §CECAR        IFEQ      *BLANKS
052700980225      *
052800980225      *  Imposto evento consegna
052900961118     C     §CECEV        IFNE      'CON'
053000961121     C     WCAMG         ANDNE     CDEDAG
053100961121     C     §CECEV        ORNE      'AVV'
053200961121     C     §CECEV        ANDNE     'CON'
053300980318     C                   MOVE      §CECEV        CD§CMC
053400961024     C                   END
053500980225      *
053600980225      *  Se data corretta imposto CDE
053700960531     C     WERRO         IFNE      'S'
053800980312      *
053900980225      *  Spedizione consegnata totalmente
054000961125     C     §CETPE        IFEQ      'C'
054100961121     C     CDEDCM        IFLT      WCAMG
054200980318     C                   MOVE      WCAMG         CD§DCM
054300980318     C                   MOVE      WHHMM         CD§HMC
054400961121     C                   END
054500960531     C                   ELSE
054600980312      *  RISERVA
054700961125     C     §CETPE        IFEQ      'R'
054800980318     C                   MOVE      WCAMG         CD§DRC
054900980318     C                   MOVE      WHHMM         CD§HMC
055000961024     C                   ELSE
055100980312      *  Spedizione consegnata parzialmente
055200961125     C     §CETPE        IFEQ      'P'
055300980318     C                   MOVE      WCAMG         CD§DCM
055400980318     C                   MOVE      WHHMM         CD§HMC
055500960531     C                   ELSE
055600980312      *  Spedizione l.avviso
055700961125     C     §CETPE        IFEQ      'A'
055800980318     C                   MOVE      WHHMM         CD§HMC
055900980318     C                   MOVE      WCAMG         CD§DLA
056000960531     C                   ELSE
056100980312      *
056200980312      *  Spedizione giacente
056300980318     C                   MOVE      WHHMM         CD§HMC
056400980318     C                   MOVE      WCAMG         CD§DAG
056500960531     C                   END                                                    Eve.l.avv.
056600960531     C                   END                                                    Eve.parzi.
056700961024     C                   END                                                    RISERVA
056800961024     C                   END                                                    Eve.conse.
056900980225      *
057000980225      * Se data/ora consegna errata stp. errore
057100960531     C                   ELSE
057200960531     C                   MOVEL     ERR(1)        DESERR           78            Data/
057300960531     C                   MOVEL     *BLANKS       WVALOR           35            Ora
057400960531     C                   MOVE      DB2380        WVALOR                         evento
057500961022     C                   EXSR      STPERR
057600960531     C                   END                                                    errata
057700980225      *
057800980225      * Evento da ignorare
057900970221     C                   ELSE
058000980408      * Se previsto gestisco l'evento 'MIC'
058100980320     C     §CECAR        IFEQ      'E'
058200980320     C     *IN34         ANDEQ     '0'
058300980320     C                   EXSR      WRTEVE
058400980320     C                   END
058500000119     C                   CLEAR                   EDIDSCE
058600970221     C                   MOVEL     *BLANKS       WCAREV
058700970221     C                   SETON                                        34
058800970221     C                   Z-ADD     0             WCAMG
058900970221     C                   END                                                    ignora evento
059000980225      *
059100980225      * Evento non codificato
059200960531     C                   ELSE
059300961113     C     KNSP          IFNE      0
059400960531     C                   MOVEL     ERR(2)        DESERR                         Evento
059500960531     C                   MOVEL     *BLANKS       WVALOR                         non
059600961025     C                   MOVEL     DB9011        WCODE5            5            codifi.
059700961025     C                   MOVE      '-'           WCODE5            5            codifi.
059800961025     C                   MOVEL     WCODE5        WCODEV            9            codifi.
059900961025     C                   MOVE      DB9013        WCODEV            9            codifi.
060000961127     C                   MOVEL     WCODEV        WVALOR
060100961022     C                   EXSR      STPERR
060200961113     C                   END
060300960531     C                   END
060400980225      *
060500980225      *  Memorizzo su EDRDE le descrizioni aggiuntive
060600961118     C  N34
060700961118     CAN 32§CEDEC        IFNE      *BLANKS
060800961112     C                   MOVEL     'DB2380'      KNMC
060900961112     C                   Z-ADD     0             RDENRP
061000961112     C     KRDE          SETGT     EDRDE01L
061100961112     C     KRDE          READPE    EDRDE01L                               31
061200961119     C                   MOVEL     'N'           W2VOLT            1
061300961120     C  N31DB9011        IFEQ      '20 '
061400961119     C     DB9013        ANDEQ     '117'
061500961119     C     §CEDEC        ANDEQ     RDEVAL
061600961119     C                   MOVEL     'S'           W2VOLT
061700961119     C                   END
061800961119     C                   Z-ADD     WAAS          RDEAAS
061900961119     C                   Z-ADD     WLNP          RDELNP
062000961119     C                   Z-ADD     WNRS          RDENRS
062100961119     C                   Z-ADD     WNSP          RDENSP
062200961112     C                   MOVEL     'DB2380'      RDENMC
062300961112     C                   ADD       1             RDENRP
062400961118     C                   MOVEL     §CEDEC        RDEVAL
062500961112     C                   WRITE     EDRDE000
062600961112     C                   END
062700961112     C                   END
062800980407      *
062900980407      *  Stampo dati spedizione ed evento - solo se trovato FNARB
063000980407     C  N34              EXSR      PRTEVE
063100980407      *
063200980407     C                   ENDSR
063300980407      *-------------------------------------------------------------*
063400980407      *? PRTEVE - STAMPO EVENTO
063500980407      *----------------------------------------------------------------
063600980407     C     PRTEVE        BEGSR
063700980225      *
063800980225      *  Stampo dati spedizione
063900980407     C     DSSPE         IFNE      WSVSPE
064000040608     c                   If        In_Stampa ='S'
064100970328     C   OA              EXCEPT    TEST1
064200970328     C                   EXCEPT    DATSPE
064300040608     c                   end
064400961113     C                   MOVEL     DSSPE         WSVSPE           16
064500961113     C                   END
064600980225      *
064700980225      *  Stampo dati evento
064800980407     C     WCAREV        IFEQ      'S'
064900960701     C                   MOVE      WCAMG         WDTEVE
065000960701     C                   MOVE      WHHMM         WHHEVE
065100980225      *
065200980225      *  Se trovato stampo codice mancata consegna altrimenti stampo ***
065300961112     C     *IN32         IFEQ      '1'
065400980225     C                   MOVEL     §CEDEC        WDEC             35
065500961118     C                   MOVEL     §CECEV        WC2A              3
065600961112     C                   ELSE
065700961112     C                   MOVEL     '***'         WC2A              3
065800961112     C                   MOVEL     *BLANKS       WDEC             35
065900961112     C                   END
066000980225      *
066100980225      *  se codice evento ATR stampo in ogni caso '???'
066200980225     C     §CETPE        IFEQ      'V'
066300980225     C                   MOVEL     '???'         WC2A
066400980225     C                   END
066500980225      *
066600040608     c                   If        In_Stampa ='S'
066700960701     C   OA              EXCEPT    TEST1
066800960701     C                   EXCEPT    DATEVE
066900040608     c                   end
067000961112     C                   END
067100980225      *
067200960531     C                   ENDSR
067300980225      *-------------------------------------------------------------*
067400980225      *? AGGSPE - AGGANCIO LA SPEDIZIONE
067500980225      *----------------------------------------------------------------
067600980225     C     AGGSPE        BEGSR
067700980225      *
067800980318     C                   SETOFF                                       37
067900980410     C                   Z-ADD     *ZEROS        MICAAS
068000980410     C                   Z-ADD     *ZEROS        MICLNP
068100980410     C                   Z-ADD     *ZEROS        MICNRS
068200980410     C                   Z-ADD     *ZEROS        MICNSP
068300980318      *
068400980225      * Reperisco la chiave
068500980225     C                   EXSR      KEYSPE
068600980225      *
068700980410      * Memorizzo la Key sped. ritornata dal partner per creare il MIC
068800980410     C                   MOVEL     WAAS          MICAAS
068900980410     C                   MOVEL     WLNP          MICLNP
069000980410     C                   MOVEL     WNRS          MICNRS
069100980410     C                   MOVEL     WNSP          MICNSP
069200980410      *
069300980225      * Se la spedizione ha una figlia eseguo chain su FNCDE con quest'ultima
069400980225     C                   MOVEL     WAAS          KAAP
069500980225     C                   MOVEL     WLNP          KLPP
069600980225     C                   MOVEL     WNRS          KNRP
069700980225     C                   MOVEL     WNSP          KNSP1
069800980225     C     KLBL          SETGT     FNLBL02L
069900980225     C     KLBL          READPE    FNLBL02L                               33
070000980225      *
070100980225     C     *IN33         IFEQ      '0'
070200980225     C     LBLLAN        ANDEQ     LBLLAP
070300980225     C                   MOVEL     LBLAAN        KAAS
070400980225     C                   MOVEL     LBLLPN        KLNP
070500980225     C                   MOVEL     LBLNRN        KNRS
070600980225     C                   MOVEL     LBLNSN        KNSP
070700980225     C                   MOVEL     'S'           WMAMMA            1
070800980225     C                   ELSE
070900980225     C                   MOVEL     WAAS          KAAS
071000980225     C                   MOVEL     WLNP          KLNP
071100980225     C                   MOVEL     WNRS          KNRS
071200980225     C                   MOVEL     WNSP          KNSP
071300980225     C                   MOVEL     'N'           WMAMMA            1
071400980225     C                   END
071500980225      *
071600980225      * Controllo esistenza spedizione in FNCDE
071700980225     C     KCDE          CHAIN     FNCDE02L                           33
071800980318      *
071900980318      * Controllo esistenza spedizione in FNCDS
072000980318     C  N33KCDE          SETLL     FNCDS000                               37
072100980225      *
072200980225     C                   MOVEL     *BLANKS       WSTS
072300980225     C                   MOVEL     *BLANKS       WCODE
072400980225     C  N33              MOVEL     CDESTS        WSTS
072500980225     C  N33              MOVEL     CDECOD        WCODE
072600980225      *
072700980410      * Aggancio FNARB mamma se esiste
072800980410     C                   Z-ADD     *ZEROS        MICLNA
072900980410     C                   SETON                                        38
073000980410     C     KAAS          IFNE      MICAAS
073100980410     C     KLNP          ORNE      MICLNP
073200980410     C     KNRS          ORNE      MICNRS
073300980410     C     KNSP          ORNE      MICNSP
073400980410     C     KARBM         CHAIN     FNARB01L                           38
073500980410     C  N38              Z-ADD     ARBLNA        MICLNA
073600980410     C                   END
073700980410      *
073800980410      * Aggancio FNARB
073900980225     C                   Z-ADD     KAAS          CDEAAS
074000980225     C                   Z-ADD     KLNP          CDELNP
074100980225     C                   Z-ADD     KNRS          CDENRS
074200980225     C                   Z-ADD     KNSP          CDENSP
074300980225     C     KARB          CHAIN     FNARB01L                           34
074400980225      *
074500980225     C                   ENDSR
074600980225      *-------------------------------------------------------------*
074700980225      *? KEYSPE - RICAVO LA CHIAVE SPEDIZIONE
074800980225      *----------------------------------------------------------------
074900980225     C     KEYSPE        BEGSR
075000980225      *
075100980225     C     §PUSEC        IFEQ      'S'                                          * Secolo
075200980225     C                   MOVEL     DB1154        WCOM16           16
075300980225     C                   TESTN                   WCOM16               98
075400980225      *
075500980225     C     *IN98         IFEQ      '1'
075600980225     C                   MOVEL     DB1154        DSSPE
075700980225     C                   ELSE
075800980225     C                   MOVE      *ZEROS        DSSPE
075900980225     C                   MOVEL     ERR(4)        WCOM49           49
076000980225     C                   MOVEL     WCOM49        DSERR
076100980225     C                   MOVEL     *BLANKS       DESERR           78
076200980225     C                   MOVEL     DSERR         DESERR           78
076300980225     C                   MOVEL     *BLANKS       WVALOR           35
076400980225     C                   MOVE      DB1154        WVALOR
076500980225     C                   EXSR      STPERR
076600980225     C                   END
076700980225      *
076800980225     C                   ELSE
076900980225      *
077000980225     C                   MOVEL     DB1154        WCOM14           14
077100980225     C                   TESTN                   WCOM14               98
077200980225      *
077300980225     C     *IN98         IFEQ      '1'
077400980225     C                   MOVEL     DB1154        DSSPE2
077500980225     C                   MOVE      DSSPE2        DSSPE
077600980225      *
077700980225     C     WAAS2         IFLT      50
077800980225     C                   MOVEL     20            WAAS
077900980225     C                   ELSE
078000980225     C                   MOVEL     19            WAAS
078100980225     C                   END
078200980225      *
078300980225     C                   ELSE
078400980225     C                   MOVE      *ZEROS        DSSPE2
078500980225     C                   MOVE      *ZEROS        DSSPE
078600980225     C                   MOVEL     ERR(4)        WCOM49           49
078700980225     C                   MOVEL     WCOM49        DSERR
078800980225     C                   MOVEL     *BLANKS       DESERR           78
078900980225     C                   MOVEL     DSERR         DESERR           78
079000980225     C                   MOVEL     *BLANKS       WVALOR           35            Ora
079100980225     C                   MOVE      DB1154        WVALOR                         evento
079200980225     C                   EXSR      STPERR
079300980225     C                   END
079400980225      *
079500980225     C                   ENDIF
079600980225      *
079700980225     C                   ENDSR
079800980320      *-------------------------------------------------------------*
079900980320      *? WRTEVE - SCRIVO EVENTO DI MESSA IN CONSEGNA
080000980320      *----------------------------------------------------------------
080100980320     C     WRTEVE        BEGSR
080200980320      *
080300980408      *  Se la spedizione risulta consegnata stampo segnalazione d'errore
080400980408     C     ARBDCM        IFGT      *ZEROS
080500980408     C                   MOVEL     ERR(8)        DESERR
080600980408     C                   MOVEL     *BLANKS       WVALOR
080700980408     C                   EXSR      STPERR
080800980408     C                   ELSE
080900980408      *
081000990810     C                   Z-ADD     DATA8G        EVBDTV
081100980320     C                   Z-ADD     WORA          EVBORV
081200020906      *
081300020906     C*******            Z-ADD     WLNA          EVBFLE
081400020906      * P.O.Utente e Codice Utente e non più la LNA
081500021114     C                   Z-ADD     O36pou        EVBFLE
081600021114     C                   Z-ADD     O36cou        EVBCDU
081700020906      *
081800980410     C                   Z-ADD     ARBAAS        EVBAAS
081900980410     C                   Z-ADD     ARBLNP        EVBLNP
082000980410     C                   Z-ADD     ARBNRS        EVBNRS
082100980410     C                   Z-ADD     ARBNSP        EVBNSP
082200980410      *
082300980410      * Se esiste mamma con LNA uguale scrivo l'evento sulla mamma
082400980410     C     *IN38         IFEQ      '0'
082500980410     C     ARBLNA        ANDEQ     MICLNA
082600980410     C                   Z-ADD     MICAAS        EVBAAS
082700980410     C                   Z-ADD     MICLNP        EVBLNP
082800980410     C                   Z-ADD     MICNRS        EVBNRS
082900980410     C                   Z-ADD     MICNSP        EVBNSP
083000980410     C                   END
083100980410      *
083200980320     C                   MOVE      WCAMG         EVBDEV
083300980403     C     WHHMM         IFGT      *ZEROS
083400980320     C                   Z-ADD     WHHMM         EVBHEV
083500980403     C                   ELSE
083600980403     C                   Z-ADD     1200          EVBHEV
083700980403     C                   END
083800980320     C                   MOVEL     'MIC'         EVBCEV
083900980320      *
084000990810     C                   WRITE     FNEVB000
084100980320      *
084200980408     C                   EXSR      PRTEVE
084300980408     C                   END
084400980408      *
084500980320     C                   ENDSR
084600960528     C*-------------------------------------------------------------*
084700960531     C*? DRED10 - DECODIFICA TIPO RECORD SD10
084800960528     C*----------------------------------------------------------------
084900960531     C     DRED10        BEGSR
085000960528     C*
085100000119     C                   MOVEL     STADAT        EDSD10DS
085200960528     C*  Controllo se esiste firmatario
085300960531     C     DC3035        IFEQ      'AP'
085400980318     C                   MOVEL     DC3036        CD§NO2
085500960531     C                   END
085600960528     C*
085700960528     C                   ENDSR
085800980226      *-------------------------------------------------------------*
085900980226      *? DRED15 - DECODIFICA TIPO RECORD SD15
086000980226      *----------------------------------------------------------------
086100960531     C     DRED15        BEGSR
086200980226      *
086300000119     C                   MOVEL     STADAT        EDSD15DS
086400960531     C* Scrivo note
086500961120     C  N34              DO        5             X
086600040608     c                   If        In_Stampa ='S'
086700960701     C   OA              EXCEPT    TEST1
086800040608     c                   end
086900960531     C     D15(X)        IFNE      *BLANKS
087000961118     C                   MOVEL     'S'           WTESLB            1
087100980319     C                   MOVEL     D15(X)        CD§NO1
087200960701     C                   MOVEL     D15(X)        WNOTCO           70
087300040608     c                   If        In_Stampa ='S'
087400961118     C                   EXCEPT    NOTCON
087500040608     c                   end
087600961118     C* memorizzo nuovo FTX su FNCDE
087700961118     C                   MOVEL     'DD4440'      KNMC
087800961118     C     KRDE          SETGT     EDRDE01L
087900961118     C     KRDE          READPE    EDRDE01L                               31
088000961120     C                   MOVEL     D15(X)        WCOM35           35
088100961120     C     WCOM35        IFNE      *BLANKS
088200961120     C                   Z-ADD     WAAS          RDEAAS
088300961120     C                   Z-ADD     WLNP          RDELNP
088400961120     C                   Z-ADD     WNRS          RDENRS
088500961120     C                   Z-ADD     WNSP          RDENSP
088600961118     C                   MOVEL     'DD4440'      RDENMC
088700961118     C                   ADD       1             RDENRP
088800961118     C                   MOVEL     D15(X)        RDEVAL
088900961118     C                   WRITE     EDRDE000
089000961120     C                   END
089100961118     C* controllo se campo + lungo di 35 chr.
089200961118     C                   MOVE      D15(X)        WCOM35           35
089300961118     C     WCOM35        IFNE      *BLANKS
089400961120     C                   Z-ADD     WAAS          RDEAAS
089500961120     C                   Z-ADD     WLNP          RDELNP
089600961120     C                   Z-ADD     WNRS          RDENRS
089700961120     C                   Z-ADD     WNSP          RDENSP
089800961118     C                   MOVEL     'DD4440'      RDENMC
089900961118     C                   ADD       1             RDENRP
090000961118     C                   MOVE      D15(X)        RDEVAL
090100961118     C                   WRITE     EDRDE000
090200961118     C                   END
090300960531     C                   END
090400960531     C                   END
090500960528     C*
090600960528     C                   ENDSR
090700960528     C*-------------------------------------------------------------*
090800960531     C*? DRED20 - DECODIFICA TIPO RECORD SD20
090900960528     C*----------------------------------------------------------------
091000960531     C     DRED20        BEGSR
091100980312      *
091200000119     C                   MOVEL     STADAT        EDSD20DS
091300980319      *
091400980312      *  controllo se devo memorizzare tot.colli
091500961125     C     WMEMCO        IFEQ      'S'
091600961125     C                   MOVEL     'S'           WARR20            1
091700021204     c                   if        %subst(EDSD20DS:10:5) <> *blanks
091800961125     C                   Z-ADD     DE7224        WTOCOP            5 0
091900021204     c                   end
092000961125     C                   END
092100980319      *
092200960703     C                   Z-ADD     0             W                 3 0
092300980319      *
092400980319      *  Elaboro 20 alla volta
092500980319     C                   DO        20            X                              -- 01
092600980319     C     D20(X)        IFNE      *LOVAL                                       -- 02
092700980403     C     D20(X)        ANDNE     *BLANKS
092800980319      *
092900980319      *  Controllo validità segnacollo
093000980408     C                   MOVEL     '1'           TCTRSG            1
093100980408     C                   EXSR      CTRSGN
093200980319      *
093300980407      *  Se segnacollo non OK stampo, se OK elaboro
093400980407     C     WOKSGN        IFNE      'S'                                          -- 03
093500980407      *  Stampo ogni 3
093600980407     C                   ADD       1             W
093700980407     C                   MOVEL     D20(X)        SGT(W)
093800980407     C     W             IFEQ      3                                            -- 04
093900980407     C                   EXSR      PRTSGN
094000980407     C                   END                                                       04 --
094100980319      *
094200980407     C                   ELSE                                                   -- 03 --
094300980407      *
094400980319      *  Se previsto lo memorizzo
094500980319     C     WMEMCO        IFEQ      'S'                                          -- 04
094600980226     C                   ADD       1             YY                3 0
094700980226     C                   MOVE      D20(X)        SGN(YY)
094800980319     C                   END                                                       04 --
094900980319      *  Stampo ogni 3
095000960701     C                   ADD       1             W
095100960701     C                   MOVEL     D20(X)        SGT(W)
095200980319     C     W             IFEQ      3                                            -- 04
095300980319     C                   EXSR      PRTSGN
095400980319     C                   END                                                       04 --
095500980319     C                   END                                                       03 --
095600980319      *
095700980319     C                   END                                                        02 --
095800980319     C                   END                                                        01 --
095900980319      *
096000980312      *  Stampo resti
096100980407     C     W             IFGE      1
096200980319     C                   EXSR      PRTSGN
096300980407     C                   END
096400980312      *
096500960528     C                   ENDSR
096600980226      *----------------------------------------------------------------
096700980319      *? PRTSGN - STAMPA SEGNACOLLI
096800980226      *----------------------------------------------------------------
096900980319     C     PRTSGN        BEGSR
097000980319      *
097100980319     C     *INOA         IFEQ      '1'
097200040608     c                   If        In_Stampa ='S'
097300980319     C                   EXCEPT    TEST1
097400980319     C                   EXCEPT    DATSGN
097500040608     c                   end
097600980319     C                   SETOFF                                       OA
097700980319     C                   END
097800980319      *
097900040608     c                   If        In_Stampa ='S'
098000980319     C                   EXCEPT    DATSG1
098100040608     c                   end
098200980407      *
098300980407     C                   Z-ADD     0             W
098400980407     C                   MOVEL     *BLANKS       SGT
098500980319      *
098600980319     C                   ENDSR
098700980319      *----------------------------------------------------------------
098800980319      *? DREA05 - DECODIFICA TIPO RECORD SD05  - A.T.R. -
098900980319      *----------------------------------------------------------------
099000980319     C     DREA05        BEGSR
099100980319      *
099200980226     C                   SETON                                        34
099300000119     C                   MOVEL     STADAT        EDSD05DS
099400980226     C                   Z-ADD     0             WDTEVE            8 0
099500980226     C                   Z-ADD     0             WHHEVE            4 0
099600980226     C                   MOVEL     DB9011        W9011
099700980226     C                   MOVEL     DB9013        W9013
099800980226      *
099900980226      *  Converto data e ora evento
100000980226     C                   MOVEL     DB2380        WDATA            35
100100980226     C                   MOVEL     DB2379        WFORM             3
100200980226     C                   EXSR      CNVDAT
100300980226     C                   MOVEL     *BLANKS       WCAREV            1
100400980226      *
100500980226      * giro data evento
100600980226     C                   MOVE      WCAMG         G02INV
100700980226     C                   MOVEL     '3'           G02ERR
100800980226     C                   CALL      'XSRDA8'
100900980226     C                   PARM                    WLBDA8
101000980226     C                   Z-ADD     G02DAT        DATAEV            8 0
101100980226      *
101200980226      * Reperisco nr.spedizione da 'AGE'
101300980226     C     DB1153        IFEQ      'AGE'
101400980226      *
101500980226      * Determino se keysped = 0 o ' ' per segnalazione eccedenza
101600980226     C                   MOVEL     DB1154        WCOM14           14
101700980226     C     WCOM14        IFNE      *ZEROS
101800980226     C     WCOM14        ANDNE     *BLANKS
101900980227     C                   EXSR      KEYSPE
102000980226     C                   EXSR      ARRSPE
102100980226     C                   END
102200980226      *
102300980226     C                   ELSE
102400980226     C                   MOVE      *ZEROS        DSSPE2
102500980226     C                   MOVE      *ZEROS        DSSPE
102600980226     C                   MOVEL     ERR(4)        DESERR           78            Data/
102700980226     C                   MOVEL     *BLANKS       WVALOR           35            Ora
102800980226     C                   MOVE      DA1004        WVALOR                         evento
102900980226     C                   EXSR      STPERR
103000980226     C                   END
103100980226      *
103200980226     C                   Z-ADD     0             RDENRP
103300980227      * Imposto dati spedizione in stampa errori
103400980226     C                   Z-ADD     KAAS          WANNO             4 0
103500980226     C                   Z-ADD     KLNP          WLINPA            3 0
103600980226     C                   Z-ADD     KNRS          WSERIE            2 0
103700980226     C                   Z-ADD     KNSP          WSPEDI            7 0
103800980226      *
103900980227      * In base al codice dell'evento determino cosa eseguire
104000980226     C                   MOVEL     DB9011        WCEV
104100980226     C                   MOVE      DB9013        WCEV              6
104200980226     C                   MOVEL     'S'           WCAREV            1
104300980226     C                   Z-ADD     1             X
104400980226     C     WCEV          LOOKUP    CEV(X)                                 32
104500980226      *
104600980226     C     *IN32         IFEQ      '1'
104700000119     C                   MOVEL     DEV(X)        EDIDSCE
104800980422      *                    MOVEL§CECCA    CD$CCA
104900980407     C                   MOVEL     §CEERR        CD$ERR
105000980407     C                   MOVEL     §CEDEC        CD$MOT
105100980226     C                   MOVEL     §CEMEM        WMEMCO            1
105200980407     C                   MOVEL     DB9011        CD$STS
105300980407     C                   MOVEL     DB9013        CD$COD
105400980226      *
105500980227      * Se EVENTO non A.T.R. stampo errore e lo tratto come evento da ignorare
105600980226     C     §CETPE        IFNE      'V'
105700980226     C                   MOVEL     ERR(6)        DESERR                         Evento
105800980226     C                   MOVEL     *BLANKS       WVALOR                         non
105900980226     C                   MOVEL     DB9011        WCODE5            5            codifi.
106000980226     C                   MOVE      '-'           WCODE5            5            codifi.
106100980226     C                   MOVEL     WCODE5        WCODEV            9            codifi.
106200980226     C                   MOVE      DB9013        WCODEV            9            codifi.
106300980226     C                   MOVEL     WCODEV        WVALOR
106400980226     C                   EXSR      STPERR
106500980226     C                   MOVEL     'S'           §CECAR
106600980226     C                   END
106700980226      *
106800980407      * Se EVENTO da non ignorare elaboro
106900980428     C     §CECAR        IFNE      *BLANKS
107000980407      *
107100980407     C                   MOVE      §CECEV        CD$CMC
107200980226      *
107300980227      * Se data/ora corretti imposto FNCDA, se errati stampo errore
107400980226     C     WERRO         IFNE      'S'
107500980407     C                   MOVE      WCAMG         CD$DAM
107600980407     C                   MOVE      WHHMM         CD$HRA
107700980226     C                   ELSE
107800980226     C                   MOVEL     ERR(1)        DESERR           78
107900980226     C                   MOVEL     *BLANKS       WVALOR           35
108000980226     C                   MOVE      DB2380        WVALOR
108100980226     C                   EXSR      STPERR
108200980226     C                   END                                                    Data errata
108300980226      *
108400980226     C                   ELSE
108500000119     C                   CLEAR                   EDIDSCE
108600980226     C                   MOVEL     *BLANKS       WCAREV
108700980226     C                   SETON                                        34
108800980226     C                   Z-ADD     0             WCAMG
108900980226     C                   END                                                    Ignora evento
109000980226      *
109100980226      * Evento non codificato
109200980226     C                   ELSE
109300980226     C     KNSP          IFNE      0
109400980226     C                   MOVEL     ERR(2)        DESERR                         Evento
109500980226     C                   MOVEL     *BLANKS       WVALOR                         non
109600980226     C                   MOVEL     DB9011        WCODE5            5            codifi.
109700980226     C                   MOVE      '-'           WCODE5            5            codifi.
109800980226     C                   MOVEL     WCODE5        WCODEV            9            codifi.
109900980226     C                   MOVE      DB9013        WCODEV            9            codifi.
110000980226     C                   MOVEL     WCODEV        WVALOR
110100980226     C                   EXSR      STPERR
110200980226     C                   END
110300980226     C                   END                                                    Evento non codificat
110400980407      *
110500980407     C                   MOVEL     'N'           W2VOLT            1
110600980407      *
110700980407      *  Memorizzo su EDRDA le descrizioni aggiuntive
110800980407     C  N34
110900980407     CAN 32§CEDEC        IFNE      *BLANKS
111000980407     C                   MOVEL     'DB2380'      KNMC
111100071220     C****               Z-ADD     0             RDENRP
111200071220     C                   Z-ADD     0             RDANRP
111300980526     C     KRDA          SETGT     EDRDA01L
111400980526     C     KRDA          READPE    EDRDA01L                               31
111500980408     C  N31DB9011        IFEQ      '3  '
111600980407     C     DB9013        ANDEQ     '117'
111700980407     C     §CEDEC        ANDEQ     RDAVAL
111800980407     C                   MOVEL     'S'           W2VOLT
111900980407     C                   END
112000980407     C                   Z-ADD     WAAS          RDAAAS
112100980407     C                   Z-ADD     WLNP          RDALNP
112200980407     C                   Z-ADD     WNRS          RDANRS
112300980407     C                   Z-ADD     WNSP          RDANSP
112400980407     C                   MOVEL     'DB2380'      RDANMC
112500980407     C                   ADD       1             RDANRP
112600980407     C                   MOVEL     §CEDEC        RDAVAL
112700980416     C                   MOVEL     CD$STS        RDASTS
112800980416     C                   MOVEL     CD$COD        RDACOD
112900980416     C                   Z-ADD     CD$DAM        RDADAM
113000980416     C                   Z-ADD     CD$HRA        RDAHRA
113100010802     C     §PUATR        IFEQ      'S'
113200071221     c     rda1          tag
113300071221     C                   WRITE     EDRDA000                             99
113400071221     c                   if        *in99
113500071221     C                   ADD       1             RDANRP
113600071221     c                   goto      rda1
113700071221     c                   end
113800010802     C                   END
113900980407     C                   END
114000980226      *
114100980226      *  Stampo dati spedizione
114200980226     C  N34DSSPE         IFNE      WSVSPE
114300040608     c                   If        In_Stampa ='S'
114400980226     C   OA              EXCEPT    TEST1
114500980226     C                   EXCEPT    DATSPE
114600040608     c                   end
114700980226     C                   MOVEL     DSSPE         WSVSPE           16
114800980226     C                   END
114900980226      *
115000980226      *  Stampo dati evento
115100980226     C  N34WCAREV        IFEQ      'S'
115200980226     C                   MOVE      WCAMG         WDTEVE
115300980226     C                   MOVE      WHHMM         WHHEVE
115400980226      *
115500980226      *  Se trovato stampo codice mancata consegna altrimenti stampo ***
115600980226     C     *IN32         IFEQ      '1'
115700980226     C                   MOVEL     §CEDEC        WDEC             35
115800980226     C                   MOVEL     §CECEV        WC2A              3
115900980226     C                   ELSE
116000980226     C                   MOVEL     '***'         WC2A              3
116100980226     C                   MOVEL     *BLANKS       WDEC             35
116200980226     C                   END
116300980226      *
116400980226      *  se codice evento non ATR stampo in ogni caso '???'
116500980226     C     §CETPE        IFNE      'V'
116600980226     C                   MOVEL     '???'         WC2A
116700980226     C                   END
116800980226      *
116900040608     c                   If        In_Stampa ='S'
117000980226     C   OA              EXCEPT    TEST1
117100980226     C                   EXCEPT    DATEVE
117200040608     c                   end
117300980226     C                   END
117400980226      *
117500980226     C                   ENDSR
117600980226      *-------------------------------------------------------------*
117700980226      *? ARRSPE - AGGANCIO LA SPEDIZIONE - A.T.R. -
117800980226      *----------------------------------------------------------------
117900980226     C     ARRSPE        BEGSR
118000980226      *
118100980226     C                   MOVEL     WAAS          KAAS
118200980226     C                   MOVEL     WLNP          KLNP
118300980226     C                   MOVEL     WNRS          KNRS
118400980226     C                   MOVEL     WNSP          KNSP
118500980226     C                   MOVEL     'N'           WMAMMA            1
118600980415      *
118700980415     C                   Z-ADD     KAAS          CDAAAS
118800980415     C                   Z-ADD     KLNP          CDALNP
118900980415     C                   Z-ADD     KNRS          CDANRS
119000980415     C                   Z-ADD     KNSP          CDANSP
119100980226      *
119200980226      * Campi per stampa
119300980226     C                   Z-ADD     KAAS          CDEAAS
119400980226     C                   Z-ADD     KLNP          CDELNP
119500980226     C                   Z-ADD     KNRS          CDENRS
119600980226     C                   Z-ADD     KNSP          CDENSP
119700980226      *
119800980226      * Aggancio FNARB
119900980226     C     KARB          CHAIN     FNARB01L                           34
120000980226      *
120100980226     C                   ENDSR
120200980226      *-------------------------------------------------------------*
120300980226      *? DRAD10 - DECODIFICA TIPO RECORD SD10  - A.T.R. -
120400980226      *----------------------------------------------------------------
120500980226     C     DREA10        BEGSR
120600980226      *
120700000119     C                   MOVEL     STADAT        EDSD10DS
120800980226      *
120900980226     C                   ENDSR
121000980226      *-------------------------------------------------------------*
121100980226      *? DREA15 - DECODIFICA TIPO RECORD SD15  - A.T.R. -
121200980226      *----------------------------------------------------------------
121300980226     C     DREA15        BEGSR
121400980226      *
121500000119     C                   MOVEL     STADAT        EDSD15DS
121600980226      *
121700980226      * Scrivo note
121800980226     C  N34              DO        5             X
121900040608     c                   If        In_Stampa ='S'
122000980226     C   OA              EXCEPT    TEST1
122100040608     c                   end
122200980226      *
122300980226     C     D15(X)        IFNE      *BLANKS
122400980226     C                   MOVEL     'S'           WTESLB            1
122500980407     C                   MOVEL     D15(X)        CD$NO1
122600980226     C                   MOVEL     D15(X)        WNOTCO           70
122700040608     c                   If        In_Stampa ='S'
122800980226     C                   EXCEPT    NOTCON
122900040608     c                   end
123000980416      * memorizzo nuovo FTX su FNCDA
123100980407     C                   MOVEL     'DD4440'      KNMC
123200980526     C     KRDA          SETGT     EDRDA01L
123300980526     C     KRDA          READPE    EDRDA01L                               31
123400980407     C                   MOVEL     D15(X)        WCOM35           35
123500980407     C     WCOM35        IFNE      *BLANKS
123600980407     C                   Z-ADD     WAAS          RDAAAS
123700980407     C                   Z-ADD     WLNP          RDALNP
123800980407     C                   Z-ADD     WNRS          RDANRS
123900980407     C                   Z-ADD     WNSP          RDANSP
124000980407     C                   MOVEL     'DD4440'      RDANMC
124100980407     C                   ADD       1             RDANRP
124200980407     C                   MOVEL     D15(X)        RDAVAL
124300980416     C                   MOVEL     CD$STS        RDASTS
124400980416     C                   MOVEL     CD$COD        RDACOD
124500980416     C                   Z-ADD     CD$DAM        RDADAM
124600980416     C                   Z-ADD     CD$HRA        RDAHRA
124700010802     C     §PUATR        IFEQ      'S'
124800071221     c     rda2          tag
124900980407     C                   WRITE     EDRDA000
125000071221     c                   if        *in99
125100071221     C                   ADD       1             RDANRP
125200071221     c                   goto      rda2
125300071221     c                   end
125400010802     C                   END
125500980407     C                   END
125600980407     C* controllo se campo + lungo di 35 chr.
125700980407     C                   MOVE      D15(X)        WCOM35           35
125800980407     C     WCOM35        IFNE      *BLANKS
125900980407     C                   Z-ADD     WAAS          RDAAAS
126000980407     C                   Z-ADD     WLNP          RDALNP
126100980407     C                   Z-ADD     WNRS          RDANRS
126200980407     C                   Z-ADD     WNSP          RDANSP
126300980407     C                   MOVEL     'DD4440'      RDANMC
126400980407     C                   ADD       1             RDANRP
126500980407     C                   MOVE      D15(X)        RDAVAL
126600980416     C                   MOVEL     CD$STS        RDASTS
126700980416     C                   MOVEL     CD$COD        RDACOD
126800980416     C                   Z-ADD     CD$DAM        RDADAM
126900980416     C                   Z-ADD     CD$HRA        RDAHRA
127000010802     C     §PUATR        IFEQ      'S'
127100071221     c     rda3          tag
127200980407     C                   WRITE     EDRDA000
127300071221     c                   if        *in99
127400071221     C                   ADD       1             RDANRP
127500071221     c                   goto      rda3
127600071221     c                   end
127700010802     C                   END
127800980407     C                   END
127900980226     C                   END
128000980226     C                   END
128100980226      *
128200980226     C                   ENDSR
128300980226      *-------------------------------------------------------------*
128400980226      *? DREA20 - DECODIFICA TIPO RECORD SD20  - A.T.R. -
128500980226      *----------------------------------------------------------------
128600980226     C     DREA20        BEGSR
128700980226      *
128800000119     C                   MOVEL     STADAT        EDSD20DS
128900980226      *
129000980226      *  memorizzo colli evento
129100021204     c                   if        %subst(EDSD20DS:10:5) <> *blanks
129200980407     C                   Z-ADD     DE7224        CD$NCL
129300021204     c                   end
129400980226      *
129500980407      *  Elaboro 20 per volta
129600980226     C                   Z-ADD     0             W                 3 0
129700980407      *
129800980226     C                   DO        20            X
129900980226     C     D20(X)        IFNE      *LOVAL
130000980403     C     D20(X)        ANDNE     *BLANKS
130100980408      *
130200980408      *  Se eccedenza memorizzo segnacollo senza controlli
130300980408     C     CD$STS        IFEQ      '3  '
130400980408     C     CD$COD        ANDEQ     '288'
130500980408     C                   ADD       1             YY                3 0
130600980408     C                   MOVE      D20(X)        SGN(YY)
130700980408     C                   ELSE
130800980312      *
130900980407      *  Controllo validità segnacollo
131000980408     C                   MOVEL     '2'           TCTRSG
131100980312     C                   EXSR      CTRSGN
131200980407      *
131300980407      *  Se segnacollo non OK stampo, se OK elaboro
131400980407     C     WOKSGN        IFNE      'S'
131500980407      *  Stampo ogni 3
131600980407     C                   ADD       1             W
131700980407     C                   MOVEL     D20(X)        SGT(W)
131800980407     C     W             IFEQ      3
131900980407     C                   EXSR      PRTSGN
132000980407     C                   END
132100980407      *
132200980407     C                   ELSE
132300980407      *
132400980407      *  Se previsto lo memorizzo
132500980407     C     WMEMCO        IFEQ      'S'
132600980226     C                   ADD       1             YY                3 0
132700980226     C                   MOVE      D20(X)        SGN(YY)
132800980407     C                   END
132900980407      *  Stampo ogni 3
133000980226     C                   ADD       1             W
133100980226     C                   MOVEL     D20(X)        SGT(W)
133200980226     C     W             IFEQ      3
133300980407     C                   EXSR      PRTSGN
133400980226     C                   END
133500980312     C                   END
133600980226      *
133700980407     C                   END
133800980408     C                   END
133900980407     C                   END
134000980407      *
134100980312      *  Stampo resti
134200980312     C     W             IFGE      1
134300040608     c                   If        In_Stampa ='S'
134400980312     C                   EXCEPT    DATSG1
134500040608     c                   end
134600980312     C                   END
134700980226      *
134800980226     C                   ENDSR
134900960531     C*----------------------------------------------------------------
135000960531     C*? CNVDAT - DECODIFICA LA DATA
135100960531     C* INPUT:  - WFORM  (FORMATO DATA : 102 --> CCYYMMGG)
135200960531     C*         - WFORM  (FORMATO DATA : 203 --> CCYYMMGGHHMM)
135300960531     C*         - WDATA  (DATA ALFANUMERICA)
135400960531     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
135500960531     C*         - WHMS   (ORA NUMERICA)
135600960531     C*----------------------------------------------------------------
135700960531     C     CNVDAT        BEGSR
135800960531     C*
135900960531     C                   MOVEL     ' '           WERRO             1
136000960531     C                   Z-ADD     *ZEROS        WCAMG             8 0
136100960531     C                   Z-ADD     *ZEROS        WHMS              6 0
136200960531     C*
136300960531     C     WFORM         IFEQ      '102'                                        CCYMMDD
136400971031     C* CONTROLLO SE LA DATA E' DI SEI CIFRE
136500971031     C                   MOVEL     WDATA         WCOMO8
136600971031     C                   MOVE      WCOMO8        WCOMO2            2
136700971031     C     WCOMO2        IFEQ      *BLANKS
136800971031     C                   MOVEL     WDATA         WCOMO6            6
136900971031     C                   MOVEL     WDATA         WCOMO2
137000971031     C     WCOMO2        IFGT      '40'
137100971031     C                   MOVEL     '19'          WCOMO8
137200971031     C                   ELSE
137300971031     C                   MOVEL     '20'          WCOMO8
137400971031     C                   END
137500971031     C                   MOVE      WCOMO6        WCOMO8            8
137600971031     C                   ELSE
137700971031     C                   MOVEL     WDATA         WCOMO8            8
137800971031     C                   END
137900971031     C*
138000960531     C                   TESTN                   WCOMO8               99
138100960531     C   99              MOVE      WCOMO8        WCAMG                          *DATA
138200960531     C  N99              MOVEL     'S'           WERRO             1
138300960531     C                   END
138400971031     C*
138500960531     C     WFORM         IFEQ      '203'                                        CCYMMDDHHMM
138600960531     C                   MOVEL     WDATA         WCOM12           12
138700960531     C                   TESTN                   WCOM12               99
138800960531     C   99              MOVEL     WCOM12        WCAMG                          *DATA
138900960531     C   99              MOVE      WCOM12        WHHMM             4 0          *ORA
139000960531     C  N99              MOVEL     'S'           WERRO             1
139100960531     C                   END
139200980226      *
139300960531     C                   ENDSR
139400980226      *----------------------------------------------------------------
139500980319      *? CTRSGN - Controllo se corrisponde con bolla
139600980226      *----------------------------------------------------------------
139700960531     C     CTRSGN        BEGSR
139800980226      *
139900980408      * TCTRSG determina il tipo di controllo da effettuare
140000980408      *  "1"  = Controllo dati consegna
140100980408      *  "2"  = Controllo dati A.T.R.
140200980408      *
140300980408     C     TCTRSG        IFEQ      '1'
140400980408     C                   Z-ADD     CDENRS        WNRS
140500980408     C                   ELSE
140600980408     C                   Z-ADD     CDANRS        WNRS
140700980408     C                   END
140800980408      *
140900960531     C                   MOVEL     'N'           WOKSGN            1
141000050503      *
141100051017      *  Ricavo il numero spedizione agganciando FIARS e FNART
141200050503     C                   MOVEL     D20(X)        KNOT
141300050503      *
141400050503      *  Ora occorre controllare il TRC poichè la vista logica seleziona
141500050503      *  oltre al tipo "E"= Euroexpress and "F" segnacollo normale
141600050503      *   ovviamente qui interessa solo il tipo Euroexpress
141700051017     C     KARS          setll     FIARS000
141800051017     C     KARS          reade     FIARS000                               98
141900050503     C     *IN98         doweq     *Off
142000050503     c                   if        ArsTrc ='E'
142100050503     c                   leave
142200050503     c                   end
142300051017     C     KARS          reade     FiARS000                               98
142400050503     c                   enddo
142500050503      *
142600050503     C     *IN98         IFEQ      *Off
142700050503     C                   Z-ADD     ARSFLS        KFLS
142800050503     C                   Z-ADD     ARSLNA        KLNA
142900050503     C                   Z-ADD     ARSNRS        KNRS
143000050503     C                   Z-ADD     ARSNSC        KNSC
143100050503     C     KART1         CHAIN     FNART027                           98
143200050503     C  N98              MOVEL     'S'           WOKSGN
143300050503     c                   end
143400980312      *
143500050504     /* ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
143600050504      * SE ANCORA NON é riuscito l'aggancio, prova al vecchio modo:
143700050504     /* ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
143800980408      * Se i caratteri dal 6 al 17 son numerici eseguo il controllo
143900050504     C                   if           WOKSGN = 'N'
144000050504     C                   MOVEL     D20(X)        WCOM17           17
144100050504     C                   MOVE      WCOM17        WCOM12           12
144200050504     C                   TESTN                   WCOM12               98
144300050504      **
144400050504     C     *IN98         IFEQ      '1'
144500050504     C                   MOVEL     WCOM12        DSSGNE
144600050504      **
144700050504     C     SGFLS         IFEQ      ARBFLS
144800050504     C     SGNRS         ANDEQ     WNRS
144900050504     C                   Z-ADD     SGNSC         KNSC
145000050504     C     KART2         CHAIN     FNART000                           98
145100050504     C  N98              MOVEL     'S'           WOKSGN
145200050504     C                   END
145300050504     C                   END
145400050504     C                   ENDif
145500980226      *
145600960531     C                   ENDSR
145700961125     C*----------------------------------------------------------------
145800961125     C*? TOTCOL - Calcolo totale da consegnare
145900961125     C*----------------------------------------------------------------
146000961125     C     TOTCOL        BEGSR
146100961125     C*
146200961125     C                   Z-ADD     0             WTOCOC            5 0
146300961125     C     KART          CHAIN     FNART01L                           36
146400961125     C     *IN36         DOWEQ     '0'
146500961125     C     ARTDCM        IFEQ      0
146600961125     C                   ADD       1             WTOCOC            5 0
146700961125     C                   END
146800961125     C     KART          READE     FNART01L                               36
146900961125     C                   END
147000961125     C*
147100961125     C                   ENDSR
147200980319      *----------------------------------------------------------------
147300980320      *? SUBWRT - Determino tipo di scrittura di FNCDE e/o FNCDS
147400980319      *----------------------------------------------------------------
147500980320     C     WRTCDE        BEGSR
147600980312      *
147700980320      *-----------------------------------------------------
147800980320      *    *IN34 = '0' => esiste FNARB e viceversa
147900980320      *    *IN33 = '1' => non esiste FNCDE e viceversa
148000980319      *    *IN37 = '1' => esiste FNCDS e viceversa
148100980319      *     YY = 0 evento totale in elaborazione
148200980319      *     YY > 0 evento parziale in elaborazione
148300980320      *-----------------------------------------------------
148400980319      *
148500980320     C     *IN34         IFEQ      '0'                                          -- 1
148600980320      *
148700980320      * Verifico se segnalare se non ho consegnato tutti i colli
148800980320     C                   EXSR      MEMCOL
148900980320      *
149000980320     C     *IN33         IFEQ      '1'                                          -- 2
149100980319      *
149200980320     C     YY            IFEQ      *ZEROS                                       -- 3
149300980320     C                   EXSR      WRTOLD                                       come prima
149400980320     C                   ELSE                                                   -- 3 --
149500980320     C                   EXSR      WRCDE0                                       scrivo cde a 0
149600980320     C                   EXSR      WRTCDS                                       scrivo cds
149700980320     C                   END                                                       3 --
149800980319      *
149900980320     C                   ELSE                                                   -- 2 --
150000980319      *
150100980320     C     *IN37         IFEQ      '0'                                          -- 3
150200980319      *
150300980320     C     YY            IFEQ      *ZEROS                                       -- 4
150400980320     C                   EXSR      WRTOLD                                       come prima
150500980320     C                   ELSE                                                   -- 4 --
150600980320     C                   EXSR      UPCDE0                                       upd cde 0 + wrt cds
150700980320     C                   EXSR      WRTCDS                                       scrivo cds
150800980320     C                   END                                                       4 --
150900980319      *
151000980320     C                   ELSE                                                   -- 3 --
151100980408     C                   EXSR      MAN2VO                                       Doppio 20-117
151200980406     C                   EXSR      WRTCDS                                       wrt cds 0 o > 0
151300980320     C                   END                                                       3 --
151400980319      *
151500980320     C                   END                                                       2 --
151600980320     C                   END                                                       1 --
151700980319      *
151800980320      *  Azzero variabili
151900980320     C                   Z-ADD     0             YY
152000980320     C                   MOVEL     *BLANKS       SGN
152100980320     C                   MOVEL     *BLANKS       SGT
152200980320      *
152300980319     C                   ENDSR
152400980320      *----------------------------------------------------------------
152500980320      *? MEMCOL - Segnalazione di consegna parziale
152600980320      *----------------------------------------------------------------
152700980320     C     MEMCOL        BEGSR
152800980320      *
152900980320     C     WMEMCO        IFEQ      'S'
153000980320     C     WARR20        ANDEQ     'S'
153100980320      *
153200980320      * Loop su FNARB per verificare quanti colli sono da consegnare
153300980320     C                   EXSR      TOTCOL
153400980320      *
153500980320      * Scrivo NOTE
153600980320     C                   MOVEL     ERR(5)        ERRCOL
153700980320     C                   Z-ADD     WTOCOC        COLSPE
153800980320     C                   Z-ADD     WTOCOP        COLCON
153900980320     C                   MOVEL     ERRCOL        CD§NO1
154000980320     C                   END
154100980320      *
154200980320     C                   ENDSR
154300980406      *----------------------------------------------------------------
154400980406      *? MAN2VO - Doppia segnalazione di mancanza
154500980406      *----------------------------------------------------------------
154600980406     C     MAN2VO        BEGSR
154700980406      *
154800980406      * Se è la 2a volta che trovo l'evento 20 - 117 imposto CD§FL1 = 'S'
154900980406      *
155000980406     C                   MOVEL     W2VOLT        CD§FL1
155100980406     C     CD§FL1        IFEQ      'S'
155200980406     C     WSTS          ANDEQ     '20 '
155300980406     C     WCODE         ANDEQ     '117'
155400980406     C                   MOVEL     'S'           CD§FL2
155500980406     C                   END
155600980406      *
155700980406     C                   ENDSR
155800980319      *----------------------------------------------------------------
155900980320      *? WRTOLD - Scrittura dati FNCDE metodo standard
156000980319      *----------------------------------------------------------------
156100980320     C     WRTOLD        BEGSR
156200980319      *
156300980320      * Imposto date per confronto
156400980320     C     CD§DCM        IFEQ      *ZEROS                                       consegna
156500980320     C                   Z-ADD     CDEDCM        CD§DCM
156600980320     C                   END
156700980320     C     CD§DLA        IFEQ      *ZEROS                                       lasciato avviso
156800980320     C                   Z-ADD     CDEDLA        CD§DLA
156900980320     C                   END
157000980320     C     CD§DAG        IFEQ      *ZEROS                                       giacenza
157100980320     C                   Z-ADD     CDEDAG        CD§DAG
157200980320     C                   END
157300980320     C     CD§DRC        IFEQ      *ZEROS                                       riserva
157400980320     C                   Z-ADD     CDEDRC        CD§DRC
157500980320     C                   END
157600980320      *
157700980312      * Se ho impostato data consegna + altra data abblenco l'altra
157800980320     C     CD§DCM        IFGT      CD§DAG
157900980320     C                   Z-ADD     0             CD§DAG
158000980320     C     CD§CMC        IFNE      'MAN'
158100980320     C     CD§CMC        ANDNE     'P  '
158200980320     C     CD§CMC        ANDNE     'RES'
158300980320     C     CD§CMC        ANDNE     'IDD'
158400980320     C     CD§CMC        ANDNE     'RIT'
158500980320     C                   MOVEL     *BLANKS       CD§CMC
158600961125     C                   END
158700961125     C                   END
158800980320      *
158900980320     C     CD§DCM        IFGT      CD§DLA
159000980320     C                   Z-ADD     0             CD§DLA
159100980320     C     CD§CMC        IFEQ      'AVV'
159200980320     C     CD§CMC        OREQ      'AV7'
159300980320     C                   MOVEL     *BLANKS       CD§CMC
159400961024     C                   END
159500961125     C                   END
159600980320      *
159700980320      * Se è la seconda volta che trovo l'evento 20 - 117 imposto
159800980320      * 'S' in CD§FL1
159900980320     C                   MOVEL     W2VOLT        CD§FL1
160000980320     C     CD§FL1        IFEQ      'S'
160100961127     C     WSTS          IFEQ      '20 '
160200961127     C     WCODE         ANDEQ     '117'
160300980320     C                   MOVEL     'S'           CD§FL2
160400961127     C                   ELSE
160500980320     C                   MOVEL     *BLANKS       CD§ERR
160600961127     C                   END
160700961127     C                   END
160800980311      *
160900980311      * Aggiorno FNCDE
161000980320     C                   MOVEL     WTESLB        CDEFTX
161100980320     C                   EXSR      MOVCD§
161200961118     C     *IN33         IFEQ      '1'
161300961118     C                   MOVEL     ARBRMN        CDERMN
161400961120     C                   Z-ADD     ARBLNA        CDELNA
161500961118     C                   WRITE     FNCDE000
161600961118     C                   ELSE
161700961118     C                   UPDATE    FNCDE000
161800961118     C                   END
161900980320      *
162000960531     C                   ENDSR
162100980320      *----------------------------------------------------------------
162200980320      *? MOVCD§ - Aggiorno campi di FNCDE per aggiornamento/scrittura
162300980320      *----------------------------------------------------------------
162400980320     C     MOVCD§        BEGSR
162500980320      *
162600980320     C                   Z-ADD     CD§DCM        CDEDCM
162700980320     C                   Z-ADD     CD§HMC        CDEHMC
162800980320     C                   Z-ADD     CD§DLA        CDEDLA
162900980320     C                   Z-ADD     CD§DAG        CDEDAG
163000980320     C                   Z-ADD     CD§DRC        CDEDRC
163100980320     C                   MOVEL     CD§NO1        CDENO1
163200980320     C                   MOVEL     CD§NO2        CDENO2
163300980320     C                   MOVEL     CD§CMC        CDECMC
163400980320     C                   MOVEL     CD§CCA        CDECCA
163500980320     C                   MOVEL     CD§ERR        CDEERR
163600980320     C                   MOVEL     CD§STS        CDESTS
163700980320     C                   MOVEL     CD§COD        CDECOD
163800980320     C                   MOVEL     CD§MOT        CDEMOT
163900980320     C                   MOVEL     CD§FL1        CDEFL1
164000980320     C                   MOVEL     CD§FL2        CDEFL2
164100980320      *
164200980320     C                   ENDSR
164300980320      *----------------------------------------------------------------
164400980320      *? WRCDE0 - Scrittura dati FNCDE, solo dati base
164500980320      *----------------------------------------------------------------
164600980320     C     WRCDE0        BEGSR
164700980406      *
164800980406     C                   EXSR      MAN2VO
164900980406      *
165000980406     C                   MOVEL     CD§FL1        CDEFL1
165100980406     C                   MOVEL     CD§FL2        CDEFL2
165200980320     C                   Z-ADD     ARBLNA        CDELNA
165300980320     C                   MOVEL     ARBRMN        CDERMN
165400980320     C                   MOVEL     WTESLB        CDEFTX
165500980320     C                   WRITE     FNCDE000
165600980320      *
165700980320     C                   ENDSR
165800980320      *----------------------------------------------------------------
165900980320      *? WRTCDS - Scrivo dati FNCDS
166000980320      *----------------------------------------------------------------
166100980320     C     WRTCDS        BEGSR
166200980320      *
166300980320     C                   Z-ADD     CDEAAS        CDSAAS
166400980320     C                   Z-ADD     CDELNP        CDSLNP
166500980320     C                   Z-ADD     CDENRS        CDSNRS
166600980320     C                   Z-ADD     CDENSP        CDSNSP
166700980320     C                   Z-ADD     CDELNA        CDSLNA
166800980320     C                   Z-ADD     CDERMN        CDSRMN
166900980320     C                   Z-ADD     CD§DCM        CDSDCM
167000980320     C                   Z-ADD     CD§HMC        CDSHMC
167100980320     C                   Z-ADD     CD§DLA        CDSDLA
167200980320     C                   Z-ADD     CD§DAG        CDSDAG
167300980320     C                   Z-ADD     CD§DRC        CDSDRC
167400980320     C                   MOVEL     CD§NO1        CDSNO1
167500980320     C                   MOVEL     CD§NO2        CDSNO2
167600980320     C                   MOVEL     CD§CMC        CDSCMC
167700980320     C                   MOVEL     CD§CCA        CDSCCA
167800980320     C                   MOVEL     CD§ERR        CDSERR
167900980320     C                   MOVEL     CD§STS        CDSSTS
168000980320     C                   MOVEL     CD§COD        CDSCOD
168100980320     C                   MOVEL     CD§MOT        CDSMOT
168200980320     C                   MOVEL     CD§FL1        CDSFL1
168300980320     C                   MOVEL     CD§FL2        CDSFL2
168400980320      *
168500980320      * Se manca dettaglio PCI scrivo 1 rcd con segnacollo = blanks
168600980320     C     YY            IFEQ      0
168700980320     C                   MOVEL     *BLANKS       CDSSEE
168800980320     C                   WRITE     FNCDS000
168900980320     C                   ELSE
169000980320      *
169100980403     C                   Z-ADD     1             X
169200980403     C     X             DOWLE     YY
169300980320     C                   MOVEL     SGN(X)        CDSSEE
169400980320     C                   ADD       1             X
169500980320     C                   WRITE     FNCDS000
169600980320     C                   END
169700980320      *
169800980320     C                   END
169900980320      *
170000980320     C                   ENDSR
170100980320      *----------------------------------------------------------------
170200980320      *? UPCDE0 - Trasformo CDE esistente in CDS e azzero CDE
170300980320      *----------------------------------------------------------------
170400980320     C     UPCDE0        BEGSR
170500980320      *
170600980320     C                   Z-ADD     CDEAAS        CDSAAS
170700980320     C                   Z-ADD     CDELNP        CDSLNP
170800980320     C                   Z-ADD     CDENRS        CDSNRS
170900980320     C                   Z-ADD     CDENSP        CDSNSP
171000980320     C                   Z-ADD     CDELNA        CDSLNA
171100980320     C                   Z-ADD     CDERMN        CDSRMN
171200980320     C                   Z-ADD     CDEDCM        CDSDCM
171300980320     C                   Z-ADD     CDEHMC        CDSHMC
171400980320     C                   Z-ADD     CDEDLA        CDSDLA
171500980320     C                   Z-ADD     CDEDAG        CDSDAG
171600980320     C                   Z-ADD     CDEDRC        CDSDRC
171700980320     C                   MOVEL     CDENO1        CDSNO1
171800980320     C                   MOVEL     CDENO2        CDSNO2
171900980320     C                   MOVEL     CDECMC        CDSCMC
172000980320     C                   MOVEL     CDECCA        CDSCCA
172100980320     C                   MOVEL     CDEERR        CDSERR
172200980320     C                   MOVEL     CDESTS        CDSSTS
172300980320     C                   MOVEL     CDECOD        CDSCOD
172400980320     C                   MOVEL     CDEMOT        CDSMOT
172500980320     C                   MOVEL     CDEFL1        CDSFL1
172600980320     C                   MOVEL     CDEFL2        CDSFL2
172700980320     C                   MOVEL     *BLANKS       CDSSEE
172800980320      *
172900980320     C                   WRITE     FNCDS000
173000980320      *
173100980401     C                   Z-ADD     *ZEROS        CDEDCM
173200980401     C                   Z-ADD     *ZEROS        CDEHMC
173300980401     C                   Z-ADD     *ZEROS        CDEDLA
173400980401     C                   Z-ADD     *ZEROS        CDEDAG
173500980401     C                   Z-ADD     *ZEROS        CDEDRC
173600980401     C                   MOVEL     *BLANKS       CDENO1
173700980401     C                   MOVEL     *BLANKS       CDENO2
173800980401     C                   MOVEL     *BLANKS       CDECMC
173900980401     C                   MOVEL     *BLANKS       CDECCA
174000980401     C                   MOVEL     *BLANKS       CDEERR
174100980401     C                   MOVEL     *BLANKS       CDESTS
174200980401     C                   MOVEL     *BLANKS       CDECOD
174300980401     C                   MOVEL     *BLANKS       CDEMOT
174400980401     C                   MOVEL     *BLANKS       CDEFL1
174500980401     C                   MOVEL     *BLANKS       CDEFL2
174600980406      *
174700980406     C                   EXSR      MAN2VO
174800980406     C                   MOVEL     CD§FL1        CDEFL1
174900980406     C                   MOVEL     CD§FL2        CDEFL2
175000980320      *
175100980320     C                   UPDATE    FNCDE000
175200980320      *
175300980320     C                   ENDSR
175400980226      *----------------------------------------------------------------
175500980226      *? WRTCDA - Scrittura dati FNCDA
175600980226      *----------------------------------------------------------------
175700980407     C     WRTCDA        BEGSR
175800980226      *
175900980407      * Se non ho agganciato FNARB aggiorno FNCDA solo se eccedenza ed ho PCI
176000980407     C     *IN34         IFEQ      '1'
176100980407      *
176200980407     C     CD$STS        IFEQ      '3  '
176300980407     C     CD$COD        ANDEQ     '288'
176400980227     C     YY            ANDGT     *ZEROS
176500980407     C                   DO        YY            XY                             elaboro i segnacolli
176600980407     C                   EXSR      SUBECC
176700980407     C                   EXSR      ELACDA
176800980407     C                   END
176900980407     C                   END
177000980227      *
177100980407     C                   ELSE
177200980407     C                   EXSR      ELACDA
177300980407     C                   END
177400980226      *
177500980226      *  Azzero variabili
177600980226     C                   Z-ADD     0             YY
177700980320     C                   Z-ADD     0             XY                3 0
177800980312     C                   MOVEL     *BLANKS       SGN
177900980312     C                   MOVEL     *BLANKS       SGT
178000980226      *
178100980226     C                   ENDSR
178200980227      *----------------------------------------------------------------
178300980227      *? SUBECC - GESTIONE ECCEDENZE
178400980227      *----------------------------------------------------------------
178500980227     C     SUBECC        BEGSR
178600980227      *
178700980407     C                   SETON                                        34
178800980407      *
178900980408      *  Ricavo il numero spedizione agganciando FNARS e FNART
179000980227     C                   MOVEL     SGN(XY)       KNOT
179100021029      *  Ora occorre controllare il TRC poichè la vista logica seleziona
179200021029      *  oltre al tipo "E"= Euroexpress and "F" segnacollo normale
179300021029      *   ovviamente qui interessa solo il tipo Euroexpress
179400051017     C     KARS          setll     FIARS000
179500051017     C     KARS          reade     FIARS000                               36
179600021029     C     *IN36         doweq     *Off
179700021029     c                   if        ArsTrc ='E'
179800021029     c                   leave
179900021029     c                   end
180000051017     C     KARS          reade     FIARS000                               36
180100021029     c                   enddo
180200980227      *
180300980407     C     *IN36         IFEQ      '1'
180400980227     C                   Z-ADD     9999          WANNO
180500980227     C                   Z-ADD     999           WLINPA
180600980227     C                   Z-ADD     99            WSERIE
180700980227     C                   Z-ADD     9999999       WSPEDI
180800980227     C                   MOVEL     ERR(7)        DESERR
180900980227     C                   MOVEL     SGN(XY)       WVALOR
181000980227     C                   EXSR      STPERR
181100980227      *
181200980407     C                   ELSE
181300980227     C                   Z-ADD     ARSFLS        KFLS
181400980227     C                   Z-ADD     ARSLNA        KLNA
181500980227     C                   Z-ADD     ARSNRS        KNRS
181600980227     C                   Z-ADD     ARSNSC        KNSC
181700980227     C     KART1         CHAIN     FNART027                           36
181800980312      *
181900980407     C     *IN36         IFEQ      '1'
182000980227     C                   Z-ADD     9999          WANNO
182100980227     C                   Z-ADD     999           WLINPA
182200980227     C                   Z-ADD     99            WSERIE
182300980227     C                   Z-ADD     9999999       WSPEDI
182400980227     C                   MOVEL     ERR(7)        DESERR
182500980227     C                   MOVEL     SGN(XY)       WVALOR
182600980227     C                   EXSR      STPERR
182700980227      *
182800980407     C                   ELSE
182900980227     C                   Z-ADD     ARTAAS        WAAS
183000980227     C                   Z-ADD     ARTLNP        WLNP
183100980227     C                   Z-ADD     ARTNRS        WNRS
183200980227     C                   Z-ADD     ARTNSP        WNSP
183300980407      * Aggancio FNARB, se non trovato IN34 = '1'
183400980407     C                   EXSR      ARRSPE
183500980407     C     *IN34         IFEQ      '1'
183600980407     C                   Z-ADD     9999          WANNO
183700980407     C                   Z-ADD     999           WLINPA
183800980407     C                   Z-ADD     99            WSERIE
183900980407     C                   Z-ADD     9999999       WSPEDI
184000980407     C                   MOVEL     ERR(7)        DESERR
184100980407     C                   MOVEL     SGN(XY)       WVALOR
184200980407     C                   EXSR      STPERR
184300980407     C                   END
184400980227      *
184500980407     C                   END
184600980407     C                   END
184700980227      *
184800980227     C                   ENDSR
184900980407      *----------------------------------------------------------------
185000980407      *? ELACDA - Scrittura dati FNCDA
185100980407      *----------------------------------------------------------------
185200980407     C     ELACDA        BEGSR
185300980407      *
185400980407      * Verifico se esistono dettaglio e totale
185500980407     C                   SETOFF                                       3337
185600980407     C                   Z-ADD     CDAAAS        KAAS
185700980407     C                   Z-ADD     CDALNP        KLNP
185800980407     C                   Z-ADD     CDANRS        KNRS
185900980407     C                   Z-ADD     CDANSP        KNSP
186000980407     C                   MOVEL     'T'           KTRC
186100980407      *  Testata
186200980407     C     KCDA          SETLL     FNCDA000                               33
186300980407      *  Dettaglio
186400980408     C   33              MOVEL     'D'           KTRC
186500980407     C   33KCDA          SETLL     FNCDA000                               37
186600980407      *
186700980421      *  Per colli evento = 0 imposto come colli evento i colli spedizione
186800980421      *    se non ho PCI, in caso contrario imposto il numero di PCI ricevuti
186900980421     C     CD$NCL        IFEQ      *ZEROS
187000980421     C     YY            IFEQ      *ZEROS
187100980421     C                   Z-ADD     ARBNCL        CD$NCL
187200980421     C                   ELSE
187300980421     C                   Z-ADD     YY            CD$NCL
187400980421     C                   END
187500090121      ** -----
187600090121     c                   else
187700090121      *  significa che stanno inviando tutti i segnacolli della spedizione
187800090121      *  quindi è come se stessero dando uno status su tutta la spedizione
187900090121      *  Non si dovrà andare a vedere i segnacolli.
188000090121     c                   if        ARBNCL = CD$NCL and arbNCL = yy
188100090121     c                   z-add     0             yy
188200090121     c                   end
188300090121      ** -----
188400980421     C                   END
188500980407      *-----------------------------------------------------
188600980407      *    *IN33 = '1' => esiste FNCDA testata e viceversa
188700980407      *    *IN37 = '1' => esiste FNCDA dettaglio e viceversa
188800980407      *     YY   = 0   => evento totale in elaborazione
188900980407      *     YY   > 0   => evento parziale in elaborazione
189000980428      * --------------
189100980428      *  Gli eventi di mancanza e gli eventi con colli evento <> da colli sped.
189200980428      *  sono considerati come eventi parziali
189300980428      *-----------------------------------------------------
189400980407      *
189500980407     C     *IN33         IFEQ      '0'                                          -- 1
189600980407      *
189700980407     C     YY            IFEQ      *ZEROS                                       -- 2
189800980421     C     ARBNCL        ANDEQ     CD$NCL
189900090821     C**** §CETEA        ANDNE     'M'
190000980407     C                   EXSR      WRATST                                       scrivo testata
190100980407     C                   ELSE                                                   -- 2 --
190200980407     C                   EXSR      WRATS0                                       scrivo testata a 0
190300980407     C                   EXSR      WRADET                                       scrivo dettaglio
190400980407     C                   END                                                       2 --
190500980407      *
190600980407     C                   ELSE                                                   -- 1 --
190700980407      *
190800980407     C     *IN37         IFEQ      '0'                                          -- 2
190900980407     C                   EXSR      UPATE0                                       wrt det + upd tst 0
191000980407     C                   EXSR      WRADET                                       scrivo dettaglio
191100980407     C                   ELSE                                                   -- 2 --
191200980408     C                   EXSR      MAN2VA                                       Doppio 3-117
191300980407     C                   EXSR      WRADET                                       scrivo dettaglio
191400980407     C                   END                                                       2 --
191500980407      *
191600980407     C                   END                                                       1 --
191700980407      *
191800980407     C                   ENDSR
191900980408      *----------------------------------------------------------------
192000980408      *? MAN2VA - Doppia segnalazione di mancanza PER A.T.R.
192100980408      *----------------------------------------------------------------
192200980408     C     MAN2VA        BEGSR
192300980408      *
192400980408      * Se è la 2a volta che trovo l'evento 3 - 117 imposto CD§FL1 = 'S'
192500980408     C                   MOVEL     W2VOLT        CD§FL1
192600980408     C     CD§FL1        IFEQ      'S'
192700980408     C     WSTS          ANDEQ     '3  '
192800980408     C     WCODE         ANDEQ     '117'
192900980408     C                   MOVEL     'S'           CD§FL2
193000980408     C                   END
193100980408      *
193200980408     C                   ENDSR
193300980407      *----------------------------------------------------------------
193400980407      *? WRATST - Scrittura dati FNCDA testata
193500980407      *----------------------------------------------------------------
193600980407     C     WRATST        BEGSR
193700980407      *
193800980408      * Se è la 2a volta che trovo l'evento 3 - 117 imposto 'S' in CD§FL1
193900980407     C                   MOVEL     W2VOLT        CD§FL1
194000980407     C     CD§FL1        IFEQ      'S'
194100980408     C     WSTS          IFEQ      '3  '
194200980407     C     WCODE         ANDEQ     '117'
194300980407     C                   MOVEL     'S'           CD§FL2
194400980407     C                   ELSE
194500980407     C                   MOVEL     *BLANKS       CD$ERR
194600980407     C                   END
194700980407     C                   END
194800980407      *
194900980407     C                   EXSR      MOVCD$
195000980407     C                   MOVEL     WTESLB        CDAFTX
195100980408     C                   MOVEL     ARBRMN        CDARMN
195200980408     C                   Z-ADD     ARBLNA        CDALNA
195300980408     C                   MOVEL     'T'           CDATRC
195400980407      *
195500010802     C     §PUATR        IFEQ      'S'
195600980407     C                   WRITE     FNCDA000
195700010802     C                   END
195800980407      *
195900980407     C                   ENDSR
196000980407      *----------------------------------------------------------------
196100980407      *? MOVCD$ - Aggiorno campi di FNCDA per aggiornamento/scrittura
196200980407      *----------------------------------------------------------------
196300980407     C     MOVCD$        BEGSR
196400980407      *
196500980407     C                   Z-ADD     CD$DAM        CDADAM
196600980407     C                   Z-ADD     CD$HRA        CDAHRA
196700980407     C                   Z-ADD     CD$NCL        CDANCL
196800980422     C                   MOVEL     CD$CCA        CDACCA
196900980407     C                   MOVEL     CD$CMC        CDACMC
197000980407     C                   MOVEL     CD$NO1        CDANO1
197100980407     C                   MOVEL     *BLANKS       CDANOT
197200980407     C                   MOVEL     CD$ERR        CDAERR
197300980407     C                   MOVEL     CD$STS        CDASTS
197400980407     C                   MOVEL     CD$COD        CDACOD
197500980407     C                   MOVEL     CD$MOT        CDAMOT
197600980407     C                   MOVEL     CD§FL1        CDAFL1
197700980407     C                   MOVEL     CD§FL2        CDAFL2
197800980407      *
197900980407     C                   ENDSR
198000980407      *----------------------------------------------------------------
198100980407      *? WRATS0 - Scrittura dati FNCDA, solo dati base testata
198200980407      *----------------------------------------------------------------
198300980407     C     WRATS0        BEGSR
198400980407      *
198500980408      * Se è la 2a volta che trovo l'evento 3 - 117 imposto 'S' in CD§FL1
198600980408     C                   EXSR      MAN2VA
198700980407      *
198800980407     C                   MOVEL     CD§FL1        CDAFL1
198900980407     C                   MOVEL     CD§FL2        CDAFL2
199000980408      *
199100980408      * Se non ho il numero spedizione azzero rif. mittente e imposto linea arri
199200980408     C     CDANSP        IFGT      *ZEROS
199300980408     C                   MOVEL     ARBRMN        CDARMN
199400980408     C                   Z-ADD     ARBLNA        CDALNA
199500980408     C                   ELSE
199600980408     C                   MOVEL     *ZEROS        CDARMN
199700980408     C                   Z-ADD     WLNA          CDALNA
199800980408     C                   END
199900980408      *
200000980407     C                   MOVEL     WTESLB        CDAFTX
200100980407     C                   MOVEL     *BLANKS       CDANOT
200200980408     C                   MOVEL     'T'           CDATRC
200300980407      *
200400010802     C     §PUATR        IFEQ      'S'
200500980407     C                   WRITE     FNCDA000
200600010802     C                   END
200700980407      *
200800980407     C                   ENDSR
200900980407      *----------------------------------------------------------------
201000980407      *? WRADET - Scrivo dati FNCDA dettaglio
201100980407      *----------------------------------------------------------------
201200980407     C     WRADET        BEGSR
201300980407      *
201400980407     C                   EXSR      MOVCD$
201500980407     C                   MOVEL     WTESLB        CDAFTX
201600980408     C                   MOVEL     'D'           CDATRC
201700980408      *
201800980408      * Se non ho il numero spedizione azzero rif. mittente e imposto linea arri
201900980408     C     CDANSP        IFGT      *ZEROS
202000980408     C                   MOVEL     ARBRMN        CDARMN
202100980408     C                   Z-ADD     ARBLNA        CDALNA
202200980408     C                   ELSE
202300980408     C                   MOVEL     *ZEROS        CDARMN
202400980408     C                   Z-ADD     WLNA          CDALNA
202500980408     C                   END
202600980408      *
202700980407      *
202800980407      * Se manca dettaglio PCI scrivo 1 rcd con segnacollo = blanks
202900980407     C     YY            IFEQ      0
203000980408     C                   MOVEL     *BLANKS       CDANOT
203100010802     C     §PUATR        IFEQ      'S'
203200980407     C                   WRITE     FNCDA000
203300010802     C                   END
203400980407     C                   ELSE
203500980407      *
203600980407     C                   Z-ADD     1             X
203700980407     C     X             DOWLE     YY
203800980407     C                   MOVEL     SGN(X)        CDANOT
203900980407     C                   ADD       1             X
204000010802     C     §PUATR        IFEQ      'S'
204100980407     C                   WRITE     FNCDA000
204200010802     C                   END
204300980407     C                   END
204400980407      *
204500980407     C                   END
204600980407      *
204700980407     C                   ENDSR
204800980407      *----------------------------------------------------------------
204900980407      *? UPATE0 - Trasformo CDA testata esistente in dettaglio e azzero testata
205000980407      *----------------------------------------------------------------
205100980407     C     UPATE0        BEGSR
205200980407      *
205300980407      *  trasformo testata in dettaglio
205400980407     C                   Z-ADD     CDAAAS        KAAS
205500980407     C                   Z-ADD     CDALNP        KLNP
205600980407     C                   Z-ADD     CDANRS        KNRS
205700980407     C                   Z-ADD     CDANSP        KNSP
205800980407     C                   MOVEL     'T'           KTRC
205900980407      *
206000980407     C     KCDA          CHAIN     FNCDA000                           32
206100980407     C                   MOVEL     'D'           CDATRC
206200980407     C                   EXCEPT    AGGCDA
206300980407      *
206400980407      *  Scrivo testata
206500980407     C                   Z-ADD     *ZEROS        CDADAM
206600980407     C                   Z-ADD     *ZEROS        CDAHRA
206700980407     C                   Z-ADD     *ZEROS        CDANCL
206800980407     C                   MOVEL     *BLANKS       CDACCA
206900980407     C                   MOVEL     *BLANKS       CDACMC
207000980407     C                   MOVEL     *BLANKS       CDANO1
207100980407     C                   MOVEL     *BLANKS       CDANOT
207200980407     C                   MOVEL     *BLANKS       CDAERR
207300980407     C                   MOVEL     *BLANKS       CDASTS
207400980407     C                   MOVEL     *BLANKS       CDACOD
207500980407     C                   MOVEL     *BLANKS       CDAMOT
207600980407     C                   MOVEL     *BLANKS       CDAFL1
207700980407     C                   MOVEL     *BLANKS       CDAFL2
207800980407     C                   MOVEL     'T'           CDATRC
207900980407      *
208000980407     C     CDAFL1        IFNE      *BLANKS
208100980407     C     CDAFL2        ANDNE     *BLANKS
208200980408     C                   EXSR      MAN2VA
208300980407     C                   MOVEL     CD§FL1        CDAFL1
208400980407     C                   MOVEL     CD§FL2        CDAFL2
208500980407     C                   END
208600980407      *
208700010802     C     §PUATR        IFEQ      'S'
208800980407     C                   WRITE     FNCDA000
208900010802     C                   END
209000980407      *
209100980407     C                   ENDSR
209200980226      *----------------------------------------------------------------
209300980226      *? AZZBOL - AZZERA DATI BOLLA
209400980226      *----------------------------------------------------------------
209500960531     C     AZZBOL        BEGSR
209600980226      *
209700980226      * Eseguo clear formato record
209800960531     C                   CLEAR                   FNCDE000
209900980226     C                   CLEAR                   FNCDA000
210000980226      * Pulisco ds impostazione dati
210100000119     C                   CLEAR                   EDST15DS
210200000119     C                   CLEAR                   EDSD10DS
210300000119     C                   CLEAR                   EDSD15DS
210400000119     C                   CLEAR                   EDSD20DS
210500960531     C                   CLEAR                   WST10
210600980227      * Pulisco schiere
210700960531     C                   CLEAR                   T10
210800960531     C                   CLEAR                   T15
210900960531     C                   CLEAR                   D15
211000960531     C                   CLEAR                   D20
211100980226      * Pulisco campi
211200961120     C                   CLEAR                   WTESLB
211300961125     C                   CLEAR                   WMEMCO
211400961125     C                   CLEAR                   WTOCOP
211500961125     C                   CLEAR                   WARR20
211600980320      *
211700980320     C                   Z-ADD     *ZEROS        CD§DCM
211800980320     C                   Z-ADD     *ZEROS        CD§HMC
211900980320     C                   Z-ADD     *ZEROS        CD§DLA
212000980320     C                   Z-ADD     *ZEROS        CD§DAG
212100980320     C                   Z-ADD     *ZEROS        CD§DRC
212200980320     C                   MOVEL     *BLANKS       CD§NO1
212300980320     C                   MOVEL     *BLANKS       CD§NO2
212400980320     C                   MOVEL     *BLANKS       CD§CMC
212500980320     C                   MOVEL     *BLANKS       CD§CCA
212600980320     C                   MOVEL     *BLANKS       CD§ERR
212700980320     C                   MOVEL     *BLANKS       CD§STS
212800980320     C                   MOVEL     *BLANKS       CD§COD
212900980320     C                   MOVEL     *BLANKS       CD§MOT
213000980320     C                   MOVEL     *BLANKS       CD§FL1
213100980320     C                   MOVEL     *BLANKS       CD§FL2
213200980226      *
213300980407     C                   Z-ADD     *ZEROS        CD$DAM
213400980407     C                   Z-ADD     *ZEROS        CD$HRA
213500980407     C                   Z-ADD     *ZEROS        CD$NCL
213600980407     C                   MOVEL     *BLANKS       CD$CCA
213700980407     C                   MOVEL     *BLANKS       CD$CMC
213800980407     C                   MOVEL     *BLANKS       CD$NO1
213900980407     C                   MOVEL     *BLANKS       CD$ERR
214000980407     C                   MOVEL     *BLANKS       CD$STS
214100980407     C                   MOVEL     *BLANKS       CD$COD
214200980407     C                   MOVEL     *BLANKS       CD$MOT
214300980407      *
214400960531     C                   ENDSR
214500960531     C*----------------------------------------------------------------
214600960531     C*? STPERR: STAMPA ERRORE
214700960531     C*----------------------------------------------------------------
214800960531     C     STPERR        BEGSR
214900960531     C*
215000970206     C     *INOF         IFEQ      '1'
215100970206     C                   EXCEPT    TESTA
215200970206     C                   EXCEPT    ERRCLI
215300970206     C                   END
215400960531     C                   EXCEPT    DETER
215500960531     C*
215600960531     C                   ENDSR
215700960528     C*----------------------------------------------------------------
215800960528     C*? *INZSR - OPERAZIONI INIZIALI
215900960528     C*----------------------------------------------------------------
216000960528     C     *INZSR        BEGSR
216100980320      *
216200960530     C     *ENTRY        PLIST
216300960530     C                   PARM                    KPJBA
216400980406      *
216500960715     C                   MOVEL     'S'           WPRIMA            1
216600040608     c                   move      'N'           In_Stampa         1
216700980406      *
216800980407      * Definisco chiavi di accesso
216900960531     C     KCDE          KLIST
217000960531     C                   KFLD                    KNDC
217100960531     C                   KFLD                    KAAS
217200960528     C                   KFLD                    KLNP
217300960528     C                   KFLD                    KNRS
217400961119     C                   KFLD                    KNSP
217500961007     C     KARB          KLIST
217600961119     C                   KFLD                    KAAS
217700961119     C                   KFLD                    KLNP
217800961119     C                   KFLD                    KNRS
217900961119     C                   KFLD                    KNSP
218000961112     C     KRDE          KLIST
218100961119     C                   KFLD                    KAAP
218200961119     C                   KFLD                    KLPP
218300961119     C                   KFLD                    KNRP
218400961119     C                   KFLD                    KNSP1
218500961119     C                   KFLD                    KNMC
218600980526     C     KRDA          KLIST
218700980526     C                   KFLD                    WAAS
218800980526     C                   KFLD                    WLNP
218900980526     C                   KFLD                    WNRS
219000980526     C                   KFLD                    WNSP
219100980526     C                   KFLD                    KNMC
219200961119     C     KLBL          KLIST
219300961119     C                   KFLD                    KAAP
219400961119     C                   KFLD                    KLPP
219500961119     C                   KFLD                    KNRP
219600961119     C                   KFLD                    KNSP1
219700961125     C     KART          KLIST
219800961125     C                   KFLD                    KAAP
219900961125     C                   KFLD                    KLPP
220000961125     C                   KFLD                    KNRP
220100961125     C                   KFLD                    KNSP1
220200980312     C     KART1         KLIST
220300980312     C                   KFLD                    KFLS
220400980312     C                   KFLD                    KLNA
220500980312     C                   KFLD                    KNRS
220600980312     C                   KFLD                    KNSC
220700980312     C     KART2         KLIST
220800980312     C                   KFLD                    KAAP
220900980312     C                   KFLD                    KLPP
221000980312     C                   KFLD                    KNRP
221100980312     C                   KFLD                    KNSP1
221200980312     C                   KFLD                    KNSC
221300980407     C     KCDA          KLIST
221400980407     C                   KFLD                    KAAS
221500980407     C                   KFLD                    KLNP
221600980407     C                   KFLD                    KNRS
221700980407     C                   KFLD                    KNSP
221800980407     C                   KFLD                    KTRC
221900980407     C     KCDA1         KLIST
222000980226     C                   KFLD                    KAAS
222100980226     C                   KFLD                    KLNP
222200980226     C                   KFLD                    KNRS
222300980226     C                   KFLD                    KNSP
222400980226     C                   KFLD                    KTRC
222500980226     C                   KFLD                    KDAM
222600980226     C                   KFLD                    KCMC
222700980227     C                   KFLD                    KNOT
222800980227     C     KARS          KLIST
222900980226     C                   KFLD                    KNOT
223000980410     C     KARBM         KLIST
223100980410     C                   KFLD                    MICAAS
223200980410     C                   KFLD                    MICLNP
223300980410     C                   KFLD                    MICNRS
223400980410     C                   KFLD                    MICNSP
223500980227      * Definizione campi di work
223600980410     C     *LIKE         DEFINE    ARBLNA        MICLNA
223700980410     C     *LIKE         DEFINE    CDEAAS        MICAAS
223800980410     C     *LIKE         DEFINE    CDELNP        MICLNP
223900980410     C     *LIKE         DEFINE    CDENRS        MICNRS
224000980410     C     *LIKE         DEFINE    CDENSP        MICNSP
224100980320     C     *LIKE         DEFINE    ARTNSC        KNSC
224200980227     C     *LIKE         DEFINE    ARTLNA        KLNA
224300980227     C     *LIKE         DEFINE    ARTFLS        KFLS
224400980227     C     *LIKE         DEFINE    CDANOT        KNOT
224500980226     C     *LIKE         DEFINE    CDATRC        KTRC
224600980226     C     *LIKE         DEFINE    CDADAM        KDAM
224700980226     C     *LIKE         DEFINE    CDACMC        KCMC
224800980226     C     *LIKE         DEFINE    CDENDC        KNDC
224900960531     C     *LIKE         DEFINE    CDEAAS        KAAS
225000960531     C     *LIKE         DEFINE    CDELNP        KLNP
225100960531     C     *LIKE         DEFINE    CDENRS        KNRS
225200960531     C     *LIKE         DEFINE    CDENSP        KNSP
225300961127     C     *LIKE         DEFINE    CDESTS        WSTS
225400961127     C     *LIKE         DEFINE    CDECOD        WCODE
225500961112     C     *LIKE         DEFINE    RDENMC        KNMC
225600961021     C     *LIKE         DEFINE    TA0007        WQUAPT
225700961119     C     *LIKE         DEFINE    LBLAAP        KAAP
225800961119     C     *LIKE         DEFINE    LBLLPP        KLPP
225900961119     C     *LIKE         DEFINE    LBLNRO        KNRP
226000961119     C     *LIKE         DEFINE    LBLNSP        KNSP1
226100980320      *
226200980320     C     *LIKE         DEFINE    CDEDCM        CD§DCM
226300980320     C     *LIKE         DEFINE    CDEHMC        CD§HMC
226400980320     C     *LIKE         DEFINE    CDEDLA        CD§DLA
226500980320     C     *LIKE         DEFINE    CDEDAG        CD§DAG
226600980320     C     *LIKE         DEFINE    CDEDRC        CD§DRC
226700980320     C     *LIKE         DEFINE    CDENO1        CD§NO1
226800980320     C     *LIKE         DEFINE    CDENO2        CD§NO2
226900980320     C     *LIKE         DEFINE    CDECMC        CD§CMC
227000980320     C     *LIKE         DEFINE    CDECCA        CD§CCA
227100980320     C     *LIKE         DEFINE    CDEERR        CD§ERR
227200980320     C     *LIKE         DEFINE    CDESTS        CD§STS
227300980320     C     *LIKE         DEFINE    CDECOD        CD§COD
227400980320     C     *LIKE         DEFINE    CDEMOT        CD§MOT
227500980320     C     *LIKE         DEFINE    CDEFL1        CD§FL1
227600980320     C     *LIKE         DEFINE    CDEFL2        CD§FL2
227700980407      *
227800980407     C     *LIKE         DEFINE    CDADAM        CD$DAM
227900980407     C     *LIKE         DEFINE    CDAHRA        CD$HRA
228000980407     C     *LIKE         DEFINE    CDACMC        CD$CMC
228100980407     C     *LIKE         DEFINE    CDANCL        CD$NCL
228200980407     C     *LIKE         DEFINE    CDACCA        CD$CCA
228300980407     C     *LIKE         DEFINE    CDANO1        CD$NO1
228400980407     C     *LIKE         DEFINE    CDAERR        CD$ERR
228500980407     C     *LIKE         DEFINE    CDASTS        CD$STS
228600980407     C     *LIKE         DEFINE    CDACOD        CD$COD
228700980407     C     *LIKE         DEFINE    CDAMOT        CD$MOT
228800980320      *
228900980320      * Recupera dati dell'utente
229000980320     C                   Z-ADD     1             CODUT             1 0
229100960531     C                   CALL      'XPARUT'
229200000119     C                   PARM                    UT§DSE0F
229300960531     C                   MOVEL     RAGUT         RSUT             20
229400020925      * imposta l'utente
229500020925      *  Quindi decodifica il profilo generico EDPCEDxxx
229600020925      *   dove xxx è il TERMINAL
229700021114     C                   CLEAR                   Tibs36Ds
229800021114     c                   EVAL      I36ute = Knmus
229900021114     c                   EVAL      I36Tla = 'L'
230000021114     C                   CALL      'TIBS36R'
230100021114     C                   PARM                    Tibs36Ds
230200020911      *
230300960528     C* Imposto variabili
230400960531     C                   Z-ADD     0             KNDC
230500960528     C                   MOVEL     'N'           WFINE             1
230600960531     C                   MOVEL     'S'           WPRIMA            1
230700960531     C* RECUPERO DATA E ORA
230800960531     C                   TIME                    WHHDAT           14 0
230900970210     C                   TIME                    WHHD12           12 0
231000960531     C                   MOVE      WHHDAT        DATA8             8 0
231100970210     C                   MOVE      WHHD12        DATA6             6 0
231200970210     C                   MOVEL     DATA6         DATA              6 0
231300960531     C                   TIME                    WORA              6 0
231400980403      * Giro la data
231500980403     C                   CLEAR                   WLBDA8
231600990810     C                   Z-ADD     DATA8         G02DAT
231700980403     C                   MOVEL     *BLANKS       G02ERR
231800980403     C                   CALL      'XSRDA8'
231900980403     C                   PARM                    WLBDA8
232000990810     C                   Z-ADD     G02INV        DATA8G            8 0
232100960528     C* Caricamento Tabella codici eventi
232200960528     C                   Z-ADD     0             X                 3 0
232300960528     C                   MOVEL     'CE'          TABCOD
232400960528     C     TABCOD        CHAIN     EDTAB01L                           30
232500960528     C     *IN30         DOWEQ     '0'
232600960528     C     TABFLG        IFEQ      *BLANKS
232700960528     C                   ADD       1             X
232800960528     C                   MOVEL     TABKEY        CEV(X)
232900961118     C                   MOVEL     TABUNI        DEV(X)
233000000119     C                   MOVEL     DEV(X)        EDIDSCE
233100960528     C                   END
233200960528     C     TABCOD        READE     EDTAB01L                               30
233300960528     C                   END
233400960528     C* Caricamento Tabella Partner esteri
233500960528     C                   Z-ADD     0             X                 3 0
233600960528     C                   MOVEL     'PT'          TABCOD
233700960528     C     TABCOD        CHAIN     EDTAB01L                           30
233800960528     C     *IN30         DOWEQ     '0'
233900960528     C     TABFLG        IFEQ      *BLANKS
234000960528     C                   ADD       1             X
234100960528     C                   MOVEL     TABKEY        CPT(X)
234200000120     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
234300960531     C                   MOVEL     TABUNI        DPT(X)
234400960528     C                   END
234500960528     C     TABCOD        READE     EDTAB01L                               30
234600960528     C                   END
234700961024     C*
234800961007     C                   MOVEL     'PU'          TABCOD
234900961007     C     TABCOD        CHAIN     EDTAB01L                           30
235000961007     C     *IN30         DOWEQ     '0'
235100961007     C     TABFLG        IFEQ      *BLANKS
235200961024     C                   Z-ADD     1             X                 3 0
235300961024     C     TABKEY        LOOKUP    CPT(X)                                 32
235400961024     C     *IN32         IFEQ      '1'
235500961007     C                   MOVEL     TABUNI        DPU(X)
235600961007     C                   END
235700961024     C                   END
235800961007     C     TABCOD        READE     EDTAB01L                               30
235900961007     C                   END
236000960715     C*
236100960715     C                   MOVEL     *ALL'-'       CMP132          132
236200961024     C                   SETON                                        OA
236300961024     C                   SETON                                        OF
236400960528     C*
236500960528     C                   ENDSR
236600980407      *****************************************************************
236700980407     OFNCDA000  E            AGGCDA
236800980407     O                       CDATRC
236900980407      *****************************************************************
237000960531     OQSYSPRT   E            TESTA            01
237100960531     O                       RSUT                20
237200960531     O                                           46 '** STAMPA ERRORI RICEZIO'
237300960531     O                                              'NE DATE CONSEGNA DA PAR'
237400960531     O                                              'TNER ESTERI **'
237500960531     O                       DATA               121 '  /  /  '
237600960531     O                                          127 'Pag.:'
237700960531     O                       PAGE          Z    132
237800960531     O          E            TESTA          1 02
237900960531     O                       WORA               121 '  :  :  '
238000970206     O          E            ERRCLI         1
238100970206     O                                           15 'CODICE CLIENTE:'
238200970206     O                       §PTKSC        Z     23
238300970206     O                       WCOD                60
238400960531     O*------------------------------------
238500960531     O          E            DETER          1
238600960531     O                       DESERR              78
238700960531     O                       WVALOR             113
238800961112     O                       WANNO         Z    120
238900961112     O                       WLINPA        Z    123
239000961112     O                       WSERIE        Z    125
239100961112     O                       WSPEDI        Z    132
239200960531     O*------------------------------------
239300980320     O**      E                DETPAR
239400980320     O**                                 24 'Colli conseganti su sped'
239500980320     O**                                    'izione'
239600980320     O**                       CDEAASZ   36
239700980320     O**                       CDELNPZ   40
239800980320     O**                       CDENRSZ   43
239900980320     O**                       CDENSPZ   51
240000980320     O**                                 52 ':'
240100980320     O**      E  1             DETCOL
240200980320     O**                       SGN1      50
240300980320     O**                       SGN2    +  1
240400980320     O**                       SGN3    +  1
240500960531     O*----------------------------------------------------------------
240600960628     OSYSPRT    E            TEST1            01
240700960628     O                       RSUT                20
240800960628     O                                           45 '** STAMPA DATI RICEVUTI'
240900961024     O                                         +  1 'IN FASE DI TRASCODIFICA'
241000960628     O                                              ' DATE CONSEGNA DA PARTN'
241100960628     O                                              'ER ESTERI **'
241200960628     O                       DATA               121 '  /  /  '
241300960628     O                                          127 'Pag.:'
241400980406     O                       PAGE1         Z    132
241500961121     O          E            TEST1            02
241600960628     O                       WORA               121 '  :  :  '
241700960628     O*  Dati messaggio
241800961121     O          E            DATMSG      2  1
241900960628     O                                           18 'DATI MESSAG: MITT.'
242000960628     O                       TA0004            +  1
242100970923     O                       TA0007            +  0
242200960628     O                                         +  1 'DESTIN.'
242300960628     O                       TA0010            +  1
242400970923     O                       TAA007            +  0
242500960628     O                                         +  1 'DATA'
242600970923     O                       WDTMSG            +  1 '  /  /    '
242700960628     O                                         +  1 'ORA'
242800970923     O                       WORMSG            +  1 '  :  '
242900960628     O          E            DATMSG         1
243000960628     O                                            4 'COD.'
243100960628     O                       TA1001            +  1
243200960628     O                                         +  1 'NOME DOC.'
243300960628     O                       TA1000            +  1
243400960703     O                                         +  1 'FUNZ.'
243500960628     O                       TA1225            +  1
243600960628     O                                         +  1 'NUM.'
243700960628     O                       TA1004            +  1
243800970923     O                                         +  2 'DATA'
243900961112     O                       WDADOC            +  1 '  /  /    '
244000961112     O                                         +  1 'ORA'
244100961112     O                       WHHDOC            +  1 '  :  '
244200960715     O*  Dati Partner
244300960715     O          E            DATPAM         1
244400960715     O                                           14 'PARTNER MITT.:'
244500960715     O                       TE3036            +  1
244600960715     O          E            DATPAD         1
244700960715     O                                           14 'PARTNER DEST.:'
244800960715     O                       TE3036            +  1
244900960628     O*  Note intero documento
245000960628     O          E            NOTMSG      1
245100960628     O                                           18 'NOTE DOCUMENTO...:'
245200960628     O                       TC4451            +  1
245300960628     O          E            NOTMS1         1
245400960628     O                       WNOT              +  1
245500960628     O*  Spedizione
245600960715     O          E            DATSPE      1  1
245700960715     O                       CMP132             132
245800960715     O          E            DATSPE      1
245900961111     O                                           14 'NR.SPEDIZIONE:'
246000961119     O                       KAAS              +  1
246100961119     O                       KLNP          Z   +  1
246200961119     O                       KNRS          Z   +  1
246300961119     O                       KNSP          Z   +  1
246400961007     O                     34                  +  2 'NON TROVATA FRA LE BOLLE'
246500961007     O                     34                  +  1 'IN ARRIVO'
246600970227     O                     34                  +  5 'RIF. PARTNER:'
246700970227     O                     34W1004             +  1
246800960628     O*  Evento
246900960715     O          E            DATEVE      1
247000960628     O                                           13 'CODICE EVENTO'
247100960628     O                       DB9011            +  1
247200960628     O                       DB9013            +  1
247300960628     O                                         +  1 'DATA'
247400960628     O                       WDTEVE            +  1
247500960628     O                                         +  1 'ORA'
247600960628     O                       WHHEVE            +  1
247700961112     O                                         +  3 '-'
247800961112     O                       WC2A              +  2
247900961112     O                       WDEC              +  1
248000961112     O***                              +  1 'RIFER.SPEDIZIONE:'
248100961112     O***                      DB1153  +  1
248200961112     O***                      DB1154  +  1
248300960628     O*  Note Consegna
248400960715     O          E            NOTCON      1
248500960628     O                                           13 'NOTE CONSEGNA'
248600960628     O                       WNOTCO            +  1
248700960628     O*  Segnacolli
248800960628     O          E            DATSGN      1
248900960628     O                                           14 'NR.SEGNACOLLI:'
249000960628     O          E            DATSG1      1
249100960628     O                       SGN1                50
249200960628     O                       SGN2              +  1
249300960628     O                       SGN3              +  1
249400980225      *----------------------------------------------------------------
249500980408** ERR                                                               *
249600960531Data/Ora evento giacenza errato
249700960531Codice evento non codificato su tabella 2A
249800960531Riferimento mittente non numerico
249900961127Numero di spedizione errato !!  -  Codice evento xxx - xxx del xxxxxxxx
250000961203Colli xxxxx DI xxxxx
250100980225ATTENZIONE - Evento non congruente con il tipo messaggio -
250200980227Evento eccedenza - spedizione non trovata
250300980408Ricevuto evento di messa in consegna per sped. già consegnata
