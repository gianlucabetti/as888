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
003000121105     D CPT             S             35    DIM(200)
003100121105     D CPTparz         S             35    DIM(%elem(CPT))
003200121105     D DPT             S             90    DIM(%elem(CPT))
003300121105     D CPU             S             35    DIM(%elem(CPT))
003400121105     D DPU             S             90    DIM(%elem(CPT))
003500121105     D TRUL0SDS      E DS
003600121105      *
003700980312      * Tabella codici eventi
003800960612     D CEV             S              6    DIM(200)
003900961118     D DEV             S             90    DIM(200)
004000980312      * Nr. documento alfanumerico da già decodificato
004100960531     D NDN             S              1    DIM(15)
004200000119      * Nr. documento alfanumerico da decodificare
004300960531     D NDA             S              1    DIM(35)
004400000119      * Errori
004500000119     D ERR             S             70    DIM(8) CTDATA PERRCD(1)
004600000119      *---------------------------------------------------------------------*
004700000119      * DS
004800000119      *---------------------------------------------------------------------*
004900000119      * .. per scompozione dati da spedire a seconda del tipo record
005000000119     D EDST00DS      E DS
005100000119     D EDST05DS      E DS
005200000119     D EDST10DS      E DS
005300000119     D  T10                   48    103    DIM(2)
005400000119     D EDST15DS      E DS
005500000119     D  T15                   13    362    DIM(5)
005600000119     D EDSD00DS      E DS
005700000119     D EDSD05DS      E DS
005800000119     D EDSD10DS      E DS
005900000119     D EDSD15DS      E DS
006000000119     D  D15                   13    362    DIM(5)
006100000119     D EDSD20DS      E DS
006200000119     D  D20                   33    732    DIM(20)
006300000119      *  Ds per scrittura dati località
006400960528     D WST10           DS
006500960528     D  WTPL                   1      3
006600960528     D  WLOC                   4     28
006700000119      *---------------------------------------------------------------------*
006800000119      * .. per reperimento tipo record di EDIFTSTA da scrivere
006900000119      *                    .. 5 - 8  : tipo record
007000960528     D WDAT            DS           382
007100960528     D  STATPR                 5      8
007200000119      *---------------------------------------------------------------------*
007300960528     D KPJBA         E DS
007400121112     d kpjbusav        S                   like(Kpjbu)
007500000119     D UT§DSE0F      E DS
007600020911      *
007700021114     D Tibs36ds      E DS
007800960528     D CNCR80        E DS
007900000119     D EDIDSCE       E DS
008000000119     D EDIDSPT       E DS
008100000119     D EDIDSPU       E DS
008200960531     D DS2A          E DS
008300980226      *
008400980407      *  DS x composizione errore nr.spedizione
008500961120     D DSERR           DS
008600961120     D  W9011                 50     52
008700961120     D  W9013                 56     58
008800961127     D  DATAEV                64     71  0
008900960528     D*  DS x scomposizione segnacollo
009000980312     D DSSGNE          DS
009100980312     D  SGFLS                  1      3  0
009200980312     D  SGNRS                  4      5  0
009300980312     D  SGNSC                  6     12  0
009400960528     D DSSGN           DS
009500961029     D  SGNFLS                 1      3  0
009600960528     D  SGNLNA                 4      6  0
009700960528     D  SGNNRS                 7      8  0
009800960528     D  SGNNSC                 9     15  0
009900961007     D DSSPE           DS
010000960531     D  WAAS                   1      4  0
010100960531     D  WLNP                   5      7  0
010200960531     D  WNRS                   8      9  0
010300960715     D  WNSP                  10     16  0
010400961007     D DSSPE2          DS
010500961007     D  WAAS2                  1      2  0
010600961007     D  WLNP2                  3      5  0
010700961007     D  WNRS2                  6      7  0
010800961007     D  WNSP2                  8     14  0
010900960528     D WLBDA8          DS
011000960528     D  G02DAT                 1      8  0
011100960528     D  G02INV                 9     16  0
011200960528     D  G02ERR                17     17
011300960528     D  G02TGI                18     22  0
011400960701     D*  DS x stampa segnacollo
011500960701     D                 DS
011600960701     D  SGN1                   1     35
011700960701     D  SGN2                  36     70
011800960701     D  SGN3                  71    105
011900000120     D  SGT                    1    105    DIM(3)
012000961125     D*  Ds per composizione errore <> nr.colli
012100961127     D ERRCOL          DS            35
012200961203     D  COLCON                 7     11  0
012300961203     D  COLSPE                16     20  0
012400000120      *---------------                                                      *
012500000120     D W035A           s             35
012600000120     D Wptparz         s              1
012700000119      *---------------------------------------------------------------------*
012800960528     IIFTSTA00
012900960528     I              SSIFTSTA                    STADAT
013000980225      *---------------------------------------------------------------------*
013100980225      * Ciclo principale
013200980225      *---------------------------------------------------------------------*
013300960528      * Posizionamento iniziale sul file  (prima lettura)
013400960528     C                   EXSR      REDFIL
013500980225      *
013600960531      * Lettura fino a fine file
013700960531     C     WFINE         DOWNE     'S'                                          - Fine file -
013800980225      *
013900980225      *  Controllo tipo record letto per eseguire decodifica
014000980225      *
014100980225      * Comuni
014200960715     C     STATPR        CASEQ     'ST00'        DRET00
014300960715     C     STATPR        CASEQ     'ST05'        DRET05
014400960715     C     STATPR        CASEQ     'ST10'        DRET10
014500960531     C     STATPR        CASEQ     'ST15'        DRET15
014600960531     C     STATPR        CASEQ     'SD00'        DRED00
014700980225     C                   END
014800980225      *
014900980225      * Cargo manifest
015000980402     C     TIPDOC        IFEQ      '784'
015100960531     C     STATPR        CASEQ     'SD05'        DRED05
015200960531     C     STATPR        CASEQ     'SD10'        DRED10
015300960531     C     STATPR        CASEQ     'SD15'        DRED15
015400960531     C     STATPR        CASEQ     'SD20'        DRED20
015500980225     C                   END
015600000119     C                   ENDIF
015700980225      *
015800980225      * Arrival notice
015900980225     C     TIPDOC        IFEQ      '781'
016000980225     C     STATPR        CASEQ     'SD05'        DREA05
016100980225     C     STATPR        CASEQ     'SD10'        DREA10
016200980225     C     STATPR        CASEQ     'SD15'        DREA15
016300980225     C     STATPR        CASEQ     'SD20'        DREA20
016400980225     C                   END
016500000119     C                   ENDIF
016600980225      *
016700960531      * Cancello record
016800960531     C                   DELETE    EDIFTSTA
016900980225      *
017000960531      * Lettura successiva
017100960531     C                   EXSR      REDFIL
017200960531     C                   END                                                    - Fine file -
017300980225      *
017400980225      * Ultimo record
017500960715     C     WPRIMA        IFEQ      'N'
017600980403     C     TIPDOC        IFEQ      '784'
017700980225     C                   EXSR      WRTCDE
017800980225     C                   ELSE
017900980225     C                   EXSR      WRTCDA
018000000119     C                   ENDIF
018100000119     C                   ENDIF
018200980225      *
018300980225      * Fine pgm.
018400960531     C                   SETON                                        LR
018500960528     C*----------------------------------------------------------------
018600960528     C*? REDFIL - LETTURA PROSSIMO RECORD
018700960531     C*----------------------------------------------------------------
018800960531     C     REDFIL        BEGSR
018900960531     C*
019000960531     C* Lettura file
019100960531     C                   READ      EDIFTSTA                               30
019200960531     C                   MOVEL     STADAT        WDAT
019300960531     C     *IN30         IFEQ      '1'
019400960531     C                   MOVEL     'S'           WFINE
019500960531     C                   END
019600960531     C*
019700960531     C                   ENDSR
019800960531     C*----------------------------------------------------------------
019900960531     C*? DRET00 - DECODIFICA TIPO RECORD ST00
020000960531     C*----------------------------------------------------------------
020100960531     C     DRET00        BEGSR
020200980226      *
020300970206     C                   SETON                                        OF
020400000119     C                   MOVEL     STADAT        EDST00DS
020500000119     C                   clear                   WDADOC            8 0
020600000119     C                   clear                   WHHDOC            4 0
020700980421      *
020800980421      * Se leggo nuovo record scrivo FNCDE o FNDCA
020900980421     C     WPRIMA        IFEQ      'N'
021000980421      *
021100980421     C     TIPDOC        IFEQ      '784'
021200980421     C                   EXSR      WRTCDE
021300980421     C                   ELSE
021400980421     C                   EXSR      WRTCDA
021500000119     C                   ENDIF
021600980421      *
021700980421     C                   EXSR      AZZBOL
021800980421     C                   MOVEL     'S'           WPRIMA            1
021900000119     C                   ENDIF
022000980226      *
022100000119      * Se è stato modificato il codice partner controllo se è codificato
022200000119     C                   IF        WCODPT <> TA0004  or  WQUAPT <> TA0007
022300960531     C                   MOVEL     TA0004        WCODPT           35
022400961021     C                   MOVEL     TA0007        WQUAPT
022500000119     C                   eval      *inOF = *on
022600000119     C                   eval      *inOA = *on
022700980420      *
022800980420      *  In base al codice del partner determino la linea d'arrivo
022900000119      *  A livello di spedizione riaggancio la tabella con la nazione come
023000000119      *  qualificatore.
023100000119     C                   CLEAR                   WLNA
023200960531     C                   Z-ADD     1             X
023300000119      * Primo tentativo rep. da codice + qualificatore
023400961021     C                   MOVEL     TA0004        WCOD             35
023500961021     C                   MOVE      TA0007        WCOD
023600961021     C     WCOD          LOOKUP    CPT(X)                                 32
023700000120     C                   If        *IN32 = *on
023800000120     c                   clear                   WPTparz
023900000119      * Secondo tentativo rep. da codice
024000000120     C                   Else
024100000119     C                   eval      x = 1
024200000120     C                   eval      W035A = %subst(TA0004:1:31)
024300000120     C     W035A         lookup    CPTparz(X)                             32
024400000120     c   32              eval      WPTparz = 'S'
024500000120     C                   Endif
024600000119      *
024700960531     C     *IN32         IFEQ      '1'
024800000119     C                   MOVEL     DPT(X)        EDIDSPT
024900960531     C                   MOVEL     §PTLNP        WLNA              3 0
025000000119     C                   MOVEL     DPU(X)        EDIDSPU
025100000119     C                   ENDIF
025200980225      *
025300000119     C                   ENDIF
025400980225      *
025500980225      *  Converto data e ora DOCUMENNTO
025600960703     C                   MOVEL     TA2380        WDATA            35
025700960703     C                   MOVEL     TA2379        WFORM             3
025800960703     C                   EXSR      CNVDAT
025900980225      *  Data corretta
026000960703     C     WERRO         IFNE      'S'
026100971031     C                   MOVEL     WCAMG         WAA2              4 0
026200971031     C                   MOVE      WCAMG         WMMGG             4 0
026300971031     C                   MOVE      WMMGG         WGG2              2 0
026400971031     C                   MOVEL     WMMGG         WMM2              2 0
026500971031     C                   MOVEL     WGG2          WGGMM             4 0
026600971031     C                   MOVE      WMM2          WGGMM             4 0
026700971031     C                   MOVEL     WGGMM         WDADOC
026800971031     C                   MOVE      WAA2          WDADOC
026900960703     C                   MOVE      WHHMM         WHHDOC
027000960703     C                   END
027100970923     C*  Giro la data messaggio
027200971031     C***§                 MOVELTA0017    WAA2    40
027300970923     C***§                 MOVELTA0017    WDTMSG  80
027400970923     C***§                 MOVE TA0017    WGG2    20
027500970923     C***§                 MOVELWGG2      WDTMSG
027600970923     C***§                 MOVE WAA2      WDTMSG
027700970923     C                   MOVEL     TA0017        WDATA
027800970923     C                   MOVEL     '102'         WFORM
027900970923     C                   EXSR      CNVDAT
028000980225      *
028100980225      *  Data corretta imposto DATA PREPARAZ. MSG
028200970923     C     WERRO         IFNE      'S'
028300971031     C                   MOVEL     WCAMG         WAA2              4 0
028400971031     C                   MOVE      WCAMG         WMMGG             4 0
028500971031     C                   MOVE      WMMGG         WGG2              2 0
028600971031     C                   MOVEL     WMMGG         WMM2              2 0
028700971031     C                   MOVEL     WGG2          WGGMM             4 0
028800971031     C                   MOVE      WMM2          WGGMM             4 0
028900971031     C                   MOVEL     WGGMM         WDTMSG            8 0
029000971031     C                   MOVE      WAA2          WDTMSG
029100970923     C                   END
029200980225      *
029300980225      *  ORA corretta imposto WORMSG
029400970923     C                   CLEAR                   WORMSG
029500970923     C                   MOVE      TA0019        WCOM4A            4
029600970923     C                   TESTN                   WCOM4A               98
029700970923     C     *IN98         IFEQ      '1'
029800970923     C                   MOVE      TA0019        WORMSG            4 0
029900970923     C                   ELSE
030000970923     C                   CLEAR                   WORMSG
030100970923     C                   END
030200000119      *
030300040608     c                   If        In_Stampa ='S'
030400960701     C   OA              EXCEPT    TEST1
030500960701     C                   EXCEPT    DATMSG
030600040608     c                   end
030700980225      *
030800980225     C                   MOVEL     TA1001        TIPDOC            3
030900980225      *
031000960531     C                   ENDSR
031100980225      *----------------------------------------------------------------
031200980225      *? DRET05 - DECODIFICA TIPO RECORD ST05
031300980225      *----------------------------------------------------------------
031400960715     C     DRET05        BEGSR
031500000119      *
031600000119      *  Imposto dati ST05
031700000119     C                   MOVEL     STADAT        EDST05DS
031800000119      *  Stampa
031900040608     c                   If        In_Stampa ='S'
032000960715     C   OA              EXCEPT    TEST1
032100000119      *
032200000119     C                   SELECT
032300000119     C                   WHEN       TE3035 = 'SF'
032400000119     C                   except    DATPAM
032500000119     C                   WHEN       TE3035 = 'ST'
032600000119     C                   except    DATPAD
032700000119     C                   ENDSL
032800040608     c                   end
032900000119      *
033000960715     C                   ENDSR
033100000119      *----------------------------------------------------------------
033200000119      *? DRET10 - DECODIFICA TIPO RECORD ST10
033300000119      *----------------------------------------------------------------
033400960531     C     DRET10        BEGSR
033500000119      *
033600000119     C                   MOVEL     STADAT        EDST10DS
033700000119      *
033800960531     C                   ENDSR
033900000119      *----------------------------------------------------------------
034000000119      *? DRET15 - DECODIFICA TIPO RECORD ST15
034100000119      *----------------------------------------------------------------
034200960531     C     DRET15        BEGSR
034300000119      *
034400000119     C                   MOVEL     STADAT        EDST15DS
034500000119      *  Stampo note documento se ci sono
034600960701     C     TC4451        IFNE      *BLANKS
034700960701     C                   Z-ADD     1             X
034800040608     c                   If        In_Stampa ='S'
034900960701     C   OA              EXCEPT    TEST1
035000960701     C                   EXCEPT    NOTMSG
035100040608     c                   end
035200960701     C                   DO        4             X
035300960701     C     T15(X)        IFNE      *BLANKS
035400960703     C                   MOVEL     T15(X)        WNOT             70
035500040608     c                   If        In_Stampa ='S'
035600960701     C                   EXCEPT    NOTMS1
035700040608     c                   end
035800000119     C                   ENDIF
035900000119     C                   ENDDO
036000000119      *
036100000119     C                   ENDIF
036200000119      *
036300960531     C                   ENDSR
036400000119      *----------------------------------------------------------------
036500000119      *? DRED00 - DECODIFICA TIPO RECORD SD00
036600000119      *----------------------------------------------------------------
036700960531     C     DRED00        BEGSR
036800980225      *
036900980225      * Se leggo nuovo record scrivo FNCDE o FNCDA
037000960715     C     WPRIMA        IFEQ      'S'
037100960715     C                   MOVEL     'N'           WPRIMA            1
037200960531     C                   ELSE
037300980225      *
037400980403     C     TIPDOC        IFEQ      '784'
037500980225     C                   EXSR      WRTCDE
037600980225     C                   ELSE
037700980225     C                   EXSR      WRTCDA
037800000119     C                   ENDIF
037900980225      *
038000960715     C                   EXSR      AZZBOL
038100000119     C                   ENDIF
038200980226      *
038300980226     C                   CLEAR                   WANNO
038400980226     C                   CLEAR                   WLINPA
038500980226     C                   CLEAR                   WSERIE
038600980226     C                   CLEAR                   WSPEDI
038700980225      *
038800980225      * Recupero il numero di spedizione
038900000119     C                   MOVEL     STADAT        EDSD00DS
039000980225      * CONTROLLO SE DEVO FORZARE IL SECOLO
039100961007     C     §PUSEC        IFEQ      'S'
039200970227     C                   MOVEL     DA1004        WCOM16           16
039300970227     C                   MOVEL     DA1004        W1004            35
039400961022     C                   TESTN                   WCOM16               98
039500000119     C                   If          *IN98 = *ON
039600960531     C                   MOVEL     DA1004        DSSPE
039700961022     C                   ELSE
039800961120     C***                  MOVE *ZEROS    DSSPE
039900961120     C***                  MOVELERR,4     DESERR 78        Data/
040000961120     C***                  MOVEL*BLANKS   WVALOR 35        Ora
040100961120     C***                  MOVE DA1004    WVALOR           evento
040200961120     C***                  EXSR STPERR
040300000119     C                   Endif
040400000119      *
040500961007     C                   ELSE
040600961022     C                   MOVEL     DA1004        WCOM14           14
040700961022     C                   TESTN                   WCOM14               98
040800961022     C     *IN98         IFEQ      '1'
040900961022     C                   MOVEL     DA1004        DSSPE2
041000961022     C                   MOVE      DSSPE2        DSSPE
041100961022     C     WAAS2         IFLT      50
041200961022     C                   MOVEL     20            WAAS
041300961022     C                   ELSE
041400961022     C                   MOVEL     19            WAAS
041500000119     C                   ENDIF
041600000119      *
041700961111     C                   ELSE
041800961111     C                   MOVE      *ZEROS        DSSPE2
041900961111     C                   MOVE      *ZEROS        DSSPE
042000000119     C                   ENDIF
042100961007     C                   ENDIF
042200960528     C*
042300960528     C                   ENDSR
042400000120      *----------------------------------------------------------------
042500000120      *? DRED05 - DECODIFICA TIPO RECORD SD05
042600000120      *----------------------------------------------------------------
042700960531     C     DRED05        BEGSR
042800960531     C*
042900961120     C                   SETON                                        34
043000000119     C                   MOVEL     STADAT        EDSD05DS
043100960701     C                   Z-ADD     0             WDTEVE            8 0
043200960701     C                   Z-ADD     0             WHHEVE            4 0
043300961120     C                   MOVEL     DB9011        W9011
043400961120     C                   MOVEL     DB9013        W9013
043500961127     C*  Converto data e ora evento
043600961127     C                   MOVEL     DB2380        WDATA            35
043700961127     C                   MOVEL     DB2379        WFORM             3
043800961127     C                   EXSR      CNVDAT
043900961127     C                   MOVEL     *BLANKS       WCAREV            1
044000961127     C* giro data evento
044100961127     C                   MOVE      WCAMG         G02INV
044200961127     C                   MOVEL     '3'           G02ERR
044300961127     C                   CALL      'XSRDA8'
044400961127     C                   PARM                    WLBDA8
044500961127     C                   Z-ADD     G02DAT        DATAEV            8 0
044600980225      *
044700980225      * Reperisco nr.spedizione da 'AGE'
044800980225     C     DB1153        IFEQ      'AGE'
044900980225     C                   EXSR      AGGSPE
045000980225     C                   ELSE
045100980225      *
045200980225     C                   MOVE      *ZEROS        DSSPE2
045300980225     C                   MOVE      *ZEROS        DSSPE
045400980225     C                   MOVEL     ERR(4)        DESERR           78            Data/
045500980225     C                   MOVEL     *BLANKS       WVALOR           35            Ora
045600980225     C                   MOVE      DA1004        WVALOR                         evento
045700980225     C                   EXSR      STPERR
045800980225     C                   END
045900000119      *
046000000119      * Se ho trovato FNARB:
046100000119     C     *IN34         IFEQ      '0'
046200000119      *
046300000120      * 1) Se a livello testata ho agganciato con chiave parziale
046400000120      *    riaggancio la tabella PT per determinare la linea d'arrivo
046500000120      *    impostando come qualificatore la nazione destinatario
046600000120     C                   IF        WPTparz = 'S'
046700000119     C                   eval      x =1
046800000120     C                   eval      W035A = %subst(TA0004:1:31) + ARBnzd
046900000120     C     W035a         LOOKUP    CPT(X)                                 32
047000000119      *
047100000120     C                   If          *IN32 = *ON
047200000120     C                   movel     DPT(X)        EDIDSPT
047300000120     C                   movel     §PTLNP        WLNA
047400000120     C                   movel     DPU(X)        EDIDSPU
047500000120     C                   Endif
047600000120     C                   ENDIF
047700980225      *
047800000119      * 2) Cancello le vecchie note
047900961119     C                   MOVEL     'DD4440'      KNMC
048000961119     C     KRDE          DELETE    EDRDE01L                           35
048100000119     C     *IN35         DOWEQ     '0'
048200000119     C     KRDE          delete    EDRDE01L                           35
048300000119     C                   ENDDO
048400000119      *
048500000119     C                   ENDIF
048600980225      *
048700961119     C                   Z-ADD     0             RDENRP
048800980225      *  Imposto dati spedizione in stampa
048900961112     C                   Z-ADD     KAAS          WANNO             4 0
049000961112     C                   Z-ADD     KLNP          WLINPA            3 0
049100961112     C                   Z-ADD     KNRS          WSERIE            2 0
049200961112     C                   Z-ADD     KNSP          WSPEDI            7 0
049300980225      *
049400980225      *  In base al codice dell'evento determino cosa eseguire
049500960531     C                   Z-ADD     1             X
049600960612     C                   MOVEL     DB9011        WCEV
049700960612     C                   MOVE      DB9013        WCEV              6
049800980225      *
049900961121     C     WCAMG         IFGE      CDEDCM
050000961121     C     WCAMG         ANDGE     CDEDAG
050100961121     C     WCAMG         ANDGE     CDEDLA
050200980225      *
050300961112     C                   MOVEL     'S'           WCAREV            1
050400960612     C     WCEV          LOOKUP    CEV(X)                                 32
050500980225      *
050600960531     C     *IN32         IFEQ      '1'
050700000119     C                   MOVEL     DEV(X)        EDIDSCE
050800980318     C                   MOVEL     §CECCA        CD§CCA
050900980318     C                   MOVEL     §CEERR        CD§ERR
051000980318     C                   MOVEL     §CEDEC        CD§MOT
051100961125     C                   MOVEL     §CEMEM        WMEMCO            1
051200980318     C                   MOVEL     DB9011        CD§STS
051300980318     C                   MOVEL     DB9013        CD§COD
051400980225      *
051500980225      *  Se EVENTO A.T.R. stampo errore e lo tratto come evento da ignorare
051600980226     C     §CETPE        IFEQ      'V'
051700980225     C                   MOVEL     ERR(6)        DESERR                         Evento
051800980225     C                   MOVEL     *BLANKS       WVALOR                         non
051900980225     C                   MOVEL     DB9011        WCODE5            5            codifi.
052000980225     C                   MOVE      '-'           WCODE5            5            codifi.
052100980225     C                   MOVEL     WCODE5        WCODEV            9            codifi.
052200980225     C                   MOVE      DB9013        WCODEV            9            codifi.
052300980225     C                   MOVEL     WCODEV        WVALOR
052400980225     C                   EXSR      STPERR
052500980225     C                   MOVEL     'S'           §CECAR
052600980225     C                   END
052700980225      *
052800980318      *  Se EVENTO da non ignorare elaboro
052900980318     C     §CECAR        IFEQ      *BLANKS
053000980225      *
053100980225      *  Imposto evento consegna
053200961118     C     §CECEV        IFNE      'CON'
053300961121     C     WCAMG         ANDNE     CDEDAG
053400961121     C     §CECEV        ORNE      'AVV'
053500961121     C     §CECEV        ANDNE     'CON'
053600980318     C                   MOVE      §CECEV        CD§CMC
053700961024     C                   END
053800980225      *
053900980225      *  Se data corretta imposto CDE
054000960531     C     WERRO         IFNE      'S'
054100980312      *
054200980225      *  Spedizione consegnata totalmente
054300961125     C     §CETPE        IFEQ      'C'
054400961121     C     CDEDCM        IFLT      WCAMG
054500980318     C                   MOVE      WCAMG         CD§DCM
054600980318     C                   MOVE      WHHMM         CD§HMC
054700961121     C                   END
054800960531     C                   ELSE
054900980312      *  RISERVA
055000961125     C     §CETPE        IFEQ      'R'
055100980318     C                   MOVE      WCAMG         CD§DRC
055200980318     C                   MOVE      WHHMM         CD§HMC
055300961024     C                   ELSE
055400980312      *  Spedizione consegnata parzialmente
055500961125     C     §CETPE        IFEQ      'P'
055600980318     C                   MOVE      WCAMG         CD§DCM
055700980318     C                   MOVE      WHHMM         CD§HMC
055800960531     C                   ELSE
055900980312      *  Spedizione l.avviso
056000961125     C     §CETPE        IFEQ      'A'
056100980318     C                   MOVE      WHHMM         CD§HMC
056200980318     C                   MOVE      WCAMG         CD§DLA
056300960531     C                   ELSE
056400980312      *
056500980312      *  Spedizione giacente
056600980318     C                   MOVE      WHHMM         CD§HMC
056700980318     C                   MOVE      WCAMG         CD§DAG
056800960531     C                   END                                                    Eve.l.avv.
056900960531     C                   END                                                    Eve.parzi.
057000961024     C                   END                                                    RISERVA
057100961024     C                   END                                                    Eve.conse.
057200980225      *
057300980225      * Se data/ora consegna errata stp. errore
057400960531     C                   ELSE
057500960531     C                   MOVEL     ERR(1)        DESERR           78            Data/
057600960531     C                   MOVEL     *BLANKS       WVALOR           35            Ora
057700960531     C                   MOVE      DB2380        WVALOR                         evento
057800961022     C                   EXSR      STPERR
057900960531     C                   END                                                    errata
058000980225      *
058100980225      * Evento da ignorare
058200970221     C                   ELSE
058300980408      * Se previsto gestisco l'evento 'MIC'
058400980320     C     §CECAR        IFEQ      'E'
058500980320     C     *IN34         ANDEQ     '0'
058600980320     C                   EXSR      WRTEVE
058700980320     C                   END
058800000119     C                   CLEAR                   EDIDSCE
058900970221     C                   MOVEL     *BLANKS       WCAREV
059000970221     C                   SETON                                        34
059100970221     C                   Z-ADD     0             WCAMG
059200970221     C                   END                                                    ignora evento
059300980225      *
059400980225      * Evento non codificato
059500960531     C                   ELSE
059600961113     C     KNSP          IFNE      0
059700960531     C                   MOVEL     ERR(2)        DESERR                         Evento
059800960531     C                   MOVEL     *BLANKS       WVALOR                         non
059900961025     C                   MOVEL     DB9011        WCODE5            5            codifi.
060000961025     C                   MOVE      '-'           WCODE5            5            codifi.
060100961025     C                   MOVEL     WCODE5        WCODEV            9            codifi.
060200961025     C                   MOVE      DB9013        WCODEV            9            codifi.
060300961127     C                   MOVEL     WCODEV        WVALOR
060400961022     C                   EXSR      STPERR
060500961113     C                   END
060600960531     C                   END
060700980225      *
060800980225      *  Memorizzo su EDRDE le descrizioni aggiuntive
060900961118     C  N34
061000961118     CAN 32§CEDEC        IFNE      *BLANKS
061100961112     C                   MOVEL     'DB2380'      KNMC
061200961112     C                   Z-ADD     0             RDENRP
061300961112     C     KRDE          SETGT     EDRDE01L
061400961112     C     KRDE          READPE    EDRDE01L                               31
061500961119     C                   MOVEL     'N'           W2VOLT            1
061600961120     C  N31DB9011        IFEQ      '20 '
061700961119     C     DB9013        ANDEQ     '117'
061800961119     C     §CEDEC        ANDEQ     RDEVAL
061900961119     C                   MOVEL     'S'           W2VOLT
062000961119     C                   END
062100961119     C                   Z-ADD     WAAS          RDEAAS
062200961119     C                   Z-ADD     WLNP          RDELNP
062300961119     C                   Z-ADD     WNRS          RDENRS
062400961119     C                   Z-ADD     WNSP          RDENSP
062500961112     C                   MOVEL     'DB2380'      RDENMC
062600961112     C                   ADD       1             RDENRP
062700961118     C                   MOVEL     §CEDEC        RDEVAL
062800961112     C                   WRITE     EDRDE000
062900961112     C                   END
063000961112     C                   END
063100980407      *
063200980407      *  Stampo dati spedizione ed evento - solo se trovato FNARB
063300980407     C  N34              EXSR      PRTEVE
063400980407      *
063500980407     C                   ENDSR
063600980407      *-------------------------------------------------------------*
063700980407      *? PRTEVE - STAMPO EVENTO
063800980407      *----------------------------------------------------------------
063900980407     C     PRTEVE        BEGSR
064000980225      *
064100980225      *  Stampo dati spedizione
064200980407     C     DSSPE         IFNE      WSVSPE
064300040608     c                   If        In_Stampa ='S'
064400970328     C   OA              EXCEPT    TEST1
064500970328     C                   EXCEPT    DATSPE
064600040608     c                   end
064700961113     C                   MOVEL     DSSPE         WSVSPE           16
064800961113     C                   END
064900980225      *
065000980225      *  Stampo dati evento
065100980407     C     WCAREV        IFEQ      'S'
065200960701     C                   MOVE      WCAMG         WDTEVE
065300960701     C                   MOVE      WHHMM         WHHEVE
065400980225      *
065500980225      *  Se trovato stampo codice mancata consegna altrimenti stampo ***
065600961112     C     *IN32         IFEQ      '1'
065700980225     C                   MOVEL     §CEDEC        WDEC             35
065800961118     C                   MOVEL     §CECEV        WC2A              3
065900961112     C                   ELSE
066000961112     C                   MOVEL     '***'         WC2A              3
066100961112     C                   MOVEL     *BLANKS       WDEC             35
066200961112     C                   END
066300980225      *
066400980225      *  se codice evento ATR stampo in ogni caso '???'
066500980225     C     §CETPE        IFEQ      'V'
066600980225     C                   MOVEL     '???'         WC2A
066700980225     C                   END
066800980225      *
066900040608     c                   If        In_Stampa ='S'
067000960701     C   OA              EXCEPT    TEST1
067100960701     C                   EXCEPT    DATEVE
067200040608     c                   end
067300961112     C                   END
067400980225      *
067500960531     C                   ENDSR
067600980225      *-------------------------------------------------------------*
067700980225      *? AGGSPE - AGGANCIO LA SPEDIZIONE
067800980225      *----------------------------------------------------------------
067900980225     C     AGGSPE        BEGSR
068000980225      *
068100980318     C                   SETOFF                                       37
068200980410     C                   Z-ADD     *ZEROS        MICAAS
068300980410     C                   Z-ADD     *ZEROS        MICLNP
068400980410     C                   Z-ADD     *ZEROS        MICNRS
068500980410     C                   Z-ADD     *ZEROS        MICNSP
068600980318      *
068700980225      * Reperisco la chiave
068800980225     C                   EXSR      KEYSPE
068900980225      *
069000980410      * Memorizzo la Key sped. ritornata dal partner per creare il MIC
069100980410     C                   MOVEL     WAAS          MICAAS
069200980410     C                   MOVEL     WLNP          MICLNP
069300980410     C                   MOVEL     WNRS          MICNRS
069400980410     C                   MOVEL     WNSP          MICNSP
069500980410      *
069600980225      * Se la spedizione ha una figlia eseguo chain su FNCDE con quest'ultima
069700980225     C                   MOVEL     WAAS          KAAP
069800980225     C                   MOVEL     WLNP          KLPP
069900980225     C                   MOVEL     WNRS          KNRP
070000980225     C                   MOVEL     WNSP          KNSP1
070100980225     C     KLBL          SETGT     FNLBL02L
070200980225     C     KLBL          READPE    FNLBL02L                               33
070300980225      *
070400980225     C     *IN33         IFEQ      '0'
070500980225     C     LBLLAN        ANDEQ     LBLLAP
070600980225     C                   MOVEL     LBLAAN        KAAS
070700980225     C                   MOVEL     LBLLPN        KLNP
070800980225     C                   MOVEL     LBLNRN        KNRS
070900980225     C                   MOVEL     LBLNSN        KNSP
071000980225     C                   MOVEL     'S'           WMAMMA            1
071100980225     C                   ELSE
071200980225     C                   MOVEL     WAAS          KAAS
071300980225     C                   MOVEL     WLNP          KLNP
071400980225     C                   MOVEL     WNRS          KNRS
071500980225     C                   MOVEL     WNSP          KNSP
071600980225     C                   MOVEL     'N'           WMAMMA            1
071700980225     C                   END
071800980225      *
071900980225      * Controllo esistenza spedizione in FNCDE
072000980225     C     KCDE          CHAIN     FNCDE02L                           33
072100980318      *
072200980318      * Controllo esistenza spedizione in FNCDS
072300980318     C  N33KCDE          SETLL     FNCDS000                               37
072400980225      *
072500980225     C                   MOVEL     *BLANKS       WSTS
072600980225     C                   MOVEL     *BLANKS       WCODE
072700980225     C  N33              MOVEL     CDESTS        WSTS
072800980225     C  N33              MOVEL     CDECOD        WCODE
072900980225      *
073000980410      * Aggancio FNARB mamma se esiste
073100980410     C                   Z-ADD     *ZEROS        MICLNA
073200980410     C                   SETON                                        38
073300980410     C     KAAS          IFNE      MICAAS
073400980410     C     KLNP          ORNE      MICLNP
073500980410     C     KNRS          ORNE      MICNRS
073600980410     C     KNSP          ORNE      MICNSP
073700980410     C     KARBM         CHAIN     FNARB01L                           38
073800980410     C  N38              Z-ADD     ARBLNA        MICLNA
073900980410     C                   END
074000980410      *
074100980410      * Aggancio FNARB
074200980225     C                   Z-ADD     KAAS          CDEAAS
074300980225     C                   Z-ADD     KLNP          CDELNP
074400980225     C                   Z-ADD     KNRS          CDENRS
074500980225     C                   Z-ADD     KNSP          CDENSP
074600980225     C     KARB          CHAIN     FNARB01L                           34
074700980225      *
074800980225     C                   ENDSR
074900980225      *-------------------------------------------------------------*
075000980225      *? KEYSPE - RICAVO LA CHIAVE SPEDIZIONE
075100980225      *----------------------------------------------------------------
075200980225     C     KEYSPE        BEGSR
075300980225      *
075400980225     C     §PUSEC        IFEQ      'S'                                          * Secolo
075500980225     C                   MOVEL     DB1154        WCOM16           16
075600980225     C                   TESTN                   WCOM16               98
075700980225      *
075800980225     C     *IN98         IFEQ      '1'
075900980225     C                   MOVEL     DB1154        DSSPE
076000980225     C                   ELSE
076100980225     C                   MOVE      *ZEROS        DSSPE
076200980225     C                   MOVEL     ERR(4)        WCOM49           49
076300980225     C                   MOVEL     WCOM49        DSERR
076400980225     C                   MOVEL     *BLANKS       DESERR           78
076500980225     C                   MOVEL     DSERR         DESERR           78
076600980225     C                   MOVEL     *BLANKS       WVALOR           35
076700980225     C                   MOVE      DB1154        WVALOR
076800980225     C                   EXSR      STPERR
076900980225     C                   END
077000980225      *
077100980225     C                   ELSE
077200980225      *
077300980225     C                   MOVEL     DB1154        WCOM14           14
077400980225     C                   TESTN                   WCOM14               98
077500980225      *
077600980225     C     *IN98         IFEQ      '1'
077700980225     C                   MOVEL     DB1154        DSSPE2
077800980225     C                   MOVE      DSSPE2        DSSPE
077900980225      *
078000980225     C     WAAS2         IFLT      50
078100980225     C                   MOVEL     20            WAAS
078200980225     C                   ELSE
078300980225     C                   MOVEL     19            WAAS
078400980225     C                   END
078500980225      *
078600980225     C                   ELSE
078700980225     C                   MOVE      *ZEROS        DSSPE2
078800980225     C                   MOVE      *ZEROS        DSSPE
078900980225     C                   MOVEL     ERR(4)        WCOM49           49
079000980225     C                   MOVEL     WCOM49        DSERR
079100980225     C                   MOVEL     *BLANKS       DESERR           78
079200980225     C                   MOVEL     DSERR         DESERR           78
079300980225     C                   MOVEL     *BLANKS       WVALOR           35            Ora
079400980225     C                   MOVE      DB1154        WVALOR                         evento
079500980225     C                   EXSR      STPERR
079600980225     C                   END
079700980225      *
079800980225     C                   ENDIF
079900980225      *
080000980225     C                   ENDSR
080100980320      *-------------------------------------------------------------*
080200980320      *? WRTEVE - SCRIVO EVENTO DI MESSA IN CONSEGNA
080300980320      *----------------------------------------------------------------
080400980320     C     WRTEVE        BEGSR
080500980320      *
080600980408      *  Se la spedizione risulta consegnata stampo segnalazione d'errore
080700980408     C     ARBDCM        IFGT      *ZEROS
080800980408     C                   MOVEL     ERR(8)        DESERR
080900980408     C                   MOVEL     *BLANKS       WVALOR
081000980408     C                   EXSR      STPERR
081100980408     C                   ELSE
081200980408      *
081300990810     C                   Z-ADD     DATA8G        EVBDTV
081400980320     C                   Z-ADD     WORA          EVBORV
081500020906      *
081600020906     C*******            Z-ADD     WLNA          EVBFLE
081700020906      * P.O.Utente e Codice Utente e non più la LNA
081800021114     C                   Z-ADD     O36pou        EVBFLE
081900021114     C                   Z-ADD     O36cou        EVBCDU
082000020906      *
082100980410     C                   Z-ADD     ARBAAS        EVBAAS
082200980410     C                   Z-ADD     ARBLNP        EVBLNP
082300980410     C                   Z-ADD     ARBNRS        EVBNRS
082400980410     C                   Z-ADD     ARBNSP        EVBNSP
082500980410      *
082600980410      * Se esiste mamma con LNA uguale scrivo l'evento sulla mamma
082700980410     C     *IN38         IFEQ      '0'
082800980410     C     ARBLNA        ANDEQ     MICLNA
082900980410     C                   Z-ADD     MICAAS        EVBAAS
083000980410     C                   Z-ADD     MICLNP        EVBLNP
083100980410     C                   Z-ADD     MICNRS        EVBNRS
083200980410     C                   Z-ADD     MICNSP        EVBNSP
083300980410     C                   END
083400980410      *
083500980320     C                   MOVE      WCAMG         EVBDEV
083600980403     C     WHHMM         IFGT      *ZEROS
083700980320     C                   Z-ADD     WHHMM         EVBHEV
083800980403     C                   ELSE
083900980403     C                   Z-ADD     1200          EVBHEV
084000980403     C                   END
084100980320     C                   MOVEL     'MIC'         EVBCEV
084200980320      *
084300990810     C                   WRITE     FNEVB000
084400980320      *
084500980408     C                   EXSR      PRTEVE
084600980408     C                   END
084700980408      *
084800980320     C                   ENDSR
084900960528     C*-------------------------------------------------------------*
085000960531     C*? DRED10 - DECODIFICA TIPO RECORD SD10
085100960528     C*----------------------------------------------------------------
085200960531     C     DRED10        BEGSR
085300960528     C*
085400000119     C                   MOVEL     STADAT        EDSD10DS
085500960528     C*  Controllo se esiste firmatario
085600960531     C     DC3035        IFEQ      'AP'
085700980318     C                   MOVEL     DC3036        CD§NO2
085800960531     C                   END
085900960528     C*
086000960528     C                   ENDSR
086100980226      *-------------------------------------------------------------*
086200980226      *? DRED15 - DECODIFICA TIPO RECORD SD15
086300980226      *----------------------------------------------------------------
086400960531     C     DRED15        BEGSR
086500980226      *
086600000119     C                   MOVEL     STADAT        EDSD15DS
086700960531     C* Scrivo note
086800961120     C  N34              DO        5             X
086900040608     c                   If        In_Stampa ='S'
087000960701     C   OA              EXCEPT    TEST1
087100040608     c                   end
087200960531     C     D15(X)        IFNE      *BLANKS
087300961118     C                   MOVEL     'S'           WTESLB            1
087400980319     C                   MOVEL     D15(X)        CD§NO1
087500960701     C                   MOVEL     D15(X)        WNOTCO           70
087600040608     c                   If        In_Stampa ='S'
087700961118     C                   EXCEPT    NOTCON
087800040608     c                   end
087900961118     C* memorizzo nuovo FTX su FNCDE
088000961118     C                   MOVEL     'DD4440'      KNMC
088100961118     C     KRDE          SETGT     EDRDE01L
088200961118     C     KRDE          READPE    EDRDE01L                               31
088300961120     C                   MOVEL     D15(X)        WCOM35           35
088400961120     C     WCOM35        IFNE      *BLANKS
088500961120     C                   Z-ADD     WAAS          RDEAAS
088600961120     C                   Z-ADD     WLNP          RDELNP
088700961120     C                   Z-ADD     WNRS          RDENRS
088800961120     C                   Z-ADD     WNSP          RDENSP
088900961118     C                   MOVEL     'DD4440'      RDENMC
089000961118     C                   ADD       1             RDENRP
089100961118     C                   MOVEL     D15(X)        RDEVAL
089200961118     C                   WRITE     EDRDE000
089300961120     C                   END
089400961118     C* controllo se campo + lungo di 35 chr.
089500961118     C                   MOVE      D15(X)        WCOM35           35
089600961118     C     WCOM35        IFNE      *BLANKS
089700961120     C                   Z-ADD     WAAS          RDEAAS
089800961120     C                   Z-ADD     WLNP          RDELNP
089900961120     C                   Z-ADD     WNRS          RDENRS
090000961120     C                   Z-ADD     WNSP          RDENSP
090100961118     C                   MOVEL     'DD4440'      RDENMC
090200961118     C                   ADD       1             RDENRP
090300961118     C                   MOVE      D15(X)        RDEVAL
090400961118     C                   WRITE     EDRDE000
090500961118     C                   END
090600960531     C                   END
090700960531     C                   END
090800960528     C*
090900960528     C                   ENDSR
091000960528     C*-------------------------------------------------------------*
091100960531     C*? DRED20 - DECODIFICA TIPO RECORD SD20
091200960528     C*----------------------------------------------------------------
091300960531     C     DRED20        BEGSR
091400980312      *
091500000119     C                   MOVEL     STADAT        EDSD20DS
091600980319      *
091700980312      *  controllo se devo memorizzare tot.colli
091800961125     C     WMEMCO        IFEQ      'S'
091900961125     C                   MOVEL     'S'           WARR20            1
092000021204     c                   if        %subst(EDSD20DS:10:5) <> *blanks
092100961125     C                   Z-ADD     DE7224        WTOCOP            5 0
092200021204     c                   end
092300961125     C                   END
092400980319      *
092500960703     C                   Z-ADD     0             W                 3 0
092600980319      *
092700980319      *  Elaboro 20 alla volta
092800980319     C                   DO        20            X                              -- 01
092900980319     C     D20(X)        IFNE      *LOVAL                                       -- 02
093000980403     C     D20(X)        ANDNE     *BLANKS
093100980319      *
093200980319      *  Controllo validità segnacollo
093300980408     C                   MOVEL     '1'           TCTRSG            1
093400980408     C                   EXSR      CTRSGN
093500980319      *
093600980407      *  Se segnacollo non OK stampo, se OK elaboro
093700980407     C     WOKSGN        IFNE      'S'                                          -- 03
093800980407      *  Stampo ogni 3
093900980407     C                   ADD       1             W
094000980407     C                   MOVEL     D20(X)        SGT(W)
094100980407     C     W             IFEQ      3                                            -- 04
094200980407     C                   EXSR      PRTSGN
094300980407     C                   END                                                       04 --
094400980319      *
094500980407     C                   ELSE                                                   -- 03 --
094600980407      *
094700980319      *  Se previsto lo memorizzo
094800980319     C     WMEMCO        IFEQ      'S'                                          -- 04
094900980226     C                   ADD       1             YY                3 0
095000980226     C                   MOVE      D20(X)        SGN(YY)
095100980319     C                   END                                                       04 --
095200980319      *  Stampo ogni 3
095300960701     C                   ADD       1             W
095400960701     C                   MOVEL     D20(X)        SGT(W)
095500980319     C     W             IFEQ      3                                            -- 04
095600980319     C                   EXSR      PRTSGN
095700980319     C                   END                                                       04 --
095800980319     C                   END                                                       03 --
095900980319      *
096000980319     C                   END                                                        02 --
096100980319     C                   END                                                        01 --
096200980319      *
096300980312      *  Stampo resti
096400980407     C     W             IFGE      1
096500980319     C                   EXSR      PRTSGN
096600980407     C                   END
096700980312      *
096800960528     C                   ENDSR
096900980226      *----------------------------------------------------------------
097000980319      *? PRTSGN - STAMPA SEGNACOLLI
097100980226      *----------------------------------------------------------------
097200980319     C     PRTSGN        BEGSR
097300980319      *
097400980319     C     *INOA         IFEQ      '1'
097500040608     c                   If        In_Stampa ='S'
097600980319     C                   EXCEPT    TEST1
097700980319     C                   EXCEPT    DATSGN
097800040608     c                   end
097900980319     C                   SETOFF                                       OA
098000980319     C                   END
098100980319      *
098200040608     c                   If        In_Stampa ='S'
098300980319     C                   EXCEPT    DATSG1
098400040608     c                   end
098500980407      *
098600980407     C                   Z-ADD     0             W
098700980407     C                   MOVEL     *BLANKS       SGT
098800980319      *
098900980319     C                   ENDSR
099000980319      *----------------------------------------------------------------
099100980319      *? DREA05 - DECODIFICA TIPO RECORD SD05  - A.T.R. -
099200980319      *----------------------------------------------------------------
099300980319     C     DREA05        BEGSR
099400980319      *
099500980226     C                   SETON                                        34
099600000119     C                   MOVEL     STADAT        EDSD05DS
099700980226     C                   Z-ADD     0             WDTEVE            8 0
099800980226     C                   Z-ADD     0             WHHEVE            4 0
099900980226     C                   MOVEL     DB9011        W9011
100000980226     C                   MOVEL     DB9013        W9013
100100980226      *
100200980226      *  Converto data e ora evento
100300980226     C                   MOVEL     DB2380        WDATA            35
100400980226     C                   MOVEL     DB2379        WFORM             3
100500980226     C                   EXSR      CNVDAT
100600980226     C                   MOVEL     *BLANKS       WCAREV            1
100700980226      *
100800980226      * giro data evento
100900980226     C                   MOVE      WCAMG         G02INV
101000980226     C                   MOVEL     '3'           G02ERR
101100980226     C                   CALL      'XSRDA8'
101200980226     C                   PARM                    WLBDA8
101300980226     C                   Z-ADD     G02DAT        DATAEV            8 0
101400980226      *
101500980226      * Reperisco nr.spedizione da 'AGE'
101600980226     C     DB1153        IFEQ      'AGE'
101700980226      *
101800980226      * Determino se keysped = 0 o ' ' per segnalazione eccedenza
101900980226     C                   MOVEL     DB1154        WCOM14           14
102000980226     C     WCOM14        IFNE      *ZEROS
102100980226     C     WCOM14        ANDNE     *BLANKS
102200980227     C                   EXSR      KEYSPE
102300980226     C                   EXSR      ARRSPE
102400980226     C                   END
102500980226      *
102600980226     C                   ELSE
102700980226     C                   MOVE      *ZEROS        DSSPE2
102800980226     C                   MOVE      *ZEROS        DSSPE
102900980226     C                   MOVEL     ERR(4)        DESERR           78            Data/
103000980226     C                   MOVEL     *BLANKS       WVALOR           35            Ora
103100980226     C                   MOVE      DA1004        WVALOR                         evento
103200980226     C                   EXSR      STPERR
103300980226     C                   END
103400980226      *
103500980226     C                   Z-ADD     0             RDENRP
103600980227      * Imposto dati spedizione in stampa errori
103700980226     C                   Z-ADD     KAAS          WANNO             4 0
103800980226     C                   Z-ADD     KLNP          WLINPA            3 0
103900980226     C                   Z-ADD     KNRS          WSERIE            2 0
104000980226     C                   Z-ADD     KNSP          WSPEDI            7 0
104100980226      *
104200980227      * In base al codice dell'evento determino cosa eseguire
104300980226     C                   MOVEL     DB9011        WCEV
104400980226     C                   MOVE      DB9013        WCEV              6
104500980226     C                   MOVEL     'S'           WCAREV            1
104600980226     C                   Z-ADD     1             X
104700980226     C     WCEV          LOOKUP    CEV(X)                                 32
104800980226      *
104900980226     C     *IN32         IFEQ      '1'
105000000119     C                   MOVEL     DEV(X)        EDIDSCE
105100980422      *                    MOVEL§CECCA    CD$CCA
105200980407     C                   MOVEL     §CEERR        CD$ERR
105300980407     C                   MOVEL     §CEDEC        CD$MOT
105400980226     C                   MOVEL     §CEMEM        WMEMCO            1
105500980407     C                   MOVEL     DB9011        CD$STS
105600980407     C                   MOVEL     DB9013        CD$COD
105700980226      *
105800980227      * Se EVENTO non A.T.R. stampo errore e lo tratto come evento da ignorare
105900980226     C     §CETPE        IFNE      'V'
106000980226     C                   MOVEL     ERR(6)        DESERR                         Evento
106100980226     C                   MOVEL     *BLANKS       WVALOR                         non
106200980226     C                   MOVEL     DB9011        WCODE5            5            codifi.
106300980226     C                   MOVE      '-'           WCODE5            5            codifi.
106400980226     C                   MOVEL     WCODE5        WCODEV            9            codifi.
106500980226     C                   MOVE      DB9013        WCODEV            9            codifi.
106600980226     C                   MOVEL     WCODEV        WVALOR
106700980226     C                   EXSR      STPERR
106800980226     C                   MOVEL     'S'           §CECAR
106900980226     C                   END
107000980226      *
107100980407      * Se EVENTO da non ignorare elaboro
107200980428     C     §CECAR        IFNE      *BLANKS
107300980407      *
107400980407     C                   MOVE      §CECEV        CD$CMC
107500980226      *
107600980227      * Se data/ora corretti imposto FNCDA, se errati stampo errore
107700980226     C     WERRO         IFNE      'S'
107800980407     C                   MOVE      WCAMG         CD$DAM
107900980407     C                   MOVE      WHHMM         CD$HRA
108000980226     C                   ELSE
108100980226     C                   MOVEL     ERR(1)        DESERR           78
108200980226     C                   MOVEL     *BLANKS       WVALOR           35
108300980226     C                   MOVE      DB2380        WVALOR
108400980226     C                   EXSR      STPERR
108500980226     C                   END                                                    Data errata
108600980226      *
108700980226     C                   ELSE
108800000119     C                   CLEAR                   EDIDSCE
108900980226     C                   MOVEL     *BLANKS       WCAREV
109000980226     C                   SETON                                        34
109100980226     C                   Z-ADD     0             WCAMG
109200980226     C                   END                                                    Ignora evento
109300980226      *
109400980226      * Evento non codificato
109500980226     C                   ELSE
109600980226     C     KNSP          IFNE      0
109700980226     C                   MOVEL     ERR(2)        DESERR                         Evento
109800980226     C                   MOVEL     *BLANKS       WVALOR                         non
109900980226     C                   MOVEL     DB9011        WCODE5            5            codifi.
110000980226     C                   MOVE      '-'           WCODE5            5            codifi.
110100980226     C                   MOVEL     WCODE5        WCODEV            9            codifi.
110200980226     C                   MOVE      DB9013        WCODEV            9            codifi.
110300980226     C                   MOVEL     WCODEV        WVALOR
110400980226     C                   EXSR      STPERR
110500980226     C                   END
110600980226     C                   END                                                    Evento non codificat
110700980407      *
110800980407     C                   MOVEL     'N'           W2VOLT            1
110900980407      *
111000980407      *  Memorizzo su EDRDA le descrizioni aggiuntive
111100980407     C  N34
111200980407     CAN 32§CEDEC        IFNE      *BLANKS
111300980407     C                   MOVEL     'DB2380'      KNMC
111400071220     C****               Z-ADD     0             RDENRP
111500071220     C                   Z-ADD     0             RDANRP
111600980526     C     KRDA          SETGT     EDRDA01L
111700980526     C     KRDA          READPE    EDRDA01L                               31
111800980408     C  N31DB9011        IFEQ      '3  '
111900980407     C     DB9013        ANDEQ     '117'
112000980407     C     §CEDEC        ANDEQ     RDAVAL
112100980407     C                   MOVEL     'S'           W2VOLT
112200980407     C                   END
112300980407     C                   Z-ADD     WAAS          RDAAAS
112400980407     C                   Z-ADD     WLNP          RDALNP
112500980407     C                   Z-ADD     WNRS          RDANRS
112600980407     C                   Z-ADD     WNSP          RDANSP
112700980407     C                   MOVEL     'DB2380'      RDANMC
112800980407     C                   ADD       1             RDANRP
112900980407     C                   MOVEL     §CEDEC        RDAVAL
113000980416     C                   MOVEL     CD$STS        RDASTS
113100980416     C                   MOVEL     CD$COD        RDACOD
113200980416     C                   Z-ADD     CD$DAM        RDADAM
113300980416     C                   Z-ADD     CD$HRA        RDAHRA
113400010802     C     §PUATR        IFEQ      'S'
113500071221     c     rda1          tag
113600071221     C                   WRITE     EDRDA000                             99
113700071221     c                   if        *in99
113800071221     C                   ADD       1             RDANRP
113900071221     c                   goto      rda1
114000071221     c                   end
114100010802     C                   END
114200980407     C                   END
114300980226      *
114400980226      *  Stampo dati spedizione
114500980226     C  N34DSSPE         IFNE      WSVSPE
114600040608     c                   If        In_Stampa ='S'
114700980226     C   OA              EXCEPT    TEST1
114800980226     C                   EXCEPT    DATSPE
114900040608     c                   end
115000980226     C                   MOVEL     DSSPE         WSVSPE           16
115100980226     C                   END
115200980226      *
115300980226      *  Stampo dati evento
115400980226     C  N34WCAREV        IFEQ      'S'
115500980226     C                   MOVE      WCAMG         WDTEVE
115600980226     C                   MOVE      WHHMM         WHHEVE
115700980226      *
115800980226      *  Se trovato stampo codice mancata consegna altrimenti stampo ***
115900980226     C     *IN32         IFEQ      '1'
116000980226     C                   MOVEL     §CEDEC        WDEC             35
116100980226     C                   MOVEL     §CECEV        WC2A              3
116200980226     C                   ELSE
116300980226     C                   MOVEL     '***'         WC2A              3
116400980226     C                   MOVEL     *BLANKS       WDEC             35
116500980226     C                   END
116600980226      *
116700980226      *  se codice evento non ATR stampo in ogni caso '???'
116800980226     C     §CETPE        IFNE      'V'
116900980226     C                   MOVEL     '???'         WC2A
117000980226     C                   END
117100980226      *
117200040608     c                   If        In_Stampa ='S'
117300980226     C   OA              EXCEPT    TEST1
117400980226     C                   EXCEPT    DATEVE
117500040608     c                   end
117600980226     C                   END
117700980226      *
117800980226     C                   ENDSR
117900980226      *-------------------------------------------------------------*
118000980226      *? ARRSPE - AGGANCIO LA SPEDIZIONE - A.T.R. -
118100980226      *----------------------------------------------------------------
118200980226     C     ARRSPE        BEGSR
118300980226      *
118400980226     C                   MOVEL     WAAS          KAAS
118500980226     C                   MOVEL     WLNP          KLNP
118600980226     C                   MOVEL     WNRS          KNRS
118700980226     C                   MOVEL     WNSP          KNSP
118800980226     C                   MOVEL     'N'           WMAMMA            1
118900980415      *
119000980415     C                   Z-ADD     KAAS          CDAAAS
119100980415     C                   Z-ADD     KLNP          CDALNP
119200980415     C                   Z-ADD     KNRS          CDANRS
119300980415     C                   Z-ADD     KNSP          CDANSP
119400980226      *
119500980226      * Campi per stampa
119600980226     C                   Z-ADD     KAAS          CDEAAS
119700980226     C                   Z-ADD     KLNP          CDELNP
119800980226     C                   Z-ADD     KNRS          CDENRS
119900980226     C                   Z-ADD     KNSP          CDENSP
120000980226      *
120100980226      * Aggancio FNARB
120200980226     C     KARB          CHAIN     FNARB01L                           34
120300980226      *
120400980226     C                   ENDSR
120500980226      *-------------------------------------------------------------*
120600980226      *? DRAD10 - DECODIFICA TIPO RECORD SD10  - A.T.R. -
120700980226      *----------------------------------------------------------------
120800980226     C     DREA10        BEGSR
120900980226      *
121000000119     C                   MOVEL     STADAT        EDSD10DS
121100980226      *
121200980226     C                   ENDSR
121300980226      *-------------------------------------------------------------*
121400980226      *? DREA15 - DECODIFICA TIPO RECORD SD15  - A.T.R. -
121500980226      *----------------------------------------------------------------
121600980226     C     DREA15        BEGSR
121700980226      *
121800000119     C                   MOVEL     STADAT        EDSD15DS
121900980226      *
122000980226      * Scrivo note
122100980226     C  N34              DO        5             X
122200040608     c                   If        In_Stampa ='S'
122300980226     C   OA              EXCEPT    TEST1
122400040608     c                   end
122500980226      *
122600980226     C     D15(X)        IFNE      *BLANKS
122700980226     C                   MOVEL     'S'           WTESLB            1
122800980407     C                   MOVEL     D15(X)        CD$NO1
122900980226     C                   MOVEL     D15(X)        WNOTCO           70
123000040608     c                   If        In_Stampa ='S'
123100980226     C                   EXCEPT    NOTCON
123200040608     c                   end
123300980416      * memorizzo nuovo FTX su FNCDA
123400980407     C                   MOVEL     'DD4440'      KNMC
123500980526     C     KRDA          SETGT     EDRDA01L
123600980526     C     KRDA          READPE    EDRDA01L                               31
123700980407     C                   MOVEL     D15(X)        WCOM35           35
123800980407     C     WCOM35        IFNE      *BLANKS
123900980407     C                   Z-ADD     WAAS          RDAAAS
124000980407     C                   Z-ADD     WLNP          RDALNP
124100980407     C                   Z-ADD     WNRS          RDANRS
124200980407     C                   Z-ADD     WNSP          RDANSP
124300980407     C                   MOVEL     'DD4440'      RDANMC
124400980407     C                   ADD       1             RDANRP
124500980407     C                   MOVEL     D15(X)        RDAVAL
124600980416     C                   MOVEL     CD$STS        RDASTS
124700980416     C                   MOVEL     CD$COD        RDACOD
124800980416     C                   Z-ADD     CD$DAM        RDADAM
124900980416     C                   Z-ADD     CD$HRA        RDAHRA
125000010802     C     §PUATR        IFEQ      'S'
125100071221     c     rda2          tag
125200980407     C                   WRITE     EDRDA000
125300071221     c                   if        *in99
125400071221     C                   ADD       1             RDANRP
125500071221     c                   goto      rda2
125600071221     c                   end
125700010802     C                   END
125800980407     C                   END
125900980407     C* controllo se campo + lungo di 35 chr.
126000980407     C                   MOVE      D15(X)        WCOM35           35
126100980407     C     WCOM35        IFNE      *BLANKS
126200980407     C                   Z-ADD     WAAS          RDAAAS
126300980407     C                   Z-ADD     WLNP          RDALNP
126400980407     C                   Z-ADD     WNRS          RDANRS
126500980407     C                   Z-ADD     WNSP          RDANSP
126600980407     C                   MOVEL     'DD4440'      RDANMC
126700980407     C                   ADD       1             RDANRP
126800980407     C                   MOVE      D15(X)        RDAVAL
126900980416     C                   MOVEL     CD$STS        RDASTS
127000980416     C                   MOVEL     CD$COD        RDACOD
127100980416     C                   Z-ADD     CD$DAM        RDADAM
127200980416     C                   Z-ADD     CD$HRA        RDAHRA
127300010802     C     §PUATR        IFEQ      'S'
127400071221     c     rda3          tag
127500980407     C                   WRITE     EDRDA000
127600071221     c                   if        *in99
127700071221     C                   ADD       1             RDANRP
127800071221     c                   goto      rda3
127900071221     c                   end
128000010802     C                   END
128100980407     C                   END
128200980226     C                   END
128300980226     C                   END
128400980226      *
128500980226     C                   ENDSR
128600980226      *-------------------------------------------------------------*
128700980226      *? DREA20 - DECODIFICA TIPO RECORD SD20  - A.T.R. -
128800980226      *----------------------------------------------------------------
128900980226     C     DREA20        BEGSR
129000980226      *
129100000119     C                   MOVEL     STADAT        EDSD20DS
129200980226      *
129300980226      *  memorizzo colli evento
129400021204     c                   if        %subst(EDSD20DS:10:5) <> *blanks
129500980407     C                   Z-ADD     DE7224        CD$NCL
129600021204     c                   end
129700980226      *
129800980407      *  Elaboro 20 per volta
129900980226     C                   Z-ADD     0             W                 3 0
130000980407      *
130100980226     C                   DO        20            X
130200980226     C     D20(X)        IFNE      *LOVAL
130300980403     C     D20(X)        ANDNE     *BLANKS
130400980408      *
130500980408      *  Se eccedenza memorizzo segnacollo senza controlli
130600980408     C     CD$STS        IFEQ      '3  '
130700980408     C     CD$COD        ANDEQ     '288'
130800980408     C                   ADD       1             YY                3 0
130900980408     C                   MOVE      D20(X)        SGN(YY)
131000980408     C                   ELSE
131100980312      *
131200980407      *  Controllo validità segnacollo
131300980408     C                   MOVEL     '2'           TCTRSG
131400980312     C                   EXSR      CTRSGN
131500980407      *
131600980407      *  Se segnacollo non OK stampo, se OK elaboro
131700980407     C     WOKSGN        IFNE      'S'
131800980407      *  Stampo ogni 3
131900980407     C                   ADD       1             W
132000980407     C                   MOVEL     D20(X)        SGT(W)
132100980407     C     W             IFEQ      3
132200980407     C                   EXSR      PRTSGN
132300980407     C                   END
132400980407      *
132500980407     C                   ELSE
132600980407      *
132700980407      *  Se previsto lo memorizzo
132800980407     C     WMEMCO        IFEQ      'S'
132900980226     C                   ADD       1             YY                3 0
133000980226     C                   MOVE      D20(X)        SGN(YY)
133100980407     C                   END
133200980407      *  Stampo ogni 3
133300980226     C                   ADD       1             W
133400980226     C                   MOVEL     D20(X)        SGT(W)
133500980226     C     W             IFEQ      3
133600980407     C                   EXSR      PRTSGN
133700980226     C                   END
133800980312     C                   END
133900980226      *
134000980407     C                   END
134100980408     C                   END
134200980407     C                   END
134300980407      *
134400980312      *  Stampo resti
134500980312     C     W             IFGE      1
134600040608     c                   If        In_Stampa ='S'
134700980312     C                   EXCEPT    DATSG1
134800040608     c                   end
134900980312     C                   END
135000980226      *
135100980226     C                   ENDSR
135200960531     C*----------------------------------------------------------------
135300960531     C*? CNVDAT - DECODIFICA LA DATA
135400960531     C* INPUT:  - WFORM  (FORMATO DATA : 102 --> CCYYMMGG)
135500960531     C*         - WFORM  (FORMATO DATA : 203 --> CCYYMMGGHHMM)
135600960531     C*         - WDATA  (DATA ALFANUMERICA)
135700960531     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
135800960531     C*         - WHMS   (ORA NUMERICA)
135900960531     C*----------------------------------------------------------------
136000960531     C     CNVDAT        BEGSR
136100960531     C*
136200960531     C                   MOVEL     ' '           WERRO             1
136300960531     C                   Z-ADD     *ZEROS        WCAMG             8 0
136400960531     C                   Z-ADD     *ZEROS        WHMS              6 0
136500960531     C*
136600960531     C     WFORM         IFEQ      '102'                                        CCYMMDD
136700971031     C* CONTROLLO SE LA DATA E' DI SEI CIFRE
136800971031     C                   MOVEL     WDATA         WCOMO8
136900971031     C                   MOVE      WCOMO8        WCOMO2            2
137000971031     C     WCOMO2        IFEQ      *BLANKS
137100971031     C                   MOVEL     WDATA         WCOMO6            6
137200971031     C                   MOVEL     WDATA         WCOMO2
137300971031     C     WCOMO2        IFGT      '40'
137400971031     C                   MOVEL     '19'          WCOMO8
137500971031     C                   ELSE
137600971031     C                   MOVEL     '20'          WCOMO8
137700971031     C                   END
137800971031     C                   MOVE      WCOMO6        WCOMO8            8
137900971031     C                   ELSE
138000971031     C                   MOVEL     WDATA         WCOMO8            8
138100971031     C                   END
138200971031     C*
138300960531     C                   TESTN                   WCOMO8               99
138400960531     C   99              MOVE      WCOMO8        WCAMG                          *DATA
138500960531     C  N99              MOVEL     'S'           WERRO             1
138600960531     C                   END
138700971031     C*
138800960531     C     WFORM         IFEQ      '203'                                        CCYMMDDHHMM
138900960531     C                   MOVEL     WDATA         WCOM12           12
139000960531     C                   TESTN                   WCOM12               99
139100960531     C   99              MOVEL     WCOM12        WCAMG                          *DATA
139200960531     C   99              MOVE      WCOM12        WHHMM             4 0          *ORA
139300960531     C  N99              MOVEL     'S'           WERRO             1
139400960531     C                   END
139500980226      *
139600960531     C                   ENDSR
139700980226      *----------------------------------------------------------------
139800980319      *? CTRSGN - Controllo se corrisponde con bolla
139900980226      *----------------------------------------------------------------
140000960531     C     CTRSGN        BEGSR
140100980226      *
140200980408      * TCTRSG determina il tipo di controllo da effettuare
140300980408      *  "1"  = Controllo dati consegna
140400980408      *  "2"  = Controllo dati A.T.R.
140500980408      *
140600980408     C     TCTRSG        IFEQ      '1'
140700980408     C                   Z-ADD     CDENRS        WNRS
140800980408     C                   ELSE
140900980408     C                   Z-ADD     CDANRS        WNRS
141000980408     C                   END
141100980408      *
141200960531     C                   MOVEL     'N'           WOKSGN            1
141300050503      *
141400051017      *  Ricavo il numero spedizione agganciando FIARS e FNART
141500050503     C                   MOVEL     D20(X)        KNOT
141600050503      *
141700050503      *  Ora occorre controllare il TRC poichè la vista logica seleziona
141800050503      *  oltre al tipo "E"= Euroexpress and "F" segnacollo normale
141900050503      *   ovviamente qui interessa solo il tipo Euroexpress
142000051017     C     KARS          setll     FIARS000
142100051017     C     KARS          reade     FIARS000                               98
142200050503     C     *IN98         doweq     *Off
142300050503     c                   if        ArsTrc ='E'
142400050503     c                   leave
142500050503     c                   end
142600051017     C     KARS          reade     FiARS000                               98
142700050503     c                   enddo
142800050503      *
142900050503     C     *IN98         IFEQ      *Off
143000050503     C                   Z-ADD     ARSFLS        KFLS
143100050503     C                   Z-ADD     ARSLNA        KLNA
143200050503     C                   Z-ADD     ARSNRS        KNRS
143300050503     C                   Z-ADD     ARSNSC        KNSC
143400050503     C     KART1         CHAIN     FNART027                           98
143500050503     C  N98              MOVEL     'S'           WOKSGN
143600050503     c                   end
143700980312      *
143800050504     /* ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
143900050504      * SE ANCORA NON é riuscito l'aggancio, prova al vecchio modo:
144000050504     /* ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
144100980408      * Se i caratteri dal 6 al 17 son numerici eseguo il controllo
144200050504     C                   if           WOKSGN = 'N'
144300050504     C                   MOVEL     D20(X)        WCOM17           17
144400050504     C                   MOVE      WCOM17        WCOM12           12
144500050504     C                   TESTN                   WCOM12               98
144600050504      **
144700050504     C     *IN98         IFEQ      '1'
144800050504     C                   MOVEL     WCOM12        DSSGNE
144900050504      **
145000050504     C     SGFLS         IFEQ      ARBFLS
145100050504     C     SGNRS         ANDEQ     WNRS
145200050504     C                   Z-ADD     SGNSC         KNSC
145300050504     C     KART2         CHAIN     FNART000                           98
145400050504     C  N98              MOVEL     'S'           WOKSGN
145500050504     C                   END
145600050504     C                   END
145700050504     C                   ENDif
145800980226      *
145900960531     C                   ENDSR
146000961125     C*----------------------------------------------------------------
146100961125     C*? TOTCOL - Calcolo totale da consegnare
146200961125     C*----------------------------------------------------------------
146300961125     C     TOTCOL        BEGSR
146400961125     C*
146500961125     C                   Z-ADD     0             WTOCOC            5 0
146600961125     C     KART          CHAIN     FNART01L                           36
146700961125     C     *IN36         DOWEQ     '0'
146800961125     C     ARTDCM        IFEQ      0
146900961125     C                   ADD       1             WTOCOC            5 0
147000961125     C                   END
147100961125     C     KART          READE     FNART01L                               36
147200961125     C                   END
147300961125     C*
147400961125     C                   ENDSR
147500980319      *----------------------------------------------------------------
147600980320      *? SUBWRT - Determino tipo di scrittura di FNCDE e/o FNCDS
147700980319      *----------------------------------------------------------------
147800980320     C     WRTCDE        BEGSR
147900980312      *
148000980320      *-----------------------------------------------------
148100980320      *    *IN34 = '0' => esiste FNARB e viceversa
148200980320      *    *IN33 = '1' => non esiste FNCDE e viceversa
148300980319      *    *IN37 = '1' => esiste FNCDS e viceversa
148400980319      *     YY = 0 evento totale in elaborazione
148500980319      *     YY > 0 evento parziale in elaborazione
148600980320      *-----------------------------------------------------
148700980319      *
148800980320     C     *IN34         IFEQ      '0'                                          -- 1
148900980320      *
149000980320      * Verifico se segnalare se non ho consegnato tutti i colli
149100980320     C                   EXSR      MEMCOL
149200980320      *
149300980320     C     *IN33         IFEQ      '1'                                          -- 2
149400980319      *
149500980320     C     YY            IFEQ      *ZEROS                                       -- 3
149600980320     C                   EXSR      WRTOLD                                       come prima
149700980320     C                   ELSE                                                   -- 3 --
149800980320     C                   EXSR      WRCDE0                                       scrivo cde a 0
149900980320     C                   EXSR      WRTCDS                                       scrivo cds
150000980320     C                   END                                                       3 --
150100980319      *
150200980320     C                   ELSE                                                   -- 2 --
150300980319      *
150400980320     C     *IN37         IFEQ      '0'                                          -- 3
150500980319      *
150600980320     C     YY            IFEQ      *ZEROS                                       -- 4
150700980320     C                   EXSR      WRTOLD                                       come prima
150800980320     C                   ELSE                                                   -- 4 --
150900980320     C                   EXSR      UPCDE0                                       upd cde 0 + wrt cds
151000980320     C                   EXSR      WRTCDS                                       scrivo cds
151100980320     C                   END                                                       4 --
151200980319      *
151300980320     C                   ELSE                                                   -- 3 --
151400980408     C                   EXSR      MAN2VO                                       Doppio 20-117
151500980406     C                   EXSR      WRTCDS                                       wrt cds 0 o > 0
151600980320     C                   END                                                       3 --
151700980319      *
151800980320     C                   END                                                       2 --
151900980320     C                   END                                                       1 --
152000980319      *
152100980320      *  Azzero variabili
152200980320     C                   Z-ADD     0             YY
152300980320     C                   MOVEL     *BLANKS       SGN
152400980320     C                   MOVEL     *BLANKS       SGT
152500980320      *
152600980319     C                   ENDSR
152700980320      *----------------------------------------------------------------
152800980320      *? MEMCOL - Segnalazione di consegna parziale
152900980320      *----------------------------------------------------------------
153000980320     C     MEMCOL        BEGSR
153100980320      *
153200980320     C     WMEMCO        IFEQ      'S'
153300980320     C     WARR20        ANDEQ     'S'
153400980320      *
153500980320      * Loop su FNARB per verificare quanti colli sono da consegnare
153600980320     C                   EXSR      TOTCOL
153700980320      *
153800980320      * Scrivo NOTE
153900980320     C                   MOVEL     ERR(5)        ERRCOL
154000980320     C                   Z-ADD     WTOCOC        COLSPE
154100980320     C                   Z-ADD     WTOCOP        COLCON
154200980320     C                   MOVEL     ERRCOL        CD§NO1
154300980320     C                   END
154400980320      *
154500980320     C                   ENDSR
154600980406      *----------------------------------------------------------------
154700980406      *? MAN2VO - Doppia segnalazione di mancanza
154800980406      *----------------------------------------------------------------
154900980406     C     MAN2VO        BEGSR
155000980406      *
155100980406      * Se è la 2a volta che trovo l'evento 20 - 117 imposto CD§FL1 = 'S'
155200980406      *
155300980406     C                   MOVEL     W2VOLT        CD§FL1
155400980406     C     CD§FL1        IFEQ      'S'
155500980406     C     WSTS          ANDEQ     '20 '
155600980406     C     WCODE         ANDEQ     '117'
155700980406     C                   MOVEL     'S'           CD§FL2
155800980406     C                   END
155900980406      *
156000980406     C                   ENDSR
156100980319      *----------------------------------------------------------------
156200980320      *? WRTOLD - Scrittura dati FNCDE metodo standard
156300980319      *----------------------------------------------------------------
156400980320     C     WRTOLD        BEGSR
156500980319      *
156600980320      * Imposto date per confronto
156700980320     C     CD§DCM        IFEQ      *ZEROS                                       consegna
156800980320     C                   Z-ADD     CDEDCM        CD§DCM
156900980320     C                   END
157000980320     C     CD§DLA        IFEQ      *ZEROS                                       lasciato avviso
157100980320     C                   Z-ADD     CDEDLA        CD§DLA
157200980320     C                   END
157300980320     C     CD§DAG        IFEQ      *ZEROS                                       giacenza
157400980320     C                   Z-ADD     CDEDAG        CD§DAG
157500980320     C                   END
157600980320     C     CD§DRC        IFEQ      *ZEROS                                       riserva
157700980320     C                   Z-ADD     CDEDRC        CD§DRC
157800980320     C                   END
157900980320      *
158000980312      * Se ho impostato data consegna + altra data abblenco l'altra
158100980320     C     CD§DCM        IFGT      CD§DAG
158200980320     C                   Z-ADD     0             CD§DAG
158300980320     C     CD§CMC        IFNE      'MAN'
158400980320     C     CD§CMC        ANDNE     'P  '
158500980320     C     CD§CMC        ANDNE     'RES'
158600980320     C     CD§CMC        ANDNE     'IDD'
158700980320     C     CD§CMC        ANDNE     'RIT'
158800980320     C                   MOVEL     *BLANKS       CD§CMC
158900961125     C                   END
159000961125     C                   END
159100980320      *
159200980320     C     CD§DCM        IFGT      CD§DLA
159300980320     C                   Z-ADD     0             CD§DLA
159400980320     C     CD§CMC        IFEQ      'AVV'
159500980320     C     CD§CMC        OREQ      'AV7'
159600980320     C                   MOVEL     *BLANKS       CD§CMC
159700961024     C                   END
159800961125     C                   END
159900980320      *
160000980320      * Se è la seconda volta che trovo l'evento 20 - 117 imposto
160100980320      * 'S' in CD§FL1
160200980320     C                   MOVEL     W2VOLT        CD§FL1
160300980320     C     CD§FL1        IFEQ      'S'
160400961127     C     WSTS          IFEQ      '20 '
160500961127     C     WCODE         ANDEQ     '117'
160600980320     C                   MOVEL     'S'           CD§FL2
160700961127     C                   ELSE
160800980320     C                   MOVEL     *BLANKS       CD§ERR
160900961127     C                   END
161000961127     C                   END
161100980311      *
161200980311      * Aggiorno FNCDE
161300980320     C                   MOVEL     WTESLB        CDEFTX
161400980320     C                   EXSR      MOVCD§
161500961118     C     *IN33         IFEQ      '1'
161600961118     C                   MOVEL     ARBRMN        CDERMN
161700961120     C                   Z-ADD     ARBLNA        CDELNA
161800961118     C                   WRITE     FNCDE000
161900961118     C                   ELSE
162000961118     C                   UPDATE    FNCDE000
162100961118     C                   END
162200980320      *
162300960531     C                   ENDSR
162400980320      *----------------------------------------------------------------
162500980320      *? MOVCD§ - Aggiorno campi di FNCDE per aggiornamento/scrittura
162600980320      *----------------------------------------------------------------
162700980320     C     MOVCD§        BEGSR
162800980320      *
162900980320     C                   Z-ADD     CD§DCM        CDEDCM
163000980320     C                   Z-ADD     CD§HMC        CDEHMC
163100980320     C                   Z-ADD     CD§DLA        CDEDLA
163200980320     C                   Z-ADD     CD§DAG        CDEDAG
163300980320     C                   Z-ADD     CD§DRC        CDEDRC
163400980320     C                   MOVEL     CD§NO1        CDENO1
163500980320     C                   MOVEL     CD§NO2        CDENO2
163600980320     C                   MOVEL     CD§CMC        CDECMC
163700980320     C                   MOVEL     CD§CCA        CDECCA
163800980320     C                   MOVEL     CD§ERR        CDEERR
163900980320     C                   MOVEL     CD§STS        CDESTS
164000980320     C                   MOVEL     CD§COD        CDECOD
164100980320     C                   MOVEL     CD§MOT        CDEMOT
164200980320     C                   MOVEL     CD§FL1        CDEFL1
164300980320     C                   MOVEL     CD§FL2        CDEFL2
164400980320      *
164500980320     C                   ENDSR
164600980320      *----------------------------------------------------------------
164700980320      *? WRCDE0 - Scrittura dati FNCDE, solo dati base
164800980320      *----------------------------------------------------------------
164900980320     C     WRCDE0        BEGSR
165000980406      *
165100980406     C                   EXSR      MAN2VO
165200980406      *
165300980406     C                   MOVEL     CD§FL1        CDEFL1
165400980406     C                   MOVEL     CD§FL2        CDEFL2
165500980320     C                   Z-ADD     ARBLNA        CDELNA
165600980320     C                   MOVEL     ARBRMN        CDERMN
165700980320     C                   MOVEL     WTESLB        CDEFTX
165800980320     C                   WRITE     FNCDE000
165900980320      *
166000980320     C                   ENDSR
166100980320      *----------------------------------------------------------------
166200980320      *? WRTCDS - Scrivo dati FNCDS
166300980320      *----------------------------------------------------------------
166400980320     C     WRTCDS        BEGSR
166500980320      *
166600980320     C                   Z-ADD     CDEAAS        CDSAAS
166700980320     C                   Z-ADD     CDELNP        CDSLNP
166800980320     C                   Z-ADD     CDENRS        CDSNRS
166900980320     C                   Z-ADD     CDENSP        CDSNSP
167000980320     C                   Z-ADD     CDELNA        CDSLNA
167100980320     C                   Z-ADD     CDERMN        CDSRMN
167200980320     C                   Z-ADD     CD§DCM        CDSDCM
167300980320     C                   Z-ADD     CD§HMC        CDSHMC
167400980320     C                   Z-ADD     CD§DLA        CDSDLA
167500980320     C                   Z-ADD     CD§DAG        CDSDAG
167600980320     C                   Z-ADD     CD§DRC        CDSDRC
167700980320     C                   MOVEL     CD§NO1        CDSNO1
167800980320     C                   MOVEL     CD§NO2        CDSNO2
167900980320     C                   MOVEL     CD§CMC        CDSCMC
168000980320     C                   MOVEL     CD§CCA        CDSCCA
168100980320     C                   MOVEL     CD§ERR        CDSERR
168200980320     C                   MOVEL     CD§STS        CDSSTS
168300980320     C                   MOVEL     CD§COD        CDSCOD
168400980320     C                   MOVEL     CD§MOT        CDSMOT
168500980320     C                   MOVEL     CD§FL1        CDSFL1
168600980320     C                   MOVEL     CD§FL2        CDSFL2
168700980320      *
168800980320      * Se manca dettaglio PCI scrivo 1 rcd con segnacollo = blanks
168900980320     C     YY            IFEQ      0
169000980320     C                   MOVEL     *BLANKS       CDSSEE
169100980320     C                   WRITE     FNCDS000
169200980320     C                   ELSE
169300980320      *
169400980403     C                   Z-ADD     1             X
169500980403     C     X             DOWLE     YY
169600980320     C                   MOVEL     SGN(X)        CDSSEE
169700980320     C                   ADD       1             X
169800980320     C                   WRITE     FNCDS000
169900980320     C                   END
170000980320      *
170100980320     C                   END
170200980320      *
170300980320     C                   ENDSR
170400980320      *----------------------------------------------------------------
170500980320      *? UPCDE0 - Trasformo CDE esistente in CDS e azzero CDE
170600980320      *----------------------------------------------------------------
170700980320     C     UPCDE0        BEGSR
170800980320      *
170900980320     C                   Z-ADD     CDEAAS        CDSAAS
171000980320     C                   Z-ADD     CDELNP        CDSLNP
171100980320     C                   Z-ADD     CDENRS        CDSNRS
171200980320     C                   Z-ADD     CDENSP        CDSNSP
171300980320     C                   Z-ADD     CDELNA        CDSLNA
171400980320     C                   Z-ADD     CDERMN        CDSRMN
171500980320     C                   Z-ADD     CDEDCM        CDSDCM
171600980320     C                   Z-ADD     CDEHMC        CDSHMC
171700980320     C                   Z-ADD     CDEDLA        CDSDLA
171800980320     C                   Z-ADD     CDEDAG        CDSDAG
171900980320     C                   Z-ADD     CDEDRC        CDSDRC
172000980320     C                   MOVEL     CDENO1        CDSNO1
172100980320     C                   MOVEL     CDENO2        CDSNO2
172200980320     C                   MOVEL     CDECMC        CDSCMC
172300980320     C                   MOVEL     CDECCA        CDSCCA
172400980320     C                   MOVEL     CDEERR        CDSERR
172500980320     C                   MOVEL     CDESTS        CDSSTS
172600980320     C                   MOVEL     CDECOD        CDSCOD
172700980320     C                   MOVEL     CDEMOT        CDSMOT
172800980320     C                   MOVEL     CDEFL1        CDSFL1
172900980320     C                   MOVEL     CDEFL2        CDSFL2
173000980320     C                   MOVEL     *BLANKS       CDSSEE
173100980320      *
173200980320     C                   WRITE     FNCDS000
173300980320      *
173400980401     C                   Z-ADD     *ZEROS        CDEDCM
173500980401     C                   Z-ADD     *ZEROS        CDEHMC
173600980401     C                   Z-ADD     *ZEROS        CDEDLA
173700980401     C                   Z-ADD     *ZEROS        CDEDAG
173800980401     C                   Z-ADD     *ZEROS        CDEDRC
173900980401     C                   MOVEL     *BLANKS       CDENO1
174000980401     C                   MOVEL     *BLANKS       CDENO2
174100980401     C                   MOVEL     *BLANKS       CDECMC
174200980401     C                   MOVEL     *BLANKS       CDECCA
174300980401     C                   MOVEL     *BLANKS       CDEERR
174400980401     C                   MOVEL     *BLANKS       CDESTS
174500980401     C                   MOVEL     *BLANKS       CDECOD
174600980401     C                   MOVEL     *BLANKS       CDEMOT
174700980401     C                   MOVEL     *BLANKS       CDEFL1
174800980401     C                   MOVEL     *BLANKS       CDEFL2
174900980406      *
175000980406     C                   EXSR      MAN2VO
175100980406     C                   MOVEL     CD§FL1        CDEFL1
175200980406     C                   MOVEL     CD§FL2        CDEFL2
175300980320      *
175400980320     C                   UPDATE    FNCDE000
175500980320      *
175600980320     C                   ENDSR
175700980226      *----------------------------------------------------------------
175800980226      *? WRTCDA - Scrittura dati FNCDA
175900980226      *----------------------------------------------------------------
176000980407     C     WRTCDA        BEGSR
176100980226      *
176200980407      * Se non ho agganciato FNARB aggiorno FNCDA solo se eccedenza ed ho PCI
176300980407     C     *IN34         IFEQ      '1'
176400980407      *
176500980407     C     CD$STS        IFEQ      '3  '
176600980407     C     CD$COD        ANDEQ     '288'
176700980227     C     YY            ANDGT     *ZEROS
176800980407     C                   DO        YY            XY                             elaboro i segnacolli
176900980407     C                   EXSR      SUBECC
177000980407     C                   EXSR      ELACDA
177100980407     C                   END
177200980407     C                   END
177300980227      *
177400980407     C                   ELSE
177500980407     C                   EXSR      ELACDA
177600980407     C                   END
177700980226      *
177800980226      *  Azzero variabili
177900980226     C                   Z-ADD     0             YY
178000980320     C                   Z-ADD     0             XY                3 0
178100980312     C                   MOVEL     *BLANKS       SGN
178200980312     C                   MOVEL     *BLANKS       SGT
178300980226      *
178400980226     C                   ENDSR
178500980227      *----------------------------------------------------------------
178600980227      *? SUBECC - GESTIONE ECCEDENZE
178700980227      *----------------------------------------------------------------
178800980227     C     SUBECC        BEGSR
178900980227      *
179000980407     C                   SETON                                        34
179100980407      *
179200980408      *  Ricavo il numero spedizione agganciando FNARS e FNART
179300980227     C                   MOVEL     SGN(XY)       KNOT
179400021029      *  Ora occorre controllare il TRC poichè la vista logica seleziona
179500021029      *  oltre al tipo "E"= Euroexpress and "F" segnacollo normale
179600021029      *   ovviamente qui interessa solo il tipo Euroexpress
179700051017     C     KARS          setll     FIARS000
179800051017     C     KARS          reade     FIARS000                               36
179900021029     C     *IN36         doweq     *Off
180000021029     c                   if        ArsTrc ='E'
180100021029     c                   leave
180200021029     c                   end
180300051017     C     KARS          reade     FIARS000                               36
180400021029     c                   enddo
180500980227      *
180600980407     C     *IN36         IFEQ      '1'
180700980227     C                   Z-ADD     9999          WANNO
180800980227     C                   Z-ADD     999           WLINPA
180900980227     C                   Z-ADD     99            WSERIE
181000980227     C                   Z-ADD     9999999       WSPEDI
181100980227     C                   MOVEL     ERR(7)        DESERR
181200980227     C                   MOVEL     SGN(XY)       WVALOR
181300980227     C                   EXSR      STPERR
181400980227      *
181500980407     C                   ELSE
181600980227     C                   Z-ADD     ARSFLS        KFLS
181700980227     C                   Z-ADD     ARSLNA        KLNA
181800980227     C                   Z-ADD     ARSNRS        KNRS
181900980227     C                   Z-ADD     ARSNSC        KNSC
182000980227     C     KART1         CHAIN     FNART027                           36
182100980312      *
182200980407     C     *IN36         IFEQ      '1'
182300980227     C                   Z-ADD     9999          WANNO
182400980227     C                   Z-ADD     999           WLINPA
182500980227     C                   Z-ADD     99            WSERIE
182600980227     C                   Z-ADD     9999999       WSPEDI
182700980227     C                   MOVEL     ERR(7)        DESERR
182800980227     C                   MOVEL     SGN(XY)       WVALOR
182900980227     C                   EXSR      STPERR
183000980227      *
183100980407     C                   ELSE
183200980227     C                   Z-ADD     ARTAAS        WAAS
183300980227     C                   Z-ADD     ARTLNP        WLNP
183400980227     C                   Z-ADD     ARTNRS        WNRS
183500980227     C                   Z-ADD     ARTNSP        WNSP
183600980407      * Aggancio FNARB, se non trovato IN34 = '1'
183700980407     C                   EXSR      ARRSPE
183800980407     C     *IN34         IFEQ      '1'
183900980407     C                   Z-ADD     9999          WANNO
184000980407     C                   Z-ADD     999           WLINPA
184100980407     C                   Z-ADD     99            WSERIE
184200980407     C                   Z-ADD     9999999       WSPEDI
184300980407     C                   MOVEL     ERR(7)        DESERR
184400980407     C                   MOVEL     SGN(XY)       WVALOR
184500980407     C                   EXSR      STPERR
184600980407     C                   END
184700980227      *
184800980407     C                   END
184900980407     C                   END
185000980227      *
185100980227     C                   ENDSR
185200980407      *----------------------------------------------------------------
185300980407      *? ELACDA - Scrittura dati FNCDA
185400980407      *----------------------------------------------------------------
185500980407     C     ELACDA        BEGSR
185600980407      *
185700980407      * Verifico se esistono dettaglio e totale
185800980407     C                   SETOFF                                       3337
185900980407     C                   Z-ADD     CDAAAS        KAAS
186000980407     C                   Z-ADD     CDALNP        KLNP
186100980407     C                   Z-ADD     CDANRS        KNRS
186200980407     C                   Z-ADD     CDANSP        KNSP
186300980407     C                   MOVEL     'T'           KTRC
186400980407      *  Testata
186500980407     C     KCDA          SETLL     FNCDA000                               33
186600980407      *  Dettaglio
186700980408     C   33              MOVEL     'D'           KTRC
186800980407     C   33KCDA          SETLL     FNCDA000                               37
186900980407      *
187000980421      *  Per colli evento = 0 imposto come colli evento i colli spedizione
187100980421      *    se non ho PCI, in caso contrario imposto il numero di PCI ricevuti
187200980421     C     CD$NCL        IFEQ      *ZEROS
187300980421     C     YY            IFEQ      *ZEROS
187400980421     C                   Z-ADD     ARBNCL        CD$NCL
187500980421     C                   ELSE
187600980421     C                   Z-ADD     YY            CD$NCL
187700980421     C                   END
187800090121      ** -----
187900090121     c                   else
188000090121      *  significa che stanno inviando tutti i segnacolli della spedizione
188100090121      *  quindi è come se stessero dando uno status su tutta la spedizione
188200090121      *  Non si dovrà andare a vedere i segnacolli.
188300090121     c                   if        ARBNCL = CD$NCL and arbNCL = yy
188400090121     c                   z-add     0             yy
188500090121     c                   end
188600090121      ** -----
188700980421     C                   END
188800980407      *-----------------------------------------------------
188900980407      *    *IN33 = '1' => esiste FNCDA testata e viceversa
189000980407      *    *IN37 = '1' => esiste FNCDA dettaglio e viceversa
189100980407      *     YY   = 0   => evento totale in elaborazione
189200980407      *     YY   > 0   => evento parziale in elaborazione
189300980428      * --------------
189400980428      *  Gli eventi di mancanza e gli eventi con colli evento <> da colli sped.
189500980428      *  sono considerati come eventi parziali
189600980428      *-----------------------------------------------------
189700980407      *
189800980407     C     *IN33         IFEQ      '0'                                          -- 1
189900980407      *
190000980407     C     YY            IFEQ      *ZEROS                                       -- 2
190100980421     C     ARBNCL        ANDEQ     CD$NCL
190200090821     C**** §CETEA        ANDNE     'M'
190300980407     C                   EXSR      WRATST                                       scrivo testata
190400980407     C                   ELSE                                                   -- 2 --
190500980407     C                   EXSR      WRATS0                                       scrivo testata a 0
190600980407     C                   EXSR      WRADET                                       scrivo dettaglio
190700980407     C                   END                                                       2 --
190800980407      *
190900980407     C                   ELSE                                                   -- 1 --
191000980407      *
191100980407     C     *IN37         IFEQ      '0'                                          -- 2
191200980407     C                   EXSR      UPATE0                                       wrt det + upd tst 0
191300980407     C                   EXSR      WRADET                                       scrivo dettaglio
191400980407     C                   ELSE                                                   -- 2 --
191500980408     C                   EXSR      MAN2VA                                       Doppio 3-117
191600980407     C                   EXSR      WRADET                                       scrivo dettaglio
191700980407     C                   END                                                       2 --
191800980407      *
191900980407     C                   END                                                       1 --
192000980407      *
192100980407     C                   ENDSR
192200980408      *----------------------------------------------------------------
192300980408      *? MAN2VA - Doppia segnalazione di mancanza PER A.T.R.
192400980408      *----------------------------------------------------------------
192500980408     C     MAN2VA        BEGSR
192600980408      *
192700980408      * Se è la 2a volta che trovo l'evento 3 - 117 imposto CD§FL1 = 'S'
192800980408     C                   MOVEL     W2VOLT        CD§FL1
192900980408     C     CD§FL1        IFEQ      'S'
193000980408     C     WSTS          ANDEQ     '3  '
193100980408     C     WCODE         ANDEQ     '117'
193200980408     C                   MOVEL     'S'           CD§FL2
193300980408     C                   END
193400980408      *
193500980408     C                   ENDSR
193600980407      *----------------------------------------------------------------
193700980407      *? WRATST - Scrittura dati FNCDA testata
193800980407      *----------------------------------------------------------------
193900980407     C     WRATST        BEGSR
194000980407      *
194100980408      * Se è la 2a volta che trovo l'evento 3 - 117 imposto 'S' in CD§FL1
194200980407     C                   MOVEL     W2VOLT        CD§FL1
194300980407     C     CD§FL1        IFEQ      'S'
194400980408     C     WSTS          IFEQ      '3  '
194500980407     C     WCODE         ANDEQ     '117'
194600980407     C                   MOVEL     'S'           CD§FL2
194700980407     C                   ELSE
194800980407     C                   MOVEL     *BLANKS       CD$ERR
194900980407     C                   END
195000980407     C                   END
195100980407      *
195200980407     C                   EXSR      MOVCD$
195300980407     C                   MOVEL     WTESLB        CDAFTX
195400980408     C                   MOVEL     ARBRMN        CDARMN
195500980408     C                   Z-ADD     ARBLNA        CDALNA
195600980408     C                   MOVEL     'T'           CDATRC
195700980407      *
195800010802     C     §PUATR        IFEQ      'S'
195900980407     C                   WRITE     FNCDA000
196000010802     C                   END
196100980407      *
196200980407     C                   ENDSR
196300980407      *----------------------------------------------------------------
196400980407      *? MOVCD$ - Aggiorno campi di FNCDA per aggiornamento/scrittura
196500980407      *----------------------------------------------------------------
196600980407     C     MOVCD$        BEGSR
196700980407      *
196800980407     C                   Z-ADD     CD$DAM        CDADAM
196900980407     C                   Z-ADD     CD$HRA        CDAHRA
197000980407     C                   Z-ADD     CD$NCL        CDANCL
197100980422     C                   MOVEL     CD$CCA        CDACCA
197200980407     C                   MOVEL     CD$CMC        CDACMC
197300980407     C                   MOVEL     CD$NO1        CDANO1
197400980407     C                   MOVEL     *BLANKS       CDANOT
197500980407     C                   MOVEL     CD$ERR        CDAERR
197600980407     C                   MOVEL     CD$STS        CDASTS
197700980407     C                   MOVEL     CD$COD        CDACOD
197800980407     C                   MOVEL     CD$MOT        CDAMOT
197900980407     C                   MOVEL     CD§FL1        CDAFL1
198000980407     C                   MOVEL     CD§FL2        CDAFL2
198100980407      *
198200980407     C                   ENDSR
198300980407      *----------------------------------------------------------------
198400980407      *? WRATS0 - Scrittura dati FNCDA, solo dati base testata
198500980407      *----------------------------------------------------------------
198600980407     C     WRATS0        BEGSR
198700980407      *
198800980408      * Se è la 2a volta che trovo l'evento 3 - 117 imposto 'S' in CD§FL1
198900980408     C                   EXSR      MAN2VA
199000980407      *
199100980407     C                   MOVEL     CD§FL1        CDAFL1
199200980407     C                   MOVEL     CD§FL2        CDAFL2
199300980408      *
199400980408      * Se non ho il numero spedizione azzero rif. mittente e imposto linea arri
199500980408     C     CDANSP        IFGT      *ZEROS
199600980408     C                   MOVEL     ARBRMN        CDARMN
199700980408     C                   Z-ADD     ARBLNA        CDALNA
199800980408     C                   ELSE
199900980408     C                   MOVEL     *ZEROS        CDARMN
200000980408     C                   Z-ADD     WLNA          CDALNA
200100980408     C                   END
200200980408      *
200300980407     C                   MOVEL     WTESLB        CDAFTX
200400980407     C                   MOVEL     *BLANKS       CDANOT
200500980408     C                   MOVEL     'T'           CDATRC
200600980407      *
200700010802     C     §PUATR        IFEQ      'S'
200800980407     C                   WRITE     FNCDA000
200900010802     C                   END
201000980407      *
201100980407     C                   ENDSR
201200980407      *----------------------------------------------------------------
201300980407      *? WRADET - Scrivo dati FNCDA dettaglio
201400980407      *----------------------------------------------------------------
201500980407     C     WRADET        BEGSR
201600980407      *
201700980407     C                   EXSR      MOVCD$
201800980407     C                   MOVEL     WTESLB        CDAFTX
201900980408     C                   MOVEL     'D'           CDATRC
202000980408      *
202100980408      * Se non ho il numero spedizione azzero rif. mittente e imposto linea arri
202200980408     C     CDANSP        IFGT      *ZEROS
202300980408     C                   MOVEL     ARBRMN        CDARMN
202400980408     C                   Z-ADD     ARBLNA        CDALNA
202500980408     C                   ELSE
202600980408     C                   MOVEL     *ZEROS        CDARMN
202700980408     C                   Z-ADD     WLNA          CDALNA
202800980408     C                   END
202900980408      *
203000980407      *
203100980407      * Se manca dettaglio PCI scrivo 1 rcd con segnacollo = blanks
203200980407     C     YY            IFEQ      0
203300980408     C                   MOVEL     *BLANKS       CDANOT
203400010802     C     §PUATR        IFEQ      'S'
203500980407     C                   WRITE     FNCDA000
203600010802     C                   END
203700980407     C                   ELSE
203800980407      *
203900980407     C                   Z-ADD     1             X
204000980407     C     X             DOWLE     YY
204100980407     C                   MOVEL     SGN(X)        CDANOT
204200980407     C                   ADD       1             X
204300010802     C     §PUATR        IFEQ      'S'
204400980407     C                   WRITE     FNCDA000
204500010802     C                   END
204600980407     C                   END
204700980407      *
204800980407     C                   END
204900980407      *
205000980407     C                   ENDSR
205100980407      *----------------------------------------------------------------
205200980407      *? UPATE0 - Trasformo CDA testata esistente in dettaglio e azzero testata
205300980407      *----------------------------------------------------------------
205400980407     C     UPATE0        BEGSR
205500980407      *
205600980407      *  trasformo testata in dettaglio
205700980407     C                   Z-ADD     CDAAAS        KAAS
205800980407     C                   Z-ADD     CDALNP        KLNP
205900980407     C                   Z-ADD     CDANRS        KNRS
206000980407     C                   Z-ADD     CDANSP        KNSP
206100980407     C                   MOVEL     'T'           KTRC
206200980407      *
206300980407     C     KCDA          CHAIN     FNCDA000                           32
206400980407     C                   MOVEL     'D'           CDATRC
206500980407     C                   EXCEPT    AGGCDA
206600980407      *
206700980407      *  Scrivo testata
206800980407     C                   Z-ADD     *ZEROS        CDADAM
206900980407     C                   Z-ADD     *ZEROS        CDAHRA
207000980407     C                   Z-ADD     *ZEROS        CDANCL
207100980407     C                   MOVEL     *BLANKS       CDACCA
207200980407     C                   MOVEL     *BLANKS       CDACMC
207300980407     C                   MOVEL     *BLANKS       CDANO1
207400980407     C                   MOVEL     *BLANKS       CDANOT
207500980407     C                   MOVEL     *BLANKS       CDAERR
207600980407     C                   MOVEL     *BLANKS       CDASTS
207700980407     C                   MOVEL     *BLANKS       CDACOD
207800980407     C                   MOVEL     *BLANKS       CDAMOT
207900980407     C                   MOVEL     *BLANKS       CDAFL1
208000980407     C                   MOVEL     *BLANKS       CDAFL2
208100980407     C                   MOVEL     'T'           CDATRC
208200980407      *
208300980407     C     CDAFL1        IFNE      *BLANKS
208400980407     C     CDAFL2        ANDNE     *BLANKS
208500980408     C                   EXSR      MAN2VA
208600980407     C                   MOVEL     CD§FL1        CDAFL1
208700980407     C                   MOVEL     CD§FL2        CDAFL2
208800980407     C                   END
208900980407      *
209000010802     C     §PUATR        IFEQ      'S'
209100980407     C                   WRITE     FNCDA000
209200010802     C                   END
209300980407      *
209400980407     C                   ENDSR
209500980226      *----------------------------------------------------------------
209600980226      *? AZZBOL - AZZERA DATI BOLLA
209700980226      *----------------------------------------------------------------
209800960531     C     AZZBOL        BEGSR
209900980226      *
210000980226      * Eseguo clear formato record
210100960531     C                   CLEAR                   FNCDE000
210200980226     C                   CLEAR                   FNCDA000
210300980226      * Pulisco ds impostazione dati
210400000119     C                   CLEAR                   EDST15DS
210500000119     C                   CLEAR                   EDSD10DS
210600000119     C                   CLEAR                   EDSD15DS
210700000119     C                   CLEAR                   EDSD20DS
210800960531     C                   CLEAR                   WST10
210900980227      * Pulisco schiere
211000960531     C                   CLEAR                   T10
211100960531     C                   CLEAR                   T15
211200960531     C                   CLEAR                   D15
211300960531     C                   CLEAR                   D20
211400980226      * Pulisco campi
211500961120     C                   CLEAR                   WTESLB
211600961125     C                   CLEAR                   WMEMCO
211700961125     C                   CLEAR                   WTOCOP
211800961125     C                   CLEAR                   WARR20
211900980320      *
212000980320     C                   Z-ADD     *ZEROS        CD§DCM
212100980320     C                   Z-ADD     *ZEROS        CD§HMC
212200980320     C                   Z-ADD     *ZEROS        CD§DLA
212300980320     C                   Z-ADD     *ZEROS        CD§DAG
212400980320     C                   Z-ADD     *ZEROS        CD§DRC
212500980320     C                   MOVEL     *BLANKS       CD§NO1
212600980320     C                   MOVEL     *BLANKS       CD§NO2
212700980320     C                   MOVEL     *BLANKS       CD§CMC
212800980320     C                   MOVEL     *BLANKS       CD§CCA
212900980320     C                   MOVEL     *BLANKS       CD§ERR
213000980320     C                   MOVEL     *BLANKS       CD§STS
213100980320     C                   MOVEL     *BLANKS       CD§COD
213200980320     C                   MOVEL     *BLANKS       CD§MOT
213300980320     C                   MOVEL     *BLANKS       CD§FL1
213400980320     C                   MOVEL     *BLANKS       CD§FL2
213500980226      *
213600980407     C                   Z-ADD     *ZEROS        CD$DAM
213700980407     C                   Z-ADD     *ZEROS        CD$HRA
213800980407     C                   Z-ADD     *ZEROS        CD$NCL
213900980407     C                   MOVEL     *BLANKS       CD$CCA
214000980407     C                   MOVEL     *BLANKS       CD$CMC
214100980407     C                   MOVEL     *BLANKS       CD$NO1
214200980407     C                   MOVEL     *BLANKS       CD$ERR
214300980407     C                   MOVEL     *BLANKS       CD$STS
214400980407     C                   MOVEL     *BLANKS       CD$COD
214500980407     C                   MOVEL     *BLANKS       CD$MOT
214600980407      *
214700960531     C                   ENDSR
214800960531     C*----------------------------------------------------------------
214900960531     C*? STPERR: STAMPA ERRORE
215000960531     C*----------------------------------------------------------------
215100960531     C     STPERR        BEGSR
215200960531     C*
215300970206     C     *INOF         IFEQ      '1'
215400970206     C                   EXCEPT    TESTA
215500970206     C                   EXCEPT    ERRCLI
215600970206     C                   END
215700960531     C                   EXCEPT    DETER
215800960531     C*
215900960531     C                   ENDSR
216000960528     C*----------------------------------------------------------------
216100960528     C*? *INZSR - OPERAZIONI INIZIALI
216200960528     C*----------------------------------------------------------------
216300960528     C     *INZSR        BEGSR
216400980320      *
216500960530     C     *ENTRY        PLIST
216600960530     C                   PARM                    KPJBA
216700980406      *
216800960715     C                   MOVEL     'S'           WPRIMA            1
216900040608     c                   move      'N'           In_Stampa         1
217000980406      *
217100980407      * Definisco chiavi di accesso
217200960531     C     KCDE          KLIST
217300960531     C                   KFLD                    KNDC
217400960531     C                   KFLD                    KAAS
217500960528     C                   KFLD                    KLNP
217600960528     C                   KFLD                    KNRS
217700961119     C                   KFLD                    KNSP
217800961007     C     KARB          KLIST
217900961119     C                   KFLD                    KAAS
218000961119     C                   KFLD                    KLNP
218100961119     C                   KFLD                    KNRS
218200961119     C                   KFLD                    KNSP
218300961112     C     KRDE          KLIST
218400961119     C                   KFLD                    KAAP
218500961119     C                   KFLD                    KLPP
218600961119     C                   KFLD                    KNRP
218700961119     C                   KFLD                    KNSP1
218800961119     C                   KFLD                    KNMC
218900980526     C     KRDA          KLIST
219000980526     C                   KFLD                    WAAS
219100980526     C                   KFLD                    WLNP
219200980526     C                   KFLD                    WNRS
219300980526     C                   KFLD                    WNSP
219400980526     C                   KFLD                    KNMC
219500961119     C     KLBL          KLIST
219600961119     C                   KFLD                    KAAP
219700961119     C                   KFLD                    KLPP
219800961119     C                   KFLD                    KNRP
219900961119     C                   KFLD                    KNSP1
220000961125     C     KART          KLIST
220100961125     C                   KFLD                    KAAP
220200961125     C                   KFLD                    KLPP
220300961125     C                   KFLD                    KNRP
220400961125     C                   KFLD                    KNSP1
220500980312     C     KART1         KLIST
220600980312     C                   KFLD                    KFLS
220700980312     C                   KFLD                    KLNA
220800980312     C                   KFLD                    KNRS
220900980312     C                   KFLD                    KNSC
221000980312     C     KART2         KLIST
221100980312     C                   KFLD                    KAAP
221200980312     C                   KFLD                    KLPP
221300980312     C                   KFLD                    KNRP
221400980312     C                   KFLD                    KNSP1
221500980312     C                   KFLD                    KNSC
221600980407     C     KCDA          KLIST
221700980407     C                   KFLD                    KAAS
221800980407     C                   KFLD                    KLNP
221900980407     C                   KFLD                    KNRS
222000980407     C                   KFLD                    KNSP
222100980407     C                   KFLD                    KTRC
222200980407     C     KCDA1         KLIST
222300980226     C                   KFLD                    KAAS
222400980226     C                   KFLD                    KLNP
222500980226     C                   KFLD                    KNRS
222600980226     C                   KFLD                    KNSP
222700980226     C                   KFLD                    KTRC
222800980226     C                   KFLD                    KDAM
222900980226     C                   KFLD                    KCMC
223000980227     C                   KFLD                    KNOT
223100980227     C     KARS          KLIST
223200980226     C                   KFLD                    KNOT
223300980410     C     KARBM         KLIST
223400980410     C                   KFLD                    MICAAS
223500980410     C                   KFLD                    MICLNP
223600980410     C                   KFLD                    MICNRS
223700980410     C                   KFLD                    MICNSP
223800980227      * Definizione campi di work
223900980410     C     *LIKE         DEFINE    ARBLNA        MICLNA
224000980410     C     *LIKE         DEFINE    CDEAAS        MICAAS
224100980410     C     *LIKE         DEFINE    CDELNP        MICLNP
224200980410     C     *LIKE         DEFINE    CDENRS        MICNRS
224300980410     C     *LIKE         DEFINE    CDENSP        MICNSP
224400980320     C     *LIKE         DEFINE    ARTNSC        KNSC
224500980227     C     *LIKE         DEFINE    ARTLNA        KLNA
224600980227     C     *LIKE         DEFINE    ARTFLS        KFLS
224700980227     C     *LIKE         DEFINE    CDANOT        KNOT
224800980226     C     *LIKE         DEFINE    CDATRC        KTRC
224900980226     C     *LIKE         DEFINE    CDADAM        KDAM
225000980226     C     *LIKE         DEFINE    CDACMC        KCMC
225100980226     C     *LIKE         DEFINE    CDENDC        KNDC
225200960531     C     *LIKE         DEFINE    CDEAAS        KAAS
225300960531     C     *LIKE         DEFINE    CDELNP        KLNP
225400960531     C     *LIKE         DEFINE    CDENRS        KNRS
225500960531     C     *LIKE         DEFINE    CDENSP        KNSP
225600961127     C     *LIKE         DEFINE    CDESTS        WSTS
225700961127     C     *LIKE         DEFINE    CDECOD        WCODE
225800961112     C     *LIKE         DEFINE    RDENMC        KNMC
225900961021     C     *LIKE         DEFINE    TA0007        WQUAPT
226000961119     C     *LIKE         DEFINE    LBLAAP        KAAP
226100961119     C     *LIKE         DEFINE    LBLLPP        KLPP
226200961119     C     *LIKE         DEFINE    LBLNRO        KNRP
226300961119     C     *LIKE         DEFINE    LBLNSP        KNSP1
226400980320      *
226500980320     C     *LIKE         DEFINE    CDEDCM        CD§DCM
226600980320     C     *LIKE         DEFINE    CDEHMC        CD§HMC
226700980320     C     *LIKE         DEFINE    CDEDLA        CD§DLA
226800980320     C     *LIKE         DEFINE    CDEDAG        CD§DAG
226900980320     C     *LIKE         DEFINE    CDEDRC        CD§DRC
227000980320     C     *LIKE         DEFINE    CDENO1        CD§NO1
227100980320     C     *LIKE         DEFINE    CDENO2        CD§NO2
227200980320     C     *LIKE         DEFINE    CDECMC        CD§CMC
227300980320     C     *LIKE         DEFINE    CDECCA        CD§CCA
227400980320     C     *LIKE         DEFINE    CDEERR        CD§ERR
227500980320     C     *LIKE         DEFINE    CDESTS        CD§STS
227600980320     C     *LIKE         DEFINE    CDECOD        CD§COD
227700980320     C     *LIKE         DEFINE    CDEMOT        CD§MOT
227800980320     C     *LIKE         DEFINE    CDEFL1        CD§FL1
227900980320     C     *LIKE         DEFINE    CDEFL2        CD§FL2
228000980407      *
228100980407     C     *LIKE         DEFINE    CDADAM        CD$DAM
228200980407     C     *LIKE         DEFINE    CDAHRA        CD$HRA
228300980407     C     *LIKE         DEFINE    CDACMC        CD$CMC
228400980407     C     *LIKE         DEFINE    CDANCL        CD$NCL
228500980407     C     *LIKE         DEFINE    CDACCA        CD$CCA
228600980407     C     *LIKE         DEFINE    CDANO1        CD$NO1
228700980407     C     *LIKE         DEFINE    CDAERR        CD$ERR
228800980407     C     *LIKE         DEFINE    CDASTS        CD$STS
228900980407     C     *LIKE         DEFINE    CDACOD        CD$COD
229000980407     C     *LIKE         DEFINE    CDAMOT        CD$MOT
229100980320      *
229200980320      * Recupera dati dell'utente
229300980320     C                   Z-ADD     1             CODUT             1 0
229400960531     C                   CALL      'XPARUT'
229500000119     C                   PARM                    UT§DSE0F
229600960531     C                   MOVEL     RAGUT         RSUT             20
229700020925      * imposta l'utente
229800020925      *  Quindi decodifica il profilo generico EDPCEDxxx
229900020925      *   dove xxx è il TERMINAL
230000021114     C                   CLEAR                   Tibs36Ds
230100021114     c                   EVAL      I36ute = Knmus
230200021114     c                   EVAL      I36Tla = 'L'
230300021114     C                   CALL      'TIBS36R'
230400021114     C                   PARM                    Tibs36Ds
230500020911      *
230600960528     C* Imposto variabili
230700960531     C                   Z-ADD     0             KNDC
230800960528     C                   MOVEL     'N'           WFINE             1
230900960531     C                   MOVEL     'S'           WPRIMA            1
231000960531     C* RECUPERO DATA E ORA
231100960531     C                   TIME                    WHHDAT           14 0
231200970210     C                   TIME                    WHHD12           12 0
231300960531     C                   MOVE      WHHDAT        DATA8             8 0
231400970210     C                   MOVE      WHHD12        DATA6             6 0
231500970210     C                   MOVEL     DATA6         DATA              6 0
231600960531     C                   TIME                    WORA              6 0
231700980403      * Giro la data
231800980403     C                   CLEAR                   WLBDA8
231900990810     C                   Z-ADD     DATA8         G02DAT
232000980403     C                   MOVEL     *BLANKS       G02ERR
232100980403     C                   CALL      'XSRDA8'
232200980403     C                   PARM                    WLBDA8
232300990810     C                   Z-ADD     G02INV        DATA8G            8 0
232400960528     C* Caricamento Tabella codici eventi
232500960528     C                   Z-ADD     0             X                 3 0
232600960528     C                   MOVEL     'CE'          TABCOD
232700960528     C     TABCOD        CHAIN     EDTAB01L                           30
232800960528     C     *IN30         DOWEQ     '0'
232900960528     C     TABFLG        IFEQ      *BLANKS
233000960528     C                   ADD       1             X
233100960528     C                   MOVEL     TABKEY        CEV(X)
233200961118     C                   MOVEL     TABUNI        DEV(X)
233300000119     C                   MOVEL     DEV(X)        EDIDSCE
233400960528     C                   END
233500960528     C     TABCOD        READE     EDTAB01L                               30
233600960528     C                   END
233700960528     C* Caricamento Tabella Partner esteri
233800960528     C                   Z-ADD     0             X                 3 0
233900960528     C                   MOVEL     'PT'          TABCOD
234000960528     C     TABCOD        CHAIN     EDTAB01L                           30
234100960528     C     *IN30         DOWEQ     '0'
234200960528     C     TABFLG        IFEQ      *BLANKS
234300960528     C                   ADD       1             X
234400960528     C                   MOVEL     TABKEY        CPT(X)
234500000120     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
234600960531     C                   MOVEL     TABUNI        DPT(X)
234700960528     C                   END
234800960528     C     TABCOD        READE     EDTAB01L                               30
234900960528     C                   END
235000121105      *
235100121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
235200121105     c                   exsr      ChecK_PT
235300121105      *
235400961024     C*
235500961007     C                   MOVEL     'PU'          TABCOD
235600961007     C     TABCOD        CHAIN     EDTAB01L                           30
235700961007     C     *IN30         DOWEQ     '0'
235800961007     C     TABFLG        IFEQ      *BLANKS
235900961024     C                   Z-ADD     1             X                 3 0
236000961024     C     TABKEY        LOOKUP    CPT(X)                                 32
236100961024     C     *IN32         IFEQ      '1'
236200961007     C                   MOVEL     TABUNI        DPU(X)
236300961007     C                   END
236400961024     C                   END
236500961007     C     TABCOD        READE     EDTAB01L                               30
236600961007     C                   END
236700960715     C*
236800960715     C                   MOVEL     *ALL'-'       CMP132          132
236900961024     C                   SETON                                        OA
237000961024     C                   SETON                                        OF
237100960528     C*
237200960528     C                   ENDSR
237300121105      *---------------------------------------------------------------*
237400121105      * CTRL caricamento schiere    PT                               -*
237500121105      *---------------------------------------------------------------*
237600121105     C     Check_PT      begsr
237700121105     C*
237800121105     c* Controllo riempimento schiera
237900121105     c                   clear                   trul0sds
238000121105     c                   eval      i0sski='CPT'
238100121105     c                   eval      i0sele=%elem(CPT)
238200121105     c                   eval      i0spie=x
238300121105     c                   eval      i0sfile='EDTAB00F'
238400121105     c                   eval      i0ssif=knsif
238500121105     c                   eval      i0spgm='TRTC84R'
238600121112     c                   movel     kpjbu         kpjbusav
238700121112     c                   clear                   kpjbu
238800121105     c                   movel     trul0sds      kpjbu
238900121105     c                   call      'TRUL0SR'
239000121105     c                   parm                    kpjba
239100121112     c                   movel     kpjbusav      kpjbu
239200121105     C*
239300121105     C                   ENDSR
239400980407      *****************************************************************
239500980407     OFNCDA000  E            AGGCDA
239600980407     O                       CDATRC
239700980407      *****************************************************************
239800960531     OQSYSPRT   E            TESTA            01
239900960531     O                       RSUT                20
240000960531     O                                           46 '** STAMPA ERRORI RICEZIO'
240100960531     O                                              'NE DATE CONSEGNA DA PAR'
240200960531     O                                              'TNER ESTERI **'
240300960531     O                       DATA               121 '  /  /  '
240400960531     O                                          127 'Pag.:'
240500960531     O                       PAGE          Z    132
240600960531     O          E            TESTA          1 02
240700960531     O                       WORA               121 '  :  :  '
240800970206     O          E            ERRCLI         1
240900970206     O                                           15 'CODICE CLIENTE:'
241000970206     O                       §PTKSC        Z     23
241100970206     O                       WCOD                60
241200960531     O*------------------------------------
241300960531     O          E            DETER          1
241400960531     O                       DESERR              78
241500960531     O                       WVALOR             113
241600961112     O                       WANNO         Z    120
241700961112     O                       WLINPA        Z    123
241800961112     O                       WSERIE        Z    125
241900961112     O                       WSPEDI        Z    132
242000960531     O*------------------------------------
242100980320     O**      E                DETPAR
242200980320     O**                                 24 'Colli conseganti su sped'
242300980320     O**                                    'izione'
242400980320     O**                       CDEAASZ   36
242500980320     O**                       CDELNPZ   40
242600980320     O**                       CDENRSZ   43
242700980320     O**                       CDENSPZ   51
242800980320     O**                                 52 ':'
242900980320     O**      E  1             DETCOL
243000980320     O**                       SGN1      50
243100980320     O**                       SGN2    +  1
243200980320     O**                       SGN3    +  1
243300960531     O*----------------------------------------------------------------
243400960628     OSYSPRT    E            TEST1            01
243500960628     O                       RSUT                20
243600960628     O                                           45 '** STAMPA DATI RICEVUTI'
243700961024     O                                         +  1 'IN FASE DI TRASCODIFICA'
243800960628     O                                              ' DATE CONSEGNA DA PARTN'
243900960628     O                                              'ER ESTERI **'
244000960628     O                       DATA               121 '  /  /  '
244100960628     O                                          127 'Pag.:'
244200980406     O                       PAGE1         Z    132
244300961121     O          E            TEST1            02
244400960628     O                       WORA               121 '  :  :  '
244500960628     O*  Dati messaggio
244600961121     O          E            DATMSG      2  1
244700960628     O                                           18 'DATI MESSAG: MITT.'
244800960628     O                       TA0004            +  1
244900970923     O                       TA0007            +  0
245000960628     O                                         +  1 'DESTIN.'
245100960628     O                       TA0010            +  1
245200970923     O                       TAA007            +  0
245300960628     O                                         +  1 'DATA'
245400970923     O                       WDTMSG            +  1 '  /  /    '
245500960628     O                                         +  1 'ORA'
245600970923     O                       WORMSG            +  1 '  :  '
245700960628     O          E            DATMSG         1
245800960628     O                                            4 'COD.'
245900960628     O                       TA1001            +  1
246000960628     O                                         +  1 'NOME DOC.'
246100960628     O                       TA1000            +  1
246200960703     O                                         +  1 'FUNZ.'
246300960628     O                       TA1225            +  1
246400960628     O                                         +  1 'NUM.'
246500960628     O                       TA1004            +  1
246600970923     O                                         +  2 'DATA'
246700961112     O                       WDADOC            +  1 '  /  /    '
246800961112     O                                         +  1 'ORA'
246900961112     O                       WHHDOC            +  1 '  :  '
247000960715     O*  Dati Partner
247100960715     O          E            DATPAM         1
247200960715     O                                           14 'PARTNER MITT.:'
247300960715     O                       TE3036            +  1
247400960715     O          E            DATPAD         1
247500960715     O                                           14 'PARTNER DEST.:'
247600960715     O                       TE3036            +  1
247700960628     O*  Note intero documento
247800960628     O          E            NOTMSG      1
247900960628     O                                           18 'NOTE DOCUMENTO...:'
248000960628     O                       TC4451            +  1
248100960628     O          E            NOTMS1         1
248200960628     O                       WNOT              +  1
248300960628     O*  Spedizione
248400960715     O          E            DATSPE      1  1
248500960715     O                       CMP132             132
248600960715     O          E            DATSPE      1
248700961111     O                                           14 'NR.SPEDIZIONE:'
248800961119     O                       KAAS              +  1
248900961119     O                       KLNP          Z   +  1
249000961119     O                       KNRS          Z   +  1
249100961119     O                       KNSP          Z   +  1
249200961007     O                     34                  +  2 'NON TROVATA FRA LE BOLLE'
249300961007     O                     34                  +  1 'IN ARRIVO'
249400970227     O                     34                  +  5 'RIF. PARTNER:'
249500970227     O                     34W1004             +  1
249600960628     O*  Evento
249700960715     O          E            DATEVE      1
249800960628     O                                           13 'CODICE EVENTO'
249900960628     O                       DB9011            +  1
250000960628     O                       DB9013            +  1
250100960628     O                                         +  1 'DATA'
250200960628     O                       WDTEVE            +  1
250300960628     O                                         +  1 'ORA'
250400960628     O                       WHHEVE            +  1
250500961112     O                                         +  3 '-'
250600961112     O                       WC2A              +  2
250700961112     O                       WDEC              +  1
250800961112     O***                              +  1 'RIFER.SPEDIZIONE:'
250900961112     O***                      DB1153  +  1
251000961112     O***                      DB1154  +  1
251100960628     O*  Note Consegna
251200960715     O          E            NOTCON      1
251300960628     O                                           13 'NOTE CONSEGNA'
251400960628     O                       WNOTCO            +  1
251500960628     O*  Segnacolli
251600960628     O          E            DATSGN      1
251700960628     O                                           14 'NR.SEGNACOLLI:'
251800960628     O          E            DATSG1      1
251900960628     O                       SGN1                50
252000960628     O                       SGN2              +  1
252100960628     O                       SGN3              +  1
252200980225      *----------------------------------------------------------------
252300980408** ERR                                                               *
252400960531Data/Ora evento giacenza errato
252500960531Codice evento non codificato su tabella 2A
252600960531Riferimento mittente non numerico
252700961127Numero di spedizione errato !!  -  Codice evento xxx - xxx del xxxxxxxx
252800961203Colli xxxxx DI xxxxx
252900980225ATTENZIONE - Evento non congruente con il tipo messaggio -
253000980227Evento eccedenza - spedizione non trovata
253100980408Ricevuto evento di messa in consegna per sped. già consegnata
