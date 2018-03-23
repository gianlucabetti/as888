000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200110503      * TNTLP1R *----------------------------------------------------*
000300110503      *         - Anagrafica punti di scambio traini
000400940915      *--------------------------------------------------------------*
000500110503     FTNTLp1d   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700110503     Ftnpsc01l  uF a E           K DISK
000800060315     Ftabel00f  IF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200060404     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
001300060315     d
001400040917     D KPJBA         E DS
001500060315     d Tibs02Ds      e ds
001600060315     d Azuteds       e ds                  extname(Azute00f)
001700060315     d dDatiute      e ds
001800060315     d dLat          e ds
001900060315     d dute01        e ds
002000060315     D TIBS34DS      E DS
002100050103     d Dataiso         s               d   datfmt(*iso)
002200050103     d Dataeur         s               d   datfmt(*eur)
002300050103     d
002400110504     d wrkgetlista     s           4096    varying
002500060315     d wabi            s                   like(UteAut)
002600060315     d kcod            s                   Like(tblCod)
002700060315     d kkey            s                   Like(TblKey)
002800110503     d varcap          s                   Like(psccap)
002900110503     d varloc          s                   Like(pscloc)
003000110503     d amgdde          s                   Like(pscdde)
003100110503     d amgdsc          s                   Like(pscdsc)
003200110503     d wdat            s                   Like(pscdsc)
003300110503     d amgdat          s                   Like(pscdsc)
003400110503     d kpsc            s                   Like(pscpsc)
003500110503     d kdde            s                   Like(pscdde)
003600110504     d wdde            s                   Like(pscdde)
003700110504     d wdsc            s                   Like(pscdsc)
003800110503      *
003900110503     d tntlp0ds      e ds
004000060315     D* DS PER FNLV13R - CONTROLLO DI UN CAP
004100060315     D DSLV13        E DS                  EXTNAME(FNLV13DS)
004200060315     D* DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
004300060315     D DSLV14        E DS                  EXTNAME(FNLV14DS)
004400060315     D* DS PER TISI95R - CONTROLLO DI CAP
004500060315     D DSSI95        E DS                  EXTNAME(TISI95DS)
004600060328     d
004700040917     C**********************************************************************
004800060315     c* INDICATORI
004900060315     C**********************************************************************
005000060315     C* 01    - PROTEZIONE CAMPI VIDEATA
005100060315     C* 02    - IMMISSIONE
005200060315     C* 03    - ANNULLAMENTO
005300060315     C* 04    - MODIFICA
005400060315     C* 05    - INTERROGAZIONE
005500060404     C* 06    - evidenzia campo di evento duplicabile  <> da STD
005600050103     C**********************************************************************
005700040917     C     *ENTRY        PLIST
005800040917     C                   PARM                    KPJBA
005900110503     C                   PARM                    tntlp0ds
006000060315     c
006100060315     c* Verifico se l'utente  abilitato alla gestione del calendario
006200060315     c                   exsr      CARINIZ
006300060315     c                   if        dp0err='1'
006400060315     c                   goto      fine
006500060315     c                   endif
006600060329     c
006700060329     c* Duplica
006800060329     c                   if        dp0op1='O03'
006900060329     c                   exsr      Duplica
007000060329     c                   goto      fine
007100060329     c                   endif
007200040917     c
007300040917     C     FOR01         TAG
007400000828      *
007500110503     C                   EXFMT     tlp1d01
007600060315     C                   SETOFF                                       2890
007700060315     c                   setoff                                       404142
007800060315     c                   setoff                                       434445
007900060315     c                   setoff                                       464748
008000060330     c                   setoff                                       495152
008100060330     c                   setoff                                       535455
008200060330     c                   setoff                                       565758
008300060404     c                   setoff                                       5960
008400040917     C* F3 - FINE
008500060315     C                   IF        *INKC
008600060315     C                   EVAL      DP0F03='1'
008700060315     C                   GOTO      FINE
008800060315     C                   ENDIF
008900060404     c
009000060315     C* F12- RITORNO
009100060315     C                   IF        *INKl
009200060315     C                   EVAL      DP0F12='1'
009300060315     C                   GOTO      FINE
009400060315     C                   ENDIF
009500060315     c
009600060315      *  CONTROLLI --> non in interrogazione
009700040917     C                   EXSR      CTRD01
009800040917     c   90              goto      for01
009900000825      *
010000060315     c* Per visualizzazione, enter esce
010100060315     c                   if        dp0op1='O05'
010200060315     c                   goto      fine
010300060315     c                   endif
010400060315     c
010500060315     c  nkf              goto      for01
010600060327     c* F06 - conferma
010700060327     c   kf              exsr      AGGIORNA
010800040917     C*
010900900515      *
011000000000     C     FINE          TAG
011100060315     C**
011200060315     C                   CLEAR                   tibs02ds
011300060315     C                   MOVEL     'C'           T02TLA
011400060315     C                   CALL      'TIBS02R'
011500060315     C                   PARM                    KPJBA
011600060315     C                   PARM                    tibs02ds
011700060315     C**
011800060315     c                   clear                   dslv13
011900060315     C                   MOVEL     'C'           I13TLA
012000060315     C                   CALL      'FNLV13R'
012100060315     C                   PARM                    KPJBA
012200060315     C                   PARM                    DSLV13
012300060315     C                   PARM                    DSSI95
012400000000     C                   SETON                                        LR
012500060329      *-----------------------------------------------------***********
012600060329      * Duplica
012700060329      *-----------------------------------------------------***********
012800060329     C     Duplica       BEGSR
012900060329     c
013000110503     C                   write     tlp1d01
013100060329     c     forw2         tag
013200110503     C                   EXFMT     tlp1dv2
013300060330     c                   setoff                                       2890
013400060330     c                   setoff                                       4041
013500060329     C* F12- RITORNO
013600060329     C                   IF        *INKl
013700060329     C                   EVAL      DP0F12='1'
013800060329     C                   GOTO      Endduplica
013900060329     C                   ENDIF
014000060329     c* controlli video 2
014100060329     c                   exsr      ctrw2
014200060329     c
014300060330     c  Nkf
014400060330     cor 90              goto      forw2
014500060330     c
014600060330     c   KF              exsr      Aggiorna
014700060329     c
014800060329     c     endduplica    endsr
014900900515      *-----------------------------------------------------***********
015000060329      * CONTROLLI                                           *  CTRD01 *
015100900515      *-----------------------------------------------------***********
015200040917     C     CTRD01        BEGSR
015300060330     c
015400110503     c                   if        v1cpsc=*blanks
015500110503     c                   seton                                        409028
015600110503     C                   MOVEL     MSG(2)        V1cMSG
015700110503     c                   goto      endct1
015800110503    1c                   endif
015900110505     c*
016000110505     c* Deve essere univoco nel file
016100110505     c                   if        dp0op1='F06'  or dp0op1='O03'
016200110505     c     v1cpsc        setll     tnpsc01l
016300110505     c                   if        %equal(tnpsc01l)
016400110505     c                   seton                                        409028
016500110505     C                   MOVEL     MSG(11)       V1cMSG
016600110505     c                   goto      endct1
016700110505    1c                   endif
016800110505    1c                   endif
016900060315     c*
017000110505     c
017100110503     c* Data decorrenza obbligatoria
017200110505     c**                 eval      wdat=v1cdde
017300110505     c**                 exsr      CTRDEV
017400110505     c**                 eval      v1cdde=wdat
017500110503
017600110505     c**                 if        *in90
017700110505     c**                 seton                                        41
017800110505     C**                 MOVEL     MSG(3)        V1cMSG
017900110505     c**                 goto      endct1
018000110505    1c**                 endif
018100110505     c**                 eval      amgdde=amgdat
018200110503     c
018300110505     c* Data scadenza
018400110505     c                   clear                   amgdsc
018500110505     c                   if        v1cdsc>0
018600110503     c                   eval      wdat=v1cdsc
018700110503     c                   exsr      CTRDEV
018800110503     c                   eval      v1cdsc=wdat
018900110503
019000110503     c                   if        *in90
019100110503     c                   seton                                        42
019200110503     C                   MOVEL     MSG(3)        V1cMSG
019300110503     c                   goto      endct1
019400110503    1c                   endif
019500110503     c                   eval      amgdsc =amgdat
019600110505     c                   endif
019700110506     c
019800110506     c*  deve essere > della data corrente
019900110506     c                   if        amgdsc<datcor   and amgdsc>0
020000110506     c                   seton                                        422890
020100110506     C                   MOVEL     MSG(12)       V1cMSG
020200110506     c                   goto      endct1
020300110506    1c                   endif
020400110503     c
020500110505     c**                 if        amgdsc<amgdde
020600110505     c**                 seton                                        422890
020700110505     C**                 MOVEL     MSG(9)        V1cMSG
020800110505     c**                 goto      endct1
020900110505    1c**                 endif
021000110504     c
021100110504     c* controllo congruenza con latri dati già inseriti
021200110505     c**                 if        amgdsc<>v1hdsc   or
021300110505     c**                           amgdde<>v1hdde
021400110505     c**                 exsr      carSQL
021500110505     c**
021600110505     c** 90              goto      endct1
021700110505     c**                 endif
021800060330     c
021900041220     c*
022000110506     c* Annullamento effettuabile se punto non utilizzano in traini in decorrenza
022100110506    1c                   if        dp0op1='O04'
022200110506     c                   exsr      SqlPSC
022300110506     c
022400110506     c                   if        *in90
022500060315     c                   goto      endct1
022600060315    1c                   endif
022700110506    1c                   endif
022800110504     c
022900110504     c* indirizzo deve essere completo
023000110504     c                   if        v1cind=*blanks
023100110504     c                   seton                                        479028
023200110504     C                   MOVEL     MSG(5)        V1cMSG
023300110504     c                   goto      endct1
023400110504    1c                   endif
023500110504
023600110504     c                   if        v1cloc=*blanks
023700110504     c                   seton                                        459028
023800110504     C                   MOVEL     MSG(6)        V1cMSG
023900110504     c                   goto      endct1
024000110504    1c                   endif
024100060315     c
024200060315     c* Nazione/cap/località
024300060315    1c                   if        v1cnar<>*blanks or v1cloc<>*blanks
024400060328     c                             or v1ccap<>*blanks or v1cprv<>*blanks
024500060315     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
024600060315     C                   CLEAR                   DSLV13
024700060315     C                   CLEAR                   DSLV14
024800060315     C                   CLEAR                   DSSI95
024900060315     C*
025000060328     c                   if        v1cloc<>*blanks
025100060315     C                   CLEAR                   I14ISO
025200110505     c* se ancora da decorrere --> controllo alla data decorrenza
025300110503     c*  se decorrente --> a udate
025400110503     c*  se scaduto --> alla data scadenza
025500110505     c**                 select
025600110505     c**                 when      amgdde>datcor
025700110505     C**                 MOVEL     amgdde        I14dri
025800110505     c**                 when      amgdde<=datcor and amgdsc>=datcor
025900110505     C**                 MOVEL     datcor        I14dri
026000110505     c**                 other
026100110505     C**                 MOVEL     amgdsc        I14dri
026200110505     c**                 endsl
026300110505     c* Se c'è la data di riferimento la passo, altrimenti data corrente
026400110505     c                   if        dp0dri>0
026500110505     C                   MOVEL     dp0dri        I14dri
026600110505     c                   else
026700110505     C                   MOVEL     datcor        I14dri
026800110505     c                   endif
026900060315     c                   movel     'S'           i14cnz
027000060315     c                   movel     'XX'          i14rsc
027100110503     c                   movel     v1cind        i14ind
027200060315     c                   movel     v1cnar        e14nar
027300060315     c                   movel     v1ccap        e14cap
027400060315     c                   movel     v1cloc        e14loc
027500060315     c                   movel     v1cprv        e14prv
027600060315     C                   CALL      'FNLV14R'
027700060315     C                   PARM                    KPJBA
027800060315     C                   PARM                    DSLV14
027900060329     C* CAMPI CHE POSSO AVER IMPOSTATO
028000060329     C                   MOVEL     E14LOC        v1cloc
028100060329     C                   MOVEL     E14PRV        V1CPRv
028200060329     C                   MOVEL     E14NAR        V1Cnar
028300060329     C                   MOVEL     E14CAP        V1Ccap
028400060315     C*
028500060315     C                   MOVEL     O14ERR        WERR              1
028600060315     C                   MOVEL     O14MSG        V1cMSG
028700060315     C**
028800060315    2C     WERR          IFNE      ' '
028900060315    3C     V1cMSG        IFNE      *BLANKS
029000060315     C                   SETON                                        28
029100060315    3c                   endif
029200060315     c                   seton                                        9044
029300060315     c                   goto      endct1
029400060315    2c                   endif
029500060328    2c                   endif
029600060315     C* CONTROLLO CAP -
029700060329     c                   if        v1cloc=*blanks
029800060328     C                   MOVEL     '3'           I95TCN
029900060328     c                   else
030000060328     C                   MOVEL     '7'           I95TCN
030100060329     C                   MOVEL     'S'           I13AF1
030200060329     C                   MOVEL     'S'           I13SZ2
0303000603299/12 C                   MOVEL     'S'           I13LA3
0304000603299/12 C                   MOVEL     'S'           I13SZ3
030500060328     c                   endif
030600060331     C                   MOVEL     v1ccAP        I95CAP
030700060331     C                   MOVEL     v1cLOC        I95LOC
030800060331     C                   MOVEL     v1cPRV        I95PRV
030900060331     C                   MOVEL     v1cNAR        I95NAR
031000110503     c
031100060315     C                   MOVEL     'S'           I13AF0
031200061010     C                   MOVEL     ' '           I13CNV
031300060315     C                   MOVEL     'S'           I13SZV
031400060315c
031500060331    2C     v1cCAP        IFNE      VARcap
031600060331     C                   CLEAR                   wfor1
031700060315     C                   CLEAR                   AAA
031800060315     C                   CLEAR                   AAB
031900060315     C                   CLEAR                   AAD
032000060331     C                   MOVEL     v1ccap        VARCAp
032100060315    2C                   ENDIF
032200060315    c
032300060315     C* MODIFICA LA LOCALITA'
032400060331    2C     v1cLOC        IFNE      VARLOc
032500060315     C                   CLEAR                   AAA
032600060315     C                   CLEAR                   AAB
032700060315     C                   CLEAR                   AAD
032800060331     C                   MOVEL     v1cloc        VARLOc
032900060315    2C                   ENDIF
033000060315     C**
033100060331     C                   MOVEL     wfor1         E13FZV
033200060315     C                   MOVEL     AAA           E13FZ1
033300060315     C                   MOVEL     AAB           E13FZ2
033400060315     C                   MOVEL     AAD           E13FZ3
033500060315     C**
033600060315     C** CALL
033700060315     C                   CALL      'FNLV13R'
033800060315     C                   PARM                    KPJBA
033900060315     C                   PARM                    DSLV13
034000060315     C                   PARM                    DSSI95
034100060315     C*
034200060315     C                   MOVEL     O13ERR        WERR              1
034300060315     C                   MOVEL     O13MSG        v1cMSG
034400060315     C                   EXSR      CTRERR
034500060315     C**
034600060315     C* REIMPOSTO LE FORZATURE
034700060331     C                   MOVEL     E13FZV        wfor1
034800060315     C                   MOVEL     E13FZ1        AAA
034900060315     C                   MOVEL     E13FZ2        AAB
035000060315     C                   MOVEL     E13FZ3        AAD
035100060315     C**
035200060315    1C     WERR          IFNE      ' '
035300060315    2C     V1cMSG        IFNE      *BLANKS
035400060315     C                   SETON                                        28
035500060315     c                   endif
035600060315     c                   seton                                        9044
035700060315     c                   goto      endct1
035800060315     c                   endif
035900060315     c                   endif
036000060327     c
036100040917     C     ENDCT1        ENDSR
036200060330      *-----------------------------------------------------***********
036300110503      * CONTROLLo Data
036400060330      *-----------------------------------------------------***********
036500060330     c     CTRDEV        BEGSR
036600110503    1c                   if        wdat>0
036700110503     c                   movel     wdat          w002a             2
036800110504     c     *eur          test(d)                 wdat                   30
036900060331    2c                   if        *in30  and w002a='00'
037000110504     c     *dmy          test(d)                 wdat                   30
037100060331     c                   endif
037200060331     c* Errore
037300060330    3c                   if        *in30
037400110503     c                   seton                                        9028
037500060330     c                   goto      endctdev
037600060331     c                   endif
037700060330     c
037800060331     c                   if        w002a='00'
037900110503     c     *dmy          move      wdat          dataeur
038000060330     c                   else
038100110503     c     *eur          move      wdat          dataeur
038200060330     c                   endif
038300060330     c
038400060330     c                   else
038500110503     c                   seton                                        9028
038600060330     c                   goto      endctdev
038700060330     c                   endif
038800060330     c* la trasformo in AAAAAMMGG - iso
038900110503     c                   move      dataeur       wdat
039000060330     c                   movel     dataeur       dataiso
039100110503     c     *iso          movel     dataiso       amgdat            8 0
039200060331     c
039300060330     c
039400060330     c     endctdev      ENDSR
039500110504      *-----------------------------------------------------***********
039600110504      * con SQL controllo decorrenza / scadenza record
039700110504      *-----------------------------------------------------***********
039800110504     c     carSQL        BEGSR
039900110504     c                   eval      wrkgetlista =
040000110504     c                             'select pscdde, pscdsc  -
040100110504     c                               from tnpsc00f where pscpsc= ''' +
040200110505     c                               v1cpsc + ''' and pscatb='' '' and +
040300110505     c                               pscdde<>' + %editc(v1hdde:'X')
040400110504
040500110504     C/EXEC SQL
040600110504     C+ PREPARE s1 FROM :wrkgetlista
040700110504     C/END-EXEC
040800110504     C
040900110504     C/EXEC SQL
041000110504     C+ DECLARE A1 CURSOR FOR S1
041100110504     C/END-EXEC
041200110504     C
041300110504     C/EXEC SQL
041400110504     C+ OPEN a1
041500110504     C/END-EXEC
041600110504
041700110504      *?Leggo il file
041800110504     c                   do        *hival
041900110504     C/EXEC SQL
042000110504     C+ FETCH NEXT FROM a1 INTO : wdde, :wdsc
042100110504     C/END-EXEC
042200110504
042300110504     c                   select
042400110504
042500110504     c                   when      sqlcod = 100
042600110504     c                   leave
042700110504     c                   when      sqlcod < 0
042800110504     c                   seton                                        H1
042900110504     c                   leave
043000110504     c                   other
043100110504     c
043200110504     c                   if        wdsc<amgdde   or wdde>amgdsc
043300110504     c                   else
043400110504     c* errore
043500110504     c                   seton                                        412890
043600110504     c                   eval      v1cmsg=msg(10)
043700110504     c                   leave
043800110504     c                   endif
043900110504     c
044000110504     c                   endsl
044100110504
044200110504     c                   enddo
044300110504
044400110504     C/EXEC SQL
044500110504     C+ CLOSE a1
044600110504     C/END-EXEC
044700110504
044800110504     c                   ENDSR
044900110506      *-----------------------------------------------------***********
045000110506      * controllo se punto di scambio presente nei traini
045100110506      *-----------------------------------------------------***********
045200110506     c     SQLpsc        BEGSR
045300110506     c                   eval      wrkgetlista =
045400110506     c                             'select TLSDDE  -
045500110506     c                               from tntls00f where tlspsc= ''' +
045600110506     c                               v1cpsc + ''' and tlsatb='' '''
045700110506
045800110506     C/EXEC SQL
045900110506     C+ PREPARE s2 FROM :wrkgetlista
046000110506     C/END-EXEC
046100110506     C
046200110506     C/EXEC SQL
046300110506     C+ DECLARE A2 CURSOR FOR S2
046400110506     C/END-EXEC
046500110506     C
046600110506     C/EXEC SQL
046700110506     C+ OPEN a2
046800110506     C/END-EXEC
046900110506
047000110506      *?Leggo il file
047100110506     C/EXEC SQL
047200110506     C+ FETCH NEXT FROM a2 INTO : wdde
047300110506     C/END-EXEC
047400110506
047500110506     c                   select
047600110506
047700110506     c                   when      sqlcod = 100
047800110506     c                   when      sqlcod < 0
047900110506     c                   seton                                        H2
048000110506     c                   other
048100110506     c* errore
048200110506     c                   seton                                        2890
048300110506     c                   eval      v1cmsg=msg(13)
048400110506     c
048500110506     c                   endsl
048600110506
048700110506     C/EXEC SQL
048800110506     C+ CLOSE a2
048900110506     C/END-EXEC
049000110506
049100110506     c                   ENDSR
049200060329      *-----------------------------------------------------***********
049300060329      * CONTROLLI x deplica
049400060329      *-----------------------------------------------------***********
049500060329     C     CTRw2         BEGSR
049600110503     c*   nuovo codice obbligaotiro
049700110503     c                   if        vvcpsc=*blanks
049800110503     c                   seton                                        409028
049900110503     C                   MOVEL     MSG(2)        V1cMSG
050000110503     c                   goto      endct2
050100110503    1c                   endif
050200110505     c*
050300110505     c* Deve essere univoco nel file
050400110505     c     vvcpsc        setll     tnpsc01l
050500110505     c                   if        %equal(tnpsc01l)
050600110505     c                   seton                                        409028
050700110505     C                   MOVEL     MSG(11)       V1cMSG
050800110505     c                   goto      endct2
050900110505    1c                   endif
051000060330     c*
051100110503     c* Data decorr  obbligatoria
051200110505     c**                 eval      wdat=vvcdde
051300110505     c**                 exsr      CTRDEV
051400110505     c**                 eval      vvcdde=wdat
051500110505     c**                 if        *in90
051600110505     c**                 seton                                        41
051700110505     C**                 MOVEL     MSG(3)        V1cMSG
051800110505     c**                 goto      endct2
051900110505    1c**                 endif
052000110505     c**                 eval      amgdde =amgdat
052100060330     c
052200110505     c* Data scadenza
052300110505     c                   clear                   amgdsc
052400110505     c                   if        vvcdsc>0
052500110503     c                   eval      wdat=vvcdsc
052600110503     c                   exsr      CTRDEV
052700110503     c                   eval      vvcdsc=wdat
052800110503     c                   if        *in90
052900110503     c                   seton                                        42
053000110503     C                   MOVEL     MSG(3)        V1cMSG
053100110503     c                   goto      endct2
053200110503    1c                   endif
053300110503     c                   eval      amgdsc =amgdsc
053400110505     c                   endif
053500110503     c
053600110505     c**                 if        amgdsc<amgdde
053700110505     c**                 seton                                        42
053800110505     C**                 MOVEL     MSG(9)        V1cMSG
053900110505     c**                 goto      endct2
054000110505    1c**                 endif
054100110503     c
054200060329     C     ENDCT2        ENDSR
054300060315     C*
054400060315     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
054500060315     C     CTRERR        BEGSR
054600060315     C* IMPOSTO CAMPI PASSATI
054700060315     C     O13RON        IFEQ      'S'
054800060315     C                   MOVEL     O95NAR        v1cNAR
054900060315     C                   ENDIF
055000060315     C     O13ROC        IFEQ      'S'
055100060315     C                   MOVEL     O95CAP        v1cCAP
055200060315     C                   ENDIF
055300060315     C     O13ROP        IFEQ      'S'
055400060315     C                   MOVEL     O95PRV        v1cPRV
055500060315     C                   ENDIF
055600060315     C     O13ROL        IFEQ      'S'
055700060315     C                   MOVEL     O95LOC        v1cLOC
055800060315     C                   ENDIF
055900060315     c
056000060315    2C                   ENDSR
056100050214     C**************************************************************************
056200060315     C*  Carico dati utente per vedere se abilitato alla gestione      cal
056300050214     C**************************************************************************
056400060315     C     CARiniz       BEGSR
056500060330     c     Ktab          Klist
056600060330     c                   Kfld                    codut             1 0
056700060330     c                   Kfld                    kCod
056800060330     c                   Kfld                    kKey
056900110503     c     ktnpsc        klist
057000110503     c                   kfld                    kpsc
057100110503     c                   kfld                    kdde
057200060331     c
057300060331     c                   z-add     1             codut
057400060327     c
057500060315     c                   clear                   dute01
057600060315      *
057700060315     c     *dtaara       define    §azute        azuteds
057800060315     c     *dtaara       define    §datiute      ddatiute
057900060315     c                   in(E)     *dtaara
058000060315    1c                   If        %error  or RSUT = *blanks
058100060315     c                   Clear                   Tibs34ds
058200060315     c                   Call      'TIBS34R'
058300060315     c                   Parm                    Tibs34ds
058400060315     c                   In        *dtaara
058500060315    1c                   EndIf
058600060404    cc                   movel     rsut          v1crsu
058700060315
058800060315     c                   Clear                   wabi
058900060315     c                   Clear                   dp0msg
059000060315     c                   Clear                   dLat
059100060315
059200060315     c* Vado avanti nei caricamenti non in interrogazione
059300060403    0c                   if        dp0op1<>'O05'
059400060315     c
059500060315      * Verifica errori e autorità profilo
059600060315s   1c                   Select
059700060315      * se ho errori nei dati utente esco dal pgm
059800060315w   1c                   When      DutErr = 'E'
059900060315     c                   GoTo      ENDINIZ
060000060315      * se non c'è l'abilitazione
060100060315      * --> se 1° livello, abilitazioni al terminal
060200060315      *     se 2° livello, abilitazioni al punto operativo
060300060315      *     se sede è impossibile ma se capita mando a fine il pgm
060400060315w   1c                   When      UteAut = *Blanks
060500060315if  2c                   If        DutLpo = '1'
060600060315     c                   Eval      wabi   = 'TP'
060700060315e   2c                   EndIf
060800060315if  2c                   If        DutLpo = '2'
060900060315     c                   Eval      wabi   = 'PO'
061000060315e   2c                   EndIf
061100060315if  2c                   If        DutLpo = 'S'
061200060315     c                   GoTo      ENDINIZ
061300060315e   2c                   EndIf
061400060315      * carica le abilitazioni del profilo
061500060315x   1c                   Other
061600060315     c                   Movel     UteFaf        Dute01
061700060315     c                   Eval      wabi = UteAut
061800060315e   1c                   EndSl
061900060315
062000060315      * controllo se ok l'abilitazione dell'utente
062100060315     c                   Clear                   Tibs02ds
062200060315     c                   Eval      T02Mod = 'C'
062300060315     c                   Eval      T02Sif = knsif
062400060315     c                   Eval      T02Cod = 'LAT'
062500060315     c                   Movel(p)  wabi          T02Ke1
062600060315     c                   Call      'TIBS02R'
062700060315     c                   Parm                    kpjba
062800060315     c                   Parm                    Tibs02ds
062900060315if  1c                   If        T02Err = *Blanks
063000060315     c                   Eval      dLat = T02Uni
063100060315e   1c                   EndIf
063200060315     c
063300060315      * errore o non abilitato
063400060315if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
063500060315     C                   movel     msg(1)        dp0msg
063600060315     c                   eval      dp0err='1'
063700060315    1c                   endif
063800060315    0c                   endif
063900060315     c
064000060315     c* Data corrente
064100060315     c                   time                    w0140            14 0
064200060315     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
064300060315     c     *eur          movel     w0080         dataeur
064400060315     c                   movel     dataeur       dataiso
064500060315     c     *iso          movel     dataiso       datcor            8 0
064600060315     c
064700060315     C*
064800060315     C* VEDO QUALE OPZINE SCELTA
064900060315     c*
065000110503     C                   CLEAR                   V1Cpsc
065100110503     C                   CLEAR                   V1cnot
065200110503     C                   CLEAR                   V1Cdde
065300110503     C                   CLEAR                   V1Cdsc
065400110504     C                   CLEAR                   V1hdde
065500110504     C                   CLEAR                   V1hdsc
065600110503     C                   CLEAR                   V1cind
065700110503     C                   CLEAR                   V1cloc
065800060315     C                   CLEAR                   V1cprv
065900060315     C                   CLEAR                   V1ccap
066000110503     C                   CLEAR                   V1cnar
066100110503     C                   CLEAR                   v1dnar
066200060315     c                   clear                   aaa               1
066300060315     c                   clear                   aab               1
066400060315     c                   clear                   aad               1
066500060331     c                   clear                   wfor1             1
066600060331     c                   clear                   wfor2             1
066700060331     c                   clear                   wfor3             1
066800060331     c                   clear                   varcap
066900060331     c                   clear                   varloc
067000060315     c                   setoff                                       404142
067100060315     c                   setoff                                       434445
067200060315     c                   setoff                                       464748
067300060315     c                   setoff                                       49
067400060315     c                   setoff                                       010203
067500060315     c                   setoff                                       0405
067600060315     c* Imposto i campi
067700110503     C                   MOVEL     DP0psc        V1Cpsc
067800110503     C                   MOVEL     DP0not        V1Cnot
067900110503     C                   MOVEL     DP0NAR        V1CNAR
068000110503     C                   MOVEL     DP0ind        V1Cind
068100110503     C                   MOVEL     DP0not        V1Cnot
068200110503     C                   MOVEL     DP0LOC        V1CLOC
068300060315     C                   MOVEL     DP0CAP        V1CCAP
068400060315     C                   MOVEL     DP0PRV        V1CPRV
068500060404     c
068600110505     C**                 IF        DP0DRI>0
068700110505     C**   *ISO          MOVEL     DP0DRI        DATAISO
068800110505     C**                 MOVEL     DATAISO       DATAEUR
068900110505     C**   *EUR          MOVEL     DATAEUR       V1Cdde
069000110505     c**                 eval      v1hdde=dp0dri
069100110505     C**                 ENDIF
069200110503     C                   IF        DP0Dsc>0
069300110503     C     *ISO          MOVEL     DP0Dsc        DATAISO
069400110503     C                   MOVEL     DATAISO       DATAEUR
069500110503     C     *EUR          MOVEL     DATAEUR       V1Cdsc
069600110504     c                   eval      v1hdsc=dp0dsc
069700110503     C                   ENDIF
069800060315     C*
069900110503     c                   if        dp0nar<>*blanks
070000060315     c                   eval      kcod='15'
070100060315     c                   movel(p)  dp0nar        kkey
070200060315     c     ktab          chain     tabel
070300060327     c                   if        %found(tabel00f)
070400060315     c                   movel     tbluni        v1dnar
070500060315     c                   endif
070600110503     c                   endif
070700060315     c* IMMISSIONE
070800060315     c                   select
070900060315     C                   WHEN      DP0OP1='F06'
071000060315     C                   SETON                                        02
071100060315     C* MANUTENZIONE
071200060315     C                   WHEN      DP0OP1='O02'
071300060315     C                   SETON                                        04
071400060315     C* ANNULLAMENTO
071500060315     C                   WHEN      DP0OP1='O04'
071600060315     C                   SETON                                        0301
071700060315     C* VISULIZZA
071800060315     C                   WHEN      DP0OP1='O05'
071900060315     C                   SETON                                        0501
072000060315     C                   ENDSL
072100060315     C*
072200060315     C
072300060315     C     ENDINIZ       ENDSR
072400060327     C**************************************************************************
072500060327     C*  Aggiornamento dati su azcep
072600060327     C**************************************************************************
072700060327     C     Aggiorna      BEGSR
072800060327     c* Annullamento
072900060327     c                   select
073000060328     c                   when      dp0op1='O04'
073100110505     c***                eval      kdde=v1hdde
073200110505     c                   eval      kpsc=v1cpsc
073300110505     c     kpsc          delete    tnpsc01l
073400110504     c
073500060327     c                   if        %error
073600060327     c                   eval      dp0msg=msg(7)
073700060327     c                   eval      dp0err='1'
073800060327     c                   endif
073900060327     c
074000060328     c                   when      dp0op1='O02'
074100110504     c*
074200110505     c****               eval      kdde=v1hdde
074300110505     c                   eval      kpsc=v1cpsc
074400110505     c     kpsc          chain(e)  tnpsc01l
074500060327     c                   if        %error
074600060327     c                   eval      dp0msg=msg(8)
074700060327     c                   eval      dp0err='1'
074800060327     c                   else
074900060327     c                   exsr      Scrivi
075000060327     c                   endif
075100060327     c
075200060330     c* Duplica
075300060330     c                   when      dp0op1='O03'
075400060404     c                   exsr      SCRIVI
075500060330     c* Inserimento unico
075600110503     c                   when      dp0op1='F06'
075700060327     c                   exsr      SCRIVI
075800110503     c                   endsl
075900060327     c
076000060327     c                   ENDSR
076100060327     C**************************************************************************
076200060327     C*  Scrittura nuovo record
076300060327     C**************************************************************************
076400060327     C     Scrivi        BEGSR
076500110503     c                   clear                   tnpsc000
076600110504     c*
076700110505     c*****              eval      pscdde=amgdde
076800110503     c                   eval      pscdsc=amgdsc
076900110503     c
077000110503     c                   if        dp0op1='O03'
077100110503     c                   eval      pscpsc=vvcpsc
077200110503     c                   eval      pscind=dp0ind
077300110503     c                   eval      pscnar=dp0nar
077400110503     c                   eval      pscloc=dp0loc
077500110503     c                   eval      pscprv=dp0prv
077600110503     c                   eval      psccap=dp0cap
077700110503     c                   eval      pscnot=dp0not
077800060330     c                   else
077900110503     c                   eval      pscpsc=v1cpsc
078000110503     c                   eval      pscind=v1cind
078100110503     c                   eval      pscnar=v1cnar
078200110503     c                   eval      pscloc=v1cloc
078300110503     c                   eval      pscprv=v1cprv
078400110503     c                   eval      psccap=v1ccap
078500110504     c                   eval      pscnot=v1cnot
078600060330     c                   endif
078700110504     c                   if        dp0op1='F06'  or dp0op1='O03'
078800110503     c                   write     tnpsc000
078900110504     c                   else
079000110504     c                   update    tnpsc000
079100110504     c                   endif
079200110504     c
079300060327     c                   endsr
079400020805      **************************************************************************
079500960302** SCHIERA MSG - MESSAGGI DI ERRORE
079600110506Utente non abilitato                                                           01
079700110503Codice punto di scambio obbligatorio                                           02
079800110503Data errata                                                                    03
079900060328Annullamento non effettuabile: data già trascorsa                              04
080000110503Indirizzo obbligatorio                                                         05
080100110503Località obbligatoria anche parziale senza cap e provincia per attivare RICERCA06
080200060328Record al momento non annullabile: in uso!!!! Verificare e riprovare           07
080300060328Record al momento non modificabile: in uso!!!! Verificare e riprovare          08
080400110505Data scadenza deve essere MAGGIORE o UGUALE alla data decorrenza               09
080500110504Il punto di scambio inserito si sovrappone ad un altro con stesso codice       10
080600110505Codice punto di scambio già esistente                                          11
080700110506Data scadenza deve essere MAGGIORE  alla data corrente
080800110506Punto di scambio non annullabile: presente nell'anagrafica traini              13
