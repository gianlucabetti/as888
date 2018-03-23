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
004700130320     D Dpscflo       E DS
004800040917     C**********************************************************************
004900060315     c* INDICATORI
005000060315     C**********************************************************************
005100060315     C* 01    - PROTEZIONE CAMPI VIDEATA
005200060315     C* 02    - IMMISSIONE
005300060315     C* 03    - ANNULLAMENTO
005400060315     C* 04    - MODIFICA
005500060315     C* 05    - INTERROGAZIONE
005600060404     C* 06    - evidenzia campo di evento duplicabile  <> da STD
005700050103     C**********************************************************************
005800040917     C     *ENTRY        PLIST
005900040917     C                   PARM                    KPJBA
006000110503     C                   PARM                    tntlp0ds
006100060315     c
006200060315     c* Verifico se l'utente  abilitato alla gestione del calendario
006300060315     c                   exsr      CARINIZ
006400060315     c                   if        dp0err='1'
006500060315     c                   goto      fine
006600060315     c                   endif
006700060329     c
006800060329     c* Duplica
006900060329     c                   if        dp0op1='O03'
007000060329     c                   exsr      Duplica
007100060329     c                   goto      fine
007200060329     c                   endif
007300040917     c
007400040917     C     FOR01         TAG
007500000828      *
007600110503     C                   EXFMT     tlp1d01
007700060315     C                   SETOFF                                       2890
007800060315     c                   setoff                                       404142
007900060315     c                   setoff                                       434445
008000060315     c                   setoff                                       464748
008100060330     c                   setoff                                       495152
008200060330     c                   setoff                                       535455
008300060330     c                   setoff                                       565758
008400130320     c                   setoff                                       596061
008500040917     C* F3 - FINE
008600060315     C                   IF        *INKC
008700060315     C                   EVAL      DP0F03='1'
008800060315     C                   GOTO      FINE
008900060315     C                   ENDIF
009000060404     c
009100060315     C* F12- RITORNO
009200060315     C                   IF        *INKl
009300060315     C                   EVAL      DP0F12='1'
009400060315     C                   GOTO      FINE
009500060315     C                   ENDIF
009600060315     c
009700060315      *  CONTROLLI --> non in interrogazione
009800040917     C                   EXSR      CTRD01
009900040917     c   90              goto      for01
010000000825      *
010100060315     c* Per visualizzazione, enter esce
010200060315     c                   if        dp0op1='O05'
010300060315     c                   goto      fine
010400060315     c                   endif
010500060315     c
010600060315     c  nkf              goto      for01
010700060327     c* F06 - conferma
010800060327     c   kf              exsr      AGGIORNA
010900040917     C*
011000900515      *
011100000000     C     FINE          TAG
011200060315     C**
011300060315     C                   CLEAR                   tibs02ds
011400060315     C                   MOVEL     'C'           T02TLA
011500060315     C                   CALL      'TIBS02R'
011600060315     C                   PARM                    KPJBA
011700060315     C                   PARM                    tibs02ds
011800060315     C**
011900060315     c                   clear                   dslv13
012000060315     C                   MOVEL     'C'           I13TLA
012100060315     C                   CALL      'FNLV13R'
012200060315     C                   PARM                    KPJBA
012300060315     C                   PARM                    DSLV13
012400060315     C                   PARM                    DSSI95
012500000000     C                   SETON                                        LR
012600060329      *-----------------------------------------------------***********
012700060329      * Duplica
012800060329      *-----------------------------------------------------***********
012900060329     C     Duplica       BEGSR
013000060329     c
013100110503     C                   write     tlp1d01
013200060329     c     forw2         tag
013300110503     C                   EXFMT     tlp1dv2
013400060330     c                   setoff                                       2890
013500060330     c                   setoff                                       4041
013600060329     C* F12- RITORNO
013700060329     C                   IF        *INKl
013800060329     C                   EVAL      DP0F12='1'
013900060329     C                   GOTO      Endduplica
014000060329     C                   ENDIF
014100060329     c* controlli video 2
014200060329     c                   exsr      ctrw2
014300060329     c
014400060330     c  Nkf
014500060330     cor 90              goto      forw2
014600060330     c
014700060330     c   KF              exsr      Aggiorna
014800060329     c
014900060329     c     endduplica    endsr
015000900515      *-----------------------------------------------------***********
015100060329      * CONTROLLI                                           *  CTRD01 *
015200900515      *-----------------------------------------------------***********
015300040917     C     CTRD01        BEGSR
015400060330     c
015500110503     c                   if        v1cpsc=*blanks
015600110503     c                   seton                                        409028
015700110503     C                   MOVEL     MSG(2)        V1cMSG
015800110503     c                   goto      endct1
015900110503    1c                   endif
016000110505     c*
016100110505     c* Deve essere univoco nel file
016200110505     c                   if        dp0op1='F06'  or dp0op1='O03'
016300110505     c     v1cpsc        setll     tnpsc01l
016400110505     c                   if        %equal(tnpsc01l)
016500110505     c                   seton                                        409028
016600110505     C                   MOVEL     MSG(11)       V1cMSG
016700110505     c                   goto      endct1
016800110505    1c                   endif
016900110505    1c                   endif
017000060315     c*
017100110505     c
017200110503     c* Data decorrenza obbligatoria
017300110505     c**                 eval      wdat=v1cdde
017400110505     c**                 exsr      CTRDEV
017500110505     c**                 eval      v1cdde=wdat
017600110503
017700110505     c**                 if        *in90
017800110505     c**                 seton                                        41
017900110505     C**                 MOVEL     MSG(3)        V1cMSG
018000110505     c**                 goto      endct1
018100110505    1c**                 endif
018200110505     c**                 eval      amgdde=amgdat
018300110503     c
018400110505     c* Data scadenza
018500110505     c                   clear                   amgdsc
018600110505     c                   if        v1cdsc>0
018700110503     c                   eval      wdat=v1cdsc
018800110503     c                   exsr      CTRDEV
018900110503     c                   eval      v1cdsc=wdat
019000110503
019100110503     c                   if        *in90
019200110503     c                   seton                                        42
019300110503     C                   MOVEL     MSG(3)        V1cMSG
019400110503     c                   goto      endct1
019500110503    1c                   endif
019600110503     c                   eval      amgdsc =amgdat
019700110505     c                   endif
019800110506     c
019900110506     c*  deve essere > della data corrente
020000110506     c                   if        amgdsc<datcor   and amgdsc>0
020100110506     c                   seton                                        422890
020200110506     C                   MOVEL     MSG(12)       V1cMSG
020300110506     c                   goto      endct1
020400110506    1c                   endif
020500110503     c
020600110505     c**                 if        amgdsc<amgdde
020700110505     c**                 seton                                        422890
020800110505     C**                 MOVEL     MSG(9)        V1cMSG
020900110505     c**                 goto      endct1
021000110505    1c**                 endif
021100110504     c
021200110504     c* controllo congruenza con latri dati già inseriti
021300110505     c**                 if        amgdsc<>v1hdsc   or
021400110505     c**                           amgdde<>v1hdde
021500110505     c**                 exsr      carSQL
021600110505     c**
021700110505     c** 90              goto      endct1
021800110505     c**                 endif
021900060330     c
022000041220     c*
022100110506     c* Annullamento effettuabile se punto non utilizzano in traini in decorrenza
022200110506    1c                   if        dp0op1='O04'
022300110506     c                   exsr      SqlPSC
022400110506     c
022500110506     c                   if        *in90
022600060315     c                   goto      endct1
022700060315    1c                   endif
022800110506    1c                   endif
022900110504     c
023000110504     c* indirizzo deve essere completo
023100110504     c                   if        v1cind=*blanks
023200110504     c                   seton                                        479028
023300110504     C                   MOVEL     MSG(5)        V1cMSG
023400110504     c                   goto      endct1
023500110504    1c                   endif
023600110504
023700110504     c                   if        v1cloc=*blanks
023800110504     c                   seton                                        459028
023900110504     C                   MOVEL     MSG(6)        V1cMSG
024000110504     c                   goto      endct1
024100110504    1c                   endif
024200060315     c
024300060315     c* Nazione/cap/località
024400060315    1c                   if        v1cnar<>*blanks or v1cloc<>*blanks
024500060328     c                             or v1ccap<>*blanks or v1cprv<>*blanks
024600060315     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
024700060315     C                   CLEAR                   DSLV13
024800060315     C                   CLEAR                   DSLV14
024900060315     C                   CLEAR                   DSSI95
025000060315     C*
025100060328     c                   if        v1cloc<>*blanks
025200060315     C                   CLEAR                   I14ISO
025300110505     c* se ancora da decorrere --> controllo alla data decorrenza
025400110503     c*  se decorrente --> a udate
025500110503     c*  se scaduto --> alla data scadenza
025600110505     c**                 select
025700110505     c**                 when      amgdde>datcor
025800110505     C**                 MOVEL     amgdde        I14dri
025900110505     c**                 when      amgdde<=datcor and amgdsc>=datcor
026000110505     C**                 MOVEL     datcor        I14dri
026100110505     c**                 other
026200110505     C**                 MOVEL     amgdsc        I14dri
026300110505     c**                 endsl
026400110505     c* Se c'è la data di riferimento la passo, altrimenti data corrente
026500110505     c                   if        dp0dri>0
026600110505     C                   MOVEL     dp0dri        I14dri
026700110505     c                   else
026800110505     C                   MOVEL     datcor        I14dri
026900110505     c                   endif
027000060315     c                   movel     'S'           i14cnz
027100060315     c                   movel     'XX'          i14rsc
027200110503     c                   movel     v1cind        i14ind
027300060315     c                   movel     v1cnar        e14nar
027400060315     c                   movel     v1ccap        e14cap
027500060315     c                   movel     v1cloc        e14loc
027600060315     c                   movel     v1cprv        e14prv
027700060315     C                   CALL      'FNLV14R'
027800060315     C                   PARM                    KPJBA
027900060315     C                   PARM                    DSLV14
028000060329     C* CAMPI CHE POSSO AVER IMPOSTATO
028100060329     C                   MOVEL     E14LOC        v1cloc
028200060329     C                   MOVEL     E14PRV        V1CPRv
028300060329     C                   MOVEL     E14NAR        V1Cnar
028400060329     C                   MOVEL     E14CAP        V1Ccap
028500060315     C*
028600060315     C                   MOVEL     O14ERR        WERR              1
028700060315     C                   MOVEL     O14MSG        V1cMSG
028800060315     C**
028900060315    2C     WERR          IFNE      ' '
029000060315    3C     V1cMSG        IFNE      *BLANKS
029100060315     C                   SETON                                        28
029200060315    3c                   endif
029300060315     c                   seton                                        9044
029400060315     c                   goto      endct1
029500060315    2c                   endif
029600060328    2c                   endif
029700060315     C* CONTROLLO CAP -
029800060329     c                   if        v1cloc=*blanks
029900060328     C                   MOVEL     '3'           I95TCN
030000060328     c                   else
030100060328     C                   MOVEL     '7'           I95TCN
030200060329     C                   MOVEL     'S'           I13AF1
030300060329     C                   MOVEL     'S'           I13SZ2
0304000603299/12 C                   MOVEL     'S'           I13LA3
0305000603299/12 C                   MOVEL     'S'           I13SZ3
030600060328     c                   endif
030700060331     C                   MOVEL     v1ccAP        I95CAP
030800060331     C                   MOVEL     v1cLOC        I95LOC
030900060331     C                   MOVEL     v1cPRV        I95PRV
031000060331     C                   MOVEL     v1cNAR        I95NAR
031100110503     c
031200060315     C                   MOVEL     'S'           I13AF0
031300061010     C                   MOVEL     ' '           I13CNV
031400060315     C                   MOVEL     'S'           I13SZV
031500060315c
031600060331    2C     v1cCAP        IFNE      VARcap
031700060331     C                   CLEAR                   wfor1
031800060315     C                   CLEAR                   AAA
031900060315     C                   CLEAR                   AAB
032000060315     C                   CLEAR                   AAD
032100060331     C                   MOVEL     v1ccap        VARCAp
032200060315    2C                   ENDIF
032300060315    c
032400060315     C* MODIFICA LA LOCALITA'
032500060331    2C     v1cLOC        IFNE      VARLOc
032600060315     C                   CLEAR                   AAA
032700060315     C                   CLEAR                   AAB
032800060315     C                   CLEAR                   AAD
032900060331     C                   MOVEL     v1cloc        VARLOc
033000060315    2C                   ENDIF
033100060315     C**
033200060331     C                   MOVEL     wfor1         E13FZV
033300060315     C                   MOVEL     AAA           E13FZ1
033400060315     C                   MOVEL     AAB           E13FZ2
033500060315     C                   MOVEL     AAD           E13FZ3
033600060315     C**
033700060315     C** CALL
033800060315     C                   CALL      'FNLV13R'
033900060315     C                   PARM                    KPJBA
034000060315     C                   PARM                    DSLV13
034100060315     C                   PARM                    DSSI95
034200060315     C*
034300060315     C                   MOVEL     O13ERR        WERR              1
034400060315     C                   MOVEL     O13MSG        v1cMSG
034500060315     C                   EXSR      CTRERR
034600060315     C**
034700060315     C* REIMPOSTO LE FORZATURE
034800060331     C                   MOVEL     E13FZV        wfor1
034900060315     C                   MOVEL     E13FZ1        AAA
035000060315     C                   MOVEL     E13FZ2        AAB
035100060315     C                   MOVEL     E13FZ3        AAD
035200060315     C**
035300060315    1C     WERR          IFNE      ' '
035400060315    2C     V1cMSG        IFNE      *BLANKS
035500060315     C                   SETON                                        28
035600060315     c                   endif
035700060315     c                   seton                                        9044
035800060315     c                   goto      endct1
035900060315     c                   endif
036000060315     c                   endif
036100130320     c* controllo terminal se immesso
036200130320     c                   clear                   v1dflr
036300130320     c                   if        v1cflr>0
036400130320     c     v1cflr        chain     azorg01l
036500130320     c                   if        not %found(azorg01l) or orgfva<>' '
036600130320     c                   seton                                        619028
036700130320     C                   MOVEL     MSG(5)        V1cMSG
036800130320     c                   goto      endct1
036900130320     c                   else
037000130320     c                   movel     orgdes        v1dflr
037100130320     c                   endif
037200130320     c                   endif
037300060327     c
037400040917     C     ENDCT1        ENDSR
037500060330      *-----------------------------------------------------***********
037600110503      * CONTROLLo Data
037700060330      *-----------------------------------------------------***********
037800060330     c     CTRDEV        BEGSR
037900110503    1c                   if        wdat>0
038000110503     c                   movel     wdat          w002a             2
038100110504     c     *eur          test(d)                 wdat                   30
038200060331    2c                   if        *in30  and w002a='00'
038300110504     c     *dmy          test(d)                 wdat                   30
038400060331     c                   endif
038500060331     c* Errore
038600060330    3c                   if        *in30
038700110503     c                   seton                                        9028
038800060330     c                   goto      endctdev
038900060331     c                   endif
039000060330     c
039100060331     c                   if        w002a='00'
039200110503     c     *dmy          move      wdat          dataeur
039300060330     c                   else
039400110503     c     *eur          move      wdat          dataeur
039500060330     c                   endif
039600060330     c
039700060330     c                   else
039800110503     c                   seton                                        9028
039900060330     c                   goto      endctdev
040000060330     c                   endif
040100060330     c* la trasformo in AAAAAMMGG - iso
040200110503     c                   move      dataeur       wdat
040300060330     c                   movel     dataeur       dataiso
040400110503     c     *iso          movel     dataiso       amgdat            8 0
040500060331     c
040600060330     c
040700060330     c     endctdev      ENDSR
040800110504      *-----------------------------------------------------***********
040900110504      * con SQL controllo decorrenza / scadenza record
041000110504      *-----------------------------------------------------***********
041100110504     c     carSQL        BEGSR
041200110504     c                   eval      wrkgetlista =
041300110504     c                             'select pscdde, pscdsc  -
041400110504     c                               from tnpsc00f where pscpsc= ''' +
041500110505     c                               v1cpsc + ''' and pscatb='' '' and +
041600110505     c                               pscdde<>' + %editc(v1hdde:'X')
041700110504
041800110504     C/EXEC SQL
041900110504     C+ PREPARE s1 FROM :wrkgetlista
042000110504     C/END-EXEC
042100110504     C
042200110504     C/EXEC SQL
042300110504     C+ DECLARE A1 CURSOR FOR S1
042400110504     C/END-EXEC
042500110504     C
042600110504     C/EXEC SQL
042700110504     C+ OPEN a1
042800110504     C/END-EXEC
042900110504
043000110504      *?Leggo il file
043100110504     c                   do        *hival
043200110504     C/EXEC SQL
043300110504     C+ FETCH NEXT FROM a1 INTO : wdde, :wdsc
043400110504     C/END-EXEC
043500110504
043600110504     c                   select
043700110504
043800110504     c                   when      sqlcod = 100
043900110504     c                   leave
044000110504     c                   when      sqlcod < 0
044100110504     c                   seton                                        H1
044200110504     c                   leave
044300110504     c                   other
044400110504     c
044500110504     c                   if        wdsc<amgdde   or wdde>amgdsc
044600110504     c                   else
044700110504     c* errore
044800110504     c                   seton                                        412890
044900110504     c                   eval      v1cmsg=msg(10)
045000110504     c                   leave
045100110504     c                   endif
045200110504     c
045300110504     c                   endsl
045400110504
045500110504     c                   enddo
045600110504
045700110504     C/EXEC SQL
045800110504     C+ CLOSE a1
045900110504     C/END-EXEC
046000110504
046100110504     c                   ENDSR
046200110506      *-----------------------------------------------------***********
046300110506      * controllo se punto di scambio presente nei traini
046400110506      *-----------------------------------------------------***********
046500110506     c     SQLpsc        BEGSR
046600110506     c                   eval      wrkgetlista =
046700110506     c                             'select TLSDDE  -
046800110506     c                               from tntls00f where tlspsc= ''' +
046900110506     c                               v1cpsc + ''' and tlsatb='' '''
047000110506
047100110506     C/EXEC SQL
047200110506     C+ PREPARE s2 FROM :wrkgetlista
047300110506     C/END-EXEC
047400110506     C
047500110506     C/EXEC SQL
047600110506     C+ DECLARE A2 CURSOR FOR S2
047700110506     C/END-EXEC
047800110506     C
047900110506     C/EXEC SQL
048000110506     C+ OPEN a2
048100110506     C/END-EXEC
048200110506
048300110506      *?Leggo il file
048400110506     C/EXEC SQL
048500110506     C+ FETCH NEXT FROM a2 INTO : wdde
048600110506     C/END-EXEC
048700110506
048800110506     c                   select
048900110506
049000110506     c                   when      sqlcod = 100
049100110506     c                   when      sqlcod < 0
049200110506     c                   seton                                        H2
049300110506     c                   other
049400110506     c* errore
049500110506     c                   seton                                        2890
049600110506     c                   eval      v1cmsg=msg(13)
049700110506     c
049800110506     c                   endsl
049900110506
050000110506     C/EXEC SQL
050100110506     C+ CLOSE a2
050200110506     C/END-EXEC
050300110506
050400110506     c                   ENDSR
050500060329      *-----------------------------------------------------***********
050600060329      * CONTROLLI x deplica
050700060329      *-----------------------------------------------------***********
050800060329     C     CTRw2         BEGSR
050900110503     c*   nuovo codice obbligaotiro
051000110503     c                   if        vvcpsc=*blanks
051100110503     c                   seton                                        409028
051200110503     C                   MOVEL     MSG(2)        V1cMSG
051300110503     c                   goto      endct2
051400110503    1c                   endif
051500110505     c*
051600110505     c* Deve essere univoco nel file
051700110505     c     vvcpsc        setll     tnpsc01l
051800110505     c                   if        %equal(tnpsc01l)
051900110505     c                   seton                                        409028
052000110505     C                   MOVEL     MSG(11)       V1cMSG
052100110505     c                   goto      endct2
052200110505    1c                   endif
052300060330     c*
052400110503     c* Data decorr  obbligatoria
052500110505     c**                 eval      wdat=vvcdde
052600110505     c**                 exsr      CTRDEV
052700110505     c**                 eval      vvcdde=wdat
052800110505     c**                 if        *in90
052900110505     c**                 seton                                        41
053000110505     C**                 MOVEL     MSG(3)        V1cMSG
053100110505     c**                 goto      endct2
053200110505    1c**                 endif
053300110505     c**                 eval      amgdde =amgdat
053400060330     c
053500110505     c* Data scadenza
053600110505     c                   clear                   amgdsc
053700110505     c                   if        vvcdsc>0
053800110503     c                   eval      wdat=vvcdsc
053900110503     c                   exsr      CTRDEV
054000110503     c                   eval      vvcdsc=wdat
054100110503     c                   if        *in90
054200110503     c                   seton                                        42
054300110503     C                   MOVEL     MSG(3)        V1cMSG
054400110503     c                   goto      endct2
054500110503    1c                   endif
054600130404     c                   eval      amgdsc =amgDAT
054700110505     c                   endif
054800110503     c
054900110505     c**                 if        amgdsc<amgdde
055000110505     c**                 seton                                        42
055100110505     C**                 MOVEL     MSG(9)        V1cMSG
055200110505     c**                 goto      endct2
055300110505    1c**                 endif
055400110503     c
055500060329     C     ENDCT2        ENDSR
055600060315     C*
055700060315     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
055800060315     C     CTRERR        BEGSR
055900060315     C* IMPOSTO CAMPI PASSATI
056000060315     C     O13RON        IFEQ      'S'
056100060315     C                   MOVEL     O95NAR        v1cNAR
056200060315     C                   ENDIF
056300060315     C     O13ROC        IFEQ      'S'
056400060315     C                   MOVEL     O95CAP        v1cCAP
056500060315     C                   ENDIF
056600060315     C     O13ROP        IFEQ      'S'
056700060315     C                   MOVEL     O95PRV        v1cPRV
056800060315     C                   ENDIF
056900060315     C     O13ROL        IFEQ      'S'
057000060315     C                   MOVEL     O95LOC        v1cLOC
057100060315     C                   ENDIF
057200060315     c
057300060315    2C                   ENDSR
057400050214     C**************************************************************************
057500060315     C*  Carico dati utente per vedere se abilitato alla gestione      cal
057600050214     C**************************************************************************
057700060315     C     CARiniz       BEGSR
057800060330     c     Ktab          Klist
057900060330     c                   Kfld                    codut             1 0
058000060330     c                   Kfld                    kCod
058100060330     c                   Kfld                    kKey
058200110503     c     ktnpsc        klist
058300110503     c                   kfld                    kpsc
058400110503     c                   kfld                    kdde
058500060331     c
058600060331     c                   z-add     1             codut
058700060327     c
058800060315     c                   clear                   dute01
058900060315      *
059000060315     c     *dtaara       define    §azute        azuteds
059100060315     c     *dtaara       define    §datiute      ddatiute
059200060315     c                   in(E)     *dtaara
059300060315    1c                   If        %error  or RSUT = *blanks
059400060315     c                   Clear                   Tibs34ds
059500060315     c                   Call      'TIBS34R'
059600060315     c                   Parm                    Tibs34ds
059700060315     c                   In        *dtaara
059800060315    1c                   EndIf
059900060404    cc                   movel     rsut          v1crsu
060000060315
060100060315     c                   Clear                   wabi
060200060315     c                   Clear                   dp0msg
060300060315     c                   Clear                   dLat
060400060315
060500060315     c* Vado avanti nei caricamenti non in interrogazione
060600060403    0c                   if        dp0op1<>'O05'
060700060315     c
060800060315      * Verifica errori e autorità profilo
060900060315s   1c                   Select
061000060315      * se ho errori nei dati utente esco dal pgm
061100060315w   1c                   When      DutErr = 'E'
061200060315     c                   GoTo      ENDINIZ
061300060315      * se non c'è l'abilitazione
061400060315      * --> se 1° livello, abilitazioni al terminal
061500060315      *     se 2° livello, abilitazioni al punto operativo
061600060315      *     se sede è impossibile ma se capita mando a fine il pgm
061700060315w   1c                   When      UteAut = *Blanks
061800060315if  2c                   If        DutLpo = '1'
061900060315     c                   Eval      wabi   = 'TP'
062000060315e   2c                   EndIf
062100060315if  2c                   If        DutLpo = '2'
062200060315     c                   Eval      wabi   = 'PO'
062300060315e   2c                   EndIf
062400060315if  2c                   If        DutLpo = 'S'
062500060315     c                   GoTo      ENDINIZ
062600060315e   2c                   EndIf
062700060315      * carica le abilitazioni del profilo
062800060315x   1c                   Other
062900060315     c                   Movel     UteFaf        Dute01
063000060315     c                   Eval      wabi = UteAut
063100060315e   1c                   EndSl
063200060315
063300060315      * controllo se ok l'abilitazione dell'utente
063400060315     c                   Clear                   Tibs02ds
063500060315     c                   Eval      T02Mod = 'C'
063600060315     c                   Eval      T02Sif = knsif
063700060315     c                   Eval      T02Cod = 'LAT'
063800060315     c                   Movel(p)  wabi          T02Ke1
063900060315     c                   Call      'TIBS02R'
064000060315     c                   Parm                    kpjba
064100060315     c                   Parm                    Tibs02ds
064200060315if  1c                   If        T02Err = *Blanks
064300060315     c                   Eval      dLat = T02Uni
064400060315e   1c                   EndIf
064500060315     c
064600060315      * errore o non abilitato
064700060315if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
064800060315     C                   movel     msg(1)        dp0msg
064900060315     c                   eval      dp0err='1'
065000060315    1c                   endif
065100060315    0c                   endif
065200060315     c
065300060315     c* Data corrente
065400060315     c                   time                    w0140            14 0
065500060315     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
065600060315     c     *eur          movel     w0080         dataeur
065700060315     c                   movel     dataeur       dataiso
065800060315     c     *iso          movel     dataiso       datcor            8 0
065900060315     c
066000060315     C*
066100060315     C* VEDO QUALE OPZINE SCELTA
066200060315     c*
066300110503     C                   CLEAR                   V1Cpsc
066400110503     C                   CLEAR                   V1cnot
066500110503     C                   CLEAR                   V1Cdde
066600110503     C                   CLEAR                   V1Cdsc
066700110504     C                   CLEAR                   V1hdde
066800110504     C                   CLEAR                   V1hdsc
066900110503     C                   CLEAR                   V1cind
067000110503     C                   CLEAR                   V1cloc
067100060315     C                   CLEAR                   V1cprv
067200060315     C                   CLEAR                   V1ccap
067300110503     C                   CLEAR                   V1cnar
067400110503     C                   CLEAR                   v1dnar
067500130320     C                   CLEAR                   v1cflr
067600130320     C                   CLEAR                   v1dflr
067700060315     c                   clear                   aaa               1
067800060315     c                   clear                   aab               1
067900060315     c                   clear                   aad               1
068000060331     c                   clear                   wfor1             1
068100060331     c                   clear                   wfor2             1
068200060331     c                   clear                   wfor3             1
068300060331     c                   clear                   varcap
068400060331     c                   clear                   varloc
068500060315     c                   setoff                                       404142
068600060315     c                   setoff                                       434445
068700060315     c                   setoff                                       464748
068800060315     c                   setoff                                       49
068900060315     c                   setoff                                       010203
069000060315     c                   setoff                                       0405
069100060315     c* Imposto i campi
069200110503     C                   MOVEL     DP0psc        V1Cpsc
069300110503     C                   MOVEL     DP0not        V1Cnot
069400110503     C                   MOVEL     DP0NAR        V1CNAR
069500110503     C                   MOVEL     DP0ind        V1Cind
069600110503     C                   MOVEL     DP0not        V1Cnot
069700110503     C                   MOVEL     DP0LOC        V1CLOC
069800060315     C                   MOVEL     DP0CAP        V1CCAP
069900060315     C                   MOVEL     DP0PRV        V1CPRV
070000130320     c                   eval      dpscflo=dp0flo
070100130320     c                   if        §pscflr>*zeros
070200130320     c                   movel     §pscflr       v1cflr
070300130320     c     v1cflr        chain     azorg01l
070400130320     c                   if        %found(azorg01l)
070500130320     c                   movel     orgdes        v1dflr
070600130320     c                   endif
070700130320     c
070800130320     c                   endif
070900060404     c
071000110505     C**                 IF        DP0DRI>0
071100110505     C**   *ISO          MOVEL     DP0DRI        DATAISO
071200110505     C**                 MOVEL     DATAISO       DATAEUR
071300110505     C**   *EUR          MOVEL     DATAEUR       V1Cdde
071400110505     c**                 eval      v1hdde=dp0dri
071500110505     C**                 ENDIF
071600110503     C                   IF        DP0Dsc>0
071700110503     C     *ISO          MOVEL     DP0Dsc        DATAISO
071800110503     C                   MOVEL     DATAISO       DATAEUR
071900110503     C     *EUR          MOVEL     DATAEUR       V1Cdsc
072000110504     c                   eval      v1hdsc=dp0dsc
072100110503     C                   ENDIF
072200060315     C*
072300110503     c                   if        dp0nar<>*blanks
072400060315     c                   eval      kcod='15'
072500060315     c                   movel(p)  dp0nar        kkey
072600060315     c     ktab          chain     tabel
072700060327     c                   if        %found(tabel00f)
072800060315     c                   movel     tbluni        v1dnar
072900060315     c                   endif
073000110503     c                   endif
073100060315     c* IMMISSIONE
073200060315     c                   select
073300060315     C                   WHEN      DP0OP1='F06'
073400060315     C                   SETON                                        02
073500060315     C* MANUTENZIONE
073600060315     C                   WHEN      DP0OP1='O02'
073700060315     C                   SETON                                        04
073800060315     C* ANNULLAMENTO
073900060315     C                   WHEN      DP0OP1='O04'
074000060315     C                   SETON                                        0301
074100060315     C* VISULIZZA
074200060315     C                   WHEN      DP0OP1='O05'
074300060315     C                   SETON                                        0501
074400060315     C                   ENDSL
074500060315     C*
074600060315     C
074700060315     C     ENDINIZ       ENDSR
074800060327     C**************************************************************************
074900060327     C*  Aggiornamento dati su azcep
075000060327     C**************************************************************************
075100060327     C     Aggiorna      BEGSR
075200060327     c* Annullamento
075300060327     c                   select
075400060328     c                   when      dp0op1='O04'
075500110505     c***                eval      kdde=v1hdde
075600110505     c                   eval      kpsc=v1cpsc
075700110505     c     kpsc          delete    tnpsc01l
075800110504     c
075900060327     c                   if        %error
076000060327     c                   eval      dp0msg=msg(7)
076100060327     c                   eval      dp0err='1'
076200060327     c                   endif
076300060327     c
076400060328     c                   when      dp0op1='O02'
076500110504     c*
076600110505     c****               eval      kdde=v1hdde
076700110505     c                   eval      kpsc=v1cpsc
076800110505     c     kpsc          chain(e)  tnpsc01l
076900060327     c                   if        %error
077000060327     c                   eval      dp0msg=msg(8)
077100060327     c                   eval      dp0err='1'
077200060327     c                   else
077300060327     c                   exsr      Scrivi
077400060327     c                   endif
077500060327     c
077600060330     c* Duplica
077700060330     c                   when      dp0op1='O03'
077800060404     c                   exsr      SCRIVI
077900060330     c* Inserimento unico
078000110503     c                   when      dp0op1='F06'
078100060327     c                   exsr      SCRIVI
078200110503     c                   endsl
078300060327     c
078400060327     c                   ENDSR
078500060327     C**************************************************************************
078600060327     C*  Scrittura nuovo record
078700060327     C**************************************************************************
078800060327     C     Scrivi        BEGSR
078900110503     c                   clear                   tnpsc000
079000110504     c*
079100110505     c*****              eval      pscdde=amgdde
079200110503     c                   eval      pscdsc=amgdsc
079300110503     c
079400110503     c                   if        dp0op1='O03'
079500110503     c                   eval      pscpsc=vvcpsc
079600110503     c                   eval      pscind=dp0ind
079700110503     c                   eval      pscnar=dp0nar
079800110503     c                   eval      pscloc=dp0loc
079900110503     c                   eval      pscprv=dp0prv
080000110503     c                   eval      psccap=dp0cap
080100110503     c                   eval      pscnot=dp0not
080200060330     c                   else
080300110503     c                   eval      pscpsc=v1cpsc
080400110503     c                   eval      pscind=v1cind
080500110503     c                   eval      pscnar=v1cnar
080600110503     c                   eval      pscloc=v1cloc
080700110503     c                   eval      pscprv=v1cprv
080800110503     c                   eval      psccap=v1ccap
080900110504     c                   eval      pscnot=v1cnot
081000130320     c                   if        v1cflr<=*zeros
081100130320     c                   clear                   pscflo
081200130320     c                   else
081300130320     c                   clear                   dpscflo
081400130320     c                   movel     v1cflr        §pscflr
081500130320     c                   eval      pscflo=Dpscflo
081600060330     c                   endif
081700130320     c
081800130320     c                   endif
081900130320     c
082000110504     c                   if        dp0op1='F06'  or dp0op1='O03'
082100110503     c                   write     tnpsc000
082200110504     c                   else
082300110504     c                   update    tnpsc000
082400110504     c                   endif
082500110504     c
082600060327     c                   endsr
082700020805      **************************************************************************
082800960302** SCHIERA MSG - MESSAGGI DI ERRORE
082900110506Utente non abilitato                                                           01
083000110503Codice punto di scambio obbligatorio                                           02
083100110503Data errata                                                                    03
083200060328Annullamento non effettuabile: data già trascorsa                              04
083300110503Indirizzo obbligatorio                                                         05
083400110503Località obbligatoria anche parziale senza cap e provincia per attivare RICERCA06
083500060328Record al momento non annullabile: in uso!!!! Verificare e riprovare           07
083600060328Record al momento non modificabile: in uso!!!! Verificare e riprovare          08
083700110505Data scadenza deve essere MAGGIORE o UGUALE alla data decorrenza               09
083800110504Il punto di scambio inserito si sovrappone ad un altro con stesso codice       10
083900110505Codice punto di scambio già esistente                                          11
084000110506Data scadenza deve essere MAGGIORE  alla data corrente
084100110506Punto di scambio non annullabile: presente nell'anagrafica traini              13
084200130320Filiale errata                                                                 14
