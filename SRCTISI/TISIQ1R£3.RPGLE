000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200060922      * TISIQ1R *----------------------------------------------------*
000300060922      *         - cappario utilizzi particolari
000400940915      *--------------------------------------------------------------*
000500060922     FTISIQ1d   CF   E             WORKSTN
000600080521     F***azcpp01l  uF a E           K DISK
000700080522     Fazcpp02l  uF a E           K DISK    infds(azcppinf)
000800080522     Fazcpp00F  uF   E             DISK    rename(azcpp000:azcppf)
000900060925     Fazcpc01l  iF   E           K DISK
001000060315     Ftabel00f  IF   E           K DISK
001100020805      *
001200020805      * DEFINIZIONE SCHIERE
001300960302     D*
001400080522     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001500060315     d
001600040917     D KPJBA         E DS
001700060315     d Tibs02Ds      e ds
001800060315     d Azuteds       e ds                  extname(Azute00f)
001900060315     d dDatiute      e ds
002000060315     d dute01        e ds
002100060315     D TIBS34DS      E DS
002200060925     D ds15          E DS
002300060922     d
002400050103     d Dataiso         s               d   datfmt(*iso)
002500050103     d Dataeur         s               d   datfmt(*eur)
002600050103     d
002700060315     d wabi            s                   like(UteAut)
002800060315     d kcod            s                   Like(tblCod)
002900060315     d kkey            s                   Like(TblKey)
003000060926     d wnar            s                   Like(cppnar)
003100060926     d varcap          s                   Like(cppcap)
003200060922     d varcapdup       s                   Like(cppcap)
003300060922     d wdat            s                   Like(cppdde)
003400060331     d yy              s              3  0
003500050103      * Ds date statistiche
003600060922     d tisiq0ds      e ds
003700061030     d dcppflo       e ds
003800060926     d
003900060926     d tisi00ds      e ds
004000060315     D* DS PER FNLV13R - CONTROLLO DI UN CAP
004100060315     D DSLV13        E DS                  EXTNAME(FNLV13DS)
004200060315     D* DS PER TISI95R - CONTROLLO DI CAP
004300060315     D DSSI95        E DS                  EXTNAME(TISI95DS)
004400080522
004500080522     D azcppinf        ds
004600080522     D  cppnrri              397    400i 0
004700040917     C**********************************************************************
004800060315     c* INDICATORI
004900060315     C**********************************************************************
005000060315     C* 01    - PROTEZIONE CAMPI VIDEATA
005100060315     C* 02    - IMMISSIONE
005200060315     C* 03    - ANNULLAMENTO
005300060315     C* 04    - MODIFICA
005400060315     C* 05    - INTERROGAZIONE
005500050103     C**********************************************************************
005600040917     C     *ENTRY        PLIST
005700040917     C                   PARM                    KPJBA
005800060922     C                   PARM                    tisiq0ds
005900060315     c
006000060315     c* Verifico se l'utente  abilitato alla gestione del calendario
006100060315     c                   exsr      CARINIZ
006200060922     c                   if        DQ0err='1'
006300060315     c                   goto      fine
006400060315     c                   endif
006500060329     c
006600060329     c* Duplica
006700060922     c                   if        DQ0op1='O03'
006800060329     c                   exsr      Duplica
006900060329     c                   goto      fine
007000060329     c                   endif
007100040917     c
007200040917     C     FOR01         TAG
007300000828      *
007400060922     C                   EXFMT     SIQ1d01
007500060315     C                   SETOFF                                       2890
007600060315     c                   setoff                                       404142
007700060315     c                   setoff                                       434445
007800060315     c                   setoff                                       464748
007900060330     c                   setoff                                       495152
008000060330     c                   setoff                                       535455
008100060330     c                   setoff                                       565758
008200061030     c                   setoff                                       596061
008300040917     C* F3 - FINE
008400060315     C                   IF        *INKC
008500060922     C                   EVAL      DQ0F03='1'
008600060315     C                   GOTO      FINE
008700060315     C                   ENDIF
008800060404     c
008900060315     C* F12- RITORNO
009000060315     C                   IF        *INKl
009100060922     C                   EVAL      DQ0F12='1'
009200060315     C                   GOTO      FINE
009300060315     C                   ENDIF
009400060315     c
009500060926     C* F7 - cappario
009600060926     C                   IF        *INKg
009700080408     c                   if        v1cnar<>'*'
009800060926     c                   eval      wnar=v1cnar
009900080408     c                   else
010000080408     c                   clear                   wnar
010100080408     c                   endif
010200060926     c                   exsr      lanpgm
010300060926     c
010400060926     c                   if        DQ0op1='F06'  and
010500060926     c                             d00cap<>*blanks
010600060926     c                   eval      v1ccap=d00cap
010700060926     c                   eval      v1cnar=d00nar
010800060926     c                   endif
010900060926     c                   goto      for01
011000060926     c*
011100060926     c                   endif
011200060926     c
011300060315      *  CONTROLLI --> non in interrogazione
011400040917     C                   EXSR      CTRD01
011500040917     c   90              goto      for01
011600000825      *
011700060315     c* Per visualizzazione, enter esce
011800060922     c                   if        DQ0op1='O05'
011900060315     c                   goto      fine
012000060315     c                   endif
012100060315     c
012200060315     c  nkf              goto      for01
012300060327     c* F06 - conferma
012400060327     c   kf              exsr      AGGIORNA
012500040917     C*
012600900515      *
012700000000     C     FINE          TAG
012800060315     C**
012900060315     C                   CLEAR                   tibs02ds
013000060315     C                   MOVEL     'C'           T02TLA
013100060315     C                   CALL      'TIBS02R'
013200060315     C                   PARM                    KPJBA
013300060922     c                   parm                    tibs02ds
013400060315     C**
013500060315     c                   clear                   dslv13
013600060315     C                   MOVEL     'C'           I13TLA
013700060315     C                   CALL      'FNLV13R'
013800060315     C                   PARM                    KPJBA
013900060315     C                   PARM                    DSLV13
014000060315     C                   PARM                    DSSI95
014100060922     c
014200000000     C                   SETON                                        LR
014300060329      *-----------------------------------------------------***********
014400060329      * Duplica
014500060329      *-----------------------------------------------------***********
014600060329     C     Duplica       BEGSR
014700060329     c
014800060926     c                   movel     dq0nar        vvcnar
014900060922     C                   write     SIQ1dw2
015000060329     c     forw2         tag
015100060922     C                   EXFMT     SIQ1dv2
015200060330     c                   setoff                                       2890
015300060330     c                   setoff                                       4041
015400060329     C* F12- RITORNO
015500060329     C                   IF        *INKl
015600060922     C                   EVAL      DQ0F12='1'
015700060329     C                   GOTO      Endduplica
015800060329     C                   ENDIF
015900060926     c
016000060926     C* F7 - cappario
016100060926     C                   IF        *INKg
016200080408     c                   if        vvcnar<>'*'
016300060926     c                   eval      wnar=vvcnar
016400080408     c                   else
016500080408     c                   clear                   wnar
016600080408     c                   endif
016700060926     c                   exsr      lanpgm
016800060926     c
016900060926     c                   if        d00cap<>*blanks
017000060926     c                   eval      vvccap=d00cap
017100060926     c                   eval      vvcnar=d00nar
017200060926     c                   endif
017300060926     c                   goto      forw2
017400060926     c                   endif
017500060926     c*
017600060329     c* controlli video 2
017700060329     c                   exsr      ctrw2
017800060329     c
017900060330     c  Nkf
018000060330     cor 90              goto      forw2
018100060330     c
018200060330     c   KF              exsr      Aggiorna
018300060329     c
018400060329     c     endduplica    endsr
018500900515      *-----------------------------------------------------***********
018600060329      * CONTROLLI                                           *  CTRD01 *
018700900515      *-----------------------------------------------------***********
018800040917     C     CTRD01        BEGSR
018900060315     c*
019000060922     c* Data decorrenza obbligatoria
019100060922     c                   eval      wdat=v1cdde
019200060922     c                   clear                   amgdde            8 0
019300060922     c                   exsr      CTRDAT
019400060922     c                   eval      v1cdde=wdat
019500060922     c* Errore
019600060922    1c                   if        *in90
019700060922     c                   seton                                        43
019800060922     c                   goto      endct1
019900060922    1c                   endif
020000060922     c
020100060922     c                   movel     amgdat        amgdde
020200060922     c* Data scadenza   obbligatoria
020300060922     c                   eval      wdat=v1cdsc
020400060922     c                   clear                   amgdsc            8 0
020500060922     c                   exsr      CTRDAT
020600060922     c                   eval      v1cdsc=wdat
020700060922     c* Errore
020800060922    1c                   if        *in90
020900060922     c                   seton                                        44
021000060922     c                   goto      endct1
021100060922    1c                   endif
021200060922     c
021300060922     c                   movel     amgdat        amgdsc
021400080522     c* Errore epr data scadenza < data decorrenza
021500080522     c                   if        amgdsc<amgdde
021600080522     c                   seton                                        904328
021700080522     C                   MOVEL     MSG(10)       V1cMSG
021800080522     c                   goto      endct1
021900080522     c                   endif
022000060922     c
022100060925     c* controllo la NAZIONE
022200080408     c                   if        v1cnar<>'*  '
022300060925     c                   eval      kcod='15'
022400060925     c                   movel(p)  v1cnar        kkey
022500060925     c     ktab          chain     tabel
022600060925     c                   if        not %found(tabel00f) or
022700060925     c                             tblflg<>' '
022800060925     c                   seton                                        409028
022900060925     C                   MOVEL     MSG(2)        V1cMSG
023000060925     c                   goto      endct1
023100060925    1c                   else
023200060925     c                   movel     tbluni        v1dnar
023300060925     c                   movel     tbluni        ds15
023400060925     c                   endif
023500080408     c                   else
023600080422     c                   clear                   v1dnar
023700080408     c* se nazione = '*' allora anche cap deve essere = '*'
023800080408     c                   if        v1ccap<>'*        '
023900080408     c                   seton                                        419028
024000080408     C                   MOVEL     MSG(9)        V1cMSG
024100080408     c                   goto      endct1
024200080408     c                   endif
024300080408     c                   endif
024400060925     c
024500060922     C* CONTROLLO CAP alla data decorrenza
024600060925     c** Se il cap inserito ha la lunghezza del cap fittizio
024700060925     c*  deve esistere sul cappario, altrimenti richiamo fnlv13
024800080408     c                   if        v1ccap<>'*        '
024900060925     c     ' '           checkr    v1ccap        posi              3 0    10
025000060925     c                   if        *in10 and posi=§15ecf
025100060925     c* Prendo la versione e chain sul cappario
025200060925     c                   eval      i96dri=amgdde
025300060925     c                   call      'TISI96R'
025400060925     c                   parm                    i96dri            8 0
025500060925     c                   parm      00000         o96ver            5 0
025600060925     c
025700060925     c     kcpc          chain     azcpc01l
025800060925     c                   if        not %found(azcpc01l)
025900060925     c                   seton                                        419028
026000060925     C                   MOVEL     MSG(4)        V1cMSG
026100060925     c                   goto      endct1
026200060925     c                   endif
026300060925     c
026400060925     c                   else
026500060922     c                   clear                   dslv13
026600060922     c                   clear                   dssi95
026700060922     C                   MOVEL     '3'           I95TCN
026800060925     C                   MOVEL     ' '           I13AF1
026900060925     C                   MOVEL     ' '           I13SZ2
0270000609259/12 C                   MOVEL     ' '           I13LA3
0271000609259/12 C                   MOVEL     ' '           I13SZ3
027200060922     c
027300060922     C                   MOVEL     v1ccAP        I95CAP
027400060922     C                   MOVEL     v1cNAR        I95NAR
027500060922     C                   MOVEL     amgdde        I95DAT
027600060925     C                   MOVEL     ' '           I13AF0
027700060922     C                   MOVEL     'S'           I13CNV
027800060922     C                   MOVEL     'S'           I13SZV
027900060922c
028000060922    2C     v1cCAP        IFNE      VARcap
028100060922     C                   CLEAR                   wfor1
028200060922     C                   CLEAR                   AAA               1
028300060922     C                   CLEAR                   AAB               1
028400060922     C                   CLEAR                   AAD               1
028500060922     C                   MOVEL     v1ccap        VARCAp
028600060922     c                   endif
028700060922     C**
028800060922     C                   MOVEL     wfor1         E13FZV
028900060922     C                   MOVEL     AAA           E13FZ1
029000060922     C                   MOVEL     AAB           E13FZ2
029100060922     C                   MOVEL     AAD           E13FZ3
029200060925     c
029300060922     C**
029400060922     C** CALL
029500060922     C                   CALL      'FNLV13R'
029600060922     C                   PARM                    KPJBA
029700060922     C                   PARM                    DSLV13
029800060922     C                   PARM                    DSSI95
029900060922     C*
030000060922     C                   MOVEL     O13ERR        WERR              1
030100060922     C                   MOVEL     O13MSG        v1cMSG
030200060922     C                   EXSR      CTRERR
030300060922     C**
030400060922     C* REIMPOSTO LE FORZATURE
030500060922     C                   MOVEL     E13FZV        wfor1
030600060922     C                   MOVEL     E13FZ1        AAA
030700060922     C                   MOVEL     E13FZ2        AAB
030800060922     C                   MOVEL     E13FZ3        AAD
030900060922     C**
031000060922    1C     WERR          IFNE      ' '
031100060922    2C     V1cMSG        IFNE      *BLANKS
031200060922     C                   SETON                                        28
031300060922     c                   endif
031400060922     c                   seton                                        9041
031500060922     c                   goto      endct1
031600060922     c                   endif
031700060925     c                   endif
031800080408     c                   endif
031900060922     c**
032000060922     c* Network
032100060922     c                   if        v1cntw=*blanks
032200060922     c                   seton                                        422890
032300060922     C                   MOVEL     MSG(1)        V1cMSG
032400060922     c                   goto      endct1
032500060922     c                   endif
032600060922     c
032700060922     C     '?'           SCAN      V1cntw                                 31
032800060922     C                   if        *in31
032900060922      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
033000060922     C                   SETON                                        98
033100060922     C                   CLEAR                   tibs02ds
033200060922     C                   MOVEL     'R'           T02MOD
033300060922     C                   MOVEL     KNSIF         T02SIF
033400060922     C                   MOVEL     'NTW'         T02COD
033500060922     C                   CALL      'TIBS02R'
033600060922     C                   PARM                    KPJBA
033700060922     C                   PARM                    tibs02ds
033800060922     C     T02ERR        IFEQ      *BLANKS
033900060922     C                   MOVEL     T02KE1        V1cntw
034000060922     C                   MOVEL     T02uni        V1dntw
034100060922     C                   ENDIF
034200060922     c
034300060922     c                   seton                                        4290
034400060922     c                   goto      endct1
034500060922     C                   ENDif
034600060922      *
034700060922     C                   CLEAR                   tibs02ds
034800060922     C                   MOVEL     'C'           T02MOD
034900060922     C                   MOVEL     KNSIF         T02SIF
035000060922     C                   MOVEL     'NTW'         T02COD
035100060922     C                   MOVEL     *BLANKS       T02KE1
035200060922     C                   MOVEL     v1cntw        T02KE1
035300060922     C                   CALL      'TIBS02R'
035400060922     C                   PARM                    KPJBA
035500060922     C                   PARM                    tibs02ds
035600060922     c*
035700060922      *
035800060922     C     T02ERR        IFNE      *BLANKS
035900060922     c                   seton                                        422890
036000060922     C                   MOVEL     MSG(1)        V1cMSG
036100060922     c                   goto      endct1
036200060922     C                   ENDIF
036300060927     c                   movel     t02uni        v1dntw
036400061030     c
036500061030     c* controllo selezioni sulle bolle
036600061030     c                   if        (v1ctut='SI' and v1ccas='SI')   or
036700080409     c                             (v1ctut='SI' and v1cpas<>' ')   or
036800080520     c                             (v1ctut='  ' and v1ccas='  ' and v1cpas=' '
036900080520     c                              and v1crit=*blanks)
037000061030     c                   seton                                        612890
037100061030     C                   MOVEL     MSG(6)        V1cMSG
037200061030     c                   goto      endct1
037300061030     C                   ENDIF
037400061030     c
037500060925     c* Controllo se record doppio x immissione o manut se modificato
037600060925     c*  decorrenza o netwok
037700060925     c                   if        *in02 or
037800080521     c*****                        (*in04 and dq0ntw<>v1cntw) or
037900080522     c                             (*in04 and dq0dde<>amgdde) or
038000080522     c                             (*in04 and dq0dsc<>amgdsc)
038100080522     c     kcppdop       setll     azcpp02l
038200080522     c     kcppdopr      readpe(N) azcpp02l
038300080522     c                   if        not %eof(azcpp02l)
038400080522     c                   if        amgdde >= cppdde and amgdde<=cppdsc
038500080522     c                             and (*in02 or (*in04 and cppnrri<>dq0nrr))
038600080522     c                   seton                                        904228
038700080522     C                   MOVEL     MSG(5)        V1cMSG
038800080522     c                   goto      endct1
038900080522     c                   endif
039000080522     c                   endif
039100080522     c     kcppdop       setll     azcpp02l
039200080522     c     kcppdopr      reade(N)  azcpp02l
039300080522     C                   IF        NOT %EOF(AZCPP02L)
039400080522     c                   if        cppdde>=amgdde and cppdde<=amgdsc
039500080522     c                             and (*in02 or (*in04 and cppnrri<>dq0nrr))
039600080522     c                   seton                                        904228
039700080522     C                   MOVEL     MSG(5)        V1cMSG
039800080522     c                   goto      endct1
039900080522     c                   endif
040000080522     c                   endif
040100060925     C                   ENDIF
040200060315     C**
040300040917     C     ENDCT1        ENDSR
040400060330      *-----------------------------------------------------***********
040500060922      * CONTROLLo Data
040600060330      *-----------------------------------------------------***********
040700060922     c     CTRDAT        BEGSR
040800060922    1c                   if        wdat>0
040900060922     c                   movel     wdat          w002a             2
041000060922     c     *eur          test(d)                 wdat                   30
041100060331    2c                   if        *in30  and w002a='00'
041200060922     c     *dmy          test(d)                 wdat                   30
041300060331     c                   endif
041400060331     c* Errore
041500060330    3c                   if        *in30
041600060922     c                   seton                                        9028
041700060330     C                   MOVEL     MSG(3)        V1cMSG
041800060330     c                   goto      endctdev
041900060331     c                   endif
042000060330     c
042100060331     c                   if        w002a='00'
042200060922     c     *dmy          move      wdat          dataeur
042300060330     c                   else
042400060922     c     *eur          move      wdat          dataeur
042500060330     c                   endif
042600060330     c
042700060330     c                   else
042800060922     c                   seton                                        9028
042900060330     C                   MOVEL     MSG(3)        V1cMSG
043000060330     c                   goto      endctdev
043100060330     c                   endif
043200060330     c* la trasformo in AAAAAMMGG - iso
043300060922     c                   move      dataeur       wdat
043400060330     c                   movel     dataeur       dataiso
043500060922     c     *iso          movel     dataiso       amgdat            8 0
043600060330     c
043700060330     c     endctdev      ENDSR
043800060329      *-----------------------------------------------------***********
043900060329      * CONTROLLI x deplica
044000060329      *-----------------------------------------------------***********
044100060329     C     CTRw2         BEGSR
044200060925     c* controllo la NAZIONE
044300080408     c                   if        vvcnar<>'*  '
044400060925     c                   eval      kcod='15'
044500060925     c                   movel(p)  vvcnar        kkey
044600060925     c     ktab          chain     tabel
044700060925     c                   if        not %found(tabel00f) or
044800060925     c                             tblflg<>' '
044900060925     c                   seton                                        409028
045000060925     C                   MOVEL     MSG(2)        V1cMSG
045100060925     c                   goto      endct2
045200060925    1c                   else
045300060925     c                   movel     tbluni        ds15
045400060925     c                   endif
045500080408     c                   else
045600080408     c* se nazione = '*' allora anche cap deve essere = '*'
045700080408     c                   if        vvccap<>'*        '
045800080408     c                   seton                                        419028
045900080408     C                   MOVEL     MSG(9)        V1cMSG
046000080409     c                   goto      endct2
046100080408     c                   endif
046200080408     c                   endif
046300060922     c* Controllo nuova nazione + cap
046400060925     c** Se il cap inserito ha la lunghezza del cap fittizio
046500060925     c*  deve esistere sul cappario, altrimenti richiamo fnlv13
046600080408     c                   if        vvccap<>'*        '
046700060925     c     ' '           checkr    vvccap        posi              3 0    10
046800060925     c                   if        *in10 and posi=§15ecf
046900060925     c* Prendo la versione e chain sul cappario
047000060925     c                   eval      i96dri=dq0dde
047100060925     c                   call      'TISI96R'
047200060925     c                   parm                    i96dri            8 0
047300060925     c                   parm      00000         o96ver            5 0
047400060925     c
047500060925     c     kcpc2         chain     azcpc01l
047600060925     c                   if        not %found(azcpc01l)
047700060925     c                   seton                                        419028
047800060925     C                   MOVEL     MSG(4)        V1cMSG
047900060925     c                   goto      endct2
048000060925     c                   endif
048100060925     c
048200060925     c                   else
048300060922     c                   clear                   dslv13
048400060922     c                   clear                   dssi95
048500060922     C                   MOVEL     '3'           I95TCN
048600060925     C                   MOVEL     ' '           I13AF1
048700060925     C                   MOVEL     ' '           I13SZ2
0488000609259/12 C                   MOVEL     ' '           I13LA3
0489000609259/12 C                   MOVEL     ' '           I13SZ3
049000060922     c
049100060922     C                   MOVEL     vvccAP        I95CAP
049200060922     C                   MOVEL     vvcNAR        I95NAR
049300060922     C                   MOVEL     dq0dde        I95DAT
049400060925     C                   MOVEL     ' '           I13AF0
049500060922     C                   MOVEL     'S'           I13CNV
049600060922     C                   MOVEL     'S'           I13SZV
049700060922c
049800060922    2C     vvcCAP        IFNE      VARcapdup
049900060922     C                   CLEAR                   wfor1             1
050000060922     C                   CLEAR                   AAA
050100060922     C                   CLEAR                   AAB
050200060922     C                   CLEAR                   AAD
050300060922     C                   MOVEL     vvccap        VARCApdup
050400060922     c                   endif
050500060922     C**
050600060922     C                   MOVEL     wfor1         E13FZV
050700060922     C                   MOVEL     AAA           E13FZ1
050800060922     C                   MOVEL     AAB           E13FZ2
050900060922     C                   MOVEL     AAD           E13FZ3
051000060922     C**
051100060922     C** CALL
051200060922     C                   CALL      'FNLV13R'
051300060922     C                   PARM                    KPJBA
051400060922     C                   PARM                    DSLV13
051500060922     C                   PARM                    DSSI95
051600060922     C*
051700060922     C                   MOVEL     O13ERR        WERR              1
051800060922     C                   MOVEL     O13MSG        v1cMSG
051900060922     C* IMPOSTO CAMPI PASSATI
052000060922     C     O13RON        IFEQ      'S'
052100060922     C                   MOVEL     O95NAR        vvcNAR
052200060922     C                   ENDIF
052300060922     C     O13ROC        IFEQ      'S'
052400060922     C                   MOVEL     O95CAP        vvcCAP
052500060922     C                   ENDIF
052600060922     c
052700060922     C**
052800060922     C* REIMPOSTO LE FORZATURE
052900060922     C                   MOVEL     E13FZV        wfor1
053000060922     C                   MOVEL     E13FZ1        AAA
053100060922     C                   MOVEL     E13FZ2        AAB
053200060922     C                   MOVEL     E13FZ3        AAD
053300060922     C**
053400060922    1C     WERR          IFNE      ' '
053500060922    2C     V1cMSG        IFNE      *BLANKS
053600060922     C                   SETON                                        28
053700060922     c                   endif
053800060922     c                   seton                                        9041
053900060922     c                   goto      endct2
054000060922     c                   endif
054100060925     c                   endif
054200080408     c                   endif
054300060925     c* Controllo se record doppio
054400080522     c*    kcppdop2      setll     azcpp02l                               90
054500080522     c*                  if        *in90
054600080522     c*                  seton                                        4228
054700080522     C*                  MOVEL     MSG(5)        V1cMSG
054800080522     c*                  goto      endct2
054900080522     C*                  ENDIF
055000080522     c     kcppdop2      setll     azcpp02l
055100080522     c     kcppdop2r     readpe(N) azcpp02l
055200080522     c                   if        not %eof(azcpp02l)
055300080523     c                   if        dq0dde >= cppdde and dq0dde<=cppdsc
055400080522     c                   seton                                        904228
055500080522     C                   MOVEL     MSG(5)        V1cMSG
055600080522     c                   goto      endct2
055700080522     c                   endif
055800080522     c                   endif
055900080522     c     kcppdop2      setll     azcpp02l
056000080522     c     kcppdop2r     reade(N)  azcpp02l
056100080522     C                   IF        NOT %EOF(AZCPP02L)
056200080523     c                   if        cppdde>=dq0dde and cppdde<=dq0dsc
056300080522     c                   seton                                        904228
056400080522     C                   MOVEL     MSG(5)        V1cMSG
056500080522     c                   goto      endct2
056600080522     c                   endif
056700080522     c                   endif
056800060330     c*
056900060329     C     ENDCT2        ENDSR
057000060315     C*
057100060315     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
057200060315     C     CTRERR        BEGSR
057300060315     C* IMPOSTO CAMPI PASSATI
057400060315     C     O13RON        IFEQ      'S'
057500060315     C                   MOVEL     O95NAR        v1cNAR
057600060315     C                   ENDIF
057700060315     C     O13ROC        IFEQ      'S'
057800060315     C                   MOVEL     O95CAP        v1cCAP
057900060315     C                   ENDIF
058000060315     c
058100060315    2C                   ENDSR
058200050214     C**************************************************************************
058300060315     C*  Carico dati utente per vedere se abilitato alla gestione      cal
058400050214     C**************************************************************************
058500060315     C     CARiniz       BEGSR
058600060922     c     kcpp          klist
058700080521     c                   kfld                    DQ0ntw
058800060922     c                   kfld                    DQ0nar
058900060922     c                   kfld                    DQ0cap
059000060922     c                   kfld                    DQ0dde
059100060925     c     kcppdop       klist
059200080521     c                   kfld                    v1cntw
059300060925     c                   kfld                    v1cnar
059400060925     c                   kfld                    v1ccap
059500060925     c                   kfld                    amgdde
059600080522     c     kcppdopr      klist
059700080522     c                   kfld                    v1cntw
059800080522     c                   kfld                    v1cnar
059900080522     c                   kfld                    v1ccap
060000060925     c     kcppdop2      klist
060100080521     c                   kfld                    dq0ntw
060200060925     c                   kfld                    vvcnar
060300060925     c                   kfld                    vvccap
060400060925     c                   kfld                    dq0dde
060500080522     c     kcppdop2r     klist
060600080522     c                   kfld                    dq0ntw
060700080522     c                   kfld                    vvcnar
060800080522     c                   kfld                    vvccap
060900060925     c     kcpc          klist
061000060925     c                   kfld                    o96ver
061100060925     c                   kfld                    v1cnar
061200060925     c                   kfld                    v1ccap
061300060925     c     kcpc2         klist
061400060925     c                   kfld                    o96ver
061500060925     c                   kfld                    vvcnar
061600060925     c                   kfld                    vvccap
061700060330     c     Ktab          Klist
061800060330     c                   Kfld                    codut             1 0
061900060330     c                   Kfld                    kCod
062000060330     c                   Kfld                    kKey
062100060331     c
062200060331     c                   z-add     1             codut
062300060327     c
062400060315     c                   clear                   dute01
062500060315      *
062600060315     c     *dtaara       define    §azute        azuteds
062700060315     c     *dtaara       define    §datiute      ddatiute
062800060315     c                   in(E)     *dtaara
062900060315    1c                   If        %error  or RSUT = *blanks
063000060315     c                   Clear                   Tibs34ds
063100060315     c                   Call      'TIBS34R'
063200060315     c                   Parm                    Tibs34ds
063300060315     c                   In        *dtaara
063400060315    1c                   EndIf
063500060404    cc                   movel     rsut          v1crsu
063600060315
063700060315     c                   Clear                   wabi
063800060922     c                   Clear                   DQ0msg
063900060315
064000060315     c* Vado avanti nei caricamenti non in interrogazione
064100060922    1c                   if        DQ0op1<>'O05'
064200060315     c
064300060315      * se ho errori nei dati utente esco dal pgm
064400060922w   2c                   if        DutErr = 'E'
064500060315     c                   GoTo      ENDINIZ
064600060922    3c                   endif
064700060922     c
064800060315      * se non c'è l'abilitazione
064900060315      * --> se 1° livello, abilitazioni al terminal
065000060315      *     se 2° livello, abilitazioni al punto operativo
065100060315      *     se sede è impossibile ma se capita mando a fine il pgm
065200060922w   2c                   if        UteAut = *Blanks
065300060922     c                   clear                   dute01
065400060315      * carica le abilitazioni del profilo
065500060922x  x2c                   else
065600060315     c                   Movel     UteFaf        Dute01
065700060922e   2c                   endif
065800060922e   2c                   endif
065900060315     c
066000060315     c* Data corrente
066100060315     c                   time                    w0140            14 0
066200060315     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
066300060315     c     *eur          movel     w0080         dataeur
066400060315     c                   movel     dataeur       dataiso
066500060315     c     *iso          movel     dataiso       datcor            8 0
066600060315     c
066700060315     C*
066800060315     C* VEDO QUALE OPZINE SCELTA
066900060315     c*
067000060922     C                   CLEAR                   V1Cntw
067100060922     C                   CLEAR                   V1Dntw
067200060315     C                   CLEAR                   V1cnar
067300060315     C                   CLEAR                   V1dnar
067400060922     C                   CLEAR                   V1ccap
067500080520     C                   CLEAR                   V1huti
067600060922     C                   CLEAR                   V1cdde
067700060922     C                   CLEAR                   V1cdsc
067800061030     C                   CLEAR                   V1ctut
067900061030     C                   CLEAR                   V1ccas
068000080409     C                   CLEAR                   V1cpas
068100080520     C                   CLEAR                   V1crit
068200061030     c* campo solo per duplica
068300061030     c                   CLEAR                   V1cbol
068400061030     c
068500060315     c                   setoff                                       404142
068600060922     c                   setoff                                       434401
068700060315     c* Imposto i campi
068800060922     C                   MOVEL     DQ0NAR        V1CNAR
068900060922     C                   MOVEL     DQ0CAP        V1CCAP
069000060922     C                   MOVEL     DQ0CAP        varCAP
069100060922     C                   MOVEL     DQ0ntw        V1Cntw
069200060922     c                   if        dq0uti='S'
069300080520     c                   eval      v1huti='SI'
069400060922     c                   else
069500080520     c                   eval      v1huti='NO'
069600060922     c                   endif
069700060404     c
069800060922     C                   IF        DQ0dde>0
069900060922     C     *ISO          MOVEL     DQ0dde        DATAISO
070000060315     C                   MOVEL     DATAISO       DATAEUR
070100060922     C     *EUR          MOVEL     DATAEUR       V1Cdde
070200060315     C                   ENDIF
070300060922     C                   IF        DQ0dsc>0
070400060922     C     *ISO          MOVEL     DQ0dsc        DATAISO
070500060922     C                   MOVEL     DATAISO       DATAEUR
070600060922     C     *EUR          MOVEL     DATAEUR       V1Cdsc
070700060922     C                   ENDIF
070800060315     C*
070900060922     c                   if        DQ0cap<>*blanks
071000060315     c                   eval      kcod='15'
071100060922     c                   movel(p)  DQ0nar        kkey
071200060315     c     ktab          chain     tabel
071300060327     c                   if        %found(tabel00f)
071400060315     c                   movel     tbluni        v1dnar
071500060315     c                   endif
071600060922     c                   endif
071700060922     c* Network
071800060922     c                   if        dq0ntw<>*blanks
071900060922     C                   MOVEL     dq0ntw        V1cntw
072000060922     C                   CLEAR                   tibs02ds
072100060922     C                   MOVEL     'C'           T02MOD
072200060922     C                   MOVEL     KNSIF         T02SIF
072300060922     C                   MOVEL     'NTW'         T02COD
072400060922     C                   MOVEL(P)  dq0ntw        T02KE1
072500060922     C                   CALL      'TIBS02R'
072600060922     C                   PARM                    KPJBA
072700060922     C                   PARM                    tibs02ds
072800060922      *
072900060922     C     T02ERR        IFNE      *BLANKS
073000060922     C                   MOVEL     *all'?'       v1dntw
073100060922     C                   ELSE
073200060927     C                   MOVEL     t02uni        v1dntw
073300060922     C                   ENDIF
073400060922     C                   ENDIF
073500061030     c* dcppflo
073600080520     c                   movel     dq0flo        dcppflo
073700080520     c* ritiri
073800080520     c                   if        §cpprit='S'
073900080520     c                   eval      v1crit='SI'
074000080520     c                   endif
074100080520     c* spedizioni
074200080520     c                   if        §cppall='S'
074300061030     c                   eval      v1ctut='SI'
074400061030     c                   eval      v1cbol='Tutte'
074500061030     c                   else
074600061030     c                   if        §cppcas='S'
074700061030     c                   eval      v1ccas='SI'
074800080409     c                   eval      v1cbol='C/Assegno'
074900061030     c                   endif
075000080409     c                   if        §cpppas<>' '
075100080409     c                   eval      v1cpas=§cpppas
075200080409     c                   if        §cppcas<>'S'
075300080409     c                   eval      v1cbol='Assegnato'
075400080409     c                   else
075500080409     c                   eval      v1cbol='C/Ass-Ass'
075600080409     c                   endif
075700080409     c                   endif
075800061030     c                   endif
075900061030     c
076000060315     c* IMMISSIONE
076100060315     c                   select
076200060922     C                   WHEN      DQ0OP1='F06'
076300060315     C                   SETON                                        02
076400060315     C* MANUTENZIONE
076500060922     C                   WHEN      DQ0OP1='O02'
076600060315     C                   SETON                                        04
076700060315     C* ANNULLAMENTO
076800060922     C                   WHEN      DQ0OP1='O04'
076900060315     C                   SETON                                        0301
077000060315     C* VISULIZZA
077100060922     C                   WHEN      DQ0OP1='O05'
077200060315     C                   SETON                                        0501
077300060315     C                   ENDSL
077400060927     c*
077500060927     c* se richiamo da Cappario disabilito F7
077600060927     c                   movel     dq0op0        wchiama           1
077700060927     c                   if        wchiama='C'
077800060927     c                   seton                                        11
077900060927     c                   else
078000060927     c                   setoff                                       11
078100060927     c                   endif
078200060315     C*
078300060315     C
078400060315     C     ENDINIZ       ENDSR
078500060327     C**************************************************************************
078600060327     C*  Aggiornamento dati su azcep
078700060327     C**************************************************************************
078800060327     C     Aggiorna      BEGSR
078900060327     c* Annullamento
079000060327     c                   select
079100060922     c                   when      DQ0op1='O04'
079200080523     c     dq0nrr        delete    azcpp00f
079300080523     c                   if        not %found
079400060922     c                   eval      DQ0msg=msg(7)
079500060922     c                   eval      DQ0err='1'
079600060327     c                   endif
079700060327     c
079800060922     c                   when      DQ0op1='O02'
079900060327     c* Visto che tutti i campi sono in key, annullo e riscrivo
080000080523     c     dq0nrr        delete    azcpp00f
080100080523     c                   if        not %found
080200060922     c                   eval      DQ0msg=msg(8)
080300060922     c                   eval      DQ0err='1'
080400060327     c                   else
080500060327     c                   exsr      Scrivi
080600060327     c                   endif
080700060327     c
080800060330     c* Duplica
080900060922     c                   when      DQ0op1='O03'
081000060404     c                   exsr      SCRIVI
081100060922     c* Inserimento
081200060922     c                   when      DQ0op1='F06'
081300060327     c                   exsr      SCRIVI
081400060327     c                   ENDSL
081500060327     c
081600060327     c
081700060327     c                   ENDSR
081800060327     C**************************************************************************
081900060327     C*  Scrittura nuovo record
082000060327     C**************************************************************************
082100060327     C     Scrivi        BEGSR
082200060922     c                   clear                   azcpp000
082300060922     c                   if        DQ0op1='O03'
082400060922     c                   eval      cppnar=vvcnar
082500060922     c                   eval      cppcap=vvccap
082600060922     c                   eval      cppdde=dq0dde
082700060922     c                   eval      cppdsc=dq0dsc
082800061030     c                   eval      cppflo=dq0flo
082900060922     c                   else
083000060922     c                   eval      cppnar=v1cnar
083100060922     c                   eval      cppcap=v1ccap
083200060922     c                   eval      cppdde=amgdde
083300060922     c                   eval      cppdsc=amgdsc
083400061030     c*
083500080520     c                   clear                   cppflo
083600080520     c                   movel     v1ctut        §cppall
083700061030     c                   movel     v1ccas        §cppcas
083800080409     c                   movel     v1cpas        §cpppas
083900080520     c                   movel     v1crit        §cpprit
084000061030     c                   movel     dcppflo       cppflo
084100061030     c                   endif
084200061030     c
084300080520     c                   MOVEL     v1huti        cpputi
084400060922     c                   eval      cppntw=v1cntw
084500060922     c                   write     azcpp000
084600060327     c                   endsr
084700060926     C**************************************************************************
084800060926     C*  richiamo int cappario per scelta
084900060926     C**************************************************************************
085000060926     C     LANPGM        BEGSR
085100060926     C* IMPOSTA LA DS DI PROCEDURA
085200060926     c                   clear                   tisi00ds
085300060926     c                   if        amgdde>0
085400060926     c                   movel     amgdde        d00dri
085500060926     c                   else
085600060926     c                   movel     datcor        d00dri
085700060926     c                   endif
085800060926     c                   if        dq0op1='F06'  or dq0op1='O03'
085900060926     C                   MOVEL     'C01'         d00OP0                         *LIVELLO PROCEDURA
086000060926     c                   else
086100060926     C                   MOVEL     'C05'         d00OP0                         *LIVELLO PROCEDURA
086200060926     c                   endif
086300060926     c
086400060926     C                   clear                   d00OP1                         *LIVELLO PGM
086500060926     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
086600060926     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
086700060926     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
086800060926     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
086900060926     C                   movel     wnar          d00NAR                         *NAZIONE
087000060926     C                   clear                   d00CAP                         *CAP
087100060926     C                   clear                   d00LOC                         *LOCALITA'
087200060926     C*
087300060926     C                   CALL      'TISI02R'
087400060926     C                   PARM                    KPJBA
087500060926     C                   PARM                    tisi00ds
087600060926     c                   ENDSR
087700020805      **************************************************************************
087800960302** SCHIERA MSG - MESSAGGI DI ERRORE
087900060922Network obbligatorio                                                           01
088000060925Nazione inesistente                                                            02
088100060922Data Errata                                                                    03
088200060925CAP fittizio inesistente sul cappario alla data decorrenza
088300080523Il record immesso si sovrappone ad uno già in decorrenza
088400080520Blocchi incongruenti o non selezionati                                         06
088500060922Record al momento non annullabile: in uso!!!! Verificare e riprovare           07
088600060922Record al momento non modificabile: in uso!!!! Verificare e riprovare          08
088700080409Immesso '*' nella nazione: immettere '*' anche nel cap                         09
088800080522Data decorrenza maggiore di data scadenza
