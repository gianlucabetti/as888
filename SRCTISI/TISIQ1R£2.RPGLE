000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200060922      * TISIQ1R *----------------------------------------------------*
000300060922      *         - cappario utilizzi particolari
000400940915      *--------------------------------------------------------------*
000500060922     FTISIQ1d   CF   E             WORKSTN
000600060922     Fazcpp01l  uF a E           K DISK
000700060925     Fazcpc01l  iF   E           K DISK
000800060315     Ftabel00f  IF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200060922     D MSG             S             78    DIM(8) CTDATA PERRCD(1)
001300060315     d
001400040917     D KPJBA         E DS
001500060315     d Tibs02Ds      e ds
001600060315     d Azuteds       e ds                  extname(Azute00f)
001700060315     d dDatiute      e ds
001800060315     d dute01        e ds
001900060315     D TIBS34DS      E DS
002000060925     D ds15          E DS
002100060922     d
002200050103     d Dataiso         s               d   datfmt(*iso)
002300050103     d Dataeur         s               d   datfmt(*eur)
002400050103     d
002500060315     d wabi            s                   like(UteAut)
002600060315     d kcod            s                   Like(tblCod)
002700060315     d kkey            s                   Like(TblKey)
002800060926     d wnar            s                   Like(cppnar)
002900060926     d varcap          s                   Like(cppcap)
003000060922     d varcapdup       s                   Like(cppcap)
003100060922     d wdat            s                   Like(cppdde)
003200060331     d yy              s              3  0
003300050103      * Ds date statistiche
003400060922     d tisiq0ds      e ds
003500061030     d dcppflo       e ds
003600060926     d
003700060926     d tisi00ds      e ds
003800060315     D* DS PER FNLV13R - CONTROLLO DI UN CAP
003900060315     D DSLV13        E DS                  EXTNAME(FNLV13DS)
004000060315     D* DS PER TISI95R - CONTROLLO DI CAP
004100060315     D DSSI95        E DS                  EXTNAME(TISI95DS)
004200040917     C**********************************************************************
004300060315     c* INDICATORI
004400060315     C**********************************************************************
004500060315     C* 01    - PROTEZIONE CAMPI VIDEATA
004600060315     C* 02    - IMMISSIONE
004700060315     C* 03    - ANNULLAMENTO
004800060315     C* 04    - MODIFICA
004900060315     C* 05    - INTERROGAZIONE
005000050103     C**********************************************************************
005100040917     C     *ENTRY        PLIST
005200040917     C                   PARM                    KPJBA
005300060922     C                   PARM                    tisiq0ds
005400060315     c
005500060315     c* Verifico se l'utente  abilitato alla gestione del calendario
005600060315     c                   exsr      CARINIZ
005700060922     c                   if        DQ0err='1'
005800060315     c                   goto      fine
005900060315     c                   endif
006000060329     c
006100060329     c* Duplica
006200060922     c                   if        DQ0op1='O03'
006300060329     c                   exsr      Duplica
006400060329     c                   goto      fine
006500060329     c                   endif
006600040917     c
006700040917     C     FOR01         TAG
006800000828      *
006900060922     C                   EXFMT     SIQ1d01
007000060315     C                   SETOFF                                       2890
007100060315     c                   setoff                                       404142
007200060315     c                   setoff                                       434445
007300060315     c                   setoff                                       464748
007400060330     c                   setoff                                       495152
007500060330     c                   setoff                                       535455
007600060330     c                   setoff                                       565758
007700061030     c                   setoff                                       596061
007800040917     C* F3 - FINE
007900060315     C                   IF        *INKC
008000060922     C                   EVAL      DQ0F03='1'
008100060315     C                   GOTO      FINE
008200060315     C                   ENDIF
008300060404     c
008400060315     C* F12- RITORNO
008500060315     C                   IF        *INKl
008600060922     C                   EVAL      DQ0F12='1'
008700060315     C                   GOTO      FINE
008800060315     C                   ENDIF
008900060315     c
009000060926     C* F7 - cappario
009100060926     C                   IF        *INKg
009200060926     c                   eval      wnar=v1cnar
009300060926     c                   exsr      lanpgm
009400060926     c
009500060926     c                   if        DQ0op1='F06'  and
009600060926     c                             d00cap<>*blanks
009700060926     c                   eval      v1ccap=d00cap
009800060926     c                   eval      v1cnar=d00nar
009900060926     c                   endif
010000060926     c                   goto      for01
010100060926     c*
010200060926     c                   endif
010300060926     c
010400060315      *  CONTROLLI --> non in interrogazione
010500040917     C                   EXSR      CTRD01
010600040917     c   90              goto      for01
010700000825      *
010800060315     c* Per visualizzazione, enter esce
010900060922     c                   if        DQ0op1='O05'
011000060315     c                   goto      fine
011100060315     c                   endif
011200060315     c
011300060315     c  nkf              goto      for01
011400060327     c* F06 - conferma
011500060327     c   kf              exsr      AGGIORNA
011600040917     C*
011700900515      *
011800000000     C     FINE          TAG
011900060315     C**
012000060315     C                   CLEAR                   tibs02ds
012100060315     C                   MOVEL     'C'           T02TLA
012200060315     C                   CALL      'TIBS02R'
012300060315     C                   PARM                    KPJBA
012400060922     c                   parm                    tibs02ds
012500060315     C**
012600060315     c                   clear                   dslv13
012700060315     C                   MOVEL     'C'           I13TLA
012800060315     C                   CALL      'FNLV13R'
012900060315     C                   PARM                    KPJBA
013000060315     C                   PARM                    DSLV13
013100060315     C                   PARM                    DSSI95
013200060922     c
013300000000     C                   SETON                                        LR
013400060329      *-----------------------------------------------------***********
013500060329      * Duplica
013600060329      *-----------------------------------------------------***********
013700060329     C     Duplica       BEGSR
013800060329     c
013900060926     c                   movel     dq0nar        vvcnar
014000060922     C                   write     SIQ1dw2
014100060329     c     forw2         tag
014200060922     C                   EXFMT     SIQ1dv2
014300060330     c                   setoff                                       2890
014400060330     c                   setoff                                       4041
014500060329     C* F12- RITORNO
014600060329     C                   IF        *INKl
014700060922     C                   EVAL      DQ0F12='1'
014800060329     C                   GOTO      Endduplica
014900060329     C                   ENDIF
015000060926     c
015100060926     C* F7 - cappario
015200060926     C                   IF        *INKg
015300060926     c                   eval      wnar=vvcnar
015400060926     c                   exsr      lanpgm
015500060926     c
015600060926     c                   if        d00cap<>*blanks
015700060926     c                   eval      vvccap=d00cap
015800060926     c                   eval      vvcnar=d00nar
015900060926     c                   endif
016000060926     c                   goto      forw2
016100060926     c                   endif
016200060926     c*
016300060329     c* controlli video 2
016400060329     c                   exsr      ctrw2
016500060329     c
016600060330     c  Nkf
016700060330     cor 90              goto      forw2
016800060330     c
016900060330     c   KF              exsr      Aggiorna
017000060329     c
017100060329     c     endduplica    endsr
017200900515      *-----------------------------------------------------***********
017300060329      * CONTROLLI                                           *  CTRD01 *
017400900515      *-----------------------------------------------------***********
017500040917     C     CTRD01        BEGSR
017600060315     c*
017700060922     c* Data decorrenza obbligatoria
017800060922     c                   eval      wdat=v1cdde
017900060922     c                   clear                   amgdde            8 0
018000060922     c                   exsr      CTRDAT
018100060922     c                   eval      v1cdde=wdat
018200060922     c* Errore
018300060922    1c                   if        *in90
018400060922     c                   seton                                        43
018500060922     c                   goto      endct1
018600060922    1c                   endif
018700060922     c
018800060922     c                   movel     amgdat        amgdde
018900060922     c* Data scadenza   obbligatoria
019000060922     c                   eval      wdat=v1cdsc
019100060922     c                   clear                   amgdsc            8 0
019200060922     c                   exsr      CTRDAT
019300060922     c                   eval      v1cdsc=wdat
019400060922     c* Errore
019500060922    1c                   if        *in90
019600060922     c                   seton                                        44
019700060922     c                   goto      endct1
019800060922    1c                   endif
019900060922     c
020000060922     c                   movel     amgdat        amgdsc
020100060922     c
020200060925     c* controllo la NAZIONE
020300060925     c                   eval      kcod='15'
020400060925     c                   movel(p)  v1cnar        kkey
020500060925     c     ktab          chain     tabel
020600060925     c                   if        not %found(tabel00f) or
020700060925     c                             tblflg<>' '
020800060925     c                   seton                                        409028
020900060925     C                   MOVEL     MSG(2)        V1cMSG
021000060925     c                   goto      endct1
021100060925    1c                   else
021200060925     c                   movel     tbluni        v1dnar
021300060925     c                   movel     tbluni        ds15
021400060925     c                   endif
021500060925     c
021600060922     C* CONTROLLO CAP alla data decorrenza
021700060925     c** Se il cap inserito ha la lunghezza del cap fittizio
021800060925     c*  deve esistere sul cappario, altrimenti richiamo fnlv13
021900060925     c     ' '           checkr    v1ccap        posi              3 0    10
022000060925     c                   if        *in10 and posi=§15ecf
022100060925     c* Prendo la versione e chain sul cappario
022200060925     c                   eval      i96dri=amgdde
022300060925     c                   call      'TISI96R'
022400060925     c                   parm                    i96dri            8 0
022500060925     c                   parm      00000         o96ver            5 0
022600060925     c
022700060925     c     kcpc          chain     azcpc01l
022800060925     c                   if        not %found(azcpc01l)
022900060925     c                   seton                                        419028
023000060925     C                   MOVEL     MSG(4)        V1cMSG
023100060925     c                   goto      endct1
023200060925     c                   endif
023300060925     c
023400060925     c                   else
023500060922     c                   clear                   dslv13
023600060922     c                   clear                   dssi95
023700060922     C                   MOVEL     '3'           I95TCN
023800060925     C                   MOVEL     ' '           I13AF1
023900060925     C                   MOVEL     ' '           I13SZ2
0240000609259/12 C                   MOVEL     ' '           I13LA3
0241000609259/12 C                   MOVEL     ' '           I13SZ3
024200060922     c
024300060922     C                   MOVEL     v1ccAP        I95CAP
024400060922     C                   MOVEL     v1cNAR        I95NAR
024500060922     C                   MOVEL     amgdde        I95DAT
024600060925     C                   MOVEL     ' '           I13AF0
024700060922     C                   MOVEL     'S'           I13CNV
024800060922     C                   MOVEL     'S'           I13SZV
024900060922c
025000060922    2C     v1cCAP        IFNE      VARcap
025100060922     C                   CLEAR                   wfor1
025200060922     C                   CLEAR                   AAA               1
025300060922     C                   CLEAR                   AAB               1
025400060922     C                   CLEAR                   AAD               1
025500060922     C                   MOVEL     v1ccap        VARCAp
025600060922     c                   endif
025700060922     C**
025800060922     C                   MOVEL     wfor1         E13FZV
025900060922     C                   MOVEL     AAA           E13FZ1
026000060922     C                   MOVEL     AAB           E13FZ2
026100060922     C                   MOVEL     AAD           E13FZ3
026200060925     c
026300060922     C**
026400060922     C** CALL
026500060922     C                   CALL      'FNLV13R'
026600060922     C                   PARM                    KPJBA
026700060922     C                   PARM                    DSLV13
026800060922     C                   PARM                    DSSI95
026900060922     C*
027000060922     C                   MOVEL     O13ERR        WERR              1
027100060922     C                   MOVEL     O13MSG        v1cMSG
027200060922     C                   EXSR      CTRERR
027300060922     C**
027400060922     C* REIMPOSTO LE FORZATURE
027500060922     C                   MOVEL     E13FZV        wfor1
027600060922     C                   MOVEL     E13FZ1        AAA
027700060922     C                   MOVEL     E13FZ2        AAB
027800060922     C                   MOVEL     E13FZ3        AAD
027900060922     C**
028000060922    1C     WERR          IFNE      ' '
028100060922    2C     V1cMSG        IFNE      *BLANKS
028200060922     C                   SETON                                        28
028300060922     c                   endif
028400060922     c                   seton                                        9041
028500060922     c                   goto      endct1
028600060922     c                   endif
028700060925     c                   endif
028800060922     c**
028900060922     c* Network
029000060922     c                   if        v1cntw=*blanks
029100060922     c                   seton                                        422890
029200060922     C                   MOVEL     MSG(1)        V1cMSG
029300060922     c                   goto      endct1
029400060922     c                   endif
029500060922     c
029600060922     C     '?'           SCAN      V1cntw                                 31
029700060922     C                   if        *in31
029800060922      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
029900060922     C                   SETON                                        98
030000060922     C                   CLEAR                   tibs02ds
030100060922     C                   MOVEL     'R'           T02MOD
030200060922     C                   MOVEL     KNSIF         T02SIF
030300060922     C                   MOVEL     'NTW'         T02COD
030400060922     C                   CALL      'TIBS02R'
030500060922     C                   PARM                    KPJBA
030600060922     C                   PARM                    tibs02ds
030700060922     C     T02ERR        IFEQ      *BLANKS
030800060922     C                   MOVEL     T02KE1        V1cntw
030900060922     C                   MOVEL     T02uni        V1dntw
031000060922     C                   ENDIF
031100060922     c
031200060922     c                   seton                                        4290
031300060922     c                   goto      endct1
031400060922     C                   ENDif
031500060922      *
031600060922     C                   CLEAR                   tibs02ds
031700060922     C                   MOVEL     'C'           T02MOD
031800060922     C                   MOVEL     KNSIF         T02SIF
031900060922     C                   MOVEL     'NTW'         T02COD
032000060922     C                   MOVEL     *BLANKS       T02KE1
032100060922     C                   MOVEL     v1cntw        T02KE1
032200060922     C                   CALL      'TIBS02R'
032300060922     C                   PARM                    KPJBA
032400060922     C                   PARM                    tibs02ds
032500060922     c*
032600060922      *
032700060922     C     T02ERR        IFNE      *BLANKS
032800060922     c                   seton                                        422890
032900060922     C                   MOVEL     MSG(1)        V1cMSG
033000060922     c                   goto      endct1
033100060922     C                   ENDIF
033200060927     c                   movel     t02uni        v1dntw
033300061030     c
033400061030     c* controllo selezioni sulle bolle
033500061030     c                   if        (v1ctut='SI' and v1ccas='SI')   or
033600061030     c                             (v1ctut='  ' and v1ccas='  ')
033700061030     c                   seton                                        612890
033800061030     C                   MOVEL     MSG(6)        V1cMSG
033900061030     c                   goto      endct1
034000061030     C                   ENDIF
034100061030     c
034200060925     c* Controllo se record doppio x immissione o manut se modificato
034300060925     c*  decorrenza o netwok
034400060925     c                   if        *in02 or
034500060925     c                             (*in04 and dq0ntw<>v1cntw) or
034600060925     c                             (*in04 and dq0dde<>amgdde)
034700060925     c     kcppdop       setll     azcpp01l                               90
034800060925     c                   if        *in90
034900060925     c                   seton                                        4228
035000060925     C                   MOVEL     MSG(5)        V1cMSG
035100060925     c                   goto      endct1
035200060925     C                   ENDIF
035300060925     C                   ENDIF
035400060315     C**
035500040917     C     ENDCT1        ENDSR
035600060330      *-----------------------------------------------------***********
035700060922      * CONTROLLo Data
035800060330      *-----------------------------------------------------***********
035900060922     c     CTRDAT        BEGSR
036000060922    1c                   if        wdat>0
036100060922     c                   movel     wdat          w002a             2
036200060922     c     *eur          test(d)                 wdat                   30
036300060331    2c                   if        *in30  and w002a='00'
036400060922     c     *dmy          test(d)                 wdat                   30
036500060331     c                   endif
036600060331     c* Errore
036700060330    3c                   if        *in30
036800060922     c                   seton                                        9028
036900060330     C                   MOVEL     MSG(3)        V1cMSG
037000060330     c                   goto      endctdev
037100060331     c                   endif
037200060330     c
037300060331     c                   if        w002a='00'
037400060922     c     *dmy          move      wdat          dataeur
037500060330     c                   else
037600060922     c     *eur          move      wdat          dataeur
037700060330     c                   endif
037800060330     c
037900060330     c                   else
038000060922     c                   seton                                        9028
038100060330     C                   MOVEL     MSG(3)        V1cMSG
038200060330     c                   goto      endctdev
038300060330     c                   endif
038400060330     c* la trasformo in AAAAAMMGG - iso
038500060922     c                   move      dataeur       wdat
038600060330     c                   movel     dataeur       dataiso
038700060922     c     *iso          movel     dataiso       amgdat            8 0
038800060330     c
038900060330     c     endctdev      ENDSR
039000060329      *-----------------------------------------------------***********
039100060329      * CONTROLLI x deplica
039200060329      *-----------------------------------------------------***********
039300060329     C     CTRw2         BEGSR
039400060925     c* controllo la NAZIONE
039500060925     c                   eval      kcod='15'
039600060925     c                   movel(p)  vvcnar        kkey
039700060925     c     ktab          chain     tabel
039800060925     c                   if        not %found(tabel00f) or
039900060925     c                             tblflg<>' '
040000060925     c                   seton                                        409028
040100060925     C                   MOVEL     MSG(2)        V1cMSG
040200060925     c                   goto      endct2
040300060925    1c                   else
040400060925     c                   movel     tbluni        ds15
040500060925     c                   endif
040600060922     c* Controllo nuova nazione + cap
040700060925     c** Se il cap inserito ha la lunghezza del cap fittizio
040800060925     c*  deve esistere sul cappario, altrimenti richiamo fnlv13
040900060925     c     ' '           checkr    vvccap        posi              3 0    10
041000060925     c                   if        *in10 and posi=§15ecf
041100060925     c* Prendo la versione e chain sul cappario
041200060925     c                   eval      i96dri=dq0dde
041300060925     c                   call      'TISI96R'
041400060925     c                   parm                    i96dri            8 0
041500060925     c                   parm      00000         o96ver            5 0
041600060925     c
041700060925     c     kcpc2         chain     azcpc01l
041800060925     c                   if        not %found(azcpc01l)
041900060925     c                   seton                                        419028
042000060925     C                   MOVEL     MSG(4)        V1cMSG
042100060925     c                   goto      endct2
042200060925     c                   endif
042300060925     c
042400060925     c                   else
042500060922     c                   clear                   dslv13
042600060922     c                   clear                   dssi95
042700060922     C                   MOVEL     '3'           I95TCN
042800060925     C                   MOVEL     ' '           I13AF1
042900060925     C                   MOVEL     ' '           I13SZ2
0430000609259/12 C                   MOVEL     ' '           I13LA3
0431000609259/12 C                   MOVEL     ' '           I13SZ3
043200060922     c
043300060922     C                   MOVEL     vvccAP        I95CAP
043400060922     C                   MOVEL     vvcNAR        I95NAR
043500060922     C                   MOVEL     dq0dde        I95DAT
043600060925     C                   MOVEL     ' '           I13AF0
043700060922     C                   MOVEL     'S'           I13CNV
043800060922     C                   MOVEL     'S'           I13SZV
043900060922c
044000060922    2C     vvcCAP        IFNE      VARcapdup
044100060922     C                   CLEAR                   wfor1             1
044200060922     C                   CLEAR                   AAA
044300060922     C                   CLEAR                   AAB
044400060922     C                   CLEAR                   AAD
044500060922     C                   MOVEL     vvccap        VARCApdup
044600060922     c                   endif
044700060922     C**
044800060922     C                   MOVEL     wfor1         E13FZV
044900060922     C                   MOVEL     AAA           E13FZ1
045000060922     C                   MOVEL     AAB           E13FZ2
045100060922     C                   MOVEL     AAD           E13FZ3
045200060922     C**
045300060922     C** CALL
045400060922     C                   CALL      'FNLV13R'
045500060922     C                   PARM                    KPJBA
045600060922     C                   PARM                    DSLV13
045700060922     C                   PARM                    DSSI95
045800060922     C*
045900060922     C                   MOVEL     O13ERR        WERR              1
046000060922     C                   MOVEL     O13MSG        v1cMSG
046100060922     C* IMPOSTO CAMPI PASSATI
046200060922     C     O13RON        IFEQ      'S'
046300060922     C                   MOVEL     O95NAR        vvcNAR
046400060922     C                   ENDIF
046500060922     C     O13ROC        IFEQ      'S'
046600060922     C                   MOVEL     O95CAP        vvcCAP
046700060922     C                   ENDIF
046800060922     c
046900060922     C**
047000060922     C* REIMPOSTO LE FORZATURE
047100060922     C                   MOVEL     E13FZV        wfor1
047200060922     C                   MOVEL     E13FZ1        AAA
047300060922     C                   MOVEL     E13FZ2        AAB
047400060922     C                   MOVEL     E13FZ3        AAD
047500060922     C**
047600060922    1C     WERR          IFNE      ' '
047700060922    2C     V1cMSG        IFNE      *BLANKS
047800060922     C                   SETON                                        28
047900060922     c                   endif
048000060922     c                   seton                                        9041
048100060922     c                   goto      endct2
048200060922     c                   endif
048300060925     c                   endif
048400060925     c* Controllo se record doppio
048500060925     c     kcppdop2      setll     azcpp01l                               90
048600060925     c                   if        *in90
048700060925     c                   seton                                        4228
048800060925     C                   MOVEL     MSG(5)        V1cMSG
048900060925     c                   goto      endct2
049000060925     C                   ENDIF
049100060330     c*
049200060329     C     ENDCT2        ENDSR
049300060315     C*
049400060315     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
049500060315     C     CTRERR        BEGSR
049600060315     C* IMPOSTO CAMPI PASSATI
049700060315     C     O13RON        IFEQ      'S'
049800060315     C                   MOVEL     O95NAR        v1cNAR
049900060315     C                   ENDIF
050000060315     C     O13ROC        IFEQ      'S'
050100060315     C                   MOVEL     O95CAP        v1cCAP
050200060315     C                   ENDIF
050300060315     c
050400060315    2C                   ENDSR
050500050214     C**************************************************************************
050600060315     C*  Carico dati utente per vedere se abilitato alla gestione      cal
050700050214     C**************************************************************************
050800060315     C     CARiniz       BEGSR
050900060922     c     kcpp          klist
051000060922     c                   kfld                    DQ0nar
051100060922     c                   kfld                    DQ0cap
051200060922     c                   kfld                    DQ0ntw
051300060922     c                   kfld                    DQ0dde
051400060925     c     kcppdop       klist
051500060925     c                   kfld                    v1cnar
051600060925     c                   kfld                    v1ccap
051700060925     c                   kfld                    v1cntw
051800060925     c                   kfld                    amgdde
051900060925     c     kcppdop2      klist
052000060925     c                   kfld                    vvcnar
052100060925     c                   kfld                    vvccap
052200060925     c                   kfld                    dq0ntw
052300060925     c                   kfld                    dq0dde
052400060925     c     kcpc          klist
052500060925     c                   kfld                    o96ver
052600060925     c                   kfld                    v1cnar
052700060925     c                   kfld                    v1ccap
052800060925     c     kcpc2         klist
052900060925     c                   kfld                    o96ver
053000060925     c                   kfld                    vvcnar
053100060925     c                   kfld                    vvccap
053200060330     c     Ktab          Klist
053300060330     c                   Kfld                    codut             1 0
053400060330     c                   Kfld                    kCod
053500060330     c                   Kfld                    kKey
053600060331     c
053700060331     c                   z-add     1             codut
053800060327     c
053900060315     c                   clear                   dute01
054000060315      *
054100060315     c     *dtaara       define    §azute        azuteds
054200060315     c     *dtaara       define    §datiute      ddatiute
054300060315     c                   in(E)     *dtaara
054400060315    1c                   If        %error  or RSUT = *blanks
054500060315     c                   Clear                   Tibs34ds
054600060315     c                   Call      'TIBS34R'
054700060315     c                   Parm                    Tibs34ds
054800060315     c                   In        *dtaara
054900060315    1c                   EndIf
055000060404    cc                   movel     rsut          v1crsu
055100060315
055200060315     c                   Clear                   wabi
055300060922     c                   Clear                   DQ0msg
055400060315
055500060315     c* Vado avanti nei caricamenti non in interrogazione
055600060922    1c                   if        DQ0op1<>'O05'
055700060315     c
055800060315      * se ho errori nei dati utente esco dal pgm
055900060922w   2c                   if        DutErr = 'E'
056000060315     c                   GoTo      ENDINIZ
056100060922    3c                   endif
056200060922     c
056300060315      * se non c'è l'abilitazione
056400060315      * --> se 1° livello, abilitazioni al terminal
056500060315      *     se 2° livello, abilitazioni al punto operativo
056600060315      *     se sede è impossibile ma se capita mando a fine il pgm
056700060922w   2c                   if        UteAut = *Blanks
056800060922     c                   clear                   dute01
056900060315      * carica le abilitazioni del profilo
057000060922x  x2c                   else
057100060315     c                   Movel     UteFaf        Dute01
057200060922e   2c                   endif
057300060922e   2c                   endif
057400060315     c
057500060315     c* Data corrente
057600060315     c                   time                    w0140            14 0
057700060315     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
057800060315     c     *eur          movel     w0080         dataeur
057900060315     c                   movel     dataeur       dataiso
058000060315     c     *iso          movel     dataiso       datcor            8 0
058100060315     c
058200060315     C*
058300060315     C* VEDO QUALE OPZINE SCELTA
058400060315     c*
058500060922     C                   CLEAR                   V1Cntw
058600060922     C                   CLEAR                   V1Dntw
058700060315     C                   CLEAR                   V1cnar
058800060315     C                   CLEAR                   V1dnar
058900060922     C                   CLEAR                   V1ccap
059000060922     C                   CLEAR                   V1cuti
059100060922     C                   CLEAR                   V1cdde
059200060922     C                   CLEAR                   V1cdsc
059300061030     C                   CLEAR                   V1ctut
059400061030     C                   CLEAR                   V1ccas
059500061030     c* campo solo per duplica
059600061030     c                   CLEAR                   V1cbol
059700061030     c
059800060315     c                   setoff                                       404142
059900060922     c                   setoff                                       434401
060000060315     c* Imposto i campi
060100060922     C                   MOVEL     DQ0NAR        V1CNAR
060200060922     C                   MOVEL     DQ0CAP        V1CCAP
060300060922     C                   MOVEL     DQ0CAP        varCAP
060400060922     C                   MOVEL     DQ0ntw        V1Cntw
060500060922     c                   if        dq0uti='S'
060600060922     c                   eval      v1cuti='SI'
060700060922     c                   else
060800060922     c                   eval      v1cuti='NO'
060900060922     c                   endif
061000060404     c
061100060922     C                   IF        DQ0dde>0
061200060922     C     *ISO          MOVEL     DQ0dde        DATAISO
061300060315     C                   MOVEL     DATAISO       DATAEUR
061400060922     C     *EUR          MOVEL     DATAEUR       V1Cdde
061500060315     C                   ENDIF
061600060922     C                   IF        DQ0dsc>0
061700060922     C     *ISO          MOVEL     DQ0dsc        DATAISO
061800060922     C                   MOVEL     DATAISO       DATAEUR
061900060922     C     *EUR          MOVEL     DATAEUR       V1Cdsc
062000060922     C                   ENDIF
062100060315     C*
062200060922     c                   if        DQ0cap<>*blanks
062300060315     c                   eval      kcod='15'
062400060922     c                   movel(p)  DQ0nar        kkey
062500060315     c     ktab          chain     tabel
062600060327     c                   if        %found(tabel00f)
062700060315     c                   movel     tbluni        v1dnar
062800060315     c                   endif
062900060922     c                   endif
063000060922     c* Network
063100060922     c                   if        dq0ntw<>*blanks
063200060922     C                   MOVEL     dq0ntw        V1cntw
063300060922     C                   CLEAR                   tibs02ds
063400060922     C                   MOVEL     'C'           T02MOD
063500060922     C                   MOVEL     KNSIF         T02SIF
063600060922     C                   MOVEL     'NTW'         T02COD
063700060922     C                   MOVEL(P)  dq0ntw        T02KE1
063800060922     C                   CALL      'TIBS02R'
063900060922     C                   PARM                    KPJBA
064000060922     C                   PARM                    tibs02ds
064100060922      *
064200060922     C     T02ERR        IFNE      *BLANKS
064300060922     C                   MOVEL     *all'?'       v1dntw
064400060922     C                   ELSE
064500060927     C                   MOVEL     t02uni        v1dntw
064600060922     C                   ENDIF
064700060922     C                   ENDIF
064800061030     c* dcppflo
064900061030     c                   if        dq0flo=*blanks
065000061030     c                   eval      v1ctut='SI'
065100061030     c                   eval      v1cbol='Tutte'
065200061030     c                   else
065300061030     c                   movel     dq0flo        dcppflo
065400061030     c                   if        §cppcas='S'
065500061030     c                   eval      v1ccas='SI'
065600061030     c                   eval      v1cbol='C/Ass'
065700061030     c                   endif
065800061030     c                   endif
065900061030     c
066000060315     c* IMMISSIONE
066100060315     c                   select
066200060922     C                   WHEN      DQ0OP1='F06'
066300060315     C                   SETON                                        02
066400060315     C* MANUTENZIONE
066500060922     C                   WHEN      DQ0OP1='O02'
066600060315     C                   SETON                                        04
066700060315     C* ANNULLAMENTO
066800060922     C                   WHEN      DQ0OP1='O04'
066900060315     C                   SETON                                        0301
067000060315     C* VISULIZZA
067100060922     C                   WHEN      DQ0OP1='O05'
067200060315     C                   SETON                                        0501
067300060315     C                   ENDSL
067400060927     c*
067500060927     c* se richiamo da Cappario disabilito F7
067600060927     c                   movel     dq0op0        wchiama           1
067700060927     c                   if        wchiama='C'
067800060927     c                   seton                                        11
067900060927     c                   else
068000060927     c                   setoff                                       11
068100060927     c                   endif
068200060315     C*
068300060315     C
068400060315     C     ENDINIZ       ENDSR
068500060327     C**************************************************************************
068600060327     C*  Aggiornamento dati su azcep
068700060327     C**************************************************************************
068800060327     C     Aggiorna      BEGSR
068900060327     c* Annullamento
069000060327     c                   select
069100060922     c                   when      DQ0op1='O04'
069200060922     c     kcpp          delete    azcpp01l
069300060327     c                   if        %error
069400060922     c                   eval      DQ0msg=msg(7)
069500060922     c                   eval      DQ0err='1'
069600060327     c                   endif
069700060327     c
069800060922     c                   when      DQ0op1='O02'
069900060327     c* Visto che tutti i campi sono in key, annullo e riscrivo
070000060922     c     kcpp          delete    azcpp01l
070100060327     c                   if        %error
070200060922     c                   eval      DQ0msg=msg(8)
070300060922     c                   eval      DQ0err='1'
070400060327     c                   else
070500060327     c                   exsr      Scrivi
070600060327     c                   endif
070700060327     c
070800060330     c* Duplica
070900060922     c                   when      DQ0op1='O03'
071000060404     c                   exsr      SCRIVI
071100060922     c* Inserimento
071200060922     c                   when      DQ0op1='F06'
071300060327     c                   exsr      SCRIVI
071400060327     c                   ENDSL
071500060327     c
071600060327     c
071700060327     c                   ENDSR
071800060327     C**************************************************************************
071900060327     C*  Scrittura nuovo record
072000060327     C**************************************************************************
072100060327     C     Scrivi        BEGSR
072200060922     c                   clear                   azcpp000
072300060922     c                   if        DQ0op1='O03'
072400060922     c                   eval      cppnar=vvcnar
072500060922     c                   eval      cppcap=vvccap
072600060922     c                   eval      cppdde=dq0dde
072700060922     c                   eval      cppdsc=dq0dsc
072800061030     c                   eval      cppflo=dq0flo
072900060922     c                   else
073000060922     c                   eval      cppnar=v1cnar
073100060922     c                   eval      cppcap=v1ccap
073200060922     c                   eval      cppdde=amgdde
073300060922     c                   eval      cppdsc=amgdsc
073400061030     c*
073500061030     c                   if        v1ctut='SI'
073600061030     c                   clear                   cppflo
073700061030     c                   else
073800061030     c                   movel     v1ccas        §cppcas
073900061030     c                   movel     dcppflo       cppflo
074000060922     c                   endif
074100061030     c                   endif
074200061030     c
074300060922     c                   MOVEL     v1cuti        cpputi
074400060922     c                   eval      cppntw=v1cntw
074500060922     c                   write     azcpp000
074600060327     c                   endsr
074700060926     C**************************************************************************
074800060926     C*  richiamo int cappario per scelta
074900060926     C**************************************************************************
075000060926     C     LANPGM        BEGSR
075100060926     C* IMPOSTA LA DS DI PROCEDURA
075200060926     c                   clear                   tisi00ds
075300060926     c                   if        amgdde>0
075400060926     c                   movel     amgdde        d00dri
075500060926     c                   else
075600060926     c                   movel     datcor        d00dri
075700060926     c                   endif
075800060926     c                   if        dq0op1='F06'  or dq0op1='O03'
075900060926     C                   MOVEL     'C01'         d00OP0                         *LIVELLO PROCEDURA
076000060926     c                   else
076100060926     C                   MOVEL     'C05'         d00OP0                         *LIVELLO PROCEDURA
076200060926     c                   endif
076300060926     c
076400060926     C                   clear                   d00OP1                         *LIVELLO PGM
076500060926     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
076600060926     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
076700060926     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
076800060926     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
076900060926     C                   movel     wnar          d00NAR                         *NAZIONE
077000060926     C                   clear                   d00CAP                         *CAP
077100060926     C                   clear                   d00LOC                         *LOCALITA'
077200060926     C*
077300060926     C                   CALL      'TISI02R'
077400060926     C                   PARM                    KPJBA
077500060926     C                   PARM                    tisi00ds
077600060926     c                   ENDSR
077700020805      **************************************************************************
077800960302** SCHIERA MSG - MESSAGGI DI ERRORE
077900060922Network obbligatorio                                                           01
078000060925Nazione inesistente                                                            02
078100060922Data Errata                                                                    03
078200060925CAP fittizio inesistente sul cappario alla data decorrenza
078300060925CAP già esistente per il network/decorrenza indicati
078400061030Selezionare o TUTTE le bolle o solo quelle con C/Assegno                       06
078500060922Record al momento non annullabile: in uso!!!! Verificare e riprovare           07
078600060922Record al momento non modificabile: in uso!!!! Verificare e riprovare          08
