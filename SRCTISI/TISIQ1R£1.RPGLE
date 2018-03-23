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
003500060926     d
003600060926     d tisi00ds      e ds
003700060315     D* DS PER FNLV13R - CONTROLLO DI UN CAP
003800060315     D DSLV13        E DS                  EXTNAME(FNLV13DS)
003900060315     D* DS PER TISI95R - CONTROLLO DI CAP
004000060315     D DSSI95        E DS                  EXTNAME(TISI95DS)
004100040917     C**********************************************************************
004200060315     c* INDICATORI
004300060315     C**********************************************************************
004400060315     C* 01    - PROTEZIONE CAMPI VIDEATA
004500060315     C* 02    - IMMISSIONE
004600060315     C* 03    - ANNULLAMENTO
004700060315     C* 04    - MODIFICA
004800060315     C* 05    - INTERROGAZIONE
004900050103     C**********************************************************************
005000040917     C     *ENTRY        PLIST
005100040917     C                   PARM                    KPJBA
005200060922     C                   PARM                    tisiq0ds
005300060315     c
005400060315     c* Verifico se l'utente  abilitato alla gestione del calendario
005500060315     c                   exsr      CARINIZ
005600060922     c                   if        DQ0err='1'
005700060315     c                   goto      fine
005800060315     c                   endif
005900060329     c
006000060329     c* Duplica
006100060922     c                   if        DQ0op1='O03'
006200060329     c                   exsr      Duplica
006300060329     c                   goto      fine
006400060329     c                   endif
006500040917     c
006600040917     C     FOR01         TAG
006700000828      *
006800060922     C                   EXFMT     SIQ1d01
006900060315     C                   SETOFF                                       2890
007000060315     c                   setoff                                       404142
007100060315     c                   setoff                                       434445
007200060315     c                   setoff                                       464748
007300060330     c                   setoff                                       495152
007400060330     c                   setoff                                       535455
007500060330     c                   setoff                                       565758
007600060404     c                   setoff                                       5960
007700040917     C* F3 - FINE
007800060315     C                   IF        *INKC
007900060922     C                   EVAL      DQ0F03='1'
008000060315     C                   GOTO      FINE
008100060315     C                   ENDIF
008200060404     c
008300060315     C* F12- RITORNO
008400060315     C                   IF        *INKl
008500060922     C                   EVAL      DQ0F12='1'
008600060315     C                   GOTO      FINE
008700060315     C                   ENDIF
008800060315     c
008900060926     C* F7 - cappario
009000060926     C                   IF        *INKg
009100060926     c                   eval      wnar=v1cnar
009200060926     c                   exsr      lanpgm
009300060926     c
009400060926     c                   if        DQ0op1='F06'  and
009500060926     c                             d00cap<>*blanks
009600060926     c                   eval      v1ccap=d00cap
009700060926     c                   eval      v1cnar=d00nar
009800060926     c                   endif
009900060926     c                   goto      for01
010000060926     c*
010100060926     c                   endif
010200060926     c
010300060315      *  CONTROLLI --> non in interrogazione
010400040917     C                   EXSR      CTRD01
010500040917     c   90              goto      for01
010600000825      *
010700060315     c* Per visualizzazione, enter esce
010800060922     c                   if        DQ0op1='O05'
010900060315     c                   goto      fine
011000060315     c                   endif
011100060315     c
011200060315     c  nkf              goto      for01
011300060327     c* F06 - conferma
011400060327     c   kf              exsr      AGGIORNA
011500040917     C*
011600900515      *
011700000000     C     FINE          TAG
011800060315     C**
011900060315     C                   CLEAR                   tibs02ds
012000060315     C                   MOVEL     'C'           T02TLA
012100060315     C                   CALL      'TIBS02R'
012200060315     C                   PARM                    KPJBA
012300060922     c                   parm                    tibs02ds
012400060315     C**
012500060315     c                   clear                   dslv13
012600060315     C                   MOVEL     'C'           I13TLA
012700060315     C                   CALL      'FNLV13R'
012800060315     C                   PARM                    KPJBA
012900060315     C                   PARM                    DSLV13
013000060315     C                   PARM                    DSSI95
013100060922     c
013200000000     C                   SETON                                        LR
013300060329      *-----------------------------------------------------***********
013400060329      * Duplica
013500060329      *-----------------------------------------------------***********
013600060329     C     Duplica       BEGSR
013700060329     c
013800060926     c                   movel     dq0nar        vvcnar
013900060922     C                   write     SIQ1dw2
014000060329     c     forw2         tag
014100060922     C                   EXFMT     SIQ1dv2
014200060330     c                   setoff                                       2890
014300060330     c                   setoff                                       4041
014400060329     C* F12- RITORNO
014500060329     C                   IF        *INKl
014600060922     C                   EVAL      DQ0F12='1'
014700060329     C                   GOTO      Endduplica
014800060329     C                   ENDIF
014900060926     c
015000060926     C* F7 - cappario
015100060926     C                   IF        *INKg
015200060926     c                   eval      wnar=vvcnar
015300060926     c                   exsr      lanpgm
015400060926     c
015500060926     c                   if        d00cap<>*blanks
015600060926     c                   eval      vvccap=d00cap
015700060926     c                   eval      vvcnar=d00nar
015800060926     c                   endif
015900060926     c                   goto      forw2
016000060926     c                   endif
016100060926     c*
016200060329     c* controlli video 2
016300060329     c                   exsr      ctrw2
016400060329     c
016500060330     c  Nkf
016600060330     cor 90              goto      forw2
016700060330     c
016800060330     c   KF              exsr      Aggiorna
016900060329     c
017000060329     c     endduplica    endsr
017100900515      *-----------------------------------------------------***********
017200060329      * CONTROLLI                                           *  CTRD01 *
017300900515      *-----------------------------------------------------***********
017400040917     C     CTRD01        BEGSR
017500060315     c*
017600060922     c* Data decorrenza obbligatoria
017700060922     c                   eval      wdat=v1cdde
017800060922     c                   clear                   amgdde            8 0
017900060922     c                   exsr      CTRDAT
018000060922     c                   eval      v1cdde=wdat
018100060922     c* Errore
018200060922    1c                   if        *in90
018300060922     c                   seton                                        43
018400060922     c                   goto      endct1
018500060922    1c                   endif
018600060922     c
018700060922     c                   movel     amgdat        amgdde
018800060922     c* Data scadenza   obbligatoria
018900060922     c                   eval      wdat=v1cdsc
019000060922     c                   clear                   amgdsc            8 0
019100060922     c                   exsr      CTRDAT
019200060922     c                   eval      v1cdsc=wdat
019300060922     c* Errore
019400060922    1c                   if        *in90
019500060922     c                   seton                                        44
019600060922     c                   goto      endct1
019700060922    1c                   endif
019800060922     c
019900060922     c                   movel     amgdat        amgdsc
020000060922     c
020100060925     c* controllo la NAZIONE
020200060925     c                   eval      kcod='15'
020300060925     c                   movel(p)  v1cnar        kkey
020400060925     c     ktab          chain     tabel
020500060925     c                   if        not %found(tabel00f) or
020600060925     c                             tblflg<>' '
020700060925     c                   seton                                        409028
020800060925     C                   MOVEL     MSG(2)        V1cMSG
020900060925     c                   goto      endct1
021000060925    1c                   else
021100060925     c                   movel     tbluni        v1dnar
021200060925     c                   movel     tbluni        ds15
021300060925     c                   endif
021400060925     c
021500060922     C* CONTROLLO CAP alla data decorrenza
021600060925     c** Se il cap inserito ha la lunghezza del cap fittizio
021700060925     c*  deve esistere sul cappario, altrimenti richiamo fnlv13
021800060925     c     ' '           checkr    v1ccap        posi              3 0    10
021900060925     c                   if        *in10 and posi=§15ecf
022000060925     c* Prendo la versione e chain sul cappario
022100060925     c                   eval      i96dri=amgdde
022200060925     c                   call      'TISI96R'
022300060925     c                   parm                    i96dri            8 0
022400060925     c                   parm      00000         o96ver            5 0
022500060925     c
022600060925     c     kcpc          chain     azcpc01l
022700060925     c                   if        not %found(azcpc01l)
022800060925     c                   seton                                        419028
022900060925     C                   MOVEL     MSG(4)        V1cMSG
023000060925     c                   goto      endct1
023100060925     c                   endif
023200060925     c
023300060925     c                   else
023400060922     c                   clear                   dslv13
023500060922     c                   clear                   dssi95
023600060922     C                   MOVEL     '3'           I95TCN
023700060925     C                   MOVEL     ' '           I13AF1
023800060925     C                   MOVEL     ' '           I13SZ2
0239000609259/12 C                   MOVEL     ' '           I13LA3
0240000609259/12 C                   MOVEL     ' '           I13SZ3
024100060922     c
024200060922     C                   MOVEL     v1ccAP        I95CAP
024300060922     C                   MOVEL     v1cNAR        I95NAR
024400060922     C                   MOVEL     amgdde        I95DAT
024500060925     C                   MOVEL     ' '           I13AF0
024600060922     C                   MOVEL     'S'           I13CNV
024700060922     C                   MOVEL     'S'           I13SZV
024800060922c
024900060922    2C     v1cCAP        IFNE      VARcap
025000060922     C                   CLEAR                   wfor1
025100060922     C                   CLEAR                   AAA               1
025200060922     C                   CLEAR                   AAB               1
025300060922     C                   CLEAR                   AAD               1
025400060922     C                   MOVEL     v1ccap        VARCAp
025500060922     c                   endif
025600060922     C**
025700060922     C                   MOVEL     wfor1         E13FZV
025800060922     C                   MOVEL     AAA           E13FZ1
025900060922     C                   MOVEL     AAB           E13FZ2
026000060922     C                   MOVEL     AAD           E13FZ3
026100060925     c
026200060922     C**
026300060922     C** CALL
026400060922     C                   CALL      'FNLV13R'
026500060922     C                   PARM                    KPJBA
026600060922     C                   PARM                    DSLV13
026700060922     C                   PARM                    DSSI95
026800060922     C*
026900060922     C                   MOVEL     O13ERR        WERR              1
027000060922     C                   MOVEL     O13MSG        v1cMSG
027100060922     C                   EXSR      CTRERR
027200060922     C**
027300060922     C* REIMPOSTO LE FORZATURE
027400060922     C                   MOVEL     E13FZV        wfor1
027500060922     C                   MOVEL     E13FZ1        AAA
027600060922     C                   MOVEL     E13FZ2        AAB
027700060922     C                   MOVEL     E13FZ3        AAD
027800060922     C**
027900060922    1C     WERR          IFNE      ' '
028000060922    2C     V1cMSG        IFNE      *BLANKS
028100060922     C                   SETON                                        28
028200060922     c                   endif
028300060922     c                   seton                                        9041
028400060922     c                   goto      endct1
028500060922     c                   endif
028600060925     c                   endif
028700060922     c**
028800060922     c* Network
028900060922     c                   if        v1cntw=*blanks
029000060922     c                   seton                                        422890
029100060922     C                   MOVEL     MSG(1)        V1cMSG
029200060922     c                   goto      endct1
029300060922     c                   endif
029400060922     c
029500060922     C     '?'           SCAN      V1cntw                                 31
029600060922     C                   if        *in31
029700060922      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
029800060922     C                   SETON                                        98
029900060922     C                   CLEAR                   tibs02ds
030000060922     C                   MOVEL     'R'           T02MOD
030100060922     C                   MOVEL     KNSIF         T02SIF
030200060922     C                   MOVEL     'NTW'         T02COD
030300060922     C                   CALL      'TIBS02R'
030400060922     C                   PARM                    KPJBA
030500060922     C                   PARM                    tibs02ds
030600060922     C     T02ERR        IFEQ      *BLANKS
030700060922     C                   MOVEL     T02KE1        V1cntw
030800060922     C                   MOVEL     T02uni        V1dntw
030900060922     C                   ENDIF
031000060922     c
031100060922     c                   seton                                        4290
031200060922     c                   goto      endct1
031300060922     C                   ENDif
031400060922      *
031500060922     C                   CLEAR                   tibs02ds
031600060922     C                   MOVEL     'C'           T02MOD
031700060922     C                   MOVEL     KNSIF         T02SIF
031800060922     C                   MOVEL     'NTW'         T02COD
031900060922     C                   MOVEL     *BLANKS       T02KE1
032000060922     C                   MOVEL     v1cntw        T02KE1
032100060922     C                   CALL      'TIBS02R'
032200060922     C                   PARM                    KPJBA
032300060922     C                   PARM                    tibs02ds
032400060922     c*
032500060922      *
032600060922     C     T02ERR        IFNE      *BLANKS
032700060922     c                   seton                                        422890
032800060922     C                   MOVEL     MSG(1)        V1cMSG
032900060922     c                   goto      endct1
033000060922     C                   ENDIF
033100060927     c                   movel     t02uni        v1dntw
033200060925     c* Controllo se record doppio x immissione o manut se modificato
033300060925     c*  decorrenza o netwok
033400060925     c                   if        *in02 or
033500060925     c                             (*in04 and dq0ntw<>v1cntw) or
033600060925     c                             (*in04 and dq0dde<>amgdde)
033700060925     c     kcppdop       setll     azcpp01l                               90
033800060925     c                   if        *in90
033900060925     c                   seton                                        4228
034000060925     C                   MOVEL     MSG(5)        V1cMSG
034100060925     c                   goto      endct1
034200060925     C                   ENDIF
034300060925     C                   ENDIF
034400060315     C**
034500040917     C     ENDCT1        ENDSR
034600060330      *-----------------------------------------------------***********
034700060922      * CONTROLLo Data
034800060330      *-----------------------------------------------------***********
034900060922     c     CTRDAT        BEGSR
035000060922    1c                   if        wdat>0
035100060922     c                   movel     wdat          w002a             2
035200060922     c     *eur          test(d)                 wdat                   30
035300060331    2c                   if        *in30  and w002a='00'
035400060922     c     *dmy          test(d)                 wdat                   30
035500060331     c                   endif
035600060331     c* Errore
035700060330    3c                   if        *in30
035800060922     c                   seton                                        9028
035900060330     C                   MOVEL     MSG(3)        V1cMSG
036000060330     c                   goto      endctdev
036100060331     c                   endif
036200060330     c
036300060331     c                   if        w002a='00'
036400060922     c     *dmy          move      wdat          dataeur
036500060330     c                   else
036600060922     c     *eur          move      wdat          dataeur
036700060330     c                   endif
036800060330     c
036900060330     c                   else
037000060922     c                   seton                                        9028
037100060330     C                   MOVEL     MSG(3)        V1cMSG
037200060330     c                   goto      endctdev
037300060330     c                   endif
037400060330     c* la trasformo in AAAAAMMGG - iso
037500060922     c                   move      dataeur       wdat
037600060330     c                   movel     dataeur       dataiso
037700060922     c     *iso          movel     dataiso       amgdat            8 0
037800060330     c
037900060330     c     endctdev      ENDSR
038000060329      *-----------------------------------------------------***********
038100060329      * CONTROLLI x deplica
038200060329      *-----------------------------------------------------***********
038300060329     C     CTRw2         BEGSR
038400060925     c* controllo la NAZIONE
038500060925     c                   eval      kcod='15'
038600060925     c                   movel(p)  vvcnar        kkey
038700060925     c     ktab          chain     tabel
038800060925     c                   if        not %found(tabel00f) or
038900060925     c                             tblflg<>' '
039000060925     c                   seton                                        409028
039100060925     C                   MOVEL     MSG(2)        V1cMSG
039200060925     c                   goto      endct2
039300060925    1c                   else
039400060925     c                   movel     tbluni        ds15
039500060925     c                   endif
039600060922     c* Controllo nuova nazione + cap
039700060925     c** Se il cap inserito ha la lunghezza del cap fittizio
039800060925     c*  deve esistere sul cappario, altrimenti richiamo fnlv13
039900060925     c     ' '           checkr    vvccap        posi              3 0    10
040000060925     c                   if        *in10 and posi=§15ecf
040100060925     c* Prendo la versione e chain sul cappario
040200060925     c                   eval      i96dri=dq0dde
040300060925     c                   call      'TISI96R'
040400060925     c                   parm                    i96dri            8 0
040500060925     c                   parm      00000         o96ver            5 0
040600060925     c
040700060925     c     kcpc2         chain     azcpc01l
040800060925     c                   if        not %found(azcpc01l)
040900060925     c                   seton                                        419028
041000060925     C                   MOVEL     MSG(4)        V1cMSG
041100060925     c                   goto      endct2
041200060925     c                   endif
041300060925     c
041400060925     c                   else
041500060922     c                   clear                   dslv13
041600060922     c                   clear                   dssi95
041700060922     C                   MOVEL     '3'           I95TCN
041800060925     C                   MOVEL     ' '           I13AF1
041900060925     C                   MOVEL     ' '           I13SZ2
0420000609259/12 C                   MOVEL     ' '           I13LA3
0421000609259/12 C                   MOVEL     ' '           I13SZ3
042200060922     c
042300060922     C                   MOVEL     vvccAP        I95CAP
042400060922     C                   MOVEL     vvcNAR        I95NAR
042500060922     C                   MOVEL     dq0dde        I95DAT
042600060925     C                   MOVEL     ' '           I13AF0
042700060922     C                   MOVEL     'S'           I13CNV
042800060922     C                   MOVEL     'S'           I13SZV
042900060922c
043000060922    2C     vvcCAP        IFNE      VARcapdup
043100060922     C                   CLEAR                   wfor1             1
043200060922     C                   CLEAR                   AAA
043300060922     C                   CLEAR                   AAB
043400060922     C                   CLEAR                   AAD
043500060922     C                   MOVEL     vvccap        VARCApdup
043600060922     c                   endif
043700060922     C**
043800060922     C                   MOVEL     wfor1         E13FZV
043900060922     C                   MOVEL     AAA           E13FZ1
044000060922     C                   MOVEL     AAB           E13FZ2
044100060922     C                   MOVEL     AAD           E13FZ3
044200060922     C**
044300060922     C** CALL
044400060922     C                   CALL      'FNLV13R'
044500060922     C                   PARM                    KPJBA
044600060922     C                   PARM                    DSLV13
044700060922     C                   PARM                    DSSI95
044800060922     C*
044900060922     C                   MOVEL     O13ERR        WERR              1
045000060922     C                   MOVEL     O13MSG        v1cMSG
045100060922     C* IMPOSTO CAMPI PASSATI
045200060922     C     O13RON        IFEQ      'S'
045300060922     C                   MOVEL     O95NAR        vvcNAR
045400060922     C                   ENDIF
045500060922     C     O13ROC        IFEQ      'S'
045600060922     C                   MOVEL     O95CAP        vvcCAP
045700060922     C                   ENDIF
045800060922     c
045900060922     C**
046000060922     C* REIMPOSTO LE FORZATURE
046100060922     C                   MOVEL     E13FZV        wfor1
046200060922     C                   MOVEL     E13FZ1        AAA
046300060922     C                   MOVEL     E13FZ2        AAB
046400060922     C                   MOVEL     E13FZ3        AAD
046500060922     C**
046600060922    1C     WERR          IFNE      ' '
046700060922    2C     V1cMSG        IFNE      *BLANKS
046800060922     C                   SETON                                        28
046900060922     c                   endif
047000060922     c                   seton                                        9041
047100060922     c                   goto      endct2
047200060922     c                   endif
047300060925     c                   endif
047400060925     c* Controllo se record doppio
047500060925     c     kcppdop2      setll     azcpp01l                               90
047600060925     c                   if        *in90
047700060925     c                   seton                                        4228
047800060925     C                   MOVEL     MSG(5)        V1cMSG
047900060925     c                   goto      endct2
048000060925     C                   ENDIF
048100060330     c*
048200060329     C     ENDCT2        ENDSR
048300060315     C*
048400060315     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
048500060315     C     CTRERR        BEGSR
048600060315     C* IMPOSTO CAMPI PASSATI
048700060315     C     O13RON        IFEQ      'S'
048800060315     C                   MOVEL     O95NAR        v1cNAR
048900060315     C                   ENDIF
049000060315     C     O13ROC        IFEQ      'S'
049100060315     C                   MOVEL     O95CAP        v1cCAP
049200060315     C                   ENDIF
049300060315     c
049400060315    2C                   ENDSR
049500050214     C**************************************************************************
049600060315     C*  Carico dati utente per vedere se abilitato alla gestione      cal
049700050214     C**************************************************************************
049800060315     C     CARiniz       BEGSR
049900060922     c     kcpp          klist
050000060922     c                   kfld                    DQ0nar
050100060922     c                   kfld                    DQ0cap
050200060922     c                   kfld                    DQ0ntw
050300060922     c                   kfld                    DQ0dde
050400060925     c     kcppdop       klist
050500060925     c                   kfld                    v1cnar
050600060925     c                   kfld                    v1ccap
050700060925     c                   kfld                    v1cntw
050800060925     c                   kfld                    amgdde
050900060925     c     kcppdop2      klist
051000060925     c                   kfld                    vvcnar
051100060925     c                   kfld                    vvccap
051200060925     c                   kfld                    dq0ntw
051300060925     c                   kfld                    dq0dde
051400060925     c     kcpc          klist
051500060925     c                   kfld                    o96ver
051600060925     c                   kfld                    v1cnar
051700060925     c                   kfld                    v1ccap
051800060925     c     kcpc2         klist
051900060925     c                   kfld                    o96ver
052000060925     c                   kfld                    vvcnar
052100060925     c                   kfld                    vvccap
052200060330     c     Ktab          Klist
052300060330     c                   Kfld                    codut             1 0
052400060330     c                   Kfld                    kCod
052500060330     c                   Kfld                    kKey
052600060331     c
052700060331     c                   z-add     1             codut
052800060327     c
052900060315     c                   clear                   dute01
053000060315      *
053100060315     c     *dtaara       define    §azute        azuteds
053200060315     c     *dtaara       define    §datiute      ddatiute
053300060315     c                   in(E)     *dtaara
053400060315    1c                   If        %error  or RSUT = *blanks
053500060315     c                   Clear                   Tibs34ds
053600060315     c                   Call      'TIBS34R'
053700060315     c                   Parm                    Tibs34ds
053800060315     c                   In        *dtaara
053900060315    1c                   EndIf
054000060404    cc                   movel     rsut          v1crsu
054100060315
054200060315     c                   Clear                   wabi
054300060922     c                   Clear                   DQ0msg
054400060315
054500060315     c* Vado avanti nei caricamenti non in interrogazione
054600060922    1c                   if        DQ0op1<>'O05'
054700060315     c
054800060315      * se ho errori nei dati utente esco dal pgm
054900060922w   2c                   if        DutErr = 'E'
055000060315     c                   GoTo      ENDINIZ
055100060922    3c                   endif
055200060922     c
055300060315      * se non c'è l'abilitazione
055400060315      * --> se 1° livello, abilitazioni al terminal
055500060315      *     se 2° livello, abilitazioni al punto operativo
055600060315      *     se sede è impossibile ma se capita mando a fine il pgm
055700060922w   2c                   if        UteAut = *Blanks
055800060922     c                   clear                   dute01
055900060315      * carica le abilitazioni del profilo
056000060922x  x2c                   else
056100060315     c                   Movel     UteFaf        Dute01
056200060922e   2c                   endif
056300060922e   2c                   endif
056400060315     c
056500060315     c* Data corrente
056600060315     c                   time                    w0140            14 0
056700060315     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
056800060315     c     *eur          movel     w0080         dataeur
056900060315     c                   movel     dataeur       dataiso
057000060315     c     *iso          movel     dataiso       datcor            8 0
057100060315     c
057200060315     C*
057300060315     C* VEDO QUALE OPZINE SCELTA
057400060315     c*
057500060922     C                   CLEAR                   V1Cntw
057600060922     C                   CLEAR                   V1Dntw
057700060315     C                   CLEAR                   V1cnar
057800060315     C                   CLEAR                   V1dnar
057900060922     C                   CLEAR                   V1ccap
058000060922     C                   CLEAR                   V1cuti
058100060922     C                   CLEAR                   V1cdde
058200060922     C                   CLEAR                   V1cdsc
058300060315     c                   setoff                                       404142
058400060922     c                   setoff                                       434401
058500060315     c* Imposto i campi
058600060922     C                   MOVEL     DQ0NAR        V1CNAR
058700060922     C                   MOVEL     DQ0CAP        V1CCAP
058800060922     C                   MOVEL     DQ0CAP        varCAP
058900060922     C                   MOVEL     DQ0ntw        V1Cntw
059000060922     c                   if        dq0uti='S'
059100060922     c                   eval      v1cuti='SI'
059200060922     c                   else
059300060922     c                   eval      v1cuti='NO'
059400060922     c                   endif
059500060404     c
059600060922     C                   IF        DQ0dde>0
059700060922     C     *ISO          MOVEL     DQ0dde        DATAISO
059800060315     C                   MOVEL     DATAISO       DATAEUR
059900060922     C     *EUR          MOVEL     DATAEUR       V1Cdde
060000060315     C                   ENDIF
060100060922     C                   IF        DQ0dsc>0
060200060922     C     *ISO          MOVEL     DQ0dsc        DATAISO
060300060922     C                   MOVEL     DATAISO       DATAEUR
060400060922     C     *EUR          MOVEL     DATAEUR       V1Cdsc
060500060922     C                   ENDIF
060600060315     C*
060700060922     c                   if        DQ0cap<>*blanks
060800060315     c                   eval      kcod='15'
060900060922     c                   movel(p)  DQ0nar        kkey
061000060315     c     ktab          chain     tabel
061100060327     c                   if        %found(tabel00f)
061200060315     c                   movel     tbluni        v1dnar
061300060315     c                   endif
061400060922     c                   endif
061500060922     c* Network
061600060922     c                   if        dq0ntw<>*blanks
061700060922     C                   MOVEL     dq0ntw        V1cntw
061800060922     C                   CLEAR                   tibs02ds
061900060922     C                   MOVEL     'C'           T02MOD
062000060922     C                   MOVEL     KNSIF         T02SIF
062100060922     C                   MOVEL     'NTW'         T02COD
062200060922     C                   MOVEL(P)  dq0ntw        T02KE1
062300060922     C                   CALL      'TIBS02R'
062400060922     C                   PARM                    KPJBA
062500060922     C                   PARM                    tibs02ds
062600060922      *
062700060922     C     T02ERR        IFNE      *BLANKS
062800060922     C                   MOVEL     *all'?'       v1dntw
062900060922     C                   ELSE
063000060927     C                   MOVEL     t02uni        v1dntw
063100060922     C                   ENDIF
063200060922     C                   ENDIF
063300060315     c* IMMISSIONE
063400060315     c                   select
063500060922     C                   WHEN      DQ0OP1='F06'
063600060315     C                   SETON                                        02
063700060315     C* MANUTENZIONE
063800060922     C                   WHEN      DQ0OP1='O02'
063900060315     C                   SETON                                        04
064000060315     C* ANNULLAMENTO
064100060922     C                   WHEN      DQ0OP1='O04'
064200060315     C                   SETON                                        0301
064300060315     C* VISULIZZA
064400060922     C                   WHEN      DQ0OP1='O05'
064500060315     C                   SETON                                        0501
064600060315     C                   ENDSL
064700060927     c*
064800060927     c* se richiamo da Cappario disabilito F7
064900060927     c                   movel     dq0op0        wchiama           1
065000060927     c                   if        wchiama='C'
065100060927     c                   seton                                        11
065200060927     c                   else
065300060927     c                   setoff                                       11
065400060927     c                   endif
065500060315     C*
065600060315     C
065700060315     C     ENDINIZ       ENDSR
065800060327     C**************************************************************************
065900060327     C*  Aggiornamento dati su azcep
066000060327     C**************************************************************************
066100060327     C     Aggiorna      BEGSR
066200060327     c* Annullamento
066300060327     c                   select
066400060922     c                   when      DQ0op1='O04'
066500060922     c     kcpp          delete    azcpp01l
066600060327     c                   if        %error
066700060922     c                   eval      DQ0msg=msg(7)
066800060922     c                   eval      DQ0err='1'
066900060327     c                   endif
067000060327     c
067100060922     c                   when      DQ0op1='O02'
067200060327     c* Visto che tutti i campi sono in key, annullo e riscrivo
067300060922     c     kcpp          delete    azcpp01l
067400060327     c                   if        %error
067500060922     c                   eval      DQ0msg=msg(8)
067600060922     c                   eval      DQ0err='1'
067700060327     c                   else
067800060327     c                   exsr      Scrivi
067900060327     c                   endif
068000060327     c
068100060330     c* Duplica
068200060922     c                   when      DQ0op1='O03'
068300060404     c                   exsr      SCRIVI
068400060922     c* Inserimento
068500060922     c                   when      DQ0op1='F06'
068600060327     c                   exsr      SCRIVI
068700060327     c                   ENDSL
068800060327     c
068900060327     c
069000060327     c                   ENDSR
069100060327     C**************************************************************************
069200060327     C*  Scrittura nuovo record
069300060327     C**************************************************************************
069400060327     C     Scrivi        BEGSR
069500060922     c                   clear                   azcpp000
069600060922     c                   if        DQ0op1='O03'
069700060922     c                   eval      cppnar=vvcnar
069800060922     c                   eval      cppcap=vvccap
069900060922     c                   eval      cppdde=dq0dde
070000060922     c                   eval      cppdsc=dq0dsc
070100060922     c                   else
070200060922     c                   eval      cppnar=v1cnar
070300060922     c                   eval      cppcap=v1ccap
070400060922     c                   eval      cppdde=amgdde
070500060922     c                   eval      cppdsc=amgdsc
070600060922     c                   endif
070700060922     c                   MOVEL     v1cuti        cpputi
070800060922     c                   eval      cppntw=v1cntw
070900060922     c                   write     azcpp000
071000060327     c                   endsr
071100060926     C**************************************************************************
071200060926     C*  richiamo int cappario per scelta
071300060926     C**************************************************************************
071400060926     C     LANPGM        BEGSR
071500060926     C* IMPOSTA LA DS DI PROCEDURA
071600060926     c                   clear                   tisi00ds
071700060926     c                   if        amgdde>0
071800060926     c                   movel     amgdde        d00dri
071900060926     c                   else
072000060926     c                   movel     datcor        d00dri
072100060926     c                   endif
072200060926     c                   if        dq0op1='F06'  or dq0op1='O03'
072300060926     C                   MOVEL     'C01'         d00OP0                         *LIVELLO PROCEDURA
072400060926     c                   else
072500060926     C                   MOVEL     'C05'         d00OP0                         *LIVELLO PROCEDURA
072600060926     c                   endif
072700060926     c
072800060926     C                   clear                   d00OP1                         *LIVELLO PGM
072900060926     C                   MOVEL     '0'           d00F03                         *PREMUTO F03=FINE
073000060926     C                   MOVEL     '0'           d00F12                         *PREMUTO F12=RITORNO
073100060926     C                   MOVEL     '0'           d00ERR                         *'1'=ERRORE
073200060926     C                   MOVEL     *BLANKS       d00MSG                         *MESSAGGIO DI RITORN
073300060926     C                   movel     wnar          d00NAR                         *NAZIONE
073400060926     C                   clear                   d00CAP                         *CAP
073500060926     C                   clear                   d00LOC                         *LOCALITA'
073600060926     C*
073700060926     C                   CALL      'TISI02R'
073800060926     C                   PARM                    KPJBA
073900060926     C                   PARM                    tisi00ds
074000060926     c                   ENDSR
074100020805      **************************************************************************
074200960302** SCHIERA MSG - MESSAGGI DI ERRORE
074300060922Network obbligatorio                                                           01
074400060925Nazione inesistente                                                            02
074500060922Data Errata                                                                    03
074600060925CAP fittizio inesistente sul cappario alla data decorrenza
074700060925CAP già esistente per il network/decorrenza indicati
074800060922
074900060922Record al momento non annullabile: in uso!!!! Verificare e riprovare           07
075000060922Record al momento non modificabile: in uso!!!! Verificare e riprovare          08
