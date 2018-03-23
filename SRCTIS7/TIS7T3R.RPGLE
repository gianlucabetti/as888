000100051018      /TITLE Upload via Internet: traduzione in FNVAR00R.
000200130108     H DFTACTGRP(*no) ACTGRP(*caller)
000300990915
000400990915     Ftivin00r  uF   E             DISK    usropn
000500051108     FFNVAR00r  O    E             DISK    commit
000600990915
000700080908
000800080908     D TxtInOut        S           2048
000900080908     D ElencoChar      S            256
001000080908     D TipoElenco      S              1
001100080908     D CharSost        S              1
001200080908     D UpperCase       S              1
001300080908     D ChkNull         S              1
001400080908     D CharNull        S              1
001500080908     D EsitoBon        S              1
001600080908
001700000724     D*------------
001800051017     D tibs55ds      e ds                  inz
001900051017     D kpjba         e ds                  inz
002000990915     D psds           sds
002100990915     D  procname         *PROC
002200990921     D tivlrds       e ds                  extname(tivlr00f)
002300990915     D fnvaraar      e ds                  inz
002400990915     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915
002900990921     C                   reset                   esito
003000990921     C                   reset                   wrkesito
003100010531     C*
003200010531     C                   exsr      opeini
003300010531     C                   exsr      rwvar
003400010531     C*
003500010531     C                   seton                                        lr
003600010531
003700010531
003800010531
003900010531
004000010531     C*--------------------------------------------------------
004100010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004200010531     C*--------------------------------------------------------
004300010531     C     PREELA        BEGSR
004400010531     C*
004500010531     C* SE OCCORRE SPEDIRE IN FILIALE
004600010531     C                   if        invfil <> *zeros and
004700010531     C                             flgGiro = '0'
004800010531     C*
004900010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005000010531     C                   eval      flgGiro = '1'
005100010531     C*
005200010531     C                   endif
005300010531     C*
005400010531     C                   ENDSR
005500010531     C***
005600010531
005700010531
005800010531
005900010531
006000010531
006100010531
006200010531
006300010531
006400010531
006500010531     C*--------------------------------------------------------
006600010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006700010531     C*--------------------------------------------------------
006800010531     C     ENDELA        BEGSR
006900000616     C*
007000010531     C                   ENDSR
007100010531     C***
007200010531
007300010531
007400010531
007500010531
007600990915
007700910830     C*--------------------------------------------------------
007800941229     C* RWVAR   LEGGE FNVARAAR E SCRIVE FNVAR00R              *
007900910830     C*--------------------------------------------------------
008000941229     C     RWVAR         BEGSR
008100990915     C*
008200990915     C                   if        not %open(tivin00r)
008300990915     C                   open      tivin00r
008400990915     C                   endif
008500990915     C*
008600010601     C                   MOVE      *ZERO         §CTROK
008700010601     C                   MOVE      *ZERO         §CTRNO
008800921023     C                   DO        *HIVAL
008900010618     C*
009000990915     C                   READ      tivin00r                               70
009100030305     C*
009200030305     C* Dopo ogni lettura azzero gli indicatori di errore
009300030305     C                   SETOFF                                       31
009400010618     C*
009500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009600010618     C                   if        vinflg = '1'
009700010618     C                   eval      flgOk = '1'
009800010618     C                   endif
009900010618     C*
010000000905     C                   if        vindta > *blanks
010100990915     C*
010200010601     C                   if        *in70 = *off and
010300990915     C                             (vinflg = *blanks
010400010711     C                              or vinflg = '0'
010500010711     C                              or vinflg = '2')
010600990915     C*
010700010531     C                   clear                   FNVAR000
010800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010900010711     C                   if        vinflg = *blanks or vinflg = '0'
011000010711     C                   clear                   vinmsg
011100010711     C                   endif
011200010711     C*
011300990915     C                   eval      fnvaraar = vindta
011400991022     C*
011500991022     C                   movel     vindta        wrkuno            1
011600991022     C                   testb     x'3F'         wrkuno                   02
011700991022     C                   if        not *in02
011800010531     C*
011900010531     C                   TESTN                   VHRLNP               30
012000010531     C  N30              TESTN                   VHRLNP                 30
012100010531     C  N30              TESTN                   VHRLNP                   30
012200010531     C                   MOVE      VHRLNP        W001A             1
012300010531     C     *IN30         IFEQ      *OFF
012400010531     C     W001A         ORLT      '0'
012500010531     C                   SETON                                        31
012600010531     C                   eval      vinmsg = %trimr(vinmsg)
012700010531     C                             + ' ' + 'VHRLNP'
012800010531     C                   ELSE
012900010531     C                   MOVE      VHRLNP        VARLNP
013000010531     C     VARLNP        IFLE      *ZEROS
013100010531     C                   SETON                                        31
013200010531     C                   eval      vinmsg = %trimr(vinmsg)
013300010531     C                             + ' ' + 'VHRLNP'
013400010531     C                   ENDIF
013500010531     C                   ENDIF
013600010531     C*
013700010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013800010531     C                   eval      depfil = VARLNP
013900010531     C                   exsr      repfil
014000010601     C                   if        depfil = invfil
014100010531     C*
014200010531     C                   exsr      PREELA
014300010531     C*
014400010531     C                   TESTN                   VHRCCM               30
014500010531     C  N30              TESTN                   VHRCCM                 30
014600010531     C  N30              TESTN                   VHRCCM                   30
014700010531     C                   MOVE      VHRCCM        W001A             1
014800010531     C     *IN30         IFEQ      *OFF
014900010531     C     W001A         ORLT      '0'
015000010531     C                   SETON                                        31
015100010604     C                   eval      vinmsg = %trimr(vinmsg)
015200010604     C                             + ' ' + 'VHRCCM'
015300010531     C                   ELSE
015400010531     C                   MOVE      VHRCCM        VARCCM
015500010531     C     VARCCM        IFLE      *ZEROS
015600010531     C                   SETON                                        31
015700010604     C                   eval      vinmsg = %trimr(vinmsg)
015800010604     C                             + ' ' + 'VHRCCM'
015900010531     C                   ENDIF
016000010531     C                   ENDIF
016100010531     C*
016200130108     C                   MOVEL     datcor        VARAAS
016300010531     C                   TESTN                   VHRAAS               30
016400010531     C  N30              TESTN                   VHRAAS                 30
016500010531     C  N30              TESTN                   VHRAAS                   30
016600010531     C                   MOVE      VHRAAS        W001A             1
016700010531     C     *IN30         IFEQ      *OFF
016800010531     C     W001A         ORLT      '0'
016900010531     C                   SETON                                        31
017000010531     C                   eval      vinmsg = %trimr(vinmsg)
017100010531     C                             + ' ' + 'VHRAAS'
017200010531     C                   ELSE
017300010531     C                   MOVE      VHRAAS        VARAAS
017400010531     C     VARAAS        IFLE      *ZEROS
017500010531     C                   SETON                                        31
017600010604     C                   eval      vinmsg = %trimr(vinmsg)
017700010604     C                             + ' ' + 'VHRAAS'
017800010531     C                   ENDIF
017900010531     C                   ENDIF
018000010531     C*
018100010531     C                   TESTN                   VHRNRS               30
018200010531     C  N30              TESTN                   VHRNRS                 30
018300010531     C  N30              TESTN                   VHRNRS                   30
018400010531     C                   MOVE      VHRNRS        W001A             1
018500010531     C     *IN30         IFEQ      *OFF
018600010531     C     W001A         ORLT      '0'
018700010531     C     W001A         ANDNE     *BLANK
018800010531     C                   SETON                                        31
018900010531     C                   eval      vinmsg = %trimr(vinmsg)
019000010531     C                             + ' ' + 'VHRNRS'
019100010531     C                   ELSE
019200010531     C                   MOVE      VHRNRS        VARNRS
019300010531     C     VARNRS        IFLT      *ZEROS
019400010531     C                   SETON                                        31
019500010604     C                   eval      vinmsg = %trimr(vinmsg)
019600010604     C                             + ' ' + 'VHRNRS'
019700010531     C                   ENDIF
019800010531     C                   ENDIF
019900010531     C*
020000010531     C                   TESTN                   VHRNSP               30
020100010531     C  N30              TESTN                   VHRNSP                 30
020200010531     C  N30              TESTN                   VHRNSP                   30
020300010531     C                   MOVE      VHRNSP        W001A             1
020400010531     C     *IN30         IFEQ      *OFF
020500010531     C     W001A         ORLT      '0'
020600010531     C                   SETON                                        31
020700010531     C                   eval      vinmsg = %trimr(vinmsg)
020800010531     C                             + ' ' + 'VHRNSP'
020900010531     C                   ELSE
021000010531     C                   MOVE      VHRNSP        VARNSP
021100010531     C     VARNSP        IFLE      *ZEROS
021200010531     C                   SETON                                        31
021300010604     C                   eval      vinmsg = %trimr(vinmsg)
021400010604     C                             + ' ' + 'VHRNSP'
021500010531     C                   ENDIF
021600010531     C                   ENDIF
021700010531     C*
021800130108     C                   MOVEL     datcor        VARAGC
021900010531     C                   TESTN                   VHRAGC               30
022000010531     C  N30              TESTN                   VHRAGC                 30
022100010531     C  N30              TESTN                   VHRAGC                   30
022200010531     C                   MOVE      VHRAGC        W001A             1
022300010531     C     *IN30         IFEQ      *OFF
022400010531     C     W001A         ORLT      '0'
022500010531     C                   SETON                                        31
022600010531     C                   eval      vinmsg = %trimr(vinmsg)
022700010531     C                             + ' ' + 'VHRAGC'
022800010531     C                   ELSE
022900010531     C                   MOVE      VHRAGC        VARAGC
023000010531     C     VARAGC        IFLE      *ZEROS
023100010531     C                   SETON                                        31
023200010604     C                   eval      vinmsg = %trimr(vinmsg)
023300010604     C                             + ' ' + 'VHRAGC'
023400010531     C                   ENDIF
023500010531     C                   ENDIF
023600010531     C*
023700010531     C                   TESTN                   VHRFGC               30
023800010531     C  N30              TESTN                   VHRFGC                 30
023900010531     C  N30              TESTN                   VHRFGC                   30
024000010531     C                   MOVE      VHRFGC        W001A             1
024100010531     C     *IN30         IFEQ      *OFF
024200010531     C     W001A         ORLT      '0'
024300010531     C                   SETON                                        31
024400010531     C                   eval      vinmsg = %trimr(vinmsg)
024500010531     C                             + ' ' + 'VHRFGC'
024600010531     C                   ELSE
024700010531     C                   MOVE      VHRFGC        VARFGC
024800010531     C     VARFGC        IFLE      *ZEROS
024900010531     C                   SETON                                        31
025000010531     C                   eval      vinmsg = %trimr(vinmsg)
025100010531     C                             + ' ' + 'VHRFGC'
025200010531     C                   ENDIF
025300010531     C                   ENDIF
025400010531     C*
025500010531     C                   TESTN                   VHRNGC               30
025600010531     C  N30              TESTN                   VHRNGC                 30
025700010531     C  N30              TESTN                   VHRNGC                   30
025800010531     C                   MOVE      VHRNGC        W001A             1
025900010531     C     *IN30         IFEQ      *OFF
026000010531     C     W001A         ORLT      '0'
026100010531     C                   SETON                                        31
026200010531     C                   eval      vinmsg = %trimr(vinmsg)
026300010531     C                             + ' ' + 'VHRNGC'
026400010531     C                   ELSE
026500010531     C                   MOVE      VHRNGC        VARNGC
026600010531     C     VARNGC        IFLE      *ZEROS
026700010531     C                   SETON                                        31
026800010604     C                   eval      vinmsg = %trimr(vinmsg)
026900010604     C                             + ' ' + 'VHRNGC'
027000010531     C                   ENDIF
027100010531     C                   ENDIF
027200010531     C*
027300010531     C                   TESTN                   VHRNPR               30
027400010531     C  N30              TESTN                   VHRNPR                 30
027500010531     C  N30              TESTN                   VHRNPR                   30
027600010531     C                   MOVE      VHRNPR        W001A             1
027700010531     C     *IN30         IFEQ      *OFF
027800010531     C     W001A         ORLT      '0'
027900010531     C                   SETON                                        31
028000010531     C                   eval      vinmsg = %trimr(vinmsg)
028100010531     C                             + ' ' + 'VHRNPR'
028200010531     C                   ELSE
028300010531     C                   MOVE      VHRNPR        VARNPR
028400010531     C     VARNPR        IFLT      *ZEROS
028500010531     C                   SETON                                        31
028600010604     C                   eval      vinmsg = %trimr(vinmsg)
028700010604     C                             + ' ' + 'VHRNPR'
028800010531     C                   ENDIF
028900010531     C                   ENDIF
029000010531     C*
029100941229     C                   MOVE      VHRDMC        VARDMC
029200010531     C*
029300010531     C                   TESTN                   VHRDTR               30
029400010531     C  N30              TESTN                   VHRDTR                 30
029500010531     C  N30              TESTN                   VHRDTR                   30
029600010531     C                   MOVE      VHRDTR        W001A             1
029700010531     C     *IN30         IFEQ      *OFF
029800010531     C     W001A         ORLT      '0'
029900010531     C                   SETON                                        31
030000010531     C                   eval      vinmsg = %trimr(vinmsg)
030100010531     C                             + ' ' + 'VHRDTR'
030200010531     C                   ELSE
030300010531     C                   MOVE      VHRDTR        VARDTR
030400010531     C     VARDTR        IFLT      *ZEROS
030500010531     C                   SETON                                        31
030600010604     C                   eval      vinmsg = %trimr(vinmsg)
030700010604     C                             + ' ' + 'VHRDTR'
030800010531     C                   ENDIF
030900010531     C                   ENDIF
031000010531     C*
031100010531     C                   TESTN                   VHRDRD               30
031200010531     C  N30              TESTN                   VHRDRD                 30
031300010531     C  N30              TESTN                   VHRDRD                   30
031400010531     C                   MOVE      VHRDRD        W001A             1
031500010531     C     *IN30         IFEQ      *OFF
031600010531     C     W001A         ORLT      '0'
031700010531     C                   SETON                                        31
031800010531     C                   eval      vinmsg = %trimr(vinmsg)
031900010531     C                             + ' ' + 'VHRDRD'
032000010531     C                   ELSE
032100010531     C                   MOVE      VHRDRD        VARDRD
032200010531     C     VARDRD        IFLT      *ZEROS
032300010531     C                   SETON                                        31
032400010604     C                   eval      vinmsg = %trimr(vinmsg)
032500010604     C                             + ' ' + 'VHRDRD'
032600010531     C                   ENDIF
032700010531     C                   ENDIF
032800080908     C*
032900080908     C* "bonifico" i dati di input da eventuali caratteri indesiderati
033000080908     C                   exsr      exeBon
033100010601     C*
033200010601     C  N31              ADD       1             §CTROK            7 0
033300010601     C   31              ADD       1             §CTRNO            7 0
033400010601     C  N31              WRITE     FNVAR000
033500010601     C*
033600010601     C                   if        *in31 = *off
033700010601     C                   eval      vinflg = '1'
033800010601     C                   else
033900010601     C                   eval      vinflg = '2'
034000010601     C                   endif
034100010601     C                   endif
034200010601     C                   endif
034300010604     C*
034400010601     C                   endif
034500990915     C*
034600000905     C                   else
034700000905     C                   eval      vinflg = '1'
034800990915     C                   endif
034900000905     C*
035000000905     C  N70              update    tivin000
035100991022     C*
035200991022     C  N70              ENDdo
035300990915     C*
035400010531     C*
035500010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035600010531     C                   if        cntNonEl = *zeros or
035700010531     C                             flgMulti = '0'
035800010601     C* Se non ci sono record con errori ...
035900010601     C                   if        §ctrno = 0 and
036000010601     C                             flgStato <> '2'
036100010601     C* ... restituisco esito OK.
036200010601     C                   eval      wrkesito = '0'
036300010601     C                   else
036400010601     C                   if        §ctrok > 0
036500010601     C                   eval      wrkesito = '1'
036600010601     C                   else
036700010615     C                   if        flgOk = '0'
036800010615     C                   eval      wrkesito = '2'
036900010615     C                   else
037000010615     C                   eval      wrkesito = '6'
037100010615     C                   endif
037200010601     C                   endif
037300010601     C                   endif
037400010531     C                   else
037500010531     C                   eval      wrkesito = '9'
037600010531     C                   endif
037700990915     C*
037800990915     C                   if        %open(tivin00r)
037900990915     C                   close     tivin00r
038000990915     C                   endif
038100010531     C*
038200010531     C                   if        vlrpoi <> 999
038300010531     C                   eval      invfil = vlrpoi
038400010531     C                   endif
038500990921     C*
038600010531     C                   if        invfil > *zeros
038700051108     C                   COMMIT
038800990921     C                   exsr      invio
038900000710     C                   endif
039000010612     C*
039100010612     C                   if        flgGiro = '1'
039200010612     C                   exsr      endela
039300010612     C                   endif
039400010531     C*
039500010531     C                   ENDSR
039600010531     C***
039700010531
039800010531
039900010531
040000010531
040100010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040200010531     C     repfil        BEGSR
040300010531     C*
040400010531     C                   if        invfil = *zeros and
040500010531     C                             depfil > *zeros and
040600010613     C                             (vinflg = *blanks or
040700010613     C                              vinflg = *zeros)
040800010531     C
040900010531     C                   eval      invfil = depfil
041000010531     C                   endif
041100010531     C*
041200010531     C                   if        depfil <> invfil and
041300010531     C                             invfil > *zeros
041400010531     C                   eval      flgMulti = '1'
041500010531     C                   if        vinflg = *blanks
041600010531     C                   add       1             cntNonEl
041700010531     C                   endif
041800010531     C                   endif
041900010531     C*
042000010531     C                   if        vinflg = '2'
042100010531     C                   eval      flgStato = '2'
042200010531     C                   endif
042300010531     C*
042400010531     C                   ENDSR
042500010531     C***
042600010531
042700010531
042800010531
042900010531
043000990921
043100990921      /TITLE Invio dei dati al punto operativo.
043200990921     C     invio         begsr
043300990921     C*
043400051017     C                   clear                   tibs55ds
043500051017     C                   move      invfil        i50apo
043600051017     C                   call(e)   'TIBS55R'
043700051017     C                   parm                    tibs55ds
043800051017     C*
043900051018     C                   if        %error or
044000051017     C                             o50err <> *blanks
044100051017     C                   eval      wrkesito = '3'
044200051017     C                   else
044300051018     C*
044400051018     C* Imposto i parametri fondamentali: azione e S.I. in cui sottomettere
044500051018     C                   clear                   KPJBA
044600051018     C                   movel     'LG95'        KCOAZ
044700051018     C                   movel     o50asi        KNSIF
044800051018     C*
044900051018     C* Imposto altri parametri affinchè nn si spacchi la sottomissinoe del job
045000051018     C                   movel     'EDPCEDXXX'   KNMUS
045100051018     C                   z-add     *zeros        KNRAZ
045200051018     C                   movel     'DSP01'       KNMTD
045300051018     C                   z-add     *zeros        KNRVJ
045400051018     C                   z-add     *zeros        KNRJO
045500051018     C                   movel     'N'           KSTJO
045600051018     C                   z-add     *zeros        KTMIN
045700051018     C                   movel     'B'           KTPAZ
045800051018     C                   movel     'RICEZ_VAR'   KNMEB
045900051018     C                   z-add     *zeros        KNMSB
046000051018     C                   z-add     *zeros        KKEYF
046100051018     C                   z-add     *zeros        KRCDS
046200051018     C                   movel     'J'           KEXCN
046300051018     C                   movel     'P5'          KCOJB
046400051018     C***                movel     'KEDI'        KQIEB
046500051018     C***                movel     'KEDI'        KQEBI
046600051018     C                   movel     'N'           KCANC
046700051108     C***                call      'BCH10'
046800051108     C***                parm                    KPJBA
046900051108     C                   call      'FNLG95C2'
047000051108     C                   parm                    KPJBA
047100051017     C                   endif
047200990921     C*
047300990921     C                   endsr
047400010531
047500010531
047600010531
047700010531
047800010531      /TITLE Invio dei dati al punto operativo.
047900010531     C     opeini        BEGSR
048000010531     C*
048100010531     C* Inizializzo flag e contatori operativi
048200010531     C                   movel     '0'           flgGiro           1
048300010531     C                   movel     '0'           flgMulti          1
048400010531     C                   movel     '1'           flgStato          1
048500010615     C                   movel     '0'           flgOk             1
048600010531     C                   z-add     *zeros        cntNonEl         10 0
048700010531     C                   z-add     *zeros        depfil            3 0
048800010531     C                   z-add     *zeros        invfil            3 0
048900030407     C*
049000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
049100040311     C                   if        %subst(vlrppt:1:1) = 'S'
049200040311     C                   seton                                        50
049300030407     C                   else
049400040311     C                   setoff                                       50
049500030407     C                   endif
049600010531     C*
049700010531     C                   ENDSR
049800010531     C***
049900051108
050000051108
050100051108
050200051108      /TITLE Gestione errore generico
050300051108     C     *pssr         BEGSR
050400051108     C*
050500051108     C* Se errore => emetto operazione d ROLLBACK
050600051108     C                   ROLBK
050700051108     C*
050800051108     C                   ENDSR     '*CANCL'
050900080908     C***
051000010531
051100080908
051200080908
051300080908      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
051400080908     C     exeBon        BEGSR
051500080908     C*
051600080908     C* Imposto i parametri x il *pgm d "bonifica"
051700080908     C                   EVAL      ElencoChar = *blanks
051800080908     C                   EVAL      TipoElenco = *blanks
051900080908     C                   EVAL      CharSost   = *blanks
052000080908     C                   EVAL      UpperCase  = '1'
052100080908     C                   EVAL      ChkNull    = '1'
052200080908     C                   EVAL      CharNull   = *blanks
052300080908     C*
052400080908     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VARDMC
052500080908     C                   EVAL      TxtInOut   = VARDMC
052600080908     C                   EVAL      EsitoBon   = *blanks
052700080908     C*
052800080908     C                   CALL      'XCHKCHAR'
052900080908     C                   PARM                    TxtInOut
053000080908     C                   PARM                    ElencoChar
053100080908     C                   PARM                    TipoElenco
053200080908     C                   PARM                    CharSost
053300080908     C                   PARM                    UpperCase
053400080908     C                   PARM                    ChkNull
053500080908     C                   PARM                    CharNull
053600080908     C                   PARM                    EsitoBon
053700080908     C*
053800080908     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
053900080908     C                   IF        EsitoBon = '1'
054000080908     C                   EVAL      VARDMC = TxtInOut
054100080908     C                   ENDIF
054200080908     C*
054300080908     C                   ENDSR
054400010531
054500080908
054600990915
054700000613     C     *inzsr        BEGSR
054800990915     C*
054900990915     C     *entry        plist
055000990921     C                   parm                    tivlrds
055100990921     C                   parm      wrkesito      esito
055200000724     C                   parm                    prmlit
055300000710     C                   parm                    prmfir
055400000613     C*
055500130108     C                   z-add     *zeros        datcor            8 0
055600130108     C                   eval      datcor = %dec(%date() : *ISO)
055700130108     C*
055800000613     C                   ENDSR
055900000613     C***
