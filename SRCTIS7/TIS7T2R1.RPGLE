000100020916      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200130116     H dftactgrp(*no) actgrp(*caller)
000300021014
000400990914     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVADwwr  O    E             DISK
000600990914
000700000724     D*------------
000800990921     D dscmz         e ds                  inz
000900990914     D psds           sds
001000990914     D  procname         *PROC
001100990914     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200990914     D* 10+3
001300990914     D                 DS
001400990914     D  WS15                   1      1
001500990914     D  WI15                   2     11
001600990914     D  WV15                  12     12
001700990914     D  WD15                  13     15
001800990914     D  WASC15                 1     15
001900990914     D*  6+1
002000990914     D                 DS
002100990914     D  WS09                   1      1
002200990914     D  WI09                   2      7
002300990914     D  WV09                   8      8
002400990914     D  WD09                   9      9
002500990914     D  WASC09                 1      9
002600990914     D*  2+3
002700990914     D                 DS
002800990914     D  WS07                   1      1
002900990914     D  WI07                   2      3
003000990914     D  WV07                   4      4
003100990914     D  WD07                   5      7
003200990914     D  WASC07                 1      7
003300990921     D tivlrds       e ds                  extname(tivlr00f)
003400990914     D fnvadaar      e ds                  inz
003500990914     D esito           s              1
003600000724     D prmlit          s             10
003700000710     D prmfir          s             10
003800990921     D wrkesito        s                   like(esito)
003900000613     D rrnum           s              6  0 INZ(*zeros)
004000010606     D*------------------
004100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200010606     D*------------------
004300010606     D WLBDA8          DS                  INZ
004400010606     D  G08DAT                 1      8  0
004500010606     D  G08INV                 9     16  0
004600010606     D  G08ERR                17     17
004700010606     D  G08TGI                18     22  0
004800010606     D*
004900051212
005000051212     D TxtInOut        S           2048
005100051212     D ElencoChar      S            256
005200051212     D TipoElenco      S              1
005300051212     D CharSost        S              1
005400051212     D UpperCase       S              1
005500051212     D ChkNull         S              1
005600051212     D CharNull        S              1
005700051212     D EsitoBon        S              1
005800010606
005900010606
006000010606
006100990914
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500010531     C*
006600010531     C                   exsr      opeini
006700010531     C                   exsr      rwvad
006800010531     C*
006900010531     C                   seton                                        lr
007000010531
007100010531
007200010531
007300010531
007400010531     C*--------------------------------------------------------
007500010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007600010531     C*--------------------------------------------------------
007700010531     C     PREELA        BEGSR
007800000724     C*
007900000724     C* SE OCCORRE SPEDIRE IN FILIALE
008000010531     C                   if        invfil <> *zeros and
008100010531     C                             flgGiro = '0'
008200010531     C*
008300010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008400010531     C                   eval      flgGiro = '1'
008500010531     C*
008600010531     C                   endif
008700010531     C*
008800010531     C                   ENDSR
008900010531     C***
009000010531
009100010531
009200010531
009300010531
009400010531
009500010531
009600010531
009700010531     C*--------------------------------------------------------
009800010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009900010531     C*--------------------------------------------------------
010000010531     C     ENDELA        BEGSR
010100000616     C*
010200010531     C                   ENDSR
010300010531     C***
010400990914
010500010531
010600010531
010700010531
010800010531
010900990914     C*--------------------------------------------------------
011000020916     C* RWVAD   LEGGE FNVADAAR E SCRIVE FIVAD00F              *
011100990914     C*--------------------------------------------------------
011200990914     C     RWVAD         BEGSR
011300990914     C*
011400990914     C                   if        not %open(tivin00r)
011500990914     C                   open      tivin00r
011600990914     C                   endif
011700020916     C                   if        not %open(fivadwwr)
011800020916     C                   open      fivadwwr
011900990914     C                   endif
012000990914     C*
012100990914     C                   clear                   §CTDOK
012200990914     C                   clear                   §CTDNO
012300990914     C*
012400990914     C                   DO        *HIVAL
012500010618     C*
012600990914     C                   READ      tivin00r                               70
012700010618     C*
012800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012900010618     C                   if        vinflg = '1'
013000010618     C                   eval      flgOk = '1'
013100010618     C                   endif
013200010618     C*
013300000905     C                   if        vindta > *blanks
013400000613     C                   add       1             rrnum
013500990914     C*
013600010601     C                   if        *in70 = *off and
013700990914     C                             (vinflg = *blanks
013800010711     C                              or vinflg = '0'
013900010711     C                              or vinflg = '2')
014000990914     C*
014100020916     C                   clear                   FIVAD000
014200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014300010711     C                   if        vinflg = *blanks or vinflg = '0'
014400010711     C                   clear                   vinmsg
014500010711     C                   endif
014600010711     C*
014700990914     C                   eval      fnvadaar = vindta
014800991022     C*
014900991022     C                   testb     x'3F'         vhdatb                   02
015000991022     C                   if        not *in02
015100991022     C*
015200990914     C                   SETOFF                                       303132
015300990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
015400010531     C*
015500010531     C                   TESTN                   VHDLNP               30
015600010531     C  N30              TESTN                   VHDLNP                 30
015700010531     C  N30              TESTN                   VHDLNP                   30
015800010531     C                   MOVE      VHDLNP        W001A             1
015900010531     C     *IN30         IFEQ      *OFF
016000010531     C     W001A         ORLT      '0'
016100010531     C                   SETON                                        31
016200010531     C                   eval      vinmsg = %trimr(vinmsg)
016300010531     C                             + ' ' + 'VHDLNP'
016400010531     C                   ELSE
016500010531     C                   MOVE      VHDLNP        VADLNP
016600010531     C     VADLNP        IFLE      *ZEROS
016700010531     C                   SETON                                        31
016800010531     C                   eval      vinmsg = %trimr(vinmsg)
016900010531     C                             + ' ' + 'VHDLNP'
017000010531     C                   ENDIF
017100010531     C                   ENDIF
017200010531     C*
017300010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017400010531     C                   eval      depfil = VADLNP
017500010531     C                   exsr      repfil
017600010601     C                   if        depfil = invfil
017700021014     C                   if        vlrpoi = 999
017800021010     C                   MOVE(p)   invfil        VADFGS
017900021014     C                   else
018000021014     C                   MOVE(p)   vlrpoi        VADFGS
018100021014     C                   endif
018200010531     C*
018300010531     C                   exsr      PREELA
018400010531     C*
018500990914     C                   TESTN                   VHDCCM               30
018600990914     C  N30              TESTN                   VHDCCM                 30
018700990914     C  N30              TESTN                   VHDCCM                   30
018800990914     C                   MOVE      VHDCCM        W001A             1
018900990914     C     *IN30         IFEQ      *OFF
019000990914     C     W001A         ORLT      '0'
019100990914     C                   SETON                                        31
019200010604     C                   eval      vinmsg = %trimr(vinmsg)
019300010604     C                             + ' ' + 'VHDCCM'
019400990914     C                   ELSE
019500990914     C                   MOVE      VHDCCM        VADCCM
019600990914     C     VADCCM        IFLE      *ZEROS
019700990914     C                   SETON                                        31
019800010604     C                   eval      vinmsg = %trimr(vinmsg)
019900010604     C                             + ' ' + 'VHDCCM'
020000990914     C                   ENDIF
020100990914     C                   ENDIF
020200010601     C*
020300010601     C                   TESTN                   VHDAAS               30
020400010601     C  N30              TESTN                   VHDAAS                 30
020500010601     C  N30              TESTN                   VHDAAS                   30
020600010601     C                   MOVE      VHDAAS        W001A             1
020700010601     C     *IN30         IFEQ      *OFF
020800010601     C     W001A         ORLT      '0'
020900010601     C                   SETON                                        32
021000010606     C                   MOVEL     datcor        VADAAS
021100010601     C                   eval      vinmsg = %trimr(vinmsg)
021200010601     C                             + ' ' + 'VHDAAS'
021300010601     C                   ELSE
021400010601     C                   MOVE      VHDAAS        VADAAS
021500010601     C                   ENDIF
021600010601     C*
021700010601     C                   TESTN                   VHDNRS               30
021800010601     C  N30              TESTN                   VHDNRS                 30
021900010601     C  N30              TESTN                   VHDNRS                   30
022000010601     C                   MOVE      VHDNRS        W001A             1
022100010601     C     *IN30         IFEQ      *OFF
022200010601     C     W001A         ORLT      '0'
022300010601     C     W001A         ANDNE     *BLANK
022400010601     C                   SETON                                        32
022500010601     C                   MOVE      *ZEROS        VADNRS
022600010601     C                   eval      vinmsg = %trimr(vinmsg)
022700010601     C                             + ' ' + 'VHDNRS'
022800010601     C                   ELSE
022900010601     C                   MOVE      VHDNRS        VADNRS
023000010601     C                   ENDIF
023100010601     C*
023200010601     C                   TESTN                   VHDNSP               30
023300010601     C  N30              TESTN                   VHDNSP                 30
023400010601     C  N30              TESTN                   VHDNSP                   30
023500010601     C                   MOVE      VHDNSP        W001A             1
023600010601     C     *IN30         IFEQ      *OFF
023700010601     C     W001A         ORLT      '0'
023800010601     C                   SETON                                        32
023900010601     C                   MOVE      *ZEROS        VADNSP
024000010601     C                   eval      vinmsg = %trimr(vinmsg)
024100010601     C                             + ' ' + 'VHDNSP'
024200010601     C                   ELSE
024300010601     C                   MOVE      VHDNSP        VADNSP
024400010601     C                   ENDIF
024500010601 FG  C*
024600010601 xxx C                   IF        VHDNCL = *blanks and
024700010601 "   C                             VADNRS > *zeros
024800010601 "   C                   MOVE      '00001'       VHDNCL
024900010601 FG  C                   ENDIF
025000010601     C*
025100010601     C                   TESTN                   VHDNCL               30
025200010601     C  N30              TESTN                   VHDNCL                 30
025300010601     C  N30              TESTN                   VHDNCL                   30
025400010601     C                   MOVE      VHDNCL        W001A             1
025500010601     C     *IN30         IFEQ      *OFF
025600010601     C     W001A         ORLT      '0'
025700010601     C                   SETON                                        32
025800010601     C                   MOVE      *ZEROS        VADNCL
025900010601     C                   eval      vinmsg = %trimr(vinmsg)
026000010601     C                             + ' ' + 'VHDNCL'
026100010601     C                   ELSE
026200010601     C                   MOVE      VHDNCL        VADNCL
026300010601     C                   ENDIF
026400010601     C*
026500010601     C                   TESTN                   VHDNCD               30
026600010601     C  N30              TESTN                   VHDNCD                 30
026700010601     C  N30              TESTN                   VHDNCD                   30
026800010601     C                   MOVE      VHDNCD        W001A             1
026900010601     C     *IN30         IFEQ      *OFF
027000010601     C     W001A         ORLT      '0'
027100010601     C     W001A         ANDNE     *BLANK
027200010601     C                   SETON                                        32
027300010601     C                   MOVE      *ZEROS        VADNCD
027400010601     C                   eval      vinmsg = %trimr(vinmsg)
027500010601     C                             + ' ' + 'VHDNCD'
027600010601     C                   ELSE
027700010601     C                   MOVE      VHDNCD        VADNCD
027800010601     C                   ENDIF
027900010601     C*
028000010601     C                   TESTN                   VHDNCA               30
028100010601     C  N30              TESTN                   VHDNCA                 30
028200010601     C  N30              TESTN                   VHDNCA                   30
028300010601     C                   MOVE      VHDNCA        W001A             1
028400010601     C     *IN30         IFEQ      *OFF
028500010601     C     W001A         ORLT      '0'
028600010601     C     W001A         ANDNE     *BLANK
028700010601     C                   SETON                                        32
028800010601     C                   MOVE      *ZEROS        VADNCA
028900010601     C                   eval      vinmsg = %trimr(vinmsg)
029000010601     C                             + ' ' + 'VHDNCA'
029100010601     C                   ELSE
029200010601     C                   MOVE      VHDNCA        VADNCA
029300010601     C                   ENDIF
029400010601     C*
029500010601     C* ALFABETICI
029600010601     C                   MOVE      VHDATB        VADATB
029700010601     C                   MOVE      VHDCDP        VADCDP
029800051212
029900051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
030000051212     c                   exsr      exeBon
030100051212
030200010601     C*
030300010604     C  N31              ADD       1             §CTDOK            7 0
030400010601     C   32
030500010604     COR 31              ADD       1             §CTDNO            7 0
030600020916     C  N31              WRITE     FIVAD000
030700010601     C*
030800010601     C                   if        *in31 = *off and
030900010601     C                             *in32 = *off
031000990914     C                   eval      vinflg = '1'
031100990914     C                   else
031200990914     C                   eval      vinflg = '2'
031300990914     C                   endif
031400000905     C                   endif
031500000905     C                   endif
031600010601     C*
031700010601     C                   endif
031800000905     C*
031900000905     C                   else
032000000905     C                   eval      vinflg = '1'
032100000905     C                   endif
032200000905     C*
032300000905     C  N70              update    tivin000
032400991022     C*
032500991022     C  N70              ENDdo
032600010531     C
032700010531     C*
032800010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032900010531     C                   if        cntNonEl = *zeros or
033000010531     C                             flgMulti = '0'
033100990914     C* Se non ci sono record con errori ...
033200010531     C                   if        §ctdno = 0 and
033300010531     C                             flgStato <> '2'
033400990914     C* ... restituisco esito OK.
033500990921     C                   eval      wrkesito = '0'
033600990914     C                   else
033700000710     C                   if        §ctdok > 0
033800990921     C                   eval      wrkesito = '1'
033900000710     C                   else
034000010615     C                   if        flgOk = '0'
034100010615     C                   eval      wrkesito = '2'
034200010615     C                   else
034300010615     C                   eval      wrkesito = '6'
034400010615     C                   endif
034500990914     C                   endif
034600000710     C                   endif
034700010531     C                   else
034800010531     C                   eval      wrkesito = '9'
034900010531     C                   endif
035000990914     C*
035100990914     C                   if        %open(tivin00r)
035200990914     C                   close     tivin00r
035300990914     C                   endif
035400020916     C                   if        %open(fivadwwr)
035500020916     C                   close     fivadwwr
035600990914     C                   endif
035700010531     C*
035800010531     C                   if        vlrpoi <> 999
035900010531     C                   eval      invfil = vlrpoi
036000010531     C                   endif
036100990914     C*
036200990921     C                   if        §ctdok > 0
036300010531     C                             and invfil > *zeros
036400990921     C                   exsr      invio
036500990921     C                   endif
036600010612     C*
036700010612     C                   if        flgGiro = '1'
036800010612     C                   exsr      endela
036900010612     C                   endif
037000990921     C*
037100990914     C                   ENDSR
037200000613     C***
037300990921
037400010531
037500010531
037600010531
037700010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
037800010531     C     repfil        BEGSR
037900010531     C*
038000010531     C                   if        invfil = *zeros and
038100010531     C                             depfil > *zeros and
038200010613     C                             (vinflg = *blanks or
038300010613     C                              vinflg = *zeros)
038400010531     C
038500010531     C                   eval      invfil = depfil
038600010531     C                   endif
038700010531     C*
038800010531     C                   if        depfil <> invfil and
038900010531     C                             invfil > *zeros
039000010531     C                   eval      flgMulti = '1'
039100010531     C                   if        vinflg = *blanks
039200010531     C                   add       1             cntNonEl
039300010531     C                   endif
039400010531     C                   endif
039500010531     C*
039600010531     C                   if        vinflg = '2'
039700010531     C                   eval      flgStato = '2'
039800010531     C                   endif
039900010531     C*
040000010531     C                   ENDSR
040100010531     C***
040200010531
040300010531
040400010531
040500010531
040600010531
040700990921      /TITLE Invio dei dati al punto operativo.
040800000613     C     invio         BEGSR
040900990921     C*
041000990921     C                   reset                   dscmz
041100010531     C                   move      invfil        cmzdst
041200990921     C                   eval      cmzfld = vlrfou
041300990921     C                   eval      cmzmbd = vlrhdl
041400990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
041500000710     C                   if        prmfir = *blanks
041600020916     C                   eval      cmzfla = 'FIVAD00F'
041700020916     C                   eval      cmzmba = 'FIVAD00F'
041800000710     C                   else
041900000710     C                   eval      cmzfla = prmfir
042000000710     C                   eval      cmzmba = prmfir
042100000710     C                   endif
042200990921     C                   eval      cmznrr = *zeros
042300990921     C                   move      §ctdok        cmznrr
042400021003     C                   eval      cmzlba = vlrfl1
042500990921     C                   call(e)   'TIS711C'
042600990921     C                   parm                    dscmz
042700990921     C                   parm      *blanks       esito
042800990921     C                   if        %error
042900990921     C                             or cmzerr = '1'
043000990921     C                             or esito  = '1'
043100000710     C                   eval      wrkesito = '3'
043200990921     C                   endif
043300990921     C*
043400000613     C                   ENDSR
043500000613     C***
043600990914
043700010531
043800010531
043900010531
044000010531      /TITLE Invio dei dati al punto operativo.
044100010531     C     opeini        BEGSR
044200010531     C*
044300010531     C* Inizializzo flag e contatori operativi
044400010531     C                   movel     '0'           flgGiro           1
044500010531     C                   movel     '0'           flgMulti          1
044600010531     C                   movel     '1'           flgStato          1
044700010615     C                   movel     '0'           flgOk             1
044800010531     C                   z-add     *zeros        cntNonEl         10 0
044900010531     C                   z-add     *zeros        depfil            3 0
045000010531     C                   z-add     *zeros        invfil            3 0
045100030407     C*
045200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
045300030407     C                   if        %subst(vlrppt:1:1) = 'S'
045400030407     C                   seton                                        50
045500030407     C                   else
045600030407     C                   setoff                                       50
045700030407     C                   endif
045800010531     C*
045900010531     C                   ENDSR
046000010531     C***
046100010531
046200051212
046300051212
046400051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
046500051212     C     exeBon        BEGSR
046600051212     C*
046700051212     C* Imposto i parametri x il *pgm d "bonifica"
046800051212     C                   EVAL      ElencoChar = *blanks
046900051212     C                   EVAL      TipoElenco = *blanks
047000051212     C                   EVAL      CharSost   = *blanks
047100051212     C                   EVAL      UpperCase  = '1'
047200051212     C                   EVAL      ChkNull    = '1'
047300051212     C                   EVAL      CharNull   = *blanks
047400051212     C*
047500051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
047600051212     C                   EVAL      TxtInOut   = VADCDP
047700051212     C                   EVAL      EsitoBon   = *blanks
047800051212     C*
047900051212     C                   CALL      'XCHKCHAR'
048000051212     C                   PARM                    TxtInOut
048100051212     C                   PARM                    ElencoChar
048200051212     C                   PARM                    TipoElenco
048300051212     C                   PARM                    CharSost
048400051212     C                   PARM                    UpperCase
048500051212     C                   PARM                    ChkNull
048600051212     C                   PARM                    CharNull
048700051212     C                   PARM                    EsitoBon
048800051212     C*
048900051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
049000051212     C                   IF        EsitoBon = '1'
049100051212     C                   EVAL      VADCDP = TxtInOut
049200051212     C                   ENDIF
049300051212     C*
049400051212     C                   ENDSR
049500051212
049600010531
049700010531
049800010531
049900000613     C     *inzsr        BEGSR
050000990914     C*
050100990914     C     *entry        plist
050200990921     C                   parm                    tivlrds
050300990921     C                   parm      wrkesito      esito
050400000724     C                   parm                    prmlit
050500000710     C                   parm                    prmfir
050600010606     C*
050700010606     C* CALCOLA LA DATA CORRENTE
050800010606     C                   time                    wn14             14 0
050900010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051000010606     C                   z-add     wn8           g08dat
051100010606     C                   z-add     *zeros        g08inv
051200010606     C                   movel     '0'           g08err
051300010606     C                   call      'XSRDA8'
051400010606     C                   parm                    wlbda8
051500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051600000613     C*
051700000613     C                   ENDSR
051800000613     C***
