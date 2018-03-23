000100110126      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200990914     H dftactgrp(*yes)
000300021014
000400990914     Ftivin00r  uF   E             DISK    usropn
000500110126     FEDIVADwr  O    E             DISK
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
004000110126     D wNomeFile       s             30    INZ(*blanks)
004100010606     D*------------------
004200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300010606     D*------------------
004400010606     D WLBDA8          DS                  INZ
004500010606     D  G08DAT                 1      8  0
004600010606     D  G08INV                 9     16  0
004700010606     D  G08ERR                17     17
004800010606     D  G08TGI                18     22  0
004900010606     D*
005000051212
005100051212     D TxtInOut        S           2048
005200051212     D ElencoChar      S            256
005300051212     D TipoElenco      S              1
005400051212     D CharSost        S              1
005500051212     D UpperCase       S              1
005600051212     D ChkNull         S              1
005700051212     D CharNull        S              1
005800051212     D EsitoBon        S              1
005900010606
006000010606
006100010606
006200990914
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600010531     C*
006700010531     C                   exsr      opeini
006800010531     C                   exsr      rwvad
006900010531     C*
007000010531     C                   seton                                        lr
007100010531
007200010531
007300010531
007400010531
007500010531     C*--------------------------------------------------------
007600010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007700010531     C*--------------------------------------------------------
007800010531     C     PREELA        BEGSR
007900000724     C*
008000000724     C* SE OCCORRE SPEDIRE IN FILIALE
008100010531     C                   if        invfil <> *zeros and
008200010531     C                             flgGiro = '0'
008300010531     C*
008400010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008500010531     C                   eval      flgGiro = '1'
008600010531     C*
008700010531     C                   endif
008800010531     C*
008900010531     C                   ENDSR
009000010531     C***
009100010531
009200010531
009300010531
009400010531
009500010531
009600010531
009700010531
009800010531     C*--------------------------------------------------------
009900010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010000010531     C*--------------------------------------------------------
010100010531     C     ENDELA        BEGSR
010200000616     C*
010300010531     C                   ENDSR
010400010531     C***
010500990914
010600010531
010700010531
010800010531
010900010531
011000990914     C*--------------------------------------------------------
011100110126     C* RWVAD   LEGGE FNVADAAR E SCRIVE EDIVAD0F              *
011200990914     C*--------------------------------------------------------
011300990914     C     RWVAD         BEGSR
011400990914     C*
011500990914     C                   if        not %open(tivin00r)
011600990914     C                   open      tivin00r
011700990914     C                   endif
011800110126     C                   if        not %open(edivadwr)
011900110126     C                   open      edivadwr
012000990914     C                   endif
012100990914     C*
012200990914     C                   clear                   §CTDOK
012300990914     C                   clear                   §CTDNO
012400990914     C*
012500990914     C                   DO        *HIVAL
012600010618     C*
012700990914     C                   READ      tivin00r                               70
012800010618     C*
012900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013000010618     C                   if        vinflg = '1'
013100010618     C                   eval      flgOk = '1'
013200010618     C                   endif
013300010618     C*
013400000905     C                   if        vindta > *blanks
013500000613     C                   add       1             rrnum
013600990914     C*
013700010601     C                   if        *in70 = *off and
013800990914     C                             (vinflg = *blanks
013900010711     C                              or vinflg = '0'
014000010711     C                              or vinflg = '2')
014100990914     C*
014200110126     C                   clear                   EDIVAD00
014300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014400010711     C                   if        vinflg = *blanks or vinflg = '0'
014500010711     C                   clear                   vinmsg
014600010711     C                   endif
014700010711     C*
014800990914     C                   eval      fnvadaar = vindta
014900991022     C*
015000991022     C                   testb     x'3F'         vhdatb                   02
015100991022     C                   if        not *in02
015200991022     C*
015300990914     C                   SETOFF                                       303132
015400990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
015500010531     C*
015600010531     C                   TESTN                   VHDLNP               30
015700010531     C  N30              TESTN                   VHDLNP                 30
015800010531     C  N30              TESTN                   VHDLNP                   30
015900010531     C                   MOVE      VHDLNP        W001A             1
016000010531     C     *IN30         IFEQ      *OFF
016100010531     C     W001A         ORLT      '0'
016200010531     C                   SETON                                        31
016300010531     C                   eval      vinmsg = %trimr(vinmsg)
016400010531     C                             + ' ' + 'VHDLNP'
016500010531     C                   ELSE
016600010531     C                   MOVE      VHDLNP        VADLNP
016700010531     C     VADLNP        IFLE      *ZEROS
016800010531     C                   SETON                                        31
016900010531     C                   eval      vinmsg = %trimr(vinmsg)
017000010531     C                             + ' ' + 'VHDLNP'
017100010531     C                   ENDIF
017200010531     C                   ENDIF
017300010531     C*
017400990914     C                   TESTN                   VHDCCM               30
017500990914     C  N30              TESTN                   VHDCCM                 30
017600990914     C  N30              TESTN                   VHDCCM                   30
017700990914     C                   MOVE      VHDCCM        W001A             1
017800990914     C     *IN30         IFEQ      *OFF
017900990914     C     W001A         ORLT      '0'
018000990914     C                   SETON                                        31
018100010604     C                   eval      vinmsg = %trimr(vinmsg)
018200010604     C                             + ' ' + 'VHDCCM'
018300990914     C                   ELSE
018400990914     C                   MOVE      VHDCCM        VADCCM
018500990914     C     VADCCM        IFLE      *ZEROS
018600990914     C                   SETON                                        31
018700010604     C                   eval      vinmsg = %trimr(vinmsg)
018800010604     C                             + ' ' + 'VHDCCM'
018900990914     C                   ENDIF
019000990914     C                   ENDIF
019100070115     C*
019200070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300070115     C                   eval      depfil = VADLNP
019400070115     C*
019500070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
019600070115     C* cliente
019700070507     C                   if        VADCCM = 0893943 OR
019800070507     C                             VADCCM = 0894169
019900070115     C                   eval      depfil = 050
020000070115     C                   eval      invfil = 050
020100070115     C                   eval      vadfgs = 050
020200070115     C                   endif
020300070507     C                   if        VADCCM = 0893942 OR
020400070507     C                             VADCCM = 0894168
020500070115     C                   eval      depfil = 005
020600070115     C                   eval      invfil = 005
020700070115     C                   eval      vadfgs = 005
020800070115     C                   endif
020900070115     C*
021000070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021100070115     C                   exsr      repfil
021200070115     C                   if        depfil = invfil
021300070115     C                   if        vlrpoi = 999
021400070115     C                   MOVE(p)   invfil        VADFGS
021500070115     C                   else
021600070115     C                   MOVE(p)   vlrpoi        VADFGS
021700070115     C                   endif
021800070115     C*
021900070115     C                   exsr      PREELA
022000010601     C*
022100010601     C                   TESTN                   VHDAAS               30
022200010601     C  N30              TESTN                   VHDAAS                 30
022300010601     C  N30              TESTN                   VHDAAS                   30
022400010601     C                   MOVE      VHDAAS        W001A             1
022500010601     C     *IN30         IFEQ      *OFF
022600010601     C     W001A         ORLT      '0'
022700010601     C                   SETON                                        32
022800010606     C                   MOVEL     datcor        VADAAS
022900010601     C                   eval      vinmsg = %trimr(vinmsg)
023000010601     C                             + ' ' + 'VHDAAS'
023100010601     C                   ELSE
023200010601     C                   MOVE      VHDAAS        VADAAS
023300010601     C                   ENDIF
023400010601     C*
023500010601     C                   TESTN                   VHDNRS               30
023600010601     C  N30              TESTN                   VHDNRS                 30
023700010601     C  N30              TESTN                   VHDNRS                   30
023800010601     C                   MOVE      VHDNRS        W001A             1
023900010601     C     *IN30         IFEQ      *OFF
024000010601     C     W001A         ORLT      '0'
024100010601     C     W001A         ANDNE     *BLANK
024200010601     C                   SETON                                        32
024300010601     C                   MOVE      *ZEROS        VADNRS
024400010601     C                   eval      vinmsg = %trimr(vinmsg)
024500010601     C                             + ' ' + 'VHDNRS'
024600010601     C                   ELSE
024700010601     C                   MOVE      VHDNRS        VADNRS
024800010601     C                   ENDIF
024900010601     C*
025000010601     C                   TESTN                   VHDNSP               30
025100010601     C  N30              TESTN                   VHDNSP                 30
025200010601     C  N30              TESTN                   VHDNSP                   30
025300010601     C                   MOVE      VHDNSP        W001A             1
025400010601     C     *IN30         IFEQ      *OFF
025500010601     C     W001A         ORLT      '0'
025600010601     C                   SETON                                        32
025700010601     C                   MOVE      *ZEROS        VADNSP
025800010601     C                   eval      vinmsg = %trimr(vinmsg)
025900010601     C                             + ' ' + 'VHDNSP'
026000010601     C                   ELSE
026100010601     C                   MOVE      VHDNSP        VADNSP
026200010601     C                   ENDIF
026300010601 FG  C*
026400010601 xxx C                   IF        VHDNCL = *blanks and
026500010601 "   C                             VADNRS > *zeros
026600010601 "   C                   MOVE      '00001'       VHDNCL
026700010601 FG  C                   ENDIF
026800010601     C*
026900010601     C                   TESTN                   VHDNCL               30
027000010601     C  N30              TESTN                   VHDNCL                 30
027100010601     C  N30              TESTN                   VHDNCL                   30
027200010601     C                   MOVE      VHDNCL        W001A             1
027300010601     C     *IN30         IFEQ      *OFF
027400010601     C     W001A         ORLT      '0'
027500010601     C                   SETON                                        32
027600010601     C                   MOVE      *ZEROS        VADNCL
027700010601     C                   eval      vinmsg = %trimr(vinmsg)
027800010601     C                             + ' ' + 'VHDNCL'
027900010601     C                   ELSE
028000010601     C                   MOVE      VHDNCL        VADNCL
028100010601     C                   ENDIF
028200010601     C*
028300010601     C                   TESTN                   VHDNCD               30
028400010601     C  N30              TESTN                   VHDNCD                 30
028500010601     C  N30              TESTN                   VHDNCD                   30
028600010601     C                   MOVE      VHDNCD        W001A             1
028700010601     C     *IN30         IFEQ      *OFF
028800010601     C     W001A         ORLT      '0'
028900010601     C     W001A         ANDNE     *BLANK
029000010601     C                   SETON                                        32
029100010601     C                   MOVE      *ZEROS        VADNCD
029200010601     C                   eval      vinmsg = %trimr(vinmsg)
029300010601     C                             + ' ' + 'VHDNCD'
029400010601     C                   ELSE
029500010601     C                   MOVE      VHDNCD        VADNCD
029600010601     C                   ENDIF
029700010601     C*
029800010601     C                   TESTN                   VHDNCA               30
029900010601     C  N30              TESTN                   VHDNCA                 30
030000010601     C  N30              TESTN                   VHDNCA                   30
030100010601     C                   MOVE      VHDNCA        W001A             1
030200010601     C     *IN30         IFEQ      *OFF
030300010601     C     W001A         ORLT      '0'
030400010601     C     W001A         ANDNE     *BLANK
030500010601     C                   SETON                                        32
030600010601     C                   MOVE      *ZEROS        VADNCA
030700010601     C                   eval      vinmsg = %trimr(vinmsg)
030800010601     C                             + ' ' + 'VHDNCA'
030900010601     C                   ELSE
031000010601     C                   MOVE      VHDNCA        VADNCA
031100010601     C                   ENDIF
031200010601     C*
031300010601     C* ALFABETICI
031400010601     C                   MOVE      VHDATB        VADATB
031500010601     C                   MOVE      VHDCDP        VADCDP
031600070108     C*
031700070108     C* "bonifico" i dati di input da eventuali caratteri indesiderati
031800070108     C                   exsr      exeBon
031900010601     C*
032000010604     C  N31              ADD       1             §CTDOK            7 0
032100010601     C   32
032200010604     COR 31              ADD       1             §CTDNO            7 0
032300110126     C*
032400110126     C* VALORIZZO CAMPI RELATIVI AL "CMR"
032500110126     C                   IF        not *in31
032600110126     C                   EVAL      VADCMR = %subst(wNomeFile:8:8)
032700110126     C                   EVAL      VADCNT = 1
032800110126     C                   ENDIF
032900110126     C*
033000110126     C  N31              WRITE     EDIVAD00
033100010601     C*
033200010601     C                   if        *in31 = *off and
033300010601     C                             *in32 = *off
033400990914     C                   eval      vinflg = '1'
033500990914     C                   else
033600990914     C                   eval      vinflg = '2'
033700990914     C                   endif
033800000905     C                   endif
033900000905     C                   endif
034000010601     C*
034100010601     C                   endif
034200000905     C*
034300000905     C                   else
034400000905     C                   eval      vinflg = '1'
034500000905     C                   endif
034600000905     C*
034700000905     C  N70              update    tivin000
034800991022     C*
034900991022     C  N70              ENDdo
035000010531     C
035100010531     C*
035200010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035300010531     C                   if        cntNonEl = *zeros or
035400010531     C                             flgMulti = '0'
035500990914     C* Se non ci sono record con errori ...
035600010531     C                   if        §ctdno = 0 and
035700010531     C                             flgStato <> '2'
035800990914     C* ... restituisco esito OK.
035900990921     C                   eval      wrkesito = '0'
036000990914     C                   else
036100000710     C                   if        §ctdok > 0
036200990921     C                   eval      wrkesito = '1'
036300000710     C                   else
036400010615     C                   if        flgOk = '0'
036500010615     C                   eval      wrkesito = '2'
036600010615     C                   else
036700010615     C                   eval      wrkesito = '6'
036800010615     C                   endif
036900990914     C                   endif
037000000710     C                   endif
037100010531     C                   else
037200010531     C                   eval      wrkesito = '9'
037300010531     C                   endif
037400990914     C*
037500990914     C                   if        %open(tivin00r)
037600990914     C                   close     tivin00r
037700990914     C                   endif
037800110126     C                   if        %open(edivadwr)
037900110126     C                   close     edivadwr
038000990914     C                   endif
038100010531     C*
038200010531     C                   if        vlrpoi <> 999
038300010531     C                   eval      invfil = vlrpoi
038400010531     C                   endif
038500990914     C*
038600990921     C                   if        §ctdok > 0
038700010531     C                             and invfil > *zeros
038800990921     C                   exsr      invio
038900990921     C                   endif
039000010612     C*
039100010612     C                   if        flgGiro = '1'
039200010612     C                   exsr      endela
039300010612     C                   endif
039400990921     C*
039500990914     C                   ENDSR
039600000613     C***
039700990921
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
043000010531
043100990921      /TITLE Invio dei dati al punto operativo.
043200000613     C     invio         BEGSR
043300990921     C*
043400990921     C                   reset                   dscmz
043500010531     C                   move      invfil        cmzdst
043600990921     C                   eval      cmzfld = vlrfou
043700990921     C                   eval      cmzmbd = vlrhdl
043800990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
043900000710     C                   if        prmfir = *blanks
044000110126     C                   eval      cmzfla = 'EDIVAD0F'
044100110126     C                   eval      cmzmba = 'EDIVAD0F'
044200000710     C                   else
044300000710     C                   eval      cmzfla = prmfir
044400000710     C                   eval      cmzmba = prmfir
044500000710     C                   endif
044600990921     C                   eval      cmznrr = *zeros
044700990921     C                   move      §ctdok        cmznrr
044800021003     C                   eval      cmzlba = vlrfl1
044900990921     C                   call(e)   'TIS711C'
045000990921     C                   parm                    dscmz
045100990921     C                   parm      *blanks       esito
045200990921     C                   if        %error
045300990921     C                             or cmzerr = '1'
045400990921     C                             or esito  = '1'
045500000710     C                   eval      wrkesito = '3'
045600990921     C                   endif
045700990921     C*
045800000613     C                   ENDSR
045900000613     C***
046000990914
046100010531
046200010531
046300010531
046400010531      /TITLE Invio dei dati al punto operativo.
046500010531     C     opeini        BEGSR
046600010531     C*
046700010531     C* Inizializzo flag e contatori operativi
046800010531     C                   movel     '0'           flgGiro           1
046900010531     C                   movel     '0'           flgMulti          1
047000010531     C                   movel     '1'           flgStato          1
047100010615     C                   movel     '0'           flgOk             1
047200010531     C                   z-add     *zeros        cntNonEl         10 0
047300010531     C                   z-add     *zeros        depfil            3 0
047400010531     C                   z-add     *zeros        invfil            3 0
047500030407     C*
047600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
047700030407     C                   if        %subst(vlrppt:1:1) = 'S'
047800030407     C                   seton                                        50
047900030407     C                   else
048000030407     C                   setoff                                       50
048100030407     C                   endif
048200010531     C*
048300010531     C                   ENDSR
048400010531     C***
048500010531
048600051212
048700051212
048800051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
048900051212     C     exeBon        BEGSR
049000051212     C*
049100051212     C* Imposto i parametri x il *pgm d "bonifica"
049200051212     C                   EVAL      ElencoChar = *blanks
049300051212     C                   EVAL      TipoElenco = *blanks
049400051212     C                   EVAL      CharSost   = *blanks
049500051212     C                   EVAL      UpperCase  = '1'
049600051212     C                   EVAL      ChkNull    = '1'
049700051212     C                   EVAL      CharNull   = *blanks
049800051212     C*
049900051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
050000051212     C                   EVAL      TxtInOut   = VADCDP
050100051212     C                   EVAL      EsitoBon   = *blanks
050200051212     C*
050300051212     C                   CALL      'XCHKCHAR'
050400051212     C                   PARM                    TxtInOut
050500051212     C                   PARM                    ElencoChar
050600051212     C                   PARM                    TipoElenco
050700051212     C                   PARM                    CharSost
050800051212     C                   PARM                    UpperCase
050900051212     C                   PARM                    ChkNull
051000051212     C                   PARM                    CharNull
051100051212     C                   PARM                    EsitoBon
051200051212     C*
051300051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
051400051212     C                   IF        EsitoBon = '1'
051500051212     C                   EVAL      VADCDP = TxtInOut
051600051212     C                   ENDIF
051700051212     C*
051800051212     C                   ENDSR
051900051212
052000010531
052100010531
052200010531
052300000613     C     *inzsr        BEGSR
052400990914     C*
052500990914     C     *entry        plist
052600990921     C                   parm                    tivlrds
052700990921     C                   parm      wrkesito      esito
052800000724     C                   parm                    prmlit
052900000710     C                   parm                    prmfir
053000110126     C*
053100110126     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
053200110126     C                   move(p)   vlrMSG        wNomeFile
053300010606     C*
053400010606     C* CALCOLA LA DATA CORRENTE
053500010606     C                   time                    wn14             14 0
053600010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053700010606     C                   z-add     wn8           g08dat
053800010606     C                   z-add     *zeros        g08inv
053900010606     C                   movel     '0'           g08err
054000010606     C                   call      'XSRDA8'
054100010606     C                   parm                    wlbda8
054200010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054300000613     C*
054400000613     C                   ENDSR
054500000613     C***
