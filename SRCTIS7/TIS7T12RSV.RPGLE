000100030616      /TITLE Upload via Internet: traduzione in EDIVADWR.
000101130115     H DFTACTGRP(*no) ACTGRP(*caller)
000300021014
000500990914     Ftivin00r  uF   E             DISK    usropn
000600030616     FEDIVADWR  O    E             DISK
001100990914
001600000724     D*------------
001700990921     D dscmz         e ds                  inz
001800990914     D psds           sds
001900990914     D  procname         *PROC
002000990914     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002100990914     D* 10+3
002200990914     D                 DS
002300990914     D  WS15                   1      1
002400990914     D  WI15                   2     11
002500990914     D  WV15                  12     12
002600990914     D  WD15                  13     15
002700990914     D  WASC15                 1     15
002800990914     D*  6+1
002900990914     D                 DS
003000990914     D  WS09                   1      1
003100990914     D  WI09                   2      7
003200990914     D  WV09                   8      8
003300990914     D  WD09                   9      9
003400990914     D  WASC09                 1      9
003500990914     D*  2+3
003600990914     D                 DS
003700990914     D  WS07                   1      1
003800990914     D  WI07                   2      3
003900990914     D  WV07                   4      4
004000990914     D  WD07                   5      7
004100990914     D  WASC07                 1      7
004200990921     D tivlrds       e ds                  extname(tivlr00f)
004300990914     D fnvadaar      e ds                  inz
004400990914     D esito           s              1
004500000724     D prmlit          s             10
004600000710     D prmfir          s             10
004700990921     D wrkesito        s                   like(esito)
005000000613     D rrnum           s              6  0 INZ(*zeros)
005300030616     D wNomeFile       s             30    INZ(*blanks)
005400051212
005500051212     D TxtInOut        S           2048
005600051212     D ElencoChar      S            256
005700051212     D TipoElenco      S              1
005800051212     D CharSost        S              1
005900051212     D UpperCase       S              1
006000051212     D ChkNull         S              1
006100051212     D CharNull        S              1
006200051212     D EsitoBon        S              1
006300010606
006400010606
006500010606
006600990914
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200010531     C*
007300010531     C                   exsr      opeini
007400010531     C                   exsr      rwvad
007500010531     C*
007600010531     C                   seton                                        lr
007700010531
007800010531
007900010531
008000010531
008100010531     C*--------------------------------------------------------
008200010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008300010531     C*--------------------------------------------------------
008400010531     C     PREELA        BEGSR
008500000724     C*
008600000724     C* SE OCCORRE SPEDIRE IN FILIALE
008700010531     C                   if        invfil <> *zeros and
008800010531     C                             flgGiro = '0'
008900010531     C*
009000010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009100010531     C                   eval      flgGiro = '1'
012200010531     C*
012300010531     C                   endif
012400010531     C*
012500010531     C                   ENDSR
012600010531     C***
012700010531
012800010531
012900010531
013000010531
013100010531
013200010531
013300010531
013400010531     C*--------------------------------------------------------
013500010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013600010531     C*--------------------------------------------------------
013700010531     C     ENDELA        BEGSR
015100000616     C*
015200010531     C                   ENDSR
015300010531     C***
017600990914
017700010531
017800010531
017900010531
018000010531
018100990914     C*--------------------------------------------------------
018200030616     C* RWVAD  LEGGE TIVIN00R E SCRIVE EDIVAD0F              *
018300990914     C*--------------------------------------------------------
018400990914     C     RWVAD         BEGSR
018500990914     C*
018600990914     C                   if        not %open(tivin00r)
018700990914     C                   open      tivin00r
018800990914     C                   endif
018900030616     C                   if        not %open(edivadwr)
019000030616     C                   open      edivadwr
019100990914     C                   endif
019200990914     C*
019300990914     C                   clear                   §CTDOK
019400990914     C                   clear                   §CTDNO
019500990914     C*
019600990914     C                   DO        *HIVAL
019700010618     C*
019800990914     C                   READ      tivin00r                               70
019900010618     C*
020000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020100010618     C                   if        vinflg = '1'
020200010618     C                   eval      flgOk = '1'
020300010618     C                   endif
020400010618     C*
020500000905     C                   if        vindta > *blanks
020600000613     C                   add       1             rrnum
020700990914     C*
020800010601     C                   if        *in70 = *off and
020900990914     C                             (vinflg = *blanks
021000010711     C                              or vinflg = '0'
021100010711     C                              or vinflg = '2')
021200990914     C*
021300030616     C                   clear                   EDIVAD00
021400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021500010711     C                   if        vinflg = *blanks or vinflg = '0'
021600010711     C                   clear                   vinmsg
021700010711     C                   endif
021800010711     C*
021900990914     C                   eval      fnvadaar = vindta
022000991022     C*
022100991022     C                   testb     x'3F'         vhdatb                   02
022200991022     C                   if        not *in02
022300991022     C*
022400990914     C                   SETOFF                                       303132
022500990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
022600010531     C*
022700010531     C                   TESTN                   VHDLNP               30
022800010531     C  N30              TESTN                   VHDLNP                 30
022900010531     C  N30              TESTN                   VHDLNP                   30
023000010531     C                   MOVE      VHDLNP        W001A             1
023100010531     C     *IN30         IFEQ      *OFF
023200010531     C     W001A         ORLT      '0'
023300010531     C                   SETON                                        31
023400010531     C                   eval      vinmsg = %trimr(vinmsg)
023500010531     C                             + ' ' + 'VHDLNP'
023600010531     C                   ELSE
023700010531     C                   MOVE      VHDLNP        VADLNP
023800010531     C     VADLNP        IFLE      *ZEROS
023900010531     C                   SETON                                        31
024000010531     C                   eval      vinmsg = %trimr(vinmsg)
024100010531     C                             + ' ' + 'VHDLNP'
024200010531     C                   ENDIF
024300010531     C                   ENDIF
024400010531     C*
024500010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024600010531     C                   eval      depfil = VADLNP
024700010531     C                   exsr      repfil
024800010601     C                   if        depfil = invfil
024900021014     C                   if        vlrpoi = 999
025000021010     C                   MOVE(p)   invfil        VADFGS
025100021014     C                   else
025200021014     C                   MOVE(p)   vlrpoi        VADFGS
025300021014     C                   endif
025400010531     C*
025500010531     C                   exsr      PREELA
025600010531     C*
025700990914     C                   TESTN                   VHDCCM               30
025800990914     C  N30              TESTN                   VHDCCM                 30
025900990914     C  N30              TESTN                   VHDCCM                   30
026000990914     C                   MOVE      VHDCCM        W001A             1
026100990914     C     *IN30         IFEQ      *OFF
026200990914     C     W001A         ORLT      '0'
026300990914     C                   SETON                                        31
026400010604     C                   eval      vinmsg = %trimr(vinmsg)
026500010604     C                             + ' ' + 'VHDCCM'
026600990914     C                   ELSE
026700990914     C                   MOVE      VHDCCM        VADCCM
026800990914     C     VADCCM        IFLE      *ZEROS
026900990914     C                   SETON                                        31
027000010604     C                   eval      vinmsg = %trimr(vinmsg)
027100010604     C                             + ' ' + 'VHDCCM'
027200990914     C                   ENDIF
027300990914     C                   ENDIF
027400010601     C*
027500010601     C                   TESTN                   VHDAAS               30
027600010601     C  N30              TESTN                   VHDAAS                 30
027700010601     C  N30              TESTN                   VHDAAS                   30
027800010601     C                   MOVE      VHDAAS        W001A             1
027900010601     C     *IN30         IFEQ      *OFF
028000010601     C     W001A         ORLT      '0'
028100010601     C                   SETON                                        32
028200010606     C                   MOVEL     datcor        VADAAS
028300010601     C                   eval      vinmsg = %trimr(vinmsg)
028400010601     C                             + ' ' + 'VHDAAS'
028500010601     C                   ELSE
028600010601     C                   MOVE      VHDAAS        VADAAS
028700010601     C                   ENDIF
028800010601     C*
028900010601     C                   TESTN                   VHDNRS               30
029000010601     C  N30              TESTN                   VHDNRS                 30
029100010601     C  N30              TESTN                   VHDNRS                   30
029200010601     C                   MOVE      VHDNRS        W001A             1
029300010601     C     *IN30         IFEQ      *OFF
029400010601     C     W001A         ORLT      '0'
029500010601     C     W001A         ANDNE     *BLANK
029600010601     C                   SETON                                        32
029700010601     C                   MOVE      *ZEROS        VADNRS
029800010601     C                   eval      vinmsg = %trimr(vinmsg)
029900010601     C                             + ' ' + 'VHDNRS'
030000010601     C                   ELSE
030100010601     C                   MOVE      VHDNRS        VADNRS
030200010601     C                   ENDIF
030300010601     C*
030400010601     C                   TESTN                   VHDNSP               30
030500010601     C  N30              TESTN                   VHDNSP                 30
030600010601     C  N30              TESTN                   VHDNSP                   30
030700010601     C                   MOVE      VHDNSP        W001A             1
030800010601     C     *IN30         IFEQ      *OFF
030900010601     C     W001A         ORLT      '0'
031000010601     C                   SETON                                        32
031100010601     C                   MOVE      *ZEROS        VADNSP
031200010601     C                   eval      vinmsg = %trimr(vinmsg)
031300010601     C                             + ' ' + 'VHDNSP'
031400010601     C                   ELSE
031500010601     C                   MOVE      VHDNSP        VADNSP
031600010601     C                   ENDIF
031700010601 FG  C*
031800010601 xxx C                   IF        VHDNCL = *blanks and
031900010601 "   C                             VADNRS > *zeros
032000010601 "   C                   MOVE      '00001'       VHDNCL
032100010601 FG  C                   ENDIF
032200010601     C*
032300010601     C                   TESTN                   VHDNCL               30
032400010601     C  N30              TESTN                   VHDNCL                 30
032500010601     C  N30              TESTN                   VHDNCL                   30
032600010601     C                   MOVE      VHDNCL        W001A             1
032700010601     C     *IN30         IFEQ      *OFF
032800010601     C     W001A         ORLT      '0'
032900010601     C                   SETON                                        32
033000010601     C                   MOVE      *ZEROS        VADNCL
033100010601     C                   eval      vinmsg = %trimr(vinmsg)
033200010601     C                             + ' ' + 'VHDNCL'
033300010601     C                   ELSE
033400010601     C                   MOVE      VHDNCL        VADNCL
033500010601     C                   ENDIF
033600010601     C*
033700010601     C                   TESTN                   VHDNCD               30
033800010601     C  N30              TESTN                   VHDNCD                 30
033900010601     C  N30              TESTN                   VHDNCD                   30
034000010601     C                   MOVE      VHDNCD        W001A             1
034100010601     C     *IN30         IFEQ      *OFF
034200010601     C     W001A         ORLT      '0'
034300010601     C     W001A         ANDNE     *BLANK
034400010601     C                   SETON                                        32
034500010601     C                   MOVE      *ZEROS        VADNCD
034600010601     C                   eval      vinmsg = %trimr(vinmsg)
034700010601     C                             + ' ' + 'VHDNCD'
034800010601     C                   ELSE
034900010601     C                   MOVE      VHDNCD        VADNCD
035000010601     C                   ENDIF
035100010601     C*
035200010601     C                   TESTN                   VHDNCA               30
035300010601     C  N30              TESTN                   VHDNCA                 30
035400010601     C  N30              TESTN                   VHDNCA                   30
035500010601     C                   MOVE      VHDNCA        W001A             1
035600010601     C     *IN30         IFEQ      *OFF
035700010601     C     W001A         ORLT      '0'
035800010601     C     W001A         ANDNE     *BLANK
035900010601     C                   SETON                                        32
036000010601     C                   MOVE      *ZEROS        VADNCA
036100010601     C                   eval      vinmsg = %trimr(vinmsg)
036200010601     C                             + ' ' + 'VHDNCA'
036300010601     C                   ELSE
036400010601     C                   MOVE      VHDNCA        VADNCA
036500010601     C                   ENDIF
036600010601     C*
036700010601     C* ALFABETICI
036800010601     C                   MOVE      VHDATB        VADATB
036900010601     C                   MOVE      VHDCDP        VADCDP
037000030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037100030616     C                   EVAL      VADCMR = %subst(wNomeFile:4:5)
037200030616     C                   EVAL      VADCNT = 1
037300051212
037400051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
037500051212     c                   exsr      exeBon
037600010601     C*
037700010604     C  N31              ADD       1             §CTDOK            7 0
037800010601     C   32
037900010604     COR 31              ADD       1             §CTDNO            7 0
038000030616     C  N31              WRITE     EDIVAD00
038100010601     C*
038200010601     C                   if        *in31 = *off and
038300010601     C                             *in32 = *off
038400990914     C                   eval      vinflg = '1'
038500990914     C                   else
039000990914     C                   eval      vinflg = '2'
039100990914     C                   endif
039200000905     C                   endif
039300000905     C                   endif
039400010601     C*
039500010601     C                   endif
039600000905     C*
039700000905     C                   else
039800000905     C                   eval      vinflg = '1'
039900000905     C                   endif
040000000905     C*
040100000905     C  N70              update    tivin000
040200991022     C*
040300991022     C  N70              ENDdo
040400010531     C
040500010531     C*
040600010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
040700010531     C                   if        cntNonEl = *zeros or
040800010531     C                             flgMulti = '0'
040900990914     C* Se non ci sono record con errori ...
041000010531     C                   if        §ctdno = 0 and
041100010531     C                             flgStato <> '2'
041200990914     C* ... restituisco esito OK.
041300990921     C                   eval      wrkesito = '0'
041400990914     C                   else
041500000710     C                   if        §ctdok > 0
041600990921     C                   eval      wrkesito = '1'
041700000710     C                   else
041800010615     C                   if        flgOk = '0'
041900010615     C                   eval      wrkesito = '2'
042000010615     C                   else
042100010615     C                   eval      wrkesito = '6'
042200010615     C                   endif
042300990914     C                   endif
042400000710     C                   endif
042500010531     C                   else
042600010531     C                   eval      wrkesito = '9'
042700010531     C                   endif
042800990914     C*
042900990914     C                   if        %open(tivin00r)
043000990914     C                   close     tivin00r
043100990914     C                   endif
043200030616     C                   if        %open(edivadwr)
043300030616     C                   close     edivadwr
043400990914     C                   endif
043500010531     C*
043600010531     C                   if        vlrpoi <> 999
043700010531     C                   eval      invfil = vlrpoi
043800010531     C                   endif
043900990914     C*
044000990921     C                   if        §ctdok > 0
044100010531     C                             and invfil > *zeros
044200990921     C                   exsr      invio
044300990921     C                   endif
044400010612     C*
044500010612     C                   if        flgGiro = '1'
044600010612     C                   exsr      endela
044700010612     C                   endif
044800990921     C*
044900990914     C                   ENDSR
045000000613     C***
045100990921
045200010531
045300010531
045400010531
045500010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045600010531     C     repfil        BEGSR
045700010531     C*
045800010531     C                   if        invfil = *zeros and
045900010531     C                             depfil > *zeros and
046000010613     C                             (vinflg = *blanks or
046100010613     C                              vinflg = *zeros)
046200010531     C
046300010531     C                   eval      invfil = depfil
046400010531     C                   endif
046500010531     C*
046600010531     C                   if        depfil <> invfil and
046700010531     C                             invfil > *zeros
046800010531     C                   eval      flgMulti = '1'
046900010531     C                   if        vinflg = *blanks
047000010531     C                   add       1             cntNonEl
047100010531     C                   endif
047200010531     C                   endif
047300010531     C*
047400010531     C                   if        vinflg = '2'
047500010531     C                   eval      flgStato = '2'
047600010531     C                   endif
047700010531     C*
047800010531     C                   ENDSR
047900010531     C***
048000010531
048100010531
048200010531
048300010531
048400010531
048500990921      /TITLE Invio dei dati al punto operativo.
048600000613     C     invio         BEGSR
048700990921     C*
048800990921     C                   reset                   dscmz
048900030618     C                   move      invfil        cmzdst
049000030618     C***                move      '789'         cmzdst
049100990921     C                   eval      cmzfld = vlrfou
049200990921     C                   eval      cmzmbd = vlrhdl
049300990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
049400000710     C                   if        prmfir = *blanks
049500030616     C                   eval      cmzfla = 'EDIVAD0F'
049600030616     C                   eval      cmzmba = 'EDIVAD0F'
049700000710     C                   else
049800000710     C                   eval      cmzfla = prmfir
049900000710     C                   eval      cmzmba = prmfir
050000000710     C                   endif
050100990921     C                   eval      cmznrr = *zeros
050200990921     C                   move      §ctdok        cmznrr
050300021003     C                   eval      cmzlba = vlrfl1
050400990921     C                   call(e)   'TIS711C'
050500990921     C                   parm                    dscmz
050600990921     C                   parm      *blanks       esito
050700990921     C                   if        %error
050800990921     C                             or cmzerr = '1'
050900990921     C                             or esito  = '1'
051000000710     C                   eval      wrkesito = '3'
051100990921     C                   endif
051200990921     C*
051300000613     C                   ENDSR
051400000613     C***
051500990914
051600010531
051700010531
051800010531
051900010531      /TITLE Invio dei dati al punto operativo.
052000010531     C     opeini        BEGSR
052100010531     C*
052200010531     C* Inizializzo flag e contatori operativi
052300010531     C                   movel     '0'           flgGiro           1
052400010531     C                   movel     '0'           flgMulti          1
052500010531     C                   movel     '1'           flgStato          1
052600010615     C                   movel     '0'           flgOk             1
052700010531     C                   z-add     *zeros        cntNonEl         10 0
052800010531     C                   z-add     *zeros        depfil            3 0
052900010531     C                   z-add     *zeros        invfil            3 0
053000030407     C*
053100030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
053200030407     C                   if        %subst(vlrppt:1:1) = 'S'
053300030407     C                   seton                                        50
053400030407     C                   else
053500030407     C                   setoff                                       50
053600030407     C                   endif
053700010531     C*
053800010531     C                   ENDSR
053900010531     C***
054000010531
054100051212
054200051212
054300051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
054400051212     C     exeBon        BEGSR
054500051212     C*
054600051212     C* Imposto i parametri x il *pgm d "bonifica"
054700051212     C                   EVAL      ElencoChar = *blanks
054800051212     C                   EVAL      TipoElenco = *blanks
054900051212     C                   EVAL      CharSost   = *blanks
055000051212     C                   EVAL      UpperCase  = '1'
055100051212     C                   EVAL      ChkNull    = '1'
055200051212     C                   EVAL      CharNull   = *blanks
055300051212     C*
055400051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
055500051212     C                   EVAL      TxtInOut   = VADCDP
055600051212     C                   EVAL      EsitoBon   = *blanks
055700051212     C*
055800051212     C                   CALL      'XCHKCHAR'
055900051212     C                   PARM                    TxtInOut
056000051212     C                   PARM                    ElencoChar
056100051212     C                   PARM                    TipoElenco
056200051212     C                   PARM                    CharSost
056300051212     C                   PARM                    UpperCase
056400051212     C                   PARM                    ChkNull
056500051212     C                   PARM                    CharNull
056600051212     C                   PARM                    EsitoBon
056700051212     C*
056800051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
056900051212     C                   IF        EsitoBon = '1'
057000051212     C                   EVAL      VADCDP = TxtInOut
057100051212     C                   ENDIF
057200051212     C*
057300051212     C                   ENDSR
057400010531
057500010531
057600010531
057700000613     C     *inzsr        BEGSR
057800990914     C*
057900990914     C     *entry        plist
058000990921     C                   parm                    tivlrds
058100990921     C                   parm      wrkesito      esito
058200000724     C                   parm                    prmlit
058300000710     C                   parm                    prmfir
058400030616     C*
058500030616     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
058600030616     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
058700030618     C                   MOVE(P)   vlrMSG        wNomeFile
058800030616     C                   IF        %subst(vlrPPT:2:1) = 'S'
058900100319     C                   SELECT
059000100319     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
059100100319     C                             %subst(wNomeFile:7:2) = '61' OR
059200100319     C                             %subst(wNomeFile:7:2) = '62' OR
059300100319     C***                          %subst(wNomeFile:7:2) = '63' OR
059400100319     C                             %subst(wNomeFile:7:2) = '64' OR
059500100319     C***                          %subst(wNomeFile:7:2) = '65' OR
059600100319     C                             %subst(wNomeFile:7:2) = '66' OR
059700100319     C***                          %subst(wNomeFile:7:2) = '67' OR
059800100319     C                             %subst(wNomeFile:7:2) = '68' OR
059900100319     C                             %subst(wNomeFile:7:2) = '69'
060000100319     C***                          %subst(wNomeFile:7:2) = '70'
060100100319     C                   EVAL      vlrpoi = 999
060200100319     C                   WHEN      %subst(wNomeFile:7:2) = '99'
060300100319     C                   EVAL      vlrpoi = 048
060400100319     C                   WHEN      %subst(wNomeFile:7:2) = '13' OR
060500100319     C                             %subst(wNomeFile:7:2) = '25' OR
060600100319     C                             %subst(wNomeFile:7:2) = '55' OR
060700100319     C***                          %subst(wNomeFile:7:2) = '10' OR
060800100319     C***                          %subst(wNomeFile:7:2) = '20' OR
060900100319     C***                          %subst(wNomeFile:7:2) = '30' OR
061000100319     C***                          %subst(wNomeFile:7:2) = '40' OR
061100100319     C***                          %subst(wNomeFile:7:2) = '45' OR
061200100319     C***                          %subst(wNomeFile:7:2) = '80'
061300100319     C                             %subst(wNomeFile:7:2) = '01' OR
061400100319     C                             %subst(wNomeFile:7:2) = '02' OR
061500100319     C                             %subst(wNomeFile:7:2) = '24' OR              * sped. "garantite"
061600100319     C                             %subst(wNomeFile:7:2) = '03' OR
061700100319     C                             %subst(wNomeFile:7:2) = '63' OR
061800100319     C                             %subst(wNomeFile:7:2) = '65' OR
061900100319     C                             %subst(wNomeFile:7:2) = '67' OR
062000100319     C                             %subst(wNomeFile:7:2) = '70'
062100100319     C                   EVAL      vlrpoi = 277
062200100319     C***                WHEN      %subst(wNomeFile:7:2) = '01' OR
062300100319     C***                          %subst(wNomeFile:7:2) = '02' OR
062400100319     C***                          %subst(wNomeFile:7:2) = '24' OR              * sped. "garantite"
062500100319     C***                WHEN      %subst(wNomeFile:7:2) = '03' OR
062600100319     C                   WHEN      %subst(wNomeFile:7:2) = '04' OR
062700100319     C                             %subst(wNomeFile:7:2) = '05' OR
062800100319     C                             %subst(wNomeFile:7:2) = '06' OR
062900100319     C                             %subst(wNomeFile:7:2) = '07' OR
063000100319     C                             %subst(wNomeFile:7:2) = '08' OR
063100100319     C                             %subst(wNomeFile:7:2) = '09'
063200100319     C                   EVAL      vlrpoi = 006
063300100319     C                   OTHER
063400100319     C                   EVAL      vlrpoi = 043
063500100319     C                   ENDSL
063600081106     C                   ENDIF
063700010606     C*
063800010606     C* CALCOLA LA DATA CORRENTE
063900100319     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
064000100319     C                   eval      datcor = %dec(%date() : *ISO)
064100000613     C*
064200000613     C                   ENDSR
064300000613     C***
