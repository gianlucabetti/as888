000100030616      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300021014
000400990914     Ftivin00r  uF   E             DISK    usropn
000500030616     FEDIVADWR  O    E             DISK
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
004000030616     D wNomeFile       s             30    INZ(*blanks)
004100030710     D wCMR            s             35    INZ(*blanks)
004200030710     D wPosDaA         s              2    INZ(*blanks)
004300030710     D wPosDa          s              2  0 INZ(*zeros)
004400030710     D wLungA          s              2    INZ(*blanks)
004500030710     D wLung           s              2  0 INZ(*zeros)
004600010606     D*------------------
004700010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800010606     D*------------------
004900010606     D WLBDA8          DS                  INZ
005000010606     D  G08DAT                 1      8  0
005100010606     D  G08INV                 9     16  0
005200010606     D  G08ERR                17     17
005300010606     D  G08TGI                18     22  0
005400010606     D*
005500010606
005600051212     D TxtInOut        S           2048
005700051212     D ElencoChar      S            256
005800051212     D TipoElenco      S              1
005900051212     D CharSost        S              1
006000051212     D UpperCase       S              1
006100051212     D ChkNull         S              1
006200051212     D CharNull        S              1
006300051212     D EsitoBon        S              1
006400010606
006500010606
006600990914
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000010531     C*
007100010531     C                   exsr      opeini
007200010531     C                   exsr      rwvad
007300010531     C*
007400010531     C                   seton                                        lr
007500010531
007600010531
007700010531
007800010531
007900010531     C*--------------------------------------------------------
008000010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008100010531     C*--------------------------------------------------------
008200010531     C     PREELA        BEGSR
008300000724     C*
008400000724     C* SE OCCORRE SPEDIRE IN FILIALE
008500010531     C                   if        invfil <> *zeros and
008600010531     C                             flgGiro = '0'
008700010531     C*
008800010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008900010531     C                   eval      flgGiro = '1'
009000010531     C*
009100010531     C                   endif
009200010531     C*
009300010531     C                   ENDSR
009400010531     C***
009500010531
009600010531
009700010531
009800010531
009900010531
010000010531
010100010531
010200010531     C*--------------------------------------------------------
010300010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010400010531     C*--------------------------------------------------------
010500010531     C     ENDELA        BEGSR
010600000616     C*
010700010531     C                   ENDSR
010800010531     C***
010900990914
011000010531
011100010531
011200010531
011300010531
011400990914     C*--------------------------------------------------------
011500030616     C* RWVAD  LEGGE TIVIN00R E SCRIVE EDIVAD0F              *
011600990914     C*--------------------------------------------------------
011700990914     C     RWVAD         BEGSR
011800030710     C*
011900030710     C                   clear                   §CTDOK
012000030710     C                   clear                   §CTDNO
012100990914     C*
012200030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
012300030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
012400030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
012500030710     C                   if        *in60 = *on
012600990914     C                   if        not %open(tivin00r)
012700990914     C                   open      tivin00r
012800990914     C                   endif
012900030616     C                   if        not %open(edivadwr)
013000030616     C                   open      edivadwr
013100990914     C                   endif
013200990914     C*
013300990914     C                   DO        *HIVAL
013400010618     C*
013500990914     C                   READ      tivin00r                               70
013600010618     C*
013700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013800010618     C                   if        vinflg = '1'
013900010618     C                   eval      flgOk = '1'
014000010618     C                   endif
014100010618     C*
014200000905     C                   if        vindta > *blanks
014300000613     C                   add       1             rrnum
014400990914     C*
014500010601     C                   if        *in70 = *off and
014600990914     C                             (vinflg = *blanks
014700010711     C                              or vinflg = '0'
014800010711     C                              or vinflg = '2')
014900990914     C*
015000030616     C                   clear                   EDIVAD00
015100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015200010711     C                   if        vinflg = *blanks or vinflg = '0'
015300010711     C                   clear                   vinmsg
015400010711     C                   endif
015500010711     C*
015600990914     C                   eval      fnvadaar = vindta
015700991022     C*
015800991022     C                   testb     x'3F'         vhdatb                   02
015900991022     C                   if        not *in02
016000991022     C*
016100990914     C                   SETOFF                                       303132
016200990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016300010531     C*
016400010531     C                   TESTN                   VHDLNP               30
016500010531     C  N30              TESTN                   VHDLNP                 30
016600010531     C  N30              TESTN                   VHDLNP                   30
016700010531     C                   MOVE      VHDLNP        W001A             1
016800010531     C     *IN30         IFEQ      *OFF
016900010531     C     W001A         ORLT      '0'
017000010531     C                   SETON                                        31
017100010531     C                   eval      vinmsg = %trimr(vinmsg)
017200010531     C                             + ' ' + 'VHDLNP'
017300010531     C                   ELSE
017400010531     C                   MOVE      VHDLNP        VADLNP
017500010531     C     VADLNP        IFLE      *ZEROS
017600010531     C                   SETON                                        31
017700010531     C                   eval      vinmsg = %trimr(vinmsg)
017800010531     C                             + ' ' + 'VHDLNP'
017900010531     C                   ENDIF
018000010531     C                   ENDIF
018100010531     C*
018200010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018300010531     C                   eval      depfil = VADLNP
018400010531     C                   exsr      repfil
018500010601     C                   if        depfil = invfil
018600021014     C                   if        vlrpoi = 999
018700021010     C                   MOVE(p)   invfil        VADFGS
018800021014     C                   else
018900021014     C                   MOVE(p)   vlrpoi        VADFGS
019000021014     C                   endif
019100010531     C*
019200010531     C                   exsr      PREELA
019300010531     C*
019400990914     C                   TESTN                   VHDCCM               30
019500990914     C  N30              TESTN                   VHDCCM                 30
019600990914     C  N30              TESTN                   VHDCCM                   30
019700990914     C                   MOVE      VHDCCM        W001A             1
019800990914     C     *IN30         IFEQ      *OFF
019900990914     C     W001A         ORLT      '0'
020000990914     C                   SETON                                        31
020100010604     C                   eval      vinmsg = %trimr(vinmsg)
020200010604     C                             + ' ' + 'VHDCCM'
020300990914     C                   ELSE
020400990914     C                   MOVE      VHDCCM        VADCCM
020500990914     C     VADCCM        IFLE      *ZEROS
020600990914     C                   SETON                                        31
020700010604     C                   eval      vinmsg = %trimr(vinmsg)
020800010604     C                             + ' ' + 'VHDCCM'
020900990914     C                   ENDIF
021000990914     C                   ENDIF
021100010601     C*
021200010601     C                   TESTN                   VHDAAS               30
021300010601     C  N30              TESTN                   VHDAAS                 30
021400010601     C  N30              TESTN                   VHDAAS                   30
021500010601     C                   MOVE      VHDAAS        W001A             1
021600010601     C     *IN30         IFEQ      *OFF
021700010601     C     W001A         ORLT      '0'
021800010601     C                   SETON                                        32
021900010606     C                   MOVEL     datcor        VADAAS
022000010601     C                   eval      vinmsg = %trimr(vinmsg)
022100010601     C                             + ' ' + 'VHDAAS'
022200010601     C                   ELSE
022300010601     C                   MOVE      VHDAAS        VADAAS
022400010601     C                   ENDIF
022500010601     C*
022600010601     C                   TESTN                   VHDNRS               30
022700010601     C  N30              TESTN                   VHDNRS                 30
022800010601     C  N30              TESTN                   VHDNRS                   30
022900010601     C                   MOVE      VHDNRS        W001A             1
023000010601     C     *IN30         IFEQ      *OFF
023100010601     C     W001A         ORLT      '0'
023200010601     C     W001A         ANDNE     *BLANK
023300010601     C                   SETON                                        32
023400010601     C                   MOVE      *ZEROS        VADNRS
023500010601     C                   eval      vinmsg = %trimr(vinmsg)
023600010601     C                             + ' ' + 'VHDNRS'
023700010601     C                   ELSE
023800010601     C                   MOVE      VHDNRS        VADNRS
023900010601     C                   ENDIF
024000010601     C*
024100010601     C                   TESTN                   VHDNSP               30
024200010601     C  N30              TESTN                   VHDNSP                 30
024300010601     C  N30              TESTN                   VHDNSP                   30
024400010601     C                   MOVE      VHDNSP        W001A             1
024500010601     C     *IN30         IFEQ      *OFF
024600010601     C     W001A         ORLT      '0'
024700010601     C                   SETON                                        32
024800010601     C                   MOVE      *ZEROS        VADNSP
024900010601     C                   eval      vinmsg = %trimr(vinmsg)
025000010601     C                             + ' ' + 'VHDNSP'
025100010601     C                   ELSE
025200010601     C                   MOVE      VHDNSP        VADNSP
025300010601     C                   ENDIF
025400010601 FG  C*
025500010601 xxx C                   IF        VHDNCL = *blanks and
025600010601 "   C                             VADNRS > *zeros
025700010601 "   C                   MOVE      '00001'       VHDNCL
025800010601 FG  C                   ENDIF
025900010601     C*
026000010601     C                   TESTN                   VHDNCL               30
026100010601     C  N30              TESTN                   VHDNCL                 30
026200010601     C  N30              TESTN                   VHDNCL                   30
026300010601     C                   MOVE      VHDNCL        W001A             1
026400010601     C     *IN30         IFEQ      *OFF
026500010601     C     W001A         ORLT      '0'
026600010601     C                   SETON                                        32
026700010601     C                   MOVE      *ZEROS        VADNCL
026800010601     C                   eval      vinmsg = %trimr(vinmsg)
026900010601     C                             + ' ' + 'VHDNCL'
027000010601     C                   ELSE
027100010601     C                   MOVE      VHDNCL        VADNCL
027200010601     C                   ENDIF
027300010601     C*
027400010601     C                   TESTN                   VHDNCD               30
027500010601     C  N30              TESTN                   VHDNCD                 30
027600010601     C  N30              TESTN                   VHDNCD                   30
027700010601     C                   MOVE      VHDNCD        W001A             1
027800010601     C     *IN30         IFEQ      *OFF
027900010601     C     W001A         ORLT      '0'
028000010601     C     W001A         ANDNE     *BLANK
028100010601     C                   SETON                                        32
028200010601     C                   MOVE      *ZEROS        VADNCD
028300010601     C                   eval      vinmsg = %trimr(vinmsg)
028400010601     C                             + ' ' + 'VHDNCD'
028500010601     C                   ELSE
028600010601     C                   MOVE      VHDNCD        VADNCD
028700010601     C                   ENDIF
028800010601     C*
028900010601     C                   TESTN                   VHDNCA               30
029000010601     C  N30              TESTN                   VHDNCA                 30
029100010601     C  N30              TESTN                   VHDNCA                   30
029200010601     C                   MOVE      VHDNCA        W001A             1
029300010601     C     *IN30         IFEQ      *OFF
029400010601     C     W001A         ORLT      '0'
029500010601     C     W001A         ANDNE     *BLANK
029600010601     C                   SETON                                        32
029700010601     C                   MOVE      *ZEROS        VADNCA
029800010601     C                   eval      vinmsg = %trimr(vinmsg)
029900010601     C                             + ' ' + 'VHDNCA'
030000010601     C                   ELSE
030100010601     C                   MOVE      VHDNCA        VADNCA
030200010601     C                   ENDIF
030300010601     C*
030400010601     C* ALFABETICI
030500010601     C                   MOVE      VHDATB        VADATB
030600010601     C                   MOVE      VHDCDP        VADCDP
030700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
030800030710     C                   EVAL      VADCMR = wCMR
030900030616     C                   EVAL      VADCNT = 1
031000051212
031100051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
031200051212     c                   exsr      exeBon
031300051212
031400010601     C*
031500010604     C  N31              ADD       1             §CTDOK            7 0
031600010601     C   32
031700010604     COR 31              ADD       1             §CTDNO            7 0
031800030616     C  N31              WRITE     EDIVAD00
031900010601     C*
032000010601     C                   if        *in31 = *off and
032100010601     C                             *in32 = *off
032200990914     C                   eval      vinflg = '1'
032300990914     C                   else
032400990914     C                   eval      vinflg = '2'
032500990914     C                   endif
032600000905     C                   endif
032700000905     C                   endif
032800010601     C*
032900010601     C                   endif
033000000905     C*
033100000905     C                   else
033200000905     C                   eval      vinflg = '1'
033300000905     C                   endif
033400000905     C*
033500000905     C  N70              update    tivin000
033600991022     C*
033700991022     C  N70              ENDdo
033800010531     C
033900010531     C*
034000010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034100010531     C                   if        cntNonEl = *zeros or
034200010531     C                             flgMulti = '0'
034300990914     C* Se non ci sono record con errori ...
034400010531     C                   if        §ctdno = 0 and
034500010531     C                             flgStato <> '2'
034600990914     C* ... restituisco esito OK.
034700990921     C                   eval      wrkesito = '0'
034800990914     C                   else
034900000710     C                   if        §ctdok > 0
035000990921     C                   eval      wrkesito = '1'
035100000710     C                   else
035200010615     C                   if        flgOk = '0'
035300010615     C                   eval      wrkesito = '2'
035400010615     C                   else
035500010615     C                   eval      wrkesito = '6'
035600010615     C                   endif
035700990914     C                   endif
035800000710     C                   endif
035900010531     C                   else
036000010531     C                   eval      wrkesito = '9'
036100010531     C                   endif
036200030710     C*
036300030710 X  1C                   else
036400030710     C                   eval      wrkesito = '8'
036500030710     C                   eval      flgGiro = '1'
036600030710 E  1C                   endif
036700990914     C*
036800990914     C                   if        %open(tivin00r)
036900990914     C                   close     tivin00r
037000990914     C                   endif
037100030616     C                   if        %open(edivadwr)
037200030616     C                   close     edivadwr
037300990914     C                   endif
037400010531     C*
037500010531     C                   if        vlrpoi <> 999
037600010531     C                   eval      invfil = vlrpoi
037700010531     C                   endif
037800990914     C*
037900990921     C                   if        §ctdok > 0
038000010531     C                             and invfil > *zeros
038100990921     C                   exsr      invio
038200990921     C                   endif
038300010612     C*
038400010612     C                   if        flgGiro = '1'
038500010612     C                   exsr      endela
038600010612     C                   endif
038700990921     C*
038800990914     C                   ENDSR
038900000613     C***
039000990921
039100010531
039200010531
039300010531
039400010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
039500010531     C     repfil        BEGSR
039600010531     C*
039700010531     C                   if        invfil = *zeros and
039800010531     C                             depfil > *zeros and
039900010613     C                             (vinflg = *blanks or
040000010613     C                              vinflg = *zeros)
040100010531     C
040200010531     C                   eval      invfil = depfil
040300010531     C                   endif
040400010531     C*
040500010531     C                   if        depfil <> invfil and
040600010531     C                             invfil > *zeros
040700010531     C                   eval      flgMulti = '1'
040800010531     C                   if        vinflg = *blanks
040900010531     C                   add       1             cntNonEl
041000010531     C                   endif
041100010531     C                   endif
041200010531     C*
041300010531     C                   if        vinflg = '2'
041400010531     C                   eval      flgStato = '2'
041500010531     C                   endif
041600010531     C*
041700010531     C                   ENDSR
041800010531     C***
041900010531
042000010531
042100010531
042200010531
042300010531
042400990921      /TITLE Invio dei dati al punto operativo.
042500000613     C     invio         BEGSR
042600990921     C*
042700990921     C                   reset                   dscmz
042800030618     C                   move      invfil        cmzdst
042900030618     C***                move      '789'         cmzdst
043000990921     C                   eval      cmzfld = vlrfou
043100990921     C                   eval      cmzmbd = vlrhdl
043200990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
043300000710     C                   if        prmfir = *blanks
043400030616     C                   eval      cmzfla = 'EDIVAD0F'
043500030616     C                   eval      cmzmba = 'EDIVAD0F'
043600000710     C                   else
043700000710     C                   eval      cmzfla = prmfir
043800000710     C                   eval      cmzmba = prmfir
043900000710     C                   endif
044000990921     C                   eval      cmznrr = *zeros
044100990921     C                   move      §ctdok        cmznrr
044200021003     C                   eval      cmzlba = vlrfl1
044300990921     C                   call(e)   'TIS711C'
044400990921     C                   parm                    dscmz
044500990921     C                   parm      *blanks       esito
044600990921     C                   if        %error
044700990921     C                             or cmzerr = '1'
044800990921     C                             or esito  = '1'
044900000710     C                   eval      wrkesito = '3'
045000990921     C                   endif
045100990921     C*
045200000613     C                   ENDSR
045300000613     C***
045400990914
045500010531
045600010531
045700010531
045800010531      /TITLE Invio dei dati al punto operativo.
045900010531     C     opeini        BEGSR
046000010531     C*
046100010531     C* Inizializzo flag e contatori operativi
046200010531     C                   movel     '0'           flgGiro           1
046300010531     C                   movel     '0'           flgMulti          1
046400010531     C                   movel     '1'           flgStato          1
046500010615     C                   movel     '0'           flgOk             1
046600010531     C                   z-add     *zeros        cntNonEl         10 0
046700010531     C                   z-add     *zeros        depfil            3 0
046800010531     C                   z-add     *zeros        invfil            3 0
046900030407     C*
047000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
047100030407     C                   if        %subst(vlrppt:1:1) = 'S'
047200030407     C                   seton                                        50
047300030407     C                   else
047400030407     C                   setoff                                       50
047500030407     C                   endif
047600010531     C*
047700010531     C                   ENDSR
047800010531     C***
047900010531
048000051212
048100051212
048200051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
048300051212     C     exeBon        BEGSR
048400051212     C*
048500051212     C* Imposto i parametri x il *pgm d "bonifica"
048600051212     C                   EVAL      ElencoChar = *blanks
048700051212     C                   EVAL      TipoElenco = *blanks
048800051212     C                   EVAL      CharSost   = *blanks
048900051212     C                   EVAL      UpperCase  = '1'
049000051212     C                   EVAL      ChkNull    = '1'
049100051212     C                   EVAL      CharNull   = *blanks
049200051212     C*
049300051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
049400051212     C                   EVAL      TxtInOut   = VADCDP
049500051212     C                   EVAL      EsitoBon   = *blanks
049600051212     C*
049700051212     C                   CALL      'XCHKCHAR'
049800051212     C                   PARM                    TxtInOut
049900051212     C                   PARM                    ElencoChar
050000051212     C                   PARM                    TipoElenco
050100051212     C                   PARM                    CharSost
050200051212     C                   PARM                    UpperCase
050300051212     C                   PARM                    ChkNull
050400051212     C                   PARM                    CharNull
050500051212     C                   PARM                    EsitoBon
050600051212     C*
050700051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
050800051212     C                   IF        EsitoBon = '1'
050900051212     C                   EVAL      VADCDP = TxtInOut
051000051212     C                   ENDIF
051100051212     C*
051200051212     C                   ENDSR
051300051212
051400010531
051500010531
051600000613     C     *inzsr        BEGSR
051700990914     C*
051800990914     C     *entry        plist
051900990921     C                   parm                    tivlrds
052000990921     C                   parm      wrkesito      esito
052100000724     C                   parm                    prmlit
052200000710     C                   parm                    prmfir
052300030710     C*
052400030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
052500030710     C                   move(p)   vlrMSG        wNomeFile
052600030710     C*
052700030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
052800030710     C* del codice CMR dal nome del file importato.
052900030710     C                   if        wNomeFile <> *blanks AND
053000030710     C                             vlrppt<>*blanks
053100030710     C                   if        %subst(vlrppt:1:1) = 'S'
053200030710     C                   seton                                        50
053300030710     C                   else
053400030710     C                   setoff                                       50
053500030710     C                   endif
053600030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
053700030710     C                   move(p)   wPosDaA       wPosDa
053800030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
053900030710     C                   move(p)   wLungA        wLung
054000030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
054100030710     C                   seton                                        60
054200030710     C                   else
054300030710     C                   setoff                                       60
054400030710     C                   endif
054500010606     C*
054600010606     C* CALCOLA LA DATA CORRENTE
054700010606     C                   time                    wn14             14 0
054800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054900010606     C                   z-add     wn8           g08dat
055000010606     C                   z-add     *zeros        g08inv
055100010606     C                   movel     '0'           g08err
055200010606     C                   call      'XSRDA8'
055300010606     C                   parm                    wlbda8
055400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055500000613     C*
055600000613     C                   ENDSR
055700000613     C***
