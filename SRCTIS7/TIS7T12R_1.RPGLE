000100030616      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200130115     H DFTACTGRP(*no) ACTGRP(*caller)
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
004100051212
004200051212     D TxtInOut        S           2048
004300051212     D ElencoChar      S            256
004400051212     D TipoElenco      S              1
004500051212     D CharSost        S              1
004600051212     D UpperCase       S              1
004700051212     D ChkNull         S              1
004800051212     D CharNull        S              1
004900051212     D EsitoBon        S              1
005000010606
005100010606
005200010606
005300990914
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700010531     C*
005800010531     C                   exsr      opeini
005900010531     C                   exsr      rwvad
006000010531     C*
006100010531     C                   seton                                        lr
006200010531
006300010531
006400010531
006500010531
006600010531     C*--------------------------------------------------------
006700010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006800010531     C*--------------------------------------------------------
006900010531     C     PREELA        BEGSR
007000000724     C*
007100000724     C* SE OCCORRE SPEDIRE IN FILIALE
007200010531     C                   if        invfil <> *zeros and
007300010531     C                             flgGiro = '0'
007400010531     C*
007500010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007600010531     C                   eval      flgGiro = '1'
007700010531     C*
007800010531     C                   endif
007900010531     C*
008000010531     C                   ENDSR
008100010531     C***
008200010531
008300010531
008400010531
008500010531
008600010531
008700010531
008800010531
008900010531     C*--------------------------------------------------------
009000010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009100010531     C*--------------------------------------------------------
009200010531     C     ENDELA        BEGSR
009300000616     C*
009400010531     C                   ENDSR
009500010531     C***
009600990914
009700010531
009800010531
009900010531
010000010531
010100990914     C*--------------------------------------------------------
010200030616     C* RWVAD  LEGGE TIVIN00R E SCRIVE EDIVAD0F              *
010300990914     C*--------------------------------------------------------
010400990914     C     RWVAD         BEGSR
010500990914     C*
010600990914     C                   if        not %open(tivin00r)
010700990914     C                   open      tivin00r
010800990914     C                   endif
010900030616     C                   if        not %open(edivadwr)
011000030616     C                   open      edivadwr
011100990914     C                   endif
011200990914     C*
011300990914     C                   clear                   §CTDOK
011400990914     C                   clear                   §CTDNO
011500990914     C*
011600990914     C                   DO        *HIVAL
011700010618     C*
011800990914     C                   READ      tivin00r                               70
011900010618     C*
012000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012100010618     C                   if        vinflg = '1'
012200010618     C                   eval      flgOk = '1'
012300010618     C                   endif
012400010618     C*
012500000905     C                   if        vindta > *blanks
012600000613     C                   add       1             rrnum
012700990914     C*
012800010601     C                   if        *in70 = *off and
012900990914     C                             (vinflg = *blanks
013000010711     C                              or vinflg = '0'
013100010711     C                              or vinflg = '2')
013200990914     C*
013300030616     C                   clear                   EDIVAD00
013400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013500010711     C                   if        vinflg = *blanks or vinflg = '0'
013600010711     C                   clear                   vinmsg
013700010711     C                   endif
013800010711     C*
013900990914     C                   eval      fnvadaar = vindta
014000991022     C*
014100991022     C                   testb     x'3F'         vhdatb                   02
014200991022     C                   if        not *in02
014300991022     C*
014400990914     C                   SETOFF                                       303132
014500990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
014600010531     C*
014700010531     C                   TESTN                   VHDLNP               30
014800010531     C  N30              TESTN                   VHDLNP                 30
014900010531     C  N30              TESTN                   VHDLNP                   30
015000010531     C                   MOVE      VHDLNP        W001A             1
015100010531     C     *IN30         IFEQ      *OFF
015200010531     C     W001A         ORLT      '0'
015300010531     C                   SETON                                        31
015400010531     C                   eval      vinmsg = %trimr(vinmsg)
015500010531     C                             + ' ' + 'VHDLNP'
015600010531     C                   ELSE
015700010531     C                   MOVE      VHDLNP        VADLNP
015800010531     C     VADLNP        IFLE      *ZEROS
015900010531     C                   SETON                                        31
016000010531     C                   eval      vinmsg = %trimr(vinmsg)
016100010531     C                             + ' ' + 'VHDLNP'
016200010531     C                   ENDIF
016300010531     C                   ENDIF
016400010531     C*
016500010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
016600010531     C                   eval      depfil = VADLNP
016700010531     C                   exsr      repfil
016800010601     C                   if        depfil = invfil
016900021014     C                   if        vlrpoi = 999
017000021010     C                   MOVE(p)   invfil        VADFGS
017100021014     C                   else
017200021014     C                   MOVE(p)   vlrpoi        VADFGS
017300021014     C                   endif
017400010531     C*
017500010531     C                   exsr      PREELA
017600010531     C*
017700990914     C                   TESTN                   VHDCCM               30
017800990914     C  N30              TESTN                   VHDCCM                 30
017900990914     C  N30              TESTN                   VHDCCM                   30
018000990914     C                   MOVE      VHDCCM        W001A             1
018100990914     C     *IN30         IFEQ      *OFF
018200990914     C     W001A         ORLT      '0'
018300990914     C                   SETON                                        31
018400010604     C                   eval      vinmsg = %trimr(vinmsg)
018500010604     C                             + ' ' + 'VHDCCM'
018600990914     C                   ELSE
018700990914     C                   MOVE      VHDCCM        VADCCM
018800990914     C     VADCCM        IFLE      *ZEROS
018900990914     C                   SETON                                        31
019000010604     C                   eval      vinmsg = %trimr(vinmsg)
019100010604     C                             + ' ' + 'VHDCCM'
019200990914     C                   ENDIF
019300990914     C                   ENDIF
019400010601     C*
019500010601     C                   TESTN                   VHDAAS               30
019600010601     C  N30              TESTN                   VHDAAS                 30
019700010601     C  N30              TESTN                   VHDAAS                   30
019800010601     C                   MOVE      VHDAAS        W001A             1
019900010601     C     *IN30         IFEQ      *OFF
020000010601     C     W001A         ORLT      '0'
020100010601     C                   SETON                                        32
020200010606     C                   MOVEL     datcor        VADAAS
020300010601     C                   eval      vinmsg = %trimr(vinmsg)
020400010601     C                             + ' ' + 'VHDAAS'
020500010601     C                   ELSE
020600010601     C                   MOVE      VHDAAS        VADAAS
020700010601     C                   ENDIF
020800010601     C*
020900010601     C                   TESTN                   VHDNRS               30
021000010601     C  N30              TESTN                   VHDNRS                 30
021100010601     C  N30              TESTN                   VHDNRS                   30
021200010601     C                   MOVE      VHDNRS        W001A             1
021300010601     C     *IN30         IFEQ      *OFF
021400010601     C     W001A         ORLT      '0'
021500010601     C     W001A         ANDNE     *BLANK
021600010601     C                   SETON                                        32
021700010601     C                   MOVE      *ZEROS        VADNRS
021800010601     C                   eval      vinmsg = %trimr(vinmsg)
021900010601     C                             + ' ' + 'VHDNRS'
022000010601     C                   ELSE
022100010601     C                   MOVE      VHDNRS        VADNRS
022200010601     C                   ENDIF
022300010601     C*
022400010601     C                   TESTN                   VHDNSP               30
022500010601     C  N30              TESTN                   VHDNSP                 30
022600010601     C  N30              TESTN                   VHDNSP                   30
022700010601     C                   MOVE      VHDNSP        W001A             1
022800010601     C     *IN30         IFEQ      *OFF
022900010601     C     W001A         ORLT      '0'
023000010601     C                   SETON                                        32
023100010601     C                   MOVE      *ZEROS        VADNSP
023200010601     C                   eval      vinmsg = %trimr(vinmsg)
023300010601     C                             + ' ' + 'VHDNSP'
023400010601     C                   ELSE
023500010601     C                   MOVE      VHDNSP        VADNSP
023600010601     C                   ENDIF
023700010601 FG  C*
023800010601 xxx C                   IF        VHDNCL = *blanks and
023900010601 "   C                             VADNRS > *zeros
024000010601 "   C                   MOVE      '00001'       VHDNCL
024100010601 FG  C                   ENDIF
024200010601     C*
024300010601     C                   TESTN                   VHDNCL               30
024400010601     C  N30              TESTN                   VHDNCL                 30
024500010601     C  N30              TESTN                   VHDNCL                   30
024600010601     C                   MOVE      VHDNCL        W001A             1
024700010601     C     *IN30         IFEQ      *OFF
024800010601     C     W001A         ORLT      '0'
024900010601     C                   SETON                                        32
025000010601     C                   MOVE      *ZEROS        VADNCL
025100010601     C                   eval      vinmsg = %trimr(vinmsg)
025200010601     C                             + ' ' + 'VHDNCL'
025300010601     C                   ELSE
025400010601     C                   MOVE      VHDNCL        VADNCL
025500010601     C                   ENDIF
025600010601     C*
025700010601     C                   TESTN                   VHDNCD               30
025800010601     C  N30              TESTN                   VHDNCD                 30
025900010601     C  N30              TESTN                   VHDNCD                   30
026000010601     C                   MOVE      VHDNCD        W001A             1
026100010601     C     *IN30         IFEQ      *OFF
026200010601     C     W001A         ORLT      '0'
026300010601     C     W001A         ANDNE     *BLANK
026400010601     C                   SETON                                        32
026500010601     C                   MOVE      *ZEROS        VADNCD
026600010601     C                   eval      vinmsg = %trimr(vinmsg)
026700010601     C                             + ' ' + 'VHDNCD'
026800010601     C                   ELSE
026900010601     C                   MOVE      VHDNCD        VADNCD
027000010601     C                   ENDIF
027100010601     C*
027200010601     C                   TESTN                   VHDNCA               30
027300010601     C  N30              TESTN                   VHDNCA                 30
027400010601     C  N30              TESTN                   VHDNCA                   30
027500010601     C                   MOVE      VHDNCA        W001A             1
027600010601     C     *IN30         IFEQ      *OFF
027700010601     C     W001A         ORLT      '0'
027800010601     C     W001A         ANDNE     *BLANK
027900010601     C                   SETON                                        32
028000010601     C                   MOVE      *ZEROS        VADNCA
028100010601     C                   eval      vinmsg = %trimr(vinmsg)
028200010601     C                             + ' ' + 'VHDNCA'
028300010601     C                   ELSE
028400010601     C                   MOVE      VHDNCA        VADNCA
028500010601     C                   ENDIF
028600010601     C*
028700010601     C* ALFABETICI
028800010601     C                   MOVE      VHDATB        VADATB
028900010601     C                   MOVE      VHDCDP        VADCDP
029000030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
029100110131     C                   EVAL      VADCMR = 'WURTH - ' + %subst(wNomeFile:4:5)
029200030616     C                   EVAL      VADCNT = 1
029300051212
029400051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
029500051212     c                   exsr      exeBon
029600010601     C*
029700010604     C  N31              ADD       1             §CTDOK            7 0
029800010601     C   32
029900010604     COR 31              ADD       1             §CTDNO            7 0
030000030616     C  N31              WRITE     EDIVAD00
030100010601     C*
030200010601     C                   if        *in31 = *off and
030300010601     C                             *in32 = *off
030400990914     C                   eval      vinflg = '1'
030500990914     C                   else
030600990914     C                   eval      vinflg = '2'
030700990914     C                   endif
030800000905     C                   endif
030900000905     C                   endif
031000010601     C*
031100010601     C                   endif
031200000905     C*
031300000905     C                   else
031400000905     C                   eval      vinflg = '1'
031500000905     C                   endif
031600000905     C*
031700000905     C  N70              update    tivin000
031800991022     C*
031900991022     C  N70              ENDdo
032000010531     C
032100010531     C*
032200010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032300010531     C                   if        cntNonEl = *zeros or
032400010531     C                             flgMulti = '0'
032500990914     C* Se non ci sono record con errori ...
032600010531     C                   if        §ctdno = 0 and
032700010531     C                             flgStato <> '2'
032800990914     C* ... restituisco esito OK.
032900990921     C                   eval      wrkesito = '0'
033000990914     C                   else
033100000710     C                   if        §ctdok > 0
033200990921     C                   eval      wrkesito = '1'
033300000710     C                   else
033400010615     C                   if        flgOk = '0'
033500010615     C                   eval      wrkesito = '2'
033600010615     C                   else
033700010615     C                   eval      wrkesito = '6'
033800010615     C                   endif
033900990914     C                   endif
034000000710     C                   endif
034100010531     C                   else
034200010531     C                   eval      wrkesito = '9'
034300010531     C                   endif
034400990914     C*
034500990914     C                   if        %open(tivin00r)
034600990914     C                   close     tivin00r
034700990914     C                   endif
034800030616     C                   if        %open(edivadwr)
034900030616     C                   close     edivadwr
035000990914     C                   endif
035100010531     C*
035200010531     C                   if        vlrpoi <> 999
035300010531     C                   eval      invfil = vlrpoi
035400010531     C                   endif
035500990914     C*
035600990921     C                   if        §ctdok > 0
035700010531     C                             and invfil > *zeros
035800990921     C                   exsr      invio
035900990921     C                   endif
036000010612     C*
036100010612     C                   if        flgGiro = '1'
036200010612     C                   exsr      endela
036300010612     C                   endif
036400990921     C*
036500990914     C                   ENDSR
036600000613     C***
036700990921
036800010531
036900010531
037000010531
037100010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
037200010531     C     repfil        BEGSR
037300010531     C*
037400010531     C                   if        invfil = *zeros and
037500010531     C                             depfil > *zeros and
037600010613     C                             (vinflg = *blanks or
037700010613     C                              vinflg = *zeros)
037800010531     C
037900010531     C                   eval      invfil = depfil
038000010531     C                   endif
038100010531     C*
038200010531     C                   if        depfil <> invfil and
038300010531     C                             invfil > *zeros
038400010531     C                   eval      flgMulti = '1'
038500010531     C                   if        vinflg = *blanks
038600010531     C                   add       1             cntNonEl
038700010531     C                   endif
038800010531     C                   endif
038900010531     C*
039000010531     C                   if        vinflg = '2'
039100010531     C                   eval      flgStato = '2'
039200010531     C                   endif
039300010531     C*
039400010531     C                   ENDSR
039500010531     C***
039600010531
039700010531
039800010531
039900010531
040000010531
040100990921      /TITLE Invio dei dati al punto operativo.
040200000613     C     invio         BEGSR
040300990921     C*
040400990921     C                   reset                   dscmz
040500030618     C                   move      invfil        cmzdst
040600030618     C***                move      '789'         cmzdst
040700990921     C                   eval      cmzfld = vlrfou
040800990921     C                   eval      cmzmbd = vlrhdl
040900990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
041000000710     C                   if        prmfir = *blanks
041100030616     C                   eval      cmzfla = 'EDIVAD0F'
041200030616     C                   eval      cmzmba = 'EDIVAD0F'
041300000710     C                   else
041400000710     C                   eval      cmzfla = prmfir
041500000710     C                   eval      cmzmba = prmfir
041600000710     C                   endif
041700990921     C                   eval      cmznrr = *zeros
041800990921     C                   move      §ctdok        cmznrr
041900021003     C                   eval      cmzlba = vlrfl1
042000990921     C                   call(e)   'TIS711C'
042100990921     C                   parm                    dscmz
042200990921     C                   parm      *blanks       esito
042300990921     C                   if        %error
042400990921     C                             or cmzerr = '1'
042500990921     C                             or esito  = '1'
042600000710     C                   eval      wrkesito = '3'
042700990921     C                   endif
042800990921     C*
042900000613     C                   ENDSR
043000000613     C***
043100990914
043200010531
043300010531
043400010531
043500010531      /TITLE Invio dei dati al punto operativo.
043600010531     C     opeini        BEGSR
043700010531     C*
043800010531     C* Inizializzo flag e contatori operativi
043900010531     C                   movel     '0'           flgGiro           1
044000010531     C                   movel     '0'           flgMulti          1
044100010531     C                   movel     '1'           flgStato          1
044200010615     C                   movel     '0'           flgOk             1
044300010531     C                   z-add     *zeros        cntNonEl         10 0
044400010531     C                   z-add     *zeros        depfil            3 0
044500010531     C                   z-add     *zeros        invfil            3 0
044600030407     C*
044700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
044800030407     C                   if        %subst(vlrppt:1:1) = 'S'
044900030407     C                   seton                                        50
045000030407     C                   else
045100030407     C                   setoff                                       50
045200030407     C                   endif
045300010531     C*
045400010531     C                   ENDSR
045500010531     C***
045600010531
045700051212
045800051212
045900051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
046000051212     C     exeBon        BEGSR
046100051212     C*
046200051212     C* Imposto i parametri x il *pgm d "bonifica"
046300051212     C                   EVAL      ElencoChar = *blanks
046400051212     C                   EVAL      TipoElenco = *blanks
046500051212     C                   EVAL      CharSost   = *blanks
046600051212     C                   EVAL      UpperCase  = '1'
046700051212     C                   EVAL      ChkNull    = '1'
046800051212     C                   EVAL      CharNull   = *blanks
046900051212     C*
047000051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
047100051212     C                   EVAL      TxtInOut   = VADCDP
047200051212     C                   EVAL      EsitoBon   = *blanks
047300051212     C*
047400051212     C                   CALL      'XCHKCHAR'
047500051212     C                   PARM                    TxtInOut
047600051212     C                   PARM                    ElencoChar
047700051212     C                   PARM                    TipoElenco
047800051212     C                   PARM                    CharSost
047900051212     C                   PARM                    UpperCase
048000051212     C                   PARM                    ChkNull
048100051212     C                   PARM                    CharNull
048200051212     C                   PARM                    EsitoBon
048300051212     C*
048400051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
048500051212     C                   IF        EsitoBon = '1'
048600051212     C                   EVAL      VADCDP = TxtInOut
048700051212     C                   ENDIF
048800051212     C*
048900051212     C                   ENDSR
049000010531
049100010531
049200010531
049300000613     C     *inzsr        BEGSR
049400990914     C*
049500990914     C     *entry        plist
049600990921     C                   parm                    tivlrds
049700990921     C                   parm      wrkesito      esito
049800000724     C                   parm                    prmlit
049900000710     C                   parm                    prmfir
050000030616     C*
050100030616     C* SE RICHIESTO NEI PARAMETRI DI TRADUZIONE
050200030616     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" (SE C'E') E REPERISCO LA FILIALE A CUI INVIARE
050300030618     C                   MOVE(P)   vlrMSG        wNomeFile
050400101013     C                   IF        %subst(vlrPPT:2:1) = 'S'
050500101013     C                   SELECT
050600101013     C                   WHEN      %subst(wNomeFile:7:2) = '60' OR
050700101013     C                             %subst(wNomeFile:7:2) = '61' OR
050800101013     C                             %subst(wNomeFile:7:2) = '62' OR
050900101013     C                             %subst(wNomeFile:7:2) = '63' OR
051000101013     C                             %subst(wNomeFile:7:2) = '64' OR
051100101013     C                             %subst(wNomeFile:7:2) = '65' OR
051200101013     C                             %subst(wNomeFile:7:2) = '66' OR
051300101013     C                             %subst(wNomeFile:7:2) = '67' OR
051400101013     C                             %subst(wNomeFile:7:2) = '68' OR
051500101013     C                             %subst(wNomeFile:7:2) = '69'
051600101013     C                   EVAL      vlrpoi = 999
051700101013     C                   WHEN      %subst(wNomeFile:7:2) = '99'
051800101013     C                   EVAL      vlrpoi = 048
051900101013     C                   WHEN      %subst(wNomeFile:7:2) = '01' OR
052000101013     C                             %subst(wNomeFile:7:2) = '02' OR
052100101013     C                             %subst(wNomeFile:7:2) = '03' OR
052200101013     C                             %subst(wNomeFile:7:2) = '04' OR
052300101013     C                             %subst(wNomeFile:7:2) = '13' OR
052400101013     C                             %subst(wNomeFile:7:2) = '24' OR
052500101013     C                             %subst(wNomeFile:7:2) = '25' OR
052600101013     C                             %subst(wNomeFile:7:2) = '55'
052700101013     C                   EVAL      vlrpoi = 277
052800130220     C                   WHEN      %subst(wNomeFile:7:2) = '95' OR
052900130220     C                             %subst(wNomeFile:7:2) = '96'
053000130220     C                   EVAL      vlrpoi = 043
053100101013     C                   OTHER
053200101013     C                   EVAL      vlrpoi = 043
053300101013     C                   ENDSL
053400101013     C                   ENDIF
053500010606     C*
053600010606     C* CALCOLA LA DATA CORRENTE
053700100319     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
053800100319     C                   eval      datcor = %dec(%date() : *ISO)
053900000613     C*
054000000613     C                   ENDSR
054100000613     C***
