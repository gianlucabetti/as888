000100020916      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200990914     H dftactgrp(*yes)
000300021014
000400000724     Fazorg01l  if   e           k disk
000500990914     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVADwwr  O    E             DISK
000700000613     Ftis7t2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900000616     Ftis7t2ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990914
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
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
004800990915     D wrkdata         s               d
004900990915     D wrkora          s               t
005000000613     D rrnum           s              6  0 INZ(*zeros)
005100000621     D recko           s            150    INZ(*blanks)
005200011123     D depcmd          s            150    INZ(*blanks)
005300010606     D*------------------
005400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010606     D*------------------
005600010606     D WLBDA8          DS                  INZ
005700010606     D  G08DAT                 1      8  0
005800010606     D  G08INV                 9     16  0
005900010606     D  G08ERR                17     17
006000010606     D  G08TGI                18     22  0
006100010606     D*
006200051212
006300051212     D TxtInOut        S           2048
006400051212     D ElencoChar      S            256
006500051212     D TipoElenco      S              1
006600051212     D CharSost        S              1
006700051212     D UpperCase       S              1
006800051212     D ChkNull         S              1
006900051212     D CharNull        S              1
007000051212     D EsitoBon        S              1
007100010606
007200010606
007300010606
007400990914
007500990915     C                   time                    wrkdata
007600990915     C                   time                    wrkora
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000010531     C*
008100010531     C                   exsr      opeini
008200010531     C                   exsr      rwvad
008300010531     C*
008400010531     C                   seton                                        lr
008500010531
008600010531
008700010531
008800010531
008900010531     C*--------------------------------------------------------
009000010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009100010531     C*--------------------------------------------------------
009200010531     C     PREELA        BEGSR
009300000724     C*
009400000724     C* SE OCCORRE SPEDIRE IN FILIALE
009500010531     C                   if        invfil <> *zeros and
009600010531     C                             flgGiro = '0'
009700010531     C*
009800010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009900010531     C                   eval      flgGiro = '1'
010000000724     C*
010100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010300010531     C     invfil        chain     azorg01l
010400000724     C                   if        %found
010500000724     C                   movel(p)  CMD(1)        depcmd
010600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010700000724     C*
010800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010900011123     C                   Z-ADD     150           LENGH            15 5
011000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011100000724     C                   PARM                    depcmd
011200000724     C                   PARM                    LENGH
011300000724     C*
011400000724     C                   endif
011500000616     C*
011600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011700000616     C                   movel(p)  CMD(2)        depcmd
011800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011900000616     C*
012000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012100011123     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    depcmd
012400000616     C                   PARM                    LENGH
012500990914     C*
012600000616     C                   if        not %open(tis7t2ps)
012700000616     C                   open      tis7t2ps
012800030407     C   50              except    testdett
012900000613     C                   endif
013000010531     C*
013100010531     C                   endif
013200010531     C*
013300010531     C                   ENDSR
013400010531     C***
013500010531
013600010531
013700010531
013800010531
013900010531
014000010531
014100010531
014200010531     C*--------------------------------------------------------
014300010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014400010531     C*--------------------------------------------------------
014500010531     C     ENDELA        BEGSR
014600010531     C*
014700010531     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014800000613     C*
014900000616     C                   if        %open(tis7t2ps)
015000030407     C   50              except    findett
015100000616     C                   close     tis7t2ps
015200000613     C                   endif
015300000616     C*
015400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015500011123     C                   Z-ADD     150           LENGH            15 5
015600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015700000616     C                   PARM                    CMD(3)
015800000616     C                   PARM                    LENGH
015900000616     C*
016000010531     C                   ENDSR
016100010531     C***
016200010531
016300010531
016400000613
016500000613
016600000613
016700000613     C*--------------------------------------------------------
016800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016900000613     C*--------------------------------------------------------
017000000621     C     STPR          BEGSR
017100000613     C*
017200000613     C                   if        not %open(tis7t2p)
017300000613     C                   open      tis7t2p
017400990915     C                   endif
017500990915     C*
017600030407     C   50              except    riepilogo
017700990915     C*
017800000613     C                   if        %open(tis7t2p)
017900000613     C                   close     tis7t2p
018000990914     C                   endif
018100000613     C*
018200000613     C                   ENDSR
018300000613     C***
018400990914
018500010531
018600010531
018700010531
018800010531
018900990914     C*--------------------------------------------------------
019000020916     C* RWVAD   LEGGE FNVADAAR E SCRIVE FIVAD00F              *
019100990914     C*--------------------------------------------------------
019200990914     C     RWVAD         BEGSR
019300990914     C*
019400990914     C                   if        not %open(tivin00r)
019500990914     C                   open      tivin00r
019600990914     C                   endif
019700020916     C                   if        not %open(fivadwwr)
019800020916     C                   open      fivadwwr
019900990914     C                   endif
020000990914     C*
020100990914     C                   clear                   §CTDOK
020200990914     C                   clear                   §CTDNO
020300990914     C*
020400990914     C                   DO        *HIVAL
020500010618     C*
020600990914     C                   READ      tivin00r                               70
020700010618     C*
020800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020900010618     C                   if        vinflg = '1'
021000010618     C                   eval      flgOk = '1'
021100010618     C                   endif
021200010618     C*
021300000905     C                   if        vindta > *blanks
021400000613     C                   add       1             rrnum
021500990914     C*
021600010601     C                   if        *in70 = *off and
021700990914     C                             (vinflg = *blanks
021800010711     C                              or vinflg = '0'
021900010711     C                              or vinflg = '2')
022000990914     C*
022100020916     C                   clear                   FIVAD000
022200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022300010711     C                   if        vinflg = *blanks or vinflg = '0'
022400010711     C                   clear                   vinmsg
022500010711     C                   endif
022600010711     C*
022700990914     C                   eval      fnvadaar = vindta
022800991022     C*
022900991022     C                   testb     x'3F'         vhdatb                   02
023000991022     C                   if        not *in02
023100991022     C*
023200990914     C                   SETOFF                                       303132
023300990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023400010531     C*
023500010531     C                   TESTN                   VHDLNP               30
023600010531     C  N30              TESTN                   VHDLNP                 30
023700010531     C  N30              TESTN                   VHDLNP                   30
023800010531     C                   MOVE      VHDLNP        W001A             1
023900010531     C     *IN30         IFEQ      *OFF
024000010531     C     W001A         ORLT      '0'
024100010531     C                   SETON                                        31
024200010531     C                   eval      vinmsg = %trimr(vinmsg)
024300010531     C                             + ' ' + 'VHDLNP'
024400010531     C                   ELSE
024500010531     C                   MOVE      VHDLNP        VADLNP
024600010531     C     VADLNP        IFLE      *ZEROS
024700010531     C                   SETON                                        31
024800010531     C                   eval      vinmsg = %trimr(vinmsg)
024900010531     C                             + ' ' + 'VHDLNP'
025000010531     C                   ENDIF
025100010531     C                   ENDIF
025200010531     C*
025300990914     C                   TESTN                   VHDCCM               30
025400990914     C  N30              TESTN                   VHDCCM                 30
025500990914     C  N30              TESTN                   VHDCCM                   30
025600990914     C                   MOVE      VHDCCM        W001A             1
025700990914     C     *IN30         IFEQ      *OFF
025800990914     C     W001A         ORLT      '0'
025900990914     C                   SETON                                        31
026000010604     C                   eval      vinmsg = %trimr(vinmsg)
026100010604     C                             + ' ' + 'VHDCCM'
026200990914     C                   ELSE
026300990914     C                   MOVE      VHDCCM        VADCCM
026400990914     C     VADCCM        IFLE      *ZEROS
026500990914     C                   SETON                                        31
026600010604     C                   eval      vinmsg = %trimr(vinmsg)
026700010604     C                             + ' ' + 'VHDCCM'
026800990914     C                   ENDIF
026900990914     C                   ENDIF
027000070115     C*
027100070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027200070115     C                   eval      depfil = VADLNP
027300070115     C*
027400070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
027500070115     C* cliente
027600070507     C                   if        VADCCM = 0893943 OR
027601070507     C                             VADCCM = 0894169
027700070115     C                   eval      depfil = 050
027800070115     C                   eval      invfil = 050
027900070115     C                   eval      vadfgs = 050
028000070115     C                   endif
028100070507     C                   if        VADCCM = 0893942 OR
028101070507     C                             VADCCM = 0894168
028200070115     C                   eval      depfil = 005
028300070115     C                   eval      invfil = 005
028400070115     C                   eval      vadfgs = 005
028500070115     C                   endif
028600070115     C*
028700070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028800070115     C                   exsr      repfil
028900070115     C                   if        depfil = invfil
029000070115     C                   if        vlrpoi = 999
029100070115     C                   MOVE(p)   invfil        VADFGS
029200070115     C                   else
029300070115     C                   MOVE(p)   vlrpoi        VADFGS
029400070115     C                   endif
029500070115     C*
029600070115     C                   exsr      PREELA
029700010601     C*
029800010601     C                   TESTN                   VHDAAS               30
029900010601     C  N30              TESTN                   VHDAAS                 30
030000010601     C  N30              TESTN                   VHDAAS                   30
030100010601     C                   MOVE      VHDAAS        W001A             1
030200010601     C     *IN30         IFEQ      *OFF
030300010601     C     W001A         ORLT      '0'
030400010601     C                   SETON                                        32
030500010606     C                   MOVEL     datcor        VADAAS
030600010601     C                   eval      vinmsg = %trimr(vinmsg)
030700010601     C                             + ' ' + 'VHDAAS'
030800010601     C                   ELSE
030900010601     C                   MOVE      VHDAAS        VADAAS
031000010601     C                   ENDIF
031100010601     C*
031200010601     C                   TESTN                   VHDNRS               30
031300010601     C  N30              TESTN                   VHDNRS                 30
031400010601     C  N30              TESTN                   VHDNRS                   30
031500010601     C                   MOVE      VHDNRS        W001A             1
031600010601     C     *IN30         IFEQ      *OFF
031700010601     C     W001A         ORLT      '0'
031800010601     C     W001A         ANDNE     *BLANK
031900010601     C                   SETON                                        32
032000010601     C                   MOVE      *ZEROS        VADNRS
032100010601     C                   eval      vinmsg = %trimr(vinmsg)
032200010601     C                             + ' ' + 'VHDNRS'
032300010601     C                   ELSE
032400010601     C                   MOVE      VHDNRS        VADNRS
032500010601     C                   ENDIF
032600010601     C*
032700010601     C                   TESTN                   VHDNSP               30
032800010601     C  N30              TESTN                   VHDNSP                 30
032900010601     C  N30              TESTN                   VHDNSP                   30
033000010601     C                   MOVE      VHDNSP        W001A             1
033100010601     C     *IN30         IFEQ      *OFF
033200010601     C     W001A         ORLT      '0'
033300010601     C                   SETON                                        32
033400010601     C                   MOVE      *ZEROS        VADNSP
033500010601     C                   eval      vinmsg = %trimr(vinmsg)
033600010601     C                             + ' ' + 'VHDNSP'
033700010601     C                   ELSE
033800010601     C                   MOVE      VHDNSP        VADNSP
033900010601     C                   ENDIF
034000010601 FG  C*
034100010601 xxx C                   IF        VHDNCL = *blanks and
034200010601 "   C                             VADNRS > *zeros
034300010601 "   C                   MOVE      '00001'       VHDNCL
034400010601 FG  C                   ENDIF
034500010601     C*
034600010601     C                   TESTN                   VHDNCL               30
034700010601     C  N30              TESTN                   VHDNCL                 30
034800010601     C  N30              TESTN                   VHDNCL                   30
034900010601     C                   MOVE      VHDNCL        W001A             1
035000010601     C     *IN30         IFEQ      *OFF
035100010601     C     W001A         ORLT      '0'
035200010601     C                   SETON                                        32
035300010601     C                   MOVE      *ZEROS        VADNCL
035400010601     C                   eval      vinmsg = %trimr(vinmsg)
035500010601     C                             + ' ' + 'VHDNCL'
035600010601     C                   ELSE
035700010601     C                   MOVE      VHDNCL        VADNCL
035800010601     C                   ENDIF
035900010601     C*
036000010601     C                   TESTN                   VHDNCD               30
036100010601     C  N30              TESTN                   VHDNCD                 30
036200010601     C  N30              TESTN                   VHDNCD                   30
036300010601     C                   MOVE      VHDNCD        W001A             1
036400010601     C     *IN30         IFEQ      *OFF
036500010601     C     W001A         ORLT      '0'
036600010601     C     W001A         ANDNE     *BLANK
036700010601     C                   SETON                                        32
036800010601     C                   MOVE      *ZEROS        VADNCD
036900010601     C                   eval      vinmsg = %trimr(vinmsg)
037000010601     C                             + ' ' + 'VHDNCD'
037100010601     C                   ELSE
037200010601     C                   MOVE      VHDNCD        VADNCD
037300010601     C                   ENDIF
037400010601     C*
037500010601     C                   TESTN                   VHDNCA               30
037600010601     C  N30              TESTN                   VHDNCA                 30
037700010601     C  N30              TESTN                   VHDNCA                   30
037800010601     C                   MOVE      VHDNCA        W001A             1
037900010601     C     *IN30         IFEQ      *OFF
038000010601     C     W001A         ORLT      '0'
038100010601     C     W001A         ANDNE     *BLANK
038200010601     C                   SETON                                        32
038300010601     C                   MOVE      *ZEROS        VADNCA
038400010601     C                   eval      vinmsg = %trimr(vinmsg)
038500010601     C                             + ' ' + 'VHDNCA'
038600010601     C                   ELSE
038700010601     C                   MOVE      VHDNCA        VADNCA
038800010601     C                   ENDIF
038900010601     C*
039000010601     C* ALFABETICI
039100010601     C                   MOVE      VHDATB        VADATB
039200010601     C                   MOVE      VHDCDP        VADCDP
039300070108     C*
039400070108     C* "bonifico" i dati di input da eventuali caratteri indesiderati
039500070108     C                   exsr      exeBon
039600010601     C*
039700010604     C  N31              ADD       1             §CTDOK            7 0
039800010601     C   32
039900010604     COR 31              ADD       1             §CTDNO            7 0
040000020916     C  N31              WRITE     FIVAD000
040100010601     C*
040200010601     C                   if        *in31 = *off and
040300010601     C                             *in32 = *off
040400990914     C                   eval      vinflg = '1'
040500990914     C                   else
040600000621     C                   eval      recko = vindta
040700010612     C                   if        %open(tis7t2ps)
040800030407     C   50              except    rigadett
040900010612     C                   endif
041000990914     C                   eval      vinflg = '2'
041100990914     C                   endif
041200000905     C                   endif
041300000905     C                   endif
041400010601     C*
041500010601     C                   endif
041600000905     C*
041700000905     C                   else
041800000905     C                   eval      vinflg = '1'
041900000905     C                   endif
042000000905     C*
042100000905     C  N70              update    tivin000
042200991022     C*
042300991022     C  N70              ENDdo
042400010531     C
042500010531     C*
042600010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
042700010531     C                   if        cntNonEl = *zeros or
042800010531     C                             flgMulti = '0'
042900990914     C* Se non ci sono record con errori ...
043000010531     C                   if        §ctdno = 0 and
043100010531     C                             flgStato <> '2'
043200990914     C* ... restituisco esito OK.
043300990921     C                   eval      wrkesito = '0'
043400990914     C                   else
043500000710     C                   if        §ctdok > 0
043600990921     C                   eval      wrkesito = '1'
043700000710     C                   else
043800010615     C                   if        flgOk = '0'
043900010615     C                   eval      wrkesito = '2'
044000010615     C                   else
044100010615     C                   eval      wrkesito = '6'
044200010615     C                   endif
044300990914     C                   endif
044400000710     C                   endif
044500010531     C                   else
044600010531     C                   eval      wrkesito = '9'
044700010531     C                   endif
044800990914     C*
044900990914     C                   if        %open(tivin00r)
045000990914     C                   close     tivin00r
045100990914     C                   endif
045200020916     C                   if        %open(fivadwwr)
045300020916     C                   close     fivadwwr
045400990914     C                   endif
045500010531     C*
045600010531     C                   if        vlrpoi <> 999
045700010531     C                   eval      invfil = vlrpoi
045800010531     C                   endif
045900990914     C*
046000990921     C                   if        §ctdok > 0
046100010531     C                             and invfil > *zeros
046200990921     C                   exsr      invio
046300990921     C                   endif
046400010612     C*
046500010612     C                   if        flgGiro = '1'
046600010612     C                   exsr      endela
046700010612     C                   endif
046800990921     C*
046900990914     C                   ENDSR
047000000613     C***
047100990921
047200010531
047300010531
047400010531
047500010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047600010531     C     repfil        BEGSR
047700010531     C*
047800010531     C                   if        invfil = *zeros and
047900010531     C                             depfil > *zeros and
048000010613     C                             (vinflg = *blanks or
048100010613     C                              vinflg = *zeros)
048200010531     C
048300010531     C                   eval      invfil = depfil
048400010531     C                   endif
048500010531     C*
048600010531     C                   if        depfil <> invfil and
048700010531     C                             invfil > *zeros
048800010531     C                   eval      flgMulti = '1'
048900010531     C                   if        vinflg = *blanks
049000010531     C                   add       1             cntNonEl
049100010531     C                   endif
049200010531     C                   endif
049300010531     C*
049400010531     C                   if        vinflg = '2'
049500010531     C                   eval      flgStato = '2'
049600010531     C                   endif
049700010531     C*
049800010531     C                   ENDSR
049900010531     C***
050000010531
050100010531
050200010531
050300010531
050400010531
050500990921      /TITLE Invio dei dati al punto operativo.
050600000613     C     invio         BEGSR
050700990921     C*
050800990921     C                   reset                   dscmz
050900010531     C                   move      invfil        cmzdst
051000990921     C                   eval      cmzfld = vlrfou
051100990921     C                   eval      cmzmbd = vlrhdl
051200990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
051300000710     C                   if        prmfir = *blanks
051400020916     C                   eval      cmzfla = 'FIVAD00F'
051500020916     C                   eval      cmzmba = 'FIVAD00F'
051600000710     C                   else
051700000710     C                   eval      cmzfla = prmfir
051800000710     C                   eval      cmzmba = prmfir
051900000710     C                   endif
052000990921     C                   eval      cmznrr = *zeros
052100990921     C                   move      §ctdok        cmznrr
052200021003     C                   eval      cmzlba = vlrfl1
052300990921     C                   call(e)   'TIS711C'
052400990921     C                   parm                    dscmz
052500990921     C                   parm      *blanks       esito
052600990921     C                   if        %error
052700990921     C                             or cmzerr = '1'
052800990921     C                             or esito  = '1'
052900000710     C                   eval      wrkesito = '3'
053000990921     C                   endif
053100990921     C*
053200000613     C                   ENDSR
053300000613     C***
053400990914
053500010531
053600010531
053700010531
053800010531      /TITLE Invio dei dati al punto operativo.
053900010531     C     opeini        BEGSR
054000010531     C*
054100010531     C* Inizializzo flag e contatori operativi
054200010531     C                   movel     '0'           flgGiro           1
054300010531     C                   movel     '0'           flgMulti          1
054400010531     C                   movel     '1'           flgStato          1
054500010615     C                   movel     '0'           flgOk             1
054600010531     C                   z-add     *zeros        cntNonEl         10 0
054700010531     C                   z-add     *zeros        depfil            3 0
054800010531     C                   z-add     *zeros        invfil            3 0
054900030407     C*
055000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
055100030407     C                   if        %subst(vlrppt:1:1) = 'S'
055200030407     C                   seton                                        50
055300030407     C                   else
055400030407     C                   setoff                                       50
055500030407     C                   endif
055600010531     C*
055700010531     C                   ENDSR
055800010531     C***
055900010531
056000051212
056100051212
056200051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
056300051212     C     exeBon        BEGSR
056400051212     C*
056500051212     C* Imposto i parametri x il *pgm d "bonifica"
056600051212     C                   EVAL      ElencoChar = *blanks
056700051212     C                   EVAL      TipoElenco = *blanks
056800051212     C                   EVAL      CharSost   = *blanks
056900051212     C                   EVAL      UpperCase  = '1'
057000051212     C                   EVAL      ChkNull    = '1'
057100051212     C                   EVAL      CharNull   = *blanks
057200051212     C*
057300051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
057400051212     C                   EVAL      TxtInOut   = VADCDP
057500051212     C                   EVAL      EsitoBon   = *blanks
057600051212     C*
057700051212     C                   CALL      'XCHKCHAR'
057800051212     C                   PARM                    TxtInOut
057900051212     C                   PARM                    ElencoChar
058000051212     C                   PARM                    TipoElenco
058100051212     C                   PARM                    CharSost
058200051212     C                   PARM                    UpperCase
058300051212     C                   PARM                    ChkNull
058400051212     C                   PARM                    CharNull
058500051212     C                   PARM                    EsitoBon
058600051212     C*
058700051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
058800051212     C                   IF        EsitoBon = '1'
058900051212     C                   EVAL      VADCDP = TxtInOut
059000051212     C                   ENDIF
059100051212     C*
059200051212     C                   ENDSR
059300051212
059400010531
059500010531
059600010531
059700000613     C     *inzsr        BEGSR
059800990914     C*
059900990914     C     *entry        plist
060000990921     C                   parm                    tivlrds
060100990921     C                   parm      wrkesito      esito
060200000724     C                   parm                    prmlit
060300000710     C                   parm                    prmfir
060400010606     C*
060500010606     C* CALCOLA LA DATA CORRENTE
060600010606     C                   time                    wn14             14 0
060700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060800010606     C                   z-add     wn8           g08dat
060900010606     C                   z-add     *zeros        g08inv
061000010606     C                   movel     '0'           g08err
061100010606     C                   call      'XSRDA8'
061200010606     C                   parm                    wlbda8
061300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061400000613     C*
061500000613     C                   ENDSR
061600000613     C***
061700990914
061800000613     Otis7t2p   E            riepilogo         2
061900990915     O                                              'Upload via Internet'
062000990915     O                                           +1 'Traduzione TIVIN00R -'
062100020916     O                                           +1 'FIVADWWR'
062200990915     O                                           +1 'Dettaglio colli'
062300990915     O          E            riepilogo   2
062400990915     O                       wrkdata
062500990915     O                       wrkora              +1
062600990915     O                       procname            +1
062700990915     O          E            riepilogo   2
062800990915     O                                              'Cliente..................:'
062900990915     O                       VADCCM        z     +1
063000990915     O          E            riepilogo   2
063100990921     O                                              'Riferimento strategi.....:'
063200990921     O                       vlrhdl              +1
063300990915     O          E            riepilogo   2
063400990915     O                                              'Giusti...................:'
063500990915     O                       §CTDOK        2   +  1
063600990915     O          E            riepilogo   2
063700010601     O                                              'Sbagliati ...............:'
063800990915     O                       §CTDNO        2   +  1
063900000613
064000000616     Otis7t2ps  E            testdett          2
064100000613     O                                              'Upload via Internet'
064200000613     O                                           +1 'Traduzione TIVIN00R -'
064300020916     O                                           +1 'FIVADWWR'
064400000613     O                                           +1 'Dettaglio colli'
064500000616     O          E            testdett    3
064600000613     O                                           +2 'N° rec'
064700000613     O                                           +3 'Anteprima contenuto'
064800000616     O          E            rigadett    2
064900000613     O                       rrnum               +2
065000000621     O                       recko               +3
065100000616     O          E            findett     2
065200000613     O                       wrkdata
065300000613     O                       wrkora              +1
065400000613     O                       procname            +1
065500000616     O          E            findett     2
065600000613     O                                              'Cliente..................:'
065700000613     O                       VADCCM        z     +1
065800000616     O          E            findett     2
065900000613     O                                              'Riferimento strategi.....:'
066000000613     O                       vlrhdl              +1
066100000616     O          E            findett     2
066200000613     O                                              'Giusti...................:'
066300000613     O                       §CTDOK        2   +  1
066400000616     O          E            findett     2
066500010601     O                                              'Sbagliati ...............:'
066600000613     O                       §CTDNO        2   +  1
066700000613** CMD - COMANDI CL
066800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T2P)  FORMTYPE(RICCLI) OUTQ(
066900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T2PS) FORMTYPE(RICCLI) OUTQ(
067000000616DLTOVR FILE(TIS7T2P TIS7T2PS) LVL(*)
067100000613
067200000613
