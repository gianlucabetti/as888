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
025300010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025400010531     C                   eval      depfil = VADLNP
025500010531     C                   exsr      repfil
025600010601     C                   if        depfil = invfil
025700021014     C                   if        vlrpoi = 999
025800021010     C                   MOVE(p)   invfil        VADFGS
025900021014     C                   else
026000021014     C                   MOVE(p)   vlrpoi        VADFGS
026100021014     C                   endif
026200010531     C*
026300010531     C                   exsr      PREELA
026400010531     C*
026500990914     C                   TESTN                   VHDCCM               30
026600990914     C  N30              TESTN                   VHDCCM                 30
026700990914     C  N30              TESTN                   VHDCCM                   30
026800990914     C                   MOVE      VHDCCM        W001A             1
026900990914     C     *IN30         IFEQ      *OFF
027000990914     C     W001A         ORLT      '0'
027100990914     C                   SETON                                        31
027200010604     C                   eval      vinmsg = %trimr(vinmsg)
027300010604     C                             + ' ' + 'VHDCCM'
027400990914     C                   ELSE
027500990914     C                   MOVE      VHDCCM        VADCCM
027600990914     C     VADCCM        IFLE      *ZEROS
027700990914     C                   SETON                                        31
027800010604     C                   eval      vinmsg = %trimr(vinmsg)
027900010604     C                             + ' ' + 'VHDCCM'
028000990914     C                   ENDIF
028100990914     C                   ENDIF
028200010601     C*
028300010601     C                   TESTN                   VHDAAS               30
028400010601     C  N30              TESTN                   VHDAAS                 30
028500010601     C  N30              TESTN                   VHDAAS                   30
028600010601     C                   MOVE      VHDAAS        W001A             1
028700010601     C     *IN30         IFEQ      *OFF
028800010601     C     W001A         ORLT      '0'
028900010601     C                   SETON                                        32
029000010606     C                   MOVEL     datcor        VADAAS
029100010601     C                   eval      vinmsg = %trimr(vinmsg)
029200010601     C                             + ' ' + 'VHDAAS'
029300010601     C                   ELSE
029400010601     C                   MOVE      VHDAAS        VADAAS
029500010601     C                   ENDIF
029600010601     C*
029700010601     C                   TESTN                   VHDNRS               30
029800010601     C  N30              TESTN                   VHDNRS                 30
029900010601     C  N30              TESTN                   VHDNRS                   30
030000010601     C                   MOVE      VHDNRS        W001A             1
030100010601     C     *IN30         IFEQ      *OFF
030200010601     C     W001A         ORLT      '0'
030300010601     C     W001A         ANDNE     *BLANK
030400010601     C                   SETON                                        32
030500010601     C                   MOVE      *ZEROS        VADNRS
030600010601     C                   eval      vinmsg = %trimr(vinmsg)
030700010601     C                             + ' ' + 'VHDNRS'
030800010601     C                   ELSE
030900010601     C                   MOVE      VHDNRS        VADNRS
031000010601     C                   ENDIF
031100010601     C*
031200010601     C                   TESTN                   VHDNSP               30
031300010601     C  N30              TESTN                   VHDNSP                 30
031400010601     C  N30              TESTN                   VHDNSP                   30
031500010601     C                   MOVE      VHDNSP        W001A             1
031600010601     C     *IN30         IFEQ      *OFF
031700010601     C     W001A         ORLT      '0'
031800010601     C                   SETON                                        32
031900010601     C                   MOVE      *ZEROS        VADNSP
032000010601     C                   eval      vinmsg = %trimr(vinmsg)
032100010601     C                             + ' ' + 'VHDNSP'
032200010601     C                   ELSE
032300010601     C                   MOVE      VHDNSP        VADNSP
032400010601     C                   ENDIF
032500010601 FG  C*
032600010601 xxx C                   IF        VHDNCL = *blanks and
032700010601 "   C                             VADNRS > *zeros
032800010601 "   C                   MOVE      '00001'       VHDNCL
032900010601 FG  C                   ENDIF
033000010601     C*
033100010601     C                   TESTN                   VHDNCL               30
033200010601     C  N30              TESTN                   VHDNCL                 30
033300010601     C  N30              TESTN                   VHDNCL                   30
033400010601     C                   MOVE      VHDNCL        W001A             1
033500010601     C     *IN30         IFEQ      *OFF
033600010601     C     W001A         ORLT      '0'
033700010601     C                   SETON                                        32
033800010601     C                   MOVE      *ZEROS        VADNCL
033900010601     C                   eval      vinmsg = %trimr(vinmsg)
034000010601     C                             + ' ' + 'VHDNCL'
034100010601     C                   ELSE
034200010601     C                   MOVE      VHDNCL        VADNCL
034300010601     C                   ENDIF
034400010601     C*
034500010601     C                   TESTN                   VHDNCD               30
034600010601     C  N30              TESTN                   VHDNCD                 30
034700010601     C  N30              TESTN                   VHDNCD                   30
034800010601     C                   MOVE      VHDNCD        W001A             1
034900010601     C     *IN30         IFEQ      *OFF
035000010601     C     W001A         ORLT      '0'
035100010601     C     W001A         ANDNE     *BLANK
035200010601     C                   SETON                                        32
035300010601     C                   MOVE      *ZEROS        VADNCD
035400010601     C                   eval      vinmsg = %trimr(vinmsg)
035500010601     C                             + ' ' + 'VHDNCD'
035600010601     C                   ELSE
035700010601     C                   MOVE      VHDNCD        VADNCD
035800010601     C                   ENDIF
035900010601     C*
036000010601     C                   TESTN                   VHDNCA               30
036100010601     C  N30              TESTN                   VHDNCA                 30
036200010601     C  N30              TESTN                   VHDNCA                   30
036300010601     C                   MOVE      VHDNCA        W001A             1
036400010601     C     *IN30         IFEQ      *OFF
036500010601     C     W001A         ORLT      '0'
036600010601     C     W001A         ANDNE     *BLANK
036700010601     C                   SETON                                        32
036800010601     C                   MOVE      *ZEROS        VADNCA
036900010601     C                   eval      vinmsg = %trimr(vinmsg)
037000010601     C                             + ' ' + 'VHDNCA'
037100010601     C                   ELSE
037200010601     C                   MOVE      VHDNCA        VADNCA
037300010601     C                   ENDIF
037400010601     C*
037500010601     C* ALFABETICI
037600010601     C                   MOVE      VHDATB        VADATB
037700010601     C                   MOVE      VHDCDP        VADCDP
037800051212
037900051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
038000051212     c                   exsr      exeBon
038100051212
038200010601     C*
038300010604     C  N31              ADD       1             §CTDOK            7 0
038400010601     C   32
038500010604     COR 31              ADD       1             §CTDNO            7 0
038600020916     C  N31              WRITE     FIVAD000
038700010601     C*
038800010601     C                   if        *in31 = *off and
038900010601     C                             *in32 = *off
039000990914     C                   eval      vinflg = '1'
039100990914     C                   else
039200000621     C                   eval      recko = vindta
039300010612     C                   if        %open(tis7t2ps)
039400030407     C   50              except    rigadett
039500010612     C                   endif
039600990914     C                   eval      vinflg = '2'
039700990914     C                   endif
039800000905     C                   endif
039900000905     C                   endif
040000010601     C*
040100010601     C                   endif
040200000905     C*
040300000905     C                   else
040400000905     C                   eval      vinflg = '1'
040500000905     C                   endif
040600000905     C*
040700000905     C  N70              update    tivin000
040800991022     C*
040900991022     C  N70              ENDdo
041000010531     C
041100010531     C*
041200010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
041300010531     C                   if        cntNonEl = *zeros or
041400010531     C                             flgMulti = '0'
041500990914     C* Se non ci sono record con errori ...
041600010531     C                   if        §ctdno = 0 and
041700010531     C                             flgStato <> '2'
041800990914     C* ... restituisco esito OK.
041900990921     C                   eval      wrkesito = '0'
042000990914     C                   else
042100000710     C                   if        §ctdok > 0
042200990921     C                   eval      wrkesito = '1'
042300000710     C                   else
042400010615     C                   if        flgOk = '0'
042500010615     C                   eval      wrkesito = '2'
042600010615     C                   else
042700010615     C                   eval      wrkesito = '6'
042800010615     C                   endif
042900990914     C                   endif
043000000710     C                   endif
043100010531     C                   else
043200010531     C                   eval      wrkesito = '9'
043300010531     C                   endif
043400990914     C*
043500990914     C                   if        %open(tivin00r)
043600990914     C                   close     tivin00r
043700990914     C                   endif
043800020916     C                   if        %open(fivadwwr)
043900020916     C                   close     fivadwwr
044000990914     C                   endif
044100010531     C*
044200010531     C                   if        vlrpoi <> 999
044300010531     C                   eval      invfil = vlrpoi
044400010531     C                   endif
044500990914     C*
044600990921     C                   if        §ctdok > 0
044700010531     C                             and invfil > *zeros
044800990921     C                   exsr      invio
044900990921     C                   endif
045000010612     C*
045100010612     C                   if        flgGiro = '1'
045200010612     C                   exsr      endela
045300010612     C                   endif
045400990921     C*
045500990914     C                   ENDSR
045600000613     C***
045700990921
045800010531
045900010531
046000010531
046100010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
046200010531     C     repfil        BEGSR
046300010531     C*
046400010531     C                   if        invfil = *zeros and
046500010531     C                             depfil > *zeros and
046600010613     C                             (vinflg = *blanks or
046700010613     C                              vinflg = *zeros)
046800010531     C
046900010531     C                   eval      invfil = depfil
047000010531     C                   endif
047100010531     C*
047200010531     C                   if        depfil <> invfil and
047300010531     C                             invfil > *zeros
047400010531     C                   eval      flgMulti = '1'
047500010531     C                   if        vinflg = *blanks
047600010531     C                   add       1             cntNonEl
047700010531     C                   endif
047800010531     C                   endif
047900010531     C*
048000010531     C                   if        vinflg = '2'
048100010531     C                   eval      flgStato = '2'
048200010531     C                   endif
048300010531     C*
048400010531     C                   ENDSR
048500010531     C***
048600010531
048700010531
048800010531
048900010531
049000010531
049100990921      /TITLE Invio dei dati al punto operativo.
049200000613     C     invio         BEGSR
049300990921     C*
049400990921     C                   reset                   dscmz
049500010531     C                   move      invfil        cmzdst
049600990921     C                   eval      cmzfld = vlrfou
049700990921     C                   eval      cmzmbd = vlrhdl
049800990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
049900000710     C                   if        prmfir = *blanks
050000020916     C                   eval      cmzfla = 'FIVAD00F'
050100020916     C                   eval      cmzmba = 'FIVAD00F'
050200000710     C                   else
050300000710     C                   eval      cmzfla = prmfir
050400000710     C                   eval      cmzmba = prmfir
050500000710     C                   endif
050600990921     C                   eval      cmznrr = *zeros
050700990921     C                   move      §ctdok        cmznrr
050800021003     C                   eval      cmzlba = vlrfl1
050900990921     C                   call(e)   'TIS711C'
051000990921     C                   parm                    dscmz
051100990921     C                   parm      *blanks       esito
051200990921     C                   if        %error
051300990921     C                             or cmzerr = '1'
051400990921     C                             or esito  = '1'
051500000710     C                   eval      wrkesito = '3'
051600990921     C                   endif
051700990921     C*
051800000613     C                   ENDSR
051900000613     C***
052000990914
052100010531
052200010531
052300010531
052400010531      /TITLE Invio dei dati al punto operativo.
052500010531     C     opeini        BEGSR
052600010531     C*
052700010531     C* Inizializzo flag e contatori operativi
052800010531     C                   movel     '0'           flgGiro           1
052900010531     C                   movel     '0'           flgMulti          1
053000010531     C                   movel     '1'           flgStato          1
053100010615     C                   movel     '0'           flgOk             1
053200010531     C                   z-add     *zeros        cntNonEl         10 0
053300010531     C                   z-add     *zeros        depfil            3 0
053400010531     C                   z-add     *zeros        invfil            3 0
053500030407     C*
053600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
053700030407     C                   if        %subst(vlrppt:1:1) = 'S'
053800030407     C                   seton                                        50
053900030407     C                   else
054000030407     C                   setoff                                       50
054100030407     C                   endif
054200010531     C*
054300010531     C                   ENDSR
054400010531     C***
054500010531
054600051212
054700051212
054800051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
054900051212     C     exeBon        BEGSR
055000051212     C*
055100051212     C* Imposto i parametri x il *pgm d "bonifica"
055200051212     C                   EVAL      ElencoChar = *blanks
055300051212     C                   EVAL      TipoElenco = *blanks
055400051212     C                   EVAL      CharSost   = *blanks
055500051212     C                   EVAL      UpperCase  = '1'
055600051212     C                   EVAL      ChkNull    = '1'
055700051212     C                   EVAL      CharNull   = *blanks
055800051212     C*
055900051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
056000051212     C                   EVAL      TxtInOut   = VADCDP
056100051212     C                   EVAL      EsitoBon   = *blanks
056200051212     C*
056300051212     C                   CALL      'XCHKCHAR'
056400051212     C                   PARM                    TxtInOut
056500051212     C                   PARM                    ElencoChar
056600051212     C                   PARM                    TipoElenco
056700051212     C                   PARM                    CharSost
056800051212     C                   PARM                    UpperCase
056900051212     C                   PARM                    ChkNull
057000051212     C                   PARM                    CharNull
057100051212     C                   PARM                    EsitoBon
057200051212     C*
057300051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
057400051212     C                   IF        EsitoBon = '1'
057500051212     C                   EVAL      VADCDP = TxtInOut
057600051212     C                   ENDIF
057700051212     C*
057800051212     C                   ENDSR
057900051212
058000010531
058100010531
058200010531
058300000613     C     *inzsr        BEGSR
058400990914     C*
058500990914     C     *entry        plist
058600990921     C                   parm                    tivlrds
058700990921     C                   parm      wrkesito      esito
058800000724     C                   parm                    prmlit
058900000710     C                   parm                    prmfir
059000010606     C*
059100010606     C* CALCOLA LA DATA CORRENTE
059200010606     C                   time                    wn14             14 0
059300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059400010606     C                   z-add     wn8           g08dat
059500010606     C                   z-add     *zeros        g08inv
059600010606     C                   movel     '0'           g08err
059700010606     C                   call      'XSRDA8'
059800010606     C                   parm                    wlbda8
059900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060000000613     C*
060100000613     C                   ENDSR
060200000613     C***
060300990914
060400000613     Otis7t2p   E            riepilogo         2
060500990915     O                                              'Upload via Internet'
060600990915     O                                           +1 'Traduzione TIVIN00R -'
060700020916     O                                           +1 'FIVADWWR'
060800990915     O                                           +1 'Dettaglio colli'
060900990915     O          E            riepilogo   2
061000990915     O                       wrkdata
061100990915     O                       wrkora              +1
061200990915     O                       procname            +1
061300990915     O          E            riepilogo   2
061400990915     O                                              'Cliente..................:'
061500990915     O                       VADCCM        z     +1
061600990915     O          E            riepilogo   2
061700990921     O                                              'Riferimento strategi.....:'
061800990921     O                       vlrhdl              +1
061900990915     O          E            riepilogo   2
062000990915     O                                              'Giusti...................:'
062100990915     O                       §CTDOK        2   +  1
062200990915     O          E            riepilogo   2
062300010601     O                                              'Sbagliati ...............:'
062400990915     O                       §CTDNO        2   +  1
062500000613
062600000616     Otis7t2ps  E            testdett          2
062700000613     O                                              'Upload via Internet'
062800000613     O                                           +1 'Traduzione TIVIN00R -'
062900020916     O                                           +1 'FIVADWWR'
063000000613     O                                           +1 'Dettaglio colli'
063100000616     O          E            testdett    3
063200000613     O                                           +2 'N° rec'
063300000613     O                                           +3 'Anteprima contenuto'
063400000616     O          E            rigadett    2
063500000613     O                       rrnum               +2
063600000621     O                       recko               +3
063700000616     O          E            findett     2
063800000613     O                       wrkdata
063900000613     O                       wrkora              +1
064000000613     O                       procname            +1
064100000616     O          E            findett     2
064200000613     O                                              'Cliente..................:'
064300000613     O                       VADCCM        z     +1
064400000616     O          E            findett     2
064500000613     O                                              'Riferimento strategi.....:'
064600000613     O                       vlrhdl              +1
064700000616     O          E            findett     2
064800000613     O                                              'Giusti...................:'
064900000613     O                       §CTDOK        2   +  1
065000000616     O          E            findett     2
065100010601     O                                              'Sbagliati ...............:'
065200000613     O                       §CTDNO        2   +  1
065300000613** CMD - COMANDI CL
065400011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T2P)  FORMTYPE(RICCLI) OUTQ(
065500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T2PS) FORMTYPE(RICCLI) OUTQ(
065600000616DLTOVR FILE(TIS7T2P TIS7T2PS) LVL(*)
065700000613
065800000613
