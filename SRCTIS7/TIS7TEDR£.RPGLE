000100030616      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200990914     H dftactgrp(*yes)
000300021014
000400000724     Fazorg01l  if   e           k disk
000500990914     Ftivin00r  uF   E             DISK    usropn
000600030616     FEDIVADWR  O    E             DISK
000700030710     Ftis7tedp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030710     Ftis7tedps O    f  198        PRINTER usropn
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
005300030616     D wNomeFile       s             30    INZ(*blanks)
005400030710     D wCMR            s             35    INZ(*blanks)
005500030710     D wPosDaA         s              2    INZ(*blanks)
005600030710     D wPosDa          s              2  0 INZ(*zeros)
005700030710     D wLungA          s              2    INZ(*blanks)
005800030710     D wLung           s              2  0 INZ(*zeros)
005900010606     D*------------------
006000010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100010606     D*------------------
006200010606     D WLBDA8          DS                  INZ
006300010606     D  G08DAT                 1      8  0
006400010606     D  G08INV                 9     16  0
006500010606     D  G08ERR                17     17
006600010606     D  G08TGI                18     22  0
006700010606     D*
006800010606
006900051212     D TxtInOut        S           2048
007000051212     D ElencoChar      S            256
007100051212     D TipoElenco      S              1
007200051212     D CharSost        S              1
007300051212     D UpperCase       S              1
007400051212     D ChkNull         S              1
007500051212     D CharNull        S              1
007600051212     D EsitoBon        S              1
007700010606
007800010606
007900990914
008000990915     C                   time                    wrkdata
008100990915     C                   time                    wrkora
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400990921     C                   reset                   wrkesito
008500010531     C*
008600010531     C                   exsr      opeini
008700010531     C                   exsr      rwvad
008800010531     C*
008900010531     C                   seton                                        lr
009000010531
009100010531
009200010531
009300010531
009400010531     C*--------------------------------------------------------
009500010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009600010531     C*--------------------------------------------------------
009700010531     C     PREELA        BEGSR
009800000724     C*
009900000724     C* SE OCCORRE SPEDIRE IN FILIALE
010000010531     C                   if        invfil <> *zeros and
010100010531     C                             flgGiro = '0'
010200010531     C*
010300010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010400010531     C                   eval      flgGiro = '1'
010500000724     C*
010600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010800030623     C     VADFGS        chain     azorg01l
010900000724     C                   if        %found
011000000724     C                   movel(p)  CMD(1)        depcmd
011100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011200000724     C*
011300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011400011123     C                   Z-ADD     150           LENGH            15 5
011500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000724     C                   PARM                    depcmd
011700000724     C                   PARM                    LENGH
011800000724     C*
011900000724     C                   endif
012000000616     C*
012100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012200000616     C                   movel(p)  CMD(2)        depcmd
012300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012400000616     C*
012500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012600011123     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    depcmd
012900000616     C                   PARM                    LENGH
013000990914     C*
013100030710     C                   if        not %open(tis7tedps)
013200030710     C                   open      tis7tedps
013300030407     C   50              except    testdett
013400000613     C                   endif
013500010531     C*
013600010531     C                   endif
013700010531     C*
013800010531     C                   ENDSR
013900010531     C***
014000010531
014100010531
014200010531
014300010531
014400010531
014500010531
014600010531
014700010531     C*--------------------------------------------------------
014800010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014900010531     C*--------------------------------------------------------
015000010531     C     ENDELA        BEGSR
015100010531     C*
015200010531     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015300000613     C*
015400030710     C                   if        %open(tis7tedps)
015500030407     C   50              except    findett
015600030710     C                   close     tis7tedps
015700000613     C                   endif
015800000616     C*
015900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016000011123     C                   Z-ADD     150           LENGH            15 5
016100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016200000616     C                   PARM                    CMD(3)
016300000616     C                   PARM                    LENGH
016400000616     C*
016500010531     C                   ENDSR
016600010531     C***
016700010531
016800010531
016900000613
017000000613
017100000613
017200000613     C*--------------------------------------------------------
017300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017400000613     C*--------------------------------------------------------
017500000621     C     STPR          BEGSR
017600000613     C*
017700030710     C                   if        not %open(tis7tedp)
017800030710     C                   open      tis7tedp
017900990915     C                   endif
018000990915     C*
018100030407     C   50              except    riepilogo
018200990915     C*
018300030710     C                   if        %open(tis7tedp)
018400030710     C                   close     tis7tedp
018500990914     C                   endif
018600000613     C*
018700000613     C                   ENDSR
018800000613     C***
018900990914
019000010531
019100010531
019200010531
019300010531
019400990914     C*--------------------------------------------------------
019500030616     C* RWVAD  LEGGE TIVIN00R E SCRIVE EDIVAD0F              *
019600990914     C*--------------------------------------------------------
019700990914     C     RWVAD         BEGSR
019800030710     C*
019900030710     C                   clear                   §CTDOK
020000030710     C                   clear                   §CTDNO
020100990914     C*
020200030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
020300030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
020400030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
020500030710     C                   if        *in60 = *on
020600990914     C                   if        not %open(tivin00r)
020700990914     C                   open      tivin00r
020800990914     C                   endif
020900030616     C                   if        not %open(edivadwr)
021000030616     C                   open      edivadwr
021100990914     C                   endif
021200990914     C*
021300990914     C                   DO        *HIVAL
021400010618     C*
021500990914     C                   READ      tivin00r                               70
021600010618     C*
021700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021800010618     C                   if        vinflg = '1'
021900010618     C                   eval      flgOk = '1'
022000010618     C                   endif
022100010618     C*
022200000905     C                   if        vindta > *blanks
022300000613     C                   add       1             rrnum
022400990914     C*
022500010601     C                   if        *in70 = *off and
022600990914     C                             (vinflg = *blanks
022700010711     C                              or vinflg = '0'
022800010711     C                              or vinflg = '2')
022900990914     C*
023000030616     C                   clear                   EDIVAD00
023100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023200010711     C                   if        vinflg = *blanks or vinflg = '0'
023300010711     C                   clear                   vinmsg
023400010711     C                   endif
023500010711     C*
023600990914     C                   eval      fnvadaar = vindta
023700991022     C*
023800991022     C                   testb     x'3F'         vhdatb                   02
023900991022     C                   if        not *in02
024000991022     C*
024100990914     C                   SETOFF                                       303132
024200990914     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
024300010531     C*
024400010531     C                   TESTN                   VHDLNP               30
024500010531     C  N30              TESTN                   VHDLNP                 30
024600010531     C  N30              TESTN                   VHDLNP                   30
024700010531     C                   MOVE      VHDLNP        W001A             1
024800010531     C     *IN30         IFEQ      *OFF
024900010531     C     W001A         ORLT      '0'
025000010531     C                   SETON                                        31
025100010531     C                   eval      vinmsg = %trimr(vinmsg)
025200010531     C                             + ' ' + 'VHDLNP'
025300010531     C                   ELSE
025400010531     C                   MOVE      VHDLNP        VADLNP
025500010531     C     VADLNP        IFLE      *ZEROS
025600010531     C                   SETON                                        31
025700010531     C                   eval      vinmsg = %trimr(vinmsg)
025800010531     C                             + ' ' + 'VHDLNP'
025900010531     C                   ENDIF
026000010531     C                   ENDIF
026100010531     C*
026200010531     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026300010531     C                   eval      depfil = VADLNP
026400010531     C                   exsr      repfil
026500010601     C                   if        depfil = invfil
026600021014     C                   if        vlrpoi = 999
026700021010     C                   MOVE(p)   invfil        VADFGS
026800021014     C                   else
026900021014     C                   MOVE(p)   vlrpoi        VADFGS
027000021014     C                   endif
027100010531     C*
027200010531     C                   exsr      PREELA
027300010531     C*
027400990914     C                   TESTN                   VHDCCM               30
027500990914     C  N30              TESTN                   VHDCCM                 30
027600990914     C  N30              TESTN                   VHDCCM                   30
027700990914     C                   MOVE      VHDCCM        W001A             1
027800990914     C     *IN30         IFEQ      *OFF
027900990914     C     W001A         ORLT      '0'
028000990914     C                   SETON                                        31
028100010604     C                   eval      vinmsg = %trimr(vinmsg)
028200010604     C                             + ' ' + 'VHDCCM'
028300990914     C                   ELSE
028400990914     C                   MOVE      VHDCCM        VADCCM
028500990914     C     VADCCM        IFLE      *ZEROS
028600990914     C                   SETON                                        31
028700010604     C                   eval      vinmsg = %trimr(vinmsg)
028800010604     C                             + ' ' + 'VHDCCM'
028900990914     C                   ENDIF
029000990914     C                   ENDIF
029100010601     C*
029200010601     C                   TESTN                   VHDAAS               30
029300010601     C  N30              TESTN                   VHDAAS                 30
029400010601     C  N30              TESTN                   VHDAAS                   30
029500010601     C                   MOVE      VHDAAS        W001A             1
029600010601     C     *IN30         IFEQ      *OFF
029700010601     C     W001A         ORLT      '0'
029800010601     C                   SETON                                        32
029900010606     C                   MOVEL     datcor        VADAAS
030000010601     C                   eval      vinmsg = %trimr(vinmsg)
030100010601     C                             + ' ' + 'VHDAAS'
030200010601     C                   ELSE
030300010601     C                   MOVE      VHDAAS        VADAAS
030400010601     C                   ENDIF
030500010601     C*
030600010601     C                   TESTN                   VHDNRS               30
030700010601     C  N30              TESTN                   VHDNRS                 30
030800010601     C  N30              TESTN                   VHDNRS                   30
030900010601     C                   MOVE      VHDNRS        W001A             1
031000010601     C     *IN30         IFEQ      *OFF
031100010601     C     W001A         ORLT      '0'
031200010601     C     W001A         ANDNE     *BLANK
031300010601     C                   SETON                                        32
031400010601     C                   MOVE      *ZEROS        VADNRS
031500010601     C                   eval      vinmsg = %trimr(vinmsg)
031600010601     C                             + ' ' + 'VHDNRS'
031700010601     C                   ELSE
031800010601     C                   MOVE      VHDNRS        VADNRS
031900010601     C                   ENDIF
032000010601     C*
032100010601     C                   TESTN                   VHDNSP               30
032200010601     C  N30              TESTN                   VHDNSP                 30
032300010601     C  N30              TESTN                   VHDNSP                   30
032400010601     C                   MOVE      VHDNSP        W001A             1
032500010601     C     *IN30         IFEQ      *OFF
032600010601     C     W001A         ORLT      '0'
032700010601     C                   SETON                                        32
032800010601     C                   MOVE      *ZEROS        VADNSP
032900010601     C                   eval      vinmsg = %trimr(vinmsg)
033000010601     C                             + ' ' + 'VHDNSP'
033100010601     C                   ELSE
033200010601     C                   MOVE      VHDNSP        VADNSP
033300010601     C                   ENDIF
033400010601 FG  C*
033500010601 xxx C                   IF        VHDNCL = *blanks and
033600010601 "   C                             VADNRS > *zeros
033700010601 "   C                   MOVE      '00001'       VHDNCL
033800010601 FG  C                   ENDIF
033900010601     C*
034000010601     C                   TESTN                   VHDNCL               30
034100010601     C  N30              TESTN                   VHDNCL                 30
034200010601     C  N30              TESTN                   VHDNCL                   30
034300010601     C                   MOVE      VHDNCL        W001A             1
034400010601     C     *IN30         IFEQ      *OFF
034500010601     C     W001A         ORLT      '0'
034600010601     C                   SETON                                        32
034700010601     C                   MOVE      *ZEROS        VADNCL
034800010601     C                   eval      vinmsg = %trimr(vinmsg)
034900010601     C                             + ' ' + 'VHDNCL'
035000010601     C                   ELSE
035100010601     C                   MOVE      VHDNCL        VADNCL
035200010601     C                   ENDIF
035300010601     C*
035400010601     C                   TESTN                   VHDNCD               30
035500010601     C  N30              TESTN                   VHDNCD                 30
035600010601     C  N30              TESTN                   VHDNCD                   30
035700010601     C                   MOVE      VHDNCD        W001A             1
035800010601     C     *IN30         IFEQ      *OFF
035900010601     C     W001A         ORLT      '0'
036000010601     C     W001A         ANDNE     *BLANK
036100010601     C                   SETON                                        32
036200010601     C                   MOVE      *ZEROS        VADNCD
036300010601     C                   eval      vinmsg = %trimr(vinmsg)
036400010601     C                             + ' ' + 'VHDNCD'
036500010601     C                   ELSE
036600010601     C                   MOVE      VHDNCD        VADNCD
036700010601     C                   ENDIF
036800010601     C*
036900010601     C                   TESTN                   VHDNCA               30
037000010601     C  N30              TESTN                   VHDNCA                 30
037100010601     C  N30              TESTN                   VHDNCA                   30
037200010601     C                   MOVE      VHDNCA        W001A             1
037300010601     C     *IN30         IFEQ      *OFF
037400010601     C     W001A         ORLT      '0'
037500010601     C     W001A         ANDNE     *BLANK
037600010601     C                   SETON                                        32
037700010601     C                   MOVE      *ZEROS        VADNCA
037800010601     C                   eval      vinmsg = %trimr(vinmsg)
037900010601     C                             + ' ' + 'VHDNCA'
038000010601     C                   ELSE
038100010601     C                   MOVE      VHDNCA        VADNCA
038200010601     C                   ENDIF
038300010601     C*
038400010601     C* ALFABETICI
038500010601     C                   MOVE      VHDATB        VADATB
038600010601     C                   MOVE      VHDCDP        VADCDP
038700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
038800030710     C                   EVAL      VADCMR = wCMR
038900030616     C                   EVAL      VADCNT = 1
039000051212
039100051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
039200051212     c                   exsr      exeBon
039300051212
039400010601     C*
039500010604     C  N31              ADD       1             §CTDOK            7 0
039600010601     C   32
039700010604     COR 31              ADD       1             §CTDNO            7 0
039800030616     C  N31              WRITE     EDIVAD00
039900010601     C*
040000010601     C                   if        *in31 = *off and
040100010601     C                             *in32 = *off
040200990914     C                   eval      vinflg = '1'
040300990914     C                   else
040400000621     C                   eval      recko = vindta
040500030710     C                   if        %open(tis7tedps)
040600030407     C   50              except    rigadett
040700010612     C                   endif
040800990914     C                   eval      vinflg = '2'
040900990914     C                   endif
041000000905     C                   endif
041100000905     C                   endif
041200010601     C*
041300010601     C                   endif
041400000905     C*
041500000905     C                   else
041600000905     C                   eval      vinflg = '1'
041700000905     C                   endif
041800000905     C*
041900000905     C  N70              update    tivin000
042000991022     C*
042100991022     C  N70              ENDdo
042200010531     C
042300010531     C*
042400010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
042500010531     C                   if        cntNonEl = *zeros or
042600010531     C                             flgMulti = '0'
042700990914     C* Se non ci sono record con errori ...
042800010531     C                   if        §ctdno = 0 and
042900010531     C                             flgStato <> '2'
043000990914     C* ... restituisco esito OK.
043100990921     C                   eval      wrkesito = '0'
043200990914     C                   else
043300000710     C                   if        §ctdok > 0
043400990921     C                   eval      wrkesito = '1'
043500000710     C                   else
043600010615     C                   if        flgOk = '0'
043700010615     C                   eval      wrkesito = '2'
043800010615     C                   else
043900010615     C                   eval      wrkesito = '6'
044000010615     C                   endif
044100990914     C                   endif
044200000710     C                   endif
044300010531     C                   else
044400010531     C                   eval      wrkesito = '9'
044500010531     C                   endif
044600030710     C*
044700030710 X  1C                   else
044800030710     C                   eval      wrkesito = '8'
044900030710     C                   eval      flgGiro = '1'
045000030710 E  1C                   endif
045100990914     C*
045200990914     C                   if        %open(tivin00r)
045300990914     C                   close     tivin00r
045400990914     C                   endif
045500030616     C                   if        %open(edivadwr)
045600030616     C                   close     edivadwr
045700990914     C                   endif
045800010531     C*
045900010531     C                   if        vlrpoi <> 999
046000010531     C                   eval      invfil = vlrpoi
046100010531     C                   endif
046200990914     C*
046300990921     C                   if        §ctdok > 0
046400010531     C                             and invfil > *zeros
046500990921     C                   exsr      invio
046600990921     C                   endif
046700010612     C*
046800010612     C                   if        flgGiro = '1'
046900010612     C                   exsr      endela
047000010612     C                   endif
047100990921     C*
047200990914     C                   ENDSR
047300000613     C***
047400990921
047500010531
047600010531
047700010531
047800010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047900010531     C     repfil        BEGSR
048000010531     C*
048100010531     C                   if        invfil = *zeros and
048200010531     C                             depfil > *zeros and
048300010613     C                             (vinflg = *blanks or
048400010613     C                              vinflg = *zeros)
048500010531     C
048600010531     C                   eval      invfil = depfil
048700010531     C                   endif
048800010531     C*
048900010531     C                   if        depfil <> invfil and
049000010531     C                             invfil > *zeros
049100010531     C                   eval      flgMulti = '1'
049200010531     C                   if        vinflg = *blanks
049300010531     C                   add       1             cntNonEl
049400010531     C                   endif
049500010531     C                   endif
049600010531     C*
049700010531     C                   if        vinflg = '2'
049800010531     C                   eval      flgStato = '2'
049900010531     C                   endif
050000010531     C*
050100010531     C                   ENDSR
050200010531     C***
050300010531
050400010531
050500010531
050600010531
050700010531
050800990921      /TITLE Invio dei dati al punto operativo.
050900000613     C     invio         BEGSR
051000990921     C*
051100990921     C                   reset                   dscmz
051200030618     C                   move      invfil        cmzdst
051300030618     C***                move      '789'         cmzdst
051400990921     C                   eval      cmzfld = vlrfou
051500990921     C                   eval      cmzmbd = vlrhdl
051600990921     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700000710     C                   if        prmfir = *blanks
051800030616     C                   eval      cmzfla = 'EDIVAD0F'
051900030616     C                   eval      cmzmba = 'EDIVAD0F'
052000000710     C                   else
052100000710     C                   eval      cmzfla = prmfir
052200000710     C                   eval      cmzmba = prmfir
052300000710     C                   endif
052400990921     C                   eval      cmznrr = *zeros
052500990921     C                   move      §ctdok        cmznrr
052600021003     C                   eval      cmzlba = vlrfl1
052700990921     C                   call(e)   'TIS711C'
052800990921     C                   parm                    dscmz
052900990921     C                   parm      *blanks       esito
053000990921     C                   if        %error
053100990921     C                             or cmzerr = '1'
053200990921     C                             or esito  = '1'
053300000710     C                   eval      wrkesito = '3'
053400990921     C                   endif
053500990921     C*
053600000613     C                   ENDSR
053700000613     C***
053800990914
053900010531
054000010531
054100010531
054200010531      /TITLE Invio dei dati al punto operativo.
054300010531     C     opeini        BEGSR
054400010531     C*
054500010531     C* Inizializzo flag e contatori operativi
054600010531     C                   movel     '0'           flgGiro           1
054700010531     C                   movel     '0'           flgMulti          1
054800010531     C                   movel     '1'           flgStato          1
054900010615     C                   movel     '0'           flgOk             1
055000010531     C                   z-add     *zeros        cntNonEl         10 0
055100010531     C                   z-add     *zeros        depfil            3 0
055200010531     C                   z-add     *zeros        invfil            3 0
055300030407     C*
055400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
055500030407     C                   if        %subst(vlrppt:1:1) = 'S'
055600030407     C                   seton                                        50
055700030407     C                   else
055800030407     C                   setoff                                       50
055900030407     C                   endif
056000010531     C*
056100010531     C                   ENDSR
056200010531     C***
056300010531
056400051212
056500051212
056600051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
056700051212     C     exeBon        BEGSR
056800051212     C*
056900051212     C* Imposto i parametri x il *pgm d "bonifica"
057000051212     C                   EVAL      ElencoChar = *blanks
057100051212     C                   EVAL      TipoElenco = *blanks
057200051212     C                   EVAL      CharSost   = *blanks
057300051212     C                   EVAL      UpperCase  = '1'
057400051212     C                   EVAL      ChkNull    = '1'
057500051212     C                   EVAL      CharNull   = *blanks
057600051212     C*
057700051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
057800051212     C                   EVAL      TxtInOut   = VADCDP
057900051212     C                   EVAL      EsitoBon   = *blanks
058000051212     C*
058100051212     C                   CALL      'XCHKCHAR'
058200051212     C                   PARM                    TxtInOut
058300051212     C                   PARM                    ElencoChar
058400051212     C                   PARM                    TipoElenco
058500051212     C                   PARM                    CharSost
058600051212     C                   PARM                    UpperCase
058700051212     C                   PARM                    ChkNull
058800051212     C                   PARM                    CharNull
058900051212     C                   PARM                    EsitoBon
059000051212     C*
059100051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
059200051212     C                   IF        EsitoBon = '1'
059300051212     C                   EVAL      VADCDP = TxtInOut
059400051212     C                   ENDIF
059500051212     C*
059600051212     C                   ENDSR
059700051212
059800010531
059900010531
060000000613     C     *inzsr        BEGSR
060100990914     C*
060200990914     C     *entry        plist
060300990921     C                   parm                    tivlrds
060400990921     C                   parm      wrkesito      esito
060500000724     C                   parm                    prmlit
060600000710     C                   parm                    prmfir
060700030710     C*
060800030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
060900030710     C                   move(p)   vlrMSG        wNomeFile
061000030710     C*
061100030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
061200030710     C* del codice CMR dal nome del file importato.
061300030710     C                   if        wNomeFile <> *blanks AND
061400030710     C                             vlrppt<>*blanks
061500030710     C                   if        %subst(vlrppt:1:1) = 'S'
061600030710     C                   seton                                        50
061700030710     C                   else
061800030710     C                   setoff                                       50
061900030710     C                   endif
062000030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
062100030710     C                   move(p)   wPosDaA       wPosDa
062200030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
062300030710     C                   move(p)   wLungA        wLung
062400030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
062500030710     C                   seton                                        60
062600030710     C                   else
062700030710     C                   setoff                                       60
062800030710     C                   endif
062900010606     C*
063000010606     C* CALCOLA LA DATA CORRENTE
063100010606     C                   time                    wn14             14 0
063200010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063300010606     C                   z-add     wn8           g08dat
063400010606     C                   z-add     *zeros        g08inv
063500010606     C                   movel     '0'           g08err
063600010606     C                   call      'XSRDA8'
063700010606     C                   parm                    wlbda8
063800010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063900000613     C*
064000000613     C                   ENDSR
064100000613     C***
064200990914
064300030710     Otis7tedp  E            riepilogo         2
064400990915     O                                              'Upload via Internet'
064500990915     O                                           +1 'Traduzione TIVIN00R -'
064600030616     O                                           +1 'EDIVADWR'
064700990915     O                                           +1 'Dettaglio colli'
064800990915     O          E            riepilogo   2
064900990915     O                       wrkdata
065000990915     O                       wrkora              +1
065100990915     O                       procname            +1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Cliente..................:'
065400990915     O                       VADCCM        z     +1
065500030618     O          E            riepilogo   2
065600030618     O                                              'Numero CMR...............:'
065700030618     O                       VADCMR              +1
065800990915     O          E            riepilogo   2
065900990921     O                                              'Riferimento strategi.....:'
066000990921     O                       vlrhdl              +1
066100990915     O          E            riepilogo   2
066200990915     O                                              'Giusti...................:'
066300990915     O                       §CTDOK        2   +  1
066400990915     O          E            riepilogo   2
066500010601     O                                              'Sbagliati ...............:'
066600990915     O                       §CTDNO        2   +  1
066700000613
066800030710     Otis7tedps E            testdett          2
066900000613     O                                              'Upload via Internet'
067000000613     O                                           +1 'Traduzione TIVIN00R -'
067100030616     O                                           +1 'EDIVADWR'
067200000613     O                                           +1 'Dettaglio colli'
067300000616     O          E            testdett    3
067400000613     O                                           +2 'N° rec'
067500000613     O                                           +3 'Anteprima contenuto'
067600000616     O          E            rigadett    2
067700000613     O                       rrnum               +2
067800000621     O                       recko               +3
067900000616     O          E            findett     2
068000000613     O                       wrkdata
068100000613     O                       wrkora              +1
068200000613     O                       procname            +1
068300000616     O          E            findett     2
068400000613     O                                              'Cliente..................:'
068500000613     O                       VADCCM        z     +1
068600030618     O          E            findett     2
068700030618     O                                              'Numero CMR...............:'
068800030618     O                       VADCMR              +1
068900000616     O          E            findett     2
069000000613     O                                              'Riferimento strategi.....:'
069100000613     O                       vlrhdl              +1
069200000616     O          E            findett     2
069300000613     O                                              'Giusti...................:'
069400000613     O                       §CTDOK        2   +  1
069500000616     O          E            findett     2
069600010601     O                                              'Sbagliati ...............:'
069700000613     O                       §CTDNO        2   +  1
069800000613** CMD - COMANDI CL
069900030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEDP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070000030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEDPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070100030710DLTOVR FILE(TIS7TEDP TIS7TEDPS) LVL(*)
070200000613
070300000613
