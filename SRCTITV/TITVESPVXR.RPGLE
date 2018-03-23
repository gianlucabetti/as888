000100040524      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040524     FFIVAXwwr  O    E             DISK    usropn
000700040524     Ftis7t13p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040524     Ftis7t13ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011123     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010612     D*
004100040524     D fnvaxaar        ds                  inz
004200040524     D  a_vaxatb               1      1
004300040524     D  a_vaxccm               3      9
004400040524     D  a_vaxlnp              11     13
004500040524     D  a_vaxaas              15     18
004600040524     D  a_vaxnrs              20     21
004700040524     D  a_vaxnsp              23     29
004800040524     D  a_vaxtmd              30     33
004900040524     D  a_vaxprr              35     38
005000040524     D  a_vaxtrk              39     40
005100040524     D  a_vaxdtt              41    540
005200010330
005300010330
005400010330
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000010601     C*
006100010601     C                   exsr      opeini
006200040524     C                   exsr      rwvax
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300010601     C     invfil        chain     azorg01l
008400000724     C                   if        %found
008500000724     C                   movel(p)  CMD(1)        depcmd
008600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000724     C*
008800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011123     C                   Z-ADD     150           LENGH            15 5
009000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000724     C                   PARM                    depcmd
009200000724     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
009500000616     C*
009600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700000616     C                   movel(p)  CMD(2)        depcmd
009800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010100011123     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000613     C*
010600040524     C                   if        not %open(tis7t13ps)
010700040524     C                   open      tis7t13ps
010800030407     C   50              except    testdett
010900000613     C                   endif
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012400010601     C*--------------------------------------------------------
012500010601     C     ENDELA        BEGSR
012600990915     C*
012700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012800000613     C*
012900040524     C                   if        %open(tis7t13ps)
013000030407     C   50              except    findett
013100040524     C                   close     tis7t13ps
013200000613     C                   endif
013300000616     C*
013400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013500011123     C                   Z-ADD     150           LENGH            15 5
013600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013700000616     C                   PARM                    CMD(3)
013800000616     C                   PARM                    LENGH
013900000616     C*
014000010601     C                   ENDSR
014100010601     C***
014200010601
014300010601
014400010601
014500010601
014600000613
014700000613     C*--------------------------------------------------------
014800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014900000613     C*--------------------------------------------------------
015000000621     C     STPR          BEGSR
015100000613     C*
015200040524     C                   if        not %open(tis7t13p)
015300040524     C                   open      tis7t13p
015400990915     C                   endif
015500990915     C*
015600030407     C   50              except    riepilogo
015700990915     C*
015800040524     C                   if        %open(tis7t13p)
015900040524     C                   close     tis7t13p
016000990914     C                   endif
016100000613     C*
016200000613     C                   ENDSR
016300000613     C***
016400000613
016500010330
016600010601
016700010601
016800010601
016900010330     C*--------------------------------------------------------
017000040524     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
017100010330     C*--------------------------------------------------------
017200040524     C     RWVAX         BEGSR
017300010330     C*
017400010330     C                   if        not %open(tivin00r)
017500010330     C                   open      tivin00r
017600010330     C                   endif
017700040524     C                   if        not %open(fivaxwwr)
017800040524     C                   open      fivaxwwr
017900010330     C                   endif
018000010330     C*
018100010604     C                   clear                   §CTROK
018200010604     C                   clear                   §CTRMO
018300010604     C                   clear                   §CTRNO
018400010330     C*
018500010330     C                   DO        *HIVAL
018600010330     C*
018700010330     C                   READ      tivin00r                               70
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300010618     C*
019400010330     C                   if        vindta > *blanks
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010711     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010711     C*
020700040524     C                   eval      fnvaxaar = vindta
020800010601     C*
020900010330     C                   exsr      inzvar
021000010330     C                   exsr      defcam
021100040524     C                   exsr      impvax
021200070115     C*
021300070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400070115     C                   eval      depfil = VAXLNP
021500070115     C*
021600070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
021700070115     C* cliente
021800070507     C                   if        VAXCCM = 0893943 OR
021801070507     C                             VAXCCM = 0894169
021900070115     C                   eval      depfil = 050
022000070115     C                   eval      invfil = 050
022100070115     C                   eval      vaxfgs = 050
022200070115     C                   endif
022300070507     C                   if        VAXCCM = 0893942 OR
022301070507     C                             VAXCCM = 0894168
022400070115     C                   eval      depfil = 005
022500070115     C                   eval      invfil = 005
022600070115     C                   eval      vaxfgs = 005
022700070115     C                   endif
022800010601     C*
022900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023000010601     C                   exsr      repfil
023100021010     C                   if        depfil = invfil
023200021014     C                   if        vlrpoi = 999
023300040524     C                   MOVE(P)   invfil        VAXFGS
023400021014     C                   else
023500040524     C                   MOVE(P)   vlrpoi        VAXFGS
023600021014     C                   endif
023700010601     C*
023800010601     C                   exsr      PREELA
023900010601     C*
024000010604     C* Ebbene...
024100010604     C*
024200010604     C  N31              ADD       1             §CTROK            7 0
024300010604     C   32              ADD       1             §CTRMO            7 0
024400010604     C   31              ADD       1             §CTRNO            7 0
024500040524     C  N31              WRITE     FIVAX000
024600010604     C*
024700010604     C                   if        *in31 = *off and
024800010604     C                             *in32 = *off
024900010604     C                   eval      vinflg = '1'
025000010604     C                   else
025100010604     C                   eval      recko = vindta
025200040524     C                   if        %open(tis7t13ps)
025300030407     C   50              except    rigadett
025400010612     C                   endif
025500010604     C                   eval      vinflg = '2'
025600010604     C                   endif
025700010604     C                   endif
025800010601     C*
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700010330     C  N70              ENDdo
026800010601     C
026900010601     C*
027000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027100010601     C                   if        cntNonEl = *zeros or
027200010601     C                             flgMulti = '0'
027300010330     C* Se non ci sono record con errori ...
027400010601     C                   if        §ctrno = 0 and
027500010604     C                             §ctrmo = 0 and
027600010601     C                             flgStato <> '2'
027700010330     C* ... restituisco esito OK.
027800010330     C                   eval      wrkesito = '0'
027900010330     C                   else
028000010330     C                   if        §ctrok > 0
028100010330     C                   eval      wrkesito = '1'
028200010330     C                   else
028300010615     C                   if        flgOk = '0'
028400010615     C                   eval      wrkesito = '2'
028500010615     C                   else
028600010615     C                   eval      wrkesito = '6'
028700010615     C                   endif
028800010330     C                   endif
028900010330     C                   endif
029000010601     C                   else
029100010601     C                   eval      wrkesito = '9'
029200010601     C                   endif
029300010330     C*
029400010330     C                   if        %open(tivin00r)
029500010330     C                   close     tivin00r
029600010330     C                   endif
029700040524     C                   if        %open(fivaxwwr)
029800040524     C                   close     fivaxwwr
029900010330     C                   endif
030000010601     C*
030100010601     C                   if        vlrpoi <> 999
030200010601     C                   eval      invfil = vlrpoi
030300010601     C                   endif
030400010330     C*
030500010330     C                   if        §ctrok > 0
030600010601     C                             and invfil > *zeros
030700010330     C                   exsr      invio
030800010330     C                   endif
030900010601     C*
031000010612     C                   if        flgGiro = '1'
031100010601     C                   exsr      endela
031200010612     C                   endif
031300010330     C*
031400010330     C                   ENDSR
031500010330     C***
031600010330
031700010601
031800010601
031900010601
032000010601
032100010330     C*----------------------------------------------------*
032200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032300010330     C*----------------------------------------------------*
032400010330     C     INZVAR        BEGSR
032500010330     C*
032600010330     C*
032700010330     C                   ENDSR
032800010330     C*----------------------------------------------------*
032900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
033000010330     C*----------------------------------------------------*
033100010330     C     DEFCAM        BEGSR
033200010330     C*
033300040524     C                   CLEAR                   FIVAX000
033400010330     C*
033500010330     C                   ENDSR
033600010330     C*----------------------------------------------------*
033700040524     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
033800010330     C*----------------------------------------------------*
033900040524     C     IMPVAX        BEGSR
034000010330     C*
034100010604     C                   SETOFF                                       3132
034200010612     C*
034300010612     C                   SETOFF                                       303132
034400010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
034500040524     C                   TESTN                   a_vaxccm             30
034600040524     C  N30              TESTN                   a_vaxccm               30
034700040524     C  N30              TESTN                   a_vaxccm                 30
034800040524     C                   MOVE      a_vaxccm      W001A             1
034900010612     C     *IN30         IFEQ      *OFF
035000010612     C     W001A         ORLT      '0'
035100010612     C                   SETON                                        31
035200040524     C                   Z-ADD     *zeros        VAXCCM
035300040524     C                   EVAL      vinmsg = 'VAXCCM'
035400010612     C                   ELSE
035500040524     C                   MOVE      a_vaxccm      VAXCCM
035600040524     C     VAXCCM        IFLE      *ZEROS
035700010612     C                   SETON                                        31
035800040524     C                   Z-ADD     *zeros        VAXCCM
035900040524     C                   EVAL      vinmsg = 'VAXCCM'
036000010612     C                   ENDIF
036100010612     C                   ENDIF
036200010612     C*
036300040524     C                   TESTN                   a_vaxlnp             30
036400040524     C  N30              TESTN                   a_vaxlnp               30
036500040524     C  N30              TESTN                   a_vaxlnp                 30
036600040524     C                   MOVE      a_vaxlnp      W001A             1
036700010612     C     *IN30         IFEQ      *OFF
036800010612     C     W001A         ORLT      '0'
036900010612     C                   SETON                                        31
037000040524     C                   Z-ADD     *zeros        VAXLNP
037100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037200040524     C                             + ' ' + 'VAXLNP'
037300010612     C                   ELSE
037400040524     C                   MOVE      a_vaxlnp      VAXLNP
037500040524     C     VAXLNP        IFLE      *ZEROS
037600010612     C                   SETON                                        31
037700040524     C                   Z-ADD     *zeros        VAXLNP
037800010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037900040524     C                             + ' ' + 'VAXLNP'
038000010612     C                   ENDIF
038100010612     C                   ENDIF
038200010612     C*
038300040524     C                   TESTN                   a_vaxaas             30
038400040524     C  N30              TESTN                   a_vaxaas               30
038500040524     C  N30              TESTN                   a_vaxaas                 30
038600040524     C                   MOVE      a_vaxaas      W001A             1
038700010612     C     *IN30         IFEQ      *OFF
038800010612     C     W001A         ORLT      '0'
038900010612     C                   SETON                                        31
039000040524     C                   MOVEL(P)  datcor        VAXAAS
039100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
039200040524     C                             + ' ' + 'VAXAAS'
039300010612     C                   ELSE
039400040524     C                   MOVE      a_vaxaas      VAXAAS
039500010612     C                   ENDIF
039600010612     C*
039700040524     C                   TESTN                   a_vaxnrs             30
039800040524     C  N30              TESTN                   a_vaxnrs               30
039900040524     C  N30              TESTN                   a_vaxnrs                 30
040000040524     C                   MOVE      a_vaxnrs      W001A             1
040100010612     C     *IN30         IFEQ      *OFF
040200010612     C     W001A         ORLT      '0'
040300010612     C     W001A         ANDNE     *BLANK
040400010612     C                   SETON                                        31
040500040524     C                   Z-ADD     *zeros        VAXNRS
040600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
040700040524     C                             + ' ' + 'VAXNRS'
040800010612     C                   ELSE
040900040524     C                   MOVE      a_vaxnrs      VAXNRS
041000010612     C                   ENDIF
041100010612     C*
041200040524     C                   TESTN                   a_vaxnsp             30
041300040524     C  N30              TESTN                   a_vaxnsp               30
041400040524     C  N30              TESTN                   a_vaxnsp                 30
041500040524     C                   MOVE      a_vaxnsp      W001A             1
041600010612     C     *IN30         IFEQ      *OFF
041700010612     C     W001A         ORLT      '0'
041800010612     C                   SETON                                        31
041900040524     C                   Z-ADD     *zeros        VAXNSP
042000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
042100040524     C                             + ' ' + 'VAXNSP'
042200010612     C                   ELSE
042300040524     C                   MOVE      a_vaxnsp      VAXNSP
042400010612     C                   ENDIF
042500040524     C*
042600040524     C                   TESTN                   a_vaxprr             30
042700040524     C  N30              TESTN                   a_vaxprr               30
042800040524     C  N30              TESTN                   a_vaxprr                 30
042900040524     C                   MOVE      a_vaxprr      W001A             1
043000040524     C     *IN30         IFEQ      *OFF
043100040524     C     W001A         ORLT      '0'
043200040524     C                   SETON                                        31
043300040524     C                   Z-ADD     *zeros        VAXPRR
043400040524     C                   EVAL      vinmsg = %trimr(vinmsg)
043500040524     C                             + ' ' + 'VAXPRR'
043600040524     C                   ELSE
043700040524     C                   MOVE      a_vaxprr      VAXPRR
043800040524     C                   ENDIF
043900010612     C*
044000010612     C* ALFABETICI
044100040524     C                   MOVE      a_vaxatb      VAXATB
044200040524     C                   MOVE      a_vaxtmd      VAXTMD
044300040524     C                   MOVE      a_vaxtrk      VAXTRK
044400040524     C                   MOVE      a_vaxdtt      VAXDTT
044500010330     C*
044600010330     C                   ENDSR
044700010612     C*----------------------------------------------------*
044800990920
044900010601
045000010601
045100010601
045200010601
045300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045400010601     C     repfil        BEGSR
045500010601     C*
045600010601     C                   if        invfil = *zeros and
045700010601     C                             depfil > *zeros and
045800010613     C                             (vinflg = *blanks or
045900010613     C                              vinflg = *zeros)
046000010601     C
046100010601     C                   eval      invfil = depfil
046200010601     C                   endif
046300010601     C*
046400010601     C                   if        depfil <> invfil and
046500010601     C                             invfil > *zeros
046600010601     C                   eval      flgMulti = '1'
046700010601     C                   if        vinflg = *blanks
046800010601     C                   add       1             cntNonEl
046900010601     C                   endif
047000010601     C                   endif
047100010601     C*
047200010601     C                   if        vinflg = '2'
047300010601     C                   eval      flgStato = '2'
047400010601     C                   endif
047500010601     C*
047600010601     C                   ENDSR
047700010601     C***
047800010601
047900010601
048000010601
048100010330
048200010330
048300010330
048400990920      /TITLE Invio dei dati al punto operativo.
048500000613     C     invio         BEGSR
048600990920     C*
048700990920     C                   reset                   dscmz
048800010601     C                   move      invfil        cmzdst
048900990920     C                   eval      cmzfld = vlrfou
049000990920     C                   eval      cmzmbd = vlrhdl
049100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
049200000710     C                   if        prmfir = *blanks
049300040524     C                   eval      cmzfla = 'FIVAX00F'
049400040524     C                   eval      cmzmba = 'FIVAX00F'
049500000710     C                   else
049600000710     C                   eval      cmzfla = prmfir
049700000710     C                   eval      cmzmba = prmfir
049800000710     C                   endif
049900990920     C                   eval      cmznrr = *zeros
050000990920     C                   move      §ctrok        cmznrr
050100021003     C                   eval      cmzlba = vlrfl1
050200990920     C                   call(e)   'TIS711C'
050300990920     C                   parm                    dscmz
050400990921     C                   parm      *blanks       esito
050500990920     C                   if        %error
050600990920     C                             or cmzerr = '1'
050700990921     C                             or esito  = '1'
050800000710     C                   eval      wrkesito = '3'
050900990920     C                   endif
051000990920     C*
051100000613     C                   ENDSR
051200990910
051300010601
051400010601
051500010601
051600010601
051700010601      /TITLE Invio dei dati al punto operativo.
051800010601     C     opeini        BEGSR
051900010601     C*
052000010601     C* Inizializzo flag e contatori operativi
052100010601     C                   movel     '0'           flgGiro           1
052200010601     C                   movel     '0'           flgMulti          1
052300010601     C                   movel     '1'           flgStato          1
052400010615     C                   movel     '0'           flgOk             1
052500010601     C                   z-add     *zeros        cntNonEl         10 0
052600010601     C                   z-add     *zeros        depfil            3 0
052700010601     C                   z-add     *zeros        invfil            3 0
052800030407     C*
052900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
053000030407     C                   if        %subst(vlrppt:1:1) = 'S'
053100030407     C                   seton                                        50
053200030407     C                   else
053300030407     C                   setoff                                       50
053400030407     C                   endif
053500010601     C*
053600010601     C                   ENDSR
053700010601     C***
053800010601
053900010601
054000010601
054100010330
054200010330
054300000613     C     *inzsr        BEGSR
054400990910     C*
054500990910     C     *entry        plist
054600990920     C                   parm                    tivlrds
054700990921     C                   parm      wrkesito      esito
054800000724     C                   parm                    prmlit
054900000710     C                   parm                    prmfir
055000010330     C*
055100010330     C* CALCOLA LA DATA CORRENTE
055200010330     C                   time                    wn14             14 0
055300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055400010330     C                   z-add     wn8           g08dat
055500010330     C                   z-add     *zeros        g08inv
055600010330     C                   movel     '0'           g08err
055700010330     C                   call      'XSRDA8'
055800010330     C                   parm                    wlbda8
055900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056000000613     C*
056100000613     C                   ENDSR
056200000613     C***
056300990908
056400040524     Otis7t13p  E            riepilogo         2
056500990915     O                                              'Upload via Internet'
056600990915     O                                           +1 'Traduzione TIVIN00R -'
056700040524     O                                           +1 'FIVAXWWR'
056800990915     O                                           +1 'Testate bolle'
056900990915     O          E            riepilogo   2
057000990915     O                       wrkdata
057100990915     O                       wrkora              +1
057200990915     O                       procname            +1
057300990915     O          E            riepilogo   2
057400990915     O                                              'Cliente..................:'
057500040524     O                       VAXCCM        z     +1
057600990915     O          E            riepilogo   2
057700990920     O                                              'Riferimento Strategi.....:'
057800990920     O                       vlrhdl              +1
057900990915     O          E            riepilogo   2
058000990915     O                                              'Giusti...................:'
058100971022     O                       §CTROK        2   +  1
058200990915     O          E            riepilogo   2
058300990915     O                                              'Sbagliati e corretti.....:'
058400971022     O                       §CTRMO        2   +  1
058500990915     O          E            riepilogo   2
058600990915     O                                              'Sbagliati e scartati.....:'
058700971022     O                       §CTRNO        2   +  1
058800000613
058900040524     Otis7t13ps E            testdett          2
059000000613     O                                              'Upload via Internet'
059100000613     O                                           +1 'Traduzione TIVIN00R -'
059200040524     O                                           +1 'FIVAXWWR'
059300000613     O                                           +1 'Testate bolle'
059400000616     O          E            testdett    3
059500000613     O                                           +2 'N° rec'
059600000613     O                                           +3 'Anteprima contenuto'
059700000616     O          E            rigadett    2
059800000613     O                       rrnum               +2
059900000621     O                       recko               +3
060000000616     O          E            findett     2
060100000613     O                       wrkdata
060200000613     O                       wrkora              +1
060300000613     O                       procname            +1
060400000616     O          E            findett     2
060500000613     O                                              'Cliente..................:'
060600040524     O                       VAXCCM        z     +1
060700000616     O          E            findett     2
060800000613     O                                              'Riferimento Strategi.....:'
060900000613     O                       vlrhdl              +1
061000000616     O          E            findett     2
061100000613     O                                              'Giusti...................:'
061200000613     O                       §CTROK        2   +  1
061300000616     O          E            findett     2
061400000613     O                                              'Sbagliati e corretti.....:'
061500000613     O                       §CTRMO        2   +  1
061600000616     O          E            findett     2
061700000613     O                                              'Sbagliati e scartati.....:'
061800000613     O                       §CTRNO        2   +  1
061900000613** CMD - COMANDI CL
062000040524OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T13P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
062100040524OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T13PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
062200040524DLTOVR FILE(TIS7T13P TIS7T13PS) LVL(*)
062300010606
062400000613
