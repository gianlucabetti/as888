000100070419      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600070419     Ffivaxwwr  O    E             DISK    usropn
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
004200070413     D  a_vaxccm               2      8  0
004300070326     D  a_vaxldv              16     26
004400070202     D  a_vaxnsp              20     26
004500070201     D  a_vaxprr              29     32
004600070201     D  a_vaxtrk              33     34
004700070201     D  a_vaxdtt              35    534
004800010330
004900010330
005000010330
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600010601     C*
005700010601     C                   exsr      opeini
005800040524     C                   exsr      rwvax
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011123     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011123     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200040524     C                   if        not %open(tis7t13ps)
010300040524     C                   open      tis7t13ps
010400030407     C   50              except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200990915     C*
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500040524     C                   if        %open(tis7t13ps)
012600030407     C   50              except    findett
012700040524     C                   close     tis7t13ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011123     C                   Z-ADD     150           LENGH            15 5
013200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000010601
014100010601
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800040524     C                   if        not %open(tis7t13p)
014900040524     C                   open      tis7t13p
015000990915     C                   endif
015100990915     C*
015200030407     C   50              except    riepilogo
015300990915     C*
015400040524     C                   if        %open(tis7t13p)
015500040524     C                   close     tis7t13p
015600990914     C                   endif
015700000613     C*
015800000613     C                   ENDSR
015900000613     C***
016000000613
016100010330
016200010601
016300010601
016400010601
016500010330     C*--------------------------------------------------------
016600070419     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
016700010330     C*--------------------------------------------------------
016800040524     C     RWVAX         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300070419     C                   if        not %open(fivaxwwr)
017400070419     C                   open      fivaxwwr
017500010330     C                   endif
017600010330     C*
017700010604     C                   clear                   §CTROK
017800010604     C                   clear                   §CTRMO
017900010604     C                   clear                   §CTRNO
018000010330     C*
018100010330     C                   DO        *HIVAL
018200010330     C*
018300010330     C                   READ      tivin00r                               70
018400010618     C*
018500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018600010618     C                   if        vinflg = '1'
018700010618     C                   eval      flgOk = '1'
018800010618     C                   endif
018900010618     C*
019000010330     C                   if        vindta > *blanks
019100010330     C                   add       1             rrnum
019200010330     C*
019300010601     C                   if        *in70 = *off and
019400010330     C                             (vinflg = *blanks
019500010330     C                              or vinflg = '0'
019600010330     C                              or vinflg = '2')
019700010711     C*
019800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019900010711     C                   if        vinflg = *blanks or vinflg = '0'
020000010711     C                   clear                   vinmsg
020100010711     C                   endif
020200010711     C*
020300040524     C                   eval      fnvaxaar = vindta
020400010601     C*
020500010330     C                   exsr      inzvar
020600010330     C                   exsr      defcam
020700040524     C                   exsr      impvax
020800070115     C*
020900070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000070115     C                   eval      depfil = VAXLNP
021100010601     C*
021200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021300010601     C                   exsr      repfil
021400021010     C                   if        depfil = invfil
021500021014     C                   if        vlrpoi = 999
021600040524     C                   MOVE(P)   invfil        VAXFGS
021700021014     C                   else
021800040524     C                   MOVE(P)   vlrpoi        VAXFGS
021900021014     C                   endif
022000010601     C*
022100010601     C                   exsr      PREELA
022200010601     C*
022300010604     C* Ebbene...
022400010604     C*
022500010604     C  N31              ADD       1             §CTROK            7 0
022600010604     C   32              ADD       1             §CTRMO            7 0
022700010604     C   31              ADD       1             §CTRNO            7 0
022800070419     C  N31              WRITE     FIVAX000
022900010604     C*
023000010604     C                   if        *in31 = *off and
023100010604     C                             *in32 = *off
023200010604     C                   eval      vinflg = '1'
023300010604     C                   else
023400010604     C                   eval      recko = vindta
023500040524     C                   if        %open(tis7t13ps)
023600030407     C   50              except    rigadett
023700010612     C                   endif
023800010604     C                   eval      vinflg = '2'
023900010604     C                   endif
024000010604     C                   endif
024100010601     C*
024200010604     C                   endif
024300010604     C*
024400010330     C                   else
024500010330     C                   eval      vinflg = '1'
024600010330     C                   endif
024700010601     C*
024800010601     C  N70              update    tivin000
024900010330     C*
025000010330     C  N70              ENDdo
025100010601     C
025200010601     C*
025300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025400010601     C                   if        cntNonEl = *zeros or
025500010601     C                             flgMulti = '0'
025600010330     C* Se non ci sono record con errori ...
025700010601     C                   if        §ctrno = 0 and
025800010604     C                             §ctrmo = 0 and
025900010601     C                             flgStato <> '2'
026000010330     C* ... restituisco esito OK.
026100010330     C                   eval      wrkesito = '0'
026200010330     C                   else
026300010330     C                   if        §ctrok > 0
026400010330     C                   eval      wrkesito = '1'
026500010330     C                   else
026600010615     C                   if        flgOk = '0'
026700010615     C                   eval      wrkesito = '2'
026800010615     C                   else
026900010615     C                   eval      wrkesito = '6'
027000010615     C                   endif
027100010330     C                   endif
027200010330     C                   endif
027300010601     C                   else
027400010601     C                   eval      wrkesito = '9'
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        %open(tivin00r)
027800010330     C                   close     tivin00r
027900010330     C                   endif
028000070419     C                   if        %open(fivaxwwr)
028100070419     C                   close     fivaxwwr
028200010330     C                   endif
028300010601     C*
028400010601     C                   if        vlrpoi <> 999
028500010601     C                   eval      invfil = vlrpoi
028600010601     C                   endif
028700010330     C*
028800010330     C                   if        §ctrok > 0
028900010601     C                             and invfil > *zeros
029000010330     C                   exsr      invio
029100010330     C                   endif
029200010601     C*
029300010612     C                   if        flgGiro = '1'
029400010601     C                   exsr      endela
029500010612     C                   endif
029600010330     C*
029700010330     C                   ENDSR
029800010330     C***
029900010330
030000010601
030100010601
030200010601
030300010601
030400010330     C*----------------------------------------------------*
030500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030600010330     C*----------------------------------------------------*
030700010330     C     INZVAR        BEGSR
030800010330     C*
030900010330     C*
031000010330     C                   ENDSR
031100010330     C*----------------------------------------------------*
031200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031300010330     C*----------------------------------------------------*
031400010330     C     DEFCAM        BEGSR
031500010330     C*
031600070419     C                   CLEAR                   FIVAX000
031700070201     C*
031800070201     C* Imposto i valori d default
031900070413     C                   EVAL      VAXCCM = a_vaxccm
032000070201     C                   EVAL      VAXLNP = 062
032100070413     C                   IF        VAXCCM = 0621257
032200070413     C                   EVAL      VAXTMD = 'ITR1'
032300070413     C                   ENDIF
032400070413     C                   IF        VAXCCM = 0620804
032500070413     C                   EVAL      VAXTMD = 'ITR2'
032600070413     C                   ENDIF
032700080716     C                   IF        VAXCCM = 0621602
032800080716     C                   EVAL      VAXTMD = 'ITR2'
032900080716     C                   ENDIF
033000070202     C                   MOVEL     datcor        VAXAAS
033100010330     C*
033200010330     C                   ENDSR
033300010330     C*----------------------------------------------------*
033400070419     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
033500010330     C*----------------------------------------------------*
033600040524     C     IMPVAX        BEGSR
033700010330     C*
033800010604     C                   SETOFF                                       3132
033900010612     C*
034000010612     C                   SETOFF                                       303132
034100010612     C*
034200040524     C                   TESTN                   a_vaxnsp             30
034300040524     C  N30              TESTN                   a_vaxnsp               30
034400040524     C  N30              TESTN                   a_vaxnsp                 30
034500040524     C                   MOVE      a_vaxnsp      W001A             1
034600010612     C     *IN30         IFEQ      *OFF
034700010612     C     W001A         ORLT      '0'
034800010612     C                   SETON                                        31
034900040524     C                   Z-ADD     *zeros        VAXNSP
035000010612     C                   EVAL      vinmsg = %trimr(vinmsg)
035100040524     C                             + ' ' + 'VAXNSP'
035200010612     C                   ELSE
035300040524     C                   MOVE      a_vaxnsp      VAXNSP
035400010612     C                   ENDIF
035500040524     C*
035600040524     C                   TESTN                   a_vaxprr             30
035700040524     C  N30              TESTN                   a_vaxprr               30
035800040524     C  N30              TESTN                   a_vaxprr                 30
035900040524     C                   MOVE      a_vaxprr      W001A             1
036000040524     C     *IN30         IFEQ      *OFF
036100040524     C     W001A         ORLT      '0'
036200040524     C                   SETON                                        31
036300040524     C                   Z-ADD     *zeros        VAXPRR
036400040524     C                   EVAL      vinmsg = %trimr(vinmsg)
036500040524     C                             + ' ' + 'VAXPRR'
036600040524     C                   ELSE
036700040524     C                   MOVE      a_vaxprr      VAXPRR
036800040524     C                   ENDIF
036900010612     C*
037000010612     C* ALFABETICI
037100040524     C                   MOVE      a_vaxtrk      VAXTRK
037200040524     C                   MOVE      a_vaxdtt      VAXDTT
037300010330     C*
037400010330     C                   ENDSR
037500010612     C*----------------------------------------------------*
037600990920
037700010601
037800010601
037900010601
038000010601
038100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038200010601     C     repfil        BEGSR
038300010601     C*
038400010601     C                   if        invfil = *zeros and
038500010601     C                             depfil > *zeros and
038600010613     C                             (vinflg = *blanks or
038700010613     C                              vinflg = *zeros)
038800010601     C
038900010601     C                   eval      invfil = depfil
039000010601     C                   endif
039100010601     C*
039200010601     C                   if        depfil <> invfil and
039300010601     C                             invfil > *zeros
039400010601     C                   eval      flgMulti = '1'
039500010601     C                   if        vinflg = *blanks
039600010601     C                   add       1             cntNonEl
039700010601     C                   endif
039800010601     C                   endif
039900010601     C*
040000010601     C                   if        vinflg = '2'
040100010601     C                   eval      flgStato = '2'
040200010601     C                   endif
040300010601     C*
040400010601     C                   ENDSR
040500010601     C***
040600010601
040700010601
040800010601
040900010330
041000010330
041100010330
041200990920      /TITLE Invio dei dati al punto operativo.
041300000613     C     invio         BEGSR
041400990920     C*
041500990920     C                   reset                   dscmz
041600010601     C                   move      invfil        cmzdst
041700990920     C                   eval      cmzfld = vlrfou
041800990920     C                   eval      cmzmbd = vlrhdl
041900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042000000710     C                   if        prmfir = *blanks
042100070419     C                   eval      cmzfla = 'FIVAX00F'
042200070419     C                   eval      cmzmba = 'FIVAX00F'
042300000710     C                   else
042400000710     C                   eval      cmzfla = prmfir
042500000710     C                   eval      cmzmba = prmfir
042600000710     C                   endif
042700990920     C                   eval      cmznrr = *zeros
042800990920     C                   move      §ctrok        cmznrr
042900021003     C                   eval      cmzlba = vlrfl1
043000990920     C                   call(e)   'TIS711C'
043100990920     C                   parm                    dscmz
043200990921     C                   parm      *blanks       esito
043300990920     C                   if        %error
043400990920     C                             or cmzerr = '1'
043500990921     C                             or esito  = '1'
043600000710     C                   eval      wrkesito = '3'
043700990920     C                   endif
043800990920     C*
043900000613     C                   ENDSR
044000990910
044100010601
044200010601
044300010601
044400010601
044500010601      /TITLE Invio dei dati al punto operativo.
044600010601     C     opeini        BEGSR
044700010601     C*
044800010601     C* Inizializzo flag e contatori operativi
044900010601     C                   movel     '0'           flgGiro           1
045000010601     C                   movel     '0'           flgMulti          1
045100010601     C                   movel     '1'           flgStato          1
045200010615     C                   movel     '0'           flgOk             1
045300010601     C                   z-add     *zeros        cntNonEl         10 0
045400010601     C                   z-add     *zeros        depfil            3 0
045500010601     C                   z-add     *zeros        invfil            3 0
045600030407     C*
045700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
045800030407     C                   if        %subst(vlrppt:1:1) = 'S'
045900030407     C                   seton                                        50
046000030407     C                   else
046100030407     C                   setoff                                       50
046200030407     C                   endif
046300010601     C*
046400010601     C                   ENDSR
046500010601     C***
046600010601
046700010601
046800010601
046900010330
047000010330
047100000613     C     *inzsr        BEGSR
047200990910     C*
047300990910     C     *entry        plist
047400990920     C                   parm                    tivlrds
047500990921     C                   parm      wrkesito      esito
047600000724     C                   parm                    prmlit
047700000710     C                   parm                    prmfir
047800010330     C*
047900010330     C* CALCOLA LA DATA CORRENTE
048000010330     C                   time                    wn14             14 0
048100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048200010330     C                   z-add     wn8           g08dat
048300010330     C                   z-add     *zeros        g08inv
048400010330     C                   movel     '0'           g08err
048500010330     C                   call      'XSRDA8'
048600010330     C                   parm                    wlbda8
048700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048800000613     C*
048900000613     C                   ENDSR
049000000613     C***
049100990908
049200040524     Otis7t13p  E            riepilogo         2
049300990915     O                                              'Upload via Internet'
049400990915     O                                           +1 'Traduzione TIVIN00R -'
049500070419     O                                           +1 'FIVAXWWR'
049600990915     O                                           +1 'Testate bolle'
049700990915     O          E            riepilogo   2
049800990915     O                       wrkdata
049900990915     O                       wrkora              +1
050000990915     O                       procname            +1
050100990915     O          E            riepilogo   2
050200990915     O                                              'Cliente..................:'
050300040524     O                       VAXCCM        z     +1
050400990915     O          E            riepilogo   2
050500990920     O                                              'Riferimento Strategi.....:'
050600990920     O                       vlrhdl              +1
050700990915     O          E            riepilogo   2
050800990915     O                                              'Giusti...................:'
050900971022     O                       §CTROK        2   +  1
051000990915     O          E            riepilogo   2
051100990915     O                                              'Sbagliati e corretti.....:'
051200971022     O                       §CTRMO        2   +  1
051300990915     O          E            riepilogo   2
051400990915     O                                              'Sbagliati e scartati.....:'
051500971022     O                       §CTRNO        2   +  1
051600000613
051700040524     Otis7t13ps E            testdett          2
051800000613     O                                              'Upload via Internet'
051900000613     O                                           +1 'Traduzione TIVIN00R -'
052000070419     O                                           +1 'FIVAXXWR'
052100000613     O                                           +1 'Testate bolle'
052200000616     O          E            testdett    3
052300000613     O                                           +2 'N° rec'
052400000613     O                                           +3 'Anteprima contenuto'
052500000616     O          E            rigadett    2
052600000613     O                       rrnum               +2
052700000621     O                       recko               +3
052800000616     O          E            findett     2
052900000613     O                       wrkdata
053000000613     O                       wrkora              +1
053100000613     O                       procname            +1
053200000616     O          E            findett     2
053300000613     O                                              'Cliente..................:'
053400040524     O                       VAXCCM        z     +1
053500000616     O          E            findett     2
053600000613     O                                              'Riferimento Strategi.....:'
053700000613     O                       vlrhdl              +1
053800000616     O          E            findett     2
053900000613     O                                              'Giusti...................:'
054000000613     O                       §CTROK        2   +  1
054100000616     O          E            findett     2
054200000613     O                                              'Sbagliati e corretti.....:'
054300000613     O                       §CTRMO        2   +  1
054400000616     O          E            findett     2
054500000613     O                                              'Sbagliati e scartati.....:'
054600000613     O                       §CTRNO        2   +  1
054700000613** CMD - COMANDI CL
054800040524OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T13P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
054900040524OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T13PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
055000040524DLTOVR FILE(TIS7T13P TIS7T13PS) LVL(*)
055100010606
055200000613
