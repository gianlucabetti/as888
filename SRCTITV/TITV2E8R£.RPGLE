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
021200010601     C*
021300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400040524     C                   eval      depfil = VAXLNP
021500010601     C                   exsr      repfil
021600021010     C                   if        depfil = invfil
021700021014     C                   if        vlrpoi = 999
021800040524     C                   MOVE(P)   invfil        VAXFGS
021900021014     C                   else
022000040524     C                   MOVE(P)   vlrpoi        VAXFGS
022100021014     C                   endif
022200010601     C*
022300010601     C                   exsr      PREELA
022400110429     C*
022500110429     C* Forzature x diversa gestione traffici ILLY
022600110429     C*
022700110429     C* CCM
022800110429     C                   MOVEL     VAXNSP        wCLI              1
022900110429     C                   IF        wCLI = '7'
023000110429     C                   EVAL      VAXCCM = 1162074
023100110429     C                   ENDIF
023200010601     C*
023300010604     C* Ebbene...
023400010604     C*
023500010604     C  N31              ADD       1             §CTROK            7 0
023600010604     C   32              ADD       1             §CTRMO            7 0
023700010604     C   31              ADD       1             §CTRNO            7 0
023800040524     C  N31              WRITE     FIVAX000
023900010604     C*
024000010604     C                   if        *in31 = *off and
024100010604     C                             *in32 = *off
024200010604     C                   eval      vinflg = '1'
024300010604     C                   else
024400010604     C                   eval      recko = vindta
024500040524     C                   if        %open(tis7t13ps)
024600030407     C   50              except    rigadett
024700010612     C                   endif
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000010330     C  N70              ENDdo
026100010601     C
026200010601     C*
026300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026400010601     C                   if        cntNonEl = *zeros or
026500010601     C                             flgMulti = '0'
026600010330     C* Se non ci sono record con errori ...
026700010601     C                   if        §ctrno = 0 and
026800010604     C                             §ctrmo = 0 and
026900010601     C                             flgStato <> '2'
027000010330     C* ... restituisco esito OK.
027100010330     C                   eval      wrkesito = '0'
027200010330     C                   else
027300010330     C                   if        §ctrok > 0
027400010330     C                   eval      wrkesito = '1'
027500010330     C                   else
027600010615     C                   if        flgOk = '0'
027700010615     C                   eval      wrkesito = '2'
027800010615     C                   else
027900010615     C                   eval      wrkesito = '6'
028000010615     C                   endif
028100010330     C                   endif
028200010330     C                   endif
028300010601     C                   else
028400010601     C                   eval      wrkesito = '9'
028500010601     C                   endif
028600010330     C*
028700010330     C                   if        %open(tivin00r)
028800010330     C                   close     tivin00r
028900010330     C                   endif
029000040524     C                   if        %open(fivaxwwr)
029100040524     C                   close     fivaxwwr
029200010330     C                   endif
029300010601     C*
029400010601     C                   if        vlrpoi <> 999
029500010601     C                   eval      invfil = vlrpoi
029600010601     C                   endif
029700010330     C*
029800010330     C                   if        §ctrok > 0
029900010601     C                             and invfil > *zeros
030000010330     C                   exsr      invio
030100010330     C                   endif
030200010601     C*
030300010612     C                   if        flgGiro = '1'
030400010601     C                   exsr      endela
030500010612     C                   endif
030600010330     C*
030700010330     C                   ENDSR
030800010330     C***
030900010330
031000010601
031100010601
031200010601
031300010601
031400010330     C*----------------------------------------------------*
031500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031600010330     C*----------------------------------------------------*
031700010330     C     INZVAR        BEGSR
031800010330     C*
031900010330     C*
032000010330     C                   ENDSR
032100010330     C*----------------------------------------------------*
032200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
032300010330     C*----------------------------------------------------*
032400010330     C     DEFCAM        BEGSR
032500010330     C*
032600040524     C                   CLEAR                   FIVAX000
032700010330     C*
032800010330     C                   ENDSR
032900010330     C*----------------------------------------------------*
033000040524     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
033100010330     C*----------------------------------------------------*
033200040524     C     IMPVAX        BEGSR
033300010330     C*
033400010604     C                   SETOFF                                       3132
033500010612     C*
033600010612     C                   SETOFF                                       303132
033700010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
033800040524     C                   TESTN                   a_vaxccm             30
033900040524     C  N30              TESTN                   a_vaxccm               30
034000040524     C  N30              TESTN                   a_vaxccm                 30
034100040524     C                   MOVE      a_vaxccm      W001A             1
034200010612     C     *IN30         IFEQ      *OFF
034300010612     C     W001A         ORLT      '0'
034400010612     C                   SETON                                        31
034500040524     C                   Z-ADD     *zeros        VAXCCM
034600040524     C                   EVAL      vinmsg = 'VAXCCM'
034700010612     C                   ELSE
034800040524     C                   MOVE      a_vaxccm      VAXCCM
034900040524     C     VAXCCM        IFLE      *ZEROS
035000010612     C                   SETON                                        31
035100040524     C                   Z-ADD     *zeros        VAXCCM
035200040524     C                   EVAL      vinmsg = 'VAXCCM'
035300010612     C                   ENDIF
035400010612     C                   ENDIF
035500010612     C*
035600040524     C                   TESTN                   a_vaxlnp             30
035700040524     C  N30              TESTN                   a_vaxlnp               30
035800040524     C  N30              TESTN                   a_vaxlnp                 30
035900040524     C                   MOVE      a_vaxlnp      W001A             1
036000010612     C     *IN30         IFEQ      *OFF
036100010612     C     W001A         ORLT      '0'
036200010612     C                   SETON                                        31
036300040524     C                   Z-ADD     *zeros        VAXLNP
036400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
036500040524     C                             + ' ' + 'VAXLNP'
036600010612     C                   ELSE
036700040524     C                   MOVE      a_vaxlnp      VAXLNP
036800040524     C     VAXLNP        IFLE      *ZEROS
036900010612     C                   SETON                                        31
037000040524     C                   Z-ADD     *zeros        VAXLNP
037100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
037200040524     C                             + ' ' + 'VAXLNP'
037300010612     C                   ENDIF
037400010612     C                   ENDIF
037500010612     C*
037600040524     C                   TESTN                   a_vaxaas             30
037700040524     C  N30              TESTN                   a_vaxaas               30
037800040524     C  N30              TESTN                   a_vaxaas                 30
037900040524     C                   MOVE      a_vaxaas      W001A             1
038000010612     C     *IN30         IFEQ      *OFF
038100010612     C     W001A         ORLT      '0'
038200010612     C                   SETON                                        31
038300040524     C                   MOVEL(P)  datcor        VAXAAS
038400010612     C                   EVAL      vinmsg = %trimr(vinmsg)
038500040524     C                             + ' ' + 'VAXAAS'
038600010612     C                   ELSE
038700040524     C                   MOVE      a_vaxaas      VAXAAS
038800010612     C                   ENDIF
038900010612     C*
039000040524     C                   TESTN                   a_vaxnrs             30
039100040524     C  N30              TESTN                   a_vaxnrs               30
039200040524     C  N30              TESTN                   a_vaxnrs                 30
039300040524     C                   MOVE      a_vaxnrs      W001A             1
039400010612     C     *IN30         IFEQ      *OFF
039500010612     C     W001A         ORLT      '0'
039600010612     C     W001A         ANDNE     *BLANK
039700010612     C                   SETON                                        31
039800040524     C                   Z-ADD     *zeros        VAXNRS
039900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
040000040524     C                             + ' ' + 'VAXNRS'
040100010612     C                   ELSE
040200040524     C                   MOVE      a_vaxnrs      VAXNRS
040300010612     C                   ENDIF
040400010612     C*
040500040524     C                   TESTN                   a_vaxnsp             30
040600040524     C  N30              TESTN                   a_vaxnsp               30
040700040524     C  N30              TESTN                   a_vaxnsp                 30
040800040524     C                   MOVE      a_vaxnsp      W001A             1
040900010612     C     *IN30         IFEQ      *OFF
041000010612     C     W001A         ORLT      '0'
041100010612     C                   SETON                                        31
041200040524     C                   Z-ADD     *zeros        VAXNSP
041300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
041400040524     C                             + ' ' + 'VAXNSP'
041500010612     C                   ELSE
041600040524     C                   MOVE      a_vaxnsp      VAXNSP
041700010612     C                   ENDIF
041800040524     C*
041900040524     C                   TESTN                   a_vaxprr             30
042000040524     C  N30              TESTN                   a_vaxprr               30
042100040524     C  N30              TESTN                   a_vaxprr                 30
042200040524     C                   MOVE      a_vaxprr      W001A             1
042300040524     C     *IN30         IFEQ      *OFF
042400040524     C     W001A         ORLT      '0'
042500040524     C                   SETON                                        31
042600040524     C                   Z-ADD     *zeros        VAXPRR
042700040524     C                   EVAL      vinmsg = %trimr(vinmsg)
042800040524     C                             + ' ' + 'VAXPRR'
042900040524     C                   ELSE
043000040524     C                   MOVE      a_vaxprr      VAXPRR
043100040524     C                   ENDIF
043200010612     C*
043300010612     C* ALFABETICI
043400040524     C                   MOVE      a_vaxatb      VAXATB
043500040524     C                   MOVE      a_vaxtmd      VAXTMD
043600040524     C                   MOVE      a_vaxtrk      VAXTRK
043700040524     C                   MOVE      a_vaxdtt      VAXDTT
043800010330     C*
043900010330     C                   ENDSR
044000010612     C*----------------------------------------------------*
044100990920
044200010601
044300010601
044400010601
044500010601
044600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044700010601     C     repfil        BEGSR
044800010601     C*
044900010601     C                   if        invfil = *zeros and
045000010601     C                             depfil > *zeros and
045100010613     C                             (vinflg = *blanks or
045200010613     C                              vinflg = *zeros)
045300010601     C
045400010601     C                   eval      invfil = depfil
045500010601     C                   endif
045600010601     C*
045700010601     C                   if        depfil <> invfil and
045800010601     C                             invfil > *zeros
045900010601     C                   eval      flgMulti = '1'
046000010601     C                   if        vinflg = *blanks
046100010601     C                   add       1             cntNonEl
046200010601     C                   endif
046300010601     C                   endif
046400010601     C*
046500010601     C                   if        vinflg = '2'
046600010601     C                   eval      flgStato = '2'
046700010601     C                   endif
046800010601     C*
046900010601     C                   ENDSR
047000010601     C***
047100010601
047200010601
047300010601
047400010330
047500010330
047600010330
047700990920      /TITLE Invio dei dati al punto operativo.
047800000613     C     invio         BEGSR
047900990920     C*
048000990920     C                   reset                   dscmz
048100010601     C                   move      invfil        cmzdst
048200990920     C                   eval      cmzfld = vlrfou
048300990920     C                   eval      cmzmbd = vlrhdl
048400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048500000710     C                   if        prmfir = *blanks
048600040524     C                   eval      cmzfla = 'FIVAX00F'
048700040524     C                   eval      cmzmba = 'FIVAX00F'
048800000710     C                   else
048900000710     C                   eval      cmzfla = prmfir
049000000710     C                   eval      cmzmba = prmfir
049100000710     C                   endif
049200990920     C                   eval      cmznrr = *zeros
049300990920     C                   move      §ctrok        cmznrr
049400021003     C                   eval      cmzlba = vlrfl1
049500990920     C                   call(e)   'TIS711C'
049600990920     C                   parm                    dscmz
049700990921     C                   parm      *blanks       esito
049800990920     C                   if        %error
049900990920     C                             or cmzerr = '1'
050000990921     C                             or esito  = '1'
050100000710     C                   eval      wrkesito = '3'
050200990920     C                   endif
050300990920     C*
050400000613     C                   ENDSR
050500990910
050600010601
050700010601
050800010601
050900010601
051000010601      /TITLE Invio dei dati al punto operativo.
051100010601     C     opeini        BEGSR
051200010601     C*
051300010601     C* Inizializzo flag e contatori operativi
051400010601     C                   movel     '0'           flgGiro           1
051500010601     C                   movel     '0'           flgMulti          1
051600010601     C                   movel     '1'           flgStato          1
051700010615     C                   movel     '0'           flgOk             1
051800010601     C                   z-add     *zeros        cntNonEl         10 0
051900010601     C                   z-add     *zeros        depfil            3 0
052000010601     C                   z-add     *zeros        invfil            3 0
052100030407     C*
052200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
052300030407     C                   if        %subst(vlrppt:1:1) = 'S'
052400030407     C                   seton                                        50
052500030407     C                   else
052600030407     C                   setoff                                       50
052700030407     C                   endif
052800010601     C*
052900010601     C                   ENDSR
053000010601     C***
053100010601
053200010601
053300010601
053400010330
053500010330
053600000613     C     *inzsr        BEGSR
053700990910     C*
053800990910     C     *entry        plist
053900990920     C                   parm                    tivlrds
054000990921     C                   parm      wrkesito      esito
054100000724     C                   parm                    prmlit
054200000710     C                   parm                    prmfir
054300010330     C*
054400010330     C* CALCOLA LA DATA CORRENTE
054500010330     C                   time                    wn14             14 0
054600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054700010330     C                   z-add     wn8           g08dat
054800010330     C                   z-add     *zeros        g08inv
054900010330     C                   movel     '0'           g08err
055000010330     C                   call      'XSRDA8'
055100010330     C                   parm                    wlbda8
055200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055300000613     C*
055400000613     C                   ENDSR
055500000613     C***
055600990908
055700040524     Otis7t13p  E            riepilogo         2
055800990915     O                                              'Upload via Internet'
055900990915     O                                           +1 'Traduzione TIVIN00R -'
056000040524     O                                           +1 'FIVAXWWR'
056100990915     O                                           +1 'Testate bolle'
056200990915     O          E            riepilogo   2
056300990915     O                       wrkdata
056400990915     O                       wrkora              +1
056500990915     O                       procname            +1
056600990915     O          E            riepilogo   2
056700990915     O                                              'Cliente..................:'
056800040524     O                       VAXCCM        z     +1
056900990915     O          E            riepilogo   2
057000990920     O                                              'Riferimento Strategi.....:'
057100990920     O                       vlrhdl              +1
057200990915     O          E            riepilogo   2
057300990915     O                                              'Giusti...................:'
057400971022     O                       §CTROK        2   +  1
057500990915     O          E            riepilogo   2
057600990915     O                                              'Sbagliati e corretti.....:'
057700971022     O                       §CTRMO        2   +  1
057800990915     O          E            riepilogo   2
057900990915     O                                              'Sbagliati e scartati.....:'
058000971022     O                       §CTRNO        2   +  1
058100000613
058200040524     Otis7t13ps E            testdett          2
058300000613     O                                              'Upload via Internet'
058400000613     O                                           +1 'Traduzione TIVIN00R -'
058500040524     O                                           +1 'FIVAXWWR'
058600000613     O                                           +1 'Testate bolle'
058700000616     O          E            testdett    3
058800000613     O                                           +2 'N° rec'
058900000613     O                                           +3 'Anteprima contenuto'
059000000616     O          E            rigadett    2
059100000613     O                       rrnum               +2
059200000621     O                       recko               +3
059300000616     O          E            findett     2
059400000613     O                       wrkdata
059500000613     O                       wrkora              +1
059600000613     O                       procname            +1
059700000616     O          E            findett     2
059800000613     O                                              'Cliente..................:'
059900040524     O                       VAXCCM        z     +1
060000000616     O          E            findett     2
060100000613     O                                              'Riferimento Strategi.....:'
060200000613     O                       vlrhdl              +1
060300000616     O          E            findett     2
060400000613     O                                              'Giusti...................:'
060500000613     O                       §CTROK        2   +  1
060600000616     O          E            findett     2
060700000613     O                                              'Sbagliati e corretti.....:'
060800000613     O                       §CTRMO        2   +  1
060900000616     O          E            findett     2
061000000613     O                                              'Sbagliati e scartati.....:'
061100000613     O                       §CTRNO        2   +  1
061200000613** CMD - COMANDI CL
061300040524OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T13P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
061400040524OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T13PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
061500040524DLTOVR FILE(TIS7T13P TIS7T13PS) LVL(*)
061600010606
061700000613
