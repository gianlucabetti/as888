000100030327      /TITLE Upload via Internet: traduzione in FIVABWWR. (x FINDER Cod. 0492057)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500030130     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900010330     D*------------------
002000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100010330     D*------------------
002200010330     D WLBDA8          DS                  INZ
002300010330     D  G08DAT                 1      8  0
002400010330     D  G08INV                 9     16  0
002500010330     D  G08ERR                17     17
002600010330     D  G08TGI                18     22  0
002700000313     D*
002800010330
002900010330
003000010330
003100000913     C                   reset                   rrnum
003200990921     C                   reset                   esito
003300990921     C                   reset                   wrkesito
003400010601     C*
003500010601     C                   exsr      opeini
003600010605     C                   exsr      rwvab
003700010601     C*
003800010601     C                   seton                                        lr
003900010601
004000010601
004100010601
004200010601
004300010601     C*--------------------------------------------------------
004400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004500010601     C*--------------------------------------------------------
004600010601     C     PREELA        BEGSR
004700010601     C*
004800010601     C* SE OCCORRE SPEDIRE IN FILIALE
004900010601     C                   if        invfil <> *zeros and
005000010601     C                             flgGiro = '0'
005100010601     C*
005200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005300010601     C                   eval      flgGiro = '1'
005400010601     C*
005500010601     C                   endif
005600010601     C*
005700010601     C                   ENDSR
005800010601     C***
005900010601
006000010601
006100010601
006200010601
006300010601
006400010601
006500010601
006600010601     C*--------------------------------------------------------
006700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006800010601     C*--------------------------------------------------------
006900010601     C     ENDELA        BEGSR
007000000616     C*
007100010601     C                   ENDSR
007200010601     C***
007300000613
007400010330
007500010601
007600010601
007700010601
007800010330     C*--------------------------------------------------------
007900030130     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008000010330     C*--------------------------------------------------------
008100010605     C     RWVAB         BEGSR
008200010330     C*
008300010330     C                   if        not %open(tivin00r)
008400010330     C                   open      tivin00r
008500010330     C                   endif
008600030130     C                   if        not %open(fivabwwr)
008700030130     C                   open      fivabwwr
008800010330     C                   endif
008900010330     C*
009000010604     C                   clear                   §CTROK
009100010604     C                   clear                   §CTRMO
009200010604     C                   clear                   §CTRNO
009300010330     C*
009400010330     C                   DO        *HIVAL
009500010330     C*
009600010330     C                   READ      tivin00r                               70
009700010618     C*
009800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009900010618     C                   if        vinflg = '1'
010000010618     C                   eval      flgOk = '1'
010100010618     C                   endif
010200010618     C*
010300010330     C                   if        vindta > *blanks
010400010330     C                   add       1             rrnum
010500010330     C*
010600010601     C                   if        *in70 = *off and
010700010330     C                             (vinflg = *blanks
010800010330     C                              or vinflg = '0'
010900010330     C                              or vinflg = '2')
011000010330     C*
011100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011200010711     C                   if        vinflg = *blanks or vinflg = '0'
011300010711     C                   clear                   vinmsg
011400010711     C                   endif
011500010601     C*
011600010330     C                   exsr      inzvar
011700010330     C                   exsr      defcam
011800010605     C                   exsr      impvab
011900010601     C*
012000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012100010605     C                   eval      depfil = VABLNP
012200010601     C                   exsr      repfil
012300010601     C                   if        depfil = invfil
012400030130     C                   if        vlrpoi = 999
012500030130     C                   MOVE(P)   invfil        VABFGS
012600030130     C                   else
012700030130     C                   MOVE(P)   vlrpoi        VABFGS
012800030130     C                   endif
012900010601     C*
013000010601     C                   exsr      PREELA
013100010601     C*
013200010604     C* Ebbene...
013300010604     C*
013400010604     C  N31              ADD       1             §CTROK            7 0
013500010604     C   32              ADD       1             §CTRMO            7 0
013600010604     C   31              ADD       1             §CTRNO            7 0
013700020205     C*
013800020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
013900030130     C  N31              WRITE     FIVAB000
014000020722     C*
014100010604     C                   if        *in31 = *off and
014200010604     C                             *in32 = *off
014300010604     C                   eval      vinflg = '1'
014400010604     C                   else
014500010604     C                   eval      vinflg = '2'
014600010604     C                   endif
014700010604     C                   endif
014800010601     C*
014900010604     C                   endif
015000010604     C*
015100010330     C                   else
015200010330     C                   eval      vinflg = '1'
015300010330     C                   endif
015400010601     C*
015500010601     C  N70              update    tivin000
015600010330     C*
015700010330     C  N70              ENDdo
015800010601     C
015900010601     C*
016000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016100010601     C                   if        cntNonEl = *zeros or
016200010601     C                             flgMulti = '0'
016300010330     C* Se non ci sono record con errori ...
016400010601     C                   if        §ctrno = 0 and
016500010604     C                             §ctrmo = 0 and
016600010601     C                             flgStato <> '2'
016700010330     C* ... restituisco esito OK.
016800010330     C                   eval      wrkesito = '0'
016900010330     C                   else
017000010330     C                   if        §ctrok > 0
017100010330     C                   eval      wrkesito = '1'
017200010330     C                   else
017300010615     C                   if        flgOk = '0'
017400010615     C                   eval      wrkesito = '2'
017500010615     C                   else
017600010615     C                   eval      wrkesito = '6'
017700010615     C                   endif
017800010330     C                   endif
017900010330     C                   endif
018000010601     C                   else
018100010601     C                   eval      wrkesito = '9'
018200010601     C                   endif
018300010330     C*
018400010330     C                   if        %open(tivin00r)
018500010330     C                   close     tivin00r
018600010330     C                   endif
018700030130     C                   if        %open(fivabwwr)
018800030130     C                   close     fivabwwr
018900010330     C                   endif
019000010601     C*
019100010601     C                   if        vlrpoi <> 999
019200010601     C                   eval      invfil = vlrpoi
019300010601     C                   endif
019400010330     C*
019500010330     C                   if        §ctrok > 0
019600010601     C                             and invfil > *zeros
019700010330     C                   exsr      invio
019800010330     C                   endif
019900010601     C*
020000010618     C                   if        flgGiro = '1'
020100010601     C                   exsr      endela
020200010618     C                   endif
020300010330     C*
020400010330     C                   ENDSR
020500010330     C***
020600010330
020700010601
020800010601
020900010601
021000010601
021100010330     C*----------------------------------------------------*
021200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021300010330     C*----------------------------------------------------*
021400010330     C     INZVAR        BEGSR
021500010330     C*
021600020204     C                   Z-ADD     *zeros        Num5_0            5 0
021700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021800020322     C                   MOVEL     '0'           FlgCAS            1
021900010330     C*
022000010330     C                   ENDSR
022100010330     C*----------------------------------------------------*
022200020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022300010330     C*----------------------------------------------------*
022400010330     C     DEFCAM        BEGSR
022500010330     C*
022600020204     C* Inizializzo il buffer del record da scrivere
022700030130     C                   CLEAR                   FIVAB000
022800020204     C* Imposto i valori di default...
022900030327     C                   EVAL      VABCCM = 0492057
023000030327     C                   EVAL      VABLNP = 049
023100020322     C                   EVAL      VABCTR = 000
023200030515     C                   EVAL      VABCTM = '7Q'
023300020204     C* ... e poi verifico se sono stati passati come parametri
023400020204     C                   IF        vlrppt > *blanks
023500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023600020204     C                   EXSR      CHKNUM
023700020204     C                   IF        PiInt=*on
023800020204     C                   Z-ADD     PiVal         VABCCM
023900020204     C                   ENDIF
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300020204     C                   Z-ADD     PiVal         VABLNP
024400020204     C                   ENDIF
024500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020204     C                   EXSR      CHKNUM
024700020204     C                   IF        PiInt=*on
024800020204     C                   Z-ADD     PiVal         VABCTR
024900020204     C                   ENDIF
025000020204     C                   ENDIF
025100020204     C*
025200010330     C                   ENDSR
025300010607     C*----------------------------------------------------*
025400030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025500010607     C*----------------------------------------------------*
025600010607     C     IMPVAB        BEGSR
025700010607     C*
025800010607     C                   SETOFF                                       3132
025900010607     C*
026000010607     C* Reperimento campi ALFA
026100010607     C*
026200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026300030327     C                   EVAL      VABRSD=%trim(%subst(vindta:126:30))
026400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026600020117     C* ==
026700041222     C                   EVAL      VABIND=%trim(%subst(vindta:156:30))
026800041222     C                   EVAL      VABLOD=%trim(%subst(vindta:186:20))
026900041222     C                   EVAL      VABPRD=%trim(%subst(vindta:221:2))
027000030327     C                   EVAL      VABRMA='15376'
027100030327     C                   IF        %subst(vindta:11:2) = 'IT'
027200030327     C                   EVAL      VABNZD = *blanks
027300030327     C                   ELSE
027400030327     C                   EVAL      VABNZD = %trim(%subst(vindta:11:2))
027500030327     C                   ENDIF
027600030327     C
027700010607     C*
027800010607     C* Reperimento campi NUMERICI
027900030130     C                   MOVEL     datcor        VABAAS
028000030130     C                   MOVE      datcor        VABMGS
028100050923     C* RMN
028200041222     C                   EVAL      PiStr=%trim(%subst(vindta:270:8))
028300030130     C                   EXSR      CHKNUM
028400030130     C                   IF        PiInt=*on
028500030130     C                   Z-ADD     PiVal         VABRMN
028600030130     C                   ELSE
028700030130     C                   SETON                                        31
028800030130     C                   Z-ADD     *zeros        VABRMN
028900030130     C                   EVAL      vinmsg = %trimr(vinmsg)
029000050923     C                             + ' ' + 'VABRMN'
029100030130     C                   ENDIF
029200051018     C* NSP
029300050923     C                   EVAL      PiStr=%trim(%subst(vindta:14:7))
029400050923     C                   EXSR      CHKNUM
029500050923     C                   IF        PiInt=*on
029600050923     C                   Z-ADD     PiVal         VABNSP
029700050923     C                   ELSE
029800050923     C                   SETON                                        31
029900050923     C                   Z-ADD     *zeros        VABNSP
030000050923     C                   EVAL      vinmsg = %trimr(vinmsg)
030100050923     C                             + ' ' + 'VABNSP'
030200050923     C                   ENDIF
030300030130     C* CAD
030400041222     C                   EVAL      PiStr=%trim(%subst(vindta:216:5))
030500010607     C                   EXSR      CHKNUM
030600010607     C                   IF        PiInt=*on
030700020204     C                   Z-ADD     PiVal         Num5_0
030800020204     C                   MOVEL(p)  Num5_0        VABCAD
030900010607     C                   ELSE
031000010607     C                   SETON                                        32
031100020204     C                   EVAL      VABCAD = *zeros
031200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031300020204     C                             + ' ' + 'VABCAD'
031400010607     C                   ENDIF
031500030130     C* NCL
031600041222     C                   EVAL      PiStr=%trim(%subst(vindta:223:4))
031700010607     C                   EXSR      CHKNUM
031800010607     C                   IF        PiInt=*on
031900010607     C                   Z-ADD     PiVal         VABNCL
032000010607     C                   ELSE
032100010607     C                   SETON                                        32
032200010607     C                   Z-ADD     *zeros        VABNCL
032300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032400010607     C                             + ' ' + 'VABNCL'
032500010607     C                   ENDIF
032600030130     C* PKB
032700041222     C                   EVAL      PiStr=%trim(%subst(vindta:227:6))
032800030130     C                   EXSR      CHKNUM
032900030210     C                   IF        PiNum=*on
033000030130     C                   Z-ADD     PiVal         VABPKB
033100030130     C                   ELSE
033200030130     C                   SETON                                        32
033300030130     C                   Z-ADD     *zeros        VABPKB
033400030130     C                   EVAL      vinmsg = %trimr(vinmsg)
033500030130     C                             + ' ' + 'VABPKB'
033600030130     C                   ENDIF
033700020322     C*
033800020322     C* Considerazioni finali su CBO/CAS
033900020322     C                   IF        FlgCAS = '1'
034000020322     C                   EVAL      VABCBO = '4'
034100020322     C                   ELSE
034200020322     C                   EVAL      VABCBO = '1'
034300020322     C                   ENDIF
034400020204     C*
034500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034600020204     C                   EXSR      CHKIMPDIV
034700020204     C*
034800010607     C                   ENDSR
034900010607     C*----------------------------------------------------*
035000010601
035100020204
035200020204     C*----------------------------------------------------*
035300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
035400020204     C*----------------------------------------------------*
035500020204     C     CHKIMPDIV     BEGSR
035600020204     C*
035700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
035800020204     C                   Z-ADD     *zeros        wrkDec            9 9
035900020204     C*
036000020204     C* Come prima cosa effettuo considerazioni sulla divisa
036100020204     C                   IF        vabIAS > *zeros
036200020204     C                   IF        vabVAS <> 'EUR'
036300020204     C                   EVAL      vabVAS =  'ITL'
036400020204     C                   ENDIF
036500020204     C                   ENDIF
036600020204     C*
036700020204     C                   IF        vabCAS > *zeros
036800020204     C                   IF        vabVCA <> 'EUR'
036900020204     C                   EVAL      vabVCA =  'ITL'
037000020204     C                   ENDIF
037100020204     C                   ENDIF
037200020204     C*
037300020204     C                   IF        vabVMD > *zeros
037400020321     C                   IF        vabVAD <> 'EUR'
037500020204     C                   EVAL      vabVAD =  'ITL'
037600020204     C                   ENDIF
037700020204     C                   ENDIF
037800020204     C*
037900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
038000020204     C                   Z-ADD     vabIAS        wrkDec
038100020204     C                   IF        wrkDec > *zeros
038200020204     C                   IF        vabVAS = 'ITL'
038300020204     C                   EVAL      vabIAS = *zeros
038400020204     C                   ENDIF
038500020204     C                   ENDIF
038600020204     C*
038700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
038800020204     C                   Z-ADD     vabCAS        wrkDec
038900020204     C                   IF        wrkDec > *zeros
039000020204     C                   IF        vabVCA = 'ITL'
039100020204     C                   EVAL      vabCAS = *zeros
039200020204     C                   ENDIF
039300020204     C                   ENDIF
039400020204     C*
039500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
039600020204     C                   Z-ADD     vabVMD        wrkDec
039700020204     C                   IF        wrkDec > *zeros
039800020204     C                   IF        vabVAD = 'ITL'
039900020204     C                   EVAL      vabVMD = *zeros
040000020204     C                   ENDIF
040100020204     C                   ENDIF
040200020204     C*
040300020204     C                   ENDSR
040400020204     C***
040500020204
040600010330
040700010330
040800010330     C*----------------------------------------------------*
040900010330     C*  CONTROLLO NUMERICITA' CAMPI
041000010330     C*----------------------------------------------------*
041100010330     C     CHKNUM        BEGSR
041200010330     C*
041300010606     C                   IF        PiDecChr = *blanks
041400030130     C                   EVAL      PiDecChr = '.'
041500010606     C                   ENDIF
041600010606     C*
041700010606     C                   CALL(e)   'ISNUMERIC'
041800010330     C                   PARM                    PiStr            30
041900010606     C                   PARM                    PiDecChr          1
042000010330     C                   PARM      *ZEROS        PiVal            30 9
042100010330     C                   PARM      '0'           PiInt             1
042200010330     C                   PARM      '0'           PiNum             1
042300010330     C                   IF        %error
042400010606     C                   EVAL      PiNum=*off
042500010330     C                   ENDIF
042600010330     C*
042700010330     C                   ENDSR
042800010330     C***
042900010330
043000010601
043100010601
043200010601
043300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043400010601     C     repfil        BEGSR
043500010601     C*
043600010601     C                   if        invfil = *zeros and
043700010601     C                             depfil > *zeros and
043800010629     C                             (vinflg = *blanks or
043900010629     C                              vinflg = *zeros)
044000010601     C
044100010601     C                   eval      invfil = depfil
044200010601     C                   endif
044300010601     C*
044400010601     C                   if        depfil <> invfil and
044500010601     C                             invfil > *zeros
044600010601     C                   eval      flgMulti = '1'
044700010601     C                   if        vinflg = *blanks
044800010601     C                   add       1             cntNonEl
044900010601     C                   endif
045000010601     C                   endif
045100010601     C*
045200010601     C                   if        vinflg = '2'
045300010601     C                   eval      flgStato = '2'
045400010601     C                   endif
045500010601     C*
045600010601     C                   ENDSR
045700010601     C***
045800010601
045900010601
046000010601
046100010330
046200010330
046300010330
046400990920      /TITLE Invio dei dati al punto operativo.
046500000613     C     invio         BEGSR
046600990920     C*
046700990920     C                   reset                   dscmz
046800010601     C                   move      invfil        cmzdst
046900990920     C                   eval      cmzfld = vlrfou
047000990920     C                   eval      cmzmbd = vlrhdl
047100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
047200000710     C                   if        prmfir = *blanks
047300030130     C                   eval      cmzfla = 'FIVAB00F'
047400030130     C                   eval      cmzmba = 'FIVAB00F'
047500000710     C                   else
047600000710     C                   eval      cmzfla = prmfir
047700000710     C                   eval      cmzmba = prmfir
047800000710     C                   endif
047900990920     C                   eval      cmznrr = *zeros
048000990920     C                   move      §ctrok        cmznrr
048100021018     C                   eval      cmzlba = vlrfl1
048200990920     C                   call(e)   'TIS711C'
048300990920     C                   parm                    dscmz
048400990921     C                   parm      *blanks       esito
048500990920     C                   if        %error
048600990920     C                             or cmzerr = '1'
048700990921     C                             or esito  = '1'
048800000710     C                   eval      wrkesito = '3'
048900990920     C                   endif
049000990920     C*
049100000613     C                   ENDSR
049200990910
049300010601
049400010601
049500010601
049600010601
049700010601      /TITLE Invio dei dati al punto operativo.
049800010601     C     opeini        BEGSR
049900010601     C*
050000010601     C* Inizializzo flag e contatori operativi
050100010601     C                   movel     '0'           flgGiro           1
050200010601     C                   movel     '0'           flgMulti          1
050300010601     C                   movel     '1'           flgStato          1
050400010615     C                   movel     '0'           flgOk             1
050500010601     C                   z-add     *zeros        cntNonEl         10 0
050600010601     C                   z-add     *zeros        depfil            3 0
050700010601     C                   z-add     *zeros        invfil            3 0
050800010601     C*
050900010601     C                   ENDSR
051000010601     C***
051100010601
051200010601
051300010601
051400010330
051500010330
051600000613     C     *inzsr        BEGSR
051700990910     C*
051800990910     C     *entry        plist
051900990920     C                   parm                    tivlrds
052000990921     C                   parm      wrkesito      esito
052100000724     C                   parm                    prmlit
052200000710     C                   parm                    prmfir
052300010330     C*
052400010330     C* CALCOLA LA DATA CORRENTE
052500010330     C                   time                    wn14             14 0
052600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
052700010330     C                   z-add     wn8           g08dat
052800010330     C                   z-add     *zeros        g08inv
052900010330     C                   movel     '0'           g08err
053000010330     C                   call      'XSRDA8'
053100010330     C                   parm                    wlbda8
053200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
053300000613     C*
053400000613     C                   ENDSR
053500000613     C***
