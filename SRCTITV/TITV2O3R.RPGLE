000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400070502     D tisi95ds      e ds
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800010330     D wrkesito        s                   like(esito)
001900010330     D rrnum           s              6  0 INZ(*zeros)
002000010330
002100010330
002200000913     C                   reset                   rrnum
002300990921     C                   reset                   esito
002400990921     C                   reset                   wrkesito
002500010601     C*
002600010601     C                   exsr      opeini
002700010605     C                   exsr      rwvab
002800070502     C*
002900070502     C* Effettuo lancio TISI95 solo x chiusura
003000070502     C                   CLEAR                   TISI95DS
003100070502     C                   EVAL      I95TLA = 'C'
003200070502     C                   CALL      'TISI95R'
003300070502     C                   PARM                    TISI95DS
003400010601     C*
003500010601     C                   seton                                        lr
003600010601
003700010601
003800010601
003900010601
004000010601     C*--------------------------------------------------------
004100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004200010601     C*--------------------------------------------------------
004300010601     C     PREELA        BEGSR
004400010601     C*
004500010601     C* SE OCCORRE SPEDIRE IN FILIALE
004600010601     C                   if        invfil <> *zeros and
004700010601     C                             flgGiro = '0'
004800010601     C*
004900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005000010601     C                   eval      flgGiro = '1'
005100010601     C*
005200010601     C                   endif
005300010601     C*
005400010601     C                   ENDSR
005500010601     C***
005600010601
005700010601
005800010601
005900010601     C*--------------------------------------------------------
006000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006100010601     C*--------------------------------------------------------
006200010601     C     ENDELA        BEGSR
006300000616     C*
006400010601     C                   ENDSR
006500010601     C***
006600000613
006700010601
006800010601
006900010330     C*--------------------------------------------------------
007000050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
007100010330     C*--------------------------------------------------------
007200010605     C     RWVAB         BEGSR
007300050408     C*
007400010330     C                   if        not %open(tivin00r)
007500010330     C                   open      tivin00r
007600010330     C                   endif
007700050408     C                   if        not %open(fivabwwr)
007800050408     C                   open      fivabwwr
007900010330     C                   endif
008000010330     C*
008100010604     C                   clear                   §CTROK
008200010604     C                   clear                   §CTRMO
008300010604     C                   clear                   §CTRNO
008400010330     C*
008500010330     C                   DO        *HIVAL
008600010330     C*
008700010330     C                   READ      tivin00r                               70
008800010618     C*
008900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009000010618     C                   if        vinflg = '1'
009100010618     C                   eval      flgOk = '1'
009200010618     C                   endif
009300010618     C*
009400010330     C                   if        vindta > *blanks
009500010330     C                   add       1             rrnum
009600010330     C*
009700010601     C                   if        *in70 = *off and
009800010330     C                             (vinflg = *blanks
009900010330     C                              or vinflg = '0'
010000010330     C                              or vinflg = '2')
010100010330     C*
010200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010300010711     C                   if        vinflg = *blanks or vinflg = '0'
010400010711     C                   clear                   vinmsg
010500010711     C                   endif
010600070601     C*
010700070601     C                   exsr      inzvar
010800070601     C                   exsr      defcam
010900070601     C                   exsr      impvab
011000010601     C*
011100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011200010605     C                   eval      depfil = VABLNP
011300010601     C                   exsr      repfil
011400010601     C                   if        depfil = invfil
011500021025     C                   if        vlrpoi = 999
011600021025     C                   MOVE(P)   invfil        VABFGS
011700021025     C                   else
011800021025     C                   MOVE(P)   vlrpoi        VABFGS
011900021025     C                   endif
012000010601     C*
012100010601     C                   exsr      PREELA
012200010601     C*
012300010604     C* Ebbene...
012400010604     C*
012500010604     C  N31              ADD       1             §CTROK            7 0
012600010604     C   32              ADD       1             §CTRMO            7 0
012700010604     C   31              ADD       1             §CTRNO            7 0
012800010604     C*
012900071120     C                   exsr      wrivab                                       => scrivo VAB
013000071120     C*
013100020722     C                   endif
013200020722     C*
013300010604     C                   if        *in31 = *off and
013400010604     C                             *in32 = *off
013500010604     C                   eval      vinflg = '1'
013600010604     C                   else
013700010604     C                   eval      vinflg = '2'
013800010604     C                   endif
013900010604     C                   endif
014000010604     C*
014100010330     C                   else
014200010330     C                   eval      vinflg = '1'
014300010330     C                   endif
014400010601     C*
014500010601     C  N70              update    tivin000
014600010330     C*
014700010330     C  N70              ENDdo
014800010601     C*
014900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015000010601     C                   if        cntNonEl = *zeros or
015100010601     C                             flgMulti = '0'
015200010330     C* Se non ci sono record con errori ...
015300010601     C                   if        §ctrno = 0 and
015400010604     C                             §ctrmo = 0 and
015500010601     C                             flgStato <> '2'
015600010330     C* ... restituisco esito OK.
015700010330     C                   eval      wrkesito = '0'
015800010330     C                   else
015900010330     C                   if        §ctrok > 0
016000010330     C                   eval      wrkesito = '1'
016100010330     C                   else
016200010615     C                   if        flgOk = '0'
016300010615     C                   eval      wrkesito = '2'
016400010615     C                   else
016500010615     C                   eval      wrkesito = '6'
016600010615     C                   endif
016700010330     C                   endif
016800010330     C                   endif
016900010601     C                   else
017000010601     C                   eval      wrkesito = '9'
017100010601     C                   endif
017200010330     C*
017300010330     C                   if        %open(tivin00r)
017400010330     C                   close     tivin00r
017500010330     C                   endif
017600050408     C                   if        %open(fivabwwr)
017700050408     C                   close     fivabwwr
017800010330     C                   endif
017900010601     C*
018000010601     C                   if        vlrpoi <> 999
018100010601     C                   eval      invfil = vlrpoi
018200010601     C                   endif
018300010330     C*
018400010330     C                   if        §ctrok > 0
018500010601     C                             and invfil > *zeros
018600010330     C                   exsr      invio
018700010330     C                   endif
018800010601     C*
018900010618     C                   if        flgGiro = '1'
019000010601     C                   exsr      endela
019100010618     C                   endif
019200010330     C*
019300010330     C                   ENDSR
019400010330     C***
019500070601
019600070601     C*----------------------------------------------------*
019700070601     C*  SCARICAMENTO BUFFER RECORDS VAB
019800070601     C*----------------------------------------------------*
019900070601     C     WRIVAB        BEGSR
020000070601     C*
020100070601     C  N31              WRITE     FIVAB000
020200070601     C*
020300070601     C                   ENDSR
020400010601
020500010601
020600010601
020700010330     C*----------------------------------------------------*
020800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900010330     C*----------------------------------------------------*
021000010330     C     INZVAR        BEGSR
021100010330     C*
021200020204     C                   Z-ADD     *zeros        Num5_0            5 0
021300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021400020322     C                   MOVEL     '0'           FlgCAS            1
021500010330     C*
021600010330     C                   ENDSR
021700010330     C*----------------------------------------------------*
021800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021900010330     C*----------------------------------------------------*
022000010330     C     DEFCAM        BEGSR
022100010330     C*
022200020204     C* Inizializzo il buffer del record da scrivere
022300050408     C                   CLEAR                   FIVAB000
022400020204     C* Imposto i valori di default...
022500130304     C                   EVAL      VABCCM = 2230097
022600130304     C                   EVAL      VABLNP = 223
022700130304     C                   EVAL      VABCTR = 003
022800070208     C                   EVAL      VABCBO = '1'
022900020204     C* ... e poi verifico se sono stati passati come parametri
023000020204     C                   IF        vlrppt > *blanks
023100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023300020204     C                   EXSR      CHKNUM
023400020204     C                   IF        PiInt=*on
023500020204     C                   Z-ADD     PiVal         VABCCM
023600020204     C                   ENDIF
023700040301     C                   ENDIF
023800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024000020204     C                   EXSR      CHKNUM
024100020204     C                   IF        PiInt=*on
024200020204     C                   Z-ADD     PiVal         VABLNP
024300020204     C                   ENDIF
024400040301     C                   ENDIF
024500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024700020204     C                   EXSR      CHKNUM
024800020204     C                   IF        PiInt=*on
024900020204     C                   Z-ADD     PiVal         VABCTR
025000040301     C                   ENDIF
025100020204     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C*
025400010330     C                   ENDSR
025500010607     C*----------------------------------------------------*
025600050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025700010607     C*----------------------------------------------------*
025800010607     C     IMPVAB        BEGSR
025900010607     C*
026000010607     C                   SETOFF                                       3132
026100010607     C*
026200010607     C* Reperimento campi ALFA
026300010607     C*
026400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026500070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
026600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026800020117     C* ==
026900070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
027000070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
027100070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
027200070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
027300060510     C*
027400060510     C* Reperimento campi NUMERICI
027500060510     C                   MOVEL     DATCOR        VABAAS
027600060510     C                   MOVE      DATCOR        VABMGS
027700060710     C* NSP/RMN
027800130305     C                   EVAL      PiStr=%trim(%subst(vindta:41:6))
027900060510     C                   EXSR      CHKNUM
028000060510     C                   IF        PiInt=*on
028100060510     C                   Z-ADD     PiVal         VABNSP
028200060510     C                   Z-ADD     PiVal         VABRMN
028300060404     C                   ELSE
028400070502     C                   SETON                                        31
028500060510     C                   Z-ADD     *zeros        VABNSP
028600060510     C                   Z-ADD     1             VABRMN
028700060404     C                   EVAL      vinmsg = %trimr(vinmsg)
028800060510     C                             + ' ' + 'VABNSP VABRMN'
028900060404     C                   ENDIF
029000040420     C* CAD
029100070925     C                   EVAL      PiStr=%trim(%subst(vindta:145:5))
029200010607     C                   EXSR      CHKNUM
029300010607     C                   IF        PiInt=*on
029400020204     C                   Z-ADD     PiVal         Num5_0
029500020204     C                   MOVEL(p)  Num5_0        VABCAD
029600010607     C                   ELSE
029700010607     C                   SETON                                        32
029800020204     C                   EVAL      VABCAD = *zeros
029900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
030000020204     C                             + ' ' + 'VABCAD'
030100060316     C                   ENDIF
030200070502     C* Reperisco la provincia dal CAP e dalla località
030300070502     C                   IF        VABCAD <> *blanks AND
030400070502     C                             VABPRD  = *blanks
030500070502     C                   CLEAR                   TISI95DS
030600070502     C                   EVAL      I95TCN = '3'
030700070502     C                   Z-ADD     datcor        I95DAT
030800070502     C                   EVAL      I95CAP = VABCAD
030900070502     C                   EVAL      I95LOC = VABLOD
031000070502     C                   EVAL      I95NAR = VABNZD
031100070502     C                   CALL      'TISI95R'
031200070502     C                   PARM                    TISI95DS
031300070502     C                   EVAL      VABPRD = O95PRV
031400070502     C                   ENDIF
031500040420     C* NCL
031600070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
031700010607     C                   EXSR      CHKNUM
031800010607     C                   IF        PiInt=*on
031900070606     C                   Z-ADD     PiVal         VABNCL
032000010607     C                   ELSE
032100010607     C                   SETON                                        32
032200010607     C                   Z-ADD     *zeros        VABNCL
032300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032400010607     C                             + ' ' + 'VABNCL'
032500010607     C                   ENDIF
032600040420     C* PKB
032700070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
032800070208     C                   EVAL      PiDecChr = '.'
032900010607     C                   EXSR      CHKNUM
033000010607     C                   IF        PiNum=*on
033100070925     C                   Z-ADD(H)  PiVal         VABPKB
033200010607     C                   ELSE
033300010607     C                   SETON                                        32
033400010607     C                   Z-ADD     *zeros        VABPKB
033500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033600010607     C                             + ' ' + 'VABPKB'
033700010607     C                   ENDIF
033800070925     C* VLB
033900070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
034000070925     C                   EVAL      PiDecChr = '.'
034100070925     C                   EXSR      CHKNUM
034200070925     C                   IF        PiNum=*on
034300070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
034400070925     C                   Z-ADD     PiVal         VABVLB
034500070925     C                   ELSE
034600070925     C                   SETON                                        32
034700070925     C                   Z-ADD     *zeros        VABVLB
034800070925     C                   EVAL      vinmsg = %trimr(vinmsg)
034900070925     C                             + ' ' + 'VABVLB'
035000070925     C                   ENDIF
035100020322     C*
035200020322     C* Considerazioni finali su CBO/CAS
035300050908     C                   IF        FlgCAS = '1'
035400040713     C                   IF        VABCBO = '1'
035500020322     C                   EVAL      VABCBO = '4'
035600040713     C                   ENDIF
035700040713     C                   IF        VABCBO = '2'
035800040713     C                   EVAL      VABCBO = '6'
035900040713     C                   ENDIF
036000020322     C                   ENDIF
036100020204     C*
036200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036300020204     C                   EXSR      CHKIMPDIV
036400020204     C*
036500010607     C                   ENDSR
036600010607     C*----------------------------------------------------*
036700010601
036800020204
036900020204     C*----------------------------------------------------*
037000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
037100020204     C*----------------------------------------------------*
037200020204     C     CHKIMPDIV     BEGSR
037300020204     C*
037400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
037500020204     C                   Z-ADD     *zeros        wrkDec            9 9
037600020204     C*
037700020204     C* Come prima cosa effettuo considerazioni sulla divisa
037800020204     C                   IF        vabIAS > *zeros
037900020204     C                   IF        vabVAS <> 'EUR'
038000020204     C                   EVAL      vabVAS =  'ITL'
038100020204     C                   ENDIF
038200020204     C                   ENDIF
038300020204     C*
038400020204     C                   IF        vabCAS > *zeros
038500020204     C                   IF        vabVCA <> 'EUR'
038600020204     C                   EVAL      vabVCA =  'ITL'
038700020204     C                   ENDIF
038800020204     C                   ENDIF
038900020204     C*
039000020204     C                   IF        vabVMD > *zeros
039100020321     C                   IF        vabVAD <> 'EUR'
039200020204     C                   EVAL      vabVAD =  'ITL'
039300020204     C                   ENDIF
039400020204     C                   ENDIF
039500020204     C*
039600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
039700020204     C                   Z-ADD     vabIAS        wrkDec
039800020204     C                   IF        wrkDec > *zeros
039900020204     C                   IF        vabVAS = 'ITL'
040000020204     C                   EVAL      vabIAS = *zeros
040100020204     C                   ENDIF
040200020204     C                   ENDIF
040300020204     C*
040400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
040500020204     C                   Z-ADD     vabCAS        wrkDec
040600020204     C                   IF        wrkDec > *zeros
040700020204     C                   IF        vabVCA = 'ITL'
040800020204     C                   EVAL      vabCAS = *zeros
040900020204     C                   ENDIF
041000020204     C                   ENDIF
041100020204     C*
041200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
041300020204     C                   Z-ADD     vabVMD        wrkDec
041400020204     C                   IF        wrkDec > *zeros
041500020204     C                   IF        vabVAD = 'ITL'
041600020204     C                   EVAL      vabVMD = *zeros
041700020204     C                   ENDIF
041800020204     C                   ENDIF
041900020204     C*
042000020204     C                   ENDSR
042100020204     C***
042200020204
042300010330
042400010330
042500010330     C*----------------------------------------------------*
042600010330     C*  CONTROLLO NUMERICITA' CAMPI
042700010330     C*----------------------------------------------------*
042800010330     C     CHKNUM        BEGSR
042900010330     C*
043000010606     C                   IF        PiDecChr = *blanks
043100070502     C                   EVAL      PiDecChr = '.'
043200010606     C                   ENDIF
043300010606     C*
043400010606     C                   CALL(e)   'ISNUMERIC'
043500010330     C                   PARM                    PiStr            30
043600010606     C                   PARM                    PiDecChr          1
043700010330     C                   PARM      *ZEROS        PiVal            30 9
043800010330     C                   PARM      '0'           PiInt             1
043900010330     C                   PARM      '0'           PiNum             1
044000010330     C                   IF        %error
044100010606     C                   EVAL      PiNum=*off
044200010330     C                   ENDIF
044300010330     C*
044400010330     C                   ENDSR
044500010330     C***
044600010330
044700010601
044800010601
044900010601
045000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045100010601     C     repfil        BEGSR
045200010601     C*
045300010601     C                   if        invfil = *zeros and
045400010601     C                             depfil > *zeros and
045500010629     C                             (vinflg = *blanks or
045600010629     C                              vinflg = *zeros)
045700010601     C
045800010601     C                   eval      invfil = depfil
045900010601     C                   endif
046000010601     C*
046100010601     C                   if        depfil <> invfil and
046200010601     C                             invfil > *zeros
046300010601     C                   eval      flgMulti = '1'
046400010601     C                   if        vinflg = *blanks
046500010601     C                   add       1             cntNonEl
046600010601     C                   endif
046700010601     C                   endif
046800010601     C*
046900010601     C                   if        vinflg = '2'
047000010601     C                   eval      flgStato = '2'
047100010601     C                   endif
047200010601     C*
047300010601     C                   ENDSR
047400010601     C***
047500010601
047600010601
047700010601
047800010330
047900010330
048000010330
048100990920      /TITLE Invio dei dati al punto operativo.
048200000613     C     invio         BEGSR
048300990920     C*
048400990920     C                   reset                   dscmz
048500010601     C                   move      invfil        cmzdst
048600990920     C                   eval      cmzfld = vlrfou
048700990920     C                   eval      cmzmbd = vlrhdl
048800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048900000710     C                   if        prmfir = *blanks
049000050408     C                   eval      cmzfla = 'FIVAB00F'
049100050408     C                   eval      cmzmba = 'FIVAB00F'
049200000710     C                   else
049300000710     C                   eval      cmzfla = prmfir
049400000710     C                   eval      cmzmba = prmfir
049500000710     C                   endif
049600990920     C                   eval      cmznrr = *zeros
049700990920     C                   move      §ctrok        cmznrr
049800021018     C                   eval      cmzlba = vlrfl1
049900990920     C                   call(e)   'TIS711C'
050000990920     C                   parm                    dscmz
050100990921     C                   parm      *blanks       esito
050200990920     C                   if        %error
050300990920     C                             or cmzerr = '1'
050400990921     C                             or esito  = '1'
050500000710     C                   eval      wrkesito = '3'
050600990920     C                   endif
050700990920     C*
050800000613     C                   ENDSR
050900990910
051000010601
051100010601
051200010601
051300010601
051400010601      /TITLE Invio dei dati al punto operativo.
051500010601     C     opeini        BEGSR
051600010601     C*
051700010601     C* Inizializzo flag e contatori operativi
051800010601     C                   movel     '0'           flgGiro           1
051900010601     C                   movel     '0'           flgMulti          1
052000010601     C                   movel     '1'           flgStato          1
052100010615     C                   movel     '0'           flgOk             1
052200010601     C                   z-add     *zeros        cntNonEl         10 0
052300010601     C                   z-add     *zeros        depfil            3 0
052400010601     C                   z-add     *zeros        invfil            3 0
052500010601     C*
052600010601     C                   ENDSR
052700010601     C***
052800010601
052900010601
053000010601
053100010330
053200010330
053300000613     C     *inzsr        BEGSR
053400990910     C*
053500990910     C     *entry        plist
053600990920     C                   parm                    tivlrds
053700990921     C                   parm      wrkesito      esito
053800000724     C                   parm                    prmlit
053900000710     C                   parm                    prmfir
054000010330     C*
054100010330     C* CALCOLA LA DATA CORRENTE
054200130304     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
054300130304     C                   eval      datcor = %dec(%date() : *ISO)
054400000613     C*
054500000613     C                   ENDSR
054600000613     C***
