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
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800010330
002900010330
003000000913     C                   reset                   rrnum
003100990921     C                   reset                   esito
003200990921     C                   reset                   wrkesito
003300010601     C*
003400010601     C                   exsr      opeini
003500010605     C                   exsr      rwvab
003600070502     C*
003700070502     C* Effettuo lancio TISI95 solo x chiusura
003800070502     C                   CLEAR                   TISI95DS
003900070502     C                   EVAL      I95TLA = 'C'
004000070502     C                   CALL      'TISI95R'
004100070502     C                   PARM                    TISI95DS
004200010601     C*
004300010601     C                   seton                                        lr
004400010601
004500010601
004600010601
004700010601
004800010601     C*--------------------------------------------------------
004900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005000010601     C*--------------------------------------------------------
005100010601     C     PREELA        BEGSR
005200010601     C*
005300010601     C* SE OCCORRE SPEDIRE IN FILIALE
005400010601     C                   if        invfil <> *zeros and
005500010601     C                             flgGiro = '0'
005600010601     C*
005700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005800010601     C                   eval      flgGiro = '1'
005900010601     C*
006000010601     C                   endif
006100010601     C*
006200010601     C                   ENDSR
006300010601     C***
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006900010601     C*--------------------------------------------------------
007000010601     C     ENDELA        BEGSR
007100000616     C*
007200010601     C                   ENDSR
007300010601     C***
007400000613
007500010601
007600010601
007700010330     C*--------------------------------------------------------
007800050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
007900010330     C*--------------------------------------------------------
008000010605     C     RWVAB         BEGSR
008100050408     C*
008200010330     C                   if        not %open(tivin00r)
008300010330     C                   open      tivin00r
008400010330     C                   endif
008500050408     C                   if        not %open(fivabwwr)
008600050408     C                   open      fivabwwr
008700010330     C                   endif
008800010330     C*
008900010604     C                   clear                   §CTROK
009000010604     C                   clear                   §CTRMO
009100010604     C                   clear                   §CTRNO
009200010330     C*
009300010330     C                   DO        *HIVAL
009400010330     C*
009500010330     C                   READ      tivin00r                               70
009600010618     C*
009700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009800010618     C                   if        vinflg = '1'
009900010618     C                   eval      flgOk = '1'
010000010618     C                   endif
010100010618     C*
010200010330     C                   if        vindta > *blanks
010300010330     C                   add       1             rrnum
010400010330     C*
010500010601     C                   if        *in70 = *off and
010600010330     C                             (vinflg = *blanks
010700010330     C                              or vinflg = '0'
010800010330     C                              or vinflg = '2')
010900010330     C*
011000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011100010711     C                   if        vinflg = *blanks or vinflg = '0'
011200010711     C                   clear                   vinmsg
011300010711     C                   endif
011400070601     C*
011500070601     C                   exsr      inzvar
011600070601     C                   exsr      defcam
011700070601     C                   exsr      impvab
011800010601     C*
011900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012000010605     C                   eval      depfil = VABLNP
012100010601     C                   exsr      repfil
012200010601     C                   if        depfil = invfil
012300021025     C                   if        vlrpoi = 999
012400021025     C                   MOVE(P)   invfil        VABFGS
012500021025     C                   else
012600021025     C                   MOVE(P)   vlrpoi        VABFGS
012700021025     C                   endif
012800010601     C*
012900010601     C                   exsr      PREELA
013000010601     C*
013100010604     C* Ebbene...
013200010604     C*
013300010604     C  N31              ADD       1             §CTROK            7 0
013400010604     C   32              ADD       1             §CTRMO            7 0
013500010604     C   31              ADD       1             §CTRNO            7 0
013600010604     C*
013700071120     C                   exsr      wrivab                                       => scrivo VAB
013800071120     C*
013900020722     C                   endif
014000020722     C*
014100010604     C                   if        *in31 = *off and
014200010604     C                             *in32 = *off
014300010604     C                   eval      vinflg = '1'
014400010604     C                   else
014500010604     C                   eval      vinflg = '2'
014600010604     C                   endif
014700010604     C                   endif
014800010604     C*
014900010330     C                   else
015000010330     C                   eval      vinflg = '1'
015100010330     C                   endif
015200010601     C*
015300010601     C  N70              update    tivin000
015400010330     C*
015500010330     C  N70              ENDdo
015600010601     C*
015700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015800010601     C                   if        cntNonEl = *zeros or
015900010601     C                             flgMulti = '0'
016000010330     C* Se non ci sono record con errori ...
016100010601     C                   if        §ctrno = 0 and
016200010604     C                             §ctrmo = 0 and
016300010601     C                             flgStato <> '2'
016400010330     C* ... restituisco esito OK.
016500010330     C                   eval      wrkesito = '0'
016600010330     C                   else
016700010330     C                   if        §ctrok > 0
016800010330     C                   eval      wrkesito = '1'
016900010330     C                   else
017000010615     C                   if        flgOk = '0'
017100010615     C                   eval      wrkesito = '2'
017200010615     C                   else
017300010615     C                   eval      wrkesito = '6'
017400010615     C                   endif
017500010330     C                   endif
017600010330     C                   endif
017700010601     C                   else
017800010601     C                   eval      wrkesito = '9'
017900010601     C                   endif
018000010330     C*
018100010330     C                   if        %open(tivin00r)
018200010330     C                   close     tivin00r
018300010330     C                   endif
018400050408     C                   if        %open(fivabwwr)
018500050408     C                   close     fivabwwr
018600010330     C                   endif
018700010601     C*
018800010601     C                   if        vlrpoi <> 999
018900010601     C                   eval      invfil = vlrpoi
019000010601     C                   endif
019100010330     C*
019200010330     C                   if        §ctrok > 0
019300010601     C                             and invfil > *zeros
019400010330     C                   exsr      invio
019500010330     C                   endif
019600010601     C*
019700010618     C                   if        flgGiro = '1'
019800010601     C                   exsr      endela
019900010618     C                   endif
020000010330     C*
020100010330     C                   ENDSR
020200010330     C***
020300070601
020400070601     C*----------------------------------------------------*
020500070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020600070601     C*----------------------------------------------------*
020700070601     C     WRIVAB        BEGSR
020800070601     C*
020900070601     C  N31              WRITE     FIVAB000
021000070601     C*
021100070601     C                   ENDSR
021200010601
021300010601
021400010601
021500010330     C*----------------------------------------------------*
021600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021700010330     C*----------------------------------------------------*
021800010330     C     INZVAR        BEGSR
021900010330     C*
022000020204     C                   Z-ADD     *zeros        Num5_0            5 0
022100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022200020322     C                   MOVEL     '0'           FlgCAS            1
022300010330     C*
022400010330     C                   ENDSR
022500010330     C*----------------------------------------------------*
022600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022700010330     C*----------------------------------------------------*
022800010330     C     DEFCAM        BEGSR
022900010330     C*
023000020204     C* Inizializzo il buffer del record da scrivere
023100050408     C                   CLEAR                   FIVAB000
023200020204     C* Imposto i valori di default...
023300070925     C                   EVAL      VABCCM = 0432751
023400070925     C                   EVAL      VABLNP = 043
023500071121     C                   EVAL      VABCTR = 300
023600070208     C                   EVAL      VABCBO = '1'
023700020204     C* ... e poi verifico se sono stati passati come parametri
023800020204     C                   IF        vlrppt > *blanks
023900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300020204     C                   Z-ADD     PiVal         VABCCM
024400020204     C                   ENDIF
024500040301     C                   ENDIF
024600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024800020204     C                   EXSR      CHKNUM
024900020204     C                   IF        PiInt=*on
025000020204     C                   Z-ADD     PiVal         VABLNP
025100020204     C                   ENDIF
025200040301     C                   ENDIF
025300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025500020204     C                   EXSR      CHKNUM
025600020204     C                   IF        PiInt=*on
025700020204     C                   Z-ADD     PiVal         VABCTR
025800040301     C                   ENDIF
025900020204     C                   ENDIF
026000020204     C                   ENDIF
026100020204     C*
026200010330     C                   ENDSR
026300010607     C*----------------------------------------------------*
026400050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026500010607     C*----------------------------------------------------*
026600010607     C     IMPVAB        BEGSR
026700010607     C*
026800010607     C                   SETOFF                                       3132
026900010607     C*
027000010607     C* Reperimento campi ALFA
027100010607     C*
027200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027300070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
027400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027600020117     C* ==
027700070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
027800070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
027900070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
028000070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
028100060510     C*
028200060510     C* Reperimento campi NUMERICI
028300060510     C                   MOVEL     DATCOR        VABAAS
028400060510     C                   MOVE      DATCOR        VABMGS
028500060710     C* NSP/RMN
028600070925     C                   EVAL      PiStr=%trim(%subst(vindta:35:6))
028700060510     C                   EXSR      CHKNUM
028800060510     C                   IF        PiInt=*on
028900060510     C                   Z-ADD     PiVal         VABNSP
029000060510     C                   Z-ADD     PiVal         VABRMN
029100060404     C                   ELSE
029200070502     C                   SETON                                        31
029300060510     C                   Z-ADD     *zeros        VABNSP
029400060510     C                   Z-ADD     1             VABRMN
029500060404     C                   EVAL      vinmsg = %trimr(vinmsg)
029600060510     C                             + ' ' + 'VABNSP VABRMN'
029700060404     C                   ENDIF
029800040420     C* CAD
029900070925     C                   EVAL      PiStr=%trim(%subst(vindta:145:5))
030000010607     C                   EXSR      CHKNUM
030100010607     C                   IF        PiInt=*on
030200020204     C                   Z-ADD     PiVal         Num5_0
030300020204     C                   MOVEL(p)  Num5_0        VABCAD
030400010607     C                   ELSE
030500010607     C                   SETON                                        32
030600020204     C                   EVAL      VABCAD = *zeros
030700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
030800020204     C                             + ' ' + 'VABCAD'
030900060316     C                   ENDIF
031000070502     C* Reperisco la provincia dal CAP e dalla località
031100070502     C                   IF        VABCAD <> *blanks AND
031200070502     C                             VABPRD  = *blanks
031300070502     C                   CLEAR                   TISI95DS
031400070502     C                   EVAL      I95TCN = '3'
031500070502     C                   Z-ADD     datcor        I95DAT
031600070502     C                   EVAL      I95CAP = VABCAD
031700070502     C                   EVAL      I95LOC = VABLOD
031800070502     C                   EVAL      I95NAR = VABNZD
031900070502     C                   CALL      'TISI95R'
032000070502     C                   PARM                    TISI95DS
032100070502     C                   EVAL      VABPRD = O95PRV
032200070502     C                   ENDIF
032300040420     C* NCL
032400070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
032500010607     C                   EXSR      CHKNUM
032600010607     C                   IF        PiInt=*on
032700070606     C                   Z-ADD     PiVal         VABNCL
032800010607     C                   ELSE
032900010607     C                   SETON                                        32
033000010607     C                   Z-ADD     *zeros        VABNCL
033100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033200010607     C                             + ' ' + 'VABNCL'
033300010607     C                   ENDIF
033400040420     C* PKB
033500070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
033600070208     C                   EVAL      PiDecChr = '.'
033700010607     C                   EXSR      CHKNUM
033800010607     C                   IF        PiNum=*on
033900070925     C                   Z-ADD(H)  PiVal         VABPKB
034000010607     C                   ELSE
034100010607     C                   SETON                                        32
034200010607     C                   Z-ADD     *zeros        VABPKB
034300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034400010607     C                             + ' ' + 'VABPKB'
034500010607     C                   ENDIF
034600070925     C* VLB
034700070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
034800070925     C                   EVAL      PiDecChr = '.'
034900070925     C                   EXSR      CHKNUM
035000070925     C                   IF        PiNum=*on
035100070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
035200070925     C                   Z-ADD     PiVal         VABVLB
035300070925     C                   ELSE
035400070925     C                   SETON                                        32
035500070925     C                   Z-ADD     *zeros        VABVLB
035600070925     C                   EVAL      vinmsg = %trimr(vinmsg)
035700070925     C                             + ' ' + 'VABVLB'
035800070925     C                   ENDIF
035900020322     C*
036000020322     C* Considerazioni finali su CBO/CAS
036100050908     C                   IF        FlgCAS = '1'
036200040713     C                   IF        VABCBO = '1'
036300020322     C                   EVAL      VABCBO = '4'
036400040713     C                   ENDIF
036500040713     C                   IF        VABCBO = '2'
036600040713     C                   EVAL      VABCBO = '6'
036700040713     C                   ENDIF
036800020322     C                   ENDIF
036900020204     C*
037000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037100020204     C                   EXSR      CHKIMPDIV
037200020204     C*
037300010607     C                   ENDSR
037400010607     C*----------------------------------------------------*
037500010601
037600020204
037700020204     C*----------------------------------------------------*
037800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
037900020204     C*----------------------------------------------------*
038000020204     C     CHKIMPDIV     BEGSR
038100020204     C*
038200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
038300020204     C                   Z-ADD     *zeros        wrkDec            9 9
038400020204     C*
038500020204     C* Come prima cosa effettuo considerazioni sulla divisa
038600020204     C                   IF        vabIAS > *zeros
038700020204     C                   IF        vabVAS <> 'EUR'
038800020204     C                   EVAL      vabVAS =  'ITL'
038900020204     C                   ENDIF
039000020204     C                   ENDIF
039100020204     C*
039200020204     C                   IF        vabCAS > *zeros
039300020204     C                   IF        vabVCA <> 'EUR'
039400020204     C                   EVAL      vabVCA =  'ITL'
039500020204     C                   ENDIF
039600020204     C                   ENDIF
039700020204     C*
039800020204     C                   IF        vabVMD > *zeros
039900020321     C                   IF        vabVAD <> 'EUR'
040000020204     C                   EVAL      vabVAD =  'ITL'
040100020204     C                   ENDIF
040200020204     C                   ENDIF
040300020204     C*
040400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
040500020204     C                   Z-ADD     vabIAS        wrkDec
040600020204     C                   IF        wrkDec > *zeros
040700020204     C                   IF        vabVAS = 'ITL'
040800020204     C                   EVAL      vabIAS = *zeros
040900020204     C                   ENDIF
041000020204     C                   ENDIF
041100020204     C*
041200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
041300020204     C                   Z-ADD     vabCAS        wrkDec
041400020204     C                   IF        wrkDec > *zeros
041500020204     C                   IF        vabVCA = 'ITL'
041600020204     C                   EVAL      vabCAS = *zeros
041700020204     C                   ENDIF
041800020204     C                   ENDIF
041900020204     C*
042000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
042100020204     C                   Z-ADD     vabVMD        wrkDec
042200020204     C                   IF        wrkDec > *zeros
042300020204     C                   IF        vabVAD = 'ITL'
042400020204     C                   EVAL      vabVMD = *zeros
042500020204     C                   ENDIF
042600020204     C                   ENDIF
042700020204     C*
042800020204     C                   ENDSR
042900020204     C***
043000020204
043100010330
043200010330
043300010330     C*----------------------------------------------------*
043400010330     C*  CONTROLLO NUMERICITA' CAMPI
043500010330     C*----------------------------------------------------*
043600010330     C     CHKNUM        BEGSR
043700010330     C*
043800010606     C                   IF        PiDecChr = *blanks
043900070502     C                   EVAL      PiDecChr = '.'
044000010606     C                   ENDIF
044100010606     C*
044200010606     C                   CALL(e)   'ISNUMERIC'
044300010330     C                   PARM                    PiStr            30
044400010606     C                   PARM                    PiDecChr          1
044500010330     C                   PARM      *ZEROS        PiVal            30 9
044600010330     C                   PARM      '0'           PiInt             1
044700010330     C                   PARM      '0'           PiNum             1
044800010330     C                   IF        %error
044900010606     C                   EVAL      PiNum=*off
045000010330     C                   ENDIF
045100010330     C*
045200010330     C                   ENDSR
045300010330     C***
045400010330
045500010601
045600010601
045700010601
045800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045900010601     C     repfil        BEGSR
046000010601     C*
046100010601     C                   if        invfil = *zeros and
046200010601     C                             depfil > *zeros and
046300010629     C                             (vinflg = *blanks or
046400010629     C                              vinflg = *zeros)
046500010601     C
046600010601     C                   eval      invfil = depfil
046700010601     C                   endif
046800010601     C*
046900010601     C                   if        depfil <> invfil and
047000010601     C                             invfil > *zeros
047100010601     C                   eval      flgMulti = '1'
047200010601     C                   if        vinflg = *blanks
047300010601     C                   add       1             cntNonEl
047400010601     C                   endif
047500010601     C                   endif
047600010601     C*
047700010601     C                   if        vinflg = '2'
047800010601     C                   eval      flgStato = '2'
047900010601     C                   endif
048000010601     C*
048100010601     C                   ENDSR
048200010601     C***
048300010601
048400010601
048500010601
048600010330
048700010330
048800010330
048900990920      /TITLE Invio dei dati al punto operativo.
049000000613     C     invio         BEGSR
049100990920     C*
049200990920     C                   reset                   dscmz
049300010601     C                   move      invfil        cmzdst
049400990920     C                   eval      cmzfld = vlrfou
049500990920     C                   eval      cmzmbd = vlrhdl
049600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
049700000710     C                   if        prmfir = *blanks
049800050408     C                   eval      cmzfla = 'FIVAB00F'
049900050408     C                   eval      cmzmba = 'FIVAB00F'
050000000710     C                   else
050100000710     C                   eval      cmzfla = prmfir
050200000710     C                   eval      cmzmba = prmfir
050300000710     C                   endif
050400990920     C                   eval      cmznrr = *zeros
050500990920     C                   move      §ctrok        cmznrr
050600021018     C                   eval      cmzlba = vlrfl1
050700990920     C                   call(e)   'TIS711C'
050800990920     C                   parm                    dscmz
050900990921     C                   parm      *blanks       esito
051000990920     C                   if        %error
051100990920     C                             or cmzerr = '1'
051200990921     C                             or esito  = '1'
051300000710     C                   eval      wrkesito = '3'
051400990920     C                   endif
051500990920     C*
051600000613     C                   ENDSR
051700990910
051800010601
051900010601
052000010601
052100010601
052200010601      /TITLE Invio dei dati al punto operativo.
052300010601     C     opeini        BEGSR
052400010601     C*
052500010601     C* Inizializzo flag e contatori operativi
052600010601     C                   movel     '0'           flgGiro           1
052700010601     C                   movel     '0'           flgMulti          1
052800010601     C                   movel     '1'           flgStato          1
052900010615     C                   movel     '0'           flgOk             1
053000010601     C                   z-add     *zeros        cntNonEl         10 0
053100010601     C                   z-add     *zeros        depfil            3 0
053200010601     C                   z-add     *zeros        invfil            3 0
053300010601     C*
053400010601     C                   ENDSR
053500010601     C***
053600010601
053700010601
053800010601
053900010330
054000010330
054100000613     C     *inzsr        BEGSR
054200990910     C*
054300990910     C     *entry        plist
054400990920     C                   parm                    tivlrds
054500990921     C                   parm      wrkesito      esito
054600000724     C                   parm                    prmlit
054700000710     C                   parm                    prmfir
054800010330     C*
054900010330     C* CALCOLA LA DATA CORRENTE
055000010330     C                   time                    wn14             14 0
055100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055200010330     C                   z-add     wn8           g08dat
055300010330     C                   z-add     *zeros        g08inv
055400010330     C                   movel     '0'           g08err
055500010330     C                   call      'XSRDA8'
055600010330     C                   parm                    wlbda8
055700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055800000613     C*
055900000613     C                   ENDSR
056000000613     C***
