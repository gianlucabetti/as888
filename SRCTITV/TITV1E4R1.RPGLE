000100120224      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500120224     FEDIVABWR  O    E             DISK    usropn
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
007000120224     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
007100010330     C*--------------------------------------------------------
007200010605     C     RWVAB         BEGSR
007300050408     C*
007400010330     C                   if        not %open(tivin00r)
007500010330     C                   open      tivin00r
007600010330     C                   endif
007700120224     C                   if        not %open(edivabwr)
007800120224     C                   open      edivabwr
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
012800071003     C                   exsr      wrivab                                       => scrivo VAB
012900010604     C*
013000020722     C                   endif
013100020722     C*
013200010604     C                   if        *in31 = *off and
013300010604     C                             *in32 = *off
013400010604     C                   eval      vinflg = '1'
013500010604     C                   else
013600010604     C                   eval      vinflg = '2'
013700010604     C                   endif
013800010604     C                   endif
013900010604     C*
014000010330     C                   else
014100010330     C                   eval      vinflg = '1'
014200010330     C                   endif
014300010601     C*
014400010601     C  N70              update    tivin000
014500010330     C*
014600010330     C  N70              ENDdo
014700010601     C*
014800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
014900010601     C                   if        cntNonEl = *zeros or
015000010601     C                             flgMulti = '0'
015100010330     C* Se non ci sono record con errori ...
015200010601     C                   if        §ctrno = 0 and
015300010604     C                             §ctrmo = 0 and
015400010601     C                             flgStato <> '2'
015500010330     C* ... restituisco esito OK.
015600010330     C                   eval      wrkesito = '0'
015700010330     C                   else
015800010330     C                   if        §ctrok > 0
015900010330     C                   eval      wrkesito = '1'
016000010330     C                   else
016100010615     C                   if        flgOk = '0'
016200010615     C                   eval      wrkesito = '2'
016300010615     C                   else
016400010615     C                   eval      wrkesito = '6'
016500010615     C                   endif
016600010330     C                   endif
016700010330     C                   endif
016800010601     C                   else
016900010601     C                   eval      wrkesito = '9'
017000010601     C                   endif
017100010330     C*
017200010330     C                   if        %open(tivin00r)
017300010330     C                   close     tivin00r
017400010330     C                   endif
017500120224     C                   if        %open(edivabwr)
017600120224     C                   close     edivabwr
017700010330     C                   endif
017800010601     C*
017900010601     C                   if        vlrpoi <> 999
018000010601     C                   eval      invfil = vlrpoi
018100010601     C                   endif
018200010330     C*
018300010330     C                   if        §ctrok > 0
018400010601     C                             and invfil > *zeros
018500010330     C                   exsr      invio
018600010330     C                   endif
018700010601     C*
018800010618     C                   if        flgGiro = '1'
018900010601     C                   exsr      endela
019000010618     C                   endif
019100010330     C*
019200010330     C                   ENDSR
019300010330     C***
019400070601
019500070601     C*----------------------------------------------------*
019600070601     C*  SCARICAMENTO BUFFER RECORDS VAB
019700070601     C*----------------------------------------------------*
019800070601     C     WRIVAB        BEGSR
019900120224     C*
020000120224     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
020100120224     C                   EVAL      VABCMR = %char(datcor)+' '+%char(oracor)
020200120224     C                   EVAL      VABDCM = datcor
020300120224     C                   EVAL      VABDTS = datcor
020400120224     C                   EVAL      VABHMS = oracor
020500120224     C                   EVAL      VABCNT = 1
020600070601     C*
020700120224     C  N31              WRITE     EDIVAB00
020800070601     C*
020900070601     C                   ENDSR
021000010601
021100010601
021200010601
021300010330     C*----------------------------------------------------*
021400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021500010330     C*----------------------------------------------------*
021600010330     C     INZVAR        BEGSR
021700010330     C*
021800020204     C                   Z-ADD     *zeros        Num5_0            5 0
021900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022000020322     C                   MOVEL     '0'           FlgCAS            1
022100010330     C*
022200010330     C                   ENDSR
022300010330     C*----------------------------------------------------*
022400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022500010330     C*----------------------------------------------------*
022600010330     C     DEFCAM        BEGSR
022700010330     C*
022800020204     C* Inizializzo il buffer del record da scrivere
022900120224     C                   CLEAR                   EDIVAB00
023000020204     C* Imposto i valori di default...
023100080307     C                   EVAL      VABCCM = 0891093
023200080307     C                   EVAL      VABLNP = 089
023300080307     C                   EVAL      VABCTR = 000
023400080307     C                   EVAL      VABTSP = 'C'
023500070208     C                   EVAL      VABCBO = '1'
023600020204     C* ... e poi verifico se sono stati passati come parametri
023700020204     C                   IF        vlrppt > *blanks
023800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024000020204     C                   EXSR      CHKNUM
024100020204     C                   IF        PiInt=*on
024200020204     C                   Z-ADD     PiVal         VABCCM
024300020204     C                   ENDIF
024400040301     C                   ENDIF
024500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024700020204     C                   EXSR      CHKNUM
024800020204     C                   IF        PiInt=*on
024900020204     C                   Z-ADD     PiVal         VABLNP
025000020204     C                   ENDIF
025100040301     C                   ENDIF
025200040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025400020204     C                   EXSR      CHKNUM
025500020204     C                   IF        PiInt=*on
025600020204     C                   Z-ADD     PiVal         VABCTR
025700040301     C                   ENDIF
025800020204     C                   ENDIF
025900020204     C                   ENDIF
026000020204     C*
026100010330     C                   ENDSR
026200010607     C*----------------------------------------------------*
026300120224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
026400010607     C*----------------------------------------------------*
026500010607     C     IMPVAB        BEGSR
026600010607     C*
026700010607     C                   SETOFF                                       3132
026800010607     C*
026900010607     C* Reperimento campi ALFA
027000010607     C*
027100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027200080307     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
027300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027500020117     C* ==
027600080307     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
027700080307     C                   EVAL      VABLOD=%trim(%subst(vindta:84:20))
027800080307     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
027900080307     C                   EVAL      VABRMA=%trim(%subst(vindta:133:6))
028000060510     C*
028100060510     C* Reperimento campi NUMERICI
028200060510     C                   MOVEL     DATCOR        VABAAS
028300060510     C                   MOVE      DATCOR        VABMGS
028400080307     C*
028500080307     C* Considerazioni relative al CCM
028600080307     C                   IF        %trim(%subst(vindta:151:1)) = '*'
028700080307     C                   EVAL      VABCCM=0891092
028800080307     C                   ENDIF
028900060710     C* NSP/RMN
029000080307     C                   EVAL      PiStr=%trim(%subst(vindta:114:6))
029100060510     C                   EXSR      CHKNUM
029200060510     C                   IF        PiInt=*on
029300060510     C                   Z-ADD     PiVal         VABNSP
029400060510     C                   Z-ADD     PiVal         VABRMN
029500060404     C                   ELSE
029600070502     C                   SETON                                        31
029700060510     C                   Z-ADD     *zeros        VABNSP
029800060510     C                   Z-ADD     1             VABRMN
029900060404     C                   EVAL      vinmsg = %trimr(vinmsg)
030000060510     C                             + ' ' + 'VABNSP VABRMN'
030100060404     C                   ENDIF
030200040420     C* CAD
030300080307     C                   EVAL      PiStr=%trim(%subst(vindta:104:5))
030400010607     C                   EXSR      CHKNUM
030500010607     C                   IF        PiInt=*on
030600020204     C                   Z-ADD     PiVal         Num5_0
030700020204     C                   MOVEL(p)  Num5_0        VABCAD
030800010607     C                   ELSE
030900010607     C                   SETON                                        32
031000020204     C                   EVAL      VABCAD = *zeros
031100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031200020204     C                             + ' ' + 'VABCAD'
031300060316     C                   ENDIF
031400070502     C* Reperisco la provincia dal CAP e dalla località
031500070502     C                   IF        VABCAD <> *blanks AND
031600070502     C                             VABPRD  = *blanks
031700070502     C                   CLEAR                   TISI95DS
031800070502     C                   EVAL      I95TCN = '3'
031900070502     C                   Z-ADD     datcor        I95DAT
032000070502     C                   EVAL      I95CAP = VABCAD
032100070502     C                   EVAL      I95LOC = VABLOD
032200070502     C                   EVAL      I95NAR = VABNZD
032300070502     C                   CALL      'TISI95R'
032400070502     C                   PARM                    TISI95DS
032500070502     C                   EVAL      VABPRD = O95PRV
032600070502     C                   ENDIF
032700040420     C* NCL
032800080403     C                   EVAL      PiStr=%trim(%subst(vindta:139:3))
032900010607     C                   EXSR      CHKNUM
033000010607     C                   IF        PiInt=*on
033100070606     C                   Z-ADD     PiVal         VABNCL
033200010607     C                   ELSE
033300010607     C                   SETON                                        32
033400010607     C                   Z-ADD     *zeros        VABNCL
033500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033600010607     C                             + ' ' + 'VABNCL'
033700010607     C                   ENDIF
033800071029     C* PKB
033900080307     C                   EVAL      PiStr=%trim(%subst(vindta:142:6))
034000010607     C                   EXSR      CHKNUM
034100010607     C                   IF        PiNum=*on
034200071003     C                   Z-ADD(H)  PiVal         VABPKB
034300010607     C                   ELSE
034400010607     C                   SETON                                        32
034500010607     C                   Z-ADD     *zeros        VABPKB
034600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034700010607     C                             + ' ' + 'VABPKB'
034800010607     C                   ENDIF
034900020322     C*
035000020322     C* Considerazioni finali su CBO/CAS
035100050908     C                   IF        FlgCAS = '1'
035200040713     C                   IF        VABCBO = '1'
035300020322     C                   EVAL      VABCBO = '4'
035400040713     C                   ENDIF
035500040713     C                   IF        VABCBO = '2'
035600040713     C                   EVAL      VABCBO = '6'
035700040713     C                   ENDIF
035800020322     C                   ENDIF
035900020204     C*
036000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036100020204     C                   EXSR      CHKIMPDIV
036200020204     C*
036300010607     C                   ENDSR
036400010607     C*----------------------------------------------------*
036500010601
036600020204
036700020204     C*----------------------------------------------------*
036800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
036900020204     C*----------------------------------------------------*
037000020204     C     CHKIMPDIV     BEGSR
037100020204     C*
037200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
037300020204     C                   Z-ADD     *zeros        wrkDec            9 9
037400020204     C*
037500020204     C* Come prima cosa effettuo considerazioni sulla divisa
037600020204     C                   IF        vabIAS > *zeros
037700020204     C                   IF        vabVAS <> 'EUR'
037800020204     C                   EVAL      vabVAS =  'ITL'
037900020204     C                   ENDIF
038000020204     C                   ENDIF
038100020204     C*
038200020204     C                   IF        vabCAS > *zeros
038300020204     C                   IF        vabVCA <> 'EUR'
038400020204     C                   EVAL      vabVCA =  'ITL'
038500020204     C                   ENDIF
038600020204     C                   ENDIF
038700020204     C*
038800020204     C                   IF        vabVMD > *zeros
038900020321     C                   IF        vabVAD <> 'EUR'
039000020204     C                   EVAL      vabVAD =  'ITL'
039100020204     C                   ENDIF
039200020204     C                   ENDIF
039300020204     C*
039400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
039500020204     C                   Z-ADD     vabIAS        wrkDec
039600020204     C                   IF        wrkDec > *zeros
039700020204     C                   IF        vabVAS = 'ITL'
039800020204     C                   EVAL      vabIAS = *zeros
039900020204     C                   ENDIF
040000020204     C                   ENDIF
040100020204     C*
040200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
040300020204     C                   Z-ADD     vabCAS        wrkDec
040400020204     C                   IF        wrkDec > *zeros
040500020204     C                   IF        vabVCA = 'ITL'
040600020204     C                   EVAL      vabCAS = *zeros
040700020204     C                   ENDIF
040800020204     C                   ENDIF
040900020204     C*
041000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
041100020204     C                   Z-ADD     vabVMD        wrkDec
041200020204     C                   IF        wrkDec > *zeros
041300020204     C                   IF        vabVAD = 'ITL'
041400020204     C                   EVAL      vabVMD = *zeros
041500020204     C                   ENDIF
041600020204     C                   ENDIF
041700020204     C*
041800020204     C                   ENDSR
041900020204     C***
042000020204
042100010330
042200010330
042300010330     C*----------------------------------------------------*
042400010330     C*  CONTROLLO NUMERICITA' CAMPI
042500010330     C*----------------------------------------------------*
042600010330     C     CHKNUM        BEGSR
042700010330     C*
042800010606     C                   IF        PiDecChr = *blanks
042900070502     C                   EVAL      PiDecChr = '.'
043000010606     C                   ENDIF
043100010606     C*
043200010606     C                   CALL(e)   'ISNUMERIC'
043300010330     C                   PARM                    PiStr            30
043400010606     C                   PARM                    PiDecChr          1
043500010330     C                   PARM      *ZEROS        PiVal            30 9
043600010330     C                   PARM      '0'           PiInt             1
043700010330     C                   PARM      '0'           PiNum             1
043800010330     C                   IF        %error
043900010606     C                   EVAL      PiNum=*off
044000010330     C                   ENDIF
044100010330     C*
044200010330     C                   ENDSR
044300010330     C***
044400010330
044500010601
044600010601
044700010601
044800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044900010601     C     repfil        BEGSR
045000010601     C*
045100010601     C                   if        invfil = *zeros and
045200010601     C                             depfil > *zeros and
045300010629     C                             (vinflg = *blanks or
045400010629     C                              vinflg = *zeros)
045500010601     C
045600010601     C                   eval      invfil = depfil
045700010601     C                   endif
045800010601     C*
045900010601     C                   if        depfil <> invfil and
046000010601     C                             invfil > *zeros
046100010601     C                   eval      flgMulti = '1'
046200010601     C                   if        vinflg = *blanks
046300010601     C                   add       1             cntNonEl
046400010601     C                   endif
046500010601     C                   endif
046600010601     C*
046700010601     C                   if        vinflg = '2'
046800010601     C                   eval      flgStato = '2'
046900010601     C                   endif
047000010601     C*
047100010601     C                   ENDSR
047200010601     C***
047300010601
047400010601
047500010601
047600010330
047700010330
047800010330
047900990920      /TITLE Invio dei dati al punto operativo.
048000000613     C     invio         BEGSR
048100990920     C*
048200990920     C                   reset                   dscmz
048300010601     C                   move      invfil        cmzdst
048400990920     C                   eval      cmzfld = vlrfou
048500990920     C                   eval      cmzmbd = vlrhdl
048600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048700000710     C                   if        prmfir = *blanks
048800120224     C                   eval      cmzfla = 'EDIVAB0F'
048900120224     C                   eval      cmzmba = 'EDIVAB0F'
049000000710     C                   else
049100000710     C                   eval      cmzfla = prmfir
049200000710     C                   eval      cmzmba = prmfir
049300000710     C                   endif
049400990920     C                   eval      cmznrr = *zeros
049500990920     C                   move      §ctrok        cmznrr
049600021018     C                   eval      cmzlba = vlrfl1
049700990920     C                   call(e)   'TIS711C'
049800990920     C                   parm                    dscmz
049900990921     C                   parm      *blanks       esito
050000990920     C                   if        %error
050100990920     C                             or cmzerr = '1'
050200990921     C                             or esito  = '1'
050300000710     C                   eval      wrkesito = '3'
050400990920     C                   endif
050500990920     C*
050600000613     C                   ENDSR
050700990910
050800010601
050900010601
051000010601
051100010601
051200010601      /TITLE Invio dei dati al punto operativo.
051300010601     C     opeini        BEGSR
051400010601     C*
051500010601     C* Inizializzo flag e contatori operativi
051600010601     C                   movel     '0'           flgGiro           1
051700010601     C                   movel     '0'           flgMulti          1
051800010601     C                   movel     '1'           flgStato          1
051900010615     C                   movel     '0'           flgOk             1
052000010601     C                   z-add     *zeros        cntNonEl         10 0
052100010601     C                   z-add     *zeros        depfil            3 0
052200010601     C                   z-add     *zeros        invfil            3 0
052300010601     C*
052400010601     C                   ENDSR
052500010601     C***
052600010601
052700010601
052800010601
052900010330
053000010330
053100000613     C     *inzsr        BEGSR
053200990910     C*
053300990910     C     *entry        plist
053400990920     C                   parm                    tivlrds
053500990921     C                   parm      wrkesito      esito
053600000724     C                   parm                    prmlit
053700000710     C                   parm                    prmfir
053800010330     C*
053900010330     C* CALCOLA LA DATA CORRENTE
054000010330     C                   time                    wn14             14 0
054100120224     C                   movel     wn14          oracor            6 0          *ORA       N G/M/AA
054200120224     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
054300120224     C                   eval      datcor = %dec(%date() : *ISO)
054400000613     C*
054500000613     C                   ENDSR
054600000613     C***
