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
022500100527     C                   EVAL      VABCCM = 0600902
022600100527     C                   EVAL      VABLNP = 060
022700100527     C                   EVAL      VABCTR = 000
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
026900080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
027000070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
027100070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
027200070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
027300100527     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
027400100527     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
027500080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
027600080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
027700080311     C                   IF        %subst(vindta:210:1) = 'M'
027800080311     C                   EVAL      VABTIC='BM'
027900080311     C                   ENDIF
028000080311     C                   IF        %subst(vindta:210:1) = 'N'
028100080311     C                   EVAL      VABTIC='TM'
028200080311     C                   ENDIF
028300100527     C                   IF        %subst(vindta:210:1) = 'R'
028400100527     C                   EVAL      VABTIC='CM'
028500080311     C                   ENDIF
028600080311     C                   IF        %subst(vindta:227:4) = *blanks
028700080311     C                   EVAL      VABVCA='EUR'
028800080311     C                   ENDIF
028900080311     C                   IF        %subst(vindta:231:1) = 'I'
029000080311     C                   EVAL      VABTC1='A'
029100080311     C                   ENDIF
029200080311     C                   IF        %subst(vindta:231:1) = '2'
029300080311     C                   EVAL      VABTC2='P'
029400080311     C                   ENDIF
029500080311     C                   IF        %subst(vindta:231:1) = '1'
029600080311     C                   EVAL      VABTC2='S'
029700080311     C                   ENDIF
029800060510     C*
029900060510     C* Reperimento campi NUMERICI
030000060510     C                   MOVEL     DATCOR        VABAAS
030100060510     C                   MOVE      DATCOR        VABMGS
030200080311     C* CCM
030300080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC300'
030400100225     C***                EVAL      VABCCM=
030500080311     C                   ENDIF
030600080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC3000'
030700100225     C***                EVAL      VABCCM=
030800080311     C                   ENDIF
030900080311     C* DCR
031000100527     C                   IF        %subst(vindta:11:8) <> *blanks
031100080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
031200080311     C                   EXSR      CHKNUM
031300080311     C                   IF        PiInt=*on
031400080311     C                   Z-ADD     PiVal         VABDCR
031500080311     C                   ELSE
031600100527     C                   Z-ADD     *zeros        VABDCR
031700080311     C                   EVAL      vinmsg = %trimr(vinmsg)
031800080311     C                             + ' ' + 'VABDCR'
031900080311     C                   ENDIF
032000100527     C                   ENDIF
032100060710     C* NSP/RMN
032200080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
032300060510     C                   EXSR      CHKNUM
032400060510     C                   IF        PiInt=*on
032500060510     C                   Z-ADD     PiVal         VABNSP
032600060510     C                   Z-ADD     PiVal         VABRMN
032700060404     C                   ELSE
032800070502     C                   SETON                                        31
032900060510     C                   Z-ADD     *zeros        VABNSP
033000060510     C                   Z-ADD     1             VABRMN
033100060404     C                   EVAL      vinmsg = %trimr(vinmsg)
033200060510     C                             + ' ' + 'VABNSP VABRMN'
033300060404     C                   ENDIF
033400040420     C* CAD
033500080311     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
033600010607     C                   EXSR      CHKNUM
033700010607     C                   IF        PiInt=*on
033800020204     C                   Z-ADD     PiVal         Num5_0
033900020204     C                   MOVEL(p)  Num5_0        VABCAD
034000010607     C                   ELSE
034100010607     C                   SETON                                        32
034200020204     C                   EVAL      VABCAD = *zeros
034300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034400020204     C                             + ' ' + 'VABCAD'
034500060316     C                   ENDIF
034600070502     C* Reperisco la provincia dal CAP e dalla località
034700070502     C                   IF        VABCAD <> *blanks AND
034800070502     C                             VABPRD  = *blanks
034900070502     C                   CLEAR                   TISI95DS
035000070502     C                   EVAL      I95TCN = '3'
035100070502     C                   Z-ADD     datcor        I95DAT
035200070502     C                   EVAL      I95CAP = VABCAD
035300070502     C                   EVAL      I95LOC = VABLOD
035400070502     C                   EVAL      I95NAR = VABNZD
035500070502     C                   CALL      'TISI95R'
035600070502     C                   PARM                    TISI95DS
035700070502     C                   EVAL      VABPRD = O95PRV
035800070502     C                   ENDIF
035900040420     C* NCL
036000070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
036100010607     C                   EXSR      CHKNUM
036200010607     C                   IF        PiInt=*on
036300070606     C                   Z-ADD     PiVal         VABNCL
036400010607     C                   ELSE
036500010607     C                   SETON                                        32
036600010607     C                   Z-ADD     *zeros        VABNCL
036700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036800010607     C                             + ' ' + 'VABNCL'
036900010607     C                   ENDIF
037000040420     C* PKB
037100070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
037200070208     C                   EVAL      PiDecChr = '.'
037300010607     C                   EXSR      CHKNUM
037400010607     C                   IF        PiNum=*on
037500070925     C                   Z-ADD(H)  PiVal         VABPKB
037600010607     C                   ELSE
037700010607     C                   SETON                                        32
037800010607     C                   Z-ADD     *zeros        VABPKB
037900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038000010607     C                             + ' ' + 'VABPKB'
038100010607     C                   ENDIF
038200070925     C* VLB
038300070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
038400070925     C                   EVAL      PiDecChr = '.'
038500070925     C                   EXSR      CHKNUM
038600070925     C                   IF        PiNum=*on
038700070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
038800070925     C                   Z-ADD     PiVal         VABVLB
038900070925     C                   ELSE
039000070925     C                   SETON                                        32
039100070925     C                   Z-ADD     *zeros        VABVLB
039200070925     C                   EVAL      vinmsg = %trimr(vinmsg)
039300070925     C                             + ' ' + 'VABVLB'
039400070925     C                   ENDIF
039500080311     C* CAS
039600080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
039700080311     C                             %subst(vindta:196:14) <> *zeros
039800080311     C                   EVAL      FlgCAS = '1'
039900080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
040000080311     C                   EXSR      CHKNUM
040100080311     C                   IF        PiNum=*on
040200080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
040300080311     C                   Z-ADD     PiVal         VABCAS
040400080311     C                   ELSE
040500080311     C                   SETON                                        32
040600080311     C                   Z-ADD     *zeros        VABVLB
040700080311     C                   EVAL      vinmsg = %trimr(vinmsg)
040800080311     C                             + ' ' + 'VABCAS'
040900080311     C                   ENDIF
041000080311     C                   ENDIF
041100100527     C*
041200100527     C* Particolarità x codice tariffa
041300100527     C                   IF        VABPKB > 30
041400100527     C                   EVAL      VABCTR = 010
041500100527     C                   ENDIF
041600020322     C*
041700020322     C* Considerazioni finali su CBO/CAS
041800050908     C                   IF        FlgCAS = '1'
041900040713     C                   IF        VABCBO = '1'
042000020322     C                   EVAL      VABCBO = '4'
042100040713     C                   ENDIF
042200040713     C                   IF        VABCBO = '2'
042300040713     C                   EVAL      VABCBO = '6'
042400040713     C                   ENDIF
042500020322     C                   ENDIF
042600020204     C*
042700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042800020204     C                   EXSR      CHKIMPDIV
042900020204     C*
043000010607     C                   ENDSR
043100010607     C*----------------------------------------------------*
043200010601
043300020204
043400020204     C*----------------------------------------------------*
043500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043600020204     C*----------------------------------------------------*
043700020204     C     CHKIMPDIV     BEGSR
043800020204     C*
043900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044000020204     C                   Z-ADD     *zeros        wrkDec            9 9
044100020204     C*
044200020204     C* Come prima cosa effettuo considerazioni sulla divisa
044300020204     C                   IF        vabIAS > *zeros
044400020204     C                   IF        vabVAS <> 'EUR'
044500020204     C                   EVAL      vabVAS =  'ITL'
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C                   IF        vabCAS > *zeros
045000020204     C                   IF        vabVCA <> 'EUR'
045100020204     C                   EVAL      vabVCA =  'ITL'
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C                   IF        vabVMD > *zeros
045600020321     C                   IF        vabVAD <> 'EUR'
045700020204     C                   EVAL      vabVAD =  'ITL'
045800020204     C                   ENDIF
045900020204     C                   ENDIF
046000020204     C*
046100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046200020204     C                   Z-ADD     vabIAS        wrkDec
046300020204     C                   IF        wrkDec > *zeros
046400020204     C                   IF        vabVAS = 'ITL'
046500020204     C                   EVAL      vabIAS = *zeros
046600020204     C                   ENDIF
046700020204     C                   ENDIF
046800020204     C*
046900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047000020204     C                   Z-ADD     vabCAS        wrkDec
047100020204     C                   IF        wrkDec > *zeros
047200020204     C                   IF        vabVCA = 'ITL'
047300020204     C                   EVAL      vabCAS = *zeros
047400020204     C                   ENDIF
047500020204     C                   ENDIF
047600020204     C*
047700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047800020204     C                   Z-ADD     vabVMD        wrkDec
047900020204     C                   IF        wrkDec > *zeros
048000020204     C                   IF        vabVAD = 'ITL'
048100020204     C                   EVAL      vabVMD = *zeros
048200020204     C                   ENDIF
048300020204     C                   ENDIF
048400020204     C*
048500020204     C                   ENDSR
048600020204     C***
048700020204
048800010330
048900010330
049000010330     C*----------------------------------------------------*
049100010330     C*  CONTROLLO NUMERICITA' CAMPI
049200010330     C*----------------------------------------------------*
049300010330     C     CHKNUM        BEGSR
049400010330     C*
049500010606     C                   IF        PiDecChr = *blanks
049600070502     C                   EVAL      PiDecChr = '.'
049700010606     C                   ENDIF
049800010606     C*
049900010606     C                   CALL(e)   'ISNUMERIC'
050000010330     C                   PARM                    PiStr            30
050100010606     C                   PARM                    PiDecChr          1
050200010330     C                   PARM      *ZEROS        PiVal            30 9
050300010330     C                   PARM      '0'           PiInt             1
050400010330     C                   PARM      '0'           PiNum             1
050500010330     C                   IF        %error
050600010606     C                   EVAL      PiNum=*off
050700010330     C                   ENDIF
050800010330     C*
050900010330     C                   ENDSR
051000010330     C***
051100010330
051200010601
051300010601
051400010601
051500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051600010601     C     repfil        BEGSR
051700010601     C*
051800010601     C                   if        invfil = *zeros and
051900010601     C                             depfil > *zeros and
052000010629     C                             (vinflg = *blanks or
052100010629     C                              vinflg = *zeros)
052200010601     C
052300010601     C                   eval      invfil = depfil
052400010601     C                   endif
052500010601     C*
052600010601     C                   if        depfil <> invfil and
052700010601     C                             invfil > *zeros
052800010601     C                   eval      flgMulti = '1'
052900010601     C                   if        vinflg = *blanks
053000010601     C                   add       1             cntNonEl
053100010601     C                   endif
053200010601     C                   endif
053300010601     C*
053400010601     C                   if        vinflg = '2'
053500010601     C                   eval      flgStato = '2'
053600010601     C                   endif
053700010601     C*
053800010601     C                   ENDSR
053900010601     C***
054000010601
054100010601
054200010601
054300010330
054400010330
054500010330
054600990920      /TITLE Invio dei dati al punto operativo.
054700000613     C     invio         BEGSR
054800990920     C*
054900990920     C                   reset                   dscmz
055000010601     C                   move      invfil        cmzdst
055100990920     C                   eval      cmzfld = vlrfou
055200990920     C                   eval      cmzmbd = vlrhdl
055300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400000710     C                   if        prmfir = *blanks
055500050408     C                   eval      cmzfla = 'FIVAB00F'
055600050408     C                   eval      cmzmba = 'FIVAB00F'
055700000710     C                   else
055800000710     C                   eval      cmzfla = prmfir
055900000710     C                   eval      cmzmba = prmfir
056000000710     C                   endif
056100990920     C                   eval      cmznrr = *zeros
056200990920     C                   move      §ctrok        cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400990920     C                   call(e)   'TIS711C'
056500990920     C                   parm                    dscmz
056600990921     C                   parm      *blanks       esito
056700990920     C                   if        %error
056800990920     C                             or cmzerr = '1'
056900990921     C                             or esito  = '1'
057000000710     C                   eval      wrkesito = '3'
057100990920     C                   endif
057200990920     C*
057300000613     C                   ENDSR
057400990910
057500010601
057600010601
057700010601
057800010601
057900010601      /TITLE Invio dei dati al punto operativo.
058000010601     C     opeini        BEGSR
058100010601     C*
058200010601     C* Inizializzo flag e contatori operativi
058300010601     C                   movel     '0'           flgGiro           1
058400010601     C                   movel     '0'           flgMulti          1
058500010601     C                   movel     '1'           flgStato          1
058600010615     C                   movel     '0'           flgOk             1
058700010601     C                   z-add     *zeros        cntNonEl         10 0
058800010601     C                   z-add     *zeros        depfil            3 0
058900010601     C                   z-add     *zeros        invfil            3 0
059000010601     C*
059100010601     C                   ENDSR
059200010601     C***
059300010601
059400010601
059500010601
059600010330
059700010330
059800000613     C     *inzsr        BEGSR
059900990910     C*
060000990910     C     *entry        plist
060100990920     C                   parm                    tivlrds
060200990921     C                   parm      wrkesito      esito
060300000724     C                   parm                    prmlit
060400000710     C                   parm                    prmfir
060500010330     C*
060600010330     C* CALCOLA LA DATA CORRENTE
060700100225     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
060800100225     C                   eval      datcor = %dec(%date() : *ISO)
060900000613     C*
061000000613     C                   ENDSR
061100000613     C***
