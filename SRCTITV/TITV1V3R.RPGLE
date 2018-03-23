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
022500100225     C                   EVAL      VABCCM = 0860249
022600100225     C                   EVAL      VABLNP = 020
022700080311     C                   EVAL      VABCTR = 100
022800080311     C                   EVAL      VABCTM = '7Q'
022900070208     C                   EVAL      VABCBO = '1'
023000020204     C* ... e poi verifico se sono stati passati come parametri
023100020204     C                   IF        vlrppt > *blanks
023200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020204     C                   EXSR      CHKNUM
023500020204     C                   IF        PiInt=*on
023600020204     C                   Z-ADD     PiVal         VABCCM
023700020204     C                   ENDIF
023800040301     C                   ENDIF
023900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300020204     C                   Z-ADD     PiVal         VABLNP
024400020204     C                   ENDIF
024500040301     C                   ENDIF
024600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024800020204     C                   EXSR      CHKNUM
024900020204     C                   IF        PiInt=*on
025000020204     C                   Z-ADD     PiVal         VABCTR
025100040301     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C                   ENDIF
025400020204     C*
025500010330     C                   ENDSR
025600010607     C*----------------------------------------------------*
025700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025800010607     C*----------------------------------------------------*
025900010607     C     IMPVAB        BEGSR
026000010607     C*
026100010607     C                   SETOFF                                       3132
026200010607     C*
026300010607     C* Reperimento campi ALFA
026400010607     C*
026500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026600070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
026700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026900020117     C* ==
027000080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
027100070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
027200070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
027300070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
027400080311     C***                EVAL      VABNZD=%trim(%subst(vindta:156:3))
027500070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
027600080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
027700080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
027800080311     C                   IF        %subst(vindta:210:1) = 'M'
027900080311     C                   EVAL      VABTIC='BM'
028000080311     C                   ENDIF
028100080311     C                   IF        %subst(vindta:210:1) = 'N'
028200080311     C                   EVAL      VABTIC='TM'
028300080311     C                   ENDIF
028400100527     C                   IF        %subst(vindta:210:1) = 'R'
028500100527     C                   EVAL      VABTIC='CM'
028600080311     C                   ENDIF
028700080311     C                   IF        %subst(vindta:227:4) = *blanks
028800080311     C                   EVAL      VABVCA='EUR'
028900080311     C                   ENDIF
029000080311     C                   IF        %subst(vindta:231:1) = 'I'
029100080311     C                   EVAL      VABTC1='A'
029200080311     C                   ENDIF
029300080311     C                   IF        %subst(vindta:231:1) = '2'
029400080311     C                   EVAL      VABTC2='P'
029500080311     C                   ENDIF
029600080311     C                   IF        %subst(vindta:231:1) = '1'
029700080311     C                   EVAL      VABTC2='S'
029800080311     C                   ENDIF
029900060510     C*
030000060510     C* Reperimento campi NUMERICI
030100060510     C                   MOVEL     DATCOR        VABAAS
030200060510     C                   MOVE      DATCOR        VABMGS
030300080311     C* CCM
030400080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC300'
030500100225     C***                EVAL      VABCCM=
030600080311     C                   ENDIF
030700080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC3000'
030800100225     C***                EVAL      VABCCM=
030900080311     C                   ENDIF
031000080311     C* DCR
031100080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
031200080311     C                   EXSR      CHKNUM
031300080311     C                   IF        PiInt=*on
031400080311     C                   Z-ADD     PiVal         VABDCR
031500080311     C                   ELSE
031600100527     C                   Z-ADD     *zeros        VABDCR
031700080311     C                   EVAL      vinmsg = %trimr(vinmsg)
031800080311     C                             + ' ' + 'VABDCR'
031900080311     C                   ENDIF
032000060710     C* NSP/RMN
032100080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
032200060510     C                   EXSR      CHKNUM
032300060510     C                   IF        PiInt=*on
032400060510     C                   Z-ADD     PiVal         VABNSP
032500060510     C                   Z-ADD     PiVal         VABRMN
032600060404     C                   ELSE
032700070502     C                   SETON                                        31
032800060510     C                   Z-ADD     *zeros        VABNSP
032900060510     C                   Z-ADD     1             VABRMN
033000060404     C                   EVAL      vinmsg = %trimr(vinmsg)
033100060510     C                             + ' ' + 'VABNSP VABRMN'
033200060404     C                   ENDIF
033300040420     C* CAD
033400080311     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
033500010607     C                   EXSR      CHKNUM
033600010607     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         Num5_0
033800020204     C                   MOVEL(p)  Num5_0        VABCAD
033900010607     C                   ELSE
034000010607     C                   SETON                                        32
034100020204     C                   EVAL      VABCAD = *zeros
034200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034300020204     C                             + ' ' + 'VABCAD'
034400060316     C                   ENDIF
034500070502     C* Reperisco la provincia dal CAP e dalla località
034600070502     C                   IF        VABCAD <> *blanks AND
034700070502     C                             VABPRD  = *blanks
034800070502     C                   CLEAR                   TISI95DS
034900070502     C                   EVAL      I95TCN = '3'
035000070502     C                   Z-ADD     datcor        I95DAT
035100070502     C                   EVAL      I95CAP = VABCAD
035200070502     C                   EVAL      I95LOC = VABLOD
035300070502     C                   EVAL      I95NAR = VABNZD
035400070502     C                   CALL      'TISI95R'
035500070502     C                   PARM                    TISI95DS
035600070502     C                   EVAL      VABPRD = O95PRV
035700070502     C                   ENDIF
035800040420     C* NCL
035900070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
036000010607     C                   EXSR      CHKNUM
036100010607     C                   IF        PiInt=*on
036200070606     C                   Z-ADD     PiVal         VABNCL
036300010607     C                   ELSE
036400010607     C                   SETON                                        32
036500010607     C                   Z-ADD     *zeros        VABNCL
036600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036700010607     C                             + ' ' + 'VABNCL'
036800010607     C                   ENDIF
036900040420     C* PKB
037000070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
037100070208     C                   EVAL      PiDecChr = '.'
037200010607     C                   EXSR      CHKNUM
037300010607     C                   IF        PiNum=*on
037400070925     C                   Z-ADD(H)  PiVal         VABPKB
037500010607     C                   ELSE
037600010607     C                   SETON                                        32
037700010607     C                   Z-ADD     *zeros        VABPKB
037800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037900010607     C                             + ' ' + 'VABPKB'
038000010607     C                   ENDIF
038100070925     C* VLB
038200070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
038300070925     C                   EVAL      PiDecChr = '.'
038400070925     C                   EXSR      CHKNUM
038500070925     C                   IF        PiNum=*on
038600070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
038700070925     C                   Z-ADD     PiVal         VABVLB
038800070925     C                   ELSE
038900070925     C                   SETON                                        32
039000070925     C                   Z-ADD     *zeros        VABVLB
039100070925     C                   EVAL      vinmsg = %trimr(vinmsg)
039200070925     C                             + ' ' + 'VABVLB'
039300070925     C                   ENDIF
039400080311     C* CAS
039500080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
039600080311     C                             %subst(vindta:196:14) <> *zeros
039700080311     C                   EVAL      FlgCAS = '1'
039800080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
039900080311     C                   EVAL      PiDecChr = '.'
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
041100020322     C*
041200020322     C* Considerazioni finali su CBO/CAS
041300050908     C                   IF        FlgCAS = '1'
041400040713     C                   IF        VABCBO = '1'
041500020322     C                   EVAL      VABCBO = '4'
041600040713     C                   ENDIF
041700040713     C                   IF        VABCBO = '2'
041800040713     C                   EVAL      VABCBO = '6'
041900040713     C                   ENDIF
042000020322     C                   ENDIF
042100020204     C*
042200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300020204     C                   EXSR      CHKIMPDIV
042400020204     C*
042500010607     C                   ENDSR
042600010607     C*----------------------------------------------------*
042700010601
042800020204
042900020204     C*----------------------------------------------------*
043000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043100020204     C*----------------------------------------------------*
043200020204     C     CHKIMPDIV     BEGSR
043300020204     C*
043400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043500020204     C                   Z-ADD     *zeros        wrkDec            9 9
043600020204     C*
043700020204     C* Come prima cosa effettuo considerazioni sulla divisa
043800020204     C                   IF        vabIAS > *zeros
043900020204     C                   IF        vabVAS <> 'EUR'
044000020204     C                   EVAL      vabVAS =  'ITL'
044100020204     C                   ENDIF
044200020204     C                   ENDIF
044300020204     C*
044400020204     C                   IF        vabCAS > *zeros
044500020204     C                   IF        vabVCA <> 'EUR'
044600020204     C                   EVAL      vabVCA =  'ITL'
044700020204     C                   ENDIF
044800020204     C                   ENDIF
044900020204     C*
045000020204     C                   IF        vabVMD > *zeros
045100020321     C                   IF        vabVAD <> 'EUR'
045200020204     C                   EVAL      vabVAD =  'ITL'
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045700020204     C                   Z-ADD     vabIAS        wrkDec
045800020204     C                   IF        wrkDec > *zeros
045900020204     C                   IF        vabVAS = 'ITL'
046000020204     C                   EVAL      vabIAS = *zeros
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046500020204     C                   Z-ADD     vabCAS        wrkDec
046600020204     C                   IF        wrkDec > *zeros
046700020204     C                   IF        vabVCA = 'ITL'
046800020204     C                   EVAL      vabCAS = *zeros
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047300020204     C                   Z-ADD     vabVMD        wrkDec
047400020204     C                   IF        wrkDec > *zeros
047500020204     C                   IF        vabVAD = 'ITL'
047600020204     C                   EVAL      vabVMD = *zeros
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C                   ENDSR
048100020204     C***
048200020204
048300010330
048400010330
048500010330     C*----------------------------------------------------*
048600010330     C*  CONTROLLO NUMERICITA' CAMPI
048700010330     C*----------------------------------------------------*
048800010330     C     CHKNUM        BEGSR
048900010330     C*
049000010606     C                   IF        PiDecChr = *blanks
049100070502     C                   EVAL      PiDecChr = '.'
049200010606     C                   ENDIF
049300010606     C*
049400010606     C                   CALL(e)   'ISNUMERIC'
049500010330     C                   PARM                    PiStr            30
049600010606     C                   PARM                    PiDecChr          1
049700010330     C                   PARM      *ZEROS        PiVal            30 9
049800010330     C                   PARM      '0'           PiInt             1
049900010330     C                   PARM      '0'           PiNum             1
050000010330     C                   IF        %error
050100010606     C                   EVAL      PiNum=*off
050200010330     C                   ENDIF
050300010330     C*
050400010330     C                   ENDSR
050500010330     C***
050600010330
050700010601
050800010601
050900010601
051000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051100010601     C     repfil        BEGSR
051200010601     C*
051300010601     C                   if        invfil = *zeros and
051400010601     C                             depfil > *zeros and
051500010629     C                             (vinflg = *blanks or
051600010629     C                              vinflg = *zeros)
051700010601     C
051800010601     C                   eval      invfil = depfil
051900010601     C                   endif
052000010601     C*
052100010601     C                   if        depfil <> invfil and
052200010601     C                             invfil > *zeros
052300010601     C                   eval      flgMulti = '1'
052400010601     C                   if        vinflg = *blanks
052500010601     C                   add       1             cntNonEl
052600010601     C                   endif
052700010601     C                   endif
052800010601     C*
052900010601     C                   if        vinflg = '2'
053000010601     C                   eval      flgStato = '2'
053100010601     C                   endif
053200010601     C*
053300010601     C                   ENDSR
053400010601     C***
053500010601
053600010601
053700010601
053800010330
053900010330
054000010330
054100990920      /TITLE Invio dei dati al punto operativo.
054200000613     C     invio         BEGSR
054300990920     C*
054400990920     C                   reset                   dscmz
054500010601     C                   move      invfil        cmzdst
054600990920     C                   eval      cmzfld = vlrfou
054700990920     C                   eval      cmzmbd = vlrhdl
054800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054900000710     C                   if        prmfir = *blanks
055000050408     C                   eval      cmzfla = 'FIVAB00F'
055100050408     C                   eval      cmzmba = 'FIVAB00F'
055200000710     C                   else
055300000710     C                   eval      cmzfla = prmfir
055400000710     C                   eval      cmzmba = prmfir
055500000710     C                   endif
055600990920     C                   eval      cmznrr = *zeros
055700990920     C                   move      §ctrok        cmznrr
055800021018     C                   eval      cmzlba = vlrfl1
055900990920     C                   call(e)   'TIS711C'
056000990920     C                   parm                    dscmz
056100990921     C                   parm      *blanks       esito
056200990920     C                   if        %error
056300990920     C                             or cmzerr = '1'
056400990921     C                             or esito  = '1'
056500000710     C                   eval      wrkesito = '3'
056600990920     C                   endif
056700990920     C*
056800000613     C                   ENDSR
056900990910
057000010601
057100010601
057200010601
057300010601
057400010601      /TITLE Invio dei dati al punto operativo.
057500010601     C     opeini        BEGSR
057600010601     C*
057700010601     C* Inizializzo flag e contatori operativi
057800010601     C                   movel     '0'           flgGiro           1
057900010601     C                   movel     '0'           flgMulti          1
058000010601     C                   movel     '1'           flgStato          1
058100010615     C                   movel     '0'           flgOk             1
058200010601     C                   z-add     *zeros        cntNonEl         10 0
058300010601     C                   z-add     *zeros        depfil            3 0
058400010601     C                   z-add     *zeros        invfil            3 0
058500010601     C*
058600010601     C                   ENDSR
058700010601     C***
058800010601
058900010601
059000010601
059100010330
059200010330
059300000613     C     *inzsr        BEGSR
059400990910     C*
059500990910     C     *entry        plist
059600990920     C                   parm                    tivlrds
059700990921     C                   parm      wrkesito      esito
059800000724     C                   parm                    prmlit
059900000710     C                   parm                    prmfir
060000010330     C*
060100010330     C* CALCOLA LA DATA CORRENTE
060200100225     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
060300100225     C                   eval      datcor = %dec(%date() : *ISO)
060400000613     C*
060500000613     C                   ENDSR
060600000613     C***
