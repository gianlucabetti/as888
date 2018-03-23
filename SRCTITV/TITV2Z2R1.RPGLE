000100121005      /TITLE Upload via Internet: traduzione in EDIVAWWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400130208     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600121005     FEDIVABWR  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500130208     D ds15          e ds
001600070502     D tisi95ds      e ds
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200170224     D wNomeFile       s             30    INZ(*blanks)
002300130208     D jNAZ            s              5  0 INZ(*zeros)
002400130208     D*------------
002500130208     D skNAZISO        S              3    DIM(1000)
002600130208     D skNAZBAR        S              3    DIM(1000)
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300130208     C                   EXSR      CARTAB                                       CARICA TABELLE
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
004700010601     C*--------------------------------------------------------
004800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004900010601     C*--------------------------------------------------------
005000010601     C     PREELA        BEGSR
005100010601     C*
005200010601     C* SE OCCORRE SPEDIRE IN FILIALE
005300010601     C                   if        invfil <> *zeros and
005400010601     C                             flgGiro = '0'
005500010601     C*
005600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005700010601     C                   eval      flgGiro = '1'
005800010601     C*
005900010601     C                   endif
006000010601     C*
006100010601     C                   ENDSR
006200010601     C***
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
007400010601
007500010601
007600010330     C*--------------------------------------------------------
007700121005     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVAWWR              *
007800010330     C*--------------------------------------------------------
007900010605     C     RWVAB         BEGSR
008000050408     C*
008100010330     C                   if        not %open(tivin00r)
008200010330     C                   open      tivin00r
008300010330     C                   endif
008400121005     C                   if        not %open(edivabwr)
008500121005     C                   open      edivabwr
008600010330     C                   endif
008700010330     C*
008800010604     C                   clear                   §CTROK
008900010604     C                   clear                   §CTRMO
009000010604     C                   clear                   §CTRNO
009100010330     C*
009200010330     C                   DO        *HIVAL
009300010330     C*
009400010330     C                   READ      tivin00r                               70
009500010618     C*
009600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009700010618     C                   if        vinflg = '1'
009800010618     C                   eval      flgOk = '1'
009900010618     C                   endif
010000010618     C*
010100010330     C                   if        vindta > *blanks
010200010330     C                   add       1             rrnum
010300010330     C*
010400010601     C                   if        *in70 = *off and
010500010330     C                             (vinflg = *blanks
010600010330     C                              or vinflg = '0'
010700010330     C                              or vinflg = '2')
010800010330     C*
010900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011000010711     C                   if        vinflg = *blanks or vinflg = '0'
011100010711     C                   clear                   vinmsg
011200010711     C                   endif
011300070601     C*
011400070601     C                   exsr      inzvar
011500070601     C                   exsr      defcam
011600070601     C                   exsr      impvab
011700010601     C*
011800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011900010605     C                   eval      depfil = VABLNP
012000010601     C                   exsr      repfil
012100010601     C                   if        depfil = invfil
012200021025     C                   if        vlrpoi = 999
012300021025     C                   MOVE(P)   invfil        VABFGS
012400021025     C                   else
012500021025     C                   MOVE(P)   vlrpoi        VABFGS
012600021025     C                   endif
012700010601     C*
012800010601     C                   exsr      PREELA
012900010601     C*
013000010604     C* Ebbene...
013100010604     C*
013200010604     C  N31              ADD       1             §CTROK            7 0
013300010604     C   32              ADD       1             §CTRMO            7 0
013400010604     C   31              ADD       1             §CTRNO            7 0
013500010604     C*
013600071120     C                   exsr      wrivab                                       => scrivo VAB
013700071120     C*
013800020722     C                   endif
013900020722     C*
014000010604     C                   if        *in31 = *off and
014100010604     C                             *in32 = *off
014200010604     C                   eval      vinflg = '1'
014300010604     C                   else
014400010604     C                   eval      vinflg = '2'
014500010604     C                   endif
014600010604     C                   endif
014700010604     C*
014800010330     C                   else
014900010330     C                   eval      vinflg = '1'
015000010330     C                   endif
015100010601     C*
015200010601     C  N70              update    tivin000
015300010330     C*
015400010330     C  N70              ENDdo
015500010601     C*
015600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015700010601     C                   if        cntNonEl = *zeros or
015800010601     C                             flgMulti = '0'
015900010330     C* Se non ci sono record con errori ...
016000010601     C                   if        §ctrno = 0 and
016100010604     C                             §ctrmo = 0 and
016200010601     C                             flgStato <> '2'
016300010330     C* ... restituisco esito OK.
016400010330     C                   eval      wrkesito = '0'
016500010330     C                   else
016600010330     C                   if        §ctrok > 0
016700010330     C                   eval      wrkesito = '1'
016800010330     C                   else
016900010615     C                   if        flgOk = '0'
017000010615     C                   eval      wrkesito = '2'
017100010615     C                   else
017200010615     C                   eval      wrkesito = '6'
017300010615     C                   endif
017400010330     C                   endif
017500010330     C                   endif
017600010601     C                   else
017700010601     C                   eval      wrkesito = '9'
017800010601     C                   endif
017900010330     C*
018000010330     C                   if        %open(tivin00r)
018100010330     C                   close     tivin00r
018200010330     C                   endif
018300121005     C                   if        %open(edivabwr)
018400121005     C                   close     edivabwr
018500010330     C                   endif
018600010601     C*
018700010601     C                   if        vlrpoi <> 999
018800010601     C                   eval      invfil = vlrpoi
018900010601     C                   endif
019000010330     C*
019100010330     C                   if        §ctrok > 0
019200010601     C                             and invfil > *zeros
019300010330     C                   exsr      invio
019400010330     C                   endif
019500010601     C*
019600010618     C                   if        flgGiro = '1'
019700010601     C                   exsr      endela
019800010618     C                   endif
019900010330     C*
020000010330     C                   ENDSR
020100010330     C***
020200170224
020300170224
020400070601
020500070601     C*----------------------------------------------------*
020600070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020700070601     C*----------------------------------------------------*
020800070601     C     WRIVAB        BEGSR
020900121005     C*
021000121005     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
021100170224     C                   EVAL      VABCMR = %subst(wNomeFile:1:12) + ' ' +
021200170224     C                                      %char(datcor)
021300121005     C                   EVAL      VABDCM = datcor
021400121005     C                   EVAL      VABDTS = datcor
021500121005     C                   EVAL      VABHMS = oracor
021600121005     C                   EVAL      VABCNT = 1
021700070601     C*
021800121005     C  N31              WRITE     EDIVAB00
021900070601     C*
022000070601     C                   ENDSR
022100010601
022200010601
022300010601
022400010330     C*----------------------------------------------------*
022500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022600010330     C*----------------------------------------------------*
022700010330     C     INZVAR        BEGSR
022800010330     C*
022900020204     C                   Z-ADD     *zeros        Num5_0            5 0
023000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023100020322     C                   MOVEL     '0'           FlgCAS            1
023200010330     C*
023300010330     C                   ENDSR
023400010330     C*----------------------------------------------------*
023500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023600010330     C*----------------------------------------------------*
023700010330     C     DEFCAM        BEGSR
023800010330     C*
023900020204     C* Inizializzo il buffer del record da scrivere
024000121005     C                   CLEAR                   EDIVAB00
024100020204     C* Imposto i valori di default...
024200170224     C                   EVAL      VABCCM = 2201097
024300170224     C                   EVAL      VABLNP = 220
024400170224     C                   EVAL      VABCTR = 001
024500080311     C                   EVAL      VABCTM = '7Q'
024600070208     C                   EVAL      VABCBO = '1'
024700020204     C* ... e poi verifico se sono stati passati come parametri
024800020204     C                   IF        vlrppt > *blanks
024900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
025000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025100020204     C                   EXSR      CHKNUM
025200020204     C                   IF        PiInt=*on
025300020204     C                   Z-ADD     PiVal         VABCCM
025400020204     C                   ENDIF
025500040301     C                   ENDIF
025600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025800020204     C                   EXSR      CHKNUM
025900020204     C                   IF        PiInt=*on
026000020204     C                   Z-ADD     PiVal         VABLNP
026100020204     C                   ENDIF
026200040301     C                   ENDIF
026300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026500020204     C                   EXSR      CHKNUM
026600020204     C                   IF        PiInt=*on
026700020204     C                   Z-ADD     PiVal         VABCTR
026800040301     C                   ENDIF
026900020204     C                   ENDIF
027000020204     C                   ENDIF
027100020204     C*
027200010330     C                   ENDSR
027300170224
027400170224
027500170224
027600010607     C*----------------------------------------------------*
027700121005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
027800010607     C*----------------------------------------------------*
027900010607     C     IMPVAB        BEGSR
028000010607     C*
028100010607     C                   SETOFF                                       3132
028200010607     C*
028300010607     C* Reperimento campi ALFA
028400010607     C*
028500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
028600070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
028700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028900020117     C* ==
029000080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
029100070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
029200070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
029300070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
029400130208     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
029500130208     C                   Z-ADD     1             jNAZ
029600130208     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
029700130208     C                   IF        %found
029800130208     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
029900130208     C                   ENDIF
030000070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
030100080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
030200080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
030300080311     C                   IF        %subst(vindta:210:1) = 'M'
030400080311     C                   EVAL      VABTIC='BM'
030500080311     C                   ENDIF
030600080311     C                   IF        %subst(vindta:210:1) = 'N'
030700080311     C                   EVAL      VABTIC='TM'
030800080311     C                   ENDIF
030900100527     C                   IF        %subst(vindta:210:1) = 'R'
031000100527     C                   EVAL      VABTIC='CM'
031100080311     C                   ENDIF
031200080311     C                   IF        %subst(vindta:227:4) = *blanks
031300080311     C                   EVAL      VABVCA='EUR'
031400080311     C                   ENDIF
031500080311     C                   IF        %subst(vindta:231:1) = 'I'
031600080311     C                   EVAL      VABTC1='A'
031700080311     C                   ENDIF
031800080311     C                   IF        %subst(vindta:231:1) = '2'
031900080311     C                   EVAL      VABTC2='P'
032000080311     C                   ENDIF
032100080311     C                   IF        %subst(vindta:231:1) = '1'
032200080311     C                   EVAL      VABTC2='S'
032300080311     C                   ENDIF
032400060510     C*
032500060510     C* Reperimento campi NUMERICI
032600060510     C                   MOVEL     DATCOR        VABAAS
032700060510     C                   MOVE      DATCOR        VABMGS
032800080311     C* DCR
032900080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
033000080311     C                   EXSR      CHKNUM
033100080311     C                   IF        PiInt=*on
033200080311     C                   Z-ADD     PiVal         VABDCR
033300080311     C                   ELSE
033400100527     C                   Z-ADD     *zeros        VABDCR
033500080311     C                   EVAL      vinmsg = %trimr(vinmsg)
033600080311     C                             + ' ' + 'VABDCR'
033700080311     C                   ENDIF
033800060710     C* NSP/RMN
033900080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
034000060510     C                   EXSR      CHKNUM
034100060510     C                   IF        PiInt=*on
034200060510     C                   Z-ADD     PiVal         VABNSP
034300060510     C                   Z-ADD     PiVal         VABRMN
034400060404     C                   ELSE
034500070502     C                   SETON                                        31
034600060510     C                   Z-ADD     *zeros        VABNSP
034700060510     C                   Z-ADD     1             VABRMN
034800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
034900060510     C                             + ' ' + 'VABNSP VABRMN'
035000060404     C                   ENDIF
035100040420     C* CAD
035200130208     C                   EVAL      VABCAD=%trim(%subst(vindta:145:9))
035300130208     C****               EVAL      PiStr=%trim(%subst(vindta:145:9))
035400130208     C****               EXSR      CHKNUM
035500130208     C****               IF        PiInt=*on
035600130208     C****               Z-ADD     PiVal         Num5_0
035700130208     C****               MOVEL(p)  Num5_0        VABCAD
035800130208     C****               ELSE
035900130208     C****               SETON                                        32
036000130208     C****               EVAL      VABCAD = *zeros
036100130208     C****               EVAL      vinmsg = %trimr(vinmsg)
036200130208     C****                         + ' ' + 'VABCAD'
036300130208     C****               ENDIF
036400070502     C* Reperisco la provincia dal CAP e dalla località
036500170223     C                   IF        VABNZD  = *blanks AND
036600170223     C                             VABCAD <> *blanks AND
036700070502     C                             VABPRD  = *blanks
036800070502     C                   CLEAR                   TISI95DS
036900070502     C                   EVAL      I95TCN = '3'
037000070502     C                   Z-ADD     datcor        I95DAT
037100070502     C                   EVAL      I95CAP = VABCAD
037200070502     C                   EVAL      I95LOC = VABLOD
037300070502     C                   EVAL      I95NAR = VABNZD
037400070502     C                   CALL      'TISI95R'
037500070502     C                   PARM                    TISI95DS
037600070502     C                   EVAL      VABPRD = O95PRV
037700070502     C                   ENDIF
037800040420     C* NCL
037900070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
038000010607     C                   EXSR      CHKNUM
038100010607     C                   IF        PiInt=*on
038200070606     C                   Z-ADD     PiVal         VABNCL
038300010607     C                   ELSE
038400010607     C                   SETON                                        32
038500010607     C                   Z-ADD     *zeros        VABNCL
038600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038700010607     C                             + ' ' + 'VABNCL'
038800010607     C                   ENDIF
038900040420     C* PKB
039000070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
039100070208     C                   EVAL      PiDecChr = '.'
039200010607     C                   EXSR      CHKNUM
039300010607     C                   IF        PiNum=*on
039400070925     C                   Z-ADD(H)  PiVal         VABPKB
039500010607     C                   ELSE
039600010607     C                   SETON                                        32
039700010607     C                   Z-ADD     *zeros        VABPKB
039800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039900010607     C                             + ' ' + 'VABPKB'
040000010607     C                   ENDIF
040100070925     C* VLB
040200070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
040300070925     C                   EVAL      PiDecChr = '.'
040400070925     C                   EXSR      CHKNUM
040500070925     C                   IF        PiNum=*on
040600070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
040700070925     C                   Z-ADD     PiVal         VABVLB
040800070925     C                   ELSE
040900070925     C                   SETON                                        32
041000070925     C                   Z-ADD     *zeros        VABVLB
041100070925     C                   EVAL      vinmsg = %trimr(vinmsg)
041200070925     C                             + ' ' + 'VABVLB'
041300070925     C                   ENDIF
041400080311     C* CAS
041500080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
041600080311     C                             %subst(vindta:196:14) <> *zeros
041700080311     C                   EVAL      FlgCAS = '1'
041800080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
041900080311     C                   EVAL      PiDecChr = '.'
042000080311     C                   EXSR      CHKNUM
042100080311     C                   IF        PiNum=*on
042200080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
042300080311     C                   Z-ADD     PiVal         VABCAS
042400080311     C                   ELSE
042500080311     C                   SETON                                        32
042600120123     C                   Z-ADD     *zeros        VABCAS
042700080311     C                   EVAL      vinmsg = %trimr(vinmsg)
042800080311     C                             + ' ' + 'VABCAS'
042900080311     C                   ENDIF
043000080311     C                   ENDIF
043100020322     C*
043200020322     C* Considerazioni finali su CBO/CAS
043300050908     C                   IF        FlgCAS = '1'
043400040713     C                   IF        VABCBO = '1'
043500020322     C                   EVAL      VABCBO = '4'
043600040713     C                   ENDIF
043700040713     C                   IF        VABCBO = '2'
043800040713     C                   EVAL      VABCBO = '6'
043900040713     C                   ENDIF
044000020322     C                   ENDIF
044100020204     C*
044200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044300020204     C                   EXSR      CHKIMPDIV
044400020204     C*
044500010607     C                   ENDSR
044600010607     C*----------------------------------------------------*
044700010601
044800170224
044900020204
045000020204     C*----------------------------------------------------*
045100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045200020204     C*----------------------------------------------------*
045300020204     C     CHKIMPDIV     BEGSR
045400020204     C*
045500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045600020204     C                   Z-ADD     *zeros        wrkDec            9 9
045700020204     C*
045800020204     C* Come prima cosa effettuo considerazioni sulla divisa
045900020204     C                   IF        vabIAS > *zeros
046000020204     C                   IF        vabVAS <> 'EUR'
046100020204     C                   EVAL      vabVAS =  'ITL'
046200020204     C                   ENDIF
046300020204     C                   ENDIF
046400020204     C*
046500020204     C                   IF        vabCAS > *zeros
046600020204     C                   IF        vabVCA <> 'EUR'
046700020204     C                   EVAL      vabVCA =  'ITL'
046800020204     C                   ENDIF
046900020204     C                   ENDIF
047000020204     C*
047100020204     C                   IF        vabVMD > *zeros
047200020321     C                   IF        vabVAD <> 'EUR'
047300020204     C                   EVAL      vabVAD =  'ITL'
047400020204     C                   ENDIF
047500020204     C                   ENDIF
047600020204     C*
047700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047800020204     C                   Z-ADD     vabIAS        wrkDec
047900020204     C                   IF        wrkDec > *zeros
048000020204     C                   IF        vabVAS = 'ITL'
048100020204     C                   EVAL      vabIAS = *zeros
048200020204     C                   ENDIF
048300020204     C                   ENDIF
048400020204     C*
048500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048600020204     C                   Z-ADD     vabCAS        wrkDec
048700020204     C                   IF        wrkDec > *zeros
048800020204     C                   IF        vabVCA = 'ITL'
048900020204     C                   EVAL      vabCAS = *zeros
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049400020204     C                   Z-ADD     vabVMD        wrkDec
049500020204     C                   IF        wrkDec > *zeros
049600020204     C                   IF        vabVAD = 'ITL'
049700020204     C                   EVAL      vabVMD = *zeros
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C                   ENDSR
050200020204     C***
050300020204
050400010330
050500010330
050600010330     C*----------------------------------------------------*
050700010330     C*  CONTROLLO NUMERICITA' CAMPI
050800010330     C*----------------------------------------------------*
050900010330     C     CHKNUM        BEGSR
051000010330     C*
051100010606     C                   IF        PiDecChr = *blanks
051200070502     C                   EVAL      PiDecChr = '.'
051300010606     C                   ENDIF
051400010606     C*
051500010606     C                   CALL(e)   'ISNUMERIC'
051600010330     C                   PARM                    PiStr            30
051700010606     C                   PARM                    PiDecChr          1
051800010330     C                   PARM      *ZEROS        PiVal            30 9
051900010330     C                   PARM      '0'           PiInt             1
052000010330     C                   PARM      '0'           PiNum             1
052100010330     C                   IF        %error
052200010606     C                   EVAL      PiNum=*off
052300010330     C                   ENDIF
052400010330     C*
052500010330     C                   ENDSR
052600010330     C***
052700010330
052800010601
052900010601
053000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053100010601     C     repfil        BEGSR
053200010601     C*
053300010601     C                   if        invfil = *zeros and
053400010601     C                             depfil > *zeros and
053500010629     C                             (vinflg = *blanks or
053600010629     C                              vinflg = *zeros)
053700010601     C
053800010601     C                   eval      invfil = depfil
053900010601     C                   endif
054000010601     C*
054100010601     C                   if        depfil <> invfil and
054200010601     C                             invfil > *zeros
054300010601     C                   eval      flgMulti = '1'
054400010601     C                   if        vinflg = *blanks
054500010601     C                   add       1             cntNonEl
054600010601     C                   endif
054700010601     C                   endif
054800010601     C*
054900010601     C                   if        vinflg = '2'
055000010601     C                   eval      flgStato = '2'
055100010601     C                   endif
055200010601     C*
055300010601     C                   ENDSR
055400010601     C***
055500010330
055600010330
055700010330
055800990920      /TITLE Invio dei dati al punto operativo.
055900000613     C     invio         BEGSR
056000990920     C*
056100990920     C                   reset                   dscmz
056200010601     C                   move      invfil        cmzdst
056300990920     C                   eval      cmzfld = vlrfou
056400990920     C                   eval      cmzmbd = vlrhdl
056500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056600000710     C                   if        prmfir = *blanks
056700121005     C                   eval      cmzfla = 'EDIVAB0F'
056800121005     C                   eval      cmzmba = 'EDIVAB0F'
056900000710     C                   else
057000000710     C                   eval      cmzfla = prmfir
057100000710     C                   eval      cmzmba = prmfir
057200000710     C                   endif
057300990920     C                   eval      cmznrr = *zeros
057400990920     C                   move      §ctrok        cmznrr
057500021018     C                   eval      cmzlba = vlrfl1
057600990920     C                   call(e)   'TIS711C'
057700990920     C                   parm                    dscmz
057800990921     C                   parm      *blanks       esito
057900990920     C                   if        %error
058000990920     C                             or cmzerr = '1'
058100990921     C                             or esito  = '1'
058200000710     C                   eval      wrkesito = '3'
058300990920     C                   endif
058400990920     C*
058500000613     C                   ENDSR
058600170224
058700170224
058800130208
058900130208     C*--------------------------------------------------------
059000130208     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
059100130208     C*--------------------------------------------------------
059200130208     C     CARTAB        BEGSR
059300130208     C*
059400130208     C* TABELLA '15' - NAZIONI
059500130208     C                   clear                   skNAZISO
059600130208     C                   clear                   skNAZBAR
059700130208     C                   eval      tblKUT = 1
059800130208     C                   eval      tblCOD = '15'
059900130208     C     KEYtabP       setll     tabel00f
060000130208     C     KEYtabP       reade     tabel00f
060100130208     C                   dow       not %eof(tabel00f)
060200130208     C                   if        tblFLG = *blanks
060300130208     C                   movel(p)  tblUNI        ds15
060400130208     C                   add       1             jNAZ
060500130208     C                   eval      skNAZBAR(jNAZ) = tblKEY
060600130208     C                   eval      skNAZISO(jNAZ) = §15COD
060700130208     C                   endif
060800130208     C     KEYtabP       reade     tabel00f
060900130208     C                   enddo
061000130208     C*
061100130208     C                   ENDSR
061200130208     C***
061300010601
061400010601
061500010601
061600010601      /TITLE Invio dei dati al punto operativo.
061700010601     C     opeini        BEGSR
061800010601     C*
061900010601     C* Inizializzo flag e contatori operativi
062000010601     C                   movel     '0'           flgGiro           1
062100010601     C                   movel     '0'           flgMulti          1
062200010601     C                   movel     '1'           flgStato          1
062300010615     C                   movel     '0'           flgOk             1
062400010601     C                   z-add     *zeros        cntNonEl         10 0
062500010601     C                   z-add     *zeros        depfil            3 0
062600010601     C                   z-add     *zeros        invfil            3 0
062700010601     C*
062800010601     C                   ENDSR
062900010601     C***
063000010601
063100010330
063200010330
063300000613     C     *inzsr        BEGSR
063400990910     C*
063500990910     C     *entry        plist
063600990920     C                   parm                    tivlrds
063700990921     C                   parm      wrkesito      esito
063800000724     C                   parm                    prmlit
063900000710     C                   parm                    prmfir
064000170224     C*
064100170224     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
064200170224     C                   move(p)   vlrMSG        wNomeFile
064300010330     C*
064400010330     C* CALCOLA LA DATA CORRENTE
064500170224     C                   time                    wn14             14 0
064600170224     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064700170224     C                   movel     wn14          oracor            6 0          *ORA
064800170224     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
064900170224     C                   eval      datcor = %dec(%date() : *iso)
065000130208     C*
065100130208     C* Chiave su TABEL00F - parziale
065200130208     C     KEYtabP       KLIST
065300130208     C                   KFLD                    tblKUT
065400130208     C                   KFLD                    tblCOD
065500000613     C*
065600000613     C                   ENDSR
065700000613     C***
