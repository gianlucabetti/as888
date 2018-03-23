000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500070502     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D wrkdata         s               d
002100010330     D wrkora          s               t
002200010330     D rrnum           s              6  0 INZ(*zeros)
002300010330     D*------------------
002400130521
002500080611
002600080611     D*------------------
002700080611     D* LINKING A DEFINIZIONI ESTERNE
002800080611     D*------------------
002900080611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003000130521     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003100010330
003200010330
003300990915     C                   time                    wrkdata
003400990915     C                   time                    wrkora
003500000913     C                   reset                   rrnum
003600990921     C                   reset                   esito
003700990921     C                   reset                   wrkesito
003800010601     C*
003900010601     C                   exsr      opeini
004000010605     C                   exsr      rwvab
004100070502     C*
004200070502     C* Effettuo lancio TISI95 solo x chiusura
004300070502     C                   CLEAR                   TISI95DS
004400070502     C                   EVAL      I95TLA = 'C'
004500070502     C                   CALL      'TISI95R'
004600070502     C                   PARM                    TISI95DS
004700010601     C*
004800010601     C                   seton                                        lr
004900000613
005000010601
005100010601
005200010330     C*--------------------------------------------------------
005300050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
005400010330     C*--------------------------------------------------------
005500010605     C     RWVAB         BEGSR
005600050408     C*
005700010330     C                   if        not %open(tivin00r)
005800010330     C                   open      tivin00r
005900010330     C                   endif
006000050408     C                   if        not %open(fivabwwr)
006100050408     C                   open      fivabwwr
006200010330     C                   endif
006300010330     C*
006400010604     C                   clear                   §CTROK
006500010604     C                   clear                   §CTRMO
006600010604     C                   clear                   §CTRNO
006700010330     C*
006800010330     C                   DO        *HIVAL
006900010330     C*
007000010330     C                   READ      tivin00r                               70
007100010618     C*
007200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
007300010618     C                   if        vinflg = '1'
007400010618     C                   eval      flgOk = '1'
007500010618     C                   endif
007600010618     C*
007700010330     C                   if        vindta > *blanks
007800010330     C                   add       1             rrnum
007900010330     C*
008000010601     C                   if        *in70 = *off and
008100010330     C                             (vinflg = *blanks
008200010330     C                              or vinflg = '0'
008300010330     C                              or vinflg = '2')
008400010330     C*
008500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
008600010711     C                   if        vinflg = *blanks or vinflg = '0'
008700010711     C                   clear                   vinmsg
008800010711     C                   endif
008900070601     C*
009000070601     C                   exsr      inzvar
009100070601     C                   exsr      defcam
009200070601     C                   exsr      impvab
009300010601     C*
009400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
009500010605     C                   eval      depfil = VABLNP
009600010601     C                   exsr      repfil
009700010601     C                   if        depfil = invfil
009800021025     C                   if        vlrpoi = 999
009900021025     C                   MOVE(P)   invfil        VABFGS
010000021025     C                   else
010100021025     C                   MOVE(P)   vlrpoi        VABFGS
010200021025     C                   endif
010300010601     C*
010400010604     C* Ebbene...
010500010604     C*
010600010604     C  N31              ADD       1             §CTROK            7 0
010700010604     C   32              ADD       1             §CTRMO            7 0
010800010604     C   31              ADD       1             §CTRNO            7 0
010900071003     C                   exsr      wrivab                                       => scrivo VAB
011000010604     C*
011100020722     C                   endif
011200020722     C*
011300010604     C                   if        *in31 = *off and
011400010604     C                             *in32 = *off
011500010604     C                   eval      vinflg = '1'
011600010604     C                   else
011700010604     C                   eval      vinflg = '2'
011800010604     C                   endif
011900010604     C                   endif
012000010604     C*
012100010330     C                   else
012200010330     C                   eval      vinflg = '1'
012300010330     C                   endif
012400010601     C*
012500010601     C  N70              update    tivin000
012600010330     C*
012700010330     C  N70              ENDdo
012800010601     C*
012900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
013000010601     C                   if        cntNonEl = *zeros or
013100010601     C                             flgMulti = '0'
013200010330     C* Se non ci sono record con errori ...
013300010601     C                   if        §ctrno = 0 and
013400010604     C                             §ctrmo = 0 and
013500010601     C                             flgStato <> '2'
013600010330     C* ... restituisco esito OK.
013700010330     C                   eval      wrkesito = '0'
013800010330     C                   else
013900010330     C                   if        §ctrok > 0
014000010330     C                   eval      wrkesito = '1'
014100010330     C                   else
014200010615     C                   if        flgOk = '0'
014300010615     C                   eval      wrkesito = '2'
014400010615     C                   else
014500010615     C                   eval      wrkesito = '6'
014600010615     C                   endif
014700010330     C                   endif
014800010330     C                   endif
014900010601     C                   else
015000010601     C                   eval      wrkesito = '9'
015100010601     C                   endif
015200010330     C*
015300010330     C                   if        %open(tivin00r)
015400010330     C                   close     tivin00r
015500010330     C                   endif
015600050408     C                   if        %open(fivabwwr)
015700050408     C                   close     fivabwwr
015800010330     C                   endif
015900010601     C*
016000010601     C                   if        vlrpoi <> 999
016100010601     C                   eval      invfil = vlrpoi
016200010601     C                   endif
016300010330     C*
016400010330     C                   if        §ctrok > 0
016500010601     C                             and invfil > *zeros
016600010330     C                   exsr      invio
016700010330     C                   endif
016800010330     C*
016900010330     C                   ENDSR
017000010330     C***
017100070601
017200130521
017300130521
017400070601     C*----------------------------------------------------*
017500070601     C*  SCARICAMENTO BUFFER RECORDS VAB
017600070601     C*----------------------------------------------------*
017700070601     C     WRIVAB        BEGSR
017800080321     C*
017900070601     C  N31              WRITE     FIVAB000
018000070601     C*
018100070601     C                   ENDSR
018200010601
018300010601
018400010601
018500010330     C*----------------------------------------------------*
018600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018700010330     C*----------------------------------------------------*
018800010330     C     INZVAR        BEGSR
018900010330     C*
019000020204     C                   Z-ADD     *zeros        Num5_0            5 0
019100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
019200020322     C                   MOVEL     '0'           FlgCAS            1
019300010330     C*
019400010330     C                   ENDSR
019500010330     C*----------------------------------------------------*
019600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
019700010330     C*----------------------------------------------------*
019800010330     C     DEFCAM        BEGSR
019900010330     C*
020000020204     C* Inizializzo il buffer del record da scrivere
020100050408     C                   CLEAR                   FIVAB000
020200020204     C* Imposto i valori di default...
020300130521     C                   EVAL      VABCCM = 0436086
020400130521     C                   EVAL      VABLNP = 043
020500130521     C                   EVAL      VABCTR = 001
020600070208     C                   EVAL      VABCBO = '1'
020700020204     C* ... e poi verifico se sono stati passati come parametri
020800020204     C                   IF        vlrppt > *blanks
020900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
021000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021100020204     C                   EXSR      CHKNUM
021200020204     C                   IF        PiInt=*on
021300020204     C                   Z-ADD     PiVal         VABCCM
021400020204     C                   ENDIF
021500040301     C                   ENDIF
021600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
021700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021800020204     C                   EXSR      CHKNUM
021900020204     C                   IF        PiInt=*on
022000020204     C                   Z-ADD     PiVal         VABLNP
022100020204     C                   ENDIF
022200040301     C                   ENDIF
022300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
022400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022500020204     C                   EXSR      CHKNUM
022600020204     C                   IF        PiInt=*on
022700020204     C                   Z-ADD     PiVal         VABCTR
022800040301     C                   ENDIF
022900020204     C                   ENDIF
023000020204     C                   ENDIF
023100020204     C*
023200010330     C                   ENDSR
023300010607     C*----------------------------------------------------*
023400050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023500010607     C*----------------------------------------------------*
023600010607     C     IMPVAB        BEGSR
023700010607     C*
023800010607     C                   SETOFF                                       3132
023900010607     C*
024000010607     C* Reperimento campi ALFA
024100081020     C                   EVAL      VABRSD=%trim(%subst(vindta:15:45))
024200081020     C                   EVAL      VABIND=%trim(%subst(vindta:60:35))
024300081020     C                   EVAL      VABLOD=%trim(%subst(vindta:95:20))
024400081020     C                   EVAL      VABPRD=%trim(%subst(vindta:115:2))
024500130521     C                   EVAL      VABRMA=%trim(%subst(vindta:115:7))+
024600130521     C                                    %trim(%subst(vindta:142:8))
024700081020     C                   MOVEL     *blanks       wNOTE            70
024800081020     C                   EVAL      wNOTE=%trim(%subst(vindta:154:40))+' '+
024900081020     C                                   %trim(%subst(vindta:240:30))
025000081020     C                   IF        %subst(vindta:228:1) = 'F'
025100081020     C                   EVAL      VABCBO='1'
025200081020     C                   ENDIF
025300081020     C                   IF        %subst(vindta:228:1) = 'A'
025400081020     C                   EVAL      VABCBO='2'
025500081020     C                   ENDIF
025600080408     C*
025700080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
025800080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025900080408     C     '@':'A'       XLATE     VABRSD        VABRSD
026000080408     C* ==
026100060510     C*
026200060510     C* Reperimento campi NUMERICI
026300060510     C                   MOVEL     DATCOR        VABAAS
026400060510     C                   MOVE      DATCOR        VABMGS
026500080410     C* NSP
026600081020     C                   EVAL      PiStr=%trim(%subst(vindta:1:8))
026700060510     C                   EXSR      CHKNUM
026800060510     C                   IF        PiInt=*on
026900060510     C                   Z-ADD     PiVal         VABNSP
027000060404     C                   ELSE
027100070502     C                   SETON                                        31
027200060510     C                   Z-ADD     *zeros        VABNSP
027300060404     C                   EVAL      vinmsg = %trimr(vinmsg)
027400080410     C                             + ' ' + 'VABNSP'
027500060404     C                   ENDIF
027600080410     C* RMN
027700081020     C                   EVAL      PiStr=%trim(%subst(vindta:1:8))
027800080410     C                   EXSR      CHKNUM
027900080410     C                   IF        PiInt=*on
028000080410     C                   Z-ADD     PiVal         VABRMN
028100080410     C                   ELSE
028200080410     C                   SETON                                        31
028300080410     C                   Z-ADD     1             VABRMN
028400080410     C                   EVAL      vinmsg = %trimr(vinmsg)
028500080410     C                             + ' ' + 'VABRMN'
028600080410     C                   ENDIF
028700081020     C* CAD
028800081020     C                   EVAL      PiStr=%trim(%subst(vindta:229:5))
028900080408     C                   EXSR      CHKNUM
029000080408     C                   IF        PiInt=*on
029100080408     C                   Z-ADD     PiVal         Num5_0
029200080408     C                   MOVEL(p)  Num5_0        VABCAD
029300080408     C                   ELSE
029400080408     C                   SETON                                        32
029500080408     C                   EVAL      VABCAD = *zeros
029600080408     C                   EVAL      vinmsg = %trimr(vinmsg)
029700080408     C                             + ' ' + 'VABCAD'
029800080408     C                   ENDIF
029900080408     C* Reperisco la provincia dal CAP e dalla località
030000080408     C                   IF        VABCAD <> *blanks AND
030100080408     C                             VABPRD  = *blanks
030200080408     C                   CLEAR                   TISI95DS
030300080408     C                   EVAL      I95TCN = '3'
030400080408     C                   Z-ADD     datcor        I95DAT
030500080408     C                   EVAL      I95CAP = VABCAD
030600080408     C                   EVAL      I95LOC = VABLOD
030700080408     C                   EVAL      I95NAR = VABNZD
030800080408     C                   CALL      'TISI95R'
030900080408     C                   PARM                    TISI95DS
031000080408     C                   EVAL      VABPRD = O95PRV
031100080408     C                   ENDIF
031200080410     C* NCL
031300081020     C                   EVAL      PiStr=%trim(%subst(vindta:117:5))
031400080410     C                   EXSR      CHKNUM
031500080410     C                   IF        PiInt=*on
031600080410     C                   Z-ADD     PiVal         VABNCL
031700080410     C                   ELSE
031800080410     C                   SETON                                        32
031900080410     C                   Z-ADD     *zeros        VABNCL
032000080410     C                   EVAL      vinmsg = %trimr(vinmsg)
032100080410     C                             + ' ' + 'VABNCL'
032200080410     C                   ENDIF
032300071029     C* PKB
032400081020     C                   EVAL      PiStr=%trim(%subst(vindta:122:7))
032500010607     C                   EXSR      CHKNUM
032600010607     C                   IF        PiNum=*on
032700081020     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
032800080903     C                   Z-ADD(H)  PiVal         VABPKB
032900010607     C                   ELSE
033000010607     C                   SETON                                        32
033100010607     C                   Z-ADD     *zeros        VABPKB
033200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033300010607     C                             + ' ' + 'VABPKB'
033400010607     C                   ENDIF
033500081020     C* VLB
033600081020     C                   EVAL      PiStr=%trim(%subst(vindta:129:4))
033700081020     C                   EXSR      CHKNUM
033800081020     C                   IF        PiNum=*on
033900081020     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
034000081020     C                   Z-ADD(H)  PiVal         VABVLB
034100081020     C                   ELSE
034200081020     C                   SETON                                        32
034300081020     C                   Z-ADD     *zeros        VABVLB
034400081020     C                   EVAL      vinmsg = %trimr(vinmsg)
034500081020     C                             + ' ' + 'VABVLB'
034600081020     C                   ENDIF
034700080320     C* CAS
034800081020     C                   IF        %trim(%subst(vindta:133:8))<>*all'0' AND
034900081020     C                             %trim(%subst(vindta:133:8))<>*blanks
035000080408     C                   EVAL      FlgCAS = '1'
035100080903     C                   EVAL      VABVCA='EUR'
035200081020     C                   EVAL      PiStr=%trim(%subst(vindta:133:8))
035300080320     C                   EXSR      CHKNUM
035400080320     C                   IF        PiNum=*on
035500081020     C                   Z-ADD     PiVal         VABCAS
035600080320     C                   ELSE
035700080320     C                   SETON                                        32
035800080320     C                   Z-ADD     *zeros        VABCAS
035900080320     C                   EVAL      vinmsg = %trimr(vinmsg)
036000080320     C                             + ' ' + 'VABCAS'
036100080408     C                   ENDIF
036200080320     C                   ENDIF
036300020322     C*
036400020322     C* Considerazioni finali su CBO/CAS
036500050908     C                   IF        FlgCAS = '1'
036600040713     C                   IF        VABCBO = '1'
036700020322     C                   EVAL      VABCBO = '4'
036800040713     C                   ENDIF
036900040713     C                   IF        VABCBO = '2'
037000040713     C                   EVAL      VABCBO = '6'
037100040713     C                   ENDIF
037200020322     C                   ENDIF
037300020204     C*
037400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037500020204     C                   EXSR      CHKIMPDIV
037600020204     C*
037700010607     C                   ENDSR
037800010607     C*----------------------------------------------------*
037900010601
038000020204
038100020204     C*----------------------------------------------------*
038200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038300020204     C*----------------------------------------------------*
038400020204     C     CHKIMPDIV     BEGSR
038500020204     C*
038600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
038700020204     C                   Z-ADD     *zeros        wrkDec            9 9
038800020204     C*
038900020204     C* Come prima cosa effettuo considerazioni sulla divisa
039000020204     C                   IF        vabIAS > *zeros
039100020204     C                   IF        vabVAS <> 'EUR'
039200020204     C                   EVAL      vabVAS =  'ITL'
039300020204     C                   ENDIF
039400020204     C                   ENDIF
039500020204     C*
039600020204     C                   IF        vabCAS > *zeros
039700020204     C                   IF        vabVCA <> 'EUR'
039800020204     C                   EVAL      vabVCA =  'ITL'
039900020204     C                   ENDIF
040000020204     C                   ENDIF
040100020204     C*
040200020204     C                   IF        vabVMD > *zeros
040300020321     C                   IF        vabVAD <> 'EUR'
040400020204     C                   EVAL      vabVAD =  'ITL'
040500020204     C                   ENDIF
040600020204     C                   ENDIF
040700020204     C*
040800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
040900020204     C                   Z-ADD     vabIAS        wrkDec
041000020204     C                   IF        wrkDec > *zeros
041100020204     C                   IF        vabVAS = 'ITL'
041200020204     C                   EVAL      vabIAS = *zeros
041300020204     C                   ENDIF
041400020204     C                   ENDIF
041500020204     C*
041600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
041700020204     C                   Z-ADD     vabCAS        wrkDec
041800020204     C                   IF        wrkDec > *zeros
041900020204     C                   IF        vabVCA = 'ITL'
042000020204     C                   EVAL      vabCAS = *zeros
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
042500020204     C                   Z-ADD     vabVMD        wrkDec
042600020204     C                   IF        wrkDec > *zeros
042700020204     C                   IF        vabVAD = 'ITL'
042800020204     C                   EVAL      vabVMD = *zeros
042900020204     C                   ENDIF
043000020204     C                   ENDIF
043100020204     C*
043200020204     C                   ENDSR
043300020204     C***
043400020204
043500010330
043600010330
043700010330     C*----------------------------------------------------*
043800010330     C*  CONTROLLO NUMERICITA' CAMPI
043900010330     C*----------------------------------------------------*
044000010330     C     CHKNUM        BEGSR
044100010330     C*
044200010606     C                   IF        PiDecChr = *blanks
044300080611     C                   EVAL      PiDecChr = ','
044400010606     C                   ENDIF
044500080611     C*
044600080611     C                   callp     UBISNUM_Check(PiStr
044700080611     C                                          :PiDecChr
044800080611     C                                          :PiVal
044900080611     C                                          :PiNum
045000080611     C                                          :PiInt)
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
055000130521     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
055100130521     C                   eval      datcor = %dec(%date() : *ISO)
055200000613     C*
055300000613     C                   ENDSR
055400000613     C***
055500990908
