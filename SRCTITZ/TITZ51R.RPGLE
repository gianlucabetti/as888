000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
000600010330     D*----------------------------------------------------
000700010330     D* DICHIARAZIOINE VARIABILI DI WRK
000800010330     D*----------------------------------------------------
000900010330     D dscmz         e ds                  inz
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300070502     D tisi95ds      e ds
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
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300010601     C                   exsr      opeini
003400010605     C                   exsr      rwvab
003500070502     C*
003600070502     C* Effettuo lancio TISI95 solo x chiusura
003700070502     C                   CLEAR                   TISI95DS
003800070502     C                   EVAL      I95TLA = 'C'
003900070502     C                   CALL      'TISI95R'
004000070502     C                   PARM                    TISI95DS
004100010601     C*
004200010601     C                   seton                                        lr
004300010601
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
007300120525
007400010330     C*--------------------------------------------------------
007500050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
007600010330     C*--------------------------------------------------------
007700010605     C     RWVAB         BEGSR
007800050408     C*
007900010330     C                   if        not %open(tivin00r)
008000010330     C                   open      tivin00r
008100010330     C                   endif
008200050408     C                   if        not %open(fivabwwr)
008300050408     C                   open      fivabwwr
008400010330     C                   endif
008500010330     C*
008600010604     C                   clear                   低TROK
008700010604     C                   clear                   低TRMO
008800010604     C                   clear                   低TRNO
008900010330     C*
009000010330     C                   DO        *HIVAL
009100010330     C*
009200010330     C                   READ      tivin00r                               70
009300010618     C*
009400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009500010618     C                   if        vinflg = '1'
009600010618     C                   eval      flgOk = '1'
009700010618     C                   endif
009800010618     C*
009900010330     C                   if        vindta > *blanks
010000010330     C                   add       1             rrnum
010100010330     C*
010200010601     C                   if        *in70 = *off and
010300010330     C                             (vinflg = *blanks
010400010330     C                              or vinflg = '0'
010500010330     C                              or vinflg = '2')
010600010330     C*
010700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010800010711     C                   if        vinflg = *blanks or vinflg = '0'
010900010711     C                   clear                   vinmsg
011000010711     C                   endif
011100070601     C*
011200070601     C                   exsr      inzvar
011300070601     C                   exsr      defcam
011400070601     C                   exsr      impvab
011500010601     C*
011600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011700010605     C                   eval      depfil = VABLNP
011800010601     C                   exsr      repfil
011900010601     C                   if        depfil = invfil
012000021025     C                   if        vlrpoi = 999
012100021025     C                   MOVE(P)   invfil        VABFGS
012200021025     C                   else
012300021025     C                   MOVE(P)   vlrpoi        VABFGS
012400021025     C                   endif
012500010601     C*
012600010601     C                   exsr      PREELA
012700010601     C*
012800010604     C* Ebbene...
012900010604     C*
013000010604     C  N31              ADD       1             低TROK            7 0
013100010604     C   32              ADD       1             低TRMO            7 0
013200010604     C   31              ADD       1             低TRNO            7 0
013300071003     C                   exsr      wrivab                                       => scrivo VAB
013400010604     C*
013500020722     C                   endif
013600020722     C*
013700010604     C                   if        *in31 = *off and
013800010604     C                             *in32 = *off
013900010604     C                   eval      vinflg = '1'
014000010604     C                   else
014100010604     C                   eval      vinflg = '2'
014200010604     C                   endif
014300010604     C                   endif
014400010604     C*
014500010330     C                   else
014600010330     C                   eval      vinflg = '1'
014700010330     C                   endif
014800010601     C*
014900010601     C  N70              update    tivin000
015000010330     C*
015100010330     C  N70              ENDdo
015200010601     C*
015300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015400010601     C                   if        cntNonEl = *zeros or
015500010601     C                             flgMulti = '0'
015600010330     C* Se non ci sono record con errori ...
015700010601     C                   if        呃trno = 0 and
015800010604     C                             呃trmo = 0 and
015900010601     C                             flgStato <> '2'
016000010330     C* ... restituisco esito OK.
016100010330     C                   eval      wrkesito = '0'
016200010330     C                   else
016300010330     C                   if        呃trok > 0
016400010330     C                   eval      wrkesito = '1'
016500010330     C                   else
016600010615     C                   if        flgOk = '0'
016700010615     C                   eval      wrkesito = '2'
016800010615     C                   else
016900010615     C                   eval      wrkesito = '6'
017000010615     C                   endif
017100010330     C                   endif
017200010330     C                   endif
017300010601     C                   else
017400010601     C                   eval      wrkesito = '9'
017500010601     C                   endif
017600010330     C*
017700010330     C                   if        %open(tivin00r)
017800010330     C                   close     tivin00r
017900010330     C                   endif
018000050408     C                   if        %open(fivabwwr)
018100050408     C                   close     fivabwwr
018200010330     C                   endif
018300010601     C*
018400010601     C                   if        vlrpoi <> 999
018500010601     C                   eval      invfil = vlrpoi
018600010601     C                   endif
018700010330     C*
018800010330     C                   if        呃trok > 0
018900010601     C                             and invfil > *zeros
019000010330     C                   exsr      invio
019100010330     C                   endif
019200010601     C*
019300010618     C                   if        flgGiro = '1'
019400010601     C                   exsr      endela
019500010618     C                   endif
019600010330     C*
019700010330     C                   ENDSR
019800010330     C***
019900070601
020000070601     C*----------------------------------------------------*
020100070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020200070601     C*----------------------------------------------------*
020300070601     C     WRIVAB        BEGSR
020400080321     C*
020500070601     C  N31              WRITE     FIVAB000
020600070601     C*
020700070601     C                   ENDSR
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
022700050408     C                   CLEAR                   FIVAB000
022800020204     C* Imposto i valori di default...
022900121024     C                   EVAL      VABCCM = 0941197
023000121024     C                   EVAL      VABLNP = 094
023100120528     C                   EVAL      VABCTR = 000
023200070208     C                   EVAL      VABCBO = '1'
023300080410     C                   EVAL      VABCTM = '7Q'
023400121024     C                   EVAL      VABTSP = 'C'
023500020204     C* ... e poi verifico se sono stati passati come parametri
023600020204     C                   IF        vlrppt > *blanks
023700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023900020204     C                   EXSR      CHKNUM
024000020204     C                   IF        PiInt=*on
024100020204     C                   Z-ADD     PiVal         VABCCM
024200020204     C                   ENDIF
024300040301     C                   ENDIF
024400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024600020204     C                   EXSR      CHKNUM
024700020204     C                   IF        PiInt=*on
024800020204     C                   Z-ADD     PiVal         VABLNP
024900020204     C                   ENDIF
025000040301     C                   ENDIF
025100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025300020204     C                   EXSR      CHKNUM
025400020204     C                   IF        PiInt=*on
025500020204     C                   Z-ADD     PiVal         VABCTR
025600040301     C                   ENDIF
025700020204     C                   ENDIF
025800020204     C                   ENDIF
025900020204     C*
026000010330     C                   ENDSR
026100010607     C*----------------------------------------------------*
026200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026300010607     C*----------------------------------------------------*
026400010607     C     IMPVAB        BEGSR
026500010607     C*
026600010607     C                   SETOFF                                       3132
026700010607     C*
026800010607     C* Reperimento campi ALFA
026900121024     C                   EVAL      VABRSD=%trim(%subst(vindta:3:30))
027000121024     C                   EVAL      VABIND=%trim(%subst(vindta:33:30))
027100121024     C                   EVAL      VABLOD=%trim(%subst(vindta:63:30))
027200121024     C                   EVAL      VABCAD=%trim(%subst(vindta:93:5))
027300121024     C                   EVAL      VABPRD=%trim(%subst(vindta:98:2))
027400121024     C                   EVAL      VABNOT=%trim(%subst(vindta:150:35))
027500121024     C                   EVAL      VABNT2=%trim(%subst(vindta:150+35:15))
027600121024     C                   EVAL      VABRMA=%trim(%subst(vindta:213:15))
027700121024     C*
027800120528     C* Reperisco la provincia dal CAP e dalla localit�
027900120528     C                   IF        VABCAD <> *blanks AND
028000120528     C                             VABPRD  = *blanks
028100120528     C                   CLEAR                   TISI95DS
028200120528     C                   EVAL      I95TCN = '3'
028300120528     C                   Z-ADD     datcor        I95DAT
028400120528     C                   EVAL      I95CAP = VABCAD
028500120528     C                   EVAL      I95LOC = VABLOD
028600120528     C                   EVAL      I95NAR = VABNZD
028700120528     C                   CALL      'TISI95R'
028800120528     C                   PARM                    TISI95DS
028900120528     C                   EVAL      VABPRD = O95PRV
029000120528     C                   ENDIF
029100080408     C*
029200080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
029300080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'� diventa A ==
029400080408     C     '@':'A'       XLATE     VABRSD        VABRSD
029500080408     C* ==
029600060510     C*
029700060510     C* Reperimento campi NUMERICI
029800060510     C                   MOVE      DATCOR        VABMGS
029900121024     C* AAS (deve essere preso da loro)
030000121024     C                   EVAL      PiStr=%trim(%subst(vindta:142:4))
030100121024     C                   EXSR      CHKNUM
030200121024     C                   IF        PiInt=*on
030300121024     C                   Z-ADD     PiVal         VABAAS
030400121024     C                   ELSE
030500121024     C                   SETON                                        31
030600121024     C                   Z-ADD     *zeros        VABAAS
030700121024     C                   EVAL      vinmsg = %trimr(vinmsg)
030800121024     C                             + ' ' + 'VABAAS'
030900121024     C                   ENDIF
031000121024     C* NSP
031100121024     C                   EVAL      PiStr=%trim(%subst(vindta:132:10))
031200060510     C                   EXSR      CHKNUM
031300060510     C                   IF        PiInt=*on
031400060510     C                   Z-ADD     PiVal         VABNSP
031500060404     C                   ELSE
031600070502     C                   SETON                                        31
031700060510     C                   Z-ADD     *zeros        VABNSP
031800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
031900080410     C                             + ' ' + 'VABNSP'
032000060404     C                   ENDIF
032100121024     C* RMN
032200121024     C                   EVAL      VABRMN = VABNSP
032300121024     C* NCL
032400121024     C                   EVAL      PiStr=%trim(%subst(vindta:243:2))
032500080410     C                   EXSR      CHKNUM
032600080410     C                   IF        PiInt=*on
032700080410     C                   Z-ADD     PiVal         VABNCL
032800080410     C                   ELSE
032900080410     C                   SETON                                        32
033000080410     C                   Z-ADD     *zeros        VABNCL
033100080410     C                   EVAL      vinmsg = %trimr(vinmsg)
033200080410     C                             + ' ' + 'VABNCL'
033300080410     C                   ENDIF
033400121024     C* VLB
033500121024     C                   EVAL      PiStr=%trim(%subst(vindta:104:8))
033600010607     C                   EXSR      CHKNUM
033700010607     C                   IF        PiNum=*on
033800121024     C                   EVAL      PiVal=PiVal/100000                           * gestisco 5 dec.
033900121024     C                   Z-ADD(H)  PiVal         VABVLB
034000010607     C                   ELSE
034100010607     C                   SETON                                        32
034200121024     C                   Z-ADD     *zeros        VABVLB
034300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034400121024     C                             + ' ' + 'VABVLB'
034500010607     C                   ENDIF
034600121024     C* PKB
034700121024     C                   EVAL      PiStr=%trim(%subst(vindta:112:8))
034800121024     C                   EXSR      CHKNUM
034900121024     C                   IF        PiNum=*on
035000121024     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
035100121024     C                   Z-ADD(H)  PiVal         VABPKB
035200121024     C                   ELSE
035300121024     C                   SETON                                        32
035400121024     C                   Z-ADD     *zeros        VABPKB
035500121024     C                   EVAL      vinmsg = %trimr(vinmsg)
035600121024     C                             + ' ' + 'VABPKB'
035700121024     C                   ENDIF
035800121024     C* CAS
035900120601     C                   EVAL      FlgCAS = '0'
036000121024     C                   IF        %subst(vindta:200:10)<>'0000000000'
036100080408     C                   EVAL      FlgCAS = '1'
036200080408     C                   EVAL      VABVCA = 'EUR'
036300121024     C                   EVAL      PiStr=%trim(%subst(vindta:200:10))
036400080320     C                   EXSR      CHKNUM
036500080320     C                   IF        PiNum=*on
036600120601     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
036700080410     C                   Z-ADD(H)  PiVal         VABCAS
036800080320     C                   ELSE
036900080320     C                   SETON                                        32
037000080320     C                   Z-ADD     *zeros        VABCAS
037100080320     C                   EVAL      vinmsg = %trimr(vinmsg)
037200080320     C                             + ' ' + 'VABCAS'
037300080408     C                   ENDIF
037400080320     C                   ENDIF
037500020322     C*
037600020322     C* Considerazioni finali su CBO/CAS
037700050908     C                   IF        FlgCAS = '1'
037800040713     C                   IF        VABCBO = '1'
037900020322     C                   EVAL      VABCBO = '4'
038000040713     C                   ENDIF
038100040713     C                   IF        VABCBO = '2'
038200040713     C                   EVAL      VABCBO = '6'
038300040713     C                   ENDIF
038400020322     C                   ENDIF
038500020204     C*
038600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038700020204     C                   EXSR      CHKIMPDIV
038800020204     C*
038900010607     C                   ENDSR
039000010607     C*----------------------------------------------------*
039100010601
039200020204
039300020204     C*----------------------------------------------------*
039400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039500020204     C*----------------------------------------------------*
039600020204     C     CHKIMPDIV     BEGSR
039700020204     C*
039800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilit�
039900020204     C                   Z-ADD     *zeros        wrkDec            9 9
040000020204     C*
040100020204     C* Come prima cosa effettuo considerazioni sulla divisa
040200020204     C                   IF        vabIAS > *zeros
040300020204     C                   IF        vabVAS <> 'EUR'
040400020204     C                   EVAL      vabVAS =  'ITL'
040500020204     C                   ENDIF
040600020204     C                   ENDIF
040700020204     C*
040800020204     C                   IF        vabCAS > *zeros
040900020204     C                   IF        vabVCA <> 'EUR'
041000020204     C                   EVAL      vabVCA =  'ITL'
041100020204     C                   ENDIF
041200020204     C                   ENDIF
041300020204     C*
041400020204     C                   IF        vabVMD > *zeros
041500020321     C                   IF        vabVAD <> 'EUR'
041600020204     C                   EVAL      vabVAD =  'ITL'
041700020204     C                   ENDIF
041800020204     C                   ENDIF
041900020204     C*
042000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042100020204     C                   Z-ADD     vabIAS        wrkDec
042200020204     C                   IF        wrkDec > *zeros
042300020204     C                   IF        vabVAS = 'ITL'
042400020204     C                   EVAL      vabIAS = *zeros
042500020204     C                   ENDIF
042600020204     C                   ENDIF
042700020204     C*
042800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
042900020204     C                   Z-ADD     vabCAS        wrkDec
043000020204     C                   IF        wrkDec > *zeros
043100020204     C                   IF        vabVCA = 'ITL'
043200020204     C                   EVAL      vabCAS = *zeros
043300020204     C                   ENDIF
043400020204     C                   ENDIF
043500020204     C*
043600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043700020204     C                   Z-ADD     vabVMD        wrkDec
043800020204     C                   IF        wrkDec > *zeros
043900020204     C                   IF        vabVAD = 'ITL'
044000020204     C                   EVAL      vabVMD = *zeros
044100020204     C                   ENDIF
044200020204     C                   ENDIF
044300020204     C*
044400020204     C                   ENDSR
044500020204     C***
044600020204
044700010330
044800010330
044900010330     C*----------------------------------------------------*
045000010330     C*  CONTROLLO NUMERICITA' CAMPI
045100010330     C*----------------------------------------------------*
045200010330     C     CHKNUM        BEGSR
045300010330     C*
045400010606     C                   IF        PiDecChr = *blanks
045500080409     C                   EVAL      PiDecChr = '.'
045600010606     C                   ENDIF
045700010606     C*
045800010606     C                   CALL(e)   'ISNUMERIC'
045900010330     C                   PARM                    PiStr            30
046000010606     C                   PARM                    PiDecChr          1
046100010330     C                   PARM      *ZEROS        PiVal            30 9
046200010330     C                   PARM      '0'           PiInt             1
046300010330     C                   PARM      '0'           PiNum             1
046400010330     C                   IF        %error
046500010606     C                   EVAL      PiNum=*off
046600010330     C                   ENDIF
046700010330     C*
046800010330     C                   ENDSR
046900010330     C***
047000010330
047100010601
047200010601
047300010601
047400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047500010601     C     repfil        BEGSR
047600010601     C*
047700010601     C                   if        invfil = *zeros and
047800010601     C                             depfil > *zeros and
047900010629     C                             (vinflg = *blanks or
048000010629     C                              vinflg = *zeros)
048100010601     C
048200010601     C                   eval      invfil = depfil
048300010601     C                   endif
048400010601     C*
048500010601     C                   if        depfil <> invfil and
048600010601     C                             invfil > *zeros
048700010601     C                   eval      flgMulti = '1'
048800010601     C                   if        vinflg = *blanks
048900010601     C                   add       1             cntNonEl
049000010601     C                   endif
049100010601     C                   endif
049200010601     C*
049300010601     C                   if        vinflg = '2'
049400010601     C                   eval      flgStato = '2'
049500010601     C                   endif
049600010601     C*
049700010601     C                   ENDSR
049800010601     C***
049900010601
050000010601
050100010601
050200010330
050300010330
050400010330
050500990920      /TITLE Invio dei dati al punto operativo.
050600000613     C     invio         BEGSR
050700990920     C*
050800990920     C                   reset                   dscmz
050900010601     C                   move      invfil        cmzdst
051000990920     C                   eval      cmzfld = vlrfou
051100990920     C                   eval      cmzmbd = vlrhdl
051200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051300000710     C                   if        prmfir = *blanks
051400050408     C                   eval      cmzfla = 'FIVAB00F'
051500050408     C                   eval      cmzmba = 'FIVAB00F'
051600000710     C                   else
051700000710     C                   eval      cmzfla = prmfir
051800000710     C                   eval      cmzmba = prmfir
051900000710     C                   endif
052000990920     C                   eval      cmznrr = *zeros
052100990920     C                   move      呃trok        cmznrr
052200021018     C                   eval      cmzlba = vlrfl1
052300990920     C                   call(e)   'TIS711C'
052400990920     C                   parm                    dscmz
052500990921     C                   parm      *blanks       esito
052600990920     C                   if        %error
052700990920     C                             or cmzerr = '1'
052800990921     C                             or esito  = '1'
052900000710     C                   eval      wrkesito = '3'
053000990920     C                   endif
053100990920     C*
053200000613     C                   ENDSR
053300990910
053400010601
053500010601
053600010601
053700010601
053800010601      /TITLE Invio dei dati al punto operativo.
053900010601     C     opeini        BEGSR
054000010601     C*
054100010601     C* Inizializzo flag e contatori operativi
054200010601     C                   movel     '0'           flgGiro           1
054300010601     C                   movel     '0'           flgMulti          1
054400010601     C                   movel     '1'           flgStato          1
054500010615     C                   movel     '0'           flgOk             1
054600010601     C                   z-add     *zeros        cntNonEl         10 0
054700010601     C                   z-add     *zeros        depfil            3 0
054800010601     C                   z-add     *zeros        invfil            3 0
054900010601     C*
055000010601     C                   ENDSR
055100010601     C***
055200010601
055300010601
055400010601
055500010330
055600010330
055700000613     C     *inzsr        BEGSR
055800990910     C*
055900990910     C     *entry        plist
056000990920     C                   parm                    tivlrds
056100990921     C                   parm      wrkesito      esito
056200000724     C                   parm                    prmlit
056300000710     C                   parm                    prmfir
056400010330     C*
056500010330     C* CALCOLA LA DATA CORRENTE
056600010330     C                   time                    wn14             14 0
056700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056800010330     C                   z-add     wn8           g08dat
056900010330     C                   z-add     *zeros        g08inv
057000010330     C                   movel     '0'           g08err
057100010330     C                   call      'XSRDA8'
057200010330     C                   parm                    wlbda8
057300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057400000613     C*
057500000613     C                   ENDSR
057600000613     C***
