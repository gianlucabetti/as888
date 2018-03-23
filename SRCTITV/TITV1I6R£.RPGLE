000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200080611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002800080611     D*------------------
002900080611     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
003000080611     D*------------------
003100080611     D PiStr           S            100A   INZ
003200080611     D PiDecChr        S              1A   INZ
003300080611     D PiVal           S             63S30 INZ
003400080611     D PiInt           S               N   INZ
003500080611     D PiNum           S               N   INZ
003600080611
003700080611     D*------------------
003800080611     D* LINKING A DEFINIZIONI ESTERNE
003900080611     D*------------------
004000080611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004100010330
004200010330
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010601     C*
004700010601     C                   exsr      opeini
004800010605     C                   exsr      rwvab
004900070502     C*
005000070502     C* Effettuo lancio TISI95 solo x chiusura
005100070502     C                   CLEAR                   TISI95DS
005200070502     C                   EVAL      I95TLA = 'C'
005300070502     C                   CALL      'TISI95R'
005400070502     C                   PARM                    TISI95DS
005500010601     C*
005600010601     C                   seton                                        lr
005700010601
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006300010601     C*--------------------------------------------------------
006400010601     C     PREELA        BEGSR
006500010601     C*
006600010601     C* SE OCCORRE SPEDIRE IN FILIALE
006700010601     C                   if        invfil <> *zeros and
006800010601     C                             flgGiro = '0'
006900010601     C*
007000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007100010601     C                   eval      flgGiro = '1'
007200010601     C*
007300010601     C                   endif
007400010601     C*
007500010601     C                   ENDSR
007600010601     C***
007700010601
007800010601
007900010601
008000010601     C*--------------------------------------------------------
008100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008200010601     C*--------------------------------------------------------
008300010601     C     ENDELA        BEGSR
008400000616     C*
008500010601     C                   ENDSR
008600010601     C***
008700000613
008800010601
008900010601
009000010330     C*--------------------------------------------------------
009100050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
009200010330     C*--------------------------------------------------------
009300010605     C     RWVAB         BEGSR
009400050408     C*
009500010330     C                   if        not %open(tivin00r)
009600010330     C                   open      tivin00r
009700010330     C                   endif
009800050408     C                   if        not %open(fivabwwr)
009900050408     C                   open      fivabwwr
010000010330     C                   endif
010100010330     C*
010200010604     C                   clear                   §CTROK
010300010604     C                   clear                   §CTRMO
010400010604     C                   clear                   §CTRNO
010500010330     C*
010600010330     C                   DO        *HIVAL
010700010330     C*
010800010330     C                   READ      tivin00r                               70
010900010618     C*
011000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011100010618     C                   if        vinflg = '1'
011200010618     C                   eval      flgOk = '1'
011300010618     C                   endif
011400010618     C*
011500010330     C                   if        vindta > *blanks
011600010330     C                   add       1             rrnum
011700010330     C*
011800010601     C                   if        *in70 = *off and
011900010330     C                             (vinflg = *blanks
012000010330     C                              or vinflg = '0'
012100010330     C                              or vinflg = '2')
012200010330     C*
012300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012400010711     C                   if        vinflg = *blanks or vinflg = '0'
012500010711     C                   clear                   vinmsg
012600010711     C                   endif
012700070601     C*
012800070601     C                   exsr      inzvar
012900070601     C                   exsr      defcam
013000070601     C                   exsr      impvab
013100010601     C*
013200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013300010605     C                   eval      depfil = VABLNP
013400010601     C                   exsr      repfil
013500010601     C                   if        depfil = invfil
013600021025     C                   if        vlrpoi = 999
013700021025     C                   MOVE(P)   invfil        VABFGS
013800021025     C                   else
013900021025     C                   MOVE(P)   vlrpoi        VABFGS
014000021025     C                   endif
014100010601     C*
014200010601     C                   exsr      PREELA
014300010601     C*
014400010604     C* Ebbene...
014500010604     C*
014600010604     C  N31              ADD       1             §CTROK            7 0
014700010604     C   32              ADD       1             §CTRMO            7 0
014800010604     C   31              ADD       1             §CTRNO            7 0
014900071003     C                   exsr      wrivab                                       => scrivo VAB
015000010604     C*
015100020722     C                   endif
015200020722     C*
015300010604     C                   if        *in31 = *off and
015400010604     C                             *in32 = *off
015500010604     C                   eval      vinflg = '1'
015600010604     C                   else
015700010604     C                   eval      vinflg = '2'
015800010604     C                   endif
015900010604     C                   endif
016000010604     C*
016100010330     C                   else
016200010330     C                   eval      vinflg = '1'
016300010330     C                   endif
016400010601     C*
016500010601     C  N70              update    tivin000
016600010330     C*
016700010330     C  N70              ENDdo
016800010601     C*
016900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017000010601     C                   if        cntNonEl = *zeros or
017100010601     C                             flgMulti = '0'
017200010330     C* Se non ci sono record con errori ...
017300010601     C                   if        §ctrno = 0 and
017400010604     C                             §ctrmo = 0 and
017500010601     C                             flgStato <> '2'
017600010330     C* ... restituisco esito OK.
017700010330     C                   eval      wrkesito = '0'
017800010330     C                   else
017900010330     C                   if        §ctrok > 0
018000010330     C                   eval      wrkesito = '1'
018100010330     C                   else
018200010615     C                   if        flgOk = '0'
018300010615     C                   eval      wrkesito = '2'
018400010615     C                   else
018500010615     C                   eval      wrkesito = '6'
018600010615     C                   endif
018700010330     C                   endif
018800010330     C                   endif
018900010601     C                   else
019000010601     C                   eval      wrkesito = '9'
019100010601     C                   endif
019200010330     C*
019300010330     C                   if        %open(tivin00r)
019400010330     C                   close     tivin00r
019500010330     C                   endif
019600050408     C                   if        %open(fivabwwr)
019700050408     C                   close     fivabwwr
019800010330     C                   endif
019900010601     C*
020000010601     C                   if        vlrpoi <> 999
020100010601     C                   eval      invfil = vlrpoi
020200010601     C                   endif
020300010330     C*
020400010330     C                   if        §ctrok > 0
020500010601     C                             and invfil > *zeros
020600010330     C                   exsr      invio
020700010330     C                   endif
020800010601     C*
020900010618     C                   if        flgGiro = '1'
021000010601     C                   exsr      endela
021100010618     C                   endif
021200010330     C*
021300010330     C                   ENDSR
021400010330     C***
021500070601
021600070601     C*----------------------------------------------------*
021700070601     C*  SCARICAMENTO BUFFER RECORDS VAB
021800070601     C*----------------------------------------------------*
021900070601     C     WRIVAB        BEGSR
022000080321     C*
022100070601     C  N31              WRITE     FIVAB000
022200070601     C*
022300070601     C                   ENDSR
022400010601
022500010601
022600010601
022700010330     C*----------------------------------------------------*
022800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022900010330     C*----------------------------------------------------*
023000010330     C     INZVAR        BEGSR
023100010330     C*
023200020204     C                   Z-ADD     *zeros        Num5_0            5 0
023300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023400020322     C                   MOVEL     '0'           FlgCAS            1
023500010330     C*
023600010330     C                   ENDSR
023700010330     C*----------------------------------------------------*
023800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023900010330     C*----------------------------------------------------*
024000010330     C     DEFCAM        BEGSR
024100010330     C*
024200020204     C* Inizializzo il buffer del record da scrivere
024300050408     C                   CLEAR                   FIVAB000
024400020204     C* Imposto i valori di default...
024500080903     C                   EVAL      VABCCM = 0410810
024600080903     C                   EVAL      VABLNP = 041
024700080903     C                   EVAL      VABCTR = 000
024800070208     C                   EVAL      VABCBO = '1'
024900020204     C* ... e poi verifico se sono stati passati come parametri
025000020204     C                   IF        vlrppt > *blanks
025100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
025200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025300020204     C                   EXSR      CHKNUM
025400020204     C                   IF        PiInt=*on
025500020204     C                   Z-ADD     PiVal         VABCCM
025600020204     C                   ENDIF
025700040301     C                   ENDIF
025800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026000020204     C                   EXSR      CHKNUM
026100020204     C                   IF        PiInt=*on
026200020204     C                   Z-ADD     PiVal         VABLNP
026300020204     C                   ENDIF
026400040301     C                   ENDIF
026500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026700020204     C                   EXSR      CHKNUM
026800020204     C                   IF        PiInt=*on
026900020204     C                   Z-ADD     PiVal         VABCTR
027000040301     C                   ENDIF
027100020204     C                   ENDIF
027200020204     C                   ENDIF
027300020204     C*
027400010330     C                   ENDSR
027500010607     C*----------------------------------------------------*
027600050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027700010607     C*----------------------------------------------------*
027800010607     C     IMPVAB        BEGSR
027900010607     C*
028000010607     C                   SETOFF                                       3132
028100010607     C*
028200010607     C* Reperimento campi ALFA
028300080903     C                   EVAL      VABRSD=%trim(%subst(vindta:1:40))
028400080903     C                   EVAL      VABIND=%trim(%subst(vindta:42:40))
028500080903     C                   EVAL      VABLOD=%trim(%subst(vindta:93:30))
028600080903     C                   EVAL      VABPRD=%trim(%subst(vindta:124:2))
028700080903     C                   EVAL      VABRMA=%trim(%subst(vindta:144:8))
028800080408     C*
028900080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
029000080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029100080408     C     '@':'A'       XLATE     VABRSD        VABRSD
029200080408     C* ==
029300060510     C*
029400060510     C* Reperimento campi NUMERICI
029500060510     C                   MOVEL     DATCOR        VABAAS
029600060510     C                   MOVE      DATCOR        VABMGS
029700080410     C* NSP
029800080903     C                   EVAL      PiStr=%trim(%subst(vindta:144:7))
029900060510     C                   EXSR      CHKNUM
030000060510     C                   IF        PiInt=*on
030100060510     C                   Z-ADD     PiVal         VABNSP
030200060404     C                   ELSE
030300070502     C                   SETON                                        31
030400060510     C                   Z-ADD     *zeros        VABNSP
030500060404     C                   EVAL      vinmsg = %trimr(vinmsg)
030600080410     C                             + ' ' + 'VABNSP'
030700060404     C                   ENDIF
030800080410     C* RMN
030900080903     C                   EVAL      PiStr=%trim(%subst(vindta:144:7))
031000080410     C                   EXSR      CHKNUM
031100080410     C                   IF        PiInt=*on
031200080410     C                   Z-ADD     PiVal         VABRMN
031300080410     C                   ELSE
031400080410     C                   SETON                                        31
031500080410     C                   Z-ADD     1             VABRMN
031600080410     C                   EVAL      vinmsg = %trimr(vinmsg)
031700080410     C                             + ' ' + 'VABRMN'
031800080410     C                   ENDIF
031900080408     C* CAD
032000080903     C                   EVAL      PiStr=%trim(%subst(vindta:83:9))
032100080408     C                   EXSR      CHKNUM
032200080408     C                   IF        PiInt=*on
032300080408     C                   Z-ADD     PiVal         Num5_0
032400080408     C                   MOVEL(p)  Num5_0        VABCAD
032500080408     C                   ELSE
032600080408     C                   SETON                                        32
032700080408     C                   EVAL      VABCAD = *zeros
032800080408     C                   EVAL      vinmsg = %trimr(vinmsg)
032900080408     C                             + ' ' + 'VABCAD'
033000080408     C                   ENDIF
033100080408     C* Reperisco la provincia dal CAP e dalla località
033200080408     C                   IF        VABCAD <> *blanks AND
033300080408     C                             VABPRD  = *blanks
033400080408     C                   CLEAR                   TISI95DS
033500080408     C                   EVAL      I95TCN = '3'
033600080408     C                   Z-ADD     datcor        I95DAT
033700080408     C                   EVAL      I95CAP = VABCAD
033800080408     C                   EVAL      I95LOC = VABLOD
033900080408     C                   EVAL      I95NAR = VABNZD
034000080408     C                   CALL      'TISI95R'
034100080408     C                   PARM                    TISI95DS
034200080408     C                   EVAL      VABPRD = O95PRV
034300080408     C                   ENDIF
034400080410     C* NCL
034500080903     C                   EVAL      PiStr=%trim(%subst(vindta:129:5))
034600080410     C                   EXSR      CHKNUM
034700080410     C                   IF        PiInt=*on
034800080410     C                   Z-ADD     PiVal         VABNCL
034900080410     C                   ELSE
035000080410     C                   SETON                                        32
035100080410     C                   Z-ADD     *zeros        VABNCL
035200080410     C                   EVAL      vinmsg = %trimr(vinmsg)
035300080410     C                             + ' ' + 'VABNCL'
035400080410     C                   ENDIF
035500071029     C* PKB
035600080903     C                   EVAL      PiStr=%trim(%subst(vindta:135:8))
035700010607     C                   EXSR      CHKNUM
035800010607     C                   IF        PiNum=*on
035900080903     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
036000080903     C                   Z-ADD(H)  PiVal         VABPKB
036100010607     C                   ELSE
036200010607     C                   SETON                                        32
036300010607     C                   Z-ADD     *zeros        VABPKB
036400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036500010607     C                             + ' ' + 'VABPKB'
036600010607     C                   ENDIF
036700080320     C* CAS
036800080903     C                   IF        %trim(%subst(vindta:162:13))<>*all'0' AND
036900080903     C                             %trim(%subst(vindta:162:13))<>*blanks
037000080408     C                   EVAL      FlgCAS = '1'
037100080903     C                   EVAL      VABVCA='EUR'
037200080903     C                   EVAL      PiStr=%trim(%subst(vindta:162:13))
037300080320     C                   EXSR      CHKNUM
037400080320     C                   IF        PiNum=*on
037500080903     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037600080903     C                   Z-ADD(H)  PiVal         VABCAS
037700080320     C                   ELSE
037800080320     C                   SETON                                        32
037900080320     C                   Z-ADD     *zeros        VABCAS
038000080320     C                   EVAL      vinmsg = %trimr(vinmsg)
038100080320     C                             + ' ' + 'VABCAS'
038200080408     C                   ENDIF
038300080320     C                   ENDIF
038400020322     C*
038500020322     C* Considerazioni finali su CBO/CAS
038600050908     C                   IF        FlgCAS = '1'
038700040713     C                   IF        VABCBO = '1'
038800020322     C                   EVAL      VABCBO = '4'
038900040713     C                   ENDIF
039000040713     C                   IF        VABCBO = '2'
039100040713     C                   EVAL      VABCBO = '6'
039200040713     C                   ENDIF
039300020322     C                   ENDIF
039400020204     C*
039500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039600020204     C                   EXSR      CHKIMPDIV
039700020204     C*
039800010607     C                   ENDSR
039900010607     C*----------------------------------------------------*
040000010601
040100020204
040200020204     C*----------------------------------------------------*
040300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040400020204     C*----------------------------------------------------*
040500020204     C     CHKIMPDIV     BEGSR
040600020204     C*
040700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040800020204     C                   Z-ADD     *zeros        wrkDec            9 9
040900020204     C*
041000020204     C* Come prima cosa effettuo considerazioni sulla divisa
041100020204     C                   IF        vabIAS > *zeros
041200020204     C                   IF        vabVAS <> 'EUR'
041300020204     C                   EVAL      vabVAS =  'ITL'
041400020204     C                   ENDIF
041500020204     C                   ENDIF
041600020204     C*
041700020204     C                   IF        vabCAS > *zeros
041800020204     C                   IF        vabVCA <> 'EUR'
041900020204     C                   EVAL      vabVCA =  'ITL'
042000020204     C                   ENDIF
042100020204     C                   ENDIF
042200020204     C*
042300020204     C                   IF        vabVMD > *zeros
042400020321     C                   IF        vabVAD <> 'EUR'
042500020204     C                   EVAL      vabVAD =  'ITL'
042600020204     C                   ENDIF
042700020204     C                   ENDIF
042800020204     C*
042900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043000020204     C                   Z-ADD     vabIAS        wrkDec
043100020204     C                   IF        wrkDec > *zeros
043200020204     C                   IF        vabVAS = 'ITL'
043300020204     C                   EVAL      vabIAS = *zeros
043400020204     C                   ENDIF
043500020204     C                   ENDIF
043600020204     C*
043700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043800020204     C                   Z-ADD     vabCAS        wrkDec
043900020204     C                   IF        wrkDec > *zeros
044000020204     C                   IF        vabVCA = 'ITL'
044100020204     C                   EVAL      vabCAS = *zeros
044200020204     C                   ENDIF
044300020204     C                   ENDIF
044400020204     C*
044500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044600020204     C                   Z-ADD     vabVMD        wrkDec
044700020204     C                   IF        wrkDec > *zeros
044800020204     C                   IF        vabVAD = 'ITL'
044900020204     C                   EVAL      vabVMD = *zeros
045000020204     C                   ENDIF
045100020204     C                   ENDIF
045200020204     C*
045300020204     C                   ENDSR
045400020204     C***
045500020204
045600010330
045700010330
045800010330     C*----------------------------------------------------*
045900010330     C*  CONTROLLO NUMERICITA' CAMPI
046000010330     C*----------------------------------------------------*
046100010330     C     CHKNUM        BEGSR
046200010330     C*
046300010606     C                   IF        PiDecChr = *blanks
046400080611     C                   EVAL      PiDecChr = ','
046500010606     C                   ENDIF
046600080611     C*
046700080611     C                   callp     UBISNUM_Check(PiStr
046800080611     C                                          :PiDecChr
046900080611     C                                          :PiVal
047000080611     C                                          :PiNum
047100080611     C                                          :PiInt)
047200010330     C*
047300010330     C                   ENDSR
047400010330     C***
047500010330
047600010601
047700010601
047800010601
047900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048000010601     C     repfil        BEGSR
048100010601     C*
048200010601     C                   if        invfil = *zeros and
048300010601     C                             depfil > *zeros and
048400010629     C                             (vinflg = *blanks or
048500010629     C                              vinflg = *zeros)
048600010601     C
048700010601     C                   eval      invfil = depfil
048800010601     C                   endif
048900010601     C*
049000010601     C                   if        depfil <> invfil and
049100010601     C                             invfil > *zeros
049200010601     C                   eval      flgMulti = '1'
049300010601     C                   if        vinflg = *blanks
049400010601     C                   add       1             cntNonEl
049500010601     C                   endif
049600010601     C                   endif
049700010601     C*
049800010601     C                   if        vinflg = '2'
049900010601     C                   eval      flgStato = '2'
050000010601     C                   endif
050100010601     C*
050200010601     C                   ENDSR
050300010601     C***
050400010601
050500010601
050600010601
050700010330
050800010330
050900010330
051000990920      /TITLE Invio dei dati al punto operativo.
051100000613     C     invio         BEGSR
051200990920     C*
051300990920     C                   reset                   dscmz
051400010601     C                   move      invfil        cmzdst
051500990920     C                   eval      cmzfld = vlrfou
051600990920     C                   eval      cmzmbd = vlrhdl
051700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051800000710     C                   if        prmfir = *blanks
051900050408     C                   eval      cmzfla = 'FIVAB00F'
052000050408     C                   eval      cmzmba = 'FIVAB00F'
052100000710     C                   else
052200000710     C                   eval      cmzfla = prmfir
052300000710     C                   eval      cmzmba = prmfir
052400000710     C                   endif
052500990920     C                   eval      cmznrr = *zeros
052600990920     C                   move      §ctrok        cmznrr
052700021018     C                   eval      cmzlba = vlrfl1
052800990920     C                   call(e)   'TIS711C'
052900990920     C                   parm                    dscmz
053000990921     C                   parm      *blanks       esito
053100990920     C                   if        %error
053200990920     C                             or cmzerr = '1'
053300990921     C                             or esito  = '1'
053400000710     C                   eval      wrkesito = '3'
053500990920     C                   endif
053600990920     C*
053700000613     C                   ENDSR
053800990910
053900010601
054000010601
054100010601
054200010601
054300010601      /TITLE Invio dei dati al punto operativo.
054400010601     C     opeini        BEGSR
054500010601     C*
054600010601     C* Inizializzo flag e contatori operativi
054700010601     C                   movel     '0'           flgGiro           1
054800010601     C                   movel     '0'           flgMulti          1
054900010601     C                   movel     '1'           flgStato          1
055000010615     C                   movel     '0'           flgOk             1
055100010601     C                   z-add     *zeros        cntNonEl         10 0
055200010601     C                   z-add     *zeros        depfil            3 0
055300010601     C                   z-add     *zeros        invfil            3 0
055400010601     C*
055500010601     C                   ENDSR
055600010601     C***
055700010601
055800010601
055900010601
056000010330
056100010330
056200000613     C     *inzsr        BEGSR
056300990910     C*
056400990910     C     *entry        plist
056500990920     C                   parm                    tivlrds
056600990921     C                   parm      wrkesito      esito
056700000724     C                   parm                    prmlit
056800000710     C                   parm                    prmfir
056900010330     C*
057000010330     C* CALCOLA LA DATA CORRENTE
057100010330     C                   time                    wn14             14 0
057200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057300010330     C                   z-add     wn8           g08dat
057400010330     C                   z-add     *zeros        g08inv
057500010330     C                   movel     '0'           g08err
057600010330     C                   call      'XSRDA8'
057700010330     C                   parm                    wlbda8
057800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057900000613     C*
058000000613     C                   ENDSR
058100000613     C***
