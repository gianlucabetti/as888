000100040723      /TITLE Upload via Internet: traduzione in FIVABWWR. (x FESTO Cod. 0891154)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040723     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400040607     D tisi95ds      e ds
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
002800000313     D*
002900010330
003000010330
003100010330
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010601     C*
003600010601     C                   exsr      opeini
003700010605     C                   exsr      rwvab
003800040928     C*
003900040928     C* Esegue lancio TISI95R solo x chiusura
004000040928     C                   CLEAR                   TISI95DS
004100040928     C                   EVAL      I95TLA = 'C'
004200040928     C                   CALL      'TISI95R'
004300040928     C                   PARM                    TISI95DS
004400010601     C*
004500010601     C                   seton                                        lr
004600010601
004700010601
004800010601
004900010601
005000010601     C*--------------------------------------------------------
005100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005200010601     C*--------------------------------------------------------
005300010601     C     PREELA        BEGSR
005400010601     C*
005500010601     C* SE OCCORRE SPEDIRE IN FILIALE
005600010601     C                   if        invfil <> *zeros and
005700010601     C                             flgGiro = '0'
005800010601     C*
005900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006000010601     C                   eval      flgGiro = '1'
006100010601     C*
006200010601     C                   endif
006300010601     C*
006400010601     C                   ENDSR
006500010601     C***
006600010601
006700010601
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010601     C*--------------------------------------------------------
007600010601     C     ENDELA        BEGSR
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000000613
008100010330
008200010601
008300010601
008400010601
008500010330     C*--------------------------------------------------------
008600040723     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008700010330     C*--------------------------------------------------------
008800010605     C     RWVAB         BEGSR
008900010330     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300040723     C                   if        not %open(fivabwwr)
009400040723     C                   open      fivabwwr
009500010330     C                   endif
009600010330     C*
009700010604     C                   clear                   §CTROK
009800010604     C                   clear                   §CTRMO
009900010604     C                   clear                   §CTRNO
010000010330     C*
010100010330     C                   DO        *HIVAL
010200010330     C*
010300010330     C                   READ      tivin00r                               70
010400010618     C*
010500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010600010618     C                   if        vinflg = '1'
010700010618     C                   eval      flgOk = '1'
010800010618     C                   endif
010900010618     C*
011000010330     C                   if        vindta > *blanks
011100010330     C                   add       1             rrnum
011200010330     C*
011300010601     C                   if        *in70 = *off and
011400010330     C                             (vinflg = *blanks
011500010330     C                              or vinflg = '0'
011600010330     C                              or vinflg = '2')
011700010330     C*
011800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011900010711     C                   if        vinflg = *blanks or vinflg = '0'
012000010711     C                   clear                   vinmsg
012100010711     C                   endif
012200010601     C*
012300010330     C                   exsr      inzvar
012400010330     C                   exsr      defcam
012500010605     C                   exsr      impvab
012600010601     C*
012700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012800010605     C                   eval      depfil = VABLNP
012900010601     C                   exsr      repfil
013000010601     C                   if        depfil = invfil
013100021025     C                   if        vlrpoi = 999
013200021025     C                   MOVE(P)   invfil        VABFGS
013300021025     C                   else
013400021025     C                   MOVE(P)   vlrpoi        VABFGS
013500021025     C                   endif
013600010601     C*
013700010601     C                   exsr      PREELA
013800010601     C*
013900010604     C* Ebbene...
014000010604     C*
014100010604     C  N31              ADD       1             §CTROK            7 0
014200010604     C   32              ADD       1             §CTRMO            7 0
014300010604     C   31              ADD       1             §CTRNO            7 0
014400020205     C*
014500040723     C  N31              WRITE     FIVAB000
014600010604     C*
014700010604     C                   if        *in31 = *off and
014800010604     C                             *in32 = *off
014900010604     C                   eval      vinflg = '1'
015000010604     C                   else
015100010604     C                   eval      vinflg = '2'
015200010604     C                   endif
015300010604     C                   endif
015400010601     C*
015500010604     C                   endif
015600010604     C*
015700010330     C                   else
015800010330     C                   eval      vinflg = '1'
015900010330     C                   endif
016000010601     C*
016100010601     C  N70              update    tivin000
016200010330     C*
016300010330     C  N70              ENDdo
016400010601     C
016500010601     C*
016600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016700010601     C                   if        cntNonEl = *zeros or
016800010601     C                             flgMulti = '0'
016900010330     C* Se non ci sono record con errori ...
017000010601     C                   if        §ctrno = 0 and
017100010604     C                             §ctrmo = 0 and
017200010601     C                             flgStato <> '2'
017300010330     C* ... restituisco esito OK.
017400010330     C                   eval      wrkesito = '0'
017500010330     C                   else
017600010330     C                   if        §ctrok > 0
017700010330     C                   eval      wrkesito = '1'
017800010330     C                   else
017900010615     C                   if        flgOk = '0'
018000010615     C                   eval      wrkesito = '2'
018100010615     C                   else
018200010615     C                   eval      wrkesito = '6'
018300010615     C                   endif
018400010330     C                   endif
018500010330     C                   endif
018600010601     C                   else
018700010601     C                   eval      wrkesito = '9'
018800010601     C                   endif
018900010330     C*
019000010330     C                   if        %open(tivin00r)
019100010330     C                   close     tivin00r
019200010330     C                   endif
019300040723     C                   if        %open(fivabwwr)
019400040723     C                   close     fivabwwr
019500010330     C                   endif
019600010601     C*
019700010601     C                   if        vlrpoi <> 999
019800010601     C                   eval      invfil = vlrpoi
019900010601     C                   endif
020000010330     C*
020100010330     C                   if        §ctrok > 0
020200010601     C                             and invfil > *zeros
020300010330     C                   exsr      invio
020400010330     C                   endif
020500010601     C*
020600010618     C                   if        flgGiro = '1'
020700010601     C                   exsr      endela
020800010618     C                   endif
020900010330     C*
021000010330     C                   ENDSR
021100010330     C***
021200010330
021300010601
021400010601
021500010601
021600010601
021700010330     C*----------------------------------------------------*
021800010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021900010330     C*----------------------------------------------------*
022000040607     C     INZVAR        BEGSR
022100010330     C*
022200020204     C                   Z-ADD     *zeros        Num5_0            5 0
022300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022400020322     C                   MOVEL     '0'           FlgCAS            1
022500010330     C*
022600010330     C                   ENDSR
022700010330     C*----------------------------------------------------*
022800040607     C*  IMPOSTAZIONE CAMPI COSTANTI
022900010330     C*----------------------------------------------------*
023000010330     C     DEFCAM        BEGSR
023100010330     C*
023200020204     C* Inizializzo il buffer del record da scrivere
023300040723     C                   CLEAR                   FIVAB000
023400020204     C* Imposto i valori di default...
023500040607     C                   EVAL      VABCCM = 0891154
023600040607     C                   EVAL      VABLNP = 089
023700020322     C                   EVAL      VABCTR = 000
023800040607     C                   EVAL      VABTSP = 'C'
023900020204     C* ... e poi verifico se sono stati passati come parametri
024000020204     C                   IF        vlrppt > *blanks
024100040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500020204     C                   Z-ADD     PiVal         VABCCM
024600020204     C                   ENDIF
024700040607     C                   ENDIF
024800040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
024900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025000020204     C                   EXSR      CHKNUM
025100020204     C                   IF        PiInt=*on
025200020204     C                   Z-ADD     PiVal         VABLNP
025300020204     C                   ENDIF
025400040607     C                   ENDIF
025500040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
025600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025700020204     C                   EXSR      CHKNUM
025800020204     C                   IF        PiInt=*on
025900020204     C                   Z-ADD     PiVal         VABCTR
026000020204     C                   ENDIF
026100040607     C                   ENDIF
026200020204     C                   ENDIF
026300020204     C*
026400010330     C                   ENDSR
026500010607     C*----------------------------------------------------*
026600040723     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026700010607     C*----------------------------------------------------*
026800010607     C     IMPVAB        BEGSR
026900010607     C*
027000010607     C                   SETOFF                                       3132
027100010607     C*
027200010607     C* Reperimento campi ALFA
027300010607     C*
027400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027500040607     C                   EVAL      VABRSD=%trim(%subst(vindta:164:30))
027600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027800020117     C* ==
027900040607     C                   EVAL      VABRD2=%trim(%subst(vindta:194:30))
028000040607     C                   EVAL      VABIND=%trim(%subst(vindta:224:25))
028100040607     C                   EVAL      VABLOD=%trim(%subst(vindta:254:25))
028200040607     C                   EVAL      VABRMA=%trim(%subst(vindta:10:7))
028300020329     C*
028400020329     C                   MOVEL     DATCOR        VABAAS
028500020329     C                   MOVE      DATCOR        VABMGS
028600010607     C*
028700010607     C* Reperimento campi NUMERICI
028800040607     C* NSP/RMN
028900040607     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
029000020304     C                   EXSR      CHKNUM
029100020304     C                   IF        PiInt=*on
029200020304     C                   Z-ADD     PiVal         VABNSP
029300040607     C                   Z-ADD     PiVal         VABRMN
029400020304     C                   ELSE
029500020304     C                   SETON                                        32
029600020304     C                   Z-ADD     *zeros        VABNSP
029700040607     C                   Z-ADD     1             VABRMN
029800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029900040607     C                             + ' ' + 'VABNSP VABRMN'
030000020304     C                   ENDIF
030100040607     C* CAD
030200040607     C                   EVAL      PiStr=%trim(%subst(vindta:249:5))
030300010607     C                   EXSR      CHKNUM
030400010607     C                   IF        PiInt=*on
030500020204     C                   Z-ADD     PiVal         Num5_0
030600020204     C                   MOVEL(p)  Num5_0        VABCAD
030700010607     C                   ELSE
030800010607     C                   SETON                                        32
030900020204     C                   EVAL      VABCAD = *zeros
031000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031100020204     C                             + ' ' + 'VABCAD'
031200010607     C                   ENDIF
031300040607     C* Reperisco la provincia dal CAP e dalla località
031400040607     C                   IF        VABCAD <> *blanks
031500040607     C                   CLEAR                   TISI95DS
031600040607     C                   EVAL      I95TCN = '3'
031700040607     C                   Z-ADD     datcor        I95DAT
031800040607     C                   EVAL      I95CAP = VABCAD
031900040607     C                   EVAL      I95LOC = VABLOD
032000040607     C                   CALL      'TISI95R'
032100040607     C                   PARM                    TISI95DS
032200040607     C                   EVAL      VABPRD = O95PRV
032300040607     C                   ENDIF
032400040607     C* NCL
032500040607     C                   EVAL      PiStr=%trim(%subst(vindta:557:4))
032600010607     C                   EXSR      CHKNUM
032700010607     C                   IF        PiInt=*on
032800010607     C                   Z-ADD     PiVal         VABNCL
032900010607     C                   ELSE
033000010607     C                   SETON                                        32
033100010607     C                   Z-ADD     *zeros        VABNCL
033200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033300010607     C                             + ' ' + 'VABNCL'
033400010607     C                   ENDIF
033500040607     C* PKB
033600040607     C                   EVAL      PiStr=%trim(%subst(vindta:561:7))
033700020204     C                   EVAL      PiDecChr = ','
033800010607     C                   EXSR      CHKNUM
033900010607     C                   IF        PiNum=*on
034000040607     C                   Z-ADD     PiVal         VABPKB
034100010607     C                   ELSE
034200010607     C                   SETON                                        32
034300010607     C                   Z-ADD     *zeros        VABPKB
034400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034500010607     C                             + ' ' + 'VABPKB'
034600010607     C                   ENDIF
034700020322     C*
034800020322     C* Considerazioni finali su CBO/CAS
034900020322     C                   IF        FlgCAS = '1'
035000020322     C                   EVAL      VABCBO = '4'
035100020322     C                   ELSE
035200020322     C                   EVAL      VABCBO = '1'
035300020322     C                   ENDIF
035400020204     C*
035500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035600020204     C                   EXSR      CHKIMPDIV
035700020204     C*
035800010607     C                   ENDSR
035900010607     C*----------------------------------------------------*
036000010601
036100020204
036200020204     C*----------------------------------------------------*
036300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
036400020204     C*----------------------------------------------------*
036500020204     C     CHKIMPDIV     BEGSR
036600020204     C*
036700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
036800020204     C                   Z-ADD     *zeros        wrkDec            9 9
036900020204     C*
037000020204     C* Come prima cosa effettuo considerazioni sulla divisa
037100020204     C                   IF        vabIAS > *zeros
037200020204     C                   IF        vabVAS <> 'EUR'
037300020204     C                   EVAL      vabVAS =  'ITL'
037400020204     C                   ENDIF
037500020204     C                   ENDIF
037600020204     C*
037700020204     C                   IF        vabCAS > *zeros
037800020204     C                   IF        vabVCA <> 'EUR'
037900020204     C                   EVAL      vabVCA =  'ITL'
038000020204     C                   ENDIF
038100020204     C                   ENDIF
038200020204     C*
038300020204     C                   IF        vabVMD > *zeros
038400020321     C                   IF        vabVAD <> 'EUR'
038500020204     C                   EVAL      vabVAD =  'ITL'
038600020204     C                   ENDIF
038700020204     C                   ENDIF
038800020204     C*
038900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
039000020204     C                   Z-ADD     vabIAS        wrkDec
039100020204     C                   IF        wrkDec > *zeros
039200020204     C                   IF        vabVAS = 'ITL'
039300020204     C                   EVAL      vabIAS = *zeros
039400020204     C                   ENDIF
039500020204     C                   ENDIF
039600020204     C*
039700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
039800020204     C                   Z-ADD     vabCAS        wrkDec
039900020204     C                   IF        wrkDec > *zeros
040000020204     C                   IF        vabVCA = 'ITL'
040100020204     C                   EVAL      vabCAS = *zeros
040200020204     C                   ENDIF
040300020204     C                   ENDIF
040400020204     C*
040500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
040600020204     C                   Z-ADD     vabVMD        wrkDec
040700020204     C                   IF        wrkDec > *zeros
040800020204     C                   IF        vabVAD = 'ITL'
040900020204     C                   EVAL      vabVMD = *zeros
041000020204     C                   ENDIF
041100020204     C                   ENDIF
041200020204     C*
041300020204     C                   ENDSR
041400020204     C***
041500020204
041600010330
041700010330
041800010330     C*----------------------------------------------------*
041900010330     C*  CONTROLLO NUMERICITA' CAMPI
042000010330     C*----------------------------------------------------*
042100010330     C     CHKNUM        BEGSR
042200010330     C*
042300010606     C                   IF        PiDecChr = *blanks
042400010606     C                   EVAL      PiDecChr = ','
042500010606     C                   ENDIF
042600010606     C*
042700010606     C                   CALL(e)   'ISNUMERIC'
042800010330     C                   PARM                    PiStr            30
042900010606     C                   PARM                    PiDecChr          1
043000010330     C                   PARM      *ZEROS        PiVal            30 9
043100010330     C                   PARM      '0'           PiInt             1
043200010330     C                   PARM      '0'           PiNum             1
043300010330     C                   IF        %error
043400010606     C                   EVAL      PiNum=*off
043500010330     C                   ENDIF
043600010330     C*
043700010330     C                   ENDSR
043800010330     C***
043900010330
044000010601
044100010601
044200010601
044300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044400010601     C     repfil        BEGSR
044500010601     C*
044600010601     C                   if        invfil = *zeros and
044700010601     C                             depfil > *zeros and
044800010629     C                             (vinflg = *blanks or
044900010629     C                              vinflg = *zeros)
045000010601     C
045100010601     C                   eval      invfil = depfil
045200010601     C                   endif
045300010601     C*
045400010601     C                   if        depfil <> invfil and
045500010601     C                             invfil > *zeros
045600010601     C                   eval      flgMulti = '1'
045700010601     C                   if        vinflg = *blanks
045800010601     C                   add       1             cntNonEl
045900010601     C                   endif
046000010601     C                   endif
046100010601     C*
046200010601     C                   if        vinflg = '2'
046300010601     C                   eval      flgStato = '2'
046400010601     C                   endif
046500010601     C*
046600010601     C                   ENDSR
046700010601     C***
046800010601
046900010601
047000010601
047100010330
047200010330
047300010330
047400990920      /TITLE Invio dei dati al punto operativo.
047500000613     C     invio         BEGSR
047600990920     C*
047700990920     C                   reset                   dscmz
047800010601     C                   move      invfil        cmzdst
047900990920     C                   eval      cmzfld = vlrfou
048000990920     C                   eval      cmzmbd = vlrhdl
048100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048200000710     C                   if        prmfir = *blanks
048300040723     C                   eval      cmzfla = 'FIVAB00F'
048400040723     C                   eval      cmzmba = 'FIVAB00F'
048500000710     C                   else
048600000710     C                   eval      cmzfla = prmfir
048700000710     C                   eval      cmzmba = prmfir
048800000710     C                   endif
048900990920     C                   eval      cmznrr = *zeros
049000990920     C                   move      §ctrok        cmznrr
049100021018     C                   eval      cmzlba = vlrfl1
049200990920     C                   call(e)   'TIS711C'
049300990920     C                   parm                    dscmz
049400990921     C                   parm      *blanks       esito
049500990920     C                   if        %error
049600990920     C                             or cmzerr = '1'
049700990921     C                             or esito  = '1'
049800000710     C                   eval      wrkesito = '3'
049900990920     C                   endif
050000990920     C*
050100000613     C                   ENDSR
050200990910
050300010601
050400010601
050500010601
050600010601
050700010601      /TITLE Invio dei dati al punto operativo.
050800010601     C     opeini        BEGSR
050900010601     C*
051000010601     C* Inizializzo flag e contatori operativi
051100010601     C                   movel     '0'           flgGiro           1
051200010601     C                   movel     '0'           flgMulti          1
051300010601     C                   movel     '1'           flgStato          1
051400010615     C                   movel     '0'           flgOk             1
051500010601     C                   z-add     *zeros        cntNonEl         10 0
051600010601     C                   z-add     *zeros        depfil            3 0
051700010601     C                   z-add     *zeros        invfil            3 0
051800010601     C*
051900010601     C                   ENDSR
052000010601     C***
052100010601
052200010601
052300010601
052400010330
052500010330
052600000613     C     *inzsr        BEGSR
052700990910     C*
052800990910     C     *entry        plist
052900990920     C                   parm                    tivlrds
053000990921     C                   parm      wrkesito      esito
053100000724     C                   parm                    prmlit
053200000710     C                   parm                    prmfir
053300010330     C*
053400010330     C* CALCOLA LA DATA CORRENTE
053500010330     C                   time                    wn14             14 0
053600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053700010330     C                   z-add     wn8           g08dat
053800010330     C                   z-add     *zeros        g08inv
053900010330     C                   movel     '0'           g08err
054000010330     C                   call      'XSRDA8'
054100010330     C                   parm                    wlbda8
054200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054300000613     C*
054400000613     C                   ENDSR
054500000613     C***
