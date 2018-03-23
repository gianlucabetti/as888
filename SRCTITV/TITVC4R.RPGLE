000100040607      /TITLE Upload via Internet: traduzione in EDIVABWR. (x FESTO Cod. 0891154)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500031121     FEDIVABwr  O    E             DISK    usropn
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
002000031121     D wNomeFile       s             30    INZ(*blanks)
002100010330     D*------------------
002200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002300010330     D*------------------
002400010330     D WLBDA8          DS                  INZ
002500010330     D  G08DAT                 1      8  0
002600010330     D  G08INV                 9     16  0
002700010330     D  G08ERR                17     17
002800010330     D  G08TGI                18     22  0
002900000313     D*
003000010330
003100010330
003200010330
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800010605     C                   exsr      rwvab
003900010601     C*
004000010601     C                   seton                                        lr
004100010601
004200010601
004300010601
004400010601
004500010601     C*--------------------------------------------------------
004600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004700010601     C*--------------------------------------------------------
004800010601     C     PREELA        BEGSR
004900010601     C*
005000010601     C* SE OCCORRE SPEDIRE IN FILIALE
005100010601     C                   if        invfil <> *zeros and
005200010601     C                             flgGiro = '0'
005300010601     C*
005400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005500010601     C                   eval      flgGiro = '1'
005600010601     C*
005700010601     C                   endif
005800010601     C*
005900010601     C                   ENDSR
006000010601     C***
006100010601
006200010601
006300010601
006400010601
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007000010601     C*--------------------------------------------------------
007100010601     C     ENDELA        BEGSR
007200050131     C*
007300050131     C* Effettuo lancio TISI95 solo x chiusura
007400050131     C                   CLEAR                   TISI95DS
007500050131     C                   EVAL      I95TLA = 'C'
007600050131     C                   CALL      'TISI95R'
007700050131     C                   PARM                    TISI95DS
007800000616     C*
007900010601     C                   ENDSR
008000010601     C***
008100000613
008200010330
008300010601
008400010601
008500010601
008600010330     C*--------------------------------------------------------
008700031121     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
008800010330     C*--------------------------------------------------------
008900010605     C     RWVAB         BEGSR
009000010330     C*
009100010330     C                   if        not %open(tivin00r)
009200010330     C                   open      tivin00r
009300010330     C                   endif
009400031121     C                   if        not %open(edivabwr)
009500031121     C                   open      edivabwr
009600010330     C                   endif
009700010330     C*
009800010604     C                   clear                   §CTROK
009900010604     C                   clear                   §CTRMO
010000010604     C                   clear                   §CTRNO
010100010330     C*
010200010330     C                   DO        *HIVAL
010300010330     C*
010400010330     C                   READ      tivin00r                               70
010500010618     C*
010600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010700010618     C                   if        vinflg = '1'
010800010618     C                   eval      flgOk = '1'
010900010618     C                   endif
011000010618     C*
011100010330     C                   if        vindta > *blanks
011200010330     C                   add       1             rrnum
011300010330     C*
011400010601     C                   if        *in70 = *off and
011500010330     C                             (vinflg = *blanks
011600010330     C                              or vinflg = '0'
011700010330     C                              or vinflg = '2')
011800010330     C*
011900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012000010711     C                   if        vinflg = *blanks or vinflg = '0'
012100010711     C                   clear                   vinmsg
012200010711     C                   endif
012300010601     C*
012400010330     C                   exsr      inzvar
012500010330     C                   exsr      defcam
012600010605     C                   exsr      impvab
012700010601     C*
012800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012900010605     C                   eval      depfil = VABLNP
013000010601     C                   exsr      repfil
013100010601     C                   if        depfil = invfil
013200021025     C                   if        vlrpoi = 999
013300021025     C                   MOVE(P)   invfil        VABFGS
013400021025     C                   else
013500021025     C                   MOVE(P)   vlrpoi        VABFGS
013600021025     C                   endif
013700010601     C*
013800010601     C                   exsr      PREELA
013900010601     C*
014000010604     C* Ebbene...
014100010604     C*
014200010604     C  N31              ADD       1             §CTROK            7 0
014300010604     C   32              ADD       1             §CTRMO            7 0
014400010604     C   31              ADD       1             §CTRNO            7 0
014500020205     C*
014600020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014700031121     C  N31              WRITE     EDIVAB00
014800010604     C*
014900010604     C                   if        *in31 = *off and
015000010604     C                             *in32 = *off
015100010604     C                   eval      vinflg = '1'
015200010604     C                   else
015300010604     C                   eval      vinflg = '2'
015400010604     C                   endif
015500010604     C                   endif
015600010601     C*
015700010604     C                   endif
015800010604     C*
015900010330     C                   else
016000010330     C                   eval      vinflg = '1'
016100010330     C                   endif
016200010601     C*
016300010601     C  N70              update    tivin000
016400010330     C*
016500010330     C  N70              ENDdo
016600010601     C
016700010601     C*
016800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016900010601     C                   if        cntNonEl = *zeros or
017000010601     C                             flgMulti = '0'
017100010330     C* Se non ci sono record con errori ...
017200010601     C                   if        §ctrno = 0 and
017300010604     C                             §ctrmo = 0 and
017400010601     C                             flgStato <> '2'
017500010330     C* ... restituisco esito OK.
017600010330     C                   eval      wrkesito = '0'
017700010330     C                   else
017800010330     C                   if        §ctrok > 0
017900010330     C                   eval      wrkesito = '1'
018000010330     C                   else
018100010615     C                   if        flgOk = '0'
018200010615     C                   eval      wrkesito = '2'
018300010615     C                   else
018400010615     C                   eval      wrkesito = '6'
018500010615     C                   endif
018600010330     C                   endif
018700010330     C                   endif
018800010601     C                   else
018900010601     C                   eval      wrkesito = '9'
019000010601     C                   endif
019100010330     C*
019200010330     C                   if        %open(tivin00r)
019300010330     C                   close     tivin00r
019400010330     C                   endif
019500031121     C                   if        %open(edivabwr)
019600031121     C                   close     edivabwr
019700010330     C                   endif
019800010601     C*
019900010601     C                   if        vlrpoi <> 999
020000010601     C                   eval      invfil = vlrpoi
020100010601     C                   endif
020200010330     C*
020300010330     C                   if        §ctrok > 0
020400010601     C                             and invfil > *zeros
020500010330     C                   exsr      invio
020600010330     C                   endif
020700010601     C*
020800010618     C                   if        flgGiro = '1'
020900010601     C                   exsr      endela
021000010618     C                   endif
021100010330     C*
021200010330     C                   ENDSR
021300010330     C***
021400010330
021500010601
021600010601
021700010601
021800010601
021900010330     C*----------------------------------------------------*
022000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022100010330     C*----------------------------------------------------*
022200040607     C     INZVAR        BEGSR
022300010330     C*
022400020204     C                   Z-ADD     *zeros        Num5_0            5 0
022500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022600020322     C                   MOVEL     '0'           FlgCAS            1
022700010330     C*
022800010330     C                   ENDSR
022900010330     C*----------------------------------------------------*
023000040607     C*  IMPOSTAZIONE CAMPI COSTANTI
023100010330     C*----------------------------------------------------*
023200010330     C     DEFCAM        BEGSR
023300010330     C*
023400020204     C* Inizializzo il buffer del record da scrivere
023500031121     C                   CLEAR                   EDIVAB00
023600020204     C* Imposto i valori di default...
023700040607     C                   EVAL      VABCCM = 0891154
023800020403     C                   EVAL      VABCTM = '7Q'
023900040607     C                   EVAL      VABLNP = 089
024000020322     C                   EVAL      VABCTR = 000
024100040607     C                   EVAL      VABTSP = 'C'
024200020204     C* ... e poi verifico se sono stati passati come parametri
024300020204     C                   IF        vlrppt > *blanks
024400040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
024500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024600020204     C                   EXSR      CHKNUM
024700020204     C                   IF        PiInt=*on
024800020204     C                   Z-ADD     PiVal         VABCCM
024900020204     C                   ENDIF
025000040607     C                   ENDIF
025100040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
025200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025300020204     C                   EXSR      CHKNUM
025400020204     C                   IF        PiInt=*on
025500020204     C                   Z-ADD     PiVal         VABLNP
025600020204     C                   ENDIF
025700040607     C                   ENDIF
025800040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
025900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026000020204     C                   EXSR      CHKNUM
026100020204     C                   IF        PiInt=*on
026200020204     C                   Z-ADD     PiVal         VABCTR
026300020204     C                   ENDIF
026400040607     C                   ENDIF
026500020204     C                   ENDIF
026600020204     C*
026700010330     C                   ENDSR
026800010607     C*----------------------------------------------------*
026900031121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
027000010607     C*----------------------------------------------------*
027100010607     C     IMPVAB        BEGSR
027200010607     C*
027300010607     C                   SETOFF                                       3132
027400010607     C*
027500010607     C* Reperimento campi ALFA
027600010607     C*
027700010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027800040607     C                   EVAL      VABRSD=%trim(%subst(vindta:164:30))
027900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028100020117     C* ==
028200040607     C                   EVAL      VABRD2=%trim(%subst(vindta:194:30))
028300040607     C                   EVAL      VABIND=%trim(%subst(vindta:224:25))
028400040607     C                   EVAL      VABLOD=%trim(%subst(vindta:254:25))
028500040607     C                   EVAL      VABRMA=%trim(%subst(vindta:10:7))
028600020329     C*
028700020329     C                   MOVEL     DATCOR        VABAAS
028800020329     C                   MOVE      DATCOR        VABMGS
028900010607     C*
029000010607     C* Reperimento campi NUMERICI
029100040607     C* NSP/RMN
029200040607     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
029300020304     C                   EXSR      CHKNUM
029400020304     C                   IF        PiInt=*on
029500020304     C                   Z-ADD     PiVal         VABNSP
029600040607     C                   Z-ADD     PiVal         VABRMN
029700020304     C                   ELSE
029800020304     C                   SETON                                        32
029900020304     C                   Z-ADD     *zeros        VABNSP
030000040607     C                   Z-ADD     1             VABRMN
030100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030200040607     C                             + ' ' + 'VABNSP VABRMN'
030300020304     C                   ENDIF
030400040607     C* CAD
030500040607     C                   EVAL      PiStr=%trim(%subst(vindta:249:5))
030600010607     C                   EXSR      CHKNUM
030700010607     C                   IF        PiInt=*on
030800020204     C                   Z-ADD     PiVal         Num5_0
030900020204     C                   MOVEL(p)  Num5_0        VABCAD
031000010607     C                   ELSE
031100010607     C                   SETON                                        32
031200020204     C                   EVAL      VABCAD = *zeros
031300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031400020204     C                             + ' ' + 'VABCAD'
031500010607     C                   ENDIF
031600040607     C* Reperisco la provincia dal CAP e dalla località
031700040607     C                   IF        VABCAD <> *blanks
031800040607     C                   CLEAR                   TISI95DS
031900040607     C                   EVAL      I95TCN = '3'
032000040607     C                   Z-ADD     datcor        I95DAT
032100040607     C                   EVAL      I95CAP = VABCAD
032200040607     C                   EVAL      I95LOC = VABLOD
032300040607     C                   CALL      'TISI95R'
032400040607     C                   PARM                    TISI95DS
032500040607     C                   EVAL      VABPRD = O95PRV
032600040607     C                   ENDIF
032700040607     C* NCL
032800040607     C                   EVAL      PiStr=%trim(%subst(vindta:557:4))
032900010607     C                   EXSR      CHKNUM
033000010607     C                   IF        PiInt=*on
033100010607     C                   Z-ADD     PiVal         VABNCL
033200010607     C                   ELSE
033300010607     C                   SETON                                        32
033400010607     C                   Z-ADD     *zeros        VABNCL
033500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033600010607     C                             + ' ' + 'VABNCL'
033700010607     C                   ENDIF
033800040607     C* PKB
033900040607     C                   EVAL      PiStr=%trim(%subst(vindta:561:7))
034000020204     C                   EVAL      PiDecChr = ','
034100010607     C                   EXSR      CHKNUM
034200010607     C                   IF        PiNum=*on
034300040607     C                   Z-ADD     PiVal         VABPKB
034400010607     C                   ELSE
034500010607     C                   SETON                                        32
034600010607     C                   Z-ADD     *zeros        VABPKB
034700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034800010607     C                             + ' ' + 'VABPKB'
034900010607     C                   ENDIF
035000020322     C*
035100020322     C* Considerazioni finali su CBO/CAS
035200020322     C                   IF        FlgCAS = '1'
035300020322     C                   EVAL      VABCBO = '4'
035400020322     C                   ELSE
035500020322     C                   EVAL      VABCBO = '1'
035600020322     C                   ENDIF
035700020204     C*
035800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035900020204     C                   EXSR      CHKIMPDIV
036000031121     C*
036100031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
036200031121     C                   MOVE(P)   vlrMSG        wNomeFile
036300040607     C                   EVAL      VABCMR = %subst(wNomeFile:3:6)
036400031121     C                   EVAL      VABDCM = DATCOR
036500031121     C                   EVAL      VABDTS = DATCOR
036600031121     C                   EVAL      VABCNT = 1
036700020204     C*
036800010607     C                   ENDSR
036900010607     C*----------------------------------------------------*
037000010601
037100020204
037200020204     C*----------------------------------------------------*
037300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
037400020204     C*----------------------------------------------------*
037500020204     C     CHKIMPDIV     BEGSR
037600020204     C*
037700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
037800020204     C                   Z-ADD     *zeros        wrkDec            9 9
037900020204     C*
038000020204     C* Come prima cosa effettuo considerazioni sulla divisa
038100020204     C                   IF        vabIAS > *zeros
038200020204     C                   IF        vabVAS <> 'EUR'
038300020204     C                   EVAL      vabVAS =  'ITL'
038400020204     C                   ENDIF
038500020204     C                   ENDIF
038600020204     C*
038700020204     C                   IF        vabCAS > *zeros
038800020204     C                   IF        vabVCA <> 'EUR'
038900020204     C                   EVAL      vabVCA =  'ITL'
039000020204     C                   ENDIF
039100020204     C                   ENDIF
039200020204     C*
039300020204     C                   IF        vabVMD > *zeros
039400020321     C                   IF        vabVAD <> 'EUR'
039500020204     C                   EVAL      vabVAD =  'ITL'
039600020204     C                   ENDIF
039700020204     C                   ENDIF
039800020204     C*
039900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
040000020204     C                   Z-ADD     vabIAS        wrkDec
040100020204     C                   IF        wrkDec > *zeros
040200020204     C                   IF        vabVAS = 'ITL'
040300020204     C                   EVAL      vabIAS = *zeros
040400020204     C                   ENDIF
040500020204     C                   ENDIF
040600020204     C*
040700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
040800020204     C                   Z-ADD     vabCAS        wrkDec
040900020204     C                   IF        wrkDec > *zeros
041000020204     C                   IF        vabVCA = 'ITL'
041100020204     C                   EVAL      vabCAS = *zeros
041200020204     C                   ENDIF
041300020204     C                   ENDIF
041400020204     C*
041500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
041600020204     C                   Z-ADD     vabVMD        wrkDec
041700020204     C                   IF        wrkDec > *zeros
041800020204     C                   IF        vabVAD = 'ITL'
041900020204     C                   EVAL      vabVMD = *zeros
042000020204     C                   ENDIF
042100020204     C                   ENDIF
042200020204     C*
042300020204     C                   ENDSR
042400020204     C***
042500020204
042600010330
042700010330
042800010330     C*----------------------------------------------------*
042900010330     C*  CONTROLLO NUMERICITA' CAMPI
043000010330     C*----------------------------------------------------*
043100010330     C     CHKNUM        BEGSR
043200010330     C*
043300010606     C                   IF        PiDecChr = *blanks
043400010606     C                   EVAL      PiDecChr = ','
043500010606     C                   ENDIF
043600010606     C*
043700010606     C                   CALL(e)   'ISNUMERIC'
043800010330     C                   PARM                    PiStr            30
043900010606     C                   PARM                    PiDecChr          1
044000010330     C                   PARM      *ZEROS        PiVal            30 9
044100010330     C                   PARM      '0'           PiInt             1
044200010330     C                   PARM      '0'           PiNum             1
044300010330     C                   IF        %error
044400010606     C                   EVAL      PiNum=*off
044500010330     C                   ENDIF
044600010330     C*
044700010330     C                   ENDSR
044800010330     C***
044900010330
045000010601
045100010601
045200010601
045300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045400010601     C     repfil        BEGSR
045500010601     C*
045600010601     C                   if        invfil = *zeros and
045700010601     C                             depfil > *zeros and
045800010629     C                             (vinflg = *blanks or
045900010629     C                              vinflg = *zeros)
046000010601     C
046100010601     C                   eval      invfil = depfil
046200010601     C                   endif
046300010601     C*
046400010601     C                   if        depfil <> invfil and
046500010601     C                             invfil > *zeros
046600010601     C                   eval      flgMulti = '1'
046700010601     C                   if        vinflg = *blanks
046800010601     C                   add       1             cntNonEl
046900010601     C                   endif
047000010601     C                   endif
047100010601     C*
047200010601     C                   if        vinflg = '2'
047300010601     C                   eval      flgStato = '2'
047400010601     C                   endif
047500010601     C*
047600010601     C                   ENDSR
047700010601     C***
047800010601
047900010601
048000010601
048100010330
048200010330
048300010330
048400990920      /TITLE Invio dei dati al punto operativo.
048500000613     C     invio         BEGSR
048600990920     C*
048700990920     C                   reset                   dscmz
048800010601     C                   move      invfil        cmzdst
048900990920     C                   eval      cmzfld = vlrfou
049000990920     C                   eval      cmzmbd = vlrhdl
049100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
049200000710     C                   if        prmfir = *blanks
049300031121     C                   eval      cmzfla = 'EDIVAB0F'
049400031121     C                   eval      cmzmba = 'EDIVAB0F'
049500000710     C                   else
049600000710     C                   eval      cmzfla = prmfir
049700000710     C                   eval      cmzmba = prmfir
049800000710     C                   endif
049900990920     C                   eval      cmznrr = *zeros
050000990920     C                   move      §ctrok        cmznrr
050100021018     C                   eval      cmzlba = vlrfl1
050200990920     C                   call(e)   'TIS711C'
050300990920     C                   parm                    dscmz
050400990921     C                   parm      *blanks       esito
050500990920     C                   if        %error
050600990920     C                             or cmzerr = '1'
050700990921     C                             or esito  = '1'
050800000710     C                   eval      wrkesito = '3'
050900990920     C                   endif
051000990920     C*
051100000613     C                   ENDSR
051200990910
051300010601
051400010601
051500010601
051600010601
051700010601      /TITLE Invio dei dati al punto operativo.
051800010601     C     opeini        BEGSR
051900010601     C*
052000010601     C* Inizializzo flag e contatori operativi
052100010601     C                   movel     '0'           flgGiro           1
052200010601     C                   movel     '0'           flgMulti          1
052300010601     C                   movel     '1'           flgStato          1
052400010615     C                   movel     '0'           flgOk             1
052500010601     C                   z-add     *zeros        cntNonEl         10 0
052600010601     C                   z-add     *zeros        depfil            3 0
052700010601     C                   z-add     *zeros        invfil            3 0
052800010601     C*
052900010601     C                   ENDSR
053000010601     C***
053100010601
053200010601
053300010601
053400010330
053500010330
053600000613     C     *inzsr        BEGSR
053700990910     C*
053800990910     C     *entry        plist
053900990920     C                   parm                    tivlrds
054000990921     C                   parm      wrkesito      esito
054100000724     C                   parm                    prmlit
054200000710     C                   parm                    prmfir
054300010330     C*
054400010330     C* CALCOLA LA DATA CORRENTE
054500010330     C                   time                    wn14             14 0
054600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054700010330     C                   z-add     wn8           g08dat
054800010330     C                   z-add     *zeros        g08inv
054900010330     C                   movel     '0'           g08err
055000010330     C                   call      'XSRDA8'
055100010330     C                   parm                    wlbda8
055200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055300000613     C*
055400000613     C                   ENDSR
055500000613     C***
