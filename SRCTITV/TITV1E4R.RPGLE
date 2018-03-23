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
013600071003     C                   exsr      wrivab                                       => scrivo VAB
013700010604     C*
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
018300050408     C                   if        %open(fivabwwr)
018400050408     C                   close     fivabwwr
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
020200070601
020300070601     C*----------------------------------------------------*
020400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020500070601     C*----------------------------------------------------*
020600070601     C     WRIVAB        BEGSR
020700070601     C*
020800070601     C  N31              WRITE     FIVAB000
020900070601     C*
021000070601     C                   ENDSR
021100010601
021200010601
021300010601
021400010330     C*----------------------------------------------------*
021500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600010330     C*----------------------------------------------------*
021700010330     C     INZVAR        BEGSR
021800010330     C*
021900020204     C                   Z-ADD     *zeros        Num5_0            5 0
022000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022100020322     C                   MOVEL     '0'           FlgCAS            1
022200010330     C*
022300010330     C                   ENDSR
022400010330     C*----------------------------------------------------*
022500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022600010330     C*----------------------------------------------------*
022700010330     C     DEFCAM        BEGSR
022800010330     C*
022900020204     C* Inizializzo il buffer del record da scrivere
023000050408     C                   CLEAR                   FIVAB000
023100020204     C* Imposto i valori di default...
023200080307     C                   EVAL      VABCCM = 0891093
023300080307     C                   EVAL      VABLNP = 089
023400080307     C                   EVAL      VABCTR = 000
023500080307     C                   EVAL      VABTSP = 'C'
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
027300080307     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
027400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027600020117     C* ==
027700080307     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
027800080307     C                   EVAL      VABLOD=%trim(%subst(vindta:84:20))
027900080307     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
028000080307     C                   EVAL      VABRMA=%trim(%subst(vindta:133:6))
028100060510     C*
028200060510     C* Reperimento campi NUMERICI
028300060510     C                   MOVEL     DATCOR        VABAAS
028400060510     C                   MOVE      DATCOR        VABMGS
028500080307     C*
028600080307     C* Considerazioni relative al CCM
028700080307     C                   IF        %trim(%subst(vindta:151:1)) = '*'
028800080307     C                   EVAL      VABCCM=0891092
028900080307     C                   ENDIF
029000060710     C* NSP/RMN
029100080307     C                   EVAL      PiStr=%trim(%subst(vindta:114:6))
029200060510     C                   EXSR      CHKNUM
029300060510     C                   IF        PiInt=*on
029400060510     C                   Z-ADD     PiVal         VABNSP
029500060510     C                   Z-ADD     PiVal         VABRMN
029600060404     C                   ELSE
029700070502     C                   SETON                                        31
029800060510     C                   Z-ADD     *zeros        VABNSP
029900060510     C                   Z-ADD     1             VABRMN
030000060404     C                   EVAL      vinmsg = %trimr(vinmsg)
030100060510     C                             + ' ' + 'VABNSP VABRMN'
030200060404     C                   ENDIF
030300040420     C* CAD
030400080307     C                   EVAL      PiStr=%trim(%subst(vindta:104:5))
030500010607     C                   EXSR      CHKNUM
030600010607     C                   IF        PiInt=*on
030700020204     C                   Z-ADD     PiVal         Num5_0
030800020204     C                   MOVEL(p)  Num5_0        VABCAD
030900010607     C                   ELSE
031000010607     C                   SETON                                        32
031100020204     C                   EVAL      VABCAD = *zeros
031200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031300020204     C                             + ' ' + 'VABCAD'
031400060316     C                   ENDIF
031500070502     C* Reperisco la provincia dal CAP e dalla località
031600070502     C                   IF        VABCAD <> *blanks AND
031700070502     C                             VABPRD  = *blanks
031800070502     C                   CLEAR                   TISI95DS
031900070502     C                   EVAL      I95TCN = '3'
032000070502     C                   Z-ADD     datcor        I95DAT
032100070502     C                   EVAL      I95CAP = VABCAD
032200070502     C                   EVAL      I95LOC = VABLOD
032300070502     C                   EVAL      I95NAR = VABNZD
032400070502     C                   CALL      'TISI95R'
032500070502     C                   PARM                    TISI95DS
032600070502     C                   EVAL      VABPRD = O95PRV
032700070502     C                   ENDIF
032800040420     C* NCL
032900080403     C                   EVAL      PiStr=%trim(%subst(vindta:139:3))
033000010607     C                   EXSR      CHKNUM
033100010607     C                   IF        PiInt=*on
033200070606     C                   Z-ADD     PiVal         VABNCL
033300010607     C                   ELSE
033400010607     C                   SETON                                        32
033500010607     C                   Z-ADD     *zeros        VABNCL
033600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033700010607     C                             + ' ' + 'VABNCL'
033800010607     C                   ENDIF
033900071029     C* PKB
034000080307     C                   EVAL      PiStr=%trim(%subst(vindta:142:6))
034100010607     C                   EXSR      CHKNUM
034200010607     C                   IF        PiNum=*on
034300071003     C                   Z-ADD(H)  PiVal         VABPKB
034400010607     C                   ELSE
034500010607     C                   SETON                                        32
034600010607     C                   Z-ADD     *zeros        VABPKB
034700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034800010607     C                             + ' ' + 'VABPKB'
034900010607     C                   ENDIF
035000020322     C*
035100020322     C* Considerazioni finali su CBO/CAS
035200050908     C                   IF        FlgCAS = '1'
035300040713     C                   IF        VABCBO = '1'
035400020322     C                   EVAL      VABCBO = '4'
035500040713     C                   ENDIF
035600040713     C                   IF        VABCBO = '2'
035700040713     C                   EVAL      VABCBO = '6'
035800040713     C                   ENDIF
035900020322     C                   ENDIF
036000020204     C*
036100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036200020204     C                   EXSR      CHKIMPDIV
036300020204     C*
036400010607     C                   ENDSR
036500010607     C*----------------------------------------------------*
036600010601
036700020204
036800020204     C*----------------------------------------------------*
036900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
037000020204     C*----------------------------------------------------*
037100020204     C     CHKIMPDIV     BEGSR
037200020204     C*
037300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
037400020204     C                   Z-ADD     *zeros        wrkDec            9 9
037500020204     C*
037600020204     C* Come prima cosa effettuo considerazioni sulla divisa
037700020204     C                   IF        vabIAS > *zeros
037800020204     C                   IF        vabVAS <> 'EUR'
037900020204     C                   EVAL      vabVAS =  'ITL'
038000020204     C                   ENDIF
038100020204     C                   ENDIF
038200020204     C*
038300020204     C                   IF        vabCAS > *zeros
038400020204     C                   IF        vabVCA <> 'EUR'
038500020204     C                   EVAL      vabVCA =  'ITL'
038600020204     C                   ENDIF
038700020204     C                   ENDIF
038800020204     C*
038900020204     C                   IF        vabVMD > *zeros
039000020321     C                   IF        vabVAD <> 'EUR'
039100020204     C                   EVAL      vabVAD =  'ITL'
039200020204     C                   ENDIF
039300020204     C                   ENDIF
039400020204     C*
039500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
039600020204     C                   Z-ADD     vabIAS        wrkDec
039700020204     C                   IF        wrkDec > *zeros
039800020204     C                   IF        vabVAS = 'ITL'
039900020204     C                   EVAL      vabIAS = *zeros
040000020204     C                   ENDIF
040100020204     C                   ENDIF
040200020204     C*
040300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
040400020204     C                   Z-ADD     vabCAS        wrkDec
040500020204     C                   IF        wrkDec > *zeros
040600020204     C                   IF        vabVCA = 'ITL'
040700020204     C                   EVAL      vabCAS = *zeros
040800020204     C                   ENDIF
040900020204     C                   ENDIF
041000020204     C*
041100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
041200020204     C                   Z-ADD     vabVMD        wrkDec
041300020204     C                   IF        wrkDec > *zeros
041400020204     C                   IF        vabVAD = 'ITL'
041500020204     C                   EVAL      vabVMD = *zeros
041600020204     C                   ENDIF
041700020204     C                   ENDIF
041800020204     C*
041900020204     C                   ENDSR
042000020204     C***
042100020204
042200010330
042300010330
042400010330     C*----------------------------------------------------*
042500010330     C*  CONTROLLO NUMERICITA' CAMPI
042600010330     C*----------------------------------------------------*
042700010330     C     CHKNUM        BEGSR
042800010330     C*
042900010606     C                   IF        PiDecChr = *blanks
043000070502     C                   EVAL      PiDecChr = '.'
043100010606     C                   ENDIF
043200010606     C*
043300010606     C                   CALL(e)   'ISNUMERIC'
043400010330     C                   PARM                    PiStr            30
043500010606     C                   PARM                    PiDecChr          1
043600010330     C                   PARM      *ZEROS        PiVal            30 9
043700010330     C                   PARM      '0'           PiInt             1
043800010330     C                   PARM      '0'           PiNum             1
043900010330     C                   IF        %error
044000010606     C                   EVAL      PiNum=*off
044100010330     C                   ENDIF
044200010330     C*
044300010330     C                   ENDSR
044400010330     C***
044500010330
044600010601
044700010601
044800010601
044900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045000010601     C     repfil        BEGSR
045100010601     C*
045200010601     C                   if        invfil = *zeros and
045300010601     C                             depfil > *zeros and
045400010629     C                             (vinflg = *blanks or
045500010629     C                              vinflg = *zeros)
045600010601     C
045700010601     C                   eval      invfil = depfil
045800010601     C                   endif
045900010601     C*
046000010601     C                   if        depfil <> invfil and
046100010601     C                             invfil > *zeros
046200010601     C                   eval      flgMulti = '1'
046300010601     C                   if        vinflg = *blanks
046400010601     C                   add       1             cntNonEl
046500010601     C                   endif
046600010601     C                   endif
046700010601     C*
046800010601     C                   if        vinflg = '2'
046900010601     C                   eval      flgStato = '2'
047000010601     C                   endif
047100010601     C*
047200010601     C                   ENDSR
047300010601     C***
047400010601
047500010601
047600010601
047700010330
047800010330
047900010330
048000990920      /TITLE Invio dei dati al punto operativo.
048100000613     C     invio         BEGSR
048200990920     C*
048300990920     C                   reset                   dscmz
048400010601     C                   move      invfil        cmzdst
048500990920     C                   eval      cmzfld = vlrfou
048600990920     C                   eval      cmzmbd = vlrhdl
048700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048800000710     C                   if        prmfir = *blanks
048900050408     C                   eval      cmzfla = 'FIVAB00F'
049000050408     C                   eval      cmzmba = 'FIVAB00F'
049100000710     C                   else
049200000710     C                   eval      cmzfla = prmfir
049300000710     C                   eval      cmzmba = prmfir
049400000710     C                   endif
049500990920     C                   eval      cmznrr = *zeros
049600990920     C                   move      §ctrok        cmznrr
049700021018     C                   eval      cmzlba = vlrfl1
049800990920     C                   call(e)   'TIS711C'
049900990920     C                   parm                    dscmz
050000990921     C                   parm      *blanks       esito
050100990920     C                   if        %error
050200990920     C                             or cmzerr = '1'
050300990921     C                             or esito  = '1'
050400000710     C                   eval      wrkesito = '3'
050500990920     C                   endif
050600990920     C*
050700000613     C                   ENDSR
050800990910
050900010601
051000010601
051100010601
051200010601
051300010601      /TITLE Invio dei dati al punto operativo.
051400010601     C     opeini        BEGSR
051500010601     C*
051600010601     C* Inizializzo flag e contatori operativi
051700010601     C                   movel     '0'           flgGiro           1
051800010601     C                   movel     '0'           flgMulti          1
051900010601     C                   movel     '1'           flgStato          1
052000010615     C                   movel     '0'           flgOk             1
052100010601     C                   z-add     *zeros        cntNonEl         10 0
052200010601     C                   z-add     *zeros        depfil            3 0
052300010601     C                   z-add     *zeros        invfil            3 0
052400010601     C*
052500010601     C                   ENDSR
052600010601     C***
052700010601
052800010601
052900010601
053000010330
053100010330
053200000613     C     *inzsr        BEGSR
053300990910     C*
053400990910     C     *entry        plist
053500990920     C                   parm                    tivlrds
053600990921     C                   parm      wrkesito      esito
053700000724     C                   parm                    prmlit
053800000710     C                   parm                    prmfir
053900010330     C*
054000010330     C* CALCOLA LA DATA CORRENTE
054100010330     C                   time                    wn14             14 0
054200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054300010330     C                   z-add     wn8           g08dat
054400010330     C                   z-add     *zeros        g08inv
054500010330     C                   movel     '0'           g08err
054600010330     C                   call      'XSRDA8'
054700010330     C                   parm                    wlbda8
054800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054900000613     C*
055000000613     C                   ENDSR
055100000613     C***
