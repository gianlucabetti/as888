000100051221      /TITLE Upload via Internet: traduzione in FIVABWWR. (x THELMA   Cod. 1050011)
000200051221      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ARTHE EL Cod. 1050213)
000300990908     H dftactgrp(*yes)
000400051221     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500050223     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
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
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010601     C*
003600010601     C                   exsr      opeini
003700010605     C                   exsr      rwvab
003800050223     C*
003900050223     C* Effettuo lancio TISI95 solo x chiusura
004000050223     C                   CLEAR                   TISI95DS
004100050223     C                   EVAL      I95TLA = 'C'
004200050223     C                   CALL      'TISI95R'
004300050223     C                   PARM                    TISI95DS
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
006900010601     C*--------------------------------------------------------
007000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007100010601     C*--------------------------------------------------------
007200010601     C     ENDELA        BEGSR
007300000616     C*
007400010601     C                   ENDSR
007500010601     C***
007600000613
007700010601
007800010601
007900010330     C*--------------------------------------------------------
008000040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008100010330     C*--------------------------------------------------------
008200010605     C     RWVAB         BEGSR
008300010330     C*
008400010330     C                   if        not %open(tivin00r)
008500010330     C                   open      tivin00r
008600010330     C                   endif
008700021014     C                   if        not %open(fivabwwr)
008800021014     C                   open      fivabwwr
008900010330     C                   endif
009000010330     C*
009100010604     C                   clear                   §CTROK
009200010604     C                   clear                   §CTRMO
009300010604     C                   clear                   §CTRNO
009400010330     C*
009500010330     C                   DO        *HIVAL
009600010330     C*
009700010330     C                   READ      tivin00r                               70
009800010618     C*
009900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010000010618     C                   if        vinflg = '1'
010100010618     C                   eval      flgOk = '1'
010200010618     C                   endif
010300010618     C*
010400010330     C                   if        vindta > *blanks
010500010330     C                   add       1             rrnum
010600010330     C*
010700010601     C                   if        *in70 = *off and
010800010330     C                             (vinflg = *blanks
010900010330     C                              or vinflg = '0'
011000010330     C                              or vinflg = '2')
011100050217     C*
011200050217     C                   clear                   FIVAB000
011300010330     C*
011400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011500010711     C                   if        vinflg = *blanks or vinflg = '0'
011600010711     C                   clear                   vinmsg
011700010711     C                   endif
011800010601     C*
011900010330     C                   exsr      inzvar
012000010330     C                   exsr      defcam
012100010605     C                   exsr      impvab
012200010601     C*
012300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012400010605     C                   eval      depfil = VABLNP
012500010601     C                   exsr      repfil
012600010601     C                   if        depfil = invfil
012700021025     C                   if        vlrpoi = 999
012800021025     C                   MOVE(P)   invfil        VABFGS
012900021025     C                   else
013000021025     C                   MOVE(P)   vlrpoi        VABFGS
013100021025     C                   endif
013200050114     C                   endif
013300010601     C*
013400010601     C                   exsr      PREELA
013500010601     C*
013600010604     C* Ebbene...
013700010604     C*
013800010604     C  N31              ADD       1             §CTROK            7 0
013900010604     C   32              ADD       1             §CTRMO            7 0
014000010604     C   31              ADD       1             §CTRNO            7 0
014100020205     C*
014200040301     C* Scarico il buffer del FIVAB
014300021014     C  N31              WRITE     FIVAB000
014400010604     C*
014500020722     C                   endif
014600020722     C*
014700010604     C                   if        *in31 = *off and
014800010604     C                             *in32 = *off
014900010604     C                   eval      vinflg = '1'
015000010604     C                   else
015100010604     C                   eval      vinflg = '2'
015200010604     C                   endif
015300010604     C*
015400010330     C                   else
015500010330     C                   eval      vinflg = '1'
015600010330     C                   endif
015700010601     C*
015800010601     C  N70              update    tivin000
015900010330     C*
016000010330     C  N70              ENDdo
016100010601     C
016200010601     C*
016300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016400010601     C                   if        cntNonEl = *zeros or
016500010601     C                             flgMulti = '0'
016600010330     C* Se non ci sono record con errori ...
016700010601     C                   if        §ctrno = 0 and
016800010604     C                             §ctrmo = 0 and
016900010601     C                             flgStato <> '2'
017000010330     C* ... restituisco esito OK.
017100010330     C                   eval      wrkesito = '0'
017200010330     C                   else
017300010330     C                   if        §ctrok > 0
017400010330     C                   eval      wrkesito = '1'
017500010330     C                   else
017600010615     C                   if        flgOk = '0'
017700010615     C                   eval      wrkesito = '2'
017800010615     C                   else
017900010615     C                   eval      wrkesito = '6'
018000010615     C                   endif
018100010330     C                   endif
018200010330     C                   endif
018300010601     C                   else
018400010601     C                   eval      wrkesito = '9'
018500010601     C                   endif
018600010330     C*
018700010330     C                   if        %open(tivin00r)
018800010330     C                   close     tivin00r
018900010330     C                   endif
019000021014     C                   if        %open(fivabwwr)
019100021014     C                   close     fivabwwr
019200010330     C                   endif
019300010601     C*
019400010601     C                   if        vlrpoi <> 999
019500010601     C                   eval      invfil = vlrpoi
019600010601     C                   endif
019700010330     C*
019800010330     C                   if        §ctrok > 0
019900010601     C                             and invfil > *zeros
020000010330     C                   exsr      invio
020100010330     C                   endif
020200010601     C*
020300010618     C                   if        flgGiro = '1'
020400010601     C                   exsr      endela
020500010618     C                   endif
020600010330     C*
020700010330     C                   ENDSR
020800010330     C***
020900010330
021000010601
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
023000021014     C                   CLEAR                   FIVAB000
023100020204     C* Imposto i valori di default...
023200060109     C                   EVAL      VABCCM = 1050011
023300060109     C                   EVAL      VABLNP = 105
023400050218     C                   EVAL      VABCTR = 300
023500041108     C                   EVAL      VABCBO = '1'
023600050218     C                   EVAL      VABTSP = 'C'
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
026400021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026500010607     C*----------------------------------------------------*
026600010607     C     IMPVAB        BEGSR
026700010607     C*
026800010607     C                   SETOFF                                       3132
026900010607     C*
027000010607     C* Reperimento campi ALFA
027100010607     C*
027200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027300050218     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
027400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027600020117     C* ==
027700050218     C                   EVAL      VABRD2=%trim(%subst(vindta:1+35:35))
027800050218     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
027900050221     C                   EVAL      VABLOD=%trim(%subst(vindta:115:35))
028000111130     C                   EVAL      VABRMA=%trim(%subst(vindta:173:6))
028100050221     C                   EVAL      VABNOT=%trim(%subst(vindta:203:35))
028200050221     C                   EVAL      VABNT2=%trim(%subst(vindta:203+35:35))
028300010607     C*
028400010607     C* Reperimento campi NUMERICI
028500020722     C                   MOVEL     DATCOR        VABAAS
028600020722     C                   MOVE      DATCOR        VABMGS
028700040420     C* NSP/RMN
028800111130     C                   EVAL      PiStr=%trim(%subst(vindta:173:6))
028900020304     C                   EXSR      CHKNUM
029000020304     C                   IF        PiInt=*on
029100040301     C                   Z-ADD     PiVal         VABNSP
029200040301     C                   Z-ADD     PiVal         VABRMN
029300020304     C                   ELSE
029400050217     C                   SETON                                        31
029500050217     C                   Z-ADD     *zeros        VABNSP
029600050217     C                   Z-ADD     *zeros        VABRMN
029700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029800040301     C                             + ' ' + 'VABNSP VABRMN'
029900020304     C                   ENDIF
030000050128     C* CAD
030100050218     C                   EVAL      PiStr=%trim(%subst(vindta:106:5))
030200050128     C                   EXSR      CHKNUM
030300050128     C                   IF        PiInt=*on
030400050128     C                   Z-ADD     PiVal         Num5_0
030500050128     C                   MOVEL(p)  Num5_0        VABCAD
030600050128     C                   ELSE
030700050128     C                   SETON                                        32
030800050128     C                   EVAL      VABCAD = *zeros
030900050128     C                   EVAL      vinmsg = %trimr(vinmsg)
031000050128     C                             + ' ' + 'VABCAD'
031100050128     C                   ENDIF
031200050223     C* PRD (reperita mediante TISI95)
031300050223     C                   IF        VABCAD <> *blanks
031400050223     C                   CLEAR                   TISI95DS
031500050223     C                   EVAL      I95TCN = '3'
031600050223     C                   Z-ADD     datcor        I95DAT
031700050223     C                   EVAL      I95CAP = VABCAD
031800050223     C                   EVAL      I95LOC = VABLOD
031900050223     C                   CALL      'TISI95R'
032000050223     C                   PARM                    TISI95DS
032100050223     C                   EVAL      VABPRD = O95PRV
032200050223     C                   ENDIF
032300040420     C* NCL
032400050221     C                   EVAL      PiStr=%trim(%subst(vindta:150:5))
032500010607     C                   EXSR      CHKNUM
032600010607     C                   IF        PiInt=*on
032700010607     C                   Z-ADD     PiVal         VABNCL
032800010607     C                   ELSE
032900010607     C                   SETON                                        32
033000010607     C                   Z-ADD     *zeros        VABNCL
033100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033200010607     C                             + ' ' + 'VABNCL'
033300010607     C                   ENDIF
033400040420     C* PKB
033500050221     C                   EVAL      PiStr=%trim(%subst(vindta:155:9))
033600010607     C                   EXSR      CHKNUM
033700010607     C                   IF        PiNum=*on
033800050114     C                   Z-ADD     PiVal         VABPKB
033900010607     C                   ELSE
034000010607     C                   SETON                                        32
034100010607     C                   Z-ADD     *zeros        VABPKB
034200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034300010607     C                             + ' ' + 'VABPKB'
034400010607     C                   ENDIF
034500050216     C* CAS
034600050221     C                   IF        %trim(%subst(vindta:189:11)) <> *blanks
034700050216     C                   MOVEL     '1'           FlgCAS
034800050221     C                   EVAL      PiStr=%trim(%subst(vindta:189:11))
034900050217     C                   EVAL      VABVCA='EUR'
035000050221     C                   EVAL      VABTIC='BM'
035100050216     C                   EXSR      CHKNUM
035200050216     C                   IF        PiNum=*on
035300050216     C                   Z-ADD     PiVal         VABCAS
035400050216     C                   ELSE
035500050216     C                   SETON                                        32
035600050216     C                   EVAL      vinmsg = %trimr(vinmsg)
035700050216     C                             + ' ' + 'VABCAS'
035800050216     C                   ENDIF
035900050216     C                   ENDIF
036000020322     C*
036100020322     C* Considerazioni finali su CBO/CAS
036200020322     C                   IF        FlgCAS = '1'
036300040713     C                   IF        VABCBO = '1'
036400020322     C                   EVAL      VABCBO = '4'
036500040713     C                   ENDIF
036600040713     C                   IF        VABCBO = '2'
036700040713     C                   EVAL      VABCBO = '6'
036800040713     C                   ENDIF
036900020322     C                   ENDIF
037000020204     C*
037100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037200020204     C                   EXSR      CHKIMPDIV
037300020204     C*
037400010607     C                   ENDSR
037500010607     C*----------------------------------------------------*
037600010601
037700020204
037800020204     C*----------------------------------------------------*
037900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038000020204     C*----------------------------------------------------*
038100020204     C     CHKIMPDIV     BEGSR
038200020204     C*
038300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
038400020204     C                   Z-ADD     *zeros        wrkDec            9 9
038500020204     C*
038600020204     C* Come prima cosa effettuo considerazioni sulla divisa
038700020204     C                   IF        vabIAS > *zeros
038800020204     C                   IF        vabVAS <> 'EUR'
038900020204     C                   EVAL      vabVAS =  'ITL'
039000020204     C                   ENDIF
039100020204     C                   ENDIF
039200020204     C*
039300020204     C                   IF        vabCAS > *zeros
039400020204     C                   IF        vabVCA <> 'EUR'
039500020204     C                   EVAL      vabVCA =  'ITL'
039600020204     C                   ENDIF
039700020204     C                   ENDIF
039800020204     C*
039900020204     C                   IF        vabVMD > *zeros
040000020321     C                   IF        vabVAD <> 'EUR'
040100020204     C                   EVAL      vabVAD =  'ITL'
040200020204     C                   ENDIF
040300020204     C                   ENDIF
040400020204     C*
040500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
040600020204     C                   Z-ADD     vabIAS        wrkDec
040700020204     C                   IF        wrkDec > *zeros
040800020204     C                   IF        vabVAS = 'ITL'
040900020204     C                   EVAL      vabIAS = *zeros
041000020204     C                   ENDIF
041100020204     C                   ENDIF
041200020204     C*
041300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
041400020204     C                   Z-ADD     vabCAS        wrkDec
041500020204     C                   IF        wrkDec > *zeros
041600020204     C                   IF        vabVCA = 'ITL'
041700020204     C                   EVAL      vabCAS = *zeros
041800020204     C                   ENDIF
041900020204     C                   ENDIF
042000020204     C*
042100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
042200020204     C                   Z-ADD     vabVMD        wrkDec
042300020204     C                   IF        wrkDec > *zeros
042400020204     C                   IF        vabVAD = 'ITL'
042500020204     C                   EVAL      vabVMD = *zeros
042600020204     C                   ENDIF
042700020204     C                   ENDIF
042800020204     C*
042900020204     C                   ENDSR
043000020204     C***
043100020204
043200010330
043300010330
043400010330     C*----------------------------------------------------*
043500010330     C*  CONTROLLO NUMERICITA' CAMPI
043600010330     C*----------------------------------------------------*
043700010330     C     CHKNUM        BEGSR
043800010330     C*
043900010606     C                   IF        PiDecChr = *blanks
044000050217     C                   EVAL      PiDecChr = ','
044100010606     C                   ENDIF
044200010606     C*
044300010606     C                   CALL(e)   'ISNUMERIC'
044400010330     C                   PARM                    PiStr            30
044500010606     C                   PARM                    PiDecChr          1
044600010330     C                   PARM      *ZEROS        PiVal            30 9
044700010330     C                   PARM      '0'           PiInt             1
044800010330     C                   PARM      '0'           PiNum             1
044900010330     C                   IF        %error
045000010606     C                   EVAL      PiNum=*off
045100010330     C                   ENDIF
045200010330     C*
045300010330     C                   ENDSR
045400010330     C***
045500010330
045600010601
045700010601
045800010601
045900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
046000010601     C     repfil        BEGSR
046100010601     C*
046200010601     C                   if        invfil = *zeros and
046300010601     C                             depfil > *zeros and
046400010629     C                             (vinflg = *blanks or
046500010629     C                              vinflg = *zeros)
046600010601     C
046700010601     C                   eval      invfil = depfil
046800010601     C                   endif
046900010601     C*
047000010601     C                   if        depfil <> invfil and
047100010601     C                             invfil > *zeros
047200010601     C                   eval      flgMulti = '1'
047300010601     C                   if        vinflg = *blanks
047400010601     C                   add       1             cntNonEl
047500010601     C                   endif
047600010601     C                   endif
047700010601     C*
047800010601     C                   if        vinflg = '2'
047900010601     C                   eval      flgStato = '2'
048000010601     C                   endif
048100010601     C*
048200010601     C                   ENDSR
048300010601     C***
048400050217
048500050217
048600050217
048700050217
048800050217      /TITLE Invio dei dati al punto operativo.
048900050217     C     invio         BEGSR
049000050217     C*
049100050217     C                   reset                   dscmz
049200050217     C                   move      vlrpoi        cmzdst
049300050217     C                   eval      cmzfld = vlrfou
049400050217     C                   eval      cmzmbd = vlrhdl
049500050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
049600050217     C***                if        prmfir = *blanks
049700050217     C                   eval      cmzfla = 'FIVAB00F'
049800050217     C                   eval      cmzmba = 'FIVAB00F'
049900050217     C***                else
050000050217     C***                eval      cmzfla = prmfir
050100050217     C***                eval      cmzmba = prmfir
050200050217     C***                endif
050300050217     C                   eval      cmznrr = *zeros
050400050217     C                   move      §ctrok        cmznrr
050500050217     C                   eval      cmzlba = vlrfl1
050600050217     C                   call(e)   'TIS711C'
050700050217     C                   parm                    dscmz
050800050217     C                   parm      *blanks       esito
050900050217     C                   if        %error
051000050217     C                             or cmzerr = '1'
051100050217     C                             or esito  = '1'
051200050217     C                   eval      wrkesito = '3'
051300050217     C                   endif
051400050217     C*
051500050217     C                   ENDSR
051600050217     C***
051700050217
051800050217
051900050217
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
055000010330     C                   time                    wn14             14 0
055100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055200010330     C                   z-add     wn8           g08dat
055300010330     C                   z-add     *zeros        g08inv
055400010330     C                   movel     '0'           g08err
055500010330     C                   call      'XSRDA8'
055600010330     C                   parm                    wlbda8
055700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055800000613     C*
055900000613     C                   ENDSR
056000000613     C***
