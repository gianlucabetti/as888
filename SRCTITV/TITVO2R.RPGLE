000100050908      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ADINOLFI  cod.2021071)
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
001400050330     D tisi95ds      e ds
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
003600010601     C*
003700010601     C                   seton                                        lr
003800010601
003900010601
004000010601
004100010601
004200010601     C*--------------------------------------------------------
004300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004400010601     C*--------------------------------------------------------
004500010601     C     PREELA        BEGSR
004600010601     C*
004700010601     C* SE OCCORRE SPEDIRE IN FILIALE
004800010601     C                   if        invfil <> *zeros and
004900010601     C                             flgGiro = '0'
005000010601     C*
005100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005200010601     C                   eval      flgGiro = '1'
005300010601     C*
005400010601     C                   endif
005500010601     C*
005600010601     C                   ENDSR
005700010601     C***
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006300010601     C*--------------------------------------------------------
006400010601     C     ENDELA        BEGSR
006500050908     C*
006600050908     C* Effettuo lancio TISI95 solo x chiusura
006700050908     C                   CLEAR                   TISI95DS
006800050908     C                   EVAL      I95TLA = 'C'
006900050908     C                   CALL      'TISI95R'
007000050908     C                   PARM                    TISI95DS
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
011400010601     C*
011500010330     C                   exsr      inzvar
011600010330     C                   exsr      defcam
011700010605     C                   exsr      impvab
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
013000061013     C*
013100150122     C* Se trattasi d bolle sul codice 2022249 o 2495355 occorre considerarle tutte come
013200061013     C* ritiri contestuali x cui reperisco il numero serie ORM in tabella 'OSR'
013300150122     C                   IF        VABCCM = 2022249 OR
013400150122     C                             VABCCM = 2495355
013500061013     C                   EVAL      VABGMA = 'RC'
013600061013     C                   ENDIF
013700090707     C*
013800090707     C*
013900010601     C*
014000010604     C* Ebbene...
014100010604     C*
014200010604     C  N31              ADD       1             §CTROK            7 0
014300010604     C   32              ADD       1             §CTRMO            7 0
014400010604     C   31              ADD       1             §CTRNO            7 0
014500020205     C*
014600050408     C* Scarico il buffer del FIVAB
014700050408     C  N31              WRITE     FIVAB000
014800061013     C*
014900010604     C*
015000020722     C                   endif
015100020722     C*
015200010604     C                   if        *in31 = *off and
015300010604     C                             *in32 = *off
015400010604     C                   eval      vinflg = '1'
015500010604     C                   else
015600010604     C                   eval      vinflg = '2'
015700010604     C                   endif
015800010604     C                   endif
015900010604     C*
016000010330     C                   else
016100010330     C                   eval      vinflg = '1'
016200010330     C                   endif
016300010601     C*
016400010601     C  N70              update    tivin000
016500010330     C*
016600010330     C  N70              ENDdo
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
019500050408     C                   if        %open(fivabwwr)
019600050408     C                   close     fivabwwr
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
022000020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022100010330     C*----------------------------------------------------*
022200010330     C     INZVAR        BEGSR
022300010330     C*
022400020204     C                   Z-ADD     *zeros        Num5_0            5 0
022500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022600020322     C                   MOVEL     '0'           FlgCAS            1
022700010330     C*
022800010330     C                   ENDSR
022900010330     C*----------------------------------------------------*
023000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023100010330     C*----------------------------------------------------*
023200010330     C     DEFCAM        BEGSR
023300010330     C*
023400020204     C* Inizializzo il buffer del record da scrivere
023500050408     C                   CLEAR                   FIVAB000
023600020204     C* Imposto i valori di default...
023700050908     C                   EVAL      VABCCM = 2021071
023800050908     C                   EVAL      VABLNP = 202
023900050908     C                   EVAL      VABCTR = 001
024000050908     C                   EVAL      VABTSP = 'C'
024100020204     C* ... e poi verifico se sono stati passati come parametri
024200020204     C                   IF        vlrppt > *blanks
024300040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024500020204     C                   EXSR      CHKNUM
024600020204     C                   IF        PiInt=*on
024700020204     C                   Z-ADD     PiVal         VABCCM
024800020204     C                   ENDIF
024900040301     C                   ENDIF
025000040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025200020204     C                   EXSR      CHKNUM
025300020204     C                   IF        PiInt=*on
025400020204     C                   Z-ADD     PiVal         VABLNP
025500020204     C                   ENDIF
025600040301     C                   ENDIF
025700040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025900020204     C                   EXSR      CHKNUM
026000020204     C                   IF        PiInt=*on
026100020204     C                   Z-ADD     PiVal         VABCTR
026200040301     C                   ENDIF
026300020204     C                   ENDIF
026400020204     C                   ENDIF
026500020204     C*
026600010330     C                   ENDSR
026700010607     C*----------------------------------------------------*
026800050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026900010607     C*----------------------------------------------------*
027000010607     C     IMPVAB        BEGSR
027100010607     C*
027200010607     C                   SETOFF                                       3132
027300010607     C*
027400010607     C* Reperimento campi ALFA
027500010607     C*
027600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027700050908     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
027800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028000020117     C* ==
028100050908     C                   EVAL      VABRD2=%trim(%subst(vindta:36:35))
028200050908     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
028300050908     C                   EVAL      VABLOD=%trim(%subst(vindta:115:35))
028400091124     C***                EVAL      VABRMA=%trim(%subst(vindta:164:13))
028500091124     C                   EVAL      VABRMA='C/TERMORETRATTI'
028600050908     C                   EVAL      VABNOT=%trim(%subst(vindta:199:35))
028700050908     C                   EVAL      VABNT2=%trim(%subst(vindta:234:35))
028800050908     C* CBO
028900050908     C                   IF        %trim(%subst(vindta:177:11)) = 'FRANCO'
029000050908     C                   EVAL      VABCBO = '1'
029100050908     C                   ENDIF
029200050908     C                   IF        %trim(%subst(vindta:177:11)) = 'ASSEGNATO'
029300050908     C                   EVAL      VABCBO = '2'
029400050908     C                   ENDIF
029500010607     C*
029600010607     C* Reperimento campi NUMERICI
029700020722     C                   MOVEL     DATCOR        VABAAS
029800020722     C                   MOVE      DATCOR        VABMGS
029900050518
030000050518     C* RMN
030100050908     C                   EVAL      PiStr=%trim(%subst(vindta:164:13))
030200050408     C                   EXSR      CHKNUM
030300050408     C                   IF        PiInt=*on
030400050408     C                   Z-ADD     PiVal         VABRMN
030500050908     C                   Z-ADD     PiVal         VABNSP
030600050408     C                   ELSE
030700050518     C                   SETON                                        32
030800050408     C                   Z-ADD     *zeros        VABRMN
030900050408     C                   EVAL      vinmsg = %trimr(vinmsg)
031000050908     C                             + ' ' + 'VABRMN VABNSP'
031100050408     C                   ENDIF
031200040420     C* CAD
031300050908     C                   EVAL      PiStr=%trim(%subst(vindta:106:9))
031400010607     C                   EXSR      CHKNUM
031500010607     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         Num5_0
031700020204     C                   MOVEL(p)  Num5_0        VABCAD
031800010607     C                   ELSE
031900010607     C                   SETON                                        32
032000020204     C                   EVAL      VABCAD = *zeros
032100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032200020204     C                             + ' ' + 'VABCAD'
032300010607     C                   ENDIF
032400050908     C* Reperisco la provincia dal CAP e dalla località
032500050908     C                   IF        VABCAD <> *blanks
032600050908     C                   CLEAR                   TISI95DS
032700050908     C                   EVAL      I95TCN = '3'
032800050908     C                   Z-ADD     datcor        I95DAT
032900050908     C                   EVAL      I95CAP = VABCAD
033000050908     C                   EVAL      I95LOC = VABLOD
033100050908     C                   EVAL      I95NAR = VABNZD
033200050908     C                   CALL      'TISI95R'
033300050908     C                   PARM                    TISI95DS
033400050908     C                   EVAL      VABPRD = O95PRV
033500050908     C                   ENDIF
033600040420     C* NCL
033700050908     C                   EVAL      PiStr=%trim(%subst(vindta:150:6))
033800010607     C                   EXSR      CHKNUM
033900010607     C                   IF        PiInt=*on
034000010607     C                   Z-ADD     PiVal         VABNCL
034100010607     C                   ELSE
034200010607     C                   SETON                                        32
034300010607     C                   Z-ADD     *zeros        VABNCL
034400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034500010607     C                             + ' ' + 'VABNCL'
034600010607     C                   ENDIF
034700040420     C* PKB
034800050908     C                   EVAL      PiStr=%trim(%subst(vindta:157:6))
034900010607     C                   EXSR      CHKNUM
035000010607     C                   IF        PiNum=*on
035100050330     C                   Z-ADD(H)  PiVal         VABPKB
035200010607     C                   ELSE
035300010607     C                   SETON                                        32
035400010607     C                   Z-ADD     *zeros        VABPKB
035500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035600010607     C                             + ' ' + 'VABPKB'
035700010607     C                   ENDIF
035800050908     C* CAS
035900050908     C                   IF        %trim(%subst(vindta:188:9)) <> '0,00'
036000050908     C                   EVAL      FlgCAS = '1'
036100050908     C                   EVAL      PiStr=%trim(%subst(vindta:188:9))
036200050908     C                   EXSR      CHKNUM
036300050908     C                   IF        PiNum=*on
036400050908     C                   Z-ADD     PiVal         VABCAS
036500050908     C                   EVAL      VABVCA = 'EUR'
036600050908     C                   EVAL      VABTIC = 'BM'
036700050908     C                   ELSE
036800050908     C                   SETON                                        32
036900050908     C                   Z-ADD     *zeros        VABCAS
037000050908     C                   EVAL      vinmsg = %trimr(vinmsg)
037100050908     C                             + ' ' + 'VABCAS'
037200050908     C                   ENDIF
037300050908     C                   ENDIF
037400050920     C* VMD/IAS
037500050908     C                   IF        %trim(%subst(vindta:269:14)) <> '0,000'
037600050908     C                   EVAL      PiStr=%trim(%subst(vindta:269:14))
037700050908     C                   EXSR      CHKNUM
037800050908     C                   IF        PiNum=*on
037900050908     C                   Z-ADD     PiVal         VABVMD
038000050920     C                   Z-ADD     PiVal         VABIAS
038100050914     C                   EVAL      VABVAD = 'EUR'
038200050920     C                   EVAL      VABVAS = 'EUR'
038300050908     C                   ELSE
038400050908     C                   SETON                                        32
038500050908     C                   Z-ADD     *zeros        VABVMD
038600050920     C                   Z-ADD     *zeros        VABIAS
038700050908     C                   EVAL      vinmsg = %trimr(vinmsg)
038800050920     C                             + ' ' + 'VABVMD VABIAS'
038900050908     C                   ENDIF
039000050908     C                   ENDIF
039100020322     C*
039200020322     C* Considerazioni finali su CBO/CAS
039300050908     C                   IF        FlgCAS = '1'
039400040713     C                   IF        VABCBO = '1'
039500020322     C                   EVAL      VABCBO = '4'
039600040713     C                   ENDIF
039700040713     C                   IF        VABCBO = '2'
039800040713     C                   EVAL      VABCBO = '6'
039900040713     C                   ENDIF
040000020322     C                   ENDIF
040100020204     C*
040200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040300020204     C                   EXSR      CHKIMPDIV
040400020204     C*
040500010607     C                   ENDSR
040600010607     C*----------------------------------------------------*
040700010601
040800020204
040900020204     C*----------------------------------------------------*
041000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041100020204     C*----------------------------------------------------*
041200020204     C     CHKIMPDIV     BEGSR
041300020204     C*
041400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041500020204     C                   Z-ADD     *zeros        wrkDec            9 9
041600020204     C*
041700020204     C* Come prima cosa effettuo considerazioni sulla divisa
041800020204     C                   IF        vabIAS > *zeros
041900020204     C                   IF        vabVAS <> 'EUR'
042000020204     C                   EVAL      vabVAS =  'ITL'
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C                   IF        vabCAS > *zeros
042500020204     C                   IF        vabVCA <> 'EUR'
042600020204     C                   EVAL      vabVCA =  'ITL'
042700020204     C                   ENDIF
042800020204     C                   ENDIF
042900020204     C*
043000020204     C                   IF        vabVMD > *zeros
043100020321     C                   IF        vabVAD <> 'EUR'
043200020204     C                   EVAL      vabVAD =  'ITL'
043300020204     C                   ENDIF
043400020204     C                   ENDIF
043500020204     C*
043600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043700020204     C                   Z-ADD     vabIAS        wrkDec
043800020204     C                   IF        wrkDec > *zeros
043900020204     C                   IF        vabVAS = 'ITL'
044000020204     C                   EVAL      vabIAS = *zeros
044100020204     C                   ENDIF
044200020204     C                   ENDIF
044300020204     C*
044400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044500020204     C                   Z-ADD     vabCAS        wrkDec
044600020204     C                   IF        wrkDec > *zeros
044700020204     C                   IF        vabVCA = 'ITL'
044800020204     C                   EVAL      vabCAS = *zeros
044900020204     C                   ENDIF
045000020204     C                   ENDIF
045100020204     C*
045200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045300020204     C                   Z-ADD     vabVMD        wrkDec
045400020204     C                   IF        wrkDec > *zeros
045500020204     C                   IF        vabVAD = 'ITL'
045600020204     C                   EVAL      vabVMD = *zeros
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C                   ENDSR
046100020204     C***
046200020204
046300010330
046400010330
046500010330     C*----------------------------------------------------*
046600010330     C*  CONTROLLO NUMERICITA' CAMPI
046700010330     C*----------------------------------------------------*
046800010330     C     CHKNUM        BEGSR
046900010330     C*
047000010606     C                   IF        PiDecChr = *blanks
047100010606     C                   EVAL      PiDecChr = ','
047200010606     C                   ENDIF
047300010606     C*
047400010606     C                   CALL(e)   'ISNUMERIC'
047500010330     C                   PARM                    PiStr            30
047600010606     C                   PARM                    PiDecChr          1
047700010330     C                   PARM      *ZEROS        PiVal            30 9
047800010330     C                   PARM      '0'           PiInt             1
047900010330     C                   PARM      '0'           PiNum             1
048000010330     C                   IF        %error
048100010606     C                   EVAL      PiNum=*off
048200010330     C                   ENDIF
048300010330     C*
048400010330     C                   ENDSR
048500010330     C***
048600010330
048700010601
048800010601
048900010601
049000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049100010601     C     repfil        BEGSR
049200010601     C*
049300010601     C                   if        invfil = *zeros and
049400010601     C                             depfil > *zeros and
049500010629     C                             (vinflg = *blanks or
049600010629     C                              vinflg = *zeros)
049700010601     C
049800010601     C                   eval      invfil = depfil
049900010601     C                   endif
050000010601     C*
050100010601     C                   if        depfil <> invfil and
050200010601     C                             invfil > *zeros
050300010601     C                   eval      flgMulti = '1'
050400010601     C                   if        vinflg = *blanks
050500010601     C                   add       1             cntNonEl
050600010601     C                   endif
050700010601     C                   endif
050800010601     C*
050900010601     C                   if        vinflg = '2'
051000010601     C                   eval      flgStato = '2'
051100010601     C                   endif
051200010601     C*
051300010601     C                   ENDSR
051400010601     C***
051500010601
051600010601
051700010601
051800010330
051900010330
052000010330
052100990920      /TITLE Invio dei dati al punto operativo.
052200000613     C     invio         BEGSR
052300990920     C*
052400990920     C                   reset                   dscmz
052500010601     C                   move      invfil        cmzdst
052600990920     C                   eval      cmzfld = vlrfou
052700990920     C                   eval      cmzmbd = vlrhdl
052800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052900000710     C                   if        prmfir = *blanks
053000050408     C                   eval      cmzfla = 'FIVAB00F'
053100050408     C                   eval      cmzmba = 'FIVAB00F'
053200000710     C                   else
053300000710     C                   eval      cmzfla = prmfir
053400000710     C                   eval      cmzmba = prmfir
053500000710     C                   endif
053600990920     C                   eval      cmznrr = *zeros
053700990920     C                   move      §ctrok        cmznrr
053800021018     C                   eval      cmzlba = vlrfl1
053900990920     C                   call(e)   'TIS711C'
054000990920     C                   parm                    dscmz
054100990921     C                   parm      *blanks       esito
054200990920     C                   if        %error
054300990920     C                             or cmzerr = '1'
054400990921     C                             or esito  = '1'
054500000710     C                   eval      wrkesito = '3'
054600990920     C                   endif
054700990920     C*
054800000613     C                   ENDSR
054900990910
055000010601
055100010601
055200010601
055300010601
055400010601      /TITLE Invio dei dati al punto operativo.
055500010601     C     opeini        BEGSR
055600010601     C*
055700010601     C* Inizializzo flag e contatori operativi
055800010601     C                   movel     '0'           flgGiro           1
055900010601     C                   movel     '0'           flgMulti          1
056000010601     C                   movel     '1'           flgStato          1
056100010615     C                   movel     '0'           flgOk             1
056200010601     C                   z-add     *zeros        cntNonEl         10 0
056300010601     C                   z-add     *zeros        depfil            3 0
056400010601     C                   z-add     *zeros        invfil            3 0
056500010601     C*
056600010601     C                   ENDSR
056700010601     C***
056800061013
056900061013
057000010601
057100010601
057200010601
057300010330
057400010330
057500000613     C     *inzsr        BEGSR
057600990910     C*
057700990910     C     *entry        plist
057800990920     C                   parm                    tivlrds
057900990921     C                   parm      wrkesito      esito
058000000724     C                   parm                    prmlit
058100000710     C                   parm                    prmfir
058200010330     C*
058300010330     C* CALCOLA LA DATA CORRENTE
058400010330     C                   time                    wn14             14 0
058500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058600010330     C                   z-add     wn8           g08dat
058700010330     C                   z-add     *zeros        g08inv
058800010330     C                   movel     '0'           g08err
058900010330     C                   call      'XSRDA8'
059000010330     C                   parm                    wlbda8
059100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059200000613     C*
059300000613     C                   ENDSR
059400000613     C***
