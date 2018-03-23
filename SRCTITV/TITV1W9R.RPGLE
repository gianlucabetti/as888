000100070718      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021014     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400041025     D tisi95ds      e ds
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
003700041025     C                   exsr      rwvab
003800041025     C*
003900041025     C* Esegue lancio TISI95R solo x chiusura
004000041025     C                   CLEAR                   TISI95DS
004100041025     C                   EVAL      I95TLA = 'C'
004200041025     C                   CALL      'TISI95R'
004300041025     C                   PARM                    TISI95DS
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
011100010330     C*
011200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011300010711     C                   if        vinflg = *blanks or vinflg = '0'
011400010711     C                   clear                   vinmsg
011500010711     C                   endif
011600010601     C*
011700010330     C                   exsr      inzvar
011800010330     C                   exsr      defcam
011900010605     C                   exsr      impvab
012000010601     C*
012100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012200010605     C                   eval      depfil = VABLNP
012300010601     C                   exsr      repfil
012400010601     C                   if        depfil = invfil
012500021025     C                   if        vlrpoi = 999
012600021025     C                   MOVE(P)   invfil        VABFGS
012700021025     C                   else
012800021025     C                   MOVE(P)   vlrpoi        VABFGS
012900021025     C                   endif
013000010601     C*
013100010601     C                   exsr      PREELA
013200010601     C*
013300010604     C* Ebbene...
013400010604     C*
013500010604     C  N31              ADD       1             §CTROK            7 0
013600010604     C   32              ADD       1             §CTRMO            7 0
013700010604     C   31              ADD       1             §CTRNO            7 0
013800020205     C*
013900040301     C* Scarico il buffer del FIVAB
014000021014     C  N31              WRITE     FIVAB000
014100010604     C*
014200020722     C                   endif
014300020722     C*
014400010604     C                   if        *in31 = *off and
014500010604     C                             *in32 = *off
014600010604     C                   eval      vinflg = '1'
014700010604     C                   else
014800010604     C                   eval      vinflg = '2'
014900010604     C                   endif
015000010604     C                   endif
015100010604     C*
015200010330     C                   else
015300010330     C                   eval      vinflg = '1'
015400010330     C                   endif
015500010601     C*
015600010601     C  N70              update    tivin000
015700010330     C*
015800010330     C  N70              ENDdo
015900010601     C
016000010601     C*
016100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016200010601     C                   if        cntNonEl = *zeros or
016300010601     C                             flgMulti = '0'
016400010330     C* Se non ci sono record con errori ...
016500010601     C                   if        §ctrno = 0 and
016600010604     C                             §ctrmo = 0 and
016700010601     C                             flgStato <> '2'
016800010330     C* ... restituisco esito OK.
016900010330     C                   eval      wrkesito = '0'
017000010330     C                   else
017100010330     C                   if        §ctrok > 0
017200010330     C                   eval      wrkesito = '1'
017300010330     C                   else
017400010615     C                   if        flgOk = '0'
017500010615     C                   eval      wrkesito = '2'
017600010615     C                   else
017700010615     C                   eval      wrkesito = '6'
017800010615     C                   endif
017900010330     C                   endif
018000010330     C                   endif
018100010601     C                   else
018200010601     C                   eval      wrkesito = '9'
018300010601     C                   endif
018400010330     C*
018500010330     C                   if        %open(tivin00r)
018600010330     C                   close     tivin00r
018700010330     C                   endif
018800021014     C                   if        %open(fivabwwr)
018900021014     C                   close     fivabwwr
019000010330     C                   endif
019100010601     C*
019200010601     C                   if        vlrpoi <> 999
019300010601     C                   eval      invfil = vlrpoi
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        §ctrok > 0
019700010601     C                             and invfil > *zeros
019800010330     C                   exsr      invio
019900010330     C                   endif
020000010601     C*
020100010618     C                   if        flgGiro = '1'
020200010601     C                   exsr      endela
020300010618     C                   endif
020400010330     C*
020500010330     C                   ENDSR
020600010330     C***
020700010330
020800010601
020900010601
021000010601
021100010601
021200010330     C*----------------------------------------------------*
021300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400010330     C*----------------------------------------------------*
021500010330     C     INZVAR        BEGSR
021600010330     C*
021700020204     C                   Z-ADD     *zeros        Num5_0            5 0
021800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021900020322     C                   MOVEL     '0'           FlgCAS            1
022000010330     C*
022100010330     C                   ENDSR
022200010330     C*----------------------------------------------------*
022300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022400010330     C*----------------------------------------------------*
022500010330     C     DEFCAM        BEGSR
022600010330     C*
022700020204     C* Inizializzo il buffer del record da scrivere
022800021014     C                   CLEAR                   FIVAB000
022900020204     C* Imposto i valori di default...
023000100416     C                   EVAL      VABCCM = 0055103
023100100416     C                   EVAL      VABLNP = 005
023200040713     C                   EVAL      VABCTR = 000
023300100416     C                   EVAL      VABCBO = '1'
023400100416     C                   EVAL      VABTIC = *blanks
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
026200021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026300010607     C*----------------------------------------------------*
026400010607     C     IMPVAB        BEGSR
026500010607     C*
026600010607     C                   SETOFF                                       3132
026700010607     C*
026800010607     C* Reperimento campi ALFA
026900010607     C*
027000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027100041012     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
027200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027400020117     C* ==
027500041012     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
027600041012     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
027700041012     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
027800041012     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
027900071204     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
028000041012     C                   EVAL      VABNOT=%trim(%subst(vindta:464:35))
028100041012     C                   EVAL      VABNT2=%trim(%subst(vindta:464+35:60-35))
028200100416     C*
028300110211     C* CAS
028400100416     C                   IF        %trim(%subst(vindta:301:3)) = '2'
028500100416     C                   MOVEL     '1'           FlgCAS
028600100416     C                   ENDIF
028700110211     C*
028800110211     C* CBO
028900110211     C                   IF        %trim(%subst(vindta:431:3)) = 'Ass' OR
029000110211     C                             %trim(%subst(vindta:431:3)) = 'ASS' OR
029100110211     C                             %trim(%subst(vindta:431:3)) = 'ass'
029200110211     C                   EVAL      VABCBO = '2'
029300110211     C                   ENDIF
029400010607     C*
029500010607     C* Reperimento campi NUMERICI
029600020722     C                   MOVEL     DATCOR        VABAAS
029700020722     C                   MOVE      DATCOR        VABMGS
029800071204     C* NSP
029900041012     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
030000020304     C                   EXSR      CHKNUM
030100020304     C                   IF        PiInt=*on
030200040301     C                   Z-ADD     PiVal         VABNSP
030300020304     C                   ELSE
030400020722     C                   SETON                                        31
030500020304     C                   Z-ADD     *zeros        VABNSP
030600020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030700071204     C                             + ' ' + 'VABNSP'
030800020304     C                   ENDIF
030900071204     C* RMN
031000100416     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
031100071204     C                   EXSR      CHKNUM
031200071204     C                   IF        PiInt=*on
031300071204     C                   Z-ADD     PiVal         VABRMN
031400071204     C                   ELSE
031500071204     C                   SETON                                        31
031600071204     C                   Z-ADD     *zeros        VABRMN
031700071204     C                   EVAL      vinmsg = %trimr(vinmsg)
031800071204     C                             + ' ' + 'VABRMN'
031900071204     C                   ENDIF
032000040420     C* CAD
032100041012     C                   EVAL      PiStr=%trim(%subst(vindta:316:7))
032200010607     C                   EXSR      CHKNUM
032300010607     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         Num5_0
032500020204     C                   MOVEL(p)  Num5_0        VABCAD
032600010607     C                   ELSE
032700010607     C                   SETON                                        32
032800020204     C                   EVAL      VABCAD = *zeros
032900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033000020204     C                             + ' ' + 'VABCAD'
033100010607     C                   ENDIF
033200041025     C* Reperisco la provincia dal CAP e dalla località
033300041025     C                   IF        VABPRD  = *blanks AND
033400041025     C                             VABCAD <> *blanks
033500041025     C                   CLEAR                   TISI95DS
033600041025     C                   EVAL      I95TCN = '3'
033700041025     C                   Z-ADD     datcor        I95DAT
033800041025     C                   EVAL      I95CAP = VABCAD
033900041025     C                   EVAL      I95LOC = VABLOD
034000041025     C                   CALL      'TISI95R'
034100041025     C                   PARM                    TISI95DS
034200041025     C                   EVAL      VABPRD = O95PRV
034300041025     C                   ENDIF
034400040420     C* NCL
034500041012     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
034600010607     C                   EXSR      CHKNUM
034700010607     C                   IF        PiInt=*on
034800010607     C                   Z-ADD     PiVal         VABNCL
034900010607     C                   ELSE
035000010607     C                   SETON                                        32
035100010607     C                   Z-ADD     *zeros        VABNCL
035200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035300010607     C                             + ' ' + 'VABNCL'
035400010607     C                   ENDIF
035500040420     C* PKB
035600041012     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
035700010607     C                   EXSR      CHKNUM
035800010607     C                   IF        PiNum=*on
035900041012     C                   EVAL(H)   VABPKB = PiVal / 100                         * gestisco 2 dec.
036000010607     C                   ELSE
036100010607     C                   SETON                                        32
036200010607     C                   Z-ADD     *zeros        VABPKB
036300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036400010607     C                             + ' ' + 'VABPKB'
036500010607     C                   ENDIF
036600041012     C* VLB
036700041012     C                   EVAL      PiStr=%trim(%subst(vindta:385:7))
036800041012     C                   EXSR      CHKNUM
036900041012     C                   IF        PiNum=*on
037000041012     C                   EVAL(H)   VABVLB = PiVal / 100                         * gestisco 2 dec.
037100041012     C                   ELSE
037200041012     C                   SETON                                        32
037300041012     C                   Z-ADD     *zeros        VABVLB
037400041012     C                   EVAL      vinmsg = %trimr(vinmsg)
037500041012     C                             + ' ' + 'VABVLB'
037600041012     C                   ENDIF
037700041012     C* DCR
037800041012     C                   IF        %trim(%subst(vindta:529:1)) = '1'
037900041012     C                   EVAL      PiStr=%trim(%subst(vindta:54:8))
038000041012     C                   EXSR      CHKNUM
038100041012     C                   IF        PiInt=*on
038200041012     C                   Z-ADD     PiVal         VABDCR
038300041012     C                   ELSE
038400041012     C                   SETON                                        32
038500041012     C                   Z-ADD     *zeros        VABDCR
038600041012     C                   EVAL      vinmsg = %trimr(vinmsg)
038700041012     C                             + ' ' + 'VABDCR'
038800041012     C                   ENDIF
038900041012     C                   ENDIF
039000041012     C* CAS/VCA
039100041012     C                   IF        %trim(%subst(vindta:392:4)) = 'EUR'
039200041012     C                   EVAL      VABVCA = %trim(%subst(vindta:392:4))
039300041012     C                   MOVEL     '1'           FlgCAS
039400041012     C* Determino il tipo incasso contrassegno (trascodifica)
039500070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'A'
039600070718     C*                  EVAL      VABTIC = *blanks
039700070718     C*                  ENDIF
039800041012     C                   IF        %trim(%subst(vindta:524:1)) = 'B'
039900041012     C                   EVAL      VABTIC = 'BM'
040000041012     C                   ENDIF
040100070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'C'
040200070718     C*                  EVAL      VABTIC = 'CM'
040300070718     C*                  ENDIF
040400070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'D'
040500070718     C*                  EVAL      VABTIC = 'BM'
040600070718     C*                  ENDIF
040700070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'E'
040800070718     C*                  EVAL      VABTIC = 'CM'
040900070718     C*                  ENDIF
041000070718     C                   IF        %trim(%subst(vindta:524:1)) = 'P'
041100070718     C                   EVAL      VABTIC = 'TM'
041200070718     C                   ENDIF
041300041012     C*
041400041012     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
041500041012     C                   EXSR      CHKNUM
041600041012     C                   IF        PiNum=*on
041700041012     C                   EVAL      VABCAS = PiVal / 1000                        * gestisco 3 dec.
041800041012     C                   ELSE
041900041012     C                   SETON                                        32
042000041012     C                   Z-ADD     *zeros        VABCAS
042100041012     C                   EVAL      vinmsg = %trimr(vinmsg)
042200041012     C                             + ' ' + 'VABCAS'
042300041012     C                   ENDIF
042400041012     C                   ENDIF
042500020322     C*
042600020322     C* Considerazioni finali su CBO/CAS
042700020322     C                   IF        FlgCAS = '1'
042800040713     C                   IF        VABCBO = '1'
042900020322     C                   EVAL      VABCBO = '4'
043000040713     C                   ENDIF
043100040713     C                   IF        VABCBO = '2'
043200040713     C                   EVAL      VABCBO = '6'
043300040713     C                   ENDIF
043400020322     C                   ENDIF
043500020204     C*
043600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043700020204     C                   EXSR      CHKIMPDIV
043800020204     C*
043900010607     C                   ENDSR
044000010607     C*----------------------------------------------------*
044100010601
044200020204
044300020204     C*----------------------------------------------------*
044400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044500020204     C*----------------------------------------------------*
044600020204     C     CHKIMPDIV     BEGSR
044700020204     C*
044800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044900020204     C                   Z-ADD     *zeros        wrkDec            9 9
045000020204     C*
045100020204     C* Come prima cosa effettuo considerazioni sulla divisa
045200020204     C                   IF        vabIAS > *zeros
045300020204     C                   IF        vabVAS <> 'EUR'
045400020204     C                   EVAL      vabVAS =  'ITL'
045500020204     C                   ENDIF
045600020204     C                   ENDIF
045700020204     C*
045800020204     C                   IF        vabCAS > *zeros
045900020204     C                   IF        vabVCA <> 'EUR'
046000020204     C                   EVAL      vabVCA =  'ITL'
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C                   IF        vabVMD > *zeros
046500020321     C                   IF        vabVAD <> 'EUR'
046600020204     C                   EVAL      vabVAD =  'ITL'
046700020204     C                   ENDIF
046800020204     C                   ENDIF
046900020204     C*
047000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047100020204     C                   Z-ADD     vabIAS        wrkDec
047200020204     C                   IF        wrkDec > *zeros
047300020204     C                   IF        vabVAS = 'ITL'
047400020204     C                   EVAL      vabIAS = *zeros
047500020204     C                   ENDIF
047600020204     C                   ENDIF
047700020204     C*
047800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047900020204     C                   Z-ADD     vabCAS        wrkDec
048000020204     C                   IF        wrkDec > *zeros
048100020204     C                   IF        vabVCA = 'ITL'
048200020204     C                   EVAL      vabCAS = *zeros
048300020204     C                   ENDIF
048400020204     C                   ENDIF
048500020204     C*
048600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048700020204     C                   Z-ADD     vabVMD        wrkDec
048800020204     C                   IF        wrkDec > *zeros
048900020204     C                   IF        vabVAD = 'ITL'
049000020204     C                   EVAL      vabVMD = *zeros
049100020204     C                   ENDIF
049200020204     C                   ENDIF
049300020204     C*
049400020204     C                   ENDSR
049500020204     C***
049600020204
049700010330
049800010330
049900010330     C*----------------------------------------------------*
050000010330     C*  CONTROLLO NUMERICITA' CAMPI
050100010330     C*----------------------------------------------------*
050200010330     C     CHKNUM        BEGSR
050300010330     C*
050400010606     C                   IF        PiDecChr = *blanks
050500010606     C                   EVAL      PiDecChr = ','
050600010606     C                   ENDIF
050700010606     C*
050800010606     C                   CALL(e)   'ISNUMERIC'
050900010330     C                   PARM                    PiStr            30
051000010606     C                   PARM                    PiDecChr          1
051100010330     C                   PARM      *ZEROS        PiVal            30 9
051200010330     C                   PARM      '0'           PiInt             1
051300010330     C                   PARM      '0'           PiNum             1
051400010330     C                   IF        %error
051500010606     C                   EVAL      PiNum=*off
051600010330     C                   ENDIF
051700010330     C*
051800010330     C                   ENDSR
051900010330     C***
052000010330
052100010601
052200010601
052300010601
052400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052500010601     C     repfil        BEGSR
052600010601     C*
052700010601     C                   if        invfil = *zeros and
052800010601     C                             depfil > *zeros and
052900010629     C                             (vinflg = *blanks or
053000010629     C                              vinflg = *zeros)
053100010601     C
053200010601     C                   eval      invfil = depfil
053300010601     C                   endif
053400010601     C*
053500010601     C                   if        depfil <> invfil and
053600010601     C                             invfil > *zeros
053700010601     C                   eval      flgMulti = '1'
053800010601     C                   if        vinflg = *blanks
053900010601     C                   add       1             cntNonEl
054000010601     C                   endif
054100010601     C                   endif
054200010601     C*
054300010601     C                   if        vinflg = '2'
054400010601     C                   eval      flgStato = '2'
054500010601     C                   endif
054600010601     C*
054700010601     C                   ENDSR
054800010601     C***
054900010601
055000010601
055100010601
055200010330
055300010330
055400010330
055500990920      /TITLE Invio dei dati al punto operativo.
055600000613     C     invio         BEGSR
055700990920     C*
055800990920     C                   reset                   dscmz
055900010601     C                   move      invfil        cmzdst
056000990920     C                   eval      cmzfld = vlrfou
056100990920     C                   eval      cmzmbd = vlrhdl
056200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056300000710     C                   if        prmfir = *blanks
056400021014     C                   eval      cmzfla = 'FIVAB00F'
056500021014     C                   eval      cmzmba = 'FIVAB00F'
056600000710     C                   else
056700000710     C                   eval      cmzfla = prmfir
056800000710     C                   eval      cmzmba = prmfir
056900000710     C                   endif
057000990920     C                   eval      cmznrr = *zeros
057100990920     C                   move      §ctrok        cmznrr
057200021018     C                   eval      cmzlba = vlrfl1
057300990920     C                   call(e)   'TIS711C'
057400990920     C                   parm                    dscmz
057500990921     C                   parm      *blanks       esito
057600990920     C                   if        %error
057700990920     C                             or cmzerr = '1'
057800990921     C                             or esito  = '1'
057900000710     C                   eval      wrkesito = '3'
058000990920     C                   endif
058100990920     C*
058200000613     C                   ENDSR
058300990910
058400010601
058500010601
058600010601
058700010601
058800010601      /TITLE Invio dei dati al punto operativo.
058900010601     C     opeini        BEGSR
059000010601     C*
059100010601     C* Inizializzo flag e contatori operativi
059200010601     C                   movel     '0'           flgGiro           1
059300010601     C                   movel     '0'           flgMulti          1
059400010601     C                   movel     '1'           flgStato          1
059500010615     C                   movel     '0'           flgOk             1
059600010601     C                   z-add     *zeros        cntNonEl         10 0
059700010601     C                   z-add     *zeros        depfil            3 0
059800010601     C                   z-add     *zeros        invfil            3 0
059900010601     C*
060000010601     C                   ENDSR
060100010601     C***
060200010601
060300010601
060400010601
060500010330
060600010330
060700000613     C     *inzsr        BEGSR
060800990910     C*
060900990910     C     *entry        plist
061000990920     C                   parm                    tivlrds
061100990921     C                   parm      wrkesito      esito
061200000724     C                   parm                    prmlit
061300000710     C                   parm                    prmfir
061400010330     C*
061500010330     C* CALCOLA LA DATA CORRENTE
061600010330     C                   time                    wn14             14 0
061700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061800010330     C                   z-add     wn8           g08dat
061900010330     C                   z-add     *zeros        g08inv
062000010330     C                   movel     '0'           g08err
062100010330     C                   call      'XSRDA8'
062200010330     C                   parm                    wlbda8
062300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062400000613     C*
062500000613     C                   ENDSR
062600000613     C***
