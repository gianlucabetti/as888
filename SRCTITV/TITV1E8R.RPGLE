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
020700080321     C*
020800080321     C* Forzatura x boiata del cliente => se colli e peso a zero la bolla nn è da considerare
020900080321     C                   IF        VABNCL = *zeros AND VABPKB = *zeros
021000080321     C                   ELSE
021100070601     C*
021200070601     C  N31              WRITE     FIVAB000
021300080321     C                   ENDIF
021400070601     C*
021500070601     C                   ENDSR
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
023700080320     C                   EVAL      VABCCM = 0011443
023800080320     C                   EVAL      VABLNP = 125
023900080307     C                   EVAL      VABCTR = 000
024000070208     C                   EVAL      VABCBO = '1'
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
027500080320     C                   IF        %subst(vindta:327:113) = *blanks
027600080320     C                   EVAL      VABRSD=%trim(%subst(vindta:212:36))
027700080320     C                   EVAL      VABIND=%trim(%subst(vindta:248:35))
027800080320     C                   EVAL      VABLOD=%trim(%subst(vindta:283:35))
027900080320     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
028000080320     C* CAD
028100080320     C                   EVAL      PiStr=%trim(%subst(vindta:318:5))
028200080320     C                   EXSR      CHKNUM
028300080320     C                   IF        PiInt=*on
028400080320     C                   Z-ADD     PiVal         Num5_0
028500080320     C                   MOVEL(p)  Num5_0        VABCAD
028600080320     C                   ELSE
028700080320     C                   SETON                                        32
028800080320     C                   EVAL      VABCAD = *zeros
028900080320     C                   EVAL      vinmsg = %trimr(vinmsg)
029000080320     C                             + ' ' + 'VABCAD'
029100080320     C                   ENDIF
029200080320     C* Reperisco la provincia dal CAP e dalla località
029300080320     C                   IF        VABCAD <> *blanks AND
029400080320     C                             VABPRD  = *blanks
029500080320     C                   CLEAR                   TISI95DS
029600080320     C                   EVAL      I95TCN = '3'
029700080320     C                   Z-ADD     datcor        I95DAT
029800080320     C                   EVAL      I95CAP = VABCAD
029900080320     C                   EVAL      I95LOC = VABLOD
030000080320     C                   EVAL      I95NAR = VABNZD
030100080320     C                   CALL      'TISI95R'
030200080320     C                   PARM                    TISI95DS
030300080320     C                   EVAL      VABPRD = O95PRV
030400080320     C                   ENDIF
030500080320     C*
030600080320     C                   ELSE
030700080320     C* ...destinazione alternativa
030800080320     C                   EVAL      VABRSD=%trim(%subst(vindta:327:36))
030900080320     C                   EVAL      VABIND=%trim(%subst(vindta:363:35))
031000080320     C                   EVAL      VABLOD=%trim(%subst(vindta:398:35))
031100080320     C                   EVAL      VABPRD=%trim(%subst(vindta:438:2))
031200080320     C* CAD
031300080320     C                   EVAL      PiStr=%trim(%subst(vindta:433:5))
031400080320     C                   EXSR      CHKNUM
031500080320     C                   IF        PiInt=*on
031600080320     C                   Z-ADD     PiVal         Num5_0
031700080320     C                   MOVEL(p)  Num5_0        VABCAD
031800080320     C                   ELSE
031900080320     C                   SETON                                        32
032000080320     C                   EVAL      VABCAD = *zeros
032100080320     C                   EVAL      vinmsg = %trimr(vinmsg)
032200080320     C                             + ' ' + 'VABCAD'
032300080320     C                   ENDIF
032400080320     C* Reperisco la provincia dal CAP e dalla località
032500080320     C                   IF        VABCAD <> *blanks AND
032600080320     C                             VABPRD  = *blanks
032700080320     C                   CLEAR                   TISI95DS
032800080320     C                   EVAL      I95TCN = '3'
032900080320     C                   Z-ADD     datcor        I95DAT
033000080320     C                   EVAL      I95CAP = VABCAD
033100080320     C                   EVAL      I95LOC = VABLOD
033200080320     C                   EVAL      I95NAR = VABNZD
033300080320     C                   CALL      'TISI95R'
033400080320     C                   PARM                    TISI95DS
033500080320     C                   EVAL      VABPRD = O95PRV
033600080320     C                   ENDIF
033700080320     C                   ENDIF
033800010607     C*
033900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034200020117     C* ==
034300080320     C                   EVAL      VABRMA=%trim(%subst(vindta:443:8))
034400080325     C                   EVAL      VABNOT=%trim(%subst(vindta:458:35))
034500110712     C*
034600130517     C                   IF        %subst(vindta:550:1) = 'A'
034700110712     C                   EVAL      VABTC1='A'
034800110712     C                   ENDIF
034900080321     C*
035000130517     C                   IF        %subst(vindta:548:2) = *blanks
035100080321     C                   EVAL      VABTIC = *blanks
035200080321     C                   ENDIF
035300130517     C                   IF        %subst(vindta:548:2) = 'BM'
035400080321     C                   EVAL      VABTIC = 'BM'
035500080321     C                   ENDIF
035600130517     C                   IF        %subst(vindta:548:2) = 'TM'
035700080321     C                   EVAL      VABTIC = 'TM'
035800080321     C                   ENDIF
035900080320     C*
036000080320     C                   IF        %subst(vindta:657:1) = 'F'
036100080320     C                   EVAL      VABCBO = '1'
036200080320     C                   ENDIF
036300080320     C                   IF        %subst(vindta:657:1) = 'A'
036400080320     C                   EVAL      VABCBO = '2'
036500080320     C                   ENDIF
036600080320     C*
036700080320     C                   IF        %subst(vindta:682:1) = 'S'
036800080320     C                   EVAL      FlgCAS = '1'
036900080320     C                   EVAL      VABVCA = 'EUR'
037000080320     C                   ENDIF
037100060510     C*
037200060510     C* Reperimento campi NUMERICI
037300060510     C                   MOVEL     DATCOR        VABAAS
037400060510     C                   MOVE      DATCOR        VABMGS
037500060710     C* NSP/RMN
037600110712     C                   EVAL      PiStr=%trim(%subst(vindta:443:7))
037700060510     C                   EXSR      CHKNUM
037800060510     C                   IF        PiInt=*on
037900060510     C                   Z-ADD     PiVal         VABNSP
038000060510     C                   Z-ADD     PiVal         VABRMN
038100060404     C                   ELSE
038200070502     C                   SETON                                        31
038300060510     C                   Z-ADD     *zeros        VABNSP
038400060510     C                   Z-ADD     1             VABRMN
038500060404     C                   EVAL      vinmsg = %trimr(vinmsg)
038600060510     C                             + ' ' + 'VABNSP VABRMN'
038700060404     C                   ENDIF
038800080320     C* DCR
038900080320     C                   EVAL      PiStr='20'+%trim(%subst(vindta:598:2))+
039000080320     C                                        %trim(%subst(vindta:596:2))+
039100080320     C                                        %trim(%subst(vindta:594:2))
039200080320     C                   EXSR      CHKNUM
039300080320     C                   IF        PiInt=*on
039400080320     C                   Z-ADD     PiVal         VABDCR
039500080320     C                   ELSE
039600080320     C                   SETON                                        32
039700080320     C                   Z-ADD     *zeros        VABDCR
039800080320     C                   EVAL      vinmsg = %trimr(vinmsg)
039900080320     C                             + ' ' + 'VABDCR'
040000080320     C                   ENDIF
040100040420     C* NCL
040200080320     C                   EVAL      PiStr=%trim(%subst(vindta:638:5))
040300010607     C                   EXSR      CHKNUM
040400010607     C                   IF        PiInt=*on
040500070606     C                   Z-ADD     PiVal         VABNCL
040600010607     C                   ELSE
040700010607     C                   SETON                                        32
040800010607     C                   Z-ADD     *zeros        VABNCL
040900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041000010607     C                             + ' ' + 'VABNCL'
041100010607     C                   ENDIF
041200071029     C* PKB
041300080320     C                   EVAL      PiStr=%trim(%subst(vindta:642:10))
041400010607     C                   EXSR      CHKNUM
041500010607     C                   IF        PiNum=*on
041600080320     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
041700071003     C                   Z-ADD(H)  PiVal         VABPKB
041800010607     C                   ELSE
041900010607     C                   SETON                                        32
042000010607     C                   Z-ADD     *zeros        VABPKB
042100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042200010607     C                             + ' ' + 'VABPKB'
042300010607     C                   ENDIF
042400080320     C* CAS
042500110712     C                   IF        %subst(vindta:503:35) <> *blanks
042600080320     C                   EVAL      PiStr=%trim(%subst(vindta:503:35))
042700080320     C                   EXSR      CHKNUM
042800080320     C                   IF        PiNum=*on
042900080320     C                   Z-ADD(H)  PiVal         VABCAS
043000080320     C                   ELSE
043100080320     C                   SETON                                        32
043200080320     C                   Z-ADD     *zeros        VABCAS
043300080320     C                   EVAL      vinmsg = %trimr(vinmsg)
043400080320     C                             + ' ' + 'VABCAS'
043500080320     C                   ENDIF
043600110712     C                   ENDIF
043700020322     C*
043800020322     C* Considerazioni finali su CBO/CAS
043900050908     C                   IF        FlgCAS = '1'
044000040713     C                   IF        VABCBO = '1'
044100020322     C                   EVAL      VABCBO = '4'
044200040713     C                   ENDIF
044300040713     C                   IF        VABCBO = '2'
044400040713     C                   EVAL      VABCBO = '6'
044500040713     C                   ENDIF
044600020322     C                   ENDIF
044700020204     C*
044800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044900020204     C                   EXSR      CHKIMPDIV
045000020204     C*
045100010607     C                   ENDSR
045200010607     C*----------------------------------------------------*
045300010601
045400020204
045500020204     C*----------------------------------------------------*
045600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045700020204     C*----------------------------------------------------*
045800020204     C     CHKIMPDIV     BEGSR
045900020204     C*
046000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046100020204     C                   Z-ADD     *zeros        wrkDec            9 9
046200020204     C*
046300020204     C* Come prima cosa effettuo considerazioni sulla divisa
046400020204     C                   IF        vabIAS > *zeros
046500020204     C                   IF        vabVAS <> 'EUR'
046600020204     C                   EVAL      vabVAS =  'ITL'
046700020204     C                   ENDIF
046800020204     C                   ENDIF
046900020204     C*
047000020204     C                   IF        vabCAS > *zeros
047100020204     C                   IF        vabVCA <> 'EUR'
047200020204     C                   EVAL      vabVCA =  'ITL'
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C                   IF        vabVMD > *zeros
047700020321     C                   IF        vabVAD <> 'EUR'
047800020204     C                   EVAL      vabVAD =  'ITL'
047900020204     C                   ENDIF
048000020204     C                   ENDIF
048100020204     C*
048200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048300020204     C                   Z-ADD     vabIAS        wrkDec
048400020204     C                   IF        wrkDec > *zeros
048500020204     C                   IF        vabVAS = 'ITL'
048600020204     C                   EVAL      vabIAS = *zeros
048700020204     C                   ENDIF
048800020204     C                   ENDIF
048900020204     C*
049000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049100020204     C                   Z-ADD     vabCAS        wrkDec
049200020204     C                   IF        wrkDec > *zeros
049300020204     C                   IF        vabVCA = 'ITL'
049400020204     C                   EVAL      vabCAS = *zeros
049500020204     C                   ENDIF
049600020204     C                   ENDIF
049700020204     C*
049800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049900020204     C                   Z-ADD     vabVMD        wrkDec
050000020204     C                   IF        wrkDec > *zeros
050100020204     C                   IF        vabVAD = 'ITL'
050200020204     C                   EVAL      vabVMD = *zeros
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C                   ENDSR
050700020204     C***
050800020204
050900010330
051000010330
051100010330     C*----------------------------------------------------*
051200010330     C*  CONTROLLO NUMERICITA' CAMPI
051300010330     C*----------------------------------------------------*
051400010330     C     CHKNUM        BEGSR
051500010330     C*
051600010606     C                   IF        PiDecChr = *blanks
051700080320     C                   EVAL      PiDecChr = ','
051800010606     C                   ENDIF
051900010606     C*
052000010606     C                   CALL(e)   'ISNUMERIC'
052100010330     C                   PARM                    PiStr            30
052200010606     C                   PARM                    PiDecChr          1
052300010330     C                   PARM      *ZEROS        PiVal            30 9
052400010330     C                   PARM      '0'           PiInt             1
052500010330     C                   PARM      '0'           PiNum             1
052600010330     C                   IF        %error
052700010606     C                   EVAL      PiNum=*off
052800010330     C                   ENDIF
052900010330     C*
053000010330     C                   ENDSR
053100010330     C***
053200010330
053300010601
053400010601
053500010601
053600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053700010601     C     repfil        BEGSR
053800010601     C*
053900010601     C                   if        invfil = *zeros and
054000010601     C                             depfil > *zeros and
054100010629     C                             (vinflg = *blanks or
054200010629     C                              vinflg = *zeros)
054300010601     C
054400010601     C                   eval      invfil = depfil
054500010601     C                   endif
054600010601     C*
054700010601     C                   if        depfil <> invfil and
054800010601     C                             invfil > *zeros
054900010601     C                   eval      flgMulti = '1'
055000010601     C                   if        vinflg = *blanks
055100010601     C                   add       1             cntNonEl
055200010601     C                   endif
055300010601     C                   endif
055400010601     C*
055500010601     C                   if        vinflg = '2'
055600010601     C                   eval      flgStato = '2'
055700010601     C                   endif
055800010601     C*
055900010601     C                   ENDSR
056000010601     C***
056100010601
056200010601
056300010601
056400010330
056500010330
056600010330
056700990920      /TITLE Invio dei dati al punto operativo.
056800000613     C     invio         BEGSR
056900990920     C*
057000990920     C                   reset                   dscmz
057100010601     C                   move      invfil        cmzdst
057200990920     C                   eval      cmzfld = vlrfou
057300990920     C                   eval      cmzmbd = vlrhdl
057400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057500000710     C                   if        prmfir = *blanks
057600050408     C                   eval      cmzfla = 'FIVAB00F'
057700050408     C                   eval      cmzmba = 'FIVAB00F'
057800000710     C                   else
057900000710     C                   eval      cmzfla = prmfir
058000000710     C                   eval      cmzmba = prmfir
058100000710     C                   endif
058200990920     C                   eval      cmznrr = *zeros
058300990920     C                   move      §ctrok        cmznrr
058400021018     C                   eval      cmzlba = vlrfl1
058500990920     C                   call(e)   'TIS711C'
058600990920     C                   parm                    dscmz
058700990921     C                   parm      *blanks       esito
058800990920     C                   if        %error
058900990920     C                             or cmzerr = '1'
059000990921     C                             or esito  = '1'
059100000710     C                   eval      wrkesito = '3'
059200990920     C                   endif
059300990920     C*
059400000613     C                   ENDSR
059500990910
059600010601
059700010601
059800010601
059900010601
060000010601      /TITLE Invio dei dati al punto operativo.
060100010601     C     opeini        BEGSR
060200010601     C*
060300010601     C* Inizializzo flag e contatori operativi
060400010601     C                   movel     '0'           flgGiro           1
060500010601     C                   movel     '0'           flgMulti          1
060600010601     C                   movel     '1'           flgStato          1
060700010615     C                   movel     '0'           flgOk             1
060800010601     C                   z-add     *zeros        cntNonEl         10 0
060900010601     C                   z-add     *zeros        depfil            3 0
061000010601     C                   z-add     *zeros        invfil            3 0
061100010601     C*
061200010601     C                   ENDSR
061300010601     C***
061400010601
061500010601
061600010601
061700010330
061800010330
061900000613     C     *inzsr        BEGSR
062000990910     C*
062100990910     C     *entry        plist
062200990920     C                   parm                    tivlrds
062300990921     C                   parm      wrkesito      esito
062400000724     C                   parm                    prmlit
062500000710     C                   parm                    prmfir
062600010330     C*
062700010330     C* CALCOLA LA DATA CORRENTE
062800010330     C                   time                    wn14             14 0
062900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063000010330     C                   z-add     wn8           g08dat
063100010330     C                   z-add     *zeros        g08inv
063200010330     C                   movel     '0'           g08err
063300010330     C                   call      'XSRDA8'
063400010330     C                   parm                    wlbda8
063500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063600000613     C*
063700000613     C                   ENDSR
063800000613     C***
