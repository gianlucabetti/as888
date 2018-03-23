000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021017     FFIVABwwr  O    E             DISK    usropn
000600021017     FFIVATwwr  O    E             DISK    usropn
000700090730     FPRTEMAIL  O    F  198        PRINTER OFLIND(*INOF) USROPN
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100090730     D depspe          s             10    INZ(*blanks)
002200010213     D depdat          s              8  0 INZ(*zeros)
002300090730     D curspe          s             10    INZ(*blanks)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800010213
002900000801     D  Num5_0         s              5  0
003000030107     D  Num7_0         s              7  0
003100000830
003200000830     D*------------------
003300000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400000830     D*------------------
003500000830     D WLBDA8          DS                  INZ
003600000830     D  G08DAT                 1      8  0
003700000830     D  G08INV                 9     16  0
003800000830     D  G08ERR                17     17
003900000830     D  G08TGI                18     22  0
004000000830     D*
004100090730     D*------------------
004200090730     D* DS ridefinizione dati utente estesi spool x mailing
004300090730     D*------------------
004400090730     D TRTCM1DS      E DS
004500990908
004600010201
004700010201
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100090730     C*
005200090730     C                   EXSR      PRESTACOM
005300000613     C*
005400030107     C                   EXSR      RWFISCHER                                    LETT/SCR. VAB
005500000613     C*
005600010202     C* Effettuo la chiamata al CLLE preposto
005700050128     C                   call(e)   'TITVVTC'
005800010202     C                   parm                    parccm
005900010202     C                   parm                    parmbr
006000010202     C                   parm      '2'           paropz
006100090730     C*
006200090730     C                   EXSR      ENDSTACOM
006300000616     C*
006400000801     C
006500010201     C                   seton                                        LR
006600990908
006700000801
006800910830     C*--------------------------------------------------------
006900030107     C* RWFISCHER LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
007000910830     C*--------------------------------------------------------
007100030107     C     RWFISCHER     BEGSR
007200990910     C*
007300990914     C                   if        not %open(tivin00r)
007400990908     C                   open      tivin00r
007500990914     C                   endif
007600021017     C                   if        not %open(fivabwwr)
007700021017     C                   open      fivabwwr
007800990914     C                   endif
007900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008000020305     C                   exsr      prevat
008100010201     C*
008200010202     C                   if        chkcall = '0'
008300010202     C*
008400021017     C                   if        not %open(fivatwwr)
008500021017     C                   open      fivatwwr
008600010201     C                   endif
008700990910     C*
008800010201     C                   clear                   §CTROKVB          5 0
008900020305     C                   clear                   §CTROKVT          5 0
009000000801     C                   clear                   §CTRMO            5 0
009100000801     C                   clear                   §CTRNO            5 0
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600000905     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600090730     C*
010700090730     C* Verifico il tipo record corrente (PAR=dati  COM=commenti)
010800090730     C                   if        %subst(vindta:1:3) = 'PAR'
010900090730     C                   seton                                        54
011000090730     C                   else
011100090730     C                   setoff                                       54
011200090730     C                   endif
011300010305     C*
011400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
011500090730     C                   EVAL      PiStr=%trim(%subst(vindta:14:10))
011600020305     C                   MOVEL(p)  PiStr         curspe
011700010305     C*
011800010305     C                   if        depspe = *blanks                             => 1° giro
011900010305     C                   eval      depspe = curspe                              => memorizz. spediz
012000090730     C   54              exsr      impvab
012100090730     C   54              exsr      wrivat_e                                     => carico VAT
012200090825     C   54              exsr      wrivat_b                                     => carico VAT
012300160120     C   54              exsr      wrivat_ij                                    => carico VAT
012400010305     C                   else
012500010305     C                   if        curspe <> depspe                             => rottura di spediz
012600010305     C                   eval      depspe = curspe                              => memorizz. spediz
012700090730     C                   exsr      staRIGA                                      * stampa commenti
012800090730     C   54              exsr      wrivab
012900090730     C   54              exsr      impvab
013000090730     C   54              exsr      wrivat_e                                     => carico VAT
013100090825     C   54              exsr      wrivat_b                                     => carico VAT
013200160120     C   54              exsr      wrivat_ij                                    => carico VAT
013300020305     C                   else                                                   => x stessa spediz
013400090730     C   54              exsr      impvab
013500090730     C   54              exsr      wrivat_e                                     => carico VAT
013600090825     C   54              exsr      wrivat_b                                     => carico VAT
013700160120     C   54              exsr      wrivat_ij                                    => carico VAT
013800010305     C                   endif
013900010305     C                   endif
014000010305     C                   endif
014100090730     C*
014200090811     C                   move(p)   VABRMN        wVABRMNalfa      15
014300090730     C  N54              exsr      staCOMM                                      * stampa commenti
014400000905     C*
014500000905     C                   else
014600000905     C                   eval      vinflg = '1'
014700000905     C                   endif
014800000905     C*
014900000905     C  N70              update    tivin000
015000000905     C*
015100991022     C  N70              ENDdo
015200010202     C*
015300020305     C* Scarico i VAB rimasti "in sospeso"
015400090811     C                   exsr      wrivab
015500010202     C*
015600010202     C                   endif
015700990910
015800990910     C* Se non ci sono record con errori ...
015900000710     C                   if        §ctrno = 0
016000990910     C* ... restituisco esito OK.
016100990921     C                   eval      wrkesito = '0'
016200990910     C                   else
016300010201     C                   if        §ctrokvb > 0
016400990921     C                   eval      wrkesito = '1'
016500000710     C                   else
016600000710     C                   eval      wrkesito = '2'
016700990910     C                   endif
016800000710     C                   endif
016900990910     C*
017000990914     C                   if        %open(tivin00r)
017100990908     C                   close     tivin00r
017200990914     C                   endif
017300021017     C                   if        %open(fivabwwr)
017400021017     C                   close     fivabwwr
017500990914     C                   endif
017600021017     C                   if        %open(fivatwwr)
017700021017     C                   close     fivatwwr
017800010201     C                   endif
017900021017     C*
018000021017     C                   if        vlrpoi <> 999
018100021017     C                   eval      vlrpoi = vlrpoi
018200021017     C                   endif
018300990910     C*
018400010201     C                   if        §ctrokvb > 0
018500021017     C                             and vlrpoi > *zeros
018600010202     C                   exsr      invio
018700990920     C                   endif
018800990920     C*
018900910830     C                   ENDSR
019000000613     C***
019100090730
019200090730     C*----------------------------------------------------*
019300090730     C*  PREPARAZIONE STAMPA COMMENTI CLIENTE
019400090730     C*----------------------------------------------------*
019500090730     C     PRESTACOM     BEGSR
019600090730     C*
019700090730     C* Imposto quindi i dati utenti estesi
019800090730     C                   EVAL      §CM1MITT = 'ced'
019900090730     C                   EVAL      §CM1VAR  = '*OBJM*' + 'Ricezione commenti ' +
020000090730     C                                        'bolle FISCHER (0061640)'
020100160302     C                   EVAL      §CM1TIPS = 'MG9'
020200130918     C                   EVAL      §CM1IDP  = '*'
020300090730     C                   MOVEL(P)  '046'         §CM1PO
020400090730     C                   EVAL      §CM1DST  = %trim(%subst(vlrppt:26:25)) +
020500130613     C                                        '@brt.it' + ';'
020600090730     C*
020700090730     C* Eseguo override x generare uno spool ad "hoc"
020800090730     C                   CALL      'TRUL83C1'
020900090730     C                   PARM      'P'           OPZIONE           1
021000090730     C                   PARM      'PRTEMAIL'    PRTFILE          10
021100090730     C                   PARM                    TRTCM1DS
021200090730     C                   PARM      'EMAILIN_K'   OUTQ             10
021300090730     C                   PARM                    Status            1
021400090730     C*
021500090730     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
021600090730     C                   IF        Status = '2'
021700090730     C                   SETON                                        55
021800090730     C                   ELSE
021900090730     C                   OPEN      PRTEMAIL
022000090730     C                   SETOFF                                       55
022100090730     C                   ENDIF
022200090730     C*
022300090730     C                   ENDSR
022400090730
022500090730     C*----------------------------------------------------*
022600090730     C*  STAMPA COMMENTI CLIENTE
022700090730     C*----------------------------------------------------*
022800090730     C     STACOMM       BEGSR
022900090730     C*
023000090730     C                   if        not *in55
023100090730     C                   movel     *blanks       wCOMM            70
023200090730     C                   eval      wCOMM = %trim(%subst(vindta:57:70))
023300090730     C                   except    COMM
023400090730     C                   endif
023500090730     C*
023600090730     C                   ENDSR
023700090730
023800090730
023900090730     C*----------------------------------------------------*
024000090730     C*  STAMPA COMMENTI CLIENTE - RIGA VUOTA
024100090730     C*----------------------------------------------------*
024200090730     C     STARIGA       BEGSR
024300090730     C*
024400090730     C                   except    RIGA
024500090810     C                   except    RIGA
024600090730     C*
024700090730     C                   ENDSR
024800090730
024900090730
025000090730     C*----------------------------------------------------*
025100090730     C*  CHIUSURA STAMPA COMMENTI CLIENTE
025200090730     C*----------------------------------------------------*
025300090730     C     ENDSTACOM     BEGSR
025400090730     C*
025500090730     C                   CLOSE     PRTEMAIL
025600090730     C*
025700090730     C* Eseguo delete override precedente
025800090730     C                   CALL      'TRUL83C1'
025900090730     C                   PARM      'D'           OPZIONE
026000090730     C                   PARM                    PRTFILE
026100090730     C                   PARM                    TRTCM1DS
026200090730     C                   PARM                    OUTQ
026300090730     C                   PARM                    Status
026400090730     C*
026500090730     C                   ENDSR
026600090730
026700010305
026800010305     C*----------------------------------------------------*
026900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027000010305     C*----------------------------------------------------*
027100020305     C     WRIVAB        BEGSR
027200010305     C*
027300021017     C                   write     fivab000                                     => scarico il VAB
027400010305     C*
027500010305     C                   ENDSR
027600990920
027700000801     C*----------------------------------------------------*
027800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027900000801     C*----------------------------------------------------*
028000010201     C     INZVAR        BEGSR
028100000801     C*
028200010201     C                   Z-ADD     *zeros        Num5_0
028300030107     C                   Z-ADD     *zeros        Num7_0
028400100519     C*
028500100519     C                   MOVEL     *blanks       FlgCAS            1
028600000801     C*
028700000801     C                   ENDSR
028800000801     C*----------------------------------------------------*
028900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029000000801     C*----------------------------------------------------*
029100000801     C     DEFCAM        BEGSR
029200000801     C*
029300021017     C                   CLEAR                   FIVAB000
029400020619     C* Imposto i valori di default...
029500030107     C                   Z-ADD     0061640       VABCCM
029600030107     C                   Z-ADD     0061640       VATCCM
029700021017     C                   Z-ADD     vlrpoi        VABFGS
029800021017     C                   Z-ADD     vlrpoi        VATFGS
029900030107     C                   Z-ADD     006           VABLNP
030000030107     C                   Z-ADD     006           VATLNP
030100030107     C                   Z-ADD     005           VABCTR
030200050125     C                   MOVEL     '7T'          VABCTM
030300020619     C* ... e poi verifico se sono stati passati come parametri
030400020619     C                   IF        vlrppt > *blanks
030500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030600020619     C                   EXSR      CHKNUM
030700020619     C                   IF        PiInt=*on
030800020619     C                   Z-ADD     PiVal         VABCCM
030900020619     C                   Z-ADD     PiVal         VATCCM
031000020619     C                   ENDIF
031100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031200020619     C                   EXSR      CHKNUM
031300020619     C                   IF        PiInt=*on
031400020619     C                   Z-ADD     PiVal         VABLNP
031500020619     C                   Z-ADD     PiVal         VATLNP
031600020619     C                   ENDIF
031700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031800020619     C                   EXSR      CHKNUM
031900020619     C                   IF        PiInt=*on
032000020619     C                   Z-ADD     PiVal         VABCTR
032100020619     C                   ENDIF
032200020619     C                   ENDIF
032300000801     C*
032400000801     C                   ENDSR
032500000801     C*----------------------------------------------------*
032600021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032700000801     C*----------------------------------------------------*
032800010201     C     IMPVAB        BEGSR
032900000801     C*
033000020305     C                   EXSR      INZVAR
033100020305     C                   EXSR      DEFCAM
033200010305     C*
033300000801     C                   Z-ADD     *zeros        errore            1 0
033400000830     C                   MOVEL     datcor        VABAAS
033500020305     C                   MOVEL     datcor        VATAAS
033600000830     C                   MOVE      datcor        VABMGS
033700090730     C                   EVAL      VABRSD=%trim(%subst(vindta:52:50))
033800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034000020117     C* ==
034100090730     C                   EVAL      VABRD2=%trim(%subst(vindta:102:50))
034200090730     C                   EVAL      VABIND=%trim(%subst(vindta:152:50))
034300090730     C                   EVAL      VABLOD=%trim(%subst(vindta:202:50))
034400090730     C                   EVAL      VABPRD=%trim(%subst(vindta:257:3))
034500090730     C                   EVAL      VABRMA=%trim(%subst(vindta:24:10))
034600100519     C                   EVAL      VABVCA=%trim(%subst(vindta:433:3))
034700121115     C                   EVAL      VABNAS=%trim(%subst(vindta:42:10))
034800091112     C*
034900091112     C                   IF        %subst(vindta:8:6) = '699108'
035000091112     C                   EVAL      VABTSP = 'E'
035100091112     C                   ENDIF
035200020305     C*
035300090730     C                   EVAL      PiStr=%trim(%subst(vindta:252:5))
035400010201     C                   EXSR      CHKNUM
035500010201     C                   IF        PiInt=*on
035600010201     C                   Z-ADD     PiVal         Num5_0
035700021017     C                   MOVEL     Num5_0        VABCAD
035800010201     C                   ELSE
035900010201     C                   ADD       1             errore
036000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036100021017     C                             + ' ' + 'VABCAD'
036200010201     C                   ENDIF
036300020619     C*
036400090730     C                   EVAL      PiStr=%trim(%subst(vindta:14:10))
036500010201     C                   EXSR      CHKNUM
036600010201     C                   IF        PiInt=*on
036700021017     C                   Z-ADD     PiVal         VABRMN
036800030107     C                   Z-ADD     PiVal         Num7_0
036900030107     C                   MOVE(P)   Num7_0        VABNSP
037000030107     C                   MOVE(P)   Num7_0        VATNSP
037100010201     C                   ELSE
037200010201     C                   ADD       1             errore
037300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037400021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
037500010201     C                   ENDIF
037600030107     C*
037700090730     C                   EVAL      PiStr=%trim(%subst(vindta:290:5))
037800030107     C                   EXSR      CHKNUM
037900030107     C                   IF        PiInt=*on
038000030107     C                   Z-ADD     PiVal         VABNCL
038100030107     C                   ELSE
038200030107     C                   ADD       1             errore
038300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038400030107     C                             + ' ' + 'VABNCL'
038500030107     C                   ENDIF
038600030107     C*
038700090730     C                   EVAL      PiStr=%trim(%subst(vindta:295:15))
038800030107     C                   EXSR      CHKNUM
038900030107     C                   IF        PiNum=*on
039000090731     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
039100090730     C                   IF        %subst(vindta:310:3) = 'GRM'
039200090730     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a Kg.
039300090730     C                   ENDIF
039400030107     C                   Z-ADD     PiVal         VABPKB
039500030107     C                   ELSE
039600030107     C                   ADD       1             errore
039700030107     C                   EVAL      vinmsg = %trimr(vinmsg)
039800030107     C                             + ' ' + 'VABPKB'
039900030107     C                   ENDIF
040000090730     C*
040100090731     C*                  EVAL      PiStr=%trim(%subst(vindta:313:15))
040200090731     C*                  EXSR      CHKNUM
040300090731     C*                  IF        PiNum=*on
040400090731     C*                  IF        %subst(vindta:328:3) = 'CMQ'
040500090731     C*                  EVAL      PiVal = PiVal / 1000000                      * da CM3 a M3
040600090731     C*                  ENDIF
040700090731     C*                  IF        %subst(vindta:328:3) = 'DMQ'
040800090731     C*                  EVAL      PiVal = PiVal / 1000                         * da DM3 a M3
040900090731     C*                  ENDIF
041000090731     C*                  Z-ADD     PiVal         VABVLB
041100090731     C*                  ELSE
041200090731     C*                  ADD       1             errore
041300090731     C*                  EVAL      vinmsg = %trimr(vinmsg)
041400090731     C*                            + ' ' + 'VABVLB'
041500090731     C*                  ENDIF
041600100519     C*
041700100519     C                   IF        %subst(vindta:422:11) <> '00000000000' AND
041800100519     C                             %subst(vindta:422:11) <> *blanks
041900100519     C                   EVAL      FlgCAS = '1'
042000100519     C                   EVAL      PiStr=%trim(%subst(vindta:422:11))
042100100519     C                   EXSR      CHKNUM
042200100519     C                   IF        PiNum=*on
042300100519     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
042400100519     C                   Z-ADD     PiVal         VABCAS
042500100519     C                   ELSE
042600100519     C                   ADD       1             errore
042700100519     C                   EVAL      vinmsg = %trimr(vinmsg)
042800100519     C                             + ' ' + 'VABCAS'
042900100519     C                   ENDIF
043000100519     C                   ENDIF
043100010205     C*
043200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043300100519     C                   IF        FlgCAS = '1'
043400010205     C                   EVAL      VABCBO = '4'
043500010205     C                   ELSE
043600010205     C                   EVAL      VABCBO = '1'
043700010205     C                   ENDIF
043800020305     C*
043900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044000011113     C                   EXSR      CHKIMPDIV
044100010202     C*
044200000801     C* Ebbene...
044300000801     C                   ADD       1             §CTRMO
044400010201     C                   IF        errore <> *zeros
044500000801     C                   ADD       1             §CTRNO
044600000801     C                   EVAL      vinflg = '2'
044700000801     C                   ELSE
044800010201     C                   ADD       1             §CTROKVB
044900000801     C                   ENDIF
045000000801     C*
045100000801     C                   ENDSR
045200010201     C*----------------------------------------------------*
045300090730     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
045400010201     C*----------------------------------------------------*
045500090730     C     WRIVAT_E      BEGSR
045600010201     C*
045700021017     C* Valorizzo l buffer di scrittura del FIVAT
045800090730     C                   MOVEL     'E'           VATTRC
045900090730     C                   EVAL      VATNOT = %triml(%subst(vindta:331:20))
046000021017     C                   write     FIVAT000
046100010201     C*
046200010201     C                   ENDSR
046300090730     C*----------------------------------------------------*
046400090730     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
046500090730     C*----------------------------------------------------*
046600090730     C     WRIVAT_B      BEGSR
046700090730     C*
046800090730     C* Valorizzo l buffer di scrittura del FIVAT
046900090730     C                   MOVEL     'B'           VATTRC
047000090730     C                   EVAL      VATNOT = %triml(%subst(vindta:260:30))
047100090730     C                   write     FIVAT000
047200090730     C*
047300090730     C                   ENDSR
047400160120     C*----------------------------------------------------*
047500160120     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" / "J"
047600160120     C*----------------------------------------------------*
047700160120     C     WRIVAT_IJ     BEGSR
047800160120     C*
047900160120     C                   MOVEL     *blanks       wEMAIL           70
048000160120     C                   EVAL      wEMAIL = %trim(%subst(vindta:436:50))
048100160120     C*
048200160120     C* Valorizzo l buffer di scrittura del FIVAT
048300160120     C                   MOVEL     'I'           VATTRC
048400160120     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
048500160120     C                   write     FIVAT000
048600160120     C*
048700160120     C                   MOVEL     'J'           VATTRC
048800160120     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
048900160120     C                   write     FIVAT000
049000160120     C*
049100160120     C                   ENDSR
049200010202     C*----------------------------------------------------*
049300021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049400010202     C*----------------------------------------------------*
049500020305     C     PREVAT        BEGSR
049600010202     C*
049700021017     C* Compongo il nome del membro da dare al FIVATWWR
049800010202     C                   eval      parmbr = vlrhdl
049900010202     C                   movel     'M'           parmbr
050000090730     C                   eval      parccm = vlrksc
050100010202     C                   eval      paropz = '1'
050200010202     C* Effettuo la chiamata al CLLE preposto
050300050128     C                   call(e)   'TITVVTC'
050400010202     C                   parm                    parccm
050500010202     C                   parm                    parmbr
050600010202     C                   parm                    paropz
050700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050800010202     C                   if        %error
050900010202     C                   movel     '1'           chkcall
051000010202     C                   else
051100010202     C                   movel     '0'           chkcall
051200010202     C                   endif
051300010202     C*
051400010202     C                   ENDSR
051500000801     C*----------------------------------------------------*
051600000801     C*  CONTROLLO NUMERICITA' CAMPI
051700000801     C*----------------------------------------------------*
051800000801     C     CHKNUM        BEGSR
051900000801     C*
052000000801     C                   call(e)   'ISNUMERIC'
052100000801     C                   PARM                    PiStr            30
052200030107     C                   PARM      ','           PiDecChr          1
052300000801     C                   PARM      *ZEROS        PiVal            30 9
052400000801     C                   PARM      '0'           PiInt             1
052500000801     C                   PARM      '0'           PiNum             1
052600000801     C                   IF        %error
052700000801     C                   EVAL      PiInt=*off
052800030107     C                   EVAL      PiNum=*off
052900000801     C                   ENDIF
053000000801     C*
053100000801     C                   ENDSR
053200000801     C***
053300000801
053400011113
053500011113     C*----------------------------------------------------*
053600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053700011113     C*----------------------------------------------------*
053800011113     C     CHKIMPDIV     BEGSR
053900011113     C*
054000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054100011113     C                   Z-ADD     *zeros        wrkDec            9 9
054200011113     C*
054300011113     C* Come prima cosa effettuo considerazioni sulla divisa
054400011113     C                   IF        vabIAS > *zeros
054500011113     C                   IF        vabVAS <> 'EUR'
054600011113     C                   EVAL      vabVAS =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C                   IF        vabCAS > *zeros
055100011113     C                   IF        vabVCA <> 'EUR'
055200011113     C                   EVAL      vabVCA =  'ITL'
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   IF        vabVMD > *zeros
055700020305     C                   IF        vabVAD <> 'EUR'
055800011113     C                   EVAL      vabVAD =  'ITL'
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056300011113     C                   Z-ADD     vabIAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVAS = 'ITL'
056600011113     C                   EVAL      vabIAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057100011113     C                   Z-ADD     vabCAS        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVCA = 'ITL'
057400011113     C                   EVAL      vabCAS = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057900011113     C                   Z-ADD     vabVMD        wrkDec
058000011113     C                   IF        wrkDec > *zeros
058100011113     C                   IF        vabVAD = 'ITL'
058200011113     C                   EVAL      vabVMD = *zeros
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C                   ENDSR
058700011113     C***
058800021017
058900000801
059000000801
059100990920      /TITLE Invio dei dati al punto operativo.
059200010202     C     invio         BEGSR
059300990920     C*
059400021017     C* 1° invio FIVAT
059500010201     C                   reset                   dscmz
059600021017     C                   move      vlrpoi        cmzdst
059700021017     C                   eval      cmzfld = 'FIVATWWR'
059800010201     C                   eval      cmzmbd = vlrhdl
059900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060000021009     C***                if        prmfir = *blanks
060100021017     C                   eval      cmzfla = 'FIVAT00F'
060200021017     C                   eval      cmzmba = 'FIVAT00F'
060300021009     C***                else
060400021009     C***                eval      cmzfla = prmfir
060500021009     C***                eval      cmzmba = prmfir
060600021009     C***                endif
060700010201     C                   eval      cmznrr = *zeros
060800020305     C                   move      §ctrokvt      cmznrr
060900021018     C                   eval      cmzlba = vlrfl1
061000010201     C                   call(e)   'TIS711C'
061100010201     C                   parm                    dscmz
061200010201     C                   parm      *blanks       esito
061300010205     C                   if        %error
061400010205     C                             or cmzerr = '1'
061500010205     C                             or esito  = '1'
061600010205     C                   eval      wrkesito = '3'
061700010205     C                   else
061800010201     C*
061900021017     C* 2° invio FIVAB
062000010201     C                   reset                   dscmz
062100021017     C                   move      vlrpoi        cmzdst
062200010201     C                   eval      cmzfld = vlrfou
062300010201     C                   eval      cmzmbd = vlrhdl
062400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062500021009     C***                if        prmfir = *blanks
062600021017     C                   eval      cmzfla = 'FIVAB00F'
062700021017     C                   eval      cmzmba = 'FIVAB00F'
062800021009     C***                else
062900021009     C***                eval      cmzfla = prmfir
063000021009     C***                eval      cmzmba = prmfir
063100021009     C***                endif
063200010201     C                   eval      cmznrr = *zeros
063300010201     C                   move      §ctrokvb      cmznrr
063400021018     C                   eval      cmzlba = vlrfl1
063500010201     C                   call(e)   'TIS711C'
063600010201     C                   parm                    dscmz
063700010201     C                   parm      *blanks       esito
063800010201     C                   if        %error
063900010201     C                             or cmzerr = '1'
064000010201     C                             or esito  = '1'
064100010201     C                   eval      wrkesito = '3'
064200010201     C                   endif
064300010205     C                   endif
064400990920     C*
064500000613     C                   ENDSR
064600000613     C***
064700070411
064800070411     C     *pssr         BEGSR
064900070411     C*
065000070411     C                   if        %open(tivin00r)
065100070411     C                   close     tivin00r
065200070411     C                   endif
065300070411     C                   if        %open(fivabwwr)
065400070411     C                   close     fivabwwr
065500070411     C                   endif
065600070411     C                   if        %open(fivatwwr)
065700070411     C                   close     fivatwwr
065800070411     C                   endif
065900070411     C*
066000070411     C* Effettuo la chiamata al CLLE preposto
066100070411     C                   call(e)   'TITVVTC'
066200070411     C                   parm                    parccm
066300070411     C                   parm                    parmbr
066400070411     C                   parm      '2'           paropz
066500070411     C*
066600070411     C                   eval      wrkesito = '2'
066700070411     C*
066800070411     C                   seton                                        LR
066900070411     C*
067000070411     C                   ENDSR     '*CANCL'
067100070411     C***
067200070411
067300990910
067400000613     C     *inzsr        BEGSR
067500990910     C*
067600990910     C     *entry        plist
067700990920     C                   parm                    tivlrds
067800990921     C                   parm      wrkesito      esito
067900000724     C                   parm                    prmlit
068000000710     C                   parm                    prmfir
068100000613     C*
068200000830     C* CALCOLA LA DATA CORRENTE
068300000830     C                   time                    wn14             14 0
068400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068500000830     C                   z-add     wn8           g08dat
068600000830     C                   z-add     *zeros        g08inv
068700000830     C                   movel     '0'           g08err
068800000830     C                   call      'XSRDA8'
068900000830     C                   parm                    wlbda8
069000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069100000830     C*
069200000613     C                   ENDSR
069300000613     C***
069400990908
069500090730     O*------------------------------------------------------------------------*
069600090730     O* STAMPA
069700090730     O*------------------------------------------------------------------------*
069800090730     OPRTEMAIL  E            COMM        1
069900160302     O***                                        +1 'Rif. mittente:'
070000160302     O                                           +1 'Rif.:'
070100090811     O                       wVABRMNalfa         +1
070200160302     O***                                        +5 'Particolarità:'
070300160302     O                                           +4 ' '
070400090730     O                       wCOMM               +1
070500090730     O          E            RIGA        1
070600090730     O                                           +1 ' '
