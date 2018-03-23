000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500100412     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100100729     D depspe          s              8    INZ(*blanks)
002200100729     D curspe          s              8    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900020725
003000020725     D*------------------
003100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003200020725     D*------------------
003300070530     D INPUT_DS        DS                  INZ
003400100729     D  VINDTA                     2048
003500100729     D  VINFLG                        1
003600100729     D  VINSPE                        8
003700100729     D  VINRRN                        8  0
003800020725     D*
003900081113
004000091223     D*------------------
004100091223     D* DS REPERIMENTO NUMERATORE
004200091223     D*------------------
004300100309     D trul33ds      e ds                  inz
004400091223     D*------------------
004500091223     D* DS ARCHITETTURA
004600091223     D*------------------
004700091223     D kpjba         e ds                  inz
004800091223
004900081113
005000081113     D*------------------
005100081113     D* LINKING A DEFINIZIONI ESTERNE
005200081113     D*------------------
005300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005400090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005500081113
005600990908
005700010201
005800010201
005900080117     C*
006000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006100080117     C
006200080117     C/EXEC SQL
006300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400080117     C/END-EXEC
006500080117     C
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000613     C*
007000100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
007100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007200000613     C*
007300010202     C* Effettuo la chiamata al CLLE preposto
007400100525     C  N51              call(e)   'TITVVTC'
007500100525     C                   parm                    parccm
007600100525     C                   parm                    parmbr
007700100525     C                   parm      '2'           paropz
007800100525     C   51              call(e)   'TITVVBC'
007900100525     C                   parm                    parccm
008000100525     C                   parm                    parmbr
008100100525     C                   parm      '2'           paropz
008200070730     C*
008300070730     C* Effettuo lancio TISI95 solo x chiusura
008400070730     C                   CLEAR                   TISI95DS
008500070730     C                   EVAL      I95TLA = 'C'
008600070730     C                   CALL      'TISI95R'
008700070730     C                   PARM                    TISI95DS
008800000616     C*
008900000801     C
009000010201     C                   seton                                        LR
009100000801
009200910830     C*--------------------------------------------------------
009300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009400910830     C*--------------------------------------------------------
009500070530     C     RWFILE        BEGSR
009600990910     C*
009700990914     C                   if        not %open(tivin00r)
009800990908     C                   open      tivin00r
009900990914     C                   endif
010000070530     C*
010100100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010200100525     C                   exsr      prevasx
010300010201     C*
010400010202     C                   if        chkcall = '0'
010500010202     C*
010600100525     C                   if        not %open(fivabwwr)
010700100525     C                   open      fivabwwr
010800100525     C                   endif
010900100525     C*
011000021113     C                   if        not %open(fivatwwr)
011100021113     C                   open      fivatwwr
011200010201     C                   endif
011300080117     C*
011400080117     C                   EXSR      INZVAR
011500080117     C                   EXSR      DEFCAM
011600990910     C*
011700010201     C                   clear                   §CTROKVB          5 0
011800020305     C                   clear                   §CTROKVT          5 0
011900000801     C                   clear                   §CTRMO            5 0
012000000801     C                   clear                   §CTRNO            5 0
012100990910     C*
012200020725     C/EXEC SQL
012300020725     C+ declare C1 cursor for select
012400100730     C+ vindta, vinflg, substr(vindta, 48, 8) as sped, rrn(tivin00r)
012500060223     C+ from tivin00r
012600060223     C+ order by sped
012700020725     C+ for read only
012800020725     C/END-EXEC
012900020725     C
013000020725     C/EXEC SQL
013100020725     C+ open C1
013200020725     C/END-EXEC
013300020725     C
013400020725     C/EXEC SQL
013500070530     C+ Fetch C1 into :INPUT_DS
013600020725     C/END-EXEC
013700020725     C*
013800020725     C                   dow       sqlcod = *zeros
013900990913     C*
014000100310     C                   if        vindta > *blanks
014100000613     C                   add       1             rrnum
014200000801     C*
014300020725     C                   if        vinflg = *blanks
014400020725     C                             or vinflg = '0'
014500020725     C                             or vinflg = '2'
014600000801     C*
014700020725     C                   clear                   x_vinmsg
014800020725     C                   eval      x_vinflg = '1'
014900010305     C*
015000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015100100730     C                   EVAL      curspe=%trim(%subst(vindta:48:8))
015200010305     C*
015300100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015400080923     C                   if        *in50 = *off
015500071003     C                   exsr      impvab
015600100525     C                   exsr      wrivab
015700100127     C                   exsr      wrivat_a                                     => carico VAT
015800071003     C                   exsr      wrivat_b                                     => carico VAT
015900100525     C   51              exsr      wrivat_e                                     => carico VAT
016000071003     C                   else
016100080923     C*
016200071009     C                   if        wDISK = 'D'
016300091223     C                   exsr      repNSP
016400071009     C                   exsr      impvab
016500100412     C                   exsr      wrivab
016600100127     C                   exsr      wrivat_a                                     => carico VAT
016700071009     C                   exsr      wrivat_b                                     => carico VAT
016800071009     C                   exsr      wrivat_e                                     => carico VAT
016900071009     C                   else
017000071009     C*
017100010305     C                   if        depspe = *blanks                             => 1° giro
017200010305     C                   eval      depspe = curspe                              => memorizz. spediz
017300080117     C                   clear                   tivinds
017400091223     C                   exsr      repNSP
017500020305     C                   exsr      impvab
017600100127     C                   exsr      wrivat_a                                     => carico VAT
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800071003     C   50              exsr      wrivat_e                                     => carico VAT
017900010305     C                   else
018000020725     C                   if        depspe <> curspe                             => rottura di spediz
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200100412     C                   exsr      wrivab
018300080117     C                   clear                   tivinds
018400091223     C                   exsr      repNSP
018500020305     C                   exsr      impvab
018600100127     C                   exsr      wrivat_a                                     => carico VAT
018700071003     C                   exsr      wrivat_b                                     => carico VAT
018800071003     C   50              exsr      wrivat_e                                     => carico VAT
018900020305     C                   else                                                   => x stessa spediz
019000100401     C*
019100100401     C* Se nn richiesta esclusione spedizioni "duplicate"
019200100412     C                   if        wDUPREC <> 'N'
019300020305     C                   exsr      impvab
019400100127     C                   exsr      wrivat_a                                     => carico VAT
019500071003     C                   exsr      wrivat_b                                     => carico VAT
019600071003     C   50              exsr      wrivat_e                                     => carico VAT
019700100401     C                   endif
019800010305     C                   endif
019900010305     C                   endif
020000010305     C                   endif
020100071003     C                   endif
020200071009     C                   endif
020300000905     C*
020400000905     C                   else
020500020725     C                   eval      x_vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800020725     C     VINRRN        chain     tivin000
020900020725     C                   if        %found(tivin00r)
021000020725     C                   eval      y_vinflg = x_vinflg
021100020725     C                   eval      y_vinmsg = x_vinmsg
021200020725     C                   update    tivin000
021300020725     C                   endif
021400020725     C*
021500020725     C/EXEC SQL
021600070530     C+ Fetch C1 into :INPUT_DS
021700020725     C/END-EXEC
021800020725     C*
021900020725     C                   enddo
022000020725     C*
022100020725     C/EXEC SQL
022200020725     C+ close C1
022300020725     C/END-EXEC
022400000905     C*
022500020305     C* Scarico i VAB rimasti "in sospeso"
022600071009     C                   if        wDISK = 'C'
022700100412     C                   exsr      wrivab
022800071009     C                   endif
022900010202     C*
023000010202     C                   endif
023100990910
023200990910     C* Se non ci sono record con errori ...
023300000710     C                   if        §ctrno = 0
023400990910     C* ... restituisco esito OK.
023500990921     C                   eval      wrkesito = '0'
023600990910     C                   else
023700100525     C                   if        §ctrokvb > 0 OR
023800100525     C                             §ctrokvt > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021113     C                   if        %open(fivabwwr)
024900021113     C                   close     fivabwwr
025000990914     C                   endif
025100021113     C                   if        %open(fivatwwr)
025200021113     C                   close     fivatwwr
025300010201     C                   endif
025400990910     C*
025500100525     C                   if        §ctrokvb > 0 OR
025600100525     C                             §ctrokvt > 0
025700000724     C                             and vlrpoi <> *zeros
025800010202     C                   exsr      invio
025900990920     C                   endif
026000990920     C*
026100910830     C                   ENDSR
026200000613     C***
026300100412
026400100412
026500010305
026600010305     C*----------------------------------------------------*
026700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026800010305     C*----------------------------------------------------*
026900020305     C     WRIVAB        BEGSR
027000100412     C*
027100100412     C* Gestisco l'eventuale rottura x numero spedizione
027200100412     C                   if        VABRMA = *blanks
027300100412     C                   movel(P)  VABRMN        VABRMA
027400100412     C                   endif
027500100412     C*
027600100525     C                   if        *in51 = *off and *in31 = *off
027700100525     C                   write     fivab000                                     => scarico il VAB
027800100525     C                   endif
027900010305     C*
028000010305     C                   ENDSR
028100990920
028200000801     C*----------------------------------------------------*
028300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028400000801     C*----------------------------------------------------*
028500010201     C     INZVAR        BEGSR
028600000801     C*
028700010201     C                   Z-ADD     *zeros        Num5_0
028800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028900020725     C                   MOVEL     '0'           FlgCAS            1
029000000801     C*
029100000801     C                   ENDSR
029200000801     C*----------------------------------------------------*
029300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029400000801     C*----------------------------------------------------*
029500020904     C     DEFCAM        BEGSR
029600080923     C*
029700100525     C                   SETOFF                                       5051
029800000801     C*
029900021113     C                   CLEAR                   FIVAB000
030000021113     C                   CLEAR                   FIVAT000
030100070730     C* Imposto i valori di default...
030200180117     C                   EVAL      VABCCM = 2553993
030300180117     C                   EVAL      VATCCM = VABCCM
030400180117     C                   EVAL      VABLNP = 114
030500180117     C                   EVAL      VATLNP = VABLNP
030600170912     C                   EVAL      VABCTR = 000
030700100525     C                   EVAL      VABCBO = '1'
030800180117     C                   EVAL      VABTSP = 'C'
030900070222     C* ... e poi verifico se sono stati passati come parametri
031000070222     C                   IF        vlrppt > *blanks
031100100525     C*
031200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABCCM
031700070222     C                   Z-ADD     PiVal         VATCCM
031800070222     C                   ENDIF
031900070222     C                   ENDIF
032000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABLNP
032500070222     C                   Z-ADD     PiVal         VATLNP
032600070222     C                   ENDIF
032700070222     C                   ENDIF
032800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000070222     C                   EXSR      CHKNUM
033100070222     C                   IF        PiInt=*on
033200070222     C                   Z-ADD     PiVal         VABCTR
033300070222     C                   ENDIF
033400070928     C                   ENDIF
033500071009     C                   MOVEL     *blanks       wDISK             1
033600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033700071009     C                   IF        wDISK <> *blanks
033800070928     C                   SETON                                        50
033900070222     C                   ENDIF
034000100525     C                   IF        wDISK  = 'T'
034100100525     C                   SETOFF                                       50
034200100525     C                   SETON                                        51
034300100525     C                   ENDIF
034400100401     C                   MOVEL     *blanks       wDUPREC           1
034500100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
034600100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
034700100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
034800100525     C                   ENDIF
034900100525     C*
035000070222     C                   ENDIF
035100071009     C*
035200071009     C   50              EVAL      VABCTM = '7Q'
035300000801     C*
035400000801     C                   ENDSR
035500091223     C*----------------------------------------------------*
035600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
035700091223     C*----------------------------------------------------*
035800091223     C     REPNSP        BEGSR
035900091223     C*
036000091223     C                   EXSR      INZVAR
036100091223     C                   EXSR      DEFCAM
036200091223     C*
036300091223     C* NSP => Stacco un numeratore da AZNUM
036400091223     C                   clear                   TRUL33DS
036500091223     C                   eval      I33OPE = *zeros
036600091223     C                   eval      I33CNU = 302
036700091223     C                   eval      I33NUM = 1
036800091223     C                   movel     TRUL33DS      KPJBU
036900091223     C                   call      'TRUL33R'
037000091223     C                   parm                    KPJBA
037100091223     C                   movel     KPJBU         TRUL33DS
037200091223     C                   if        O33ERR = *zeros
037300091223     C                   z-add     O33NRF        VABNSP
037400091223     C                   z-add     O33NRF        VATNSP
037500091223     C                   else
037600091223     C                   SETON                                        31
037700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037800091223     C                             + ' ' + 'VABNSP VATNSP'
037900091223     C                   endif
038000091223     C*
038100091223     C                   ENDSR
038200000801     C*----------------------------------------------------*
038300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
038400000801     C*----------------------------------------------------*
038500100730     C     IMPVAB        BEGSR
038600070530     C*
038700070530     C                   SETOFF                                       3132
038800070928     C*
038900070928     C                   MOVE(P)   vlrpoi        VABFGS
039000070928     C                   MOVE(P)   vlrpoi        VATFGS
039100070928     C*
039200070928     C                   MOVEL     datcor        VABAAS
039300070928     C                   MOVEL     datcor        VATAAS
039400070928     C                   MOVE      datcor        VABMGS
039500100729     C*
039600100729     C* Reperimento campi ALFA
039700100729     C                   EVAL      VABRMO=%trim(%subst(vindta:107:30))
039800100729     C                   EVAL      VABRSD=%trim(%subst(vindta:236:30))
039900100729     C                   EVAL      VABIND=%trim(%subst(vindta:266:30))
040000100729     C                   EVAL      VABLOD=%trim(%subst(vindta:301:25))
040100100729     C                   EVAL      VABPRD=%trim(%subst(vindta:326:2))
040200100729     C                   EVAL      VABRMA=%trim(%subst(vindta:44:12))
040300100730     C                   movel     *blanks       wNOTE            70
040400100730     C                   eval      wNOTE = %trim(%subst(vindta:328:30))+
040500100730     C                                     %trim(%subst(vindta:358:30))+
040600100730     C                                     %trim(%subst(vindta:502:30))+
040700100730     C                                     %trim(%subst(vindta:532:30))
040800100730     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
040900100730     C                   EVAL      VABNT2=%subst(wNOTE:1+35:35)
041000100729     C*
041100100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
041200100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041300100729     C     '@':'A'       XLATE     VABRSD        VABRSD
041400100729     C* ==
041500100729     C*
041600170912     C* GMA
041700170912     C                   SELECT
041800170912     C                   WHEN      %subst(vindta:418:2) = 'ML'
041900170912     C                   EVAL      VABGMA = 'F'
042000170912     C                   WHEN      %subst(vindta:418:2) = 'PT'
042100170912     C                   EVAL      VABTIC = 'A'
042200170912     C                   ENDSL
042300100730     C* CBO
042400100730     C                   SELECT
042500100730     C                   WHEN      %subst(vindta:62:1) = 'A'
042600100730     C                   EVAL      VABCBO = '2'
042700100730     C                   OTHER
042800100730     C                   EVAL      VABCBO = '1'
042900100730     C                   ENDSL
043000100729     C*
043100170912     C* Reperimento campi NUMERICI                                       .
043200100729     C                   MOVEL     DATCOR        VABAAS
043300100729     C                   MOVE      DATCOR        VABMGS
043400100730     C* RMN
043500100730     C                   EVAL      PiStr=%trim(%subst(vindta:449:15))
043600100730     C                   EXSR      CHKNUM
043700100730     C                   IF        PiInt=*on
043800100730     C                   Z-ADD     PiVal         VABRMN
043900100730     C                   ELSE
044000100730     C                   SETON                                        32
044100100730     C                   Z-ADD     1             VABRMN
044200100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044300100730     C                             + ' ' + 'VABRMN'
044400100730     C                   ENDIF
044500100729     C* CAD
044600100729     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
044700100729     C                   EXSR      CHKNUM
044800100729     C                   IF        PiInt=*on
044900100729     C                   Z-ADD     PiVal         Num5_0
045000100729     C                   MOVEL(p)  Num5_0        VABCAD
045100100729     C                   ELSE
045200100729     C                   SETON                                        32
045300100729     C                   EVAL      VABCAD = *zeros
045400100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045500100729     C                             + ' ' + 'VABCAD'
045600100729     C                   ENDIF
045700100729     C* DCR
045800100730     C                   IF        %subst(vindta:443:6) <> *blanks AND
045900100730     C                             %subst(vindta:443:6) <> *zeros
046000100730     C                   EVAL      PiStr='20'+%trim(%subst(vindta:443:6))
046100100729     C                   EXSR      CHKNUM
046200100729     C                   IF        PiInt=*on
046300100729     C                   Z-ADD     PiVal         VABDCR
046400100729     C                   ELSE
046500100729     C                   SETON                                        32
046600100729     C                   Z-ADD     *zeros        VABDCR
046700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046800100729     C                             + ' ' + 'VABDCR'
046900100729     C                   ENDIF
047000100729     C                   ENDIF
047100100729     C* NCL
047200100729     C                   EVAL      PiStr=%trim(%subst(vindta:400:6))
047300100729     C                   EXSR      CHKNUM
047400100729     C                   IF        PiInt=*on
047500100729     C                   Z-ADD     PiVal         VABNCL
047600100729     C                   ELSE
047700100729     C                   SETON                                        32
047800100729     C                   Z-ADD     *zeros        VABNCL
047900100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000100729     C                             + ' ' + 'VABNCL'
048100100729     C                   ENDIF
048200100729     C* PKB
048300100729     C                   EVAL      PiStr=%trim(%subst(vindta:406:7))
048400100729     C                   EXSR      CHKNUM
048500100729     C                   IF        PiNum=*on
048600100729     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
048700100729     C                   Z-ADD(H)  PiVal         VABPKB
048800100729     C                   ELSE
048900100729     C                   SETON                                        32
049000100729     C                   Z-ADD     *zeros        VABPKB
049100100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200100729     C                             + ' ' + 'VABPKB'
049300100729     C                   ENDIF
049400100730     C* VLB
049500100730     C                   EVAL      PiStr=%trim(%subst(vindta:413:5))
049600100730     C                   EXSR      CHKNUM
049700100730     C                   IF        PiNum=*on
049800100730     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
049900100730     C                   Z-ADD(H)  PiVal         VABVLB
050000100730     C                   ELSE
050100100730     C                   SETON                                        32
050200100730     C                   Z-ADD     *zeros        VABVLB
050300100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050400100730     C                             + ' ' + 'VABVLB'
050500100730     C                   ENDIF
050600100729     C* CAS
050700180117     C                   IF        %subst(vindta:63:10)<>'0000000.00' and
050800180117     C                             %subst(vindta:63:10)<>*blank
050900100729     C                   EVAL      FlgCAS = '1'
051000100729     C                   EVAL      VABVCA = 'EUR'
051100100729     C                   EVAL      PiStr=%trim(%subst(vindta:63:10))
051200100729     C                   EXSR      CHKNUM
051300100729     C                   IF        PiNum=*on
051400100729     C                   Z-ADD(H)  PiVal         VABCAS
051500170912     C* TIC
051600170912     C                   SELECT
051700170912     C                   WHEN      %subst(vindta:623:1) = '1'
051800170912     C                   EVAL      VABTIC = *blanks
051900170912     C                   WHEN      %subst(vindta:623:1) = '2'
052000170912     C                   EVAL      VABTIC = 'BM'
052100170912     C                   WHEN      %subst(vindta:623:1) = '3'
052200170912     C                   EVAL      VABTIC = 'BM'
052300170912     C                   ENDSL
052400100729     C                   ELSE
052500100729     C                   SETON                                        32
052600100729     C                   Z-ADD     *zeros        VABCAS
052700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052800100729     C                             + ' ' + 'VABCAS'
052900100729     C                   ENDIF
053000100729     C                   ENDIF
053100100729     C*
053200100729     C* Se provincia nn valorizzata la reperisco
053300100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
053400100729     C                   IF        VABNZD  = *blanks AND
053500100729     C                             VABPRD  = *blanks AND
053600100729     C                             VABCAD <> *blanks
053700100729     C                   CLEAR                   TISI95DS
053800100729     C                   EVAL      I95TCN = '3'
053900100729     C                   Z-ADD     datcor        I95DAT
054000100729     C                   EVAL      I95NAR = VABNZD
054100100729     C                   EVAL      I95CAP = VABCAD
054200100729     C                   EVAL      I95LOC = VABLOD
054300100729     C                   CALL      'TISI95R'
054400100729     C                   PARM                    TISI95DS
054500100729     C                   EVAL      VABPRD = O95PRV
054600100729     C                   ENDIF
054700100729     C*
054800100729     C* Considerazioni finali su CBO/CAS
054900100729     C                   IF        FlgCAS = '1'
055000100729     C                   IF        VABCBO = '1'
055100100729     C                   EVAL      VABCBO = '4'
055200100729     C                   ENDIF
055300100729     C                   IF        VABCBO = '2'
055400100729     C                   EVAL      VABCBO = '6'
055500100729     C                   ENDIF
055600100729     C                   ENDIF
055700010202     C*
055800000801     C* Ebbene...
055900000801     C                   ADD       1             §CTRMO
056000070530     C                   IF        *in31 <> *zeros OR
056100070530     C                             *in32 <> *zeros
056200000801     C                   ADD       1             §CTRNO
056300020725     C                   EVAL      x_vinflg = '2'
056400000801     C                   ELSE
056500010201     C                   ADD       1             §CTROKVB
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900100127     C*----------------------------------------------------*
057000100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057100100127     C*----------------------------------------------------*
057200100127     C     WRIVAT_A      BEGSR
057300100127     C*
057400100127     C* Valorizzo l buffer di scrittura del FIVAT
057500100127     C* - TIPO RECORD "A"
057600100412     C***                eval      VATTRC = 'A'
057700100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
057800100412     C***                exsr      wrivat
057900100127     C*
058000100127     C                   ENDSR
058100010201     C*----------------------------------------------------*
058200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
058300010201     C*----------------------------------------------------*
058400071003     C     WRIVAT_B      BEGSR
058500010201     C*
058600021113     C* Valorizzo l buffer di scrittura del FIVAT
058700070928     C* - TIPO RECORD "B"
058800100412     C***                eval      VATTRC = 'B'
058900100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
059000100412     C***                exsr      wrivat
059100010201     C*
059200010201     C                   ENDSR
059300071003     C*----------------------------------------------------*
059400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059500071003     C*----------------------------------------------------*
059600071003     C     WRIVAT_E      BEGSR
059700071003     C*
059800071003     C* Valorizzo l buffer di scrittura del FIVAT
059900071003     C* - TIPO RECORD "E"
060000100729     C*
060100100729     C                   Z-ADD     1             wCollo            3 0
060200170912     C                   MOVEL     *zeros        wColloAlfa1      12
060300100729     C*
060400100729     C                   EVAL      VATTRC='E'
060500170912     C                   EVAL      wColloAlfa1=
060600170912     C                                         %subst(vindta:44:12)
060700100729     C*
060800100729     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
060900100729     C                   dow       wCollo <= VABNCL and wCollo < 999
061000170912     C                   eval      VATNOT = 'N'     +
061100100730     C                                      wColloAlfa1 +
061200100730     C                                      %editc(wCollo : 'X')
061300100729     C                   exsr      wrivat                                       => scarico VAT
061400100729     C                   add       1             wCollo
061500100729     C                   enddo
061600071003     C*
061700071003     C                   ENDSR
061800100127     C*----------------------------------------------------*
061900100127     C*  SCARICO BUFFER FIVAT
062000100127     C*----------------------------------------------------*
062100100127     C     WRIVAT        BEGSR
062200100127     C*
062300100127     C                   if        vatNOT <> *blanks
062400100127     C                   ADD       1             §CTROKVT
062500100127     C                   write     FIVAT000
062600100127     C                   endif
062700100127     C*
062800100127     C                   ENDSR
062900020904
063000020904
063100010202     C*----------------------------------------------------*
063200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
063300010202     C*----------------------------------------------------*
063400100525     C     PREVASX       BEGSR
063500100525     C*
063600021113     C* Compongo il nome del membro da dare al FIVATWWR
063700010202     C                   eval      parmbr = vlrhdl
063800010202     C                   movel     'M'           parmbr
063900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
064000010202     C                   eval      paropz = '1'
064100100525     C*
064200010202     C* Effettuo la chiamata al CLLE preposto
064300100525     C  N51              call(e)   'TITVVTC'
064400010202     C                   parm                    parccm
064500010202     C                   parm                    parmbr
064600010202     C                   parm                    paropz
064700100525     C   51              call(e)   'TITVVBC'
064800100525     C                   parm                    parccm
064900100525     C                   parm                    parmbr
065000100525     C                   parm                    paropz
065100100525     C*
065200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065300010202     C                   if        %error
065400010202     C                   movel     '1'           chkcall
065500010202     C                   else
065600010202     C                   movel     '0'           chkcall
065700010202     C                   endif
065800010202     C*
065900010202     C                   ENDSR
066000000801     C*----------------------------------------------------*
066100000801     C*  CONTROLLO NUMERICITA' CAMPI
066200000801     C*----------------------------------------------------*
066300000801     C     CHKNUM        BEGSR
066400081113     C*
066500081113     C                   IF        PiDecChr = *blanks
066600100503     C                   EVAL      PiDecChr = '.'
066700081113     C                   ENDIF
066800091223     C*
066900081113     C                   callp(e)  UBISNUM_Check(PiStr
067000081113     C                                          :PiDecChr
067100081113     C                                          :PiVal
067200081113     C                                          :PiNum
067300081113     C                                          :PiInt)
067400081113     C*
067500000801     C                   IF        %error
067600000801     C                   EVAL      PiInt=*off
067700000801     C                   ENDIF
067800000801     C*
067900000801     C                   ENDSR
068000000801     C***
068100011113
068200011113
068300000801
068400000801
068500990920      /TITLE Invio dei dati al punto operativo.
068600010202     C     invio         BEGSR
068700990920     C*
068800021113     C* 1° invio FIVAT
068900010201     C                   reset                   dscmz
069000010201     C                   move      vlrpoi        cmzdst
069100021113     C                   eval      cmzfld = 'FIVATWWR'
069200010201     C                   eval      cmzmbd = vlrhdl
069300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069400021009     C***                if        prmfir = *blanks
069500021113     C                   eval      cmzfla = 'FIVAT00F'
069600021113     C                   eval      cmzmba = 'FIVAT00F'
069700021009     C***                else
069800021009     C***                eval      cmzfla = prmfir
069900021009     C***                eval      cmzmba = prmfir
070000021009     C***                endif
070100010201     C                   eval      cmznrr = *zeros
070200020305     C                   move      §ctrokvt      cmznrr
070300021018     C                   eval      cmzlba = vlrfl1
070400010201     C                   call(e)   'TIS711C'
070500010201     C                   parm                    dscmz
070600010201     C                   parm      *blanks       esito
070700010205     C                   if        %error
070800010205     C                             or cmzerr = '1'
070900010205     C                             or esito  = '1'
071000010205     C                   eval      wrkesito = '3'
071100010205     C                   else
071200010201     C*
071300021113     C* 2° invio FIVAB
071400100525     C                   if        *in51 = *off
071500010201     C                   reset                   dscmz
071600010201     C                   move      vlrpoi        cmzdst
071700010201     C                   eval      cmzfld = vlrfou
071800010201     C                   eval      cmzmbd = vlrhdl
071900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072000021009     C***                if        prmfir = *blanks
072100021113     C                   eval      cmzfla = 'FIVAB00F'
072200021113     C                   eval      cmzmba = 'FIVAB00F'
072300021009     C***                else
072400021009     C***                eval      cmzfla = prmfir
072500021009     C***                eval      cmzmba = prmfir
072600021009     C***                endif
072700010201     C                   eval      cmznrr = *zeros
072800010201     C                   move      §ctrokvb      cmznrr
072900021018     C                   eval      cmzlba = vlrfl1
073000010201     C                   call(e)   'TIS711C'
073100010201     C                   parm                    dscmz
073200010201     C                   parm      *blanks       esito
073300010201     C                   if        %error
073400010201     C                             or cmzerr = '1'
073500010201     C                             or esito  = '1'
073600010201     C                   eval      wrkesito = '3'
073700010201     C                   endif
073800100525     C                   endif
073900010205     C                   endif
074000990920     C*
074100000613     C                   ENDSR
074200000613     C***
074300070411
074400070411     C     *pssr         BEGSR
074500070411     C*
074600070411     C                   if        %open(tivin00r)
074700070411     C                   close     tivin00r
074800070411     C                   endif
074900070411     C                   if        %open(fivabwwr)
075000070411     C                   close     fivabwwr
075100070411     C                   endif
075200070411     C                   if        %open(fivatwwr)
075300070411     C                   close     fivatwwr
075400070411     C                   endif
075500070411     C*
075600070411     C* Effettuo la chiamata al CLLE preposto
075700100525     C  N51              call(e)   'TITVVTC'
075800100525     C                   parm                    parccm
075900100525     C                   parm                    parmbr
076000100525     C                   parm      '2'           paropz
076100100525     C   51              call(e)   'TITVVBC'
076200100525     C                   parm                    parccm
076300100525     C                   parm                    parmbr
076400100525     C                   parm      '2'           paropz
076500070411     C*
076600070411     C                   eval      wrkesito = '2'
076700070411     C*
076800070411     C                   seton                                        LR
076900070411     C*
077000070411     C                   ENDSR     '*CANCL'
077100070411     C***
077200070411
077300990910
077400000613     C     *inzsr        BEGSR
077500990910     C*
077600990910     C     *entry        plist
077700990920     C                   parm                    tivlrds
077800990921     C                   parm      wrkesito      esito
077900000724     C                   parm                    prmlit
078000000710     C                   parm                    prmfir
078100000613     C*
078200000830     C* CALCOLA LA DATA CORRENTE
078300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078400091223     C                   eval      datcor = %dec(%date() : *ISO)
078500000830     C*
078600000613     C                   ENDSR
078700000613     C***
