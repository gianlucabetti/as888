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
030200121115     C                   EVAL      VABCCM = 2221405
030300121115     C                   EVAL      VATCCM = 2221405
030400121115     C                   EVAL      VABLNP = 222
030500121115     C                   EVAL      VATLNP = 222
030600100729     C                   EVAL      VABCTR = 001
030700100525     C                   EVAL      VABCBO = '1'
030800070222     C* ... e poi verifico se sono stati passati come parametri
030900070222     C                   IF        vlrppt > *blanks
031000100525     C*
031100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031300070222     C                   EXSR      CHKNUM
031400070222     C                   IF        PiInt=*on
031500070222     C                   Z-ADD     PiVal         VABCCM
031600070222     C                   Z-ADD     PiVal         VATCCM
031700070222     C                   ENDIF
031800070222     C                   ENDIF
031900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100070222     C                   EXSR      CHKNUM
032200070222     C                   IF        PiInt=*on
032300070222     C                   Z-ADD     PiVal         VABLNP
032400070222     C                   Z-ADD     PiVal         VATLNP
032500070222     C                   ENDIF
032600070222     C                   ENDIF
032700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032900070222     C                   EXSR      CHKNUM
033000070222     C                   IF        PiInt=*on
033100070222     C                   Z-ADD     PiVal         VABCTR
033200070222     C                   ENDIF
033300070928     C                   ENDIF
033400071009     C                   MOVEL     *blanks       wDISK             1
033500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033600071009     C                   IF        wDISK <> *blanks
033700070928     C                   SETON                                        50
033800070222     C                   ENDIF
033900100525     C                   IF        wDISK  = 'T'
034000100525     C                   SETOFF                                       50
034100100525     C                   SETON                                        51
034200100525     C                   ENDIF
034300100401     C                   MOVEL     *blanks       wDUPREC           1
034400100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
034500100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
034600100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
034700100525     C                   ENDIF
034800100525     C*
034900070222     C                   ENDIF
035000071009     C*
035100071009     C   50              EVAL      VABCTM = '7Q'
035200000801     C*
035300000801     C                   ENDSR
035400091223     C*----------------------------------------------------*
035500091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
035600091223     C*----------------------------------------------------*
035700091223     C     REPNSP        BEGSR
035800091223     C*
035900091223     C                   EXSR      INZVAR
036000091223     C                   EXSR      DEFCAM
036100091223     C*
036200091223     C* NSP => Stacco un numeratore da AZNUM
036300091223     C                   clear                   TRUL33DS
036400091223     C                   eval      I33OPE = *zeros
036500091223     C                   eval      I33CNU = 302
036600091223     C                   eval      I33NUM = 1
036700091223     C                   movel     TRUL33DS      KPJBU
036800091223     C                   call      'TRUL33R'
036900091223     C                   parm                    KPJBA
037000091223     C                   movel     KPJBU         TRUL33DS
037100091223     C                   if        O33ERR = *zeros
037200091223     C                   z-add     O33NRF        VABNSP
037300091223     C                   z-add     O33NRF        VATNSP
037400091223     C                   else
037500091223     C                   SETON                                        31
037600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037700091223     C                             + ' ' + 'VABNSP VATNSP'
037800091223     C                   endif
037900091223     C*
038000091223     C                   ENDSR
038100000801     C*----------------------------------------------------*
038200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
038300000801     C*----------------------------------------------------*
038400100730     C     IMPVAB        BEGSR
038500070530     C*
038600070530     C                   SETOFF                                       3132
038700070928     C*
038800070928     C                   MOVE(P)   vlrpoi        VABFGS
038900070928     C                   MOVE(P)   vlrpoi        VATFGS
039000070928     C*
039100070928     C                   MOVEL     datcor        VABAAS
039200070928     C                   MOVEL     datcor        VATAAS
039300070928     C                   MOVE      datcor        VABMGS
039400100729     C*
039500100729     C* Reperimento campi ALFA
039600100729     C                   EVAL      VABRMO=%trim(%subst(vindta:107:30))
039700100729     C                   EVAL      VABRSD=%trim(%subst(vindta:236:30))
039800100729     C                   EVAL      VABIND=%trim(%subst(vindta:266:30))
039900100729     C                   EVAL      VABLOD=%trim(%subst(vindta:301:25))
040000100729     C                   EVAL      VABPRD=%trim(%subst(vindta:326:2))
040100100729     C                   EVAL      VABRMA=%trim(%subst(vindta:44:12))
040200100730     C                   movel     *blanks       wNOTE            70
040300100730     C                   eval      wNOTE = %trim(%subst(vindta:328:30))+
040400100730     C                                     %trim(%subst(vindta:358:30))+
040500100730     C                                     %trim(%subst(vindta:502:30))+
040600100730     C                                     %trim(%subst(vindta:532:30))
040700100730     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
040800100730     C                   EVAL      VABNT2=%subst(wNOTE:1+35:35)
040900100729     C*
041000100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
041100100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041200100729     C     '@':'A'       XLATE     VABRSD        VABRSD
041300100729     C* ==
041400100729     C*
041500100730     C* CBO
041600100730     C                   SELECT
041700100730     C                   WHEN      %subst(vindta:62:1) = 'A'
041800100730     C                   EVAL      VABCBO = '2'
041900100730     C                   OTHER
042000100730     C                   EVAL      VABCBO = '1'
042100100730     C                   ENDSL
042200100730     C* TIC
042300100730     C                   SELECT
042400100730     C                   WHEN      %subst(vindta:623:1) = '1'
042500100730     C                   EVAL      VABTIC = *blanks
042600100730     C                   WHEN      %subst(vindta:623:1) = '2'
042700100730     C                   EVAL      VABTIC = 'BM'
042800100730     C                   WHEN      %subst(vindta:623:1) = '3'
042900100730     C                   EVAL      VABTIC = 'BM'
043000100730     C                   ENDSL
043100100729     C*
043200100729     C* Reperimento campi NUMERICI
043300100729     C                   MOVEL     DATCOR        VABAAS
043400100729     C                   MOVE      DATCOR        VABMGS
043500100730     C* RMN
043600100730     C                   EVAL      PiStr=%trim(%subst(vindta:449:15))
043700100730     C                   EXSR      CHKNUM
043800100730     C                   IF        PiInt=*on
043900100730     C                   Z-ADD     PiVal         VABRMN
044000100730     C                   ELSE
044100100730     C                   SETON                                        32
044200100730     C                   Z-ADD     1             VABRMN
044300100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044400100730     C                             + ' ' + 'VABRMN'
044500100730     C                   ENDIF
044600100729     C* CAD
044700100729     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
044800100729     C                   EXSR      CHKNUM
044900100729     C                   IF        PiInt=*on
045000100729     C                   Z-ADD     PiVal         Num5_0
045100100729     C                   MOVEL(p)  Num5_0        VABCAD
045200100729     C                   ELSE
045300100729     C                   SETON                                        32
045400100729     C                   EVAL      VABCAD = *zeros
045500100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600100729     C                             + ' ' + 'VABCAD'
045700100729     C                   ENDIF
045800100729     C* DCR
045900100730     C                   IF        %subst(vindta:443:6) <> *blanks AND
046000100730     C                             %subst(vindta:443:6) <> *zeros
046100100730     C                   EVAL      PiStr='20'+%trim(%subst(vindta:443:6))
046200100729     C                   EXSR      CHKNUM
046300100729     C                   IF        PiInt=*on
046400100729     C                   Z-ADD     PiVal         VABDCR
046500100729     C                   ELSE
046600100729     C                   SETON                                        32
046700100729     C                   Z-ADD     *zeros        VABDCR
046800100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900100729     C                             + ' ' + 'VABDCR'
047000100729     C                   ENDIF
047100100729     C                   ENDIF
047200100729     C* NCL
047300100729     C                   EVAL      PiStr=%trim(%subst(vindta:400:6))
047400100729     C                   EXSR      CHKNUM
047500100729     C                   IF        PiInt=*on
047600100729     C                   Z-ADD     PiVal         VABNCL
047700100729     C                   ELSE
047800100729     C                   SETON                                        32
047900100729     C                   Z-ADD     *zeros        VABNCL
048000100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100100729     C                             + ' ' + 'VABNCL'
048200100729     C                   ENDIF
048300100729     C* PKB
048400100729     C                   EVAL      PiStr=%trim(%subst(vindta:406:7))
048500100729     C                   EXSR      CHKNUM
048600100729     C                   IF        PiNum=*on
048700100729     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
048800100729     C                   Z-ADD(H)  PiVal         VABPKB
048900100729     C                   ELSE
049000100729     C                   SETON                                        32
049100100729     C                   Z-ADD     *zeros        VABPKB
049200100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049300100729     C                             + ' ' + 'VABPKB'
049400100729     C                   ENDIF
049500100730     C* VLB
049600100730     C                   EVAL      PiStr=%trim(%subst(vindta:413:5))
049700100730     C                   EXSR      CHKNUM
049800100730     C                   IF        PiNum=*on
049900100730     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
050000100730     C                   Z-ADD(H)  PiVal         VABVLB
050100100730     C                   ELSE
050200100730     C                   SETON                                        32
050300100730     C                   Z-ADD     *zeros        VABVLB
050400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050500100730     C                             + ' ' + 'VABVLB'
050600100730     C                   ENDIF
050700100729     C* CAS
050800100730     C                   IF        %subst(vindta:63:10)<>'0000000.00'
050900100729     C                   EVAL      FlgCAS = '1'
051000100729     C                   EVAL      VABVCA = 'EUR'
051100100729     C                   EVAL      PiStr=%trim(%subst(vindta:63:10))
051200100729     C                   EXSR      CHKNUM
051300100729     C                   IF        PiNum=*on
051400100729     C                   Z-ADD(H)  PiVal         VABCAS
051500100729     C                   ELSE
051600100729     C                   SETON                                        32
051700100729     C                   Z-ADD     *zeros        VABCAS
051800100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051900100729     C                             + ' ' + 'VABCAS'
052000100729     C                   ENDIF
052100100729     C                   ENDIF
052200100729     C*
052300100729     C* Se provincia nn valorizzata la reperisco
052400100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
052500100729     C                   IF        VABNZD  = *blanks AND
052600100729     C                             VABPRD  = *blanks AND
052700100729     C                             VABCAD <> *blanks
052800100729     C                   CLEAR                   TISI95DS
052900100729     C                   EVAL      I95TCN = '3'
053000100729     C                   Z-ADD     datcor        I95DAT
053100100729     C                   EVAL      I95NAR = VABNZD
053200100729     C                   EVAL      I95CAP = VABCAD
053300100729     C                   EVAL      I95LOC = VABLOD
053400100729     C                   CALL      'TISI95R'
053500100729     C                   PARM                    TISI95DS
053600100729     C                   EVAL      VABPRD = O95PRV
053700100729     C                   ENDIF
053800100729     C*
053900100729     C* Considerazioni finali su CBO/CAS
054000100729     C                   IF        FlgCAS = '1'
054100100729     C                   IF        VABCBO = '1'
054200100729     C                   EVAL      VABCBO = '4'
054300100729     C                   ENDIF
054400100729     C                   IF        VABCBO = '2'
054500100729     C                   EVAL      VABCBO = '6'
054600100729     C                   ENDIF
054700100729     C                   ENDIF
054800010202     C*
054900000801     C* Ebbene...
055000000801     C                   ADD       1             §CTRMO
055100070530     C                   IF        *in31 <> *zeros OR
055200070530     C                             *in32 <> *zeros
055300000801     C                   ADD       1             §CTRNO
055400020725     C                   EVAL      x_vinflg = '2'
055500000801     C                   ELSE
055600010201     C                   ADD       1             §CTROKVB
055700000801     C                   ENDIF
055800000801     C*
055900000801     C                   ENDSR
056000100127     C*----------------------------------------------------*
056100100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
056200100127     C*----------------------------------------------------*
056300100127     C     WRIVAT_A      BEGSR
056400100127     C*
056500100127     C* Valorizzo l buffer di scrittura del FIVAT
056600100127     C* - TIPO RECORD "A"
056700100412     C***                eval      VATTRC = 'A'
056800100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
056900100412     C***                exsr      wrivat
057000100127     C*
057100100127     C                   ENDSR
057200010201     C*----------------------------------------------------*
057300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
057400010201     C*----------------------------------------------------*
057500071003     C     WRIVAT_B      BEGSR
057600010201     C*
057700021113     C* Valorizzo l buffer di scrittura del FIVAT
057800070928     C* - TIPO RECORD "B"
057900100412     C***                eval      VATTRC = 'B'
058000100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
058100100412     C***                exsr      wrivat
058200010201     C*
058300010201     C                   ENDSR
058400071003     C*----------------------------------------------------*
058500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058600071003     C*----------------------------------------------------*
058700071003     C     WRIVAT_E      BEGSR
058800071003     C*
058900071003     C* Valorizzo l buffer di scrittura del FIVAT
059000071003     C* - TIPO RECORD "E"
059100100729     C*
059200100729     C                   Z-ADD     1             wCollo            3 0
059300100730     C                   MOVEL     *zeros        wColloAlfa1       8
059400100729     C*
059500100729     C                   EVAL      VATTRC='E'
059600100729     C                   EVAL      wColloAlfa1=%subst(vindta:48:8)
059700100729     C*
059800100729     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
059900100729     C                   dow       wCollo <= VABNCL and wCollo < 999
060000121115     C                   eval      VATNOT = 'SMAS'     +
060100100730     C                                      wColloAlfa1 +
060200100730     C                                      %editc(wCollo : 'X')
060300100729     C                   exsr      wrivat                                       => scarico VAT
060400100729     C                   add       1             wCollo
060500100729     C                   enddo
060600071003     C*
060700071003     C                   ENDSR
060800100127     C*----------------------------------------------------*
060900100127     C*  SCARICO BUFFER FIVAT
061000100127     C*----------------------------------------------------*
061100100127     C     WRIVAT        BEGSR
061200100127     C*
061300100127     C                   if        vatNOT <> *blanks
061400100127     C                   ADD       1             §CTROKVT
061500100127     C                   write     FIVAT000
061600100127     C                   endif
061700100127     C*
061800100127     C                   ENDSR
061900020904
062000020904
062100010202     C*----------------------------------------------------*
062200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
062300010202     C*----------------------------------------------------*
062400100525     C     PREVASX       BEGSR
062500100525     C*
062600021113     C* Compongo il nome del membro da dare al FIVATWWR
062700010202     C                   eval      parmbr = vlrhdl
062800010202     C                   movel     'M'           parmbr
062900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063000010202     C                   eval      paropz = '1'
063100100525     C*
063200010202     C* Effettuo la chiamata al CLLE preposto
063300100525     C  N51              call(e)   'TITVVTC'
063400010202     C                   parm                    parccm
063500010202     C                   parm                    parmbr
063600010202     C                   parm                    paropz
063700100525     C   51              call(e)   'TITVVBC'
063800100525     C                   parm                    parccm
063900100525     C                   parm                    parmbr
064000100525     C                   parm                    paropz
064100100525     C*
064200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064300010202     C                   if        %error
064400010202     C                   movel     '1'           chkcall
064500010202     C                   else
064600010202     C                   movel     '0'           chkcall
064700010202     C                   endif
064800010202     C*
064900010202     C                   ENDSR
065000000801     C*----------------------------------------------------*
065100000801     C*  CONTROLLO NUMERICITA' CAMPI
065200000801     C*----------------------------------------------------*
065300000801     C     CHKNUM        BEGSR
065400081113     C*
065500081113     C                   IF        PiDecChr = *blanks
065600100503     C                   EVAL      PiDecChr = '.'
065700081113     C                   ENDIF
065800091223     C*
065900081113     C                   callp(e)  UBISNUM_Check(PiStr
066000081113     C                                          :PiDecChr
066100081113     C                                          :PiVal
066200081113     C                                          :PiNum
066300081113     C                                          :PiInt)
066400081113     C*
066500000801     C                   IF        %error
066600000801     C                   EVAL      PiInt=*off
066700000801     C                   ENDIF
066800000801     C*
066900000801     C                   ENDSR
067000000801     C***
067100011113
067200011113
067300000801
067400000801
067500990920      /TITLE Invio dei dati al punto operativo.
067600010202     C     invio         BEGSR
067700990920     C*
067800021113     C* 1° invio FIVAT
067900010201     C                   reset                   dscmz
068000010201     C                   move      vlrpoi        cmzdst
068100021113     C                   eval      cmzfld = 'FIVATWWR'
068200010201     C                   eval      cmzmbd = vlrhdl
068300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068400021009     C***                if        prmfir = *blanks
068500021113     C                   eval      cmzfla = 'FIVAT00F'
068600021113     C                   eval      cmzmba = 'FIVAT00F'
068700021009     C***                else
068800021009     C***                eval      cmzfla = prmfir
068900021009     C***                eval      cmzmba = prmfir
069000021009     C***                endif
069100010201     C                   eval      cmznrr = *zeros
069200020305     C                   move      §ctrokvt      cmznrr
069300021018     C                   eval      cmzlba = vlrfl1
069400010201     C                   call(e)   'TIS711C'
069500010201     C                   parm                    dscmz
069600010201     C                   parm      *blanks       esito
069700010205     C                   if        %error
069800010205     C                             or cmzerr = '1'
069900010205     C                             or esito  = '1'
070000010205     C                   eval      wrkesito = '3'
070100010205     C                   else
070200010201     C*
070300021113     C* 2° invio FIVAB
070400100525     C                   if        *in51 = *off
070500010201     C                   reset                   dscmz
070600010201     C                   move      vlrpoi        cmzdst
070700010201     C                   eval      cmzfld = vlrfou
070800010201     C                   eval      cmzmbd = vlrhdl
070900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071000021009     C***                if        prmfir = *blanks
071100021113     C                   eval      cmzfla = 'FIVAB00F'
071200021113     C                   eval      cmzmba = 'FIVAB00F'
071300021009     C***                else
071400021009     C***                eval      cmzfla = prmfir
071500021009     C***                eval      cmzmba = prmfir
071600021009     C***                endif
071700010201     C                   eval      cmznrr = *zeros
071800010201     C                   move      §ctrokvb      cmznrr
071900021018     C                   eval      cmzlba = vlrfl1
072000010201     C                   call(e)   'TIS711C'
072100010201     C                   parm                    dscmz
072200010201     C                   parm      *blanks       esito
072300010201     C                   if        %error
072400010201     C                             or cmzerr = '1'
072500010201     C                             or esito  = '1'
072600010201     C                   eval      wrkesito = '3'
072700010201     C                   endif
072800100525     C                   endif
072900010205     C                   endif
073000990920     C*
073100000613     C                   ENDSR
073200000613     C***
073300070411
073400070411     C     *pssr         BEGSR
073500070411     C*
073600070411     C                   if        %open(tivin00r)
073700070411     C                   close     tivin00r
073800070411     C                   endif
073900070411     C                   if        %open(fivabwwr)
074000070411     C                   close     fivabwwr
074100070411     C                   endif
074200070411     C                   if        %open(fivatwwr)
074300070411     C                   close     fivatwwr
074400070411     C                   endif
074500070411     C*
074600070411     C* Effettuo la chiamata al CLLE preposto
074700100525     C  N51              call(e)   'TITVVTC'
074800100525     C                   parm                    parccm
074900100525     C                   parm                    parmbr
075000100525     C                   parm      '2'           paropz
075100100525     C   51              call(e)   'TITVVBC'
075200100525     C                   parm                    parccm
075300100525     C                   parm                    parmbr
075400100525     C                   parm      '2'           paropz
075500070411     C*
075600070411     C                   eval      wrkesito = '2'
075700070411     C*
075800070411     C                   seton                                        LR
075900070411     C*
076000070411     C                   ENDSR     '*CANCL'
076100070411     C***
076200070411
076300990910
076400000613     C     *inzsr        BEGSR
076500990910     C*
076600990910     C     *entry        plist
076700990920     C                   parm                    tivlrds
076800990921     C                   parm      wrkesito      esito
076900000724     C                   parm                    prmlit
077000000710     C                   parm                    prmfir
077100000613     C*
077200000830     C* CALCOLA LA DATA CORRENTE
077300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077400091223     C                   eval      datcor = %dec(%date() : *ISO)
077500000830     C*
077600000613     C                   ENDSR
077700000613     C***
