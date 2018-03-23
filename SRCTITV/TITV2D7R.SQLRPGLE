000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
009100990908
009200000801
009300910830     C*--------------------------------------------------------
009400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009500910830     C*--------------------------------------------------------
009600070530     C     RWFILE        BEGSR
009700990910     C*
009800990914     C                   if        not %open(tivin00r)
009900990908     C                   open      tivin00r
010000990914     C                   endif
010100070530     C*
010200100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010300100525     C                   exsr      prevasx
010400010201     C*
010500010202     C                   if        chkcall = '0'
010600010202     C*
010700100525     C                   if        not %open(fivabwwr)
010800100525     C                   open      fivabwwr
010900100525     C                   endif
011000100525     C*
011100021113     C                   if        not %open(fivatwwr)
011200021113     C                   open      fivatwwr
011300010201     C                   endif
011400080117     C*
011500080117     C                   EXSR      INZVAR
011600080117     C                   EXSR      DEFCAM
011700990910     C*
011800010201     C                   clear                   §CTROKVB          5 0
011900020305     C                   clear                   §CTROKVT          5 0
012000000801     C                   clear                   §CTRMO            5 0
012100000801     C                   clear                   §CTRNO            5 0
012200990910     C*
012300020725     C/EXEC SQL
012400020725     C+ declare C1 cursor for select
012500100730     C+ vindta, vinflg, substr(vindta, 48, 8) as sped, rrn(tivin00r)
012600060223     C+ from tivin00r
012700060223     C+ order by sped
012800020725     C+ for read only
012900020725     C/END-EXEC
013000020725     C
013100020725     C/EXEC SQL
013200020725     C+ open C1
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600070530     C+ Fetch C1 into :INPUT_DS
013700020725     C/END-EXEC
013800020725     C*
013900020725     C                   dow       sqlcod = *zeros
014000990913     C*
014100100310     C                   if        vindta > *blanks
014200000613     C                   add       1             rrnum
014300000801     C*
014400020725     C                   if        vinflg = *blanks
014500020725     C                             or vinflg = '0'
014600020725     C                             or vinflg = '2'
014700000801     C*
014800020725     C                   clear                   x_vinmsg
014900020725     C                   eval      x_vinflg = '1'
015000010305     C*
015100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015200100730     C                   EVAL      curspe=%trim(%subst(vindta:48:8))
015300010305     C*
015400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015500080923     C                   if        *in50 = *off
015600071003     C                   exsr      impvab
015700100525     C                   exsr      wrivab
015800100127     C                   exsr      wrivat_a                                     => carico VAT
015900071003     C                   exsr      wrivat_b                                     => carico VAT
016000100525     C   51              exsr      wrivat_e                                     => carico VAT
016100071003     C                   else
016200080923     C*
016300071009     C                   if        wDISK = 'D'
016400091223     C                   exsr      repNSP
016500071009     C                   exsr      impvab
016600100412     C                   exsr      wrivab
016700100127     C                   exsr      wrivat_a                                     => carico VAT
016800071009     C                   exsr      wrivat_b                                     => carico VAT
016900071009     C                   exsr      wrivat_e                                     => carico VAT
017000071009     C                   else
017100071009     C*
017200010305     C                   if        depspe = *blanks                             => 1° giro
017300010305     C                   eval      depspe = curspe                              => memorizz. spediz
017400080117     C                   clear                   tivinds
017500091223     C                   exsr      repNSP
017600020305     C                   exsr      impvab
017700100127     C                   exsr      wrivat_a                                     => carico VAT
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300100412     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500091223     C                   exsr      repNSP
018600020305     C                   exsr      impvab
018700100127     C                   exsr      wrivat_a                                     => carico VAT
018800071003     C                   exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000020305     C                   else                                                   => x stessa spediz
019100100401     C*
019200100401     C* Se nn richiesta esclusione spedizioni "duplicate"
019300100412     C                   if        wDUPREC <> 'N'
019400020305     C                   exsr      impvab
019500100127     C                   exsr      wrivat_a                                     => carico VAT
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800100401     C                   endif
019900010305     C                   endif
020000010305     C                   endif
020100010305     C                   endif
020200071003     C                   endif
020300071009     C                   endif
020400000905     C*
020500000905     C                   else
020600020725     C                   eval      x_vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900020725     C     VINRRN        chain     tivin000
021000020725     C                   if        %found(tivin00r)
021100020725     C                   eval      y_vinflg = x_vinflg
021200020725     C                   eval      y_vinmsg = x_vinmsg
021300020725     C                   update    tivin000
021400020725     C                   endif
021500020725     C*
021600020725     C/EXEC SQL
021700070530     C+ Fetch C1 into :INPUT_DS
021800020725     C/END-EXEC
021900020725     C*
022000020725     C                   enddo
022100020725     C*
022200020725     C/EXEC SQL
022300020725     C+ close C1
022400020725     C/END-EXEC
022500000905     C*
022600020305     C* Scarico i VAB rimasti "in sospeso"
022700071009     C                   if        wDISK = 'C'
022800100412     C                   exsr      wrivab
022900071009     C                   endif
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800100525     C                   if        §ctrokvb > 0 OR
023900100525     C                             §ctrokvt > 0
024000990921     C                   eval      wrkesito = '1'
024100000710     C                   else
024200000710     C                   eval      wrkesito = '2'
024300990910     C                   endif
024400000710     C                   endif
024500990910     C*
024600990914     C                   if        %open(tivin00r)
024700990908     C                   close     tivin00r
024800990914     C                   endif
024900021113     C                   if        %open(fivabwwr)
025000021113     C                   close     fivabwwr
025100990914     C                   endif
025200021113     C                   if        %open(fivatwwr)
025300021113     C                   close     fivatwwr
025400010201     C                   endif
025500990910     C*
025600100525     C                   if        §ctrokvb > 0 OR
025700100525     C                             §ctrokvt > 0
025800000724     C                             and vlrpoi <> *zeros
025900010202     C                   exsr      invio
026000990920     C                   endif
026100990920     C*
026200910830     C                   ENDSR
026300000613     C***
026400100412
026500100412
026600010305
026700010305     C*----------------------------------------------------*
026800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026900010305     C*----------------------------------------------------*
027000020305     C     WRIVAB        BEGSR
027100100412     C*
027200100412     C* Gestisco l'eventuale rottura x numero spedizione
027300100412     C                   if        VABRMA = *blanks
027400100412     C                   movel(P)  VABRMN        VABRMA
027500100412     C                   endif
027600100412     C*
027700100525     C                   if        *in51 = *off and *in31 = *off
027800100525     C                   write     fivab000                                     => scarico il VAB
027900100525     C                   endif
028000010305     C*
028100010305     C                   ENDSR
028200990920
028300000801     C*----------------------------------------------------*
028400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028500000801     C*----------------------------------------------------*
028600010201     C     INZVAR        BEGSR
028700000801     C*
028800010201     C                   Z-ADD     *zeros        Num5_0
028900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029000020725     C                   MOVEL     '0'           FlgCAS            1
029100000801     C*
029200000801     C                   ENDSR
029300000801     C*----------------------------------------------------*
029400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029500000801     C*----------------------------------------------------*
029600020904     C     DEFCAM        BEGSR
029700080923     C*
029800100525     C                   SETOFF                                       5051
029900000801     C*
030000021113     C                   CLEAR                   FIVAB000
030100021113     C                   CLEAR                   FIVAT000
030200070730     C* Imposto i valori di default...
030300110329     C                   EVAL      VABCCM = 0120206
030400110329     C                   EVAL      VATCCM = 0120206
030500110329     C                   EVAL      VABLNP = 012
030600110329     C                   EVAL      VATLNP = 012
030700110329     C                   EVAL      VABCTR = 000
030800100525     C                   EVAL      VABCBO = '1'
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
041600100730     C* CBO
041700100730     C                   SELECT
041800100730     C                   WHEN      %subst(vindta:62:1) = 'A'
041900100730     C                   EVAL      VABCBO = '2'
042000100730     C                   OTHER
042100100730     C                   EVAL      VABCBO = '1'
042200100730     C                   ENDSL
042300100730     C* TIC
042400100730     C                   SELECT
042500100730     C                   WHEN      %subst(vindta:623:1) = '1'
042600100730     C                   EVAL      VABTIC = *blanks
042700100730     C                   WHEN      %subst(vindta:623:1) = '2'
042800100730     C                   EVAL      VABTIC = 'BM'
042900100730     C                   WHEN      %subst(vindta:623:1) = '3'
043000100730     C                   EVAL      VABTIC = 'BM'
043100100730     C                   ENDSL
043200100729     C* CCM/CTR
043300100730     C                   IF        %subst(vindta:44:4) = 'FAZA'
043400100730     C***                EVAL      VABCCM = 0054082
043500100730     C***                EVAL      VABCTR = 100
043600100729     C                   ENDIF
043700100729     C*
043800100729     C* Reperimento campi NUMERICI
043900100729     C                   MOVEL     DATCOR        VABAAS
044000100729     C                   MOVE      DATCOR        VABMGS
044100100730     C* RMN
044200120203     C***                EVAL      PiStr=%trim(%subst(vindta:449:15))
044300120203     C                   EVAL      PiStr=%trim(%subst(vindta:44+4:15-4))
044400100730     C                   EXSR      CHKNUM
044500100730     C                   IF        PiInt=*on
044600100730     C                   Z-ADD     PiVal         VABRMN
044700100730     C                   ELSE
044800100730     C                   SETON                                        32
044900100730     C                   Z-ADD     1             VABRMN
045000100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100100730     C                             + ' ' + 'VABRMN'
045200100730     C                   ENDIF
045300100729     C* CAD
045400100729     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
045500100729     C                   EXSR      CHKNUM
045600100729     C                   IF        PiInt=*on
045700100729     C                   Z-ADD     PiVal         Num5_0
045800100729     C                   MOVEL(p)  Num5_0        VABCAD
045900100729     C                   ELSE
046000100729     C                   SETON                                        32
046100100729     C                   EVAL      VABCAD = *zeros
046200100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300100729     C                             + ' ' + 'VABCAD'
046400100729     C                   ENDIF
046500100729     C* DCR
046600100730     C                   IF        %subst(vindta:443:6) <> *blanks AND
046700100730     C                             %subst(vindta:443:6) <> *zeros
046800100730     C                   EVAL      PiStr='20'+%trim(%subst(vindta:443:6))
046900100729     C                   EXSR      CHKNUM
047000100729     C                   IF        PiInt=*on
047100100729     C                   Z-ADD     PiVal         VABDCR
047200100729     C                   ELSE
047300100729     C                   SETON                                        32
047400100729     C                   Z-ADD     *zeros        VABDCR
047500100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600100729     C                             + ' ' + 'VABDCR'
047700100729     C                   ENDIF
047800100729     C                   ENDIF
047900100729     C* NCL
048000100729     C                   EVAL      PiStr=%trim(%subst(vindta:400:6))
048100100729     C                   EXSR      CHKNUM
048200100729     C                   IF        PiInt=*on
048300100729     C                   Z-ADD     PiVal         VABNCL
048400100729     C                   ELSE
048500100729     C                   SETON                                        32
048600100729     C                   Z-ADD     *zeros        VABNCL
048700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048800100729     C                             + ' ' + 'VABNCL'
048900100729     C                   ENDIF
049000100729     C* PKB
049100100729     C                   EVAL      PiStr=%trim(%subst(vindta:406:7))
049200100729     C                   EXSR      CHKNUM
049300100729     C                   IF        PiNum=*on
049400100729     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
049500100729     C                   Z-ADD(H)  PiVal         VABPKB
049600100729     C                   ELSE
049700100729     C                   SETON                                        32
049800100729     C                   Z-ADD     *zeros        VABPKB
049900100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050000100729     C                             + ' ' + 'VABPKB'
050100100729     C                   ENDIF
050200100730     C* VLB
050300100730     C                   EVAL      PiStr=%trim(%subst(vindta:413:5))
050400100730     C                   EXSR      CHKNUM
050500100730     C                   IF        PiNum=*on
050600100730     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
050700100730     C                   Z-ADD(H)  PiVal         VABVLB
050800100730     C                   ELSE
050900100730     C                   SETON                                        32
051000100730     C                   Z-ADD     *zeros        VABVLB
051100100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200100730     C                             + ' ' + 'VABVLB'
051300100730     C                   ENDIF
051400100729     C* CAS
051500110329     C                   IF        %subst(vindta:63:10)<>'0000000.00' AND
051600110329     C                             %subst(vindta:63:10)<>*blanks
051700100729     C                   EVAL      FlgCAS = '1'
051800100729     C                   EVAL      VABVCA = 'EUR'
051900100729     C                   EVAL      PiStr=%trim(%subst(vindta:63:10))
052000100729     C                   EXSR      CHKNUM
052100100729     C                   IF        PiNum=*on
052200100729     C                   Z-ADD(H)  PiVal         VABCAS
052300100729     C                   ELSE
052400100729     C                   SETON                                        32
052500100729     C                   Z-ADD     *zeros        VABCAS
052600100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052700100729     C                             + ' ' + 'VABCAS'
052800100729     C                   ENDIF
052900100729     C                   ENDIF
053000100729     C*
053100100729     C* Se provincia nn valorizzata la reperisco
053200100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
053300100729     C                   IF        VABNZD  = *blanks AND
053400100729     C                             VABPRD  = *blanks AND
053500100729     C                             VABCAD <> *blanks
053600100729     C                   CLEAR                   TISI95DS
053700100729     C                   EVAL      I95TCN = '3'
053800100729     C                   Z-ADD     datcor        I95DAT
053900100729     C                   EVAL      I95NAR = VABNZD
054000100729     C                   EVAL      I95CAP = VABCAD
054100100729     C                   EVAL      I95LOC = VABLOD
054200100729     C                   CALL      'TISI95R'
054300100729     C                   PARM                    TISI95DS
054400100729     C                   EVAL      VABPRD = O95PRV
054500100729     C                   ENDIF
054600100729     C*
054700100729     C* Considerazioni finali su CBO/CAS
054800100729     C                   IF        FlgCAS = '1'
054900100729     C                   IF        VABCBO = '1'
055000100729     C                   EVAL      VABCBO = '4'
055100100729     C                   ENDIF
055200100729     C                   IF        VABCBO = '2'
055300100729     C                   EVAL      VABCBO = '6'
055400100729     C                   ENDIF
055500100729     C                   ENDIF
055600010202     C*
055700000801     C* Ebbene...
055800000801     C                   ADD       1             §CTRMO
055900070530     C                   IF        *in31 <> *zeros OR
056000070530     C                             *in32 <> *zeros
056100000801     C                   ADD       1             §CTRNO
056200020725     C                   EVAL      x_vinflg = '2'
056300000801     C                   ELSE
056400010201     C                   ADD       1             §CTROKVB
056500000801     C                   ENDIF
056600000801     C*
056700000801     C                   ENDSR
056800100127     C*----------------------------------------------------*
056900100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057000100127     C*----------------------------------------------------*
057100100127     C     WRIVAT_A      BEGSR
057200100127     C*
057300100127     C* Valorizzo l buffer di scrittura del FIVAT
057400100127     C* - TIPO RECORD "A"
057500100412     C***                eval      VATTRC = 'A'
057600100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
057700100412     C***                exsr      wrivat
057800100127     C*
057900100127     C                   ENDSR
058000010201     C*----------------------------------------------------*
058100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
058200010201     C*----------------------------------------------------*
058300071003     C     WRIVAT_B      BEGSR
058400010201     C*
058500021113     C* Valorizzo l buffer di scrittura del FIVAT
058600070928     C* - TIPO RECORD "B"
058700100412     C***                eval      VATTRC = 'B'
058800100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
058900100412     C***                exsr      wrivat
059000010201     C*
059100010201     C                   ENDSR
059200071003     C*----------------------------------------------------*
059300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059400071003     C*----------------------------------------------------*
059500071003     C     WRIVAT_E      BEGSR
059600071003     C*
059700071003     C* Valorizzo l buffer di scrittura del FIVAT
059800071003     C* - TIPO RECORD "E"
059900100729     C*
060000100729     C                   Z-ADD     1             wCollo            3 0
060100100730     C                   MOVEL     *zeros        wColloAlfa1       8
060200100729     C*
060300100729     C                   EVAL      VATTRC='E'
060400100729     C                   EVAL      wColloAlfa1=%subst(vindta:48:8)
060500100729     C*
060600100729     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
060700100729     C                   dow       wCollo <= VABNCL and wCollo < 999
060800110329     C                   eval      VATNOT = 'NLOLO'     +
060900100730     C                                      wColloAlfa1 +
061000100730     C                                      %editc(wCollo : 'X')
061100100729     C                   exsr      wrivat                                       => scarico VAT
061200100729     C                   add       1             wCollo
061300100729     C                   enddo
061400071003     C*
061500071003     C                   ENDSR
061600100127     C*----------------------------------------------------*
061700100127     C*  SCARICO BUFFER FIVAT
061800100127     C*----------------------------------------------------*
061900100127     C     WRIVAT        BEGSR
062000100127     C*
062100100127     C                   if        vatNOT <> *blanks
062200100127     C                   ADD       1             §CTROKVT
062300100127     C                   write     FIVAT000
062400100127     C                   endif
062500100127     C*
062600100127     C                   ENDSR
062700020904
062800020904
062900010202     C*----------------------------------------------------*
063000100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
063100010202     C*----------------------------------------------------*
063200100525     C     PREVASX       BEGSR
063300100525     C*
063400021113     C* Compongo il nome del membro da dare al FIVATWWR
063500010202     C                   eval      parmbr = vlrhdl
063600010202     C                   movel     'M'           parmbr
063700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063800010202     C                   eval      paropz = '1'
063900100525     C*
064000010202     C* Effettuo la chiamata al CLLE preposto
064100100525     C  N51              call(e)   'TITVVTC'
064200010202     C                   parm                    parccm
064300010202     C                   parm                    parmbr
064400010202     C                   parm                    paropz
064500100525     C   51              call(e)   'TITVVBC'
064600100525     C                   parm                    parccm
064700100525     C                   parm                    parmbr
064800100525     C                   parm                    paropz
064900100525     C*
065000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065100010202     C                   if        %error
065200010202     C                   movel     '1'           chkcall
065300010202     C                   else
065400010202     C                   movel     '0'           chkcall
065500010202     C                   endif
065600010202     C*
065700010202     C                   ENDSR
065800000801     C*----------------------------------------------------*
065900000801     C*  CONTROLLO NUMERICITA' CAMPI
066000000801     C*----------------------------------------------------*
066100000801     C     CHKNUM        BEGSR
066200081113     C*
066300081113     C                   IF        PiDecChr = *blanks
066400100503     C                   EVAL      PiDecChr = '.'
066500081113     C                   ENDIF
066600091223     C*
066700081113     C                   callp(e)  UBISNUM_Check(PiStr
066800081113     C                                          :PiDecChr
066900081113     C                                          :PiVal
067000081113     C                                          :PiNum
067100081113     C                                          :PiInt)
067200081113     C*
067300000801     C                   IF        %error
067400000801     C                   EVAL      PiInt=*off
067500000801     C                   ENDIF
067600000801     C*
067700000801     C                   ENDSR
067800000801     C***
067900011113
068000011113
068100000801
068200000801
068300990920      /TITLE Invio dei dati al punto operativo.
068400010202     C     invio         BEGSR
068500990920     C*
068600021113     C* 1° invio FIVAT
068700010201     C                   reset                   dscmz
068800010201     C                   move      vlrpoi        cmzdst
068900021113     C                   eval      cmzfld = 'FIVATWWR'
069000010201     C                   eval      cmzmbd = vlrhdl
069100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069200021009     C***                if        prmfir = *blanks
069300021113     C                   eval      cmzfla = 'FIVAT00F'
069400021113     C                   eval      cmzmba = 'FIVAT00F'
069500021009     C***                else
069600021009     C***                eval      cmzfla = prmfir
069700021009     C***                eval      cmzmba = prmfir
069800021009     C***                endif
069900010201     C                   eval      cmznrr = *zeros
070000020305     C                   move      §ctrokvt      cmznrr
070100021018     C                   eval      cmzlba = vlrfl1
070200010201     C                   call(e)   'TIS711C'
070300010201     C                   parm                    dscmz
070400010201     C                   parm      *blanks       esito
070500010205     C                   if        %error
070600010205     C                             or cmzerr = '1'
070700010205     C                             or esito  = '1'
070800010205     C                   eval      wrkesito = '3'
070900010205     C                   else
071000010201     C*
071100021113     C* 2° invio FIVAB
071200100525     C                   if        *in51 = *off
071300010201     C                   reset                   dscmz
071400010201     C                   move      vlrpoi        cmzdst
071500010201     C                   eval      cmzfld = vlrfou
071600010201     C                   eval      cmzmbd = vlrhdl
071700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071800021009     C***                if        prmfir = *blanks
071900021113     C                   eval      cmzfla = 'FIVAB00F'
072000021113     C                   eval      cmzmba = 'FIVAB00F'
072100021009     C***                else
072200021009     C***                eval      cmzfla = prmfir
072300021009     C***                eval      cmzmba = prmfir
072400021009     C***                endif
072500010201     C                   eval      cmznrr = *zeros
072600010201     C                   move      §ctrokvb      cmznrr
072700021018     C                   eval      cmzlba = vlrfl1
072800010201     C                   call(e)   'TIS711C'
072900010201     C                   parm                    dscmz
073000010201     C                   parm      *blanks       esito
073100010201     C                   if        %error
073200010201     C                             or cmzerr = '1'
073300010201     C                             or esito  = '1'
073400010201     C                   eval      wrkesito = '3'
073500010201     C                   endif
073600100525     C                   endif
073700010205     C                   endif
073800990920     C*
073900000613     C                   ENDSR
074000000613     C***
074100070411
074200070411     C     *pssr         BEGSR
074300070411     C*
074400070411     C                   if        %open(tivin00r)
074500070411     C                   close     tivin00r
074600070411     C                   endif
074700070411     C                   if        %open(fivabwwr)
074800070411     C                   close     fivabwwr
074900070411     C                   endif
075000070411     C                   if        %open(fivatwwr)
075100070411     C                   close     fivatwwr
075200070411     C                   endif
075300070411     C*
075400070411     C* Effettuo la chiamata al CLLE preposto
075500100525     C  N51              call(e)   'TITVVTC'
075600100525     C                   parm                    parccm
075700100525     C                   parm                    parmbr
075800100525     C                   parm      '2'           paropz
075900100525     C   51              call(e)   'TITVVBC'
076000100525     C                   parm                    parccm
076100100525     C                   parm                    parmbr
076200100525     C                   parm      '2'           paropz
076300070411     C*
076400070411     C                   eval      wrkesito = '2'
076500070411     C*
076600070411     C                   seton                                        LR
076700070411     C*
076800070411     C                   ENDSR     '*CANCL'
076900070411     C***
077000070411
077100990910
077200000613     C     *inzsr        BEGSR
077300990910     C*
077400990910     C     *entry        plist
077500990920     C                   parm                    tivlrds
077600990921     C                   parm      wrkesito      esito
077700000724     C                   parm                    prmlit
077800000710     C                   parm                    prmfir
077900000613     C*
078000000830     C* CALCOLA LA DATA CORRENTE
078100091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078200091223     C                   eval      datcor = %dec(%date() : *ISO)
078300000830     C*
078400000613     C                   ENDSR
078500000613     C***
