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
002100100525     D depspe          s              5    INZ(*blanks)
002200100525     D curspe          s              5    INZ(*blanks)
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
003400020725     D  VINDTA                 1   2048
003500020725     D  VINFLG              2049   2049
003600020725     D  VINSPE              2050   2059
003700020725     D  VINRRN              2060   2067  0
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
012500100525     C+ vindta, vinflg, substr(vindta, 1, 5) as sped, rrn(tivin00r)
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
015200100525     C                   EVAL      curspe=%trim(%subst(vindta:1:5))
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
030300100525     C                   EVAL      VABCCM = 0771079
030400100525     C                   EVAL      VATCCM = 0771079
030500100525     C                   EVAL      VABLNP = 077
030600100525     C                   EVAL      VATLNP = 077
030700100525     C                   EVAL      VABCTR = 000
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
038500010201     C     IMPVAB        BEGSR
038600070530     C*
038700070530     C                   SETOFF                                       3132
038800070928     C*
038900070928     C                   MOVE(P)   vlrpoi        VABFGS
039000070928     C                   MOVE(P)   vlrpoi        VATFGS
039100070928     C*
039200070928     C                   MOVEL     datcor        VABAAS
039300070928     C                   MOVEL     datcor        VATAAS
039400070928     C                   MOVE      datcor        VABMGS
039500100525     C***                EVAL      VABRSD=%trim(%subst(vindta:224:30))
039600100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
039700100503     C     '@':'A'       XLATE     VABRSD        VABRSD
039800100503     C* ==
039900100525     C***                EVAL      VABIND=%trim(%subst(vindta:254:30))
040000100525     C***                EVAL      VABLOD=%trim(%subst(vindta:284:30))
040100100525     C***                EVAL      VABPRD=%trim(%subst(vindta:314:5))
040200100525     C***                EVAL      VABRMA=%trim(%subst(vindta:644:20))
040300100525     C***                EVAL      VABNOT=%trim(%subst(vindta:574:35))
040400100525     C***                EVAL      VABNT2=%trim(%subst(vindta:609:35))
040500100525     C***                EVAL      VABCBO=%trim(%subst(vindta:2:1))
040600100520     C*
040700100525     C* NSP
040800100525     C                   IF        *in50 = *off
040900100525     C                   EVAL      PiStr=%trim(%subst(vindta:1:5))
041000100525     C                   EXSR      CHKNUM
041100100525     C                   IF        PiInt=*on
041200100525     C                   Z-ADD     PiVal         VABNSP
041300100525     C                   Z-ADD     PiVal         VATNSP
041400100525     C                   ELSE
041500100525     C                   SETON                                        31
041600100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041700100525     C                             + ' ' + 'VABNSP VATNSP'
041800100525     C                   ENDIF
041900100525     C                   ENDIF
042000100525     C* RMN
042100100525     C***                EVAL      PiStr=%trim(%subst(vindta:1:5))
042200100525     C***                EXSR      CHKNUM
042300100525     C***                IF        PiInt=*on
042400100525     C***                Z-ADD     PiVal         VABRMN
042500100525     C***                ELSE
042600100525     C***                SETON                                        32
042700100525     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
042800100525     C***                          + ' ' + 'VABRMN'
042900100525     C***                ENDIF
043000100503     C* CAD
043100100525     C***                EVAL      PiStr=%trim(%subst(vindta:319:8))
043200100525     C***                EXSR      CHKNUM
043300100525     C***                IF        PiInt=*on
043400100525     C***                Z-ADD     PiVal         Num5_0
043500100525     C***                MOVEL     Num5_0        VABCAD
043600100525     C***                ELSE
043700100525     C***                SETON                                        32
043800100525     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
043900100525     C***                          + ' ' + 'VABCAD'
044000100525     C***                ENDIF
044100100503     C* NCL
044200100525     C***                EVAL      PiStr=%trim(%subst(vindta:364:5))
044300100525     C***                EXSR      CHKNUM
044400100525     C***                IF        PiInt=*on
044500100525     C***                Z-ADD     PiVal         VABNCL
044600100525     C***                ELSE
044700100525     C***                SETON                                        32
044800100525     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
044900100525     C***                          + ' ' + 'VABNCL'
045000100525     C***                ENDIF
045100100503     C* PKB
045200100525     C***                EVAL      PiStr=%trim(%subst(vindta:357:7))
045300100525     C***                EXSR      CHKNUM
045400100525     C***                IF        PiNum=*on
045500100525     C***                EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
045600100525     C***                Z-ADD     PiVal         VABPKB
045700100525     C***                ELSE
045800100525     C***                SETON                                        32
045900100525     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
046000100525     C***                          + ' ' + 'VABPKB'
046100100525     C***                ENDIF
046200100503     C* CAS
046300100525     C***                IF        %trim(%subst(vindta:388:13))<>*blanks
046400100525     C***                          AND
046500100525     C***                          %trim(%subst(vindta:388:13))<>'0000000000000'
046600100525     C***                          AND
046700100525     C***                          %trim(%subst(vindta:388:12))<>'0'
046800100525     C***                MOVEL     '1'           FlgCAS
046900100525     C***                EVAL      PiStr=%trim(%subst(vindta:388:13))
047000100525     C***                EXSR      CHKNUM
047100100525     C***                IF        PiNum=*on
047200100525     C***                EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
047300100525     C***                Z-ADD     PiVal         VABCAS
047400100525     C***                ELSE
047500100525     C***                SETON                                        32
047600100525     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
047700100525     C***                          + ' ' + 'VABCAS'
047800100525     C***                ENDIF
047900100525     C***                ENDIF
048000100310     C*
048100100310     C* Se provincia nn valorizzata la reperisco
048200100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
048300100525     C***                IF        VABNZD  = *blanks AND
048400100525     C***                          VABPRD  = *blanks AND
048500100525     C***                          VABCAD <> *blanks
048600100525     C***                CLEAR                   TISI95DS
048700100525     C***                EVAL      I95TCN = '3'
048800100525     C***                Z-ADD     datcor        I95DAT
048900100525     C***                EVAL      I95NAR = VABNZD
049000100525     C***                EVAL      I95CAP = VABCAD
049100100525     C***                EVAL      I95LOC = VABLOD
049200100525     C***                CALL      'TISI95R'
049300100525     C***                PARM                    TISI95DS
049400100525     C***                EVAL      VABPRD = O95PRV
049500100525     C***                ENDIF
049600070730     C*
049700070730     C* Considerazioni finali su CBO/CAS
049800100525     C***                IF        FlgCAS = '1'
049900100525     C***                IF        VABCBO = '1'
050000100525     C***                EVAL      VABCBO = '4'
050100100525     C***                ENDIF
050200100525     C***                IF        VABCBO = '2'
050300100525     C***                EVAL      VABCBO = '6'
050400100525     C***                ENDIF
050500100525     C***                ENDIF
050600010202     C*
050700000801     C* Ebbene...
050800000801     C                   ADD       1             §CTRMO
050900070530     C                   IF        *in31 <> *zeros OR
051000070530     C                             *in32 <> *zeros
051100000801     C                   ADD       1             §CTRNO
051200020725     C                   EVAL      x_vinflg = '2'
051300000801     C                   ELSE
051400010201     C                   ADD       1             §CTROKVB
051500000801     C                   ENDIF
051600000801     C*
051700000801     C                   ENDSR
051800100127     C*----------------------------------------------------*
051900100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
052000100127     C*----------------------------------------------------*
052100100127     C     WRIVAT_A      BEGSR
052200100127     C*
052300100127     C* Valorizzo l buffer di scrittura del FIVAT
052400100127     C* - TIPO RECORD "A"
052500100412     C***                eval      VATTRC = 'A'
052600100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
052700100412     C***                exsr      wrivat
052800100127     C*
052900100127     C                   ENDSR
053000010201     C*----------------------------------------------------*
053100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053200010201     C*----------------------------------------------------*
053300071003     C     WRIVAT_B      BEGSR
053400010201     C*
053500021113     C* Valorizzo l buffer di scrittura del FIVAT
053600070928     C* - TIPO RECORD "B"
053700100412     C***                eval      VATTRC = 'B'
053800100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
053900100412     C***                exsr      wrivat
054000010201     C*
054100010201     C                   ENDSR
054200071003     C*----------------------------------------------------*
054300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054400071003     C*----------------------------------------------------*
054500071003     C     WRIVAT_E      BEGSR
054600071003     C*
054700071003     C* Valorizzo l buffer di scrittura del FIVAT
054800071003     C* - TIPO RECORD "E"
054900100310     C                   eval      VATTRC = 'E'
055000110215     C                   eval      VATNOT = %trim(%subst(vindta:14:14))
055100100525     C                   exsr      wrivat
055200071003     C*
055300071003     C                   ENDSR
055400100127     C*----------------------------------------------------*
055500100127     C*  SCARICO BUFFER FIVAT
055600100127     C*----------------------------------------------------*
055700100127     C     WRIVAT        BEGSR
055800100127     C*
055900100127     C                   if        vatNOT <> *blanks
056000100127     C                   ADD       1             §CTROKVT
056100100127     C                   write     FIVAT000
056200100127     C                   endif
056300100127     C*
056400100127     C                   ENDSR
056500020904
056600020904
056700010202     C*----------------------------------------------------*
056800100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
056900010202     C*----------------------------------------------------*
057000100525     C     PREVASX       BEGSR
057100100525     C*
057200021113     C* Compongo il nome del membro da dare al FIVATWWR
057300010202     C                   eval      parmbr = vlrhdl
057400010202     C                   movel     'M'           parmbr
057500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057600010202     C                   eval      paropz = '1'
057700100525     C*
057800010202     C* Effettuo la chiamata al CLLE preposto
057900100525     C  N51              call(e)   'TITVVTC'
058000010202     C                   parm                    parccm
058100010202     C                   parm                    parmbr
058200010202     C                   parm                    paropz
058300100525     C   51              call(e)   'TITVVBC'
058400100525     C                   parm                    parccm
058500100525     C                   parm                    parmbr
058600100525     C                   parm                    paropz
058700100525     C*
058800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058900010202     C                   if        %error
059000010202     C                   movel     '1'           chkcall
059100010202     C                   else
059200010202     C                   movel     '0'           chkcall
059300010202     C                   endif
059400010202     C*
059500010202     C                   ENDSR
059600000801     C*----------------------------------------------------*
059700000801     C*  CONTROLLO NUMERICITA' CAMPI
059800000801     C*----------------------------------------------------*
059900000801     C     CHKNUM        BEGSR
060000081113     C*
060100081113     C                   IF        PiDecChr = *blanks
060200100503     C                   EVAL      PiDecChr = '.'
060300081113     C                   ENDIF
060400091223     C*
060500081113     C                   callp(e)  UBISNUM_Check(PiStr
060600081113     C                                          :PiDecChr
060700081113     C                                          :PiVal
060800081113     C                                          :PiNum
060900081113     C                                          :PiInt)
061000081113     C*
061100000801     C                   IF        %error
061200000801     C                   EVAL      PiInt=*off
061300000801     C                   ENDIF
061400000801     C*
061500000801     C                   ENDSR
061600000801     C***
061700011113
061800011113
061900000801
062000000801
062100990920      /TITLE Invio dei dati al punto operativo.
062200010202     C     invio         BEGSR
062300990920     C*
062400021113     C* 1° invio FIVAT
062500010201     C                   reset                   dscmz
062600010201     C                   move      vlrpoi        cmzdst
062700021113     C                   eval      cmzfld = 'FIVATWWR'
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021009     C***                if        prmfir = *blanks
063100021113     C                   eval      cmzfla = 'FIVAT00F'
063200021113     C                   eval      cmzmba = 'FIVAT00F'
063300021009     C***                else
063400021009     C***                eval      cmzfla = prmfir
063500021009     C***                eval      cmzmba = prmfir
063600021009     C***                endif
063700010201     C                   eval      cmznrr = *zeros
063800020305     C                   move      §ctrokvt      cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000010201     C                   call(e)   'TIS711C'
064100010201     C                   parm                    dscmz
064200010201     C                   parm      *blanks       esito
064300010205     C                   if        %error
064400010205     C                             or cmzerr = '1'
064500010205     C                             or esito  = '1'
064600010205     C                   eval      wrkesito = '3'
064700010205     C                   else
064800010201     C*
064900021113     C* 2° invio FIVAB
065000100525     C                   if        *in51 = *off
065100010201     C                   reset                   dscmz
065200010201     C                   move      vlrpoi        cmzdst
065300010201     C                   eval      cmzfld = vlrfou
065400010201     C                   eval      cmzmbd = vlrhdl
065500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065600021009     C***                if        prmfir = *blanks
065700021113     C                   eval      cmzfla = 'FIVAB00F'
065800021113     C                   eval      cmzmba = 'FIVAB00F'
065900021009     C***                else
066000021009     C***                eval      cmzfla = prmfir
066100021009     C***                eval      cmzmba = prmfir
066200021009     C***                endif
066300010201     C                   eval      cmznrr = *zeros
066400010201     C                   move      §ctrokvb      cmznrr
066500021018     C                   eval      cmzlba = vlrfl1
066600010201     C                   call(e)   'TIS711C'
066700010201     C                   parm                    dscmz
066800010201     C                   parm      *blanks       esito
066900010201     C                   if        %error
067000010201     C                             or cmzerr = '1'
067100010201     C                             or esito  = '1'
067200010201     C                   eval      wrkesito = '3'
067300010201     C                   endif
067400100525     C                   endif
067500010205     C                   endif
067600990920     C*
067700000613     C                   ENDSR
067800000613     C***
067900070411
068000070411     C     *pssr         BEGSR
068100070411     C*
068200070411     C                   if        %open(tivin00r)
068300070411     C                   close     tivin00r
068400070411     C                   endif
068500070411     C                   if        %open(fivabwwr)
068600070411     C                   close     fivabwwr
068700070411     C                   endif
068800070411     C                   if        %open(fivatwwr)
068900070411     C                   close     fivatwwr
069000070411     C                   endif
069100070411     C*
069200070411     C* Effettuo la chiamata al CLLE preposto
069300100525     C  N51              call(e)   'TITVVTC'
069400100525     C                   parm                    parccm
069500100525     C                   parm                    parmbr
069600100525     C                   parm      '2'           paropz
069700100525     C   51              call(e)   'TITVVBC'
069800100525     C                   parm                    parccm
069900100525     C                   parm                    parmbr
070000100525     C                   parm      '2'           paropz
070100070411     C*
070200070411     C                   eval      wrkesito = '2'
070300070411     C*
070400070411     C                   seton                                        LR
070500070411     C*
070600070411     C                   ENDSR     '*CANCL'
070700070411     C***
070800070411
070900990910
071000000613     C     *inzsr        BEGSR
071100990910     C*
071200990910     C     *entry        plist
071300990920     C                   parm                    tivlrds
071400990921     C                   parm      wrkesito      esito
071500000724     C                   parm                    prmlit
071600000710     C                   parm                    prmfir
071700000613     C*
071800000830     C* CALCOLA LA DATA CORRENTE
071900091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072000091223     C                   eval      datcor = %dec(%date() : *ISO)
072100000830     C*
072200000613     C                   ENDSR
072300000613     C***
