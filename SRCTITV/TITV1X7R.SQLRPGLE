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
002100100520     D depspe          s             20    INZ(*blanks)
002200100520     D curspe          s             20    INZ(*blanks)
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
007000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007100000613     C*
007200010202     C* Effettuo la chiamata al CLLE preposto
007300050128     C                   call(e)   'TITVVTC'
007400010202     C                   parm                    parccm
007500010202     C                   parm                    parmbr
007600010202     C                   parm      '2'           paropz
007700070730     C*
007800070730     C* Effettuo lancio TISI95 solo x chiusura
007900070730     C                   CLEAR                   TISI95DS
008000070730     C                   EVAL      I95TLA = 'C'
008100070730     C                   CALL      'TISI95R'
008200070730     C                   PARM                    TISI95DS
008300000616     C*
008400000801     C
008500010201     C                   seton                                        LR
008600990908
008700000801
008800910830     C*--------------------------------------------------------
008900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009000910830     C*--------------------------------------------------------
009100070530     C     RWFILE        BEGSR
009200990910     C*
009300990914     C                   if        not %open(tivin00r)
009400990908     C                   open      tivin00r
009500990914     C                   endif
009600021113     C                   if        not %open(fivabwwr)
009700021113     C                   open      fivabwwr
009800990914     C                   endif
009900070530     C*
010000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010100020305     C                   exsr      prevat
010200010201     C*
010300010202     C                   if        chkcall = '0'
010400010202     C*
010500021113     C                   if        not %open(fivatwwr)
010600021113     C                   open      fivatwwr
010700010201     C                   endif
010800080117     C*
010900080117     C                   EXSR      INZVAR
011000080117     C                   EXSR      DEFCAM
011100990910     C*
011200010201     C                   clear                   §CTROKVB          5 0
011300020305     C                   clear                   §CTROKVT          5 0
011400000801     C                   clear                   §CTRMO            5 0
011500000801     C                   clear                   §CTRNO            5 0
011600990910     C*
011700020725     C/EXEC SQL
011800020725     C+ declare C1 cursor for select
011900100520     C+ vindta, vinflg, substr(vindta, 11, 20) as sped, rrn(tivin00r)
012000060223     C+ from tivin00r
012100060223     C+ order by sped
012200020725     C+ for read only
012300020725     C/END-EXEC
012400020725     C
012500020725     C/EXEC SQL
012600020725     C+ open C1
012700020725     C/END-EXEC
012800020725     C
012900020725     C/EXEC SQL
013000070530     C+ Fetch C1 into :INPUT_DS
013100020725     C/END-EXEC
013200020725     C*
013300020725     C                   dow       sqlcod = *zeros
013400990913     C*
013500100310     C                   if        vindta > *blanks
013600000613     C                   add       1             rrnum
013700000801     C*
013800020725     C                   if        vinflg = *blanks
013900020725     C                             or vinflg = '0'
014000020725     C                             or vinflg = '2'
014100000801     C*
014200020725     C                   clear                   x_vinmsg
014300020725     C                   eval      x_vinflg = '1'
014400010305     C*
014500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014600100520     C                   EVAL      curspe=%trim(%subst(vindta:11:20))
014700010305     C*
014800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
014900080923     C                   if        *in50 = *off
015000091223     C                   exsr      repNSP
015100071003     C                   exsr      impvab
015200100412     C                   exsr      wrivab
015300100127     C                   exsr      wrivat_a                                     => carico VAT
015400071003     C                   exsr      wrivat_b                                     => carico VAT
015500071003     C                   else
015600080923     C*
015700071009     C                   if        wDISK = 'D'
015800091223     C                   exsr      repNSP
015900071009     C                   exsr      impvab
016000100412     C                   exsr      wrivab
016100100127     C                   exsr      wrivat_a                                     => carico VAT
016200071009     C                   exsr      wrivat_b                                     => carico VAT
016300071009     C                   exsr      wrivat_e                                     => carico VAT
016400071009     C                   else
016500071009     C*
016600010305     C                   if        depspe = *blanks                             => 1° giro
016700010305     C                   eval      depspe = curspe                              => memorizz. spediz
016800080117     C                   clear                   tivinds
016900091223     C                   exsr      repNSP
017000020305     C                   exsr      impvab
017100100127     C                   exsr      wrivat_a                                     => carico VAT
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300071003     C   50              exsr      wrivat_e                                     => carico VAT
017400010305     C                   else
017500020725     C                   if        depspe <> curspe                             => rottura di spediz
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700100412     C                   exsr      wrivab
017800080117     C                   clear                   tivinds
017900091223     C                   exsr      repNSP
018000020305     C                   exsr      impvab
018100100127     C                   exsr      wrivat_a                                     => carico VAT
018200071003     C                   exsr      wrivat_b                                     => carico VAT
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400020305     C                   else                                                   => x stessa spediz
018500100401     C*
018600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
018700100412     C                   if        wDUPREC <> 'N'
018800020305     C                   exsr      impvab
018900100127     C                   exsr      wrivat_a                                     => carico VAT
019000071003     C                   exsr      wrivat_b                                     => carico VAT
019100071003     C   50              exsr      wrivat_e                                     => carico VAT
019200100401     C                   endif
019300010305     C                   endif
019400010305     C                   endif
019500010305     C                   endif
019600071003     C                   endif
019700071009     C                   endif
019800000905     C*
019900000905     C                   else
020000020725     C                   eval      x_vinflg = '1'
020100000905     C                   endif
020200000905     C*
020300020725     C     VINRRN        chain     tivin000
020400020725     C                   if        %found(tivin00r)
020500020725     C                   eval      y_vinflg = x_vinflg
020600020725     C                   eval      y_vinmsg = x_vinmsg
020700020725     C                   update    tivin000
020800020725     C                   endif
020900020725     C*
021000020725     C/EXEC SQL
021100070530     C+ Fetch C1 into :INPUT_DS
021200020725     C/END-EXEC
021300020725     C*
021400020725     C                   enddo
021500020725     C*
021600020725     C/EXEC SQL
021700020725     C+ close C1
021800020725     C/END-EXEC
021900000905     C*
022000020305     C* Scarico i VAB rimasti "in sospeso"
022100071009     C                   if        wDISK = 'C'
022200100412     C                   exsr      wrivab
022300071009     C                   endif
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200021113     C                   if        %open(fivabwwr)
024300021113     C                   close     fivabwwr
024400990914     C                   endif
024500021113     C                   if        %open(fivatwwr)
024600021113     C                   close     fivatwwr
024700010201     C                   endif
024800990910     C*
024900010201     C                   if        §ctrokvb > 0
025000000724     C                             and vlrpoi <> *zeros
025100010202     C                   exsr      invio
025200990920     C                   endif
025300990920     C*
025400910830     C                   ENDSR
025500000613     C***
025600100412
025700100412
025800010305
025900010305     C*----------------------------------------------------*
026000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026100010305     C*----------------------------------------------------*
026200020305     C     WRIVAB        BEGSR
026300100412     C*
026400100412     C* Gestisco l'eventuale rottura x numero spedizione
026500100412     C                   if        VABRMA = *blanks
026600100412     C                   movel(P)  VABRMN        VABRMA
026700100412     C                   endif
026800100412     C*
026900100412     C                   write     fivab000                                     => scarico il VAB
027000010305     C*
027100010305     C                   ENDSR
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800010201     C                   Z-ADD     *zeros        Num5_0
027900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028000020725     C                   MOVEL     '0'           FlgCAS            1
028100000801     C*
028200000801     C                   ENDSR
028300000801     C*----------------------------------------------------*
028400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028500000801     C*----------------------------------------------------*
028600020904     C     DEFCAM        BEGSR
028700080923     C*
028800090310     C                   SETOFF                                       50
028900000801     C*
029000021113     C                   CLEAR                   FIVAB000
029100021113     C                   CLEAR                   FIVAT000
029200070730     C* Imposto i valori di default...
029300100520     C                   EVAL      VABCCM = 2022562
029400100520     C                   EVAL      VATCCM = 2022562
029500100520     C                   EVAL      VABLNP = 202
029600100520     C                   EVAL      VATLNP = 202
029700100503     C                   EVAL      VABCTR = 000
029800070730     C                   EVAL      VABCBO = '1'
029900070222     C* ... e poi verifico se sono stati passati come parametri
030000070222     C                   IF        vlrppt > *blanks
030100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030300070222     C                   EXSR      CHKNUM
030400070222     C                   IF        PiInt=*on
030500070222     C                   Z-ADD     PiVal         VABCCM
030600070222     C                   Z-ADD     PiVal         VATCCM
030700070222     C                   ENDIF
030800070222     C                   ENDIF
030900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031100070222     C                   EXSR      CHKNUM
031200070222     C                   IF        PiInt=*on
031300070222     C                   Z-ADD     PiVal         VABLNP
031400070222     C                   Z-ADD     PiVal         VATLNP
031500070222     C                   ENDIF
031600070222     C                   ENDIF
031700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031900070222     C                   EXSR      CHKNUM
032000070222     C                   IF        PiInt=*on
032100070222     C                   Z-ADD     PiVal         VABCTR
032200070222     C                   ENDIF
032300070928     C                   ENDIF
032400071009     C                   MOVEL     *blanks       wDISK             1
032500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032600071009     C                   IF        wDISK <> *blanks
032700070928     C                   SETON                                        50
032800070222     C                   ENDIF
032900100401     C                   MOVEL     *blanks       wDUPREC           1
033000100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
033100070222     C                   ENDIF
033200071009     C*
033300071009     C   50              EVAL      VABCTM = '7Q'
033400000801     C*
033500000801     C                   ENDSR
033600091223     C*----------------------------------------------------*
033700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033800091223     C*----------------------------------------------------*
033900091223     C     REPNSP        BEGSR
034000091223     C*
034100091223     C                   EXSR      INZVAR
034200091223     C                   EXSR      DEFCAM
034300091223     C*
034400091223     C* NSP => Stacco un numeratore da AZNUM
034500091223     C                   clear                   TRUL33DS
034600091223     C                   eval      I33OPE = *zeros
034700091223     C                   eval      I33CNU = 302
034800091223     C                   eval      I33NUM = 1
034900091223     C                   movel     TRUL33DS      KPJBU
035000091223     C                   call      'TRUL33R'
035100091223     C                   parm                    KPJBA
035200091223     C                   movel     KPJBU         TRUL33DS
035300091223     C                   if        O33ERR = *zeros
035400091223     C                   z-add     O33NRF        VABNSP
035500091223     C                   z-add     O33NRF        VATNSP
035600091223     C                   else
035700091223     C                   SETON                                        31
035800091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035900091223     C                             + ' ' + 'VABNSP VATNSP'
036000091223     C                   endif
036100091223     C*
036200091223     C                   ENDSR
036300000801     C*----------------------------------------------------*
036400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036500000801     C*----------------------------------------------------*
036600010201     C     IMPVAB        BEGSR
036700070530     C*
036800070530     C                   SETOFF                                       3132
036900070928     C*
037000070928     C                   MOVE(P)   vlrpoi        VABFGS
037100070928     C                   MOVE(P)   vlrpoi        VATFGS
037200070928     C*
037300070928     C                   MOVEL     datcor        VABAAS
037400070928     C                   MOVEL     datcor        VATAAS
037500070928     C                   MOVE      datcor        VABMGS
037600100520     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
037700100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037800100503     C     '@':'A'       XLATE     VABRSD        VABRSD
037900100503     C* ==
038000100520     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
038100100520     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
038200100520     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
038300100521     C                   EVAL      VABRMA=%trim(%subst(vindta:644:20))
038400100520     C                   EVAL      VABNOT=%trim(%subst(vindta:574:35))
038500100520     C                   EVAL      VABNT2=%trim(%subst(vindta:609:35))
038600100520     C                   EVAL      VABCBO=%trim(%subst(vindta:2:1))
038700100520     C*
038800100520     C* Particolarità
038900100520     C                   select
039000100520     C* DCR - "IL"
039100100520     C                   when      %subst(vindta:490:1) = '1'
039200100520     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
039300100520     C                   EXSR      CHKNUM
039400100520     C                   IF        PiInt=*on
039500100520     C                   Z-ADD     PiVal         VABDCR
039600100520     C                   ELSE
039700100520     C                   SETON                                        32
039800100520     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039900100520     C                             + ' ' + 'VABDCR'
040000100520     C                   ENDIF
040100100520     C* DCR - "PRIMA DEL"
040200100520     C                   when      %subst(vindta:490:1) = '3'
040300100520     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
040400100520     C                   EXSR      CHKNUM
040500100520     C                   IF        PiInt=*on
040600100520     C                   Z-ADD     PiVal         VABDCR
040700100520     C                   ELSE
040800100520     C                   SETON                                        32
040900100520     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000100520     C                             + ' ' + 'VABDCR'
041100100520     C                   ENDIF
041200100520     C                   EVAL      VABTCR = 'P'
041300100520     C* DCR - "DOPO IL"
041400100520     C                   when      %subst(vindta:490:1) = '4'
041500100520     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
041600100520     C                   EXSR      CHKNUM
041700100520     C                   IF        PiInt=*on
041800100520     C                   Z-ADD     PiVal         VABDCR
041900100520     C                   ELSE
042000100520     C                   SETON                                        32
042100100520     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200100520     C                             + ' ' + 'VABDCR'
042300100520     C                   ENDIF
042400100520     C                   EVAL      VABTCR = 'D'
042500100520     C* TC1 - "APPUNTAMENTO"
042600100520     C                   when      %subst(vindta:490:1) = '2'
042700100520     C                   EVAL      VABTC1 = 'A'
042800100520     C                   endsl
042900100520     C*
043000100520     C* Tipo incasso contrassegno
043100100520     C                   select
043200100520     C                   when      %subst(vindta:401:1) = 'A'
043300100520     C                   EVAL      VABTIC = 'BM'
043400100520     C                   when      %subst(vindta:401:1) = 'B'
043500100520     C                   EVAL      VABTIC = 'CM'
043600100520     C                   endsl
043700100503     C* CAD
043800100520     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
043900100503     C                   EXSR      CHKNUM
044000100503     C                   IF        PiInt=*on
044100100503     C                   Z-ADD     PiVal         Num5_0
044200100503     C                   MOVEL     Num5_0        VABCAD
044300100503     C                   ELSE
044400100503     C                   SETON                                        32
044500100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600100503     C                             + ' ' + 'VABCAD'
044700100503     C                   ENDIF
044800100521     C* RMN
044900100521     C                   EVAL      PiStr=%trim(%subst(vindta:11:20))
045000100503     C                   EXSR      CHKNUM
045100100503     C                   IF        PiInt=*on
045200100503     C                   Z-ADD     PiVal         VABRMN
045300100503     C                   ELSE
045400100503     C                   SETON                                        32
045500100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600100503     C                             + ' ' + 'VABRMN'
045700100503     C                   ENDIF
045800100503     C* NCL
045900100520     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
046000100503     C                   EXSR      CHKNUM
046100100503     C                   IF        PiInt=*on
046200100503     C                   Z-ADD     PiVal         VABNCL
046300100503     C                   ELSE
046400100503     C                   SETON                                        32
046500100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046600100503     C                             + ' ' + 'VABNCL'
046700100503     C                   ENDIF
046800100503     C* PKB
046900100520     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
047000100503     C                   EXSR      CHKNUM
047100100503     C                   IF        PiNum=*on
047200100520     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
047300100503     C                   Z-ADD     PiVal         VABPKB
047400100503     C                   ELSE
047500100503     C                   SETON                                        32
047600100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700100503     C                             + ' ' + 'VABPKB'
047800100503     C                   ENDIF
047900100526     C* VLB
048000100526     C                   EVAL      PiStr=%trim(%subst(vindta:369:5))
048100100526     C                   EXSR      CHKNUM
048200100526     C                   IF        PiNum=*on
048300100702     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
048400100526     C                   Z-ADD     PiVal         VABVLB
048500100526     C                   ELSE
048600100526     C                   SETON                                        32
048700100526     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048800100526     C                             + ' ' + 'VABVLB'
048900100526     C                   ENDIF
049000100503     C* CAS
049100100520     C                   IF        %trim(%subst(vindta:388:13))<>*blanks
049200100503     C                             AND
049300100520     C                             %trim(%subst(vindta:388:13))<>'0000000000000'
049400100520     C                             AND
049500100520     C                             %trim(%subst(vindta:388:12))<>'0'
049600100503     C                   MOVEL     '1'           FlgCAS
049700100525     C                   EVAL      VABVCA='EUR'
049800100520     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
049900100503     C                   EXSR      CHKNUM
050000100503     C                   IF        PiNum=*on
050100100520     C                   EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
050200100503     C                   Z-ADD     PiVal         VABCAS
050300100503     C                   ELSE
050400100503     C                   SETON                                        32
050500100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050600100503     C                             + ' ' + 'VABCAS'
050700100503     C                   ENDIF
050800100503     C                   ENDIF
050900100310     C*
051000100310     C* Se provincia nn valorizzata la reperisco
051100100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
051200100310     C                   IF        VABNZD = *blanks AND
051300100310     C                             VABPRD = *blanks
051400100310     C                   CLEAR                   TISI95DS
051500100310     C                   EVAL      I95TCN = '3'
051600100310     C                   Z-ADD     datcor        I95DAT
051700100310     C                   EVAL      I95NAR = VABNZD
051800100310     C                   EVAL      I95CAP = VABCAD
051900100310     C                   EVAL      I95LOC = VABLOD
052000100310     C                   CALL      'TISI95R'
052100100310     C                   PARM                    TISI95DS
052200100310     C                   EVAL      VABPRD = O95PRV
052300100310     C                   ENDIF
052400070730     C*
052500070730     C* Considerazioni finali su CBO/CAS
052600070730     C                   IF        FlgCAS = '1'
052700070730     C                   IF        VABCBO = '1'
052800070730     C                   EVAL      VABCBO = '4'
052900070730     C                   ENDIF
053000070730     C                   IF        VABCBO = '2'
053100070730     C                   EVAL      VABCBO = '6'
053200070730     C                   ENDIF
053300070730     C                   ENDIF
053400010202     C*
053500000801     C* Ebbene...
053600000801     C                   ADD       1             §CTRMO
053700070530     C                   IF        *in31 <> *zeros OR
053800070530     C                             *in32 <> *zeros
053900000801     C                   ADD       1             §CTRNO
054000020725     C                   EVAL      x_vinflg = '2'
054100000801     C                   ELSE
054200010201     C                   ADD       1             §CTROKVB
054300000801     C                   ENDIF
054400000801     C*
054500000801     C                   ENDSR
054600100127     C*----------------------------------------------------*
054700100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
054800100127     C*----------------------------------------------------*
054900100127     C     WRIVAT_A      BEGSR
055000100127     C*
055100100127     C* Valorizzo l buffer di scrittura del FIVAT
055200100127     C* - TIPO RECORD "A"
055300100412     C***                eval      VATTRC = 'A'
055400100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
055500100412     C***                exsr      wrivat
055600100127     C*
055700100127     C                   ENDSR
055800010201     C*----------------------------------------------------*
055900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056000010201     C*----------------------------------------------------*
056100071003     C     WRIVAT_B      BEGSR
056200010201     C*
056300021113     C* Valorizzo l buffer di scrittura del FIVAT
056400070928     C* - TIPO RECORD "B"
056500100412     C***                eval      VATTRC = 'B'
056600100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
056700100412     C***                exsr      wrivat
056800010201     C*
056900010201     C                   ENDSR
057000071003     C*----------------------------------------------------*
057100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057200071003     C*----------------------------------------------------*
057300071003     C     WRIVAT_E      BEGSR
057400071003     C*
057500071003     C* Valorizzo l buffer di scrittura del FIVAT
057600071003     C* - TIPO RECORD "E"
057700100310     C                   eval      VATTRC = 'E'
057800100419     C*
057900100419     C* X ogni collo indicato in testata
058000100419     C                   z-add     1             wColli            5 0
058100100419     C                   dow       wColli <= VABNCL
058200100419     C*
058300100419     C* Valorizzo il buffer di scrittura del FIVAT
058400100419     C                   move(p)   wColli        wColliAlfa        2
058500100503     C***                eval      VATNOT = %trim(%subst(vindta:331:26))+
058600100503     C***                                   wColliAlfa
058700100419     C                   exsr      wrivat                                       => scarico VAT
058800100419     C*
058900100419     C                   add       1             wColli
059000100419     C                   enddo
059100100419     C*
059200071003     C*
059300071003     C                   ENDSR
059400100127     C*----------------------------------------------------*
059500100127     C*  SCARICO BUFFER FIVAT
059600100127     C*----------------------------------------------------*
059700100127     C     WRIVAT        BEGSR
059800100127     C*
059900100127     C                   if        vatNOT <> *blanks
060000100127     C                   ADD       1             §CTROKVT
060100100127     C                   write     FIVAT000
060200100127     C                   endif
060300100127     C*
060400100127     C                   ENDSR
060500020904
060600020904
060700010202     C*----------------------------------------------------*
060800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060900010202     C*----------------------------------------------------*
061000020305     C     PREVAT        BEGSR
061100010202     C*
061200021113     C* Compongo il nome del membro da dare al FIVATWWR
061300010202     C                   eval      parmbr = vlrhdl
061400010202     C                   movel     'M'           parmbr
061500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061600010202     C                   eval      paropz = '1'
061700010202     C* Effettuo la chiamata al CLLE preposto
061800050128     C                   call(e)   'TITVVTC'
061900010202     C                   parm                    parccm
062000010202     C                   parm                    parmbr
062100010202     C                   parm                    paropz
062200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062300010202     C                   if        %error
062400010202     C                   movel     '1'           chkcall
062500010202     C                   else
062600010202     C                   movel     '0'           chkcall
062700010202     C                   endif
062800010202     C*
062900010202     C                   ENDSR
063000000801     C*----------------------------------------------------*
063100000801     C*  CONTROLLO NUMERICITA' CAMPI
063200000801     C*----------------------------------------------------*
063300000801     C     CHKNUM        BEGSR
063400081113     C*
063500081113     C                   IF        PiDecChr = *blanks
063600100503     C                   EVAL      PiDecChr = '.'
063700081113     C                   ENDIF
063800091223     C*
063900081113     C                   callp(e)  UBISNUM_Check(PiStr
064000081113     C                                          :PiDecChr
064100081113     C                                          :PiVal
064200081113     C                                          :PiNum
064300081113     C                                          :PiInt)
064400081113     C*
064500000801     C                   IF        %error
064600000801     C                   EVAL      PiInt=*off
064700000801     C                   ENDIF
064800000801     C*
064900000801     C                   ENDSR
065000000801     C***
065100011113
065200011113
065300000801
065400000801
065500990920      /TITLE Invio dei dati al punto operativo.
065600010202     C     invio         BEGSR
065700990920     C*
065800021113     C* 1° invio FIVAT
065900010201     C                   reset                   dscmz
066000010201     C                   move      vlrpoi        cmzdst
066100021113     C                   eval      cmzfld = 'FIVATWWR'
066200010201     C                   eval      cmzmbd = vlrhdl
066300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066400021009     C***                if        prmfir = *blanks
066500021113     C                   eval      cmzfla = 'FIVAT00F'
066600021113     C                   eval      cmzmba = 'FIVAT00F'
066700021009     C***                else
066800021009     C***                eval      cmzfla = prmfir
066900021009     C***                eval      cmzmba = prmfir
067000021009     C***                endif
067100010201     C                   eval      cmznrr = *zeros
067200020305     C                   move      §ctrokvt      cmznrr
067300021018     C                   eval      cmzlba = vlrfl1
067400010201     C                   call(e)   'TIS711C'
067500010201     C                   parm                    dscmz
067600010201     C                   parm      *blanks       esito
067700010205     C                   if        %error
067800010205     C                             or cmzerr = '1'
067900010205     C                             or esito  = '1'
068000010205     C                   eval      wrkesito = '3'
068100010205     C                   else
068200010201     C*
068300021113     C* 2° invio FIVAB
068400010201     C                   reset                   dscmz
068500010201     C                   move      vlrpoi        cmzdst
068600010201     C                   eval      cmzfld = vlrfou
068700010201     C                   eval      cmzmbd = vlrhdl
068800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068900021009     C***                if        prmfir = *blanks
069000021113     C                   eval      cmzfla = 'FIVAB00F'
069100021113     C                   eval      cmzmba = 'FIVAB00F'
069200021009     C***                else
069300021009     C***                eval      cmzfla = prmfir
069400021009     C***                eval      cmzmba = prmfir
069500021009     C***                endif
069600010201     C                   eval      cmznrr = *zeros
069700010201     C                   move      §ctrokvb      cmznrr
069800021018     C                   eval      cmzlba = vlrfl1
069900010201     C                   call(e)   'TIS711C'
070000010201     C                   parm                    dscmz
070100010201     C                   parm      *blanks       esito
070200010201     C                   if        %error
070300010201     C                             or cmzerr = '1'
070400010201     C                             or esito  = '1'
070500010201     C                   eval      wrkesito = '3'
070600010201     C                   endif
070700010205     C                   endif
070800990920     C*
070900000613     C                   ENDSR
071000000613     C***
071100070411
071200070411     C     *pssr         BEGSR
071300070411     C*
071400070411     C                   if        %open(tivin00r)
071500070411     C                   close     tivin00r
071600070411     C                   endif
071700070411     C                   if        %open(fivabwwr)
071800070411     C                   close     fivabwwr
071900070411     C                   endif
072000070411     C                   if        %open(fivatwwr)
072100070411     C                   close     fivatwwr
072200070411     C                   endif
072300070411     C*
072400070411     C* Effettuo la chiamata al CLLE preposto
072500070411     C                   call(e)   'TITVVTC'
072600070411     C                   parm                    parccm
072700070411     C                   parm                    parmbr
072800070411     C                   parm      '2'           paropz
072900070411     C*
073000070411     C                   eval      wrkesito = '2'
073100070411     C*
073200070411     C                   seton                                        LR
073300070411     C*
073400070411     C                   ENDSR     '*CANCL'
073500070411     C***
073600070411
073700990910
073800000613     C     *inzsr        BEGSR
073900990910     C*
074000990910     C     *entry        plist
074100990920     C                   parm                    tivlrds
074200990921     C                   parm      wrkesito      esito
074300000724     C                   parm                    prmlit
074400000710     C                   parm                    prmfir
074500000613     C*
074600000830     C* CALCOLA LA DATA CORRENTE
074700091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074800091223     C                   eval      datcor = %dec(%date() : *ISO)
074900000830     C*
075000000613     C                   ENDSR
075100000613     C***
