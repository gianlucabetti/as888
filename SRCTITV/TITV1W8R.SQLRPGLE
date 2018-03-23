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
002100110110     D depspe          s              8    INZ(*blanks)
002200110110     D curspe          s              8    INZ(*blanks)
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
011900100419     C+ vindta, vinflg, substr(vindta, 81, 6) as sped, rrn(tivin00r)
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
014600110110     C                   EVAL      curspe=%trim(%subst(vindta:80:8))
014700010305     C*
014800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
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
029300100419     C                   EVAL      VABCCM = 0492495
029400100419     C                   EVAL      VATCCM = 0492495
029500100419     C                   EVAL      VABLNP = 049
029600100419     C                   EVAL      VATLNP = 049
029700100419     C                   EVAL      VABCTR = 300
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
037600100419     C                   EVAL      VABRSD=%trim(%subst(vindta:155:35))
037700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
037900070928     C* ==
038000100419     C                   EVAL      VABIND=%trim(%subst(vindta:225:35))
038100100419     C                   EVAL      VABLOD=%trim(%subst(vindta:260:35))
038200100419     C                   EVAL      VABCAD=%trim(%subst(vindta:315:7))
038300100419     C                   EVAL      VABPRD=%trim(%subst(vindta:322:2))
038400110110     C                   EVAL      VABRMA=%trim(%subst(vindta:80:8))
038500100419     C                   EVAL      VABCBO=%trim(%subst(vindta:12:1))
038600100419     C                   EVAL      VABNOT=%trim(%subst(vindta:461:35))
038700100419     C                   EVAL      VABNT2=%trim(%subst(vindta:461+35:60-35))
038800100419     C* CCM
038900100419     C                   IF        %trim(%subst(vindta:2:7)) = '2100341'
039000100419     C                   EVAL      VABCCM = 2100341
039100100419     C                   EVAL      VATCCM = 2100341
039200100419     C                   EVAL      VABCTR = 000
039300100419     C                   ENDIF
039400100309     C* NCL
039500100419     C                   EVAL      PiStr=%trim(%subst(vindta:353:5))
039600100309     C                   EXSR      CHKNUM
039700100309     C                   IF        PiInt=*on
039800100309     C                   Z-ADD     PiVal         VABNCL
039900100309     C                   ELSE
040000100309     C                   SETON                                        32
040100100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040200100309     C                             + ' ' + 'VABNCL'
040300100309     C                   ENDIF
040400100309     C* PKB
040500100419     C                   EVAL      PiStr=%trim(%subst(vindta:358:7))
040600100309     C                   EXSR      CHKNUM
040700100309     C                   IF        PiNum=*on
040800100510     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
040900100412     C                   ADD(H)    PiVal         VABPKB
041000100309     C                   ELSE
041100100309     C                   SETON                                        32
041200100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300100309     C                             + ' ' + 'VABPKB'
041400100309     C                   ENDIF
041500100412     C* RMN
041600110110     C                   EVAL      PiStr=%trim(%subst(vindta:80:8))
041700080923     C                   EXSR      CHKNUM
041800080923     C                   IF        PiInt=*on
041900080923     C                   Z-ADD     PiVal         VABRMN
042000080923     C                   ELSE
042100080923     C                   SETON                                        32
042200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042300080923     C                             + ' ' + 'VABRMN'
042400080923     C                   ENDIF
042500100310     C*
042600100310     C* Se provincia nn valorizzata la reperisco
042700100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
042800100310     C                   IF        VABNZD = *blanks AND
042900100310     C                             VABPRD = *blanks
043000100310     C                   CLEAR                   TISI95DS
043100100310     C                   EVAL      I95TCN = '3'
043200100310     C                   Z-ADD     datcor        I95DAT
043300100310     C                   EVAL      I95NAR = VABNZD
043400100310     C                   EVAL      I95CAP = VABCAD
043500100310     C                   EVAL      I95LOC = VABLOD
043600100310     C                   CALL      'TISI95R'
043700100310     C                   PARM                    TISI95DS
043800100310     C                   EVAL      VABPRD = O95PRV
043900100310     C                   ENDIF
044000070730     C*
044100070730     C* Considerazioni finali su CBO/CAS
044200070730     C                   IF        FlgCAS = '1'
044300070730     C                   IF        VABCBO = '1'
044400070730     C                   EVAL      VABCBO = '4'
044500070730     C                   ENDIF
044600070730     C                   IF        VABCBO = '2'
044700070730     C                   EVAL      VABCBO = '6'
044800070730     C                   ENDIF
044900070730     C                   ENDIF
045000010202     C*
045100000801     C* Ebbene...
045200000801     C                   ADD       1             §CTRMO
045300070530     C                   IF        *in31 <> *zeros OR
045400070530     C                             *in32 <> *zeros
045500000801     C                   ADD       1             §CTRNO
045600020725     C                   EVAL      x_vinflg = '2'
045700000801     C                   ELSE
045800010201     C                   ADD       1             §CTROKVB
045900000801     C                   ENDIF
046000000801     C*
046100000801     C                   ENDSR
046200100127     C*----------------------------------------------------*
046300100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
046400100127     C*----------------------------------------------------*
046500100127     C     WRIVAT_A      BEGSR
046600100127     C*
046700100127     C* Valorizzo l buffer di scrittura del FIVAT
046800100127     C* - TIPO RECORD "A"
046900100412     C***                eval      VATTRC = 'A'
047000100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
047100100412     C***                exsr      wrivat
047200100127     C*
047300100127     C                   ENDSR
047400010201     C*----------------------------------------------------*
047500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
047600010201     C*----------------------------------------------------*
047700071003     C     WRIVAT_B      BEGSR
047800010201     C*
047900021113     C* Valorizzo l buffer di scrittura del FIVAT
048000070928     C* - TIPO RECORD "B"
048100100412     C***                eval      VATTRC = 'B'
048200100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
048300100412     C***                exsr      wrivat
048400010201     C*
048500010201     C                   ENDSR
048600071003     C*----------------------------------------------------*
048700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
048800071003     C*----------------------------------------------------*
048900071003     C     WRIVAT_E      BEGSR
049000071003     C*
049100071003     C* Valorizzo l buffer di scrittura del FIVAT
049200071003     C* - TIPO RECORD "E"
049300100310     C                   eval      VATTRC = 'E'
049400100419     C*
049500100419     C* X ogni collo indicato in testata
049600100419     C                   z-add     1             wColli            5 0
049700100419     C                   dow       wColli <= VABNCL
049800100419     C*
049900100419     C* Valorizzo il buffer di scrittura del FIVAT
050000100419     C                   move(p)   wColli        wColliAlfa        2
050100100504     C                   eval      VATNOT = %trim(%subst(vindta:21:6))+
050200100419     C                                      wColliAlfa
050300100419     C                   exsr      wrivat                                       => scarico VAT
050400100419     C*
050500100419     C                   add       1             wColli
050600100419     C                   enddo
050700100419     C*
050800071003     C*
050900071003     C                   ENDSR
051000100127     C*----------------------------------------------------*
051100100127     C*  SCARICO BUFFER FIVAT
051200100127     C*----------------------------------------------------*
051300100127     C     WRIVAT        BEGSR
051400100127     C*
051500100127     C                   if        vatNOT <> *blanks
051600100127     C                   ADD       1             §CTROKVT
051700100127     C                   write     FIVAT000
051800100127     C                   endif
051900100127     C*
052000100127     C                   ENDSR
052100020904
052200020904
052300010202     C*----------------------------------------------------*
052400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052500010202     C*----------------------------------------------------*
052600020305     C     PREVAT        BEGSR
052700010202     C*
052800021113     C* Compongo il nome del membro da dare al FIVATWWR
052900010202     C                   eval      parmbr = vlrhdl
053000010202     C                   movel     'M'           parmbr
053100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
053200010202     C                   eval      paropz = '1'
053300010202     C* Effettuo la chiamata al CLLE preposto
053400050128     C                   call(e)   'TITVVTC'
053500010202     C                   parm                    parccm
053600010202     C                   parm                    parmbr
053700010202     C                   parm                    paropz
053800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053900010202     C                   if        %error
054000010202     C                   movel     '1'           chkcall
054100010202     C                   else
054200010202     C                   movel     '0'           chkcall
054300010202     C                   endif
054400010202     C*
054500010202     C                   ENDSR
054600000801     C*----------------------------------------------------*
054700000801     C*  CONTROLLO NUMERICITA' CAMPI
054800000801     C*----------------------------------------------------*
054900000801     C     CHKNUM        BEGSR
055000081113     C*
055100081113     C                   IF        PiDecChr = *blanks
055200091223     C                   EVAL      PiDecChr = ','
055300081113     C                   ENDIF
055400091223     C*
055500081113     C                   callp(e)  UBISNUM_Check(PiStr
055600081113     C                                          :PiDecChr
055700081113     C                                          :PiVal
055800081113     C                                          :PiNum
055900081113     C                                          :PiInt)
056000081113     C*
056100000801     C                   IF        %error
056200000801     C                   EVAL      PiInt=*off
056300000801     C                   ENDIF
056400000801     C*
056500000801     C                   ENDSR
056600000801     C***
056700011113
056800011113
056900000801
057000000801
057100990920      /TITLE Invio dei dati al punto operativo.
057200010202     C     invio         BEGSR
057300990920     C*
057400021113     C* 1° invio FIVAT
057500010201     C                   reset                   dscmz
057600010201     C                   move      vlrpoi        cmzdst
057700021113     C                   eval      cmzfld = 'FIVATWWR'
057800010201     C                   eval      cmzmbd = vlrhdl
057900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058000021009     C***                if        prmfir = *blanks
058100021113     C                   eval      cmzfla = 'FIVAT00F'
058200021113     C                   eval      cmzmba = 'FIVAT00F'
058300021009     C***                else
058400021009     C***                eval      cmzfla = prmfir
058500021009     C***                eval      cmzmba = prmfir
058600021009     C***                endif
058700010201     C                   eval      cmznrr = *zeros
058800020305     C                   move      §ctrokvt      cmznrr
058900021018     C                   eval      cmzlba = vlrfl1
059000010201     C                   call(e)   'TIS711C'
059100010201     C                   parm                    dscmz
059200010201     C                   parm      *blanks       esito
059300010205     C                   if        %error
059400010205     C                             or cmzerr = '1'
059500010205     C                             or esito  = '1'
059600010205     C                   eval      wrkesito = '3'
059700010205     C                   else
059800010201     C*
059900021113     C* 2° invio FIVAB
060000010201     C                   reset                   dscmz
060100010201     C                   move      vlrpoi        cmzdst
060200010201     C                   eval      cmzfld = vlrfou
060300010201     C                   eval      cmzmbd = vlrhdl
060400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500021009     C***                if        prmfir = *blanks
060600021113     C                   eval      cmzfla = 'FIVAB00F'
060700021113     C                   eval      cmzmba = 'FIVAB00F'
060800021009     C***                else
060900021009     C***                eval      cmzfla = prmfir
061000021009     C***                eval      cmzmba = prmfir
061100021009     C***                endif
061200010201     C                   eval      cmznrr = *zeros
061300010201     C                   move      §ctrokvb      cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500010201     C                   call(e)   'TIS711C'
061600010201     C                   parm                    dscmz
061700010201     C                   parm      *blanks       esito
061800010201     C                   if        %error
061900010201     C                             or cmzerr = '1'
062000010201     C                             or esito  = '1'
062100010201     C                   eval      wrkesito = '3'
062200010201     C                   endif
062300010205     C                   endif
062400990920     C*
062500000613     C                   ENDSR
062600000613     C***
062700070411
062800070411     C     *pssr         BEGSR
062900070411     C*
063000070411     C                   if        %open(tivin00r)
063100070411     C                   close     tivin00r
063200070411     C                   endif
063300070411     C                   if        %open(fivabwwr)
063400070411     C                   close     fivabwwr
063500070411     C                   endif
063600070411     C                   if        %open(fivatwwr)
063700070411     C                   close     fivatwwr
063800070411     C                   endif
063900070411     C*
064000070411     C* Effettuo la chiamata al CLLE preposto
064100070411     C                   call(e)   'TITVVTC'
064200070411     C                   parm                    parccm
064300070411     C                   parm                    parmbr
064400070411     C                   parm      '2'           paropz
064500070411     C*
064600070411     C                   eval      wrkesito = '2'
064700070411     C*
064800070411     C                   seton                                        LR
064900070411     C*
065000070411     C                   ENDSR     '*CANCL'
065100070411     C***
065200070411
065300990910
065400000613     C     *inzsr        BEGSR
065500990910     C*
065600990910     C     *entry        plist
065700990920     C                   parm                    tivlrds
065800990921     C                   parm      wrkesito      esito
065900000724     C                   parm                    prmlit
066000000710     C                   parm                    prmfir
066100000613     C*
066200000830     C* CALCOLA LA DATA CORRENTE
066300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066400091223     C                   eval      datcor = %dec(%date() : *ISO)
066500000830     C*
066600000613     C                   ENDSR
066700000613     C***
