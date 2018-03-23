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
002000990915     D wrkdata         s               d
002100990915     D wrkora          s               t
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300000621     D recko           s            150    INZ(*blanks)
002400100928     D depspe          s              5    INZ(*blanks)
002500100928     D curspe          s              5    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100091223     D Num5_0          s              5  0
003200020725
003300020725     D*------------------
003400020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003500020725     D*------------------
003600070530     D INPUT_DS        DS                  INZ
003700100729     D  VINDTA                     2048
003800100729     D  VINFLG                        1
003900100729     D  VINSPE                        8
004000100729     D  VINRRN                        8  0
004100020725     D*
004200081113
004300091223     D*------------------
004400091223     D* DS REPERIMENTO NUMERATORE
004500091223     D*------------------
004600100309     D trul33ds      e ds                  inz
004700091223     D*------------------
004800091223     D* DS ARCHITETTURA
004900091223     D*------------------
005000091223     D kpjba         e ds                  inz
005100091223
005200081113
005300081113     D*------------------
005400081113     D* LINKING A DEFINIZIONI ESTERNE
005500081113     D*------------------
005600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005800081113
005900990908
006000010201
006100010201
006200080117     C*
006300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400080117     C
006500080117     C/EXEC SQL
006600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700080117     C/END-EXEC
006800080117     C
006900990915     C                   time                    wrkdata
007000990915     C                   time                    wrkora
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400000613     C*
007500100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
007600070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007700000613     C*
007800010202     C* Effettuo la chiamata al CLLE preposto
007900100525     C  N51              call(e)   'TITVVTC'
008000100525     C                   parm                    parccm
008100100525     C                   parm                    parmbr
008200100525     C                   parm      '2'           paropz
008300100525     C   51              call(e)   'TITVVBC'
008400100525     C                   parm                    parccm
008500100525     C                   parm                    parmbr
008600100525     C                   parm      '2'           paropz
008700070730     C*
008800070730     C* Effettuo lancio TISI95 solo x chiusura
008900070730     C                   CLEAR                   TISI95DS
009000070730     C                   EVAL      I95TLA = 'C'
009100070730     C                   CALL      'TISI95R'
009200070730     C                   PARM                    TISI95DS
009300000616     C*
009400000801     C
009500010201     C                   seton                                        LR
009600990908
009700000801
009800910830     C*--------------------------------------------------------
009900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010000910830     C*--------------------------------------------------------
010100070530     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600070530     C*
010700100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010800100525     C                   exsr      prevasx
010900010201     C*
011000010202     C                   if        chkcall = '0'
011100010202     C*
011200100525     C                   if        not %open(fivabwwr)
011300100525     C                   open      fivabwwr
011400100525     C                   endif
011500100525     C*
011600021113     C                   if        not %open(fivatwwr)
011700021113     C                   open      fivatwwr
011800010201     C                   endif
011900080117     C*
012000080117     C                   EXSR      INZVAR
012100080117     C                   EXSR      DEFCAM
012200990910     C*
012300010201     C                   clear                   §CTROKVB          5 0
012400020305     C                   clear                   §CTROKVT          5 0
012500000801     C                   clear                   §CTRMO            5 0
012600000801     C                   clear                   §CTRNO            5 0
012700990910     C*
012800020725     C/EXEC SQL
012900020725     C+ declare C1 cursor for select
013000100928     C+ vindta, vinflg, substr(vindta, 6, 5) as sped, rrn(tivin00r)
013100060223     C+ from tivin00r
013200100928     C+ order by sped, substr(vindta, 208, 6) desc
013300020725     C+ for read only
013400020725     C/END-EXEC
013500020725     C
013600020725     C/EXEC SQL
013700020725     C+ open C1
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100070530     C+ Fetch C1 into :INPUT_DS
014200020725     C/END-EXEC
014300020725     C*
014400020725     C                   dow       sqlcod = *zeros
014500990913     C*
014600100310     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800000801     C*
014900020725     C                   if        vinflg = *blanks
015000020725     C                             or vinflg = '0'
015100020725     C                             or vinflg = '2'
015200000801     C*
015300020725     C                   clear                   x_vinmsg
015400020725     C                   eval      x_vinflg = '1'
015500010305     C*
015600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015700100928     C                   EVAL      curspe=%trim(%subst(vindta:6:5))
015800010305     C*
015900100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100071003     C                   exsr      impvab
016200100525     C                   exsr      wrivab
016300100127     C                   exsr      wrivat_a                                     => carico VAT
016400071003     C                   exsr      wrivat_b                                     => carico VAT
016500100525     C   51              exsr      wrivat_e                                     => carico VAT
016600160119     C                   exsr      wrivat_s                                     => carico VAT
016700160119     C                   exsr      wrivat_i                                     => carico VAT
016800160119     C                   exsr      wrivat_j                                     => carico VAT
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200091223     C                   exsr      repNSP
017300071009     C                   exsr      impvab
017400100412     C                   exsr      wrivab
017500100127     C                   exsr      wrivat_a                                     => carico VAT
017600071009     C                   exsr      wrivat_b                                     => carico VAT
017700071009     C                   exsr      wrivat_e                                     => carico VAT
017800160119     C                   exsr      wrivat_s                                     => carico VAT
017900160119     C                   exsr      wrivat_i                                     => carico VAT
018000160119     C                   exsr      wrivat_j                                     => carico VAT
018100071009     C                   else
018200071009     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500080117     C                   clear                   tivinds
018600091223     C                   exsr      repNSP
018700020305     C                   exsr      impvab
018800100127     C                   exsr      wrivat_a                                     => carico VAT
018900071003     C                   exsr      wrivat_b                                     => carico VAT
019000071003     C   50              exsr      wrivat_e                                     => carico VAT
019100160119     C                   exsr      wrivat_s                                     => carico VAT
019200160119     C                   exsr      wrivat_i                                     => carico VAT
019300160119     C                   exsr      wrivat_j                                     => carico VAT
019400010305     C                   else
019500020725     C                   if        depspe <> curspe                             => rottura di spediz
019600010305     C                   eval      depspe = curspe                              => memorizz. spediz
019700100412     C                   exsr      wrivab
019800080117     C                   clear                   tivinds
019900091223     C                   exsr      repNSP
020000020305     C                   exsr      impvab
020100100127     C                   exsr      wrivat_a                                     => carico VAT
020200071003     C                   exsr      wrivat_b                                     => carico VAT
020300071003     C   50              exsr      wrivat_e                                     => carico VAT
020400160119     C                   exsr      wrivat_s                                     => carico VAT
020500160119     C                   exsr      wrivat_i                                     => carico VAT
020600160119     C                   exsr      wrivat_j                                     => carico VAT
020700020305     C                   else                                                   => x stessa spediz
020800100401     C*
020900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021000100928     C                   select
021100100928     C                   when      wDUPREC = ' '
021200020305     C                   exsr      impvab
021300100127     C                   exsr      wrivat_a                                     => carico VAT
021400071003     C                   exsr      wrivat_b                                     => carico VAT
021500071003     C   50              exsr      wrivat_e                                     => carico VAT
021600160119     C                   exsr      wrivat_s                                     => carico VAT
021700160119     C                   exsr      wrivat_i                                     => carico VAT
021800160119     C                   exsr      wrivat_j                                     => carico VAT
021900100928     C                   when      wDUPREC = 'C'
022000100928     C   50              exsr      wrivat_e                                     => carico VAT
022100100928     C                   endsl
022200010305     C                   endif
022300010305     C                   endif
022400010305     C                   endif
022500071003     C                   endif
022600071009     C                   endif
022700000905     C*
022800000905     C                   else
022900020725     C                   eval      x_vinflg = '1'
023000000905     C                   endif
023100000905     C*
023200020725     C     VINRRN        chain     tivin000
023300020725     C                   if        %found(tivin00r)
023400020725     C                   eval      y_vinflg = x_vinflg
023500020725     C                   eval      y_vinmsg = x_vinmsg
023600020725     C                   update    tivin000
023700020725     C                   endif
023800020725     C*
023900020725     C/EXEC SQL
024000070530     C+ Fetch C1 into :INPUT_DS
024100020725     C/END-EXEC
024200020725     C*
024300020725     C                   enddo
024400020725     C*
024500020725     C/EXEC SQL
024600020725     C+ close C1
024700020725     C/END-EXEC
024800000905     C*
024900020305     C* Scarico i VAB rimasti "in sospeso"
025000071009     C                   if        wDISK = 'C'
025100100412     C                   exsr      wrivab
025200071009     C                   endif
025300010202     C*
025400010202     C                   endif
025500990910
025600990910     C* Se non ci sono record con errori ...
025700000710     C                   if        §ctrno = 0
025800990910     C* ... restituisco esito OK.
025900990921     C                   eval      wrkesito = '0'
026000990910     C                   else
026100100525     C                   if        §ctrokvb > 0 OR
026200100525     C                             §ctrokvt > 0
026300990921     C                   eval      wrkesito = '1'
026400000710     C                   else
026500000710     C                   eval      wrkesito = '2'
026600990910     C                   endif
026700000710     C                   endif
026800990910     C*
026900990914     C                   if        %open(tivin00r)
027000990908     C                   close     tivin00r
027100990914     C                   endif
027200021113     C                   if        %open(fivabwwr)
027300021113     C                   close     fivabwwr
027400990914     C                   endif
027500021113     C                   if        %open(fivatwwr)
027600021113     C                   close     fivatwwr
027700010201     C                   endif
027800990910     C*
027900100525     C                   if        §ctrokvb > 0 OR
028000100525     C                             §ctrokvt > 0
028100000724     C                             and vlrpoi <> *zeros
028200010202     C                   exsr      invio
028300990920     C                   endif
028400990920     C*
028500910830     C                   ENDSR
028600000613     C***
028700100412
028800100412
028900010305
029000010305     C*----------------------------------------------------*
029100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029200010305     C*----------------------------------------------------*
029300020305     C     WRIVAB        BEGSR
029400100412     C*
029500100412     C* Gestisco l'eventuale rottura x numero spedizione
029600100412     C                   if        VABRMA = *blanks
029700100412     C                   movel(P)  VABRMN        VABRMA
029800100412     C                   endif
029900100412     C*
030000100525     C                   if        *in51 = *off and *in31 = *off
030100100525     C                   write     fivab000                                     => scarico il VAB
030200100525     C                   endif
030300010305     C*
030400010305     C                   ENDSR
030500990920
030600000801     C*----------------------------------------------------*
030700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030800000801     C*----------------------------------------------------*
030900010201     C     INZVAR        BEGSR
031000000801     C*
031100010201     C                   Z-ADD     *zeros        Num5_0
031200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031300020725     C                   MOVEL     '0'           FlgCAS            1
031400000801     C*
031500000801     C                   ENDSR
031600000801     C*----------------------------------------------------*
031700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031800000801     C*----------------------------------------------------*
031900020904     C     DEFCAM        BEGSR
032000080923     C*
032100100928     C                   SETOFF                                       505160
032200000801     C*
032300021113     C                   CLEAR                   FIVAB000
032400021113     C                   CLEAR                   FIVAT000
032500070730     C* Imposto i valori di default...
032600160119     C                   EVAL      VABCCM = 0361155
032700160119     C                   EVAL      VATCCM = 0361155
032800100928     C                   EVAL      VABLNP = 036
032900100928     C                   EVAL      VATLNP = 036
033000100928     C                   EVAL      VABCTR = 000
033100100928     C                   EVAL      VABCTM = '7Q'
033200100928     C                   EVAL      VABTSP = 'C'
033300100525     C                   EVAL      VABCBO = '1'
033400070222     C* ... e poi verifico se sono stati passati come parametri
033500070222     C                   IF        vlrppt > *blanks
033600100525     C*
033700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033900070222     C                   EXSR      CHKNUM
034000070222     C                   IF        PiInt=*on
034100070222     C                   Z-ADD     PiVal         VABCCM
034200070222     C                   Z-ADD     PiVal         VATCCM
034300070222     C                   ENDIF
034400070222     C                   ENDIF
034500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034700070222     C                   EXSR      CHKNUM
034800070222     C                   IF        PiInt=*on
034900070222     C                   Z-ADD     PiVal         VABLNP
035000070222     C                   Z-ADD     PiVal         VATLNP
035100070222     C                   ENDIF
035200070222     C                   ENDIF
035300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035500070222     C                   EXSR      CHKNUM
035600070222     C                   IF        PiInt=*on
035700070222     C                   Z-ADD     PiVal         VABCTR
035800070222     C                   ENDIF
035900070928     C                   ENDIF
036000071009     C                   MOVEL     *blanks       wDISK             1
036100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036200071009     C                   IF        wDISK <> *blanks
036300070928     C                   SETON                                        50
036400070222     C                   ENDIF
036500100525     C                   IF        wDISK  = 'T'
036600100525     C                   SETOFF                                       50
036700100525     C                   SETON                                        51
036800100525     C                   ENDIF
036900100401     C                   MOVEL     *blanks       wDUPREC           1
037000100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037100100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037200100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037300100525     C                   ENDIF
037400100525     C*
037500070222     C                   ENDIF
037600071009     C*
037700071009     C   50              EVAL      VABCTM = '7Q'
037800000801     C*
037900000801     C                   ENDSR
038000091223     C*----------------------------------------------------*
038100091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038200091223     C*----------------------------------------------------*
038300091223     C     REPNSP        BEGSR
038400091223     C*
038500091223     C                   EXSR      INZVAR
038600091223     C                   EXSR      DEFCAM
038700100928     C*
038800100928     C                   SETON                                        60
038900091223     C*
039000091223     C* NSP => Stacco un numeratore da AZNUM
039100091223     C                   clear                   TRUL33DS
039200091223     C                   eval      I33OPE = *zeros
039300091223     C                   eval      I33CNU = 302
039400091223     C                   eval      I33NUM = 1
039500091223     C                   movel     TRUL33DS      KPJBU
039600091223     C                   call      'TRUL33R'
039700091223     C                   parm                    KPJBA
039800091223     C                   movel     KPJBU         TRUL33DS
039900091223     C                   if        O33ERR = *zeros
040000091223     C                   z-add     O33NRF        VABNSP
040100091223     C                   z-add     O33NRF        VATNSP
040200091223     C                   else
040300091223     C                   SETON                                        31
040400091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040500091223     C                             + ' ' + 'VABNSP VATNSP'
040600091223     C                   endif
040700091223     C*
040800091223     C                   ENDSR
040900000801     C*----------------------------------------------------*
041000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041100000801     C*----------------------------------------------------*
041200100730     C     IMPVAB        BEGSR
041300070530     C*
041400070530     C                   SETOFF                                       3132
041500070928     C*
041600070928     C                   MOVE(P)   vlrpoi        VABFGS
041700070928     C                   MOVE(P)   vlrpoi        VATFGS
041800070928     C*
041900070928     C                   MOVEL     datcor        VABAAS
042000070928     C                   MOVEL     datcor        VATAAS
042100070928     C                   MOVE      datcor        VABMGS
042200100729     C*
042300100729     C* Reperimento campi ALFA
042400100928     C                   EVAL      VABRSD=%trim(%subst(vindta:46:35))
042500100928     C                   EVAL      VABRD2=%trim(%subst(vindta:81:35))
042600100928     C                   EVAL      VABIND=%trim(%subst(vindta:116:35))
042700100928     C                   EVAL      VABLOD=%trim(%subst(vindta:151:35))
042800160119     C                   EVAL      VABCAD=%trim(%subst(vindta:389:5))
042900100928     C                   EVAL      VABPRD=%trim(%subst(vindta:186:2))
043000101118     C***                EVAL      VABRMA=%trim(%subst(vindta:11:6))
043100160212     C***                EVAL      VABRMA=%trim(%subst(vindta:216:3))
043200160212     C                   EVAL      VABRMA=%trim(%subst(vindta:294:10))
043300100928     C                   EVAL      VABNOT=%trim(%subst(vindta:258:35))
043400101028     C*
043500160126     C***                IF        %subst(vindta:258:10)='TELEFONARE'
043600160126     C***                EVAL      VABTC1 = 'A'
043700160126     C***                ENDIF
043800160126     C*
043900160126     C                   IF        %subst(vindta:399:2)='PR'
044000160126     C                   EVAL      VABTC1 = 'A'
044100160126     C                   ENDIF
044200100729     C*
044300100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044400100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044500100729     C     '@':'A'       XLATE     VABRSD        VABRSD
044600100729     C* ==
044700100729     C*
044800100928     C* CCM
044900100730     C                   SELECT
045000160119     C                   WHEN      %trim(%subst(vindta:294:10)) = 'C100'
045100160119     C                   EVAL      VABCCM = 0361156
045200160119     C                   EVAL      VATCCM = 0361156
045300160119     C                   WHEN      %trim(%subst(vindta:294:10)) = 'C200'
045400160119     C                   EVAL      VABCCM = 0361157
045500160119     C                   EVAL      VATCCM = 0361157
045600160119     C                   OTHER
045700160119     C                   EVAL      VABCCM = 0361155
045800160119     C                   EVAL      VATCCM = 0361155
045900100730     C                   ENDSL
046000100729     C*
046100100729     C* Reperimento campi NUMERICI
046200100729     C                   MOVEL     DATCOR        VABAAS
046300100729     C                   MOVE      DATCOR        VABMGS
046400100730     C* RMN
046500100928     C                   EVAL      PiStr=%trim(%subst(vindta:6:5))
046600100730     C                   EXSR      CHKNUM
046700100730     C                   IF        PiInt=*on
046800100730     C                   Z-ADD     PiVal         VABRMN
046900100928     C  N60              Z-ADD     PiVal         VABNSP
047000100928     C  N60              Z-ADD     PiVal         VATNSP
047100100730     C                   ELSE
047200100730     C                   SETON                                        32
047300100730     C                   Z-ADD     1             VABRMN
047400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500100928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
047600100730     C                   ENDIF
047700100729     C* DCR
047800100730     C                   IF        %subst(vindta:443:6) <> *blanks AND
047900100730     C                             %subst(vindta:443:6) <> *zeros
048000100928     C                   EVAL      PiStr=%trim(%subst(vindta:237:8))
048100100729     C                   EXSR      CHKNUM
048200100729     C                   IF        PiInt=*on
048300100729     C                   Z-ADD     PiVal         VABDCR
048400100729     C                   ELSE
048500100729     C                   SETON                                        32
048600100729     C                   Z-ADD     *zeros        VABDCR
048700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048800100729     C                             + ' ' + 'VABDCR'
048900100729     C                   ENDIF
049000100729     C                   ENDIF
049100100729     C* NCL
049200100928     C                   IF        %subst(vindta:304:4) <> *blanks AND
049300100928     C                             %subst(vindta:304:4) <> *zeros
049400100928     C                   EVAL      PiStr=%trim(%subst(vindta:304:4))
049500100928     C                   EXSR      CHKNUM
049600100928     C                   IF        PiInt=*on
049700100928     C                   Z-ADD     PiVal         VABNCL
049800100928     C                   ELSE
049900100928     C                   SETON                                        32
050000100928     C                   Z-ADD     *zeros        VABNCL
050100100928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200100928     C                             + ' ' + 'VABNCL'
050300100928     C                   ENDIF
050400100928     C                   ELSE
050500100928     C                   EVAL      PiStr=%trim(%subst(vindta:188:4))
050600100729     C                   EXSR      CHKNUM
050700100729     C                   IF        PiInt=*on
050800100729     C                   Z-ADD     PiVal         VABNCL
050900100729     C                   ELSE
051000100729     C                   SETON                                        32
051100100729     C                   Z-ADD     *zeros        VABNCL
051200100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300100729     C                             + ' ' + 'VABNCL'
051400100928     C                   ENDIF
051500100729     C                   ENDIF
051600100729     C* PKB
051700160119     C                   EVAL      PiStr=%trim(%subst(vindta:208:8))
051800100729     C                   EXSR      CHKNUM
051900100729     C                   IF        PiNum=*on
052000160119     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
052100100729     C                   Z-ADD(H)  PiVal         VABPKB
052200100729     C                   ELSE
052300100729     C                   SETON                                        32
052400100729     C                   Z-ADD     *zeros        VABPKB
052500100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052600100729     C                             + ' ' + 'VABPKB'
052700100729     C                   ENDIF
052800100729     C* CAS
052900160119     C                   IF        %subst(vindta:249:9)<> *blanks AND
053000160119     C                             %subst(vindta:249:9)<> *zeros
053100100729     C                   EVAL      FlgCAS = '1'
053200100729     C                   EVAL      VABVCA = 'EUR'
053300160119     C                   EVAL      VABTIC = 'TM'
053400160119     C                   EVAL      PiStr=%trim(%subst(vindta:249:9))
053500100729     C                   EXSR      CHKNUM
053600100729     C                   IF        PiNum=*on
053700100928     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
053800100928     C                   Z-ADD(H)  PiVal         VABCAS
053900100729     C                   ELSE
054000100729     C                   SETON                                        32
054100100729     C                   Z-ADD     *zeros        VABCAS
054200100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054300100729     C                             + ' ' + 'VABCAS'
054400100729     C                   ENDIF
054500100729     C                   ENDIF
054600100729     C*
054700100729     C* Se provincia nn valorizzata la reperisco
054800100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
054900100729     C                   IF        VABNZD  = *blanks AND
055000100729     C                             VABPRD  = *blanks AND
055100100729     C                             VABCAD <> *blanks
055200100729     C                   CLEAR                   TISI95DS
055300100729     C                   EVAL      I95TCN = '3'
055400100729     C                   Z-ADD     datcor        I95DAT
055500100729     C                   EVAL      I95NAR = VABNZD
055600100729     C                   EVAL      I95CAP = VABCAD
055700100729     C                   EVAL      I95LOC = VABLOD
055800100729     C                   CALL      'TISI95R'
055900100729     C                   PARM                    TISI95DS
056000100729     C                   EVAL      VABPRD = O95PRV
056100100729     C                   ENDIF
056200100729     C*
056300100729     C* Considerazioni finali su CBO/CAS
056400100729     C                   IF        FlgCAS = '1'
056500100729     C                   IF        VABCBO = '1'
056600100729     C                   EVAL      VABCBO = '4'
056700100729     C                   ENDIF
056800100729     C                   IF        VABCBO = '2'
056900100729     C                   EVAL      VABCBO = '6'
057000100729     C                   ENDIF
057100100729     C                   ENDIF
057200010202     C*
057300000801     C* Ebbene...
057400000801     C                   ADD       1             §CTRMO
057500070530     C                   IF        *in31 <> *zeros OR
057600070530     C                             *in32 <> *zeros
057700000801     C                   ADD       1             §CTRNO
057800000801     C                   EVAL      recko = vindta
057900020725     C                   EVAL      x_vinflg = '2'
058000000801     C                   ELSE
058100010201     C                   ADD       1             §CTROKVB
058200000801     C                   ENDIF
058300000801     C*
058400000801     C                   ENDSR
058500100127     C*----------------------------------------------------*
058600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
058700100127     C*----------------------------------------------------*
058800100127     C     WRIVAT_A      BEGSR
058900100127     C*
059000100127     C* Valorizzo l buffer di scrittura del FIVAT
059100100127     C* - TIPO RECORD "A"
059200100412     C***                eval      VATTRC = 'A'
059300100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
059400100412     C***                exsr      wrivat
059500100127     C*
059600100127     C                   ENDSR
059700010201     C*----------------------------------------------------*
059800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059900010201     C*----------------------------------------------------*
060000071003     C     WRIVAT_B      BEGSR
060100010201     C*
060200021113     C* Valorizzo l buffer di scrittura del FIVAT
060300070928     C* - TIPO RECORD "B"
060400100412     C***                eval      VATTRC = 'B'
060500100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
060600100412     C***                exsr      wrivat
060700010201     C*
060800010201     C                   ENDSR
060900071003     C*----------------------------------------------------*
061000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
061100071003     C*----------------------------------------------------*
061200071003     C     WRIVAT_E      BEGSR
061300071003     C*
061400071003     C* Valorizzo l buffer di scrittura del FIVAT
061500071003     C* - TIPO RECORD "E"
061600100729     C*
061700100928     C                   eval      VATTRC = 'E'
061800100928     C                   eval      VATNOT = %trim(%subst(vindta:192:16))
061900100928     C                   exsr      wrivat
062000071003     C*
062100071003     C                   ENDSR
062200160119     C*----------------------------------------------------*
062300160119     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
062400160119     C*----------------------------------------------------*
062500160119     C     WRIVAT_S      BEGSR
062600160119     C*
062700160119     C* Valorizzo l buffer di scrittura del FIVAT
062800160119     C* - TIPO RECORD "S"
062900160119     C*
063000160119     C                   eval      VATTRC = 'S'
063100160122     C                   eval      VATNOT = %trim(%subst(vindta:308:16))
063200160119     C                   exsr      wrivat
063300160119     C*
063400160119     C                   ENDSR
063500160119     C*----------------------------------------------------*
063600160119     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
063700160119     C*----------------------------------------------------*
063800160119     C     WRIVAT_I      BEGSR
063900160119     C*
064000160119     C* Valorizzo l buffer di scrittura del FIVAT
064100160119     C* - TIPO RECORD "I"
064200160119     C*
064300160119     C                   eval      VATTRC = 'I'
064400160122     C                   eval      VATNOT = %trim(%subst(vindta:324:35))
064500160119     C                   exsr      wrivat
064600160119     C*
064700160119     C                   ENDSR
064800160119     C*----------------------------------------------------*
064900160119     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "J"
065000160119     C*----------------------------------------------------*
065100160119     C     WRIVAT_J      BEGSR
065200160119     C*
065300160119     C* Valorizzo l buffer di scrittura del FIVAT
065400160119     C* - TIPO RECORD "J"
065500160119     C*
065600160119     C                   eval      VATTRC = 'J'
065700160122     C                   eval      VATNOT = %trim(%subst(vindta:324+35:50-35))
065800160119     C                   exsr      wrivat
065900160119     C*
066000160119     C                   ENDSR
066100100127     C*----------------------------------------------------*
066200100127     C*  SCARICO BUFFER FIVAT
066300100127     C*----------------------------------------------------*
066400100127     C     WRIVAT        BEGSR
066500100127     C*
066600100127     C                   if        vatNOT <> *blanks
066700100127     C                   ADD       1             §CTROKVT
066800100127     C                   write     FIVAT000
066900100127     C                   endif
067000100127     C*
067100100127     C                   ENDSR
067200020904
067300020904
067400010202     C*----------------------------------------------------*
067500100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
067600010202     C*----------------------------------------------------*
067700100525     C     PREVASX       BEGSR
067800100525     C*
067900021113     C* Compongo il nome del membro da dare al FIVATWWR
068000010202     C                   eval      parmbr = vlrhdl
068100010202     C                   movel     'M'           parmbr
068200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
068300010202     C                   eval      paropz = '1'
068400100525     C*
068500010202     C* Effettuo la chiamata al CLLE preposto
068600100525     C  N51              call(e)   'TITVVTC'
068700010202     C                   parm                    parccm
068800010202     C                   parm                    parmbr
068900010202     C                   parm                    paropz
069000100525     C   51              call(e)   'TITVVBC'
069100100525     C                   parm                    parccm
069200100525     C                   parm                    parmbr
069300100525     C                   parm                    paropz
069400100525     C*
069500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069600010202     C                   if        %error
069700010202     C                   movel     '1'           chkcall
069800010202     C                   else
069900010202     C                   movel     '0'           chkcall
070000010202     C                   endif
070100010202     C*
070200010202     C                   ENDSR
070300000801     C*----------------------------------------------------*
070400000801     C*  CONTROLLO NUMERICITA' CAMPI
070500000801     C*----------------------------------------------------*
070600000801     C     CHKNUM        BEGSR
070700081113     C*
070800081113     C                   IF        PiDecChr = *blanks
070900100503     C                   EVAL      PiDecChr = '.'
071000081113     C                   ENDIF
071100091223     C*
071200081113     C                   callp(e)  UBISNUM_Check(PiStr
071300081113     C                                          :PiDecChr
071400081113     C                                          :PiVal
071500081113     C                                          :PiNum
071600081113     C                                          :PiInt)
071700081113     C*
071800000801     C                   IF        %error
071900000801     C                   EVAL      PiInt=*off
072000000801     C                   ENDIF
072100000801     C*
072200000801     C                   ENDSR
072300000801     C***
072400011113
072500011113
072600000801
072700000801
072800990920      /TITLE Invio dei dati al punto operativo.
072900010202     C     invio         BEGSR
073000990920     C*
073100021113     C* 1° invio FIVAT
073200010201     C                   reset                   dscmz
073300010201     C                   move      vlrpoi        cmzdst
073400021113     C                   eval      cmzfld = 'FIVATWWR'
073500010201     C                   eval      cmzmbd = vlrhdl
073600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073700021009     C***                if        prmfir = *blanks
073800021113     C                   eval      cmzfla = 'FIVAT00F'
073900021113     C                   eval      cmzmba = 'FIVAT00F'
074000021009     C***                else
074100021009     C***                eval      cmzfla = prmfir
074200021009     C***                eval      cmzmba = prmfir
074300021009     C***                endif
074400010201     C                   eval      cmznrr = *zeros
074500020305     C                   move      §ctrokvt      cmznrr
074600021018     C                   eval      cmzlba = vlrfl1
074700010201     C                   call(e)   'TIS711C'
074800010201     C                   parm                    dscmz
074900010201     C                   parm      *blanks       esito
075000010205     C                   if        %error
075100010205     C                             or cmzerr = '1'
075200010205     C                             or esito  = '1'
075300010205     C                   eval      wrkesito = '3'
075400010205     C                   else
075500010201     C*
075600021113     C* 2° invio FIVAB
075700100525     C                   if        *in51 = *off
075800010201     C                   reset                   dscmz
075900010201     C                   move      vlrpoi        cmzdst
076000010201     C                   eval      cmzfld = vlrfou
076100010201     C                   eval      cmzmbd = vlrhdl
076200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076300021009     C***                if        prmfir = *blanks
076400021113     C                   eval      cmzfla = 'FIVAB00F'
076500021113     C                   eval      cmzmba = 'FIVAB00F'
076600021009     C***                else
076700021009     C***                eval      cmzfla = prmfir
076800021009     C***                eval      cmzmba = prmfir
076900021009     C***                endif
077000010201     C                   eval      cmznrr = *zeros
077100010201     C                   move      §ctrokvb      cmznrr
077200021018     C                   eval      cmzlba = vlrfl1
077300010201     C                   call(e)   'TIS711C'
077400010201     C                   parm                    dscmz
077500010201     C                   parm      *blanks       esito
077600010201     C                   if        %error
077700010201     C                             or cmzerr = '1'
077800010201     C                             or esito  = '1'
077900010201     C                   eval      wrkesito = '3'
078000010201     C                   endif
078100100525     C                   endif
078200010205     C                   endif
078300990920     C*
078400000613     C                   ENDSR
078500000613     C***
078600070411
078700070411     C     *pssr         BEGSR
078800070411     C*
078900070411     C                   if        %open(tivin00r)
079000070411     C                   close     tivin00r
079100070411     C                   endif
079200070411     C                   if        %open(fivabwwr)
079300070411     C                   close     fivabwwr
079400070411     C                   endif
079500070411     C                   if        %open(fivatwwr)
079600070411     C                   close     fivatwwr
079700070411     C                   endif
079800070411     C*
079900070411     C* Effettuo la chiamata al CLLE preposto
080000100525     C  N51              call(e)   'TITVVTC'
080100100525     C                   parm                    parccm
080200100525     C                   parm                    parmbr
080300100525     C                   parm      '2'           paropz
080400100525     C   51              call(e)   'TITVVBC'
080500100525     C                   parm                    parccm
080600100525     C                   parm                    parmbr
080700100525     C                   parm      '2'           paropz
080800070411     C*
080900070411     C                   eval      wrkesito = '2'
081000070411     C*
081100070411     C                   seton                                        LR
081200070411     C*
081300070411     C                   ENDSR     '*CANCL'
081400070411     C***
081500070411
081600990910
081700000613     C     *inzsr        BEGSR
081800990910     C*
081900990910     C     *entry        plist
082000990920     C                   parm                    tivlrds
082100990921     C                   parm      wrkesito      esito
082200000724     C                   parm                    prmlit
082300000710     C                   parm                    prmfir
082400000613     C*
082500000830     C* CALCOLA LA DATA CORRENTE
082600091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082700091223     C                   eval      datcor = %dec(%date() : *ISO)
082800000830     C*
082900000613     C                   ENDSR
083000000613     C***
083100990908
