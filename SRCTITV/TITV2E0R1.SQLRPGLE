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
002100110408     D depspe          s             10    INZ(*blanks)
002200110408     D curspe          s             10    INZ(*blanks)
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
003600110408     D  VINSPE                       10
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
004900101119
005000101119     D*-------------------
005100101119     D* COSTANTI
005200101119     D*-------------------
005300101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005400101119     D                                     èéù')
005500101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005600101119     D                                     EEU')
005700081113
005800081113     D*------------------
005900081113     D* LINKING A DEFINIZIONI ESTERNE
006000081113     D*------------------
006100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006200090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113
006400990908
006500010201
006600010201
006700080117     C*
006800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006900080117     C
007000080117     C/EXEC SQL
007100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007200080117     C/END-EXEC
007300080117     C
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000613     C*
007800171205     C                   EXSR      INZVAR
007900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300100525     C  N51              call(e)   'TITVVTC'
008400100525     C                   parm                    parccm
008500100525     C                   parm                    parmbr
008600100525     C                   parm      '2'           paropz
008700100525     C   51              call(e)   'TITVVBC'
008800100525     C                   parm                    parccm
008900100525     C                   parm                    parmbr
009000100525     C                   parm      '2'           paropz
009100070730     C*
009200070730     C* Effettuo lancio TISI95 solo x chiusura
009300070730     C                   CLEAR                   TISI95DS
009400070730     C                   EVAL      I95TLA = 'C'
009500070730     C                   CALL      'TISI95R'
009600070730     C                   PARM                    TISI95DS
009700000616     C*
009800000801     C
009900010201     C                   seton                                        LR
010000990908
010100000801
010200910830     C*--------------------------------------------------------
010300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010400910830     C*--------------------------------------------------------
010500070530     C     RWFILE        BEGSR
010600990910     C*
010700990914     C                   if        not %open(tivin00r)
010800990908     C                   open      tivin00r
010900990914     C                   endif
011000070530     C*
011100100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011200100525     C                   exsr      prevasx
011300010201     C*
011400010202     C                   if        chkcall = '0'
011500010202     C*
011600100525     C                   if        not %open(fivabwwr)
011700100525     C                   open      fivabwwr
011800100525     C                   endif
011900100525     C*
012000021113     C                   if        not %open(fivatwwr)
012100021113     C                   open      fivatwwr
012200010201     C                   endif
012300080117     C*
012400080117     C                   EXSR      INZVAR
012500080117     C                   EXSR      DEFCAM
012600990910     C*
012700010201     C                   clear                   §CTROKVB          5 0
012800020305     C                   clear                   §CTROKVT          5 0
012900000801     C                   clear                   §CTRMO            5 0
013000000801     C                   clear                   §CTRNO            5 0
013100990910     C*
013200020725     C/EXEC SQL
013300020725     C+ declare C1 cursor for select
013400110408     C+ vindta, vinflg, substr(vindta, 1, 10) as sped, rrn(tivin00r)
013500060223     C+ from tivin00r
013600110408     C+ order by sped, substr(vindta, 1, 10) desc
013700020725     C+ for read only
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100020725     C+ open C1
014200020725     C/END-EXEC
014300020725     C
014400020725     C/EXEC SQL
014500070530     C+ Fetch C1 into :INPUT_DS
014600020725     C/END-EXEC
014700020725     C*
014800020725     C                   dow       sqlcod = *zeros
014900990913     C*
015000100310     C                   if        vindta > *blanks
015100000613     C                   add       1             rrnum
015200000801     C*
015300020725     C                   if        vinflg = *blanks
015400020725     C                             or vinflg = '0'
015500020725     C                             or vinflg = '2'
015600000801     C*
015700020725     C                   clear                   x_vinmsg
015800020725     C                   eval      x_vinflg = '1'
015900101119     C*
016000101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016100101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016200010305     C*
016300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016400110408     C                   EVAL      curspe=%trim(%subst(vindta:1:10))
016500010305     C*
016600100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016700080923     C                   if        *in50 = *off
016800101119     C                   exsr      inzvar
016900101119     C                   exsr      defcam
017000071003     C                   exsr      impvab
017100100525     C                   exsr      wrivab
017200100127     C                   exsr      wrivat_a                                     => carico VAT
017300071003     C                   exsr      wrivat_b                                     => carico VAT
017400160122     C                   exsr      wrivat_ij                                    => carico VAT
017500160122     C                   exsr      wrivat_s                                     => carico VAT
017600100525     C   51              exsr      wrivat_e                                     => carico VAT
017700071003     C                   else
017800080923     C*
017900071009     C                   if        wDISK = 'D'
018000150202     C***                exsr      repNSP
018100071009     C                   exsr      impvab
018200100412     C                   exsr      wrivab
018300100127     C                   exsr      wrivat_a                                     => carico VAT
018400071009     C                   exsr      wrivat_b                                     => carico VAT
018500160122     C                   exsr      wrivat_ij                                    => carico VAT
018600160122     C                   exsr      wrivat_s                                     => carico VAT
018700071009     C                   exsr      wrivat_e                                     => carico VAT
018800071009     C                   else
018900071009     C*
019000010305     C                   if        depspe = *blanks                             => 1° giro
019100010305     C                   eval      depspe = curspe                              => memorizz. spediz
019200080117     C                   clear                   tivinds
019300150202     C***                exsr      repNSP
019400020305     C                   exsr      impvab
019500100127     C                   exsr      wrivat_a                                     => carico VAT
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700160122     C                   exsr      wrivat_ij                                    => carico VAT
019800160122     C                   exsr      wrivat_s                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000010305     C                   else
020100020725     C                   if        depspe <> curspe                             => rottura di spediz
020200010305     C                   eval      depspe = curspe                              => memorizz. spediz
020300100412     C                   exsr      wrivab
020400080117     C                   clear                   tivinds
020500150202     C***                exsr      repNSP
020600020305     C                   exsr      impvab
020700100127     C                   exsr      wrivat_a                                     => carico VAT
020800071003     C                   exsr      wrivat_b                                     => carico VAT
020900160122     C                   exsr      wrivat_ij                                    => carico VAT
021000160122     C                   exsr      wrivat_s                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200020305     C                   else                                                   => x stessa spediz
021300100401     C*
021400100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021500100928     C                   select
021600100928     C                   when      wDUPREC = ' '
021700020305     C                   exsr      impvab
021800100127     C                   exsr      wrivat_a                                     => carico VAT
021900071003     C                   exsr      wrivat_b                                     => carico VAT
022000160122     C                   exsr      wrivat_ij                                    => carico VAT
022100160122     C                   exsr      wrivat_s                                     => carico VAT
022200071003     C   50              exsr      wrivat_e                                     => carico VAT
022300100928     C                   when      wDUPREC = 'C'
022400100928     C   50              exsr      wrivat_e                                     => carico VAT
022500100928     C                   endsl
022600010305     C                   endif
022700010305     C                   endif
022800010305     C                   endif
022900071003     C                   endif
023000110201     C*
023100071009     C                   endif
023200000905     C*
023300000905     C                   else
023400020725     C                   eval      x_vinflg = '1'
023500000905     C                   endif
023600000905     C*
023700020725     C     VINRRN        chain     tivin000
023800020725     C                   if        %found(tivin00r)
023900020725     C                   eval      y_vinflg = x_vinflg
024000020725     C                   eval      y_vinmsg = x_vinmsg
024100020725     C                   update    tivin000
024200020725     C                   endif
024300020725     C*
024400020725     C/EXEC SQL
024500070530     C+ Fetch C1 into :INPUT_DS
024600020725     C/END-EXEC
024700020725     C*
024800020725     C                   enddo
024900020725     C*
025000020725     C/EXEC SQL
025100020725     C+ close C1
025200020725     C/END-EXEC
025300000905     C*
025400020305     C* Scarico i VAB rimasti "in sospeso"
025500071009     C                   if        wDISK = 'C'
025600100412     C                   exsr      wrivab
025700071009     C                   endif
025800010202     C*
025900010202     C                   endif
026000990910
026100990910     C* Se non ci sono record con errori ...
026200000710     C                   if        §ctrno = 0
026300990910     C* ... restituisco esito OK.
026400990921     C                   eval      wrkesito = '0'
026500990910     C                   else
026600100525     C                   if        §ctrokvb > 0 OR
026700100525     C                             §ctrokvt > 0
026800990921     C                   eval      wrkesito = '1'
026900000710     C                   else
027000000710     C                   eval      wrkesito = '2'
027100990910     C                   endif
027200000710     C                   endif
027300990910     C*
027400990914     C                   if        %open(tivin00r)
027500990908     C                   close     tivin00r
027600990914     C                   endif
027700021113     C                   if        %open(fivabwwr)
027800021113     C                   close     fivabwwr
027900990914     C                   endif
028000021113     C                   if        %open(fivatwwr)
028100021113     C                   close     fivatwwr
028200010201     C                   endif
028300990910     C*
028400100525     C                   if        §ctrokvb > 0 OR
028500100525     C                             §ctrokvt > 0
028600000724     C                             and vlrpoi <> *zeros
028700010202     C                   exsr      invio
028800990920     C                   endif
028900990920     C*
029000910830     C                   ENDSR
029100000613     C***
029200100412
029300100412
029400010305
029500010305     C*----------------------------------------------------*
029600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029700010305     C*----------------------------------------------------*
029800020305     C     WRIVAB        BEGSR
029900100412     C*
030000100412     C* Gestisco l'eventuale rottura x numero spedizione
030100100412     C                   if        VABRMA = *blanks
030200100412     C                   movel(P)  VABRMN        VABRMA
030300100412     C                   endif
030400100412     C*
030500100525     C                   if        *in51 = *off and *in31 = *off
030600100525     C                   write     fivab000                                     => scarico il VAB
030700100525     C                   endif
030800010305     C*
030900010305     C                   ENDSR
031000990920
031100000801     C*----------------------------------------------------*
031200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031300000801     C*----------------------------------------------------*
031400010201     C     INZVAR        BEGSR
031500000801     C*
031600010201     C                   Z-ADD     *zeros        Num5_0
031700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031800020725     C                   MOVEL     '0'           FlgCAS            1
031900000801     C*
032000000801     C                   ENDSR
032100000801     C*----------------------------------------------------*
032200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032300000801     C*----------------------------------------------------*
032400020904     C     DEFCAM        BEGSR
032500080923     C*
032600100928     C                   SETOFF                                       505160
032700000801     C*
032800021113     C                   CLEAR                   FIVAB000
032900021113     C                   CLEAR                   FIVAT000
033000070730     C* Imposto i valori di default...
033100110408     C                   EVAL      VABCCM = 2022829
033200110408     C                   EVAL      VATCCM = 2022829
033300110408     C                   EVAL      VABLNP = 202
033400110408     C                   EVAL      VATLNP = 202
033500110128     C                   EVAL      VABCTR = 000
033600110408     C                   EVAL      VABCTM = '7A'
033700100525     C                   EVAL      VABCBO = '1'
033800070222     C* ... e poi verifico se sono stati passati come parametri
033900070222     C                   IF        vlrppt > *blanks
034000100525     C*
034100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034300070222     C                   EXSR      CHKNUM
034400070222     C                   IF        PiInt=*on
034500070222     C                   Z-ADD     PiVal         VABCCM
034600070222     C                   Z-ADD     PiVal         VATCCM
034700070222     C                   ENDIF
034800070222     C                   ENDIF
034900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035100070222     C                   EXSR      CHKNUM
035200070222     C                   IF        PiInt=*on
035300070222     C                   Z-ADD     PiVal         VABLNP
035400070222     C                   Z-ADD     PiVal         VATLNP
035500070222     C                   ENDIF
035600070222     C                   ENDIF
035700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035900070222     C                   EXSR      CHKNUM
036000070222     C                   IF        PiInt=*on
036100070222     C                   Z-ADD     PiVal         VABCTR
036200070222     C                   ENDIF
036300070928     C                   ENDIF
036400071009     C                   MOVEL     *blanks       wDISK             1
036500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036600071009     C                   IF        wDISK <> *blanks
036700070928     C                   SETON                                        50
036800070222     C                   ENDIF
036900100525     C                   IF        wDISK  = 'T'
037000100525     C                   SETOFF                                       50
037100100525     C                   SETON                                        51
037200100525     C                   ENDIF
037300150202     C*
037400150202     C   50              EVAL      VABCTM = '7Q'
037500150202     C*
037600100401     C                   MOVEL     *blanks       wDUPREC           1
037700100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037800100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037900100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038000100525     C                   ENDIF
038100100525     C*
038200070222     C                   ENDIF
038300000801     C*
038400000801     C                   ENDSR
038500091223     C*----------------------------------------------------*
038600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038700091223     C*----------------------------------------------------*
038800091223     C     REPNSP        BEGSR
038900091223     C*
039000091223     C                   EXSR      INZVAR
039100091223     C                   EXSR      DEFCAM
039200100928     C*
039300100928     C                   SETON                                        60
039400091223     C*
039500091223     C* NSP => Stacco un numeratore da AZNUM
039600091223     C                   clear                   TRUL33DS
039700091223     C                   eval      I33OPE = *zeros
039800091223     C                   eval      I33CNU = 302
039900091223     C                   eval      I33NUM = 1
040000091223     C                   movel     TRUL33DS      KPJBU
040100091223     C                   call      'TRUL33R'
040200091223     C                   parm                    KPJBA
040300091223     C                   movel     KPJBU         TRUL33DS
040400091223     C                   if        O33ERR = *zeros
040500091223     C                   z-add     O33NRF        VABNSP
040600091223     C                   z-add     O33NRF        VATNSP
040700091223     C                   else
040800091223     C                   SETON                                        31
040900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000091223     C                             + ' ' + 'VABNSP VATNSP'
041100091223     C                   endif
041200091223     C*
041300091223     C                   ENDSR
041400000801     C*----------------------------------------------------*
041500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041600000801     C*----------------------------------------------------*
041700100730     C     IMPVAB        BEGSR
041800070530     C*
041900070530     C                   SETOFF                                       3132
042000171205     C*
042100171205     C                   EXSR      INZVAR
042200171205     C                   EXSR      DEFCAM
042300070928     C*
042400070928     C                   MOVE(P)   vlrpoi        VABFGS
042500070928     C                   MOVE(P)   vlrpoi        VATFGS
042600070928     C*
042700070928     C                   MOVEL     datcor        VABAAS
042800070928     C                   MOVEL     datcor        VATAAS
042900070928     C                   MOVE      datcor        VABMGS
043000100729     C*
043100100729     C* Reperimento campi ALFA
043200110408     C                   EVAL      VABRSD=%trim(%subst(vindta:19:35))
043300110408     C                   EVAL      VABRD2=%trim(%subst(vindta:19+35:40-35))
043400110408     C                   EVAL      VABRMA=%trim(%subst(vindta:1:10))
043500110408     C                   EVAL      VABIND=%trim(%subst(vindta:59:40))
043600110408     C                   EVAL      VABLOD=%trim(%subst(vindta:109:35))
043700110408     C                   EVAL      VABPRD=%trim(%subst(vindta:144:2))
043800110408     C                   EVAL      VABCAD=%trim(%subst(vindta:99:5))
043900110408     C                   EVAL      VABNOT=%trim(%subst(vindta:178:35))
044000110408     C                   EVAL      VABNT2=%trim(%subst(vindta:178+35:75-35))
044100110503     C                   EVAL      VABTSP=%trim(%subst(vindta:262:1))
044200100729     C*
044300100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044400100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044500100729     C     '@':'A'       XLATE     VABRSD        VABRSD
044600100729     C* ==
044700100729     C*
044800100729     C* Reperimento campi NUMERICI
044900100729     C                   MOVEL     DATCOR        VABAAS
045000100729     C                   MOVE      DATCOR        VABMGS
045100110408     C* CCM
045200110408     C                   EVAL      PiStr=%trim(%subst(vindta:247:7))
045300110408     C                   EXSR      CHKNUM
045400110408     C                   IF        PiInt=*on
045500110408     C                   Z-ADD     PiVal         VABCCM
045600110408     C                   Z-ADD     PiVal         VATCCM
045700110408     C                   ELSE
045800110408     C  N51              SETON                                        32
045900110408     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
046000110408     C                             + ' ' + 'VABCCM VATCCM'
046100110408     C                   ENDIF
046200110408     C*
046300110408     C                   SELECT
046400110408     C                   WHEN      VABCCM = 2022829
046500110408     C                   EVAL      VABLNP = 202
046600110408     C                   EVAL      VABFGS = 202
046700150202     C                   EVAL      VATLNP = 202
046800110411     C                   EVAL      VATFGS = 202
046900110408     C                   EVAL      VABCTR = 000
047000110408     C                   WHEN      VABCCM = 1660846
047100110408     C                   EVAL      VABLNP = 166
047200110408     C                   EVAL      VABFGS = 166
047300150202     C                   EVAL      VATLNP = 166
047400110411     C                   EVAL      VATFGS = 166
047500110408     C                   EVAL      VABCTR = 000
047600110408     C                   WHEN      VABCCM = 1162029
047700110408     C                   EVAL      VABLNP = 116
047800110408     C                   EVAL      VABFGS = 116
047900150202     C                   EVAL      VATLNP = 116
048000110411     C                   EVAL      VATFGS = 116
048100110408     C                   EVAL      VABCTR = 000
048200150119     C                   WHEN      VABCCM = 0057864
048300150119     C                   EVAL      VABLNP = 005
048400150119     C                   EVAL      VABFGS = 005
048500150202     C                   EVAL      VATLNP = 005
048600150119     C                   EVAL      VATFGS = 005
048700150119     C                   EVAL      VABCTR = 000
048800150119     C                   WHEN      VABCCM = 0057865
048900150119     C                   EVAL      VABLNP = 005
049000150119     C                   EVAL      VABFGS = 005
049100150202     C                   EVAL      VATLNP = 005
049200150119     C                   EVAL      VATFGS = 005
049300150119     C                   EVAL      VABCTR = 000
049400160414     C                   WHEN      VABCCM = 0058368
049500160414     C                   EVAL      VABLNP = 005
049600160414     C                   EVAL      VABFGS = 005
049700160414     C                   EVAL      VATLNP = 005
049800160414     C                   EVAL      VATFGS = 005
049900171024     C                   EVAL      VABCTR = 000
050000171024     C                   WHEN      VABCCM = 0058905
050100171024     C                   EVAL      VABLNP = 005
050200171024     C                   EVAL      VABFGS = 005
050300171024     C                   EVAL      VATLNP = 005
050400171024     C                   EVAL      VATFGS = 005
050500171024     C                   EVAL      VABCTR = 399
050600171108     C                   EVAL      VABCTM = '7R'
050700171115     C                   EVAL      VABNZD=%trim(%subst(vindta:147:3))
050800171024     C                   WHEN      VABCCM = 0058906
050900171024     C                   EVAL      VABLNP = 005
051000171024     C                   EVAL      VABFGS = 005
051100171024     C                   EVAL      VATLNP = 005
051200171024     C                   EVAL      VATFGS = 005
051300171024     C                   EVAL      VABCTR = 090
051400171108     C                   EVAL      VABCTM = '7Q'
051500171115     C                   EVAL      VABNZD=%trim(%subst(vindta:147:3))
051600180122     C                   WHEN      VABCCM = 0058966
051700180122     C                   EVAL      VABLNP = 005
051800180122     C                   EVAL      VABFGS = 005
051900180122     C                   EVAL      VATLNP = 005
052000180122     C                   EVAL      VATFGS = 005
052100180122     C                   EVAL      VABCTR = 000
052200180124     C                   EVAL      VABCTM = '7T'
052300180122     C                   WHEN      VABCCM = 0058967
052400180122     C                   EVAL      VABLNP = 005
052500180122     C                   EVAL      VABFGS = 005
052600180122     C                   EVAL      VATLNP = 005
052700180122     C                   EVAL      VATFGS = 005
052800180122     C                   EVAL      VABCTR = 001
052900180124     C                   EVAL      VABCTM = '7T'
053000110408     C                   ENDSL
053100110408     C*
053200110127     C* NSP / RMN
053300101119     C                   IF        *IN51 = *off
053400110412     C                   EVAL      PiStr=%subst(vindta:1:2) +
053500110412     C                                   %subst(vindta:6:5)
053600100730     C                   EXSR      CHKNUM
053700100730     C                   IF        PiInt=*on
053800100730     C                   Z-ADD     PiVal         VABRMN
053900100928     C  N60              Z-ADD     PiVal         VABNSP
054000100928     C  N60              Z-ADD     PiVal         VATNSP
054100100730     C                   ELSE
054200100730     C                   SETON                                        32
054300100730     C                   Z-ADD     1             VABRMN
054400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054500101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
054600100730     C                   ENDIF
054700101119     C*
054800101119     C                   ELSE
054900101119     C*
055000110412     C                   EVAL      PiStr=%subst(vindta:1:2) +
055100110412     C                                   %subst(vindta:6:5)
055200101119     C                   EXSR      CHKNUM
055300101119     C                   IF        PiInt=*on
055400101119     C  N60              Z-ADD     PiVal         VATNSP
055500101119     C                   ELSE
055600101119     C                   SETON                                        32
055700101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800101119     C                             + ' ' + 'VATNSP'
055900101119     C                   ENDIF
056000101119     C                   ENDIF
056100110201     C* NCL
056200110408     C                   EVAL      PiStr=%trim(%subst(vindta:150:5))
056300100928     C                   EXSR      CHKNUM
056400100928     C                   IF        PiInt=*on
056500110325     C                   Z-ADD     PiVal         VABNCL
056600100928     C                   ELSE
056700101119     C  N51              SETON                                        32
056800101119     C  N51              Z-ADD     *zeros        VABNCL
056900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057000100928     C                             + ' ' + 'VABNCL'
057100100928     C                   ENDIF
057200100729     C* PKB
057300110408     C                   IF        %subst(vindta:155:8) <> *all'0' AND
057400110408     C                             %subst(vindta:155:8) <> *blanks
057500110408     C                   EVAL      PiStr=%trim(%subst(vindta:155:8))
057600100729     C                   EXSR      CHKNUM
057700100729     C                   IF        PiNum=*on
057800110325     C                   Z-ADD(H)  PiVal         VABPKB
057900100729     C                   ELSE
058000101119     C  N51              SETON                                        32
058100101119     C  N51              Z-ADD     *zeros        VABPKB
058200101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
058300100729     C                             + ' ' + 'VABPKB'
058400100729     C                   ENDIF
058500110325     C                   ENDIF
058600110325     C* VLB
058700110408     C                   IF        %subst(vindta:165:8) <> *all'0' AND
058800110408     C                             %subst(vindta:165:8) <> *blanks
058900110408     C                   EVAL      PiStr=%trim(%subst(vindta:165:8))
059000110325     C                   EXSR      CHKNUM
059100110325     C                   IF        PiNum=*on
059200110325     C                   Z-ADD(H)  PiVal         VABVLB
059300110325     C                   ELSE
059400110325     C  N51              SETON                                        32
059500110325     C  N51              Z-ADD     *zeros        VABVLB
059600110325     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
059700110325     C                             + ' ' + 'VABVLB'
059800110325     C                   ENDIF
059900110325     C                   ENDIF
060000100729     C*
060100100729     C* Se provincia nn valorizzata la reperisco
060200100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
060300100729     C                   IF        VABNZD  = *blanks AND
060400100729     C                             VABPRD  = *blanks AND
060500100729     C                             VABCAD <> *blanks
060600100729     C                   CLEAR                   TISI95DS
060700100729     C                   EVAL      I95TCN = '3'
060800100729     C                   Z-ADD     datcor        I95DAT
060900100729     C                   EVAL      I95NAR = VABNZD
061000100729     C                   EVAL      I95CAP = VABCAD
061100100729     C                   EVAL      I95LOC = VABLOD
061200100729     C                   CALL      'TISI95R'
061300100729     C                   PARM                    TISI95DS
061400100729     C                   EVAL      VABPRD = O95PRV
061500100729     C                   ENDIF
061600110503     C* DCR
061700110503     C                   IF        %subst(vindta:254:8) <> *all'0' AND
061800110503     C                             %subst(vindta:254:8) <> *blanks
061900110503     C                   EVAL      PiStr=%trim(%subst(vindta:254:8))
062000110503     C                   EXSR      CHKNUM
062100110503     C                   IF        PiInt=*on
062200110503     C                   Z-ADD(H)  PiVal         VABDCR
062300110503     C                   ELSE
062400110503     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
062500110503     C                             + ' ' + 'VABDCR'
062600110503     C                   ENDIF
062700110503     C                   ENDIF
062800171121     C* CAS
062900171121     C                   IF        %subst(vindta:349:14)<>*zeros and
063000171121     C                             %subst(vindta:349:14)<>*blank and
063100171121     C                             %trim(%subst(vindta:349:14))<>'%BT%' and
063200171121     C                             %trim(%subst(vindta:349:14))<>'0.00'
063300171121     C                   EVAL      FlgCAS = '1'
063400171121     C                   EVAL      VABVCA = %trim(%subst(vindta:365:3))
063500171121     C                   EVAL      VABTIC = %trim(%subst(vindta:363:2))
063600171121     C                   EVAL      PiStr=%trim(%subst(vindta:349:14))
063700171121     C                   EXSR      CHKNUM
063800171121     C                   IF        PiNum=*on
063900171121     C                   EVAL(H)   VABCAS = PiVal
064000171121     C                   ELSE
064100171121     C  N51              SETON                                        32
064200171121     C  N51              Z-ADD     *zeros        VABCAS
064300171121     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
064400171121     C                             + ' ' + 'VABCAS'
064500171121     C                   ENDIF
064600171121     C                   ENDIF
064700100729     C*
064800100729     C* Considerazioni finali su CBO/CAS
064900100729     C                   IF        FlgCAS = '1'
065000100729     C                   IF        VABCBO = '1'
065100100729     C                   EVAL      VABCBO = '4'
065200100729     C                   ENDIF
065300100729     C                   IF        VABCBO = '2'
065400100729     C                   EVAL      VABCBO = '6'
065500100729     C                   ENDIF
065600100729     C                   ENDIF
065700010202     C*
065800000801     C* Ebbene...
065900000801     C                   ADD       1             §CTRMO
066000070530     C                   IF        *in31 <> *zeros OR
066100070530     C                             *in32 <> *zeros
066200000801     C                   ADD       1             §CTRNO
066300020725     C                   EVAL      x_vinflg = '2'
066400000801     C                   ELSE
066500010201     C                   ADD       1             §CTROKVB
066600000801     C                   ENDIF
066700000801     C*
066800000801     C                   ENDSR
066900100127     C*----------------------------------------------------*
067000100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
067100100127     C*----------------------------------------------------*
067200100127     C     WRIVAT_A      BEGSR
067300100127     C*
067400100127     C* Valorizzo l buffer di scrittura del FIVAT
067500100127     C* - TIPO RECORD "A"
067600101119     C***                eval      VATTRC = 'A'
067700101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
067800101119     C***                exsr      wrivat
067900100127     C*
068000100127     C                   ENDSR
068100010201     C*----------------------------------------------------*
068200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
068300010201     C*----------------------------------------------------*
068400071003     C     WRIVAT_B      BEGSR
068500010201     C*
068600021113     C* Valorizzo l buffer di scrittura del FIVAT
068700070928     C* - TIPO RECORD "B"
068800110325     C                   eval      VATTRC = 'B'
068900110325     C                   eval      VATNOT = %trim(%subst(vindta:206:15))
069000110325     C                   exsr      wrivat
069100010201     C*
069200010201     C                   ENDSR
069300071003     C*----------------------------------------------------*
069400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
069500071003     C*----------------------------------------------------*
069600071003     C     WRIVAT_E      BEGSR
069700071003     C*
069800071003     C* Valorizzo l buffer di scrittura del FIVAT
069900071003     C* - TIPO RECORD "E"
070000100729     C*
070100100928     C                   eval      VATTRC = 'E'
070200150122     C*
070300150122     C* Sviluppati i "CHI SONO" in relazione al numero colli
070400150122     C*
070500150122     C     1             DO        VABNCL        wNumCollo         4 0
070600150122     C*** da analisi del POC il progressivo colli è max 4 char e deve ccontiene gli zeri non
070700150122     C*** significativi
070800150122     C                   eval      VATNOT = %trim(%subst(vindta:1:10)) +
070900150122     C                                %editc(wNumCollo:'X')
071000150122     C                   exsr      wrivat
071100150122     C                   ENDDO
071200071003     C*
071300071003     C                   ENDSR
071400160122     C*----------------------------------------------------*
071500160122     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" / "J"
071600160122     C*----------------------------------------------------*
071700160122     C     WRIVAT_IJ     BEGSR
071800160122     C*
071900160122     C                   MOVEL     *blanks       wEMAIL           70
072000160122     C                   eval      wEMAIL = %trim(%subst(vindta:263:70))
072100160122     C*
072200160122     C* Valorizzo l buffer di scrittura del FIVAT
072300160122     C* - TIPO RECORD "I"
072400160122     C                   eval      VATTRC = 'I'
072500160122     C                   eval      VATNOT = %subst(wEMAIL:1:35)
072600160122     C                   exsr      wrivat
072700160122     C*
072800160122     C* Valorizzo l buffer di scrittura del FIVAT
072900160122     C* - TIPO RECORD "J"
073000160122     C                   eval      VATTRC = 'J'
073100160122     C                   eval      VATNOT = %subst(wEMAIL:1+35:35)
073200160122     C                   exsr      wrivat
073300160122     C*
073400160122     C                   ENDSR
073500160122     C*----------------------------------------------------*
073600160122     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
073700160122     C*----------------------------------------------------*
073800160122     C     WRIVAT_S      BEGSR
073900160122     C*
074000160122     C* Valorizzo l buffer di scrittura del FIVAT
074100160122     C* - TIPO RECORD "S"
074200160122     C                   eval      VATTRC = 'S'
074300160122     C                   eval      VATNOT = %trim(%subst(vindta:333:16))
074400160122     C                   exsr      wrivat
074500160122     C*
074600160122     C                   ENDSR
074700100127     C*----------------------------------------------------*
074800100127     C*  SCARICO BUFFER FIVAT
074900100127     C*----------------------------------------------------*
075000100127     C     WRIVAT        BEGSR
075100100127     C*
075200100127     C                   if        vatNOT <> *blanks
075300100127     C                   ADD       1             §CTROKVT
075400100127     C                   write     FIVAT000
075500100127     C                   endif
075600100127     C*
075700100127     C                   ENDSR
075800020904
075900020904
076000010202     C*----------------------------------------------------*
076100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
076200010202     C*----------------------------------------------------*
076300100525     C     PREVASX       BEGSR
076400100525     C*
076500021113     C* Compongo il nome del membro da dare al FIVATWWR
076600010202     C                   eval      parmbr = vlrhdl
076700010202     C                   movel     'M'           parmbr
076800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
076900010202     C                   eval      paropz = '1'
077000100525     C*
077100010202     C* Effettuo la chiamata al CLLE preposto
077200100525     C  N51              call(e)   'TITVVTC'
077300010202     C                   parm                    parccm
077400010202     C                   parm                    parmbr
077500010202     C                   parm                    paropz
077600100525     C   51              call(e)   'TITVVBC'
077700100525     C                   parm                    parccm
077800100525     C                   parm                    parmbr
077900100525     C                   parm                    paropz
078000100525     C*
078100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
078200010202     C                   if        %error
078300010202     C                   movel     '1'           chkcall
078400010202     C                   else
078500010202     C                   movel     '0'           chkcall
078600010202     C                   endif
078700010202     C*
078800010202     C                   ENDSR
078900000801     C*----------------------------------------------------*
079000000801     C*  CONTROLLO NUMERICITA' CAMPI
079100000801     C*----------------------------------------------------*
079200000801     C     CHKNUM        BEGSR
079300081113     C*
079400081113     C                   IF        PiDecChr = *blanks
079500110408     C                   EVAL      PiDecChr = '.'
079600081113     C                   ENDIF
079700091223     C*
079800081113     C                   callp(e)  UBISNUM_Check(PiStr
079900081113     C                                          :PiDecChr
080000081113     C                                          :PiVal
080100081113     C                                          :PiNum
080200081113     C                                          :PiInt)
080300081113     C*
080400000801     C                   IF        %error
080500000801     C                   EVAL      PiInt=*off
080600000801     C                   ENDIF
080700000801     C*
080800000801     C                   ENDSR
080900000801     C***
081000011113
081100011113
081200000801
081300000801
081400990920      /TITLE Invio dei dati al punto operativo.
081500010202     C     invio         BEGSR
081600990920     C*
081700021113     C* 1° invio FIVAT
081800010201     C                   reset                   dscmz
081900150130     C                   if        vlrpoi = 999
082000150130     C                   move      vatfgs        cmzdst
082100150130     C                   else
082200150130     C                   move      vlrpoi        cmzdst
082300150130     C                   endif
082400021113     C                   eval      cmzfld = 'FIVATWWR'
082500010201     C                   eval      cmzmbd = vlrhdl
082600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082700021009     C***                if        prmfir = *blanks
082800021113     C                   eval      cmzfla = 'FIVAT00F'
082900021113     C                   eval      cmzmba = 'FIVAT00F'
083000021009     C***                else
083100021009     C***                eval      cmzfla = prmfir
083200021009     C***                eval      cmzmba = prmfir
083300021009     C***                endif
083400010201     C                   eval      cmznrr = *zeros
083500020305     C                   move      §ctrokvt      cmznrr
083600021018     C                   eval      cmzlba = vlrfl1
083700010201     C                   call(e)   'TIS711C'
083800010201     C                   parm                    dscmz
083900010201     C                   parm      *blanks       esito
084000010205     C                   if        %error
084100010205     C                             or cmzerr = '1'
084200010205     C                             or esito  = '1'
084300010205     C                   eval      wrkesito = '3'
084400010205     C                   else
084500010201     C*
084600021113     C* 2° invio FIVAB
084700100525     C                   if        *in51 = *off
084800010201     C                   reset                   dscmz
084900150130     C                   if        vlrpoi = 999
085000150130     C                   move      vabfgs        cmzdst
085100150130     C                   else
085200150130     C                   move      vlrpoi        cmzdst
085300150130     C                   endif
085400010201     C                   eval      cmzfld = vlrfou
085500010201     C                   eval      cmzmbd = vlrhdl
085600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085700021009     C***                if        prmfir = *blanks
085800021113     C                   eval      cmzfla = 'FIVAB00F'
085900021113     C                   eval      cmzmba = 'FIVAB00F'
086000021009     C***                else
086100021009     C***                eval      cmzfla = prmfir
086200021009     C***                eval      cmzmba = prmfir
086300021009     C***                endif
086400010201     C                   eval      cmznrr = *zeros
086500010201     C                   move      §ctrokvb      cmznrr
086600021018     C                   eval      cmzlba = vlrfl1
086700010201     C                   call(e)   'TIS711C'
086800010201     C                   parm                    dscmz
086900010201     C                   parm      *blanks       esito
087000010201     C                   if        %error
087100010201     C                             or cmzerr = '1'
087200010201     C                             or esito  = '1'
087300010201     C                   eval      wrkesito = '3'
087400010201     C                   endif
087500100525     C                   endif
087600010205     C                   endif
087700990920     C*
087800000613     C                   ENDSR
087900000613     C***
088000070411
088100070411     C     *pssr         BEGSR
088200070411     C*
088300070411     C                   if        %open(tivin00r)
088400070411     C                   close     tivin00r
088500070411     C                   endif
088600070411     C                   if        %open(fivabwwr)
088700070411     C                   close     fivabwwr
088800070411     C                   endif
088900070411     C                   if        %open(fivatwwr)
089000070411     C                   close     fivatwwr
089100070411     C                   endif
089200070411     C*
089300070411     C* Effettuo la chiamata al CLLE preposto
089400100525     C  N51              call(e)   'TITVVTC'
089500100525     C                   parm                    parccm
089600100525     C                   parm                    parmbr
089700100525     C                   parm      '2'           paropz
089800100525     C   51              call(e)   'TITVVBC'
089900100525     C                   parm                    parccm
090000100525     C                   parm                    parmbr
090100100525     C                   parm      '2'           paropz
090200070411     C*
090300070411     C                   eval      wrkesito = '2'
090400070411     C*
090500070411     C                   seton                                        LR
090600070411     C*
090700070411     C                   ENDSR     '*CANCL'
090800070411     C***
090900070411
091000990910
091100000613     C     *inzsr        BEGSR
091200990910     C*
091300990910     C     *entry        plist
091400990920     C                   parm                    tivlrds
091500990921     C                   parm      wrkesito      esito
091600000724     C                   parm                    prmlit
091700000710     C                   parm                    prmfir
091800000613     C*
091900000830     C* CALCOLA LA DATA CORRENTE
092000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
092100091223     C                   eval      datcor = %dec(%date() : *ISO)
092200000830     C*
092300000613     C                   ENDSR
092400000613     C***
