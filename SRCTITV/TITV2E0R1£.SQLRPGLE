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
051600110408     C                   ENDSL
051700110408     C*
051800110127     C* NSP / RMN
051900101119     C                   IF        *IN51 = *off
052000110412     C                   EVAL      PiStr=%subst(vindta:1:2) +
052100110412     C                                   %subst(vindta:6:5)
052200100730     C                   EXSR      CHKNUM
052300100730     C                   IF        PiInt=*on
052400100730     C                   Z-ADD     PiVal         VABRMN
052500100928     C  N60              Z-ADD     PiVal         VABNSP
052600100928     C  N60              Z-ADD     PiVal         VATNSP
052700100730     C                   ELSE
052800100730     C                   SETON                                        32
052900100730     C                   Z-ADD     1             VABRMN
053000100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053100101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
053200100730     C                   ENDIF
053300101119     C*
053400101119     C                   ELSE
053500101119     C*
053600110412     C                   EVAL      PiStr=%subst(vindta:1:2) +
053700110412     C                                   %subst(vindta:6:5)
053800101119     C                   EXSR      CHKNUM
053900101119     C                   IF        PiInt=*on
054000101119     C  N60              Z-ADD     PiVal         VATNSP
054100101119     C                   ELSE
054200101119     C                   SETON                                        32
054300101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054400101119     C                             + ' ' + 'VATNSP'
054500101119     C                   ENDIF
054600101119     C                   ENDIF
054700110201     C* NCL
054800110408     C                   EVAL      PiStr=%trim(%subst(vindta:150:5))
054900100928     C                   EXSR      CHKNUM
055000100928     C                   IF        PiInt=*on
055100110325     C                   Z-ADD     PiVal         VABNCL
055200100928     C                   ELSE
055300101119     C  N51              SETON                                        32
055400101119     C  N51              Z-ADD     *zeros        VABNCL
055500101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055600100928     C                             + ' ' + 'VABNCL'
055700100928     C                   ENDIF
055800100729     C* PKB
055900110408     C                   IF        %subst(vindta:155:8) <> *all'0' AND
056000110408     C                             %subst(vindta:155:8) <> *blanks
056100110408     C                   EVAL      PiStr=%trim(%subst(vindta:155:8))
056200100729     C                   EXSR      CHKNUM
056300100729     C                   IF        PiNum=*on
056400110325     C                   Z-ADD(H)  PiVal         VABPKB
056500100729     C                   ELSE
056600101119     C  N51              SETON                                        32
056700101119     C  N51              Z-ADD     *zeros        VABPKB
056800101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056900100729     C                             + ' ' + 'VABPKB'
057000100729     C                   ENDIF
057100110325     C                   ENDIF
057200110325     C* VLB
057300110408     C                   IF        %subst(vindta:165:8) <> *all'0' AND
057400110408     C                             %subst(vindta:165:8) <> *blanks
057500110408     C                   EVAL      PiStr=%trim(%subst(vindta:165:8))
057600110325     C                   EXSR      CHKNUM
057700110325     C                   IF        PiNum=*on
057800110325     C                   Z-ADD(H)  PiVal         VABVLB
057900110325     C                   ELSE
058000110325     C  N51              SETON                                        32
058100110325     C  N51              Z-ADD     *zeros        VABVLB
058200110325     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
058300110325     C                             + ' ' + 'VABVLB'
058400110325     C                   ENDIF
058500110325     C                   ENDIF
058600100729     C*
058700100729     C* Se provincia nn valorizzata la reperisco
058800100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
058900100729     C                   IF        VABNZD  = *blanks AND
059000100729     C                             VABPRD  = *blanks AND
059100100729     C                             VABCAD <> *blanks
059200100729     C                   CLEAR                   TISI95DS
059300100729     C                   EVAL      I95TCN = '3'
059400100729     C                   Z-ADD     datcor        I95DAT
059500100729     C                   EVAL      I95NAR = VABNZD
059600100729     C                   EVAL      I95CAP = VABCAD
059700100729     C                   EVAL      I95LOC = VABLOD
059800100729     C                   CALL      'TISI95R'
059900100729     C                   PARM                    TISI95DS
060000100729     C                   EVAL      VABPRD = O95PRV
060100100729     C                   ENDIF
060200110503     C* DCR
060300110503     C                   IF        %subst(vindta:254:8) <> *all'0' AND
060400110503     C                             %subst(vindta:254:8) <> *blanks
060500110503     C                   EVAL      PiStr=%trim(%subst(vindta:254:8))
060600110503     C                   EXSR      CHKNUM
060700110503     C                   IF        PiInt=*on
060800110503     C                   Z-ADD(H)  PiVal         VABDCR
060900110503     C                   ELSE
061000110503     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
061100110503     C                             + ' ' + 'VABDCR'
061200110503     C                   ENDIF
061300110503     C                   ENDIF
061400171121     C* CAS
061500171121     C                   IF        %subst(vindta:349:14)<>*zeros and
061600171121     C                             %subst(vindta:349:14)<>*blank and
061700171121     C                             %trim(%subst(vindta:349:14))<>'%BT%' and
061800171121     C                             %trim(%subst(vindta:349:14))<>'0.00'
061900171121     C                   EVAL      FlgCAS = '1'
062000171121     C                   EVAL      VABVCA = %trim(%subst(vindta:365:3))
062100171121     C                   EVAL      VABTIC = %trim(%subst(vindta:363:2))
062200171121     C                   EVAL      PiStr=%trim(%subst(vindta:349:14))
062300171121     C                   EXSR      CHKNUM
062400171121     C                   IF        PiNum=*on
062500171121     C                   EVAL(H)   VABCAS = PiVal
062600171121     C                   ELSE
062700171121     C  N51              SETON                                        32
062800171121     C  N51              Z-ADD     *zeros        VABCAS
062900171121     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
063000171121     C                             + ' ' + 'VABCAS'
063100171121     C                   ENDIF
063200171121     C                   ENDIF
063300100729     C*
063400100729     C* Considerazioni finali su CBO/CAS
063500100729     C                   IF        FlgCAS = '1'
063600100729     C                   IF        VABCBO = '1'
063700100729     C                   EVAL      VABCBO = '4'
063800100729     C                   ENDIF
063900100729     C                   IF        VABCBO = '2'
064000100729     C                   EVAL      VABCBO = '6'
064100100729     C                   ENDIF
064200100729     C                   ENDIF
064300010202     C*
064400000801     C* Ebbene...
064500000801     C                   ADD       1             §CTRMO
064600070530     C                   IF        *in31 <> *zeros OR
064700070530     C                             *in32 <> *zeros
064800000801     C                   ADD       1             §CTRNO
064900020725     C                   EVAL      x_vinflg = '2'
065000000801     C                   ELSE
065100010201     C                   ADD       1             §CTROKVB
065200000801     C                   ENDIF
065300000801     C*
065400000801     C                   ENDSR
065500100127     C*----------------------------------------------------*
065600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
065700100127     C*----------------------------------------------------*
065800100127     C     WRIVAT_A      BEGSR
065900100127     C*
066000100127     C* Valorizzo l buffer di scrittura del FIVAT
066100100127     C* - TIPO RECORD "A"
066200101119     C***                eval      VATTRC = 'A'
066300101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
066400101119     C***                exsr      wrivat
066500100127     C*
066600100127     C                   ENDSR
066700010201     C*----------------------------------------------------*
066800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
066900010201     C*----------------------------------------------------*
067000071003     C     WRIVAT_B      BEGSR
067100010201     C*
067200021113     C* Valorizzo l buffer di scrittura del FIVAT
067300070928     C* - TIPO RECORD "B"
067400110325     C                   eval      VATTRC = 'B'
067500110325     C                   eval      VATNOT = %trim(%subst(vindta:206:15))
067600110325     C                   exsr      wrivat
067700010201     C*
067800010201     C                   ENDSR
067900071003     C*----------------------------------------------------*
068000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
068100071003     C*----------------------------------------------------*
068200071003     C     WRIVAT_E      BEGSR
068300071003     C*
068400071003     C* Valorizzo l buffer di scrittura del FIVAT
068500071003     C* - TIPO RECORD "E"
068600100729     C*
068700100928     C                   eval      VATTRC = 'E'
068800150122     C*
068900150122     C* Sviluppati i "CHI SONO" in relazione al numero colli
069000150122     C*
069100150122     C     1             DO        VABNCL        wNumCollo         4 0
069200150122     C*** da analisi del POC il progressivo colli è max 4 char e deve ccontiene gli zeri non
069300150122     C*** significativi
069400150122     C                   eval      VATNOT = %trim(%subst(vindta:1:10)) +
069500150122     C                                %editc(wNumCollo:'X')
069600150122     C                   exsr      wrivat
069700150122     C                   ENDDO
069800071003     C*
069900071003     C                   ENDSR
070000160122     C*----------------------------------------------------*
070100160122     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" / "J"
070200160122     C*----------------------------------------------------*
070300160122     C     WRIVAT_IJ     BEGSR
070400160122     C*
070500160122     C                   MOVEL     *blanks       wEMAIL           70
070600160122     C                   eval      wEMAIL = %trim(%subst(vindta:263:70))
070700160122     C*
070800160122     C* Valorizzo l buffer di scrittura del FIVAT
070900160122     C* - TIPO RECORD "I"
071000160122     C                   eval      VATTRC = 'I'
071100160122     C                   eval      VATNOT = %subst(wEMAIL:1:35)
071200160122     C                   exsr      wrivat
071300160122     C*
071400160122     C* Valorizzo l buffer di scrittura del FIVAT
071500160122     C* - TIPO RECORD "J"
071600160122     C                   eval      VATTRC = 'J'
071700160122     C                   eval      VATNOT = %subst(wEMAIL:1+35:35)
071800160122     C                   exsr      wrivat
071900160122     C*
072000160122     C                   ENDSR
072100160122     C*----------------------------------------------------*
072200160122     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
072300160122     C*----------------------------------------------------*
072400160122     C     WRIVAT_S      BEGSR
072500160122     C*
072600160122     C* Valorizzo l buffer di scrittura del FIVAT
072700160122     C* - TIPO RECORD "S"
072800160122     C                   eval      VATTRC = 'S'
072900160122     C                   eval      VATNOT = %trim(%subst(vindta:333:16))
073000160122     C                   exsr      wrivat
073100160122     C*
073200160122     C                   ENDSR
073300100127     C*----------------------------------------------------*
073400100127     C*  SCARICO BUFFER FIVAT
073500100127     C*----------------------------------------------------*
073600100127     C     WRIVAT        BEGSR
073700100127     C*
073800100127     C                   if        vatNOT <> *blanks
073900100127     C                   ADD       1             §CTROKVT
074000100127     C                   write     FIVAT000
074100100127     C                   endif
074200100127     C*
074300100127     C                   ENDSR
074400020904
074500020904
074600010202     C*----------------------------------------------------*
074700100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
074800010202     C*----------------------------------------------------*
074900100525     C     PREVASX       BEGSR
075000100525     C*
075100021113     C* Compongo il nome del membro da dare al FIVATWWR
075200010202     C                   eval      parmbr = vlrhdl
075300010202     C                   movel     'M'           parmbr
075400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
075500010202     C                   eval      paropz = '1'
075600100525     C*
075700010202     C* Effettuo la chiamata al CLLE preposto
075800100525     C  N51              call(e)   'TITVVTC'
075900010202     C                   parm                    parccm
076000010202     C                   parm                    parmbr
076100010202     C                   parm                    paropz
076200100525     C   51              call(e)   'TITVVBC'
076300100525     C                   parm                    parccm
076400100525     C                   parm                    parmbr
076500100525     C                   parm                    paropz
076600100525     C*
076700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076800010202     C                   if        %error
076900010202     C                   movel     '1'           chkcall
077000010202     C                   else
077100010202     C                   movel     '0'           chkcall
077200010202     C                   endif
077300010202     C*
077400010202     C                   ENDSR
077500000801     C*----------------------------------------------------*
077600000801     C*  CONTROLLO NUMERICITA' CAMPI
077700000801     C*----------------------------------------------------*
077800000801     C     CHKNUM        BEGSR
077900081113     C*
078000081113     C                   IF        PiDecChr = *blanks
078100110408     C                   EVAL      PiDecChr = '.'
078200081113     C                   ENDIF
078300091223     C*
078400081113     C                   callp(e)  UBISNUM_Check(PiStr
078500081113     C                                          :PiDecChr
078600081113     C                                          :PiVal
078700081113     C                                          :PiNum
078800081113     C                                          :PiInt)
078900081113     C*
079000000801     C                   IF        %error
079100000801     C                   EVAL      PiInt=*off
079200000801     C                   ENDIF
079300000801     C*
079400000801     C                   ENDSR
079500000801     C***
079600011113
079700011113
079800000801
079900000801
080000990920      /TITLE Invio dei dati al punto operativo.
080100010202     C     invio         BEGSR
080200990920     C*
080300021113     C* 1° invio FIVAT
080400010201     C                   reset                   dscmz
080500150130     C                   if        vlrpoi = 999
080600150130     C                   move      vatfgs        cmzdst
080700150130     C                   else
080800150130     C                   move      vlrpoi        cmzdst
080900150130     C                   endif
081000021113     C                   eval      cmzfld = 'FIVATWWR'
081100010201     C                   eval      cmzmbd = vlrhdl
081200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081300021009     C***                if        prmfir = *blanks
081400021113     C                   eval      cmzfla = 'FIVAT00F'
081500021113     C                   eval      cmzmba = 'FIVAT00F'
081600021009     C***                else
081700021009     C***                eval      cmzfla = prmfir
081800021009     C***                eval      cmzmba = prmfir
081900021009     C***                endif
082000010201     C                   eval      cmznrr = *zeros
082100020305     C                   move      §ctrokvt      cmznrr
082200021018     C                   eval      cmzlba = vlrfl1
082300010201     C                   call(e)   'TIS711C'
082400010201     C                   parm                    dscmz
082500010201     C                   parm      *blanks       esito
082600010205     C                   if        %error
082700010205     C                             or cmzerr = '1'
082800010205     C                             or esito  = '1'
082900010205     C                   eval      wrkesito = '3'
083000010205     C                   else
083100010201     C*
083200021113     C* 2° invio FIVAB
083300100525     C                   if        *in51 = *off
083400010201     C                   reset                   dscmz
083500150130     C                   if        vlrpoi = 999
083600150130     C                   move      vabfgs        cmzdst
083700150130     C                   else
083800150130     C                   move      vlrpoi        cmzdst
083900150130     C                   endif
084000010201     C                   eval      cmzfld = vlrfou
084100010201     C                   eval      cmzmbd = vlrhdl
084200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
084300021009     C***                if        prmfir = *blanks
084400021113     C                   eval      cmzfla = 'FIVAB00F'
084500021113     C                   eval      cmzmba = 'FIVAB00F'
084600021009     C***                else
084700021009     C***                eval      cmzfla = prmfir
084800021009     C***                eval      cmzmba = prmfir
084900021009     C***                endif
085000010201     C                   eval      cmznrr = *zeros
085100010201     C                   move      §ctrokvb      cmznrr
085200021018     C                   eval      cmzlba = vlrfl1
085300010201     C                   call(e)   'TIS711C'
085400010201     C                   parm                    dscmz
085500010201     C                   parm      *blanks       esito
085600010201     C                   if        %error
085700010201     C                             or cmzerr = '1'
085800010201     C                             or esito  = '1'
085900010201     C                   eval      wrkesito = '3'
086000010201     C                   endif
086100100525     C                   endif
086200010205     C                   endif
086300990920     C*
086400000613     C                   ENDSR
086500000613     C***
086600070411
086700070411     C     *pssr         BEGSR
086800070411     C*
086900070411     C                   if        %open(tivin00r)
087000070411     C                   close     tivin00r
087100070411     C                   endif
087200070411     C                   if        %open(fivabwwr)
087300070411     C                   close     fivabwwr
087400070411     C                   endif
087500070411     C                   if        %open(fivatwwr)
087600070411     C                   close     fivatwwr
087700070411     C                   endif
087800070411     C*
087900070411     C* Effettuo la chiamata al CLLE preposto
088000100525     C  N51              call(e)   'TITVVTC'
088100100525     C                   parm                    parccm
088200100525     C                   parm                    parmbr
088300100525     C                   parm      '2'           paropz
088400100525     C   51              call(e)   'TITVVBC'
088500100525     C                   parm                    parccm
088600100525     C                   parm                    parmbr
088700100525     C                   parm      '2'           paropz
088800070411     C*
088900070411     C                   eval      wrkesito = '2'
089000070411     C*
089100070411     C                   seton                                        LR
089200070411     C*
089300070411     C                   ENDSR     '*CANCL'
089400070411     C***
089500070411
089600990910
089700000613     C     *inzsr        BEGSR
089800990910     C*
089900990910     C     *entry        plist
090000990920     C                   parm                    tivlrds
090100990921     C                   parm      wrkesito      esito
090200000724     C                   parm                    prmlit
090300000710     C                   parm                    prmfir
090400000613     C*
090500000830     C* CALCOLA LA DATA CORRENTE
090600091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
090700091223     C                   eval      datcor = %dec(%date() : *ISO)
090800000830     C*
090900000613     C                   ENDSR
091000000613     C***
