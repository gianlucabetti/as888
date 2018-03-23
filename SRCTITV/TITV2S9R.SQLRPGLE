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
002100140808     D depspe          s             10    INZ(*blanks)
002200140808     D curspe          s             10    INZ(*blanks)
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
003600140808     D  VINSPE                       10
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
007800100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
007900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008000000613     C*
008100010202     C* Effettuo la chiamata al CLLE preposto
008200100525     C  N51              call(e)   'TITVVTC'
008300100525     C                   parm                    parccm
008400100525     C                   parm                    parmbr
008500100525     C                   parm      '2'           paropz
008600100525     C   51              call(e)   'TITVVBC'
008700100525     C                   parm                    parccm
008800100525     C                   parm                    parmbr
008900100525     C                   parm      '2'           paropz
009000070730     C*
009100070730     C* Effettuo lancio TISI95 solo x chiusura
009200070730     C                   CLEAR                   TISI95DS
009300070730     C                   EVAL      I95TLA = 'C'
009400070730     C                   CALL      'TISI95R'
009500070730     C                   PARM                    TISI95DS
009600000616     C*
009700000801     C
009800010201     C                   seton                                        LR
009900990908
010000000801
010100910830     C*--------------------------------------------------------
010200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010300910830     C*--------------------------------------------------------
010400070530     C     RWFILE        BEGSR
010500990910     C*
010600990914     C                   if        not %open(tivin00r)
010700990908     C                   open      tivin00r
010800990914     C                   endif
010900070530     C*
011000100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011100100525     C                   exsr      prevasx
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500100525     C                   if        not %open(fivabwwr)
011600100525     C                   open      fivabwwr
011700100525     C                   endif
011800100525     C*
011900021113     C                   if        not %open(fivatwwr)
012000021113     C                   open      fivatwwr
012100010201     C                   endif
012200080117     C*
012300080117     C                   EXSR      INZVAR
012400080117     C                   EXSR      DEFCAM
012500990910     C*
012600010201     C                   clear                   §CTROKVB          5 0
012700020305     C                   clear                   §CTROKVT          5 0
012800000801     C                   clear                   §CTRMO            5 0
012900000801     C                   clear                   §CTRNO            5 0
013000990910     C*
013100020725     C/EXEC SQL
013200020725     C+ declare C1 cursor for select
013300140808     C+ vindta, vinflg, substr(vindta, 16, 10) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500111020     C+ order by sped
013600020725     C+ for read only
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000020725     C+ open C1
014100020725     C/END-EXEC
014200020725     C
014300020725     C/EXEC SQL
014400070530     C+ Fetch C1 into :INPUT_DS
014500020725     C/END-EXEC
014600020725     C*
014700020725     C                   dow       sqlcod = *zeros
014800990913     C*
014900100310     C                   if        vindta > *blanks
015000000613     C                   add       1             rrnum
015100000801     C*
015200020725     C                   if        vinflg = *blanks
015300020725     C                             or vinflg = '0'
015400020725     C                             or vinflg = '2'
015500000801     C*
015600020725     C                   clear                   x_vinmsg
015700020725     C                   eval      x_vinflg = '1'
015800101119     C*
015900101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016000101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016100010305     C*
016200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016300140808     C                   EVAL      curspe=%trim(%subst(vindta:16:10))
016400010305     C*
016500100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016600080923     C                   if        *in50 = *off
016700101119     C                   exsr      inzvar
016800101119     C                   exsr      defcam
016900071003     C                   exsr      impvab
017000100525     C                   exsr      wrivab
017100100127     C                   exsr      wrivat_a                                     => carico VAT
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300100525     C   51              exsr      wrivat_e                                     => carico VAT
017400071003     C                   else
017500080923     C*
017600071009     C                   if        wDISK = 'D'
017700091223     C                   exsr      repNSP
017800071009     C                   exsr      impvab
017900100412     C                   exsr      wrivab
018000100127     C                   exsr      wrivat_a                                     => carico VAT
018100071009     C                   exsr      wrivat_b                                     => carico VAT
018200071009     C                   exsr      wrivat_e                                     => carico VAT
018300071009     C                   else
018400071009     C*
018500010305     C                   if        depspe = *blanks                             => 1° giro
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700080117     C                   clear                   tivinds
018800091223     C                   exsr      repNSP
018900020305     C                   exsr      impvab
019000100127     C                   exsr      wrivat_a                                     => carico VAT
019100071003     C                   exsr      wrivat_b                                     => carico VAT
019200071003     C   50              exsr      wrivat_e                                     => carico VAT
019300010305     C                   else
019400020725     C                   if        depspe <> curspe                             => rottura di spediz
019500010305     C                   eval      depspe = curspe                              => memorizz. spediz
019600100412     C                   exsr      wrivab
019700080117     C                   clear                   tivinds
019800091223     C                   exsr      repNSP
019900020305     C                   exsr      impvab
020000100127     C                   exsr      wrivat_a                                     => carico VAT
020100071003     C                   exsr      wrivat_b                                     => carico VAT
020200071003     C   50              exsr      wrivat_e                                     => carico VAT
020300020305     C                   else                                                   => x stessa spediz
020400100401     C*
020500100401     C* Se nn richiesta esclusione spedizioni "duplicate"
020600100928     C                   select
020700100928     C                   when      wDUPREC = ' '
020800020305     C                   exsr      impvab
020900100127     C                   exsr      wrivat_a                                     => carico VAT
021000071003     C                   exsr      wrivat_b                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200100928     C                   when      wDUPREC = 'C'
021300100928     C   50              exsr      wrivat_e                                     => carico VAT
021400100928     C                   endsl
021500010305     C                   endif
021600010305     C                   endif
021700010305     C                   endif
021800071003     C                   endif
021900110201     C*
022000071009     C                   endif
022100000905     C*
022200000905     C                   else
022300020725     C                   eval      x_vinflg = '1'
022400000905     C                   endif
022500000905     C*
022600020725     C     VINRRN        chain     tivin000
022700020725     C                   if        %found(tivin00r)
022800020725     C                   eval      y_vinflg = x_vinflg
022900020725     C                   eval      y_vinmsg = x_vinmsg
023000020725     C                   update    tivin000
023100020725     C                   endif
023200020725     C*
023300020725     C/EXEC SQL
023400070530     C+ Fetch C1 into :INPUT_DS
023500020725     C/END-EXEC
023600020725     C*
023700020725     C                   enddo
023800020725     C*
023900020725     C/EXEC SQL
024000020725     C+ close C1
024100020725     C/END-EXEC
024200000905     C*
024300020305     C* Scarico i VAB rimasti "in sospeso"
024400071009     C                   if        wDISK = 'C'
024500100412     C                   exsr      wrivab
024600071009     C                   endif
024700010202     C*
024800010202     C                   endif
024900990910
025000990910     C* Se non ci sono record con errori ...
025100000710     C                   if        §ctrno = 0
025200990910     C* ... restituisco esito OK.
025300990921     C                   eval      wrkesito = '0'
025400990910     C                   else
025500100525     C                   if        §ctrokvb > 0 OR
025600100525     C                             §ctrokvt > 0
025700990921     C                   eval      wrkesito = '1'
025800000710     C                   else
025900000710     C                   eval      wrkesito = '2'
026000990910     C                   endif
026100000710     C                   endif
026200990910     C*
026300990914     C                   if        %open(tivin00r)
026400990908     C                   close     tivin00r
026500990914     C                   endif
026600021113     C                   if        %open(fivabwwr)
026700021113     C                   close     fivabwwr
026800990914     C                   endif
026900021113     C                   if        %open(fivatwwr)
027000021113     C                   close     fivatwwr
027100010201     C                   endif
027200990910     C*
027300100525     C                   if        §ctrokvb > 0 OR
027400100525     C                             §ctrokvt > 0
027500000724     C                             and vlrpoi <> *zeros
027600010202     C                   exsr      invio
027700990920     C                   endif
027800990920     C*
027900910830     C                   ENDSR
028000000613     C***
028100100412
028200100412
028300010305
028400010305     C*----------------------------------------------------*
028500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028600010305     C*----------------------------------------------------*
028700020305     C     WRIVAB        BEGSR
028800100412     C*
028900100412     C* Gestisco l'eventuale rottura x numero spedizione
029000100412     C                   if        VABRMA = *blanks
029100100412     C                   movel(P)  VABRMN        VABRMA
029200100412     C                   endif
029300100412     C*
029400100525     C                   if        *in51 = *off and *in31 = *off
029500100525     C                   write     fivab000                                     => scarico il VAB
029600100525     C                   endif
029700010305     C*
029800010305     C                   ENDSR
029900990920
030000000801     C*----------------------------------------------------*
030100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200000801     C*----------------------------------------------------*
030300010201     C     INZVAR        BEGSR
030400000801     C*
030500010201     C                   Z-ADD     *zeros        Num5_0
030600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030700020725     C                   MOVEL     '0'           FlgCAS            1
030800000801     C*
030900000801     C                   ENDSR
031000000801     C*----------------------------------------------------*
031100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031200000801     C*----------------------------------------------------*
031300020904     C     DEFCAM        BEGSR
031400080923     C*
031500100928     C                   SETOFF                                       505160
031600000801     C*
031700021113     C                   CLEAR                   FIVAB000
031800021113     C                   CLEAR                   FIVAT000
031900070730     C* Imposto i valori di default...
032000140808     C                   EVAL      VABCCM = 1411894
032100140808     C                   EVAL      VATCCM = 1411894
032200140808     C                   EVAL      VABLNP = 141
032300140808     C                   EVAL      VATLNP = 141
032400140808     C                   EVAL      VABTSP = 'C'
032500140808     C                   EVAL      VABCTM = '7K'
032600100525     C                   EVAL      VABCBO = '1'
032700070222     C* ... e poi verifico se sono stati passati come parametri
032800070222     C                   IF        vlrppt > *blanks
032900100525     C*
033000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033200070222     C                   EXSR      CHKNUM
033300070222     C                   IF        PiInt=*on
033400070222     C                   Z-ADD     PiVal         VABCCM
033500070222     C                   Z-ADD     PiVal         VATCCM
033600070222     C                   ENDIF
033700070222     C                   ENDIF
033800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034000070222     C                   EXSR      CHKNUM
034100070222     C                   IF        PiInt=*on
034200070222     C                   Z-ADD     PiVal         VABLNP
034300070222     C                   Z-ADD     PiVal         VATLNP
034400070222     C                   ENDIF
034500070222     C                   ENDIF
034600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034800070222     C                   EXSR      CHKNUM
034900070222     C                   IF        PiInt=*on
035000070222     C                   Z-ADD     PiVal         VABCTR
035100070222     C                   ENDIF
035200070928     C                   ENDIF
035300071009     C                   MOVEL     *blanks       wDISK             1
035400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035500071009     C                   IF        wDISK <> *blanks
035600070928     C                   SETON                                        50
035700070222     C                   ENDIF
035800100525     C                   IF        wDISK  = 'T'
035900100525     C                   SETOFF                                       50
036000100525     C                   SETON                                        51
036100100525     C                   ENDIF
036200100401     C                   MOVEL     *blanks       wDUPREC           1
036300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036600100525     C                   ENDIF
036700100525     C*
036800070222     C                   ENDIF
036900071009     C*
037000071009     C   50              EVAL      VABCTM = '7Q'
037100000801     C*
037200000801     C                   ENDSR
037300091223     C*----------------------------------------------------*
037400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037500091223     C*----------------------------------------------------*
037600091223     C     REPNSP        BEGSR
037700091223     C*
037800091223     C                   EXSR      INZVAR
037900091223     C                   EXSR      DEFCAM
038000100928     C*
038100100928     C                   SETON                                        60
038200091223     C*
038300091223     C* NSP => Stacco un numeratore da AZNUM
038400091223     C                   clear                   TRUL33DS
038500091223     C                   eval      I33OPE = *zeros
038600091223     C                   eval      I33CNU = 302
038700091223     C                   eval      I33NUM = 1
038800091223     C                   movel     TRUL33DS      KPJBU
038900091223     C                   call      'TRUL33R'
039000091223     C                   parm                    KPJBA
039100091223     C                   movel     KPJBU         TRUL33DS
039200091223     C                   if        O33ERR = *zeros
039300091223     C                   z-add     O33NRF        VABNSP
039400091223     C                   z-add     O33NRF        VATNSP
039500091223     C                   else
039600091223     C                   SETON                                        31
039700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039800091223     C                             + ' ' + 'VABNSP VATNSP'
039900091223     C                   endif
040000091223     C*
040100091223     C                   ENDSR
040200000801     C*----------------------------------------------------*
040300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040400000801     C*----------------------------------------------------*
040500100730     C     IMPVAB        BEGSR
040600070530     C*
040700070530     C                   SETOFF                                       3132
040800070928     C*
040900070928     C                   MOVE(P)   vlrpoi        VABFGS
041000070928     C                   MOVE(P)   vlrpoi        VATFGS
041100070928     C*
041200070928     C                   MOVEL     datcor        VABAAS
041300070928     C                   MOVEL     datcor        VATAAS
041400070928     C                   MOVE      datcor        VABMGS
041500100729     C*
041600100729     C* Reperimento campi ALFA
041700140808     C                   EVAL      VABRSD=%trim(%subst(vindta:133:30))
041800140808     C                   EVAL      VABIND=%trim(%subst(vindta:163:30))
041900140818     C                   EVAL      VABLOD=%trim(%subst(vindta:198:30))
042000140808     C                   EVAL      VABCAD=%trim(%subst(vindta:193:5))
042100140808     C                   EVAL      VABPRD=%trim(%subst(vindta:228:2))
042200140808     C                   EVAL      VABNZD=%trim(%subst(vindta:232:2))
042300140808     C                   SELECT
042400140808     C                   WHEN      VABNZD = 'IT'
042500140808     C                   EVAL      VABNZD = *blanks
042600140818     C                   EVAL      VABCTR = 001
042700140808     C                   WHEN      VABNZD = 'BE'
042800140808     C                   EVAL      VABNZD = 'B'
042900140808     C                   WHEN      VABNZD = 'DE'
043000140808     C                   EVAL      VABNZD = 'D'
043100140808     C                   WHEN      VABNZD = 'AT'
043200140808     C                   EVAL      VABNZD = 'A'
043300140808     C                   WHEN      VABNZD = 'ES'
043400140808     C                   EVAL      VABNZD = 'E'
043500140808     C                   WHEN      VABNZD = 'NL'
043600140808     C                   EVAL      VABNZD = 'NL'
043700140808     C                   WHEN      VABNZD = 'LU'
043800140808     C                   EVAL      VABNZD = 'L'
043900140808     C                   ENDSL
044000140808     C                   EVAL      VABRMA=%trim(%subst(vindta:411:11))
044100110801     C*
044200140808     C                   MOVEL     *blanks       wNOTE            40
044300140808     C                   EVAL      wNOTE=%trim(%subst(vindta:331:40))
044400140808     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
044500140808     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:40-35))
044600100729     C*
044700100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044800100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044900100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045000100729     C* ==
045100100729     C*
045200100729     C* Reperimento campi NUMERICI
045300100729     C                   MOVEL     DATCOR        VABAAS
045400100729     C                   MOVE      DATCOR        VABMGS
045500110408     C*
045600111020     C* NSP
045700101119     C                   IF        *IN51 = *off
045800140808     C                   EVAL      PiStr=%subst(vindta:16:10)
045900100730     C                   EXSR      CHKNUM
046000100730     C                   IF        PiInt=*on
046100100928     C  N60              Z-ADD     PiVal         VABNSP
046200100928     C  N60              Z-ADD     PiVal         VATNSP
046300100730     C                   ELSE
046400100730     C                   SETON                                        32
046500100730     C                   Z-ADD     1             VABRMN
046600100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046700111020     C                             + ' ' + 'VABNSP VATNSP'
046800100730     C                   ENDIF
046900101119     C*
047000101119     C                   ELSE
047100101119     C*
047200140808     C                   EVAL      PiStr=%subst(vindta:16:10)
047300101119     C                   EXSR      CHKNUM
047400101119     C                   IF        PiInt=*on
047500101119     C  N60              Z-ADD     PiVal         VATNSP
047600101119     C                   ELSE
047700101119     C                   SETON                                        32
047800101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047900101119     C                             + ' ' + 'VATNSP'
048000101119     C                   ENDIF
048100101119     C                   ENDIF
048200111020     C* RMN
048300140808     C                   EVAL      PiStr=%subst(vindta:16:10)
048400111020     C                   EXSR      CHKNUM
048500111020     C                   IF        PiInt=*on
048600111020     C                   Z-ADD     PiVal         VABRMN
048700111020     C                   ELSE
048800111020     C  N51              SETON                                        32
048900111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049000111020     C                             + ' ' + 'VABRMN'
049100111020     C                   ENDIF
049200110801     C* NCL
049300140808     C                   EVAL      PiStr=%subst(vindta:235:5)
049400110801     C                   EXSR      CHKNUM
049500110801     C                   IF        PiInt=*on
049600110801     C                   ADD       PiVal         VABNCL
049700110801     C                   ELSE
049800110801     C  N51              SETON                                        32
049900111020     C  N51              Z-ADD     *zeros        VABNCL
050000110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050100110801     C                             + ' ' + 'VABNCL'
050200110801     C                   ENDIF
050300100729     C* PKB
050400140808     C                   EVAL      PiStr=%trim(%subst(vindta:242:6))
050500100729     C                   EXSR      CHKNUM
050600100729     C                   IF        PiNum=*on
050700140808     C***                EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
050800110627     C                   ADD(H)    PiVal         VABPKB
050900100729     C                   ELSE
051000101119     C  N51              SETON                                        32
051100101119     C  N51              Z-ADD     *zeros        VABPKB
051200101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051300100729     C                             + ' ' + 'VABPKB'
051400100729     C                   ENDIF
051500100729     C*
051600100729     C* Se provincia nn valorizzata la reperisco
051700100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
051800100729     C                   IF        VABNZD  = *blanks AND
051900100729     C                             VABPRD  = *blanks AND
052000100729     C                             VABCAD <> *blanks
052100100729     C                   CLEAR                   TISI95DS
052200100729     C                   EVAL      I95TCN = '3'
052300100729     C                   Z-ADD     datcor        I95DAT
052400100729     C                   EVAL      I95NAR = VABNZD
052500100729     C                   EVAL      I95CAP = VABCAD
052600100729     C                   EVAL      I95LOC = VABLOD
052700100729     C                   CALL      'TISI95R'
052800100729     C                   PARM                    TISI95DS
052900100729     C                   EVAL      VABPRD = O95PRV
053000100729     C                   ENDIF
053100100729     C*
053200100729     C* Considerazioni finali su CBO/CAS
053300100729     C                   IF        FlgCAS = '1'
053400100729     C                   IF        VABCBO = '1'
053500100729     C                   EVAL      VABCBO = '4'
053600100729     C                   ENDIF
053700100729     C                   IF        VABCBO = '2'
053800100729     C                   EVAL      VABCBO = '6'
053900100729     C                   ENDIF
054000100729     C                   ENDIF
054100010202     C*
054200000801     C* Ebbene...
054300000801     C                   ADD       1             §CTRMO
054400070530     C                   IF        *in31 <> *zeros OR
054500070530     C                             *in32 <> *zeros
054600000801     C                   ADD       1             §CTRNO
054700020725     C                   EVAL      x_vinflg = '2'
054800000801     C                   ELSE
054900010201     C                   ADD       1             §CTROKVB
055000000801     C                   ENDIF
055100000801     C*
055200000801     C                   ENDSR
055300100127     C*----------------------------------------------------*
055400100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
055500100127     C*----------------------------------------------------*
055600100127     C     WRIVAT_A      BEGSR
055700100127     C*
055800100127     C* Valorizzo l buffer di scrittura del FIVAT
055900100127     C* - TIPO RECORD "A"
056000101119     C***                eval      VATTRC = 'A'
056100101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
056200101119     C***                exsr      wrivat
056300100127     C*
056400100127     C                   ENDSR
056500010201     C*----------------------------------------------------*
056600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056700010201     C*----------------------------------------------------*
056800071003     C     WRIVAT_B      BEGSR
056900010201     C*
057000021113     C* Valorizzo l buffer di scrittura del FIVAT
057100070928     C* - TIPO RECORD "B"
057200110627     C***                eval      VATTRC = 'B'
057300110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
057400110627     C***                exsr      wrivat
057500010201     C*
057600010201     C                   ENDSR
057700071003     C*----------------------------------------------------*
057800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057900071003     C*----------------------------------------------------*
058000071003     C     WRIVAT_E      BEGSR
058100071003     C*
058200071003     C* Valorizzo l buffer di scrittura del FIVAT
058300071003     C* - TIPO RECORD "E"
058400100729     C*
058500140808     C***                eval      VATTRC = 'E'
058600140808     C***                eval      VATNOT = %trim(%subst(vindta:5:6))
058700140808 xxx C***                exsr      wrivat
058800111110     C*
058900111110     C* Sviluppati i "CHI SONO" in relazione al numero colli
059000111110     C*
059100111110     C* Inizializzo progressivo collo su totale colli della spedizione
059200140808     C***                Z-ADD     1             wNumColloDA       5 0
059300140808     C***                MOVEL     *blanks       wNumColloCor      2
059400140808     C***                MOVEL     *blanks       wWhoIs            6
059500140808     C***                eval      wWhoIs = %trim(%subst(vindta:5:6))
059600111110     C*
059700140808     C***                DOW       wNumColloDA <= VABNCL
059800140808     C***                MOVE(P)   wNumColloDA   wNumColloCor
059900140808     C***                EVAL      VATNOT =
060000140808     C***                             %subst(%char(%dec(%date() : *ISO)):3:2) +
060100140808     C***                             wNumColloCor + wWhoIs
060200140808     C***                exsr      wrivat
060300140808     C***                add       1             §CTROKVT
060400111110     C*
060500140808     C***                EVAL      wNumColloDA = wNumColloDA + 1
060600140808     C***                ENDDO
060700071003     C*
060800071003     C                   ENDSR
060900100127     C*----------------------------------------------------*
061000100127     C*  SCARICO BUFFER FIVAT
061100100127     C*----------------------------------------------------*
061200100127     C     WRIVAT        BEGSR
061300100127     C*
061400100127     C                   if        vatNOT <> *blanks
061500100127     C                   ADD       1             §CTROKVT
061600100127     C                   write     FIVAT000
061700100127     C                   endif
061800100127     C*
061900100127     C                   ENDSR
062000020904
062100020904
062200010202     C*----------------------------------------------------*
062300100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
062400010202     C*----------------------------------------------------*
062500100525     C     PREVASX       BEGSR
062600100525     C*
062700021113     C* Compongo il nome del membro da dare al FIVATWWR
062800010202     C                   eval      parmbr = vlrhdl
062900010202     C                   movel     'M'           parmbr
063000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063100010202     C                   eval      paropz = '1'
063200100525     C*
063300010202     C* Effettuo la chiamata al CLLE preposto
063400100525     C  N51              call(e)   'TITVVTC'
063500010202     C                   parm                    parccm
063600010202     C                   parm                    parmbr
063700010202     C                   parm                    paropz
063800100525     C   51              call(e)   'TITVVBC'
063900100525     C                   parm                    parccm
064000100525     C                   parm                    parmbr
064100100525     C                   parm                    paropz
064200100525     C*
064300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064400010202     C                   if        %error
064500010202     C                   movel     '1'           chkcall
064600010202     C                   else
064700010202     C                   movel     '0'           chkcall
064800010202     C                   endif
064900010202     C*
065000010202     C                   ENDSR
065100000801     C*----------------------------------------------------*
065200000801     C*  CONTROLLO NUMERICITA' CAMPI
065300000801     C*----------------------------------------------------*
065400000801     C     CHKNUM        BEGSR
065500081113     C*
065600081113     C                   IF        PiDecChr = *blanks
065700110408     C                   EVAL      PiDecChr = '.'
065800081113     C                   ENDIF
065900091223     C*
066000081113     C                   callp(e)  UBISNUM_Check(PiStr
066100081113     C                                          :PiDecChr
066200081113     C                                          :PiVal
066300081113     C                                          :PiNum
066400081113     C                                          :PiInt)
066500081113     C*
066600000801     C                   IF        %error
066700000801     C                   EVAL      PiInt=*off
066800000801     C                   ENDIF
066900000801     C*
067000000801     C                   ENDSR
067100000801     C***
067200011113
067300011113
067400000801
067500000801
067600990920      /TITLE Invio dei dati al punto operativo.
067700010202     C     invio         BEGSR
067800990920     C*
067900021113     C* 1° invio FIVAT
068000010201     C                   reset                   dscmz
068100110411     C                   move      vatfgs        cmzdst
068200021113     C                   eval      cmzfld = 'FIVATWWR'
068300010201     C                   eval      cmzmbd = vlrhdl
068400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068500021009     C***                if        prmfir = *blanks
068600021113     C                   eval      cmzfla = 'FIVAT00F'
068700021113     C                   eval      cmzmba = 'FIVAT00F'
068800021009     C***                else
068900021009     C***                eval      cmzfla = prmfir
069000021009     C***                eval      cmzmba = prmfir
069100021009     C***                endif
069200010201     C                   eval      cmznrr = *zeros
069300020305     C                   move      §ctrokvt      cmznrr
069400021018     C                   eval      cmzlba = vlrfl1
069500010201     C                   call(e)   'TIS711C'
069600010201     C                   parm                    dscmz
069700010201     C                   parm      *blanks       esito
069800010205     C                   if        %error
069900010205     C                             or cmzerr = '1'
070000010205     C                             or esito  = '1'
070100010205     C                   eval      wrkesito = '3'
070200010205     C                   else
070300010201     C*
070400021113     C* 2° invio FIVAB
070500100525     C                   if        *in51 = *off
070600010201     C                   reset                   dscmz
070700110411     C                   move      vabfgs        cmzdst
070800010201     C                   eval      cmzfld = vlrfou
070900010201     C                   eval      cmzmbd = vlrhdl
071000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071100021009     C***                if        prmfir = *blanks
071200021113     C                   eval      cmzfla = 'FIVAB00F'
071300021113     C                   eval      cmzmba = 'FIVAB00F'
071400021009     C***                else
071500021009     C***                eval      cmzfla = prmfir
071600021009     C***                eval      cmzmba = prmfir
071700021009     C***                endif
071800010201     C                   eval      cmznrr = *zeros
071900010201     C                   move      §ctrokvb      cmznrr
072000021018     C                   eval      cmzlba = vlrfl1
072100010201     C                   call(e)   'TIS711C'
072200010201     C                   parm                    dscmz
072300010201     C                   parm      *blanks       esito
072400010201     C                   if        %error
072500010201     C                             or cmzerr = '1'
072600010201     C                             or esito  = '1'
072700010201     C                   eval      wrkesito = '3'
072800010201     C                   endif
072900100525     C                   endif
073000010205     C                   endif
073100990920     C*
073200000613     C                   ENDSR
073300000613     C***
073400070411
073500110627     C     *pssr         BEGSR
073600070411     C*
073700110627     C                   if        %open(tivin00r)
073800110627     C                   close     tivin00r
073900110627     C                   endif
074000110627     C                   if        %open(fivabwwr)
074100110627     C                   close     fivabwwr
074200110627     C                   endif
074300110627     C                   if        %open(fivatwwr)
074400110627     C                   close     fivatwwr
074500110627     C                   endif
074600110627     C*
074700110627     C* Effettuo la chiamata al CLLE preposto
074800110627     C  N51              call(e)   'TITVVTC'
074900110627     C                   parm                    parccm
075000110627     C                   parm                    parmbr
075100110627     C                   parm      '2'           paropz
075200110627     C   51              call(e)   'TITVVBC'
075300110627     C                   parm                    parccm
075400110627     C                   parm                    parmbr
075500110627     C                   parm      '2'           paropz
075600110627     C*
075700110627     C                   eval      wrkesito = '2'
075800110627     C*
075900110627     C                   seton                                        LR
076000110627     C*
076100110627     C                   ENDSR     '*CANCL'
076200070411     C***
076300070411
076400990910
076500000613     C     *inzsr        BEGSR
076600990910     C*
076700990910     C     *entry        plist
076800990920     C                   parm                    tivlrds
076900990921     C                   parm      wrkesito      esito
077000000724     C                   parm                    prmlit
077100000710     C                   parm                    prmfir
077200000613     C*
077300000830     C* CALCOLA LA DATA CORRENTE
077400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077500091223     C                   eval      datcor = %dec(%date() : *ISO)
077600000830     C*
077700000613     C                   ENDSR
077800000613     C***
