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
002100111020     D depspe          s              8    INZ(*blanks)
002200111020     D curspe          s              8    INZ(*blanks)
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
003600111020     D  VINSPE                        8
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
013300111020     C+ vindta, vinflg, substr(vindta, 124, 8) as sped, rrn(tivin00r)
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
016300111020     C                   EVAL      curspe=%trim(%subst(vindta:124:8))
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
032000111020     C                   EVAL      VABCCM = 2751348
032100111020     C                   EVAL      VATCCM = 2751348
032200111020     C                   EVAL      VABLNP = 275
032300111020     C                   EVAL      VATLNP = 275
032400111020     C                   EVAL      VABCTR = 300
032500100525     C                   EVAL      VABCBO = '1'
032600070222     C* ... e poi verifico se sono stati passati come parametri
032700070222     C                   IF        vlrppt > *blanks
032800100525     C*
032900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033100070222     C                   EXSR      CHKNUM
033200070222     C                   IF        PiInt=*on
033300070222     C                   Z-ADD     PiVal         VABCCM
033400070222     C                   Z-ADD     PiVal         VATCCM
033500070222     C                   ENDIF
033600070222     C                   ENDIF
033700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033900070222     C                   EXSR      CHKNUM
034000070222     C                   IF        PiInt=*on
034100070222     C                   Z-ADD     PiVal         VABLNP
034200070222     C                   Z-ADD     PiVal         VATLNP
034300070222     C                   ENDIF
034400070222     C                   ENDIF
034500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034700070222     C                   EXSR      CHKNUM
034800070222     C                   IF        PiInt=*on
034900070222     C                   Z-ADD     PiVal         VABCTR
035000070222     C                   ENDIF
035100070928     C                   ENDIF
035200071009     C                   MOVEL     *blanks       wDISK             1
035300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035400071009     C                   IF        wDISK <> *blanks
035500070928     C                   SETON                                        50
035600070222     C                   ENDIF
035700100525     C                   IF        wDISK  = 'T'
035800100525     C                   SETOFF                                       50
035900100525     C                   SETON                                        51
036000100525     C                   ENDIF
036100100401     C                   MOVEL     *blanks       wDUPREC           1
036200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036500100525     C                   ENDIF
036600100525     C*
036700070222     C                   ENDIF
036800071009     C*
036900071009     C   50              EVAL      VABCTM = '7Q'
037000000801     C*
037100000801     C                   ENDSR
037200091223     C*----------------------------------------------------*
037300091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037400091223     C*----------------------------------------------------*
037500091223     C     REPNSP        BEGSR
037600091223     C*
037700091223     C                   EXSR      INZVAR
037800091223     C                   EXSR      DEFCAM
037900100928     C*
038000100928     C                   SETON                                        60
038100091223     C*
038200091223     C* NSP => Stacco un numeratore da AZNUM
038300091223     C                   clear                   TRUL33DS
038400091223     C                   eval      I33OPE = *zeros
038500091223     C                   eval      I33CNU = 302
038600091223     C                   eval      I33NUM = 1
038700091223     C                   movel     TRUL33DS      KPJBU
038800091223     C                   call      'TRUL33R'
038900091223     C                   parm                    KPJBA
039000091223     C                   movel     KPJBU         TRUL33DS
039100091223     C                   if        O33ERR = *zeros
039200091223     C                   z-add     O33NRF        VABNSP
039300091223     C                   z-add     O33NRF        VATNSP
039400091223     C                   else
039500091223     C                   SETON                                        31
039600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700091223     C                             + ' ' + 'VABNSP VATNSP'
039800091223     C                   endif
039900091223     C*
040000091223     C                   ENDSR
040100000801     C*----------------------------------------------------*
040200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040300000801     C*----------------------------------------------------*
040400100730     C     IMPVAB        BEGSR
040500070530     C*
040600070530     C                   SETOFF                                       3132
040700070928     C*
040800070928     C                   MOVE(P)   vlrpoi        VABFGS
040900070928     C                   MOVE(P)   vlrpoi        VATFGS
041000070928     C*
041100070928     C                   MOVEL     datcor        VABAAS
041200070928     C                   MOVEL     datcor        VATAAS
041300070928     C                   MOVE      datcor        VABMGS
041400100729     C*
041500100729     C* Reperimento campi ALFA
041600111020     C                   EVAL      VABRSD=%trim(%subst(vindta:162:50))
041700111020     C                   EVAL      VABRD2=%trim(%subst(vindta:212:35))
041800111020     C                   EVAL      VABIND=%trim(%subst(vindta:247:30))
041900111020     C                   EVAL      VABLOD=%trim(%subst(vindta:277:30))
042000111020     C                   EVAL      VABCAD=%trim(%subst(vindta:307:5))
042100111020     C                   EVAL      VABPRD=%trim(%subst(vindta:314:2))
042200111020     C                   EVAL      VABRMA=%trim(%subst(vindta:648:8))
042300110801     C*
042400110801     C                   SELECT
042500111020     C                   WHEN      %subst(vindta:647:1) = 'C'
042600111020     C                   EVAL      VABCBO='4'
042700110801     C                   ENDSL
042800100729     C*
042900100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
043000100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043100100729     C     '@':'A'       XLATE     VABRSD        VABRSD
043200100729     C* ==
043300100729     C*
043400100729     C* Reperimento campi NUMERICI
043500100729     C                   MOVEL     DATCOR        VABAAS
043600100729     C                   MOVE      DATCOR        VABMGS
043700110408     C*
043800111020     C* NSP
043900101119     C                   IF        *IN51 = *off
044000130311     C                   EVAL      PiStr=%subst(vindta:124:8)
044100100730     C                   EXSR      CHKNUM
044200100730     C                   IF        PiInt=*on
044300100928     C  N60              Z-ADD     PiVal         VABNSP
044400100928     C  N60              Z-ADD     PiVal         VATNSP
044500100730     C                   ELSE
044600100730     C                   SETON                                        32
044700100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044800111020     C                             + ' ' + 'VABNSP VATNSP'
044900100730     C                   ENDIF
045000101119     C*
045100101119     C                   ELSE
045200101119     C*
045300130311     C                   EVAL      PiStr=%subst(vindta:124:8)
045400101119     C                   EXSR      CHKNUM
045500101119     C                   IF        PiInt=*on
045600101119     C  N60              Z-ADD     PiVal         VATNSP
045700101119     C                   ELSE
045800101119     C                   SETON                                        32
045900101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046000101119     C                             + ' ' + 'VATNSP'
046100101119     C                   ENDIF
046200101119     C                   ENDIF
046300111020     C* RMN
046400111020     C                   EVAL      PiStr=%subst(vindta:124:8)
046500111020     C                   EXSR      CHKNUM
046600111020     C                   IF        PiInt=*on
046700111020     C                   Z-ADD     PiVal         VABRMN
046800111020     C                   ELSE
046900130311     C                   Z-ADD     1             VABRMN
047000111020     C  N51              SETON                                        32
047100111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
047200111020     C                             + ' ' + 'VABRMN'
047300111020     C                   ENDIF
047400110801     C* NCL
047500111020     C                   EVAL      PiStr=%subst(vindta:510:5)
047600110801     C                   EXSR      CHKNUM
047700110801     C                   IF        PiInt=*on
047800110801     C                   ADD       PiVal         VABNCL
047900110801     C                   ELSE
048000110801     C  N51              SETON                                        32
048100111020     C  N51              Z-ADD     *zeros        VABNCL
048200110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
048300110801     C                             + ' ' + 'VABNCL'
048400110801     C                   ENDIF
048500100729     C* PKB
048600111020     C                   EVAL      PiStr=%trim(%subst(vindta:486:8))
048700100729     C                   EXSR      CHKNUM
048800100729     C                   IF        PiNum=*on
048900110627     C                   ADD(H)    PiVal         VABPKB
049000100729     C                   ELSE
049100101119     C  N51              SETON                                        32
049200101119     C  N51              Z-ADD     *zeros        VABPKB
049300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049400100729     C                             + ' ' + 'VABPKB'
049500100729     C                   ENDIF
049600111020     C*
049700130318     C                   IF        VABPKB <= 30
049800130318     C                   EVAL      VABCCM  = 2751348
049900130318     C                   ELSE
050000130318     C                   EVAL      VABCCM  = 2751349
050100130318     C                   ENDIF
050200111020     C*
050300111020     C                   SELECT
050400111020     C                   WHEN      VABCCM = 2751348
050500111020     C                   EVAL      VABCTR = 300
050600111020     C                   WHEN      VABCCM = 2751349
050700111020     C                   EVAL      VABCTR = 001
050800111020     C                   ENDSL
050900100729     C*
051000100729     C* Se provincia nn valorizzata la reperisco
051100100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
051200100729     C                   IF        VABNZD  = *blanks AND
051300100729     C                             VABPRD  = *blanks AND
051400100729     C                             VABCAD <> *blanks
051500100729     C                   CLEAR                   TISI95DS
051600100729     C                   EVAL      I95TCN = '3'
051700100729     C                   Z-ADD     datcor        I95DAT
051800100729     C                   EVAL      I95NAR = VABNZD
051900100729     C                   EVAL      I95CAP = VABCAD
052000100729     C                   EVAL      I95LOC = VABLOD
052100100729     C                   CALL      'TISI95R'
052200100729     C                   PARM                    TISI95DS
052300100729     C                   EVAL      VABPRD = O95PRV
052400100729     C                   ENDIF
052500100729     C*
052600100729     C* Considerazioni finali su CBO/CAS
052700100729     C                   IF        FlgCAS = '1'
052800100729     C                   IF        VABCBO = '1'
052900100729     C                   EVAL      VABCBO = '4'
053000100729     C                   ENDIF
053100100729     C                   IF        VABCBO = '2'
053200100729     C                   EVAL      VABCBO = '6'
053300100729     C                   ENDIF
053400100729     C                   ENDIF
053500010202     C*
053600000801     C* Ebbene...
053700000801     C                   ADD       1             §CTRMO
053800070530     C                   IF        *in31 <> *zeros OR
053900070530     C                             *in32 <> *zeros
054000000801     C                   ADD       1             §CTRNO
054100020725     C                   EVAL      x_vinflg = '2'
054200000801     C                   ELSE
054300010201     C                   ADD       1             §CTROKVB
054400000801     C                   ENDIF
054500000801     C*
054600000801     C                   ENDSR
054700100127     C*----------------------------------------------------*
054800100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
054900100127     C*----------------------------------------------------*
055000100127     C     WRIVAT_A      BEGSR
055100100127     C*
055200100127     C* Valorizzo l buffer di scrittura del FIVAT
055300100127     C* - TIPO RECORD "A"
055400101119     C***                eval      VATTRC = 'A'
055500101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
055600101119     C***                exsr      wrivat
055700100127     C*
055800100127     C                   ENDSR
055900010201     C*----------------------------------------------------*
056000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056100010201     C*----------------------------------------------------*
056200071003     C     WRIVAT_B      BEGSR
056300010201     C*
056400021113     C* Valorizzo l buffer di scrittura del FIVAT
056500070928     C* - TIPO RECORD "B"
056600110627     C***                eval      VATTRC = 'B'
056700110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
056800110627     C***                exsr      wrivat
056900010201     C*
057000010201     C                   ENDSR
057100071003     C*----------------------------------------------------*
057200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057300071003     C*----------------------------------------------------*
057400071003     C     WRIVAT_E      BEGSR
057500071003     C*
057600071003     C* Valorizzo l buffer di scrittura del FIVAT
057700071003     C* - TIPO RECORD "E"
057800100729     C*
057900100928     C                   eval      VATTRC = 'E'
058000110627     C                   eval      VATNOT = %trim(%subst(vindta:415:11))
058100100928     C                   exsr      wrivat
058200071003     C*
058300071003     C                   ENDSR
058400100127     C*----------------------------------------------------*
058500100127     C*  SCARICO BUFFER FIVAT
058600100127     C*----------------------------------------------------*
058700100127     C     WRIVAT        BEGSR
058800100127     C*
058900100127     C                   if        vatNOT <> *blanks
059000100127     C                   ADD       1             §CTROKVT
059100100127     C                   write     FIVAT000
059200100127     C                   endif
059300100127     C*
059400100127     C                   ENDSR
059500020904
059600020904
059700010202     C*----------------------------------------------------*
059800100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
059900010202     C*----------------------------------------------------*
060000100525     C     PREVASX       BEGSR
060100100525     C*
060200021113     C* Compongo il nome del membro da dare al FIVATWWR
060300010202     C                   eval      parmbr = vlrhdl
060400010202     C                   movel     'M'           parmbr
060500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
060600010202     C                   eval      paropz = '1'
060700100525     C*
060800010202     C* Effettuo la chiamata al CLLE preposto
060900100525     C  N51              call(e)   'TITVVTC'
061000010202     C                   parm                    parccm
061100010202     C                   parm                    parmbr
061200010202     C                   parm                    paropz
061300100525     C   51              call(e)   'TITVVBC'
061400100525     C                   parm                    parccm
061500100525     C                   parm                    parmbr
061600100525     C                   parm                    paropz
061700100525     C*
061800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061900010202     C                   if        %error
062000010202     C                   movel     '1'           chkcall
062100010202     C                   else
062200010202     C                   movel     '0'           chkcall
062300010202     C                   endif
062400010202     C*
062500010202     C                   ENDSR
062600000801     C*----------------------------------------------------*
062700000801     C*  CONTROLLO NUMERICITA' CAMPI
062800000801     C*----------------------------------------------------*
062900000801     C     CHKNUM        BEGSR
063000081113     C*
063100081113     C                   IF        PiDecChr = *blanks
063200110408     C                   EVAL      PiDecChr = '.'
063300081113     C                   ENDIF
063400091223     C*
063500081113     C                   callp(e)  UBISNUM_Check(PiStr
063600081113     C                                          :PiDecChr
063700081113     C                                          :PiVal
063800081113     C                                          :PiNum
063900081113     C                                          :PiInt)
064000081113     C*
064100000801     C                   IF        %error
064200000801     C                   EVAL      PiInt=*off
064300000801     C                   ENDIF
064400000801     C*
064500000801     C                   ENDSR
064600000801     C***
064700011113
064800011113
064900000801
065000000801
065100990920      /TITLE Invio dei dati al punto operativo.
065200010202     C     invio         BEGSR
065300990920     C*
065400021113     C* 1° invio FIVAT
065500010201     C                   reset                   dscmz
065600110411     C                   move      vatfgs        cmzdst
065700021113     C                   eval      cmzfld = 'FIVATWWR'
065800010201     C                   eval      cmzmbd = vlrhdl
065900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000021009     C***                if        prmfir = *blanks
066100021113     C                   eval      cmzfla = 'FIVAT00F'
066200021113     C                   eval      cmzmba = 'FIVAT00F'
066300021009     C***                else
066400021009     C***                eval      cmzfla = prmfir
066500021009     C***                eval      cmzmba = prmfir
066600021009     C***                endif
066700010201     C                   eval      cmznrr = *zeros
066800020305     C                   move      §ctrokvt      cmznrr
066900021018     C                   eval      cmzlba = vlrfl1
067000010201     C                   call(e)   'TIS711C'
067100010201     C                   parm                    dscmz
067200010201     C                   parm      *blanks       esito
067300010205     C                   if        %error
067400010205     C                             or cmzerr = '1'
067500010205     C                             or esito  = '1'
067600010205     C                   eval      wrkesito = '3'
067700010205     C                   else
067800010201     C*
067900021113     C* 2° invio FIVAB
068000100525     C                   if        *in51 = *off
068100010201     C                   reset                   dscmz
068200110411     C                   move      vabfgs        cmzdst
068300010201     C                   eval      cmzfld = vlrfou
068400010201     C                   eval      cmzmbd = vlrhdl
068500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068600021009     C***                if        prmfir = *blanks
068700021113     C                   eval      cmzfla = 'FIVAB00F'
068800021113     C                   eval      cmzmba = 'FIVAB00F'
068900021009     C***                else
069000021009     C***                eval      cmzfla = prmfir
069100021009     C***                eval      cmzmba = prmfir
069200021009     C***                endif
069300010201     C                   eval      cmznrr = *zeros
069400010201     C                   move      §ctrokvb      cmznrr
069500021018     C                   eval      cmzlba = vlrfl1
069600010201     C                   call(e)   'TIS711C'
069700010201     C                   parm                    dscmz
069800010201     C                   parm      *blanks       esito
069900010201     C                   if        %error
070000010201     C                             or cmzerr = '1'
070100010201     C                             or esito  = '1'
070200010201     C                   eval      wrkesito = '3'
070300010201     C                   endif
070400100525     C                   endif
070500010205     C                   endif
070600990920     C*
070700000613     C                   ENDSR
070800000613     C***
070900070411
071000110627     C     *pssr         BEGSR
071100070411     C*
071200110627     C                   if        %open(tivin00r)
071300110627     C                   close     tivin00r
071400110627     C                   endif
071500110627     C                   if        %open(fivabwwr)
071600110627     C                   close     fivabwwr
071700110627     C                   endif
071800110627     C                   if        %open(fivatwwr)
071900110627     C                   close     fivatwwr
072000110627     C                   endif
072100110627     C*
072200110627     C* Effettuo la chiamata al CLLE preposto
072300110627     C  N51              call(e)   'TITVVTC'
072400110627     C                   parm                    parccm
072500110627     C                   parm                    parmbr
072600110627     C                   parm      '2'           paropz
072700110627     C   51              call(e)   'TITVVBC'
072800110627     C                   parm                    parccm
072900110627     C                   parm                    parmbr
073000110627     C                   parm      '2'           paropz
073100110627     C*
073200110627     C                   eval      wrkesito = '2'
073300110627     C*
073400110627     C                   seton                                        LR
073500110627     C*
073600110627     C                   ENDSR     '*CANCL'
073700070411     C***
073800070411
073900990910
074000000613     C     *inzsr        BEGSR
074100990910     C*
074200990910     C     *entry        plist
074300990920     C                   parm                    tivlrds
074400990921     C                   parm      wrkesito      esito
074500000724     C                   parm                    prmlit
074600000710     C                   parm                    prmfir
074700000613     C*
074800000830     C* CALCOLA LA DATA CORRENTE
074900091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075000091223     C                   eval      datcor = %dec(%date() : *ISO)
075100000830     C*
075200000613     C                   ENDSR
075300000613     C***
