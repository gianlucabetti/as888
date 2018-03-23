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
002100110627     D depspe          s             11    INZ(*blanks)
002200110627     D curspe          s             11    INZ(*blanks)
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
003600110627     D  VINSPE                       11
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
013300110627     C+ vindta, vinflg, substr(vindta, 217, 11) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500110627     C+ order by sped, substr(vindta, 217, 11) desc
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
016300110627     C                   EVAL      curspe=%trim(%subst(vindta:217:11))
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
029300110627     C*
029400110627     C* Gestisco considerazioni x codice tariffa
029500110627     C                   if        FlgCTR = '1'
029600110627     C                   eval      VABCTR = 100
029700110627     C                   endif
029800100412     C*
029900100525     C                   if        *in51 = *off and *in31 = *off
030000100525     C                   write     fivab000                                     => scarico il VAB
030100100525     C                   endif
030200010305     C*
030300010305     C                   ENDSR
030400990920
030500000801     C*----------------------------------------------------*
030600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030700000801     C*----------------------------------------------------*
030800010201     C     INZVAR        BEGSR
030900000801     C*
031000010201     C                   Z-ADD     *zeros        Num5_0
031100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031200020725     C                   MOVEL     '0'           FlgCAS            1
031300110627     C* Inizializzo flag d wrk x codice tariffa
031400110627     C                   MOVEL     '1'           FlgCTR            1
031500000801     C*
031600000801     C                   ENDSR
031700000801     C*----------------------------------------------------*
031800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031900000801     C*----------------------------------------------------*
032000020904     C     DEFCAM        BEGSR
032100080923     C*
032200100928     C                   SETOFF                                       505160
032300000801     C*
032400021113     C                   CLEAR                   FIVAB000
032500021113     C                   CLEAR                   FIVAT000
032600070730     C* Imposto i valori di default...
032700110627     C                   EVAL      VABCCM = 0410544
032800110627     C                   EVAL      VATCCM = 0410544
032900110627     C                   EVAL      VABLNP = 041
033000110627     C                   EVAL      VATLNP = 041
033100110627     C                   EVAL      VABCTR = 000
033200100525     C                   EVAL      VABCBO = '1'
033300070222     C* ... e poi verifico se sono stati passati come parametri
033400070222     C                   IF        vlrppt > *blanks
033500100525     C*
033600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033800070222     C                   EXSR      CHKNUM
033900070222     C                   IF        PiInt=*on
034000070222     C                   Z-ADD     PiVal         VABCCM
034100070222     C                   Z-ADD     PiVal         VATCCM
034200070222     C                   ENDIF
034300070222     C                   ENDIF
034400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034600070222     C                   EXSR      CHKNUM
034700070222     C                   IF        PiInt=*on
034800070222     C                   Z-ADD     PiVal         VABLNP
034900070222     C                   Z-ADD     PiVal         VATLNP
035000070222     C                   ENDIF
035100070222     C                   ENDIF
035200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035400070222     C                   EXSR      CHKNUM
035500070222     C                   IF        PiInt=*on
035600070222     C                   Z-ADD     PiVal         VABCTR
035700070222     C                   ENDIF
035800070928     C                   ENDIF
035900071009     C                   MOVEL     *blanks       wDISK             1
036000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036100071009     C                   IF        wDISK <> *blanks
036200070928     C                   SETON                                        50
036300070222     C                   ENDIF
036400100525     C                   IF        wDISK  = 'T'
036500100525     C                   SETOFF                                       50
036600100525     C                   SETON                                        51
036700100525     C                   ENDIF
036800100401     C                   MOVEL     *blanks       wDUPREC           1
036900100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037000100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037100100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037200100525     C                   ENDIF
037300100525     C*
037400070222     C                   ENDIF
037500071009     C*
037600071009     C   50              EVAL      VABCTM = '7Q'
037700000801     C*
037800000801     C                   ENDSR
037900091223     C*----------------------------------------------------*
038000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038100091223     C*----------------------------------------------------*
038200091223     C     REPNSP        BEGSR
038300091223     C*
038400091223     C                   EXSR      INZVAR
038500091223     C                   EXSR      DEFCAM
038600100928     C*
038700100928     C                   SETON                                        60
038800091223     C*
038900091223     C* NSP => Stacco un numeratore da AZNUM
039000091223     C                   clear                   TRUL33DS
039100091223     C                   eval      I33OPE = *zeros
039200091223     C                   eval      I33CNU = 302
039300091223     C                   eval      I33NUM = 1
039400091223     C                   movel     TRUL33DS      KPJBU
039500091223     C                   call      'TRUL33R'
039600091223     C                   parm                    KPJBA
039700091223     C                   movel     KPJBU         TRUL33DS
039800091223     C                   if        O33ERR = *zeros
039900091223     C                   z-add     O33NRF        VABNSP
040000091223     C                   z-add     O33NRF        VATNSP
040100091223     C                   else
040200091223     C                   SETON                                        31
040300091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400091223     C                             + ' ' + 'VABNSP VATNSP'
040500091223     C                   endif
040600091223     C*
040700091223     C                   ENDSR
040800000801     C*----------------------------------------------------*
040900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041000000801     C*----------------------------------------------------*
041100100730     C     IMPVAB        BEGSR
041200070530     C*
041300070530     C                   SETOFF                                       3132
041400070928     C*
041500070928     C                   MOVE(P)   vlrpoi        VABFGS
041600070928     C                   MOVE(P)   vlrpoi        VATFGS
041700070928     C*
041800070928     C                   MOVEL     datcor        VABAAS
041900070928     C                   MOVEL     datcor        VATAAS
042000070928     C                   MOVE      datcor        VABMGS
042100100729     C*
042200100729     C* Reperimento campi ALFA
042300110627     C                   EVAL      VABRSD=%trim(%subst(vindta:274:35))
042400110627     C                   EVAL      VABRMA=%trim(%subst(vindta:221:7))
042500110627     C                   EVAL      VABIND=%trim(%subst(vindta:309:35))
042600110627     C                   EVAL      VABLOD=%trim(%subst(vindta:354:35))
042700141104     C                   EVAL      VABPRD=%trim(%subst(vindta:391:2))
042800110627     C                   EVAL      VABCAD=%trim(%subst(vindta:344:10))
042900100729     C*
043000100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
043100100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043200100729     C     '@':'A'       XLATE     VABRSD        VABRSD
043300100729     C* ==
043400100729     C*
043500100729     C* Reperimento campi NUMERICI
043600100729     C                   MOVEL     DATCOR        VABAAS
043700100729     C                   MOVE      DATCOR        VABMGS
043800110408     C*
043900110127     C* NSP / RMN
044000101119     C                   IF        *IN51 = *off
044100110627     C                   EVAL      PiStr=%subst(vindta:223:5)
044200100730     C                   EXSR      CHKNUM
044300100730     C                   IF        PiInt=*on
044400100730     C                   Z-ADD     PiVal         VABRMN
044500100928     C  N60              Z-ADD     PiVal         VABNSP
044600100928     C  N60              Z-ADD     PiVal         VATNSP
044700100730     C                   ELSE
044800100730     C                   SETON                                        32
044900100730     C                   Z-ADD     1             VABRMN
045000100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
045200100730     C                   ENDIF
045300101119     C*
045400101119     C                   ELSE
045500101119     C*
045600110627     C                   EVAL      PiStr=%subst(vindta:223:5)
045700101119     C                   EXSR      CHKNUM
045800101119     C                   IF        PiInt=*on
045900101119     C  N60              Z-ADD     PiVal         VATNSP
046000101119     C                   ELSE
046100101119     C                   SETON                                        32
046200101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300101119     C                             + ' ' + 'VATNSP'
046400101119     C                   ENDIF
046500101119     C                   ENDIF
046600110627     C* NCL
046700110627     C                   EVAL      VABNCL = VABNCL + 1
046800100729     C* PKB
046900110627     C                   IF        %trim(%subst(vindta:454:8)) <> *all'0' AND
047000110627     C                             %trim(%subst(vindta:454:8)) <> *blanks
047100110627     C                   EVAL      PiStr=%trim(%subst(vindta:454:8))
047200100729     C                   EXSR      CHKNUM
047300100729     C                   IF        PiNum=*on
047400110627     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
047500110627     C                   ADD(H)    PiVal         VABPKB
047600100729     C                   ELSE
047700101119     C  N51              SETON                                        32
047800101119     C  N51              Z-ADD     *zeros        VABPKB
047900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
048000100729     C                             + ' ' + 'VABPKB'
048100100729     C                   ENDIF
048200110325     C                   ENDIF
048300110627     C* VLB
048400110627     C                   IF        %trim(%subst(vindta:470:8)) <> *all'0' AND
048500110627     C                             %trim(%subst(vindta:470:8)) <> *blanks
048600110627     C                   EVAL      PiStr=%trim(%subst(vindta:470:8))
048700110627     C                   EXSR      CHKNUM
048800110627     C                   IF        PiNum=*on
048900110627     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
049000110627     C                   ADD(H)    PiVal         VABVLB
049100110627     C                   ELSE
049200110627     C  N51              SETON                                        32
049300110627     C  N51              Z-ADD     *zeros        VABVLB
049400110627     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049500110627     C                             + ' ' + 'VABVLB'
049600110627     C                   ENDIF
049700110627     C                   ENDIF
049800110627     C* CTR
049900110627     C                   IF        %subst(vindta:426:3) <> 'BAU' AND
050000110627     C                             %subst(vindta:426:3) <> 'APP'
050100110627     C                   EVAL      FlgCTR = '0'
050200110627     C                   ENDIF
050300100729     C*
050400100729     C* Se provincia nn valorizzata la reperisco
050500100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
050600100729     C                   IF        VABNZD  = *blanks AND
050700100729     C                             VABPRD  = *blanks AND
050800100729     C                             VABCAD <> *blanks
050900100729     C                   CLEAR                   TISI95DS
051000100729     C                   EVAL      I95TCN = '3'
051100100729     C                   Z-ADD     datcor        I95DAT
051200100729     C                   EVAL      I95NAR = VABNZD
051300100729     C                   EVAL      I95CAP = VABCAD
051400100729     C                   EVAL      I95LOC = VABLOD
051500100729     C                   CALL      'TISI95R'
051600100729     C                   PARM                    TISI95DS
051700100729     C                   EVAL      VABPRD = O95PRV
051800100729     C                   ENDIF
051900100729     C*
052000100729     C* Considerazioni finali su CBO/CAS
052100100729     C                   IF        FlgCAS = '1'
052200100729     C                   IF        VABCBO = '1'
052300100729     C                   EVAL      VABCBO = '4'
052400100729     C                   ENDIF
052500100729     C                   IF        VABCBO = '2'
052600100729     C                   EVAL      VABCBO = '6'
052700100729     C                   ENDIF
052800100729     C                   ENDIF
052900010202     C*
053000000801     C* Ebbene...
053100000801     C                   ADD       1             §CTRMO
053200070530     C                   IF        *in31 <> *zeros OR
053300070530     C                             *in32 <> *zeros
053400000801     C                   ADD       1             §CTRNO
053500020725     C                   EVAL      x_vinflg = '2'
053600000801     C                   ELSE
053700010201     C                   ADD       1             §CTROKVB
053800000801     C                   ENDIF
053900000801     C*
054000000801     C                   ENDSR
054100100127     C*----------------------------------------------------*
054200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
054300100127     C*----------------------------------------------------*
054400100127     C     WRIVAT_A      BEGSR
054500100127     C*
054600100127     C* Valorizzo l buffer di scrittura del FIVAT
054700100127     C* - TIPO RECORD "A"
054800101119     C***                eval      VATTRC = 'A'
054900101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
055000101119     C***                exsr      wrivat
055100100127     C*
055200100127     C                   ENDSR
055300010201     C*----------------------------------------------------*
055400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
055500010201     C*----------------------------------------------------*
055600071003     C     WRIVAT_B      BEGSR
055700010201     C*
055800021113     C* Valorizzo l buffer di scrittura del FIVAT
055900070928     C* - TIPO RECORD "B"
056000110627     C***                eval      VATTRC = 'B'
056100110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
056200110627     C***                exsr      wrivat
056300010201     C*
056400010201     C                   ENDSR
056500071003     C*----------------------------------------------------*
056600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
056700071003     C*----------------------------------------------------*
056800071003     C     WRIVAT_E      BEGSR
056900071003     C*
057000071003     C* Valorizzo l buffer di scrittura del FIVAT
057100071003     C* - TIPO RECORD "E"
057200100729     C*
057300100928     C                   eval      VATTRC = 'E'
057400141104     C                   eval      VATNOT = %trim(%subst(vindta:415:18))
057500100928     C                   exsr      wrivat
057600071003     C*
057700071003     C                   ENDSR
057800100127     C*----------------------------------------------------*
057900100127     C*  SCARICO BUFFER FIVAT
058000100127     C*----------------------------------------------------*
058100100127     C     WRIVAT        BEGSR
058200100127     C*
058300100127     C                   if        vatNOT <> *blanks
058400100127     C                   ADD       1             §CTROKVT
058500100127     C                   write     FIVAT000
058600100127     C                   endif
058700100127     C*
058800100127     C                   ENDSR
058900020904
059000020904
059100010202     C*----------------------------------------------------*
059200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
059300010202     C*----------------------------------------------------*
059400100525     C     PREVASX       BEGSR
059500100525     C*
059600021113     C* Compongo il nome del membro da dare al FIVATWWR
059700010202     C                   eval      parmbr = vlrhdl
059800010202     C                   movel     'M'           parmbr
059900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
060000010202     C                   eval      paropz = '1'
060100100525     C*
060200010202     C* Effettuo la chiamata al CLLE preposto
060300100525     C  N51              call(e)   'TITVVTC'
060400010202     C                   parm                    parccm
060500010202     C                   parm                    parmbr
060600010202     C                   parm                    paropz
060700100525     C   51              call(e)   'TITVVBC'
060800100525     C                   parm                    parccm
060900100525     C                   parm                    parmbr
061000100525     C                   parm                    paropz
061100100525     C*
061200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061300010202     C                   if        %error
061400010202     C                   movel     '1'           chkcall
061500010202     C                   else
061600010202     C                   movel     '0'           chkcall
061700010202     C                   endif
061800010202     C*
061900010202     C                   ENDSR
062000000801     C*----------------------------------------------------*
062100000801     C*  CONTROLLO NUMERICITA' CAMPI
062200000801     C*----------------------------------------------------*
062300000801     C     CHKNUM        BEGSR
062400081113     C*
062500081113     C                   IF        PiDecChr = *blanks
062600110408     C                   EVAL      PiDecChr = '.'
062700081113     C                   ENDIF
062800091223     C*
062900081113     C                   callp(e)  UBISNUM_Check(PiStr
063000081113     C                                          :PiDecChr
063100081113     C                                          :PiVal
063200081113     C                                          :PiNum
063300081113     C                                          :PiInt)
063400081113     C*
063500000801     C                   IF        %error
063600000801     C                   EVAL      PiInt=*off
063700000801     C                   ENDIF
063800000801     C*
063900000801     C                   ENDSR
064000000801     C***
064100011113
064200011113
064300000801
064400000801
064500990920      /TITLE Invio dei dati al punto operativo.
064600010202     C     invio         BEGSR
064700990920     C*
064800021113     C* 1° invio FIVAT
064900010201     C                   reset                   dscmz
065000110411     C                   move      vatfgs        cmzdst
065100021113     C                   eval      cmzfld = 'FIVATWWR'
065200010201     C                   eval      cmzmbd = vlrhdl
065300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065400021009     C***                if        prmfir = *blanks
065500021113     C                   eval      cmzfla = 'FIVAT00F'
065600021113     C                   eval      cmzmba = 'FIVAT00F'
065700021009     C***                else
065800021009     C***                eval      cmzfla = prmfir
065900021009     C***                eval      cmzmba = prmfir
066000021009     C***                endif
066100010201     C                   eval      cmznrr = *zeros
066200020305     C                   move      §ctrokvt      cmznrr
066300021018     C                   eval      cmzlba = vlrfl1
066400010201     C                   call(e)   'TIS711C'
066500010201     C                   parm                    dscmz
066600010201     C                   parm      *blanks       esito
066700010205     C                   if        %error
066800010205     C                             or cmzerr = '1'
066900010205     C                             or esito  = '1'
067000010205     C                   eval      wrkesito = '3'
067100010205     C                   else
067200010201     C*
067300021113     C* 2° invio FIVAB
067400100525     C                   if        *in51 = *off
067500010201     C                   reset                   dscmz
067600110411     C                   move      vabfgs        cmzdst
067700010201     C                   eval      cmzfld = vlrfou
067800010201     C                   eval      cmzmbd = vlrhdl
067900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068000021009     C***                if        prmfir = *blanks
068100021113     C                   eval      cmzfla = 'FIVAB00F'
068200021113     C                   eval      cmzmba = 'FIVAB00F'
068300021009     C***                else
068400021009     C***                eval      cmzfla = prmfir
068500021009     C***                eval      cmzmba = prmfir
068600021009     C***                endif
068700010201     C                   eval      cmznrr = *zeros
068800010201     C                   move      §ctrokvb      cmznrr
068900021018     C                   eval      cmzlba = vlrfl1
069000010201     C                   call(e)   'TIS711C'
069100010201     C                   parm                    dscmz
069200010201     C                   parm      *blanks       esito
069300010201     C                   if        %error
069400010201     C                             or cmzerr = '1'
069500010201     C                             or esito  = '1'
069600010201     C                   eval      wrkesito = '3'
069700010201     C                   endif
069800100525     C                   endif
069900010205     C                   endif
070000990920     C*
070100000613     C                   ENDSR
070200000613     C***
070300070411
070400110627     C     *pssr         BEGSR
070500070411     C*
070600110627     C                   if        %open(tivin00r)
070700110627     C                   close     tivin00r
070800110627     C                   endif
070900110627     C                   if        %open(fivabwwr)
071000110627     C                   close     fivabwwr
071100110627     C                   endif
071200110627     C                   if        %open(fivatwwr)
071300110627     C                   close     fivatwwr
071400110627     C                   endif
071500110627     C*
071600110627     C* Effettuo la chiamata al CLLE preposto
071700110627     C  N51              call(e)   'TITVVTC'
071800110627     C                   parm                    parccm
071900110627     C                   parm                    parmbr
072000110627     C                   parm      '2'           paropz
072100110627     C   51              call(e)   'TITVVBC'
072200110627     C                   parm                    parccm
072300110627     C                   parm                    parmbr
072400110627     C                   parm      '2'           paropz
072500110627     C*
072600110627     C                   eval      wrkesito = '2'
072700110627     C*
072800110627     C                   seton                                        LR
072900110627     C*
073000110627     C                   ENDSR     '*CANCL'
073100070411     C***
073200070411
073300990910
073400000613     C     *inzsr        BEGSR
073500990910     C*
073600990910     C     *entry        plist
073700990920     C                   parm                    tivlrds
073800990921     C                   parm      wrkesito      esito
073900000724     C                   parm                    prmlit
074000000710     C                   parm                    prmfir
074100000613     C*
074200000830     C* CALCOLA LA DATA CORRENTE
074300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074400091223     C                   eval      datcor = %dec(%date() : *ISO)
074500000830     C*
074600000613     C                   ENDSR
074700000613     C***
