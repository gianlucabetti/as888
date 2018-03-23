000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002100120327     D depspe          s              7    INZ(*blanks)
002200120327     D curspe          s              7    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900120327     D* mi serve staccare numeratore per nr.spedizione?
003000120327     D* lo si deve capire dai dati passati
003100120327     D* (se il loro nr.spedizione è > 7 char, bisogna rinumerarla)
003200120327     D* IN QUESTO CASO NO
003300020725
003400020725     D*------------------
003500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003600020725     D*------------------
003700120327     D INPUT_DS        DS                  INZ
003800100729     D  VINDTA                     2048
003900100729     D  VINFLG                        1
004000120327     D  VINSPE                        7
004100100729     D  VINRRN                        8  0
004200020725     D*
004300081113
004400091223     D*------------------
004500091223     D* DS REPERIMENTO NUMERATORE
004600091223     D*------------------
004700100309     D trul33ds      e ds                  inz
004800091223     D*------------------
004900091223     D* DS ARCHITETTURA
005000091223     D*------------------
005100091223     D kpjba         e ds                  inz
005200091223
005300101119
005400101119     D*-------------------
005500101119     D* COSTANTI
005600101119     D*-------------------
005700101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005800101119     D                                     èéù')
005900101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006000101119     D                                     EEU')
006100081113
006200081113     D*------------------
006300081113     D* LINKING A DEFINIZIONI ESTERNE
006400081113     D*------------------
006500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006700081113
006800990908
006900010201
007000010201
007100080117     C*
007200080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007300080117     C
007400080117     C/EXEC SQL
007500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600080117     C/END-EXEC
007700080117     C
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100000613     C*
008200100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008400000613     C*
008500010202     C* Effettuo la chiamata al CLLE preposto
008600100525     C  N51              call(e)   'TITVVTC'
008700100525     C                   parm                    parccm
008800100525     C                   parm                    parmbr
008900100525     C                   parm      '2'           paropz
009000100525     C   51              call(e)   'TITVVBC'
009100100525     C                   parm                    parccm
009200100525     C                   parm                    parmbr
009300100525     C                   parm      '2'           paropz
009400070730     C*
009500070730     C* Effettuo lancio TISI95 solo x chiusura
009600070730     C                   CLEAR                   TISI95DS
009700070730     C                   EVAL      I95TLA = 'C'
009800070730     C                   CALL      'TISI95R'
009900070730     C                   PARM                    TISI95DS
010000000616     C*
010100000801     C
010200010201     C                   seton                                        LR
010300990908
010400000801
010500910830     C*--------------------------------------------------------
010600070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010700910830     C*--------------------------------------------------------
010800070530     C     RWFILE        BEGSR
010900990910     C*
011000990914     C                   if        not %open(tivin00r)
011100990908     C                   open      tivin00r
011200990914     C                   endif
011300070530     C*
011400100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011500100525     C                   exsr      prevasx
011600010201     C*
011700010202     C                   if        chkcall = '0'
011800010202     C*
011900100525     C                   if        not %open(fivabwwr)
012000100525     C                   open      fivabwwr
012100100525     C                   endif
012200100525     C*
012300021113     C                   if        not %open(fivatwwr)
012400021113     C                   open      fivatwwr
012500010201     C                   endif
012600080117     C*
012700080117     C                   EXSR      INZVAR
012800080117     C                   EXSR      DEFCAM
012900990910     C*
013000010201     C                   clear                   §CTROKVB          5 0
013100020305     C                   clear                   §CTROKVT          5 0
013200000801     C                   clear                   §CTRMO            5 0
013300000801     C                   clear                   §CTRNO            5 0
013400990910     C*
013500020725     C/EXEC SQL
013600020725     C+ declare C1 cursor for select
013700120327     C+ vindta, vinflg, substr(vindta, 12, 7) as sped, rrn(tivin00r)
013800060223     C+ from tivin00r
013900111020     C+ order by sped
014000020725     C+ for read only
014100020725     C/END-EXEC
014200020725     C
014300020725     C/EXEC SQL
014400020725     C+ open C1
014500020725     C/END-EXEC
014600020725     C
014700020725     C/EXEC SQL
014800070530     C+ Fetch C1 into :INPUT_DS
014900020725     C/END-EXEC
015000020725     C*
015100020725     C                   dow       sqlcod = *zeros
015200990913     C*
015300100310     C                   if        vindta > *blanks
015400000613     C                   add       1             rrnum
015500000801     C*
015600020725     C                   if        vinflg = *blanks
015700020725     C                             or vinflg = '0'
015800020725     C                             or vinflg = '2'
015900000801     C*
016000020725     C                   clear                   x_vinmsg
016100020725     C                   eval      x_vinflg = '1'
016200101119     C*
016300101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016400101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016500010305     C*
016600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016700120327     C                   EVAL      curspe=%trim(%subst(vindta:12:7))
016800010305     C*
016900100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017000080923     C                   if        *in50 = *off
017100101119     C                   exsr      inzvar
017200101119     C                   exsr      defcam
017300071003     C                   exsr      impvab
017400100525     C                   exsr      wrivab
017500100127     C                   exsr      wrivat_a                                     => carico VAT
017600071003     C                   exsr      wrivat_b                                     => carico VAT
017700100525     C   51              exsr      wrivat_e                                     => carico VAT
017800071003     C                   else
017900080923     C*
018000071009     C                   if        wDISK = 'D'
018100091223     C                   exsr      repNSP
018200071009     C                   exsr      impvab
018300100412     C                   exsr      wrivab
018400100127     C                   exsr      wrivat_a                                     => carico VAT
018500071009     C                   exsr      wrivat_b                                     => carico VAT
018600071009     C                   exsr      wrivat_e                                     => carico VAT
018700071009     C                   else
018800071009     C*
018900010305     C                   if        depspe = *blanks                             => 1° giro
019000010305     C                   eval      depspe = curspe                              => memorizz. spediz
019100080117     C                   clear                   tivinds
019200091223     C                   exsr      repNSP
019300020305     C                   exsr      impvab
019400100127     C                   exsr      wrivat_a                                     => carico VAT
019500071003     C                   exsr      wrivat_b                                     => carico VAT
019600071003     C   50              exsr      wrivat_e                                     => carico VAT
019700010305     C                   else
019800020725     C                   if        depspe <> curspe                             => rottura di spediz
019900010305     C                   eval      depspe = curspe                              => memorizz. spediz
020000100412     C                   exsr      wrivab
020100080117     C                   clear                   tivinds
020200091223     C                   exsr      repNSP
020300020305     C                   exsr      impvab
020400100127     C                   exsr      wrivat_a                                     => carico VAT
020500071003     C                   exsr      wrivat_b                                     => carico VAT
020600071003     C   50              exsr      wrivat_e                                     => carico VAT
020700020305     C                   else                                                   => x stessa spediz
020800100401     C*
020900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021000100928     C                   select
021100100928     C                   when      wDUPREC = ' '
021200020305     C                   exsr      impvab
021300100127     C                   exsr      wrivat_a                                     => carico VAT
021400071003     C                   exsr      wrivat_b                                     => carico VAT
021500071003     C   50              exsr      wrivat_e                                     => carico VAT
021600100928     C                   when      wDUPREC = 'C'
021700100928     C   50              exsr      wrivat_e                                     => carico VAT
021800100928     C                   endsl
021900010305     C                   endif
022000010305     C                   endif
022100010305     C                   endif
022200071003     C                   endif
022300110201     C*
022400071009     C                   endif
022500000905     C*
022600000905     C                   else
022700020725     C                   eval      x_vinflg = '1'
022800000905     C                   endif
022900000905     C*
023000020725     C     VINRRN        chain     tivin000
023100020725     C                   if        %found(tivin00r)
023200020725     C                   eval      y_vinflg = x_vinflg
023300020725     C                   eval      y_vinmsg = x_vinmsg
023400020725     C                   update    tivin000
023500020725     C                   endif
023600020725     C*
023700020725     C/EXEC SQL
023800070530     C+ Fetch C1 into :INPUT_DS
023900020725     C/END-EXEC
024000020725     C*
024100020725     C                   enddo
024200020725     C*
024300020725     C/EXEC SQL
024400020725     C+ close C1
024500020725     C/END-EXEC
024600000905     C*
024700020305     C* Scarico i VAB rimasti "in sospeso"
024800071009     C                   if        wDISK = 'C'
024900100412     C                   exsr      wrivab
025000071009     C                   endif
025100010202     C*
025200010202     C                   endif
025300990910
025400990910     C* Se non ci sono record con errori ...
025500000710     C                   if        §ctrno = 0
025600990910     C* ... restituisco esito OK.
025700990921     C                   eval      wrkesito = '0'
025800990910     C                   else
025900100525     C                   if        §ctrokvb > 0 OR
026000100525     C                             §ctrokvt > 0
026100990921     C                   eval      wrkesito = '1'
026200000710     C                   else
026300000710     C                   eval      wrkesito = '2'
026400990910     C                   endif
026500000710     C                   endif
026600990910     C*
026700990914     C                   if        %open(tivin00r)
026800990908     C                   close     tivin00r
026900990914     C                   endif
027000021113     C                   if        %open(fivabwwr)
027100021113     C                   close     fivabwwr
027200990914     C                   endif
027300021113     C                   if        %open(fivatwwr)
027400021113     C                   close     fivatwwr
027500010201     C                   endif
027600990910     C*
027700100525     C                   if        §ctrokvb > 0 OR
027800100525     C                             §ctrokvt > 0
027900000724     C                             and vlrpoi <> *zeros
028000010202     C                   exsr      invio
028100990920     C                   endif
028200990920     C*
028300910830     C                   ENDSR
028400000613     C***
028500100412
028600100412
028700010305
028800010305     C*----------------------------------------------------*
028900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029000010305     C*----------------------------------------------------*
029100020305     C     WRIVAB        BEGSR
029200100412     C*
029300100412     C* Gestisco l'eventuale rottura x numero spedizione
029400100412     C                   if        VABRMA = *blanks
029500100412     C                   movel(P)  VABRMN        VABRMA
029600100412     C                   endif
029700100412     C*
029800100525     C                   if        *in51 = *off and *in31 = *off
029900100525     C                   write     fivab000                                     => scarico il VAB
030000100525     C                   endif
030100010305     C*
030200010305     C                   ENDSR
030300990920
030400000801     C*----------------------------------------------------*
030500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030600000801     C*----------------------------------------------------*
030700010201     C     INZVAR        BEGSR
030800000801     C*
030900010201     C                   Z-ADD     *zeros        Num5_0
031000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031100020725     C                   MOVEL     '0'           FlgCAS            1
031200000801     C*
031300000801     C                   ENDSR
031400000801     C*----------------------------------------------------*
031500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031600000801     C*----------------------------------------------------*
031700020904     C     DEFCAM        BEGSR
031800080923     C*
031900100928     C                   SETOFF                                       505160
032000000801     C*
032100021113     C                   CLEAR                   FIVAB000
032200021113     C                   CLEAR                   FIVAT000
032300070730     C* Imposto i valori di default...
032400120327     C                   EVAL      VABCCM = 1310906
032500120327     C                   EVAL      VATCCM = 1310906
032600120327     C                   EVAL      VABLNP = 131
032700120327     C                   EVAL      VATLNP = 131
032800111110     C                   EVAL      VABCTR = 000
032900100525     C                   EVAL      VABCBO = '1'
033000070222     C* ... e poi verifico se sono stati passati come parametri
033100070222     C                   IF        vlrppt > *blanks
033200100525     C*
033300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033500070222     C                   EXSR      CHKNUM
033600070222     C                   IF        PiInt=*on
033700070222     C                   Z-ADD     PiVal         VABCCM
033800070222     C                   Z-ADD     PiVal         VATCCM
033900070222     C                   ENDIF
034000070222     C                   ENDIF
034100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034300070222     C                   EXSR      CHKNUM
034400070222     C                   IF        PiInt=*on
034500070222     C                   Z-ADD     PiVal         VABLNP
034600070222     C                   Z-ADD     PiVal         VATLNP
034700070222     C                   ENDIF
034800070222     C                   ENDIF
034900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035100070222     C                   EXSR      CHKNUM
035200070222     C                   IF        PiInt=*on
035300070222     C                   Z-ADD     PiVal         VABCTR
035400070222     C                   ENDIF
035500070928     C                   ENDIF
035600071009     C                   MOVEL     *blanks       wDISK             1
035700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035800071009     C                   IF        wDISK <> *blanks
035900070928     C                   SETON                                        50
036000070222     C                   ENDIF
036100100525     C                   IF        wDISK  = 'T'
036200100525     C                   SETOFF                                       50
036300100525     C                   SETON                                        51
036400100525     C                   ENDIF
036500100401     C                   MOVEL     *blanks       wDUPREC           1
036600100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036700100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036800100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036900100525     C                   ENDIF
037000100525     C*
037100070222     C                   ENDIF
037200071009     C*
037300071009     C   50              EVAL      VABCTM = '7Q'
037400000801     C*
037500000801     C                   ENDSR
037600091223     C*----------------------------------------------------*
037700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037800091223     C*----------------------------------------------------*
037900091223     C     REPNSP        BEGSR
038000091223     C*
038100091223     C                   EXSR      INZVAR
038200091223     C                   EXSR      DEFCAM
038300100928     C*
038400100928     C                   SETON                                        60
038500091223     C*
038600091223     C* NSP => Stacco un numeratore da AZNUM
038700091223     C                   clear                   TRUL33DS
038800091223     C                   eval      I33OPE = *zeros
038900091223     C                   eval      I33CNU = 302
039000091223     C                   eval      I33NUM = 1
039100091223     C                   movel     TRUL33DS      KPJBU
039200091223     C                   call      'TRUL33R'
039300091223     C                   parm                    KPJBA
039400091223     C                   movel     KPJBU         TRUL33DS
039500091223     C                   if        O33ERR = *zeros
039600091223     C                   z-add     O33NRF        VABNSP
039700091223     C                   z-add     O33NRF        VATNSP
039800091223     C                   else
039900091223     C                   SETON                                        31
040000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100091223     C                             + ' ' + 'VABNSP VATNSP'
040200091223     C                   endif
040300091223     C*
040400091223     C                   ENDSR
040500000801     C*----------------------------------------------------*
040600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040700000801     C*----------------------------------------------------*
040800100730     C     IMPVAB        BEGSR
040900070530     C*
041000070530     C                   SETOFF                                       3132
041100070928     C*
041200070928     C                   MOVE(P)   vlrpoi        VABFGS
041300070928     C                   MOVE(P)   vlrpoi        VATFGS
041400070928     C*
041500070928     C                   MOVEL     datcor        VABAAS
041600070928     C                   MOVEL     datcor        VATAAS
041700070928     C                   MOVE      datcor        VABMGS
041800100729     C*
041900100729     C* Reperimento campi ALFA
042000120327     C                   IF        %trim(%subst(vindta:27:1)) = 'F'
042100120327     C                   EVAL      VABCBO='1'
042200120327     C                   ENDIF
042300120327     C                   IF        %trim(%subst(vindta:27:1)) = 'A'
042400120327     C                   EVAL      VABCBO='2'
042500120327     C                   ENDIF
042600120327     C                   EVAL      VABRMA=%trim(%subst(vindta:29:16))
042700120515     C                   EVAL      VABRMO=%trim(%subst(vindta:52:22))
042800120327     C                   EVAL      VABRSD=%trim(%subst(vindta:159:30))
042900120327     C                   EVAL      VABIND=%trim(%subst(vindta:189:36))
043000120327     C                   EVAL      VABLOD=%trim(%subst(vindta:225:24))
043100120327     C                   EVAL      VABPRD=%trim(%subst(vindta:249:2))
043200120404     C                   EVAL      VABCAD=%trim(%subst(vindta:251:5))
043300120328     C                   EVAL      VABNOT=%trim(%subst(vindta:298:30))
043400120328     C                   EVAL      VABNT2=%trim(%subst(vindta:328:30))
043500120327     C                   EVAL      VABTSP=*blank
043600120327     C                   IF        %trim(%subst(vindta:530:1)) = 'E'
043700120327     C                   EVAL      VABTSP='E'
043800120327     C                   ENDIF
043900120327     C                   EVAL      VABTC1=*blank
044000120327     C                   IF        %trim(%subst(vindta:531:1)) = 'A'
044100120327     C                   EVAL      VABTC1='A'
044200120327     C                   ENDIF
044300120327     C                   EVAL      VABTC2=*blank
044400120327     C                   IF        %trim(%subst(vindta:554:1)) = 'P'
044500120327     C                   EVAL      VABTC2='P'
044600120327     C                   ENDIF
044700120327     C                   EVAL      VABGC1=%trim(%subst(vindta:535:2))
044800100729     C*
044900100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
045000100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045100100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045200100729     C* ==
045300100729     C*
045400100729     C* Reperimento campi NUMERICI
045500100729     C                   MOVEL     DATCOR        VABAAS
045600100729     C                   MOVE      DATCOR        VABMGS
045700110408     C*
045800111020     C* NSP
045900101119     C                   IF        *IN51 = *off
046000120327     C                   EVAL      PiStr=%subst(vindta:12:7)
046100100730     C                   EXSR      CHKNUM
046200100730     C                   IF        PiInt=*on
046300100928     C  N60              Z-ADD     PiVal         VABNSP
046400120327     C  N60              Z-ADD     PiVal         VATNSP
046500100730     C                   ELSE
046600100730     C                   SETON                                        32
046700100730     C                   Z-ADD     1             VABRMN
046800100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900111020     C                             + ' ' + 'VABNSP VATNSP'
047000100730     C                   ENDIF
047100101119     C*
047200101119     C                   ELSE
047300101119     C*
047400120327     C                   EVAL      PiStr=%subst(vindta:12:7)
047500101119     C                   EXSR      CHKNUM
047600101119     C                   IF        PiInt=*on
047700101119     C  N60              Z-ADD     PiVal         VATNSP
047800101119     C                   ELSE
047900101119     C                   SETON                                        32
048000101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100101119     C                             + ' ' + 'VATNSP'
048200101119     C                   ENDIF
048300101119     C                   ENDIF
048400111020     C* RMN
048500120327     C                   EVAL      PiStr=%subst(vindta:12:7)
048600111020     C                   EXSR      CHKNUM
048700111020     C                   IF        PiInt=*on
048800111020     C                   Z-ADD     PiVal         VABRMN
048900111020     C                   ELSE
049000111020     C  N51              SETON                                        32
049100111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049200111020     C                             + ' ' + 'VABRMN'
049300111020     C                   ENDIF
049400110801     C* NCL
049500120327     C                   EVAL      PiStr=%subst(vindta:259:5)
049600110801     C                   EXSR      CHKNUM
049700110801     C                   IF        PiInt=*on
049800120330     C                   Z-ADD     PiVal         VABNCL
049900110801     C                   ELSE
050000110801     C  N51              SETON                                        32
050100111020     C  N51              Z-ADD     *zeros        VABNCL
050200110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050300110801     C                             + ' ' + 'VABNCL'
050400110801     C                   ENDIF
050500100729     C* PKB
050600120327     C                   EVAL      PiStr=%trim(%subst(vindta:270:7))
050700100729     C                   EXSR      CHKNUM
050800100729     C                   IF        PiNum=*on
050900120327     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
051000120330     C                   Z-ADD(H)  PiVal         VABPKB
051100100729     C                   ELSE
051200101119     C  N51              SETON                                        32
051300101119     C  N51              Z-ADD     *zeros        VABPKB
051400101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051500100729     C                             + ' ' + 'VABPKB'
051600100729     C                   ENDIF
051700120327     C* CAS
051800120515     C                   IF        %subst(vindta:396:11) <> *blanks AND
051900120515     C                             %subst(vindta:396:11) <> *all'0'
052000120327     C                   EVAL      PiStr=%trim(%subst(vindta:396:11))
052100120515     C                   EVAL      VABTIC='OM'
052200120327     C                   EXSR      CHKNUM
052300120327     C                   IF        PiNum=*on
052400120327     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
052500120330     C                   Z-ADD(H)  PiVal         VABCAS
052600120327     C                   ELSE
052700120327     C  N51              SETON                                        32
052800120327     C  N51              Z-ADD     *zeros        VABCAS
052900120327     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053000120327     C                             + ' ' + 'VABCAS'
053100120327     C                   ENDIF
053200120515     C                   ENDIF
053300120327     C* DCR
053400120327     C* ipotizzo che la data che mi arriva sia in formato AAAAMMGG come il nostro,
053500120327     C* il POC non ha mandato esempi
053600120327     C                   EVAL      PiStr=%subst(vindta:522:8)
053700120327     C                   EXSR      CHKNUM
053800120327     C                   IF        PiInt=*on
053900120327     C                   Z-ADD     PiVal         VABDCR
054000120327     C                   ELSE
054100120327     C  N51              SETON                                        32
054200120327     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054300120327     C                             + ' ' + 'VABDCR'
054400120327     C                   ENDIF
054500100729     C*
054600100729     C* Se provincia nn valorizzata la reperisco
054700100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
054800100729     C                   IF        VABNZD  = *blanks AND
054900100729     C                             VABPRD  = *blanks AND
055000100729     C                             VABCAD <> *blanks
055100100729     C                   CLEAR                   TISI95DS
055200100729     C                   EVAL      I95TCN = '3'
055300100729     C                   Z-ADD     datcor        I95DAT
055400100729     C                   EVAL      I95NAR = VABNZD
055500100729     C                   EVAL      I95CAP = VABCAD
055600100729     C                   EVAL      I95LOC = VABLOD
055700100729     C                   CALL      'TISI95R'
055800100729     C                   PARM                    TISI95DS
055900100729     C                   EVAL      VABPRD = O95PRV
056000100729     C                   ENDIF
056100100729     C*
056200100729     C* Considerazioni finali su CBO/CAS
056300100729     C                   IF        FlgCAS = '1'
056400100729     C                   IF        VABCBO = '1'
056500100729     C                   EVAL      VABCBO = '4'
056600100729     C                   ENDIF
056700100729     C                   IF        VABCBO = '2'
056800100729     C                   EVAL      VABCBO = '6'
056900100729     C                   ENDIF
057000100729     C                   ENDIF
057100010202     C*
057200000801     C* Ebbene...
057300000801     C                   ADD       1             §CTRMO
057400070530     C                   IF        *in31 <> *zeros OR
057500070530     C                             *in32 <> *zeros
057600000801     C                   ADD       1             §CTRNO
057700020725     C                   EVAL      x_vinflg = '2'
057800000801     C                   ELSE
057900010201     C                   ADD       1             §CTROKVB
058000000801     C                   ENDIF
058100000801     C*
058200000801     C                   ENDSR
058300100127     C*----------------------------------------------------*
058400100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
058500100127     C*----------------------------------------------------*
058600100127     C     WRIVAT_A      BEGSR
058700100127     C*
058800120327     C* NON GESTITO TIPO RCD A
058900120327     C*
059000100127     C* Valorizzo l buffer di scrittura del FIVAT
059100100127     C* - TIPO RECORD "A"
059200101119     C***                eval      VATTRC = 'A'
059300120327     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yy))
059400101119     C***                exsr      wrivat
059500100127     C*
059600100127     C                   ENDSR
059700010201     C*----------------------------------------------------*
059800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059900010201     C*----------------------------------------------------*
060000071003     C     WRIVAT_B      BEGSR
060100010201     C*
060200021113     C* Valorizzo l buffer di scrittura del FIVAT
060300070928     C* - TIPO RECORD "B"
060400120327     C                   eval      VATTRC = 'B'
060500120327     C                   eval      VATNOT = %trim(%subst(vindta:532:15))
060600120327     C                   exsr      wrivat
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
061800120330     C* i CHI SONO vanno sviluppati prendendo il valore passato come template
061900120330     C* togliendo le ultime 3 cifre e sostituendole con lo sviluppo
062000120330     C* sviluppo che parte da 1 e va in relazione al numero colli
062100120330     C                   MOVEL     *blanks       wWhoIsRef        30
062200120330     C                   eval      wWhoIsRef = %trim(%subst(vindta:492:30))
062300120330     C                   eval      wWhoIsRef = %subst(wWhoIsRef:1:
062400120403     C                              %len(%trim(wWhoIsRef)) - 6)
062500120330     C***                exsr      wrivat
062600120330     C*
062700120330     C* Sviluppati i "CHI SONO" in relazione al numero colli
062800120330     C*
062900120330     C* Inizializzo progressivo collo su totale colli della spedizione
063000121221     C                   Z-ADD     1             wNumColloDA       5 0
063100120330     C                   MOVEL     *blanks       wNumColloCor      3
063200120330     C***                MOVEL     *blanks       wWhoIs            6
063300120330     C*
063400120330     C                   DOW       wNumColloDA <= VABNCL
063500120330     C*
063600121221     C***                MOVE(P)   wNumColloDA   wNumColloCor
063700120330     C                   EVAL      VATNOT =
063800121221     C                              %trim(wWhoIsRef) +
063900121221     C                              %subst(%editc(wNumColloDA:'X') : 3 : 3)
064000120330     C                   exsr      wrivat
064100120330     C                   add       1             §CTROKVT
064200120330     C*
064300120330     C                   EVAL      wNumColloDA = wNumColloDA + 1
064400120330     C                   ENDDO
064500071003     C*
064600071003     C                   ENDSR
064700100127     C*----------------------------------------------------*
064800100127     C*  SCARICO BUFFER FIVAT
064900100127     C*----------------------------------------------------*
065000100127     C     WRIVAT        BEGSR
065100100127     C*
065200100127     C                   if        vatNOT <> *blanks
065300100127     C                   ADD       1             §CTROKVT
065400100127     C                   write     FIVAT000
065500100127     C                   endif
065600100127     C*
065700100127     C                   ENDSR
065800020904
065900020904
066000010202     C*----------------------------------------------------*
066100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
066200010202     C*----------------------------------------------------*
066300100525     C     PREVASX       BEGSR
066400100525     C*
066500021113     C* Compongo il nome del membro da dare al FIVATWWR
066600010202     C                   eval      parmbr = vlrhdl
066700010202     C                   movel     'M'           parmbr
066800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066900010202     C                   eval      paropz = '1'
067000100525     C*
067100010202     C* Effettuo la chiamata al CLLE preposto
067200100525     C  N51              call(e)   'TITVVTC'
067300010202     C                   parm                    parccm
067400010202     C                   parm                    parmbr
067500010202     C                   parm                    paropz
067600100525     C   51              call(e)   'TITVVBC'
067700100525     C                   parm                    parccm
067800100525     C                   parm                    parmbr
067900100525     C                   parm                    paropz
068000100525     C*
068100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
068200010202     C                   if        %error
068300010202     C                   movel     '1'           chkcall
068400010202     C                   else
068500010202     C                   movel     '0'           chkcall
068600010202     C                   endif
068700010202     C*
068800010202     C                   ENDSR
068900000801     C*----------------------------------------------------*
069000000801     C*  CONTROLLO NUMERICITA' CAMPI
069100000801     C*----------------------------------------------------*
069200000801     C     CHKNUM        BEGSR
069300081113     C*
069400081113     C                   IF        PiDecChr = *blanks
069500110408     C                   EVAL      PiDecChr = '.'
069600081113     C                   ENDIF
069700091223     C*
069800081113     C                   callp(e)  UBISNUM_Check(PiStr
069900081113     C                                          :PiDecChr
070000081113     C                                          :PiVal
070100081113     C                                          :PiNum
070200081113     C                                          :PiInt)
070300081113     C*
070400000801     C                   IF        %error
070500000801     C                   EVAL      PiInt=*off
070600000801     C                   ENDIF
070700000801     C*
070800000801     C                   ENDSR
070900000801     C***
071000011113
071100011113
071200000801
071300000801
071400990920      /TITLE Invio dei dati al punto operativo.
071500010202     C     invio         BEGSR
071600990920     C*
071700021113     C* 1° invio FIVAT
071800010201     C                   reset                   dscmz
071900110411     C                   move      vatfgs        cmzdst
072000021113     C                   eval      cmzfld = 'FIVATWWR'
072100010201     C                   eval      cmzmbd = vlrhdl
072200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072300021009     C***                if        prmfir = *blanks
072400021113     C                   eval      cmzfla = 'FIVAT00F'
072500021113     C                   eval      cmzmba = 'FIVAT00F'
072600021009     C***                else
072700021009     C***                eval      cmzfla = prmfir
072800021009     C***                eval      cmzmba = prmfir
072900021009     C***                endif
073000010201     C                   eval      cmznrr = *zeros
073100020305     C                   move      §ctrokvt      cmznrr
073200021018     C                   eval      cmzlba = vlrfl1
073300010201     C                   call(e)   'TIS711C'
073400010201     C                   parm                    dscmz
073500010201     C                   parm      *blanks       esito
073600010205     C                   if        %error
073700010205     C                             or cmzerr = '1'
073800010205     C                             or esito  = '1'
073900010205     C                   eval      wrkesito = '3'
074000010205     C                   else
074100010201     C*
074200021113     C* 2° invio FIVAB
074300100525     C                   if        *in51 = *off
074400010201     C                   reset                   dscmz
074500110411     C                   move      vabfgs        cmzdst
074600010201     C                   eval      cmzfld = vlrfou
074700010201     C                   eval      cmzmbd = vlrhdl
074800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074900021009     C***                if        prmfir = *blanks
075000021113     C                   eval      cmzfla = 'FIVAB00F'
075100021113     C                   eval      cmzmba = 'FIVAB00F'
075200021009     C***                else
075300021009     C***                eval      cmzfla = prmfir
075400021009     C***                eval      cmzmba = prmfir
075500021009     C***                endif
075600010201     C                   eval      cmznrr = *zeros
075700010201     C                   move      §ctrokvb      cmznrr
075800021018     C                   eval      cmzlba = vlrfl1
075900010201     C                   call(e)   'TIS711C'
076000010201     C                   parm                    dscmz
076100010201     C                   parm      *blanks       esito
076200010201     C                   if        %error
076300010201     C                             or cmzerr = '1'
076400010201     C                             or esito  = '1'
076500010201     C                   eval      wrkesito = '3'
076600010201     C                   endif
076700100525     C                   endif
076800010205     C                   endif
076900990920     C*
077000000613     C                   ENDSR
077100000613     C***
077200070411
077300110627     C     *pssr         BEGSR
077400070411     C*
077500110627     C                   if        %open(tivin00r)
077600110627     C                   close     tivin00r
077700110627     C                   endif
077800110627     C                   if        %open(fivabwwr)
077900110627     C                   close     fivabwwr
078000110627     C                   endif
078100110627     C                   if        %open(fivatwwr)
078200110627     C                   close     fivatwwr
078300110627     C                   endif
078400110627     C*
078500110627     C* Effettuo la chiamata al CLLE preposto
078600110627     C  N51              call(e)   'TITVVTC'
078700110627     C                   parm                    parccm
078800110627     C                   parm                    parmbr
078900110627     C                   parm      '2'           paropz
079000110627     C   51              call(e)   'TITVVBC'
079100110627     C                   parm                    parccm
079200110627     C                   parm                    parmbr
079300110627     C                   parm      '2'           paropz
079400110627     C*
079500110627     C                   eval      wrkesito = '2'
079600110627     C*
079700110627     C                   seton                                        LR
079800110627     C*
079900110627     C                   ENDSR     '*CANCL'
080000070411     C***
080100070411
080200990910
080300000613     C     *inzsr        BEGSR
080400990910     C*
080500990910     C     *entry        plist
080600990920     C                   parm                    tivlrds
080700990921     C                   parm      wrkesito      esito
080800000724     C                   parm                    prmlit
080900000710     C                   parm                    prmfir
081000000613     C*
081100000830     C* CALCOLA LA DATA CORRENTE
081200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081300091223     C                   eval      datcor = %dec(%date() : *ISO)
081400000830     C*
081500000613     C                   ENDSR
081600000613     C***
