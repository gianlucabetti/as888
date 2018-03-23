000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130312     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002100101119     D depspe          s             15    INZ(*blanks)
002200101119     D curspe          s             15    INZ(*blanks)
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
003600100729     D  VINSPE                        8
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
009900000801
010000910830     C*--------------------------------------------------------
010100070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010200910830     C*--------------------------------------------------------
010300070530     C     RWFILE        BEGSR
010400990910     C*
010500990914     C                   if        not %open(tivin00r)
010600990908     C                   open      tivin00r
010700990914     C                   endif
010800070530     C*
010900100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011000100525     C                   exsr      prevasx
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400100525     C                   if        not %open(fivabwwr)
011500100525     C                   open      fivabwwr
011600100525     C                   endif
011700100525     C*
011800021113     C                   if        not %open(fivatwwr)
011900021113     C                   open      fivatwwr
012000010201     C                   endif
012100080117     C*
012200080117     C                   EXSR      INZVAR
012300080117     C                   EXSR      DEFCAM
012400990910     C*
012500010201     C                   clear                   §CTROKVB          5 0
012600020305     C                   clear                   §CTROKVT          5 0
012700000801     C                   clear                   §CTRMO            5 0
012800000801     C                   clear                   §CTRNO            5 0
012900990910     C*
013000020725     C/EXEC SQL
013100020725     C+ declare C1 cursor for select
013200101119     C+ vindta, vinflg, substr(vindta, 80, 15) as sped, rrn(tivin00r)
013300060223     C+ from tivin00r
013400101119     C+ order by sped, substr(vindta, 80, 15) desc
013500020725     C+ for read only
013600020725     C/END-EXEC
013700020725     C
013800020725     C/EXEC SQL
013900020725     C+ open C1
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300070530     C+ Fetch C1 into :INPUT_DS
014400020725     C/END-EXEC
014500020725     C*
014600020725     C                   dow       sqlcod = *zeros
014700990913     C*
014800100310     C                   if        vindta > *blanks
014900000613     C                   add       1             rrnum
015000000801     C*
015100020725     C                   if        vinflg = *blanks
015200020725     C                             or vinflg = '0'
015300020725     C                             or vinflg = '2'
015400000801     C*
015500020725     C                   clear                   x_vinmsg
015600020725     C                   eval      x_vinflg = '1'
015700101119     C*
015800101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015900101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016000010305     C*
016100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016200101119     C                   EVAL      curspe=%trim(%subst(vindta:80:15))
016300010305     C*
016400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016500080923     C                   if        *in50 = *off
016600101119     C                   exsr      inzvar
016700101119     C                   exsr      defcam
016800071003     C                   exsr      impvab
016900100525     C                   exsr      wrivab
017000100127     C                   exsr      wrivat_a                                     => carico VAT
017100071003     C                   exsr      wrivat_b                                     => carico VAT
017200100525     C   51              exsr      wrivat_e                                     => carico VAT
017300071003     C                   else
017400080923     C*
017500071009     C                   if        wDISK = 'D'
017600091223     C                   exsr      repNSP
017700071009     C                   exsr      impvab
017800100412     C                   exsr      wrivab
017900100127     C                   exsr      wrivat_a                                     => carico VAT
018000071009     C                   exsr      wrivat_b                                     => carico VAT
018100071009     C                   exsr      wrivat_e                                     => carico VAT
018200071009     C                   else
018300071009     C*
018400010305     C                   if        depspe = *blanks                             => 1° giro
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600080117     C                   clear                   tivinds
018700091223     C                   exsr      repNSP
018800020305     C                   exsr      impvab
018900100127     C                   exsr      wrivat_a                                     => carico VAT
019000071003     C                   exsr      wrivat_b                                     => carico VAT
019100071003     C   50              exsr      wrivat_e                                     => carico VAT
019200010305     C                   else
019300020725     C                   if        depspe <> curspe                             => rottura di spediz
019400010305     C                   eval      depspe = curspe                              => memorizz. spediz
019500100412     C                   exsr      wrivab
019600080117     C                   clear                   tivinds
019700091223     C                   exsr      repNSP
019800020305     C                   exsr      impvab
019900100127     C                   exsr      wrivat_a                                     => carico VAT
020000071003     C                   exsr      wrivat_b                                     => carico VAT
020100071003     C   50              exsr      wrivat_e                                     => carico VAT
020200020305     C                   else                                                   => x stessa spediz
020300100401     C*
020400100401     C* Se nn richiesta esclusione spedizioni "duplicate"
020500100928     C                   select
020600100928     C                   when      wDUPREC = ' '
020700020305     C                   exsr      impvab
020800100127     C                   exsr      wrivat_a                                     => carico VAT
020900071003     C                   exsr      wrivat_b                                     => carico VAT
021000071003     C   50              exsr      wrivat_e                                     => carico VAT
021100100928     C                   when      wDUPREC = 'C'
021200100928     C   50              exsr      wrivat_e                                     => carico VAT
021300100928     C                   endsl
021400010305     C                   endif
021500010305     C                   endif
021600010305     C                   endif
021700071003     C                   endif
021800071009     C                   endif
021900000905     C*
022000000905     C                   else
022100020725     C                   eval      x_vinflg = '1'
022200000905     C                   endif
022300000905     C*
022400020725     C     VINRRN        chain     tivin000
022500020725     C                   if        %found(tivin00r)
022600020725     C                   eval      y_vinflg = x_vinflg
022700020725     C                   eval      y_vinmsg = x_vinmsg
022800020725     C                   update    tivin000
022900020725     C                   endif
023000020725     C*
023100020725     C/EXEC SQL
023200070530     C+ Fetch C1 into :INPUT_DS
023300020725     C/END-EXEC
023400020725     C*
023500020725     C                   enddo
023600020725     C*
023700020725     C/EXEC SQL
023800020725     C+ close C1
023900020725     C/END-EXEC
024000000905     C*
024100020305     C* Scarico i VAB rimasti "in sospeso"
024200071009     C                   if        wDISK = 'C'
024300100412     C                   exsr      wrivab
024400071009     C                   endif
024500010202     C*
024600010202     C                   endif
024700990910
024800990910     C* Se non ci sono record con errori ...
024900000710     C                   if        §ctrno = 0
025000990910     C* ... restituisco esito OK.
025100990921     C                   eval      wrkesito = '0'
025200990910     C                   else
025300100525     C                   if        §ctrokvb > 0 OR
025400100525     C                             §ctrokvt > 0
025500990921     C                   eval      wrkesito = '1'
025600000710     C                   else
025700000710     C                   eval      wrkesito = '2'
025800990910     C                   endif
025900000710     C                   endif
026000990910     C*
026100990914     C                   if        %open(tivin00r)
026200990908     C                   close     tivin00r
026300990914     C                   endif
026400021113     C                   if        %open(fivabwwr)
026500021113     C                   close     fivabwwr
026600990914     C                   endif
026700021113     C                   if        %open(fivatwwr)
026800021113     C                   close     fivatwwr
026900010201     C                   endif
027000990910     C*
027100100525     C                   if        §ctrokvb > 0 OR
027200100525     C                             §ctrokvt > 0
027300000724     C                             and vlrpoi <> *zeros
027400010202     C                   exsr      invio
027500990920     C                   endif
027600990920     C*
027700910830     C                   ENDSR
027800000613     C***
027900100412
028000100412
028100010305
028200010305     C*----------------------------------------------------*
028300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028400010305     C*----------------------------------------------------*
028500020305     C     WRIVAB        BEGSR
028600100412     C*
028700100412     C* Gestisco l'eventuale rottura x numero spedizione
028800100412     C                   if        VABRMA = *blanks
028900100412     C                   movel(P)  VABRMN        VABRMA
029000100412     C                   endif
029100100412     C*
029200100525     C                   if        *in51 = *off and *in31 = *off
029300100525     C                   write     fivab000                                     => scarico il VAB
029400100525     C                   endif
029500010305     C*
029600010305     C                   ENDSR
029700990920
029800000801     C*----------------------------------------------------*
029900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030000000801     C*----------------------------------------------------*
030100010201     C     INZVAR        BEGSR
030200000801     C*
030300010201     C                   Z-ADD     *zeros        Num5_0
030400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030500020725     C                   MOVEL     '0'           FlgCAS            1
030600000801     C*
030700000801     C                   ENDSR
030800000801     C*----------------------------------------------------*
030900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031000000801     C*----------------------------------------------------*
031100020904     C     DEFCAM        BEGSR
031200080923     C*
031300100928     C                   SETOFF                                       505160
031400000801     C*
031500021113     C                   CLEAR                   FIVAB000
031600021113     C                   CLEAR                   FIVAT000
031700070730     C* Imposto i valori di default...
031800101119     C                   EVAL      VABCCM = 0021984
031900101119     C                   EVAL      VATCCM = 0021984
032000101119     C                   EVAL      VABLNP = 002
032100101119     C                   EVAL      VATLNP = 002
032200100928     C                   EVAL      VABCTR = 000
032300100928     C                   EVAL      VABCTM = '7Q'
032400100525     C                   EVAL      VABCBO = '1'
032500070222     C* ... e poi verifico se sono stati passati come parametri
032600070222     C                   IF        vlrppt > *blanks
032700100525     C*
032800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
032900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033000070222     C                   EXSR      CHKNUM
033100070222     C                   IF        PiInt=*on
033200070222     C                   Z-ADD     PiVal         VABCCM
033300070222     C                   Z-ADD     PiVal         VATCCM
033400070222     C                   ENDIF
033500070222     C                   ENDIF
033600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033800070222     C                   EXSR      CHKNUM
033900070222     C                   IF        PiInt=*on
034000070222     C                   Z-ADD     PiVal         VABLNP
034100070222     C                   Z-ADD     PiVal         VATLNP
034200070222     C                   ENDIF
034300070222     C                   ENDIF
034400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034600070222     C                   EXSR      CHKNUM
034700070222     C                   IF        PiInt=*on
034800070222     C                   Z-ADD     PiVal         VABCTR
034900070222     C                   ENDIF
035000070928     C                   ENDIF
035100071009     C                   MOVEL     *blanks       wDISK             1
035200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035300071009     C                   IF        wDISK <> *blanks
035400070928     C                   SETON                                        50
035500070222     C                   ENDIF
035600100525     C                   IF        wDISK  = 'T'
035700100525     C                   SETOFF                                       50
035800100525     C                   SETON                                        51
035900100525     C                   ENDIF
036000100401     C                   MOVEL     *blanks       wDUPREC           1
036100100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036200100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036300100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036400100525     C                   ENDIF
036500100525     C*
036600070222     C                   ENDIF
036700071009     C*
036800071009     C   50              EVAL      VABCTM = '7Q'
036900000801     C*
037000000801     C                   ENDSR
037100091223     C*----------------------------------------------------*
037200091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037300091223     C*----------------------------------------------------*
037400091223     C     REPNSP        BEGSR
037500091223     C*
037600091223     C                   EXSR      INZVAR
037700091223     C                   EXSR      DEFCAM
037800100928     C*
037900100928     C                   SETON                                        60
038000091223     C*
038100091223     C* NSP => Stacco un numeratore da AZNUM
038200091223     C                   clear                   TRUL33DS
038300091223     C                   eval      I33OPE = *zeros
038400091223     C                   eval      I33CNU = 302
038500091223     C                   eval      I33NUM = 1
038600091223     C                   movel     TRUL33DS      KPJBU
038700091223     C                   call      'TRUL33R'
038800091223     C                   parm                    KPJBA
038900091223     C                   movel     KPJBU         TRUL33DS
039000091223     C                   if        O33ERR = *zeros
039100091223     C                   z-add     O33NRF        VABNSP
039200091223     C                   z-add     O33NRF        VATNSP
039300091223     C                   else
039400091223     C                   SETON                                        31
039500091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039600091223     C                             + ' ' + 'VABNSP VATNSP'
039700091223     C                   endif
039800091223     C*
039900091223     C                   ENDSR
040000000801     C*----------------------------------------------------*
040100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040200000801     C*----------------------------------------------------*
040300100730     C     IMPVAB        BEGSR
040400070530     C*
040500070530     C                   SETOFF                                       3132
040600070928     C*
040700070928     C                   MOVE(P)   vlrpoi        VABFGS
040800070928     C                   MOVE(P)   vlrpoi        VATFGS
040900070928     C*
041000070928     C                   MOVEL     datcor        VABAAS
041100070928     C                   MOVEL     datcor        VATAAS
041200070928     C                   MOVE      datcor        VABMGS
041300100729     C*
041400100729     C* Reperimento campi ALFA
041500101119     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
041600101119     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
041700101119     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
041800101119     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
041900101119     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
042000101119     C                   EVAL      VABCAD=%trim(%subst(vindta:316:7))
042100101117     C                   EVAL      VABNZD=%trim(%subst(vindta:674:3))
042200101117     C                   IF        VABNZD='I'   or
042300101117     C                             VABNZD='IT'  or
042400101117     C                             VABNZD='ITA'
042500101117     C                   EVAL      VABNZD=*blanks
042600101117     C                   ENDIF
042700101119     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
042800101119     C                   EVAL      VABNOT=%trim(%subst(vindta:464:60))
042900101119     C*
043000101119     C* Considerazioni particolari x giorni chiusura
043100101119     C                   SELECT
043200101119     C                   WHEN      %scan('LUNEDI':%subst(vindta:118:25)) > 0
043300101119     C                   EVAL      VABGC1 = '1'
043400101119     C                   WHEN      %scan('MARTEDI':%subst(vindta:118:25)) > 0
043500101119     C                   EVAL      VABGC1 = '2'
043600101119     C                   WHEN      %scan('MERCOLEDI':%subst(vindta:118:25)) > 0
043700101119     C                   EVAL      VABGC1 = '3'
043800101119     C                   WHEN      %scan('GIOVEDI':%subst(vindta:118:25)) > 0
043900101119     C                   EVAL      VABGC1 = '4'
044000101119     C                   WHEN      %scan('VENERDI':%subst(vindta:118:25)) > 0
044100101119     C                   EVAL      VABGC1 = '5'
044200101119     C                   WHEN      %scan('SABATO':%subst(vindta:118:25)) > 0
044300101119     C                   EVAL      VABGC1 = '6'
044400101119     C                   WHEN      %scan('DOMENICA':%subst(vindta:118:25)) > 0
044500101119     C                   EVAL      VABGC1 = '7'
044600101119     C                   ENDSL
044700101119     C*
044800101119     C                   SELECT
044900101119     C                   WHEN      %scan('POME':%subst(vindta:118:25)) > 0
045000101119     C                   EVAL      %subst(VABGC1:2:1) = 'P'
045100101119     C                   WHEN      %scan('MATT':%subst(vindta:118:25)) > 0
045200101119     C                   EVAL      %subst(VABGC1:2:1) = 'M'
045300101119     C                   ENDSL
045400100729     C*
045500100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
045600100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045700100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045800100729     C* ==
045900100729     C*
046000100729     C* Reperimento campi NUMERICI
046100100729     C                   MOVEL     DATCOR        VABAAS
046200100729     C                   MOVE      DATCOR        VABMGS
046300101119     C* DCR
046400101119     C                   IF        %subst(vindta:46:8) <> *blanks AND
046500101119     C                             %subst(vindta:46:8) <> *zeros
046600101119     C                   EVAL      PiStr=%trim(%subst(vindta:46:8))
046700101119     C                   EXSR      CHKNUM
046800101119     C                   IF        PiInt=*on
046900101119     C                   Z-ADD     PiVal         VABDCR
047000101119     C                   ELSE
047100101119     C  N51              SETON                                        32
047200101119     C  N51              Z-ADD     1             VABDCR
047300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
047400101119     C                             + ' ' + 'VABDCR'
047500101119     C                   ENDIF
047600101119     C                   ENDIF
047700101119     C* NSP / RMN
047800101119     C                   IF        *IN51 = *off
047900101119     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
048000101119     C     '/':'0'       XLATE     PiStr         PiStr
048100101119     C     'D':'4'       XLATE     PiStr         PiStr
048200101119     C     'F':'6'       XLATE     PiStr         PiStr
048300100730     C                   EXSR      CHKNUM
048400100730     C                   IF        PiInt=*on
048500100730     C                   Z-ADD     PiVal         VABRMN
048600100928     C  N60              Z-ADD     PiVal         VABNSP
048700100928     C  N60              Z-ADD     PiVal         VATNSP
048800100730     C                   ELSE
048900100730     C                   SETON                                        32
049000100730     C                   Z-ADD     1             VABRMN
049100100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
049300100730     C                   ENDIF
049400101119     C*
049500101119     C                   ELSE
049600101119     C*
049700101119     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
049800101119     C     '/':'0'       XLATE     PiStr         PiStr
049900101119     C     'D':'4'       XLATE     PiStr         PiStr
050000101119     C     'F':'6'       XLATE     PiStr         PiStr
050100101119     C                   EXSR      CHKNUM
050200101119     C                   IF        PiInt=*on
050300101119     C  N60              Z-ADD     PiVal         VATNSP
050400101119     C                   ELSE
050500101119     C                   SETON                                        32
050600101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700101119     C                             + ' ' + 'VATNSP'
050800101119     C                   ENDIF
050900101119     C                   ENDIF
051000100729     C* NCL
051100101119     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
051200100928     C                   EXSR      CHKNUM
051300100928     C                   IF        PiInt=*on
051400100928     C                   Z-ADD     PiVal         VABNCL
051500100928     C                   ELSE
051600101119     C  N51              SETON                                        32
051700101119     C  N51              Z-ADD     *zeros        VABNCL
051800101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051900100928     C                             + ' ' + 'VABNCL'
052000100928     C                   ENDIF
052100100729     C* PKB
052200101119     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
052300100729     C                   EXSR      CHKNUM
052400100729     C                   IF        PiNum=*on
052500101119     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
052600100729     C                   Z-ADD(H)  PiVal         VABPKB
052700100729     C                   ELSE
052800101119     C  N51              SETON                                        32
052900101119     C  N51              Z-ADD     *zeros        VABPKB
053000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053100100729     C                             + ' ' + 'VABPKB'
053200100729     C                   ENDIF
053300101117     C* VLB
053400101119     C                   IF        %subst(vindta:385:7) <> *zeros
053500101119     C                   EVAL      PiStr=%trim(%subst(vindta:385:7))
053600101117     C                   EXSR      CHKNUM
053700101117     C                   IF        PiNum=*on
053800101119     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
053900101117     C                   Z-ADD(H)  PiVal         VABVLB
054000101117     C                   ELSE
054100101119     C  N51              SETON                                        32
054200101119     C  N51              Z-ADD     *zeros        VABVLB
054300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054400101117     C                             + ' ' + 'VABVLB'
054500101117     C                   ENDIF
054600101119     C                   ENDIF
054700100729     C* CAS
054800101119     C                   IF        %subst(vindta:396:15)<> *blanks AND
054900101119     C                             %subst(vindta:396:15)<> *zeros
055000100729     C                   EVAL      FlgCAS = '1'
055100101119     C                   EVAL      VABVCA = %trim(%subst(vindta:392:4))
055200101119     C                   EVAL      VABTIC = 'BM'
055300101119     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
055400100729     C                   EXSR      CHKNUM
055500100729     C                   IF        PiNum=*on
055600101119     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
055700100928     C                   Z-ADD(H)  PiVal         VABCAS
055800100729     C                   ELSE
055900101119     C  N51              SETON                                        32
056000101119     C  N51              Z-ADD     *zeros        VABCAS
056100101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056200100729     C                             + ' ' + 'VABCAS'
056300100729     C                   ENDIF
056400101119     C                   ELSE
056500101119     C                   EVAL      FlgCAS = '0'
056600100729     C                   ENDIF
056700100729     C*
056800100729     C* Se provincia nn valorizzata la reperisco
056900100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
057000100729     C                   IF        VABNZD  = *blanks AND
057100100729     C                             VABPRD  = *blanks AND
057200100729     C                             VABCAD <> *blanks
057300100729     C                   CLEAR                   TISI95DS
057400100729     C                   EVAL      I95TCN = '3'
057500100729     C                   Z-ADD     datcor        I95DAT
057600100729     C                   EVAL      I95NAR = VABNZD
057700100729     C                   EVAL      I95CAP = VABCAD
057800100729     C                   EVAL      I95LOC = VABLOD
057900100729     C                   CALL      'TISI95R'
058000100729     C                   PARM                    TISI95DS
058100100729     C                   EVAL      VABPRD = O95PRV
058200100729     C                   ENDIF
058300100729     C*
058400100729     C* Considerazioni finali su CBO/CAS
058500100729     C                   IF        FlgCAS = '1'
058600100729     C                   IF        VABCBO = '1'
058700100729     C                   EVAL      VABCBO = '4'
058800100729     C                   ENDIF
058900100729     C                   IF        VABCBO = '2'
059000100729     C                   EVAL      VABCBO = '6'
059100100729     C                   ENDIF
059200100729     C                   ENDIF
059300010202     C*
059400000801     C* Ebbene...
059500000801     C                   ADD       1             §CTRMO
059600070530     C                   IF        *in31 <> *zeros OR
059700070530     C                             *in32 <> *zeros
059800000801     C                   ADD       1             §CTRNO
059900020725     C                   EVAL      x_vinflg = '2'
060000000801     C                   ELSE
060100010201     C                   ADD       1             §CTROKVB
060200000801     C                   ENDIF
060300000801     C*
060400000801     C                   ENDSR
060500100127     C*----------------------------------------------------*
060600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
060700100127     C*----------------------------------------------------*
060800100127     C     WRIVAT_A      BEGSR
060900100127     C*
061000100127     C* Valorizzo l buffer di scrittura del FIVAT
061100100127     C* - TIPO RECORD "A"
061200101119     C***                eval      VATTRC = 'A'
061300101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
061400101119     C***                exsr      wrivat
061500100127     C*
061600100127     C                   ENDSR
061700010201     C*----------------------------------------------------*
061800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061900010201     C*----------------------------------------------------*
062000071003     C     WRIVAT_B      BEGSR
062100010201     C*
062200021113     C* Valorizzo l buffer di scrittura del FIVAT
062300070928     C* - TIPO RECORD "B"
062400101119     C***                eval      VATTRC = 'B'
062500101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
062600101119     C***                exsr      wrivat
062700010201     C*
062800010201     C                   ENDSR
062900071003     C*----------------------------------------------------*
063000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063100071003     C*----------------------------------------------------*
063200071003     C     WRIVAT_E      BEGSR
063300071003     C*
063400071003     C* Valorizzo l buffer di scrittura del FIVAT
063500071003     C* - TIPO RECORD "E"
063600100729     C*
063700100928     C                   eval      VATTRC = 'E'
063800101119     C                   eval      VATNOT = %trim(%subst(vindta:16:20))
063900100928     C                   exsr      wrivat
064000071003     C*
064100071003     C                   ENDSR
064200100127     C*----------------------------------------------------*
064300100127     C*  SCARICO BUFFER FIVAT
064400100127     C*----------------------------------------------------*
064500100127     C     WRIVAT        BEGSR
064600100127     C*
064700100127     C                   if        vatNOT <> *blanks
064800100127     C                   ADD       1             §CTROKVT
064900100127     C                   write     FIVAT000
065000100127     C                   endif
065100100127     C*
065200100127     C                   ENDSR
065300020904
065400020904
065500010202     C*----------------------------------------------------*
065600100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
065700010202     C*----------------------------------------------------*
065800100525     C     PREVASX       BEGSR
065900100525     C*
066000021113     C* Compongo il nome del membro da dare al FIVATWWR
066100010202     C                   eval      parmbr = vlrhdl
066200010202     C                   movel     'M'           parmbr
066300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066400010202     C                   eval      paropz = '1'
066500100525     C*
066600010202     C* Effettuo la chiamata al CLLE preposto
066700100525     C  N51              call(e)   'TITVVTC'
066800010202     C                   parm                    parccm
066900010202     C                   parm                    parmbr
067000010202     C                   parm                    paropz
067100100525     C   51              call(e)   'TITVVBC'
067200100525     C                   parm                    parccm
067300100525     C                   parm                    parmbr
067400100525     C                   parm                    paropz
067500100525     C*
067600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067700010202     C                   if        %error
067800010202     C                   movel     '1'           chkcall
067900010202     C                   else
068000010202     C                   movel     '0'           chkcall
068100010202     C                   endif
068200010202     C*
068300010202     C                   ENDSR
068400000801     C*----------------------------------------------------*
068500000801     C*  CONTROLLO NUMERICITA' CAMPI
068600000801     C*----------------------------------------------------*
068700000801     C     CHKNUM        BEGSR
068800081113     C*
068900081113     C                   IF        PiDecChr = *blanks
069000100503     C                   EVAL      PiDecChr = '.'
069100081113     C                   ENDIF
069200091223     C*
069300081113     C                   callp(e)  UBISNUM_Check(PiStr
069400081113     C                                          :PiDecChr
069500081113     C                                          :PiVal
069600081113     C                                          :PiNum
069700081113     C                                          :PiInt)
069800081113     C*
069900000801     C                   IF        %error
070000000801     C                   EVAL      PiInt=*off
070100000801     C                   ENDIF
070200000801     C*
070300000801     C                   ENDSR
070400000801     C***
070500011113
070600011113
070700000801
070800000801
070900990920      /TITLE Invio dei dati al punto operativo.
071000010202     C     invio         BEGSR
071100990920     C*
071200021113     C* 1° invio FIVAT
071300010201     C                   reset                   dscmz
071400010201     C                   move      vlrpoi        cmzdst
071500021113     C                   eval      cmzfld = 'FIVATWWR'
071600010201     C                   eval      cmzmbd = vlrhdl
071700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071800021009     C***                if        prmfir = *blanks
071900021113     C                   eval      cmzfla = 'FIVAT00F'
072000021113     C                   eval      cmzmba = 'FIVAT00F'
072100021009     C***                else
072200021009     C***                eval      cmzfla = prmfir
072300021009     C***                eval      cmzmba = prmfir
072400021009     C***                endif
072500010201     C                   eval      cmznrr = *zeros
072600020305     C                   move      §ctrokvt      cmznrr
072700021018     C                   eval      cmzlba = vlrfl1
072800010201     C                   call(e)   'TIS711C'
072900010201     C                   parm                    dscmz
073000010201     C                   parm      *blanks       esito
073100010205     C                   if        %error
073200010205     C                             or cmzerr = '1'
073300010205     C                             or esito  = '1'
073400010205     C                   eval      wrkesito = '3'
073500010205     C                   else
073600010201     C*
073700021113     C* 2° invio FIVAB
073800100525     C                   if        *in51 = *off
073900010201     C                   reset                   dscmz
074000010201     C                   move      vlrpoi        cmzdst
074100010201     C                   eval      cmzfld = vlrfou
074200010201     C                   eval      cmzmbd = vlrhdl
074300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074400021009     C***                if        prmfir = *blanks
074500021113     C                   eval      cmzfla = 'FIVAB00F'
074600021113     C                   eval      cmzmba = 'FIVAB00F'
074700021009     C***                else
074800021009     C***                eval      cmzfla = prmfir
074900021009     C***                eval      cmzmba = prmfir
075000021009     C***                endif
075100010201     C                   eval      cmznrr = *zeros
075200010201     C                   move      §ctrokvb      cmznrr
075300021018     C                   eval      cmzlba = vlrfl1
075400010201     C                   call(e)   'TIS711C'
075500010201     C                   parm                    dscmz
075600010201     C                   parm      *blanks       esito
075700010201     C                   if        %error
075800010201     C                             or cmzerr = '1'
075900010201     C                             or esito  = '1'
076000010201     C                   eval      wrkesito = '3'
076100010201     C                   endif
076200100525     C                   endif
076300010205     C                   endif
076400990920     C*
076500000613     C                   ENDSR
076600000613     C***
076700070411
076800070411     C     *pssr         BEGSR
076900070411     C*
077000070411     C                   if        %open(tivin00r)
077100070411     C                   close     tivin00r
077200070411     C                   endif
077300070411     C                   if        %open(fivabwwr)
077400070411     C                   close     fivabwwr
077500070411     C                   endif
077600070411     C                   if        %open(fivatwwr)
077700070411     C                   close     fivatwwr
077800070411     C                   endif
077900070411     C*
078000070411     C* Effettuo la chiamata al CLLE preposto
078100100525     C  N51              call(e)   'TITVVTC'
078200100525     C                   parm                    parccm
078300100525     C                   parm                    parmbr
078400100525     C                   parm      '2'           paropz
078500100525     C   51              call(e)   'TITVVBC'
078600100525     C                   parm                    parccm
078700100525     C                   parm                    parmbr
078800100525     C                   parm      '2'           paropz
078900070411     C*
079000070411     C                   eval      wrkesito = '2'
079100070411     C*
079200070411     C                   seton                                        LR
079300070411     C*
079400070411     C                   ENDSR     '*CANCL'
079500070411     C***
079600070411
079700990910
079800000613     C     *inzsr        BEGSR
079900990910     C*
080000990910     C     *entry        plist
080100990920     C                   parm                    tivlrds
080200990921     C                   parm      wrkesito      esito
080300000724     C                   parm                    prmlit
080400000710     C                   parm                    prmfir
080500000613     C*
080600000830     C* CALCOLA LA DATA CORRENTE
080700091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080800091223     C                   eval      datcor = %dec(%date() : *ISO)
080900000830     C*
081000000613     C                   ENDSR
081100000613     C***
