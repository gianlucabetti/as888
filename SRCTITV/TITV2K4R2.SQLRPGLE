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
002100110201     D depspe          s             10    INZ(*blanks)
002200110201     D curspe          s             10    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900130911     D IdColloIn       s             15  0
003000141030     D wX              s              2  0
003100020725
003200020725     D*------------------
003300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003400020725     D*------------------
003500070530     D INPUT_DS        DS                  INZ
003600100729     D  VINDTA                     2048
003700100729     D  VINFLG                        1
003800120423     D  VINSPE                       10
003900100729     D  VINRRN                        8  0
004000020725     D*
004100081113
004200091223     D*------------------
004300091223     D* DS REPERIMENTO NUMERATORE
004400091223     D*------------------
004500100309     D trul33ds      e ds                  inz
004600091223     D*------------------
004700091223     D* DS ARCHITETTURA
004800091223     D*------------------
004900091223     D kpjba         e ds                  inz
005000091223
005100101119
005200101119     D*-------------------
005300101119     D* COSTANTI
005400101119     D*-------------------
005500101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005600101119     D                                     èéù')
005700101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005800101119     D                                     EEU')
005900081113
006000081113     D*------------------
006100081113     D* LINKING A DEFINIZIONI ESTERNE
006200081113     D*------------------
006300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006400090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006500081113
006600990908
006700010201
006800010201
006900080117     C*
007000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007100080117     C
007200080117     C/EXEC SQL
007300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007400080117     C/END-EXEC
007500080117     C
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900000613     C*
008000100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008200000613     C*
008300010202     C* Effettuo la chiamata al CLLE preposto
008400100525     C  N51              call(e)   'TITVVTC'
008500100525     C                   parm                    parccm
008600100525     C                   parm                    parmbr
008700100525     C                   parm      '2'           paropz
008800100525     C   51              call(e)   'TITVVBC'
008900100525     C                   parm                    parccm
009000100525     C                   parm                    parmbr
009100100525     C                   parm      '2'           paropz
009200070730     C*
009300070730     C* Effettuo lancio TISI95 solo x chiusura
009400070730     C                   CLEAR                   TISI95DS
009500070730     C                   EVAL      I95TLA = 'C'
009600070730     C                   CALL      'TISI95R'
009700070730     C                   PARM                    TISI95DS
009800000616     C*
009900000801     C
010000010201     C                   seton                                        LR
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
013400110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013500060223     C+ from tivin00r
013600110201     C+ order by sped, substr(vindta, 108, 10) desc
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
016200110201     C*
016300110201     C* Skippo il record finale 'EOF'
016400110201     C                   IF        %trim(vindta) = 'EOF'
016500110201     C                   eval      x_vinflg = '1'
016600110201     C                   ELSE
016700010305     C*
016800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016900110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
017000010305     C*
017100100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017200080923     C                   if        *in50 = *off
017300101119     C                   exsr      inzvar
017400101119     C                   exsr      defcam
017500071003     C                   exsr      impvab
017600100525     C                   exsr      wrivab
017700100127     C                   exsr      wrivat_a                                     => carico VAT
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900140725     C                   exsr      wrivat_i                                     => carico VAT
018000140725     C                   exsr      wrivat_s                                     => carico VAT
018100100525     C   51              exsr      wrivat_e                                     => carico VAT
018200071003     C                   else
018300080923     C*
018400071009     C                   if        wDISK = 'D'
018500091223     C                   exsr      repNSP
018600071009     C                   exsr      impvab
018700100412     C                   exsr      wrivab
018800100127     C                   exsr      wrivat_a                                     => carico VAT
018900071009     C                   exsr      wrivat_b                                     => carico VAT
019000140725     C                   exsr      wrivat_i                                     => carico VAT
019100140725     C                   exsr      wrivat_s                                     => carico VAT
019200071009     C                   exsr      wrivat_e                                     => carico VAT
019300071009     C                   else
019400071009     C*
019500010305     C                   if        depspe = *blanks                             => 1° giro
019600010305     C                   eval      depspe = curspe                              => memorizz. spediz
019700080117     C                   clear                   tivinds
019800091223     C                   exsr      repNSP
019900020305     C                   exsr      impvab
020000100127     C                   exsr      wrivat_a                                     => carico VAT
020100071003     C                   exsr      wrivat_b                                     => carico VAT
020200140725     C                   exsr      wrivat_i                                     => carico VAT
020300140725     C                   exsr      wrivat_s                                     => carico VAT
020400071003     C   50              exsr      wrivat_e                                     => carico VAT
020500010305     C                   else
020600020725     C                   if        depspe <> curspe                             => rottura di spediz
020700010305     C                   eval      depspe = curspe                              => memorizz. spediz
020800100412     C                   exsr      wrivab
020900080117     C                   clear                   tivinds
021000091223     C                   exsr      repNSP
021100020305     C                   exsr      impvab
021200100127     C                   exsr      wrivat_a                                     => carico VAT
021300071003     C                   exsr      wrivat_b                                     => carico VAT
021400140725     C                   exsr      wrivat_i                                     => carico VAT
021500140725     C                   exsr      wrivat_s                                     => carico VAT
021600071003     C   50              exsr      wrivat_e                                     => carico VAT
021700020305     C                   else                                                   => x stessa spediz
021800100401     C*
021900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
022000100928     C                   select
022100100928     C                   when      wDUPREC = ' '
022200020305     C                   exsr      impvab
022300100127     C                   exsr      wrivat_a                                     => carico VAT
022400071003     C                   exsr      wrivat_b                                     => carico VAT
022500140725     C                   exsr      wrivat_i                                     => carico VAT
022600140725     C                   exsr      wrivat_s                                     => carico VAT
022700071003     C   50              exsr      wrivat_e                                     => carico VAT
022800100928     C                   when      wDUPREC = 'C'
022900100928     C   50              exsr      wrivat_e                                     => carico VAT
023000100928     C                   endsl
023100010305     C                   endif
023200010305     C                   endif
023300010305     C                   endif
023400071003     C                   endif
023500110201     C*
023600110201     C                   endif
023700071009     C                   endif
023800000905     C*
023900000905     C                   else
024000020725     C                   eval      x_vinflg = '1'
024100000905     C                   endif
024200000905     C*
024300020725     C     VINRRN        chain     tivin000
024400020725     C                   if        %found(tivin00r)
024500020725     C                   eval      y_vinflg = x_vinflg
024600020725     C                   eval      y_vinmsg = x_vinmsg
024700020725     C                   update    tivin000
024800020725     C                   endif
024900020725     C*
025000020725     C/EXEC SQL
025100070530     C+ Fetch C1 into :INPUT_DS
025200020725     C/END-EXEC
025300020725     C*
025400020725     C                   enddo
025500020725     C*
025600020725     C/EXEC SQL
025700020725     C+ close C1
025800020725     C/END-EXEC
025900000905     C*
026000020305     C* Scarico i VAB rimasti "in sospeso"
026100071009     C                   if        wDISK = 'C'
026200100412     C                   exsr      wrivab
026300071009     C                   endif
026400010202     C*
026500010202     C                   endif
026600990910
026700990910     C* Se non ci sono record con errori ...
026800000710     C                   if        §ctrno = 0
026900990910     C* ... restituisco esito OK.
027000990921     C                   eval      wrkesito = '0'
027100990910     C                   else
027200100525     C                   if        §ctrokvb > 0 OR
027300100525     C                             §ctrokvt > 0
027400990921     C                   eval      wrkesito = '1'
027500000710     C                   else
027600000710     C                   eval      wrkesito = '2'
027700990910     C                   endif
027800000710     C                   endif
027900990910     C*
028000990914     C                   if        %open(tivin00r)
028100990908     C                   close     tivin00r
028200990914     C                   endif
028300021113     C                   if        %open(fivabwwr)
028400021113     C                   close     fivabwwr
028500990914     C                   endif
028600021113     C                   if        %open(fivatwwr)
028700021113     C                   close     fivatwwr
028800010201     C                   endif
028900990910     C*
029000100525     C                   if        §ctrokvb > 0 OR
029100100525     C                             §ctrokvt > 0
029200000724     C                             and vlrpoi <> *zeros
029300010202     C                   exsr      invio
029400990920     C                   endif
029500990920     C*
029600910830     C                   ENDSR
029700000613     C***
029800100412
029900100412
030000010305
030100010305     C*----------------------------------------------------*
030200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030300010305     C*----------------------------------------------------*
030400020305     C     WRIVAB        BEGSR
030500100412     C*
030600100412     C* Gestisco l'eventuale rottura x numero spedizione
030700100412     C                   if        VABRMA = *blanks
030800100412     C                   movel(P)  VABRMN        VABRMA
030900100412     C                   endif
031000100412     C*
031100100525     C                   if        *in51 = *off and *in31 = *off
031200100525     C                   write     fivab000                                     => scarico il VAB
031300100525     C                   endif
031400010305     C*
031500010305     C                   ENDSR
031600990920
031700000801     C*----------------------------------------------------*
031800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031900000801     C*----------------------------------------------------*
032000010201     C     INZVAR        BEGSR
032100000801     C*
032200010201     C                   Z-ADD     *zeros        Num5_0
032300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032400020725     C                   MOVEL     '0'           FlgCAS            1
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032900000801     C*----------------------------------------------------*
033000020904     C     DEFCAM        BEGSR
033100080923     C*
033200100928     C                   SETOFF                                       505160
033300000801     C*
033400021113     C                   CLEAR                   FIVAB000
033500021113     C                   CLEAR                   FIVAT000
033600070730     C* Imposto i valori di default...
033700140725     C                   EVAL      VABCCM = 0790568
033800140725     C                   EVAL      VATCCM = 0790568
033900140725     C                   EVAL      VABLNP = 079
034000140725     C                   EVAL      VATLNP = 079
034100110128     C                   EVAL      VABCTR = 000
034200100525     C                   EVAL      VABCBO = '1'
034300070222     C* ... e poi verifico se sono stati passati come parametri
034400070222     C                   IF        vlrppt > *blanks
034500100525     C*
034600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034800070222     C                   EXSR      CHKNUM
034900070222     C                   IF        PiInt=*on
035000070222     C                   Z-ADD     PiVal         VABCCM
035100070222     C                   Z-ADD     PiVal         VATCCM
035200070222     C                   ENDIF
035300070222     C                   ENDIF
035400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035600070222     C                   EXSR      CHKNUM
035700070222     C                   IF        PiInt=*on
035800070222     C                   Z-ADD     PiVal         VABLNP
035900070222     C                   Z-ADD     PiVal         VATLNP
036000070222     C                   ENDIF
036100070222     C                   ENDIF
036200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036400070222     C                   EXSR      CHKNUM
036500070222     C                   IF        PiInt=*on
036600070222     C                   Z-ADD     PiVal         VABCTR
036700070222     C                   ENDIF
036800070928     C                   ENDIF
036900071009     C                   MOVEL     *blanks       wDISK             1
037000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037100071009     C                   IF        wDISK <> *blanks
037200070928     C                   SETON                                        50
037300070222     C                   ENDIF
037400100525     C                   IF        wDISK  = 'T'
037500100525     C                   SETOFF                                       50
037600100525     C                   SETON                                        51
037700100525     C                   ENDIF
037800100401     C                   MOVEL     *blanks       wDUPREC           1
037900100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
038000100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
038100100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038200100525     C                   ENDIF
038300100525     C*
038400070222     C                   ENDIF
038500071009     C*
038600071009     C   50              EVAL      VABCTM = '7Q'
038700000801     C*
038800000801     C                   ENDSR
038900091223     C*----------------------------------------------------*
039000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039100091223     C*----------------------------------------------------*
039200091223     C     REPNSP        BEGSR
039300091223     C*
039400091223     C                   EXSR      INZVAR
039500091223     C                   EXSR      DEFCAM
039600100928     C*
039700100928     C                   SETON                                        60
039800091223     C*
039900091223     C* NSP => Stacco un numeratore da AZNUM
040000091223     C                   clear                   TRUL33DS
040100091223     C                   eval      I33OPE = *zeros
040200091223     C                   eval      I33CNU = 302
040300091223     C                   eval      I33NUM = 1
040400091223     C                   movel     TRUL33DS      KPJBU
040500091223     C                   call      'TRUL33R'
040600091223     C                   parm                    KPJBA
040700091223     C                   movel     KPJBU         TRUL33DS
040800091223     C                   if        O33ERR = *zeros
040900091223     C                   z-add     O33NRF        VABNSP
041000091223     C                   z-add     O33NRF        VATNSP
041100091223     C                   else
041200091223     C                   SETON                                        31
041300091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041400091223     C                             + ' ' + 'VABNSP VATNSP'
041500091223     C                   endif
041600091223     C*
041700091223     C                   ENDSR
041800000801     C*----------------------------------------------------*
041900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042000000801     C*----------------------------------------------------*
042100100730     C     IMPVAB        BEGSR
042200070530     C*
042300070530     C                   SETOFF                                       3132
042400070928     C*
042500070928     C                   MOVE(P)   vlrpoi        VABFGS
042600070928     C                   MOVE(P)   vlrpoi        VATFGS
042700070928     C*
042800070928     C                   MOVEL     datcor        VABAAS
042900070928     C                   MOVEL     datcor        VATAAS
043000070928     C                   MOVE      datcor        VABMGS
043100100729     C*
043200100729     C* Reperimento campi ALFA
043300110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
043400110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
043500110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
043600110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
043700110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
043800150520     C                   EVAL      VABNZD=%trim(%subst(vindta:257:3))
043900150520     C                   IF        VABNZD = 'I' or
044000150520     C                             VABNZD = 'IT' or
044100150520     C                             VABNZD = 'ITA'
044200150520     C                   EVAL      VABNZD = *blank
044300150520     C                   ENDIF
044400110127     C*
044500110127     C                   SELECT
044600110201     C                   WHEN      %subst(vindta:207:1) = 'F'
044700110201     C                   EVAL      VABCBO = '1'
044800110201     C                   WHEN      %subst(vindta:207:1) = 'A'
044900110201     C                   EVAL      VABCBO = '2'
045000140725     C                   ENDSL
045100150528     C*
045200150528     C                   SELECT
045300150528     C                   WHEN      %subst(vindta:263:1) = 'D'
045400150528     C                   EVAL      VABCTM = '7R'
045500150528     C                   WHEN      %subst(vindta:263:1) = 'E'
045600150528     C                   EVAL      VABCTM = '7Q'
045700150528     C                   WHEN      %subst(vindta:263:1) = 'F'
045800150528     C                   EVAL      VABCTM = '7Q'
045900150528     C                   WHEN      %subst(vindta:263:1) = *BLANK
046000150528     C                   EVAL      VABCTM = '7Q'
046100150528     C                   OTHER
046200150528     C                   EVAL      VABCTM = '7Q'
046300150528     C                   ENDSL
046400110127     C*
046500140725     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
046600150520     C*** le note non vengono più passate
046700150520     C***                EVAL      VABNOT=%trim(%subst(vindta:257:600))
046800140725     C                   EVAL      VABNT2=%trim(%subst(vindta:857:10))
046900140725     C                   EVAL      VABTSP=%trim(%subst(vindta:1028:50))
047000100729     C*
047100100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
047200100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
047300100729     C     '@':'A'       XLATE     VABRSD        VABRSD
047400100729     C* ==
047500100729     C*
047600100729     C* Reperimento campi NUMERICI
047700100729     C                   MOVEL     DATCOR        VABAAS
047800100729     C                   MOVE      DATCOR        VABMGS
047900110127     C* NSP / RMN
048000101119     C                   IF        *IN51 = *off
048100140725     C                   EVAL      PiStr=%trim(%subst(vindta:108:6))
048200100730     C                   EXSR      CHKNUM
048300100730     C                   IF        PiInt=*on
048400100730     C                   Z-ADD     PiVal         VABRMN
048500100928     C  N60              Z-ADD     PiVal         VABNSP
048600100928     C  N60              Z-ADD     PiVal         VATNSP
048700100730     C                   ELSE
048800100730     C                   SETON                                        32
048900100730     C                   Z-ADD     1             VABRMN
049000100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
049200100730     C                   ENDIF
049300101119     C*
049400101119     C                   ELSE
049500101119     C*
049600140725     C                   EVAL      PiStr=%trim(%subst(vindta:108:6))
049700101119     C                   EXSR      CHKNUM
049800101119     C                   IF        PiInt=*on
049900101119     C  N60              Z-ADD     PiVal         VATNSP
050000101119     C                   ELSE
050100101119     C                   SETON                                        32
050200101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300101119     C                             + ' ' + 'VATNSP'
050400101119     C                   ENDIF
050500101119     C                   ENDIF
050600110201     C* NCL
050700110201     C                   z-add     *zeros        wCOLLI            5 0
050800110201     C                   z-add     *zeros        wBANCALI          5 0
050900110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
051000100928     C                   EXSR      CHKNUM
051100100928     C                   IF        PiInt=*on
051200110201     C                   Z-ADD     PiVal         wCOLLI
051300100928     C                   ELSE
051400101119     C  N51              SETON                                        32
051500101119     C  N51              Z-ADD     *zeros        VABNCL
051600101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051700100928     C                             + ' ' + 'VABNCL'
051800100928     C                   ENDIF
051900110201     C*
052000140728     C***                EVAL      PiStr=%trim(%subst(vindta:129:2))
052100140728     C***                EXSR      CHKNUM
052200140728     C***                IF        PiInt=*on
052300140728     C***                Z-ADD     PiVal         wBANCALI
052400140728     C***                ELSE
052500140728     C**N51              SETON                                        32
052600140728     C**N51              Z-ADD     *zeros        VABNCL
052700140728     C**N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052800140728     C***                          + ' ' + 'VABNCL'
052900140728     C***                ENDIF
053000110201     C*
053100110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
053200100729     C* PKB
053300110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
053400100729     C                   EXSR      CHKNUM
053500100729     C                   IF        PiNum=*on
053600110201     C                   Z-ADD     PiVal         VABPKB
053700100729     C                   ELSE
053800101119     C  N51              SETON                                        32
053900101119     C  N51              Z-ADD     *zeros        VABPKB
054000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054100100729     C                             + ' ' + 'VABPKB'
054200100729     C                   ENDIF
054300110201     C* VLB
054400140725     C                   IF        %trim(%subst(vindta:234:11)) <> '0,0'
054500140725     C                   EVAL      PiStr=%trim(%subst(vindta:234:11))
054600140725     C                   EXSR      CHKNUM
054700140725     C                   IF        PiNum=*on
054800140725     C                   Z-ADD(H)  PiVal         VABVLB
054900140725     C                   ELSE
055000140725     C  N51              SETON                                        32
055100140725     C  N51              Z-ADD     *zeros        VABVLB
055200140725     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055300140725     C                             + ' ' + 'VABVLB'
055400140725     C                   ENDIF
055500140725     C                   ENDIF
055600100729     C* CAS
055700140725     C                   IF        %subst(vindta:137:10)<> *blanks      AND
055800140725     C                             %subst(vindta:137:10)<> *zeros       AND
055900140725     C                             %subst(vindta:137:10)<> '0000000,00' AND
056000140725     C                             %trim(%subst(vindta:137:10))<> '0,00'
056100100729     C                   EVAL      FlgCAS = '1'
056200110127     C                   EVAL      VABVCA = 'EUR'
056300140725     C                   EVAL      VABTIC=%trim(%subst(vindta:147:40))
056400110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
056500100729     C                   EXSR      CHKNUM
056600100729     C                   IF        PiNum=*on
056700100928     C                   Z-ADD(H)  PiVal         VABCAS
056800100729     C                   ELSE
056900101119     C  N51              SETON                                        32
057000101119     C  N51              Z-ADD     *zeros        VABCAS
057100101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057200100729     C                             + ' ' + 'VABCAS'
057300100729     C                   ENDIF
057400101119     C                   ELSE
057500101119     C                   EVAL      FlgCAS = '0'
057600100729     C                   ENDIF
057700110201     C* IAS
057800140725     C                   IF        %subst(vindta:223:11)<> *blanks       AND
057900140725     C                             %subst(vindta:223:11)<> *zeros        AND
058000140725     C                             %subst(vindta:223:11)<> '00000000,00' AND
058100140725     C                             %trim(%subst(vindta:223:11))<> '0,00'
058200110201     C                   EVAL      VABVAS = 'EUR'
058300110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
058400110201     C                   EXSR      CHKNUM
058500110201     C                   IF        PiNum=*on
058600110201     C                   Z-ADD(H)  PiVal         VABIAS
058700110201     C                   ELSE
058800110201     C  N51              SETON                                        32
058900110201     C  N51              Z-ADD     *zeros        VABIAS
059000110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
059100110201     C                             + ' ' + 'VABIAS'
059200110201     C                   ENDIF
059300110201     C                   ENDIF
059400150520     C* CTR
059500150520     C                   EVAL      PiStr=%trim(%subst(vindta:260:3))
059600150520     C                   EXSR      CHKNUM
059700150520     C                   IF        PiInt=*on
059800150520     C                   Z-ADD     PiVal         VABCTR
059900150520     C                   ELSE
060000150520     C  N51              SETON                                        32
060100150520     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
060200150520     C                             + ' ' + 'VABCTR'
060300150520     C                   ENDIF
060400100729     C*
060500100729     C* Se provincia nn valorizzata la reperisco
060600100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
060700100729     C                   IF        VABNZD  = *blanks AND
060800100729     C                             VABPRD  = *blanks AND
060900100729     C                             VABCAD <> *blanks
061000100729     C                   CLEAR                   TISI95DS
061100100729     C                   EVAL      I95TCN = '3'
061200100729     C                   Z-ADD     datcor        I95DAT
061300100729     C                   EVAL      I95NAR = VABNZD
061400100729     C                   EVAL      I95CAP = VABCAD
061500100729     C                   EVAL      I95LOC = VABLOD
061600100729     C                   CALL      'TISI95R'
061700100729     C                   PARM                    TISI95DS
061800100729     C                   EVAL      VABPRD = O95PRV
061900100729     C                   ENDIF
062000100729     C*
062100100729     C* Considerazioni finali su CBO/CAS
062200100729     C                   IF        FlgCAS = '1'
062300100729     C                   IF        VABCBO = '1'
062400100729     C                   EVAL      VABCBO = '4'
062500100729     C                   ENDIF
062600100729     C                   IF        VABCBO = '2'
062700100729     C                   EVAL      VABCBO = '6'
062800100729     C                   ENDIF
062900100729     C                   ENDIF
063000010202     C*
063100000801     C* Ebbene...
063200000801     C                   ADD       1             §CTRMO
063300070530     C                   IF        *in31 <> *zeros OR
063400070530     C                             *in32 <> *zeros
063500000801     C                   ADD       1             §CTRNO
063600020725     C                   EVAL      x_vinflg = '2'
063700000801     C                   ELSE
063800010201     C                   ADD       1             §CTROKVB
063900000801     C                   ENDIF
064000000801     C*
064100000801     C                   ENDSR
064200100127     C*----------------------------------------------------*
064300100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
064400100127     C*----------------------------------------------------*
064500100127     C     WRIVAT_A      BEGSR
064600100127     C*
064700100127     C* Valorizzo l buffer di scrittura del FIVAT
064800100127     C* - TIPO RECORD "A"
064900140725     C                   eval      VATTRC = 'A'
065000140725     C                   eval      VATNOT = %trim(%subst(vindta:1008:10))
065100140725     C                   exsr      wrivat
065200100127     C*
065300100127     C                   ENDSR
065400010201     C*----------------------------------------------------*
065500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065600010201     C*----------------------------------------------------*
065700071003     C     WRIVAT_B      BEGSR
065800010201     C*
065900021113     C* Valorizzo l buffer di scrittura del FIVAT
066000070928     C* - TIPO RECORD "B"
066100101119     C***                eval      VATTRC = 'B'
066200101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
066300101119     C***                exsr      wrivat
066400010201     C*
066500010201     C                   ENDSR
066600140725     C*----------------------------------------------------*
066700140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
066800140725     C*----------------------------------------------------*
066900140725     C     WRIVAT_I      BEGSR
067000140725     C*
067100140725     C* Valorizzo l buffer di scrittura del FIVAT
067200140725     C* - TIPO RECORD "I" / "J"
067300140728     C***                movel     *blanks       wTRC             70
067400140728     C***                eval      wTRC   = %trim(%subst(vindta:938:70))
067500140728     C***                eval      VATTRC = 'I'
067600140728     C***                eval      VATNOT = %subst(wTRC:01:35)
067700140728     C***                exsr      wrivat
067800140728     C***                eval      VATTRC = 'J'
067900140728     C***                eval      VATNOT = %subst(wTRC:36:35)
068000140728     C***                exsr      wrivat
068100140725     C*
068200140725     C                   ENDSR
068300140725     C*----------------------------------------------------*
068400140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
068500140725     C*----------------------------------------------------*
068600140725     C     WRIVAT_S      BEGSR
068700140725     C*
068800140725     C* Valorizzo l buffer di scrittura del FIVAT
068900140725     C* - TIPO RECORD "S"
069000140725     C                   eval      VATTRC = 'S'
069100140725     C                   eval      VATNOT = %trim(%subst(vindta:1018:10))
069200140725     C                   exsr      wrivat
069300140725     C*
069400140725     C                   ENDSR
069500071003     C*----------------------------------------------------*
069600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
069700071003     C*----------------------------------------------------*
069800071003     C     WRIVAT_E      BEGSR
069900071003     C*
070000071003     C* Valorizzo l buffer di scrittura del FIVAT
070100071003     C* - TIPO RECORD "E"
070200100729     C*
070300130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
070400130911     C*
070500130911     C*
070600100928     C                   eval      VATTRC = 'E'
070700141030     C***                eval      IdColloIn = %dec((%subst(vindta:897:15))
070800141030     C***                                       : 15 : 0)
070900130911     C*
071000141030     C     1             DO        VABNCL        wX
071100130911     C                   eval      IdColloIn = IdColloIn + 1
071200141030     C                   EVAL      VATNOT = 'CC' +
071300141030     C                              %editc(%dec(%subst(vindta:897:7):7:0):'X') +
071400141030     C                              %editc(%dec(%subst(vindta:118:2):2:0):'X') +
071500141030     C                              %editc(%dec(%subst(vindta:108:8):8:0):'X') +
071600141030     C                              %editc(wX:'X')
071700130911     C*
071800100928     C                   exsr      wrivat
071900130911     C                   ENDDO
072000071003     C*
072100071003     C                   ENDSR
072200100127     C*----------------------------------------------------*
072300100127     C*  SCARICO BUFFER FIVAT
072400100127     C*----------------------------------------------------*
072500100127     C     WRIVAT        BEGSR
072600100127     C*
072700100127     C                   if        vatNOT <> *blanks
072800100127     C                   ADD       1             §CTROKVT
072900100127     C                   write     FIVAT000
073000100127     C                   endif
073100100127     C*
073200100127     C                   ENDSR
073300020904
073400020904
073500010202     C*----------------------------------------------------*
073600100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
073700010202     C*----------------------------------------------------*
073800100525     C     PREVASX       BEGSR
073900100525     C*
074000021113     C* Compongo il nome del membro da dare al FIVATWWR
074100010202     C                   eval      parmbr = vlrhdl
074200010202     C                   movel     'M'           parmbr
074300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
074400010202     C                   eval      paropz = '1'
074500100525     C*
074600010202     C* Effettuo la chiamata al CLLE preposto
074700100525     C  N51              call(e)   'TITVVTC'
074800010202     C                   parm                    parccm
074900010202     C                   parm                    parmbr
075000010202     C                   parm                    paropz
075100100525     C   51              call(e)   'TITVVBC'
075200100525     C                   parm                    parccm
075300100525     C                   parm                    parmbr
075400100525     C                   parm                    paropz
075500100525     C*
075600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
075700010202     C                   if        %error
075800010202     C                   movel     '1'           chkcall
075900010202     C                   else
076000010202     C                   movel     '0'           chkcall
076100010202     C                   endif
076200010202     C*
076300010202     C                   ENDSR
076400000801     C*----------------------------------------------------*
076500000801     C*  CONTROLLO NUMERICITA' CAMPI
076600000801     C*----------------------------------------------------*
076700000801     C     CHKNUM        BEGSR
076800081113     C*
076900081113     C                   IF        PiDecChr = *blanks
077000110201     C                   EVAL      PiDecChr = ','
077100081113     C                   ENDIF
077200091223     C*
077300081113     C                   callp(e)  UBISNUM_Check(PiStr
077400081113     C                                          :PiDecChr
077500081113     C                                          :PiVal
077600081113     C                                          :PiNum
077700081113     C                                          :PiInt)
077800081113     C*
077900000801     C                   IF        %error
078000000801     C                   EVAL      PiInt=*off
078100000801     C                   ENDIF
078200000801     C*
078300000801     C                   ENDSR
078400000801     C***
078500011113
078600011113
078700000801
078800000801
078900990920      /TITLE Invio dei dati al punto operativo.
079000010202     C     invio         BEGSR
079100990920     C*
079200021113     C* 1° invio FIVAT
079300010201     C                   reset                   dscmz
079400010201     C                   move      vlrpoi        cmzdst
079500021113     C                   eval      cmzfld = 'FIVATWWR'
079600010201     C                   eval      cmzmbd = vlrhdl
079700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079800021009     C***                if        prmfir = *blanks
079900021113     C                   eval      cmzfla = 'FIVAT00F'
080000021113     C                   eval      cmzmba = 'FIVAT00F'
080100021009     C***                else
080200021009     C***                eval      cmzfla = prmfir
080300021009     C***                eval      cmzmba = prmfir
080400021009     C***                endif
080500010201     C                   eval      cmznrr = *zeros
080600020305     C                   move      §ctrokvt      cmznrr
080700021018     C                   eval      cmzlba = vlrfl1
080800010201     C                   call(e)   'TIS711C'
080900010201     C                   parm                    dscmz
081000010201     C                   parm      *blanks       esito
081100010205     C                   if        %error
081200010205     C                             or cmzerr = '1'
081300010205     C                             or esito  = '1'
081400010205     C                   eval      wrkesito = '3'
081500010205     C                   else
081600010201     C*
081700021113     C* 2° invio FIVAB
081800100525     C                   if        *in51 = *off
081900010201     C                   reset                   dscmz
082000010201     C                   move      vlrpoi        cmzdst
082100010201     C                   eval      cmzfld = vlrfou
082200010201     C                   eval      cmzmbd = vlrhdl
082300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082400021009     C***                if        prmfir = *blanks
082500021113     C                   eval      cmzfla = 'FIVAB00F'
082600021113     C                   eval      cmzmba = 'FIVAB00F'
082700021009     C***                else
082800021009     C***                eval      cmzfla = prmfir
082900021009     C***                eval      cmzmba = prmfir
083000021009     C***                endif
083100010201     C                   eval      cmznrr = *zeros
083200010201     C                   move      §ctrokvb      cmznrr
083300021018     C                   eval      cmzlba = vlrfl1
083400010201     C                   call(e)   'TIS711C'
083500010201     C                   parm                    dscmz
083600010201     C                   parm      *blanks       esito
083700010201     C                   if        %error
083800010201     C                             or cmzerr = '1'
083900010201     C                             or esito  = '1'
084000010201     C                   eval      wrkesito = '3'
084100010201     C                   endif
084200100525     C                   endif
084300010205     C                   endif
084400990920     C*
084500000613     C                   ENDSR
084600000613     C***
084700070411
084800070411     C     *pssr         BEGSR
084900070411     C*
085000070411     C                   if        %open(tivin00r)
085100070411     C                   close     tivin00r
085200070411     C                   endif
085300070411     C                   if        %open(fivabwwr)
085400070411     C                   close     fivabwwr
085500070411     C                   endif
085600070411     C                   if        %open(fivatwwr)
085700070411     C                   close     fivatwwr
085800070411     C                   endif
085900070411     C*
086000070411     C* Effettuo la chiamata al CLLE preposto
086100100525     C  N51              call(e)   'TITVVTC'
086200100525     C                   parm                    parccm
086300100525     C                   parm                    parmbr
086400100525     C                   parm      '2'           paropz
086500100525     C   51              call(e)   'TITVVBC'
086600100525     C                   parm                    parccm
086700100525     C                   parm                    parmbr
086800100525     C                   parm      '2'           paropz
086900070411     C*
087000070411     C                   eval      wrkesito = '2'
087100070411     C*
087200070411     C                   seton                                        LR
087300070411     C*
087400070411     C                   ENDSR     '*CANCL'
087500070411     C***
087600070411
087700990910
087800000613     C     *inzsr        BEGSR
087900990910     C*
088000990910     C     *entry        plist
088100990920     C                   parm                    tivlrds
088200990921     C                   parm      wrkesito      esito
088300000724     C                   parm                    prmlit
088400000710     C                   parm                    prmfir
088500000613     C*
088600000830     C* CALCOLA LA DATA CORRENTE
088700091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088800091223     C                   eval      datcor = %dec(%date() : *ISO)
088900000830     C*
089000000613     C                   ENDSR
089100000613     C***
