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
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500100729     D  VINDTA                     2048
003600100729     D  VINFLG                        1
003700120423     D  VINSPE                       10
003800100729     D  VINRRN                        8  0
003900020725     D*
004000081113
004100091223     D*------------------
004200091223     D* DS REPERIMENTO NUMERATORE
004300091223     D*------------------
004400100309     D trul33ds      e ds                  inz
004500091223     D*------------------
004600091223     D* DS ARCHITETTURA
004700091223     D*------------------
004800091223     D kpjba         e ds                  inz
004900091223
005000101119
005100101119     D*-------------------
005200101119     D* COSTANTI
005300101119     D*-------------------
005400101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005500101119     D                                     èéù')
005600101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005700101119     D                                     EEU')
005800081113
005900081113     D*------------------
006000081113     D* LINKING A DEFINIZIONI ESTERNE
006100081113     D*------------------
006200081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006300090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006400081113
006500990908
006600010201
006700010201
006800080117     C*
006900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000080117     C
007100080117     C/EXEC SQL
007200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300080117     C/END-EXEC
007400080117     C
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800000613     C*
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
013300110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500110201     C+ order by sped, substr(vindta, 108, 10) desc
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
016100110201     C*
016200110201     C* Skippo il record finale 'EOF'
016300110201     C                   IF        %trim(vindta) = 'EOF'
016400110201     C                   eval      x_vinflg = '1'
016500110201     C                   ELSE
016600010305     C*
016700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016800110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
016900010305     C*
017000100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017100080923     C                   if        *in50 = *off
017200101119     C                   exsr      inzvar
017300101119     C                   exsr      defcam
017400071003     C                   exsr      impvab
017500100525     C                   exsr      wrivab
017600100127     C                   exsr      wrivat_a                                     => carico VAT
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800140725     C                   exsr      wrivat_i                                     => carico VAT
017900140725     C                   exsr      wrivat_s                                     => carico VAT
018000100525     C   51              exsr      wrivat_e                                     => carico VAT
018100071003     C                   else
018200080923     C*
018300071009     C                   if        wDISK = 'D'
018400091223     C                   exsr      repNSP
018500071009     C                   exsr      impvab
018600100412     C                   exsr      wrivab
018700100127     C                   exsr      wrivat_a                                     => carico VAT
018800071009     C                   exsr      wrivat_b                                     => carico VAT
018900140725     C                   exsr      wrivat_i                                     => carico VAT
019000140725     C                   exsr      wrivat_s                                     => carico VAT
019100071009     C                   exsr      wrivat_e                                     => carico VAT
019200071009     C                   else
019300071009     C*
019400010305     C                   if        depspe = *blanks                             => 1° giro
019500010305     C                   eval      depspe = curspe                              => memorizz. spediz
019600080117     C                   clear                   tivinds
019700091223     C                   exsr      repNSP
019800020305     C                   exsr      impvab
019900100127     C                   exsr      wrivat_a                                     => carico VAT
020000071003     C                   exsr      wrivat_b                                     => carico VAT
020100140725     C                   exsr      wrivat_i                                     => carico VAT
020200140725     C                   exsr      wrivat_s                                     => carico VAT
020300071003     C   50              exsr      wrivat_e                                     => carico VAT
020400010305     C                   else
020500020725     C                   if        depspe <> curspe                             => rottura di spediz
020600010305     C                   eval      depspe = curspe                              => memorizz. spediz
020700100412     C                   exsr      wrivab
020800080117     C                   clear                   tivinds
020900091223     C                   exsr      repNSP
021000020305     C                   exsr      impvab
021100100127     C                   exsr      wrivat_a                                     => carico VAT
021200071003     C                   exsr      wrivat_b                                     => carico VAT
021300140725     C                   exsr      wrivat_i                                     => carico VAT
021400140725     C                   exsr      wrivat_s                                     => carico VAT
021500071003     C   50              exsr      wrivat_e                                     => carico VAT
021600020305     C                   else                                                   => x stessa spediz
021700100401     C*
021800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021900100928     C                   select
022000100928     C                   when      wDUPREC = ' '
022100020305     C                   exsr      impvab
022200100127     C                   exsr      wrivat_a                                     => carico VAT
022300071003     C                   exsr      wrivat_b                                     => carico VAT
022400140725     C                   exsr      wrivat_i                                     => carico VAT
022500140725     C                   exsr      wrivat_s                                     => carico VAT
022600071003     C   50              exsr      wrivat_e                                     => carico VAT
022700100928     C                   when      wDUPREC = 'C'
022800100928     C   50              exsr      wrivat_e                                     => carico VAT
022900100928     C                   endsl
023000010305     C                   endif
023100010305     C                   endif
023200010305     C                   endif
023300071003     C                   endif
023400110201     C*
023500110201     C                   endif
023600071009     C                   endif
023700000905     C*
023800000905     C                   else
023900020725     C                   eval      x_vinflg = '1'
024000000905     C                   endif
024100000905     C*
024200020725     C     VINRRN        chain     tivin000
024300020725     C                   if        %found(tivin00r)
024400020725     C                   eval      y_vinflg = x_vinflg
024500020725     C                   eval      y_vinmsg = x_vinmsg
024600020725     C                   update    tivin000
024700020725     C                   endif
024800020725     C*
024900020725     C/EXEC SQL
025000070530     C+ Fetch C1 into :INPUT_DS
025100020725     C/END-EXEC
025200020725     C*
025300020725     C                   enddo
025400020725     C*
025500020725     C/EXEC SQL
025600020725     C+ close C1
025700020725     C/END-EXEC
025800000905     C*
025900020305     C* Scarico i VAB rimasti "in sospeso"
026000071009     C                   if        wDISK = 'C'
026100100412     C                   exsr      wrivab
026200071009     C                   endif
026300010202     C*
026400010202     C                   endif
026500990910
026600990910     C* Se non ci sono record con errori ...
026700000710     C                   if        §ctrno = 0
026800990910     C* ... restituisco esito OK.
026900990921     C                   eval      wrkesito = '0'
027000990910     C                   else
027100100525     C                   if        §ctrokvb > 0 OR
027200100525     C                             §ctrokvt > 0
027300990921     C                   eval      wrkesito = '1'
027400000710     C                   else
027500000710     C                   eval      wrkesito = '2'
027600990910     C                   endif
027700000710     C                   endif
027800990910     C*
027900990914     C                   if        %open(tivin00r)
028000990908     C                   close     tivin00r
028100990914     C                   endif
028200021113     C                   if        %open(fivabwwr)
028300021113     C                   close     fivabwwr
028400990914     C                   endif
028500021113     C                   if        %open(fivatwwr)
028600021113     C                   close     fivatwwr
028700010201     C                   endif
028800990910     C*
028900100525     C                   if        §ctrokvb > 0 OR
029000100525     C                             §ctrokvt > 0
029100000724     C                             and vlrpoi <> *zeros
029200010202     C                   exsr      invio
029300990920     C                   endif
029400990920     C*
029500910830     C                   ENDSR
029600000613     C***
029700100412
029800100412
029900010305
030000010305     C*----------------------------------------------------*
030100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030200010305     C*----------------------------------------------------*
030300020305     C     WRIVAB        BEGSR
030400100412     C*
030500100412     C* Gestisco l'eventuale rottura x numero spedizione
030600100412     C                   if        VABRMA = *blanks
030700100412     C                   movel(P)  VABRMN        VABRMA
030800100412     C                   endif
030900100412     C*
031000100525     C                   if        *in51 = *off and *in31 = *off
031100100525     C                   write     fivab000                                     => scarico il VAB
031200100525     C                   endif
031300010305     C*
031400010305     C                   ENDSR
031500990920
031600000801     C*----------------------------------------------------*
031700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031800000801     C*----------------------------------------------------*
031900010201     C     INZVAR        BEGSR
032000000801     C*
032100010201     C                   Z-ADD     *zeros        Num5_0
032200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032300020725     C                   MOVEL     '0'           FlgCAS            1
032400000801     C*
032500000801     C                   ENDSR
032600000801     C*----------------------------------------------------*
032700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032800000801     C*----------------------------------------------------*
032900020904     C     DEFCAM        BEGSR
033000080923     C*
033100100928     C                   SETOFF                                       505160
033200000801     C*
033300021113     C                   CLEAR                   FIVAB000
033400021113     C                   CLEAR                   FIVAT000
033500070730     C* Imposto i valori di default...
033600140725     C                   EVAL      VABCCM = 0790568
033700140725     C                   EVAL      VATCCM = 0790568
033800140725     C                   EVAL      VABLNP = 079
033900140725     C                   EVAL      VATLNP = 079
034000110128     C                   EVAL      VABCTR = 000
034100100525     C                   EVAL      VABCBO = '1'
034200070222     C* ... e poi verifico se sono stati passati come parametri
034300070222     C                   IF        vlrppt > *blanks
034400100525     C*
034500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034700070222     C                   EXSR      CHKNUM
034800070222     C                   IF        PiInt=*on
034900070222     C                   Z-ADD     PiVal         VABCCM
035000070222     C                   Z-ADD     PiVal         VATCCM
035100070222     C                   ENDIF
035200070222     C                   ENDIF
035300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035500070222     C                   EXSR      CHKNUM
035600070222     C                   IF        PiInt=*on
035700070222     C                   Z-ADD     PiVal         VABLNP
035800070222     C                   Z-ADD     PiVal         VATLNP
035900070222     C                   ENDIF
036000070222     C                   ENDIF
036100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036300070222     C                   EXSR      CHKNUM
036400070222     C                   IF        PiInt=*on
036500070222     C                   Z-ADD     PiVal         VABCTR
036600070222     C                   ENDIF
036700070928     C                   ENDIF
036800071009     C                   MOVEL     *blanks       wDISK             1
036900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037000071009     C                   IF        wDISK <> *blanks
037100070928     C                   SETON                                        50
037200070222     C                   ENDIF
037300100525     C                   IF        wDISK  = 'T'
037400100525     C                   SETOFF                                       50
037500100525     C                   SETON                                        51
037600100525     C                   ENDIF
037700100401     C                   MOVEL     *blanks       wDUPREC           1
037800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037900100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
038000100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038100100525     C                   ENDIF
038200100525     C*
038300070222     C                   ENDIF
038400071009     C*
038500071009     C   50              EVAL      VABCTM = '7Q'
038600000801     C*
038700000801     C                   ENDSR
038800091223     C*----------------------------------------------------*
038900091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039000091223     C*----------------------------------------------------*
039100091223     C     REPNSP        BEGSR
039200091223     C*
039300091223     C                   EXSR      INZVAR
039400091223     C                   EXSR      DEFCAM
039500100928     C*
039600100928     C                   SETON                                        60
039700091223     C*
039800091223     C* NSP => Stacco un numeratore da AZNUM
039900091223     C                   clear                   TRUL33DS
040000091223     C                   eval      I33OPE = *zeros
040100091223     C                   eval      I33CNU = 302
040200091223     C                   eval      I33NUM = 1
040300091223     C                   movel     TRUL33DS      KPJBU
040400091223     C                   call      'TRUL33R'
040500091223     C                   parm                    KPJBA
040600091223     C                   movel     KPJBU         TRUL33DS
040700091223     C                   if        O33ERR = *zeros
040800091223     C                   z-add     O33NRF        VABNSP
040900091223     C                   z-add     O33NRF        VATNSP
041000091223     C                   else
041100091223     C                   SETON                                        31
041200091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300091223     C                             + ' ' + 'VABNSP VATNSP'
041400091223     C                   endif
041500091223     C*
041600091223     C                   ENDSR
041700000801     C*----------------------------------------------------*
041800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041900000801     C*----------------------------------------------------*
042000100730     C     IMPVAB        BEGSR
042100070530     C*
042200070530     C                   SETOFF                                       3132
042300070928     C*
042400070928     C                   MOVE(P)   vlrpoi        VABFGS
042500070928     C                   MOVE(P)   vlrpoi        VATFGS
042600070928     C*
042700070928     C                   MOVEL     datcor        VABAAS
042800070928     C                   MOVEL     datcor        VATAAS
042900070928     C                   MOVE      datcor        VABMGS
043000100729     C*
043100100729     C* Reperimento campi ALFA
043200110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
043300110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
043400110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
043500110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
043600110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
043700110127     C*
043800110127     C                   SELECT
043900110201     C                   WHEN      %subst(vindta:207:1) = 'F'
044000110201     C                   EVAL      VABCBO = '1'
044100110201     C                   WHEN      %subst(vindta:207:1) = 'A'
044200110201     C                   EVAL      VABCBO = '2'
044300140725     C                   ENDSL
044400110127     C*
044500140725     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
044600140725     C                   EVAL      VABNOT=%trim(%subst(vindta:257:600))
044700140725     C                   EVAL      VABNT2=%trim(%subst(vindta:857:10))
044800140725     C                   EVAL      VABTSP=%trim(%subst(vindta:1028:50))
044900100729     C*
045000100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
045100100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045200100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045300100729     C* ==
045400100729     C*
045500100729     C* Reperimento campi NUMERICI
045600100729     C                   MOVEL     DATCOR        VABAAS
045700100729     C                   MOVE      DATCOR        VABMGS
045800110127     C* NSP / RMN
045900101119     C                   IF        *IN51 = *off
046000140725     C                   EVAL      PiStr=%trim(%subst(vindta:108:6))
046100100730     C                   EXSR      CHKNUM
046200100730     C                   IF        PiInt=*on
046300100730     C                   Z-ADD     PiVal         VABRMN
046400100928     C  N60              Z-ADD     PiVal         VABNSP
046500100928     C  N60              Z-ADD     PiVal         VATNSP
046600100730     C                   ELSE
046700100730     C                   SETON                                        32
046800100730     C                   Z-ADD     1             VABRMN
046900100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047000101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
047100100730     C                   ENDIF
047200101119     C*
047300101119     C                   ELSE
047400101119     C*
047500140725     C                   EVAL      PiStr=%trim(%subst(vindta:108:6))
047600101119     C                   EXSR      CHKNUM
047700101119     C                   IF        PiInt=*on
047800101119     C  N60              Z-ADD     PiVal         VATNSP
047900101119     C                   ELSE
048000101119     C                   SETON                                        32
048100101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048200101119     C                             + ' ' + 'VATNSP'
048300101119     C                   ENDIF
048400101119     C                   ENDIF
048500110201     C* NCL
048600110201     C                   z-add     *zeros        wCOLLI            5 0
048700110201     C                   z-add     *zeros        wBANCALI          5 0
048800110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
048900100928     C                   EXSR      CHKNUM
049000100928     C                   IF        PiInt=*on
049100110201     C                   Z-ADD     PiVal         wCOLLI
049200100928     C                   ELSE
049300101119     C  N51              SETON                                        32
049400101119     C  N51              Z-ADD     *zeros        VABNCL
049500101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049600100928     C                             + ' ' + 'VABNCL'
049700100928     C                   ENDIF
049800110201     C*
049900140728     C***                EVAL      PiStr=%trim(%subst(vindta:129:2))
050000140728     C***                EXSR      CHKNUM
050100140728     C***                IF        PiInt=*on
050200140728     C***                Z-ADD     PiVal         wBANCALI
050300140728     C***                ELSE
050400140728     C**N51              SETON                                        32
050500140728     C**N51              Z-ADD     *zeros        VABNCL
050600140728     C**N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050700140728     C***                          + ' ' + 'VABNCL'
050800140728     C***                ENDIF
050900110201     C*
051000110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
051100100729     C* PKB
051200110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
051300100729     C                   EXSR      CHKNUM
051400100729     C                   IF        PiNum=*on
051500110201     C                   Z-ADD     PiVal         VABPKB
051600100729     C                   ELSE
051700101119     C  N51              SETON                                        32
051800101119     C  N51              Z-ADD     *zeros        VABPKB
051900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052000100729     C                             + ' ' + 'VABPKB'
052100100729     C                   ENDIF
052200110201     C* VLB
052300140725     C                   IF        %trim(%subst(vindta:234:11)) <> '0,0'
052400140725     C                   EVAL      PiStr=%trim(%subst(vindta:234:11))
052500140725     C                   EXSR      CHKNUM
052600140725     C                   IF        PiNum=*on
052700140725     C                   Z-ADD(H)  PiVal         VABVLB
052800140725     C                   ELSE
052900140725     C  N51              SETON                                        32
053000140725     C  N51              Z-ADD     *zeros        VABVLB
053100140725     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053200140725     C                             + ' ' + 'VABVLB'
053300140725     C                   ENDIF
053400140725     C                   ENDIF
053500100729     C* CAS
053600140725     C                   IF        %subst(vindta:137:10)<> *blanks      AND
053700140725     C                             %subst(vindta:137:10)<> *zeros       AND
053800140725     C                             %subst(vindta:137:10)<> '0000000,00' AND
053900140725     C                             %trim(%subst(vindta:137:10))<> '0,00'
054000100729     C                   EVAL      FlgCAS = '1'
054100110127     C                   EVAL      VABVCA = 'EUR'
054200140725     C                   EVAL      VABTIC=%trim(%subst(vindta:147:40))
054300110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
054400100729     C                   EXSR      CHKNUM
054500100729     C                   IF        PiNum=*on
054600100928     C                   Z-ADD(H)  PiVal         VABCAS
054700100729     C                   ELSE
054800101119     C  N51              SETON                                        32
054900101119     C  N51              Z-ADD     *zeros        VABCAS
055000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055100100729     C                             + ' ' + 'VABCAS'
055200100729     C                   ENDIF
055300101119     C                   ELSE
055400101119     C                   EVAL      FlgCAS = '0'
055500100729     C                   ENDIF
055600110201     C* IAS
055700140725     C                   IF        %subst(vindta:223:11)<> *blanks       AND
055800140725     C                             %subst(vindta:223:11)<> *zeros        AND
055900140725     C                             %subst(vindta:223:11)<> '00000000,00' AND
056000140725     C                             %trim(%subst(vindta:223:11))<> '0,00'
056100110201     C                   EVAL      VABVAS = 'EUR'
056200110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
056300110201     C                   EXSR      CHKNUM
056400110201     C                   IF        PiNum=*on
056500110201     C                   Z-ADD(H)  PiVal         VABIAS
056600110201     C                   ELSE
056700110201     C  N51              SETON                                        32
056800110201     C  N51              Z-ADD     *zeros        VABIAS
056900110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057000110201     C                             + ' ' + 'VABIAS'
057100110201     C                   ENDIF
057200110201     C                   ENDIF
057300100729     C*
057400100729     C* Se provincia nn valorizzata la reperisco
057500100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
057600100729     C                   IF        VABNZD  = *blanks AND
057700100729     C                             VABPRD  = *blanks AND
057800100729     C                             VABCAD <> *blanks
057900100729     C                   CLEAR                   TISI95DS
058000100729     C                   EVAL      I95TCN = '3'
058100100729     C                   Z-ADD     datcor        I95DAT
058200100729     C                   EVAL      I95NAR = VABNZD
058300100729     C                   EVAL      I95CAP = VABCAD
058400100729     C                   EVAL      I95LOC = VABLOD
058500100729     C                   CALL      'TISI95R'
058600100729     C                   PARM                    TISI95DS
058700100729     C                   EVAL      VABPRD = O95PRV
058800100729     C                   ENDIF
058900100729     C*
059000100729     C* Considerazioni finali su CBO/CAS
059100100729     C                   IF        FlgCAS = '1'
059200100729     C                   IF        VABCBO = '1'
059300100729     C                   EVAL      VABCBO = '4'
059400100729     C                   ENDIF
059500100729     C                   IF        VABCBO = '2'
059600100729     C                   EVAL      VABCBO = '6'
059700100729     C                   ENDIF
059800100729     C                   ENDIF
059900010202     C*
060000000801     C* Ebbene...
060100000801     C                   ADD       1             §CTRMO
060200070530     C                   IF        *in31 <> *zeros OR
060300070530     C                             *in32 <> *zeros
060400000801     C                   ADD       1             §CTRNO
060500020725     C                   EVAL      x_vinflg = '2'
060600000801     C                   ELSE
060700010201     C                   ADD       1             §CTROKVB
060800000801     C                   ENDIF
060900000801     C*
061000000801     C                   ENDSR
061100100127     C*----------------------------------------------------*
061200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
061300100127     C*----------------------------------------------------*
061400100127     C     WRIVAT_A      BEGSR
061500100127     C*
061600100127     C* Valorizzo l buffer di scrittura del FIVAT
061700100127     C* - TIPO RECORD "A"
061800140725     C                   eval      VATTRC = 'A'
061900140725     C                   eval      VATNOT = %trim(%subst(vindta:1008:10))
062000140725     C                   exsr      wrivat
062100100127     C*
062200100127     C                   ENDSR
062300010201     C*----------------------------------------------------*
062400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
062500010201     C*----------------------------------------------------*
062600071003     C     WRIVAT_B      BEGSR
062700010201     C*
062800021113     C* Valorizzo l buffer di scrittura del FIVAT
062900070928     C* - TIPO RECORD "B"
063000101119     C***                eval      VATTRC = 'B'
063100101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
063200101119     C***                exsr      wrivat
063300010201     C*
063400010201     C                   ENDSR
063500140725     C*----------------------------------------------------*
063600140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
063700140725     C*----------------------------------------------------*
063800140725     C     WRIVAT_I      BEGSR
063900140725     C*
064000140725     C* Valorizzo l buffer di scrittura del FIVAT
064100140725     C* - TIPO RECORD "I" / "J"
064200140728     C***                movel     *blanks       wTRC             70
064300140728     C***                eval      wTRC   = %trim(%subst(vindta:938:70))
064400140728     C***                eval      VATTRC = 'I'
064500140728     C***                eval      VATNOT = %subst(wTRC:01:35)
064600140728     C***                exsr      wrivat
064700140728     C***                eval      VATTRC = 'J'
064800140728     C***                eval      VATNOT = %subst(wTRC:36:35)
064900140728     C***                exsr      wrivat
065000140725     C*
065100140725     C                   ENDSR
065200140725     C*----------------------------------------------------*
065300140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
065400140725     C*----------------------------------------------------*
065500140725     C     WRIVAT_S      BEGSR
065600140725     C*
065700140725     C* Valorizzo l buffer di scrittura del FIVAT
065800140725     C* - TIPO RECORD "S"
065900140725     C                   eval      VATTRC = 'S'
066000140725     C                   eval      VATNOT = %trim(%subst(vindta:1018:10))
066100140725     C                   exsr      wrivat
066200140725     C*
066300140725     C                   ENDSR
066400071003     C*----------------------------------------------------*
066500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
066600071003     C*----------------------------------------------------*
066700071003     C     WRIVAT_E      BEGSR
066800071003     C*
066900071003     C* Valorizzo l buffer di scrittura del FIVAT
067000071003     C* - TIPO RECORD "E"
067100100729     C*
067200130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
067300130911     C*
067400130911     C*
067500100928     C                   eval      VATTRC = 'E'
067600130911     C***                eval      VATNOT = %trim(%subst(vindta:459:8))
067700130911     C                   eval      IdColloIn = %dec((%subst(vindta:897:15))
067800130911     C                                          : 15 : 0)
067900130911     C*
068000130911     C     1             DO        VABNCL
068100130911     C                   eval      IdColloIn = IdColloIn + 1
068200130911     C                   EVAL      VATNOT = %trim(%editc(IdColloIn-1:'4'))
068300130911     C*
068400100928     C                   exsr      wrivat
068500130911     C                   ENDDO
068600071003     C*
068700071003     C                   ENDSR
068800100127     C*----------------------------------------------------*
068900100127     C*  SCARICO BUFFER FIVAT
069000100127     C*----------------------------------------------------*
069100100127     C     WRIVAT        BEGSR
069200100127     C*
069300100127     C                   if        vatNOT <> *blanks
069400100127     C                   ADD       1             §CTROKVT
069500100127     C                   write     FIVAT000
069600100127     C                   endif
069700100127     C*
069800100127     C                   ENDSR
069900020904
070000020904
070100010202     C*----------------------------------------------------*
070200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
070300010202     C*----------------------------------------------------*
070400100525     C     PREVASX       BEGSR
070500100525     C*
070600021113     C* Compongo il nome del membro da dare al FIVATWWR
070700010202     C                   eval      parmbr = vlrhdl
070800010202     C                   movel     'M'           parmbr
070900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
071000010202     C                   eval      paropz = '1'
071100100525     C*
071200010202     C* Effettuo la chiamata al CLLE preposto
071300100525     C  N51              call(e)   'TITVVTC'
071400010202     C                   parm                    parccm
071500010202     C                   parm                    parmbr
071600010202     C                   parm                    paropz
071700100525     C   51              call(e)   'TITVVBC'
071800100525     C                   parm                    parccm
071900100525     C                   parm                    parmbr
072000100525     C                   parm                    paropz
072100100525     C*
072200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072300010202     C                   if        %error
072400010202     C                   movel     '1'           chkcall
072500010202     C                   else
072600010202     C                   movel     '0'           chkcall
072700010202     C                   endif
072800010202     C*
072900010202     C                   ENDSR
073000000801     C*----------------------------------------------------*
073100000801     C*  CONTROLLO NUMERICITA' CAMPI
073200000801     C*----------------------------------------------------*
073300000801     C     CHKNUM        BEGSR
073400081113     C*
073500081113     C                   IF        PiDecChr = *blanks
073600110201     C                   EVAL      PiDecChr = ','
073700081113     C                   ENDIF
073800091223     C*
073900081113     C                   callp(e)  UBISNUM_Check(PiStr
074000081113     C                                          :PiDecChr
074100081113     C                                          :PiVal
074200081113     C                                          :PiNum
074300081113     C                                          :PiInt)
074400081113     C*
074500000801     C                   IF        %error
074600000801     C                   EVAL      PiInt=*off
074700000801     C                   ENDIF
074800000801     C*
074900000801     C                   ENDSR
075000000801     C***
075100011113
075200011113
075300000801
075400000801
075500990920      /TITLE Invio dei dati al punto operativo.
075600010202     C     invio         BEGSR
075700990920     C*
075800021113     C* 1° invio FIVAT
075900010201     C                   reset                   dscmz
076000010201     C                   move      vlrpoi        cmzdst
076100021113     C                   eval      cmzfld = 'FIVATWWR'
076200010201     C                   eval      cmzmbd = vlrhdl
076300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076400021009     C***                if        prmfir = *blanks
076500021113     C                   eval      cmzfla = 'FIVAT00F'
076600021113     C                   eval      cmzmba = 'FIVAT00F'
076700021009     C***                else
076800021009     C***                eval      cmzfla = prmfir
076900021009     C***                eval      cmzmba = prmfir
077000021009     C***                endif
077100010201     C                   eval      cmznrr = *zeros
077200020305     C                   move      §ctrokvt      cmznrr
077300021018     C                   eval      cmzlba = vlrfl1
077400010201     C                   call(e)   'TIS711C'
077500010201     C                   parm                    dscmz
077600010201     C                   parm      *blanks       esito
077700010205     C                   if        %error
077800010205     C                             or cmzerr = '1'
077900010205     C                             or esito  = '1'
078000010205     C                   eval      wrkesito = '3'
078100010205     C                   else
078200010201     C*
078300021113     C* 2° invio FIVAB
078400100525     C                   if        *in51 = *off
078500010201     C                   reset                   dscmz
078600010201     C                   move      vlrpoi        cmzdst
078700010201     C                   eval      cmzfld = vlrfou
078800010201     C                   eval      cmzmbd = vlrhdl
078900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079000021009     C***                if        prmfir = *blanks
079100021113     C                   eval      cmzfla = 'FIVAB00F'
079200021113     C                   eval      cmzmba = 'FIVAB00F'
079300021009     C***                else
079400021009     C***                eval      cmzfla = prmfir
079500021009     C***                eval      cmzmba = prmfir
079600021009     C***                endif
079700010201     C                   eval      cmznrr = *zeros
079800010201     C                   move      §ctrokvb      cmznrr
079900021018     C                   eval      cmzlba = vlrfl1
080000010201     C                   call(e)   'TIS711C'
080100010201     C                   parm                    dscmz
080200010201     C                   parm      *blanks       esito
080300010201     C                   if        %error
080400010201     C                             or cmzerr = '1'
080500010201     C                             or esito  = '1'
080600010201     C                   eval      wrkesito = '3'
080700010201     C                   endif
080800100525     C                   endif
080900010205     C                   endif
081000990920     C*
081100000613     C                   ENDSR
081200000613     C***
081300070411
081400070411     C     *pssr         BEGSR
081500070411     C*
081600070411     C                   if        %open(tivin00r)
081700070411     C                   close     tivin00r
081800070411     C                   endif
081900070411     C                   if        %open(fivabwwr)
082000070411     C                   close     fivabwwr
082100070411     C                   endif
082200070411     C                   if        %open(fivatwwr)
082300070411     C                   close     fivatwwr
082400070411     C                   endif
082500070411     C*
082600070411     C* Effettuo la chiamata al CLLE preposto
082700100525     C  N51              call(e)   'TITVVTC'
082800100525     C                   parm                    parccm
082900100525     C                   parm                    parmbr
083000100525     C                   parm      '2'           paropz
083100100525     C   51              call(e)   'TITVVBC'
083200100525     C                   parm                    parccm
083300100525     C                   parm                    parmbr
083400100525     C                   parm      '2'           paropz
083500070411     C*
083600070411     C                   eval      wrkesito = '2'
083700070411     C*
083800070411     C                   seton                                        LR
083900070411     C*
084000070411     C                   ENDSR     '*CANCL'
084100070411     C***
084200070411
084300990910
084400000613     C     *inzsr        BEGSR
084500990910     C*
084600990910     C     *entry        plist
084700990920     C                   parm                    tivlrds
084800990921     C                   parm      wrkesito      esito
084900000724     C                   parm                    prmlit
085000000710     C                   parm                    prmfir
085100000613     C*
085200000830     C* CALCOLA LA DATA CORRENTE
085300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085400091223     C                   eval      datcor = %dec(%date() : *ISO)
085500000830     C*
085600000613     C                   ENDSR
085700000613     C***
