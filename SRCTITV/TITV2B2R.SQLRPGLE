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
002100101117     D depspe          s             20    INZ(*blanks)
002200101117     D curspe          s             20    INZ(*blanks)
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
004900081113
005000081113     D*------------------
005100081113     D* LINKING A DEFINIZIONI ESTERNE
005200081113     D*------------------
005300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005400090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005500081113
005600990908
005700010201
005800010201
005900080117     C*
006000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006100080117     C
006200080117     C/EXEC SQL
006300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400080117     C/END-EXEC
006500080117     C
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000613     C*
007000100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
007100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007200000613     C*
007300010202     C* Effettuo la chiamata al CLLE preposto
007400100525     C  N51              call(e)   'TITVVTC'
007500100525     C                   parm                    parccm
007600100525     C                   parm                    parmbr
007700100525     C                   parm      '2'           paropz
007800100525     C   51              call(e)   'TITVVBC'
007900100525     C                   parm                    parccm
008000100525     C                   parm                    parmbr
008100100525     C                   parm      '2'           paropz
008200070730     C*
008300070730     C* Effettuo lancio TISI95 solo x chiusura
008400070730     C                   CLEAR                   TISI95DS
008500070730     C                   EVAL      I95TLA = 'C'
008600070730     C                   CALL      'TISI95R'
008700070730     C                   PARM                    TISI95DS
008800000616     C*
008900000801     C
009000010201     C                   seton                                        LR
009100990908
009200000801
009300910830     C*--------------------------------------------------------
009400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009500910830     C*--------------------------------------------------------
009600070530     C     RWFILE        BEGSR
009700990910     C*
009800990914     C                   if        not %open(tivin00r)
009900990908     C                   open      tivin00r
010000990914     C                   endif
010100070530     C*
010200100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010300100525     C                   exsr      prevasx
010400010201     C*
010500010202     C                   if        chkcall = '0'
010600010202     C*
010700100525     C                   if        not %open(fivabwwr)
010800100525     C                   open      fivabwwr
010900100525     C                   endif
011000100525     C*
011100021113     C                   if        not %open(fivatwwr)
011200021113     C                   open      fivatwwr
011300010201     C                   endif
011400080117     C*
011500080117     C                   EXSR      INZVAR
011600080117     C                   EXSR      DEFCAM
011700990910     C*
011800010201     C                   clear                   §CTROKVB          5 0
011900020305     C                   clear                   §CTROKVT          5 0
012000000801     C                   clear                   §CTRMO            5 0
012100000801     C                   clear                   §CTRNO            5 0
012200990910     C*
012300020725     C/EXEC SQL
012400020725     C+ declare C1 cursor for select
012500101117     C+ vindta, vinflg, substr(vindta, 25, 20) as sped, rrn(tivin00r)
012600060223     C+ from tivin00r
012700101117     C+ order by sped, substr(vindta, 25, 20) desc
012800020725     C+ for read only
012900020725     C/END-EXEC
013000020725     C
013100020725     C/EXEC SQL
013200020725     C+ open C1
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600070530     C+ Fetch C1 into :INPUT_DS
013700020725     C/END-EXEC
013800020725     C*
013900020725     C                   dow       sqlcod = *zeros
014000990913     C*
014100100310     C                   if        vindta > *blanks
014200000613     C                   add       1             rrnum
014300000801     C*
014400020725     C                   if        vinflg = *blanks
014500020725     C                             or vinflg = '0'
014600020725     C                             or vinflg = '2'
014700000801     C*
014800020725     C                   clear                   x_vinmsg
014900020725     C                   eval      x_vinflg = '1'
015000010305     C*
015100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015200101117     C                   EVAL      curspe=%trim(%subst(vindta:25:20))
015300010305     C*
015400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015500080923     C                   if        *in50 = *off
015600071003     C                   exsr      impvab
015700100525     C                   exsr      wrivab
015800100127     C                   exsr      wrivat_a                                     => carico VAT
015900071003     C                   exsr      wrivat_b                                     => carico VAT
016000100525     C   51              exsr      wrivat_e                                     => carico VAT
016100071003     C                   else
016200080923     C*
016300071009     C                   if        wDISK = 'D'
016400091223     C                   exsr      repNSP
016500071009     C                   exsr      impvab
016600100412     C                   exsr      wrivab
016700100127     C                   exsr      wrivat_a                                     => carico VAT
016800071009     C                   exsr      wrivat_b                                     => carico VAT
016900071009     C                   exsr      wrivat_e                                     => carico VAT
017000071009     C                   else
017100071009     C*
017200010305     C                   if        depspe = *blanks                             => 1° giro
017300010305     C                   eval      depspe = curspe                              => memorizz. spediz
017400080117     C                   clear                   tivinds
017500091223     C                   exsr      repNSP
017600020305     C                   exsr      impvab
017700100127     C                   exsr      wrivat_a                                     => carico VAT
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300100412     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500091223     C                   exsr      repNSP
018600020305     C                   exsr      impvab
018700100127     C                   exsr      wrivat_a                                     => carico VAT
018800071003     C                   exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000020305     C                   else                                                   => x stessa spediz
019100100401     C*
019200100401     C* Se nn richiesta esclusione spedizioni "duplicate"
019300100928     C                   select
019400100928     C                   when      wDUPREC = ' '
019500020305     C                   exsr      impvab
019600100127     C                   exsr      wrivat_a                                     => carico VAT
019700071003     C                   exsr      wrivat_b                                     => carico VAT
019800071003     C   50              exsr      wrivat_e                                     => carico VAT
019900100928     C                   when      wDUPREC = 'C'
020000100928     C   50              exsr      wrivat_e                                     => carico VAT
020100100928     C                   endsl
020200010305     C                   endif
020300010305     C                   endif
020400010305     C                   endif
020500071003     C                   endif
020600071009     C                   endif
020700000905     C*
020800000905     C                   else
020900020725     C                   eval      x_vinflg = '1'
021000000905     C                   endif
021100000905     C*
021200020725     C     VINRRN        chain     tivin000
021300020725     C                   if        %found(tivin00r)
021400020725     C                   eval      y_vinflg = x_vinflg
021500020725     C                   eval      y_vinmsg = x_vinmsg
021600020725     C                   update    tivin000
021700020725     C                   endif
021800020725     C*
021900020725     C/EXEC SQL
022000070530     C+ Fetch C1 into :INPUT_DS
022100020725     C/END-EXEC
022200020725     C*
022300020725     C                   enddo
022400020725     C*
022500020725     C/EXEC SQL
022600020725     C+ close C1
022700020725     C/END-EXEC
022800000905     C*
022900020305     C* Scarico i VAB rimasti "in sospeso"
023000071009     C                   if        wDISK = 'C'
023100100412     C                   exsr      wrivab
023200071009     C                   endif
023300010202     C*
023400010202     C                   endif
023500990910
023600990910     C* Se non ci sono record con errori ...
023700000710     C                   if        §ctrno = 0
023800990910     C* ... restituisco esito OK.
023900990921     C                   eval      wrkesito = '0'
024000990910     C                   else
024100100525     C                   if        §ctrokvb > 0 OR
024200100525     C                             §ctrokvt > 0
024300990921     C                   eval      wrkesito = '1'
024400000710     C                   else
024500000710     C                   eval      wrkesito = '2'
024600990910     C                   endif
024700000710     C                   endif
024800990910     C*
024900990914     C                   if        %open(tivin00r)
025000990908     C                   close     tivin00r
025100990914     C                   endif
025200021113     C                   if        %open(fivabwwr)
025300021113     C                   close     fivabwwr
025400990914     C                   endif
025500021113     C                   if        %open(fivatwwr)
025600021113     C                   close     fivatwwr
025700010201     C                   endif
025800990910     C*
025900100525     C                   if        §ctrokvb > 0 OR
026000100525     C                             §ctrokvt > 0
026100000724     C                             and vlrpoi <> *zeros
026200010202     C                   exsr      invio
026300990920     C                   endif
026400990920     C*
026500910830     C                   ENDSR
026600000613     C***
026700100412
026800100412
026900010305
027000010305     C*----------------------------------------------------*
027100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027200010305     C*----------------------------------------------------*
027300020305     C     WRIVAB        BEGSR
027400100412     C*
027500100412     C* Gestisco l'eventuale rottura x numero spedizione
027600100412     C                   if        VABRMA = *blanks
027700100412     C                   movel(P)  VABRMN        VABRMA
027800100412     C                   endif
027900100412     C*
028000100525     C                   if        *in51 = *off and *in31 = *off
028100100525     C                   write     fivab000                                     => scarico il VAB
028200100525     C                   endif
028300010305     C*
028400010305     C                   ENDSR
028500990920
028600000801     C*----------------------------------------------------*
028700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028800000801     C*----------------------------------------------------*
028900010201     C     INZVAR        BEGSR
029000000801     C*
029100010201     C                   Z-ADD     *zeros        Num5_0
029200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029300020725     C                   MOVEL     '0'           FlgCAS            1
029400000801     C*
029500000801     C                   ENDSR
029600000801     C*----------------------------------------------------*
029700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029800000801     C*----------------------------------------------------*
029900020904     C     DEFCAM        BEGSR
030000080923     C*
030100100928     C                   SETOFF                                       505160
030200000801     C*
030300021113     C                   CLEAR                   FIVAB000
030400021113     C                   CLEAR                   FIVAT000
030500070730     C* Imposto i valori di default...
030600101116     C                   EVAL      VABCCM = 2493817
030700101116     C                   EVAL      VATCCM = 2493817
030800101116     C                   EVAL      VABLNP = 249
030900101116     C                   EVAL      VATLNP = 249
031000100928     C                   EVAL      VABCTR = 000
031100100928     C                   EVAL      VABCTM = '7Q'
031200100525     C                   EVAL      VABCBO = '1'
031300070222     C* ... e poi verifico se sono stati passati come parametri
031400070222     C                   IF        vlrppt > *blanks
031500100525     C*
031600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031800070222     C                   EXSR      CHKNUM
031900070222     C                   IF        PiInt=*on
032000070222     C                   Z-ADD     PiVal         VABCCM
032100070222     C                   Z-ADD     PiVal         VATCCM
032200070222     C                   ENDIF
032300070222     C                   ENDIF
032400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032600070222     C                   EXSR      CHKNUM
032700070222     C                   IF        PiInt=*on
032800070222     C                   Z-ADD     PiVal         VABLNP
032900070222     C                   Z-ADD     PiVal         VATLNP
033000070222     C                   ENDIF
033100070222     C                   ENDIF
033200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
033300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033400070222     C                   EXSR      CHKNUM
033500070222     C                   IF        PiInt=*on
033600070222     C                   Z-ADD     PiVal         VABCTR
033700070222     C                   ENDIF
033800070928     C                   ENDIF
033900071009     C                   MOVEL     *blanks       wDISK             1
034000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
034100071009     C                   IF        wDISK <> *blanks
034200070928     C                   SETON                                        50
034300070222     C                   ENDIF
034400100525     C                   IF        wDISK  = 'T'
034500100525     C                   SETOFF                                       50
034600100525     C                   SETON                                        51
034700100525     C                   ENDIF
034800100401     C                   MOVEL     *blanks       wDUPREC           1
034900100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
035000100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
035100100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
035200100525     C                   ENDIF
035300100525     C*
035400070222     C                   ENDIF
035500071009     C*
035600071009     C   50              EVAL      VABCTM = '7Q'
035700000801     C*
035800000801     C                   ENDSR
035900091223     C*----------------------------------------------------*
036000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
036100091223     C*----------------------------------------------------*
036200091223     C     REPNSP        BEGSR
036300091223     C*
036400091223     C                   EXSR      INZVAR
036500091223     C                   EXSR      DEFCAM
036600100928     C*
036700100928     C                   SETON                                        60
036800091223     C*
036900091223     C* NSP => Stacco un numeratore da AZNUM
037000091223     C                   clear                   TRUL33DS
037100091223     C                   eval      I33OPE = *zeros
037200091223     C                   eval      I33CNU = 302
037300091223     C                   eval      I33NUM = 1
037400091223     C                   movel     TRUL33DS      KPJBU
037500091223     C                   call      'TRUL33R'
037600091223     C                   parm                    KPJBA
037700091223     C                   movel     KPJBU         TRUL33DS
037800091223     C                   if        O33ERR = *zeros
037900091223     C                   z-add     O33NRF        VABNSP
038000091223     C                   z-add     O33NRF        VATNSP
038100091223     C                   else
038200091223     C                   SETON                                        31
038300091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038400091223     C                             + ' ' + 'VABNSP VATNSP'
038500091223     C                   endif
038600091223     C*
038700091223     C                   ENDSR
038800000801     C*----------------------------------------------------*
038900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039000000801     C*----------------------------------------------------*
039100100730     C     IMPVAB        BEGSR
039200070530     C*
039300070530     C                   SETOFF                                       3132
039400070928     C*
039500070928     C                   MOVE(P)   vlrpoi        VABFGS
039600070928     C                   MOVE(P)   vlrpoi        VATFGS
039700070928     C*
039800070928     C                   MOVEL     datcor        VABAAS
039900070928     C                   MOVEL     datcor        VATAAS
040000070928     C                   MOVE      datcor        VABMGS
040100100729     C*
040200100729     C* Reperimento campi ALFA
040300101117     C                   EVAL      VABRSD=%trim(%subst(vindta:510:35))
040400101230     C***                EVAL      VABRD2=%trim(%subst(vindta:545:30))
040500101230     C                   EVAL      VABIND=%trim(%subst(vindta:545:30))
040600101229     C***                EVAL      VABIND=%trim(%subst(vindta:575:30))
040700101122     C                   EVAL      VABCAD=%trim(%subst(vindta:635:9))
040800101117     C                   EVAL      VABLOD=%trim(%subst(vindta:644:30))
040900101117     C                   EVAL      VABNZD=%trim(%subst(vindta:674:3))
041000101117     C                   IF        VABNZD='I'   or
041100101117     C                             VABNZD='IT'  or
041200101117     C                             VABNZD='ITA'
041300101117     C                   EVAL      VABNZD=*blanks
041400101117     C                   ELSE
041500101117     C                   EVAL      VABCTR=090
041600101117     C                   ENDIF
041700101117     C                   EVAL      VABRMA=%trim(%subst(vindta:1:24))
041800101117     C                   EVAL      VABNOT=%trim(%subst(vindta:833:35))
041900101117     C                   EVAL      VABNT2=%trim(%subst(vindta:833+35:35))
042000101117     C                   EVAL      VABRMO=%trim(%subst(vindta:54:35))
042100101028     C*
042200101117     C                   IF        %subst(vindta:784:1)='R'
042300101117     C                   EVAL      VABCBO = '2'
042400101117     C                   ELSE
042500101117     C                   EVAL      VABCBO = '1'
042600101117     C                   ENDIF
042700100729     C*
042800100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
042900100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043000100729     C     '@':'A'       XLATE     VABRSD        VABRSD
043100100729     C* ==
043200100729     C*
043300100729     C* Reperimento campi NUMERICI
043400100729     C                   MOVEL     DATCOR        VABAAS
043500100729     C                   MOVE      DATCOR        VABMGS
043600100730     C* RMN
043700101117     C                   EVAL      PiStr=%trim(%subst(vindta:25:20))
043800100730     C                   EXSR      CHKNUM
043900100730     C                   IF        PiInt=*on
044000100730     C                   Z-ADD     PiVal         VABRMN
044100100928     C  N60              Z-ADD     PiVal         VABNSP
044200100928     C  N60              Z-ADD     PiVal         VATNSP
044300100730     C                   ELSE
044400100730     C                   SETON                                        32
044500100730     C                   Z-ADD     1             VABRMN
044600100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700100928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
044800100730     C                   ENDIF
044900101117     C* CCM
045000101117     C                   EVAL      PiStr=%trim(%subst(vindta:45:9))
045100101117     C                   EXSR      CHKNUM
045200101117     C                   IF        PiInt=*on
045300101117     C                   Z-ADD     PiVal         VABCCM
045400101229     C                   Z-ADD     PiVal         VATCCM
045500101117     C                   ELSE
045600101117     C                   SETON                                        32
045700101117     C                   Z-ADD     *zeros        VABCCM
045800101229     C                   Z-ADD     *zeros        VATCCM
045900101117     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046000101229     C                             + ' ' + 'VABCCM VATCCM'
046100101117     C                   ENDIF
046200100729     C* NCL
046300101117     C                   EVAL      PiStr=%trim(%subst(vindta:787:4))
046400100928     C                   EXSR      CHKNUM
046500100928     C                   IF        PiInt=*on
046600100928     C                   Z-ADD     PiVal         VABNCL
046700100928     C                   ELSE
046800100928     C                   SETON                                        32
046900100928     C                   Z-ADD     *zeros        VABNCL
047000100928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100100928     C                             + ' ' + 'VABNCL'
047200100928     C                   ENDIF
047300100729     C* PKB
047400101117     C                   EVAL      PiStr=%trim(%subst(vindta:791:7))
047500100729     C                   EXSR      CHKNUM
047600100729     C                   IF        PiNum=*on
047700101117     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
047800100729     C                   Z-ADD(H)  PiVal         VABPKB
047900100729     C                   ELSE
048000100729     C                   SETON                                        32
048100100729     C                   Z-ADD     *zeros        VABPKB
048200100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048300100729     C                             + ' ' + 'VABPKB'
048400100729     C                   ENDIF
048500101117     C* VLB
048600101117     C                   EVAL      PiStr=%trim(%subst(vindta:798:6))
048700101117     C                   EXSR      CHKNUM
048800101117     C                   IF        PiNum=*on
048900101117     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
049000101117     C                   Z-ADD(H)  PiVal         VABVLB
049100101117     C                   ELSE
049200101117     C                   SETON                                        32
049300101117     C                   Z-ADD     *zeros        VABVLB
049400101117     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049500101117     C                             + ' ' + 'VABVLB'
049600101117     C                   ENDIF
049700100729     C* CAS
049800101117     C                   IF        %subst(vindta:940:13)<> *blanks AND
049900101117     C                             %subst(vindta:940:13)<> *zeros
050000100729     C                   EVAL      FlgCAS = '1'
050100100729     C                   EVAL      VABVCA = 'EUR'
050200101117     C                   EVAL      VABTIC = %trim(%subst(vindta:781:3))
050300101117     C                   EVAL      PiStr=%trim(%subst(vindta:940:13))
050400100729     C                   EXSR      CHKNUM
050500100729     C                   IF        PiNum=*on
050600100928     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
050700100928     C                   Z-ADD(H)  PiVal         VABCAS
050800100729     C                   ELSE
050900100729     C                   SETON                                        32
051000100729     C                   Z-ADD     *zeros        VABCAS
051100100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200100729     C                             + ' ' + 'VABCAS'
051300100729     C                   ENDIF
051400100729     C                   ENDIF
051500101117     C* IAS
051600101117     C                   IF        %subst(vindta:957:13)<> *blanks AND
051700101117     C                             %subst(vindta:957:13)<> *zeros
051800101117     C                   EVAL      VABVAS = 'EUR'
051900101117     C                   EVAL      PiStr=%trim(%subst(vindta:957:13))
052000101117     C                   EXSR      CHKNUM
052100101117     C                   IF        PiNum=*on
052200101117     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
052300101117     C                   Z-ADD(H)  PiVal         VABIAS
052400101117     C                   ELSE
052500101117     C                   SETON                                        32
052600101117     C                   Z-ADD     *zeros        VABIAS
052700101117     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052800101117     C                             + ' ' + 'VABIAS'
052900101117     C                   ENDIF
053000101117     C                   ENDIF
053100100729     C*
053200100729     C* Se provincia nn valorizzata la reperisco
053300100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
053400100729     C                   IF        VABNZD  = *blanks AND
053500100729     C                             VABPRD  = *blanks AND
053600100729     C                             VABCAD <> *blanks
053700100729     C                   CLEAR                   TISI95DS
053800100729     C                   EVAL      I95TCN = '3'
053900100729     C                   Z-ADD     datcor        I95DAT
054000100729     C                   EVAL      I95NAR = VABNZD
054100100729     C                   EVAL      I95CAP = VABCAD
054200100729     C                   EVAL      I95LOC = VABLOD
054300100729     C                   CALL      'TISI95R'
054400100729     C                   PARM                    TISI95DS
054500100729     C                   EVAL      VABPRD = O95PRV
054600100729     C                   ENDIF
054700100729     C*
054800100729     C* Considerazioni finali su CBO/CAS
054900100729     C                   IF        FlgCAS = '1'
055000100729     C                   IF        VABCBO = '1'
055100100729     C                   EVAL      VABCBO = '4'
055200100729     C                   ENDIF
055300100729     C                   IF        VABCBO = '2'
055400100729     C                   EVAL      VABCBO = '6'
055500100729     C                   ENDIF
055600100729     C                   ENDIF
055700010202     C*
055800000801     C* Ebbene...
055900000801     C                   ADD       1             §CTRMO
056000070530     C                   IF        *in31 <> *zeros OR
056100070530     C                             *in32 <> *zeros
056200000801     C                   ADD       1             §CTRNO
056300020725     C                   EVAL      x_vinflg = '2'
056400000801     C                   ELSE
056500010201     C                   ADD       1             §CTROKVB
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900100127     C*----------------------------------------------------*
057000100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057100100127     C*----------------------------------------------------*
057200100127     C     WRIVAT_A      BEGSR
057300100127     C*
057400100127     C* Valorizzo l buffer di scrittura del FIVAT
057500100127     C* - TIPO RECORD "A"
057600101117     C                   eval      VATTRC = 'A'
057700101117     C                   eval      VATNOT = %trim(%subst(vindta:697:22))
057800101117     C                   exsr      wrivat
057900100127     C*
058000100127     C                   ENDSR
058100010201     C*----------------------------------------------------*
058200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
058300010201     C*----------------------------------------------------*
058400071003     C     WRIVAT_B      BEGSR
058500010201     C*
058600021113     C* Valorizzo l buffer di scrittura del FIVAT
058700070928     C* - TIPO RECORD "B"
058800101117     C                   eval      VATTRC = 'B'
058900101117     C                   eval      VATNOT = %trim(%subst(vindta:726:12))
059000101117     C                   exsr      wrivat
059100010201     C*
059200010201     C                   ENDSR
059300071003     C*----------------------------------------------------*
059400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059500071003     C*----------------------------------------------------*
059600071003     C     WRIVAT_E      BEGSR
059700071003     C*
059800071003     C* Valorizzo l buffer di scrittura del FIVAT
059900071003     C* - TIPO RECORD "E"
060000100729     C*
060100100928     C                   eval      VATTRC = 'E'
060200101122     C*
060300101122     C                   Z-ADD     1             wCollo            4 0
060400101122     C                   MOVEL     *zeros        wColloAlfa1      12
060500101122     C*
060600101122     C                   EVAL      wColloAlfa1=%subst(vindta:738:12)
060700101122     C*
060800101122     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
060900101122     C                   dow       wCollo <= VABNCL and wCollo < 99
061000101122     C                   eval      VATNOT = %trim(wColloAlfa1) +
061100101122     C                                      %trim(%editc(wCollo:'X'))
061200101122     C                   exsr      wrivat                                       => scarico VAT
061300101122     C                   add       1             wCollo
061400101122     C                   enddo
061500101122     C*
061600071003     C*
061700071003     C                   ENDSR
061800100127     C*----------------------------------------------------*
061900100127     C*  SCARICO BUFFER FIVAT
062000100127     C*----------------------------------------------------*
062100100127     C     WRIVAT        BEGSR
062200100127     C*
062300100127     C                   if        vatNOT <> *blanks
062400100127     C                   ADD       1             §CTROKVT
062500100127     C                   write     FIVAT000
062600100127     C                   endif
062700100127     C*
062800100127     C                   ENDSR
062900020904
063000020904
063100010202     C*----------------------------------------------------*
063200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
063300010202     C*----------------------------------------------------*
063400100525     C     PREVASX       BEGSR
063500100525     C*
063600021113     C* Compongo il nome del membro da dare al FIVATWWR
063700010202     C                   eval      parmbr = vlrhdl
063800010202     C                   movel     'M'           parmbr
063900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
064000010202     C                   eval      paropz = '1'
064100100525     C*
064200010202     C* Effettuo la chiamata al CLLE preposto
064300100525     C  N51              call(e)   'TITVVTC'
064400010202     C                   parm                    parccm
064500010202     C                   parm                    parmbr
064600010202     C                   parm                    paropz
064700100525     C   51              call(e)   'TITVVBC'
064800100525     C                   parm                    parccm
064900100525     C                   parm                    parmbr
065000100525     C                   parm                    paropz
065100100525     C*
065200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065300010202     C                   if        %error
065400010202     C                   movel     '1'           chkcall
065500010202     C                   else
065600010202     C                   movel     '0'           chkcall
065700010202     C                   endif
065800010202     C*
065900010202     C                   ENDSR
066000000801     C*----------------------------------------------------*
066100000801     C*  CONTROLLO NUMERICITA' CAMPI
066200000801     C*----------------------------------------------------*
066300000801     C     CHKNUM        BEGSR
066400081113     C*
066500081113     C                   IF        PiDecChr = *blanks
066600100503     C                   EVAL      PiDecChr = '.'
066700081113     C                   ENDIF
066800091223     C*
066900081113     C                   callp(e)  UBISNUM_Check(PiStr
067000081113     C                                          :PiDecChr
067100081113     C                                          :PiVal
067200081113     C                                          :PiNum
067300081113     C                                          :PiInt)
067400081113     C*
067500000801     C                   IF        %error
067600000801     C                   EVAL      PiInt=*off
067700000801     C                   ENDIF
067800000801     C*
067900000801     C                   ENDSR
068000000801     C***
068100011113
068200011113
068300000801
068400000801
068500990920      /TITLE Invio dei dati al punto operativo.
068600010202     C     invio         BEGSR
068700990920     C*
068800021113     C* 1° invio FIVAT
068900010201     C                   reset                   dscmz
069000010201     C                   move      vlrpoi        cmzdst
069100021113     C                   eval      cmzfld = 'FIVATWWR'
069200010201     C                   eval      cmzmbd = vlrhdl
069300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069400021009     C***                if        prmfir = *blanks
069500021113     C                   eval      cmzfla = 'FIVAT00F'
069600021113     C                   eval      cmzmba = 'FIVAT00F'
069700021009     C***                else
069800021009     C***                eval      cmzfla = prmfir
069900021009     C***                eval      cmzmba = prmfir
070000021009     C***                endif
070100010201     C                   eval      cmznrr = *zeros
070200020305     C                   move      §ctrokvt      cmznrr
070300021018     C                   eval      cmzlba = vlrfl1
070400010201     C                   call(e)   'TIS711C'
070500010201     C                   parm                    dscmz
070600010201     C                   parm      *blanks       esito
070700010205     C                   if        %error
070800010205     C                             or cmzerr = '1'
070900010205     C                             or esito  = '1'
071000010205     C                   eval      wrkesito = '3'
071100010205     C                   else
071200010201     C*
071300021113     C* 2° invio FIVAB
071400100525     C                   if        *in51 = *off
071500010201     C                   reset                   dscmz
071600010201     C                   move      vlrpoi        cmzdst
071700010201     C                   eval      cmzfld = vlrfou
071800010201     C                   eval      cmzmbd = vlrhdl
071900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072000021009     C***                if        prmfir = *blanks
072100021113     C                   eval      cmzfla = 'FIVAB00F'
072200021113     C                   eval      cmzmba = 'FIVAB00F'
072300021009     C***                else
072400021009     C***                eval      cmzfla = prmfir
072500021009     C***                eval      cmzmba = prmfir
072600021009     C***                endif
072700010201     C                   eval      cmznrr = *zeros
072800010201     C                   move      §ctrokvb      cmznrr
072900021018     C                   eval      cmzlba = vlrfl1
073000010201     C                   call(e)   'TIS711C'
073100010201     C                   parm                    dscmz
073200010201     C                   parm      *blanks       esito
073300010201     C                   if        %error
073400010201     C                             or cmzerr = '1'
073500010201     C                             or esito  = '1'
073600010201     C                   eval      wrkesito = '3'
073700010201     C                   endif
073800100525     C                   endif
073900010205     C                   endif
074000990920     C*
074100000613     C                   ENDSR
074200000613     C***
074300070411
074400070411     C     *pssr         BEGSR
074500070411     C*
074600070411     C                   if        %open(tivin00r)
074700070411     C                   close     tivin00r
074800070411     C                   endif
074900070411     C                   if        %open(fivabwwr)
075000070411     C                   close     fivabwwr
075100070411     C                   endif
075200070411     C                   if        %open(fivatwwr)
075300070411     C                   close     fivatwwr
075400070411     C                   endif
075500070411     C*
075600070411     C* Effettuo la chiamata al CLLE preposto
075700100525     C  N51              call(e)   'TITVVTC'
075800100525     C                   parm                    parccm
075900100525     C                   parm                    parmbr
076000100525     C                   parm      '2'           paropz
076100100525     C   51              call(e)   'TITVVBC'
076200100525     C                   parm                    parccm
076300100525     C                   parm                    parmbr
076400100525     C                   parm      '2'           paropz
076500070411     C*
076600070411     C                   eval      wrkesito = '2'
076700070411     C*
076800070411     C                   seton                                        LR
076900070411     C*
077000070411     C                   ENDSR     '*CANCL'
077100070411     C***
077200070411
077300990910
077400000613     C     *inzsr        BEGSR
077500990910     C*
077600990910     C     *entry        plist
077700990920     C                   parm                    tivlrds
077800990921     C                   parm      wrkesito      esito
077900000724     C                   parm                    prmlit
078000000710     C                   parm                    prmfir
078100000613     C*
078200000830     C* CALCOLA LA DATA CORRENTE
078300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078400091223     C                   eval      datcor = %dec(%date() : *ISO)
078500000830     C*
078600000613     C                   ENDSR
078700000613     C***
