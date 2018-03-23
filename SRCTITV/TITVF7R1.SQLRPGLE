000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300121025
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
002000120319     D rrnum           s              8  0 INZ(*zeros)
002100120319     d wdcr            s              8  0
002200120319     D depspe          s              5    INZ(*blanks)
002300120319     D curspe          s              5    INZ(*blanks)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002900091223     D Num5_0          s              5  0
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500100729     D  VINDTA                     2048
003600100729     D  VINFLG                        1
003700120319     D  VINSPE                        5
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
004900120319
005000120319     D trul13ds      e ds                  inz
005100091223
005200101119
005300101119     D*-------------------
005400101119     D* COSTANTI
005500101119     D*-------------------
005600101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005700101119     D                                     èéù')
005800101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005900101119     D                                     EEU')
006000081113
006100081113     D*------------------
006200081113     D* LINKING A DEFINIZIONI ESTERNE
006300081113     D*------------------
006400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006500090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006600081113
006700990908
006800010201
006900010201
007000080117     C*
007100080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007200080117     C
007300080117     C/EXEC SQL
007400080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007500080117     C/END-EXEC
007600080117     C
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000000613     C*
008100100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008300000613     C*
008400010202     C* Effettuo la chiamata al CLLE preposto
008500100525     C  N51              call(e)   'TITVVTC'
008600100525     C                   parm                    parccm
008700100525     C                   parm                    parmbr
008800100525     C                   parm      '2'           paropz
008900100525     C   51              call(e)   'TITVVBC'
009000100525     C                   parm                    parccm
009100100525     C                   parm                    parmbr
009200100525     C                   parm      '2'           paropz
009300070730     C*
009400070730     C* Effettuo lancio TISI95 solo x chiusura
009500070730     C                   CLEAR                   TISI95DS
009600070730     C                   EVAL      I95TLA = 'C'
009700070730     C                   CALL      'TISI95R'
009800070730     C                   PARM                    TISI95DS
009900000616     C*
010000000801     C
010100010201     C                   seton                                        LR
010200990908
010300000801
010400910830     C*--------------------------------------------------------
010500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010600910830     C*--------------------------------------------------------
010700070530     C     RWFILE        BEGSR
010800990910     C*
010900990914     C                   if        not %open(tivin00r)
011000990908     C                   open      tivin00r
011100990914     C                   endif
011200070530     C*
011300100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011400100525     C                   exsr      prevasx
011500010201     C*
011600010202     C                   if        chkcall = '0'
011700010202     C*
011800100525     C                   if        not %open(fivabwwr)
011900100525     C                   open      fivabwwr
012000100525     C                   endif
012100100525     C*
012200021113     C                   if        not %open(fivatwwr)
012300021113     C                   open      fivatwwr
012400010201     C                   endif
012500080117     C*
012600080117     C                   EXSR      INZVAR
012700080117     C                   EXSR      DEFCAM
012800990910     C*
012900010201     C                   clear                   §CTROKVB          5 0
013000020305     C                   clear                   §CTROKVT          5 0
013100000801     C                   clear                   §CTRMO            5 0
013200000801     C                   clear                   §CTRNO            5 0
013300990910     C*
013400020725     C/EXEC SQL
013500020725     C+ declare C1 cursor for select
013600120319     C+ vindta, vinflg, substr(vindta, 113, 5) as sped, rrn(tivin00r)
013700060223     C+ from tivin00r
013800111020     C+ order by sped
013900020725     C+ for read only
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300020725     C+ open C1
014400020725     C/END-EXEC
014500020725     C
014600020725     C/EXEC SQL
014700070530     C+ Fetch C1 into :INPUT_DS
014800020725     C/END-EXEC
014900020725     C*
015000020725     C                   dow       sqlcod = *zeros
015100990913     C*
015200100310     C                   if        vindta > *blanks
015300000613     C                   add       1             rrnum
015400000801     C*
015500020725     C                   if        vinflg = *blanks
015600020725     C                             or vinflg = '0'
015700020725     C                             or vinflg = '2'
015800000801     C*
015900020725     C                   clear                   x_vinmsg
016000020725     C                   eval      x_vinflg = '1'
016100101119     C*
016200101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016300101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016400010305     C*
016500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016600120319     C                   EVAL      curspe=%trim(%subst(vindta:113:5))
016700010305     C*
016800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016900080923     C                   if        *in50 = *off
017000101119     C                   exsr      inzvar
017100101119     C                   exsr      defcam
017200071003     C                   exsr      impvab
017300100525     C                   exsr      wrivab
017400100127     C                   exsr      wrivat_a                                     => carico VAT
017500071003     C                   exsr      wrivat_b                                     => carico VAT
017600100525     C   51              exsr      wrivat_e                                     => carico VAT
017700121025     C                   exsr      wrivat_i                                     => carico VAT
017800121025     C                   exsr      wrivat_j                                     => carico VAT
017900071003     C                   else
018000080923     C*
018100071009     C                   if        wDISK = 'D'
018200091223     C                   exsr      repNSP
018300071009     C                   exsr      impvab
018400100412     C                   exsr      wrivab
018500100127     C                   exsr      wrivat_a                                     => carico VAT
018600071009     C                   exsr      wrivat_b                                     => carico VAT
018700071009     C                   exsr      wrivat_e                                     => carico VAT
018800121025     C                   exsr      wrivat_i                                     => carico VAT
018900121025     C                   exsr      wrivat_j                                     => carico VAT
019000071009     C                   else
019100071009     C*
019200010305     C                   if        depspe = *blanks                             => 1° giro
019300010305     C                   eval      depspe = curspe                              => memorizz. spediz
019400080117     C                   clear                   tivinds
019500091223     C                   exsr      repNSP
019600020305     C                   exsr      impvab
019700100127     C                   exsr      wrivat_a                                     => carico VAT
019800071003     C                   exsr      wrivat_b                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000121025     C                   exsr      wrivat_i                                     => carico VAT
020100121025     C                   exsr      wrivat_j                                     => carico VAT
020200010305     C                   else
020300020725     C                   if        depspe <> curspe                             => rottura di spediz
020400010305     C                   eval      depspe = curspe                              => memorizz. spediz
020500100412     C                   exsr      wrivab
020600080117     C                   clear                   tivinds
020700091223     C                   exsr      repNSP
020800020305     C                   exsr      impvab
020900100127     C                   exsr      wrivat_a                                     => carico VAT
021000071003     C                   exsr      wrivat_b                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200121025     C                   exsr      wrivat_i                                     => carico VAT
021300121025     C                   exsr      wrivat_j                                     => carico VAT
021400020305     C                   else                                                   => x stessa spediz
021500100401     C*
021600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021700100928     C                   select
021800100928     C                   when      wDUPREC = ' '
021900020305     C                   exsr      impvab
022000100127     C                   exsr      wrivat_a                                     => carico VAT
022100071003     C                   exsr      wrivat_b                                     => carico VAT
022200071003     C   50              exsr      wrivat_e                                     => carico VAT
022300121025     C                   exsr      wrivat_i                                     => carico VAT
022400121025     C                   exsr      wrivat_j                                     => carico VAT
022500100928     C                   when      wDUPREC = 'C'
022600100928     C   50              exsr      wrivat_e                                     => carico VAT
022700100928     C                   endsl
022800010305     C                   endif
022900010305     C                   endif
023000010305     C                   endif
023100071003     C                   endif
023200110201     C*
023300071009     C                   endif
023400000905     C*
023500000905     C                   else
023600020725     C                   eval      x_vinflg = '1'
023700000905     C                   endif
023800000905     C*
023900020725     C     VINRRN        chain     tivin000
024000020725     C                   if        %found(tivin00r)
024100020725     C                   eval      y_vinflg = x_vinflg
024200020725     C                   eval      y_vinmsg = x_vinmsg
024300020725     C                   update    tivin000
024400020725     C                   endif
024500020725     C*
024600020725     C/EXEC SQL
024700070530     C+ Fetch C1 into :INPUT_DS
024800020725     C/END-EXEC
024900020725     C*
025000020725     C                   enddo
025100020725     C*
025200020725     C/EXEC SQL
025300020725     C+ close C1
025400020725     C/END-EXEC
025500000905     C*
025600020305     C* Scarico i VAB rimasti "in sospeso"
025700071009     C                   if        wDISK = 'C'
025800100412     C                   exsr      wrivab
025900071009     C                   endif
026000010202     C*
026100010202     C                   endif
026200990910
026300990910     C* Se non ci sono record con errori ...
026400000710     C                   if        §ctrno = 0
026500990910     C* ... restituisco esito OK.
026600990921     C                   eval      wrkesito = '0'
026700990910     C                   else
026800100525     C                   if        §ctrokvb > 0 OR
026900100525     C                             §ctrokvt > 0
027000990921     C                   eval      wrkesito = '1'
027100000710     C                   else
027200000710     C                   eval      wrkesito = '2'
027300990910     C                   endif
027400000710     C                   endif
027500990910     C*
027600990914     C                   if        %open(tivin00r)
027700990908     C                   close     tivin00r
027800990914     C                   endif
027900021113     C                   if        %open(fivabwwr)
028000021113     C                   close     fivabwwr
028100990914     C                   endif
028200021113     C                   if        %open(fivatwwr)
028300021113     C                   close     fivatwwr
028400010201     C                   endif
028500990910     C*
028600100525     C                   if        §ctrokvb > 0 OR
028700100525     C                             §ctrokvt > 0
028800000724     C                             and vlrpoi <> *zeros
028900010202     C                   exsr      invio
029000990920     C                   endif
029100990920     C*
029200910830     C                   ENDSR
029300000613     C***
029400100412
029500100412
029600010305
029700010305     C*----------------------------------------------------*
029800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029900010305     C*----------------------------------------------------*
030000020305     C     WRIVAB        BEGSR
030100100412     C*
030200100412     C* Gestisco l'eventuale rottura x numero spedizione
030300100412     C                   if        VABRMA = *blanks
030400100412     C                   movel(P)  VABRMN        VABRMA
030500100412     C                   endif
030600100412     C*
030700100525     C                   if        *in51 = *off and *in31 = *off
030800100525     C                   write     fivab000                                     => scarico il VAB
030900100525     C                   endif
031000010305     C*
031100010305     C                   ENDSR
031200990920
031300000801     C*----------------------------------------------------*
031400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031500000801     C*----------------------------------------------------*
031600010201     C     INZVAR        BEGSR
031700000801     C*
031800010201     C                   Z-ADD     *zeros        Num5_0
031900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032000020725     C                   MOVEL     '0'           FlgCAS            1
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032500000801     C*----------------------------------------------------*
032600020904     C     DEFCAM        BEGSR
032700080923     C*
032800100928     C                   SETOFF                                       505160
032900000801     C*
033000021113     C                   CLEAR                   FIVAB000
033100021113     C                   CLEAR                   FIVAT000
033200070730     C* Imposto i valori di default...
033300120319     C                   EVAL      VABCCM = 0010051
033400120319     C                   EVAL      VATCCM = 0010051
033500120319     C                   EVAL      VABLNP = 001
033600120319     C                   EVAL      VATLNP = 001
033700120319     C                   EVAL      VABCTR = 000
033800120319     C                   EVAL      VABCTM = '7Q'
033900120319     C                   EVAL      VABCBO = '1'
034000070222     C* ... e poi verifico se sono stati passati come parametri
034100070222     C                   IF        vlrppt > *blanks
034200100525     C*
034300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034500070222     C                   EXSR      CHKNUM
034600070222     C                   IF        PiInt=*on
034700070222     C                   Z-ADD     PiVal         VABCCM
034800070222     C                   Z-ADD     PiVal         VATCCM
034900070222     C                   ENDIF
035000070222     C                   ENDIF
035100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035300070222     C                   EXSR      CHKNUM
035400070222     C                   IF        PiInt=*on
035500070222     C                   Z-ADD     PiVal         VABLNP
035600070222     C                   Z-ADD     PiVal         VATLNP
035700070222     C                   ENDIF
035800070222     C                   ENDIF
035900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036100070222     C                   EXSR      CHKNUM
036200070222     C                   IF        PiInt=*on
036300070222     C                   Z-ADD     PiVal         VABCTR
036400070222     C                   ENDIF
036500070928     C                   ENDIF
036600071009     C                   MOVEL     *blanks       wDISK             1
036700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036800071009     C                   IF        wDISK <> *blanks
036900070928     C                   SETON                                        50
037000070222     C                   ENDIF
037100100525     C                   IF        wDISK  = 'T'
037200100525     C                   SETOFF                                       50
037300100525     C                   SETON                                        51
037400100525     C                   ENDIF
037500100401     C                   MOVEL     *blanks       wDUPREC           1
037600100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037700100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037800100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037900100525     C                   ENDIF
038000100525     C*
038100070222     C                   ENDIF
038200071009     C*
038300071009     C   50              EVAL      VABCTM = '7Q'
038400000801     C*
038500000801     C                   ENDSR
038600091223     C*----------------------------------------------------*
038700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038800091223     C*----------------------------------------------------*
038900091223     C     REPNSP        BEGSR
039000091223     C*
039100091223     C                   EXSR      INZVAR
039200091223     C                   EXSR      DEFCAM
039300100928     C*
039400100928     C                   SETON                                        60
039500091223     C*
039600091223     C* NSP => Stacco un numeratore da AZNUM
039700091223     C                   clear                   TRUL33DS
039800091223     C                   eval      I33OPE = *zeros
039900091223     C                   eval      I33CNU = 302
040000091223     C                   eval      I33NUM = 1
040100091223     C                   movel     TRUL33DS      KPJBU
040200091223     C                   call      'TRUL33R'
040300091223     C                   parm                    KPJBA
040400091223     C                   movel     KPJBU         TRUL33DS
040500091223     C                   if        O33ERR = *zeros
040600091223     C                   z-add     O33NRF        VABNSP
040700091223     C                   z-add     O33NRF        VATNSP
040800091223     C                   else
040900091223     C                   SETON                                        31
041000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041100091223     C                             + ' ' + 'VABNSP VATNSP'
041200091223     C                   endif
041300091223     C*
041400091223     C                   ENDSR
041500120319     C*----------------------------------------------------*
041600120319     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041700120319     C*----------------------------------------------------*
041800120319     C     IMPVAB        BEGSR
041900120319     C*
042000120319     C                   SETOFF                                       3132
042100120319     C*
042200120319     C* Reperimento campi ALFA
042300120319     C*
042400120319     C* Considerazioni sulla ragione sociale del destinatario da indicare
042500120319     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
042600120319     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042700120319     C     '@':'A'       XLATE     VABRSD        VABRSD
042800120319     C* ==
042900120319     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
043000120319     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
043100120319     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
043200160107     C***                EVAL      VABRMA=%trim(%subst(vindta:114:4))
043300160107     C                   EVAL      VABRMA=%trim(%subst(vindta:371:13))
043400150424     C                   EVAL      VABCBO=%trim(%subst(vindta:207:1))
043500120319     C                   EVAL      VABNOT=%trim(%subst(vindta:221:35))
043600120319     C                   EVAL      VABNT2=%trim(%subst(vindta:256:35))
043700120319      * TIC
043800120319     C                   SELECT
043900120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '001' OR
044000120319     C                             %trim(%subst(vindta:208:3)) = '002'
044100120319     C                   CLEAR                   VABTIC
044200120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '010'
044300120319     C                   EVAL      VABTIC = 'BM'
044400120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '012'
044500120319     C                   EVAL      VABTIC = 'TM'
044600120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '013'
044700120319     C                   EVAL      VABTC1 = 'A'
044800120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '016'
044900120319     C                   EVAL      VABTC1 = 'P'
045000120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '018'
045100120319     C                   EVAL      VABTC1 = 'P'
045200120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '019'
045300120319     C                   EVAL      VABTIC = 'BM'
045400120319     C                   EVAL      VABDCR = %dec(%date() + %days(1) : *ISO)
045500120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '021'
045600120319     C                   EVAL      VABTIC = 'BM'
045700120319     C                   EVAL      VABTC1 = 'A'
045800120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '022'
045900120319     C                   EVAL      VABTIC = *blanks
046000120319     C                   EVAL      VABTC1 = 'A'
046100120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '023'
046200120319     C                   EVAL      VABTIC = 'TM'
046300120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '024'
046400120319     C                   EVAL      VABTSP = 'E'
046500120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '025'
046600120319     C                   EVAL      VABTSP = 'H'
046700120319     C                   WHEN      %trim(%subst(vindta:208:3)) = '014'
046800120319     C                   EVAL      VABTIC = 'BM'
046900120319     C                   EVAL      VABTC1 = 'A'
047000150317     C                   WHEN      %trim(%subst(vindta:208:3)) = '026'
047100150317     C                   EVAL      VABTIC = 'BM'
047200150317     C                   EVAL      VABTSP = 'E'
047300150317     C                   WHEN      %trim(%subst(vindta:208:3)) = '027'
047400150317     C                   EVAL      VABTIC = 'BM'
047500150317     C                   EVAL      VABTSP = 'H'
047600120319     C                   ENDSL
047700120319     C*
047800120319     C* Reperimento campi NUMERICI
047900120319     C                   MOVEL     DATCOR        VABAAS
048000120319     C                   MOVE      DATCOR        VABMGS
048100120319     C* NSP/RMN
048200120319     C                   EVAL      PiStr=%trim(%subst(vindta:113:5))
048300120319     C                   EXSR      CHKNUM
048400120319     C                   IF        PiInt=*on
048500120319     C                   Z-ADD     PiVal         VABNSP
048600120319     C                   Z-ADD     PiVal         VABRMN
048700120319     C                   ELSE
048800120319     C                   SETON                                        31
048900120319     C                   Z-ADD     *zeros        VABNSP
049000120319     C                   Z-ADD     *zeros        VABRMN
049100120319     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200120319     C                             + ' ' + 'VABNSP VABRMN'
049300120319     C                   ENDIF
049400120319     C* CAD
049500120319     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
049600120319     C                   EXSR      CHKNUM
049700120319     C                   IF        PiInt=*on
049800120319     C                   Z-ADD     PiVal         Num5_0
049900120319     C                   MOVEL(p)  Num5_0        VABCAD
050000120319     C                   ELSE
050100120319     C                   SETON                                        32
050200120319     C                   EVAL      VABCAD = *zeros
050300120319     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050400120319     C                             + ' ' + 'VABCAD'
050500120319     C                   ENDIF
050600120319     C* NCL
050700120319     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
050800120319     C                   EXSR      CHKNUM
050900120319     C                   IF        PiInt=*on
051000120319     C                   Z-ADD     PiVal         VABNCL
051100120319     C                   ELSE
051200120319     C                   SETON                                        32
051300120319     C                   Z-ADD     *zeros        VABNCL
051400120319     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051500120319     C                             + ' ' + 'VABNCL'
051600120319     C                   ENDIF
051700120319     C* PKB
051800120319     C                   EVAL      PiStr=%trim(%subst(vindta:129:8))
051900120319     C                   EXSR      CHKNUM
052000120319     C                   IF        PiNum=*on
052100120319     C                   Z-ADD     PiVal         VABPKB
052200120319     C                   ELSE
052300120319     C                   SETON                                        32
052400120319     C                   Z-ADD     *zeros        VABPKB
052500120319     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052600120319     C                             + ' ' + 'VABPKB'
052700120319     C                   ENDIF
052800120319     C* CAS
052900120319     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
053000120319     C                   EXSR      CHKNUM
053100120319     C                   IF        PiNum=*on
053200120319     C                   Z-ADD     PiVal         VABCAS
053300120319     C                   IF        vabcas > *Zeros
053400120319     C                   MOVEL     '1'           FlgCAS
053500120319     C                   EVAL      VABVCA = 'EUR'
053600120319     C                   ENDIF
053700120319     C                   ELSE
053800120319     C                   SETON                                        32
053900120319     C                   Z-ADD     *zeros        VABCAS
054000120319     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054100120319     C                             + ' ' + 'VABCAS'
054200120319     C                   ENDIF
054300120319     C* DCR
054400120319     C                   IF        %trim(%subst(vindta:208:3)) = '003'
054500120319     C                   EVAL      VABDCR = wdcr
054600120319     C                   ENDIF
054700120319     C* IAS
054800120319     C                   IF        %trim(%subst(vindta:208:3)) = '008'
054900120319     C                   EVAL      VABVAS = 'EUR'
055000120319     C                   EVAL      PiStr=%trim(%subst(vindta:211:10))
055100120319     C                   EXSR      CHKNUM
055200120319     C                   IF        PiNum=*on
055300120319     C                   Z-ADD     PiVal         VABIAS
055400120319     C                   ELSE
055500120319     C                   SETON                                        32
055600120319     C                   Z-ADD     *zeros        VABIAS
055700120319     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800120319     C                             + ' ' + 'VABIAS'
055900120319     C                   ENDIF
056000120319     C                   ENDIF
056100120319     C*
056200120319     C* Considerazioni finali su CBO/CAS
056300120319     C                   IF        FlgCAS = '1'
056400120319     C                   IF        VABCBO = '1'
056500120319     C                   EVAL      VABCBO = '4'
056600120319     C                   ENDIF
056700120319     C                   IF        VABCBO = '2'
056800120319     C                   EVAL      VABCBO = '6'
056900120319     C                   ENDIF
057000120319     C                   ENDIF
057100120319     C*
057200120319     C                   if        vlrpoi = 999
057300120319     C                   MOVE(P)   VABLNP        VABFGS
057400120319     C                   MOVE(P)   VABLNP        VATFGS
057500120319     C                   else
057600120319     C                   MOVE(P)   vlrpoi        VABFGS
057700120319     C                   MOVE(P)   vlrpoi        VATFGS
057800120319     C                   endif
057900120319     C*
058000120319     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058100120319     C                   EXSR      CHKIMPDIV
058200120319     C*
058300120319     C* Ebbene...
058400120319     C                   ADD       1             §CTRMO
058500120319     C                   IF        *in31 <> *zeros OR
058600120319     C                             *in32 <> *zeros
058700120319     C                   ADD       1             §CTRNO
058800120319     C                   EVAL      x_vinflg = '2'
058900120319     C                   ELSE
059000120319     C                   ADD       1             §CTROKVB
059100120319     C                   ENDIF
059200120319     C*
059300120319     C                   ENDSR
059400120319     C*----------------------------------------------------*
059500120319     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059600120319     C*----------------------------------------------------*
059700120319     C     CHKIMPDIV     BEGSR
059800120319     C*
059900120319     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060000120319     C                   Z-ADD     *zeros        wrkDec            9 9
060100120319     C*
060200120319     C* Come prima cosa effettuo considerazioni sulla divisa
060300120319     C                   IF        vabIAS > *zeros
060400120319     C                   IF        vabVAS <> 'EUR'
060500120319     C                   EVAL      vabVAS =  'ITL'
060600120319     C                   ENDIF
060700120319     C                   ENDIF
060800120319     C*
060900120319     C                   IF        vabCAS > *zeros
061000120319     C                   IF        vabVCA <> 'EUR'
061100120319     C                   EVAL      vabVCA =  'ITL'
061200120319     C                   ENDIF
061300120319     C                   ENDIF
061400120319     C*
061500120319     C                   IF        vabVMD > *zeros
061600120319     C                   IF        vabVAD <> 'EUR'
061700120319     C                   EVAL      vabVAD =  'ITL'
061800120319     C                   ENDIF
061900120319     C                   ENDIF
062000120319     C*
062100120319     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062200120319     C                   Z-ADD     vabIAS        wrkDec
062300120319     C                   IF        wrkDec > *zeros
062400120319     C                   IF        vabVAS = 'ITL'
062500120319     C                   EVAL      vabIAS = *zeros
062600120319     C                   ENDIF
062700120319     C                   ENDIF
062800120319     C*
062900120319     C* Stabilisco se il contrasegno ha decimali valorizzati
063000120319     C                   Z-ADD     vabCAS        wrkDec
063100120319     C                   IF        wrkDec > *zeros
063200120319     C                   IF        vabVCA = 'ITL'
063300120319     C                   EVAL      vabCAS = *zeros
063400120319     C                   ENDIF
063500120319     C                   ENDIF
063600120319     C*
063700120319     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063800120319     C                   Z-ADD     vabVMD        wrkDec
063900120319     C                   IF        wrkDec > *zeros
064000120319     C                   IF        vabVAD = 'ITL'
064100120319     C                   EVAL      vabVMD = *zeros
064200120319     C                   ENDIF
064300120319     C                   ENDIF
064400120319     C*
064500120319     C                   ENDSR
064600120319     C***
064700100127     C*----------------------------------------------------*
064800100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
064900100127     C*----------------------------------------------------*
065000100127     C     WRIVAT_A      BEGSR
065100100127     C*
065200100127     C* Valorizzo l buffer di scrittura del FIVAT
065300100127     C* - TIPO RECORD "A"
065400101119     C***                eval      VATTRC = 'A'
065500101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
065600101119     C***                exsr      wrivat
065700100127     C*
065800100127     C                   ENDSR
065900010201     C*----------------------------------------------------*
066000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
066100010201     C*----------------------------------------------------*
066200071003     C     WRIVAT_B      BEGSR
066300010201     C*
066400021113     C* Valorizzo l buffer di scrittura del FIVAT
066500070928     C* - TIPO RECORD "B"
066600110627     C***                eval      VATTRC = 'B'
066700110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
066800110627     C***                exsr      wrivat
066900010201     C*
067000010201     C                   ENDSR
067100071003     C*----------------------------------------------------*
067200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
067300071003     C*----------------------------------------------------*
067400071003     C     WRIVAT_E      BEGSR
067500071003     C*
067600071003     C* Valorizzo l buffer di scrittura del FIVAT
067700071003     C* - TIPO RECORD "E"
067800100729     C*
067900100928     C                   eval      VATTRC = 'E'
068000120319     C                   eval      VATNOT = %trim(%subst(vindta:321:12))
068100120319     C                   exsr      wrivat
068200071003     C*
068300071003     C                   ENDSR
068400121025     C*----------------------------------------------------*
068500121025     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
068600121025     C*----------------------------------------------------*
068700121025     C     WRIVAT_I      BEGSR
068800121025     C*
068900121025     C* Valorizzo l buffer di scrittura del FIVAT
069000121025     C* - TIPO RECORD "I"
069100121025     C                   eval      VATTRC = 'I'
069200160107     C                   eval      VATNOT = %trim(%subst(vindta:341:30))
069300121025     C                   exsr      wrivat
069400121025     C*
069500121025     C                   ENDSR
069600121025     C*----------------------------------------------------*
069700121025     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "J"
069800121025     C*----------------------------------------------------*
069900121025     C     WRIVAT_J      BEGSR
070000121025     C*
070100121025     C* Valorizzo l buffer di scrittura del FIVAT
070200121025     C* - TIPO RECORD "J" (a oggi non serve perché l'indirizzo e-mail è al max 35 char)
070300121025     C                   eval      VATTRC = 'I'
070400121025     C                   eval      VATNOT = *blank
070500121025     C                   exsr      wrivat
070600121025     C*
070700121025     C                   ENDSR
070800100127     C*----------------------------------------------------*
070900100127     C*  SCARICO BUFFER FIVAT
071000100127     C*----------------------------------------------------*
071100100127     C     WRIVAT        BEGSR
071200100127     C*
071300120319     C                   eval      VATCCM = VABCCM
071400120319     C                   eval      VATAAS = VABAAS
071500120319     C                   eval      VATLNP = VABLNP
071600120319     C                   eval      VATNRS = VABNRS
071700120319     C                   eval      VATNSP = VABNSP
071800120319     C*
071900100127     C                   if        vatNOT <> *blanks
072000100127     C                   ADD       1             §CTROKVT
072100100127     C                   write     FIVAT000
072200100127     C                   endif
072300100127     C*
072400100127     C                   ENDSR
072500020904
072600020904
072700010202     C*----------------------------------------------------*
072800100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
072900010202     C*----------------------------------------------------*
073000100525     C     PREVASX       BEGSR
073100100525     C*
073200021113     C* Compongo il nome del membro da dare al FIVATWWR
073300010202     C                   eval      parmbr = vlrhdl
073400010202     C                   movel     'M'           parmbr
073500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
073600010202     C                   eval      paropz = '1'
073700100525     C*
073800010202     C* Effettuo la chiamata al CLLE preposto
073900100525     C  N51              call(e)   'TITVVTC'
074000010202     C                   parm                    parccm
074100010202     C                   parm                    parmbr
074200010202     C                   parm                    paropz
074300100525     C   51              call(e)   'TITVVBC'
074400100525     C                   parm                    parccm
074500100525     C                   parm                    parmbr
074600100525     C                   parm                    paropz
074700100525     C*
074800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
074900010202     C                   if        %error
075000010202     C                   movel     '1'           chkcall
075100010202     C                   else
075200010202     C                   movel     '0'           chkcall
075300010202     C                   endif
075400010202     C*
075500010202     C                   ENDSR
075600000801     C*----------------------------------------------------*
075700000801     C*  CONTROLLO NUMERICITA' CAMPI
075800000801     C*----------------------------------------------------*
075900000801     C     CHKNUM        BEGSR
076000081113     C*
076100081113     C                   IF        PiDecChr = *blanks
076200120321     C                   EVAL      PiDecChr = ','
076300081113     C                   ENDIF
076400091223     C*
076500081113     C                   callp(e)  UBISNUM_Check(PiStr
076600081113     C                                          :PiDecChr
076700081113     C                                          :PiVal
076800081113     C                                          :PiNum
076900081113     C                                          :PiInt)
077000081113     C*
077100000801     C                   IF        %error
077200000801     C                   EVAL      PiInt=*off
077300000801     C                   ENDIF
077400000801     C*
077500000801     C                   ENDSR
077600000801     C***
077700011113
077800011113
077900000801
078000000801
078100990920      /TITLE Invio dei dati al punto operativo.
078200010202     C     invio         BEGSR
078300990920     C*
078400021113     C* 1° invio FIVAT
078500010201     C                   reset                   dscmz
078600110411     C                   move      vatfgs        cmzdst
078700021113     C                   eval      cmzfld = 'FIVATWWR'
078800010201     C                   eval      cmzmbd = vlrhdl
078900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079000021009     C***                if        prmfir = *blanks
079100021113     C                   eval      cmzfla = 'FIVAT00F'
079200021113     C                   eval      cmzmba = 'FIVAT00F'
079300021009     C***                else
079400021009     C***                eval      cmzfla = prmfir
079500021009     C***                eval      cmzmba = prmfir
079600021009     C***                endif
079700010201     C                   eval      cmznrr = *zeros
079800020305     C                   move      §ctrokvt      cmznrr
079900021018     C                   eval      cmzlba = vlrfl1
080000010201     C                   call(e)   'TIS711C'
080100010201     C                   parm                    dscmz
080200010201     C                   parm      *blanks       esito
080300010205     C                   if        %error
080400010205     C                             or cmzerr = '1'
080500010205     C                             or esito  = '1'
080600010205     C                   eval      wrkesito = '3'
080700010205     C                   else
080800010201     C*
080900021113     C* 2° invio FIVAB
081000100525     C                   if        *in51 = *off
081100010201     C                   reset                   dscmz
081200110411     C                   move      vabfgs        cmzdst
081300010201     C                   eval      cmzfld = vlrfou
081400010201     C                   eval      cmzmbd = vlrhdl
081500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081600021009     C***                if        prmfir = *blanks
081700021113     C                   eval      cmzfla = 'FIVAB00F'
081800021113     C                   eval      cmzmba = 'FIVAB00F'
081900021009     C***                else
082000021009     C***                eval      cmzfla = prmfir
082100021009     C***                eval      cmzmba = prmfir
082200021009     C***                endif
082300010201     C                   eval      cmznrr = *zeros
082400010201     C                   move      §ctrokvb      cmznrr
082500021018     C                   eval      cmzlba = vlrfl1
082600010201     C                   call(e)   'TIS711C'
082700010201     C                   parm                    dscmz
082800010201     C                   parm      *blanks       esito
082900010201     C                   if        %error
083000010201     C                             or cmzerr = '1'
083100010201     C                             or esito  = '1'
083200010201     C                   eval      wrkesito = '3'
083300010201     C                   endif
083400100525     C                   endif
083500010205     C                   endif
083600990920     C*
083700000613     C                   ENDSR
083800000613     C***
083900070411
084000110627     C     *pssr         BEGSR
084100070411     C*
084200110627     C                   if        %open(tivin00r)
084300110627     C                   close     tivin00r
084400110627     C                   endif
084500110627     C                   if        %open(fivabwwr)
084600110627     C                   close     fivabwwr
084700110627     C                   endif
084800110627     C                   if        %open(fivatwwr)
084900110627     C                   close     fivatwwr
085000110627     C                   endif
085100110627     C*
085200110627     C* Effettuo la chiamata al CLLE preposto
085300110627     C  N51              call(e)   'TITVVTC'
085400110627     C                   parm                    parccm
085500110627     C                   parm                    parmbr
085600110627     C                   parm      '2'           paropz
085700110627     C   51              call(e)   'TITVVBC'
085800110627     C                   parm                    parccm
085900110627     C                   parm                    parmbr
086000110627     C                   parm      '2'           paropz
086100110627     C*
086200110627     C                   eval      wrkesito = '2'
086300110627     C*
086400110627     C                   seton                                        LR
086500110627     C*
086600110627     C                   ENDSR     '*CANCL'
086700070411     C***
086800070411
086900990910
087000000613     C     *inzsr        BEGSR
087100990910     C*
087200990910     C     *entry        plist
087300990920     C                   parm                    tivlrds
087400990921     C                   parm      wrkesito      esito
087500000724     C                   parm                    prmlit
087600000710     C                   parm                    prmfir
087700000613     C*
087800000830     C* CALCOLA LA DATA CORRENTE
087900091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088000091223     C                   eval      datcor = %dec(%date() : *ISO)
088100120319
088200120319      * calcolo già oggi + 1 gg lavorativo per la data consegna richiesta
088300120319     C                   time                    wn14             14 0
088400120319     C                   clear                   TRUL13DS
088500120319     C                   eval      I13MOD = 'P'
088600120319     C                   eval      I13FIL = 001
088700120319     C                   eval      I13DIN = datcor
088800120319     C                   Movel     wn14          I13HIN
088900120319     C                   eval      I13GIO = 1
089000120319     C                   eval      I13ORE = *zeros
089100120319     C                   call      'TRUL13R'
089200120319     C                   parm                    TRUL13DS
089300120319     C                   if        O13ERR = *blanks
089400120319     C                   eval      wdcr  = O13DFI
089500120319     C                   else
089600120319     C                   eval      wdcr  = datcor
089700120319     C                   endif
089800000830     C*
089900000613     C                   ENDSR
090000000613     C***
