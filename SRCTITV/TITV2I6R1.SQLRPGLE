000100130315      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130315     C* questa gestione è un falso disk A (perché non scrive VAT_E) ma in realtà è un disk C
000300130315     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000400990908
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600130315     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000700130315     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070730     D tisi95ds      e ds
001700130315     D fivabds       e ds                  extname(EDIVAB0f)
001800130315     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
001900130315     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500111020     D depspe          s              8    INZ(*blanks)
002600111020     D curspe          s              8    INZ(*blanks)
002700010202     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003200091223     D Num5_0          s              5  0
003300130315     D wNomeFile       s             30    INZ(*blanks)
003400130315     D wCMR            s             35    INZ(*blanks)
003500130315     D wPosDaA         s              2    INZ(*blanks)
003600130315     D wPosDa          s              2  0 INZ(*zeros)
003700130315     D wLungA          s              2    INZ(*blanks)
003800130315     D wLung           s              2  0 INZ(*zeros)
003900130315     D wDataOraAlfa    s             14    INZ(*blanks)
004000020725
004100020725     D*------------------
004200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004300020725     D*------------------
004400070530     D INPUT_DS        DS                  INZ
004500100729     D  VINDTA                     2048
004600100729     D  VINFLG                        1
004700111020     D  VINSPE                        8
004800100729     D  VINRRN                        8  0
004900020725     D*
005000081113
005100091223     D*------------------
005200091223     D* DS REPERIMENTO NUMERATORE
005300091223     D*------------------
005400100309     D trul33ds      e ds                  inz
005500091223     D*------------------
005600091223     D* DS ARCHITETTURA
005700091223     D*------------------
005800091223     D kpjba         e ds                  inz
005900091223
006000101119
006100101119     D*-------------------
006200101119     D* COSTANTI
006300101119     D*-------------------
006400101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006500101119     D                                     èéù')
006600101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006700101119     D                                     EEU')
006800081113
006900081113     D*------------------
007000081113     D* LINKING A DEFINIZIONI ESTERNE
007100081113     D*------------------
007200081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007300090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007400081113
007500990908
007600010201
007700010201
007800080117     C*
007900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008000080117     C
008100080117     C/EXEC SQL
008200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008300080117     C/END-EXEC
008400080117     C
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800000613     C*
008900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
009000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009100000613     C*
009200010202     C* Effettuo la chiamata al CLLE preposto
009300130315     C  N51              call(e)   'TITVEVTC'
009400100525     C                   parm                    parccm
009500100525     C                   parm                    parmbr
009600100525     C                   parm      '2'           paropz
009700130315     C   51              call(e)   'TITVEVBC'
009800100525     C                   parm                    parccm
009900100525     C                   parm                    parmbr
010000100525     C                   parm      '2'           paropz
010100070730     C*
010200070730     C* Effettuo lancio TISI95 solo x chiusura
010300070730     C                   CLEAR                   TISI95DS
010400070730     C                   EVAL      I95TLA = 'C'
010500070730     C                   CALL      'TISI95R'
010600070730     C                   PARM                    TISI95DS
010700000616     C*
010800000801     C
010900010201     C                   seton                                        LR
011000990908
011100000801
011200910830     C*--------------------------------------------------------
011300130315     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011400910830     C*--------------------------------------------------------
011500070530     C     RWFILE        BEGSR
011600990910     C*
011700990914     C                   if        not %open(tivin00r)
011800990908     C                   open      tivin00r
011900990914     C                   endif
012000130315     C                   if        not %open(edivabwr)
012100130315     C                   open      edivabwr
012200130315     C                   endif
012300070530     C*
012400130315     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012500130315     C                   exsr      prevat
012600010201     C*
012700010202     C                   if        chkcall = '0'
012800010202     C*
012900130315     C                   if        not %open(edivatwr)
013000130315     C                   open      edivatwr
013100100525     C                   endif
013200080117     C*
013300080117     C                   EXSR      INZVAR
013400080117     C                   EXSR      DEFCAM
013500990910     C*
013600010201     C                   clear                   §CTROKVB          5 0
013700020305     C                   clear                   §CTROKVT          5 0
013800000801     C                   clear                   §CTRMO            5 0
013900000801     C                   clear                   §CTRNO            5 0
014000990910     C*
014100020725     C/EXEC SQL
014200020725     C+ declare C1 cursor for select
014300111020     C+ vindta, vinflg, substr(vindta, 124, 8) as sped, rrn(tivin00r)
014400060223     C+ from tivin00r
014500111020     C+ order by sped
014600020725     C+ for read only
014700020725     C/END-EXEC
014800020725     C
014900020725     C/EXEC SQL
015000020725     C+ open C1
015100020725     C/END-EXEC
015200020725     C
015300020725     C/EXEC SQL
015400070530     C+ Fetch C1 into :INPUT_DS
015500020725     C/END-EXEC
015600020725     C*
015700020725     C                   dow       sqlcod = *zeros
015800990913     C*
015900100310     C                   if        vindta > *blanks
016000000613     C                   add       1             rrnum
016100000801     C*
016200020725     C                   if        vinflg = *blanks
016300020725     C                             or vinflg = '0'
016400020725     C                             or vinflg = '2'
016500000801     C*
016600020725     C                   clear                   x_vinmsg
016700020725     C                   eval      x_vinflg = '1'
016800101119     C*
016900101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
017000101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
017100010305     C*
017200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017300111020     C                   EVAL      curspe=%trim(%subst(vindta:124:8))
017400010305     C*
017500100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017600080923     C                   if        *in50 = *off
017700101119     C                   exsr      inzvar
017800101119     C                   exsr      defcam
017900071003     C                   exsr      impvab
018000130315     C  N31              eval      fivabds_ok = fivabds
018100130315     C  N31              exsr      wrivab
018200100127     C                   exsr      wrivat_a                                     => carico VAT
018300071003     C                   exsr      wrivat_b                                     => carico VAT
018400100525     C   51              exsr      wrivat_e                                     => carico VAT
018500071003     C                   else
018600080923     C*
018700071009     C                   if        wDISK = 'D'
018800091223     C                   exsr      repNSP
018900071009     C                   exsr      impvab
019000130315     C  N31              eval      fivabds_ok = fivabds
019100130315     C  N31              exsr      wrivab
019200100127     C                   exsr      wrivat_a                                     => carico VAT
019300071009     C                   exsr      wrivat_b                                     => carico VAT
019400071009     C                   exsr      wrivat_e                                     => carico VAT
019500071009     C                   else
019600071009     C*
019700010305     C                   if        depspe = *blanks                             => 1° giro
019800010305     C                   eval      depspe = curspe                              => memorizz. spediz
019900080117     C                   clear                   tivinds
020000091223     C                   exsr      repNSP
020100020305     C                   exsr      impvab
020200130315     C                   z-add     VABNCL        sav_VABNCL
020300130315     C                   z-add     VABPKB        sav_VABPKB
020400130315     C                   z-add     VABVLB        sav_VABVLB
020500100127     C                   exsr      wrivat_a                                     => carico VAT
020600071003     C                   exsr      wrivat_b                                     => carico VAT
020700071003     C   50              exsr      wrivat_e                                     => carico VAT
020800010305     C                   else
020900020725     C                   if        depspe <> curspe                             => rottura di spediz
021000010305     C                   eval      depspe = curspe                              => memorizz. spediz
021100130315     C                   z-add     sav_VABNCL    VABNCL
021200130315     C                   z-add     sav_VABPKB    VABPKB
021300130315     C                   z-add     sav_VABVLB    VABVLB
021400130315     C  N31              eval      fivabds_ok = fivabds
021500130315     C  N31              exsr      wrivab
021600080117     C                   clear                   tivinds
021700091223     C                   exsr      repNSP
021800020305     C                   exsr      impvab
021900130315     C                   if        wCntRecSpe = *zeros
022000130315     C                   z-add     VABNCL        sav_VABNCL
022100130315     C                   z-add     VABPKB        sav_VABPKB
022200130315     C                   z-add     VABVLB        sav_VABVLB
022300130315     C                   endif
022400130315     C                   add       1             wCntRecSpe
022500100127     C                   exsr      wrivat_a                                     => carico VAT
022600071003     C                   exsr      wrivat_b                                     => carico VAT
022700071003     C   50              exsr      wrivat_e                                     => carico VAT
022800020305     C                   else                                                   => x stessa spediz
022900100401     C*
023000100401     C* Se nn richiesta esclusione spedizioni "duplicate"
023100100928     C                   select
023200100928     C                   when      wDUPREC = ' '
023300020305     C                   exsr      impvab
023400100127     C                   exsr      wrivat_a                                     => carico VAT
023500071003     C                   exsr      wrivat_b                                     => carico VAT
023600071003     C   50              exsr      wrivat_e                                     => carico VAT
023700100928     C                   when      wDUPREC = 'C'
023800100928     C   50              exsr      wrivat_e                                     => carico VAT
023900100928     C                   endsl
024000010305     C                   endif
024100010305     C                   endif
024200010305     C                   endif
024300071003     C                   endif
024400110201     C*
024500071009     C                   endif
024600000905     C*
024700000905     C                   else
024800020725     C                   eval      x_vinflg = '1'
024900000905     C                   endif
025000000905     C*
025100020725     C     VINRRN        chain     tivin000
025200020725     C                   if        %found(tivin00r)
025300020725     C                   eval      y_vinflg = x_vinflg
025400020725     C                   eval      y_vinmsg = x_vinmsg
025500020725     C                   update    tivin000
025600020725     C                   endif
025700020725     C*
025800020725     C/EXEC SQL
025900070530     C+ Fetch C1 into :INPUT_DS
026000020725     C/END-EXEC
026100020725     C*
026200020725     C                   enddo
026300020725     C*
026400020725     C/EXEC SQL
026500020725     C+ close C1
026600020725     C/END-EXEC
026700000905     C*
026800020305     C* Scarico i VAB rimasti "in sospeso"
026900071009     C                   if        wDISK = 'C'
027000100412     C                   exsr      wrivab
027100071009     C                   endif
027200010202     C*
027300010202     C                   endif
027400990910
027500990910     C* Se non ci sono record con errori ...
027600000710     C                   if        §ctrno = 0
027700990910     C* ... restituisco esito OK.
027800990921     C                   eval      wrkesito = '0'
027900990910     C                   else
028000100525     C                   if        §ctrokvb > 0 OR
028100100525     C                             §ctrokvt > 0
028200990921     C                   eval      wrkesito = '1'
028300000710     C                   else
028400000710     C                   eval      wrkesito = '2'
028500990910     C                   endif
028600000710     C                   endif
028700990910     C*
028800990914     C                   if        %open(tivin00r)
028900990908     C                   close     tivin00r
029000990914     C                   endif
029100130315     C                   if        %open(edivabwr)
029200130315     C                   close     edivabwr
029300130315     C                   endif
029400130315     C                   if        %open(edivatwr)
029500130315     C                   close     edivatwr
029600130315     C                   endif
029700990910     C*
029800100525     C                   if        §ctrokvb > 0 OR
029900100525     C                             §ctrokvt > 0
030000000724     C                             and vlrpoi <> *zeros
030100010202     C                   exsr      invio
030200990920     C                   endif
030300990920     C*
030400910830     C                   ENDSR
030500000613     C***
030600100412
030700100412
030800010305
030900010305     C*----------------------------------------------------*
031000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031100010305     C*----------------------------------------------------*
031200020305     C     WRIVAB        BEGSR
031300100412     C*
031400100412     C* Gestisco l'eventuale rottura x numero spedizione
031500130315     C                   if        ok_VABRMA = *blanks
031600130315     C                   movel(P)  ok_VABRMN     ok_VABRMA
031700130315     C                   endif
031800130315     C*
031900130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
032000130315     C                   EVAL      ok_VABCMR = wCMR
032100130315     C                   EVAL      ok_VABDCM = datcor
032200130315     C                   EVAL      ok_VABDTS = datcor
032300130315     C                   EVAL      ok_VABHMS = oracor
032400130315     C                   EVAL      ok_VABCNT = 1
032500100412     C*
032600100525     C                   if        *in51 = *off and *in31 = *off
032700130315     C                   write     edivab00                                     => scarico il VAB
032800100525     C                   endif
032900010305     C*
033000010305     C                   ENDSR
033100990920
033200000801     C*----------------------------------------------------*
033300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033400000801     C*----------------------------------------------------*
033500010201     C     INZVAR        BEGSR
033600130315     C*
033700130315     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033800000801     C*
033900010201     C                   Z-ADD     *zeros        Num5_0
034000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034100020725     C                   MOVEL     '0'           FlgCAS            1
034200000801     C*
034300000801     C                   ENDSR
034400000801     C*----------------------------------------------------*
034500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034600000801     C*----------------------------------------------------*
034700020904     C     DEFCAM        BEGSR
034800080923     C*
034900100928     C                   SETOFF                                       505160
035000000801     C*
035100130315     C                   CLEAR                   FIVABDS
035200130315     C                   CLEAR                   FIVABDS_OK
035300130315     C                   CLEAR                   FIVABDS_SAV
035400130315     C                   CLEAR                   EDIVAB00
035500130315     C                   CLEAR                   EDIVAT00
035600070730     C* Imposto i valori di default...
035700111020     C                   EVAL      VABCCM = 2751348
035800111020     C                   EVAL      VATCCM = 2751348
035900111020     C                   EVAL      VABLNP = 275
036000111020     C                   EVAL      VATLNP = 275
036100130315     C                   EVAL      VABCTR = 300
036200100525     C                   EVAL      VABCBO = '1'
036300070222     C* ... e poi verifico se sono stati passati come parametri
036400070222     C                   IF        vlrppt > *blanks
036500100525     C*
036600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036800070222     C                   EXSR      CHKNUM
036900070222     C                   IF        PiInt=*on
037000070222     C                   Z-ADD     PiVal         VABCCM
037100070222     C                   Z-ADD     PiVal         VATCCM
037200070222     C                   ENDIF
037300070222     C                   ENDIF
037400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037600070222     C                   EXSR      CHKNUM
037700070222     C                   IF        PiInt=*on
037800070222     C                   Z-ADD     PiVal         VABLNP
037900070222     C                   Z-ADD     PiVal         VATLNP
038000070222     C                   ENDIF
038100070222     C                   ENDIF
038200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
038300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038400070222     C                   EXSR      CHKNUM
038500070222     C                   IF        PiInt=*on
038600070222     C                   Z-ADD     PiVal         VABCTR
038700070222     C                   ENDIF
038800070928     C                   ENDIF
038900071009     C                   MOVEL     *blanks       wDISK             1
039000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
039100071009     C                   IF        wDISK <> *blanks
039200070928     C                   SETON                                        50
039300070222     C                   ENDIF
039400100525     C                   IF        wDISK  = 'T'
039500100525     C                   SETOFF                                       50
039600100525     C                   SETON                                        51
039700100525     C                   ENDIF
039800100401     C                   MOVEL     *blanks       wDUPREC           1
039900100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
040000100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
040100100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
040200100525     C                   ENDIF
040300130315     C*
040400130315     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
040500130315     C                   move(p)   vlrMSG        wNomeFile
040600130315     C*
040700130315     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
040800130315     C* del codice CMR dal nome del file importato.
040900130315     C                   if        wNomeFile <> *blanks AND
041000130315     C                             vlrppt<>*blanks
041100130315     C                   eval      wPosDaA = %subst(vlrppt:19:2)
041200130315     C                   move(p)   wPosDaA       wPosDa
041300130315     C                   eval      wLungA  = %subst(vlrppt:21:2)
041400130315     C                   move(p)   wLungA        wLung
041500130315     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
041600130315     C                   else
041700130315     C                   movel     datcor        wDataOraAlfa
041800130315     C                   move      oracor        wDataOraAlfa
041900130315     C                   eval      wCMR = 'INVIO DEL: ' + wDataOraAlfa
042000130315     C                   endif
042100130315     C*
042200070222     C                   ENDIF
042300071009     C*
042400130315     C* questa gestione è un falso disk A (perché non scrive VAT_E) ma in realtà è un disk C
042500130315     C***50              EVAL      VABCTM = '7Q'
042600130315     C                   EVAL      VABCTM = '7Q'
042700000801     C*
042800000801     C                   ENDSR
042900091223     C*----------------------------------------------------*
043000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
043100091223     C*----------------------------------------------------*
043200091223     C     REPNSP        BEGSR
043300091223     C*
043400091223     C                   EXSR      INZVAR
043500091223     C                   EXSR      DEFCAM
043600100928     C*
043700100928     C                   SETON                                        60
043800091223     C*
043900091223     C* NSP => Stacco un numeratore da AZNUM
044000091223     C                   clear                   TRUL33DS
044100091223     C                   eval      I33OPE = *zeros
044200091223     C                   eval      I33CNU = 302
044300091223     C                   eval      I33NUM = 1
044400091223     C                   movel     TRUL33DS      KPJBU
044500091223     C                   call      'TRUL33R'
044600091223     C                   parm                    KPJBA
044700091223     C                   movel     KPJBU         TRUL33DS
044800091223     C                   if        O33ERR = *zeros
044900091223     C                   z-add     O33NRF        VABNSP
045000091223     C                   z-add     O33NRF        VATNSP
045100091223     C                   else
045200091223     C                   SETON                                        31
045300091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045400091223     C                             + ' ' + 'VABNSP VATNSP'
045500091223     C                   endif
045600091223     C*
045700091223     C                   ENDSR
045800000801     C*----------------------------------------------------*
045900130315     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
046000000801     C*----------------------------------------------------*
046100100730     C     IMPVAB        BEGSR
046200070530     C*
046300070530     C                   SETOFF                                       3132
046400070928     C*
046500070928     C                   MOVE(P)   vlrpoi        VABFGS
046600070928     C                   MOVE(P)   vlrpoi        VATFGS
046700070928     C*
046800070928     C                   MOVEL     datcor        VABAAS
046900070928     C                   MOVEL     datcor        VATAAS
047000070928     C                   MOVE      datcor        VABMGS
047100100729     C*
047200100729     C* Reperimento campi ALFA
047300111020     C                   EVAL      VABRSD=%trim(%subst(vindta:162:50))
047400111020     C                   EVAL      VABRD2=%trim(%subst(vindta:212:35))
047500111020     C                   EVAL      VABIND=%trim(%subst(vindta:247:30))
047600111020     C                   EVAL      VABLOD=%trim(%subst(vindta:277:30))
047700111020     C                   EVAL      VABCAD=%trim(%subst(vindta:307:5))
047800111020     C                   EVAL      VABPRD=%trim(%subst(vindta:314:2))
047900111020     C                   EVAL      VABRMA=%trim(%subst(vindta:648:8))
048000110801     C*
048100110801     C                   SELECT
048200111020     C                   WHEN      %subst(vindta:647:1) = 'C'
048300111020     C                   EVAL      VABCBO='4'
048400110801     C                   ENDSL
048500100729     C*
048600100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
048700100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
048800100729     C     '@':'A'       XLATE     VABRSD        VABRSD
048900100729     C* ==
049000100729     C*
049100100729     C* Reperimento campi NUMERICI
049200100729     C                   MOVEL     DATCOR        VABAAS
049300100729     C                   MOVE      DATCOR        VABMGS
049400110408     C*
049500111020     C* NSP
049600101119     C                   IF        *IN51 = *off
049700130311     C                   EVAL      PiStr=%subst(vindta:124:8)
049800100730     C                   EXSR      CHKNUM
049900100730     C                   IF        PiInt=*on
050000100928     C  N60              Z-ADD     PiVal         VABNSP
050100100928     C  N60              Z-ADD     PiVal         VATNSP
050200100730     C                   ELSE
050300100730     C                   SETON                                        32
050400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050500111020     C                             + ' ' + 'VABNSP VATNSP'
050600100730     C                   ENDIF
050700101119     C*
050800101119     C                   ELSE
050900101119     C*
051000130311     C                   EVAL      PiStr=%subst(vindta:124:8)
051100101119     C                   EXSR      CHKNUM
051200101119     C                   IF        PiInt=*on
051300101119     C  N60              Z-ADD     PiVal         VATNSP
051400101119     C                   ELSE
051500101119     C                   SETON                                        32
051600101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051700101119     C                             + ' ' + 'VATNSP'
051800101119     C                   ENDIF
051900101119     C                   ENDIF
052000111020     C* RMN
052100111020     C                   EVAL      PiStr=%subst(vindta:124:8)
052200111020     C                   EXSR      CHKNUM
052300111020     C                   IF        PiInt=*on
052400111020     C                   Z-ADD     PiVal         VABRMN
052500111020     C                   ELSE
052600130311     C                   Z-ADD     1             VABRMN
052700111020     C  N51              SETON                                        32
052800111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052900111020     C                             + ' ' + 'VABRMN'
053000111020     C                   ENDIF
053100110801     C* NCL
053200111020     C                   EVAL      PiStr=%subst(vindta:510:5)
053300110801     C                   EXSR      CHKNUM
053400110801     C                   IF        PiInt=*on
053500110801     C                   ADD       PiVal         VABNCL
053600110801     C                   ELSE
053700110801     C  N51              SETON                                        32
053800111020     C  N51              Z-ADD     *zeros        VABNCL
053900110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054000110801     C                             + ' ' + 'VABNCL'
054100110801     C                   ENDIF
054200100729     C* PKB
054300111020     C                   EVAL      PiStr=%trim(%subst(vindta:486:8))
054400100729     C                   EXSR      CHKNUM
054500100729     C                   IF        PiNum=*on
054600110627     C                   ADD(H)    PiVal         VABPKB
054700100729     C                   ELSE
054800101119     C  N51              SETON                                        32
054900101119     C  N51              Z-ADD     *zeros        VABPKB
055000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055100100729     C                             + ' ' + 'VABPKB'
055200100729     C                   ENDIF
055300111020     C*
055400130315     C***                IF        VABPKB <= 30
055500130315     C***                EVAL      VABCCM  = 2751348
055600130315     C***                ELSE
055700130315     C***                EVAL      VABCCM  = 2751349
055800130315     C***                ENDIF
055900111020     C*
056000111020     C                   SELECT
056100111020     C                   WHEN      VABCCM = 2751348
056200130315     C                   EVAL      VABCTR = 001
056300111020     C                   WHEN      VABCCM = 2751349
056400111020     C                   EVAL      VABCTR = 001
056500111020     C                   ENDSL
056600100729     C*
056700100729     C* Se provincia nn valorizzata la reperisco
056800100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
056900100729     C                   IF        VABNZD  = *blanks AND
057000100729     C                             VABPRD  = *blanks AND
057100100729     C                             VABCAD <> *blanks
057200100729     C                   CLEAR                   TISI95DS
057300100729     C                   EVAL      I95TCN = '3'
057400100729     C                   Z-ADD     datcor        I95DAT
057500100729     C                   EVAL      I95NAR = VABNZD
057600100729     C                   EVAL      I95CAP = VABCAD
057700100729     C                   EVAL      I95LOC = VABLOD
057800100729     C                   CALL      'TISI95R'
057900100729     C                   PARM                    TISI95DS
058000100729     C                   EVAL      VABPRD = O95PRV
058100100729     C                   ENDIF
058200100729     C*
058300100729     C* Considerazioni finali su CBO/CAS
058400100729     C                   IF        FlgCAS = '1'
058500100729     C                   IF        VABCBO = '1'
058600100729     C                   EVAL      VABCBO = '4'
058700100729     C                   ENDIF
058800100729     C                   IF        VABCBO = '2'
058900100729     C                   EVAL      VABCBO = '6'
059000100729     C                   ENDIF
059100100729     C                   ENDIF
059200010202     C*
059300000801     C* Ebbene...
059400000801     C                   ADD       1             §CTRMO
059500070530     C                   IF        *in31 <> *zeros OR
059600070530     C                             *in32 <> *zeros
059700000801     C                   ADD       1             §CTRNO
059800020725     C                   EVAL      x_vinflg = '2'
059900000801     C                   ELSE
060000010201     C                   ADD       1             §CTROKVB
060100000801     C                   ENDIF
060200000801     C*
060300000801     C                   ENDSR
060400100127     C*----------------------------------------------------*
060500130315     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
060600100127     C*----------------------------------------------------*
060700100127     C     WRIVAT_A      BEGSR
060800100127     C*
060900130315     C* Valorizzo l buffer di scrittura del EDIVAT
061000100127     C* - TIPO RECORD "A"
061100101119     C***                eval      VATTRC = 'A'
061200130315     C*
061300130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
061400130315     C***
061500130315     C***                EVAL      VATCMR = wCMR
061600130315     C***                EVAL      VATCNT = 1
061700130315     C*
061800101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
061900101119     C***                exsr      wrivat
062000100127     C*
062100100127     C                   ENDSR
062200010201     C*----------------------------------------------------*
062300130315     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
062400010201     C*----------------------------------------------------*
062500071003     C     WRIVAT_B      BEGSR
062600010201     C*
062700130315     C* Valorizzo l buffer di scrittura del EDIVAT
062800070928     C* - TIPO RECORD "B"
062900110627     C***                eval      VATTRC = 'B'
063000130315     C*
063100130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
063200130315     C***
063300130315     C***                EVAL      VATCMR = wCMR
063400130315     C***                EVAL      VATCNT = 1
063500130315     C*
063600110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
063700110627     C***                exsr      wrivat
063800010201     C*
063900010201     C                   ENDSR
064000071003     C*----------------------------------------------------*
064100130315     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
064200071003     C*----------------------------------------------------*
064300071003     C     WRIVAT_E      BEGSR
064400071003     C*
064500130315     C* Valorizzo l buffer di scrittura del EDIVAT
064600071003     C* - TIPO RECORD "E"
064700130315     C*
064800130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064900130315     C***
065000130315     C***                EVAL      VATCMR = wCMR
065100130315     C***                EVAL      VATCNT = 1
065200130315     C***
065300130315     C***                eval      VATTRC = 'E'
065400130315     C***                eval      VATNOT = %trim(%subst(vindta:415:11))
065500130315     C***                exsr      wrivat
065600071003     C*
065700071003     C                   ENDSR
065800100127     C*----------------------------------------------------*
065900130315     C*  SCARICO BUFFER EDIVAT
066000100127     C*----------------------------------------------------*
066100100127     C     WRIVAT        BEGSR
066200100127     C*
066300100127     C                   if        vatNOT <> *blanks
066400100127     C                   ADD       1             §CTROKVT
066500130315     C                   write     EDIVAT00
066600100127     C                   endif
066700100127     C*
066800100127     C                   ENDSR
066900020904
067000020904
067100130315     C*----------------------------------------------------*
067200130315     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
067300130315     C*----------------------------------------------------*
067400130315     C     PREVAT        BEGSR
067500130315     C*
067600130315     C* Compongo il nome del membro da dare al EDIVATWR
067700130315     C                   eval      parmbr = vlrhdl
067800130315     C                   movel     'M'           parmbr
067900130315     C                   eval      parccm = %subst(vlrKSC:2:7)
068000130315     C                   eval      paropz = '1'
068100130315     C* Effettuo la chiamata al CLLE preposto
068200130315     C  N51              call(e)   'TITVEVTC'
068300130315     C                   parm                    parccm
068400130315     C                   parm                    parmbr
068500130315     C                   parm                    paropz
068600130315     C   51              call(e)   'TITVEVBC'
068700130315     C                   parm                    parccm
068800130315     C                   parm                    parmbr
068900130315     C                   parm                    paropz
069000130315     C*
069100130315     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069200130315     C                   if        %error
069300130315     C                   movel     '1'           chkcall
069400130315     C                   else
069500130315     C                   movel     '0'           chkcall
069600130315     C                   endif
069700130315     C*
069800130315     C                   ENDSR
069900000801     C*----------------------------------------------------*
070000000801     C*  CONTROLLO NUMERICITA' CAMPI
070100000801     C*----------------------------------------------------*
070200000801     C     CHKNUM        BEGSR
070300081113     C*
070400081113     C                   IF        PiDecChr = *blanks
070500110408     C                   EVAL      PiDecChr = '.'
070600081113     C                   ENDIF
070700091223     C*
070800081113     C                   callp(e)  UBISNUM_Check(PiStr
070900081113     C                                          :PiDecChr
071000081113     C                                          :PiVal
071100081113     C                                          :PiNum
071200081113     C                                          :PiInt)
071300081113     C*
071400000801     C                   IF        %error
071500000801     C                   EVAL      PiInt=*off
071600000801     C                   ENDIF
071700000801     C*
071800000801     C                   ENDSR
071900000801     C***
072000011113
072100011113
072200000801
072300000801
072400990920      /TITLE Invio dei dati al punto operativo.
072500010202     C     invio         BEGSR
072600990920     C*
072700130315     C* 1° invio EDIVAT
072800010201     C                   reset                   dscmz
072900110411     C                   move      vatfgs        cmzdst
073000130315     C                   eval      cmzfld = 'EDIVATWR'
073100010201     C                   eval      cmzmbd = vlrhdl
073200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073300021009     C***                if        prmfir = *blanks
073400130315     C                   eval      cmzfla = 'EDIVAT0F'
073500130315     C                   eval      cmzmba = 'EDIVAT0F'
073600021009     C***                else
073700021009     C***                eval      cmzfla = prmfir
073800021009     C***                eval      cmzmba = prmfir
073900021009     C***                endif
074000010201     C                   eval      cmznrr = *zeros
074100020305     C                   move      §ctrokvt      cmznrr
074200021018     C                   eval      cmzlba = vlrfl1
074300010201     C                   call(e)   'TIS711C'
074400010201     C                   parm                    dscmz
074500010201     C                   parm      *blanks       esito
074600010205     C                   if        %error
074700010205     C                             or cmzerr = '1'
074800010205     C                             or esito  = '1'
074900010205     C                   eval      wrkesito = '3'
075000010205     C                   else
075100010201     C*
075200130315     C* 2° invio EDIVAB
075300100525     C                   if        *in51 = *off
075400010201     C                   reset                   dscmz
075500110411     C                   move      vabfgs        cmzdst
075600010201     C                   eval      cmzfld = vlrfou
075700010201     C                   eval      cmzmbd = vlrhdl
075800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075900021009     C***                if        prmfir = *blanks
076000130315     C                   eval      cmzfla = 'EDIVAB0F'
076100130315     C                   eval      cmzmba = 'EDIVAB0F'
076200021009     C***                else
076300021009     C***                eval      cmzfla = prmfir
076400021009     C***                eval      cmzmba = prmfir
076500021009     C***                endif
076600010201     C                   eval      cmznrr = *zeros
076700010201     C                   move      §ctrokvb      cmznrr
076800021018     C                   eval      cmzlba = vlrfl1
076900010201     C                   call(e)   'TIS711C'
077000010201     C                   parm                    dscmz
077100010201     C                   parm      *blanks       esito
077200010201     C                   if        %error
077300010201     C                             or cmzerr = '1'
077400010201     C                             or esito  = '1'
077500010201     C                   eval      wrkesito = '3'
077600010201     C                   endif
077700100525     C                   endif
077800010205     C                   endif
077900990920     C*
078000000613     C                   ENDSR
078100000613     C***
078200070411
078300110627     C     *pssr         BEGSR
078400070411     C*
078500110627     C                   if        %open(tivin00r)
078600110627     C                   close     tivin00r
078700110627     C                   endif
078800130315     C                   if        %open(edivabwr)
078900130315     C                   close     edivabwr
079000130315     C                   endif
079100130315     C                   if        %open(edivatwr)
079200130315     C                   close     edivatwr
079300130315     C                   endif
079400110627     C*
079500110627     C* Effettuo la chiamata al CLLE preposto
079600130315     C  N51              call(e)   'TITVEVTC'
079700110627     C                   parm                    parccm
079800110627     C                   parm                    parmbr
079900110627     C                   parm      '2'           paropz
080000130315     C   51              call(e)   'TITVEVBC'
080100110627     C                   parm                    parccm
080200110627     C                   parm                    parmbr
080300110627     C                   parm      '2'           paropz
080400110627     C*
080500110627     C                   eval      wrkesito = '2'
080600110627     C*
080700110627     C                   seton                                        LR
080800110627     C*
080900110627     C                   ENDSR     '*CANCL'
081000070411     C***
081100070411
081200990910
081300000613     C     *inzsr        BEGSR
081400990910     C*
081500990910     C     *entry        plist
081600990920     C                   parm                    tivlrds
081700990921     C                   parm      wrkesito      esito
081800000724     C                   parm                    prmlit
081900000710     C                   parm                    prmfir
082000000613     C*
082100000830     C* CALCOLA LA DATA CORRENTE
082200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082300091223     C                   eval      datcor = %dec(%date() : *ISO)
082400130315     C                   time                    wn14             14 0
082500130315     C                   movel     wn14          oracor            6 0          *ORA
082600000830     C*
082700000613     C                   ENDSR
082800000613     C***
