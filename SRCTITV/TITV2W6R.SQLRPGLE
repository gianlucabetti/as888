000100130315      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130315     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500130315     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000600130315     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600130315     D fivabds       e ds                  extname(EDIVAB0f)
001700130315     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
001800130315     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400111020     D depspe          s              8    INZ(*blanks)
002500111020     D curspe          s              8    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100091223     D Num5_0          s              5  0
003200130315     D wNomeFile       s             30    INZ(*blanks)
003300130315     D wCMR            s             35    INZ(*blanks)
003400130315     D wPosDaA         s              2    INZ(*blanks)
003500130315     D wPosDa          s              2  0 INZ(*zeros)
003600130315     D wLungA          s              2    INZ(*blanks)
003700130315     D wLung           s              2  0 INZ(*zeros)
003800130315     D wDataOraAlfa    s             14    INZ(*blanks)
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400100729     D  VINDTA                     2048
004500100729     D  VINFLG                        1
004600111020     D  VINSPE                        8
004700100729     D  VINRRN                        8  0
004800020725     D*
004900081113
005000091223     D*------------------
005100091223     D* DS REPERIMENTO NUMERATORE
005200091223     D*------------------
005300100309     D trul33ds      e ds                  inz
005400091223     D*------------------
005500091223     D* DS ARCHITETTURA
005600091223     D*------------------
005700091223     D kpjba         e ds                  inz
005800091223
005900101119
006000101119     D*-------------------
006100101119     D* COSTANTI
006200101119     D*-------------------
006300101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006400101119     D                                     èéù')
006500101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006600101119     D                                     EEU')
006700081113
006800081113     D*------------------
006900081113     D* LINKING A DEFINIZIONI ESTERNE
007000081113     D*------------------
007100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007200090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007300081113
007400990908
007500010201
007600010201
007700080117     C*
007800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007900080117     C
008000080117     C/EXEC SQL
008100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008200080117     C/END-EXEC
008300080117     C
008400000913     C                   reset                   rrnum
008500990921     C                   reset                   esito
008600990921     C                   reset                   wrkesito
008700000613     C*
008800100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009000000613     C*
009100010202     C* Effettuo la chiamata al CLLE preposto
009200130315     C  N51              call(e)   'TITVEVTC'
009300100525     C                   parm                    parccm
009400100525     C                   parm                    parmbr
009500100525     C                   parm      '2'           paropz
009600130315     C   51              call(e)   'TITVEVBC'
009700100525     C                   parm                    parccm
009800100525     C                   parm                    parmbr
009900100525     C                   parm      '2'           paropz
010000070730     C*
010100070730     C* Effettuo lancio TISI95 solo x chiusura
010200070730     C                   CLEAR                   TISI95DS
010300070730     C                   EVAL      I95TLA = 'C'
010400070730     C                   CALL      'TISI95R'
010500070730     C                   PARM                    TISI95DS
010600000616     C*
010700000801     C
010800010201     C                   seton                                        LR
010900990908
011000000801
011100910830     C*--------------------------------------------------------
011200130315     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011300910830     C*--------------------------------------------------------
011400070530     C     RWFILE        BEGSR
011500990910     C*
011600990914     C                   if        not %open(tivin00r)
011700990908     C                   open      tivin00r
011800990914     C                   endif
011900130315     C                   if        not %open(edivabwr)
012000130315     C                   open      edivabwr
012100130315     C                   endif
012200070530     C*
012300130315     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012400130315     C                   exsr      prevat
012500010201     C*
012600010202     C                   if        chkcall = '0'
012700010202     C*
012800130315     C                   if        not %open(edivatwr)
012900130315     C                   open      edivatwr
013000100525     C                   endif
013100080117     C*
013200080117     C                   EXSR      INZVAR
013300080117     C                   EXSR      DEFCAM
013400990910     C*
013500010201     C                   clear                   §CTROKVB          5 0
013600020305     C                   clear                   §CTROKVT          5 0
013700000801     C                   clear                   §CTRMO            5 0
013800000801     C                   clear                   §CTRNO            5 0
013900990910     C*
014000020725     C/EXEC SQL
014100020725     C+ declare C1 cursor for select
014200161024     C+ vindta, vinflg, substr(vindta, 9, 8) as sped, rrn(tivin00r)
014300060223     C+ from tivin00r
014400111020     C+ order by sped
014500020725     C+ for read only
014600020725     C/END-EXEC
014700020725     C
014800020725     C/EXEC SQL
014900020725     C+ open C1
015000020725     C/END-EXEC
015100020725     C
015200020725     C/EXEC SQL
015300070530     C+ Fetch C1 into :INPUT_DS
015400020725     C/END-EXEC
015500020725     C*
015600020725     C                   dow       sqlcod = *zeros
015700990913     C*
015800100310     C                   if        vindta > *blanks
015900000613     C                   add       1             rrnum
016000000801     C*
016100020725     C                   if        vinflg = *blanks
016200020725     C                             or vinflg = '0'
016300020725     C                             or vinflg = '2'
016400000801     C*
016500020725     C                   clear                   x_vinmsg
016600020725     C                   eval      x_vinflg = '1'
016700101119     C*
016800101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016900101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
017000010305     C*
017100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017200161024     C                   EVAL      curspe=%trim(%subst(vindta:9:8))
017300010305     C*
017400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017500080923     C                   if        *in50 = *off
017600101119     C                   exsr      inzvar
017700101119     C                   exsr      defcam
017800071003     C                   exsr      impvab
017900130315     C  N31              eval      fivabds_ok = fivabds
018000130315     C  N31              exsr      wrivab
018100100127     C                   exsr      wrivat_a                                     => carico VAT
018200071003     C                   exsr      wrivat_b                                     => carico VAT
018300100525     C   51              exsr      wrivat_e                                     => carico VAT
018400071003     C                   else
018500080923     C*
018600071009     C                   if        wDISK = 'D'
018700091223     C                   exsr      repNSP
018800071009     C                   exsr      impvab
018900130315     C  N31              eval      fivabds_ok = fivabds
019000130315     C  N31              exsr      wrivab
019100100127     C                   exsr      wrivat_a                                     => carico VAT
019200071009     C                   exsr      wrivat_b                                     => carico VAT
019300071009     C                   exsr      wrivat_e                                     => carico VAT
019400071009     C                   else
019500071009     C*
019600010305     C                   if        depspe = *blanks                             => 1° giro
019700010305     C                   eval      depspe = curspe                              => memorizz. spediz
019800080117     C                   clear                   tivinds
019900091223     C                   exsr      repNSP
020000020305     C                   exsr      impvab
020100130315     C                   z-add     VABNCL        sav_VABNCL
020200130315     C                   z-add     VABPKB        sav_VABPKB
020300130315     C                   z-add     VABVLB        sav_VABVLB
020400100127     C                   exsr      wrivat_a                                     => carico VAT
020500071003     C                   exsr      wrivat_b                                     => carico VAT
020600071003     C   50              exsr      wrivat_e                                     => carico VAT
020700010305     C                   else
020800020725     C                   if        depspe <> curspe                             => rottura di spediz
020900010305     C                   eval      depspe = curspe                              => memorizz. spediz
021000130315     C                   z-add     sav_VABNCL    VABNCL
021100130315     C                   z-add     sav_VABPKB    VABPKB
021200130315     C                   z-add     sav_VABVLB    VABVLB
021300130315     C  N31              eval      fivabds_ok = fivabds
021400130315     C  N31              exsr      wrivab
021500080117     C                   clear                   tivinds
021600091223     C                   exsr      repNSP
021700020305     C                   exsr      impvab
021800130315     C                   if        wCntRecSpe = *zeros
021900130315     C                   z-add     VABNCL        sav_VABNCL
022000130315     C                   z-add     VABPKB        sav_VABPKB
022100130315     C                   z-add     VABVLB        sav_VABVLB
022200130315     C                   endif
022300130315     C                   add       1             wCntRecSpe
022400100127     C                   exsr      wrivat_a                                     => carico VAT
022500071003     C                   exsr      wrivat_b                                     => carico VAT
022600071003     C   50              exsr      wrivat_e                                     => carico VAT
022700020305     C                   else                                                   => x stessa spediz
022800100401     C*
022900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
023000100928     C                   select
023100100928     C                   when      wDUPREC = ' '
023200020305     C                   exsr      impvab
023300100127     C                   exsr      wrivat_a                                     => carico VAT
023400071003     C                   exsr      wrivat_b                                     => carico VAT
023500071003     C   50              exsr      wrivat_e                                     => carico VAT
023600100928     C                   when      wDUPREC = 'C'
023700100928     C   50              exsr      wrivat_e                                     => carico VAT
023800100928     C                   endsl
023900010305     C                   endif
024000010305     C                   endif
024100010305     C                   endif
024200071003     C                   endif
024300110201     C*
024400071009     C                   endif
024500000905     C*
024600000905     C                   else
024700020725     C                   eval      x_vinflg = '1'
024800000905     C                   endif
024900000905     C*
025000020725     C     VINRRN        chain     tivin000
025100020725     C                   if        %found(tivin00r)
025200020725     C                   eval      y_vinflg = x_vinflg
025300020725     C                   eval      y_vinmsg = x_vinmsg
025400020725     C                   update    tivin000
025500020725     C                   endif
025600020725     C*
025700020725     C/EXEC SQL
025800070530     C+ Fetch C1 into :INPUT_DS
025900020725     C/END-EXEC
026000020725     C*
026100020725     C                   enddo
026200020725     C*
026300020725     C/EXEC SQL
026400020725     C+ close C1
026500020725     C/END-EXEC
026600000905     C*
026700020305     C* Scarico i VAB rimasti "in sospeso"
026800071009     C                   if        wDISK = 'C'
026900161117     C  N31              eval      fivabds_ok = fivabds
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
035700161024     C                   EVAL      VABCCM = 0041153
035800161024     C                   EVAL      VATCCM = 0041153
035900161024     C                   EVAL      VABLNP = 004
036000161024     C                   EVAL      VATLNP = 004
036100161024     C                   EVAL      VABCTR = 010
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
040400130315     C                   movel     datcor        wDataOraAlfa
040500130315     C                   move      oracor        wDataOraAlfa
040600130315     C                   eval      wCMR = 'INVIO DEL: ' + wDataOraAlfa
040700130315     C*
040800070222     C                   ENDIF
040900071009     C*
041000161024     C   50              EVAL      VABCTM = '7Q'
041100000801     C*
041200000801     C                   ENDSR
041300091223     C*----------------------------------------------------*
041400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041500091223     C*----------------------------------------------------*
041600091223     C     REPNSP        BEGSR
041700091223     C*
041800091223     C                   EXSR      INZVAR
041900091223     C                   EXSR      DEFCAM
042000100928     C*
042100100928     C                   SETON                                        60
042200091223     C*
042300091223     C* NSP => Stacco un numeratore da AZNUM
042400091223     C                   clear                   TRUL33DS
042500091223     C                   eval      I33OPE = *zeros
042600091223     C                   eval      I33CNU = 302
042700091223     C                   eval      I33NUM = 1
042800091223     C                   movel     TRUL33DS      KPJBU
042900091223     C                   call      'TRUL33R'
043000091223     C                   parm                    KPJBA
043100091223     C                   movel     KPJBU         TRUL33DS
043200091223     C                   if        O33ERR = *zeros
043300091223     C                   z-add     O33NRF        VABNSP
043400091223     C                   z-add     O33NRF        VATNSP
043500091223     C                   else
043600091223     C                   SETON                                        31
043700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043800091223     C                             + ' ' + 'VABNSP VATNSP'
043900091223     C                   endif
044000091223     C*
044100091223     C                   ENDSR
044200000801     C*----------------------------------------------------*
044300130315     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
044400000801     C*----------------------------------------------------*
044500100730     C     IMPVAB        BEGSR
044600070530     C*
044700070530     C                   SETOFF                                       3132
044800070928     C*
044900070928     C                   MOVE(P)   vlrpoi        VABFGS
045000070928     C                   MOVE(P)   vlrpoi        VATFGS
045100070928     C*
045200070928     C                   MOVEL     datcor        VABAAS
045300070928     C                   MOVEL     datcor        VATAAS
045400070928     C                   MOVE      datcor        VABMGS
045500100729     C*
045600100729     C* Reperimento campi ALFA
045700161024     C                   EVAL      VABRSD=%trim(%subst(vindta:204:35))
045800161024     C                   EVAL      VABRD2=%trim(%subst(vindta:239:35))
045900161024     C                   EVAL      VABIND=%trim(%subst(vindta:274:70))
046000161024     C                   EVAL      VABLOD=%trim(%subst(vindta:353:35))
046100161024     C                   EVAL      VABCAD=%trim(%subst(vindta:344:9))
046200170208     C***                EVAL      VABRMA=%trim(%subst(vindta:9:8))
046300170210     C                   EVAL      VABRMA=%trim(%subst(vindta:624:35))
046400161024     C                   MOVEL     *blanks       wNOTE            70
046500170208     C***                EVAL      wNOTE =%trim(%subst(vindta:454:72))
046600170208     C                   EVAL      wNOTE =%trim(%subst(vindta:680:500))
046700161024     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
046800161024     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
046900170208     C                   IF        %trim(%subst(vindta:659:2)) = 'A'
047000170208     C                   EVAL      VABTC1='A'
047100170208     C                   ENDIF
047200170208     C                   IF        %trim(%subst(vindta:661:5)) = 'BM'
047300170208     C                   EVAL      VABTIC='BM'
047400170208     C                   ENDIF
047500100729     C*
047600100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
047700100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
047800100729     C     '@':'A'       XLATE     VABRSD        VABRSD
047900100729     C* ==
048000100729     C*
048100100729     C* Reperimento campi NUMERICI
048200100729     C                   MOVEL     DATCOR        VABAAS
048300100729     C                   MOVE      DATCOR        VABMGS
048400110408     C*
048500111020     C* NSP
048600101119     C                   IF        *IN51 = *off
048700161024     C                   EVAL      PiStr=%subst(vindta:9:8)
048800100730     C                   EXSR      CHKNUM
048900100730     C                   IF        PiInt=*on
049000100928     C  N60              Z-ADD     PiVal         VABNSP
049100100928     C  N60              Z-ADD     PiVal         VATNSP
049200100730     C                   ELSE
049300100730     C                   SETON                                        32
049400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049500111020     C                             + ' ' + 'VABNSP VATNSP'
049600100730     C                   ENDIF
049700101119     C*
049800101119     C                   ELSE
049900101119     C*
050000161024     C                   EVAL      PiStr=%subst(vindta:9:8)
050100101119     C                   EXSR      CHKNUM
050200101119     C                   IF        PiInt=*on
050300101119     C  N60              Z-ADD     PiVal         VATNSP
050400101119     C                   ELSE
050500101119     C                   SETON                                        32
050600101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700101119     C                             + ' ' + 'VATNSP'
050800101119     C                   ENDIF
050900101119     C                   ENDIF
051000111020     C* RMN
051100161024     C                   EVAL      PiStr=%subst(vindta:9:8)
051200111020     C                   EXSR      CHKNUM
051300111020     C                   IF        PiInt=*on
051400111020     C                   Z-ADD     PiVal         VABRMN
051500111020     C                   ELSE
051600130311     C                   Z-ADD     1             VABRMN
051700111020     C  N51              SETON                                        32
051800111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051900111020     C                             + ' ' + 'VABRMN'
052000111020     C                   ENDIF
052100110801     C* NCL
052200161024     C                   EVAL      PiStr=%subst(vindta:426:5)
052300110801     C                   EXSR      CHKNUM
052400110801     C                   IF        PiInt=*on
052500161117     C                   Z-ADD     PiVal         VABNCL
052600110801     C                   ELSE
052700110801     C  N51              SETON                                        32
052800111020     C  N51              Z-ADD     *zeros        VABNCL
052900110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053000110801     C                             + ' ' + 'VABNCL'
053100110801     C                   ENDIF
053200100729     C* PKB
053300161024     C                   EVAL      PiStr=%trim(%subst(vindta:431:6))
053400100729     C                   EXSR      CHKNUM
053500100729     C                   IF        PiNum=*on
053600161024     C                   EVAL      PiVal=PiVal/10                               * gestisco 1 dec.
053700161123     C                   Z-ADD     PiVal         VABPKB
053800100729     C                   ELSE
053900101119     C  N51              SETON                                        32
054000101119     C  N51              Z-ADD     *zeros        VABPKB
054100101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054200100729     C                             + ' ' + 'VABPKB'
054300100729     C                   ENDIF
054400100729     C*
054500100729     C* Se provincia nn valorizzata la reperisco
054600100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
054700100729     C                   IF        VABNZD  = *blanks AND
054800100729     C                             VABPRD  = *blanks AND
054900100729     C                             VABCAD <> *blanks
055000100729     C                   CLEAR                   TISI95DS
055100100729     C                   EVAL      I95TCN = '3'
055200100729     C                   Z-ADD     datcor        I95DAT
055300100729     C                   EVAL      I95NAR = VABNZD
055400100729     C                   EVAL      I95CAP = VABCAD
055500100729     C                   EVAL      I95LOC = VABLOD
055600100729     C                   CALL      'TISI95R'
055700100729     C                   PARM                    TISI95DS
055800100729     C                   EVAL      VABPRD = O95PRV
055900100729     C                   ENDIF
056000170208     C* CAS
056100170210     C                   IF        %trim(%subst(vindta:666:14)) <> *blanks
056200170208     C                   EVAL      PiStr=%trim(%subst(vindta:666:14))
056300170208     C                   EXSR      CHKNUM
056400170208     C                   IF        PiNum=*on
056500170208     C                   EVAL      FlgCAS = '1'
056600170210     C                   EVAL      VABVCA = 'EUR'
056700170208     C                   Z-ADD     PiVal         VABCAS
056800170208     C                   ELSE
056900170208     C  N51              SETON                                        32
057000170208     C  N51              Z-ADD     *zeros        VABCAS
057100170208     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057200170208     C                             + ' ' + 'VABCAS'
057300170208     C                   ENDIF
057400170210     C                   ENDIF
057500100729     C*
057600100729     C* Considerazioni finali su CBO/CAS
057700100729     C                   IF        FlgCAS = '1'
057800100729     C                   IF        VABCBO = '1'
057900100729     C                   EVAL      VABCBO = '4'
058000100729     C                   ENDIF
058100100729     C                   IF        VABCBO = '2'
058200100729     C                   EVAL      VABCBO = '6'
058300100729     C                   ENDIF
058400100729     C                   ENDIF
058500010202     C*
058600000801     C* Ebbene...
058700000801     C                   ADD       1             §CTRMO
058800070530     C                   IF        *in31 <> *zeros OR
058900070530     C                             *in32 <> *zeros
059000000801     C                   ADD       1             §CTRNO
059100020725     C                   EVAL      x_vinflg = '2'
059200000801     C                   ELSE
059300010201     C                   ADD       1             §CTROKVB
059400000801     C                   ENDIF
059500000801     C*
059600000801     C                   ENDSR
059700100127     C*----------------------------------------------------*
059800130315     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
059900100127     C*----------------------------------------------------*
060000100127     C     WRIVAT_A      BEGSR
060100100127     C*
060200130315     C* Valorizzo l buffer di scrittura del EDIVAT
060300100127     C* - TIPO RECORD "A"
060400130315     C*
060500130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
060600130315     C***                EVAL      VATCMR = wCMR
060700130315     C***                EVAL      VATCNT = 1
060800130315     C*
060900161024     C***                eval      VATTRC = 'A'
061000101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
061100101119     C***                exsr      wrivat
061200100127     C*
061300100127     C                   ENDSR
061400010201     C*----------------------------------------------------*
061500130315     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
061600010201     C*----------------------------------------------------*
061700071003     C     WRIVAT_B      BEGSR
061800010201     C*
061900130315     C* Valorizzo l buffer di scrittura del EDIVAT
062000070928     C* - TIPO RECORD "B"
062100130315     C*
062200130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
062300161024     C                   EVAL      VATCMR = wCMR
062400161024     C                   EVAL      VATCNT = 1
062500130315     C*
062600161024     C                   eval      VATTRC = 'B'
062700161024     C                   eval      VATNOT = %trim(%subst(vindta:391:35))
062800161024     C                   exsr      wrivat
062900010201     C*
063000010201     C                   ENDSR
063100071003     C*----------------------------------------------------*
063200130315     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
063300071003     C*----------------------------------------------------*
063400071003     C     WRIVAT_E      BEGSR
063500071003     C*
063600130315     C* Valorizzo l buffer di scrittura del EDIVAT
063700071003     C* - TIPO RECORD "E"
063800130315     C*
063900130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064000161024     C                   EVAL      VATCMR = wCMR
064100161024     C                   EVAL      VATCNT = 1
064200161024     C*
064300161024     C                   eval      VATTRC = 'E'
064400161024     C                   eval      VATNOT = %trim(%subst(vindta:596:28))
064500161024     C                   exsr      wrivat
064600071003     C*
064700071003     C                   ENDSR
064800100127     C*----------------------------------------------------*
064900130315     C*  SCARICO BUFFER EDIVAT
065000100127     C*----------------------------------------------------*
065100100127     C     WRIVAT        BEGSR
065200100127     C*
065300100127     C                   if        vatNOT <> *blanks
065400100127     C                   ADD       1             §CTROKVT
065500130315     C                   write     EDIVAT00
065600100127     C                   endif
065700100127     C*
065800100127     C                   ENDSR
065900020904
066000020904
066100130315     C*----------------------------------------------------*
066200130315     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
066300130315     C*----------------------------------------------------*
066400130315     C     PREVAT        BEGSR
066500130315     C*
066600130315     C* Compongo il nome del membro da dare al EDIVATWR
066700130315     C                   eval      parmbr = vlrhdl
066800130315     C                   movel     'M'           parmbr
066900130315     C                   eval      parccm = %subst(vlrKSC:2:7)
067000130315     C                   eval      paropz = '1'
067100130315     C* Effettuo la chiamata al CLLE preposto
067200130315     C  N51              call(e)   'TITVEVTC'
067300130315     C                   parm                    parccm
067400130315     C                   parm                    parmbr
067500130315     C                   parm                    paropz
067600130315     C   51              call(e)   'TITVEVBC'
067700130315     C                   parm                    parccm
067800130315     C                   parm                    parmbr
067900130315     C                   parm                    paropz
068000130315     C*
068100130315     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
068200130315     C                   if        %error
068300130315     C                   movel     '1'           chkcall
068400130315     C                   else
068500130315     C                   movel     '0'           chkcall
068600130315     C                   endif
068700130315     C*
068800130315     C                   ENDSR
068900000801     C*----------------------------------------------------*
069000000801     C*  CONTROLLO NUMERICITA' CAMPI
069100000801     C*----------------------------------------------------*
069200000801     C     CHKNUM        BEGSR
069300081113     C*
069400081113     C                   IF        PiDecChr = *blanks
069500170210     C                   EVAL      PiDecChr = ','
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
071700130315     C* 1° invio EDIVAT
071800010201     C                   reset                   dscmz
071900110411     C                   move      vatfgs        cmzdst
072000130315     C                   eval      cmzfld = 'EDIVATWR'
072100010201     C                   eval      cmzmbd = vlrhdl
072200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072300021009     C***                if        prmfir = *blanks
072400130315     C                   eval      cmzfla = 'EDIVAT0F'
072500130315     C                   eval      cmzmba = 'EDIVAT0F'
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
074200130315     C* 2° invio EDIVAB
074300100525     C                   if        *in51 = *off
074400010201     C                   reset                   dscmz
074500110411     C                   move      vabfgs        cmzdst
074600010201     C                   eval      cmzfld = vlrfou
074700010201     C                   eval      cmzmbd = vlrhdl
074800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074900021009     C***                if        prmfir = *blanks
075000130315     C                   eval      cmzfla = 'EDIVAB0F'
075100130315     C                   eval      cmzmba = 'EDIVAB0F'
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
077800130315     C                   if        %open(edivabwr)
077900130315     C                   close     edivabwr
078000130315     C                   endif
078100130315     C                   if        %open(edivatwr)
078200130315     C                   close     edivatwr
078300130315     C                   endif
078400110627     C*
078500110627     C* Effettuo la chiamata al CLLE preposto
078600130315     C  N51              call(e)   'TITVEVTC'
078700110627     C                   parm                    parccm
078800110627     C                   parm                    parmbr
078900110627     C                   parm      '2'           paropz
079000130315     C   51              call(e)   'TITVEVBC'
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
081400130315     C                   time                    wn14             14 0
081500130315     C                   movel     wn14          oracor            6 0          *ORA
081600000830     C*
081700000613     C                   ENDSR
081800000613     C***
