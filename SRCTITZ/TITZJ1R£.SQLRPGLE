000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400150527      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800120709     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000900120706     FEDIVATwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070730     D tisi95ds      e ds
001900120706     D fivabds       e ds                  extname(EDIVAB0f)
002000120706     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
002100120706     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700160701     D depspe          s             11    INZ(*blanks)
002800160701     D curspe          s             11    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500120903     D wDivPKB         s                   like(VABNCL)
003600150527     D w70             s             70
003700000830
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300101007     D  VINDTA                     2048
004400101007     D  VINFLG                        1
004500160701     D  VINSPE                       11
004600101007     D  VINRRN                        8  0
004700020725     D*
004800080923     D*------------------
004900080923     D* DS REPERIMENTO NUMERATORE
005000080923     D*------------------
005100080923     D trul33ds      e ds                  inz
005200150528     D*------------------
005300150528     D TRUL28DSX     e ds                  inz
005400080923     D*------------------
005500080923     D* DS ARCHITETTURA
005600080923     D*------------------
005700080923     D kpjba         e ds                  inz
005800080923     D*------------------
005900121113     D  Num7_0         s              7  0
006000081113
006100081113     D*------------------
006200081113     D* LINKING A DEFINIZIONI ESTERNE
006300081113     D*------------------
006400100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
008100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008200000613     C*
008300010202     C* Effettuo la chiamata al CLLE preposto
008400120706     C                   call(e)   'TITVEVTC'
008500010202     C                   parm                    parccm
008600010202     C                   parm                    parmbr
008700010202     C                   parm      '2'           paropz
008800070730     C*
008900070730     C* Effettuo lancio TISI95 solo x chiusura
009000070730     C                   CLEAR                   TISI95DS
009100070730     C                   EVAL      I95TLA = 'C'
009200070730     C                   CALL      'TISI95R'
009300070730     C                   PARM                    TISI95DS
009400000616     C*
009500000801     C
009600010201     C                   seton                                        LR
009700990908
009800000801
009900910830     C*--------------------------------------------------------
010000120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
010100910830     C*--------------------------------------------------------
010200070530     C     RWFILE        BEGSR
010300990910     C*
010400990914     C                   if        not %open(tivin00r)
010500990908     C                   open      tivin00r
010600990914     C                   endif
010700120706     C                   if        not %open(edivabwr)
010800120706     C                   open      edivabwr
010900990914     C                   endif
011000070530     C*
011100120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011200020305     C                   exsr      prevat
011300010201     C*
011400010202     C                   if        chkcall = '0'
011500010202     C*
011600120706     C                   if        not %open(edivatwr)
011700120706     C                   open      edivatwr
011800010201     C                   endif
011900080117     C*
012000080117     C                   EXSR      INZVAR
012100080117     C                   EXSR      DEFCAM
012200990910     C*
012300010201     C                   clear                   §CTROKVB          5 0
012400020305     C                   clear                   §CTROKVT          5 0
012500000801     C                   clear                   §CTRMO            5 0
012600000801     C                   clear                   §CTRNO            5 0
012700990910     C*
012800020725     C/EXEC SQL
012900020725     C+ declare C1 cursor for select
013000160701     C+ vindta, vinflg, substr(vindta, 79, 11) as sped, rrn(tivin00r)
013100060223     C+ from tivin00r
013200060223     C+ order by sped
013300020725     C+ for read only
013400020725     C/END-EXEC
013500020725     C
013600020725     C/EXEC SQL
013700020725     C+ open C1
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100070530     C+ Fetch C1 into :INPUT_DS
014200020725     C/END-EXEC
014300020725     C*
014400020725     C                   dow       sqlcod = *zeros
014500990913     C*
014600020725     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800000801     C*
014900020725     C                   if        vinflg = *blanks
015000020725     C                             or vinflg = '0'
015100020725     C                             or vinflg = '2'
015200000801     C*
015300020725     C                   clear                   x_vinmsg
015400020725     C                   eval      x_vinflg = '1'
015500010305     C*
015600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015700160701     C                   EVAL      PiStr=%trim(%subst(vindta:79:11))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200160701     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione, in questo caso
016300160701     C* si è deciso di farlo
016400160701     C                   exsr      repNSP
016500071003     C                   exsr      impvab
016600111130     C  N31              eval      fivabds_ok = fivabds
016700111130     C  N31              exsr      wrivab
016800160701     C*                  exsr      wrivat_b                                     => carico VAT
016900160701     C*                  exsr      wrivat_i                                     => carico VAT
017000160701     C*                  exsr      wrivat_s                                     => carico VAT
017100111125     C                   exsr      inzvar
017200071003     C                   else
017300080923     C*
017400071009     C                   if        wDISK = 'D'
017500150527     C                   exsr      repNSP
017600071009     C                   exsr      impvab
017700111130     C  N31              eval      fivabds_ok = fivabds
017800111130     C  N31              exsr      wrivab
017900160701     C*                  exsr      wrivat_b                                     => carico VAT
018000160701     C*                  exsr      wrivat_i                                     => carico VAT
018100160701     C*                  exsr      wrivat_s                                     => carico VAT
018200160701     C*                  exsr      wrivat_e                                     => carico VAT
018300111125     C                   exsr      inzvar
018400071009     C                   else
018500071009     C*
018600010305     C                   if        depspe = *blanks                             => 1° giro
018700010305     C                   eval      depspe = curspe                              => memorizz. spediz
018800080117     C                   clear                   tivinds
018900150527     C                   exsr      repNSP
019000020305     C                   exsr      impvab
019100111130     C                   z-add     VABNCL        sav_VABNCL
019200111130     C                   z-add     VABPKB        sav_VABPKB
019300111130     C                   z-add     VABVLB        sav_VABVLB
019400160701     C*                  exsr      wrivat_b                                     => carico VAT
019500160701     C*                  exsr      wrivat_i                                     => carico VAT
019600160701     C*                  exsr      wrivat_s                                     => carico VAT
019700160701     C*  50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   else
019900020725     C                   if        depspe <> curspe                             => rottura di spediz
020000111130     C                   z-add     sav_VABNCL    VABNCL
020100111130     C                   z-add     sav_VABPKB    VABPKB
020200111130     C                   z-add     sav_VABVLB    VABVLB
020300111130     C  N31              eval      fivabds_ok = fivabds
020400111125     C  N31              exsr      wrivab
020500111125     C                   exsr      inzvar
020600111125     C                   eval      depspe = curspe                              => memorizz. spediz
020700080117     C                   clear                   tivinds
020800150527     C                   exsr      repNSP
020900020305     C                   exsr      impvab
021000111130     C                   if        wCntRecSpe = *zeros
021100111130     C                   z-add     VABNCL        sav_VABNCL
021200111130     C                   z-add     VABPKB        sav_VABPKB
021300111130     C                   z-add     VABVLB        sav_VABVLB
021400111130     C                   endif
021500111130     C                   add       1             wCntRecSpe
021600160701     C*                  exsr      wrivat_b                                     => carico VAT
021700160701     C*                  exsr      wrivat_i                                     => carico VAT
021800160701     C*                  exsr      wrivat_s                                     => carico VAT
021900160701     C*  50              exsr      wrivat_e                                     => carico VAT
022000020305     C                   else                                                   => x stessa spediz
022100111130     C                   exsr      impvab
022200120706     C   51              add       VABNCL        sav_VABNCL
022300111125     C   51              add       VABPKB        sav_VABPKB
022400111125     C   51              add       VABVLB        sav_VABVLB
022500111125     C   52              z-add     VABNCL        sav_VABNCL
022600111125     C   52              z-add     VABPKB        sav_VABPKB
022700111125     C   52              z-add     VABVLB        sav_VABVLB
022800111125     C   53              add       VABNCL        sav_VABNCL
022900111125     C   53              z-add     VABPKB        sav_VABPKB
023000111125     C   53              z-add     VABVLB        sav_VABVLB
023100111125     C*
023200160701     C*                  exsr      wrivat_b                                     => carico VAT
023300160701     C*                  exsr      wrivat_i                                     => carico VAT
023400160701     C*                  exsr      wrivat_s                                     => carico VAT
023500160701     C*  50              exsr      wrivat_e                                     => carico VAT
023600010305     C                   endif
023700010305     C                   endif
023800010305     C                   endif
023900071003     C                   endif
024000071009     C                   endif
024100000905     C*
024200000905     C                   else
024300020725     C                   eval      x_vinflg = '1'
024400000905     C                   endif
024500000905     C*
024600020725     C     VINRRN        chain     tivin000
024700020725     C                   if        %found(tivin00r)
024800020725     C                   eval      y_vinflg = x_vinflg
024900020725     C                   eval      y_vinmsg = x_vinmsg
025000020725     C                   update    tivin000
025100020725     C                   endif
025200020725     C*
025300020725     C/EXEC SQL
025400070530     C+ Fetch C1 into :INPUT_DS
025500020725     C/END-EXEC
025600020725     C*
025700020725     C                   enddo
025800020725     C*
025900020725     C/EXEC SQL
026000020725     C+ close C1
026100020725     C/END-EXEC
026200000905     C*
026300020305     C* Scarico i VAB rimasti "in sospeso"
026400071009     C                   if        wDISK = 'C'
026500111130     C  N31              eval      fivabds_ok = fivabds
026600111130     C  N31              exsr      wrivab
026700071009     C                   endif
026800010202     C*
026900010202     C                   endif
027000990910
027100990910     C* Se non ci sono record con errori ...
027200000710     C                   if        §ctrno = 0
027300990910     C* ... restituisco esito OK.
027400990921     C                   eval      wrkesito = '0'
027500990910     C                   else
027600010201     C                   if        §ctrokvb > 0
027700990921     C                   eval      wrkesito = '1'
027800000710     C                   else
027900000710     C                   eval      wrkesito = '2'
028000990910     C                   endif
028100000710     C                   endif
028200990910     C*
028300990914     C                   if        %open(tivin00r)
028400990908     C                   close     tivin00r
028500990914     C                   endif
028600120706     C                   if        %open(edivabwr)
028700120706     C                   close     edivabwr
028800120706     C                   endif
028900120706     C                   if        %open(edivatwr)
029000120706     C                   close     edivatwr
029100010201     C                   endif
029200990910     C*
029300111031     C                   if        §ctrokvb > 0 or
029400111031     C                             §ctrokvt > 0
029500111031     C                             and wPOI <> *zeros
029600010202     C                   exsr      invio
029700990920     C                   endif
029800990920     C*
029900910830     C                   ENDSR
030000000613     C***
030100010305
030200010305     C*----------------------------------------------------*
030300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030400010305     C*----------------------------------------------------*
030500020305     C     WRIVAB        BEGSR
030600070730     C*
030700071003     C* Considerazioni finali
030800111125     C                   if        ok_VABRMA = *blanks
030900111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031000071003     C                   endif
031100120706     C*
031200120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
031300160701     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
031400120709     C                   EVAL      ok_VABDCM = datcor
031500120709     C                   EVAL      ok_VABDTS = datcor
031600120709     C                   EVAL      ok_VABHMS = oracor
031700120709     C                   EVAL      ok_VABCNT = 1
031800071003     C*
031900120706     C                   write     edivab00                                     => scarico il VAB
032000010305     C*
032100010305     C                   ENDSR
032200990920
032300000801     C*----------------------------------------------------*
032400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032500000801     C*----------------------------------------------------*
032600010201     C     INZVAR        BEGSR
032700000801     C*
032800010201     C                   Z-ADD     *zeros        Num5_0
032900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033000020725     C                   MOVEL     '0'           FlgCAS            1
033100111125     C*
033200111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033300111130     C*
033400111125     C                   CLEAR                   FIVABDS
033500111130     C                   CLEAR                   FIVABDS_OK
033600111130     C                   CLEAR                   FIVABDS_SAV
033700120706     C                   CLEAR                   EDIVAB00
033800120706     C                   CLEAR                   EDIVAT00
033900000801     C*
034000000801     C                   ENDSR
034100000801     C*----------------------------------------------------*
034200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034300000801     C*----------------------------------------------------*
034400020904     C     DEFCAM        BEGSR
034500080923     C*
034600111125     C                   SETOFF                                       50
034700111125     C                   SETOFF                                       515253
034800070730     C* Imposto i valori di default...
034900160701     C                   EVAL      VABCCM = 0437342
035000160701     C                   EVAL      VATCCM = 0437342
035100160701     C                   EVAL      VABLNP = 043
035200160701     C                   EVAL      VATLNP = 043
035300160701     C                   EVAL      VABCTR = 000
035400160701     C                   EVAL      VABCBO = '1'
035500070222     C* ... e poi verifico se sono stati passati come parametri
035600070222     C                   IF        vlrppt > *blanks
035700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035900070222     C                   EXSR      CHKNUM
036000070222     C                   IF        PiInt=*on
036100070222     C                   Z-ADD     PiVal         VABCCM
036200070222     C                   Z-ADD     PiVal         VATCCM
036300070222     C                   ENDIF
036400070222     C                   ENDIF
036500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036700070222     C                   EXSR      CHKNUM
036800070222     C                   IF        PiInt=*on
036900070222     C                   Z-ADD     PiVal         VABLNP
037000070222     C                   Z-ADD     PiVal         VATLNP
037100070222     C                   ENDIF
037200070222     C                   ENDIF
037300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037500070222     C                   EXSR      CHKNUM
037600070222     C                   IF        PiInt=*on
037700070222     C                   Z-ADD     PiVal         VABCTR
037800070222     C                   ENDIF
037900070928     C                   ENDIF
038000071009     C                   MOVEL     *blanks       wDISK             1
038100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038200071009     C                   IF        wDISK <> *blanks
038300070928     C                   SETON                                        50
038400070222     C                   ENDIF
038500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038600111125     C                   SETON                                        41
038700080923     C                   ENDIF
038800070222     C                   ENDIF
038900120706     C*
039000120706     C* valido per nr.colli/peso/volume
039100120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039200120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039300120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039400120706     C                   SELECT
039500120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039600120706     C                   SETON                                        51
039700120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039800120706     C                   SETON                                        52
039900120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040000120706     C                   SETON                                        53
040100120706     C                   ENDSL
040200120706     C*
040300071009     C   50              EVAL      VABCTM = '7Q'
040400000801     C*
040500000801     C                   ENDSR
040600150527     C*----------------------------------------------------*
040700150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040800150527     C*----------------------------------------------------*
040900150527     C     REPNSP        BEGSR
041000150527     C*
041100150527     C                   EXSR      INZVAR
041200150527     C                   EXSR      DEFCAM
041300150527     C*
041400150527     C                   SETON                                        60
041500150527     C*
041600150527     C* NSP => Stacco un numeratore da AZNUM
041700150527     C                   clear                   TRUL33DS
041800150527     C                   eval      I33OPE = *zeros
041900150527     C                   eval      I33CNU = 302
042000150527     C                   eval      I33NUM = 1
042100150527     C                   movel     TRUL33DS      KPJBU
042200150527     C                   call      'TRUL33R'
042300150527     C                   parm                    KPJBA
042400150527     C                   movel     KPJBU         TRUL33DS
042500150527     C                   if        O33ERR = *zeros
042600150527     C                   z-add     O33NRF        VABNSP
042700150527     C                   z-add     O33NRF        VATNSP
042800150527     C                   else
042900150527     C                   SETON                                        31
043000150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100150527     C                             + ' ' + 'VABNSP VATNSP'
043200150527     C                   endif
043300150527     C*
043400150527     C                   ENDSR
043500000801     C*----------------------------------------------------*
043600120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
043700000801     C*----------------------------------------------------*
043800010201     C     IMPVAB        BEGSR
043900070530     C*
044000070530     C                   SETOFF                                       3132
044100070928     C*
044200070928     C                   EXSR      DEFCAM
044300070928     C*
044400111031     C                   IF        vlrpoi <> 999
044500070928     C                   MOVE(P)   vlrpoi        VABFGS
044600070928     C                   MOVE(P)   vlrpoi        VATFGS
044700111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044800111031     C                   ENDIF
044900070928     C*
045000070928     C                   MOVEL     datcor        VABAAS
045100070928     C                   MOVEL     datcor        VATAAS
045200070928     C                   MOVE      datcor        VABMGS
045300160701     C                   SELECT
045400160701     C                   WHEN      %subst(vindta:2:1) = '5'  or
045500160701     C                             %subst(vindta:2:1) = '6'  or
045600160701     C                             %subst(vindta:2:1) = '7'
045700160701     C                   EVAL      VABCBO = '2'
045800160701     C                   ENDSL
045900160701     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
046000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
046200070928     C* ==
046300160701     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
046400160701     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
046500160701     C                   EVAL      VABNOT=%trim(%subst(vindta:491:40))
046600160701     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
046700160701     C                   EVAL      VABNAS=%trim(%subst(vindta:334:3))
046800160701     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
046900160701     C                   SELECT
047000160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'LUN'
047100160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '1'
047200160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'MAR'
047300160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '2'
047400160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'MER'
047500160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '3'
047600160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'GIO'
047700160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '4'
047800160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'VEN'
047900160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '5'
048000160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'SAB'
048100160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '6'
048200160701     C                   WHEN      %trim(%subst(vindta:478:3)) = 'DOM'
048300160701     C                   EVAL      %subst(VABGC1 : 1 :1) = '7'
048400160701     C                   ENDSL
048500160701     C                   SELECT
048600160701     C                   WHEN      %trim(%subst(vindta:481:1)) = '1'
048700160701     C                   EVAL      %subst(VABGC1 : 2 :1) = 'M'
048800160701     C                   WHEN      %trim(%subst(vindta:481:1)) = '2'
048900160701     C                   EVAL      %subst(VABGC1 : 2 :1) = 'P'
049000160701     C                   WHEN      %trim(%subst(vindta:481:1)) = '3'
049100160701     C                   EVAL      %subst(VABGC1 : 2 :1) = *blank
049200160701     C                   ENDSL
049300160701     C                   SELECT
049400160701     C                   WHEN      %trim(%subst(vindta:490:1)) = '1'
049500160701     C                   EVAL      VABTCR = *blank
049600160701     C                   WHEN      %trim(%subst(vindta:490:1)) = '3'
049700160701     C                   EVAL      VABTCR = 'P'
049800160701     C                   WHEN      %trim(%subst(vindta:490:1)) = '4'
049900160701     C                   EVAL      VABTCR = 'D'
050000160701     C                   ENDSL
050100160701     C****               EVAL      VABCMR =%trim(%subst(vindta:337:15))
050200160701
050300071003     C* CAD
050400160701     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
050500070928     C                   EXSR      CHKNUM
050600070928     C                   IF        PiInt=*on
050700070928     C                   Z-ADD     PiVal         Num5_0
050800070928     C                   MOVEL     Num5_0        VABCAD
050900070928     C                   ELSE
051000070928     C                   SETON                                        32
051100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200070928     C                             + ' ' + 'VABCAD'
051300070928     C                   ENDIF
051400121113     C*
051500121113     C* Se provincia nn valorizzata la reperisco
051600121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
051700121113     C                   IF        VABPRD = *blanks
051800121113     C                   CLEAR                   TISI95DS
051900121113     C                   EVAL      I95TCN = '3'
052000121113     C                   Z-ADD     datcor        I95DAT
052100121113     C                   EVAL      I95NAR = VABNZD
052200121113     C                   EVAL      I95CAP = VABCAD
052300121113     C                   EVAL      I95LOC = VABLOD
052400121113     C                   CALL      'TISI95R'
052500121113     C                   PARM                    TISI95DS
052600121113     C                   EVAL      VABPRD = O95PRV
052700121113     C                   ENDIF
052800121113     C* RMN
052900160701     C                   EVAL      PiStr=%trim(%subst(vindta:79:11))
053000121113     C                   EXSR      CHKNUM
053100121113     C                   IF        PiInt=*on
053200121113     C                   Z-ADD     PiVal         VABRMN
053300121113     C                   ELSE
053400121113     C                   SETON                                        32
053500121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053600121113     C                             + ' ' + 'VABRMN'
053700121113     C                   ENDIF
053800121113     C* NCL
053900160701     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
054000121113     C                   EXSR      CHKNUM
054100121113     C                   IF        PiInt=*on
054200121113     C                   Z-ADD     PiVal         VABNCL
054300121113     C                   ELSE
054400121113     C                   SETON                                        32
054500121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600121113     C                             + ' ' + 'VABNCL'
054700121113     C                   ENDIF
054800121113     C* PKB
054900160701     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
055000121113     C                   EXSR      CHKNUM
055100121113     C                   IF        PiNum=*on
055200160701     C                   EVAL      VABPKB=PiVal/100
055300121113     C                   ELSE
055400121113     C                   SETON                                        32
055500121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055600121113     C                             + ' ' + 'VABPKB'
055700121113     C                   ENDIF
055800160701     C* VLB
055900160701     C                   EVAL      PiStr=%trim(%subst(vindta:369:5))
056000160701     C                   EXSR      CHKNUM
056100160701     C                   IF        PiNum=*on
056200160701     C                   EVAL      VABVLB=PiVal/100
056300160701     C                   ELSE
056400160701     C                   SETON                                        32
056500160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056600160701     C                             + ' ' + 'VABVLB'
056700160701     C                   ENDIF
056800121113     C* CAS
056900160701     C                   IF        %subst(vindta:388:13) <> *blanks and
057000160701     C                             %subst(vindta:388:13) <> *zeros
057100121113     C                   MOVEL     '1'           FlgCAS
057200160701     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
057300160701     C                   IF        PiStr=%trim(%subst(vindta:401:3))
057400150527     C                   EVAL      VABVCA='EUR'
057500160701     C                   ELSE
057600160701     C                   EVAL      VABVCA=%trim(%subst(vindta:401:3))
057700160701     C                   ENDIF
057800121113     C                   EXSR      CHKNUM
057900121113     C                   IF        PiNum=*on
058000121113     C                   Z-ADD     PiVal         VABCAS
058100121113     C                   ELSE
058200121113     C                   SETON                                        32
058300121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058400121113     C                             + ' ' + 'VABCAS'
058500121113     C                   ENDIF
058600121113     C                   ENDIF
058700160701     C* DCR
058800160701     C                   IF        %subst(vindta:482:8) <> *blanks AND
058900160701     C                             %subst(vindta:482:8) <> *all'0'
059000160701     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
059100160701     C                   EXSR      CHKNUM
059200160701     C                   IF        PiInt=*on
059300160701     C                   Z-ADD     PiVal         VABDCR
059400160701     C                   ELSE
059500160701     C                   SETON                                        32
059600160701     C                   Z-ADD     *zeros        VABDCR
059700160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059800160701     C                             + ' ' + 'VABDCR'
059900160701     C                   ENDIF
060000160701     C                   ENDIF
060100160701     C* VMD
060200160701     C                   IF        %subst(vindta:558:13) <> *blanks and
060300160701     C                             %subst(vindta:558:13) <> *zeros
060400160701     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
060500160701     C                   IF        PiStr=%trim(%subst(vindta:571:3))
060600160701     C                   EVAL      VABVAD='EUR'
060700160701     C                   ELSE
060800160701     C                   EVAL      VABVAD=%trim(%subst(vindta:571:3))
060900160701     C                   ENDIF
061000160701     C                   EXSR      CHKNUM
061100160701     C                   IF        PiNum=*on
061200160701     C                   Z-ADD     PiVal         VABVMD
061300160701     C                   ELSE
061400160701     C                   SETON                                        32
061500160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061600160701     C                             + ' ' + 'VABVMD'
061700160701     C                   ENDIF
061800160701     C                   ENDIF
061900070730     C*
062000070730     C* Considerazioni finali su CBO/CAS
062100121113     C                   IF        FlgCAS <> '0'
062200070730     C                   IF        VABCBO = '1'
062300070730     C                   EVAL      VABCBO = '4'
062400070730     C                   ENDIF
062500070730     C                   IF        VABCBO = '2'
062600070730     C                   EVAL      VABCBO = '6'
062700070730     C                   ENDIF
062800070730     C                   ENDIF
062900020305     C*
063000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
063100011113     C                   EXSR      CHKIMPDIV
063200010202     C*
063300000801     C* Ebbene...
063400000801     C                   ADD       1             §CTRMO
063500070530     C                   IF        *in31 <> *zeros OR
063600070530     C                             *in32 <> *zeros
063700000801     C                   ADD       1             §CTRNO
063800020725     C                   EVAL      x_vinflg = '2'
063900000801     C                   ELSE
064000010201     C                   ADD       1             §CTROKVB
064100000801     C                   ENDIF
064200000801     C*
064300000801     C                   ENDSR
064400010201     C*----------------------------------------------------*
064500120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
064600010201     C*----------------------------------------------------*
064700071003     C     WRIVAT_B      BEGSR
064800090204     C*
064900090204     C                   eval      vatAAS = vabAAS
065000090204     C                   eval      vatLNP = vabLNP
065100090204     C                   eval      vatNRS = vabNRS
065200090204     C                   eval      vatNSP = vabNSP
065300010201     C*
065400120706     C* Valorizzo il buffer di scrittura del EDIVAT
065500070928     C* - TIPO RECORD "B"
065600150527     C                   if        %subst(vindta:208:12) <> *blanks
065700120706     C*
065800120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
065900160701     C                   EVAL      VATCMR = vabcmr
066000120706     C                   EVAL      VATCNT = 1
066100120706     C*
066200070928     C                   eval      VATTRC = 'B'
066300150527     C                   eval      VATNOT = %trim(%subst(vindta:208:12))
066400150527     C                   write     EDIVAT00
066500111031     C                   add       1             §ctrokvt
066600150527     C                   endif
066700010201     C*
066800010201     C                   ENDSR
066900150527     C*----------------------------------------------------*
067000150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "S"
067100150527     C*----------------------------------------------------*
067200150527     C     WRIVAT_S      BEGSR
067300150527     C*
067400150527     C                   eval      vatAAS = vabAAS
067500150527     C                   eval      vatLNP = vabLNP
067600150527     C                   eval      vatNRS = vabNRS
067700150527     C                   eval      vatNSP = vabNSP
067800150527     C*
067900150527     C* Valorizzo il buffer di scrittura del EDIVAT
068000150527     C* - TIPO RECORD "S"
068100150527     C                   if        %subst(vindta:491:50) <> *blanks
068200150527     C*
068300150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
068400160701     C                   EVAL      VATCMR = vabcmr
068500150527     C                   EVAL      VATCNT = 1
068600150527     C*
068700150527     C                   eval      VATTRC = 'S'
068800150527     C                   eval      VATNOT = %trim(%subst(vindta:491:50))
068900150527     C                   eval      %subst(VATNOT:17:2)=*blank
069000150527     C                   write     EDIVAT00
069100150527     C                   add       1             §ctrokvt
069200150527     C                   endif
069300150527     C*
069400150527     C                   ENDSR
069500150527     C*----------------------------------------------------*
069600150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
069700150527     C*----------------------------------------------------*
069800150527     C     WRIVAT_I      BEGSR
069900150527     C*
070000150527     C                   eval      vatAAS = vabAAS
070100150527     C                   eval      vatLNP = vabLNP
070200150527     C                   eval      vatNRS = vabNRS
070300150527     C                   eval      vatNSP = vabNSP
070400150527     C*
070500150527     C* Valorizzo il buffer di scrittura del EDIVAT
070600150527     C* - TIPO RECORD "I" e "J"
070700150527     C                   if        %subst(vindta:541:50) <> *blanks
070800150527     C                   eval      w70 = %trim(%subst(vindta:541:50))
070900150527     C*
071000150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
071100160701     C                   EVAL      VATCMR = vabcmr
071200150527     C                   EVAL      VATCNT = 1
071300150527     C*
071400150527     C                   eval      VATTRC = 'I'
071500150527     C                   eval      VATNOT = %subst(w70:1:35)
071600150527     C                   write     EDIVAT00
071700150527     C                   add       1             §ctrokvt
071800150527     C*
071900150527     C                   IF        %subst(w70:36:35) <> *blank
072000150527     C                   eval      VATTRC = 'J'
072100150527     C                   eval      VATNOT = %subst(w70:36:35)
072200150527     C                   write     EDIVAT00
072300150527     C                   add       1             §ctrokvt
072400150527     C                   endif
072500150527     C                   endif
072600150527     C*
072700150527     C                   ENDSR
072800071003     C*----------------------------------------------------*
072900120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
073000071003     C*----------------------------------------------------*
073100071003     C     WRIVAT_E      BEGSR
073200090204     C*
073300090204     C                   eval      vatAAS = vabAAS
073400090204     C                   eval      vatLNP = vabLNP
073500090204     C                   eval      vatNRS = vabNRS
073600090204     C                   eval      vatNSP = vabNSP
073700071003     C*
073800120706     C* Valorizzo il buffer di scrittura del EDIVAT
073900071003     C* - TIPO RECORD "E"
074000150527     C                   if        %subst(vindta:280:25) <> *blanks
074100120706     C*
074200120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
074300160701     C                   EVAL      VATCMR = vabcmr
074400120706     C                   EVAL      VATCNT = 1
074500120706     C*
074600071003     C                   eval      VATTRC = 'E'
074700150527     C                   eval      vatNOT = %trim(%subst(vindta:280:25))
074800150528     C* recupero il check digit
074900150528     C                   clear                   TRUL28DSX
075000150528     C                   eval      I284STD = 'E10'
075100150528     C                   eval      I284COD = VATNOT
075200150528     C                   call      'TRUL28R4'
075300150528     C                   parm                    TRUL28DSX
075400150528     C* Se tutto ok scrivo il "chi sono" corrente col check digit
075500150528     C                   IF        O284err = *blanks
075600150528     C                   EVAL      VATNOT = %trim(VATNOT)
075700150528     C                              + %trim(O284CkD)
075800120706     C                   write     EDIVAT00
075900111031     C                   add       1             §ctrokvt
076000150528     C                   ELSE
076100150528     C                   SETON                                        31
076200150528     C                   EVAL      VATNOT = *blank
076300150528     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
076400150528     C                             + ' ' + 'VATNOT'
076500071003     C                   endif
076600150528     C                   endif
076700071003     C*
076800071003     C                   ENDSR
076900020904
077000020904
077100010202     C*----------------------------------------------------*
077200120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
077300010202     C*----------------------------------------------------*
077400020305     C     PREVAT        BEGSR
077500010202     C*
077600120706     C* Compongo il nome del membro da dare al EDIVATWR
077700010202     C                   eval      parmbr = vlrhdl
077800010202     C                   movel     'M'           parmbr
077900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
078000010202     C                   eval      paropz = '1'
078100010202     C* Effettuo la chiamata al CLLE preposto
078200120706     C                   call(e)   'TITVEVTC'
078300010202     C                   parm                    parccm
078400010202     C                   parm                    parmbr
078500010202     C                   parm                    paropz
078600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
078700010202     C                   if        %error
078800010202     C                   movel     '1'           chkcall
078900010202     C                   else
079000010202     C                   movel     '0'           chkcall
079100010202     C                   endif
079200010202     C*
079300010202     C                   ENDSR
079400000801     C*----------------------------------------------------*
079500000801     C*  CONTROLLO NUMERICITA' CAMPI
079600000801     C*----------------------------------------------------*
079700000801     C     CHKNUM        BEGSR
079800081113     C*
079900081113     C                   IF        PiDecChr = *blanks
080000121113     C                   EVAL      PiDecChr = '.'
080100081113     C                   ENDIF
080200081113     C*
080300081113     C                   callp(e)  UBISNUM_Check(PiStr
080400081113     C                                          :PiDecChr
080500081113     C                                          :PiVal
080600081113     C                                          :PiNum
080700081113     C                                          :PiInt)
080800081113     C*
080900000801     C                   IF        %error
081000000801     C                   EVAL      PiInt=*off
081100000801     C                   ENDIF
081200000801     C*
081300000801     C                   ENDSR
081400000801     C***
081500000801
081600011113
081700011113     C*----------------------------------------------------*
081800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
081900011113     C*----------------------------------------------------*
082000011113     C     CHKIMPDIV     BEGSR
082100011113     C*
082200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
082300011113     C                   Z-ADD     *zeros        wrkDec            9 9
082400011113     C*
082500011113     C* Come prima cosa effettuo considerazioni sulla divisa
082600011113     C                   IF        vabIAS > *zeros
082700011113     C                   IF        vabVAS <> 'EUR'
082800011113     C                   EVAL      vabVAS =  'ITL'
082900011113     C                   ENDIF
083000011113     C                   ENDIF
083100011113     C*
083200011113     C                   IF        vabCAS > *zeros
083300011113     C                   IF        vabVCA <> 'EUR'
083400011113     C                   EVAL      vabVCA =  'ITL'
083500011113     C                   ENDIF
083600011113     C                   ENDIF
083700011113     C*
083800011113     C                   IF        vabVMD > *zeros
083900020305     C                   IF        vabVAD <> 'EUR'
084000011113     C                   EVAL      vabVAD =  'ITL'
084100011113     C                   ENDIF
084200011113     C                   ENDIF
084300011113     C*
084400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
084500011113     C                   Z-ADD     vabIAS        wrkDec
084600011113     C                   IF        wrkDec > *zeros
084700011113     C                   IF        vabVAS = 'ITL'
084800011113     C                   EVAL      vabIAS = *zeros
084900011113     C                   ENDIF
085000011113     C                   ENDIF
085100011113     C*
085200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
085300011113     C                   Z-ADD     vabCAS        wrkDec
085400011113     C                   IF        wrkDec > *zeros
085500011113     C                   IF        vabVCA = 'ITL'
085600011113     C                   EVAL      vabCAS = *zeros
085700011113     C                   ENDIF
085800011113     C                   ENDIF
085900011113     C*
086000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
086100011113     C                   Z-ADD     vabVMD        wrkDec
086200011113     C                   IF        wrkDec > *zeros
086300011113     C                   IF        vabVAD = 'ITL'
086400011113     C                   EVAL      vabVMD = *zeros
086500011113     C                   ENDIF
086600011113     C                   ENDIF
086700011113     C*
086800011113     C                   ENDSR
086900011113     C***
087000011113
087100011113
087200000801
087300000801
087400990920      /TITLE Invio dei dati al punto operativo.
087500010202     C     invio         BEGSR
087600990920     C*
087700120706     C* 1° invio EDIVAT
087800010201     C                   reset                   dscmz
087900111031     C                   move      wPOI          cmzdst
088000120706     C                   eval      cmzfld = 'EDIVATWR'
088100010201     C                   eval      cmzmbd = vlrhdl
088200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
088300021009     C***                if        prmfir = *blanks
088400120706     C                   eval      cmzfla = 'EDIVAT0F'
088500120706     C                   eval      cmzmba = 'EDIVAT0F'
088600021009     C***                else
088700021009     C***                eval      cmzfla = prmfir
088800021009     C***                eval      cmzmba = prmfir
088900021009     C***                endif
089000010201     C                   eval      cmznrr = *zeros
089100020305     C                   move      §ctrokvt      cmznrr
089200021018     C                   eval      cmzlba = vlrfl1
089300010201     C                   call(e)   'TIS711C'
089400010201     C                   parm                    dscmz
089500010201     C                   parm      *blanks       esito
089600010205     C                   if        %error
089700010205     C                             or cmzerr = '1'
089800010205     C                             or esito  = '1'
089900010205     C                   eval      wrkesito = '3'
090000010205     C                   else
090100010201     C*
090200120706     C* 2° invio EDIVAB
090300010201     C                   reset                   dscmz
090400111031     C                   move      wPOI          cmzdst
090500010201     C                   eval      cmzfld = vlrfou
090600010201     C                   eval      cmzmbd = vlrhdl
090700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
090800021009     C***                if        prmfir = *blanks
090900120712     C                   eval      cmzfla = 'EDIVAB0F'
091000120712     C                   eval      cmzmba = 'EDIVAB0F'
091100021009     C***                else
091200021009     C***                eval      cmzfla = prmfir
091300021009     C***                eval      cmzmba = prmfir
091400021009     C***                endif
091500010201     C                   eval      cmznrr = *zeros
091600010201     C                   move      §ctrokvb      cmznrr
091700021018     C                   eval      cmzlba = vlrfl1
091800010201     C                   call(e)   'TIS711C'
091900010201     C                   parm                    dscmz
092000010201     C                   parm      *blanks       esito
092100010201     C                   if        %error
092200010201     C                             or cmzerr = '1'
092300010201     C                             or esito  = '1'
092400010201     C                   eval      wrkesito = '3'
092500010201     C                   endif
092600010205     C                   endif
092700990920     C*
092800000613     C                   ENDSR
092900000613     C***
093000070411
093100070411     C     *pssr         BEGSR
093200070411     C*
093300070411     C                   if        %open(tivin00r)
093400070411     C                   close     tivin00r
093500070411     C                   endif
093600120706     C                   if        %open(edivabwr)
093700120706     C                   close     edivabwr
093800120706     C                   endif
093900120706     C                   if        %open(edivatwr)
094000120706     C                   close     edivatwr
094100070411     C                   endif
094200070411     C*
094300070411     C* Effettuo la chiamata al CLLE preposto
094400120706     C                   call(e)   'TITVEVTC'
094500070411     C                   parm                    parccm
094600070411     C                   parm                    parmbr
094700070411     C                   parm      '2'           paropz
094800070411     C*
094900070411     C                   eval      wrkesito = '2'
095000070411     C*
095100070411     C                   seton                                        LR
095200070411     C*
095300070411     C                   ENDSR     '*CANCL'
095400070411     C***
095500070411
095600990910
095700000613     C     *inzsr        BEGSR
095800990910     C*
095900990910     C     *entry        plist
096000990920     C                   parm                    tivlrds
096100990921     C                   parm      wrkesito      esito
096200000724     C                   parm                    prmlit
096300000710     C                   parm                    prmfir
096400000613     C*
096500000830     C* CALCOLA LA DATA CORRENTE
096600120706     C                   time                    wn14             14 0
096700120706     C                   movel     wn14          oracor            6 0          *ORA
096800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
096900100324     C                   eval      datcor = %dec(%date() : *ISO)
097000000830     C*
097100000613     C                   ENDSR
097200000613     C***
