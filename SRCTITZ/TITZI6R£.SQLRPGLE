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
002700150527     D depspe          s             15    INZ(*blanks)
002800150527     D curspe          s             15    INZ(*blanks)
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
004500150527     D  VINSPE                       15
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
013000150527     C+ vindta, vinflg, substr(vindta, 1, 15) as sped, rrn(tivin00r)
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
015700150527     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200071003     C                   exsr      impvab
016300111130     C  N31              eval      fivabds_ok = fivabds
016400111130     C  N31              exsr      wrivab
016500071003     C                   exsr      wrivat_b                                     => carico VAT
016600150527     C                   exsr      wrivat_i                                     => carico VAT
016700150527     C                   exsr      wrivat_s                                     => carico VAT
016800111125     C                   exsr      inzvar
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200150527     C                   exsr      repNSP
017300071009     C                   exsr      impvab
017400111130     C  N31              eval      fivabds_ok = fivabds
017500111130     C  N31              exsr      wrivab
017600071009     C                   exsr      wrivat_b                                     => carico VAT
017700150527     C                   exsr      wrivat_i                                     => carico VAT
017800150527     C                   exsr      wrivat_s                                     => carico VAT
017900071009     C                   exsr      wrivat_e                                     => carico VAT
018000111125     C                   exsr      inzvar
018100071009     C                   else
018200071009     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500080117     C                   clear                   tivinds
018600150527     C                   exsr      repNSP
018700020305     C                   exsr      impvab
018800111130     C                   z-add     VABNCL        sav_VABNCL
018900111130     C                   z-add     VABPKB        sav_VABPKB
019000111130     C                   z-add     VABVLB        sav_VABVLB
019100071003     C                   exsr      wrivat_b                                     => carico VAT
019200150527     C                   exsr      wrivat_i                                     => carico VAT
019300150527     C                   exsr      wrivat_s                                     => carico VAT
019400071003     C   50              exsr      wrivat_e                                     => carico VAT
019500010305     C                   else
019600020725     C                   if        depspe <> curspe                             => rottura di spediz
019700111130     C                   z-add     sav_VABNCL    VABNCL
019800111130     C                   z-add     sav_VABPKB    VABPKB
019900111130     C                   z-add     sav_VABVLB    VABVLB
020000111130     C  N31              eval      fivabds_ok = fivabds
020100111125     C  N31              exsr      wrivab
020200111125     C                   exsr      inzvar
020300111125     C                   eval      depspe = curspe                              => memorizz. spediz
020400080117     C                   clear                   tivinds
020500150527     C                   exsr      repNSP
020600020305     C                   exsr      impvab
020700111130     C                   if        wCntRecSpe = *zeros
020800111130     C                   z-add     VABNCL        sav_VABNCL
020900111130     C                   z-add     VABPKB        sav_VABPKB
021000111130     C                   z-add     VABVLB        sav_VABVLB
021100111130     C                   endif
021200111130     C                   add       1             wCntRecSpe
021300071003     C                   exsr      wrivat_b                                     => carico VAT
021400150527     C                   exsr      wrivat_i                                     => carico VAT
021500150527     C                   exsr      wrivat_s                                     => carico VAT
021600071003     C   50              exsr      wrivat_e                                     => carico VAT
021700020305     C                   else                                                   => x stessa spediz
021800111130     C                   exsr      impvab
021900120706     C   51              add       VABNCL        sav_VABNCL
022000111125     C   51              add       VABPKB        sav_VABPKB
022100111125     C   51              add       VABVLB        sav_VABVLB
022200111125     C   52              z-add     VABNCL        sav_VABNCL
022300111125     C   52              z-add     VABPKB        sav_VABPKB
022400111125     C   52              z-add     VABVLB        sav_VABVLB
022500111125     C   53              add       VABNCL        sav_VABNCL
022600111125     C   53              z-add     VABPKB        sav_VABPKB
022700111125     C   53              z-add     VABVLB        sav_VABVLB
022800111125     C*
022900071003     C                   exsr      wrivat_b                                     => carico VAT
023000150527     C                   exsr      wrivat_i                                     => carico VAT
023100150527     C                   exsr      wrivat_s                                     => carico VAT
023200071003     C   50              exsr      wrivat_e                                     => carico VAT
023300010305     C                   endif
023400010305     C                   endif
023500010305     C                   endif
023600071003     C                   endif
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
026200111130     C  N31              eval      fivabds_ok = fivabds
026300111130     C  N31              exsr      wrivab
026400071009     C                   endif
026500010202     C*
026600010202     C                   endif
026700990910
026800990910     C* Se non ci sono record con errori ...
026900000710     C                   if        §ctrno = 0
027000990910     C* ... restituisco esito OK.
027100990921     C                   eval      wrkesito = '0'
027200990910     C                   else
027300010201     C                   if        §ctrokvb > 0
027400990921     C                   eval      wrkesito = '1'
027500000710     C                   else
027600000710     C                   eval      wrkesito = '2'
027700990910     C                   endif
027800000710     C                   endif
027900990910     C*
028000990914     C                   if        %open(tivin00r)
028100990908     C                   close     tivin00r
028200990914     C                   endif
028300120706     C                   if        %open(edivabwr)
028400120706     C                   close     edivabwr
028500120706     C                   endif
028600120706     C                   if        %open(edivatwr)
028700120706     C                   close     edivatwr
028800010201     C                   endif
028900990910     C*
029000111031     C                   if        §ctrokvb > 0 or
029100111031     C                             §ctrokvt > 0
029200111031     C                             and wPOI <> *zeros
029300010202     C                   exsr      invio
029400990920     C                   endif
029500990920     C*
029600910830     C                   ENDSR
029700000613     C***
029800010305
029900010305     C*----------------------------------------------------*
030000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030100010305     C*----------------------------------------------------*
030200020305     C     WRIVAB        BEGSR
030300070730     C*
030400071003     C* Considerazioni finali
030500111125     C                   if        ok_VABRMA = *blanks
030600111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030700071003     C                   endif
030800120706     C*
030900120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
031000120709     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
031100120709     C                   EVAL      ok_VABDCM = datcor
031200120709     C                   EVAL      ok_VABDTS = datcor
031300120709     C                   EVAL      ok_VABHMS = oracor
031400120709     C                   EVAL      ok_VABCNT = 1
031500071003     C*
031600120706     C                   write     edivab00                                     => scarico il VAB
031700010305     C*
031800010305     C                   ENDSR
031900990920
032000000801     C*----------------------------------------------------*
032100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032200000801     C*----------------------------------------------------*
032300010201     C     INZVAR        BEGSR
032400000801     C*
032500010201     C                   Z-ADD     *zeros        Num5_0
032600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032700020725     C                   MOVEL     '0'           FlgCAS            1
032800111125     C*
032900111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033000111130     C*
033100111125     C                   CLEAR                   FIVABDS
033200111130     C                   CLEAR                   FIVABDS_OK
033300111130     C                   CLEAR                   FIVABDS_SAV
033400120706     C                   CLEAR                   EDIVAB00
033500120706     C                   CLEAR                   EDIVAT00
033600000801     C*
033700000801     C                   ENDSR
033800000801     C*----------------------------------------------------*
033900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034000000801     C*----------------------------------------------------*
034100020904     C     DEFCAM        BEGSR
034200080923     C*
034300111125     C                   SETOFF                                       50
034400111125     C                   SETOFF                                       515253
034500070730     C* Imposto i valori di default...
034600150527     C                   EVAL      VABCCM = 2221715
034700150527     C                   EVAL      VATCCM = 2221715
034800150527     C                   EVAL      VABLNP = 222
034900150527     C                   EVAL      VATLNP = 222
035000121113     C                   EVAL      VABCTR = 000
035100120903     C                   EVAL      VABCBO = '1'
035200150527     C                   EVAL      VABCTM = '7Q'
035300070222     C* ... e poi verifico se sono stati passati come parametri
035400070222     C                   IF        vlrppt > *blanks
035500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABCCM
036000070222     C                   Z-ADD     PiVal         VATCCM
036100070222     C                   ENDIF
036200070222     C                   ENDIF
036300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABLNP
036800070222     C                   Z-ADD     PiVal         VATLNP
036900070222     C                   ENDIF
037000070222     C                   ENDIF
037100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037300070222     C                   EXSR      CHKNUM
037400070222     C                   IF        PiInt=*on
037500070222     C                   Z-ADD     PiVal         VABCTR
037600070222     C                   ENDIF
037700070928     C                   ENDIF
037800071009     C                   MOVEL     *blanks       wDISK             1
037900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038000071009     C                   IF        wDISK <> *blanks
038100070928     C                   SETON                                        50
038200070222     C                   ENDIF
038300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038400111125     C                   SETON                                        41
038500080923     C                   ENDIF
038600070222     C                   ENDIF
038700120706     C*
038800120706     C* valido per nr.colli/peso/volume
038900120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039000120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039100120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039200120706     C                   SELECT
039300120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039400120706     C                   SETON                                        51
039500120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039600120706     C                   SETON                                        52
039700120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039800120706     C                   SETON                                        53
039900120706     C                   ENDSL
040000120706     C*
040100071009     C   50              EVAL      VABCTM = '7Q'
040200000801     C*
040300000801     C                   ENDSR
040400150527     C*----------------------------------------------------*
040500150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040600150527     C*----------------------------------------------------*
040700150527     C     REPNSP        BEGSR
040800150527     C*
040900150527     C                   EXSR      INZVAR
041000150527     C                   EXSR      DEFCAM
041100150527     C*
041200150527     C                   SETON                                        60
041300150527     C*
041400150527     C* NSP => Stacco un numeratore da AZNUM
041500150527     C                   clear                   TRUL33DS
041600150527     C                   eval      I33OPE = *zeros
041700150527     C                   eval      I33CNU = 302
041800150527     C                   eval      I33NUM = 1
041900150527     C                   movel     TRUL33DS      KPJBU
042000150527     C                   call      'TRUL33R'
042100150527     C                   parm                    KPJBA
042200150527     C                   movel     KPJBU         TRUL33DS
042300150527     C                   if        O33ERR = *zeros
042400150527     C                   z-add     O33NRF        VABNSP
042500150527     C                   z-add     O33NRF        VATNSP
042600150527     C                   else
042700150527     C                   SETON                                        31
042800150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042900150527     C                             + ' ' + 'VABNSP VATNSP'
043000150527     C                   endif
043100150527     C*
043200150527     C                   ENDSR
043300000801     C*----------------------------------------------------*
043400120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
043500000801     C*----------------------------------------------------*
043600010201     C     IMPVAB        BEGSR
043700070530     C*
043800070530     C                   SETOFF                                       3132
043900070928     C*
044000070928     C                   EXSR      DEFCAM
044100070928     C*
044200111031     C                   IF        vlrpoi <> 999
044300070928     C                   MOVE(P)   vlrpoi        VABFGS
044400070928     C                   MOVE(P)   vlrpoi        VATFGS
044500111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044600111031     C                   ENDIF
044700070928     C*
044800070928     C                   MOVEL     datcor        VABAAS
044900070928     C                   MOVEL     datcor        VATAAS
045000070928     C                   MOVE      datcor        VABMGS
045100150527     C                   EVAL      w70=%trim(%subst(vindta:115:40))
045200150527     C                   EVAL      VABRSD=%subst(w70:1:35)
045300150527     C                   EVAL      VABRD2=%subst(w70:36:35)
045400070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045500070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045600150527     C     '@':'A'       XLATE     VABRD2        VABRD2
045700070928     C* ==
045800150527     C                   EVAL      VABIND=%trim(%subst(vindta:155:50))
045900150527     C                   EVAL      VABLOD=%trim(%subst(vindta:220:50))
046000150527     C                   EVAL      VABTIC=%trim(%subst(vindta:357:2))
046100150527     C                   EVAL      w70=%trim(%subst(vindta:359:100))
046200150527     C                   EVAL      VABNOT=%subst(w70:1:35)
046300150527     C                   EVAL      VABNT2=%subst(w70:36:35)
046400150723     C                   EVAL      VABRMA=%trim(%subst(vindta:1:12))
046500071003     C* CAD
046600150527     C                   EVAL      PiStr=%trim(%subst(vindta:270:10))
046700070928     C                   EXSR      CHKNUM
046800070928     C                   IF        PiInt=*on
046900070928     C                   Z-ADD     PiVal         Num5_0
047000070928     C                   MOVEL     Num5_0        VABCAD
047100070928     C                   ELSE
047200070928     C                   SETON                                        32
047300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400070928     C                             + ' ' + 'VABCAD'
047500070928     C                   ENDIF
047600121113     C*
047700121113     C* Se provincia nn valorizzata la reperisco
047800121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
047900121113     C                   IF        VABPRD = *blanks
048000121113     C                   CLEAR                   TISI95DS
048100121113     C                   EVAL      I95TCN = '3'
048200121113     C                   Z-ADD     datcor        I95DAT
048300121113     C                   EVAL      I95NAR = VABNZD
048400121113     C                   EVAL      I95CAP = VABCAD
048500121113     C                   EVAL      I95LOC = VABLOD
048600121113     C                   CALL      'TISI95R'
048700121113     C                   PARM                    TISI95DS
048800121113     C                   EVAL      VABPRD = O95PRV
048900121113     C                   ENDIF
049000121113     C* RMN
049100150527     C                   EVAL      PiStr=%trim(%subst(vindta:287:15))
049200121113     C                   EXSR      CHKNUM
049300121113     C                   IF        PiInt=*on
049400121113     C                   Z-ADD     PiVal         VABRMN
049500121113     C                   ELSE
049600121113     C                   SETON                                        32
049700121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800121113     C                             + ' ' + 'VABRMN'
049900121113     C                   ENDIF
050000121113     C* NCL
050100150527     C                   EVAL      PiStr=%trim(%subst(vindta:315:3))
050200121113     C                   EXSR      CHKNUM
050300121113     C                   IF        PiInt=*on
050400121113     C                   Z-ADD     PiVal         VABNCL
050500121113     C                   ELSE
050600121113     C                   SETON                                        32
050700121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050800121113     C                             + ' ' + 'VABNCL'
050900121113     C                   ENDIF
051000121113     C* PKB
051100150527     C                   EVAL      PiStr=%trim(%subst(vindta:318:12))
051200121113     C                   EXSR      CHKNUM
051300121113     C                   IF        PiNum=*on
051400121113     C                   Z-ADD     PiVal         VABPKB
051500121113     C                   ELSE
051600121113     C                   SETON                                        32
051700121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800121113     C                             + ' ' + 'VABPKB'
051900121113     C                   ENDIF
052000121113     C* CAS
052100150527     C                   IF        %subst(vindta:333:12) <> *blanks and
052200150527     C                             %subst(vindta:333:12) <> *zeros  and
052300150527     C                             %subst(vindta:333:12) <> '000000000.00'
052400121113     C                   MOVEL     '1'           FlgCAS
052500150527     C                   EVAL      PiStr=%trim(%subst(vindta:333:12))
052600150527     C                   EVAL      VABVCA='EUR'
052700121113     C                   EXSR      CHKNUM
052800121113     C                   IF        PiNum=*on
052900121113     C                   Z-ADD     PiVal         VABCAS
053000121113     C                   ELSE
053100121113     C                   SETON                                        32
053200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053300121113     C                             + ' ' + 'VABCAS'
053400121113     C                   ENDIF
053500121113     C                   ENDIF
053600070730     C*
053700070730     C* Considerazioni finali su CBO/CAS
053800121113     C                   IF        FlgCAS <> '0'
053900070730     C                   IF        VABCBO = '1'
054000070730     C                   EVAL      VABCBO = '4'
054100070730     C                   ENDIF
054200070730     C                   IF        VABCBO = '2'
054300070730     C                   EVAL      VABCBO = '6'
054400070730     C                   ENDIF
054500070730     C                   ENDIF
054600020305     C*
054700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054800011113     C                   EXSR      CHKIMPDIV
054900010202     C*
055000000801     C* Ebbene...
055100000801     C                   ADD       1             §CTRMO
055200070530     C                   IF        *in31 <> *zeros OR
055300070530     C                             *in32 <> *zeros
055400000801     C                   ADD       1             §CTRNO
055500020725     C                   EVAL      x_vinflg = '2'
055600000801     C                   ELSE
055700010201     C                   ADD       1             §CTROKVB
055800000801     C                   ENDIF
055900000801     C*
056000000801     C                   ENDSR
056100010201     C*----------------------------------------------------*
056200120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
056300010201     C*----------------------------------------------------*
056400071003     C     WRIVAT_B      BEGSR
056500090204     C*
056600090204     C                   eval      vatAAS = vabAAS
056700090204     C                   eval      vatLNP = vabLNP
056800090204     C                   eval      vatNRS = vabNRS
056900090204     C                   eval      vatNSP = vabNSP
057000010201     C*
057100120706     C* Valorizzo il buffer di scrittura del EDIVAT
057200070928     C* - TIPO RECORD "B"
057300150527     C                   if        %subst(vindta:208:12) <> *blanks
057400120706     C*
057500120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
057600120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
057700120706     C                   EVAL      VATCNT = 1
057800120706     C*
057900070928     C                   eval      VATTRC = 'B'
058000150527     C                   eval      VATNOT = %trim(%subst(vindta:208:12))
058100150527     C                   write     EDIVAT00
058200111031     C                   add       1             §ctrokvt
058300150527     C                   endif
058400010201     C*
058500010201     C                   ENDSR
058600150527     C*----------------------------------------------------*
058700150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "S"
058800150527     C*----------------------------------------------------*
058900150527     C     WRIVAT_S      BEGSR
059000150527     C*
059100150527     C                   eval      vatAAS = vabAAS
059200150527     C                   eval      vatLNP = vabLNP
059300150527     C                   eval      vatNRS = vabNRS
059400150527     C                   eval      vatNSP = vabNSP
059500150527     C*
059600150527     C* Valorizzo il buffer di scrittura del EDIVAT
059700150527     C* - TIPO RECORD "S"
059800150527     C                   if        %subst(vindta:491:50) <> *blanks
059900150527     C*
060000150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
060100150527     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
060200150527     C                   EVAL      VATCNT = 1
060300150527     C*
060400150527     C                   eval      VATTRC = 'S'
060500150527     C                   eval      VATNOT = %trim(%subst(vindta:491:50))
060600150527     C                   eval      %subst(VATNOT:17:2)=*blank
060700150527     C                   write     EDIVAT00
060800150527     C                   add       1             §ctrokvt
060900150527     C                   endif
061000150527     C*
061100150527     C                   ENDSR
061200150527     C*----------------------------------------------------*
061300150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
061400150527     C*----------------------------------------------------*
061500150527     C     WRIVAT_I      BEGSR
061600150527     C*
061700150527     C                   eval      vatAAS = vabAAS
061800150527     C                   eval      vatLNP = vabLNP
061900150527     C                   eval      vatNRS = vabNRS
062000150527     C                   eval      vatNSP = vabNSP
062100150527     C*
062200150527     C* Valorizzo il buffer di scrittura del EDIVAT
062300150527     C* - TIPO RECORD "I" e "J"
062400150527     C                   if        %subst(vindta:541:50) <> *blanks
062500150527     C                   eval      w70 = %trim(%subst(vindta:541:50))
062600150527     C*
062700150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
062800150527     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
062900150527     C                   EVAL      VATCNT = 1
063000150527     C*
063100150527     C                   eval      VATTRC = 'I'
063200150527     C                   eval      VATNOT = %subst(w70:1:35)
063300150527     C                   write     EDIVAT00
063400150527     C                   add       1             §ctrokvt
063500150527     C*
063600150527     C                   IF        %subst(w70:36:35) <> *blank
063700150527     C                   eval      VATTRC = 'J'
063800150527     C                   eval      VATNOT = %subst(w70:36:35)
063900150527     C                   write     EDIVAT00
064000150527     C                   add       1             §ctrokvt
064100150527     C                   endif
064200150527     C                   endif
064300150527     C*
064400150527     C                   ENDSR
064500071003     C*----------------------------------------------------*
064600120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
064700071003     C*----------------------------------------------------*
064800071003     C     WRIVAT_E      BEGSR
064900090204     C*
065000090204     C                   eval      vatAAS = vabAAS
065100090204     C                   eval      vatLNP = vabLNP
065200090204     C                   eval      vatNRS = vabNRS
065300090204     C                   eval      vatNSP = vabNSP
065400071003     C*
065500120706     C* Valorizzo il buffer di scrittura del EDIVAT
065600071003     C* - TIPO RECORD "E"
065700150527     C                   if        %subst(vindta:280:25) <> *blanks
065800120706     C*
065900120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
066000120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
066100120706     C                   EVAL      VATCNT = 1
066200120706     C*
066300071003     C                   eval      VATTRC = 'E'
066400150527     C                   eval      vatNOT = %trim(%subst(vindta:280:25))
066500150528     C* recupero il check digit
066600150528     C                   clear                   TRUL28DSX
066700150528     C                   eval      I284STD = 'E10'
066800150528     C                   eval      I284COD = VATNOT
066900150528     C                   call      'TRUL28R4'
067000150528     C                   parm                    TRUL28DSX
067100150528     C* Se tutto ok scrivo il "chi sono" corrente col check digit
067200150528     C                   IF        O284err = *blanks
067300150528     C                   EVAL      VATNOT = %trim(VATNOT)
067400150528     C                              + %trim(O284CkD)
067500120706     C                   write     EDIVAT00
067600111031     C                   add       1             §ctrokvt
067700150528     C                   ELSE
067800150528     C                   SETON                                        31
067900150528     C                   EVAL      VATNOT = *blank
068000150528     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
068100150528     C                             + ' ' + 'VATNOT'
068200071003     C                   endif
068300150528     C                   endif
068400071003     C*
068500071003     C                   ENDSR
068600020904
068700020904
068800010202     C*----------------------------------------------------*
068900120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
069000010202     C*----------------------------------------------------*
069100020305     C     PREVAT        BEGSR
069200010202     C*
069300120706     C* Compongo il nome del membro da dare al EDIVATWR
069400010202     C                   eval      parmbr = vlrhdl
069500010202     C                   movel     'M'           parmbr
069600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069700010202     C                   eval      paropz = '1'
069800010202     C* Effettuo la chiamata al CLLE preposto
069900120706     C                   call(e)   'TITVEVTC'
070000010202     C                   parm                    parccm
070100010202     C                   parm                    parmbr
070200010202     C                   parm                    paropz
070300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070400010202     C                   if        %error
070500010202     C                   movel     '1'           chkcall
070600010202     C                   else
070700010202     C                   movel     '0'           chkcall
070800010202     C                   endif
070900010202     C*
071000010202     C                   ENDSR
071100000801     C*----------------------------------------------------*
071200000801     C*  CONTROLLO NUMERICITA' CAMPI
071300000801     C*----------------------------------------------------*
071400000801     C     CHKNUM        BEGSR
071500081113     C*
071600081113     C                   IF        PiDecChr = *blanks
071700121113     C                   EVAL      PiDecChr = '.'
071800081113     C                   ENDIF
071900081113     C*
072000081113     C                   callp(e)  UBISNUM_Check(PiStr
072100081113     C                                          :PiDecChr
072200081113     C                                          :PiVal
072300081113     C                                          :PiNum
072400081113     C                                          :PiInt)
072500081113     C*
072600000801     C                   IF        %error
072700000801     C                   EVAL      PiInt=*off
072800000801     C                   ENDIF
072900000801     C*
073000000801     C                   ENDSR
073100000801     C***
073200000801
073300011113
073400011113     C*----------------------------------------------------*
073500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
073600011113     C*----------------------------------------------------*
073700011113     C     CHKIMPDIV     BEGSR
073800011113     C*
073900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
074000011113     C                   Z-ADD     *zeros        wrkDec            9 9
074100011113     C*
074200011113     C* Come prima cosa effettuo considerazioni sulla divisa
074300011113     C                   IF        vabIAS > *zeros
074400011113     C                   IF        vabVAS <> 'EUR'
074500011113     C                   EVAL      vabVAS =  'ITL'
074600011113     C                   ENDIF
074700011113     C                   ENDIF
074800011113     C*
074900011113     C                   IF        vabCAS > *zeros
075000011113     C                   IF        vabVCA <> 'EUR'
075100011113     C                   EVAL      vabVCA =  'ITL'
075200011113     C                   ENDIF
075300011113     C                   ENDIF
075400011113     C*
075500011113     C                   IF        vabVMD > *zeros
075600020305     C                   IF        vabVAD <> 'EUR'
075700011113     C                   EVAL      vabVAD =  'ITL'
075800011113     C                   ENDIF
075900011113     C                   ENDIF
076000011113     C*
076100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
076200011113     C                   Z-ADD     vabIAS        wrkDec
076300011113     C                   IF        wrkDec > *zeros
076400011113     C                   IF        vabVAS = 'ITL'
076500011113     C                   EVAL      vabIAS = *zeros
076600011113     C                   ENDIF
076700011113     C                   ENDIF
076800011113     C*
076900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
077000011113     C                   Z-ADD     vabCAS        wrkDec
077100011113     C                   IF        wrkDec > *zeros
077200011113     C                   IF        vabVCA = 'ITL'
077300011113     C                   EVAL      vabCAS = *zeros
077400011113     C                   ENDIF
077500011113     C                   ENDIF
077600011113     C*
077700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
077800011113     C                   Z-ADD     vabVMD        wrkDec
077900011113     C                   IF        wrkDec > *zeros
078000011113     C                   IF        vabVAD = 'ITL'
078100011113     C                   EVAL      vabVMD = *zeros
078200011113     C                   ENDIF
078300011113     C                   ENDIF
078400011113     C*
078500011113     C                   ENDSR
078600011113     C***
078700011113
078800011113
078900000801
079000000801
079100990920      /TITLE Invio dei dati al punto operativo.
079200010202     C     invio         BEGSR
079300990920     C*
079400120706     C* 1° invio EDIVAT
079500010201     C                   reset                   dscmz
079600111031     C                   move      wPOI          cmzdst
079700120706     C                   eval      cmzfld = 'EDIVATWR'
079800010201     C                   eval      cmzmbd = vlrhdl
079900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080000021009     C***                if        prmfir = *blanks
080100120706     C                   eval      cmzfla = 'EDIVAT0F'
080200120706     C                   eval      cmzmba = 'EDIVAT0F'
080300021009     C***                else
080400021009     C***                eval      cmzfla = prmfir
080500021009     C***                eval      cmzmba = prmfir
080600021009     C***                endif
080700010201     C                   eval      cmznrr = *zeros
080800020305     C                   move      §ctrokvt      cmznrr
080900021018     C                   eval      cmzlba = vlrfl1
081000010201     C                   call(e)   'TIS711C'
081100010201     C                   parm                    dscmz
081200010201     C                   parm      *blanks       esito
081300010205     C                   if        %error
081400010205     C                             or cmzerr = '1'
081500010205     C                             or esito  = '1'
081600010205     C                   eval      wrkesito = '3'
081700010205     C                   else
081800010201     C*
081900120706     C* 2° invio EDIVAB
082000010201     C                   reset                   dscmz
082100111031     C                   move      wPOI          cmzdst
082200010201     C                   eval      cmzfld = vlrfou
082300010201     C                   eval      cmzmbd = vlrhdl
082400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082500021009     C***                if        prmfir = *blanks
082600120712     C                   eval      cmzfla = 'EDIVAB0F'
082700120712     C                   eval      cmzmba = 'EDIVAB0F'
082800021009     C***                else
082900021009     C***                eval      cmzfla = prmfir
083000021009     C***                eval      cmzmba = prmfir
083100021009     C***                endif
083200010201     C                   eval      cmznrr = *zeros
083300010201     C                   move      §ctrokvb      cmznrr
083400021018     C                   eval      cmzlba = vlrfl1
083500010201     C                   call(e)   'TIS711C'
083600010201     C                   parm                    dscmz
083700010201     C                   parm      *blanks       esito
083800010201     C                   if        %error
083900010201     C                             or cmzerr = '1'
084000010201     C                             or esito  = '1'
084100010201     C                   eval      wrkesito = '3'
084200010201     C                   endif
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
085300120706     C                   if        %open(edivabwr)
085400120706     C                   close     edivabwr
085500120706     C                   endif
085600120706     C                   if        %open(edivatwr)
085700120706     C                   close     edivatwr
085800070411     C                   endif
085900070411     C*
086000070411     C* Effettuo la chiamata al CLLE preposto
086100120706     C                   call(e)   'TITVEVTC'
086200070411     C                   parm                    parccm
086300070411     C                   parm                    parmbr
086400070411     C                   parm      '2'           paropz
086500070411     C*
086600070411     C                   eval      wrkesito = '2'
086700070411     C*
086800070411     C                   seton                                        LR
086900070411     C*
087000070411     C                   ENDSR     '*CANCL'
087100070411     C***
087200070411
087300990910
087400000613     C     *inzsr        BEGSR
087500990910     C*
087600990910     C     *entry        plist
087700990920     C                   parm                    tivlrds
087800990921     C                   parm      wrkesito      esito
087900000724     C                   parm                    prmlit
088000000710     C                   parm                    prmfir
088100000613     C*
088200000830     C* CALCOLA LA DATA CORRENTE
088300120706     C                   time                    wn14             14 0
088400120706     C                   movel     wn14          oracor            6 0          *ORA
088500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088600100324     C                   eval      datcor = %dec(%date() : *ISO)
088700000830     C*
088800000613     C                   ENDSR
088900000613     C***
