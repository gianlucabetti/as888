000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120903      *
000300170711      * PARTICOLARITA':
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
002700170519     D depspe          s             10    INZ(*blanks)
002800170519     D curspe          s             10    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500120903     D wDivPKB         s                   like(VABNCL)
003600150527     D w70             s             70
003700170711     D w80             s             80
003800000830
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400101007     D  VINDTA                     2048
004500101007     D  VINFLG                        1
004600170310     D  VINSPE                        7
004700101007     D  VINRRN                        8  0
004800020725     D*
004900080923     D*------------------
005000080923     D* DS REPERIMENTO NUMERATORE
005100080923     D*------------------
005200080923     D trul33ds      e ds                  inz
005300150528     D*------------------
005400150528     D TRUL28DSX     e ds                  inz
005500080923     D*------------------
005600080923     D* DS ARCHITETTURA
005700080923     D*------------------
005800080923     D kpjba         e ds                  inz
005900080923     D*------------------
006000121113     D  Num7_0         s              7  0
006100081113
006200081113     D*------------------
006300081113     D* LINKING A DEFINIZIONI ESTERNE
006400081113     D*------------------
006500100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
008200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008300000613     C*
008400010202     C* Effettuo la chiamata al CLLE preposto
008500120706     C                   call(e)   'TITVEVTC'
008600010202     C                   parm                    parccm
008700010202     C                   parm                    parmbr
008800010202     C                   parm      '2'           paropz
008900070730     C*
009000070730     C* Effettuo lancio TISI95 solo x chiusura
009100070730     C                   CLEAR                   TISI95DS
009200070730     C                   EVAL      I95TLA = 'C'
009300070730     C                   CALL      'TISI95R'
009400070730     C                   PARM                    TISI95DS
009500000616     C*
009600000801     C
009700010201     C                   seton                                        LR
009800990908
009900000801
010000910830     C*--------------------------------------------------------
010100120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
010200910830     C*--------------------------------------------------------
010300070530     C     RWFILE        BEGSR
010400990910     C*
010500990914     C                   if        not %open(tivin00r)
010600990908     C                   open      tivin00r
010700990914     C                   endif
010800120706     C                   if        not %open(edivabwr)
010900120706     C                   open      edivabwr
011000990914     C                   endif
011100070530     C*
011200120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011300020305     C                   exsr      prevat
011400010201     C*
011500010202     C                   if        chkcall = '0'
011600010202     C*
011700120706     C                   if        not %open(edivatwr)
011800120706     C                   open      edivatwr
011900010201     C                   endif
012000080117     C*
012100080117     C                   EXSR      INZVAR
012200080117     C                   EXSR      DEFCAM
012300990910     C*
012400010201     C                   clear                   §CTROKVB          5 0
012500020305     C                   clear                   §CTROKVT          5 0
012600000801     C                   clear                   §CTRMO            5 0
012700000801     C                   clear                   §CTRNO            5 0
012800990910     C*
012900020725     C/EXEC SQL
013000020725     C+ declare C1 cursor for select
013100170310     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
013200060223     C+ from tivin00r
013300060223     C+ order by sped
013400020725     C+ for read only
013500020725     C/END-EXEC
013600020725     C
013700020725     C/EXEC SQL
013800020725     C+ open C1
013900020725     C/END-EXEC
014000020725     C
014100020725     C/EXEC SQL
014200070530     C+ Fetch C1 into :INPUT_DS
014300020725     C/END-EXEC
014400020725     C*
014500020725     C                   dow       sqlcod = *zeros
014600990913     C*
014700020725     C                   if        vindta > *blanks
014800000613     C                   add       1             rrnum
014900000801     C*
015000020725     C                   if        vinflg = *blanks
015100020725     C                             or vinflg = '0'
015200020725     C                             or vinflg = '2'
015300000801     C*
015400020725     C                   clear                   x_vinmsg
015500020725     C                   eval      x_vinflg = '1'
015600010305     C*
015700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015800170310     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
015900020305     C                   MOVEL(p)  PiStr         curspe
016000010305     C*
016100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016200080923     C                   if        *in50 = *off
016300170310     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione
016400170310     C***                exsr      repNSP
016500071003     C                   exsr      impvab
016600111130     C  N31              eval      fivabds_ok = fivabds
016700111130     C  N31              exsr      wrivab
016800170330     C                   exsr      WRIVAT_B                                     => carico VAT
016900170310     C                   exsr      wrivat_i                                     => carico VAT
017000111125     C                   exsr      inzvar
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400150527     C                   exsr      repNSP
017500071009     C                   exsr      impvab
017600111130     C  N31              eval      fivabds_ok = fivabds
017700111130     C  N31              exsr      wrivab
017800170330     C                   exsr      WRIVAT_B                                     => carico VAT
017900170310     C                   exsr      wrivat_i                                     => carico VAT
018000170310     C                   exsr      wrivat_e                                     => carico VAT
018100111125     C                   exsr      inzvar
018200071009     C                   else
018300071009     C*
018400010305     C                   if        depspe = *blanks                             => 1° giro
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600080117     C                   clear                   tivinds
018700150527     C                   exsr      repNSP
018800020305     C                   exsr      impvab
018900111130     C                   z-add     VABNCL        sav_VABNCL
019000111130     C                   z-add     VABPKB        sav_VABPKB
019100111130     C                   z-add     VABVLB        sav_VABVLB
019200170330     C                   exsr      WRIVAT_B                                     => carico VAT
019300170310     C                   exsr      wrivat_i                                     => carico VAT
019400170310     C   50              exsr      wrivat_e                                     => carico VAT
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
021300170330     C                   exsr      WRIVAT_B                                     => carico VAT
021400170310     C                   exsr      wrivat_i                                     => carico VAT
021500170310     C   50              exsr      wrivat_e                                     => carico VAT
021600020305     C                   else                                                   => x stessa spediz
021700111130     C                   exsr      impvab
021800120706     C   51              add       VABNCL        sav_VABNCL
021900111125     C   51              add       VABPKB        sav_VABPKB
022000111125     C   51              add       VABVLB        sav_VABVLB
022100111125     C   52              z-add     VABNCL        sav_VABNCL
022200111125     C   52              z-add     VABPKB        sav_VABPKB
022300111125     C   52              z-add     VABVLB        sav_VABVLB
022400111125     C   53              add       VABNCL        sav_VABNCL
022500111125     C   53              z-add     VABPKB        sav_VABPKB
022600111125     C   53              z-add     VABVLB        sav_VABVLB
022700111125     C*
022800170330     C                   exsr      WRIVAT_B                                     => carico VAT
022900170310     C                   exsr      wrivat_i                                     => carico VAT
023000170310     C   50              exsr      wrivat_e                                     => carico VAT
023100010305     C                   endif
023200010305     C                   endif
023300010305     C                   endif
023400071003     C                   endif
023500071009     C                   endif
023600000905     C*
023700000905     C                   else
023800020725     C                   eval      x_vinflg = '1'
023900000905     C                   endif
024000000905     C*
024100020725     C     VINRRN        chain     tivin000
024200020725     C                   if        %found(tivin00r)
024300020725     C                   eval      y_vinflg = x_vinflg
024400020725     C                   eval      y_vinmsg = x_vinmsg
024500020725     C                   update    tivin000
024600020725     C                   endif
024700020725     C*
024800020725     C/EXEC SQL
024900070530     C+ Fetch C1 into :INPUT_DS
025000020725     C/END-EXEC
025100020725     C*
025200020725     C                   enddo
025300020725     C*
025400020725     C/EXEC SQL
025500020725     C+ close C1
025600020725     C/END-EXEC
025700000905     C*
025800020305     C* Scarico i VAB rimasti "in sospeso"
025900071009     C                   if        wDISK = 'C'
026000111130     C  N31              eval      fivabds_ok = fivabds
026100111130     C  N31              exsr      wrivab
026200071009     C                   endif
026300010202     C*
026400010202     C                   endif
026500990910
026600990910     C* Se non ci sono record con errori ...
026700000710     C                   if        §ctrno = 0
026800990910     C* ... restituisco esito OK.
026900990921     C                   eval      wrkesito = '0'
027000990910     C                   else
027100010201     C                   if        §ctrokvb > 0
027200990921     C                   eval      wrkesito = '1'
027300000710     C                   else
027400000710     C                   eval      wrkesito = '2'
027500990910     C                   endif
027600000710     C                   endif
027700990910     C*
027800990914     C                   if        %open(tivin00r)
027900990908     C                   close     tivin00r
028000990914     C                   endif
028100120706     C                   if        %open(edivabwr)
028200120706     C                   close     edivabwr
028300120706     C                   endif
028400120706     C                   if        %open(edivatwr)
028500120706     C                   close     edivatwr
028600010201     C                   endif
028700990910     C*
028800111031     C                   if        §ctrokvb > 0 or
028900111031     C                             §ctrokvt > 0
029000111031     C                             and wPOI <> *zeros
029100010202     C                   exsr      invio
029200990920     C                   endif
029300990920     C*
029400910830     C                   ENDSR
029500000613     C***
029600010305
029700010305     C*----------------------------------------------------*
029800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029900010305     C*----------------------------------------------------*
030000020305     C     WRIVAB        BEGSR
030100070730     C*
030200071003     C* Considerazioni finali
030300111125     C                   if        ok_VABRMA = *blanks
030400111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030500071003     C                   endif
030600120706     C*
030700120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
030800160701     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
030900120709     C                   EVAL      ok_VABDCM = datcor
031000120709     C                   EVAL      ok_VABDTS = datcor
031100120709     C                   EVAL      ok_VABHMS = oracor
031200120709     C                   EVAL      ok_VABCNT = 1
031300071003     C*
031400120706     C                   write     edivab00                                     => scarico il VAB
031500010305     C*
031600010305     C                   ENDSR
031700990920
031800000801     C*----------------------------------------------------*
031900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032000000801     C*----------------------------------------------------*
032100010201     C     INZVAR        BEGSR
032200000801     C*
032300010201     C                   Z-ADD     *zeros        Num5_0
032400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032500020725     C                   MOVEL     '0'           FlgCAS            1
032600111125     C*
032700111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032800111130     C*
032900111125     C                   CLEAR                   FIVABDS
033000111130     C                   CLEAR                   FIVABDS_OK
033100111130     C                   CLEAR                   FIVABDS_SAV
033200120706     C                   CLEAR                   EDIVAB00
033300120706     C                   CLEAR                   EDIVAT00
033400000801     C*
033500000801     C                   ENDSR
033600000801     C*----------------------------------------------------*
033700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033800000801     C*----------------------------------------------------*
033900020904     C     DEFCAM        BEGSR
034000080923     C*
034100111125     C                   SETOFF                                       50
034200111125     C                   SETOFF                                       515253
034300070730     C* Imposto i valori di default...
034400170421     C                   EVAL      VABCCM = 0901950
034500170421     C                   EVAL      VATCCM = 0901950
034600170421     C                   EVAL      VABLNP = 090
034700170421     C                   EVAL      VATLNP = 090
034800160701     C                   EVAL      VABCTR = 000
034900160701     C                   EVAL      VABCBO = '1'
035000070222     C* ... e poi verifico se sono stati passati come parametri
035100070222     C                   IF        vlrppt > *blanks
035200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035400070222     C                   EXSR      CHKNUM
035500070222     C                   IF        PiInt=*on
035600070222     C                   Z-ADD     PiVal         VABCCM
035700070222     C                   Z-ADD     PiVal         VATCCM
035800070222     C                   ENDIF
035900070222     C                   ENDIF
036000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABLNP
036500070222     C                   Z-ADD     PiVal         VATLNP
036600070222     C                   ENDIF
036700070222     C                   ENDIF
036800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037000070222     C                   EXSR      CHKNUM
037100070222     C                   IF        PiInt=*on
037200070222     C                   Z-ADD     PiVal         VABCTR
037300070222     C                   ENDIF
037400070928     C                   ENDIF
037500071009     C                   MOVEL     *blanks       wDISK             1
037600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037700071009     C                   IF        wDISK <> *blanks
037800070928     C                   SETON                                        50
037900070222     C                   ENDIF
038000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038100111125     C                   SETON                                        41
038200080923     C                   ENDIF
038300070222     C                   ENDIF
038400120706     C*
038500120706     C* valido per nr.colli/peso/volume
038600120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038700120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038800120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038900120706     C                   SELECT
039000120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039100120706     C                   SETON                                        51
039200120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039300120706     C                   SETON                                        52
039400120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039500120706     C                   SETON                                        53
039600120706     C                   ENDSL
039700120706     C*
039800071009     C   50              EVAL      VABCTM = '7Q'
039900000801     C*
040000000801     C                   ENDSR
040100150527     C*----------------------------------------------------*
040200150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040300150527     C*----------------------------------------------------*
040400150527     C     REPNSP        BEGSR
040500150527     C*
040600150527     C                   EXSR      INZVAR
040700150527     C                   EXSR      DEFCAM
040800150527     C*
040900150527     C                   SETON                                        60
041000150527     C*
041100150527     C* NSP => Stacco un numeratore da AZNUM
041200150527     C                   clear                   TRUL33DS
041300150527     C                   eval      I33OPE = *zeros
041400150527     C                   eval      I33CNU = 302
041500150527     C                   eval      I33NUM = 1
041600150527     C                   movel     TRUL33DS      KPJBU
041700150527     C                   call      'TRUL33R'
041800150527     C                   parm                    KPJBA
041900150527     C                   movel     KPJBU         TRUL33DS
042000150527     C                   if        O33ERR = *zeros
042100150527     C                   z-add     O33NRF        VABNSP
042200150527     C                   z-add     O33NRF        VATNSP
042300150527     C                   else
042400150527     C                   SETON                                        31
042500150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042600150527     C                             + ' ' + 'VABNSP VATNSP'
042700150527     C                   endif
042800150527     C*
042900150527     C                   ENDSR
043000000801     C*----------------------------------------------------*
043100120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
043200000801     C*----------------------------------------------------*
043300010201     C     IMPVAB        BEGSR
043400070530     C*
043500070530     C                   SETOFF                                       3132
043600070928     C*
043700070928     C                   EXSR      DEFCAM
043800070928     C*
043900111031     C                   IF        vlrpoi <> 999
044000070928     C                   MOVE(P)   vlrpoi        VABFGS
044100070928     C                   MOVE(P)   vlrpoi        VATFGS
044200111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044300111031     C                   ENDIF
044400070928     C*
044500070928     C                   MOVEL     datcor        VABAAS
044600070928     C                   MOVEL     datcor        VATAAS
044700070928     C                   MOVE      datcor        VABMGS
044800170310     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
044900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045100070928     C* ==
045200170310     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
045300170310     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
045400170310     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
045500170310     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
045600170711     C                   EVAL      w80   =%trim(%subst(vindta:197:80))
045700170711     C                   eval      VABNOT = %subst(w80:1:35)
045800170711     C                   eval      VABNT2 = %subst(w80:36:35)
045900160701
046000071003     C* CAD
046100170310     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
046200070928     C                   EXSR      CHKNUM
046300070928     C                   IF        PiInt=*on
046400070928     C                   Z-ADD     PiVal         Num5_0
046500070928     C                   MOVEL     Num5_0        VABCAD
046600070928     C                   ELSE
046700070928     C                   SETON                                        32
046800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900070928     C                             + ' ' + 'VABCAD'
047000070928     C                   ENDIF
047100121113     C*
047200121113     C* Se provincia nn valorizzata la reperisco
047300121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
047400121113     C                   IF        VABPRD = *blanks
047500121113     C                   CLEAR                   TISI95DS
047600121113     C                   EVAL      I95TCN = '3'
047700121113     C                   Z-ADD     datcor        I95DAT
047800121113     C                   EVAL      I95NAR = VABNZD
047900121113     C                   EVAL      I95CAP = VABCAD
048000121113     C                   EVAL      I95LOC = VABLOD
048100121113     C                   CALL      'TISI95R'
048200121113     C                   PARM                    TISI95DS
048300121113     C                   EVAL      VABPRD = O95PRV
048400121113     C                   ENDIF
048500121113     C* RMN
048600170330     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
048700121113     C                   EXSR      CHKNUM
048800121113     C                   IF        PiInt=*on
048900121113     C                   Z-ADD     PiVal         VABRMN
049000121113     C                   ELSE
049100121113     C                   SETON                                        32
049200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049300121113     C                             + ' ' + 'VABRMN'
049400121113     C                   ENDIF
049500121113     C* NCL
049600170310     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
049700121113     C                   EXSR      CHKNUM
049800121113     C                   IF        PiInt=*on
049900121113     C                   Z-ADD     PiVal         VABNCL
050000121113     C                   ELSE
050100121113     C                   SETON                                        32
050200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300121113     C                             + ' ' + 'VABNCL'
050400121113     C                   ENDIF
050500121113     C* PKB
050600170310     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
050700121113     C                   EXSR      CHKNUM
050800121113     C                   IF        PiNum=*on
050900170310     C                   Z-ADD     PiVal         VABPKB
051000121113     C                   ELSE
051100121113     C                   SETON                                        32
051200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300121113     C                             + ' ' + 'VABPKB'
051400121113     C                   ENDIF
051500171004     C                   IF        VABPKB = *zeros
051600171004     C                   EVAL      VABPKB = 0,1
051700171004     C                   ENDIF
051800170310     C* CAS
051900170310     C                   IF        %subst(vindta:280:12) <> *blanks and
052000170310     C                             %subst(vindta:280:12) <> *zeros  and
052100170310     C                             %subst(vindta:280:12) <> '000000000.00'
052200170310     C                   MOVEL     '1'           FlgCAS
052300170310     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
052400170310     C                   EVAL      VABVCA='EUR'
052500170310     C                   EXSR      CHKNUM
052600170310     C                   IF        PiNum=*on
052700170310     C                   Z-ADD     PiVal         VABCAS
052800170310     C                   ELSE
052900170310     C                   SETON                                        32
053000170310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053100170310     C                             + ' ' + 'VABCAS'
053200170310     C                   ENDIF
053300170310     C                   ENDIF
053400170310     C* IAS
053500170310     C                   IF        %subst(vindta:295:12) <> *blanks and
053600170310     C                             %subst(vindta:295:12) <> *zeros  and
053700170310     C                             %subst(vindta:295:12) <> '000000000.00'
053800170310     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
053900170310     C                   EVAL      VABVAS='EUR'
054000170310     C                   EXSR      CHKNUM
054100170310     C                   IF        PiNum=*on
054200170310     C                   Z-ADD     PiVal         VABIAS
054300170310     C                   ELSE
054400170310     C                   SETON                                        32
054500170310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600170310     C                             + ' ' + 'VABIAS'
054700170310     C                   ENDIF
054800170310     C                   ENDIF
054900170310
055000070730     C*
055100070730     C* Considerazioni finali su CBO/CAS
055200121113     C                   IF        FlgCAS <> '0'
055300070730     C                   IF        VABCBO = '1'
055400070730     C                   EVAL      VABCBO = '4'
055500070730     C                   ENDIF
055600070730     C                   IF        VABCBO = '2'
055700070730     C                   EVAL      VABCBO = '6'
055800070730     C                   ENDIF
055900070730     C                   ENDIF
056000020305     C*
056100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
056200011113     C                   EXSR      CHKIMPDIV
056300010202     C*
056400000801     C* Ebbene...
056500000801     C                   ADD       1             §CTRMO
056600070530     C                   IF        *in31 <> *zeros OR
056700070530     C                             *in32 <> *zeros
056800000801     C                   ADD       1             §CTRNO
056900020725     C                   EVAL      x_vinflg = '2'
057000000801     C                   ELSE
057100010201     C                   ADD       1             §CTROKVB
057200000801     C                   ENDIF
057300000801     C*
057400000801     C                   ENDSR
057500010201     C*----------------------------------------------------*
057600120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
057700010201     C*----------------------------------------------------*
057800170330     C     WRIVAT_B      BEGSR
057900090204     C*
058000090204     C                   eval      vatAAS = vabAAS
058100090204     C                   eval      vatLNP = vabLNP
058200090204     C                   eval      vatNRS = vabNRS
058300090204     C                   eval      vatNSP = vabNSP
058400010201     C*
058500120706     C* Valorizzo il buffer di scrittura del EDIVAT
058600170310     C* - TIPO RECORD "A"
058700170310     C                   if        %subst(vindta:92:15) <> *blanks
058800120706     C*
058900120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
059000170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
059100120706     C                   EVAL      VATCNT = 1
059200120706     C*
059300170330     C                   eval      VATTRC = 'B'
059400170310     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
059500150527     C                   write     EDIVAT00
059600111031     C                   add       1             §ctrokvt
059700150527     C                   endif
059800010201     C*
059900010201     C                   ENDSR
060000150527     C*----------------------------------------------------*
060100150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
060200150527     C*----------------------------------------------------*
060300150527     C     WRIVAT_I      BEGSR
060400150527     C*
060500150527     C                   eval      vatAAS = vabAAS
060600150527     C                   eval      vatLNP = vabLNP
060700150527     C                   eval      vatNRS = vabNRS
060800150527     C                   eval      vatNSP = vabNSP
060900150527     C*
061000150527     C* Valorizzo il buffer di scrittura del EDIVAT
061100150527     C* - TIPO RECORD "I" e "J"
061200170310     C                   if        %subst(vindta:412:70) <> *blanks
061300170310     C                   eval      w70 = %trim(%subst(vindta:412:70))
061400150527     C*
061500150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
061600170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
061700150527     C                   EVAL      VATCNT = 1
061800150527     C*
061900150527     C                   eval      VATTRC = 'I'
062000150527     C                   eval      VATNOT = %subst(w70:1:35)
062100150527     C                   write     EDIVAT00
062200150527     C                   add       1             §ctrokvt
062300150527     C*
062400150527     C                   IF        %subst(w70:36:35) <> *blank
062500150527     C                   eval      VATTRC = 'J'
062600150527     C                   eval      VATNOT = %subst(w70:36:35)
062700150527     C                   write     EDIVAT00
062800150527     C                   add       1             §ctrokvt
062900150527     C                   endif
063000150527     C                   endif
063100150527     C*
063200150527     C                   ENDSR
063300071003     C*----------------------------------------------------*
063400120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
063500071003     C*----------------------------------------------------*
063600071003     C     WRIVAT_E      BEGSR
063700090204     C*
063800090204     C                   eval      vatAAS = vabAAS
063900090204     C                   eval      vatLNP = vabLNP
064000090204     C                   eval      vatNRS = vabNRS
064100090204     C                   eval      vatNSP = vabNSP
064200071003     C*
064300120706     C* Valorizzo il buffer di scrittura del EDIVAT
064400071003     C* - TIPO RECORD "E"
064500170310     C                   if        %subst(vindta:331:26) <> *blanks
064600120706     C*
064700120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064800170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
064900120706     C                   EVAL      VATCNT = 1
065000120706     C*
065100071003     C                   eval      VATTRC = 'E'
065200170310     C                   eval      vatNOT = %trim(%subst(vindta:331:26))
065300120706     C                   write     EDIVAT00
065400111031     C                   add       1             §ctrokvt
065500150528     C                   endif
065600071003     C*
065700071003     C                   ENDSR
065800020904
065900020904
066000010202     C*----------------------------------------------------*
066100120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
066200010202     C*----------------------------------------------------*
066300020305     C     PREVAT        BEGSR
066400010202     C*
066500120706     C* Compongo il nome del membro da dare al EDIVATWR
066600010202     C                   eval      parmbr = vlrhdl
066700010202     C                   movel     'M'           parmbr
066800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066900010202     C                   eval      paropz = '1'
067000010202     C* Effettuo la chiamata al CLLE preposto
067100120706     C                   call(e)   'TITVEVTC'
067200010202     C                   parm                    parccm
067300010202     C                   parm                    parmbr
067400010202     C                   parm                    paropz
067500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067600010202     C                   if        %error
067700010202     C                   movel     '1'           chkcall
067800010202     C                   else
067900010202     C                   movel     '0'           chkcall
068000010202     C                   endif
068100010202     C*
068200010202     C                   ENDSR
068300000801     C*----------------------------------------------------*
068400000801     C*  CONTROLLO NUMERICITA' CAMPI
068500000801     C*----------------------------------------------------*
068600000801     C     CHKNUM        BEGSR
068700081113     C*
068800081113     C                   IF        PiDecChr = *blanks
068900170421     C                   EVAL      PiDecChr = ','
069000081113     C                   ENDIF
069100081113     C*
069200081113     C                   callp(e)  UBISNUM_Check(PiStr
069300081113     C                                          :PiDecChr
069400081113     C                                          :PiVal
069500081113     C                                          :PiNum
069600081113     C                                          :PiInt)
069700081113     C*
069800000801     C                   IF        %error
069900000801     C                   EVAL      PiInt=*off
070000000801     C                   ENDIF
070100000801     C*
070200000801     C                   ENDSR
070300000801     C***
070400000801
070500011113
070600011113     C*----------------------------------------------------*
070700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070800011113     C*----------------------------------------------------*
070900011113     C     CHKIMPDIV     BEGSR
071000011113     C*
071100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
071200011113     C                   Z-ADD     *zeros        wrkDec            9 9
071300011113     C*
071400011113     C* Come prima cosa effettuo considerazioni sulla divisa
071500011113     C                   IF        vabIAS > *zeros
071600011113     C                   IF        vabVAS <> 'EUR'
071700011113     C                   EVAL      vabVAS =  'ITL'
071800011113     C                   ENDIF
071900011113     C                   ENDIF
072000011113     C*
072100011113     C                   IF        vabCAS > *zeros
072200011113     C                   IF        vabVCA <> 'EUR'
072300011113     C                   EVAL      vabVCA =  'ITL'
072400011113     C                   ENDIF
072500011113     C                   ENDIF
072600011113     C*
072700011113     C                   IF        vabVMD > *zeros
072800020305     C                   IF        vabVAD <> 'EUR'
072900011113     C                   EVAL      vabVAD =  'ITL'
073000011113     C                   ENDIF
073100011113     C                   ENDIF
073200011113     C*
073300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
073400011113     C                   Z-ADD     vabIAS        wrkDec
073500011113     C                   IF        wrkDec > *zeros
073600011113     C                   IF        vabVAS = 'ITL'
073700011113     C                   EVAL      vabIAS = *zeros
073800011113     C                   ENDIF
073900011113     C                   ENDIF
074000011113     C*
074100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
074200011113     C                   Z-ADD     vabCAS        wrkDec
074300011113     C                   IF        wrkDec > *zeros
074400011113     C                   IF        vabVCA = 'ITL'
074500011113     C                   EVAL      vabCAS = *zeros
074600011113     C                   ENDIF
074700011113     C                   ENDIF
074800011113     C*
074900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
075000011113     C                   Z-ADD     vabVMD        wrkDec
075100011113     C                   IF        wrkDec > *zeros
075200011113     C                   IF        vabVAD = 'ITL'
075300011113     C                   EVAL      vabVMD = *zeros
075400011113     C                   ENDIF
075500011113     C                   ENDIF
075600011113     C*
075700011113     C                   ENDSR
075800011113     C***
075900011113
076000011113
076100000801
076200000801
076300990920      /TITLE Invio dei dati al punto operativo.
076400010202     C     invio         BEGSR
076500990920     C*
076600120706     C* 1° invio EDIVAT
076700010201     C                   reset                   dscmz
076800111031     C                   move      wPOI          cmzdst
076900120706     C                   eval      cmzfld = 'EDIVATWR'
077000010201     C                   eval      cmzmbd = vlrhdl
077100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077200021009     C***                if        prmfir = *blanks
077300120706     C                   eval      cmzfla = 'EDIVAT0F'
077400120706     C                   eval      cmzmba = 'EDIVAT0F'
077500021009     C***                else
077600021009     C***                eval      cmzfla = prmfir
077700021009     C***                eval      cmzmba = prmfir
077800021009     C***                endif
077900010201     C                   eval      cmznrr = *zeros
078000020305     C                   move      §ctrokvt      cmznrr
078100021018     C                   eval      cmzlba = vlrfl1
078200010201     C                   call(e)   'TIS711C'
078300010201     C                   parm                    dscmz
078400010201     C                   parm      *blanks       esito
078500010205     C                   if        %error
078600010205     C                             or cmzerr = '1'
078700010205     C                             or esito  = '1'
078800010205     C                   eval      wrkesito = '3'
078900010205     C                   else
079000010201     C*
079100120706     C* 2° invio EDIVAB
079200010201     C                   reset                   dscmz
079300111031     C                   move      wPOI          cmzdst
079400010201     C                   eval      cmzfld = vlrfou
079500010201     C                   eval      cmzmbd = vlrhdl
079600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079700021009     C***                if        prmfir = *blanks
079800120712     C                   eval      cmzfla = 'EDIVAB0F'
079900120712     C                   eval      cmzmba = 'EDIVAB0F'
080000021009     C***                else
080100021009     C***                eval      cmzfla = prmfir
080200021009     C***                eval      cmzmba = prmfir
080300021009     C***                endif
080400010201     C                   eval      cmznrr = *zeros
080500010201     C                   move      §ctrokvb      cmznrr
080600021018     C                   eval      cmzlba = vlrfl1
080700010201     C                   call(e)   'TIS711C'
080800010201     C                   parm                    dscmz
080900010201     C                   parm      *blanks       esito
081000010201     C                   if        %error
081100010201     C                             or cmzerr = '1'
081200010201     C                             or esito  = '1'
081300010201     C                   eval      wrkesito = '3'
081400010201     C                   endif
081500010205     C                   endif
081600990920     C*
081700000613     C                   ENDSR
081800000613     C***
081900070411
082000070411     C     *pssr         BEGSR
082100070411     C*
082200070411     C                   if        %open(tivin00r)
082300070411     C                   close     tivin00r
082400070411     C                   endif
082500120706     C                   if        %open(edivabwr)
082600120706     C                   close     edivabwr
082700120706     C                   endif
082800120706     C                   if        %open(edivatwr)
082900120706     C                   close     edivatwr
083000070411     C                   endif
083100070411     C*
083200070411     C* Effettuo la chiamata al CLLE preposto
083300120706     C                   call(e)   'TITVEVTC'
083400070411     C                   parm                    parccm
083500070411     C                   parm                    parmbr
083600070411     C                   parm      '2'           paropz
083700070411     C*
083800070411     C                   eval      wrkesito = '2'
083900070411     C*
084000070411     C                   seton                                        LR
084100070411     C*
084200070411     C                   ENDSR     '*CANCL'
084300070411     C***
084400070411
084500990910
084600000613     C     *inzsr        BEGSR
084700990910     C*
084800990910     C     *entry        plist
084900990920     C                   parm                    tivlrds
085000990921     C                   parm      wrkesito      esito
085100000724     C                   parm                    prmlit
085200000710     C                   parm                    prmfir
085300000613     C*
085400000830     C* CALCOLA LA DATA CORRENTE
085500120706     C                   time                    wn14             14 0
085600120706     C                   movel     wn14          oracor            6 0          *ORA
085700100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085800100324     C                   eval      datcor = %dec(%date() : *ISO)
085900000830     C*
086000000613     C                   ENDSR
086100000613     C***
