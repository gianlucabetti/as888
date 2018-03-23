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
002700170310     D depspe          s              7    INZ(*blanks)
002800170310     D curspe          s              7    INZ(*blanks)
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
004500170310     D  VINSPE                        7
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
013000170310     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
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
015700170310     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200170310     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione
016300170310     C***                exsr      repNSP
016400071003     C                   exsr      impvab
016500111130     C  N31              eval      fivabds_ok = fivabds
016600111130     C  N31              exsr      wrivab
016700170310     C                   exsr      wrivat_a                                     => carico VAT
016800170310     C                   exsr      wrivat_i                                     => carico VAT
016900170310     C                   exsr      wrivat_p                                     => carico VAT
017000111125     C                   exsr      inzvar
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400150527     C                   exsr      repNSP
017500071009     C                   exsr      impvab
017600111130     C  N31              eval      fivabds_ok = fivabds
017700111130     C  N31              exsr      wrivab
017800170310     C                   exsr      wrivat_a                                     => carico VAT
017900170310     C                   exsr      wrivat_i                                     => carico VAT
018000170310     C                   exsr      wrivat_p                                     => carico VAT
018100170310     C                   exsr      wrivat_e                                     => carico VAT
018200111125     C                   exsr      inzvar
018300071009     C                   else
018400071009     C*
018500010305     C                   if        depspe = *blanks                             => 1° giro
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700080117     C                   clear                   tivinds
018800150527     C                   exsr      repNSP
018900020305     C                   exsr      impvab
019000111130     C                   z-add     VABNCL        sav_VABNCL
019100111130     C                   z-add     VABPKB        sav_VABPKB
019200111130     C                   z-add     VABVLB        sav_VABVLB
019300170310     C                   exsr      wrivat_a                                     => carico VAT
019400170310     C                   exsr      wrivat_i                                     => carico VAT
019500170310     C                   exsr      wrivat_p                                     => carico VAT
019600170310     C   50              exsr      wrivat_e                                     => carico VAT
019700010305     C                   else
019800020725     C                   if        depspe <> curspe                             => rottura di spediz
019900111130     C                   z-add     sav_VABNCL    VABNCL
020000111130     C                   z-add     sav_VABPKB    VABPKB
020100111130     C                   z-add     sav_VABVLB    VABVLB
020200111130     C  N31              eval      fivabds_ok = fivabds
020300111125     C  N31              exsr      wrivab
020400111125     C                   exsr      inzvar
020500111125     C                   eval      depspe = curspe                              => memorizz. spediz
020600080117     C                   clear                   tivinds
020700150527     C                   exsr      repNSP
020800020305     C                   exsr      impvab
020900111130     C                   if        wCntRecSpe = *zeros
021000111130     C                   z-add     VABNCL        sav_VABNCL
021100111130     C                   z-add     VABPKB        sav_VABPKB
021200111130     C                   z-add     VABVLB        sav_VABVLB
021300111130     C                   endif
021400111130     C                   add       1             wCntRecSpe
021500170310     C                   exsr      wrivat_a                                     => carico VAT
021600170310     C                   exsr      wrivat_i                                     => carico VAT
021700170310     C                   exsr      wrivat_p                                     => carico VAT
021800170310     C   50              exsr      wrivat_e                                     => carico VAT
021900020305     C                   else                                                   => x stessa spediz
022000111130     C                   exsr      impvab
022100120706     C   51              add       VABNCL        sav_VABNCL
022200111125     C   51              add       VABPKB        sav_VABPKB
022300111125     C   51              add       VABVLB        sav_VABVLB
022400111125     C   52              z-add     VABNCL        sav_VABNCL
022500111125     C   52              z-add     VABPKB        sav_VABPKB
022600111125     C   52              z-add     VABVLB        sav_VABVLB
022700111125     C   53              add       VABNCL        sav_VABNCL
022800111125     C   53              z-add     VABPKB        sav_VABPKB
022900111125     C   53              z-add     VABVLB        sav_VABVLB
023000111125     C*
023100170310     C                   exsr      wrivat_a                                     => carico VAT
023200170310     C                   exsr      wrivat_i                                     => carico VAT
023300170310     C                   exsr      wrivat_p                                     => carico VAT
023400170310     C   50              exsr      wrivat_e                                     => carico VAT
023500010305     C                   endif
023600010305     C                   endif
023700010305     C                   endif
023800071003     C                   endif
023900071009     C                   endif
024000000905     C*
024100000905     C                   else
024200020725     C                   eval      x_vinflg = '1'
024300000905     C                   endif
024400000905     C*
024500020725     C     VINRRN        chain     tivin000
024600020725     C                   if        %found(tivin00r)
024700020725     C                   eval      y_vinflg = x_vinflg
024800020725     C                   eval      y_vinmsg = x_vinmsg
024900020725     C                   update    tivin000
025000020725     C                   endif
025100020725     C*
025200020725     C/EXEC SQL
025300070530     C+ Fetch C1 into :INPUT_DS
025400020725     C/END-EXEC
025500020725     C*
025600020725     C                   enddo
025700020725     C*
025800020725     C/EXEC SQL
025900020725     C+ close C1
026000020725     C/END-EXEC
026100000905     C*
026200020305     C* Scarico i VAB rimasti "in sospeso"
026300071009     C                   if        wDISK = 'C'
026400111130     C  N31              eval      fivabds_ok = fivabds
026500111130     C  N31              exsr      wrivab
026600071009     C                   endif
026700010202     C*
026800010202     C                   endif
026900990910
027000990910     C* Se non ci sono record con errori ...
027100000710     C                   if        §ctrno = 0
027200990910     C* ... restituisco esito OK.
027300990921     C                   eval      wrkesito = '0'
027400990910     C                   else
027500010201     C                   if        §ctrokvb > 0
027600990921     C                   eval      wrkesito = '1'
027700000710     C                   else
027800000710     C                   eval      wrkesito = '2'
027900990910     C                   endif
028000000710     C                   endif
028100990910     C*
028200990914     C                   if        %open(tivin00r)
028300990908     C                   close     tivin00r
028400990914     C                   endif
028500120706     C                   if        %open(edivabwr)
028600120706     C                   close     edivabwr
028700120706     C                   endif
028800120706     C                   if        %open(edivatwr)
028900120706     C                   close     edivatwr
029000010201     C                   endif
029100990910     C*
029200111031     C                   if        §ctrokvb > 0 or
029300111031     C                             §ctrokvt > 0
029400111031     C                             and wPOI <> *zeros
029500010202     C                   exsr      invio
029600990920     C                   endif
029700990920     C*
029800910830     C                   ENDSR
029900000613     C***
030000010305
030100010305     C*----------------------------------------------------*
030200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030300010305     C*----------------------------------------------------*
030400020305     C     WRIVAB        BEGSR
030500070730     C*
030600071003     C* Considerazioni finali
030700111125     C                   if        ok_VABRMA = *blanks
030800111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030900071003     C                   endif
031000120706     C*
031100120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
031200160701     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
031300120709     C                   EVAL      ok_VABDCM = datcor
031400120709     C                   EVAL      ok_VABDTS = datcor
031500120709     C                   EVAL      ok_VABHMS = oracor
031600120709     C                   EVAL      ok_VABCNT = 1
031700071003     C*
031800120706     C                   write     edivab00                                     => scarico il VAB
031900010305     C*
032000010305     C                   ENDSR
032100990920
032200000801     C*----------------------------------------------------*
032300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032400000801     C*----------------------------------------------------*
032500010201     C     INZVAR        BEGSR
032600000801     C*
032700010201     C                   Z-ADD     *zeros        Num5_0
032800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032900020725     C                   MOVEL     '0'           FlgCAS            1
033000111125     C*
033100111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033200111130     C*
033300111125     C                   CLEAR                   FIVABDS
033400111130     C                   CLEAR                   FIVABDS_OK
033500111130     C                   CLEAR                   FIVABDS_SAV
033600120706     C                   CLEAR                   EDIVAB00
033700120706     C                   CLEAR                   EDIVAT00
033800000801     C*
033900000801     C                   ENDSR
034000000801     C*----------------------------------------------------*
034100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034200000801     C*----------------------------------------------------*
034300020904     C     DEFCAM        BEGSR
034400080923     C*
034500111125     C                   SETOFF                                       50
034600111125     C                   SETOFF                                       515253
034700070730     C* Imposto i valori di default...
034800170310     C                   EVAL      VABCCM = 0507097
034900170310     C                   EVAL      VATCCM = 0507097
035000170310     C                   EVAL      VABLNP = 050
035100170310     C                   EVAL      VATLNP = 050
035200160701     C                   EVAL      VABCTR = 000
035300160701     C                   EVAL      VABCBO = '1'
035400070222     C* ... e poi verifico se sono stati passati come parametri
035500070222     C                   IF        vlrppt > *blanks
035600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035800070222     C                   EXSR      CHKNUM
035900070222     C                   IF        PiInt=*on
036000070222     C                   Z-ADD     PiVal         VABCCM
036100070222     C                   Z-ADD     PiVal         VATCCM
036200070222     C                   ENDIF
036300070222     C                   ENDIF
036400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036600070222     C                   EXSR      CHKNUM
036700070222     C                   IF        PiInt=*on
036800070222     C                   Z-ADD     PiVal         VABLNP
036900070222     C                   Z-ADD     PiVal         VATLNP
037000070222     C                   ENDIF
037100070222     C                   ENDIF
037200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037400070222     C                   EXSR      CHKNUM
037500070222     C                   IF        PiInt=*on
037600070222     C                   Z-ADD     PiVal         VABCTR
037700070222     C                   ENDIF
037800070928     C                   ENDIF
037900071009     C                   MOVEL     *blanks       wDISK             1
038000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038100071009     C                   IF        wDISK <> *blanks
038200070928     C                   SETON                                        50
038300070222     C                   ENDIF
038400080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038500111125     C                   SETON                                        41
038600080923     C                   ENDIF
038700070222     C                   ENDIF
038800120706     C*
038900120706     C* valido per nr.colli/peso/volume
039000120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039100120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039200120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039300120706     C                   SELECT
039400120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039500120706     C                   SETON                                        51
039600120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039700120706     C                   SETON                                        52
039800120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039900120706     C                   SETON                                        53
040000120706     C                   ENDSL
040100120706     C*
040200071009     C   50              EVAL      VABCTM = '7Q'
040300000801     C*
040400000801     C                   ENDSR
040500150527     C*----------------------------------------------------*
040600150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040700150527     C*----------------------------------------------------*
040800150527     C     REPNSP        BEGSR
040900150527     C*
041000150527     C                   EXSR      INZVAR
041100150527     C                   EXSR      DEFCAM
041200150527     C*
041300150527     C                   SETON                                        60
041400150527     C*
041500150527     C* NSP => Stacco un numeratore da AZNUM
041600150527     C                   clear                   TRUL33DS
041700150527     C                   eval      I33OPE = *zeros
041800150527     C                   eval      I33CNU = 302
041900150527     C                   eval      I33NUM = 1
042000150527     C                   movel     TRUL33DS      KPJBU
042100150527     C                   call      'TRUL33R'
042200150527     C                   parm                    KPJBA
042300150527     C                   movel     KPJBU         TRUL33DS
042400150527     C                   if        O33ERR = *zeros
042500150527     C                   z-add     O33NRF        VABNSP
042600150527     C                   z-add     O33NRF        VATNSP
042700150527     C                   else
042800150527     C                   SETON                                        31
042900150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043000150527     C                             + ' ' + 'VABNSP VATNSP'
043100150527     C                   endif
043200150527     C*
043300150527     C                   ENDSR
043400000801     C*----------------------------------------------------*
043500120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
043600000801     C*----------------------------------------------------*
043700010201     C     IMPVAB        BEGSR
043800070530     C*
043900070530     C                   SETOFF                                       3132
044000070928     C*
044100070928     C                   EXSR      DEFCAM
044200070928     C*
044300111031     C                   IF        vlrpoi <> 999
044400070928     C                   MOVE(P)   vlrpoi        VABFGS
044500070928     C                   MOVE(P)   vlrpoi        VATFGS
044600111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044700111031     C                   ENDIF
044800070928     C*
044900070928     C                   MOVEL     datcor        VABAAS
045000070928     C                   MOVEL     datcor        VATAAS
045100070928     C                   MOVE      datcor        VABMGS
045200170310     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
045300070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045400070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045500070928     C* ==
045600170310     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
045700170310     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
045800170310     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
045900170310     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
046000160701
046100071003     C* CAD
046200170310     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
046300070928     C                   EXSR      CHKNUM
046400070928     C                   IF        PiInt=*on
046500070928     C                   Z-ADD     PiVal         Num5_0
046600070928     C                   MOVEL     Num5_0        VABCAD
046700070928     C                   ELSE
046800070928     C                   SETON                                        32
046900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047000070928     C                             + ' ' + 'VABCAD'
047100070928     C                   ENDIF
047200121113     C*
047300121113     C* Se provincia nn valorizzata la reperisco
047400121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
047500121113     C                   IF        VABPRD = *blanks
047600121113     C                   CLEAR                   TISI95DS
047700121113     C                   EVAL      I95TCN = '3'
047800121113     C                   Z-ADD     datcor        I95DAT
047900121113     C                   EVAL      I95NAR = VABNZD
048000121113     C                   EVAL      I95CAP = VABCAD
048100121113     C                   EVAL      I95LOC = VABLOD
048200121113     C                   CALL      'TISI95R'
048300121113     C                   PARM                    TISI95DS
048400121113     C                   EVAL      VABPRD = O95PRV
048500121113     C                   ENDIF
048600121113     C* RMN
048700170310     C                   EVAL      PiStr=%trim(%subst(vindta:187:7))
048800121113     C                   EXSR      CHKNUM
048900121113     C                   IF        PiInt=*on
049000121113     C                   Z-ADD     PiVal         VABRMN
049100121113     C                   ELSE
049200121113     C                   SETON                                        32
049300121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049400121113     C                             + ' ' + 'VABRMN'
049500121113     C                   ENDIF
049600121113     C* NCL
049700170310     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
049800121113     C                   EXSR      CHKNUM
049900121113     C                   IF        PiInt=*on
050000121113     C                   Z-ADD     PiVal         VABNCL
050100121113     C                   ELSE
050200121113     C                   SETON                                        32
050300121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050400121113     C                             + ' ' + 'VABNCL'
050500121113     C                   ENDIF
050600121113     C* PKB
050700170310     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
050800121113     C                   EXSR      CHKNUM
050900121113     C                   IF        PiNum=*on
051000170310     C                   Z-ADD     PiVal         VABPKB
051100121113     C                   ELSE
051200121113     C                   SETON                                        32
051300121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051400121113     C                             + ' ' + 'VABPKB'
051500121113     C                   ENDIF
051600170310     C* CAS
051700170310     C                   IF        %subst(vindta:280:12) <> *blanks and
051800170310     C                             %subst(vindta:280:12) <> *zeros  and
051900170310     C                             %subst(vindta:280:12) <> '000000000.00'
052000170310     C                   MOVEL     '1'           FlgCAS
052100170310     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
052200170310     C                   EVAL      VABVCA='EUR'
052300170310     C                   EXSR      CHKNUM
052400170310     C                   IF        PiNum=*on
052500170310     C                   Z-ADD     PiVal         VABCAS
052600170310     C                   ELSE
052700170310     C                   SETON                                        32
052800170310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900170310     C                             + ' ' + 'VABCAS'
053000170310     C                   ENDIF
053100170310     C                   ENDIF
053200170310     C* IAS
053300170310     C                   IF        %subst(vindta:295:12) <> *blanks and
053400170310     C                             %subst(vindta:295:12) <> *zeros  and
053500170310     C                             %subst(vindta:295:12) <> '000000000.00'
053600170310     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
053700170310     C                   EVAL      VABVAS='EUR'
053800170310     C                   EXSR      CHKNUM
053900170310     C                   IF        PiNum=*on
054000170310     C                   Z-ADD     PiVal         VABIAS
054100170310     C                   ELSE
054200170310     C                   SETON                                        32
054300170310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054400170310     C                             + ' ' + 'VABIAS'
054500170310     C                   ENDIF
054600170310     C                   ENDIF
054700170310
054800070730     C*
054900070730     C* Considerazioni finali su CBO/CAS
055000121113     C                   IF        FlgCAS <> '0'
055100070730     C                   IF        VABCBO = '1'
055200070730     C                   EVAL      VABCBO = '4'
055300070730     C                   ENDIF
055400070730     C                   IF        VABCBO = '2'
055500070730     C                   EVAL      VABCBO = '6'
055600070730     C                   ENDIF
055700070730     C                   ENDIF
055800020305     C*
055900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
056000011113     C                   EXSR      CHKIMPDIV
056100010202     C*
056200000801     C* Ebbene...
056300000801     C                   ADD       1             §CTRMO
056400070530     C                   IF        *in31 <> *zeros OR
056500070530     C                             *in32 <> *zeros
056600000801     C                   ADD       1             §CTRNO
056700020725     C                   EVAL      x_vinflg = '2'
056800000801     C                   ELSE
056900010201     C                   ADD       1             §CTROKVB
057000000801     C                   ENDIF
057100000801     C*
057200000801     C                   ENDSR
057300010201     C*----------------------------------------------------*
057400120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
057500010201     C*----------------------------------------------------*
057600170310     C     WRIVAT_A      BEGSR
057700090204     C*
057800090204     C                   eval      vatAAS = vabAAS
057900090204     C                   eval      vatLNP = vabLNP
058000090204     C                   eval      vatNRS = vabNRS
058100090204     C                   eval      vatNSP = vabNSP
058200010201     C*
058300120706     C* Valorizzo il buffer di scrittura del EDIVAT
058400170310     C* - TIPO RECORD "A"
058500170310     C                   if        %subst(vindta:92:15) <> *blanks
058600120706     C*
058700120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
058800170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
058900120706     C                   EVAL      VATCNT = 1
059000120706     C*
059100170310     C                   eval      VATTRC = 'A'
059200170310     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
059300150527     C                   write     EDIVAT00
059400111031     C                   add       1             §ctrokvt
059500150527     C                   endif
059600010201     C*
059700010201     C                   ENDSR
059800150527     C*----------------------------------------------------*
059900150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "S"
060000150527     C*----------------------------------------------------*
060100170310     C     WRIVAT_P      BEGSR
060200150527     C*
060300150527     C                   eval      vatAAS = vabAAS
060400150527     C                   eval      vatLNP = vabLNP
060500150527     C                   eval      vatNRS = vabNRS
060600150527     C                   eval      vatNSP = vabNSP
060700150527     C*
060800150527     C* Valorizzo il buffer di scrittura del EDIVAT
060900170310     C* - TIPO RECORD "P"
061000170310     C                   if        %subst(vindta:187:10) <> *blanks
061100150527     C*
061200150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
061300170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
061400150527     C                   EVAL      VATCNT = 1
061500150527     C*
061600170310     C                   eval      VATTRC = 'P'
061700170310     C                   eval      VATNOT = %trim(%subst(vindta:187:10))  +
061800170310     C                             '.pdf'
061900170310     C                   eval      %subst(VATNOT:34:2)='10'
062000150527     C                   write     EDIVAT00
062100150527     C                   add       1             §ctrokvt
062200150527     C                   endif
062300150527     C*
062400150527     C                   ENDSR
062500150527     C*----------------------------------------------------*
062600150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
062700150527     C*----------------------------------------------------*
062800150527     C     WRIVAT_I      BEGSR
062900150527     C*
063000150527     C                   eval      vatAAS = vabAAS
063100150527     C                   eval      vatLNP = vabLNP
063200150527     C                   eval      vatNRS = vabNRS
063300150527     C                   eval      vatNSP = vabNSP
063400150527     C*
063500150527     C* Valorizzo il buffer di scrittura del EDIVAT
063600150527     C* - TIPO RECORD "I" e "J"
063700170310     C                   if        %subst(vindta:412:70) <> *blanks
063800170310     C                   eval      w70 = %trim(%subst(vindta:412:70))
063900150527     C*
064000150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064100170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
064200150527     C                   EVAL      VATCNT = 1
064300150527     C*
064400150527     C                   eval      VATTRC = 'I'
064500150527     C                   eval      VATNOT = %subst(w70:1:35)
064600150527     C                   write     EDIVAT00
064700150527     C                   add       1             §ctrokvt
064800150527     C*
064900150527     C                   IF        %subst(w70:36:35) <> *blank
065000150527     C                   eval      VATTRC = 'J'
065100150527     C                   eval      VATNOT = %subst(w70:36:35)
065200150527     C                   write     EDIVAT00
065300150527     C                   add       1             §ctrokvt
065400150527     C                   endif
065500150527     C                   endif
065600150527     C*
065700150527     C                   ENDSR
065800071003     C*----------------------------------------------------*
065900120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
066000071003     C*----------------------------------------------------*
066100071003     C     WRIVAT_E      BEGSR
066200090204     C*
066300090204     C                   eval      vatAAS = vabAAS
066400090204     C                   eval      vatLNP = vabLNP
066500090204     C                   eval      vatNRS = vabNRS
066600090204     C                   eval      vatNSP = vabNSP
066700071003     C*
066800120706     C* Valorizzo il buffer di scrittura del EDIVAT
066900071003     C* - TIPO RECORD "E"
067000170310     C                   if        %subst(vindta:331:26) <> *blanks
067100120706     C*
067200120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
067300170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
067400120706     C                   EVAL      VATCNT = 1
067500120706     C*
067600071003     C                   eval      VATTRC = 'E'
067700170310     C                   eval      vatNOT = %trim(%subst(vindta:331:26))
067800120706     C                   write     EDIVAT00
067900111031     C                   add       1             §ctrokvt
068000150528     C                   endif
068100071003     C*
068200071003     C                   ENDSR
068300020904
068400020904
068500010202     C*----------------------------------------------------*
068600120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
068700010202     C*----------------------------------------------------*
068800020305     C     PREVAT        BEGSR
068900010202     C*
069000120706     C* Compongo il nome del membro da dare al EDIVATWR
069100010202     C                   eval      parmbr = vlrhdl
069200010202     C                   movel     'M'           parmbr
069300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069400010202     C                   eval      paropz = '1'
069500010202     C* Effettuo la chiamata al CLLE preposto
069600120706     C                   call(e)   'TITVEVTC'
069700010202     C                   parm                    parccm
069800010202     C                   parm                    parmbr
069900010202     C                   parm                    paropz
070000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070100010202     C                   if        %error
070200010202     C                   movel     '1'           chkcall
070300010202     C                   else
070400010202     C                   movel     '0'           chkcall
070500010202     C                   endif
070600010202     C*
070700010202     C                   ENDSR
070800000801     C*----------------------------------------------------*
070900000801     C*  CONTROLLO NUMERICITA' CAMPI
071000000801     C*----------------------------------------------------*
071100000801     C     CHKNUM        BEGSR
071200081113     C*
071300081113     C                   IF        PiDecChr = *blanks
071400121113     C                   EVAL      PiDecChr = '.'
071500081113     C                   ENDIF
071600081113     C*
071700081113     C                   callp(e)  UBISNUM_Check(PiStr
071800081113     C                                          :PiDecChr
071900081113     C                                          :PiVal
072000081113     C                                          :PiNum
072100081113     C                                          :PiInt)
072200081113     C*
072300000801     C                   IF        %error
072400000801     C                   EVAL      PiInt=*off
072500000801     C                   ENDIF
072600000801     C*
072700000801     C                   ENDSR
072800000801     C***
072900000801
073000011113
073100011113     C*----------------------------------------------------*
073200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
073300011113     C*----------------------------------------------------*
073400011113     C     CHKIMPDIV     BEGSR
073500011113     C*
073600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
073700011113     C                   Z-ADD     *zeros        wrkDec            9 9
073800011113     C*
073900011113     C* Come prima cosa effettuo considerazioni sulla divisa
074000011113     C                   IF        vabIAS > *zeros
074100011113     C                   IF        vabVAS <> 'EUR'
074200011113     C                   EVAL      vabVAS =  'ITL'
074300011113     C                   ENDIF
074400011113     C                   ENDIF
074500011113     C*
074600011113     C                   IF        vabCAS > *zeros
074700011113     C                   IF        vabVCA <> 'EUR'
074800011113     C                   EVAL      vabVCA =  'ITL'
074900011113     C                   ENDIF
075000011113     C                   ENDIF
075100011113     C*
075200011113     C                   IF        vabVMD > *zeros
075300020305     C                   IF        vabVAD <> 'EUR'
075400011113     C                   EVAL      vabVAD =  'ITL'
075500011113     C                   ENDIF
075600011113     C                   ENDIF
075700011113     C*
075800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
075900011113     C                   Z-ADD     vabIAS        wrkDec
076000011113     C                   IF        wrkDec > *zeros
076100011113     C                   IF        vabVAS = 'ITL'
076200011113     C                   EVAL      vabIAS = *zeros
076300011113     C                   ENDIF
076400011113     C                   ENDIF
076500011113     C*
076600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
076700011113     C                   Z-ADD     vabCAS        wrkDec
076800011113     C                   IF        wrkDec > *zeros
076900011113     C                   IF        vabVCA = 'ITL'
077000011113     C                   EVAL      vabCAS = *zeros
077100011113     C                   ENDIF
077200011113     C                   ENDIF
077300011113     C*
077400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
077500011113     C                   Z-ADD     vabVMD        wrkDec
077600011113     C                   IF        wrkDec > *zeros
077700011113     C                   IF        vabVAD = 'ITL'
077800011113     C                   EVAL      vabVMD = *zeros
077900011113     C                   ENDIF
078000011113     C                   ENDIF
078100011113     C*
078200011113     C                   ENDSR
078300011113     C***
078400011113
078500011113
078600000801
078700000801
078800990920      /TITLE Invio dei dati al punto operativo.
078900010202     C     invio         BEGSR
079000990920     C*
079100120706     C* 1° invio EDIVAT
079200010201     C                   reset                   dscmz
079300111031     C                   move      wPOI          cmzdst
079400120706     C                   eval      cmzfld = 'EDIVATWR'
079500010201     C                   eval      cmzmbd = vlrhdl
079600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079700021009     C***                if        prmfir = *blanks
079800120706     C                   eval      cmzfla = 'EDIVAT0F'
079900120706     C                   eval      cmzmba = 'EDIVAT0F'
080000021009     C***                else
080100021009     C***                eval      cmzfla = prmfir
080200021009     C***                eval      cmzmba = prmfir
080300021009     C***                endif
080400010201     C                   eval      cmznrr = *zeros
080500020305     C                   move      §ctrokvt      cmznrr
080600021018     C                   eval      cmzlba = vlrfl1
080700010201     C                   call(e)   'TIS711C'
080800010201     C                   parm                    dscmz
080900010201     C                   parm      *blanks       esito
081000010205     C                   if        %error
081100010205     C                             or cmzerr = '1'
081200010205     C                             or esito  = '1'
081300010205     C                   eval      wrkesito = '3'
081400010205     C                   else
081500010201     C*
081600120706     C* 2° invio EDIVAB
081700010201     C                   reset                   dscmz
081800111031     C                   move      wPOI          cmzdst
081900010201     C                   eval      cmzfld = vlrfou
082000010201     C                   eval      cmzmbd = vlrhdl
082100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082200021009     C***                if        prmfir = *blanks
082300120712     C                   eval      cmzfla = 'EDIVAB0F'
082400120712     C                   eval      cmzmba = 'EDIVAB0F'
082500021009     C***                else
082600021009     C***                eval      cmzfla = prmfir
082700021009     C***                eval      cmzmba = prmfir
082800021009     C***                endif
082900010201     C                   eval      cmznrr = *zeros
083000010201     C                   move      §ctrokvb      cmznrr
083100021018     C                   eval      cmzlba = vlrfl1
083200010201     C                   call(e)   'TIS711C'
083300010201     C                   parm                    dscmz
083400010201     C                   parm      *blanks       esito
083500010201     C                   if        %error
083600010201     C                             or cmzerr = '1'
083700010201     C                             or esito  = '1'
083800010201     C                   eval      wrkesito = '3'
083900010201     C                   endif
084000010205     C                   endif
084100990920     C*
084200000613     C                   ENDSR
084300000613     C***
084400070411
084500070411     C     *pssr         BEGSR
084600070411     C*
084700070411     C                   if        %open(tivin00r)
084800070411     C                   close     tivin00r
084900070411     C                   endif
085000120706     C                   if        %open(edivabwr)
085100120706     C                   close     edivabwr
085200120706     C                   endif
085300120706     C                   if        %open(edivatwr)
085400120706     C                   close     edivatwr
085500070411     C                   endif
085600070411     C*
085700070411     C* Effettuo la chiamata al CLLE preposto
085800120706     C                   call(e)   'TITVEVTC'
085900070411     C                   parm                    parccm
086000070411     C                   parm                    parmbr
086100070411     C                   parm      '2'           paropz
086200070411     C*
086300070411     C                   eval      wrkesito = '2'
086400070411     C*
086500070411     C                   seton                                        LR
086600070411     C*
086700070411     C                   ENDSR     '*CANCL'
086800070411     C***
086900070411
087000990910
087100000613     C     *inzsr        BEGSR
087200990910     C*
087300990910     C     *entry        plist
087400990920     C                   parm                    tivlrds
087500990921     C                   parm      wrkesito      esito
087600000724     C                   parm                    prmlit
087700000710     C                   parm                    prmfir
087800000613     C*
087900000830     C* CALCOLA LA DATA CORRENTE
088000120706     C                   time                    wn14             14 0
088100120706     C                   movel     wn14          oracor            6 0          *ORA
088200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088300100324     C                   eval      datcor = %dec(%date() : *ISO)
088400000830     C*
088500000613     C                   ENDSR
088600000613     C***
