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
002700170316     D depspe          s             15    INZ(*blanks)
002800170316     D curspe          s             15    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500120903     D wDivPKB         s                   like(VABNCL)
003600150527     D w70             s             70
003700170316     D w4              s              4
003800000830
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400101007     D  VINDTA                     2048
004500101007     D  VINFLG                        1
004600170316     D  VINSPE                       15
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
013100170316     C+ vindta, vinflg, substr(vindta, 1 , 15) as sped, rrn(tivin00r)
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
015800170316     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
015900020305     C                   MOVEL(p)  PiStr         curspe
016000010305     C*
016100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016200080923     C                   if        *in50 = *off
016300170310     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione
016400170316     C***                exsr      repNSP
016500071003     C                   exsr      impvab
016600111130     C  N31              eval      fivabds_ok = fivabds
016700111130     C  N31              exsr      wrivab
016800170316     C                   exsr      wrivat_b                                     => carico VAT
016900170316     C                   exsr      wrivat_ij                                    => carico VAT
017000111125     C                   exsr      inzvar
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400150527     C                   exsr      repNSP
017500071009     C                   exsr      impvab
017600111130     C  N31              eval      fivabds_ok = fivabds
017700111130     C  N31              exsr      wrivab
017800170316     C                   exsr      wrivat_b                                     => carico VAT
017900170316     C                   exsr      wrivat_ij                                    => carico VAT
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
019200170316     C                   exsr      wrivat_b                                     => carico VAT
019300170316     C                   exsr      wrivat_ij                                    => carico VAT
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
021300170316     C                   exsr      wrivat_b                                     => carico VAT
021400170316     C                   exsr      wrivat_ij                                    => carico VAT
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
022800170316     C                   exsr      wrivat_b                                     => carico VAT
022900170316     C                   exsr      wrivat_ij                                    => carico VAT
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
032600170316     C                   clear                   w70
032700170316     C                   clear                   w4
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
034600170316     C                   EVAL      VABCCM = 0495923
034700170316     C                   EVAL      VATCCM = 0495923
034800170316     C                   EVAL      VABLNP = 049
034900170316     C                   EVAL      VATLNP = 049
035000170316     C                   EVAL      VABCTR = 100
035100160701     C                   EVAL      VABCBO = '1'
035200170316     C                   EVAL      VABCTM = '7Q'
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
045100170316     C                   CLEAR                   w70
045200170316     C                   EVAL      w70=%trim(%subst(vindta:24:60))
045300170316     C                   EVAL      VABRSD=%trim(%subst(w70:1:35))
045400070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045500070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045600070928     C* ==
045700170316     C                   EVAL      VABRD2=%trim(%subst(w70:36:35))
045800170316     C                   EVAL      VABIND=%trim(%subst(vindta:84:60))
045900170316     C                   EVAL      VABLOD=%trim(%subst(vindta:154:60))
046000170316     C                   EVAL      VABPRD=%trim(%subst(vindta:214:10))
046100170316     C                   EVAL      VABNZD=%trim(%subst(vindta:224:40))
046200170316     C                   EVAL      VABNAS=%trim(%subst(vindta:264:60))
046300170316     C                   EVAL      VABRMA=%trim(%subst(vindta:424:30))
046400170316     C                   CLEAR                   w70
046500170316     C                   EVAL      w70=%trim(%subst(vindta:469:60))
046600170316     C                   EVAL      VABNOT=%trim(%subst(w70:1:35))
046700170316     C                   EVAL      VABNT2=%trim(%subst(w70:36:35))
046800170316     C                   IF        %subst(vindta:659:60) <> *blank
046900170316     C                   EVAL      w4 = %trim(%subst(vindta:659:60))
047000170316     C                   EVAL      VABGC1=%subst(w4:1:2)
047100170316     C                   EVAL      VABGC2=%subst(w4:3:2)
047200170316     C                   ENDIF
047300160701
047400071003     C* CAD
047500170316     C* facendo estero lo gestisco alfabetico
047600170316     C                   EVAL      VABCAD=%trim(%subst(vindta:144:10))
047700170316     C***                EVAL      PiStr=%trim(%subst(vindta:107:5))
047800170316     C***                EXSR      CHKNUM
047900170316     C***                IF        PiInt=*on
048000170316     C***                Z-ADD     PiVal         Num5_0
048100170316     C***                MOVEL     Num5_0        VABCAD
048200170316     C***                ELSE
048300170316     C***                SETON                                        32
048400170316     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
048500170316     C***                          + ' ' + 'VABCAD'
048600170316     C***                ENDIF
048700121113     C*
048800121113     C* Se provincia nn valorizzata la reperisco
048900121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
049000121113     C                   IF        VABPRD = *blanks
049100121113     C                   CLEAR                   TISI95DS
049200121113     C                   EVAL      I95TCN = '3'
049300121113     C                   Z-ADD     datcor        I95DAT
049400121113     C                   EVAL      I95NAR = VABNZD
049500121113     C                   EVAL      I95CAP = VABCAD
049600121113     C                   EVAL      I95LOC = VABLOD
049700121113     C                   CALL      'TISI95R'
049800121113     C                   PARM                    TISI95DS
049900121113     C                   EVAL      VABPRD = O95PRV
050000121113     C                   ENDIF
050100121113     C* RMN
050200170316     C                   EVAL      PiStr=%trim(%subst(vindta:425:29))
050300121113     C                   EXSR      CHKNUM
050400121113     C                   IF        PiInt=*on
050500121113     C                   Z-ADD     PiVal         VABRMN
050600121113     C                   ELSE
050700121113     C                   SETON                                        32
050800121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050900121113     C                             + ' ' + 'VABRMN'
051000121113     C                   ENDIF
051100121113     C* NCL
051200170316     C                   EVAL      PiStr=%trim(%subst(vindta:324:10))
051300121113     C                   EXSR      CHKNUM
051400121113     C                   IF        PiInt=*on
051500121113     C                   Z-ADD     PiVal         VABNCL
051600121113     C                   ELSE
051700121113     C                   SETON                                        32
051800121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051900121113     C                             + ' ' + 'VABNCL'
052000121113     C                   ENDIF
052100121113     C* PKB
052200170316     C                   EVAL      PiStr=%trim(%subst(vindta:334:10))
052300121113     C                   EXSR      CHKNUM
052400121113     C                   IF        PiNum=*on
052500170316     C                   EVAL(h)   VABPKB = PiVal/1000
052600121113     C                   ELSE
052700121113     C                   SETON                                        32
052800121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900121113     C                             + ' ' + 'VABPKB'
053000121113     C                   ENDIF
053100170310     C* CAS
053200170316     C                   IF        %subst(vindta:454:15) <> *blanks and
053300170316     C                             %subst(vindta:454:15) <> *zeros
053400170310     C                   MOVEL     '1'           FlgCAS
053500170316     C                   EVAL      PiStr=%trim(%subst(vindta:454:15))
053600170310     C                   EVAL      VABVCA='EUR'
053700170407     C                   EVAL      VABTIC='TM'
053800170310     C                   EXSR      CHKNUM
053900170310     C                   IF        PiNum=*on
054000170316     C                   EVAL(h)   VABCAS = PiVal/100
054100170310     C                   ELSE
054200170310     C                   SETON                                        32
054300170310     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054400170310     C                             + ' ' + 'VABCAS'
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
057600170316     C     WRIVAT_B      BEGSR
057700090204     C*
057800090204     C                   eval      vatAAS = vabAAS
057900090204     C                   eval      vatLNP = vabLNP
058000090204     C                   eval      vatNRS = vabNRS
058100090204     C                   eval      vatNSP = vabNSP
058200010201     C*
058300120706     C* Valorizzo il buffer di scrittura del EDIVAT
058400170316     C* - TIPO RECORD "B"
058500170316     C                   if        %subst(vindta:344:40) <> *blanks
058600120706     C*
058700120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
058800170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
058900120706     C                   EVAL      VATCNT = 1
059000120706     C*
059100170316     C                   eval      VATTRC = 'B'
059200170316     C                   eval      VATNOT = %trim(%subst(vindta:344:40))
059300150527     C                   write     EDIVAT00
059400111031     C                   add       1             §ctrokvt
059500150527     C                   endif
059600010201     C*
059700010201     C                   ENDSR
059800150527     C*----------------------------------------------------*
059900150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
060000150527     C*----------------------------------------------------*
060100170316     C     WRIVAT_IJ     BEGSR
060200150527     C*
060300150527     C                   eval      vatAAS = vabAAS
060400150527     C                   eval      vatLNP = vabLNP
060500150527     C                   eval      vatNRS = vabNRS
060600150527     C                   eval      vatNSP = vabNSP
060700150527     C*
060800150527     C* Valorizzo il buffer di scrittura del EDIVAT
060900150527     C* - TIPO RECORD "I" e "J"
061000170316     C                   if        %subst(vindta:599:60) <> *blanks
061100170316     C                   clear                   w70
061200170316     C                   eval      w70 = %trim(%subst(vindta:599:60))
061300150527     C*
061400150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
061500170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
061600150527     C                   EVAL      VATCNT = 1
061700150527     C*
061800150527     C                   eval      VATTRC = 'I'
061900150527     C                   eval      VATNOT = %subst(w70:1:35)
062000150527     C                   write     EDIVAT00
062100150527     C                   add       1             §ctrokvt
062200150527     C*
062300150527     C                   IF        %subst(w70:36:35) <> *blank
062400150527     C                   eval      VATTRC = 'J'
062500150527     C                   eval      VATNOT = %subst(w70:36:35)
062600150527     C                   write     EDIVAT00
062700150527     C                   add       1             §ctrokvt
062800150527     C                   endif
062900150527     C                   endif
063000150527     C*
063100150527     C                   ENDSR
063200071003     C*----------------------------------------------------*
063300120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
063400071003     C*----------------------------------------------------*
063500071003     C     WRIVAT_E      BEGSR
063600090204     C*
063700090204     C                   eval      vatAAS = vabAAS
063800090204     C                   eval      vatLNP = vabLNP
063900090204     C                   eval      vatNRS = vabNRS
064000090204     C                   eval      vatNSP = vabNSP
064100071003     C*
064200120706     C* Valorizzo il buffer di scrittura del EDIVAT
064300071003     C* - TIPO RECORD "E"
064400170316     C                   if        %subst(vindta:539:60) <> *blanks
064500120706     C*
064600120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064700170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
064800120706     C                   EVAL      VATCNT = 1
064900120706     C*
065000071003     C                   eval      VATTRC = 'E'
065100170316     C                   eval      vatNOT = %trim(%subst(vindta:539:60))
065200120706     C                   write     EDIVAT00
065300111031     C                   add       1             §ctrokvt
065400150528     C                   endif
065500071003     C*
065600071003     C                   ENDSR
065700020904
065800020904
065900010202     C*----------------------------------------------------*
066000120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
066100010202     C*----------------------------------------------------*
066200020305     C     PREVAT        BEGSR
066300010202     C*
066400120706     C* Compongo il nome del membro da dare al EDIVATWR
066500010202     C                   eval      parmbr = vlrhdl
066600010202     C                   movel     'M'           parmbr
066700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066800010202     C                   eval      paropz = '1'
066900010202     C* Effettuo la chiamata al CLLE preposto
067000120706     C                   call(e)   'TITVEVTC'
067100010202     C                   parm                    parccm
067200010202     C                   parm                    parmbr
067300010202     C                   parm                    paropz
067400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067500010202     C                   if        %error
067600010202     C                   movel     '1'           chkcall
067700010202     C                   else
067800010202     C                   movel     '0'           chkcall
067900010202     C                   endif
068000010202     C*
068100010202     C                   ENDSR
068200000801     C*----------------------------------------------------*
068300000801     C*  CONTROLLO NUMERICITA' CAMPI
068400000801     C*----------------------------------------------------*
068500000801     C     CHKNUM        BEGSR
068600081113     C*
068700081113     C                   IF        PiDecChr = *blanks
068800121113     C                   EVAL      PiDecChr = '.'
068900081113     C                   ENDIF
069000081113     C*
069100081113     C                   callp(e)  UBISNUM_Check(PiStr
069200081113     C                                          :PiDecChr
069300081113     C                                          :PiVal
069400081113     C                                          :PiNum
069500081113     C                                          :PiInt)
069600081113     C*
069700000801     C                   IF        %error
069800000801     C                   EVAL      PiInt=*off
069900000801     C                   ENDIF
070000000801     C*
070100000801     C                   ENDSR
070200000801     C***
070300000801
070400011113
070500011113     C*----------------------------------------------------*
070600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070700011113     C*----------------------------------------------------*
070800011113     C     CHKIMPDIV     BEGSR
070900011113     C*
071000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
071100011113     C                   Z-ADD     *zeros        wrkDec            9 9
071200011113     C*
071300011113     C* Come prima cosa effettuo considerazioni sulla divisa
071400011113     C                   IF        vabIAS > *zeros
071500011113     C                   IF        vabVAS <> 'EUR'
071600011113     C                   EVAL      vabVAS =  'ITL'
071700011113     C                   ENDIF
071800011113     C                   ENDIF
071900011113     C*
072000011113     C                   IF        vabCAS > *zeros
072100011113     C                   IF        vabVCA <> 'EUR'
072200011113     C                   EVAL      vabVCA =  'ITL'
072300011113     C                   ENDIF
072400011113     C                   ENDIF
072500011113     C*
072600011113     C                   IF        vabVMD > *zeros
072700020305     C                   IF        vabVAD <> 'EUR'
072800011113     C                   EVAL      vabVAD =  'ITL'
072900011113     C                   ENDIF
073000011113     C                   ENDIF
073100011113     C*
073200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
073300011113     C                   Z-ADD     vabIAS        wrkDec
073400011113     C                   IF        wrkDec > *zeros
073500011113     C                   IF        vabVAS = 'ITL'
073600011113     C                   EVAL      vabIAS = *zeros
073700011113     C                   ENDIF
073800011113     C                   ENDIF
073900011113     C*
074000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
074100011113     C                   Z-ADD     vabCAS        wrkDec
074200011113     C                   IF        wrkDec > *zeros
074300011113     C                   IF        vabVCA = 'ITL'
074400011113     C                   EVAL      vabCAS = *zeros
074500011113     C                   ENDIF
074600011113     C                   ENDIF
074700011113     C*
074800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074900011113     C                   Z-ADD     vabVMD        wrkDec
075000011113     C                   IF        wrkDec > *zeros
075100011113     C                   IF        vabVAD = 'ITL'
075200011113     C                   EVAL      vabVMD = *zeros
075300011113     C                   ENDIF
075400011113     C                   ENDIF
075500011113     C*
075600011113     C                   ENDSR
075700011113     C***
075800011113
075900011113
076000000801
076100000801
076200990920      /TITLE Invio dei dati al punto operativo.
076300010202     C     invio         BEGSR
076400990920     C*
076500120706     C* 1° invio EDIVAT
076600010201     C                   reset                   dscmz
076700111031     C                   move      wPOI          cmzdst
076800120706     C                   eval      cmzfld = 'EDIVATWR'
076900010201     C                   eval      cmzmbd = vlrhdl
077000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077100021009     C***                if        prmfir = *blanks
077200120706     C                   eval      cmzfla = 'EDIVAT0F'
077300120706     C                   eval      cmzmba = 'EDIVAT0F'
077400021009     C***                else
077500021009     C***                eval      cmzfla = prmfir
077600021009     C***                eval      cmzmba = prmfir
077700021009     C***                endif
077800010201     C                   eval      cmznrr = *zeros
077900020305     C                   move      §ctrokvt      cmznrr
078000021018     C                   eval      cmzlba = vlrfl1
078100010201     C                   call(e)   'TIS711C'
078200010201     C                   parm                    dscmz
078300010201     C                   parm      *blanks       esito
078400010205     C                   if        %error
078500010205     C                             or cmzerr = '1'
078600010205     C                             or esito  = '1'
078700010205     C                   eval      wrkesito = '3'
078800010205     C                   else
078900010201     C*
079000120706     C* 2° invio EDIVAB
079100010201     C                   reset                   dscmz
079200111031     C                   move      wPOI          cmzdst
079300010201     C                   eval      cmzfld = vlrfou
079400010201     C                   eval      cmzmbd = vlrhdl
079500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079600021009     C***                if        prmfir = *blanks
079700120712     C                   eval      cmzfla = 'EDIVAB0F'
079800120712     C                   eval      cmzmba = 'EDIVAB0F'
079900021009     C***                else
080000021009     C***                eval      cmzfla = prmfir
080100021009     C***                eval      cmzmba = prmfir
080200021009     C***                endif
080300010201     C                   eval      cmznrr = *zeros
080400010201     C                   move      §ctrokvb      cmznrr
080500021018     C                   eval      cmzlba = vlrfl1
080600010201     C                   call(e)   'TIS711C'
080700010201     C                   parm                    dscmz
080800010201     C                   parm      *blanks       esito
080900010201     C                   if        %error
081000010201     C                             or cmzerr = '1'
081100010201     C                             or esito  = '1'
081200010201     C                   eval      wrkesito = '3'
081300010201     C                   endif
081400010205     C                   endif
081500990920     C*
081600000613     C                   ENDSR
081700000613     C***
081800070411
081900070411     C     *pssr         BEGSR
082000070411     C*
082100070411     C                   if        %open(tivin00r)
082200070411     C                   close     tivin00r
082300070411     C                   endif
082400120706     C                   if        %open(edivabwr)
082500120706     C                   close     edivabwr
082600120706     C                   endif
082700120706     C                   if        %open(edivatwr)
082800120706     C                   close     edivatwr
082900070411     C                   endif
083000070411     C*
083100070411     C* Effettuo la chiamata al CLLE preposto
083200120706     C                   call(e)   'TITVEVTC'
083300070411     C                   parm                    parccm
083400070411     C                   parm                    parmbr
083500070411     C                   parm      '2'           paropz
083600070411     C*
083700070411     C                   eval      wrkesito = '2'
083800070411     C*
083900070411     C                   seton                                        LR
084000070411     C*
084100070411     C                   ENDSR     '*CANCL'
084200070411     C***
084300070411
084400990910
084500000613     C     *inzsr        BEGSR
084600990910     C*
084700990910     C     *entry        plist
084800990920     C                   parm                    tivlrds
084900990921     C                   parm      wrkesito      esito
085000000724     C                   parm                    prmlit
085100000710     C                   parm                    prmfir
085200000613     C*
085300000830     C* CALCOLA LA DATA CORRENTE
085400120706     C                   time                    wn14             14 0
085500120706     C                   movel     wn14          oracor            6 0          *ORA
085600100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085700100324     C                   eval      datcor = %dec(%date() : *ISO)
085800000830     C*
085900000613     C                   ENDSR
086000000613     C***
