000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400150527      *
000500171005      * - disk A
000600171005      * - tutte spedizioni in porto franco senza c/assegno
000700171005      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000900990908
001000020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001100120709     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001200120706     FEDIVATwr  O    E             DISK    usropn
001300990908
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100070730     D tisi95ds      e ds
002200120706     D fivabds       e ds                  extname(EDIVAB0f)
002300120706     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
002400120706     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
002500990910     D esito           s              1
002600000724     D prmlit          s             10
002700000710     D prmfir          s             10
002800990921     D wrkesito        s                   like(esito)
002900000613     D rrnum           s              6  0 INZ(*zeros)
003000171005     D depspe          s              6    INZ(*blanks)
003100171005     D curspe          s              6    INZ(*blanks)
003200010202     D parccm          s              8    INZ(*blanks)
003300010202     D parmbr          s             10    INZ(*blanks)
003400010202     D paropz          s              1    INZ(*blanks)
003500010202     D chkcall         s              1    INZ(*blanks)
003600080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003700070530     D Num5_0          s              5  0
003800120903     D wDivPKB         s                   like(VABNCL)
003900150527     D w70             s             70
004000170316     D w4              s              4
004100000830
004200020725
004300020725     D*------------------
004400020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004500020725     D*------------------
004600070530     D INPUT_DS        DS                  INZ
004700101007     D  VINDTA                     2048
004800101007     D  VINFLG                        1
004900171005     D  VINSPE                        6
005000101007     D  VINRRN                        8  0
005100020725     D*
005200080923     D*------------------
005300080923     D* DS REPERIMENTO NUMERATORE
005400080923     D*------------------
005500080923     D trul33ds      e ds                  inz
005600150528     D*------------------
005700150528     D TRUL28DSX     e ds                  inz
005800080923     D*------------------
005900080923     D* DS ARCHITETTURA
006000080923     D*------------------
006100080923     D kpjba         e ds                  inz
006200080923     D*------------------
006300121113     D  Num7_0         s              7  0
006400081113
006500081113     D*------------------
006600081113     D* LINKING A DEFINIZIONI ESTERNE
006700081113     D*------------------
006800100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006900081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000081113
007100990908
007200010201
007300010201
007400080117     C*
007500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600080117     C
007700080117     C/EXEC SQL
007800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900080117     C/END-EXEC
008000080117     C
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400000613     C*
008500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008600000613     C*
008700010202     C* Effettuo la chiamata al CLLE preposto
008800120706     C                   call(e)   'TITVEVTC'
008900010202     C                   parm                    parccm
009000010202     C                   parm                    parmbr
009100010202     C                   parm      '2'           paropz
009200070730     C*
009300070730     C* Effettuo lancio TISI95 solo x chiusura
009400070730     C                   CLEAR                   TISI95DS
009500070730     C                   EVAL      I95TLA = 'C'
009600070730     C                   CALL      'TISI95R'
009700070730     C                   PARM                    TISI95DS
009800000616     C*
009900000801     C
010000010201     C                   seton                                        LR
010100990908
010200000801
010300910830     C*--------------------------------------------------------
010400120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
010500910830     C*--------------------------------------------------------
010600070530     C     RWFILE        BEGSR
010700990910     C*
010800990914     C                   if        not %open(tivin00r)
010900990908     C                   open      tivin00r
011000990914     C                   endif
011100120706     C                   if        not %open(edivabwr)
011200120706     C                   open      edivabwr
011300990914     C                   endif
011400070530     C*
011500120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011600020305     C                   exsr      prevat
011700010201     C*
011800010202     C                   if        chkcall = '0'
011900010202     C*
012000120706     C                   if        not %open(edivatwr)
012100120706     C                   open      edivatwr
012200010201     C                   endif
012300080117     C*
012400080117     C                   EXSR      INZVAR
012500080117     C                   EXSR      DEFCAM
012600990910     C*
012700010201     C                   clear                   §CTROKVB          5 0
012800020305     C                   clear                   §CTROKVT          5 0
012900000801     C                   clear                   §CTRMO            5 0
013000000801     C                   clear                   §CTRNO            5 0
013100990910     C*
013200020725     C/EXEC SQL
013300020725     C+ declare C1 cursor for select
013400170316     C+ vindta, vinflg, substr(vindta, 1 , 15) as sped, rrn(tivin00r)
013500060223     C+ from tivin00r
013600060223     C+ order by sped
013700020725     C+ for read only
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100020725     C+ open C1
014200020725     C/END-EXEC
014300020725     C
014400020725     C/EXEC SQL
014500070530     C+ Fetch C1 into :INPUT_DS
014600020725     C/END-EXEC
014700020725     C*
014800020725     C                   dow       sqlcod = *zeros
014900990913     C*
015000020725     C                   if        vindta > *blanks
015100000613     C                   add       1             rrnum
015200000801     C*
015300020725     C                   if        vinflg = *blanks
015400020725     C                             or vinflg = '0'
015500020725     C                             or vinflg = '2'
015600000801     C*
015700020725     C                   clear                   x_vinmsg
015800020725     C                   eval      x_vinflg = '1'
015900010305     C*
016000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016100171005     C                   EVAL      PiStr=%trim(%subst(vindta:113:6))
016200020305     C                   MOVEL(p)  PiStr         curspe
016300010305     C*
016400071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016500080923     C                   if        *in50 = *off
016600170310     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione
016700171005     C* MA in questo caso sì
016800171005     C                   exsr      repNSP
016900071003     C                   exsr      impvab
017000111130     C  N31              eval      fivabds_ok = fivabds
017100111130     C  N31              exsr      wrivab
017200171005     C***                exsr      wrivat_b                                     => carico VAT
017300171005     C***                exsr      wrivat_ij                                    => carico VAT
017400111125     C                   exsr      inzvar
017500071003     C                   else
017600080923     C*
017700071009     C                   if        wDISK = 'D'
017800150527     C                   exsr      repNSP
017900071009     C                   exsr      impvab
018000111130     C  N31              eval      fivabds_ok = fivabds
018100111130     C  N31              exsr      wrivab
018200171005     C***                exsr      wrivat_b                                     => carico VAT
018300171005     C***                exsr      wrivat_ij                                    => carico VAT
018400171005     C***                exsr      wrivat_e                                     => carico VAT
018500111125     C                   exsr      inzvar
018600071009     C                   else
018700071009     C*
018800010305     C                   if        depspe = *blanks                             => 1° giro
018900010305     C                   eval      depspe = curspe                              => memorizz. spediz
019000080117     C                   clear                   tivinds
019100150527     C                   exsr      repNSP
019200020305     C                   exsr      impvab
019300111130     C                   z-add     VABNCL        sav_VABNCL
019400111130     C                   z-add     VABPKB        sav_VABPKB
019500111130     C                   z-add     VABVLB        sav_VABVLB
019600171005     C***                exsr      wrivat_b                                     => carico VAT
019700171005     C***                exsr      wrivat_ij                                    => carico VAT
019800171005     C***50              exsr      wrivat_e                                     => carico VAT
019900010305     C                   else
020000020725     C                   if        depspe <> curspe                             => rottura di spediz
020100111130     C                   z-add     sav_VABNCL    VABNCL
020200111130     C                   z-add     sav_VABPKB    VABPKB
020300111130     C                   z-add     sav_VABVLB    VABVLB
020400111130     C  N31              eval      fivabds_ok = fivabds
020500111125     C  N31              exsr      wrivab
020600111125     C                   exsr      inzvar
020700111125     C                   eval      depspe = curspe                              => memorizz. spediz
020800080117     C                   clear                   tivinds
020900150527     C                   exsr      repNSP
021000020305     C                   exsr      impvab
021100111130     C                   if        wCntRecSpe = *zeros
021200111130     C                   z-add     VABNCL        sav_VABNCL
021300111130     C                   z-add     VABPKB        sav_VABPKB
021400111130     C                   z-add     VABVLB        sav_VABVLB
021500111130     C                   endif
021600111130     C                   add       1             wCntRecSpe
021700171005     C***                exsr      wrivat_b                                     => carico VAT
021800171005     C***                exsr      wrivat_ij                                    => carico VAT
021900171005     C***50              exsr      wrivat_e                                     => carico VAT
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
023200171005     C***                exsr      wrivat_b                                     => carico VAT
023300171005     C***                exsr      wrivat_ij                                    => carico VAT
023400171005     C***50              exsr      wrivat_e                                     => carico VAT
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
033000170316     C                   clear                   w70
033100170316     C                   clear                   w4
033200111125     C*
033300111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033400111130     C*
033500111125     C                   CLEAR                   FIVABDS
033600111130     C                   CLEAR                   FIVABDS_OK
033700111130     C                   CLEAR                   FIVABDS_SAV
033800120706     C                   CLEAR                   EDIVAB00
033900120706     C                   CLEAR                   EDIVAT00
034000000801     C*
034100000801     C                   ENDSR
034200000801     C*----------------------------------------------------*
034300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034400000801     C*----------------------------------------------------*
034500020904     C     DEFCAM        BEGSR
034600080923     C*
034700111125     C                   SETOFF                                       50
034800111125     C                   SETOFF                                       515253
034900070730     C* Imposto i valori di default...
035000171005     C                   Z-ADD     1412251       VABCCM
035100171005     C                   Z-ADD     1412251       VATCCM
035200171005     C                   Z-ADD     141           VABLNP
035300171005     C                   Z-ADD     141           VATLNP
035400171005     C                   Z-ADD     000           VABCTR
035500171005     C                   MOVEL     '1'           VABCBO
035600171005     C                   MOVEL     '7K'          VABCTM
035700171005     C                   MOVEL     'C'           VABTSP
035800070222     C* ... e poi verifico se sono stati passati come parametri
035900070222     C                   IF        vlrppt > *blanks
036000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABCCM
036500070222     C                   Z-ADD     PiVal         VATCCM
036600070222     C                   ENDIF
036700070222     C                   ENDIF
036800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037000070222     C                   EXSR      CHKNUM
037100070222     C                   IF        PiInt=*on
037200070222     C                   Z-ADD     PiVal         VABLNP
037300070222     C                   Z-ADD     PiVal         VATLNP
037400070222     C                   ENDIF
037500070222     C                   ENDIF
037600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037800070222     C                   EXSR      CHKNUM
037900070222     C                   IF        PiInt=*on
038000070222     C                   Z-ADD     PiVal         VABCTR
038100070222     C                   ENDIF
038200070928     C                   ENDIF
038300071009     C                   MOVEL     *blanks       wDISK             1
038400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038500071009     C                   IF        wDISK <> *blanks
038600070928     C                   SETON                                        50
038700070222     C                   ENDIF
038800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038900111125     C                   SETON                                        41
039000080923     C                   ENDIF
039100070222     C                   ENDIF
039200120706     C*
039300120706     C* valido per nr.colli/peso/volume
039400120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039500120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039600120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039700120706     C                   SELECT
039800120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039900120706     C                   SETON                                        51
040000120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
040100120706     C                   SETON                                        52
040200120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040300120706     C                   SETON                                        53
040400120706     C                   ENDSL
040500120706     C*
040600071009     C   50              EVAL      VABCTM = '7Q'
040700000801     C*
040800000801     C                   ENDSR
040900150527     C*----------------------------------------------------*
041000150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041100150527     C*----------------------------------------------------*
041200150527     C     REPNSP        BEGSR
041300150527     C*
041400150527     C                   EXSR      INZVAR
041500150527     C                   EXSR      DEFCAM
041600150527     C*
041700150527     C                   SETON                                        60
041800150527     C*
041900150527     C* NSP => Stacco un numeratore da AZNUM
042000150527     C                   clear                   TRUL33DS
042100150527     C                   eval      I33OPE = *zeros
042200150527     C                   eval      I33CNU = 302
042300150527     C                   eval      I33NUM = 1
042400150527     C                   movel     TRUL33DS      KPJBU
042500150527     C                   call      'TRUL33R'
042600150527     C                   parm                    KPJBA
042700150527     C                   movel     KPJBU         TRUL33DS
042800150527     C                   if        O33ERR = *zeros
042900150527     C                   z-add     O33NRF        VABNSP
043000150527     C                   z-add     O33NRF        VATNSP
043100150527     C                   else
043200150527     C                   SETON                                        31
043300150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043400150527     C                             + ' ' + 'VABNSP VATNSP'
043500150527     C                   endif
043600150527     C*
043700150527     C                   ENDSR
043800000801     C*----------------------------------------------------*
043900120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
044000000801     C*----------------------------------------------------*
044100010201     C     IMPVAB        BEGSR
044200070530     C*
044300070530     C                   SETOFF                                       3132
044400070928     C*
044500070928     C                   EXSR      DEFCAM
044600070928     C*
044700111031     C                   IF        vlrpoi <> 999
044800070928     C                   MOVE(P)   vlrpoi        VABFGS
044900070928     C                   MOVE(P)   vlrpoi        VATFGS
045000111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045100111031     C                   ENDIF
045200070928     C*
045300070928     C                   MOVEL     datcor        VABAAS
045400070928     C                   MOVEL     datcor        VATAAS
045500070928     C                   MOVE      datcor        VABMGS
045600170316     C                   CLEAR                   w70
045700171005     C                   EVAL      w70=%trim(%subst(vindta:1:35))
045800170316     C                   EVAL      VABRSD=%trim(%subst(w70:1:35))
045900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
046100070928     C* ==
046200171005     C***                EVAL      VABRD2=%trim(%subst(w70:36:35))
046300171005     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
046400171005     C                   EVAL      VABLOD=%trim(%subst(vindta:71:25))
046500171005     C                   EVAL      VABPRD=%trim(%subst(vindta:105:2))
046600171005     C                   EVAL      VABRMA=%trim(%subst(vindta:107:6))
046700171005
046800071003     C* CAD
046900171005     C* siccome non pare che il cliente faccia estero, lo gestisco numerico
047000171005     C***                EVAL      VABCAD=%trim(%subst(vindta:96:9))
047100171005     C                   EVAL      PiStr=%trim(%subst(vindta:96:9))
047200171005     C                   EXSR      CHKNUM
047300171005     C                   IF        PiInt=*on
047400171005     C                   Z-ADD     PiVal         Num5_0
047500171005     C                   MOVEL     Num5_0        VABCAD
047600171005     C                   ELSE
047700171005     C                   SETON                                        32
047800171005     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047900171005     C                             + ' ' + 'VABCAD'
048000171005     C                   ENDIF
048100121113     C*
048200121113     C* Se provincia nn valorizzata la reperisco
048300121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
048400121113     C                   IF        VABPRD = *blanks
048500121113     C                   CLEAR                   TISI95DS
048600121113     C                   EVAL      I95TCN = '3'
048700121113     C                   Z-ADD     datcor        I95DAT
048800121113     C                   EVAL      I95NAR = VABNZD
048900121113     C                   EVAL      I95CAP = VABCAD
049000121113     C                   EVAL      I95LOC = VABLOD
049100121113     C                   CALL      'TISI95R'
049200121113     C                   PARM                    TISI95DS
049300121113     C                   EVAL      VABPRD = O95PRV
049400121113     C                   ENDIF
049500121113     C* RMN
049600171005     C                   EVAL      PiStr=%trim(%subst(vindta:113:6))
049700121113     C                   EXSR      CHKNUM
049800121113     C                   IF        PiInt=*on
049900121113     C                   Z-ADD     PiVal         VABRMN
050000121113     C                   ELSE
050100121113     C                   SETON                                        32
050200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300121113     C                             + ' ' + 'VABRMN'
050400121113     C                   ENDIF
050500121113     C* NCL
050600171005     C                   EVAL      PiStr=%trim(%subst(vindta:127:6))
050700121113     C                   EXSR      CHKNUM
050800121113     C                   IF        PiInt=*on
050900121113     C                   Z-ADD     PiVal         VABNCL
051000121113     C                   ELSE
051100121113     C                   SETON                                        32
051200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300121113     C                             + ' ' + 'VABNCL'
051400121113     C                   ENDIF
051500121113     C* PKB
051600171005     C                   EVAL      PiStr=%trim(%subst(vindta:133:9))
051700121113     C                   EXSR      CHKNUM
051800121113     C                   IF        PiNum=*on
051900171005     C* non ci sono decimali
052000171005     C                   EVAL(h)   VABPKB = PiVal
052100121113     C                   ELSE
052200121113     C                   SETON                                        32
052300121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052400121113     C                             + ' ' + 'VABPKB'
052500121113     C                   ENDIF
052600171005     C* VLB
052700171005     C                   EVAL      PiStr=%trim(%subst(vindta:142:15))
052800171005     C                   EXSR      CHKNUM
052900171005     C                   IF        PiNum=*on
053000171005     C* ci sono 3 decimali
053100171006     C                   EVAL(h)   VABVLB = PiVal/1000
053200171005     C                   ELSE
053300171005     C                   SETON                                        32
053400171005     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053500171005     C                             + ' ' + 'VABPKB'
053600171005     C                   ENDIF
053700170310
053800070730     C*
053900070730     C* Considerazioni finali su CBO/CAS
054000121113     C                   IF        FlgCAS <> '0'
054100070730     C                   IF        VABCBO = '1'
054200070730     C                   EVAL      VABCBO = '4'
054300070730     C                   ENDIF
054400070730     C                   IF        VABCBO = '2'
054500070730     C                   EVAL      VABCBO = '6'
054600070730     C                   ENDIF
054700070730     C                   ENDIF
054800020305     C*
054900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055000011113     C                   EXSR      CHKIMPDIV
055100010202     C*
055200000801     C* Ebbene...
055300000801     C                   ADD       1             §CTRMO
055400070530     C                   IF        *in31 <> *zeros OR
055500070530     C                             *in32 <> *zeros
055600000801     C                   ADD       1             §CTRNO
055700020725     C                   EVAL      x_vinflg = '2'
055800000801     C                   ELSE
055900010201     C                   ADD       1             §CTROKVB
056000000801     C                   ENDIF
056100000801     C*
056200000801     C                   ENDSR
056300010201     C*----------------------------------------------------*
056400120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
056500010201     C*----------------------------------------------------*
056600170316     C     WRIVAT_B      BEGSR
056700090204     C*
056800090204     C                   eval      vatAAS = vabAAS
056900090204     C                   eval      vatLNP = vabLNP
057000090204     C                   eval      vatNRS = vabNRS
057100090204     C                   eval      vatNSP = vabNSP
057200010201     C*
057300120706     C* Valorizzo il buffer di scrittura del EDIVAT
057400170316     C* - TIPO RECORD "B"
057500170316     C                   if        %subst(vindta:344:40) <> *blanks
057600120706     C*
057700120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
057800170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
057900120706     C                   EVAL      VATCNT = 1
058000120706     C*
058100170316     C                   eval      VATTRC = 'B'
058200170316     C                   eval      VATNOT = %trim(%subst(vindta:344:40))
058300150527     C                   write     EDIVAT00
058400111031     C                   add       1             §ctrokvt
058500150527     C                   endif
058600010201     C*
058700010201     C                   ENDSR
058800150527     C*----------------------------------------------------*
058900150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
059000150527     C*----------------------------------------------------*
059100170316     C     WRIVAT_IJ     BEGSR
059200150527     C*
059300150527     C                   eval      vatAAS = vabAAS
059400150527     C                   eval      vatLNP = vabLNP
059500150527     C                   eval      vatNRS = vabNRS
059600150527     C                   eval      vatNSP = vabNSP
059700150527     C*
059800150527     C* Valorizzo il buffer di scrittura del EDIVAT
059900150527     C* - TIPO RECORD "I" e "J"
060000170316     C                   if        %subst(vindta:599:60) <> *blanks
060100170316     C                   clear                   w70
060200170316     C                   eval      w70 = %trim(%subst(vindta:599:60))
060300150527     C*
060400150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
060500170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
060600150527     C                   EVAL      VATCNT = 1
060700150527     C*
060800150527     C                   eval      VATTRC = 'I'
060900150527     C                   eval      VATNOT = %subst(w70:1:35)
061000150527     C                   write     EDIVAT00
061100150527     C                   add       1             §ctrokvt
061200150527     C*
061300150527     C                   IF        %subst(w70:36:35) <> *blank
061400150527     C                   eval      VATTRC = 'J'
061500150527     C                   eval      VATNOT = %subst(w70:36:35)
061600150527     C                   write     EDIVAT00
061700150527     C                   add       1             §ctrokvt
061800150527     C                   endif
061900150527     C                   endif
062000150527     C*
062100150527     C                   ENDSR
062200071003     C*----------------------------------------------------*
062300120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
062400071003     C*----------------------------------------------------*
062500071003     C     WRIVAT_E      BEGSR
062600090204     C*
062700090204     C                   eval      vatAAS = vabAAS
062800090204     C                   eval      vatLNP = vabLNP
062900090204     C                   eval      vatNRS = vabNRS
063000090204     C                   eval      vatNSP = vabNSP
063100071003     C*
063200120706     C* Valorizzo il buffer di scrittura del EDIVAT
063300071003     C* - TIPO RECORD "E"
063400170316     C                   if        %subst(vindta:539:60) <> *blanks
063500120706     C*
063600120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
063700170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
063800120706     C                   EVAL      VATCNT = 1
063900120706     C*
064000071003     C                   eval      VATTRC = 'E'
064100170316     C                   eval      vatNOT = %trim(%subst(vindta:539:60))
064200120706     C                   write     EDIVAT00
064300111031     C                   add       1             §ctrokvt
064400150528     C                   endif
064500071003     C*
064600071003     C                   ENDSR
064700020904
064800020904
064900010202     C*----------------------------------------------------*
065000120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
065100010202     C*----------------------------------------------------*
065200020305     C     PREVAT        BEGSR
065300010202     C*
065400120706     C* Compongo il nome del membro da dare al EDIVATWR
065500010202     C                   eval      parmbr = vlrhdl
065600010202     C                   movel     'M'           parmbr
065700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065800010202     C                   eval      paropz = '1'
065900010202     C* Effettuo la chiamata al CLLE preposto
066000120706     C                   call(e)   'TITVEVTC'
066100010202     C                   parm                    parccm
066200010202     C                   parm                    parmbr
066300010202     C                   parm                    paropz
066400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066500010202     C                   if        %error
066600010202     C                   movel     '1'           chkcall
066700010202     C                   else
066800010202     C                   movel     '0'           chkcall
066900010202     C                   endif
067000010202     C*
067100010202     C                   ENDSR
067200000801     C*----------------------------------------------------*
067300000801     C*  CONTROLLO NUMERICITA' CAMPI
067400000801     C*----------------------------------------------------*
067500000801     C     CHKNUM        BEGSR
067600081113     C*
067700081113     C                   IF        PiDecChr = *blanks
067800121113     C                   EVAL      PiDecChr = '.'
067900081113     C                   ENDIF
068000081113     C*
068100081113     C                   callp(e)  UBISNUM_Check(PiStr
068200081113     C                                          :PiDecChr
068300081113     C                                          :PiVal
068400081113     C                                          :PiNum
068500081113     C                                          :PiInt)
068600081113     C*
068700000801     C                   IF        %error
068800000801     C                   EVAL      PiInt=*off
068900000801     C                   ENDIF
069000000801     C*
069100000801     C                   ENDSR
069200000801     C***
069300000801
069400011113
069500011113     C*----------------------------------------------------*
069600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069700011113     C*----------------------------------------------------*
069800011113     C     CHKIMPDIV     BEGSR
069900011113     C*
070000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070100011113     C                   Z-ADD     *zeros        wrkDec            9 9
070200011113     C*
070300011113     C* Come prima cosa effettuo considerazioni sulla divisa
070400011113     C                   IF        vabIAS > *zeros
070500011113     C                   IF        vabVAS <> 'EUR'
070600011113     C                   EVAL      vabVAS =  'ITL'
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C                   IF        vabCAS > *zeros
071100011113     C                   IF        vabVCA <> 'EUR'
071200011113     C                   EVAL      vabVCA =  'ITL'
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C                   IF        vabVMD > *zeros
071700020305     C                   IF        vabVAD <> 'EUR'
071800011113     C                   EVAL      vabVAD =  'ITL'
071900011113     C                   ENDIF
072000011113     C                   ENDIF
072100011113     C*
072200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072300011113     C                   Z-ADD     vabIAS        wrkDec
072400011113     C                   IF        wrkDec > *zeros
072500011113     C                   IF        vabVAS = 'ITL'
072600011113     C                   EVAL      vabIAS = *zeros
072700011113     C                   ENDIF
072800011113     C                   ENDIF
072900011113     C*
073000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073100011113     C                   Z-ADD     vabCAS        wrkDec
073200011113     C                   IF        wrkDec > *zeros
073300011113     C                   IF        vabVCA = 'ITL'
073400011113     C                   EVAL      vabCAS = *zeros
073500011113     C                   ENDIF
073600011113     C                   ENDIF
073700011113     C*
073800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073900011113     C                   Z-ADD     vabVMD        wrkDec
074000011113     C                   IF        wrkDec > *zeros
074100011113     C                   IF        vabVAD = 'ITL'
074200011113     C                   EVAL      vabVMD = *zeros
074300011113     C                   ENDIF
074400011113     C                   ENDIF
074500011113     C*
074600011113     C                   ENDSR
074700011113     C***
074800011113
074900011113
075000000801
075100000801
075200990920      /TITLE Invio dei dati al punto operativo.
075300010202     C     invio         BEGSR
075400990920     C*
075500120706     C* 1° invio EDIVAT
075600010201     C                   reset                   dscmz
075700111031     C                   move      wPOI          cmzdst
075800120706     C                   eval      cmzfld = 'EDIVATWR'
075900010201     C                   eval      cmzmbd = vlrhdl
076000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076100021009     C***                if        prmfir = *blanks
076200120706     C                   eval      cmzfla = 'EDIVAT0F'
076300120706     C                   eval      cmzmba = 'EDIVAT0F'
076400021009     C***                else
076500021009     C***                eval      cmzfla = prmfir
076600021009     C***                eval      cmzmba = prmfir
076700021009     C***                endif
076800010201     C                   eval      cmznrr = *zeros
076900020305     C                   move      §ctrokvt      cmznrr
077000021018     C                   eval      cmzlba = vlrfl1
077100010201     C                   call(e)   'TIS711C'
077200010201     C                   parm                    dscmz
077300010201     C                   parm      *blanks       esito
077400010205     C                   if        %error
077500010205     C                             or cmzerr = '1'
077600010205     C                             or esito  = '1'
077700010205     C                   eval      wrkesito = '3'
077800010205     C                   else
077900010201     C*
078000120706     C* 2° invio EDIVAB
078100010201     C                   reset                   dscmz
078200111031     C                   move      wPOI          cmzdst
078300010201     C                   eval      cmzfld = vlrfou
078400010201     C                   eval      cmzmbd = vlrhdl
078500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078600021009     C***                if        prmfir = *blanks
078700120712     C                   eval      cmzfla = 'EDIVAB0F'
078800120712     C                   eval      cmzmba = 'EDIVAB0F'
078900021009     C***                else
079000021009     C***                eval      cmzfla = prmfir
079100021009     C***                eval      cmzmba = prmfir
079200021009     C***                endif
079300010201     C                   eval      cmznrr = *zeros
079400010201     C                   move      §ctrokvb      cmznrr
079500021018     C                   eval      cmzlba = vlrfl1
079600010201     C                   call(e)   'TIS711C'
079700010201     C                   parm                    dscmz
079800010201     C                   parm      *blanks       esito
079900010201     C                   if        %error
080000010201     C                             or cmzerr = '1'
080100010201     C                             or esito  = '1'
080200010201     C                   eval      wrkesito = '3'
080300010201     C                   endif
080400010205     C                   endif
080500990920     C*
080600000613     C                   ENDSR
080700000613     C***
080800070411
080900070411     C     *pssr         BEGSR
081000070411     C*
081100070411     C                   if        %open(tivin00r)
081200070411     C                   close     tivin00r
081300070411     C                   endif
081400120706     C                   if        %open(edivabwr)
081500120706     C                   close     edivabwr
081600120706     C                   endif
081700120706     C                   if        %open(edivatwr)
081800120706     C                   close     edivatwr
081900070411     C                   endif
082000070411     C*
082100070411     C* Effettuo la chiamata al CLLE preposto
082200120706     C                   call(e)   'TITVEVTC'
082300070411     C                   parm                    parccm
082400070411     C                   parm                    parmbr
082500070411     C                   parm      '2'           paropz
082600070411     C*
082700070411     C                   eval      wrkesito = '2'
082800070411     C*
082900070411     C                   seton                                        LR
083000070411     C*
083100070411     C                   ENDSR     '*CANCL'
083200070411     C***
083300070411
083400990910
083500000613     C     *inzsr        BEGSR
083600990910     C*
083700990910     C     *entry        plist
083800990920     C                   parm                    tivlrds
083900990921     C                   parm      wrkesito      esito
084000000724     C                   parm                    prmlit
084100000710     C                   parm                    prmfir
084200000613     C*
084300000830     C* CALCOLA LA DATA CORRENTE
084400120706     C                   time                    wn14             14 0
084500120706     C                   movel     wn14          oracor            6 0          *ORA
084600100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084700100324     C                   eval      datcor = %dec(%date() : *ISO)
084800000830     C*
084900000613     C                   ENDSR
085000000613     C***
