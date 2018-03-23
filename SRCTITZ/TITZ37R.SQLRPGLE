000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400120903      * Il nr.spedizione mi viene passato per cui asterisco la routine REPNSP per il disk C con
000500120903      * N rcd per spedizione.
000600120903      * Il riferimento numerico viene preso come il rif.num.alfanumerico ma partendo 2 char dopo per
000700120903      * saltare la parte alfabetica.
000800120903      * I colli vanno sommati riga per riga della stessa spedizione, ma ogni riga è composta da 1
000900120903      * collo.
001000120903      * Il peso non è quello indicato sul file ma <quello indicato su file>/<colli indicati su file>
001100121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001200990908
001300020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001400120709     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001500120706     FEDIVATwr  O    E             DISK    usropn
001600990908
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400070730     D tisi95ds      e ds
002500120706     D fivabds       e ds                  extname(EDIVAB0f)
002600120706     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
002700120706     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
002800990910     D esito           s              1
002900000724     D prmlit          s             10
003000000710     D prmfir          s             10
003100990921     D wrkesito        s                   like(esito)
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300120706     D depspe          s             10    INZ(*blanks)
003400120706     D curspe          s             10    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004000070530     D Num5_0          s              5  0
004100120903     D wDivPKB         s                   like(VABNCL)
004200000830
004300020725
004400020725     D*------------------
004500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004600020725     D*------------------
004700070530     D INPUT_DS        DS                  INZ
004800101007     D  VINDTA                     2048
004900101007     D  VINFLG                        1
005000110524     D  VINSPE                        8
005100101007     D  VINRRN                        8  0
005200020725     D*
005300080923     D*------------------
005400080923     D* DS REPERIMENTO NUMERATORE
005500080923     D*------------------
005600080923     D trul33ds      e ds                  inz
005700080923     D*------------------
005800080923     D* DS ARCHITETTURA
005900080923     D*------------------
006000080923     D kpjba         e ds                  inz
006100080923     D*------------------
006200081113
006300081113     D*------------------
006400081113     D* LINKING A DEFINIZIONI ESTERNE
006500081113     D*------------------
006600100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006800081113
006900990908
007000010201
007100010201
007200080117     C*
007300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400080117     C
007500080117     C/EXEC SQL
007600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700080117     C/END-EXEC
007800080117     C
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200000613     C*
008300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008400000613     C*
008500010202     C* Effettuo la chiamata al CLLE preposto
008600120706     C                   call(e)   'TITVEVTC'
008700010202     C                   parm                    parccm
008800010202     C                   parm                    parmbr
008900010202     C                   parm      '2'           paropz
009000070730     C*
009100070730     C* Effettuo lancio TISI95 solo x chiusura
009200070730     C                   CLEAR                   TISI95DS
009300070730     C                   EVAL      I95TLA = 'C'
009400070730     C                   CALL      'TISI95R'
009500070730     C                   PARM                    TISI95DS
009600000616     C*
009700000801     C
009800010201     C                   seton                                        LR
009900990908
010000000801
010100910830     C*--------------------------------------------------------
010200120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
010300910830     C*--------------------------------------------------------
010400070530     C     RWFILE        BEGSR
010500990910     C*
010600990914     C                   if        not %open(tivin00r)
010700990908     C                   open      tivin00r
010800990914     C                   endif
010900120706     C                   if        not %open(edivabwr)
011000120706     C                   open      edivabwr
011100990914     C                   endif
011200070530     C*
011300120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011400020305     C                   exsr      prevat
011500010201     C*
011600010202     C                   if        chkcall = '0'
011700010202     C*
011800120706     C                   if        not %open(edivatwr)
011900120706     C                   open      edivatwr
012000010201     C                   endif
012100080117     C*
012200080117     C                   EXSR      INZVAR
012300080117     C                   EXSR      DEFCAM
012400990910     C*
012500010201     C                   clear                   §CTROKVB          5 0
012600020305     C                   clear                   §CTROKVT          5 0
012700000801     C                   clear                   §CTRMO            5 0
012800000801     C                   clear                   §CTRNO            5 0
012900990910     C*
013000020725     C/EXEC SQL
013100020725     C+ declare C1 cursor for select
013200120903     C+ vindta, vinflg, substr(vindta, 241, 10) as sped, rrn(tivin00r)
013300060223     C+ from tivin00r
013400060223     C+ order by sped
013500020725     C+ for read only
013600020725     C/END-EXEC
013700020725     C
013800020725     C/EXEC SQL
013900020725     C+ open C1
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300070530     C+ Fetch C1 into :INPUT_DS
014400020725     C/END-EXEC
014500020725     C*
014600020725     C                   dow       sqlcod = *zeros
014700990913     C*
014800020725     C                   if        vindta > *blanks
014900000613     C                   add       1             rrnum
015000000801     C*
015100020725     C                   if        vinflg = *blanks
015200020725     C                             or vinflg = '0'
015300020725     C                             or vinflg = '2'
015400000801     C*
015500020725     C                   clear                   x_vinmsg
015600020725     C                   eval      x_vinflg = '1'
015700010305     C*
015800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015900120903     C                   EVAL      PiStr=%trim(%subst(vindta:241:10))
016000020305     C                   MOVEL(p)  PiStr         curspe
016100010305     C*
016200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016300080923     C                   if        *in50 = *off
016400071003     C                   exsr      impvab
016500111130     C  N31              eval      fivabds_ok = fivabds
016600111130     C  N31              exsr      wrivab
016700071003     C                   exsr      wrivat_b                                     => carico VAT
016800111125     C                   exsr      inzvar
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200071009     C                   exsr      impvab
017300111130     C  N31              eval      fivabds_ok = fivabds
017400111130     C  N31              exsr      wrivab
017500071009     C                   exsr      wrivat_b                                     => carico VAT
017600071009     C                   exsr      wrivat_e                                     => carico VAT
017700111125     C                   exsr      inzvar
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300020305     C                   exsr      impvab
018400111130     C                   z-add     VABNCL        sav_VABNCL
018500111130     C                   z-add     VABPKB        sav_VABPKB
018600111130     C                   z-add     VABVLB        sav_VABVLB
018700130917     C                   z-add     VABCAS        sav_VABCAS
018800071003     C                   exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   else
019100020725     C                   if        depspe <> curspe                             => rottura di spediz
019200111130     C                   z-add     sav_VABNCL    VABNCL
019300111130     C                   z-add     sav_VABPKB    VABPKB
019400111130     C                   z-add     sav_VABVLB    VABVLB
019500130917     C                   z-add     sav_VABCAS    VABCAS
019600111130     C  N31              eval      fivabds_ok = fivabds
019700111125     C  N31              exsr      wrivab
019800111125     C                   exsr      inzvar
019900111125     C                   eval      depspe = curspe                              => memorizz. spediz
020000080117     C                   clear                   tivinds
020100020305     C                   exsr      impvab
020200111130     C                   if        wCntRecSpe = *zeros
020300111130     C                   z-add     VABNCL        sav_VABNCL
020400111130     C                   z-add     VABPKB        sav_VABPKB
020500111130     C                   z-add     VABVLB        sav_VABVLB
020600130917     C                   z-add     VABCAS        sav_VABCAS
020700111130     C                   endif
020800111130     C                   add       1             wCntRecSpe
020900071003     C                   exsr      wrivat_b                                     => carico VAT
021000071003     C   50              exsr      wrivat_e                                     => carico VAT
021100020305     C                   else                                                   => x stessa spediz
021200111130     C                   exsr      impvab
021300120706     C   51              add       VABNCL        sav_VABNCL
021400111125     C   51              add       VABPKB        sav_VABPKB
021500111125     C   51              add       VABVLB        sav_VABVLB
021600130917     C   51              add       VABCAS        sav_VABCAS
021700111125     C   52              z-add     VABNCL        sav_VABNCL
021800111125     C   52              z-add     VABPKB        sav_VABPKB
021900111125     C   52              z-add     VABVLB        sav_VABVLB
022000130917     C   52              z-add     VABCAS        sav_VABCAS
022100111125     C   53              add       VABNCL        sav_VABNCL
022200111125     C   53              z-add     VABPKB        sav_VABPKB
022300111125     C   53              z-add     VABVLB        sav_VABVLB
022400130917     C   53              z-add     VABCAS        sav_VABCAS
022500111125     C*
022600071003     C                   exsr      wrivat_b                                     => carico VAT
022700071003     C   50              exsr      wrivat_e                                     => carico VAT
022800010305     C                   endif
022900010305     C                   endif
023000010305     C                   endif
023100071003     C                   endif
023200071009     C                   endif
023300000905     C*
023400000905     C                   else
023500020725     C                   eval      x_vinflg = '1'
023600000905     C                   endif
023700000905     C*
023800020725     C     VINRRN        chain     tivin000
023900020725     C                   if        %found(tivin00r)
024000020725     C                   eval      y_vinflg = x_vinflg
024100020725     C                   eval      y_vinmsg = x_vinmsg
024200020725     C                   update    tivin000
024300020725     C                   endif
024400020725     C*
024500020725     C/EXEC SQL
024600070530     C+ Fetch C1 into :INPUT_DS
024700020725     C/END-EXEC
024800020725     C*
024900020725     C                   enddo
025000020725     C*
025100020725     C/EXEC SQL
025200020725     C+ close C1
025300020725     C/END-EXEC
025400000905     C*
025500020305     C* Scarico i VAB rimasti "in sospeso"
025600071009     C                   if        wDISK = 'C'
025700111130     C  N31              eval      fivabds_ok = fivabds
025800111130     C  N31              exsr      wrivab
025900071009     C                   endif
026000010202     C*
026100010202     C                   endif
026200990910
026300990910     C* Se non ci sono record con errori ...
026400000710     C                   if        §ctrno = 0
026500990910     C* ... restituisco esito OK.
026600990921     C                   eval      wrkesito = '0'
026700990910     C                   else
026800010201     C                   if        §ctrokvb > 0
026900990921     C                   eval      wrkesito = '1'
027000000710     C                   else
027100000710     C                   eval      wrkesito = '2'
027200990910     C                   endif
027300000710     C                   endif
027400990910     C*
027500990914     C                   if        %open(tivin00r)
027600990908     C                   close     tivin00r
027700990914     C                   endif
027800120706     C                   if        %open(edivabwr)
027900120706     C                   close     edivabwr
028000120706     C                   endif
028100120706     C                   if        %open(edivatwr)
028200120706     C                   close     edivatwr
028300010201     C                   endif
028400990910     C*
028500111031     C                   if        §ctrokvb > 0 or
028600111031     C                             §ctrokvt > 0
028700111031     C                             and wPOI <> *zeros
028800010202     C                   exsr      invio
028900990920     C                   endif
029000990920     C*
029100910830     C                   ENDSR
029200000613     C***
029300010305
029400010305     C*----------------------------------------------------*
029500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029600010305     C*----------------------------------------------------*
029700020305     C     WRIVAB        BEGSR
029800070730     C*
029900071003     C* Considerazioni finali
030000111125     C                   if        ok_VABRMA = *blanks
030100111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030200071003     C                   endif
030300120706     C*
030400120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
030500120709     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
030600120709     C                   EVAL      ok_VABDCM = datcor
030700120709     C                   EVAL      ok_VABDTS = datcor
030800120709     C                   EVAL      ok_VABHMS = oracor
030900120709     C                   EVAL      ok_VABCNT = 1
031000071003     C*
031100120706     C                   write     edivab00                                     => scarico il VAB
031200010305     C*
031300010305     C                   ENDSR
031400990920
031500000801     C*----------------------------------------------------*
031600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031700000801     C*----------------------------------------------------*
031800010201     C     INZVAR        BEGSR
031900000801     C*
032000010201     C                   Z-ADD     *zeros        Num5_0
032100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032200020725     C                   MOVEL     '0'           FlgCAS            1
032300111125     C*
032400111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032500111130     C*
032600111125     C                   CLEAR                   FIVABDS
032700111130     C                   CLEAR                   FIVABDS_OK
032800111130     C                   CLEAR                   FIVABDS_SAV
032900120706     C                   CLEAR                   EDIVAB00
033000120706     C                   CLEAR                   EDIVAT00
033100000801     C*
033200000801     C                   ENDSR
033300000801     C*----------------------------------------------------*
033400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033500000801     C*----------------------------------------------------*
033600020904     C     DEFCAM        BEGSR
033700080923     C*
033800111125     C                   SETOFF                                       50
033900111125     C                   SETOFF                                       515253
034000070730     C* Imposto i valori di default...
034100120903     C                   EVAL      VABCCM = 0661788
034200120903     C                   EVAL      VATCCM = 0661788
034300120903     C                   EVAL      VABLNP = 066
034400120903     C                   EVAL      VATLNP = 066
034500120903     C                   EVAL      VABCTR = 300
034600120903     C                   EVAL      VABCBO = '1'
034700120903     C                   EVAL      VABTSP = 'C'
034800070222     C* ... e poi verifico se sono stati passati come parametri
034900070222     C                   IF        vlrppt > *blanks
035000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035200070222     C                   EXSR      CHKNUM
035300070222     C                   IF        PiInt=*on
035400070222     C                   Z-ADD     PiVal         VABCCM
035500070222     C                   Z-ADD     PiVal         VATCCM
035600070222     C                   ENDIF
035700070222     C                   ENDIF
035800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036000070222     C                   EXSR      CHKNUM
036100070222     C                   IF        PiInt=*on
036200070222     C                   Z-ADD     PiVal         VABLNP
036300070222     C                   Z-ADD     PiVal         VATLNP
036400070222     C                   ENDIF
036500070222     C                   ENDIF
036600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036800070222     C                   EXSR      CHKNUM
036900070222     C                   IF        PiInt=*on
037000070222     C                   Z-ADD     PiVal         VABCTR
037100070222     C                   ENDIF
037200070928     C                   ENDIF
037300071009     C                   MOVEL     *blanks       wDISK             1
037400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037500071009     C                   IF        wDISK <> *blanks
037600070928     C                   SETON                                        50
037700070222     C                   ENDIF
037800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
037900111125     C                   SETON                                        41
038000080923     C                   ENDIF
038100070222     C                   ENDIF
038200120706     C*
038300120706     C* valido per nr.colli/peso/volume
038400120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038500120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038600120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038700120706     C                   SELECT
038800120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
038900120706     C                   SETON                                        51
039000120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039100120706     C                   SETON                                        52
039200120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039300120706     C                   SETON                                        53
039400120706     C                   ENDSL
039500120706     C*
039600071009     C   50              EVAL      VABCTM = '7Q'
039700000801     C*
039800000801     C                   ENDSR
039900000801     C*----------------------------------------------------*
040000120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
040100000801     C*----------------------------------------------------*
040200010201     C     IMPVAB        BEGSR
040300070530     C*
040400070530     C                   SETOFF                                       3132
040500070928     C*
040600070928     C                   EXSR      DEFCAM
040700070928     C*
040800111031     C                   IF        vlrpoi <> 999
040900070928     C                   MOVE(P)   vlrpoi        VABFGS
041000070928     C                   MOVE(P)   vlrpoi        VATFGS
041100111031     C                   Z-ADD     vlrpoi        wPOI              3 0
041200111031     C                   ENDIF
041300070928     C*
041400070928     C                   MOVEL     datcor        VABAAS
041500070928     C                   MOVEL     datcor        VATAAS
041600070928     C                   MOVE      datcor        VABMGS
041700120903     C                   EVAL      VABRSD=%trim(%subst(vindta:12:35))
041800120903     C                   EVAL      VABRD2=%trim(%subst(vindta:12+35:17))
041900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
042100070928     C* ==
042200120903     C                   EVAL      VABIND=%trim(%subst(vindta:64:40))
042300120903     C                   EVAL      VABLOD=%trim(%subst(vindta:134:30))
042400120903     C                   EVAL      VABPRD=%trim(%subst(vindta:164:2))
042500120903     C                   EVAL      VABRMA=%trim(%subst(vindta:190:10))
042600120903     C                   EVAL      VABVCA=%trim(%subst(vindta:200:3))
042700071003     C* CAD
042800120903     C                   EVAL      PiStr=%trim(%subst(vindta:124:10))
042900070928     C                   EXSR      CHKNUM
043000070928     C                   IF        PiInt=*on
043100070928     C                   Z-ADD     PiVal         Num5_0
043200070928     C                   MOVEL     Num5_0        VABCAD
043300070928     C                   ELSE
043400070928     C                   SETON                                        32
043500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043600070928     C                             + ' ' + 'VABCAD'
043700070928     C                   ENDIF
043800080923     C* RMN
043900120903     C                   EVAL      PiStr=%trim(%subst(vindta:190+2:10-2))
044000080923     C                   EXSR      CHKNUM
044100080923     C                   IF        PiInt=*on
044200080923     C                   Z-ADD     PiVal         VABRMN
044300080923     C                   ELSE
044400080923     C                   SETON                                        32
044500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600080923     C                             + ' ' + 'VABRMN'
044700080923     C                   ENDIF
044800080923     C* NSP
044900111125     C                   IF        *in41 = *on
045000080923     C* NSP => Stacco un numeratore da AZNUM
045100080923     C                   clear                   TRUL33DS
045200080923     C                   eval      I33OPE = *zeros
045300080923     C                   eval      I33CNU = 302
045400080923     C                   eval      I33NUM = 1
045500080923     C                   movel     TRUL33DS      KPJBU
045600080923     C                   call      'TRUL33R'
045700080923     C                   parm                    KPJBA
045800080923     C                   movel     KPJBU         TRUL33DS
045900080923     C                   if        O33ERR = *zeros
046000080923     C                   z-add     O33NRF        VABNSP
046100080923     C                   z-add     O33NRF        VATNSP
046200080923     C                   else
046300080923     C                   SETON                                        31
046400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046500080923     C                             + ' ' + 'VABNSP VATNSP'
046600080923     C                   endif
046700080923     C                   ELSE
046800120903     C* parto dal secondo byte perché il primo è alfabetico
046900120903     C                   EVAL      PiStr=%subst(vindta:241+1:10-1)
047000080923     C                   EXSR      CHKNUM
047100080923     C                   IF        PiInt=*on
047200080923     C                   Z-ADD     PiVal         VABNSP
047300080923     C                   Z-ADD     PiVal         VATNSP
047400080923     C                   ELSE
047500080923     C                   SETON                                        31
047600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700080923     C                             + ' ' + 'VABNSP VATNSP'
047800080923     C                   ENDIF
047900080923     C                   ENDIF
048000071003     C* NCL
048100120903     C* VABNCL è fisso a 1 per riga per cui non uso il dato reperito dal file
048200120903     C* Il dato lo reperisco lo stesso perché mi serve per calcolare il peso
048300120903     C                   EVAL      PiStr=%trim(%subst(vindta:166:5))
048400070928     C                   EXSR      CHKNUM
048500070928     C                   IF        PiInt=*on
048600120903     C                   Z-ADD     1             VABNCL
048700120903     C                   EVAL      wDivPKB = PiVal
048800070928     C                   ELSE
048900070928     C                   SETON                                        32
049000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100070928     C                             + ' ' + 'VABNCL'
049200070928     C                   ENDIF
049300071003     C* PKB
049400120903     C                   IF        %subst(vindta:171:10) <> *blanks
049500120903     C                   EVAL      PiStr=%trim(%subst(vindta:171:10))
049600070928     C                   EXSR      CHKNUM
049700070928     C                   IF        PiNum=*on
049800120903     C* il peso ci arriva KG
049900120903     C                   EVAL(H)   VABPKB = PiVal/wDivPKB
050000070928     C                   ELSE
050100070928     C                   SETON                                        32
050200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300070928     C                             + ' ' + 'VABPKB'
050400070928     C                   ENDIF
050500110520     C                   ENDIF
050600120706     C* VLB
050700140711     C***                IF        %subst(vindta:181:9) <> *blanks
050800140711     C***                EVAL      PiStr=%trim(%subst(vindta:181:9))
050900140711     C***                EXSR      CHKNUM
051000140711     C***                IF        PiNum=*on
051100140711     C***                Z-ADD(H)  PiVal         VABVLB
051200140711     C***                ELSE
051300140711     C***                SETON                                        32
051400140711     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
051500140711     C***                          + ' ' + 'VABVLB'
051600140711     C***                ENDIF
051700140711     C***                ENDIF
051800080923     C* CAS
051900120903     C                   IF        %trim(%subst(vindta:203:12))<>*blanks
052000080923     C                             AND
052100120903     C                             %trim(%subst(vindta:203:12))<>*zeros
052200080923     C                             AND
052300120903     C                             %trim(%subst(vindta:203:12))<>'0,00'
052400080923     C                             AND
052500120903     C                             %trim(%subst(vindta:203:12))<>'000000000,00'
052600080923     C                             AND
052700120903     C                             %trim(%subst(vindta:203:12))<>'000000000.00'
052800070928     C                   MOVEL     '1'           FlgCAS
052900120903     C                   EVAL      PiStr=%trim(%subst(vindta:203:12))
053000070928     C                   EXSR      CHKNUM
053100070928     C                   IF        PiNum=*on
053200070928     C                   Z-ADD     PiVal         VABCAS
053300070928     C                   ELSE
053400070928     C                   SETON                                        32
053500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053600070928     C                             + ' ' + 'VABCAS'
053700070928     C                   ENDIF
053800070928     C                   ENDIF
053900070730     C*
054000070730     C* Considerazioni finali su CBO/CAS
054100070730     C                   IF        FlgCAS = '1'
054200070730     C                   IF        VABCBO = '1'
054300070730     C                   EVAL      VABCBO = '4'
054400070730     C                   ENDIF
054500070730     C                   IF        VABCBO = '2'
054600070730     C                   EVAL      VABCBO = '6'
054700070730     C                   ENDIF
054800070730     C                   ENDIF
054900020305     C*
055000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055100011113     C                   EXSR      CHKIMPDIV
055200010202     C*
055300000801     C* Ebbene...
055400000801     C                   ADD       1             §CTRMO
055500070530     C                   IF        *in31 <> *zeros OR
055600070530     C                             *in32 <> *zeros
055700000801     C                   ADD       1             §CTRNO
055800020725     C                   EVAL      x_vinflg = '2'
055900000801     C                   ELSE
056000010201     C                   ADD       1             §CTROKVB
056100000801     C                   ENDIF
056200000801     C*
056300000801     C                   ENDSR
056400010201     C*----------------------------------------------------*
056500120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
056600010201     C*----------------------------------------------------*
056700071003     C     WRIVAT_B      BEGSR
056800090204     C*
056900090204     C                   eval      vatAAS = vabAAS
057000090204     C                   eval      vatLNP = vabLNP
057100090204     C                   eval      vatNRS = vabNRS
057200090204     C                   eval      vatNSP = vabNSP
057300010201     C*
057400120706     C* Valorizzo il buffer di scrittura del EDIVAT
057500070928     C* - TIPO RECORD "B"
057600120903     C                   if        %subst(vindta:104:20) <> *blanks
057700120706     C*
057800120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
057900120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
058000120706     C                   EVAL      VATCNT = 1
058100120706     C*
058200070928     C                   eval      VATTRC = 'B'
058300120903     C                   eval      VATNOT = %trim(%subst(vindta:104:20))
058400120706     C                   write     EDIVAT00
058500111031     C                   add       1             §ctrokvt
058600020725     C                   endif
058700010201     C*
058800010201     C                   ENDSR
058900071003     C*----------------------------------------------------*
059000120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
059100071003     C*----------------------------------------------------*
059200071003     C     WRIVAT_E      BEGSR
059300090204     C*
059400090204     C                   eval      vatAAS = vabAAS
059500090204     C                   eval      vatLNP = vabLNP
059600090204     C                   eval      vatNRS = vabNRS
059700090204     C                   eval      vatNSP = vabNSP
059800071003     C*
059900120706     C* Valorizzo il buffer di scrittura del EDIVAT
060000071003     C* - TIPO RECORD "E"
060100120903     C                   if        %subst(vindta:215:26) <> *blanks
060200120706     C*
060300120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
060400120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
060500120706     C                   EVAL      VATCNT = 1
060600120706     C*
060700071003     C                   eval      VATTRC = 'E'
060800120903     C                   eval      VATNOT = %trim(%subst(vindta:215:26))
060900120706     C                   write     EDIVAT00
061000111031     C                   add       1             §ctrokvt
061100071003     C                   endif
061200071003     C*
061300071003     C                   ENDSR
061400020904
061500020904
061600010202     C*----------------------------------------------------*
061700120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
061800010202     C*----------------------------------------------------*
061900020305     C     PREVAT        BEGSR
062000010202     C*
062100120706     C* Compongo il nome del membro da dare al EDIVATWR
062200010202     C                   eval      parmbr = vlrhdl
062300010202     C                   movel     'M'           parmbr
062400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062500010202     C                   eval      paropz = '1'
062600010202     C* Effettuo la chiamata al CLLE preposto
062700120706     C                   call(e)   'TITVEVTC'
062800010202     C                   parm                    parccm
062900010202     C                   parm                    parmbr
063000010202     C                   parm                    paropz
063100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063200010202     C                   if        %error
063300010202     C                   movel     '1'           chkcall
063400010202     C                   else
063500010202     C                   movel     '0'           chkcall
063600010202     C                   endif
063700010202     C*
063800010202     C                   ENDSR
063900000801     C*----------------------------------------------------*
064000000801     C*  CONTROLLO NUMERICITA' CAMPI
064100000801     C*----------------------------------------------------*
064200000801     C     CHKNUM        BEGSR
064300081113     C*
064400081113     C                   IF        PiDecChr = *blanks
064500120903     C                   EVAL      PiDecChr = ','
064600081113     C                   ENDIF
064700081113     C*
064800081113     C                   callp(e)  UBISNUM_Check(PiStr
064900081113     C                                          :PiDecChr
065000081113     C                                          :PiVal
065100081113     C                                          :PiNum
065200081113     C                                          :PiInt)
065300081113     C*
065400000801     C                   IF        %error
065500000801     C                   EVAL      PiInt=*off
065600000801     C                   ENDIF
065700000801     C*
065800000801     C                   ENDSR
065900000801     C***
066000000801
066100011113
066200011113     C*----------------------------------------------------*
066300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066400011113     C*----------------------------------------------------*
066500011113     C     CHKIMPDIV     BEGSR
066600011113     C*
066700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066800011113     C                   Z-ADD     *zeros        wrkDec            9 9
066900011113     C*
067000011113     C* Come prima cosa effettuo considerazioni sulla divisa
067100011113     C                   IF        vabIAS > *zeros
067200011113     C                   IF        vabVAS <> 'EUR'
067300011113     C                   EVAL      vabVAS =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C                   IF        vabCAS > *zeros
067800011113     C                   IF        vabVCA <> 'EUR'
067900011113     C                   EVAL      vabVCA =  'ITL'
068000011113     C                   ENDIF
068100011113     C                   ENDIF
068200011113     C*
068300011113     C                   IF        vabVMD > *zeros
068400020305     C                   IF        vabVAD <> 'EUR'
068500011113     C                   EVAL      vabVAD =  'ITL'
068600011113     C                   ENDIF
068700011113     C                   ENDIF
068800011113     C*
068900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069000011113     C                   Z-ADD     vabIAS        wrkDec
069100011113     C                   IF        wrkDec > *zeros
069200011113     C                   IF        vabVAS = 'ITL'
069300011113     C                   EVAL      vabIAS = *zeros
069400011113     C                   ENDIF
069500011113     C                   ENDIF
069600011113     C*
069700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069800011113     C                   Z-ADD     vabCAS        wrkDec
069900011113     C                   IF        wrkDec > *zeros
070000011113     C                   IF        vabVCA = 'ITL'
070100011113     C                   EVAL      vabCAS = *zeros
070200011113     C                   ENDIF
070300011113     C                   ENDIF
070400011113     C*
070500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070600011113     C                   Z-ADD     vabVMD        wrkDec
070700011113     C                   IF        wrkDec > *zeros
070800011113     C                   IF        vabVAD = 'ITL'
070900011113     C                   EVAL      vabVMD = *zeros
071000011113     C                   ENDIF
071100011113     C                   ENDIF
071200011113     C*
071300011113     C                   ENDSR
071400011113     C***
071500011113
071600011113
071700000801
071800000801
071900990920      /TITLE Invio dei dati al punto operativo.
072000010202     C     invio         BEGSR
072100990920     C*
072200120706     C* 1° invio EDIVAT
072300010201     C                   reset                   dscmz
072400111031     C                   move      wPOI          cmzdst
072500120706     C                   eval      cmzfld = 'EDIVATWR'
072600010201     C                   eval      cmzmbd = vlrhdl
072700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072800021009     C***                if        prmfir = *blanks
072900120706     C                   eval      cmzfla = 'EDIVAT0F'
073000120706     C                   eval      cmzmba = 'EDIVAT0F'
073100021009     C***                else
073200021009     C***                eval      cmzfla = prmfir
073300021009     C***                eval      cmzmba = prmfir
073400021009     C***                endif
073500010201     C                   eval      cmznrr = *zeros
073600020305     C                   move      §ctrokvt      cmznrr
073700021018     C                   eval      cmzlba = vlrfl1
073800010201     C                   call(e)   'TIS711C'
073900010201     C                   parm                    dscmz
074000010201     C                   parm      *blanks       esito
074100010205     C                   if        %error
074200010205     C                             or cmzerr = '1'
074300010205     C                             or esito  = '1'
074400010205     C                   eval      wrkesito = '3'
074500010205     C                   else
074600010201     C*
074700120706     C* 2° invio EDIVAB
074800010201     C                   reset                   dscmz
074900111031     C                   move      wPOI          cmzdst
075000010201     C                   eval      cmzfld = vlrfou
075100010201     C                   eval      cmzmbd = vlrhdl
075200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075300021009     C***                if        prmfir = *blanks
075400120712     C                   eval      cmzfla = 'EDIVAB0F'
075500120712     C                   eval      cmzmba = 'EDIVAB0F'
075600021009     C***                else
075700021009     C***                eval      cmzfla = prmfir
075800021009     C***                eval      cmzmba = prmfir
075900021009     C***                endif
076000010201     C                   eval      cmznrr = *zeros
076100010201     C                   move      §ctrokvb      cmznrr
076200021018     C                   eval      cmzlba = vlrfl1
076300010201     C                   call(e)   'TIS711C'
076400010201     C                   parm                    dscmz
076500010201     C                   parm      *blanks       esito
076600010201     C                   if        %error
076700010201     C                             or cmzerr = '1'
076800010201     C                             or esito  = '1'
076900010201     C                   eval      wrkesito = '3'
077000010201     C                   endif
077100010205     C                   endif
077200990920     C*
077300000613     C                   ENDSR
077400000613     C***
077500070411
077600070411     C     *pssr         BEGSR
077700070411     C*
077800070411     C                   if        %open(tivin00r)
077900070411     C                   close     tivin00r
078000070411     C                   endif
078100120706     C                   if        %open(edivabwr)
078200120706     C                   close     edivabwr
078300120706     C                   endif
078400120706     C                   if        %open(edivatwr)
078500120706     C                   close     edivatwr
078600070411     C                   endif
078700070411     C*
078800070411     C* Effettuo la chiamata al CLLE preposto
078900120706     C                   call(e)   'TITVEVTC'
079000070411     C                   parm                    parccm
079100070411     C                   parm                    parmbr
079200070411     C                   parm      '2'           paropz
079300070411     C*
079400070411     C                   eval      wrkesito = '2'
079500070411     C*
079600070411     C                   seton                                        LR
079700070411     C*
079800070411     C                   ENDSR     '*CANCL'
079900070411     C***
080000070411
080100990910
080200000613     C     *inzsr        BEGSR
080300990910     C*
080400990910     C     *entry        plist
080500990920     C                   parm                    tivlrds
080600990921     C                   parm      wrkesito      esito
080700000724     C                   parm                    prmlit
080800000710     C                   parm                    prmfir
080900000613     C*
081000000830     C* CALCOLA LA DATA CORRENTE
081100120706     C                   time                    wn14             14 0
081200120706     C                   movel     wn14          oracor            6 0          *ORA
081300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081400100324     C                   eval      datcor = %dec(%date() : *ISO)
081500000830     C*
081600000613     C                   ENDSR
081700000613     C***
