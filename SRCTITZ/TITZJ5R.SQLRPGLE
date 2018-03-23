000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400170328      * - il file che arriva ha le prime 9 righe contenente testo del cliente
000500170328      *   la soluzione è stata cambiare l'SQL
000600170328      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000120709     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001100120706     FEDIVATwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100120706     D fivabds       e ds                  extname(EDIVAB0f)
002200120706     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
002300120706     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800000613     D rrnum           s              6  0 INZ(*zeros)
002900170328     D depspe          s             20    INZ(*blanks)
003000170328     D curspe          s             20    INZ(*blanks)
003100010202     D parccm          s              8    INZ(*blanks)
003200010202     D parmbr          s             10    INZ(*blanks)
003300010202     D paropz          s              1    INZ(*blanks)
003400010202     D chkcall         s              1    INZ(*blanks)
003500080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003600070530     D Num5_0          s              5  0
003700120903     D wDivPKB         s                   like(VABNCL)
003800150527     D w70             s             70
003900170316     D w4              s              4
004000000830
004100020725
004200020725     D*------------------
004300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004400020725     D*------------------
004500070530     D INPUT_DS        DS                  INZ
004600101007     D  VINDTA                     2048
004700101007     D  VINFLG                        1
004800170328     D  VINSPE                       20
004900101007     D  VINRRN                        8  0
005000020725     D*
005100080923     D*------------------
005200080923     D* DS REPERIMENTO NUMERATORE
005300080923     D*------------------
005400080923     D trul33ds      e ds                  inz
005500150528     D*------------------
005600150528     D TRUL28DSX     e ds                  inz
005700080923     D*------------------
005800080923     D* DS ARCHITETTURA
005900080923     D*------------------
006000080923     D kpjba         e ds                  inz
006100080923     D*------------------
006200121113     D  Num7_0         s              7  0
006300081113
006400081113     D*------------------
006500081113     D* LINKING A DEFINIZIONI ESTERNE
006600081113     D*------------------
006700100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900081113
007000990908
007100010201
007200010201
007300080117     C*
007400080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007500080117     C
007600080117     C/EXEC SQL
007700080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007800080117     C/END-EXEC
007900080117     C
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008500000613     C*
008600010202     C* Effettuo la chiamata al CLLE preposto
008700120706     C                   call(e)   'TITVEVTC'
008800010202     C                   parm                    parccm
008900010202     C                   parm                    parmbr
009000010202     C                   parm      '2'           paropz
009100070730     C*
009200070730     C* Effettuo lancio TISI95 solo x chiusura
009300070730     C                   CLEAR                   TISI95DS
009400070730     C                   EVAL      I95TLA = 'C'
009500070730     C                   CALL      'TISI95R'
009600070730     C                   PARM                    TISI95DS
009700000616     C*
009800000801     C
009900010201     C                   seton                                        LR
010000990908
010100000801
010200910830     C*--------------------------------------------------------
010300120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
010400910830     C*--------------------------------------------------------
010500070530     C     RWFILE        BEGSR
010600990910     C*
010700990914     C                   if        not %open(tivin00r)
010800990908     C                   open      tivin00r
010900990914     C                   endif
011000120706     C                   if        not %open(edivabwr)
011100120706     C                   open      edivabwr
011200990914     C                   endif
011300070530     C*
011400120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011500020305     C                   exsr      prevat
011600010201     C*
011700010202     C                   if        chkcall = '0'
011800010202     C*
011900120706     C                   if        not %open(edivatwr)
012000120706     C                   open      edivatwr
012100010201     C                   endif
012200080117     C*
012300080117     C                   EXSR      INZVAR
012400080117     C                   EXSR      DEFCAM
012500990910     C*
012600010201     C                   clear                   §CTROKVB          5 0
012700020305     C                   clear                   §CTROKVT          5 0
012800000801     C                   clear                   §CTRMO            5 0
012900000801     C                   clear                   §CTRNO            5 0
013000990910     C*
013100170328     C* siccome le prime 9 righe del .txt sono testo, le devo scartare.
013200170328     C* non posso farlo dopo il consueto SQL:
013300170328     C*EXEC SQL
013400170328     C* declare C1 cursor for select
013500170328     C* vindta, vinflg, substr(vindta, 14 , 20) as sped, rrn(tivin00r)
013600170328     C* from tivin00r
013700170328     C* order by sped
013800170328     C* for read only
013900170328     C*END-EXEC
014000170328     C* perché ordinando per sped c'è rischio che parte del testo finisca dopo la riga 9.
014100170328     C* Per cui devo toglierle con l'SQL, ma siccome la clausola row_number() non può essere messa
014200170328     C* in WHERE, devo eseguire 2 selezioni, una che aggiunga il numero riga ma non ordini, e la
014300170328     C* seconda che tolga le 9 righe e poi ordini.
014400170328     C*
014500170328     C*
014600020725     C/EXEC SQL
014700170328     C+ declare C1 cursor for
014800170328     C+ with base as
014900170328     C+ (SELECT
015000170328     C+ row_number() over() as riga,
015100170328     C+ vindta, vinflg, substr(vindta, 14 , 20) as sped, rrn(tivin00r) as r
015200170328     C+ from tivin00r)
015300170328     C+ SELECT
015400170328     C+ vindta, vinflg, sped, r
015500170328     C+ from base
015600170328     C+ WHERE RIGA > 9
015700060223     C+ order by sped
015800020725     C+ for read only
015900020725     C/END-EXEC
016000020725     C
016100020725     C/EXEC SQL
016200020725     C+ open C1
016300020725     C/END-EXEC
016400020725     C
016500020725     C/EXEC SQL
016600070530     C+ Fetch C1 into :INPUT_DS
016700020725     C/END-EXEC
016800020725     C*
016900020725     C                   dow       sqlcod = *zeros
017000990913     C*
017100020725     C                   if        vindta > *blanks
017200000613     C                   add       1             rrnum
017300000801     C*
017400020725     C                   if        vinflg = *blanks
017500020725     C                             or vinflg = '0'
017600020725     C                             or vinflg = '2'
017700000801     C*
017800020725     C                   clear                   x_vinmsg
017900020725     C                   eval      x_vinflg = '1'
018000010305     C*
018100170328     C* Determino il numero di Spedizione (in questo caso RMN) e a rottura eseguo operazioni
018200170328     C                   EVAL      PiStr=%trim(%subst(vindta:14:20))
018300020305     C                   MOVEL(p)  PiStr         curspe
018400010305     C*
018500071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
018600080923     C                   if        *in50 = *off
018700170310     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione
018800170328     C***                exsr      repNSP
018900071003     C                   exsr      impvab
019000111130     C  N31              eval      fivabds_ok = fivabds
019100111130     C  N31              exsr      wrivab
019200170328     C***                exsr      wrivat_b                                     => carico VAT
019300170328     C***                exsr      wrivat_ij                                    => carico VAT
019400111125     C                   exsr      inzvar
019500071003     C                   else
019600080923     C*
019700071009     C                   if        wDISK = 'D'
019800150527     C                   exsr      repNSP
019900071009     C                   exsr      impvab
020000111130     C  N31              eval      fivabds_ok = fivabds
020100111130     C  N31              exsr      wrivab
020200170328     C***                exsr      wrivat_b                                     => carico VAT
020300170328     C***                exsr      wrivat_ij                                    => carico VAT
020400170310     C                   exsr      wrivat_e                                     => carico VAT
020500111125     C                   exsr      inzvar
020600071009     C                   else
020700071009     C*
020800010305     C                   if        depspe = *blanks                             => 1° giro
020900010305     C                   eval      depspe = curspe                              => memorizz. spediz
021000080117     C                   clear                   tivinds
021100150527     C                   exsr      repNSP
021200020305     C                   exsr      impvab
021300111130     C                   z-add     VABNCL        sav_VABNCL
021400111130     C                   z-add     VABPKB        sav_VABPKB
021500111130     C                   z-add     VABVLB        sav_VABVLB
021600170328     C***                exsr      wrivat_b                                     => carico VAT
021700170328     C***                exsr      wrivat_ij                                    => carico VAT
021800170310     C   50              exsr      wrivat_e                                     => carico VAT
021900010305     C                   else
022000020725     C                   if        depspe <> curspe                             => rottura di spediz
022100111130     C                   z-add     sav_VABNCL    VABNCL
022200111130     C                   z-add     sav_VABPKB    VABPKB
022300111130     C                   z-add     sav_VABVLB    VABVLB
022400111130     C  N31              eval      fivabds_ok = fivabds
022500111125     C  N31              exsr      wrivab
022600111125     C                   exsr      inzvar
022700111125     C                   eval      depspe = curspe                              => memorizz. spediz
022800080117     C                   clear                   tivinds
022900150527     C                   exsr      repNSP
023000020305     C                   exsr      impvab
023100111130     C                   if        wCntRecSpe = *zeros
023200111130     C                   z-add     VABNCL        sav_VABNCL
023300111130     C                   z-add     VABPKB        sav_VABPKB
023400111130     C                   z-add     VABVLB        sav_VABVLB
023500111130     C                   endif
023600111130     C                   add       1             wCntRecSpe
023700170328     C***                exsr      wrivat_b                                     => carico VAT
023800170328     C***                exsr      wrivat_ij                                    => carico VAT
023900170310     C   50              exsr      wrivat_e                                     => carico VAT
024000020305     C                   else                                                   => x stessa spediz
024100111130     C                   exsr      impvab
024200120706     C   51              add       VABNCL        sav_VABNCL
024300111125     C   51              add       VABPKB        sav_VABPKB
024400111125     C   51              add       VABVLB        sav_VABVLB
024500111125     C   52              z-add     VABNCL        sav_VABNCL
024600111125     C   52              z-add     VABPKB        sav_VABPKB
024700111125     C   52              z-add     VABVLB        sav_VABVLB
024800111125     C   53              add       VABNCL        sav_VABNCL
024900111125     C   53              z-add     VABPKB        sav_VABPKB
025000111125     C   53              z-add     VABVLB        sav_VABVLB
025100111125     C*
025200170328     C***                exsr      wrivat_b                                     => carico VAT
025300170328     C***                exsr      wrivat_ij                                    => carico VAT
025400170310     C   50              exsr      wrivat_e                                     => carico VAT
025500010305     C                   endif
025600010305     C                   endif
025700010305     C                   endif
025800071003     C                   endif
025900071009     C                   endif
026000000905     C*
026100000905     C                   else
026200020725     C                   eval      x_vinflg = '1'
026300000905     C                   endif
026400000905     C*
026500020725     C     VINRRN        chain     tivin000
026600020725     C                   if        %found(tivin00r)
026700020725     C                   eval      y_vinflg = x_vinflg
026800020725     C                   eval      y_vinmsg = x_vinmsg
026900020725     C                   update    tivin000
027000020725     C                   endif
027100020725     C*
027200020725     C/EXEC SQL
027300070530     C+ Fetch C1 into :INPUT_DS
027400020725     C/END-EXEC
027500020725     C*
027600020725     C                   enddo
027700020725     C*
027800020725     C/EXEC SQL
027900020725     C+ close C1
028000020725     C/END-EXEC
028100000905     C*
028200020305     C* Scarico i VAB rimasti "in sospeso"
028300071009     C                   if        wDISK = 'C'
028400111130     C  N31              eval      fivabds_ok = fivabds
028500111130     C  N31              exsr      wrivab
028600071009     C                   endif
028700010202     C*
028800010202     C                   endif
028900990910
029000990910     C* Se non ci sono record con errori ...
029100000710     C                   if        §ctrno = 0
029200990910     C* ... restituisco esito OK.
029300990921     C                   eval      wrkesito = '0'
029400990910     C                   else
029500010201     C                   if        §ctrokvb > 0
029600990921     C                   eval      wrkesito = '1'
029700000710     C                   else
029800000710     C                   eval      wrkesito = '2'
029900990910     C                   endif
030000000710     C                   endif
030100990910     C*
030200990914     C                   if        %open(tivin00r)
030300990908     C                   close     tivin00r
030400990914     C                   endif
030500120706     C                   if        %open(edivabwr)
030600120706     C                   close     edivabwr
030700120706     C                   endif
030800120706     C                   if        %open(edivatwr)
030900120706     C                   close     edivatwr
031000010201     C                   endif
031100990910     C*
031200111031     C                   if        §ctrokvb > 0 or
031300111031     C                             §ctrokvt > 0
031400111031     C                             and wPOI <> *zeros
031500010202     C                   exsr      invio
031600990920     C                   endif
031700990920     C*
031800910830     C                   ENDSR
031900000613     C***
032000010305
032100010305     C*----------------------------------------------------*
032200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
032300010305     C*----------------------------------------------------*
032400020305     C     WRIVAB        BEGSR
032500070730     C*
032600071003     C* Considerazioni finali
032700111125     C                   if        ok_VABRMA = *blanks
032800111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
032900071003     C                   endif
033000120706     C*
033100120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
033200160701     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
033300120709     C                   EVAL      ok_VABDCM = datcor
033400120709     C                   EVAL      ok_VABDTS = datcor
033500120709     C                   EVAL      ok_VABHMS = oracor
033600120709     C                   EVAL      ok_VABCNT = 1
033700071003     C*
033800120706     C                   write     edivab00                                     => scarico il VAB
033900010305     C*
034000010305     C                   ENDSR
034100990920
034200000801     C*----------------------------------------------------*
034300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034400000801     C*----------------------------------------------------*
034500010201     C     INZVAR        BEGSR
034600000801     C*
034700010201     C                   Z-ADD     *zeros        Num5_0
034800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034900020725     C                   MOVEL     '0'           FlgCAS            1
035000170316     C                   clear                   w70
035100170316     C                   clear                   w4
035200111125     C*
035300111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
035400111130     C*
035500111125     C                   CLEAR                   FIVABDS
035600111130     C                   CLEAR                   FIVABDS_OK
035700111130     C                   CLEAR                   FIVABDS_SAV
035800120706     C                   CLEAR                   EDIVAB00
035900120706     C                   CLEAR                   EDIVAT00
036000000801     C*
036100000801     C                   ENDSR
036200000801     C*----------------------------------------------------*
036300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
036400000801     C*----------------------------------------------------*
036500020904     C     DEFCAM        BEGSR
036600080923     C*
036700111125     C                   SETOFF                                       50
036800111125     C                   SETOFF                                       515253
036900070730     C* Imposto i valori di default...
037000170328     C                   EVAL      VABCCM = 0899334
037100170328     C                   EVAL      VATCCM = 0899334
037200170328     C                   EVAL      VABLNP = 089
037300170328     C                   EVAL      VATLNP = 089
037400170328     C                   EVAL      VABCTR = 300
037500160701     C                   EVAL      VABCBO = '1'
037600170316     C                   EVAL      VABCTM = '7Q'
037700170328     C                   EVAL      VABNCL = 1
037800170328     C                   EVAL      VABPKB = 10
037900070222     C* ... e poi verifico se sono stati passati come parametri
038000070222     C                   IF        vlrppt > *blanks
038100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
038200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
038300070222     C                   EXSR      CHKNUM
038400070222     C                   IF        PiInt=*on
038500070222     C                   Z-ADD     PiVal         VABCCM
038600070222     C                   Z-ADD     PiVal         VATCCM
038700070222     C                   ENDIF
038800070222     C                   ENDIF
038900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039100070222     C                   EXSR      CHKNUM
039200070222     C                   IF        PiInt=*on
039300070222     C                   Z-ADD     PiVal         VABLNP
039400070222     C                   Z-ADD     PiVal         VATLNP
039500070222     C                   ENDIF
039600070222     C                   ENDIF
039700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
039800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
039900070222     C                   EXSR      CHKNUM
040000070222     C                   IF        PiInt=*on
040100070222     C                   Z-ADD     PiVal         VABCTR
040200070222     C                   ENDIF
040300070928     C                   ENDIF
040400071009     C                   MOVEL     *blanks       wDISK             1
040500170328     C                   EVAL      wDISK = %subst(vlrppt:14:1)
040600071009     C                   IF        wDISK <> *blanks
040700070928     C                   SETON                                        50
040800070222     C                   ENDIF
040900080923     C                   IF        %subst(vlrppt:15:1) = 'S'
041000111125     C                   SETON                                        41
041100080923     C                   ENDIF
041200070222     C                   ENDIF
041300120706     C*
041400120706     C* valido per nr.colli/peso/volume
041500120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
041600120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
041700120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
041800120706     C                   SELECT
041900120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
042000120706     C                   SETON                                        51
042100120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
042200120706     C                   SETON                                        52
042300120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
042400120706     C                   SETON                                        53
042500120706     C                   ENDSL
042600120706     C*
042700071009     C   50              EVAL      VABCTM = '7Q'
042800000801     C*
042900000801     C                   ENDSR
043000150527     C*----------------------------------------------------*
043100150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
043200150527     C*----------------------------------------------------*
043300150527     C     REPNSP        BEGSR
043400150527     C*
043500150527     C                   EXSR      INZVAR
043600150527     C                   EXSR      DEFCAM
043700150527     C*
043800150527     C                   SETON                                        60
043900150527     C*
044000150527     C* NSP => Stacco un numeratore da AZNUM
044100150527     C                   clear                   TRUL33DS
044200150527     C                   eval      I33OPE = *zeros
044300150527     C                   eval      I33CNU = 302
044400150527     C                   eval      I33NUM = 1
044500150527     C                   movel     TRUL33DS      KPJBU
044600150527     C                   call      'TRUL33R'
044700150527     C                   parm                    KPJBA
044800150527     C                   movel     KPJBU         TRUL33DS
044900150527     C                   if        O33ERR = *zeros
045000150527     C                   z-add     O33NRF        VABNSP
045100150527     C                   z-add     O33NRF        VATNSP
045200150527     C                   else
045300150527     C                   SETON                                        31
045400150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045500150527     C                             + ' ' + 'VABNSP VATNSP'
045600150527     C                   endif
045700150527     C*
045800150527     C                   ENDSR
045900000801     C*----------------------------------------------------*
046000120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
046100000801     C*----------------------------------------------------*
046200010201     C     IMPVAB        BEGSR
046300070530     C*
046400070530     C                   SETOFF                                       3132
046500070928     C*
046600070928     C                   EXSR      DEFCAM
046700070928     C*
046800111031     C                   IF        vlrpoi <> 999
046900070928     C                   MOVE(P)   vlrpoi        VABFGS
047000070928     C                   MOVE(P)   vlrpoi        VATFGS
047100111031     C                   Z-ADD     vlrpoi        wPOI              3 0
047200111031     C                   ENDIF
047300070928     C*
047400070928     C                   MOVEL     datcor        VABAAS
047500070928     C                   MOVEL     datcor        VATAAS
047600070928     C                   MOVE      datcor        VABMGS
047700170316     C                   CLEAR                   w70
047800170328     C                   EVAL      VABRSD=%trim(%subst(vindta:35:43))
047900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
048000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
048100070928     C* ==
048200170328     C                   EVAL      VABRD2=%trim(%subst(vindta:79:44))
048300170328     C                   EVAL      VABIND=%trim(%subst(vindta:176:44))
048400170328     C                   EVAL      VABLOD=%trim(%subst(vindta:131:31))
048500170328     C                   EVAL      VABPRD=%trim(%subst(vindta:163:12))
048600170328     C                   EVAL      VABRMA=%trim(%subst(vindta:1:12))
048700160701
048800071003     C* CAD
048900170328     C                   EVAL      PiStr=%trim(%subst(vindta:124:6))
049000170328     C                   EXSR      CHKNUM
049100170328     C                   IF        PiInt=*on
049200170328     C                   Z-ADD     PiVal         Num5_0
049300170328     C                   MOVEL     Num5_0        VABCAD
049400170328     C                   ELSE
049500170328     C                   SETON                                        32
049600170328     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700170328     C                             + ' ' + 'VABCAD'
049800170328     C                   ENDIF
049900121113     C*
050000121113     C* Se provincia nn valorizzata la reperisco
050100121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
050200121113     C                   IF        VABPRD = *blanks
050300121113     C                   CLEAR                   TISI95DS
050400121113     C                   EVAL      I95TCN = '3'
050500121113     C                   Z-ADD     datcor        I95DAT
050600121113     C                   EVAL      I95NAR = VABNZD
050700121113     C                   EVAL      I95CAP = VABCAD
050800121113     C                   EVAL      I95LOC = VABLOD
050900121113     C                   CALL      'TISI95R'
051000121113     C                   PARM                    TISI95DS
051100121113     C                   EVAL      VABPRD = O95PRV
051200121113     C                   ENDIF
051300121113     C* RMN
051400170328     C                   EVAL      PiStr=%trim(%subst(vindta:14:20))
051500121113     C                   EXSR      CHKNUM
051600121113     C                   IF        PiInt=*on
051700121113     C                   Z-ADD     PiVal         VABRMN
051800121113     C                   ELSE
051900121113     C                   SETON                                        32
052000121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052100121113     C                             + ' ' + 'VABRMN'
052200121113     C                   ENDIF
052300170310
052400070730     C*
052500070730     C* Considerazioni finali su CBO/CAS
052600121113     C                   IF        FlgCAS <> '0'
052700070730     C                   IF        VABCBO = '1'
052800070730     C                   EVAL      VABCBO = '4'
052900070730     C                   ENDIF
053000070730     C                   IF        VABCBO = '2'
053100070730     C                   EVAL      VABCBO = '6'
053200070730     C                   ENDIF
053300070730     C                   ENDIF
053400020305     C*
053500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053600011113     C                   EXSR      CHKIMPDIV
053700010202     C*
053800000801     C* Ebbene...
053900000801     C                   ADD       1             §CTRMO
054000070530     C                   IF        *in31 <> *zeros OR
054100070530     C                             *in32 <> *zeros
054200000801     C                   ADD       1             §CTRNO
054300020725     C                   EVAL      x_vinflg = '2'
054400000801     C                   ELSE
054500010201     C                   ADD       1             §CTROKVB
054600000801     C                   ENDIF
054700000801     C*
054800000801     C                   ENDSR
054900010201     C*----------------------------------------------------*
055000120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
055100010201     C*----------------------------------------------------*
055200170316     C     WRIVAT_B      BEGSR
055300090204     C*
055400090204     C                   eval      vatAAS = vabAAS
055500090204     C                   eval      vatLNP = vabLNP
055600090204     C                   eval      vatNRS = vabNRS
055700090204     C                   eval      vatNSP = vabNSP
055800010201     C*
055900120706     C* Valorizzo il buffer di scrittura del EDIVAT
056000170316     C* - TIPO RECORD "B"
056100170316     C                   if        %subst(vindta:344:40) <> *blanks
056200120706     C*
056300120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
056400170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
056500120706     C                   EVAL      VATCNT = 1
056600120706     C*
056700170316     C                   eval      VATTRC = 'B'
056800170316     C                   eval      VATNOT = %trim(%subst(vindta:344:40))
056900150527     C                   write     EDIVAT00
057000111031     C                   add       1             §ctrokvt
057100150527     C                   endif
057200010201     C*
057300010201     C                   ENDSR
057400150527     C*----------------------------------------------------*
057500150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
057600150527     C*----------------------------------------------------*
057700170316     C     WRIVAT_IJ     BEGSR
057800150527     C*
057900150527     C                   eval      vatAAS = vabAAS
058000150527     C                   eval      vatLNP = vabLNP
058100150527     C                   eval      vatNRS = vabNRS
058200150527     C                   eval      vatNSP = vabNSP
058300150527     C*
058400150527     C* Valorizzo il buffer di scrittura del EDIVAT
058500150527     C* - TIPO RECORD "I" e "J"
058600170316     C                   if        %subst(vindta:599:60) <> *blanks
058700170316     C                   clear                   w70
058800170316     C                   eval      w70 = %trim(%subst(vindta:599:60))
058900150527     C*
059000150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
059100170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
059200150527     C                   EVAL      VATCNT = 1
059300150527     C*
059400150527     C                   eval      VATTRC = 'I'
059500150527     C                   eval      VATNOT = %subst(w70:1:35)
059600150527     C                   write     EDIVAT00
059700150527     C                   add       1             §ctrokvt
059800150527     C*
059900150527     C                   IF        %subst(w70:36:35) <> *blank
060000150527     C                   eval      VATTRC = 'J'
060100150527     C                   eval      VATNOT = %subst(w70:36:35)
060200150527     C                   write     EDIVAT00
060300150527     C                   add       1             §ctrokvt
060400150527     C                   endif
060500150527     C                   endif
060600150527     C*
060700150527     C                   ENDSR
060800071003     C*----------------------------------------------------*
060900120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
061000071003     C*----------------------------------------------------*
061100071003     C     WRIVAT_E      BEGSR
061200090204     C*
061300090204     C                   eval      vatAAS = vabAAS
061400090204     C                   eval      vatLNP = vabLNP
061500090204     C                   eval      vatNRS = vabNRS
061600090204     C                   eval      vatNSP = vabNSP
061700071003     C*
061800120706     C* Valorizzo il buffer di scrittura del EDIVAT
061900071003     C* - TIPO RECORD "E"
062000170328     C                   if        %subst(vindta:1:12) <> *blanks
062100120706     C*
062200120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
062300170316     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
062400120706     C                   EVAL      VATCNT = 1
062500120706     C*
062600071003     C                   eval      VATTRC = 'E'
062700170328     C                   eval      vatNOT = %trim(%subst(vindta:1:12))
062800120706     C                   write     EDIVAT00
062900111031     C                   add       1             §ctrokvt
063000150528     C                   endif
063100071003     C*
063200071003     C                   ENDSR
063300020904
063400020904
063500010202     C*----------------------------------------------------*
063600120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
063700010202     C*----------------------------------------------------*
063800020305     C     PREVAT        BEGSR
063900010202     C*
064000120706     C* Compongo il nome del membro da dare al EDIVATWR
064100010202     C                   eval      parmbr = vlrhdl
064200010202     C                   movel     'M'           parmbr
064300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
064400010202     C                   eval      paropz = '1'
064500010202     C* Effettuo la chiamata al CLLE preposto
064600120706     C                   call(e)   'TITVEVTC'
064700010202     C                   parm                    parccm
064800010202     C                   parm                    parmbr
064900010202     C                   parm                    paropz
065000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065100010202     C                   if        %error
065200010202     C                   movel     '1'           chkcall
065300010202     C                   else
065400010202     C                   movel     '0'           chkcall
065500010202     C                   endif
065600010202     C*
065700010202     C                   ENDSR
065800000801     C*----------------------------------------------------*
065900000801     C*  CONTROLLO NUMERICITA' CAMPI
066000000801     C*----------------------------------------------------*
066100000801     C     CHKNUM        BEGSR
066200081113     C*
066300081113     C                   IF        PiDecChr = *blanks
066400121113     C                   EVAL      PiDecChr = '.'
066500081113     C                   ENDIF
066600081113     C*
066700081113     C                   callp(e)  UBISNUM_Check(PiStr
066800081113     C                                          :PiDecChr
066900081113     C                                          :PiVal
067000081113     C                                          :PiNum
067100081113     C                                          :PiInt)
067200081113     C*
067300000801     C                   IF        %error
067400000801     C                   EVAL      PiInt=*off
067500000801     C                   ENDIF
067600000801     C*
067700000801     C                   ENDSR
067800000801     C***
067900000801
068000011113
068100011113     C*----------------------------------------------------*
068200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
068300011113     C*----------------------------------------------------*
068400011113     C     CHKIMPDIV     BEGSR
068500011113     C*
068600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
068700011113     C                   Z-ADD     *zeros        wrkDec            9 9
068800011113     C*
068900011113     C* Come prima cosa effettuo considerazioni sulla divisa
069000011113     C                   IF        vabIAS > *zeros
069100011113     C                   IF        vabVAS <> 'EUR'
069200011113     C                   EVAL      vabVAS =  'ITL'
069300011113     C                   ENDIF
069400011113     C                   ENDIF
069500011113     C*
069600011113     C                   IF        vabCAS > *zeros
069700011113     C                   IF        vabVCA <> 'EUR'
069800011113     C                   EVAL      vabVCA =  'ITL'
069900011113     C                   ENDIF
070000011113     C                   ENDIF
070100011113     C*
070200011113     C                   IF        vabVMD > *zeros
070300020305     C                   IF        vabVAD <> 'EUR'
070400011113     C                   EVAL      vabVAD =  'ITL'
070500011113     C                   ENDIF
070600011113     C                   ENDIF
070700011113     C*
070800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070900011113     C                   Z-ADD     vabIAS        wrkDec
071000011113     C                   IF        wrkDec > *zeros
071100011113     C                   IF        vabVAS = 'ITL'
071200011113     C                   EVAL      vabIAS = *zeros
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
071700011113     C                   Z-ADD     vabCAS        wrkDec
071800011113     C                   IF        wrkDec > *zeros
071900011113     C                   IF        vabVCA = 'ITL'
072000011113     C                   EVAL      vabCAS = *zeros
072100011113     C                   ENDIF
072200011113     C                   ENDIF
072300011113     C*
072400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
072500011113     C                   Z-ADD     vabVMD        wrkDec
072600011113     C                   IF        wrkDec > *zeros
072700011113     C                   IF        vabVAD = 'ITL'
072800011113     C                   EVAL      vabVMD = *zeros
072900011113     C                   ENDIF
073000011113     C                   ENDIF
073100011113     C*
073200011113     C                   ENDSR
073300011113     C***
073400011113
073500011113
073600000801
073700000801
073800990920      /TITLE Invio dei dati al punto operativo.
073900010202     C     invio         BEGSR
074000990920     C*
074100120706     C* 1° invio EDIVAT
074200010201     C                   reset                   dscmz
074300111031     C                   move      wPOI          cmzdst
074400120706     C                   eval      cmzfld = 'EDIVATWR'
074500010201     C                   eval      cmzmbd = vlrhdl
074600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074700021009     C***                if        prmfir = *blanks
074800120706     C                   eval      cmzfla = 'EDIVAT0F'
074900120706     C                   eval      cmzmba = 'EDIVAT0F'
075000021009     C***                else
075100021009     C***                eval      cmzfla = prmfir
075200021009     C***                eval      cmzmba = prmfir
075300021009     C***                endif
075400010201     C                   eval      cmznrr = *zeros
075500020305     C                   move      §ctrokvt      cmznrr
075600021018     C                   eval      cmzlba = vlrfl1
075700010201     C                   call(e)   'TIS711C'
075800010201     C                   parm                    dscmz
075900010201     C                   parm      *blanks       esito
076000010205     C                   if        %error
076100010205     C                             or cmzerr = '1'
076200010205     C                             or esito  = '1'
076300010205     C                   eval      wrkesito = '3'
076400010205     C                   else
076500010201     C*
076600120706     C* 2° invio EDIVAB
076700010201     C                   reset                   dscmz
076800111031     C                   move      wPOI          cmzdst
076900010201     C                   eval      cmzfld = vlrfou
077000010201     C                   eval      cmzmbd = vlrhdl
077100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077200021009     C***                if        prmfir = *blanks
077300120712     C                   eval      cmzfla = 'EDIVAB0F'
077400120712     C                   eval      cmzmba = 'EDIVAB0F'
077500021009     C***                else
077600021009     C***                eval      cmzfla = prmfir
077700021009     C***                eval      cmzmba = prmfir
077800021009     C***                endif
077900010201     C                   eval      cmznrr = *zeros
078000010201     C                   move      §ctrokvb      cmznrr
078100021018     C                   eval      cmzlba = vlrfl1
078200010201     C                   call(e)   'TIS711C'
078300010201     C                   parm                    dscmz
078400010201     C                   parm      *blanks       esito
078500010201     C                   if        %error
078600010201     C                             or cmzerr = '1'
078700010201     C                             or esito  = '1'
078800010201     C                   eval      wrkesito = '3'
078900010201     C                   endif
079000010205     C                   endif
079100990920     C*
079200000613     C                   ENDSR
079300000613     C***
079400070411
079500070411     C     *pssr         BEGSR
079600070411     C*
079700070411     C                   if        %open(tivin00r)
079800070411     C                   close     tivin00r
079900070411     C                   endif
080000120706     C                   if        %open(edivabwr)
080100120706     C                   close     edivabwr
080200120706     C                   endif
080300120706     C                   if        %open(edivatwr)
080400120706     C                   close     edivatwr
080500070411     C                   endif
080600070411     C*
080700070411     C* Effettuo la chiamata al CLLE preposto
080800120706     C                   call(e)   'TITVEVTC'
080900070411     C                   parm                    parccm
081000070411     C                   parm                    parmbr
081100070411     C                   parm      '2'           paropz
081200070411     C*
081300070411     C                   eval      wrkesito = '2'
081400070411     C*
081500070411     C                   seton                                        LR
081600070411     C*
081700070411     C                   ENDSR     '*CANCL'
081800070411     C***
081900070411
082000990910
082100000613     C     *inzsr        BEGSR
082200990910     C*
082300990910     C     *entry        plist
082400990920     C                   parm                    tivlrds
082500990921     C                   parm      wrkesito      esito
082600000724     C                   parm                    prmlit
082700000710     C                   parm                    prmfir
082800000613     C*
082900000830     C* CALCOLA LA DATA CORRENTE
083000120706     C                   time                    wn14             14 0
083100120706     C                   movel     wn14          oracor            6 0          *ORA
083200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
083300100324     C                   eval      datcor = %dec(%date() : *ISO)
083400000830     C*
083500000613     C                   ENDSR
083600000613     C***
