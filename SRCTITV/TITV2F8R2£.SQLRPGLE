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
002200170705     D edivabds      e ds                  extname(EDIVAB0f) prefix(pdf_)
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800160711     D depspe          s             20    INZ(*blanks)
002900160711     D curspe          s             20    INZ(*blanks)
003000010202     D parccm          s              8    INZ(*blanks)
003100010202     D parmbr          s             10    INZ(*blanks)
003200010202     D paropz          s              1    INZ(*blanks)
003300010202     D chkcall         s              1    INZ(*blanks)
003400080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003500070530     D Num5_0          s              5  0
003600120903     D wDivPKB         s                   like(VABNCL)
003700150527     D w70             s             70
003800000830
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400101007     D  VINDTA                     2048
004500101007     D  VINFLG                        1
004600160711     D  VINSPE                       20
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
008900170731     C*
009000170731     C                   if        *in53
009100170731     C                   call      'TIS7P2SER'
009200170731     C                   parm      'C'           pIn_Opz           1
009300170731     C                   parm                    tivlrds
009400170731     C                   parm                    EDIVABDS
009500170731     C                   parm      *blanks       pIn_CdIdAz        2
009600170731     C                   parm      *blanks       pIn_MaskPDF      50
009700170731     C                   parm      *blanks       pOut_Esito        1
009800170731     C                   endif
009900070730     C*
010000070730     C* Effettuo lancio TISI95 solo x chiusura
010100070730     C                   CLEAR                   TISI95DS
010200070730     C                   EVAL      I95TLA = 'C'
010300070730     C                   CALL      'TISI95R'
010400070730     C                   PARM                    TISI95DS
010500000616     C*
010600000801     C
010700010201     C                   seton                                        LR
010800990908
010900000801
011000910830     C*--------------------------------------------------------
011100120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011200910830     C*--------------------------------------------------------
011300070530     C     RWFILE        BEGSR
011400990910     C*
011500990914     C                   if        not %open(tivin00r)
011600990908     C                   open      tivin00r
011700990914     C                   endif
011800120706     C                   if        not %open(edivabwr)
011900120706     C                   open      edivabwr
012000990914     C                   endif
012100070530     C*
012200120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012300020305     C                   exsr      prevat
012400010201     C*
012500010202     C                   if        chkcall = '0'
012600010202     C*
012700120706     C                   if        not %open(edivatwr)
012800120706     C                   open      edivatwr
012900010201     C                   endif
013000080117     C*
013100080117     C                   EXSR      INZVAR
013200080117     C                   EXSR      DEFCAM
013300990910     C*
013400010201     C                   clear                   §CTROKVB          5 0
013500020305     C                   clear                   §CTROKVT          5 0
013600000801     C                   clear                   §CTRMO            5 0
013700000801     C                   clear                   §CTRNO            5 0
013800170705     C*
013900170705     C                   if        *in53
014000170705     C                   call      'TIS7P2SER'
014100170705     C                   parm      'O'           pIn_Opz           1
014200170705     C                   parm                    tivlrds
014300170705     C                   parm                    EDIVABDS
014400170705     C                   parm      *blanks       pIn_CdIdAz        2
014500170705     C                   parm      *blanks       pIn_MaskPDF      50
014600170705     C                   parm      *blanks       pOut_Esito        1
014700170705     C                   endif
014800990910     C*
014900020725     C/EXEC SQL
015000020725     C+ declare C1 cursor for select
015100160711     C+ vindta, vinflg, substr(vindta, 866, 20) as sped, rrn(tivin00r)
015200060223     C+ from tivin00r
015300060223     C+ order by sped
015400020725     C+ for read only
015500020725     C/END-EXEC
015600020725     C
015700020725     C/EXEC SQL
015800020725     C+ open C1
015900020725     C/END-EXEC
016000020725     C
016100020725     C/EXEC SQL
016200070530     C+ Fetch C1 into :INPUT_DS
016300020725     C/END-EXEC
016400020725     C*
016500020725     C                   dow       sqlcod = *zeros
016600990913     C*
016700020725     C                   if        vindta > *blanks
016800000613     C                   add       1             rrnum
016900000801     C*
017000020725     C                   if        vinflg = *blanks
017100020725     C                             or vinflg = '0'
017200020725     C                             or vinflg = '2'
017300000801     C*
017400020725     C                   clear                   x_vinmsg
017500020725     C                   eval      x_vinflg = '1'
017600010305     C*
017700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017800160711     C                   EVAL      PiStr=%trim(%subst(vindta:866:20))
017900020305     C                   MOVEL(p)  PiStr         curspe
018000010305     C*
018100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
018200080923     C                   if        *in50 = *off
018300160711     C* di solito quando la soluzione è disk A non si stacca il nr.spedizione
018400160711     C***                exsr      repNSP
018500071003     C                   exsr      impvab
018600111130     C  N31              eval      fivabds_ok = fivabds
018700111130     C  N31              exsr      wrivab
018800160711     C                   exsr      wrivat_b                                     => carico VAT
018900160711     C                   exsr      wrivat_i                                     => carico VAT
019000160711     C                   exsr      wrivat_s                                     => carico VAT
019100111125     C                   exsr      inzvar
019200071003     C                   else
019300080923     C*
019400071009     C                   if        wDISK = 'D'
019500150527     C                   exsr      repNSP
019600071009     C                   exsr      impvab
019700111130     C  N31              eval      fivabds_ok = fivabds
019800111130     C  N31              exsr      wrivab
019900160711     C                   exsr      wrivat_b                                     => carico VAT
020000160711     C                   exsr      wrivat_i                                     => carico VAT
020100160711     C                   exsr      wrivat_s                                     => carico VAT
020200160711     C                   exsr      wrivat_e                                     => carico VAT
020300111125     C                   exsr      inzvar
020400071009     C                   else
020500071009     C*
020600010305     C                   if        depspe = *blanks                             => 1° giro
020700010305     C                   eval      depspe = curspe                              => memorizz. spediz
020800080117     C                   clear                   tivinds
020900150527     C                   exsr      repNSP
021000020305     C                   exsr      impvab
021100111130     C                   z-add     VABNCL        sav_VABNCL
021200111130     C                   z-add     VABPKB        sav_VABPKB
021300111130     C                   z-add     VABVLB        sav_VABVLB
021400160711     C                   exsr      wrivat_b                                     => carico VAT
021500160711     C                   exsr      wrivat_i                                     => carico VAT
021600160711     C                   exsr      wrivat_s                                     => carico VAT
021700160711     C   50              exsr      wrivat_e                                     => carico VAT
021800010305     C                   else
021900020725     C                   if        depspe <> curspe                             => rottura di spediz
022000111130     C                   z-add     sav_VABNCL    VABNCL
022100111130     C                   z-add     sav_VABPKB    VABPKB
022200111130     C                   z-add     sav_VABVLB    VABVLB
022300111130     C  N31              eval      fivabds_ok = fivabds
022400111125     C  N31              exsr      wrivab
022500111125     C                   exsr      inzvar
022600111125     C                   eval      depspe = curspe                              => memorizz. spediz
022700080117     C                   clear                   tivinds
022800150527     C                   exsr      repNSP
022900020305     C                   exsr      impvab
023000111130     C                   if        wCntRecSpe = *zeros
023100111130     C                   z-add     VABNCL        sav_VABNCL
023200111130     C                   z-add     VABPKB        sav_VABPKB
023300111130     C                   z-add     VABVLB        sav_VABVLB
023400111130     C                   endif
023500111130     C                   add       1             wCntRecSpe
023600160711     C                   exsr      wrivat_b                                     => carico VAT
023700160711     C                   exsr      wrivat_i                                     => carico VAT
023800160711     C                   exsr      wrivat_s                                     => carico VAT
023900160711     C   50              exsr      wrivat_e                                     => carico VAT
024000020305     C                   else                                                   => x stessa spediz
024100111130     C                   exsr      impvab
024200120706     C   51              add       VABNCL        sav_VABNCL
024300111125     C   51              add       VABPKB        sav_VABPKB
024400111125     C   51              add       VABVLB        sav_VABVLB
024500111125     C   52              z-add     VABNCL        sav_VABNCL
024600111125     C   52              z-add     VABPKB        sav_VABPKB
024700111125     C   52              z-add     VABVLB        sav_VABVLB
024800170705     C   54              add       VABNCL        sav_VABNCL
024900170705     C   54              z-add     VABPKB        sav_VABPKB
025000170705     C   54              z-add     VABVLB        sav_VABVLB
025100111125     C*
025200160711     C                   exsr      wrivat_b                                     => carico VAT
025300160711     C                   exsr      wrivat_i                                     => carico VAT
025400160711     C                   exsr      wrivat_s                                     => carico VAT
025500160711     C   50              exsr      wrivat_e                                     => carico VAT
025600010305     C                   endif
025700010305     C                   endif
025800010305     C                   endif
025900071003     C                   endif
026000071009     C                   endif
026100000905     C*
026200000905     C                   else
026300020725     C                   eval      x_vinflg = '1'
026400000905     C                   endif
026500000905     C*
026600020725     C     VINRRN        chain     tivin000
026700020725     C                   if        %found(tivin00r)
026800020725     C                   eval      y_vinflg = x_vinflg
026900020725     C                   eval      y_vinmsg = x_vinmsg
027000020725     C                   update    tivin000
027100020725     C                   endif
027200020725     C*
027300020725     C/EXEC SQL
027400070530     C+ Fetch C1 into :INPUT_DS
027500020725     C/END-EXEC
027600020725     C*
027700020725     C                   enddo
027800020725     C*
027900020725     C/EXEC SQL
028000020725     C+ close C1
028100020725     C/END-EXEC
028200000905     C*
028300020305     C* Scarico i VAB rimasti "in sospeso"
028400071009     C                   if        wDISK = 'C'
028500111130     C  N31              eval      fivabds_ok = fivabds
028600111130     C  N31              exsr      wrivab
028700071009     C                   endif
028800010202     C*
028900010202     C                   endif
029000990910
029100990910     C* Se non ci sono record con errori ...
029200000710     C                   if        §ctrno = 0
029300990910     C* ... restituisco esito OK.
029400990921     C                   eval      wrkesito = '0'
029500990910     C                   else
029600010201     C                   if        §ctrokvb > 0
029700990921     C                   eval      wrkesito = '1'
029800000710     C                   else
029900000710     C                   eval      wrkesito = '2'
030000990910     C                   endif
030100000710     C                   endif
030200990910     C*
030300990914     C                   if        %open(tivin00r)
030400990908     C                   close     tivin00r
030500990914     C                   endif
030600120706     C                   if        %open(edivabwr)
030700120706     C                   close     edivabwr
030800120706     C                   endif
030900120706     C                   if        %open(edivatwr)
031000120706     C                   close     edivatwr
031100010201     C                   endif
031200990910     C*
031300111031     C                   if        §ctrokvb > 0 or
031400111031     C                             §ctrokvt > 0
031500111031     C                             and wPOI <> *zeros
031600010202     C                   exsr      invio
031700990920     C                   endif
031800990920     C*
031900910830     C                   ENDSR
032000000613     C***
032100010305
032200010305     C*----------------------------------------------------*
032300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
032400010305     C*----------------------------------------------------*
032500020305     C     WRIVAB        BEGSR
032600070730     C*
032700071003     C* Considerazioni finali
032800111125     C                   if        ok_VABRMA = *blanks
032900111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
033000071003     C                   endif
033100120706     C*
033200120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
033300160701     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
033400120709     C                   EVAL      ok_VABDCM = datcor
033500120709     C                   EVAL      ok_VABDTS = datcor
033600120709     C                   EVAL      ok_VABHMS = oracor
033700120709     C                   EVAL      ok_VABCNT = 1
033800170705     C***
033900170705     C                   if        *in53
034000170705     C* Solo se valorizzato RMA
034100170705     C                   if        ok_VABRMA <> *blanks AND
034200170705     C                             ok_VABCCM <> 0241160
034300170705     C                   eval      ok_VABCTM = '7T'
034400170705     C                   eval      edivabds = fivabds_ok
034500170705     C                   eval      pIn_MaskPDF = %trim(ok_VABRMA)+'.pdf'
034600170705     C                   call      'TIS7P2SER'
034700170705     C                   parm      'W'           pIn_Opz           1
034800170705     C                   parm                    tivlrds
034900170705     C                   parm                    EDIVABDS
035000170705     C                   parm      '10'          pIn_CdIdAz        2
035100170705     C                   parm                    pIn_MaskPDF      50
035200170705     C                   parm      *blanks       pOut_Esito        1
035300170705     C                   endif
035400170705     C                   endif
035500170705     C***
035600071003     C*
035700120706     C                   write     edivab00                                     => scarico il VAB
035800010305     C*
035900010305     C                   ENDSR
036000990920
036100000801     C*----------------------------------------------------*
036200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036300000801     C*----------------------------------------------------*
036400010201     C     INZVAR        BEGSR
036500000801     C*
036600010201     C                   Z-ADD     *zeros        Num5_0
036700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036800020725     C                   MOVEL     '0'           FlgCAS            1
036900111125     C*
037000111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
037100111130     C*
037200111125     C                   CLEAR                   FIVABDS
037300111130     C                   CLEAR                   FIVABDS_OK
037400111130     C                   CLEAR                   FIVABDS_SAV
037500120706     C                   CLEAR                   EDIVAB00
037600120706     C                   CLEAR                   EDIVAT00
037700000801     C*
037800000801     C                   ENDSR
037900000801     C*----------------------------------------------------*
038000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
038100000801     C*----------------------------------------------------*
038200020904     C     DEFCAM        BEGSR
038300080923     C*
038400111125     C                   SETOFF                                       50
038500170705     C                   SETOFF                                       515254
038600170705     C                   SETOFF                                       53
038700070730     C* Imposto i valori di default...
038800160711     C                   EVAL      VABCCM = 0241160
038900160711     C                   EVAL      VATCCM = 0241160
039000160711     C                   EVAL      VABLNP = 024
039100160711     C                   EVAL      VATLNP = 024
039200160727     C                   EVAL      VABCTR = 100
039300160701     C                   EVAL      VABCBO = '1'
039400160711     C                   EVAL      VABCTM = '7Q'
039500070222     C* ... e poi verifico se sono stati passati come parametri
039600070222     C                   IF        vlrppt > *blanks
039700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
039800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039900070222     C                   EXSR      CHKNUM
040000070222     C                   IF        PiInt=*on
040100070222     C                   Z-ADD     PiVal         VABCCM
040200070222     C                   Z-ADD     PiVal         VATCCM
040300070222     C                   ENDIF
040400070222     C                   ENDIF
040500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
040600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040700070222     C                   EXSR      CHKNUM
040800070222     C                   IF        PiInt=*on
040900070222     C                   Z-ADD     PiVal         VABLNP
041000070222     C                   Z-ADD     PiVal         VATLNP
041100070222     C                   ENDIF
041200070222     C                   ENDIF
041300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
041400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041500070222     C                   EXSR      CHKNUM
041600070222     C                   IF        PiInt=*on
041700070222     C                   Z-ADD     PiVal         VABCTR
041800070222     C                   ENDIF
041900070928     C                   ENDIF
042000071009     C                   MOVEL     *blanks       wDISK             1
042100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042200071009     C                   IF        wDISK <> *blanks
042300070928     C                   SETON                                        50
042400070222     C                   ENDIF
042500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
042600111125     C                   SETON                                        41
042700080923     C                   ENDIF
042800170705     C                   IF        %subst(vlrppt:16:1) = 'S'
042900170705     C                   SETON                                        53
043000170705     C                   ELSE
043100170705     C                   SETOFF                                       53
043200170705     C                   ENDIF
043300070222     C                   ENDIF
043400120706     C*
043500120706     C* valido per nr.colli/peso/volume
043600120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
043700120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
043800120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
043900120706     C                   SELECT
044000120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
044100120706     C                   SETON                                        51
044200120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
044300120706     C                   SETON                                        52
044400120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
044500170705     C                   SETON                                        54
044600120706     C                   ENDSL
044700120706     C*
044800071009     C   50              EVAL      VABCTM = '7Q'
044900000801     C*
045000000801     C                   ENDSR
045100150527     C*----------------------------------------------------*
045200150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
045300150527     C*----------------------------------------------------*
045400150527     C     REPNSP        BEGSR
045500150527     C*
045600150527     C                   EXSR      INZVAR
045700150527     C                   EXSR      DEFCAM
045800150527     C*
045900150527     C                   SETON                                        60
046000150527     C*
046100150527     C* NSP => Stacco un numeratore da AZNUM
046200150527     C                   clear                   TRUL33DS
046300150527     C                   eval      I33OPE = *zeros
046400150527     C                   eval      I33CNU = 302
046500150527     C                   eval      I33NUM = 1
046600150527     C                   movel     TRUL33DS      KPJBU
046700150527     C                   call      'TRUL33R'
046800150527     C                   parm                    KPJBA
046900150527     C                   movel     KPJBU         TRUL33DS
047000150527     C                   if        O33ERR = *zeros
047100150527     C                   z-add     O33NRF        VABNSP
047200150527     C                   z-add     O33NRF        VATNSP
047300150527     C                   else
047400150527     C                   SETON                                        31
047500150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600150527     C                             + ' ' + 'VABNSP VATNSP'
047700150527     C                   endif
047800150527     C*
047900150527     C                   ENDSR
048000000801     C*----------------------------------------------------*
048100120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
048200000801     C*----------------------------------------------------*
048300010201     C     IMPVAB        BEGSR
048400070530     C*
048500070530     C                   SETOFF                                       3132
048600070928     C*
048700070928     C                   EXSR      DEFCAM
048800070928     C*
048900111031     C                   IF        vlrpoi <> 999
049000070928     C                   MOVE(P)   vlrpoi        VABFGS
049100070928     C                   MOVE(P)   vlrpoi        VATFGS
049200111031     C                   Z-ADD     vlrpoi        wPOI              3 0
049300111031     C                   ENDIF
049400070928     C*
049500070928     C                   MOVEL     datcor        VABAAS
049600070928     C                   MOVEL     datcor        VATAAS
049700070928     C                   MOVE      datcor        VABMGS
049800160711     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
049900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
050000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
050100070928     C* ==
050200160711     C                   EVAL      VABIND=%trim(%subst(vindta:336:40))
050300160711     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
050400160701     C                   EVAL      VABNOT=%trim(%subst(vindta:491:40))
050500160711     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
050600160711     C                   SELECT
050700160711     C                   WHEN      %subst(vindta:43:1) = 'P'
050800160711     C                   EVAL      VABCBO = '2'
050900160711     C***                WHEN      %subst(vindta:43:1) = 'T'
051000160711     C***                EVAL      VABCBO = ????
051100160711     C                   ENDSL
051200160711     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
051300161019     C***                EVAL      VABTIC=%trim(%subst(vindta:511:3))
051400160711     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
051500160711     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
051600160711     C                   EVAL      VABNZD=%trim(%subst(vindta:615:3))
051700160711     C                   IF        VABNZD = 'I'    OR
051800160711     C                             VABNZD = 'IT'   OR
051900160711     C                             VABNZD = 'ITA'
052000160711     C                   EVAL      VABNZD = *blank
052100160711     C                   ENDIF
052200160711     C                   EVAL      VABTSP=%trim(%subst(vindta:660:3))
052300160711     C                   EVAL      VABNT2=%trim(%subst(vindta:759:50))
052400160711     C                   EVAL      VABFFD=%trim(%subst(vindta:836:1))
052500160726     C*
052600160711     C                   IF        %subst(vindta:1:7) <> *blank
052700160711     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
052800160711     C                   EXSR      CHKNUM
052900160711     C                   IF        PiInt=*on
053000160711     C                   Z-ADD     PiVal         VABCCM
053100160711     C                   Z-ADD     PiVal         VATCCM
053200160711     C                   ENDIF
053300160711     C                   ENDIF
053400160727     C*
053500160727     C                   IF        VABCCM = 0241160
053600160727     C                   EVAL      VABCTR = 000
053700160727     C                   ENDIF
053800160726     C*
053900160727     C                   IF        %subst(vindta:837:10) <> *blank
054000160727     C                   EVAL      PiStr=%trim(%subst(vindta:837:10))
054100160726     C                   EXSR      CHKNUM
054200160726     C                   IF        PiInt=*on
054300160726     C                   Z-ADD     PiVal         VABLNP
054400160726     C                   Z-ADD     PiVal         VATLNP
054500160802     C                   Z-ADD     PiVal         VABFGS
054600160802     C                   Z-ADD     PiVal         VATFGS
054700160802     C                   Z-ADD     PiVal         wPOI
054800160726     C                   ENDIF
054900160726     C                   ENDIF
055000160711     C* AAS/MGS
055100160711     C* se sbagliato ciò che è scritto su file, uso la data odierna già impostata
055200160711     C                   IF        %trim(%subst(vindta:71:8)) <> *blank  and
055300160711     C                             %trim(%subst(vindta:71:8)) <> *zero
055400160711     C                   EVAL      PiStr=%trim(%subst(vindta:71:8))
055500070928     C                   EXSR      CHKNUM
055600070928     C                   IF        PiInt=*on
055700160711     C                   EVAL      VABAAS = %dec(%subst(
055800160711     C                                            %trim(%subst(vindta:71:8))
055900160711     C                                            : 1 : 4) : 4 : 0)
056000160711     C                   EVAL      VABMGS = %dec(%subst(
056100160711     C                                            %trim(%subst(vindta:71:8))
056200160711     C                                            : 5 : 4) : 4 : 0)
056300070928     C                   ELSE
056400160711     C***                SETON                                        32
056500160711     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
056600160711     C***                          + ' ' + 'VABAAS VABMGS'
056700070928     C                   ENDIF
056800160711     C                   ENDIF
056900160711     C* CAD
057000160711     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
057100160711     C                   EXSR      CHKNUM
057200160711     C                   IF        PiInt=*on
057300160711     C                   Z-ADD     PiVal         Num5_0
057400160711     C                   MOVEL     Num5_0        VABCAD
057500160711     C                   ELSE
057600160711     C                   SETON                                        32
057700160711     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057800160711     C                             + ' ' + 'VABCAD'
057900160711     C                   ENDIF
058000121113     C*
058100121113     C* Se provincia nn valorizzata la reperisco
058200121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
058300121113     C                   IF        VABPRD = *blanks
058400121113     C                   CLEAR                   TISI95DS
058500121113     C                   EVAL      I95TCN = '3'
058600121113     C                   Z-ADD     datcor        I95DAT
058700121113     C                   EVAL      I95NAR = VABNZD
058800121113     C                   EVAL      I95CAP = VABCAD
058900121113     C                   EVAL      I95LOC = VABLOD
059000121113     C                   CALL      'TISI95R'
059100121113     C                   PARM                    TISI95DS
059200121113     C                   EVAL      VABPRD = O95PRV
059300121113     C                   ENDIF
059400121113     C* RMN
059500160802     C                   EVAL      PiStr=%trim(%subst(
059600160802     C                                   %subst(vindta:866:20):7:10))
059700121113     C                   EXSR      CHKNUM
059800121113     C                   IF        PiInt=*on
059900121113     C                   Z-ADD     PiVal         VABRMN
060000121113     C                   ELSE
060100121113     C                   SETON                                        32
060200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060300121113     C                             + ' ' + 'VABRMN'
060400121113     C                   ENDIF
060500121113     C* NCL
060600160711     C                   EVAL      PiStr=%trim(%subst(vindta:820:5))
060700121113     C                   EXSR      CHKNUM
060800121113     C                   IF        PiInt=*on
060900121113     C                   Z-ADD     PiVal         VABNCL
061000121113     C                   ELSE
061100121113     C                   SETON                                        32
061200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061300121113     C                             + ' ' + 'VABNCL'
061400121113     C                   ENDIF
061500160711     C* PKB (espresso in g.)
061600160727     C                   EVAL      PiStr=%trim(%subst(vindta:857:9))
061700121113     C                   EXSR      CHKNUM
061800121113     C                   IF        PiNum=*on
061900160711     C                   EVAL(H)   VABPKB=PiVal/1000
062000121113     C                   ELSE
062100121113     C                   SETON                                        32
062200121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
062300121113     C                             + ' ' + 'VABPKB'
062400121113     C                   ENDIF
062500160711     C* VLB (espresso in cm3) - 1 M3 = 1.000.000 cm3
062600160802     C                   IF        %subst(vindta:896:9) <> *blank AND
062700160802     C                             %subst(vindta:896:9) <> *zeros
062800160727     C                   EVAL      PiStr=%trim(%subst(vindta:896:9))
062900160701     C                   EXSR      CHKNUM
063000160701     C                   IF        PiNum=*on
063100160711     C                   EVAL(H)   VABVLB=PiVal/1000000
063200160701     C                   ELSE
063300160701     C                   SETON                                        32
063400160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
063500160701     C                             + ' ' + 'VABVLB'
063600160701     C                   ENDIF
063700160711     C                   ENDIF
063800121113     C* CAS
063900160711     C                   IF        %subst(vindta:502:9) <> *blanks and
064000160711     C                             %subst(vindta:502:9) <> *zeros
064100121113     C                   MOVEL     '1'           FlgCAS
064200160711     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
064300150527     C                   EVAL      VABVCA='EUR'
064400121113     C                   EXSR      CHKNUM
064500121113     C                   IF        PiNum=*on
064600121113     C                   Z-ADD     PiVal         VABCAS
064700121113     C                   ELSE
064800121113     C                   SETON                                        32
064900121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
065000121113     C                             + ' ' + 'VABCAS'
065100121113     C                   ENDIF
065200121113     C                   ENDIF
065300160701     C* VMD
065400160711     C                   IF        %subst(vindta:618:10) <> *blanks and
065500160711     C                             %subst(vindta:618:10) <> *zeros
065600160711     C                   EVAL      PiStr=%trim(%subst(vindta:618:10))
065700160701     C                   EVAL      VABVAD='EUR'
065800160701     C                   EXSR      CHKNUM
065900160701     C                   IF        PiNum=*on
066000160701     C                   Z-ADD     PiVal         VABVMD
066100160701     C                   ELSE
066200160701     C                   SETON                                        32
066300160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
066400160701     C                             + ' ' + 'VABVMD'
066500160701     C                   ENDIF
066600160701     C                   ENDIF
066700160711     C* IAS
066800160711     C                   IF        %subst(vindta:544:10) <> *blanks and
066900160711     C                             %subst(vindta:544:10) <> *zeros
067000160711     C                   MOVEL     '1'           FlgCAS
067100160711     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
067200160711     C                   EVAL      VABVAS='EUR'
067300160711     C                   EXSR      CHKNUM
067400160711     C                   IF        PiNum=*on
067500160711     C                   Z-ADD     PiVal         VABIAS
067600160711     C                   ELSE
067700160711     C                   SETON                                        32
067800160711     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
067900160711     C                             + ' ' + 'VABIAS'
068000160711     C                   ENDIF
068100160711     C                   ENDIF
068200070730     C*
068300070730     C* Considerazioni finali su CBO/CAS
068400121113     C                   IF        FlgCAS <> '0'
068500070730     C                   IF        VABCBO = '1'
068600070730     C                   EVAL      VABCBO = '4'
068700070730     C                   ENDIF
068800070730     C                   IF        VABCBO = '2'
068900070730     C                   EVAL      VABCBO = '6'
069000070730     C                   ENDIF
069100070730     C                   ENDIF
069200020305     C*
069300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
069400011113     C                   EXSR      CHKIMPDIV
069500010202     C*
069600000801     C* Ebbene...
069700000801     C                   ADD       1             §CTRMO
069800070530     C                   IF        *in31 <> *zeros OR
069900070530     C                             *in32 <> *zeros
070000000801     C                   ADD       1             §CTRNO
070100020725     C                   EVAL      x_vinflg = '2'
070200000801     C                   ELSE
070300010201     C                   ADD       1             §CTROKVB
070400000801     C                   ENDIF
070500000801     C*
070600000801     C                   ENDSR
070700010201     C*----------------------------------------------------*
070800120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
070900010201     C*----------------------------------------------------*
071000071003     C     WRIVAT_B      BEGSR
071100090204     C*
071200090204     C                   eval      vatAAS = vabAAS
071300090204     C                   eval      vatLNP = vabLNP
071400090204     C                   eval      vatNRS = vabNRS
071500090204     C                   eval      vatNSP = vabNSP
071600010201     C*
071700120706     C* Valorizzo il buffer di scrittura del EDIVAT
071800070928     C* - TIPO RECORD "B"
071900160711     C                   if        %subst(vindta:376:15) <> *blanks
072000120706     C*
072100120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
072200160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
072300120706     C                   EVAL      VATCNT = 1
072400120706     C*
072500070928     C                   eval      VATTRC = 'B'
072600160711     C                   eval      VATNOT = %trim(%subst(vindta:376:15))
072700150527     C                   write     EDIVAT00
072800111031     C                   add       1             §ctrokvt
072900150527     C                   endif
073000010201     C*
073100010201     C                   ENDSR
073200150527     C*----------------------------------------------------*
073300150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "S"
073400150527     C*----------------------------------------------------*
073500150527     C     WRIVAT_S      BEGSR
073600150527     C*
073700150527     C                   eval      vatAAS = vabAAS
073800150527     C                   eval      vatLNP = vabLNP
073900150527     C                   eval      vatNRS = vabNRS
074000150527     C                   eval      vatNSP = vabNSP
074100150527     C*
074200150527     C* Valorizzo il buffer di scrittura del EDIVAT
074300150527     C* - TIPO RECORD "S"
074400160711     C                   if        %subst(vindta:713:14) <> *blanks
074500150527     C*
074600150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
074700160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
074800150527     C                   EVAL      VATCNT = 1
074900150527     C*
075000150527     C                   eval      VATTRC = 'S'
075100160711     C                   eval      VATNOT = %trim(%subst(vindta:713:14))
075200150527     C                   eval      %subst(VATNOT:17:2)=*blank
075300150527     C                   write     EDIVAT00
075400150527     C                   add       1             §ctrokvt
075500150527     C                   endif
075600150527     C*
075700150527     C                   ENDSR
075800150527     C*----------------------------------------------------*
075900150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
076000150527     C*----------------------------------------------------*
076100150527     C     WRIVAT_I      BEGSR
076200150527     C*
076300150527     C                   eval      vatAAS = vabAAS
076400150527     C                   eval      vatLNP = vabLNP
076500150527     C                   eval      vatNRS = vabNRS
076600150527     C                   eval      vatNSP = vabNSP
076700150527     C*
076800150527     C* Valorizzo il buffer di scrittura del EDIVAT
076900150527     C* - TIPO RECORD "I" e "J"
077000160711     C                   if        %subst(vindta:663:50) <> *blanks
077100160711     C                   eval      w70 = %trim(%subst(vindta:663:50))
077200150527     C*
077300150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
077400160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
077500150527     C                   EVAL      VATCNT = 1
077600150527     C*
077700150527     C                   eval      VATTRC = 'I'
077800150527     C                   eval      VATNOT = %subst(w70:1:35)
077900150527     C                   write     EDIVAT00
078000150527     C                   add       1             §ctrokvt
078100150527     C*
078200150527     C                   IF        %subst(w70:36:35) <> *blank
078300150527     C                   eval      VATTRC = 'J'
078400150527     C                   eval      VATNOT = %subst(w70:36:35)
078500150527     C                   write     EDIVAT00
078600150527     C                   add       1             §ctrokvt
078700150527     C                   endif
078800150527     C                   endif
078900150527     C*
079000150527     C                   ENDSR
079100071003     C*----------------------------------------------------*
079200120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
079300071003     C*----------------------------------------------------*
079400071003     C     WRIVAT_E      BEGSR
079500090204     C*
079600090204     C                   eval      vatAAS = vabAAS
079700090204     C                   eval      vatLNP = vabLNP
079800090204     C                   eval      vatNRS = vabNRS
079900090204     C                   eval      vatNSP = vabNSP
080000071003     C*
080100120706     C* Valorizzo il buffer di scrittura del EDIVAT
080200071003     C* - TIPO RECORD "E"
080300160711     C                   if        %subst(vindta:554:25) <> *blanks
080400120706     C*
080500120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
080600160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
080700120706     C                   EVAL      VATCNT = 1
080800120706     C*
080900071003     C                   eval      VATTRC = 'E'
081000160711     C                   eval      vatNOT = %trim(%subst(vindta:554:25))
081100120706     C                   write     EDIVAT00
081200111031     C                   add       1             §ctrokvt
081300150528     C                   endif
081400071003     C*
081500071003     C                   ENDSR
081600020904
081700020904
081800010202     C*----------------------------------------------------*
081900120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
082000010202     C*----------------------------------------------------*
082100020305     C     PREVAT        BEGSR
082200010202     C*
082300120706     C* Compongo il nome del membro da dare al EDIVATWR
082400010202     C                   eval      parmbr = vlrhdl
082500010202     C                   movel     'M'           parmbr
082600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
082700010202     C                   eval      paropz = '1'
082800010202     C* Effettuo la chiamata al CLLE preposto
082900120706     C                   call(e)   'TITVEVTC'
083000010202     C                   parm                    parccm
083100010202     C                   parm                    parmbr
083200010202     C                   parm                    paropz
083300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
083400010202     C                   if        %error
083500010202     C                   movel     '1'           chkcall
083600010202     C                   else
083700010202     C                   movel     '0'           chkcall
083800010202     C                   endif
083900010202     C*
084000010202     C                   ENDSR
084100000801     C*----------------------------------------------------*
084200000801     C*  CONTROLLO NUMERICITA' CAMPI
084300000801     C*----------------------------------------------------*
084400000801     C     CHKNUM        BEGSR
084500081113     C*
084600081113     C                   IF        PiDecChr = *blanks
084700121113     C                   EVAL      PiDecChr = '.'
084800081113     C                   ENDIF
084900081113     C*
085000081113     C                   callp(e)  UBISNUM_Check(PiStr
085100081113     C                                          :PiDecChr
085200081113     C                                          :PiVal
085300081113     C                                          :PiNum
085400081113     C                                          :PiInt)
085500081113     C*
085600000801     C                   IF        %error
085700000801     C                   EVAL      PiInt=*off
085800000801     C                   ENDIF
085900000801     C*
086000000801     C                   ENDSR
086100000801     C***
086200000801
086300011113
086400011113     C*----------------------------------------------------*
086500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
086600011113     C*----------------------------------------------------*
086700011113     C     CHKIMPDIV     BEGSR
086800011113     C*
086900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
087000011113     C                   Z-ADD     *zeros        wrkDec            9 9
087100011113     C*
087200011113     C* Come prima cosa effettuo considerazioni sulla divisa
087300011113     C                   IF        vabIAS > *zeros
087400011113     C                   IF        vabVAS <> 'EUR'
087500011113     C                   EVAL      vabVAS =  'ITL'
087600011113     C                   ENDIF
087700011113     C                   ENDIF
087800011113     C*
087900011113     C                   IF        vabCAS > *zeros
088000011113     C                   IF        vabVCA <> 'EUR'
088100011113     C                   EVAL      vabVCA =  'ITL'
088200011113     C                   ENDIF
088300011113     C                   ENDIF
088400011113     C*
088500011113     C                   IF        vabVMD > *zeros
088600020305     C                   IF        vabVAD <> 'EUR'
088700011113     C                   EVAL      vabVAD =  'ITL'
088800011113     C                   ENDIF
088900011113     C                   ENDIF
089000011113     C*
089100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
089200011113     C                   Z-ADD     vabIAS        wrkDec
089300011113     C                   IF        wrkDec > *zeros
089400011113     C                   IF        vabVAS = 'ITL'
089500011113     C                   EVAL      vabIAS = *zeros
089600011113     C                   ENDIF
089700011113     C                   ENDIF
089800011113     C*
089900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
090000011113     C                   Z-ADD     vabCAS        wrkDec
090100011113     C                   IF        wrkDec > *zeros
090200011113     C                   IF        vabVCA = 'ITL'
090300011113     C                   EVAL      vabCAS = *zeros
090400011113     C                   ENDIF
090500011113     C                   ENDIF
090600011113     C*
090700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
090800011113     C                   Z-ADD     vabVMD        wrkDec
090900011113     C                   IF        wrkDec > *zeros
091000011113     C                   IF        vabVAD = 'ITL'
091100011113     C                   EVAL      vabVMD = *zeros
091200011113     C                   ENDIF
091300011113     C                   ENDIF
091400011113     C*
091500011113     C                   ENDSR
091600011113     C***
091700011113
091800011113
091900000801
092000000801
092100990920      /TITLE Invio dei dati al punto operativo.
092200010202     C     invio         BEGSR
092300990920     C*
092400120706     C* 1° invio EDIVAT
092500010201     C                   reset                   dscmz
092600111031     C                   move      wPOI          cmzdst
092700120706     C                   eval      cmzfld = 'EDIVATWR'
092800010201     C                   eval      cmzmbd = vlrhdl
092900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
093000021009     C***                if        prmfir = *blanks
093100120706     C                   eval      cmzfla = 'EDIVAT0F'
093200120706     C                   eval      cmzmba = 'EDIVAT0F'
093300021009     C***                else
093400021009     C***                eval      cmzfla = prmfir
093500021009     C***                eval      cmzmba = prmfir
093600021009     C***                endif
093700010201     C                   eval      cmznrr = *zeros
093800020305     C                   move      §ctrokvt      cmznrr
093900021018     C                   eval      cmzlba = vlrfl1
094000010201     C                   call(e)   'TIS711C'
094100010201     C                   parm                    dscmz
094200010201     C                   parm      *blanks       esito
094300010205     C                   if        %error
094400010205     C                             or cmzerr = '1'
094500010205     C                             or esito  = '1'
094600010205     C                   eval      wrkesito = '3'
094700010205     C                   else
094800010201     C*
094900120706     C* 2° invio EDIVAB
095000010201     C                   reset                   dscmz
095100111031     C                   move      wPOI          cmzdst
095200010201     C                   eval      cmzfld = vlrfou
095300010201     C                   eval      cmzmbd = vlrhdl
095400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
095500021009     C***                if        prmfir = *blanks
095600120712     C                   eval      cmzfla = 'EDIVAB0F'
095700120712     C                   eval      cmzmba = 'EDIVAB0F'
095800021009     C***                else
095900021009     C***                eval      cmzfla = prmfir
096000021009     C***                eval      cmzmba = prmfir
096100021009     C***                endif
096200010201     C                   eval      cmznrr = *zeros
096300010201     C                   move      §ctrokvb      cmznrr
096400021018     C                   eval      cmzlba = vlrfl1
096500010201     C                   call(e)   'TIS711C'
096600010201     C                   parm                    dscmz
096700010201     C                   parm      *blanks       esito
096800010201     C                   if        %error
096900010201     C                             or cmzerr = '1'
097000010201     C                             or esito  = '1'
097100010201     C                   eval      wrkesito = '3'
097200010201     C                   endif
097300010205     C                   endif
097400990920     C*
097500000613     C                   ENDSR
097600000613     C***
097700070411
097800070411     C     *pssr         BEGSR
097900070411     C*
098000070411     C                   if        %open(tivin00r)
098100070411     C                   close     tivin00r
098200070411     C                   endif
098300120706     C                   if        %open(edivabwr)
098400120706     C                   close     edivabwr
098500120706     C                   endif
098600120706     C                   if        %open(edivatwr)
098700120706     C                   close     edivatwr
098800070411     C                   endif
098900070411     C*
099000070411     C* Effettuo la chiamata al CLLE preposto
099100120706     C                   call(e)   'TITVEVTC'
099200070411     C                   parm                    parccm
099300070411     C                   parm                    parmbr
099400070411     C                   parm      '2'           paropz
099500070411     C*
099600070411     C                   eval      wrkesito = '2'
099700070411     C*
099800070411     C                   seton                                        LR
099900070411     C*
100000070411     C                   ENDSR     '*CANCL'
100100070411     C***
100200070411
100300990910
100400000613     C     *inzsr        BEGSR
100500990910     C*
100600990910     C     *entry        plist
100700990920     C                   parm                    tivlrds
100800990921     C                   parm      wrkesito      esito
100900000724     C                   parm                    prmlit
101000000710     C                   parm                    prmfir
101100000613     C*
101200000830     C* CALCOLA LA DATA CORRENTE
101300120706     C                   time                    wn14             14 0
101400120706     C                   movel     wn14          oracor            6 0          *ORA
101500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
101600100324     C                   eval      datcor = %dec(%date() : *ISO)
101700000830     C*
101800000613     C                   ENDSR
101900000613     C***
