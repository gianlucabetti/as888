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
034200171130     C***                          ok_VABCCM <> 0241160
034300171130     C* questi soono tutti i clienti teddy a questa data che devono avere la gestione pdf
034400171130     C                             (ok_VABCCM = 1311537 or
034500171130     C                              ok_VABCCM = 0662209 or
034600171130     C                              ok_VABCCM = 0652085 or
034700171130     C                              ok_VABCCM = 0240073)
034800170705     C                   eval      ok_VABCTM = '7T'
034900170705     C                   eval      edivabds = fivabds_ok
035000170705     C                   eval      pIn_MaskPDF = %trim(ok_VABRMA)+'.pdf'
035100170705     C                   call      'TIS7P2SER'
035200170705     C                   parm      'W'           pIn_Opz           1
035300170705     C                   parm                    tivlrds
035400170705     C                   parm                    EDIVABDS
035500170705     C                   parm      '10'          pIn_CdIdAz        2
035600170705     C                   parm                    pIn_MaskPDF      50
035700170705     C                   parm      *blanks       pOut_Esito        1
035800170705     C                   endif
035900170705     C                   endif
036000170705     C***
036100071003     C*
036200120706     C                   write     edivab00                                     => scarico il VAB
036300010305     C*
036400010305     C                   ENDSR
036500990920
036600000801     C*----------------------------------------------------*
036700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036800000801     C*----------------------------------------------------*
036900010201     C     INZVAR        BEGSR
037000000801     C*
037100010201     C                   Z-ADD     *zeros        Num5_0
037200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
037300020725     C                   MOVEL     '0'           FlgCAS            1
037400111125     C*
037500111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
037600111130     C*
037700111125     C                   CLEAR                   FIVABDS
037800111130     C                   CLEAR                   FIVABDS_OK
037900111130     C                   CLEAR                   FIVABDS_SAV
038000120706     C                   CLEAR                   EDIVAB00
038100120706     C                   CLEAR                   EDIVAT00
038200000801     C*
038300000801     C                   ENDSR
038400000801     C*----------------------------------------------------*
038500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
038600000801     C*----------------------------------------------------*
038700020904     C     DEFCAM        BEGSR
038800080923     C*
038900111125     C                   SETOFF                                       50
039000170705     C                   SETOFF                                       515254
039100170705     C                   SETOFF                                       53
039200070730     C* Imposto i valori di default...
039300160711     C                   EVAL      VABCCM = 0241160
039400160711     C                   EVAL      VATCCM = 0241160
039500160711     C                   EVAL      VABLNP = 024
039600160711     C                   EVAL      VATLNP = 024
039700160727     C                   EVAL      VABCTR = 100
039800160701     C                   EVAL      VABCBO = '1'
039900160711     C                   EVAL      VABCTM = '7Q'
040000070222     C* ... e poi verifico se sono stati passati come parametri
040100070222     C                   IF        vlrppt > *blanks
040200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
040300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
040400070222     C                   EXSR      CHKNUM
040500070222     C                   IF        PiInt=*on
040600070222     C                   Z-ADD     PiVal         VABCCM
040700070222     C                   Z-ADD     PiVal         VATCCM
040800070222     C                   ENDIF
040900070222     C                   ENDIF
041000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
041100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
041200070222     C                   EXSR      CHKNUM
041300070222     C                   IF        PiInt=*on
041400070222     C                   Z-ADD     PiVal         VABLNP
041500070222     C                   Z-ADD     PiVal         VATLNP
041600070222     C                   ENDIF
041700070222     C                   ENDIF
041800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
041900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
042000070222     C                   EXSR      CHKNUM
042100070222     C                   IF        PiInt=*on
042200070222     C                   Z-ADD     PiVal         VABCTR
042300070222     C                   ENDIF
042400070928     C                   ENDIF
042500071009     C                   MOVEL     *blanks       wDISK             1
042600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042700071009     C                   IF        wDISK <> *blanks
042800070928     C                   SETON                                        50
042900070222     C                   ENDIF
043000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
043100111125     C                   SETON                                        41
043200080923     C                   ENDIF
043300170705     C                   IF        %subst(vlrppt:16:1) = 'S'
043400170705     C                   SETON                                        53
043500170705     C                   ELSE
043600170705     C                   SETOFF                                       53
043700170705     C                   ENDIF
043800070222     C                   ENDIF
043900120706     C*
044000120706     C* valido per nr.colli/peso/volume
044100120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
044200120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
044300120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
044400120706     C                   SELECT
044500120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
044600120706     C                   SETON                                        51
044700120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
044800120706     C                   SETON                                        52
044900120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
045000170705     C                   SETON                                        54
045100120706     C                   ENDSL
045200120706     C*
045300071009     C   50              EVAL      VABCTM = '7Q'
045400000801     C*
045500000801     C                   ENDSR
045600150527     C*----------------------------------------------------*
045700150527     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
045800150527     C*----------------------------------------------------*
045900150527     C     REPNSP        BEGSR
046000150527     C*
046100150527     C                   EXSR      INZVAR
046200150527     C                   EXSR      DEFCAM
046300150527     C*
046400150527     C                   SETON                                        60
046500150527     C*
046600150527     C* NSP => Stacco un numeratore da AZNUM
046700150527     C                   clear                   TRUL33DS
046800150527     C                   eval      I33OPE = *zeros
046900150527     C                   eval      I33CNU = 302
047000150527     C                   eval      I33NUM = 1
047100150527     C                   movel     TRUL33DS      KPJBU
047200150527     C                   call      'TRUL33R'
047300150527     C                   parm                    KPJBA
047400150527     C                   movel     KPJBU         TRUL33DS
047500150527     C                   if        O33ERR = *zeros
047600150527     C                   z-add     O33NRF        VABNSP
047700150527     C                   z-add     O33NRF        VATNSP
047800150527     C                   else
047900150527     C                   SETON                                        31
048000150527     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100150527     C                             + ' ' + 'VABNSP VATNSP'
048200150527     C                   endif
048300150527     C*
048400150527     C                   ENDSR
048500000801     C*----------------------------------------------------*
048600120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
048700000801     C*----------------------------------------------------*
048800010201     C     IMPVAB        BEGSR
048900070530     C*
049000070530     C                   SETOFF                                       3132
049100070928     C*
049200070928     C                   EXSR      DEFCAM
049300070928     C*
049400111031     C                   IF        vlrpoi <> 999
049500070928     C                   MOVE(P)   vlrpoi        VABFGS
049600070928     C                   MOVE(P)   vlrpoi        VATFGS
049700111031     C                   Z-ADD     vlrpoi        wPOI              3 0
049800111031     C                   ENDIF
049900070928     C*
050000070928     C                   MOVEL     datcor        VABAAS
050100070928     C                   MOVEL     datcor        VATAAS
050200070928     C                   MOVE      datcor        VABMGS
050300160711     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
050400070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
050500070928     C     '@':'A'       XLATE     VABRSD        VABRSD
050600070928     C* ==
050700160711     C                   EVAL      VABIND=%trim(%subst(vindta:336:40))
050800160711     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
050900160701     C                   EVAL      VABNOT=%trim(%subst(vindta:491:40))
051000160711     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
051100160711     C                   SELECT
051200160711     C                   WHEN      %subst(vindta:43:1) = 'P'
051300160711     C                   EVAL      VABCBO = '2'
051400160711     C***                WHEN      %subst(vindta:43:1) = 'T'
051500160711     C***                EVAL      VABCBO = ????
051600160711     C                   ENDSL
051700160711     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
051800161019     C***                EVAL      VABTIC=%trim(%subst(vindta:511:3))
051900160711     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
052000160711     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
052100160711     C                   EVAL      VABNZD=%trim(%subst(vindta:615:3))
052200160711     C                   IF        VABNZD = 'I'    OR
052300160711     C                             VABNZD = 'IT'   OR
052400160711     C                             VABNZD = 'ITA'
052500160711     C                   EVAL      VABNZD = *blank
052600160711     C                   ENDIF
052700160711     C                   EVAL      VABTSP=%trim(%subst(vindta:660:3))
052800160711     C                   EVAL      VABNT2=%trim(%subst(vindta:759:50))
052900160711     C                   EVAL      VABFFD=%trim(%subst(vindta:836:1))
053000160726     C*
053100160711     C                   IF        %subst(vindta:1:7) <> *blank
053200160711     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
053300160711     C                   EXSR      CHKNUM
053400160711     C                   IF        PiInt=*on
053500160711     C                   Z-ADD     PiVal         VABCCM
053600160711     C                   Z-ADD     PiVal         VATCCM
053700160711     C                   ENDIF
053800160711     C                   ENDIF
053900160727     C*
054000160727     C                   IF        VABCCM = 0241160
054100160727     C                   EVAL      VABCTR = 000
054200160727     C                   ENDIF
054300160726     C*
054400160727     C                   IF        %subst(vindta:837:10) <> *blank
054500160727     C                   EVAL      PiStr=%trim(%subst(vindta:837:10))
054600160726     C                   EXSR      CHKNUM
054700160726     C                   IF        PiInt=*on
054800160726     C                   Z-ADD     PiVal         VABLNP
054900160726     C                   Z-ADD     PiVal         VATLNP
055000160802     C                   Z-ADD     PiVal         VABFGS
055100160802     C                   Z-ADD     PiVal         VATFGS
055200160802     C                   Z-ADD     PiVal         wPOI
055300160726     C                   ENDIF
055400160726     C                   ENDIF
055500160711     C* AAS/MGS
055600160711     C* se sbagliato ciò che è scritto su file, uso la data odierna già impostata
055700160711     C                   IF        %trim(%subst(vindta:71:8)) <> *blank  and
055800160711     C                             %trim(%subst(vindta:71:8)) <> *zero
055900160711     C                   EVAL      PiStr=%trim(%subst(vindta:71:8))
056000070928     C                   EXSR      CHKNUM
056100070928     C                   IF        PiInt=*on
056200160711     C                   EVAL      VABAAS = %dec(%subst(
056300160711     C                                            %trim(%subst(vindta:71:8))
056400160711     C                                            : 1 : 4) : 4 : 0)
056500160711     C                   EVAL      VABMGS = %dec(%subst(
056600160711     C                                            %trim(%subst(vindta:71:8))
056700160711     C                                            : 5 : 4) : 4 : 0)
056800070928     C                   ELSE
056900160711     C***                SETON                                        32
057000160711     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
057100160711     C***                          + ' ' + 'VABAAS VABMGS'
057200070928     C                   ENDIF
057300160711     C                   ENDIF
057400160711     C* CAD
057500160711     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
057600160711     C                   EXSR      CHKNUM
057700160711     C                   IF        PiInt=*on
057800160711     C                   Z-ADD     PiVal         Num5_0
057900160711     C                   MOVEL     Num5_0        VABCAD
058000160711     C                   ELSE
058100160711     C                   SETON                                        32
058200160711     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058300160711     C                             + ' ' + 'VABCAD'
058400160711     C                   ENDIF
058500121113     C*
058600121113     C* Se provincia nn valorizzata la reperisco
058700121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
058800121113     C                   IF        VABPRD = *blanks
058900121113     C                   CLEAR                   TISI95DS
059000121113     C                   EVAL      I95TCN = '3'
059100121113     C                   Z-ADD     datcor        I95DAT
059200121113     C                   EVAL      I95NAR = VABNZD
059300121113     C                   EVAL      I95CAP = VABCAD
059400121113     C                   EVAL      I95LOC = VABLOD
059500121113     C                   CALL      'TISI95R'
059600121113     C                   PARM                    TISI95DS
059700121113     C                   EVAL      VABPRD = O95PRV
059800121113     C                   ENDIF
059900121113     C* RMN
060000160802     C                   EVAL      PiStr=%trim(%subst(
060100160802     C                                   %subst(vindta:866:20):7:10))
060200121113     C                   EXSR      CHKNUM
060300121113     C                   IF        PiInt=*on
060400121113     C                   Z-ADD     PiVal         VABRMN
060500121113     C                   ELSE
060600121113     C                   SETON                                        32
060700121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060800121113     C                             + ' ' + 'VABRMN'
060900121113     C                   ENDIF
061000121113     C* NCL
061100160711     C                   EVAL      PiStr=%trim(%subst(vindta:820:5))
061200121113     C                   EXSR      CHKNUM
061300121113     C                   IF        PiInt=*on
061400121113     C                   Z-ADD     PiVal         VABNCL
061500121113     C                   ELSE
061600121113     C                   SETON                                        32
061700121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061800121113     C                             + ' ' + 'VABNCL'
061900121113     C                   ENDIF
062000160711     C* PKB (espresso in g.)
062100160727     C                   EVAL      PiStr=%trim(%subst(vindta:857:9))
062200121113     C                   EXSR      CHKNUM
062300121113     C                   IF        PiNum=*on
062400160711     C                   EVAL(H)   VABPKB=PiVal/1000
062500121113     C                   ELSE
062600121113     C                   SETON                                        32
062700121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
062800121113     C                             + ' ' + 'VABPKB'
062900121113     C                   ENDIF
063000160711     C* VLB (espresso in cm3) - 1 M3 = 1.000.000 cm3
063100160802     C                   IF        %subst(vindta:896:9) <> *blank AND
063200160802     C                             %subst(vindta:896:9) <> *zeros
063300160727     C                   EVAL      PiStr=%trim(%subst(vindta:896:9))
063400160701     C                   EXSR      CHKNUM
063500160701     C                   IF        PiNum=*on
063600160711     C                   EVAL(H)   VABVLB=PiVal/1000000
063700160701     C                   ELSE
063800160701     C                   SETON                                        32
063900160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
064000160701     C                             + ' ' + 'VABVLB'
064100160701     C                   ENDIF
064200160711     C                   ENDIF
064300121113     C* CAS
064400160711     C                   IF        %subst(vindta:502:9) <> *blanks and
064500160711     C                             %subst(vindta:502:9) <> *zeros
064600121113     C                   MOVEL     '1'           FlgCAS
064700160711     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
064800150527     C                   EVAL      VABVCA='EUR'
064900121113     C                   EXSR      CHKNUM
065000121113     C                   IF        PiNum=*on
065100121113     C                   Z-ADD     PiVal         VABCAS
065200121113     C                   ELSE
065300121113     C                   SETON                                        32
065400121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
065500121113     C                             + ' ' + 'VABCAS'
065600121113     C                   ENDIF
065700121113     C                   ENDIF
065800160701     C* VMD
065900160711     C                   IF        %subst(vindta:618:10) <> *blanks and
066000160711     C                             %subst(vindta:618:10) <> *zeros
066100160711     C                   EVAL      PiStr=%trim(%subst(vindta:618:10))
066200160701     C                   EVAL      VABVAD='EUR'
066300160701     C                   EXSR      CHKNUM
066400160701     C                   IF        PiNum=*on
066500160701     C                   Z-ADD     PiVal         VABVMD
066600160701     C                   ELSE
066700160701     C                   SETON                                        32
066800160701     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
066900160701     C                             + ' ' + 'VABVMD'
067000160701     C                   ENDIF
067100160701     C                   ENDIF
067200160711     C* IAS
067300160711     C                   IF        %subst(vindta:544:10) <> *blanks and
067400160711     C                             %subst(vindta:544:10) <> *zeros
067500160711     C                   MOVEL     '1'           FlgCAS
067600160711     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
067700160711     C                   EVAL      VABVAS='EUR'
067800160711     C                   EXSR      CHKNUM
067900160711     C                   IF        PiNum=*on
068000160711     C                   Z-ADD     PiVal         VABIAS
068100160711     C                   ELSE
068200160711     C                   SETON                                        32
068300160711     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
068400160711     C                             + ' ' + 'VABIAS'
068500160711     C                   ENDIF
068600160711     C                   ENDIF
068700070730     C*
068800070730     C* Considerazioni finali su CBO/CAS
068900121113     C                   IF        FlgCAS <> '0'
069000070730     C                   IF        VABCBO = '1'
069100070730     C                   EVAL      VABCBO = '4'
069200070730     C                   ENDIF
069300070730     C                   IF        VABCBO = '2'
069400070730     C                   EVAL      VABCBO = '6'
069500070730     C                   ENDIF
069600070730     C                   ENDIF
069700020305     C*
069800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
069900011113     C                   EXSR      CHKIMPDIV
070000010202     C*
070100000801     C* Ebbene...
070200000801     C                   ADD       1             §CTRMO
070300070530     C                   IF        *in31 <> *zeros OR
070400070530     C                             *in32 <> *zeros
070500000801     C                   ADD       1             §CTRNO
070600020725     C                   EVAL      x_vinflg = '2'
070700000801     C                   ELSE
070800010201     C                   ADD       1             §CTROKVB
070900000801     C                   ENDIF
071000000801     C*
071100000801     C                   ENDSR
071200010201     C*----------------------------------------------------*
071300120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
071400010201     C*----------------------------------------------------*
071500071003     C     WRIVAT_B      BEGSR
071600090204     C*
071700090204     C                   eval      vatAAS = vabAAS
071800090204     C                   eval      vatLNP = vabLNP
071900090204     C                   eval      vatNRS = vabNRS
072000090204     C                   eval      vatNSP = vabNSP
072100010201     C*
072200120706     C* Valorizzo il buffer di scrittura del EDIVAT
072300070928     C* - TIPO RECORD "B"
072400160711     C                   if        %subst(vindta:376:15) <> *blanks
072500120706     C*
072600120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
072700160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
072800120706     C                   EVAL      VATCNT = 1
072900120706     C*
073000070928     C                   eval      VATTRC = 'B'
073100160711     C                   eval      VATNOT = %trim(%subst(vindta:376:15))
073200150527     C                   write     EDIVAT00
073300111031     C                   add       1             §ctrokvt
073400150527     C                   endif
073500010201     C*
073600010201     C                   ENDSR
073700150527     C*----------------------------------------------------*
073800150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "S"
073900150527     C*----------------------------------------------------*
074000150527     C     WRIVAT_S      BEGSR
074100150527     C*
074200150527     C                   eval      vatAAS = vabAAS
074300150527     C                   eval      vatLNP = vabLNP
074400150527     C                   eval      vatNRS = vabNRS
074500150527     C                   eval      vatNSP = vabNSP
074600150527     C*
074700150527     C* Valorizzo il buffer di scrittura del EDIVAT
074800150527     C* - TIPO RECORD "S"
074900160711     C                   if        %subst(vindta:713:14) <> *blanks
075000150527     C*
075100150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
075200160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
075300150527     C                   EVAL      VATCNT = 1
075400150527     C*
075500150527     C                   eval      VATTRC = 'S'
075600160711     C                   eval      VATNOT = %trim(%subst(vindta:713:14))
075700150527     C                   eval      %subst(VATNOT:17:2)=*blank
075800150527     C                   write     EDIVAT00
075900150527     C                   add       1             §ctrokvt
076000150527     C                   endif
076100150527     C*
076200150527     C                   ENDSR
076300150527     C*----------------------------------------------------*
076400150527     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I" e "J"
076500150527     C*----------------------------------------------------*
076600150527     C     WRIVAT_I      BEGSR
076700150527     C*
076800150527     C                   eval      vatAAS = vabAAS
076900150527     C                   eval      vatLNP = vabLNP
077000150527     C                   eval      vatNRS = vabNRS
077100150527     C                   eval      vatNSP = vabNSP
077200150527     C*
077300150527     C* Valorizzo il buffer di scrittura del EDIVAT
077400150527     C* - TIPO RECORD "I" e "J"
077500160711     C                   if        %subst(vindta:663:50) <> *blanks
077600160711     C                   eval      w70 = %trim(%subst(vindta:663:50))
077700150527     C*
077800150527     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
077900160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
078000150527     C                   EVAL      VATCNT = 1
078100150527     C*
078200150527     C                   eval      VATTRC = 'I'
078300150527     C                   eval      VATNOT = %subst(w70:1:35)
078400150527     C                   write     EDIVAT00
078500150527     C                   add       1             §ctrokvt
078600150527     C*
078700150527     C                   IF        %subst(w70:36:35) <> *blank
078800150527     C                   eval      VATTRC = 'J'
078900150527     C                   eval      VATNOT = %subst(w70:36:35)
079000150527     C                   write     EDIVAT00
079100150527     C                   add       1             §ctrokvt
079200150527     C                   endif
079300150527     C                   endif
079400150527     C*
079500150527     C                   ENDSR
079600071003     C*----------------------------------------------------*
079700120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
079800071003     C*----------------------------------------------------*
079900071003     C     WRIVAT_E      BEGSR
080000090204     C*
080100090204     C                   eval      vatAAS = vabAAS
080200090204     C                   eval      vatLNP = vabLNP
080300090204     C                   eval      vatNRS = vabNRS
080400090204     C                   eval      vatNSP = vabNSP
080500071003     C*
080600120706     C* Valorizzo il buffer di scrittura del EDIVAT
080700071003     C* - TIPO RECORD "E"
080800160711     C                   if        %subst(vindta:554:25) <> *blanks
080900120706     C*
081000120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
081100160711     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
081200120706     C                   EVAL      VATCNT = 1
081300120706     C*
081400071003     C                   eval      VATTRC = 'E'
081500160711     C                   eval      vatNOT = %trim(%subst(vindta:554:25))
081600120706     C                   write     EDIVAT00
081700111031     C                   add       1             §ctrokvt
081800150528     C                   endif
081900071003     C*
082000071003     C                   ENDSR
082100020904
082200020904
082300010202     C*----------------------------------------------------*
082400120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
082500010202     C*----------------------------------------------------*
082600020305     C     PREVAT        BEGSR
082700010202     C*
082800120706     C* Compongo il nome del membro da dare al EDIVATWR
082900010202     C                   eval      parmbr = vlrhdl
083000010202     C                   movel     'M'           parmbr
083100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
083200010202     C                   eval      paropz = '1'
083300010202     C* Effettuo la chiamata al CLLE preposto
083400120706     C                   call(e)   'TITVEVTC'
083500010202     C                   parm                    parccm
083600010202     C                   parm                    parmbr
083700010202     C                   parm                    paropz
083800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
083900010202     C                   if        %error
084000010202     C                   movel     '1'           chkcall
084100010202     C                   else
084200010202     C                   movel     '0'           chkcall
084300010202     C                   endif
084400010202     C*
084500010202     C                   ENDSR
084600000801     C*----------------------------------------------------*
084700000801     C*  CONTROLLO NUMERICITA' CAMPI
084800000801     C*----------------------------------------------------*
084900000801     C     CHKNUM        BEGSR
085000081113     C*
085100081113     C                   IF        PiDecChr = *blanks
085200121113     C                   EVAL      PiDecChr = '.'
085300081113     C                   ENDIF
085400081113     C*
085500081113     C                   callp(e)  UBISNUM_Check(PiStr
085600081113     C                                          :PiDecChr
085700081113     C                                          :PiVal
085800081113     C                                          :PiNum
085900081113     C                                          :PiInt)
086000081113     C*
086100000801     C                   IF        %error
086200000801     C                   EVAL      PiInt=*off
086300000801     C                   ENDIF
086400000801     C*
086500000801     C                   ENDSR
086600000801     C***
086700000801
086800011113
086900011113     C*----------------------------------------------------*
087000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
087100011113     C*----------------------------------------------------*
087200011113     C     CHKIMPDIV     BEGSR
087300011113     C*
087400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
087500011113     C                   Z-ADD     *zeros        wrkDec            9 9
087600011113     C*
087700011113     C* Come prima cosa effettuo considerazioni sulla divisa
087800011113     C                   IF        vabIAS > *zeros
087900011113     C                   IF        vabVAS <> 'EUR'
088000011113     C                   EVAL      vabVAS =  'ITL'
088100011113     C                   ENDIF
088200011113     C                   ENDIF
088300011113     C*
088400011113     C                   IF        vabCAS > *zeros
088500011113     C                   IF        vabVCA <> 'EUR'
088600011113     C                   EVAL      vabVCA =  'ITL'
088700011113     C                   ENDIF
088800011113     C                   ENDIF
088900011113     C*
089000011113     C                   IF        vabVMD > *zeros
089100020305     C                   IF        vabVAD <> 'EUR'
089200011113     C                   EVAL      vabVAD =  'ITL'
089300011113     C                   ENDIF
089400011113     C                   ENDIF
089500011113     C*
089600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
089700011113     C                   Z-ADD     vabIAS        wrkDec
089800011113     C                   IF        wrkDec > *zeros
089900011113     C                   IF        vabVAS = 'ITL'
090000011113     C                   EVAL      vabIAS = *zeros
090100011113     C                   ENDIF
090200011113     C                   ENDIF
090300011113     C*
090400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
090500011113     C                   Z-ADD     vabCAS        wrkDec
090600011113     C                   IF        wrkDec > *zeros
090700011113     C                   IF        vabVCA = 'ITL'
090800011113     C                   EVAL      vabCAS = *zeros
090900011113     C                   ENDIF
091000011113     C                   ENDIF
091100011113     C*
091200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
091300011113     C                   Z-ADD     vabVMD        wrkDec
091400011113     C                   IF        wrkDec > *zeros
091500011113     C                   IF        vabVAD = 'ITL'
091600011113     C                   EVAL      vabVMD = *zeros
091700011113     C                   ENDIF
091800011113     C                   ENDIF
091900011113     C*
092000011113     C                   ENDSR
092100011113     C***
092200011113
092300011113
092400000801
092500000801
092600990920      /TITLE Invio dei dati al punto operativo.
092700010202     C     invio         BEGSR
092800990920     C*
092900120706     C* 1° invio EDIVAT
093000010201     C                   reset                   dscmz
093100111031     C                   move      wPOI          cmzdst
093200120706     C                   eval      cmzfld = 'EDIVATWR'
093300010201     C                   eval      cmzmbd = vlrhdl
093400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
093500021009     C***                if        prmfir = *blanks
093600120706     C                   eval      cmzfla = 'EDIVAT0F'
093700120706     C                   eval      cmzmba = 'EDIVAT0F'
093800021009     C***                else
093900021009     C***                eval      cmzfla = prmfir
094000021009     C***                eval      cmzmba = prmfir
094100021009     C***                endif
094200010201     C                   eval      cmznrr = *zeros
094300020305     C                   move      §ctrokvt      cmznrr
094400021018     C                   eval      cmzlba = vlrfl1
094500010201     C                   call(e)   'TIS711C'
094600010201     C                   parm                    dscmz
094700010201     C                   parm      *blanks       esito
094800010205     C                   if        %error
094900010205     C                             or cmzerr = '1'
095000010205     C                             or esito  = '1'
095100010205     C                   eval      wrkesito = '3'
095200010205     C                   else
095300010201     C*
095400120706     C* 2° invio EDIVAB
095500010201     C                   reset                   dscmz
095600111031     C                   move      wPOI          cmzdst
095700010201     C                   eval      cmzfld = vlrfou
095800010201     C                   eval      cmzmbd = vlrhdl
095900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
096000021009     C***                if        prmfir = *blanks
096100120712     C                   eval      cmzfla = 'EDIVAB0F'
096200120712     C                   eval      cmzmba = 'EDIVAB0F'
096300021009     C***                else
096400021009     C***                eval      cmzfla = prmfir
096500021009     C***                eval      cmzmba = prmfir
096600021009     C***                endif
096700010201     C                   eval      cmznrr = *zeros
096800010201     C                   move      §ctrokvb      cmznrr
096900021018     C                   eval      cmzlba = vlrfl1
097000010201     C                   call(e)   'TIS711C'
097100010201     C                   parm                    dscmz
097200010201     C                   parm      *blanks       esito
097300010201     C                   if        %error
097400010201     C                             or cmzerr = '1'
097500010201     C                             or esito  = '1'
097600010201     C                   eval      wrkesito = '3'
097700010201     C                   endif
097800010205     C                   endif
097900990920     C*
098000000613     C                   ENDSR
098100000613     C***
098200070411
098300070411     C     *pssr         BEGSR
098400070411     C*
098500070411     C                   if        %open(tivin00r)
098600070411     C                   close     tivin00r
098700070411     C                   endif
098800120706     C                   if        %open(edivabwr)
098900120706     C                   close     edivabwr
099000120706     C                   endif
099100120706     C                   if        %open(edivatwr)
099200120706     C                   close     edivatwr
099300070411     C                   endif
099400070411     C*
099500070411     C* Effettuo la chiamata al CLLE preposto
099600120706     C                   call(e)   'TITVEVTC'
099700070411     C                   parm                    parccm
099800070411     C                   parm                    parmbr
099900070411     C                   parm      '2'           paropz
100000070411     C*
100100070411     C                   eval      wrkesito = '2'
100200070411     C*
100300070411     C                   seton                                        LR
100400070411     C*
100500070411     C                   ENDSR     '*CANCL'
100600070411     C***
100700070411
100800990910
100900000613     C     *inzsr        BEGSR
101000990910     C*
101100990910     C     *entry        plist
101200990920     C                   parm                    tivlrds
101300990921     C                   parm      wrkesito      esito
101400000724     C                   parm                    prmlit
101500000710     C                   parm                    prmfir
101600000613     C*
101700000830     C* CALCOLA LA DATA CORRENTE
101800120706     C                   time                    wn14             14 0
101900120706     C                   movel     wn14          oracor            6 0          *ORA
102000100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
102100100324     C                   eval      datcor = %dec(%date() : *ISO)
102200000830     C*
102300000613     C                   ENDSR
102400000613     C***
