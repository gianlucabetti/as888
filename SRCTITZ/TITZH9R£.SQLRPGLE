000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130306      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800021113     FFIVABwwr  O    E             DISK    usropn
000900021113     FFIVATwwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070730     D tisi95ds      e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400150508     D depspe          s             15    INZ(*blanks)
002500150508     D curspe          s             15    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200121011     D w70             s             70
003300000830
003400020725
003500020725     D*------------------
003600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003700020725     D*------------------
003800070530     D INPUT_DS        DS                  INZ
003900101007     D  VINDTA                     2048
004000101007     D  VINFLG                        1
004100150508     D  VINSPE                       15
004200101007     D  VINRRN                        8  0
004300020725     D*
004400080923     D*------------------
004500080923     D* DS REPERIMENTO NUMERATORE
004600080923     D*------------------
004700080923     D trul33ds      e ds                  inz
004800080923     D*------------------
004900080923     D* DS ARCHITETTURA
005000080923     D*------------------
005100080923     D kpjba         e ds                  inz
005200080923     D*------------------
005300121126     D BarcodeDS       DS           770    INZ
005400121126     D  wBarcode                     14    dim(55)
005500121109     D*------------------
005600121109     D TRUL28DSX     e ds                  inz
005700121109     D*------------------
005800121109     D wX              s              3s 0
005900081113
006000081113     D*------------------
006100081113     D* LINKING A DEFINIZIONI ESTERNE
006200081113     D*------------------
006300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006500081113
006600990908
006700010201
006800010201
006900080117     C*
007000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007100080117     C
007200080117     C/EXEC SQL
007300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007400080117     C/END-EXEC
007500080117     C
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900000613     C*
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300050128     C                   call(e)   'TITVVTC'
008400010202     C                   parm                    parccm
008500010202     C                   parm                    parmbr
008600010202     C                   parm      '2'           paropz
008700070730     C*
008800070730     C* Effettuo lancio TISI95 solo x chiusura
008900070730     C                   CLEAR                   TISI95DS
009000070730     C                   EVAL      I95TLA = 'C'
009100070730     C                   CALL      'TISI95R'
009200070730     C                   PARM                    TISI95DS
009300000616     C*
009400000801     C
009500010201     C                   seton                                        LR
009600000801
009700910830     C*--------------------------------------------------------
009800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009900910830     C*--------------------------------------------------------
010000070530     C     RWFILE        BEGSR
010100990910     C*
010200990914     C                   if        not %open(tivin00r)
010300990908     C                   open      tivin00r
010400990914     C                   endif
010500021113     C                   if        not %open(fivabwwr)
010600021113     C                   open      fivabwwr
010700990914     C                   endif
010800070530     C*
010900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011000020305     C                   exsr      prevat
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400021113     C                   if        not %open(fivatwwr)
011500021113     C                   open      fivatwwr
011600010201     C                   endif
011700080117     C*
011800080117     C                   EXSR      INZVAR
011900080117     C                   EXSR      DEFCAM
012000990910     C*
012100010201     C                   clear                   §CTROKVB          5 0
012200020305     C                   clear                   §CTROKVT          5 0
012300000801     C                   clear                   §CTRMO            5 0
012400000801     C                   clear                   §CTRNO            5 0
012500990910     C*
012600020725     C/EXEC SQL
012700020725     C+ declare C1 cursor for select
012800150810     C+ vindta, vinflg, substr(vindta, 80, 15) as sped, rrn(tivin00r)
012900060223     C+ from tivin00r
013000060223     C+ order by sped
013100020725     C+ for read only
013200020725     C/END-EXEC
013300020725     C
013400020725     C/EXEC SQL
013500020725     C+ open C1
013600020725     C/END-EXEC
013700020725     C
013800020725     C/EXEC SQL
013900070530     C+ Fetch C1 into :INPUT_DS
014000020725     C/END-EXEC
014100020725     C*
014200020725     C                   dow       sqlcod = *zeros
014300990913     C*
014400020725     C                   if        vindta > *blanks
014500000613     C                   add       1             rrnum
014600000801     C*
014700020725     C                   if        vinflg = *blanks
014800020725     C                             or vinflg = '0'
014900020725     C                             or vinflg = '2'
015000000801     C*
015100020725     C                   clear                   x_vinmsg
015200020725     C                   eval      x_vinflg = '1'
015300010305     C*
015400121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015500121109     C* (uso l'RMN)
015600150810     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100071003     C                   exsr      impvab
016200071003     C                   exsr      wrivab
016300130723     C***                exsr      wrivat_b                                     => carico VAT
016400071003     C                   else
016500080923     C*
016600071009     C                   if        wDISK = 'D'
016700071009     C                   exsr      impvab
016800071009     C                   exsr      wrivab
016900130723     C***                exsr      wrivat_b                                     => carico VAT
017000130306     C***                exsr      wrivat_e                                     => carico VAT
017100071009     C                   else
017200071009     C*
017300010305     C                   if        depspe = *blanks                             => 1° giro
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500080117     C                   clear                   tivinds
017600020305     C                   exsr      impvab
017700130723     C***                exsr      wrivat_b                                     => carico VAT
017800130306     C***50              exsr      wrivat_e                                     => carico VAT
017900010305     C                   else
018000020725     C                   if        depspe <> curspe                             => rottura di spediz
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200070928     C                   exsr      wrivab
018300080117     C                   clear                   tivinds
018400020305     C                   exsr      impvab
018500130723     C***                exsr      wrivat_b                                     => carico VAT
018600130306     C***50              exsr      wrivat_e                                     => carico VAT
018700020305     C                   else                                                   => x stessa spediz
018800090204     C***                exsr      impvab
018900130723     C***                exsr      wrivat_b                                     => carico VAT
019000130306     C***50              exsr      wrivat_e                                     => carico VAT
019100010305     C                   endif
019200010305     C                   endif
019300010305     C                   endif
019400071003     C                   endif
019500071009     C                   endif
019600000905     C*
019700000905     C                   else
019800020725     C                   eval      x_vinflg = '1'
019900000905     C                   endif
020000000905     C*
020100020725     C     VINRRN        chain     tivin000
020200020725     C                   if        %found(tivin00r)
020300020725     C                   eval      y_vinflg = x_vinflg
020400020725     C                   eval      y_vinmsg = x_vinmsg
020500020725     C                   update    tivin000
020600020725     C                   endif
020700020725     C*
020800020725     C/EXEC SQL
020900070530     C+ Fetch C1 into :INPUT_DS
021000020725     C/END-EXEC
021100020725     C*
021200020725     C                   enddo
021300020725     C*
021400020725     C/EXEC SQL
021500020725     C+ close C1
021600020725     C/END-EXEC
021700000905     C*
021800020305     C* Scarico i VAB rimasti "in sospeso"
021900071009     C                   if        wDISK = 'C'
022000071009     C                   exsr      wrivab
022100071009     C                   endif
022200010202     C*
022300010202     C                   endif
022400990910
022500990910     C* Se non ci sono record con errori ...
022600000710     C                   if        §ctrno = 0
022700990910     C* ... restituisco esito OK.
022800990921     C                   eval      wrkesito = '0'
022900990910     C                   else
023000010201     C                   if        §ctrokvb > 0
023100990921     C                   eval      wrkesito = '1'
023200000710     C                   else
023300000710     C                   eval      wrkesito = '2'
023400990910     C                   endif
023500000710     C                   endif
023600990910     C*
023700990914     C                   if        %open(tivin00r)
023800990908     C                   close     tivin00r
023900990914     C                   endif
024000021113     C                   if        %open(fivabwwr)
024100021113     C                   close     fivabwwr
024200990914     C                   endif
024300021113     C                   if        %open(fivatwwr)
024400021113     C                   close     fivatwwr
024500010201     C                   endif
024600990910     C*
024700010201     C                   if        §ctrokvb > 0
024800000724     C                             and vlrpoi <> *zeros
024900010202     C                   exsr      invio
025000990920     C                   endif
025100990920     C*
025200910830     C                   ENDSR
025300000613     C***
025400010305
025500010305     C*----------------------------------------------------*
025600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025700010305     C*----------------------------------------------------*
025800020305     C     WRIVAB        BEGSR
025900070730     C*
026000071003     C* Considerazioni finali
026100071003     C                   if        VABRMA = *blanks
026200071003     C                   movel(P)  VABRMN        VABRMA
026300071003     C                   endif
026400071003     C*
026500021113     C                   write     fivab000                                     => scarico il VAB
026600010305     C*
026700010305     C                   ENDSR
026800990920
026900000801     C*----------------------------------------------------*
027000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100000801     C*----------------------------------------------------*
027200010201     C     INZVAR        BEGSR
027300000801     C*
027400010201     C                   Z-ADD     *zeros        Num5_0
027500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027600020725     C                   MOVEL     '0'           FlgCAS            1
027700000801     C*
027800000801     C                   ENDSR
027900000801     C*----------------------------------------------------*
028000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028100000801     C*----------------------------------------------------*
028200020904     C     DEFCAM        BEGSR
028300080923     C*
028400080923     C                   SETOFF                                       5051
028500000801     C*
028600021113     C                   CLEAR                   FIVAB000
028700021113     C                   CLEAR                   FIVAT000
028800070730     C* Imposto i valori di default...
028900150810     C                   EVAL      VABCCM = 0017652
029000150810     C                   EVAL      VATCCM = 0017652
029100150810     C                   EVAL      VABLNP = 275
029200150810     C                   EVAL      VATLNP = 275
029300130306     C                   EVAL      VABCTR = 000
029400130723     C                   EVAL      VABCBO = '1'
029500150508     C                   EVAL      VABCTM = '7Q'
029600070222     C* ... e poi verifico se sono stati passati come parametri
029700070222     C                   IF        vlrppt > *blanks
029800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030000070222     C                   EXSR      CHKNUM
030100070222     C                   IF        PiInt=*on
030200070222     C                   Z-ADD     PiVal         VABCCM
030300070222     C                   Z-ADD     PiVal         VATCCM
030400070222     C                   ENDIF
030500070222     C                   ENDIF
030600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030800070222     C                   EXSR      CHKNUM
030900070222     C                   IF        PiInt=*on
031000070222     C                   Z-ADD     PiVal         VABLNP
031100070222     C                   Z-ADD     PiVal         VATLNP
031200070222     C                   ENDIF
031300070222     C                   ENDIF
031400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031600070222     C                   EXSR      CHKNUM
031700070222     C                   IF        PiInt=*on
031800070222     C                   Z-ADD     PiVal         VABCTR
031900070222     C                   ENDIF
032000070928     C                   ENDIF
032100071009     C                   MOVEL     *blanks       wDISK             1
032200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032300071009     C                   IF        wDISK <> *blanks
032400070928     C                   SETON                                        50
032500070222     C                   ENDIF
032600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032700080923     C                   SETON                                        51
032800080923     C                   ENDIF
032900130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
033000130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033100130507     C                   ENDIF
033200070222     C                   ENDIF
033300071009     C*
033400071009     C   50              EVAL      VABCTM = '7Q'
033500000801     C*
033600000801     C                   ENDSR
033700000801     C*----------------------------------------------------*
033800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033900000801     C*----------------------------------------------------*
034000010201     C     IMPVAB        BEGSR
034100070530     C*
034200070530     C                   SETOFF                                       3132
034300070928     C*
034400070928     C                   EXSR      INZVAR
034500070928     C                   EXSR      DEFCAM
034600070928     C*
034700070928     C                   MOVE(P)   vlrpoi        VABFGS
034800070928     C                   MOVE(P)   vlrpoi        VATFGS
034900130723     C                   MOVEL     datcor        VABAAS
035000070928     C                   MOVE      datcor        VABMGS
035100150810     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
035200150508     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035300150810     C     '@':'A'       XLATE     VABRSD        VABRSD
035400070928     C* ==
035500150810     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
035600150810     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
035700150810     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
035800150810     C                   EVAL      VABCAD=%trim(%subst(vindta:316:7))
035900150810     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
036000150810     C                   EVAL      VABNZD=%trim(%subst(vindta:325:4))
036100150810     C                   IF        %trim(VABNZD) = 'I' or
036200150810     C                             %trim(VABNZD) = 'IT' or
036300150810     C                             %trim(VABNZD) = 'ITA'
036400150508     C                   EVAL      VABNZD = *blanks
036500150508     C                   ENDIF
036600150810     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
036700150810     C                   EVAL      w70=%trim(%subst(vindta:464:70))
036800150810     C                   EVAL      VABNOT=%subst(w70:1:35)
036900150810     C                   EVAL      VABNT2=%subst(w70:36:35)
037000150508     C*
037100150810     C* DCR
037200150810     C                   EVAL      PiStr=%trim(%subst(vindta:46:8))
037300150810     C                   EXSR      CHKNUM
037400150810     C                   IF        PiInt=*on
037500150810     C                   Z-ADD     PiVal         VABDCR
037600150810     C                   ELSE
037700150810     C                   SETON                                        32
037800150810     C                   Z-ADD     *zeros        VABDCR
037900150810     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038000150810     C                             + ' ' + 'VABDCR'
038100150810     C                   ENDIF
038200080923     C* RMN
038300150810     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
038400121001     C                   EXSR      CHKNUM
038500121001     C                   IF        PiInt=*on
038600121001     C                   Z-ADD     PiVal         VABRMN
038700121001     C                   ELSE
038800121001     C                   SETON                                        32
038900121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039000121001     C                             + ' ' + 'VABRMN'
039100121001     C                   ENDIF
039200130723     C* NSP
039300150810     C                   EVAL      PiStr=%trim(%subst(vindta:85:7))
039400150810     C                   EXSR      CHKNUM
039500150810     C                   IF        PiInt=*on
039600150810     C                   Z-ADD     PiVal         VABNSP
039700150810     C                   ELSE
039800150810     C                   SETON                                        32
039900150810     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040000150810     C                             + ' ' + 'VABNSP'
040100150810     C                   ENDIF
040200071003     C* NCL
040300150810     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
040400070928     C                   EXSR      CHKNUM
040500070928     C                   IF        PiInt=*on
040600070928     C                   Z-ADD     PiVal         VABNCL
040700070928     C                   ELSE
040800070928     C                   SETON                                        32
040900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000070928     C                             + ' ' + 'VABNCL'
041100070928     C                   ENDIF
041200071003     C* PKB
041300150810     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
041400070928     C                   EXSR      CHKNUM
041500070928     C                   IF        PiNum=*on
041600150810     C* il dato ha 2 decimali senza separatore
041700150810     C                   EVAL(H)   VABPKB = PiVal/100
041800070928     C                   ELSE
041900070928     C                   SETON                                        32
042000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042100070928     C                             + ' ' + 'VABPKB'
042200070928     C                   ENDIF
042300150508     C* CAS
042400150810     C                   IF        %trim(%subst(vindta:396:15)) <> *blanks AND
042500150810     C                             %trim(%subst(vindta:396:15)) <> *zeros
042600150508     C                   MOVEL     '1'           FlgCAS
042700150508     C                   EVAL      VABVCA = 'EUR'
042800150810     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
042900150508     C                   EXSR      CHKNUM
043000150508     C                   IF        PiNum=*on
043100150508     C                   EVAL      PiVal = PiVal / 1000                         * gest. 3 decimali
043200150508     C                   Z-ADD     PiVal         VABCAS
043300150508     C                   ELSE
043400150508     C                   SETON                                        32
043500150508     C                   Z-ADD     *zeros        VABCAS
043600150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043700150508     C                             + ' ' + 'VABCAS'
043800150508     C                   ENDIF
043900150508     C                   ENDIF
044000070730     C*
044100121109     C* Considerazioni finali su CBO/CAS
044200121106     C*
044300121106      * se c/assegno impostato
044400121106     C                   IF        VABCAS > 0
044500121109     C                   IF        VABCBO = '1'
044600121109     C                   EVAL      VABCBO = '4'
044700121109     C                   ENDIF
044800121109     C                   IF        VABCBO = '2'
044900121109     C                   EVAL      VABCBO = '6'
045000121109     C                   ENDIF
045100121106     C                   ENDIF
045200130306     C*
045300130306     C* Se provincia nn valorizzata la reperisco
045400130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
045500130306     C                   IF        VABPRD =  *blanks AND
045600130306     C                             VABNZD =  *blanks AND
045700130306     C                             VABCAD <> *blanks AND
045800130306     C                             VABLOD <> *blanks
045900130306     C                   CLEAR                   TISI95DS
046000130306     C                   EVAL      I95TCN = '3'
046100130306     C                   Z-ADD     datcor        I95DAT
046200130306     C                   EVAL      I95NAR = VABNZD
046300130306     C                   EVAL      I95CAP = VABCAD
046400130306     C                   EVAL      I95LOC = VABLOD
046500130306     C                   CALL      'TISI95R'
046600130306     C                   PARM                    TISI95DS
046700130306     C                   EVAL      VABPRD = O95PRV
046800130306     C                   ENDIF
046900020305     C*
047000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047100011113     C                   EXSR      CHKIMPDIV
047200010202     C*
047300000801     C* Ebbene...
047400000801     C                   ADD       1             §CTRMO
047500070530     C                   IF        *in31 <> *zeros OR
047600070530     C                             *in32 <> *zeros
047700000801     C                   ADD       1             §CTRNO
047800020725     C                   EVAL      x_vinflg = '2'
047900000801     C                   ELSE
048000010201     C                   ADD       1             §CTROKVB
048100000801     C                   ENDIF
048200000801     C*
048300000801     C                   ENDSR
048400010201     C*----------------------------------------------------*
048500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
048600010201     C*----------------------------------------------------*
048700071003     C     WRIVAT_B      BEGSR
048800090204     C*
048900090204     C                   eval      vatAAS = vabAAS
049000090204     C                   eval      vatLNP = vabLNP
049100090204     C                   eval      vatNRS = vabNRS
049200090204     C                   eval      vatNSP = vabNSP
049300010201     C*
049400101007     C* Valorizzo il buffer di scrittura del FIVAT
049500070928     C* - TIPO RECORD "B"
049600130306     C                   if        %subst(vindta:260:13) <> *blanks
049700070928     C                   eval      VATTRC = 'B'
049800130306     C                   eval      VATNOT = %trim(%subst(vindta:260:13))
049900021113     C                   write     FIVAT000
050000020725     C                   endif
050100010201     C*
050200010201     C                   ENDSR
050300071003     C*----------------------------------------------------*
050400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
050500071003     C*----------------------------------------------------*
050600071003     C     WRIVAT_E      BEGSR
050700090204     C*
050800090204     C                   eval      vatAAS = vabAAS
050900090204     C                   eval      vatLNP = vabLNP
051000090204     C                   eval      vatNRS = vabNRS
051100090204     C                   eval      vatNSP = vabNSP
051200071003     C*
051300101007     C* Valorizzo il buffer di scrittura del FIVAT
051400071003     C* - TIPO RECORD "E"
051500130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
051600130306     C                   eval      VATTRC = 'E'
051700121109     C*
051800130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
051900071003     C                   write     FIVAT000
052000121109     C*
052100130306     C***                endif
052200071003     C*
052300071003     C                   ENDSR
052400020904
052500020904
052600010202     C*----------------------------------------------------*
052700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052800010202     C*----------------------------------------------------*
052900020305     C     PREVAT        BEGSR
053000010202     C*
053100021113     C* Compongo il nome del membro da dare al FIVATWWR
053200010202     C                   eval      parmbr = vlrhdl
053300010202     C                   movel     'M'           parmbr
053400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
053500010202     C                   eval      paropz = '1'
053600010202     C* Effettuo la chiamata al CLLE preposto
053700050128     C                   call(e)   'TITVVTC'
053800010202     C                   parm                    parccm
053900010202     C                   parm                    parmbr
054000010202     C                   parm                    paropz
054100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054200010202     C                   if        %error
054300010202     C                   movel     '1'           chkcall
054400010202     C                   else
054500010202     C                   movel     '0'           chkcall
054600010202     C                   endif
054700010202     C*
054800010202     C                   ENDSR
054900000801     C*----------------------------------------------------*
055000000801     C*  CONTROLLO NUMERICITA' CAMPI
055100000801     C*----------------------------------------------------*
055200000801     C     CHKNUM        BEGSR
055300081113     C*
055400081113     C                   IF        PiDecChr = *blanks
055500121109     C                   EVAL      PiDecChr = ','
055600081113     C                   ENDIF
055700081113     C*
055800081113     C                   callp(e)  UBISNUM_Check(PiStr
055900081113     C                                          :PiDecChr
056000081113     C                                          :PiVal
056100081113     C                                          :PiNum
056200081113     C                                          :PiInt)
056300081113     C*
056400000801     C                   IF        %error
056500000801     C                   EVAL      PiInt=*off
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900000801     C***
057000000801
057100011113
057200011113     C*----------------------------------------------------*
057300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057400011113     C*----------------------------------------------------*
057500011113     C     CHKIMPDIV     BEGSR
057600011113     C*
057700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057800011113     C                   Z-ADD     *zeros        wrkDec            9 9
057900011113     C*
058000011113     C* Come prima cosa effettuo considerazioni sulla divisa
058100011113     C                   IF        vabIAS > *zeros
058200011113     C                   IF        vabVAS <> 'EUR'
058300011113     C                   EVAL      vabVAS =  'ITL'
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C                   IF        vabCAS > *zeros
058800011113     C                   IF        vabVCA <> 'EUR'
058900011113     C                   EVAL      vabVCA =  'ITL'
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   IF        vabVMD > *zeros
059400020305     C                   IF        vabVAD <> 'EUR'
059500011113     C                   EVAL      vabVAD =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060000011113     C                   Z-ADD     vabIAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVAS = 'ITL'
060300011113     C                   EVAL      vabIAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060800011113     C                   Z-ADD     vabCAS        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVCA = 'ITL'
061100011113     C                   EVAL      vabCAS = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061600011113     C                   Z-ADD     vabVMD        wrkDec
061700011113     C                   IF        wrkDec > *zeros
061800011113     C                   IF        vabVAD = 'ITL'
061900011113     C                   EVAL      vabVMD = *zeros
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C                   ENDSR
062400011113     C***
062500011113
062600011113
062700000801
062800000801
062900990920      /TITLE Invio dei dati al punto operativo.
063000010202     C     invio         BEGSR
063100990920     C*
063200021113     C* 1° invio FIVAT
063300010201     C                   reset                   dscmz
063400010201     C                   move      vlrpoi        cmzdst
063500021113     C                   eval      cmzfld = 'FIVATWWR'
063600010201     C                   eval      cmzmbd = vlrhdl
063700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063800021009     C***                if        prmfir = *blanks
063900021113     C                   eval      cmzfla = 'FIVAT00F'
064000021113     C                   eval      cmzmba = 'FIVAT00F'
064100021009     C***                else
064200021009     C***                eval      cmzfla = prmfir
064300021009     C***                eval      cmzmba = prmfir
064400021009     C***                endif
064500010201     C                   eval      cmznrr = *zeros
064600020305     C                   move      §ctrokvt      cmznrr
064700021018     C                   eval      cmzlba = vlrfl1
064800010201     C                   call(e)   'TIS711C'
064900010201     C                   parm                    dscmz
065000010201     C                   parm      *blanks       esito
065100010205     C                   if        %error
065200010205     C                             or cmzerr = '1'
065300010205     C                             or esito  = '1'
065400010205     C                   eval      wrkesito = '3'
065500010205     C                   else
065600010201     C*
065700021113     C* 2° invio FIVAB
065800010201     C                   reset                   dscmz
065900010201     C                   move      vlrpoi        cmzdst
066000010201     C                   eval      cmzfld = vlrfou
066100010201     C                   eval      cmzmbd = vlrhdl
066200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066300021009     C***                if        prmfir = *blanks
066400021113     C                   eval      cmzfla = 'FIVAB00F'
066500021113     C                   eval      cmzmba = 'FIVAB00F'
066600021009     C***                else
066700021009     C***                eval      cmzfla = prmfir
066800021009     C***                eval      cmzmba = prmfir
066900021009     C***                endif
067000010201     C                   eval      cmznrr = *zeros
067100010201     C                   move      §ctrokvb      cmznrr
067200021018     C                   eval      cmzlba = vlrfl1
067300010201     C                   call(e)   'TIS711C'
067400010201     C                   parm                    dscmz
067500010201     C                   parm      *blanks       esito
067600010201     C                   if        %error
067700010201     C                             or cmzerr = '1'
067800010201     C                             or esito  = '1'
067900010201     C                   eval      wrkesito = '3'
068000010201     C                   endif
068100010205     C                   endif
068200990920     C*
068300000613     C                   ENDSR
068400000613     C***
068500070411
068600070411     C     *pssr         BEGSR
068700070411     C*
068800070411     C                   if        %open(tivin00r)
068900070411     C                   close     tivin00r
069000070411     C                   endif
069100070411     C                   if        %open(fivabwwr)
069200070411     C                   close     fivabwwr
069300070411     C                   endif
069400070411     C                   if        %open(fivatwwr)
069500070411     C                   close     fivatwwr
069600070411     C                   endif
069700070411     C*
069800070411     C* Effettuo la chiamata al CLLE preposto
069900070411     C                   call(e)   'TITVVTC'
070000070411     C                   parm                    parccm
070100070411     C                   parm                    parmbr
070200070411     C                   parm      '2'           paropz
070300070411     C*
070400070411     C                   eval      wrkesito = '2'
070500070411     C*
070600070411     C                   seton                                        LR
070700070411     C*
070800070411     C                   ENDSR     '*CANCL'
070900070411     C***
071000070411
071100990910
071200000613     C     *inzsr        BEGSR
071300990910     C*
071400990910     C     *entry        plist
071500990920     C                   parm                    tivlrds
071600990921     C                   parm      wrkesito      esito
071700000724     C                   parm                    prmlit
071800000710     C                   parm                    prmfir
071900000613     C*
072000000830     C* CALCOLA LA DATA CORRENTE
072100100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072200100324     C                   eval      datcor = %dec(%date() : *ISO)
072300000830     C*
072400000613     C                   ENDSR
072500000613     C***
