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
002400150514     D depspe          s              5    INZ(*blanks)
002500150514     D curspe          s              5    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200150514     D Num7_0          s              7  0
003300121011     D w70             s             70
003400000830
003500020725
003600020725     D*------------------
003700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003800020725     D*------------------
003900070530     D INPUT_DS        DS                  INZ
004000101007     D  VINDTA                     2048
004100101007     D  VINFLG                        1
004200150514     D  VINSPE                        5
004300101007     D  VINRRN                        8  0
004400020725     D*
004500080923     D*------------------
004600080923     D* DS REPERIMENTO NUMERATORE
004700080923     D*------------------
004800080923     D trul33ds      e ds                  inz
004900080923     D*------------------
005000080923     D* DS ARCHITETTURA
005100080923     D*------------------
005200080923     D kpjba         e ds                  inz
005300080923     D*------------------
005400121126     D BarcodeDS       DS           770    INZ
005500121126     D  wBarcode                     14    dim(55)
005600121109     D*------------------
005700121109     D TRUL28DSX     e ds                  inz
005800121109     D*------------------
005900121109     D wX              s              3s 0
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
008400050128     C                   call(e)   'TITVVTC'
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
009700000801
009800910830     C*--------------------------------------------------------
009900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010000910830     C*--------------------------------------------------------
010100070530     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600021113     C                   if        not %open(fivabwwr)
010700021113     C                   open      fivabwwr
010800990914     C                   endif
010900070530     C*
011000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011100020305     C                   exsr      prevat
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500021113     C                   if        not %open(fivatwwr)
011600021113     C                   open      fivatwwr
011700010201     C                   endif
011800080117     C*
011900080117     C                   EXSR      INZVAR
012000080117     C                   EXSR      DEFCAM
012100990910     C*
012200010201     C                   clear                   §CTROKVB          5 0
012300020305     C                   clear                   §CTROKVT          5 0
012400000801     C                   clear                   §CTRMO            5 0
012500000801     C                   clear                   §CTRNO            5 0
012600990910     C*
012700020725     C/EXEC SQL
012800020725     C+ declare C1 cursor for select
012900150514     C+ vindta, vinflg, substr(vindta, 175, 5) as sped, rrn(tivin00r)
013000060223     C+ from tivin00r
013100060223     C+ order by sped
013200020725     C+ for read only
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600020725     C+ open C1
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000070530     C+ Fetch C1 into :INPUT_DS
014100020725     C/END-EXEC
014200020725     C*
014300020725     C                   dow       sqlcod = *zeros
014400990913     C*
014500020725     C                   if        vindta > *blanks
014600000613     C                   add       1             rrnum
014700000801     C*
014800020725     C                   if        vinflg = *blanks
014900020725     C                             or vinflg = '0'
015000020725     C                             or vinflg = '2'
015100000801     C*
015200020725     C                   clear                   x_vinmsg
015300020725     C                   eval      x_vinflg = '1'
015400010305     C*
015500121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015600121109     C* (uso l'RMN)
015700150514     C                   EVAL      PiStr=%trim(%subst(vindta:175:5))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200071003     C                   exsr      impvab
016300071003     C                   exsr      wrivab
016400150514     C                   exsr      wrivat_ij                                    => carico VAT
016500071003     C                   else
016600080923     C*
016700071009     C                   if        wDISK = 'D'
016800071009     C                   exsr      impvab
016900071009     C                   exsr      wrivab
017000150514     C                   exsr      wrivat_ij                                    => carico VAT
017100130306     C***                exsr      wrivat_e                                     => carico VAT
017200071009     C                   else
017300071009     C*
017400010305     C                   if        depspe = *blanks                             => 1° giro
017500010305     C                   eval      depspe = curspe                              => memorizz. spediz
017600080117     C                   clear                   tivinds
017700020305     C                   exsr      impvab
017800150514     C                   exsr      wrivat_ij                                    => carico VAT
017900130306     C***50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300070928     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500020305     C                   exsr      impvab
018600150514     C                   exsr      wrivat_ij                                    => carico VAT
018700130306     C***50              exsr      wrivat_e                                     => carico VAT
018800020305     C                   else                                                   => x stessa spediz
018900090204     C***                exsr      impvab
019000130723     C***                exsr      wrivat_b                                     => carico VAT
019100130306     C***50              exsr      wrivat_e                                     => carico VAT
019200010305     C                   endif
019300010305     C                   endif
019400010305     C                   endif
019500071003     C                   endif
019600071009     C                   endif
019700000905     C*
019800000905     C                   else
019900020725     C                   eval      x_vinflg = '1'
020000000905     C                   endif
020100000905     C*
020200020725     C     VINRRN        chain     tivin000
020300020725     C                   if        %found(tivin00r)
020400020725     C                   eval      y_vinflg = x_vinflg
020500020725     C                   eval      y_vinmsg = x_vinmsg
020600020725     C                   update    tivin000
020700020725     C                   endif
020800020725     C*
020900020725     C/EXEC SQL
021000070530     C+ Fetch C1 into :INPUT_DS
021100020725     C/END-EXEC
021200020725     C*
021300020725     C                   enddo
021400020725     C*
021500020725     C/EXEC SQL
021600020725     C+ close C1
021700020725     C/END-EXEC
021800000905     C*
021900020305     C* Scarico i VAB rimasti "in sospeso"
022000071009     C                   if        wDISK = 'C'
022100071009     C                   exsr      wrivab
022200071009     C                   endif
022300010202     C*
022400010202     C                   endif
022500990910
022600990910     C* Se non ci sono record con errori ...
022700000710     C                   if        §ctrno = 0
022800990910     C* ... restituisco esito OK.
022900990921     C                   eval      wrkesito = '0'
023000990910     C                   else
023100010201     C                   if        §ctrokvb > 0
023200990921     C                   eval      wrkesito = '1'
023300000710     C                   else
023400000710     C                   eval      wrkesito = '2'
023500990910     C                   endif
023600000710     C                   endif
023700990910     C*
023800990914     C                   if        %open(tivin00r)
023900990908     C                   close     tivin00r
024000990914     C                   endif
024100021113     C                   if        %open(fivabwwr)
024200021113     C                   close     fivabwwr
024300990914     C                   endif
024400021113     C                   if        %open(fivatwwr)
024500021113     C                   close     fivatwwr
024600010201     C                   endif
024700990910     C*
024800010201     C                   if        §ctrokvb > 0
024900000724     C                             and vlrpoi <> *zeros
025000010202     C                   exsr      invio
025100990920     C                   endif
025200990920     C*
025300910830     C                   ENDSR
025400000613     C***
025500010305
025600010305     C*----------------------------------------------------*
025700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025800010305     C*----------------------------------------------------*
025900020305     C     WRIVAB        BEGSR
026000070730     C*
026100071003     C* Considerazioni finali
026200071003     C                   if        VABRMA = *blanks
026300071003     C                   movel(P)  VABRMN        VABRMA
026400071003     C                   endif
026500071003     C*
026600021113     C                   write     fivab000                                     => scarico il VAB
026700010305     C*
026800010305     C                   ENDSR
026900990920
027000000801     C*----------------------------------------------------*
027100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027200000801     C*----------------------------------------------------*
027300010201     C     INZVAR        BEGSR
027400000801     C*
027500010201     C                   Z-ADD     *zeros        Num5_0
027600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027700020725     C                   MOVEL     '0'           FlgCAS            1
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028200000801     C*----------------------------------------------------*
028300020904     C     DEFCAM        BEGSR
028400080923     C*
028500080923     C                   SETOFF                                       5051
028600000801     C*
028700021113     C                   CLEAR                   FIVAB000
028800021113     C                   CLEAR                   FIVAT000
028900070730     C* Imposto i valori di default...
029000150514     C                   EVAL      VABCCM = 0230002
029100150514     C                   EVAL      VABLNP = 023
029200150514     C                   EVAL      VABCTR = 000
029300070222     C* ... e poi verifico se sono stati passati come parametri
029400070222     C                   IF        vlrppt > *blanks
029500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029700070222     C                   EXSR      CHKNUM
029800070222     C                   IF        PiInt=*on
029900070222     C                   Z-ADD     PiVal         VABCCM
030000070222     C                   Z-ADD     PiVal         VATCCM
030100070222     C                   ENDIF
030200070222     C                   ENDIF
030300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030500070222     C                   EXSR      CHKNUM
030600070222     C                   IF        PiInt=*on
030700070222     C                   Z-ADD     PiVal         VABLNP
030800070222     C                   Z-ADD     PiVal         VATLNP
030900070222     C                   ENDIF
031000070222     C                   ENDIF
031100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031300070222     C                   EXSR      CHKNUM
031400070222     C                   IF        PiInt=*on
031500070222     C                   Z-ADD     PiVal         VABCTR
031600070222     C                   ENDIF
031700070928     C                   ENDIF
031800071009     C                   MOVEL     *blanks       wDISK             1
031900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032000071009     C                   IF        wDISK <> *blanks
032100070928     C                   SETON                                        50
032200070222     C                   ENDIF
032300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032400080923     C                   SETON                                        51
032500080923     C                   ENDIF
032600130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
032700130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
032800130507     C                   ENDIF
032900070222     C                   ENDIF
033000071009     C*
033100071009     C   50              EVAL      VABCTM = '7Q'
033200000801     C*
033300000801     C                   ENDSR
033400000801     C*----------------------------------------------------*
033500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600000801     C*----------------------------------------------------*
033700010201     C     IMPVAB        BEGSR
033800070530     C*
033900070530     C                   SETOFF                                       3132
034000070928     C*
034100070928     C                   EXSR      INZVAR
034200070928     C                   EXSR      DEFCAM
034300070928     C*
034400070928     C                   MOVE(P)   vlrpoi        VABFGS
034500070928     C                   MOVE(P)   vlrpoi        VATFGS
034600130723     C                   MOVEL     datcor        VABAAS
034700070928     C                   MOVE      datcor        VABMGS
034800150514     C                   EVAL      VABRSD=%trim(%subst(vindta:1:40))
034900150514     C     '@':'A'       XLATE     VABRSD        VABRSD
035000150514     C* ==
035100150514     C                   EVAL      VABRD2=%trim(%subst(vindta:42:40))
035200150514     C                   EVAL      VABIND=%trim(%subst(vindta:83:40))
035300150514     C                   EVAL      VABLOD=%trim(%subst(vindta:130:38))
035400150514     C                   EVAL      VABPRD=%trim(%subst(vindta:169:2))
035500150514     C                   EVAL      VABRMA=%trim(%subst(vindta:175:5))
035600150514     C                   EVAL      VABCBO=%trim(%subst(vindta:219:1))
035700150514     C                   EVAL      VABNOT=%trim(%subst(vindta:221:35))
035800150514     C                   EVAL      VABNT2=%trim(%subst(vindta:(221+35):35))
035900150514     C                   EVAL      VABTC1=%trim(%subst(vindta:292:1))
036000150514     C* NSP/RMN
036100150514     C                   EVAL      PiStr=%trim(%subst(vindta:175:5))
036200150514     C                   EXSR      CHKNUM
036300150514     C                   IF        PiInt=*on
036400150514     C                   Z-ADD     PiVal         Num7_0
036500150514     C                   MOVE(P)   Num7_0        VABNSP
036600150514     C                   Z-ADD     PiVal         VABRMN
036700150514     C                   ELSE
036800150514     C                   SETON                                        31
036900150514     C                   Z-ADD     *zeros        VABNSP
037000150514     C                   Z-ADD     *zeros        VABRMN
037100150514     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037200150514     C                             + ' ' + 'VABNSP VABRMN'
037300150514     C                   ENDIF
037400150514     C* CAD
037500150514     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
037600150514     C                   EXSR      CHKNUM
037700150514     C                   IF        PiInt=*on
037800150514     C                   Z-ADD     PiVal         Num5_0
037900150514     C                   MOVEL(p)  Num5_0        VABCAD
038000150514     C                   ELSE
038100150514     C                   SETON                                        32
038200150514     C                   EVAL      VABCAD = *zeros
038300150514     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038400150514     C                             + ' ' + 'VABCAD'
038500150514     C                   ENDIF
038600150514     C* NCL
038700150514     C                   EVAL      PiStr=%trim(%subst(vindta:192:5))
038800150514     C                   EXSR      CHKNUM
038900150514     C                   IF        PiInt=*on
039000150514     C                   Z-ADD     PiVal         VABNCL
039100150514     C                   ELSE
039200150514     C                   SETON                                        32
039300150514     C                   Z-ADD     *zeros        VABNCL
039400150514     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039500150514     C                             + ' ' + 'VABNCL'
039600150514     C                   ENDIF
039700150514     C* PKB
039800150514     C                   EVAL      PiStr=%trim(%subst(vindta:198:7))
039900150514     C                   EXSR      CHKNUM
040000150514     C                   IF        PiNum=*on
040100150514     C                   Z-ADD     PiVal         VABPKB
040200150514     C                   ELSE
040300150514     C                   SETON                                        32
040400150514     C                   Z-ADD     *zeros        VABPKB
040500150514     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040600150514     C                             + ' ' + 'VABPKB'
040700150514     C                   ENDIF
040800150514     C* CAS
040900150514     C                   EVAL      PiStr=%trim(%subst(vindta:206:12))
041000150514     C                   EXSR      CHKNUM
041100150514     C                   IF        PiNum=*on
041200150514     C                   Z-ADD     PiVal         VABCAS
041300150514     C                   IF        VABCAS > *zeros
041400150514     C                   EVAL      VABTIC = 'BM'
041500150514     C                   EVAL      VABVCA = 'EUR'
041600150514     C                   ENDIF
041700150514     C                   ELSE
041800150514     C                   SETON                                        32
041900150514     C                   Z-ADD     *zeros        VABCAS
042000150514     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042100150514     C                             + ' ' + 'VABCAS'
042200150514     C                   ENDIF
042300070730     C*
042400121109     C* Considerazioni finali su CBO/CAS
042500121106     C*
042600121106      * se c/assegno impostato
042700121106     C                   IF        VABCAS > 0
042800121109     C                   IF        VABCBO = '1'
042900121109     C                   EVAL      VABCBO = '4'
043000121109     C                   ENDIF
043100121109     C                   IF        VABCBO = '2'
043200121109     C                   EVAL      VABCBO = '6'
043300121109     C                   ENDIF
043400121106     C                   ENDIF
043500130306     C*
043600130306     C* Se provincia nn valorizzata la reperisco
043700130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
043800130306     C                   IF        VABPRD =  *blanks AND
043900130306     C                             VABNZD =  *blanks AND
044000130306     C                             VABCAD <> *blanks AND
044100130306     C                             VABLOD <> *blanks
044200130306     C                   CLEAR                   TISI95DS
044300130306     C                   EVAL      I95TCN = '3'
044400130306     C                   Z-ADD     datcor        I95DAT
044500130306     C                   EVAL      I95NAR = VABNZD
044600130306     C                   EVAL      I95CAP = VABCAD
044700130306     C                   EVAL      I95LOC = VABLOD
044800130306     C                   CALL      'TISI95R'
044900130306     C                   PARM                    TISI95DS
045000130306     C                   EVAL      VABPRD = O95PRV
045100130306     C                   ENDIF
045200020305     C*
045300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045400011113     C                   EXSR      CHKIMPDIV
045500010202     C*
045600000801     C* Ebbene...
045700000801     C                   ADD       1             §CTRMO
045800070530     C                   IF        *in31 <> *zeros OR
045900070530     C                             *in32 <> *zeros
046000000801     C                   ADD       1             §CTRNO
046100020725     C                   EVAL      x_vinflg = '2'
046200000801     C                   ELSE
046300010201     C                   ADD       1             §CTROKVB
046400000801     C                   ENDIF
046500000801     C*
046600000801     C                   ENDSR
046700010201     C*----------------------------------------------------*
046800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
046900010201     C*----------------------------------------------------*
047000150514     C     WRIVAT_IJ     BEGSR
047100090204     C*
047200090204     C                   eval      vatAAS = vabAAS
047300090204     C                   eval      vatLNP = vabLNP
047400090204     C                   eval      vatNRS = vabNRS
047500090204     C                   eval      vatNSP = vabNSP
047600010201     C*
047700150514     C* Valorizzo il buffer di scrittura del FIVAT
047800150514     C* - TIPO RECORD "I"
047900150514     C                   EVAL      w70=%trim(%subst(vindta:333:60))
048000150514     C                   if        %subst(w70:1:35) <> *blanks
048100150514     C                   eval      VATTRC = 'I'
048200150514     C                   eval      VATNOT = %trim(%subst(w70:1:35))
048300150514     C                   write     FIVAT000
048400150514     C                   endif
048500150514     C*
048600150514     C* Valorizzo il buffer di scrittura del FIVAT
048700150514     C* - TIPO RECORD "J"
048800150514     C                   if        %subst(w70:36:35) <> *blanks
048900150514     C                   eval      VATTRC = 'J'
049000150514     C                   eval      VATNOT = %trim(%subst(w70:36:35))
049100150514     C                   write     FIVAT000
049200150514     C                   endif
049300010201     C*
049400010201     C                   ENDSR
049500071003     C*----------------------------------------------------*
049600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
049700071003     C*----------------------------------------------------*
049800071003     C     WRIVAT_E      BEGSR
049900090204     C*
050000090204     C                   eval      vatAAS = vabAAS
050100090204     C                   eval      vatLNP = vabLNP
050200090204     C                   eval      vatNRS = vabNRS
050300090204     C                   eval      vatNSP = vabNSP
050400071003     C*
050500101007     C* Valorizzo il buffer di scrittura del FIVAT
050600071003     C* - TIPO RECORD "E"
050700130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
050800130306     C                   eval      VATTRC = 'E'
050900121109     C*
051000130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
051100071003     C                   write     FIVAT000
051200121109     C*
051300130306     C***                endif
051400071003     C*
051500071003     C                   ENDSR
051600020904
051700020904
051800010202     C*----------------------------------------------------*
051900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052000010202     C*----------------------------------------------------*
052100020305     C     PREVAT        BEGSR
052200010202     C*
052300021113     C* Compongo il nome del membro da dare al FIVATWWR
052400010202     C                   eval      parmbr = vlrhdl
052500010202     C                   movel     'M'           parmbr
052600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
052700010202     C                   eval      paropz = '1'
052800010202     C* Effettuo la chiamata al CLLE preposto
052900050128     C                   call(e)   'TITVVTC'
053000010202     C                   parm                    parccm
053100010202     C                   parm                    parmbr
053200010202     C                   parm                    paropz
053300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053400010202     C                   if        %error
053500010202     C                   movel     '1'           chkcall
053600010202     C                   else
053700010202     C                   movel     '0'           chkcall
053800010202     C                   endif
053900010202     C*
054000010202     C                   ENDSR
054100000801     C*----------------------------------------------------*
054200000801     C*  CONTROLLO NUMERICITA' CAMPI
054300000801     C*----------------------------------------------------*
054400000801     C     CHKNUM        BEGSR
054500081113     C*
054600081113     C                   IF        PiDecChr = *blanks
054700121109     C                   EVAL      PiDecChr = ','
054800081113     C                   ENDIF
054900081113     C*
055000081113     C                   callp(e)  UBISNUM_Check(PiStr
055100081113     C                                          :PiDecChr
055200081113     C                                          :PiVal
055300081113     C                                          :PiNum
055400081113     C                                          :PiInt)
055500081113     C*
055600000801     C                   IF        %error
055700000801     C                   EVAL      PiInt=*off
055800000801     C                   ENDIF
055900000801     C*
056000000801     C                   ENDSR
056100000801     C***
056200000801
056300011113
056400011113     C*----------------------------------------------------*
056500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056600011113     C*----------------------------------------------------*
056700011113     C     CHKIMPDIV     BEGSR
056800011113     C*
056900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057000011113     C                   Z-ADD     *zeros        wrkDec            9 9
057100011113     C*
057200011113     C* Come prima cosa effettuo considerazioni sulla divisa
057300011113     C                   IF        vabIAS > *zeros
057400011113     C                   IF        vabVAS <> 'EUR'
057500011113     C                   EVAL      vabVAS =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C                   IF        vabCAS > *zeros
058000011113     C                   IF        vabVCA <> 'EUR'
058100011113     C                   EVAL      vabVCA =  'ITL'
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C                   IF        vabVMD > *zeros
058600020305     C                   IF        vabVAD <> 'EUR'
058700011113     C                   EVAL      vabVAD =  'ITL'
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059200011113     C                   Z-ADD     vabIAS        wrkDec
059300011113     C                   IF        wrkDec > *zeros
059400011113     C                   IF        vabVAS = 'ITL'
059500011113     C                   EVAL      vabIAS = *zeros
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060000011113     C                   Z-ADD     vabCAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVCA = 'ITL'
060300011113     C                   EVAL      vabCAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060800011113     C                   Z-ADD     vabVMD        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVAD = 'ITL'
061100011113     C                   EVAL      vabVMD = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   ENDSR
061600011113     C***
061700011113
061800011113
061900000801
062000000801
062100990920      /TITLE Invio dei dati al punto operativo.
062200010202     C     invio         BEGSR
062300990920     C*
062400021113     C* 1° invio FIVAT
062500010201     C                   reset                   dscmz
062600010201     C                   move      vlrpoi        cmzdst
062700021113     C                   eval      cmzfld = 'FIVATWWR'
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021009     C***                if        prmfir = *blanks
063100021113     C                   eval      cmzfla = 'FIVAT00F'
063200021113     C                   eval      cmzmba = 'FIVAT00F'
063300021009     C***                else
063400021009     C***                eval      cmzfla = prmfir
063500021009     C***                eval      cmzmba = prmfir
063600021009     C***                endif
063700010201     C                   eval      cmznrr = *zeros
063800020305     C                   move      §ctrokvt      cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000010201     C                   call(e)   'TIS711C'
064100010201     C                   parm                    dscmz
064200010201     C                   parm      *blanks       esito
064300010205     C                   if        %error
064400010205     C                             or cmzerr = '1'
064500010205     C                             or esito  = '1'
064600010205     C                   eval      wrkesito = '3'
064700010205     C                   else
064800010201     C*
064900021113     C* 2° invio FIVAB
065000010201     C                   reset                   dscmz
065100010201     C                   move      vlrpoi        cmzdst
065200010201     C                   eval      cmzfld = vlrfou
065300010201     C                   eval      cmzmbd = vlrhdl
065400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065500021009     C***                if        prmfir = *blanks
065600021113     C                   eval      cmzfla = 'FIVAB00F'
065700021113     C                   eval      cmzmba = 'FIVAB00F'
065800021009     C***                else
065900021009     C***                eval      cmzfla = prmfir
066000021009     C***                eval      cmzmba = prmfir
066100021009     C***                endif
066200010201     C                   eval      cmznrr = *zeros
066300010201     C                   move      §ctrokvb      cmznrr
066400021018     C                   eval      cmzlba = vlrfl1
066500010201     C                   call(e)   'TIS711C'
066600010201     C                   parm                    dscmz
066700010201     C                   parm      *blanks       esito
066800010201     C                   if        %error
066900010201     C                             or cmzerr = '1'
067000010201     C                             or esito  = '1'
067100010201     C                   eval      wrkesito = '3'
067200010201     C                   endif
067300010205     C                   endif
067400990920     C*
067500000613     C                   ENDSR
067600000613     C***
067700070411
067800070411     C     *pssr         BEGSR
067900070411     C*
068000070411     C                   if        %open(tivin00r)
068100070411     C                   close     tivin00r
068200070411     C                   endif
068300070411     C                   if        %open(fivabwwr)
068400070411     C                   close     fivabwwr
068500070411     C                   endif
068600070411     C                   if        %open(fivatwwr)
068700070411     C                   close     fivatwwr
068800070411     C                   endif
068900070411     C*
069000070411     C* Effettuo la chiamata al CLLE preposto
069100070411     C                   call(e)   'TITVVTC'
069200070411     C                   parm                    parccm
069300070411     C                   parm                    parmbr
069400070411     C                   parm      '2'           paropz
069500070411     C*
069600070411     C                   eval      wrkesito = '2'
069700070411     C*
069800070411     C                   seton                                        LR
069900070411     C*
070000070411     C                   ENDSR     '*CANCL'
070100070411     C***
070200070411
070300990910
070400000613     C     *inzsr        BEGSR
070500990910     C*
070600990910     C     *entry        plist
070700990920     C                   parm                    tivlrds
070800990921     C                   parm      wrkesito      esito
070900000724     C                   parm                    prmlit
071000000710     C                   parm                    prmfir
071100000613     C*
071200000830     C* CALCOLA LA DATA CORRENTE
071300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071400100324     C                   eval      datcor = %dec(%date() : *ISO)
071500000830     C*
071600000613     C                   ENDSR
071700000613     C***
