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
002400130306     D depspe          s              8    INZ(*blanks)
002500130306     D curspe          s              8    INZ(*blanks)
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
004100130306     D  VINSPE                        8
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
012800130306     C+ vindta, vinflg, substr(vindta, 134, 8) as sped, rrn(tivin00r)
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
015600130723     C                   EVAL      PiStr=%trim(%subst(vindta:001:15))
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
028900130723     C                   EVAL      VABCCM = 1660151
029000130723     C                   EVAL      VATCCM = 1660151
029100130723     C                   EVAL      VABLNP = 166
029200130723     C                   EVAL      VATLNP = 166
029300130306     C                   EVAL      VABCTR = 000
029400130723     C                   EVAL      VABCBO = '1'
029500070222     C* ... e poi verifico se sono stati passati come parametri
029600070222     C                   IF        vlrppt > *blanks
029700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029900070222     C                   EXSR      CHKNUM
030000070222     C                   IF        PiInt=*on
030100070222     C                   Z-ADD     PiVal         VABCCM
030200070222     C                   Z-ADD     PiVal         VATCCM
030300070222     C                   ENDIF
030400070222     C                   ENDIF
030500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030700070222     C                   EXSR      CHKNUM
030800070222     C                   IF        PiInt=*on
030900070222     C                   Z-ADD     PiVal         VABLNP
031000070222     C                   Z-ADD     PiVal         VATLNP
031100070222     C                   ENDIF
031200070222     C                   ENDIF
031300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031500070222     C                   EXSR      CHKNUM
031600070222     C                   IF        PiInt=*on
031700070222     C                   Z-ADD     PiVal         VABCTR
031800070222     C                   ENDIF
031900070928     C                   ENDIF
032000071009     C                   MOVEL     *blanks       wDISK             1
032100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032200071009     C                   IF        wDISK <> *blanks
032300070928     C                   SETON                                        50
032400070222     C                   ENDIF
032500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032600080923     C                   SETON                                        51
032700080923     C                   ENDIF
032800130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
032900130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033000130507     C                   ENDIF
033100070222     C                   ENDIF
033200071009     C*
033300071009     C   50              EVAL      VABCTM = '7Q'
033400000801     C*
033500000801     C                   ENDSR
033600000801     C*----------------------------------------------------*
033700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800000801     C*----------------------------------------------------*
033900010201     C     IMPVAB        BEGSR
034000070530     C*
034100070530     C                   SETOFF                                       3132
034200070928     C*
034300070928     C                   EXSR      INZVAR
034400070928     C                   EXSR      DEFCAM
034500070928     C*
034600070928     C                   MOVE(P)   vlrpoi        VABFGS
034700070928     C                   MOVE(P)   vlrpoi        VATFGS
034800130723     C                   MOVEL     datcor        VABAAS
034900070928     C                   MOVE      datcor        VABMGS
035000130723     C                   EVAL      VABRSD=%trim(%subst(vindta:26:35))
035100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035300070928     C* ==
035400130723     C                   EVAL      VABIND=%trim(%subst(vindta:61:35))
035500130723     C                   EVAL      VABLOD=%trim(%subst(vindta:96:35))
035600130723     C                   EVAL      VABPRD=%trim(%subst(vindta:136:02))
035700130723     C                   EVAL      VABRMA=%trim(%subst(vindta:001:15))
035800130723     C                   EVAL      w70=%trim(%subst(vindta:176:60))
035900130723     C                   EVAL      VABNOT=%subst(w70:1:35)
036000130723     C                   EVAL      VABNT2=%subst(w70:36:35)
036100120717     C*
036200071003     C* CAD
036300130723     C                   EVAL      PiStr=%trim(%subst(vindta:131:5))
036400121001     C                   EXSR      CHKNUM
036500121001     C                   IF        PiInt=*on
036600121001     C                   Z-ADD     PiVal         Num5_0
036700121001     C                   MOVEL     Num5_0        VABCAD
036800121001     C                   ELSE
036900121001     C                   SETON                                        32
037000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037100121001     C                             + ' ' + 'VABCAD'
037200121001     C                   ENDIF
037300080923     C* RMN
037400130723     C                   EVAL      PiStr=%trim(%subst(vindta:001:15))
037500121001     C                   EXSR      CHKNUM
037600121001     C                   IF        PiInt=*on
037700121001     C                   Z-ADD     PiVal         VABRMN
037800121001     C                   ELSE
037900121001     C                   SETON                                        32
038000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038100121001     C                             + ' ' + 'VABRMN'
038200121001     C                   ENDIF
038300130723     C* NSP
038400130723     C                   EVAL      PiStr=%trim(%subst(vindta:001:15))
038500130723     C                   EXSR      CHKNUM
038600130723     C                   IF        PiInt=*on
038700130723     C                   Z-ADD     PiVal         VABNSP
038800130723     C                   ELSE
038900130723     C                   SETON                                        32
039000130723     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039100130723     C                             + ' ' + 'VABNSP'
039200130723     C                   ENDIF
039300071003     C* NCL
039400130723     C                   EVAL      PiStr=%trim(%subst(vindta:138:5))
039500070928     C                   EXSR      CHKNUM
039600070928     C                   IF        PiInt=*on
039700070928     C                   Z-ADD     PiVal         VABNCL
039800070928     C                   ELSE
039900070928     C                   SETON                                        32
040000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100070928     C                             + ' ' + 'VABNCL'
040200070928     C                   ENDIF
040300071003     C* PKB
040400130723     C                   EVAL      PiStr=%trim(%subst(vindta:143:7))
040500070928     C                   EXSR      CHKNUM
040600070928     C                   IF        PiNum=*on
040700130306     C* il dato ha 2 decimali
040800130306     C                   EVAL(H)   VABPKB = PiVal/100
040900070928     C                   ELSE
041000070928     C                   SETON                                        32
041100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041200070928     C                             + ' ' + 'VABPKB'
041300070928     C                   ENDIF
041400070730     C*
041500121109     C* Considerazioni finali su CBO/CAS
041600121106     C*
041700121106      * se c/assegno impostato
041800121106     C                   IF        VABCAS > 0
041900121109     C                   IF        VABCBO = '1'
042000121109     C                   EVAL      VABCBO = '4'
042100121109     C                   ENDIF
042200121109     C                   IF        VABCBO = '2'
042300121109     C                   EVAL      VABCBO = '6'
042400121109     C                   ENDIF
042500121106     C                   ENDIF
042600130306     C*
042700130306     C* Se provincia nn valorizzata la reperisco
042800130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
042900130306     C                   IF        VABPRD =  *blanks AND
043000130306     C                             VABNZD =  *blanks AND
043100130306     C                             VABCAD <> *blanks AND
043200130306     C                             VABLOD <> *blanks
043300130306     C                   CLEAR                   TISI95DS
043400130306     C                   EVAL      I95TCN = '3'
043500130306     C                   Z-ADD     datcor        I95DAT
043600130306     C                   EVAL      I95NAR = VABNZD
043700130306     C                   EVAL      I95CAP = VABCAD
043800130306     C                   EVAL      I95LOC = VABLOD
043900130306     C                   CALL      'TISI95R'
044000130306     C                   PARM                    TISI95DS
044100130306     C                   EVAL      VABPRD = O95PRV
044200130306     C                   ENDIF
044300020305     C*
044400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044500011113     C                   EXSR      CHKIMPDIV
044600010202     C*
044700000801     C* Ebbene...
044800000801     C                   ADD       1             §CTRMO
044900070530     C                   IF        *in31 <> *zeros OR
045000070530     C                             *in32 <> *zeros
045100000801     C                   ADD       1             §CTRNO
045200020725     C                   EVAL      x_vinflg = '2'
045300000801     C                   ELSE
045400010201     C                   ADD       1             §CTROKVB
045500000801     C                   ENDIF
045600000801     C*
045700000801     C                   ENDSR
045800010201     C*----------------------------------------------------*
045900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
046000010201     C*----------------------------------------------------*
046100071003     C     WRIVAT_B      BEGSR
046200090204     C*
046300090204     C                   eval      vatAAS = vabAAS
046400090204     C                   eval      vatLNP = vabLNP
046500090204     C                   eval      vatNRS = vabNRS
046600090204     C                   eval      vatNSP = vabNSP
046700010201     C*
046800101007     C* Valorizzo il buffer di scrittura del FIVAT
046900070928     C* - TIPO RECORD "B"
047000130306     C                   if        %subst(vindta:260:13) <> *blanks
047100070928     C                   eval      VATTRC = 'B'
047200130306     C                   eval      VATNOT = %trim(%subst(vindta:260:13))
047300021113     C                   write     FIVAT000
047400020725     C                   endif
047500010201     C*
047600010201     C                   ENDSR
047700071003     C*----------------------------------------------------*
047800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
047900071003     C*----------------------------------------------------*
048000071003     C     WRIVAT_E      BEGSR
048100090204     C*
048200090204     C                   eval      vatAAS = vabAAS
048300090204     C                   eval      vatLNP = vabLNP
048400090204     C                   eval      vatNRS = vabNRS
048500090204     C                   eval      vatNSP = vabNSP
048600071003     C*
048700101007     C* Valorizzo il buffer di scrittura del FIVAT
048800071003     C* - TIPO RECORD "E"
048900130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
049000130306     C                   eval      VATTRC = 'E'
049100121109     C*
049200130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
049300071003     C                   write     FIVAT000
049400121109     C*
049500130306     C***                endif
049600071003     C*
049700071003     C                   ENDSR
049800020904
049900020904
050000010202     C*----------------------------------------------------*
050100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050200010202     C*----------------------------------------------------*
050300020305     C     PREVAT        BEGSR
050400010202     C*
050500021113     C* Compongo il nome del membro da dare al FIVATWWR
050600010202     C                   eval      parmbr = vlrhdl
050700010202     C                   movel     'M'           parmbr
050800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
050900010202     C                   eval      paropz = '1'
051000010202     C* Effettuo la chiamata al CLLE preposto
051100050128     C                   call(e)   'TITVVTC'
051200010202     C                   parm                    parccm
051300010202     C                   parm                    parmbr
051400010202     C                   parm                    paropz
051500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051600010202     C                   if        %error
051700010202     C                   movel     '1'           chkcall
051800010202     C                   else
051900010202     C                   movel     '0'           chkcall
052000010202     C                   endif
052100010202     C*
052200010202     C                   ENDSR
052300000801     C*----------------------------------------------------*
052400000801     C*  CONTROLLO NUMERICITA' CAMPI
052500000801     C*----------------------------------------------------*
052600000801     C     CHKNUM        BEGSR
052700081113     C*
052800081113     C                   IF        PiDecChr = *blanks
052900121109     C                   EVAL      PiDecChr = ','
053000081113     C                   ENDIF
053100081113     C*
053200081113     C                   callp(e)  UBISNUM_Check(PiStr
053300081113     C                                          :PiDecChr
053400081113     C                                          :PiVal
053500081113     C                                          :PiNum
053600081113     C                                          :PiInt)
053700081113     C*
053800000801     C                   IF        %error
053900000801     C                   EVAL      PiInt=*off
054000000801     C                   ENDIF
054100000801     C*
054200000801     C                   ENDSR
054300000801     C***
054400000801
054500011113
054600011113     C*----------------------------------------------------*
054700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054800011113     C*----------------------------------------------------*
054900011113     C     CHKIMPDIV     BEGSR
055000011113     C*
055100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055200011113     C                   Z-ADD     *zeros        wrkDec            9 9
055300011113     C*
055400011113     C* Come prima cosa effettuo considerazioni sulla divisa
055500011113     C                   IF        vabIAS > *zeros
055600011113     C                   IF        vabVAS <> 'EUR'
055700011113     C                   EVAL      vabVAS =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabCAS > *zeros
056200011113     C                   IF        vabVCA <> 'EUR'
056300011113     C                   EVAL      vabVCA =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   IF        vabVMD > *zeros
056800020305     C                   IF        vabVAD <> 'EUR'
056900011113     C                   EVAL      vabVAD =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057400011113     C                   Z-ADD     vabIAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVAS = 'ITL'
057700011113     C                   EVAL      vabIAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058200011113     C                   Z-ADD     vabCAS        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVCA = 'ITL'
058500011113     C                   EVAL      vabCAS = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059000011113     C                   Z-ADD     vabVMD        wrkDec
059100011113     C                   IF        wrkDec > *zeros
059200011113     C                   IF        vabVAD = 'ITL'
059300011113     C                   EVAL      vabVMD = *zeros
059400011113     C                   ENDIF
059500011113     C                   ENDIF
059600011113     C*
059700011113     C                   ENDSR
059800011113     C***
059900011113
060000011113
060100000801
060200000801
060300990920      /TITLE Invio dei dati al punto operativo.
060400010202     C     invio         BEGSR
060500990920     C*
060600021113     C* 1° invio FIVAT
060700010201     C                   reset                   dscmz
060800010201     C                   move      vlrpoi        cmzdst
060900021113     C                   eval      cmzfld = 'FIVATWWR'
061000010201     C                   eval      cmzmbd = vlrhdl
061100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061200021009     C***                if        prmfir = *blanks
061300021113     C                   eval      cmzfla = 'FIVAT00F'
061400021113     C                   eval      cmzmba = 'FIVAT00F'
061500021009     C***                else
061600021009     C***                eval      cmzfla = prmfir
061700021009     C***                eval      cmzmba = prmfir
061800021009     C***                endif
061900010201     C                   eval      cmznrr = *zeros
062000020305     C                   move      §ctrokvt      cmznrr
062100021018     C                   eval      cmzlba = vlrfl1
062200010201     C                   call(e)   'TIS711C'
062300010201     C                   parm                    dscmz
062400010201     C                   parm      *blanks       esito
062500010205     C                   if        %error
062600010205     C                             or cmzerr = '1'
062700010205     C                             or esito  = '1'
062800010205     C                   eval      wrkesito = '3'
062900010205     C                   else
063000010201     C*
063100021113     C* 2° invio FIVAB
063200010201     C                   reset                   dscmz
063300010201     C                   move      vlrpoi        cmzdst
063400010201     C                   eval      cmzfld = vlrfou
063500010201     C                   eval      cmzmbd = vlrhdl
063600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063700021009     C***                if        prmfir = *blanks
063800021113     C                   eval      cmzfla = 'FIVAB00F'
063900021113     C                   eval      cmzmba = 'FIVAB00F'
064000021009     C***                else
064100021009     C***                eval      cmzfla = prmfir
064200021009     C***                eval      cmzmba = prmfir
064300021009     C***                endif
064400010201     C                   eval      cmznrr = *zeros
064500010201     C                   move      §ctrokvb      cmznrr
064600021018     C                   eval      cmzlba = vlrfl1
064700010201     C                   call(e)   'TIS711C'
064800010201     C                   parm                    dscmz
064900010201     C                   parm      *blanks       esito
065000010201     C                   if        %error
065100010201     C                             or cmzerr = '1'
065200010201     C                             or esito  = '1'
065300010201     C                   eval      wrkesito = '3'
065400010201     C                   endif
065500010205     C                   endif
065600990920     C*
065700000613     C                   ENDSR
065800000613     C***
065900070411
066000070411     C     *pssr         BEGSR
066100070411     C*
066200070411     C                   if        %open(tivin00r)
066300070411     C                   close     tivin00r
066400070411     C                   endif
066500070411     C                   if        %open(fivabwwr)
066600070411     C                   close     fivabwwr
066700070411     C                   endif
066800070411     C                   if        %open(fivatwwr)
066900070411     C                   close     fivatwwr
067000070411     C                   endif
067100070411     C*
067200070411     C* Effettuo la chiamata al CLLE preposto
067300070411     C                   call(e)   'TITVVTC'
067400070411     C                   parm                    parccm
067500070411     C                   parm                    parmbr
067600070411     C                   parm      '2'           paropz
067700070411     C*
067800070411     C                   eval      wrkesito = '2'
067900070411     C*
068000070411     C                   seton                                        LR
068100070411     C*
068200070411     C                   ENDSR     '*CANCL'
068300070411     C***
068400070411
068500990910
068600000613     C     *inzsr        BEGSR
068700990910     C*
068800990910     C     *entry        plist
068900990920     C                   parm                    tivlrds
069000990921     C                   parm      wrkesito      esito
069100000724     C                   parm                    prmlit
069200000710     C                   parm                    prmfir
069300000613     C*
069400000830     C* CALCOLA LA DATA CORRENTE
069500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069600100324     C                   eval      datcor = %dec(%date() : *ISO)
069700000830     C*
069800000613     C                   ENDSR
069900000613     C***
