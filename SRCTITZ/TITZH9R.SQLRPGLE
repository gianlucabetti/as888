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
002400160205     D depspe          s              8    INZ(*blanks)
002500160205     D curspe          s              8    INZ(*blanks)
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
004100160205     D  VINSPE                        8
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
012800160205     C+ vindta, vinflg, substr(vindta, 8, 8) as sped, rrn(tivin00r)
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
015600160205     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100160205     C                   exsr      repNSP
016200071003     C                   exsr      impvab
016300071003     C                   exsr      wrivab
016400130723     C***                exsr      wrivat_b                                     => carico VAT
016500071003     C                   else
016600080923     C*
016700071009     C                   if        wDISK = 'D'
016800160205     C                   exsr      repNSP
016900071009     C                   exsr      impvab
017000071009     C                   exsr      wrivab
017100130723     C***                exsr      wrivat_b                                     => carico VAT
017200130306     C***                exsr      wrivat_e                                     => carico VAT
017300071009     C                   else
017400071009     C*
017500010305     C                   if        depspe = *blanks                             => 1° giro
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700080117     C                   clear                   tivinds
017800160205     C                   exsr      repNSP
017900020305     C                   exsr      impvab
018000130723     C***                exsr      wrivat_b                                     => carico VAT
018100130306     C***50              exsr      wrivat_e                                     => carico VAT
018200010305     C                   else
018300020725     C                   if        depspe <> curspe                             => rottura di spediz
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500070928     C                   exsr      wrivab
018600080117     C                   clear                   tivinds
018700160205     C                   exsr      repNSP
018800020305     C                   exsr      impvab
018900130723     C***                exsr      wrivat_b                                     => carico VAT
019000130306     C***50              exsr      wrivat_e                                     => carico VAT
019100020305     C                   else                                                   => x stessa spediz
019200090204     C***                exsr      impvab
019300130723     C***                exsr      wrivat_b                                     => carico VAT
019400130306     C***50              exsr      wrivat_e                                     => carico VAT
019500010305     C                   endif
019600010305     C                   endif
019700010305     C                   endif
019800071003     C                   endif
019900071009     C                   endif
020000000905     C*
020100000905     C                   else
020200020725     C                   eval      x_vinflg = '1'
020300000905     C                   endif
020400000905     C*
020500020725     C     VINRRN        chain     tivin000
020600020725     C                   if        %found(tivin00r)
020700020725     C                   eval      y_vinflg = x_vinflg
020800020725     C                   eval      y_vinmsg = x_vinmsg
020900020725     C                   update    tivin000
021000020725     C                   endif
021100020725     C*
021200020725     C/EXEC SQL
021300070530     C+ Fetch C1 into :INPUT_DS
021400020725     C/END-EXEC
021500020725     C*
021600020725     C                   enddo
021700020725     C*
021800020725     C/EXEC SQL
021900020725     C+ close C1
022000020725     C/END-EXEC
022100000905     C*
022200020305     C* Scarico i VAB rimasti "in sospeso"
022300071009     C                   if        wDISK = 'C'
022400071009     C                   exsr      wrivab
022500071009     C                   endif
022600010202     C*
022700010202     C                   endif
022800990910
022900990910     C* Se non ci sono record con errori ...
023000000710     C                   if        §ctrno = 0
023100990910     C* ... restituisco esito OK.
023200990921     C                   eval      wrkesito = '0'
023300990910     C                   else
023400010201     C                   if        §ctrokvb > 0
023500990921     C                   eval      wrkesito = '1'
023600000710     C                   else
023700000710     C                   eval      wrkesito = '2'
023800990910     C                   endif
023900000710     C                   endif
024000990910     C*
024100990914     C                   if        %open(tivin00r)
024200990908     C                   close     tivin00r
024300990914     C                   endif
024400021113     C                   if        %open(fivabwwr)
024500021113     C                   close     fivabwwr
024600990914     C                   endif
024700021113     C                   if        %open(fivatwwr)
024800021113     C                   close     fivatwwr
024900010201     C                   endif
025000990910     C*
025100010201     C                   if        §ctrokvb > 0
025200000724     C                             and vlrpoi <> *zeros
025300010202     C                   exsr      invio
025400990920     C                   endif
025500990920     C*
025600910830     C                   ENDSR
025700000613     C***
025800010305
025900010305     C*----------------------------------------------------*
026000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026100010305     C*----------------------------------------------------*
026200020305     C     WRIVAB        BEGSR
026300070730     C*
026400071003     C* Considerazioni finali
026500071003     C                   if        VABRMA = *blanks
026600071003     C                   movel(P)  VABRMN        VABRMA
026700071003     C                   endif
026800071003     C*
026900021113     C                   write     fivab000                                     => scarico il VAB
027000010305     C*
027100010305     C                   ENDSR
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800010201     C                   Z-ADD     *zeros        Num5_0
027900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028000020725     C                   MOVEL     '0'           FlgCAS            1
028100000801     C*
028200000801     C                   ENDSR
028300000801     C*----------------------------------------------------*
028400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028500000801     C*----------------------------------------------------*
028600020904     C     DEFCAM        BEGSR
028700080923     C*
028800080923     C                   SETOFF                                       5051
028900000801     C*
029000021113     C                   CLEAR                   FIVAB000
029100021113     C                   CLEAR                   FIVAT000
029200070730     C* Imposto i valori di default...
029300160205     C                   EVAL      VABCCM = 2495211
029400160205     C                   EVAL      VATCCM = 2495211
029500160205     C                   EVAL      VABLNP = 249
029600160205     C                   EVAL      VATLNP = 249
029700130306     C                   EVAL      VABCTR = 000
029800130723     C                   EVAL      VABCBO = '1'
029900160205     C***                EVAL      VABCTM = '7Q'
030000070222     C* ... e poi verifico se sono stati passati come parametri
030100070222     C                   IF        vlrppt > *blanks
030200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABCCM
030700070222     C                   Z-ADD     PiVal         VATCCM
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031200070222     C                   EXSR      CHKNUM
031300070222     C                   IF        PiInt=*on
031400070222     C                   Z-ADD     PiVal         VABLNP
031500070222     C                   Z-ADD     PiVal         VATLNP
031600070222     C                   ENDIF
031700070222     C                   ENDIF
031800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032000070222     C                   EXSR      CHKNUM
032100070222     C                   IF        PiInt=*on
032200070222     C                   Z-ADD     PiVal         VABCTR
032300070222     C                   ENDIF
032400070928     C                   ENDIF
032500071009     C                   MOVEL     *blanks       wDISK             1
032600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032700071009     C                   IF        wDISK <> *blanks
032800070928     C                   SETON                                        50
032900070222     C                   ENDIF
033000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033100080923     C                   SETON                                        51
033200080923     C                   ENDIF
033300130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
033400130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033500130507     C                   ENDIF
033600070222     C                   ENDIF
033700071009     C*
033800071009     C   50              EVAL      VABCTM = '7Q'
033900000801     C*
034000000801     C                   ENDSR
034100160205     C*----------------------------------------------------*
034200160205     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
034300160205     C*----------------------------------------------------*
034400160205     C     REPNSP        BEGSR
034500160205     C*
034600160205     C                   EXSR      INZVAR
034700160205     C                   EXSR      DEFCAM
034800160205     C*
034900160205     C                   SETON                                        60
035000160205     C*
035100160205     C* NSP => Stacco un numeratore da AZNUM
035200160205     C                   clear                   TRUL33DS
035300160205     C                   eval      I33OPE = *zeros
035400160205     C                   eval      I33CNU = 302
035500160205     C                   eval      I33NUM = 1
035600160205     C                   movel     TRUL33DS      KPJBU
035700160205     C                   call      'TRUL33R'
035800160205     C                   parm                    KPJBA
035900160205     C                   movel     KPJBU         TRUL33DS
036000160205     C                   if        O33ERR = *zeros
036100160205     C                   z-add     O33NRF        VABNSP
036200160205     C                   z-add     O33NRF        VATNSP
036300160205     C                   else
036400160205     C                   SETON                                        31
036500160205     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036600160205     C                             + ' ' + 'VABNSP VATNSP'
036700160205     C                   endif
036800160205     C*
036900160205     C                   ENDSR
037000000801     C*----------------------------------------------------*
037100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
037200000801     C*----------------------------------------------------*
037300010201     C     IMPVAB        BEGSR
037400070530     C*
037500070530     C                   SETOFF                                       3132
037600070928     C*
037700070928     C                   MOVE(P)   vlrpoi        VABFGS
037800070928     C                   MOVE(P)   vlrpoi        VATFGS
037900130723     C                   MOVEL     datcor        VABAAS
038000070928     C                   MOVE      datcor        VABMGS
038100160205     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
038200150508     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038300150810     C     '@':'A'       XLATE     VABRSD        VABRSD
038400070928     C* ==
038500160205     C                   EVAL      VABIND=%trim(%subst(vindta:58:30))
038600160205     C                   EVAL      VABLOD=%trim(%subst(vindta:88:30))
038700160205     C                   EVAL      VABRMA=%trim(%subst(vindta:8:8))
038800160205     C                   EVAL      VABNOT=%trim(%subst(vindta:139:30))
038900160205     C                   EVAL      VABCBO=%trim(%subst(vindta:16:1))
039000160205     C*
039100160205     C* CAP
039200160205     C                   EVAL      PiStr=%trim(%subst(vindta:118:5))
039300160205     C                   EXSR      CHKNUM
039400160205     C                   IF        PiInt=*on
039500160205     C                   Z-ADD     PiVal         Num5_0
039600160205     C                   MOVEL(P)  Num5_0        VABCAD
039700160205     C                   ELSE
039800160205     C                   SETON                                        32
039900160205     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040000160205     C                             + ' ' + 'VABCAD'
040100160205     C                   ENDIF
040200080923     C* RMN
040300160205     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
040400121001     C                   EXSR      CHKNUM
040500121001     C                   IF        PiInt=*on
040600121001     C                   Z-ADD     PiVal         VABRMN
040700121001     C                   ELSE
040800121001     C                   SETON                                        32
040900121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000121001     C                             + ' ' + 'VABRMN'
041100121001     C                   ENDIF
041200071003     C* NCL
041300160205     C                   EVAL      PiStr=%trim(%subst(vindta:125:5))
041400070928     C                   EXSR      CHKNUM
041500070928     C                   IF        PiInt=*on
041600070928     C                   Z-ADD     PiVal         VABNCL
041700070928     C                   ELSE
041800070928     C                   SETON                                        32
041900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042000070928     C                             + ' ' + 'VABNCL'
042100070928     C                   ENDIF
042200071003     C* PKB
042300160205     C                   EVAL      PiStr=%trim(%subst(vindta:130:9))
042400070928     C                   EXSR      CHKNUM
042500070928     C                   IF        PiNum=*on
042600160205     C* il dato ha 1 decimali senza separatore
042700160205     C                   EVAL(H)   VABPKB = PiVal/10
042800070928     C                   ELSE
042900070928     C                   SETON                                        32
043000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100070928     C                             + ' ' + 'VABPKB'
043200070928     C                   ENDIF
043300160205     C* VLB
043400160205     C                   EVAL      PiStr=%trim(%subst(vindta:170:5))
043500160205     C                   EXSR      CHKNUM
043600160205     C                   IF        PiNum=*on
043700160205     C* il dato ha 2 decimali senza separatore
043800160205     C                   EVAL(H)   VABVLB = PiVal/100
043900160205     C                   ELSE
044000160205     C                   SETON                                        32
044100160205     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200160205     C                             + ' ' + 'VABVLB'
044300160205     C                   ENDIF
044400150508     C* CAS
044500160205     C                   IF        %trim(%subst(vindta:19:9)) <> *blanks AND
044600160205     C                             %trim(%subst(vindta:19:9)) <> *zeros
044700150508     C                   MOVEL     '1'           FlgCAS
044800150508     C                   EVAL      VABVCA = 'EUR'
044900160205     C                   EVAL      VABTIC = *blank
045000160205     C                   EVAL      PiStr=%trim(%subst(vindta:19:9))
045100150508     C                   EXSR      CHKNUM
045200150508     C                   IF        PiNum=*on
045300160205     C                   EVAL      PiVal = PiVal
045400150508     C                   Z-ADD     PiVal         VABCAS
045500150508     C                   ELSE
045600150508     C                   SETON                                        32
045700150508     C                   Z-ADD     *zeros        VABCAS
045800150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045900150508     C                             + ' ' + 'VABCAS'
046000150508     C                   ENDIF
046100150508     C                   ENDIF
046200070730     C*
046300121109     C* Considerazioni finali su CBO/CAS
046400121106     C*
046500121106      * se c/assegno impostato
046600121106     C                   IF        VABCAS > 0
046700121109     C                   IF        VABCBO = '1'
046800121109     C                   EVAL      VABCBO = '4'
046900121109     C                   ENDIF
047000121109     C                   IF        VABCBO = '2'
047100121109     C                   EVAL      VABCBO = '6'
047200121109     C                   ENDIF
047300121106     C                   ENDIF
047400130306     C*
047500130306     C* Se provincia nn valorizzata la reperisco
047600130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
047700130306     C                   IF        VABPRD =  *blanks AND
047800130306     C                             VABNZD =  *blanks AND
047900130306     C                             VABCAD <> *blanks AND
048000130306     C                             VABLOD <> *blanks
048100130306     C                   CLEAR                   TISI95DS
048200130306     C                   EVAL      I95TCN = '3'
048300130306     C                   Z-ADD     datcor        I95DAT
048400130306     C                   EVAL      I95NAR = VABNZD
048500130306     C                   EVAL      I95CAP = VABCAD
048600130306     C                   EVAL      I95LOC = VABLOD
048700130306     C                   CALL      'TISI95R'
048800130306     C                   PARM                    TISI95DS
048900130306     C                   EVAL      VABPRD = O95PRV
049000130306     C                   ENDIF
049100020305     C*
049200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049300011113     C                   EXSR      CHKIMPDIV
049400010202     C*
049500000801     C* Ebbene...
049600000801     C                   ADD       1             §CTRMO
049700070530     C                   IF        *in31 <> *zeros OR
049800070530     C                             *in32 <> *zeros
049900000801     C                   ADD       1             §CTRNO
050000020725     C                   EVAL      x_vinflg = '2'
050100000801     C                   ELSE
050200010201     C                   ADD       1             §CTROKVB
050300000801     C                   ENDIF
050400000801     C*
050500000801     C                   ENDSR
050600010201     C*----------------------------------------------------*
050700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
050800010201     C*----------------------------------------------------*
050900071003     C     WRIVAT_B      BEGSR
051000090204     C*
051100090204     C                   eval      vatAAS = vabAAS
051200090204     C                   eval      vatLNP = vabLNP
051300090204     C                   eval      vatNRS = vabNRS
051400090204     C                   eval      vatNSP = vabNSP
051500010201     C*
051600101007     C* Valorizzo il buffer di scrittura del FIVAT
051700070928     C* - TIPO RECORD "B"
051800130306     C                   if        %subst(vindta:260:13) <> *blanks
051900070928     C                   eval      VATTRC = 'B'
052000130306     C                   eval      VATNOT = %trim(%subst(vindta:260:13))
052100021113     C                   write     FIVAT000
052200020725     C                   endif
052300010201     C*
052400010201     C                   ENDSR
052500071003     C*----------------------------------------------------*
052600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
052700071003     C*----------------------------------------------------*
052800071003     C     WRIVAT_E      BEGSR
052900090204     C*
053000090204     C                   eval      vatAAS = vabAAS
053100090204     C                   eval      vatLNP = vabLNP
053200090204     C                   eval      vatNRS = vabNRS
053300090204     C                   eval      vatNSP = vabNSP
053400071003     C*
053500101007     C* Valorizzo il buffer di scrittura del FIVAT
053600071003     C* - TIPO RECORD "E"
053700130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
053800130306     C                   eval      VATTRC = 'E'
053900121109     C*
054000130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
054100071003     C                   write     FIVAT000
054200121109     C*
054300130306     C***                endif
054400071003     C*
054500071003     C                   ENDSR
054600020904
054700020904
054800010202     C*----------------------------------------------------*
054900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055000010202     C*----------------------------------------------------*
055100020305     C     PREVAT        BEGSR
055200010202     C*
055300021113     C* Compongo il nome del membro da dare al FIVATWWR
055400010202     C                   eval      parmbr = vlrhdl
055500010202     C                   movel     'M'           parmbr
055600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
055700010202     C                   eval      paropz = '1'
055800010202     C* Effettuo la chiamata al CLLE preposto
055900050128     C                   call(e)   'TITVVTC'
056000010202     C                   parm                    parccm
056100010202     C                   parm                    parmbr
056200010202     C                   parm                    paropz
056300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056400010202     C                   if        %error
056500010202     C                   movel     '1'           chkcall
056600010202     C                   else
056700010202     C                   movel     '0'           chkcall
056800010202     C                   endif
056900010202     C*
057000010202     C                   ENDSR
057100000801     C*----------------------------------------------------*
057200000801     C*  CONTROLLO NUMERICITA' CAMPI
057300000801     C*----------------------------------------------------*
057400000801     C     CHKNUM        BEGSR
057500081113     C*
057600081113     C                   IF        PiDecChr = *blanks
057700121109     C                   EVAL      PiDecChr = ','
057800081113     C                   ENDIF
057900081113     C*
058000081113     C                   callp(e)  UBISNUM_Check(PiStr
058100081113     C                                          :PiDecChr
058200081113     C                                          :PiVal
058300081113     C                                          :PiNum
058400081113     C                                          :PiInt)
058500081113     C*
058600000801     C                   IF        %error
058700000801     C                   EVAL      PiInt=*off
058800000801     C                   ENDIF
058900000801     C*
059000000801     C                   ENDSR
059100000801     C***
059200000801
059300011113
059400011113     C*----------------------------------------------------*
059500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059600011113     C*----------------------------------------------------*
059700011113     C     CHKIMPDIV     BEGSR
059800011113     C*
059900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060000011113     C                   Z-ADD     *zeros        wrkDec            9 9
060100011113     C*
060200011113     C* Come prima cosa effettuo considerazioni sulla divisa
060300011113     C                   IF        vabIAS > *zeros
060400011113     C                   IF        vabVAS <> 'EUR'
060500011113     C                   EVAL      vabVAS =  'ITL'
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C                   IF        vabCAS > *zeros
061000011113     C                   IF        vabVCA <> 'EUR'
061100011113     C                   EVAL      vabVCA =  'ITL'
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   IF        vabVMD > *zeros
061600020305     C                   IF        vabVAD <> 'EUR'
061700011113     C                   EVAL      vabVAD =  'ITL'
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062200011113     C                   Z-ADD     vabIAS        wrkDec
062300011113     C                   IF        wrkDec > *zeros
062400011113     C                   IF        vabVAS = 'ITL'
062500011113     C                   EVAL      vabIAS = *zeros
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063000011113     C                   Z-ADD     vabCAS        wrkDec
063100011113     C                   IF        wrkDec > *zeros
063200011113     C                   IF        vabVCA = 'ITL'
063300011113     C                   EVAL      vabCAS = *zeros
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063800011113     C                   Z-ADD     vabVMD        wrkDec
063900011113     C                   IF        wrkDec > *zeros
064000011113     C                   IF        vabVAD = 'ITL'
064100011113     C                   EVAL      vabVMD = *zeros
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   ENDSR
064600011113     C***
064700011113
064800011113
064900000801
065000000801
065100990920      /TITLE Invio dei dati al punto operativo.
065200010202     C     invio         BEGSR
065300990920     C*
065400021113     C* 1° invio FIVAT
065500010201     C                   reset                   dscmz
065600010201     C                   move      vlrpoi        cmzdst
065700021113     C                   eval      cmzfld = 'FIVATWWR'
065800010201     C                   eval      cmzmbd = vlrhdl
065900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000021009     C***                if        prmfir = *blanks
066100021113     C                   eval      cmzfla = 'FIVAT00F'
066200021113     C                   eval      cmzmba = 'FIVAT00F'
066300021009     C***                else
066400021009     C***                eval      cmzfla = prmfir
066500021009     C***                eval      cmzmba = prmfir
066600021009     C***                endif
066700010201     C                   eval      cmznrr = *zeros
066800020305     C                   move      §ctrokvt      cmznrr
066900021018     C                   eval      cmzlba = vlrfl1
067000010201     C                   call(e)   'TIS711C'
067100010201     C                   parm                    dscmz
067200010201     C                   parm      *blanks       esito
067300010205     C                   if        %error
067400010205     C                             or cmzerr = '1'
067500010205     C                             or esito  = '1'
067600010205     C                   eval      wrkesito = '3'
067700010205     C                   else
067800010201     C*
067900021113     C* 2° invio FIVAB
068000010201     C                   reset                   dscmz
068100010201     C                   move      vlrpoi        cmzdst
068200010201     C                   eval      cmzfld = vlrfou
068300010201     C                   eval      cmzmbd = vlrhdl
068400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068500021009     C***                if        prmfir = *blanks
068600021113     C                   eval      cmzfla = 'FIVAB00F'
068700021113     C                   eval      cmzmba = 'FIVAB00F'
068800021009     C***                else
068900021009     C***                eval      cmzfla = prmfir
069000021009     C***                eval      cmzmba = prmfir
069100021009     C***                endif
069200010201     C                   eval      cmznrr = *zeros
069300010201     C                   move      §ctrokvb      cmznrr
069400021018     C                   eval      cmzlba = vlrfl1
069500010201     C                   call(e)   'TIS711C'
069600010201     C                   parm                    dscmz
069700010201     C                   parm      *blanks       esito
069800010201     C                   if        %error
069900010201     C                             or cmzerr = '1'
070000010201     C                             or esito  = '1'
070100010201     C                   eval      wrkesito = '3'
070200010201     C                   endif
070300010205     C                   endif
070400990920     C*
070500000613     C                   ENDSR
070600000613     C***
070700070411
070800070411     C     *pssr         BEGSR
070900070411     C*
071000070411     C                   if        %open(tivin00r)
071100070411     C                   close     tivin00r
071200070411     C                   endif
071300070411     C                   if        %open(fivabwwr)
071400070411     C                   close     fivabwwr
071500070411     C                   endif
071600070411     C                   if        %open(fivatwwr)
071700070411     C                   close     fivatwwr
071800070411     C                   endif
071900070411     C*
072000070411     C* Effettuo la chiamata al CLLE preposto
072100070411     C                   call(e)   'TITVVTC'
072200070411     C                   parm                    parccm
072300070411     C                   parm                    parmbr
072400070411     C                   parm      '2'           paropz
072500070411     C*
072600070411     C                   eval      wrkesito = '2'
072700070411     C*
072800070411     C                   seton                                        LR
072900070411     C*
073000070411     C                   ENDSR     '*CANCL'
073100070411     C***
073200070411
073300990910
073400000613     C     *inzsr        BEGSR
073500990910     C*
073600990910     C     *entry        plist
073700990920     C                   parm                    tivlrds
073800990921     C                   parm      wrkesito      esito
073900000724     C                   parm                    prmlit
074000000710     C                   parm                    prmfir
074100000613     C*
074200000830     C* CALCOLA LA DATA CORRENTE
074300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074400100324     C                   eval      datcor = %dec(%date() : *ISO)
074500000830     C*
074600000613     C                   ENDSR
074700000613     C***
