000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130311      * Giorni di chiusura:
000500130311      * il cliente invia 2 possibili giorni di chiusura (da 1 a 7, da lun a dom) e poi la mod.
000600130311      * di chiusura, a differenza di noi che abbiano due campi da 2 char con il primo byte il nr.
000700130311      * del giorno e nel secondo la modalità. Quindi va fatto una scomposizione del dato.
000800130311      * Inoltre noi con ' ' dichiariamo tutto il giorno, mentre il cliente scrive 'T'.
000900130522      * Tipo servizio:
001000130522      * per noi è fisso a 'C'
001100130311      *
001200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001300990908
001400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001500021113     FFIVABwwr  O    E             DISK    usropn
001600021113     FFIVATwwr  O    E             DISK    usropn
001700990908
001800000801     D*----------------------------------------------------
001900000801     D* DICHIARAZIOINE VARIABILI DI WRK
002000000801     D*----------------------------------------------------
002100990920     D dscmz         e ds                  inz
002200990910     D psds           sds
002300990910     D  procname         *PROC
002400990920     D tivlrds       e ds                  extname(tivlr00f)
002500070730     D tisi95ds      e ds
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100130311     D depspe          s             13    INZ(*blanks)
003200130311     D curspe          s             13    INZ(*blanks)
003300010202     D parccm          s              8    INZ(*blanks)
003400010202     D parmbr          s             10    INZ(*blanks)
003500010202     D paropz          s              1    INZ(*blanks)
003600010202     D chkcall         s              1    INZ(*blanks)
003700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003800070530     D Num5_0          s              5  0
003900121011     D w70             s             70
004000000830
004100020725
004200020725     D*------------------
004300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004400020725     D*------------------
004500070530     D INPUT_DS        DS                  INZ
004600101007     D  VINDTA                     2048
004700101007     D  VINFLG                        1
004800130311     D  VINSPE                       13
004900101007     D  VINRRN                        8  0
005000020725     D*
005100080923     D*------------------
005200080923     D* DS REPERIMENTO NUMERATORE
005300080923     D*------------------
005400080923     D trul33ds      e ds                  inz
005500080923     D*------------------
005600080923     D* DS ARCHITETTURA
005700080923     D*------------------
005800080923     D kpjba         e ds                  inz
005900080923     D*------------------
006000121126     D BarcodeDS       DS           770    INZ
006100121126     D  wBarcode                     14    dim(55)
006200121109     D*------------------
006300121109     D TRUL28DSX     e ds                  inz
006400121109     D*------------------
006500121109     D wX              s              3s 0
006600081113
006700081113     D*------------------
006800081113     D* LINKING A DEFINIZIONI ESTERNE
006900081113     D*------------------
007000100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007200081113
007300990908
007400010201
007500010201
007600080117     C*
007700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800080117     C
007900080117     C/EXEC SQL
008000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100080117     C/END-EXEC
008200080117     C
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000613     C*
008700070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008800000613     C*
008900010202     C* Effettuo la chiamata al CLLE preposto
009000050128     C                   call(e)   'TITVVTC'
009100010202     C                   parm                    parccm
009200010202     C                   parm                    parmbr
009300010202     C                   parm      '2'           paropz
009400070730     C*
009500070730     C* Effettuo lancio TISI95 solo x chiusura
009600070730     C                   CLEAR                   TISI95DS
009700070730     C                   EVAL      I95TLA = 'C'
009800070730     C                   CALL      'TISI95R'
009900070730     C                   PARM                    TISI95DS
010000000616     C*
010100000801     C
010200010201     C                   seton                                        LR
010300000801
010400910830     C*--------------------------------------------------------
010500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010600910830     C*--------------------------------------------------------
010700070530     C     RWFILE        BEGSR
010800990910     C*
010900990914     C                   if        not %open(tivin00r)
011000990908     C                   open      tivin00r
011100990914     C                   endif
011200021113     C                   if        not %open(fivabwwr)
011300021113     C                   open      fivabwwr
011400990914     C                   endif
011500070530     C*
011600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011700020305     C                   exsr      prevat
011800010201     C*
011900010202     C                   if        chkcall = '0'
012000010202     C*
012100021113     C                   if        not %open(fivatwwr)
012200021113     C                   open      fivatwwr
012300010201     C                   endif
012400080117     C*
012500080117     C                   EXSR      INZVAR
012600080117     C                   EXSR      DEFCAM
012700990910     C*
012800010201     C                   clear                   §CTROKVB          5 0
012900020305     C                   clear                   §CTROKVT          5 0
013000000801     C                   clear                   §CTRMO            5 0
013100000801     C                   clear                   §CTRNO            5 0
013200990910     C*
013300020725     C/EXEC SQL
013400020725     C+ declare C1 cursor for select
013500130311     C+ vindta, vinflg, substr(vindta, 13, 13) as sped, rrn(tivin00r)
013600060223     C+ from tivin00r
013700060223     C+ order by sped
013800020725     C+ for read only
013900020725     C/END-EXEC
014000020725     C
014100020725     C/EXEC SQL
014200020725     C+ open C1
014300020725     C/END-EXEC
014400020725     C
014500020725     C/EXEC SQL
014600070530     C+ Fetch C1 into :INPUT_DS
014700020725     C/END-EXEC
014800020725     C*
014900020725     C                   dow       sqlcod = *zeros
015000990913     C*
015100020725     C                   if        vindta > *blanks
015200000613     C                   add       1             rrnum
015300000801     C*
015400020725     C                   if        vinflg = *blanks
015500020725     C                             or vinflg = '0'
015600020725     C                             or vinflg = '2'
015700000801     C*
015800020725     C                   clear                   x_vinmsg
015900020725     C                   eval      x_vinflg = '1'
016000010305     C*
016100121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016200121109     C* (uso l'RMN)
016300130311     C                   EVAL      PiStr=%trim(%subst(vindta:13:13))
016400020305     C                   MOVEL(p)  PiStr         curspe
016500010305     C*
016600071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016700080923     C                   if        *in50 = *off
016800071003     C                   exsr      impvab
016900071003     C                   exsr      wrivab
017000130306     C                   exsr      wrivat_b                                     => carico VAT
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400071009     C                   exsr      impvab
017500071009     C                   exsr      wrivab
017600130306     C                   exsr      wrivat_b                                     => carico VAT
017700130306     C***                exsr      wrivat_e                                     => carico VAT
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300020305     C                   exsr      impvab
018400130306     C                   exsr      wrivat_b                                     => carico VAT
018500130306     C***50              exsr      wrivat_e                                     => carico VAT
018600010305     C                   else
018700020725     C                   if        depspe <> curspe                             => rottura di spediz
018800010305     C                   eval      depspe = curspe                              => memorizz. spediz
018900070928     C                   exsr      wrivab
019000080117     C                   clear                   tivinds
019100020305     C                   exsr      impvab
019200130306     C                   exsr      wrivat_b                                     => carico VAT
019300130306     C***50              exsr      wrivat_e                                     => carico VAT
019400020305     C                   else                                                   => x stessa spediz
019500090204     C***                exsr      impvab
019600130306     C                   exsr      wrivat_b                                     => carico VAT
019700130306     C***50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   endif
019900010305     C                   endif
020000010305     C                   endif
020100071003     C                   endif
020200071009     C                   endif
020300000905     C*
020400000905     C                   else
020500020725     C                   eval      x_vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800020725     C     VINRRN        chain     tivin000
020900020725     C                   if        %found(tivin00r)
021000020725     C                   eval      y_vinflg = x_vinflg
021100020725     C                   eval      y_vinmsg = x_vinmsg
021200020725     C                   update    tivin000
021300020725     C                   endif
021400020725     C*
021500020725     C/EXEC SQL
021600070530     C+ Fetch C1 into :INPUT_DS
021700020725     C/END-EXEC
021800020725     C*
021900020725     C                   enddo
022000020725     C*
022100020725     C/EXEC SQL
022200020725     C+ close C1
022300020725     C/END-EXEC
022400000905     C*
022500020305     C* Scarico i VAB rimasti "in sospeso"
022600071009     C                   if        wDISK = 'C'
022700071009     C                   exsr      wrivab
022800071009     C                   endif
022900010202     C*
023000010202     C                   endif
023100990910
023200990910     C* Se non ci sono record con errori ...
023300000710     C                   if        §ctrno = 0
023400990910     C* ... restituisco esito OK.
023500990921     C                   eval      wrkesito = '0'
023600990910     C                   else
023700010201     C                   if        §ctrokvb > 0
023800990921     C                   eval      wrkesito = '1'
023900000710     C                   else
024000000710     C                   eval      wrkesito = '2'
024100990910     C                   endif
024200000710     C                   endif
024300990910     C*
024400990914     C                   if        %open(tivin00r)
024500990908     C                   close     tivin00r
024600990914     C                   endif
024700021113     C                   if        %open(fivabwwr)
024800021113     C                   close     fivabwwr
024900990914     C                   endif
025000021113     C                   if        %open(fivatwwr)
025100021113     C                   close     fivatwwr
025200010201     C                   endif
025300990910     C*
025400010201     C                   if        §ctrokvb > 0
025500000724     C                             and vlrpoi <> *zeros
025600010202     C                   exsr      invio
025700990920     C                   endif
025800990920     C*
025900910830     C                   ENDSR
026000000613     C***
026100010305
026200010305     C*----------------------------------------------------*
026300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026400010305     C*----------------------------------------------------*
026500020305     C     WRIVAB        BEGSR
026600070730     C*
026700071003     C* Considerazioni finali
026800071003     C                   if        VABRMA = *blanks
026900071003     C                   movel(P)  VABRMN        VABRMA
027000071003     C                   endif
027100071003     C*
027200021113     C                   write     fivab000                                     => scarico il VAB
027300010305     C*
027400010305     C                   ENDSR
027500990920
027600000801     C*----------------------------------------------------*
027700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027800000801     C*----------------------------------------------------*
027900010201     C     INZVAR        BEGSR
028000000801     C*
028100010201     C                   Z-ADD     *zeros        Num5_0
028200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028300020725     C                   MOVEL     '0'           FlgCAS            1
028400000801     C*
028500000801     C                   ENDSR
028600000801     C*----------------------------------------------------*
028700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028800000801     C*----------------------------------------------------*
028900020904     C     DEFCAM        BEGSR
029000080923     C*
029100080923     C                   SETOFF                                       5051
029200000801     C*
029300021113     C                   CLEAR                   FIVAB000
029400021113     C                   CLEAR                   FIVAT000
029500070730     C* Imposto i valori di default...
029600130311     C                   EVAL      VABCCM = 0016186
029700130311     C                   EVAL      VATCCM = 0016186
029800130311     C                   EVAL      VABLNP = 001
029900130311     C                   EVAL      VATLNP = 001
030000130306     C                   EVAL      VABCTR = 000
030100130311     C                   EVAL      VABCBO='1'
030200130522     C                   EVAL      VABTSP='C'
030300070222     C* ... e poi verifico se sono stati passati come parametri
030400070222     C                   IF        vlrppt > *blanks
030500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700070222     C                   EXSR      CHKNUM
030800070222     C                   IF        PiInt=*on
030900070222     C                   Z-ADD     PiVal         VABCCM
031000070222     C                   Z-ADD     PiVal         VATCCM
031100070222     C                   ENDIF
031200070222     C                   ENDIF
031300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031500070222     C                   EXSR      CHKNUM
031600070222     C                   IF        PiInt=*on
031700070222     C                   Z-ADD     PiVal         VABLNP
031800070222     C                   Z-ADD     PiVal         VATLNP
031900070222     C                   ENDIF
032000070222     C                   ENDIF
032100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032300070222     C                   EXSR      CHKNUM
032400070222     C                   IF        PiInt=*on
032500070222     C                   Z-ADD     PiVal         VABCTR
032600070222     C                   ENDIF
032700070928     C                   ENDIF
032800071009     C                   MOVEL     *blanks       wDISK             1
032900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033000071009     C                   IF        wDISK <> *blanks
033100070928     C                   SETON                                        50
033200070222     C                   ENDIF
033300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033400080923     C                   SETON                                        51
033500080923     C                   ENDIF
033600070222     C                   ENDIF
033700071009     C*
033800071009     C   50              EVAL      VABCTM = '7Q'
033900000801     C*
034000000801     C                   ENDSR
034100000801     C*----------------------------------------------------*
034200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034300000801     C*----------------------------------------------------*
034400010201     C     IMPVAB        BEGSR
034500070530     C*
034600070530     C                   SETOFF                                       3132
034700070928     C*
034800070928     C                   EXSR      INZVAR
034900070928     C                   EXSR      DEFCAM
035000070928     C*
035100070928     C                   MOVE(P)   vlrpoi        VABFGS
035200070928     C                   MOVE(P)   vlrpoi        VATFGS
035300130311     C                   MOVEL     datcor        VABAAS
035400130311     C                   MOVE      datcor        VABMGS
035500130311     C                   EVAL      VABRSD=%trim(%subst(vindta:110:30))
035600070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035700070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035800070928     C* ==
035900130311     C                   EVAL      VABIND=%trim(%subst(vindta:140:35))
036000130311     C                   EVAL      VABLOD=%trim(%subst(vindta:205:35))
036100130311     C                   EVAL      VABPRD=%trim(%subst(vindta:265:2))
036200130311     C                   EVAL      VABRMA=%trim(%subst(vindta:67:15))
036300130311     C                   EVAL      w70=%trim(%subst(vindta:300:46))
036400130306     C                   EVAL      VABNOT=%subst(w70:1:35)
036500130306     C                   EVAL      VABNT2=%subst(w70:36:35)
036600130311     C                   EVAL      %subst(VABGC1:1:1)=%subst(vindta:296:1)
036700130311     C                   IF        %subst(vindta:298:1) <> 'T'
036800130311     C                   EVAL      %subst(VABGC1:2:1)=%subst(vindta:298:1)
036900130311     C                   ELSE
037000130311     C                   EVAL      %subst(VABGC1:2:1)=*blank
037100130311     C                   ENDIF
037200130311     C                   EVAL      %subst(VABGC2:1:1)=%subst(vindta:296:1)
037300130311     C                   IF        %subst(vindta:298:1) <> 'T'
037400130311     C                   EVAL      %subst(VABGC2:2:1)=%subst(vindta:298:1)
037500130311     C                   ELSE
037600130311     C                   EVAL      %subst(VABGC2:2:1)=*blank
037700130311     C                   ENDIF
037800130311     C* se i 2 gg di chiusura sono gli stessi, elimino il secondo
037900130311     C                   IF        VABGC2 = VABGC1
038000130311     C                   EVAL      VABGC2 = *blank
038100130311     C                   ENDIF
038200120717     C*
038300071003     C* CAD
038400130311     C                   EVAL      PiStr=%trim(%subst(vindta:402:5))
038500121001     C                   EXSR      CHKNUM
038600121001     C                   IF        PiInt=*on
038700121001     C                   Z-ADD     PiVal         Num5_0
038800121001     C                   MOVEL     Num5_0        VABCAD
038900121001     C                   ELSE
039000121001     C                   SETON                                        32
039100121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039200121001     C                             + ' ' + 'VABCAD'
039300121001     C                   ENDIF
039400080923     C* RMN
039500130311     C                   EVAL      PiStr=%trim(%subst(vindta:13:13))
039600121001     C                   EXSR      CHKNUM
039700121001     C                   IF        PiInt=*on
039800121001     C                   Z-ADD     PiVal         VABRMN
039900121001     C                   ELSE
040000121001     C                   SETON                                        32
040100121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040200121001     C                             + ' ' + 'VABRMN'
040300121001     C                   ENDIF
040400080923     C* NSP
040500080923     C                   IF        *in51 = *on
040600080923     C* NSP => Stacco un numeratore da AZNUM
040700080923     C                   clear                   TRUL33DS
040800080923     C                   eval      I33OPE = *zeros
040900080923     C                   eval      I33CNU = 302
041000080923     C                   eval      I33NUM = 1
041100080923     C                   movel     TRUL33DS      KPJBU
041200080923     C                   call      'TRUL33R'
041300080923     C                   parm                    KPJBA
041400080923     C                   movel     KPJBU         TRUL33DS
041500080923     C                   if        O33ERR = *zeros
041600080923     C                   z-add     O33NRF        VABNSP
041700080923     C                   z-add     O33NRF        VATNSP
041800080923     C                   else
041900080923     C                   SETON                                        31
042000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042100080923     C                             + ' ' + 'VABNSP VATNSP'
042200080923     C                   endif
042300080923     C                   ELSE
042400130306     C                   EVAL      PiStr=%trim(%subst(vindta:134:8))
042500130306     C                   EXSR      CHKNUM
042600130306     C                   IF        PiInt=*on
042700130306     C                   Z-ADD     PiVal         VABNSP
042800130306     C                   Z-ADD     PiVal         VATNSP
042900130306     C                   ELSE
043000130306     C                   SETON                                        31
043100130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043200130306     C                             + ' ' + 'VABNSP VATNSP'
043300130306     C                   ENDIF
043400080923     C                   ENDIF
043500071003     C* NCL
043600130311     C                   EVAL      PiStr=%trim(%subst(vindta:267:5))
043700070928     C                   EXSR      CHKNUM
043800070928     C                   IF        PiInt=*on
043900070928     C                   Z-ADD     PiVal         VABNCL
044000070928     C                   ELSE
044100070928     C                   SETON                                        32
044200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044300070928     C                             + ' ' + 'VABNCL'
044400070928     C                   ENDIF
044500071003     C* PKB
044600130311     C                   EVAL      PiStr=%trim(%subst(vindta:272:9))
044700070928     C                   EXSR      CHKNUM
044800070928     C                   IF        PiNum=*on
044900130306     C* il dato ha 2 decimali
045000130306     C                   EVAL(H)   VABPKB = PiVal/100
045100070928     C                   ELSE
045200070928     C                   SETON                                        32
045300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045400070928     C                             + ' ' + 'VABPKB'
045500070928     C                   ENDIF
045600080923     C* CAS
045700130306     C* l'importo contrassegno ha 2 decimali e NON viene scritto con separatore decimale
045800130311     C                   IF        %trim(%subst(vindta:369:13))<>*blanks
045900080923     C                             AND
046000130311     C                             %trim(%subst(vindta:369:13))<>*zeros
046100130311     C                   EVAL      VABVCA=%trim(%subst(vindta:366:3))
046200070928     C                   MOVEL     '1'           FlgCAS
046300130311     C                   EVAL      PiStr=%trim(%subst(vindta:369:13))
046400070928     C                   EXSR      CHKNUM
046500070928     C                   IF        PiNum=*on
046600130311     C* il dato ha 3 decimali
046700130311     C                   EVAL(H)   VABCAS = PiVal/1000
046800070928     C                   ELSE
046900070928     C                   SETON                                        32
047000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100070928     C                             + ' ' + 'VABCAS'
047200070928     C                   ENDIF
047300070928     C                   ENDIF
047400130311     C*
047500130311     C* DCR
047600130311     C                   EVAL      PiStr=%trim(%subst(vindta:382:8))
047700130311     C                   EXSR      CHKNUM
047800130311     C                   IF        PiInt=*on
047900130311     C                   EVAL      VABDCR = PiVal
048000130311     C                   ELSE
048100130311     C                   SETON                                        32
048200130311     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048300130311     C                             + ' ' + 'VABDCR'
048400130311     C                   ENDIF
048500070730     C*
048600121109     C* Considerazioni finali su CBO/CAS
048700121106     C*
048800121106      * se c/assegno impostato
048900121106     C                   IF        VABCAS > 0
049000121109     C                   IF        VABCBO = '1'
049100121109     C                   EVAL      VABCBO = '4'
049200121109     C                   ENDIF
049300121109     C                   IF        VABCBO = '2'
049400121109     C                   EVAL      VABCBO = '6'
049500121109     C                   ENDIF
049600121106     C                   ENDIF
049700130306     C*
049800130306     C* Se provincia nn valorizzata la reperisco
049900130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
050000130306     C                   IF        VABPRD =  *blanks AND
050100130306     C                             VABNZD =  *blanks AND
050200130306     C                             VABCAD <> *blanks AND
050300130306     C                             VABLOD <> *blanks
050400130306     C                   CLEAR                   TISI95DS
050500130306     C                   EVAL      I95TCN = '3'
050600130306     C                   Z-ADD     datcor        I95DAT
050700130306     C                   EVAL      I95NAR = VABNZD
050800130306     C                   EVAL      I95CAP = VABCAD
050900130306     C                   EVAL      I95LOC = VABLOD
051000130306     C                   CALL      'TISI95R'
051100130306     C                   PARM                    TISI95DS
051200130306     C                   EVAL      VABPRD = O95PRV
051300130306     C                   ENDIF
051400020305     C*
051500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051600011113     C                   EXSR      CHKIMPDIV
051700010202     C*
051800000801     C* Ebbene...
051900000801     C                   ADD       1             §CTRMO
052000070530     C                   IF        *in31 <> *zeros OR
052100070530     C                             *in32 <> *zeros
052200000801     C                   ADD       1             §CTRNO
052300020725     C                   EVAL      x_vinflg = '2'
052400000801     C                   ELSE
052500010201     C                   ADD       1             §CTROKVB
052600000801     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900010201     C*----------------------------------------------------*
053000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053100010201     C*----------------------------------------------------*
053200071003     C     WRIVAT_B      BEGSR
053300090204     C*
053400090204     C                   eval      vatAAS = vabAAS
053500090204     C                   eval      vatLNP = vabLNP
053600090204     C                   eval      vatNRS = vabNRS
053700090204     C                   eval      vatNSP = vabNSP
053800010201     C*
053900101007     C* Valorizzo il buffer di scrittura del FIVAT
054000070928     C* - TIPO RECORD "B"
054100130311     C                   if        %subst(vindta:281:15) <> *blanks
054200070928     C                   eval      VATTRC = 'B'
054300130311     C                   eval      VATNOT = %trim(%subst(vindta:281:15))
054400021113     C                   write     FIVAT000
054500020725     C                   endif
054600010201     C*
054700010201     C                   ENDSR
054800071003     C*----------------------------------------------------*
054900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
055000071003     C*----------------------------------------------------*
055100071003     C     WRIVAT_E      BEGSR
055200090204     C*
055300090204     C                   eval      vatAAS = vabAAS
055400090204     C                   eval      vatLNP = vabLNP
055500090204     C                   eval      vatNRS = vabNRS
055600090204     C                   eval      vatNSP = vabNSP
055700071003     C*
055800101007     C* Valorizzo il buffer di scrittura del FIVAT
055900071003     C* - TIPO RECORD "E"
056000130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
056100130306     C                   eval      VATTRC = 'E'
056200121109     C*
056300130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
056400071003     C                   write     FIVAT000
056500121109     C*
056600130306     C***                endif
056700071003     C*
056800071003     C                   ENDSR
056900020904
057000020904
057100010202     C*----------------------------------------------------*
057200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057300010202     C*----------------------------------------------------*
057400020305     C     PREVAT        BEGSR
057500010202     C*
057600021113     C* Compongo il nome del membro da dare al FIVATWWR
057700010202     C                   eval      parmbr = vlrhdl
057800010202     C                   movel     'M'           parmbr
057900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
058000010202     C                   eval      paropz = '1'
058100010202     C* Effettuo la chiamata al CLLE preposto
058200050128     C                   call(e)   'TITVVTC'
058300010202     C                   parm                    parccm
058400010202     C                   parm                    parmbr
058500010202     C                   parm                    paropz
058600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058700010202     C                   if        %error
058800010202     C                   movel     '1'           chkcall
058900010202     C                   else
059000010202     C                   movel     '0'           chkcall
059100010202     C                   endif
059200010202     C*
059300010202     C                   ENDSR
059400000801     C*----------------------------------------------------*
059500000801     C*  CONTROLLO NUMERICITA' CAMPI
059600000801     C*----------------------------------------------------*
059700000801     C     CHKNUM        BEGSR
059800081113     C*
059900081113     C                   IF        PiDecChr = *blanks
060000121109     C                   EVAL      PiDecChr = ','
060100081113     C                   ENDIF
060200081113     C*
060300081113     C                   callp(e)  UBISNUM_Check(PiStr
060400081113     C                                          :PiDecChr
060500081113     C                                          :PiVal
060600081113     C                                          :PiNum
060700081113     C                                          :PiInt)
060800081113     C*
060900000801     C                   IF        %error
061000000801     C                   EVAL      PiInt=*off
061100000801     C                   ENDIF
061200000801     C*
061300000801     C                   ENDSR
061400000801     C***
061500000801
061600011113
061700011113     C*----------------------------------------------------*
061800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061900011113     C*----------------------------------------------------*
062000011113     C     CHKIMPDIV     BEGSR
062100011113     C*
062200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062300011113     C                   Z-ADD     *zeros        wrkDec            9 9
062400011113     C*
062500011113     C* Come prima cosa effettuo considerazioni sulla divisa
062600011113     C                   IF        vabIAS > *zeros
062700011113     C                   IF        vabVAS <> 'EUR'
062800011113     C                   EVAL      vabVAS =  'ITL'
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C                   IF        vabCAS > *zeros
063300011113     C                   IF        vabVCA <> 'EUR'
063400011113     C                   EVAL      vabVCA =  'ITL'
063500011113     C                   ENDIF
063600011113     C                   ENDIF
063700011113     C*
063800011113     C                   IF        vabVMD > *zeros
063900020305     C                   IF        vabVAD <> 'EUR'
064000011113     C                   EVAL      vabVAD =  'ITL'
064100011113     C                   ENDIF
064200011113     C                   ENDIF
064300011113     C*
064400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064500011113     C                   Z-ADD     vabIAS        wrkDec
064600011113     C                   IF        wrkDec > *zeros
064700011113     C                   IF        vabVAS = 'ITL'
064800011113     C                   EVAL      vabIAS = *zeros
064900011113     C                   ENDIF
065000011113     C                   ENDIF
065100011113     C*
065200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065300011113     C                   Z-ADD     vabCAS        wrkDec
065400011113     C                   IF        wrkDec > *zeros
065500011113     C                   IF        vabVCA = 'ITL'
065600011113     C                   EVAL      vabCAS = *zeros
065700011113     C                   ENDIF
065800011113     C                   ENDIF
065900011113     C*
066000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066100011113     C                   Z-ADD     vabVMD        wrkDec
066200011113     C                   IF        wrkDec > *zeros
066300011113     C                   IF        vabVAD = 'ITL'
066400011113     C                   EVAL      vabVMD = *zeros
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C                   ENDSR
066900011113     C***
067000011113
067100011113
067200000801
067300000801
067400990920      /TITLE Invio dei dati al punto operativo.
067500010202     C     invio         BEGSR
067600990920     C*
067700021113     C* 1° invio FIVAT
067800010201     C                   reset                   dscmz
067900010201     C                   move      vlrpoi        cmzdst
068000021113     C                   eval      cmzfld = 'FIVATWWR'
068100010201     C                   eval      cmzmbd = vlrhdl
068200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068300021009     C***                if        prmfir = *blanks
068400021113     C                   eval      cmzfla = 'FIVAT00F'
068500021113     C                   eval      cmzmba = 'FIVAT00F'
068600021009     C***                else
068700021009     C***                eval      cmzfla = prmfir
068800021009     C***                eval      cmzmba = prmfir
068900021009     C***                endif
069000010201     C                   eval      cmznrr = *zeros
069100020305     C                   move      §ctrokvt      cmznrr
069200021018     C                   eval      cmzlba = vlrfl1
069300010201     C                   call(e)   'TIS711C'
069400010201     C                   parm                    dscmz
069500010201     C                   parm      *blanks       esito
069600010205     C                   if        %error
069700010205     C                             or cmzerr = '1'
069800010205     C                             or esito  = '1'
069900010205     C                   eval      wrkesito = '3'
070000010205     C                   else
070100010201     C*
070200021113     C* 2° invio FIVAB
070300010201     C                   reset                   dscmz
070400010201     C                   move      vlrpoi        cmzdst
070500010201     C                   eval      cmzfld = vlrfou
070600010201     C                   eval      cmzmbd = vlrhdl
070700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070800021009     C***                if        prmfir = *blanks
070900021113     C                   eval      cmzfla = 'FIVAB00F'
071000021113     C                   eval      cmzmba = 'FIVAB00F'
071100021009     C***                else
071200021009     C***                eval      cmzfla = prmfir
071300021009     C***                eval      cmzmba = prmfir
071400021009     C***                endif
071500010201     C                   eval      cmznrr = *zeros
071600010201     C                   move      §ctrokvb      cmznrr
071700021018     C                   eval      cmzlba = vlrfl1
071800010201     C                   call(e)   'TIS711C'
071900010201     C                   parm                    dscmz
072000010201     C                   parm      *blanks       esito
072100010201     C                   if        %error
072200010201     C                             or cmzerr = '1'
072300010201     C                             or esito  = '1'
072400010201     C                   eval      wrkesito = '3'
072500010201     C                   endif
072600010205     C                   endif
072700990920     C*
072800000613     C                   ENDSR
072900000613     C***
073000070411
073100070411     C     *pssr         BEGSR
073200070411     C*
073300070411     C                   if        %open(tivin00r)
073400070411     C                   close     tivin00r
073500070411     C                   endif
073600070411     C                   if        %open(fivabwwr)
073700070411     C                   close     fivabwwr
073800070411     C                   endif
073900070411     C                   if        %open(fivatwwr)
074000070411     C                   close     fivatwwr
074100070411     C                   endif
074200070411     C*
074300070411     C* Effettuo la chiamata al CLLE preposto
074400070411     C                   call(e)   'TITVVTC'
074500070411     C                   parm                    parccm
074600070411     C                   parm                    parmbr
074700070411     C                   parm      '2'           paropz
074800070411     C*
074900070411     C                   eval      wrkesito = '2'
075000070411     C*
075100070411     C                   seton                                        LR
075200070411     C*
075300070411     C                   ENDSR     '*CANCL'
075400070411     C***
075500070411
075600990910
075700000613     C     *inzsr        BEGSR
075800990910     C*
075900990910     C     *entry        plist
076000990920     C                   parm                    tivlrds
076100990921     C                   parm      wrkesito      esito
076200000724     C                   parm                    prmlit
076300000710     C                   parm                    prmfir
076400000613     C*
076500000830     C* CALCOLA LA DATA CORRENTE
076600100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076700100324     C                   eval      datcor = %dec(%date() : *ISO)
076800000830     C*
076900000613     C                   ENDSR
077000000613     C***
