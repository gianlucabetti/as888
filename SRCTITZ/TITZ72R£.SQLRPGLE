000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130311      * Giorni di chiusura:
000500130311      * il cliente invia 2 possibili giorni di chiusura (da 1 a 7, da lun a dom) e poi la mod.
000600130311      * di chiusura, a differenza di noi che abbiano due campi da 2 char con il primo byte il nr.
000700130311      * del giorno e nel secondo la modalità. Quindi va fatto una scomposizione del dato.
000800130311      * Inoltre noi con ' ' dichiariamo tutto il giorno, mentre il cliente scrive 'T'.
000900130311      * Tipo servizio:
001000130311      * il cliente dichiara diversi tipi si servizio ma a noi interessa solo se indica U che per noi
001100130311      * diventa 'E'.
001200130311      *
001300121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001400990908
001500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001600021113     FFIVABwwr  O    E             DISK    usropn
001700021113     FFIVATwwr  O    E             DISK    usropn
001800990908
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100000613     D rrnum           s              6  0 INZ(*zeros)
003200130311     D depspe          s             13    INZ(*blanks)
003300130311     D curspe          s             13    INZ(*blanks)
003400010202     D parccm          s              8    INZ(*blanks)
003500010202     D parmbr          s             10    INZ(*blanks)
003600010202     D paropz          s              1    INZ(*blanks)
003700010202     D chkcall         s              1    INZ(*blanks)
003800080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003900070530     D Num5_0          s              5  0
004000121011     D w70             s             70
004100000830
004200020725
004300020725     D*------------------
004400020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004500020725     D*------------------
004600070530     D INPUT_DS        DS                  INZ
004700101007     D  VINDTA                     2048
004800101007     D  VINFLG                        1
004900130311     D  VINSPE                       13
005000101007     D  VINRRN                        8  0
005100020725     D*
005200080923     D*------------------
005300080923     D* DS REPERIMENTO NUMERATORE
005400080923     D*------------------
005500080923     D trul33ds      e ds                  inz
005600080923     D*------------------
005700080923     D* DS ARCHITETTURA
005800080923     D*------------------
005900080923     D kpjba         e ds                  inz
006000080923     D*------------------
006100121126     D BarcodeDS       DS           770    INZ
006200121126     D  wBarcode                     14    dim(55)
006300121109     D*------------------
006400121109     D TRUL28DSX     e ds                  inz
006500121109     D*------------------
006600121109     D wX              s              3s 0
006700081113
006800081113     D*------------------
006900081113     D* LINKING A DEFINIZIONI ESTERNE
007000081113     D*------------------
007100100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007200081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007300081113
007400990908
007500010201
007600010201
007700080117     C*
007800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007900080117     C
008000080117     C/EXEC SQL
008100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008200080117     C/END-EXEC
008300080117     C
008400000913     C                   reset                   rrnum
008500990921     C                   reset                   esito
008600990921     C                   reset                   wrkesito
008700000613     C*
008800070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008900000613     C*
009000010202     C* Effettuo la chiamata al CLLE preposto
009100050128     C                   call(e)   'TITVVTC'
009200010202     C                   parm                    parccm
009300010202     C                   parm                    parmbr
009400010202     C                   parm      '2'           paropz
009500070730     C*
009600070730     C* Effettuo lancio TISI95 solo x chiusura
009700070730     C                   CLEAR                   TISI95DS
009800070730     C                   EVAL      I95TLA = 'C'
009900070730     C                   CALL      'TISI95R'
010000070730     C                   PARM                    TISI95DS
010100000616     C*
010200000801     C
010300010201     C                   seton                                        LR
010400000801
010500910830     C*--------------------------------------------------------
010600070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010700910830     C*--------------------------------------------------------
010800070530     C     RWFILE        BEGSR
010900990910     C*
011000990914     C                   if        not %open(tivin00r)
011100990908     C                   open      tivin00r
011200990914     C                   endif
011300021113     C                   if        not %open(fivabwwr)
011400021113     C                   open      fivabwwr
011500990914     C                   endif
011600070530     C*
011700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011800020305     C                   exsr      prevat
011900010201     C*
012000010202     C                   if        chkcall = '0'
012100010202     C*
012200021113     C                   if        not %open(fivatwwr)
012300021113     C                   open      fivatwwr
012400010201     C                   endif
012500080117     C*
012600080117     C                   EXSR      INZVAR
012700080117     C                   EXSR      DEFCAM
012800990910     C*
012900010201     C                   clear                   §CTROKVB          5 0
013000020305     C                   clear                   §CTROKVT          5 0
013100000801     C                   clear                   §CTRMO            5 0
013200000801     C                   clear                   §CTRNO            5 0
013300990910     C*
013400020725     C/EXEC SQL
013500020725     C+ declare C1 cursor for select
013600130311     C+ vindta, vinflg, substr(vindta, 13, 13) as sped, rrn(tivin00r)
013700060223     C+ from tivin00r
013800060223     C+ order by sped
013900020725     C+ for read only
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300020725     C+ open C1
014400020725     C/END-EXEC
014500020725     C
014600020725     C/EXEC SQL
014700070530     C+ Fetch C1 into :INPUT_DS
014800020725     C/END-EXEC
014900020725     C*
015000020725     C                   dow       sqlcod = *zeros
015100990913     C*
015200020725     C                   if        vindta > *blanks
015300000613     C                   add       1             rrnum
015400000801     C*
015500020725     C                   if        vinflg = *blanks
015600020725     C                             or vinflg = '0'
015700020725     C                             or vinflg = '2'
015800000801     C*
015900020725     C                   clear                   x_vinmsg
016000020725     C                   eval      x_vinflg = '1'
016100010305     C*
016200121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016300121109     C* (uso l'RMN)
016400130311     C                   EVAL      PiStr=%trim(%subst(vindta:13:13))
016500020305     C                   MOVEL(p)  PiStr         curspe
016600010305     C*
016700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016800080923     C                   if        *in50 = *off
016900071003     C                   exsr      impvab
017000071003     C                   exsr      wrivab
017100130306     C                   exsr      wrivat_b                                     => carico VAT
017200071003     C                   else
017300080923     C*
017400071009     C                   if        wDISK = 'D'
017500071009     C                   exsr      impvab
017600071009     C                   exsr      wrivab
017700130306     C                   exsr      wrivat_b                                     => carico VAT
017800130306     C***                exsr      wrivat_e                                     => carico VAT
017900071009     C                   else
018000071009     C*
018100010305     C                   if        depspe = *blanks                             => 1° giro
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300080117     C                   clear                   tivinds
018400020305     C                   exsr      impvab
018500130306     C                   exsr      wrivat_b                                     => carico VAT
018600130306     C***50              exsr      wrivat_e                                     => carico VAT
018700010305     C                   else
018800020725     C                   if        depspe <> curspe                             => rottura di spediz
018900010305     C                   eval      depspe = curspe                              => memorizz. spediz
019000070928     C                   exsr      wrivab
019100080117     C                   clear                   tivinds
019200020305     C                   exsr      impvab
019300130306     C                   exsr      wrivat_b                                     => carico VAT
019400130306     C***50              exsr      wrivat_e                                     => carico VAT
019500020305     C                   else                                                   => x stessa spediz
019600090204     C***                exsr      impvab
019700130306     C                   exsr      wrivat_b                                     => carico VAT
019800130306     C***50              exsr      wrivat_e                                     => carico VAT
019900010305     C                   endif
020000010305     C                   endif
020100010305     C                   endif
020200071003     C                   endif
020300071009     C                   endif
020400000905     C*
020500000905     C                   else
020600020725     C                   eval      x_vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900020725     C     VINRRN        chain     tivin000
021000020725     C                   if        %found(tivin00r)
021100020725     C                   eval      y_vinflg = x_vinflg
021200020725     C                   eval      y_vinmsg = x_vinmsg
021300020725     C                   update    tivin000
021400020725     C                   endif
021500020725     C*
021600020725     C/EXEC SQL
021700070530     C+ Fetch C1 into :INPUT_DS
021800020725     C/END-EXEC
021900020725     C*
022000020725     C                   enddo
022100020725     C*
022200020725     C/EXEC SQL
022300020725     C+ close C1
022400020725     C/END-EXEC
022500000905     C*
022600020305     C* Scarico i VAB rimasti "in sospeso"
022700071009     C                   if        wDISK = 'C'
022800071009     C                   exsr      wrivab
022900071009     C                   endif
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800010201     C                   if        §ctrokvb > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021113     C                   if        %open(fivabwwr)
024900021113     C                   close     fivabwwr
025000990914     C                   endif
025100021113     C                   if        %open(fivatwwr)
025200021113     C                   close     fivatwwr
025300010201     C                   endif
025400990910     C*
025500010201     C                   if        §ctrokvb > 0
025600000724     C                             and vlrpoi <> *zeros
025700010202     C                   exsr      invio
025800990920     C                   endif
025900990920     C*
026000910830     C                   ENDSR
026100000613     C***
026200010305
026300010305     C*----------------------------------------------------*
026400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026500010305     C*----------------------------------------------------*
026600020305     C     WRIVAB        BEGSR
026700070730     C*
026800071003     C* Considerazioni finali
026900071003     C                   if        VABRMA = *blanks
027000071003     C                   movel(P)  VABRMN        VABRMA
027100071003     C                   endif
027200071003     C*
027300021113     C                   write     fivab000                                     => scarico il VAB
027400010305     C*
027500010305     C                   ENDSR
027600990920
027700000801     C*----------------------------------------------------*
027800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027900000801     C*----------------------------------------------------*
028000010201     C     INZVAR        BEGSR
028100000801     C*
028200010201     C                   Z-ADD     *zeros        Num5_0
028300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028400020725     C                   MOVEL     '0'           FlgCAS            1
028500000801     C*
028600000801     C                   ENDSR
028700000801     C*----------------------------------------------------*
028800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028900000801     C*----------------------------------------------------*
029000020904     C     DEFCAM        BEGSR
029100080923     C*
029200080923     C                   SETOFF                                       5051
029300000801     C*
029400021113     C                   CLEAR                   FIVAB000
029500021113     C                   CLEAR                   FIVAT000
029600070730     C* Imposto i valori di default...
029700130311     C                   EVAL      VABCCM = 0016186
029800130311     C                   EVAL      VATCCM = 0016186
029900130311     C                   EVAL      VABLNP = 001
030000130311     C                   EVAL      VATLNP = 001
030100130306     C                   EVAL      VABCTR = 000
030200130311     C                   EVAL      VABCBO='1'
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
038200130311     C* se nel campo cliente c'è 'U' allora è priority ('E')
038300130311     C                   IF        %trim(%subst(vindta:390:2))='U'
038400130311     C                   EVAL      VABTSP='E'
038500130311     C                   ENDIF
038600120717     C*
038700071003     C* CAD
038800130311     C                   EVAL      PiStr=%trim(%subst(vindta:402:5))
038900121001     C                   EXSR      CHKNUM
039000121001     C                   IF        PiInt=*on
039100121001     C                   Z-ADD     PiVal         Num5_0
039200121001     C                   MOVEL     Num5_0        VABCAD
039300121001     C                   ELSE
039400121001     C                   SETON                                        32
039500121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039600121001     C                             + ' ' + 'VABCAD'
039700121001     C                   ENDIF
039800080923     C* RMN
039900130311     C                   EVAL      PiStr=%trim(%subst(vindta:13:13))
040000121001     C                   EXSR      CHKNUM
040100121001     C                   IF        PiInt=*on
040200121001     C                   Z-ADD     PiVal         VABRMN
040300121001     C                   ELSE
040400121001     C                   SETON                                        32
040500121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040600121001     C                             + ' ' + 'VABRMN'
040700121001     C                   ENDIF
040800080923     C* NSP
040900080923     C                   IF        *in51 = *on
041000080923     C* NSP => Stacco un numeratore da AZNUM
041100080923     C                   clear                   TRUL33DS
041200080923     C                   eval      I33OPE = *zeros
041300080923     C                   eval      I33CNU = 302
041400080923     C                   eval      I33NUM = 1
041500080923     C                   movel     TRUL33DS      KPJBU
041600080923     C                   call      'TRUL33R'
041700080923     C                   parm                    KPJBA
041800080923     C                   movel     KPJBU         TRUL33DS
041900080923     C                   if        O33ERR = *zeros
042000080923     C                   z-add     O33NRF        VABNSP
042100080923     C                   z-add     O33NRF        VATNSP
042200080923     C                   else
042300080923     C                   SETON                                        31
042400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042500080923     C                             + ' ' + 'VABNSP VATNSP'
042600080923     C                   endif
042700080923     C                   ELSE
042800130306     C                   EVAL      PiStr=%trim(%subst(vindta:134:8))
042900130306     C                   EXSR      CHKNUM
043000130306     C                   IF        PiInt=*on
043100130306     C                   Z-ADD     PiVal         VABNSP
043200130306     C                   Z-ADD     PiVal         VATNSP
043300130306     C                   ELSE
043400130306     C                   SETON                                        31
043500130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043600130306     C                             + ' ' + 'VABNSP VATNSP'
043700130306     C                   ENDIF
043800080923     C                   ENDIF
043900071003     C* NCL
044000130311     C                   EVAL      PiStr=%trim(%subst(vindta:267:5))
044100070928     C                   EXSR      CHKNUM
044200070928     C                   IF        PiInt=*on
044300070928     C                   Z-ADD     PiVal         VABNCL
044400070928     C                   ELSE
044500070928     C                   SETON                                        32
044600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700070928     C                             + ' ' + 'VABNCL'
044800070928     C                   ENDIF
044900071003     C* PKB
045000130311     C                   EVAL      PiStr=%trim(%subst(vindta:272:9))
045100070928     C                   EXSR      CHKNUM
045200070928     C                   IF        PiNum=*on
045300130306     C* il dato ha 2 decimali
045400130306     C                   EVAL(H)   VABPKB = PiVal/100
045500070928     C                   ELSE
045600070928     C                   SETON                                        32
045700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045800070928     C                             + ' ' + 'VABPKB'
045900070928     C                   ENDIF
046000080923     C* CAS
046100130306     C* l'importo contrassegno ha 2 decimali e NON viene scritto con separatore decimale
046200130311     C                   IF        %trim(%subst(vindta:369:13))<>*blanks
046300080923     C                             AND
046400130311     C                             %trim(%subst(vindta:369:13))<>*zeros
046500130311     C                   EVAL      VABVCA=%trim(%subst(vindta:366:3))
046600070928     C                   MOVEL     '1'           FlgCAS
046700130311     C                   EVAL      PiStr=%trim(%subst(vindta:369:13))
046800070928     C                   EXSR      CHKNUM
046900070928     C                   IF        PiNum=*on
047000130311     C* il dato ha 3 decimali
047100130311     C                   EVAL(H)   VABCAS = PiVal/1000
047200070928     C                   ELSE
047300070928     C                   SETON                                        32
047400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500070928     C                             + ' ' + 'VABCAS'
047600070928     C                   ENDIF
047700070928     C                   ENDIF
047800130311     C*
047900130311     C* DCR
048000130311     C                   EVAL      PiStr=%trim(%subst(vindta:382:8))
048100130311     C                   EXSR      CHKNUM
048200130311     C                   IF        PiInt=*on
048300130311     C                   EVAL      VABDCR = PiVal
048400130311     C                   ELSE
048500130311     C                   SETON                                        32
048600130311     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700130311     C                             + ' ' + 'VABDCR'
048800130311     C                   ENDIF
048900070730     C*
049000121109     C* Considerazioni finali su CBO/CAS
049100121106     C*
049200121106      * se c/assegno impostato
049300121106     C                   IF        VABCAS > 0
049400121109     C                   IF        VABCBO = '1'
049500121109     C                   EVAL      VABCBO = '4'
049600121109     C                   ENDIF
049700121109     C                   IF        VABCBO = '2'
049800121109     C                   EVAL      VABCBO = '6'
049900121109     C                   ENDIF
050000121106     C                   ENDIF
050100130306     C*
050200130306     C* Se provincia nn valorizzata la reperisco
050300130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
050400130306     C                   IF        VABPRD =  *blanks AND
050500130306     C                             VABNZD =  *blanks AND
050600130306     C                             VABCAD <> *blanks AND
050700130306     C                             VABLOD <> *blanks
050800130306     C                   CLEAR                   TISI95DS
050900130306     C                   EVAL      I95TCN = '3'
051000130306     C                   Z-ADD     datcor        I95DAT
051100130306     C                   EVAL      I95NAR = VABNZD
051200130306     C                   EVAL      I95CAP = VABCAD
051300130306     C                   EVAL      I95LOC = VABLOD
051400130306     C                   CALL      'TISI95R'
051500130306     C                   PARM                    TISI95DS
051600130306     C                   EVAL      VABPRD = O95PRV
051700130306     C                   ENDIF
051800020305     C*
051900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052000011113     C                   EXSR      CHKIMPDIV
052100010202     C*
052200000801     C* Ebbene...
052300000801     C                   ADD       1             §CTRMO
052400070530     C                   IF        *in31 <> *zeros OR
052500070530     C                             *in32 <> *zeros
052600000801     C                   ADD       1             §CTRNO
052700020725     C                   EVAL      x_vinflg = '2'
052800000801     C                   ELSE
052900010201     C                   ADD       1             §CTROKVB
053000000801     C                   ENDIF
053100000801     C*
053200000801     C                   ENDSR
053300010201     C*----------------------------------------------------*
053400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053500010201     C*----------------------------------------------------*
053600071003     C     WRIVAT_B      BEGSR
053700090204     C*
053800090204     C                   eval      vatAAS = vabAAS
053900090204     C                   eval      vatLNP = vabLNP
054000090204     C                   eval      vatNRS = vabNRS
054100090204     C                   eval      vatNSP = vabNSP
054200010201     C*
054300101007     C* Valorizzo il buffer di scrittura del FIVAT
054400070928     C* - TIPO RECORD "B"
054500130311     C                   if        %subst(vindta:281:15) <> *blanks
054600070928     C                   eval      VATTRC = 'B'
054700130311     C                   eval      VATNOT = %trim(%subst(vindta:281:15))
054800021113     C                   write     FIVAT000
054900020725     C                   endif
055000010201     C*
055100010201     C                   ENDSR
055200071003     C*----------------------------------------------------*
055300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
055400071003     C*----------------------------------------------------*
055500071003     C     WRIVAT_E      BEGSR
055600090204     C*
055700090204     C                   eval      vatAAS = vabAAS
055800090204     C                   eval      vatLNP = vabLNP
055900090204     C                   eval      vatNRS = vabNRS
056000090204     C                   eval      vatNSP = vabNSP
056100071003     C*
056200101007     C* Valorizzo il buffer di scrittura del FIVAT
056300071003     C* - TIPO RECORD "E"
056400130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
056500130306     C                   eval      VATTRC = 'E'
056600121109     C*
056700130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
056800071003     C                   write     FIVAT000
056900121109     C*
057000130306     C***                endif
057100071003     C*
057200071003     C                   ENDSR
057300020904
057400020904
057500010202     C*----------------------------------------------------*
057600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057700010202     C*----------------------------------------------------*
057800020305     C     PREVAT        BEGSR
057900010202     C*
058000021113     C* Compongo il nome del membro da dare al FIVATWWR
058100010202     C                   eval      parmbr = vlrhdl
058200010202     C                   movel     'M'           parmbr
058300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
058400010202     C                   eval      paropz = '1'
058500010202     C* Effettuo la chiamata al CLLE preposto
058600050128     C                   call(e)   'TITVVTC'
058700010202     C                   parm                    parccm
058800010202     C                   parm                    parmbr
058900010202     C                   parm                    paropz
059000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059100010202     C                   if        %error
059200010202     C                   movel     '1'           chkcall
059300010202     C                   else
059400010202     C                   movel     '0'           chkcall
059500010202     C                   endif
059600010202     C*
059700010202     C                   ENDSR
059800000801     C*----------------------------------------------------*
059900000801     C*  CONTROLLO NUMERICITA' CAMPI
060000000801     C*----------------------------------------------------*
060100000801     C     CHKNUM        BEGSR
060200081113     C*
060300081113     C                   IF        PiDecChr = *blanks
060400121109     C                   EVAL      PiDecChr = ','
060500081113     C                   ENDIF
060600081113     C*
060700081113     C                   callp(e)  UBISNUM_Check(PiStr
060800081113     C                                          :PiDecChr
060900081113     C                                          :PiVal
061000081113     C                                          :PiNum
061100081113     C                                          :PiInt)
061200081113     C*
061300000801     C                   IF        %error
061400000801     C                   EVAL      PiInt=*off
061500000801     C                   ENDIF
061600000801     C*
061700000801     C                   ENDSR
061800000801     C***
061900000801
062000011113
062100011113     C*----------------------------------------------------*
062200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
062300011113     C*----------------------------------------------------*
062400011113     C     CHKIMPDIV     BEGSR
062500011113     C*
062600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062700011113     C                   Z-ADD     *zeros        wrkDec            9 9
062800011113     C*
062900011113     C* Come prima cosa effettuo considerazioni sulla divisa
063000011113     C                   IF        vabIAS > *zeros
063100011113     C                   IF        vabVAS <> 'EUR'
063200011113     C                   EVAL      vabVAS =  'ITL'
063300011113     C                   ENDIF
063400011113     C                   ENDIF
063500011113     C*
063600011113     C                   IF        vabCAS > *zeros
063700011113     C                   IF        vabVCA <> 'EUR'
063800011113     C                   EVAL      vabVCA =  'ITL'
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C                   IF        vabVMD > *zeros
064300020305     C                   IF        vabVAD <> 'EUR'
064400011113     C                   EVAL      vabVAD =  'ITL'
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064900011113     C                   Z-ADD     vabIAS        wrkDec
065000011113     C                   IF        wrkDec > *zeros
065100011113     C                   IF        vabVAS = 'ITL'
065200011113     C                   EVAL      vabIAS = *zeros
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065700011113     C                   Z-ADD     vabCAS        wrkDec
065800011113     C                   IF        wrkDec > *zeros
065900011113     C                   IF        vabVCA = 'ITL'
066000011113     C                   EVAL      vabCAS = *zeros
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066500011113     C                   Z-ADD     vabVMD        wrkDec
066600011113     C                   IF        wrkDec > *zeros
066700011113     C                   IF        vabVAD = 'ITL'
066800011113     C                   EVAL      vabVMD = *zeros
066900011113     C                   ENDIF
067000011113     C                   ENDIF
067100011113     C*
067200011113     C                   ENDSR
067300011113     C***
067400011113
067500011113
067600000801
067700000801
067800990920      /TITLE Invio dei dati al punto operativo.
067900010202     C     invio         BEGSR
068000990920     C*
068100021113     C* 1° invio FIVAT
068200010201     C                   reset                   dscmz
068300010201     C                   move      vlrpoi        cmzdst
068400021113     C                   eval      cmzfld = 'FIVATWWR'
068500010201     C                   eval      cmzmbd = vlrhdl
068600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068700021009     C***                if        prmfir = *blanks
068800021113     C                   eval      cmzfla = 'FIVAT00F'
068900021113     C                   eval      cmzmba = 'FIVAT00F'
069000021009     C***                else
069100021009     C***                eval      cmzfla = prmfir
069200021009     C***                eval      cmzmba = prmfir
069300021009     C***                endif
069400010201     C                   eval      cmznrr = *zeros
069500020305     C                   move      §ctrokvt      cmznrr
069600021018     C                   eval      cmzlba = vlrfl1
069700010201     C                   call(e)   'TIS711C'
069800010201     C                   parm                    dscmz
069900010201     C                   parm      *blanks       esito
070000010205     C                   if        %error
070100010205     C                             or cmzerr = '1'
070200010205     C                             or esito  = '1'
070300010205     C                   eval      wrkesito = '3'
070400010205     C                   else
070500010201     C*
070600021113     C* 2° invio FIVAB
070700010201     C                   reset                   dscmz
070800010201     C                   move      vlrpoi        cmzdst
070900010201     C                   eval      cmzfld = vlrfou
071000010201     C                   eval      cmzmbd = vlrhdl
071100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071200021009     C***                if        prmfir = *blanks
071300021113     C                   eval      cmzfla = 'FIVAB00F'
071400021113     C                   eval      cmzmba = 'FIVAB00F'
071500021009     C***                else
071600021009     C***                eval      cmzfla = prmfir
071700021009     C***                eval      cmzmba = prmfir
071800021009     C***                endif
071900010201     C                   eval      cmznrr = *zeros
072000010201     C                   move      §ctrokvb      cmznrr
072100021018     C                   eval      cmzlba = vlrfl1
072200010201     C                   call(e)   'TIS711C'
072300010201     C                   parm                    dscmz
072400010201     C                   parm      *blanks       esito
072500010201     C                   if        %error
072600010201     C                             or cmzerr = '1'
072700010201     C                             or esito  = '1'
072800010201     C                   eval      wrkesito = '3'
072900010201     C                   endif
073000010205     C                   endif
073100990920     C*
073200000613     C                   ENDSR
073300000613     C***
073400070411
073500070411     C     *pssr         BEGSR
073600070411     C*
073700070411     C                   if        %open(tivin00r)
073800070411     C                   close     tivin00r
073900070411     C                   endif
074000070411     C                   if        %open(fivabwwr)
074100070411     C                   close     fivabwwr
074200070411     C                   endif
074300070411     C                   if        %open(fivatwwr)
074400070411     C                   close     fivatwwr
074500070411     C                   endif
074600070411     C*
074700070411     C* Effettuo la chiamata al CLLE preposto
074800070411     C                   call(e)   'TITVVTC'
074900070411     C                   parm                    parccm
075000070411     C                   parm                    parmbr
075100070411     C                   parm      '2'           paropz
075200070411     C*
075300070411     C                   eval      wrkesito = '2'
075400070411     C*
075500070411     C                   seton                                        LR
075600070411     C*
075700070411     C                   ENDSR     '*CANCL'
075800070411     C***
075900070411
076000990910
076100000613     C     *inzsr        BEGSR
076200990910     C*
076300990910     C     *entry        plist
076400990920     C                   parm                    tivlrds
076500990921     C                   parm      wrkesito      esito
076600000724     C                   parm                    prmlit
076700000710     C                   parm                    prmfir
076800000613     C*
076900000830     C* CALCOLA LA DATA CORRENTE
077000100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077100100324     C                   eval      datcor = %dec(%date() : *ISO)
077200000830     C*
077300000613     C                   ENDSR
077400000613     C***
