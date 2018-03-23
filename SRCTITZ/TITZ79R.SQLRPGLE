000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130405     C* se sarà presente un nr.telefonico, indicare la consegna per appuntamento
000500130405     C* se c'è il contrassegno, il tipo incasso è BM (ASSEGNO BANCARIO INTESTATO AL MITTENTE)
000600130306      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000021113     FFIVABwwr  O    E             DISK    usropn
001100021113     FFIVATwwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600130405     D depspe          s             15    INZ(*blanks)
002700130405     D curspe          s             15    INZ(*blanks)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003300070530     D Num5_0          s              5  0
003400121011     D w70             s             70
003500000830
003600020725
003700020725     D*------------------
003800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003900020725     D*------------------
004000070530     D INPUT_DS        DS                  INZ
004100101007     D  VINDTA                     2048
004200101007     D  VINFLG                        1
004300130405     D  VINSPE                       15
004400101007     D  VINRRN                        8  0
004500020725     D*
004600080923     D*------------------
004700080923     D* DS REPERIMENTO NUMERATORE
004800080923     D*------------------
004900080923     D trul33ds      e ds                  inz
005000080923     D*------------------
005100080923     D* DS ARCHITETTURA
005200080923     D*------------------
005300080923     D kpjba         e ds                  inz
005400080923     D*------------------
005500121126     D BarcodeDS       DS           770    INZ
005600121126     D  wBarcode                     14    dim(55)
005700121109     D*------------------
005800121109     D TRUL28DSX     e ds                  inz
005900121109     D*------------------
006000130405     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100130405     D*------------------
006200130405     D WLBDA8          DS                  INZ
006300130405     D  G08DAT                 1      8  0
006400130405     D  G08INV                 9     16  0
006500130405     D  G08ERR                17     17
006600130405     D  G08TGI                18     22  0
006700130405     D*
006800121109     D wX              s              3s 0
006900081113
007000081113     D*------------------
007100081113     D* LINKING A DEFINIZIONI ESTERNE
007200081113     D*------------------
007300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007500081113
007600990908
007700010201
007800010201
007900080117     C*
008000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008100080117     C
008200080117     C/EXEC SQL
008300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008400080117     C/END-EXEC
008500080117     C
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900000613     C*
009000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009100000613     C*
009200010202     C* Effettuo la chiamata al CLLE preposto
009300050128     C                   call(e)   'TITVVTC'
009400010202     C                   parm                    parccm
009500010202     C                   parm                    parmbr
009600010202     C                   parm      '2'           paropz
009700070730     C*
009800070730     C* Effettuo lancio TISI95 solo x chiusura
009900070730     C                   CLEAR                   TISI95DS
010000070730     C                   EVAL      I95TLA = 'C'
010100070730     C                   CALL      'TISI95R'
010200070730     C                   PARM                    TISI95DS
010300000616     C*
010400000801     C
010500010201     C                   seton                                        LR
010600000801
010700910830     C*--------------------------------------------------------
010800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010900910830     C*--------------------------------------------------------
011000070530     C     RWFILE        BEGSR
011100990910     C*
011200990914     C                   if        not %open(tivin00r)
011300990908     C                   open      tivin00r
011400990914     C                   endif
011500021113     C                   if        not %open(fivabwwr)
011600021113     C                   open      fivabwwr
011700990914     C                   endif
011800070530     C*
011900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012000020305     C                   exsr      prevat
012100010201     C*
012200010202     C                   if        chkcall = '0'
012300010202     C*
012400021113     C                   if        not %open(fivatwwr)
012500021113     C                   open      fivatwwr
012600010201     C                   endif
012700080117     C*
012800080117     C                   EXSR      INZVAR
012900080117     C                   EXSR      DEFCAM
013000990910     C*
013100010201     C                   clear                   §CTROKVB          5 0
013200020305     C                   clear                   §CTROKVT          5 0
013300000801     C                   clear                   §CTRMO            5 0
013400000801     C                   clear                   §CTRNO            5 0
013500990910     C*
013600020725     C/EXEC SQL
013700020725     C+ declare C1 cursor for select
013800130405     C+ vindta, vinflg, substr(vindta, 24, 15) as sped, rrn(tivin00r)
013900060223     C+ from tivin00r
014000060223     C+ order by sped
014100020725     C+ for read only
014200020725     C/END-EXEC
014300020725     C
014400020725     C/EXEC SQL
014500020725     C+ open C1
014600020725     C/END-EXEC
014700020725     C
014800020725     C/EXEC SQL
014900070530     C+ Fetch C1 into :INPUT_DS
015000020725     C/END-EXEC
015100020725     C*
015200020725     C                   dow       sqlcod = *zeros
015300990913     C*
015400020725     C                   if        vindta > *blanks
015500000613     C                   add       1             rrnum
015600000801     C*
015700020725     C                   if        vinflg = *blanks
015800020725     C                             or vinflg = '0'
015900020725     C                             or vinflg = '2'
016000000801     C*
016100020725     C                   clear                   x_vinmsg
016200020725     C                   eval      x_vinflg = '1'
016300010305     C*
016400121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016500121109     C* (uso l'RMN)
016600130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
016700020305     C                   MOVEL(p)  PiStr         curspe
016800010305     C*
016900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017000080923     C                   if        *in50 = *off
017100071003     C                   exsr      impvab
017200071003     C                   exsr      wrivab
017300130306     C                   exsr      wrivat_b                                     => carico VAT
017400071003     C                   else
017500080923     C*
017600071009     C                   if        wDISK = 'D'
017700071009     C                   exsr      impvab
017800071009     C                   exsr      wrivab
017900130306     C                   exsr      wrivat_b                                     => carico VAT
018000130306     C***                exsr      wrivat_e                                     => carico VAT
018100071009     C                   else
018200071009     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500080117     C                   clear                   tivinds
018600020305     C                   exsr      impvab
018700130306     C                   exsr      wrivat_b                                     => carico VAT
018800130306     C***50              exsr      wrivat_e                                     => carico VAT
018900010305     C                   else
019000020725     C                   if        depspe <> curspe                             => rottura di spediz
019100010305     C                   eval      depspe = curspe                              => memorizz. spediz
019200070928     C                   exsr      wrivab
019300080117     C                   clear                   tivinds
019400020305     C                   exsr      impvab
019500130306     C                   exsr      wrivat_b                                     => carico VAT
019600130306     C***50              exsr      wrivat_e                                     => carico VAT
019700020305     C                   else                                                   => x stessa spediz
019800090204     C***                exsr      impvab
019900130306     C                   exsr      wrivat_b                                     => carico VAT
020000130306     C***50              exsr      wrivat_e                                     => carico VAT
020100010305     C                   endif
020200010305     C                   endif
020300010305     C                   endif
020400071003     C                   endif
020500071009     C                   endif
020600000905     C*
020700000905     C                   else
020800020725     C                   eval      x_vinflg = '1'
020900000905     C                   endif
021000000905     C*
021100020725     C     VINRRN        chain     tivin000
021200020725     C                   if        %found(tivin00r)
021300020725     C                   eval      y_vinflg = x_vinflg
021400020725     C                   eval      y_vinmsg = x_vinmsg
021500020725     C                   update    tivin000
021600020725     C                   endif
021700020725     C*
021800020725     C/EXEC SQL
021900070530     C+ Fetch C1 into :INPUT_DS
022000020725     C/END-EXEC
022100020725     C*
022200020725     C                   enddo
022300020725     C*
022400020725     C/EXEC SQL
022500020725     C+ close C1
022600020725     C/END-EXEC
022700000905     C*
022800020305     C* Scarico i VAB rimasti "in sospeso"
022900071009     C                   if        wDISK = 'C'
023000071009     C                   exsr      wrivab
023100071009     C                   endif
023200010202     C*
023300010202     C                   endif
023400990910
023500990910     C* Se non ci sono record con errori ...
023600000710     C                   if        §ctrno = 0
023700990910     C* ... restituisco esito OK.
023800990921     C                   eval      wrkesito = '0'
023900990910     C                   else
024000010201     C                   if        §ctrokvb > 0
024100990921     C                   eval      wrkesito = '1'
024200000710     C                   else
024300000710     C                   eval      wrkesito = '2'
024400990910     C                   endif
024500000710     C                   endif
024600990910     C*
024700990914     C                   if        %open(tivin00r)
024800990908     C                   close     tivin00r
024900990914     C                   endif
025000021113     C                   if        %open(fivabwwr)
025100021113     C                   close     fivabwwr
025200990914     C                   endif
025300021113     C                   if        %open(fivatwwr)
025400021113     C                   close     fivatwwr
025500010201     C                   endif
025600990910     C*
025700010201     C                   if        §ctrokvb > 0
025800000724     C                             and vlrpoi <> *zeros
025900010202     C                   exsr      invio
026000990920     C                   endif
026100990920     C*
026200910830     C                   ENDSR
026300000613     C***
026400010305
026500010305     C*----------------------------------------------------*
026600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026700010305     C*----------------------------------------------------*
026800020305     C     WRIVAB        BEGSR
026900070730     C*
027000071003     C* Considerazioni finali
027100071003     C                   if        VABRMA = *blanks
027200071003     C                   movel(P)  VABRMN        VABRMA
027300071003     C                   endif
027400071003     C*
027500021113     C                   write     fivab000                                     => scarico il VAB
027600010305     C*
027700010305     C                   ENDSR
027800990920
027900000801     C*----------------------------------------------------*
028000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028100000801     C*----------------------------------------------------*
028200010201     C     INZVAR        BEGSR
028300000801     C*
028400010201     C                   Z-ADD     *zeros        Num5_0
028500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028600020725     C                   MOVEL     '0'           FlgCAS            1
028700000801     C*
028800000801     C                   ENDSR
028900000801     C*----------------------------------------------------*
029000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029100000801     C*----------------------------------------------------*
029200020904     C     DEFCAM        BEGSR
029300080923     C*
029400080923     C                   SETOFF                                       5051
029500000801     C*
029600021113     C                   CLEAR                   FIVAB000
029700021113     C                   CLEAR                   FIVAT000
029800070730     C* Imposto i valori di default...
029900130404     C                   EVAL      VABCCM = 0891506
030000130404     C                   EVAL      VATCCM = 0891506
030100130404     C                   EVAL      VABLNP = 089
030200130404     C                   EVAL      VATLNP = 089
030300130306     C                   EVAL      VABCTR = 000
030400130405     C                   EVAL      VABCBO = '1'
030500130405     C* in realtà é una soluzione disk C anche se simulata gest. A perché arrivano 2 file separati
030600130405     C                   EVAL      VABCTM = '7Q'
030700070222     C* ... e poi verifico se sono stati passati come parametri
030800070222     C                   IF        vlrppt > *blanks
030900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031100070222     C                   EXSR      CHKNUM
031200070222     C                   IF        PiInt=*on
031300070222     C                   Z-ADD     PiVal         VABCCM
031400070222     C                   Z-ADD     PiVal         VATCCM
031500070222     C                   ENDIF
031600070222     C                   ENDIF
031700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900070222     C                   EXSR      CHKNUM
032000070222     C                   IF        PiInt=*on
032100070222     C                   Z-ADD     PiVal         VABLNP
032200070222     C                   Z-ADD     PiVal         VATLNP
032300070222     C                   ENDIF
032400070222     C                   ENDIF
032500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700070222     C                   EXSR      CHKNUM
032800070222     C                   IF        PiInt=*on
032900070222     C                   Z-ADD     PiVal         VABCTR
033000070222     C                   ENDIF
033100070928     C                   ENDIF
033200071009     C                   MOVEL     *blanks       wDISK             1
033300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033400071009     C                   IF        wDISK <> *blanks
033500070928     C                   SETON                                        50
033600070222     C                   ENDIF
033700080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033800080923     C                   SETON                                        51
033900080923     C                   ENDIF
034000070222     C                   ENDIF
034100071009     C*
034200071009     C   50              EVAL      VABCTM = '7Q'
034300000801     C*
034400000801     C                   ENDSR
034500000801     C*----------------------------------------------------*
034600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034700000801     C*----------------------------------------------------*
034800010201     C     IMPVAB        BEGSR
034900070530     C*
035000070530     C                   SETOFF                                       3132
035100070928     C*
035200070928     C                   EXSR      INZVAR
035300070928     C                   EXSR      DEFCAM
035400070928     C*
035500070928     C                   MOVE(P)   vlrpoi        VABFGS
035600070928     C                   MOVE(P)   vlrpoi        VATFGS
035700130404     C                   MOVEL     datcor        VABAAS
035800130404     C                   MOVE      datcor        VABMGS
035900130404     C                   EVAL      VABRSD=%trim(%subst(vindta:62:35))
036000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036200070928     C* ==
036300130404     C                   EVAL      VABIND=%trim(%subst(vindta:97:35))
036400130404     C                   EVAL      VABLOD=%trim(%subst(vindta:132:35))
036500130404     C                   EVAL      VABRMA=%trim(%subst(vindta:39:15))
036600130404     C                   EVAL      VABPRD=%trim(%subst(vindta:174:2))
036700130404     C                   EVAL      VABGC1=%trim(%subst(vindta:229:2))
036800130404     C                   EVAL      VABGC1=%trim(%subst(vindta:231:2))
036900120717     C*
037000071003     C* CAD
037100130404     C                   EVAL      PiStr=%trim(%subst(vindta:167:7))
037200121001     C                   EXSR      CHKNUM
037300121001     C                   IF        PiInt=*on
037400121001     C                   Z-ADD     PiVal         Num5_0
037500121001     C                   MOVEL     Num5_0        VABCAD
037600121001     C                   ELSE
037700121001     C                   SETON                                        32
037800121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037900121001     C                             + ' ' + 'VABCAD'
038000121001     C                   ENDIF
038100080923     C* RMN
038200130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
038300121001     C                   EXSR      CHKNUM
038400121001     C                   IF        PiInt=*on
038500121001     C                   Z-ADD     PiVal         VABRMN
038600121001     C                   ELSE
038700121001     C                   SETON                                        32
038800121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038900121001     C                             + ' ' + 'VABRMN'
039000121001     C                   ENDIF
039100080923     C* NSP
039200080923     C                   IF        *in51 = *on
039300080923     C* NSP => Stacco un numeratore da AZNUM
039400080923     C                   clear                   TRUL33DS
039500080923     C                   eval      I33OPE = *zeros
039600080923     C                   eval      I33CNU = 302
039700080923     C                   eval      I33NUM = 1
039800080923     C                   movel     TRUL33DS      KPJBU
039900080923     C                   call      'TRUL33R'
040000080923     C                   parm                    KPJBA
040100080923     C                   movel     KPJBU         TRUL33DS
040200080923     C                   if        O33ERR = *zeros
040300080923     C                   z-add     O33NRF        VABNSP
040400080923     C                   z-add     O33NRF        VATNSP
040500080923     C                   else
040600080923     C                   SETON                                        31
040700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800080923     C                             + ' ' + 'VABNSP VATNSP'
040900080923     C                   endif
041000080923     C                   ELSE
041100130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
041200130306     C                   EXSR      CHKNUM
041300130306     C                   IF        PiInt=*on
041400130306     C                   Z-ADD     PiVal         VABNSP
041500130306     C                   Z-ADD     PiVal         VATNSP
041600130306     C                   ELSE
041700130306     C                   SETON                                        31
041800130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041900130306     C                             + ' ' + 'VABNSP VATNSP'
042000130306     C                   ENDIF
042100080923     C                   ENDIF
042200071003     C* NCL
042300130404     C                   EVAL      PiStr=%trim(%subst(vindta:198:5))
042400070928     C                   EXSR      CHKNUM
042500070928     C                   IF        PiInt=*on
042600070928     C                   Z-ADD     PiVal         VABNCL
042700070928     C                   ELSE
042800070928     C                   SETON                                        32
042900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043000070928     C                             + ' ' + 'VABNCL'
043100070928     C                   ENDIF
043200071003     C* PKB
043300130404     C                   EVAL      PiStr=%trim(%subst(vindta:203:7))
043400070928     C                   EXSR      CHKNUM
043500070928     C                   IF        PiNum=*on
043600130306     C* il dato ha 2 decimali
043700130306     C                   EVAL(H)   VABPKB = PiVal/100
043800070928     C                   ELSE
043900070928     C                   SETON                                        32
044000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044100070928     C                             + ' ' + 'VABPKB'
044200070928     C                   ENDIF
044300080923     C* CAS
044400130404     C* l'importo contrassegno ha 3 decimali e NON viene scritto con separatore decimale
044500130404     C                   IF        %trim(%subst(vindta:214:15))<>*blanks
044600080923     C                             AND
044700130404     C                             %trim(%subst(vindta:214:15))<>*zeros
044800121011     C                   EVAL      VABVCA = 'EUR'
044900130405     C                   EVAL      VABTIC = 'BM'
045000070928     C                   MOVEL     '1'           FlgCAS
045100130404     C                   EVAL      PiStr=%trim(%subst(vindta:214:15))
045200070928     C                   EXSR      CHKNUM
045300070928     C                   IF        PiNum=*on
045400130404     C                   EVAL(H)   VABCAS = PiVal/1000
045500070928     C                   ELSE
045600070928     C                   SETON                                        32
045700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045800070928     C                             + ' ' + 'VABCAS'
045900070928     C                   ENDIF
046000070928     C                   ENDIF
046100130405     C* IAS
046200130405     C* non è chiaro cosa c'è nella stringa che passa il cliente, ipotizziamo ci sia un numero
046300130405     C* con decimali sparati da una virgola
046400130510     C* il cliente garantisce che non scriverà mai 0,00 all'interno della stringa, semmai metterà
046500130510     C* blank
046600130506     C                   IF        %trim(%subst(vindta:269:40))<>*blanks AND
046700130506     C                             %trim(%subst(vindta:269:40))<>*zeros
046800130506     C                   EVAL      PiStr=%trim(%subst(vindta:269:40))
046900130506     C                   EXSR      CHKNUM
047000130506     C                   IF        PiNum=*on
047100130506     C***                EVAL      PiVal = PiVal / 100
047200130506     C                   EVAL(H)   VABIAS = PiVal
047300130506     C                   EVAL      VABVAS = 'EUR'
047400130506     C                   ELSE
047500130506     C                   SETON                                        32
047600130506     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700130506     C                             + ' ' + 'VABIAS'
047800130506     C                   ENDIF
047900130506     C                   ENDIF
048000130405     C*
048100130405     C* DCR (arriva in formato GGMMAAAA)
048200130405     C                   IF        %trim(%subst(vindta:4:8))<>*blanks
048300130510     C* costruisco il dato da testare subito in formato AAAAMMGG
048400130510     C                   EVAL      PiStr=%trim(%subst(vindta:8:4) +
048500130510     C                                         %subst(vindta:6:2) +
048600130510     C                                         %subst(vindta:4:2))
048700130405     C                   EXSR      CHKNUM
048800130405     C                   IF        PiInt=*on
048900130510     C                   Z-ADD     PiVal         VABDCR
049000130405     C                   ELSE
049100130510     C                   SETON                                        32
049200130405     C                   Z-ADD     *zeros        VABDCR
049300130405     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049400130405     C                             + ' ' + 'VABDCR'
049500130405     C                   ENDIF
049600130405     C                   ENDIF
049700130405     C*
049800130405     C* se sarà presente un nr.telefonico
049900130405     C                   if        %subst(vindta:233:36) <> *blanks
050000130405     C* telefonare per appuntamento
050100130405     C                   eval      VABTC1 = 'A'
050200130405     C                   endif
050300070730     C*
050400121109     C* Considerazioni finali su CBO/CAS
050500121106     C*
050600121106      * se c/assegno impostato
050700121106     C                   IF        VABCAS > 0
050800121109     C                   IF        VABCBO = '1'
050900121109     C                   EVAL      VABCBO = '4'
051000121109     C                   ENDIF
051100121109     C                   IF        VABCBO = '2'
051200121109     C                   EVAL      VABCBO = '6'
051300121109     C                   ENDIF
051400121106     C                   ENDIF
051500130306     C*
051600130306     C* Se provincia nn valorizzata la reperisco
051700130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
051800130306     C                   IF        VABPRD =  *blanks AND
051900130306     C                             VABNZD =  *blanks AND
052000130306     C                             VABCAD <> *blanks AND
052100130306     C                             VABLOD <> *blanks
052200130306     C                   CLEAR                   TISI95DS
052300130306     C                   EVAL      I95TCN = '3'
052400130306     C                   Z-ADD     datcor        I95DAT
052500130306     C                   EVAL      I95NAR = VABNZD
052600130306     C                   EVAL      I95CAP = VABCAD
052700130306     C                   EVAL      I95LOC = VABLOD
052800130306     C                   CALL      'TISI95R'
052900130306     C                   PARM                    TISI95DS
053000130306     C                   EVAL      VABPRD = O95PRV
053100130306     C                   ENDIF
053200020305     C*
053300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053400011113     C                   EXSR      CHKIMPDIV
053500010202     C*
053600000801     C* Ebbene...
053700000801     C                   ADD       1             §CTRMO
053800070530     C                   IF        *in31 <> *zeros OR
053900070530     C                             *in32 <> *zeros
054000000801     C                   ADD       1             §CTRNO
054100020725     C                   EVAL      x_vinflg = '2'
054200000801     C                   ELSE
054300010201     C                   ADD       1             §CTROKVB
054400000801     C                   ENDIF
054500000801     C*
054600000801     C                   ENDSR
054700010201     C*----------------------------------------------------*
054800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
054900010201     C*----------------------------------------------------*
055000071003     C     WRIVAT_B      BEGSR
055100090204     C*
055200090204     C                   eval      vatAAS = vabAAS
055300090204     C                   eval      vatLNP = vabLNP
055400090204     C                   eval      vatNRS = vabNRS
055500090204     C                   eval      vatNSP = vabNSP
055600010201     C*
055700101007     C* Valorizzo il buffer di scrittura del FIVAT
055800070928     C* - TIPO RECORD "B"
055900130405     C                   if        %subst(vindta:233:36) <> *blanks
056000070928     C                   eval      VATTRC = 'B'
056100130405     C                   eval      VATNOT = %trim(%subst(vindta:233:36))
056200021113     C                   write     FIVAT000
056300020725     C                   endif
056400010201     C*
056500010201     C                   ENDSR
056600071003     C*----------------------------------------------------*
056700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
056800071003     C*----------------------------------------------------*
056900071003     C     WRIVAT_E      BEGSR
057000090204     C*
057100090204     C                   eval      vatAAS = vabAAS
057200090204     C                   eval      vatLNP = vabLNP
057300090204     C                   eval      vatNRS = vabNRS
057400090204     C                   eval      vatNSP = vabNSP
057500071003     C*
057600101007     C* Valorizzo il buffer di scrittura del FIVAT
057700071003     C* - TIPO RECORD "E"
057800130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
057900130306     C                   eval      VATTRC = 'E'
058000121109     C*
058100130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
058200071003     C                   write     FIVAT000
058300121109     C*
058400130306     C***                endif
058500071003     C*
058600071003     C                   ENDSR
058700020904
058800020904
058900010202     C*----------------------------------------------------*
059000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
059100010202     C*----------------------------------------------------*
059200020305     C     PREVAT        BEGSR
059300010202     C*
059400021113     C* Compongo il nome del membro da dare al FIVATWWR
059500010202     C                   eval      parmbr = vlrhdl
059600010202     C                   movel     'M'           parmbr
059700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
059800010202     C                   eval      paropz = '1'
059900010202     C* Effettuo la chiamata al CLLE preposto
060000050128     C                   call(e)   'TITVVTC'
060100010202     C                   parm                    parccm
060200010202     C                   parm                    parmbr
060300010202     C                   parm                    paropz
060400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060500010202     C                   if        %error
060600010202     C                   movel     '1'           chkcall
060700010202     C                   else
060800010202     C                   movel     '0'           chkcall
060900010202     C                   endif
061000010202     C*
061100010202     C                   ENDSR
061200000801     C*----------------------------------------------------*
061300000801     C*  CONTROLLO NUMERICITA' CAMPI
061400000801     C*----------------------------------------------------*
061500000801     C     CHKNUM        BEGSR
061600081113     C*
061700081113     C                   IF        PiDecChr = *blanks
061800130508     C                   EVAL      PiDecChr = ','
061900081113     C                   ENDIF
062000081113     C*
062100081113     C                   callp(e)  UBISNUM_Check(PiStr
062200081113     C                                          :PiDecChr
062300081113     C                                          :PiVal
062400081113     C                                          :PiNum
062500081113     C                                          :PiInt)
062600081113     C*
062700000801     C                   IF        %error
062800000801     C                   EVAL      PiInt=*off
062900000801     C                   ENDIF
063000000801     C*
063100000801     C                   ENDSR
063200000801     C***
063300000801
063400011113
063500011113     C*----------------------------------------------------*
063600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063700011113     C*----------------------------------------------------*
063800011113     C     CHKIMPDIV     BEGSR
063900011113     C*
064000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
064100011113     C                   Z-ADD     *zeros        wrkDec            9 9
064200011113     C*
064300011113     C* Come prima cosa effettuo considerazioni sulla divisa
064400011113     C                   IF        vabIAS > *zeros
064500011113     C                   IF        vabVAS <> 'EUR'
064600011113     C                   EVAL      vabVAS =  'ITL'
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C                   IF        vabCAS > *zeros
065100011113     C                   IF        vabVCA <> 'EUR'
065200011113     C                   EVAL      vabVCA =  'ITL'
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C                   IF        vabVMD > *zeros
065700020305     C                   IF        vabVAD <> 'EUR'
065800011113     C                   EVAL      vabVAD =  'ITL'
065900011113     C                   ENDIF
066000011113     C                   ENDIF
066100011113     C*
066200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066300011113     C                   Z-ADD     vabIAS        wrkDec
066400011113     C                   IF        wrkDec > *zeros
066500011113     C                   IF        vabVAS = 'ITL'
066600011113     C                   EVAL      vabIAS = *zeros
066700011113     C                   ENDIF
066800011113     C                   ENDIF
066900011113     C*
067000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
067100011113     C                   Z-ADD     vabCAS        wrkDec
067200011113     C                   IF        wrkDec > *zeros
067300011113     C                   IF        vabVCA = 'ITL'
067400011113     C                   EVAL      vabCAS = *zeros
067500011113     C                   ENDIF
067600011113     C                   ENDIF
067700011113     C*
067800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067900011113     C                   Z-ADD     vabVMD        wrkDec
068000011113     C                   IF        wrkDec > *zeros
068100011113     C                   IF        vabVAD = 'ITL'
068200011113     C                   EVAL      vabVMD = *zeros
068300011113     C                   ENDIF
068400011113     C                   ENDIF
068500011113     C*
068600011113     C                   ENDSR
068700011113     C***
068800011113
068900011113
069000000801
069100000801
069200990920      /TITLE Invio dei dati al punto operativo.
069300010202     C     invio         BEGSR
069400990920     C*
069500021113     C* 1° invio FIVAT
069600010201     C                   reset                   dscmz
069700010201     C                   move      vlrpoi        cmzdst
069800021113     C                   eval      cmzfld = 'FIVATWWR'
069900010201     C                   eval      cmzmbd = vlrhdl
070000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070100021009     C***                if        prmfir = *blanks
070200021113     C                   eval      cmzfla = 'FIVAT00F'
070300021113     C                   eval      cmzmba = 'FIVAT00F'
070400021009     C***                else
070500021009     C***                eval      cmzfla = prmfir
070600021009     C***                eval      cmzmba = prmfir
070700021009     C***                endif
070800010201     C                   eval      cmznrr = *zeros
070900020305     C                   move      §ctrokvt      cmznrr
071000021018     C                   eval      cmzlba = vlrfl1
071100010201     C                   call(e)   'TIS711C'
071200010201     C                   parm                    dscmz
071300010201     C                   parm      *blanks       esito
071400010205     C                   if        %error
071500010205     C                             or cmzerr = '1'
071600010205     C                             or esito  = '1'
071700010205     C                   eval      wrkesito = '3'
071800010205     C                   else
071900010201     C*
072000021113     C* 2° invio FIVAB
072100010201     C                   reset                   dscmz
072200010201     C                   move      vlrpoi        cmzdst
072300010201     C                   eval      cmzfld = vlrfou
072400010201     C                   eval      cmzmbd = vlrhdl
072500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072600021009     C***                if        prmfir = *blanks
072700021113     C                   eval      cmzfla = 'FIVAB00F'
072800021113     C                   eval      cmzmba = 'FIVAB00F'
072900021009     C***                else
073000021009     C***                eval      cmzfla = prmfir
073100021009     C***                eval      cmzmba = prmfir
073200021009     C***                endif
073300010201     C                   eval      cmznrr = *zeros
073400010201     C                   move      §ctrokvb      cmznrr
073500021018     C                   eval      cmzlba = vlrfl1
073600010201     C                   call(e)   'TIS711C'
073700010201     C                   parm                    dscmz
073800010201     C                   parm      *blanks       esito
073900010201     C                   if        %error
074000010201     C                             or cmzerr = '1'
074100010201     C                             or esito  = '1'
074200010201     C                   eval      wrkesito = '3'
074300010201     C                   endif
074400010205     C                   endif
074500990920     C*
074600000613     C                   ENDSR
074700000613     C***
074800070411
074900070411     C     *pssr         BEGSR
075000070411     C*
075100070411     C                   if        %open(tivin00r)
075200070411     C                   close     tivin00r
075300070411     C                   endif
075400070411     C                   if        %open(fivabwwr)
075500070411     C                   close     fivabwwr
075600070411     C                   endif
075700070411     C                   if        %open(fivatwwr)
075800070411     C                   close     fivatwwr
075900070411     C                   endif
076000070411     C*
076100070411     C* Effettuo la chiamata al CLLE preposto
076200070411     C                   call(e)   'TITVVTC'
076300070411     C                   parm                    parccm
076400070411     C                   parm                    parmbr
076500070411     C                   parm      '2'           paropz
076600070411     C*
076700070411     C                   eval      wrkesito = '2'
076800070411     C*
076900070411     C                   seton                                        LR
077000070411     C*
077100070411     C                   ENDSR     '*CANCL'
077200070411     C***
077300070411
077400990910
077500000613     C     *inzsr        BEGSR
077600990910     C*
077700990910     C     *entry        plist
077800990920     C                   parm                    tivlrds
077900990921     C                   parm      wrkesito      esito
078000000724     C                   parm                    prmlit
078100000710     C                   parm                    prmfir
078200000613     C*
078300000830     C* CALCOLA LA DATA CORRENTE
078400100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078500100324     C                   eval      datcor = %dec(%date() : *ISO)
078600000830     C*
078700000613     C                   ENDSR
078800000613     C***
