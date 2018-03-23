000100130510      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130405     C* se sarà presente un nr.telefonico, indicare la consegna per appuntamento
000500130405     C* se c'è il contrassegno, il tipo incasso è BM (ASSEGNO BANCARIO INTESTATO AL MITTENTE)
000600130306      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000130510     FEDIVABwr  O    E             DISK    usropn
001100130510     FEDIVATwr  O    E             DISK    usropn
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
003500130510     D wNomeFile       s             30    INZ(*blanks)
003600000830
003700020725
003800020725     D*------------------
003900020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004000020725     D*------------------
004100070530     D INPUT_DS        DS                  INZ
004200101007     D  VINDTA                     2048
004300101007     D  VINFLG                        1
004400130405     D  VINSPE                       15
004500101007     D  VINRRN                        8  0
004600020725     D*
004700080923     D*------------------
004800080923     D* DS REPERIMENTO NUMERATORE
004900080923     D*------------------
005000080923     D trul33ds      e ds                  inz
005100080923     D*------------------
005200080923     D* DS ARCHITETTURA
005300080923     D*------------------
005400080923     D kpjba         e ds                  inz
005500080923     D*------------------
005600121126     D BarcodeDS       DS           770    INZ
005700121126     D  wBarcode                     14    dim(55)
005800121109     D*------------------
005900121109     D TRUL28DSX     e ds                  inz
006000121109     D*------------------
006100130405     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200130405     D*------------------
006300130405     D WLBDA8          DS                  INZ
006400130405     D  G08DAT                 1      8  0
006500130405     D  G08INV                 9     16  0
006600130405     D  G08ERR                17     17
006700130405     D  G08TGI                18     22  0
006800130405     D*
006900121109     D wX              s              3s 0
007000081113
007100081113     D*------------------
007200081113     D* LINKING A DEFINIZIONI ESTERNE
007300081113     D*------------------
007400100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600081113
007700990908
007800010201
007900010201
008000080117     C*
008100080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008200080117     C
008300080117     C/EXEC SQL
008400080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008500080117     C/END-EXEC
008600080117     C
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000000613     C*
009100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009200000613     C*
009300010202     C* Effettuo la chiamata al CLLE preposto
009400130510     C                   call(e)   'TITVEVTC'
009500010202     C                   parm                    parccm
009600010202     C                   parm                    parmbr
009700010202     C                   parm      '2'           paropz
009800070730     C*
009900070730     C* Effettuo lancio TISI95 solo x chiusura
010000070730     C                   CLEAR                   TISI95DS
010100070730     C                   EVAL      I95TLA = 'C'
010200070730     C                   CALL      'TISI95R'
010300070730     C                   PARM                    TISI95DS
010400000616     C*
010500000801     C
010600010201     C                   seton                                        LR
010700000801
010800910830     C*--------------------------------------------------------
010900130510     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011000910830     C*--------------------------------------------------------
011100070530     C     RWFILE        BEGSR
011200990910     C*
011300990914     C                   if        not %open(tivin00r)
011400990908     C                   open      tivin00r
011500990914     C                   endif
011600130510     C                   if        not %open(edivabwr)
011700130510     C                   open      edivabwr
011800990914     C                   endif
011900070530     C*
012000130510     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012100020305     C                   exsr      prevat
012200010201     C*
012300010202     C                   if        chkcall = '0'
012400010202     C*
012500130510     C                   if        not %open(edivatwr)
012600130510     C                   open      edivatwr
012700010201     C                   endif
012800080117     C*
012900080117     C                   EXSR      INZVAR
013000080117     C                   EXSR      DEFCAM
013100990910     C*
013200010201     C                   clear                   §CTROKVB          5 0
013300020305     C                   clear                   §CTROKVT          5 0
013400000801     C                   clear                   §CTRMO            5 0
013500000801     C                   clear                   §CTRNO            5 0
013600990910     C*
013700020725     C/EXEC SQL
013800020725     C+ declare C1 cursor for select
013900130405     C+ vindta, vinflg, substr(vindta, 24, 15) as sped, rrn(tivin00r)
014000060223     C+ from tivin00r
014100060223     C+ order by sped
014200020725     C+ for read only
014300020725     C/END-EXEC
014400020725     C
014500020725     C/EXEC SQL
014600020725     C+ open C1
014700020725     C/END-EXEC
014800020725     C
014900020725     C/EXEC SQL
015000070530     C+ Fetch C1 into :INPUT_DS
015100020725     C/END-EXEC
015200020725     C*
015300020725     C                   dow       sqlcod = *zeros
015400990913     C*
015500020725     C                   if        vindta > *blanks
015600000613     C                   add       1             rrnum
015700000801     C*
015800020725     C                   if        vinflg = *blanks
015900020725     C                             or vinflg = '0'
016000020725     C                             or vinflg = '2'
016100000801     C*
016200020725     C                   clear                   x_vinmsg
016300020725     C                   eval      x_vinflg = '1'
016400010305     C*
016500121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016600121109     C* (uso l'RMN)
016700130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
016800020305     C                   MOVEL(p)  PiStr         curspe
016900010305     C*
017000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017100080923     C                   if        *in50 = *off
017200071003     C                   exsr      impvab
017300071003     C                   exsr      wrivab
017400130306     C                   exsr      wrivat_b                                     => carico VAT
017500071003     C                   else
017600080923     C*
017700071009     C                   if        wDISK = 'D'
017800071009     C                   exsr      impvab
017900071009     C                   exsr      wrivab
018000130306     C                   exsr      wrivat_b                                     => carico VAT
018100130306     C***                exsr      wrivat_e                                     => carico VAT
018200071009     C                   else
018300071009     C*
018400010305     C                   if        depspe = *blanks                             => 1° giro
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600080117     C                   clear                   tivinds
018700020305     C                   exsr      impvab
018800130306     C                   exsr      wrivat_b                                     => carico VAT
018900130306     C***50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   else
019100020725     C                   if        depspe <> curspe                             => rottura di spediz
019200010305     C                   eval      depspe = curspe                              => memorizz. spediz
019300070928     C                   exsr      wrivab
019400080117     C                   clear                   tivinds
019500020305     C                   exsr      impvab
019600130306     C                   exsr      wrivat_b                                     => carico VAT
019700130306     C***50              exsr      wrivat_e                                     => carico VAT
019800020305     C                   else                                                   => x stessa spediz
019900090204     C***                exsr      impvab
020000130306     C                   exsr      wrivat_b                                     => carico VAT
020100130306     C***50              exsr      wrivat_e                                     => carico VAT
020200010305     C                   endif
020300010305     C                   endif
020400010305     C                   endif
020500071003     C                   endif
020600071009     C                   endif
020700000905     C*
020800000905     C                   else
020900020725     C                   eval      x_vinflg = '1'
021000000905     C                   endif
021100000905     C*
021200020725     C     VINRRN        chain     tivin000
021300020725     C                   if        %found(tivin00r)
021400020725     C                   eval      y_vinflg = x_vinflg
021500020725     C                   eval      y_vinmsg = x_vinmsg
021600020725     C                   update    tivin000
021700020725     C                   endif
021800020725     C*
021900020725     C/EXEC SQL
022000070530     C+ Fetch C1 into :INPUT_DS
022100020725     C/END-EXEC
022200020725     C*
022300020725     C                   enddo
022400020725     C*
022500020725     C/EXEC SQL
022600020725     C+ close C1
022700020725     C/END-EXEC
022800000905     C*
022900020305     C* Scarico i VAB rimasti "in sospeso"
023000071009     C                   if        wDISK = 'C'
023100071009     C                   exsr      wrivab
023200071009     C                   endif
023300010202     C*
023400010202     C                   endif
023500990910
023600990910     C* Se non ci sono record con errori ...
023700000710     C                   if        §ctrno = 0
023800990910     C* ... restituisco esito OK.
023900990921     C                   eval      wrkesito = '0'
024000990910     C                   else
024100010201     C                   if        §ctrokvb > 0
024200990921     C                   eval      wrkesito = '1'
024300000710     C                   else
024400000710     C                   eval      wrkesito = '2'
024500990910     C                   endif
024600000710     C                   endif
024700990910     C*
024800990914     C                   if        %open(tivin00r)
024900990908     C                   close     tivin00r
025000990914     C                   endif
025100130510     C                   if        %open(edivabwr)
025200130510     C                   close     edivabwr
025300990914     C                   endif
025400130510     C                   if        %open(edivatwr)
025500130510     C                   close     edivatwr
025600010201     C                   endif
025700990910     C*
025800010201     C                   if        §ctrokvb > 0
025900000724     C                             and vlrpoi <> *zeros
026000010202     C                   exsr      invio
026100990920     C                   endif
026200990920     C*
026300910830     C                   ENDSR
026400000613     C***
026500010305
026600010305     C*----------------------------------------------------*
026700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026800010305     C*----------------------------------------------------*
026900020305     C     WRIVAB        BEGSR
027000130510     C*
027100130510     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
027200130510     C                   EVAL      VABCMR = 'SAFETY ' +
027300130510     C                                      %char(datcor)
027400130510     C                   EVAL      VABDCM = DATCOR
027500130510     C                   EVAL      VABDTS = DATCOR
027600130510     C                   EVAL      VABHMS = ORACOR
027700130510     C                   EVAL      VABCNT = 1
027800070730     C*
027900071003     C* Considerazioni finali
028000071003     C                   if        VABRMA = *blanks
028100071003     C                   movel(P)  VABRMN        VABRMA
028200071003     C                   endif
028300071003     C*
028400130510     C                   write     edivab00                                     => scarico il VAB
028500010305     C*
028600010305     C                   ENDSR
028700990920
028800000801     C*----------------------------------------------------*
028900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029000000801     C*----------------------------------------------------*
029100010201     C     INZVAR        BEGSR
029200000801     C*
029300010201     C                   Z-ADD     *zeros        Num5_0
029400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029500020725     C                   MOVEL     '0'           FlgCAS            1
029600000801     C*
029700000801     C                   ENDSR
029800000801     C*----------------------------------------------------*
029900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030000000801     C*----------------------------------------------------*
030100020904     C     DEFCAM        BEGSR
030200080923     C*
030300080923     C                   SETOFF                                       5051
030400000801     C*
030500130510     C                   CLEAR                   EDIVAB00
030600130510     C                   CLEAR                   EDIVAT00
030700070730     C* Imposto i valori di default...
030800130404     C                   EVAL      VABCCM = 0891506
030900130404     C                   EVAL      VATCCM = 0891506
031000130404     C                   EVAL      VABLNP = 089
031100130404     C                   EVAL      VATLNP = 089
031200130306     C                   EVAL      VABCTR = 000
031300130405     C                   EVAL      VABCBO = '1'
031400130405     C* in realtà é una soluzione disk C anche se simulata gest. A perché arrivano 2 file separati
031500130405     C                   EVAL      VABCTM = '7Q'
031600070222     C* ... e poi verifico se sono stati passati come parametri
031700070222     C                   IF        vlrppt > *blanks
031800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000070222     C                   EXSR      CHKNUM
032100070222     C                   IF        PiInt=*on
032200070222     C                   Z-ADD     PiVal         VABCCM
032300070222     C                   Z-ADD     PiVal         VATCCM
032400070222     C                   ENDIF
032500070222     C                   ENDIF
032600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800070222     C                   EXSR      CHKNUM
032900070222     C                   IF        PiInt=*on
033000070222     C                   Z-ADD     PiVal         VABLNP
033100070222     C                   Z-ADD     PiVal         VATLNP
033200070222     C                   ENDIF
033300070222     C                   ENDIF
033400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
033500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033600070222     C                   EXSR      CHKNUM
033700070222     C                   IF        PiInt=*on
033800070222     C                   Z-ADD     PiVal         VABCTR
033900070222     C                   ENDIF
034000070928     C                   ENDIF
034100071009     C                   MOVEL     *blanks       wDISK             1
034200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
034300071009     C                   IF        wDISK <> *blanks
034400070928     C                   SETON                                        50
034500070222     C                   ENDIF
034600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
034700080923     C                   SETON                                        51
034800080923     C                   ENDIF
034900070222     C                   ENDIF
035000071009     C*
035100071009     C   50              EVAL      VABCTM = '7Q'
035200000801     C*
035300000801     C                   ENDSR
035400000801     C*----------------------------------------------------*
035500130510     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
035600000801     C*----------------------------------------------------*
035700010201     C     IMPVAB        BEGSR
035800070530     C*
035900070530     C                   SETOFF                                       3132
036000070928     C*
036100070928     C                   EXSR      INZVAR
036200070928     C                   EXSR      DEFCAM
036300070928     C*
036400070928     C                   MOVE(P)   vlrpoi        VABFGS
036500070928     C                   MOVE(P)   vlrpoi        VATFGS
036600130404     C                   MOVEL     datcor        VABAAS
036700130404     C                   MOVE      datcor        VABMGS
036800130404     C                   EVAL      VABRSD=%trim(%subst(vindta:62:35))
036900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
037100070928     C* ==
037200130404     C                   EVAL      VABIND=%trim(%subst(vindta:97:35))
037300130404     C                   EVAL      VABLOD=%trim(%subst(vindta:132:35))
037400130404     C                   EVAL      VABRMA=%trim(%subst(vindta:39:15))
037500130404     C                   EVAL      VABPRD=%trim(%subst(vindta:174:2))
037600130404     C                   EVAL      VABGC1=%trim(%subst(vindta:229:2))
037700130404     C                   EVAL      VABGC1=%trim(%subst(vindta:231:2))
037800130513     C                   EVAL      w70=%trim(%subst(vindta:315:50))
037900130513     C                   EVAL      VABNOT=%subst(w70:01:35)
038000130513     C                   EVAL      VABNT2=%subst(w70:36:15)
038100120717     C*
038200071003     C* CAD
038300130404     C                   EVAL      PiStr=%trim(%subst(vindta:167:7))
038400121001     C                   EXSR      CHKNUM
038500121001     C                   IF        PiInt=*on
038600121001     C                   Z-ADD     PiVal         Num5_0
038700121001     C                   MOVEL     Num5_0        VABCAD
038800121001     C                   ELSE
038900121001     C                   SETON                                        32
039000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039100121001     C                             + ' ' + 'VABCAD'
039200121001     C                   ENDIF
039300080923     C* RMN
039400130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
039500121001     C                   EXSR      CHKNUM
039600121001     C                   IF        PiInt=*on
039700121001     C                   Z-ADD     PiVal         VABRMN
039800121001     C                   ELSE
039900121001     C                   SETON                                        32
040000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100121001     C                             + ' ' + 'VABRMN'
040200121001     C                   ENDIF
040300080923     C* NSP
040400080923     C                   IF        *in51 = *on
040500080923     C* NSP => Stacco un numeratore da AZNUM
040600080923     C                   clear                   TRUL33DS
040700080923     C                   eval      I33OPE = *zeros
040800080923     C                   eval      I33CNU = 302
040900080923     C                   eval      I33NUM = 1
041000080923     C                   movel     TRUL33DS      KPJBU
041100080923     C                   call      'TRUL33R'
041200080923     C                   parm                    KPJBA
041300080923     C                   movel     KPJBU         TRUL33DS
041400080923     C                   if        O33ERR = *zeros
041500080923     C                   z-add     O33NRF        VABNSP
041600080923     C                   z-add     O33NRF        VATNSP
041700080923     C                   else
041800080923     C                   SETON                                        31
041900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042000080923     C                             + ' ' + 'VABNSP VATNSP'
042100080923     C                   endif
042200080923     C                   ELSE
042300130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
042400130306     C                   EXSR      CHKNUM
042500130306     C                   IF        PiInt=*on
042600130306     C                   Z-ADD     PiVal         VABNSP
042700130306     C                   Z-ADD     PiVal         VATNSP
042800130306     C                   ELSE
042900130306     C                   SETON                                        31
043000130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100130306     C                             + ' ' + 'VABNSP VATNSP'
043200130306     C                   ENDIF
043300080923     C                   ENDIF
043400071003     C* NCL
043500130404     C                   EVAL      PiStr=%trim(%subst(vindta:198:5))
043600070928     C                   EXSR      CHKNUM
043700070928     C                   IF        PiInt=*on
043800070928     C                   Z-ADD     PiVal         VABNCL
043900070928     C                   ELSE
044000070928     C                   SETON                                        32
044100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200070928     C                             + ' ' + 'VABNCL'
044300070928     C                   ENDIF
044400071003     C* PKB
044500130404     C                   EVAL      PiStr=%trim(%subst(vindta:203:7))
044600070928     C                   EXSR      CHKNUM
044700070928     C                   IF        PiNum=*on
044800130306     C* il dato ha 2 decimali
044900130306     C                   EVAL(H)   VABPKB = PiVal/100
045000070928     C                   ELSE
045100070928     C                   SETON                                        32
045200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045300070928     C                             + ' ' + 'VABPKB'
045400070928     C                   ENDIF
045500080923     C* CAS
045600130404     C* l'importo contrassegno ha 3 decimali e NON viene scritto con separatore decimale
045700130404     C                   IF        %trim(%subst(vindta:214:15))<>*blanks
045800080923     C                             AND
045900130404     C                             %trim(%subst(vindta:214:15))<>*zeros
046000121011     C                   EVAL      VABVCA = 'EUR'
046100070928     C                   MOVEL     '1'           FlgCAS
046200130404     C                   EVAL      PiStr=%trim(%subst(vindta:214:15))
046300070928     C                   EXSR      CHKNUM
046400070928     C                   IF        PiNum=*on
046500130404     C                   EVAL(H)   VABCAS = PiVal/1000
046600130917     C                   SELECT
046700131202     C                   WHEN      VABCAS > 500
046800130917     C                   EVAL      VABTIC = 'BM'
046900130917     C                   OTHER
047000130917     C                   EVAL      VABTIC = *blank
047100130917     C                   ENDSL
047200070928     C                   ELSE
047300070928     C                   SETON                                        32
047400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500070928     C                             + ' ' + 'VABCAS'
047600070928     C                   ENDIF
047700070928     C                   ENDIF
047800130405     C* IAS
047900130405     C* non è chiaro cosa c'è nella stringa che passa il cliente, ipotizziamo ci sia un numero
048000130405     C* con decimali sparati da una virgola
048100130510     C* il cliente garantisce che non scriverà mai 0,00 all'interno della stringa, semmai metterà
048200130510     C* blank
048300130506     C                   IF        %trim(%subst(vindta:269:40))<>*blanks AND
048400130506     C                             %trim(%subst(vindta:269:40))<>*zeros
048500130506     C                   EVAL      PiStr=%trim(%subst(vindta:269:40))
048600130506     C                   EXSR      CHKNUM
048700130506     C                   IF        PiNum=*on
048800130506     C***                EVAL      PiVal = PiVal / 100
048900130506     C                   EVAL(H)   VABIAS = PiVal
049000130506     C                   EVAL      VABVAS = 'EUR'
049100130506     C                   ELSE
049200130506     C                   SETON                                        32
049300130506     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049400130506     C                             + ' ' + 'VABIAS'
049500130506     C                   ENDIF
049600130506     C                   ENDIF
049700130405     C*
049800130405     C* DCR (arriva in formato GGMMAAAA)
049900130405     C                   IF        %trim(%subst(vindta:4:8))<>*blanks
050000130510     C* costruisco il dato da testare subito in formato AAAAMMGG
050100130510     C                   EVAL      PiStr=%trim(%subst(vindta:8:4) +
050200130510     C                                         %subst(vindta:6:2) +
050300130510     C                                         %subst(vindta:4:2))
050400130405     C                   EXSR      CHKNUM
050500130405     C                   IF        PiInt=*on
050600130510     C                   Z-ADD     PiVal         VABDCR
050700130405     C                   ELSE
050800130510     C                   SETON                                        32
050900130405     C                   Z-ADD     *zeros        VABDCR
051000130405     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051100130405     C                             + ' ' + 'VABDCR'
051200130405     C                   ENDIF
051300130405     C                   ENDIF
051400130405     C*
051500130405     C* se sarà presente un nr.telefonico
051600130405     C                   if        %subst(vindta:233:36) <> *blanks
051700130405     C* telefonare per appuntamento
051800130405     C                   eval      VABTC1 = 'A'
051900130405     C                   endif
052000070730     C*
052100121109     C* Considerazioni finali su CBO/CAS
052200121106     C*
052300121106      * se c/assegno impostato
052400121106     C                   IF        VABCAS > 0
052500121109     C                   IF        VABCBO = '1'
052600121109     C                   EVAL      VABCBO = '4'
052700121109     C                   ENDIF
052800121109     C                   IF        VABCBO = '2'
052900121109     C                   EVAL      VABCBO = '6'
053000121109     C                   ENDIF
053100121106     C                   ENDIF
053200130306     C*
053300130306     C* Se provincia nn valorizzata la reperisco
053400130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
053500130306     C                   IF        VABPRD =  *blanks AND
053600130306     C                             VABNZD =  *blanks AND
053700130306     C                             VABCAD <> *blanks AND
053800130306     C                             VABLOD <> *blanks
053900130306     C                   CLEAR                   TISI95DS
054000130306     C                   EVAL      I95TCN = '3'
054100130306     C                   Z-ADD     datcor        I95DAT
054200130306     C                   EVAL      I95NAR = VABNZD
054300130306     C                   EVAL      I95CAP = VABCAD
054400130306     C                   EVAL      I95LOC = VABLOD
054500130306     C                   CALL      'TISI95R'
054600130306     C                   PARM                    TISI95DS
054700130306     C                   EVAL      VABPRD = O95PRV
054800130306     C                   ENDIF
054900020305     C*
055000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055100011113     C                   EXSR      CHKIMPDIV
055200010202     C*
055300000801     C* Ebbene...
055400000801     C                   ADD       1             §CTRMO
055500070530     C                   IF        *in31 <> *zeros OR
055600070530     C                             *in32 <> *zeros
055700000801     C                   ADD       1             §CTRNO
055800020725     C                   EVAL      x_vinflg = '2'
055900000801     C                   ELSE
056000010201     C                   ADD       1             §CTROKVB
056100000801     C                   ENDIF
056200000801     C*
056300000801     C                   ENDSR
056400010201     C*----------------------------------------------------*
056500130510     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
056600010201     C*----------------------------------------------------*
056700071003     C     WRIVAT_B      BEGSR
056800090204     C*
056900090204     C                   eval      vatAAS = vabAAS
057000090204     C                   eval      vatLNP = vabLNP
057100090204     C                   eval      vatNRS = vabNRS
057200090204     C                   eval      vatNSP = vabNSP
057300010201     C*
057400130510     C* Valorizzo il buffer di scrittura del EDIVAT
057500070928     C* - TIPO RECORD "B"
057600130405     C                   if        %subst(vindta:233:36) <> *blanks
057700070928     C                   eval      VATTRC = 'B'
057800130405     C                   eval      VATNOT = %trim(%subst(vindta:233:36))
057900130510     C                   WRITE     EDIVAT00
058000020725     C                   endif
058100010201     C*
058200010201     C                   ENDSR
058300071003     C*----------------------------------------------------*
058400130510     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
058500071003     C*----------------------------------------------------*
058600071003     C     WRIVAT_E      BEGSR
058700090204     C*
058800090204     C                   eval      vatAAS = vabAAS
058900090204     C                   eval      vatLNP = vabLNP
059000090204     C                   eval      vatNRS = vabNRS
059100090204     C                   eval      vatNSP = vabNSP
059200071003     C*
059300130510     C* Valorizzo il buffer di scrittura del EDIVAT
059400071003     C* - TIPO RECORD "E"
059500130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
059600130306     C                   eval      VATTRC = 'E'
059700121109     C*
059800130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
059900130510     C                   WRITE     EDIVAT00
060000121109     C*
060100130306     C***                endif
060200071003     C*
060300071003     C                   ENDSR
060400020904
060500020904
060600010202     C*----------------------------------------------------*
060700130510     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
060800010202     C*----------------------------------------------------*
060900020305     C     PREVAT        BEGSR
061000010202     C*
061100130510     C* Compongo il nome del membro da dare al EDIVATWR
061200010202     C                   eval      parmbr = vlrhdl
061300010202     C                   movel     'M'           parmbr
061400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061500010202     C                   eval      paropz = '1'
061600010202     C* Effettuo la chiamata al CLLE preposto
061700130510     C                   call(e)   'TITVEVTC'
061800010202     C                   parm                    parccm
061900010202     C                   parm                    parmbr
062000010202     C                   parm                    paropz
062100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062200010202     C                   if        %error
062300010202     C                   movel     '1'           chkcall
062400010202     C                   else
062500010202     C                   movel     '0'           chkcall
062600010202     C                   endif
062700010202     C*
062800010202     C                   ENDSR
062900000801     C*----------------------------------------------------*
063000000801     C*  CONTROLLO NUMERICITA' CAMPI
063100000801     C*----------------------------------------------------*
063200000801     C     CHKNUM        BEGSR
063300081113     C*
063400081113     C                   IF        PiDecChr = *blanks
063500130508     C                   EVAL      PiDecChr = ','
063600081113     C                   ENDIF
063700081113     C*
063800081113     C                   callp(e)  UBISNUM_Check(PiStr
063900081113     C                                          :PiDecChr
064000081113     C                                          :PiVal
064100081113     C                                          :PiNum
064200081113     C                                          :PiInt)
064300081113     C*
064400000801     C                   IF        %error
064500000801     C                   EVAL      PiInt=*off
064600000801     C                   ENDIF
064700000801     C*
064800000801     C                   ENDSR
064900000801     C***
065000000801
065100011113
065200011113     C*----------------------------------------------------*
065300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065400011113     C*----------------------------------------------------*
065500011113     C     CHKIMPDIV     BEGSR
065600011113     C*
065700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065800011113     C                   Z-ADD     *zeros        wrkDec            9 9
065900011113     C*
066000011113     C* Come prima cosa effettuo considerazioni sulla divisa
066100011113     C                   IF        vabIAS > *zeros
066200011113     C                   IF        vabVAS <> 'EUR'
066300011113     C                   EVAL      vabVAS =  'ITL'
066400011113     C                   ENDIF
066500011113     C                   ENDIF
066600011113     C*
066700011113     C                   IF        vabCAS > *zeros
066800011113     C                   IF        vabVCA <> 'EUR'
066900011113     C                   EVAL      vabVCA =  'ITL'
067000011113     C                   ENDIF
067100011113     C                   ENDIF
067200011113     C*
067300011113     C                   IF        vabVMD > *zeros
067400020305     C                   IF        vabVAD <> 'EUR'
067500011113     C                   EVAL      vabVAD =  'ITL'
067600011113     C                   ENDIF
067700011113     C                   ENDIF
067800011113     C*
067900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068000011113     C                   Z-ADD     vabIAS        wrkDec
068100011113     C                   IF        wrkDec > *zeros
068200011113     C                   IF        vabVAS = 'ITL'
068300011113     C                   EVAL      vabIAS = *zeros
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068800011113     C                   Z-ADD     vabCAS        wrkDec
068900011113     C                   IF        wrkDec > *zeros
069000011113     C                   IF        vabVCA = 'ITL'
069100011113     C                   EVAL      vabCAS = *zeros
069200011113     C                   ENDIF
069300011113     C                   ENDIF
069400011113     C*
069500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069600011113     C                   Z-ADD     vabVMD        wrkDec
069700011113     C                   IF        wrkDec > *zeros
069800011113     C                   IF        vabVAD = 'ITL'
069900011113     C                   EVAL      vabVMD = *zeros
070000011113     C                   ENDIF
070100011113     C                   ENDIF
070200011113     C*
070300011113     C                   ENDSR
070400011113     C***
070500011113
070600011113
070700000801
070800000801
070900990920      /TITLE Invio dei dati al punto operativo.
071000010202     C     invio         BEGSR
071100990920     C*
071200130510     C* 1° invio EDIVAT
071300010201     C                   reset                   dscmz
071400010201     C                   move      vlrpoi        cmzdst
071500130510     C                   eval      cmzfld = 'EDIVATWR'
071600010201     C                   eval      cmzmbd = vlrhdl
071700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071800021009     C***                if        prmfir = *blanks
071900130510     C                   eval      cmzfla = 'EDIVAT0F'
072000130510     C                   eval      cmzmba = 'EDIVAT0F'
072100021009     C***                else
072200021009     C***                eval      cmzfla = prmfir
072300021009     C***                eval      cmzmba = prmfir
072400021009     C***                endif
072500010201     C                   eval      cmznrr = *zeros
072600020305     C                   move      §ctrokvt      cmznrr
072700021018     C                   eval      cmzlba = vlrfl1
072800010201     C                   call(e)   'TIS711C'
072900010201     C                   parm                    dscmz
073000010201     C                   parm      *blanks       esito
073100010205     C                   if        %error
073200010205     C                             or cmzerr = '1'
073300010205     C                             or esito  = '1'
073400010205     C                   eval      wrkesito = '3'
073500010205     C                   else
073600010201     C*
073700130510     C* 2° invio EDIVAB
073800010201     C                   reset                   dscmz
073900010201     C                   move      vlrpoi        cmzdst
074000010201     C                   eval      cmzfld = vlrfou
074100010201     C                   eval      cmzmbd = vlrhdl
074200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074300021009     C***                if        prmfir = *blanks
074400130510     C                   eval      cmzfla = 'EDIVAB0F'
074500130510     C                   eval      cmzmba = 'EDIVAB0F'
074600021009     C***                else
074700021009     C***                eval      cmzfla = prmfir
074800021009     C***                eval      cmzmba = prmfir
074900021009     C***                endif
075000010201     C                   eval      cmznrr = *zeros
075100010201     C                   move      §ctrokvb      cmznrr
075200021018     C                   eval      cmzlba = vlrfl1
075300010201     C                   call(e)   'TIS711C'
075400010201     C                   parm                    dscmz
075500010201     C                   parm      *blanks       esito
075600010201     C                   if        %error
075700010201     C                             or cmzerr = '1'
075800010201     C                             or esito  = '1'
075900010201     C                   eval      wrkesito = '3'
076000010201     C                   endif
076100010205     C                   endif
076200990920     C*
076300000613     C                   ENDSR
076400000613     C***
076500070411
076600070411     C     *pssr         BEGSR
076700070411     C*
076800070411     C                   if        %open(tivin00r)
076900070411     C                   close     tivin00r
077000070411     C                   endif
077100130510     C                   if        %open(edivabwr)
077200130510     C                   close     edivabwr
077300130510     C                   endif
077400130510     C                   if        %open(edivatwr)
077500130510     C                   close     edivatwr
077600130510     C                   endif
077700070411     C*
077800070411     C* Effettuo la chiamata al CLLE preposto
077900130510     C                   call(e)   'TITVEVTC'
078000070411     C                   parm                    parccm
078100070411     C                   parm                    parmbr
078200070411     C                   parm      '2'           paropz
078300070411     C*
078400070411     C                   eval      wrkesito = '2'
078500070411     C*
078600070411     C                   seton                                        LR
078700070411     C*
078800070411     C                   ENDSR     '*CANCL'
078900070411     C***
079000070411
079100990910
079200000613     C     *inzsr        BEGSR
079300990910     C*
079400990910     C     *entry        plist
079500990920     C                   parm                    tivlrds
079600990921     C                   parm      wrkesito      esito
079700000724     C                   parm                    prmlit
079800000710     C                   parm                    prmfir
079900130510     C*
080000130510     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
080100130510     C                   move(p)   vlrMSG        wNomeFile
080200000613     C*
080300000830     C* CALCOLA LA DATA CORRENTE
080400100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080500100324     C                   eval      datcor = %dec(%date() : *ISO)
080600130510     C                   time                    wn14             14 0
080700130510     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080800130510     C                   movel     wn14          oracor            6 0          *ORA
080900000830     C*
081000000613     C                   ENDSR
081100000613     C***
