000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021017     FFIVABwwr  O    E             DISK    usropn
000600021017     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000050517     D depspe          s              6    INZ(*blanks)
002100050517     D curspe          s              6    INZ(*blanks)
002200050329     D cntspe          s              9  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700010213
002800000801     D  Num5_0         s              5  0
002900030107     D  Num7_0         s              7  0
003000050517     D  wpkb           s              7  2
003100070625
003200070625     D wExport         s              1    INZ(*blanks)
003300000830
003400000830     D*------------------
003500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600000830     D*------------------
003700000830     D WLBDA8          DS                  INZ
003800000830     D  G08DAT                 1      8  0
003900000830     D  G08INV                 9     16  0
004000000830     D  G08ERR                17     17
004100000830     D  G08TGI                18     22  0
004200070625     D*------------------
004300070625     D* DS REPERIMENTO NUMERATORE
004400070625     D*------------------
004500070625     D trul33ds      e ds                  inz
004600070625     D*------------------
004700070625     D* DS ARCHITETTURA
004800070625     D*------------------
004900070625     D kpjba         e ds                  inz
005000990908
005100010201
005200010201
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000613     C*
005700030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005800000613     C*
005900010202     C* Effettuo la chiamata al CLLE preposto
006000031201     C                   call(e)   'TITVVTC'
006100010202     C                   parm                    parccm
006200010202     C                   parm                    parmbr
006300010202     C                   parm      '2'           paropz
006400000616     C*
006500010201     C                   seton                                        LR
006600990908
006700000801
006800910830     C*--------------------------------------------------------
006900030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
007000910830     C*--------------------------------------------------------
007100030226     C     RWFILE        BEGSR
007200990910     C*
007300990914     C                   if        not %open(tivin00r)
007400990908     C                   open      tivin00r
007500990914     C                   endif
007600021017     C                   if        not %open(fivabwwr)
007700021017     C                   open      fivabwwr
007800990914     C                   endif
007900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008000020305     C                   exsr      prevat
008100010201     C*
008200010202     C                   if        chkcall = '0'
008300010202     C*
008400021017     C                   if        not %open(fivatwwr)
008500021017     C                   open      fivatwwr
008600010201     C                   endif
008700990910     C*
008800010201     C                   clear                   §CTROKVB          5 0
008900020305     C                   clear                   §CTROKVT          5 0
009000000801     C                   clear                   §CTRMO            5 0
009100000801     C                   clear                   §CTRNO            5 0
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600000905     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600030310     C                   eval      vinflg = '1'
010700030310     C*
010800030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
010900050517     C                   eval      PiStr=%trim(%subst(vindta:19:6))
011000030411     C                   movel(p)  PiStr         curspe
011100030411     C                   add       1             cntspe
011200030411     C*
011300030411     C                   if        cntspe = 1
011400030411     C* A rottura inizializzo campi di totalizzazione
011500030411     C                   clear                   fivab000
011600030411     C*
011700030411     C                   exsr      impvab                                       => carico VAB
011800031201     C                   exsr      exevat                                       => carico VAT
011900030411     C                   eval      depspe = curspe
012000030411     C                   else
012100070625     C                   if        wExport = 'N'                                * ovvero se DPD
012200070625     C                   exsr      wrivab                                       => scarico VAB
012300070625     C* A rottura inizializzo campi di totalizzazione
012400070625     C                   clear                   fivab000
012500070625     C*
012600070625     C                   exsr      impvab                                       => carico VAB
012700070625     C                   exsr      exevat                                       => carico VAT
012800070625     C                   eval      depspe = curspe
012900070625     C                   else
013000030411     C                   if        curspe <> depspe
013100030411     C                   exsr      wrivab                                       => scarico VAB
013200030411     C* A rottura inizializzo campi di totalizzazione
013300030411     C                   clear                   fivab000
013400030411     C*
013500030411     C                   exsr      impvab                                       => carico VAB
013600031201     C                   exsr      exevat                                       => carico VAT
013700030411     C                   eval      depspe = curspe
013800030411     C                   else
013900030411     C                   exsr      impvab                                       => carico VAB
014000031201     C                   exsr      exevat                                       => carico VAT
014100030411     C                   endif
014200070625     C                   endif
014300030411     C                   endif
014400000905     C*
014500000905     C                   else
014600000905     C                   eval      vinflg = '1'
014700000905     C                   endif
014800030411     C                   endif
014900000905     C*
015000000905     C  N70              update    tivin000
015100000905     C*
015200991022     C  N70              ENDdo
015300030430     C*
015400030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
015500030430     C                   exsr      wrivab                                       => scarico VAB
015600010202     C*
015700010202     C                   endif
015800990910
015900990910     C* Se non ci sono record con errori ...
016000000710     C                   if        §ctrno = 0
016100990910     C* ... restituisco esito OK.
016200990921     C                   eval      wrkesito = '0'
016300990910     C                   else
016400010201     C                   if        §ctrokvb > 0
016500990921     C                   eval      wrkesito = '1'
016600000710     C                   else
016700000710     C                   eval      wrkesito = '2'
016800990910     C                   endif
016900000710     C                   endif
017000990910     C*
017100990914     C                   if        %open(tivin00r)
017200990908     C                   close     tivin00r
017300990914     C                   endif
017400021017     C                   if        %open(fivabwwr)
017500021017     C                   close     fivabwwr
017600990914     C                   endif
017700021017     C                   if        %open(fivatwwr)
017800021017     C                   close     fivatwwr
017900010201     C                   endif
018000021017     C*
018100021017     C                   if        vlrpoi <> 999
018200021017     C                   eval      vlrpoi = vlrpoi
018300021017     C                   endif
018400990910     C*
018500010201     C                   if        §ctrokvb > 0
018600021017     C                             and vlrpoi > *zeros
018700010202     C                   exsr      invio
018800990920     C                   endif
018900990920     C*
019000910830     C                   ENDSR
019100000613     C***
019200990920
019300000801     C*----------------------------------------------------*
019400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019500000801     C*----------------------------------------------------*
019600010201     C     INZVAR        BEGSR
019700000801     C*
019800010201     C                   Z-ADD     *zeros        Num5_0
019900030107     C                   Z-ADD     *zeros        Num7_0
020000030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
020100030310     C                   MOVEL     '0'           FlgCAS            1
020200000801     C*
020300000801     C                   ENDSR
020400000801     C*----------------------------------------------------*
020500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020600000801     C*----------------------------------------------------*
020700000801     C     DEFCAM        BEGSR
020800000801     C*
020900020619     C* Imposto i valori di default...
021000050517     C                   Z-ADD     0250297       VABCCM
021100050517     C                   Z-ADD     0250297       VATCCM
021200021017     C                   Z-ADD     vlrpoi        VABFGS
021300021017     C                   Z-ADD     vlrpoi        VATFGS
021400050517     C                   Z-ADD     025           VABLNP
021500050517     C                   Z-ADD     025           VATLNP
021600070507     C                   Z-ADD     100           VABCTR
021700021017     C                   MOVEL     '7Q'          VABCTM
021800070730     C                   MOVEL     'D'           VABTSP
021900050321     C                   MOVEL     '1'           VABCBO
022000020619     C* ... e poi verifico se sono stati passati come parametri
022100020619     C                   IF        vlrppt > *blanks
022200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022300020619     C                   EXSR      CHKNUM
022400020619     C                   IF        PiInt=*on
022500020619     C                   Z-ADD     PiVal         VABCCM
022600020619     C                   Z-ADD     PiVal         VATCCM
022700020619     C                   ENDIF
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABLNP
023200020619     C                   Z-ADD     PiVal         VATLNP
023300020619     C                   ENDIF
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABCTR
023800020619     C                   ENDIF
023900020619     C                   ENDIF
024000000801     C*
024100000801     C                   ENDSR
024200000801     C*----------------------------------------------------*
024300030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024400000801     C*----------------------------------------------------*
024500010201     C     IMPVAB        BEGSR
024600000801     C*
024700020305     C                   EXSR      INZVAR
024800020305     C                   EXSR      DEFCAM
024900070625     C                   EXSR      OVRCCM
025000010305     C*
025100070625     C* Valorizzazine flag d wrk x identificare se trattasi d traffico export
025200070625     C                   EVAL      wExport=%trim(%subst(vindta:1:1))
025300070625     C*
025400030226     C                   Z-ADD     *zeros        errore            1 0
025500000830     C                   MOVEL     datcor        VABAAS
025600000830     C                   MOVE      datcor        VABMGS
025700030226     C*
025800050517     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
025900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026100020117     C* ==
026200050517     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
026300050517     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
026400060118     C                   EVAL      VABPRD=%trim(%subst(vindta:576:2))
026500050517     C                   EVAL      VABRMA=%trim(%subst(vindta:11:14))
026600120703     C                   EVAL      VABNOT=%trim(%subst(vindta:493:40))
026700120703     C                   EVAL      VABNT2=%trim(%subst(vindta:533:40))
026800120703     C* CAD
026900050517     C                   EVAL      PiStr=%trim(%subst(vindta:319:8))
027000010201     C                   EXSR      CHKNUM
027100010201     C                   IF        PiInt=*on
027200010201     C                   Z-ADD     PiVal         Num5_0
027300021017     C                   MOVEL     Num5_0        VABCAD
027400010201     C                   ELSE
027500010201     C                   ADD       1             errore
027600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027700021017     C                             + ' ' + 'VABCAD'
027800010201     C                   ENDIF
027900031201     C* NSP/RMN
028000050517     C                   EVAL      PiStr=%trim(%subst(vindta:19:6))
028100010201     C                   EXSR      CHKNUM
028200050329     C                   IF        PiInt=*on
028300050317     C                   Z-ADD     PiVal         VABNSP
028400050317     C                   Z-ADD     PiVal         VATNSP
028500031201     C                   Z-ADD     PiVal         VABRMN
028600010201     C                   ELSE
028700050329     C                   ADD       1             errore
028800050329     C                   EVAL      vinmsg = %trimr(vinmsg)
028900031201     C                             + ' ' + 'VABNSP VATNSP VABRMN'
029000010201     C                   ENDIF
029100070625     C*
029200070625     C* X spedizioni export DPD => Stacco un numeratore da AZNUM
029300070625     C                   if        wExport='N'
029400070625     C                   clear                   TRUL33DS
029500070625     C                   eval      I33OPE = *zeros
029600070625     C                   eval      I33CNU = 302
029700070625     C                   eval      I33NUM = 1
029800070625     C                   movel     TRUL33DS      KPJBU
029900070625     C                   call      'TRUL33R'
030000070625     C                   parm                    KPJBA
030100070625     C                   movel     KPJBU         TRUL33DS
030200070625     C                   if        O33ERR = *zeros
030300070625     C                   z-add     O33NRF        VABNSP
030400070625     C                   z-add     O33NRF        VATNSP
030500070625     C                   else
030600070625     C                   Z-ADD     1             errore
030700070625     C                   EVAL      vinmsg = %trimr(vinmsg)
030800070625     C                             + ' ' + 'VABNSP VATNSP'
030900070625     C                   endif
031000070625     C                   endif
031100030226     C* NCL
031200050517     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
031300050317     C                   EXSR      CHKNUM
031400050317     C                   IF        PiInt=*on
031500050517     C                   add       PiVal         VABNCL
031600050317     C                   ELSE
031700050317     C                   ADD       1             errore
031800050317     C                   EVAL      vinmsg = %trimr(vinmsg)
031900050317     C                             + ' ' + 'VABNCL'
032000050317     C                   ENDIF
032100030226     C* PKB
032200050517     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
032300030107     C                   EXSR      CHKNUM
032400030107     C                   IF        PiNum=*on
032500050517     C                   EVAL(h)   PiVal=PiVal/100                              * gestisco 2 decim.
032600050517     C                   add       PiVal         wpkb
032700030107     C                   ELSE
032800030107     C                   ADD       1             errore
032900030107     C                   EVAL      vinmsg = %trimr(vinmsg)
033000030107     C                             + ' ' + 'VABPKB'
033100030107     C                   ENDIF
033200050317     C* CAS/VCA
033300060118     C                   IF        %subst(vindta:390:13) <> '0000000000000'
033400050321     C                   MOVEL     '1'           FlgCAS
033500060118     C                   EVAL      PiStr=%trim(%subst(vindta:390:13))
033600050317     C                   EXSR      CHKNUM
033700050317     C                   IF        PiNum=*on
033800050517     C                   EVAL(h)   PiVal=PiVal/1000                             * gestisco 3 decim.
033900050321     C                   Z-ADD     PiVal         VABCAS
034000050329     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
034100050317     C                   ELSE
034200050317     C                   SETON                                        32
034300050317     C                   Z-ADD     *zeros        VABCAS
034400050317     C                   EVAL      vinmsg = %trimr(vinmsg)
034500050317     C                             + ' ' + 'VABCAS'
034600050317     C                   ENDIF
034700050321     C                   ENDIF
034800081118     C* IAS/VAS
034900081118     C                   IF        %subst(vindta:403:13) <> '0000000000000'
035000081118     C                   EVAL      PiStr=%trim(%subst(vindta:403:13))
035100081118     C                   EXSR      CHKNUM
035200081118     C                   IF        PiNum=*on
035300081118     C                   EVAL(h)   PiVal=PiVal/100                              * gestisco 2 decim.
035400081118     C                   Z-ADD     PiVal         VABIAS
035500081119     C                   EVAL      VABVAS='EUR'
035600081118     C                   ELSE
035700081118     C                   SETON                                        32
035800081118     C                   Z-ADD     *zeros        VABIAS
035900081118     C                   EVAL      vinmsg = %trimr(vinmsg)
036000081118     C                             + ' ' + 'VABIAS'
036100081118     C                   ENDIF
036200081118     C                   ENDIF
036300030310     C*
036400050321     C* Considerazioni finali su CBO/CAS
036500030310     C                   IF        FlgCAS = '1'
036600050321     C                   IF        VABCBO = '1'
036700010205     C                   EVAL      VABCBO = '4'
036800120703     C                   ENDIF
036900050321     C                   IF        VABCBO = '2'
037000050321     C                   EVAL      VABCBO = '6'
037100050321     C                   ENDIF
037200010205     C                   ENDIF
037300020305     C*
037400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037500011113     C                   EXSR      CHKIMPDIV
037600010202     C*
037700000801     C* Ebbene...
037800000801     C                   ADD       1             §CTRMO
037900010201     C                   IF        errore <> *zeros
038000000801     C                   ADD       1             §CTRNO
038100000801     C                   EVAL      vinflg = '2'
038200000801     C                   ELSE
038300010201     C                   ADD       1             §CTROKVB
038400000801     C                   ENDIF
038500000801     C*
038600000801     C                   ENDSR
038700030226     C*----------------------------------------------------*
038800030226     C*  SCARICAMENTO BUFFER RECORDS VAB
038900030226     C*----------------------------------------------------*
039000030226     C     WRIVAB        BEGSR
039100070625     C*
039200070625     C* Imposto il peso totale della spedizione
039300050517     C                   Eval(h)   vabpkb = wpkb
039400050517     C                   Z-ADD     *zeros        wpkb
039500030310     C*
039600030310     C                   write     FIVAB000
039700030226     C*
039800030226     C                   ENDSR
039900070625     C*----------------------------------------------------*
040000070625     C*  FORZATURA CODICI CLIENTI MITTENTI / TARIFFE / TRATTAMENTI MERCE
040100070625     C*----------------------------------------------------*
040200070625     C     OVRCCM        BEGSR
040300070625     C*
040400070625     C* Verifica e forzatura x traffici export
040500070625     C                   IF        %trim(%subst(vindta:1:1)) = 'E'
040600070625     C                   EVAL      VABCCM = 0250481
040700070625     C                   EVAL      VATCCM = 0250481
040800070625     C                   EVAL      VABCTR = 052
040900070730     C                   EVAL      VABTSP = 'C'
041000070625     C                   ENDIF
041100070625     C                   IF        %trim(%subst(vindta:1:1)) = 'N'
041200070625     C                   EVAL      VABCCM = 0250481
041300070625     C                   EVAL      VATCCM = 0250481
041400070625     C                   EVAL      VABCTR = 353
041500070625     C                   EVAL      VABCTM = '7R'
041600070730     C                   EVAL      VABTSP = 'C'
041700070625     C                   ENDIF
041800070625     C*
041900070625     C                   ENDSR
042000030226     C*----------------------------------------------------*
042100030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
042200030226     C*----------------------------------------------------*
042300031201     C     EXEVAT        BEGSR
042400030226     C*
042500030226     C                   EXSR      INZVAR
042600030226     C                   EXSR      DEFCAM
042700070625     C                   EXSR      OVRCCM
042800030226     C*
042900030226     C                   MOVEL     datcor        VATAAS
043000030226     C*
043100030415     C* Valorizzo il barcode
043200120918     C*** ho 2 modalità di inserimento che dipendono dal char a pos. 2 x 1
043300120918     C*** se = 1: da pos. 377 x 6
043400120918     C*** se = 2: da pos. 623 x 12
043500120918     C                   EVAL      VATTRC='E'
043600120918     C* cambiata regola:
043700120918     C* importare VATNOT tipo record 'E' dalla posizione 624 x 12 bytes
043800120918     C***                IF        %trim(%subst(vindta:2:1))='1'
043900120918     C***                EVAL      VATNOT=%trim(%subst(vindta:377:6))
044000120918     C***                ENDIF
044100120918     C***                IF        %trim(%subst(vindta:2:1))='2'
044200120918     C***                EVAL      VATNOT=%trim(%subst(vindta:623:12))
044300120918     C***                ENDIF
044400120918     C                   EVAL      VATNOT=%trim(%subst(vindta:624:12))
044500031201     C                   exsr      wrivat                                       => scarico VAT
044600120703     C*
044700120703     C* Valorizzo i tipi rcd I/J delle e-mail
044800120703     C                   IF        %trim(%subst(vindta:583:40)) <> *blank
044900120703     C                   EVAL      VATTRC='I'
045000120703     C                   EVAL      VATNOT=%trim(%subst(vindta:583:35))
045100120703     C                   exsr      wrivat                                       => scarico VAT
045200120703     C                   IF        %trim(%subst(vindta:583+35:5)) <> *blank
045300120703     C                   EVAL      VATTRC='J'
045400120703     C                   EVAL      VATNOT=%trim(%subst(vindta:583+35:5))
045500120703     C                   exsr      wrivat                                       => scarico VAT
045600120703     C                   ENDIF
045700120703     C                   ENDIF
045800030226     C*
045900030226     C                   ENDSR
046000010201     C*----------------------------------------------------*
046100021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046200010201     C*----------------------------------------------------*
046300020305     C     WRIVAT        BEGSR
046400010201     C*
046500021017     C                   write     FIVAT000
046600010201     C*
046700010201     C                   ENDSR
046800010202     C*----------------------------------------------------*
046900021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047000010202     C*----------------------------------------------------*
047100020305     C     PREVAT        BEGSR
047200010202     C*
047300021017     C* Compongo il nome del membro da dare al FIVATWWR
047400010202     C                   eval      parmbr = vlrhdl
047500010202     C                   movel     'M'           parmbr
047600050317     C                   eval      parccm = %subst(vlrKSC:2:7)
047700010202     C                   eval      paropz = '1'
047800010202     C* Effettuo la chiamata al CLLE preposto
047900031201     C                   call(e)   'TITVVTC'
048000010202     C                   parm                    parccm
048100010202     C                   parm                    parmbr
048200010202     C                   parm                    paropz
048300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048400010202     C                   if        %error
048500010202     C                   movel     '1'           chkcall
048600010202     C                   else
048700010202     C                   movel     '0'           chkcall
048800010202     C                   endif
048900010202     C*
049000010202     C                   ENDSR
049100000801     C*----------------------------------------------------*
049200000801     C*  CONTROLLO NUMERICITA' CAMPI
049300000801     C*----------------------------------------------------*
049400000801     C     CHKNUM        BEGSR
049500050329
049600050329     C                   IF        PiDecChr = *blanks
049700050329     C                   EVAL      PiDecChr = ','
049800050329     C                   ENDIF
049900000801     C*
050000000801     C                   call(e)   'ISNUMERIC'
050100000801     C                   PARM                    PiStr            30
050200050329     C                   PARM                    PiDecChr          1
050300000801     C                   PARM      *ZEROS        PiVal            30 9
050400000801     C                   PARM      '0'           PiInt             1
050500000801     C                   PARM      '0'           PiNum             1
050600000801     C                   IF        %error
050700000801     C                   EVAL      PiInt=*off
050800030107     C                   EVAL      PiNum=*off
050900000801     C                   ENDIF
051000000801     C*
051100000801     C                   ENDSR
051200000801     C***
051300000801
051400011113
051500011113     C*----------------------------------------------------*
051600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051700011113     C*----------------------------------------------------*
051800011113     C     CHKIMPDIV     BEGSR
051900011113     C*
052000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052100011113     C                   Z-ADD     *zeros        wrkDec            9 9
052200011113     C*
052300011113     C* Come prima cosa effettuo considerazioni sulla divisa
052400011113     C                   IF        vabIAS > *zeros
052500011113     C                   IF        vabVAS <> 'EUR'
052600011113     C                   EVAL      vabVAS =  'ITL'
052700011113     C                   ENDIF
052800011113     C                   ENDIF
052900011113     C*
053000011113     C                   IF        vabCAS > *zeros
053100011113     C                   IF        vabVCA <> 'EUR'
053200011113     C                   EVAL      vabVCA =  'ITL'
053300011113     C                   ENDIF
053400011113     C                   ENDIF
053500011113     C*
053600011113     C                   IF        vabVMD > *zeros
053700020305     C                   IF        vabVAD <> 'EUR'
053800011113     C                   EVAL      vabVAD =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054300011113     C                   Z-ADD     vabIAS        wrkDec
054400011113     C                   IF        wrkDec > *zeros
054500011113     C                   IF        vabVAS = 'ITL'
054600011113     C                   EVAL      vabIAS = *zeros
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055100011113     C                   Z-ADD     vabCAS        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVCA = 'ITL'
055400011113     C                   EVAL      vabCAS = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055900011113     C                   Z-ADD     vabVMD        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVAD = 'ITL'
056200011113     C                   EVAL      vabVMD = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C                   ENDSR
056700011113     C***
056800021017
056900000801
057000000801
057100990920      /TITLE Invio dei dati al punto operativo.
057200010202     C     invio         BEGSR
057300990920     C*
057400021017     C* 1° invio FIVAT
057500010201     C                   reset                   dscmz
057600021017     C                   move      vlrpoi        cmzdst
057700021017     C                   eval      cmzfld = 'FIVATWWR'
057800010201     C                   eval      cmzmbd = vlrhdl
057900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058000021017     C                   eval      cmzfla = 'FIVAT00F'
058100021017     C                   eval      cmzmba = 'FIVAT00F'
058200010201     C                   eval      cmznrr = *zeros
058300020305     C                   move      §ctrokvt      cmznrr
058400021018     C                   eval      cmzlba = vlrfl1
058500010201     C                   call(e)   'TIS711C'
058600010201     C                   parm                    dscmz
058700010201     C                   parm      *blanks       esito
058800010205     C                   if        %error
058900010205     C                             or cmzerr = '1'
059000010205     C                             or esito  = '1'
059100010205     C                   eval      wrkesito = '3'
059200010205     C                   else
059300010201     C*
059400021017     C* 2° invio FIVAB
059500010201     C                   reset                   dscmz
059600021017     C                   move      vlrpoi        cmzdst
059700010201     C                   eval      cmzfld = vlrfou
059800010201     C                   eval      cmzmbd = vlrhdl
059900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060000021017     C                   eval      cmzfla = 'FIVAB00F'
060100021017     C                   eval      cmzmba = 'FIVAB00F'
060200010201     C                   eval      cmznrr = *zeros
060300010201     C                   move      §ctrokvb      cmznrr
060400021018     C                   eval      cmzlba = vlrfl1
060500010201     C                   call(e)   'TIS711C'
060600010201     C                   parm                    dscmz
060700010201     C                   parm      *blanks       esito
060800010201     C                   if        %error
060900010201     C                             or cmzerr = '1'
061000010201     C                             or esito  = '1'
061100010201     C                   eval      wrkesito = '3'
061200010201     C                   endif
061300010205     C                   endif
061400990920     C*
061500000613     C                   ENDSR
061600000613     C***
061700070411
061800070411     C     *pssr         BEGSR
061900070411     C*
062000070411     C                   if        %open(tivin00r)
062100070411     C                   close     tivin00r
062200070411     C                   endif
062300070411     C                   if        %open(fivabwwr)
062400070411     C                   close     fivabwwr
062500070411     C                   endif
062600070411     C                   if        %open(fivatwwr)
062700070411     C                   close     fivatwwr
062800070411     C                   endif
062900070411     C*
063000070411     C* Effettuo la chiamata al CLLE preposto
063100070411     C                   call(e)   'TITVVTC'
063200070411     C                   parm                    parccm
063300070411     C                   parm                    parmbr
063400070411     C                   parm      '2'           paropz
063500070411     C*
063600070411     C                   eval      wrkesito = '2'
063700070411     C*
063800070411     C                   seton                                        LR
063900070411     C*
064000070411     C                   ENDSR     '*CANCL'
064100070411     C***
064200070411
064300990910
064400000613     C     *inzsr        BEGSR
064500990910     C*
064600990910     C     *entry        plist
064700990920     C                   parm                    tivlrds
064800990921     C                   parm      wrkesito      esito
064900000724     C                   parm                    prmlit
065000000710     C                   parm                    prmfir
065100000613     C*
065200000830     C* CALCOLA LA DATA CORRENTE
065300000830     C                   time                    wn14             14 0
065400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065500000830     C                   z-add     wn8           g08dat
065600000830     C                   z-add     *zeros        g08inv
065700000830     C                   movel     '0'           g08err
065800000830     C                   call      'XSRDA8'
065900000830     C                   parm                    wlbda8
066000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066100000830     C*
066200000613     C                   ENDSR
066300000613     C***
