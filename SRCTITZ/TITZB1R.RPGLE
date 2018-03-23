000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140225      *
000300140225      * PARTICOLARITA':
000400140225      * importare VABCAD senza trasformarlo in numerico (trattarlo come alfanumerico allineato a SX)
000500140225      * VABRSD è il concat di 2 stringhe
000600140225      *
000700081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000800990908
000900990910     Ftivin00r  uF   E             DISK    usropn
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
002000060307     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000140210     D curSped         s             10    INZ(*blanks)
003100140210     D depSped         s             10    INZ(*blanks)
003200140225     D w82             s             82    INZ(*blanks)
003300000830
003400000830     D*------------------
003500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600000830     D*------------------
003700000830     D WLBDA8          DS                  INZ
003800000830     D  G08DAT                 1      8  0
003900000830     D  G08INV                 9     16  0
004000000830     D  G08ERR                17     17
004100000830     D  G08TGI                18     22  0
004200010201
004300081118
004400081118     D*------------------
004500081118     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
004600081118     D*------------------
004700081118     D PiStr           S            100A   INZ
004800081118     D PiDecChr        S              1A   INZ
004900081118     D PiVal           S             63S30 INZ
005000081118     D PiInt           S               N   INZ
005100081118     D PiNum           S               N   INZ
005200081118
005300081118     D*------------------
005400081118     D* LINKING A DEFINIZIONI ESTERNE
005500081118     D*------------------
005600081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005700081118
005800081118
005900010201
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300081121     C*
006400081121     C* SALVO IL P.O. INVIO INIZIALE
006500081121     C                   z-add     vlrpoi        sv_vlrpoi         3 0
006600000613     C*
006700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006800000613     C*
006900010202     C* Effettuo la chiamata al CLLE preposto
007000040506     C                   call(e)   'TITVVTC'
007100010202     C                   parm                    parccm
007200010202     C                   parm                    parmbr
007300010202     C                   parm      '2'           paropz
007400050201     C*
007500050201     C* Effettuo lancio TISI95 solo x chiusura
007600050201     C                   CLEAR                   TISI95DS
007700050201     C                   EVAL      I95TLA = 'C'
007800050201     C                   CALL      'TISI95R'
007900050201     C                   PARM                    TISI95DS
008000000616     C*
008100000801     C
008200010201     C                   seton                                        LR
008300990908
008400000801
008500910830     C*--------------------------------------------------------
008600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008700910830     C*--------------------------------------------------------
008800040526     C     RWFILE        BEGSR
008900990910     C*
009000990914     C                   if        not %open(tivin00r)
009100990908     C                   open      tivin00r
009200990914     C                   endif
009300021113     C                   if        not %open(fivabwwr)
009400021113     C                   open      fivabwwr
009500990914     C                   endif
009600070103     C*
009700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009800020305     C                   exsr      prevat
009900010201     C*
010000010202     C                   if        chkcall = '0'
010100010202     C*
010200021113     C                   if        not %open(fivatwwr)
010300021113     C                   open      fivatwwr
010400010201     C                   endif
010500990910     C*
010600010201     C                   clear                   §CTROKVB          5 0
010700020305     C                   clear                   §CTROKVT          5 0
010800000801     C                   clear                   §CTRMO            5 0
010900000801     C                   clear                   §CTRNO            5 0
011000990910     C*
011100921023     C                   DO        *HIVAL
011200990913     C*
011300990915     C                   READ      tivin00r                               70
011400050627     C                   if        vindta > *blanks
011500000613     C                   add       1             rrnum
011600000801     C*
011700000801     C                   if        *in70 = *off
011800000801     C                             and
011900000801     C                             (vinflg = *blanks
012000000801     C                              or vinflg = '0'
012100000801     C                              or vinflg = '2')
012200000801     C*
012300000801     C                   clear                   vinmsg
012400000801     C                   eval      vinflg = '1'
012500070103     C*
012600070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
012700140210     C                   eval      curSped = %subst(vindta:694:10)
012800070213     C                   if        curSped <> depSped
012900070213     C* Se prima bolla => importo bolla corrente
013000070213     C                   if        §CTRMO = *zeros
013100070213     C                   exsr      inzvar
013200070213     C                   exsr      defcam
013300070213     C                   exsr      impvab                                       => carico  VAB
013400070213     C                   else
013500070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013600070213     C                   exsr      wrivab                                       => scarico VAB
013700070213     C                   exsr      inzvar
013800070213     C                   exsr      defcam
013900070213     C                   exsr      impvab                                       => carico  VAB
014000070213     C                   endif
014100071121     C                   exsr      exevate                                      => write VAT-E
014200070103     C* Salvo il raggruppamento spedizione cliente corrente
014300070103     C                   eval      depSped = curSped
014400070213     C*
014500070213     C* Se collo successivo x stessa bolla
014600070213     C                   else
014700070213     C                   exsr      impvab                                       => carico  VAB
014800070213     C                   exsr      exevate                                      => write VAT-E
014900070103     C                   endif
015000000905     C*
015100000905     C                   else
015200000905     C                   eval      vinflg = '1'
015300050628     C                   endif
015400000905     C                   endif
015500000905     C*
015600000905     C  N70              update    tivin000
015700000905     C*
015800991022     C  N70              ENDdo
015900070213     C*
016000070213     C* Scarico testata bolla rimasta in sospesa
016100070213     C                   exsr      wrivab                                       => scarico VAB
016200010202     C*
016300010202     C                   endif
016400990910
016500990910     C* Se non ci sono record con errori ...
016600000710     C                   if        §ctrno = 0
016700990910     C* ... restituisco esito OK.
016800990921     C                   eval      wrkesito = '0'
016900990910     C                   else
017000010201     C                   if        §ctrokvb > 0
017100990921     C                   eval      wrkesito = '1'
017200000710     C                   else
017300000710     C                   eval      wrkesito = '2'
017400990910     C                   endif
017500000710     C                   endif
017600990910     C*
017700990914     C                   if        %open(tivin00r)
017800990908     C                   close     tivin00r
017900990914     C                   endif
018000021113     C                   if        %open(fivabwwr)
018100021113     C                   close     fivabwwr
018200990914     C                   endif
018300021113     C                   if        %open(fivatwwr)
018400021113     C                   close     fivatwwr
018500010201     C                   endif
018600990910     C*
018700010201     C                   if        §ctrokvb > 0
018800000724     C                             and vlrpoi <> *zeros
018900010202     C                   exsr      invio
019000990920     C                   endif
019100990920     C*
019200910830     C                   ENDSR
019300000613     C***
019400010305
019500010305     C*----------------------------------------------------*
019600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019700010305     C*----------------------------------------------------*
019800020305     C     WRIVAB        BEGSR
019900010305     C*
020000060225     C* Quindi scarico il buffer del file d testata
020100021113     C                   write     fivab000                                     => scarico il VAB
020200010305     C*
020300010305     C                   ENDSR
020400990920
020500000801     C*----------------------------------------------------*
020600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020700000801     C*----------------------------------------------------*
020800010201     C     INZVAR        BEGSR
020900000801     C*
021000070213     C                   CLEAR                   FIVAB000
021100070213     C                   CLEAR                   FIVAT000
021200070213     C*
021300040802     C                   Z-ADD     *zeros        Num5_0            5 0
021400040802     C                   MOVEL     '0'           FlgCAS            1
021500000801     C*
021600000801     C                   ENDSR
021700000801     C*----------------------------------------------------*
021800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021900000801     C*----------------------------------------------------*
022000000801     C     DEFCAM        BEGSR
022100000801     C*
022200020619     C* Imposto i valori di default...
022300140210     C                   Z-ADD     0897650       VABCCM
022400140210     C                   Z-ADD     0897650       VATCCM
022500081120     C                   Z-ADD     089           VABLNP
022600140210     C                   Z-ADD     089           VATLNP
022700140210     C                   Z-ADD     090           VABCTR
022800070103     C                   MOVEL     '7Q'          VABCTM
022900040823     C                   MOVEL     '1'           VABCBO
023000020619     C* ... e poi verifico se sono stati passati come parametri
023100020619     C                   IF        vlrppt > *blanks
023200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABCCM
023700020619     C                   Z-ADD     PiVal         VATCCM
023800020619     C                   ENDIF
023900040506     C                   ENDIF
024000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024200020619     C                   EXSR      CHKNUM
024300020619     C                   IF        PiInt=*on
024400020619     C                   Z-ADD     PiVal         VABLNP
024500020619     C                   Z-ADD     PiVal         VATLNP
024600040506     C                   ENDIF
024700020619     C                   ENDIF
024800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025000020619     C                   EXSR      CHKNUM
025100020619     C                   IF        PiInt=*on
025200020619     C                   Z-ADD     PiVal         VABCTR
025300040506     C                   ENDIF
025400020619     C                   ENDIF
025500020619     C                   ENDIF
025600000801     C*
025700000801     C                   ENDSR
025800000801     C*----------------------------------------------------*
025900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026000000801     C*----------------------------------------------------*
026100040823     C     IMPVAB        BEGSR
026200010305     C*
026300000801     C                   Z-ADD     *zeros        errore            1 0
026400000830     C                   MOVEL     datcor        VABAAS
026500020305     C                   MOVEL     datcor        VATAAS
026600040526     C                   MOVE      datcor        VABMGS
026700040823     C                   MOVE(P)   vlrpoi        VABFGS
026800040823     C                   MOVE(P)   vlrpoi        VATFGS
026900080409     C*
027000080409     C* Reperimento campi ALFA
027100080409     C*
027200080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
027300140225     C                   EVAL      w82   =%trim(%subst(vindta:211:40))
027400140225     C                                    + ' ' + %trim(%subst(vindta:251:41))
027500140225     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027600140225     C     '@':'A'       XLATE     w82           w82
027700140225     C                   EVAL      VABRSD=%subst(w82:1:35)
027800080409     C* ==
027900140225     C                   EVAL      VABRD2=%trim(%subst(vindta:724:60))
028000081120     C                   EVAL      VABIND=%trim(%subst(vindta:292:35))
028100081120     C                   EVAL      VABLOD=%trim(%subst(vindta:336:30))
028200140212     C***                EVAL      VABPRD=%trim(%subst(vindta:366:2))
028300140226     C                   EVAL      VABCAD=%trim(%subst(vindta:327:9))
028400140212     C                   EVAL      VABNZD=%trim(%subst(vindta:368:3))
028500140212     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
028600140212     C                   IF        %trim(VABNZD) = 'I'   OR
028700140212     C                             %trim(VABNZD) = 'IT'  OR
028800140212     C                             %trim(VABNZD) = 'ITA'
028900140212     C                   EVAL      VABNZD = *blanks
029000140212     C                   ENDIF
029100140210     C                   EVAL      VABRMA=%trim(%subst(vindta:694:10))
029200081120     C                   MOVEL     *blanks       wNOTE            70
029300081120     C                   EVAL      wNOTE =%trim(%subst(vindta:431:50))+
029400081120     C                                    %trim(%subst(vindta:371:18))
029500081120     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
029600081118     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:35))
029700111031     C* CBO
029800081120     C                   EVAL      VABCBO=%trim(%subst(vindta:430:1))
029900081120     C                   SELECT
030000081120     C                   WHEN      VABCBO = 'Y'
030100081120     C                   EVAL      VABCBO = '2'
030200081120     C                   WHEN      VABCBO = 'Z'
030300081120     C                   EVAL      VABCBO = '1'
030400081120     C                   WHEN      VABCBO = 'F'
030500081120     C                   EVAL      VABCBO = '1'
030600081120     C                   WHEN      VABCBO = 'X'
030700081120     C                   EVAL      VABCBO = '1'
030800081120     C                   WHEN      VABCBO = 'J'
030900081120     C                   EVAL      VABCBO = '1'
031000081120     C                   ENDSL
031100111031     C* GC1
031200111031     C                   EVAL      %subst(VABGC1:1:1)=
031300111031     C                                    %trim(%subst(vindta:659:1))
031400111031     C                   EVAL      %subst(VABGC1:2:1)=
031500111031     C                                    %trim(%subst(vindta:660:1))
031600111031     C* TCR
031700111031     C                   SELECT
031800111031     C                   WHEN      %subst(vindta:661:1) = 'P'
031900111031     C                   EVAL      VABTCR = 'P'
032000111031     C                   WHEN      %subst(vindta:661:1) = 'D'
032100111031     C                   EVAL      VABTCR = 'D'
032200111031     C                   ENDSL
032300111031     C* DCR
032400111031     C                   IF        %subst(vindta:662:8) <> *blanks AND
032500111031     C                             %subst(vindta:662:8) <> *all'0'
032600111031     C                   EVAL      PiStr=%trim(%subst(vindta:662:8))
032700111031     C                   EXSR      CHKNUM
032800111031     C                   IF        PiInt=*on
032900111031     C                   Z-ADD     PiVal         VABDCR
033000111031     C                   ELSE
033100111031     C                   ADD       1             errore
033200111031     C                   Z-ADD     *zeros        VABDCR
033300111031     C                   EVAL      vinmsg = %trimr(vinmsg)
033400111031     C                             + ' ' + 'VABDCR'
033500111031     C                   ENDIF
033600111031     C                   ENDIF
033700111031     C* HCR
033800111031     C                   IF        %subst(vindta:670:4) <> *blanks AND
033900111031     C                             %subst(vindta:670:4) <> *all'0'
034000111031     C                   EVAL      PiStr=%trim(%subst(vindta:670:8))
034100111031     C                   EXSR      CHKNUM
034200111031     C                   IF        PiInt=*on
034300111031     C                   Z-ADD     PiVal         VABHCR
034400111031     C                   ELSE
034500111031     C                   ADD       1             errore
034600111031     C                   Z-ADD     *zeros        VABHCR
034700111031     C                   EVAL      vinmsg = %trimr(vinmsg)
034800111031     C                             + ' ' + 'VABHCR'
034900111031     C                   ENDIF
035000111031     C                   ENDIF
035100111031     C* TC1
035200111031     C                   EVAL      VABTC1=%trim(%subst(vindta:674:1))
035300080409     C*
035400111031     C* FFD
035500111031     C                   EVAL      VABFFD=%trim(%subst(vindta:693:1))
035600111031     C*
035700080409     C* Reperimento campi NUMERICI
035800080409     C                   MOVEL     DATCOR        VABAAS
035900080409     C                   MOVE      DATCOR        VABMGS
036000081120     C* CCM
036100081120     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
036200081120     C                   EXSR      CHKNUM
036300081120     C                   IF        PiInt=*on
036400081120     C                   Z-ADD     PiVal         VABCCM
036500081120     C                   MOVEL     VABCCM        VABLNP
036600081121     C                   MOVEL     VABCCM        VATLNP
036700081121     C                   IF        sv_vlrpoi = 999
036800081121     C                   Z-ADD     VABLNP        VABFGS
036900081121     C                   Z-ADD     VATLNP        VATFGS
037000081121     C                   Z-ADD     VABFGS        vlrpoi
037100081121     C                   ENDIF
037200081120     C                   ELSE
037300081120     C                   ADD       1             errore
037400081120     C                   Z-ADD     *zeros        VABCCM
037500081120     C                   Z-ADD     *zeros        VABLNP
037600081120     C                   EVAL      vinmsg = %trimr(vinmsg)
037700081120     C                             + ' ' + 'VABCCM VABLNP'
037800081120     C                   ENDIF
037900081216     C*********
038000081216     C* Forzature rispetto al CCM
038100081216     C                   IF        VABCCM = 0893665 OR
038200081216     C                             VABCCM = 0892224 OR
038300081216     C                             VABCCM = 0892225 OR
038400081216     C                             VABCCM = 0892864 OR
038500091207     C                             VABCCM = 0893375 OR
038600101013     C                             VABCCM = 0895870 OR
038700081216     C                             VABCCM = 0893300
038800081216     C                   EVAL      VABLNP = 089
038900081216     C                   EVAL      VABFGS = 089
039000081216     C                   ENDIF
039100081216     C                   IF        VABCCM = 1050240 OR
039200081216     C                             VABCCM = 1050588
039300081216     C                   EVAL      VABLNP = 105
039400081216     C                   EVAL      VABFGS = 105
039500081216     C                   ENDIF
039600081216     C                   IF        VABCCM = 0894815 OR
039700081216     C                             VABCCM = 0892667 OR
039800081216     C                             VABCCM = 0894580
039900081216     C                   EVAL      VABLNP = 202
040000081216     C                   EVAL      VABFGS = 202
040100081216     C                   ENDIF
040200091207     C                   IF        VABCCM = 0893375 OR
040300091207     C                             VABCCM = 1050588
040400091207     C                   EVAL      VABCTR = 300
040500091207     C                   ENDIF
040600091207     C                   IF        VABCCM = 0893665 OR
040700091207     C                             VABCCM = 0892224 OR
040800091207     C                             VABCCM = 0892225 OR
040900091207     C                             VABCCM = 0892864 OR
041000091207     C                             VABCCM = 0894815 OR
041100091207     C                             VABCCM = 0892667 OR
041200091207     C                             VABCCM = 0893300
041300091207     C                   EVAL      VABCTR = 000
041400091207     C                   ENDIF
041500091207     C                   IF        VABCCM = 0895079
041600091207     C                   EVAL      VABRMO = 'DIVITA SRL'
041700091207     C                   EVAL      VABCMO = '21013'
041800091207     C                   EVAL      VABTIC = 'OM'
041900091207     C                   ENDIF
042000120718     C                   IF        VABCCM = 0896473
042100120718     C                   EVAL      VABFGS = 050
042200120718     C                   EVAL      VATFGS = 050
042300120718     C                   EVAL      vlrpoi = 050
042400120718     C                   ENDIF
042500120718     C                   IF        VABCCM = 0895205
042600120725     C                   EVAL      VABFGS = 089
042700120725     C                   EVAL      VATFGS = 089
042800120725     C                   EVAL      vlrpoi = 089
042900120718     C                   ENDIF
043000081216     C*********
043100081216     C
043200080409     C* NSP/RMN
043300140210     C                   EVAL      PiStr=%trim(%subst(vindta:694:10))
043400080409     C                   EXSR      CHKNUM
043500080409     C                   IF        PiInt=*on
043600080409     C                   Z-ADD     PiVal         VABNSP
043700080409     C                   Z-ADD     PiVal         VATNSP
043800080409     C                   Z-ADD     PiVal         VABRMN
043900080409     C                   ELSE
044000080409     C                   ADD       1             errore
044100080409     C                   Z-ADD     *zeros        VABNSP
044200080409     C                   Z-ADD     1             VABRMN
044300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
044400080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
044500080409     C                   ENDIF
044600080409     C* CAD
044700140225     C***                EVAL      PiStr=%trim(%subst(vindta:327:5))
044800140225     C***                EXSR      CHKNUM
044900140225     C***                IF        PiInt=*on
045000140225     C***                Z-ADD     PiVal         Num5_0
045100140225     C***                MOVEL(p)  Num5_0        VABCAD
045200140225     C***                ELSE
045300140225     C***                ADD       1             errore
045400140225     C***                EVAL      VABCAD = *zeros
045500140225     C***                EVAL      vinmsg = %trimr(vinmsg)
045600140225     C***                          + ' ' + 'VABCAD'
045700140225     C***                ENDIF
045800080409     C* NCL
045900081120     C                   EVAL      PiStr=%trim(%subst(vindta:481:6))
046000080409     C                   EXSR      CHKNUM
046100080409     C                   IF        PiInt=*on
046200080409     C                   Z-ADD     PiVal         VABNCL
046300080409     C                   ELSE
046400080409     C                   ADD       1             errore
046500080409     C                   Z-ADD     *zeros        VABNCL
046600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
046700080409     C                             + ' ' + 'VABNCL'
046800080409     C                   ENDIF
046900080409     C* PKB
047000081120     C                   EVAL      PiStr=%trim(%subst(vindta:487:9))
047100080409     C                   EXSR      CHKNUM
047200080409     C                   IF        PiNum=*on
047300081120     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
047400081120     C                   Z-ADD(H)  PiVal         VABPKB
047500080409     C                   ELSE
047600080409     C                   ADD       1             errore
047700080409     C                   Z-ADD     *zeros        VABPKB
047800080409     C                   EVAL      vinmsg = %trimr(vinmsg)
047900080409     C                             + ' ' + 'VABPKB'
048000080409     C                   ENDIF
048100081120     C* VLB
048200081120     C                   EVAL      PiStr=%trim(%subst(vindta:496:17))
048300081120     C                   EXSR      CHKNUM
048400081120     C                   IF        PiNum=*on
048500081120     C                   EVAL      PiVal = PiVal / 1000000                      * gestisco 6 dec.
048600081120     C                   Z-ADD(H)  PiVal         VABVLB
048700081120     C                   ELSE
048800081120     C                   ADD       1             errore
048900081120     C                   Z-ADD     *zeros        VABVLB
049000081120     C                   EVAL      vinmsg = %trimr(vinmsg)
049100081120     C                             + ' ' + 'VABVLB'
049200081120     C                   ENDIF
049300080409     C* CAS
049400081120     C                   IF        %subst(vindta:516:17) <> *all'0'
049500080409     C                   EVAL      FlgCAS = '1'
049600080415     C                   EVAL      VABVCA = 'EUR'
049700081120     C                   EVAL      PiStr=%trim(%subst(vindta:516:17))
049800080409     C                   EXSR      CHKNUM
049900080409     C                   IF        PiNum=*on
050000090507     C                   EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
050100090507     C                   Z-ADD(H)  PiVal         VABCAS
050200080409     C                   ELSE
050300080409     C                   ADD       1             errore
050400080409     C                   Z-ADD     *zeros        VABCAS
050500080409     C                   EVAL      vinmsg = %trimr(vinmsg)
050600080409     C                             + ' ' + 'VABCAS'
050700080409     C                   ENDIF
050800080409     C                   ENDIF
050900071121     C*
051000080415     C* Carico l'estensioni A e B del FNVAT
051100080415     C                   exsr      exevata
051200071121     C                   exsr      exevatb
051300010205     C*
051400010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051500040802     C                   IF        FlgCAS <> '0'
051600081118     C                   IF        VABCBO = '1'
051700010205     C                   EVAL      VABCBO = '4'
051800010205     C                   ENDIF
051900081118     C                   IF        VABCBO = '2'
052000081118     C                   EVAL      VABCBO = '6'
052100081118     C                   ENDIF
052200081118     C                   ENDIF
052300020305     C*
052400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052500011113     C                   EXSR      CHKIMPDIV
052600010202     C*
052700000801     C* Ebbene...
052800000801     C                   ADD       1             §CTRMO
052900010201     C                   IF        errore <> *zeros
053000000801     C                   ADD       1             §CTRNO
053100000801     C                   EVAL      vinflg = '2'
053200000801     C                   ELSE
053300010201     C                   ADD       1             §CTROKVB
053400000801     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700070102     C*----------------------------------------------------*
053800070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
053900070102     C*----------------------------------------------------*
054000070103     C     EXEVATE       BEGSR
054100081118     C*
054200081120     C                   EVAL      VATTRC = 'E'
054300081120     C                   EVAL      VATNOT=%subst(vindta:645:14)
054400070102     C*
054500081120     C                   exsr      wrivat                                       => scarico VAT
054600070102     C*
054700070102     C                   ENDSR
054800080415     C*----------------------------------------------------*
054900080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
055000080415     C*----------------------------------------------------*
055100080415     C     EXEVATA       BEGSR
055200080415     C*
055300080415     C*
055400080415     C***                exsr      wrivat                                       => scarico VAT
055500080415     C*
055600080415     C                   ENDSR
055700071121     C*----------------------------------------------------*
055800071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
055900071121     C*----------------------------------------------------*
056000071121     C     EXEVATB       BEGSR
056100071121     C*
056200111031     C                   EVAL      VATTRC = 'B'
056300111031     C                   EVAL      VATNOT=%subst(vindta:675:18)
056400071121     C*
056500111031     C                   exsr      wrivat                                       => scarico VAT
056600071121     C*
056700071121     C                   ENDSR
056800010201     C*----------------------------------------------------*
056900040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
057000010201     C*----------------------------------------------------*
057100020305     C     WRIVAT        BEGSR
057200081118     C*
057300081118     C* Imposto gli stessi campi chiave della testata relativa
057400081216     C                   EVAL      VATFGS = VABFGS
057500081118     C                   EVAL      VATAAS = VABAAS
057600081118     C                   EVAL      VATLNP = VABLNP
057700081118     C                   EVAL      VATNRS = VABNRS
057800081118     C                   EVAL      VATNSP = VABNSP
057900081118     C                   EVAL      VATCCM = VABCCM
058000050628     C*
058100060223     C* Scrivo solo se valorizzato qualcosa
058200060223     C                   IF        VATNOT <> *blanks
058300040802     C                   WRITE     FIVAT000
058400060223     C                   ENDIF
058500010201     C*
058600010201     C                   ENDSR
058700010202     C*----------------------------------------------------*
058800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058900010202     C*----------------------------------------------------*
059000020305     C     PREVAT        BEGSR
059100010202     C*
059200021113     C* Compongo il nome del membro da dare al FIVATWWR
059300010202     C                   eval      parmbr = vlrhdl
059400010202     C                   movel     'M'           parmbr
059500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
059600010202     C                   eval      paropz = '1'
059700010202     C* Effettuo la chiamata al CLLE preposto
059800040506     C                   call(e)   'TITVVTC'
059900010202     C                   parm                    parccm
060000010202     C                   parm                    parmbr
060100010202     C                   parm                    paropz
060200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060300010202     C                   if        %error
060400010202     C                   movel     '1'           chkcall
060500010202     C                   else
060600010202     C                   movel     '0'           chkcall
060700010202     C                   endif
060800010202     C*
060900010202     C                   ENDSR
061000000801     C*----------------------------------------------------*
061100000801     C*  CONTROLLO NUMERICITA' CAMPI
061200000801     C*----------------------------------------------------*
061300000801     C     CHKNUM        BEGSR
061400081118     C*
061500081118     C                   IF        PiDecChr = *blanks
061600081118     C                   EVAL      PiDecChr = ','
061700081118     C                   ENDIF
061800081118     C*
061900081118     C                   callp(e)  UBISNUM_Check(PiStr
062000081118     C                                          :PiDecChr
062100081118     C                                          :PiVal
062200081118     C                                          :PiNum
062300081118     C                                          :PiInt)
062400081118     C*
062500081118     C                   IF        %error
062600081118     C                   EVAL      PiInt=*off
062700081118     C                   ENDIF
062800000801     C*
062900000801     C                   ENDSR
063000000801     C***
063100000801
063200011113
063300011113     C*----------------------------------------------------*
063400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063500011113     C*----------------------------------------------------*
063600011113     C     CHKIMPDIV     BEGSR
063700011113     C*
063800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063900011113     C                   Z-ADD     *zeros        wrkDec            9 9
064000011113     C*
064100011113     C* Come prima cosa effettuo considerazioni sulla divisa
064200011113     C                   IF        vabIAS > *zeros
064300011113     C                   IF        vabVAS <> 'EUR'
064400011113     C                   EVAL      vabVAS =  'ITL'
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C                   IF        vabCAS > *zeros
064900011113     C                   IF        vabVCA <> 'EUR'
065000011113     C                   EVAL      vabVCA =  'ITL'
065100011113     C                   ENDIF
065200011113     C                   ENDIF
065300011113     C*
065400011113     C                   IF        vabVMD > *zeros
065500020305     C                   IF        vabVAD <> 'EUR'
065600011113     C                   EVAL      vabVAD =  'ITL'
065700011113     C                   ENDIF
065800011113     C                   ENDIF
065900011113     C*
066000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066100011113     C                   Z-ADD     vabIAS        wrkDec
066200011113     C                   IF        wrkDec > *zeros
066300011113     C                   IF        vabVAS = 'ITL'
066400011113     C                   EVAL      vabIAS = *zeros
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066900011113     C                   Z-ADD     vabCAS        wrkDec
067000011113     C                   IF        wrkDec > *zeros
067100011113     C                   IF        vabVCA = 'ITL'
067200011113     C                   EVAL      vabCAS = *zeros
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067700011113     C                   Z-ADD     vabVMD        wrkDec
067800011113     C                   IF        wrkDec > *zeros
067900011113     C                   IF        vabVAD = 'ITL'
068000011113     C                   EVAL      vabVMD = *zeros
068100011113     C                   ENDIF
068200011113     C                   ENDIF
068300011113     C*
068400011113     C                   ENDSR
068500011113     C***
068600011113
068700011113
068800000801
068900000801
069000990920      /TITLE Invio dei dati al punto operativo.
069100010202     C     invio         BEGSR
069200990920     C*
069300021113     C* 1° invio FIVAT
069400010201     C                   reset                   dscmz
069500010201     C                   move      vlrpoi        cmzdst
069600021113     C                   eval      cmzfld = 'FIVATWWR'
069700010201     C                   eval      cmzmbd = vlrhdl
069800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069900021009     C***                if        prmfir = *blanks
070000021113     C                   eval      cmzfla = 'FIVAT00F'
070100021113     C                   eval      cmzmba = 'FIVAT00F'
070200021009     C***                else
070300021009     C***                eval      cmzfla = prmfir
070400021009     C***                eval      cmzmba = prmfir
070500021009     C***                endif
070600010201     C                   eval      cmznrr = *zeros
070700020305     C                   move      §ctrokvt      cmznrr
070800021018     C                   eval      cmzlba = vlrfl1
070900010201     C                   call(e)   'TIS711C'
071000010201     C                   parm                    dscmz
071100010201     C                   parm      *blanks       esito
071200010205     C                   if        %error
071300010205     C                             or cmzerr = '1'
071400010205     C                             or esito  = '1'
071500010205     C                   eval      wrkesito = '3'
071600010205     C                   else
071700010201     C*
071800021113     C* 2° invio FIVAB
071900010201     C                   reset                   dscmz
072000010201     C                   move      vlrpoi        cmzdst
072100010201     C                   eval      cmzfld = vlrfou
072200010201     C                   eval      cmzmbd = vlrhdl
072300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072400021009     C***                if        prmfir = *blanks
072500021113     C                   eval      cmzfla = 'FIVAB00F'
072600021113     C                   eval      cmzmba = 'FIVAB00F'
072700021009     C***                else
072800021009     C***                eval      cmzfla = prmfir
072900021009     C***                eval      cmzmba = prmfir
073000021009     C***                endif
073100010201     C                   eval      cmznrr = *zeros
073200010201     C                   move      §ctrokvb      cmznrr
073300021018     C                   eval      cmzlba = vlrfl1
073400010201     C                   call(e)   'TIS711C'
073500010201     C                   parm                    dscmz
073600010201     C                   parm      *blanks       esito
073700010201     C                   if        %error
073800010201     C                             or cmzerr = '1'
073900010201     C                             or esito  = '1'
074000010201     C                   eval      wrkesito = '3'
074100010201     C                   endif
074200010205     C                   endif
074300990920     C*
074400000613     C                   ENDSR
074500000613     C***
074600070411
074700070411     C     *pssr         BEGSR
074800070411     C*
074900070411     C                   if        %open(tivin00r)
075000070411     C                   close     tivin00r
075100070411     C                   endif
075200070411     C                   if        %open(fivabwwr)
075300070411     C                   close     fivabwwr
075400070411     C                   endif
075500070411     C                   if        %open(fivatwwr)
075600070411     C                   close     fivatwwr
075700070411     C                   endif
075800070411     C*
075900070411     C* Effettuo la chiamata al CLLE preposto
076000070411     C                   call(e)   'TITVVTC'
076100070411     C                   parm                    parccm
076200070411     C                   parm                    parmbr
076300070411     C                   parm      '2'           paropz
076400070411     C*
076500070411     C                   eval      wrkesito = '2'
076600070411     C*
076700070411     C                   seton                                        LR
076800070411     C*
076900070411     C                   ENDSR     '*CANCL'
077000070411     C***
077100070411
077200990910
077300000613     C     *inzsr        BEGSR
077400990910     C*
077500990910     C     *entry        plist
077600990920     C                   parm                    tivlrds
077700990921     C                   parm      wrkesito      esito
077800000724     C                   parm                    prmlit
077900000710     C                   parm                    prmfir
078000000613     C*
078100000830     C* CALCOLA LA DATA CORRENTE
078200000830     C                   time                    wn14             14 0
078300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
078400000830     C                   z-add     wn8           g08dat
078500000830     C                   z-add     *zeros        g08inv
078600000830     C                   movel     '0'           g08err
078700000830     C                   call      'XSRDA8'
078800000830     C                   parm                    wlbda8
078900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
079000000830     C*
079100000613     C                   ENDSR
079200000613     C***
