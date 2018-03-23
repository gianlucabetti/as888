000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500060307     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500081120     D curSped         s              6    INZ(*blanks)
002600081120     D depSped         s              6    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600010201
003700081118
003800081118     D*------------------
003900081118     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
004000081118     D*------------------
004100081118     D PiStr           S            100A   INZ
004200081118     D PiDecChr        S              1A   INZ
004300081118     D PiVal           S             63S30 INZ
004400081118     D PiInt           S               N   INZ
004500081118     D PiNum           S               N   INZ
004600081118
004700081118     D*------------------
004800081118     D* LINKING A DEFINIZIONI ESTERNE
004900081118     D*------------------
005000081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005100081118
005200081118
005300010201
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700081121     C*
005800081121     C* SALVO IL P.O. INVIO INIZIALE
005900081121     C                   z-add     vlrpoi        sv_vlrpoi         3 0
006000000613     C*
006100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006200000613     C*
006300010202     C* Effettuo la chiamata al CLLE preposto
006400040506     C                   call(e)   'TITVVTC'
006500010202     C                   parm                    parccm
006600010202     C                   parm                    parmbr
006700010202     C                   parm      '2'           paropz
006800050201     C*
006900050201     C* Effettuo lancio TISI95 solo x chiusura
007000050201     C                   CLEAR                   TISI95DS
007100050201     C                   EVAL      I95TLA = 'C'
007200050201     C                   CALL      'TISI95R'
007300050201     C                   PARM                    TISI95DS
007400000616     C*
007500000801     C
007600010201     C                   seton                                        LR
007700990908
007800000801
007900910830     C*--------------------------------------------------------
008000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008100910830     C*--------------------------------------------------------
008200040526     C     RWFILE        BEGSR
008300990910     C*
008400990914     C                   if        not %open(tivin00r)
008500990908     C                   open      tivin00r
008600990914     C                   endif
008700021113     C                   if        not %open(fivabwwr)
008800021113     C                   open      fivabwwr
008900990914     C                   endif
009000070103     C*
009100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009200020305     C                   exsr      prevat
009300010201     C*
009400010202     C                   if        chkcall = '0'
009500010202     C*
009600021113     C                   if        not %open(fivatwwr)
009700021113     C                   open      fivatwwr
009800010201     C                   endif
009900990910     C*
010000010201     C                   clear                   §CTROKVB          5 0
010100020305     C                   clear                   §CTROKVT          5 0
010200000801     C                   clear                   §CTRMO            5 0
010300000801     C                   clear                   §CTRNO            5 0
010400990910     C*
010500921023     C                   DO        *HIVAL
010600990913     C*
010700990915     C                   READ      tivin00r                               70
010800050627     C                   if        vindta > *blanks
010900000613     C                   add       1             rrnum
011000000801     C*
011100000801     C                   if        *in70 = *off
011200000801     C                             and
011300000801     C                             (vinflg = *blanks
011400000801     C                              or vinflg = '0'
011500000801     C                              or vinflg = '2')
011600000801     C*
011700000801     C                   clear                   vinmsg
011800000801     C                   eval      vinflg = '1'
011900070103     C*
012000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
012100081120     C                   eval      curSped = %subst(vindta:197:6)
012200070213     C                   if        curSped <> depSped
012300070213     C* Se prima bolla => importo bolla corrente
012400070213     C                   if        §CTRMO = *zeros
012500070213     C                   exsr      inzvar
012600070213     C                   exsr      defcam
012700070213     C                   exsr      impvab                                       => carico  VAB
012800070213     C                   else
012900070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
013000070213     C                   exsr      wrivab                                       => scarico VAB
013100070213     C                   exsr      inzvar
013200070213     C                   exsr      defcam
013300070213     C                   exsr      impvab                                       => carico  VAB
013400070213     C                   endif
013500071121     C                   exsr      exevate                                      => write VAT-E
013600070103     C* Salvo il raggruppamento spedizione cliente corrente
013700070103     C                   eval      depSped = curSped
013800070213     C*
013900070213     C* Se collo successivo x stessa bolla
014000070213     C                   else
014100070213     C                   exsr      impvab                                       => carico  VAB
014200070213     C                   exsr      exevate                                      => write VAT-E
014300070103     C                   endif
014400000905     C*
014500000905     C                   else
014600000905     C                   eval      vinflg = '1'
014700050628     C                   endif
014800000905     C                   endif
014900000905     C*
015000000905     C  N70              update    tivin000
015100000905     C*
015200991022     C  N70              ENDdo
015300070213     C*
015400070213     C* Scarico testata bolla rimasta in sospesa
015500070213     C                   exsr      wrivab                                       => scarico VAB
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
017400021113     C                   if        %open(fivabwwr)
017500021113     C                   close     fivabwwr
017600990914     C                   endif
017700021113     C                   if        %open(fivatwwr)
017800021113     C                   close     fivatwwr
017900010201     C                   endif
018000990910     C*
018100010201     C                   if        §ctrokvb > 0
018200000724     C                             and vlrpoi <> *zeros
018300010202     C                   exsr      invio
018400990920     C                   endif
018500990920     C*
018600910830     C                   ENDSR
018700000613     C***
018800010305
018900010305     C*----------------------------------------------------*
019000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019100010305     C*----------------------------------------------------*
019200020305     C     WRIVAB        BEGSR
019300010305     C*
019400060225     C* Quindi scarico il buffer del file d testata
019500021113     C                   write     fivab000                                     => scarico il VAB
019600010305     C*
019700010305     C                   ENDSR
019800990920
019900000801     C*----------------------------------------------------*
020000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020100000801     C*----------------------------------------------------*
020200010201     C     INZVAR        BEGSR
020300000801     C*
020400070213     C                   CLEAR                   FIVAB000
020500070213     C                   CLEAR                   FIVAT000
020600070213     C*
020700040802     C                   Z-ADD     *zeros        Num5_0            5 0
020800040802     C                   MOVEL     '0'           FlgCAS            1
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500000801     C*
021600020619     C* Imposto i valori di default...
021700081120     C                   Z-ADD     0894815       VABCCM
021800081120     C                   Z-ADD     0894815       VATCCM
021900081120     C                   Z-ADD     089           VABLNP
022000081120     C                   Z-ADD     089           VATLNP
022100081118     C                   Z-ADD     000           VABCTR
022200070103     C                   MOVEL     '7Q'          VABCTM
022300040823     C                   MOVEL     '1'           VABCBO
022400081121     C                   MOVEL     'C'           VABTSP
022500110321     C***                MOVEL     'CHIMICI'     VABNAS
022600020619     C* ... e poi verifico se sono stati passati come parametri
022700020619     C                   IF        vlrppt > *blanks
022800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABCCM
023300020619     C                   Z-ADD     PiVal         VATCCM
023400020619     C                   ENDIF
023500040506     C                   ENDIF
023600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABLNP
024100020619     C                   Z-ADD     PiVal         VATLNP
024200040506     C                   ENDIF
024300020619     C                   ENDIF
024400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020619     C                   EXSR      CHKNUM
024700020619     C                   IF        PiInt=*on
024800020619     C                   Z-ADD     PiVal         VABCTR
024900040506     C                   ENDIF
025000020619     C                   ENDIF
025100020619     C                   ENDIF
025200000801     C*
025300000801     C                   ENDSR
025400000801     C*----------------------------------------------------*
025500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025600000801     C*----------------------------------------------------*
025700040823     C     IMPVAB        BEGSR
025800010305     C*
025900000801     C                   Z-ADD     *zeros        errore            1 0
026000000830     C                   MOVEL     datcor        VABAAS
026100020305     C                   MOVEL     datcor        VATAAS
026200040526     C                   MOVE      datcor        VABMGS
026300040823     C                   MOVE(P)   vlrpoi        VABFGS
026400040823     C                   MOVE(P)   vlrpoi        VATFGS
026500080409     C*
026600080409     C* Reperimento campi ALFA
026700080409     C*
026800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
026900081120     C                   EVAL      VABRSD=%trim(%subst(vindta:211:40))
027000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
027200080409     C* ==
027300081120     C                   EVAL      VABRD2=%trim(%subst(vindta:251:41))
027400081120     C                   EVAL      VABIND=%trim(%subst(vindta:292:35))
027500081120     C                   EVAL      VABLOD=%trim(%subst(vindta:336:30))
027600081120     C                   EVAL      VABPRD=%trim(%subst(vindta:366:2))
027700090114     C                   EVAL      VABRMA=%trim(%subst(vindta:193:10))
027800081120     C                   MOVEL     *blanks       wNOTE            70
027900081120     C                   EVAL      wNOTE =%trim(%subst(vindta:431:50))+
028000081120     C                                    %trim(%subst(vindta:371:18))
028100081120     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
028200081118     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:35))
028300111031     C* CBO
028400081120     C                   EVAL      VABCBO=%trim(%subst(vindta:430:1))
028500081120     C                   SELECT
028600081120     C                   WHEN      VABCBO = 'Y'
028700081120     C                   EVAL      VABCBO = '2'
028800081120     C                   WHEN      VABCBO = 'Z'
028900081120     C                   EVAL      VABCBO = '1'
029000081120     C                   WHEN      VABCBO = 'F'
029100081120     C                   EVAL      VABCBO = '1'
029200081120     C                   WHEN      VABCBO = 'X'
029300081120     C                   EVAL      VABCBO = '1'
029400081120     C                   WHEN      VABCBO = 'J'
029500081120     C                   EVAL      VABCBO = '1'
029600081120     C                   ENDSL
029700111031     C* GC1
029800111031     C                   EVAL      %subst(VABGC1:1:1)=
029900111031     C                                    %trim(%subst(vindta:659:1))
030000111031     C                   EVAL      %subst(VABGC1:2:1)=
030100111031     C                                    %trim(%subst(vindta:660:1))
030200111031     C* TCR
030300111031     C                   SELECT
030400111031     C                   WHEN      %subst(vindta:661:1) = 'P'
030500111031     C                   EVAL      VABTCR = 'P'
030600111031     C                   WHEN      %subst(vindta:661:1) = 'D'
030700111031     C                   EVAL      VABTCR = 'D'
030800111031     C                   ENDSL
030900111031     C* DCR
031000111031     C                   IF        %subst(vindta:662:8) <> *blanks AND
031100111031     C                             %subst(vindta:662:8) <> *all'0'
031200111031     C                   EVAL      PiStr=%trim(%subst(vindta:662:8))
031300111031     C                   EXSR      CHKNUM
031400111031     C                   IF        PiInt=*on
031500111031     C                   Z-ADD     PiVal         VABDCR
031600111031     C                   ELSE
031700111031     C                   ADD       1             errore
031800111031     C                   Z-ADD     *zeros        VABDCR
031900111031     C                   EVAL      vinmsg = %trimr(vinmsg)
032000111031     C                             + ' ' + 'VABDCR'
032100111031     C                   ENDIF
032200111031     C                   ENDIF
032300111031     C* HCR
032400111031     C                   IF        %subst(vindta:670:4) <> *blanks AND
032500111031     C                             %subst(vindta:670:4) <> *all'0'
032600111031     C                   EVAL      PiStr=%trim(%subst(vindta:670:8))
032700111031     C                   EXSR      CHKNUM
032800111031     C                   IF        PiInt=*on
032900111031     C                   Z-ADD     PiVal         VABHCR
033000111031     C                   ELSE
033100111031     C                   ADD       1             errore
033200111031     C                   Z-ADD     *zeros        VABHCR
033300111031     C                   EVAL      vinmsg = %trimr(vinmsg)
033400111031     C                             + ' ' + 'VABHCR'
033500111031     C                   ENDIF
033600111031     C                   ENDIF
033700111031     C* TC1
033800111031     C                   EVAL      VABTC1=%trim(%subst(vindta:674:1))
033900080409     C*
034000111031     C* FFD
034100111031     C                   EVAL      VABFFD=%trim(%subst(vindta:693:1))
034200111031     C*
034300080409     C* Reperimento campi NUMERICI
034400080409     C                   MOVEL     DATCOR        VABAAS
034500080409     C                   MOVE      DATCOR        VABMGS
034600081120     C* CCM
034700081120     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
034800081120     C                   EXSR      CHKNUM
034900081120     C                   IF        PiInt=*on
035000081120     C                   Z-ADD     PiVal         VABCCM
035100081120     C                   MOVEL     VABCCM        VABLNP
035200081121     C                   MOVEL     VABCCM        VATLNP
035300081121     C                   IF        sv_vlrpoi = 999
035400081121     C                   Z-ADD     VABLNP        VABFGS
035500081121     C                   Z-ADD     VATLNP        VATFGS
035600081121     C                   Z-ADD     VABFGS        vlrpoi
035700081121     C                   ENDIF
035800081120     C                   ELSE
035900081120     C                   ADD       1             errore
036000081120     C                   Z-ADD     *zeros        VABCCM
036100081120     C                   Z-ADD     *zeros        VABLNP
036200081120     C                   EVAL      vinmsg = %trimr(vinmsg)
036300081120     C                             + ' ' + 'VABCCM VABLNP'
036400081120     C                   ENDIF
036500081216     C*********
036600081216     C* Forzature rispetto al CCM
036700081216     C                   IF        VABCCM = 0893665 OR
036800081216     C                             VABCCM = 0892224 OR
036900081216     C                             VABCCM = 0892225 OR
037000081216     C                             VABCCM = 0892864 OR
037100091207     C                             VABCCM = 0893375 OR
037200101013     C                             VABCCM = 0895870 OR
037300081216     C                             VABCCM = 0893300
037400081216     C                   EVAL      VABLNP = 089
037500081216     C                   EVAL      VABFGS = 089
037600081216     C                   ENDIF
037700081216     C                   IF        VABCCM = 1050240 OR
037800081216     C                             VABCCM = 1050588
037900081216     C                   EVAL      VABLNP = 105
038000081216     C                   EVAL      VABFGS = 105
038100081216     C                   ENDIF
038200081216     C                   IF        VABCCM = 0894815 OR
038300081216     C                             VABCCM = 0892667 OR
038400081216     C                             VABCCM = 0894580
038500081216     C                   EVAL      VABLNP = 202
038600081216     C                   EVAL      VABFGS = 202
038700081216     C                   ENDIF
038800091207     C                   IF        VABCCM = 0893375 OR
038900091207     C                             VABCCM = 1050588
039000091207     C                   EVAL      VABCTR = 300
039100091207     C                   ENDIF
039200091207     C                   IF        VABCCM = 0893665 OR
039300091207     C                             VABCCM = 0892224 OR
039400091207     C                             VABCCM = 0892225 OR
039500091207     C                             VABCCM = 0892864 OR
039600091207     C                             VABCCM = 0894815 OR
039700091207     C                             VABCCM = 0892667 OR
039800091207     C                             VABCCM = 0893300
039900091207     C                   EVAL      VABCTR = 000
040000091207     C                   ENDIF
040100091207     C                   IF        VABCCM = 0895079
040200091207     C                   EVAL      VABRMO = 'DIVITA SRL'
040300091207     C                   EVAL      VABCMO = '21013'
040400091207     C                   EVAL      VABTIC = 'OM'
040500091207     C                   ENDIF
040600120718     C                   IF        VABCCM = 0896473
040700151001     C                   EVAL      VABFGS = 089
040800151001     C                   EVAL      VATFGS = 089
040900151001     C                   EVAL      vlrpoi = 089
041000120718     C                   ENDIF
041100120718     C                   IF        VABCCM = 0895205
041200120725     C                   EVAL      VABFGS = 089
041300120725     C                   EVAL      VATFGS = 089
041400120725     C                   EVAL      vlrpoi = 089
041500120718     C                   ENDIF
041600171011     C                   IF        VABCCM = 1600941
041700171011     C                   EVAL      VABRMO = 'DIVITA SRL'
041800171011     C                   EVAL      VABCMO = '21013'
041900171011     C                   EVAL      VABTIC = 'TO'
042000171011     C                   ENDIF
042100081216     C*********
042200081216     C
042300080409     C* NSP/RMN
042400081120     C                   EVAL      PiStr=%trim(%subst(vindta:197:6))
042500080409     C                   EXSR      CHKNUM
042600080409     C                   IF        PiInt=*on
042700080409     C                   Z-ADD     PiVal         VABNSP
042800080409     C                   Z-ADD     PiVal         VATNSP
042900080409     C                   Z-ADD     PiVal         VABRMN
043000080409     C                   ELSE
043100080409     C                   ADD       1             errore
043200080409     C                   Z-ADD     *zeros        VABNSP
043300080409     C                   Z-ADD     1             VABRMN
043400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
043500080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
043600080409     C                   ENDIF
043700080409     C* CAD
043800081120     C                   EVAL      PiStr=%trim(%subst(vindta:327:5))
043900080409     C                   EXSR      CHKNUM
044000080409     C                   IF        PiInt=*on
044100080409     C                   Z-ADD     PiVal         Num5_0
044200080409     C                   MOVEL(p)  Num5_0        VABCAD
044300080409     C                   ELSE
044400080409     C                   ADD       1             errore
044500080409     C                   EVAL      VABCAD = *zeros
044600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
044700080409     C                             + ' ' + 'VABCAD'
044800080409     C                   ENDIF
044900080409     C* NCL
045000081120     C                   EVAL      PiStr=%trim(%subst(vindta:481:6))
045100080409     C                   EXSR      CHKNUM
045200080409     C                   IF        PiInt=*on
045300080409     C                   Z-ADD     PiVal         VABNCL
045400080409     C                   ELSE
045500080409     C                   ADD       1             errore
045600080409     C                   Z-ADD     *zeros        VABNCL
045700080409     C                   EVAL      vinmsg = %trimr(vinmsg)
045800080409     C                             + ' ' + 'VABNCL'
045900080409     C                   ENDIF
046000080409     C* PKB
046100081120     C                   EVAL      PiStr=%trim(%subst(vindta:487:9))
046200080409     C                   EXSR      CHKNUM
046300080409     C                   IF        PiNum=*on
046400081120     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
046500081120     C                   Z-ADD(H)  PiVal         VABPKB
046600080409     C                   ELSE
046700080409     C                   ADD       1             errore
046800080409     C                   Z-ADD     *zeros        VABPKB
046900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
047000080409     C                             + ' ' + 'VABPKB'
047100080409     C                   ENDIF
047200081120     C* VLB
047300081120     C                   EVAL      PiStr=%trim(%subst(vindta:496:17))
047400081120     C                   EXSR      CHKNUM
047500081120     C                   IF        PiNum=*on
047600081120     C                   EVAL      PiVal = PiVal / 1000000                      * gestisco 6 dec.
047700081120     C                   Z-ADD(H)  PiVal         VABVLB
047800081120     C                   ELSE
047900081120     C                   ADD       1             errore
048000081120     C                   Z-ADD     *zeros        VABVLB
048100081120     C                   EVAL      vinmsg = %trimr(vinmsg)
048200081120     C                             + ' ' + 'VABVLB'
048300081120     C                   ENDIF
048400080409     C* CAS
048500081120     C                   IF        %subst(vindta:516:17) <> *all'0'
048600080409     C                   EVAL      FlgCAS = '1'
048700080415     C                   EVAL      VABVCA = 'EUR'
048800081120     C                   EVAL      PiStr=%trim(%subst(vindta:516:17))
048900080409     C                   EXSR      CHKNUM
049000080409     C                   IF        PiNum=*on
049100090507     C                   EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
049200090507     C                   Z-ADD(H)  PiVal         VABCAS
049300080409     C                   ELSE
049400080409     C                   ADD       1             errore
049500080409     C                   Z-ADD     *zeros        VABCAS
049600080409     C                   EVAL      vinmsg = %trimr(vinmsg)
049700080409     C                             + ' ' + 'VABCAS'
049800080409     C                   ENDIF
049900080409     C                   ENDIF
050000071121     C*
050100080415     C* Carico l'estensioni A e B del FNVAT
050200080415     C                   exsr      exevata
050300071121     C                   exsr      exevatb
050400010205     C*
050500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
050600040802     C                   IF        FlgCAS <> '0'
050700081118     C                   IF        VABCBO = '1'
050800010205     C                   EVAL      VABCBO = '4'
050900010205     C                   ENDIF
051000081118     C                   IF        VABCBO = '2'
051100081118     C                   EVAL      VABCBO = '6'
051200081118     C                   ENDIF
051300081118     C                   ENDIF
051400020305     C*
051500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051600011113     C                   EXSR      CHKIMPDIV
051700010202     C*
051800000801     C* Ebbene...
051900000801     C                   ADD       1             §CTRMO
052000010201     C                   IF        errore <> *zeros
052100000801     C                   ADD       1             §CTRNO
052200000801     C                   EVAL      vinflg = '2'
052300000801     C                   ELSE
052400010201     C                   ADD       1             §CTROKVB
052500000801     C                   ENDIF
052600000801     C*
052700000801     C                   ENDSR
052800070102     C*----------------------------------------------------*
052900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
053000070102     C*----------------------------------------------------*
053100070103     C     EXEVATE       BEGSR
053200081118     C*
053300081120     C                   EVAL      VATTRC = 'E'
053400081120     C                   EVAL      VATNOT=%subst(vindta:645:14)
053500070102     C*
053600081120     C                   exsr      wrivat                                       => scarico VAT
053700070102     C*
053800070102     C                   ENDSR
053900080415     C*----------------------------------------------------*
054000080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
054100080415     C*----------------------------------------------------*
054200080415     C     EXEVATA       BEGSR
054300080415     C*
054400080415     C*
054500080415     C***                exsr      wrivat                                       => scarico VAT
054600080415     C*
054700080415     C                   ENDSR
054800071121     C*----------------------------------------------------*
054900071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
055000071121     C*----------------------------------------------------*
055100071121     C     EXEVATB       BEGSR
055200071121     C*
055300111031     C                   EVAL      VATTRC = 'B'
055400111031     C                   EVAL      VATNOT=%subst(vindta:675:18)
055500071121     C*
055600111031     C                   exsr      wrivat                                       => scarico VAT
055700071121     C*
055800071121     C                   ENDSR
055900010201     C*----------------------------------------------------*
056000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
056100010201     C*----------------------------------------------------*
056200020305     C     WRIVAT        BEGSR
056300081118     C*
056400081118     C* Imposto gli stessi campi chiave della testata relativa
056500081216     C                   EVAL      VATFGS = VABFGS
056600081118     C                   EVAL      VATAAS = VABAAS
056700081118     C                   EVAL      VATLNP = VABLNP
056800081118     C                   EVAL      VATNRS = VABNRS
056900081118     C                   EVAL      VATNSP = VABNSP
057000081118     C                   EVAL      VATCCM = VABCCM
057100050628     C*
057200060223     C* Scrivo solo se valorizzato qualcosa
057300060223     C                   IF        VATNOT <> *blanks
057400040802     C                   WRITE     FIVAT000
057500060223     C                   ENDIF
057600010201     C*
057700010201     C                   ENDSR
057800010202     C*----------------------------------------------------*
057900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058000010202     C*----------------------------------------------------*
058100020305     C     PREVAT        BEGSR
058200010202     C*
058300021113     C* Compongo il nome del membro da dare al FIVATWWR
058400010202     C                   eval      parmbr = vlrhdl
058500010202     C                   movel     'M'           parmbr
058600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
058700010202     C                   eval      paropz = '1'
058800010202     C* Effettuo la chiamata al CLLE preposto
058900040506     C                   call(e)   'TITVVTC'
059000010202     C                   parm                    parccm
059100010202     C                   parm                    parmbr
059200010202     C                   parm                    paropz
059300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059400010202     C                   if        %error
059500010202     C                   movel     '1'           chkcall
059600010202     C                   else
059700010202     C                   movel     '0'           chkcall
059800010202     C                   endif
059900010202     C*
060000010202     C                   ENDSR
060100000801     C*----------------------------------------------------*
060200000801     C*  CONTROLLO NUMERICITA' CAMPI
060300000801     C*----------------------------------------------------*
060400000801     C     CHKNUM        BEGSR
060500081118     C*
060600081118     C                   IF        PiDecChr = *blanks
060700081118     C                   EVAL      PiDecChr = ','
060800081118     C                   ENDIF
060900081118     C*
061000081118     C                   callp(e)  UBISNUM_Check(PiStr
061100081118     C                                          :PiDecChr
061200081118     C                                          :PiVal
061300081118     C                                          :PiNum
061400081118     C                                          :PiInt)
061500081118     C*
061600081118     C                   IF        %error
061700081118     C                   EVAL      PiInt=*off
061800081118     C                   ENDIF
061900000801     C*
062000000801     C                   ENDSR
062100000801     C***
062200000801
062300011113
062400011113     C*----------------------------------------------------*
062500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
062600011113     C*----------------------------------------------------*
062700011113     C     CHKIMPDIV     BEGSR
062800011113     C*
062900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063000011113     C                   Z-ADD     *zeros        wrkDec            9 9
063100011113     C*
063200011113     C* Come prima cosa effettuo considerazioni sulla divisa
063300011113     C                   IF        vabIAS > *zeros
063400011113     C                   IF        vabVAS <> 'EUR'
063500011113     C                   EVAL      vabVAS =  'ITL'
063600011113     C                   ENDIF
063700011113     C                   ENDIF
063800011113     C*
063900011113     C                   IF        vabCAS > *zeros
064000011113     C                   IF        vabVCA <> 'EUR'
064100011113     C                   EVAL      vabVCA =  'ITL'
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   IF        vabVMD > *zeros
064600020305     C                   IF        vabVAD <> 'EUR'
064700011113     C                   EVAL      vabVAD =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065200011113     C                   Z-ADD     vabIAS        wrkDec
065300011113     C                   IF        wrkDec > *zeros
065400011113     C                   IF        vabVAS = 'ITL'
065500011113     C                   EVAL      vabIAS = *zeros
065600011113     C                   ENDIF
065700011113     C                   ENDIF
065800011113     C*
065900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066000011113     C                   Z-ADD     vabCAS        wrkDec
066100011113     C                   IF        wrkDec > *zeros
066200011113     C                   IF        vabVCA = 'ITL'
066300011113     C                   EVAL      vabCAS = *zeros
066400011113     C                   ENDIF
066500011113     C                   ENDIF
066600011113     C*
066700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066800011113     C                   Z-ADD     vabVMD        wrkDec
066900011113     C                   IF        wrkDec > *zeros
067000011113     C                   IF        vabVAD = 'ITL'
067100011113     C                   EVAL      vabVMD = *zeros
067200011113     C                   ENDIF
067300011113     C                   ENDIF
067400011113     C*
067500011113     C                   ENDSR
067600011113     C***
067700011113
067800011113
067900000801
068000000801
068100990920      /TITLE Invio dei dati al punto operativo.
068200010202     C     invio         BEGSR
068300990920     C*
068400021113     C* 1° invio FIVAT
068500010201     C                   reset                   dscmz
068600010201     C                   move      vlrpoi        cmzdst
068700021113     C                   eval      cmzfld = 'FIVATWWR'
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100021113     C                   eval      cmzfla = 'FIVAT00F'
069200021113     C                   eval      cmzmba = 'FIVAT00F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800020305     C                   move      §ctrokvt      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010205     C                   if        %error
070400010205     C                             or cmzerr = '1'
070500010205     C                             or esito  = '1'
070600010205     C                   eval      wrkesito = '3'
070700010205     C                   else
070800010201     C*
070900021113     C* 2° invio FIVAB
071000010201     C                   reset                   dscmz
071100010201     C                   move      vlrpoi        cmzdst
071200010201     C                   eval      cmzfld = vlrfou
071300010201     C                   eval      cmzmbd = vlrhdl
071400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071500021009     C***                if        prmfir = *blanks
071600021113     C                   eval      cmzfla = 'FIVAB00F'
071700021113     C                   eval      cmzmba = 'FIVAB00F'
071800021009     C***                else
071900021009     C***                eval      cmzfla = prmfir
072000021009     C***                eval      cmzmba = prmfir
072100021009     C***                endif
072200010201     C                   eval      cmznrr = *zeros
072300010201     C                   move      §ctrokvb      cmznrr
072400021018     C                   eval      cmzlba = vlrfl1
072500010201     C                   call(e)   'TIS711C'
072600010201     C                   parm                    dscmz
072700010201     C                   parm      *blanks       esito
072800010201     C                   if        %error
072900010201     C                             or cmzerr = '1'
073000010201     C                             or esito  = '1'
073100010201     C                   eval      wrkesito = '3'
073200010201     C                   endif
073300010205     C                   endif
073400990920     C*
073500000613     C                   ENDSR
073600000613     C***
073700070411
073800070411     C     *pssr         BEGSR
073900070411     C*
074000070411     C                   if        %open(tivin00r)
074100070411     C                   close     tivin00r
074200070411     C                   endif
074300070411     C                   if        %open(fivabwwr)
074400070411     C                   close     fivabwwr
074500070411     C                   endif
074600070411     C                   if        %open(fivatwwr)
074700070411     C                   close     fivatwwr
074800070411     C                   endif
074900070411     C*
075000070411     C* Effettuo la chiamata al CLLE preposto
075100070411     C                   call(e)   'TITVVTC'
075200070411     C                   parm                    parccm
075300070411     C                   parm                    parmbr
075400070411     C                   parm      '2'           paropz
075500070411     C*
075600070411     C                   eval      wrkesito = '2'
075700070411     C*
075800070411     C                   seton                                        LR
075900070411     C*
076000070411     C                   ENDSR     '*CANCL'
076100070411     C***
076200070411
076300990910
076400000613     C     *inzsr        BEGSR
076500990910     C*
076600990910     C     *entry        plist
076700990920     C                   parm                    tivlrds
076800990921     C                   parm      wrkesito      esito
076900000724     C                   parm                    prmlit
077000000710     C                   parm                    prmfir
077100000613     C*
077200000830     C* CALCOLA LA DATA CORRENTE
077300000830     C                   time                    wn14             14 0
077400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
077500000830     C                   z-add     wn8           g08dat
077600000830     C                   z-add     *zeros        g08inv
077700000830     C                   movel     '0'           g08err
077800000830     C                   call      'XSRDA8'
077900000830     C                   parm                    wlbda8
078000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
078100000830     C*
078200000613     C                   ENDSR
078300000613     C***
