000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000010202     D parccm          s              8    INZ(*blanks)
002100010202     D parmbr          s             10    INZ(*blanks)
002200010202     D paropz          s              1    INZ(*blanks)
002300010202     D chkcall         s              1    INZ(*blanks)
002400000830
002500000830     D*------------------
002600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700000830     D*------------------
002800000830     D WLBDA8          DS                  INZ
002900000830     D  G08DAT                 1      8  0
003000000830     D  G08INV                 9     16  0
003100000830     D  G08ERR                17     17
003200000830     D  G08TGI                18     22  0
003300041025     D*------------------
003400041025     D* DS REPERIMENTO NUMERATORE
003500041025     D*------------------
003600041025     D trul33ds      e ds                  inz
003700041025     D*------------------
003800041025     D* DS ARCHITETTURA
003900041025     D*------------------
004000041025     D kpjba         e ds                  inz
004100041025     D*------------------
004200990908
004300010201
004400010201
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000613     C*
004900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200040506     C                   call(e)   'TITVVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600000616     C*
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800021113     C                   if        not %open(fivabwwr)
006900021113     C                   open      fivabwwr
007000990914     C                   endif
007100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600021113     C                   if        not %open(fivatwwr)
007700021113     C                   open      fivatwwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400040910     C*
008500921023     C                   DO        *HIVAL
008600990913     C*
008700990915     C                   READ      tivin00r                               70
008800040910     C                   if        vindta > *blanks
008900000613     C                   add       1             rrnum
009000000801     C*
009100000801     C                   if        *in70 = *off
009200000801     C                             and
009300000801     C                             (vinflg = *blanks
009400000801     C                              or vinflg = '0'
009500000801     C                              or vinflg = '2')
009600000801     C*
009700000801     C                   clear                   vinmsg
009800000801     C                   eval      vinflg = '1'
009900040910     C*
010000040910     C* Eseguo routine d traduzione
010100040910     C                   exsr      impvabvat
010200040802     C*
010300010305     C                   endif
010400000905     C*
010500000905     C                   else
010600000905     C                   eval      vinflg = '1'
010700000905     C                   endif
010800000905     C*
010900000905     C  N70              update    tivin000
011000000905     C*
011100991022     C  N70              ENDdo
011200010202     C*
011300010202     C                   endif
011400990910
011500990910     C* Se non ci sono record con errori ...
011600000710     C                   if        §ctrno = 0
011700990910     C* ... restituisco esito OK.
011800990921     C                   eval      wrkesito = '0'
011900990910     C                   else
012000010201     C                   if        §ctrokvb > 0
012100990921     C                   eval      wrkesito = '1'
012200000710     C                   else
012300000710     C                   eval      wrkesito = '2'
012400990910     C                   endif
012500000710     C                   endif
012600990910     C*
012700990914     C                   if        %open(tivin00r)
012800990908     C                   close     tivin00r
012900990914     C                   endif
013000021113     C                   if        %open(fivabwwr)
013100021113     C                   close     fivabwwr
013200990914     C                   endif
013300021113     C                   if        %open(fivatwwr)
013400021113     C                   close     fivatwwr
013500010201     C                   endif
013600990910     C*
013700010201     C                   if        §ctrokvb > 0
013800000724     C                             and vlrpoi <> *zeros
013900010202     C                   exsr      invio
014000990920     C                   endif
014100990920     C*
014200910830     C                   ENDSR
014300000613     C***
014400990920
014500000801     C*----------------------------------------------------*
014600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014700000801     C*----------------------------------------------------*
014800010201     C     INZVAR        BEGSR
014900000801     C*
015000040802     C                   Z-ADD     *zeros        Num5_0            5 0
015100040802     C                   MOVEL     '0'           FlgCAS            1
015200000801     C*
015300000801     C                   ENDSR
015400000801     C*----------------------------------------------------*
015500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
015600000801     C*----------------------------------------------------*
015700000801     C     DEFCAM        BEGSR
015800000801     C*
015900021113     C                   CLEAR                   FIVAB000
016000040802     C                   CLEAR                   FIVAT000
016100020619     C* Imposto i valori di default...
016200090922     C                   Z-ADD     0503045       VABCCM
016300090922     C                   Z-ADD     0503045       VATCCM
016400060202     C                   Z-ADD     050           VABLNP
016500060202     C                   Z-ADD     050           VATLNP
016600090922     C                   Z-ADD     002           VABCTR
016700070531     C                   MOVEL     '7Q'          VABCTM
016800040823     C                   MOVEL     '1'           VABCBO
016900020619     C* ... e poi verifico se sono stati passati come parametri
017000020619     C                   IF        vlrppt > *blanks
017100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017300020619     C                   EXSR      CHKNUM
017400020619     C                   IF        PiInt=*on
017500020619     C                   Z-ADD     PiVal         VABCCM
017600020619     C                   Z-ADD     PiVal         VATCCM
017700020619     C                   ENDIF
017800040506     C                   ENDIF
017900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018100020619     C                   EXSR      CHKNUM
018200020619     C                   IF        PiInt=*on
018300020619     C                   Z-ADD     PiVal         VABLNP
018400020619     C                   Z-ADD     PiVal         VATLNP
018500040506     C                   ENDIF
018600020619     C                   ENDIF
018700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
018800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
018900020619     C                   EXSR      CHKNUM
019000020619     C                   IF        PiInt=*on
019100020619     C                   Z-ADD     PiVal         VABCTR
019200040506     C                   ENDIF
019300020619     C                   ENDIF
019400060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
019500060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
019600060202     C                   ENDIF
019700020619     C                   ENDIF
019800000801     C*
019900000801     C                   ENDSR
020000000801     C*----------------------------------------------------*
020100040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020200000801     C*----------------------------------------------------*
020300040910     C     IMPVABVAT     BEGSR
020400040910     C*
020500040910     C* Traduzione relativa ai tipi record del file del cliente
020600040910     C*
020700040910     C***
020800060202     C* ...tipo record 'TS'
020900060202     C                   IF        %subst(vindta:1:2) = 'TS'
021000040910     C* ......inizializzazioni iniziali e formati record file Bartolini
021100040910     C                   EXSR      INZVAR
021200040910     C                   EXSR      DEFCAM
021300040928     C*
021400040928     C                   Z-ADD     *zeros        errore            1 0
021500040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
021600040928     C                   MOVEL     datcor        VABAAS
021700040928     C                   MOVEL     datcor        VATAAS
021800040928     C                   MOVE      datcor        VABMGS
021900040928     C                   MOVE(P)   vlrpoi        VABFGS
022000040928     C                   MOVE(P)   vlrpoi        VATFGS
022100040910     C* ......VABNSP/VATNSP
022200041025     C* NSP => Stacco un numeratore da AZNUM
022300041025     C                   clear                   TRUL33DS
022400041025     C                   eval      I33OPE = *zeros
022500041025     C                   eval      I33CNU = 302
022600041025     C                   eval      I33NUM = 1
022700041025     C                   movel     TRUL33DS      KPJBU
022800041025     C                   call      'TRUL33R'
022900041025     C                   parm                    KPJBA
023000041025     C                   movel     KPJBU         TRUL33DS
023100041025     C                   if        O33ERR = *zeros
023200041025     C                   z-add     O33NRF        VABNSP
023300041025     C                   z-add     O33NRF        VATNSP
023400041025     C                   else
023500041025     C                   Z-ADD     1             errore
023600041025     C                   EVAL      vinmsg = %trimr(vinmsg)
023700041025     C                             + ' ' + 'VABNSP VATNSP'
023800041025     C                   endif
023900060202     C* ......VABRMA
024000060202     C                   EVAL      VABRMA=%trim(%subst(vindta:3:20))
024100060202     C* ......VABRMN
024200060202     C                   EVAL      PiStr=%trim(%subst(vindta:41:10))
024300060202     C                   EXSR      CHKNUM
024400060202     C                   IF        PiInt=*on
024500060202     C                   Z-ADD     PiVal         VABRMN
024600060202     C                   ELSE
024700060202     C                   ADD       1             errore
024800060202     C                   EVAL      vinmsg = %trimr(vinmsg)
024900060202     C                             + ' ' + 'VABRMN'
025000060202     C                   ENDIF
025100060202     C* ......VABNCL
025200060202     C                   EVAL      PiStr=%trim(%subst(vindta:59:15))
025300060202     C                   EXSR      CHKNUM
025400060202     C                   IF        PiInt=*on
025500060202     C                   Z-ADD     PiVal         VABNCL
025600060202     C                   ELSE
025700060202     C                   ADD       1             errore
025800060202     C                   EVAL      vinmsg = %trimr(vinmsg)
025900060202     C                             + ' ' + 'VABNCL'
026000060202     C                   ENDIF
026100060202     C* ......VABPKB
026200060202     C                   EVAL      PiStr=%trim(%subst(vindta:74:15))
026300060202     C                   EXSR      CHKNUM
026400060202     C                   IF        PiNum=*on
026500070108     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
026600070108     C                   Z-ADD(H)  PiVal         VABPKB
026700060202     C                   ELSE
026800060202     C                   ADD       1             errore
026900060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027000060202     C                             + ' ' + 'VABPKB'
027100060202     C                   ENDIF
027200060202     C* ......VABVLB
027300060202     C                   EVAL      PiStr=%trim(%subst(vindta:89:15))
027400060202     C                   EXSR      CHKNUM
027500060202     C                   IF        PiNum=*on
027600070108     C                   EVAL      PiVal=PiVal/100000                           * gestisco 5 dec.
027700070108     C                   Z-ADD(H)  PiVal         VABVLB
027800060202     C                   ELSE
027900060202     C                   ADD       1             errore
028000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028100060202     C                             + ' ' + 'VABVLB'
028200060202     C                   ENDIF
028300060202     C* ......VABRSD
028400060202     C                   EVAL      VABRSD=%trim(%subst(vindta:104:35))
028500060202     C* ......VABIND
028600060202     C                   EVAL      VABIND=%trim(%subst(vindta:139:35))
028700060202     C* ......VABLOD
028800060202     C                   EVAL      VABLOD=%trim(%subst(vindta:174:35))
028900060202     C* ......VABCAD
029000060202     C                   EVAL      PiStr=%trim(%subst(vindta:209:6))
029100060202     C                   EXSR      CHKNUM
029200060202     C                   IF        PiInt=*on
029300060202     C                   Z-ADD     PiVal         Num5_0
029400060202     C                   MOVEL(P)  Num5_0        VABCAD
029500060202     C                   ELSE
029600060202     C                   ADD       1             errore
029700060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029800060202     C                             + ' ' + 'VABCAD'
029900060202     C                   ENDIF
030000060202     C* ......VABPRD
030100060202     C                   EVAL      VABPRD=%trim(%subst(vindta:215:2))
030200060202     C* ......VABCAS
030300060202     C                   IF        %subst(vindta:217:15) <> *zeros
030400060202     C                   EVAL      FlgCAS = '1'
030500060202     C                   EVAL      PiStr=%trim(%subst(vindta:217:15))
030600060202     C                   EXSR      CHKNUM
030700060202     C                   IF        PiNum=*on
030800060202     C                   EVAL      VABCAS=PiVal/1000                            * gestisco 3 dec.
030900060202     C                   ELSE
031000060202     C                   ADD       1             errore
031100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
031200060202     C                             + ' ' + 'VABCAS'
031300060202     C                   ENDIF
031400060202     C                   ENDIF
031500060202     C* ......VABVCA
031600060202     C                   EVAL      VABVCA=%trim(%subst(vindta:232:3))
031700100513     C                   IF        VABVCA=*blanks
031800100513     C                   EVAL      VABVCA='EUR'
031900100513     C                   ENDIF
032000060202     C* ......VABNOT
032100060202     C                   EVAL      VABNOT=%trim(%subst(vindta:235:30))
032200060202     C* ......VABNT2
032300060202     C                   EVAL      VABNT2=%trim(%subst(vindta:265:30))
032400060202     C* ......VABRMO
032500060202     C                   EVAL      VABRMO=%trim(%subst(vindta:295:30))
032600060202     C* ......VABDCR
032700060202     C                   IF        %subst(vindta:325:8) <> *zeros
032800060202     C                   EVAL      PiStr=%trim(%subst(vindta:325:8))
032900060202     C                   EXSR      CHKNUM
033000060202     C                   IF        PiInt=*on
033100060202     C                   Z-ADD     PiVal         VABDCR
033200060202     C                   ELSE
033300060202     C                   ADD       1             errore
033400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
033500060202     C                             + ' ' + 'VABDCR'
033600060202     C                   ENDIF
033700060202     C                   ENDIF
033800060202     C* ......VABTIC
033900060202     C                   EVAL      VABTIC=%trim(%subst(vindta:337:2))
034000060202     C* ......VABTC1
034100060202     C                   EVAL      VABTC1=%trim(%subst(vindta:339:1))
034200060202     C*
034300060202     C* Considerazioni particolari x identificare il cliente ed il traffico specifico
034400060202     C                   IF        %subst(vindta:344:3) = 'PRE'
034500060202     C                   EVAL      VABCCM = 0501158
034600070530     C                   EVAL      VATCCM = 0501158
034700070530     C                   EVAL      VABLNP = 050
034800070530     C                   EVAL      VATLNP = 050
034900060202     C                   EVAL      VABCTR = 100
035000060202     C                   EVAL      VABCTM = '7Q'
035100061013     C                   EVAL      VABFGS = 050
035200061013     C                   EVAL      VATFGS = 050
035300060202     C                   ENDIF
035400060202     C*
035500060202     C                   IF        %subst(vindta:344:3) = 'ALC'
035600090922     C                   EVAL      VABCCM = 0503045
035700090922     C                   EVAL      VATCCM = 0503045
035800060202     C                   EVAL      VABLNP = 050
035900070530     C                   EVAL      VATLNP = 050
036000090922     C                   EVAL      VABCTR = 002
036100060202     C                   EVAL      VABCTM = '7Q'
036200060206     C                   EVAL      VABFGS = 050
036300060206     C                   EVAL      VATFGS = 050
036400060202     C                   ENDIF
036500060202     C*
036600090914     C                   IF        %subst(vindta:344:3) = 'CG3' AND
036700090914     C                             %subst(vindta:341:3) = '340'
036800060419     C                   EVAL      VABCCM = 0503050
036900070531     C                   EVAL      VATCCM = 0503050
037000060419     C                   EVAL      VABLNP = 050
037100070531     C                   EVAL      VATLNP = 050
037200060206     C                   EVAL      VABCTR = 001
037300070531     C                   EVAL      VABCTM = '7Q'
037400060419     C                   EVAL      VABFGS = 050
037500070531     C                   EVAL      VATFGS = 050
037600060206     C                   ENDIF
037700070524     C*
037800070524     C                   IF        %subst(vindta:344:3) = 'GEM'
037900070524     C                   EVAL      VABCCM = 1300315
038000070530     C                   EVAL      VATCCM = 1300315
038100070524     C                   EVAL      VABLNP = 050
038200070530     C                   EVAL      VATLNP = 050
038300070524     C                   EVAL      VABCTM = '7Q'
038400070524     C                   EVAL      VABCTR = 000
038500070524     C                   EVAL      VABFGS = 050
038600070524     C                   EVAL      VATFGS = 050
038700070524     C                   ENDIF
038800070524     C*
038900070524     C                   IF        %subst(vindta:344:3) = 'SUB'
039000070627     C                   EVAL      VABCCM = 1050462
039100070627     C                   EVAL      VATCCM = 1050462
039200070531     C                   EVAL      VABLNP = 050
039300070531     C                   EVAL      VATLNP = 050
039400070524     C                   EVAL      VABCTM = '7Q'
039500070524     C                   EVAL      VABCTR = 000
039600070531     C                   EVAL      VABFGS = 050
039700070531     C                   EVAL      VATFGS = 050
039800070524     C                   ENDIF
039900090914     C*
040000090914     C                   IF        %subst(vindta:344:3) = 'EPS' AND
040100090914     C                             %subst(vindta:341:3) = '340'
040200091204     C                   EVAL      VABCCM = 0503934
040300091204     C                   EVAL      VATCCM = 0503934
040400070615     C                   EVAL      VABLNP = 050
040500070615     C                   EVAL      VATLNP = 050
040600070615     C                   EVAL      VABCTM = '7Q'
040700091204     C                   EVAL      VABCTR = 100
040800070615     C                   EVAL      VABFGS = 050
040900070615     C                   EVAL      VATFGS = 050
041000070615     C                   ENDIF
041100090914     C*
041200090914     C                   IF        %subst(vindta:344:3) = 'THQ' AND
041300090914     C                             %subst(vindta:341:3) = *blanks
041400090914     C                   EVAL      VABCCM = 0504051
041500090914     C                   EVAL      VATCCM = 0504051
041600090914     C                   EVAL      VABLNP = 050
041700090914     C                   EVAL      VATLNP = 050
041800090914     C                   EVAL      VABCTM = '7Q'
041900090914     C                   EVAL      VABCTR = 001
042000090914     C                   EVAL      VABFGS = 050
042100090914     C                   EVAL      VATFGS = 050
042200090914     C                   ENDIF
042300070627     C*
042400090914     C                   IF        %subst(vindta:344:3) = 'THQ' AND
042500090914     C                             %subst(vindta:341:3) = '340'
042600070627     C                   EVAL      VABCCM = 0503566
042700070627     C                   EVAL      VATCCM = 0503566
042800070627     C                   EVAL      VABLNP = 050
042900070627     C                   EVAL      VATLNP = 050
043000070627     C                   EVAL      VABCTM = '7Q'
043100070627     C                   EVAL      VABCTR = 001
043200070627     C                   EVAL      VABFGS = 050
043300070627     C                   EVAL      VATFGS = 050
043400070627     C                   ENDIF
043500070918     C*
043600090914     C                   IF        %subst(vindta:344:3) = 'DXX' AND
043700090914     C                             %subst(vindta:341:3) = '340'
043800090922     C                   EVAL      VABCCM = 0504363
043900090922     C                   EVAL      VATCCM = 0504363
044000070918     C                   EVAL      VABLNP = 050
044100070918     C                   EVAL      VATLNP = 050
044200070918     C                   EVAL      VABCTM = '7Q'
044300070918     C                   EVAL      VABCTR = 002
044400070918     C                   EVAL      VABFGS = 050
044500070918     C                   EVAL      VATFGS = 050
044600070918     C                   ENDIF
044700080505     C*
044800090922     C                   IF        %subst(vindta:344:3) = 'SHR' AND
044900090922     C                             %subst(vindta:341:3) = '340'
045000080505     C                   EVAL      VABCCM = 0503925
045100080505     C                   EVAL      VATCCM = 0503925
045200080505     C                   EVAL      VABLNP = 050
045300080505     C                   EVAL      VATLNP = 050
045400080505     C                   EVAL      VABCTM = '7Q'
045500080505     C                   EVAL      VABCTR = 002
045600080505     C                   EVAL      VABFGS = 050
045700080505     C                   EVAL      VATFGS = 050
045800080505     C                   ENDIF
045900080508     C*
046000090914     C                   IF        %subst(vindta:344:3) = 'EPH' AND
046100090914     C                             %subst(vindta:341:3) = '340'
046200080508     C                   EVAL      VABCCM = 0503934
046300080508     C                   EVAL      VATCCM = 0503934
046400080508     C                   EVAL      VABLNP = 050
046500080508     C                   EVAL      VATLNP = 050
046600080508     C                   EVAL      VABCTM = '7Q'
046700080508     C                   EVAL      VABCTR = 100
046800080508     C                   EVAL      VABFGS = 050
046900080508     C                   EVAL      VATFGS = 050
047000080508     C                   ENDIF
047100090914     C*
047200090914     C                   IF        %subst(vindta:344:3) = 'EPH' AND
047300090914     C                             %subst(vindta:341:3) = 'HUB'
047400090914     C                   EVAL      VABCCM = 0303049
047500090914     C                   EVAL      VATCCM = 0303049
047600090914     C                   EVAL      VABLNP = 050
047700090914     C                   EVAL      VATLNP = 050
047800090914     C                   EVAL      VABCTM = '7Q'
047900090922     C                   EVAL      VABCTR = 001
048000090914     C                   EVAL      VABFGS = 050
048100090914     C                   EVAL      VATFGS = 050
048200090914     C                   ENDIF
048300080611     C*
048400081126     C                   IF        %subst(vindta:344:3) = 'ADD'
048500081126     C                   EVAL      VABCCM = 0503045
048600081126     C                   EVAL      VATCCM = 0503045
048700081126     C                   EVAL      VABLNP = 050
048800081126     C                   EVAL      VATLNP = 050
048900081126     C                   EVAL      VABCTM = '7Q'
049000090922     C                   EVAL      VABCTR = 002
049100081126     C                   EVAL      VABFGS = 050
049200081126     C                   EVAL      VATFGS = 050
049300081126     C                   ENDIF
049400090204     C*
049500090204     C                   IF        %subst(vindta:344:3) = 'AUD'
049600090204     C                   EVAL      VABCCM = 1301279
049700090204     C                   EVAL      VATCCM = 1301279
049800090204     C                   EVAL      VABLNP = 130
049900090204     C                   EVAL      VATLNP = 130
050000090204     C                   EVAL      VABCTM = '7Q'
050100090204     C                   EVAL      VABCTR = 000
050200090204     C                   EVAL      VABFGS = 130
050300090204     C                   EVAL      VATFGS = 130
050400090204     C                   ENDIF
050500090703     C*
050600090922     C                   IF        %subst(vindta:344:3) = 'F68' AND
050700090922     C                             %subst(vindta:341:3) = '340'
050800090703     C                   EVAL      VABCCM = 0504440
050900090703     C                   EVAL      VATCCM = 0504440
051000090703     C                   EVAL      VABLNP = 050
051100090703     C                   EVAL      VATLNP = 050
051200090703     C                   EVAL      VABCTM = '7Q'
051300090922     C                   EVAL      VABCTR = 002
051400090703     C                   EVAL      VABFGS = 050
051500090703     C                   EVAL      VATFGS = 050
051600090703     C                   ENDIF
051700090914     C*
051800090914     C                   IF        %subst(vindta:344:3) = 'EPR' AND
051900090914     C                             %subst(vindta:341:3) = '340'
052000091204     C                   EVAL      VABCCM = 0503934
052100091204     C                   EVAL      VATCCM = 0503934
052200090914     C                   EVAL      VABLNP = 050
052300090914     C                   EVAL      VATLNP = 050
052400090914     C                   EVAL      VABCTM = '7Q'
052500091204     C                   EVAL      VABCTR = 100
052600090914     C                   EVAL      VABFGS = 050
052700090914     C                   EVAL      VATFGS = 050
052800090914     C                   ENDIF
052900090914     C*
053000090914     C                   IF        %subst(vindta:344:3) = 'VIT' AND
053100090914     C                             %subst(vindta:341:3) = '340'
053200090922     C                   EVAL      VABCCM = 0432896
053300090922     C                   EVAL      VATCCM = 0432896
053400090914     C                   EVAL      VABLNP = 050
053500090914     C                   EVAL      VATLNP = 050
053600090914     C                   EVAL      VABCTM = '7Q'
053700090922     C                   EVAL      VABCTR = 000
053800090914     C                   EVAL      VABFGS = 050
053900090914     C                   EVAL      VATFGS = 050
054000090914     C                   ENDIF
054100090914     C*
054200090914     C                   IF        %subst(vindta:344:3) = '   ' AND
054300090914     C                             %subst(vindta:341:3) = '340'
054400090914     C                   EVAL      VABCCM = 0503045
054500090914     C                   EVAL      VATCCM = 0503045
054600090914     C                   EVAL      VABLNP = 050
054700090914     C                   EVAL      VATLNP = 050
054800090914     C                   EVAL      VABCTM = '7Q'
054900090922     C                   EVAL      VABCTR = 002
055000090914     C                   EVAL      VABFGS = 050
055100090914     C                   EVAL      VATFGS = 050
055200090914     C                   ENDIF
055300090914     C*
055400090914     C                   IF        %subst(vindta:344:3) = 'YAM'
055500090922     C                   EVAL      VABCCM = 2022096
055600090922     C                   EVAL      VATCCM = 2022096
055700090914     C                   EVAL      VABLNP = 050
055800090914     C                   EVAL      VATLNP = 050
055900090914     C                   EVAL      VABCTM = '7Q'
056000090922     C                   EVAL      VABCTR = 000
056100090914     C                   EVAL      VABFGS = 050
056200090914     C                   EVAL      VATFGS = 050
056300090914     C                   ENDIF
056400090922     C*
056500090922     C                   IF        %subst(vindta:344:3) = 'IMO'
056600090922     C                   EVAL      VABCCM = 0503045
056700090922     C                   EVAL      VATCCM = 0503045
056800090922     C                   EVAL      VABLNP = 050
056900090922     C                   EVAL      VATLNP = 050
057000090922     C                   EVAL      VABCTM = '7Q'
057100090922     C                   EVAL      VABCTR = 002
057200090922     C                   EVAL      VABFGS = 050
057300090922     C                   EVAL      VATFGS = 050
057400090922     C                   ENDIF
057500091119     C*
057600091119     C                   IF        %subst(vindta:344:3) = 'EPS' AND
057700091119     C                             %subst(vindta:341:3) = 'HUB'
057800091119     C                   EVAL      VABCCM = 0503049
057900091119     C                   EVAL      VATCCM = 0503049
058000091119     C                   EVAL      VABLNP = 050
058100091119     C                   EVAL      VATLNP = 050
058200091119     C                   EVAL      VABCTM = '7Q'
058300091119     C                   EVAL      VABCTR = 001
058400091119     C                   EVAL      VABFGS = 050
058500091119     C                   EVAL      VATFGS = 050
058600091119     C                   ENDIF
058700091119     C*
058800091119     C                   IF        %subst(vindta:344:3) = 'EPR' AND
058900091119     C                             %subst(vindta:341:3) = 'HUB'
059000091119     C                   EVAL      VABCCM = 0503049
059100091119     C                   EVAL      VATCCM = 0503049
059200091119     C                   EVAL      VABLNP = 050
059300091119     C                   EVAL      VATLNP = 050
059400091119     C                   EVAL      VABCTM = '7Q'
059500091119     C                   EVAL      VABCTR = 001
059600091119     C                   EVAL      VABFGS = 050
059700091119     C                   EVAL      VATFGS = 050
059800091119     C                   ENDIF
059900091119     C*
060000091119     C                   IF        %subst(vindta:344:3) = 'EPB' AND
060100091119     C                             %subst(vindta:341:3) = 'HUB'
060200091119     C                   EVAL      VABCCM = 0503049
060300091119     C                   EVAL      VATCCM = 0503049
060400091119     C                   EVAL      VABLNP = 050
060500091119     C                   EVAL      VATLNP = 050
060600091119     C                   EVAL      VABCTM = '7Q'
060700091119     C                   EVAL      VABCTR = 001
060800091119     C                   EVAL      VABFGS = 050
060900091119     C                   EVAL      VATFGS = 050
061000091119     C                   ENDIF
061100091119     C*
061200091119     C                   IF        %subst(vindta:344:3) = 'EPB' AND
061300091119     C                             %subst(vindta:341:3) = '340'
061400091119     C                   EVAL      VABCCM = 0503934
061500091119     C                   EVAL      VATCCM = 0503934
061600091119     C                   EVAL      VABLNP = 050
061700091119     C                   EVAL      VATLNP = 050
061800091119     C                   EVAL      VABCTM = '7Q'
061900091119     C                   EVAL      VABCTR = 100
062000091119     C                   EVAL      VABFGS = 050
062100091119     C                   EVAL      VATFGS = 050
062200091119     C                   ENDIF
062300100329     C*
062400100329     C                   IF        %subst(vindta:344:3) = 'PRE' AND
062500100329     C                             %subst(vindta:341:3) = '34B'
062600100329     C                   EVAL      VABCCM = 0504077
062700100329     C                   EVAL      VATCCM = 0504077
062800100329     C                   EVAL      VABLNP = 050
062900100329     C                   EVAL      VATLNP = 050
063000100329     C                   EVAL      VABCTM = '7Q'
063100100329     C                   EVAL      VABCTR = 000
063200100329     C                   EVAL      VABFGS = 050
063300100329     C                   EVAL      VATFGS = 050
063400100329     C                   ENDIF
063500101026     C*
063600101026     C                   IF        %subst(vindta:344:3) = 'BNJ' AND
063700101026     C                             %subst(vindta:341:3) = '340'
063800101026     C                   EVAL      VABCCM = 0504842
063900101026     C                   EVAL      VATCCM = 0504842
064000101026     C                   EVAL      VABLNP = 050
064100101026     C                   EVAL      VATLNP = 050
064200101026     C                   EVAL      VABCTM = '7Q'
064300101026     C                   EVAL      VABCTR = 000
064400101026     C                   EVAL      VABFGS = 050
064500101026     C                   EVAL      VATFGS = 050
064600101026     C                   ENDIF
064700150511     C*
064800150511     C                   IF        %subst(vindta:344:3) = 'DEL' AND
064900150511     C                             %subst(vindta:341:3) = '34A'
065000150511     C                   EVAL      VABCCM = 0506520
065100150511     C                   EVAL      VATCCM = 0506520
065200150511     C                   EVAL      VABLNP = 050
065300150511     C                   EVAL      VATLNP = 050
065400150511     C                   EVAL      VABCTR = 102
065500150511     C                   EVAL      VABCTM = '7Q'
065600150511     C                   EVAL      VABFGS = 050
065700150511     C                   EVAL      VATFGS = 050
065800150511     C                   ENDIF
065900150511     C*
066000150511     C                   IF        %subst(vindta:344:3) = 'DEL' AND
066100150511     C                             %subst(vindta:341:3) = '34E'
066200150511     C                   EVAL      VABCCM = 0506520
066300150511     C                   EVAL      VATCCM = 0506520
066400150511     C                   EVAL      VABLNP = 050
066500150511     C                   EVAL      VATLNP = 050
066600150511     C                   EVAL      VABCTR = 390
066700150511     C                   EVAL      VABCTM = '7Q'
066800150511     C                   EVAL      VABFGS = 050
066900150511     C                   EVAL      VATFGS = 050
067000150511     C                   ENDIF
067100060202     C*
067200060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
067300060202     C                   IF        FlgCAS <> '0'
067400060202     C                   IF        VABCBO = '1'
067500060202     C                   EVAL      VABCBO = '4'
067600060202     C                   ELSE
067700060202     C                   EVAL      VABCBO = '6'
067800060202     C                   ENDIF
067900060202     C                   ENDIF
068000060202     C*
068100060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
068200060202     C                   EXSR      CHKIMPDIV
068300060202     C* ...e in fine...
068400060202     C                   WRITE     FIVAB000
068500040910     C*
068600040910     C                   ENDIF
068700040910     C***
068800060202     C* ...tipo record 'RS'
068900060202     C                   IF        %subst(vindta:1:2) = 'RS'
069000040910     C* ......VATNOT
069100060202     C                   EVAL      VATNOT=%trim(%subst(vindta:43:20))
069200040910     C                   EVAL      VATTRC = 'E'
069300060202     C* x ogni tipo record 'RS' devo scaricare il buffer del file FIVAT
069400060202     C                   IF        VATNOT <> *blanks
069500040910     C                   WRITE     FIVAT000
069600060202     C                   ENDIF
069700040910     C*
069800040910     C                   ENDIF
069900010202     C*
070000000801     C* Ebbene...
070100000801     C                   ADD       1             §CTRMO
070200010201     C                   IF        errore <> *zeros
070300000801     C                   ADD       1             §CTRNO
070400000801     C                   EVAL      vinflg = '2'
070500000801     C                   ELSE
070600010201     C                   ADD       1             §CTROKVB
070700000801     C                   ENDIF
070800000801     C*
070900000801     C                   ENDSR
071000010202     C*----------------------------------------------------*
071100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
071200010202     C*----------------------------------------------------*
071300020305     C     PREVAT        BEGSR
071400010202     C*
071500021113     C* Compongo il nome del membro da dare al FIVATWWR
071600010202     C                   eval      parmbr = vlrhdl
071700010202     C                   movel     'M'           parmbr
071800060113     C                   eval      parccm = vlrksc
071900010202     C                   eval      paropz = '1'
072000010202     C* Effettuo la chiamata al CLLE preposto
072100040506     C                   call(e)   'TITVVTC'
072200010202     C                   parm                    parccm
072300010202     C                   parm                    parmbr
072400010202     C                   parm                    paropz
072500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072600010202     C                   if        %error
072700010202     C                   movel     '1'           chkcall
072800010202     C                   else
072900010202     C                   movel     '0'           chkcall
073000010202     C                   endif
073100010202     C*
073200010202     C                   ENDSR
073300000801     C*----------------------------------------------------*
073400000801     C*  CONTROLLO NUMERICITA' CAMPI
073500000801     C*----------------------------------------------------*
073600000801     C     CHKNUM        BEGSR
073700000801     C*
073800000801     C                   call(e)   'ISNUMERIC'
073900000801     C                   PARM                    PiStr            30
074000040714     C                   PARM      ','           PiDecChr          1
074100000801     C                   PARM      *ZEROS        PiVal            30 9
074200000801     C                   PARM      '0'           PiInt             1
074300000801     C                   PARM      '0'           PiNum             1
074400000801     C                   IF        %error
074500000801     C                   EVAL      PiInt=*off
074600000801     C                   ENDIF
074700000801     C*
074800000801     C                   ENDSR
074900000801     C***
075000000801
075100011113
075200011113     C*----------------------------------------------------*
075300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
075400011113     C*----------------------------------------------------*
075500011113     C     CHKIMPDIV     BEGSR
075600011113     C*
075700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
075800011113     C                   Z-ADD     *zeros        wrkDec            9 9
075900011113     C*
076000011113     C* Come prima cosa effettuo considerazioni sulla divisa
076100011113     C                   IF        vabIAS > *zeros
076200011113     C                   IF        vabVAS <> 'EUR'
076300011113     C                   EVAL      vabVAS =  'ITL'
076400011113     C                   ENDIF
076500011113     C                   ENDIF
076600011113     C*
076700011113     C                   IF        vabCAS > *zeros
076800011113     C                   IF        vabVCA <> 'EUR'
076900011113     C                   EVAL      vabVCA =  'ITL'
077000011113     C                   ENDIF
077100011113     C                   ENDIF
077200011113     C*
077300011113     C                   IF        vabVMD > *zeros
077400020305     C                   IF        vabVAD <> 'EUR'
077500011113     C                   EVAL      vabVAD =  'ITL'
077600011113     C                   ENDIF
077700011113     C                   ENDIF
077800011113     C*
077900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
078000011113     C                   Z-ADD     vabIAS        wrkDec
078100011113     C                   IF        wrkDec > *zeros
078200011113     C                   IF        vabVAS = 'ITL'
078300011113     C                   EVAL      vabIAS = *zeros
078400011113     C                   ENDIF
078500011113     C                   ENDIF
078600011113     C*
078700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
078800011113     C                   Z-ADD     vabCAS        wrkDec
078900011113     C                   IF        wrkDec > *zeros
079000011113     C                   IF        vabVCA = 'ITL'
079100011113     C                   EVAL      vabCAS = *zeros
079200011113     C                   ENDIF
079300011113     C                   ENDIF
079400011113     C*
079500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
079600011113     C                   Z-ADD     vabVMD        wrkDec
079700011113     C                   IF        wrkDec > *zeros
079800011113     C                   IF        vabVAD = 'ITL'
079900011113     C                   EVAL      vabVMD = *zeros
080000011113     C                   ENDIF
080100011113     C                   ENDIF
080200011113     C*
080300011113     C                   ENDSR
080400011113     C***
080500011113
080600011113
080700000801
080800000801
080900990920      /TITLE Invio dei dati al punto operativo.
081000010202     C     invio         BEGSR
081100990920     C*
081200021113     C* 1° invio FIVAT
081300010201     C                   reset                   dscmz
081400010201     C                   move      vlrpoi        cmzdst
081500021113     C                   eval      cmzfld = 'FIVATWWR'
081600010201     C                   eval      cmzmbd = vlrhdl
081700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081800021009     C***                if        prmfir = *blanks
081900021113     C                   eval      cmzfla = 'FIVAT00F'
082000021113     C                   eval      cmzmba = 'FIVAT00F'
082100021009     C***                else
082200021009     C***                eval      cmzfla = prmfir
082300021009     C***                eval      cmzmba = prmfir
082400021009     C***                endif
082500010201     C                   eval      cmznrr = *zeros
082600020305     C                   move      §ctrokvt      cmznrr
082700021018     C                   eval      cmzlba = vlrfl1
082800010201     C                   call(e)   'TIS711C'
082900010201     C                   parm                    dscmz
083000010201     C                   parm      *blanks       esito
083100010205     C                   if        %error
083200010205     C                             or cmzerr = '1'
083300010205     C                             or esito  = '1'
083400010205     C                   eval      wrkesito = '3'
083500010205     C                   else
083600010201     C*
083700021113     C* 2° invio FIVAB
083800010201     C                   reset                   dscmz
083900010201     C                   move      vlrpoi        cmzdst
084000010201     C                   eval      cmzfld = vlrfou
084100010201     C                   eval      cmzmbd = vlrhdl
084200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
084300021009     C***                if        prmfir = *blanks
084400021113     C                   eval      cmzfla = 'FIVAB00F'
084500021113     C                   eval      cmzmba = 'FIVAB00F'
084600021009     C***                else
084700021009     C***                eval      cmzfla = prmfir
084800021009     C***                eval      cmzmba = prmfir
084900021009     C***                endif
085000010201     C                   eval      cmznrr = *zeros
085100010201     C                   move      §ctrokvb      cmznrr
085200021018     C                   eval      cmzlba = vlrfl1
085300010201     C                   call(e)   'TIS711C'
085400010201     C                   parm                    dscmz
085500010201     C                   parm      *blanks       esito
085600010201     C                   if        %error
085700010201     C                             or cmzerr = '1'
085800010201     C                             or esito  = '1'
085900010201     C                   eval      wrkesito = '3'
086000010201     C                   endif
086100010205     C                   endif
086200990920     C*
086300000613     C                   ENDSR
086400000613     C***
086500070411
086600070411     C     *pssr         BEGSR
086700070411     C*
086800070411     C                   if        %open(tivin00r)
086900070411     C                   close     tivin00r
087000070411     C                   endif
087100070411     C                   if        %open(fivabwwr)
087200070411     C                   close     fivabwwr
087300070411     C                   endif
087400070411     C                   if        %open(fivatwwr)
087500070411     C                   close     fivatwwr
087600070411     C                   endif
087700070411     C*
087800070411     C* Effettuo la chiamata al CLLE preposto
087900070411     C                   call(e)   'TITVVTC'
088000070411     C                   parm                    parccm
088100070411     C                   parm                    parmbr
088200070411     C                   parm      '2'           paropz
088300070411     C*
088400070411     C                   eval      wrkesito = '2'
088500070411     C*
088600070411     C                   seton                                        LR
088700070411     C*
088800070411     C                   ENDSR     '*CANCL'
088900070411     C***
089000070411
089100990910
089200000613     C     *inzsr        BEGSR
089300990910     C*
089400990910     C     *entry        plist
089500990920     C                   parm                    tivlrds
089600990921     C                   parm      wrkesito      esito
089700000724     C                   parm                    prmlit
089800000710     C                   parm                    prmfir
089900000613     C*
090000000830     C* CALCOLA LA DATA CORRENTE
090100000830     C                   time                    wn14             14 0
090200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
090300000830     C                   z-add     wn8           g08dat
090400000830     C                   z-add     *zeros        g08inv
090500000830     C                   movel     '0'           g08err
090600000830     C                   call      'XSRDA8'
090700000830     C                   parm                    wlbda8
090800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
090900000830     C*
091000000613     C                   ENDSR
091100000613     C***
