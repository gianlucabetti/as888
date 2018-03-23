000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130524     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002400071206     D wGiro           s              1    INZ('0')
002500000830
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400041025     D*------------------
003500041025     D* DS REPERIMENTO NUMERATORE
003600041025     D*------------------
003700041025     D trul33ds      e ds                  inz
003800041025     D*------------------
003900041025     D* DS ARCHITETTURA
004000041025     D*------------------
004100041025     D kpjba         e ds                  inz
004200041025     D*------------------
004300990908
004400010201
004500010201
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900000613     C*
005000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300040506     C                   call(e)   'TITVVTC'
005400010202     C                   parm                    parccm
005500010202     C                   parm                    parmbr
005600010202     C                   parm      '2'           paropz
005700000616     C*
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900021113     C                   if        not %open(fivabwwr)
007000021113     C                   open      fivabwwr
007100990914     C                   endif
007200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700021113     C                   if        not %open(fivatwwr)
007800021113     C                   open      fivatwwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500040910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900040910     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000040910     C*
010100040910     C* Eseguo routine d traduzione
010200040910     C                   exsr      impvabvat
010300040802     C*
010400010305     C                   endif
010500000905     C*
010600000905     C                   else
010700000905     C                   eval      vinflg = '1'
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200071206     C  N70              ENDdo
011300071206     C*
011400071206     C* Scarico la testata rimasta in canna
011500071206     C                   EXSR      WRIVAB
011600010202     C*
011700010202     C                   endif
011800990910
011900990910     C* Se non ci sono record con errori ...
012000000710     C                   if        §ctrno = 0
012100990910     C* ... restituisco esito OK.
012200990921     C                   eval      wrkesito = '0'
012300990910     C                   else
012400010201     C                   if        §ctrokvb > 0
012500990921     C                   eval      wrkesito = '1'
012600000710     C                   else
012700000710     C                   eval      wrkesito = '2'
012800990910     C                   endif
012900000710     C                   endif
013000990910     C*
013100990914     C                   if        %open(tivin00r)
013200990908     C                   close     tivin00r
013300990914     C                   endif
013400021113     C                   if        %open(fivabwwr)
013500021113     C                   close     fivabwwr
013600990914     C                   endif
013700021113     C                   if        %open(fivatwwr)
013800021113     C                   close     fivatwwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   if        §ctrokvb > 0
014200000724     C                             and vlrpoi <> *zeros
014300010202     C                   exsr      invio
014400990920     C                   endif
014500990920     C*
014600910830     C                   ENDSR
014700000613     C***
014800990920
014900000801     C*----------------------------------------------------*
015000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015100000801     C*----------------------------------------------------*
015200010201     C     INZVAR        BEGSR
015300000801     C*
015400040802     C                   Z-ADD     *zeros        Num5_0            5 0
015500040802     C                   MOVEL     '0'           FlgCAS            1
015600071206     C                   MOVEL     *blanks       wNOT_A           35
015700071206     C                   MOVEL     *blanks       wNOT_B           35
015800071206     C                   MOVEL     *blanks       wNOT_E           35
015900121023     C                   MOVEL     *blanks       wNOT_I           35
016000121023     C                   MOVEL     *blanks       wNOT_J           35
016100071206     C                   MOVEL     *zeros        wNCL              5
016200080923     C                   Z-ADD     *zeros        wPos              5 0
016300080923     C                   MOVEL     *blanks       wConstCAS        20
016400000801     C*
016500000801     C                   ENDSR
016600000801     C*----------------------------------------------------*
016700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016800000801     C*----------------------------------------------------*
016900000801     C     DEFCAM        BEGSR
017000000801     C*
017100021113     C                   CLEAR                   FIVAB000
017200040802     C                   CLEAR                   FIVAT000
017300020619     C* Imposto i valori di default...
017400150507     C                   Z-ADD     2495394       VABCCM
017500150507     C                   Z-ADD     2495394       VATCCM
017600150507     C                   Z-ADD     249           VABLNP
017700150507     C                   Z-ADD     249           VATLNP
017800070531     C                   Z-ADD     000           VABCTR
017900070531     C                   MOVEL     '7Q'          VABCTM
018000040823     C                   MOVEL     '1'           VABCBO
018100020619     C* ... e poi verifico se sono stati passati come parametri
018200020619     C                   IF        vlrppt > *blanks
018300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
018400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018500020619     C                   EXSR      CHKNUM
018600020619     C                   IF        PiInt=*on
018700020619     C                   Z-ADD     PiVal         VABCCM
018800020619     C                   Z-ADD     PiVal         VATCCM
018900020619     C                   ENDIF
019000040506     C                   ENDIF
019100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
019200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
019300020619     C                   EXSR      CHKNUM
019400020619     C                   IF        PiInt=*on
019500020619     C                   Z-ADD     PiVal         VABLNP
019600020619     C                   Z-ADD     PiVal         VATLNP
019700040506     C                   ENDIF
019800020619     C                   ENDIF
019900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020100020619     C                   EXSR      CHKNUM
020200020619     C                   IF        PiInt=*on
020300020619     C                   Z-ADD     PiVal         VABCTR
020400040506     C                   ENDIF
020500020619     C                   ENDIF
020600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
020700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020800060202     C                   ENDIF
020900020619     C                   ENDIF
021000000801     C*
021100000801     C                   ENDSR
021200000801     C*----------------------------------------------------*
021300040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
021400000801     C*----------------------------------------------------*
021500040910     C     IMPVABVAT     BEGSR
021600040910     C*
021700040910     C* Traduzione relativa ai tipi record del file del cliente
021800040910     C*
021900040910     C***
022000071206     C* ...tipo record '01'
022100071206     C                   IF        %subst(vindta:1:2) = '01'
022200071206     C*
022300071206     C* Se giro successivo al primo => scarico buffer precedente
022400071206     C                   IF        wGiro <> '1'
022500071206     C                   EVAL      wGiro  = '1'
022600071206     C                   ELSE
022700071206     C                   EXSR      WRIVAB
022800071206     C                   ENDIF
022900071206     C*
023000040910     C* ......inizializzazioni iniziali e formati record file Bartolini
023100040910     C                   EXSR      INZVAR
023200040910     C                   EXSR      DEFCAM
023300040928     C*
023400040928     C                   Z-ADD     *zeros        errore            1 0
023500040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023600040928     C                   MOVEL     datcor        VABAAS
023700040928     C                   MOVEL     datcor        VATAAS
023800040928     C                   MOVE      datcor        VABMGS
023900040928     C                   MOVE(P)   vlrpoi        VABFGS
024000040928     C                   MOVE(P)   vlrpoi        VATFGS
024100040910     C* ......VABNSP/VATNSP
024200041025     C* NSP => Stacco un numeratore da AZNUM
024300041025     C                   clear                   TRUL33DS
024400041025     C                   eval      I33OPE = *zeros
024500041025     C                   eval      I33CNU = 302
024600041025     C                   eval      I33NUM = 1
024700041025     C                   movel     TRUL33DS      KPJBU
024800041025     C                   call      'TRUL33R'
024900041025     C                   parm                    KPJBA
025000041025     C                   movel     KPJBU         TRUL33DS
025100041025     C                   if        O33ERR = *zeros
025200041025     C                   z-add     O33NRF        VABNSP
025300041025     C                   z-add     O33NRF        VATNSP
025400041025     C                   else
025500041025     C                   Z-ADD     1             errore
025600041025     C                   EVAL      vinmsg = %trimr(vinmsg)
025700041025     C                             + ' ' + 'VABNSP VATNSP'
025800041025     C                   endif
025900060202     C* ......VABRMA
026000071206     C                   EVAL      VABRMA=%trim(%subst(vindta:35:14))
026100150507     C* ......VABCCM /VATCCM per più mandanti
026200180118     C                   SELECT
026300180118     C                   WHEN      %subst(vindta:35:2) = 'HE'
026400180118     C                   Z-ADD     2495888       VABCCM
026500180118     C                   EVAL      VATCCM = VABCCM
026600180118     C                   WHEN      %subst(vindta:35:2) = 'KI'
026700180118     C                   Z-ADD     2495889       VABCCM
026800180118     C                   EVAL      VATCCM = VABCCM
026900180118     C                   WHEN      %subst(vindta:35:2) = 'KA'
027000180118     C                   Z-ADD     2495890       VABCCM
027100180119     C                   Z-ADD     249           VABLNP
027200180119     C                   EVAL      VATCCM = VABCCM
027300180119     C                   EVAL      VATLNP = VABLNP
027400180118     C                   WHEN      %subst(vindta:35:2) = 'MN'
027500180118     C                   Z-ADD     2495891       VABCCM
027600180118     C                   EVAL      VATCCM = VABCCM
027700180118     C                   WHEN      %subst(vindta:35:2) = 'RN'
027800180118     C                   Z-ADD     2495892       VABCCM
027900180118     C                   EVAL      VATCCM = VABCCM
028000180118     C                   WHEN      %subst(vindta:35:2) = 'MG'
028100180118     C                   Z-ADD     2495893       VABCCM
028200180118     C                   EVAL      VATCCM = VABCCM
028300180118     C                   WHEN      %subst(vindta:35:2) = 'FS'
028400180118     C                   Z-ADD     2495894       VABCCM
028500180118     C                   EVAL      VATCCM = VABCCM
028600180118     C                   WHEN      %subst(vindta:35:2) = 'BA'
028700180118     C                   Z-ADD     2495895       VABCCM
028800180118     C                   EVAL      VATCCM = VABCCM
028900180119     C                   WHEN      %subst(vindta:35:2) = 'SI'
029000180119     C                   Z-ADD     1664083       VABCCM
029100180119     C                   Z-ADD     166           VABLNP
029200180119     C                   EVAL      VATCCM = VABCCM
029300180119     C                   EVAL      VATLNP = VABLNP
029400180119     C                   WHEN      %subst(vindta:35:2) = 'M1'
029500180119     C                   Z-ADD     16640834      VABCCM
029600180119     C                   Z-ADD     166           VABLNP
029700180119     C                   EVAL      VATCCM = VABCCM
029800180119     C                   EVAL      VATLNP = VABLNP
029900180119     C                   WHEN      %subst(vindta:35:2) = 'DA'
030000180119     C                   Z-ADD     0507383       VABCCM
030100180119     C                   Z-ADD     050           VABLNP
030200180119     C                   EVAL      VATCCM = VABCCM
030300180119     C                   EVAL      VATLNP = VABLNP
030400180118     C                   WHEN      %subst(vindta:35:2) = 'DE'
030500180118     C                   Z-ADD     2495896       VABCCM
030600180118     C                   EVAL      VATCCM = VABCCM
030700180118     C                   WHEN      %subst(vindta:35:2) = 'NU'
030800180118     C                   Z-ADD     2495897       VABCCM
030900180118     C                   EVAL      VATCCM = VABCCM
031000180118     C                   ENDSL
031100180119     C                   IF        VLRPOI = 999
031200180119     C                   Z-ADD     VABLNP        VABFGS
031300180119     C                   Z-ADD     VATLNP        VATFGS
031400180119     C                   ENDIF
031500060202     C* ......VABRMN
031600150507     C                   EVAL      PiStr=%trim(%subst(vindta:38:6))
031700060202     C                   EXSR      CHKNUM
031800060202     C                   IF        PiInt=*on
031900060202     C                   Z-ADD     PiVal         VABRMN
032000060202     C                   ELSE
032100060202     C                   ADD       1             errore
032200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
032300060202     C                             + ' ' + 'VABRMN'
032400060202     C                   ENDIF
032500060202     C* ......VABNCL
032600071206     C                   EVAL      PiStr=%trim(%subst(vindta:318:4))
032700060202     C                   EXSR      CHKNUM
032800060202     C                   IF        PiInt=*on
032900060202     C                   Z-ADD     PiVal         VABNCL
033000060202     C                   ELSE
033100060202     C                   ADD       1             errore
033200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
033300060202     C                             + ' ' + 'VABNCL'
033400060202     C                   ENDIF
033500060202     C* ......VABPKB
033600071206     C                   EVAL      PiStr=%trim(%subst(vindta:302:8))
033700060202     C                   EXSR      CHKNUM
033800060202     C                   IF        PiNum=*on
033900071206     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
034000070108     C                   Z-ADD(H)  PiVal         VABPKB
034100060202     C                   ELSE
034200060202     C                   ADD       1             errore
034300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
034400060202     C                             + ' ' + 'VABPKB'
034500060202     C                   ENDIF
034600060202     C* ......VABRSD
034700071206     C                   EVAL      VABRSD=%trim(%subst(vindta:69:30))
034800060202     C* ......VABIND
034900071206     C                   EVAL      VABIND=%trim(%subst(vindta:139:30))
035000060202     C* ......VABLOD
035100071206     C                   EVAL      VABLOD=%trim(%subst(vindta:174:21))
035200071206     C* ......VABPRD
035300071206     C                   EVAL      VABPRD=%trim(%subst(vindta:209:2))
035400071206     C* ......VABRD2
035500071206     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
035600060202     C* ......VABCAD
035700071206     C                   EVAL      PiStr=%trim(%subst(vindta:211:5))
035800060202     C                   EXSR      CHKNUM
035900060202     C                   IF        PiInt=*on
036000060202     C                   Z-ADD     PiVal         Num5_0
036100060202     C                   MOVEL(P)  Num5_0        VABCAD
036200060202     C                   ELSE
036300060202     C                   ADD       1             errore
036400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
036500060202     C                             + ' ' + 'VABCAD'
036600071206     C                   ENDIF
036700060202     C* ......VABNOT
036800071206     C                   EVAL      VABNOT=%trim(%subst(vindta:104:35))
036900071206     C* ......VABCAS
037000071206     C                   IF        %trim(%subst(vindta:277:11)) = *all'9'
037100071206     C                   EVAL      FlgCAS = '1'
037200130524     C                   EVAL      VABTIC = 'TO'
037300071206     C                   ENDIF
037400071206     C*
037500071206     C                   ENDIF
037600071206     C***
037700071206     C* ...tipo record '02'
037800071206     C                   IF        %subst(vindta:1:2) = '02'
037900080923     C* ......VABCAS
038000080923     C                   IF        FlgCAS = '1'
038100080923     C                   EVAL      wConstCAS = '** CONTRASSEGNO:'
038200080925     C                   EVAL      wPos = %scan(%trim(wConstCAS):vindta:52)
038300080923     C                   IF        wPos > *zeros
038400080923     C                   EVAL      VABNOT=%trim(%subst(vindta:wPos:36))
038500080923     C                   EVAL      VABVCA=%trim(%subst(vindta:wPos+
038600080925     C                                   %len(%trim(wConstCAS))+1:3))
038700080923     C                   EVAL      PiStr =%trim(%subst(vindta:wPos+
038800080925     C                                   %len(%trim(wConstCAS))+1+3:13))
038900080923     C                   EXSR      CHKNUM
039000080923     C                   IF        PiNum=*on
039100080925     C                   Z-ADD(H)  PiVal         VABCAS
039200080923     C                   ELSE
039300080923     C                   ADD       1             errore
039400080923     C                   EVAL      vinmsg = %trimr(vinmsg)
039500080925     C                             + ' ' + 'VABCAS'
039600080923     C                   ENDIF
039700080923     C                   ENDIF
039800080923     C                   ENDIF
039900060202     C* ......VABNT2
040000080228     C                   EVAL      VABNT2=%trim(%subst(vindta:52:40))
040100071206     C* ......VATNOT_A
040200071206     C                   EVAL      wNOT_A=%trim(%subst(vindta:172:40))
040300071206     C*
040400071206     C                   IF        wNOT_A <> *blanks
040500071206     C                   EVAL      VATTRC = 'A'
040600071206     C                   EVAL      VATNOT = wNOT_A
040700071206     C                   WRITE     FIVAT000
040800071206     C                   ENDIF
040900071206     C*
041000040910     C                   ENDIF
041100071206     C***
041200071206     C* ...tipo record '03'
041300071206     C                   IF        %subst(vindta:1:2) = '03'
041400071206     C* ......VATNOT_E
041500071206     C                   EVAL      wNOT_E=%trim(%subst(vindta:73:15))
041600071206     C* Verifico eventuale bancalatura
041700071206     C                   IF        %subst(wNOT_E:9:3) <> *zeros
041800071206     C                   EVAL      wNCL = '00'+%subst(wNOT_E:9:3)
041900071206     C                   ENDIF
042000071206     C*
042100071206     C                   IF        wNOT_E <> *blanks
042200071206     C                   EVAL      VATTRC = 'E'
042300071206     C                   EVAL      VATNOT = wNOT_E
042400071206     C                   WRITE     FIVAT000
042500071206     C                   ENDIF
042600071206     C*
042700071206     C                   ENDIF
042800121023     C***
042900121023     C* ...tipo record '01'
043000121023     C                   IF        %subst(vindta:1:2) = '01'
043100121023     C*
043200121023     C* ......VATNOT_I
043300121023     C                   EVAL      wNOT_I=%trim(%subst(vindta:322:35))
043400121023     C*
043500121023     C                   IF        wNOT_I <> *blanks
043600121023     C                   EVAL      VATTRC = 'I'
043700121023     C                   EVAL      VATNOT = wNOT_I
043800121023     C                   WRITE     FIVAT000
043900121023     C                   ENDIF
044000121023     C*
044100121023     C* ......VATNOT_J
044200121023     C                   EVAL      wNOT_J=%trim(%subst(vindta:322+35:25))
044300121023     C*
044400121023     C                   IF        wNOT_J <> *blanks
044500121023     C                   EVAL      VATTRC = 'J'
044600121023     C                   EVAL      VATNOT = wNOT_J
044700121023     C                   WRITE     FIVAT000
044800121023     C                   ENDIF
044900121023     C*
045000121023     C                   ENDIF
045100121023     C***
045200010202     C*
045300000801     C* Ebbene...
045400000801     C                   ADD       1             §CTRMO
045500010201     C                   IF        errore <> *zeros
045600000801     C                   ADD       1             §CTRNO
045700000801     C                   EVAL      vinflg = '2'
045800000801     C                   ELSE
045900010201     C                   ADD       1             §CTROKVB
046000000801     C                   ENDIF
046100000801     C*
046200000801     C                   ENDSR
046300071206     C*----------------------------------------------------*
046400071206     C*  WRIVAB - SCARICO BUFFER FILE TESTATA - FNVAB
046500071206     C*----------------------------------------------------*
046600071206     C     WRIVAB        BEGSR
046700071206     C*
046800071206     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046900071206     C                   IF        FlgCAS <> '0'
047000071206     C                   IF        VABCBO = '1'
047100071206     C                   EVAL      VABCBO = '4'
047200071206     C                   ELSE
047300071206     C                   EVAL      VABCBO = '6'
047400071206     C                   ENDIF
047500071206     C                   ENDIF
047600071206     C*
047700071206     C* Se effettuata bancalatura overrido il numero dei colli in testata con il numero d
047800071206     C* bancali (e evidenzio in natura merce)
047900071206     C                   IF        wNCL <> *zeros
048000071206     C                   EVAL      VABNAS = 'Cl.' + %trim(%char(VABNCL)) +
048100071206     C                                      ' Banc.' + %subst(wNCL:3:3)
048200071206     C                   MOVE(P)   wNCL          VABNCL
048300071206     C                   ENDIF
048400071206     C*
048500071206     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048600071206     C                   EXSR      CHKIMPDIV
048700071206     C* ...e in fine...
048800071206     C                   WRITE     FIVAB000
048900071206     C*
049000071206     C                   ENDSR
049100010202     C*----------------------------------------------------*
049200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049300010202     C*----------------------------------------------------*
049400020305     C     PREVAT        BEGSR
049500010202     C*
049600021113     C* Compongo il nome del membro da dare al FIVATWWR
049700010202     C                   eval      parmbr = vlrhdl
049800010202     C                   movel     'M'           parmbr
049900060113     C                   eval      parccm = vlrksc
050000010202     C                   eval      paropz = '1'
050100010202     C* Effettuo la chiamata al CLLE preposto
050200040506     C                   call(e)   'TITVVTC'
050300010202     C                   parm                    parccm
050400010202     C                   parm                    parmbr
050500010202     C                   parm                    paropz
050600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050700010202     C                   if        %error
050800010202     C                   movel     '1'           chkcall
050900010202     C                   else
051000010202     C                   movel     '0'           chkcall
051100010202     C                   endif
051200010202     C*
051300010202     C                   ENDSR
051400000801     C*----------------------------------------------------*
051500000801     C*  CONTROLLO NUMERICITA' CAMPI
051600000801     C*----------------------------------------------------*
051700000801     C     CHKNUM        BEGSR
051800000801     C*
051900000801     C                   call(e)   'ISNUMERIC'
052000000801     C                   PARM                    PiStr            30
052100040714     C                   PARM      ','           PiDecChr          1
052200000801     C                   PARM      *ZEROS        PiVal            30 9
052300000801     C                   PARM      '0'           PiInt             1
052400000801     C                   PARM      '0'           PiNum             1
052500000801     C                   IF        %error
052600000801     C                   EVAL      PiInt=*off
052700000801     C                   ENDIF
052800000801     C*
052900000801     C                   ENDSR
053000000801     C***
053100000801
053200011113
053300011113     C*----------------------------------------------------*
053400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053500011113     C*----------------------------------------------------*
053600011113     C     CHKIMPDIV     BEGSR
053700011113     C*
053800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053900011113     C                   Z-ADD     *zeros        wrkDec            9 9
054000011113     C*
054100011113     C* Come prima cosa effettuo considerazioni sulla divisa
054200011113     C                   IF        vabIAS > *zeros
054300011113     C                   IF        vabVAS <> 'EUR'
054400011113     C                   EVAL      vabVAS =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C                   IF        vabCAS > *zeros
054900011113     C                   IF        vabVCA <> 'EUR'
055000011113     C                   EVAL      vabVCA =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C                   IF        vabVMD > *zeros
055500020305     C                   IF        vabVAD <> 'EUR'
055600011113     C                   EVAL      vabVAD =  'ITL'
055700011113     C                   ENDIF
055800011113     C                   ENDIF
055900011113     C*
056000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056100011113     C                   Z-ADD     vabIAS        wrkDec
056200011113     C                   IF        wrkDec > *zeros
056300011113     C                   IF        vabVAS = 'ITL'
056400011113     C                   EVAL      vabIAS = *zeros
056500011113     C                   ENDIF
056600011113     C                   ENDIF
056700011113     C*
056800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056900011113     C                   Z-ADD     vabCAS        wrkDec
057000011113     C                   IF        wrkDec > *zeros
057100011113     C                   IF        vabVCA = 'ITL'
057200011113     C                   EVAL      vabCAS = *zeros
057300011113     C                   ENDIF
057400011113     C                   ENDIF
057500011113     C*
057600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057700011113     C                   Z-ADD     vabVMD        wrkDec
057800011113     C                   IF        wrkDec > *zeros
057900011113     C                   IF        vabVAD = 'ITL'
058000011113     C                   EVAL      vabVMD = *zeros
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C                   ENDSR
058500011113     C***
058600011113
058700011113
058800000801
058900000801
059000990920      /TITLE Invio dei dati al punto operativo.
059100010202     C     invio         BEGSR
059200990920     C*
059300021113     C* 1° invio FIVAT
059400010201     C                   reset                   dscmz
059500180119     C                   move      vatfgs        cmzdst
059600021113     C                   eval      cmzfld = 'FIVATWWR'
059700010201     C                   eval      cmzmbd = vlrhdl
059800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059900021009     C***                if        prmfir = *blanks
060000021113     C                   eval      cmzfla = 'FIVAT00F'
060100021113     C                   eval      cmzmba = 'FIVAT00F'
060200021009     C***                else
060300021009     C***                eval      cmzfla = prmfir
060400021009     C***                eval      cmzmba = prmfir
060500021009     C***                endif
060600010201     C                   eval      cmznrr = *zeros
060700020305     C                   move      §ctrokvt      cmznrr
060800021018     C                   eval      cmzlba = vlrfl1
060900010201     C                   call(e)   'TIS711C'
061000010201     C                   parm                    dscmz
061100010201     C                   parm      *blanks       esito
061200010205     C                   if        %error
061300010205     C                             or cmzerr = '1'
061400010205     C                             or esito  = '1'
061500010205     C                   eval      wrkesito = '3'
061600010205     C                   else
061700010201     C*
061800021113     C* 2° invio FIVAB
061900010201     C                   reset                   dscmz
062000180119     C                   move      vabfgs        cmzdst
062100010201     C                   eval      cmzfld = vlrfou
062200010201     C                   eval      cmzmbd = vlrhdl
062300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062400021009     C***                if        prmfir = *blanks
062500021113     C                   eval      cmzfla = 'FIVAB00F'
062600021113     C                   eval      cmzmba = 'FIVAB00F'
062700021009     C***                else
062800021009     C***                eval      cmzfla = prmfir
062900021009     C***                eval      cmzmba = prmfir
063000021009     C***                endif
063100010201     C                   eval      cmznrr = *zeros
063200010201     C                   move      §ctrokvb      cmznrr
063300021018     C                   eval      cmzlba = vlrfl1
063400010201     C                   call(e)   'TIS711C'
063500010201     C                   parm                    dscmz
063600010201     C                   parm      *blanks       esito
063700010201     C                   if        %error
063800010201     C                             or cmzerr = '1'
063900010201     C                             or esito  = '1'
064000010201     C                   eval      wrkesito = '3'
064100010201     C                   endif
064200010205     C                   endif
064300990920     C*
064400000613     C                   ENDSR
064500000613     C***
064600070411
064700070411     C     *pssr         BEGSR
064800070411     C*
064900070411     C                   if        %open(tivin00r)
065000070411     C                   close     tivin00r
065100070411     C                   endif
065200070411     C                   if        %open(fivabwwr)
065300070411     C                   close     fivabwwr
065400070411     C                   endif
065500070411     C                   if        %open(fivatwwr)
065600070411     C                   close     fivatwwr
065700070411     C                   endif
065800070411     C*
065900070411     C* Effettuo la chiamata al CLLE preposto
066000070411     C                   call(e)   'TITVVTC'
066100070411     C                   parm                    parccm
066200070411     C                   parm                    parmbr
066300070411     C                   parm      '2'           paropz
066400070411     C*
066500070411     C                   eval      wrkesito = '2'
066600070411     C*
066700070411     C                   seton                                        LR
066800070411     C*
066900070411     C                   ENDSR     '*CANCL'
067000070411     C***
067100070411
067200990910
067300000613     C     *inzsr        BEGSR
067400990910     C*
067500990910     C     *entry        plist
067600990920     C                   parm                    tivlrds
067700990921     C                   parm      wrkesito      esito
067800000724     C                   parm                    prmlit
067900000710     C                   parm                    prmfir
068000000613     C*
068100000830     C* CALCOLA LA DATA CORRENTE
068200000830     C                   time                    wn14             14 0
068300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068400000830     C                   z-add     wn8           g08dat
068500000830     C                   z-add     *zeros        g08inv
068600000830     C                   movel     '0'           g08err
068700000830     C                   call      'XSRDA8'
068800000830     C                   parm                    wlbda8
068900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069000000830     C*
069100000613     C                   ENDSR
069200000613     C***
