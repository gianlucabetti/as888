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
026200150507     C                   IF        %subst(vindta:35:2) = 'NU'
026300150507     C                   Z-ADD     2495394       VABCCM
026400150507     C                   Z-ADD     2495394       VATCCM
026500150507     C                   ENDIF
026600150519     C                   IF        %subst(vindta:35:2) = 'DE'
026700150519     C                   Z-ADD     2495450       VABCCM
026800150519     C                   Z-ADD     2495450       VATCCM
026900150519     C                   ENDIF
027000060202     C* ......VABRMN
027100150507     C                   EVAL      PiStr=%trim(%subst(vindta:38:6))
027200060202     C                   EXSR      CHKNUM
027300060202     C                   IF        PiInt=*on
027400060202     C                   Z-ADD     PiVal         VABRMN
027500060202     C                   ELSE
027600060202     C                   ADD       1             errore
027700060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027800060202     C                             + ' ' + 'VABRMN'
027900060202     C                   ENDIF
028000060202     C* ......VABNCL
028100071206     C                   EVAL      PiStr=%trim(%subst(vindta:318:4))
028200060202     C                   EXSR      CHKNUM
028300060202     C                   IF        PiInt=*on
028400060202     C                   Z-ADD     PiVal         VABNCL
028500060202     C                   ELSE
028600060202     C                   ADD       1             errore
028700060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028800060202     C                             + ' ' + 'VABNCL'
028900060202     C                   ENDIF
029000060202     C* ......VABPKB
029100071206     C                   EVAL      PiStr=%trim(%subst(vindta:302:8))
029200060202     C                   EXSR      CHKNUM
029300060202     C                   IF        PiNum=*on
029400071206     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
029500070108     C                   Z-ADD(H)  PiVal         VABPKB
029600060202     C                   ELSE
029700060202     C                   ADD       1             errore
029800060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029900060202     C                             + ' ' + 'VABPKB'
030000060202     C                   ENDIF
030100060202     C* ......VABRSD
030200071206     C                   EVAL      VABRSD=%trim(%subst(vindta:69:30))
030300060202     C* ......VABIND
030400071206     C                   EVAL      VABIND=%trim(%subst(vindta:139:30))
030500060202     C* ......VABLOD
030600071206     C                   EVAL      VABLOD=%trim(%subst(vindta:174:21))
030700071206     C* ......VABPRD
030800071206     C                   EVAL      VABPRD=%trim(%subst(vindta:209:2))
030900071206     C* ......VABRD2
031000071206     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
031100060202     C* ......VABCAD
031200071206     C                   EVAL      PiStr=%trim(%subst(vindta:211:5))
031300060202     C                   EXSR      CHKNUM
031400060202     C                   IF        PiInt=*on
031500060202     C                   Z-ADD     PiVal         Num5_0
031600060202     C                   MOVEL(P)  Num5_0        VABCAD
031700060202     C                   ELSE
031800060202     C                   ADD       1             errore
031900060202     C                   EVAL      vinmsg = %trimr(vinmsg)
032000060202     C                             + ' ' + 'VABCAD'
032100071206     C                   ENDIF
032200060202     C* ......VABNOT
032300071206     C                   EVAL      VABNOT=%trim(%subst(vindta:104:35))
032400071206     C* ......VABCAS
032500071206     C                   IF        %trim(%subst(vindta:277:11)) = *all'9'
032600071206     C                   EVAL      FlgCAS = '1'
032700130524     C                   EVAL      VABTIC = 'TO'
032800071206     C                   ENDIF
032900071206     C*
033000071206     C                   ENDIF
033100071206     C***
033200071206     C* ...tipo record '02'
033300071206     C                   IF        %subst(vindta:1:2) = '02'
033400080923     C* ......VABCAS
033500080923     C                   IF        FlgCAS = '1'
033600080923     C                   EVAL      wConstCAS = '** CONTRASSEGNO:'
033700080925     C                   EVAL      wPos = %scan(%trim(wConstCAS):vindta:52)
033800080923     C                   IF        wPos > *zeros
033900080923     C                   EVAL      VABNOT=%trim(%subst(vindta:wPos:36))
034000080923     C                   EVAL      VABVCA=%trim(%subst(vindta:wPos+
034100080925     C                                   %len(%trim(wConstCAS))+1:3))
034200080923     C                   EVAL      PiStr =%trim(%subst(vindta:wPos+
034300080925     C                                   %len(%trim(wConstCAS))+1+3:13))
034400080923     C                   EXSR      CHKNUM
034500080923     C                   IF        PiNum=*on
034600080925     C                   Z-ADD(H)  PiVal         VABCAS
034700080923     C                   ELSE
034800080923     C                   ADD       1             errore
034900080923     C                   EVAL      vinmsg = %trimr(vinmsg)
035000080925     C                             + ' ' + 'VABCAS'
035100080923     C                   ENDIF
035200080923     C                   ENDIF
035300080923     C                   ENDIF
035400060202     C* ......VABNT2
035500080228     C                   EVAL      VABNT2=%trim(%subst(vindta:52:40))
035600071206     C* ......VATNOT_A
035700071206     C                   EVAL      wNOT_A=%trim(%subst(vindta:172:40))
035800071206     C*
035900071206     C                   IF        wNOT_A <> *blanks
036000071206     C                   EVAL      VATTRC = 'A'
036100071206     C                   EVAL      VATNOT = wNOT_A
036200071206     C                   WRITE     FIVAT000
036300071206     C                   ENDIF
036400071206     C*
036500040910     C                   ENDIF
036600071206     C***
036700071206     C* ...tipo record '03'
036800071206     C                   IF        %subst(vindta:1:2) = '03'
036900071206     C* ......VATNOT_E
037000071206     C                   EVAL      wNOT_E=%trim(%subst(vindta:73:15))
037100071206     C* Verifico eventuale bancalatura
037200071206     C                   IF        %subst(wNOT_E:9:3) <> *zeros
037300071206     C                   EVAL      wNCL = '00'+%subst(wNOT_E:9:3)
037400071206     C                   ENDIF
037500071206     C*
037600071206     C                   IF        wNOT_E <> *blanks
037700071206     C                   EVAL      VATTRC = 'E'
037800071206     C                   EVAL      VATNOT = wNOT_E
037900071206     C                   WRITE     FIVAT000
038000071206     C                   ENDIF
038100071206     C*
038200071206     C                   ENDIF
038300121023     C***
038400121023     C* ...tipo record '01'
038500121023     C                   IF        %subst(vindta:1:2) = '01'
038600121023     C*
038700121023     C* ......VATNOT_I
038800121023     C                   EVAL      wNOT_I=%trim(%subst(vindta:322:35))
038900121023     C*
039000121023     C                   IF        wNOT_I <> *blanks
039100121023     C                   EVAL      VATTRC = 'I'
039200121023     C                   EVAL      VATNOT = wNOT_I
039300121023     C                   WRITE     FIVAT000
039400121023     C                   ENDIF
039500121023     C*
039600121023     C* ......VATNOT_J
039700121023     C                   EVAL      wNOT_J=%trim(%subst(vindta:322+35:25))
039800121023     C*
039900121023     C                   IF        wNOT_J <> *blanks
040000121023     C                   EVAL      VATTRC = 'J'
040100121023     C                   EVAL      VATNOT = wNOT_J
040200121023     C                   WRITE     FIVAT000
040300121023     C                   ENDIF
040400121023     C*
040500121023     C                   ENDIF
040600121023     C***
040700010202     C*
040800000801     C* Ebbene...
040900000801     C                   ADD       1             §CTRMO
041000010201     C                   IF        errore <> *zeros
041100000801     C                   ADD       1             §CTRNO
041200000801     C                   EVAL      vinflg = '2'
041300000801     C                   ELSE
041400010201     C                   ADD       1             §CTROKVB
041500000801     C                   ENDIF
041600000801     C*
041700000801     C                   ENDSR
041800071206     C*----------------------------------------------------*
041900071206     C*  WRIVAB - SCARICO BUFFER FILE TESTATA - FNVAB
042000071206     C*----------------------------------------------------*
042100071206     C     WRIVAB        BEGSR
042200071206     C*
042300071206     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042400071206     C                   IF        FlgCAS <> '0'
042500071206     C                   IF        VABCBO = '1'
042600071206     C                   EVAL      VABCBO = '4'
042700071206     C                   ELSE
042800071206     C                   EVAL      VABCBO = '6'
042900071206     C                   ENDIF
043000071206     C                   ENDIF
043100071206     C*
043200071206     C* Se effettuata bancalatura overrido il numero dei colli in testata con il numero d
043300071206     C* bancali (e evidenzio in natura merce)
043400071206     C                   IF        wNCL <> *zeros
043500071206     C                   EVAL      VABNAS = 'Cl.' + %trim(%char(VABNCL)) +
043600071206     C                                      ' Banc.' + %subst(wNCL:3:3)
043700071206     C                   MOVE(P)   wNCL          VABNCL
043800071206     C                   ENDIF
043900071206     C*
044000071206     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044100071206     C                   EXSR      CHKIMPDIV
044200071206     C* ...e in fine...
044300071206     C                   WRITE     FIVAB000
044400071206     C*
044500071206     C                   ENDSR
044600010202     C*----------------------------------------------------*
044700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044800010202     C*----------------------------------------------------*
044900020305     C     PREVAT        BEGSR
045000010202     C*
045100021113     C* Compongo il nome del membro da dare al FIVATWWR
045200010202     C                   eval      parmbr = vlrhdl
045300010202     C                   movel     'M'           parmbr
045400060113     C                   eval      parccm = vlrksc
045500010202     C                   eval      paropz = '1'
045600010202     C* Effettuo la chiamata al CLLE preposto
045700040506     C                   call(e)   'TITVVTC'
045800010202     C                   parm                    parccm
045900010202     C                   parm                    parmbr
046000010202     C                   parm                    paropz
046100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046200010202     C                   if        %error
046300010202     C                   movel     '1'           chkcall
046400010202     C                   else
046500010202     C                   movel     '0'           chkcall
046600010202     C                   endif
046700010202     C*
046800010202     C                   ENDSR
046900000801     C*----------------------------------------------------*
047000000801     C*  CONTROLLO NUMERICITA' CAMPI
047100000801     C*----------------------------------------------------*
047200000801     C     CHKNUM        BEGSR
047300000801     C*
047400000801     C                   call(e)   'ISNUMERIC'
047500000801     C                   PARM                    PiStr            30
047600040714     C                   PARM      ','           PiDecChr          1
047700000801     C                   PARM      *ZEROS        PiVal            30 9
047800000801     C                   PARM      '0'           PiInt             1
047900000801     C                   PARM      '0'           PiNum             1
048000000801     C                   IF        %error
048100000801     C                   EVAL      PiInt=*off
048200000801     C                   ENDIF
048300000801     C*
048400000801     C                   ENDSR
048500000801     C***
048600000801
048700011113
048800011113     C*----------------------------------------------------*
048900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049000011113     C*----------------------------------------------------*
049100011113     C     CHKIMPDIV     BEGSR
049200011113     C*
049300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049400011113     C                   Z-ADD     *zeros        wrkDec            9 9
049500011113     C*
049600011113     C* Come prima cosa effettuo considerazioni sulla divisa
049700011113     C                   IF        vabIAS > *zeros
049800011113     C                   IF        vabVAS <> 'EUR'
049900011113     C                   EVAL      vabVAS =  'ITL'
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C                   IF        vabCAS > *zeros
050400011113     C                   IF        vabVCA <> 'EUR'
050500011113     C                   EVAL      vabVCA =  'ITL'
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C                   IF        vabVMD > *zeros
051000020305     C                   IF        vabVAD <> 'EUR'
051100011113     C                   EVAL      vabVAD =  'ITL'
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051600011113     C                   Z-ADD     vabIAS        wrkDec
051700011113     C                   IF        wrkDec > *zeros
051800011113     C                   IF        vabVAS = 'ITL'
051900011113     C                   EVAL      vabIAS = *zeros
052000011113     C                   ENDIF
052100011113     C                   ENDIF
052200011113     C*
052300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052400011113     C                   Z-ADD     vabCAS        wrkDec
052500011113     C                   IF        wrkDec > *zeros
052600011113     C                   IF        vabVCA = 'ITL'
052700011113     C                   EVAL      vabCAS = *zeros
052800011113     C                   ENDIF
052900011113     C                   ENDIF
053000011113     C*
053100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053200011113     C                   Z-ADD     vabVMD        wrkDec
053300011113     C                   IF        wrkDec > *zeros
053400011113     C                   IF        vabVAD = 'ITL'
053500011113     C                   EVAL      vabVMD = *zeros
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   ENDSR
054000011113     C***
054100011113
054200011113
054300000801
054400000801
054500990920      /TITLE Invio dei dati al punto operativo.
054600010202     C     invio         BEGSR
054700990920     C*
054800021113     C* 1° invio FIVAT
054900010201     C                   reset                   dscmz
055000010201     C                   move      vlrpoi        cmzdst
055100021113     C                   eval      cmzfld = 'FIVATWWR'
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500021113     C                   eval      cmzfla = 'FIVAT00F'
055600021113     C                   eval      cmzmba = 'FIVAT00F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200020305     C                   move      §ctrokvt      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010205     C                   if        %error
056800010205     C                             or cmzerr = '1'
056900010205     C                             or esito  = '1'
057000010205     C                   eval      wrkesito = '3'
057100010205     C                   else
057200010201     C*
057300021113     C* 2° invio FIVAB
057400010201     C                   reset                   dscmz
057500010201     C                   move      vlrpoi        cmzdst
057600010201     C                   eval      cmzfld = vlrfou
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021113     C                   eval      cmzfla = 'FIVAB00F'
058100021113     C                   eval      cmzmba = 'FIVAB00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700010201     C                   move      §ctrokvb      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010201     C                   if        %error
059300010201     C                             or cmzerr = '1'
059400010201     C                             or esito  = '1'
059500010201     C                   eval      wrkesito = '3'
059600010201     C                   endif
059700010205     C                   endif
059800990920     C*
059900000613     C                   ENDSR
060000000613     C***
060100070411
060200070411     C     *pssr         BEGSR
060300070411     C*
060400070411     C                   if        %open(tivin00r)
060500070411     C                   close     tivin00r
060600070411     C                   endif
060700070411     C                   if        %open(fivabwwr)
060800070411     C                   close     fivabwwr
060900070411     C                   endif
061000070411     C                   if        %open(fivatwwr)
061100070411     C                   close     fivatwwr
061200070411     C                   endif
061300070411     C*
061400070411     C* Effettuo la chiamata al CLLE preposto
061500070411     C                   call(e)   'TITVVTC'
061600070411     C                   parm                    parccm
061700070411     C                   parm                    parmbr
061800070411     C                   parm      '2'           paropz
061900070411     C*
062000070411     C                   eval      wrkesito = '2'
062100070411     C*
062200070411     C                   seton                                        LR
062300070411     C*
062400070411     C                   ENDSR     '*CANCL'
062500070411     C***
062600070411
062700990910
062800000613     C     *inzsr        BEGSR
062900990910     C*
063000990910     C     *entry        plist
063100990920     C                   parm                    tivlrds
063200990921     C                   parm      wrkesito      esito
063300000724     C                   parm                    prmlit
063400000710     C                   parm                    prmfir
063500000613     C*
063600000830     C* CALCOLA LA DATA CORRENTE
063700000830     C                   time                    wn14             14 0
063800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063900000830     C                   z-add     wn8           g08dat
064000000830     C                   z-add     *zeros        g08inv
064100000830     C                   movel     '0'           g08err
064200000830     C                   call      'XSRDA8'
064300000830     C                   parm                    wlbda8
064400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064500000830     C*
064600000613     C                   ENDSR
064700000613     C***
