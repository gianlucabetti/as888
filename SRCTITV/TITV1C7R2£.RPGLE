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
017400071206     C                   Z-ADD     0054471       VABCCM
017500071206     C                   Z-ADD     0054471       VATCCM
017600071030     C                   Z-ADD     005           VABLNP
017700071030     C                   Z-ADD     005           VATLNP
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
026100121113     C* ......VABCCM /VATCCM
026200150212     C***                IF        %subst(%trim(VABRMA):1:2) = '72'
026300150212     C***                EVAL      VABCCM = 2494917
026400150212     C***                EVAL      VATCCM = 2494917
026500150212     C***                ENDIF
026600060202     C* ......VABRMN
026700071206     C                   EVAL      PiStr=%trim(%subst(vindta:35:8))
026800060202     C                   EXSR      CHKNUM
026900060202     C                   IF        PiInt=*on
027000060202     C                   Z-ADD     PiVal         VABRMN
027100060202     C                   ELSE
027200060202     C                   ADD       1             errore
027300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027400060202     C                             + ' ' + 'VABRMN'
027500060202     C                   ENDIF
027600060202     C* ......VABNCL
027700071206     C                   EVAL      PiStr=%trim(%subst(vindta:318:4))
027800060202     C                   EXSR      CHKNUM
027900060202     C                   IF        PiInt=*on
028000060202     C                   Z-ADD     PiVal         VABNCL
028100060202     C                   ELSE
028200060202     C                   ADD       1             errore
028300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028400060202     C                             + ' ' + 'VABNCL'
028500060202     C                   ENDIF
028600060202     C* ......VABPKB
028700071206     C                   EVAL      PiStr=%trim(%subst(vindta:302:8))
028800060202     C                   EXSR      CHKNUM
028900060202     C                   IF        PiNum=*on
029000071206     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
029100070108     C                   Z-ADD(H)  PiVal         VABPKB
029200060202     C                   ELSE
029300060202     C                   ADD       1             errore
029400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029500060202     C                             + ' ' + 'VABPKB'
029600060202     C                   ENDIF
029700060202     C* ......VABRSD
029800071206     C                   EVAL      VABRSD=%trim(%subst(vindta:69:30))
029900060202     C* ......VABIND
030000071206     C                   EVAL      VABIND=%trim(%subst(vindta:139:30))
030100060202     C* ......VABLOD
030200071206     C                   EVAL      VABLOD=%trim(%subst(vindta:174:21))
030300071206     C* ......VABPRD
030400071206     C                   EVAL      VABPRD=%trim(%subst(vindta:209:2))
030500071206     C* ......VABRD2
030600071206     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
030700060202     C* ......VABCAD
030800071206     C                   EVAL      PiStr=%trim(%subst(vindta:211:5))
030900060202     C                   EXSR      CHKNUM
031000060202     C                   IF        PiInt=*on
031100060202     C                   Z-ADD     PiVal         Num5_0
031200060202     C                   MOVEL(P)  Num5_0        VABCAD
031300060202     C                   ELSE
031400060202     C                   ADD       1             errore
031500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
031600060202     C                             + ' ' + 'VABCAD'
031700071206     C                   ENDIF
031800060202     C* ......VABNOT
031900071206     C                   EVAL      VABNOT=%trim(%subst(vindta:104:35))
032000071206     C* ......VABCAS
032100071206     C                   IF        %trim(%subst(vindta:277:11)) = *all'9'
032200071206     C                   EVAL      FlgCAS = '1'
032300130524     C                   EVAL      VABTIC = 'TO'
032400071206     C                   ENDIF
032500071206     C*
032600071206     C                   ENDIF
032700071206     C***
032800071206     C* ...tipo record '02'
032900071206     C                   IF        %subst(vindta:1:2) = '02'
033000080923     C* ......VABCAS
033100080923     C                   IF        FlgCAS = '1'
033200080923     C                   EVAL      wConstCAS = '** CONTRASSEGNO:'
033300080925     C                   EVAL      wPos = %scan(%trim(wConstCAS):vindta:52)
033400080923     C                   IF        wPos > *zeros
033500080923     C                   EVAL      VABNOT=%trim(%subst(vindta:wPos:36))
033600080923     C                   EVAL      VABVCA=%trim(%subst(vindta:wPos+
033700080925     C                                   %len(%trim(wConstCAS))+1:3))
033800080923     C                   EVAL      PiStr =%trim(%subst(vindta:wPos+
033900080925     C                                   %len(%trim(wConstCAS))+1+3:13))
034000080923     C                   EXSR      CHKNUM
034100080923     C                   IF        PiNum=*on
034200080925     C                   Z-ADD(H)  PiVal         VABCAS
034300080923     C                   ELSE
034400080923     C                   ADD       1             errore
034500080923     C                   EVAL      vinmsg = %trimr(vinmsg)
034600080925     C                             + ' ' + 'VABCAS'
034700080923     C                   ENDIF
034800080923     C                   ENDIF
034900080923     C                   ENDIF
035000060202     C* ......VABNT2
035100080228     C                   EVAL      VABNT2=%trim(%subst(vindta:52:40))
035200071206     C* ......VATNOT_A
035300071206     C                   EVAL      wNOT_A=%trim(%subst(vindta:172:40))
035400071206     C*
035500071206     C                   IF        wNOT_A <> *blanks
035600071206     C                   EVAL      VATTRC = 'A'
035700071206     C                   EVAL      VATNOT = wNOT_A
035800071206     C                   WRITE     FIVAT000
035900071206     C                   ENDIF
036000071206     C*
036100040910     C                   ENDIF
036200071206     C***
036300071206     C* ...tipo record '03'
036400071206     C                   IF        %subst(vindta:1:2) = '03'
036500071206     C* ......VATNOT_E
036600071206     C                   EVAL      wNOT_E=%trim(%subst(vindta:73:15))
036700071206     C* Verifico eventuale bancalatura
036800071206     C                   IF        %subst(wNOT_E:9:3) <> *zeros
036900071206     C                   EVAL      wNCL = '00'+%subst(wNOT_E:9:3)
037000071206     C                   ENDIF
037100071206     C*
037200071206     C                   IF        wNOT_E <> *blanks
037300071206     C                   EVAL      VATTRC = 'E'
037400071206     C                   EVAL      VATNOT = wNOT_E
037500071206     C                   WRITE     FIVAT000
037600071206     C                   ENDIF
037700071206     C*
037800071206     C                   ENDIF
037900121023     C***
038000121023     C* ...tipo record '01'
038100121023     C                   IF        %subst(vindta:1:2) = '01'
038200121023     C*
038300121023     C* ......VATNOT_I
038400121023     C                   EVAL      wNOT_I=%trim(%subst(vindta:322:35))
038500121023     C*
038600121023     C                   IF        wNOT_I <> *blanks
038700121023     C                   EVAL      VATTRC = 'I'
038800121023     C                   EVAL      VATNOT = wNOT_I
038900121023     C                   WRITE     FIVAT000
039000121023     C                   ENDIF
039100121023     C*
039200121023     C* ......VATNOT_J
039300121023     C                   EVAL      wNOT_J=%trim(%subst(vindta:322+35:25))
039400121023     C*
039500121023     C                   IF        wNOT_J <> *blanks
039600121023     C                   EVAL      VATTRC = 'J'
039700121023     C                   EVAL      VATNOT = wNOT_J
039800121023     C                   WRITE     FIVAT000
039900121023     C                   ENDIF
040000121023     C*
040100121023     C                   ENDIF
040200121023     C***
040300010202     C*
040400000801     C* Ebbene...
040500000801     C                   ADD       1             §CTRMO
040600010201     C                   IF        errore <> *zeros
040700000801     C                   ADD       1             §CTRNO
040800000801     C                   EVAL      vinflg = '2'
040900000801     C                   ELSE
041000010201     C                   ADD       1             §CTROKVB
041100000801     C                   ENDIF
041200000801     C*
041300000801     C                   ENDSR
041400071206     C*----------------------------------------------------*
041500071206     C*  WRIVAB - SCARICO BUFFER FILE TESTATA - FNVAB
041600071206     C*----------------------------------------------------*
041700071206     C     WRIVAB        BEGSR
041800071206     C*
041900071206     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042000071206     C                   IF        FlgCAS <> '0'
042100071206     C                   IF        VABCBO = '1'
042200071206     C                   EVAL      VABCBO = '4'
042300071206     C                   ELSE
042400071206     C                   EVAL      VABCBO = '6'
042500071206     C                   ENDIF
042600071206     C                   ENDIF
042700071206     C*
042800071206     C* Se effettuata bancalatura overrido il numero dei colli in testata con il numero d
042900071206     C* bancali (e evidenzio in natura merce)
043000071206     C                   IF        wNCL <> *zeros
043100071206     C                   EVAL      VABNAS = 'Cl.' + %trim(%char(VABNCL)) +
043200071206     C                                      ' Banc.' + %subst(wNCL:3:3)
043300071206     C                   MOVE(P)   wNCL          VABNCL
043400071206     C                   ENDIF
043500071206     C*
043600071206     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043700071206     C                   EXSR      CHKIMPDIV
043800071206     C* ...e in fine...
043900071206     C                   WRITE     FIVAB000
044000071206     C*
044100071206     C                   ENDSR
044200010202     C*----------------------------------------------------*
044300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044400010202     C*----------------------------------------------------*
044500020305     C     PREVAT        BEGSR
044600010202     C*
044700021113     C* Compongo il nome del membro da dare al FIVATWWR
044800010202     C                   eval      parmbr = vlrhdl
044900010202     C                   movel     'M'           parmbr
045000060113     C                   eval      parccm = vlrksc
045100010202     C                   eval      paropz = '1'
045200010202     C* Effettuo la chiamata al CLLE preposto
045300040506     C                   call(e)   'TITVVTC'
045400010202     C                   parm                    parccm
045500010202     C                   parm                    parmbr
045600010202     C                   parm                    paropz
045700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045800010202     C                   if        %error
045900010202     C                   movel     '1'           chkcall
046000010202     C                   else
046100010202     C                   movel     '0'           chkcall
046200010202     C                   endif
046300010202     C*
046400010202     C                   ENDSR
046500000801     C*----------------------------------------------------*
046600000801     C*  CONTROLLO NUMERICITA' CAMPI
046700000801     C*----------------------------------------------------*
046800000801     C     CHKNUM        BEGSR
046900000801     C*
047000000801     C                   call(e)   'ISNUMERIC'
047100000801     C                   PARM                    PiStr            30
047200040714     C                   PARM      ','           PiDecChr          1
047300000801     C                   PARM      *ZEROS        PiVal            30 9
047400000801     C                   PARM      '0'           PiInt             1
047500000801     C                   PARM      '0'           PiNum             1
047600000801     C                   IF        %error
047700000801     C                   EVAL      PiInt=*off
047800000801     C                   ENDIF
047900000801     C*
048000000801     C                   ENDSR
048100000801     C***
048200000801
048300011113
048400011113     C*----------------------------------------------------*
048500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048600011113     C*----------------------------------------------------*
048700011113     C     CHKIMPDIV     BEGSR
048800011113     C*
048900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049000011113     C                   Z-ADD     *zeros        wrkDec            9 9
049100011113     C*
049200011113     C* Come prima cosa effettuo considerazioni sulla divisa
049300011113     C                   IF        vabIAS > *zeros
049400011113     C                   IF        vabVAS <> 'EUR'
049500011113     C                   EVAL      vabVAS =  'ITL'
049600011113     C                   ENDIF
049700011113     C                   ENDIF
049800011113     C*
049900011113     C                   IF        vabCAS > *zeros
050000011113     C                   IF        vabVCA <> 'EUR'
050100011113     C                   EVAL      vabVCA =  'ITL'
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C                   IF        vabVMD > *zeros
050600020305     C                   IF        vabVAD <> 'EUR'
050700011113     C                   EVAL      vabVAD =  'ITL'
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051200011113     C                   Z-ADD     vabIAS        wrkDec
051300011113     C                   IF        wrkDec > *zeros
051400011113     C                   IF        vabVAS = 'ITL'
051500011113     C                   EVAL      vabIAS = *zeros
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052000011113     C                   Z-ADD     vabCAS        wrkDec
052100011113     C                   IF        wrkDec > *zeros
052200011113     C                   IF        vabVCA = 'ITL'
052300011113     C                   EVAL      vabCAS = *zeros
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052800011113     C                   Z-ADD     vabVMD        wrkDec
052900011113     C                   IF        wrkDec > *zeros
053000011113     C                   IF        vabVAD = 'ITL'
053100011113     C                   EVAL      vabVMD = *zeros
053200011113     C                   ENDIF
053300011113     C                   ENDIF
053400011113     C*
053500011113     C                   ENDSR
053600011113     C***
053700011113
053800011113
053900000801
054000000801
054100990920      /TITLE Invio dei dati al punto operativo.
054200010202     C     invio         BEGSR
054300990920     C*
054400021113     C* 1° invio FIVAT
054500010201     C                   reset                   dscmz
054600010201     C                   move      vlrpoi        cmzdst
054700021113     C                   eval      cmzfld = 'FIVATWWR'
054800010201     C                   eval      cmzmbd = vlrhdl
054900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055000021009     C***                if        prmfir = *blanks
055100021113     C                   eval      cmzfla = 'FIVAT00F'
055200021113     C                   eval      cmzmba = 'FIVAT00F'
055300021009     C***                else
055400021009     C***                eval      cmzfla = prmfir
055500021009     C***                eval      cmzmba = prmfir
055600021009     C***                endif
055700010201     C                   eval      cmznrr = *zeros
055800020305     C                   move      §ctrokvt      cmznrr
055900021018     C                   eval      cmzlba = vlrfl1
056000010201     C                   call(e)   'TIS711C'
056100010201     C                   parm                    dscmz
056200010201     C                   parm      *blanks       esito
056300010205     C                   if        %error
056400010205     C                             or cmzerr = '1'
056500010205     C                             or esito  = '1'
056600010205     C                   eval      wrkesito = '3'
056700010205     C                   else
056800010201     C*
056900021113     C* 2° invio FIVAB
057000010201     C                   reset                   dscmz
057100010201     C                   move      vlrpoi        cmzdst
057200010201     C                   eval      cmzfld = vlrfou
057300010201     C                   eval      cmzmbd = vlrhdl
057400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057500021009     C***                if        prmfir = *blanks
057600021113     C                   eval      cmzfla = 'FIVAB00F'
057700021113     C                   eval      cmzmba = 'FIVAB00F'
057800021009     C***                else
057900021009     C***                eval      cmzfla = prmfir
058000021009     C***                eval      cmzmba = prmfir
058100021009     C***                endif
058200010201     C                   eval      cmznrr = *zeros
058300010201     C                   move      §ctrokvb      cmznrr
058400021018     C                   eval      cmzlba = vlrfl1
058500010201     C                   call(e)   'TIS711C'
058600010201     C                   parm                    dscmz
058700010201     C                   parm      *blanks       esito
058800010201     C                   if        %error
058900010201     C                             or cmzerr = '1'
059000010201     C                             or esito  = '1'
059100010201     C                   eval      wrkesito = '3'
059200010201     C                   endif
059300010205     C                   endif
059400990920     C*
059500000613     C                   ENDSR
059600000613     C***
059700070411
059800070411     C     *pssr         BEGSR
059900070411     C*
060000070411     C                   if        %open(tivin00r)
060100070411     C                   close     tivin00r
060200070411     C                   endif
060300070411     C                   if        %open(fivabwwr)
060400070411     C                   close     fivabwwr
060500070411     C                   endif
060600070411     C                   if        %open(fivatwwr)
060700070411     C                   close     fivatwwr
060800070411     C                   endif
060900070411     C*
061000070411     C* Effettuo la chiamata al CLLE preposto
061100070411     C                   call(e)   'TITVVTC'
061200070411     C                   parm                    parccm
061300070411     C                   parm                    parmbr
061400070411     C                   parm      '2'           paropz
061500070411     C*
061600070411     C                   eval      wrkesito = '2'
061700070411     C*
061800070411     C                   seton                                        LR
061900070411     C*
062000070411     C                   ENDSR     '*CANCL'
062100070411     C***
062200070411
062300990910
062400000613     C     *inzsr        BEGSR
062500990910     C*
062600990910     C     *entry        plist
062700990920     C                   parm                    tivlrds
062800990921     C                   parm      wrkesito      esito
062900000724     C                   parm                    prmlit
063000000710     C                   parm                    prmfir
063100000613     C*
063200000830     C* CALCOLA LA DATA CORRENTE
063300000830     C                   time                    wn14             14 0
063400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063500000830     C                   z-add     wn8           g08dat
063600000830     C                   z-add     *zeros        g08inv
063700000830     C                   movel     '0'           g08err
063800000830     C                   call      'XSRDA8'
063900000830     C                   parm                    wlbda8
064000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064100000830     C*
064200000613     C                   ENDSR
064300000613     C***
