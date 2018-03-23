000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130422      *
000300130422      * PARTICOLARITA':
000400130422      * si ipotizza che i tipi rcd seguano l'ordine indicato dal POC:
000500130422      * SD-CZ-CN-GD-RE-MA-BC
000600130422      * già nell'esempio mandato non è vero tra BC e MA, l'importante però che il primo utile sia
000700130422      * sempre SD
000800130422      *
000900130422      * i tipi rcd si prendono di 4 caratteri perché di GD ce ne sono 2: 'GD  ' e GDDD'
001000130422      *
001100130422      * i barcode sono al max 7 per riga del tipi rcd BC (non ci è stato detto se potrebbero essrci
001200130422      *  più righe BC, ma il pgm lo gestisce)
001300130422      *
001400130422      * i parametri sono diversi dal 14esimo in poi
001500130422      *
001600130422     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001700990908
001800990910     Ftivin00r  uF   E             DISK    usropn
001900021113     FFIVABwwr  O    E             DISK    usropn
002000021113     FFIVATwwr  O    E             DISK    usropn
002100990908
002200000801     D*----------------------------------------------------
002300000801     D* DICHIARAZIOINE VARIABILI DI WRK
002400000801     D*----------------------------------------------------
002500990920     D dscmz         e ds                  inz
002600990910     D psds           sds
002700990910     D  procname         *PROC
002800990920     D tivlrds       e ds                  extname(tivlr00f)
002900990910     D esito           s              1
003000000724     D prmlit          s             10
003100000710     D prmfir          s             10
003200990921     D wrkesito        s                   like(esito)
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400010202     D parccm          s              8    INZ(*blanks)
003500010202     D parmbr          s             10    INZ(*blanks)
003600010202     D paropz          s              1    INZ(*blanks)
003700010202     D chkcall         s              1    INZ(*blanks)
003800071206     D wGiro           s              1    INZ('0')
003900130422     D w60             s             60
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900041025     D*------------------
005000041025     D* DS REPERIMENTO NUMERATORE
005100041025     D*------------------
005200041025     D trul33ds      e ds                  inz
005300041025     D*------------------
005400041025     D* DS ARCHITETTURA
005500041025     D*------------------
005600041025     D kpjba         e ds                  inz
005700041025     D*------------------
005800130422     D* DS suddivisione barcode
005900130422     D*------------------
006000130422     D WNOT_E_DS       DS
006100130422     D  WNOT_E_SK                    25    dim(7)
006200990908
006300010201
006400010201
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800000613     C*
006900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
007000000613     C*
007100010202     C* Effettuo la chiamata al CLLE preposto
007200040506     C                   call(e)   'TITVVTC'
007300010202     C                   parm                    parccm
007400010202     C                   parm                    parmbr
007500010202     C                   parm      '2'           paropz
007600000616     C*
007700010201     C                   seton                                        LR
007800990908
007900000801
008000910830     C*--------------------------------------------------------
008100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008200910830     C*--------------------------------------------------------
008300040526     C     RWFILE        BEGSR
008400990910     C*
008500990914     C                   if        not %open(tivin00r)
008600990908     C                   open      tivin00r
008700990914     C                   endif
008800021113     C                   if        not %open(fivabwwr)
008900021113     C                   open      fivabwwr
009000990914     C                   endif
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
010400040910     C*
010500921023     C                   DO        *HIVAL
010600990913     C*
010700990915     C                   READ      tivin00r                               70
010800040910     C                   if        vindta > *blanks
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
011900040910     C*
012000040910     C* Eseguo routine d traduzione
012100040910     C                   exsr      impvabvat
012200040802     C*
012300010305     C                   endif
012400000905     C*
012500000905     C                   else
012600000905     C                   eval      vinflg = '1'
012700000905     C                   endif
012800000905     C*
012900000905     C  N70              update    tivin000
013000000905     C*
013100071206     C  N70              ENDdo
013200071206     C*
013300071206     C* Scarico la testata rimasta in canna
013400071206     C                   EXSR      WRIVAB
013500010202     C*
013600010202     C                   endif
013700990910
013800990910     C* Se non ci sono record con errori ...
013900000710     C                   if        §ctrno = 0
014000990910     C* ... restituisco esito OK.
014100130422     C                   eval      wrkesito = '0'
014200990910     C                   else
014300010201     C                   if        §ctrokvb > 0
014400990921     C                   eval      wrkesito = '1'
014500000710     C                   else
014600000710     C                   eval      wrkesito = '2'
014700990910     C                   endif
014800000710     C                   endif
014900990910     C*
015000990914     C                   if        %open(tivin00r)
015100990908     C                   close     tivin00r
015200990914     C                   endif
015300021113     C                   if        %open(fivabwwr)
015400021113     C                   close     fivabwwr
015500990914     C                   endif
015600021113     C                   if        %open(fivatwwr)
015700021113     C                   close     fivatwwr
015800010201     C                   endif
015900990910     C*
016000010201     C                   if        §ctrokvb > 0
016100000724     C                             and vlrpoi <> *zeros
016200010202     C                   exsr      invio
016300990920     C                   endif
016400990920     C*
016500910830     C                   ENDSR
016600000613     C***
016700990920
016800000801     C*----------------------------------------------------*
016900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017000000801     C*----------------------------------------------------*
017100010201     C     INZVAR        BEGSR
017200000801     C*
017300040802     C                   Z-ADD     *zeros        Num5_0            5 0
017400040802     C                   MOVEL     '0'           FlgCAS            1
017500071206     C                   MOVEL     *blanks       wNOT_A           35
017600071206     C                   MOVEL     *blanks       wNOT_B           35
017700071206     C                   MOVEL     *blanks       wNOT_E           35
017800121023     C                   MOVEL     *blanks       wNOT_I           35
017900121023     C                   MOVEL     *blanks       wNOT_J           35
018000071206     C                   MOVEL     *zeros        wNCL              5
018100080923     C                   Z-ADD     *zeros        wPos              5 0
018200080923     C                   MOVEL     *blanks       wConstCAS        20
018300000801     C*
018400000801     C                   ENDSR
018500000801     C*----------------------------------------------------*
018600040910     C*  IMPOSTAZIONE CAMPI COSTANTI
018700000801     C*----------------------------------------------------*
018800000801     C     DEFCAM        BEGSR
018900000801     C*
019000021113     C                   CLEAR                   FIVAB000
019100040802     C                   CLEAR                   FIVAT000
019200020619     C* Imposto i valori di default...
019300130422     C                   Z-ADD     2022716       VABCCM
019400130422     C                   Z-ADD     2022716       VATCCM
019500130422     C                   Z-ADD     202           VABLNP
019600130422     C                   Z-ADD     202           VATLNP
019700150519     C                   Z-ADD     050           VABCTR
019800130503     C                   MOVEL     *blank        VABCTM
019900040823     C                   MOVEL     '1'           VABCBO
020000130422     C                   MOVEL     'C'           VABTSP
020100020619     C* ... e poi verifico se sono stati passati come parametri
020200020619     C                   IF        vlrppt > *blanks
020300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020500020619     C                   EXSR      CHKNUM
020600020619     C                   IF        PiInt=*on
020700020619     C                   Z-ADD     PiVal         VABCCM
020800020619     C                   Z-ADD     PiVal         VATCCM
020900020619     C                   ENDIF
021000040506     C                   ENDIF
021100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABLNP
021600020619     C                   Z-ADD     PiVal         VATLNP
021700040506     C                   ENDIF
021800020619     C                   ENDIF
021900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABCTR
022400040506     C                   ENDIF
022500020619     C                   ENDIF
022600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
022700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
022800060202     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200000801     C*----------------------------------------------------*
023300040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
023400000801     C*----------------------------------------------------*
023500040910     C     IMPVABVAT     BEGSR
023600040910     C*
023700040910     C* Traduzione relativa ai tipi record del file del cliente
023800040910     C*
023900040910     C***
024000130422     C* ...tipo record 'SD '
024100130422     C                   IF        %subst(vindta:1:3) = 'SD '
024200071206     C*
024300071206     C* Se giro successivo al primo => scarico buffer precedente
024400071206     C                   IF        wGiro <> '1'
024500071206     C                   EVAL      wGiro  = '1'
024600071206     C                   ELSE
024700071206     C                   EXSR      WRIVAB
024800071206     C                   ENDIF
024900071206     C*
025000040910     C* ......inizializzazioni iniziali e formati record file Bartolini
025100040910     C                   EXSR      INZVAR
025200040910     C                   EXSR      DEFCAM
025300040928     C*
025400040928     C                   Z-ADD     *zeros        errore            1 0
025500040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
025600040928     C                   MOVEL     datcor        VABAAS
025700040928     C                   MOVEL     datcor        VATAAS
025800040928     C                   MOVE      datcor        VABMGS
025900040928     C                   MOVE(P)   vlrpoi        VABFGS
026000040928     C                   MOVE(P)   vlrpoi        VATFGS
026100040910     C* ......VABNSP/VATNSP
026200041025     C* NSP => Stacco un numeratore da AZNUM
026300041025     C                   clear                   TRUL33DS
026400041025     C                   eval      I33OPE = *zeros
026500041025     C                   eval      I33CNU = 302
026600041025     C                   eval      I33NUM = 1
026700041025     C                   movel     TRUL33DS      KPJBU
026800041025     C                   call      'TRUL33R'
026900041025     C                   parm                    KPJBA
027000041025     C                   movel     KPJBU         TRUL33DS
027100041025     C                   if        O33ERR = *zeros
027200041025     C                   z-add     O33NRF        VABNSP
027300041025     C                   z-add     O33NRF        VATNSP
027400041025     C                   else
027500041025     C                   Z-ADD     1             errore
027600041025     C                   EVAL      vinmsg = %trimr(vinmsg)
027700041025     C                             + ' ' + 'VABNSP VATNSP'
027800041025     C                   endif
027900060202     C* ......VABRMA
028000130422     C                   EVAL      VABRMA=%trim(%subst(vindta:4:20))
028100060202     C* ......VABRMN
028200130422     C                   EVAL      PiStr=%trim(%subst(vindta:4:20))
028300060202     C                   EXSR      CHKNUM
028400060202     C                   IF        PiInt=*on
028500060202     C                   Z-ADD     PiVal         VABRMN
028600060202     C                   ELSE
028700060202     C                   ADD       1             errore
028800060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028900060202     C                             + ' ' + 'VABRMN'
029000060202     C                   ENDIF
029100130422     C*
029200130422     C* ......VABDCR
029300130422     C* arriva in formato SAAMMGG
029400130422     C                   IF        %trim(%subst(vindta:49:7))<>*blanks
029500130422     C                   EVAL      PiStr=%trim(%subst(vindta:49:7))
029600130422     C                   EXSR      CHKNUM
029700130422     C                   IF        PiInt=*on
029800130422     C* adesso che so che è numerico, imposto il secolo a 20 se il cliente ha passato 1
029900130422     C                   IF        PiVal > 999999
030000130422     C                   EVAL      PiVal = PiVal + 19000000
030100130422     C                   ENDIF
030200130422     C                   Z-ADD     PiVal         VABDCR
030300130422     C                   ELSE
030400130422     C                   SETON                                        31
030500130422     C                   Z-ADD     *zeros        VABDCR
030600130422     C                   EVAL      vinmsg = %trimr(vinmsg)
030700130422     C                             + ' ' + 'VABDCR'
030800130422     C                   ENDIF
030900130422     C                   ENDIF
031000130422     C*
031100130422     C                   ENDIF
031200130422     C***
031300130422     C* ...tipo record 'CZ  '
031400130422     C                   IF        %subst(vindta:1:3) = 'CZ '
031500130422     C*
031600130422     C* ......VABRMO
031700130422     C                   EVAL      VABRMO=%trim(%subst(vindta:20:35))
031800130422     C* ......VABCMO
031900130422     C                   EVAL      VABCMO=%trim(%subst(vindta:117:8))
032000130422     C*
032100130422     C                   ENDIF
032200130422     C***
032300130422     C* ...tipo record 'CN  '
032400130422     C                   IF        %subst(vindta:1:3) = 'CN '
032500130422     C*
032600130422     C* ......VABRSD
032700130422     C                   EVAL      VABRSD=%trim(%subst(vindta:20:35))
032800130422     C* ......VABIND
032900130422     C                   EVAL      VABIND=%trim(%subst(vindta:55:35))
033000130422     C* ......VABLOD
033100130422     C                   EVAL      VABLOD=%trim(%subst(vindta:90:25))
033200130422     C* ......VABPRD
033300130422     C                   EVAL      VABPRD=%trim(%subst(vindta:115:2))
033400130422     C* ......VABCAD
033500130422     C                   EVAL      PiStr=%trim(%subst(vindta:117:8))
033600130422     C                   EXSR      CHKNUM
033700130422     C                   IF        PiInt=*on
033800130422     C                   Z-ADD     PiVal         Num5_0
033900130422     C                   MOVEL(P)  Num5_0        VABCAD
034000130422     C                   ELSE
034100130422     C                   ADD       1             errore
034200130422     C                   EVAL      vinmsg = %trimr(vinmsg)
034300130422     C                             + ' ' + 'VABCAD'
034400130422     C                   ENDIF
034500130422     C*
034600130422     C                   ENDIF
034700130705     C***
034800130705     C* ...tipo record 'DP  '
034900130705     C                   IF        %subst(vindta:1:3) = 'DP '
035000130705     C*
035100130705     C* ......VABRSD
035200130705     C                   EVAL      VABRSD=%trim(%subst(vindta:20:35))
035300130705     C* ......VABIND
035400130705     C                   EVAL      VABIND=%trim(%subst(vindta:55:35))
035500130705     C* ......VABLOD
035600130705     C                   EVAL      VABLOD=%trim(%subst(vindta:90:25))
035700130705     C* ......VABPRD
035800130705     C                   EVAL      VABPRD=%trim(%subst(vindta:115:2))
035900130705     C* ......VABCAD
036000130705     C                   EVAL      PiStr=%trim(%subst(vindta:117:8))
036100130705     C                   EXSR      CHKNUM
036200130705     C                   IF        PiInt=*on
036300130705     C                   Z-ADD     PiVal         Num5_0
036400130705     C                   MOVEL(P)  Num5_0        VABCAD
036500130705     C                   ELSE
036600130705     C                   ADD       1             errore
036700130705     C                   EVAL      vinmsg = %trimr(vinmsg)
036800130705     C                             + ' ' + 'VABCAD'
036900130705     C                   ENDIF
037000130705     C*
037100130705     C                   ENDIF
037200130422     C***
037300130422     C* ...tipo record 'GD  '
037400130422     C                   IF        %subst(vindta:1:3) = 'GD '
037500060202     C* ......VABNCL
037600130422     C                   EVAL      PiStr=%trim(%subst(vindta:54:7))
037700060202     C                   EXSR      CHKNUM
037800060202     C                   IF        PiInt=*on
037900060202     C                   Z-ADD     PiVal         VABNCL
038000060202     C                   ELSE
038100060202     C                   ADD       1             errore
038200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
038300060202     C                             + ' ' + 'VABNCL'
038400060202     C                   ENDIF
038500060202     C* ......VABPKB
038600130422     C                   EVAL      PiStr=%trim(%subst(vindta:61:10))
038700060202     C                   EXSR      CHKNUM
038800060202     C                   IF        PiNum=*on
038900130422     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
039000070108     C                   Z-ADD(H)  PiVal         VABPKB
039100060202     C                   ELSE
039200060202     C                   ADD       1             errore
039300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
039400060202     C                             + ' ' + 'VABPKB'
039500060202     C                   ENDIF
039600170418     C* ......VABVLB
039700170418     C                   EVAL      PiStr=%trim(%subst(vindta:83:8))
039800170418     C                   EXSR      CHKNUM
039900170418     C                   IF        PiNum=*on
040000170418     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
040100170418     C                   Z-ADD(H)  PiVal         VABVLB
040200170418     C                   ELSE
040300170418     C                   ADD       1             errore
040400170418     C                   EVAL      vinmsg = %trimr(vinmsg)
040500170418     C                             + ' ' + 'VABVLB'
040600170418     C                   ENDIF
040700130422     C*
040800130422     C                   ENDIF
040900130422     C***
041000130422     C* ...tipo record 'RE  '
041100130422     C                   IF        %subst(vindta:1:3) = 'RE '
041200130422     C*
041300060202     C* ......VABNOT
041400130422     C                   EVAL      w60=%trim(%subst(vindta:5:60))
041500130422     C                   EVAL      VABNOT=%subst(w60:1:35)
041600130422     C                   EVAL      VABNT2=%subst(w60:36:25)
041700130422     C*
041800130422     C                   ENDIF
041900130422     C***
042000130422     C* ...tipo record 'MA  '
042100130422     C                   IF        %subst(vindta:1:3) = 'MA '
042200130422     C*
042300071206     C* ......VABCAS
042400130422     C                   IF        %trim(%subst(vindta:18:13)) <> *blanks and
042500130422     C                             %trim(%subst(vindta:18:13)) <> *zeros
042600071206     C                   EVAL      FlgCAS = '1'
042700130422     C                   EVAL      VABVCA = 'EUR'
042800130422     C                   EVAL      PiStr=%trim(%subst(vindta:18:13))
042900080923     C                   EXSR      CHKNUM
043000080923     C                   IF        PiNum=*on
043100130422     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
043200080923     C                   ELSE
043300080923     C                   ADD       1             errore
043400080923     C                   EVAL      vinmsg = %trimr(vinmsg)
043500080925     C                             + ' ' + 'VABCAS'
043600080923     C                   ENDIF
043700080923     C                   ENDIF
043800161030     C*
043900161030     C* ......VABTIC
044000161030     C                   SELECT
044100161030     C                   WHEN      %trim(%subst(vindta:6:2)) = 'AP'
044200161030     C                   EVAL      VABTIC = 'TO'
044300161030     C                   WHEN      %trim(%subst(vindta:6:2)) = 'CB'
044400161030     C                   EVAL      VABTIC = 'BM'
044500161030     C                   ENDSL
044600130422     C*
044700080923     C                   ENDIF
044800150519     C***
044900150519     C* ...tipo record 'DO  '
045000150519     C                   IF        %subst(vindta:1:3) = 'DO '
045100150519     C* ...tipo 'M'
045200150519     C                             and %subst(vindta:4:1) = 'M'
045300150519     C*
045400150519     C* ......VABRMA
045500150519     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
045600150519     C                   ENDIF
045700130422     C***
045800130422     C* ...tipo record 'BC  '
045900130422     C                   IF        %subst(vindta:1:3) = 'BC '
046000130422     C*
046100071206     C* ......VATNOT_E
046200130503     C***                EVAL      wNOT_E_DS=%trim(%subst(vindta:4:175))
046300130503     C* gestisco un tipo servizio A
046400130503     C                   CLEAR                   wNOT_E_DS
046500071206     C*
046600130422     C* ciclo per i possibili 7 barcode
046700130422     C     1             DO        7             wI                3 0
046800130422     C                   IF        wNOT_E_SK(wI) <> *blanks
046900071206     C                   EVAL      VATTRC = 'E'
047000130422     C                   EVAL      VATNOT = wNOT_E_SK(wI)
047100071206     C                   WRITE     FIVAT000
047200071206     C                   ENDIF
047300130422     C                   ENDDO
047400071206     C*
047500071206     C                   ENDIF
047600141003     C*
047700141003     C***
047800141003     C* ...tipo record 'PAR'
047900141003     C                   IF        %subst(vindta:1:3) = 'PAR'
048000141003     C                   IF        %subst(vindta:15:20) <> *blank
048100141003     C                   EVAL      VATTRC = 'B'
048200141003     C                   EVAL      VATNOT = %trim(%subst(vindta:15:20))
048300141003     C                   WRITE     FIVAT000
048400141003     C                   ENDIF
048500141003     C                   ENDIF
048600141003     C*
048700141003     C***
048800141003     C* ...tipo record 'EV '
048900141003     C                   IF        %subst(vindta:1:3) = 'EV '
049000141003     C                   IF        %subst(vindta:7:6) = 'PRENOT'
049100141003     C                   EVAL      VABTC1 = 'A'
049200141003     C                   ENDIF
049300141003     C                   ENDIF
049400141003     C*
049500121023     C***
049600010202     C*
049700000801     C* Ebbene...
049800000801     C                   ADD       1             §CTRMO
049900010201     C                   IF        errore <> *zeros
050000000801     C                   ADD       1             §CTRNO
050100000801     C                   EVAL      vinflg = '2'
050200000801     C                   ELSE
050300010201     C                   ADD       1             §CTROKVB
050400000801     C                   ENDIF
050500000801     C*
050600000801     C                   ENDSR
050700071206     C*----------------------------------------------------*
050800071206     C*  WRIVAB - SCARICO BUFFER FILE TESTATA - FNVAB
050900071206     C*----------------------------------------------------*
051000071206     C     WRIVAB        BEGSR
051100071206     C*
051200071206     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051300071206     C                   IF        FlgCAS <> '0'
051400071206     C                   IF        VABCBO = '1'
051500071206     C                   EVAL      VABCBO = '4'
051600071206     C                   ELSE
051700071206     C                   EVAL      VABCBO = '6'
051800071206     C                   ENDIF
051900071206     C                   ENDIF
052000071206     C*
052100071206     C* Se effettuata bancalatura overrido il numero dei colli in testata con il numero d
052200071206     C* bancali (e evidenzio in natura merce)
052300071206     C                   IF        wNCL <> *zeros
052400071206     C                   EVAL      VABNAS = 'Cl.' + %trim(%char(VABNCL)) +
052500071206     C                                      ' Banc.' + %subst(wNCL:3:3)
052600071206     C                   MOVE(P)   wNCL          VABNCL
052700071206     C                   ENDIF
052800071206     C*
052900071206     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053000071206     C                   EXSR      CHKIMPDIV
053100071206     C* ...e in fine...
053200071206     C                   WRITE     FIVAB000
053300071206     C*
053400071206     C                   ENDSR
053500010202     C*----------------------------------------------------*
053600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053700010202     C*----------------------------------------------------*
053800020305     C     PREVAT        BEGSR
053900010202     C*
054000021113     C* Compongo il nome del membro da dare al FIVATWWR
054100010202     C                   eval      parmbr = vlrhdl
054200010202     C                   movel     'M'           parmbr
054300060113     C                   eval      parccm = vlrksc
054400010202     C                   eval      paropz = '1'
054500010202     C* Effettuo la chiamata al CLLE preposto
054600040506     C                   call(e)   'TITVVTC'
054700010202     C                   parm                    parccm
054800010202     C                   parm                    parmbr
054900010202     C                   parm                    paropz
055000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055100010202     C                   if        %error
055200010202     C                   movel     '1'           chkcall
055300010202     C                   else
055400010202     C                   movel     '0'           chkcall
055500010202     C                   endif
055600010202     C*
055700010202     C                   ENDSR
055800000801     C*----------------------------------------------------*
055900000801     C*  CONTROLLO NUMERICITA' CAMPI
056000000801     C*----------------------------------------------------*
056100000801     C     CHKNUM        BEGSR
056200000801     C*
056300000801     C                   call(e)   'ISNUMERIC'
056400000801     C                   PARM                    PiStr            30
056500040714     C                   PARM      ','           PiDecChr          1
056600000801     C                   PARM      *ZEROS        PiVal            30 9
056700000801     C                   PARM      '0'           PiInt             1
056800000801     C                   PARM      '0'           PiNum             1
056900000801     C                   IF        %error
057000000801     C                   EVAL      PiInt=*off
057100000801     C                   ENDIF
057200000801     C*
057300000801     C                   ENDSR
057400000801     C***
057500000801
057600011113
057700011113     C*----------------------------------------------------*
057800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057900011113     C*----------------------------------------------------*
058000011113     C     CHKIMPDIV     BEGSR
058100011113     C*
058200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058300011113     C                   Z-ADD     *zeros        wrkDec            9 9
058400011113     C*
058500011113     C* Come prima cosa effettuo considerazioni sulla divisa
058600011113     C                   IF        vabIAS > *zeros
058700011113     C                   IF        vabVAS <> 'EUR'
058800011113     C                   EVAL      vabVAS =  'ITL'
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C                   IF        vabCAS > *zeros
059300011113     C                   IF        vabVCA <> 'EUR'
059400011113     C                   EVAL      vabVCA =  'ITL'
059500011113     C                   ENDIF
059600011113     C                   ENDIF
059700011113     C*
059800011113     C                   IF        vabVMD > *zeros
059900020305     C                   IF        vabVAD <> 'EUR'
060000011113     C                   EVAL      vabVAD =  'ITL'
060100011113     C                   ENDIF
060200011113     C                   ENDIF
060300011113     C*
060400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060500011113     C                   Z-ADD     vabIAS        wrkDec
060600011113     C                   IF        wrkDec > *zeros
060700011113     C                   IF        vabVAS = 'ITL'
060800011113     C                   EVAL      vabIAS = *zeros
060900011113     C                   ENDIF
061000011113     C                   ENDIF
061100011113     C*
061200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061300011113     C                   Z-ADD     vabCAS        wrkDec
061400011113     C                   IF        wrkDec > *zeros
061500011113     C                   IF        vabVCA = 'ITL'
061600011113     C                   EVAL      vabCAS = *zeros
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062100011113     C                   Z-ADD     vabVMD        wrkDec
062200011113     C                   IF        wrkDec > *zeros
062300011113     C                   IF        vabVAD = 'ITL'
062400011113     C                   EVAL      vabVMD = *zeros
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C                   ENDSR
062900011113     C***
063000011113
063100011113
063200000801
063300000801
063400990920      /TITLE Invio dei dati al punto operativo.
063500010202     C     invio         BEGSR
063600990920     C*
063700021113     C* 1° invio FIVAT
063800010201     C                   reset                   dscmz
063900010201     C                   move      vlrpoi        cmzdst
064000021113     C                   eval      cmzfld = 'FIVATWWR'
064100010201     C                   eval      cmzmbd = vlrhdl
064200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064300021009     C***                if        prmfir = *blanks
064400021113     C                   eval      cmzfla = 'FIVAT00F'
064500021113     C                   eval      cmzmba = 'FIVAT00F'
064600021009     C***                else
064700021009     C***                eval      cmzfla = prmfir
064800021009     C***                eval      cmzmba = prmfir
064900021009     C***                endif
065000010201     C                   eval      cmznrr = *zeros
065100020305     C                   move      §ctrokvt      cmznrr
065200021018     C                   eval      cmzlba = vlrfl1
065300010201     C                   call(e)   'TIS711C'
065400010201     C                   parm                    dscmz
065500010201     C                   parm      *blanks       esito
065600010205     C                   if        %error
065700010205     C                             or cmzerr = '1'
065800010205     C                             or esito  = '1'
065900010205     C                   eval      wrkesito = '3'
066000010205     C                   else
066100010201     C*
066200021113     C* 2° invio FIVAB
066300010201     C                   reset                   dscmz
066400010201     C                   move      vlrpoi        cmzdst
066500010201     C                   eval      cmzfld = vlrfou
066600010201     C                   eval      cmzmbd = vlrhdl
066700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066800021009     C***                if        prmfir = *blanks
066900021113     C                   eval      cmzfla = 'FIVAB00F'
067000021113     C                   eval      cmzmba = 'FIVAB00F'
067100021009     C***                else
067200021009     C***                eval      cmzfla = prmfir
067300021009     C***                eval      cmzmba = prmfir
067400021009     C***                endif
067500010201     C                   eval      cmznrr = *zeros
067600010201     C                   move      §ctrokvb      cmznrr
067700021018     C                   eval      cmzlba = vlrfl1
067800010201     C                   call(e)   'TIS711C'
067900010201     C                   parm                    dscmz
068000010201     C                   parm      *blanks       esito
068100010201     C                   if        %error
068200010201     C                             or cmzerr = '1'
068300010201     C                             or esito  = '1'
068400010201     C                   eval      wrkesito = '3'
068500010201     C                   endif
068600010205     C                   endif
068700990920     C*
068800000613     C                   ENDSR
068900000613     C***
069000070411
069100070411     C     *pssr         BEGSR
069200070411     C*
069300070411     C                   if        %open(tivin00r)
069400070411     C                   close     tivin00r
069500070411     C                   endif
069600070411     C                   if        %open(fivabwwr)
069700070411     C                   close     fivabwwr
069800070411     C                   endif
069900070411     C                   if        %open(fivatwwr)
070000070411     C                   close     fivatwwr
070100070411     C                   endif
070200070411     C*
070300070411     C* Effettuo la chiamata al CLLE preposto
070400070411     C                   call(e)   'TITVVTC'
070500070411     C                   parm                    parccm
070600070411     C                   parm                    parmbr
070700070411     C                   parm      '2'           paropz
070800070411     C*
070900070411     C                   eval      wrkesito = '2'
071000070411     C*
071100070411     C                   seton                                        LR
071200070411     C*
071300070411     C                   ENDSR     '*CANCL'
071400070411     C***
071500070411
071600990910
071700000613     C     *inzsr        BEGSR
071800990910     C*
071900990910     C     *entry        plist
072000990920     C                   parm                    tivlrds
072100990921     C                   parm      wrkesito      esito
072200000724     C                   parm                    prmlit
072300000710     C                   parm                    prmfir
072400000613     C*
072500000830     C* CALCOLA LA DATA CORRENTE
072600000830     C                   time                    wn14             14 0
072700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072800000830     C                   z-add     wn8           g08dat
072900000830     C                   z-add     *zeros        g08inv
073000000830     C                   movel     '0'           g08err
073100000830     C                   call      'XSRDA8'
073200000830     C                   parm                    wlbda8
073300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073400000830     C*
073500000613     C                   ENDSR
073600000613     C***
