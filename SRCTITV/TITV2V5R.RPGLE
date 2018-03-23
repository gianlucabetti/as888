000100150813      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300120207     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600150813     FEDIVABwr  O    E             DISK    usropn
000700150813     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600040506     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600120130     D wOut            s              1    INZ('0')
002700120207     D wNomeFile       s             30    INZ(*blanks)
002800000830
002900041025     D*------------------
003000041025     D* DS REPERIMENTO NUMERATORE
003100041025     D*------------------
003200041025     D trul33ds      e ds                  inz
003300041025     D*------------------
003400041025     D* DS ARCHITETTURA
003500041025     D*------------------
003600041025     D kpjba         e ds                  inz
003700060913     D*------------------
003800060913     D* DS RIDEFINIZIONE TIPO RECORD CLIENTE '25' - Informazioni aggiuntive
003900060913     D*------------------
004000060913     D dsRT25          ds                  inz
004100060928     D  dRT25_RT25                    2
004200060913     D  dRT25_TC1                     1
004300060913     D  dRT25_VATB                   25
004400071031     D  dRT25_VATA                   26
004500060913     D  dRT25_DCRDD                   2
004600060913     D  dRT25_DCRMM                   2
004700060913     D  dRT25_DCRYYYY                 4
004800060913     D*------------------
004900990908
005000081222
005100081222     D*------------------
005200081222     D* LINKING A DEFINIZIONI ESTERNE
005300081222     D*------------------
005400081222     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500081222     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600010201
005700010201
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000613     C*
006200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006300000613     C*
006400010202     C* Effettuo la chiamata al CLLE preposto
006500150813     C                   call(e)   'TITVEVTC'
006600010202     C                   parm                    parccm
006700010202     C                   parm                    parmbr
006800010202     C                   parm      '2'           paropz
006900040929     C*
007000040929     C* Esegue lancio TISI95R solo x chiusura
007100040929     C                   CLEAR                   TISI95DS
007200040929     C                   EVAL      I95TLA = 'C'
007300040929     C                   CALL      'TISI95R'
007400040929     C                   PARM                    TISI95DS
007500000616     C*
007600010201     C                   seton                                        LR
007700990908
007800000801
007900910830     C*--------------------------------------------------------
008000150813     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008100910830     C*--------------------------------------------------------
008200040526     C     RWFILE        BEGSR
008300990910     C*
008400990914     C                   if        not %open(tivin00r)
008500990908     C                   open      tivin00r
008600990914     C                   endif
008700150813     C                   if        not %open(edivabwr)
008800150813     C                   open      edivabwr
008900990914     C                   endif
009000150813     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009100020305     C                   exsr      prevat
009200010201     C*
009300010202     C                   if        chkcall = '0'
009400010202     C*
009500150813     C                   if        not %open(edivatwr)
009600150813     C                   open      edivatwr
009700010201     C                   endif
009800990910     C*
009900010201     C                   clear                   §CTROKVB          5 0
010000020305     C                   clear                   §CTROKVT          5 0
010100000801     C                   clear                   §CTRMO            5 0
010200000801     C                   clear                   §CTRNO            5 0
010300040910     C*
010400921023     C                   DO        *HIVAL
010500990913     C*
010600990915     C                   READ      tivin00r                               70
010700040910     C                   if        vindta > *blanks
010800000613     C                   add       1             rrnum
010900000801     C*
011000000801     C                   if        *in70 = *off
011100000801     C                             and
011200000801     C                             (vinflg = *blanks
011300000801     C                              or vinflg = '0'
011400000801     C                              or vinflg = '2')
011500000801     C*
011600000801     C                   clear                   vinmsg
011700000801     C                   eval      vinflg = '1'
011800040910     C*
011900040910     C* Eseguo routine d traduzione
012000040910     C                   exsr      impvabvat
012100040802     C*
012200010305     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100010202     C*
013200120130     C* Scarico il buffer d output in sospeso ...
013300150813     C                   WRITE     EDIVAB00
013400120130     C*
013500010202     C                   endif
013600990910
013700990910     C* Se non ci sono record con errori ...
013800000710     C                   if        §ctrno = 0
013900990910     C* ... restituisco esito OK.
014000990921     C                   eval      wrkesito = '0'
014100990910     C                   else
014200010201     C                   if        §ctrokvb > 0
014300990921     C                   eval      wrkesito = '1'
014400000710     C                   else
014500000710     C                   eval      wrkesito = '2'
014600990910     C                   endif
014700000710     C                   endif
014800990910     C*
014900990914     C                   if        %open(tivin00r)
015000990908     C                   close     tivin00r
015100990914     C                   endif
015200150813     C                   if        %open(edivabwr)
015300150813     C                   close     edivabwr
015400990914     C                   endif
015500150813     C                   if        %open(edivatwr)
015600150813     C                   close     edivatwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   if        §ctrokvb > 0
016000000724     C                             and vlrpoi <> *zeros
016100010202     C                   exsr      invio
016200990920     C                   endif
016300990920     C*
016400910830     C                   ENDSR
016500000613     C***
016600990920
016700000801     C*----------------------------------------------------*
016800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016900000801     C*----------------------------------------------------*
017000010201     C     INZVAR        BEGSR
017100000801     C*
017200040802     C                   Z-ADD     *zeros        Num5_0            5 0
017300040802     C                   MOVEL     '0'           FlgCAS            1
017400000801     C*
017500000801     C                   ENDSR
017600000801     C*----------------------------------------------------*
017700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017800000801     C*----------------------------------------------------*
017900000801     C     DEFCAM        BEGSR
018000000801     C*
018100150813     C                   CLEAR                   EDIVAB00
018200150813     C                   CLEAR                   EDIVAT00
018300020619     C* Imposto i valori di default...
018400160901     C                   Z-ADD     0896508       VABCCM
018500160901     C                   Z-ADD     0896508       VATCCM
018600160901     C                   Z-ADD     089           VABLNP
018700160901     C                   Z-ADD     089           VATLNP
018800141024     C                   Z-ADD     000           VABCTR
018900040714     C                   MOVEL     '7Q'          VABCTM
019000040823     C                   MOVEL     '1'           VABCBO
019100020619     C* ... e poi verifico se sono stati passati come parametri
019200020619     C                   IF        vlrppt > *blanks
019300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019500020619     C                   EXSR      CHKNUM
019600020619     C                   IF        PiInt=*on
019700020619     C                   Z-ADD     PiVal         VABCCM
019800020619     C                   Z-ADD     PiVal         VATCCM
019900020619     C                   ENDIF
020000040506     C                   ENDIF
020100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020300020619     C                   EXSR      CHKNUM
020400020619     C                   IF        PiInt=*on
020500020619     C                   Z-ADD     PiVal         VABLNP
020600020619     C                   Z-ADD     PiVal         VATLNP
020700040506     C                   ENDIF
020800020619     C                   ENDIF
020900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021100020619     C                   EXSR      CHKNUM
021200020619     C                   IF        PiInt=*on
021300020619     C                   Z-ADD     PiVal         VABCTR
021400040506     C                   ENDIF
021500020619     C                   ENDIF
021600070427     C                   IF        %subst(vlrppt:14:1) <> *blanks
021700070427     C                   EVAL      VABTSP = %subst(vlrppt:14:1)
021800070427     C                   ENDIF
021900020619     C                   ENDIF
022000000801     C*
022100000801     C                   ENDSR
022200000801     C*----------------------------------------------------*
022300150813     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
022400000801     C*----------------------------------------------------*
022500040910     C     IMPVABVAT     BEGSR
022600040910     C*
022700040910     C* Traduzione relativa ai tipi record del file del cliente
022800040910     C*
022900040910     C***
023000040910     C* ...tipo record 11
023100040910     C                   IF        %subst(vindta:1:2) = '11'
023200120130     C*
023300120130     C* Se buffer da scaricare => scarico ...
023400120130     C                   IF        wOut = '1'
023500150813     C                   WRITE     EDIVAB00
023600120130     C                   ENDIF
023700120130     C*
023800120130     C                   EVAL      wOut = '1'
023900120130     C*
024000040910     C* ......inizializzazioni iniziali e formati record file Bartolini
024100040910     C                   EXSR      INZVAR
024200040910     C                   EXSR      DEFCAM
024300081222     C*
024400081222     C                   Z-ADD     *zeros        errore            1 0
024500081222     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
024600081222     C                   MOVEL     datcor        VABAAS
024700081222     C                   MOVEL     datcor        VATAAS
024800081222     C                   MOVE      datcor        VABMGS
024900081222     C                   MOVE(P)   vlrpoi        VABFGS
025000081222     C                   MOVE(P)   vlrpoi        VATFGS
025100150813     C*
025200150813     C* ......Dati CMR
025300160901     C                   EVAL      VABCMR = 'Manifest: ' +
025400151203     C                                      %subst(vindta:16:2) + '.'   +
025500151203     C                                      %subst(vindta:14:2) + '.20' +
025600151203     C                                      %subst(vindta:12:2) +
025700160901     C                                      '  -  ' +
025800160901     C                                      %subst(wNomeFile:5:18-5)
025900151203     C***                EVAL      VABCMR = wNomeFile
026000150813     C                   EVAL      VABDCM = datcor
026100150813     C                   EVAL      VABDTS = datcor
026200150813     C                   EVAL      VABCNT = 1
026300150813     C                   EVAL      VATCMR = VABCMR
026400150813     C                   EVAL      VATCNT = VABCNT
026500120130     C*
026600040910     C* ......VABNSP/VATNSP
026700041025     C*                  EVAL      PiStr=%trim(%subst(vindta:21:6))
026800041025     C*                  EXSR      CHKNUM
026900041025     C*                  IF        PiInt=*on
027000041025     C*                  Z-ADD     PiVal         VABNSP
027100041025     C*                  Z-ADD     PiVal         VATNSP
027200041025     C*                  ELSE
027300041025     C*                  ADD       1             errore
027400041025     C*                  EVAL      vinmsg = %trimr(vinmsg)
027500041025     C*                            + ' ' + 'VABNSP/VATNSP'
027600041025     C*                  ENDIF
027700041025     C* NSP => Stacco un numeratore da AZNUM
027800041025     C                   clear                   TRUL33DS
027900041025     C                   eval      I33OPE = *zeros
028000041025     C                   eval      I33CNU = 302
028100041025     C                   eval      I33NUM = 1
028200041025     C                   movel     TRUL33DS      KPJBU
028300041025     C                   call      'TRUL33R'
028400041025     C                   parm                    KPJBA
028500041025     C                   movel     KPJBU         TRUL33DS
028600041025     C                   if        O33ERR = *zeros
028700041025     C                   z-add     O33NRF        VABNSP
028800041025     C                   z-add     O33NRF        VATNSP
028900041025     C                   else
029000041025     C                   Z-ADD     1             errore
029100041025     C                   EVAL      vinmsg = %trimr(vinmsg)
029200041025     C                             + ' ' + 'VABNSP VATNSP'
029300041025     C                   endif
029400040910     C* ......VABRSD
029500040910     C                   EVAL      VABRSD=%trim(%subst(vindta:28:30))
029600060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
029700060928     C     '@':'A'       XLATE     VABRSD        VABRSD
029800060928     C     '§':'S'       XLATE     VABRSD        VABRSD
029900060928     C     '!':' '       XLATE     VABRSD        VABRSD
030000060928     C     ':':' '       XLATE     VABRSD        VABRSD
030100060928     C*
030200040910     C                   EVAL      VABRD2=%trim(%subst(vindta:58:30))
030300060928     C* == sostituisco caratteri che bloccano POSTEL/piccione ==
030400060928     C     '@':'A'       XLATE     VABRD2        VABRD2
030500060928     C     '§':'S'       XLATE     VABRD2        VABRD2
030600060928     C     '!':' '       XLATE     VABRD2        VABRD2
030700060928     C     ':':' '       XLATE     VABRD2        VABRD2
030800040910     C*
030900040910     C                   ENDIF
031000040910     C***
031100040910     C* ...tipo record 12
031200040910     C                   IF        %subst(vindta:1:2) = '12'
031300040910     C* ......VABIND
031400040910     C                   EVAL      VABIND=%trim(%subst(vindta:57:30))
031500141024     C                   IF        VABIND = *blanks AND VABRD2 <> *blanks
031600141024     C                   EVAL      VABIND = VABRD2
031700141024     C                   EVAL      VABRD2 = *blanks
031800141024     C                   ENDIF
031900040910     C* ......VABCAD
032000040910     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
032100040910     C                   EXSR      CHKNUM
032200040910     C                   IF        PiInt=*on
032300040910     C                   Z-ADD     PiVal         Num5_0
032400040910     C                   MOVEL(P)  Num5_0        VABCAD
032500040910     C                   ELSE
032600040910     C                   ADD       1             errore
032700040910     C                   EVAL      vinmsg = %trimr(vinmsg)
032800040910     C                             + ' ' + 'VABCAD'
032900040910     C                   ENDIF
033000040910     C* ......VABLOD
033100040910     C                   EVAL      VABLOD=%trim(%subst(vindta:100:25))
033200040910     C* Reperisco la provincia dal CAP e dalla località
033300040910     C                   IF        VABCAD <> *blanks AND
033400040910     C                             VABPRD  = *blanks
033500040910     C                   CLEAR                   TISI95DS
033600040910     C                   EVAL      I95TCN = '3'
033700040910     C                   Z-ADD     datcor        I95DAT
033800040910     C                   EVAL      I95CAP = VABCAD
033900040910     C                   EVAL      I95LOC = VABLOD
034000040910     C                   CALL      'TISI95R'
034100040910     C                   PARM                    TISI95DS
034200040910     C                   EVAL      VABPRD = O95PRV
034300040910     C                   ENDIF
034400160901     C* ......VABFFD
034500141126     C                   IF        %subst(vindta:125:2) = 'SX'
034600141126     C                   EVAL      VABFFD = 'S'
034700141126     C                   ENDIF
034800040910     C*
034900040910     C                   ENDIF
035000040910     C***
035100040910     C* ...tipo record 13
035200040910     C                   IF        %subst(vindta:1:2) = '13'
035300050719     C                   Z-ADD     *zeros        VABNCL
035400081222     C* ......VABRMA
035500161115     C                   EVAL      VABRMA=%trim(%subst(vindta:95:10))
035600120130     C* ......VABRMN
035700161115     C                   EVAL      PiStr=%trim(%subst(vindta:80:10))
035800120130     C                   EXSR      CHKNUM
035900120130     C                   IF        PiInt=*on
036000120130     C                   ADD       PiVal         VABRMN
036100120130     C                   ELSE
036200120130     C                   ADD       1             errore
036300120130     C                   EVAL      vinmsg = %trimr(vinmsg)
036400120130     C                             + ' ' + 'VABRMN'
036500120130     C                   ENDIF
036600040910     C* ......VABNCL
036700141024     C                   SELECT
036800141024     C                   WHEN      %trim(%subst(vindta:31:3))<>*all'0'          * colli
036900141024     C                   EVAL      VABCTR = 000
037000141024     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
037100141024     C*
037200141024     C                   WHEN      %trim(%subst(vindta:27:2))<>*all'0'          * bancali
037300161115     C                   EVAL      VABCTR = 000
037400141024     C                   EVAL      PiStr=%trim(%subst(vindta:27:2))
037500141024     C                   ENDSL
037600141024     C*
037700040910     C                   EXSR      CHKNUM
037800040910     C                   IF        PiInt=*on
037900050719     C                   ADD       PiVal         VABNCL
038000040910     C                   ELSE
038100040910     C                   ADD       1             errore
038200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
038300040910     C                             + ' ' + 'VABNCL'
038400040910     C                   ENDIF
038500040910     C* ......VABPKB
038600040910     C                   EVAL      PiStr=%trim(%subst(vindta:34:7))
038700040910     C                   EXSR      CHKNUM
038800040910     C                   IF        PiNum=*on
038900040910     C                   EVAL(H)   VABPKB=PiVal/100                             * gestisco 2 dec.
039000040910     C                   ELSE
039100040910     C                   ADD       1             errore
039200040910     C                   EVAL      vinmsg = %trimr(vinmsg)
039300040910     C                             + ' ' + 'VABPKB'
039400040910     C                   ENDIF
039500040929     C* ......VABCBO
039600090701     C                   IF        %subst(vindta:46:1) = '4'
039700040929     C                   EVAL      VABCBO='2'
039800040929     C                   ELSE
039900040929     C                   EVAL      VABCBO='1'
040000040929     C                   ENDIF
040100040910     C* ......VABDCR
040200040910     C                   IF        %subst(vindta:47:6) <> *zeros
040300040910     C                   EVAL      PiStr='20'+%trim(%subst(vindta:47:6))
040400040910     C                   EXSR      CHKNUM
040500040910     C                   IF        PiInt=*on
040600040910     C                   Z-ADD     PiVal         VABDCR
040700040910     C                   ELSE
040800040910     C                   ADD       1             errore
040900040910     C                   EVAL      vinmsg = %trimr(vinmsg)
041000040910     C                             + ' ' + 'VABDCR'
041100040910     C                   ENDIF
041200040910     C                   ENDIF
041300040910     C* ......VABHCR
041400040910     C                   EVAL      PiStr=%trim(%subst(vindta:53:4))
041500040910     C                   EXSR      CHKNUM
041600040910     C                   IF        PiInt=*on
041700040910     C                   Z-ADD     PiVal         VABHCR
041800040910     C                   ELSE
041900040910     C                   ADD       1             errore
042000040910     C                   EVAL      vinmsg = %trimr(vinmsg)
042100040910     C                             + ' ' + 'VABHCR'
042200040910     C                   ENDIF
042300040910     C* ......VABCAS
042400040910     C                   IF        %subst(vindta:57:9) <> *zeros
042500040910     C                   EVAL      FlgCAS = '1'
042600040910     C                   ENDIF
042700040910     C                   EVAL      PiStr=%trim(%subst(vindta:57:9))
042800040910     C                   EXSR      CHKNUM
042900040910     C                   IF        PiNum=*on
043000040910     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
043100040910     C                   ELSE
043200040910     C                   ADD       1             errore
043300040910     C                   EVAL      vinmsg = %trimr(vinmsg)
043400040910     C                             + ' ' + 'VABCAS'
043500040910     C                   ENDIF
043600110621     C* ......VABTIC
043700110621     C                   SELECT
043800110621     C                   WHEN      %subst(vindta:66:1) = '3'
043900161115     C                   EVAL      VABTIC='BM'
044000110621     C                   WHEN      %subst(vindta:66:1) = '5'
044100110621     C                   EVAL      VABTIC='OM'
044200161115     C                   OTHER
044300161115     C                   EVAL      VABTIC=*blanks
044400110621     C                   ENDSL
044500040910     C* ......VABVCA
044600040910     C                   EVAL      VABVCA=%trim(%subst(vindta:67:3))
044700040910     C* ......VABIAS
044800060126     C*                  EVAL      PiStr=%trim(%subst(vindta:106:9))
044900060126     C*                  EXSR      CHKNUM
045000060126     C*                  IF        PiNum=*on
045100060126     C*                  EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
045200060126     C*                  ELSE
045300060126     C*                  ADD       1             errore
045400060126     C*                  EVAL      vinmsg = %trimr(vinmsg)
045500060126     C*                            + ' ' + 'VABIAS'
045600060126     C*                  ENDIF
045700040910     C* ......VABVAS
045800040910     C                   EVAL      VABVAS=%trim(%subst(vindta:115:3))
045900040910     C*
046000040910     C                   ENDIF
046100040910     C***
046200040910     C* ...tipo record 14
046300040910     C                   IF        %subst(vindta:1:2) = '14'
046400081222     C* ......VATNOT - "E"
046500161111     C***                EVAL      VATNOT=%trim(%subst(vindta:29:24))
046600161111     C                   EVAL      VATNOT=%trim(%subst(vindta:27:24))
046700040910     C                   EVAL      VATTRC = 'E'
046800150813     C* x ogni tipo record 14 devo scaricare il buffer del file EDIVAT
046900150813     C                   WRITE     EDIVAT00
047000040910     C*
047100040910     C                   ENDIF
047200041111     C***
047300041111     C* ...tipo record 22
047400041111     C                   IF        %subst(vindta:1:2) = '22'
047500041111     C* ......VABRMO
047600041111     C                   EVAL      VABRMO=%trim(%subst(vindta:3:30))
047700041111     C                   ENDIF
047800060913     C***
047900060914     C* ...tipo record 25
048000071031     C                   IF        %subst(vindta:1:2) = '25'
048100071031     C*
048200071031     C                   EVAL      dsRT25 = vindta
048300071119     C* Consegna particolare (solo valori ben determinati)
048400071119     C                   IF        dRT25_TC1 = 'A' or
048500071119     C                             dRT25_TC1 = 'P' or
048600071119     C                             dRT25_TC1 = 'S'
048700071031     C                   EVAL      VABTC1 = dRT25_TC1
048800071119     C                   ENDIF
048900060913     C* Telefono destinatario
049000071031     C                   EVAL      VATNOT = dRT25_VATB
049100071031     C                   EVAL      VATTRC = 'B'
049200150813     C                   WRITE     EDIVAT00
049300061013     C* Referente consegna
049400071031     C                   EVAL      VATNOT = dRT25_VATA
049500071031     C                   EVAL      VATTRC = 'A'
049600150813     C                   WRITE     EDIVAT00
049700060913     C* Data consegna richiesta
049800071219     C                   EVAL      PiStr=dRT25_DCRYYYY+dRT25_DCRMM+dRT25_DCRDD
049900071219     C                   EXSR      CHKNUM
050000071219     C                   IF        PiInt=*on
050100071219     C                   Z-ADD     PiVal         VABDCR
050200071219     C                   ELSE
050300071219     C                   Z-ADD     *zeros        VABDCR
050400071219     C                   ADD       1             errore
050500071219     C                   EVAL      vinmsg = %trimr(vinmsg)
050600071219     C                             + ' ' + 'VABDCR'
050700071219     C                   ENDIF
050800071031     C*
050900071031     C                   ENDIF
051000040910     C***
051100040910     C* ...tipo record 41
051200040910     C                   IF        %subst(vindta:1:2) = '41'
051300040910     C* ......VABNAS
051400040910     C                   EVAL      VABNAS=%trim(%subst(vindta:3:126))
051500040910     C*
051600040910     C                   ENDIF
051700040910     C***
051800040910     C* ...tipo record 42
051900040910     C                   IF        %subst(vindta:1:2) = '42'
052000040910     C* ......VABNOT/VABNT2
052100040910     C                   EVAL      VABNOT=%trim(%subst(vindta:3:35))
052200040910     C                   EVAL      VABNT2=%trim(%subst(vindta:3+35:35))
052300040910     C*
052400040910     C                   ENDIF
052500040910     C***
052600161115     C*
052700161115     C* Considerazioni finali su CBO/CAS
052800161115     C                   IF        FlgCAS = '1'    OR
052900161115     C                             VABCAS  > *zeros
053000161115     C                   IF        VABCBO = '1'
053100161115     C                   EVAL      VABCBO = '4'
053200161115     C                   ENDIF
053300161115     C                   IF        VABCBO = '2'
053400161115     C                   EVAL      VABCBO = '6'
053500161115     C                   ENDIF
053600161115     C                   ENDIF
053700010202     C*
053800000801     C* Ebbene...
053900000801     C                   ADD       1             §CTRMO
054000010201     C                   IF        errore <> *zeros
054100000801     C                   ADD       1             §CTRNO
054200000801     C                   EVAL      vinflg = '2'
054300000801     C                   ELSE
054400010201     C                   ADD       1             §CTROKVB
054500000801     C                   ENDIF
054600000801     C*
054700000801     C                   ENDSR
054800010202     C*----------------------------------------------------*
054900150813     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
055000010202     C*----------------------------------------------------*
055100020305     C     PREVAT        BEGSR
055200010202     C*
055300150813     C* Compongo il nome del membro da dare al EDIVATWR
055400010202     C                   eval      parmbr = vlrhdl
055500010202     C                   movel     'M'           parmbr
055600060113     C                   eval      parccm = vlrksc
055700010202     C                   eval      paropz = '1'
055800010202     C* Effettuo la chiamata al CLLE preposto
055900150813     C                   call(e)   'TITVEVTC'
056000010202     C                   parm                    parccm
056100010202     C                   parm                    parmbr
056200010202     C                   parm                    paropz
056300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056400010202     C                   if        %error
056500010202     C                   movel     '1'           chkcall
056600010202     C                   else
056700010202     C                   movel     '0'           chkcall
056800010202     C                   endif
056900010202     C*
057000010202     C                   ENDSR
057100000801     C*----------------------------------------------------*
057200000801     C*  CONTROLLO NUMERICITA' CAMPI
057300000801     C*----------------------------------------------------*
057400000801     C     CHKNUM        BEGSR
057500081222     C*
057600081222     C                   IF        PiDecChr = *blanks
057700081222     C                   EVAL      PiDecChr = ','
057800081222     C                   ENDIF
057900081222     C*
058000081222     C                   callp(e)  UBISNUM_Check(PiStr
058100081222     C                                          :PiDecChr
058200081222     C                                          :PiVal
058300081222     C                                          :PiNum
058400081222     C                                          :PiInt)
058500081222     C*
058600081222     C                   IF        %error
058700081222     C                   EVAL      PiInt=*off
058800081222     C                   ENDIF
058900000801     C*
059000000801     C                   ENDSR
059100000801     C***
059200000801
059300011113
059400011113     C*----------------------------------------------------*
059500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059600011113     C*----------------------------------------------------*
059700011113     C     CHKIMPDIV     BEGSR
059800011113     C*
059900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060000011113     C                   Z-ADD     *zeros        wrkDec            9 9
060100011113     C*
060200011113     C* Come prima cosa effettuo considerazioni sulla divisa
060300011113     C                   IF        vabIAS > *zeros
060400011113     C                   IF        vabVAS <> 'EUR'
060500011113     C                   EVAL      vabVAS =  'ITL'
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C                   IF        vabCAS > *zeros
061000011113     C                   IF        vabVCA <> 'EUR'
061100011113     C                   EVAL      vabVCA =  'ITL'
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   IF        vabVMD > *zeros
061600020305     C                   IF        vabVAD <> 'EUR'
061700011113     C                   EVAL      vabVAD =  'ITL'
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062200011113     C                   Z-ADD     vabIAS        wrkDec
062300011113     C                   IF        wrkDec > *zeros
062400011113     C                   IF        vabVAS = 'ITL'
062500011113     C                   EVAL      vabIAS = *zeros
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063000011113     C                   Z-ADD     vabCAS        wrkDec
063100011113     C                   IF        wrkDec > *zeros
063200011113     C                   IF        vabVCA = 'ITL'
063300011113     C                   EVAL      vabCAS = *zeros
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063800011113     C                   Z-ADD     vabVMD        wrkDec
063900011113     C                   IF        wrkDec > *zeros
064000011113     C                   IF        vabVAD = 'ITL'
064100011113     C                   EVAL      vabVMD = *zeros
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   ENDSR
064600011113     C***
064700011113
064800011113
064900000801
065000000801
065100990920      /TITLE Invio dei dati al punto operativo.
065200010202     C     invio         BEGSR
065300990920     C*
065400150813     C* 1° invio EDIVAT
065500010201     C                   reset                   dscmz
065600010201     C                   move      vlrpoi        cmzdst
065700150813     C                   eval      cmzfld = 'EDIVATWR'
065800010201     C                   eval      cmzmbd = vlrhdl
065900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000021009     C***                if        prmfir = *blanks
066100150813     C                   eval      cmzfla = 'EDIVAT0F'
066200150813     C                   eval      cmzmba = 'EDIVAT0F'
066300021009     C***                else
066400021009     C***                eval      cmzfla = prmfir
066500021009     C***                eval      cmzmba = prmfir
066600021009     C***                endif
066700010201     C                   eval      cmznrr = *zeros
066800020305     C                   move      §ctrokvt      cmznrr
066900021018     C                   eval      cmzlba = vlrfl1
067000010201     C                   call(e)   'TIS711C'
067100010201     C                   parm                    dscmz
067200010201     C                   parm      *blanks       esito
067300010205     C                   if        %error
067400010205     C                             or cmzerr = '1'
067500010205     C                             or esito  = '1'
067600010205     C                   eval      wrkesito = '3'
067700010205     C                   else
067800010201     C*
067900150813     C* 2° invio EDIVAB
068000010201     C                   reset                   dscmz
068100010201     C                   move      vlrpoi        cmzdst
068200010201     C                   eval      cmzfld = vlrfou
068300010201     C                   eval      cmzmbd = vlrhdl
068400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068500021009     C***                if        prmfir = *blanks
068600150813     C                   eval      cmzfla = 'EDIVAB0F'
068700150813     C                   eval      cmzmba = 'EDIVAB0F'
068800021009     C***                else
068900021009     C***                eval      cmzfla = prmfir
069000021009     C***                eval      cmzmba = prmfir
069100021009     C***                endif
069200010201     C                   eval      cmznrr = *zeros
069300010201     C                   move      §ctrokvb      cmznrr
069400021018     C                   eval      cmzlba = vlrfl1
069500010201     C                   call(e)   'TIS711C'
069600010201     C                   parm                    dscmz
069700010201     C                   parm      *blanks       esito
069800010201     C                   if        %error
069900010201     C                             or cmzerr = '1'
070000010201     C                             or esito  = '1'
070100010201     C                   eval      wrkesito = '3'
070200010201     C                   endif
070300010205     C                   endif
070400990920     C*
070500000613     C                   ENDSR
070600000613     C***
070700070411
070800071219     C     *pssr         BEGSR
070900070411     C*
071000071219     C                   if        %open(tivin00r)
071100071219     C                   close     tivin00r
071200071219     C                   endif
071300150813     C                   if        %open(edivabwr)
071400150813     C                   close     edivabwr
071500071219     C                   endif
071600150813     C                   if        %open(edivatwr)
071700150813     C                   close     edivatwr
071800071219     C                   endif
071900070411     C*
072000070411     C* Effettuo la chiamata al CLLE preposto
072100150813     C                   call(e)   'TITVEVTC'
072200071219     C                   parm                    parccm
072300071219     C                   parm                    parmbr
072400071219     C                   parm      '2'           paropz
072500071219     C*
072600071219     C                   eval      wrkesito = '2'
072700071219     C*
072800071219     C                   seton                                        LR
072900070411     C*
073000071219     C                   ENDSR     '*CANCL'
073100070411     C***
073200120207
073300070411
073400990910
073500000613     C     *inzsr        BEGSR
073600990910     C*
073700990910     C     *entry        plist
073800990920     C                   parm                    tivlrds
073900990921     C                   parm      wrkesito      esito
074000000724     C                   parm                    prmlit
074100000710     C                   parm                    prmfir
074200120207     C*
074300120207     C                   MOVE(P)   vlrMSG        wNomeFile
074400000613     C*
074500000830     C* CALCOLA LA DATA CORRENTE
074600120207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074700120207     C                   eval      datcor = %dec(%date() : *iso)
074800000830     C*
074900000613     C                   ENDSR
075000000613     C***
