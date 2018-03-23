000100090313      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090313     FEDIVABwr  O    E             DISK    usropn
000700090313     FEDIVATwr  O    E             DISK    usropn
000800990908
000900041210     D*------------
001000041210     D* SCHIERE
001100041210     D*------------
001200110321     D skNAZISO        S              2    DIM(1000)
001300050131     D skNAZBAR        S              3    DIM(1000)
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100050131     D tisi95ds      e ds
002200050131     D ds15          e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800041210     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200080311     D FlgNewBol       s              1    INZ(*blanks)
003300050131     D jNAZ            s              5  0 INZ(*zeros)
003400000830
003500000830     D*------------------
003600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700000830     D*------------------
003800000830     D WLBDA8          DS                  INZ
003900000830     D  G08DAT                 1      8  0
004000000830     D  G08INV                 9     16  0
004100000830     D  G08ERR                17     17
004200000830     D  G08TGI                18     22  0
004300041025     D*------------------
004400041025     D* DS REPERIMENTO NUMERATORE
004500041025     D*------------------
004600050131     D trul33ds      e ds                  inz
004700041025     D*------------------
004800041025     D* DS ARCHITETTURA
004900041025     D*------------------
005000041025     D kpjba         e ds                  inz
005100041025     D*------------------
005200101217
005300990908
005400101217     D*------------------
005500101217     D* DS RIDEFINIZIONE "DELIVERY CONTACTS"
005600101217     D*------------------
005700101217     D FIELD_37        DS                  INZ
005800101217     D  FLD37_TRCA                   35
005900110321     D  FLD37_TRCB                    5
006000101217     D*------------------
006100101217     D* DS RIDEFINIZIONE "DELIVERY INSTRUCTIONS"
006200101217     D*------------------
006300101217     D FIELD_38        DS                  INZ
006400101217     D  FLD38_TC1                     1
006500101217     D  FLD38_TC2                     1
006600101217     D  FLD38_FILLER1                 1
006700101222     D  FLD38_DCR                     8
006800101217     D  FLD38_TCR                     1
006900101217     D  FLD38_GC1                     2
007000101217     D  FLD38_GC2                     2
007100101217     D  FLD38_FILLER2                35
007200101222     D  FLD38_CTR                     3
007300101217     D  FLD38_FILLER3                 1
007400101217     D  FLD38_FILLER4                 1
007500101217     D  FLD38_FILLER5                 4
007600010201
007700010201
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100000613     C*
008200050131     C                   EXSR      CARTAB                                       CARICA TABELLE
008300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008400000613     C*
008500010202     C* Effettuo la chiamata al CLLE preposto
008600090313     C                   call(e)   'TITVEVTC'
008700010202     C                   parm                    parccm
008800010202     C                   parm                    parmbr
008900010202     C                   parm      '2'           paropz
009000050131     C*
009100050131     C* Effettuo lancio TISI95 solo x chiusura
009200050131     C                   CLEAR                   TISI95DS
009300050131     C                   EVAL      I95TLA = 'C'
009400050131     C                   CALL      'TISI95R'
009500050131     C                   PARM                    TISI95DS
009600000616     C*
009700010201     C                   seton                                        LR
009800041210
009900041210     C*--------------------------------------------------------
010000050131     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
010100041210     C*--------------------------------------------------------
010200041210     C     CARTAB        BEGSR
010300041210     C*
010400041210     C* TABELLA '15' - NAZIONI
010500041210     C                   clear                   skNAZISO
010600041210     C                   clear                   skNAZBAR
010700041210     C                   eval      tblKUT = 1
010800041210     C                   eval      tblCOD = '15'
010900041210     C     KEYtabP       setll     tabel00f
011000041210     C     KEYtabP       reade     tabel00f
011100041210     C                   dow       not %eof(tabel00f)
011200041210     C                   if        tblFLG = *blanks
011300041210     C                   movel(p)  tblUNI        ds15
011400041210     C                   add       1             jNAZ
011500041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
011600110321     C                   eval      skNAZISO(jNAZ) = §15ISO
011700041210     C                   endif
011800041210     C     KEYtabP       reade     tabel00f
011900041210     C                   enddo
012000041210     C*
012100041210     C                   ENDSR
012200041210     C***
012300041210
012400041210
012500041210
012600910830     C*--------------------------------------------------------
012700090313     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012800910830     C*--------------------------------------------------------
012900040526     C     RWFILE        BEGSR
013000990910     C*
013100990914     C                   if        not %open(tivin00r)
013200990908     C                   open      tivin00r
013300990914     C                   endif
013400090313     C                   if        not %open(edivabwr)
013500090313     C                   open      edivabwr
013600990914     C                   endif
013700090313     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
013800020305     C                   exsr      prevat
013900010201     C*
014000010202     C                   if        chkcall = '0'
014100010202     C*
014200090313     C                   if        not %open(edivatwr)
014300090313     C                   open      edivatwr
014400010201     C                   endif
014500990910     C*
014600010201     C                   clear                   §CTROKVB          5 0
014700020305     C                   clear                   §CTROKVT          5 0
014800000801     C                   clear                   §CTRMO            5 0
014900000801     C                   clear                   §CTRNO            5 0
015000040910     C*
015100921023     C                   DO        *HIVAL
015200990913     C*
015300990915     C                   READ      tivin00r                               70
015400040910     C                   if        vindta > *blanks
015500000613     C                   add       1             rrnum
015600000801     C*
015700000801     C                   if        *in70 = *off
015800000801     C                             and
015900000801     C                             (vinflg = *blanks
016000000801     C                              or vinflg = '0'
016100000801     C                              or vinflg = '2')
016200000801     C*
016300000801     C                   clear                   vinmsg
016400000801     C                   eval      vinflg = '1'
016500040910     C*
016600040910     C* Eseguo routine d traduzione
016700050131     C                   exsr      impvabvat
016800040802     C*
016900010305     C                   endif
017000000905     C*
017100000905     C                   else
017200000905     C                   eval      vinflg = '1'
017300000905     C                   endif
017400000905     C*
017500000905     C  N70              update    tivin000
017600000905     C*
017700991022     C  N70              ENDdo
017800041210     C*
017900050131     C* Scarico i buffer eventualmente rimasti in sospeso
018000041210     C                   IF        FlgNewBol = '1'
018100121227     C                   EVAL      VABCMR = 'LEVI ' + %trim(wCMR) +
018200121227     C                                      ' tariffa ' + %char(VABCTR)
018300090313     C                   EVAL      VABDCM = datcor
018400090313     C                   EVAL      VABCNT = 1
018500090313     C                   WRITE     EDIVAB00
018600041210     C                   ENDIF
018700010202     C*
018800010202     C                   endif
018900990910
019000990910     C* Se non ci sono record con errori ...
019100000710     C                   if        §ctrno = 0
019200990910     C* ... restituisco esito OK.
019300990921     C                   eval      wrkesito = '0'
019400990910     C                   else
019500010201     C                   if        §ctrokvb > 0
019600990921     C                   eval      wrkesito = '1'
019700000710     C                   else
019800000710     C                   eval      wrkesito = '2'
019900990910     C                   endif
020000000710     C                   endif
020100990910     C*
020200990914     C                   if        %open(tivin00r)
020300990908     C                   close     tivin00r
020400990914     C                   endif
020500090313     C                   if        %open(edivabwr)
020600090313     C                   close     edivabwr
020700990914     C                   endif
020800090313     C                   if        %open(edivatwr)
020900090313     C                   close     edivatwr
021000010201     C                   endif
021100990910     C*
021200010201     C                   if        §ctrokvb > 0
021300000724     C                             and vlrpoi <> *zeros
021400010202     C                   exsr      invio
021500990920     C                   endif
021600990920     C*
021700910830     C                   ENDSR
021800000613     C***
021900990920
022000000801     C*----------------------------------------------------*
022100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022200000801     C*----------------------------------------------------*
022300010201     C     INZVAR        BEGSR
022400000801     C*
022500040802     C                   Z-ADD     *zeros        Num5_0            5 0
022600040802     C                   MOVEL     '0'           FlgCAS            1
022700000801     C*
022800000801     C                   ENDSR
022900000801     C*----------------------------------------------------*
023000040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023100000801     C*----------------------------------------------------*
023200000801     C     DEFCAM        BEGSR
023300000801     C*
023400090313     C                   CLEAR                   EDIVAB00
023500090313     C                   CLEAR                   EDIVAT00
023600020619     C* Imposto i valori di default...
023700050217     C                   Z-ADD     0893062       VABCCM
023800050217     C                   Z-ADD     0893062       VATCCM
023900050217     C                   Z-ADD     089           VABLNP
024000050217     C                   Z-ADD     089           VATLNP
024100050311     C                   Z-ADD     103           VABCTR
024200040823     C                   MOVEL     '1'           VABCBO
024300050131     C                   MOVEL     '7Q'          VABCTM
024400020619     C* ... e poi verifico se sono stati passati come parametri
024500020619     C                   IF        vlrppt > *blanks
024600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
024700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024800020619     C                   EXSR      CHKNUM
024900020619     C                   IF        PiInt=*on
025000020619     C                   Z-ADD     PiVal         VABCCM
025100020619     C                   Z-ADD     PiVal         VATCCM
025200020619     C                   ENDIF
025300040506     C                   ENDIF
025400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
025500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025600020619     C                   EXSR      CHKNUM
025700020619     C                   IF        PiInt=*on
025800020619     C                   Z-ADD     PiVal         VABLNP
025900020619     C                   Z-ADD     PiVal         VATLNP
026000040506     C                   ENDIF
026100020619     C                   ENDIF
026200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
026300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026400020619     C                   EXSR      CHKNUM
026500020619     C                   IF        PiInt=*on
026600020619     C                   Z-ADD     PiVal         VABCTR
026700040506     C                   ENDIF
026800020619     C                   ENDIF
026900041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
027000041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
027100041210     C                   ENDIF
027200130201     C                   SETOFF                                       45
027300130201     C                   IF        %subst(vlrppt:16:1) = 'S'
027400130201     C                   SETON                                        45
027500130201     C                   ENDIF
027600020619     C                   ENDIF
027700000801     C*
027800000801     C                   ENDSR
027900000801     C*----------------------------------------------------*
028000090313     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
028100000801     C*----------------------------------------------------*
028200040910     C     IMPVABVAT     BEGSR
028300041210     C*
028400041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
028500041210     C                   Z-ADD     *zeros        errore            1 0
028600110318     C********
028700110318     C* Tipo record '00' (dati generali a livello d file
028800110318     C********
028900110318     C                   IF        %trim(%subst(vindta:1:2)) = '00'
029000110318     C                   MOVEL     *blanks       wCMR             20
029100110321     C                   EVAL      wCMR = %trim(%subst(vindta:30:32))
029200110318     C                   ENDIF
029300050317     C********
029400050317     C* Tipo record 'C1' (dati relativi alla transazione: sender ID, etc...)
029500050317     C********
029600050317     C                   IF        %trim(%subst(vindta:1:2)) = 'C1'
029700050318     C                   Z-ADD     *zeros        wVABCCM           7 0
029800130131     C*
029900130131     C* Il CCM viene pilotato dal file stesso solo se dati non diretti a Palmanova
030000130131     C                   IF        VABCCM <> 1161525
030100130131     C*
030200050317     C* Verifico se trattasi d traffico U.K.
030300050317     C                   IF        %trim(%subst(vindta:180:3)) = 'UK1'
030400090311     C***                EVAL      wVABCCM = 0893062
030500090610     C***                EVAL      wVABCCM = 0503436
030600090610     C                   EVAL      wVABCCM = 0504403
030700050317     C                   ENDIF
030800130131     C*
030900050317     C* Verifico se trattasi d traffico BORNHEM
031000050317     C                   IF        %trim(%subst(vindta:180:3)) = 'B01'
031100090610     C***                EVAL      wVABCCM = 0503436
031200090610     C                   EVAL      wVABCCM = 0504402
031300050317     C                   ENDIF
031400130131     C*
031500090311     C* Verifico se trattasi d traffico dalla REPUBBLICA CECA
031600090311     C                   IF        %trim(%subst(vindta:180:3)) = 'CZ1'
031700090311     C                   EVAL      wVABCCM = 1161525
031800090311     C                   ENDIF
031900130131     C                   ENDIF
032000050317     C                   ENDIF
032100040910     C*
032200041210     C********
032300050131     C* Tipo record 'C2' (dati testata: riferimenti, destinatario, contrassegno, etc...)
032400041210     C********
032500050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C2'
032600090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
032700041210     C                   IF        FlgNewBol = '1'
032800121227     C                   EVAL      VABCMR = 'LEVI ' + %trim(wCMR) +
032900121227     C                                      ' tariffa ' + %char(VABCTR)
033000090313     C                   EVAL      VABDCM = datcor
033100090313     C                   EVAL      VABCNT = 1
033200090313     C                   WRITE     EDIVAB00
033300050131     C                   EVAL      FlgNewBol = '0'
033400041210     C                   ENDIF
033500041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
033600040910     C                   EXSR      INZVAR
033700040910     C                   EXSR      DEFCAM
033800050318     C* ......verifico il codice cliente mittente reperito da file, se valorizzato utilizzo quello
033900050318     C                   IF        wVABCCM <> *zeros
034000050318     C                   EVAL      VABCCM = wVABCCM
034100050318     C                   EVAL      VATCCM = wVABCCM
034200050318     C                   ENDIF
034300041210     C                   EVAL      FlgNewBol = '1'
034400040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
034500040928     C                   MOVEL     datcor        VABAAS
034600040928     C                   MOVEL     datcor        VATAAS
034700040928     C                   MOVE      datcor        VABMGS
034800040928     C                   MOVE(P)   vlrpoi        VABFGS
034900040928     C                   MOVE(P)   vlrpoi        VATFGS
035000041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
035100041210     C                   CLEAR                   TRUL33DS
035200041210     C                   EVAL      I33OPE = *zeros
035300041210     C                   EVAL      I33CNU = 302
035400041210     C                   EVAL      I33NUM = 1
035500041210     C                   MOVEL     TRUL33DS      KPJBU
035600041210     C                   CALL      'TRUL33R'
035700041210     C                   PARM                    KPJBA
035800041210     C                   MOVEL     KPJBU         TRUL33DS
035900041210     C                   IF        O33ERR = *zeros
036000041210     C                   Z-ADD     O33NRF        VABNSP
036100041210     C                   Z-ADD     O33NRF        VATNSP
036200041210     C                   ELSE
036300041210     C                   ADD       1             errore
036400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
036500041025     C                             + ' ' + 'VABNSP VATNSP'
036600041210     C                   ENDIF
036700041210     C* ......VABRMN
036800050131     C                   EVAL      PiStr=%trim(%subst(vindta:23:8))
036900041210     C                   EXSR      CHKNUM
037000041210     C                   IF        PiInt=*on
037100041210     C                   Z-ADD     PiVal         VABRMN
037200041210     C                   ELSE
037300041210     C                   Z-ADD     1             VABRMN
037400041210     C                   ADD       1             errore
037500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
037600041210     C                             + ' ' + 'VABRMN'
037700041210     C                   ENDIF
037800050131     C* ......VABRSD
037900110321     C                   EVAL      VABRSD=%trim(%subst(vindta:41:40))
038000100429     C                   IF        %subst(VABRSD:1:8) = 'COIN SPA'
038100100429     C                   EVAL      VABRSD = 'COIN SPA'
038200100429     C                   ENDIF
038300050131     C* ......VABRD2
038400110321     C                   EVAL      VABRD2=%trim(%subst(vindta:81:40))
038500050131     C* ......VABIND
038600110321     C                   EVAL      VABIND=%trim(%subst(vindta:121:40))
038700050131     C* ......VABLOD
038800110321     C                   EVAL      VABLOD=%trim(%subst(vindta:201:40))
038900050131     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
039000110321     C                   MOVEL     *blanks       wNAZ              2
039100110321     C                   EVAL      wNAZ=%subst(vindta:251:2)
039200050131     C                   Z-ADD     1             jNAZ
039300110321     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         55
039400050131     C                   IF        %found
039500050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
039600050131     C                   ENDIF
039700050131     C* ......VABCAD
039800110321     C                   EVAL      PiStr=%trim(%subst(vindta:241:10))
039900050131     C                   EXSR      CHKNUM
040000050131     C                   IF        PiInt=*on
040100050131     C                   Z-ADD     PiVal         Num5_0
040200050131     C                   MOVEL(P)  Num5_0        VABCAD
040300050131     C                   ELSE
040400050131     C                   ADD       1             errore
040500050131     C                   EVAL      vinmsg = %trimr(vinmsg)
040600050131     C                             + ' ' + 'VABCAD'
040700050131     C                   ENDIF
040800050131     C* ......VABPRD (reperita mediante TISI95)
040900050131     C                   IF        VABCAD <> *blanks
041000050131     C                   CLEAR                   TISI95DS
041100050131     C                   EVAL      I95TCN = '3'
041200050131     C                   Z-ADD     datcor        I95DAT
041300050131     C                   EVAL      I95CAP = VABCAD
041400050131     C                   EVAL      I95LOC = VABLOD
041500050131     C                   CALL      'TISI95R'
041600050131     C                   PARM                    TISI95DS
041700050131     C                   EVAL      VABPRD = O95PRV
041800050131     C                   ENDIF
041900101217     C*
042000101217     C* Gestione flag operativi - campo 38
042100110321     C                   CLEAR                   FIELD_38
042200110802     C                   EVAL      FIELD_38 = %subst(vindta:338:60)
042300101217     C* ......VABTC1
042400101217     C                   EVAL      VABTC1 = FLD38_TC1
042500101217     C* ......VABTC2
042600101217     C                   EVAL      VABTC2 = FLD38_TC2
042700101222     C* ......VABDCR
042800101222     C                   IF        FLD38_DCR <> *blanks
042900101222     C                   MOVE(P)   FLD38_DCR     VABDCR
043000101222     C                   ENDIF
043100101217     C* ......VABTCR
043200101217     C                   EVAL      VABTCR = FLD38_TCR
043300101217     C* ......VABGC1
043400101217     C                   EVAL      VABGC1 = FLD38_GC1
043500101217     C* ......VABGC2
043600101217     C                   EVAL      VABGC2 = FLD38_GC2
043700101217     C* ......VABCTR
043800101222     C                   IF        FLD38_CTR <> *blanks
043900101222     C                   MOVE(P)   FLD38_CTR     VABCTR
044000101222     C                   ENDIF
044100121227     C                   SETOFF                                       50
044200121227     C                   SELECT
044300121227     C                   WHEN      VABCTR = 100
044400121227     C                   EVAL      VABCTM = '7Q'
044500121227     C                   WHEN      VABCTR = 105
044600121227     C                   EVAL      VABCTM = '7Q'
044700130201     C                   WHEN      VABCTR = 103
044800130201     C  N45              EVAL      VABCTM = *blanks
044900130201     C  N45              SETON                                        50
045000121227     C                   ENDSL
045100101217     C*
045200101217     C* Gestione flag operativi - campo 37
045300110321     C                   CLEAR                   FIELD_37
045400110321     C                   EVAL      FIELD_37 = %subst(vindta:268:40)
045500101217     C* ......VATNOT (referente x consegna)
045600101217     C                   EVAL      VATNOT=FLD37_TRCA
045700050131     C                   EVAL      VATTRC = 'A'
045800050131     C                   IF        VATNOT <> *blanks
045900121227     C                   EVAL      VATCMR = 'LEVI ' + %trim(wCMR) +
046000121227     C                                      ' tariffa ' + %char(VABCTR)
046100090318     C                   EVAL      VATCNT = 1
046200121227     C                   WRITE     EDIVAT00
046300050131     C                   ENDIF
046400101217     C* ......VATNOT (telefono referente)
046500101217     C                   EVAL      VATNOT=FLD37_TRCB
046600101217     C                   EVAL      VATTRC = 'B'
046700101217     C                   IF        VATNOT <> *blanks
046800121227     C                   EVAL      VATCMR = 'LEVI ' + %trim(wCMR) +
046900121227     C                                      ' tariffa ' + %char(VABCTR)
047000101217     C                   EVAL      VATCNT = 1
047100101217     C                   WRITE     EDIVAT00
047200101217     C                   ENDIF
047300050131     C* ......VABCAS
047400110321     C                   IF        %subst(vindta:398:1) = 'Y'
047500050131     C                   EVAL      FlgCAS = '1'
047600110321     C                   EVAL      VABVCA=%trim(%subst(vindta:409:3))
047700110321     C                   EVAL      PiStr=%trim(%subst(vindta:399:10))
047800050131     C                   EXSR      CHKNUM
047900050131     C                   IF        PiNum=*on
048000050131     C                   Z-ADD     PiVal         VABCAS
048100050131     C                   ELSE
048200050131     C                   ADD       1             errore
048300050131     C                   EVAL      vinmsg = %trimr(vinmsg)
048400050131     C                             + ' ' + 'VABCAS'
048500050131     C                   ENDIF
048600050131     C                   ENDIF
048700050131     C*
048800050131     C                   ENDIF
048900050131     C*
049000050131     C********
049100050131     C* Tipo record 'C3' (dati dettaglio: barcode, peso, volume, colli, etc...)
049200050131     C********
049300050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C3'
049400090318     C* ......VABNCL
049500090318     C                   ADD       1             VABNCL
049600050131     C* ......VATNOT ("chi sono")
049700110921     C                   IF        %subst(vindta:51:20) <> *blanks
049800050131     C                   EVAL      VATNOT=%trim(%subst(vindta:51:20))
049900110921     C                   ELSE
050000110921     C                   EVAL      VATNOT=%trim(%subst(vindta:31:20))
050100110921     C                   ENDIF
050200050131     C                   EVAL      VATTRC = 'E'
050300050131     C                   IF        VATNOT <> *blanks
050400121227     C                   EVAL      VATCMR = 'LEVI ' + %trim(wCMR) +
050500121227     C                                      ' tariffa ' + %char(VABCTR)
050600090318     C                   EVAL      VATCNT = 1
050700121227     C  N50              WRITE     EDIVAT00
050800050131     C                   ENDIF
050900050131     C* ......VABPKB
051000050131     C                   EVAL      PiStr=%trim(%subst(vindta:71:7))
051100041210     C                   EXSR      CHKNUM
051200041210     C                   IF        PiNum=*on
051300050131     C                   ADD(H)    PiVal         VABPKB
051400041210     C                   ELSE
051500041210     C                   ADD       1             errore
051600041210     C                   EVAL      vinmsg = %trimr(vinmsg)
051700041210     C                             + ' ' + 'VABPKB'
051800041210     C                   ENDIF
051900110321     C* ......VABVLB
052000050401     C                   EVAL      PiStr=%trim(%subst(vindta:80:7))
052100050401     C                   EXSR      CHKNUM
052200050401     C                   IF        PiNum=*on
052300050401     C                   ADD(H)    PiVal         VABVLB
052400050401     C                   ELSE
052500050401     C                   ADD       1             errore
052600050401     C                   EVAL      vinmsg = %trimr(vinmsg)
052700050401     C                             + ' ' + 'VABVLB'
052800050401     C                   ENDIF
052900041210     C*
053000041210     C                   ENDIF
053100101217     C*
053200101217     C********
053300101217     C* Tipo record 'C4' (dati di ordine, etc...)
053400101217     C********
053500101217     C                   IF        %trim(%subst(vindta:1:2)) = 'C4'
053600101217     C* ......VABNOT
053700110321     C                   EVAL      VABNOT=%trim(%subst(vindta:61:35))
053800101217     C*
053900101217     C                   ENDIF
054000040910     C*
054100040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
054200040910     C                   IF        FlgCAS <> '0'
054300040929     C                   IF        VABCBO = '1'
054400040910     C                   EVAL      VABCBO = '4'
054500040910     C                   ELSE
054600040929     C                   EVAL      VABCBO = '6'
054700040910     C                   ENDIF
054800040929     C                   ENDIF
054900040910     C*
055000040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055100040910     C                   EXSR      CHKIMPDIV
055200010202     C*
055300000801     C* Ebbene...
055400000801     C                   ADD       1             §CTRMO
055500010201     C                   IF        errore <> *zeros
055600000801     C                   ADD       1             §CTRNO
055700000801     C                   EVAL      vinflg = '2'
055800000801     C                   ELSE
055900010201     C                   ADD       1             §CTROKVB
056000000801     C                   ENDIF
056100000801     C*
056200000801     C                   ENDSR
056300010202     C*----------------------------------------------------*
056400090313     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
056500010202     C*----------------------------------------------------*
056600020305     C     PREVAT        BEGSR
056700010202     C*
056800090313     C* Compongo il nome del membro da dare al EDIVATWR
056900010202     C                   eval      parmbr = vlrhdl
057000010202     C                   movel     'M'           parmbr
057100041210     C                   eval      parccm = vlrksc
057200010202     C                   eval      paropz = '1'
057300010202     C* Effettuo la chiamata al CLLE preposto
057400090313     C                   call(e)   'TITVEVTC'
057500010202     C                   parm                    parccm
057600010202     C                   parm                    parmbr
057700010202     C                   parm                    paropz
057800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057900010202     C                   if        %error
058000010202     C                   movel     '1'           chkcall
058100010202     C                   else
058200010202     C                   movel     '0'           chkcall
058300010202     C                   endif
058400010202     C*
058500010202     C                   ENDSR
058600000801     C*----------------------------------------------------*
058700000801     C*  CONTROLLO NUMERICITA' CAMPI
058800000801     C*----------------------------------------------------*
058900000801     C     CHKNUM        BEGSR
059000000801     C*
059100000801     C                   call(e)   'ISNUMERIC'
059200000801     C                   PARM                    PiStr            30
059300041210     C                   PARM      '.'           PiDecChr          1
059400000801     C                   PARM      *ZEROS        PiVal            30 9
059500000801     C                   PARM      '0'           PiInt             1
059600000801     C                   PARM      '0'           PiNum             1
059700000801     C                   IF        %error
059800000801     C                   EVAL      PiInt=*off
059900000801     C                   ENDIF
060000000801     C*
060100000801     C                   ENDSR
060200000801     C***
060300000801
060400011113
060500011113     C*----------------------------------------------------*
060600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060700011113     C*----------------------------------------------------*
060800011113     C     CHKIMPDIV     BEGSR
060900011113     C*
061000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061100011113     C                   Z-ADD     *zeros        wrkDec            9 9
061200011113     C*
061300011113     C* Come prima cosa effettuo considerazioni sulla divisa
061400011113     C                   IF        vabIAS > *zeros
061500011113     C                   IF        vabVAS <> 'EUR'
061600011113     C                   EVAL      vabVAS =  'ITL'
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C                   IF        vabCAS > *zeros
062100011113     C                   IF        vabVCA <> 'EUR'
062200011113     C                   EVAL      vabVCA =  'ITL'
062300011113     C                   ENDIF
062400011113     C                   ENDIF
062500011113     C*
062600011113     C                   IF        vabVMD > *zeros
062700020305     C                   IF        vabVAD <> 'EUR'
062800011113     C                   EVAL      vabVAD =  'ITL'
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063300011113     C                   Z-ADD     vabIAS        wrkDec
063400011113     C                   IF        wrkDec > *zeros
063500011113     C                   IF        vabVAS = 'ITL'
063600011113     C                   EVAL      vabIAS = *zeros
063700011113     C                   ENDIF
063800011113     C                   ENDIF
063900011113     C*
064000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064100011113     C                   Z-ADD     vabCAS        wrkDec
064200011113     C                   IF        wrkDec > *zeros
064300011113     C                   IF        vabVCA = 'ITL'
064400011113     C                   EVAL      vabCAS = *zeros
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064900011113     C                   Z-ADD     vabVMD        wrkDec
065000011113     C                   IF        wrkDec > *zeros
065100011113     C                   IF        vabVAD = 'ITL'
065200011113     C                   EVAL      vabVMD = *zeros
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C                   ENDSR
065700011113     C***
065800011113
065900011113
066000000801
066100000801
066200990920      /TITLE Invio dei dati al punto operativo.
066300010202     C     invio         BEGSR
066400990920     C*
066500090313     C* 1° invio EDIVAT
066600010201     C                   reset                   dscmz
066700010201     C                   move      vlrpoi        cmzdst
066800090313     C                   eval      cmzfld = 'EDIVATWR'
066900010201     C                   eval      cmzmbd = vlrhdl
067000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067100021009     C***                if        prmfir = *blanks
067200090313     C                   eval      cmzfla = 'EDIVAT0F'
067300090313     C                   eval      cmzmba = 'EDIVAT0F'
067400021009     C***                else
067500021009     C***                eval      cmzfla = prmfir
067600021009     C***                eval      cmzmba = prmfir
067700021009     C***                endif
067800010201     C                   eval      cmznrr = *zeros
067900020305     C                   move      §ctrokvt      cmznrr
068000021018     C                   eval      cmzlba = vlrfl1
068100010201     C                   call(e)   'TIS711C'
068200010201     C                   parm                    dscmz
068300010201     C                   parm      *blanks       esito
068400010205     C                   if        %error
068500010205     C                             or cmzerr = '1'
068600010205     C                             or esito  = '1'
068700010205     C                   eval      wrkesito = '3'
068800010205     C                   else
068900010201     C*
069000090313     C* 2° invio EDIVAB
069100010201     C                   reset                   dscmz
069200010201     C                   move      vlrpoi        cmzdst
069300010201     C                   eval      cmzfld = vlrfou
069400010201     C                   eval      cmzmbd = vlrhdl
069500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600021009     C***                if        prmfir = *blanks
069700090313     C                   eval      cmzfla = 'EDIVAB0F'
069800090313     C                   eval      cmzmba = 'EDIVAB0F'
069900021009     C***                else
070000021009     C***                eval      cmzfla = prmfir
070100021009     C***                eval      cmzmba = prmfir
070200021009     C***                endif
070300010201     C                   eval      cmznrr = *zeros
070400010201     C                   move      §ctrokvb      cmznrr
070500021018     C                   eval      cmzlba = vlrfl1
070600010201     C                   call(e)   'TIS711C'
070700010201     C                   parm                    dscmz
070800010201     C                   parm      *blanks       esito
070900010201     C                   if        %error
071000010201     C                             or cmzerr = '1'
071100010201     C                             or esito  = '1'
071200010201     C                   eval      wrkesito = '3'
071300010201     C                   endif
071400010205     C                   endif
071500990920     C*
071600000613     C                   ENDSR
071700000613     C***
071800990910
071900000613     C     *inzsr        BEGSR
072000990910     C*
072100990910     C     *entry        plist
072200990920     C                   parm                    tivlrds
072300990921     C                   parm      wrkesito      esito
072400000724     C                   parm                    prmlit
072500000710     C                   parm                    prmfir
072600000613     C*
072700000830     C* CALCOLA LA DATA CORRENTE
072800000830     C                   time                    wn14             14 0
072900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073000000830     C                   z-add     wn8           g08dat
073100000830     C                   z-add     *zeros        g08inv
073200000830     C                   movel     '0'           g08err
073300000830     C                   call      'XSRDA8'
073400000830     C                   parm                    wlbda8
073500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073600041210     C*
073700041210     C* Chiave su TABEL00F - parziale
073800041210     C     KEYtabP       KLIST
073900041210     C                   KFLD                    tblKUT
074000041210     C                   KFLD                    tblCOD
074100000830     C*
074200000613     C                   ENDSR
074300000613     C***
