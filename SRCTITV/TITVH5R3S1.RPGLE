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
018100110318     C                   EVAL      VABCMR = 'LEVI ' + wCMR
018200090313     C                   EVAL      VABDCM = datcor
018300090313     C                   EVAL      VABCNT = 1
018400090313     C                   WRITE     EDIVAB00
018500041210     C                   ENDIF
018600010202     C*
018700010202     C                   endif
018800990910
018900990910     C* Se non ci sono record con errori ...
019000000710     C                   if        §ctrno = 0
019100990910     C* ... restituisco esito OK.
019200990921     C                   eval      wrkesito = '0'
019300990910     C                   else
019400010201     C                   if        §ctrokvb > 0
019500990921     C                   eval      wrkesito = '1'
019600000710     C                   else
019700000710     C                   eval      wrkesito = '2'
019800990910     C                   endif
019900000710     C                   endif
020000990910     C*
020100990914     C                   if        %open(tivin00r)
020200990908     C                   close     tivin00r
020300990914     C                   endif
020400090313     C                   if        %open(edivabwr)
020500090313     C                   close     edivabwr
020600990914     C                   endif
020700090313     C                   if        %open(edivatwr)
020800090313     C                   close     edivatwr
020900010201     C                   endif
021000990910     C*
021100010201     C                   if        §ctrokvb > 0
021200000724     C                             and vlrpoi <> *zeros
021300010202     C                   exsr      invio
021400990920     C                   endif
021500990920     C*
021600910830     C                   ENDSR
021700000613     C***
021800990920
021900000801     C*----------------------------------------------------*
022000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022100000801     C*----------------------------------------------------*
022200010201     C     INZVAR        BEGSR
022300000801     C*
022400040802     C                   Z-ADD     *zeros        Num5_0            5 0
022500040802     C                   MOVEL     '0'           FlgCAS            1
022600000801     C*
022700000801     C                   ENDSR
022800000801     C*----------------------------------------------------*
022900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023000000801     C*----------------------------------------------------*
023100000801     C     DEFCAM        BEGSR
023200000801     C*
023300090313     C                   CLEAR                   EDIVAB00
023400090313     C                   CLEAR                   EDIVAT00
023500020619     C* Imposto i valori di default...
023600050217     C                   Z-ADD     0893062       VABCCM
023700050217     C                   Z-ADD     0893062       VATCCM
023800050217     C                   Z-ADD     089           VABLNP
023900050217     C                   Z-ADD     089           VATLNP
024000050311     C                   Z-ADD     103           VABCTR
024100040823     C                   MOVEL     '1'           VABCBO
024200050131     C                   MOVEL     '7Q'          VABCTM
024300020619     C* ... e poi verifico se sono stati passati come parametri
024400020619     C                   IF        vlrppt > *blanks
024500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
024600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024700020619     C                   EXSR      CHKNUM
024800020619     C                   IF        PiInt=*on
024900020619     C                   Z-ADD     PiVal         VABCCM
025000020619     C                   Z-ADD     PiVal         VATCCM
025100020619     C                   ENDIF
025200040506     C                   ENDIF
025300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
025400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025500020619     C                   EXSR      CHKNUM
025600020619     C                   IF        PiInt=*on
025700020619     C                   Z-ADD     PiVal         VABLNP
025800020619     C                   Z-ADD     PiVal         VATLNP
025900040506     C                   ENDIF
026000020619     C                   ENDIF
026100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
026200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026300020619     C                   EXSR      CHKNUM
026400020619     C                   IF        PiInt=*on
026500020619     C                   Z-ADD     PiVal         VABCTR
026600040506     C                   ENDIF
026700020619     C                   ENDIF
026800041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
026900041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
027000041210     C                   ENDIF
027100020619     C                   ENDIF
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500090313     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
027600000801     C*----------------------------------------------------*
027700040910     C     IMPVABVAT     BEGSR
027800041210     C*
027900041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
028000041210     C                   Z-ADD     *zeros        errore            1 0
028100110318     C********
028200110318     C* Tipo record '00' (dati generali a livello d file
028300110318     C********
028400110318     C                   IF        %trim(%subst(vindta:1:2)) = '00'
028500110318     C                   MOVEL     *blanks       wCMR             20
028600110321     C                   EVAL      wCMR = %trim(%subst(vindta:30:32))
028700110318     C                   ENDIF
028800050317     C********
028900050317     C* Tipo record 'C1' (dati relativi alla transazione: sender ID, etc...)
029000050317     C********
029100050317     C                   IF        %trim(%subst(vindta:1:2)) = 'C1'
029200050318     C                   Z-ADD     *zeros        wVABCCM           7 0
029300050317     C* Verifico se trattasi d traffico U.K.
029400050317     C                   IF        %trim(%subst(vindta:180:3)) = 'UK1'
029500090311     C***                EVAL      wVABCCM = 0893062
029600090610     C***                EVAL      wVABCCM = 0503436
029700090610     C                   EVAL      wVABCCM = 0504403
029800050317     C                   ENDIF
029900050317     C* Verifico se trattasi d traffico BORNHEM
030000050317     C                   IF        %trim(%subst(vindta:180:3)) = 'B01'
030100090610     C***                EVAL      wVABCCM = 0503436
030200090610     C                   EVAL      wVABCCM = 0504402
030300050317     C                   ENDIF
030400090311     C* Verifico se trattasi d traffico dalla REPUBBLICA CECA
030500090311     C                   IF        %trim(%subst(vindta:180:3)) = 'CZ1'
030600090311     C                   EVAL      wVABCCM = 1161525
030700090311     C                   ENDIF
030800050317     C                   ENDIF
030900040910     C*
031000041210     C********
031100050131     C* Tipo record 'C2' (dati testata: riferimenti, destinatario, contrassegno, etc...)
031200041210     C********
031300050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C2'
031400090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
031500041210     C                   IF        FlgNewBol = '1'
031600110318     C                   EVAL      VABCMR = 'LEVI ' + wCMR
031700090313     C                   EVAL      VABDCM = datcor
031800090313     C                   EVAL      VABCNT = 1
031900090313     C                   WRITE     EDIVAB00
032000050131     C                   EVAL      FlgNewBol = '0'
032100041210     C                   ENDIF
032200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
032300040910     C                   EXSR      INZVAR
032400040910     C                   EXSR      DEFCAM
032500050318     C* ......verifico il codice cliente mittente reperito da file, se valorizzato utilizzo quello
032600050318     C                   IF        wVABCCM <> *zeros
032700050318     C                   EVAL      VABCCM = wVABCCM
032800050318     C                   EVAL      VATCCM = wVABCCM
032900050318     C                   ENDIF
033000041210     C                   EVAL      FlgNewBol = '1'
033100040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
033200040928     C                   MOVEL     datcor        VABAAS
033300040928     C                   MOVEL     datcor        VATAAS
033400040928     C                   MOVE      datcor        VABMGS
033500040928     C                   MOVE(P)   vlrpoi        VABFGS
033600040928     C                   MOVE(P)   vlrpoi        VATFGS
033700041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
033800041210     C                   CLEAR                   TRUL33DS
033900041210     C                   EVAL      I33OPE = *zeros
034000041210     C                   EVAL      I33CNU = 302
034100041210     C                   EVAL      I33NUM = 1
034200041210     C                   MOVEL     TRUL33DS      KPJBU
034300041210     C                   CALL      'TRUL33R'
034400041210     C                   PARM                    KPJBA
034500041210     C                   MOVEL     KPJBU         TRUL33DS
034600041210     C                   IF        O33ERR = *zeros
034700041210     C                   Z-ADD     O33NRF        VABNSP
034800041210     C                   Z-ADD     O33NRF        VATNSP
034900041210     C                   ELSE
035000041210     C                   ADD       1             errore
035100041210     C                   EVAL      vinmsg = %trimr(vinmsg)
035200041025     C                             + ' ' + 'VABNSP VATNSP'
035300041210     C                   ENDIF
035400041210     C* ......VABRMN
035500050131     C                   EVAL      PiStr=%trim(%subst(vindta:23:8))
035600041210     C                   EXSR      CHKNUM
035700041210     C                   IF        PiInt=*on
035800041210     C                   Z-ADD     PiVal         VABRMN
035900041210     C                   ELSE
036000041210     C                   Z-ADD     1             VABRMN
036100041210     C                   ADD       1             errore
036200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
036300041210     C                             + ' ' + 'VABRMN'
036400041210     C                   ENDIF
036500050131     C* ......VABRSD
036600110321     C                   EVAL      VABRSD=%trim(%subst(vindta:41:40))
036700100429     C                   IF        %subst(VABRSD:1:8) = 'COIN SPA'
036800100429     C                   EVAL      VABRSD = 'COIN SPA'
036900100429     C                   ENDIF
037000050131     C* ......VABRD2
037100110321     C                   EVAL      VABRD2=%trim(%subst(vindta:81:40))
037200050131     C* ......VABIND
037300110321     C                   EVAL      VABIND=%trim(%subst(vindta:121:40))
037400050131     C* ......VABLOD
037500110321     C                   EVAL      VABLOD=%trim(%subst(vindta:201:40))
037600050131     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
037700110321     C                   MOVEL     *blanks       wNAZ              2
037800110321     C                   EVAL      wNAZ=%subst(vindta:251:2)
037900050131     C                   Z-ADD     1             jNAZ
038000110321     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         55
038100050131     C                   IF        %found
038200050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
038300050131     C                   ENDIF
038400050131     C* ......VABCAD
038500110321     C                   EVAL      PiStr=%trim(%subst(vindta:241:10))
038600050131     C                   EXSR      CHKNUM
038700050131     C                   IF        PiInt=*on
038800050131     C                   Z-ADD     PiVal         Num5_0
038900050131     C                   MOVEL(P)  Num5_0        VABCAD
039000050131     C                   ELSE
039100050131     C                   ADD       1             errore
039200050131     C                   EVAL      vinmsg = %trimr(vinmsg)
039300050131     C                             + ' ' + 'VABCAD'
039400050131     C                   ENDIF
039500050131     C* ......VABPRD (reperita mediante TISI95)
039600050131     C                   IF        VABCAD <> *blanks
039700050131     C                   CLEAR                   TISI95DS
039800050131     C                   EVAL      I95TCN = '3'
039900050131     C                   Z-ADD     datcor        I95DAT
040000050131     C                   EVAL      I95CAP = VABCAD
040100050131     C                   EVAL      I95LOC = VABLOD
040200050131     C                   CALL      'TISI95R'
040300050131     C                   PARM                    TISI95DS
040400050131     C                   EVAL      VABPRD = O95PRV
040500050131     C                   ENDIF
040600101217     C*
040700101217     C* Gestione flag operativi - campo 38
040800110321     C                   CLEAR                   FIELD_38
040900110802     C                   EVAL      FIELD_38 = %subst(vindta:338:60)
041000101217     C* ......VABTC1
041100101217     C                   EVAL      VABTC1 = FLD38_TC1
041200101217     C* ......VABTC2
041300101217     C                   EVAL      VABTC2 = FLD38_TC2
041400101222     C* ......VABDCR
041500101222     C                   IF        FLD38_DCR <> *blanks
041600101222     C                   MOVE(P)   FLD38_DCR     VABDCR
041700101222     C                   ENDIF
041800101217     C* ......VABTCR
041900101217     C                   EVAL      VABTCR = FLD38_TCR
042000101217     C* ......VABGC1
042100101217     C                   EVAL      VABGC1 = FLD38_GC1
042200101217     C* ......VABGC2
042300101217     C                   EVAL      VABGC2 = FLD38_GC2
042400101217     C* ......VABCTR
042500101222     C                   IF        FLD38_CTR <> *blanks
042600101222     C                   MOVE(P)   FLD38_CTR     VABCTR
042700101222     C                   ENDIF
042800101217     C*
042900101217     C* Gestione flag operativi - campo 37
043000110321     C                   CLEAR                   FIELD_37
043100110321     C                   EVAL      FIELD_37 = %subst(vindta:268:40)
043200101217     C* ......VATNOT (referente x consegna)
043300101217     C                   EVAL      VATNOT=FLD37_TRCA
043400050131     C                   EVAL      VATTRC = 'A'
043500050131     C                   IF        VATNOT <> *blanks
043600110318     C                   EVAL      VATCMR = 'LEVI ' + wCMR
043700090318     C                   EVAL      VATCNT = 1
043800090313     C                   WRITE     EDIVAT00
043900050131     C                   ENDIF
044000101217     C* ......VATNOT (telefono referente)
044100101217     C                   EVAL      VATNOT=FLD37_TRCB
044200101217     C                   EVAL      VATTRC = 'B'
044300101217     C                   IF        VATNOT <> *blanks
044400110318     C                   EVAL      VATCMR = 'LEVI ' + wCMR
044500101217     C                   EVAL      VATCNT = 1
044600101217     C                   WRITE     EDIVAT00
044700101217     C                   ENDIF
044800050131     C* ......VABCAS
044900110321     C                   IF        %subst(vindta:398:1) = 'Y'
045000050131     C                   EVAL      FlgCAS = '1'
045100110321     C                   EVAL      VABVCA=%trim(%subst(vindta:409:3))
045200110321     C                   EVAL      PiStr=%trim(%subst(vindta:399:10))
045300050131     C                   EXSR      CHKNUM
045400050131     C                   IF        PiNum=*on
045500050131     C                   Z-ADD     PiVal         VABCAS
045600050131     C                   ELSE
045700050131     C                   ADD       1             errore
045800050131     C                   EVAL      vinmsg = %trimr(vinmsg)
045900050131     C                             + ' ' + 'VABCAS'
046000050131     C                   ENDIF
046100050131     C                   ENDIF
046200050131     C*
046300050131     C                   ENDIF
046400050131     C*
046500050131     C********
046600050131     C* Tipo record 'C3' (dati dettaglio: barcode, peso, volume, colli, etc...)
046700050131     C********
046800050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C3'
046900090318     C* ......VABNCL
047000090318     C                   ADD       1             VABNCL
047100050131     C* ......VATNOT ("chi sono")
047200110921     C                   IF        %subst(vindta:51:20) <> *blanks
047300050131     C                   EVAL      VATNOT=%trim(%subst(vindta:51:20))
047400110921     C                   ELSE
047500110921     C                   EVAL      VATNOT=%trim(%subst(vindta:31:20))
047600110921     C                   ENDIF
047700050131     C                   EVAL      VATTRC = 'E'
047800050131     C                   IF        VATNOT <> *blanks
047900110318     C                   EVAL      VATCMR = 'LEVI ' + wCMR
048000090318     C                   EVAL      VATCNT = 1
048100090313     C                   WRITE     EDIVAT00
048200050131     C                   ENDIF
048300050131     C* ......VABPKB
048400050131     C                   EVAL      PiStr=%trim(%subst(vindta:71:7))
048500041210     C                   EXSR      CHKNUM
048600041210     C                   IF        PiNum=*on
048700050131     C                   ADD(H)    PiVal         VABPKB
048800041210     C                   ELSE
048900041210     C                   ADD       1             errore
049000041210     C                   EVAL      vinmsg = %trimr(vinmsg)
049100041210     C                             + ' ' + 'VABPKB'
049200041210     C                   ENDIF
049300110321     C* ......VABVLB
049400050401     C                   EVAL      PiStr=%trim(%subst(vindta:80:7))
049500050401     C                   EXSR      CHKNUM
049600050401     C                   IF        PiNum=*on
049700050401     C                   ADD(H)    PiVal         VABVLB
049800050401     C                   ELSE
049900050401     C                   ADD       1             errore
050000050401     C                   EVAL      vinmsg = %trimr(vinmsg)
050100050401     C                             + ' ' + 'VABVLB'
050200050401     C                   ENDIF
050300041210     C*
050400041210     C                   ENDIF
050500101217     C*
050600101217     C********
050700101217     C* Tipo record 'C4' (dati di ordine, etc...)
050800101217     C********
050900101217     C                   IF        %trim(%subst(vindta:1:2)) = 'C4'
051000101217     C* ......VABNOT
051100110321     C                   EVAL      VABNOT=%trim(%subst(vindta:61:35))
051200101217     C*
051300101217     C                   ENDIF
051400040910     C*
051500040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051600040910     C                   IF        FlgCAS <> '0'
051700040929     C                   IF        VABCBO = '1'
051800040910     C                   EVAL      VABCBO = '4'
051900040910     C                   ELSE
052000040929     C                   EVAL      VABCBO = '6'
052100040910     C                   ENDIF
052200040929     C                   ENDIF
052300040910     C*
052400040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052500040910     C                   EXSR      CHKIMPDIV
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
053700010202     C*----------------------------------------------------*
053800090313     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053900010202     C*----------------------------------------------------*
054000020305     C     PREVAT        BEGSR
054100010202     C*
054200090313     C* Compongo il nome del membro da dare al EDIVATWR
054300010202     C                   eval      parmbr = vlrhdl
054400010202     C                   movel     'M'           parmbr
054500041210     C                   eval      parccm = vlrksc
054600010202     C                   eval      paropz = '1'
054700010202     C* Effettuo la chiamata al CLLE preposto
054800090313     C                   call(e)   'TITVEVTC'
054900010202     C                   parm                    parccm
055000010202     C                   parm                    parmbr
055100010202     C                   parm                    paropz
055200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055300010202     C                   if        %error
055400010202     C                   movel     '1'           chkcall
055500010202     C                   else
055600010202     C                   movel     '0'           chkcall
055700010202     C                   endif
055800010202     C*
055900010202     C                   ENDSR
056000000801     C*----------------------------------------------------*
056100000801     C*  CONTROLLO NUMERICITA' CAMPI
056200000801     C*----------------------------------------------------*
056300000801     C     CHKNUM        BEGSR
056400000801     C*
056500000801     C                   call(e)   'ISNUMERIC'
056600000801     C                   PARM                    PiStr            30
056700041210     C                   PARM      '.'           PiDecChr          1
056800000801     C                   PARM      *ZEROS        PiVal            30 9
056900000801     C                   PARM      '0'           PiInt             1
057000000801     C                   PARM      '0'           PiNum             1
057100000801     C                   IF        %error
057200000801     C                   EVAL      PiInt=*off
057300000801     C                   ENDIF
057400000801     C*
057500000801     C                   ENDSR
057600000801     C***
057700000801
057800011113
057900011113     C*----------------------------------------------------*
058000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058100011113     C*----------------------------------------------------*
058200011113     C     CHKIMPDIV     BEGSR
058300011113     C*
058400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058500011113     C                   Z-ADD     *zeros        wrkDec            9 9
058600011113     C*
058700011113     C* Come prima cosa effettuo considerazioni sulla divisa
058800011113     C                   IF        vabIAS > *zeros
058900011113     C                   IF        vabVAS <> 'EUR'
059000011113     C                   EVAL      vabVAS =  'ITL'
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C                   IF        vabCAS > *zeros
059500011113     C                   IF        vabVCA <> 'EUR'
059600011113     C                   EVAL      vabVCA =  'ITL'
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C                   IF        vabVMD > *zeros
060100020305     C                   IF        vabVAD <> 'EUR'
060200011113     C                   EVAL      vabVAD =  'ITL'
060300011113     C                   ENDIF
060400011113     C                   ENDIF
060500011113     C*
060600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060700011113     C                   Z-ADD     vabIAS        wrkDec
060800011113     C                   IF        wrkDec > *zeros
060900011113     C                   IF        vabVAS = 'ITL'
061000011113     C                   EVAL      vabIAS = *zeros
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061500011113     C                   Z-ADD     vabCAS        wrkDec
061600011113     C                   IF        wrkDec > *zeros
061700011113     C                   IF        vabVCA = 'ITL'
061800011113     C                   EVAL      vabCAS = *zeros
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062300011113     C                   Z-ADD     vabVMD        wrkDec
062400011113     C                   IF        wrkDec > *zeros
062500011113     C                   IF        vabVAD = 'ITL'
062600011113     C                   EVAL      vabVMD = *zeros
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C                   ENDSR
063100011113     C***
063200011113
063300011113
063400000801
063500000801
063600990920      /TITLE Invio dei dati al punto operativo.
063700010202     C     invio         BEGSR
063800990920     C*
063900090313     C* 1° invio EDIVAT
064000010201     C                   reset                   dscmz
064100010201     C                   move      vlrpoi        cmzdst
064200090313     C                   eval      cmzfld = 'EDIVATWR'
064300010201     C                   eval      cmzmbd = vlrhdl
064400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064500021009     C***                if        prmfir = *blanks
064600090313     C                   eval      cmzfla = 'EDIVAT0F'
064700090313     C                   eval      cmzmba = 'EDIVAT0F'
064800021009     C***                else
064900021009     C***                eval      cmzfla = prmfir
065000021009     C***                eval      cmzmba = prmfir
065100021009     C***                endif
065200010201     C                   eval      cmznrr = *zeros
065300020305     C                   move      §ctrokvt      cmznrr
065400021018     C                   eval      cmzlba = vlrfl1
065500010201     C                   call(e)   'TIS711C'
065600010201     C                   parm                    dscmz
065700010201     C                   parm      *blanks       esito
065800010205     C                   if        %error
065900010205     C                             or cmzerr = '1'
066000010205     C                             or esito  = '1'
066100010205     C                   eval      wrkesito = '3'
066200010205     C                   else
066300010201     C*
066400090313     C* 2° invio EDIVAB
066500010201     C                   reset                   dscmz
066600010201     C                   move      vlrpoi        cmzdst
066700010201     C                   eval      cmzfld = vlrfou
066800010201     C                   eval      cmzmbd = vlrhdl
066900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067000021009     C***                if        prmfir = *blanks
067100090313     C                   eval      cmzfla = 'EDIVAB0F'
067200090313     C                   eval      cmzmba = 'EDIVAB0F'
067300021009     C***                else
067400021009     C***                eval      cmzfla = prmfir
067500021009     C***                eval      cmzmba = prmfir
067600021009     C***                endif
067700010201     C                   eval      cmznrr = *zeros
067800010201     C                   move      §ctrokvb      cmznrr
067900021018     C                   eval      cmzlba = vlrfl1
068000010201     C                   call(e)   'TIS711C'
068100010201     C                   parm                    dscmz
068200010201     C                   parm      *blanks       esito
068300010201     C                   if        %error
068400010201     C                             or cmzerr = '1'
068500010201     C                             or esito  = '1'
068600010201     C                   eval      wrkesito = '3'
068700010201     C                   endif
068800010205     C                   endif
068900990920     C*
069000000613     C                   ENDSR
069100000613     C***
069200990910
069300000613     C     *inzsr        BEGSR
069400990910     C*
069500990910     C     *entry        plist
069600990920     C                   parm                    tivlrds
069700990921     C                   parm      wrkesito      esito
069800000724     C                   parm                    prmlit
069900000710     C                   parm                    prmfir
070000000613     C*
070100000830     C* CALCOLA LA DATA CORRENTE
070200000830     C                   time                    wn14             14 0
070300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
070400000830     C                   z-add     wn8           g08dat
070500000830     C                   z-add     *zeros        g08inv
070600000830     C                   movel     '0'           g08err
070700000830     C                   call      'XSRDA8'
070800000830     C                   parm                    wlbda8
070900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
071000041210     C*
071100041210     C* Chiave su TABEL00F - parziale
071200041210     C     KEYtabP       KLIST
071300041210     C                   KFLD                    tblKUT
071400041210     C                   KFLD                    tblCOD
071500000830     C*
071600000613     C                   ENDSR
071700000613     C***
