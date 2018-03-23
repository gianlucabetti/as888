000100090313      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500041210     Ftabel00f  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700090313     FEDIVABwr  O    E             DISK    usropn
000800090313     FEDIVATwr  O    E             DISK    usropn
000900050131     Ftitvh5p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100050131     Ftitvh5ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300990908
001400000512     D*------------
001500000512     D* COMANDI
001600000512     D*------------
001700011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001800041210     D*------------
001900041210     D* SCHIERE
002000041210     D*------------
002100050131     D skNAZISO        S              3    DIM(1000)
002200050131     D skNAZBAR        S              3    DIM(1000)
002300000801     D*----------------------------------------------------
002400000801     D* DICHIARAZIOINE VARIABILI DI WRK
002500000801     D*----------------------------------------------------
002600990920     D dscmz         e ds                  inz
002700990910     D psds           sds
002800990910     D  procname         *PROC
002900990920     D tivlrds       e ds                  extname(tivlr00f)
003000050131     D tisi95ds      e ds
003100050131     D ds15          e ds
003200990910     D esito           s              1
003300000724     D prmlit          s             10
003400000710     D prmfir          s             10
003500990921     D wrkesito        s                   like(esito)
003600990915     D wrkdata         s               d
003700990915     D wrkora          s               t
003800000613     D rrnum           s              6  0 INZ(*zeros)
003900000621     D recko           s            150    INZ(*blanks)
004000011113     D depcmd          s            150    INZ(*blanks)
004100041210     D parccm          s              8    INZ(*blanks)
004200010202     D parmbr          s             10    INZ(*blanks)
004300010202     D paropz          s              1    INZ(*blanks)
004400010202     D chkcall         s              1    INZ(*blanks)
004500080311     D FlgNewBol       s              1    INZ(*blanks)
004600050131     D jNAZ            s              5  0 INZ(*zeros)
004700000830
004800000830     D*------------------
004900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000000830     D*------------------
005100000830     D WLBDA8          DS                  INZ
005200000830     D  G08DAT                 1      8  0
005300000830     D  G08INV                 9     16  0
005400000830     D  G08ERR                17     17
005500000830     D  G08TGI                18     22  0
005600041025     D*------------------
005700041025     D* DS REPERIMENTO NUMERATORE
005800041025     D*------------------
005900050131     D trul33ds      e ds                  inz
006000041025     D*------------------
006100041025     D* DS ARCHITETTURA
006200041025     D*------------------
006300041025     D kpjba         e ds                  inz
006400041025     D*------------------
006500101217
006600990908
006700101217     D*------------------
006800101217     D* DS RIDEFINIZIONE "DELIVERY CONTACTS"
006900101217     D*------------------
007000101217     D FIELD_37        DS                  INZ
007100101217     D  FLD37_TRCA                   35
007200101217     D  FLD37_TRCB                   25
007300101217     D*------------------
007400101217     D* DS RIDEFINIZIONE "DELIVERY INSTRUCTIONS"
007500101217     D*------------------
007600101217     D FIELD_38        DS                  INZ
007700101217     D  FLD38_TC1                     1
007800101217     D  FLD38_TC2                     1
007900101217     D  FLD38_FILLER1                 1
008000101217     D  FLD38_DCR                     8  0
008100101217     D  FLD38_TCR                     1
008200101217     D  FLD38_GC1                     2
008300101217     D  FLD38_GC2                     2
008400101217     D  FLD38_FILLER2                35
008500101217     D  FLD38_CTR                     3  0
008600101217     D  FLD38_FILLER3                 1
008700101217     D  FLD38_FILLER4                 1
008800101217     D  FLD38_FILLER5                 4
008900010201
009000010201
009100990915     C                   time                    wrkdata
009200990915     C                   time                    wrkora
009300000913     C                   reset                   rrnum
009400990921     C                   reset                   esito
009500990921     C                   reset                   wrkesito
009600000724     C*
009700000724     C* SE OCCORRE SPEDIRE IN FILIALE
009800000724     C                   if        vlrpoi <> *zeros
009900000724     C*
010000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200000724     C     vlrpoi        chain     azorg01l
010300041210     C                   if        %found(azorg01l)
010400000616     C                   movel(p)  CMD(1)        depcmd
010500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010600000616     C*
010700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010800011113     C                   Z-ADD     150           LENGH            15 5
010900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000616     C                   PARM                    depcmd
011100000616     C                   PARM                    LENGH
011200000724     C*
011300000724     C                   endif
011400000724     C                   endif
011500000616     C*
011600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011700000616     C                   movel(p)  CMD(2)        depcmd
011800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011900000616     C*
012000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    depcmd
012400000616     C                   PARM                    LENGH
012500000616     C*
012600050131     C                   if        not %open(titvh5ps)
012700050131     C                   open      titvh5ps
012800000616     C                   except    testdett
012900000613     C                   endif
013000000613     C*
013100050131     C                   EXSR      CARTAB                                       CARICA TABELLE
013200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
013300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013400000613     C*
013500010202     C* Effettuo la chiamata al CLLE preposto
013600090313     C                   call(e)   'TITVEVTC'
013700010202     C                   parm                    parccm
013800010202     C                   parm                    parmbr
013900010202     C                   parm      '2'           paropz
014000050131     C*
014100050131     C* Effettuo lancio TISI95 solo x chiusura
014200050131     C                   CLEAR                   TISI95DS
014300050131     C                   EVAL      I95TLA = 'C'
014400050131     C                   CALL      'TISI95R'
014500050131     C                   PARM                    TISI95DS
014600010202     C*
014700050131     C                   if        %open(titvh5ps)
014800000616     C                   except    findett
014900050131     C                   close     titvh5ps
015000000613     C                   endif
015100000616     C*
015200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015300011113     C                   Z-ADD     150           LENGH            15 5
015400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015500000616     C                   PARM                    CMD(3)
015600000616     C                   PARM                    LENGH
015700000616     C*
015800010201     C                   seton                                        LR
015900000613
016000000613
016100000613     C*--------------------------------------------------------
016200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016300000613     C*--------------------------------------------------------
016400000621     C     STPR          BEGSR
016500000613     C*
016600050131     C                   if        not %open(titvh5p)
016700050131     C                   open      titvh5p
016800990915     C                   endif
016900990915     C*
017000990915     C                   except    riepilogo
017100990915     C*
017200050131     C                   if        %open(titvh5p)
017300050131     C                   close     titvh5p
017400990914     C                   endif
017500990910     C*
017600000613     C                   ENDSR
017700000613     C***
017800041210
017900041210     C*--------------------------------------------------------
018000050131     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
018100041210     C*--------------------------------------------------------
018200041210     C     CARTAB        BEGSR
018300041210     C*
018400041210     C* TABELLA '15' - NAZIONI
018500041210     C                   clear                   skNAZISO
018600041210     C                   clear                   skNAZBAR
018700041210     C                   eval      tblKUT = 1
018800041210     C                   eval      tblCOD = '15'
018900041210     C     KEYtabP       setll     tabel00f
019000041210     C     KEYtabP       reade     tabel00f
019100041210     C                   dow       not %eof(tabel00f)
019200041210     C                   if        tblFLG = *blanks
019300041210     C                   movel(p)  tblUNI        ds15
019400041210     C                   add       1             jNAZ
019500041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
019600041210     C                   eval      skNAZISO(jNAZ) = §15COD
019700041210     C                   endif
019800041210     C     KEYtabP       reade     tabel00f
019900041210     C                   enddo
020000041210     C*
020100041210     C                   ENDSR
020200041210     C***
020300041210
020400041210
020500041210
020600910830     C*--------------------------------------------------------
020700090313     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
020800910830     C*--------------------------------------------------------
020900040526     C     RWFILE        BEGSR
021000990910     C*
021100990914     C                   if        not %open(tivin00r)
021200990908     C                   open      tivin00r
021300990914     C                   endif
021400090313     C                   if        not %open(edivabwr)
021500090313     C                   open      edivabwr
021600990914     C                   endif
021700090313     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
021800020305     C                   exsr      prevat
021900010201     C*
022000010202     C                   if        chkcall = '0'
022100010202     C*
022200090313     C                   if        not %open(edivatwr)
022300090313     C                   open      edivatwr
022400010201     C                   endif
022500990910     C*
022600010201     C                   clear                   §CTROKVB          5 0
022700020305     C                   clear                   §CTROKVT          5 0
022800000801     C                   clear                   §CTRMO            5 0
022900000801     C                   clear                   §CTRNO            5 0
023000040910     C*
023100921023     C                   DO        *HIVAL
023200990913     C*
023300990915     C                   READ      tivin00r                               70
023400040910     C                   if        vindta > *blanks
023500000613     C                   add       1             rrnum
023600000801     C*
023700000801     C                   if        *in70 = *off
023800000801     C                             and
023900000801     C                             (vinflg = *blanks
024000000801     C                              or vinflg = '0'
024100000801     C                              or vinflg = '2')
024200000801     C*
024300000801     C                   clear                   vinmsg
024400000801     C                   eval      vinflg = '1'
024500040910     C*
024600040910     C* Eseguo routine d traduzione
024700050131     C                   exsr      impvabvat
024800040802     C*
024900010305     C                   endif
025000000905     C*
025100000905     C                   else
025200000905     C                   eval      vinflg = '1'
025300000905     C                   endif
025400000905     C*
025500000905     C  N70              update    tivin000
025600000905     C*
025700991022     C  N70              ENDdo
025800041210     C*
025900050131     C* Scarico i buffer eventualmente rimasti in sospeso
026000041210     C                   IF        FlgNewBol = '1'
026100090313     C                   EVAL      VABCMR = 'LEVI'
026200090313     C                   EVAL      VABDCM = datcor
026300090313     C                   EVAL      VABCNT = 1
026400090313     C                   WRITE     EDIVAB00
026500041210     C                   ENDIF
026600010202     C*
026700010202     C                   endif
026800990910
026900990910     C* Se non ci sono record con errori ...
027000000710     C                   if        §ctrno = 0
027100990910     C* ... restituisco esito OK.
027200990921     C                   eval      wrkesito = '0'
027300990910     C                   else
027400010201     C                   if        §ctrokvb > 0
027500990921     C                   eval      wrkesito = '1'
027600000710     C                   else
027700000710     C                   eval      wrkesito = '2'
027800990910     C                   endif
027900000710     C                   endif
028000990910     C*
028100990914     C                   if        %open(tivin00r)
028200990908     C                   close     tivin00r
028300990914     C                   endif
028400090313     C                   if        %open(edivabwr)
028500090313     C                   close     edivabwr
028600990914     C                   endif
028700090313     C                   if        %open(edivatwr)
028800090313     C                   close     edivatwr
028900010201     C                   endif
029000990910     C*
029100010201     C                   if        §ctrokvb > 0
029200000724     C                             and vlrpoi <> *zeros
029300010202     C                   exsr      invio
029400990920     C                   endif
029500990920     C*
029600910830     C                   ENDSR
029700000613     C***
029800990920
029900000801     C*----------------------------------------------------*
030000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030100000801     C*----------------------------------------------------*
030200010201     C     INZVAR        BEGSR
030300000801     C*
030400040802     C                   Z-ADD     *zeros        Num5_0            5 0
030500040802     C                   MOVEL     '0'           FlgCAS            1
030600000801     C*
030700000801     C                   ENDSR
030800000801     C*----------------------------------------------------*
030900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
031000000801     C*----------------------------------------------------*
031100000801     C     DEFCAM        BEGSR
031200000801     C*
031300090313     C                   CLEAR                   EDIVAB00
031400090313     C                   CLEAR                   EDIVAT00
031500020619     C* Imposto i valori di default...
031600050217     C                   Z-ADD     0893062       VABCCM
031700050217     C                   Z-ADD     0893062       VATCCM
031800050217     C                   Z-ADD     089           VABLNP
031900050217     C                   Z-ADD     089           VATLNP
032000050311     C                   Z-ADD     103           VABCTR
032100040823     C                   MOVEL     '1'           VABCBO
032200050131     C                   MOVEL     '7Q'          VABCTM
032300020619     C* ... e poi verifico se sono stati passati come parametri
032400020619     C                   IF        vlrppt > *blanks
032500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
032600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032700020619     C                   EXSR      CHKNUM
032800020619     C                   IF        PiInt=*on
032900020619     C                   Z-ADD     PiVal         VABCCM
033000020619     C                   Z-ADD     PiVal         VATCCM
033100020619     C                   ENDIF
033200040506     C                   ENDIF
033300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
033400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033500020619     C                   EXSR      CHKNUM
033600020619     C                   IF        PiInt=*on
033700020619     C                   Z-ADD     PiVal         VABLNP
033800020619     C                   Z-ADD     PiVal         VATLNP
033900040506     C                   ENDIF
034000020619     C                   ENDIF
034100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
034200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034300020619     C                   EXSR      CHKNUM
034400020619     C                   IF        PiInt=*on
034500020619     C                   Z-ADD     PiVal         VABCTR
034600040506     C                   ENDIF
034700020619     C                   ENDIF
034800041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
034900041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
035000041210     C                   ENDIF
035100020619     C                   ENDIF
035200000801     C*
035300000801     C                   ENDSR
035400000801     C*----------------------------------------------------*
035500090313     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
035600000801     C*----------------------------------------------------*
035700040910     C     IMPVABVAT     BEGSR
035800041210     C*
035900041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
036000041210     C                   Z-ADD     *zeros        errore            1 0
036100050317     C********
036200050317     C* Tipo record 'C1' (dati relativi alla transazione: sender ID, etc...)
036300050317     C********
036400050317     C                   IF        %trim(%subst(vindta:1:2)) = 'C1'
036500050318     C                   Z-ADD     *zeros        wVABCCM           7 0
036600050317     C* Verifico se trattasi d traffico U.K.
036700050317     C                   IF        %trim(%subst(vindta:180:3)) = 'UK1'
036800090311     C***                EVAL      wVABCCM = 0893062
036900090610     C***                EVAL      wVABCCM = 0503436
037000090610     C                   EVAL      wVABCCM = 0504403
037100050317     C                   ENDIF
037200050317     C* Verifico se trattasi d traffico BORNHEM
037300050317     C                   IF        %trim(%subst(vindta:180:3)) = 'B01'
037400090610     C***                EVAL      wVABCCM = 0503436
037500090610     C                   EVAL      wVABCCM = 0504402
037600050317     C                   ENDIF
037700090311     C* Verifico se trattasi d traffico dalla REPUBBLICA CECA
037800090311     C                   IF        %trim(%subst(vindta:180:3)) = 'CZ1'
037900090311     C                   EVAL      wVABCCM = 1161525
038000090311     C                   ENDIF
038100050317     C                   ENDIF
038200040910     C*
038300041210     C********
038400050131     C* Tipo record 'C2' (dati testata: riferimenti, destinatario, contrassegno, etc...)
038500041210     C********
038600050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C2'
038700090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
038800041210     C                   IF        FlgNewBol = '1'
038900090313     C                   EVAL      VABCMR = 'LEVI'
039000090313     C                   EVAL      VABDCM = datcor
039100090313     C                   EVAL      VABCNT = 1
039200090313     C                   WRITE     EDIVAB00
039300050131     C                   EVAL      FlgNewBol = '0'
039400041210     C                   ENDIF
039500041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
039600040910     C                   EXSR      INZVAR
039700040910     C                   EXSR      DEFCAM
039800050318     C* ......verifico il codice cliente mittente reperito da file, se valorizzato utilizzo quello
039900050318     C                   IF        wVABCCM <> *zeros
040000050318     C                   EVAL      VABCCM = wVABCCM
040100050318     C                   EVAL      VATCCM = wVABCCM
040200050318     C                   ENDIF
040300041210     C                   EVAL      FlgNewBol = '1'
040400040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
040500040928     C                   MOVEL     datcor        VABAAS
040600040928     C                   MOVEL     datcor        VATAAS
040700040928     C                   MOVE      datcor        VABMGS
040800040928     C                   MOVE(P)   vlrpoi        VABFGS
040900040928     C                   MOVE(P)   vlrpoi        VATFGS
041000041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
041100041210     C                   CLEAR                   TRUL33DS
041200041210     C                   EVAL      I33OPE = *zeros
041300041210     C                   EVAL      I33CNU = 302
041400041210     C                   EVAL      I33NUM = 1
041500041210     C                   MOVEL     TRUL33DS      KPJBU
041600041210     C                   CALL      'TRUL33R'
041700041210     C                   PARM                    KPJBA
041800041210     C                   MOVEL     KPJBU         TRUL33DS
041900041210     C                   IF        O33ERR = *zeros
042000041210     C                   Z-ADD     O33NRF        VABNSP
042100041210     C                   Z-ADD     O33NRF        VATNSP
042200041210     C                   ELSE
042300041210     C                   ADD       1             errore
042400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
042500041025     C                             + ' ' + 'VABNSP VATNSP'
042600041210     C                   ENDIF
042700041210     C* ......VABRMN
042800050131     C                   EVAL      PiStr=%trim(%subst(vindta:23:8))
042900041210     C                   EXSR      CHKNUM
043000041210     C                   IF        PiInt=*on
043100041210     C                   Z-ADD     PiVal         VABRMN
043200041210     C                   ELSE
043300041210     C                   Z-ADD     1             VABRMN
043400041210     C                   ADD       1             errore
043500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
043600041210     C                             + ' ' + 'VABRMN'
043700041210     C                   ENDIF
043800041210     C* ......VABRMA
043900090901     C***                EVAL      VABRMA=%trim(%subst(vindta:23:8))
044000050131     C* ......VABRSD
044100050131     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
044200100429     C                   IF        %subst(VABRSD:1:8) = 'COIN SPA'
044300100429     C                   EVAL      VABRSD = 'COIN SPA'
044400100429     C                   ENDIF
044500050131     C* ......VABRD2
044600050131     C                   EVAL      VABRD2=%trim(%subst(vindta:76:30))
044700050131     C* ......VABIND
044800050131     C                   EVAL      VABIND=%trim(%subst(vindta:106:30))
044900050131     C* ......VABLOD
045000050131     C                   EVAL      VABLOD=%trim(%subst(vindta:166:30))
045100050131     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
045200050131     C                   EVAL      VABNZD=%trim(%subst(vindta:206:3))
045300050131     C                   Z-ADD     1             jNAZ
045400050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
045500050131     C                   IF        %found
045600050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
045700050131     C                   ENDIF
045800050131     C* ......VABCAD
045900050131     C                   EVAL      PiStr=%trim(%subst(vindta:196:10))
046000050131     C                   EXSR      CHKNUM
046100050131     C                   IF        PiInt=*on
046200050131     C                   Z-ADD     PiVal         Num5_0
046300050131     C                   MOVEL(P)  Num5_0        VABCAD
046400050131     C                   ELSE
046500050131     C                   ADD       1             errore
046600050131     C                   EVAL      vinmsg = %trimr(vinmsg)
046700050131     C                             + ' ' + 'VABCAD'
046800050131     C                   ENDIF
046900050131     C* ......VABPRD (reperita mediante TISI95)
047000050131     C                   IF        VABCAD <> *blanks
047100050131     C                   CLEAR                   TISI95DS
047200050131     C                   EVAL      I95TCN = '3'
047300050131     C                   Z-ADD     datcor        I95DAT
047400050131     C                   EVAL      I95CAP = VABCAD
047500050131     C                   EVAL      I95LOC = VABLOD
047600050131     C                   CALL      'TISI95R'
047700050131     C                   PARM                    TISI95DS
047800050131     C                   EVAL      VABPRD = O95PRV
047900050131     C                   ENDIF
048000101217     C*
048100101217     C* Gestione flag operativi - campo 38
048200101217     C                   EVAL      FIELD_38 = %subst(vindta:329:60)
048300101217     C* ......VABTC1
048400101217     C                   EVAL      VABTC1 = FLD38_TC1
048500101217     C* ......VABTC2
048600101217     C                   EVAL      VABTC2 = FLD38_TC2
048700101217     C* ......VABDCR
048800101217     C                   EVAL      VABDCR = FLD38_DCR
048900101217     C* ......VABTCR
049000101217     C                   EVAL      VABTCR = FLD38_TCR
049100101217     C* ......VABGC1
049200101217     C                   EVAL      VABGC1 = FLD38_GC1
049300101217     C* ......VABGC2
049400101217     C                   EVAL      VABGC2 = FLD38_GC2
049500101217     C* ......VABCTR
049600101217     C                   EVAL      VABCTR = FLD38_CTR
049700101217     C*
049800101217     C* Gestione flag operativi - campo 37
049900101217     C                   EVAL      FIELD_37 = %subst(vindta:239:60)
050000101217     C* ......VATNOT (referente x consegna)
050100101217     C                   EVAL      VATNOT=FLD37_TRCA
050200050131     C                   EVAL      VATTRC = 'A'
050300050131     C                   IF        VATNOT <> *blanks
050400090318     C                   EVAL      VATCMR = 'LEVI'
050500090318     C                   EVAL      VATCNT = 1
050600090313     C                   WRITE     EDIVAT00
050700050131     C                   ENDIF
050800101217     C* ......VATNOT (telefono referente)
050900101217     C                   EVAL      VATNOT=FLD37_TRCB
051000101217     C                   EVAL      VATTRC = 'B'
051100101217     C                   IF        VATNOT <> *blanks
051200101217     C                   EVAL      VATCMR = 'LEVI'
051300101217     C                   EVAL      VATCNT = 1
051400101217     C                   WRITE     EDIVAT00
051500101217     C                   ENDIF
051600050131     C* ......VABCAS
051700050131     C                   IF        %subst(vindta:389:1) = 'Y'
051800050131     C                   EVAL      FlgCAS = '1'
051900050131     C                   EVAL      VABVCA=%trim(%subst(vindta:400:3))
052000050131     C                   EVAL      PiStr=%trim(%subst(vindta:390:10))
052100050131     C                   EXSR      CHKNUM
052200050131     C                   IF        PiNum=*on
052300050131     C                   Z-ADD     PiVal         VABCAS
052400050131     C                   ELSE
052500050131     C                   ADD       1             errore
052600050131     C                   EVAL      vinmsg = %trimr(vinmsg)
052700050131     C                             + ' ' + 'VABCAS'
052800050131     C                   ENDIF
052900050131     C                   ENDIF
053000050131     C*
053100050131     C                   ENDIF
053200050131     C*
053300050131     C********
053400050131     C* Tipo record 'C3' (dati dettaglio: barcode, peso, volume, colli, etc...)
053500050131     C********
053600050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C3'
053700090318     C* ......VABNCL
053800090318     C                   ADD       1             VABNCL
053900050131     C* ......VATNOT ("chi sono")
054000050131     C                   EVAL      VATNOT=%trim(%subst(vindta:51:20))
054100050131     C                   EVAL      VATTRC = 'E'
054200050131     C                   IF        VATNOT <> *blanks
054300090318     C                   EVAL      VATCMR = 'LEVI'
054400090318     C                   EVAL      VATCNT = 1
054500090313     C                   WRITE     EDIVAT00
054600050131     C                   ENDIF
054700050131     C* ......VABPKB
054800050131     C                   EVAL      PiStr=%trim(%subst(vindta:71:7))
054900041210     C                   EXSR      CHKNUM
055000041210     C                   IF        PiNum=*on
055100050131     C                   ADD(H)    PiVal         VABPKB
055200041210     C                   ELSE
055300041210     C                   ADD       1             errore
055400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
055500041210     C                             + ' ' + 'VABPKB'
055600041210     C                   ENDIF
055700050131     C* ......VABVLB
055800100407     C                   SELECT
055900100407     C                   WHEN      VABCCM = 1161525
056000100407     C***                ADD(H)    0,068         VABVLB
056100100407     C                   WHEN      VABCCM = 0893089
056200050407     C* Effettuo forzatura sul volume x codice 0893089 (traffico Bornhem)
056300050407     C                   ADD(H)    0,067         VABVLB
056400100407     C                   OTHER
056500050401     C                   EVAL      PiStr=%trim(%subst(vindta:80:7))
056600050401     C                   EXSR      CHKNUM
056700050401     C                   IF        PiNum=*on
056800050401     C                   ADD(H)    PiVal         VABVLB
056900050401     C                   ELSE
057000050401     C                   ADD       1             errore
057100050401     C                   EVAL      vinmsg = %trimr(vinmsg)
057200050401     C                             + ' ' + 'VABVLB'
057300050401     C                   ENDIF
057400100407     C                   ENDSL
057500041210     C*
057600041210     C                   ENDIF
057700101217     C*
057800101217     C********
057900101217     C* Tipo record 'C4' (dati di ordine, etc...)
058000101217     C********
058100101217     C                   IF        %trim(%subst(vindta:1:2)) = 'C4'
058200101217     C* ......VABNOT
058300101217     C                   EVAL      VABNOT=%trim(%subst(vindta:60:20))
058400101217     C*
058500101217     C                   ENDIF
058600040910     C*
058700040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
058800040910     C                   IF        FlgCAS <> '0'
058900040929     C                   IF        VABCBO = '1'
059000040910     C                   EVAL      VABCBO = '4'
059100040910     C                   ELSE
059200040929     C                   EVAL      VABCBO = '6'
059300040910     C                   ENDIF
059400040929     C                   ENDIF
059500040910     C*
059600040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
059700040910     C                   EXSR      CHKIMPDIV
059800010202     C*
059900000801     C* Ebbene...
060000000801     C                   ADD       1             §CTRMO
060100010201     C                   IF        errore <> *zeros
060200000801     C                   ADD       1             §CTRNO
060300000801     C                   EVAL      recko = vindta
060400000801     C                   EXCEPT    rigadett
060500000801     C                   EVAL      vinflg = '2'
060600000801     C                   ELSE
060700010201     C                   ADD       1             §CTROKVB
060800000801     C                   ENDIF
060900000801     C*
061000000801     C                   ENDSR
061100010202     C*----------------------------------------------------*
061200090313     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
061300010202     C*----------------------------------------------------*
061400020305     C     PREVAT        BEGSR
061500010202     C*
061600090313     C* Compongo il nome del membro da dare al EDIVATWR
061700010202     C                   eval      parmbr = vlrhdl
061800010202     C                   movel     'M'           parmbr
061900041210     C                   eval      parccm = vlrksc
062000010202     C                   eval      paropz = '1'
062100010202     C* Effettuo la chiamata al CLLE preposto
062200090313     C                   call(e)   'TITVEVTC'
062300010202     C                   parm                    parccm
062400010202     C                   parm                    parmbr
062500010202     C                   parm                    paropz
062600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062700010202     C                   if        %error
062800010202     C                   movel     '1'           chkcall
062900010202     C                   else
063000010202     C                   movel     '0'           chkcall
063100010202     C                   endif
063200010202     C*
063300010202     C                   ENDSR
063400000801     C*----------------------------------------------------*
063500000801     C*  CONTROLLO NUMERICITA' CAMPI
063600000801     C*----------------------------------------------------*
063700000801     C     CHKNUM        BEGSR
063800000801     C*
063900000801     C                   call(e)   'ISNUMERIC'
064000000801     C                   PARM                    PiStr            30
064100041210     C                   PARM      '.'           PiDecChr          1
064200000801     C                   PARM      *ZEROS        PiVal            30 9
064300000801     C                   PARM      '0'           PiInt             1
064400000801     C                   PARM      '0'           PiNum             1
064500000801     C                   IF        %error
064600000801     C                   EVAL      PiInt=*off
064700000801     C                   ENDIF
064800000801     C*
064900000801     C                   ENDSR
065000000801     C***
065100000801
065200011113
065300011113     C*----------------------------------------------------*
065400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065500011113     C*----------------------------------------------------*
065600011113     C     CHKIMPDIV     BEGSR
065700011113     C*
065800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065900011113     C                   Z-ADD     *zeros        wrkDec            9 9
066000011113     C*
066100011113     C* Come prima cosa effettuo considerazioni sulla divisa
066200011113     C                   IF        vabIAS > *zeros
066300011113     C                   IF        vabVAS <> 'EUR'
066400011113     C                   EVAL      vabVAS =  'ITL'
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C                   IF        vabCAS > *zeros
066900011113     C                   IF        vabVCA <> 'EUR'
067000011113     C                   EVAL      vabVCA =  'ITL'
067100011113     C                   ENDIF
067200011113     C                   ENDIF
067300011113     C*
067400011113     C                   IF        vabVMD > *zeros
067500020305     C                   IF        vabVAD <> 'EUR'
067600011113     C                   EVAL      vabVAD =  'ITL'
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068100011113     C                   Z-ADD     vabIAS        wrkDec
068200011113     C                   IF        wrkDec > *zeros
068300011113     C                   IF        vabVAS = 'ITL'
068400011113     C                   EVAL      vabIAS = *zeros
068500011113     C                   ENDIF
068600011113     C                   ENDIF
068700011113     C*
068800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068900011113     C                   Z-ADD     vabCAS        wrkDec
069000011113     C                   IF        wrkDec > *zeros
069100011113     C                   IF        vabVCA = 'ITL'
069200011113     C                   EVAL      vabCAS = *zeros
069300011113     C                   ENDIF
069400011113     C                   ENDIF
069500011113     C*
069600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069700011113     C                   Z-ADD     vabVMD        wrkDec
069800011113     C                   IF        wrkDec > *zeros
069900011113     C                   IF        vabVAD = 'ITL'
070000011113     C                   EVAL      vabVMD = *zeros
070100011113     C                   ENDIF
070200011113     C                   ENDIF
070300011113     C*
070400011113     C                   ENDSR
070500011113     C***
070600011113
070700011113
070800000801
070900000801
071000990920      /TITLE Invio dei dati al punto operativo.
071100010202     C     invio         BEGSR
071200990920     C*
071300090313     C* 1° invio EDIVAT
071400010201     C                   reset                   dscmz
071500010201     C                   move      vlrpoi        cmzdst
071600090313     C                   eval      cmzfld = 'EDIVATWR'
071700010201     C                   eval      cmzmbd = vlrhdl
071800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071900021009     C***                if        prmfir = *blanks
072000090313     C                   eval      cmzfla = 'EDIVAT0F'
072100090313     C                   eval      cmzmba = 'EDIVAT0F'
072200021009     C***                else
072300021009     C***                eval      cmzfla = prmfir
072400021009     C***                eval      cmzmba = prmfir
072500021009     C***                endif
072600010201     C                   eval      cmznrr = *zeros
072700020305     C                   move      §ctrokvt      cmznrr
072800021018     C                   eval      cmzlba = vlrfl1
072900010201     C                   call(e)   'TIS711C'
073000010201     C                   parm                    dscmz
073100010201     C                   parm      *blanks       esito
073200010205     C                   if        %error
073300010205     C                             or cmzerr = '1'
073400010205     C                             or esito  = '1'
073500010205     C                   eval      wrkesito = '3'
073600010205     C                   else
073700010201     C*
073800090313     C* 2° invio EDIVAB
073900010201     C                   reset                   dscmz
074000010201     C                   move      vlrpoi        cmzdst
074100010201     C                   eval      cmzfld = vlrfou
074200010201     C                   eval      cmzmbd = vlrhdl
074300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074400021009     C***                if        prmfir = *blanks
074500090313     C                   eval      cmzfla = 'EDIVAB0F'
074600090313     C                   eval      cmzmba = 'EDIVAB0F'
074700021009     C***                else
074800021009     C***                eval      cmzfla = prmfir
074900021009     C***                eval      cmzmba = prmfir
075000021009     C***                endif
075100010201     C                   eval      cmznrr = *zeros
075200010201     C                   move      §ctrokvb      cmznrr
075300021018     C                   eval      cmzlba = vlrfl1
075400010201     C                   call(e)   'TIS711C'
075500010201     C                   parm                    dscmz
075600010201     C                   parm      *blanks       esito
075700010201     C                   if        %error
075800010201     C                             or cmzerr = '1'
075900010201     C                             or esito  = '1'
076000010201     C                   eval      wrkesito = '3'
076100010201     C                   endif
076200010205     C                   endif
076300990920     C*
076400000613     C                   ENDSR
076500000613     C***
076600990910
076700000613     C     *inzsr        BEGSR
076800990910     C*
076900990910     C     *entry        plist
077000990920     C                   parm                    tivlrds
077100990921     C                   parm      wrkesito      esito
077200000724     C                   parm                    prmlit
077300000710     C                   parm                    prmfir
077400000613     C*
077500000830     C* CALCOLA LA DATA CORRENTE
077600000830     C                   time                    wn14             14 0
077700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
077800000830     C                   z-add     wn8           g08dat
077900000830     C                   z-add     *zeros        g08inv
078000000830     C                   movel     '0'           g08err
078100000830     C                   call      'XSRDA8'
078200000830     C                   parm                    wlbda8
078300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
078400041210     C*
078500041210     C* Chiave su TABEL00F - parziale
078600041210     C     KEYtabP       KLIST
078700041210     C                   KFLD                    tblKUT
078800041210     C                   KFLD                    tblCOD
078900000830     C*
079000000613     C                   ENDSR
079100000613     C***
079200990908
079300050131     Otitvh5p   E            riepilogo         2
079400990915     O                                              'Upload via Internet'
079500990915     O                                           +1 'Traduzione TIVIN00R -'
079600090313     O                                           +1 'EDIVABWR/EDIVATWR'
079700010201     O                                           +1 'Report testate bolle'
079800990915     O          E            riepilogo   2
079900990915     O                       wrkdata
080000990915     O                       wrkora              +1
080100990915     O                       procname            +1
080200990915     O          E            riepilogo   2
080300990915     O                                              'Cliente..................:'
080400990915     O                       VABCCM        z     +1
080500990915     O          E            riepilogo   2
080600990920     O                                              'Riferimento Strategi.....:'
080700990920     O                       vlrhdl              +1
080800990915     O          E            riepilogo   2
080900990915     O                                              'Giusti...................:'
081000010201     O                       §CTROKVB      2   +  1
081100990915     O          E            riepilogo   2
081200990915     O                                              'Sbagliati e corretti.....:'
081300971022     O                       §CTRMO        2   +  1
081400990915     O          E            riepilogo   2
081500990915     O                                              'Sbagliati e scartati.....:'
081600971022     O                       §CTRNO        2   +  1
081700000613
081800050131     Otitvh5ps  E            testdett          2
081900000613     O                                              'Upload via Internet'
082000000613     O                                           +1 'Traduzione TIVIN00R -'
082100090313     O                                           +1 'EDIVABWR/EDIVATWR'
082200010201     O                                           +1 'Report testate bolle'
082300000616     O          E            testdett    3
082400000613     O                                           +2 'N° rec'
082500000613     O                                           +3 'Anteprima contenuto'
082600000616     O          E            rigadett    2
082700000613     O                       rrnum               +2
082800000621     O                       recko               +3
082900000616     O          E            findett     2
083000000613     O                       wrkdata
083100000613     O                       wrkora              +1
083200000613     O                       procname            +1
083300000616     O          E            findett     2
083400000613     O                                              'Cliente..................:'
083500000613     O                       VABCCM        z     +1
083600000616     O          E            findett     2
083700000613     O                                              'Riferimento Strategi.....:'
083800000613     O                       vlrhdl              +1
083900000616     O          E            findett     2
084000000613     O                                              'Giusti...................:'
084100010201     O                       §CTROKVB      2   +  1
084200000616     O          E            findett     2
084300000613     O                                              'Sbagliati e corretti.....:'
084400000613     O                       §CTRMO        2   +  1
084500000616     O          E            findett     2
084600000613     O                                              'Sbagliati e scartati.....:'
084700000613     O                       §CTRNO        2   +  1
084800000512** CMD - COMANDI CL
084900050131OVRPRTF FILE(TITVH5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
085000050131OVRPRTF FILE(TITVH5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
085100050131DLTOVR FILE(TITVH5P TITVH5PS) LVL(*)
085200000512
085300000512
