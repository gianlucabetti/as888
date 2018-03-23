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
008000101222     D  FLD38_DCR                     8
008100101217     D  FLD38_TCR                     1
008200101217     D  FLD38_GC1                     2
008300101217     D  FLD38_GC2                     2
008400101217     D  FLD38_FILLER2                35
008500101222     D  FLD38_CTR                     3
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
026100110318     C                   EVAL      VABCMR = 'LEVI ' + wCMR
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
036100110318     C********
036200110318     C* Tipo record '00' (dati generali a livello d file
036300110318     C********
036400110318     C                   IF        %trim(%subst(vindta:1:2)) = '00'
036500110318     C                   MOVEL     *blanks       wCMR             20
036600110318     C                   EVAL      wCMR = %trim(%subst(vindta:30:20))
036700110318     C                   ENDIF
036800050317     C********
036900050317     C* Tipo record 'C1' (dati relativi alla transazione: sender ID, etc...)
037000050317     C********
037100050317     C                   IF        %trim(%subst(vindta:1:2)) = 'C1'
037200050318     C                   Z-ADD     *zeros        wVABCCM           7 0
037300050317     C* Verifico se trattasi d traffico U.K.
037400050317     C                   IF        %trim(%subst(vindta:180:3)) = 'UK1'
037500090311     C***                EVAL      wVABCCM = 0893062
037600090610     C***                EVAL      wVABCCM = 0503436
037700090610     C                   EVAL      wVABCCM = 0504403
037800050317     C                   ENDIF
037900050317     C* Verifico se trattasi d traffico BORNHEM
038000050317     C                   IF        %trim(%subst(vindta:180:3)) = 'B01'
038100090610     C***                EVAL      wVABCCM = 0503436
038200090610     C                   EVAL      wVABCCM = 0504402
038300050317     C                   ENDIF
038400090311     C* Verifico se trattasi d traffico dalla REPUBBLICA CECA
038500090311     C                   IF        %trim(%subst(vindta:180:3)) = 'CZ1'
038600090311     C                   EVAL      wVABCCM = 1161525
038700090311     C                   ENDIF
038800050317     C                   ENDIF
038900040910     C*
039000041210     C********
039100050131     C* Tipo record 'C2' (dati testata: riferimenti, destinatario, contrassegno, etc...)
039200041210     C********
039300050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C2'
039400090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
039500041210     C                   IF        FlgNewBol = '1'
039600110318     C                   EVAL      VABCMR = 'LEVI ' + wCMR
039700090313     C                   EVAL      VABDCM = datcor
039800090313     C                   EVAL      VABCNT = 1
039900090313     C                   WRITE     EDIVAB00
040000050131     C                   EVAL      FlgNewBol = '0'
040100041210     C                   ENDIF
040200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
040300040910     C                   EXSR      INZVAR
040400040910     C                   EXSR      DEFCAM
040500050318     C* ......verifico il codice cliente mittente reperito da file, se valorizzato utilizzo quello
040600050318     C                   IF        wVABCCM <> *zeros
040700050318     C                   EVAL      VABCCM = wVABCCM
040800050318     C                   EVAL      VATCCM = wVABCCM
040900050318     C                   ENDIF
041000041210     C                   EVAL      FlgNewBol = '1'
041100040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
041200040928     C                   MOVEL     datcor        VABAAS
041300040928     C                   MOVEL     datcor        VATAAS
041400040928     C                   MOVE      datcor        VABMGS
041500040928     C                   MOVE(P)   vlrpoi        VABFGS
041600040928     C                   MOVE(P)   vlrpoi        VATFGS
041700041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
041800041210     C                   CLEAR                   TRUL33DS
041900041210     C                   EVAL      I33OPE = *zeros
042000041210     C                   EVAL      I33CNU = 302
042100041210     C                   EVAL      I33NUM = 1
042200041210     C                   MOVEL     TRUL33DS      KPJBU
042300041210     C                   CALL      'TRUL33R'
042400041210     C                   PARM                    KPJBA
042500041210     C                   MOVEL     KPJBU         TRUL33DS
042600041210     C                   IF        O33ERR = *zeros
042700041210     C                   Z-ADD     O33NRF        VABNSP
042800041210     C                   Z-ADD     O33NRF        VATNSP
042900041210     C                   ELSE
043000041210     C                   ADD       1             errore
043100041210     C                   EVAL      vinmsg = %trimr(vinmsg)
043200041025     C                             + ' ' + 'VABNSP VATNSP'
043300041210     C                   ENDIF
043400041210     C* ......VABRMN
043500050131     C                   EVAL      PiStr=%trim(%subst(vindta:23:8))
043600041210     C                   EXSR      CHKNUM
043700041210     C                   IF        PiInt=*on
043800041210     C                   Z-ADD     PiVal         VABRMN
043900041210     C                   ELSE
044000041210     C                   Z-ADD     1             VABRMN
044100041210     C                   ADD       1             errore
044200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
044300041210     C                             + ' ' + 'VABRMN'
044400041210     C                   ENDIF
044500041210     C* ......VABRMA
044600090901     C***                EVAL      VABRMA=%trim(%subst(vindta:23:8))
044700050131     C* ......VABRSD
044800050131     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
044900100429     C                   IF        %subst(VABRSD:1:8) = 'COIN SPA'
045000100429     C                   EVAL      VABRSD = 'COIN SPA'
045100100429     C                   ENDIF
045200050131     C* ......VABRD2
045300050131     C                   EVAL      VABRD2=%trim(%subst(vindta:76:30))
045400050131     C* ......VABIND
045500050131     C                   EVAL      VABIND=%trim(%subst(vindta:106:30))
045600050131     C* ......VABLOD
045700050131     C                   EVAL      VABLOD=%trim(%subst(vindta:166:30))
045800050131     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
045900050131     C                   EVAL      VABNZD=%trim(%subst(vindta:206:3))
046000050131     C                   Z-ADD     1             jNAZ
046100050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
046200050131     C                   IF        %found
046300050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
046400050131     C                   ENDIF
046500050131     C* ......VABCAD
046600050131     C                   EVAL      PiStr=%trim(%subst(vindta:196:10))
046700050131     C                   EXSR      CHKNUM
046800050131     C                   IF        PiInt=*on
046900050131     C                   Z-ADD     PiVal         Num5_0
047000050131     C                   MOVEL(P)  Num5_0        VABCAD
047100050131     C                   ELSE
047200050131     C                   ADD       1             errore
047300050131     C                   EVAL      vinmsg = %trimr(vinmsg)
047400050131     C                             + ' ' + 'VABCAD'
047500050131     C                   ENDIF
047600050131     C* ......VABPRD (reperita mediante TISI95)
047700050131     C                   IF        VABCAD <> *blanks
047800050131     C                   CLEAR                   TISI95DS
047900050131     C                   EVAL      I95TCN = '3'
048000050131     C                   Z-ADD     datcor        I95DAT
048100050131     C                   EVAL      I95CAP = VABCAD
048200050131     C                   EVAL      I95LOC = VABLOD
048300050131     C                   CALL      'TISI95R'
048400050131     C                   PARM                    TISI95DS
048500050131     C                   EVAL      VABPRD = O95PRV
048600050131     C                   ENDIF
048700101217     C*
048800101217     C* Gestione flag operativi - campo 38
048900101217     C                   EVAL      FIELD_38 = %subst(vindta:329:60)
049000101217     C* ......VABTC1
049100101217     C                   EVAL      VABTC1 = FLD38_TC1
049200101217     C* ......VABTC2
049300101217     C                   EVAL      VABTC2 = FLD38_TC2
049400101222     C* ......VABDCR
049500101222     C                   IF        FLD38_DCR <> *blanks
049600101222     C                   MOVE(P)   FLD38_DCR     VABDCR
049700101222     C                   ENDIF
049800101217     C* ......VABTCR
049900101217     C                   EVAL      VABTCR = FLD38_TCR
050000101217     C* ......VABGC1
050100101217     C                   EVAL      VABGC1 = FLD38_GC1
050200101217     C* ......VABGC2
050300101217     C                   EVAL      VABGC2 = FLD38_GC2
050400101217     C* ......VABCTR
050500101222     C                   IF        FLD38_CTR <> *blanks
050600101222     C                   MOVE(P)   FLD38_CTR     VABCTR
050700101222     C                   ENDIF
050800101217     C*
050900101217     C* Gestione flag operativi - campo 37
051000101217     C                   EVAL      FIELD_37 = %subst(vindta:239:60)
051100101217     C* ......VATNOT (referente x consegna)
051200101217     C                   EVAL      VATNOT=FLD37_TRCA
051300050131     C                   EVAL      VATTRC = 'A'
051400050131     C                   IF        VATNOT <> *blanks
051500110318     C                   EVAL      VATCMR = 'LEVI ' + wCMR
051600090318     C                   EVAL      VATCNT = 1
051700090313     C                   WRITE     EDIVAT00
051800050131     C                   ENDIF
051900101217     C* ......VATNOT (telefono referente)
052000101217     C                   EVAL      VATNOT=FLD37_TRCB
052100101217     C                   EVAL      VATTRC = 'B'
052200101217     C                   IF        VATNOT <> *blanks
052300110318     C                   EVAL      VATCMR = 'LEVI ' + wCMR
052400101217     C                   EVAL      VATCNT = 1
052500101217     C                   WRITE     EDIVAT00
052600101217     C                   ENDIF
052700050131     C* ......VABCAS
052800050131     C                   IF        %subst(vindta:389:1) = 'Y'
052900050131     C                   EVAL      FlgCAS = '1'
053000050131     C                   EVAL      VABVCA=%trim(%subst(vindta:400:3))
053100050131     C                   EVAL      PiStr=%trim(%subst(vindta:390:10))
053200050131     C                   EXSR      CHKNUM
053300050131     C                   IF        PiNum=*on
053400050131     C                   Z-ADD     PiVal         VABCAS
053500050131     C                   ELSE
053600050131     C                   ADD       1             errore
053700050131     C                   EVAL      vinmsg = %trimr(vinmsg)
053800050131     C                             + ' ' + 'VABCAS'
053900050131     C                   ENDIF
054000050131     C                   ENDIF
054100050131     C*
054200050131     C                   ENDIF
054300050131     C*
054400050131     C********
054500050131     C* Tipo record 'C3' (dati dettaglio: barcode, peso, volume, colli, etc...)
054600050131     C********
054700050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C3'
054800090318     C* ......VABNCL
054900090318     C                   ADD       1             VABNCL
055000050131     C* ......VATNOT ("chi sono")
055100050131     C                   EVAL      VATNOT=%trim(%subst(vindta:51:20))
055200050131     C                   EVAL      VATTRC = 'E'
055300050131     C                   IF        VATNOT <> *blanks
055400110318     C                   EVAL      VATCMR = 'LEVI ' + wCMR
055500090318     C                   EVAL      VATCNT = 1
055600090313     C                   WRITE     EDIVAT00
055700050131     C                   ENDIF
055800050131     C* ......VABPKB
055900050131     C                   EVAL      PiStr=%trim(%subst(vindta:71:7))
056000041210     C                   EXSR      CHKNUM
056100041210     C                   IF        PiNum=*on
056200050131     C                   ADD(H)    PiVal         VABPKB
056300041210     C                   ELSE
056400041210     C                   ADD       1             errore
056500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
056600041210     C                             + ' ' + 'VABPKB'
056700041210     C                   ENDIF
056800050131     C* ......VABVLB
056900100407     C                   SELECT
057000100407     C                   WHEN      VABCCM = 1161525
057100100407     C***                ADD(H)    0,068         VABVLB
057200100407     C                   WHEN      VABCCM = 0893089
057300050407     C* Effettuo forzatura sul volume x codice 0893089 (traffico Bornhem)
057400050407     C                   ADD(H)    0,067         VABVLB
057500100407     C                   OTHER
057600050401     C                   EVAL      PiStr=%trim(%subst(vindta:80:7))
057700050401     C                   EXSR      CHKNUM
057800050401     C                   IF        PiNum=*on
057900050401     C                   ADD(H)    PiVal         VABVLB
058000050401     C                   ELSE
058100050401     C                   ADD       1             errore
058200050401     C                   EVAL      vinmsg = %trimr(vinmsg)
058300050401     C                             + ' ' + 'VABVLB'
058400050401     C                   ENDIF
058500100407     C                   ENDSL
058600041210     C*
058700041210     C                   ENDIF
058800101217     C*
058900101217     C********
059000101217     C* Tipo record 'C4' (dati di ordine, etc...)
059100101217     C********
059200101217     C                   IF        %trim(%subst(vindta:1:2)) = 'C4'
059300101217     C* ......VABNOT
059400101217     C                   EVAL      VABNOT=%trim(%subst(vindta:60:20))
059500101217     C*
059600101217     C                   ENDIF
059700040910     C*
059800040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
059900040910     C                   IF        FlgCAS <> '0'
060000040929     C                   IF        VABCBO = '1'
060100040910     C                   EVAL      VABCBO = '4'
060200040910     C                   ELSE
060300040929     C                   EVAL      VABCBO = '6'
060400040910     C                   ENDIF
060500040929     C                   ENDIF
060600040910     C*
060700040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
060800040910     C                   EXSR      CHKIMPDIV
060900010202     C*
061000000801     C* Ebbene...
061100000801     C                   ADD       1             §CTRMO
061200010201     C                   IF        errore <> *zeros
061300000801     C                   ADD       1             §CTRNO
061400000801     C                   EVAL      recko = vindta
061500000801     C                   EXCEPT    rigadett
061600000801     C                   EVAL      vinflg = '2'
061700000801     C                   ELSE
061800010201     C                   ADD       1             §CTROKVB
061900000801     C                   ENDIF
062000000801     C*
062100000801     C                   ENDSR
062200010202     C*----------------------------------------------------*
062300090313     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
062400010202     C*----------------------------------------------------*
062500020305     C     PREVAT        BEGSR
062600010202     C*
062700090313     C* Compongo il nome del membro da dare al EDIVATWR
062800010202     C                   eval      parmbr = vlrhdl
062900010202     C                   movel     'M'           parmbr
063000041210     C                   eval      parccm = vlrksc
063100010202     C                   eval      paropz = '1'
063200010202     C* Effettuo la chiamata al CLLE preposto
063300090313     C                   call(e)   'TITVEVTC'
063400010202     C                   parm                    parccm
063500010202     C                   parm                    parmbr
063600010202     C                   parm                    paropz
063700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063800010202     C                   if        %error
063900010202     C                   movel     '1'           chkcall
064000010202     C                   else
064100010202     C                   movel     '0'           chkcall
064200010202     C                   endif
064300010202     C*
064400010202     C                   ENDSR
064500000801     C*----------------------------------------------------*
064600000801     C*  CONTROLLO NUMERICITA' CAMPI
064700000801     C*----------------------------------------------------*
064800000801     C     CHKNUM        BEGSR
064900000801     C*
065000000801     C                   call(e)   'ISNUMERIC'
065100000801     C                   PARM                    PiStr            30
065200041210     C                   PARM      '.'           PiDecChr          1
065300000801     C                   PARM      *ZEROS        PiVal            30 9
065400000801     C                   PARM      '0'           PiInt             1
065500000801     C                   PARM      '0'           PiNum             1
065600000801     C                   IF        %error
065700000801     C                   EVAL      PiInt=*off
065800000801     C                   ENDIF
065900000801     C*
066000000801     C                   ENDSR
066100000801     C***
066200000801
066300011113
066400011113     C*----------------------------------------------------*
066500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066600011113     C*----------------------------------------------------*
066700011113     C     CHKIMPDIV     BEGSR
066800011113     C*
066900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067000011113     C                   Z-ADD     *zeros        wrkDec            9 9
067100011113     C*
067200011113     C* Come prima cosa effettuo considerazioni sulla divisa
067300011113     C                   IF        vabIAS > *zeros
067400011113     C                   IF        vabVAS <> 'EUR'
067500011113     C                   EVAL      vabVAS =  'ITL'
067600011113     C                   ENDIF
067700011113     C                   ENDIF
067800011113     C*
067900011113     C                   IF        vabCAS > *zeros
068000011113     C                   IF        vabVCA <> 'EUR'
068100011113     C                   EVAL      vabVCA =  'ITL'
068200011113     C                   ENDIF
068300011113     C                   ENDIF
068400011113     C*
068500011113     C                   IF        vabVMD > *zeros
068600020305     C                   IF        vabVAD <> 'EUR'
068700011113     C                   EVAL      vabVAD =  'ITL'
068800011113     C                   ENDIF
068900011113     C                   ENDIF
069000011113     C*
069100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069200011113     C                   Z-ADD     vabIAS        wrkDec
069300011113     C                   IF        wrkDec > *zeros
069400011113     C                   IF        vabVAS = 'ITL'
069500011113     C                   EVAL      vabIAS = *zeros
069600011113     C                   ENDIF
069700011113     C                   ENDIF
069800011113     C*
069900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
070000011113     C                   Z-ADD     vabCAS        wrkDec
070100011113     C                   IF        wrkDec > *zeros
070200011113     C                   IF        vabVCA = 'ITL'
070300011113     C                   EVAL      vabCAS = *zeros
070400011113     C                   ENDIF
070500011113     C                   ENDIF
070600011113     C*
070700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070800011113     C                   Z-ADD     vabVMD        wrkDec
070900011113     C                   IF        wrkDec > *zeros
071000011113     C                   IF        vabVAD = 'ITL'
071100011113     C                   EVAL      vabVMD = *zeros
071200011113     C                   ENDIF
071300011113     C                   ENDIF
071400011113     C*
071500011113     C                   ENDSR
071600011113     C***
071700011113
071800011113
071900000801
072000000801
072100990920      /TITLE Invio dei dati al punto operativo.
072200010202     C     invio         BEGSR
072300990920     C*
072400090313     C* 1° invio EDIVAT
072500010201     C                   reset                   dscmz
072600010201     C                   move      vlrpoi        cmzdst
072700090313     C                   eval      cmzfld = 'EDIVATWR'
072800010201     C                   eval      cmzmbd = vlrhdl
072900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073000021009     C***                if        prmfir = *blanks
073100090313     C                   eval      cmzfla = 'EDIVAT0F'
073200090313     C                   eval      cmzmba = 'EDIVAT0F'
073300021009     C***                else
073400021009     C***                eval      cmzfla = prmfir
073500021009     C***                eval      cmzmba = prmfir
073600021009     C***                endif
073700010201     C                   eval      cmznrr = *zeros
073800020305     C                   move      §ctrokvt      cmznrr
073900021018     C                   eval      cmzlba = vlrfl1
074000010201     C                   call(e)   'TIS711C'
074100010201     C                   parm                    dscmz
074200010201     C                   parm      *blanks       esito
074300010205     C                   if        %error
074400010205     C                             or cmzerr = '1'
074500010205     C                             or esito  = '1'
074600010205     C                   eval      wrkesito = '3'
074700010205     C                   else
074800010201     C*
074900090313     C* 2° invio EDIVAB
075000010201     C                   reset                   dscmz
075100010201     C                   move      vlrpoi        cmzdst
075200010201     C                   eval      cmzfld = vlrfou
075300010201     C                   eval      cmzmbd = vlrhdl
075400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075500021009     C***                if        prmfir = *blanks
075600090313     C                   eval      cmzfla = 'EDIVAB0F'
075700090313     C                   eval      cmzmba = 'EDIVAB0F'
075800021009     C***                else
075900021009     C***                eval      cmzfla = prmfir
076000021009     C***                eval      cmzmba = prmfir
076100021009     C***                endif
076200010201     C                   eval      cmznrr = *zeros
076300010201     C                   move      §ctrokvb      cmznrr
076400021018     C                   eval      cmzlba = vlrfl1
076500010201     C                   call(e)   'TIS711C'
076600010201     C                   parm                    dscmz
076700010201     C                   parm      *blanks       esito
076800010201     C                   if        %error
076900010201     C                             or cmzerr = '1'
077000010201     C                             or esito  = '1'
077100010201     C                   eval      wrkesito = '3'
077200010201     C                   endif
077300010205     C                   endif
077400990920     C*
077500000613     C                   ENDSR
077600000613     C***
077700990910
077800000613     C     *inzsr        BEGSR
077900990910     C*
078000990910     C     *entry        plist
078100990920     C                   parm                    tivlrds
078200990921     C                   parm      wrkesito      esito
078300000724     C                   parm                    prmlit
078400000710     C                   parm                    prmfir
078500000613     C*
078600000830     C* CALCOLA LA DATA CORRENTE
078700000830     C                   time                    wn14             14 0
078800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
078900000830     C                   z-add     wn8           g08dat
079000000830     C                   z-add     *zeros        g08inv
079100000830     C                   movel     '0'           g08err
079200000830     C                   call      'XSRDA8'
079300000830     C                   parm                    wlbda8
079400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
079500041210     C*
079600041210     C* Chiave su TABEL00F - parziale
079700041210     C     KEYtabP       KLIST
079800041210     C                   KFLD                    tblKUT
079900041210     C                   KFLD                    tblCOD
080000000830     C*
080100000613     C                   ENDSR
080200000613     C***
080300990908
080400050131     Otitvh5p   E            riepilogo         2
080500990915     O                                              'Upload via Internet'
080600990915     O                                           +1 'Traduzione TIVIN00R -'
080700090313     O                                           +1 'EDIVABWR/EDIVATWR'
080800010201     O                                           +1 'Report testate bolle'
080900990915     O          E            riepilogo   2
081000990915     O                       wrkdata
081100990915     O                       wrkora              +1
081200990915     O                       procname            +1
081300990915     O          E            riepilogo   2
081400990915     O                                              'Cliente..................:'
081500990915     O                       VABCCM        z     +1
081600990915     O          E            riepilogo   2
081700990920     O                                              'Riferimento Strategi.....:'
081800990920     O                       vlrhdl              +1
081900990915     O          E            riepilogo   2
082000990915     O                                              'Giusti...................:'
082100010201     O                       §CTROKVB      2   +  1
082200990915     O          E            riepilogo   2
082300990915     O                                              'Sbagliati e corretti.....:'
082400971022     O                       §CTRMO        2   +  1
082500990915     O          E            riepilogo   2
082600990915     O                                              'Sbagliati e scartati.....:'
082700971022     O                       §CTRNO        2   +  1
082800000613
082900050131     Otitvh5ps  E            testdett          2
083000000613     O                                              'Upload via Internet'
083100000613     O                                           +1 'Traduzione TIVIN00R -'
083200090313     O                                           +1 'EDIVABWR/EDIVATWR'
083300010201     O                                           +1 'Report testate bolle'
083400000616     O          E            testdett    3
083500000613     O                                           +2 'N° rec'
083600000613     O                                           +3 'Anteprima contenuto'
083700000616     O          E            rigadett    2
083800000613     O                       rrnum               +2
083900000621     O                       recko               +3
084000000616     O          E            findett     2
084100000613     O                       wrkdata
084200000613     O                       wrkora              +1
084300000613     O                       procname            +1
084400000616     O          E            findett     2
084500000613     O                                              'Cliente..................:'
084600000613     O                       VABCCM        z     +1
084700000616     O          E            findett     2
084800000613     O                                              'Riferimento Strategi.....:'
084900000613     O                       vlrhdl              +1
085000000616     O          E            findett     2
085100000613     O                                              'Giusti...................:'
085200010201     O                       §CTROKVB      2   +  1
085300000616     O          E            findett     2
085400000613     O                                              'Sbagliati e corretti.....:'
085500000613     O                       §CTRMO        2   +  1
085600000616     O          E            findett     2
085700000613     O                                              'Sbagliati e scartati.....:'
085800000613     O                       §CTRNO        2   +  1
085900000512** CMD - COMANDI CL
086000050131OVRPRTF FILE(TITVH5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
086100050131OVRPRTF FILE(TITVH5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
086200050131DLTOVR FILE(TITVH5P TITVH5PS) LVL(*)
086300000512
086400000512
