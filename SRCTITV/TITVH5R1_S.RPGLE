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
006500990908
006600010201
006700010201
006800990915     C                   time                    wrkdata
006900990915     C                   time                    wrkora
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300000724     C*
007400000724     C* SE OCCORRE SPEDIRE IN FILIALE
007500000724     C                   if        vlrpoi <> *zeros
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900000724     C     vlrpoi        chain     azorg01l
008000041210     C                   if        %found(azorg01l)
008100000616     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000616     C*
008400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000616     C                   PARM                    depcmd
008800000616     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000724     C                   endif
009200000616     C*
009300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009400000616     C                   movel(p)  CMD(2)        depcmd
009500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000616     C*
010300050131     C                   if        not %open(titvh5ps)
010400050131     C                   open      titvh5ps
010500000616     C                   except    testdett
010600000613     C                   endif
010700000613     C*
010800050131     C                   EXSR      CARTAB                                       CARICA TABELLE
010900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011100000613     C*
011200010202     C* Effettuo la chiamata al CLLE preposto
011300090313     C                   call(e)   'TITVEVTC'
011400010202     C                   parm                    parccm
011500010202     C                   parm                    parmbr
011600010202     C                   parm      '2'           paropz
011700050131     C*
011800050131     C* Effettuo lancio TISI95 solo x chiusura
011900050131     C                   CLEAR                   TISI95DS
012000050131     C                   EVAL      I95TLA = 'C'
012100050131     C                   CALL      'TISI95R'
012200050131     C                   PARM                    TISI95DS
012300010202     C*
012400050131     C                   if        %open(titvh5ps)
012500000616     C                   except    findett
012600050131     C                   close     titvh5ps
012700000613     C                   endif
012800000616     C*
012900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000616     C                   PARM                    CMD(3)
013300000616     C                   PARM                    LENGH
013400000616     C*
013500010201     C                   seton                                        LR
013600000613
013700000613
013800000613     C*--------------------------------------------------------
013900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014000000613     C*--------------------------------------------------------
014100000621     C     STPR          BEGSR
014200000613     C*
014300050131     C                   if        not %open(titvh5p)
014400050131     C                   open      titvh5p
014500990915     C                   endif
014600990915     C*
014700990915     C                   except    riepilogo
014800990915     C*
014900050131     C                   if        %open(titvh5p)
015000050131     C                   close     titvh5p
015100990914     C                   endif
015200990910     C*
015300000613     C                   ENDSR
015400000613     C***
015500041210
015600041210     C*--------------------------------------------------------
015700050131     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
015800041210     C*--------------------------------------------------------
015900041210     C     CARTAB        BEGSR
016000041210     C*
016100041210     C* TABELLA '15' - NAZIONI
016200041210     C                   clear                   skNAZISO
016300041210     C                   clear                   skNAZBAR
016400041210     C                   eval      tblKUT = 1
016500041210     C                   eval      tblCOD = '15'
016600041210     C     KEYtabP       setll     tabel00f
016700041210     C     KEYtabP       reade     tabel00f
016800041210     C                   dow       not %eof(tabel00f)
016900041210     C                   if        tblFLG = *blanks
017000041210     C                   movel(p)  tblUNI        ds15
017100041210     C                   add       1             jNAZ
017200041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
017300041210     C                   eval      skNAZISO(jNAZ) = §15COD
017400041210     C                   endif
017500041210     C     KEYtabP       reade     tabel00f
017600041210     C                   enddo
017700041210     C*
017800041210     C                   ENDSR
017900041210     C***
018000041210
018100041210
018200041210
018300910830     C*--------------------------------------------------------
018400090313     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
018500910830     C*--------------------------------------------------------
018600040526     C     RWFILE        BEGSR
018700990910     C*
018800990914     C                   if        not %open(tivin00r)
018900990908     C                   open      tivin00r
019000990914     C                   endif
019100090313     C                   if        not %open(edivabwr)
019200090313     C                   open      edivabwr
019300990914     C                   endif
019400090313     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
019500020305     C                   exsr      prevat
019600010201     C*
019700010202     C                   if        chkcall = '0'
019800010202     C*
019900090313     C                   if        not %open(edivatwr)
020000090313     C                   open      edivatwr
020100010201     C                   endif
020200990910     C*
020300010201     C                   clear                   §CTROKVB          5 0
020400020305     C                   clear                   §CTROKVT          5 0
020500000801     C                   clear                   §CTRMO            5 0
020600000801     C                   clear                   §CTRNO            5 0
020700040910     C*
020800921023     C                   DO        *HIVAL
020900990913     C*
021000990915     C                   READ      tivin00r                               70
021100040910     C                   if        vindta > *blanks
021200000613     C                   add       1             rrnum
021300000801     C*
021400000801     C                   if        *in70 = *off
021500000801     C                             and
021600000801     C                             (vinflg = *blanks
021700000801     C                              or vinflg = '0'
021800000801     C                              or vinflg = '2')
021900000801     C*
022000000801     C                   clear                   vinmsg
022100000801     C                   eval      vinflg = '1'
022200040910     C*
022300040910     C* Eseguo routine d traduzione
022400050131     C                   exsr      impvabvat
022500040802     C*
022600010305     C                   endif
022700000905     C*
022800000905     C                   else
022900000905     C                   eval      vinflg = '1'
023000000905     C                   endif
023100000905     C*
023200000905     C  N70              update    tivin000
023300000905     C*
023400991022     C  N70              ENDdo
023500041210     C*
023600050131     C* Scarico i buffer eventualmente rimasti in sospeso
023700041210     C                   IF        FlgNewBol = '1'
023800090313     C                   EVAL      VABCMR = 'LEVI'
023900090313     C                   EVAL      VABDCM = datcor
024000090313     C                   EVAL      VABCNT = 1
024100090313     C                   WRITE     EDIVAB00
024200041210     C                   ENDIF
024300010202     C*
024400010202     C                   endif
024500990910
024600990910     C* Se non ci sono record con errori ...
024700000710     C                   if        §ctrno = 0
024800990910     C* ... restituisco esito OK.
024900990921     C                   eval      wrkesito = '0'
025000990910     C                   else
025100010201     C                   if        §ctrokvb > 0
025200990921     C                   eval      wrkesito = '1'
025300000710     C                   else
025400000710     C                   eval      wrkesito = '2'
025500990910     C                   endif
025600000710     C                   endif
025700990910     C*
025800990914     C                   if        %open(tivin00r)
025900990908     C                   close     tivin00r
026000990914     C                   endif
026100090313     C                   if        %open(edivabwr)
026200090313     C                   close     edivabwr
026300990914     C                   endif
026400090313     C                   if        %open(edivatwr)
026500090313     C                   close     edivatwr
026600010201     C                   endif
026700990910     C*
026800010201     C                   if        §ctrokvb > 0
026900000724     C                             and vlrpoi <> *zeros
027000010202     C                   exsr      invio
027100990920     C                   endif
027200990920     C*
027300910830     C                   ENDSR
027400000613     C***
027500990920
027600000801     C*----------------------------------------------------*
027700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027800000801     C*----------------------------------------------------*
027900010201     C     INZVAR        BEGSR
028000000801     C*
028100040802     C                   Z-ADD     *zeros        Num5_0            5 0
028200040802     C                   MOVEL     '0'           FlgCAS            1
028300000801     C*
028400000801     C                   ENDSR
028500000801     C*----------------------------------------------------*
028600040910     C*  IMPOSTAZIONE CAMPI COSTANTI
028700000801     C*----------------------------------------------------*
028800000801     C     DEFCAM        BEGSR
028900000801     C*
029000090313     C                   CLEAR                   EDIVAB00
029100090313     C                   CLEAR                   EDIVAT00
029200020619     C* Imposto i valori di default...
029300050217     C                   Z-ADD     0893062       VABCCM
029400050217     C                   Z-ADD     0893062       VATCCM
029500050217     C                   Z-ADD     089           VABLNP
029600050217     C                   Z-ADD     089           VATLNP
029700050311     C                   Z-ADD     103           VABCTR
029800040823     C                   MOVEL     '1'           VABCBO
029900050131     C                   MOVEL     '7Q'          VABCTM
030000020619     C* ... e poi verifico se sono stati passati come parametri
030100020619     C                   IF        vlrppt > *blanks
030200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABCCM
030700020619     C                   Z-ADD     PiVal         VATCCM
030800020619     C                   ENDIF
030900040506     C                   ENDIF
031000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
031100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031200020619     C                   EXSR      CHKNUM
031300020619     C                   IF        PiInt=*on
031400020619     C                   Z-ADD     PiVal         VABLNP
031500020619     C                   Z-ADD     PiVal         VATLNP
031600040506     C                   ENDIF
031700020619     C                   ENDIF
031800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032000020619     C                   EXSR      CHKNUM
032100020619     C                   IF        PiInt=*on
032200020619     C                   Z-ADD     PiVal         VABCTR
032300040506     C                   ENDIF
032400020619     C                   ENDIF
032500041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
032600041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
032700041210     C                   ENDIF
032800020619     C                   ENDIF
032900000801     C*
033000000801     C                   ENDSR
033100000801     C*----------------------------------------------------*
033200090313     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
033300000801     C*----------------------------------------------------*
033400040910     C     IMPVABVAT     BEGSR
033500041210     C*
033600041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
033700041210     C                   Z-ADD     *zeros        errore            1 0
033800050317     C********
033900050317     C* Tipo record 'C1' (dati relativi alla transazione: sender ID, etc...)
034000050317     C********
034100050317     C                   IF        %trim(%subst(vindta:1:2)) = 'C1'
034200050318     C                   Z-ADD     *zeros        wVABCCM           7 0
034300050317     C* Verifico se trattasi d traffico U.K.
034400050317     C                   IF        %trim(%subst(vindta:180:3)) = 'UK1'
034500090311     C***                EVAL      wVABCCM = 0893062
034600090610     C***                EVAL      wVABCCM = 0503436
034700090610     C                   EVAL      wVABCCM = 0504403
034800050317     C                   ENDIF
034900050317     C* Verifico se trattasi d traffico BORNHEM
035000050317     C                   IF        %trim(%subst(vindta:180:3)) = 'B01'
035100090610     C***                EVAL      wVABCCM = 0503436
035200090610     C                   EVAL      wVABCCM = 0504402
035300050317     C                   ENDIF
035400090311     C* Verifico se trattasi d traffico dalla REPUBBLICA CECA
035500090311     C                   IF        %trim(%subst(vindta:180:3)) = 'CZ1'
035600090311     C                   EVAL      wVABCCM = 1161525
035700090311     C                   ENDIF
035800050317     C                   ENDIF
035900040910     C*
036000041210     C********
036100050131     C* Tipo record 'C2' (dati testata: riferimenti, destinatario, contrassegno, etc...)
036200041210     C********
036300050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C2'
036400090313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
036500041210     C                   IF        FlgNewBol = '1'
036600090313     C                   EVAL      VABCMR = 'LEVI'
036700090313     C                   EVAL      VABDCM = datcor
036800090313     C                   EVAL      VABCNT = 1
036900090313     C                   WRITE     EDIVAB00
037000050131     C                   EVAL      FlgNewBol = '0'
037100041210     C                   ENDIF
037200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
037300040910     C                   EXSR      INZVAR
037400040910     C                   EXSR      DEFCAM
037500050318     C* ......verifico il codice cliente mittente reperito da file, se valorizzato utilizzo quello
037600050318     C                   IF        wVABCCM <> *zeros
037700050318     C                   EVAL      VABCCM = wVABCCM
037800050318     C                   EVAL      VATCCM = wVABCCM
037900050318     C                   ENDIF
038000041210     C                   EVAL      FlgNewBol = '1'
038100040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
038200040928     C                   MOVEL     datcor        VABAAS
038300040928     C                   MOVEL     datcor        VATAAS
038400040928     C                   MOVE      datcor        VABMGS
038500040928     C                   MOVE(P)   vlrpoi        VABFGS
038600040928     C                   MOVE(P)   vlrpoi        VATFGS
038700041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
038800041210     C                   CLEAR                   TRUL33DS
038900041210     C                   EVAL      I33OPE = *zeros
039000041210     C                   EVAL      I33CNU = 302
039100041210     C                   EVAL      I33NUM = 1
039200041210     C                   MOVEL     TRUL33DS      KPJBU
039300041210     C                   CALL      'TRUL33R'
039400041210     C                   PARM                    KPJBA
039500041210     C                   MOVEL     KPJBU         TRUL33DS
039600041210     C                   IF        O33ERR = *zeros
039700041210     C                   Z-ADD     O33NRF        VABNSP
039800041210     C                   Z-ADD     O33NRF        VATNSP
039900041210     C                   ELSE
040000041210     C                   ADD       1             errore
040100041210     C                   EVAL      vinmsg = %trimr(vinmsg)
040200041025     C                             + ' ' + 'VABNSP VATNSP'
040300041210     C                   ENDIF
040400041210     C* ......VABRMN
040500050131     C                   EVAL      PiStr=%trim(%subst(vindta:23:8))
040600041210     C                   EXSR      CHKNUM
040700041210     C                   IF        PiInt=*on
040800041210     C                   Z-ADD     PiVal         VABRMN
040900041210     C                   ELSE
041000041210     C                   Z-ADD     1             VABRMN
041100041210     C                   ADD       1             errore
041200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
041300041210     C                             + ' ' + 'VABRMN'
041400041210     C                   ENDIF
041500041210     C* ......VABRMA
041600090901     C***                EVAL      VABRMA=%trim(%subst(vindta:23:8))
041700050131     C* ......VABRSD
041800050131     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
041900100429     C                   IF        %subst(VABRSD:1:8) = 'COIN SPA'
042000100429     C                   EVAL      VABRSD = 'COIN SPA'
042100100429     C                   ENDIF
042200050131     C* ......VABRD2
042300050131     C                   EVAL      VABRD2=%trim(%subst(vindta:76:30))
042400050131     C* ......VABIND
042500050131     C                   EVAL      VABIND=%trim(%subst(vindta:106:30))
042600050131     C* ......VABLOD
042700050131     C                   EVAL      VABLOD=%trim(%subst(vindta:166:30))
042800050131     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
042900050131     C                   EVAL      VABNZD=%trim(%subst(vindta:206:3))
043000050131     C                   Z-ADD     1             jNAZ
043100050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
043200050131     C                   IF        %found
043300050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
043400050131     C                   ENDIF
043500050131     C* ......VABCAD
043600050131     C                   EVAL      PiStr=%trim(%subst(vindta:196:10))
043700050131     C                   EXSR      CHKNUM
043800050131     C                   IF        PiInt=*on
043900050131     C                   Z-ADD     PiVal         Num5_0
044000050131     C                   MOVEL(P)  Num5_0        VABCAD
044100050131     C                   ELSE
044200050131     C                   ADD       1             errore
044300050131     C                   EVAL      vinmsg = %trimr(vinmsg)
044400050131     C                             + ' ' + 'VABCAD'
044500050131     C                   ENDIF
044600050131     C* ......VABPRD (reperita mediante TISI95)
044700050131     C                   IF        VABCAD <> *blanks
044800050131     C                   CLEAR                   TISI95DS
044900050131     C                   EVAL      I95TCN = '3'
045000050131     C                   Z-ADD     datcor        I95DAT
045100050131     C                   EVAL      I95CAP = VABCAD
045200050131     C                   EVAL      I95LOC = VABLOD
045300050131     C                   CALL      'TISI95R'
045400050131     C                   PARM                    TISI95DS
045500050131     C                   EVAL      VABPRD = O95PRV
045600050131     C                   ENDIF
045700050131     C* ......VABNOT
045800050131     C                   EVAL      VABNOT=%trim(%subst(vindta:329:35))
045900050131     C* ......VABNT2
046000050131     C                   EVAL      VABNT2=%trim(%subst(vindta:329+35:60-35))
046100050131     C* ......VATNOT (referente x consegna)
046200050131     C                   EVAL      VATNOT=%trim(%subst(vindta:239:60))
046300050131     C                   EVAL      VATTRC = 'A'
046400050131     C                   IF        VATNOT <> *blanks
046500090318     C                   EVAL      VATCMR = 'LEVI'
046600090318     C                   EVAL      VATCNT = 1
046700090313     C                   WRITE     EDIVAT00
046800050131     C                   ENDIF
046900050131     C* ......VABCAS
047000050131     C                   IF        %subst(vindta:389:1) = 'Y'
047100050131     C                   EVAL      FlgCAS = '1'
047200050131     C                   EVAL      VABVCA=%trim(%subst(vindta:400:3))
047300050131     C                   EVAL      PiStr=%trim(%subst(vindta:390:10))
047400050131     C                   EXSR      CHKNUM
047500050131     C                   IF        PiNum=*on
047600050131     C                   Z-ADD     PiVal         VABCAS
047700050131     C                   ELSE
047800050131     C                   ADD       1             errore
047900050131     C                   EVAL      vinmsg = %trimr(vinmsg)
048000050131     C                             + ' ' + 'VABCAS'
048100050131     C                   ENDIF
048200050131     C                   ENDIF
048300050131     C*
048400050131     C                   ENDIF
048500050131     C*
048600050131     C********
048700050131     C* Tipo record 'C3' (dati dettaglio: barcode, peso, volume, colli, etc...)
048800050131     C********
048900050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C3'
049000090318     C* ......VABNCL
049100090318     C                   ADD       1             VABNCL
049200050131     C* ......VATNOT ("chi sono")
049300050131     C                   EVAL      VATNOT=%trim(%subst(vindta:51:20))
049400050131     C                   EVAL      VATTRC = 'E'
049500050131     C                   IF        VATNOT <> *blanks
049600090318     C                   EVAL      VATCMR = 'LEVI'
049700090318     C                   EVAL      VATCNT = 1
049800090313     C                   WRITE     EDIVAT00
049900050131     C                   ENDIF
050000050131     C* ......VABPKB
050100050131     C                   EVAL      PiStr=%trim(%subst(vindta:71:7))
050200041210     C                   EXSR      CHKNUM
050300041210     C                   IF        PiNum=*on
050400050131     C                   ADD(H)    PiVal         VABPKB
050500041210     C                   ELSE
050600041210     C                   ADD       1             errore
050700041210     C                   EVAL      vinmsg = %trimr(vinmsg)
050800041210     C                             + ' ' + 'VABPKB'
050900041210     C                   ENDIF
051000050131     C* ......VABVLB
051100100407     C                   SELECT
051200100407     C                   WHEN      VABCCM = 1161525
051300100407     C***                ADD(H)    0,068         VABVLB
051400100407     C                   WHEN      VABCCM = 0893089
051500050407     C* Effettuo forzatura sul volume x codice 0893089 (traffico Bornhem)
051600050407     C                   ADD(H)    0,067         VABVLB
051700100407     C                   OTHER
051800050401     C                   EVAL      PiStr=%trim(%subst(vindta:80:7))
051900050401     C                   EXSR      CHKNUM
052000050401     C                   IF        PiNum=*on
052100050401     C                   ADD(H)    PiVal         VABVLB
052200050401     C                   ELSE
052300050401     C                   ADD       1             errore
052400050401     C                   EVAL      vinmsg = %trimr(vinmsg)
052500050401     C                             + ' ' + 'VABVLB'
052600050401     C                   ENDIF
052700100407     C                   ENDSL
052800041210     C*
052900041210     C                   ENDIF
053000040910     C*
053100040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
053200040910     C                   IF        FlgCAS <> '0'
053300040929     C                   IF        VABCBO = '1'
053400040910     C                   EVAL      VABCBO = '4'
053500040910     C                   ELSE
053600040929     C                   EVAL      VABCBO = '6'
053700040910     C                   ENDIF
053800040929     C                   ENDIF
053900040910     C*
054000040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054100040910     C                   EXSR      CHKIMPDIV
054200010202     C*
054300000801     C* Ebbene...
054400000801     C                   ADD       1             §CTRMO
054500010201     C                   IF        errore <> *zeros
054600000801     C                   ADD       1             §CTRNO
054700000801     C                   EVAL      recko = vindta
054800000801     C                   EXCEPT    rigadett
054900000801     C                   EVAL      vinflg = '2'
055000000801     C                   ELSE
055100010201     C                   ADD       1             §CTROKVB
055200000801     C                   ENDIF
055300000801     C*
055400000801     C                   ENDSR
055500010202     C*----------------------------------------------------*
055600090313     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
055700010202     C*----------------------------------------------------*
055800020305     C     PREVAT        BEGSR
055900010202     C*
056000090313     C* Compongo il nome del membro da dare al EDIVATWR
056100010202     C                   eval      parmbr = vlrhdl
056200010202     C                   movel     'M'           parmbr
056300041210     C                   eval      parccm = vlrksc
056400010202     C                   eval      paropz = '1'
056500010202     C* Effettuo la chiamata al CLLE preposto
056600090313     C                   call(e)   'TITVEVTC'
056700010202     C                   parm                    parccm
056800010202     C                   parm                    parmbr
056900010202     C                   parm                    paropz
057000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057100010202     C                   if        %error
057200010202     C                   movel     '1'           chkcall
057300010202     C                   else
057400010202     C                   movel     '0'           chkcall
057500010202     C                   endif
057600010202     C*
057700010202     C                   ENDSR
057800000801     C*----------------------------------------------------*
057900000801     C*  CONTROLLO NUMERICITA' CAMPI
058000000801     C*----------------------------------------------------*
058100000801     C     CHKNUM        BEGSR
058200000801     C*
058300000801     C                   call(e)   'ISNUMERIC'
058400000801     C                   PARM                    PiStr            30
058500041210     C                   PARM      '.'           PiDecChr          1
058600000801     C                   PARM      *ZEROS        PiVal            30 9
058700000801     C                   PARM      '0'           PiInt             1
058800000801     C                   PARM      '0'           PiNum             1
058900000801     C                   IF        %error
059000000801     C                   EVAL      PiInt=*off
059100000801     C                   ENDIF
059200000801     C*
059300000801     C                   ENDSR
059400000801     C***
059500000801
059600011113
059700011113     C*----------------------------------------------------*
059800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059900011113     C*----------------------------------------------------*
060000011113     C     CHKIMPDIV     BEGSR
060100011113     C*
060200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060300011113     C                   Z-ADD     *zeros        wrkDec            9 9
060400011113     C*
060500011113     C* Come prima cosa effettuo considerazioni sulla divisa
060600011113     C                   IF        vabIAS > *zeros
060700011113     C                   IF        vabVAS <> 'EUR'
060800011113     C                   EVAL      vabVAS =  'ITL'
060900011113     C                   ENDIF
061000011113     C                   ENDIF
061100011113     C*
061200011113     C                   IF        vabCAS > *zeros
061300011113     C                   IF        vabVCA <> 'EUR'
061400011113     C                   EVAL      vabVCA =  'ITL'
061500011113     C                   ENDIF
061600011113     C                   ENDIF
061700011113     C*
061800011113     C                   IF        vabVMD > *zeros
061900020305     C                   IF        vabVAD <> 'EUR'
062000011113     C                   EVAL      vabVAD =  'ITL'
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062500011113     C                   Z-ADD     vabIAS        wrkDec
062600011113     C                   IF        wrkDec > *zeros
062700011113     C                   IF        vabVAS = 'ITL'
062800011113     C                   EVAL      vabIAS = *zeros
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063300011113     C                   Z-ADD     vabCAS        wrkDec
063400011113     C                   IF        wrkDec > *zeros
063500011113     C                   IF        vabVCA = 'ITL'
063600011113     C                   EVAL      vabCAS = *zeros
063700011113     C                   ENDIF
063800011113     C                   ENDIF
063900011113     C*
064000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064100011113     C                   Z-ADD     vabVMD        wrkDec
064200011113     C                   IF        wrkDec > *zeros
064300011113     C                   IF        vabVAD = 'ITL'
064400011113     C                   EVAL      vabVMD = *zeros
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C                   ENDSR
064900011113     C***
065000011113
065100011113
065200000801
065300000801
065400990920      /TITLE Invio dei dati al punto operativo.
065500010202     C     invio         BEGSR
065600990920     C*
065700090313     C* 1° invio EDIVAT
065800010201     C                   reset                   dscmz
065900010201     C                   move      vlrpoi        cmzdst
066000090313     C                   eval      cmzfld = 'EDIVATWR'
066100010201     C                   eval      cmzmbd = vlrhdl
066200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066300021009     C***                if        prmfir = *blanks
066400090313     C                   eval      cmzfla = 'EDIVAT0F'
066500090313     C                   eval      cmzmba = 'EDIVAT0F'
066600021009     C***                else
066700021009     C***                eval      cmzfla = prmfir
066800021009     C***                eval      cmzmba = prmfir
066900021009     C***                endif
067000010201     C                   eval      cmznrr = *zeros
067100020305     C                   move      §ctrokvt      cmznrr
067200021018     C                   eval      cmzlba = vlrfl1
067300010201     C                   call(e)   'TIS711C'
067400010201     C                   parm                    dscmz
067500010201     C                   parm      *blanks       esito
067600010205     C                   if        %error
067700010205     C                             or cmzerr = '1'
067800010205     C                             or esito  = '1'
067900010205     C                   eval      wrkesito = '3'
068000010205     C                   else
068100010201     C*
068200090313     C* 2° invio EDIVAB
068300010201     C                   reset                   dscmz
068400010201     C                   move      vlrpoi        cmzdst
068500010201     C                   eval      cmzfld = vlrfou
068600010201     C                   eval      cmzmbd = vlrhdl
068700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068800021009     C***                if        prmfir = *blanks
068900090313     C                   eval      cmzfla = 'EDIVAB0F'
069000090313     C                   eval      cmzmba = 'EDIVAB0F'
069100021009     C***                else
069200021009     C***                eval      cmzfla = prmfir
069300021009     C***                eval      cmzmba = prmfir
069400021009     C***                endif
069500010201     C                   eval      cmznrr = *zeros
069600010201     C                   move      §ctrokvb      cmznrr
069700021018     C                   eval      cmzlba = vlrfl1
069800010201     C                   call(e)   'TIS711C'
069900010201     C                   parm                    dscmz
070000010201     C                   parm      *blanks       esito
070100010201     C                   if        %error
070200010201     C                             or cmzerr = '1'
070300010201     C                             or esito  = '1'
070400010201     C                   eval      wrkesito = '3'
070500010201     C                   endif
070600010205     C                   endif
070700990920     C*
070800000613     C                   ENDSR
070900000613     C***
071000990910
071100000613     C     *inzsr        BEGSR
071200990910     C*
071300990910     C     *entry        plist
071400990920     C                   parm                    tivlrds
071500990921     C                   parm      wrkesito      esito
071600000724     C                   parm                    prmlit
071700000710     C                   parm                    prmfir
071800000613     C*
071900000830     C* CALCOLA LA DATA CORRENTE
072000000830     C                   time                    wn14             14 0
072100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072200000830     C                   z-add     wn8           g08dat
072300000830     C                   z-add     *zeros        g08inv
072400000830     C                   movel     '0'           g08err
072500000830     C                   call      'XSRDA8'
072600000830     C                   parm                    wlbda8
072700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072800041210     C*
072900041210     C* Chiave su TABEL00F - parziale
073000041210     C     KEYtabP       KLIST
073100041210     C                   KFLD                    tblKUT
073200041210     C                   KFLD                    tblCOD
073300000830     C*
073400000613     C                   ENDSR
073500000613     C***
073600990908
073700050131     Otitvh5p   E            riepilogo         2
073800990915     O                                              'Upload via Internet'
073900990915     O                                           +1 'Traduzione TIVIN00R -'
074000090313     O                                           +1 'EDIVABWR/EDIVATWR'
074100010201     O                                           +1 'Report testate bolle'
074200990915     O          E            riepilogo   2
074300990915     O                       wrkdata
074400990915     O                       wrkora              +1
074500990915     O                       procname            +1
074600990915     O          E            riepilogo   2
074700990915     O                                              'Cliente..................:'
074800990915     O                       VABCCM        z     +1
074900990915     O          E            riepilogo   2
075000990920     O                                              'Riferimento Strategi.....:'
075100990920     O                       vlrhdl              +1
075200990915     O          E            riepilogo   2
075300990915     O                                              'Giusti...................:'
075400010201     O                       §CTROKVB      2   +  1
075500990915     O          E            riepilogo   2
075600990915     O                                              'Sbagliati e corretti.....:'
075700971022     O                       §CTRMO        2   +  1
075800990915     O          E            riepilogo   2
075900990915     O                                              'Sbagliati e scartati.....:'
076000971022     O                       §CTRNO        2   +  1
076100000613
076200050131     Otitvh5ps  E            testdett          2
076300000613     O                                              'Upload via Internet'
076400000613     O                                           +1 'Traduzione TIVIN00R -'
076500090313     O                                           +1 'EDIVABWR/EDIVATWR'
076600010201     O                                           +1 'Report testate bolle'
076700000616     O          E            testdett    3
076800000613     O                                           +2 'N° rec'
076900000613     O                                           +3 'Anteprima contenuto'
077000000616     O          E            rigadett    2
077100000613     O                       rrnum               +2
077200000621     O                       recko               +3
077300000616     O          E            findett     2
077400000613     O                       wrkdata
077500000613     O                       wrkora              +1
077600000613     O                       procname            +1
077700000616     O          E            findett     2
077800000613     O                                              'Cliente..................:'
077900000613     O                       VABCCM        z     +1
078000000616     O          E            findett     2
078100000613     O                                              'Riferimento Strategi.....:'
078200000613     O                       vlrhdl              +1
078300000616     O          E            findett     2
078400000613     O                                              'Giusti...................:'
078500010201     O                       §CTROKVB      2   +  1
078600000616     O          E            findett     2
078700000613     O                                              'Sbagliati e corretti.....:'
078800000613     O                       §CTRMO        2   +  1
078900000616     O          E            findett     2
079000000613     O                                              'Sbagliati e scartati.....:'
079100000613     O                       §CTRNO        2   +  1
079200000512** CMD - COMANDI CL
079300050131OVRPRTF FILE(TITVH5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
079400050131OVRPRTF FILE(TITVH5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
079500050131DLTOVR FILE(TITVH5P TITVH5PS) LVL(*)
079600000512
079700000512
