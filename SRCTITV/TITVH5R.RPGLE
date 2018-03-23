000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500041210     Ftabel00f  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021113     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
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
011300040506     C                   call(e)   'TITVVTC'
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
018400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
018500910830     C*--------------------------------------------------------
018600040526     C     RWFILE        BEGSR
018700990910     C*
018800990914     C                   if        not %open(tivin00r)
018900990908     C                   open      tivin00r
019000990914     C                   endif
019100021113     C                   if        not %open(fivabwwr)
019200021113     C                   open      fivabwwr
019300990914     C                   endif
019400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
019500020305     C                   exsr      prevat
019600010201     C*
019700010202     C                   if        chkcall = '0'
019800010202     C*
019900021113     C                   if        not %open(fivatwwr)
020000021113     C                   open      fivatwwr
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
023800041210     C                   WRITE     FIVAB000
023900041210     C                   ENDIF
024000010202     C*
024100010202     C                   endif
024200990910
024300990910     C* Se non ci sono record con errori ...
024400000710     C                   if        §ctrno = 0
024500990910     C* ... restituisco esito OK.
024600990921     C                   eval      wrkesito = '0'
024700990910     C                   else
024800010201     C                   if        §ctrokvb > 0
024900990921     C                   eval      wrkesito = '1'
025000000710     C                   else
025100000710     C                   eval      wrkesito = '2'
025200990910     C                   endif
025300000710     C                   endif
025400990910     C*
025500990914     C                   if        %open(tivin00r)
025600990908     C                   close     tivin00r
025700990914     C                   endif
025800021113     C                   if        %open(fivabwwr)
025900021113     C                   close     fivabwwr
026000990914     C                   endif
026100021113     C                   if        %open(fivatwwr)
026200021113     C                   close     fivatwwr
026300010201     C                   endif
026400990910     C*
026500010201     C                   if        §ctrokvb > 0
026600000724     C                             and vlrpoi <> *zeros
026700010202     C                   exsr      invio
026800990920     C                   endif
026900990920     C*
027000910830     C                   ENDSR
027100000613     C***
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800040802     C                   Z-ADD     *zeros        Num5_0            5 0
027900040802     C                   MOVEL     '0'           FlgCAS            1
028000000801     C*
028100000801     C                   ENDSR
028200000801     C*----------------------------------------------------*
028300040910     C*  IMPOSTAZIONE CAMPI COSTANTI
028400000801     C*----------------------------------------------------*
028500000801     C     DEFCAM        BEGSR
028600000801     C*
028700021113     C                   CLEAR                   FIVAB000
028800040802     C                   CLEAR                   FIVAT000
028900020619     C* Imposto i valori di default...
029000050217     C                   Z-ADD     0893062       VABCCM
029100050217     C                   Z-ADD     0893062       VATCCM
029200050217     C                   Z-ADD     089           VABLNP
029300050217     C                   Z-ADD     089           VATLNP
029400050311     C                   Z-ADD     103           VABCTR
029500040823     C                   MOVEL     '1'           VABCBO
029600050131     C                   MOVEL     '7Q'          VABCTM
029700020619     C* ... e poi verifico se sono stati passati come parametri
029800020619     C                   IF        vlrppt > *blanks
029900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030100020619     C                   EXSR      CHKNUM
030200020619     C                   IF        PiInt=*on
030300020619     C                   Z-ADD     PiVal         VABCCM
030400020619     C                   Z-ADD     PiVal         VATCCM
030500020619     C                   ENDIF
030600040506     C                   ENDIF
030700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030900020619     C                   EXSR      CHKNUM
031000020619     C                   IF        PiInt=*on
031100020619     C                   Z-ADD     PiVal         VABLNP
031200020619     C                   Z-ADD     PiVal         VATLNP
031300040506     C                   ENDIF
031400020619     C                   ENDIF
031500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031700020619     C                   EXSR      CHKNUM
031800020619     C                   IF        PiInt=*on
031900020619     C                   Z-ADD     PiVal         VABCTR
032000040506     C                   ENDIF
032100020619     C                   ENDIF
032200041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
032300041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
032400041210     C                   ENDIF
032500020619     C                   ENDIF
032600000801     C*
032700000801     C                   ENDSR
032800000801     C*----------------------------------------------------*
032900040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
033000000801     C*----------------------------------------------------*
033100040910     C     IMPVABVAT     BEGSR
033200041210     C*
033300041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
033400041210     C                   Z-ADD     *zeros        errore            1 0
033500050317     C********
033600050317     C* Tipo record 'C1' (dati relativi alla transazione: sender ID, etc...)
033700050317     C********
033800050317     C                   IF        %trim(%subst(vindta:1:2)) = 'C1'
033900050318     C                   Z-ADD     *zeros        wVABCCM           7 0
034000050317     C* Verifico se trattasi d traffico U.K.
034100050317     C                   IF        %trim(%subst(vindta:180:3)) = 'UK1'
034200090311     C***                EVAL      wVABCCM = 0893062
034300090311     C                   EVAL      wVABCCM = 0503436
034400050317     C                   ENDIF
034500050317     C* Verifico se trattasi d traffico BORNHEM
034600050317     C                   IF        %trim(%subst(vindta:180:3)) = 'B01'
034700070509     C                   EVAL      wVABCCM = 0503436
034800050317     C                   ENDIF
034900090311     C* Verifico se trattasi d traffico dalla REPUBBLICA CECA
035000090311     C                   IF        %trim(%subst(vindta:180:3)) = 'CZ1'
035100090311     C                   EVAL      wVABCCM = 1161525
035200090311     C                   ENDIF
035300050317     C                   ENDIF
035400040910     C*
035500041210     C********
035600050131     C* Tipo record 'C2' (dati testata: riferimenti, destinatario, contrassegno, etc...)
035700041210     C********
035800050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C2'
035900041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
036000041210     C                   IF        FlgNewBol = '1'
036100041210     C                   WRITE     FIVAB000
036200050131     C                   EVAL      FlgNewBol = '0'
036300041210     C                   ENDIF
036400041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
036500040910     C                   EXSR      INZVAR
036600040910     C                   EXSR      DEFCAM
036700050318     C* ......verifico il codice cliente mittente reperito da file, se valorizzato utilizzo quello
036800050318     C                   IF        wVABCCM <> *zeros
036900050318     C                   EVAL      VABCCM = wVABCCM
037000050318     C                   EVAL      VATCCM = wVABCCM
037100050318     C                   ENDIF
037200041210     C                   EVAL      FlgNewBol = '1'
037300040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
037400040928     C                   MOVEL     datcor        VABAAS
037500040928     C                   MOVEL     datcor        VATAAS
037600040928     C                   MOVE      datcor        VABMGS
037700040928     C                   MOVE(P)   vlrpoi        VABFGS
037800040928     C                   MOVE(P)   vlrpoi        VATFGS
037900041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
038000041210     C                   CLEAR                   TRUL33DS
038100041210     C                   EVAL      I33OPE = *zeros
038200041210     C                   EVAL      I33CNU = 302
038300041210     C                   EVAL      I33NUM = 1
038400041210     C                   MOVEL     TRUL33DS      KPJBU
038500041210     C                   CALL      'TRUL33R'
038600041210     C                   PARM                    KPJBA
038700041210     C                   MOVEL     KPJBU         TRUL33DS
038800041210     C                   IF        O33ERR = *zeros
038900041210     C                   Z-ADD     O33NRF        VABNSP
039000041210     C                   Z-ADD     O33NRF        VATNSP
039100041210     C                   ELSE
039200041210     C                   ADD       1             errore
039300041210     C                   EVAL      vinmsg = %trimr(vinmsg)
039400041025     C                             + ' ' + 'VABNSP VATNSP'
039500041210     C                   ENDIF
039600041210     C* ......VABRMN
039700050131     C                   EVAL      PiStr=%trim(%subst(vindta:23:8))
039800041210     C                   EXSR      CHKNUM
039900041210     C                   IF        PiInt=*on
040000041210     C                   Z-ADD     PiVal         VABRMN
040100041210     C                   ELSE
040200041210     C                   Z-ADD     1             VABRMN
040300041210     C                   ADD       1             errore
040400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
040500041210     C                             + ' ' + 'VABRMN'
040600041210     C                   ENDIF
040700041210     C* ......VABRMA
040800050131     C                   EVAL      VABRMA=%trim(%subst(vindta:23:8))
040900050131     C* ......VABRSD
041000050131     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
041100050131     C* ......VABRD2
041200050131     C                   EVAL      VABRD2=%trim(%subst(vindta:76:30))
041300050131     C* ......VABIND
041400050131     C                   EVAL      VABIND=%trim(%subst(vindta:106:30))
041500050131     C* ......VABLOD
041600050131     C                   EVAL      VABLOD=%trim(%subst(vindta:166:30))
041700050131     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
041800050131     C                   EVAL      VABNZD=%trim(%subst(vindta:206:3))
041900050131     C                   Z-ADD     1             jNAZ
042000050131     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
042100050131     C                   IF        %found
042200050131     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
042300050131     C                   ENDIF
042400050131     C* ......VABCAD
042500050131     C                   EVAL      PiStr=%trim(%subst(vindta:196:10))
042600050131     C                   EXSR      CHKNUM
042700050131     C                   IF        PiInt=*on
042800050131     C                   Z-ADD     PiVal         Num5_0
042900050131     C                   MOVEL(P)  Num5_0        VABCAD
043000050131     C                   ELSE
043100050131     C                   ADD       1             errore
043200050131     C                   EVAL      vinmsg = %trimr(vinmsg)
043300050131     C                             + ' ' + 'VABCAD'
043400050131     C                   ENDIF
043500050131     C* ......VABPRD (reperita mediante TISI95)
043600050131     C                   IF        VABCAD <> *blanks
043700050131     C                   CLEAR                   TISI95DS
043800050131     C                   EVAL      I95TCN = '3'
043900050131     C                   Z-ADD     datcor        I95DAT
044000050131     C                   EVAL      I95CAP = VABCAD
044100050131     C                   EVAL      I95LOC = VABLOD
044200050131     C                   CALL      'TISI95R'
044300050131     C                   PARM                    TISI95DS
044400050131     C                   EVAL      VABPRD = O95PRV
044500050131     C                   ENDIF
044600050131     C* ......VABNOT
044700050131     C                   EVAL      VABNOT=%trim(%subst(vindta:329:35))
044800050131     C* ......VABNT2
044900050131     C                   EVAL      VABNT2=%trim(%subst(vindta:329+35:60-35))
045000050131     C* ......VATNOT (referente x consegna)
045100050131     C                   EVAL      VATNOT=%trim(%subst(vindta:239:60))
045200050131     C                   EVAL      VATTRC = 'A'
045300050131     C                   IF        VATNOT <> *blanks
045400050131     C                   WRITE     FIVAT000
045500050131     C                   ENDIF
045600050131     C* ......VABCAS
045700050131     C                   IF        %subst(vindta:389:1) = 'Y'
045800050131     C                   EVAL      FlgCAS = '1'
045900050131     C                   EVAL      VABVCA=%trim(%subst(vindta:400:3))
046000050131     C                   EVAL      PiStr=%trim(%subst(vindta:390:10))
046100050131     C                   EXSR      CHKNUM
046200050131     C                   IF        PiNum=*on
046300050131     C                   Z-ADD     PiVal         VABCAS
046400050131     C                   ELSE
046500050131     C                   ADD       1             errore
046600050131     C                   EVAL      vinmsg = %trimr(vinmsg)
046700050131     C                             + ' ' + 'VABCAS'
046800050131     C                   ENDIF
046900050131     C                   ENDIF
047000050131     C*
047100050131     C                   ENDIF
047200050131     C*
047300050131     C********
047400050131     C* Tipo record 'C3' (dati dettaglio: barcode, peso, volume, colli, etc...)
047500050131     C********
047600050131     C                   IF        %trim(%subst(vindta:1:2)) = 'C3'
047700050131     C* ......VATNOT ("chi sono")
047800050131     C                   EVAL      VATNOT=%trim(%subst(vindta:51:20))
047900050131     C                   EVAL      VATTRC = 'E'
048000050131     C                   IF        VATNOT <> *blanks
048100050131     C                   WRITE     FIVAT000
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
049300050131     C* ......VABVLB
049400050407     C                   IF        VABCCM = 0893089
049500050407     C* Effettuo forzatura sul volume x codice 0893089 (traffico Bornhem)
049600050407     C                   ADD(H)    0,067         VABVLB
049700050407     C                   ELSE
049800050401     C                   EVAL      PiStr=%trim(%subst(vindta:80:7))
049900050401     C                   EXSR      CHKNUM
050000050401     C                   IF        PiNum=*on
050100050401     C                   ADD(H)    PiVal         VABVLB
050200050401     C                   ELSE
050300050401     C                   ADD       1             errore
050400050401     C                   EVAL      vinmsg = %trimr(vinmsg)
050500050401     C                             + ' ' + 'VABVLB'
050600050401     C                   ENDIF
050700050401     C                   ENDIF
050800050131     C* ......VABNCL
050900050131     C                   ADD       1             VABNCL
051000041210     C*
051100041210     C                   ENDIF
051200040910     C*
051300040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051400040910     C                   IF        FlgCAS <> '0'
051500040929     C                   IF        VABCBO = '1'
051600040910     C                   EVAL      VABCBO = '4'
051700040910     C                   ELSE
051800040929     C                   EVAL      VABCBO = '6'
051900040910     C                   ENDIF
052000040929     C                   ENDIF
052100040910     C*
052200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052300040910     C                   EXSR      CHKIMPDIV
052400010202     C*
052500000801     C* Ebbene...
052600000801     C                   ADD       1             §CTRMO
052700010201     C                   IF        errore <> *zeros
052800000801     C                   ADD       1             §CTRNO
052900000801     C                   EVAL      recko = vindta
053000000801     C                   EXCEPT    rigadett
053100000801     C                   EVAL      vinflg = '2'
053200000801     C                   ELSE
053300010201     C                   ADD       1             §CTROKVB
053400000801     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700010202     C*----------------------------------------------------*
053800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053900010202     C*----------------------------------------------------*
054000020305     C     PREVAT        BEGSR
054100010202     C*
054200021113     C* Compongo il nome del membro da dare al FIVATWWR
054300010202     C                   eval      parmbr = vlrhdl
054400010202     C                   movel     'M'           parmbr
054500041210     C                   eval      parccm = vlrksc
054600010202     C                   eval      paropz = '1'
054700010202     C* Effettuo la chiamata al CLLE preposto
054800040506     C                   call(e)   'TITVVTC'
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
063900021113     C* 1° invio FIVAT
064000010201     C                   reset                   dscmz
064100010201     C                   move      vlrpoi        cmzdst
064200021113     C                   eval      cmzfld = 'FIVATWWR'
064300010201     C                   eval      cmzmbd = vlrhdl
064400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064500021009     C***                if        prmfir = *blanks
064600021113     C                   eval      cmzfla = 'FIVAT00F'
064700021113     C                   eval      cmzmba = 'FIVAT00F'
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
066400021113     C* 2° invio FIVAB
066500010201     C                   reset                   dscmz
066600010201     C                   move      vlrpoi        cmzdst
066700010201     C                   eval      cmzfld = vlrfou
066800010201     C                   eval      cmzmbd = vlrhdl
066900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067000021009     C***                if        prmfir = *blanks
067100021113     C                   eval      cmzfla = 'FIVAB00F'
067200021113     C                   eval      cmzmba = 'FIVAB00F'
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
071800990908
071900050131     Otitvh5p   E            riepilogo         2
072000990915     O                                              'Upload via Internet'
072100990915     O                                           +1 'Traduzione TIVIN00R -'
072200021113     O                                           +1 'FIVABWWR/FIVATWWR'
072300010201     O                                           +1 'Report testate bolle'
072400990915     O          E            riepilogo   2
072500990915     O                       wrkdata
072600990915     O                       wrkora              +1
072700990915     O                       procname            +1
072800990915     O          E            riepilogo   2
072900990915     O                                              'Cliente..................:'
073000990915     O                       VABCCM        z     +1
073100990915     O          E            riepilogo   2
073200990920     O                                              'Riferimento Strategi.....:'
073300990920     O                       vlrhdl              +1
073400990915     O          E            riepilogo   2
073500990915     O                                              'Giusti...................:'
073600010201     O                       §CTROKVB      2   +  1
073700990915     O          E            riepilogo   2
073800990915     O                                              'Sbagliati e corretti.....:'
073900971022     O                       §CTRMO        2   +  1
074000990915     O          E            riepilogo   2
074100990915     O                                              'Sbagliati e scartati.....:'
074200971022     O                       §CTRNO        2   +  1
074300000613
074400050131     Otitvh5ps  E            testdett          2
074500000613     O                                              'Upload via Internet'
074600000613     O                                           +1 'Traduzione TIVIN00R -'
074700021113     O                                           +1 'FIVABWWR/FIVATWWR'
074800010201     O                                           +1 'Report testate bolle'
074900000616     O          E            testdett    3
075000000613     O                                           +2 'N° rec'
075100000613     O                                           +3 'Anteprima contenuto'
075200000616     O          E            rigadett    2
075300000613     O                       rrnum               +2
075400000621     O                       recko               +3
075500000616     O          E            findett     2
075600000613     O                       wrkdata
075700000613     O                       wrkora              +1
075800000613     O                       procname            +1
075900000616     O          E            findett     2
076000000613     O                                              'Cliente..................:'
076100000613     O                       VABCCM        z     +1
076200000616     O          E            findett     2
076300000613     O                                              'Riferimento Strategi.....:'
076400000613     O                       vlrhdl              +1
076500000616     O          E            findett     2
076600000613     O                                              'Giusti...................:'
076700010201     O                       §CTROKVB      2   +  1
076800000616     O          E            findett     2
076900000613     O                                              'Sbagliati e corretti.....:'
077000000613     O                       §CTRMO        2   +  1
077100000616     O          E            findett     2
077200000613     O                                              'Sbagliati e scartati.....:'
077300000613     O                       §CTRNO        2   +  1
077400000512** CMD - COMANDI CL
077500050131OVRPRTF FILE(TITVH5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
077600050131OVRPRTF FILE(TITVH5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077700050131DLTOVR FILE(TITVH5P TITVH5PS) LVL(*)
077800000512
077900000512
