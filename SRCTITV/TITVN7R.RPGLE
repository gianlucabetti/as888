000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500041210     Ftabel00f  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021113     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
000900050805     Ftitvn7p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200050805     Ftitvn7ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070411     F                                     infsr(*pssr)
001500990908
001600000512     D*------------
001700000512     D* COMANDI
001800000512     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000041210     D*------------
002100041210     D* SCHIERE
002200041210     D*------------
002300050805 xxx D skNAZISO        S              3    DIM(1000)
002400041210     D skNAZBAR        S              3    DIM(1000)
002500000801     D*----------------------------------------------------
002600000801     D* DICHIARAZIOINE VARIABILI DI WRK
002700000801     D*----------------------------------------------------
002800990920     D dscmz         e ds                  inz
002900990910     D psds           sds
003000990910     D  procname         *PROC
003100990920     D tivlrds       e ds                  extname(tivlr00f)
003200050805 xxx D ds15          e ds
003300990910     D esito           s              1
003400000724     D prmlit          s             10
003500000710     D prmfir          s             10
003600990921     D wrkesito        s                   like(esito)
003700990915     D wrkdata         s               d
003800990915     D wrkora          s               t
003900000613     D rrnum           s              6  0 INZ(*zeros)
004000000621     D recko           s            150    INZ(*blanks)
004100011113     D depcmd          s            150    INZ(*blanks)
004200041210     D parccm          s              8    INZ(*blanks)
004300010202     D parmbr          s             10    INZ(*blanks)
004400010202     D paropz          s              1    INZ(*blanks)
004500010202     D chkcall         s              1    INZ(*blanks)
004600050805     D FlgNewBol       s              1    INZ(*blanks)
004700050805 xxx D jNAZ            s              5  0 INZ(*zeros)
004800000830
004900000830     D*------------------
005000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100000830     D*------------------
005200000830     D WLBDA8          DS                  INZ
005300000830     D  G08DAT                 1      8  0
005400000830     D  G08INV                 9     16  0
005500000830     D  G08ERR                17     17
005600000830     D  G08TGI                18     22  0
005700041025     D*------------------
005800041025     D* DS REPERIMENTO NUMERATORE
005900041025     D*------------------
006000050805     D trul33ds      e ds                  inz
006100041025     D*------------------
006200041025     D* DS ARCHITETTURA
006300041025     D*------------------
006400041025     D kpjba         e ds                  inz
006500041025     D*------------------
006600990908
006700010201
006800010201
006900990915     C                   time                    wrkdata
007000990915     C                   time                    wrkora
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400000724     C*
007500000724     C* SE OCCORRE SPEDIRE IN FILIALE
007600000724     C                   if        vlrpoi <> *zeros
007700000724     C*
007800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008000000724     C     vlrpoi        chain     azorg01l
008100041210     C                   if        %found(azorg01l)
008200000616     C                   movel(p)  CMD(1)        depcmd
008300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008600011113     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000000724     C*
009100000724     C                   endif
009200000724     C                   endif
009300000616     C*
009400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500000616     C                   movel(p)  CMD(2)        depcmd
009600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009900011113     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000616     C*
010400050805     C                   if        not %open(titvn7ps)
010500050805     C                   open      titvn7ps
010600000616     C                   except    testdett
010700000613     C                   endif
010800000613     C*
010900041210     C                   EXSR      CARTAB                                       CARICA TABELLE
011000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011200000613     C*
011300010202     C* Effettuo la chiamata al CLLE preposto
011400040506     C                   call(e)   'TITVVTC'
011500010202     C                   parm                    parccm
011600010202     C                   parm                    parmbr
011700010202     C                   parm      '2'           paropz
011800010202     C*
011900050805     C                   if        %open(titvn7ps)
012000000616     C                   except    findett
012100050805     C                   close     titvn7ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010201     C                   seton                                        LR
013100000613
013200000613
013300000613     C*--------------------------------------------------------
013400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013500000613     C*--------------------------------------------------------
013600000621     C     STPR          BEGSR
013700000613     C*
013800050805     C                   if        not %open(titvn7p)
013900050805     C                   open      titvn7p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400050805     C                   if        %open(titvn7p)
014500050805     C                   close     titvn7p
014600990914     C                   endif
014700990910     C*
014800000613     C                   ENDSR
014900000613     C***
015000041210
015100041210     C*--------------------------------------------------------
015200041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
015300041210     C*--------------------------------------------------------
015400050805 xxx C     CARTAB        BEGSR
015500041210     C*
015600041210     C* TABELLA '15' - NAZIONI
015700041210     C                   clear                   skNAZISO
015800041210     C                   clear                   skNAZBAR
015900041210     C                   eval      tblKUT = 1
016000041210     C                   eval      tblCOD = '15'
016100041210     C     KEYtabP       setll     tabel00f
016200041210     C     KEYtabP       reade     tabel00f
016300041210     C                   dow       not %eof(tabel00f)
016400041210     C                   if        tblFLG = *blanks
016500041210     C                   movel(p)  tblUNI        ds15
016600041210     C                   add       1             jNAZ
016700041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
016800041210     C                   eval      skNAZISO(jNAZ) = §15COD
016900041210     C                   endif
017000041210     C     KEYtabP       reade     tabel00f
017100041210     C                   enddo
017200041210     C*
017300041210     C                   ENDSR
017400041210     C***
017500041210
017600041210
017700041210
017800910830     C*--------------------------------------------------------
017900040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
018000910830     C*--------------------------------------------------------
018100040526     C     RWFILE        BEGSR
018200990910     C*
018300990914     C                   if        not %open(tivin00r)
018400990908     C                   open      tivin00r
018500990914     C                   endif
018600021113     C                   if        not %open(fivabwwr)
018700021113     C                   open      fivabwwr
018800990914     C                   endif
018900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
019000020305     C                   exsr      prevat
019100010201     C*
019200010202     C                   if        chkcall = '0'
019300010202     C*
019400021113     C                   if        not %open(fivatwwr)
019500021113     C                   open      fivatwwr
019600010201     C                   endif
019700990910     C*
019800010201     C                   clear                   §CTROKVB          5 0
019900020305     C                   clear                   §CTROKVT          5 0
020000000801     C                   clear                   §CTRMO            5 0
020100000801     C                   clear                   §CTRNO            5 0
020200040910     C*
020300921023     C                   DO        *HIVAL
020400990913     C*
020500990915     C                   READ      tivin00r                               70
020600040910     C                   if        vindta > *blanks
020700000613     C                   add       1             rrnum
020800000801     C*
020900000801     C                   if        *in70 = *off
021000000801     C                             and
021100000801     C                             (vinflg = *blanks
021200000801     C                              or vinflg = '0'
021300000801     C                              or vinflg = '2')
021400000801     C*
021500000801     C                   clear                   vinmsg
021600000801     C                   eval      vinflg = '1'
021700040910     C*
021800040910     C* Eseguo routine d traduzione
021900040910     C                   exsr      impvabvat
022000040802     C*
022100010305     C                   endif
022200000905     C*
022300000905     C                   else
022400000905     C                   eval      vinflg = '1'
022500000905     C                   endif
022600000905     C*
022700000905     C  N70              update    tivin000
022800000905     C*
022900991022     C  N70              ENDdo
023000041210     C*
023100050805     C* Scarico i buffer eventualmente rimasti in sospeso
023200041210     C                   IF        FlgNewBol = '1'
023300041210     C                   WRITE     FIVAB000
023400041210     C                   WRITE     FIVAT000
023500041210     C                   ENDIF
023600010202     C*
023700010202     C                   endif
023800990910
023900990910     C* Se non ci sono record con errori ...
024000000710     C                   if        §ctrno = 0
024100990910     C* ... restituisco esito OK.
024200990921     C                   eval      wrkesito = '0'
024300990910     C                   else
024400010201     C                   if        §ctrokvb > 0
024500990921     C                   eval      wrkesito = '1'
024600000710     C                   else
024700000710     C                   eval      wrkesito = '2'
024800990910     C                   endif
024900000710     C                   endif
025000990910     C*
025100990914     C                   if        %open(tivin00r)
025200990908     C                   close     tivin00r
025300990914     C                   endif
025400021113     C                   if        %open(fivabwwr)
025500021113     C                   close     fivabwwr
025600990914     C                   endif
025700021113     C                   if        %open(fivatwwr)
025800021113     C                   close     fivatwwr
025900010201     C                   endif
026000990910     C*
026100010201     C                   if        §ctrokvb > 0
026200000724     C                             and vlrpoi <> *zeros
026300010202     C                   exsr      invio
026400990920     C                   endif
026500990920     C*
026600910830     C                   ENDSR
026700000613     C***
026800990920
026900000801     C*----------------------------------------------------*
027000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100000801     C*----------------------------------------------------*
027200010201     C     INZVAR        BEGSR
027300000801     C*
027400040802     C                   Z-ADD     *zeros        Num5_0            5 0
027500040802     C                   MOVEL     '0'           FlgCAS            1
027600000801     C*
027700000801     C                   ENDSR
027800000801     C*----------------------------------------------------*
027900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
028000000801     C*----------------------------------------------------*
028100000801     C     DEFCAM        BEGSR
028200000801     C*
028300021113     C                   CLEAR                   FIVAB000
028400040802     C                   CLEAR                   FIVAT000
028500020619     C* Imposto i valori di default...
028600050805 xxx C                   Z-ADD     0431893       VABCCM
028700050805 xxx C                   Z-ADD     0431893       VATCCM
028800041210     C                   Z-ADD     043           VABLNP
028900041210     C                   Z-ADD     043           VATLNP
029000050805 xxx C                   Z-ADD     000           VABCTR
029100050805 xxx C                   MOVEL     '1'           VABCBO
029200020619     C* ... e poi verifico se sono stati passati come parametri
029300020619     C                   IF        vlrppt > *blanks
029400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCCM
029900020619     C                   Z-ADD     PiVal         VATCCM
030000020619     C                   ENDIF
030100040506     C                   ENDIF
030200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400020619     C                   EXSR      CHKNUM
030500020619     C                   IF        PiInt=*on
030600020619     C                   Z-ADD     PiVal         VABLNP
030700020619     C                   Z-ADD     PiVal         VATLNP
030800040506     C                   ENDIF
030900020619     C                   ENDIF
031000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200020619     C                   EXSR      CHKNUM
031300020619     C                   IF        PiInt=*on
031400020619     C                   Z-ADD     PiVal         VABCTR
031500040506     C                   ENDIF
031600020619     C                   ENDIF
031700041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
031800041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
031900041210     C                   ENDIF
032000020619     C                   ENDIF
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
032500000801     C*----------------------------------------------------*
032600040910     C     IMPVABVAT     BEGSR
032700041210     C*
032800041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
032900041210     C                   Z-ADD     *zeros        errore            1 0
033000040910     C*
033100041210     C********
033200050805     C* Tipo record 'KSTA_' (dati destinatario, importi e particolarità consegna)
033300041210     C********
033400050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KSTA_'
033500041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
033600041210     C                   IF        FlgNewBol = '1'
033700041210     C                   WRITE     FIVAB000
033800041210     C                   WRITE     FIVAT000
033900041210     C                   EVAL      FlgNewBol = '0'
034000041210     C                   ENDIF
034100041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
034200040910     C                   EXSR      INZVAR
034300040910     C                   EXSR      DEFCAM
034400041210     C                   EVAL      FlgNewBol = '1'
034500040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
034600040928     C                   MOVEL     datcor        VABAAS
034700040928     C                   MOVEL     datcor        VATAAS
034800040928     C                   MOVE      datcor        VABMGS
034900040928     C                   MOVE(P)   vlrpoi        VABFGS
035000040928     C                   MOVE(P)   vlrpoi        VATFGS
035100041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
035200041210     C                   CLEAR                   TRUL33DS
035300041210     C                   EVAL      I33OPE = *zeros
035400041210     C                   EVAL      I33CNU = 302
035500041210     C                   EVAL      I33NUM = 1
035600041210     C                   MOVEL     TRUL33DS      KPJBU
035700041210     C                   CALL      'TRUL33R'
035800041210     C                   PARM                    KPJBA
035900041210     C                   MOVEL     KPJBU         TRUL33DS
036000041210     C                   IF        O33ERR = *zeros
036100041210     C                   Z-ADD     O33NRF        VABNSP
036200041210     C                   Z-ADD     O33NRF        VATNSP
036300041210     C                   ELSE
036400041210     C                   ADD       1             errore
036500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
036600041025     C                             + ' ' + 'VABNSP VATNSP'
036700041210     C                   ENDIF
036800050805     C* ......VABRMA
036900050805     C                   EVAL      VABRMA=%trim(%subst(vindta:44:17))
037000050805     C* ......VABRSD/VABRD2
037100050805     C                   MOVEL     *blanks       wRAGSOCDEST      70
037200050805     C                   EVAL      wRAGSOCDEST=%trim(%subst(vindta:61:30))+' '+
037300050805     C                                         %trim(%subst(vindta:91:30))+' '+
037400050805     C                                         %trim(%subst(vindta:121:30))
037500050805     C                   EVAL      VABRSD=%trim(%subst(wRAGSOCDEST:1:35))
037600050805     C                   EVAL      VABRD2=%trim(%subst(wRAGSOCDEST:36:35))
037700050805     C* ......VABIND
037800050805     C                   EVAL      VABRMA=%trim(%subst(vindta:151:30))
037900050805     C* ......VABNZD
038000050805     C                   EVAL      VABRMA=%trim(%subst(vindta:181:3))
038100050805     C* ......VABLOD
038200050805     C                   EVAL      VABRMA=%trim(%subst(vindta:190:26))
038300050805     C* ......VABCAD
038400050805     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
038500050805     C                   EXSR      CHKNUM
038600050805     C                   IF        PiInt=*on
038700050805     C                   Z-ADD     PiVal         Num5_0
038800050805     C                   MOVEL(P)  Num5_0        VABCAD
038900050805     C                   ELSE
039000050805     C                   ADD       1             errore
039100050805     C                   EVAL      vinmsg = %trimr(vinmsg)
039200050805     C                             + ' ' + 'VABCAD'
039300050805     C                   ENDIF
039400050805     C* ......VABIAS
039500050805     C                   EVAL      PiStr=%trim(%subst(vindta:334:9))
039600050805     C                   EXSR      CHKNUM
039700050805     C                   IF        PiNum=*on
039800050805     C                   Z-ADD     PiVal         VABIAS
039900050805     C                   ELSE
040000050805     C                   ADD       1             errore
040100050805     C                   EVAL      vinmsg = %trimr(vinmsg)
040200050805     C                             + ' ' + 'VABIAS'
040300050805     C                   ENDIF
040400050805     C                   IF        VABIAS > *zeros
040500050805     C                   IF        %trim(%subst(vindta:343:3)) = '001'
040600050805     C                   EVAL      VABVAS = 'CHF'
040700050805     C                   ENDIF
040800050805     C                   IF        %trim(%subst(vindta:343:3)) = '002'
040900050805     C                   EVAL      VABVAS = 'USD'
041000050805     C                   ENDIF
041100050805     C                   IF        %trim(%subst(vindta:343:3)) = '003'
041200050805     C                   EVAL      VABVAS = 'GBP'
041300050805     C                   ENDIF
041400050805     C                   IF        %trim(%subst(vindta:343:3)) = '006'
041500050805     C                   EVAL      VABVAS = 'NOK'
041600050805     C                   ENDIF
041700050805     C                   IF        %trim(%subst(vindta:343:3)) = '100'
041800050805     C                   EVAL      VABVAS = 'EUR'
041900050805     C                   ENDIF
042000050805     C                   IF        %trim(%subst(vindta:343:3)) = '114'
042100050805     C                   EVAL      VABVAS = '***'                               Fiorino Ungherese
042200050805     C                   ENDIF
042300050805     C                   ENDIF
042400050805     C* ......VABCAS
042500050805     C                   EVAL      PiStr=%trim(%subst(vindta:368:9))
042600050805     C                   EXSR      CHKNUM
042700050805     C                   IF        PiNum=*on
042800050805     C                   Z-ADD     PiVal         VABCAS
042900050805     C                   MOVEL     '1'           FlgCAS
043000050805     C                   ELSE
043100050805     C                   ADD       1             errore
043200050805     C                   EVAL      vinmsg = %trimr(vinmsg)
043300050805     C                             + ' ' + 'VABCAS'
043400050805     C                   ENDIF
043500050805     C                   IF        VABCAS > *zeros
043600050805     C                   IF        %trim(%subst(vindta:343:3)) = '001'
043700050805     C                   EVAL      VABVCA = 'CHF'
043800050805     C                   ENDIF
043900050805     C                   IF        %trim(%subst(vindta:343:3)) = '002'
044000050805     C                   EVAL      VABVCA = 'USD'
044100050805     C                   ENDIF
044200050805     C                   IF        %trim(%subst(vindta:343:3)) = '003'
044300050805     C                   EVAL      VABVCA = 'GBP'
044400050805     C                   ENDIF
044500050805     C                   IF        %trim(%subst(vindta:343:3)) = '006'
044600050805     C                   EVAL      VABVCA = 'NOK'
044700050805     C                   ENDIF
044800050805     C                   IF        %trim(%subst(vindta:343:3)) = '100'
044900050805     C                   EVAL      VABVCA = 'EUR'
045000050805     C                   ENDIF
045100050805     C                   IF        %trim(%subst(vindta:343:3)) = '114'
045200050805     C                   EVAL      VABVCA = '***'                               Fiorino Ungherese
045300050805     C                   ENDIF
045400050805     C                   ENDIF
045500050805     C* ......VABDCR
045600050805     C                   IF        %trim(%subst(vindta:328:1)) = 'Q'
045700050805     C                   MOVEL     *blanks       wDCR              8
045800050805     C                   EVAL      wDCR=%subst(vindta:398:4)+
045900050805     C                                  %subst(vindta:396:2)+
046000050805     C                                  %subst(vindta:394:2)
046100050805     C                   EVAL      PiStr=wDCR
046200050805     C                   EXSR      CHKNUM
046300050805     C                   IF        PiInt=*on
046400050805     C                   MOVEL     wDCR          VABDCR
046500050805     C                   ELSE
046600050805     C                   ADD       1             errore
046700050805     C                   EVAL      vinmsg = %trimr(vinmsg)
046800050805     C                             + ' ' + 'VABDCR'
046900050805     C                   ENDIF
047000050805     C*
047100050805     C                   ENDIF
047200050805     C*
047300050805     C********
047400050805     C* Tipo record 'KZEI_' (colli, peso e natura merce)
047500050805     C********
047600050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KZEI_'
047700050805     C* ......VABNCL
047800050805     C                   EVAL      PiStr=%trim(%subst(vindta:37:5))
047900050805     C                   EXSR      CHKNUM
048000050805     C                   IF        PiInt=*on
048100050805     C                   Z-ADD     PiVal         VABNCL
048200050805     C                   ELSE
048300050805     C                   ADD       1             errore
048400050805     C                   EVAL      vinmsg = %trimr(vinmsg)
048500050805     C                             + ' ' + 'VABNCL'
048600050805     C                   ENDIF
048700050805     C* ......VABNAS
048800050805     C                   EVAL      VABNAS=%trim(%subst(vindta:45:20))
048900050805     C* ......VABPKB
049000050805     C                   EVAL      PiStr=%trim(%subst(vindta:65:5))
049100050805     C                   EXSR      CHKNUM
049200050805     C                   IF        PiNum=*on
049300050805     C                   Z-ADD     PiVal         VABPKB
049400050805     C                   ELSE
049500050805     C                   ADD       1             errore
049600050805     C                   EVAL      vinmsg = %trimr(vinmsg)
049700050805     C                             + ' ' + 'VABPKB'
049800050805     C                   ENDIF
049900050805     C*
050000050805     C                   ENDIF
050100050805     C*
050200050805     C********
050300050805     C* Tipo record 'KPPA_' (barcode)
050400050805     C********
050500050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KPPA_'
050600050805     C* ......VATNOT/VATTRC
050700050805     C                   EVAL      VATTRC='E'
050800050805     C                   EVAL      VATNOT=%trim(%subst(vindta:39:35))
050900050805     C*
051000050805     C                   ENDIF
051100050805     C*
051200050805     C********
051300050805     C* Tipo record 'KUTX_' (riferimento mittente e note)
051400050805     C********
051500050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KUTX_'
051600041210     C* ......VABRMN
051700050805     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
051800041210     C                   EXSR      CHKNUM
051900041210     C                   IF        PiInt=*on
052000041210     C                   Z-ADD     PiVal         VABRMN
052100041210     C                   ELSE
052200041210     C                   Z-ADD     1             VABRMN
052300041210     C                   ADD       1             errore
052400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
052500041210     C                             + ' ' + 'VABRMN'
052600041210     C                   ENDIF
052700050805     C* ......VABNOT/VABNT2
052800050805     C                   EVAL      VABNOT=%trim(%subst(vindta:39:35))
052900050805     C                   EVAL      VABNT2=%trim(%subst(vindta:39+35:60-35))
053000041210     C*
053100041210     C                   ENDIF
053200040910     C*
053300040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
053400040910     C                   IF        FlgCAS <> '0'
053500040929     C                   IF        VABCBO = '1'
053600040910     C                   EVAL      VABCBO = '4'
053700040910     C                   ELSE
053800040929     C                   EVAL      VABCBO = '6'
053900040910     C                   ENDIF
054000040929     C                   ENDIF
054100040910     C*
054200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054300040910     C                   EXSR      CHKIMPDIV
054400010202     C*
054500000801     C* Ebbene...
054600000801     C                   ADD       1             §CTRMO
054700010201     C                   IF        errore <> *zeros
054800000801     C                   ADD       1             §CTRNO
054900000801     C                   EVAL      recko = vindta
055000000801     C                   EXCEPT    rigadett
055100000801     C                   EVAL      vinflg = '2'
055200000801     C                   ELSE
055300010201     C                   ADD       1             §CTROKVB
055400000801     C                   ENDIF
055500000801     C*
055600000801     C                   ENDSR
055700010202     C*----------------------------------------------------*
055800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055900010202     C*----------------------------------------------------*
056000020305     C     PREVAT        BEGSR
056100010202     C*
056200021113     C* Compongo il nome del membro da dare al FIVATWWR
056300010202     C                   eval      parmbr = vlrhdl
056400010202     C                   movel     'M'           parmbr
056500041210     C                   eval      parccm = vlrksc
056600010202     C                   eval      paropz = '1'
056700010202     C* Effettuo la chiamata al CLLE preposto
056800040506     C                   call(e)   'TITVVTC'
056900010202     C                   parm                    parccm
057000010202     C                   parm                    parmbr
057100010202     C                   parm                    paropz
057200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057300010202     C                   if        %error
057400010202     C                   movel     '1'           chkcall
057500010202     C                   else
057600010202     C                   movel     '0'           chkcall
057700010202     C                   endif
057800010202     C*
057900010202     C                   ENDSR
058000000801     C*----------------------------------------------------*
058100000801     C*  CONTROLLO NUMERICITA' CAMPI
058200000801     C*----------------------------------------------------*
058300000801     C     CHKNUM        BEGSR
058400000801     C*
058500000801     C                   call(e)   'ISNUMERIC'
058600000801     C                   PARM                    PiStr            30
058700041210     C                   PARM      '.'           PiDecChr          1
058800000801     C                   PARM      *ZEROS        PiVal            30 9
058900000801     C                   PARM      '0'           PiInt             1
059000000801     C                   PARM      '0'           PiNum             1
059100000801     C                   IF        %error
059200000801     C                   EVAL      PiInt=*off
059300000801     C                   ENDIF
059400000801     C*
059500000801     C                   ENDSR
059600000801     C***
059700000801
059800011113
059900011113     C*----------------------------------------------------*
060000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060100011113     C*----------------------------------------------------*
060200011113     C     CHKIMPDIV     BEGSR
060300011113     C*
060400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060500011113     C                   Z-ADD     *zeros        wrkDec            9 9
060600011113     C*
060700011113     C* Come prima cosa effettuo considerazioni sulla divisa
060800011113     C                   IF        vabIAS > *zeros
060900011113     C                   IF        vabVAS <> 'EUR'
061000011113     C                   EVAL      vabVAS =  'ITL'
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C                   IF        vabCAS > *zeros
061500011113     C                   IF        vabVCA <> 'EUR'
061600011113     C                   EVAL      vabVCA =  'ITL'
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C                   IF        vabVMD > *zeros
062100020305     C                   IF        vabVAD <> 'EUR'
062200011113     C                   EVAL      vabVAD =  'ITL'
062300011113     C                   ENDIF
062400011113     C                   ENDIF
062500011113     C*
062600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062700011113     C                   Z-ADD     vabIAS        wrkDec
062800011113     C                   IF        wrkDec > *zeros
062900011113     C                   IF        vabVAS = 'ITL'
063000011113     C                   EVAL      vabIAS = *zeros
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063500011113     C                   Z-ADD     vabCAS        wrkDec
063600011113     C                   IF        wrkDec > *zeros
063700011113     C                   IF        vabVCA = 'ITL'
063800011113     C                   EVAL      vabCAS = *zeros
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064300011113     C                   Z-ADD     vabVMD        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVAD = 'ITL'
064600011113     C                   EVAL      vabVMD = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C                   ENDSR
065100011113     C***
065200011113
065300011113
065400000801
065500000801
065600990920      /TITLE Invio dei dati al punto operativo.
065700010202     C     invio         BEGSR
065800990920     C*
065900021113     C* 1° invio FIVAT
066000010201     C                   reset                   dscmz
066100010201     C                   move      vlrpoi        cmzdst
066200021113     C                   eval      cmzfld = 'FIVATWWR'
066300010201     C                   eval      cmzmbd = vlrhdl
066400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066500021009     C***                if        prmfir = *blanks
066600021113     C                   eval      cmzfla = 'FIVAT00F'
066700021113     C                   eval      cmzmba = 'FIVAT00F'
066800021009     C***                else
066900021009     C***                eval      cmzfla = prmfir
067000021009     C***                eval      cmzmba = prmfir
067100021009     C***                endif
067200010201     C                   eval      cmznrr = *zeros
067300020305     C                   move      §ctrokvt      cmznrr
067400021018     C                   eval      cmzlba = vlrfl1
067500010201     C                   call(e)   'TIS711C'
067600010201     C                   parm                    dscmz
067700010201     C                   parm      *blanks       esito
067800010205     C                   if        %error
067900010205     C                             or cmzerr = '1'
068000010205     C                             or esito  = '1'
068100010205     C                   eval      wrkesito = '3'
068200010205     C                   else
068300010201     C*
068400021113     C* 2° invio FIVAB
068500010201     C                   reset                   dscmz
068600010201     C                   move      vlrpoi        cmzdst
068700010201     C                   eval      cmzfld = vlrfou
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100021113     C                   eval      cmzfla = 'FIVAB00F'
069200021113     C                   eval      cmzmba = 'FIVAB00F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800010201     C                   move      §ctrokvb      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010201     C                   if        %error
070400010201     C                             or cmzerr = '1'
070500010201     C                             or esito  = '1'
070600010201     C                   eval      wrkesito = '3'
070700010201     C                   endif
070800010205     C                   endif
070900990920     C*
071000000613     C                   ENDSR
071100000613     C***
071200070411
071300070411     C     *pssr         BEGSR
071400070411     C*
071500070411     C                   if        %open(tivin00r)
071600070411     C                   close     tivin00r
071700070411     C                   endif
071800070411     C                   if        %open(fivabwwr)
071900070411     C                   close     fivabwwr
072000070411     C                   endif
072100070411     C                   if        %open(fivatwwr)
072200070411     C                   close     fivatwwr
072300070411     C                   endif
072400070411     C*
072500070411     C* Effettuo la chiamata al CLLE preposto
072600070411     C                   call(e)   'TITVVTC'
072700070411     C                   parm                    parccm
072800070411     C                   parm                    parmbr
072900070411     C                   parm      '2'           paropz
073000070411     C*
073100070411     C                   eval      wrkesito = '2'
073200070411     C*
073300070411     C                   seton                                        LR
073400070411     C*
073500070411     C                   ENDSR     '*CANCL'
073600070411     C***
073700070411
073800990910
073900000613     C     *inzsr        BEGSR
074000990910     C*
074100990910     C     *entry        plist
074200990920     C                   parm                    tivlrds
074300990921     C                   parm      wrkesito      esito
074400000724     C                   parm                    prmlit
074500000710     C                   parm                    prmfir
074600000613     C*
074700000830     C* CALCOLA LA DATA CORRENTE
074800000830     C                   time                    wn14             14 0
074900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075000000830     C                   z-add     wn8           g08dat
075100000830     C                   z-add     *zeros        g08inv
075200000830     C                   movel     '0'           g08err
075300000830     C                   call      'XSRDA8'
075400000830     C                   parm                    wlbda8
075500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075600041210     C*
075700041210     C* Chiave su TABEL00F - parziale
075800041210     C     KEYtabP       KLIST
075900041210     C                   KFLD                    tblKUT
076000041210     C                   KFLD                    tblCOD
076100000830     C*
076200000613     C                   ENDSR
076300000613     C***
076400990908
076500050805     Otitvn7p   E            riepilogo         2
076600990915     O                                              'Upload via Internet'
076700990915     O                                           +1 'Traduzione TIVIN00R -'
076800021113     O                                           +1 'FIVABWWR/FIVATWWR'
076900010201     O                                           +1 'Report testate bolle'
077000990915     O          E            riepilogo   2
077100990915     O                       wrkdata
077200990915     O                       wrkora              +1
077300990915     O                       procname            +1
077400990915     O          E            riepilogo   2
077500990915     O                                              'Cliente..................:'
077600990915     O                       VABCCM        z     +1
077700990915     O          E            riepilogo   2
077800990920     O                                              'Riferimento Strategi.....:'
077900990920     O                       vlrhdl              +1
078000990915     O          E            riepilogo   2
078100990915     O                                              'Giusti...................:'
078200010201     O                       §CTROKVB      2   +  1
078300990915     O          E            riepilogo   2
078400990915     O                                              'Sbagliati e corretti.....:'
078500971022     O                       §CTRMO        2   +  1
078600990915     O          E            riepilogo   2
078700990915     O                                              'Sbagliati e scartati.....:'
078800971022     O                       §CTRNO        2   +  1
078900000613
079000050805     Otitvn7ps  E            testdett          2
079100000613     O                                              'Upload via Internet'
079200000613     O                                           +1 'Traduzione TIVIN00R -'
079300021113     O                                           +1 'FIVABWWR/FIVATWWR'
079400010201     O                                           +1 'Report testate bolle'
079500000616     O          E            testdett    3
079600000613     O                                           +2 'N° rec'
079700000613     O                                           +3 'Anteprima contenuto'
079800000616     O          E            rigadett    2
079900000613     O                       rrnum               +2
080000000621     O                       recko               +3
080100000616     O          E            findett     2
080200000613     O                       wrkdata
080300000613     O                       wrkora              +1
080400000613     O                       procname            +1
080500000616     O          E            findett     2
080600000613     O                                              'Cliente..................:'
080700000613     O                       VABCCM        z     +1
080800000616     O          E            findett     2
080900000613     O                                              'Riferimento Strategi.....:'
081000000613     O                       vlrhdl              +1
081100000616     O          E            findett     2
081200000613     O                                              'Giusti...................:'
081300010201     O                       §CTROKVB      2   +  1
081400000616     O          E            findett     2
081500000613     O                                              'Sbagliati e corretti.....:'
081600000613     O                       §CTRMO        2   +  1
081700000616     O          E            findett     2
081800000613     O                                              'Sbagliati e scartati.....:'
081900000613     O                       §CTRNO        2   +  1
082000000512** CMD - COMANDI CL
082100050805OVRPRTF FILE(TITVN7P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
082200050805OVRPRTF FILE(TITVN7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
082300050805DLTOVR FILE(TITVN7P TITVN7PS) LVL(*)
082400000512
082500000512
