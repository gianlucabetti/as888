000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500041210     Ftabel00f  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021113     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
000900041206     Ftitvg4p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200041206     Ftitvg4ps  O    f  198        PRINTER usropn
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
002300041210     D skNAZISO        S              3    DIM(1000)
002400041210     D skNAZBAR        S              3    DIM(1000)
002500000801     D*----------------------------------------------------
002600000801     D* DICHIARAZIOINE VARIABILI DI WRK
002700000801     D*----------------------------------------------------
002800990920     D dscmz         e ds                  inz
002900990910     D psds           sds
003000990910     D  procname         *PROC
003100990920     D tivlrds       e ds                  extname(tivlr00f)
003200041210     D ds15          e ds
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
004600041210     D FlgNewBol       s              1    INZ(*blanks)
004700041210     D jNAZ            s              5  0 INZ(*zeros)
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
006000041025     D trul33ds      e ds                  inz
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
010400041206     C                   if        not %open(titvg4ps)
010500041206     C                   open      titvg4ps
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
011900041206     C                   if        %open(titvg4ps)
012000000616     C                   except    findett
012100041206     C                   close     titvg4ps
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
013800041206     C                   if        not %open(titvg4p)
013900041206     C                   open      titvg4p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400041206     C                   if        %open(titvg4p)
014500041206     C                   close     titvg4p
014600990914     C                   endif
014700990910     C*
014800000613     C                   ENDSR
014900000613     C***
015000041210
015100041210     C*--------------------------------------------------------
015200041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
015300041210     C*--------------------------------------------------------
015400041210     C     CARTAB        BEGSR
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
023100041210     C* SCarico i buffer eventualmente rimasti in sospeso
023200041210     C                   IF        FlgNewBol = '1'
023300041210     C                   WRITE     FIVAB000
023400041210     C                   IF        VATNOT <> *blanks
023500041210     C                   WRITE     FIVAT000
023600041210     C                   ENDIF
023700041210     C                   ENDIF
023800010202     C*
023900010202     C                   endif
024000990910
024100990910     C* Se non ci sono record con errori ...
024200000710     C                   if        §ctrno = 0
024300990910     C* ... restituisco esito OK.
024400990921     C                   eval      wrkesito = '0'
024500990910     C                   else
024600010201     C                   if        §ctrokvb > 0
024700990921     C                   eval      wrkesito = '1'
024800000710     C                   else
024900000710     C                   eval      wrkesito = '2'
025000990910     C                   endif
025100000710     C                   endif
025200990910     C*
025300990914     C                   if        %open(tivin00r)
025400990908     C                   close     tivin00r
025500990914     C                   endif
025600021113     C                   if        %open(fivabwwr)
025700021113     C                   close     fivabwwr
025800990914     C                   endif
025900021113     C                   if        %open(fivatwwr)
026000021113     C                   close     fivatwwr
026100010201     C                   endif
026200990910     C*
026300010201     C                   if        §ctrokvb > 0
026400000724     C                             and vlrpoi <> *zeros
026500010202     C                   exsr      invio
026600990920     C                   endif
026700990920     C*
026800910830     C                   ENDSR
026900000613     C***
027000990920
027100000801     C*----------------------------------------------------*
027200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027300000801     C*----------------------------------------------------*
027400010201     C     INZVAR        BEGSR
027500000801     C*
027600040802     C                   Z-ADD     *zeros        Num5_0            5 0
027700040802     C                   MOVEL     '0'           FlgCAS            1
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
028200000801     C*----------------------------------------------------*
028300000801     C     DEFCAM        BEGSR
028400000801     C*
028500021113     C                   CLEAR                   FIVAB000
028600040802     C                   CLEAR                   FIVAT000
028700020619     C* Imposto i valori di default...
028800041210     C                   Z-ADD     0431893       VABCCM
028900041210     C                   Z-ADD     0431893       VATCCM
029000041210     C                   Z-ADD     043           VABLNP
029100041210     C                   Z-ADD     043           VATLNP
029200041210     C                   Z-ADD     000           VABCTR
029300040823     C                   MOVEL     '1'           VABCBO
029400020619     C* ... e poi verifico se sono stati passati come parametri
029500020619     C                   IF        vlrppt > *blanks
029600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABCCM
030100020619     C                   Z-ADD     PiVal         VATCCM
030200020619     C                   ENDIF
030300040506     C                   ENDIF
030400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030600020619     C                   EXSR      CHKNUM
030700020619     C                   IF        PiInt=*on
030800020619     C                   Z-ADD     PiVal         VABLNP
030900020619     C                   Z-ADD     PiVal         VATLNP
031000040506     C                   ENDIF
031100020619     C                   ENDIF
031200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031400020619     C                   EXSR      CHKNUM
031500020619     C                   IF        PiInt=*on
031600020619     C                   Z-ADD     PiVal         VABCTR
031700040506     C                   ENDIF
031800020619     C                   ENDIF
031900041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
032000041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
032100041210     C                   ENDIF
032200020619     C                   ENDIF
032300000801     C*
032400000801     C                   ENDSR
032500000801     C*----------------------------------------------------*
032600040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
032700000801     C*----------------------------------------------------*
032800040910     C     IMPVABVAT     BEGSR
032900041210     C*
033000041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
033100041210     C                   Z-ADD     *zeros        errore            1 0
033200040910     C*
033300041210     C********
033400050204     C* Tipo record 'E1EDL20' (dati testata: riferimenti, peso, volume, colli, etc..)
033500041210     C********
033600050204     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL20'
033700041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
033800041210     C                   IF        FlgNewBol = '1'
033900041210     C                   WRITE     FIVAB000
034000041210     C                   IF        VATNOT <> *blanks
034100041210     C                   WRITE     FIVAT000
034200041210     C                   EVAL      FlgNewBol = '0'
034300041210     C                   ENDIF
034400041210     C                   ENDIF
034500041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
034600040910     C                   EXSR      INZVAR
034700040910     C                   EXSR      DEFCAM
034800041210     C                   EVAL      FlgNewBol = '1'
034900040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
035000040928     C                   MOVEL     datcor        VABAAS
035100040928     C                   MOVEL     datcor        VATAAS
035200040928     C                   MOVE      datcor        VABMGS
035300040928     C                   MOVE(P)   vlrpoi        VABFGS
035400040928     C                   MOVE(P)   vlrpoi        VATFGS
035500041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
035600041210     C                   CLEAR                   TRUL33DS
035700041210     C                   EVAL      I33OPE = *zeros
035800041210     C                   EVAL      I33CNU = 302
035900041210     C                   EVAL      I33NUM = 1
036000041210     C                   MOVEL     TRUL33DS      KPJBU
036100041210     C                   CALL      'TRUL33R'
036200041210     C                   PARM                    KPJBA
036300041210     C                   MOVEL     KPJBU         TRUL33DS
036400041210     C                   IF        O33ERR = *zeros
036500041210     C                   Z-ADD     O33NRF        VABNSP
036600041210     C                   Z-ADD     O33NRF        VATNSP
036700041210     C                   ELSE
036800041210     C                   ADD       1             errore
036900041210     C                   EVAL      vinmsg = %trimr(vinmsg)
037000041025     C                             + ' ' + 'VABNSP VATNSP'
037100041210     C                   ENDIF
037200041210     C* ......VABRMN
037300041210     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
037400041210     C                   EXSR      CHKNUM
037500041210     C                   IF        PiInt=*on
037600041210     C                   Z-ADD     PiVal         VABRMN
037700041210     C                   ELSE
037800041210     C                   Z-ADD     1             VABRMN
037900041210     C                   ADD       1             errore
038000041210     C                   EVAL      vinmsg = %trimr(vinmsg)
038100041210     C                             + ' ' + 'VABRMN'
038200041210     C                   ENDIF
038300041210     C* ......VABRMA
038400041210     C                   EVAL      VABRMA=%trim(%subst(vindta:64:10))
038500041210     C* ......VABPKB
038600041210     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
038700041210     C                   EXSR      CHKNUM
038800041210     C                   IF        PiNum=*on
038900041210     C                   IF        %trim(%subst(vindta:187:3)) = 'GRM'          * da grammi a kili
039000041210     C                   EVAL      PiVal=PiVal/1000
039100050209     C                   Z-ADD     PiVal         VABPKB
039200041210     C                   ELSE
039300050209     C                   Z-ADD     PiVal         VABPKB                        * altrimenti dft. kg
039400041210     C                   ENDIF
039500041210     C                   ELSE
039600041210     C                   ADD       1             errore
039700041210     C                   EVAL      vinmsg = %trimr(vinmsg)
039800041210     C                             + ' ' + 'VABPKB'
039900041210     C                   ENDIF
040000041210     C* ......VABVLB
040100041210     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
040200041210     C                   EXSR      CHKNUM
040300041210     C                   IF        PiNum=*on
040400041210     C                   IF        %trim(%subst(vindta:205:3)) = 'DMQ'          * da dec3 a m3
040500041210     C                   EVAL      PiVal=PiVal/1000
040600050209     C                   Z-ADD     PiVal         VABVLB
040700041210     C                   ELSE
040800050209     C                   Z-ADD     PiVal         VABVLB                         * altrimenti dft. m3
040900041210     C                   ENDIF
041000041210     C                   ELSE
041100041210     C                   ADD       1             errore
041200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
041300041210     C                             + ' ' + 'VABVLB'
041400041210     C                   ENDIF
041500041210     C* ......VABNCL
041600041210     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
041700041210     C                   EXSR      CHKNUM
041800041210     C                   IF        PiInt=*on
041900041210     C                   Z-ADD     PiVal         VABNCL
042000041210     C                   ELSE
042100041210     C                   ADD       1             errore
042200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
042300041210     C                             + ' ' + 'VABNCL'
042400041210     C                   ENDIF
042500041210     C*
042600041210     C                   ENDIF
042700041210     C*
042800041210     C********
042900050204     C* Tipo record 'E1ADRM1' (dati instradamento: rag. soc. dest, indirizzo dest, etc...)
043000041210     C* - soto tipo 'WE' => Destinatario Merce
043100041210     C********
043200050204     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
043300041210     C                             %trim(%subst(vindta:64:3)) = 'WE'
043400041210     C* ......VABRSD/VABRD2
043500041210     C                   MOVEL     *blanks       wRSD             70
043600041210     C                   EVAL      wRSD = %trim(%subst(vindta:119:40)) + ' ' +
043700041210     C                                    %trim(%subst(vindta:159:40)) + ' ' +
043800041210     C                                    %trim(%subst(vindta:199:40)) + ' ' +
043900041210     C                                    %trim(%subst(vindta:239:40))
044000041210     C                   EVAL      VABRSD=%subst(wRSD:1:35)
044100041210     C                   EVAL      VABRD2=%subst(wRSD:1+35:35)
044200041210     C* ......VABIND
044300041210     C                   EVAL      VABIND=%trim(%subst(vindta:439:40)) + ' ' +
044400041210     C                                    %trim(%subst(vindta:479:40)) + ' ' +
044500041210     C                                    %trim(%subst(vindta:519:40)) + ' ' +
044600041210     C                                    %trim(%subst(vindta:559:40))
044700041210     C* ......VABCAD
044800041210     C                   EVAL      PiStr=%trim(%subst(vindta:573:10))
044900041210     C                   EXSR      CHKNUM
045000041210     C                   IF        PiInt=*on
045100041210     C                   Z-ADD     PiVal         Num5_0
045200041210     C                   MOVEL(P)  Num5_0        VABCAD
045300041210     C                   ELSE
045400041210     C                   ADD       1             errore
045500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
045600041210     C                             + ' ' + 'VABCAD'
045700041210     C                   ENDIF
045800041210     C* ......VABLOD
045900041210     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40)) + ' ' +
046000041210     C                                    %trim(%subst(vindta:648:40))
046100041210     C* ......VATNOT
046200041210     C                   EVAL      VATNOT=%trim(%subst(vindta:748:30)) + ' ' +
046300041210     C                                    %trim(%subst(vindta:778:40))
046400041210     C                   EVAL      VATTRC = 'B'
046500041210     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
046600041210     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
046700041210     C                   Z-ADD     1             jNAZ
046800041210     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
046900041210     C                   IF        %found
047000041210     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
047100041210     C                   ENDIF
047200041210     C* ......VATPRD
047300041210     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
047400040910     C*
047500040910     C                   ENDIF
047600040910     C*
047700040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
047800040910     C                   IF        FlgCAS <> '0'
047900040929     C                   IF        VABCBO = '1'
048000040910     C                   EVAL      VABCBO = '4'
048100040910     C                   ELSE
048200040929     C                   EVAL      VABCBO = '6'
048300040910     C                   ENDIF
048400040929     C                   ENDIF
048500040910     C*
048600040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048700040910     C                   EXSR      CHKIMPDIV
048800010202     C*
048900000801     C* Ebbene...
049000000801     C                   ADD       1             §CTRMO
049100010201     C                   IF        errore <> *zeros
049200000801     C                   ADD       1             §CTRNO
049300000801     C                   EVAL      recko = vindta
049400000801     C                   EXCEPT    rigadett
049500000801     C                   EVAL      vinflg = '2'
049600000801     C                   ELSE
049700010201     C                   ADD       1             §CTROKVB
049800000801     C                   ENDIF
049900000801     C*
050000000801     C                   ENDSR
050100010202     C*----------------------------------------------------*
050200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050300010202     C*----------------------------------------------------*
050400020305     C     PREVAT        BEGSR
050500010202     C*
050600021113     C* Compongo il nome del membro da dare al FIVATWWR
050700010202     C                   eval      parmbr = vlrhdl
050800010202     C                   movel     'M'           parmbr
050900041210     C                   eval      parccm = vlrksc
051000010202     C                   eval      paropz = '1'
051100010202     C* Effettuo la chiamata al CLLE preposto
051200040506     C                   call(e)   'TITVVTC'
051300010202     C                   parm                    parccm
051400010202     C                   parm                    parmbr
051500010202     C                   parm                    paropz
051600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051700010202     C                   if        %error
051800010202     C                   movel     '1'           chkcall
051900010202     C                   else
052000010202     C                   movel     '0'           chkcall
052100010202     C                   endif
052200010202     C*
052300010202     C                   ENDSR
052400000801     C*----------------------------------------------------*
052500000801     C*  CONTROLLO NUMERICITA' CAMPI
052600000801     C*----------------------------------------------------*
052700000801     C     CHKNUM        BEGSR
052800000801     C*
052900000801     C                   call(e)   'ISNUMERIC'
053000000801     C                   PARM                    PiStr            30
053100041210     C                   PARM      '.'           PiDecChr          1
053200000801     C                   PARM      *ZEROS        PiVal            30 9
053300000801     C                   PARM      '0'           PiInt             1
053400000801     C                   PARM      '0'           PiNum             1
053500000801     C                   IF        %error
053600000801     C                   EVAL      PiInt=*off
053700000801     C                   ENDIF
053800000801     C*
053900000801     C                   ENDSR
054000000801     C***
054100000801
054200011113
054300011113     C*----------------------------------------------------*
054400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054500011113     C*----------------------------------------------------*
054600011113     C     CHKIMPDIV     BEGSR
054700011113     C*
054800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054900011113     C                   Z-ADD     *zeros        wrkDec            9 9
055000011113     C*
055100011113     C* Come prima cosa effettuo considerazioni sulla divisa
055200011113     C                   IF        vabIAS > *zeros
055300011113     C                   IF        vabVAS <> 'EUR'
055400011113     C                   EVAL      vabVAS =  'ITL'
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C                   IF        vabCAS > *zeros
055900011113     C                   IF        vabVCA <> 'EUR'
056000011113     C                   EVAL      vabVCA =  'ITL'
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C                   IF        vabVMD > *zeros
056500020305     C                   IF        vabVAD <> 'EUR'
056600011113     C                   EVAL      vabVAD =  'ITL'
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057100011113     C                   Z-ADD     vabIAS        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVAS = 'ITL'
057400011113     C                   EVAL      vabIAS = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057900011113     C                   Z-ADD     vabCAS        wrkDec
058000011113     C                   IF        wrkDec > *zeros
058100011113     C                   IF        vabVCA = 'ITL'
058200011113     C                   EVAL      vabCAS = *zeros
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058700011113     C                   Z-ADD     vabVMD        wrkDec
058800011113     C                   IF        wrkDec > *zeros
058900011113     C                   IF        vabVAD = 'ITL'
059000011113     C                   EVAL      vabVMD = *zeros
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C                   ENDSR
059500011113     C***
059600011113
059700011113
059800000801
059900000801
060000990920      /TITLE Invio dei dati al punto operativo.
060100010202     C     invio         BEGSR
060200990920     C*
060300021113     C* 1° invio FIVAT
060400010201     C                   reset                   dscmz
060500010201     C                   move      vlrpoi        cmzdst
060600021113     C                   eval      cmzfld = 'FIVATWWR'
060700010201     C                   eval      cmzmbd = vlrhdl
060800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060900021009     C***                if        prmfir = *blanks
061000021113     C                   eval      cmzfla = 'FIVAT00F'
061100021113     C                   eval      cmzmba = 'FIVAT00F'
061200021009     C***                else
061300021009     C***                eval      cmzfla = prmfir
061400021009     C***                eval      cmzmba = prmfir
061500021009     C***                endif
061600010201     C                   eval      cmznrr = *zeros
061700020305     C                   move      §ctrokvt      cmznrr
061800021018     C                   eval      cmzlba = vlrfl1
061900010201     C                   call(e)   'TIS711C'
062000010201     C                   parm                    dscmz
062100010201     C                   parm      *blanks       esito
062200010205     C                   if        %error
062300010205     C                             or cmzerr = '1'
062400010205     C                             or esito  = '1'
062500010205     C                   eval      wrkesito = '3'
062600010205     C                   else
062700010201     C*
062800021113     C* 2° invio FIVAB
062900010201     C                   reset                   dscmz
063000010201     C                   move      vlrpoi        cmzdst
063100010201     C                   eval      cmzfld = vlrfou
063200010201     C                   eval      cmzmbd = vlrhdl
063300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063400021009     C***                if        prmfir = *blanks
063500021113     C                   eval      cmzfla = 'FIVAB00F'
063600021113     C                   eval      cmzmba = 'FIVAB00F'
063700021009     C***                else
063800021009     C***                eval      cmzfla = prmfir
063900021009     C***                eval      cmzmba = prmfir
064000021009     C***                endif
064100010201     C                   eval      cmznrr = *zeros
064200010201     C                   move      §ctrokvb      cmznrr
064300021018     C                   eval      cmzlba = vlrfl1
064400010201     C                   call(e)   'TIS711C'
064500010201     C                   parm                    dscmz
064600010201     C                   parm      *blanks       esito
064700010201     C                   if        %error
064800010201     C                             or cmzerr = '1'
064900010201     C                             or esito  = '1'
065000010201     C                   eval      wrkesito = '3'
065100010201     C                   endif
065200010205     C                   endif
065300990920     C*
065400000613     C                   ENDSR
065500000613     C***
065600070411
065700070411     C     *pssr         BEGSR
065800070411     C*
065900070411     C                   if        %open(tivin00r)
066000070411     C                   close     tivin00r
066100070411     C                   endif
066200070411     C                   if        %open(fivabwwr)
066300070411     C                   close     fivabwwr
066400070411     C                   endif
066500070411     C                   if        %open(fivatwwr)
066600070411     C                   close     fivatwwr
066700070411     C                   endif
066800070411     C*
066900070411     C* Effettuo la chiamata al CLLE preposto
067000070411     C                   call(e)   'TITVVTC'
067100070411     C                   parm                    parccm
067200070411     C                   parm                    parmbr
067300070411     C                   parm      '2'           paropz
067400070411     C*
067500070411     C                   eval      wrkesito = '2'
067600070411     C*
067700070411     C                   seton                                        LR
067800070411     C*
067900070411     C                   ENDSR     '*CANCL'
068000070411     C***
068100070411
068200990910
068300000613     C     *inzsr        BEGSR
068400990910     C*
068500990910     C     *entry        plist
068600990920     C                   parm                    tivlrds
068700990921     C                   parm      wrkesito      esito
068800000724     C                   parm                    prmlit
068900000710     C                   parm                    prmfir
069000000613     C*
069100000830     C* CALCOLA LA DATA CORRENTE
069200000830     C                   time                    wn14             14 0
069300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069400000830     C                   z-add     wn8           g08dat
069500000830     C                   z-add     *zeros        g08inv
069600000830     C                   movel     '0'           g08err
069700000830     C                   call      'XSRDA8'
069800000830     C                   parm                    wlbda8
069900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070000041210     C*
070100041210     C* Chiave su TABEL00F - parziale
070200041210     C     KEYtabP       KLIST
070300041210     C                   KFLD                    tblKUT
070400041210     C                   KFLD                    tblCOD
070500000830     C*
070600000613     C                   ENDSR
070700000613     C***
070800990908
070900041206     Otitvg4p   E            riepilogo         2
071000990915     O                                              'Upload via Internet'
071100990915     O                                           +1 'Traduzione TIVIN00R -'
071200021113     O                                           +1 'FIVABWWR/FIVATWWR'
071300010201     O                                           +1 'Report testate bolle'
071400990915     O          E            riepilogo   2
071500990915     O                       wrkdata
071600990915     O                       wrkora              +1
071700990915     O                       procname            +1
071800990915     O          E            riepilogo   2
071900990915     O                                              'Cliente..................:'
072000990915     O                       VABCCM        z     +1
072100990915     O          E            riepilogo   2
072200990920     O                                              'Riferimento Strategi.....:'
072300990920     O                       vlrhdl              +1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Giusti...................:'
072600010201     O                       §CTROKVB      2   +  1
072700990915     O          E            riepilogo   2
072800990915     O                                              'Sbagliati e corretti.....:'
072900971022     O                       §CTRMO        2   +  1
073000990915     O          E            riepilogo   2
073100990915     O                                              'Sbagliati e scartati.....:'
073200971022     O                       §CTRNO        2   +  1
073300000613
073400041206     Otitvg4ps  E            testdett          2
073500000613     O                                              'Upload via Internet'
073600000613     O                                           +1 'Traduzione TIVIN00R -'
073700021113     O                                           +1 'FIVABWWR/FIVATWWR'
073800010201     O                                           +1 'Report testate bolle'
073900000616     O          E            testdett    3
074000000613     O                                           +2 'N° rec'
074100000613     O                                           +3 'Anteprima contenuto'
074200000616     O          E            rigadett    2
074300000613     O                       rrnum               +2
074400000621     O                       recko               +3
074500000616     O          E            findett     2
074600000613     O                       wrkdata
074700000613     O                       wrkora              +1
074800000613     O                       procname            +1
074900000616     O          E            findett     2
075000000613     O                                              'Cliente..................:'
075100000613     O                       VABCCM        z     +1
075200000616     O          E            findett     2
075300000613     O                                              'Riferimento Strategi.....:'
075400000613     O                       vlrhdl              +1
075500000616     O          E            findett     2
075600000613     O                                              'Giusti...................:'
075700010201     O                       §CTROKVB      2   +  1
075800000616     O          E            findett     2
075900000613     O                                              'Sbagliati e corretti.....:'
076000000613     O                       §CTRMO        2   +  1
076100000616     O          E            findett     2
076200000613     O                                              'Sbagliati e scartati.....:'
076300000613     O                       §CTRNO        2   +  1
076400000512** CMD - COMANDI CL
076500041206OVRPRTF FILE(TITVG4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
076600041206OVRPRTF FILE(TITVG4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076700041206DLTOVR FILE(TITVG4P TITVG4PS) LVL(*)
076800000512
076900000512
