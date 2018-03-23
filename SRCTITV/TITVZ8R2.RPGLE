000100080925      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080925     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080925     FFIVABWwr  O    E             DISK    usropn
000700080925     FFIVATWwr  O    E             DISK    usropn
000800080925     FFIVAB02L  uf   e           k disk    usropn
000900080925     F                                     prefix(w_)
001000080925     F                                     rename(fivab000:fivab002)
001100080925     F                                     extfile(LibFileVB)
001200080925     FFIVAT01L  if   e           k disk    usropn
001300080925     F                                     prefix(w_)
001400080925     F                                     rename(fivat000:fivat001)
001500080925     F                                     extfile(LibFileVT)
001600070719     Ftitvz8p   O    f  132        PRINTER usropn
001700000621     F                                     oflind(*inoa)
001800070411     F                                     infsr(*pssr)
001900070719     Ftitvz8ps  O    f  198        PRINTER usropn
002000000621     F                                     oflind(*inob)
002100070411     F                                     infsr(*pssr)
002200990908
002300000512     D*------------
002400000512     D* COMANDI
002500000512     D*------------
002600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002700000801     D*----------------------------------------------------
002800000801     D* DICHIARAZIOINE VARIABILI DI WRK
002900000801     D*----------------------------------------------------
003000990920     D dscmz         e ds                  inz
003100990910     D psds           sds
003200990910     D  procname         *PROC
003300990920     D tivlrds       e ds                  extname(tivlr00f)
003400070719     D tisi95ds      e ds
003500990910     D esito           s              1
003600000724     D prmlit          s             10
003700000710     D prmfir          s             10
003800990921     D wrkesito        s                   like(esito)
003900990915     D wrkdata         s               d
004000990915     D wrkora          s               t
004100000613     D rrnum           s              6  0 INZ(*zeros)
004200000621     D recko           s            150    INZ(*blanks)
004300011113     D depcmd          s            150    INZ(*blanks)
004400010202     D parccm          s              8    INZ(*blanks)
004500010202     D parmbr          s             10    INZ(*blanks)
004600010202     D paropz          s              1    INZ(*blanks)
004700010202     D chkcall         s              1    INZ(*blanks)
004800070919     D wPrimaBolla     s              1    INZ('0')
004900080925     D LibFileVB       s             21A   inz
005000080925     D LibFileVT       s             21A   inz
005100080925     D currSysNeta     s              8A   inz('*NULL')
005200000830
005300000830     D*------------------
005400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500000830     D*------------------
005600000830     D WLBDA8          DS                  INZ
005700000830     D  G08DAT                 1      8  0
005800000830     D  G08INV                 9     16  0
005900000830     D  G08ERR                17     17
006000000830     D  G08TGI                18     22  0
006100041025     D*------------------
006200041025     D* DS REPERIMENTO NUMERATORE
006300041025     D*------------------
006400041025     D trul33ds      e ds                  inz
006500041025     D*------------------
006600041025     D* DS ARCHITETTURA
006700041025     D*------------------
006800041025     D kpjba         e ds                  inz
006900041025     D*------------------
007000990908
007100080925     D*------------------
007200080925     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
007300080925     D*------------------
007400080925     D PiStr           S            100A   INZ
007500080925     D PiDecChr        S              1A   INZ
007600080925     D PiVal           S             63S30 INZ
007700080925     D PiInt           S               N   INZ
007800080925     D PiNum           S               N   INZ
007900080925
008000080925
008100080925     D*------------------
008200080925     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
008300080925     D*------------------
008400080925     D  PiOutSysNeta   S              8A
008500080925
008600080925
008700080925     D*------------------
008800080925     D* LINKING A DEFINIZIONI ESTERNE
008900080925     D*------------------
009000080925     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100080925     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009200080925
009300080925
009400080925
009500080925
009600080925
009700080925     C*
009800080925     C* Reperisco il sistema AS/400 corrente
009900080925     C                   callp     UBRTVNETA_Rtv(currSysNeta)
010000080925     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
010100080925     C                   eval      LibFileVB = 'FILTRAGRU/FIVAB02L'
010200080925     C                   eval      LibFileVT = 'FILTRAGRU/FIVAT01L'
010300080925     C                   else
010400080925     C                   eval      LibFileVB = 'FILTRAGRPF/FIVAB02L'
010500080925     C                   eval      LibFileVT = 'FILTRAGRPF/FIVAT01L'
010600080925     C                   endif
010700080925     C*
010800080925     C                   if        not %open(FIVAB02L)
010900080925     C                   open      FIVAB02L
011000080925     C                   endif
011100080925     C                   if        not %open(FIVAT01L)
011200080925     C                   open      FIVAT01L
011300080925     C                   endif
011400080925     C*
011500990915     C                   time                    wrkdata
011600990915     C                   time                    wrkora
011700000913     C                   reset                   rrnum
011800990921     C                   reset                   esito
011900990921     C                   reset                   wrkesito
012000000724     C*
012100000724     C* SE OCCORRE SPEDIRE IN FILIALE
012200000724     C                   if        vlrpoi <> *zeros
012300000724     C*
012400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600000724     C     vlrpoi        chain     azorg01l
012700000724     C                   if        %found
012800000616     C                   movel(p)  CMD(1)        depcmd
012900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013000000616     C*
013100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    depcmd
013500000616     C                   PARM                    LENGH
013600000724     C*
013700000724     C                   endif
013800000724     C                   endif
013900000616     C*
014000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014100000616     C                   movel(p)  CMD(2)        depcmd
014200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014300000616     C*
014400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000616     C                   PARM                    depcmd
014800000616     C                   PARM                    LENGH
014900000616     C*
015000070719     C                   if        not %open(titvz8ps)
015100070719     C                   open      titvz8ps
015200000616     C                   except    testdett
015300000613     C                   endif
015400000613     C*
015500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
015600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015700070719     C*
015800070719     C* Esegue lancio TISI95R solo x chiusura
015900070719     C                   CLEAR                   TISI95DS
016000070719     C                   EVAL      I95TLA = 'C'
016100070719     C                   CALL      'TISI95R'
016200070719     C                   PARM                    TISI95DS
016300000613     C*
016400010202     C* Effettuo la chiamata al CLLE preposto
016500080925     C                   call(e)   'TITVVTC'
016600010202     C                   parm                    parccm
016700010202     C                   parm                    parmbr
016800010202     C                   parm      '2'           paropz
016900080925     C*
017000080925     C                   if        %open(FIVAB02L)
017100080925     C                   close     FIVAB02L
017200080925     C                   endif
017300080925     C                   if        %open(FIVAT01L)
017400080925     C                   close     FIVAT01L
017500080925     C                   endif
017600010202     C*
017700070719     C                   if        %open(titvz8ps)
017800000616     C                   except    findett
017900070719     C                   close     titvz8ps
018000000613     C                   endif
018100000616     C*
018200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018300011113     C                   Z-ADD     150           LENGH            15 5
018400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018500000616     C                   PARM                    CMD(3)
018600000616     C                   PARM                    LENGH
018700000616     C*
018800010201     C                   seton                                        LR
018900000613
019000000613
019100000613     C*--------------------------------------------------------
019200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019300000613     C*--------------------------------------------------------
019400000621     C     STPR          BEGSR
019500000613     C*
019600070719     C                   if        not %open(titvz8p)
019700070719     C                   open      titvz8p
019800990915     C                   endif
019900990915     C*
020000990915     C                   except    riepilogo
020100990915     C*
020200070719     C                   if        %open(titvz8p)
020300070719     C                   close     titvz8p
020400990914     C                   endif
020500990910     C*
020600000613     C                   ENDSR
020700000613     C***
020800990908
020900000801
021000910830     C*--------------------------------------------------------
021100080925     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
021200910830     C*--------------------------------------------------------
021300040526     C     RWFILE        BEGSR
021400990910     C*
021500990914     C                   if        not %open(tivin00r)
021600990908     C                   open      tivin00r
021700990914     C                   endif
021800080925     C                   if        not %open(fivabwwr)
021900080925     C                   open      fivabwwr
022000990914     C                   endif
022100080925     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022200020305     C                   exsr      prevat
022300010201     C*
022400010202     C                   if        chkcall = '0'
022500010202     C*
022600080925     C                   if        not %open(fivatwwr)
022700080925     C                   open      fivatwwr
022800010201     C                   endif
022900990910     C*
023000010201     C                   clear                   §CTROKVB          5 0
023100020305     C                   clear                   §CTROKVT          5 0
023200000801     C                   clear                   §CTRMO            5 0
023300000801     C                   clear                   §CTRNO            5 0
023400070919     C*
023500070919     C* Inizializzazioni iniziali e formati record file Bartolini
023600070919     C                   EXSR      INZVAR
023700070919     C                   EXSR      DEFCAM
023800040910     C*
023900921023     C                   DO        *HIVAL
024000990913     C*
024100990915     C                   READ      tivin00r                               70
024200040910     C                   if        vindta > *blanks
024300000613     C                   add       1             rrnum
024400000801     C*
024500000801     C                   if        *in70 = *off
024600000801     C                             and
024700000801     C                             (vinflg = *blanks
024800000801     C                              or vinflg = '0'
024900000801     C                              or vinflg = '2')
025000000801     C*
025100000801     C                   clear                   vinmsg
025200000801     C                   eval      vinflg = '1'
025300040910     C*
025400040910     C* Eseguo routine d traduzione
025500040910     C                   exsr      impvabvat
025600040802     C*
025700010305     C                   endif
025800000905     C*
025900000905     C                   else
026000000905     C                   eval      vinflg = '1'
026100000905     C                   endif
026200000905     C*
026300000905     C  N70              update    tivin000
026400000905     C*
026500991022     C  N70              ENDdo
026600010202     C*
026700070919     C* Scarico l'ultima bolla rimasta in canna
026800070919     C                   IF        wPrimaBolla = '1'
026900080925     C                   exsr      wriVAB
027000070919     C                   ENDIF
027100070919     C*
027200010202     C                   endif
027300990910
027400990910     C* Se non ci sono record con errori ...
027500000710     C                   if        §ctrno = 0
027600990910     C* ... restituisco esito OK.
027700990921     C                   eval      wrkesito = '0'
027800990910     C                   else
027900010201     C                   if        §ctrokvb > 0
028000990921     C                   eval      wrkesito = '1'
028100000710     C                   else
028200000710     C                   eval      wrkesito = '2'
028300990910     C                   endif
028400000710     C                   endif
028500990910     C*
028600990914     C                   if        %open(tivin00r)
028700990908     C                   close     tivin00r
028800990914     C                   endif
028900080925     C                   if        %open(fivabwwr)
029000080925     C                   close     fivabwwr
029100990914     C                   endif
029200080925     C                   if        %open(fivatwwr)
029300080925     C                   close     fivatwwr
029400010201     C                   endif
029500990910     C*
029600010201     C                   if        §ctrokvb > 0
029700000724     C                             and vlrpoi <> *zeros
029800010202     C                   exsr      invio
029900990920     C                   endif
030000990920     C*
030100910830     C                   ENDSR
030200000613     C***
030300990920
030400000801     C*----------------------------------------------------*
030500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030600000801     C*----------------------------------------------------*
030700010201     C     INZVAR        BEGSR
030800000801     C*
030900040802     C                   Z-ADD     *zeros        Num5_0            5 0
031000040802     C                   MOVEL     '0'           FlgCAS            1
031100000801     C*
031200000801     C                   ENDSR
031300000801     C*----------------------------------------------------*
031400040910     C*  IMPOSTAZIONE CAMPI COSTANTI
031500000801     C*----------------------------------------------------*
031600000801     C     DEFCAM        BEGSR
031700000801     C*
031800080925     C                   CLEAR                   FIVAB000
031900080925     C                   CLEAR                   FIVAT000
032000020619     C* Imposto i valori di default...
032100070801     C                   Z-ADD     0433996       VABCCM
032200070801     C                   Z-ADD     0433996       VATCCM
032300070801     C                   Z-ADD     043           VABLNP
032400070801     C                   Z-ADD     043           VATLNP
032500070531     C                   Z-ADD     000           VABCTR
032600080825     C                   MOVEL     '7Q'          VABCTM
032700040823     C                   MOVEL     '1'           VABCBO
032800020619     C* ... e poi verifico se sono stati passati come parametri
032900020619     C                   IF        vlrppt > *blanks
033000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
033100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033200020619     C                   EXSR      CHKNUM
033300020619     C                   IF        PiInt=*on
033400020619     C                   Z-ADD     PiVal         VABCCM
033500020619     C                   Z-ADD     PiVal         VATCCM
033600020619     C                   ENDIF
033700040506     C                   ENDIF
033800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
033900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034000020619     C                   EXSR      CHKNUM
034100020619     C                   IF        PiInt=*on
034200020619     C                   Z-ADD     PiVal         VABLNP
034300020619     C                   Z-ADD     PiVal         VATLNP
034400040506     C                   ENDIF
034500020619     C                   ENDIF
034600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
034700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034800020619     C                   EXSR      CHKNUM
034900020619     C                   IF        PiInt=*on
035000020619     C                   Z-ADD     PiVal         VABCTR
035100040506     C                   ENDIF
035200020619     C                   ENDIF
035300060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
035400060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
035500060202     C                   ENDIF
035600090202     C                   IF        %subst(vlrppt:16:5) <> *blanks
035700090202     C                   MOVEL     *blanks       wPreWHOARE        5
035800090202     C                   EVAL      wPreWHOARE = %subst(vlrppt:16:5)
035900090202     C                   ENDIF
036000020619     C                   ENDIF
036100000801     C*
036200000801     C                   ENDSR
036300000801     C*----------------------------------------------------*
036400080925     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
036500000801     C*----------------------------------------------------*
036600040910     C     IMPVABVAT     BEGSR
036700040910     C*
036800040910     C* Traduzione relativa ai tipi record del file del cliente
036900040910     C*
037000070719     C* Resetto indicatore d anomalia sul singolo record
037100070719     C                   eval      vinflg = '1'
037200070808     C***
037300070808     C* ...tipo record '01' (testata)
037400070808     C                   IF        %subst(vindta:1:2) = '01'
037500070919     C* ......verifico se è la prima bolla elaborata
037600070919     C                   IF        wPrimaBolla = '0'
037700070919     C                   EVAL      wPrimaBolla = '1'
037800070919     C                   ELSE
037900080925     C                   EXSR      WRIVAB
038000070919     C* ......inizializzazioni iniziali e formati record file Bartolini
038100070919     C                   EXSR      INZVAR
038200070919     C                   EXSR      DEFCAM
038300070919     C                   ENDIF
038400040928     C*
038500040928     C                   Z-ADD     *zeros        errore            1 0
038600040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
038700040928     C                   MOVEL     datcor        VABAAS
038800040928     C                   MOVEL     datcor        VATAAS
038900040928     C                   MOVE      datcor        VABMGS
039000040928     C                   MOVE(P)   vlrpoi        VABFGS
039100040928     C                   MOVE(P)   vlrpoi        VATFGS
039200040910     C* ......VABNSP/VATNSP
039300041025     C* NSP => Stacco un numeratore da AZNUM
039400070807     C*                  clear                   TRUL33DS
039500070807     C*                  eval      I33OPE = *zeros
039600070807     C*                  eval      I33CNU = 302
039700070807     C*                  eval      I33NUM = 1
039800070807     C*                  movel     TRUL33DS      KPJBU
039900070807     C*                  call      'TRUL33R'
040000070807     C*                  parm                    KPJBA
040100070807     C*                  movel     KPJBU         TRUL33DS
040200070807     C*                  if        O33ERR = *zeros
040300070807     C*                  z-add     O33NRF        VABNSP
040400070807     C*                  z-add     O33NRF        VATNSP
040500070807     C*                  else
040600070807     C*                  Z-ADD     1             errore
040700070807     C*                  EVAL      vinmsg = %trimr(vinmsg)
040800070807     C*                            + ' ' + 'VABNSP VATNSP'
040900070807     C*                  endif
041000070807     C* ......VABNSP/VATNSP
041100070807     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
041200070807     C                   EXSR      CHKNUM
041300070807     C                   IF        PiInt=*on
041400070807     C                   Z-ADD     PiVal         VABNSP
041500070807     C                   Z-ADD     PiVal         VATNSP
041600070807     C                   ELSE
041700070807     C                   ADD       1             errore
041800070807     C                   EVAL      vinmsg = %trimr(vinmsg)
041900070807     C                             + ' ' + 'VABNSP VATNSP'
042000070807     C                   ENDIF
042100060202     C* ......VABRMA
042200070719     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
042300060202     C* ......VABRMN
042400070917     C***                EVAL      PiStr=%trim(%subst(vindta:209:10))
042500070917     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
042600060202     C                   EXSR      CHKNUM
042700060202     C                   IF        PiInt=*on
042800060202     C                   Z-ADD     PiVal         VABRMN
042900060202     C                   ELSE
043000060202     C                   ADD       1             errore
043100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
043200060202     C                             + ' ' + 'VABRMN'
043300060202     C                   ENDIF
043400080926 xxx C* ......VABNCL
043500081008     C                   EVAL      PiStr=%trim(%subst(vindta:183:5))
043600081008     C                   EXSR      CHKNUM
043700081008     C                   IF        PiInt=*on
043800081008     C                   Z-ADD     PiVal         VABNCL
043900081008     C                   ELSE
044000081008     C                   ADD       1             errore
044100081008     C                   EVAL      vinmsg = %trimr(vinmsg)
044200081008     C                             + ' ' + 'VABNCL'
044300081008     C                   ENDIF
044400060202     C* ......VABPKB
044500070719     C                   EVAL      PiStr=%trim(%subst(vindta:188:12))
044600060202     C                   EXSR      CHKNUM
044700060202     C                   IF        PiNum=*on
044800070719     C                   Z-ADD     PiVal         VABPKB
044900060202     C                   ELSE
045000060202     C                   ADD       1             errore
045100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
045200060202     C                             + ' ' + 'VABPKB'
045300060202     C                   ENDIF
045400060202     C* ......VABVLB
045500070808     C                   IF        %subst(vindta:330:7) <> *blanks
045600070719     C                   EVAL      PiStr=%trim(%subst(vindta:330:7))
045700060202     C                   EXSR      CHKNUM
045800060202     C                   IF        PiNum=*on
045900070719     C                   Z-ADD     PiVal         VABVLB
046000060202     C                   ELSE
046100060202     C                   ADD       1             errore
046200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
046300060202     C                             + ' ' + 'VABVLB'
046400060202     C                   ENDIF
046500070808     C                   ENDIF
046600060202     C* ......VABRSD
046700070719     C                   EVAL      VABRSD=%trim(%subst(vindta:35:40))
046800060202     C* ......VABIND
046900070719     C                   EVAL      VABIND=%trim(%subst(vindta:75:35))
047000060202     C* ......VABLOD
047100070719     C                   EVAL      VABLOD=%trim(%subst(vindta:130:30))
047200060202     C* ......VABCAD
047300070719     C                   EVAL      PiStr=%trim(%subst(vindta:125:5))
047400060202     C                   EXSR      CHKNUM
047500060202     C                   IF        PiInt=*on
047600060202     C                   Z-ADD     PiVal         Num5_0
047700060202     C                   MOVEL(P)  Num5_0        VABCAD
047800060202     C                   ELSE
047900060202     C                   ADD       1             errore
048000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
048100060202     C                             + ' ' + 'VABCAD'
048200060202     C                   ENDIF
048300060202     C* ......VABPRD
048400070719     C                   EVAL      VABPRD=%trim(%subst(vindta:160:2))
048500070719     C* Reperisco la provincia dal CAP e dalla località
048600070719     C                   IF        VABPRD  = *blanks AND
048700070719     C                             VABCAD <> *blanks
048800070719     C                   CLEAR                   TISI95DS
048900070719     C                   EVAL      I95TCN = '3'
049000070719     C                   Z-ADD     datcor        I95DAT
049100070719     C                   EVAL      I95CAP = VABCAD
049200070719     C                   EVAL      I95LOC = VABLOD
049300070719     C                   CALL      'TISI95R'
049400070719     C                   PARM                    TISI95DS
049500070719     C                   EVAL      VABPRD = O95PRV
049600070719     C                   ENDIF
049700070719     C* ......VABNOT+VABNT2
049800070719     C                   MOVEL     *blanks       wNOTE            80
049900070719     C                   EVAL      wNOTE=%trim(%subst(vindta:219:80))
050000070719     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
050100070719     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:80-35))
050200070719     C* ......VABVCA
050300070719     C                   EVAL      VABVCA=%trim(%subst(vindta:299:3))
050400070719     C* ......VABCAS
050500070919     C                   IF        %subst(vindta:302:12)        <> *blanks AND
050600070919     C                             %trim(%subst(vindta:302:12)) <> '0,00'
050700070719     C                   EVAL      FlgCAS = '1'
050800070727     C                   EVAL      VABTIC = 'BM'
050900070719     C                   EVAL      PiStr=%trim(%subst(vindta:302:12))
051000070719     C                   EXSR      CHKNUM
051100070719     C                   IF        PiNum=*on
051200070719     C                   Z-ADD     PiVal         VABCAS
051300070719     C                   ELSE
051400070719     C                   ADD       1             errore
051500070719     C                   EVAL      vinmsg = %trimr(vinmsg)
051600070719     C                             + ' ' + 'VABCAS'
051700070719     C                   ENDIF
051800070719     C                   ENDIF
051900070719     C* ......VABVAS
052000070719     C                   EVAL      VABVAS=%trim(%subst(vindta:314:3))
052100070719     C* ......VABIAS
052200070719     C                   IF        %trim(%subst(vindta:317:12)) <> *blanks
052300070719     C                   EVAL      PiStr=%trim(%subst(vindta:317:12))
052400070719     C                   EXSR      CHKNUM
052500070719     C                   IF        PiNum=*on
052600070719     C                   Z-ADD     PiVal         VABIAS
052700070719     C                   ELSE
052800070719     C                   ADD       1             errore
052900070719     C                   EVAL      vinmsg = %trimr(vinmsg)
053000070719     C                             + ' ' + 'VABIAS'
053100070719     C                   ENDIF
053200070719     C                   ENDIF
053300070719     C* ......VATNOT "B" - Telefono destinatario
053400070719     C                   EVAL      VATNOT=%trim(%subst(vindta:110:15))
053500070719     C                   EVAL      VATTRC = 'B'
053600070719     C                   IF        VATNOT <> *blanks
053700080925     C                   EXSR      WRIVAT
053800070719     C                   ENDIF
053900060202     C*
054000060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
054100060202     C                   IF        FlgCAS <> '0'
054200060202     C                   IF        VABCBO = '1'
054300060202     C                   EVAL      VABCBO = '4'
054400060202     C                   ELSE
054500060202     C                   EVAL      VABCBO = '6'
054600060202     C                   ENDIF
054700060202     C                   ENDIF
054800060202     C*
054900060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055000060202     C                   EXSR      CHKIMPDIV
055100040910     C*
055200040910     C                   ENDIF
055300040910     C***
055400070719     C* ...tipo record '02' (dettaglio barcode segnacooli)
055500070719     C                   IF        %subst(vindta:1:2) = '02'
055600070719     C* Resetto indicatore d anomalia sul singolo record
055700070719     C                   eval      vinflg = '1'
055800070719     C*
055900070719     C* Ciclo x tutti i 50 colli max previsti sul record
056000070719     C                   Z-ADD     19            wPosWho           3 0
056100070719     C                   Z-ADD     10            wLenWho           2 0
056200070719     C                   DOW       wPosWho <= %len(%trim(vindta))
056300040910     C* ......VATNOT
056400090202     C                   EVAL      VATNOT=%trim(wPreWHOARE) +
056500090202     C                                    %trim(%subst(vindta:wPosWho:wLenWho))
056600080926 xxx C***                EVAL      VATNOT=%subst(%trim(VATNOT):2)
056700040910     C                   EVAL      VATTRC = 'E'
056800080925     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
056900060202     C                   IF        VATNOT <> *blanks
057000081008 xxx C***                EVAL      vabNCL = vabNCL + 1
057100080925     C                   EXSR      WRIVAT
057200060202     C                   ENDIF
057300070830     C                   EVAL      wPosWho = wPosWho + wLenWho
057400070719     C                   ENDDO
057500040910     C*
057600040910     C                   ENDIF
057700070919     C***
057800070919     C* ...tipo record '03' (estensione testata: note consegna)
057900070919     C                   IF        %subst(vindta:1:2) = '03'
058000070919     C                   MOVEL     *blanks       wNOTE2           70
058100070919     C                   EVAL      wNOTE2 = %trim(%subst(vindta:13))
058200070919     C                   EVAL      VABNOT = %subst(wNOTE2:1:35)
058300070919     C                   EVAL      VABNT2 = %subst(wNOTE2:36:35)
058400070919     C                   ENDIF
058500010202     C*
058600000801     C* Ebbene...
058700000801     C                   ADD       1             §CTRMO
058800010201     C                   IF        errore <> *zeros
058900000801     C                   ADD       1             §CTRNO
059000000801     C                   EVAL      recko = vindta
059100000801     C                   EXCEPT    rigadett
059200000801     C                   EVAL      vinflg = '2'
059300000801     C                   ELSE
059400010201     C                   ADD       1             §CTROKVB
059500000801     C                   ENDIF
059600000801     C*
059700000801     C                   ENDSR
059800080925
059900080925
060000080925     C*--------------------------------------------------------
060100080925     C* WRIVAB - EFFETTUA CONTROLLI D "NN DOPPIETA" E SCARICA IL BUFFER
060200080925     C*--------------------------------------------------------
060300080925     C     WRIVAB        BEGSR
060400080925     C*
060500080925     C* Verifico che stessa testata nn già presente
060600080925     C     KEYvab02_C    CHAIN     fivab02l
060700080925     C                   IF        %found(fivab02l)
060800080925     C                   IF        vlrpoi <> 789
060900080925     C                   EVAL      w_vabPKB = w_vabPKB + vabPKB
061000081008 xxx C***                EVAL      w_vabNCL = w_vabNCL + vabNCL
061100080925     C                   UPDATE    FIVAB002
061200080925     C                   ENDIF
061300080925     C                   ELSE
061400080925     C                   WRITE     FIVAB000
061500080925     C                   ENDIF
061600080925     C*
061700080925     C                   ENDSR
061800080925     C***
061900080925
062000080925
062100080925     C*--------------------------------------------------------
062200080925     C* WRIVAT - EFFETTUA CONTROLLI D "NN DOPPIETA" E SCARICA IL BUFFER
062300080925     C*--------------------------------------------------------
062400080925     C     WRIVAT        BEGSR
062500080925     C*
062600080925     C* Verifico che stessa testata nn già presente
062700080925     C     KEYvat01_C    SETLL     fivat01l
062800080925     C                   IF        %equal(fivat01l)
062900080925     C                   ELSE
063000080925     C                   WRITE     FIVAT000
063100080925     C                   ENDIF
063200080925     C*
063300080925     C                   ENDSR
063400080925     C***
063500080925
063600080925
063700010202     C*----------------------------------------------------*
063800080925     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
063900010202     C*----------------------------------------------------*
064000020305     C     PREVAT        BEGSR
064100010202     C*
064200080925     C* Compongo il nome del membro da dare al FIVATWWR
064300010202     C                   eval      parmbr = vlrhdl
064400010202     C                   movel     'M'           parmbr
064500060113     C                   eval      parccm = vlrksc
064600010202     C                   eval      paropz = '1'
064700010202     C* Effettuo la chiamata al CLLE preposto
064800080925     C                   call(e)   'TITVVTC'
064900010202     C                   parm                    parccm
065000010202     C                   parm                    parmbr
065100010202     C                   parm                    paropz
065200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065300010202     C                   if        %error
065400010202     C                   movel     '1'           chkcall
065500010202     C                   else
065600010202     C                   movel     '0'           chkcall
065700010202     C                   endif
065800010202     C*
065900010202     C                   ENDSR
066000000801     C*----------------------------------------------------*
066100000801     C*  CONTROLLO NUMERICITA' CAMPI
066200000801     C*----------------------------------------------------*
066300000801     C     CHKNUM        BEGSR
066400000801     C*
066500080925     C                   IF        PiDecChr = *blanks
066600080925     C                   EVAL      PiDecChr = ','
066700080925     C                   ENDIF
066800080925     C*
066900080925     C                   callp     UBISNUM_Check(PiStr
067000080925     C                                          :PiDecChr
067100080925     C                                          :PiVal
067200080925     C                                          :PiNum
067300080925     C                                          :PiInt)
067400000801     C*
067500000801     C                   ENDSR
067600000801     C***
067700000801
067800011113
067900011113     C*----------------------------------------------------*
068000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
068100011113     C*----------------------------------------------------*
068200011113     C     CHKIMPDIV     BEGSR
068300011113     C*
068400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
068500011113     C                   Z-ADD     *zeros        wrkDec            9 9
068600011113     C*
068700011113     C* Come prima cosa effettuo considerazioni sulla divisa
068800011113     C                   IF        vabIAS > *zeros
068900011113     C                   IF        vabVAS <> 'EUR'
069000011113     C                   EVAL      vabVAS =  'ITL'
069100011113     C                   ENDIF
069200011113     C                   ENDIF
069300011113     C*
069400011113     C                   IF        vabCAS > *zeros
069500011113     C                   IF        vabVCA <> 'EUR'
069600011113     C                   EVAL      vabVCA =  'ITL'
069700011113     C                   ENDIF
069800011113     C                   ENDIF
069900011113     C*
070000011113     C                   IF        vabVMD > *zeros
070100020305     C                   IF        vabVAD <> 'EUR'
070200011113     C                   EVAL      vabVAD =  'ITL'
070300011113     C                   ENDIF
070400011113     C                   ENDIF
070500011113     C*
070600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070700011113     C                   Z-ADD     vabIAS        wrkDec
070800011113     C                   IF        wrkDec > *zeros
070900011113     C                   IF        vabVAS = 'ITL'
071000011113     C                   EVAL      vabIAS = *zeros
071100011113     C                   ENDIF
071200011113     C                   ENDIF
071300011113     C*
071400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
071500011113     C                   Z-ADD     vabCAS        wrkDec
071600011113     C                   IF        wrkDec > *zeros
071700011113     C                   IF        vabVCA = 'ITL'
071800011113     C                   EVAL      vabCAS = *zeros
071900011113     C                   ENDIF
072000011113     C                   ENDIF
072100011113     C*
072200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
072300011113     C                   Z-ADD     vabVMD        wrkDec
072400011113     C                   IF        wrkDec > *zeros
072500011113     C                   IF        vabVAD = 'ITL'
072600011113     C                   EVAL      vabVMD = *zeros
072700011113     C                   ENDIF
072800011113     C                   ENDIF
072900011113     C*
073000011113     C                   ENDSR
073100011113     C***
073200011113
073300011113
073400000801
073500000801
073600990920      /TITLE Invio dei dati al punto operativo.
073700010202     C     invio         BEGSR
073800990920     C*
073900080925     C* 1° invio FIVAT
074000010201     C                   reset                   dscmz
074100010201     C                   move      vlrpoi        cmzdst
074200080925     C                   eval      cmzfld = 'FIVATWWR'
074300010201     C                   eval      cmzmbd = vlrhdl
074400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074500021009     C***                if        prmfir = *blanks
074600080925     C                   eval      cmzfla = 'FIVAT00F'
074700080925     C                   eval      cmzmba = 'FIVAT00F'
074800021009     C***                else
074900021009     C***                eval      cmzfla = prmfir
075000021009     C***                eval      cmzmba = prmfir
075100021009     C***                endif
075200010201     C                   eval      cmznrr = *zeros
075300020305     C                   move      §ctrokvt      cmznrr
075400021018     C                   eval      cmzlba = vlrfl1
075500010201     C                   call(e)   'TIS711C'
075600010201     C                   parm                    dscmz
075700010201     C                   parm      *blanks       esito
075800010205     C                   if        %error
075900010205     C                             or cmzerr = '1'
076000010205     C                             or esito  = '1'
076100010205     C                   eval      wrkesito = '3'
076200010205     C                   else
076300010201     C*
076400080925     C* 2° invio FIVAB
076500010201     C                   reset                   dscmz
076600010201     C                   move      vlrpoi        cmzdst
076700010201     C                   eval      cmzfld = vlrfou
076800010201     C                   eval      cmzmbd = vlrhdl
076900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077000021009     C***                if        prmfir = *blanks
077100080925     C                   eval      cmzfla = 'FIVAB00F'
077200080925     C                   eval      cmzmba = 'FIVAB00F'
077300021009     C***                else
077400021009     C***                eval      cmzfla = prmfir
077500021009     C***                eval      cmzmba = prmfir
077600021009     C***                endif
077700010201     C                   eval      cmznrr = *zeros
077800010201     C                   move      §ctrokvb      cmznrr
077900021018     C                   eval      cmzlba = vlrfl1
078000010201     C                   call(e)   'TIS711C'
078100010201     C                   parm                    dscmz
078200010201     C                   parm      *blanks       esito
078300010201     C                   if        %error
078400010201     C                             or cmzerr = '1'
078500010201     C                             or esito  = '1'
078600010201     C                   eval      wrkesito = '3'
078700010201     C                   endif
078800010205     C                   endif
078900990920     C*
079000000613     C                   ENDSR
079100000613     C***
079200070411
079300070411     C     *pssr         BEGSR
079400070411     C*
079500070411     C                   if        %open(tivin00r)
079600070411     C                   close     tivin00r
079700070411     C                   endif
079800080925     C                   if        %open(fivabwwr)
079900080925     C                   close     fivabwwr
080000070411     C                   endif
080100080925     C                   if        %open(fivatwwr)
080200080925     C                   close     fivatwwr
080300070411     C                   endif
080400080925     C                   if        %open(fivab02l)
080500080925     C                   close     fivab02l
080600080925     C                   endif
080700070411     C*
080800070411     C* Effettuo la chiamata al CLLE preposto
080900080925     C                   call(e)   'TITVVTC'
081000070411     C                   parm                    parccm
081100070411     C                   parm                    parmbr
081200070411     C                   parm      '2'           paropz
081300070411     C*
081400070411     C                   eval      wrkesito = '2'
081500070411     C*
081600070411     C                   seton                                        LR
081700070411     C*
081800070411     C                   ENDSR     '*CANCL'
081900070411     C***
082000070411
082100990910
082200000613     C     *inzsr        BEGSR
082300990910     C*
082400990910     C     *entry        plist
082500990920     C                   parm                    tivlrds
082600990921     C                   parm      wrkesito      esito
082700000724     C                   parm                    prmlit
082800000710     C                   parm                    prmfir
082900080925     C*
083000080925     C* FIVAB02L - completa
083100080925     C     KEYvab02_C    klist
083200080925     C                   kfld                    vabFGS
083300080925     C                   kfld                    vabCCM
083400080925     C                   kfld                    vabRMN
083500080925     C*
083600080925     C* FIVAT01L - completa
083700080925     C     KEYvat01_C    klist
083800080925     C                   kfld                    VATFGS
083900080925     C                   kfld                    VATCCM
084000080925     C                   kfld                    VATAAS
084100080925     C                   kfld                    VATLNP
084200080925     C                   kfld                    VATNRS
084300080925     C                   kfld                    VATNSP
084400080925     C                   kfld                    VATTRC
084500080925     C                   kfld                    VATNOT
084600000613     C*
084700000830     C* CALCOLA LA DATA CORRENTE
084800000830     C                   time                    wn14             14 0
084900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
085000070808     C                   movel     wn14          oracor            6 0          *ORA
085100000830     C                   z-add     wn8           g08dat
085200000830     C                   z-add     *zeros        g08inv
085300000830     C                   movel     '0'           g08err
085400000830     C                   call      'XSRDA8'
085500000830     C                   parm                    wlbda8
085600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
085700000830     C*
085800000613     C                   ENDSR
085900000613     C***
086000990908
086100070719     Otitvz8p   E            riepilogo         2
086200990915     O                                              'Upload via Internet'
086300990915     O                                           +1 'Traduzione TIVIN00R -'
086400080925     O                                           +1 'FIVABWWR/FIVATWWR'
086500010201     O                                           +1 'Report testate bolle'
086600990915     O          E            riepilogo   2
086700990915     O                       wrkdata
086800990915     O                       wrkora              +1
086900990915     O                       procname            +1
087000990915     O          E            riepilogo   2
087100990915     O                                              'Cliente..................:'
087200990915     O                       VABCCM        z     +1
087300990915     O          E            riepilogo   2
087400990920     O                                              'Riferimento Strategi.....:'
087500990920     O                       vlrhdl              +1
087600990915     O          E            riepilogo   2
087700990915     O                                              'Giusti...................:'
087800010201     O                       §CTROKVB      2   +  1
087900990915     O          E            riepilogo   2
088000990915     O                                              'Sbagliati e corretti.....:'
088100971022     O                       §CTRMO        2   +  1
088200990915     O          E            riepilogo   2
088300990915     O                                              'Sbagliati e scartati.....:'
088400971022     O                       §CTRNO        2   +  1
088500000613
088600070719     Otitvz8ps  E            testdett          2
088700000613     O                                              'Upload via Internet'
088800000613     O                                           +1 'Traduzione TIVIN00R -'
088900080925     O                                           +1 'FIVABWWR/FIVATWWR'
089000010201     O                                           +1 'Report testate bolle'
089100000616     O          E            testdett    3
089200000613     O                                           +2 'N° rec'
089300000613     O                                           +3 'Anteprima contenuto'
089400000616     O          E            rigadett    2
089500000613     O                       rrnum               +2
089600000621     O                       recko               +3
089700000616     O          E            findett     2
089800000613     O                       wrkdata
089900000613     O                       wrkora              +1
090000000613     O                       procname            +1
090100000616     O          E            findett     2
090200000613     O                                              'Cliente..................:'
090300000613     O                       VABCCM        z     +1
090400000616     O          E            findett     2
090500000613     O                                              'Riferimento Strategi.....:'
090600000613     O                       vlrhdl              +1
090700000616     O          E            findett     2
090800000613     O                                              'Giusti...................:'
090900010201     O                       §CTROKVB      2   +  1
091000000616     O          E            findett     2
091100000613     O                                              'Sbagliati e corretti.....:'
091200000613     O                       §CTRMO        2   +  1
091300000616     O          E            findett     2
091400000613     O                                              'Sbagliati e scartati.....:'
091500000613     O                       §CTRNO        2   +  1
091600000512** CMD - COMANDI CL
091700070719OVRPRTF FILE(TITVZ8P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
091800070719OVRPRTF FILE(TITVZ8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
091900070719DLTOVR FILE(TITVZ8P TITVZ8PS) LVL(*)
092000000512
092100000512
