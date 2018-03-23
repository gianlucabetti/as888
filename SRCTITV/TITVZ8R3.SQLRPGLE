000100080925      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081104     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA':'QC2LE') ACTGRP('BARTVAS')
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
001600081104     FFISGN01L  if   e           k disk    usropn
001700081104     F                                     prefix(w_)
001800081104     F                                     rename(fisgn000:fisgn001)
001900081104     F                                     extfile(LibFileSGN)
002000070719     Ftitvz8p   O    f  132        PRINTER usropn
002100000621     F                                     oflind(*inoa)
002200070411     F                                     infsr(*pssr)
002300070719     Ftitvz8ps  O    f  198        PRINTER usropn
002400000621     F                                     oflind(*inob)
002500070411     F                                     infsr(*pssr)
002600990908
002700000512     D*------------
002800000512     D* COMANDI
002900000512     D*------------
003000011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
003100000801     D*----------------------------------------------------
003200000801     D* DICHIARAZIOINE VARIABILI DI WRK
003300000801     D*----------------------------------------------------
003400990920     D dscmz         e ds                  inz
003500990910     D psds           sds
003600990910     D  procname         *PROC
003700990920     D tivlrds       e ds                  extname(tivlr00f)
003800070719     D tisi95ds      e ds
003900081104     D fnlv55ds      e ds
004000990910     D esito           s              1
004100000724     D prmlit          s             10
004200000710     D prmfir          s             10
004300990921     D wrkesito        s                   like(esito)
004400990915     D wrkdata         s               d
004500990915     D wrkora          s               t
004600000613     D rrnum           s              6  0 INZ(*zeros)
004700000621     D recko           s            150    INZ(*blanks)
004800011113     D depcmd          s            150    INZ(*blanks)
004900010202     D parccm          s              8    INZ(*blanks)
005000010202     D parmbr          s             10    INZ(*blanks)
005100010202     D paropz          s              1    INZ(*blanks)
005200010202     D chkcall         s              1    INZ(*blanks)
005300070919     D wPrimaBolla     s              1    INZ('0')
005400081029     D wCurrTypRec     s              2    INZ(*blanks)
005500080925     D LibFileVB       s             21A   inz
005600080925     D LibFileVT       s             21A   inz
005700081104     D LibFileSGN      s             21A   inz
005800080925     D currSysNeta     s              8A   inz('*NULL')
005900081104     D commandLib1     s             10A   inz
006000081104     D commandLib2     s             10A   inz
006100000830
006200000830     D*------------------
006300000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400000830     D*------------------
006500000830     D WLBDA8          DS                  INZ
006600000830     D  G08DAT                 1      8  0
006700000830     D  G08INV                 9     16  0
006800000830     D  G08ERR                17     17
006900000830     D  G08TGI                18     22  0
007000041025     D*------------------
007100041025     D* DS REPERIMENTO NUMERATORE
007200041025     D*------------------
007300041025     D trul33ds      e ds                  inz
007400041025     D*------------------
007500041025     D* DS ARCHITETTURA
007600041025     D*------------------
007700041025     D kpjba         e ds                  inz
007800041025     D*------------------
007900081029
008000081029     D*------------------
008100081029     D* DS "XSRDA8" - CONTROLLA DATA (8)
008200081029     D*------------------
008300081029     D WHOIAM_DS       DS                  INZ QUALIFIED
008400081029     D  BLPDATASPED                  16  0
008500081029     D  BLPAAS                        4  0
008600081029     D  BLPLNP                        3  0
008700081029     D  BLPNRS                        2  0
008800081029     D  BLPNSP                        7  0
008900081029     D  BLPRMN                       15  0
009000081029     D  BLPRMA                       15
009100081029     D  ARSFLS                        3  0
009200081029     D  ARSLNA                        3  0
009300081029     D  ARSNSC                        7  0
009400081029     D  ARSNOT                       35
009500081029     D  ARTNPP                        2  0
009600990908
009700080925     D*------------------
009800080925     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
009900080925     D*------------------
010000080925     D PiStr           S            100A   INZ
010100080925     D PiDecChr        S              1A   INZ
010200080925     D PiVal           S             63S30 INZ
010300080925     D PiInt           S               N   INZ
010400080925     D PiNum           S               N   INZ
010500080925
010600080925
010700080925     D*------------------
010800080925     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
010900080925     D*------------------
011000080925     D  PiOutSysNeta   S              8A
011100081104
011200081104
011300081104     D*------------------
011400081104     D* PASSAGGIO PARAMETRI A PROCEDURE SYSTEM
011500081104     D*------------------
011600081104     D system          PR            10I 0 extproc('system')
011700081104     D   command                       *   value options(*string)
011800081104     D
011900081104     D ErrMsgID        S              7A   Import('_EXCP_MSGID')
012000081104     D rc              S             10i 0
012100081104
012200081104
012300080925
012400080925
012500080925     D*------------------
012600080925     D* LINKING A DEFINIZIONI ESTERNE
012700080925     D*------------------
012800080925     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
012900080925     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
013000080925
013100080925
013200080925
013300080925
013400080925
013500081029
013600081029     C*
013700081029     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013800081029     C
013900081029     C/EXEC SQL
014000081029     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014100081029     C/END-EXEC
014200081029     C
014300081104     C*
014400080925     C* Reperisco il sistema AS/400 corrente
014500080925     C                   callp     UBRTVNETA_Rtv(currSysNeta)
014600080925     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
014700081104     C                   eval      LibFileVB   = 'FILTRAGRU/FIVAB02L'
014800081104     C                   eval      LibFileVT   = 'FILTRAGRU/FIVAT01L'
014900081104     C                   eval      LibFileSGN  = 'FILTRA201/FISGN01L'
015000081104     C                   eval      commandLib1 = 'FILTRAGRU'
015100081104     C                   eval      commandLib2 = 'FILTRA201'
015200080925     C                   else
015300081104     C                   eval      LibFileVB   = 'FILTRAGRPF/FIVAB02L'
015400081104     C                   eval      LibFileVT   = 'FILTRAGRPF/FIVAT01L'
015500081104     C                   eval      LibFileSGN  = 'FILTRAPRD/FISGN01L'
015600081104     C                   eval      commandLib1 = 'FILTRAGRPF'
015700081104     C                   eval      commandLib2 = 'FILTRAPRD'
015800080925     C                   endif
015900080925     C*
016000080925     C                   if        not %open(FIVAB02L)
016100080925     C                   open      FIVAB02L
016200080925     C                   endif
016300080925     C                   if        not %open(FIVAT01L)
016400080925     C                   open      FIVAT01L
016500080925     C                   endif
016600081104     C                   if        not %open(FISGN01L)
016700081104     C                   open      FISGN01L
016800081104     C                   endif
016900081104     C*
017000081104     C* Aggiungo in lista le librerie necessarie
017100081104      /free
017200081104
017300081104              rc = system('ADDLIBLE ' + commandLib1 + ' *LAST');
017400081104              rc = system('ADDLIBLE ' + commandLib2 + ' *LAST');
017500081104
017600081104         //   select;
017700081104         //   when rc = 0;
017800081104         //   when ErrMsgId = 'CPFxxxx';
017900081104         //   other;
018000081104         //   endsl;
018100081104
018200081104
018300081104      /end-free
018400081104     C*
018500081104     C* Reperisco il Terminal Partenza dal P.O. gestione
018600081104     C                   clear                   fnlv55ds
018700081104     C                   movel     'P'           d55tpt
018800081104     C                   z-add     vlrpoi        d55lin
018900081104     C                   z-add     datcor        d55drf
019000081104     C                   z-add     vlrpoi        d55lnp
019100081104     C                   call      'FNLV55R'
019200081104     C                   parm                    fnlv55ds
019300081104     C                   if        d55err = *blanks
019400081104     C                   z-add     d55tfp        w_sgnTNP
019500081104     C                   else
019600081104     C                   z-add     vlrpoi        w_sgnTNP
019700081104     C                   endif
019800080925     C*
019900990915     C                   time                    wrkdata
020000990915     C                   time                    wrkora
020100000913     C                   reset                   rrnum
020200990921     C                   reset                   esito
020300990921     C                   reset                   wrkesito
020400000724     C*
020500000724     C* SE OCCORRE SPEDIRE IN FILIALE
020600000724     C                   if        vlrpoi <> *zeros
020700000724     C*
020800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
020900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
021000000724     C     vlrpoi        chain     azorg01l
021100000724     C                   if        %found
021200000616     C                   movel(p)  CMD(1)        depcmd
021300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
021400000616     C*
021500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
021600011113     C                   Z-ADD     150           LENGH            15 5
021700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
021800000616     C                   PARM                    depcmd
021900000616     C                   PARM                    LENGH
022000000724     C*
022100000724     C                   endif
022200000724     C                   endif
022300000616     C*
022400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
022500000616     C                   movel(p)  CMD(2)        depcmd
022600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
022700000616     C*
022800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
022900011113     C                   Z-ADD     150           LENGH            15 5
023000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023100000616     C                   PARM                    depcmd
023200000616     C                   PARM                    LENGH
023300000616     C*
023400070719     C                   if        not %open(titvz8ps)
023500070719     C                   open      titvz8ps
023600000616     C                   except    testdett
023700000613     C                   endif
023800000613     C*
023900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
024000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
024100070719     C*
024200070719     C* Esegue lancio TISI95R solo x chiusura
024300070719     C                   CLEAR                   TISI95DS
024400070719     C                   EVAL      I95TLA = 'C'
024500070719     C                   CALL      'TISI95R'
024600070719     C                   PARM                    TISI95DS
024700000613     C*
024800010202     C* Effettuo la chiamata al CLLE preposto
024900080925     C                   call(e)   'TITVVTC'
025000010202     C                   parm                    parccm
025100010202     C                   parm                    parmbr
025200010202     C                   parm      '2'           paropz
025300080925     C*
025400080925     C                   if        %open(FIVAB02L)
025500080925     C                   close     FIVAB02L
025600080925     C                   endif
025700080925     C                   if        %open(FIVAT01L)
025800080925     C                   close     FIVAT01L
025900080925     C                   endif
026000081104     C                   if        %open(FISGN01L)
026100081104     C                   close     FISGN01L
026200081104     C                   endif
026300010202     C*
026400070719     C                   if        %open(titvz8ps)
026500000616     C                   except    findett
026600070719     C                   close     titvz8ps
026700000613     C                   endif
026800000616     C*
026900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
027000011113     C                   Z-ADD     150           LENGH            15 5
027100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027200000616     C                   PARM                    CMD(3)
027300000616     C                   PARM                    LENGH
027400000616     C*
027500010201     C                   seton                                        LR
027600000613
027700000613
027800000613     C*--------------------------------------------------------
027900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
028000000613     C*--------------------------------------------------------
028100000621     C     STPR          BEGSR
028200000613     C*
028300070719     C                   if        not %open(titvz8p)
028400070719     C                   open      titvz8p
028500990915     C                   endif
028600990915     C*
028700990915     C                   except    riepilogo
028800990915     C*
028900070719     C                   if        %open(titvz8p)
029000070719     C                   close     titvz8p
029100990914     C                   endif
029200990910     C*
029300000613     C                   ENDSR
029400000613     C***
029500990908
029600000801
029700910830     C*--------------------------------------------------------
029800080925     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
029900910830     C*--------------------------------------------------------
030000040526     C     RWFILE        BEGSR
030100990910     C*
030200990914     C                   if        not %open(tivin00r)
030300990908     C                   open      tivin00r
030400990914     C                   endif
030500080925     C                   if        not %open(fivabwwr)
030600080925     C                   open      fivabwwr
030700990914     C                   endif
030800080925     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
030900020305     C                   exsr      prevat
031000010201     C*
031100010202     C                   if        chkcall = '0'
031200010202     C*
031300080925     C                   if        not %open(fivatwwr)
031400080925     C                   open      fivatwwr
031500010201     C                   endif
031600990910     C*
031700010201     C                   clear                   §CTROKVB          5 0
031800020305     C                   clear                   §CTROKVT          5 0
031900000801     C                   clear                   §CTRMO            5 0
032000000801     C                   clear                   §CTRNO            5 0
032100070919     C*
032200070919     C* Inizializzazioni iniziali e formati record file Bartolini
032300070919     C                   EXSR      INZVAR
032400070919     C                   EXSR      DEFCAM
032500040910     C*
032600921023     C                   DO        *HIVAL
032700990913     C*
032800990915     C                   READ      tivin00r                               70
032900040910     C                   if        vindta > *blanks
033000000613     C                   add       1             rrnum
033100000801     C*
033200000801     C                   if        *in70 = *off
033300000801     C                             and
033400000801     C                             (vinflg = *blanks
033500000801     C                              or vinflg = '0'
033600000801     C                              or vinflg = '2')
033700000801     C*
033800000801     C                   clear                   vinmsg
033900000801     C                   eval      vinflg = '1'
034000040910     C*
034100040910     C* Eseguo routine d traduzione
034200040910     C                   exsr      impvabvat
034300040802     C*
034400010305     C                   endif
034500000905     C*
034600000905     C                   else
034700000905     C                   eval      vinflg = '1'
034800000905     C                   endif
034900000905     C*
035000000905     C  N70              update    tivin000
035100000905     C*
035200991022     C  N70              ENDdo
035300010202     C*
035400070919     C* Scarico l'ultima bolla rimasta in canna
035500070919     C                   IF        wPrimaBolla = '1'
035600080925     C                   exsr      wriVAB
035700070919     C                   ENDIF
035800070919     C*
035900010202     C                   endif
036000990910
036100990910     C* Se non ci sono record con errori ...
036200000710     C                   if        §ctrno = 0
036300990910     C* ... restituisco esito OK.
036400990921     C                   eval      wrkesito = '0'
036500990910     C                   else
036600010201     C                   if        §ctrokvb > 0
036700990921     C                   eval      wrkesito = '1'
036800000710     C                   else
036900000710     C                   eval      wrkesito = '2'
037000990910     C                   endif
037100000710     C                   endif
037200990910     C*
037300990914     C                   if        %open(tivin00r)
037400990908     C                   close     tivin00r
037500990914     C                   endif
037600080925     C                   if        %open(fivabwwr)
037700080925     C                   close     fivabwwr
037800990914     C                   endif
037900080925     C                   if        %open(fivatwwr)
038000080925     C                   close     fivatwwr
038100010201     C                   endif
038200990910     C*
038300010201     C                   if        §ctrokvb > 0
038400000724     C                             and vlrpoi <> *zeros
038500010202     C                   exsr      invio
038600990920     C                   endif
038700990920     C*
038800910830     C                   ENDSR
038900000613     C***
039000990920
039100000801     C*----------------------------------------------------*
039200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039300000801     C*----------------------------------------------------*
039400010201     C     INZVAR        BEGSR
039500000801     C*
039600040802     C                   Z-ADD     *zeros        Num5_0            5 0
039700040802     C                   MOVEL     '0'           FlgCAS            1
039800000801     C*
039900000801     C                   ENDSR
040000000801     C*----------------------------------------------------*
040100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
040200000801     C*----------------------------------------------------*
040300000801     C     DEFCAM        BEGSR
040400081104     C*
040500081104     C                   SETOFF                                       45
040600000801     C*
040700080925     C                   CLEAR                   FIVAB000
040800080925     C                   CLEAR                   FIVAT000
040900020619     C* Imposto i valori di default...
041000070801     C                   Z-ADD     0433996       VABCCM
041100070801     C                   Z-ADD     0433996       VATCCM
041200070801     C                   Z-ADD     043           VABLNP
041300070801     C                   Z-ADD     043           VATLNP
041400070531     C                   Z-ADD     000           VABCTR
041500080825     C                   MOVEL     '7Q'          VABCTM
041600040823     C                   MOVEL     '1'           VABCBO
041700020619     C* ... e poi verifico se sono stati passati come parametri
041800020619     C                   IF        vlrppt > *blanks
041900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
042000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
042100020619     C                   EXSR      CHKNUM
042200020619     C                   IF        PiInt=*on
042300020619     C                   Z-ADD     PiVal         VABCCM
042400020619     C                   Z-ADD     PiVal         VATCCM
042500020619     C                   ENDIF
042600040506     C                   ENDIF
042700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
042800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
042900020619     C                   EXSR      CHKNUM
043000020619     C                   IF        PiInt=*on
043100020619     C                   Z-ADD     PiVal         VABLNP
043200020619     C                   Z-ADD     PiVal         VATLNP
043300040506     C                   ENDIF
043400020619     C                   ENDIF
043500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
043600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
043700020619     C                   EXSR      CHKNUM
043800020619     C                   IF        PiInt=*on
043900020619     C                   Z-ADD     PiVal         VABCTR
044000040506     C                   ENDIF
044100020619     C                   ENDIF
044200060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
044300060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
044400060202     C                   ENDIF
044500081104     C                   IF        %subst(vlrppt:16:1) = 'T'
044600081104     C                   SETON                                        45
044700081104     C                   ENDIF
044800020619     C                   ENDIF
044900000801     C*
045000000801     C                   ENDSR
045100000801     C*----------------------------------------------------*
045200080925     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
045300000801     C*----------------------------------------------------*
045400040910     C     IMPVABVAT     BEGSR
045500040910     C*
045600040910     C* Traduzione relativa ai tipi record del file del cliente
045700040910     C*
045800070719     C* Resetto indicatore d anomalia sul singolo record
045900070719     C                   eval      vinflg = '1'
046000081029     C*
046100081029     C* Memorizzo ogni tipo-record corrente
046200081029     C                   EVAL      wCurrTypRec = %subst(vindta:1:2)
046300070808     C***
046400070808     C* ...tipo record '01' (testata)
046500081029     C                   IF        wCurrTypRec = '01'
046600081029     C* ......inizializzo l'indicatore che discrimina la scrittura della testata bolle
046700081029     C                   SETOFF                                       60
046800070919     C* ......verifico se è la prima bolla elaborata
046900070919     C                   IF        wPrimaBolla = '0'
047000070919     C                   EVAL      wPrimaBolla = '1'
047100070919     C                   ELSE
047200081104     C                   EXSR      WRIVAB
047300070919     C* ......inizializzazioni iniziali e formati record file Bartolini
047400070919     C                   EXSR      INZVAR
047500070919     C                   EXSR      DEFCAM
047600070919     C                   ENDIF
047700040928     C*
047800040928     C                   Z-ADD     *zeros        errore            1 0
047900040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
048000040928     C                   MOVEL     datcor        VABAAS
048100040928     C                   MOVEL     datcor        VATAAS
048200040928     C                   MOVE      datcor        VABMGS
048300040928     C                   MOVE(P)   vlrpoi        VABFGS
048400040928     C                   MOVE(P)   vlrpoi        VATFGS
048500040910     C* ......VABNSP/VATNSP
048600041025     C* NSP => Stacco un numeratore da AZNUM
048700070807     C*                  clear                   TRUL33DS
048800070807     C*                  eval      I33OPE = *zeros
048900070807     C*                  eval      I33CNU = 302
049000070807     C*                  eval      I33NUM = 1
049100070807     C*                  movel     TRUL33DS      KPJBU
049200070807     C*                  call      'TRUL33R'
049300070807     C*                  parm                    KPJBA
049400070807     C*                  movel     KPJBU         TRUL33DS
049500070807     C*                  if        O33ERR = *zeros
049600070807     C*                  z-add     O33NRF        VABNSP
049700070807     C*                  z-add     O33NRF        VATNSP
049800070807     C*                  else
049900070807     C*                  Z-ADD     1             errore
050000070807     C*                  EVAL      vinmsg = %trimr(vinmsg)
050100070807     C*                            + ' ' + 'VABNSP VATNSP'
050200070807     C*                  endif
050300070807     C* ......VABNSP/VATNSP
050400070807     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
050500070807     C                   EXSR      CHKNUM
050600070807     C                   IF        PiInt=*on
050700070807     C                   Z-ADD     PiVal         VABNSP
050800070807     C                   Z-ADD     PiVal         VATNSP
050900070807     C                   ELSE
051000070807     C                   ADD       1             errore
051100070807     C                   EVAL      vinmsg = %trimr(vinmsg)
051200070807     C                             + ' ' + 'VABNSP VATNSP'
051300070807     C                   ENDIF
051400060202     C* ......VABRMA
051500070719     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
051600060202     C* ......VABRMN
051700070917     C***                EVAL      PiStr=%trim(%subst(vindta:209:10))
051800070917     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
051900060202     C                   EXSR      CHKNUM
052000060202     C                   IF        PiInt=*on
052100060202     C                   Z-ADD     PiVal         VABRMN
052200060202     C                   ELSE
052300060202     C                   ADD       1             errore
052400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
052500060202     C                             + ' ' + 'VABRMN'
052600060202     C                   ENDIF
052700080926 xxx C* ......VABNCL
052800081008     C                   EVAL      PiStr=%trim(%subst(vindta:183:5))
052900081008     C                   EXSR      CHKNUM
053000081008     C                   IF        PiInt=*on
053100081008     C                   Z-ADD     PiVal         VABNCL
053200081008     C                   ELSE
053300081008     C                   ADD       1             errore
053400081008     C                   EVAL      vinmsg = %trimr(vinmsg)
053500081008     C                             + ' ' + 'VABNCL'
053600081008     C                   ENDIF
053700060202     C* ......VABPKB
053800070719     C                   EVAL      PiStr=%trim(%subst(vindta:188:12))
053900060202     C                   EXSR      CHKNUM
054000060202     C                   IF        PiNum=*on
054100070719     C                   Z-ADD     PiVal         VABPKB
054200060202     C                   ELSE
054300060202     C                   ADD       1             errore
054400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
054500060202     C                             + ' ' + 'VABPKB'
054600060202     C                   ENDIF
054700060202     C* ......VABVLB
054800070808     C                   IF        %subst(vindta:330:7) <> *blanks
054900070719     C                   EVAL      PiStr=%trim(%subst(vindta:330:7))
055000060202     C                   EXSR      CHKNUM
055100060202     C                   IF        PiNum=*on
055200070719     C                   Z-ADD     PiVal         VABVLB
055300060202     C                   ELSE
055400060202     C                   ADD       1             errore
055500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
055600060202     C                             + ' ' + 'VABVLB'
055700060202     C                   ENDIF
055800070808     C                   ENDIF
055900060202     C* ......VABRSD
056000070719     C                   EVAL      VABRSD=%trim(%subst(vindta:35:40))
056100060202     C* ......VABIND
056200070719     C                   EVAL      VABIND=%trim(%subst(vindta:75:35))
056300060202     C* ......VABLOD
056400070719     C                   EVAL      VABLOD=%trim(%subst(vindta:130:30))
056500081104     C* ......VABNZD
056600081104     C                   IF        %trim(%subst(vindta:435:3)) <> 'IT'
056700081104     C                   EVAL      VABNZD = %trim(%subst(vindta:435:3))
056800081104     C                   EVAL      VABCCM = 0434316
056900081104     C                   EVAL      VATCCM = 0434316
057000081104     C                   ENDIF
057100060202     C* ......VABCAD
057200070719     C                   EVAL      PiStr=%trim(%subst(vindta:125:5))
057300060202     C                   EXSR      CHKNUM
057400060202     C                   IF        PiInt=*on
057500060202     C                   Z-ADD     PiVal         Num5_0
057600060202     C                   MOVEL(P)  Num5_0        VABCAD
057700060202     C                   ELSE
057800060202     C                   ADD       1             errore
057900060202     C                   EVAL      vinmsg = %trimr(vinmsg)
058000060202     C                             + ' ' + 'VABCAD'
058100060202     C                   ENDIF
058200060202     C* ......VABPRD
058300070719     C                   EVAL      VABPRD=%trim(%subst(vindta:160:2))
058400070719     C* Reperisco la provincia dal CAP e dalla località
058500070719     C                   IF        VABPRD  = *blanks AND
058600070719     C                             VABCAD <> *blanks
058700070719     C                   CLEAR                   TISI95DS
058800070719     C                   EVAL      I95TCN = '3'
058900070719     C                   Z-ADD     datcor        I95DAT
059000070719     C                   EVAL      I95CAP = VABCAD
059100070719     C                   EVAL      I95LOC = VABLOD
059200070719     C                   CALL      'TISI95R'
059300070719     C                   PARM                    TISI95DS
059400070719     C                   EVAL      VABPRD = O95PRV
059500070719     C                   ENDIF
059600070719     C* ......VABNOT+VABNT2
059700070719     C                   MOVEL     *blanks       wNOTE            80
059800070719     C                   EVAL      wNOTE=%trim(%subst(vindta:219:80))
059900070719     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
060000070719     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:80-35))
060100070719     C* ......VABVCA
060200070719     C                   EVAL      VABVCA=%trim(%subst(vindta:299:3))
060300070719     C* ......VABCAS
060400070919     C                   IF        %subst(vindta:302:12)        <> *blanks AND
060500070919     C                             %trim(%subst(vindta:302:12)) <> '0,00'
060600070719     C                   EVAL      FlgCAS = '1'
060700070727     C                   EVAL      VABTIC = 'BM'
060800070719     C                   EVAL      PiStr=%trim(%subst(vindta:302:12))
060900070719     C                   EXSR      CHKNUM
061000070719     C                   IF        PiNum=*on
061100070719     C                   Z-ADD     PiVal         VABCAS
061200070719     C                   ELSE
061300070719     C                   ADD       1             errore
061400070719     C                   EVAL      vinmsg = %trimr(vinmsg)
061500070719     C                             + ' ' + 'VABCAS'
061600070719     C                   ENDIF
061700070719     C                   ENDIF
061800070719     C* ......VABVAS
061900070719     C                   EVAL      VABVAS=%trim(%subst(vindta:314:3))
062000070719     C* ......VABIAS
062100070719     C                   IF        %trim(%subst(vindta:317:12)) <> *blanks
062200070719     C                   EVAL      PiStr=%trim(%subst(vindta:317:12))
062300070719     C                   EXSR      CHKNUM
062400070719     C                   IF        PiNum=*on
062500070719     C                   Z-ADD     PiVal         VABIAS
062600070719     C                   ELSE
062700070719     C                   ADD       1             errore
062800070719     C                   EVAL      vinmsg = %trimr(vinmsg)
062900070719     C                             + ' ' + 'VABIAS'
063000070719     C                   ENDIF
063100070719     C                   ENDIF
063200070719     C* ......VATNOT "B" - Telefono destinatario
063300070719     C                   EVAL      VATNOT=%trim(%subst(vindta:110:15))
063400070719     C                   EVAL      VATTRC = 'B'
063500070719     C                   IF        VATNOT <> *blanks
063600080925     C                   EXSR      WRIVAT
063700070719     C                   ENDIF
063800060202     C*
063900060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
064000060202     C                   IF        FlgCAS <> '0'
064100060202     C                   IF        VABCBO = '1'
064200060202     C                   EVAL      VABCBO = '4'
064300060202     C                   ELSE
064400060202     C                   EVAL      VABCBO = '6'
064500060202     C                   ENDIF
064600060202     C                   ENDIF
064700060202     C*
064800060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
064900060202     C                   EXSR      CHKIMPDIV
065000040910     C*
065100040910     C                   ENDIF
065200040910     C***
065300070719     C* ...tipo record '02' (dettaglio barcode segnacooli)
065400081029     C                   IF        wCurrTypRec = '02'
065500070719     C* Resetto indicatore d anomalia sul singolo record
065600070719     C                   eval      vinflg = '1'
065700070719     C*
065800070719     C* Ciclo x tutti i 50 colli max previsti sul record
065900070719     C                   Z-ADD     19            wPosWho           3 0
066000070719     C                   Z-ADD     10            wLenWho           2 0
066100070719     C                   DOW       wPosWho <= %len(%trim(vindta))
066200040910     C* ......VATNOT
066300070719     C                   EVAL      VATNOT=%trim(%subst(vindta:wPosWho:wLenWho))
066400080926 xxx C***                EVAL      VATNOT=%subst(%trim(VATNOT):2)
066500040910     C                   EVAL      VATTRC = 'E'
066600080925     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
066700060202     C                   IF        VATNOT <> *blanks
066800081008 xxx C***                EVAL      vabNCL = vabNCL + 1
066900081029     C                   SETOFF                                       55
067000081104     C                   EXSR      chkWHOIAM
067100081029     C                   IF        not *IN55
067200081104     C                   EXSR      WRIVAT
067300081029     C                   ENDIF
067400060202     C                   ENDIF
067500070830     C                   EVAL      wPosWho = wPosWho + wLenWho
067600070719     C                   ENDDO
067700040910     C*
067800040910     C                   ENDIF
067900081029     C***
068000081029     C* ...tipo record '08' (dettaglio barcode segnacolli nn affidati)
068100081029     C                   IF        wCurrTypRec = '08'
068200081029     C* Resetto indicatore d anomalia sul singolo record
068300081029     C                   eval      vinflg = '1'
068400081029     C*
068500081029     C* Ciclo x tutti i 50 colli max previsti sul record
068600081029     C                   Z-ADD     19            wPosWho           3 0
068700081029     C                   Z-ADD     10            wLenWho           2 0
068800081029     C                   DOW       wPosWho <= %len(%trim(vindta))
068900081029     C* ......VATNOT
069000081029     C                   EVAL      VATNOT=%trim(%subst(vindta:wPosWho:wLenWho))
069100081029 xxx C***                EVAL      VATNOT=%subst(%trim(VATNOT):2)
069200081029     C                   EVAL      VATTRC = 'E'
069300081029     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
069400081029     C                   IF        VATNOT <> *blanks
069500081029 xxx C***                EVAL      vabNCL = vabNCL + 1
069600081029     C                   SETOFF                                       55
069700081104     C                   EXSR      chkWHOIAM
069800081029     C                   IF        not *IN55
069900081104     C                   EXSR      WRIVAT
070000081029     C                   ENDIF
070100081029     C                   ENDIF
070200081029     C                   EVAL      wPosWho = wPosWho + wLenWho
070300081029     C                   ENDDO
070400081029     C*
070500081029     C                   ENDIF
070600070919     C***
070700070919     C* ...tipo record '03' (estensione testata: note consegna)
070800081029     C                   IF        wCurrTypRec = '03'
070900070919     C                   MOVEL     *blanks       wNOTE2           70
071000070919     C                   EVAL      wNOTE2 = %trim(%subst(vindta:13))
071100070919     C                   EVAL      VABNOT = %subst(wNOTE2:1:35)
071200070919     C                   EVAL      VABNT2 = %subst(wNOTE2:36:35)
071300070919     C                   ENDIF
071400010202     C*
071500000801     C* Ebbene...
071600000801     C                   ADD       1             §CTRMO
071700010201     C                   IF        errore <> *zeros
071800000801     C                   ADD       1             §CTRNO
071900000801     C                   EVAL      recko = vindta
072000000801     C                   EXCEPT    rigadett
072100000801     C                   EVAL      vinflg = '2'
072200000801     C                   ELSE
072300010201     C                   ADD       1             §CTROKVB
072400000801     C                   ENDIF
072500000801     C*
072600000801     C                   ENDSR
072700080925
072800080925
072900080925     C*--------------------------------------------------------
073000080925     C* WRIVAB - EFFETTUA CONTROLLI D "NN DOPPIETA" E SCARICA IL BUFFER
073100080925     C*--------------------------------------------------------
073200080925     C     WRIVAB        BEGSR
073300081029     C*
073400081029     C* Inserisco la testata solo se efffettivamente inseriti suoi "chi sono"
073500081029     C                   IF        *IN60
073600080925     C*
073700080925     C* Verifico che stessa testata nn già presente
073800080925     C     KEYvab02_C    CHAIN     fivab02l
073900080925     C                   IF        %found(fivab02l)
074000080925     C                   IF        vlrpoi <> 789
074100080925     C                   EVAL      w_vabPKB = w_vabPKB + vabPKB
074200081008 xxx C***                EVAL      w_vabNCL = w_vabNCL + vabNCL
074300080925     C                   UPDATE    FIVAB002
074400080925     C                   ENDIF
074500080925     C                   ELSE
074600081104     C                   Z-ADD     vabFGS        savFGS            3 0
074700081104     C                   IF        *IN45
074800081104     C                   EVAL      vabFGS = 999
074900081104     C                   ENDIF
075000080925     C                   WRITE     FIVAB000
075100081104     C                   Z-ADD     savFGS        vabFGS
075200080925     C                   ENDIF
075300081029     C*
075400081029     C                   ENDIF
075500080925     C*
075600080925     C                   ENDSR
075700080925     C***
075800081029
075900081029
076000081029
076100081029     C*--------------------------------------------------------
076200081029     C* CHKWHOIAM - EFFETTUA CONTROLLI D "NN DOPPIETA" DEI SEGNACOLLI CLIENTE
076300081029     C*--------------------------------------------------------
076400081029     C     CHKWHOIAM     BEGSR
076500081029     C*
076600081029     C* Effettuo primo controllo sul FIVAT
076700081029     C     KEYvat01_C    setll     fivat01l
076800081029     C                   if        %equal(fivat01l)
076900081029     C                   seton                                        55
077000081029     C                   endif
077100081029     C*
077200081103     C* Effettuo secondo controllo sul FISGN
077300081029     C                   if        not *IN55
077400081104     C                   eval      w_sgnWHO = VATNOT
077500081104     C     KEYsgn01_C    setll     fisgn01l
077600081104     C                   if        %equal(fisgn01l)
077700081104     C     KEYsgn01_C    reade     fisgn01l
077800081104     C                   dow       not %eof(fisgn01l)
077900081104     C                   if        w_sgnKSC = VABCCM
078000081104     C                   seton                                        55
078100081104     C                   leave
078200081104     C                   endif
078300081104     C     KEYsgn01_C    reade     fisgn01l
078400081104     C                   enddo
078500081104     C                   endif
078600081029     C                   endif
078700081029     C*
078800081029     C* Se nn trovato segnacollo cliente sul FISGN => proseguo con il controllo
078900081029     C* sui file del "consolidato"
079000081029     C                   if        not *IN55
079100081029     C*
079200081029     C/EXEC SQL
079300081029     C+ declare C1 cursor for
079400081029     C+ select (blpaas*10000+blpmgs), blpaas, blplnp, blpnrs, blpnsp,
079500081029     C+ blprmn, blprma, art.arsfls, art.arslna, art.arsnsc,
079600081029     C+ art.arsnot, art.artnpp from fnblp00f
079700081029     C+ join (select artaas, artlnp, artnrs, artnsp, artnpp,
079800081029     C+ ars.arsfls, ars.arslna, ars.arsnsc, ars.arsnot from fnart00f
079900081029     C+ join (select arsfls, arslna, arsnrs, arsnsc, arstrc, arsnot
080000081103     C+ from fiars00f where arstrc='C' and arsnot=:VATNOT) as ARS on
080100081029     C+ artfls=arsfls and artlna=arslna and artnrs=arsnrs and
080200081029     C+ artnsc=arsnsc and 'C'=arstrc) as ART
080300081029     C+ on blpaas=artaas and blplnp=artlnp and blpnrs=artnrs and
080400081104     C+ blpnsp=artnsp where blpksc=:VABCCM
080500081029     C+ for read only
080600081029     C/END-EXEC
080700081029     C
080800081029     C/EXEC SQL
080900081029     C+ open C1
081000081029     C/END-EXEC
081100081029     C*
081200081029     C                   if        sqlcod = *zeros
081300081029     C                   seton                                        55
081400081029     C
081500081029     C/EXEC SQL
081600081029     C+ Fetch C1 into :WHOIAM_DS
081700081029     C/END-EXEC
081800081029     C                   if        sqlcod = *zeros
081900081029     C                   if        WHOIAM_DS.ARTNPP = 88 AND
082000081029     C                             wCurrTypRec = '02'
082100081029     C                   setoff                                       55
082200081029     C                   endif
082300081029     C                   endif
082400081029     C*
082500081029     C                   endif
082600081029     C*
082700081029     C*
082800081029     C/EXEC SQL
082900081029     C+ close C1
083000081029     C/END-EXEC
083100081029     C*
083200081029     C                   endif
083300081029     C*
083400081029     C                   ENDSR
083500081029     C***
083600081029
083700081029
083800080925     C*--------------------------------------------------------
083900081029     C* WRIVAT - SCARICA IL BUFFER SEGNACOLLI CLIENTE
084000080925     C*--------------------------------------------------------
084100080925     C     WRIVAT        BEGSR
084200081029     C*
084300081104     C                   if        vatTRC='E'
084400081029     C                   seton                                        60
084500081104     C                   endif
084600081104     C*
084700081104     C                   Z-ADD     vatFGS        savFGS
084800081104     C                   IF        *IN45
084900081104     C                   EVAL      vatFGS = 999
085000081104     C                   ENDIF
085100080925     C                   WRITE     FIVAT000
085200081104     C                   Z-ADD     savFGS        vatFGS
085300080925     C*
085400080925     C                   ENDSR
085500080925     C***
085600080925
085700080925
085800010202     C*----------------------------------------------------*
085900080925     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
086000010202     C*----------------------------------------------------*
086100020305     C     PREVAT        BEGSR
086200010202     C*
086300080925     C* Compongo il nome del membro da dare al FIVATWWR
086400010202     C                   eval      parmbr = vlrhdl
086500010202     C                   movel     'M'           parmbr
086600060113     C                   eval      parccm = vlrksc
086700010202     C                   eval      paropz = '1'
086800010202     C* Effettuo la chiamata al CLLE preposto
086900080925     C                   call(e)   'TITVVTC'
087000010202     C                   parm                    parccm
087100010202     C                   parm                    parmbr
087200010202     C                   parm                    paropz
087300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
087400010202     C                   if        %error
087500010202     C                   movel     '1'           chkcall
087600010202     C                   else
087700010202     C                   movel     '0'           chkcall
087800010202     C                   endif
087900010202     C*
088000010202     C                   ENDSR
088100000801     C*----------------------------------------------------*
088200000801     C*  CONTROLLO NUMERICITA' CAMPI
088300000801     C*----------------------------------------------------*
088400000801     C     CHKNUM        BEGSR
088500000801     C*
088600080925     C                   IF        PiDecChr = *blanks
088700080925     C                   EVAL      PiDecChr = ','
088800080925     C                   ENDIF
088900080925     C*
089000080925     C                   callp     UBISNUM_Check(PiStr
089100080925     C                                          :PiDecChr
089200080925     C                                          :PiVal
089300080925     C                                          :PiNum
089400080925     C                                          :PiInt)
089500000801     C*
089600000801     C                   ENDSR
089700000801     C***
089800000801
089900011113
090000011113     C*----------------------------------------------------*
090100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
090200011113     C*----------------------------------------------------*
090300011113     C     CHKIMPDIV     BEGSR
090400011113     C*
090500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
090600011113     C                   Z-ADD     *zeros        wrkDec            9 9
090700011113     C*
090800011113     C* Come prima cosa effettuo considerazioni sulla divisa
090900011113     C                   IF        vabIAS > *zeros
091000011113     C                   IF        vabVAS <> 'EUR'
091100011113     C                   EVAL      vabVAS =  'ITL'
091200011113     C                   ENDIF
091300011113     C                   ENDIF
091400011113     C*
091500011113     C                   IF        vabCAS > *zeros
091600011113     C                   IF        vabVCA <> 'EUR'
091700011113     C                   EVAL      vabVCA =  'ITL'
091800011113     C                   ENDIF
091900011113     C                   ENDIF
092000011113     C*
092100011113     C                   IF        vabVMD > *zeros
092200020305     C                   IF        vabVAD <> 'EUR'
092300011113     C                   EVAL      vabVAD =  'ITL'
092400011113     C                   ENDIF
092500011113     C                   ENDIF
092600011113     C*
092700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
092800011113     C                   Z-ADD     vabIAS        wrkDec
092900011113     C                   IF        wrkDec > *zeros
093000011113     C                   IF        vabVAS = 'ITL'
093100011113     C                   EVAL      vabIAS = *zeros
093200011113     C                   ENDIF
093300011113     C                   ENDIF
093400011113     C*
093500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
093600011113     C                   Z-ADD     vabCAS        wrkDec
093700011113     C                   IF        wrkDec > *zeros
093800011113     C                   IF        vabVCA = 'ITL'
093900011113     C                   EVAL      vabCAS = *zeros
094000011113     C                   ENDIF
094100011113     C                   ENDIF
094200011113     C*
094300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
094400011113     C                   Z-ADD     vabVMD        wrkDec
094500011113     C                   IF        wrkDec > *zeros
094600011113     C                   IF        vabVAD = 'ITL'
094700011113     C                   EVAL      vabVMD = *zeros
094800011113     C                   ENDIF
094900011113     C                   ENDIF
095000011113     C*
095100011113     C                   ENDSR
095200011113     C***
095300011113
095400011113
095500000801
095600000801
095700990920      /TITLE Invio dei dati al punto operativo.
095800010202     C     invio         BEGSR
095900990920     C*
096000080925     C* 1° invio FIVAT
096100010201     C                   reset                   dscmz
096200010201     C                   move      vlrpoi        cmzdst
096300080925     C                   eval      cmzfld = 'FIVATWWR'
096400010201     C                   eval      cmzmbd = vlrhdl
096500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
096600021009     C***                if        prmfir = *blanks
096700080925     C                   eval      cmzfla = 'FIVAT00F'
096800080925     C                   eval      cmzmba = 'FIVAT00F'
096900021009     C***                else
097000021009     C***                eval      cmzfla = prmfir
097100021009     C***                eval      cmzmba = prmfir
097200021009     C***                endif
097300010201     C                   eval      cmznrr = *zeros
097400020305     C                   move      §ctrokvt      cmznrr
097500021018     C                   eval      cmzlba = vlrfl1
097600010201     C                   call(e)   'TIS711C'
097700010201     C                   parm                    dscmz
097800010201     C                   parm      *blanks       esito
097900010205     C                   if        %error
098000010205     C                             or cmzerr = '1'
098100010205     C                             or esito  = '1'
098200010205     C                   eval      wrkesito = '3'
098300010205     C                   else
098400010201     C*
098500080925     C* 2° invio FIVAB
098600010201     C                   reset                   dscmz
098700010201     C                   move      vlrpoi        cmzdst
098800010201     C                   eval      cmzfld = vlrfou
098900010201     C                   eval      cmzmbd = vlrhdl
099000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
099100021009     C***                if        prmfir = *blanks
099200080925     C                   eval      cmzfla = 'FIVAB00F'
099300080925     C                   eval      cmzmba = 'FIVAB00F'
099400021009     C***                else
099500021009     C***                eval      cmzfla = prmfir
099600021009     C***                eval      cmzmba = prmfir
099700021009     C***                endif
099800010201     C                   eval      cmznrr = *zeros
099900010201     C                   move      §ctrokvb      cmznrr
100000021018     C                   eval      cmzlba = vlrfl1
100100010201     C                   call(e)   'TIS711C'
100200010201     C                   parm                    dscmz
100300010201     C                   parm      *blanks       esito
100400010201     C                   if        %error
100500010201     C                             or cmzerr = '1'
100600010201     C                             or esito  = '1'
100700010201     C                   eval      wrkesito = '3'
100800010201     C                   endif
100900010205     C                   endif
101000990920     C*
101100000613     C                   ENDSR
101200000613     C***
101300070411
101400070411     C     *pssr         BEGSR
101500070411     C*
101600070411     C                   if        %open(tivin00r)
101700070411     C                   close     tivin00r
101800070411     C                   endif
101900080925     C                   if        %open(fivabwwr)
102000080925     C                   close     fivabwwr
102100070411     C                   endif
102200080925     C                   if        %open(fivatwwr)
102300080925     C                   close     fivatwwr
102400070411     C                   endif
102500080925     C                   if        %open(fivab02l)
102600080925     C                   close     fivab02l
102700080925     C                   endif
102800070411     C*
102900070411     C* Effettuo la chiamata al CLLE preposto
103000080925     C                   call(e)   'TITVVTC'
103100070411     C                   parm                    parccm
103200070411     C                   parm                    parmbr
103300070411     C                   parm      '2'           paropz
103400070411     C*
103500070411     C                   eval      wrkesito = '2'
103600070411     C*
103700070411     C                   seton                                        LR
103800070411     C*
103900070411     C                   ENDSR     '*CANCL'
104000070411     C***
104100070411
104200990910
104300000613     C     *inzsr        BEGSR
104400990910     C*
104500990910     C     *entry        plist
104600990920     C                   parm                    tivlrds
104700990921     C                   parm      wrkesito      esito
104800000724     C                   parm                    prmlit
104900000710     C                   parm                    prmfir
105000081104     C*
105100081104     C* FISGN01L - completa
105200081104     C     KEYsgn01_C    klist
105300081104     C                   kfld                    w_sgnTNP
105400081104     C                   kfld                    w_sgnWHO
105500080925     C*
105600080925     C* FIVAB02L - completa
105700080925     C     KEYvab02_C    klist
105800080925     C                   kfld                    vabFGS
105900080925     C                   kfld                    vabCCM
106000080925     C                   kfld                    vabRMN
106100080925     C*
106200080925     C* FIVAT01L - completa
106300080925     C     KEYvat01_C    klist
106400080925     C                   kfld                    VATFGS
106500080925     C                   kfld                    VATCCM
106600080925     C                   kfld                    VATAAS
106700080925     C                   kfld                    VATLNP
106800080925     C                   kfld                    VATNRS
106900080925     C                   kfld                    VATNSP
107000080925     C                   kfld                    VATTRC
107100080925     C                   kfld                    VATNOT
107200000613     C*
107300000830     C* CALCOLA LA DATA CORRENTE
107400000830     C                   time                    wn14             14 0
107500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
107600070808     C                   movel     wn14          oracor            6 0          *ORA
107700000830     C                   z-add     wn8           g08dat
107800000830     C                   z-add     *zeros        g08inv
107900000830     C                   movel     '0'           g08err
108000000830     C                   call      'XSRDA8'
108100000830     C                   parm                    wlbda8
108200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
108300000830     C*
108400000613     C                   ENDSR
108500000613     C***
108600990908
108700070719     Otitvz8p   E            riepilogo         2
108800990915     O                                              'Upload via Internet'
108900990915     O                                           +1 'Traduzione TIVIN00R -'
109000080925     O                                           +1 'FIVABWWR/FIVATWWR'
109100010201     O                                           +1 'Report testate bolle'
109200990915     O          E            riepilogo   2
109300990915     O                       wrkdata
109400990915     O                       wrkora              +1
109500990915     O                       procname            +1
109600990915     O          E            riepilogo   2
109700990915     O                                              'Cliente..................:'
109800990915     O                       VABCCM        z     +1
109900990915     O          E            riepilogo   2
110000990920     O                                              'Riferimento Strategi.....:'
110100990920     O                       vlrhdl              +1
110200990915     O          E            riepilogo   2
110300990915     O                                              'Giusti...................:'
110400010201     O                       §CTROKVB      2   +  1
110500990915     O          E            riepilogo   2
110600990915     O                                              'Sbagliati e corretti.....:'
110700971022     O                       §CTRMO        2   +  1
110800990915     O          E            riepilogo   2
110900990915     O                                              'Sbagliati e scartati.....:'
111000971022     O                       §CTRNO        2   +  1
111100000613
111200070719     Otitvz8ps  E            testdett          2
111300000613     O                                              'Upload via Internet'
111400000613     O                                           +1 'Traduzione TIVIN00R -'
111500080925     O                                           +1 'FIVABWWR/FIVATWWR'
111600010201     O                                           +1 'Report testate bolle'
111700000616     O          E            testdett    3
111800000613     O                                           +2 'N° rec'
111900000613     O                                           +3 'Anteprima contenuto'
112000000616     O          E            rigadett    2
112100000613     O                       rrnum               +2
112200000621     O                       recko               +3
112300000616     O          E            findett     2
112400000613     O                       wrkdata
112500000613     O                       wrkora              +1
112600000613     O                       procname            +1
112700000616     O          E            findett     2
112800000613     O                                              'Cliente..................:'
112900000613     O                       VABCCM        z     +1
113000000616     O          E            findett     2
113100000613     O                                              'Riferimento Strategi.....:'
113200000613     O                       vlrhdl              +1
113300000616     O          E            findett     2
113400000613     O                                              'Giusti...................:'
113500010201     O                       §CTROKVB      2   +  1
113600000616     O          E            findett     2
113700000613     O                                              'Sbagliati e corretti.....:'
113800000613     O                       §CTRMO        2   +  1
113900000616     O          E            findett     2
114000000613     O                                              'Sbagliati e scartati.....:'
114100000613     O                       §CTRNO        2   +  1
114200000512** CMD - COMANDI CL
114300070719OVRPRTF FILE(TITVZ8P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
114400070719OVRPRTF FILE(TITVZ8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
114500070719DLTOVR FILE(TITVZ8P TITVZ8PS) LVL(*)
114600000512
114700000512
