000100070808      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600070808     FEDIVABwr  O    E             DISK    usropn
000700070808     FEDIVATwr  O    E             DISK    usropn
000800070719     Ftitvz8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070719     Ftitvz8ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070719     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000070906     D wNomeFile       s             30    INZ(*blanks)
004100070906     D wCMR            s             35    INZ(*blanks)
004200070906     D wPosDaA         s              2    INZ(*blanks)
004300070906     D wPosDa          s              2  0 INZ(*zeros)
004400070906     D wLungA          s              2    INZ(*blanks)
004500070906     D wLung           s              2  0 INZ(*zeros)
004600070919     D wPrimaBolla     s              1    INZ('0')
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
005900041025     D trul33ds      e ds                  inz
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
008000000724     C                   if        %found
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
010300070719     C                   if        not %open(titvz8ps)
010400070719     C                   open      titvz8ps
010500000616     C                   except    testdett
010600000613     C                   endif
010700000613     C*
010800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011000070719     C*
011100070719     C* Esegue lancio TISI95R solo x chiusura
011200070719     C                   CLEAR                   TISI95DS
011300070719     C                   EVAL      I95TLA = 'C'
011400070719     C                   CALL      'TISI95R'
011500070719     C                   PARM                    TISI95DS
011600000613     C*
011700010202     C* Effettuo la chiamata al CLLE preposto
011800070808     C                   call(e)   'TITVEVTC'
011900010202     C                   parm                    parccm
012000010202     C                   parm                    parmbr
012100010202     C                   parm      '2'           paropz
012200010202     C*
012300070719     C                   if        %open(titvz8ps)
012400000616     C                   except    findett
012500070719     C                   close     titvz8ps
012600000613     C                   endif
012700000616     C*
012800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    CMD(3)
013200000616     C                   PARM                    LENGH
013300000616     C*
013400010201     C                   seton                                        LR
013500000613
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200070719     C                   if        not %open(titvz8p)
014300070719     C                   open      titvz8p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800070719     C                   if        %open(titvz8p)
014900070719     C                   close     titvz8p
015000990914     C                   endif
015100990910     C*
015200000613     C                   ENDSR
015300000613     C***
015400990908
015500000801
015600910830     C*--------------------------------------------------------
015700070808     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
015800910830     C*--------------------------------------------------------
015900040526     C     RWFILE        BEGSR
016000990910     C*
016100990914     C                   if        not %open(tivin00r)
016200990908     C                   open      tivin00r
016300990914     C                   endif
016400070808     C                   if        not %open(edivabwr)
016500070808     C                   open      edivabwr
016600990914     C                   endif
016700070808     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016800020305     C                   exsr      prevat
016900010201     C*
017000010202     C                   if        chkcall = '0'
017100010202     C*
017200070808     C                   if        not %open(edivatwr)
017300070808     C                   open      edivatwr
017400010201     C                   endif
017500990910     C*
017600010201     C                   clear                   §CTROKVB          5 0
017700020305     C                   clear                   §CTROKVT          5 0
017800000801     C                   clear                   §CTRMO            5 0
017900000801     C                   clear                   §CTRNO            5 0
018000070919     C*
018100070919     C* Inizializzazioni iniziali e formati record file Bartolini
018200070919     C                   EXSR      INZVAR
018300070919     C                   EXSR      DEFCAM
018400040910     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800040910     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000000801     C*
019100000801     C                   if        *in70 = *off
019200000801     C                             and
019300000801     C                             (vinflg = *blanks
019400000801     C                              or vinflg = '0'
019500000801     C                              or vinflg = '2')
019600000801     C*
019700000801     C                   clear                   vinmsg
019800000801     C                   eval      vinflg = '1'
019900040910     C*
020000040910     C* Eseguo routine d traduzione
020100040910     C                   exsr      impvabvat
020200040802     C*
020300010305     C                   endif
020400000905     C*
020500000905     C                   else
020600000905     C                   eval      vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900000905     C  N70              update    tivin000
021000000905     C*
021100991022     C  N70              ENDdo
021200010202     C*
021300070919     C* Scarico l'ultima bolla rimasta in canna
021400070919     C                   IF        wPrimaBolla = '1'
021500070919     C                   WRITE     EDIVAB00
021600070919     C                   ENDIF
021700070919     C*
021800010202     C                   endif
021900990910
022000990910     C* Se non ci sono record con errori ...
022100000710     C                   if        §ctrno = 0
022200990910     C* ... restituisco esito OK.
022300990921     C                   eval      wrkesito = '0'
022400990910     C                   else
022500010201     C                   if        §ctrokvb > 0
022600990921     C                   eval      wrkesito = '1'
022700000710     C                   else
022800000710     C                   eval      wrkesito = '2'
022900990910     C                   endif
023000000710     C                   endif
023100990910     C*
023200990914     C                   if        %open(tivin00r)
023300990908     C                   close     tivin00r
023400990914     C                   endif
023500070808     C                   if        %open(edivabwr)
023600070808     C                   close     edivabwr
023700990914     C                   endif
023800070808     C                   if        %open(edivatwr)
023900070808     C                   close     edivatwr
024000010201     C                   endif
024100990910     C*
024200010201     C                   if        §ctrokvb > 0
024300000724     C                             and vlrpoi <> *zeros
024400010202     C                   exsr      invio
024500990920     C                   endif
024600990920     C*
024700910830     C                   ENDSR
024800000613     C***
024900990920
025000000801     C*----------------------------------------------------*
025100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025200000801     C*----------------------------------------------------*
025300010201     C     INZVAR        BEGSR
025400000801     C*
025500040802     C                   Z-ADD     *zeros        Num5_0            5 0
025600040802     C                   MOVEL     '0'           FlgCAS            1
025700000801     C*
025800000801     C                   ENDSR
025900000801     C*----------------------------------------------------*
026000040910     C*  IMPOSTAZIONE CAMPI COSTANTI
026100000801     C*----------------------------------------------------*
026200000801     C     DEFCAM        BEGSR
026300000801     C*
026400070808     C                   CLEAR                   EDIVAB00
026500070808     C                   CLEAR                   EDIVAT00
026600020619     C* Imposto i valori di default...
026700070801     C                   Z-ADD     0433996       VABCCM
026800070801     C                   Z-ADD     0433996       VATCCM
026900070801     C                   Z-ADD     043           VABLNP
027000070801     C                   Z-ADD     043           VATLNP
027100070531     C                   Z-ADD     000           VABCTR
027200080825     C                   MOVEL     '7Q'          VABCTM
027300040823     C                   MOVEL     '1'           VABCBO
027400020619     C* ... e poi verifico se sono stati passati come parametri
027500020619     C                   IF        vlrppt > *blanks
027600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027800020619     C                   EXSR      CHKNUM
027900020619     C                   IF        PiInt=*on
028000020619     C                   Z-ADD     PiVal         VABCCM
028100020619     C                   Z-ADD     PiVal         VATCCM
028200020619     C                   ENDIF
028300040506     C                   ENDIF
028400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028600020619     C                   EXSR      CHKNUM
028700020619     C                   IF        PiInt=*on
028800020619     C                   Z-ADD     PiVal         VABLNP
028900020619     C                   Z-ADD     PiVal         VATLNP
029000040506     C                   ENDIF
029100020619     C                   ENDIF
029200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029400020619     C                   EXSR      CHKNUM
029500020619     C                   IF        PiInt=*on
029600020619     C                   Z-ADD     PiVal         VABCTR
029700040506     C                   ENDIF
029800020619     C                   ENDIF
029900060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
030000060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
030100060202     C                   ENDIF
030200020619     C                   ENDIF
030300000801     C*
030400000801     C                   ENDSR
030500000801     C*----------------------------------------------------*
030600070808     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
030700000801     C*----------------------------------------------------*
030800040910     C     IMPVABVAT     BEGSR
030900040910     C*
031000040910     C* Traduzione relativa ai tipi record del file del cliente
031100040910     C*
031200070719     C* Resetto indicatore d anomalia sul singolo record
031300070719     C                   eval      vinflg = '1'
031400070808     C***
031500070808     C* ...tipo record '01' (testata)
031600070808     C                   IF        %subst(vindta:1:2) = '01'
031700070919     C* ......verifico se è la prima bolla elaborata
031800070919     C                   IF        wPrimaBolla = '0'
031900070919     C                   EVAL      wPrimaBolla = '1'
032000070919     C                   ELSE
032100070919     C                   WRITE     EDIVAB00
032200070919     C* ......inizializzazioni iniziali e formati record file Bartolini
032300070919     C                   EXSR      INZVAR
032400070919     C                   EXSR      DEFCAM
032500070919     C                   ENDIF
032600040928     C*
032700040928     C                   Z-ADD     *zeros        errore            1 0
032800040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
032900040928     C                   MOVEL     datcor        VABAAS
033000040928     C                   MOVEL     datcor        VATAAS
033100040928     C                   MOVE      datcor        VABMGS
033200040928     C                   MOVE(P)   vlrpoi        VABFGS
033300040928     C                   MOVE(P)   vlrpoi        VATFGS
033400040910     C* ......VABNSP/VATNSP
033500041025     C* NSP => Stacco un numeratore da AZNUM
033600070807     C*                  clear                   TRUL33DS
033700070807     C*                  eval      I33OPE = *zeros
033800070807     C*                  eval      I33CNU = 302
033900070807     C*                  eval      I33NUM = 1
034000070807     C*                  movel     TRUL33DS      KPJBU
034100070807     C*                  call      'TRUL33R'
034200070807     C*                  parm                    KPJBA
034300070807     C*                  movel     KPJBU         TRUL33DS
034400070807     C*                  if        O33ERR = *zeros
034500070807     C*                  z-add     O33NRF        VABNSP
034600070807     C*                  z-add     O33NRF        VATNSP
034700070807     C*                  else
034800070807     C*                  Z-ADD     1             errore
034900070807     C*                  EVAL      vinmsg = %trimr(vinmsg)
035000070807     C*                            + ' ' + 'VABNSP VATNSP'
035100070807     C*                  endif
035200070807     C* ......VABNSP/VATNSP
035300070807     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
035400070807     C                   EXSR      CHKNUM
035500070807     C                   IF        PiInt=*on
035600070807     C                   Z-ADD     PiVal         VABNSP
035700070807     C                   Z-ADD     PiVal         VATNSP
035800070807     C                   ELSE
035900070807     C                   ADD       1             errore
036000070807     C                   EVAL      vinmsg = %trimr(vinmsg)
036100070807     C                             + ' ' + 'VABNSP VATNSP'
036200070807     C                   ENDIF
036300060202     C* ......VABRMA
036400070719     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
036500060202     C* ......VABRMN
036600070917     C***                EVAL      PiStr=%trim(%subst(vindta:209:10))
036700070917     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
036800060202     C                   EXSR      CHKNUM
036900060202     C                   IF        PiInt=*on
037000060202     C                   Z-ADD     PiVal         VABRMN
037100060202     C                   ELSE
037200060202     C                   ADD       1             errore
037300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
037400060202     C                             + ' ' + 'VABRMN'
037500060202     C                   ENDIF
037600060202     C* ......VABNCL
037700070719     C                   EVAL      PiStr=%trim(%subst(vindta:183:5))
037800060202     C                   EXSR      CHKNUM
037900060202     C                   IF        PiInt=*on
038000060202     C                   Z-ADD     PiVal         VABNCL
038100060202     C                   ELSE
038200060202     C                   ADD       1             errore
038300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
038400060202     C                             + ' ' + 'VABNCL'
038500060202     C                   ENDIF
038600060202     C* ......VABPKB
038700070719     C                   EVAL      PiStr=%trim(%subst(vindta:188:12))
038800060202     C                   EXSR      CHKNUM
038900060202     C                   IF        PiNum=*on
039000070719     C                   Z-ADD     PiVal         VABPKB
039100060202     C                   ELSE
039200060202     C                   ADD       1             errore
039300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
039400060202     C                             + ' ' + 'VABPKB'
039500060202     C                   ENDIF
039600060202     C* ......VABVLB
039700070808     C                   IF        %subst(vindta:330:7) <> *blanks
039800070719     C                   EVAL      PiStr=%trim(%subst(vindta:330:7))
039900060202     C                   EXSR      CHKNUM
040000060202     C                   IF        PiNum=*on
040100070719     C                   Z-ADD     PiVal         VABVLB
040200060202     C                   ELSE
040300060202     C                   ADD       1             errore
040400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
040500060202     C                             + ' ' + 'VABVLB'
040600060202     C                   ENDIF
040700070808     C                   ENDIF
040800060202     C* ......VABRSD
040900070719     C                   EVAL      VABRSD=%trim(%subst(vindta:35:40))
041000060202     C* ......VABIND
041100070719     C                   EVAL      VABIND=%trim(%subst(vindta:75:35))
041200060202     C* ......VABLOD
041300070719     C                   EVAL      VABLOD=%trim(%subst(vindta:130:30))
041400060202     C* ......VABCAD
041500070719     C                   EVAL      PiStr=%trim(%subst(vindta:125:5))
041600060202     C                   EXSR      CHKNUM
041700060202     C                   IF        PiInt=*on
041800060202     C                   Z-ADD     PiVal         Num5_0
041900060202     C                   MOVEL(P)  Num5_0        VABCAD
042000060202     C                   ELSE
042100060202     C                   ADD       1             errore
042200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
042300060202     C                             + ' ' + 'VABCAD'
042400060202     C                   ENDIF
042500060202     C* ......VABPRD
042600070719     C                   EVAL      VABPRD=%trim(%subst(vindta:160:2))
042700070719     C* Reperisco la provincia dal CAP e dalla località
042800070719     C                   IF        VABPRD  = *blanks AND
042900070719     C                             VABCAD <> *blanks
043000070719     C                   CLEAR                   TISI95DS
043100070719     C                   EVAL      I95TCN = '3'
043200070719     C                   Z-ADD     datcor        I95DAT
043300070719     C                   EVAL      I95CAP = VABCAD
043400070719     C                   EVAL      I95LOC = VABLOD
043500070719     C                   CALL      'TISI95R'
043600070719     C                   PARM                    TISI95DS
043700070719     C                   EVAL      VABPRD = O95PRV
043800070719     C                   ENDIF
043900070719     C* ......VABNOT+VABNT2
044000070719     C                   MOVEL     *blanks       wNOTE            80
044100070719     C                   EVAL      wNOTE=%trim(%subst(vindta:219:80))
044200070719     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
044300070719     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:80-35))
044400070719     C* ......VABVCA
044500070719     C                   EVAL      VABVCA=%trim(%subst(vindta:299:3))
044600070719     C* ......VABCAS
044700070919     C                   IF        %subst(vindta:302:12)        <> *blanks AND
044800070919     C                             %trim(%subst(vindta:302:12)) <> '0,00'
044900070719     C                   EVAL      FlgCAS = '1'
045000070727     C                   EVAL      VABTIC = 'BM'
045100070719     C                   EVAL      PiStr=%trim(%subst(vindta:302:12))
045200070719     C                   EXSR      CHKNUM
045300070719     C                   IF        PiNum=*on
045400070719     C                   Z-ADD     PiVal         VABCAS
045500070719     C                   ELSE
045600070719     C                   ADD       1             errore
045700070719     C                   EVAL      vinmsg = %trimr(vinmsg)
045800070719     C                             + ' ' + 'VABCAS'
045900070719     C                   ENDIF
046000070719     C                   ENDIF
046100070719     C* ......VABVAS
046200070719     C                   EVAL      VABVAS=%trim(%subst(vindta:314:3))
046300070719     C* ......VABIAS
046400070719     C                   IF        %trim(%subst(vindta:317:12)) <> *blanks
046500070719     C                   EVAL      PiStr=%trim(%subst(vindta:317:12))
046600070719     C                   EXSR      CHKNUM
046700070719     C                   IF        PiNum=*on
046800070719     C                   Z-ADD     PiVal         VABIAS
046900070719     C                   ELSE
047000070719     C                   ADD       1             errore
047100070719     C                   EVAL      vinmsg = %trimr(vinmsg)
047200070719     C                             + ' ' + 'VABIAS'
047300070719     C                   ENDIF
047400070719     C                   ENDIF
047500070719     C* ......VATNOT "B" - Telefono destinatario
047600070719     C                   EVAL      VATNOT=%trim(%subst(vindta:110:15))
047700070719     C                   EVAL      VATTRC = 'B'
047800070719     C                   IF        VATNOT <> *blanks
047900070808     C                   EXSR      VALVATCMR
048000070808     C                   WRITE     EDIVAT00
048100070719     C                   ENDIF
048200060202     C*
048300060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
048400060202     C                   IF        FlgCAS <> '0'
048500060202     C                   IF        VABCBO = '1'
048600060202     C                   EVAL      VABCBO = '4'
048700060202     C                   ELSE
048800060202     C                   EVAL      VABCBO = '6'
048900060202     C                   ENDIF
049000060202     C                   ENDIF
049100060202     C*
049200060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049300060202     C                   EXSR      CHKIMPDIV
049400060202     C* ...e in fine...
049500070808     C                   EXSR      VALVABCMR
049600040910     C*
049700040910     C                   ENDIF
049800040910     C***
049900070719     C* ...tipo record '02' (dettaglio barcode segnacooli)
050000070719     C                   IF        %subst(vindta:1:2) = '02'
050100070719     C* Resetto indicatore d anomalia sul singolo record
050200070719     C                   eval      vinflg = '1'
050300070719     C*
050400070719     C* Ciclo x tutti i 50 colli max previsti sul record
050500070719     C                   Z-ADD     19            wPosWho           3 0
050600070719     C                   Z-ADD     10            wLenWho           2 0
050700070719     C                   DOW       wPosWho <= %len(%trim(vindta))
050800040910     C* ......VATNOT
050900070719     C                   EVAL      VATNOT=%trim(%subst(vindta:wPosWho:wLenWho))
051000080825     C                   EVAL      VATNOT=%subst(%trim(VATNOT):2)
051100040910     C                   EVAL      VATTRC = 'E'
051200070808     C* x ogni tipo record 'EBC' devo scaricare il buffer del file EDIVAT
051300060202     C                   IF        VATNOT <> *blanks
051400070808     C                   EXSR      VALVATCMR
051500070808     C                   WRITE     EDIVAT00
051600060202     C                   ENDIF
051700070830     C                   EVAL      wPosWho = wPosWho + wLenWho
051800070719     C                   ENDDO
051900040910     C*
052000040910     C                   ENDIF
052100070919     C***
052200070919     C* ...tipo record '03' (estensione testata: note consegna)
052300070919     C                   IF        %subst(vindta:1:2) = '03'
052400070919     C                   MOVEL     *blanks       wNOTE2           70
052500070919     C                   EVAL      wNOTE2 = %trim(%subst(vindta:13))
052600070919     C                   EVAL      VABNOT = %subst(wNOTE2:1:35)
052700070919     C                   EVAL      VABNT2 = %subst(wNOTE2:36:35)
052800070919     C                   ENDIF
052900010202     C*
053000000801     C* Ebbene...
053100000801     C                   ADD       1             §CTRMO
053200010201     C                   IF        errore <> *zeros
053300000801     C                   ADD       1             §CTRNO
053400000801     C                   EVAL      recko = vindta
053500000801     C                   EXCEPT    rigadett
053600000801     C                   EVAL      vinflg = '2'
053700000801     C                   ELSE
053800010201     C                   ADD       1             §CTROKVB
053900000801     C                   ENDIF
054000000801     C*
054100000801     C                   ENDSR
054200070808     C*----------------------------------------------------*
054300070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAB
054400070808     C*----------------------------------------------------*
054500070808     C     VALVABCMR     BEGSR
054600070906     C*
054700070906     C***                EVAL      VABCMR = 'THUN'
054800070906     C                   EVAL      VABCMR = wCMR
054900070808     C                   EVAL      VABDCM = DATCOR
055000070808     C                   EVAL      VABDTS = DATCOR
055100070808     C                   EVAL      VABHMS = ORACOR
055200070808     C                   EVAL      VABCNT = 1
055300070808     C*
055400070808     C                   ENDSR
055500070808     C*----------------------------------------------------*
055600070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAT
055700070808     C*----------------------------------------------------*
055800070808     C     VALVATCMR     BEGSR
055900070808     C*
056000070808     C* VALORIZZO CAMPI RELATIVI AL "CMR"
056100070907     C                   EVAL      VATCMR = wCMR
056200070907     C                   EVAL      VATCNT = 1
056300070808     C*
056400070808     C                   ENDSR
056500010202     C*----------------------------------------------------*
056600070808     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
056700010202     C*----------------------------------------------------*
056800020305     C     PREVAT        BEGSR
056900010202     C*
057000070808     C* Compongo il nome del membro da dare al EDIVATWR
057100010202     C                   eval      parmbr = vlrhdl
057200010202     C                   movel     'M'           parmbr
057300060113     C                   eval      parccm = vlrksc
057400010202     C                   eval      paropz = '1'
057500010202     C* Effettuo la chiamata al CLLE preposto
057600070808     C                   call(e)   'TITVEVTC'
057700010202     C                   parm                    parccm
057800010202     C                   parm                    parmbr
057900010202     C                   parm                    paropz
058000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058100010202     C                   if        %error
058200010202     C                   movel     '1'           chkcall
058300010202     C                   else
058400010202     C                   movel     '0'           chkcall
058500010202     C                   endif
058600010202     C*
058700010202     C                   ENDSR
058800000801     C*----------------------------------------------------*
058900000801     C*  CONTROLLO NUMERICITA' CAMPI
059000000801     C*----------------------------------------------------*
059100000801     C     CHKNUM        BEGSR
059200000801     C*
059300000801     C                   call(e)   'ISNUMERIC'
059400000801     C                   PARM                    PiStr            30
059500040714     C                   PARM      ','           PiDecChr          1
059600000801     C                   PARM      *ZEROS        PiVal            30 9
059700000801     C                   PARM      '0'           PiInt             1
059800000801     C                   PARM      '0'           PiNum             1
059900000801     C                   IF        %error
060000000801     C                   EVAL      PiInt=*off
060100000801     C                   ENDIF
060200000801     C*
060300000801     C                   ENDSR
060400000801     C***
060500000801
060600011113
060700011113     C*----------------------------------------------------*
060800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060900011113     C*----------------------------------------------------*
061000011113     C     CHKIMPDIV     BEGSR
061100011113     C*
061200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061300011113     C                   Z-ADD     *zeros        wrkDec            9 9
061400011113     C*
061500011113     C* Come prima cosa effettuo considerazioni sulla divisa
061600011113     C                   IF        vabIAS > *zeros
061700011113     C                   IF        vabVAS <> 'EUR'
061800011113     C                   EVAL      vabVAS =  'ITL'
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C                   IF        vabCAS > *zeros
062300011113     C                   IF        vabVCA <> 'EUR'
062400011113     C                   EVAL      vabVCA =  'ITL'
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C                   IF        vabVMD > *zeros
062900020305     C                   IF        vabVAD <> 'EUR'
063000011113     C                   EVAL      vabVAD =  'ITL'
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063500011113     C                   Z-ADD     vabIAS        wrkDec
063600011113     C                   IF        wrkDec > *zeros
063700011113     C                   IF        vabVAS = 'ITL'
063800011113     C                   EVAL      vabIAS = *zeros
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064300011113     C                   Z-ADD     vabCAS        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVCA = 'ITL'
064600011113     C                   EVAL      vabCAS = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065100011113     C                   Z-ADD     vabVMD        wrkDec
065200011113     C                   IF        wrkDec > *zeros
065300011113     C                   IF        vabVAD = 'ITL'
065400011113     C                   EVAL      vabVMD = *zeros
065500011113     C                   ENDIF
065600011113     C                   ENDIF
065700011113     C*
065800011113     C                   ENDSR
065900011113     C***
066000011113
066100011113
066200000801
066300000801
066400990920      /TITLE Invio dei dati al punto operativo.
066500010202     C     invio         BEGSR
066600990920     C*
066700070808     C* 1° invio EDIVAT
066800010201     C                   reset                   dscmz
066900010201     C                   move      vlrpoi        cmzdst
067000070808     C                   eval      cmzfld = 'EDIVATWR'
067100010201     C                   eval      cmzmbd = vlrhdl
067200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067300021009     C***                if        prmfir = *blanks
067400070808     C                   eval      cmzfla = 'EDIVAT0F'
067500070808     C                   eval      cmzmba = 'EDIVAT0F'
067600021009     C***                else
067700021009     C***                eval      cmzfla = prmfir
067800021009     C***                eval      cmzmba = prmfir
067900021009     C***                endif
068000010201     C                   eval      cmznrr = *zeros
068100020305     C                   move      §ctrokvt      cmznrr
068200021018     C                   eval      cmzlba = vlrfl1
068300010201     C                   call(e)   'TIS711C'
068400010201     C                   parm                    dscmz
068500010201     C                   parm      *blanks       esito
068600010205     C                   if        %error
068700010205     C                             or cmzerr = '1'
068800010205     C                             or esito  = '1'
068900010205     C                   eval      wrkesito = '3'
069000010205     C                   else
069100010201     C*
069200070808     C* 2° invio EDIVAB
069300010201     C                   reset                   dscmz
069400010201     C                   move      vlrpoi        cmzdst
069500010201     C                   eval      cmzfld = vlrfou
069600010201     C                   eval      cmzmbd = vlrhdl
069700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069800021009     C***                if        prmfir = *blanks
069900070808     C                   eval      cmzfla = 'EDIVAB0F'
070000070808     C                   eval      cmzmba = 'EDIVAB0F'
070100021009     C***                else
070200021009     C***                eval      cmzfla = prmfir
070300021009     C***                eval      cmzmba = prmfir
070400021009     C***                endif
070500010201     C                   eval      cmznrr = *zeros
070600010201     C                   move      §ctrokvb      cmznrr
070700021018     C                   eval      cmzlba = vlrfl1
070800010201     C                   call(e)   'TIS711C'
070900010201     C                   parm                    dscmz
071000010201     C                   parm      *blanks       esito
071100010201     C                   if        %error
071200010201     C                             or cmzerr = '1'
071300010201     C                             or esito  = '1'
071400010201     C                   eval      wrkesito = '3'
071500010201     C                   endif
071600010205     C                   endif
071700990920     C*
071800000613     C                   ENDSR
071900000613     C***
072000070411
072100070411     C     *pssr         BEGSR
072200070411     C*
072300070411     C                   if        %open(tivin00r)
072400070411     C                   close     tivin00r
072500070411     C                   endif
072600070808     C                   if        %open(edivabwr)
072700070808     C                   close     edivabwr
072800070411     C                   endif
072900070808     C                   if        %open(edivatwr)
073000070808     C                   close     edivatwr
073100070411     C                   endif
073200070411     C*
073300070411     C* Effettuo la chiamata al CLLE preposto
073400070808     C                   call(e)   'TITVEVTC'
073500070411     C                   parm                    parccm
073600070411     C                   parm                    parmbr
073700070411     C                   parm      '2'           paropz
073800070411     C*
073900070411     C                   eval      wrkesito = '2'
074000070411     C*
074100070411     C                   seton                                        LR
074200070411     C*
074300070411     C                   ENDSR     '*CANCL'
074400070411     C***
074500070411
074600990910
074700000613     C     *inzsr        BEGSR
074800990910     C*
074900990910     C     *entry        plist
075000990920     C                   parm                    tivlrds
075100990921     C                   parm      wrkesito      esito
075200000724     C                   parm                    prmlit
075300000710     C                   parm                    prmfir
075400000613     C*
075500000830     C* CALCOLA LA DATA CORRENTE
075600000830     C                   time                    wn14             14 0
075700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075800070808     C                   movel     wn14          oracor            6 0          *ORA
075900000830     C                   z-add     wn8           g08dat
076000000830     C                   z-add     *zeros        g08inv
076100000830     C                   movel     '0'           g08err
076200000830     C                   call      'XSRDA8'
076300000830     C                   parm                    wlbda8
076400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076500070907     C*
076600070907     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
076700070907     C                   move(p)   vlrMSG        wNomeFile
076800070907     C*
076900070907     C* VALORIZZO CAMPI RELATIVI AL "CMR"
077000070910     C                   if        %subst(vlrppt:20:10) <> *blanks
077100070910     C                   eval      wCMR = %trim(%subst(vlrppt:20:10))
077200070907     C                   else
077300070910     C                   eval      wPosDaA = %subst(vlrppt:16:2)
077400070907     C                   move(p)   wPosDaA       wPosDa
077500070910     C                   eval      wLungA  = %subst(vlrppt:18:2)
077600070907     C                   move(p)   wLungA        wLung
077700070907     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
077800070907     C                   endif
077900000830     C*
078000000613     C                   ENDSR
078100000613     C***
078200990908
078300070719     Otitvz8p   E            riepilogo         2
078400990915     O                                              'Upload via Internet'
078500990915     O                                           +1 'Traduzione TIVIN00R -'
078600070808     O                                           +1 'EDIVABWR/EDIVATWR'
078700010201     O                                           +1 'Report testate bolle'
078800990915     O          E            riepilogo   2
078900990915     O                       wrkdata
079000990915     O                       wrkora              +1
079100990915     O                       procname            +1
079200990915     O          E            riepilogo   2
079300990915     O                                              'Cliente..................:'
079400990915     O                       VABCCM        z     +1
079500990915     O          E            riepilogo   2
079600990920     O                                              'Riferimento Strategi.....:'
079700990920     O                       vlrhdl              +1
079800990915     O          E            riepilogo   2
079900990915     O                                              'Giusti...................:'
080000010201     O                       §CTROKVB      2   +  1
080100990915     O          E            riepilogo   2
080200990915     O                                              'Sbagliati e corretti.....:'
080300971022     O                       §CTRMO        2   +  1
080400990915     O          E            riepilogo   2
080500990915     O                                              'Sbagliati e scartati.....:'
080600971022     O                       §CTRNO        2   +  1
080700000613
080800070719     Otitvz8ps  E            testdett          2
080900000613     O                                              'Upload via Internet'
081000000613     O                                           +1 'Traduzione TIVIN00R -'
081100070808     O                                           +1 'EDIVABWR/EDIVATWR'
081200010201     O                                           +1 'Report testate bolle'
081300000616     O          E            testdett    3
081400000613     O                                           +2 'N° rec'
081500000613     O                                           +3 'Anteprima contenuto'
081600000616     O          E            rigadett    2
081700000613     O                       rrnum               +2
081800000621     O                       recko               +3
081900000616     O          E            findett     2
082000000613     O                       wrkdata
082100000613     O                       wrkora              +1
082200000613     O                       procname            +1
082300000616     O          E            findett     2
082400000613     O                                              'Cliente..................:'
082500000613     O                       VABCCM        z     +1
082600000616     O          E            findett     2
082700000613     O                                              'Riferimento Strategi.....:'
082800000613     O                       vlrhdl              +1
082900000616     O          E            findett     2
083000000613     O                                              'Giusti...................:'
083100010201     O                       §CTROKVB      2   +  1
083200000616     O          E            findett     2
083300000613     O                                              'Sbagliati e corretti.....:'
083400000613     O                       §CTRMO        2   +  1
083500000616     O          E            findett     2
083600000613     O                                              'Sbagliati e scartati.....:'
083700000613     O                       §CTRNO        2   +  1
083800000512** CMD - COMANDI CL
083900070719OVRPRTF FILE(TITVZ8P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
084000070719OVRPRTF FILE(TITVZ8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
084100070719DLTOVR FILE(TITVZ8P TITVZ8PS) LVL(*)
084200000512
084300000512
