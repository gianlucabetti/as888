000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800061212     Ftitvv9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100061212     Ftitvv9ps  O    f  198        PRINTER usropn
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
002600060725     D tisi95ds      e ds
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
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900000830     D*
005000990908
005100060307     D*------------------
005200060307     D* DS REPERIMENTO NUMERATORE
005300060307     D*------------------
005400060529     D trul33ds      e ds                  inz
005500060307     D*------------------
005600060307     D* DS ARCHITETTURA
005700060307     D*------------------
005800060307     D kpjba         e ds                  inz
005900060307     D*------------------
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700061212     C                   if        not %open(titvv9ps)
009800061212     C                   open      titvv9ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600040506     C                   call(e)   'TITVVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600010202     C*
011700061212     C                   if        %open(titvv9ps)
011800000616     C                   except    findett
011900061212     C                   close     titvv9ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800000801     C
012900010201     C                   seton                                        LR
013000000613
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700061212     C                   if        not %open(titvv9p)
013800061212     C                   open      titvv9p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300061212     C                   if        %open(titvv9p)
014400061212     C                   close     titvv9p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015300910830     C*--------------------------------------------------------
015400040526     C     RWFILE        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900021113     C                   if        not %open(fivabwwr)
016000021113     C                   open      fivabwwr
016100990914     C                   endif
016200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700021113     C                   if        not %open(fivatwwr)
016800021113     C                   open      fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   clear                   §CTROKVB          5 0
017200020305     C                   clear                   §CTROKVT          5 0
017300000801     C                   clear                   §CTRMO            5 0
017400000801     C                   clear                   §CTRNO            5 0
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900050627     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000801     C*
018200000801     C                   if        *in70 = *off
018300000801     C                             and
018400000801     C                             (vinflg = *blanks
018500000801     C                              or vinflg = '0'
018600000801     C                              or vinflg = '2')
018700000801     C*
018800000801     C                   clear                   vinmsg
018900000801     C                   eval      vinflg = '1'
019000010305     C*
019100050628     C                   clear                   fivab000
019200061212     C                   clear                   fivat000
019300061212     C*
019400061212     C* Verifico che il record corrente sia effettivamente 1 record d dati
019500061212     C                   IF        %subst(vindta:1:5) <> *blanks
019600061212     C                   EVAL      PiStr=%trim(%subst(vindta:1:5))
019700061212     C                   EXSR      CHKNUM
019800061212     C                   IF        PiInt=*on
019900050628     C                   exsr      impvab                                       => carico VAB
020000060725     C                   exsr      wrivab                                       => scarico VAB
020100061212     C                   ENDIF
020200061212     C                   ENDIF
020300000905     C*
020400000905     C                   else
020500000905     C                   eval      vinflg = '1'
020600050628     C                   endif
020700000905     C                   endif
020800000905     C*
020900000905     C  N70              update    tivin000
021000000905     C*
021100991022     C  N70              ENDdo
021200010202     C*
021300010202     C                   endif
021400990910
021500990910     C* Se non ci sono record con errori ...
021600000710     C                   if        §ctrno = 0
021700990910     C* ... restituisco esito OK.
021800990921     C                   eval      wrkesito = '0'
021900990910     C                   else
022000010201     C                   if        §ctrokvb > 0
022100990921     C                   eval      wrkesito = '1'
022200000710     C                   else
022300000710     C                   eval      wrkesito = '2'
022400990910     C                   endif
022500000710     C                   endif
022600990910     C*
022700990914     C                   if        %open(tivin00r)
022800990908     C                   close     tivin00r
022900990914     C                   endif
023000021113     C                   if        %open(fivabwwr)
023100021113     C                   close     fivabwwr
023200990914     C                   endif
023300021113     C                   if        %open(fivatwwr)
023400021113     C                   close     fivatwwr
023500010201     C                   endif
023600990910     C*
023700010201     C                   if        §ctrokvb > 0
023800000724     C                             and vlrpoi <> *zeros
023900010202     C                   exsr      invio
024000990920     C                   endif
024100990920     C*
024200910830     C                   ENDSR
024300000613     C***
024400010305
024500010305     C*----------------------------------------------------*
024600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024700010305     C*----------------------------------------------------*
024800020305     C     WRIVAB        BEGSR
024900010305     C*
025000060225     C* Quindi scarico il buffer del file d testata
025100021113     C                   write     fivab000                                     => scarico il VAB
025200010305     C*
025300010305     C                   ENDSR
025400990920
025500000801     C*----------------------------------------------------*
025600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025700000801     C*----------------------------------------------------*
025800010201     C     INZVAR        BEGSR
025900000801     C*
026000040802     C                   Z-ADD     *zeros        Num5_0            5 0
026100040802     C                   MOVEL     '0'           FlgCAS            1
026200000801     C*
026300000801     C                   ENDSR
026400000801     C*----------------------------------------------------*
026500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026600000801     C*----------------------------------------------------*
026700000801     C     DEFCAM        BEGSR
026800000801     C*
026900020619     C* Imposto i valori di default...
027000061212     C                   Z-ADD     0780249       VABCCM
027100061212     C                   Z-ADD     0780249       VATCCM
027200061212     C                   Z-ADD     078           VABLNP
027300061212     C                   Z-ADD     078           VATLNP
027400040714     C                   Z-ADD     000           VABCTR
027500060725     C                   MOVEL     '1'           VABCBO
027600020619     C* ... e poi verifico se sono stati passati come parametri
027700020619     C                   IF        vlrppt > *blanks
027800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028000020619     C                   EXSR      CHKNUM
028100020619     C                   IF        PiInt=*on
028200020619     C                   Z-ADD     PiVal         VABCCM
028300020619     C                   Z-ADD     PiVal         VATCCM
028400020619     C                   ENDIF
028500040506     C                   ENDIF
028600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028800020619     C                   EXSR      CHKNUM
028900020619     C                   IF        PiInt=*on
029000020619     C                   Z-ADD     PiVal         VABLNP
029100020619     C                   Z-ADD     PiVal         VATLNP
029200040506     C                   ENDIF
029300020619     C                   ENDIF
029400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029600020619     C                   EXSR      CHKNUM
029700020619     C                   IF        PiInt=*on
029800020619     C                   Z-ADD     PiVal         VABCTR
029900040506     C                   ENDIF
030000020619     C                   ENDIF
030100020619     C                   ENDIF
030200000801     C*
030300000801     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030600000801     C*----------------------------------------------------*
030700040823     C     IMPVAB        BEGSR
030800040823     C*
030900020305     C                   EXSR      INZVAR
031000020305     C                   EXSR      DEFCAM
031100010305     C*
031200000801     C                   Z-ADD     *zeros        errore            1 0
031300000830     C                   MOVEL     datcor        VABAAS
031400020305     C                   MOVEL     datcor        VATAAS
031500040526     C                   MOVE      datcor        VABMGS
031600060529     C                   MOVE(P)   vlrpoi        VABFGS
031700060529     C                   MOVE(P)   vlrpoi        VATFGS
031800050628     C*
031900061212     C                   EVAL      VABRSD=%trim(%subst(vindta:17:30))
032000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032200020117     C* ==
032300061212     C                   EVAL      VABRD2=%trim(%subst(vindta:192:30))
032400061212     C                   EVAL      VABIND=%trim(%subst(vindta:48:30))
032500061212     C                   EVAL      VABLOD=%trim(%subst(vindta:85:23))
032600061212     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
032700061212     C                   MOVEL     *BLANKS       wNOTE            78
032800061212     C                   EVAL      wNOTE=%trim(%subst(vindta:223:78))
032900061212     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
033000061212     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
033100061212     C                   EVAL      VABRMA=%trim(%subst(vindta:152:7))
033200061212     C*
033300061212     C* Gestione giorno d chiusura
033400061212     C                   IF        %subst(vindta:128:4) = 'Lune'
033500061212     C                   EVAL      VABGC1='1'
033600061212     C                   ENDIF
033700061212     C                   IF        %subst(vindta:128:4) = 'Mart'
033800061212     C                   EVAL      VABGC1='2'
033900061212     C                   ENDIF
034000061212     C                   IF        %subst(vindta:128:4) = 'Merc'
034100061212     C                   EVAL      VABGC1='3'
034200061212     C                   ENDIF
034300061212     C                   IF        %subst(vindta:128:4) = 'Giov'
034400061212     C                   EVAL      VABGC1='4'
034500061212     C                   ENDIF
034600061212     C                   IF        %subst(vindta:128:4) = 'Vene'
034700061212     C                   EVAL      VABGC1='5'
034800061212     C                   ENDIF
034900060529     C* RMN
035000061212     C                   EVAL      PiStr=%trim(%subst(vindta:10:6))
035100060529     C                   EXSR      CHKNUM
035200060529     C                   IF        PiInt=*on
035300060529     C                   Z-ADD     PiVal         VABRMN
035400060529     C                   ELSE
035500060529     C                   ADD       1             errore
035600060529     C                   EVAL      vinmsg = %trimr(vinmsg)
035700060529     C                             + ' ' + 'VABRMN'
035800060529     C                   ENDIF
035900060529     C* NSP => Stacco un numeratore da AZNUM
036000060529     C                   clear                   TRUL33DS
036100060529     C                   eval      I33OPE = *zeros
036200060529     C                   eval      I33CNU = 302
036300060529     C                   eval      I33NUM = 1
036400060529     C                   movel     TRUL33DS      KPJBU
036500060529     C                   call      'TRUL33R'
036600060529     C                   parm                    KPJBA
036700060529     C                   movel     KPJBU         TRUL33DS
036800060529     C                   if        O33ERR = *zeros
036900060529     C                   z-add     O33NRF        VABNSP
037000060529     C                   z-add     O33NRF        VATNSP
037100060529     C                   else
037200060529     C                   Z-ADD     1             errore
037300060529     C                   EVAL      vinmsg = %trimr(vinmsg)
037400060529     C                             + ' ' + 'VABNSP VATNSP'
037500060529     C                   endif
037600060529     C* CAD
037700061212     C                   EVAL      PiStr=%trim(%subst(vindta:79:5))
037800010201     C                   EXSR      CHKNUM
037900010201     C                   IF        PiInt=*on
038000010201     C                   Z-ADD     PiVal         Num5_0
038100040506     C                   MOVEL(P)  Num5_0        VABCAD
038200010201     C                   ELSE
038300040506     C                   ADD       1             errore
038400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038500040506     C                             + ' ' + 'VABCAD'
038600010201     C                   ENDIF
038700040506     C* Reperisco la provincia dal CAP e dalla località
038800040526     C                   IF        VABCAD <> *blanks AND
038900040526     C                             VABPRD  = *blanks
039000040506     C                   CLEAR                   TISI95DS
039100040506     C                   EVAL      I95TCN = '3'
039200040506     C                   Z-ADD     datcor        I95DAT
039300040506     C                   EVAL      I95CAP = VABCAD
039400040506     C                   EVAL      I95LOC = VABLOD
039500050627     C                   EVAL      I95NAR = VABNZD
039600040506     C                   CALL      'TISI95R'
039700040506     C                   PARM                    TISI95DS
039800040506     C                   EVAL      VABPRD = O95PRV
039900040506     C                   ENDIF
040000060725     C* NCL
040100061212     C                   EVAL      PiStr=%trim(%subst(vindta:140:4))
040200060725     C                   EXSR      CHKNUM
040300060725     C                   IF        PiInt=*on
040400060725     C                   Z-ADD     PiVal         VABNCL
040500060725     C                   ELSE
040600060725     C                   ADD       1             errore
040700060725     C                   EVAL      vinmsg = %trimr(vinmsg)
040800060725     C                             + ' ' + 'VABNCL'
040900060725     C                   ENDIF
041000040506     C* PKB
041100061212     C                   EVAL      PiStr=%trim(%subst(vindta:146:5))
041200010201     C                   EXSR      CHKNUM
041300010201     C                   IF        PiNum=*on
041400060725     C                   Z-ADD(H)  PiVal         VABPKB
041500010201     C                   ELSE
041600010201     C                   ADD       1             errore
041700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
041800010201     C                             + ' ' + 'VABPKB'
041900010201     C                   ENDIF
042000060725     C* CAS
042100061212     C                   IF        %trim(%subst(vindta:171:12))<>'0,00'
042200061212     C                   MOVEL     '1'           FlgCAS
042300060725     C                   EVAL      VABVCA='EUR'
042400061212     C                   EVAL      PiStr=%trim(%subst(vindta:171:12))
042500060307     C                   EXSR      CHKNUM
042600060307     C                   IF        PiNum=*on
042700060725     C                   Z-ADD     PiVal         VABCAS
042800060307     C                   ELSE
042900060307     C                   ADD       1             errore
043000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
043100060307     C                             + ' ' + 'VABCAS'
043200060307     C                   ENDIF
043300060725     C                   ENDIF
043400060529     C*
043500061212     C* Gestione telefono destinatario
043600061212     C                   exsr      exeVAT_B
043700010205     C*
043800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043900040802     C                   IF        FlgCAS <> '0'
044000010205     C                   EVAL      VABCBO = '4'
044100010205     C                   ELSE
044200010205     C                   EVAL      VABCBO = '1'
044300010205     C                   ENDIF
044400020305     C*
044500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044600011113     C                   EXSR      CHKIMPDIV
044700010202     C*
044800000801     C* Ebbene...
044900000801     C                   ADD       1             §CTRMO
045000010201     C                   IF        errore <> *zeros
045100000801     C                   ADD       1             §CTRNO
045200000801     C                   EVAL      recko = vindta
045300000801     C                   EXCEPT    rigadett
045400000801     C                   EVAL      vinflg = '2'
045500000801     C                   ELSE
045600010201     C                   ADD       1             §CTROKVB
045700000801     C                   ENDIF
045800000801     C*
045900000801     C                   ENDSR
046000050628     C*----------------------------------------------------*
046100061212     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046200050628     C*----------------------------------------------------*
046300061212     C     EXEVAT_B      BEGSR
046400050628     C*
046500061212     C                   EVAL      VATTRC='B'
046600061212     C                   EVAL      VATNOT=%trim(%subst(vindta:112:15))
046700050628     C                   exsr      wrivat                                       => scarico VAT
046800050628     C*
046900050628     C                   ENDSR
047000010201     C*----------------------------------------------------*
047100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047200010201     C*----------------------------------------------------*
047300020305     C     WRIVAT        BEGSR
047400050628     C*
047500060223     C* Scrivo solo se valorizzato qualcosa
047600060223     C                   IF        VATNOT <> *blanks
047700040802     C                   WRITE     FIVAT000
047800060223     C                   ENDIF
047900010201     C*
048000010201     C                   ENDSR
048100010202     C*----------------------------------------------------*
048200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048300010202     C*----------------------------------------------------*
048400020305     C     PREVAT        BEGSR
048500010202     C*
048600021113     C* Compongo il nome del membro da dare al FIVATWWR
048700010202     C                   eval      parmbr = vlrhdl
048800010202     C                   movel     'M'           parmbr
048900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049000010202     C                   eval      paropz = '1'
049100010202     C* Effettuo la chiamata al CLLE preposto
049200040506     C                   call(e)   'TITVVTC'
049300010202     C                   parm                    parccm
049400010202     C                   parm                    parmbr
049500010202     C                   parm                    paropz
049600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049700010202     C                   if        %error
049800010202     C                   movel     '1'           chkcall
049900010202     C                   else
050000010202     C                   movel     '0'           chkcall
050100010202     C                   endif
050200010202     C*
050300010202     C                   ENDSR
050400000801     C*----------------------------------------------------*
050500000801     C*  CONTROLLO NUMERICITA' CAMPI
050600000801     C*----------------------------------------------------*
050700000801     C     CHKNUM        BEGSR
050800000801     C*
050900000801     C                   call(e)   'ISNUMERIC'
051000000801     C                   PARM                    PiStr            30
051100061212     C                   PARM      ','           PiDecChr          1
051200000801     C                   PARM      *ZEROS        PiVal            30 9
051300000801     C                   PARM      '0'           PiInt             1
051400000801     C                   PARM      '0'           PiNum             1
051500000801     C                   IF        %error
051600000801     C                   EVAL      PiInt=*off
051700000801     C                   ENDIF
051800000801     C*
051900000801     C                   ENDSR
052000000801     C***
052100000801
052200011113
052300011113     C*----------------------------------------------------*
052400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052500011113     C*----------------------------------------------------*
052600011113     C     CHKIMPDIV     BEGSR
052700011113     C*
052800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052900011113     C                   Z-ADD     *zeros        wrkDec            9 9
053000011113     C*
053100011113     C* Come prima cosa effettuo considerazioni sulla divisa
053200011113     C                   IF        vabIAS > *zeros
053300011113     C                   IF        vabVAS <> 'EUR'
053400011113     C                   EVAL      vabVAS =  'ITL'
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C                   IF        vabCAS > *zeros
053900011113     C                   IF        vabVCA <> 'EUR'
054000011113     C                   EVAL      vabVCA =  'ITL'
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C                   IF        vabVMD > *zeros
054500020305     C                   IF        vabVAD <> 'EUR'
054600011113     C                   EVAL      vabVAD =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055100011113     C                   Z-ADD     vabIAS        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVAS = 'ITL'
055400011113     C                   EVAL      vabIAS = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055900011113     C                   Z-ADD     vabCAS        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVCA = 'ITL'
056200011113     C                   EVAL      vabCAS = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056700011113     C                   Z-ADD     vabVMD        wrkDec
056800011113     C                   IF        wrkDec > *zeros
056900011113     C                   IF        vabVAD = 'ITL'
057000011113     C                   EVAL      vabVMD = *zeros
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C                   ENDSR
057500011113     C***
057600011113
057700011113
057800000801
057900000801
058000990920      /TITLE Invio dei dati al punto operativo.
058100010202     C     invio         BEGSR
058200990920     C*
058300021113     C* 1° invio FIVAT
058400010201     C                   reset                   dscmz
058500010201     C                   move      vlrpoi        cmzdst
058600021113     C                   eval      cmzfld = 'FIVATWWR'
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000021113     C                   eval      cmzfla = 'FIVAT00F'
059100021113     C                   eval      cmzmba = 'FIVAT00F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700020305     C                   move      §ctrokvt      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010205     C                   if        %error
060300010205     C                             or cmzerr = '1'
060400010205     C                             or esito  = '1'
060500010205     C                   eval      wrkesito = '3'
060600010205     C                   else
060700010201     C*
060800021113     C* 2° invio FIVAB
060900010201     C                   reset                   dscmz
061000010201     C                   move      vlrpoi        cmzdst
061100010201     C                   eval      cmzfld = vlrfou
061200010201     C                   eval      cmzmbd = vlrhdl
061300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061400021009     C***                if        prmfir = *blanks
061500021113     C                   eval      cmzfla = 'FIVAB00F'
061600021113     C                   eval      cmzmba = 'FIVAB00F'
061700021009     C***                else
061800021009     C***                eval      cmzfla = prmfir
061900021009     C***                eval      cmzmba = prmfir
062000021009     C***                endif
062100010201     C                   eval      cmznrr = *zeros
062200010201     C                   move      §ctrokvb      cmznrr
062300021018     C                   eval      cmzlba = vlrfl1
062400010201     C                   call(e)   'TIS711C'
062500010201     C                   parm                    dscmz
062600010201     C                   parm      *blanks       esito
062700010201     C                   if        %error
062800010201     C                             or cmzerr = '1'
062900010201     C                             or esito  = '1'
063000010201     C                   eval      wrkesito = '3'
063100010201     C                   endif
063200010205     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500000613     C***
063600070411
063700070411     C     *pssr         BEGSR
063800070411     C*
063900070411     C                   if        %open(tivin00r)
064000070411     C                   close     tivin00r
064100070411     C                   endif
064200070411     C                   if        %open(fivabwwr)
064300070411     C                   close     fivabwwr
064400070411     C                   endif
064500070411     C                   if        %open(fivatwwr)
064600070411     C                   close     fivatwwr
064700070411     C                   endif
064800070411     C*
064900070411     C* Effettuo la chiamata al CLLE preposto
065000070411     C                   call(e)   'TITVVTC'
065100070411     C                   parm                    parccm
065200070411     C                   parm                    parmbr
065300070411     C                   parm      '2'           paropz
065400070411     C*
065500070411     C                   eval      wrkesito = '2'
065600070411     C*
065700070411     C                   seton                                        LR
065800070411     C*
065900070411     C                   ENDSR     '*CANCL'
066000070411     C***
066100070411
066200990910
066300000613     C     *inzsr        BEGSR
066400990910     C*
066500990910     C     *entry        plist
066600990920     C                   parm                    tivlrds
066700990921     C                   parm      wrkesito      esito
066800000724     C                   parm                    prmlit
066900000710     C                   parm                    prmfir
067000000613     C*
067100000830     C* CALCOLA LA DATA CORRENTE
067200000830     C                   time                    wn14             14 0
067300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067400000830     C                   z-add     wn8           g08dat
067500000830     C                   z-add     *zeros        g08inv
067600000830     C                   movel     '0'           g08err
067700000830     C                   call      'XSRDA8'
067800000830     C                   parm                    wlbda8
067900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068000000830     C*
068100000613     C                   ENDSR
068200000613     C***
068300990908
068400061212     Otitvv9p   E            riepilogo         2
068500990915     O                                              'Upload via Internet'
068600990915     O                                           +1 'Traduzione TIVIN00R -'
068700021113     O                                           +1 'FIVABWWR/FIVATWWR'
068800010201     O                                           +1 'Report testate bolle'
068900990915     O          E            riepilogo   2
069000990915     O                       wrkdata
069100990915     O                       wrkora              +1
069200990915     O                       procname            +1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Cliente..................:'
069500990915     O                       VABCCM        z     +1
069600990915     O          E            riepilogo   2
069700990920     O                                              'Riferimento Strategi.....:'
069800990920     O                       vlrhdl              +1
069900990915     O          E            riepilogo   2
070000990915     O                                              'Giusti...................:'
070100010201     O                       §CTROKVB      2   +  1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Sbagliati e corretti.....:'
070400971022     O                       §CTRMO        2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e scartati.....:'
070700971022     O                       §CTRNO        2   +  1
070800000613
070900061212     Otitvv9ps  E            testdett          2
071000000613     O                                              'Upload via Internet'
071100000613     O                                           +1 'Traduzione TIVIN00R -'
071200021113     O                                           +1 'FIVABWWR/FIVATWWR'
071300010201     O                                           +1 'Report testate bolle'
071400000616     O          E            testdett    3
071500000613     O                                           +2 'N° rec'
071600000613     O                                           +3 'Anteprima contenuto'
071700000616     O          E            rigadett    2
071800000613     O                       rrnum               +2
071900000621     O                       recko               +3
072000000616     O          E            findett     2
072100000613     O                       wrkdata
072200000613     O                       wrkora              +1
072300000613     O                       procname            +1
072400000616     O          E            findett     2
072500000613     O                                              'Cliente..................:'
072600000613     O                       VABCCM        z     +1
072700000616     O          E            findett     2
072800000613     O                                              'Riferimento Strategi.....:'
072900000613     O                       vlrhdl              +1
073000000616     O          E            findett     2
073100000613     O                                              'Giusti...................:'
073200010201     O                       §CTROKVB      2   +  1
073300000616     O          E            findett     2
073400000613     O                                              'Sbagliati e corretti.....:'
073500000613     O                       §CTRMO        2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e scartati.....:'
073800000613     O                       §CTRNO        2   +  1
073900000512** CMD - COMANDI CL
074000061212OVRPRTF FILE(TITVV9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
074100061212OVRPRTF FILE(TITVV9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074200061212DLTOVR FILE(TITVV9P TITVV9PS) LVL(*)
074300000512
074400000512
