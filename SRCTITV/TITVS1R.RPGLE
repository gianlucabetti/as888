000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800060315     Ftitvs1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060315     Ftitvs1ps  O    f  198        PRINTER usropn
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
002600060307     D tisi95ds      e ds
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
005000010201
005100010201
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000724     C*
005800000724     C* SE OCCORRE SPEDIRE IN FILIALE
005900000724     C                   if        vlrpoi <> *zeros
006000000724     C*
006100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006300000724     C     vlrpoi        chain     azorg01l
006400000724     C                   if        %found
006500000616     C                   movel(p)  CMD(1)        depcmd
006600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006700000616     C*
006800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006900011113     C                   Z-ADD     150           LENGH            15 5
007000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007100000616     C                   PARM                    depcmd
007200000616     C                   PARM                    LENGH
007300000724     C*
007400000724     C                   endif
007500000724     C                   endif
007600000616     C*
007700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000616     C                   movel(p)  CMD(2)        depcmd
007900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000616     C*
008700060315     C                   if        not %open(titvs1ps)
008800060315     C                   open      titvs1ps
008900000616     C                   except    testdett
009000000613     C                   endif
009100000613     C*
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009400000613     C*
009500010202     C* Effettuo la chiamata al CLLE preposto
009600040506     C                   call(e)   'TITVVTC'
009700010202     C                   parm                    parccm
009800010202     C                   parm                    parmbr
009900010202     C                   parm      '2'           paropz
010000050201     C*
010100050201     C* Effettuo lancio TISI95 solo x chiusura
010200050201     C                   CLEAR                   TISI95DS
010300050201     C                   EVAL      I95TLA = 'C'
010400050201     C                   CALL      'TISI95R'
010500050201     C                   PARM                    TISI95DS
010600010202     C*
010700060315     C                   if        %open(titvs1ps)
010800000616     C                   except    findett
010900060315     C                   close     titvs1ps
011000000613     C                   endif
011100000616     C*
011200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    CMD(3)
011600000616     C                   PARM                    LENGH
011700000616     C*
011800000801     C
011900010201     C                   seton                                        LR
012000000613
012100000613
012200000613     C*--------------------------------------------------------
012300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012400000613     C*--------------------------------------------------------
012500000621     C     STPR          BEGSR
012600000613     C*
012700060315     C                   if        not %open(titvs1p)
012800060315     C                   open      titvs1p
012900990915     C                   endif
013000990915     C*
013100990915     C                   except    riepilogo
013200990915     C*
013300060315     C                   if        %open(titvs1p)
013400060315     C                   close     titvs1p
013500990914     C                   endif
013600990910     C*
013700000613     C                   ENDSR
013800000613     C***
013900990908
014000000801
014100910830     C*--------------------------------------------------------
014200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014300910830     C*--------------------------------------------------------
014400040526     C     RWFILE        BEGSR
014500990910     C*
014600990914     C                   if        not %open(tivin00r)
014700990908     C                   open      tivin00r
014800990914     C                   endif
014900021113     C                   if        not %open(fivabwwr)
015000021113     C                   open      fivabwwr
015100990914     C                   endif
015200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300020305     C                   exsr      prevat
015400010201     C*
015500010202     C                   if        chkcall = '0'
015600010202     C*
015700021113     C                   if        not %open(fivatwwr)
015800021113     C                   open      fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   clear                   §CTROKVB          5 0
016200020305     C                   clear                   §CTROKVT          5 0
016300000801     C                   clear                   §CTRMO            5 0
016400000801     C                   clear                   §CTRNO            5 0
016500990910     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900050627     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000801     C*
017200000801     C                   if        *in70 = *off
017300000801     C                             and
017400000801     C                             (vinflg = *blanks
017500000801     C                              or vinflg = '0'
017600000801     C                              or vinflg = '2')
017700000801     C*
017800000801     C                   clear                   vinmsg
017900000801     C                   eval      vinflg = '1'
018000050628     C*
018100050628     C* A rottura inizializzo campi di totalizzazione
018200050628     C                   clear                   fivab000
018300050628     C*
018400060315     C                   exsr      impvab                                       => carico  VAB
018500060315     C                   exsr      wrivab                                       => scarico VAB
018600000905     C*
018700000905     C                   else
018800000905     C                   eval      vinflg = '1'
018900050628     C                   endif
019000000905     C                   endif
019100000905     C*
019200000905     C  N70              update    tivin000
019300000905     C*
019400991022     C  N70              ENDdo
019500010202     C*
019600010202     C                   endif
019700990910
019800990910     C* Se non ci sono record con errori ...
019900000710     C                   if        §ctrno = 0
020000990910     C* ... restituisco esito OK.
020100990921     C                   eval      wrkesito = '0'
020200990910     C                   else
020300010201     C                   if        §ctrokvb > 0
020400990921     C                   eval      wrkesito = '1'
020500000710     C                   else
020600000710     C                   eval      wrkesito = '2'
020700990910     C                   endif
020800000710     C                   endif
020900990910     C*
021000990914     C                   if        %open(tivin00r)
021100990908     C                   close     tivin00r
021200990914     C                   endif
021300021113     C                   if        %open(fivabwwr)
021400021113     C                   close     fivabwwr
021500990914     C                   endif
021600021113     C                   if        %open(fivatwwr)
021700021113     C                   close     fivatwwr
021800010201     C                   endif
021900990910     C*
022000010201     C                   if        §ctrokvb > 0
022100000724     C                             and vlrpoi <> *zeros
022200010202     C                   exsr      invio
022300990920     C                   endif
022400990920     C*
022500910830     C                   ENDSR
022600000613     C***
022700010305
022800010305     C*----------------------------------------------------*
022900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023000010305     C*----------------------------------------------------*
023100020305     C     WRIVAB        BEGSR
023200010305     C*
023300060225     C* Quindi scarico il buffer del file d testata
023400021113     C                   write     fivab000                                     => scarico il VAB
023500010305     C*
023600010305     C                   ENDSR
023700990920
023800000801     C*----------------------------------------------------*
023900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024000000801     C*----------------------------------------------------*
024100010201     C     INZVAR        BEGSR
024200000801     C*
024300040802     C                   Z-ADD     *zeros        Num5_0            5 0
024400040802     C                   MOVEL     '0'           FlgCAS            1
024500000801     C*
024600000801     C                   ENDSR
024700000801     C*----------------------------------------------------*
024800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024900000801     C*----------------------------------------------------*
025000000801     C     DEFCAM        BEGSR
025100000801     C*
025200020619     C* Imposto i valori di default...
025300060315     C                   Z-ADD     0590137       VABCCM
025400060315     C                   Z-ADD     0590137       VATCCM
025500060315     C                   Z-ADD     059           VABLNP
025600060315     C                   Z-ADD     059           VATLNP
025700040714     C                   Z-ADD     000           VABCTR
025800040823     C                   MOVEL     '1'           VABCBO
025900020619     C* ... e poi verifico se sono stati passati come parametri
026000020619     C                   IF        vlrppt > *blanks
026100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026300020619     C                   EXSR      CHKNUM
026400020619     C                   IF        PiInt=*on
026500020619     C                   Z-ADD     PiVal         VABCCM
026600020619     C                   Z-ADD     PiVal         VATCCM
026700020619     C                   ENDIF
026800040506     C                   ENDIF
026900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABLNP
027400020619     C                   Z-ADD     PiVal         VATLNP
027500040506     C                   ENDIF
027600020619     C                   ENDIF
027700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027900020619     C                   EXSR      CHKNUM
028000020619     C                   IF        PiInt=*on
028100020619     C                   Z-ADD     PiVal         VABCTR
028200040506     C                   ENDIF
028300020619     C                   ENDIF
028400020619     C                   ENDIF
028500000801     C*
028600000801     C                   ENDSR
028700000801     C*----------------------------------------------------*
028800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028900000801     C*----------------------------------------------------*
029000040823     C     IMPVAB        BEGSR
029100040823     C*
029200020305     C                   EXSR      INZVAR
029300020305     C                   EXSR      DEFCAM
029400010305     C*
029500000801     C                   Z-ADD     *zeros        errore            1 0
029600000830     C                   MOVEL     datcor        VABAAS
029700020305     C                   MOVEL     datcor        VATAAS
029800040526     C                   MOVE      datcor        VABMGS
029900040823     C                   MOVE(P)   vlrpoi        VABFGS
030000040823     C                   MOVE(P)   vlrpoi        VATFGS
030100050628     C*
030200060316     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
030300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030500020117     C* ==
030600060316     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
030700060316     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
030800060316     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
030900060316     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
031000060316     C                   IF        %subst(vindta:16:1) = '1'
031100060316     C                   EVAL      VABFFD = 'S'
031200060316     C                   ENDIF
031300050628     C* CAP
031400060316     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
031500010201     C                   EXSR      CHKNUM
031600010201     C                   IF        PiInt=*on
031700010201     C                   Z-ADD     PiVal         Num5_0
031800040506     C                   MOVEL(P)  Num5_0        VABCAD
031900010201     C                   ELSE
032000040506     C                   ADD       1             errore
032100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032200040506     C                             + ' ' + 'VABCAD'
032300010201     C                   ENDIF
032400040506     C* Reperisco la provincia dal CAP e dalla località
032500040526     C                   IF        VABCAD <> *blanks AND
032600040526     C                             VABPRD  = *blanks
032700040506     C                   CLEAR                   TISI95DS
032800040506     C                   EVAL      I95TCN = '3'
032900040506     C                   Z-ADD     datcor        I95DAT
033000040506     C                   EVAL      I95CAP = VABCAD
033100040506     C                   EVAL      I95LOC = VABLOD
033200050627     C                   EVAL      I95NAR = VABNZD
033300040506     C                   CALL      'TISI95R'
033400040506     C                   PARM                    TISI95DS
033500040506     C                   EVAL      VABPRD = O95PRV
033600040506     C                   ENDIF
033700060307     C* NCL
033800060316     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
033900060307     C                   EXSR      CHKNUM
034000060307     C                   IF        PiInt=*on
034100060307     C                   ADD       PiVal         VABNCL
034200060307     C                   ELSE
034300060307     C                   ADD       1             errore
034400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
034500060307     C                             + ' ' + 'VABNCL'
034600060307     C                   ENDIF
034700040506     C* PKB
034800060316     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
034900010201     C                   EXSR      CHKNUM
035000010201     C                   IF        PiNum=*on
035100060316     C                   Z-ADD     PiVal         VABPKB
035200010201     C                   ELSE
035300010201     C                   ADD       1             errore
035400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035500010201     C                             + ' ' + 'VABPKB'
035600010201     C                   ENDIF
035700060316     C* VLB
035800060316     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
035900060316     C                   EXSR      CHKNUM
036000060316     C                   IF        PiNum=*on
036100060316     C                   Z-ADD     PiVal         VABVLB
036200060316     C                   ELSE
036300060316     C                   ADD       1             errore
036400060316     C                   EVAL      vinmsg = %trimr(vinmsg)
036500060316     C                             + ' ' + 'VABVLB'
036600060316     C                   ENDIF
036700060316     C* NSP/RMN
036800060316     C                   EVAL      PiStr=%trim(%subst(vindta:331:6))
036900050928     C                   EXSR      CHKNUM
037000050928     C                   IF        PiInt=*on
037100060316     C                   Z-ADD     PiVal         VABNSP
037200060316     C                   Z-ADD     PiVal         VATNSP
037300050928     C                   Z-ADD     PiVal         VABRMN
037400050928     C                   ELSE
037500050928     C                   ADD       1             errore
037600050928     C                   EVAL      vinmsg = %trimr(vinmsg)
037700060316     C                             + ' ' + 'VABNSP VATNSP VABRMN'
037800050928     C                   ENDIF
037900060307     C* CAS
038000060316     C                   IF        %subst(vindta:280:12) <> '000000000,00'
038100060307     C                   EVAL      FlgCAS = '1'
038200060316     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
038300060307     C                   ENDIF
038400060316     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
038500060307     C                   EXSR      CHKNUM
038600060307     C                   IF        PiNum=*on
038700060316     C                   Z-ADD     PiVal         VABCAS
038800060307     C                   ELSE
038900060307     C                   ADD       1             errore
039000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
039100060307     C                             + ' ' + 'VABCAS'
039200060307     C                   ENDIF
039300060316     C* IAS
039400060316     C                   IF        %subst(vindta:295:12) <> '000000000,00'
039500060316     C                   EVAL      FlgCAS = '1'
039600060316     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
039700060316     C                   ENDIF
039800060316     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
039900060316     C                   EXSR      CHKNUM
040000060316     C                   IF        PiNum=*on
040100060316     C                   Z-ADD     PiVal         VABIAS
040200060316     C                   ELSE
040300060316     C                   ADD       1             errore
040400060316     C                   EVAL      vinmsg = %trimr(vinmsg)
040500060316     C                             + ' ' + 'VABIAS'
040600060316     C                   ENDIF
040700060316     C*
040800060316     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DEST.
040900060316     C                   exsr      exevatb
041000010205     C*
041100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041200040802     C                   IF        FlgCAS <> '0'
041300010205     C                   EVAL      VABCBO = '4'
041400010205     C                   ELSE
041500010205     C                   EVAL      VABCBO = '1'
041600010205     C                   ENDIF
041700020305     C*
041800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041900011113     C                   EXSR      CHKIMPDIV
042000010202     C*
042100000801     C* Ebbene...
042200000801     C                   ADD       1             §CTRMO
042300010201     C                   IF        errore <> *zeros
042400000801     C                   ADD       1             §CTRNO
042500000801     C                   EVAL      recko = vindta
042600000801     C                   EXCEPT    rigadett
042700000801     C                   EVAL      vinflg = '2'
042800000801     C                   ELSE
042900010201     C                   ADD       1             §CTROKVB
043000000801     C                   ENDIF
043100000801     C*
043200000801     C                   ENDSR
043300060307     C*----------------------------------------------------*
043400060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
043500060307     C*----------------------------------------------------*
043600060307     C     EXEVATB       BEGSR
043700060307     C*
043800060307     C                   EXSR      INZVAR
043900060307     C                   EXSR      DEFCAM
044000060307     C*
044100060307     C* Valorizzo il barcode
044200060307     C                   EVAL      VATTRC='B'
044300060316     C                   EVAL      VATNOT = %trim(%subst(vindta:92:15))
044400060307     C                   exsr      wrivat                                       => scarico VAT
044500060307     C*
044600060307     C                   ENDSR
044700010201     C*----------------------------------------------------*
044800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044900010201     C*----------------------------------------------------*
045000020305     C     WRIVAT        BEGSR
045100050628     C*
045200060223     C* Scrivo solo se valorizzato qualcosa
045300060223     C                   IF        VATNOT <> *blanks
045400040802     C                   WRITE     FIVAT000
045500060223     C                   ENDIF
045600010201     C*
045700010201     C                   ENDSR
045800010202     C*----------------------------------------------------*
045900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046000010202     C*----------------------------------------------------*
046100020305     C     PREVAT        BEGSR
046200010202     C*
046300021113     C* Compongo il nome del membro da dare al FIVATWWR
046400010202     C                   eval      parmbr = vlrhdl
046500010202     C                   movel     'M'           parmbr
046600050627     C                   eval      parccm = %subst(vlrKSC:2:7)
046700010202     C                   eval      paropz = '1'
046800010202     C* Effettuo la chiamata al CLLE preposto
046900040506     C                   call(e)   'TITVVTC'
047000010202     C                   parm                    parccm
047100010202     C                   parm                    parmbr
047200010202     C                   parm                    paropz
047300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047400010202     C                   if        %error
047500010202     C                   movel     '1'           chkcall
047600010202     C                   else
047700010202     C                   movel     '0'           chkcall
047800010202     C                   endif
047900010202     C*
048000010202     C                   ENDSR
048100000801     C*----------------------------------------------------*
048200000801     C*  CONTROLLO NUMERICITA' CAMPI
048300000801     C*----------------------------------------------------*
048400000801     C     CHKNUM        BEGSR
048500000801     C*
048600000801     C                   call(e)   'ISNUMERIC'
048700000801     C                   PARM                    PiStr            30
048800050627     C                   PARM      ','           PiDecChr          1
048900000801     C                   PARM      *ZEROS        PiVal            30 9
049000000801     C                   PARM      '0'           PiInt             1
049100000801     C                   PARM      '0'           PiNum             1
049200000801     C                   IF        %error
049300000801     C                   EVAL      PiInt=*off
049400000801     C                   ENDIF
049500000801     C*
049600000801     C                   ENDSR
049700000801     C***
049800000801
049900011113
050000011113     C*----------------------------------------------------*
050100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050200011113     C*----------------------------------------------------*
050300011113     C     CHKIMPDIV     BEGSR
050400011113     C*
050500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050600011113     C                   Z-ADD     *zeros        wrkDec            9 9
050700011113     C*
050800011113     C* Come prima cosa effettuo considerazioni sulla divisa
050900011113     C                   IF        vabIAS > *zeros
051000011113     C                   IF        vabVAS <> 'EUR'
051100011113     C                   EVAL      vabVAS =  'ITL'
051200011113     C                   ENDIF
051300011113     C                   ENDIF
051400011113     C*
051500011113     C                   IF        vabCAS > *zeros
051600011113     C                   IF        vabVCA <> 'EUR'
051700011113     C                   EVAL      vabVCA =  'ITL'
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C                   IF        vabVMD > *zeros
052200020305     C                   IF        vabVAD <> 'EUR'
052300011113     C                   EVAL      vabVAD =  'ITL'
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052800011113     C                   Z-ADD     vabIAS        wrkDec
052900011113     C                   IF        wrkDec > *zeros
053000011113     C                   IF        vabVAS = 'ITL'
053100011113     C                   EVAL      vabIAS = *zeros
053200011113     C                   ENDIF
053300011113     C                   ENDIF
053400011113     C*
053500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
053600011113     C                   Z-ADD     vabCAS        wrkDec
053700011113     C                   IF        wrkDec > *zeros
053800011113     C                   IF        vabVCA = 'ITL'
053900011113     C                   EVAL      vabCAS = *zeros
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054400011113     C                   Z-ADD     vabVMD        wrkDec
054500011113     C                   IF        wrkDec > *zeros
054600011113     C                   IF        vabVAD = 'ITL'
054700011113     C                   EVAL      vabVMD = *zeros
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C                   ENDSR
055200011113     C***
055300011113
055400011113
055500000801
055600000801
055700990920      /TITLE Invio dei dati al punto operativo.
055800010202     C     invio         BEGSR
055900990920     C*
056000021113     C* 1° invio FIVAT
056100010201     C                   reset                   dscmz
056200010201     C                   move      vlrpoi        cmzdst
056300021113     C                   eval      cmzfld = 'FIVATWWR'
056400010201     C                   eval      cmzmbd = vlrhdl
056500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056600021009     C***                if        prmfir = *blanks
056700021113     C                   eval      cmzfla = 'FIVAT00F'
056800021113     C                   eval      cmzmba = 'FIVAT00F'
056900021009     C***                else
057000021009     C***                eval      cmzfla = prmfir
057100021009     C***                eval      cmzmba = prmfir
057200021009     C***                endif
057300010201     C                   eval      cmznrr = *zeros
057400020305     C                   move      §ctrokvt      cmznrr
057500021018     C                   eval      cmzlba = vlrfl1
057600010201     C                   call(e)   'TIS711C'
057700010201     C                   parm                    dscmz
057800010201     C                   parm      *blanks       esito
057900010205     C                   if        %error
058000010205     C                             or cmzerr = '1'
058100010205     C                             or esito  = '1'
058200010205     C                   eval      wrkesito = '3'
058300010205     C                   else
058400010201     C*
058500021113     C* 2° invio FIVAB
058600010201     C                   reset                   dscmz
058700010201     C                   move      vlrpoi        cmzdst
058800010201     C                   eval      cmzfld = vlrfou
058900010201     C                   eval      cmzmbd = vlrhdl
059000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059100021009     C***                if        prmfir = *blanks
059200021113     C                   eval      cmzfla = 'FIVAB00F'
059300021113     C                   eval      cmzmba = 'FIVAB00F'
059400021009     C***                else
059500021009     C***                eval      cmzfla = prmfir
059600021009     C***                eval      cmzmba = prmfir
059700021009     C***                endif
059800010201     C                   eval      cmznrr = *zeros
059900010201     C                   move      §ctrokvb      cmznrr
060000021018     C                   eval      cmzlba = vlrfl1
060100010201     C                   call(e)   'TIS711C'
060200010201     C                   parm                    dscmz
060300010201     C                   parm      *blanks       esito
060400010201     C                   if        %error
060500010201     C                             or cmzerr = '1'
060600010201     C                             or esito  = '1'
060700010201     C                   eval      wrkesito = '3'
060800010201     C                   endif
060900010205     C                   endif
061000990920     C*
061100000613     C                   ENDSR
061200000613     C***
061300070411
061400070411     C     *pssr         BEGSR
061500070411     C*
061600070411     C                   if        %open(tivin00r)
061700070411     C                   close     tivin00r
061800070411     C                   endif
061900070411     C                   if        %open(fivabwwr)
062000070411     C                   close     fivabwwr
062100070411     C                   endif
062200070411     C                   if        %open(fivatwwr)
062300070411     C                   close     fivatwwr
062400070411     C                   endif
062500070411     C*
062600070411     C* Effettuo la chiamata al CLLE preposto
062700070411     C                   call(e)   'TITVVTC'
062800070411     C                   parm                    parccm
062900070411     C                   parm                    parmbr
063000070411     C                   parm      '2'           paropz
063100070411     C*
063200070411     C                   eval      wrkesito = '2'
063300070411     C*
063400070411     C                   seton                                        LR
063500070411     C*
063600070411     C                   ENDSR     '*CANCL'
063700070411     C***
063800070411
063900990910
064000000613     C     *inzsr        BEGSR
064100990910     C*
064200990910     C     *entry        plist
064300990920     C                   parm                    tivlrds
064400990921     C                   parm      wrkesito      esito
064500000724     C                   parm                    prmlit
064600000710     C                   parm                    prmfir
064700000613     C*
064800000830     C* CALCOLA LA DATA CORRENTE
064900000830     C                   time                    wn14             14 0
065000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065100000830     C                   z-add     wn8           g08dat
065200000830     C                   z-add     *zeros        g08inv
065300000830     C                   movel     '0'           g08err
065400000830     C                   call      'XSRDA8'
065500000830     C                   parm                    wlbda8
065600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065700000830     C*
065800000613     C                   ENDSR
065900000613     C***
066000990908
066100060315     Otitvs1p   E            riepilogo         2
066200990915     O                                              'Upload via Internet'
066300990915     O                                           +1 'Traduzione TIVIN00R -'
066400021113     O                                           +1 'FIVABWWR/FIVATWWR'
066500010201     O                                           +1 'Report testate bolle'
066600990915     O          E            riepilogo   2
066700990915     O                       wrkdata
066800990915     O                       wrkora              +1
066900990915     O                       procname            +1
067000990915     O          E            riepilogo   2
067100990915     O                                              'Cliente..................:'
067200990915     O                       VABCCM        z     +1
067300990915     O          E            riepilogo   2
067400990920     O                                              'Riferimento Strategi.....:'
067500990920     O                       vlrhdl              +1
067600990915     O          E            riepilogo   2
067700990915     O                                              'Giusti...................:'
067800010201     O                       §CTROKVB      2   +  1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Sbagliati e corretti.....:'
068100971022     O                       §CTRMO        2   +  1
068200990915     O          E            riepilogo   2
068300990915     O                                              'Sbagliati e scartati.....:'
068400971022     O                       §CTRNO        2   +  1
068500000613
068600060315     Otitvs1ps  E            testdett          2
068700000613     O                                              'Upload via Internet'
068800000613     O                                           +1 'Traduzione TIVIN00R -'
068900021113     O                                           +1 'FIVABWWR/FIVATWWR'
069000010201     O                                           +1 'Report testate bolle'
069100000616     O          E            testdett    3
069200000613     O                                           +2 'N° rec'
069300000613     O                                           +3 'Anteprima contenuto'
069400000616     O          E            rigadett    2
069500000613     O                       rrnum               +2
069600000621     O                       recko               +3
069700000616     O          E            findett     2
069800000613     O                       wrkdata
069900000613     O                       wrkora              +1
070000000613     O                       procname            +1
070100000616     O          E            findett     2
070200000613     O                                              'Cliente..................:'
070300000613     O                       VABCCM        z     +1
070400000616     O          E            findett     2
070500000613     O                                              'Riferimento Strategi.....:'
070600000613     O                       vlrhdl              +1
070700000616     O          E            findett     2
070800000613     O                                              'Giusti...................:'
070900010201     O                       §CTROKVB      2   +  1
071000000616     O          E            findett     2
071100000613     O                                              'Sbagliati e corretti.....:'
071200000613     O                       §CTRMO        2   +  1
071300000616     O          E            findett     2
071400000613     O                                              'Sbagliati e scartati.....:'
071500000613     O                       §CTRNO        2   +  1
071600000512** CMD - COMANDI CL
071700060315OVRPRTF FILE(TITVS1P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
071800060315OVRPRTF FILE(TITVS1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071900060315DLTOVR FILE(TITVS1P TITVS1PS) LVL(*)
072000000512
072100000512
