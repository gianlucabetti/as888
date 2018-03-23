000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090114     FFIVABwwr  O    E             DISK    usropn
000700090114     FFIVATwwr  O    E             DISK    usropn
000800090527     Ftitv1p9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090527     Ftitv1p9ps O    f  198        PRINTER usropn
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
004900090114
005000081217
005100081217     D*------------------
005200081217     D* LINKING A DEFINIZIONI ESTERNE
005300081217     D*------------------
005400081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600081217
005700081217
005800010201
005900081217
006000081217     C
006100990915     C                   time                    wrkdata
006200990915     C                   time                    wrkora
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600000724     C*
006700000724     C* SE OCCORRE SPEDIRE IN FILIALE
006800000724     C                   if        vlrpoi <> *zeros
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200000724     C     vlrpoi        chain     azorg01l
007300000724     C                   if        %found
007400000616     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000616     C*
007700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000616     C                   PARM                    depcmd
008100000616     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000724     C                   endif
008500000616     C*
008600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700000616     C                   movel(p)  CMD(2)        depcmd
008800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008900000616     C*
009000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000616     C                   PARM                    depcmd
009400000616     C                   PARM                    LENGH
009500000616     C*
009600090527     C                   if        not %open(titv1p9ps)
009700090527     C                   open      titv1p9ps
009800130115     C***                except    testdett
009900000613     C                   endif
010000000613     C*
010100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010300000613     C*
010400010202     C* Effettuo la chiamata al CLLE preposto
010500090114     C                   call(e)   'TITVVTC'
010600010202     C                   parm                    parccm
010700010202     C                   parm                    parmbr
010800010202     C                   parm      '2'           paropz
010900050201     C*
011000050201     C* Effettuo lancio TISI95 solo x chiusura
011100050201     C                   CLEAR                   TISI95DS
011200050201     C                   EVAL      I95TLA = 'C'
011300050201     C                   CALL      'TISI95R'
011400050201     C                   PARM                    TISI95DS
011500010202     C*
011600090527     C                   if        %open(titv1p9ps)
011700130115     C***                except    findett
011800090527     C                   close     titv1p9ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700000801     C
012800010201     C                   seton                                        LR
012900000613
013000000613
013100000613     C*--------------------------------------------------------
013200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013300000613     C*--------------------------------------------------------
013400000621     C     STPR          BEGSR
013500000613     C*
013600090527     C                   if        not %open(titv1p9p)
013700090527     C                   open      titv1p9p
013800990915     C                   endif
013900990915     C*
014000130115     C***                except    riepilogo
014100990915     C*
014200090527     C                   if        %open(titv1p9p)
014300090527     C                   close     titv1p9p
014400990914     C                   endif
014500990910     C*
014600000613     C                   ENDSR
014700000613     C***
014800990908
014900000801
015000910830     C*--------------------------------------------------------
015100090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015200910830     C*--------------------------------------------------------
015300040526     C     RWFILE        BEGSR
015400990910     C*
015500990914     C                   if        not %open(tivin00r)
015600990908     C                   open      tivin00r
015700990914     C                   endif
015800090114     C                   if        not %open(fivabwwr)
015900090114     C                   open      fivabwwr
016000990914     C                   endif
016100070103     C*
016200090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700090114     C                   if        not %open(fivatwwr)
016800090114     C                   open      fivatwwr
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
019000070103     C*
019100090309     C                   EXSR      INZVAR
019200090309     C                   EXSR      DEFCAM
019300090527     C*
019400090527     C* Se trattasi d record "chi sono"...
019500090527     C                   if        %subst(vindta:1:5) = 'C7411'
019600090527     C                   exsr      exevate                                      => write VAT-E
019700090527     C                   endif
019800090527     C*
019900090527     C* Se trattasi d record testata....
020000090527     C                   if        %subst(vindta:1:5) = *blanks
020100070213     C                   exsr      impvab                                       => carico  VAB
020200090527     C                   exsr      wrivab                                       => scarico VAB
020300090527     C                   endif
020400000905     C*
020500000905     C                   else
020600000905     C                   eval      vinflg = '1'
020700050628     C                   endif
020800000905     C                   endif
020900000905     C*
021000000905     C  N70              update    tivin000
021100000905     C*
021200991022     C  N70              ENDdo
021300010202     C*
021400010202     C                   endif
021500990910
021600990910     C* Se non ci sono record con errori ...
021700000710     C                   if        §ctrno = 0
021800990910     C* ... restituisco esito OK.
021900990921     C                   eval      wrkesito = '0'
022000990910     C                   else
022100010201     C                   if        §ctrokvb > 0
022200990921     C                   eval      wrkesito = '1'
022300000710     C                   else
022400000710     C                   eval      wrkesito = '2'
022500990910     C                   endif
022600000710     C                   endif
022700990910     C*
022800990914     C                   if        %open(tivin00r)
022900990908     C                   close     tivin00r
023000990914     C                   endif
023100090114     C                   if        %open(fivabwwr)
023200090114     C                   close     fivabwwr
023300990914     C                   endif
023400090114     C                   if        %open(fivatwwr)
023500090114     C                   close     fivatwwr
023600010201     C                   endif
023700990910     C*
023800010201     C                   if        §ctrokvb > 0
023900000724     C                             and vlrpoi <> *zeros
024000010202     C                   exsr      invio
024100990920     C                   endif
024200990920     C*
024300910830     C                   ENDSR
024400000613     C***
024500010305
024600010305     C*----------------------------------------------------*
024700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024800010305     C*----------------------------------------------------*
024900020305     C     WRIVAB        BEGSR
025000010305     C*
025100060225     C* Quindi scarico il buffer del file d testata
025200090114     C                   write     fivab000                                     => scarico il VAB
025300010305     C*
025400010305     C                   ENDSR
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100090114     C                   CLEAR                   FIVAB000
026200090114     C                   CLEAR                   FIVAT000
026300070213     C*
026400040802     C                   Z-ADD     *zeros        Num5_0            5 0
026500040802     C                   MOVEL     '0'           FlgCAS            1
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027000000801     C*----------------------------------------------------*
027100000801     C     DEFCAM        BEGSR
027200000801     C*
027300020619     C* Imposto i valori di default...
027400090527     C                   MOVEL     datcor        VABAAS
027500090527     C                   MOVEL     datcor        VATAAS
027600090527     C                   MOVE(P)   vlrpoi        VABFGS
027700090527     C                   MOVE(P)   vlrpoi        VATFGS
027800090527     C*
027900090527     C                   Z-ADD     2220563       VABCCM
028000090527     C                   Z-ADD     2220563       VATCCM
028100090527     C                   Z-ADD     222           VABLNP
028200090527     C                   Z-ADD     222           VATLNP
028300090519     C                   Z-ADD     000           VABCTR
028400040823     C                   MOVEL     '1'           VABCBO
028500090527     C                   MOVEL     '7Q'          VABCTM
028600020619     C* ... e poi verifico se sono stati passati come parametri
028700020619     C                   IF        vlrppt > *blanks
028800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABCCM
029300020619     C                   Z-ADD     PiVal         VATCCM
029400020619     C                   ENDIF
029500040506     C                   ENDIF
029600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABLNP
030100020619     C                   Z-ADD     PiVal         VATLNP
030200040506     C                   ENDIF
030300020619     C                   ENDIF
030400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030600020619     C                   EXSR      CHKNUM
030700020619     C                   IF        PiInt=*on
030800020619     C                   Z-ADD     PiVal         VABCTR
030900040506     C                   ENDIF
031000020619     C                   ENDIF
031100020619     C                   ENDIF
031200000801     C*
031300000801     C                   ENDSR
031400000801     C*----------------------------------------------------*
031500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031600000801     C*----------------------------------------------------*
031700040823     C     IMPVAB        BEGSR
031800010305     C*
031900000801     C                   Z-ADD     *zeros        errore            1 0
032000040526     C                   MOVE      datcor        VABMGS
032100080409     C*
032200080409     C* Reperimento campi ALFA
032300080409     C*
032400080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
032500090527     C                   EVAL      VABRSD=%trim(%subst(vindta:51:30))
032600080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032700080409     C     '@':'A'       XLATE     VABRSD        VABRSD
032800080409     C* ==
032900090527     C                   EVAL      VABIND=%trim(%subst(vindta:81:30))
033000090527     C                   EVAL      VABLOD=%trim(%subst(vindta:111:30))
033100090527     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
033200090527     C                   EVAL      VABRMA=%trim(%subst(vindta:30:3))+
033300090527     C                                    %trim(%subst(vindta:33:1))+
033400090527     C                                    %trim(%subst(vindta:34:8))
033500080409     C*
033600080409     C* Reperimento campi NUMERICI
033700090309     C* NSP
033800090527     C                   EVAL      PiStr=%trim(%subst(vindta:34:8))
033900090309     C                   EXSR      CHKNUM
034000090309     C                   IF        PiInt=*on
034100090309     C                   Z-ADD     PiVal         VABNSP
034200090309     C                   Z-ADD     PiVal         VATNSP
034300090309     C                   ELSE
034400090309     C                   EVAL      vinmsg = %trimr(vinmsg)
034500090309     C                             + ' ' + 'VABNSP VATNSP'
034600090309     C                   ENDIF
034700090309     C* RMN
034800090527     C                   EVAL      PiStr=%trim(%subst(vindta:34:8))
034900081217     C                   EXSR      CHKNUM
035000081217     C                   IF        PiInt=*on
035100081217     C                   Z-ADD     PiVal         VABRMN
035200081217     C                   ELSE
035300081217     C                   ADD       1             errore
035400081217     C                   Z-ADD     1             VABRMN
035500081217     C                   EVAL      vinmsg = %trimr(vinmsg)
035600081217     C                             + ' ' + 'VABRMN'
035700081217     C                   ENDIF
035800080409     C* CAD
035900090527     C                   EVAL      PiStr=%trim(%subst(vindta:143:5))
036000090114     C                   EXSR      CHKNUM
036100090114     C                   IF        PiInt=*on
036200090114     C                   Z-ADD     PiVal         Num5_0
036300090114     C                   MOVEL(p)  Num5_0        VABCAD
036400090114     C                   ELSE
036500090114     C                   ADD       1             errore
036600090114     C                   EVAL      VABCAD = *zeros
036700090114     C                   EVAL      vinmsg = %trimr(vinmsg)
036800090114     C                             + ' ' + 'VABCAD'
036900090114     C                   ENDIF
037000081217     C* NCL
037100090527     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
037200081217     C                   EXSR      CHKNUM
037300081217     C                   IF        PiInt=*on
037400081217     C                   Z-ADD     PiVal         VABNCL
037500081217     C                   ELSE
037600090309     C                   ADD       1             errore
037700081217     C                   EVAL      vinmsg = %trimr(vinmsg)
037800081217     C                             + ' ' + 'VABNCL'
037900081217     C                   ENDIF
038000090312     C* PKB
038100090527     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
038200090312     C                   EXSR      CHKNUM
038300090312     C                   IF        PiNum=*on
038400090527     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
038500090527     C                   Z-ADD     PiVal         VABPKB
038600090312     C                   ELSE
038700090312     C                   ADD       1             errore
038800090312     C                   EVAL      vinmsg = %trimr(vinmsg)
038900090312     C                             + ' ' + 'VABPKB'
039000090312     C                   ENDIF
039100090527     C* VLB
039200090527     C                   EVAL      PiStr=%trim(%subst(vindta:160:7))
039300090527     C                   EXSR      CHKNUM
039400090527     C                   IF        PiNum=*on
039500090527     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
039600090527     C                   Z-ADD     PiVal         VABVLB
039700090527     C                   ELSE
039800090527     C                   ADD       1             errore
039900090527     C                   EVAL      vinmsg = %trimr(vinmsg)
040000090527     C                             + ' ' + 'VABVLB'
040100090527     C                   ENDIF
040200090527     C* CAS
040300090527     C                   IF        %subst(vindta:187:9) <> *blanks AND
040400090527     C                             %subst(vindta:187:9) <> *zeros
040500090527     C                   EVAL      VABVCA = 'EUR'
040600090527     C                   EVAL      PiStr=%trim(%subst(vindta:187:9))
040700090428     C                   EXSR      CHKNUM
040800090428     C                   IF        PiNum=*on
040900090527     C                   Z-ADD     PiVal         VABCAS
041000090428     C                   ELSE
041100090428     C                   ADD       1             errore
041200090428     C                   EVAL      vinmsg = %trimr(vinmsg)
041300090527     C                             + ' ' + 'VABCAS'
041400090428     C                   ENDIF
041500090428     C                   ENDIF
041600010205     C*
041700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041800040802     C                   IF        FlgCAS <> '0'
041900010205     C                   EVAL      VABCBO = '4'
042000010205     C                   ENDIF
042100020305     C*
042200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300011113     C                   EXSR      CHKIMPDIV
042400010202     C*
042500000801     C* Ebbene...
042600000801     C                   ADD       1             §CTRMO
042700010201     C                   IF        errore <> *zeros
042800000801     C                   ADD       1             §CTRNO
042900000801     C                   EVAL      recko = vindta
043000130115     C***                EXCEPT    rigadett
043100000801     C                   EVAL      vinflg = '2'
043200000801     C                   ELSE
043300010201     C                   ADD       1             §CTROKVB
043400000801     C                   ENDIF
043500000801     C*
043600000801     C                   ENDSR
043700070102     C*----------------------------------------------------*
043800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043900070102     C*----------------------------------------------------*
044000070103     C     EXEVATE       BEGSR
044100090527     C*
044200090527     C                   EVAL      VATTRC = 'E'
044300090527     C* NCL
044400090527     C                   EVAL      PiStr=%trim(%subst(vindta:9:8))
044500090527     C                   EXSR      CHKNUM
044600090527     C                   IF        PiInt=*on
044700090527     C                   Z-ADD     PiVal         VATNSP
044800090527     C                   EVAL      VATNOT = %trim(%subst(vindta:21:32))
044900090527     C                   ELSE
045000090527     C                   ADD       1             errore
045100090527     C                   EVAL      vinmsg = %trimr(vinmsg)
045200090527     C                             + ' ' + 'VATNSP'
045300090527     C                   ENDIF
045400090428     C*
045500090527     C                   exsr      wrivat                                       => scarico VAT
045600070102     C*
045700070102     C                   ENDSR
045800080415     C*----------------------------------------------------*
045900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
046000080415     C*----------------------------------------------------*
046100080415     C     EXEVATA       BEGSR
046200080415     C*
046300090504     C                   EVAL      VATTRC = 'A'
046400080415     C*
046500080415     C***                exsr      wrivat                                       => scarico VAT
046600080415     C*
046700080415     C                   ENDSR
046800071121     C*----------------------------------------------------*
046900090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
047000071121     C*----------------------------------------------------*
047100071121     C     EXEVATB       BEGSR
047200071121     C*
047300090504     C                   EVAL      VATTRC = 'B'
047400071121     C*
047500080409     C***                exsr      wrivat                                       => scarico VAT
047600071121     C*
047700071121     C                   ENDSR
047800010201     C*----------------------------------------------------*
047900090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048000010201     C*----------------------------------------------------*
048100020305     C     WRIVAT        BEGSR
048200050628     C*
048300060223     C* Scrivo solo se valorizzato qualcosa
048400060223     C                   IF        VATNOT <> *blanks
048500090114     C                   WRITE     FIVAT000
048600060223     C                   ENDIF
048700010201     C*
048800010201     C                   ENDSR
048900010202     C*----------------------------------------------------*
049000090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049100010202     C*----------------------------------------------------*
049200020305     C     PREVAT        BEGSR
049300010202     C*
049400090114     C* Compongo il nome del membro da dare al FIVATWWR
049500010202     C                   eval      parmbr = vlrhdl
049600010202     C                   movel     'M'           parmbr
049700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049800010202     C                   eval      paropz = '1'
049900010202     C* Effettuo la chiamata al CLLE preposto
050000090114     C                   call(e)   'TITVVTC'
050100010202     C                   parm                    parccm
050200010202     C                   parm                    parmbr
050300010202     C                   parm                    paropz
050400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050500010202     C                   if        %error
050600010202     C                   movel     '1'           chkcall
050700010202     C                   else
050800010202     C                   movel     '0'           chkcall
050900010202     C                   endif
051000010202     C*
051100010202     C                   ENDSR
051200000801     C*----------------------------------------------------*
051300000801     C*  CONTROLLO NUMERICITA' CAMPI
051400000801     C*----------------------------------------------------*
051500000801     C     CHKNUM        BEGSR
051600081217     C*
051700081217     C                   IF        PiDecChr = *blanks
051800090519     C                   EVAL      PiDecChr = ','
051900081217     C                   ENDIF
052000081217     C*
052100081217     C                   callp(e)  UBISNUM_Check(PiStr
052200081217     C                                          :PiDecChr
052300081217     C                                          :PiVal
052400081217     C                                          :PiNum
052500081217     C                                          :PiInt)
052600081217     C*
052700081217     C                   IF        %error
052800081217     C                   EVAL      PiInt=*off
052900081217     C                   ENDIF
053000000801     C*
053100000801     C                   ENDSR
053200000801     C***
053300000801
053400011113
053500011113     C*----------------------------------------------------*
053600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053700011113     C*----------------------------------------------------*
053800011113     C     CHKIMPDIV     BEGSR
053900011113     C*
054000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054100011113     C                   Z-ADD     *zeros        wrkDec            9 9
054200011113     C*
054300011113     C* Come prima cosa effettuo considerazioni sulla divisa
054400011113     C                   IF        vabIAS > *zeros
054500011113     C                   IF        vabVAS <> 'EUR'
054600011113     C                   EVAL      vabVAS =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C                   IF        vabCAS > *zeros
055100011113     C                   IF        vabVCA <> 'EUR'
055200011113     C                   EVAL      vabVCA =  'ITL'
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   IF        vabVMD > *zeros
055700020305     C                   IF        vabVAD <> 'EUR'
055800011113     C                   EVAL      vabVAD =  'ITL'
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056300011113     C                   Z-ADD     vabIAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVAS = 'ITL'
056600011113     C                   EVAL      vabIAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057100011113     C                   Z-ADD     vabCAS        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVCA = 'ITL'
057400011113     C                   EVAL      vabCAS = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057900011113     C                   Z-ADD     vabVMD        wrkDec
058000011113     C                   IF        wrkDec > *zeros
058100011113     C                   IF        vabVAD = 'ITL'
058200011113     C                   EVAL      vabVMD = *zeros
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C                   ENDSR
058700011113     C***
058800011113
058900011113
059000000801
059100000801
059200990920      /TITLE Invio dei dati al punto operativo.
059300010202     C     invio         BEGSR
059400990920     C*
059500090114     C* 1° invio FIVAT
059600010201     C                   reset                   dscmz
059700010201     C                   move      vlrpoi        cmzdst
059800090114     C                   eval      cmzfld = 'FIVATWWR'
059900010201     C                   eval      cmzmbd = vlrhdl
060000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060100021009     C***                if        prmfir = *blanks
060200090114     C                   eval      cmzfla = 'FIVAT00F'
060300090114     C                   eval      cmzmba = 'FIVAT00F'
060400021009     C***                else
060500021009     C***                eval      cmzfla = prmfir
060600021009     C***                eval      cmzmba = prmfir
060700021009     C***                endif
060800010201     C                   eval      cmznrr = *zeros
060900020305     C                   move      §ctrokvt      cmznrr
061000021018     C                   eval      cmzlba = vlrfl1
061100010201     C                   call(e)   'TIS711C'
061200010201     C                   parm                    dscmz
061300010201     C                   parm      *blanks       esito
061400010205     C                   if        %error
061500010205     C                             or cmzerr = '1'
061600010205     C                             or esito  = '1'
061700010205     C                   eval      wrkesito = '3'
061800010205     C                   else
061900010201     C*
062000090114     C* 2° invio FIVAB
062100010201     C                   reset                   dscmz
062200010201     C                   move      vlrpoi        cmzdst
062300010201     C                   eval      cmzfld = vlrfou
062400010201     C                   eval      cmzmbd = vlrhdl
062500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062600021009     C***                if        prmfir = *blanks
062700090114     C                   eval      cmzfla = 'FIVAB00F'
062800090114     C                   eval      cmzmba = 'FIVAB00F'
062900021009     C***                else
063000021009     C***                eval      cmzfla = prmfir
063100021009     C***                eval      cmzmba = prmfir
063200021009     C***                endif
063300010201     C                   eval      cmznrr = *zeros
063400010201     C                   move      §ctrokvb      cmznrr
063500021018     C                   eval      cmzlba = vlrfl1
063600010201     C                   call(e)   'TIS711C'
063700010201     C                   parm                    dscmz
063800010201     C                   parm      *blanks       esito
063900010201     C                   if        %error
064000010201     C                             or cmzerr = '1'
064100010201     C                             or esito  = '1'
064200010201     C                   eval      wrkesito = '3'
064300010201     C                   endif
064400010205     C                   endif
064500990920     C*
064600000613     C                   ENDSR
064700000613     C***
064800070411
064900090331
065000090331
065100090331
065200070411     C     *pssr         BEGSR
065300070411     C*
065400070411     C                   if        %open(tivin00r)
065500070411     C                   close     tivin00r
065600070411     C                   endif
065700090114     C                   if        %open(fivabwwr)
065800090114     C                   close     fivabwwr
065900070411     C                   endif
066000090114     C                   if        %open(fivatwwr)
066100090114     C                   close     fivatwwr
066200070411     C                   endif
066300070411     C*
066400070411     C* Effettuo la chiamata al CLLE preposto
066500090114     C                   call(e)   'TITVVTC'
066600070411     C                   parm                    parccm
066700070411     C                   parm                    parmbr
066800070411     C                   parm      '2'           paropz
066900070411     C*
067000070411     C                   eval      wrkesito = '2'
067100070411     C*
067200070411     C                   seton                                        LR
067300070411     C*
067400070411     C                   ENDSR     '*CANCL'
067500070411     C***
067600070411
067700090331
067800090331
067900990910
068000000613     C     *inzsr        BEGSR
068100990910     C*
068200990910     C     *entry        plist
068300990920     C                   parm                    tivlrds
068400990921     C                   parm      wrkesito      esito
068500000724     C                   parm                    prmlit
068600000710     C                   parm                    prmfir
068700000613     C*
068800000830     C* CALCOLA LA DATA CORRENTE
068900000830     C                   time                    wn14             14 0
069000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069100080512     C                   movel     wn14          oracor            6 0          *ORA
069200000830     C                   z-add     wn8           g08dat
069300000830     C                   z-add     *zeros        g08inv
069400000830     C                   movel     '0'           g08err
069500000830     C                   call      'XSRDA8'
069600000830     C                   parm                    wlbda8
069700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069800000830     C*
069900000613     C                   ENDSR
070000000613     C***
070100990908
070200090331
070300090331
070400090527     Otitv1p9p  E            riepilogo         2
070500990915     O                                              'Upload via Internet'
070600990915     O                                           +1 'Traduzione TIVIN00R -'
070700090114     O                                           +1 'FIVABWWR/FIVATWWR'
070800010201     O                                           +1 'Report testate bolle'
070900990915     O          E            riepilogo   2
071000990915     O                       wrkdata
071100990915     O                       wrkora              +1
071200990915     O                       procname            +1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Cliente..................:'
071500990915     O                       VABCCM        z     +1
071600990915     O          E            riepilogo   2
071700990920     O                                              'Riferimento Strategi.....:'
071800990920     O                       vlrhdl              +1
071900990915     O          E            riepilogo   2
072000990915     O                                              'Giusti...................:'
072100010201     O                       §CTROKVB      2   +  1
072200990915     O          E            riepilogo   2
072300990915     O                                              'Sbagliati e corretti.....:'
072400971022     O                       §CTRMO        2   +  1
072500990915     O          E            riepilogo   2
072600990915     O                                              'Sbagliati e scartati.....:'
072700971022     O                       §CTRNO        2   +  1
072800000613
072900090527     Otitv1p9ps E            testdett          2
073000000613     O                                              'Upload via Internet'
073100000613     O                                           +1 'Traduzione TIVIN00R -'
073200090114     O                                           +1 'FIVABWWR/FIVATWWR'
073300010201     O                                           +1 'Report testate bolle'
073400000616     O          E            testdett    3
073500000613     O                                           +2 'N° rec'
073600000613     O                                           +3 'Anteprima contenuto'
073700000616     O          E            rigadett    2
073800000613     O                       rrnum               +2
073900000621     O                       recko               +3
074000000616     O          E            findett     2
074100000613     O                       wrkdata
074200000613     O                       wrkora              +1
074300000613     O                       procname            +1
074400000616     O          E            findett     2
074500000613     O                                              'Cliente..................:'
074600000613     O                       VABCCM        z     +1
074700000616     O          E            findett     2
074800000613     O                                              'Riferimento Strategi.....:'
074900000613     O                       vlrhdl              +1
075000000616     O          E            findett     2
075100000613     O                                              'Giusti...................:'
075200010201     O                       §CTROKVB      2   +  1
075300000616     O          E            findett     2
075400000613     O                                              'Sbagliati e corretti.....:'
075500000613     O                       §CTRMO        2   +  1
075600000616     O          E            findett     2
075700000613     O                                              'Sbagliati e scartati.....:'
075800000613     O                       §CTRNO        2   +  1
075900000512** CMD - COMANDI CL
076000090527OVRPRTF FILE(TITV1P9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076100090527OVRPRTF FILE(TITV1P9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
076200090527DLTOVR FILE(TITV1P9P TITV1P9PS) LVL(*)
076300000512
076400000512
