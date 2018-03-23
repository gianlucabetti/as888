000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130125     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090107     Ftitv1l7p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090107     Ftitv1l7ps O    f  198        PRINTER usropn
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
004000090107     D curSped         s             15    INZ(*blanks)
004100090107     D depSped         s             15    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100081118
005200081118     D*------------------
005300081118     D* LINKING A DEFINIZIONI ESTERNE
005400081118     D*------------------
005500081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005600081218     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700081118
005800081118
005900010201
006000990915     C                   time                    wrkdata
006100990915     C                   time                    wrkora
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500000724     C*
006600000724     C* SE OCCORRE SPEDIRE IN FILIALE
006700000724     C                   if        vlrpoi <> *zeros
006800000724     C*
006900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100000724     C     vlrpoi        chain     azorg01l
007200000724     C                   if        %found
007300000616     C                   movel(p)  CMD(1)        depcmd
007400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007500000616     C*
007600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011113     C                   Z-ADD     150           LENGH            15 5
007800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000616     C                   PARM                    depcmd
008000000616     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000616     C*
009500090107     C                   if        not %open(titv1l7ps)
009600090107     C                   open      titv1l7ps
009700130125     C***                except    testdett
009800000613     C                   endif
009900000613     C*
010000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010200000613     C*
010300010202     C* Effettuo la chiamata al CLLE preposto
010400040506     C                   call(e)   'TITVVTC'
010500010202     C                   parm                    parccm
010600010202     C                   parm                    parmbr
010700010202     C                   parm      '2'           paropz
010800050201     C*
010900050201     C* Effettuo lancio TISI95 solo x chiusura
011000050201     C                   CLEAR                   TISI95DS
011100050201     C                   EVAL      I95TLA = 'C'
011200050201     C                   CALL      'TISI95R'
011300050201     C                   PARM                    TISI95DS
011400010202     C*
011500090107     C                   if        %open(titv1l7ps)
011600130125     C***                except    findett
011700090107     C                   close     titv1l7ps
011800000613     C                   endif
011900000616     C*
012000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    CMD(3)
012400000616     C                   PARM                    LENGH
012500000616     C*
012600000801     C
012700010201     C                   seton                                        LR
012800000613
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500090107     C                   if        not %open(titv1l7p)
013600090107     C                   open      titv1l7p
013700990915     C                   endif
013800990915     C*
013900130125     C***                except    riepilogo
014000990915     C*
014100090107     C                   if        %open(titv1l7p)
014200090107     C                   close     titv1l7p
014300990914     C                   endif
014400990910     C*
014500000613     C                   ENDSR
014600000613     C***
014700990908
014800000801
014900910830     C*--------------------------------------------------------
015000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015100910830     C*--------------------------------------------------------
015200040526     C     RWFILE        BEGSR
015300990910     C*
015400990914     C                   if        not %open(tivin00r)
015500990908     C                   open      tivin00r
015600990914     C                   endif
015700021113     C                   if        not %open(fivabwwr)
015800021113     C                   open      fivabwwr
015900990914     C                   endif
016000070103     C*
016100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016200020305     C                   exsr      prevat
016300010201     C*
016400010202     C                   if        chkcall = '0'
016500010202     C*
016600021113     C                   if        not %open(fivatwwr)
016700021113     C                   open      fivatwwr
016800010201     C                   endif
016900990910     C*
017000010201     C                   clear                   §CTROKVB          5 0
017100020305     C                   clear                   §CTROKVT          5 0
017200000801     C                   clear                   §CTRMO            5 0
017300000801     C                   clear                   §CTRNO            5 0
017400990910     C*
017500921023     C                   DO        *HIVAL
017600990913     C*
017700990915     C                   READ      tivin00r                               70
017800050627     C                   if        vindta > *blanks
017900000613     C                   add       1             rrnum
018000000801     C*
018100000801     C                   if        *in70 = *off
018200000801     C                             and
018300000801     C                             (vinflg = *blanks
018400000801     C                              or vinflg = '0'
018500000801     C                              or vinflg = '2')
018600000801     C*
018700000801     C                   clear                   vinmsg
018800000801     C                   eval      vinflg = '1'
018900070103     C*
019000070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019100090107     C                   eval      curSped = %trim(%subst(vindta:163:15))
019200070213     C                   if        curSped <> depSped
019300070213     C* Se prima bolla => importo bolla corrente
019400070213     C                   if        §CTRMO = *zeros
019500070213     C                   exsr      inzvar
019600070213     C                   exsr      defcam
019700070213     C                   exsr      impvab                                       => carico  VAB
019800070213     C                   else
019900070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020000070213     C                   exsr      wrivab                                       => scarico VAB
020100070213     C                   exsr      inzvar
020200070213     C                   exsr      defcam
020300070213     C                   exsr      impvab                                       => carico  VAB
020400070213     C                   endif
020500071121     C                   exsr      exevate                                      => write VAT-E
020600070103     C* Salvo il raggruppamento spedizione cliente corrente
020700070103     C                   eval      depSped = curSped
020800070213     C*
020900070213     C* Se collo successivo x stessa bolla
021000070213     C                   else
021100070213     C                   exsr      impvab                                       => carico  VAB
021200070213     C                   exsr      exevate                                      => write VAT-E
021300070103     C                   endif
021400000905     C*
021500000905     C                   else
021600000905     C                   eval      vinflg = '1'
021700050628     C                   endif
021800000905     C                   endif
021900000905     C*
022000000905     C  N70              update    tivin000
022100000905     C*
022200991022     C  N70              ENDdo
022300070213     C*
022400070213     C* Scarico testata bolla rimasta in sospesa
022500070213     C                   exsr      wrivab                                       => scarico VAB
022600010202     C*
022700010202     C                   endif
022800990910
022900990910     C* Se non ci sono record con errori ...
023000000710     C                   if        §ctrno = 0
023100990910     C* ... restituisco esito OK.
023200990921     C                   eval      wrkesito = '0'
023300990910     C                   else
023400010201     C                   if        §ctrokvb > 0
023500990921     C                   eval      wrkesito = '1'
023600000710     C                   else
023700000710     C                   eval      wrkesito = '2'
023800990910     C                   endif
023900000710     C                   endif
024000990910     C*
024100990914     C                   if        %open(tivin00r)
024200990908     C                   close     tivin00r
024300990914     C                   endif
024400021113     C                   if        %open(fivabwwr)
024500021113     C                   close     fivabwwr
024600990914     C                   endif
024700021113     C                   if        %open(fivatwwr)
024800021113     C                   close     fivatwwr
024900010201     C                   endif
025000990910     C*
025100010201     C                   if        §ctrokvb > 0
025200000724     C                             and vlrpoi <> *zeros
025300010202     C                   exsr      invio
025400990920     C                   endif
025500990920     C*
025600910830     C                   ENDSR
025700000613     C***
025800010305
025900010305     C*----------------------------------------------------*
026000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026100010305     C*----------------------------------------------------*
026200020305     C     WRIVAB        BEGSR
026300010305     C*
026400060225     C* Quindi scarico il buffer del file d testata
026500021113     C                   write     fivab000                                     => scarico il VAB
026600010305     C*
026700010305     C                   ENDSR
026800990920
026900000801     C*----------------------------------------------------*
027000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100000801     C*----------------------------------------------------*
027200010201     C     INZVAR        BEGSR
027300000801     C*
027400070213     C                   CLEAR                   FIVAB000
027500070213     C                   CLEAR                   FIVAT000
027600070213     C*
027700040802     C                   Z-ADD     *zeros        Num5_0            5 0
027800040802     C                   MOVEL     '0'           FlgCAS            1
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028300000801     C*----------------------------------------------------*
028400000801     C     DEFCAM        BEGSR
028500000801     C*
028600020619     C* Imposto i valori di default...
028700090107     C                   Z-ADD     0895008       VABCCM
028800090107     C                   Z-ADD     0895008       VATCCM
028900090107     C                   Z-ADD     089           VABLNP
029000090107     C                   Z-ADD     089           VATLNP
029100090107     C                   Z-ADD     000           VABCTR
029200070103     C                   MOVEL     '7Q'          VABCTM
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
031900020619     C                   ENDIF
032000000801     C*
032100000801     C                   ENDSR
032200000801     C*----------------------------------------------------*
032300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032400000801     C*----------------------------------------------------*
032500040823     C     IMPVAB        BEGSR
032600010305     C*
032700000801     C                   Z-ADD     *zeros        errore            1 0
032800000830     C                   MOVEL     datcor        VABAAS
032900020305     C                   MOVEL     datcor        VATAAS
033000040526     C                   MOVE      datcor        VABMGS
033100040823     C                   MOVE(P)   vlrpoi        VABFGS
033200040823     C                   MOVE(P)   vlrpoi        VATFGS
033300080409     C*
033400080409     C* Reperimento campi ALFA
033500080409     C*
033600080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
033700090107     C                   EVAL      VABRSD=%trim(%subst(vindta:11:40))
033800080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033900080409     C     '@':'A'       XLATE     VABRSD        VABRSD
034000080409     C* ==
034100090107     C                   EVAL      VABIND=%trim(%subst(vindta:91:32))
034200090107     C                   EVAL      VABLOD=%trim(%subst(vindta:123:32))
034300090107     C                   EVAL      VABPRD=%trim(%subst(vindta:155:2))
034400090107     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
034500090107     C                   EVAL      VABNAS=%trim(%subst(vindta:207:15))
034600090107     C                   EVAL      VABNOT=%trim(%subst(vindta:259:35))
034700090107     C                   EVAL      VABNT2=%trim(%subst(vindta:259+35:54-35))
034800090107     C                   IF        %trim(%subst(vindta:162:1)) = 'F'
034900081218     C                   EVAL      VABCBO = '1'
035000081121     C                   ENDIF
035100090107     C                   IF        %trim(%subst(vindta:162:1)) = 'A'
035200081218     C                   EVAL      VABCBO = '2'
035300081218     C                   ENDIF
035400090107     C                   IF        %trim(%subst(vindta:344:1)) = 'T'
035500090107     C                   EVAL      VABTC1 = 'A'
035600090107     C                   ENDIF
035700080409     C*
035800080409     C* Reperimento campi NUMERICI
035900081218     C
036000080409     C* NSP/RMN
036100090107     C                   EVAL      PiStr=%trim(%subst(vindta:163:15))
036200080409     C                   EXSR      CHKNUM
036300080409     C                   IF        PiInt=*on
036400080409     C                   Z-ADD     PiVal         VABNSP
036500080409     C                   Z-ADD     PiVal         VATNSP
036600080409     C                   Z-ADD     PiVal         VABRMN
036700080409     C                   ELSE
036800080409     C                   ADD       1             errore
036900080409     C                   Z-ADD     *zeros        VABNSP
037000080409     C                   Z-ADD     1             VABRMN
037100080409     C                   EVAL      vinmsg = %trimr(vinmsg)
037200080409     C                             + ' ' + 'VABNSP VABRMN VATNSP'
037300080409     C                   ENDIF
037400080409     C* CAD
037500090107     C                   EVAL      PiStr=%trim(%subst(vindta:157:5))
037600080409     C                   EXSR      CHKNUM
037700080409     C                   IF        PiInt=*on
037800080409     C                   Z-ADD     PiVal         Num5_0
037900080409     C                   MOVEL(p)  Num5_0        VABCAD
038000080409     C                   ELSE
038100080409     C                   ADD       1             errore
038200080409     C                   EVAL      VABCAD = *zeros
038300080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038400080409     C                             + ' ' + 'VABCAD'
038500080409     C                   ENDIF
038600080409     C* NCL
038700090107     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
038800080409     C                   EXSR      CHKNUM
038900080409     C                   IF        PiInt=*on
039000080409     C                   Z-ADD     PiVal         VABNCL
039100080409     C                   ELSE
039200080409     C                   ADD       1             errore
039300080409     C                   Z-ADD     *zeros        VABNCL
039400080409     C                   EVAL      vinmsg = %trimr(vinmsg)
039500080409     C                             + ' ' + 'VABNCL'
039600080409     C                   ENDIF
039700090107     C* DCR
039800090107     C                   IF        %subst(vindta:201:6) <> *blanks AND
039900090107     C                             %subst(vindta:201:6) <> *zeros
040000090107     C                   EVAL      PiStr='20'+%subst(vindta:201:6)
040100090107     C                   EXSR      CHKNUM
040200090107     C                   IF        PiInt=*on
040300090107     C                   Z-ADD     PiVal         VABDCR
040400090107     C                   ELSE
040500090107     C                   ADD       1             errore
040600090107     C                   Z-ADD     *zeros        VABDCR
040700090107     C                   EVAL      vinmsg = %trimr(vinmsg)
040800090107     C                             + ' ' + 'VABDCR'
040900090107     C                   ENDIF
041000090107     C                   ENDIF
041100080409     C* PKB
041200090107     C                   EVAL      PiStr=%trim(%subst(vindta:227:5))
041300080409     C                   EXSR      CHKNUM
041400080409     C                   IF        PiNum=*on
041500081218     C                   Z-ADD(H)  PiVal         VABPKB
041600080409     C                   ELSE
041700080409     C                   ADD       1             errore
041800080409     C                   Z-ADD     *zeros        VABPKB
041900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
042000080409     C                             + ' ' + 'VABPKB'
042100080409     C                   ENDIF
042200090107     C* CAS
042300090107     C                   IF        %subst(vindta:313:13) <> *blanks AND
042400090107     C                             %subst(vindta:313:13) <> *zeros
042500090107     C                   EVAL      FlgCAS = '1'
042600090107     C                   EVAL      VABVCA = 'EUR'
042700090107     C                   EVAL      PiStr=%trim(%subst(vindta:313:13))
042800090107     C                   EXSR      CHKNUM
042900090107     C                   IF        PiInt=*on
043000090107     C                   Z-ADD     PiVal         VABCAS
043100090107     C                   ELSE
043200090107     C                   ADD       1             errore
043300090107     C                   Z-ADD     *zeros        VABCAS
043400090107     C                   EVAL      vinmsg = %trimr(vinmsg)
043500090107     C                             + ' ' + 'VABCAS'
043600090107     C                   ENDIF
043700090107     C                   ENDIF
043800071121     C*
043900080415     C* Carico l'estensioni A e B del FNVAT
044000080415     C                   exsr      exevata
044100071121     C                   exsr      exevatb
044200010205     C*
044300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
044400040802     C                   IF        FlgCAS <> '0'
044500081118     C                   IF        VABCBO = '1'
044600010205     C                   EVAL      VABCBO = '4'
044700010205     C                   ENDIF
044800081118     C                   IF        VABCBO = '2'
044900081118     C                   EVAL      VABCBO = '6'
045000081118     C                   ENDIF
045100081118     C                   ENDIF
045200020305     C*
045300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045400011113     C                   EXSR      CHKIMPDIV
045500010202     C*
045600000801     C* Ebbene...
045700000801     C                   ADD       1             §CTRMO
045800010201     C                   IF        errore <> *zeros
045900000801     C                   ADD       1             §CTRNO
046000000801     C                   EVAL      recko = vindta
046100130125     C***                EXCEPT    rigadett
046200000801     C                   EVAL      vinflg = '2'
046300000801     C                   ELSE
046400010201     C                   ADD       1             §CTROKVB
046500000801     C                   ENDIF
046600000801     C*
046700000801     C                   ENDSR
046800070102     C*----------------------------------------------------*
046900070103     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
047000070102     C*----------------------------------------------------*
047100070103     C     EXEVATE       BEGSR
047200081118     C*
047300070102     C*
047400081118     C***                exsr      wrivat                                       => scarico VAT
047500070102     C*
047600070102     C                   ENDSR
047700080415     C*----------------------------------------------------*
047800080415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047900080415     C*----------------------------------------------------*
048000080415     C     EXEVATA       BEGSR
048100080415     C*
048200080415     C*
048300080415     C***                exsr      wrivat                                       => scarico VAT
048400080415     C*
048500080415     C                   ENDSR
048600071121     C*----------------------------------------------------*
048700071121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
048800071121     C*----------------------------------------------------*
048900071121     C     EXEVATB       BEGSR
049000071121     C*
049100071121     C*
049200080409     C***                exsr      wrivat                                       => scarico VAT
049300071121     C*
049400071121     C                   ENDSR
049500010201     C*----------------------------------------------------*
049600040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049700010201     C*----------------------------------------------------*
049800020305     C     WRIVAT        BEGSR
049900081118     C*
050000081118     C* Imposto gli stessi campi chiave della testata relativa
050100081118     C                   EVAL      VATAAS = VABAAS
050200081118     C                   EVAL      VATLNP = VABLNP
050300081118     C                   EVAL      VATNRS = VABNRS
050400081118     C                   EVAL      VATNSP = VABNSP
050500081118     C                   EVAL      VATCCM = VABCCM
050600050628     C*
050700060223     C* Scrivo solo se valorizzato qualcosa
050800060223     C                   IF        VATNOT <> *blanks
050900040802     C                   WRITE     FIVAT000
051000060223     C                   ENDIF
051100010201     C*
051200010201     C                   ENDSR
051300010202     C*----------------------------------------------------*
051400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051500010202     C*----------------------------------------------------*
051600020305     C     PREVAT        BEGSR
051700010202     C*
051800021113     C* Compongo il nome del membro da dare al FIVATWWR
051900010202     C                   eval      parmbr = vlrhdl
052000010202     C                   movel     'M'           parmbr
052100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
052200010202     C                   eval      paropz = '1'
052300010202     C* Effettuo la chiamata al CLLE preposto
052400040506     C                   call(e)   'TITVVTC'
052500010202     C                   parm                    parccm
052600010202     C                   parm                    parmbr
052700010202     C                   parm                    paropz
052800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052900010202     C                   if        %error
053000010202     C                   movel     '1'           chkcall
053100010202     C                   else
053200010202     C                   movel     '0'           chkcall
053300010202     C                   endif
053400010202     C*
053500010202     C                   ENDSR
053600000801     C*----------------------------------------------------*
053700000801     C*  CONTROLLO NUMERICITA' CAMPI
053800000801     C*----------------------------------------------------*
053900000801     C     CHKNUM        BEGSR
054000081118     C*
054100081118     C                   IF        PiDecChr = *blanks
054200081118     C                   EVAL      PiDecChr = ','
054300081118     C                   ENDIF
054400081118     C*
054500081118     C                   callp(e)  UBISNUM_Check(PiStr
054600081118     C                                          :PiDecChr
054700081118     C                                          :PiVal
054800081118     C                                          :PiNum
054900081118     C                                          :PiInt)
055000081118     C*
055100081118     C                   IF        %error
055200081118     C                   EVAL      PiInt=*off
055300081118     C                   ENDIF
055400000801     C*
055500000801     C                   ENDSR
055600000801     C***
055700000801
055800011113
055900011113     C*----------------------------------------------------*
056000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056100011113     C*----------------------------------------------------*
056200011113     C     CHKIMPDIV     BEGSR
056300011113     C*
056400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056500011113     C                   Z-ADD     *zeros        wrkDec            9 9
056600011113     C*
056700011113     C* Come prima cosa effettuo considerazioni sulla divisa
056800011113     C                   IF        vabIAS > *zeros
056900011113     C                   IF        vabVAS <> 'EUR'
057000011113     C                   EVAL      vabVAS =  'ITL'
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C                   IF        vabCAS > *zeros
057500011113     C                   IF        vabVCA <> 'EUR'
057600011113     C                   EVAL      vabVCA =  'ITL'
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C                   IF        vabVMD > *zeros
058100020305     C                   IF        vabVAD <> 'EUR'
058200011113     C                   EVAL      vabVAD =  'ITL'
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058700011113     C                   Z-ADD     vabIAS        wrkDec
058800011113     C                   IF        wrkDec > *zeros
058900011113     C                   IF        vabVAS = 'ITL'
059000011113     C                   EVAL      vabIAS = *zeros
059100011113     C                   ENDIF
059200011113     C                   ENDIF
059300011113     C*
059400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059500011113     C                   Z-ADD     vabCAS        wrkDec
059600011113     C                   IF        wrkDec > *zeros
059700011113     C                   IF        vabVCA = 'ITL'
059800011113     C                   EVAL      vabCAS = *zeros
059900011113     C                   ENDIF
060000011113     C                   ENDIF
060100011113     C*
060200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060300011113     C                   Z-ADD     vabVMD        wrkDec
060400011113     C                   IF        wrkDec > *zeros
060500011113     C                   IF        vabVAD = 'ITL'
060600011113     C                   EVAL      vabVMD = *zeros
060700011113     C                   ENDIF
060800011113     C                   ENDIF
060900011113     C*
061000011113     C                   ENDSR
061100011113     C***
061200011113
061300011113
061400000801
061500000801
061600990920      /TITLE Invio dei dati al punto operativo.
061700010202     C     invio         BEGSR
061800990920     C*
061900021113     C* 1° invio FIVAT
062000010201     C                   reset                   dscmz
062100010201     C                   move      vlrpoi        cmzdst
062200021113     C                   eval      cmzfld = 'FIVATWWR'
062300010201     C                   eval      cmzmbd = vlrhdl
062400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062500021009     C***                if        prmfir = *blanks
062600021113     C                   eval      cmzfla = 'FIVAT00F'
062700021113     C                   eval      cmzmba = 'FIVAT00F'
062800021009     C***                else
062900021009     C***                eval      cmzfla = prmfir
063000021009     C***                eval      cmzmba = prmfir
063100021009     C***                endif
063200010201     C                   eval      cmznrr = *zeros
063300020305     C                   move      §ctrokvt      cmznrr
063400021018     C                   eval      cmzlba = vlrfl1
063500010201     C                   call(e)   'TIS711C'
063600010201     C                   parm                    dscmz
063700010201     C                   parm      *blanks       esito
063800010205     C                   if        %error
063900010205     C                             or cmzerr = '1'
064000010205     C                             or esito  = '1'
064100010205     C                   eval      wrkesito = '3'
064200010205     C                   else
064300010201     C*
064400021113     C* 2° invio FIVAB
064500010201     C                   reset                   dscmz
064600010201     C                   move      vlrpoi        cmzdst
064700010201     C                   eval      cmzfld = vlrfou
064800010201     C                   eval      cmzmbd = vlrhdl
064900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000021009     C***                if        prmfir = *blanks
065100021113     C                   eval      cmzfla = 'FIVAB00F'
065200021113     C                   eval      cmzmba = 'FIVAB00F'
065300021009     C***                else
065400021009     C***                eval      cmzfla = prmfir
065500021009     C***                eval      cmzmba = prmfir
065600021009     C***                endif
065700010201     C                   eval      cmznrr = *zeros
065800010201     C                   move      §ctrokvb      cmznrr
065900021018     C                   eval      cmzlba = vlrfl1
066000010201     C                   call(e)   'TIS711C'
066100010201     C                   parm                    dscmz
066200010201     C                   parm      *blanks       esito
066300010201     C                   if        %error
066400010201     C                             or cmzerr = '1'
066500010201     C                             or esito  = '1'
066600010201     C                   eval      wrkesito = '3'
066700010201     C                   endif
066800010205     C                   endif
066900990920     C*
067000000613     C                   ENDSR
067100000613     C***
067200070411
067300070411     C     *pssr         BEGSR
067400070411     C*
067500070411     C                   if        %open(tivin00r)
067600070411     C                   close     tivin00r
067700070411     C                   endif
067800070411     C                   if        %open(fivabwwr)
067900070411     C                   close     fivabwwr
068000070411     C                   endif
068100070411     C                   if        %open(fivatwwr)
068200070411     C                   close     fivatwwr
068300070411     C                   endif
068400070411     C*
068500070411     C* Effettuo la chiamata al CLLE preposto
068600070411     C                   call(e)   'TITVVTC'
068700070411     C                   parm                    parccm
068800070411     C                   parm                    parmbr
068900070411     C                   parm      '2'           paropz
069000070411     C*
069100070411     C                   eval      wrkesito = '2'
069200070411     C*
069300070411     C                   seton                                        LR
069400070411     C*
069500070411     C                   ENDSR     '*CANCL'
069600070411     C***
069700070411
069800990910
069900000613     C     *inzsr        BEGSR
070000990910     C*
070100990910     C     *entry        plist
070200990920     C                   parm                    tivlrds
070300990921     C                   parm      wrkesito      esito
070400000724     C                   parm                    prmlit
070500000710     C                   parm                    prmfir
070600000613     C*
070700000830     C* CALCOLA LA DATA CORRENTE
070800000830     C                   time                    wn14             14 0
070900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071000000830     C                   z-add     wn8           g08dat
071100000830     C                   z-add     *zeros        g08inv
071200000830     C                   movel     '0'           g08err
071300000830     C                   call      'XSRDA8'
071400000830     C                   parm                    wlbda8
071500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
071600000830     C*
071700000613     C                   ENDSR
071800000613     C***
071900990908
072000090107     Otitv1l7p  E            riepilogo         2
072100990915     O                                              'Upload via Internet'
072200990915     O                                           +1 'Traduzione TIVIN00R -'
072300021113     O                                           +1 'FIVABWWR/FIVATWWR'
072400010201     O                                           +1 'Report testate bolle'
072500990915     O          E            riepilogo   2
072600990915     O                       wrkdata
072700990915     O                       wrkora              +1
072800990915     O                       procname            +1
072900990915     O          E            riepilogo   2
073000990915     O                                              'Cliente..................:'
073100990915     O                       VABCCM        z     +1
073200990915     O          E            riepilogo   2
073300990920     O                                              'Riferimento Strategi.....:'
073400990920     O                       vlrhdl              +1
073500990915     O          E            riepilogo   2
073600990915     O                                              'Giusti...................:'
073700010201     O                       §CTROKVB      2   +  1
073800990915     O          E            riepilogo   2
073900990915     O                                              'Sbagliati e corretti.....:'
074000971022     O                       §CTRMO        2   +  1
074100990915     O          E            riepilogo   2
074200990915     O                                              'Sbagliati e scartati.....:'
074300971022     O                       §CTRNO        2   +  1
074400000613
074500090107     Otitv1l7ps E            testdett          2
074600000613     O                                              'Upload via Internet'
074700000613     O                                           +1 'Traduzione TIVIN00R -'
074800021113     O                                           +1 'FIVABWWR/FIVATWWR'
074900010201     O                                           +1 'Report testate bolle'
075000000616     O          E            testdett    3
075100000613     O                                           +2 'N° rec'
075200000613     O                                           +3 'Anteprima contenuto'
075300000616     O          E            rigadett    2
075400000613     O                       rrnum               +2
075500000621     O                       recko               +3
075600000616     O          E            findett     2
075700000613     O                       wrkdata
075800000613     O                       wrkora              +1
075900000613     O                       procname            +1
076000000616     O          E            findett     2
076100000613     O                                              'Cliente..................:'
076200000613     O                       VABCCM        z     +1
076300000616     O          E            findett     2
076400000613     O                                              'Riferimento Strategi.....:'
076500000613     O                       vlrhdl              +1
076600000616     O          E            findett     2
076700000613     O                                              'Giusti...................:'
076800010201     O                       §CTROKVB      2   +  1
076900000616     O          E            findett     2
077000000613     O                                              'Sbagliati e corretti.....:'
077100000613     O                       §CTRMO        2   +  1
077200000616     O          E            findett     2
077300000613     O                                              'Sbagliati e scartati.....:'
077400000613     O                       §CTRNO        2   +  1
077500000512** CMD - COMANDI CL
077600090107OVRPRTF FILE(TITV1L7P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077700090107OVRPRTF FILE(TITV1L7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077800090107DLTOVR FILE(TITV1L7P TITV1L7PS) LVL(*)
077900000512
078000000512
